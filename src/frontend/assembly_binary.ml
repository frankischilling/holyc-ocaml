type compression = Raw | Ct_none | Ct_7_bit | Ct_8_bit

type provenance = {
  spelling : string;
  resolved_path : string;
  display_path : string;
  raw_size : int;
  decoded_size : int;
  raw_sha256 : string;
  decoded_sha256 : string;
  compression : compression;
}

type t = { bytes : string; provenance : provenance }

let ( let* ) = Result.bind
let header_bytes = 17
let dictionary_size = 1 lsl 12

let limits ~max_input_bytes ~max_output_bytes =
  if max_input_bytes < 0 then
    Error "BINFILE input size limit must be nonnegative"
  else if max_output_bytes < 0 then
    Error "BINFILE output size limit must be nonnegative"
  else Ok ()

let checked_size ~name ~limit value =
  if value < 0L then Error (name ^ " is negative")
  else if value > Int64.of_int limit then
    Error
      (Printf.sprintf "%s is %Ld bytes; the limit is %d bytes" name value limit)
  else if value > Int64.of_int Sys.max_string_length then
    Error (name ^ " exceeds the host string size limit")
  else Ok (Int64.to_int value)

let int64_le bytes offset =
  let value = ref 0L in
  for index = 0 to 7 do
    value :=
      Int64.logor !value
        (Int64.shift_left
           (Int64.of_int (Char.code bytes.[offset + index]))
           (8 * index))
  done;
  !value

type header = { compression : compression; expanded_size : int }

let read_header ~max_input_bytes ~max_output_bytes ~raw_size bytes =
  if raw_size < header_bytes || String.length bytes < header_bytes then
    Error "BINFILE compressed file has a truncated CArcCompress header"
  else
    let* compressed_size =
      checked_size ~name:"BINFILE declared compressed size"
        ~limit:max_input_bytes (int64_le bytes 0)
    in
    if compressed_size <> raw_size then
      Error
        (Printf.sprintf
           "BINFILE declared compressed size is %d bytes, but the file has %d \
            bytes"
           compressed_size raw_size)
    else
      let* expanded_size =
        checked_size ~name:"BINFILE expanded size" ~limit:max_output_bytes
          (int64_le bytes 8)
      in
      let* compression =
        match Char.code bytes.[16] with
        | 1 -> Ok Ct_none
        | 2 -> Ok Ct_7_bit
        | 3 -> Ok Ct_8_bit
        | code ->
            Error
              (Printf.sprintf "BINFILE has unsupported compression type %d" code)
      in
      if compression = Ct_none && expanded_size <> raw_size - header_bytes then
        Error "BINFILE CT_NONE payload size differs from its expanded size"
      else if expanded_size = 0 && raw_size <> header_bytes then
        Error "BINFILE empty archive contains unexpected payload bytes"
      else Ok { compression; expanded_size }

exception Invalid_archive of string

let invalid message =
  raise (Invalid_archive ("BINFILE compressed data " ^ message))

(* Kernel/Compress.HC: ArcCtrlNew, ArcEntryGet and ArcExpandBuf. Codes are
   least-significant-bit first. At 12 bits, ArcEntryGet reuses a leaf entry;
   there is no reset code. Parents and suffixes remain readable after unlinking,
   as in the original, until the reserved dictionary entry is overwritten. *)
let expand_codes ~min_bits ~expanded_size bytes emit =
  if expanded_size > 0 then (
    let alphabet = 1 lsl min_bits in
    let parent = Array.make dictionary_size (-1) in
    let suffix = Bytes.make dictionary_size '\000' in
    let initialized = Array.make dictionary_size false in
    let linked = Array.make dictionary_size false in
    let children = Array.make dictionary_size 0 in
    let seen = Array.make dictionary_size 0 in
    let generation = ref 0 in
    let stack = Bytes.create dictionary_size in
    let stack_size = ref 0 in
    let free_index = ref alphabet in
    let next_bits = ref (min_bits + 1) in
    let free_limit = ref (1 lsl !next_bits) in
    let current_entry = ref (-1) in
    let next_entry = ref (-1) in
    let advance_entry () =
      current_entry := !next_entry;
      if !next_bits < 12 then (
        next_entry := !free_index;
        incr free_index;
        if !free_index = !free_limit then (
          incr next_bits;
          free_limit := 1 lsl !next_bits))
      else
        let candidate = ref !free_index in
        let attempts = ref (!free_limit - alphabet) in
        let found = ref false in
        while (not !found) && !attempts > 0 do
          decr attempts;
          incr candidate;
          if !candidate = !free_limit then candidate := alphabet;
          found := children.(!candidate) = 0
        done;
        if not !found then invalid "has no reusable dictionary leaf";
        next_entry := !candidate;
        free_index := !candidate;
        if linked.(!candidate) then (
          let prefix = parent.(!candidate) in
          if prefix < 0 || children.(prefix) <= 0 then
            invalid "has inconsistent dictionary links";
          children.(prefix) <- children.(prefix) - 1;
          linked.(!candidate) <- false)
    in
    let byte_position = ref header_bytes in
    let bit_position = ref 0 in
    let read_code () =
      let required = (!bit_position + !next_bits + 7) / 8 in
      if required > String.length bytes - !byte_position then
        invalid "ends before its declared expanded size";
      let code = ref 0 in
      for bit = 0 to !next_bits - 1 do
        let value =
          (Char.code bytes.[!byte_position] lsr !bit_position) land 1
        in
        code := !code lor (value lsl bit);
        incr bit_position;
        if !bit_position = 8 then (
          bit_position := 0;
          incr byte_position)
      done;
      !code
    in
    let push byte =
      if !stack_size = dictionary_size then
        invalid "has a dictionary chain exceeding 4096 bytes";
      Bytes.set stack !stack_size (Char.chr byte);
      incr stack_size
    in
    let output_position = ref 0 in
    let output byte =
      (match emit with
      | None -> ()
      | Some write -> write !output_position byte);
      incr output_position
    in
    advance_entry ();
    let first = read_code () in
    if first >= alphabet then invalid "starts with a nonliteral dictionary code";
    output (Char.chr first);
    advance_entry ();
    let last_code = ref first in
    let last_character = ref first in
    while !output_position < expanded_size do
      let basecode = read_code () in
      stack_size := 0;
      incr generation;
      let code =
        if basecode = !current_entry then (
          push !last_character;
          ref !last_code)
        else ref basecode
      in
      while !code >= alphabet do
        if !code >= dictionary_size || not initialized.(!code) then
          invalid "references an undefined dictionary code";
        if seen.(!code) = !generation then
          invalid "has a cyclic dictionary chain";
        seen.(!code) <- !generation;
        push (Char.code (Bytes.get suffix !code));
        code := parent.(!code)
      done;
      if !code < 0 then invalid "has an invalid dictionary prefix";
      push !code;
      if !stack_size > expanded_size - !output_position then
        invalid "exceeds its declared expanded size";
      last_character := !code;
      if
        !current_entry < alphabet
        || !current_entry >= dictionary_size
        || linked.(!current_entry)
      then invalid "reuses an active dictionary entry";
      parent.(!current_entry) <- !last_code;
      Bytes.set suffix !current_entry (Char.chr !last_character);
      initialized.(!current_entry) <- true;
      linked.(!current_entry) <- true;
      children.(!last_code) <- children.(!last_code) + 1;
      advance_entry ();
      for index = !stack_size - 1 downto 0 do
        output (Bytes.get stack index)
      done;
      last_code := basecode
    done;
    let used_bytes = !byte_position + if !bit_position = 0 then 0 else 1 in
    if used_bytes <> String.length bytes then
      invalid "has trailing bytes after its declared expanded size")

let decode ~max_input_bytes ~max_output_bytes ~compressed bytes =
  let* () = limits ~max_input_bytes ~max_output_bytes in
  let* raw_size =
    checked_size ~name:"BINFILE input size" ~limit:max_input_bytes
      (Int64.of_int (String.length bytes))
  in
  if not compressed then
    let* _ =
      checked_size ~name:"BINFILE output size" ~limit:max_output_bytes
        (Int64.of_int raw_size)
    in
    Ok (bytes, Raw)
  else
    let* header =
      read_header ~max_input_bytes ~max_output_bytes ~raw_size bytes
    in
    match header.compression with
    | Raw -> assert false
    | Ct_none ->
        Ok
          ( String.sub bytes header_bytes header.expanded_size,
            header.compression )
    | Ct_7_bit | Ct_8_bit -> (
        let min_bits = if header.compression = Ct_7_bit then 7 else 8 in
        try
          (* Validate the whole stream before reserving the expanded buffer. *)
          expand_codes ~min_bits ~expanded_size:header.expanded_size bytes None;
          let output = Bytes.create header.expanded_size in
          expand_codes ~min_bits ~expanded_size:header.expanded_size bytes
            (Some (Bytes.set output));
          Ok (Bytes.unsafe_to_string output, header.compression)
        with Invalid_archive message -> Error message)

let resolver_error = function
  | Include_resolver.Empty_path -> "BINFILE path cannot be empty"
  | Include_resolver.Path_contains_nul -> "BINFILE path contains a NUL byte"
  | Include_resolver.Home_path_requires_mapping path ->
      Printf.sprintf "BINFILE home path requires an explicit mapping: %S" path
  | Include_resolver.Templeos_root_requires_mapping path ->
      Printf.sprintf "BINFILE TempleOS root requires an explicit mapping: %S"
        path
  | Include_resolver.Drive_path_unsupported path ->
      Printf.sprintf "BINFILE drive path is unsupported on this host: %S" path
  | Include_resolver.Not_found { spelling; searched } ->
      Printf.sprintf "could not find BINFILE %S; searched %s" spelling
        (String.concat ", " searched)
  | Include_resolver.Outside_allowed_roots { spelling; resolved; _ } ->
      Printf.sprintf "BINFILE %S resolves outside the allowed roots: %s"
        spelling resolved
  | Include_resolver.Is_directory path ->
      Printf.sprintf "BINFILE path is a directory: %s" path
  | Include_resolver.Not_regular_file path ->
      Printf.sprintf "BINFILE path is not a regular file: %s" path
  | Include_resolver.Io_error { path; message } ->
      Printf.sprintf "could not inspect BINFILE %s: %s" path message

(* DskStrA.HC: FileExtDot and ExtDft inspect the caller's spelling. *)
let default_extension spelling =
  let rec has_dot index =
    index < String.length spelling
    && (spelling.[index] = '.'
        && (index + 1 = String.length spelling
           || (spelling.[index + 1] <> '/' && spelling.[index + 1] <> '.'))
       || has_dot (index + 1))
  in
  if has_dot 0 then spelling else spelling ^ ".BIN"

let is_dot_z path =
  (* FileAttr receives the directory entry's filename. A lone .Z extension
     is not TempleOS's compressed-file suffix; it requires two dots. *)
  let name = Filename.basename path in
  String.ends_with ~suffix:".Z" name
  && String.fold_left
       (fun count byte -> if byte = '.' then count + 1 else count)
       0 name
     > 1

let resolve resolver spelling =
  if spelling = "" then Error (resolver_error Include_resolver.Empty_path)
  else if String.contains spelling '\000' then
    Error (resolver_error Include_resolver.Path_contains_nul)
  else
    let primary = default_extension spelling in
    match Include_resolver.resolve_file resolver ~spelling:primary with
    | Ok resolution -> Ok (primary, resolution)
    | Error (Include_resolver.Not_found { searched; _ }) -> (
        let alternative =
          if is_dot_z primary then
            String.sub primary 0 (String.length primary - 2)
          else primary ^ ".Z"
        in
        match Include_resolver.resolve_file resolver ~spelling:alternative with
        | Ok resolution -> Ok (alternative, resolution)
        | Error (Include_resolver.Not_found { searched = more; _ }) ->
            Error
              (resolver_error
                 (Include_resolver.Not_found
                    { spelling; searched = searched @ more }))
        | Error error -> Error (resolver_error error))
    | Error error -> Error (resolver_error error)

let same_file (left : Unix.LargeFile.stats) (right : Unix.LargeFile.stats) =
  left.st_dev = right.st_dev && left.st_ino = right.st_ino
  && left.st_kind = right.st_kind

let same_version (left : Unix.LargeFile.stats) (right : Unix.LargeFile.stats) =
  same_file left right
  && left.st_size = right.st_size
  && left.st_mtime = right.st_mtime
  && left.st_ctime = right.st_ctime

let read_file ~resolver ~selected ~resolution ~max_input_bytes ~max_output_bytes
    ~compressed =
  try
    let path = resolution.Include_resolver.canonical_path in
    let before = Unix.LargeFile.stat path in
    if before.st_kind <> Unix.S_REG then
      Error (Printf.sprintf "BINFILE path is not a regular file: %s" path)
    else
      (* Nonblocking open prevents a regular-file/FIFO replacement from
         blocking before the descriptor's kind can be checked. *)
      let descriptor =
        Unix.openfile path [ Unix.O_RDONLY; Unix.O_NONBLOCK ] 0
      in
      let channel = Unix.in_channel_of_descr descriptor in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          set_binary_mode_in channel true;
          let opened = Unix.LargeFile.fstat descriptor in
          let check_path () =
            match Include_resolver.resolve_file resolver ~spelling:selected with
            | Error error -> Error (resolver_error error)
            | Ok current ->
                if
                  (not
                     (Include_resolver.equal_path path current.canonical_path))
                  || not
                       (same_version opened
                          (Unix.LargeFile.stat current.canonical_path))
                then Error "BINFILE file changed while it was being read"
                else Ok ()
          in
          if opened.st_kind <> Unix.S_REG then
            Error (Printf.sprintf "BINFILE path is not a regular file: %s" path)
          else if not (same_version before opened) then
            Error "BINFILE file changed while it was being opened"
          else
            let* () = check_path () in
            let* size =
              checked_size ~name:"BINFILE input size" ~limit:max_input_bytes
                opened.st_size
            in
            let* prefix =
              if compressed then
                if size < header_bytes then
                  Error
                    "BINFILE compressed file has a truncated CArcCompress \
                     header"
                else
                  let prefix = really_input_string channel header_bytes in
                  let* _ =
                    read_header ~max_input_bytes ~max_output_bytes
                      ~raw_size:size prefix
                  in
                  Ok prefix
              else
                let* _ =
                  checked_size ~name:"BINFILE output size"
                    ~limit:max_output_bytes opened.st_size
                in
                Ok ""
            in
            let raw = Bytes.create size in
            let prefix_size = String.length prefix in
            Bytes.blit_string prefix 0 raw 0 prefix_size;
            really_input channel raw prefix_size (size - prefix_size);
            let at_end =
              match input_char channel with
              | _ -> false
              | exception End_of_file -> true
            in
            if
              (not at_end)
              || not (same_version opened (Unix.LargeFile.fstat descriptor))
            then Error "BINFILE file changed while it was being read"
            else
              let* () = check_path () in
              Ok (Bytes.unsafe_to_string raw))
  with
  | End_of_file -> Error "BINFILE file was truncated while it was being read"
  | Sys_error message -> Error ("could not read BINFILE: " ^ message)
  | Unix.Unix_error (error, operation, argument) ->
      Error
        (Printf.sprintf "could not read BINFILE: %s %s: %s" operation argument
           (Unix.error_message error))

let load ~resolver ~max_input_bytes ~max_output_bytes ~spelling =
  let* () = limits ~max_input_bytes ~max_output_bytes in
  let* selected, resolution = resolve resolver spelling in
  let compressed = is_dot_z resolution.Include_resolver.source_path in
  let* raw =
    read_file ~resolver ~selected ~resolution ~max_input_bytes ~max_output_bytes
      ~compressed
  in
  let* bytes, compression =
    decode ~max_input_bytes ~max_output_bytes ~compressed raw
  in
  let sha256 bytes = Digestif.SHA256.(to_hex (digest_string bytes)) in
  let raw_sha256 = sha256 raw in
  let decoded_sha256 = if bytes == raw then raw_sha256 else sha256 bytes in
  Ok
    {
      bytes;
      provenance =
        {
          spelling;
          resolved_path = resolution.canonical_path;
          display_path = resolution.source_path;
          raw_size = String.length raw;
          decoded_size = String.length bytes;
          raw_sha256;
          decoded_sha256;
          compression;
        };
    }
