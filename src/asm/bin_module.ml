module Spec = Generated.Bin_records
module Entry = Spec.Entry

type heap = { name : string; size : int64; offsets : int list }

type record =
  | Import of { kind : Entry.t; offset : int; name : string }
  | Export of { kind : Entry.t; value : int64; name : string }
  | Absolute_addresses of int list
  | Code_heap of { zeroed : bool; heap : heap }
  | Data_heap of { zeroed : bool; heap : heap }
  | Main of int

type error = {
  offset : int option;
  record_index : int option;
  message : string;
}

type t = {
  bytes_ : string;
  body_ : string;
  records_ : record list;
  origin_ : int64 option;
  alignment_bits_ : int;
  patch_table_offset_ : int;
  table_end_ : int;
  record_offsets_ : int list;
  work_ : int;
}

let max_file_bytes = min (16 * 1024 * 1024) Sys.max_string_length
let copy_string value = Bytes.to_string (Bytes.of_string value)

let copy_record = function
  | Import r -> Import { r with name = copy_string r.name }
  | Export r -> Export { r with name = copy_string r.name }
  | Code_heap r ->
      Code_heap { r with heap = { r.heap with name = copy_string r.heap.name } }
  | Data_heap r ->
      Data_heap { r with heap = { r.heap with name = copy_string r.heap.name } }
  | (Absolute_addresses _ | Main _) as record -> record

let bytes t = copy_string t.bytes_
let body t = copy_string t.body_
let records t = List.map copy_record t.records_
let origin t = t.origin_
let alignment_bits t = t.alignment_bits_
let patch_table_offset t = t.patch_table_offset_
let table_end t = t.table_end_
let file_size t = String.length t.bytes_
let record_offsets t = t.record_offsets_
let work t = t.work_

exception Invalid of error

let invalid ?offset ?record_index message =
  raise (Invalid { offset; record_index; message })

let record_entry = function
  | Import r -> r.kind
  | Export r -> r.kind
  | Absolute_addresses _ -> Entry.Abs_addr
  | Code_heap { zeroed = false; _ } -> Entry.Code_heap
  | Code_heap { zeroed = true; _ } -> Entry.Zeroed_code_heap
  | Data_heap { zeroed = false; _ } -> Entry.Data_heap
  | Data_heap { zeroed = true; _ } -> Entry.Zeroed_data_heap
  | Main _ -> Entry.Main

let record_name = function
  | Import r -> r.name
  | Export r -> r.name
  | Code_heap r -> r.heap.name
  | Data_heap r -> r.heap.name
  | Absolute_addresses _ | Main _ -> ""

let record_value = function
  | Import r -> Int64.of_int r.offset
  | Export r -> r.value
  | Main offset -> Int64.of_int offset
  | Absolute_addresses offsets -> Int64.of_int (List.length offsets)
  | Code_heap r -> Int64.of_int (List.length r.heap.offsets)
  | Data_heap r -> Int64.of_int (List.length r.heap.offsets)

let references = function
  | Absolute_addresses offsets -> offsets
  | Code_heap r -> r.heap.offsets
  | Data_heap r -> r.heap.offsets
  | Import _ | Export _ | Main _ -> []

let payload_size record =
  let base =
    match record with
    | Code_heap _ -> 4
    | Data_heap _ -> 8
    | _ -> 0
  in
  base + (4 * List.length (references record))

let record_size record =
  6 + String.length (record_name record) + payload_size record

let get_word input offset width =
  let value = ref 0L in
  for i = 0 to width - 1 do
    value :=
      Int64.logor !value
        (Int64.shift_left (Int64.of_int (Char.code input.[offset + i])) (8 * i))
  done;
  !value

let put_word output offset width value =
  for i = 0 to width - 1 do
    Bytes.set output (offset + i)
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical value (8 * i)) 255L)))
  done

let budget max_bytes max_work =
  if max_bytes <= 0 || max_bytes > max_file_bytes then
    invalid "max_bytes must be positive and at most 16 MiB";
  if max_work <= 0 || max_work > 100_000_000 then
    invalid "max_work must be between 1 and 100000000";
  let used = ref 0 in
  let charge amount =
    if amount < 0 || amount > max_work - !used then
      invalid "module work exceeds max_work";
    used := !used + amount
  in
  (used, charge)

let check_alignment alignment_bits =
  if alignment_bits < 0 || alignment_bits > 62 then
    invalid "alignment_bits must select a positive signed 64-bit alignment"

let validate_records charge body records =
  let length = String.length body in
  let import_group = ref false in
  List.iteri
    (fun record_index record ->
      let bad message = invalid ~record_index message in
      charge 1;
      let entry = record_entry record in
      let info = Entry.info entry in
      let name = record_name record in
      charge (String.length name);
      if String.contains name '\000' then
        bad "record names must not contain NUL";
      let u32 value =
        if value < 0L || value > 0xffff_ffffL then
          bad "record value must fit its original U32 envelope"
      in
      u32 (record_value record);
      let field offset width =
        charge 1;
        if offset < 0 || width > length || offset > length - width then
          bad "patch field extends beyond the module body"
      in
      if info.category <> Spec.Import then (
        if !import_group && name = "" then
          bad "an import group would consume this empty-name nonimport record";
        import_group := false);
      match record with
      | Import r ->
          if info.category <> Spec.Import then
            bad "import uses a nonimport IET kind";
          if name = "" && not !import_group then
            bad "an import group must begin with a nonempty name";
          import_group := true;
          let relocation =
            match info.relocation with
            | Some relocation -> relocation
            | None -> bad "import has no source field shape"
          in
          field r.offset (Spec.width_bytes relocation.width)
      | Export r -> (
          if info.category <> Spec.Export then
            bad "export uses a nonexport IET kind";
          if name = "" then bad "an export must have a name";
          match info.pass1 with
          | Spec.Register_relative_export when r.value > Int64.of_int length ->
              bad "relative export extends beyond the module body"
          | _ -> ())
      | Absolute_addresses offsets ->
          List.iter (fun offset -> field offset 4) offsets
      | Code_heap r ->
          if r.heap.size < 0L || r.heap.size > 0x7fff_ffffL then
            bad "code heap size must fit a nonnegative I32";
          List.iter (fun offset -> field offset 4) r.heap.offsets
      | Data_heap r ->
          if r.heap.size < 0L then bad "data heap size must be nonnegative";
          List.iter (fun offset -> field offset 8) r.heap.offsets
      | Main offset ->
          if offset < 0 || offset >= length then
            bad "MAIN must identify a byte in the module body")
    records

let create ?(max_bytes = max_file_bytes) ?(max_work = 1_000_000) ~alignment_bits
    ~origin ~body records =
  try
    let used, charge = budget max_bytes max_work in
    check_alignment alignment_bits;
    let body_size = String.length body in
    if body_size > max_bytes - Spec.header_size - 1 then
      invalid "module body exceeds max_bytes";
    charge body_size;
    validate_records charge body records;
    let patch_table_offset_ = Spec.header_size + body_size in
    let cursor = ref patch_table_offset_ in
    let offsets =
      List.map
        (fun record ->
          let start = !cursor in
          let size = record_size record in
          if size > max_bytes - !cursor - 1 then
            invalid "patch table exceeds max_bytes";
          cursor := !cursor + size;
          start)
        records
    in
    let table_end_ = !cursor + 1 in
    let padding = (16 - (table_end_ mod 16)) mod 16 in
    if padding > max_bytes - table_end_ then
      invalid "padded module exceeds max_bytes";
    let size = table_end_ + padding in
    charge (size - body_size);
    let output = Bytes.make size '\000' in
    put_word output 0 2 0x1eebL;
    put_word output 2 1 (Int64.of_int alignment_bits);
    put_word output 4 4 (Int64.of_int32 Spec.signature_value);
    put_word output 8 8 (Option.value ~default:Int64.max_int origin);
    put_word output 16 8 (Int64.of_int patch_table_offset_);
    put_word output 24 8 (Int64.of_int size);
    Bytes.blit_string body 0 output Spec.header_size body_size;
    List.iter2
      (fun start record ->
        put_word output start 1
          (Int64.of_int (Entry.to_code (record_entry record)));
        put_word output (start + 1) 4 (record_value record);
        let name = record_name record in
        Bytes.blit_string name 0 output (start + 5) (String.length name);
        let payload = start + 6 + String.length name in
        let width, heap_size =
          match record with
          | Code_heap r -> (4, r.heap.size)
          | Data_heap r -> (8, r.heap.size)
          | _ -> (0, 0L)
        in
        put_word output payload width heap_size;
        List.iteri
          (fun i offset ->
            put_word output (payload + width + (4 * i)) 4 (Int64.of_int offset))
          (references record))
      offsets records;
    let origin_ = if origin = Some Int64.max_int then None else origin in
    Ok
      {
        bytes_ = Bytes.unsafe_to_string output;
        body_ = copy_string body;
        records_ = List.map copy_record records;
        origin_;
        alignment_bits_ = alignment_bits;
        patch_table_offset_;
        table_end_;
        record_offsets_ = offsets;
        work_ = !used;
      }
  with Invalid error -> Error error

let read ?(max_bytes = max_file_bytes) ?(max_work = 1_000_000) input =
  try
    let used, charge = budget max_bytes max_work in
    let size = String.length input in
    if size > max_bytes then invalid ~offset:0 "module input exceeds max_bytes";
    charge size;
    if size < Spec.header_size then invalid ~offset:0 "truncated module header";
    if get_word input 0 2 <> 0x1eebL then
      invalid ~offset:0 "invalid header jump";
    if get_word input 3 1 <> 0L then
      invalid ~offset:3 "reserved header byte is nonzero";
    if get_word input 4 4 <> Int64.of_int32 Spec.signature_value then
      invalid ~offset:4 "invalid TOSB signature";
    let alignment_bits_ = Char.code input.[2] in
    check_alignment alignment_bits_;
    if size mod 16 <> 0 || get_word input 24 8 <> Int64.of_int size then
      invalid ~offset:24
        "file size must match the complete 16-byte-aligned input";
    let patch = get_word input 16 8 in
    if patch < Int64.of_int Spec.header_size || patch >= Int64.of_int size then
      invalid ~offset:16 "patch table offset is outside the module";
    let patch_table_offset_ = Int64.to_int patch in
    let cursor = ref patch_table_offset_ in
    let ensure count =
      if count < 0 || count > size - !cursor then
        invalid ~offset:!cursor "truncated patch record"
    in
    let take width =
      ensure width;
      let value = get_word input !cursor width in
      cursor := !cursor + width;
      value
    in
    let name () =
      let start = !cursor in
      while !cursor < size && input.[!cursor] <> '\000' do
        incr cursor
      done;
      if !cursor = size then invalid ~offset:start "unterminated patch name";
      charge (!cursor - start);
      let value = String.sub input start (!cursor - start) in
      incr cursor;
      value
    in
    let offsets count =
      if count > Int64.of_int ((size - !cursor) / 4) then
        invalid ~offset:!cursor "truncated patch offset list";
      let count = Int64.to_int count in
      charge count;
      List.init count (fun _ -> Int64.to_int (take 4))
    in
    let rec table collected starts =
      let start = !cursor in
      let code = Int64.to_int (take 1) in
      if code = 0 then (List.rev collected, List.rev starts)
      else (
        charge 1;
        let kind =
          match Entry.of_code code with
          | Some kind -> kind
          | None -> invalid ~offset:start "reserved or unknown IET record"
        in
        let value = take 4 in
        let name = name () in
        let info = Entry.info kind in
        let record =
          match info.category with
          | Spec.Import ->
              charge 1;
              Import { kind; offset = Int64.to_int value; name }
          | Spec.Export -> Export { kind; value; name }
          | Spec.Absolute_addresses ->
              if name <> "" then
                invalid ~offset:start "absolute record name must be empty";
              Absolute_addresses (offsets value)
          | Spec.Code_heap | Spec.Data_heap ->
              let width = if info.category = Spec.Code_heap then 4 else 8 in
              let bytes = take width in
              let heap_size =
                if width = 4 then
                  Int64.shift_right (Int64.shift_left bytes 32) 32
                else bytes
              in
              let heap = { name; size = heap_size; offsets = offsets value } in
              if width = 4 then
                Code_heap { zeroed = kind = Entry.Zeroed_code_heap; heap }
              else Data_heap { zeroed = kind = Entry.Zeroed_data_heap; heap }
          | Spec.Main ->
              if name <> "" then invalid ~offset:start "MAIN name must be empty";
              Main (Int64.to_int value)
          | Spec.Terminator -> assert false
        in
        table (record :: collected) (start :: starts))
    in
    let records_, record_offsets_ = table [] [] in
    let table_end_ = !cursor in
    if size - table_end_ > 15 then
      invalid ~offset:table_end_ "excess trailing module data";
    for i = table_end_ to size - 1 do
      if input.[i] <> '\000' then
        invalid ~offset:i "module padding must be zero"
    done;
    let body_ =
      String.sub input Spec.header_size (patch_table_offset_ - Spec.header_size)
    in
    validate_records (fun _ -> ()) body_ records_;
    let org = get_word input 8 8 in
    Ok
      {
        bytes_ = copy_string input;
        body_;
        records_;
        origin_ = (if org = Int64.max_int then None else Some org);
        alignment_bits_;
        patch_table_offset_;
        table_end_;
        record_offsets_;
        work_ = !used;
      }
  with Invalid error -> Error error

let map t =
  let buffer = Buffer.create 128 in
  Buffer.add_string buffer "holyc-templeos-bin-map-v1\n";
  Buffer.add_string buffer
    (Printf.sprintf "body 00000020 %08X\n" (String.length t.body_));
  List.iter2
    (fun offset record ->
      Buffer.add_string buffer
        (Printf.sprintf "record %08X %s %LX %S\n" offset
           (Entry.to_source_name (record_entry record))
           (record_value record) (record_name record)))
    t.record_offsets_ t.records_;
  Buffer.add_string buffer (Printf.sprintf "end %08X\n" (t.table_end_ - 1));
  Buffer.contents buffer

let to_yojson t =
  let integer value = `String (Int64.to_string value) in
  `Assoc
    [
      ("schema", `String "holyc-templeos-bin-v1");
      ("reference_commit", `String Spec.reference_commit);
      ("file_size", `Int (file_size t));
      ("body_size", `Int (String.length t.body_));
      ("patch_table_offset", `Int t.patch_table_offset_);
      ("table_end", `Int t.table_end_);
      ("alignment_bits", `Int t.alignment_bits_);
      ( "origin",
        match t.origin_ with
        | None -> `Null
        | Some org -> integer org );
      ("sha256", `String Digestif.SHA256.(to_hex (digest_string t.bytes_)));
      ( "records",
        `List
          (List.map2
             (fun offset record ->
               let fields =
                 [
                   ("offset", `Int offset);
                   ("type", `String (Entry.to_source_name (record_entry record)));
                   ("code", `Int (Entry.to_code (record_entry record)));
                   ("value", integer (record_value record));
                   ("name", `String (record_name record));
                   ( "references",
                     `List (List.map (fun n -> `Int n) (references record)) );
                 ]
               in
               let fields =
                 match record with
                 | Code_heap r ->
                     fields
                     @ [
                         ("heap_size", integer r.heap.size);
                         ("zeroed", `Bool r.zeroed);
                       ]
                 | Data_heap r ->
                     fields
                     @ [
                         ("heap_size", integer r.heap.size);
                         ("zeroed", `Bool r.zeroed);
                       ]
                 | _ -> fields
               in
               `Assoc fields)
             t.record_offsets_ t.records_) );
    ]

let records_of_yojson ?(max_work = 1_000_000) json =
  try
    let _, charge = budget max_file_bytes max_work in
    let objects =
      match json with
      | `List objects -> objects
      | _ -> invalid "record input must be a JSON array"
    in
    Ok
      (List.mapi
         (fun record_index json ->
           charge 1;
           let bad message = invalid ~record_index message in
           let fields =
             match json with
             | `Assoc fields -> fields
             | _ -> bad "each record must be a JSON object"
           in
           charge (List.length fields);
           let field key =
             match List.assoc_opt key fields with
             | Some value -> value
             | None -> bad ("missing record field: " ^ key)
           in
           let string key =
             match field key with
             | `String value ->
                 charge (String.length value);
                 value
             | _ -> bad (key ^ " must be a string")
           in
           let integer_json key = function
             | `Int value -> Int64.of_int value
             | `Intlit value | `String value -> (
                 charge (String.length value);
                 try Int64.of_string value
                 with Failure _ -> bad (key ^ " exceeds I64"))
             | _ -> bad (key ^ " must be an exact integer or integer string")
           in
           let integer key = integer_json key (field key) in
           let offset_json key json =
             let value = integer_json key json in
             if value < 0L || value > 0xffff_ffffL then
               bad (key ^ " exceeds U32");
             Int64.to_int value
           in
           let offset key = offset_json key (field key) in
           let offsets () =
             match field "offsets" with
             | `List values ->
                 List.map
                   (fun value ->
                     charge 1;
                     offset_json "offset" value)
                   values
             | _ -> bad "offsets must be an array"
           in
           let kind =
             match Entry.of_source_name (string "type") with
             | Some kind -> kind
             | None -> bad "unknown original IET type name"
           in
           let info = Entry.info kind in
           let keys, record =
             match info.category with
             | Spec.Import ->
                 ( [ "type"; "offset"; "name" ],
                   Import
                     { kind; offset = offset "offset"; name = string "name" } )
             | Spec.Export ->
                 ( [ "type"; "value"; "name" ],
                   Export
                     { kind; value = integer "value"; name = string "name" } )
             | Spec.Absolute_addresses ->
                 ([ "type"; "offsets" ], Absolute_addresses (offsets ()))
             | Spec.Code_heap | Spec.Data_heap ->
                 let heap =
                   {
                     name = string "name";
                     size = integer "size";
                     offsets = offsets ();
                   }
                 in
                 let zeroed =
                   kind = Entry.Zeroed_code_heap
                   || kind = Entry.Zeroed_data_heap
                 in
                 ( [ "type"; "name"; "size"; "offsets" ],
                   if info.category = Spec.Code_heap then
                     Code_heap { zeroed; heap }
                   else Data_heap { zeroed; heap } )
             | Spec.Main -> ([ "type"; "offset" ], Main (offset "offset"))
             | Spec.Terminator -> bad "IET_END is supplied automatically"
           in
           if
             List.length fields <> List.length keys
             || List.exists
                  (fun key ->
                    List.length
                      (List.filter (fun (name, _) -> name = key) fields)
                    <> 1)
                  keys
           then bad "record fields must be unique and match their IET shape";
           record)
         objects)
  with Invalid error -> Error error
