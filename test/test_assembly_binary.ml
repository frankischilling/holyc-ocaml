open Holyc_lib
module Binary = Assembly_binary

let contains text fragment =
  let rec at offset =
    offset + String.length fragment <= String.length text
    && (String.sub text offset (String.length fragment) = fragment
       || at (offset + 1))
  in
  at 0

let expect_error label fragment = function
  | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label
  | Error message ->
      Alcotest.(check bool)
        (label ^ ": " ^ message)
        true
        (contains message fragment)

let decode ?(max_input_bytes = 100_000) ?(max_output_bytes = 100_000)
    ?(compressed = true) bytes =
  Binary.decode ~max_input_bytes ~max_output_bytes ~compressed bytes

let expect_bytes label expected compression result =
  match result with
  | Error message -> Alcotest.failf "%s: %s" label message
  | Ok (bytes, actual_compression) ->
      Alcotest.(check string) label expected bytes;
      Alcotest.(check bool)
        (label ^ " compression kind")
        true
        (actual_compression = compression)

let put_int64 bytes offset value =
  for index = 0 to 7 do
    Bytes.set bytes (offset + index)
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical value (8 * index)) 255L)))
  done

let archive kind expanded body =
  let bytes = Bytes.make (17 + String.length body) '\000' in
  put_int64 bytes 0 (Int64.of_int (Bytes.length bytes));
  put_int64 bytes 8 (Int64.of_int expanded);
  Bytes.set bytes 16 (Char.chr kind);
  Bytes.blit_string body 0 bytes 17 (String.length body);
  Bytes.unsafe_to_string bytes

let replace_int64 packet offset value =
  let bytes = Bytes.of_string packet in
  put_int64 bytes offset value;
  Bytes.unsafe_to_string bytes

(* These codewords come directly from ArcExpandBuf's two-code dictionary
   construction, independently of a compressor implementation:
   A, B, AB, ABA and 0, FF, 0FF, 0FF0. *)
let seven_bit = archive 2 7 "\x41\x42\x80\x82"
let eight_bit = archive 3 7 "\x00\xfe\x01\x14\x08"

let hand_packed_archives () =
  expect_bytes "CT_7_BIT with the current-entry special case" "ABABABA"
    Binary.Ct_7_bit (decode seven_bit);
  expect_bytes "CT_8_BIT retains high bytes and NUL"
    "\000\255\000\255\000\255\000" Binary.Ct_8_bit (decode eight_bit);
  let raw = "\000\255\r\n\026abc" in
  expect_bytes "raw input preserves every byte" raw Binary.Raw
    (decode ~compressed:false raw);
  expect_bytes "CT_NONE preserves every body byte" raw Binary.Ct_none
    (decode (archive 1 (String.length raw) raw));
  List.iter
    (fun (kind, compression) ->
      expect_bytes "empty archive" "" compression
        (decode ~max_output_bytes:0 (archive kind 0 "")))
    [ (1, Binary.Ct_none); (2, Binary.Ct_7_bit); (3, Binary.Ct_8_bit) ];
  expect_bytes "empty raw file has exact zero limits" "" Binary.Raw
    (decode ~compressed:false ~max_input_bytes:0 ~max_output_bytes:0 "")

(* Literal packets exercise the bit-width schedule without using the decoder's
   dictionary or implementing a compressor. The first growth occurs at code
   127 for CT_7_BIT and code 255 for CT_8_BIT, counting from zero. *)
let pack_codes min_bits codes =
  let alphabet = 1 lsl min_bits in
  let width index =
    let bits = ref (min_bits + 1) in
    while !bits < 12 && alphabet + index + 1 >= 1 lsl !bits do
      incr bits
    done;
    !bits
  in
  let total =
    Array.fold_left ( + ) 0 (Array.mapi (fun index _ -> width index) codes)
  in
  let body = Bytes.make ((total + 7) / 8) '\000' in
  let position = ref 0 in
  Array.iteri
    (fun index code ->
      for bit = 0 to width index - 1 do
        (if code land (1 lsl bit) <> 0 then
           let byte = !position / 8 in
           Bytes.set body byte
             (Char.chr
                (Char.code (Bytes.get body byte) lor (1 lsl (!position mod 8)))));
        incr position
      done)
    codes;
  Bytes.unsafe_to_string body

let dictionary_boundaries () =
  List.iter
    (fun min_bits ->
      let alphabet = 1 lsl min_bits in
      let kind = if min_bits = 7 then 2 else 3 in
      let compression =
        if min_bits = 7 then Binary.Ct_7_bit else Binary.Ct_8_bit
      in
      List.iter
        (fun count ->
          let codes = Array.init count (fun index -> index mod alphabet) in
          let expected =
            String.init count (fun index -> Char.chr codes.(index))
          in
          let bytes = archive kind count (pack_codes min_bits codes) in
          expect_bytes
            (Printf.sprintf "%d-bit alphabet, %d literal codes" min_bits count)
            expected compression
            (decode ~max_input_bytes:(String.length bytes)
               ~max_output_bytes:count bytes))
        [ alphabet - 1; alphabet; alphabet + 1; 2048 - alphabet; 5000 ])
    [ 7; 8 ]

let malformed_headers () =
  for length = 0 to 16 do
    expect_error "partial header" "truncated CArcCompress header"
      (decode (String.make length '\000'))
  done;
  List.iter
    (fun kind ->
      expect_error "unknown compression kind" "unsupported compression type"
        (decode (archive kind 0 "")))
    [ 0; 4; 255 ];
  List.iter
    (fun value ->
      expect_error "negative compressed size" "compressed size is negative"
        (decode (replace_int64 seven_bit 0 value));
      expect_error "negative expanded size" "expanded size is negative"
        (decode (replace_int64 seven_bit 8 value)))
    [ -1L; Int64.min_int ];
  expect_error "oversized compressed size" "declared compressed size"
    (decode (replace_int64 seven_bit 0 Int64.max_int));
  expect_error "oversized expanded size" "expanded size"
    (decode (replace_int64 seven_bit 8 Int64.max_int));
  expect_error "declared input shorter than file" "but the file has"
    (decode (replace_int64 seven_bit 0 20L));
  expect_error "declared input longer than file" "but the file has"
    (decode (replace_int64 seven_bit 0 22L));
  expect_error "truncated CT_NONE body" "CT_NONE payload size"
    (decode (archive 1 4 "abc"));
  expect_error "extra CT_NONE body" "CT_NONE payload size"
    (decode (archive 1 2 "abc"));
  expect_error "empty compressed output with payload" "unexpected payload bytes"
    (decode (archive 2 0 "\000"))

let malformed_codes () =
  expect_error "nonliteral first code" "starts with a nonliteral"
    (decode (archive 2 1 "\x80"));
  expect_error "uninitialized future entry" "undefined dictionary code"
    (decode (archive 2 3 "\x41\x81"));
  expect_error "truncated 9-bit code" "ends before its declared"
    (decode (archive 3 1 "\x41"));
  expect_error "missing final code" "ends before its declared"
    (decode (archive 2 7 "\x41\x42\x80"));
  expect_error "expanded count truncates a dictionary word"
    "exceeds its declared"
    (decode (replace_int64 seven_bit 8 6L));
  expect_error "expanded count requires another code" "ends before its declared"
    (decode (replace_int64 seven_bit 8 8L));
  expect_error "unused compressed bytes" "trailing bytes"
    (decode (archive 2 1 "\x41\x42"));
  (* At this point ArcEntryGet has wrapped from entry 4095 to entry 128.
     Referencing retired entries 128,129 makes the new entry 128 its own
     prefix; the final 128 code must terminate with a diagnostic. *)
  let codes = Array.append (Array.make 3967 65) [| 128; 129; 128 |] in
  expect_error "cyclic reused dictionary entry" "cyclic dictionary chain"
    (decode (archive 2 3973 (pack_codes 7 codes)))

let exact_decode_limits () =
  expect_bytes "exact compressed and expanded byte limits" "ABABABA"
    Binary.Ct_7_bit
    (decode ~max_input_bytes:21 ~max_output_bytes:7 seven_bit);
  expect_error "input limit one byte short" "input size"
    (decode ~max_input_bytes:20 seven_bit);
  expect_error "output limit one byte short" "expanded size"
    (decode ~max_output_bytes:6 seven_bit);
  expect_bytes "exact raw limits" "abc" Binary.Raw
    (decode ~compressed:false ~max_input_bytes:3 ~max_output_bytes:3 "abc");
  expect_error "raw output limit one byte short" "output size"
    (decode ~compressed:false ~max_output_bytes:2 "abc");
  expect_error "negative input limit" "input size limit must be nonnegative"
    (decode ~max_input_bytes:(-1) "");
  expect_error "negative output limit" "output size limit must be nonnegative"
    (decode ~max_output_bytes:(-1) "")

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_directory run =
  let path = Filename.temp_dir "holyc-assembly-binary-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path bytes =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes)

let resolver ?include_roots ?templeos_root working_directory =
  match
    Include_resolver.create ~working_directory ?include_roots ?templeos_root ()
  with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let load ?(max_input_bytes = 100_000) ?(max_output_bytes = 100_000) resolver
    spelling =
  Binary.load ~resolver ~max_input_bytes ~max_output_bytes ~spelling

let loaded = function
  | Ok result -> result
  | Error message -> Alcotest.fail message

let file_bytes_and_provenance () =
  with_directory (fun directory ->
      write_file (Filename.concat directory "plain.BIN") "abc";
      let resolver = resolver directory in
      let plain =
        load ~max_input_bytes:3 ~max_output_bytes:3 resolver "plain" |> loaded
      in
      Alcotest.(check string) "default BIN extension" "abc" plain.bytes;
      let provenance = plain.provenance in
      Alcotest.(check string) "original spelling" "plain" provenance.spelling;
      Alcotest.(check string)
        "canonical path"
        (Unix.realpath (Filename.concat directory "plain.BIN"))
        provenance.resolved_path;
      Alcotest.(check string)
        "display path"
        (Filename.concat
           (Include_resolver.working_directory resolver)
           "plain.BIN")
        provenance.display_path;
      Alcotest.(check int) "raw size" 3 provenance.raw_size;
      Alcotest.(check int) "decoded size" 3 provenance.decoded_size;
      Alcotest.(check string)
        "raw SHA-256"
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        provenance.raw_sha256;
      Alcotest.(check string)
        "decoded SHA-256" provenance.raw_sha256 provenance.decoded_sha256;
      Alcotest.(check bool)
        "raw provenance" true
        (provenance.compression = Binary.Raw);
      let all_bytes = String.init 256 Char.chr in
      write_file (Filename.concat directory "all.DAT") all_bytes;
      Alcotest.(check string)
        "all 256 bytes survive file reading" all_bytes
        (load resolver "all.DAT" |> loaded).bytes;
      expect_error "file input limit" "input size"
        (load ~max_input_bytes:2 resolver "plain");
      expect_error "file output limit" "output size"
        (load ~max_output_bytes:2 resolver "plain"))

let filename_and_compression_rules () =
  with_directory (fun directory ->
      let resolver = resolver directory in
      write_file (Filename.concat directory "seven.BIN.Z") seven_bit;
      let result =
        load ~max_input_bytes:21 ~max_output_bytes:7 resolver "seven" |> loaded
      in
      Alcotest.(check string)
        "default BIN then compressed counterpart" "ABABABA" result.bytes;
      Alcotest.(check int) "compressed raw size" 21 result.provenance.raw_size;
      Alcotest.(check int)
        "compressed decoded size" 7 result.provenance.decoded_size;
      Alcotest.(check string)
        "compressed raw hash from independent SHA-256"
        "7cd2a826e9f536e1469b66a27908a37184152c65ece059e2e5e657d4922e02e8"
        result.provenance.raw_sha256;
      Alcotest.(check string)
        "decoded hash from independent SHA-256"
        "5f8759998bc50f1afa6549f25aa6786d52c6e2f1172ffda616b6a230e3963f4f"
        result.provenance.decoded_sha256;
      Alcotest.(check bool)
        "compressed provenance kind" true
        (result.provenance.compression = Binary.Ct_7_bit);
      write_file (Filename.concat directory "eight.BIN.Z") eight_bit;
      Alcotest.(check string)
        "CT_8_BIT file expansion" "\000\255\000\255\000\255\000"
        (load resolver "eight.BIN.Z" |> loaded).bytes;
      write_file (Filename.concat directory "stored.BIN.Z") (archive 1 3 "abc");
      let stored = load resolver "stored.BIN.Z" |> loaded in
      Alcotest.(check string) "CT_NONE file expansion" "abc" stored.bytes;
      Alcotest.(check bool)
        "CT_NONE remains distinct from raw provenance" true
        (stored.provenance.compression = Binary.Ct_none);
      expect_error "compressed file output preflight" "expanded size"
        (load ~max_output_bytes:6 resolver "seven");
      write_file (Filename.concat directory "seven.BIN") "primary";
      Alcotest.(check string)
        "named raw file wins" "primary" (load resolver "seven" |> loaded).bytes;
      Alcotest.(check string)
        "explicit compressed file wins" "ABABABA"
        (load resolver "seven.BIN.Z" |> loaded).bytes;
      write_file (Filename.concat directory "other.BIN") "fallback";
      Alcotest.(check string)
        "explicit Z falls back to raw counterpart" "fallback"
        (load resolver "other.BIN.Z" |> loaded).bytes;
      write_file (Filename.concat directory "bad.BIN") "raw fallback";
      write_file (Filename.concat directory "bad.BIN.Z") "bad";
      expect_error "invalid compressed file never becomes raw"
        "truncated CArcCompress"
        (load resolver "bad.BIN.Z");
      write_file (Filename.concat directory "single.Z") "raw single extension";
      Alcotest.(check string)
        "single .Z is an ordinary extension" "raw single extension"
        (load resolver "single.Z" |> loaded).bytes;
      write_file (Filename.concat directory "lower.BIN.z") "lowercase raw";
      Alcotest.(check string)
        "IsDotZ is case sensitive" "lowercase raw"
        (load resolver "lower.BIN.z" |> loaded).bytes;
      Unix.mkdir (Filename.concat directory "dir.ext") 0o700;
      write_file (Filename.concat directory "dir.ext/payload") "exact";
      Alcotest.(check string)
        "FileExtDot sees the caller's dotted directory" "exact"
        (load resolver "dir.ext/payload" |> loaded).bytes)

let root_capabilities_and_file_failures () =
  with_directory (fun directory ->
      let inside = Filename.concat directory "inside" in
      let outside = Filename.concat directory "outside" in
      Unix.mkdir inside 0o700;
      Unix.mkdir outside 0o700;
      write_file (Filename.concat outside "secret.BIN") "private";
      write_file (Filename.concat outside "seven.BIN.Z") seven_bit;
      let restricted = resolver inside in
      expect_error "relative path escapes configured roots"
        "outside the allowed roots"
        (load restricted "../outside/secret.BIN");
      expect_error "missing input" "could not find BINFILE"
        (load restricted "missing");
      expect_error "empty input path" "path cannot be empty"
        (load restricted "");
      expect_error "NUL input path" "NUL byte" (load restricted "bad\000.BIN");
      expect_error "unmapped TempleOS root" "requires an explicit mapping"
        (load restricted "::/secret.BIN");
      expect_error "unmapped home" "home path requires"
        (load restricted "~/secret.BIN");
      Unix.mkdir (Filename.concat inside "directory.BIN") 0o700;
      expect_error "directory input" "is a directory"
        (load restricted "directory.BIN");
      let includes = resolver ~include_roots:[ outside ] inside in
      Alcotest.(check string)
        "explicit include root permits binary reads" "private"
        (load includes "secret" |> loaded).bytes;
      let mapped = resolver ~templeos_root:outside inside in
      Alcotest.(check string)
        "TempleOS root mapping applies before expansion" "ABABABA"
        (load mapped "::/seven.BIN.Z" |> loaded).bytes;
      if not Sys.win32 then (
        Unix.symlink
          (Filename.concat outside "secret.BIN")
          (Filename.concat inside "escape.BIN");
        expect_error "symlink escapes configured roots"
          "outside the allowed roots"
          (load restricted "escape.BIN");
        Unix.mkfifo (Filename.concat inside "pipe.BIN") 0o600;
        expect_error "FIFO input is refused before open" "not a regular file"
          (load restricted "pipe.BIN")))

let tests =
  [
    Alcotest.test_case "original hand-packed archive codewords" `Quick
      hand_packed_archives;
    Alcotest.test_case "variable code widths and 12-bit leaf replacement" `Quick
      dictionary_boundaries;
    Alcotest.test_case "archive header preflight" `Quick malformed_headers;
    Alcotest.test_case "malformed and cyclic compressed streams" `Quick
      malformed_codes;
    Alcotest.test_case "inclusive byte limits" `Quick exact_decode_limits;
    Alcotest.test_case "raw files and exact provenance" `Quick
      file_bytes_and_provenance;
    Alcotest.test_case "original extension and compression selection" `Quick
      filename_and_compression_rules;
    Alcotest.test_case "root capabilities and regular files" `Quick
      root_capabilities_and_file_failures;
  ]
