open Holyc_lib
module Bin = Templeos_bin
module Entry = Bin.Entry

let hex value =
  String.concat ""
    (List.init (String.length value) (fun i ->
         Printf.sprintf "%02x" (Char.code value.[i])))

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let get = function
  | Ok value -> value
  | Error (e : Bin.error) -> Alcotest.fail e.message

let create ?max_bytes ?max_work ?(body = "\195") ?(alignment_bits = 0)
    ?(origin = None) records =
  Bin.create ?max_bytes ?max_work ~alignment_bits ~origin ~body records

let check_error label = function
  | Ok _ -> Alcotest.fail (label ^ " unexpectedly succeeded")
  | Error _ -> ()

let literal_header () =
  let module_ = get (create []) in
  Alcotest.(check string)
    "native CMain empty-table envelope"
    "eb1e0000544f5342ffffffffffffff7f21000000000000003000000000000000c3000000000000000000000000000000"
    (hex (Bin.bytes module_));
  let empty = get (create ~body:"" []) in
  Alcotest.(check int) "empty body padded file" 48 (Bin.file_size empty);
  Alcotest.(check int)
    "empty body patch offset" 32
    (Bin.patch_table_offset empty);
  Alcotest.(check (option int64)) "INVALID_PTR origin" None (Bin.origin empty);
  let fixed = get (create ~alignment_bits:12 ~origin:(Some 0x123456789L) []) in
  Alcotest.(check string)
    "literal alignment and origin" "0c00544f53428967452301000000"
    (hex (String.sub (Bin.bytes fixed) 2 14))

let record_shapes () =
  let body = String.make 64 '\000' in
  let cases =
    [
      ( Bin.Export { kind = Entry.Rel32_export; value = 2L; name = "R" },
        "10020000005200" );
      ( Bin.Export { kind = Entry.Imm32_export; value = 0xdeadbeefL; name = "I" },
        "11efbeadde4900" );
      ( Bin.Export { kind = Entry.Rel64_export; value = 3L; name = "R" },
        "12030000005200" );
      ( Bin.Export { kind = Entry.Imm64_export; value = 0x89abcdefL; name = "I" },
        "13efcdab894900" );
      ( Bin.Absolute_addresses [ 1; 5; 1 ],
        "140300000000010000000500000001000000" );
      ( Bin.Code_heap
          {
            zeroed = false;
            heap = { name = "C"; size = 0x1234L; offsets = [ 4; 8 ] };
          },
        "15020000004300341200000400000008000000" );
      ( Bin.Code_heap
          { zeroed = true; heap = { name = ""; size = 0L; offsets = [] } },
        "16000000000000000000" );
      ( Bin.Data_heap
          {
            zeroed = false;
            heap = { name = "D"; size = 0x100000001L; offsets = [ 16 ] };
          },
        "17010000004400010000000100000010000000" );
      ( Bin.Data_heap
          {
            zeroed = true;
            heap = { name = ""; size = 17L; offsets = [ 24; 32 ] };
          },
        "18020000000011000000000000001800000020000000" );
      (Bin.Main 7, "190700000000");
    ]
  in
  let imports =
    List.init 10 (fun i ->
        let code = i + 2 in
        let kind = Option.get (Entry.of_code code) in
        ( Bin.Import { kind; offset = 9; name = "X" },
          Printf.sprintf "%02x090000005800" code ))
  in
  List.iter
    (fun (record, expected) ->
      let module_ = get (create ~body [ record ]) in
      let patch = Bin.patch_table_offset module_ in
      Alcotest.(check string)
        (Entry.to_source_name (Bin.record_entry record))
        expected
        (hex
           (String.sub (Bin.bytes module_) patch (String.length expected / 2)));
      ignore (get (Bin.read (Bin.bytes module_))))
    (cases @ imports)

let budgets () =
  let records =
    [
      Bin.Export { kind = Entry.Rel32_export; value = 0L; name = "X" };
      Bin.Main 0;
      Bin.Import { kind = Entry.Imm_u8; offset = 0; name = "External" };
    ]
  in
  let module_ = get (create records) in
  let length = Bin.file_size module_ and work = Bin.work module_ in
  ignore (get (create ~max_bytes:length ~max_work:work records));
  check_error "one below file" (create ~max_bytes:(length - 1) records);
  check_error "one below writer work" (create ~max_work:(work - 1) records);
  let read =
    get (Bin.read ~max_bytes:length ~max_work:work (Bin.bytes module_))
  in
  Alcotest.(check int) "writer/reader work agreement" work (Bin.work read);
  check_error "one below reader work"
    (Bin.read ~max_work:(work - 1) (Bin.bytes module_));
  List.iter
    (fun limit -> check_error "invalid bytes" (create ~max_bytes:limit []))
    [ -1; 0; Bin.max_file_bytes + 1 ];
  List.iter
    (fun limit -> check_error "invalid work" (create ~max_work:limit []))
    [ -1; 0; 100_000_001 ];
  List.iter
    (fun bits ->
      check_error "invalid alignment" (create ~alignment_bits:bits []))
    [ -1; 63; 255 ]

let invalid_records () =
  let body = String.make 8 '\000' in
  List.iter
    (fun record -> check_error "invalid record" (create ~body [ record ]))
    [
      Bin.Import { kind = Entry.Main; offset = 0; name = "X" };
      Bin.Import { kind = Entry.Imm_u8; offset = 0; name = "" };
      Bin.Import { kind = Entry.Imm_i64; offset = 1; name = "X" };
      Bin.Import { kind = Entry.Imm_u8; offset = -1; name = "X" };
      Bin.Export { kind = Entry.Imm_u8; value = 0L; name = "X" };
      Bin.Export
        { kind = Entry.Imm64_export; value = 0x1_0000_0000L; name = "X" };
      Bin.Export { kind = Entry.Imm32_export; value = -1L; name = "X" };
      Bin.Export { kind = Entry.Rel32_export; value = 9L; name = "X" };
      Bin.Export { kind = Entry.Rel32_export; value = 0L; name = "" };
      Bin.Export { kind = Entry.Imm32_export; value = 0L; name = "bad\000name" };
      Bin.Absolute_addresses [ 5 ];
      Bin.Code_heap
        {
          zeroed = false;
          heap = { name = ""; size = 0x80000000L; offsets = [] };
        };
      Bin.Code_heap
        { zeroed = true; heap = { name = ""; size = -1L; offsets = [] } };
      Bin.Data_heap
        { zeroed = true; heap = { name = ""; size = -1L; offsets = [] } };
      Bin.Main 8;
    ];
  let head = Bin.Import { kind = Entry.Imm_u8; offset = 0; name = "X" } in
  check_error "MAIN swallowed by import group"
    (create ~body [ head; Bin.Main 0 ]);
  check_error "unnamed heap swallowed"
    (create ~body
       [
         head;
         Bin.Data_heap
           { zeroed = true; heap = { name = ""; size = 0L; offsets = [] } };
       ]);
  ignore
    (get
       (create ~body
          [
            head;
            Bin.Export { kind = Entry.Imm32_export; value = 7L; name = "X" };
            Bin.Main 0;
          ]));
  ignore
    (get
       (create ~body
          [ Bin.Import { kind = Entry.Rel_i0; offset = 8; name = "X" } ]))

let malformed () =
  let valid = Bin.bytes (get (create [])) in
  let mutate offset hex =
    let b = Bytes.of_string valid in
    Bytes.blit_string (bytes hex) 0 b offset (String.length hex / 2);
    Bytes.to_string b
  in
  List.iter
    (fun (label, input) -> check_error label (Bin.read input))
    [
      ("jump", mutate 0 "00");
      ("signature", mutate 4 "00");
      ("reserved", mutate 3 "01");
      ("alignment", mutate 2 "40");
      ("size", mutate 24 "2f");
      ("offset", mutate 16 "00");
      ("negative offset", mutate 16 "ffffffffffffffff");
      ("record gap", mutate 33 "01");
      ("nonzero padding", mutate 47 "01");
      ("extra block", valid ^ String.make 16 '\000');
    ];
  for length = 0 to String.length valid - 1 do
    check_error "every header/module truncation"
      (Bin.read (String.sub valid 0 length))
  done;
  let input records =
    Bin.bytes (get (create ~body:(String.make 16 '\000') records))
  in
  let patch = 48 in
  List.iter
    (fun records ->
      let raw = input records in
      let b = Bytes.of_string raw in
      for i = patch + 5 to Bytes.length b - 1 do
        Bytes.set b i 'X'
      done;
      check_error "unterminated name" (Bin.read (Bytes.to_string b)))
    [
      [ Bin.Export { kind = Entry.Imm32_export; value = 3L; name = "X" } ];
      [ Bin.Main 0 ];
    ];
  let raw =
    Bytes.of_string
      (input
         [
           Bin.Data_heap
             {
               zeroed = false;
               heap = { name = ""; size = 8L; offsets = [ 0 ] };
             };
         ])
  in
  Bytes.blit_string "\255\255\255\255" 0 raw (patch + 1) 4;
  check_error "payload count overflow before allocation"
    (Bin.read (Bytes.to_string raw))

let ownership () =
  let body = Bytes.make 8 '\000' and name = Bytes.of_string "Export" in
  let record =
    Bin.Export
      {
        kind = Entry.Imm32_export;
        value = 42L;
        name = Bytes.unsafe_to_string name;
      }
  in
  let module_ =
    get
      (create
         ~body:(Bytes.unsafe_to_string body)
         [ record; record; Bin.Main 0; Bin.Main 0 ])
  in
  let expected = Bin.bytes module_ in
  Bytes.set body 0 'X';
  Bytes.set name 0 'X';
  Alcotest.(check string)
    "writer owns body and names" expected (Bin.bytes module_);
  Bytes.set (Bytes.unsafe_of_string (Bin.bytes module_)) 0 'X';
  let read_input = Bytes.of_string expected in
  let inspected = get (Bin.read (Bytes.unsafe_to_string read_input)) in
  Bytes.set read_input 0 'X';
  (match Bin.records inspected with
  | Bin.Export r :: _ -> Bytes.set (Bytes.unsafe_of_string r.name) 0 'X'
  | _ -> Alcotest.fail "missing export");
  Alcotest.(check string)
    "reader and projections own bytes/names" expected (Bin.bytes inspected);
  Alcotest.(check string)
    "repeat records remain present" "Export"
    (Bin.record_name (List.hd (Bin.records inspected)));
  Alcotest.(check int)
    "duplicate MAIN and exports" 4
    (List.length (Bin.records inspected))

let json_input () =
  let parse text = Bin.records_of_yojson (Yojson.Safe.from_string text) in
  ignore
    (get
       (parse
          "[{\"type\":\"IET_IMM64_EXPORT\",\"name\":\"Exact\",\"value\":\"4294967295\"}]"));
  List.iter
    (fun text -> check_error "malformed metadata" (parse text))
    [
      "{}";
      "[0]";
      "[{\"type\":\"IET_END\"}]";
      "[{\"type\":\"AAT_ADD_U32\"}]";
      "[{\"type\":\"IET_MAIN\",\"offset\":0,\"extra\":0}]";
      "[{\"type\":\"IET_MAIN\",\"offset\":0,\"offset\":0}]";
      "[{\"type\":\"IET_MAIN\",\"offset\":4294967296}]";
      "[{\"type\":\"IET_MAIN\",\"offset\":0.5}]";
    ]

let assembly_adapter () =
  let encode ?(position = 0L) source =
    let session = Session.create () in
    let source =
      Session.add_source session ~path:"module.asm" ~contents:source
    in
    let config =
      match
        Preprocessor.Config.create ~compilation_mode:Preprocessor.Aot ()
      with
      | Ok value -> value
      | Error message -> Alcotest.fail message
    in
    let parsed = parse_assembly_detailed session ~config ~source in
    let assembly =
      match parsed.assembly with
      | Some value -> value
      | None -> Alcotest.fail "fixture parse"
    in
    match
      Assembly_encoding.encode ~position ~config ~max_code_bytes:256 assembly
    with
    | Ok value -> value
    | Error d -> Alcotest.fail d.message
  in
  let image =
    encode "entry:: RET; DU8 entry; DU16 entry; DU32 entry; DU64 entry;"
  in
  let module_ =
    match Assembly_bin.emit ~main:[ "entry"; "entry" ] image with
    | Ok value -> value
    | Error d -> Alcotest.fail d.message
  in
  Alcotest.(check string)
    "AAT storage widths become original U32 offset table"
    "14040000000008000000040000000200000001000000"
    (hex (String.sub (Bin.bytes module_) (Bin.patch_table_offset module_) 22));
  Alcotest.(check int)
    "export and repeated main entries" 4
    (List.length (Bin.records module_));
  List.iter
    (fun image ->
      match Assembly_bin.emit image with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "invalid assembly adapter succeeded")
    [ encode "entry: RET; DU8 entry;"; encode ~position:16L "RET;" ];
  let image = encode "IMPORT A,B; DU64 A; DU32 B; DU16 A;" in
  let module_ =
    match Assembly_bin.emit image with
    | Ok value -> value
    | Error d -> Alcotest.fail d.message
  in
  Alcotest.(check (list string))
    "stable symbol groups retain declared widths" [ "A"; ""; "B" ]
    (List.map Bin.record_name (Bin.records module_));
  Alcotest.(check (list int))
    "grouped original IET identities" [ 11; 7; 9 ]
    (List.map
       (fun record -> Entry.to_code (Bin.record_entry record))
       (Bin.records module_));
  match Assembly_bin.emit ~main:[ "missing" ] image with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "missing MAIN label emitted"

let tests =
  [
    Alcotest.test_case "literal header and empty body" `Quick literal_header;
    Alcotest.test_case "every original record shape" `Quick record_shapes;
    Alcotest.test_case "exact and one-below budgets" `Quick budgets;
    Alcotest.test_case "invalid fields and group boundaries" `Quick
      invalid_records;
    Alcotest.test_case "malformed and truncated input" `Quick malformed;
    Alcotest.test_case "owned bytes, names and repeated entries" `Quick
      ownership;
    Alcotest.test_case "explicit metadata is exact and typed" `Quick json_input;
    Alcotest.test_case "source assembler and original AAT policy" `Quick
      assembly_adapter;
  ]
