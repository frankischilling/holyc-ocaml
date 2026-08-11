module Bin = Holyc_lib.Templeos_bin_spec

let header () =
  Alcotest.(check string) "signature spelling" "'TOSB'" Bin.signature_spelling;
  Alcotest.(check int32) "signature value" 0x42534f54l Bin.signature_value;
  Alcotest.(check int) "header size" 32 Bin.header_size;
  Alcotest.(check (list string))
    "field names"
    [
      "jmp";
      "module_align_bits";
      "reserved";
      "bin_signature";
      "org";
      "patch_table_offset";
      "file_size";
    ]
    (List.map (fun (field : Bin.header_field) -> field.name) Bin.header_fields);
  Alcotest.(check (list int))
    "field offsets" [ 0; 2; 3; 4; 8; 16; 24 ]
    (List.map
       (fun (field : Bin.header_field) -> field.offset)
       Bin.header_fields)

let entry_registry () =
  Alcotest.(check (list int))
    "numeric domain"
    [
      0; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25;
    ]
    (List.map Bin.Entry.to_code Bin.Entry.all);
  List.iter
    (fun entry ->
      Alcotest.(check bool)
        "code round trip" true
        (Bin.Entry.of_code (Bin.Entry.to_code entry) = Some entry);
      Alcotest.(check bool)
        "name round trip" true
        (Bin.Entry.of_source_name (Bin.Entry.to_source_name entry) = Some entry))
    Bin.Entry.all;
  Alcotest.(check bool)
    "known decode" true
    (Bin.Entry.decode 25 = Bin.Entry.Entry Bin.Entry.Main);
  Alcotest.(check bool)
    "reserved decode" true
    (Bin.Entry.decode 12 = Bin.Entry.Reserved 12);
  Alcotest.(check bool)
    "unknown decode" true
    (Bin.Entry.decode 26 = Bin.Entry.Unknown 26)

let source_statuses () =
  let status entry = (Bin.Entry.info entry).status in
  Alcotest.(check bool)
    "zero-width import" true
    (status Bin.Entry.Rel_i0 = Bin.Source_fictitious);
  Alcotest.(check bool)
    "64-bit export" true
    (status Bin.Entry.Imm64_export = Bin.Source_not_implemented);
  Alcotest.(check bool)
    "zeroed heap" true
    (status Bin.Entry.Zeroed_data_heap = Bin.Source_not_really_used);
  Alcotest.(check bool) "main" true (status Bin.Entry.Main = Bin.Source_active)

let relocation_metadata () =
  let relocation entry =
    match (Bin.Entry.info entry).relocation with
    | Some relocation -> relocation
    | None -> Alcotest.fail "import entry has no relocation metadata"
  in
  List.iter
    (fun (entry, width, bias) ->
      let relocation = relocation entry in
      Alcotest.(check int) "width" width (Bin.width_bytes relocation.width);
      Alcotest.(check int) "bias" bias relocation.displacement_bias;
      Alcotest.(check bool) "relative" true (relocation.kind = Bin.Relative))
    [
      (Bin.Entry.Rel_i0, 0, 0);
      (Bin.Entry.Rel_i8, 1, 1);
      (Bin.Entry.Rel_i16, 2, 2);
      (Bin.Entry.Rel_i32, 4, 4);
      (Bin.Entry.Rel_i64, 8, 8);
    ];
  List.iter
    (fun entry ->
      let relocation = relocation entry in
      Alcotest.(check bool) "immediate" true (relocation.kind = Bin.Immediate);
      Alcotest.(check int) "immediate bias" 0 relocation.displacement_bias)
    [
      Bin.Entry.Imm_u0;
      Bin.Entry.Imm_u8;
      Bin.Entry.Imm_u16;
      Bin.Entry.Imm_u32;
      Bin.Entry.Imm_i64;
    ]

let record_shapes () =
  let info = Bin.Entry.info in
  let import = info Bin.Entry.Rel_i32 in
  Alcotest.(check bool)
    "import name group" true
    (import.name_mode = Bin.First_name_then_inherited);
  Alcotest.(check bool)
    "import leading offset" true
    (import.leading_value = Bin.Patch_offset);
  let export = info Bin.Entry.Rel32_export in
  Alcotest.(check bool) "export name" true (export.name_mode = Bin.Required_name);
  Alcotest.(check bool)
    "relative export" true
    (export.pass1 = Bin.Register_relative_export);
  let absolute = info Bin.Entry.Abs_addr in
  Alcotest.(check bool)
    "absolute offsets" true
    (absolute.payload = Bin.U32_offsets);
  Alcotest.(check bool)
    "absolute pass two" true
    (absolute.pass2 = Bin.Skip_u32_offsets);
  let code = info Bin.Entry.Code_heap in
  Alcotest.(check bool)
    "code size" true
    (code.payload = Bin.I32_size_then_u32_offsets);
  let data = info Bin.Entry.Data_heap in
  Alcotest.(check bool)
    "data size" true
    (data.payload = Bin.I64_size_then_u32_offsets);
  let main = info Bin.Entry.Main in
  Alcotest.(check bool) "main pass two" true (main.pass2 = Bin.Execute_main);
  let ending = info Bin.Entry.End in
  Alcotest.(check bool)
    "terminator has no header tail" true
    (ending.name_mode = Bin.No_name_field)

let adjustments () =
  Alcotest.(check (list int))
    "adjustment codes" [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    (List.map Bin.Adjustment.to_code Bin.Adjustment.all);
  Alcotest.(check (list int))
    "adjustment widths" [ 1; 1; 2; 2; 4; 4; 8; 8 ]
    (List.map
       (fun adjustment ->
         Bin.width_bytes (Bin.Adjustment.info adjustment).width)
       Bin.Adjustment.all);
  Alcotest.(check (list bool))
    "adjustment operation"
    [ true; false; true; false; true; false; true; false ]
    (List.map
       (fun adjustment -> (Bin.Adjustment.info adjustment).operation = Bin.Add)
       Bin.Adjustment.all);
  List.iter
    (fun adjustment ->
      Alcotest.(check bool)
        "adjustment round trip" true
        (Bin.Adjustment.of_code (Bin.Adjustment.to_code adjustment)
        = Some adjustment))
    Bin.Adjustment.all

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Bin.reference_commit;
  Alcotest.(check int) "source count" 13 (List.length Bin.sources);
  Alcotest.(check int) "signature line" 383 Bin.signature_definition_line;
  Alcotest.(check int) "immediate mask" 1 Bin.immediate_not_relative_mask;
  let source_path reference = reference.Bin.path in
  Alcotest.(check string)
    "import source" "Kernel/KLoad.HC"
    (source_path Bin.behavior_sources.import_patches);
  Alcotest.(check int) "import line" 41 Bin.behavior_sources.import_patches.line;
  Alcotest.(check string)
    "header writer" "Compiler/CMain.HC"
    (source_path Bin.behavior_sources.header_write);
  Alcotest.(check int) "header line" 540 Bin.behavior_sources.header_write.line

let consumer_metadata () =
  let has path line references =
    List.exists
      (fun (reference : Bin.source_reference) ->
        String.equal reference.path path && reference.line = line)
      references
  in
  Alcotest.(check bool)
    "assembler consumer" true
    (has "Compiler/AsmResolve.HC" 34
       (Bin.Entry.info Bin.Entry.Rel_i32).consumers);
  Alcotest.(check bool)
    "loader consumer" true
    (has "Kernel/KLoad.HC" 163 (Bin.Entry.info Bin.Entry.Main).consumers);
  Alcotest.(check bool)
    "adjustment consumer" true
    (has "Compiler/CMain.HC" 188
       (Bin.Adjustment.info Bin.Adjustment.Add_u8).consumers)

let tests =
  [
    Alcotest.test_case "header" `Quick header;
    Alcotest.test_case "entry registry" `Quick entry_registry;
    Alcotest.test_case "source statuses" `Quick source_statuses;
    Alcotest.test_case "relocations" `Quick relocation_metadata;
    Alcotest.test_case "record shapes" `Quick record_shapes;
    Alcotest.test_case "adjustments" `Quick adjustments;
    Alcotest.test_case "provenance" `Quick provenance;
    Alcotest.test_case "consumer metadata" `Quick consumer_metadata;
  ]
