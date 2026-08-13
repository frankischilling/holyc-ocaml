open Holyc_lib
module Flag = Global_record_flag

let global_type () =
  Alcotest.(check string)
    "index name" "HTt_GLBL_VAR" Flag.global_type.index_name;
  Alcotest.(check string) "mask name" "HTT_GLBL_VAR" Flag.global_type.mask_name;
  Alcotest.(check int) "type index" 3 Flag.global_type.type_index;
  Alcotest.(check int64) "type mask" 0x8L Flag.global_type.type_mask;
  Alcotest.(check int) "index line" 659 Flag.global_type.index_definition_line;
  Alcotest.(check int) "mask line" 689 Flag.global_type.mask_definition_line

let hash_registry () =
  Alcotest.(check (list string))
    "source names"
    [
      "HTF_PRIVATE";
      "HTF_PUBLIC";
      "HTF_EXPORT";
      "HTF_IMPORT";
      "HTF_IMM";
      "HTF_GOTO_LABEL";
      "HTF_RESOLVE";
      "HTF_UNRESOLVED";
      "HTF_LOCAL";
    ]
    (List.map Flag.Hash_flag.to_source_name Flag.Hash_flag.all);
  Alcotest.(check (list int))
    "bit indexes"
    [ 23; 24; 25; 26; 27; 28; 29; 30; 31 ]
    (List.map Flag.Hash_flag.to_bit_index Flag.Hash_flag.all)

let global_registry () =
  Alcotest.(check (list string))
    "source names"
    [
      "GVF_FUN";
      "GVF_IMPORT";
      "GVF_EXTERN";
      "GVF_DATA_HEAP";
      "GVF_ALIAS";
      "GVF_ARRAY";
    ]
    (List.map Flag.Global_flag.to_source_name Flag.Global_flag.all);
  Alcotest.(check (list int64))
    "masks"
    [ 1L; 2L; 4L; 8L; 16L; 32L ]
    (List.map Flag.Global_flag.to_mask Flag.Global_flag.all)

let mask_operations () =
  let open Flag.Hash_flag in
  let mask = set ~mask:0L Public |> fun mask -> set ~mask Export in
  Alcotest.(check int64) "combined hash mask" 0x03000000L mask;
  Alcotest.(check bool) "public set" true (is_set ~mask Public);
  Alcotest.(check bool) "import clear" false (is_set ~mask Import);
  Alcotest.(check int64) "clear export" 0x01000000L (clear ~mask Export);
  let global =
    Flag.Global_flag.set ~mask:0L Flag.Global_flag.Array |> fun mask ->
    Flag.Global_flag.set ~mask Flag.Global_flag.Data_heap
  in
  Alcotest.(check int64) "combined global mask" 0x28L global

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Flag.reference_commit;
  Alcotest.(check int) "source count" 7 (List.length Flag.sources);
  Alcotest.(check int) "behavior count" 25 (List.length Flag.behaviors);
  let behavior = Flag.behavior "extern-value-slot" |> Option.get in
  Alcotest.(check (pair string int))
    "extern value source" ("Kernel/KHashB.HC", 25)
    (behavior.source.path, behavior.source.line);
  let alias = Flag.Global_flag.info Flag.Global_flag.Alias in
  Alcotest.(check (list (pair string int)))
    "alias consumers"
    [
      ("Compiler/PrsStmt.HC", 310);
      ("Compiler/PrsStmt.HC", 443);
      ("Kernel/KHashB.HC", 78);
    ]
    (List.map
       (fun (reference : Flag.source_reference) ->
         (reference.path, reference.line))
       alias.consumers)

let tests =
  [
    Alcotest.test_case "global hash type" `Quick global_type;
    Alcotest.test_case "hash registry" `Quick hash_registry;
    Alcotest.test_case "global registry" `Quick global_registry;
    Alcotest.test_case "mask operations" `Quick mask_operations;
    Alcotest.test_case "provenance" `Quick provenance;
  ]
