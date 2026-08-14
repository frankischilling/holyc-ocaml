open Holyc_lib
module Flag = Member_flag

let registry () =
  Alcotest.(check (list string))
    "source names"
    [
      "MLF_DFT_AVAILABLE";
      "MLF_LASTCLASS";
      "MLF_STR_DFT_AVAILABLE";
      "MLF_FUN";
      "MLF_DOT_DOT_DOT";
      "MLF_NO_UNUSED_WARN";
      "MLF_STATIC";
    ]
    (List.map Flag.to_source_name Flag.all);
  Alcotest.(check (list int))
    "bit indexes" [ 0; 1; 2; 3; 4; 5; 6 ]
    (List.map Flag.to_bit_index Flag.all);
  Alcotest.(check (list int64))
    "masks"
    [ 1L; 2L; 4L; 8L; 16L; 32L; 64L ]
    (List.map Flag.to_mask Flag.all)

let lookups () =
  Alcotest.(check bool)
    "callback lookup" true
    (Flag.of_source_name "MLF_FUN" = Some Flag.Function_pointer);
  Alcotest.(check bool)
    "unknown lookup" true
    (Flag.of_source_name "MLF_UNKNOWN" = None)

let mask_operations () =
  let mask =
    Flag.set ~mask:0L Flag.Function_pointer |> fun mask ->
    Flag.set ~mask Flag.Static
  in
  Alcotest.(check int64) "combined mask" 0x48L mask;
  Alcotest.(check bool)
    "callback set" true
    (Flag.is_set ~mask Flag.Function_pointer);
  Alcotest.(check bool)
    "default clear" false
    (Flag.is_set ~mask Flag.Default_available);
  Alcotest.(check int64)
    "clear callback" 0x40L
    (Flag.clear ~mask Flag.Function_pointer)

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Flag.reference_commit;
  Alcotest.(check int) "source count" 5 (List.length Flag.sources);
  Alcotest.(check int) "behavior count" 23 (List.length Flag.behaviors);
  let callback = Flag.info Flag.Function_pointer in
  Alcotest.(check (pair string int))
    "definition" ("MLF_FUN", 781)
    (callback.source_name, callback.definition_line);
  Alcotest.(check (list (pair string int)))
    "callback consumers"
    [
      ("Compiler/PrsVar.HC", 524);
      ("Compiler/LexLib.HC", 193);
      ("Compiler/LexLib.HC", 232);
      ("Compiler/PrsExp.HC", 767);
      ("Compiler/PrsExp.HC", 1009);
    ]
    (List.map
       (fun (reference : Flag.source_reference) ->
         (reference.path, reference.line))
       callback.consumers)

let behavior_lookup () =
  let behavior = Flag.behavior "callback-member-expression" |> Option.get in
  Alcotest.(check (pair string int))
    "member expression source"
    ("Compiler/PrsExp.HC", 1009)
    (behavior.source.path, behavior.source.line)

let tests =
  [
    Alcotest.test_case "registry" `Quick registry;
    Alcotest.test_case "lookups" `Quick lookups;
    Alcotest.test_case "mask operations" `Quick mask_operations;
    Alcotest.test_case "provenance" `Quick provenance;
    Alcotest.test_case "behavior lookup" `Quick behavior_lookup;
  ]
