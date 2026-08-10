open Holyc_lib

let reference_commit () =
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Version.reference_commit

let rendered_identity () =
  match String.split_on_char '\n' (Version.render ()) with
  | [ compiler; implementation; reference ] ->
      Alcotest.(check bool)
        "compiler command" true
        (String.starts_with ~prefix:"holyc " compiler);
      Alcotest.(check string)
        "implementation field"
        ("implementation " ^ Version.implementation_commit)
        implementation;
      Alcotest.(check string)
        "reference field"
        ("templeos-reference " ^ Version.reference_commit)
        reference
  | _ -> Alcotest.fail "version output must contain exactly three lines"

let tests =
  [
    Alcotest.test_case "reference commit" `Quick reference_commit;
    Alcotest.test_case "rendered identity" `Quick rendered_identity;
  ]
