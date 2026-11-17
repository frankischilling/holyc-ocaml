external run : unit -> unit = "holyc_test_native_float_comparison"

let () =
  Alcotest.run "native F64 comparison instructions"
    [
      ( "raw-word architectural evidence",
        [ Alcotest.test_case "predicates and x87 balance" `Quick run ] );
    ]
