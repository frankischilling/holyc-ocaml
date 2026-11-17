external run : unit -> unit = "holyc_test_native_fpu_boundary"

let () =
  Alcotest.run "native FPU state boundary"
    [
      ( "actual generated code",
        [ Alcotest.test_case "host and nested state" `Quick run ] );
    ]
