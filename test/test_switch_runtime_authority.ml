open Holyc_lib
module Preparation = Holyc_lib__Sema.Integer_switch_preparation

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let expect_diagnostic label code = function
  | Error (errors : Diagnostic.t list) ->
      Alcotest.(check string) label code (List.hd errors).code
  | Ok _ -> Alcotest.fail (label ^ ": expected a diagnostic")

let expect_error label message = function
  | Error actual -> Alcotest.(check string) label message actual
  | Ok () -> Alcotest.fail (label ^ ": operation unexpectedly succeeded")

let parse ~mode ~max_work ~preparing contents =
  let budget = Preparation.create_budget ~max_work |> require_ok Fun.id in
  let tracker = Preparation.create_tracker ~budget in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"switch-runtime-authority.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = Some (fun _ -> Ok ());
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      declaration =
        Some
          (function
          | Parser.Switch_case_preparing original ->
              preparing budget tracker original
          | event -> Preparation.observe tracker event);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  (output, tracker, budget)

let exact_original_endpoint_and_work () =
  List.iter
    (fun mode ->
      let endpoints = ref [] in
      let preparing budget tracker (original : Parser.switch_case_preparation) =
        Alcotest.(check bool)
          "preparation callback is currently executing" true
          (Parser.switch_case_preparation_is_current original);
        let endpoint =
          Preparation.begin_runtime_endpoint tracker original
          |> require_ok diagnostics
        in
        endpoints := endpoint :: !endpoints;
        Alcotest.(check bool)
          "runtime endpoint retains the actual parser receipt" true
          (Preparation.runtime_endpoint_receipt endpoint == original);
        expect_diagnostic "active receipt cannot begin twice" "HCRUN0004"
          (Preparation.begin_runtime_endpoint tracker original);
        let foreign = Preparation.create_tracker ~budget in
        expect_error "other tracker cannot complete the endpoint"
          "switch completion requires its original active endpoint"
          (Preparation.complete_runtime_endpoint foreign endpoint ~bits:99L);
        expect_error "other tracker cannot fail the endpoint"
          "switch failure requires its original active endpoint"
          (Preparation.fail_runtime_endpoint foreign endpoint);
        Alcotest.(check bool)
          "wrong-tracker operations retain the active endpoint" true
          (Preparation.runtime_remaining endpoint > 0);
        let bits =
          match original.switch_case_endpoint with
          | Parser.Switch_case_start ->
              Alcotest.(check int)
                "first endpoint sees all available work" 3
                (Preparation.runtime_remaining endpoint);
              Preparation.charge_runtime_work endpoint ~work:0
              |> require_ok Fun.id;
              expect_error "negative work does not charge"
                "switch work requires its original active endpoint"
                (Preparation.charge_runtime_work endpoint ~work:(-1));
              Preparation.charge_runtime_work endpoint ~work:2
              |> require_ok Fun.id;
              Alcotest.(check int)
                "start retains one remaining work unit" 1
                (Preparation.runtime_remaining endpoint);
              1L
          | Parser.Switch_case_end ->
              Alcotest.(check int)
                "end shares the charged switch budget" 1
                (Preparation.runtime_remaining endpoint);
              expect_error "overquota charge rejects without spending"
                "HCSW0003: switch case preparation exhausted max_switch_work"
                (Preparation.charge_runtime_work endpoint ~work:2);
              Alcotest.(check int)
                "overquota retains previous work" 2
                (Preparation.budget_work budget);
              Alcotest.(check int)
                "overquota retains remaining allowance" 1
                (Preparation.runtime_remaining endpoint);
              Preparation.charge_runtime_work endpoint ~work:1
              |> require_ok Fun.id;
              expect_error "exhausted budget rejects an additional unit"
                "HCSW0003: switch case preparation exhausted max_switch_work"
                (Preparation.charge_runtime_work endpoint ~work:1);
              3L
        in
        Preparation.complete_runtime_endpoint tracker endpoint ~bits
        |> require_ok Fun.id;
        Alcotest.(check int)
          "completed endpoint has no remaining authority" 0
          (Preparation.runtime_remaining endpoint);
        expect_error "completion can happen only once"
          "switch completion requires its original active endpoint"
          (Preparation.complete_runtime_endpoint tracker endpoint ~bits);
        expect_error "completion cannot be changed into failure"
          "switch failure requires its original active endpoint"
          (Preparation.fail_runtime_endpoint tracker endpoint);
        expect_error "completed receipt cannot charge more work"
          "switch work requires its original active endpoint"
          (Preparation.charge_runtime_work endpoint ~work:0);
        expect_diagnostic "completed endpoint cannot begin again" "HCRUN0004"
          (Preparation.begin_runtime_endpoint tracker original);
        Ok ()
      in
      let output, tracker, budget =
        parse ~mode ~max_work:3 ~preparing "U0 F(){switch(0){case 1...3:;}}"
      in
      if Parser.has_errors output then
        Alcotest.fail (diagnostics output.diagnostics);
      Alcotest.(check int)
        "both original range endpoints were seen" 2 (List.length !endpoints);
      Alcotest.(check int) "exact total work" 3 (Preparation.budget_work budget);
      let prepared = List.hd (Preparation.preparations tracker) in
      Alcotest.(check int)
        "exact per-owner runtime work" 3
        (Preparation.work prepared);
      Alcotest.(check (list (pair int64 int64)))
        "completed case retains evaluated runtime endpoints"
        [ (1L, 3L) ]
        (List.map
           (fun case ->
             ( Preparation.case_lower_bound case,
               Preparation.case_upper_bound case ))
           (Preparation.cases prepared));
      List.iter
        (fun endpoint ->
          let original = Preparation.runtime_endpoint_receipt endpoint in
          Alcotest.(check bool)
            "callback expires after parsing" false
            (Parser.switch_case_preparation_is_current original);
          expect_diagnostic "expired original receipt cannot begin" "HCRUN0004"
            (Preparation.begin_runtime_endpoint tracker original);
          Alcotest.(check int)
            "expired endpoint has no allowance" 0
            (Preparation.runtime_remaining endpoint);
          expect_error "expired endpoint cannot be failed"
            "switch failure requires its original active endpoint"
            (Preparation.fail_runtime_endpoint tracker endpoint))
        !endpoints;
      Alcotest.(check int)
        "attempted callback reuse spent no additional work" 3
        (Preparation.budget_work budget))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let failure_revokes_owner_and_later_endpoints () =
  List.iter
    (fun mode ->
      let failed_start = ref None in
      let later_end_rejected = ref false in
      let preparing budget tracker (original : Parser.switch_case_preparation) =
        match original.switch_case_endpoint with
        | Parser.Switch_case_start ->
            let endpoint =
              Preparation.begin_runtime_endpoint tracker original
              |> require_ok diagnostics
            in
            failed_start := Some endpoint;
            Preparation.charge_runtime_work endpoint ~work:1
            |> require_ok Fun.id;
            let foreign = Preparation.create_tracker ~budget in
            expect_error "foreign tracker cannot revoke owner's work"
              "switch failure requires its original active endpoint"
              (Preparation.fail_runtime_endpoint foreign endpoint);
            Alcotest.(check int)
              "foreign failure does not reset the owner's work" 1
              (Preparation.runtime_remaining endpoint);
            Preparation.fail_runtime_endpoint tracker endpoint
            |> require_ok Fun.id;
            expect_error "failed endpoint cannot fail twice"
              "switch failure requires its original active endpoint"
              (Preparation.fail_runtime_endpoint tracker endpoint);
            expect_error "failed endpoint cannot complete"
              "switch completion requires its original active endpoint"
              (Preparation.complete_runtime_endpoint tracker endpoint ~bits:5L);
            expect_error "failed endpoint cannot charge"
              "switch work requires its original active endpoint"
              (Preparation.charge_runtime_work endpoint ~work:1);
            expect_diagnostic "failed owner cannot reacquire same receipt"
              "HCRUN0004"
              (Preparation.begin_runtime_endpoint tracker original);
            Ok ()
        | Parser.Switch_case_end ->
            Alcotest.(check bool)
              "later range callback is still the original parser callback" true
              (Parser.switch_case_preparation_is_current original);
            expect_diagnostic "revoked owner cannot prepare its next endpoint"
              "HCRUN0004"
              (Preparation.begin_runtime_endpoint tracker original);
            later_end_rejected := true;
            Ok ()
      in
      let output, tracker, budget =
        parse ~mode ~max_work:2 ~preparing
          "U0 F(){switch(0){case 5...8:;case 9:;}}"
      in
      Alcotest.(check bool)
        "failed owner prevents successful parsing" true
        (Parser.has_errors output);
      Alcotest.(check bool)
        "failure rejects a later original endpoint" true !later_end_rejected;
      Alcotest.(check bool)
        "failed case completion diagnoses revoked owner" true
        (List.exists
           (fun (error : Diagnostic.t) -> error.code = "HCRUN0004")
           output.diagnostics);
      Alcotest.(check int)
        "failed owner's spent work remains charged" 1
        (Preparation.budget_work budget);
      Alcotest.(check int)
        "failed owner never publishes a prepared switch" 0
        (List.length (Preparation.preparations tracker));
      match !failed_start with
      | None -> Alcotest.fail "the first range endpoint was never reached"
      | Some endpoint ->
          Alcotest.(check bool)
            "rejected callback is expired after parser failure" false
            (Parser.switch_case_preparation_is_current
               (Preparation.runtime_endpoint_receipt endpoint));
          Alcotest.(check int)
            "failed endpoint cannot borrow unused shared work" 0
            (Preparation.runtime_remaining endpoint);
          expect_error "failed endpoint cannot complete after callback expiry"
            "switch completion requires its original active endpoint"
            (Preparation.complete_runtime_endpoint tracker endpoint ~bits:8L))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "runtime receipt, exact work and replay ownership" `Quick
      exact_original_endpoint_and_work;
    Alcotest.test_case "failed runtime endpoint revokes its switch owner" `Quick
      failure_revokes_owner_and_later_endpoints;
  ]
