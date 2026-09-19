open Holyc_lib

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let parse ~mode ~declaration contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"switch-callback-lifetime.hc" ~contents
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (fun _ -> Ok ());
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      declaration = Some declaration;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  Parser.parse ~commands ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session) ~config source

let original_receipt_lifetime () =
  List.iter
    (fun mode ->
      let preparations = ref [] in
      let cases = ref [] in
      let switches = ref [] in
      let expired_preparations () =
        List.iter
          (fun original ->
            Alcotest.(check bool)
              "an earlier endpoint cannot borrow a later callback" false
              (Parser.switch_case_preparation_is_current original))
          !preparations
      in
      let expired_cases () =
        List.iter
          (fun original ->
            Alcotest.(check bool)
              "an earlier case cannot borrow a later callback" false
              (Parser.switch_case_completion_is_current original))
          !cases
      in
      let declaration event =
        match event with
        | Parser.Switch_case_preparing preparation ->
            expired_preparations ();
            expired_cases ();
            Alcotest.(check bool)
              "the actual endpoint is current" true
              (Parser.switch_case_preparation_is_current preparation);
            Alcotest.(check bool)
              "the actual endpoint retains its active owner" true
              (Parser.switch_owner_is_current preparation.switch_owner);
            preparations := preparation :: !preparations;
            Ok ()
        | Parser.Switch_case_completed completed ->
            expired_preparations ();
            expired_cases ();
            Alcotest.(check bool)
              "the actual case completion is current" true
              (Parser.switch_case_completion_is_current completed);
            cases := completed :: !cases;
            Ok ()
        | Parser.Switch_completed completed ->
            expired_preparations ();
            expired_cases ();
            List.iter
              (fun original ->
                Alcotest.(check bool)
                  "a completed nested switch is no longer current" false
                  (Parser.switch_completion_is_current original))
              !switches;
            Alcotest.(check bool)
              "the actual switch completion is current" true
              (Parser.switch_completion_is_current completed);
            switches := completed :: !switches;
            Ok ()
        | _ -> Ok ()
      in
      let output =
        parse ~mode ~declaration
          "U0 F(){switch(0){case 1...2:switch(0){case 0:;}case \
           3:;case:;default:;}}"
      in
      if Parser.has_errors output then
        Alcotest.fail (diagnostics_text output.diagnostics);
      Alcotest.(check int)
        "all explicit endpoints observed" 4
        (List.length !preparations);
      Alcotest.(check int)
        "implicit and explicit cases observed" 4 (List.length !cases);
      Alcotest.(check int)
        "nested switch completion observed" 2 (List.length !switches);
      expired_preparations ();
      expired_cases ();
      List.iter
        (fun (completed : Parser.completed_switch) ->
          Alcotest.(check bool)
            "completed receipt expires" false
            (Parser.switch_completion_is_current completed);
          Alcotest.(check bool)
            "completed owner expires" false
            (Parser.switch_owner_is_current completed.switch_owner))
        !switches)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let rejected_callback_expires_before_later_source () =
  List.iter
    (fun mode ->
      let reached = ref None in
      let completed = ref 0 in
      let declaration = function
        | Parser.Switch_case_preparing preparation ->
            reached := Some preparation;
            Error
              [
                Diagnostic.make ~code:"HCTEST0001" ~severity:Diagnostic.Error
                  ~message:"intentional switch observer failure"
                  ~primary:
                    (Ast.expression_location preparation.switch_case_expression)
                      .span ();
              ]
        | Parser.Switch_case_completed _ | Parser.Switch_completed _ ->
            incr completed;
            Ok ()
        | _ -> Ok ()
      in
      let output =
        parse ~mode ~declaration
          "U0 F(){switch(0){case 1:\n#assert 0\n;case 2:;}}"
      in
      Alcotest.(check bool)
        "callback rejection stops parsing" true (Parser.has_errors output);
      let errors =
        List.filter
          (fun (error : Diagnostic.t) -> error.severity = Diagnostic.Error)
          output.diagnostics
      in
      Alcotest.(check (list string))
        "no later assertion or case is reached" [ "HCTEST0001" ]
        (List.map (fun (error : Diagnostic.t) -> error.code) errors);
      Alcotest.(check int)
        "no completion follows a rejected endpoint" 0 !completed;
      match !reached with
      | None -> Alcotest.fail "original endpoint callback was never reached"
      | Some preparation ->
          Alcotest.(check bool)
            "failed endpoint expires" false
            (Parser.switch_case_preparation_is_current preparation);
          Alcotest.(check bool)
            "failed owner expires" false
            (Parser.switch_owner_is_current preparation.switch_owner))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "exact current endpoint, case and switch" `Quick
      original_receipt_lifetime;
    Alcotest.test_case "callback rejection revokes and stops" `Quick
      rejected_callback_expires_before_later_source;
  ]
