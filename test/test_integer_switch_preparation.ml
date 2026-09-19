open Holyc_lib
module Preparation = Holyc_lib__Sema.Integer_switch_preparation

let require_ok show = function
  | Ok value -> value
  | Error e -> Alcotest.fail (show e)

let diagnostics errors =
  String.concat "; "
    (List.map (fun (e : Diagnostic.t) -> e.code ^ ": " ^ e.message) errors)

let parse ?(max_work = 100000) ?budget ~mode contents =
  let budget =
    match budget with
    | Some b -> b
    | None -> Preparation.create_budget ~max_work |> require_ok Fun.id
  in
  let tracker = Preparation.create_tracker ~budget in
  let events = ref [] in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"switch-preparation.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (fun _ -> Ok ());
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      declaration =
        Some
          (fun event ->
            events := event :: !events;
            Preparation.observe tracker event);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  (output, tracker, budget, List.rev !events)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let expect_error code output =
  Alcotest.(check bool)
    ("contains " ^ code) true
    (List.exists
       (fun (e : Diagnostic.t) -> e.code = code)
       output.Parser.diagnostics)

let values_and_ownership () =
  List.iter
    (fun mode ->
      let output, tracker, budget, events =
        parse ~mode "U0 F(){switch(0){case 5...3:;case:;case -2:;default:;}}"
      in
      if Parser.has_errors output then
        Alcotest.fail (diagnostics output.diagnostics);
      let prepared = List.hd (Preparation.preparations tracker) in
      Alcotest.(check int64)
        "lower bound" (-2L)
        (Preparation.lower_bound prepared);
      Alcotest.(check int) "range" 9 (Preparation.range prepared);
      Alcotest.(check (list (pair int64 int64)))
        "normalized source cases"
        [ (3L, 5L); (6L, 6L); (-2L, -2L) ]
        (List.map
           (fun c ->
             (Preparation.case_lower_bound c, Preparation.case_upper_bound c))
           (Preparation.cases prepared));
      Alcotest.(check bool)
        "exact AST lookup" true
        (Preparation.find tracker (Preparation.source prepared) = Some prepared);
      let work = Preparation.budget_work budget in
      let endpoint =
        List.find
          (function
            | Parser.Switch_case_preparing _ -> true
            | _ -> false)
          events
      in
      let foreign = Preparation.create_tracker ~budget in
      (match Preparation.observe foreign endpoint with
      | Error errors ->
          Alcotest.(check string)
            "expired original callback rejects" "HCRUN0004"
            (List.hd errors).code
      | Ok () -> Alcotest.fail "replayed callback admitted");
      Alcotest.(check int)
        "replay spends no work" work
        (Preparation.budget_work budget);
      let other, other_tracker, _, _ =
        parse ~mode "U0 F(){switch(0){case 5...3:;case:;case -2:;default:;}}"
      in
      if Parser.has_errors other then
        Alcotest.fail (diagnostics other.diagnostics);
      let other_prepared = List.hd (Preparation.preparations other_tracker) in
      Alcotest.(check bool)
        "equal source is foreign" true
        (Option.is_none
           (Preparation.find tracker (Preparation.source other_prepared))))
    modes

let bounds_and_timing () =
  List.iter
    (fun mode ->
      let source = "U0 F(){switch(0){case 2+3:;}}" in
      let output, _, budget, _ = parse ~max_work:3 ~mode source in
      if Parser.has_errors output then
        Alcotest.fail (diagnostics output.diagnostics);
      Alcotest.(check int) "exact work" 3 (Preparation.budget_work budget);
      let output, _, budget, _ = parse ~max_work:2 ~mode source in
      expect_error "HCSW0003" output;
      Alcotest.(check int)
        "one below retains reached work" 2
        (Preparation.budget_work budget);
      let output, _, budget, _ = parse ~mode "U0 F(){switch(0){case 2+3;}}" in
      Alcotest.(check bool)
        "invalid delimiter fails" true (Parser.has_errors output);
      Alcotest.(check int)
        "evaluation precedes delimiter failure" 3
        (Preparation.budget_work budget);
      let output, tracker, budget, _ =
        parse ~mode "U0 F(){switch(0){case 17...65551:;}}"
      in
      if Parser.has_errors output then
        Alcotest.fail (diagnostics output.diagnostics);
      Alcotest.(check int)
        "largest source range" 65535
        (Preparation.range (List.hd (Preparation.preparations tracker)));
      Alcotest.(check int)
        "whole table cap" 65536
        (Preparation.budget_table_entries budget);
      let output, _, _, _ = parse ~budget ~mode "U0 G(){switch(0){case 0:;}}" in
      expect_error "HCSW0004" output;
      List.iter
        (fun (source, code) ->
          let output, _, _, _ = parse ~mode source in
          expect_error code output)
        [
          ("U0 F(){switch(0){case 17...65552:;}}", "HCSW0001");
          ("U0 F(){switch(0){case 1...3:;case 2:;}}", "HCSW0002");
          ("U0 F(){switch(0){default:;}}", "HCSW0001");
          ("U0 F(){switch[0]{case 0:;}}", "HCRUN0001");
          ("U0 F(){switch(0){case 0:;default:;default:;}}", "HCRUN0001");
        ])
    modes

let tests =
  [
    Alcotest.test_case "original values and receipt ownership" `Quick
      values_and_ownership;
    Alcotest.test_case "exact work, delimiter timing and table bounds" `Quick
      bounds_and_timing;
  ]
