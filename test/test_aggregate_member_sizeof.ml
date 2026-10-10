open Holyc_lib
module Cases = Aggregate_member_sizeof_cases
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let run ?max_steps ?max_initializer_steps mode source =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"aggregate-member-sizeof.hc"
      ~contents:(Cases.headers ^ source)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ?max_initializer_steps
    ~max_steps:(Option.value ~default:100_000 max_steps)
    session ~source ~config

let describe errors =
  String.concat "; "
    (List.map
       (fun (diagnostic : Diagnostic.t) ->
         diagnostic.code ^ ": " ^ diagnostic.message)
       errors)

let contains text fragment =
  let text = String.lowercase_ascii text
  and fragment = String.lowercase_ascii fragment in
  let length = String.length fragment in
  let rec loop at =
    at + length <= String.length text
    && (String.sub text at length = fragment || loop (at + 1))
  in
  loop 0

let expect (case : Cases.case) report =
  let result =
    match integer_program_report_outcome report with
    | Ok result -> result
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check string)
    (case.label ^ " output occurs exactly once")
    case.output
    (integer_program_report_output_bytes report);
  Alcotest.(check bool)
    "successful source reaches stream end" true
    (VM.termination result.value = VM.Stream_end);
  Alcotest.(check (option int64))
    (case.label ^ " selected TempleOS member byte count")
    (Some case.expected)
    (VM.final_value result.value
    |> Option.map (fun (word : VM.word) -> word.bits));
  Alcotest.(check bool)
    "final result is internal I64" true
    (match VM.final_value result.value with
    | Some word -> word.type_ = VM.I64
    | None -> false)

let value mode (case : Cases.case) () = expect case (run mode case.source)

let fail_code code report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists
           (fun (diagnostic : Diagnostic.t) -> diagnostic.code = code)
           errors)

let reject ?(expected_output = "") mode (case : Cases.rejection) () =
  let report = run mode case.source in
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail (case.label ^ " unexpectedly compiled")
  | Error errors ->
      Alcotest.(check bool) "rejection has a diagnostic" true (errors <> []);
      Option.iter
        (fun code ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors))
        case.code;
      Option.iter
        (fun fragment ->
          Alcotest.(check bool)
            (describe errors) true
            (contains (describe errors) fragment))
        case.message_fragment;
      Alcotest.(check string)
        "rejection retains exactly the source-reached output" expected_output
        (integer_program_report_output_bytes report)

let unsupported_storage mode (case : Cases.unsupported_storage_case) () =
  reject mode (Cases.reject ~code:case.ir_code case.label case.source) ()

let runtime_layout_effects_and_exact_limits () =
  let case = Cases.runtime_layout in
  let baseline = run Preprocessor.Jit case.source in
  expect case baseline;
  let preparation =
    integer_program_report_preparation_work baseline |> Option.get
  in
  let steps =
    match integer_program_report_outcome baseline with
    | Error _ -> Alcotest.fail "successful report lost its result"
    | Ok result -> VM.executed_steps result.value
  in
  Alcotest.(check bool)
    "layout preparation consumes real work" true (preparation > 0);
  Alcotest.(check bool) "native program source executes" true (steps > 0);
  expect case
    (run ~max_steps:steps ~max_initializer_steps:preparation Preprocessor.Jit
       case.source);
  fail_code "HCIRVM0007"
    (run ~max_initializer_steps:(preparation - 1) Preprocessor.Jit case.source);
  fail_code "HCIRVM0007"
    (run ~max_steps:(steps - 1) Preprocessor.Jit case.source)

let expanded_member_provenance () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"aggregate-member-provenance.hc"
      ~contents:Cases.provenance_source
  in
  let events = ref [] and root_was_current = ref false in
  let query event =
    (match event with
    | Parser.Query_root root ->
        root_was_current := Parser.query_root_is_current root
    | _ -> ());
    events := event :: !events;
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference = None;
      call = None;
      implicit_output = None;
      declaration = None;
      query = Some query;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config source
  in
  if Parser.has_errors parsed then Alcotest.fail (describe parsed.diagnostics);
  let module_ =
    match parsed.ast with
    | Some module_ -> module_
    | None -> Alcotest.fail "expanded query produced no syntax tree"
  in
  let events = List.rev !events in
  let root, starts, members, completed =
    match events with
    | [
     Parser.Query_root root;
     Parser.Query_member_started first_start;
     Parser.Query_member first;
     Parser.Query_member_started second_start;
     Parser.Query_member second;
     Parser.Query_completed completed;
    ] -> (root, [ first_start; second_start ], [ first; second ], completed)
    | _ -> Alcotest.fail "parser substituted or reordered live member reads"
  in
  Alcotest.(check bool)
    "root was live at its actual lexical callback" true !root_was_current;
  Alcotest.(check bool)
    "root expires after the parser returns" false
    (Parser.query_root_is_current root);
  Alcotest.(check bool)
    "all members retain the original root" true
    (List.for_all
       (fun (member : Parser.query_member) -> member.query_member_root == root)
       members);
  Alcotest.(check bool)
    "completed query retains the exact root" true
    (completed.query_root == root);
  match (module_.items, completed.query_expression) with
  | [ Ast.Function_prototype prototype ], Ast.Sizeof_expression expression ->
      (match prototype.binding.target with
      | Ast.Expression_binding_target target ->
          Alcotest.(check bool)
            "completed expression is the original AST" true
            (target == completed.query_expression)
      | _ -> Alcotest.fail "source query lost its binding expression");
      (match root.query_node with
      | Parser.Sizeof_target target ->
          Alcotest.(check bool)
            "root is the original identifier" true
            (target == expression.sizeof_target)
      | _ -> Alcotest.fail "query root changed kind");
      Alcotest.(check (list string))
        "expanded path spells the source-selected members" [ "inner"; "bytes" ]
        (List.map
           (fun (member : Ast.sizeof_member) ->
             member.sizeof_member_name.spelling)
           expression.sizeof_members);
      Alcotest.(check (list int))
        "five original pointer suffixes" [ 1; 2; 3; 4; 5 ]
        (List.map
           (fun (layer : Ast.pointer_layer) -> layer.depth)
           expression.sizeof_pointer_layers);
      List.iter2
        (fun start (member : Parser.query_member) ->
          Alcotest.(check bool)
            "member receipt holds its exact dot" true
            (member.query_member_start == start);
          match member.query_member_node with
          | Parser.Sizeof_member original ->
              Alcotest.(check bool)
                "member is original AST child" true
                (List.exists (( == ) original) expression.sizeof_members);
              Alcotest.(check bool)
                "dot is original source child" true
                (start.member_start_dot == original.sizeof_member_dot)
          | _ -> Alcotest.fail "sizeof member changed query kind")
        starts members;
      List.iter
        (fun (location : Ast.location) ->
          Alcotest.(check bool)
            "macro expansion retains invocation" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "macro expansion retains definition" true
            (Option.is_some location.defined_at))
        (expression.sizeof_target.location
        :: List.concat_map
             (fun (member : Ast.sizeof_member) ->
               [ member.sizeof_member_dot; member.sizeof_member_name.location ])
             expression.sizeof_members)
  | _ -> Alcotest.fail "completed query does not own the source prototype"

let () =
  Alcotest.run "Original aggregate member sizeof"
    [
      ( "IR",
        List.concat_map
          (fun mode ->
            let prefix = if mode = Preprocessor.Jit then "JIT " else "AOT " in
            List.map
              (fun (case : Cases.case) ->
                Alcotest.test_case (prefix ^ case.label) `Quick
                  (value mode case))
              Cases.closed
            @ List.map
                (fun (case : Cases.rejection) ->
                  Alcotest.test_case
                    (prefix ^ "reject " ^ case.label)
                    `Quick (reject mode case))
                Cases.rejected
            @ List.map
                (fun (case : Cases.unsupported_storage_case) ->
                  Alcotest.test_case
                    (prefix ^ "storage boundary " ^ case.label)
                    `Quick
                    (unsupported_storage mode case))
                Cases.unsupported_storage)
          modes
        @ List.map
            (fun (case : Cases.case) ->
              Alcotest.test_case ("JIT " ^ case.label) `Quick
                (value Preprocessor.Jit case))
            Cases.jit_only
        @ List.map
            (fun ((case : Cases.rejection), expected_output) ->
              Alcotest.test_case
                ("JIT read timing " ^ case.label)
                `Quick
                (reject ~expected_output Preprocessor.Jit case))
            Cases.jit_rejected_effects
        @ [
            Alcotest.test_case "JIT member layout effects and exact budgets"
              `Quick runtime_layout_effects_and_exact_limits;
            Alcotest.test_case "expanded member read provenance" `Quick
              expanded_member_provenance;
          ] );
    ]
