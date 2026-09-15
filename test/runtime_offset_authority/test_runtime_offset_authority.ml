open Holyc_lib
module VM = Ir_integer_interpreter
module C = Semantic_declaration_collection
module Record = Semantic_compiler_record
module Fragment = Holyc_lib__Sema.Offset_fragment
module Globals = Holyc_lib__Ir.Integer_globals
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Destination = Holyc_lib__Ir.Offset_fragment_destination
module Lowering = Holyc_lib__Driver.Offset_fragment_lowering
module Program = Holyc_lib__Ir.Offset_fragment_program
module Calls = Holyc_lib__Ir.Runtime_call_context

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let with_offset ?max_initializer_steps ?(contents = "class Span {$$=$$+8;};")
    callback =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let owner =
    VM.create_task_state ?max_initializer_steps ~table () |> checked
  in
  let foreign = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner namespace |> checked;
  VM.bind_task_namespace foreign namespace |> checked;
  let reached, finish = callback table namespace owner foreign in
  let progress = ref None in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        progress :=
          Some (Record.begin_aggregate ~table ~namespace publication |> checked)
    | Parser.Aggregate_advanced phase ->
        let current = Option.get !progress in
        (match phase.phase_step with
        | Parser.Aggregate_offset_reached _ -> reached current phase
        | _ -> ());
        Record.advance_aggregate ~dimensions:(fun _ -> None) current phase
        |> checked
    | _ -> ()
  in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event owner event |> checked;
            Ok ());
      call = None;
      implicit_output = None;
      reference = None;
      declaration =
        Some
          (fun event ->
            declaration event;
            Ok ());
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"offset-authority.hc" ~contents
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors parsed);
  finish ()

let fragment table namespace owner progress receipt =
  let view = VM.task_snapshot owner |> checked in
  let fragment =
    Fragment.create ~table ~namespace ~progress ~receipt
      ~environment:(Globals.task_environment view)
      ~references:[] ~queries:[]
    |> checked
  in
  let authority = Fragment.authorize fragment |> checked in
  let attempt = VM.begin_task_offset owner authority |> checked in
  let context =
    Typing.create_context ~table ~parent:(C.namespace_scope namespace)
    |> checked
  in
  let typed = Typing.prepare_offset context fragment |> checked in
  let destination = Destination.create ~task_view:view typed |> checked in
  (fragment, authority, attempt, context, destination)

let execution_lifetime () =
  with_offset (fun table namespace owner foreign ->
      let saved = ref None in
      let reached progress receipt =
        let fragment, authority, attempt, context, destination =
          fragment table namespace owner progress receipt
        in
        reject "another runtime cannot begin the source attempt"
          (VM.begin_task_offset foreign authority);
        let foreign_view = VM.task_snapshot foreign |> checked in
        reject "destination cannot borrow another snapshot"
          (Destination.create ~task_view:foreign_view
             (Destination.typed destination));
        let execution =
          Lowering.prepare ~context ~authority ~runtime:owner destination
          |> diagnostics
        in
        let (Program.Scheduled program) = Program.code execution in
        let module Lower = Holyc_lib__Ir.Integer_program_lowering in
        let unrelated_lowering =
          Lower.lower_complete
            ~globals:(Destination.globals destination)
            ~span:(Destination.span destination)
            [ Lower.Empty (Destination.span destination) ]
          |> diagnostics
        in
        reject "original graph cannot borrow another lowering's source proof"
          (Program.create ~authority ~destination ~lowered:unrelated_lowering
             ~entry:(Program.entry program)
             ~initialization:(Program.initialization program)
             ~runtime_calls:(Program.runtime_calls program));
        let retyped = Typing.prepare_offset context fragment |> checked in
        let unrelated =
          Calls.create ~records:(Typing.records context)
            ~function_sources:(Typing.function_sources context)
            ~top_level:retyped
            ~initialization:(Program.initialization program)
            ~entry:(Program.entry program) ~entry_calls:[] ~functions:[]
          |> diagnostics
        in
        reject "program retains exact typed offset source"
          (Program.create ~authority ~destination
             ~lowered:(Program.lowered program) ~entry:(Program.entry program)
             ~initialization:(Program.initialization program)
             ~runtime_calls:unrelated);
        let wrong_work =
          Program.prepare ~authority ~destination ~code:(Program.code execution)
            ~steps:(Program.steps execution + 1)
          |> checked
        in
        reject "execution rejects substituted preparation count"
          (VM.execute_task_offset owner attempt wrong_work);
        reject "execution cannot borrow a task"
          (VM.execute_task_offset foreign attempt execution);
        Alcotest.(check int)
          "foreign preflight has no runtime effects" 0
          (VM.task_executed_steps foreign);
        VM.execute_task_offset owner attempt execution |> diagnostics;
        reject "execution cannot replay"
          (VM.execute_task_offset owner attempt execution);
        reject "boundary cannot begin again"
          (VM.begin_task_offset owner authority);
        reject "semantic preparation cannot be authorized twice"
          (Fragment.authorize fragment);
        let offset = Option.get (VM.task_offset owner receipt) in
        Alcotest.(check int64)
          "actual offset value" 8L
          (Record.aggregate_offset_value offset);
        reject "runtime metadata cannot authorize isolated execution"
          (VM.charge_isolated_aggregate_offsets foreign ~table [ offset ]);
        saved := Some (fragment, attempt, execution)
      in
      ( reached,
        fun () ->
          let fragment, attempt, execution = Option.get !saved in
          reject "source authority expires" (Fragment.authorize fragment);
          reject "execution expires"
            (VM.execute_task_offset owner attempt execution) ))

let metadata_is_not_execution () =
  with_offset (fun table namespace owner foreign ->
      let reached progress receipt =
        let _, authority, attempt, context, destination =
          fragment table namespace owner progress receipt
        in
        let execution =
          Lowering.prepare ~context ~authority ~runtime:owner destination
          |> diagnostics
        in
        let metadata =
          Record.finish_runtime_aggregate_offset
            (Fragment.preparation authority)
            ~value:8L ~work:(Program.steps execution)
          |> checked
        in
        reject "metadata consumed the preparation without executing it"
          (VM.execute_task_offset owner attempt execution);
        VM.fail_task_offset owner attempt |> checked;
        Alcotest.(check bool)
          "matching metadata is not a runtime result" true
          (VM.task_offset owner receipt = None);
        reject "another runtime cannot charge matching bits as source work"
          (VM.charge_isolated_aggregate_offsets foreign ~table [ metadata ]);
        Alcotest.(check int)
          "no fabricated execution" 0
          (VM.task_executed_steps owner)
      in
      (reached, fun () -> ()))

let inherited_offset_preflight scheduled () =
  List.iter
    (fun first_executed ->
      let contents =
        if scheduled then "class Span {$$=$$+8;$$=(\"A\"[0]=66);};"
        else "class Span {$$=$$+8;$$=8;};"
      in
      with_offset ~contents (fun table namespace owner _ ->
          let offsets = ref 0 in
          let reached progress receipt =
            incr offsets;
            if !offsets = 1 then
              let _, authority, attempt, context, destination =
                fragment table namespace owner progress receipt
              in
              let execution =
                Lowering.prepare ~context ~authority ~runtime:owner destination
                |> diagnostics
              in
              if first_executed then
                VM.execute_task_offset owner attempt execution |> diagnostics
              else (
                ignore
                  (Record.finish_runtime_aggregate_offset
                     (Fragment.preparation authority)
                     ~value:8L ~work:(Program.steps execution)
                  |> checked);
                VM.fail_task_offset owner attempt |> checked;
                Alcotest.(check bool)
                  "fabricated predecessor has no successful execution" true
                  (VM.task_offset owner receipt = None))
            else if !offsets <> 2 then
              Alcotest.fail "unexpected extra aggregate offset"
            else if scheduled then (
              let _, authority, attempt, context, destination =
                fragment table namespace owner progress receipt
              in
              let execution =
                Lowering.prepare ~context ~authority ~runtime:owner destination
                |> diagnostics
              in
              let (Program.Scheduled program) = Program.code execution in
              let has_store =
                Program.entry program |> Ir_x87_stack.graph
                |> Ir_block_graph.blocks
                |> List.exists (fun block ->
                    Ir_block_graph.instructions block
                    |> Ir_instruction_sequence.instructions
                    |> List.exists (fun instruction ->
                        (Ir_instruction_sequence.description instruction).opcode
                        = Ir_opcode.Ic_assign))
              in
              Alcotest.(check bool)
                "later offset contains an actual memory store" true has_store;
              let before = VM.task_progress owner in
              let result = VM.execute_task_offset owner attempt execution in
              if first_executed then (
                diagnostics result;
                let offset = Option.get (VM.task_offset owner receipt) in
                Alcotest.(check int64)
                  "valid predecessor permits the store result" 66L
                  (Record.aggregate_offset_value offset);
                Alcotest.(check bool)
                  "valid later offset executes instructions" true
                  (VM.task_executed_steps owner > before.executed_steps);
                Alcotest.(check int)
                  "valid later offset allocates its literal" 2
                  ((VM.task_progress owner).literal_bytes - before.literal_bytes))
              else (
                (match result with
                | Ok () ->
                    Alcotest.fail
                      "a fabricated predecessor authorized a later store"
                | Error errors ->
                    Alcotest.(check bool)
                      "inherited dependency fails during zero-step preflight"
                      true
                      (List.exists
                         (fun error ->
                           error.VM.code = "HCIRVM0026"
                           && error.stage = VM.Preflight
                           && error.executed_steps = 0)
                         errors));
                Alcotest.(check bool)
                  "failed preflight preserves work, storage and output" true
                  (VM.task_progress owner = before);
                Alcotest.(check bool)
                  "rejected later offset publishes no result" true
                  (VM.task_offset owner receipt = None);
                reject "failed later execution cannot replay"
                  (VM.execute_task_offset owner attempt execution)))
            else
              let before = VM.task_progress owner in
              let result, work =
                VM.prepare_task_aggregate_offset owner ~table ~namespace
                  ~queries:[] progress receipt
              in
              if first_executed then (
                let offset = checked result in
                Alcotest.(check int64)
                  "valid predecessor permits the closed offset" 8L
                  (Record.aggregate_offset_value offset);
                Alcotest.(check bool)
                  "valid closed offset performs preparation" true (work > 0))
              else (
                reject "closed offset also requires its inherited execution"
                  result;
                Alcotest.(check int)
                  "rejected closed offset performs no preparation" 0 work;
                Alcotest.(check bool)
                  "rejected closed offset preserves all task progress" true
                  (VM.task_progress owner = before);
                Alcotest.(check bool)
                  "rejected closed offset leaves its source phase unconsumed"
                  true
                  (Record.aggregate_offset_is_current ~table ~namespace progress
                     receipt))
          in
          ( reached,
            fun () ->
              Alcotest.(check int)
                "both original offset callbacks were reached" 2 !offsets )))
    [ false; true ]

let position_evidence () =
  let module Resolution = Holyc_lib__Sema.Function_call_resolution in
  let module Source = Holyc_lib__Sema.Initializer_source in
  with_offset (fun table namespace owner _ ->
      let saved = ref None in
      let reached progress receipt =
        let view = VM.task_snapshot owner |> checked in
        let create () =
          Fragment.create ~table ~namespace ~progress ~receipt
            ~environment:(Globals.task_environment view)
            ~references:[] ~queries:[]
        in
        let selected = create () |> checked in
        let other = create () |> checked in
        let node, non_position =
          match Fragment.expression selected with
          | Ast.Binary_expression binary ->
              (binary.binary_left, binary.binary_right)
          | _ -> Alcotest.fail "expected original binary expression"
        in
        let position = Fragment.position_for selected node |> checked in
        reject "literal cannot obtain position evidence"
          (Fragment.position_for selected non_position);
        let copied =
          match node with
          | Ast.Current_position_expression operator ->
              Ast.Current_position_expression operator
          | _ -> Alcotest.fail "expected original current position"
        in
        reject "copied source node is not original position"
          (Fragment.position_for selected copied);
        let expression kind =
          Resolution.make_argument_expression ~kind
            ~origin:(Ast.expression_location node |> Source.origin_of_location)
        in
        let checked_position =
          expression
            (Resolution.Unresolved_expression
               (Resolution.Aggregate_position_expression position))
        in
        let validate ?offset_fragment source expression =
          Resolution.validate_source_expression ?offset_fragment ~source
            ~expression ~calls:[] ()
        in
        validate ~offset_fragment:selected node checked_position |> checked;
        reject "position cannot borrow another fragment with equal source"
          (validate ~offset_fragment:other node checked_position);
        reject "position cannot become an ordinary expression"
          (validate node checked_position);
        reject "position cannot substitute an equal source wrapper"
          (validate ~offset_fragment:selected copied checked_position);
        reject "ordinary instruction pointer cannot replace aggregate position"
          (validate ~offset_fragment:selected node
             (expression
                (Resolution.Unresolved_expression
                   Resolution.Current_position_expression)));
        reject "integer bits cannot replace original position evidence"
          (validate ~offset_fragment:selected node
             (expression (Resolution.Integer_literal 0L)));
        let _, authority, attempt, context, destination =
          fragment table namespace owner progress receipt
        in
        let execution =
          Lowering.prepare ~context ~authority ~runtime:owner destination
          |> diagnostics
        in
        VM.execute_task_offset owner attempt execution |> diagnostics;
        reject "consumed phase cannot capture another position" (create ());
        saved := Some position
      in
      ( reached,
        fun () ->
          Alcotest.(check int64)
            "immutable position survives without execution authority" 0L
            (Fragment.position_value (Option.get !saved)) ))

let failed_preparation () =
  with_offset ~max_initializer_steps:2 (fun table namespace owner _ ->
      let reached progress receipt =
        let fragment, authority, attempt, context, destination =
          fragment table namespace owner progress receipt
        in
        reject "preparation cannot exceed remaining allowance"
          (Lowering.prepare ~context ~authority ~runtime:owner destination);
        VM.fail_task_offset owner attempt |> checked;
        Alcotest.(check int)
          "failed preparation keeps work" 2
          (VM.task_initializer_steps owner);
        Alcotest.(check int)
          "failed preparation never runs graph" 0
          (VM.task_executed_steps owner);
        reject "failed preparation consumes source phase"
          (Fragment.authorize fragment);
        reject "failed attempt cannot restart"
          (VM.begin_task_offset owner authority);
        Alcotest.(check bool)
          "failed attempt has no offset" true
          (VM.task_offset owner receipt = None)
      in
      (reached, fun () -> ()))

let completion_is_single_use () =
  let module Ledger = Holyc_lib__Driver.Task_declarations in
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let runtime = VM.create_task_state ~table () |> checked in
  let ledger = Ledger.create ~runtime session |> checked in
  let completed = ref false in
  let declaration = function
    | Parser.Aggregate_advanced
        ({ phase_step = Parser.Aggregate_offset_reached _; _ } as phase) ->
        let view = VM.task_snapshot runtime |> checked in
        let before = VM.task_initializer_steps runtime in
        let authority, attempt =
          Ledger.begin_runtime_offset ledger ~runtime ~task_view:view phase
          |> diagnostics
        in
        let context =
          Typing.create_context ~table ~parent:(Ledger.initializer_scope ledger)
          |> checked
        in
        let typed =
          Typing.prepare_offset context (Fragment.authorized_fragment authority)
          |> checked
        in
        let destination = Destination.create ~task_view:view typed |> checked in
        let execution =
          Lowering.prepare ~context ~authority ~runtime destination
          |> diagnostics
        in
        VM.execute_task_offset runtime attempt execution |> diagnostics;
        reject "completion cannot replace its starting counter"
          (Ledger.finish_runtime_offset ledger ~runtime ~before:(before + 1)
             ~succeeded:true phase);
        Ledger.finish_runtime_offset ledger ~runtime ~before ~succeeded:true
          phase
        |> diagnostics;
        let work = Ledger.offset_work ledger in
        reject "ledger completion cannot replay"
          (Ledger.finish_runtime_offset ledger ~runtime ~before ~succeeded:true
             phase);
        Alcotest.(check int)
          "replayed completion does not charge" work
          (Ledger.offset_work ledger);
        completed := true;
        Ok ()
    | event -> Ledger.observe ledger event
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (Ledger.observe_command ledger);
      call = None;
      implicit_output = None;
      reference = None;
      query = None;
      declaration = Some declaration;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"offset-completion.hc"
      ~contents:"class Span {$$=1+7;};"
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  ignore
    (Parser.parse ~commands ~sources:(Session.sources session)
       ~definitions:(Session.definitions session)
       ~symbols:(Session.symbols session) ~config source);
  Alcotest.(check bool) "completion callback reached" true !completed

let standalone_dependencies () =
  List.iter
    (fun (tail, expected_dependencies) ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"owned-offset.hc"
          ~contents:
            ("#exe {I64 N=8;class Span {$$=N;I64 x;};" ^ tail
           ^ "StreamPrint(\"42;\");}")
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Jit () |> checked
      in
      let report =
        run_integer_program_report ~max_steps:10000 session ~config ~source
      in
      ignore (integer_program_report_outcome report |> diagnostics);
      let definitions =
        integer_program_report_task_units report
        |> List.concat_map integer_program_functions
        |> List.filter (fun (definition : VM.function_definition) ->
            Semantic_symbol.name (Ir_function_body.symbol definition.body) = "F")
      in
      Alcotest.(check bool) "function actually compiled" true (definitions <> []);
      List.iter
        (fun (definition : VM.function_definition) ->
          Alcotest.(check int)
            "each independent runtime offset is retained once"
            expected_dependencies
            (List.length (Ir_function_body.offset_dependencies definition.body));
          match
            VM.execute_function ~max_steps:100 ~max_frame_bytes:1024
              ~frame:definition.frame ~arguments:[] definition.body
          with
          | Ok _ ->
              Alcotest.fail
                "runtime offset escaped its task through derived layout"
          | Error errors ->
              Alcotest.(check bool)
                "dependency rejected before execution" true
                (List.exists
                   (fun error ->
                     error.VM.code = "HCIRVM0026" && error.executed_steps = 0)
                   errors))
        definitions)
    (List.map
       (fun tail -> (tail, 1))
       [
         "I64 F(){return sizeof(Span)+26;};";
         "I64 F(){U8 A[sizeof(Span)];return 42;};";
         "I64 F(){static U8 A[sizeof(Span)];return 42;};";
         "U8 A[sizeof(Span)];I64 F(){return sizeof(A)+26;};";
         "class B {$$=sizeof(Span);};I64 F(){return sizeof(B)+26;};";
         "class B {$$=sizeof(Span);"
         ^ String.concat "" (List.init 20 (fun _ -> "$$=$$+sizeof(Span);"))
         ^ "};I64 F(){return sizeof(B)-294;};";
         "class B {U8 data[sizeof(Span)];};I64 F(){return sizeof(B)+26;};";
       ]
    @ [
        ( "class B {$$=1+ #exe {class Noise {$$=N;};} $$;};I64 F(){return \
           sizeof(B)+33;};",
          1 );
        ( "class B {$$=N+ #exe {class Noise {$$=N;};} $$;};I64 F(){return \
           sizeof(B)+26;};",
          2 );
      ])

let () =
  Alcotest.run "runtime offset authority"
    [
      ( "offsets",
        [
          Alcotest.test_case "typed execution owns original offset result"
            `Quick execution_lifetime;
          Alcotest.test_case "matching metadata is not execution" `Quick
            metadata_is_not_execution;
          Alcotest.test_case "closed offsets validate inherited execution"
            `Quick
            (inherited_offset_preflight false);
          Alcotest.test_case "offset stores validate inherited execution" `Quick
            (inherited_offset_preflight true);
          Alcotest.test_case "positions retain exact source and fragment" `Quick
            position_evidence;
          Alcotest.test_case "failure consumes original bounded attempt" `Quick
            failed_preparation;
          Alcotest.test_case "derived layouts retain runtime dependencies"
            `Quick standalone_dependencies;
          Alcotest.test_case "ledger completion charges once" `Quick
            completion_is_single_use;
        ] );
    ]
