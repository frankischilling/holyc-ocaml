open Holyc_lib
module D = Task_declarations
module VM = Ir_integer_interpreter
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Preparation = Holyc_lib__Driver.Integer_initializers
module Fragment = Holyc_lib__Sema.Default_fragment
module Destination = Holyc_lib__Ir.Default_fragment_destination
module Lower = Holyc_lib__Ir.Integer_program_lowering
module Typed = Holyc_lib__Sema.Function_call_expression_result

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let evaluation ?(publish = true) ?(tamper = false) ?(hold = false)
    ?(max_steps = 100) ?(contents = "I64 F(I64 x=20+22){return x;};") ?works
    ?(values = [ 42L ]) () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let source =
    Session.add_source session ~path:"default-authority.hc" ~contents
  in
  let ledger = D.create_source session ~source |> checked in
  let owner = VM.create_task_state ~table () |> checked in
  let foreign = VM.create_task_state ~table () |> checked in
  let completed = ref [] in
  let declaration event =
    D.observe ledger event |> diagnostics;
    (match event with
    | Parser.Parameter_default_completed receipt -> (
        let authority =
          D.begin_source_default ledger ~runtime:owner receipt |> diagnostics
        in
        let fragment = Fragment.authorized_fragment authority in
        let context =
          Typing.create_aot_context ~table ~parent:(D.initializer_scope ledger)
          |> checked
        in
        let typed = Typing.prepare_default context fragment |> checked in
        let destination = Destination.create_source typed |> checked in
        let globals = Destination.globals destination in
        let span = Destination.span destination in
        let lower value =
          Lower.lower_complete ~globals ~span [ Lower.Expression value ]
          |> diagnostics
        in
        let root = Typed.top_level_root_value (Destination.root destination) in
        let lowered = lower root in
        let empty =
          Lower.lower_complete ~globals ~span [ Lower.Empty span ]
          |> diagnostics
        in
        reject "source metadata cannot authorize empty constant IR"
          (VM.prepare_default_constant owner ~authority ~destination
             ~lowered:empty ~max_steps:100);
        let copied = Typing.prepare_default context fragment |> checked in
        let copied_root =
          Typed.top_level_statements copied
          |> List.concat_map Typed.top_level_statement_roots
          |> List.hd |> Typed.top_level_root_value
        in
        reject
          "copied typed value cannot authorize original constant evaluation"
          (VM.prepare_default_constant owner ~authority ~destination
             ~lowered:(lower copied_root) ~max_steps:100);
        let other_destination = Destination.create_source typed |> checked in
        reject "another globals context cannot borrow the original lowering"
          (VM.prepare_default_constant owner ~authority
             ~destination:other_destination ~lowered ~max_steps:100);
        let before = VM.task_initializer_steps owner in
        let prepared, work =
          Preparation.prepare_default ~runtime:owner ~authority ~max_steps
            ~top_calls:[] destination
          |> fun result ->
          if max_steps < 5 then (
            reject "failed evaluation produces no completion" result;
            (None, 0))
          else
            match diagnostics result with
            | Preparation.Prepared_default result, work -> (Some result, work)
            | _ -> Alcotest.fail "constant default was scheduled"
        in
        match prepared with
        | None ->
            Alcotest.(check int)
              "failed evaluation retains reached work" 4
              (VM.task_initializer_steps owner - before);
            reject "failed original evaluation cannot restart"
              (VM.prepare_default_constant owner ~authority ~destination
                 ~lowered ~max_steps:100);
            Alcotest.(check int)
              "failed replay charges no work" (before + 4)
              (VM.task_initializer_steps owner)
        | Some result ->
            Alcotest.(check int64)
              "actual original evaluated bits"
              (List.nth values receipt.default_parameter_index)
              (VM.default_constant_bits result);
            let expected_work =
              match works with
              | None -> 5
              | Some works -> List.nth works receipt.default_parameter_index
            in
            Alcotest.(check int)
              "actual original evaluation work" expected_work work;
            Alcotest.(check int)
              "owning invocation pays automatically" (before + expected_work)
              (VM.task_initializer_steps owner);
            reject "successful original evaluation cannot replay"
              (VM.prepare_default_constant owner ~authority ~destination
                 ~lowered ~max_steps:100);
            List.iter
              (fun prior ->
                reject "another original expression cannot borrow a completion"
                  (D.finish_source_default ledger prior))
              !completed;
            let foreign_result =
              VM.prepare_default_constant foreign ~authority ~destination
                ~lowered ~max_steps:100
              |> diagnostics
            in
            reject
              "equal-value foreign evaluation cannot complete this invocation"
              (D.finish_source_default ledger foreign_result);
            reject
              "original completion cannot be consumed by another invocation"
              (VM.consume_default_constant foreign result);
            if tamper then (
              VM.record_task_preparation owner
                ~before:(VM.task_initializer_steps owner)
                ~steps:1;
              reject "changed work cannot complete an actual value"
                (D.finish_source_default ledger result))
            else if not hold then (
              D.finish_source_default ledger result |> diagnostics;
              reject "completion cannot replay"
                (D.finish_source_default ledger result);
              reject "consumption is single use"
                (VM.consume_default_constant owner result));
            completed := result :: !completed)
    | Parser.Function_header_completed header
      when publish && (not tamper) && (not hold) && max_steps >= 5 ->
        D.complete_source_defaults ledger header |> diagnostics
    | _ -> ());
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (D.observe_command ledger);
      call = None;
      implicit_output = None;
      reference = Some (D.observe_reference ledger);
      declaration = Some declaration;
      query = Some (D.observe_query ledger);
      dimension_count = Some (D.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config = Preprocessor.Config.create ~compilation_mode:Aot () |> checked in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors output);
  List.iter
    (fun result ->
      reject "completion cannot outlive callback"
        (D.finish_source_default ledger result);
      reject "consumption cannot outlive callback"
        (VM.consume_default_constant owner result))
    !completed;
  let ast = Option.get output.ast in
  let sealed = D.seal_source ledger ast in
  if publish && (not tamper) && (not hold) && max_steps >= 5 then
    let sealed = diagnostics sealed in
    let saved = D.source_defaults ~table ~ast sealed |> diagnostics in
    Alcotest.(check (list int64))
      "seal retains only actual original values" values
      (List.map Holyc_lib__Ir.Prepared_parameter_default.bits saved)
  else reject "uncompleted values cannot enter output seal" sealed

let missing_preparation () =
  List.iter
    (fun contents ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"missing-default.hc" ~contents
      in
      let ledger = D.create_source session ~source |> checked in
      let commands : Parser.command_sink =
        {
          checkpoint = Some (D.observe_command ledger);
          call = None;
          implicit_output = None;
          reference = Some (D.observe_reference ledger);
          declaration = Some (D.observe ledger);
          query = Some (D.observe_query ledger);
          dimension_count = Some (D.grammar_dimension_count ledger);
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Aot () |> checked
      in
      let output =
        Parser.parse ~commands ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      Alcotest.(check bool)
        "unprepared source parsed" false (Parser.has_errors output);
      reject "unused default requires preparation before sealing"
        (D.seal_source ledger (Option.get output.ast)))
    [ "I64 F(I64 x=20+22){return x;};"; "extern I64 F(I64 x=20+22);" ]

let original_position_evidence () =
  let module Resolution = Holyc_lib__Sema.Function_call_resolution in
  let module Source = Holyc_lib__Sema.Initializer_source in
  let module Record = Holyc_lib__Sema.Compiler_record in
  List.iter
    (fun contents ->
      let session = Session.create () in
      let table = Session.semantic_symbols session in
      let sources = Session.sources session in
      let source =
        Session.add_source session ~path:"position-authority.hc" ~contents
      in
      let positions = Record.create_compiler_positions ~sources in
      let ledger =
        D.create_source ~compiler_positions:positions session ~source |> checked
      in
      let owner = VM.create_task_state ~table () |> checked in
      let reached = ref 0 in
      let check authority =
        incr reached;
        let fragment = Fragment.authorized_fragment authority in
        let node = Fragment.expression fragment in
        let position = Fragment.position_for fragment node |> checked in
        Alcotest.(check int64)
          "first original header write" 0L
          (Fragment.position_value position);
        let copied =
          match node with
          | Ast.Current_position_expression operator ->
              Ast.Current_position_expression operator
          | _ -> Alcotest.fail "expected original current position node"
        in
        reject "copied operator cannot obtain an original read"
          (Fragment.position_for fragment copied);
        let other =
          Fragment.with_positions ~compiler_positions:positions fragment
          |> checked
        in
        let expression kind =
          Resolution.make_argument_expression ~kind
            ~origin:(Ast.expression_location node |> Source.origin_of_location)
        in
        let value =
          expression
            (Resolution.Unresolved_expression
               (Resolution.Default_position_expression position))
        in
        let validate ?default_fragment source expression =
          Resolution.validate_source_expression ?default_fragment ~source
            ~expression ~calls:[] ()
        in
        validate ~default_fragment:fragment node value |> checked;
        reject "equal original source cannot borrow another fragment"
          (validate ~default_fragment:other node value);
        reject "default position cannot become an ordinary instruction pointer"
          (validate node value);
        reject "equal copied node cannot borrow original evidence"
          (validate ~default_fragment:fragment copied value);
        reject "instruction pointer cannot replace a default position"
          (validate ~default_fragment:fragment node
             (expression
                (Resolution.Unresolved_expression
                   Resolution.Current_position_expression)));
        reject "same source manager without original writes grants no position"
          (Fragment.with_positions
             ~compiler_positions:(Record.create_compiler_positions ~sources)
             fragment);
        reject "foreign source manager grants no position"
          (Fragment.with_positions
             ~compiler_positions:
               (Record.create_compiler_positions
                  ~sources:(Source_manager.create ()))
             fragment)
      in
      let declaration event =
        D.observe ledger event |> diagnostics;
        (match event with
        | Parser.Parameter_default_completed receipt ->
            D.begin_source_default ledger ~runtime:owner receipt
            |> diagnostics |> check
        | Parser.Callback_default_completed receipt ->
            D.begin_source_callback_default ledger ~runtime:owner receipt
            |> diagnostics |> check
        | Parser.Callback_position_written receipt ->
            reject "anonymous write cannot replay"
              (Record.record_callback_position positions ~parameters:[] receipt)
        | _ -> ());
        Ok ()
      in
      let commands : Parser.command_sink =
        {
          checkpoint = Some (D.observe_command ledger);
          call = None;
          implicit_output = None;
          reference = Some (D.observe_reference ledger);
          declaration = Some declaration;
          query = Some (D.observe_query ledger);
          dimension_count = Some (D.grammar_dimension_count ledger);
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Aot () |> checked
      in
      let output =
        Parser.parse ~commands ~sources
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      Alcotest.(check bool)
        "original position source parsed" false (Parser.has_errors output);
      Alcotest.(check int) "one exact default was checked" 1 !reached)
    [ "I64 F(I64 n=$$);"; "I64 F(I64 (*p)(I64 n=$$));" ]

let () =
  Alcotest.run "AOT default authority"
    [
      ( "preparation",
        [
          Alcotest.test_case
            "default positions require exact original write and node" `Quick
            original_position_evidence;
          Alcotest.test_case
            "positional defaults execute their actual constants" `Quick
            (evaluation ~contents:"I64 F(I64 x=$$,I64 y=$$+34){return x+y;};"
               ~values:[ 0L; 42L ] ~works:[ 3; 5 ]);
          Alcotest.test_case "only actual owning evaluation can complete" `Quick
            evaluation;
          Alcotest.test_case "sealing requires header publication" `Quick
            (evaluation ~publish:false);
          Alcotest.test_case "sealing requires even unused defaults" `Quick
            missing_preparation;
          Alcotest.test_case
            "equal-valued defaults keep their original expression" `Quick
            (evaluation ~contents:"I64 F(I64 x=20+22,I64 y=40+2){return x+y;};"
               ~values:[ 42L; 42L ]);
          Alcotest.test_case "changed work cannot complete actual evaluation"
            `Quick (evaluation ~tamper:true);
          Alcotest.test_case "unconsumed evaluation expires with callback"
            `Quick (evaluation ~hold:true);
          Alcotest.test_case "failed evaluation retains work and cannot restart"
            `Quick (evaluation ~max_steps:4);
          Alcotest.test_case "exact constant evaluation budget" `Quick
            (evaluation ~max_steps:5);
        ] );
    ]
