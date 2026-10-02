open Holyc_lib
module VM = Ir_integer_interpreter
module C = Semantic_declaration_collection
module Fragment = Holyc_lib__Sema.Default_fragment
module Globals = Holyc_lib__Ir.Integer_globals
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Destination = Holyc_lib__Ir.Default_fragment_destination
module Program = Holyc_lib__Ir.Default_fragment_program
module Calls = Holyc_lib__Ir.Runtime_call_context
module Lower = Holyc_lib__Ir.Integer_program_lowering
module Typed = Holyc_lib__Sema.Function_call_expression_result
module Initialization = Holyc_lib__Ir.Global_initialization

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let original_program ~contents ~values () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let owner = VM.create_task_state ~table () |> checked in
  let foreign = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner namespace |> checked;
  VM.bind_task_namespace foreign namespace |> checked;
  let publication = ref None and saved = ref [] in
  let context =
    Typing.create_context ~table ~parent:(C.namespace_scope namespace)
    |> checked
  in
  let declaration = function
    | Parser.Function_declared source ->
        publication := Some (C.publish_function namespace source |> checked)
    | Parser.Parameter_default_completed receipt ->
        let view = VM.task_snapshot owner |> checked in
        let attempt =
          VM.begin_task_default owner ~namespace
            ~publication:(Option.get !publication) receipt
          |> checked
        in
        let fragment =
          Fragment.create ~table ~publication:(Option.get !publication) ~receipt
            ~environment:(Globals.task_environment view)
            ~references:[] ~queries:[]
          |> checked
        in
        let authority = Fragment.authorize ~namespace fragment |> checked in
        let typed = Typing.prepare_default context fragment |> checked in
        let destination = Destination.create ~task_view:view typed |> checked in
        let globals = Destination.globals destination in
        let span = Destination.span destination in
        let lower ?(globals = globals) typed =
          let root =
            Typed.top_level_statements typed
            |> List.concat_map Typed.top_level_statement_roots
            |> List.hd
          in
          Lower.lower_complete ~globals ~records:(Typing.records context) ~span
            [ Lower.Expression (Typed.top_level_root_value root) ]
          |> diagnostics
        in
        let context_for typed entry =
          let initialization =
            Initialization.create ~span ~globals ~entry [] |> diagnostics
          in
          let runtime_calls =
            Calls.create ~records:(Typing.records context)
              ~function_sources:(Typing.function_sources context)
              ~top_level:typed ~initialization ~entry ~entry_calls:[]
              ~functions:[]
            |> diagnostics
          in
          (initialization, runtime_calls)
        in
        let lowered = lower typed in
        let entry = Lower.graph lowered in
        let initialization, runtime_calls = context_for typed entry in
        let program =
          Program.create ~authority ~destination ~lowered ~entry ~initialization
            ~runtime_calls
          |> checked
        in
        let retyped = Typing.prepare_default context fragment |> checked in
        let _, copied_context = context_for retyped entry in
        reject "call-free defaults retain their exact typed context"
          (Program.create ~authority ~destination ~lowered ~entry
             ~initialization ~runtime_calls:copied_context);
        let copied_lowering = lower retyped in
        let copied_entry = Lower.graph copied_lowering in
        let copied_initialization, copied_calls =
          context_for typed copied_entry
        in
        reject "equal copied typed values grant no default expression authority"
          (Program.create ~authority ~destination ~lowered:copied_lowering
             ~entry:copied_entry ~initialization:copied_initialization
             ~runtime_calls:copied_calls);
        let other_globals =
          Destination.create ~task_view:view typed
          |> checked |> Destination.globals
        in
        let other_lowering = lower ~globals:other_globals typed in
        let other_entry = Lower.graph other_lowering in
        let other_initialization, other_calls = context_for typed other_entry in
        reject "same typed default cannot borrow another globals context"
          (Program.create ~authority ~destination ~lowered:other_lowering
             ~entry:other_entry ~initialization:other_initialization
             ~runtime_calls:other_calls);
        let empty =
          Lower.lower_complete ~globals ~span [ Lower.Empty span ]
          |> diagnostics
        in
        let empty_entry = Lower.graph empty in
        let empty_initialization, empty_calls = context_for typed empty_entry in
        reject "source facts cannot authorize empty default IR"
          (Program.create ~authority ~destination ~lowered:empty
             ~entry:empty_entry ~initialization:empty_initialization
             ~runtime_calls:empty_calls);
        reject "original lowering cannot authorize an empty default graph"
          (Program.create ~authority ~destination ~lowered ~entry:empty_entry
             ~initialization:empty_initialization ~runtime_calls:empty_calls);
        reject "another lowering cannot authorize the original default graph"
          (Program.create ~authority ~destination ~lowered:empty ~entry
             ~initialization ~runtime_calls);
        let graph = Ir_x87_stack.graph entry in
        let descriptions =
          Ir_block_graph.blocks graph
          |> List.map (fun block ->
              let instructions =
                Ir_block_graph.instructions block
                |> Ir_instruction_sequence.instructions
                |> List.map (fun instruction ->
                    let description =
                      Ir_instruction_sequence.description instruction
                    in
                    match description.payload with
                    | Some (Ir_instruction_sequence.Integer _) ->
                        {
                          description with
                          payload = Some (Ir_instruction_sequence.Integer 9L);
                        }
                    | _ -> description)
              in
              ({ block_id = Ir_block_graph.block_id block; instructions }
                : Ir_block_graph.block_description))
        in
        let changed =
          Ir_block_graph.create
            ~entry:(Ir_block_graph.block_id (Ir_block_graph.entry graph))
            descriptions
          |> diagnostics |> Ir_x87_stack.verify |> diagnostics
        in
        let changed_initialization, changed_calls = context_for typed changed in
        reject "changed constants cannot supply original default values"
          (Program.create ~authority ~destination ~lowered ~entry:changed
             ~initialization:changed_initialization ~runtime_calls:changed_calls);
        Alcotest.(check int)
          "rejected substitutions execute no instructions"
          (5 * receipt.default_parameter_index)
          (VM.task_executed_steps owner);
        Alcotest.(check int)
          "graph construction charges no initializer work" 0
          (VM.task_initializer_steps owner);
        let execution =
          Program.prepare ~authority ~destination
            ~code:(Program.Scheduled program) ~steps:0
          |> checked
        in
        reject "default execution belongs to its task"
          (VM.execute_task_default foreign attempt execution);
        VM.execute_task_default owner attempt execution |> diagnostics;
        Alcotest.(check (option int64))
          "original evaluated default value"
          (Some (List.nth values receipt.default_parameter_index))
          (VM.task_default_bits owner receipt);
        reject "default execution cannot replay"
          (VM.execute_task_default owner attempt execution);
        reject "default boundary cannot begin again"
          (VM.begin_task_default owner ~namespace
             ~publication:(Option.get !publication) receipt);
        saved := (fragment, attempt, execution) :: !saved
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
    Session.add_source session ~path:"default-graph-authority.hc" ~contents
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors parsed);
  Alcotest.(check int)
    "all original defaults executed"
    (List.length values * 5)
    (VM.task_executed_steps owner);
  List.iter
    (fun (fragment, attempt, execution) ->
      reject "default authority expires"
        (Fragment.authorize ~namespace fragment);
      reject "execution cannot outlive its source attempt"
        (VM.execute_task_default owner attempt execution))
    !saved

let () =
  Alcotest.run "runtime default graph authority"
    [
      ( "defaults",
        [
          Alcotest.test_case "original scheduled expression owns its graph"
            `Quick
            (original_program ~contents:"I64 F(I64 x=20+22);" ~values:[ 42L ]);
          Alcotest.test_case
            "ordered defaults retain distinct expression ownership" `Quick
            (original_program ~contents:"I64 F(I64 x=20+22,I64 y=3+4);"
               ~values:[ 42L; 7L ]);
        ] );
    ]
