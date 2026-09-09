module VM = Ir.Integer_interpreter

type stream = VM.task_stream

type t = {
  session : Session.t;
  config : Frontend.Preprocessor.Config.t;
  state : VM.task_state;
  declarations : Task_declarations.t;
  identity : unit ref;
  mutable commands : (Frontend.Ast.module_ * command) list;
}

and command = {
  owner : unit ref;
  program : Integer_program.compiled;
  span : Common.Span.t;
  mutable frontend_pending : bool;
}

let create ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work ?max_generated_bytes ?max_stream_depth session =
  let session = Session.task_frontend session in
  let config =
    match Frontend.Preprocessor.Config.create ~compilation_mode:Jit () with
    | Ok config -> config
    | Error message -> invalid_arg message
  in
  VM.create_task_state ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work ?max_generated_bytes ?max_stream_depth
    ~table:(Session.semantic_symbols session)
    ()
  |> fun result ->
  Result.bind result (fun state ->
      Task_declarations.create ~runtime:state session
      |> Result.map (fun declarations ->
          {
            session;
            config;
            state;
            declarations;
            identity = ref ();
            commands = [];
          }))

let frontend task = task.session
let output_bytes task = VM.task_output_bytes task.state
let output_work task = VM.task_output_work task.state
let generated_bytes task = VM.task_generated_bytes task.state
let executed_steps task = VM.task_executed_steps task.state
let initializer_steps task = VM.task_initializer_steps task.state
let dimension_work task = Task_declarations.dimension_work task.declarations
let begin_stream task = VM.begin_task_stream task.state
let finish_stream task stream = VM.finish_task_stream task.state stream
let abort_stream task stream = VM.abort_task_stream task.state stream

let same_item left right =
  let open Frontend.Ast in
  match (left, right) with
  | Aggregate_forward_declaration left, Aggregate_forward_declaration right ->
      left == right
  | Aggregate_definition left, Aggregate_definition right -> left == right
  | Global_variable left, Global_variable right -> left == right
  | Global_declaration left, Global_declaration right -> left == right
  | Function_prototype left, Function_prototype right -> left == right
  | Function_definition left, Function_definition right -> left == right
  | Top_level_statement left, Top_level_statement right ->
      statement_location left == statement_location right
  | _ -> false

let same_syntax (left : Frontend.Ast.module_) (right : Frontend.Ast.module_) =
  left == right
  || left.items <> []
     && List.length left.items = List.length right.items
     && List.for_all2 same_item left.items right.items

let compile_ast_internal ?declaration_command task (ast : Frontend.Ast.module_)
    =
  match
    List.find_opt (fun (source, _) -> same_syntax source ast) task.commands
  with
  | Some (_, command) -> Ok command
  | None
    when List.exists
           (fun (source, _) ->
             List.exists
               (fun prior -> List.exists (same_item prior) ast.items)
               source.Frontend.Ast.items)
           task.commands ->
      Error
        [
          Integer_source.diagnostic ~span:ast.span "HCIRVM0026"
            "parsed syntax already belongs to another compiled task command";
        ]
  | None ->
      let ( let* ) = Result.bind in
      let* checked =
        Integer_program.compile_task_ast ~task:task.state ?declaration_command
          task.session ~config:task.config ast
      in
      let command =
        {
          owner = task.identity;
          program = checked.Integer_program.value;
          span = ast.span;
          frontend_pending = Option.is_none declaration_command;
        }
      in
      task.commands <- (ast, command) :: task.commands;
      Ok command

let compile_ast task ast = compile_ast_internal task ast

let execute task command =
  let program = command.program in
  if task.identity != command.owner then
    Error
      [
        Integer_source.diagnostic ~span:command.span "HCIRVM0026"
          "compiled command belongs to another task";
      ]
  else
    let outcome =
      VM.execute_task_program task.state
        ~runtime_calls:(Integer_program.runtime_calls program)
        ~globals:(Integer_program.globals program)
        ~initialization:(Integer_program.initialization program)
        ~functions:(Integer_program.functions program)
        (Integer_program.entry program)
      |> Result.map_error
           (Integer_execution_diagnostics.of_errors ~span:command.span)
    in
    let publication =
      if not command.frontend_pending then Ok ()
      else
        match
          VM.task_admission task.state
            ~globals:(Integer_program.globals program)
            ~entry:(Integer_program.entry program)
        with
        | None -> Ok ()
        | Some receipt ->
            Task_declarations.observe_admission task.declarations receipt
            |> Result.map_error (fun message ->
                [
                  Integer_source.diagnostic ~span:command.span "HCRUN0004"
                    message;
                ])
            |> Result.map (fun () -> command.frontend_pending <- false)
    in
    match (outcome, publication) with
    | Ok value, Ok () -> Ok value
    | Error errors, Ok () | Ok _, Error errors -> Error errors
    | Error errors, Error publication_errors ->
        Error (errors @ publication_errors)

let run task ~source =
  let ( let* ) = Result.bind in
  let* () =
    match
      Common.Source_manager.find
        (Session.sources task.session)
        (Common.Source_file.id source)
    with
    | Some registered when registered == source -> Ok ()
    | _ ->
        Error
          [
            Integer_source.diagnostic
              ~span:(Integer_source.source_span source)
              "HCRUN0004" "task input is not the exact registered source";
          ]
  in
  let commands : Frontend.Parser.command_sink =
    {
      checkpoint = Some (Task_declarations.observe_command task.declarations);
      query = Some (Task_declarations.observe_query task.declarations);
      reference = Some (Task_declarations.observe_reference task.declarations);
      declaration = Some (Task_declarations.observe task.declarations);
      dimension_count =
        Some (Task_declarations.grammar_dimension_count task.declarations);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Frontend.Parser.parse ~commands
      ~sources:(Session.sources task.session)
      ~definitions:(Session.definitions task.session)
      ~symbols:(Session.symbols task.session)
      ~config:task.config source
  in
  match parsed.ast with
  | None -> Error parsed.diagnostics
  | Some ast ->
      let* declaration_command = Task_declarations.seal task.declarations ast in
      let* command = compile_ast_internal ~declaration_command task ast in
      execute task command
