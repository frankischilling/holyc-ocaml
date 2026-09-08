module VM = Ir.Integer_interpreter

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
}

let create ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work session =
  let config =
    match Frontend.Preprocessor.Config.create ~compilation_mode:Jit () with
    | Ok config -> config
    | Error message -> invalid_arg message
  in
  VM.create_task_state ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work
    ~table:(Session.semantic_symbols session)
    ()
  |> fun result ->
  Result.bind result (fun state ->
      Task_declarations.create session
      |> Result.map (fun declarations ->
          {
            session;
            config;
            state;
            declarations;
            identity = ref ();
            commands = [];
          }))

let output_bytes task = VM.task_output_bytes task.state
let output_work task = VM.task_output_work task.state
let executed_steps task = VM.task_executed_steps task.state
let initializer_steps task = VM.task_initializer_steps task.state

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
      let* task_view =
        VM.task_snapshot task.state
        |> Result.map_error (fun message ->
            [ Integer_source.diagnostic ~span:ast.span "HCRUN0004" message ])
      in
      let before = VM.task_initializer_steps task.state in
      let max_initializer_steps =
        VM.task_initializer_limit task.state - before
      in
      let* checked =
        Integer_program.compile_task_ast ~task_view ~max_initializer_steps
          ?declaration_command
          ~retained_function_source:(VM.task_function_source task.state)
          ~initializer_progress:(fun steps ->
            VM.record_task_preparation task.state ~before ~steps)
          task.session ~config:task.config ast
      in
      let command =
        {
          owner = task.identity;
          program = checked.Integer_program.value;
          span = ast.span;
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
    VM.execute_task_program task.state
      ~runtime_calls:(Integer_program.runtime_calls program)
      ~globals:(Integer_program.globals program)
      ~initialization:(Integer_program.initialization program)
      ~functions:(Integer_program.functions program)
      (Integer_program.entry program)
    |> Result.map_error
         (Integer_execution_diagnostics.of_errors ~span:command.span)

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
      reference = None;
      declaration = Some (Task_declarations.observe task.declarations);
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
