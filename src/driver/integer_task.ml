module VM = Ir.Integer_interpreter

type stream = VM.task_stream
type progress = { runtime : VM.task_progress; dimension_work : int }

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
  program : Integer_unit.compiled;
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

let adopt_source_with_promotion promote ?max_steps ?max_initializer_steps
    ?max_global_bytes ?max_literal_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work ?max_generated_bytes ?max_stream_depth
    session ~source ~ledger =
  let ( let* ) = Result.bind in
  let* config = Frontend.Preprocessor.Config.create ~compilation_mode:Jit () in
  let* state =
    VM.create_task_state ?max_steps ?max_initializer_steps ?max_global_bytes
      ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
      ?max_output_work ?max_generated_bytes ?max_stream_depth
      ~table:(Session.semantic_symbols session)
      ()
  in
  let* () = promote ledger ~runtime:state session ~source in
  Ok
    {
      session;
      config;
      state;
      declarations = ledger;
      identity = ref ();
      commands = [];
    }

let adopt_source = adopt_source_with_promotion Task_declarations.promote_source

let adopt_source_for_activation =
  adopt_source_with_promotion Task_declarations.promote_source_for_activation

let output_bytes task = VM.task_output_bytes task.state
let output_work task = VM.task_output_work task.state
let generated_bytes task = VM.task_generated_bytes task.state
let executed_steps task = VM.task_executed_steps task.state
let initializer_steps task = VM.task_initializer_steps task.state
let dimension_work task = Task_declarations.dimension_work task.declarations

let progress task =
  {
    runtime = VM.task_progress task.state;
    dimension_work = dimension_work task;
  }

let admit_global task publication =
  Task_declarations.admit_global task.declarations ~runtime:task.state
    publication

let prepare_initializer_context task receipt =
  let ( let* ) = Result.bind in
  let span =
    receipt.Frontend.Parser.leaf_initializer.initializer_owner.global_name
      .location
      .span
  in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority =
    Task_declarations.initializer_fragment_authority task.declarations
      ~runtime:task.state ~task_view receipt
  in
  let fragment = Sema.Initializer_fragment.authorized_fragment authority in
  let* context =
    Initializer_fragment_typing.create_context
      ~table:(Session.semantic_symbols task.session)
      ~parent:(Task_declarations.initializer_scope task.declarations)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare context fragment |> diagnose
  in
  Ok (context, authority, task_view, typed)

let prepare_initializer task receipt =
  prepare_initializer_context task receipt
  |> Result.map (fun (_, _, _, typed) -> typed)

let prepare_default_context task receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.default_ast.location.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority =
    Task_declarations.default_fragment_authority task.declarations
      ~runtime:task.state ~task_view receipt
  in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let* context =
    Initializer_fragment_typing.create_context
      ~table:(Session.semantic_symbols task.session)
      ~parent:(Task_declarations.initializer_scope task.declarations)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare_default context fragment |> diagnose
  in
  Ok (context, authority, task_view, typed)

let prepare_parameter_default task receipt =
  prepare_default_context task receipt
  |> Result.map (fun (_, _, _, typed) -> typed)

let prepare_initializer_destination_context task ~destination receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.leaf_initializer.initializer_equals.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* context, authority, task_view, typed =
    prepare_initializer_context task receipt
  in
  let* declaration =
    match Ir.Integer_initializer_layout.declared_owner destination with
    | Some declaration -> Ok declaration
    | None ->
        Error "initializer destination has no original declared layout"
        |> diagnose
  in
  let* reference, slot =
    match
      VM.admitted_publication_for_symbol task.state
        (Sema.Compiler_record.declared_global_symbol declaration)
    with
    | Some (VM.Admitted_declared_global (reference, slot)) ->
        Ok (reference, slot)
    | _ ->
        Error "initializer destination has no retained declared object"
        |> diagnose
  in
  let* destination =
    Ir.Initializer_fragment_destination.create ~task_view ~reference ~slot
      ~layout:destination typed
    |> diagnose
  in
  Ok (context, authority, destination)

let prepare_initializer_destination task ~destination receipt =
  prepare_initializer_destination_context task ~destination receipt
  |> Result.map (fun (_, _, destination) -> destination)

let lower_initializer_fragment task ~destination receipt =
  let ( let* ) = Result.bind in
  let* context, authority, destination =
    prepare_initializer_destination_context task ~destination receipt
  in
  Initializer_fragment_lowering.lower ~context ~authority destination

let execute_initializer_leaf task receipt =
  let ( let* ) = Result.bind in
  let* attempt =
    Task_declarations.begin_initializer_attempt task.declarations
      ~runtime:task.state receipt
  in
  let outcome =
    let destination = VM.initializer_attempt_destination attempt in
    let* context, authority, destination =
      prepare_initializer_destination_context task ~destination receipt
    in
    let* execution =
      Initializer_fragment_lowering.prepare ~context ~authority
        ~runtime:task.state destination
    in
    VM.execute_task_initializer task.state attempt execution
    |> Result.map_error
         (Integer_execution_diagnostics.of_errors
            ~span:
              receipt.Frontend.Parser.leaf_initializer.initializer_equals.span)
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_initializer_attempt task.state attempt)
  | Ok () -> ());
  outcome

let execute_parameter_default task receipt =
  let ( let* ) = Result.bind in
  let* attempt =
    Task_declarations.begin_default_attempt task.declarations
      ~runtime:task.state receipt
  in
  let outcome =
    let* context, authority, task_view, typed =
      prepare_default_context task receipt
    in
    let span = receipt.Frontend.Parser.default_ast.location.span in
    let* destination =
      Ir.Default_fragment_destination.create ~task_view typed
      |> Result.map_error (fun message ->
          [ Integer_source.message_diagnostic ~span message ])
    in
    let* execution =
      Default_fragment_lowering.prepare ~context ~authority ~runtime:task.state
        destination
    in
    VM.execute_task_default task.state attempt execution
    |> Result.map_error (Integer_execution_diagnostics.of_errors ~span)
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_default task.state attempt)
  | Ok () -> ());
  outcome

let observe_initializer task event =
  (match event with
    | Frontend.Parser.Global_declared publication ->
        admit_global task publication
    | Frontend.Parser.Global_initializer_started start ->
        Task_declarations.begin_initializer_runtime task.declarations
          ~runtime:task.state start
    | Frontend.Parser.Global_initializer_delimiter_completed receipt ->
        Task_declarations.observe_initializer_delimiter task.declarations
          ~runtime:task.state receipt
    | Frontend.Parser.Global_initializer_leaf_completed receipt ->
        execute_initializer_leaf task receipt
    | Frontend.Parser.Parameter_default_completed receipt -> (
        match receipt.default_ast.value with
        | Frontend.Ast.Expression_default _ ->
            execute_parameter_default task receipt
        | Frontend.Ast.Lastclass_default _ -> Ok ())
    | Frontend.Parser.Function_header_completed header ->
        Task_declarations.complete_defaults_runtime task.declarations
          ~runtime:task.state header
    | Frontend.Parser.Global_completed (_, completed)
      when Option.is_some completed.global_initial_value ->
        Task_declarations.complete_initializer_runtime task.declarations
          ~runtime:task.state event
    | _ -> Ok ())
  |> Result.map_error
       (List.map (fun (error : Common.Diagnostic.t) ->
            if
              error.code = "HCRUN0004"
              && String.starts_with ~prefix:"HC" error.message
              && String.contains error.message ':'
            then
              let decoded =
                Integer_source.message_diagnostic ~span:error.primary
                  error.message
              in
              { error with code = decoded.code; message = decoded.message }
            else error))

let compiled_units task =
  List.rev_map (fun (_, command) -> command.program) task.commands

let compile_isolated task ~source_command session ~config parsed =
  Integer_unit.compile_source_in_task_budget ~task:task.state ~source_command
    session ~config parsed

let execute_isolated task program =
  VM.execute_isolated_program_in_task task.state
    ~runtime_calls:(Integer_unit.runtime_calls program)
    ~globals:(Integer_unit.globals program)
    ~initialization:(Integer_unit.initialization program)
    ~functions:(Integer_unit.functions program)
    (Integer_unit.entry program)

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
        Integer_unit.compile_task_ast ~task:task.state ?declaration_command
          task.session ~config:task.config ast
      in
      let command =
        {
          owner = task.identity;
          program = checked.Integer_unit.value;
          span = ast.span;
          frontend_pending = Option.is_none declaration_command;
        }
      in
      task.commands <- (ast, command) :: task.commands;
      Ok command

let compile_ast task ast = compile_ast_internal task ast

let compile_source_ast task ast =
  Result.bind (Task_declarations.seal task.declarations ast)
    (fun declaration_command ->
      compile_ast_internal ~declaration_command task ast)

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
        ~runtime_calls:(Integer_unit.runtime_calls program)
        ~globals:(Integer_unit.globals program)
        ~initialization:(Integer_unit.initialization program)
        ~functions:(Integer_unit.functions program)
        (Integer_unit.entry program)
      |> Result.map_error
           (Integer_execution_diagnostics.of_errors ~span:command.span)
    in
    let publication =
      if not command.frontend_pending then Ok ()
      else
        match
          VM.task_admission task.state
            ~globals:(Integer_unit.globals program)
            ~entry:(Integer_unit.entry program)
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

let stream_diagnostics span message =
  let code, detail =
    match String.index_opt message ':' with
    | Some separator ->
        ( String.sub message 0 separator,
          String.sub message (separator + 1)
            (String.length message - separator - 1)
          |> String.trim )
    | None -> ("HCIRVM0027", message)
  in
  [ Integer_source.diagnostic ~span code detail ]

let activate_source task ~span =
  Task_declarations.activate_source task.declarations ~runtime:task.state ~span
    ~declaration:(observe_initializer task) ~command:(fun ast ->
      Result.bind (compile_source_ast task ast) (fun command ->
          execute task command |> Result.map ignore))

let result task ~sequence =
  VM.task_result task.state ~sequence
  |> Result.map_error (fun message ->
      [
        Integer_source.diagnostic
          ~span:sequence.Frontend.Parser.sequence_ast.span "HCRUN0004" message;
      ])

let stream_executor task span =
  let ( let* ) = Result.bind in
  let* stream =
    begin_stream task |> Result.map_error (stream_diagnostics span)
  in
  let context = ref None in
  let sequence = ref None in
  let aborted = ref false in
  let closed = ref false in
  let invalid () =
    Error
      (stream_diagnostics span
         "HCIRVM0027: parser executor does not own the active stream context")
  in
  let active () =
    if !closed || !aborted || not (VM.task_stream_is_active task.state stream)
    then invalid ()
    else Ok ()
  in
  let owns candidate =
    match !context with
    | Some owner -> owner == candidate
    | None -> false
  in
  let reading candidate =
    let* () = active () in
    if owns candidate && Option.is_none !sequence then Ok () else invalid ()
  in
  let command_context (start : Frontend.Parser.command_start) =
    start.command_context
  in
  let reference selection =
    let* () =
      reading (Frontend.Parser.selected_command selection |> command_context)
    in
    Task_declarations.observe_execution_reference task.declarations selection
  in
  let query event =
    let open Frontend.Parser in
    let root =
      match event with
      | Query_root root -> root
      | Query_member_started start -> start.member_start_root
      | Query_member member -> member.query_member_root
      | Query_completed completed -> completed.query_root
    in
    let* () = reading root.query_command.command_context in
    Task_declarations.observe_query task.declarations event
  in
  let declaration event =
    let open Frontend.Parser in
    let start =
      match event with
      | Array_dimension_preparing preparation ->
          preparation.dimension_owner.dimensions_command
      | Array_dimension_completed completed ->
          completed.dimension_preparation.dimension_owner.dimensions_command
      | Global_declared publication | Global_completed (publication, _) ->
          publication.global_header.declaration_command
      | Global_initializer_started start ->
          start.initializer_owner.global_header.declaration_command
      | Global_initializer_leaf_completed leaf ->
          leaf.leaf_initializer.initializer_owner.global_header
            .declaration_command
      | Global_initializer_delimiter_completed delimiter ->
          delimiter.delimiter_initializer.initializer_owner.global_header
            .declaration_command
      | Function_declared publication ->
          publication.function_header.declaration_command
      | Parameter_default_completed receipt ->
          receipt.default_function.function_header.declaration_command
      | Function_header_completed header | Function_body_completed (header, _)
        -> header.function_publication.function_header.declaration_command
    in
    let* () = reading start.command_context in
    let* () = Task_declarations.observe task.declarations event in
    observe_initializer task event
  in
  let dimension_count (completed : Frontend.Parser.completed_array_dimension) =
    let* () =
      reading
        completed.dimension_preparation.dimension_owner.dimensions_command
          .command_context
    in
    Task_declarations.grammar_dimension_count task.declarations completed
  in
  let commands : Frontend.Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            let open Frontend.Parser in
            let candidate =
              match event with
              | Sequence_started candidate | Sequence_aborted candidate ->
                  candidate
              | Command_started start -> start.command_context
              | Command_completed completed | Command_resumed completed ->
                  completed.command_start.command_context
              | Sequence_completed completed -> completed.sequence_context
            in
            let* () =
              match event with
              | Sequence_aborted _ when owns candidate && not !aborted ->
                  (* Source cleanup must survive an earlier explicit buffer
                     abort. It grants no compilation or execution progress. *)
                  Ok ()
              | _ -> active ()
            in
            let* () =
              match event with
              | Sequence_started _ when Option.is_none !context ->
                  context := Some candidate;
                  Ok ()
              | Sequence_started _ -> invalid ()
              | Sequence_aborted _ when owns candidate -> Ok ()
              | _ -> reading candidate
            in
            let* () =
              Task_declarations.observe_command task.declarations event
            in
            match event with
            | Frontend.Parser.Command_resumed completed ->
                let ast = completed.command_ast in
                let* command = compile_source_ast task ast in
                execute task command |> Result.map ignore
            | Frontend.Parser.Sequence_completed completed ->
                sequence := Some completed;
                Ok ()
            | Frontend.Parser.Sequence_aborted _ ->
                aborted := true;
                Ok ()
            | _ -> Ok ());
      query = Some query;
      reference = Some reference;
      declaration = Some declaration;
      dimension_count = Some dimension_count;
      command = (fun _ -> active ());
      resume = active;
    }
  in
  Ok
    Frontend.Parser.
      {
        definitions = Session.definitions task.session;
        symbols = Session.symbols task.session;
        commands;
        finish =
          (fun () ->
            let* () = active () in
            match !sequence with
            | Some completed
              when owns completed.sequence_context
                   && sequence_accepted completed ->
                let* generated =
                  finish_stream task stream
                  |> Result.map_error (stream_diagnostics span)
                in
                closed := true;
                Ok generated
            | _ ->
                Error
                  (stream_diagnostics span
                     "HCIRVM0027: stream sequence has not been accepted"));
        abort =
          (fun () ->
            match abort_stream task stream with
            | Ok () -> closed := true
            | Error _ -> ());
      }

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
