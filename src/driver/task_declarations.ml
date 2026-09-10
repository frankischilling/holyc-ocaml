module Ast = Frontend.Ast
module Parser = Frontend.Parser
module Visibility = Frontend.Symbol_visibility
module Collection = Sema.Declaration_collection
module VM = Ir.Integer_interpreter

module Names = Hashtbl.Make (struct
  type t = Ast.identifier

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

module Entries = Hashtbl.Make (struct
  type t = Visibility.entry

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

module Query_roots = Hashtbl.Make (struct
  type t = Parser.query_root

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

module Query_expressions = Hashtbl.Make (struct
  type t = Ast.expression

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

module Dimensions = Hashtbl.Make (struct
  type t = Ast.array_dimension

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

type dimension_evaluation =
  | Observed_dimension
  | Failed_dimension
  | Prepared_dimension of Sema.Compiler_record.dimension_preparation

type pending_dimension = {
  preparation : Parser.array_dimension_preparation;
  mutable evaluation : dimension_evaluation;
}

type reading_dimensions = {
  owner : Parser.array_dimensions_owner;
  mutable pending : pending_dimension option;
  mutable completed_rev : Parser.completed_array_dimension list;
  mutable next_index : int;
}

type source =
  | Global of {
      publication : Parser.global_publication;
      mutable completed : Ast.global_declarator option;
      mutable initializing : Sema.Initializer_source.pending option;
    }
  | Function of {
      publication : Parser.function_publication;
      mutable defaults_rev : Parser.completed_parameter_default list;
      mutable header : Parser.completed_function_header option;
      mutable body : Ast.function_definition option;
    }

type assigned = {
  publication : Collection.publication;
  ordinal : int;
  source : source;
  mutable claimed : bool;
}

type storage_boundary = {
  storage_source : Parser.global_publication;
  storage_predecessor : Parser.completed_command option;
  storage_previous_global : Sema.Symbol.t option;
  mutable storage_declaration : Sema.Compiler_record.declared_global option;
  mutable storage_completion : Parser.declaration_event option;
}

type reference_stage =
  | Global_selection of Parser.global_publication * Ast.global_declarator option
  | Provisional_function_selection of Parser.function_publication
  | Function_selection of
      Parser.completed_function_header * Ast.function_definition option

type reference_target =
  | Selected_absent
  | Selected_unbound of Visibility.entry
  | Selected_local
  | Selected_source of {
      publication : Collection.publication;
      stage : reference_stage;
      admitted : VM.admitted_publication option;
    }
  | Selected_runtime of VM.admitted_publication

type selected_reference = {
  selection : Parser.reference_selection;
  target : reference_target;
}

type query = {
  query_receipt : Parser.completed_query;
  query_target : reference_target;
  query_selection : Sema.Query_selection.t;
}

type reading_query = {
  target : reference_target;
  mutable sizeof_read : Sema.Compiler_record.sizeof_read option;
  mutable member_start : Parser.query_member_start option;
  mutable members_rev : Parser.query_member list;
  mutable completed : bool;
}

type command = {
  table : Sema.Symbol_table.t;
  runtime : VM.task_state option;
  ast : Ast.module_;
  declarations : Collection.t;
  references : selected_reference Names.t;
  queries : query Query_expressions.t;
  dimensions : Parser.completed_array_dimension Dimensions.t;
  checked_dimensions : Sema.Compiler_record.declared_dimension Dimensions.t;
  initializers : (Ast.global_initializer * Sema.Initializer_source.t) Names.t;
  source_order : Sema.Task_command_order.command option;
}

type source_command = Source_command of command

type authority =
  | Semantic_analysis
  | Source_compilation of Common.Source_file.t
  | Task_runtime of VM.task_state

type command_phase =
  | Ready
  | Reading of Parser.command_start
  | Pending of Parser.completed_command
  | Closed
  | Aborted

type parsed_command = {
  receipt : Parser.completed_command;
  mutable sealed : bool;
}

type command_sequence = {
  context : Parser.command_context;
  mutable phase : command_phase;
  mutable completed_rev : parsed_command list;
}

type t = {
  storage_boundaries : storage_boundary Names.t;
  mutable last_storage_global : Sema.Symbol.t option;
  session : Session.t;
  table : Sema.Symbol_table.t;
  sources : Common.Source_manager.t;
  symbols : Visibility.Environment.t;
  namespace : Collection.namespace;
  names : assigned Names.t;
  entries : assigned Entries.t;
  mutable commands : command list;
  mutable next_ordinal : int;
  mutable sequences : command_sequence list;
  mutable active : command_sequence list;
  mutable views : (Ast.module_ * parsed_command list) list;
  mutable sequence_views : (Ast.module_ * Parser.completed_sequence) list;
  mutable authority : authority;
  mutable source_events_rev : Parser.command_event list;
  mutable activation_events_rev : Sema.Source_activation.event list;
  mutable activation : Sema.Source_activation.t option;
  max_dimension_work : int;
  mutable dimension_work : int;
  mutable source_dimensions_rev :
    Sema.Compiler_record.dimension_preparation list;
  runtime_entries : VM.admitted_publication Entries.t;
  runtime_records : (Sema.Compiler_record.t, string) result Entries.t;
  mutable admissions : VM.task_admission list;
  references : selected_reference Names.t;
  query_roots : reading_query Query_roots.t;
  queries : query Query_expressions.t;
  dimension_owners : reading_dimensions Names.t;
  dimensions : Parser.completed_array_dimension Dimensions.t;
  checked_dimensions : Sema.Compiler_record.declared_dimension Dimensions.t;
  initializers : (Ast.global_initializer * Sema.Initializer_source.t) Names.t;
}

exception Invalid of Common.Diagnostic.t

let fail ?(code = "HCRUN0004") span message =
  raise
    (Invalid
       (Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error
          ~primary:span ~message ()))

let protect run =
  try Ok (run ()) with Invalid diagnostic -> Error [ diagnostic ]

let checked span = function
  | Ok value -> value
  | Error message -> fail span message

let origin (name : Ast.identifier) =
  let location = name.location in
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let create_with_authority ?(max_dimension_work = 100_000) authority session =
  let runtime =
    match authority with
    | Task_runtime runtime -> Some runtime
    | _ -> None
  in
  let table = Session.semantic_symbols session in
  if
    Option.fold ~none:false
      ~some:(fun task -> not (VM.task_owns_table task table))
      runtime
  then Error "task declaration runtime belongs to another semantic table"
  else
    let module_name =
      match authority with
      | Source_compilation source ->
          Some (Common.Source_file.display_path source)
      | _ -> None
    in
    Collection.create_namespace ~table ?module_name () |> fun result ->
    Result.bind result (fun namespace ->
        let binding =
          match authority with
          | Task_runtime runtime -> VM.bind_task_namespace runtime namespace
          | _ -> Ok ()
        in
        Result.map
          (fun () ->
            {
              storage_boundaries = Names.create 16;
              last_storage_global = None;
              session;
              table;
              namespace;
              sources = Session.sources session;
              symbols = Session.symbols session;
              names = Names.create 32;
              entries = Entries.create 32;
              commands = [];
              next_ordinal = 0;
              sequences = [];
              active = [];
              views = [];
              sequence_views = [];
              authority;
              source_events_rev = [];
              activation_events_rev = [];
              activation = None;
              max_dimension_work;
              dimension_work = 0;
              source_dimensions_rev = [];
              runtime_entries = Entries.create 32;
              runtime_records = Entries.create 32;
              admissions = [];
              references = Names.create 32;
              query_roots = Query_roots.create 16;
              queries = Query_expressions.create 16;
              dimension_owners = Names.create 16;
              dimensions = Dimensions.create 16;
              checked_dimensions = Dimensions.create 16;
              initializers = Names.create 16;
            })
          binding)

let create ?runtime session =
  create_with_authority
    (match runtime with
    | None -> Semantic_analysis
    | Some runtime -> Task_runtime runtime)
    session

let create_source ?(max_dimension_work = 100_000) session ~source =
  if max_dimension_work <= 0 then
    Error "source preparation limit must be positive"
  else
    match
      Common.Source_manager.find (Session.sources session)
        (Common.Source_file.id source)
    with
    | Some registered when registered == source ->
        create_with_authority ~max_dimension_work (Source_compilation source)
          session
    | _ -> Error "ordinary source ledger requires its exact registered input"

let promote_source_with_activation ~activate ledger ~runtime session ~source =
  if
    ledger.session != session
    || ledger.sources != Session.sources session
    || ledger.symbols != Session.symbols session
    || ledger.table != Session.semantic_symbols session
    || not (VM.task_owns_table runtime ledger.table)
  then Error "source promotion requires its exact frontend and semantic table"
  else
    match (ledger.authority, ledger.active, ledger.sequences) with
    | Source_compilation original, [ active ], [ sequence ]
      when original == source && active == sequence
           && Parser.context_is_current active.context
                ~observed_events:(List.length ledger.source_events_rev)
           && Parser.context_source active.context == source
           && Parser.context_mode active.context = Frontend.Preprocessor.Jit
           && Option.is_none (Parser.context_parent active.context)
           && (match active.phase with
             | Ready | Reading _ | Pending _ -> true
             | Closed | Aborted -> false)
           && ledger.commands = [] ->
        (if activate then
           Sema.Source_activation.create ~namespace:ledger.namespace
             ~context:active.context
             ~observed_events:(List.length ledger.source_events_rev)
             (List.rev ledger.activation_events_rev)
           |> fun result ->
           Result.bind result (fun activation ->
               VM.promote_task_source_activation runtime
                 ~namespace:ledger.namespace ~activation
                 ~dimensions:(List.rev ledger.source_dimensions_rev)
               |> Result.map (fun () ->
                   ledger.activation <- Some activation;
                   ledger.dimension_work <- 0))
         else
           VM.promote_task_source runtime ~namespace:ledger.namespace
             ~events:(List.rev ledger.source_events_rev)
             ~dimension_steps:ledger.dimension_work)
        |> Result.map (fun () ->
            if not activate then ledger.source_dimensions_rev <- [];
            ledger.authority <- Task_runtime runtime)
    | _ -> Error "source promotion requires its original live unsealed JIT root"

let promote_source = promote_source_with_activation ~activate:false

let promote_source_for_activation =
  promote_source_with_activation ~activate:true

let ledger_runtime ledger =
  match ledger.authority with
  | Task_runtime runtime -> Some runtime
  | _ -> None

let requires_query_metadata ledger =
  match ledger.authority with
  | Semantic_analysis -> false
  | _ -> true

let dimension_work ledger = ledger.dimension_work

let selected_dimensions ledger dimensions =
  List.filter_map (Dimensions.find_opt ledger.checked_dimensions) dimensions

let runtime_symbol = VM.admitted_source_symbol
let retained_for ledger entry = Entries.find_opt ledger.runtime_entries entry

let symbol_for ledger entry =
  match Entries.find_opt ledger.entries entry with
  | Some assigned -> Some (Collection.publication_symbol assigned.publication)
  | None -> retained_for ledger entry |> Option.map runtime_symbol

let frontend_origin symbol =
  match Sema.Symbol.origin symbol with
  | Sema.Symbol.Pinned_source { path; line } ->
      Visibility.Pinned_source { path; line }
  | Sema.Symbol.Synthesized _ -> Visibility.Session_registration
  | Sema.Symbol.Source_location location ->
      Visibility.Source_location
        {
          span = location.span;
          source_segments = location.source_segments;
          generated_from = location.generated_from;
          defined_at = location.defined_at;
        }

let observe_admission ledger receipt =
  let publications = VM.admission_publications receipt in
  if
    not
      (Option.fold ~none:false
         ~some:(fun runtime -> VM.owns_task_admission runtime receipt)
         (ledger_runtime ledger))
  then Error "runtime admission belongs to another task"
  else if List.exists (fun saved -> saved == receipt) ledger.admissions then
    Error "runtime admission was already published to the frontend"
  else if
    not
      (Option.fold ~none:false
         ~some:(fun runtime ->
           Option.fold ~none:false
             ~some:(fun current -> current == receipt)
             (VM.latest_task_admission runtime))
         (ledger_runtime ledger))
  then Error "runtime admission is no longer the current publication boundary"
  else if
    List.exists
      (fun publication ->
        not
          (Sema.Symbol_table.owns_symbol ledger.table
             (runtime_symbol publication)))
      publications
  then Error "runtime publication belongs to another semantic table"
  else
    let ( let* ) = Result.bind in
    let* () =
      List.fold_left
        (fun result publication ->
          let* () = result in
          match publication with
          | VM.Admitted_declared_global _ ->
              Error
                "partial storage cannot masquerade as a completed command \
                 admission"
          | VM.Admitted_function _ -> Ok ()
          | VM.Admitted_global (reference, slot) ->
              if
                Ir.Retained_global.symbol reference
                != Ir.Integer_globals.slot_symbol slot
              then Error "runtime global publication has another storage symbol"
              else
                Ir.Integer_globals.validate_slot_extent ~table:ledger.table slot)
        (Ok ()) publications
    in
    List.iter
      (fun publication ->
        let symbol = runtime_symbol publication in
        let kind, function_call_shape =
          match publication with
          | VM.Admitted_global _ | VM.Admitted_declared_global _ ->
              (Visibility.Global_variable, None)
          | VM.Admitted_function reference ->
              let module Function = Sema.Function_type_resolution in
              let signature =
                Ir.Retained_function.metadata reference
                |> Sema.Outer_environment.function_declaration
                |> Sema.Function_resolution.resolved_declaration_site
                |> Sema.Function_resolution.declaration_site_function
                |> Function.function_signature
              in
              let shape : Visibility.function_call_shape =
                {
                  parameters =
                    List.map
                      (fun parameter ->
                        Visibility.
                          {
                            parameter_name = Function.parameter_name parameter;
                            has_default =
                              Option.is_some
                                (Function.parameter_default parameter);
                          })
                      (Function.signature_parameters signature);
                  variadic =
                    Option.is_some
                      (Function.signature_variadic_origin signature);
                }
              in
              (Visibility.Function, Some shape)
        in
        let entry =
          Visibility.Environment.add ledger.symbols
            ~name:(Sema.Symbol.name symbol) ~kind
            ~origin:(frontend_origin symbol) ?function_call_shape ()
        in
        Entries.add ledger.runtime_entries entry publication;
        match publication with
        | VM.Admitted_declared_global _ -> assert false
        | VM.Admitted_function _ -> ()
        | VM.Admitted_global (_, slot) ->
            let record =
              Ir.Integer_globals.slot_record slot
              |> Sema.Global_record_classification.classified_record_source
            in
            Entries.add ledger.runtime_records entry
              (Sema.Compiler_record.bind_retained_global ~table:ledger.table
                 ~entry ~record
                 ~extent:(Ir.Integer_globals.slot_extent slot)))
      publications;
    ledger.admissions <- receipt :: ledger.admissions;
    Ok ()

let context_span context =
  let source = Parser.context_source context in
  Common.Span.unsafe_make
    ~source:(Common.Source_file.id source)
    ~start:0
    ~stop:(Common.Source_file.length source)

let active_sequence ledger context =
  match ledger.active with
  | sequence :: _ when sequence.context == context -> sequence
  | _ -> fail (context_span context) "parser command context is not active"

let same_option equal left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> equal left right
  | _ -> false

let observe_command_source ledger event =
  protect (fun () ->
      match event with
      | Parser.Sequence_started context ->
          let source = Parser.context_source context in
          let span = context_span context in
          if
            Parser.context_sources context != ledger.sources
            || Parser.context_environment context != ledger.symbols
            || not
                 (Option.fold ~none:false
                    ~some:(fun registered -> registered == source)
                    (Common.Source_manager.find ledger.sources
                       (Common.Source_file.id source)))
          then
            fail span
              "parser command context has a foreign source or environment";
          (match (ledger.authority, Parser.context_parent context) with
          | Source_compilation original, None when original != source ->
              fail span "ordinary source context belongs to another input"
          | _ -> ());
          if
            List.exists
              (fun sequence -> sequence.context == context)
              ledger.sequences
          then fail span "parser command context was already consumed";
          let parent_context, matches =
            match Parser.context_parent context with
            | None -> (None, fun _ -> false)
            | Some (Parser.Before_first_command parent) ->
                ( Some parent,
                  fun sequence ->
                    sequence.phase = Ready && sequence.completed_rev = [] )
            | Some (Parser.Reading_command start) ->
                ( Some start.command_context,
                  fun sequence ->
                    match sequence.phase with
                    | Reading saved -> saved == start
                    | _ -> false )
            | Some (Parser.Awaiting_resume command) ->
                ( Some command.command_start.command_context,
                  fun sequence ->
                    match sequence.phase with
                    | Pending saved -> saved == command
                    | _ -> false )
          in
          (match (parent_context, ledger.active) with
          | None, [] -> ()
          | Some parent, sequence :: _
            when sequence.context == parent && matches sequence -> ()
          | Some parent, []
            when Parser.context_environment parent != ledger.symbols -> ()
          | _ ->
              fail span
                "nested parser context does not match the suspended parent \
                 phase");
          let sequence = { context; phase = Ready; completed_rev = [] } in
          ledger.sequences <- sequence :: ledger.sequences;
          ledger.active <- sequence :: ledger.active
      | Parser.Command_started start ->
          let sequence = active_sequence ledger start.command_context in
          let previous =
            List.nth_opt sequence.completed_rev 0
            |> Option.map (fun entry -> entry.receipt)
          in
          if
            sequence.phase <> Ready
            || start.command_ordinal <> List.length sequence.completed_rev
            || not (same_option ( == ) start.command_predecessor previous)
          then
            fail
              (context_span start.command_context)
              "parser command start is repeated or out of order";
          sequence.phase <- Reading start
      | Parser.Command_completed receipt ->
          let sequence =
            active_sequence ledger receipt.command_start.command_context
          in
          (match sequence.phase with
          | Reading start when start == receipt.command_start -> ()
          | _ ->
              fail receipt.command_ast.span
                "parser command completion is foreign or out of order");
          let entry = { receipt; sealed = false } in
          sequence.completed_rev <- entry :: sequence.completed_rev;
          sequence.phase <- Pending receipt;
          ledger.views <- (receipt.command_ast, [ entry ]) :: ledger.views
      | Parser.Command_resumed receipt -> (
          let sequence =
            active_sequence ledger receipt.command_start.command_context
          in
          match sequence.phase with
          | Pending saved when saved == receipt -> sequence.phase <- Ready
          | _ ->
              fail receipt.command_ast.span
                "parser command resume is foreign, repeated or out of order")
      | Parser.Sequence_completed receipt ->
          let sequence = active_sequence ledger receipt.sequence_context in
          let entries = List.rev sequence.completed_rev in
          if
            sequence.phase <> Ready
            || List.length entries <> List.length receipt.sequence_commands
            || not
                 (List.for_all2
                    (fun entry command -> entry.receipt == command)
                    entries receipt.sequence_commands)
          then
            fail receipt.sequence_ast.span
              "parser sequence is incomplete or out of order";
          ledger.views <- (receipt.sequence_ast, entries) :: ledger.views;
          ledger.sequence_views <-
            (receipt.sequence_ast, receipt) :: ledger.sequence_views;
          sequence.phase <- Closed;
          ledger.active <- List.tl ledger.active
      | Parser.Sequence_aborted context -> (
          let tentative =
            List.exists
              (fun (_, receipt) ->
                receipt.Parser.sequence_context == context
                && not (Parser.sequence_accepted receipt))
              ledger.sequence_views
          in
          match
            List.find_opt
              (fun sequence -> sequence.context == context)
              ledger.sequences
          with
          | Some sequence when tentative && sequence.phase = Closed ->
              sequence.phase <- Aborted
          | _ ->
              let sequence = active_sequence ledger context in
              sequence.phase <- Aborted;
              ledger.active <- List.tl ledger.active))

let record_activation_event ledger event =
  match ledger.authority with
  | Source_compilation _ ->
      ledger.activation_events_rev <- event :: ledger.activation_events_rev
  | _ -> ()

let observe_command ledger event =
  let result =
    Result.bind (observe_command_source ledger event) (fun () ->
        match ledger_runtime ledger with
        | None -> Ok ()
        | Some runtime ->
            let context =
              match event with
              | Parser.Sequence_started context
              | Parser.Sequence_aborted context -> context
              | Parser.Command_started start -> start.command_context
              | Parser.Command_completed receipt
              | Parser.Command_resumed receipt ->
                  receipt.command_start.command_context
              | Parser.Sequence_completed receipt -> receipt.sequence_context
            in
            protect (fun () ->
                VM.observe_task_source_event runtime event
                |> checked (context_span context)))
  in
  Result.map
    (fun () -> ledger.source_events_rev <- event :: ledger.source_events_rev)
    result

let rec source_root context =
  match Parser.context_parent context with
  | None -> context
  | Some (Parser.Before_first_command parent) -> source_root parent
  | Some (Parser.Reading_command start) -> source_root start.command_context
  | Some (Parser.Awaiting_resume completed) ->
      source_root completed.command_start.command_context

let observe_command ledger event =
  Result.map
    (fun () ->
      record_activation_event ledger (Sema.Source_activation.Command event))
    (observe_command ledger event)

let validate_command ledger (header : Parser.declaration_header) =
  let start = header.declaration_command in
  let sequence = active_sequence ledger start.command_context in
  match sequence.phase with
  | Reading saved when saved == start -> ()
  | _ ->
      fail
        (context_span start.command_context)
        "declaration does not belong to the active parser command"

let selection_target ledger span = function
  | Visibility.Absent -> Selected_absent
  | Visibility.Shadowed_by_local -> Selected_local
  | Visibility.Present entry -> (
      match Entries.find_opt ledger.entries entry with
      | Some assigned ->
          let stage =
            match assigned.source with
            | Global state ->
                Global_selection (state.publication, state.completed)
            | Function state when state.publication.function_entry == entry ->
                Provisional_function_selection state.publication
            | Function { header = Some header; body; _ }
              when header.completed_entry == entry ->
                Function_selection (header, body)
            | Function _ ->
                fail span
                  "selected function entry has no original header witness"
          in
          let admitted =
            Option.bind (ledger_runtime ledger) (fun runtime ->
                VM.admitted_publication_for_symbol runtime
                  (Collection.publication_symbol assigned.publication))
          in
          Selected_source
            { publication = assigned.publication; stage; admitted }
      | None -> (
          match retained_for ledger entry with
          | Some publication -> Selected_runtime publication
          | None -> Selected_unbound entry))

let observe_reference ledger selection =
  protect (fun () ->
      let identifier = Parser.selected_identifier selection in
      let start = Parser.selected_command selection in
      let sequence = active_sequence ledger start.command_context in
      (match sequence.phase with
      | Reading saved when saved == start -> ()
      | _ ->
          fail identifier.location.span
            "identifier selection does not belong to the active parser command");
      if Parser.selected_environment selection != ledger.symbols then
        fail identifier.location.span
          "identifier selection belongs to another frontend environment";
      if Names.mem ledger.references identifier then
        fail identifier.location.span
          "identifier selection was already consumed";
      let target =
        selection_target ledger identifier.location.span
          (Parser.selected_lookup selection)
      in
      Names.add ledger.references identifier { selection; target })

let observe_reference ledger selection =
  Result.map
    (fun () ->
      record_activation_event ledger
        (Sema.Source_activation.Reference selection))
    (observe_reference ledger selection)

let validate_source_reference ledger selection =
  let identifier = Parser.selected_identifier selection in
  let span = identifier.location.span in
  protect (fun () ->
      (match ledger.authority with
      | Source_compilation _ -> ()
      | _ ->
          fail span
            "source reference validation requires its original source ledger");
      let reference =
        match Names.find_opt ledger.references identifier with
        | Some reference when reference.selection == selection -> reference
        | _ -> fail span "source read lacks its exact observed selection"
      in
      match reference.target with
      | Selected_absent ->
          fail ~code:"HCRUN0003" span
            "source expression identifier is absent at its source read"
      | Selected_unbound _ ->
          fail ~code:"HCRUN0003" span
            "source expression entry has no checked source publication"
      | Selected_local | Selected_source _ | Selected_runtime _ -> ())

let validate_execution_target selection target =
  let span = (Parser.selected_identifier selection).location.span in
  let unavailable message = fail ~code:"HCRUN0003" span message in
  match target with
  | Selected_absent ->
      unavailable "task expression identifier is absent at its source read"
  | Selected_unbound _ ->
      unavailable "task expression entry has no checked runtime publication"
  | Selected_source
      { stage = Provisional_function_selection _; admitted = None; _ } ->
      unavailable "function header is unfinished at its source read"
  | Selected_source { stage; admitted = None; _ } ->
      let start =
        match stage with
        | Global_selection (publication, _) ->
            publication.global_header.declaration_command
        | Provisional_function_selection publication ->
            publication.function_header.declaration_command
        | Function_selection (header, _) ->
            header.function_publication.function_header.declaration_command
      in
      if start != Parser.selected_command selection then
        unavailable
          "partial source publication has not reached runtime admission"
  | Selected_local | Selected_runtime _ | Selected_source _ -> ()

let observe_execution_reference ledger selection =
  let identifier = Parser.selected_identifier selection in
  let validate =
    protect (fun () ->
        if Option.is_none (ledger_runtime ledger) then
          fail identifier.location.span
            "execution reference observation requires an owning runtime")
  in
  Result.bind validate (fun () ->
      Result.bind (observe_reference ledger selection) (fun () ->
          protect (fun () ->
              validate_execution_target selection
                (Names.find ledger.references identifier).target)))

let read_sizeof ledger (root : Parser.query_root) target =
  match root.query_node with
  | Parser.Defined_target _ | Parser.Offset_target _ -> None
  | Parser.Sizeof_target _ when target = Selected_local -> (
      let function_publication =
        Entries.fold
          (fun _ assigned found ->
            match assigned.source with
            | Function state
              when state.publication.function_header.declaration_command
                   == root.query_command -> Some assigned.publication
            | _ -> found)
          ledger.entries None
      in
      match function_publication with
      | Some function_publication -> (
          match
            Sema.Compiler_record.read_local_sizeof ~table:ledger.table
              ~namespace:ledger.namespace ~function_publication ~root
              ~dimensions:
                (match root.query_local with
                | Some { local_source = Parser.Local_variable local; _ } ->
                    selected_dimensions ledger local.local_array_dimensions
                | _ -> [])
          with
          | Ok read -> Some read
          | Error message when requires_query_metadata ledger ->
              fail root.query_location.span message
          | Error _ -> None)
      | None ->
          fail root.query_location.span
            "local sizeof has no original function publication")
  | Parser.Sizeof_target _ -> (
      let record =
        match target with
        | Selected_source
            { publication; stage = Global_selection (source, _); _ } ->
            Some
              (Sema.Compiler_record.published_scalar ~table:ledger.table
                 ~namespace:ledger.namespace
                 ~dimensions:
                   (selected_dimensions ledger source.global_dimensions)
                 publication)
        | Selected_unbound entry ->
            Session.primitive_for ledger.session entry
            |> Option.map (fun binding -> Ok (Session.primitive_record binding))
        | Selected_runtime _ -> (
            match root.query_lookup with
            | Visibility.Present entry ->
                Entries.find_opt ledger.runtime_records entry
            | _ -> None)
        | _ -> None
      in
      match record with
      | Some (Ok record) ->
          Some
            (Sema.Compiler_record.read_sizeof ~table:ledger.table ~root record
            |> checked root.query_location.span)
      | Some (Error message) when requires_query_metadata ledger ->
          fail root.query_location.span message
      | None when requires_query_metadata ledger ->
          fail root.query_location.span
            "sizeof target has no selected compiler size metadata"
      | Some (Error _) | None -> None)

let observe_query ledger event =
  protect (fun () ->
      let root =
        match event with
        | Parser.Query_root root -> root
        | Parser.Query_member_started start -> start.member_start_root
        | Parser.Query_member member -> member.query_member_root
        | Parser.Query_completed query -> query.query_root
      in
      let span = root.query_location.span in
      let start = root.query_command in
      let sequence = active_sequence ledger start.command_context in
      (match sequence.phase with
      | Reading saved when saved == start -> ()
      | _ -> fail span "query read does not belong to the active parser command");
      if root.query_environment != ledger.symbols then
        fail span "query read belongs to another frontend environment";
      match event with
      | Parser.Query_root _ ->
          if Query_roots.mem ledger.query_roots root then
            fail span "query root was already consumed";
          let target = selection_target ledger span root.query_lookup in
          let sizeof_read = read_sizeof ledger root target in
          Query_roots.add ledger.query_roots root
            {
              target;
              sizeof_read;
              member_start = None;
              members_rev = [];
              completed = false;
            }
      | Parser.Query_member_started start ->
          let state =
            match Query_roots.find_opt ledger.query_roots root with
            | Some state
              when (not state.completed) && Option.is_none state.member_start ->
                state
            | _ ->
                fail span
                  "query dot has no active root or repeats a pending member"
          in
          let next =
            match state.members_rev with
            | [] -> 0
            | previous :: _ -> previous.query_member_ordinal + 1
          in
          if start.member_start_ordinal <> next then
            fail span "query dot is missing, replayed or out of order";
          if
            Option.fold ~none:false
              ~some:Sema.Compiler_record.sizeof_is_internal state.sizeof_read
          then
            fail start.member_start_dot.span
              "sizeof internal type cannot select a member";
          if not (requires_query_metadata ledger) then state.sizeof_read <- None;
          state.member_start <- Some start
      | Parser.Query_member member ->
          let state =
            match Query_roots.find_opt ledger.query_roots root with
            | Some state when not state.completed -> state
            | _ -> fail span "query member has no active observed root"
          in
          let next =
            match state.members_rev with
            | [] -> 0
            | previous :: _ -> previous.query_member_ordinal + 1
          in
          if member.query_member_ordinal <> next then
            fail span "query member is missing, replayed or out of order";
          let dot =
            match member.query_member_node with
            | Parser.Sizeof_member member -> member.sizeof_member_dot
            | Parser.Offset_member member -> member.offset_member_dot
          in
          (match state.member_start with
          | Some start
            when start == member.query_member_start
                 && start.member_start_dot == dot -> ()
          | _ -> fail span "query member lacks its original observed dot");
          if Option.is_some state.sizeof_read then
            fail span
              "sizeof requires the selected class's checked member layout";
          state.member_start <- None;
          state.members_rev <- member :: state.members_rev
      | Parser.Query_completed receipt ->
          let state =
            match Query_roots.find_opt ledger.query_roots root with
            | Some state when not state.completed -> state
            | _ -> fail span "query completion has no active observed root"
          in
          let members = List.rev state.members_rev in
          if
            Option.is_some state.member_start
            || List.length members <> List.length receipt.query_members
            || not (List.for_all2 ( == ) members receipt.query_members)
          then
            fail span "query completion lacks its original ordered member reads";
          if Query_expressions.mem ledger.queries receipt.query_expression then
            fail span "query expression was already completed";
          let query_selection =
            Sema.Query_selection.make ?sizeof_read:state.sizeof_read
              ~table:ledger.table ~receipt ()
            |> checked span
          in
          Query_expressions.add ledger.queries receipt.query_expression
            {
              query_receipt = receipt;
              query_target = state.target;
              query_selection;
            };
          state.completed <- true)

let validate_source ledger environment (header : Parser.declaration_header)
    (name : Ast.identifier) =
  validate_command ledger header;
  if
    environment != ledger.symbols
    || header.declaration_sources != ledger.sources
  then
    fail name.Ast.location.span
      "parser declaration belongs to another source or symbol environment";
  match
    Common.Source_manager.find ledger.sources
      (Common.Source_file.id header.declaration_source)
  with
  | Some source when source == header.declaration_source -> ()
  | _ ->
      fail name.location.span
        "parser declaration input is not the exact registered source"

let assign ledger (name : Ast.identifier) kind source entry =
  if Names.mem ledger.names name || Entries.mem ledger.entries entry then
    fail name.Ast.location.span
      "parser declaration publication was already consumed";
  if ledger.next_ordinal = max_int then
    fail name.location.span "task declaration publication order is exhausted";
  let publication =
    (match source with
      | Global state ->
          Collection.publish_global ledger.namespace state.publication
      | Function state ->
          Collection.publish_function ledger.namespace state.publication)
    |> checked name.location.span
  in
  if Sema.Symbol.kind (Collection.publication_symbol publication) <> kind then
    fail name.location.span "parser publication has the wrong declaration kind";
  let assigned =
    { publication; source; ordinal = ledger.next_ordinal; claimed = false }
  in
  ledger.next_ordinal <- ledger.next_ordinal + 1;
  Names.add ledger.names name assigned;
  Entries.add ledger.entries entry assigned

let find ledger (name : Ast.identifier) =
  match Names.find_opt ledger.names name with
  | Some assigned when not assigned.claimed -> assigned
  | Some _ ->
      fail name.Ast.location.span
        "parser publication already belongs to a sealed command"
  | None ->
      fail name.location.span
        "source declaration has no assigned parser publication"

let validate_dimension_owner ledger (owner : Parser.array_dimensions_owner) =
  let start = owner.dimensions_command in
  let span = owner.dimensions_name.location.span in
  let sequence = active_sequence ledger start.command_context in
  (match sequence.phase with
  | Reading saved when saved == start -> ()
  | _ ->
      fail span "array dimension does not belong to the active parser command");
  if owner.dimensions_environment != ledger.symbols then
    fail span "array dimension belongs to another frontend environment"

let prepare_dimension ledger (preparation : Parser.array_dimension_preparation)
    =
  let owner = preparation.dimension_owner in
  validate_dimension_owner ledger owner;
  let span = preparation.dimension_opening.span in
  let state =
    match Names.find_opt ledger.dimension_owners owner.dimensions_name with
    | Some state when state.owner == owner -> state
    | Some _ -> fail span "array dimension has a different prospective owner"
    | None ->
        if
          preparation.dimension_index <> 0
          || Option.is_some preparation.dimension_predecessor
        then fail span "array dimension preparation is missing its predecessor";
        let state =
          { owner; pending = None; completed_rev = []; next_index = 0 }
        in
        Names.add ledger.dimension_owners owner.dimensions_name state;
        state
  in
  if
    Option.is_some state.pending
    || preparation.dimension_index <> state.next_index
    || not
         (same_option ( == ) preparation.dimension_predecessor
            (List.nth_opt state.completed_rev 0))
  then fail span "array dimension preparation is repeated or out of order";
  let pending = { preparation; evaluation = Failed_dimension } in
  state.pending <- Some pending;
  match ledger.authority with
  | Semantic_analysis -> pending.evaluation <- Observed_dimension
  | Source_compilation _ | Task_runtime _ -> (
      let queries =
        Option.fold ~none:[] ~some:Sema.Query_selection.source_queries
          preparation.dimension_expression
        |> List.map (fun expression ->
            match Query_expressions.find_opt ledger.queries expression with
            | Some query ->
                Sema.Query_selection.checked_read query.query_selection
            | None ->
                fail span "array preparation is missing an original query read")
      in
      (* Expression lookahead can execute nested directives; take the baseline
         only after the parser returns the original expression. *)
      let before, limit =
        match ledger.authority with
        | Task_runtime task ->
            (VM.task_initializer_steps task, VM.task_initializer_limit task)
        | Source_compilation _ ->
            (ledger.dimension_work, ledger.max_dimension_work)
        | Semantic_analysis -> assert false
      in
      let result, work =
        Sema.Compiler_record.prepare_dimension ~table:ledger.table
          ~namespace:ledger.namespace ~max_work:(limit - before) ~preparation
          ~queries
      in
      (match ledger.authority with
      | Task_runtime task -> VM.record_task_preparation task ~before ~steps:work
      | _ -> ());
      ledger.dimension_work <- ledger.dimension_work + work;
      match result with
      | Ok prepared -> (
          pending.evaluation <- Prepared_dimension prepared;
          match ledger.authority with
          | Source_compilation _ ->
              ledger.source_dimensions_rev <-
                prepared :: ledger.source_dimensions_rev
          | _ -> ())
      | Error message ->
          let code, message =
            match String.index_opt message ':' with
            | Some separator when String.starts_with ~prefix:"HC" message ->
                ( String.sub message 0 separator,
                  String.trim
                    (String.sub message (separator + 1)
                       (String.length message - separator - 1)) )
            | _ -> ("HCRUN0004", message)
          in
          fail ~code span message)

let complete_dimension ledger (receipt : Parser.completed_array_dimension) =
  let preparation = receipt.dimension_preparation in
  let owner = preparation.dimension_owner in
  validate_dimension_owner ledger owner;
  let dimension = receipt.dimension_ast in
  let span = dimension.location.span in
  let state =
    match Names.find_opt ledger.dimension_owners owner.dimensions_name with
    | Some state
      when state.owner == owner
           && Option.fold ~none:false
                ~some:(fun pending -> pending.preparation == preparation)
                state.pending -> state
    | _ -> fail span "array dimension completion has no original preparation"
  in
  if
    dimension.opening_bracket != preparation.dimension_opening
    || (not
          (same_option ( == ) dimension.dimension_expression
             preparation.dimension_expression))
    || Dimensions.mem ledger.dimensions dimension
  then
    fail span
      "array dimension completion has substituted or repeated source children";
  if state.next_index = max_int then
    fail span "array dimension preparation index space is exhausted";
  let prepared =
    match (Option.get state.pending).evaluation with
    | Observed_dimension -> None
    | Failed_dimension ->
        fail span "array dimension preparation did not succeed"
    | Prepared_dimension prepared ->
        Some
          (Sema.Compiler_record.complete_dimension ~receipt prepared
          |> checked span)
  in
  state.pending <- None;
  state.completed_rev <- receipt :: state.completed_rev;
  state.next_index <- state.next_index + 1;
  Dimensions.add ledger.dimensions dimension receipt;
  Option.iter (Dimensions.add ledger.checked_dimensions dimension) prepared

let grammar_dimension_count ledger (receipt : Parser.completed_array_dimension)
    =
  protect (fun () ->
      validate_dimension_owner ledger
        receipt.dimension_preparation.dimension_owner;
      let dimension = receipt.dimension_ast in
      let span = dimension.location.span in
      (match Dimensions.find_opt ledger.dimensions dimension with
      | Some original when original == receipt -> ()
      | _ -> fail span "array count read has no original completed dimension");
      match ledger.authority with
      | Semantic_analysis -> None
      | Source_compilation _ | Task_runtime _ ->
          let prepared =
            match Dimensions.find_opt ledger.checked_dimensions dimension with
            | Some prepared
              when Sema.Compiler_record.dimension_receipt prepared == receipt ->
                prepared
            | _ ->
                fail span
                  "array count read has no checked dimension preparation"
          in
          Sema.Compiler_record.validate_dimension ~table:ledger.table ~dimension
            prepared
          |> checked span;
          Some (receipt, Sema.Compiler_record.dimension_count prepared))

let validate_global_dimensions ledger (publication : Parser.global_publication)
    =
  let dimensions = publication.global_dimensions in
  match Names.find_opt ledger.dimension_owners publication.global_name with
  | None when dimensions = [] -> ()
  | Some state
    when state.owner.dimensions_command
         == publication.global_header.declaration_command
         && Option.is_none state.pending
         && List.length dimensions = List.length state.completed_rev
         && List.for_all2
              (fun dimension receipt ->
                dimension == receipt.Parser.dimension_ast)
              dimensions
              (List.rev state.completed_rev) -> ()
  | _ ->
      fail publication.global_name.location.span
        "global publication is missing its original completed array dimensions"

let observe ledger event =
  protect (fun () ->
      match event with
      | Parser.Array_dimension_preparing preparation ->
          prepare_dimension ledger preparation
      | Parser.Array_dimension_completed receipt ->
          complete_dimension ledger receipt
      | Parser.Global_initializer_started start -> (
          let publication = start.initializer_owner in
          validate_command ledger publication.global_header;
          match (find ledger publication.global_name).source with
          | Global state
            when state.publication == publication
                 && Option.is_none state.completed
                 && Option.is_none state.initializing ->
              let pending =
                Sema.Initializer_source.begin_parser start
                |> checked start.initializer_equals.span
              in
              state.initializing <- Some pending
          | _ ->
              fail start.initializer_equals.span
                "initializer start is foreign, repeated or out of order")
      | Parser.Global_initializer_delimiter_completed delimiter -> (
          let start = delimiter.delimiter_initializer in
          let publication = start.initializer_owner in
          validate_command ledger publication.global_header;
          match (find ledger publication.global_name).source with
          | Global
              {
                publication = original;
                completed = None;
                initializing = Some pending;
              }
            when original == publication ->
              Sema.Initializer_source.observe_parser_delimiter pending delimiter
              |> checked start.initializer_equals.span
          | _ ->
              fail start.initializer_equals.span
                "initializer delimiter has no active original initializer")
      | Parser.Global_initializer_leaf_completed leaf -> (
          let start = leaf.leaf_initializer in
          let publication = start.initializer_owner in
          validate_command ledger publication.global_header;
          match (find ledger publication.global_name).source with
          | Global
              {
                publication = original;
                completed = None;
                initializing = Some pending;
              }
            when original == publication ->
              ignore
                (Sema.Initializer_source.observe_parser_leaf pending leaf
                |> checked start.initializer_equals.span)
          | _ ->
              fail start.initializer_equals.span
                "initializer leaf has no active original initializer")
      | Parser.Global_declared publication ->
          if not (Parser.global_publication_is_current publication) then
            fail publication.global_name.location.span
              "global publication is outside its original callback";
          validate_source ledger publication.global_environment
            publication.global_header publication.global_name;
          validate_global_dimensions ledger publication;
          assign ledger publication.global_name Sema.Symbol.Global_variable
            (Global { publication; completed = None; initializing = None })
            publication.global_entry;
          let family =
            source_root
              publication.global_header.declaration_command.command_context
          in
          let predecessor =
            List.find_map
              (function
                | Parser.Command_resumed receipt
                  when source_root receipt.command_start.command_context
                       == family -> Some receipt
                | _ -> None)
              ledger.source_events_rev
          in
          Names.add ledger.storage_boundaries publication.global_name
            {
              storage_source = publication;
              storage_predecessor = predecessor;
              storage_previous_global = ledger.last_storage_global;
              storage_declaration = None;
              storage_completion = None;
            };
          ledger.last_storage_global <-
            Some
              (Collection.publication_symbol
                 (find ledger publication.global_name).publication)
      | Parser.Function_declared publication ->
          validate_source ledger publication.function_environment
            publication.function_header publication.function_name;
          assign ledger publication.function_name Sema.Symbol.Function
            (Function
               { publication; defaults_rev = []; header = None; body = None })
            publication.function_entry
      | Parser.Parameter_default_completed receipt -> (
          let publication = receipt.default_function in
          let span = receipt.default_ast.location.span in
          validate_command ledger publication.function_header;
          if not (Parser.parameter_default_is_current receipt) then
            fail span "parameter default is outside its original callback";
          match (find ledger publication.function_name).source with
          | Function state
            when state.publication == publication && Option.is_none state.header
            ->
              let previous = List.nth_opt state.defaults_rev 0 in
              if
                (not (same_option ( == ) previous receipt.default_predecessor))
                || receipt.default_parameter_index < 0
                || Option.fold ~none:false
                     ~some:(fun previous ->
                       previous.Parser.default_parameter_index
                       >= receipt.default_parameter_index)
                     previous
              then
                fail span
                  "parameter default is skipped, repeated or out of order";
              state.defaults_rev <- receipt :: state.defaults_rev
          | _ ->
              fail span
                "parameter default belongs to another or completed function")
      | Parser.Global_completed (publication, completed) -> (
          validate_command ledger publication.global_header;
          let assigned = find ledger publication.global_name in
          match assigned.source with
          | Global state
            when state.publication == publication
                 && Option.is_none state.completed ->
              (match (state.initializing, completed.global_initial_value) with
              | None, None -> ()
              | Some pending, Some initial ->
                  let source =
                    Sema.Initializer_source.complete_parser pending event
                    |> checked completed.location.span
                  in
                  Names.add ledger.initializers completed.name (initial, source)
              | _ ->
                  fail completed.location.span
                    "global completion is missing its original initializer \
                     transcript");
              let boundary =
                Names.find ledger.storage_boundaries publication.global_name
              in
              Option.iter
                (fun declaration ->
                  Sema.Compiler_record.complete_declared_global declaration
                    event
                  |> checked completed.location.span)
                boundary.storage_declaration;
              boundary.storage_completion <- Some event;
              state.completed <- Some completed
          | _ ->
              fail publication.global_name.location.span
                "global completion is foreign, repeated or out of order")
      | Parser.Function_header_completed header -> (
          let publication = header.function_publication in
          validate_command ledger publication.function_header;
          let assigned = find ledger publication.function_name in
          match assigned.source with
          | Function state
            when state.publication == publication && Option.is_none state.header
            ->
              if Entries.mem ledger.entries header.completed_entry then
                fail publication.function_name.location.span
                  "completed parser entry already has a semantic owner";
              let expected =
                List.mapi
                  (fun index (parameter : Ast.function_parameter) ->
                    Option.map
                      (fun default -> (index, parameter, default))
                      parameter.default)
                  header.parameters
                |> List.filter_map Fun.id
              in
              if
                List.length expected <> List.length state.defaults_rev
                || not
                     (List.for_all2
                        (fun ( index,
                               (parameter : Ast.function_parameter),
                               default ) receipt ->
                          index = receipt.Parser.default_parameter_index
                          && default == receipt.default_ast
                          && parameter.Ast.type_specifier
                             == receipt.default_type_specifier
                          && parameter.pointer_layers
                             == receipt.default_pointer_layers
                          && parameter.register_qualifiers
                             == receipt.default_register_qualifiers
                          && same_option ( == ) parameter.name
                               receipt.default_parameter_name
                          && same_option ( == ) parameter.function_pointer
                               receipt.default_function_pointer)
                        expected
                        (List.rev state.defaults_rev))
              then
                fail publication.function_name.location.span
                  "function header is missing its exact original parameter \
                   defaults";
              state.header <- Some header;
              Entries.add ledger.entries header.completed_entry assigned
          | _ ->
              fail publication.function_name.location.span
                "function header completion is foreign, repeated or out of \
                 order")
      | Parser.Function_body_completed (header, body) -> (
          let publication = header.function_publication in
          validate_command ledger publication.function_header;
          let assigned = find ledger publication.function_name in
          match assigned.source with
          | Function state
            when state.publication == publication
                 && Option.is_none state.body
                 && Option.fold ~none:false
                      ~some:(fun saved -> saved == header)
                      state.header -> state.body <- Some body
          | _ ->
              fail publication.function_name.location.span
                "function body completion is foreign, repeated or out of order"))

let observe ledger event =
  Result.map
    (fun () ->
      record_activation_event ledger (Sema.Source_activation.Declaration event))
    (observe ledger event)

let admit_global ledger ~runtime (publication : Parser.global_publication) =
  protect (fun () ->
      let span = publication.global_name.location.span in
      if
        not
          (Sema.Source_activation.global_admission ledger.activation publication)
      then
        fail span
          "deferred storage admission is outside its original activation event";
      if
        not
          (Option.fold ~none:false ~some:(( == ) runtime)
             (ledger_runtime ledger))
      then fail span "declared storage belongs to another task runtime";
      let boundary =
        match
          Names.find_opt ledger.storage_boundaries publication.global_name
        with
        | Some boundary when boundary.storage_source == publication -> boundary
        | _ -> fail span "declared storage has no original observed publication"
      in
      let context =
        publication.global_header.declaration_command.command_context
      in
      let observed_events =
        List.fold_left
          (fun count event ->
            let candidate =
              match event with
              | Parser.Sequence_started context
              | Parser.Sequence_aborted context -> context
              | Parser.Command_started start -> start.command_context
              | Parser.Command_completed receipt
              | Parser.Command_resumed receipt ->
                  receipt.command_start.command_context
              | Parser.Sequence_completed receipt -> receipt.sequence_context
            in
            if candidate == context then count + 1 else count)
          0 ledger.source_events_rev
      in
      if
        (not (Parser.context_is_current context ~observed_events))
        || not
             (List.exists
                (fun sequence -> sequence.context == context)
                ledger.active)
      then fail span "declared storage source context is no longer live";
      let declaration =
        match boundary.storage_declaration with
        | Some declaration -> declaration
        | None ->
            let assigned = Names.find ledger.names publication.global_name in
            let declaration =
              Sema.Compiler_record.declare_global
                ~dimensions:
                  (selected_dimensions ledger publication.global_dimensions)
                ~predecessor:boundary.storage_predecessor
                ~previous_global:boundary.storage_previous_global
                ~table:ledger.table ~namespace:ledger.namespace
                assigned.publication
              |> checked span
            in
            if Option.is_none ledger.activation then
              Option.iter
                (fun event ->
                  Sema.Compiler_record.complete_declared_global declaration
                    event
                  |> checked span)
                boundary.storage_completion;
            boundary.storage_declaration <- Some declaration;
            declaration
      in
      VM.admit_declared_global runtime declaration |> checked span)

let check_global_header (name : Ast.identifier)
    (header : Parser.declaration_header) modifiers binding type_specifier =
  if
    header.modifiers != modifiers
    || (not (same_option ( == ) header.binding binding))
    || header.type_specifier != type_specifier
  then
    fail name.Ast.location.span
      "global command substituted its original declaration header"

let seal ledger (ast : Ast.module_) =
  if
    List.exists
      (fun (view, receipt) ->
        view == ast && not (Parser.sequence_accepted receipt))
      ledger.sequence_views
  then
    protect (fun () ->
        fail ast.span "parser sequence completion was not accepted")
  else
    match List.find_opt (fun command -> command.ast == ast) ledger.commands with
    | Some command -> Ok command
    | None ->
        protect (fun () ->
            let original_commands =
              match
                List.find_opt (fun (view, _) -> view == ast) ledger.views
              with
              | Some (_, entries) -> entries
              | None ->
                  fail ast.span
                    "task AST has no exact parser command or sequence receipt"
            in
            if List.exists (fun entry -> entry.sealed) original_commands then
              fail ast.span "task AST overlaps an already sealed parser command";
            if
              Option.is_none
                (Common.Source_manager.find ledger.sources ast.source)
            then fail ast.span "task command source is not registered";
            let claimed = ref [] in
            let facts = ref [] in
            let previous_ordinal = ref (-1) in
            let add item_index ?declarator_index kind (name : Ast.identifier)
                assigned =
              let header =
                match assigned.source with
                | Global state -> state.publication.global_header
                | Function state -> state.publication.function_header
              in
              if
                not
                  (List.exists
                     (fun entry ->
                       entry.receipt.command_start == header.declaration_command)
                     original_commands)
              then
                fail name.location.span
                  "declaration belongs to a different parser command";
              if
                not
                  (Common.Source_id.equal
                     (Common.Source_file.id header.declaration_source)
                     ast.source)
              then
                fail name.location.span
                  "task command has a different parser input source";
              if List.exists (fun saved -> saved == assigned) !claimed then
                fail name.location.span
                  "task command repeats an assigned declaration";
              if assigned.ordinal <= !previous_ordinal then
                fail name.location.span
                  "task command reorders original declaration publications";
              previous_ordinal := assigned.ordinal;
              let fact =
                Collection.make_declaration ~name:name.spelling
                  ~declaration_kind:kind ~origin:(origin name) ~item_index
                  ?declarator_index ()
                |> checked name.location.span
              in
              claimed := assigned :: !claimed;
              facts := (assigned.publication, fact) :: !facts
            in
            List.iteri
              (fun item_index -> function
                | Ast.Global_variable variable -> (
                    let assigned = find ledger variable.name in
                    match assigned.source with
                    | Global { publication; completed = Some completed; _ } ->
                        check_global_header variable.name
                          publication.global_header variable.modifiers
                          variable.binding variable.type_specifier;
                        if
                          completed.name != variable.name
                          || completed.pointer_layers != variable.pointer_layers
                          || completed.array_dimensions
                             != variable.array_dimensions
                          || Option.is_some completed.global_initial_value
                          || Option.is_some completed.function_pointer
                          || completed.delimiter.kind <> Ast.Semicolon
                          || completed.delimiter.location.span
                             != variable.semicolon
                        then
                          fail variable.location.span
                            "singleton global substituted its completed \
                             declarator";
                        add item_index Collection.Global_variable variable.name
                          assigned
                    | _ ->
                        fail variable.location.span
                          "global command lacks its completed declarator")
                | Ast.Global_declaration declaration ->
                    List.iteri
                      (fun declarator_index (declarator : Ast.global_declarator)
                         ->
                        let assigned = find ledger declarator.name in
                        match assigned.source with
                        | Global { publication; completed = Some completed; _ }
                          when completed == declarator ->
                            check_global_header declarator.name
                              publication.global_header declaration.modifiers
                              declaration.binding declaration.type_specifier;
                            add item_index ~declarator_index
                              Collection.Global_variable declarator.name
                              assigned
                        | _ ->
                            fail declarator.location.span
                              "global command substituted or lacks its \
                               completed declarator")
                      declaration.declarators
                | Ast.Function_prototype prototype -> (
                    let assigned = find ledger prototype.name in
                    match assigned.source with
                    | Function
                        { publication; header = Some header; body = None; _ } ->
                        if
                          (not
                             (same_option ( == )
                                publication.function_header.binding
                                (Some prototype.binding)))
                          || publication.function_header.modifiers
                             != prototype.modifiers
                          || publication.function_header.type_specifier
                             != prototype.return_type
                          || publication.function_pointer_layers
                             != prototype.return_pointer_layers
                          || publication.function_opening_parenthesis
                             != prototype.opening_parenthesis
                          || header.parameters != prototype.parameters
                          || header.empty_parameter_entries
                             != prototype.empty_parameter_entries
                          || (not
                                (same_option ( == ) header.variadic
                                   prototype.variadic))
                          || header.closing_parenthesis
                             != prototype.closing_parenthesis
                        then
                          fail prototype.location.span
                            "prototype command substituted its completed header";
                        add item_index Collection.Function_prototype
                          prototype.name assigned
                    | _ ->
                        fail prototype.location.span
                          "prototype command lacks its completed header")
                | Ast.Function_definition definition -> (
                    let assigned = find ledger definition.name in
                    match assigned.source with
                    | Function { body = Some body; _ } when body == definition
                      ->
                        add item_index Collection.Function_definition
                          definition.name assigned
                    | _ ->
                        fail definition.location.span
                          "function command substituted or lacks its completed \
                           body")
                | Ast.Top_level_statement _ -> ()
                | Ast.Aggregate_forward_declaration _
                | Ast.Aggregate_definition _ ->
                    fail ~code:"HCRUN0001" ast.span
                      "declaration is outside integer program execution")
              ast.items;
            let declarations =
              Collection.view ledger.namespace (List.rev !facts)
              |> checked ast.span
            in
            let references = Names.create 32 in
            Names.iter
              (fun identifier reference ->
                if
                  List.exists
                    (fun entry ->
                      entry.receipt.command_start
                      == Parser.selected_command reference.selection)
                    original_commands
                then Names.add references identifier reference)
              ledger.references;
            let queries = Query_expressions.create 16 in
            Query_expressions.iter
              (fun expression query ->
                if
                  List.exists
                    (fun entry ->
                      entry.receipt.command_start
                      == query.query_receipt.query_root.query_command)
                    original_commands
                then Query_expressions.add queries expression query)
              ledger.queries;
            let dimensions = Dimensions.create 16 in
            Dimensions.iter
              (fun dimension receipt ->
                if
                  List.exists
                    (fun entry ->
                      entry.receipt.command_start
                      == receipt.Parser.dimension_preparation.dimension_owner
                           .dimensions_command)
                    original_commands
                then Dimensions.add dimensions dimension receipt)
              ledger.dimensions;
            let command =
              {
                table = ledger.table;
                runtime = ledger_runtime ledger;
                ast;
                source_order =
                  Option.map
                    (fun runtime ->
                      let order = VM.task_source_order runtime in
                      (match
                         List.find_opt
                           (fun (view, _) -> view == ast)
                           ledger.sequence_views
                       with
                        | Some (_, receipt) ->
                            Sema.Task_command_order.seal_sequence order receipt
                        | None -> (
                            match original_commands with
                            | [ entry ] ->
                                Sema.Task_command_order.seal_command order
                                  entry.receipt
                            | _ ->
                                Error
                                  "task command lacks its original \
                                   source-order view"))
                      |> checked ast.span)
                    (ledger_runtime ledger);
                declarations;
                references;
                queries;
                dimensions;
                initializers =
                  (let initializers = Names.create 16 in
                   List.iter
                     (fun assigned ->
                       match assigned.source with
                       | Global state ->
                           let name = state.publication.global_name in
                           Option.iter
                             (Names.add initializers name)
                             (Names.find_opt ledger.initializers name)
                       | Function _ -> ())
                     !claimed;
                   initializers);
                checked_dimensions =
                  Dimensions.fold
                    (fun dimension _ checked ->
                      Option.iter
                        (Dimensions.add checked dimension)
                        (Dimensions.find_opt ledger.checked_dimensions dimension);
                      checked)
                    dimensions (Dimensions.create 16);
              }
            in
            List.iter (fun entry -> entry.sealed <- true) original_commands;
            List.iter (fun assigned -> assigned.claimed <- true) !claimed;
            ledger.commands <- command :: ledger.commands;
            command)

let owns_runtime runtime (command : command) =
  Option.fold ~none:false ~some:(fun owner -> owner == runtime) command.runtime

let initializer_leaf_for ledger (receipt : Parser.completed_initializer_leaf) =
  protect (fun () ->
      let publication = receipt.leaf_initializer.initializer_owner in
      let span = publication.global_name.location.span in
      match Names.find_opt ledger.names publication.global_name with
      | Some
          {
            source =
              Global { publication = original; initializing = Some pending; _ };
            _;
          }
        when original == publication ->
          Sema.Initializer_source.parser_leaf pending receipt |> checked span
      | _ -> fail span "initializer leaf belongs to another source declaration")

let initializer_declaration ledger (start : Parser.global_initializer_start) =
  protect (fun () ->
      let publication = start.initializer_owner in
      let span = start.initializer_equals.span in
      let deferred =
        Sema.Source_activation.initializer_start ledger.activation start
      in
      if not (Parser.initializer_start_is_current start || deferred) then
        fail span "initializer layout is outside its original start callback";
      if not deferred then validate_command ledger publication.global_header;
      (match (find ledger publication.global_name).source with
      | Global { publication = original; completed; initializing = Some _ }
        when original == publication && (Option.is_none completed || deferred)
        -> ()
      | _ -> fail span "initializer layout has no observed original start");
      let declaration =
        match
          Names.find_opt ledger.storage_boundaries publication.global_name
        with
        | Some { storage_source; storage_declaration = Some declaration; _ }
          when storage_source == publication -> declaration
        | _ ->
            fail span
              "initializer layout has no checked original storage declaration"
      in
      let runtime =
        match ledger_runtime ledger with
        | Some runtime -> runtime
        | None -> fail span "initializer layout has no retained task storage"
      in
      (match
         VM.admitted_publication_for_symbol runtime
           (Sema.Compiler_record.declared_global_symbol declaration)
       with
      | Some (VM.Admitted_declared_global (_, slot))
        when Ir.Integer_globals.declared_record slot == declaration -> ()
      | _ -> fail span "initializer layout storage has not been admitted");
      declaration)

let selected_fragment_transcript ledger ~task_view ~span expression =
  let module Selection = Sema.Reference_selection in
  let module Globals = Ir.Integer_globals in
  let table = ledger.table in
  let environment = Globals.task_environment task_view in
  let retained name publication =
    let binding =
      match publication with
      | VM.Admitted_global (reference, _)
      | VM.Admitted_declared_global (reference, _) ->
          Globals.task_global_binding task_view reference
      | VM.Admitted_function reference ->
          Globals.task_function_binding task_view reference
    in
    match binding with
    | Some binding ->
        Selection.outer ~table ~name ~environment ~binding |> checked span
    | None ->
        fail span "initializer reference is absent from its exact task snapshot"
  in
  let references =
    List.map
      (fun (identifier : Ast.identifier) ->
        let name = identifier.spelling in
        let selection =
          match Names.find_opt ledger.references identifier with
          | None ->
              fail span
                "initializer reference has no original source observation"
          | Some { target; _ } -> (
              match target with
              | Selected_absent -> Selection.absent ~table ~name |> checked span
              | Selected_unbound _ | Selected_source { admitted = None; _ } ->
                  Selection.unavailable ~table ~name |> checked span
              | Selected_local ->
                  fail span "global initializer selected a local reference"
              | Selected_runtime publication
              | Selected_source { admitted = Some publication; _ } ->
                  retained name publication)
        in
        (identifier, selection))
      (Sema.Initializer_source.expression_identifier_nodes expression)
  in
  let queries =
    Sema.Query_selection.source_queries expression
    |> List.map (fun expression ->
        match Query_expressions.find_opt ledger.queries expression with
        | Some query -> query.query_selection
        | None ->
            fail span "initializer query has no original source observation")
  in
  (environment, references, queries)

let initializer_fragment ledger ~runtime ~task_view
    (receipt : Parser.completed_initializer_leaf) =
  let ( let* ) = Result.bind in
  let* leaf = initializer_leaf_for ledger receipt in
  protect (fun () ->
      let module Fragment = Sema.Initializer_fragment in
      let module Globals = Ir.Integer_globals in
      let publication = receipt.leaf_initializer.initializer_owner in
      let span = publication.global_name.location.span in
      if
        not
          (Parser.initializer_leaf_is_current receipt
          || Sema.Source_activation.initializer_leaf ledger.activation receipt)
      then
        fail span "initializer fragment is outside its original leaf callback";
      if
        (not
           (Option.fold ~none:false ~some:(( == ) runtime)
              (ledger_runtime ledger)))
        || not (VM.task_owns_snapshot runtime task_view)
      then fail span "initializer fragment has another task runtime or snapshot";
      let declaration =
        match
          Names.find_opt ledger.storage_boundaries publication.global_name
        with
        | Some { storage_source; storage_declaration = Some declaration; _ }
          when storage_source == publication -> declaration
        | _ ->
            fail span
              "initializer fragment has no checked original storage declaration"
      in
      (match
         VM.admitted_publication_for_symbol runtime
           (Sema.Compiler_record.declared_global_symbol declaration)
       with
      | Some (VM.Admitted_declared_global (reference, slot))
        when Globals.declared_record slot == declaration
             && Option.is_some (Globals.task_global_binding task_view reference)
        -> ()
      | _ ->
          fail span
            "initializer fragment storage is absent from its exact task \
             snapshot");
      let table = ledger.table in
      let environment, references, queries =
        selected_fragment_transcript ledger ~task_view ~span
          (Sema.Initializer_source.leaf_expression_ast leaf)
      in
      Fragment.create ~table ~declaration ~leaf ~environment ~references
        ~queries
      |> checked span)

let initializer_fragment_authority ledger ~runtime ~task_view receipt =
  let ( let* ) = Result.bind in
  let* fragment = initializer_fragment ledger ~runtime ~task_view receipt in
  protect (fun () ->
      Sema.Initializer_fragment.authorize ?activation:ledger.activation
        ~namespace:ledger.namespace fragment
      |> checked receipt.Parser.leaf_initializer.initializer_equals.span)

let require_initializer_runtime ledger runtime span =
  if
    not (Option.fold ~none:false ~some:(( == ) runtime) (ledger_runtime ledger))
  then fail span "initializer operation belongs to another task runtime"

let complete_defaults_runtime ledger ~runtime header =
  protect (fun () ->
      let span =
        header.Parser.function_publication.function_name.location.span
      in
      require_initializer_runtime ledger runtime span;
      let assigned = find ledger header.function_publication.function_name in
      (match assigned.source with
      | Function state
        when Option.fold ~none:false ~some:(( == ) header) state.header -> ()
      | _ -> fail span "default completion lacks its original observed header");
      VM.complete_task_defaults runtime ~namespace:ledger.namespace header
      |> checked span)

let begin_default_attempt ledger ~runtime receipt =
  protect (fun () ->
      let span = receipt.Parser.default_ast.location.span in
      require_initializer_runtime ledger runtime span;
      let assigned = find ledger receipt.default_function.function_name in
      (match assigned.source with
      | Function state
        when state.publication == receipt.default_function
             && (Option.is_none state.header
                 && Option.fold ~none:false ~some:(( == ) receipt)
                      (List.nth_opt state.defaults_rev 0)
                || Sema.Source_activation.parameter_default ledger.activation
                     receipt
                   && List.exists (( == ) receipt) state.defaults_rev) -> ()
      | _ ->
          fail span
            "default execution lacks its original observed function boundary");
      VM.begin_task_default runtime ~namespace:ledger.namespace
        ~publication:assigned.publication receipt
      |> checked span)

let default_fragment_authority ledger ~runtime ~task_view receipt =
  protect (fun () ->
      let span = receipt.Parser.default_ast.location.span in
      require_initializer_runtime ledger runtime span;
      if
        (not (VM.task_owns_snapshot runtime task_view))
        || not
             (Parser.parameter_default_is_current receipt
             || Sema.Source_activation.parameter_default ledger.activation
                  receipt)
      then fail span "default fragment has another task snapshot or callback";
      let assigned = find ledger receipt.default_function.function_name in
      (match assigned.source with
      | Function state
        when state.publication == receipt.default_function
             && (Option.is_none state.header
                 && Option.fold ~none:false ~some:(( == ) receipt)
                      (List.nth_opt state.defaults_rev 0)
                || Sema.Source_activation.parameter_default ledger.activation
                     receipt
                   && List.exists (( == ) receipt) state.defaults_rev) -> ()
      | _ ->
          fail span
            "default fragment lacks its original observed function boundary");
      let expression =
        match receipt.default_ast.value with
        | Ast.Expression_default expression -> expression
        | Ast.Lastclass_default _ ->
            fail span "lastclass is not a declaration-time expression"
      in
      let environment, references, queries =
        selected_fragment_transcript ledger ~task_view ~span expression
      in
      let fragment =
        Sema.Default_fragment.create ~table:ledger.table
          ~publication:assigned.publication ~receipt ~environment ~references
          ~queries
        |> checked span
      in
      Sema.Default_fragment.authorize ?activation:ledger.activation
        ~namespace:ledger.namespace fragment
      |> checked span)

let begin_initializer_runtime ledger ~runtime start =
  let ( let* ) = Result.bind in
  let* declaration = initializer_declaration ledger start in
  protect (fun () ->
      let span = start.Parser.initializer_equals.span in
      require_initializer_runtime ledger runtime span;
      VM.begin_task_initializer runtime ~namespace:ledger.namespace declaration
        start
      |> checked span)

let observe_initializer_delimiter ledger ~runtime receipt =
  protect (fun () ->
      let span = receipt.Parser.delimiter_initializer.initializer_equals.span in
      require_initializer_runtime ledger runtime span;
      VM.observe_task_initializer_delimiter runtime ~namespace:ledger.namespace
        receipt
      |> checked span)

let begin_initializer_attempt ledger ~runtime receipt =
  let ( let* ) = Result.bind in
  let* leaf = initializer_leaf_for ledger receipt in
  protect (fun () ->
      let span = receipt.Parser.leaf_initializer.initializer_equals.span in
      require_initializer_runtime ledger runtime span;
      VM.begin_task_initializer_leaf runtime ~namespace:ledger.namespace leaf
      |> checked span)

let complete_initializer_runtime ledger ~runtime event =
  protect (fun () ->
      match event with
      | Parser.Global_completed (publication, completed) ->
          let span = completed.Ast.location.span in
          require_initializer_runtime ledger runtime span;
          let boundary =
            Names.find ledger.storage_boundaries publication.global_name
          in
          if
            not
              (Option.fold ~none:false ~some:(( == ) event)
                 boundary.storage_completion)
          then
            fail span "initializer completion has no original observed boundary";
          let _, source =
            match Names.find_opt ledger.initializers completed.name with
            | Some pair -> pair
            | None ->
                fail span
                  "initializer completion has no original source transcript"
          in
          let start =
            match Sema.Initializer_source.leaves source with
            | leaf :: _ ->
                (Option.get (Sema.Initializer_source.leaf_parser_receipt leaf))
                  .leaf_initializer
            | [] ->
                fail span "initializer completion has no reached source leaf"
          in
          VM.complete_task_initializer runtime ~namespace:ledger.namespace start
            source
          |> checked span
      | _ ->
          invalid_arg
            "initializer completion requires its original global boundary")

let activate_source ledger ~runtime ~span ~declaration ~command =
  let ( let* ) = Result.bind in
  let* activation =
    protect (fun () ->
        let context =
          match (ledger.active, ledger.sequences) with
          | [ active ], [ sequence ] when active == sequence -> active.context
          | _ -> fail span "source activation requires its original root"
        in
        let span = context_span context in
        require_initializer_runtime ledger runtime span;
        if ledger.commands <> [] then
          fail span
            "source activation has already started or source commands were \
             sealed";
        match ledger.activation with
        | Some activation -> activation
        | None ->
            let activation =
              Sema.Source_activation.create ~namespace:ledger.namespace ~context
                ~observed_events:(List.length ledger.source_events_rev)
                (List.rev ledger.activation_events_rev)
              |> checked span
            in
            VM.bind_source_activation runtime ~namespace:ledger.namespace
              activation
            |> checked span;
            ledger.activation <- Some activation;
            activation)
  in
  let invalid =
    [
      Common.Diagnostic.make ~primary:span ~severity:Common.Diagnostic.Error
        ~code:"HCRUN0004"
        ~message:
          "source activation is no longer current or was already consumed"
        ();
    ]
  in
  Sema.Source_activation.run activation ~invalid (function
    | Sema.Source_activation.Reference selection ->
        protect (fun () ->
            let identifier = Parser.selected_identifier selection in
            if
              not (Sema.Source_activation.reference ledger.activation selection)
            then
              fail identifier.location.span
                "source read is outside its activation event";
            let original =
              match Names.find_opt ledger.references identifier with
              | Some original when original.selection == selection -> original
              | _ ->
                  fail identifier.location.span
                    "source activation lacks its original read"
            in
            let target =
              match original.target with
              | Selected_source selected ->
                  let admitted =
                    match selected.stage with
                    | Provisional_function_selection _ -> None
                    | _ ->
                        VM.admitted_publication_for_symbol runtime
                          (Collection.publication_symbol selected.publication)
                  in
                  Selected_source { selected with admitted }
              | target -> target
            in
            Names.replace ledger.references identifier { original with target };
            validate_execution_target selection target)
    | Sema.Source_activation.Declaration event ->
        let* () =
          protect (fun () ->
              match event with
              | Parser.Array_dimension_preparing preparation
                when ledger.source_dimensions_rev <> [] -> (
                  let before = VM.task_initializer_steps runtime in
                  let result = VM.charge_source_dimension runtime preparation in
                  ledger.dimension_work <-
                    ledger.dimension_work
                    + VM.task_initializer_steps runtime
                    - before;
                  match result with
                  | Ok () -> ()
                  | Error message ->
                      if String.starts_with ~prefix:"HCIRVM0007:" message then
                        fail ~code:"HCIRVM0007"
                          preparation.dimension_opening.span
                          "the bounded array dimension preparation work limit \
                           was exhausted"
                      else fail preparation.dimension_opening.span message)
              | Parser.Global_completed (publication, completed) ->
                  let boundary =
                    Names.find ledger.storage_boundaries publication.global_name
                  in
                  Option.iter
                    (fun record ->
                      Sema.Compiler_record.complete_declared_global record event
                      |> checked completed.location.span)
                    boundary.storage_declaration
              | _ -> ())
        in
        declaration event
    | Sema.Source_activation.Command (Parser.Command_resumed receipt) ->
        command receipt.command_ast
    | Sema.Source_activation.Command _ -> Ok ())

let initializer_scope ledger = Collection.namespace_scope ledger.namespace

let initializer_for ~table ~ast (command : command) name initial =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span "initializer source belongs to another table or AST";
      match Names.find_opt command.initializers name with
      | Some (original, source) when original == initial -> source
      | _ ->
          fail ast.span
            "initializer source lacks its original completed transcript")

let source_initializer_for ~table ~ast (Source_command command) name initial =
  initializer_for ~table ~ast command name initial

let command_order ~runtime ~table ~ast (command : command) =
  protect (fun () ->
      if
        (not (owns_runtime runtime command))
        || command.table != table || command.ast != ast
      then
        fail ast.Ast.span
          "source order belongs to another runtime, table or AST";
      match command.source_order with
      | Some order -> order
      | None -> fail ast.span "task command has no original source order")

let collection ~table ~ast (command : command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "task declaration seal belongs to another table or source AST";
      command.declarations)

let reference_for ~table ~ast (command : command) (identifier : Ast.identifier)
    =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "task reference seal belongs to another table or source AST";
      match Names.find_opt command.references identifier with
      | Some reference -> reference.target
      | None ->
          fail identifier.location.span
            "identifier has no parser selection in this task command")

let query_receipt query = query.query_receipt
let query_selection query = query.query_selection
let query_target query = query.query_target
let query_presence query = query.query_receipt.query_root.query_present

let query_for ~table ~ast (command : command) expression =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "task query seal belongs to another table or source AST";
      match Query_expressions.find_opt command.queries expression with
      | Some query -> query
      | None ->
          fail (Ast.expression_location expression).span
            "query has no complete parser receipt in this task command")

let dimension_for ~table ~ast (command : command)
    (dimension : Ast.array_dimension) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "task dimension seal belongs to another table or source AST";
      match Dimensions.find_opt command.dimensions dimension with
      | Some receipt -> receipt
      | None ->
          fail dimension.location.span
            "array dimension has no complete parser receipt in this task \
             command")

let checked_dimension_for ~table ~ast (command : command)
    (dimension : Ast.array_dimension) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "checked dimension seal belongs to another table or source AST";
      match Dimensions.find_opt command.checked_dimensions dimension with
      | Some checked -> checked
      | None ->
          fail dimension.location.span
            "array dimension has no checked preparation in this command")

let command_dimension_work (command : command) =
  Dimensions.fold
    (fun _ dimension count ->
      count + Sema.Compiler_record.dimension_work dimension)
    command.checked_dimensions 0

let seal_source ledger ast =
  match ledger.authority with
  | Source_compilation _ ->
      Result.map (fun command -> Source_command command) (seal ledger ast)
  | Semantic_analysis | Task_runtime _ ->
      protect (fun () ->
          fail ast.Ast.span
            "ordinary source seal requires its original source-compilation \
             ledger")

let source_collection ~table ~ast (Source_command command) =
  collection ~table ~ast command

let source_query_for ~table ~ast (Source_command command) expression =
  query_for ~table ~ast command expression

let source_dimension_for ~table ~ast (Source_command command) dimension =
  dimension_for ~table ~ast command dimension

let source_checked_dimension_for ~table ~ast (Source_command command) dimension
    =
  checked_dimension_for ~table ~ast command dimension

let source_dimension_work (Source_command command) =
  command_dimension_work command

let reference_resolver ~table ~ast ~task_view command =
  let module Selection = Sema.Reference_selection in
  let module Globals = Ir.Integer_globals in
  let ( let* ) = Result.bind in
  let* declarations = collection ~table ~ast command in
  let environment = Globals.task_environment task_view in
  if
    not
      (Option.fold ~none:false
         ~some:(fun runtime -> VM.task_owns_snapshot runtime task_view)
         command.runtime)
  then
    protect (fun () ->
        fail ast.Ast.span
          "parser command has no authority for this runtime snapshot")
  else if not (Sema.Outer_environment.owns_table environment table) then
    protect (fun () ->
        fail ast.Ast.span "task reference view has a foreign semantic table")
  else
    let cache = Names.create 32 in
    let retained name publication =
      let binding =
        match publication with
        | VM.Admitted_global (reference, _)
        | VM.Admitted_declared_global (reference, _) ->
            Globals.task_global_binding task_view reference
        | VM.Admitted_function reference ->
            Globals.task_function_binding task_view reference
      in
      match binding with
      | Some binding -> Selection.outer ~table ~name ~environment ~binding
      | None ->
          Error
            "selected runtime publication is absent from this exact task \
             snapshot"
    in
    let resolve (identifier : Ast.identifier) =
      let* target =
        reference_for ~table ~ast command identifier
        |> Result.map_error (fun diagnostics ->
            diagnostics
            |> List.map (fun diagnostic ->
                diagnostic.Common.Diagnostic.code ^ ": " ^ diagnostic.message)
            |> String.concat "; ")
      in
      let name = identifier.spelling in
      match target with
      | Selected_absent -> Selection.absent ~table ~name
      | Selected_unbound _ -> Selection.unavailable ~table ~name
      | Selected_local -> Selection.local ~table ~name
      | Selected_runtime publication -> retained name publication
      | Selected_source { publication; stage; admitted } -> (
          let symbol = Collection.publication_symbol publication in
          if
            List.exists
              (fun entry -> Collection.entry_symbol entry == symbol)
              (Collection.entries declarations)
          then
            let stage =
              match stage with
              | Global_selection (_, None) -> Selection.Global_declared
              | Global_selection (_, Some _) -> Selection.Global_completed
              | Provisional_function_selection _ -> Selection.Function_declared
              | Function_selection (_, None) ->
                  Selection.Function_header_completed
              | Function_selection (_, Some _) ->
                  Selection.Function_body_completed
            in
            Selection.source ~table ~name ~symbol ~stage
          else
            match admitted with
            | Some publication -> retained name publication
            | None -> Selection.unavailable ~table ~name)
    in
    Ok
      (fun identifier ->
        match Names.find_opt cache identifier with
        | Some result -> result
        | None ->
            let result = resolve identifier in
            Names.add cache identifier result;
            result)
