module Ast = Frontend.Ast
module Parser = Frontend.Parser
module Visibility = Frontend.Symbol_visibility
module Collection = Sema.Declaration_collection
module VM = Ir.Integer_interpreter
module Switch = Sema.Integer_switch_preparation

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
  let hash root = Hashtbl.hash root.Parser.query_location
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

module Type_specifiers = Hashtbl.Make (struct
  type t = Ast.type_specifier

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

type dimension_evaluation =
  | Observed_dimension
  | Failed_dimension
  | Deferred_runtime_dimension
  | Awaiting_runtime_dimension
  | Executing_dimension of VM.dimension_attempt * int
  | Proposed_runtime_dimension of
      Sema.Compiler_record.runtime_dimension_proposal
      * Sema.Compiler_record.query_read list
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
  | Aggregate of {
      publication : Parser.aggregate_publication;
      mutable progress : Sema.Compiler_record.aggregate_progress option;
      mutable completed : Parser.completed_aggregate option;
      mutable record : (Sema.Compiler_record.t, string) result option;
    }
  | Global of {
      publication : Parser.global_publication;
      mutable completed : Ast.global_declarator option;
      mutable initializing : Sema.Initializer_source.pending option;
    }
  | Function of {
      publication : Parser.function_publication;
      mutable provisional_source : Sema.Provisional_function.t option;
      mutable native_record : Sema.Function_record_phase.t option;
      mutable runtime_phase :
        Sema.Function_record_classification.classified_declaration option;
      mutable defaults_rev : Parser.completed_parameter_default list;
      mutable header : Parser.completed_function_header option;
      mutable declared_header : Sema.Compiler_record.declared_function option;
      mutable typed_header :
        (Sema.Function_collection.collected_function
        * Sema.Function_type_resolution.resolved_function)
        option;
      mutable body : Ast.function_definition option;
      mutable body_compiler_options : int64 option;
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
  native_record : Sema.Function_record_phase.t option;
}

type selected_call = {
  start : Parser.call_start;
  capture : Sema.Function_record_phase.call_start_snapshot option;
  mutable emission_capture :
    Sema.Function_record_phase.call_emission_snapshot option;
  arguments : Sema.Function_record_phase.snapshot option;
  mutable runtime_start : VM.task_call_start option;
  mutable runtime_phase : Sema.Function_call_phase.t option;
  mutable emission :
    (Parser.completed_call * Sema.Function_record_phase.snapshot option) option;
}

type selected_implicit_output = {
  implicit_native : Sema.Function_record_phase.t option;
  mutable arguments_capture :
    Sema.Function_record_phase.implicit_arguments_snapshot option;
  mutable emitted_capture :
    Sema.Function_record_phase.implicit_emission_snapshot option;
  mutable implicit_pending : VM.task_implicit_call_start option;
  mutable implicit_phase : Sema.Function_call_phase.t option;
  mutable arguments_observed : bool;
  mutable emission_observed : bool;
  implicit_selection : Parser.implicit_output_selection;
  implicit_target : reference_target;
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
  calls : Sema.Function_call_phase.t list;
  function_compiler_options : (Sema.Symbol.t * int64) list;
  namespace : Collection.namespace;
  inherited_metadata : Sema.Compiler_record.inherited_metadata list;
  selected_aggregate_types :
    Sema.Source_type_reference.selected_aggregate Type_specifiers.t;
  function_headers :
    (Sema.Compiler_record.declared_function
    * Sema.Function_collection.collected_function
    * Sema.Function_type_resolution.resolved_function)
    list;
  static_allocations : Sema.Compiler_record.static_allocation list;
  implicit_outputs : selected_implicit_output list;
  source_callback_defaults : Ir.Prepared_callback_default.t list;
  native_source_callback_defaults : Ir.Prepared_callback_default.t list;
  source_defaults : Ir.Prepared_parameter_default.t list;
  native_source_defaults : Ir.Prepared_parameter_default.t list;
  table : Sema.Symbol_table.t;
  runtime : VM.task_state option;
  ast : Ast.module_;
  declarations : Collection.t;
  references : selected_reference Names.t;
  queries : query Query_expressions.t;
  dimensions : Parser.completed_array_dimension Dimensions.t;
  checked_dimensions : Sema.Compiler_record.declared_dimension Dimensions.t;
  switches : Switch.t list;
  offsets : Sema.Compiler_record.aggregate_offset list;
  initializers : (Ast.global_initializer * Sema.Initializer_source.t) Names.t;
  source_order : Sema.Task_command_order.command option;
}

type source_command = Source_command of command

type authority =
  | Semantic_analysis
  | Source_compilation of Common.Source_file.t
  | Task_runtime of VM.task_state

type source_default_owner = Output_aot_default | Native_source_default

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

type callback_state = {
  callback_publication : Parser.callback_signature_publication;
  mutable callback_pending : Parser.callback_parameter_publication option;
  mutable callback_members_rev : Parser.completed_callback_parameter list;
  mutable callback_defaults_rev : Parser.completed_callback_default list;
  mutable callback_header : Parser.completed_callback_signature option;
}

type t = {
  mutable callback_states : callback_state list;
  mutable source_callback_attempts :
    (Parser.completed_callback_default
    * Sema.Default_fragment.authority
    * source_default_owner
    * int
    * int64 option ref)
    list;
  mutable prepared_source_callback_defaults :
    Ir.Prepared_callback_default.t list;
  compiler_positions : Sema.Compiler_record.compiler_positions;
  call_journal : Sema.Source_activation.call_journal;
  mutable calls : selected_call list;
  mutable native_functions : Sema.Function_record_phase.registry option;
  mutable native_function_events :
    (Parser.declaration_event * Sema.Function_record_phase.snapshot) list;
  mutable implicit_outputs : selected_implicit_output list;
  mutable source_default_attempts :
    (Parser.completed_parameter_default
    * Sema.Default_fragment.authority
    * source_default_owner
    * int
    * int64 option ref)
    list;
  mutable source_defaults_runtime : VM.task_state option;
  mutable prepared_source_defaults : Ir.Prepared_parameter_default.t list;
  mutable native_static_attempts : Parser.static_initializer_preparation list;
  mutable internal_bindings : Parser.internal_binding_preparation list;
  mutable prepared_internal_bindings : Sema.Prepared_internal_binding.t list;
  mutable static_preparations : Parser.static_initializer_preparation list;
  mutable static_completions : Parser.completed_static_initializer list;
  mutable static_allocations_rev : Sema.Compiler_record.static_allocation list;
  mutable native_initializer_attempts : Sema.Initializer_source.leaf list;
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
  next_ordinal : int ref;
  mutable sequences : command_sequence list;
  mutable active : command_sequence list;
  saved_parent : Parser.command_context option;
  mutable views : (Ast.module_ * parsed_command list) list;
  mutable sequence_views : (Ast.module_ * Parser.completed_sequence) list;
  mutable authority : authority;
  mutable source_events_rev : Parser.command_event list;
  mutable activation_events_rev : Sema.Source_activation.event list;
  mutable pending_runtime_offset : Parser.aggregate_phase option;
  mutable activation : Sema.Source_activation.t option;
  switch_budget : Switch.budget;
  switch_tracker : Switch.tracker;
  max_dimension_work : int;
  max_offset_work : int;
  mutable dimension_work : int;
  mutable offset_work : int;
  mutable offsets_rev : Sema.Compiler_record.aggregate_offset list;
  mutable runtime_offset_completions :
    (Parser.aggregate_phase * int * bool ref) list;
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
  selected_aggregate_types :
    Sema.Source_type_reference.selected_aggregate Type_specifiers.t;
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

let create_with_authority ?enclosing_ledger ?saved_parent ?compiler_positions
    ?max_switch_work ?switch_budget ?(max_dimension_work = 100_000)
    ?(max_offset_work = 100_000) authority session =
  let runtime =
    match authority with
    | Task_runtime runtime -> Some runtime
    | _ -> None
  in
  let table = Session.semantic_symbols session in
  let compiler_positions =
    match compiler_positions with
    | Some positions -> positions
    | None ->
        Sema.Compiler_record.create_compiler_positions
          ~sources:(Session.sources session)
  in
  let switch_budget =
    match (switch_budget, max_switch_work) with
    | Some budget, None -> Ok budget
    | Some budget, Some limit when Switch.budget_limit budget = limit ->
        Ok budget
    | Some _, Some _ ->
        Error "shared switch preparation budget has a different max_switch_work"
    | None, limit ->
        Switch.create_budget ~max_work:(Option.value ~default:100_000 limit)
  in
  if
    (not
       (Sema.Compiler_record.compiler_positions_own_sources compiler_positions
          (Session.sources session)))
    || Option.fold ~none:false
         ~some:(fun task -> not (VM.task_owns_table task table))
         runtime
  then Error "task declaration runtime belongs to another semantic table"
  else
    Result.bind switch_budget (fun switch_budget ->
        let module_name =
          match authority with
          | Source_compilation source ->
              Some (Common.Source_file.display_path source)
          | _ -> None
        in
        (match enclosing_ledger with
          | None -> Collection.create_namespace ~table ?module_name ()
          | Some enclosing -> Ok enclosing.namespace)
        |> fun result ->
        Result.bind result (fun namespace ->
            let binding =
              match authority with
              | Task_runtime runtime -> VM.bind_task_namespace runtime namespace
              | _ -> Ok ()
            in
            Result.map
              (fun () ->
                {
                  compiler_positions;
                  call_journal =
                    Sema.Source_activation.create_call_journal ~namespace ();
                  calls = [];
                  native_functions = None;
                  native_function_events = [];
                  callback_states = [];
                  source_callback_attempts = [];
                  prepared_source_callback_defaults = [];
                  source_default_attempts = [];
                  source_defaults_runtime = None;
                  prepared_source_defaults = [];
                  native_initializer_attempts = [];
                  native_static_attempts = [];
                  internal_bindings = [];
                  prepared_internal_bindings = [];
                  static_preparations = [];
                  static_completions = [];
                  static_allocations_rev = [];
                  storage_boundaries = Names.create 16;
                  last_storage_global = None;
                  session;
                  table;
                  namespace;
                  sources = Session.sources session;
                  symbols = Session.symbols session;
                  names =
                    Option.fold ~none:(Names.create 32)
                      ~some:(fun enclosing -> enclosing.names)
                      enclosing_ledger;
                  entries =
                    Option.fold ~none:(Entries.create 32)
                      ~some:(fun enclosing -> enclosing.entries)
                      enclosing_ledger;
                  commands = [];
                  next_ordinal =
                    Option.fold ~none:(ref 0)
                      ~some:(fun enclosing -> enclosing.next_ordinal)
                      enclosing_ledger;
                  sequences = [];
                  active = [];
                  saved_parent;
                  views = [];
                  sequence_views = [];
                  authority;
                  source_events_rev = [];
                  activation_events_rev = [];
                  pending_runtime_offset = None;
                  activation = None;
                  switch_budget;
                  switch_tracker = Switch.create_tracker ~budget:switch_budget;
                  max_dimension_work;
                  max_offset_work;
                  dimension_work = 0;
                  offset_work = 0;
                  offsets_rev = [];
                  runtime_offset_completions = [];
                  source_dimensions_rev = [];
                  runtime_entries = Entries.create 32;
                  runtime_records = Entries.create 32;
                  admissions = [];
                  references = Names.create 32;
                  implicit_outputs = [];
                  query_roots = Query_roots.create 16;
                  queries = Query_expressions.create 16;
                  dimension_owners = Names.create 16;
                  dimensions =
                    Option.fold ~none:(Dimensions.create 16)
                      ~some:(fun enclosing -> enclosing.dimensions)
                      enclosing_ledger;
                  checked_dimensions =
                    Option.fold ~none:(Dimensions.create 16)
                      ~some:(fun enclosing -> enclosing.checked_dimensions)
                      enclosing_ledger;
                  selected_aggregate_types =
                    Option.fold
                      ~none:(Type_specifiers.create 16)
                      ~some:(fun enclosing ->
                        enclosing.selected_aggregate_types)
                      enclosing_ledger;
                  initializers = Names.create 16;
                })
              binding))

let create ?compiler_positions ?max_switch_work ?switch_budget ?runtime session
    =
  create_with_authority ?compiler_positions ?max_switch_work ?switch_budget
    (match runtime with
    | None -> Semantic_analysis
    | Some runtime -> Task_runtime runtime)
    session

let create_source ?compiler_positions ?max_switch_work ?switch_budget
    ?(max_dimension_work = 100_000) ?(max_offset_work = 100_000) session ~source
    =
  if max_dimension_work <= 0 || max_offset_work <= 0 then
    Error "source preparation limit must be positive"
  else
    match
      Common.Source_manager.find (Session.sources session)
        (Common.Source_file.id source)
    with
    | Some registered when registered == source ->
        create_with_authority ?compiler_positions ?max_switch_work
          ?switch_budget ~max_dimension_work ~max_offset_work
          (Source_compilation source) session
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
           Sema.Source_activation.create ~calls:ledger.call_journal
             ~namespace:ledger.namespace ~context:active.context
             ~observed_events:(List.length ledger.source_events_rev)
             (List.rev ledger.activation_events_rev)
           |> fun result ->
           Result.bind result (fun activation ->
               let pending_runtime_dimension =
                 Names.fold
                   (fun _ state found ->
                     match state.pending with
                     | Some
                         {
                           preparation;
                           evaluation = Deferred_runtime_dimension;
                         } -> preparation :: found
                     | _ -> found)
                   ledger.dimension_owners []
               in
               let pending_runtime_dimension =
                 match pending_runtime_dimension with
                 | [] -> Ok None
                 | [ preparation ] -> Ok (Some preparation)
                 | _ ->
                     Error
                       "source activation has multiple deferred runtime \
                        dimensions"
               in
               Result.bind pending_runtime_dimension
                 (fun pending_runtime_dimension ->
                   VM.promote_task_source_activation ?pending_runtime_dimension
                     ?pending_runtime_offset:ledger.pending_runtime_offset
                     ~offsets:(List.rev ledger.offsets_rev)
                     runtime ~namespace:ledger.namespace ~activation
                     ~dimensions:(List.rev ledger.source_dimensions_rev)
                   |> Result.map (fun () ->
                       ledger.activation <- Some activation;
                       ledger.offset_work <- 0;
                       ledger.dimension_work <- 0)))
         else
           VM.promote_task_source runtime ~namespace:ledger.namespace
             ~offsets:(List.rev ledger.offsets_rev)
             ~events:(List.rev ledger.source_events_rev)
             ~dimensions:(List.rev ledger.source_dimensions_rev)
             ~completed_dimensions:
               (List.filter_map
                  (function
                    | Sema.Source_activation.Declaration
                        (Parser.Array_dimension_completed receipt) ->
                        Dimensions.find_opt ledger.checked_dimensions
                          receipt.dimension_ast
                    | _ -> None)
                  (List.rev ledger.activation_events_rev))
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
let compiler_positions ledger = ledger.compiler_positions
let offset_work ledger = ledger.offset_work
let switch_budget ledger = ledger.switch_budget
let switch_work ledger = Switch.budget_work ledger.switch_budget
let switch_preparation_work = switch_work

let selected_dimensions ledger dimensions =
  List.filter_map (Dimensions.find_opt ledger.checked_dimensions) dimensions

let selected_aggregate_for ledger type_specifier =
  Type_specifiers.find_opt ledger.selected_aggregate_types type_specifier

let prepare_selected_aggregate ledger source =
  let module Source = Sema.Source_type_reference in
  let span =
    match source with
    | Source.Function_return p -> p.Parser.function_name.location.span
    | Source.Function_parameter p ->
        p.Parser.parameter_function.function_name.location.span
    | Source.Callback_return p -> p.Parser.callback_opening.span
    | Source.Callback_parameter p ->
        p.Parser.callback_parameter_signature.callback_opening.span
    | Source.Function_local p ->
        p.Parser.allocation_function.function_name.location.span
    | Source.Global_type p -> p.Parser.global_name.location.span
  in
  let type_specifier, selection = Source.source_type source |> checked span in
  match type_specifier with
  | Ast.Primitive_type_specifier _ | Ast.Internal_type_specifier _ ->
      if Option.is_some selection then
        fail span
          "primitive function type unexpectedly retained a class selection";
      None
  | Ast.Named_type_specifier _ ->
      let selection =
        match selection with
        | Some selection -> selection
        | None ->
            fail span "named function type lacks its original class selection"
      in
      if selection.environment != ledger.symbols then
        fail span "named function type selection has another parser environment";
      let publication =
        match Entries.find_opt ledger.entries selection.entry with
        | Some
            { publication; source = Aggregate { publication = original; _ }; _ }
          when original.aggregate_entry == selection.entry -> publication
        | _ ->
            fail span
              "named function type selection has no exact aggregate publication"
      in
      Some
        (Sema.Source_type_reference.select_aggregate ~table:ledger.table
           ~namespace:ledger.namespace ~source publication
        |> checked span)

let retain_selected_aggregate ledger type_specifier = function
  | None -> ()
  | Some proof ->
      let proof =
        match
          Type_specifiers.find_opt ledger.selected_aggregate_types
            type_specifier
        with
        | None -> proof
        | Some original ->
            Sema.Source_type_reference.merge_selections original proof
            |> checked
                 (Frontend.Ast.type_specifier_location type_specifier).span
      in
      Type_specifiers.replace ledger.selected_aggregate_types type_specifier
        proof

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

let function_alias_source ledger reference =
  let header_symbol =
    Ir.Retained_function.metadata reference
    |> Sema.Outer_environment.function_declaration
    |> Sema.Function_resolution.resolved_declaration_header
    |> Sema.Function_type_resolution.function_symbol
  in
  Names.fold
    (fun _ assigned found ->
      match (found, assigned.source) with
      | Some _, _ -> found
      | None, Function state
        when Collection.publication_symbol assigned.publication == header_symbol
        ->
          Some
            (match state.header with
            | Some header -> header.Parser.completed_entry
            | None -> state.publication.function_entry)
      | None, _ -> None)
    ledger.names None

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
          | VM.Admitted_function reference -> (
              match function_alias_source ledger reference with
              | None -> Ok ()
              | Some original_entry ->
                  Visibility.Environment.validate_function_alias ledger.symbols
                    ~original_entry)
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
        let visible =
          match publication with
          | VM.Admitted_function reference -> (
              let declaration =
                Ir.Retained_function.metadata reference
                |> Sema.Outer_environment.function_declaration
              in
              if
                Option.is_none
                  (Sema.Function_resolution
                   .resolved_declaration_completion_source declaration)
              then true
              else
                match
                  Visibility.Environment.find_function ledger.symbols
                    (Sema.Symbol.name symbol)
                with
                | Some entry -> (
                    match symbol_for ledger entry with
                    | Some current -> current == symbol
                    | None -> true)
                | None -> true)
          | _ -> true
        in
        if visible then (
          let kind, function_call_shape =
            match publication with
            | VM.Admitted_global _ | VM.Admitted_declared_global _ ->
                (Visibility.Global_variable, None)
            | VM.Admitted_function reference ->
                let module Function = Sema.Function_type_resolution in
                let signature =
                  Ir.Retained_function.metadata reference
                  |> Sema.Outer_environment.function_declaration
                  |> Sema.Function_resolution.resolved_declaration_header
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
            let original =
              match publication with
              | VM.Admitted_function reference ->
                  function_alias_source ledger reference
              | _ -> None
            in
            match original with
            | Some original_entry ->
                (* The complete batch was checked above. Entry publication does
                   not remove original entries or invoke user callbacks. *)
                Visibility.Environment.add_function_alias ?function_call_shape
                  ledger.symbols ~original_entry ()
                |> Result.get_ok
            | None ->
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
                   ~extent:(Ir.Integer_globals.slot_extent slot))))
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
          let parent =
            match Parser.context_parent context with
            | None -> None
            | Some position ->
                let parent =
                  match position with
                  | Parser.Before_first_command parent -> parent
                  | Parser.Reading_command start -> start.command_context
                  | Parser.Awaiting_resume completed ->
                      completed.command_start.command_context
                in
                (* Historical events in this namespace remain observations.
                   Crossing compiler tables requires the live original stack. *)
                if Parser.context_environment parent == ledger.symbols then
                  Some position
                else
                  Parser.context_parent_in_environment context
                    ~environment:ledger.symbols
                  |> checked span
          in
          let parent_context, matches =
            match parent with
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
                    | Ready -> (
                        match sequence.completed_rev with
                        | latest :: _ -> latest.receipt == command
                        | [] -> false)
                    | _ -> false )
          in
          (match (parent_context, ledger.active) with
          | None, [] -> ()
          | Some parent, sequence :: _
            when sequence.context == parent && matches sequence -> ()
          | Some parent, []
            when Option.fold ~none:false ~some:(( == ) parent)
                   ledger.saved_parent -> ()
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
      | Some { source = Aggregate _; _ } -> Selected_unbound entry
      | Some assigned ->
          let stage =
            match assigned.source with
            | Aggregate _ -> assert false
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

let rec native_record_for_entry ledger entry =
  match Entries.find_opt ledger.entries entry with
  | Some { source = Function state; _ } -> state.native_record
  | Some _ -> None
  | None ->
      Option.bind
        (Visibility.function_alias_original entry)
        (native_record_for_entry ledger)

let capture_runtime_reference ledger selection target =
  match (ledger_runtime ledger, target) with
  | ( Some runtime,
      ( Selected_source { admitted = Some (VM.Admitted_function selected); _ }
      | Selected_runtime (VM.Admitted_function selected) ) ) ->
      VM.observe_task_function_selection runtime ~namespace:ledger.namespace
        ~selection ~selected
      |> checked (Parser.selected_identifier selection).location.span
  | _ -> ()

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
      let native_record =
        match Parser.selected_lookup selection with
        | Visibility.Present entry -> native_record_for_entry ledger entry
        | _ -> None
      in
      Names.add ledger.references identifier
        { selection; target; native_record };
      capture_runtime_reference ledger selection target)

let observe_reference ledger selection =
  Result.map
    (fun () ->
      record_activation_event ledger
        (Sema.Source_activation.Reference selection))
    (observe_reference ledger selection)

let call_reference ledger reference =
  let identifier = Parser.selected_identifier reference in
  let span = identifier.location.span in
  let start = Parser.selected_command reference in
  let sequence = active_sequence ledger start.command_context in
  (match sequence.phase with
  | Reading original when original == start -> ()
  | _ -> fail span "call does not belong to the active parser command");
  match Names.find_opt ledger.references identifier with
  | Some original when original.selection == reference -> original
  | _ -> fail span "call lacks its exact observed identifier selection"

let capture_runtime_call_start ledger call =
  match (ledger_runtime ledger, call.capture) with
  | Some runtime, Some capture ->
      let snapshot =
        Sema.Function_record_phase.call_argument_snapshot capture
      in
      let identifier = Parser.selected_identifier call.start.call_reference in
      let reference = Names.find ledger.references identifier in
      let retained =
        match reference.target with
        | Selected_source { admitted = Some (VM.Admitted_function retained); _ }
        | Selected_runtime (VM.Admitted_function retained) -> retained
        | _ ->
            fail identifier.location.span
              "native call lacks its selected runtime function"
      in
      let scope =
        Ir.Retained_function.metadata retained
        |> Sema.Outer_environment.function_declaration
        |> Sema.Function_resolution.resolved_declaration_header
        |> Sema.Function_type_resolution.function_scope
      in
      let arguments =
        Sema.Function_record_phase.call_shape snapshot
        |> checked identifier.location.span
        |> Function_type_resolution.resolve_provisional_call ~scope
             ~selected_aggregate:(selected_aggregate_for ledger)
             ~table:ledger.table ~namespace:ledger.namespace
        |> checked identifier.location.span
      in
      call.runtime_start <-
        Some
          (VM.capture_task_call_start runtime ~namespace:ledger.namespace
             ~capture ~selected:retained ~arguments
          |> checked identifier.location.span)
  | _ -> ()

let capture_runtime_call_emission ledger call =
  match (ledger_runtime ledger, call.runtime_start, call.emission_capture) with
  | Some runtime, Some pending, Some capture ->
      let span =
        (Parser.selected_identifier call.start.call_reference).location.span
      in
      call.runtime_phase <-
        Some
          (VM.capture_task_call_emission runtime ~table:ledger.table ~capture
             pending
          |> checked span)
  | Some _, None, Some _ ->
      fail (Parser.selected_identifier call.start.call_reference).location.span
        "native emission lacks its original runtime argument capture"
  | _ -> ()

let native_call_arguments ledger span target native_record =
  let snapshot = Option.map Sema.Function_record_phase.snapshot native_record in
  match snapshot with
  | None -> (None, None, None)
  | Some snapshot -> (
      match Sema.Function_record_phase.call_shape snapshot with
      | Error message ->
          (* Legacy providers and earlier session compilations can leave
                 an untracked native predecessor. Only their exact unchanged
                 completed ordinary header keeps legacy grammar; this creates
                 neither a native count nor native emission evidence. *)
          let ordinary_runtime_header retained =
            let function_ =
              Ir.Retained_function.metadata retained
              |> Sema.Outer_environment.function_declaration
              |> Sema.Function_resolution.resolved_declaration_header
            in
            if
              Option.is_some
                (Sema.Function_type_resolution.function_provisional_call
                   function_)
            then None
            else
              Sema.Function_type_resolution.function_completed_header function_
          in
          let legacy_header =
            match (ledger.authority, target) with
            | ( Source_compilation _,
                Selected_source { stage = Function_selection (header, _); _ } )
              -> Some header
            | ( Task_runtime _,
                Selected_source
                  {
                    stage = Function_selection (header, _);
                    admitted = Some (VM.Admitted_function retained);
                    _;
                  } ) ->
                Option.bind (ordinary_runtime_header retained) (fun current ->
                    if current == header then Some header else None)
            | Task_runtime _, Selected_runtime (VM.Admitted_function retained)
              -> ordinary_runtime_header retained
            | _ -> None
          in
          let legacy_header =
            Sema.Function_record_phase.unavailable_reason snapshot
            = Some "previous native function record is untracked"
            && Option.fold ~none:false
                 ~some:(fun header ->
                   Sema.Function_record_phase.native_source snapshot
                   == header.Parser.function_publication
                   && Option.fold ~none:false ~some:(( == ) header)
                        (Sema.Provisional_function.completed_header
                           (Sema.Function_record_phase.source_snapshot snapshot)))
                 legacy_header
          in
          if legacy_header then (None, None, None) else fail span message
      | Ok shape ->
          ( native_record,
            Some snapshot,
            Some
              Visibility.
                {
                  parameters =
                    List.map
                      (fun member ->
                        let source =
                          Sema.Provisional_function.member_source member
                        in
                        {
                          parameter_name =
                            Option.map
                              (fun (name : Ast.identifier) -> name.spelling)
                              source.parameter_name;
                          has_default =
                            Option.is_some
                              (Sema.Provisional_function.member_default_source
                                 member);
                        })
                      (Sema.Function_record_phase.fixed_members shape);
                  variadic =
                    Option.is_some
                      (Sema.Function_record_phase.variadic_tail shape);
                } ))

let observe_call_start ledger start =
  protect (fun () ->
      let span =
        (Parser.selected_identifier start.Parser.call_reference).location.span
      in
      if not (Parser.call_start_is_current start) then
        fail span "call start is outside its original parser callback";
      let reference = call_reference ledger start.call_reference in
      if List.exists (fun call -> call.start == start) ledger.calls then
        fail span "call start was already observed";
      let native_record, arguments, shape =
        native_call_arguments ledger span reference.target
          reference.native_record
      in
      (match ledger.authority with
      | Source_compilation _ ->
          Sema.Source_activation.capture_call_start ledger.call_journal
            ~events_rev:ledger.activation_events_rev start
          |> checked span
          |> record_activation_event ledger
      | _ -> ());
      let capture =
        Option.map
          (fun record ->
            Sema.Function_record_phase.capture_call_start record start
            |> checked span)
          native_record
      in
      let call =
        {
          start;
          capture;
          emission_capture = None;
          arguments;
          emission = None;
          runtime_start = None;
          runtime_phase = None;
        }
      in
      capture_runtime_call_start ledger call;
      ledger.calls <- call :: ledger.calls;
      shape)

let observe_call_emission ledger receipt =
  protect (fun () ->
      let start = receipt.Parser.call_start in
      let span =
        (Parser.selected_identifier start.call_reference).location.span
      in
      if not (Parser.call_emission_is_current receipt) then
        fail span "call emission is outside its original parser callback";
      ignore (call_reference ledger start.call_reference);
      let call =
        match List.find_opt (fun call -> call.start == start) ledger.calls with
        | Some call when Option.is_none call.emission -> call
        | _ -> fail span "call emission lacks an unfinished original call start"
      in
      call.emission_capture <-
        Option.map
          (fun capture ->
            Sema.Function_record_phase.capture_call_emission capture receipt
            |> checked span)
          call.capture;
      let snapshot =
        Option.map Sema.Function_record_phase.call_emission_snapshot
          call.emission_capture
      in
      (match ledger.authority with
      | Source_compilation _ ->
          Sema.Source_activation.capture_call_emission ledger.call_journal
            ~events_rev:ledger.activation_events_rev receipt
          |> checked span
          |> record_activation_event ledger
      | _ -> ());
      capture_runtime_call_emission ledger call;
      call.emission <- Some (receipt, snapshot))

let call_record_snapshots ledger receipt =
  protect (fun () ->
      let span =
        (Parser.selected_identifier receipt.Parser.call_start.call_reference)
          .location
          .span
      in
      match
        List.find_opt
          (fun call -> call.start == receipt.call_start)
          ledger.calls
      with
      | Some { arguments; emission = Some (original, snapshot); _ }
        when original == receipt -> (arguments, snapshot)
      | _ -> fail span "call phases lack their exact observed completion")

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

let validate_execution_target_at span command target =
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
      if start != command then
        unavailable
          "partial source publication has not reached runtime admission"
  | Selected_local | Selected_runtime _ | Selected_source _ -> ()

let validate_execution_target selection target =
  validate_execution_target_at
    (Parser.selected_identifier selection).location.span
    (Parser.selected_command selection)
    target

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

let capture_runtime_implicit_selection ledger selection target =
  match (ledger_runtime ledger, target) with
  | ( Some runtime,
      ( Selected_source { admitted = Some (VM.Admitted_function selected); _ }
      | Selected_runtime (VM.Admitted_function selected) ) ) ->
      VM.observe_task_implicit_selection runtime ~namespace:ledger.namespace
        ~selection ~selected
      |> checked (Parser.implicit_marker selection).span
  | _ -> ()

let observe_implicit_output ledger selection =
  let span = (Parser.implicit_marker selection).span in
  Result.map
    (fun () ->
      record_activation_event ledger
        (Sema.Source_activation.Implicit_output selection))
    (protect (fun () ->
         let start = Parser.implicit_command selection in
         if not (Parser.implicit_selection_is_current selection) then
           fail span "implicit target is outside its original parser callback";
         let sequence = active_sequence ledger start.command_context in
         (match sequence.phase with
         | Reading saved when saved == start -> ()
         | _ -> fail span "implicit target belongs to another parser command");
         if Parser.implicit_environment selection != ledger.symbols then
           fail span "implicit target has another frontend environment";
         if
           List.exists
             (fun original -> original.implicit_selection == selection)
             ledger.implicit_outputs
         then fail span "implicit target selection was already observed";
         let target =
           selection_target ledger span
             (match Parser.implicit_lookup selection with
             | None -> Visibility.Absent
             | Some entry -> Visibility.Present entry)
         in
         ledger.implicit_outputs <-
           {
             implicit_selection = selection;
             implicit_target = target;
             implicit_native =
               Option.bind
                 (Parser.implicit_lookup selection)
                 (native_record_for_entry ledger);
             arguments_capture = None;
             emitted_capture = None;
             implicit_pending = None;
             implicit_phase = None;
             arguments_observed = false;
             emission_observed = false;
           }
           :: ledger.implicit_outputs;
         capture_runtime_implicit_selection ledger selection target))

let implicit_call ledger selection =
  match
    List.find_opt
      (fun call -> call.implicit_selection == selection)
      ledger.implicit_outputs
  with
  | Some call -> call
  | None ->
      fail (Parser.implicit_marker selection).span
        "implicit call lacks its original target selection"

let capture_runtime_implicit_arguments ledger call =
  match (ledger_runtime ledger, call.arguments_capture) with
  | Some runtime, Some capture ->
      let span = (Parser.implicit_marker call.implicit_selection).span in
      let selected =
        match call.implicit_target with
        | Selected_source { admitted = Some (VM.Admitted_function selected); _ }
        | Selected_runtime (VM.Admitted_function selected) -> selected
        | _ -> fail span "implicit call lacks its admitted selected function"
      in
      let scope =
        Ir.Retained_function.metadata selected
        |> Sema.Outer_environment.function_declaration
        |> Sema.Function_resolution.resolved_declaration_header
        |> Sema.Function_type_resolution.function_scope
      in
      let arguments =
        Sema.Function_record_phase.implicit_argument_snapshot capture
        |> Sema.Function_record_phase.call_shape |> checked span
        |> Function_type_resolution.resolve_provisional_call ~scope
             ~selected_aggregate:(selected_aggregate_for ledger)
             ~table:ledger.table ~namespace:ledger.namespace
        |> checked span
      in
      call.implicit_pending <-
        Some
          (VM.capture_task_implicit_arguments runtime
             ~namespace:ledger.namespace ~capture ~selected ~arguments
          |> checked span)
  | _ -> ()

let capture_runtime_implicit_emission ledger call =
  match
    (ledger_runtime ledger, call.emitted_capture, call.implicit_pending)
  with
  | Some runtime, Some capture, Some pending ->
      call.implicit_phase <-
        Some
          (VM.capture_task_implicit_emission runtime ~table:ledger.table
             ~capture pending
          |> checked (Parser.implicit_marker call.implicit_selection).span)
  | Some _, Some _, None ->
      fail (Parser.implicit_marker call.implicit_selection).span
        "implicit emission lacks its original runtime argument capture"
  | _ -> ()

let observe_implicit_arguments ledger selection =
  protect (fun () ->
      let span = (Parser.implicit_marker selection).span in
      let call = implicit_call ledger selection in
      if
        (not (Parser.implicit_arguments_are_current selection))
        || call.arguments_observed
      then
        fail span
          "implicit arguments are outside their original unfinished callback";
      let native, _, shape =
        native_call_arguments ledger span call.implicit_target
          call.implicit_native
      in
      let capture =
        Option.map
          (fun record ->
            Sema.Function_record_phase.capture_implicit_arguments record
              selection
            |> checked span)
          native
      in
      (match ledger.authority with
      | Source_compilation _ ->
          Sema.Source_activation.capture_implicit ledger.call_journal
            ~events_rev:ledger.activation_events_rev selection ~emission:false
          |> checked span
          |> record_activation_event ledger
      | _ -> ());
      call.arguments_observed <- true;
      call.arguments_capture <- capture;
      capture_runtime_implicit_arguments ledger call;
      shape)

let observe_implicit_emission ledger selection =
  protect (fun () ->
      let span = (Parser.implicit_marker selection).span in
      let call = implicit_call ledger selection in
      if
        (not
           (Parser.implicit_emission_is_current selection
           && call.arguments_observed))
        || call.emission_observed
      then
        fail span
          "implicit emission lacks its original unfinished argument capture";
      let capture =
        Option.map
          (fun arguments ->
            Sema.Function_record_phase.capture_implicit_emission arguments
              selection
            |> checked span)
          call.arguments_capture
      in
      (match ledger.authority with
      | Source_compilation _ ->
          Sema.Source_activation.capture_implicit ledger.call_journal
            ~events_rev:ledger.activation_events_rev selection ~emission:true
          |> checked span
          |> record_activation_event ledger
      | _ -> ());
      call.emission_observed <- true;
      call.emitted_capture <- capture;
      capture_runtime_implicit_emission ledger call)

let validate_implicit_output ledger selection ~execution =
  let span = (Parser.implicit_marker selection).span in
  protect (fun () ->
      let original =
        List.find_opt
          (fun original -> original.implicit_selection == selection)
          ledger.implicit_outputs
        |> function
        | Some original -> original
        | None -> fail span "implicit output has no exact original selection"
      in
      if execution then (
        if Option.is_none (ledger_runtime ledger) then
          fail span "implicit execution selection has no owning runtime";
        validate_execution_target_at span
          (Parser.implicit_command selection)
          original.implicit_target)
      else
        match original.implicit_target with
        | Selected_absent | Selected_unbound _ ->
            fail ~code:"HCRUN0003" span
              "implicit output has no checked function header at its source \
               read"
        | _ -> ())

let selected_base_record ledger (phase : Parser.aggregate_phase) =
  match phase.phase_step with
  | Parser.Aggregate_base_attached selection
    when selection.base_environment == ledger.symbols -> (
      match Entries.find_opt ledger.entries selection.base_entry with
      | Some { publication; source = Aggregate _; _ } -> (
          match
            Collection.current_aggregate_publication ledger.namespace
              publication
          with
          | None ->
              Error "inherited class has no original canonical publication"
          | Some current -> (
              let selected =
                Entries.fold
                  (fun _ assigned found ->
                    if assigned.publication == current then Some assigned.source
                    else found)
                  ledger.entries None
              in
              match selected with
              | Some (Aggregate { record = Some (Ok record); _ }) ->
                  Sema.Compiler_record.select_aggregate_base ~table:ledger.table
                    ~namespace:ledger.namespace
                    ~selected_publication:publication phase record
              | Some (Aggregate { record = Some (Error message); _ }) ->
                  Error message
              | _ ->
                  Error
                    "inherited class lacks its current original layout record"))
      | _ -> Error "inherited class has no original retained source publication"
      )
  | _ -> Error "inherited layout read belongs to another original base phase"

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
        | Selected_unbound entry -> (
            match Entries.find_opt ledger.entries entry with
            | Some { source = Aggregate state; _ } -> (
                match state.record with
                | Some record -> Some record
                | None ->
                    Some
                      (Error
                         "sizeof requires a completed retained aggregate; \
                          partial class layout is not implemented"))
            | _ ->
                Session.primitive_for ledger.session entry
                |> Option.map (fun binding ->
                    Ok (Session.primitive_record binding)))
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
  if !(ledger.next_ordinal) = max_int then
    fail name.location.span "task declaration publication order is exhausted";
  let publication =
    (match source with
      | Aggregate state ->
          Collection.publish_aggregate ledger.namespace state.publication
      | Global state ->
          Collection.publish_global ledger.namespace state.publication
      | Function state ->
          Collection.publish_function ledger.namespace state.publication)
    |> checked name.location.span
  in
  if Sema.Symbol.kind (Collection.publication_symbol publication) <> kind then
    fail name.location.span "parser publication has the wrong declaration kind";
  (match source with
  | Function state when Parser.function_publication_is_current state.publication
    ->
      if
        Parser.context_mode
          state.publication.function_header.declaration_command.command_context
        = Frontend.Preprocessor.Jit
      then
        let registry =
          match ledger.native_functions with
          | Some registry -> registry
          | None ->
              let registry =
                Sema.Function_record_phase.create_registry
                  ~mode:Frontend.Preprocessor.Jit ~table:ledger.table
                  ~namespace:ledger.namespace
                |> checked name.location.span
              in
              ledger.native_functions <- Some registry;
              registry
        in
        state.native_record <-
          Some
            (Sema.Function_record_phase.begin_header
               ?internal_target:
                 (List.find_opt
                    (fun target ->
                      Sema.Prepared_internal_binding.matches_header target
                        state.publication.function_header)
                    ledger.prepared_internal_bindings)
               registry publication state.publication
            |> checked name.location.span)
      else
        state.provisional_source <-
          Some
            (Sema.Provisional_function.create ~table:ledger.table
               ~namespace:ledger.namespace publication state.publication
            |> checked name.location.span)
  | _ -> ());
  let assigned =
    { publication; source; ordinal = !(ledger.next_ordinal); claimed = false }
  in
  incr ledger.next_ordinal;
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

let function_record_snapshot ledger publication =
  protect (fun () ->
      let span = publication.Parser.function_name.location.span in
      if not (Sema.Source_activation.finished ledger.activation) then
        match
          List.find_opt
            (fun (event, snapshot) ->
              Sema.Source_activation.declaration ledger.activation event
              && Sema.Function_record_phase.source snapshot == publication)
            ledger.native_function_events
        with
        | Some (_, snapshot) -> snapshot
        | None ->
            fail span
              "native function phase is outside its original activation event"
      else
        match Names.find_opt ledger.names publication.function_name with
        | Some { source = Function state; _ }
          when state.publication == publication -> (
            match state.native_record with
            | Some record -> Sema.Function_record_phase.snapshot record
            | None -> fail span "function has no observed JIT native record")
        | _ ->
            fail span
              "native function record belongs to another source publication")

let observe_function_source span native source event =
  match (native, source) with
  | Some record, None ->
      Sema.Function_record_phase.observe record event |> checked span
  | None, Some source ->
      Sema.Provisional_function.observe source event |> checked span
  | _ -> fail span "provisional member lacks its original declaration callback"

let require_function_record_snapshot ledger publication =
  match function_record_snapshot ledger publication with
  | Ok snapshot -> snapshot
  | Error (diagnostic :: _) -> raise (Invalid diagnostic)
  | Error [] -> assert false

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

let offset_requires_runtime (phase : Parser.aggregate_phase) =
  match phase.phase_step with
  | Parser.Aggregate_offset_reached expression ->
      Sema.Initializer_source.expression_identifier_nodes expression <> []
  | _ -> false

let dimension_requires_runtime
    (preparation : Parser.array_dimension_preparation) =
  Option.fold ~none:false
    ~some:(fun expression ->
      Sema.Initializer_source.expression_identifier_nodes expression <> [])
    preparation.dimension_expression

let prepare_dimension ?(defer_runtime = false) ledger
    (preparation : Parser.array_dimension_preparation) =
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
  | Task_runtime _ when dimension_requires_runtime preparation ->
      pending.evaluation <- Awaiting_runtime_dimension
  | Source_compilation _
    when defer_runtime && dimension_requires_runtime preparation ->
      pending.evaluation <- Deferred_runtime_dimension
  | Source_compilation _ when dimension_requires_runtime preparation ->
      fail ~code:"HCRUN0006" span
        "runtime AOT dimensions require output relocation and callable \
         authority"
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
        match ledger.authority with
        | Task_runtime task ->
            VM.prepare_task_closed_dimension task ~table:ledger.table
              ~namespace:ledger.namespace ~preparation ~queries
        | _ ->
            Sema.Compiler_record.prepare_dimension ~table:ledger.table
              ~namespace:ledger.namespace ~max_work:(limit - before)
              ~preparation ~queries
      in
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
    | Failed_dimension
    | Deferred_runtime_dimension
    | Awaiting_runtime_dimension
    | Executing_dimension _ ->
        fail span "array dimension preparation did not succeed"
    | Proposed_runtime_dimension (proposal, queries) ->
        Some
          (Sema.Compiler_record.complete_runtime_dimension ~table:ledger.table
             ~receipt ~queries proposal
          |> checked span)
    | Prepared_dimension prepared ->
        Some
          (Sema.Compiler_record.complete_dimension ~receipt prepared
          |> checked span)
  in
  (match (ledger.authority, prepared) with
  | Task_runtime task, Some prepared ->
      VM.complete_task_dimension task ~namespace:ledger.namespace prepared
      |> checked span
  | _ -> ());
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

let callback_state ledger publication span =
  match
    List.find_opt
      (fun state -> state.callback_publication == publication)
      ledger.callback_states
  with
  | Some state -> state
  | None ->
      fail span "anonymous signature has no original observed declaration scope"

let validate_callback_command ledger publication span =
  let sequence =
    active_sequence ledger publication.Parser.callback_command.command_context
  in
  match sequence.phase with
  | Reading original when original == publication.callback_command -> ()
  | _ -> fail span "anonymous signature belongs to another source command"

let completed_callback_header ledger source =
  List.find_map
    (fun state ->
      Option.bind state.callback_header (fun header ->
          if header.Parser.callback_pointer == source then Some header else None))
    ledger.callback_states

let declared_callback_for ledger span =
  Option.map (fun source ->
      let header =
        match completed_callback_header ledger source with
        | Some header -> header
        | None ->
            fail span "declared callback lacks its original observed header"
      in
      let pointer =
        Function_type_resolution.resolve_completed_callback ~table:ledger.table
          ~namespace:ledger.namespace
          ~selected_aggregate:(selected_aggregate_for ledger)
          header
        |> checked span
      in
      (header, pointer))

let observe ?offset_runtime ledger event =
  protect (fun () ->
      match event with
      | Parser.Callback_position_written receipt ->
          let owner = receipt.callback_position_signature in
          let span = owner.callback_opening.span in
          validate_callback_command ledger owner span;
          let state = callback_state ledger owner span in
          if
            Option.is_some state.callback_header
            || Option.is_some state.callback_pending
          then
            fail span
              "anonymous position is outside its original member boundary";
          Sema.Compiler_record.record_callback_position
            ledger.compiler_positions
            ~parameters:(List.rev state.callback_members_rev)
            receipt
          |> checked span
      | Parser.Callback_signature_started publication ->
          let span = publication.callback_opening.span in
          validate_callback_command ledger publication span;
          if
            (not (Parser.callback_signature_is_current publication))
            || List.exists
                 (fun state -> state.callback_publication == publication)
                 ledger.callback_states
          then fail span "anonymous signature start is foreign or repeated";
          let selected =
            prepare_selected_aggregate ledger
              (Sema.Source_type_reference.Callback_return publication)
          in
          retain_selected_aggregate ledger
            publication.callback_return_type_specifier selected;
          ledger.callback_states <-
            {
              callback_publication = publication;
              callback_pending = None;
              callback_members_rev = [];
              callback_defaults_rev = [];
              callback_header = None;
            }
            :: ledger.callback_states
      | Parser.Callback_parameter_declared publication ->
          let owner = publication.callback_parameter_signature in
          let span = owner.callback_opening.span in
          validate_callback_command ledger owner span;
          let state = callback_state ledger owner span in
          if
            (not (Parser.callback_parameter_is_current publication))
            || Option.is_some state.callback_header
            || Option.is_some state.callback_pending
            || publication.callback_parameter_index
               <> List.length state.callback_members_rev
            || not
                 (same_option ( == ) publication.callback_parameter_predecessor
                    (List.nth_opt state.callback_members_rev 0))
          then
            fail span
              "anonymous parameter has another original position or predecessor";
          let selected =
            prepare_selected_aggregate ledger
              (Sema.Source_type_reference.Callback_parameter publication)
          in
          retain_selected_aggregate ledger
            publication.callback_parameter_type_specifier selected;
          state.callback_pending <- Some publication
      | Parser.Callback_default_completed receipt ->
          let owner = receipt.callback_default_signature in
          let span = receipt.callback_default_ast.location.span in
          validate_callback_command ledger owner span;
          let state = callback_state ledger owner span in
          if
            (not (Parser.callback_default_is_current receipt))
            || Option.is_some state.callback_header
            || (not
                  (Option.fold ~none:false
                     ~some:(( == ) receipt.callback_default_parameter)
                     state.callback_pending))
            || receipt.callback_default_index
               <> receipt.callback_default_parameter.callback_parameter_index
            || (not
                  (same_option ( == ) receipt.callback_default_predecessor
                     (List.nth_opt state.callback_defaults_rev 0)))
            || List.exists (( == ) receipt) state.callback_defaults_rev
          then
            fail span
              "anonymous default has another original member or completion \
               order";
          state.callback_defaults_rev <- receipt :: state.callback_defaults_rev
      | Parser.Callback_parameter_completed receipt ->
          let publication = receipt.callback_parameter_publication in
          let owner = publication.callback_parameter_signature in
          let span = receipt.callback_parameter_ast.location.span in
          validate_callback_command ledger owner span;
          let state = callback_state ledger owner span in
          let ast = receipt.callback_parameter_ast in
          if
            (not (Parser.callback_parameter_completion_is_current receipt))
            || (not
                  (Option.fold ~none:false ~some:(( == ) publication)
                     state.callback_pending))
            || ast.type_specifier
               != publication.callback_parameter_type_specifier
            || ast.pointer_layers
               != publication.callback_parameter_pointer_layers
            || ast.register_qualifiers
               != publication.callback_parameter_register_qualifiers
            || (not
                  (same_option ( == ) ast.name
                     publication.callback_parameter_name))
            || (not
                  (same_option ( == ) ast.function_pointer
                     publication.callback_parameter_function_pointer))
            ||
            match ast.default with
            | None ->
                List.exists
                  (fun r -> r.Parser.callback_default_parameter == publication)
                  state.callback_defaults_rev
            | Some default ->
                not
                  (List.exists
                     (fun r ->
                       r.Parser.callback_default_parameter == publication
                       && r.callback_default_ast == default)
                     state.callback_defaults_rev)
          then
            fail span
              "anonymous parameter completion lost its exact original children";
          state.callback_pending <- None;
          state.callback_members_rev <- receipt :: state.callback_members_rev
      | Parser.Callback_signature_completed header ->
          let owner = header.callback_signature_publication in
          let span = owner.callback_opening.span in
          validate_callback_command ledger owner span;
          let state = callback_state ledger owner span in
          if
            (not (Parser.callback_signature_completion_is_current header))
            || Option.is_some state.callback_header
            || Option.is_some state.callback_pending
            || List.length header.callback_parameters
               <> List.length state.callback_members_rev
            || (not
                  (List.for_all2 ( == ) header.callback_parameters
                     (List.rev state.callback_members_rev)))
            || List.length header.callback_defaults
               <> List.length state.callback_defaults_rev
            || (not
                  (List.for_all2 ( == ) header.callback_defaults
                     (List.rev state.callback_defaults_rev)))
            || List.length header.callback_pointer.signature_parameters
               <> List.length header.callback_parameters
            || not
                 (List.for_all2
                    (fun ast r -> ast == r.Parser.callback_parameter_ast)
                    header.callback_pointer.signature_parameters
                    header.callback_parameters)
          then
            fail span
              "anonymous signature completion is foreign, repeated or missing \
               original members";
          state.callback_header <- Some header
      | Parser.Internal_binding_preparing receipt ->
          let span = receipt.binding_ast.location.span in
          let sequence =
            active_sequence ledger receipt.binding_command.command_context
          in
          if
            (not (Parser.internal_binding_is_current receipt))
            || receipt.binding_environment != ledger.symbols
            || List.exists (( == ) receipt) ledger.internal_bindings
            ||
            match sequence.phase with
            | Reading start -> start != receipt.binding_command
            | _ -> true
          then
            fail span
              "internal binding is foreign, repeated or outside its original \
               callback";
          ledger.internal_bindings <- receipt :: ledger.internal_bindings
      | Parser.Aggregate_declared publication -> (
          validate_source ledger publication.aggregate_environment
            publication.aggregate_header publication.aggregate_name;
          if not (Parser.aggregate_publication_is_current publication) then
            fail publication.aggregate_name.location.span
              "aggregate publication is outside its original callback";
          assign ledger publication.aggregate_name Sema.Symbol.Aggregate_type
            (Aggregate
               { publication; progress = None; completed = None; record = None })
            publication.aggregate_entry;
          let assigned = find ledger publication.aggregate_name in
          match assigned.source with
          | Aggregate state ->
              let progress =
                Sema.Compiler_record.begin_aggregate ~table:ledger.table
                  ~compiler_positions:ledger.compiler_positions
                  ~namespace:ledger.namespace assigned.publication
                |> checked publication.aggregate_name.location.span
              in
              state.progress <- Some progress;
              state.record <-
                Some (Sema.Compiler_record.aggregate_metadata progress)
          | _ -> assert false)
      | Parser.Aggregate_advanced phase
        when (match ledger.authority with
               | Task_runtime _ -> true
               | _ -> false)
             && offset_requires_runtime phase ->
          if Option.is_some offset_runtime then
            fail phase.phase_location.span
              "runtime offset cannot borrow isolated authority";
          validate_command ledger phase.phase_aggregate.aggregate_header
      | Parser.Aggregate_advanced phase -> (
          let publication = phase.phase_aggregate in
          validate_command ledger publication.aggregate_header;
          let assigned = find ledger publication.aggregate_name in
          match assigned.source with
          | Aggregate { progress = Some progress; _ } as source -> (
              Option.iter
                (fun runtime ->
                  match (ledger.authority, phase.phase_step) with
                  | Source_compilation _, Parser.Aggregate_offset_reached _ -> (
                      match ledger.source_defaults_runtime with
                      | Some previous when previous != runtime ->
                          fail phase.phase_location.span
                            "source preparation belongs to another directive \
                             task"
                      | _ -> ledger.source_defaults_runtime <- Some runtime)
                  | _ ->
                      fail phase.phase_location.span
                        "isolated offset requires its original source phase")
                offset_runtime;
              (match (phase.phase_step, ledger.authority) with
              | ( Parser.Aggregate_offset_reached expression,
                  (Source_compilation _ | Task_runtime _) ) -> (
                  let queries =
                    Sema.Query_selection.source_queries expression
                    |> List.map (fun expression ->
                        match
                          Query_expressions.find_opt ledger.queries expression
                        with
                        | Some query ->
                            Sema.Query_selection.checked_read
                              query.query_selection
                        | None ->
                            fail phase.phase_location.span
                              "aggregate offset lacks its original query read")
                  in
                  let result, work =
                    match (ledger.authority, offset_runtime) with
                    | Task_runtime task, _ ->
                        VM.prepare_task_aggregate_offset task
                          ~table:ledger.table ~namespace:ledger.namespace
                          ~queries progress phase
                    | Source_compilation _, Some runtime ->
                        VM.prepare_isolated_aggregate_offset runtime
                          ~table:ledger.table ~namespace:ledger.namespace
                          ~queries progress phase
                    | _ ->
                        Sema.Compiler_record.prepare_aggregate_offset
                          ~table:ledger.table ~namespace:ledger.namespace
                          ~queries
                          ~max_work:(ledger.max_offset_work - ledger.offset_work)
                          progress phase
                  in
                  ledger.offset_work <- ledger.offset_work + work;
                  match result with
                  | Ok offset ->
                      ledger.offsets_rev <- offset :: ledger.offsets_rev
                  | Error message ->
                      let code =
                        if String.starts_with ~prefix:"HCIRVM0007:" message then
                          "HCIRVM0007"
                        else "HCRUN0004"
                      in
                      fail ~code phase.phase_location.span message)
              | _ -> ());
              Sema.Compiler_record.advance_aggregate
                ~callbacks:(completed_callback_header ledger)
                ~bases:(selected_base_record ledger)
                ~dimensions:(Dimensions.find_opt ledger.checked_dimensions)
                progress phase
              |> checked phase.phase_location.span;
              match source with
              | Aggregate state ->
                  state.record <-
                    Some (Sema.Compiler_record.aggregate_metadata progress)
              | _ -> assert false)
          | _ ->
              fail phase.phase_location.span
                "aggregate phase has no original publication")
      | Parser.Aggregate_completed receipt -> (
          let publication = receipt.aggregate_publication in
          validate_command ledger publication.aggregate_header;
          let assigned = find ledger publication.aggregate_name in
          match assigned.source with
          | Aggregate state
            when state.publication == publication
                 && Option.is_none state.completed
                 && Parser.aggregate_completion_is_current receipt ->
              state.record <-
                Some
                  (Sema.Compiler_record.complete_aggregate
                     ~callbacks:(completed_callback_header ledger)
                     ?progress:state.progress ~table:ledger.table
                     ~dimensions:(Dimensions.find_opt ledger.checked_dimensions)
                     ~namespace:ledger.namespace assigned.publication receipt);
              state.completed <- Some receipt
          | _ ->
              fail publication.aggregate_name.location.span
                "aggregate completion is foreign, expired or repeated")
      | Parser.Array_dimension_preparing preparation ->
          prepare_dimension ledger preparation
      | Parser.Array_dimension_completed receipt ->
          complete_dimension ledger receipt
      | Parser.Switch_case_preparing _
      | Parser.Switch_case_completed _
      | Parser.Switch_completed _ -> ()
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
          let selected =
            prepare_selected_aggregate ledger
              (Sema.Source_type_reference.Global_type publication)
          in
          retain_selected_aggregate ledger
            publication.global_header.type_specifier selected;
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
          let selected =
            prepare_selected_aggregate ledger
              (Sema.Source_type_reference.Function_return publication)
          in
          assign ledger publication.function_name Sema.Symbol.Function
            (Function
               {
                 publication;
                 provisional_source = None;
                 native_record = None;
                 runtime_phase = None;
                 defaults_rev = [];
                 header = None;
                 declared_header = None;
                 typed_header = None;
                 body = None;
                 body_compiler_options = None;
               })
            publication.function_entry;
          retain_selected_aggregate ledger
            publication.function_header.type_specifier selected
      | Parser.Function_local_allocated receipt -> (
          let publication = receipt.allocation_function in
          validate_command ledger publication.function_header;
          let span = publication.function_name.location.span in
          if not (Parser.function_local_allocation_is_current receipt) then
            fail span "local allocation is outside its original callback";
          let selected =
            prepare_selected_aggregate ledger
              (Sema.Source_type_reference.Function_local receipt)
          in
          (match receipt.allocation_local.local_source with
          | Parser.Local_variable local ->
              retain_selected_aggregate ledger local.local_type_specifier
                selected
          | _ -> fail span "local allocation lacks its original type occurrence");
          match (find ledger publication.function_name).source with
          | Function state when state.publication == publication ->
              Option.iter
                (fun record ->
                  let dimensions =
                    match receipt.allocation_local.local_source with
                    | Parser.Local_variable source ->
                        selected_dimensions ledger source.local_array_dimensions
                    | _ -> []
                  in
                  Sema.Compiler_record.record_local_allocation
                    ~table:ledger.table ~namespace:ledger.namespace ~dimensions
                    ledger.compiler_positions record receipt
                  |> checked span;
                  Option.iter
                    (fun allocation ->
                      ledger.static_allocations_rev <-
                        allocation :: ledger.static_allocations_rev)
                    (Sema.Compiler_record.static_allocation
                       ledger.compiler_positions receipt))
                state.native_record
          | _ -> fail span "local allocation belongs to another declaration")
      | Parser.Static_initializer_preparing receipt ->
          let publication = receipt.static_allocation.allocation_function in
          let span = publication.function_name.location.span in
          validate_command ledger publication.function_header;
          let previous = List.nth_opt ledger.static_preparations 0 in
          let ordered =
            match
              ( receipt.static_leaf_index,
                receipt.static_leaf_predecessor,
                previous )
            with
            | 0, None, _ -> true
            | index, Some predecessor, Some previous
              when index > 0 && predecessor == previous
                   && predecessor.static_initializer
                      == receipt.static_initializer
                   && predecessor.static_leaf_index + 1 = index -> true
            | _ -> false
          in
          if
            (not (Parser.static_initializer_is_current receipt))
            || (not ordered)
            || List.exists (( == ) receipt) ledger.static_preparations
          then
            fail span
              "static preparation is outside its original callback, repeated \
               or out of order";
          (match (find ledger publication.function_name).source with
          | Function state when state.publication == publication -> ()
          | _ -> fail span "static preparation belongs to another declaration");
          ledger.static_preparations <- receipt :: ledger.static_preparations
      | Parser.Static_initializer_completed receipt ->
          let publication =
            receipt.static_completed_start.static_start_allocation
              .allocation_function
          in
          let span = publication.function_name.location.span in
          validate_command ledger publication.function_header;
          let observed_last =
            match List.nth_opt ledger.static_preparations 0 with
            | Some preparation
              when preparation.static_initializer
                   == receipt.static_completed_start -> Some preparation
            | None | Some _ -> None
          in
          if
            (not (Parser.static_initializer_completion_is_current receipt))
            || (not
                  (same_option ( == ) observed_last receipt.static_preparation))
            || List.exists
                 (fun p ->
                   p.Parser.static_completed_start
                   == receipt.static_completed_start)
                 ledger.static_completions
          then
            fail span "static completion is foreign, repeated or out of order";
          ledger.static_completions <- receipt :: ledger.static_completions
      | Parser.Function_position_written receipt -> (
          let publication = receipt.position_function in
          validate_command ledger publication.function_header;
          let span = publication.function_name.location.span in
          if not (Parser.function_position_is_current receipt) then
            fail span "function position write is outside its original callback";
          match (find ledger publication.function_name).source with
          | Function state when state.publication == publication ->
              Option.iter
                (fun record ->
                  Sema.Compiler_record.record_function_position
                    ledger.compiler_positions record receipt
                  |> checked span)
                state.native_record
          | _ -> fail span "function position belongs to another declaration")
      | ( Parser.Function_parameter_declared _
        | Parser.Function_parameter_completed _
        | Parser.Function_variadic_started _
        | Parser.Function_variadic_completed _ ) as event -> (
          let publication =
            match event with
            | Parser.Function_parameter_declared p -> p.parameter_function
            | Parser.Function_parameter_completed p ->
                p.parameter_publication.parameter_function
            | Parser.Function_variadic_started p
            | Parser.Function_variadic_completed p -> p.variadic_function
            | _ -> assert false
          in
          validate_command ledger publication.function_header;
          let span = publication.function_name.location.span in
          match (find ledger publication.function_name).source with
          | Function state when state.publication == publication -> (
              let selected =
                match event with
                | Parser.Function_parameter_declared parameter ->
                    prepare_selected_aggregate ledger
                      (Sema.Source_type_reference.Function_parameter parameter)
                | _ -> None
              in
              observe_function_source span state.native_record
                state.provisional_source event;
              match event with
              | Parser.Function_parameter_declared parameter ->
                  retain_selected_aggregate ledger
                    parameter.parameter_type_specifier selected
              | _ -> ())
          | _ -> fail span "provisional member belongs to another function")
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
              observe_function_source span state.native_record
                state.provisional_source event;
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
              let declared_header =
                if Parser.function_header_is_current header then
                  Some
                    (Sema.Compiler_record.declare_function ~table:ledger.table
                       ~namespace:ledger.namespace assigned.publication header
                    |> checked publication.function_name.location.span)
                else None
              in
              if
                Option.is_some state.native_record
                || Option.is_some state.provisional_source
              then
                observe_function_source publication.function_name.location.span
                  state.native_record state.provisional_source event;
              state.declared_header <- declared_header;
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
                      state.header ->
              Option.iter
                (fun record ->
                  Sema.Function_record_phase.observe record event
                  |> checked body.location.span)
                state.native_record;
              state.body_compiler_options <-
                Some
                  (Parser.function_body_compiler_options header body
                  |> checked body.location.span);
              state.body <- Some body
          | _ ->
              fail publication.function_name.location.span
                "function body completion is foreign, repeated or out of order"))

let observe ?offset_runtime ledger event =
  let observed = observe ?offset_runtime ledger event in
  let observed =
    Result.bind observed (fun () ->
        match ledger.authority with
        | Semantic_analysis -> Ok ()
        | Source_compilation _ | Task_runtime _ ->
            Switch.observe ledger.switch_tracker event)
  in
  Result.map
    (fun () ->
      let publication =
        match event with
        | Parser.Function_declared publication -> Some publication
        | Parser.Function_parameter_declared member ->
            Some member.parameter_function
        | Parser.Function_parameter_completed member ->
            Some member.parameter_publication.parameter_function
        | Parser.Parameter_default_completed receipt ->
            Some receipt.default_function
        | Parser.Function_variadic_started receipt
        | Parser.Function_variadic_completed receipt ->
            Some receipt.variadic_function
        | Parser.Function_header_completed header
        | Parser.Function_body_completed (header, _) ->
            Some header.function_publication
        | _ -> None
      in
      Option.iter
        (fun publication ->
          match
            Names.find_opt ledger.names publication.Parser.function_name
          with
          | Some { source = Function { native_record = Some record; _ }; _ } ->
              ledger.native_function_events <-
                (event, Sema.Function_record_phase.snapshot record)
                :: ledger.native_function_events
          | _ -> ())
        publication;
      record_activation_event ledger (Sema.Source_activation.Declaration event))
    observed

let defer_source_runtime_dimension ledger ~preparation event =
  protect (fun () ->
      match event with
      | Parser.Array_dimension_preparing original when original == preparation
        ->
          let context =
            preparation.dimension_owner.dimensions_command.command_context
          in
          let span = preparation.dimension_opening.span in
          if
            (match ledger.authority with
              | Source_compilation _ -> false
              | _ -> true)
            || (not (dimension_requires_runtime preparation))
            || Parser.context_mode context <> Frontend.Preprocessor.Jit
            || Option.is_some (Parser.context_parent context)
            || not (Parser.dimension_preparation_is_current preparation)
          then
            fail span
              "deferred dimension requires its original live JIT source \
               callback";
          prepare_dimension ~defer_runtime:true ledger preparation;
          record_activation_event ledger
            (Sema.Source_activation.Declaration event)
      | _ ->
          fail preparation.dimension_opening.span
            "deferred runtime dimension requires its exact preparation event")

let defer_source_runtime_offset ledger ~phase event =
  protect (fun () ->
      let span = phase.Parser.phase_location.span in
      match event with
      | Parser.Aggregate_advanced original when original == phase ->
          let context =
            phase.phase_aggregate.aggregate_header.declaration_command
              .command_context
          in
          if
            (match ledger.authority with
              | Source_compilation _ -> false
              | _ -> true)
            || (not (offset_requires_runtime phase))
            || Parser.context_mode context <> Frontend.Preprocessor.Jit
            || Option.is_some (Parser.context_parent context)
            || not (Parser.aggregate_phase_is_current phase)
          then
            fail span "deferred offset requires its original live JIT callback";
          validate_command ledger phase.phase_aggregate.aggregate_header;
          (match (find ledger phase.phase_aggregate.aggregate_name).source with
          | Aggregate { publication; progress = Some _; _ }
            when publication == phase.phase_aggregate -> ()
          | _ ->
              fail span "deferred offset lacks its original aggregate progress");
          if Option.is_some ledger.pending_runtime_offset then
            fail span "source offset was already deferred";
          ledger.pending_runtime_offset <- Some phase;
          record_activation_event ledger
            (Sema.Source_activation.Declaration event)
      | _ -> fail span "deferred offset requires its exact original phase event")

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
            let callback =
              declared_callback_for ledger span
                publication.global_function_pointer
            in
            let declaration =
              Sema.Compiler_record.declare_global ?callback
                ~selected_aggregate:(selected_aggregate_for ledger)
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
                | Aggregate state -> state.publication.aggregate_header
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
            List.iter
              (fun (item_index, item) ->
                match item with
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
                          || not
                               (same_option ( == ) header.closing_parenthesis
                                  prototype.closing_parenthesis)
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
                | ( Ast.Aggregate_forward_declaration _
                  | Ast.Aggregate_definition _ ) as item -> (
                    let name, kind =
                      match item with
                      | Ast.Aggregate_forward_declaration source ->
                          (source.name, Collection.Aggregate_forward)
                      | Ast.Aggregate_definition source ->
                          (source.name, Collection.Aggregate_definition)
                      | _ -> assert false
                    in
                    let assigned = find ledger name in
                    match assigned.source with
                    | Aggregate { completed = Some receipt; record; _ }
                      when receipt.aggregate_item == item ->
                        (match (ledger.authority, record) with
                        | Task_runtime _, Some (Error message) ->
                            fail ~code:"HCRUN0001" name.location.span message
                        | Task_runtime _, None ->
                            fail name.location.span
                              "aggregate has no original preparation"
                        | _ -> ());
                        add item_index kind name assigned
                    | _ ->
                        fail name.location.span
                          "aggregate command lacks its original completed \
                           declaration"))
              (Ast.declaration_items ast);
            (match ledger.authority with
            | Source_compilation _ ->
                List.iter
                  (fun assigned ->
                    match assigned.source with
                    | Function state
                      when Parser.context_mode
                             state.publication.function_header
                               .declaration_command
                               .command_context
                           = Frontend.Preprocessor.Aot ->
                        List.iter
                          (fun receipt ->
                            match receipt.Parser.default_ast.value with
                            | Ast.Lastclass_default _ -> ()
                            | Ast.Expression_default _ ->
                                if
                                  not
                                    (List.exists
                                       (fun value ->
                                         Ir.Prepared_parameter_default.receipt
                                           value
                                         == receipt)
                                       ledger.prepared_source_defaults)
                                then
                                  fail receipt.default_ast.location.span
                                    "output source seal requires every \
                                     original default preparation and header \
                                     publication")
                          state.defaults_rev
                    | _ -> ())
                  !claimed
            | Semantic_analysis | Task_runtime _ -> ());
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
                calls =
                  List.filter_map
                    (fun call ->
                      if
                        List.exists
                          (fun entry ->
                            entry.receipt.command_start
                            == Parser.selected_command call.start.call_reference)
                          original_commands
                      then call.runtime_phase
                      else None)
                    ledger.calls
                  @ List.filter_map
                      (fun call ->
                        if
                          List.exists
                            (fun entry ->
                              entry.receipt.command_start
                              == Parser.implicit_command call.implicit_selection)
                            original_commands
                        then call.implicit_phase
                        else None)
                      ledger.implicit_outputs;
                namespace = ledger.namespace;
                function_compiler_options =
                  List.filter_map
                    (fun assigned ->
                      match assigned.source with
                      | Function
                          { header = Some header; body_compiler_options; _ } ->
                          Some
                            ( Collection.publication_symbol assigned.publication,
                              Option.value body_compiler_options
                                ~default:header.header_compiler_options )
                      | _ -> None)
                    !claimed;
                inherited_metadata =
                  List.filter_map
                    (fun assigned ->
                      match assigned.source with
                      | Aggregate
                          {
                            completed =
                              Some
                                {
                                  aggregate_item =
                                    Ast.Aggregate_definition definition;
                                  _;
                                };
                            record = Some (Ok record);
                            _;
                          }
                        when Option.is_some definition.base ->
                          Some
                            (Sema.Compiler_record.retain_inherited_metadata
                               ~table:ledger.table ~namespace:ledger.namespace
                               definition record
                            |> checked definition.location.span)
                      | _ -> None)
                    !claimed;
                static_allocations =
                  List.rev ledger.static_allocations_rev
                  |> List.filter (fun allocation ->
                      List.exists
                        (fun assigned ->
                          assigned.publication
                          == Sema.Compiler_record.static_allocation_publication
                               allocation)
                        !claimed);
                selected_aggregate_types =
                  Type_specifiers.copy ledger.selected_aggregate_types;
                function_headers =
                  List.filter_map
                    (fun assigned ->
                      match assigned.source with
                      | Function
                          {
                            declared_header = Some source;
                            typed_header = Some (collected, typed);
                            _;
                          } -> Some (source, collected, typed)
                      | _ -> None)
                    !claimed;
                implicit_outputs =
                  List.filter
                    (fun original ->
                      List.exists
                        (fun entry ->
                          entry.receipt.command_start
                          == Parser.implicit_command original.implicit_selection)
                        original_commands)
                    ledger.implicit_outputs;
                source_callback_defaults =
                  List.filter
                    (fun value ->
                      let owner =
                        (Ir.Prepared_callback_default.header value)
                          .Parser.callback_signature_publication
                      in
                      List.exists
                        (fun entry ->
                          entry.receipt.command_start == owner.callback_command)
                        original_commands)
                    ledger.prepared_source_callback_defaults;
                native_source_callback_defaults =
                  List.filter
                    (fun value ->
                      let publication =
                        (Ir.Prepared_callback_default.header value)
                          .Parser.callback_signature_publication
                      in
                      List.exists
                        (fun entry ->
                          entry.receipt.command_start
                          == publication.callback_command)
                        original_commands
                      && List.exists
                           (fun (receipt, _, owner, _, bits) ->
                             owner = Native_source_default
                             && receipt
                                == Ir.Prepared_callback_default.receipt value
                             && !bits
                                = Ir.Prepared_callback_default.word_bits value)
                           ledger.source_callback_attempts)
                    ledger.prepared_source_callback_defaults;
                source_defaults =
                  List.filter
                    (fun value ->
                      List.exists
                        (fun assigned ->
                          assigned.publication
                          == Ir.Prepared_parameter_default.publication value)
                        !claimed)
                    ledger.prepared_source_defaults;
                native_source_defaults =
                  List.filter
                    (fun value ->
                      List.exists
                        (fun assigned ->
                          assigned.publication
                          == Ir.Prepared_parameter_default.publication value)
                        !claimed
                      && List.exists
                           (fun (receipt, _, owner, _, bits) ->
                             owner = Native_source_default
                             && receipt
                                == Ir.Prepared_parameter_default.receipt value
                             && !bits
                                = Ir.Prepared_parameter_default.word_bits value
                             && Option.is_some !bits)
                           ledger.source_default_attempts)
                    ledger.prepared_source_defaults;
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
                       | Function _ | Aggregate _ -> ())
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
                switches =
                  List.filter
                    (fun prepared ->
                      List.exists
                        (fun entry ->
                          entry.receipt.command_start == Switch.command prepared)
                        original_commands)
                    (Switch.preparations ledger.switch_tracker);
                offsets =
                  List.filter
                    (fun offset ->
                      let phase =
                        Sema.Compiler_record.aggregate_offset_phase offset
                      in
                      List.exists
                        (fun assigned ->
                          match assigned.source with
                          | Aggregate state ->
                              state.publication == phase.phase_aggregate
                          | _ -> false)
                        !claimed)
                    ledger.offsets_rev;
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

let native_initializer_source ledger ~runtime ~closed receipt =
  let ( let* ) = Result.bind in
  let* leaf = initializer_leaf_for ledger receipt in
  protect (fun () ->
      let publication = receipt.Parser.leaf_initializer.initializer_owner in
      let span = receipt.leaf_initializer.initializer_equals.span in
      (match ledger.authority with
      | Source_compilation _ when Parser.initializer_leaf_is_current receipt ->
          ()
      | _ ->
          fail span "native initializer requires its original source callback");
      validate_command ledger publication.global_header;
      if not (VM.task_owns_table runtime ledger.table) then
        fail span "native initializer has another semantic table";
      (match ledger.source_defaults_runtime with
      | Some prior when prior != runtime ->
          fail span "native initializer has another invocation budget"
      | _ -> ());
      if List.exists (( == ) leaf) ledger.native_initializer_attempts then
        fail span "native initializer was already attempted";
      if closed && Sema.Initializer_source.leaf_identifier_nodes leaf <> [] then
        fail ~code:"HCRUN0006" span
          "native initializers require closed expressions without value or \
           function references";
      let boundary =
        Names.find ledger.storage_boundaries publication.global_name
      in
      let declaration =
        match boundary.storage_declaration with
        | Some declaration -> declaration
        | None ->
            let dimensions =
              selected_dimensions ledger publication.global_dimensions
            in
            if
              List.length dimensions
              <> List.length publication.global_dimensions
            then
              fail ~code:"HCRUN0001" span
                "native array initializer requires every original dimension to \
                 have a checked fixed bound";
            let assigned = find ledger publication.global_name in
            let callback =
              declared_callback_for ledger span
                publication.global_function_pointer
            in
            let declaration =
              Sema.Compiler_record.declare_global ?callback
                ~selected_aggregate:(selected_aggregate_for ledger)
                ~dimensions ~table:ledger.table ~namespace:ledger.namespace
                ~predecessor:boundary.storage_predecessor
                ~previous_global:boundary.storage_previous_global
                assigned.publication
              |> checked span
            in
            boundary.storage_declaration <- Some declaration;
            declaration
      in
      ledger.native_initializer_attempts <-
        leaf :: ledger.native_initializer_attempts;
      ledger.source_defaults_runtime <- Some runtime;
      (declaration, leaf))

let native_load_initializer_source ledger ~runtime receipt =
  let context =
    receipt.Parser.leaf_initializer.initializer_owner.global_header
      .declaration_command
      .command_context
  in
  if Parser.context_mode context <> Frontend.Preprocessor.Aot then
    protect (fun () ->
        fail ~code:"HCRUN0006" receipt.leaf_initializer.initializer_equals.span
          "native load initializer requires its original AOT callback")
  else native_initializer_source ledger ~runtime ~closed:false receipt

let native_initializer_fragment ledger ~runtime receipt =
  let ( let* ) = Result.bind in
  let* declaration, leaf =
    native_initializer_source ledger ~runtime ~closed:true receipt
  in
  protect (fun () ->
      let publication = receipt.Parser.leaf_initializer.initializer_owner in
      let span = receipt.leaf_initializer.initializer_equals.span in
      let module Outer = Sema.Outer_environment in
      let compilation_mode, tables =
        match
          Parser.context_mode
            publication.global_header.declaration_command.command_context
        with
        | Frontend.Preprocessor.Aot -> (Outer.Aot, [ (Outer.Assembler, 0) ])
        | Frontend.Preprocessor.Jit ->
            (Outer.Jit, [ (Outer.Jit_task 0, 0); (Outer.Assembler, 1) ])
      in
      let tables =
        List.map
          (fun (table_kind, table_index) ->
            Outer.make_table ~table_kind ~table_index []
            |> Result.map_error Outer.error_to_string
            |> checked span)
          tables
      in
      let environment =
        Outer.create ~table:ledger.table ~compilation_mode tables
        |> Result.map_error Outer.error_to_string
        |> checked span
      in
      let queries =
        Sema.Query_selection.source_queries
          (Sema.Initializer_source.leaf_expression_ast leaf)
        |> List.map (fun expression ->
            match Query_expressions.find_opt ledger.queries expression with
            | Some query -> query.query_selection
            | None -> fail span "native initializer lacks its original query")
      in
      let fragment =
        Sema.Initializer_fragment.create_native_closed ~table:ledger.table
          ~declaration ~leaf ~environment ~queries
        |> checked span
      in
      let authority =
        Sema.Initializer_fragment.authorize ~namespace:ledger.namespace fragment
        |> checked span
      in
      authority)

let declare_static_symbol ledger ~runtime receipt =
  protect (fun () ->
      let publication = receipt.Parser.allocation_function in
      let span = publication.function_name.location.span in
      if
        not
          (Option.fold ~none:false ~some:(( == ) runtime)
             (ledger_runtime ledger))
      then fail span "static symbol belongs to another task runtime";
      if
        not (Sema.Source_activation.static_allocation ledger.activation receipt)
      then validate_command ledger publication.function_header;
      if receipt.allocation_storage <> Ast.Static_local then
        fail span "native static symbol requires original static storage";
      let allocation =
        match
          Sema.Compiler_record.static_allocation ledger.compiler_positions
            receipt
        with
        | Some allocation -> allocation
        | None -> fail span "native static symbol has no observed allocation"
      in
      let partial =
        match (find ledger publication.function_name).source with
        | Function state when state.publication == publication -> (
            match state.typed_header with
            | Some (collected, _) -> collected
            | None ->
                fail span "native static symbol has no original partial header")
        | _ -> fail span "native static symbol has another function owner"
      in
      let symbol =
        Sema.Function_collection.declare_static ?activation:ledger.activation
          ~table:ledger.table partial allocation
        |> checked span
      in
      let storage =
        let callback =
          match receipt.allocation_local.local_source with
          | Parser.Local_variable source ->
              declared_callback_for ledger span source.local_function_pointer
          | _ -> None
        in
        Ir.Integer_static_allocation.create ?activation:ledger.activation
          ?callback
          ~selected_aggregate:(selected_aggregate_for ledger)
          ~table:ledger.table ~header:partial allocation
        |> checked span
      in
      if Ir.Integer_static_allocation.symbol storage != symbol then
        fail span "private static storage substituted its original symbol";
      VM.admit_static_allocation runtime storage |> checked span;
      storage)

let declare_native_static_symbol = declare_static_symbol

let native_static_initializer_fragment ledger ~runtime
    (receipt : Parser.static_initializer_preparation) =
  protect (fun () ->
      let publication = receipt.Parser.static_allocation.allocation_function in
      let span = publication.function_name.location.span in
      (match ledger.authority with
      | Source_compilation _ when Parser.static_initializer_is_current receipt
        -> ()
      | _ ->
          fail span
            "native static initializer requires its original source callback");
      validate_command ledger publication.function_header;
      if not (VM.task_owns_table runtime ledger.table) then
        fail span "native static initializer has another semantic table";
      (match ledger.source_defaults_runtime with
      | Some prior when prior != runtime ->
          fail span "native static initializer has another invocation budget"
      | _ -> ());
      if
        (not (List.exists (( == ) receipt) ledger.static_preparations))
        || List.exists (( == ) receipt) ledger.native_static_attempts
      then
        fail span "native static initializer is unobserved or already attempted";
      let assigned = find ledger publication.function_name in
      let module Outer = Sema.Outer_environment in
      let compilation_mode, tables =
        match
          Parser.context_mode
            publication.function_header.declaration_command.command_context
        with
        | Frontend.Preprocessor.Aot -> (Outer.Aot, [ (Outer.Assembler, 0) ])
        | Frontend.Preprocessor.Jit ->
            (Outer.Jit, [ (Outer.Jit_task 0, 0); (Outer.Assembler, 1) ])
      in
      let tables =
        List.map
          (fun (table_kind, table_index) ->
            Outer.make_table ~table_kind ~table_index []
            |> Result.map_error Outer.error_to_string
            |> checked span)
          tables
      in
      let environment =
        Outer.create ~table:ledger.table ~compilation_mode tables
        |> Result.map_error Outer.error_to_string
        |> checked span
      in
      let expression =
        match receipt.static_leaf_value with
        | Ast.Scalar_initializer expression -> expression
        | _ ->
            fail ~code:"HCRUN0004" span
              "native static initializer receipt is not an original source leaf"
      in
      let queries =
        Sema.Query_selection.source_queries expression
        |> List.map (fun expression ->
            match Query_expressions.find_opt ledger.queries expression with
            | Some query -> query.query_selection
            | None ->
                fail span "native static initializer lacks its original query")
      in
      let fragment =
        let callback =
          match receipt.static_allocation.allocation_local.local_source with
          | Parser.Local_variable source ->
              declared_callback_for ledger span source.local_function_pointer
          | _ -> None
        in
        let dimensions =
          match receipt.static_allocation.allocation_local.local_source with
          | Parser.Local_variable source ->
              selected_dimensions ledger source.local_array_dimensions
              |> List.map Sema.Compiler_record.dimension_count
          | _ -> []
        in
        Sema.Static_initializer_fragment.create ?callback
          ~selected_aggregate:(selected_aggregate_for ledger)
          ~table:ledger.table ~namespace:ledger.namespace
          ~publication:assigned.publication ~receipt ~dimensions ~environment
          ~queries ()
        |> function
        | Error message when String.starts_with ~prefix:"HCRUN0001: " message ->
            fail ~code:"HCRUN0001" span
              (String.sub message 11 (String.length message - 11))
        | Error message when String.starts_with ~prefix:"HCRUN0006: " message ->
            fail ~code:"HCRUN0006" span
              (String.sub message 11 (String.length message - 11))
        | result -> checked span result
      in
      ledger.native_static_attempts <- receipt :: ledger.native_static_attempts;
      ledger.source_defaults_runtime <- Some runtime;
      fragment)

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

let selected_fragment_transcript ?resolve_local ledger ~task_view ~span
    expression =
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
          | Some ({ target; _ } as original) -> (
              match target with
              | Selected_absent -> Selection.absent ~table ~name |> checked span
              | Selected_unbound _ | Selected_source { admitted = None; _ } ->
                  Selection.unavailable ~table ~name |> checked span
              | Selected_local -> (
                  match resolve_local with
                  | Some resolve -> resolve identifier original
                  | None ->
                      fail span "global initializer selected a local reference")
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

let task_static_fragment ledger ~runtime ~task_view receipt =
  protect (fun () ->
      let publication = receipt.Parser.static_allocation.allocation_function in
      let span = publication.function_name.location.span in
      (match ledger.authority with
      | Task_runtime owner when owner == runtime -> ()
      | _ ->
          fail span
            "native static initializer requires its original task authority");
      if
        (not
           (Parser.static_initializer_is_current receipt
           || Sema.Source_activation.static_initializer ledger.activation
                receipt))
        || (not
              (Option.fold ~none:false ~some:(( == ) runtime)
                 (ledger_runtime ledger)))
        || (not (VM.task_owns_snapshot runtime task_view))
        || (not (List.exists (( == ) receipt) ledger.static_preparations))
        || List.exists (( == ) receipt) ledger.native_static_attempts
      then
        fail span
          "native task static initializer is foreign, unobserved or already \
           attempted";
      if
        not
          (Sema.Source_activation.static_initializer ledger.activation receipt)
      then validate_command ledger publication.function_header;
      let allocation =
        match
          List.find_opt
            (fun allocation ->
              Ir.Integer_static_allocation.source allocation
              |> Sema.Compiler_record.static_allocation_receipt
              |> fun original -> original == receipt.static_allocation)
            (Ir.Integer_globals.private_static_allocations task_view)
        with
        | Some allocation -> allocation
        | None ->
            fail span
              "native static initializer has no admitted original allocation"
      in
      let assigned = find ledger publication.function_name in
      let expression =
        match receipt.static_leaf_value with
        | Ast.Scalar_initializer expression -> expression
        | _ ->
            fail span "native static initializer is not an original scalar leaf"
      in
      let resolve_local (identifier : Ast.identifier) original =
        let selected = Parser.selected_local original.selection in
        let storage =
          List.find_opt
            (fun storage ->
              let original =
                Ir.Integer_static_allocation.source storage
                |> Sema.Compiler_record.static_allocation_receipt
              in
              original.allocation_function == publication
              && Option.fold ~none:false
                   ~some:(( == ) original.allocation_local)
                   selected)
            (Ir.Integer_globals.private_static_allocations task_view)
        in
        let storage =
          match storage with
          | Some storage -> storage
          | None ->
              fail ~code:"HCRUN0006" span
                "native static initializer cannot read automatic or parameter \
                 storage"
        in
        let header =
          match assigned.source with
          | Function state -> (
              match state.typed_header with
              | Some (header, _) -> header
              | None ->
                  fail span "static reference has no original partial header")
          | _ -> fail span "static reference has another declaring function"
        in
        let reference =
          Sema.Static_reference.create ~table:ledger.table ~header
            ?callback:(Ir.Integer_static_allocation.callback_source storage)
            ~selected_aggregate:(selected_aggregate_for ledger)
            ~allocation:(Ir.Integer_static_allocation.source storage)
            ~selection:original.selection ()
          |> checked span
        in
        Sema.Reference_selection.static_local ~table:ledger.table
          ~name:identifier.Ast.spelling reference
        |> checked span
      in
      let environment, references, queries =
        selected_fragment_transcript ~resolve_local ledger ~task_view ~span
          expression
      in
      let fragment =
        Sema.Static_initializer_fragment.create_selected
          ?activation:ledger.activation
          ?callback:(Ir.Integer_static_allocation.callback_source allocation)
          ~selected_aggregate:(selected_aggregate_for ledger)
          ~table:ledger.table ~namespace:ledger.namespace
          ~publication:assigned.publication ~receipt
          ~dimensions:
            (Ir.Integer_storage_shape.dimensions
               (Ir.Integer_static_allocation.shape allocation))
          ~environment ~references ~queries ()
        |> checked span
      in
      ledger.native_static_attempts <- receipt :: ledger.native_static_attempts;
      (allocation, fragment))

let native_task_static_fragment = task_static_fragment

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

let runtime_dimension_pending ledger ~runtime receipt =
  let span = receipt.Parser.dimension_opening.span in
  require_initializer_runtime ledger runtime span;
  validate_dimension_owner ledger receipt.dimension_owner;
  if not (Parser.dimension_preparation_is_current receipt) then
    fail span "runtime dimension callback is not current";
  match
    Names.find_opt ledger.dimension_owners
      receipt.dimension_owner.dimensions_name
  with
  | Some state when state.owner == receipt.dimension_owner -> (
      match state.pending with
      | Some pending when pending.preparation == receipt -> pending
      | _ ->
          fail span "runtime dimension lacks its original pending preparation")
  | _ -> fail span "runtime dimension has another prospective owner"

let runtime_offset_progress ledger ~runtime phase =
  let span = phase.Parser.phase_location.span in
  require_initializer_runtime ledger runtime span;
  if not (Parser.aggregate_phase_is_current phase) then
    fail span "runtime offset is outside its original callback";
  validate_command ledger phase.phase_aggregate.aggregate_header;
  match (find ledger phase.phase_aggregate.aggregate_name).source with
  | Aggregate { publication; progress = Some progress; _ }
    when publication == phase.phase_aggregate -> progress
  | _ -> fail span "runtime offset has no original aggregate progress"

let begin_runtime_offset ledger ~runtime ~task_view phase =
  protect (fun () ->
      let span = phase.Parser.phase_location.span in
      let progress = runtime_offset_progress ledger ~runtime phase in
      if not (VM.task_owns_snapshot runtime task_view) then
        fail span "runtime offset has another task snapshot";
      let expression =
        match phase.phase_step with
        | Parser.Aggregate_offset_reached expression -> expression
        | _ -> fail span "runtime offset requires an offset expression"
      in
      let environment, references, queries =
        selected_fragment_transcript ledger ~task_view ~span expression
      in
      let fragment =
        Sema.Offset_fragment.create ~table:ledger.table
          ~namespace:ledger.namespace ~progress ~receipt:phase ~environment
          ~references ~queries
        |> checked span
      in
      let authority = Sema.Offset_fragment.authorize fragment |> checked span in
      let attempt = VM.begin_task_offset runtime authority |> checked span in
      ledger.runtime_offset_completions <-
        (phase, VM.task_initializer_steps runtime, ref false)
        :: ledger.runtime_offset_completions;
      (authority, attempt))

let finish_runtime_offset ledger ~runtime ~before ~succeeded phase =
  protect (fun () ->
      let span = phase.Parser.phase_location.span in
      let progress = runtime_offset_progress ledger ~runtime phase in
      let consumed =
        match
          List.find_opt
            (fun (original, _, _) -> original == phase)
            ledger.runtime_offset_completions
        with
        | Some (_, original_before, consumed)
          when original_before = before && not !consumed -> consumed
        | _ ->
            fail span
              "runtime offset completion is foreign, repeated or unprepared"
      in
      consumed := true;
      let work = VM.task_initializer_steps runtime - before in
      if work < 0 then fail span "runtime offset work moved backward";
      ledger.offset_work <- ledger.offset_work + work;
      if succeeded then (
        let offset =
          match VM.task_offset runtime phase with
          | Some offset
            when Sema.Compiler_record.aggregate_offset_work offset = work ->
              offset
          | _ ->
              fail span
                "runtime offset has no original successful typed execution"
        in
        ledger.offsets_rev <- offset :: ledger.offsets_rev;
        Sema.Compiler_record.advance_aggregate
          ~callbacks:(completed_callback_header ledger)
          ~dimensions:(Dimensions.find_opt ledger.checked_dimensions)
          progress phase
        |> checked span;
        match (find ledger phase.phase_aggregate.aggregate_name).source with
        | Aggregate state ->
            state.record <-
              Some (Sema.Compiler_record.aggregate_metadata progress)
        | _ -> assert false))

let begin_runtime_internal_binding ledger ~runtime ~task_view receipt =
  protect (fun () ->
      let span = receipt.Parser.binding_ast.location.span in
      require_initializer_runtime ledger runtime span;
      if not (List.exists (( == ) receipt) ledger.internal_bindings) then
        fail span "internal binding lacks its original observed preparation";
      let expression =
        match receipt.binding_ast.target with
        | Ast.Expression_binding_target expression -> expression
        | _ -> fail span "internal binding lacks its original expression"
      in
      let environment, references, queries =
        selected_fragment_transcript ledger ~task_view ~span expression
      in
      let fragment =
        Sema.Internal_binding_fragment.create ~table:ledger.table
          ~namespace:ledger.namespace ~receipt ~environment ~references ~queries
        |> checked span
      in
      let authority =
        Sema.Internal_binding_fragment.authorize fragment |> checked span
      in
      let attempt =
        VM.begin_task_internal_binding runtime authority |> checked span
      in
      (authority, attempt))

let finish_runtime_internal_binding ledger ~runtime ~succeeded receipt =
  protect (fun () ->
      let span = receipt.Parser.binding_ast.location.span in
      require_initializer_runtime ledger runtime span;
      if succeeded then
        match VM.task_internal_binding_target runtime receipt with
        | Some target
          when not
                 (List.exists (( == ) target) ledger.prepared_internal_bindings)
          ->
            ledger.prepared_internal_bindings <-
              target :: ledger.prepared_internal_bindings
        | _ ->
            fail span "internal binding lacks its successful original execution")

let begin_runtime_dimension ledger ~runtime ~task_view receipt =
  protect (fun () ->
      let span = receipt.Parser.dimension_opening.span in
      let pending = runtime_dimension_pending ledger ~runtime receipt in
      (match pending.evaluation with
      | Awaiting_runtime_dimension -> ()
      | _ ->
          fail span "runtime dimension was already attempted or is not pending");
      pending.evaluation <- Failed_dimension;
      let expression = Option.get receipt.dimension_expression in
      let environment, references, queries =
        selected_fragment_transcript ledger ~task_view ~span expression
      in
      let fragment =
        Sema.Dimension_fragment.create ~table:ledger.table
          ~namespace:ledger.namespace ~receipt ~environment ~references ~queries
        |> checked span
      in
      let authority =
        Sema.Dimension_fragment.authorize fragment |> checked span
      in
      let before = VM.task_initializer_steps runtime in
      let attempt = VM.begin_task_dimension runtime authority |> checked span in
      pending.evaluation <- Executing_dimension (attempt, before);
      (authority, attempt))

let finish_runtime_dimension ledger ~runtime ~succeeded receipt =
  protect (fun () ->
      let span = receipt.Parser.dimension_opening.span in
      let pending = runtime_dimension_pending ledger ~runtime receipt in
      let before =
        match pending.evaluation with
        | Executing_dimension (_, before) -> before
        | _ ->
            fail span "runtime dimension completion lacks its original attempt"
      in
      pending.evaluation <- Failed_dimension;
      let work = VM.task_initializer_steps runtime - before in
      if work < 0 then
        fail span "runtime dimension preparation counter moved backward";
      ledger.dimension_work <- ledger.dimension_work + work;
      if succeeded then
        let count =
          match VM.task_dimension_bits runtime receipt with
          | Some bits -> bits
          | None ->
              fail span "runtime dimension has no successful original execution"
        in
        let queries =
          Sema.Query_selection.source_queries
            (Option.get receipt.dimension_expression)
          |> List.map (fun expression ->
              match Query_expressions.find_opt ledger.queries expression with
              | Some query ->
                  Sema.Query_selection.checked_read query.query_selection
              | None ->
                  fail span "runtime dimension lost its original checked query")
        in
        let prepared =
          Sema.Compiler_record.propose_runtime_dimension
            ~namespace:ledger.namespace ~preparation:receipt ~count ~work
          |> checked span
        in
        pending.evaluation <- Proposed_runtime_dimension (prepared, queries))

let declared_function_header ledger header =
  protect (fun () ->
      let span =
        header.Parser.function_publication.function_name.location.span
      in
      match (find ledger header.function_publication.function_name).source with
      | Function
          { header = Some original; declared_header = Some declaration; _ }
        when original == header
             && Sema.Compiler_record.declared_function_source declaration
                == header -> declaration
      | _ ->
          fail span
            "completed function header lacks its original observed source \
             authority")

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

let retained_function_headers ~table ~ast (command : command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span "retained headers belong to another source command";
      (command.namespace, command.function_headers))

let function_compiler_options ~table ~ast (command : command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span "function options belong to another source command";
      fun symbol ->
        if not (Sema.Symbol_table.owns_symbol table symbol) then
          Error "function options have another symbol table"
        else
          match
            List.find_opt
              (fun (original, _) -> original == symbol)
              command.function_compiler_options
          with
          | Some (_, mask) -> Ok mask
          | None -> Error "function has no original reached option snapshot")

let source_function_compiler_options ~table ~ast (Source_command command) =
  function_compiler_options ~table ~ast command

let selected_type_resolver ~table ~ast (command : command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span "selected types belong to another source command";
      ( command.namespace,
        fun type_specifier ->
          Type_specifiers.find_opt command.selected_aggregate_types
            type_specifier ))

let source_selected_type_resolver ~table ~ast (Source_command command) =
  selected_type_resolver ~table ~ast command

let inherited_metadata ~table ~ast (command : command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "inherited metadata belongs to another original source command";
      command.inherited_metadata)

let source_inherited_metadata ~table ~ast (Source_command command) =
  inherited_metadata ~table ~ast command

let native_publication_event = function
  | Parser.Function_declared p -> Some p
  | Parser.Function_parameter_declared p -> Some p.parameter_function
  | Parser.Function_parameter_completed p ->
      Some p.parameter_publication.parameter_function
  | Parser.Parameter_default_completed p -> Some p.default_function
  | Parser.Function_variadic_started p | Parser.Function_variadic_completed p ->
      Some p.variadic_function
  | _ -> None

let admit_function_phase ledger ~runtime event =
  protect (fun () ->
      match native_publication_event event with
      | None -> ()
      | Some publication -> (
          let span = publication.function_name.location.span in
          require_initializer_runtime ledger runtime span;
          let assigned = find ledger publication.function_name in
          match assigned.source with
          | Function state when state.publication == publication ->
              let eligible =
                match publication.function_header.binding with
                | None -> true
                | Some
                    {
                      Ast.kind = Ast.Extern;
                      spelling = "extern";
                      target = Ast.No_binding_target;
                      _;
                    } -> true
                | _ ->
                    Sema.Function_record_phase.has_literal_internal_target
                      publication
                    || List.exists
                         (fun target ->
                           Sema.Prepared_internal_binding.matches_header target
                             publication.function_header)
                         ledger.prepared_internal_bindings
              in
              if eligible && Option.is_some state.native_record then
                let snapshot =
                  require_function_record_snapshot ledger publication
                in
                if
                  Sema.Function_record_phase.unavailable_reason snapshot
                  <> Some "previous native function record is untracked"
                then (
                  VM.check_function_phase_source runtime
                    ~namespace:ledger.namespace ~event snapshot
                  |> checked span;
                  let compiler_option_mask =
                    Parser.context_compiler_options
                      publication.function_header.declaration_command
                        .command_context
                    |> checked span
                  in
                  let module R = Sema.Function_resolution in
                  let module C = Sema.Function_record_classification in
                  let current =
                    VM.function_record_head runtime snapshot
                    |> Option.map (fun reference ->
                        Ir.Retained_function.metadata reference
                        |> Sema.Outer_environment
                           .function_classified_declaration)
                  in
                  let scope =
                    Option.map
                      (fun prior ->
                        C.classified_declaration_source prior
                        |> R.resolved_declaration_site
                        |> R.declaration_site_function
                        |> Sema.Function_type_resolution.function_scope)
                      state.runtime_phase
                  in
                  let shape =
                    Sema.Function_record_phase.call_shape snapshot
                    |> checked span
                  in
                  let function_ =
                    Function_type_resolution.resolve_provisional_call ?scope
                      ~selected_aggregate:(selected_aggregate_for ledger)
                      ~table:ledger.table ~namespace:ledger.namespace shape
                    |> checked span
                  in
                  let fact =
                    match current with
                    | None ->
                        R.make_provisional_declaration ~table:ledger.table
                          ~namespace:ledger.namespace ~compiler_option_mask
                          ~function_
                    | Some current ->
                        let current = C.classified_declaration_source current in
                        let earlier =
                          R.resolved_declaration_site current
                          |> R.declaration_site_native_snapshot |> Option.get
                        in
                        let transition =
                          Sema.Function_record_phase.transition ~earlier
                            ~later:snapshot
                          |> checked span
                        in
                        let pending =
                          Option.map C.classified_declaration_source
                            state.runtime_phase
                        in
                        R.make_provisional_advance ?pending ~table:ledger.table
                          ~namespace:ledger.namespace ~compiler_option_mask
                          ~current ~transition ~function_ ()
                  in
                  let fact = fact |> checked span in
                  let previous = Option.to_list current in
                  let resolution =
                    R.resolve
                      ~previous:
                        (List.map C.classified_declaration_source previous)
                      ~table:ledger.table
                      ~parent:(Collection.namespace_scope ledger.namespace)
                      ~compilation_mode:R.Jit [ fact ]
                    |> checked span
                  in
                  let records =
                    Function_record_classification.classify_publication
                      ~previous ~resolution publication
                    |> checked span
                  in
                  VM.admit_function_phase runtime ~namespace:ledger.namespace
                    ~event ~snapshot ~records
                  |> checked span;
                  state.runtime_phase <- Some (List.hd (C.declarations records)))
          | _ ->
              fail span "native phase lacks its original function publication"))

let admit_function_header ledger ~runtime header =
  let ( let* ) = Result.bind in
  let* source = declared_function_header ledger header in
  protect (fun () ->
      let span =
        header.Parser.function_publication.function_name.location.span
      in
      require_initializer_runtime ledger runtime span;
      VM.check_function_header_source runtime ~namespace:ledger.namespace source
      |> checked span;
      let assigned = find ledger header.function_publication.function_name in
      let pending_native =
        match assigned.source with
        | Function state -> state.runtime_phase
        | _ -> None
      in
      let function_ =
        match assigned.source with
        | Function state -> (
            match state.typed_header with
            | Some (_, typed) -> typed
            | None ->
                let pair =
                  Function_type_resolution
                  .resolve_completed_header_with_collection ~table:ledger.table
                    ~namespace:ledger.namespace
                    ~selected_aggregate:(selected_aggregate_for ledger)
                    source
                  |> checked span
                in
                state.typed_header <- Some pair;
                snd pair)
        | _ -> fail span "completed header has another declaration kind"
      in
      let view = VM.task_snapshot runtime |> checked span in
      let module Outer = Sema.Outer_environment in
      let previous =
        match Outer.tables (Ir.Integer_globals.task_environment view) with
        | current :: _ ->
            List.find_map
              (fun entry ->
                if
                  Sema.Symbol.name (Outer.entry_symbol entry)
                  = Sema.Symbol.name
                      (Sema.Compiler_record.declared_function_symbol source)
                  && Sema.Symbol.Scope_id.equal
                       (Sema.Symbol.scope_id (Outer.entry_symbol entry))
                       (Sema.Symbol_table.scope_id
                          (Collection.namespace_scope ledger.namespace))
                then
                  Option.map Outer.function_classified_declaration
                    (Outer.entry_function_metadata entry)
                else None)
              (List.rev (Outer.table_entries current))
            |> Option.to_list
        | [] -> []
      in
      let native_current, fact =
        match pending_native with
        | Some pending ->
            let module R = Sema.Function_resolution in
            let module C = Sema.Function_record_classification in
            let snapshot =
              require_function_record_snapshot ledger
                header.function_publication
            in
            let current =
              match VM.function_record_head runtime snapshot with
              | Some reference ->
                  Ir.Retained_function.metadata reference
                  |> Sema.Outer_environment.function_classified_declaration
              | None ->
                  fail span
                    "completed native header has no admitted current record"
            in
            let current_declaration = C.classified_declaration_source current in
            let earlier =
              R.resolved_declaration_site current_declaration
              |> R.declaration_site_native_snapshot |> Option.get
            in
            let transition =
              Sema.Function_record_phase.transition ~earlier ~later:snapshot
              |> checked span
            in
            let callable_function =
              Sema.Function_record_phase.call_shape snapshot
              |> checked span
              |> Function_type_resolution.resolve_provisional_call
                   ~scope:
                     (Sema.Function_type_resolution.function_scope function_)
                   ~selected_aggregate:(selected_aggregate_for ledger)
                   ~table:ledger.table ~namespace:ledger.namespace
              |> checked span
            in
            ( Some current,
              R.make_header_advance ~table:ledger.table
                ~namespace:ledger.namespace
                ~pending:(C.classified_declaration_source pending)
                ~current:current_declaration ~transition ~source ~function_
                ~callable_function )
        | None ->
            ( None,
              Sema.Function_resolution.make_pending_declaration
                ~table:ledger.table ~namespace:ledger.namespace
                ~compiler_option_mask:header.header_compiler_options ~source
                ~function_ )
      in
      let fact = fact |> checked span in
      let classified_previous =
        match native_current with
        | Some current when not (List.exists (( == ) current) previous) ->
            current :: previous
        | _ -> previous
      in
      let resolution =
        Sema.Function_resolution.resolve
          ~record_heads:
            (Option.to_list native_current
            |> List.map
                 Sema.Function_record_classification
                 .classified_declaration_source)
          ~previous:
            (List.map
               Sema.Function_record_classification.classified_declaration_source
               previous)
          ~table:ledger.table
          ~parent:(Collection.namespace_scope ledger.namespace)
          ~compilation_mode:Sema.Function_resolution.Jit [ fact ]
        |> checked span
      in
      let records =
        Function_record_classification.classify_completed_header
          ~previous:classified_previous ~resolution source
        |> checked span
      in
      VM.admit_function_header runtime ~namespace:ledger.namespace ~source
        ~records
      |> checked span;
      match assigned.source with
      | Function state when Option.is_some pending_native ->
          state.runtime_phase <-
            Some
              (List.hd
                 (Sema.Function_record_classification.declarations records))
      | _ -> ())

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
        |> (fun result ->
        Result.bind result
          (Sema.Default_fragment.with_positions
             ~compiler_positions:ledger.compiler_positions))
        |> checked span
      in
      Sema.Default_fragment.authorize ?activation:ledger.activation
        ~namespace:ledger.namespace fragment
      |> checked span)

let begin_source_default_with_owner owner ledger ~runtime receipt =
  protect (fun () ->
      let span = receipt.Parser.default_ast.location.span in
      let mode =
        Parser.context_mode
          receipt.default_function.function_header.declaration_command
            .command_context
      in
      (match ledger.authority with
      | Source_compilation _
        when owner = Native_source_default || mode = Frontend.Preprocessor.Aot
        -> ()
      | _ ->
          fail span
            "output default preparation requires its original AOT source ledger");
      if
        owner = Native_source_default
        && not (VM.task_owns_table runtime ledger.table)
      then fail span "source default preparation has another semantic table";
      if not (Parser.parameter_default_is_current receipt) then
        fail span "output default preparation is outside its original callback";
      if
        List.exists
          (fun (prior, _, _, _, _) -> prior == receipt)
          ledger.source_default_attempts
      then fail span "output default preparation was already attempted";
      (match ledger.source_defaults_runtime with
      | Some prior when prior != runtime ->
          fail span "output defaults have another invocation budget"
      | _ -> ());
      let assigned = find ledger receipt.default_function.function_name in
      (match assigned.source with
      | Function state
        when state.publication == receipt.default_function
             && state.header = None
             && Option.fold ~none:false ~some:(( == ) receipt)
                  (List.nth_opt state.defaults_rev 0) -> ()
      | _ -> fail span "output default lacks its original observed parameter");
      Option.iter
        (fun previous ->
          match previous.Parser.default_ast.value with
          | Ast.Lastclass_default _ -> ()
          | Ast.Expression_default _ ->
              if
                not
                  (List.exists
                     (fun (prior, _, prior_owner, _, value) ->
                       prior == previous && prior_owner = owner
                       && Option.is_some !value)
                     ledger.source_default_attempts)
              then
                fail span "output default requires its successful predecessor")
        receipt.default_predecessor;
      let expression =
        match receipt.default_ast.value with
        | Ast.Expression_default expression -> expression
        | Ast.Lastclass_default _ ->
            fail span "lastclass needs separate materialization"
      in
      if Sema.Initializer_source.expression_identifier_nodes expression <> []
      then
        fail ~code:"HCRUN0006" span
          (match owner with
          | Output_aot_default ->
              "AOT default references require proven output relocation and \
               callable authority"
          | Native_source_default ->
              "native defaults require closed expressions without value or \
               function references");
      let module Outer = Sema.Outer_environment in
      let compilation_mode, tables =
        match mode with
        | Frontend.Preprocessor.Aot -> (Outer.Aot, [ (Outer.Assembler, 0) ])
        | Frontend.Preprocessor.Jit ->
            (Outer.Jit, [ (Outer.Jit_task 0, 0); (Outer.Assembler, 1) ])
      in
      let tables =
        List.map
          (fun (table_kind, table_index) ->
            Outer.make_table ~table_kind ~table_index []
            |> Result.map_error Outer.error_to_string
            |> checked span)
          tables
      in
      let environment =
        Outer.create ~table:ledger.table ~compilation_mode tables
        |> Result.map_error Outer.error_to_string
        |> checked span
      in
      let queries =
        Sema.Query_selection.source_queries expression
        |> List.map (fun expression ->
            match Query_expressions.find_opt ledger.queries expression with
            | Some query -> query.query_selection
            | None ->
                fail span "output default lacks its original checked query")
      in
      let fragment =
        Sema.Default_fragment.create ~table:ledger.table
          ~publication:assigned.publication ~receipt ~environment ~references:[]
          ~queries
        |> (fun result ->
        Result.bind result
          (Sema.Default_fragment.with_positions
             ~compiler_positions:ledger.compiler_positions))
        |> checked span
      in
      let authority =
        Sema.Default_fragment.authorize ~namespace:ledger.namespace fragment
        |> checked span
      in
      ledger.source_defaults_runtime <- Some runtime;
      ledger.source_default_attempts <-
        (receipt, authority, owner, VM.task_initializer_steps runtime, ref None)
        :: ledger.source_default_attempts;
      authority)

let begin_source_default = begin_source_default_with_owner Output_aot_default

let begin_native_source_default =
  begin_source_default_with_owner Native_source_default

let finish_source_default_with_owner expected_owner ledger execution =
  protect (fun () ->
      let authority = VM.default_constant_authority execution in
      let fragment = Sema.Default_fragment.authorized_fragment authority in
      let receipt = Sema.Default_fragment.receipt fragment in
      let span = receipt.Parser.default_ast.location.span in
      if not (Parser.parameter_default_is_current receipt) then
        fail span "output default completion is delayed";
      let before, value =
        match
          List.find_opt
            (fun (prior, proof, owner, _, _) ->
              prior == receipt && proof == authority && owner = expected_owner)
            ledger.source_default_attempts
        with
        | Some (_, _, _, before, value) when !value = None -> (before, value)
        | _ -> fail span "output default completion is foreign or repeated"
      in
      (match ledger.source_defaults_runtime with
      | Some runtime
        when VM.task_initializer_steps runtime - before
             = VM.default_constant_steps execution -> ()
      | _ ->
          fail span
            "output default preparation was not charged to its owning \
             invocation");
      let runtime = Option.get ledger.source_defaults_runtime in
      let bits =
        VM.consume_default_constant runtime execution |> checked span
      in
      value := Some bits)

let finish_source_default = finish_source_default_with_owner Output_aot_default

let finish_native_source_default =
  finish_source_default_with_owner Native_source_default

let complete_source_defaults ledger header =
  protect (fun () ->
      let span =
        header.Parser.function_publication.function_name.location.span
      in
      let assigned = find ledger header.function_publication.function_name in
      (match
         (ledger.authority, assigned.source, ledger.activation_events_rev)
       with
      | ( Source_compilation _,
          Function state,
          Sema.Source_activation.Declaration
            (Parser.Function_header_completed original)
          :: _ )
        when original == header
             && Option.fold ~none:false ~some:(( == ) header) state.header
             && Parser.context_is_current
                  header.function_publication.function_header
                    .declaration_command
                    .command_context
                  ~observed_events:(List.length ledger.source_events_rev) -> ()
      | _ ->
          fail span
            "output defaults require their original current completed header");
      let values =
        List.mapi
          (fun index (parameter : Ast.function_parameter) ->
            match parameter.default with
            | Some { value = Ast.Expression_default _; _ } ->
                let receipt, bits =
                  match
                    List.find_opt
                      (fun (receipt, _, _, _, _) ->
                        receipt.Parser.default_function
                        == header.function_publication
                        && receipt.default_parameter_index = index)
                      ledger.source_default_attempts
                  with
                  | Some (receipt, _, _, _, value) when Option.is_some !value ->
                      (receipt, Option.get !value)
                  | _ ->
                      fail span
                        "output header requires every original default \
                         preparation"
                in
                if
                  List.exists
                    (fun value ->
                      Ir.Prepared_parameter_default.receipt value == receipt)
                    ledger.prepared_source_defaults
                then fail span "output defaults cannot be published twice";
                Some
                  (Ir.Prepared_parameter_default.create
                     ~publication:assigned.publication ~header ~receipt ~bits
                  |> checked span)
            | _ -> None)
          header.parameters
        |> List.filter_map Fun.id
      in
      ledger.prepared_source_defaults <-
        values @ ledger.prepared_source_defaults)

let source_defaults ~table ~ast (Source_command command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span "output defaults belong to another source seal";
      command.source_defaults)

let native_source_defaults ~table ~ast (Source_command command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span "native defaults belong to another source seal";
      command.native_source_defaults)

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
              Sema.Source_activation.create ~calls:ledger.call_journal
                ~namespace:ledger.namespace ~context
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
    | Sema.Source_activation.Call_start start ->
        protect (fun () ->
            if
              (not (Sema.Source_activation.call_start ledger.activation start))
              || not
                   (List.exists (fun call -> call.start == start) ledger.calls)
            then fail span "call start lacks its original activation event";
            let call =
              List.find (fun call -> call.start == start) ledger.calls
            in
            capture_runtime_call_start ledger call)
    | Sema.Source_activation.Call_emission receipt ->
        let* () =
          protect (fun () ->
              if
                not
                  (Sema.Source_activation.call_emission ledger.activation
                     receipt)
              then fail span "call emission lacks its original activation event")
        in
        let* _ = call_record_snapshots ledger receipt in
        protect (fun () ->
            let call =
              List.find
                (fun call -> call.start == receipt.call_start)
                ledger.calls
            in
            capture_runtime_call_emission ledger call)
    | Sema.Source_activation.Implicit_arguments selection ->
        protect (fun () ->
            if
              not
                (Sema.Source_activation.implicit_arguments ledger.activation
                   selection)
            then
              fail span
                "implicit arguments lack their original activation event";
            capture_runtime_implicit_arguments ledger
              (implicit_call ledger selection))
    | Sema.Source_activation.Implicit_emission selection ->
        protect (fun () ->
            if
              not
                (Sema.Source_activation.implicit_emission ledger.activation
                   selection)
            then
              fail span "implicit emission lacks its original activation event";
            capture_runtime_implicit_emission ledger
              (implicit_call ledger selection))
    | Sema.Source_activation.Implicit_output selection ->
        let* () =
          protect (fun () ->
              let span = (Parser.implicit_marker selection).span in
              if
                not
                  (Sema.Source_activation.implicit_output ledger.activation
                     selection)
              then fail span "implicit target is outside its activation event";
              let found = ref false in
              ledger.implicit_outputs <-
                List.map
                  (fun original ->
                    if original.implicit_selection != selection then original
                    else (
                      found := true;
                      let target =
                        match original.implicit_target with
                        | Selected_source selected ->
                            let admitted =
                              VM.admitted_publication_for_symbol runtime
                                (Collection.publication_symbol
                                   selected.publication)
                            in
                            Selected_source { selected with admitted }
                        | target -> target
                      in
                      capture_runtime_implicit_selection ledger selection target;
                      { original with implicit_target = target }))
                  ledger.implicit_outputs;
              if not !found then
                fail span "source activation lacks its original implicit target")
        in
        validate_implicit_output ledger selection ~execution:true
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
                    VM.admitted_publication_for_symbol runtime
                      (Collection.publication_symbol selected.publication)
                  in
                  Selected_source { selected with admitted }
              | target -> target
            in
            Names.replace ledger.references identifier { original with target };
            capture_runtime_reference ledger selection target;
            validate_execution_target selection target)
    | Sema.Source_activation.Declaration event ->
        let* () =
          protect (fun () ->
              match event with
              | Parser.Aggregate_advanced phase
                when Option.fold ~none:false ~some:(( == ) phase)
                       ledger.pending_runtime_offset ->
                  if not (Parser.aggregate_phase_is_current phase) then
                    fail phase.phase_location.span
                      "deferred source offset expired";
                  ledger.pending_runtime_offset <- None
              | Parser.Aggregate_advanced
                  ({ phase_step = Parser.Aggregate_offset_reached _; _ } as
                   phase) -> (
                  let before = VM.task_initializer_steps runtime in
                  let result =
                    VM.charge_source_aggregate_offset runtime phase
                  in
                  ledger.offset_work <-
                    ledger.offset_work
                    + VM.task_initializer_steps runtime
                    - before;
                  match result with
                  | Ok () -> ()
                  | Error message ->
                      let code =
                        if String.starts_with ~prefix:"HCIRVM0007:" message then
                          "HCIRVM0007"
                        else "HCRUN0004"
                      in
                      fail ~code phase.phase_location.span message)
              | Parser.Array_dimension_preparing preparation -> (
                  let deferred =
                    Option.bind
                      (Names.find_opt ledger.dimension_owners
                         preparation.dimension_owner.dimensions_name)
                      (fun state ->
                        Option.bind state.pending (fun pending ->
                            if
                              pending.preparation == preparation
                              && pending.evaluation = Deferred_runtime_dimension
                            then Some pending
                            else None))
                  in
                  match deferred with
                  | Some pending ->
                      if
                        not
                          (Sema.Source_activation.dimension_preparing
                             ledger.activation preparation)
                      then
                        fail preparation.dimension_opening.span
                          "deferred dimension lacks its original activation \
                           event";
                      pending.evaluation <- Awaiting_runtime_dimension
                  | None when ledger.source_dimensions_rev <> [] -> (
                      let before = VM.task_initializer_steps runtime in
                      let result =
                        VM.charge_source_dimension runtime preparation
                      in
                      ledger.dimension_work <-
                        ledger.dimension_work
                        + VM.task_initializer_steps runtime
                        - before;
                      match result with
                      | Ok () -> ()
                      | Error message ->
                          if String.starts_with ~prefix:"HCIRVM0007:" message
                          then
                            fail ~code:"HCIRVM0007"
                              preparation.dimension_opening.span
                              "the bounded array dimension preparation work \
                               limit was exhausted"
                          else fail preparation.dimension_opening.span message)
                  | None -> ())
              | Parser.Global_completed (publication, completed) ->
                  let boundary =
                    Names.find ledger.storage_boundaries publication.global_name
                  in
                  Option.iter
                    (fun record ->
                      Sema.Compiler_record.complete_declared_global record event
                      |> checked completed.location.span)
                    boundary.storage_declaration
              | Parser.Array_dimension_completed receipt ->
                  let prepared =
                    Dimensions.find ledger.checked_dimensions
                      receipt.dimension_ast
                  in
                  if not (VM.task_dimension_is_completed runtime receipt) then
                    VM.complete_task_dimension runtime
                      ~namespace:ledger.namespace prepared
                    |> checked receipt.dimension_ast.location.span
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

let static_allocations ~table ~ast (command : command) =
  Result.map
    (fun _ -> command.static_allocations)
    (collection ~table ~ast command)

let source_static_allocations ~table ~ast (Source_command command) =
  static_allocations ~table ~ast command

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

let switch_for ~table ~ast (command : command) (source : Ast.switch_statement) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "switch preparation seal belongs to another table or source AST";
      match
        List.find_opt
          (fun prepared -> Switch.source prepared == source)
          command.switches
      with
      | Some prepared -> prepared
      | None ->
          fail source.switch_location.span
            "switch has no original completed case preparation in this command")

let command_switch_work (command : command) =
  List.fold_left
    (fun total prepared -> total + Switch.work prepared)
    0 command.switches

let command_switch_preparation_work = command_switch_work

let checked_offset_for ~table ~ast (command : command) expression =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "aggregate offset seal belongs to another table or source AST";
      match
        List.find_opt
          (fun offset ->
            Sema.Compiler_record.aggregate_offset_expression offset
            == expression)
          command.offsets
      with
      | Some offset -> offset
      | None ->
          fail ast.span
            "aggregate offset has no original preparation in this command")

let source_checked_offset_for ~table ~ast (Source_command command) expression =
  checked_offset_for ~table ~ast command expression

let source_offset_work (Source_command command) =
  List.fold_left
    (fun total offset ->
      total + Sema.Compiler_record.aggregate_offset_work offset)
    0 command.offsets

let source_offsets (Source_command command) = command.offsets

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

let source_switch_for ~table ~ast (Source_command command) source =
  switch_for ~table ~ast command source

let source_switch_work (Source_command command) = command_switch_work command
let source_switch_preparation_work = source_switch_work

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
      | Selected_source
          { admitted = Some (VM.Admitted_function _ as publication); _ } ->
          retained name publication
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

let call_resolver ~table ~ast ~task_view (command : command) =
  protect (fun () ->
      if
        command.table != table || command.ast != ast
        || not
             (Option.fold ~none:false
                ~some:(fun runtime -> VM.task_owns_snapshot runtime task_view)
                command.runtime)
      then
        fail ast.Ast.span "original calls belong to another task or source AST";
      fun source ->
        match
          List.find_opt
            (fun phase ->
              Option.fold ~none:false ~some:(( == ) source)
                (Sema.Function_call_phase.source phase))
            command.calls
        with
        | None -> Ok None
        | Some phase
          when Option.fold ~none:false
                 ~some:(fun runtime -> VM.owns_call_phase runtime phase)
                 command.runtime -> Ok (Some phase)
        | Some _ -> Error "original call lacks its owning runtime capture")

let implicit_call_resolver ~table ~ast ~task_view (command : command) =
  protect (fun () ->
      if
        command.table != table || command.ast != ast
        || not
             (Option.fold ~none:false
                ~some:(fun runtime -> VM.task_owns_snapshot runtime task_view)
                command.runtime)
      then
        fail ast.Ast.span "original calls belong to another task or source AST";
      fun source ->
        match
          List.find_opt
            (fun phase ->
              Option.fold ~none:false ~some:(( == ) source)
                (Sema.Function_call_phase.implicit_source phase))
            command.calls
        with
        | None -> Ok None
        | Some phase
          when Option.fold ~none:false
                 ~some:(fun runtime -> VM.owns_call_phase runtime phase)
                 command.runtime -> Ok (Some phase)
        | Some _ -> Error "original call lacks its owning runtime capture")

let implicit_output_resolver ~table ~ast ~task_view (command : command) =
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
        fail ast.Ast.span "implicit output has another task snapshot")
  else
    let retained name publication =
      match publication with
      | VM.Admitted_function reference -> (
          match Globals.task_function_binding task_view reference with
          | Some binding -> Selection.outer ~table ~name ~environment ~binding
          | None -> Error "implicit target has no exact retained snapshot entry"
          )
      | _ -> Error "implicit output selected a nonfunction publication"
    in
    Ok
      (fun source_statement ->
        let matches =
          List.filter
            (fun original ->
              Option.fold ~none:false
                ~some:(fun (statement : Ast.implicit_output_statement) ->
                  statement == source_statement)
                (Parser.implicit_statement original.implicit_selection))
            command.implicit_outputs
        in
        match matches with
        | [ original ] -> (
            let name =
              match Parser.implicit_target original.implicit_selection with
              | Ast.Print_target -> "Print"
              | Ast.Put_chars_target -> "PutChars"
            in
            match original.implicit_target with
            | Selected_absent -> Selection.absent ~table ~name
            | Selected_local | Selected_unbound _ ->
                Selection.unavailable ~table ~name
            | Selected_runtime publication -> retained name publication
            | Selected_source
                { admitted = Some (VM.Admitted_function _ as publication); _ }
              -> retained name publication
            | Selected_source { publication; stage; admitted } -> (
                let symbol = Collection.publication_symbol publication in
                if
                  List.exists
                    (fun entry -> Collection.entry_symbol entry == symbol)
                    (Collection.entries declarations)
                then
                  let stage =
                    match stage with
                    | Provisional_function_selection _ ->
                        Selection.Function_declared
                    | Function_selection (_, None) ->
                        Selection.Function_header_completed
                    | Function_selection (_, Some _) ->
                        Selection.Function_body_completed
                    | _ -> Selection.Global_declared
                  in
                  Selection.source ~table ~name ~symbol ~stage
                else
                  match admitted with
                  | Some publication -> retained name publication
                  | None -> Selection.unavailable ~table ~name))
        | _ -> Error "implicit output lacks one exact observed source marker")

let parser_suspension ledger =
  match ledger.active with
  | active :: _ -> Parser.suspend_context active.context
  | [] -> Error "task has no suspended parser source"

let compiler_control_context ledger ~runtime =
  let ( let* ) = Result.bind in
  let* () =
    match ledger.authority with
    | Task_runtime original when original == runtime -> Ok ()
    | _ -> Error "compiler option request has another original source task"
  in
  let* context =
    match ledger.active with
    | current :: _ -> Ok current.context
    | [] -> Error "compiler option request has no active parser control"
  in
  let observed_events =
    List.fold_left
      (fun count event ->
        let original =
          match event with
          | Parser.Sequence_started context | Parser.Sequence_aborted context ->
              context
          | Parser.Command_started start -> start.command_context
          | Parser.Command_completed completed
          | Parser.Command_resumed completed ->
              completed.command_start.command_context
          | Parser.Sequence_completed completed -> completed.sequence_context
        in
        if original == context then count + 1 else count)
      0 ledger.source_events_rev
  in
  if
    Parser.context_environment context != ledger.symbols
    || Parser.context_sources context != ledger.sources
    || not (Parser.context_is_current context ~observed_events)
  then
    Error
      "compiler option request lacks its original fully observed parser control"
  else Ok context

let execute_compiler_option ledger ~runtime index enabled =
  Result.bind (compiler_control_context ledger ~runtime) (fun context ->
      match enabled with
      | None -> Parser.context_get_option context ~bit_index:index
      | Some value -> Parser.context_set_option context ~bit_index:index value)

let emit_compiler_warnings ledger ~runtime diagnostics =
  match diagnostics with
  | [] -> Ok ()
  | _ ->
      Result.bind (compiler_control_context ledger ~runtime) (fun context ->
          List.fold_left
            (fun result diagnostic ->
              Result.bind result (fun () ->
                  Parser.context_emit_compiler_warning context diagnostic))
            (Ok ()) diagnostics)

let saved_compiler_context ledger ~session ~suspension =
  let ( let* ) = Result.bind in
  let* context = Parser.suspension_enclosing_context suspension in
  let observed_events =
    List.fold_left
      (fun count event ->
        let original =
          match event with
          | Parser.Sequence_started original | Parser.Sequence_aborted original
            -> original
          | Parser.Command_started start -> start.command_context
          | Parser.Command_completed completed
          | Parser.Command_resumed completed ->
              completed.command_start.command_context
          | Parser.Sequence_completed completed -> completed.sequence_context
        in
        if original == context then count + 1 else count)
      0 ledger.source_events_rev
  in
  if
    ledger.session != session
    || ledger.sources != Session.sources session
    || ledger.symbols != Session.symbols session
    || ledger.table != Session.semantic_symbols session
    || Parser.context_environment context != ledger.symbols
    || (not (Parser.context_is_current context ~observed_events))
    || not
         (match ledger.active with
         | active :: _ -> active.context == context
         | [] -> false)
  then Error "saved compiler context has another original source ledger"
  else Ok context

let create_saved_compiler_runtime ledger ~session ~suspension ~runtime =
  let ( let* ) = Result.bind in
  let* saved_parent = saved_compiler_context ledger ~session ~suspension in
  create_with_authority ~enclosing_ledger:ledger ~saved_parent
    ~compiler_positions:ledger.compiler_positions
    ~switch_budget:ledger.switch_budget
    ~max_dimension_work:ledger.max_dimension_work
    ~max_offset_work:ledger.max_offset_work (Task_runtime runtime) session

let require_observed_callback_default ledger receipt =
  let span = receipt.Parser.callback_default_ast.location.span in
  let state = callback_state ledger receipt.callback_default_signature span in
  if
    (not (List.exists (( == ) receipt) state.callback_defaults_rev))
    || not
         (Parser.callback_default_is_current receipt
         || Sema.Source_activation.callback_default ledger.activation receipt)
  then fail span "anonymous default lacks its original observed active boundary"

let begin_callback_default_attempt ledger ~runtime receipt =
  protect (fun () ->
      let span = receipt.Parser.callback_default_ast.location.span in
      require_initializer_runtime ledger runtime span;
      require_observed_callback_default ledger receipt;
      VM.begin_task_callback_default runtime ~namespace:ledger.namespace receipt
      |> checked span)

let callback_default_fragment_authority ledger ~runtime ~task_view receipt =
  protect (fun () ->
      let span = receipt.Parser.callback_default_ast.location.span in
      require_initializer_runtime ledger runtime span;
      require_observed_callback_default ledger receipt;
      if not (VM.task_owns_snapshot runtime task_view) then
        fail span "anonymous default has another task snapshot";
      let expression =
        match receipt.callback_default_ast.value with
        | Ast.Expression_default e -> e
        | Lastclass_default _ ->
            fail span "lastclass needs separate materialization"
      in
      let environment, references, queries =
        selected_fragment_transcript ledger ~task_view ~span expression
      in
      let fragment =
        Sema.Default_fragment.create_callback ~table:ledger.table
          ~namespace:ledger.namespace ~receipt ~environment ~references ~queries
        |> (fun result ->
        Result.bind result
          (Sema.Default_fragment.with_positions
             ~compiler_positions:ledger.compiler_positions))
        |> checked span
      in
      Sema.Default_fragment.authorize ?activation:ledger.activation
        ~namespace:ledger.namespace fragment
      |> checked span)

let complete_callback_defaults_runtime ledger ~runtime header =
  protect (fun () ->
      let span =
        header.Parser.callback_signature_publication.callback_opening.span
      in
      require_initializer_runtime ledger runtime span;
      let state =
        callback_state ledger header.callback_signature_publication span
      in
      if
        not
          (Option.fold ~none:false ~some:(( == ) header) state.callback_header)
      then fail span "anonymous completion lacks its original observed header";
      VM.complete_task_callback_defaults runtime ~namespace:ledger.namespace
        header
      |> checked span)

let begin_source_callback_default_with_owner owner ledger ~runtime receipt =
  protect (fun () ->
      let span = receipt.Parser.callback_default_ast.location.span in
      require_observed_callback_default ledger receipt;
      let mode =
        Parser.context_mode
          receipt.callback_default_signature.callback_command.command_context
      in
      (match ledger.authority with
      | Source_compilation _
        when owner = Native_source_default || mode = Frontend.Preprocessor.Aot
        -> ()
      | _ ->
          fail span
            "anonymous output defaults require their original AOT source ledger");
      if
        owner = Native_source_default
        && not (VM.task_owns_table runtime ledger.table)
      then fail span "anonymous native preparation has another semantic table";
      if
        List.exists
          (fun (r, _, _, _, _) -> r == receipt)
          ledger.source_callback_attempts
      then fail span "anonymous output default already attempted";
      (match ledger.source_defaults_runtime with
      | Some prior when prior != runtime ->
          fail span "anonymous defaults have another invocation budget"
      | _ -> ());
      let rec predecessor = function
        | None -> ()
        | Some r -> (
            match r.Parser.callback_default_ast.value with
            | Ast.Lastclass_default _ ->
                predecessor r.callback_default_predecessor
            | Expression_default _ ->
                if
                  not
                    (List.exists
                       (fun (p, _, prior_owner, _, bits) ->
                         p == r && prior_owner = owner && Option.is_some !bits)
                       ledger.source_callback_attempts)
                then
                  fail span
                    "anonymous default requires its successful predecessor")
      in
      predecessor receipt.callback_default_predecessor;
      let expression =
        match receipt.callback_default_ast.value with
        | Ast.Expression_default e -> e
        | Lastclass_default _ ->
            fail span "lastclass needs separate materialization"
      in
      if Sema.Initializer_source.expression_identifier_nodes expression <> []
      then
        fail ~code:"HCRUN0006" span
          (match owner with
          | Output_aot_default ->
              "AOT anonymous default references require output relocation and \
               callable authority"
          | Native_source_default ->
              "native defaults require closed expressions without value or \
               function references");
      let module Outer = Sema.Outer_environment in
      let compilation_mode, tables =
        match mode with
        | Frontend.Preprocessor.Aot -> (Outer.Aot, [ (Outer.Assembler, 0) ])
        | Frontend.Preprocessor.Jit ->
            (Outer.Jit, [ (Outer.Jit_task 0, 0); (Outer.Assembler, 1) ])
      in
      let tables =
        List.map
          (fun (table_kind, table_index) ->
            Outer.make_table ~table_kind ~table_index []
            |> Result.map_error Outer.error_to_string
            |> checked span)
          tables
      in
      let environment =
        Outer.create ~table:ledger.table ~compilation_mode tables
        |> Result.map_error Outer.error_to_string
        |> checked span
      in
      let queries =
        Sema.Query_selection.source_queries expression
        |> List.map (fun expression ->
            match Query_expressions.find_opt ledger.queries expression with
            | Some query -> query.query_selection
            | None ->
                fail span "anonymous default lacks its original checked query")
      in
      let fragment =
        Sema.Default_fragment.create_callback ~table:ledger.table
          ~namespace:ledger.namespace ~receipt ~environment ~references:[]
          ~queries
        |> (fun result ->
        Result.bind result
          (Sema.Default_fragment.with_positions
             ~compiler_positions:ledger.compiler_positions))
        |> checked span
      in
      let authority =
        Sema.Default_fragment.authorize ~namespace:ledger.namespace fragment
        |> checked span
      in
      ledger.source_defaults_runtime <- Some runtime;
      ledger.source_callback_attempts <-
        (receipt, authority, owner, VM.task_initializer_steps runtime, ref None)
        :: ledger.source_callback_attempts;
      authority)

let begin_source_callback_default =
  begin_source_callback_default_with_owner Output_aot_default

let begin_native_source_callback_default =
  begin_source_callback_default_with_owner Native_source_default

let finish_source_callback_default_with_owner owner ledger execution =
  protect (fun () ->
      let fragment =
        Sema.Default_fragment.authorized_fragment
          (VM.default_constant_authority execution)
      in
      let receipt =
        match Sema.Default_fragment.source fragment with
        | Callback (namespace, r) when namespace == ledger.namespace -> r
        | _ ->
            fail (Sema.Default_fragment.ast fragment).location.span
              "anonymous output default has another original source"
      in
      let span = receipt.callback_default_ast.location.span in
      require_observed_callback_default ledger receipt;
      let before, bits =
        match
          List.find_opt
            (fun (r, a, prior_owner, _, bits) ->
              r == receipt && prior_owner = owner
              && a == VM.default_constant_authority execution
              && !bits = None)
            ledger.source_callback_attempts
        with
        | Some (_, _, _, before, bits) -> (before, bits)
        | _ ->
            fail span
              "anonymous output default completion is foreign or repeated"
      in
      let runtime =
        match ledger.source_defaults_runtime with
        | Some runtime
          when VM.task_initializer_steps runtime - before
               = VM.default_constant_steps execution -> runtime
        | _ ->
            fail span
              "anonymous output preparation was not charged to its owning \
               invocation"
      in
      bits :=
        Some (VM.consume_default_constant runtime execution |> checked span))

let finish_source_callback_default =
  finish_source_callback_default_with_owner Output_aot_default

let finish_native_source_callback_default =
  finish_source_callback_default_with_owner Native_source_default

let complete_source_callback_defaults ledger header =
  protect (fun () ->
      let span =
        header.Parser.callback_signature_publication.callback_opening.span
      in
      let state =
        callback_state ledger header.callback_signature_publication span
      in
      if
        (not (Parser.callback_signature_completion_is_current header))
        || not
             (Option.fold ~none:false ~some:(( == ) header)
                state.callback_header)
      then
        fail span
          "anonymous output defaults require their original current completed \
           signature";
      let values =
        List.filter_map
          (fun receipt ->
            match receipt.Parser.callback_default_ast.value with
            | Ast.Lastclass_default _ -> None
            | Expression_default _ ->
                let bits =
                  match
                    List.find_opt
                      (fun (r, _, _, _, bits) ->
                        r == receipt && Option.is_some !bits)
                      ledger.source_callback_attempts
                  with
                  | Some (_, _, _, _, bits) -> Option.get !bits
                  | _ ->
                      fail span
                        "anonymous output signature requires every successful \
                         original default"
                in
                if
                  List.exists
                    (fun v -> Ir.Prepared_callback_default.receipt v == receipt)
                    ledger.prepared_source_callback_defaults
                then fail span "anonymous output defaults cannot publish twice";
                Some
                  (Ir.Prepared_callback_default.create
                     ~namespace:ledger.namespace ~header ~receipt ~bits
                  |> checked span))
          header.callback_defaults
      in
      ledger.prepared_source_callback_defaults <-
        values @ ledger.prepared_source_callback_defaults)

let source_callback_defaults ~table ~ast (Source_command command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "anonymous output defaults belong to another original source seal";
      command.source_callback_defaults)

let native_source_callback_defaults ~table ~ast (Source_command command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "anonymous native defaults belong to another original source seal";
      command.native_source_callback_defaults)
