module Parser = Frontend.Parser
module Ast = Frontend.Ast
module Visibility = Frontend.Symbol_visibility
module P = Provisional_function
module C = Declaration_collection

type native_identity = unit ref
type revision = { previous_revision : revision option }

type phase_event =
  | Initial_publication of Parser.function_publication
  | Observed_event of Parser.declaration_event

type slot =
  | Concrete of P.member
  | Argc of Parser.function_variadic_publication
  | Argv of Parser.function_variadic_publication

type native_state = {
  owner : Parser.function_publication;
  slots : slot list;
  members : int option;
  arguments : int option;
  ellipsis : bool;
  extern : bool option;
  unavailable : string option;
}

type native = {
  identity : native_identity;
  mutable state : native_state;
  mutable revision : revision;
}

type registry = {
  table : Symbol_table.t;
  namespace : C.namespace;
  mutable records : t list;
}

and t = {
  registry : registry;
  transcript : P.t;
  native : native;
  saved_arguments : int option;
  mutable aliases : Visibility.entry list;
  mutable body : Ast.function_definition option;
  mutable latest_phase_event : phase_event option;
  mutable phase_revision : revision;
  mutable cached : snapshot option;
}

and snapshot = {
  source_state : P.snapshot;
  native_state : native_state;
  identity : native_identity;
  snapshot_saved_arguments : int option;
  revision : revision;
  phase_event : phase_event option;
}

type transition = { earlier : snapshot; later : snapshot }

let transition_earlier proof = proof.earlier
let transition_later proof = proof.later

let transition ~earlier ~later =
  let rec follows revision =
    match revision.previous_revision with
    | None -> false
    | Some previous -> previous == earlier.revision || follows previous
  in
  if earlier.identity == later.identity && follows later.revision then
    Ok { earlier; later }
  else
    Error "native function transition requires strict checked forward ancestry"

let advance_native native state =
  native.state <- state;
  native.revision <- { previous_revision = Some native.revision }

type checked_call_shape = {
  snapshot : snapshot;
  fixed : P.member list;
  tail : Parser.function_variadic_publication option;
}

let source_snapshot snapshot = snapshot.source_state
let source snapshot = P.source snapshot.source_state
let publication snapshot = P.publication snapshot.source_state

let matches_event snapshot event =
  match (snapshot.phase_event, event) with
  | Some (Initial_publication original), Parser.Function_declared candidate ->
      original == candidate
  | ( Some (Observed_event (Parser.Function_parameter_declared original)),
      Parser.Function_parameter_declared candidate ) -> original == candidate
  | ( Some (Observed_event (Parser.Parameter_default_completed original)),
      Parser.Parameter_default_completed candidate ) -> original == candidate
  | ( Some (Observed_event (Parser.Function_parameter_completed original)),
      Parser.Function_parameter_completed candidate ) -> original == candidate
  | ( Some (Observed_event (Parser.Function_variadic_started original)),
      Parser.Function_variadic_started candidate ) -> original == candidate
  | ( Some (Observed_event (Parser.Function_variadic_completed original)),
      Parser.Function_variadic_completed candidate ) -> original == candidate
  | ( Some (Observed_event (Parser.Function_header_completed original)),
      Parser.Function_header_completed candidate ) -> original == candidate
  | _ -> false

let owns_table snapshot table = P.owns_table snapshot.source_state table

let owns_namespace snapshot namespace =
  P.owns_namespace snapshot.source_state namespace

let native_identity snapshot = snapshot.identity
let same_identity left right = left.identity == right.identity

let same_revision left right =
  same_identity left right && left.revision == right.revision

let same_cursor left right =
  same_identity left right
  && left.native_state.owner == right.native_state.owner
  && left.native_state.slots == right.native_state.slots
  && left.native_state.arguments = right.native_state.arguments
  && left.native_state.ellipsis = right.native_state.ellipsis

let native_source snapshot = snapshot.native_state.owner

let native_members snapshot =
  List.filter_map
    (function
      | Concrete member -> Some member
      | Argc _ | Argv _ -> None)
    snapshot.native_state.slots

let argument_count snapshot = snapshot.native_state.arguments
let member_count snapshot = snapshot.native_state.members
let saved_previous_argument_count snapshot = snapshot.snapshot_saved_arguments
let ellipsis_flag snapshot = snapshot.native_state.ellipsis
let is_extern snapshot = snapshot.native_state.extern

let unavailable_reason snapshot =
  match
    (snapshot.native_state.unavailable, snapshot.native_state.arguments)
  with
  | Some reason, _ -> Some reason
  | None, None -> Some "native argument count is unavailable"
  | None, Some _ -> None

let shape_snapshot shape = shape.snapshot
let fixed_members shape = shape.fixed
let variadic_tail shape = shape.tail

let snapshot record =
  let source_state = P.snapshot record.transcript in
  let native_state = record.native.state in
  match record.cached with
  | Some cached
    when cached.source_state == source_state
         && cached.native_state == native_state
         && cached.revision == record.native.revision -> cached
  | _ ->
      let cached =
        {
          source_state;
          native_state;
          identity = record.native.identity;
          snapshot_saved_arguments = record.saved_arguments;
          revision = record.native.revision;
          phase_event =
            (if record.phase_revision == record.native.revision then
               record.latest_phase_event
             else None);
        }
      in
      record.cached <- Some cached;
      cached

type call_start_snapshot = {
  call_record : t;
  original_start : Parser.call_start;
  argument_snapshot : snapshot;
}

type call_emission_snapshot = {
  original_arguments : call_start_snapshot;
  original_emission : Parser.completed_call;
  emission_snapshot : snapshot;
}

let call_start_receipt token = token.original_start
let call_argument_snapshot token = token.argument_snapshot
let call_emission_receipt token = token.original_emission
let call_emission_snapshot token = token.emission_snapshot
let call_emission_arguments token = token.original_arguments

let capture_call_start record start =
  let captured = snapshot record in
  let rec original entry =
    match Visibility.function_alias_original entry with
    | Some source -> original source
    | None -> entry
  in
  let selected =
    match Parser.selected_lookup start.Parser.call_reference with
    | Visibility.Present entry ->
        let entry = original entry in
        entry == (source captured).function_entry
        || Option.fold ~none:false
             ~some:(fun header -> entry == header.Parser.completed_entry)
             (P.completed_header captured.source_state)
    | _ -> false
  in
  if not (Parser.call_start_is_current start && selected) then
    Error "native call capture requires its original live selected record"
  else
    Ok
      {
        call_record = record;
        original_start = start;
        argument_snapshot = captured;
      }

let capture_call_emission arguments receipt =
  if
    not
      (Parser.call_emission_is_current receipt
      && receipt.Parser.call_start == arguments.original_start)
  then Error "native emission capture requires its original live call"
  else
    Ok
      {
        original_arguments = arguments;
        original_emission = receipt;
        emission_snapshot = snapshot arguments.call_record;
      }

type implicit_arguments_snapshot = {
  implicit_record : t;
  implicit_receipt : Parser.implicit_output_selection;
  implicit_snapshot : snapshot;
}

type implicit_emission_snapshot = {
  implicit_arguments : implicit_arguments_snapshot;
  implicit_emitted : snapshot;
}

let implicit_arguments_receipt capture = capture.implicit_receipt
let implicit_argument_snapshot capture = capture.implicit_snapshot
let implicit_emission_arguments capture = capture.implicit_arguments
let implicit_emitted_snapshot capture = capture.implicit_emitted

let capture_implicit_arguments record receipt =
  let captured = snapshot record in
  let rec original entry =
    match Visibility.function_alias_original entry with
    | Some source -> original source
    | None -> entry
  in
  let selected =
    Option.fold ~none:false
      ~some:(fun entry ->
        let entry = original entry in
        entry == (source captured).function_entry
        || Option.fold ~none:false
             ~some:(fun header -> entry == header.Parser.completed_entry)
             (P.completed_header captured.source_state))
      (Parser.implicit_lookup receipt)
  in
  if not (Parser.implicit_arguments_are_current receipt && selected) then
    Error
      "implicit arguments require their original live selected native record"
  else
    Ok
      {
        implicit_record = record;
        implicit_receipt = receipt;
        implicit_snapshot = captured;
      }

let capture_implicit_emission arguments receipt =
  if
    not
      (Parser.implicit_emission_is_current receipt
      && receipt == arguments.implicit_receipt)
  then Error "implicit emission requires its original live argument capture"
  else
    Ok
      {
        implicit_arguments = arguments;
        implicit_emitted = snapshot arguments.implicit_record;
      }

let create_registry ~mode ~table ~namespace =
  if mode <> Frontend.Preprocessor.Jit then
    Error "native function record phases require JIT compilation"
  else if not (C.namespace_owns_table namespace table) then
    Error "native function registry requires its exact namespace table"
  else Ok { table; namespace; records = [] }

let same_source_owner left right =
  left.Parser.function_environment == right.Parser.function_environment
  && left.function_name.spelling = right.function_name.spelling

let rec entry_has_ancestry entries entry =
  List.exists (( == ) entry) entries
  || Option.fold ~none:false
       ~some:(entry_has_ancestry entries)
       (Visibility.function_alias_original entry)

let begin_header ?activation registry publication source =
  let latest =
    List.find_opt
      (fun record ->
        same_source_owner source (P.source (P.snapshot record.transcript)))
      registry.records
  in
  let lineage =
    match (source.Parser.function_previous, latest) with
    | Visibility.Absent, None -> Ok None
    | Visibility.Present entry, Some record
      when entry_has_ancestry record.aliases entry -> Ok (Some record)
    | Visibility.Present entry, _
      when not
             (List.exists
                (fun record -> entry_has_ancestry record.aliases entry)
                registry.records) -> Ok None
    | _ ->
        Error
          "native function header has inconsistent previous parser-entry \
           lineage"
  in
  if
    Parser.context_mode
      source.function_header.declaration_command.command_context
    <> Frontend.Preprocessor.Jit
  then Error "native function header is outside JIT source context"
  else if
    List.exists
      (fun record -> P.source (P.snapshot record.transcript) == source)
      registry.records
  then Error "native function header publication was already observed"
  else
    match lineage with
    | Error message -> Error message
    | Ok previous -> (
        match
          P.create ?activation ~table:registry.table
            ~namespace:registry.namespace publication source
        with
        | Error message -> Error message
        | Ok transcript ->
            let native, saved_arguments =
              match previous with
              | Some prior when prior.native.state.extern = Some true ->
                  let old = prior.native.state in
                  let state =
                    {
                      old with
                      owner = source;
                      slots = [];
                      members = Some 0;
                      arguments = Some 0;
                    }
                  in
                  advance_native prior.native state;
                  (prior.native, old.arguments)
              | previous ->
                  let unknown =
                    match (previous, source.function_previous) with
                    | Some prior, _ -> Option.is_none prior.native.state.extern
                    | None, Visibility.Present _ -> true
                    | _ -> false
                  in
                  let state =
                    {
                      owner = source;
                      slots = [];
                      members = (if unknown then None else Some 0);
                      arguments = (if unknown then None else Some 0);
                      ellipsis = false;
                      extern = (if unknown then None else Some true);
                      unavailable =
                        (if unknown then
                           Some "previous native function record is untracked"
                         else None);
                    }
                  in
                  ( {
                      identity = ref ();
                      state;
                      revision = { previous_revision = None };
                    },
                    None )
            in
            let record =
              {
                registry;
                transcript;
                native;
                saved_arguments;
                aliases = [ source.function_entry ];
                body = None;
                latest_phase_event = Some (Initial_publication source);
                phase_revision = native.revision;
                cached = None;
              }
            in
            registry.records <- record :: registry.records;
            Ok record)

let event_belongs record event =
  match event with
  | Parser.Function_body_completed (header, _) ->
      header.function_publication == P.source (P.snapshot record.transcript)
  | _ -> P.event_belongs record.transcript event

(* These source forms cannot add native body members. Other forms deliberately
   remain unavailable for a suspended header's later member_cnt assignment. *)
let rec body_preserves_members = function
  | Ast.Block_statement block ->
      List.for_all body_preserves_members block.block_statements
  | Ast.Sequence_statement sequence ->
      List.for_all
        (fun element -> body_preserves_members element.Ast.sequence_statement)
        sequence.sequence_elements
  | Ast.If_statement conditional ->
      body_preserves_members conditional.if_then_branch
      && Option.fold ~none:true
           ~some:(fun clause -> body_preserves_members clause.Ast.else_branch)
           conditional.if_else_clause
  | Ast.While_statement loop -> body_preserves_members loop.while_body
  | Ast.Do_while_statement loop -> body_preserves_members loop.do_body
  | Ast.For_statement loop ->
      body_preserves_members loop.for_initializer
      && Option.fold ~none:true ~some:body_preserves_members loop.for_update
      && body_preserves_members loop.for_body
  | Ast.Lock_statement lock -> body_preserves_members lock.lock_body
  | Ast.Break_statement _
  | Ast.Empty_statement _
  | Ast.Expression_statement _
  | Ast.Goto_statement _
  | Ast.Implicit_output_statement _
  | Ast.Label_statement _
  | Ast.No_warn_statement _
  | Ast.Return_statement _ -> true
  | Ast.Assembly_block_statement _
  | Ast.Inline_assembly_statement _
  | Ast.Local_declaration_statement _
  | Ast.Switch_statement _
  | Ast.Try_catch_statement _ -> false

let body_is_current ?activation record header definition event =
  Parser.function_body_completion_is_current header definition
  || Option.fold ~none:false
       ~some:(fun active ->
         Source_activation.owns_namespace active record.registry.namespace)
       activation
     && Source_activation.declaration activation event

let slot_name = function
  | Concrete member ->
      Option.map
        (fun (name : Ast.identifier) -> name.spelling)
        (P.member_source member).Parser.parameter_name
  | Argc _ -> Some "argc"
  | Argv _ -> Some "argv"

let member_collides slots name =
  match name with
  | None | Some ("pad" | "reserved" | "_anon_") -> false
  | Some spelling ->
      List.exists (fun slot -> slot_name slot = Some spelling) slots

let invalid_insertion state =
  {
    state with
    arguments = None;
    members = None;
    extern = None;
    unavailable = Some "native MemberAdd rejects a duplicate member name";
  }

let observe ?activation record event =
  let state = record.native.state in
  match event with
  | Parser.Function_body_completed (header, definition) ->
      if
        (not (event_belongs record event))
        || Option.is_some record.body
        || (not
              (Option.fold ~none:false ~some:(( == ) header)
                 (P.completed_header (P.snapshot record.transcript))))
        || not (body_is_current ?activation record header definition event)
      then Error "native function body lacks its original live completed header"
      else (
        record.body <- Some definition;
        let members =
          if Option.fold ~none:true ~some:body_preserves_members definition.body
          then state.members
          else None
        in
        let extern =
          if Option.is_some state.unavailable then None else Some false
        in
        advance_native record.native { state with extern; members };
        record.latest_phase_event <- None;
        record.phase_revision <- record.native.revision;
        Ok ())
  | _ -> (
      match P.observe ?activation record.transcript event with
      | Error _ as error -> error
      | Ok () ->
          let transcript = P.snapshot record.transcript in
          let member source =
            List.find
              (fun member -> P.member_source member == source)
              (P.members transcript)
          in
          let update_member source =
            List.map
              (function
                | Concrete prior when P.member_source prior == source ->
                    Concrete (member source)
                | slot -> slot)
              state.slots
          in
          let next =
            match event with
            | Parser.Function_parameter_declared source ->
                let inserted = Concrete (member source) in
                if member_collides state.slots (slot_name inserted) then
                  invalid_insertion state
                else
                  {
                    state with
                    slots = state.slots @ [ inserted ];
                    members = Option.map succ state.members;
                  }
            | Parser.Parameter_default_completed receipt ->
                if
                  List.exists
                    (function
                      | Concrete prior ->
                          P.member_source prior == receipt.default_parameter
                      | _ -> false)
                    state.slots
                then
                  { state with slots = update_member receipt.default_parameter }
                else
                  {
                    state with
                    unavailable =
                      Some
                        (Option.value state.unavailable
                           ~default:
                             "reentrant header replaced the native member \
                              before its default completed");
                  }
            | Parser.Function_parameter_completed complete ->
                {
                  state with
                  slots = update_member complete.parameter_publication;
                }
            | Parser.Function_variadic_started _ ->
                { state with ellipsis = true }
            | Parser.Function_variadic_completed source ->
                if member_collides state.slots (Some "argc") then
                  invalid_insertion state
                else
                  let with_argc =
                    { state with slots = state.slots @ [ Argc source ] }
                  in
                  if member_collides with_argc.slots (Some "argv") then
                    invalid_insertion with_argc
                  else
                    { with_argc with slots = with_argc.slots @ [ Argv source ] }
            | Parser.Function_header_completed header ->
                record.aliases <- header.completed_entry :: record.aliases;
                let bounded_binding =
                  match header.function_publication.function_header.binding with
                  | None -> true
                  | Some
                      {
                        Ast.kind = Ast.Extern;
                        spelling = "extern";
                        target = Ast.No_binding_target;
                        _;
                      } -> true
                  | Some _ -> false
                in
                if bounded_binding then { state with arguments = state.members }
                else
                  {
                    state with
                    arguments = state.members;
                    extern = None;
                    unavailable =
                      Some
                        "bound function lifecycle requires executable \
                         installation evidence";
                  }
            | _ -> state
          in
          advance_native record.native next;
          record.latest_phase_event <- Some (Observed_event event);
          record.phase_revision <- record.native.revision;
          Ok ())

let call_shape snapshot =
  let state = snapshot.native_state in
  let rec traverse remaining slots fixed =
    if remaining = 0 then
      let tail =
        match slots with
        | Argc source :: Argv following :: _ when source == following ->
            Some source
        | _ -> None
      in
      Ok { snapshot; fixed = List.rev fixed; tail }
    else
      match slots with
      | Concrete member :: rest ->
          traverse (remaining - 1) rest (member :: fixed)
      | _ ->
          Error "native argument count exceeds checked concrete member cursor"
  in
  match (state.unavailable, state.arguments) with
  | Some reason, _ -> Error reason
  | None, None -> Error "native argument count is unavailable"
  | None, Some count when count >= 0 -> traverse count state.slots []
  | _ -> Error "native argument count is invalid"
