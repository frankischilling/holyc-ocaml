module Parser = Frontend.Parser

type event = Parser.source_observation =
  | Command of Parser.command_event
  | Declaration of Parser.declaration_event
  | Reference of Parser.reference_selection
  | Call_start of Parser.call_start
  | Call_emission of Parser.completed_call
  | Implicit_output of Parser.implicit_output_selection
  | Implicit_arguments of Parser.implicit_output_selection
  | Implicit_emission of Parser.implicit_output_selection

type call_journal = {
  namespace : Declaration_collection.namespace;
  mutable prefix_rev : event list;
  mutable captures_rev : event list;
  mutable context : Parser.command_context option;
  mutable revision : int;
  mutable claimed : unit ref option;
}

let create_call_journal ~namespace () =
  {
    namespace;
    prefix_rev = [];
    captures_rev = [];
    context = None;
    revision = 0;
    claimed = None;
  }

type t = {
  namespace : Declaration_collection.namespace;
  context : Parser.command_context;
  observed_events : int;
  events : event list;
  mutable consumed : bool;
  mutable finished : bool;
  mutable active : event option;
  calls : (call_journal * int) option;
  identity : unit ref;
  observation_count : int option;
}

let current t =
  Parser.context_is_current t.context ~observed_events:t.observed_events
  && Parser.source_observation_count t.context = t.observation_count
  && Option.fold ~none:true
       ~some:(fun (calls, revision) ->
         calls.revision = revision
         && Option.fold ~none:true ~some:(( == ) t.identity) calls.claimed)
       t.calls

let owns_namespace t namespace = t.namespace == namespace
let available t = (not t.consumed) && current t

let command_events t =
  List.filter_map
    (function
      | Command event -> Some event
      | _ -> None)
    t.events

let dimension_preparations t =
  List.filter_map
    (function
      | Declaration (Parser.Array_dimension_preparing p) -> Some p
      | _ -> None)
    t.events

let trailing_dimension_preparation t =
  match List.rev t.events with
  | Declaration (Parser.Array_dimension_preparing preparation) :: _ ->
      Some preparation
  | _ -> None

let before_dimension t preparation =
  if not (current t) then false
  else
    let rec scan = function
      | [] -> false
      | Declaration (Parser.Array_dimension_preparing p) :: _
        when p == preparation -> false
      | event :: rest ->
          if
            Option.fold ~none:false
              ~some:(fun active -> active == event)
              t.active
          then true
          else scan rest
    in
    scan t.events

let event_context = function
  | Command (Parser.Sequence_started context | Parser.Sequence_aborted context)
    -> context
  | Command (Parser.Command_started start) -> start.command_context
  | Command (Parser.Command_completed receipt | Parser.Command_resumed receipt)
    -> receipt.command_start.command_context
  | Command (Parser.Sequence_completed receipt) -> receipt.sequence_context
  | Reference receipt -> (Parser.selected_command receipt).command_context
  | Call_start receipt ->
      (Parser.selected_command receipt.call_reference).command_context
  | Call_emission receipt ->
      (Parser.selected_command receipt.call_start.call_reference)
        .command_context
  | Implicit_output receipt
  | Implicit_arguments receipt
  | Implicit_emission receipt ->
      (Parser.implicit_command receipt).command_context
  | Declaration event ->
      let start =
        match event with
        | Parser.Aggregate_declared p -> p.aggregate_header.declaration_command
        | Parser.Aggregate_completed p ->
            p.aggregate_publication.aggregate_header.declaration_command
        | Parser.Array_dimension_preparing p ->
            p.dimension_owner.dimensions_command
        | Parser.Array_dimension_completed p ->
            p.dimension_preparation.dimension_owner.dimensions_command
        | Parser.Global_declared p | Parser.Global_completed (p, _) ->
            p.global_header.declaration_command
        | Parser.Global_initializer_started p ->
            p.initializer_owner.global_header.declaration_command
        | Parser.Global_initializer_leaf_completed p ->
            p.leaf_initializer.initializer_owner.global_header
              .declaration_command
        | Parser.Global_initializer_delimiter_completed p ->
            p.delimiter_initializer.initializer_owner.global_header
              .declaration_command
        | Parser.Function_declared p -> p.function_header.declaration_command
        | Parser.Function_parameter_declared p ->
            p.parameter_function.function_header.declaration_command
        | Parser.Function_parameter_completed p ->
            p.parameter_publication.parameter_function.function_header
              .declaration_command
        | Parser.Function_variadic_started p
        | Parser.Function_variadic_completed p ->
            p.variadic_function.function_header.declaration_command
        | Parser.Parameter_default_completed p ->
            p.default_function.function_header.declaration_command
        | Parser.Function_header_completed p
        | Parser.Function_body_completed (p, _) ->
            p.function_publication.function_header.declaration_command
      in
      start.command_context

let rec same_events left right =
  match (left, right) with
  | [], [] -> true
  | first :: left, second :: right -> first == second && same_events left right
  | _ -> false

let extends_prefix events_rev prefix_rev =
  let extra = List.length events_rev - List.length prefix_rev in
  let rec drop count events =
    if count = 0 then same_events events prefix_rev
    else
      match events with
      | [] -> false
      | _ :: rest -> drop (count - 1) rest
  in
  extra >= 0 && drop extra events_rev

let call_events events =
  List.filter
    (function
      | Call_start _
      | Call_emission _
      | Implicit_arguments _
      | Implicit_emission _ -> true
      | _ -> false)
    events

let validate_capture_owner journal events_rev (start : Parser.command_start)
    matches_reference =
  let context = start.command_context in
  let commands =
    List.filter_map
      (function
        | Command event -> Some event
        | _ -> None)
      events_rev
  in
  let observed_events = List.length commands in
  let reading =
    match commands with
    | Parser.Command_started original :: _ -> original == start
    | _ -> false
  in
  Option.is_none journal.claimed
  && Parser.context_mode context = Frontend.Preprocessor.Jit
  && Option.is_none (Parser.context_parent context)
  && Parser.context_is_current context ~observed_events
  && reading
  && Option.fold ~none:true ~some:(( == ) context) journal.context
  && List.for_all (fun event -> event_context event == context) events_rev
  && List.exists matches_reference events_rev
  && extends_prefix events_rev journal.prefix_rev
  && same_events (call_events events_rev) journal.captures_rev

let validate_capture journal events_rev reference =
  validate_capture_owner journal events_rev (Parser.selected_command reference)
    (function
    | Reference original -> original == reference
    | _ -> false)

let capture (journal : call_journal) events_rev reference event =
  journal.context <- Some (Parser.selected_command reference).command_context;
  journal.prefix_rev <- event :: events_rev;
  journal.captures_rev <- event :: journal.captures_rev;
  journal.revision <- journal.revision + 1;
  Ok event

let capture_call_start journal ~events_rev receipt =
  let event = Call_start receipt in
  let context =
    (Parser.selected_command receipt.Parser.call_reference).command_context
  in
  if
    (not (Parser.call_start_is_current receipt))
    || (not (validate_capture journal events_rev receipt.Parser.call_reference))
    || List.exists
         (function
           | Call_start original -> original == receipt
           | _ -> false)
         journal.captures_rev
    || Parser.source_observations_match context ~events_rev:(event :: events_rev)
       <> Some true
    || not (Parser.claim_call_start receipt)
  then
    Error "call start requires its original live reference and journal prefix"
  else capture journal events_rev receipt.call_reference event

let capture_call_emission journal ~events_rev receipt =
  let reference = receipt.Parser.call_start.call_reference in
  let event = Call_emission receipt in
  let context = (Parser.selected_command reference).command_context in
  if
    (not (Parser.call_emission_is_current receipt))
    || (not (validate_capture journal events_rev reference))
    || (not
          (List.exists
             (function
               | Call_start original -> original == receipt.call_start
               | _ -> false)
             journal.captures_rev))
    || List.exists
         (function
           | Call_emission original -> original == receipt
           | _ -> false)
         journal.captures_rev
    || Parser.source_observations_match context ~events_rev:(event :: events_rev)
       <> Some true
    || not (Parser.claim_call_emission receipt)
  then Error "call emission requires its original live start and journal prefix"
  else capture journal events_rev reference event

let capture_implicit journal ~events_rev receipt ~emission =
  let event =
    if emission then Implicit_emission receipt else Implicit_arguments receipt
  in
  let start = Parser.implicit_command receipt in
  let matches = function
    | Implicit_arguments original -> (not emission) && original == receipt
    | Implicit_emission original -> emission && original == receipt
    | _ -> false
  in
  let live =
    if emission then Parser.implicit_emission_is_current receipt
    else Parser.implicit_arguments_are_current receipt
  in
  let original_start =
    (not emission)
    || List.exists
         (function
           | Implicit_arguments original -> original == receipt
           | _ -> false)
         journal.captures_rev
  in
  if
    not
      (live && original_start
      && validate_capture_owner journal events_rev start (function
        | Implicit_output original -> original == receipt
        | _ -> false)
      && (not (List.exists matches journal.captures_rev))
      && Parser.source_observations_match start.command_context
           ~events_rev:(event :: events_rev)
         = Some true
      &&
      if emission then Parser.claim_implicit_emission receipt
      else Parser.claim_implicit_arguments receipt)
  then
    Error
      "implicit call capture requires its original live target and journal \
       prefix"
  else (
    journal.context <- Some start.command_context;
    journal.prefix_rev <- event :: events_rev;
    journal.captures_rev <- event :: journal.captures_rev;
    journal.revision <- journal.revision + 1;
    Ok event)

let create ?calls ~namespace ~context ~observed_events events =
  let checked_calls =
    match calls with
    | None -> call_events events = []
    | Some (calls : call_journal) ->
        calls.namespace == namespace
        && Option.is_none calls.claimed
        && Option.fold ~none:true ~some:(( == ) context) calls.context
        && extends_prefix (List.rev events) calls.prefix_rev
        && same_events (List.rev (call_events events)) calls.captures_rev
  in
  if
    (not checked_calls)
    || Parser.source_observations_match context ~events_rev:(List.rev events)
       = Some false
    || (not (Parser.context_is_current context ~observed_events))
    || Parser.context_mode context <> Frontend.Preprocessor.Jit
    || Option.is_some (Parser.context_parent context)
    || (not (List.for_all (fun event -> event_context event == context) events))
    || List.fold_left
         (fun count -> function
           | Command _ -> count + 1
           | _ -> count)
         0 events
       <> observed_events
  then Error "source activation requires its complete original live JIT journal"
  else
    Ok
      {
        namespace;
        context;
        observed_events;
        events;
        consumed = false;
        finished = false;
        active = None;
        calls = Option.map (fun calls -> (calls, calls.revision)) calls;
        identity = ref ();
        observation_count = Parser.source_observation_count context;
      }

let run t ~invalid callback =
  if t.consumed || not (current t) then Error invalid
  else (
    t.consumed <- true;
    Option.iter (fun (calls, _) -> calls.claimed <- Some t.identity) t.calls;
    Fun.protect
      ~finally:(fun () -> t.active <- None)
      (fun () ->
        let rec consume = function
          | [] ->
              t.finished <- true;
              Ok ()
          | event :: rest ->
              if not (current t) then Error invalid
              else (
                t.active <- Some event;
                let result =
                  Fun.protect
                    ~finally:(fun () -> t.active <- None)
                    (fun () -> callback event)
                in
                Result.bind result (fun () -> consume rest))
        in
        consume t.events))

let allows activation matches =
  Option.fold ~none:false
    ~some:(fun t -> current t && Option.fold ~none:false ~some:matches t.active)
    activation

let function_header activation header =
  allows activation (function
    | Declaration (Parser.Function_header_completed original) ->
        original == header
    | _ -> false)

let function_declaration activation publication =
  allows activation (function
    | Declaration (Parser.Function_declared original) -> original == publication
    | _ -> false)

let initializer_start activation receipt =
  allows activation (function
    | Declaration (Parser.Global_initializer_started original) ->
        original == receipt
    | _ -> false)

let initializer_leaf activation receipt =
  allows activation (function
    | Declaration (Parser.Global_initializer_leaf_completed original) ->
        original == receipt
    | _ -> false)

let initializer_delimiter activation receipt =
  allows activation (function
    | Declaration (Parser.Global_initializer_delimiter_completed original) ->
        original == receipt
    | _ -> false)

let parameter_default activation receipt =
  allows activation (function
    | Declaration (Parser.Parameter_default_completed original) ->
        original == receipt
    | _ -> false)

let declaration activation receipt =
  allows activation (function
    | Declaration original -> original == receipt
    | _ -> false)

let dimension_preparing activation receipt =
  allows activation (function
    | Declaration (Parser.Array_dimension_preparing original) ->
        original == receipt
    | _ -> false)

let dimension_completed activation receipt =
  allows activation (function
    | Declaration (Parser.Array_dimension_completed original) ->
        original == receipt
    | _ -> false)

let reference activation receipt =
  allows activation (function
    | Reference original -> original == receipt
    | _ -> false)

let call_start activation receipt =
  allows activation (function
    | Call_start original -> original == receipt
    | _ -> false)

let call_emission activation receipt =
  allows activation (function
    | Call_emission original -> original == receipt
    | _ -> false)

let implicit_output activation receipt =
  allows activation (function
    | Implicit_output original -> original == receipt
    | _ -> false)

let finished activation =
  Option.fold ~none:true ~some:(fun t -> t.finished) activation

let owns_context activation context =
  Option.fold ~none:true ~some:(fun t -> t.context == context) activation

let admission activation matches =
  match activation with
  | None -> true
  | Some t -> (not (List.exists matches t.events)) || allows activation matches

let global_admission activation publication =
  admission activation (function
    | Declaration (Parser.Global_declared original) -> original == publication
    | _ -> false)

let dimension_admission activation preparation =
  admission activation (function
    | Declaration (Parser.Array_dimension_preparing original) ->
        original == preparation
    | _ -> false)

let function_phase_admission activation event =
  let same_phase = function
    | Declaration original -> (
        match (original, event) with
        | Parser.Function_declared left, Parser.Function_declared right ->
            left == right
        | ( Parser.Function_parameter_declared left,
            Parser.Function_parameter_declared right ) -> left == right
        | ( Parser.Parameter_default_completed left,
            Parser.Parameter_default_completed right ) -> left == right
        | ( Parser.Function_parameter_completed left,
            Parser.Function_parameter_completed right ) -> left == right
        | ( Parser.Function_variadic_started left,
            Parser.Function_variadic_started right ) -> left == right
        | ( Parser.Function_variadic_completed left,
            Parser.Function_variadic_completed right ) -> left == right
        | ( Parser.Function_header_completed left,
            Parser.Function_header_completed right ) -> left == right
        | _ -> false)
    | _ -> false
  in
  same_phase (Declaration event) && admission activation same_phase

let command_admission activation receipt =
  admission activation (function
    | Command (Parser.Command_resumed original) -> original == receipt
    | _ -> false)

let default_completion activation header =
  admission activation (function
    | Declaration (Parser.Function_header_completed original) ->
        original == header
    | _ -> false)

let call_start_admission activation start =
  admission activation (function
    | Call_start original -> original == start
    | _ -> false)

let reference_admission activation receipt =
  admission activation (function
    | Reference original -> original == receipt
    | _ -> false)

let call_binding_available activation receipt ~committed =
  committed
  ||
  match activation with
  | None -> true
  | Some t ->
      (not
         (List.exists
            (function
              | Call_emission original -> original == receipt
              | _ -> false)
            t.events))
      || t.finished
      || (current t && Option.is_some t.active)

let call_emission_admission activation receipt =
  admission activation (function
    | Call_emission original -> original == receipt
    | _ -> false)

let initializer_completion activation start =
  admission activation (function
    | Declaration (Parser.Global_completed (original, _)) ->
        original == start.Parser.initializer_owner
    | _ -> false)

let implicit_arguments activation receipt =
  allows activation (function
    | Implicit_arguments original -> original == receipt
    | _ -> false)

let implicit_emission activation receipt =
  allows activation (function
    | Implicit_emission original -> original == receipt
    | _ -> false)

let implicit_selection_admission activation receipt =
  admission activation (function
    | Implicit_output original -> original == receipt
    | _ -> false)

let implicit_arguments_admission activation receipt =
  admission activation (function
    | Implicit_arguments original -> original == receipt
    | _ -> false)

let implicit_emission_admission activation receipt =
  admission activation (function
    | Implicit_emission original -> original == receipt
    | _ -> false)

let implicit_binding_available activation receipt ~committed =
  committed
  ||
  match activation with
  | None -> true
  | Some t ->
      (not
         (List.exists
            (function
              | Implicit_emission original -> original == receipt
              | _ -> false)
            t.events))
      || t.finished
      || (current t && Option.is_some t.active)
