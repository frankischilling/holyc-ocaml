module Parser = Frontend.Parser

type event =
  | Command of Parser.command_event
  | Declaration of Parser.declaration_event
  | Reference of Parser.reference_selection
  | Implicit_output of Parser.implicit_output_selection

type t = {
  namespace : Declaration_collection.namespace;
  context : Parser.command_context;
  observed_events : int;
  events : event list;
  mutable consumed : bool;
  mutable finished : bool;
  mutable active : event option;
}

let current t =
  Parser.context_is_current t.context ~observed_events:t.observed_events

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
  | Implicit_output receipt -> (Parser.implicit_command receipt).command_context
  | Declaration event ->
      let start =
        match event with
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
        | Parser.Parameter_default_completed p ->
            p.default_function.function_header.declaration_command
        | Parser.Function_header_completed p
        | Parser.Function_body_completed (p, _) ->
            p.function_publication.function_header.declaration_command
      in
      start.command_context

let create ~namespace ~context ~observed_events events =
  if
    (not (Parser.context_is_current context ~observed_events))
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
      }

let run t ~invalid callback =
  if t.consumed || not (current t) then Error invalid
  else (
    t.consumed <- true;
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

let command_admission activation receipt =
  admission activation (function
    | Command (Parser.Command_resumed original) -> original == receipt
    | _ -> false)

let default_completion activation header =
  admission activation (function
    | Declaration (Parser.Function_header_completed original) ->
        original == header
    | _ -> false)

let initializer_completion activation start =
  admission activation (function
    | Declaration (Parser.Global_completed (original, _)) ->
        original == start.Parser.initializer_owner
    | _ -> false)
