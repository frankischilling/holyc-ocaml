module Parser = Frontend.Parser
module Ast = Frontend.Ast
module Collection = Declaration_collection

type member = {
  source : Parser.function_parameter_publication;
  default : Parser.completed_parameter_default option;
  completion : Parser.completed_function_parameter option;
}

type snapshot = {
  table : Symbol_table.t;
  namespace : Collection.namespace;
  publication : Collection.publication;
  source : Parser.function_publication;
  members_rev : member list;
  variadic : Parser.function_variadic_publication option;
  variadic_completed : bool;
  header : Parser.completed_function_header option;
}

type t = { mutable current : snapshot }

let snapshot record = record.current
let source (state : snapshot) = state.source
let publication state = state.publication
let owns_table state table = state.table == table
let owns_namespace state namespace = state.namespace == namespace
let previous_lookup (state : snapshot) = state.source.function_previous
let members state = List.rev state.members_rev
let member_source (member : member) = member.source
let member_default_source member = member.default
let member_completion member = member.completion
let variadic_source state = state.variadic
let variadic_members_present state = state.variadic_completed
let completed_header state = state.header

let same_option equal left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> equal left right
  | _ -> false

let same_list left right =
  List.length left = List.length right && List.for_all2 ( == ) left right

let create ?activation ~table ~namespace publication source =
  if
    (not (Collection.namespace_owns_table namespace table))
    || not (Collection.namespace_owns_publication namespace publication)
  then Error "provisional source requires its exact namespace and table"
  else if
    not
      (same_option ( == )
         (Collection.publication_source_function publication)
         (Some source))
  then Error "provisional source requires its original function publication"
  else if
    not
      (Parser.function_publication_is_current source
      || Option.fold ~none:false
           ~some:(fun active ->
             Source_activation.owns_namespace active namespace)
           activation
         && Source_activation.function_declaration activation source)
  then Error "provisional source is outside its original declaration callback"
  else
    Ok
      {
        current =
          {
            table;
            namespace;
            publication;
            source;
            members_rev = [];
            variadic = None;
            variadic_completed = false;
            header = None;
          };
      }

let event_source = function
  | Parser.Function_parameter_declared p -> Some p.parameter_function
  | Parser.Parameter_default_completed p -> Some p.default_function
  | Parser.Function_parameter_completed p ->
      Some p.parameter_publication.parameter_function
  | Parser.Function_variadic_started p | Parser.Function_variadic_completed p ->
      Some p.variadic_function
  | Parser.Function_header_completed h -> Some h.function_publication
  | _ -> None

let event_belongs record event =
  same_option ( == ) (event_source event) (Some record.current.source)

let event_is_current = function
  | Parser.Function_parameter_declared p ->
      Parser.function_parameter_is_current p
  | Parser.Parameter_default_completed p ->
      Parser.parameter_default_is_current p
  | Parser.Function_parameter_completed p ->
      Parser.function_parameter_completion_is_current p
  | Parser.Function_variadic_started p ->
      Parser.function_variadic_start_is_current p
  | Parser.Function_variadic_completed p ->
      Parser.function_variadic_completion_is_current p
  | Parser.Function_header_completed h -> Parser.function_header_is_current h
  | _ -> false

let completed_predecessor state =
  match state.members_rev with
  | [] -> None
  | member :: _ -> member.completion

let ready_for_member state =
  Option.is_none state.variadic
  &&
  match state.members_rev with
  | [] -> true
  | member :: _ -> Option.is_some member.completion

let head_matches (source : Parser.function_parameter_publication)
    (ast : Ast.function_parameter) =
  source.parameter_type_specifier == ast.type_specifier
  && same_list source.parameter_register_qualifiers ast.register_qualifiers
  && same_list source.parameter_pointer_layers ast.pointer_layers
  && same_option ( == ) source.parameter_name ast.name
  && same_option ( == ) source.parameter_function_pointer ast.function_pointer

let update state = function
  | Parser.Function_parameter_declared source ->
      if
        (not (ready_for_member state))
        || source.parameter_index <> List.length state.members_rev
        || not
             (same_option ( == ) source.parameter_predecessor
                (completed_predecessor state))
      then Error "provisional member is repeated, incomplete or out of order"
      else
        Ok
          {
            state with
            members_rev =
              { source; default = None; completion = None } :: state.members_rev;
          }
  | Parser.Parameter_default_completed receipt -> (
      match state.members_rev with
      | member :: rest
        when member.source == receipt.default_parameter
             && Option.is_none member.default
             && Option.is_none member.completion
             && Option.is_none state.variadic ->
          let head = member.source in
          let predecessor = List.find_map (fun member -> member.default) rest in
          if
            receipt.default_parameter_index <> head.parameter_index
            || (not
                  (same_option ( == ) predecessor receipt.default_predecessor))
            || receipt.default_type_specifier != head.parameter_type_specifier
            || (not
                  (same_list receipt.default_register_qualifiers
                     head.parameter_register_qualifiers))
            || (not
                  (same_list receipt.default_pointer_layers
                     head.parameter_pointer_layers))
            || (not
                  (same_option ( == ) receipt.default_parameter_name
                     head.parameter_name))
            || not
                 (same_option ( == ) receipt.default_function_pointer
                    head.parameter_function_pointer)
          then
            Error "provisional default substituted its original member children"
          else
            Ok
              {
                state with
                members_rev = { member with default = Some receipt } :: rest;
              }
      | _ -> Error "provisional default has no original unfinished member")
  | Parser.Function_parameter_completed completed -> (
      match state.members_rev with
      | member :: rest
        when member.source == completed.parameter_publication
             && Option.is_none member.completion
             && Option.is_none state.variadic ->
          if
            (not (head_matches member.source completed.parameter_ast))
            || not
                 (same_option ( == )
                    (Option.map
                       (fun receipt -> receipt.Parser.default_ast)
                       member.default)
                    completed.parameter_ast.default)
          then
            Error
              "provisional completion substituted its original member or \
               default"
          else
            Ok
              {
                state with
                members_rev =
                  { member with completion = Some completed } :: rest;
              }
      | _ -> Error "provisional completion has no original unfinished member")
  | Parser.Function_variadic_started source ->
      if
        (not (ready_for_member state))
        || not
             (same_option ( == ) source.variadic_parameter_predecessor
                (completed_predecessor state))
      then Error "provisional ellipsis is repeated, incomplete or out of order"
      else Ok { state with variadic = Some source }
  | Parser.Function_variadic_completed source ->
      if
        state.variadic_completed
        || not (same_option ( == ) state.variadic (Some source))
      then Error "provisional ellipsis members lack their original start"
      else Ok { state with variadic_completed = true }
  | Parser.Function_header_completed header ->
      let members = List.rev state.members_rev in
      let completions =
        List.filter_map (fun member -> member.completion) members
      in
      if
        List.length completions <> List.length members
        || (not (same_list completions header.parameter_completions))
        || (not
              (same_list
                 (List.map (fun p -> p.Parser.parameter_ast) completions)
                 header.parameters))
        || (not (same_option ( == ) state.variadic header.variadic_publication))
        || (not
              (same_option ( == )
                 (Option.map (fun p -> p.Parser.variadic_marker) state.variadic)
                 header.variadic))
        || (Option.is_some state.variadic && not state.variadic_completed)
      then
        Error
          "completed header lacks its original provisional member transcript"
      else Ok { state with header = Some header }
  | _ -> Error "event is not a provisional function member phase"

let observe ?activation record event =
  let state = record.current in
  if not (event_belongs record event) then
    Error "provisional member event belongs to another function"
  else if Option.is_some state.header then
    Error "provisional function header has already completed"
  else if
    not
      (event_is_current event
      || Option.fold ~none:false
           ~some:(fun active ->
             Source_activation.owns_namespace active state.namespace)
           activation
         && Source_activation.declaration activation event)
  then Error "provisional member event is outside its original callback"
  else
    Result.map (fun updated -> record.current <- updated) (update state event)
