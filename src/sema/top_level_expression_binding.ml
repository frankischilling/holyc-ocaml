type identifier_event = { name : string; origin : Symbol.origin }

type query_event = {
  role : Function_expression_binding.query_role;
  name : string;
  origin : Symbol.origin;
}

type event = Identifier of identifier_event | Name_query of query_event

let make_identifier ~name ~origin =
  if String.length name = 0 then
    Error "top-level expression identifier cannot be empty"
  else Ok (Identifier { name; origin })

let make_name_query ~role ~name ~origin =
  if String.length name = 0 then
    Error "top-level expression query cannot be empty"
  else Ok (Name_query { role; name; origin })

type input = {
  statement_index : int;
  item_index : int;
  origin : Symbol.origin;
  events : event list;
  initial_owner :
    (Global_initializer_binding.t * Global_initializer_binding.resolved_global)
    option;
}

let make_statement ~statement_index ~item_index ~origin events =
  if statement_index < 0 then
    Error "top-level statement index cannot be negative"
  else if item_index < 0 then Error "top-level item index cannot be negative"
  else Ok { statement_index; item_index; origin; events; initial_owner = None }

let make_global_initializer ~statement_index ~initializers ~global events =
  let symbol = Global_initializer_binding.global_symbol global in
  match Global_initializer_binding.find_global initializers symbol with
  | Some expected when expected == global -> (
      match Global_initializer_binding.global_initializer_origin global with
      | None -> Error "global initializer owner has no initializer"
      | Some origin -> (
          match
            make_statement ~statement_index
              ~item_index:(Global_initializer_binding.global_item_index global)
              ~origin events
          with
          | Error _ as error -> error
          | Ok input ->
              Ok { input with initial_owner = Some (initializers, global) }))
  | _ -> Error "global initializer owner belongs to another binding batch"

type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_candidate

type occurrence = {
  index : int;
  source : identifier_event;
  resolution : resolution;
}

type query = { index : int; source : query_event; resolution : resolution }

type statement = {
  source : input;
  occurrences : occurrence list;
  queries : query list;
}

type t = {
  table : Symbol_table.t;
  module_expressions_ : Module_expression_binding.t;
  statements_ : statement list;
  all_occurrences_ : occurrence list;
  all_queries_ : query list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input message =
  { code = "HCSEMA0052"; kind = Invalid_input message; origin = None }

let owns_table result table = result.table == table

let owns_module_expressions result module_expressions =
  result.module_expressions_ == module_expressions

let module_expressions result = result.module_expressions_
let statements result = result.statements_
let all_occurrences result = result.all_occurrences_
let all_queries result = result.all_queries_
let statement_source (statement : statement) = statement.source
let statement_index (statement : statement) = statement.source.statement_index
let statement_item_index (statement : statement) = statement.source.item_index
let statement_origin (statement : statement) = statement.source.origin
let statement_occurrences (statement : statement) = statement.occurrences
let statement_queries (statement : statement) = statement.queries

let statement_initializer (statement : statement) =
  Option.map snd statement.source.initial_owner

let initializer_bindings result =
  List.find_map
    (fun (statement : statement) ->
      Option.map fst statement.source.initial_owner)
    result.statements_

let occurrence_index (occurrence : occurrence) = occurrence.index
let occurrence_name (occurrence : occurrence) = occurrence.source.name
let occurrence_origin (occurrence : occurrence) = occurrence.source.origin
let occurrence_resolution (occurrence : occurrence) = occurrence.resolution
let query_index (query : query) = query.index
let query_role (query : query) = query.source.role
let query_name (query : query) = query.source.name
let query_origin (query : query) = query.source.origin
let query_resolution (query : query) = query.resolution
let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error

let symbol_in_scope symbol scope =
  Symbol.Scope_id.equal (Symbol.scope_id symbol) (Symbol_table.scope_id scope)

let validate_publications table parent publications =
  let rec loop expected_index previous_item = function
    | [] -> Ok ()
    | publication :: rest ->
        let source =
          Module_expression_binding.publication_source_symbol publication
        in
        let canonical =
          Module_expression_binding.publication_canonical_symbol publication
        in
        let declaration_index =
          Module_expression_binding.publication_declaration_index publication
        in
        let item_index =
          Module_expression_binding.publication_item_index publication
        in
        if declaration_index <> expected_index then
          Error
            (invalid_input
               "top-level module publication indexes are not contiguous")
        else if item_index < previous_item then
          Error
            (invalid_input
               "top-level module publications do not follow source order")
        else if
          not
            (Symbol_table.owns_symbol table source
            && Symbol_table.owns_symbol table canonical)
        then
          Error
            (invalid_input
               "top-level module publication belongs to another symbol table")
        else if
          not (symbol_in_scope source parent && symbol_in_scope canonical parent)
        then
          Error
            (invalid_input
               "top-level module publication has the wrong module scope")
        else loop (expected_index + 1) item_index rest
  in
  loop 0 (-1) publications

let validate_inputs inputs =
  let ordered previous input =
    match previous with
    | None -> true
    | Some previous when previous.item_index < input.item_index -> true
    | Some previous when previous.item_index = input.item_index -> (
        match (previous.initial_owner, input.initial_owner) with
        | Some (_, left), Some (_, right) ->
            Module_expression_binding.publication_declaration_index
              (Global_initializer_binding.global_publication left)
            < Module_expression_binding.publication_declaration_index
                (Global_initializer_binding.global_publication right)
        | _ -> false)
    | Some _ -> false
  in
  let rec loop expected_statement previous = function
    | [] -> Ok ()
    | input :: rest ->
        if input.statement_index <> expected_statement then
          Error (invalid_input "top-level statement indexes are not contiguous")
        else if not (ordered previous input) then
          Error
            (invalid_input "top-level statements do not follow source order")
        else loop (expected_statement + 1) (Some input) rest
  in
  loop 0 None inputs

module String_map = Map.Make (String)

let add_publication visible publication =
  let symbol =
    Module_expression_binding.publication_source_symbol publication
  in
  String_map.add (Symbol.name symbol) publication visible

let rec publish_before item_index visible = function
  | publication :: rest
    when Module_expression_binding.publication_item_index publication
         < item_index ->
      publish_before item_index (add_publication visible publication) rest
  | publications -> (visible, publications)

let publish_for_input input visible publications =
  match input.initial_owner with
  | None -> publish_before input.item_index visible publications
  | Some (_, global) ->
      let last =
        global |> Global_initializer_binding.global_publication
        |> Module_expression_binding.publication_declaration_index
      in
      let rec loop visible = function
        | publication :: rest
          when Module_expression_binding.publication_declaration_index
                 publication
               <= last -> loop (add_publication visible publication) rest
        | rest -> (visible, rest)
      in
      loop visible publications

let validate_initializer_occurrences input occurrences =
  match input.initial_owner with
  | None -> Ok ()
  | Some (_, global) ->
      let rec same actual expected =
        match (actual, expected) with
        | [], [] -> true
        | occurrence :: rest, selected :: tail ->
            occurrence_name occurrence
            = Global_initializer_binding.occurrence_name selected
            && occurrence_origin occurrence
               = Global_initializer_binding.occurrence_origin selected
            && Global_initializer_binding.occurrence_initializer_path selected
               = []
            && (match
                  ( occurrence_resolution occurrence,
                    Global_initializer_binding.occurrence_resolution selected )
                with
              | ( Module_binding actual,
                  Global_initializer_binding.Module_binding expected ) ->
                  actual == expected
              | Outer_candidate, Global_initializer_binding.Outer_binding _ ->
                  true
              | _ -> false)
            && same rest tail
        | _ -> false
      in
      if same occurrences (Global_initializer_binding.global_occurrences global)
      then Ok ()
      else
        Error
          (invalid_input
             "global initializer occurrences do not match their checked owner")

let resolve_events visible next_occurrence next_query events =
  let resolution name =
    match String_map.find_opt name visible with
    | Some publication -> Module_binding publication
    | None -> Outer_candidate
  in
  let rec loop next_occurrence next_query occurrences_rev queries_rev = function
    | [] ->
        Ok
          ( next_occurrence,
            next_query,
            List.rev occurrences_rev,
            List.rev queries_rev )
    | Identifier source :: rest ->
        let occurrence : occurrence =
          {
            index = next_occurrence;
            source;
            resolution = resolution source.name;
          }
        in
        if next_occurrence = max_int then
          Error
            (invalid_input "top-level occurrence identity space is exhausted")
        else
          loop (next_occurrence + 1) next_query
            (occurrence :: occurrences_rev)
            queries_rev rest
    | Name_query source :: rest ->
        let query : query =
          { index = next_query; source; resolution = resolution source.name }
        in
        if next_query = max_int then
          Error (invalid_input "top-level query identity space is exhausted")
        else
          loop next_occurrence (next_query + 1) occurrences_rev
            (query :: queries_rev) rest
  in
  loop next_occurrence next_query [] [] events

let resolve_validated publications inputs =
  let rec loop visible publications next_occurrence next_query statements_rev
      occurrences_rev queries_rev = function
    | [] ->
        Ok
          ( List.rev statements_rev,
            List.rev occurrences_rev,
            List.rev queries_rev )
    | input :: rest -> (
        let visible, publications =
          publish_for_input input visible publications
        in
        match
          resolve_events visible next_occurrence next_query input.events
        with
        | Error _ as error -> error
        | Ok (next_occurrence, next_query, occurrences, queries) -> (
            match validate_initializer_occurrences input occurrences with
            | Error _ as error -> error
            | Ok () ->
                loop visible publications next_occurrence next_query
                  ({ source = input; occurrences; queries } :: statements_rev)
                  (List.rev_append occurrences occurrences_rev)
                  (List.rev_append queries queries_rev)
                  rest))
  in
  loop String_map.empty publications 0 0 [] [] [] inputs

let resolve ~table ~parent ~module_expressions inputs =
  if not (Symbol_table.owns_scope table parent) then
    Error (invalid_input "top-level expression parent belongs to another table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "top-level expression binding requires a module scope")
  else if not (Module_expression_binding.owns_table module_expressions table)
  then
    Error
      (invalid_input
         "top-level module expressions belong to another symbol table")
  else if
    let first =
      List.find_map (fun input -> Option.map fst input.initial_owner) inputs
    in
    List.exists
      (fun input ->
        match input.initial_owner with
        | None -> false
        | Some (batch, global) ->
            (not (Global_initializer_binding.owns_table batch table))
            || Global_initializer_binding.expressions batch
               != module_expressions
            || (match first with
              | Some expected -> batch != expected
              | None -> true)
            || not
                 (List.exists
                    (( == )
                       (Global_initializer_binding.global_publication global))
                    (Module_expression_binding.publications module_expressions)))
      inputs
  then
    Error
      (invalid_input "global initializer groups have foreign binding evidence")
  else
    let publications =
      Module_expression_binding.publications module_expressions
    in
    match validate_publications table parent publications with
    | Error _ as error -> error
    | Ok () -> (
        match validate_inputs inputs with
        | Error _ as error -> error
        | Ok () -> (
            match resolve_validated publications inputs with
            | Error _ as error -> error
            | Ok (statements_, all_occurrences_, all_queries_) ->
                Ok
                  {
                    table;
                    module_expressions_ = module_expressions;
                    statements_;
                    all_occurrences_;
                    all_queries_;
                  }))
