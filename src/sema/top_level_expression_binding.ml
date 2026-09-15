type identifier_event = {
  name : string;
  origin : Symbol.origin;
  initializer_leaf : Initializer_source.leaf option;
  selection : Reference_selection.t option;
}

type query_event = {
  role : Function_expression_binding.query_role;
  name : string;
  origin : Symbol.origin;
  selection : Query_selection.t option;
  initializer_leaf : Initializer_source.leaf option;
}

type event = Identifier of identifier_event | Name_query of query_event

let make_identifier ~name ~origin =
  if String.length name = 0 then
    Error "top-level expression identifier cannot be empty"
  else
    Ok (Identifier { name; origin; initializer_leaf = None; selection = None })

let make_initializer_identifier ~leaf ~name ~origin =
  if
    not
      (List.exists
         (fun (expected_name, expected_origin) ->
           name = expected_name && origin = expected_origin)
         (Initializer_source.leaf_identifiers leaf))
  then Error "initializer identifier is absent from its retained source leaf"
  else
    Ok
      (Identifier
         { name; origin; initializer_leaf = Some leaf; selection = None })

let with_selection selection = function
  | Ok (Identifier source) ->
      Ok (Identifier { source with selection = Some selection })
  | Ok (Name_query _) -> assert false
  | Error _ as error -> error

let make_selected_identifier ~selection ~name ~origin =
  make_identifier ~name ~origin |> with_selection selection

let make_selected_initializer_identifier ~selection ~leaf ~name ~origin =
  make_initializer_identifier ~leaf ~name ~origin |> with_selection selection

let make_name_query ~role ~name ~origin =
  if String.length name = 0 then
    Error "top-level expression query cannot be empty"
  else
    Ok
      (Name_query
         { role; name; origin; selection = None; initializer_leaf = None })

let make_selected_name_query ~selection ~role ~name ~origin =
  make_name_query ~role ~name ~origin
  |> Result.map (function
    | Name_query query -> Name_query { query with selection = Some selection }
    | Identifier _ -> assert false)

let make_initializer_name_query ?selection ~leaf ~role ~name ~origin () =
  if
    not
      (List.exists
         (fun expression ->
           Query_selection.name_query_facts expression
           = Some (role, name, origin)
           && Option.fold ~none:true
                ~some:(fun selection ->
                  Query_selection.expression selection == expression)
                selection)
         (Query_selection.source_queries
            (Initializer_source.leaf_expression_ast leaf)))
  then Error "initializer query is absent from its retained source leaf"
  else
    make_name_query ~role ~name ~origin
    |> Result.map (function
      | Name_query query ->
          Name_query { query with selection; initializer_leaf = Some leaf }
      | Identifier _ -> assert false)

type input = {
  original_statement : Frontend.Ast.statement option;
  statement_index : int;
  item_index : int;
  origin : Symbol.origin;
  events : event list;
  initial_owner :
    (Global_initializer_binding.t * Global_initializer_binding.resolved_global)
    option;
  fragment_owner : Initializer_fragment.t option;
  default_owner : Default_fragment.t option;
  dimension_owner : Dimension_fragment.t option;
  offset_owner : Offset_fragment.t option;
}

let make_statement ~statement_index ~item_index ~origin events =
  if statement_index < 0 then
    Error "top-level statement index cannot be negative"
  else if item_index < 0 then Error "top-level item index cannot be negative"
  else
    Ok
      {
        original_statement = None;
        statement_index;
        item_index;
        origin;
        events;
        initial_owner = None;
        fragment_owner = None;
        default_owner = None;
        dimension_owner = None;
        offset_owner = None;
      }

let make_source_statement ~source ~statement_index ~item_index events =
  make_statement ~statement_index ~item_index
    ~origin:
      (Initializer_source.origin_of_location
         (Frontend.Ast.statement_location source))
    events
  |> Result.map (fun input -> { input with original_statement = Some source })

let make_fragment_input ~leaf ~origin ~references ~source_queries
    ~fragment_owner ~default_owner ~dimension_owner ~offset_owner events =
  let same_leaf actual =
    match (leaf, actual) with
    | None, None -> true
    | Some expected, Some actual -> expected == actual
    | _ -> false
  in
  let identifiers =
    List.filter_map
      (function
        | Identifier value -> Some value
        | _ -> None)
      events
  in
  let queries =
    List.filter_map
      (function
        | Name_query value -> Some value
        | _ -> None)
      events
  in
  let expected_queries =
    source_queries
    |> List.filter_map (fun selection ->
        Option.map
          (fun facts -> (selection, facts))
          (Query_selection.name_query_facts
             (Query_selection.expression selection)))
  in
  if
    List.length identifiers <> List.length references
    || (not
          (List.for_all2
             (fun (actual : identifier_event)
                  ((identifier : Frontend.Ast.identifier), selection) ->
               actual.name = identifier.spelling
               && actual.origin
                  = Initializer_source.origin_of_location identifier.location
               && same_leaf actual.initializer_leaf
               && Option.fold ~none:false ~some:(( == ) selection)
                    actual.selection)
             identifiers references))
    || List.length queries <> List.length expected_queries
    || not
         (List.for_all2
            (fun (actual : query_event) (selection, (role, name, origin)) ->
              actual.role = role && actual.name = name && actual.origin = origin
              && same_leaf actual.initializer_leaf
              && Option.fold ~none:false ~some:(( == ) selection)
                   actual.selection)
            queries expected_queries)
  then
    Error "initializer fragment events differ from its checked leaf transcript"
  else
    Ok
      {
        original_statement = None;
        statement_index = 0;
        item_index = 0;
        origin;
        events;
        initial_owner = None;
        fragment_owner;
        default_owner;
        dimension_owner;
        offset_owner;
      }

let make_initializer_fragment ~fragment events =
  let leaf = Initializer_fragment.leaf fragment in
  make_fragment_input ~leaf:(Some leaf)
    ~origin:(Initializer_source.leaf_origin leaf)
    ~references:(Initializer_fragment.references fragment)
    ~source_queries:(Initializer_fragment.queries fragment)
    ~fragment_owner:(Some fragment) ~default_owner:None ~dimension_owner:None
    ~offset_owner:None events

let make_default_fragment ~fragment events =
  make_fragment_input ~leaf:None
    ~origin:(Default_fragment.origin fragment)
    ~references:(Default_fragment.references fragment)
    ~source_queries:(Default_fragment.queries fragment)
    ~fragment_owner:None ~default_owner:(Some fragment) ~dimension_owner:None
    ~offset_owner:None events

let make_dimension_fragment ~fragment events =
  make_fragment_input ~leaf:None
    ~origin:(Dimension_fragment.origin fragment)
    ~references:(Dimension_fragment.references fragment)
    ~source_queries:(Dimension_fragment.queries fragment)
    ~fragment_owner:None ~default_owner:None ~dimension_owner:(Some fragment)
    ~offset_owner:None events

let make_offset_fragment ~fragment events =
  make_fragment_input ~leaf:None
    ~origin:(Offset_fragment.origin fragment)
    ~references:(Offset_fragment.references fragment)
    ~source_queries:(Offset_fragment.queries fragment)
    ~fragment_owner:None ~default_owner:None ~offset_owner:(Some fragment)
    ~dimension_owner:None events

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
  initializer_binding : Outer_environment.binding option;
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
let statement_ast statement = statement.source.original_statement
let statement_index (statement : statement) = statement.source.statement_index
let statement_item_index (statement : statement) = statement.source.item_index
let statement_origin (statement : statement) = statement.source.origin
let statement_occurrences (statement : statement) = statement.occurrences
let statement_queries (statement : statement) = statement.queries

let statement_initializer (statement : statement) =
  Option.map snd statement.source.initial_owner

let statement_fragment (statement : statement) = statement.source.fragment_owner
let statement_default (statement : statement) = statement.source.default_owner

let statement_dimension (statement : statement) =
  statement.source.dimension_owner

let statement_offset (statement : statement) = statement.source.offset_owner

let initializer_bindings result =
  List.find_map
    (fun (statement : statement) ->
      Option.map fst statement.source.initial_owner)
    result.statements_

let occurrence_index (occurrence : occurrence) = occurrence.index
let occurrence_name (occurrence : occurrence) = occurrence.source.name
let occurrence_origin (occurrence : occurrence) = occurrence.source.origin
let occurrence_resolution (occurrence : occurrence) = occurrence.resolution
let occurrence_selection (occurrence : occurrence) = occurrence.source.selection

let occurrence_initializer_binding (occurrence : occurrence) =
  occurrence.initializer_binding

let query_index (query : query) = query.index
let query_role (query : query) = query.source.role
let query_name (query : query) = query.source.name
let query_origin (query : query) = query.source.origin
let query_resolution (query : query) = query.resolution
let query_selection (query : query) = query.source.selection
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
  | None -> Ok occurrences
  | Some (batch, global) -> (
      let path_matches (occurrence : occurrence) selected =
        match occurrence.source.initializer_leaf with
        | None ->
            Global_initializer_binding.occurrence_initializer_path selected = []
            && global |> Global_initializer_binding.global_record
               |> Global_resolution.global_record_global
               |> Global_type_resolution.global_array_dimensions = []
        | Some leaf ->
            Initializer_source.leaf_path leaf
            = Global_initializer_binding.occurrence_initializer_path selected
            && Option.fold ~none:false
                 ~some:(fun source -> Initializer_source.owns_leaf source leaf)
                 (Global_initializer_binding.global_source global)
      in
      let rec same actual expected =
        match (actual, expected) with
        | [], [] -> Some []
        | occurrence :: rest, selected :: tail ->
            if
              occurrence_name occurrence
              = Global_initializer_binding.occurrence_name selected
              && occurrence_origin occurrence
                 = Global_initializer_binding.occurrence_origin selected
              && path_matches occurrence selected
            then
              let matched =
                match
                  ( occurrence_resolution occurrence,
                    Global_initializer_binding.occurrence_resolution selected )
                with
                | ( Module_binding actual,
                    Global_initializer_binding.Module_binding expected ) ->
                    if actual == expected then Some occurrence else None
                | ( Outer_candidate,
                    Global_initializer_binding.Outer_binding expected ) ->
                    let matches =
                      match occurrence.source.selection with
                      | None -> true
                      | Some selection -> (
                          match Reference_selection.kind selection with
                          | Reference_selection.Outer (environment, actual) ->
                              actual == expected
                              && environment
                                 == Global_initializer_binding.environment batch
                          | _ -> false)
                    in
                    if matches then
                      Some
                        { occurrence with initializer_binding = Some expected }
                    else None
                | _ -> None
              in
              Option.bind matched (fun occurrence ->
                  Option.map (fun rest -> occurrence :: rest) (same rest tail))
            else None
        | _ -> None
      in
      match
        same occurrences (Global_initializer_binding.global_occurrences global)
      with
      | Some occurrences -> Ok occurrences
      | None ->
          Error
            (invalid_input
               "global initializer occurrences do not match their checked owner")
      )

let validate_initializer_queries input queries =
  match input.initial_owner with
  | None -> Ok ()
  | Some (_, global) ->
      let expected =
        Global_initializer_binding.global_queries global
        |> List.filter_map (fun query ->
            Query_selection.name_query_facts
              (Global_initializer_binding.query_expression query)
            |> Option.map (fun facts -> (query, facts)))
      in
      let rec matches actual expected =
        match (actual, expected) with
        | [], [] -> true
        | (actual : query) :: rest, (expected, (role, name, origin)) :: tail ->
            let leaf = Global_initializer_binding.query_leaf expected in
            let same_leaf =
              match actual.source.initializer_leaf with
              | Some actual_leaf -> actual_leaf == leaf
              | None ->
                  Option.is_none actual.source.selection
                  && Initializer_source.leaf_path leaf = []
                  && global |> Global_initializer_binding.global_record
                     |> Global_resolution.global_record_global
                     |> Global_type_resolution.global_array_dimensions = []
            in
            let same_selection =
              match
                ( actual.source.selection,
                  Global_initializer_binding.query_selection expected )
              with
              | None, None -> true
              | Some actual, Some expected -> actual == expected
              | _ -> false
            in
            same_leaf && same_selection && actual.source.role = role
            && actual.source.name = name
            && actual.source.origin = origin
            && matches rest tail
        | _ -> false
      in
      if matches queries expected then Ok ()
      else
        Error
          (invalid_input
             "global initializer queries do not match their original source \
              reads")

let resolve_events table publications visible next_occurrence next_query events
    =
  let resolution name =
    match String_map.find_opt name visible with
    | Some publication -> Module_binding publication
    | None -> Outer_candidate
  in
  let selected_resolution (source : identifier_event) =
    match source.selection with
    | None -> Ok (resolution source.name)
    | Some selection ->
        Result.bind
          (Reference_selection.validate ~table ~name:source.name selection
          |> Result.map_error invalid_input)
          (fun () ->
            match Reference_selection.kind selection with
            | Reference_selection.Absent
            | Reference_selection.Unavailable
            | Reference_selection.Outer _ -> Ok Outer_candidate
            | Reference_selection.Local ->
                Error (invalid_input "selected local has no top-level binding")
            | Reference_selection.Source
                (_, Reference_selection.Function_declared) ->
                Error
                  (invalid_input
                     "selected function header was still provisional")
            | Reference_selection.Source (symbol, _) -> (
                match
                  List.find_opt
                    (fun publication ->
                      Module_expression_binding.publication_source_symbol
                        publication
                      == symbol)
                    publications
                with
                | Some publication -> Ok (Module_binding publication)
                | None ->
                    Error
                      (invalid_input
                         "selected source declaration is outside the visible \
                          module prefix")))
  in
  let rec loop next_occurrence next_query occurrences_rev queries_rev = function
    | [] ->
        Ok
          ( next_occurrence,
            next_query,
            List.rev occurrences_rev,
            List.rev queries_rev )
    | Identifier source :: rest -> (
        match selected_resolution source with
        | Error _ as error -> error
        | Ok resolution ->
            let occurrence : occurrence =
              {
                index = next_occurrence;
                source;
                resolution;
                initializer_binding = None;
              }
            in
            if next_occurrence = max_int then
              Error
                (invalid_input
                   "top-level occurrence identity space is exhausted")
            else
              loop (next_occurrence + 1) next_query
                (occurrence :: occurrences_rev)
                queries_rev rest)
    | Name_query source :: rest -> (
        let checked =
          match source.selection with
          | None -> Ok ()
          | Some selection ->
              Result.bind
                (Query_selection.validate ~table ~role:source.role
                   ~name:source.name ~origin:source.origin selection
                |> Result.map_error invalid_input)
                (fun () ->
                  if Query_selection.is_local selection then
                    Error
                      (invalid_input
                         "selected local query has no top-level source binding")
                  else Ok ())
        in
        match checked with
        | Error _ as error -> error
        | Ok () ->
            let query : query =
              {
                index = next_query;
                source;
                resolution =
                  (match source.selection with
                  | Some _ -> Outer_candidate
                  | None -> resolution source.name);
              }
            in
            if next_query = max_int then
              Error
                (invalid_input "top-level query identity space is exhausted")
            else
              loop next_occurrence (next_query + 1) occurrences_rev
                (query :: queries_rev) rest)
  in
  loop next_occurrence next_query [] [] events

let resolve_validated table all_publications inputs =
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
          let selected_publications =
            List.filter
              (fun publication ->
                match input.initial_owner with
                | None ->
                    Module_expression_binding.publication_item_index publication
                    < input.item_index
                | Some (_, global) ->
                    Module_expression_binding.publication_declaration_index
                      publication
                    <= (Global_initializer_binding.global_publication global
                       |> Module_expression_binding
                          .publication_declaration_index))
              all_publications
          in
          resolve_events table selected_publications visible next_occurrence
            next_query input.events
        with
        | Error _ as error -> error
        | Ok (next_occurrence, next_query, occurrences, queries) -> (
            match
              Result.bind (validate_initializer_queries input queries)
                (fun () -> validate_initializer_occurrences input occurrences)
            with
            | Error _ as error -> error
            | Ok occurrences ->
                loop visible publications next_occurrence next_query
                  ({ source = input; occurrences; queries } :: statements_rev)
                  (List.rev_append occurrences occurrences_rev)
                  (List.rev_append queries queries_rev)
                  rest))
  in
  loop String_map.empty all_publications 0 0 [] [] [] inputs

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
    List.exists
      (fun input ->
        Option.fold ~none:false
          ~some:(fun fragment ->
            (not (Default_fragment.owns_table fragment table))
            || (not
                  (symbol_in_scope
                     (Declaration_collection.publication_symbol
                        (Default_fragment.publication fragment))
                     parent))
            || Module_expression_binding.publications module_expressions <> []
            || List.length inputs <> 1)
          input.default_owner
        || Option.fold ~none:false
             ~some:(fun fragment ->
               (not (Dimension_fragment.owns_table fragment table))
               || Declaration_collection.namespace_scope
                    (Dimension_fragment.namespace fragment)
                  != parent
               || Module_expression_binding.publications module_expressions
                  <> []
               || List.length inputs <> 1)
             input.dimension_owner
        || Option.fold ~none:false
             ~some:(fun fragment ->
               (not (Offset_fragment.owns_table fragment table))
               || Declaration_collection.namespace_scope
                    (Offset_fragment.namespace fragment)
                  != parent
               || Module_expression_binding.publications module_expressions
                  <> []
               || List.length inputs <> 1)
             input.offset_owner
        ||
        match input.fragment_owner with
        | None -> false
        | Some fragment ->
            (not (Initializer_fragment.owns_table fragment table))
            || (not
                  (symbol_in_scope
                     (Compiler_record.declared_global_symbol
                        (Initializer_fragment.declaration fragment))
                     parent))
            || Module_expression_binding.publications module_expressions <> []
            || List.length inputs <> 1)
      inputs
  then
    Error
      (invalid_input
         "initializer fragment requires its own empty module expression context")
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
            match resolve_validated table publications inputs with
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
