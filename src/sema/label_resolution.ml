type definition_kind =
  | Language_label
  | Assembly_global_label
  | Assembly_exported_global_label
  | Assembly_local_label

type occurrence_kind = Definition of definition_kind | Goto_reference

type occurrence = {
  name : string;
  kind : occurrence_kind;
  origin : Symbol.origin;
  index : int;
}

type function_labels = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  occurrences : occurrence list;
}

type resolved_occurrence = {
  symbol : Symbol.t;
  kind : occurrence_kind;
  origin : Symbol.origin;
  index : int;
}

type label = {
  symbol : Symbol.t;
  definition_kind : definition_kind;
  first_occurrence_index : int;
  goto_count : int;
  use_count : int;
}

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  labels : label list;
  occurrences : resolved_occurrence list;
}

type t = { functions : resolved_function list }

let functions resolution = resolution.functions
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index (function_ : resolved_function) = function_.item_index
let function_labels (function_ : resolved_function) = function_.labels
let function_occurrences (function_ : resolved_function) = function_.occurrences
let label_symbol (label : label) = label.symbol
let label_definition_kind (label : label) = label.definition_kind
let label_first_occurrence_index (label : label) = label.first_occurrence_index
let label_goto_count (label : label) = label.goto_count
let label_use_count (label : label) = label.use_count
let occurrence_symbol (occurrence : resolved_occurrence) = occurrence.symbol
let occurrence_kind (occurrence : resolved_occurrence) = occurrence.kind
let occurrence_origin (occurrence : resolved_occurrence) = occurrence.origin
let occurrence_index (occurrence : resolved_occurrence) = occurrence.index

let definition_kind_name = function
  | Language_label -> "language"
  | Assembly_global_label -> "assembly-global"
  | Assembly_exported_global_label -> "assembly-exported-global"
  | Assembly_local_label -> "assembly-local"

let occurrence_kind_name = function
  | Definition kind -> "definition:" ^ definition_kind_name kind
  | Goto_reference -> "goto-reference"

let check_name name =
  if String.equal name "" then Error "semantic label name cannot be empty"
  else Ok ()

let check_origin = function
  | Symbol.Pinned_source { path; line } ->
      if String.equal path "" then
        Error "pinned semantic symbol path cannot be empty"
      else if line < 1 then Error "pinned semantic symbol line must be positive"
      else Ok ()
  | Symbol.Source_location _ -> Ok ()
  | Symbol.Synthesized description ->
      if String.equal description "" then
        Error "synthesized semantic symbol origin cannot be empty"
      else Ok ()

let make_occurrence ~name ~kind ~origin ~occurrence_index =
  match check_name name with
  | Error _ as error -> error
  | Ok () -> (
      match check_origin origin with
      | Error _ as error -> error
      | Ok () ->
          if occurrence_index < 0 then
            Error "semantic label occurrence index cannot be negative"
          else Ok { name; kind; origin; index = occurrence_index })

let make_definition ~name ~definition_kind ~origin ~occurrence_index =
  make_occurrence ~name ~kind:(Definition definition_kind) ~origin
    ~occurrence_index

let make_goto ~name ~origin ~occurrence_index =
  make_occurrence ~name ~kind:Goto_reference ~origin ~occurrence_index

let validate_occurrences (occurrences : occurrence list) =
  let rec validate previous_index = function
    | [] -> Ok ()
    | (occurrence : occurrence) :: rest ->
        if occurrence.index <= previous_index then
          Error "semantic label occurrences must be in increasing source order"
        else validate occurrence.index rest
  in
  validate (-1) occurrences

let function_scope_matches_symbol symbol scope =
  match (Symbol_table.scope_name scope, Symbol_table.parent scope) with
  | Some name, Some parent ->
      String.equal name (Symbol.name symbol)
      && Symbol.Scope_id.equal
           (Symbol_table.scope_id parent)
           (Symbol.scope_id symbol)
  | None, _ | _, None -> false

let make_function ~symbol ~scope ~item_index occurrences =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic label owner must be a function symbol"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "semantic labels require a function scope"
  else if not (function_scope_matches_symbol symbol scope) then
    Error "semantic label scope does not match its function symbol"
  else if item_index < 0 then
    Error "semantic label function item index cannot be negative"
  else
    Result.map
      (fun () -> { symbol; scope; item_index; occurrences })
      (validate_occurrences occurrences)

module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

let validate_function table previous_item_index seen_symbols seen_scopes
    (function_ : function_labels) =
  let symbol_id = Symbol.Id.to_int (Symbol.id function_.symbol) in
  let scope_id =
    Symbol.Scope_id.to_int (Symbol_table.scope_id function_.scope)
  in
  if not (Symbol_table.owns_symbol table function_.symbol) then
    Error "semantic label owner belongs to a different symbol table"
  else if not (Symbol_table.owns_scope table function_.scope) then
    Error "semantic label scope belongs to a different symbol table"
  else if not (Symbol.equal_kind (Symbol.kind function_.symbol) Symbol.Function)
  then Error "semantic label owner must be a function symbol"
  else if Symbol_table.scope_kind function_.scope <> Symbol_table.Function then
    Error "semantic labels require a function scope"
  else if not (function_scope_matches_symbol function_.symbol function_.scope)
  then Error "semantic label scope does not match its function symbol"
  else if function_.item_index <= previous_item_index then
    Error "semantic label functions must be in increasing item order"
  else if Int_set.mem symbol_id seen_symbols then
    Error "semantic label function symbols cannot repeat"
  else if Int_set.mem scope_id seen_scopes then
    Error "semantic label function scopes cannot repeat"
  else
    match validate_occurrences function_.occurrences with
    | Error _ as error -> error
    | Ok () ->
        Ok
          ( function_.item_index,
            Int_set.add symbol_id seen_symbols,
            Int_set.add scope_id seen_scopes )

let validate table functions =
  let rec validate_all previous_item_index seen_symbols seen_scopes = function
    | [] -> Ok ()
    | function_ :: rest -> (
        match
          validate_function table previous_item_index seen_symbols seen_scopes
            function_
        with
        | Error _ as error -> error
        | Ok (item_index, seen_symbols, seen_scopes) ->
            validate_all item_index seen_symbols seen_scopes rest)
  in
  validate_all (-1) Int_set.empty Int_set.empty functions

type pending_label = {
  name : string;
  first_occurrence_index : int;
  definition : (definition_kind * Symbol.origin) option;
  goto_count : int;
  use_count : int;
}

type prepared_label = {
  name : string;
  first_occurrence_index : int;
  definition_kind : definition_kind;
  definition_origin : Symbol.origin;
  goto_count : int;
  use_count : int;
}

type prepared_function = {
  source : function_labels;
  labels : prepared_label list;
}

let assembly_definition = function
  | Language_label -> false
  | Assembly_global_label
  | Assembly_exported_global_label
  | Assembly_local_label -> true

let increment count error =
  if count = max_int then Error error else Ok (count + 1)

let add_new_pending (occurrence : occurrence) =
  match occurrence.kind with
  | Goto_reference ->
      Ok
        {
          name = occurrence.name;
          first_occurrence_index = occurrence.index;
          definition = None;
          goto_count = 1;
          use_count = 1;
        }
  | Definition kind ->
      Ok
        {
          name = occurrence.name;
          first_occurrence_index = occurrence.index;
          definition = Some (kind, occurrence.origin);
          goto_count = 0;
          use_count = (if assembly_definition kind then 1 else 0);
        }

let update_pending function_name (pending : pending_label)
    (occurrence : occurrence) =
  match occurrence.kind with
  | Goto_reference -> (
      match
        ( increment pending.goto_count "semantic label goto count is exhausted",
          increment pending.use_count "semantic label use count is exhausted" )
      with
      | (Error _ as error), _ | _, (Error _ as error) -> error
      | Ok goto_count, Ok use_count -> Ok { pending with goto_count; use_count }
      )
  | Definition kind -> (
      match pending.definition with
      | Some _ ->
          Error
            (Printf.sprintf "label %S is defined more than once in function %S"
               occurrence.name function_name)
      | None ->
          if assembly_definition kind then
            Result.map
              (fun use_count ->
                {
                  pending with
                  definition = Some (kind, occurrence.origin);
                  use_count;
                })
              (increment pending.use_count
                 "semantic label use count is exhausted")
          else Ok { pending with definition = Some (kind, occurrence.origin) })

let collect_pending function_name (occurrences : occurrence list) =
  let rec collect (pending : pending_label String_map.t) order_rev = function
    | [] -> Ok (pending, List.rev order_rev)
    | (occurrence : occurrence) :: rest -> (
        match String_map.find_opt occurrence.name pending with
        | None -> (
            match add_new_pending occurrence with
            | Error _ as error -> error
            | Ok label ->
                collect
                  (String_map.add occurrence.name label pending)
                  (occurrence.name :: order_rev)
                  rest)
        | Some label -> (
            match update_pending function_name label occurrence with
            | Error _ as error -> error
            | Ok label ->
                collect
                  (String_map.add occurrence.name label pending)
                  order_rev rest))
  in
  collect String_map.empty [] occurrences

let prepare_labels function_name (occurrences : occurrence list) =
  match collect_pending function_name occurrences with
  | Error _ as error -> error
  | Ok (pending, order) ->
      let rec prepare labels_rev = function
        | [] -> Ok (List.rev labels_rev)
        | name :: rest -> (
            let label = String_map.find name pending in
            match label.definition with
            | None ->
                Error
                  (Printf.sprintf "goto target %S is not defined in function %S"
                     name function_name)
            | Some (definition_kind, definition_origin) ->
                prepare
                  ({
                     name = label.name;
                     first_occurrence_index = label.first_occurrence_index;
                     definition_kind;
                     definition_origin;
                     goto_count = label.goto_count;
                     use_count = label.use_count;
                   }
                  :: labels_rev)
                  rest)
      in
      prepare [] order

let prepare (functions : function_labels list) =
  let rec prepare_all prepared_rev = function
    | [] -> Ok (List.rev prepared_rev)
    | (function_ : function_labels) :: rest -> (
        match
          prepare_labels (Symbol.name function_.symbol) function_.occurrences
        with
        | Error _ as error -> error
        | Ok labels ->
            prepare_all ({ source = function_; labels } :: prepared_rev) rest)
  in
  prepare_all [] functions

let validate_available table prepared =
  let rec validate_labels scope function_name = function
    | [] -> Ok ()
    | label :: rest -> (
        match
          Symbol_table.lookup_local table ~scope ~name:label.name
            ~kinds:[ Symbol.Label ] ()
        with
        | Error _ as error -> error
        | Ok None -> validate_labels scope function_name rest
        | Ok (Some _) ->
            Error
              (Printf.sprintf
                 "function %S already has a semantic label named %S"
                 function_name label.name))
  in
  let rec validate_functions = function
    | [] -> Ok ()
    | function_ :: rest -> (
        match
          validate_labels function_.source.scope
            (Symbol.name function_.source.symbol)
            function_.labels
        with
        | Error _ as error -> error
        | Ok () -> validate_functions rest)
  in
  validate_functions prepared

let add_labels table scope labels =
  let rec add resolved_rev by_name = function
    | [] -> Ok (List.rev resolved_rev, by_name)
    | label :: rest -> (
        match
          Symbol_table.add table ~scope ~name:label.name ~kind:Symbol.Label
            ~origin:label.definition_origin
        with
        | Error _ as error -> error
        | Ok symbol ->
            let resolved =
              {
                symbol;
                definition_kind = label.definition_kind;
                first_occurrence_index = label.first_occurrence_index;
                goto_count = label.goto_count;
                use_count = label.use_count;
              }
            in
            add (resolved :: resolved_rev)
              (String_map.add label.name symbol by_name)
              rest)
  in
  add [] String_map.empty labels

let resolve_occurrences by_name (occurrences : occurrence list) =
  List.map
    (fun (occurrence : occurrence) ->
      {
        symbol = String_map.find occurrence.name by_name;
        kind = occurrence.kind;
        origin = occurrence.origin;
        index = occurrence.index;
      })
    occurrences

let add_function table prepared =
  match add_labels table prepared.source.scope prepared.labels with
  | Error _ as error -> error
  | Ok (labels, by_name) ->
      Ok
        {
          symbol = prepared.source.symbol;
          scope = prepared.source.scope;
          item_index = prepared.source.item_index;
          labels;
          occurrences = resolve_occurrences by_name prepared.source.occurrences;
        }

let add_all table prepared =
  let rec add functions_rev = function
    | [] -> Ok { functions = List.rev functions_rev }
    | function_ :: rest -> (
        match add_function table function_ with
        | Error _ as error -> error
        | Ok function_ -> add (function_ :: functions_rev) rest)
  in
  add [] prepared

let resolve ~table function_facts =
  match validate table function_facts with
  | Error _ as error -> error
  | Ok () -> (
      match prepare function_facts with
      | Error _ as error -> error
      | Ok prepared -> (
          match validate_available table prepared with
          | Error _ as error -> error
          | Ok () -> add_all table prepared))
