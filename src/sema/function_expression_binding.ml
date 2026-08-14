type event =
  | Identifier of { name : string; origin : Symbol.origin }
  | Publish_local of {
      name : string;
      origin : Symbol.origin;
      declaration_index : int;
      declarator_index : int;
    }
  | No_warn_target of { name : string; origin : Symbol.origin }
  | Reset_local_initializer of {
      name : string;
      origin : Symbol.origin;
      declaration_index : int;
      declarator_index : int;
    }

type function_input = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  events : event list;
}

type resolution =
  | Function_binding of Function_binding_index.binding
  | Nonlocal_candidate

type occurrence = {
  index : int;
  name : string;
  origin : Symbol.origin;
  resolution : resolution;
}

type suppression = {
  index : int;
  name : string;
  origin : Symbol.origin;
  binding : Function_binding_index.binding;
}

type initializer_use_reset = {
  index : int;
  origin : Symbol.origin;
  binding : Function_binding_index.binding;
}

type binding_event =
  | Bound_use of occurrence
  | No_warn_suppression of suppression
  | Initializer_use_reset of initializer_use_reset

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  binding_events : binding_event list;
  occurrences : occurrence list;
  suppressions : suppression list;
  initializer_use_resets : initializer_use_reset list;
}

module Int_map = Map.Make (Int)
module String_map = Map.Make (String)

type t = {
  table : Symbol_table.t;
  functions : resolved_function list;
  by_symbol : resolved_function Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Publication_mismatch of {
      function_symbol : Symbol.t;
      name : string;
      declaration_index : int;
      declarator_index : int;
      expected : Symbol.t option;
    }
  | Missing_publication of {
      function_symbol : Symbol.t;
      binding : Symbol.t;
      declaration_index : int;
      declarator_index : int;
    }
  | Suppression_mismatch of { function_symbol : Symbol.t; name : string }
  | Initializer_reset_mismatch of {
      function_symbol : Symbol.t;
      name : string;
      declaration_index : int;
      declarator_index : int;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let functions result = result.functions
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index (function_ : resolved_function) = function_.item_index
let function_binding_events function_ = function_.binding_events
let function_occurrences (function_ : resolved_function) = function_.occurrences
let function_suppressions function_ = function_.suppressions
let function_initializer_use_resets function_ = function_.initializer_use_resets
let occurrence_index (occurrence : occurrence) = occurrence.index
let occurrence_name (occurrence : occurrence) = occurrence.name
let occurrence_origin (occurrence : occurrence) = occurrence.origin
let occurrence_resolution (occurrence : occurrence) = occurrence.resolution
let suppression_index (suppression : suppression) = suppression.index
let suppression_name (suppression : suppression) = suppression.name
let suppression_origin (suppression : suppression) = suppression.origin
let suppression_binding (suppression : suppression) = suppression.binding
let initializer_use_reset_index (reset : initializer_use_reset) = reset.index
let initializer_use_reset_origin (reset : initializer_use_reset) = reset.origin

let initializer_use_reset_binding (reset : initializer_use_reset) =
  reset.binding

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let invalid_input message =
  { code = "HCSEMA0017"; kind = Invalid_input message; origin = None }

let publication_mismatch function_symbol name declaration_index declarator_index
    expected origin =
  {
    code = "HCSEMA0018";
    kind =
      Publication_mismatch
        { function_symbol; name; declaration_index; declarator_index; expected };
    origin = Some origin;
  }

let missing_publication function_symbol
    (binding : Function_binding_index.binding) =
  {
    code = "HCSEMA0019";
    kind =
      Missing_publication
        {
          function_symbol;
          binding = binding.symbol;
          declaration_index = Option.get binding.local_declaration_index;
          declarator_index = Option.get binding.local_declarator_index;
        };
    origin = Some (Symbol.origin binding.symbol);
  }

let suppression_mismatch function_symbol name origin =
  {
    code = "HCSEMA0031";
    kind = Suppression_mismatch { function_symbol; name };
    origin = Some origin;
  }

let initializer_reset_mismatch function_symbol name declaration_index
    declarator_index origin =
  {
    code = "HCSEMA0032";
    kind =
      Initializer_reset_mismatch
        { function_symbol; name; declaration_index; declarator_index };
    origin = Some origin;
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Publication_mismatch
      { function_symbol; name; declaration_index; declarator_index; expected }
    -> (
      match expected with
      | None ->
          Printf.sprintf
            "function %S publishes unexpected local %S at declaration %d, \
             declarator %d"
            (Symbol.name function_symbol)
            name declaration_index declarator_index
      | Some expected ->
          Printf.sprintf
            "function %S publishes local %S at declaration %d, declarator %d; \
             the next indexed binding is %S"
            (Symbol.name function_symbol)
            name declaration_index declarator_index (Symbol.name expected))
  | Missing_publication
      { function_symbol; binding; declaration_index; declarator_index } ->
      Printf.sprintf
        "function %S never publishes indexed local %S at declaration %d, \
         declarator %d"
        (Symbol.name function_symbol)
        (Symbol.name binding) declaration_index declarator_index
  | Suppression_mismatch { function_symbol; name } ->
      Printf.sprintf
        "function %S cannot bind no_warn target %S at this source position"
        (Symbol.name function_symbol)
        name
  | Initializer_reset_mismatch
      { function_symbol; name; declaration_index; declarator_index } ->
      Printf.sprintf
        "function %S cannot reset initializer uses for %S at declaration %d, \
         declarator %d"
        (Symbol.name function_symbol)
        name declaration_index declarator_index

let error_to_string error = error.code ^ ": " ^ error_message error

let valid_origin = function
  | Symbol.Pinned_source { path; line } ->
      (not (String.equal path "")) && line >= 1
  | Symbol.Source_location _ -> true
  | Symbol.Synthesized description -> not (String.equal description "")

let make_identifier ~name ~origin =
  if String.equal name "" then
    Error "function expression identifier cannot be empty"
  else if not (valid_origin origin) then
    Error "function expression identifier has an invalid source origin"
  else Ok (Identifier { name; origin })

let make_local_publication ~name ~origin ~declaration_index ~declarator_index =
  if String.equal name "" then Error "local publication name cannot be empty"
  else if not (valid_origin origin) then
    Error "local publication has an invalid source origin"
  else if declaration_index < 0 || declarator_index < 0 then
    Error "local publication position cannot be negative"
  else Ok (Publish_local { name; origin; declaration_index; declarator_index })

let make_no_warn_suppression ~name ~origin =
  if String.equal name "" then Error "no_warn target name cannot be empty"
  else if not (valid_origin origin) then
    Error "no_warn target has an invalid source origin"
  else Ok (No_warn_target { name; origin })

let make_initializer_use_reset ~name ~origin ~declaration_index
    ~declarator_index =
  if String.equal name "" then
    Error "initializer use-reset name cannot be empty"
  else if not (valid_origin origin) then
    Error "initializer use reset has an invalid source origin"
  else if declaration_index < 0 || declarator_index < 0 then
    Error "initializer use-reset position cannot be negative"
  else
    Ok
      (Reset_local_initializer
         { name; origin; declaration_index; declarator_index })

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let function_scope_matches_symbol symbol scope =
  match (Symbol_table.scope_name scope, Symbol_table.parent scope) with
  | Some name, Some parent ->
      String.equal name (Symbol.name symbol)
      && Symbol.Scope_id.equal
           (Symbol_table.scope_id parent)
           (Symbol.scope_id symbol)
  | None, _ | _, None -> false

let make_function ~symbol ~scope ~item_index events =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "function expression owner must be a function symbol"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "function expression binding requires a function scope"
  else if not (function_scope_matches_symbol symbol scope) then
    Error "function expression scope does not match its function symbol"
  else if item_index < 0 then
    Error "function expression item index cannot be negative"
  else Ok { symbol; scope; item_index; events }

let binding_is_parameter (binding : Function_binding_index.binding) =
  match binding.kind with
  | Function_binding_index.Named_parameter
  | Function_binding_index.Variadic_argc
  | Function_binding_index.Variadic_argv -> true
  | Function_binding_index.Automatic_local | Function_binding_index.Static_local
    -> false

let binding_is_local binding = not (binding_is_parameter binding)

let validate_indexed_function table parent indexed =
  let symbol = Function_binding_index.function_symbol indexed in
  let scope = Function_binding_index.function_scope indexed in
  if not (Symbol_table.owns_symbol table symbol) then
    Error
      (invalid_input "function binding index belongs to another symbol table")
  else if not (Symbol_table.owns_scope table scope) then
    Error
      (invalid_input "function binding scope belongs to another symbol table")
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error (invalid_input "function binding index has a nonfunction owner")
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error (invalid_input "function binding index has a nonfunction scope")
  else if not (function_scope_matches_symbol symbol scope) then
    Error (invalid_input "function binding scope does not match its owner")
  else if
    match Symbol_table.parent scope with
    | Some scope_parent -> not (same_scope scope_parent parent)
    | None -> true
  then
    Error (invalid_input "function binding scope has the wrong module parent")
  else
    let rec validate_bindings = function
      | [] -> Ok ()
      | (binding : Function_binding_index.binding) :: rest ->
          if not (Symbol_table.owns_symbol table binding.symbol) then
            Error
              (invalid_input
                 "function binding entry belongs to another symbol table")
          else if
            not
              (Symbol.Scope_id.equal
                 (Symbol.scope_id binding.symbol)
                 (Symbol_table.scope_id scope))
          then
            Error
              (invalid_input
                 "function binding entry has the wrong function scope")
          else validate_bindings rest
    in
    validate_bindings (Function_binding_index.function_bindings indexed)

let validate_function_input table parent indexed (input : function_input) =
  let expected_symbol = Function_binding_index.function_symbol indexed in
  let expected_scope = Function_binding_index.function_scope indexed in
  let expected_item = Function_binding_index.function_item_index indexed in
  if not (Symbol_table.owns_symbol table input.symbol) then
    Error
      (invalid_input "function expression owner belongs to another symbol table")
  else if not (Symbol_table.owns_scope table input.scope) then
    Error
      (invalid_input "function expression scope belongs to another symbol table")
  else if
    not (Symbol.Id.equal (Symbol.id input.symbol) (Symbol.id expected_symbol))
  then
    Error
      (invalid_input
         "function expression owner does not match its binding index")
  else if not (same_scope input.scope expected_scope) then
    Error
      (invalid_input
         "function expression scope does not match its binding index")
  else if input.item_index <> expected_item then
    Error
      (invalid_input
         "function expression module position does not match its binding index")
  else validate_indexed_function table parent indexed

let add_first name binding environment =
  if String_map.mem name environment then environment
  else String_map.add name binding environment

let initial_environment bindings =
  List.fold_left
    (fun environment (binding : Function_binding_index.binding) ->
      if binding_is_parameter binding then
        add_first (Symbol.name binding.symbol) binding environment
      else environment)
    String_map.empty bindings

let local_bindings bindings = List.filter binding_is_local bindings

let publication_matches event (binding : Function_binding_index.binding) =
  match event with
  | Identifier _ | No_warn_target _ | Reset_local_initializer _ -> false
  | Publish_local { name; origin; declaration_index; declarator_index } ->
      String.equal name (Symbol.name binding.symbol)
      && Symbol.origin binding.symbol = origin
      && binding.local_declaration_index = Some declaration_index
      && binding.local_declarator_index = Some declarator_index

let reset_matches name origin declaration_index declarator_index
    (binding : Function_binding_index.binding) =
  String.equal name (Symbol.name binding.symbol)
  && Symbol.origin binding.symbol = origin
  && binding.local_declaration_index = Some declaration_index
  && binding.local_declarator_index = Some declarator_index

let local_binding_at bindings declaration_index declarator_index =
  List.find_opt
    (fun (binding : Function_binding_index.binding) ->
      binding.local_declaration_index = Some declaration_index
      && binding.local_declarator_index = Some declarator_index)
    bindings

module Int_set = Set.Make (Int)

let resolve_function indexed (input : function_input) =
  let bindings = Function_binding_index.function_bindings indexed in
  let rec events environment published remaining_locals binding_events_rev
      next_occurrence next_suppression next_reset = function
    | [] -> (
        match remaining_locals with
        | [] ->
            let binding_events = List.rev binding_events_rev in
            let occurrences, suppressions, initializer_use_resets =
              List.fold_left
                (fun (occurrences, suppressions, resets) -> function
                  | Bound_use occurrence ->
                      (occurrence :: occurrences, suppressions, resets)
                  | No_warn_suppression suppression ->
                      (occurrences, suppression :: suppressions, resets)
                  | Initializer_use_reset reset ->
                      (occurrences, suppressions, reset :: resets))
                ([], [], []) binding_events
            in
            Ok
              {
                symbol = input.symbol;
                scope = input.scope;
                item_index = input.item_index;
                binding_events;
                occurrences = List.rev occurrences;
                suppressions = List.rev suppressions;
                initializer_use_resets = List.rev initializer_use_resets;
              }
        | binding :: _ -> Error (missing_publication input.symbol binding))
    | Identifier { name; origin } :: rest ->
        if next_occurrence = max_int then
          Error
            (invalid_input "function expression occurrence space is exhausted")
        else
          let resolution =
            match String_map.find_opt name environment with
            | Some binding -> Function_binding binding
            | None -> Nonlocal_candidate
          in
          let occurrence =
            { index = next_occurrence; name; origin; resolution }
          in
          events environment published remaining_locals
            (Bound_use occurrence :: binding_events_rev)
            (next_occurrence + 1) next_suppression next_reset rest
    | (Publish_local publication as event) :: rest -> (
        match remaining_locals with
        | binding :: local_rest when publication_matches event binding ->
            let environment = add_first publication.name binding environment in
            events environment
              (Int_set.add (symbol_number binding.symbol) published)
              local_rest binding_events_rev next_occurrence next_suppression
              next_reset rest
        | binding :: _ ->
            Error
              (publication_mismatch input.symbol publication.name
                 publication.declaration_index publication.declarator_index
                 (Some binding.symbol) publication.origin)
        | [] ->
            Error
              (publication_mismatch input.symbol publication.name
                 publication.declaration_index publication.declarator_index None
                 publication.origin))
    | No_warn_target { name; origin } :: rest -> (
        match String_map.find_opt name environment with
        | None -> Error (suppression_mismatch input.symbol name origin)
        | Some binding ->
            if next_suppression = max_int then
              Error
                (invalid_input "function no_warn suppression space is exhausted")
            else
              let suppression =
                { index = next_suppression; name; origin; binding }
              in
              events environment published remaining_locals
                (No_warn_suppression suppression :: binding_events_rev)
                next_occurrence (next_suppression + 1) next_reset rest)
    | Reset_local_initializer
        { name; origin; declaration_index; declarator_index }
      :: rest -> (
        match local_binding_at bindings declaration_index declarator_index with
        | Some binding
          when reset_matches name origin declaration_index declarator_index
                 binding
               && Int_set.mem (symbol_number binding.symbol) published ->
            if next_reset = max_int then
              Error
                (invalid_input
                   "function initializer use-reset space is exhausted")
            else
              let reset = { index = next_reset; origin; binding } in
              events environment published remaining_locals
                (Initializer_use_reset reset :: binding_events_rev)
                next_occurrence next_suppression (next_reset + 1) rest
        | Some _ | None ->
            Error
              (initializer_reset_mismatch input.symbol name declaration_index
                 declarator_index origin))
  in
  events
    (initial_environment bindings)
    Int_set.empty (local_bindings bindings) [] 0 0 0 input.events

let resolve_validated indexed_functions inputs =
  let rec loop functions_rev by_symbol indexed_functions inputs =
    match (indexed_functions, inputs) with
    | [], [] -> Ok (List.rev functions_rev, by_symbol)
    | indexed :: indexed_rest, input :: input_rest -> (
        match resolve_function indexed input with
        | Error _ as error -> error
        | Ok function_ ->
            loop
              (function_ :: functions_rev)
              (Int_map.add (symbol_number function_.symbol) function_ by_symbol)
              indexed_rest input_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input
             "function expression inputs do not match the binding index count")
  in
  loop [] Int_map.empty indexed_functions inputs

let resolve ~table ~parent ~bindings (inputs : function_input list) =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input
         "function expression parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "function expression binding requires a module scope")
  else
    let indexed_functions = Function_binding_index.functions bindings in
    let rec validate indexed_functions inputs =
      match (indexed_functions, inputs) with
      | [], [] -> Ok ()
      | indexed :: indexed_rest, input :: input_rest -> (
          match validate_function_input table parent indexed input with
          | Error _ as error -> error
          | Ok () -> validate indexed_rest input_rest)
      | [], _ :: _ | _ :: _, [] ->
          Error
            (invalid_input
               "function expression inputs do not match the binding index count")
    in
    match validate indexed_functions inputs with
    | Error _ as error -> error
    | Ok () ->
        Result.map
          (fun (functions, by_symbol) -> { table; functions; by_symbol })
          (resolve_validated indexed_functions inputs)

let find_function result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when function_.symbol == symbol -> Some function_
    | Some _ | None -> None
