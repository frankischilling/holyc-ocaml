type function_pointer = Function_type_resolution.function_pointer
type declarator_kind = Object | Function_pointer of function_pointer

type array_dimension = {
  index : int;
  origin : Symbol.origin;
  opening_origin : Symbol.origin;
  expression_origin : Symbol.origin option;
  closing_origin : Symbol.origin;
}

type delimiter_kind = Comma | Semicolon
type delimiter = { kind : delimiter_kind; origin : Symbol.origin }
type initializer_kind = Scalar_initializer | Braced_initializer

type initial_value = {
  kind : initializer_kind;
  origin : Symbol.origin;
  equals_origin : Symbol.origin;
  value_origin : Symbol.origin;
}

type global = {
  symbol : Symbol.t;
  item_index : int;
  declarator_index : int option;
  declarator_origin : Symbol.origin;
  type_reference : Type_reference.t;
  declarator_kind : declarator_kind;
  array_dimensions : array_dimension list;
  initial_value : initial_value option;
  delimiter : delimiter;
}

type t = { globals : global list }

let globals resolution = resolution.globals
let global_symbol global = global.symbol
let global_item_index global = global.item_index
let global_declarator_index global = global.declarator_index
let global_declarator_origin global = global.declarator_origin
let global_type_reference global = global.type_reference
let global_declarator_kind global = global.declarator_kind
let global_array_dimensions global = global.array_dimensions
let global_initializer global = global.initial_value
let global_delimiter global = global.delimiter
let array_dimension_index (dimension : array_dimension) = dimension.index
let array_dimension_origin (dimension : array_dimension) = dimension.origin

let array_dimension_opening_origin (dimension : array_dimension) =
  dimension.opening_origin

let array_dimension_expression_origin (dimension : array_dimension) =
  dimension.expression_origin

let array_dimension_closing_origin (dimension : array_dimension) =
  dimension.closing_origin

let delimiter_kind (delimiter : delimiter) = delimiter.kind
let delimiter_origin (delimiter : delimiter) = delimiter.origin
let initializer_kind (initial_value : initial_value) = initial_value.kind
let initializer_origin (initial_value : initial_value) = initial_value.origin
let initializer_equals_origin initial_value = initial_value.equals_origin
let initializer_value_origin initial_value = initial_value.value_origin
let function_pointer_origin = Function_type_resolution.function_pointer_origin

let function_pointer_opening_origin =
  Function_type_resolution.function_pointer_opening_origin

let function_pointer_indirection_origins =
  Function_type_resolution.function_pointer_indirection_origins

let function_pointer_closing_origin =
  Function_type_resolution.function_pointer_closing_origin

let function_pointer_signature =
  Function_type_resolution.function_pointer_signature

let delimiter_kind_name = function
  | Comma -> "comma"
  | Semicolon -> "semicolon"

let initializer_kind_name = function
  | Scalar_initializer -> "scalar"
  | Braced_initializer -> "braced"

let make_array_dimension ~index ~origin ~opening_origin ?expression_origin
    ~closing_origin () =
  if index < 0 then Error "semantic global array index cannot be negative"
  else if index > 0 && Option.is_none expression_origin then
    Error "only the first semantic global array dimension can be empty"
  else Ok { index; origin; opening_origin; expression_origin; closing_origin }

let make_delimiter ~kind ~origin = { kind; origin }

let make_initializer ~kind ~origin ~equals_origin ~value_origin =
  { kind; origin; equals_origin; value_origin }

let dimensions_are_ordered dimensions =
  let rec check expected = function
    | [] -> true
    | dimension :: rest ->
        dimension.index = expected && check (expected + 1) rest
  in
  check 0 dimensions

let make_global ~symbol ~item_index ?declarator_index ~declarator_origin
    ~type_reference ~declarator_kind ~array_dimensions ~initial_value ~delimiter
    () =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Global_variable) then
    Error "semantic global type requires a global-variable symbol"
  else if item_index < 0 then
    Error "semantic global type item index cannot be negative"
  else if
    Option.fold ~none:false ~some:(fun index -> index < 0) declarator_index
  then Error "semantic global type declarator index cannot be negative"
  else if not (dimensions_are_ordered array_dimensions) then
    Error "semantic global array dimensions must use consecutive indexes"
  else
    Ok
      {
        symbol;
        item_index;
        declarator_index;
        declarator_origin;
        type_reference;
        declarator_kind;
        array_dimensions;
        initial_value;
        delimiter;
      }

let validate_type_reference ~table ~parent reference =
  match Type.base (Type_reference.resolved_type reference) with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol ->
      if not (Symbol_table.owns_symbol table symbol) then
        Error "semantic global type target belongs to a different symbol table"
      else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
      then Error "semantic global type target is not an aggregate-type symbol"
      else if
        not
          (Symbol.Scope_id.equal (Symbol.scope_id symbol)
             (Symbol_table.scope_id parent))
      then Error "semantic global type target does not belong to the module"
      else Ok ()

let rec validate_signature_types ~table ~parent signature =
  let rec validate_parameters = function
    | [] -> Ok ()
    | parameter :: rest -> (
        match
          validate_type_reference ~table ~parent
            (Function_type_resolution.parameter_type_reference parameter)
        with
        | Error _ as error -> error
        | Ok () -> (
            match
              Function_type_resolution.parameter_declarator_kind parameter
            with
            | Function_type_resolution.Object -> validate_parameters rest
            | Function_type_resolution.Function_pointer pointer -> (
                match
                  validate_signature_types ~table ~parent
                    (Function_type_resolution.function_pointer_signature pointer)
                with
                | Error _ as error -> error
                | Ok () -> validate_parameters rest)))
  in
  validate_parameters (Function_type_resolution.signature_parameters signature)

let compare_position left right =
  let item_comparison = Int.compare left.item_index right.item_index in
  if item_comparison <> 0 then item_comparison
  else
    match (left.declarator_index, right.declarator_index) with
    | Some left, Some right -> Int.compare left right
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1

let validate_position previous global =
  match previous with
  | None -> (
      match global.declarator_index with
      | None | Some 0 -> Ok ()
      | Some _ -> Error "semantic global declaration group must start at zero")
  | Some previous -> (
      if compare_position previous global >= 0 then
        Error "semantic global types must follow module source order"
      else if previous.item_index = global.item_index then
        match (previous.declarator_index, global.declarator_index) with
        | Some previous_index, Some index when index = previous_index + 1 ->
            if previous.delimiter.kind <> Comma then
              Error
                "semantic global declaration group ends before its next item"
            else Ok ()
        | Some _, Some _ ->
            Error "semantic global declarator indexes must be consecutive"
        | None, _ | _, None ->
            Error "semantic global declaration group has inconsistent indexes"
      else if previous.delimiter.kind <> Semicolon then
        Error "semantic global declaration group must end with a semicolon"
      else
        match global.declarator_index with
        | None | Some 0 -> Ok ()
        | Some _ -> Error "semantic global declaration group must start at zero"
      )

module Int_set = Set.Make (Int)

let validate_global ~table ~parent ~seen_symbols global =
  let symbol_number = Symbol.Id.to_int (Symbol.id global.symbol) in
  if not (Symbol_table.owns_symbol table global.symbol) then
    Error "semantic global type symbol belongs to a different symbol table"
  else if
    not (Symbol.equal_kind (Symbol.kind global.symbol) Symbol.Global_variable)
  then Error "semantic global type symbol is not a global variable"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id global.symbol)
         (Symbol_table.scope_id parent))
  then Error "semantic global type symbol does not belong to the module"
  else if Int_set.mem symbol_number seen_symbols then
    Error "semantic global type symbols cannot repeat"
  else
    match validate_type_reference ~table ~parent global.type_reference with
    | Error _ as error -> error
    | Ok () -> (
        match global.declarator_kind with
        | Object -> Ok (Int_set.add symbol_number seen_symbols)
        | Function_pointer pointer ->
            Result.map
              (fun () -> Int_set.add symbol_number seen_symbols)
              (validate_signature_types ~table ~parent
                 (Function_type_resolution.function_pointer_signature pointer)))

let resolve ~table ~parent globals =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic global type parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic global types require a module scope"
  else
    let rec validate previous seen_symbols = function
      | [] -> (
          match previous with
          | Some global when global.delimiter.kind <> Semicolon ->
              Error "semantic global declaration group is unterminated"
          | None | Some _ -> Ok { globals })
      | global :: rest -> (
          match validate_position previous global with
          | Error _ as error -> error
          | Ok () -> (
              match validate_global ~table ~parent ~seen_symbols global with
              | Error _ as error -> error
              | Ok seen_symbols -> validate (Some global) seen_symbols rest))
    in
    validate None Int_set.empty globals
