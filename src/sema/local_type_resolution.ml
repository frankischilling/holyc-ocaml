type function_pointer = Function_type_resolution.function_pointer
type declarator_kind = Object | Function_pointer of function_pointer
type storage = Automatic | Static
type register_request_kind = Allocate | Disable
type register_position = Before_type | After_type

type register_request = {
  request_kind : register_request_kind;
  request_position : register_position;
  request_spelling : string;
  request_origin : Symbol.origin;
  explicit_register : string option;
  explicit_register_origin : Symbol.origin option;
}

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

type local = {
  symbol : Symbol.t;
  declaration_index : int;
  declarator_index : int;
  declaration_origin : Symbol.origin;
  declarator_origin : Symbol.origin;
  storage : storage;
  storage_origins : Symbol.origin list;
  type_reference : Type_reference.t;
  register_requests : register_request list;
  declarator_kind : declarator_kind;
  array_dimensions : array_dimension list;
  initial_value : initial_value option;
  delimiter : delimiter;
}

type function_declaration = {
  function_symbol_ : Symbol.t;
  function_scope_ : Symbol_table.scope;
  function_item_index_ : int;
  function_locals_ : local list;
}

type resolved_function = function_declaration
type t = { functions : resolved_function list }

let functions resolution = resolution.functions
let function_symbol function_ = function_.function_symbol_
let function_scope function_ = function_.function_scope_
let function_item_index function_ = function_.function_item_index_
let function_locals function_ = function_.function_locals_
let local_symbol local = local.symbol
let local_declaration_index local = local.declaration_index
let local_declarator_index local = local.declarator_index
let local_declaration_origin local = local.declaration_origin
let local_declarator_origin local = local.declarator_origin
let local_storage local = local.storage
let local_storage_origins local = local.storage_origins
let local_type_reference local = local.type_reference
let local_register_requests local = local.register_requests
let local_declarator_kind local = local.declarator_kind
let local_array_dimensions local = local.array_dimensions
let local_initializer local = local.initial_value
let local_delimiter local = local.delimiter
let register_request_kind request = request.request_kind
let register_request_position request = request.request_position
let register_request_spelling request = request.request_spelling
let register_request_origin request = request.request_origin
let register_request_explicit_register request = request.explicit_register

let register_request_explicit_register_origin request =
  request.explicit_register_origin

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

let storage_name = function
  | Automatic -> "automatic"
  | Static -> "static"

let register_request_kind_name = function
  | Allocate -> "reg"
  | Disable -> "noreg"

let register_position_name = function
  | Before_type -> "before-type"
  | After_type -> "after-type"

let delimiter_kind_name = function
  | Comma -> "comma"
  | Semicolon -> "semicolon"

let initializer_kind_name = function
  | Scalar_initializer -> "scalar"
  | Braced_initializer -> "braced"

let make_register_request ~kind ~position ~spelling ~origin ?explicit_register
    ?explicit_register_origin () =
  let expected_spelling = register_request_kind_name kind in
  if not (String.equal spelling expected_spelling) then
    Error
      (Printf.sprintf "semantic local register spelling %S does not match %S"
         spelling expected_spelling)
  else
    match (explicit_register, explicit_register_origin) with
    | None, Some _ ->
        Error "semantic local register origin requires an explicit register"
    | Some _, None ->
        Error "semantic local explicit register requires source provenance"
    | Some _, Some _ when kind = Disable ->
        Error "semantic local noreg request cannot name a register"
    | Some register, Some _ when String.equal register "" ->
        Error "semantic local explicit register cannot be empty"
    | None, None | Some _, Some _ ->
        Ok
          {
            request_kind = kind;
            request_position = position;
            request_spelling = spelling;
            request_origin = origin;
            explicit_register;
            explicit_register_origin;
          }

let make_array_dimension ~index ~origin ~opening_origin ?expression_origin
    ~closing_origin () =
  if index < 0 then Error "semantic local array index cannot be negative"
  else if index > 0 && Option.is_none expression_origin then
    Error "only the first semantic local array dimension can be empty"
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

let make_local ~symbol ~declaration_index ~declarator_index ~declaration_origin
    ~declarator_origin ~storage ~storage_origins ~type_reference
    ~register_requests ~declarator_kind ~array_dimensions ~initial_value
    ~delimiter () =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Local_variable) then
    Error "semantic local type requires a local-variable symbol"
  else if declaration_index < 0 then
    Error "semantic local declaration index cannot be negative"
  else if declarator_index < 0 then
    Error "semantic local declarator index cannot be negative"
  else if storage = Automatic && storage_origins <> [] then
    Error "semantic automatic local cannot carry static provenance"
  else if storage = Static && storage_origins = [] then
    Error "semantic static local requires static provenance"
  else if storage = Static && register_requests <> [] then
    Error "semantic static local cannot carry register requests"
  else if not (dimensions_are_ordered array_dimensions) then
    Error "semantic local array dimensions must use consecutive indexes"
  else if
    match (storage, initial_value) with
    | Automatic, Some { kind = Braced_initializer; _ } -> true
    | Automatic, None | Automatic, Some _ | Static, _ -> false
  then Error "semantic automatic local cannot use a braced initializer"
  else if
    match (declarator_kind, initial_value) with
    | Function_pointer _, Some _ -> true
    | Object, _ | Function_pointer _, None -> false
  then Error "semantic function-pointer local cannot have an initializer"
  else
    Ok
      {
        symbol;
        declaration_index;
        declarator_index;
        declaration_origin;
        declarator_origin;
        storage;
        storage_origins;
        type_reference;
        register_requests;
        declarator_kind;
        array_dimensions;
        initial_value;
        delimiter;
      }

let make_function ~symbol ~scope ~item_index locals =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic local type owner must be a function symbol"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "semantic local types require a function scope"
  else if item_index < 0 then
    Error "semantic local type function item index cannot be negative"
  else
    Ok
      {
        function_symbol_ = symbol;
        function_scope_ = scope;
        function_item_index_ = item_index;
        function_locals_ = locals;
      }

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let validate_type_reference ~table ~parent reference =
  match Type.base (Type_reference.resolved_type reference) with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol ->
      if not (Symbol_table.owns_symbol table symbol) then
        Error "semantic local type target belongs to a different symbol table"
      else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
      then Error "semantic local type target is not an aggregate type"
      else if
        not
          (Symbol.Scope_id.equal (Symbol.scope_id symbol)
             (Symbol_table.scope_id parent))
      then Error "semantic local type target does not belong to the module"
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

let validate_declarator_type ~table ~parent local =
  match validate_type_reference ~table ~parent local.type_reference with
  | Error _ as error -> error
  | Ok () -> (
      match local.declarator_kind with
      | Object -> Ok ()
      | Function_pointer pointer ->
          validate_signature_types ~table ~parent
            (Function_type_resolution.function_pointer_signature pointer))

module Int_set = Set.Make (Int)

let symbol_number symbol = Symbol.Id.to_int (Symbol.id symbol)
let scope_number scope = Symbol.Scope_id.to_int (Symbol_table.scope_id scope)

let validate_local_position previous local =
  match previous with
  | None ->
      if local.declaration_index <> 0 || local.declarator_index <> 0 then
        Error "semantic local declarations must start at position zero"
      else Ok ()
  | Some previous ->
      if local.declaration_index = previous.declaration_index then
        if local.declarator_index <> previous.declarator_index + 1 then
          Error "semantic local declarator indexes must be consecutive"
        else if previous.delimiter.kind <> Comma then
          Error "semantic local declaration ends before its next declarator"
        else if local.declaration_origin <> previous.declaration_origin then
          Error "semantic local declarators disagree on declaration provenance"
        else if local.storage <> previous.storage then
          Error "semantic local declarators disagree on storage"
        else if local.storage_origins <> previous.storage_origins then
          Error "semantic local declarators disagree on storage provenance"
        else Ok ()
      else if local.declaration_index = previous.declaration_index + 1 then
        if local.declarator_index <> 0 then
          Error "semantic local declaration group must start at zero"
        else if previous.delimiter.kind <> Semicolon then
          Error "semantic local declaration group must end with a semicolon"
        else Ok ()
      else Error "semantic local declarations must follow source order"

let validate_local ~table ~parent ~scope ~previous ~seen local =
  let number = symbol_number local.symbol in
  if Int_set.mem number seen then Error "semantic local type symbol is repeated"
  else if not (Symbol_table.owns_symbol table local.symbol) then
    Error "semantic local type symbol belongs to a different symbol table"
  else if
    not (Symbol.equal_kind (Symbol.kind local.symbol) Symbol.Local_variable)
  then Error "semantic local type symbol is not a local variable"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id local.symbol)
         (Symbol_table.scope_id scope))
  then Error "semantic local type symbol belongs to the wrong function scope"
  else
    match validate_local_position previous local with
    | Error _ as error -> error
    | Ok () ->
        Result.map
          (fun () -> Int_set.add number seen)
          (validate_declarator_type ~table ~parent local)

let validate_function ~table ~parent ~previous_item ~seen_symbols ~seen_scopes
    function_ =
  let symbol_number = symbol_number function_.function_symbol_ in
  let scope_number = scope_number function_.function_scope_ in
  if function_.function_item_index_ <= previous_item then
    Error "semantic local type functions must follow module source order"
  else if Int_set.mem symbol_number seen_symbols then
    Error "semantic local type function symbol is repeated"
  else if Int_set.mem scope_number seen_scopes then
    Error "semantic local type function scope is repeated"
  else if not (Symbol_table.owns_symbol table function_.function_symbol_) then
    Error "semantic local type function belongs to a different symbol table"
  else if not (Symbol_table.owns_scope table function_.function_scope_) then
    Error "semantic local type scope belongs to a different symbol table"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id function_.function_symbol_)
         (Symbol_table.scope_id parent))
  then Error "semantic local type function does not belong to the module"
  else if
    match Symbol_table.parent function_.function_scope_ with
    | Some scope -> not (same_scope scope parent)
    | None -> true
  then Error "semantic local type function scope does not belong to the module"
  else
    let rec validate_locals previous seen = function
      | [] -> (
          match previous with
          | Some local when local.delimiter.kind <> Semicolon ->
              Error "semantic local declaration group is unterminated"
          | None | Some _ -> Ok ())
      | local :: rest -> (
          match
            validate_local ~table ~parent ~scope:function_.function_scope_
              ~previous ~seen local
          with
          | Error _ as error -> error
          | Ok seen -> validate_locals (Some local) seen rest)
    in
    Result.map
      (fun () ->
        ( function_.function_item_index_,
          Int_set.add symbol_number seen_symbols,
          Int_set.add scope_number seen_scopes ))
      (validate_locals None Int_set.empty function_.function_locals_)

let resolve ~table ~parent function_declarations =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic local type parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic local types require a module scope"
  else
    let rec validate previous_item seen_symbols seen_scopes = function
      | [] -> Ok { functions = function_declarations }
      | function_ :: rest -> (
          match
            validate_function ~table ~parent ~previous_item ~seen_symbols
              ~seen_scopes function_
          with
          | Error _ as error -> error
          | Ok (item_index, seen_symbols, seen_scopes) ->
              validate item_index seen_symbols seen_scopes rest)
    in
    validate (-1) Int_set.empty Int_set.empty function_declarations
