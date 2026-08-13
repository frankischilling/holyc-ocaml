type parameter_default =
  | Expression_default of {
      origin : Symbol.origin;
      equals_origin : Symbol.origin;
      expression_origin : Symbol.origin;
    }
  | Lastclass_default of {
      origin : Symbol.origin;
      equals_origin : Symbol.origin;
      keyword_origin : Symbol.origin;
    }

type declarator_kind = Object | Function_pointer of function_pointer

and function_pointer = {
  pointer_origin : Symbol.origin;
  pointer_opening_origin : Symbol.origin;
  pointer_indirection_origins : Symbol.origin list;
  pointer_closing_origin : Symbol.origin;
  pointer_signature : signature;
}

and parameter = {
  parameter_index_ : int;
  parameter_origin_ : Symbol.origin;
  parameter_name_ : string option;
  parameter_name_origin_ : Symbol.origin option;
  parameter_type_reference_ : Type_reference.t;
  parameter_declarator_kind_ : declarator_kind;
  parameter_default_ : parameter_default option;
  parameter_delimiter_origin_ : Symbol.origin option;
}

and signature = {
  signature_opening_origin_ : Symbol.origin;
  signature_parameters_ : parameter list;
  signature_variadic_origin_ : Symbol.origin option;
  signature_closing_origin_ : Symbol.origin;
}

type parameter_binding = {
  binding_parameter_index : int;
  binding_symbol : Symbol.t;
}

type synthetic_parameter = Argc | Argv

type synthetic_shape =
  | Scalar
  | Array of { source_extent : int option; compiler_placeholder_extent : int }

type synthetic_binding = {
  synthetic_kind : synthetic_parameter;
  synthetic_symbol : Symbol.t;
  synthetic_parameter_index : int;
  synthetic_type : Type.t;
  synthetic_shape : synthetic_shape;
}

type variadic_bindings = {
  variadic_marker_origin_ : Symbol.origin;
  variadic_argc_ : synthetic_binding;
  variadic_argv_ : synthetic_binding;
}

type function_declaration = {
  function_symbol_ : Symbol.t;
  function_scope_ : Symbol_table.scope;
  function_item_index_ : int;
  function_return_type_ : Type_reference.t;
  function_signature_ : signature;
  function_parameter_bindings_ : parameter_binding list;
  function_variadic_bindings_ : variadic_bindings option;
}

type resolved_function = function_declaration
type t = { functions : resolved_function list }

let functions resolution = resolution.functions
let function_symbol function_ = function_.function_symbol_
let function_scope function_ = function_.function_scope_
let function_item_index function_ = function_.function_item_index_
let function_return_type function_ = function_.function_return_type_
let function_signature function_ = function_.function_signature_

let function_parameter_bindings function_ =
  function_.function_parameter_bindings_

let function_variadic_bindings function_ = function_.function_variadic_bindings_
let signature_opening_origin signature = signature.signature_opening_origin_
let signature_parameters signature = signature.signature_parameters_
let signature_variadic_origin signature = signature.signature_variadic_origin_
let signature_closing_origin signature = signature.signature_closing_origin_
let parameter_index parameter = parameter.parameter_index_
let parameter_origin parameter = parameter.parameter_origin_
let parameter_name parameter = parameter.parameter_name_
let parameter_name_origin parameter = parameter.parameter_name_origin_
let parameter_type_reference parameter = parameter.parameter_type_reference_
let parameter_declarator_kind parameter = parameter.parameter_declarator_kind_
let parameter_default parameter = parameter.parameter_default_
let parameter_delimiter_origin parameter = parameter.parameter_delimiter_origin_
let function_pointer_origin pointer = pointer.pointer_origin
let function_pointer_opening_origin pointer = pointer.pointer_opening_origin

let function_pointer_indirection_origins pointer =
  pointer.pointer_indirection_origins

let function_pointer_closing_origin pointer = pointer.pointer_closing_origin
let function_pointer_signature pointer = pointer.pointer_signature
let parameter_binding_index binding = binding.binding_parameter_index
let parameter_binding_symbol binding = binding.binding_symbol
let variadic_marker_origin variadic = variadic.variadic_marker_origin_
let variadic_argc variadic = variadic.variadic_argc_
let variadic_argv variadic = variadic.variadic_argv_
let synthetic_binding_kind binding = binding.synthetic_kind
let synthetic_binding_symbol binding = binding.synthetic_symbol
let synthetic_binding_index binding = binding.synthetic_parameter_index
let synthetic_binding_type binding = binding.synthetic_type
let synthetic_binding_shape binding = binding.synthetic_shape

let synthetic_parameter_name = function
  | Argc -> "argc"
  | Argv -> "argv"

let make_parameter ~index ~origin ?name ?name_origin ~type_reference
    ~declarator_kind ~default ?delimiter_origin () =
  if index < 0 then Error "semantic function parameter index cannot be negative"
  else
    match (name, name_origin) with
    | None, Some _ ->
        Error "unnamed semantic function parameter cannot have a name origin"
    | Some _, None ->
        Error "named semantic function parameter requires a name origin"
    | Some name, Some _ when String.equal name "" ->
        Error "semantic function parameter name cannot be empty"
    | None, None | Some _, Some _ ->
        Ok
          {
            parameter_index_ = index;
            parameter_origin_ = origin;
            parameter_name_ = name;
            parameter_name_origin_ = name_origin;
            parameter_type_reference_ = type_reference;
            parameter_declarator_kind_ = declarator_kind;
            parameter_default_ = default;
            parameter_delimiter_origin_ = delimiter_origin;
          }

let make_function_pointer ~origin ~opening_origin ~indirection_origins
    ~closing_origin ~signature =
  let depth = List.length indirection_origins in
  if depth = 0 then
    Error "semantic callback signature requires at least one indirection layer"
  else if depth > Type.max_pointer_depth then
    Error
      (Printf.sprintf
         "semantic callback signature indirection depth %d exceeds HolyC's \
          limit of %d"
         depth Type.max_pointer_depth)
  else
    Ok
      {
        pointer_origin = origin;
        pointer_opening_origin = opening_origin;
        pointer_indirection_origins = indirection_origins;
        pointer_closing_origin = closing_origin;
        pointer_signature = signature;
      }

let make_signature ~opening_origin ~parameters ?variadic_origin ~closing_origin
    () =
  let rec validate expected = function
    | [] -> Ok ()
    | parameter :: rest ->
        if parameter.parameter_index_ <> expected then
          Error "semantic function parameters must occupy consecutive slots"
        else
          let needs_delimiter = rest <> [] || Option.is_some variadic_origin in
          if
            needs_delimiter
            <> Option.is_some parameter.parameter_delimiter_origin_
          then
            Error
              "semantic function parameter delimiter does not match the \
               signature"
          else validate (expected + 1) rest
  in
  match validate 0 parameters with
  | Error _ as error -> error
  | Ok () ->
      Ok
        {
          signature_opening_origin_ = opening_origin;
          signature_parameters_ = parameters;
          signature_variadic_origin_ = variadic_origin;
          signature_closing_origin_ = closing_origin;
        }

let make_parameter_binding ~parameter_index ~symbol =
  if parameter_index < 0 then
    Error "semantic parameter binding index cannot be negative"
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Parameter) then
    Error "semantic function parameter binding requires a parameter symbol"
  else Ok { binding_parameter_index = parameter_index; binding_symbol = symbol }

let internal_i64 type_ =
  Type.pointer_depth type_ = 0
  &&
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, primitive) ->
      Primitive_type.equal primitive Primitive_type.I64
  | Type.Primitive (Type.Public_spelling, _) | Type.Aggregate _ -> false

let valid_synthetic_shape kind shape =
  match (kind, shape) with
  | Argc, Scalar -> true
  | Argv, Array { source_extent = None; compiler_placeholder_extent = 127 } ->
      true
  | Argc, Array _ | Argv, Scalar | Argv, Array _ -> false

let make_synthetic_binding kind ~symbol ~parameter_index ~resolved_type ~shape =
  let expected_name = synthetic_parameter_name kind in
  if parameter_index < 0 then
    Error "semantic variadic binding index cannot be negative"
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Parameter) then
    Error "semantic variadic binding requires a parameter symbol"
  else if not (String.equal (Symbol.name symbol) expected_name) then
    Error
      (Printf.sprintf "semantic variadic binding must be named %S" expected_name)
  else if not (internal_i64 resolved_type) then
    Error "semantic variadic binding must use the internal I64 storage type"
  else if not (valid_synthetic_shape kind shape) then
    Error
      (Printf.sprintf "semantic %s binding has the wrong source shape"
         expected_name)
  else
    Ok
      {
        synthetic_kind = kind;
        synthetic_symbol = symbol;
        synthetic_parameter_index = parameter_index;
        synthetic_type = resolved_type;
        synthetic_shape = shape;
      }

let make_variadic_bindings ~marker_origin ~argc ~argv =
  if argc.synthetic_kind <> Argc || argv.synthetic_kind <> Argv then
    Error "semantic variadic bindings must contain argc followed by argv"
  else if argv.synthetic_parameter_index <> argc.synthetic_parameter_index + 1
  then Error "semantic variadic argv must immediately follow argc"
  else if Symbol.origin argc.synthetic_symbol <> marker_origin then
    Error "semantic variadic argc origin does not match the ellipsis"
  else if Symbol.origin argv.synthetic_symbol <> marker_origin then
    Error "semantic variadic argv origin does not match the ellipsis"
  else
    Ok
      {
        variadic_marker_origin_ = marker_origin;
        variadic_argc_ = argc;
        variadic_argv_ = argv;
      }

let named_parameters signature =
  List.filter_map
    (fun parameter ->
      match (parameter.parameter_name_, parameter.parameter_name_origin_) with
      | Some name, Some origin -> Some (parameter.parameter_index_, name, origin)
      | None, None -> None
      | None, Some _ | Some _, None -> assert false)
    signature.signature_parameters_

let validate_parameter_bindings signature bindings =
  let expected = named_parameters signature in
  let rec validate expected bindings =
    match (expected, bindings) with
    | [], [] -> Ok ()
    | (index, name, origin) :: expected_rest, binding :: binding_rest ->
        if binding.binding_parameter_index <> index then
          Error "semantic parameter binding has the wrong signature slot"
        else if not (String.equal (Symbol.name binding.binding_symbol) name)
        then Error "semantic parameter binding has the wrong name"
        else if Symbol.origin binding.binding_symbol <> origin then
          Error "semantic parameter binding has the wrong source origin"
        else validate expected_rest binding_rest
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic parameter bindings do not match the named parameters"
  in
  validate expected bindings

let validate_variadic_bindings signature bindings =
  match (signature.signature_variadic_origin_, bindings) with
  | None, None -> Ok ()
  | Some expected, Some actual when actual.variadic_marker_origin_ = expected ->
      let first_variadic_index = List.length signature.signature_parameters_ in
      if actual.variadic_argc_.synthetic_parameter_index <> first_variadic_index
      then Error "semantic variadic argc has the wrong signature slot"
      else Ok ()
  | Some _, Some _ ->
      Error "semantic variadic bindings do not match the ellipsis origin"
  | None, Some _ | Some _, None ->
      Error "semantic variadic bindings do not match the signature"

let make_function ~symbol ~scope ~item_index ~return_type ~signature
    ~parameter_bindings ~variadic_bindings =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic function type owner must be a function symbol"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "semantic function type requires a function scope"
  else if item_index < 0 then
    Error "semantic function type item index cannot be negative"
  else
    match validate_parameter_bindings signature parameter_bindings with
    | Error _ as error -> error
    | Ok () -> (
        match validate_variadic_bindings signature variadic_bindings with
        | Error _ as error -> error
        | Ok () ->
            Ok
              {
                function_symbol_ = symbol;
                function_scope_ = scope;
                function_item_index_ = item_index;
                function_return_type_ = return_type;
                function_signature_ = signature;
                function_parameter_bindings_ = parameter_bindings;
                function_variadic_bindings_ = variadic_bindings;
              })

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let validate_type_reference ~table ~parent reference =
  match Type.base (Type_reference.resolved_type reference) with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol ->
      if not (Symbol_table.owns_symbol table symbol) then
        Error
          "semantic function type target belongs to a different symbol table"
      else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
      then Error "semantic function type target is not an aggregate type"
      else if
        not
          (Symbol.Scope_id.equal (Symbol.scope_id symbol)
             (Symbol_table.scope_id parent))
      then Error "semantic function type target does not belong to the module"
      else Ok ()

let rec validate_signature_types ~table ~parent signature =
  let rec validate_parameters = function
    | [] -> Ok ()
    | parameter :: rest -> (
        match
          validate_type_reference ~table ~parent
            parameter.parameter_type_reference_
        with
        | Error _ as error -> error
        | Ok () -> (
            match parameter.parameter_declarator_kind_ with
            | Object -> validate_parameters rest
            | Function_pointer pointer -> (
                match
                  validate_signature_types ~table ~parent
                    pointer.pointer_signature
                with
                | Error _ as error -> error
                | Ok () -> validate_parameters rest)))
  in
  validate_parameters signature.signature_parameters_

module Int_set = Set.Make (Int)

let symbol_number symbol = Symbol.Id.to_int (Symbol.id symbol)
let scope_number scope = Symbol.Scope_id.to_int (Symbol_table.scope_id scope)

let validate_binding_ownership ~table ~scope seen (binding : parameter_binding)
    =
  let symbol = binding.binding_symbol in
  let number = symbol_number symbol in
  if Int_set.mem number seen then
    Error "semantic function parameter binding symbol is repeated"
  else if not (Symbol_table.owns_symbol table symbol) then
    Error
      "semantic function parameter binding belongs to a different symbol table"
  else if
    not
      (Symbol.Scope_id.equal (Symbol.scope_id symbol)
         (Symbol_table.scope_id scope))
  then Error "semantic function parameter binding belongs to the wrong scope"
  else Ok (Int_set.add number seen)

let validate_variadic_ownership ~table ~scope seen = function
  | None -> Ok seen
  | Some variadic -> (
      match
        validate_binding_ownership ~table ~scope seen
          {
            binding_parameter_index =
              variadic.variadic_argc_.synthetic_parameter_index;
            binding_symbol = variadic.variadic_argc_.synthetic_symbol;
          }
      with
      | Error _ as error -> error
      | Ok seen ->
          validate_binding_ownership ~table ~scope seen
            {
              binding_parameter_index =
                variadic.variadic_argv_.synthetic_parameter_index;
              binding_symbol = variadic.variadic_argv_.synthetic_symbol;
            })

let validate_function ~table ~parent previous_item seen_symbols seen_scopes
    function_ =
  let symbol_number = symbol_number function_.function_symbol_ in
  let scope_number = scope_number function_.function_scope_ in
  if function_.function_item_index_ <= previous_item then
    Error "semantic function types must follow module source order"
  else if Int_set.mem symbol_number seen_symbols then
    Error "semantic function type owner is repeated"
  else if Int_set.mem scope_number seen_scopes then
    Error "semantic function type scope is repeated"
  else if not (Symbol_table.owns_symbol table function_.function_symbol_) then
    Error "semantic function type owner belongs to a different symbol table"
  else if not (Symbol_table.owns_scope table function_.function_scope_) then
    Error "semantic function type scope belongs to a different symbol table"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id function_.function_symbol_)
         (Symbol_table.scope_id parent))
  then Error "semantic function type owner does not belong to the module"
  else if
    match Symbol_table.parent function_.function_scope_ with
    | Some scope -> not (same_scope scope parent)
    | None -> true
  then Error "semantic function type scope does not belong to the module"
  else
    match
      validate_type_reference ~table ~parent function_.function_return_type_
    with
    | Error _ as error -> error
    | Ok () -> (
        match
          validate_signature_types ~table ~parent function_.function_signature_
        with
        | Error _ as error -> error
        | Ok () -> (
            let rec validate_bindings seen = function
              | [] -> Ok seen
              | binding :: rest -> (
                  match
                    validate_binding_ownership ~table
                      ~scope:function_.function_scope_ seen binding
                  with
                  | Error _ as error -> error
                  | Ok seen -> validate_bindings seen rest)
            in
            match
              validate_bindings Int_set.empty
                function_.function_parameter_bindings_
            with
            | Error _ as error -> error
            | Ok seen -> (
                match
                  validate_variadic_ownership ~table
                    ~scope:function_.function_scope_ seen
                    function_.function_variadic_bindings_
                with
                | Error _ as error -> error
                | Ok _ ->
                    Ok
                      ( function_.function_item_index_,
                        Int_set.add symbol_number seen_symbols,
                        Int_set.add scope_number seen_scopes ))))

let resolve ~table ~parent function_declarations =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic function type parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic function types require a module scope"
  else
    let rec validate previous_item seen_symbols seen_scopes = function
      | [] -> Ok { functions = function_declarations }
      | function_ :: rest -> (
          match
            validate_function ~table ~parent previous_item seen_symbols
              seen_scopes function_
          with
          | Error _ as error -> error
          | Ok (item_index, seen_symbols, seen_scopes) ->
              validate item_index seen_symbols seen_scopes rest)
    in
    validate (-1) Int_set.empty Int_set.empty function_declarations
