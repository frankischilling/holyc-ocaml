type parameter_default =
  | Expression_default of {
      origin : Symbol.origin;
      equals_origin : Symbol.origin;
      expression_origin : Symbol.origin;
      contains_string_literal : bool;
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
  parameter_source_ : Frontend.Ast.function_parameter option;
  parameter_index_ : int;
  parameter_origin_ : Symbol.origin;
  parameter_register_requests_ : Register_request.t list;
  parameter_name_ : string option;
  parameter_name_origin_ : Symbol.origin option;
  parameter_type_reference_ : Type_reference.t;
  parameter_declarator_kind_ : declarator_kind;
  parameter_default_ : parameter_default option;
  parameter_flag_mask_ : int64;
  parameter_delimiter_origin_ : Symbol.origin option;
}

and signature = {
  signature_opening_origin_ : Symbol.origin;
  signature_parameters_ : parameter list;
  signature_variadic_origin_ : Symbol.origin option;
  signature_variadic_register_requests_ : Register_request.t list;
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
  synthetic_register_requests : Register_request.t list;
  synthetic_flag_mask : int64;
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
  function_completed_header_ : Frontend.Parser.completed_function_header option;
  mutable function_header_reused_ : bool;
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
let function_completed_header function_ = function_.function_completed_header_
let signature_opening_origin signature = signature.signature_opening_origin_
let signature_parameters signature = signature.signature_parameters_
let signature_variadic_origin signature = signature.signature_variadic_origin_

let signature_variadic_register_requests signature =
  signature.signature_variadic_register_requests_

let signature_variadic_register_selection signature =
  Register_request.effective signature.signature_variadic_register_requests_

let signature_closing_origin signature = signature.signature_closing_origin_
let parameter_index parameter = parameter.parameter_index_
let parameter_source parameter = parameter.parameter_source_
let parameter_origin parameter = parameter.parameter_origin_

let parameter_register_requests parameter =
  parameter.parameter_register_requests_

let parameter_register_selection parameter =
  Register_request.effective parameter.parameter_register_requests_

let parameter_name parameter = parameter.parameter_name_
let parameter_name_origin parameter = parameter.parameter_name_origin_
let parameter_type_reference parameter = parameter.parameter_type_reference_
let parameter_declarator_kind parameter = parameter.parameter_declarator_kind_
let parameter_default parameter = parameter.parameter_default_
let parameter_flag_mask parameter = parameter.parameter_flag_mask_

let parameter_has_flag parameter flag =
  Member_flag.is_set ~mask:parameter.parameter_flag_mask_ flag

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

let synthetic_binding_register_requests binding =
  binding.synthetic_register_requests

let synthetic_binding_register_selection binding =
  Register_request.effective binding.synthetic_register_requests

let synthetic_binding_flag_mask binding = binding.synthetic_flag_mask

let synthetic_binding_has_flag binding flag =
  Member_flag.is_set ~mask:binding.synthetic_flag_mask flag

let synthetic_parameter_name = function
  | Argc -> "argc"
  | Argv -> "argv"

let set_flag_if condition flag mask =
  if condition then Member_flag.set ~mask flag else mask

let set_flag flag mask = Member_flag.set ~mask flag

let parameter_flags ~name ~declarator_kind ~default =
  let mask =
    0L
    |> set_flag_if (Option.is_none name) Member_flag.No_unused_warning
    |> set_flag_if
         (match declarator_kind with
         | Function_pointer _ -> true
         | Object -> false)
         Member_flag.Function_pointer
  in
  match default with
  | None -> mask
  | Some (Lastclass_default _) ->
      mask
      |> set_flag Member_flag.Default_available
      |> set_flag Member_flag.Lastclass
  | Some (Expression_default { contains_string_literal; _ }) ->
      mask
      |> set_flag Member_flag.Default_available
      |> set_flag_if contains_string_literal
           Member_flag.String_default_available

let source_location (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let source_parameter_matches (source : Frontend.Ast.function_parameter) ~origin
    ~name ~name_origin ~type_reference ~default =
  origin = source_location source.location
  && name
     = Option.map
         (fun (name : Frontend.Ast.identifier) -> name.spelling)
         source.name
  && name_origin
     = Option.map
         (fun (name : Frontend.Ast.identifier) -> source_location name.location)
         source.name
  && Type_reference.spelling type_reference
     = Frontend.Ast.type_specifier_spelling source.type_specifier
  && Type_reference.spelling_origin type_reference
     = source_location
         (Frontend.Ast.type_specifier_location source.type_specifier)
  && Type_reference.pointer_origins type_reference
     = List.map
         (fun (layer : Frontend.Ast.pointer_layer) ->
           source_location layer.location)
         source.pointer_layers
  &&
  match (source.default, default) with
  | None, None -> true
  | Some source, Some (Expression_default value) -> (
      value.origin = source_location source.location
      && value.equals_origin = source_location source.equals
      &&
      match source.value with
      | Frontend.Ast.Expression_default expression ->
          value.expression_origin
          = source_location (Frontend.Ast.expression_location expression)
      | _ -> false)
  | Some source, Some (Lastclass_default value) -> (
      value.origin = source_location source.location
      && value.equals_origin = source_location source.equals
      &&
      match source.value with
      | Frontend.Ast.Lastclass_default keyword ->
          value.keyword_origin = source_location keyword.lastclass_location
      | _ -> false)
  | _ -> false

let make_parameter ?source ~index ~origin ?(register_requests = []) ?name
    ?name_origin ~type_reference ~declarator_kind ~default ?delimiter_origin ()
    =
  if index < 0 then Error "semantic function parameter index cannot be negative"
  else if
    not
      (Option.fold ~none:true
         ~some:(fun source ->
           source_parameter_matches source ~origin ~name ~name_origin
             ~type_reference ~default)
         source)
  then
    Error
      "semantic parameter metadata differs from its original source parameter"
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
            parameter_source_ = source;
            parameter_index_ = index;
            parameter_origin_ = origin;
            parameter_register_requests_ = register_requests;
            parameter_name_ = name;
            parameter_name_origin_ = name_origin;
            parameter_type_reference_ = type_reference;
            parameter_declarator_kind_ = declarator_kind;
            parameter_default_ = default;
            parameter_flag_mask_ =
              parameter_flags ~name ~declarator_kind ~default;
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

let make_signature ~opening_origin ~parameters ?variadic_origin
    ?(variadic_register_requests = []) ~closing_origin () =
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
  match (variadic_origin, variadic_register_requests) with
  | None, _ :: _ ->
      Error "semantic variadic register requests require an ellipsis"
  | None, [] | Some _, _ -> (
      match validate 0 parameters with
      | Error _ as error -> error
      | Ok () ->
          Ok
            {
              signature_opening_origin_ = opening_origin;
              signature_parameters_ = parameters;
              signature_variadic_origin_ = variadic_origin;
              signature_variadic_register_requests_ = variadic_register_requests;
              signature_closing_origin_ = closing_origin;
            })

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

let make_synthetic_binding kind ~symbol ~parameter_index ~resolved_type
    ?(register_requests = []) ~shape () =
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
        synthetic_register_requests = register_requests;
        synthetic_flag_mask = Member_flag.to_mask Member_flag.Variadic;
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
  else if
    not
      (List.equal Register_request.equal argc.synthetic_register_requests
         argv.synthetic_register_requests)
  then Error "semantic variadic argc and argv register requests do not match"
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
      else if
        not
          (List.equal Register_request.equal
             signature.signature_variadic_register_requests_
             actual.variadic_argc_.synthetic_register_requests)
      then
        Error "semantic variadic register requests do not match the signature"
      else Ok ()
  | Some _, Some _ ->
      Error "semantic variadic bindings do not match the ellipsis origin"
  | None, Some _ | Some _, None ->
      Error "semantic variadic bindings do not match the signature"

let make_function_record completed_header ~symbol ~scope ~item_index
    ~return_type ~signature ~parameter_bindings ~variadic_bindings =
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
                function_completed_header_ = completed_header;
                function_header_reused_ = false;
              })

let make_function ~symbol ~scope ~item_index ~return_type ~signature
    ~parameter_bindings ~variadic_bindings =
  make_function_record None ~symbol ~scope ~item_index ~return_type ~signature
    ~parameter_bindings ~variadic_bindings

let make_function_with_completed_header completed_header ~symbol ~scope
    ~item_index ~return_type ~signature ~parameter_bindings ~variadic_bindings =
  make_function_record (Some completed_header) ~symbol ~scope ~item_index
    ~return_type ~signature ~parameter_bindings ~variadic_bindings

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

let same_type_reference left right =
  String.equal (Type_reference.spelling left) (Type_reference.spelling right)
  && Type_reference.spelling_origin left = Type_reference.spelling_origin right
  && Type_reference.pointer_origins left = Type_reference.pointer_origins right
  && Type.equal
       (Type_reference.resolved_type left)
       (Type_reference.resolved_type right)

let same_source left right =
  match (left, right) with
  | Some left, Some right -> left == right
  | None, None -> true
  | Some _, None | None, Some _ -> false

let rec same_signature left right =
  left.signature_opening_origin_ = right.signature_opening_origin_
  && left.signature_variadic_origin_ = right.signature_variadic_origin_
  && List.equal Register_request.equal
       left.signature_variadic_register_requests_
       right.signature_variadic_register_requests_
  && left.signature_closing_origin_ = right.signature_closing_origin_
  && List.equal same_parameter left.signature_parameters_
       right.signature_parameters_

and same_parameter left right =
  same_source left.parameter_source_ right.parameter_source_
  && left.parameter_index_ = right.parameter_index_
  && left.parameter_origin_ = right.parameter_origin_
  && List.equal Register_request.equal left.parameter_register_requests_
       right.parameter_register_requests_
  && left.parameter_name_ = right.parameter_name_
  && left.parameter_name_origin_ = right.parameter_name_origin_
  && same_type_reference left.parameter_type_reference_
       right.parameter_type_reference_
  && same_declarator_kind left.parameter_declarator_kind_
       right.parameter_declarator_kind_
  && left.parameter_default_ = right.parameter_default_
  && left.parameter_flag_mask_ = right.parameter_flag_mask_
  && left.parameter_delimiter_origin_ = right.parameter_delimiter_origin_

and same_declarator_kind left right =
  match (left, right) with
  | Object, Object -> true
  | Function_pointer left, Function_pointer right ->
      left.pointer_origin = right.pointer_origin
      && left.pointer_opening_origin = right.pointer_opening_origin
      && left.pointer_indirection_origins = right.pointer_indirection_origins
      && left.pointer_closing_origin = right.pointer_closing_origin
      && same_signature left.pointer_signature right.pointer_signature
  | Object, Function_pointer _ | Function_pointer _, Object -> false

let same_parameter_binding left right =
  left.binding_parameter_index = right.binding_parameter_index
  && left.binding_symbol == right.binding_symbol

let same_synthetic_shape left right =
  match (left, right) with
  | Scalar, Scalar -> true
  | ( Array
        {
          source_extent = left_source;
          compiler_placeholder_extent = left_placeholder;
        },
      Array
        {
          source_extent = right_source;
          compiler_placeholder_extent = right_placeholder;
        } ) ->
      left_source = right_source && left_placeholder = right_placeholder
  | Scalar, Array _ | Array _, Scalar -> false

let same_synthetic_binding left right =
  left.synthetic_kind = right.synthetic_kind
  && left.synthetic_symbol == right.synthetic_symbol
  && left.synthetic_parameter_index = right.synthetic_parameter_index
  && Type.equal left.synthetic_type right.synthetic_type
  && same_synthetic_shape left.synthetic_shape right.synthetic_shape
  && List.equal Register_request.equal left.synthetic_register_requests
       right.synthetic_register_requests
  && left.synthetic_flag_mask = right.synthetic_flag_mask

let same_variadic_bindings left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right ->
      left.variadic_marker_origin_ = right.variadic_marker_origin_
      && same_synthetic_binding left.variadic_argc_ right.variadic_argc_
      && same_synthetic_binding left.variadic_argv_ right.variadic_argv_
  | None, Some _ | Some _, None -> false

let same_completed_header left right =
  match (left, right) with
  | Some left, Some right -> left == right
  | None, None -> true
  | Some _, None | None, Some _ -> false

let validate_retained_header ~table ~parent retained function_ =
  if retained.function_header_reused_ then
    Error "semantic retained function type was already completed"
  else if Option.is_none retained.function_completed_header_ then
    Error "semantic retained function type is not a completed header"
  else if
    not
      (same_completed_header retained.function_completed_header_
         function_.function_completed_header_)
  then Error "semantic retained function type has different source evidence"
  else if retained.function_symbol_ != function_.function_symbol_ then
    Error "semantic retained function type has the wrong function symbol"
  else if retained.function_scope_ != function_.function_scope_ then
    Error "semantic retained function type has the wrong function scope"
  else if retained.function_item_index_ <> function_.function_item_index_ then
    Error "semantic retained function type has the wrong item order"
  else if
    not
      (same_type_reference retained.function_return_type_
         function_.function_return_type_)
  then Error "semantic retained function type has a different return type"
  else if
    not
      (same_signature retained.function_signature_ function_.function_signature_)
  then Error "semantic retained function type has a different signature"
  else if
    not
      (List.equal same_parameter_binding retained.function_parameter_bindings_
         function_.function_parameter_bindings_)
  then Error "semantic retained function type has different parameter bindings"
  else if
    not
      (same_variadic_bindings retained.function_variadic_bindings_
         function_.function_variadic_bindings_)
  then Error "semantic retained function type has different variadic bindings"
  else
    match
      validate_function ~table ~parent (-1) Int_set.empty Int_set.empty retained
    with
    | Error _ as error -> error
    | Ok _ -> Ok ()

let find_retained retained_headers function_ =
  List.filter
    (fun retained -> retained.function_symbol_ == function_.function_symbol_)
    retained_headers
  |> function
  | [] -> Ok None
  | [ retained ] -> Ok (Some retained)
  | _ -> Error "semantic retained function type repeats a function symbol"

let substitute_retained ~table ~parent retained_headers function_declarations =
  let rec substitute functions_rev used_rev = function
    | [] ->
        if
          List.length used_rev = List.length retained_headers
          && List.for_all
               (fun retained -> List.memq retained used_rev)
               retained_headers
        then Ok (List.rev functions_rev, used_rev)
        else Error "semantic retained function type was not consumed"
    | function_ :: rest -> (
        match find_retained retained_headers function_ with
        | Error _ as error -> error
        | Ok None -> substitute (function_ :: functions_rev) used_rev rest
        | Ok (Some retained) -> (
            match
              validate_retained_header ~table ~parent retained function_
            with
            | Error _ as error -> error
            | Ok () ->
                substitute
                  (retained :: functions_rev)
                  (retained :: used_rev) rest))
  in
  substitute [] [] function_declarations

let resolve ?(retained_headers = []) ~table ~parent function_declarations =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic function type parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic function types require a module scope"
  else
    let rec validate previous_item seen_symbols seen_scopes = function
      | [] -> (
          match
            substitute_retained ~table ~parent retained_headers
              function_declarations
          with
          | Error _ as error -> error
          | Ok (functions, reused) ->
              List.iter
                (fun function_ -> function_.function_header_reused_ <- true)
                reused;
              Ok { functions })
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
