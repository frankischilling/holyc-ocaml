type location_kind =
  | Named_parameter
  | Variadic_argc
  | Variadic_argv
  | Automatic_local
  | Static_local

type declarator_shape = Object | Function_pointer
type value_shape = Scalar | Array
type dimension_kind = Source_extent | Compiler_placeholder_extent
type dimension = { kind : dimension_kind; value : int64 }
type frame_slot = { displacement : int64; size : int64 }

type location = {
  binding : Function_binding_index.binding;
  symbol : Symbol.t;
  kind : location_kind;
  type_reference : Type_reference.t option;
  checked_type : Type.t;
  declarator_shape : declarator_shape;
  value_shape : value_shape;
  dimensions : dimension list;
  element_size : int64;
  allocated_size : int64;
  alignment : int;
  frame_slot : frame_slot option;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

type function_layout = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  locations : location list;
  frame_size : int64;
  by_symbol : location Int_map.t;
}

type t = {
  table : Symbol_table.t;
  functions : function_layout list;
  by_symbol : function_layout Int_map.t;
}

type dimension_expression =
  | Empty_dimension
  | Closed_expression of Aggregate_layout.expression
  | Non_integral_expression of { detail : string; origin : Symbol.origin }

type dimension_input = {
  dimension : Local_type_resolution.array_dimension;
  expression_origin : Symbol.origin option;
  expression : dimension_expression;
}

type local_input = {
  local : Local_type_resolution.local;
  dimensions : dimension_input list;
}

type function_input = {
  indexed_function : Function_binding_index.indexed_function;
  typed_function : Function_type_resolution.resolved_function;
  local_function : Local_type_resolution.resolved_function;
  locals : local_input list;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_local_extent of {
      symbol : Symbol.t;
      dimension_index : int;
      detail : string;
    }
  | Non_integral_local_extent of {
      symbol : Symbol.t;
      dimension_index : int;
      detail : string;
    }
  | Invalid_local_extent of {
      symbol : Symbol.t;
      dimension_index : int;
      detail : string;
    }
  | Metadata_overflow of { symbol : Symbol.t; detail : string }
  | Incomplete_aggregate_layout of Symbol.t

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let functions (result : t) = result.functions
let function_symbol (function_ : function_layout) = function_.symbol
let function_scope (function_ : function_layout) = function_.scope
let function_item_index (function_ : function_layout) = function_.item_index
let function_locations (function_ : function_layout) = function_.locations
let function_frame_size (function_ : function_layout) = function_.frame_size
let location_binding (location : location) = location.binding
let location_symbol (location : location) = location.symbol
let location_kind (location : location) = location.kind
let location_type_reference (location : location) = location.type_reference
let location_checked_type (location : location) = location.checked_type
let location_declarator_shape (location : location) = location.declarator_shape
let location_value_shape (location : location) = location.value_shape
let location_dimensions (location : location) = location.dimensions
let location_element_size (location : location) = location.element_size
let location_allocated_size (location : location) = location.allocated_size
let location_alignment (location : location) = location.alignment
let location_frame_slot (location : location) = location.frame_slot
let dimension_kind (dimension : dimension) = dimension.kind
let dimension_value (dimension : dimension) = dimension.value
let frame_slot_displacement (slot : frame_slot) = slot.displacement
let frame_slot_size (slot : frame_slot) = slot.size

let location_kind_name = function
  | Named_parameter -> "named-parameter"
  | Variadic_argc -> "variadic-argc"
  | Variadic_argv -> "variadic-argv"
  | Automatic_local -> "automatic-local"
  | Static_local -> "static-local"

let declarator_shape_name = function
  | Object -> "object"
  | Function_pointer -> "function-pointer"

let value_shape_name = function
  | Scalar -> "scalar"
  | Array -> "array"

let dimension_kind_name = function
  | Source_extent -> "source-extent"
  | Compiler_placeholder_extent -> "compiler-placeholder-extent"

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let invalid_input ?origin message =
  { code = "HCSEMA0069"; kind = Invalid_input message; origin }

let unresolved_extent symbol dimension_index detail origin =
  {
    code = "HCSEMA0070";
    kind = Unresolved_local_extent { symbol; dimension_index; detail };
    origin = Some origin;
  }

let non_integral_extent symbol dimension_index detail origin =
  {
    code = "HCSEMA0071";
    kind = Non_integral_local_extent { symbol; dimension_index; detail };
    origin = Some origin;
  }

let invalid_extent symbol dimension_index detail origin =
  {
    code = "HCSEMA0072";
    kind = Invalid_local_extent { symbol; dimension_index; detail };
    origin = Some origin;
  }

let metadata_overflow symbol detail origin =
  {
    code = "HCSEMA0073";
    kind = Metadata_overflow { symbol; detail };
    origin = Some origin;
  }

let incomplete_aggregate symbol origin =
  {
    code = "HCSEMA0074";
    kind = Incomplete_aggregate_layout symbol;
    origin = Some origin;
  }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Unresolved_local_extent { symbol; dimension_index; detail } ->
      Printf.sprintf "local %S dimension %d cannot be resolved: %s"
        (Symbol.name symbol) dimension_index detail
  | Non_integral_local_extent { symbol; dimension_index; detail } ->
      Printf.sprintf "local %S dimension %d is not integral: %s"
        (Symbol.name symbol) dimension_index detail
  | Invalid_local_extent { symbol; dimension_index; detail } ->
      Printf.sprintf "local %S dimension %d is invalid: %s" (Symbol.name symbol)
        dimension_index detail
  | Metadata_overflow { symbol; detail } ->
      Printf.sprintf "frame metadata for %S exceeds the signed 64-bit range: %s"
        (Symbol.name symbol) detail
  | Incomplete_aggregate_layout symbol ->
      Printf.sprintf
        "by-value aggregate type %S has no completed earlier layout"
        (Symbol.name symbol)

let error_to_string error = error.code ^ ": " ^ error_message error

let find_function (result : t) symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) result.by_symbol with
    | Some function_ when function_.symbol == symbol -> Some function_
    | Some _ | None -> None

let find_location (function_ : function_layout) symbol =
  match Int_map.find_opt (symbol_number symbol) function_.by_symbol with
  | Some location when location.symbol == symbol -> Some location
  | Some _ | None -> None

let find_binding_location (function_ : function_layout) binding =
  let symbol = Function_binding_index.binding_symbol binding in
  match find_location function_ symbol with
  | Some (location : location)
    when location.binding == binding && location.symbol == symbol ->
      Some location
  | Some _ | None -> None

let symbol_has_scope symbol scope =
  Symbol.Scope_id.equal (Symbol.scope_id symbol) (Symbol_table.scope_id scope)

let checked_multiply_nonnegative symbol origin detail left right =
  if Int64.compare left 0L < 0 || Int64.compare right 0L < 0 then
    Error (invalid_input ~origin (detail ^ " contains a negative value"))
  else if
    Int64.compare left 0L <> 0
    && Int64.compare right (Int64.div Int64.max_int left) > 0
  then Error (metadata_overflow symbol detail origin)
  else Ok (Int64.mul left right)

let checked_subtract_nonnegative symbol origin detail left right =
  if Int64.compare right 0L < 0 then
    Error (invalid_input ~origin (detail ^ " is negative"))
  else if Int64.compare left (Int64.add Int64.min_int right) < 0 then
    Error (metadata_overflow symbol detail origin)
  else Ok (Int64.sub left right)

let checked_parameter_displacement symbol index =
  let origin = Symbol.origin symbol in
  if index < 0 then
    Error
      (invalid_input ~origin
         (Printf.sprintf "binding %S has a negative parameter index"
            (Symbol.name symbol)))
  else
    let index = Int64.of_int index in
    Result.bind
      (checked_multiply_nonnegative symbol origin "the parameter displacement"
         index 8L) (fun offset ->
        if Int64.compare offset (Int64.sub Int64.max_int 16L) > 0 then
          Error (metadata_overflow symbol "the parameter displacement" origin)
        else Ok (Int64.add 16L offset))

let alignment_for_size size =
  if Int64.compare size 8L >= 0 then 8
  else if Int64.compare size 4L >= 0 then 4
  else if Int64.compare size 2L >= 0 then 2
  else 1

let align_down value alignment =
  Int64.logand value (Int64.neg (Int64.of_int alignment))

let declarator_shape_of_parameter = function
  | Function_type_resolution.Object -> Object
  | Function_type_resolution.Function_pointer _ -> Function_pointer

let declarator_shape_of_local = function
  | Local_type_resolution.Object -> Object
  | Local_type_resolution.Function_pointer _ -> Function_pointer

let validate_checked_type table origin checked_type =
  match Type.base checked_type with
  | Type.Aggregate symbol when not (Symbol_table.owns_symbol table symbol) ->
      Error
        (invalid_input ~origin
           "a frame location type uses an aggregate from another symbol table")
  | Type.Primitive _ | Type.Aggregate _ -> Ok ()

let aggregate_element_size table aggregate_layouts ~before_item origin symbol =
  if not (Symbol_table.owns_symbol table symbol) then
    Error
      (invalid_input ~origin
         "a frame location aggregate belongs to another symbol table")
  else
    match Aggregate_layout.find aggregate_layouts symbol with
    | None -> Error (incomplete_aggregate symbol origin)
    | Some layout when layout.symbol != symbol ->
        Error
          (invalid_input ~origin
             "an aggregate layout has a different semantic identity")
    | Some layout when layout.item_index >= before_item ->
        Error (incomplete_aggregate symbol origin)
    | Some layout when Int64.compare layout.size 0L < 0 ->
        Error
          (invalid_input ~origin
             "a completed aggregate layout has a negative size")
    | Some layout -> Ok layout.size

let element_size table aggregate_layouts ~before_item origin declarator_shape
    checked_type =
  Result.bind (validate_checked_type table origin checked_type) (fun () ->
      if
        declarator_shape = Function_pointer
        || Type.pointer_depth checked_type > 0
      then Ok (Int64.of_int Primitive_type.pointer_byte_size)
      else
        match Type.base checked_type with
        | Type.Primitive (_, primitive) ->
            Ok (Int64.of_int (Primitive_type.info primitive).byte_size)
        | Type.Aggregate symbol ->
            aggregate_element_size table aggregate_layouts ~before_item origin
              symbol)

let rec closed_expression_error = function
  | Aggregate_layout.Integer_expression _
  | Aggregate_layout.Floating_expression _ -> None
  | Aggregate_layout.Current_position_expression origin ->
      Some (origin, "a current-position expression is not closed")
  | Aggregate_layout.Unary_expression { operand; _ } ->
      closed_expression_error operand
  | Aggregate_layout.Binary_expression { left; right; _ } -> (
      match closed_expression_error left with
      | Some _ as error -> error
      | None -> closed_expression_error right)
  | Aggregate_layout.Dependency_expression _
  | Aggregate_layout.Unsupported_expression _ -> None

type expression_result_kind = Integer_result | Floating_result

let common_result_kind left right =
  match (left, right) with
  | Integer_result, Integer_result -> Integer_result
  | (Integer_result | Floating_result), (Integer_result | Floating_result) ->
      Floating_result

let rec expression_result_kind = function
  | Aggregate_layout.Integer_expression _
  | Aggregate_layout.Current_position_expression _
  | Aggregate_layout.Dependency_expression _
  | Aggregate_layout.Unsupported_expression _ -> Integer_result
  | Aggregate_layout.Floating_expression _ -> Floating_result
  | Aggregate_layout.Unary_expression { operator; operand; _ } -> (
      match operator with
      | Aggregate_layout.Identity | Aggregate_layout.Negate ->
          expression_result_kind operand
      | Aggregate_layout.Logical_not | Aggregate_layout.Bitwise_not ->
          Integer_result)
  | Aggregate_layout.Binary_expression { operator; left; right; _ } -> (
      match operator with
      | Aggregate_layout.Power -> Floating_result
      | Aggregate_layout.Less
      | Aggregate_layout.Greater
      | Aggregate_layout.Less_equal
      | Aggregate_layout.Greater_equal
      | Aggregate_layout.Equal
      | Aggregate_layout.Not_equal
      | Aggregate_layout.Logical_and
      | Aggregate_layout.Logical_xor
      | Aggregate_layout.Logical_or -> Integer_result
      | Aggregate_layout.Shift_left
      | Aggregate_layout.Shift_right
      | Aggregate_layout.Multiply
      | Aggregate_layout.Divide
      | Aggregate_layout.Modulo
      | Aggregate_layout.Bit_and
      | Aggregate_layout.Bit_xor
      | Aggregate_layout.Bit_or
      | Aggregate_layout.Add
      | Aggregate_layout.Subtract ->
          common_result_kind
            (expression_result_kind left)
            (expression_result_kind right))

let integral_value_of_evaluated symbol dimension_index expression_origin
    expression value =
  match expression_result_kind expression with
  | Integer_result -> Ok value
  | Floating_result ->
      let value = Int64.float_of_bits value in
      if Float.trunc value <> value then
        Error
          (non_integral_extent symbol dimension_index
             (Printf.sprintf "the closed expression evaluates to %.17g" value)
             expression_origin)
      else
        let lower = Int64.to_float Int64.min_int in
        let upper = 9223372036854775808.0 in
        if value < lower || value >= upper then
          Error
            (invalid_extent symbol dimension_index
               "the closed expression is outside the signed 64-bit range"
               expression_origin)
        else Ok (Int64.of_float value)

let evaluate_dimension symbol expected_index input =
  let semantic_dimension = input.dimension in
  let actual_index =
    Local_type_resolution.array_dimension_index semantic_dimension
  in
  let origin =
    Local_type_resolution.array_dimension_origin semantic_dimension
  in
  let expected_expression_origin =
    Local_type_resolution.array_dimension_expression_origin semantic_dimension
  in
  if actual_index <> expected_index then
    Error
      (invalid_input ~origin
         (Printf.sprintf "local %S dimensions are outside source order"
            (Symbol.name symbol)))
  else if input.expression_origin <> expected_expression_origin then
    Error
      (invalid_input ~origin
         "local dimension evidence has a different expression origin")
  else
    match input.expression with
    | Empty_dimension ->
        if expected_index <> 0 then
          Error
            (invalid_extent symbol expected_index
               "only the first array dimension can be empty" origin)
        else if
          Option.is_some
            (Local_type_resolution.array_dimension_expression_origin
               semantic_dimension)
        then
          Error
            (invalid_input ~origin
               "an empty dimension does not match its semantic expression")
        else Ok 0L
    | Non_integral_expression { detail; origin = expression_origin } ->
        if
          Option.is_none
            (Local_type_resolution.array_dimension_expression_origin
               semantic_dimension)
        then
          Error
            (invalid_input ~origin
               "a non-integral dimension does not match an empty dimension")
        else
          Error
            (non_integral_extent symbol expected_index detail expression_origin)
    | Closed_expression expression -> (
        if
          Option.is_none
            (Local_type_resolution.array_dimension_expression_origin
               semantic_dimension)
        then
          Error
            (invalid_input ~origin
               "a dimension expression does not match an empty dimension")
        else
          match closed_expression_error expression with
          | Some (expression_origin, detail) ->
              Error
                (invalid_extent symbol expected_index detail expression_origin)
          | None -> (
              let result_kind = expression_result_kind expression in
              let context =
                match result_kind with
                | Integer_result -> Aggregate_layout.Array_dimension
                | Floating_result -> Aggregate_layout.Aggregate_offset
              in
              match
                Aggregate_layout.evaluate_expression ~context
                  ~current_position:0L expression
              with
              | Ok evaluated ->
                  let expression_origin =
                    Option.value expected_expression_origin ~default:origin
                  in
                  Result.bind
                    (integral_value_of_evaluated symbol expected_index
                       expression_origin expression evaluated) (fun value ->
                      if Int64.compare value 0L < 0 then
                        Error
                          (invalid_extent symbol expected_index
                             (Printf.sprintf "extent %Ld is negative" value)
                             origin)
                      else Ok value)
              | Error error -> (
                  let error_origin =
                    Option.value
                      (Aggregate_layout.error_origin error)
                      ~default:origin
                  in
                  match Aggregate_layout.error_kind error with
                  | Aggregate_layout.Unresolved_dependency { detail; _ } ->
                      Error
                        (unresolved_extent symbol expected_index detail
                           error_origin)
                  | Aggregate_layout.Metadata_overflow detail ->
                      Error (metadata_overflow symbol detail error_origin)
                  | Aggregate_layout.Invalid_array_dimension value ->
                      Error
                        (invalid_extent symbol expected_index
                           (Printf.sprintf "extent %Ld is negative" value)
                           error_origin)
                  | Aggregate_layout.Invalid_input detail
                  | Aggregate_layout.Invalid_layout_expression detail ->
                      Error
                        (invalid_extent symbol expected_index detail
                           error_origin)
                  | Aggregate_layout.Division_by_zero
                  | Aggregate_layout.Signed_division_overflow
                  | Aggregate_layout.Non_finite_layout_value
                  | Aggregate_layout.Numeric_conversion_overflow ->
                      Error
                        (invalid_extent symbol expected_index
                           (Aggregate_layout.error_message error)
                           error_origin))))

let evaluate_dimensions symbol semantic_dimensions inputs =
  let rec loop index total values_rev semantic inputs =
    match (semantic, inputs) with
    | [], [] -> Ok (total, List.rev values_rev)
    | expected :: semantic_rest, input :: input_rest ->
        if input.dimension != expected then
          Error
            (invalid_input
               ~origin:(Local_type_resolution.array_dimension_origin expected)
               "local dimension evidence has a different semantic identity")
        else
          Result.bind (evaluate_dimension symbol index input) (fun value ->
              Result.bind
                (checked_multiply_nonnegative symbol
                   (Local_type_resolution.array_dimension_origin expected)
                   "the local array element count" total value)
                (fun total ->
                  loop (index + 1) total
                    ({ kind = Source_extent; value } :: values_rev)
                    semantic_rest input_rest))
    | [], _ :: _ | _ :: _, [] ->
        Error
          (invalid_input ~origin:(Symbol.origin symbol)
             "local dimension evidence does not match the resolved type")
  in
  loop 0 1L [] semantic_dimensions inputs

let typed_parameter_evidence function_ =
  let named =
    Function_type_resolution.function_parameter_bindings function_
    |> List.map (fun binding -> `Named binding)
  in
  let synthetic =
    match Function_type_resolution.function_variadic_bindings function_ with
    | None -> []
    | Some variadic ->
        [
          `Synthetic (Function_type_resolution.variadic_argc variadic);
          `Synthetic (Function_type_resolution.variadic_argv variadic);
        ]
  in
  named @ synthetic

let parameter_at_index function_ index =
  Function_type_resolution.function_signature function_
  |> Function_type_resolution.signature_parameters
  |> List.find_opt (fun parameter ->
      Function_type_resolution.parameter_index parameter = index)

let validate_binding_common table scope ordinal binding =
  let symbol = Function_binding_index.binding_symbol binding in
  if Function_binding_index.binding_ordinal binding <> ordinal then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "function frame bindings are outside source order")
  else if not (Symbol_table.owns_symbol table symbol) then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "a function frame binding belongs to another symbol table")
  else if not (symbol_has_scope symbol scope) then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "a function frame binding belongs to another function scope")
  else Ok ()

let parameter_location table aggregate_layouts typed_function binding evidence =
  let symbol = Function_binding_index.binding_symbol binding in
  let binding_index = Function_binding_index.binding_parameter_index binding in
  let origin = Symbol.origin symbol in
  match (Function_binding_index.binding_kind binding, evidence) with
  | Function_binding_index.Named_parameter, `Named typed_binding -> (
      let typed_symbol =
        Function_type_resolution.parameter_binding_symbol typed_binding
      in
      let typed_index =
        Function_type_resolution.parameter_binding_index typed_binding
      in
      if typed_symbol != symbol then
        Error
          (invalid_input ~origin
             "a named parameter has a different binding identity")
      else if binding_index <> Some typed_index then
        Error
          (invalid_input ~origin
             "a named parameter has a different source position")
      else
        match parameter_at_index typed_function typed_index with
        | None ->
            Error
              (invalid_input ~origin
                 "a named parameter has no resolved signature parameter")
        | Some parameter ->
            let type_reference =
              Function_type_resolution.parameter_type_reference parameter
            in
            let checked_type = Type_reference.resolved_type type_reference in
            let declarator_shape =
              Function_type_resolution.parameter_declarator_kind parameter
              |> declarator_shape_of_parameter
            in
            Result.bind
              (element_size table aggregate_layouts
                 ~before_item:
                   (Function_type_resolution.function_item_index typed_function)
                 origin declarator_shape checked_type)
              (fun element_size ->
                Result.map
                  (fun displacement ->
                    {
                      binding;
                      symbol;
                      kind = Named_parameter;
                      type_reference = Some type_reference;
                      checked_type;
                      declarator_shape;
                      value_shape = Scalar;
                      dimensions = [];
                      element_size;
                      allocated_size = 8L;
                      alignment = 8;
                      frame_slot = Some { displacement; size = 8L };
                    })
                  (checked_parameter_displacement symbol typed_index)))
  | ( ( Function_binding_index.Variadic_argc
      | Function_binding_index.Variadic_argv ),
      `Synthetic typed_binding ) ->
      let typed_symbol =
        Function_type_resolution.synthetic_binding_symbol typed_binding
      in
      let typed_index =
        Function_type_resolution.synthetic_binding_index typed_binding
      in
      let expected_kind, expected_synthetic =
        match Function_binding_index.binding_kind binding with
        | Function_binding_index.Variadic_argc ->
            (Variadic_argc, Function_type_resolution.Argc)
        | Function_binding_index.Variadic_argv ->
            (Variadic_argv, Function_type_resolution.Argv)
        | Function_binding_index.Named_parameter
        | Function_binding_index.Automatic_local
        | Function_binding_index.Static_local -> assert false
      in
      if typed_symbol != symbol then
        Error
          (invalid_input ~origin
             "a synthetic parameter has a different binding identity")
      else if
        Function_type_resolution.synthetic_binding_kind typed_binding
        <> expected_synthetic
      then
        Error
          (invalid_input ~origin
             "a synthetic parameter has a different binding kind")
      else if binding_index <> Some typed_index then
        Error
          (invalid_input ~origin
             "a synthetic parameter has a different source position")
      else
        let checked_type =
          Function_type_resolution.synthetic_binding_type typed_binding
        in
        let value_shape, dimensions =
          match
            Function_type_resolution.synthetic_binding_shape typed_binding
          with
          | Function_type_resolution.Scalar -> (Scalar, [])
          | Function_type_resolution.Array
              { source_extent; compiler_placeholder_extent } ->
              let dimensions =
                Option.to_list
                  (Option.map
                     (fun value ->
                       { kind = Source_extent; value = Int64.of_int value })
                     source_extent)
                @ [
                    {
                      kind = Compiler_placeholder_extent;
                      value = Int64.of_int compiler_placeholder_extent;
                    };
                  ]
              in
              (Array, dimensions)
        in
        if
          List.exists
            (fun dimension -> Int64.compare dimension.value 0L < 0)
            dimensions
        then
          Error
            (invalid_input ~origin
               "a synthetic parameter has a negative array extent")
        else
          Result.bind
            (element_size table aggregate_layouts
               ~before_item:
                 (Function_type_resolution.function_item_index typed_function)
               origin Object checked_type)
            (fun element_size ->
              Result.map
                (fun displacement ->
                  {
                    binding;
                    symbol;
                    kind = expected_kind;
                    type_reference = None;
                    checked_type;
                    declarator_shape = Object;
                    value_shape;
                    dimensions;
                    element_size;
                    allocated_size = 8L;
                    alignment = 8;
                    frame_slot = Some { displacement; size = 8L };
                  })
                (checked_parameter_displacement symbol typed_index))
  | Function_binding_index.Named_parameter, `Synthetic _
  | ( ( Function_binding_index.Variadic_argc
      | Function_binding_index.Variadic_argv ),
      `Named _ ) ->
      Error
        (invalid_input ~origin
           "function binding and parameter type kinds do not match")
  | ( ( Function_binding_index.Automatic_local
      | Function_binding_index.Static_local ),
      (`Named _ | `Synthetic _) ) ->
      Error
        (invalid_input ~origin
           "a local binding appears among the parameter type evidence")

let local_location table aggregate_layouts ~function_item cursor binding input =
  let symbol = Function_binding_index.binding_symbol binding in
  let local = input.local in
  let local_symbol = Local_type_resolution.local_symbol local in
  let origin = Symbol.origin symbol in
  let expected_storage, kind =
    match Function_binding_index.binding_kind binding with
    | Function_binding_index.Automatic_local ->
        (Local_type_resolution.Automatic, Automatic_local)
    | Function_binding_index.Static_local ->
        (Local_type_resolution.Static, Static_local)
    | Function_binding_index.Named_parameter
    | Function_binding_index.Variadic_argc
    | Function_binding_index.Variadic_argv -> assert false
  in
  if local_symbol != symbol then
    Error (invalid_input ~origin "a local has a different binding identity")
  else if Local_type_resolution.local_storage local <> expected_storage then
    Error (invalid_input ~origin "a local has a different storage class")
  else if
    Function_binding_index.binding_local_declaration_index binding
    <> Some (Local_type_resolution.local_declaration_index local)
    || Function_binding_index.binding_local_declarator_index binding
       <> Some (Local_type_resolution.local_declarator_index local)
  then Error (invalid_input ~origin "a local has a different source position")
  else
    let type_reference = Local_type_resolution.local_type_reference local in
    let checked_type = Type_reference.resolved_type type_reference in
    let declarator_shape =
      Local_type_resolution.local_declarator_kind local
      |> declarator_shape_of_local
    in
    Result.bind
      (element_size table aggregate_layouts ~before_item:function_item origin
         declarator_shape checked_type) (fun element_size ->
        Result.bind
          (evaluate_dimensions symbol
             (Local_type_resolution.local_array_dimensions local)
             input.dimensions)
          (fun (element_count, dimensions) ->
            Result.bind
              (checked_multiply_nonnegative symbol origin
                 "the local storage size" element_size element_count)
              (fun allocated_size ->
                let alignment = alignment_for_size allocated_size in
                let value_shape =
                  match dimensions with
                  | [] -> Scalar
                  | _ :: _ -> Array
                in
                match kind with
                | Static_local ->
                    if
                      Int64.compare allocated_size (Int64.sub Int64.max_int 7L)
                      > 0
                    then
                      Error
                        (metadata_overflow symbol
                           "the eight-byte static-storage padding" origin)
                    else
                      Ok
                        ( {
                            binding;
                            symbol;
                            kind;
                            type_reference = Some type_reference;
                            checked_type;
                            declarator_shape;
                            value_shape;
                            dimensions;
                            element_size;
                            allocated_size;
                            alignment = 8;
                            frame_slot = None;
                          },
                          cursor )
                | Automatic_local ->
                    Result.map
                      (fun subtracted ->
                        let displacement =
                          if alignment = 1 then subtracted
                          else align_down subtracted alignment
                        in
                        ( {
                            binding;
                            symbol;
                            kind;
                            type_reference = Some type_reference;
                            checked_type;
                            declarator_shape;
                            value_shape;
                            dimensions;
                            element_size;
                            allocated_size;
                            alignment;
                            frame_slot =
                              Some { displacement; size = allocated_size };
                          },
                          displacement ))
                      (checked_subtract_nonnegative symbol origin
                         "the automatic-local frame cursor" cursor
                         allocated_size)
                | Named_parameter | Variadic_argc | Variadic_argv ->
                    assert false)))

let finalize_frame_size function_symbol cursor =
  let origin = Symbol.origin function_symbol in
  let final_cursor = align_down cursor 8 in
  if Int64.equal final_cursor Int64.min_int then
    Error
      (metadata_overflow function_symbol "the final local-frame size" origin)
  else Ok (Int64.neg final_cursor)

let validate_function_identity table parent previous_item seen_symbols input =
  let indexed = input.indexed_function in
  let typed = input.typed_function in
  let local = input.local_function in
  let symbol = Function_binding_index.function_symbol indexed in
  let scope = Function_binding_index.function_scope indexed in
  let item = Function_binding_index.function_item_index indexed in
  let typed_symbol = Function_type_resolution.function_symbol typed in
  let typed_scope = Function_type_resolution.function_scope typed in
  let typed_item = Function_type_resolution.function_item_index typed in
  let local_symbol = Local_type_resolution.function_symbol local in
  let local_scope = Local_type_resolution.function_scope local in
  let local_item = Local_type_resolution.function_item_index local in
  let key = symbol_number symbol in
  if item < 0 || item <= previous_item then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "function frame inputs are outside module source order")
  else if Int_set.mem key seen_symbols then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "a function frame input is repeated")
  else if
    not
      (Symbol_table.owns_symbol table symbol
      && Symbol_table.owns_symbol table typed_symbol
      && Symbol_table.owns_symbol table local_symbol)
  then Error (invalid_input "function frame inputs use another symbol table")
  else if
    not
      (Symbol_table.owns_scope table scope
      && Symbol_table.owns_scope table typed_scope
      && Symbol_table.owns_scope table local_scope)
  then Error (invalid_input "function frame scopes use another symbol table")
  else if typed_symbol != symbol || local_symbol != symbol then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "function frame passes have different function identities")
  else if typed_scope != scope || local_scope != scope then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "function frame passes have different function scopes")
  else if typed_item <> item || local_item <> item then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "function frame passes have different module positions")
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "a function frame input does not use a function symbol")
  else if not (symbol_has_scope symbol parent) then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "a function frame symbol does not belong to the module")
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "a function frame input does not use a function scope")
  else if
    match Symbol_table.parent scope with
    | Some scope_parent -> scope_parent != parent
    | None -> true
  then
    Error
      (invalid_input ~origin:(Symbol.origin symbol)
         "a function frame scope does not belong to the module")
  else Ok (item, Int_set.add key seen_symbols)

let build_function table aggregate_layouts input =
  let indexed = input.indexed_function in
  let typed = input.typed_function in
  let local_function = input.local_function in
  let function_symbol = Function_binding_index.function_symbol indexed in
  let function_scope = Function_binding_index.function_scope indexed in
  let bindings = Function_binding_index.function_bindings indexed in
  let parameter_evidence = typed_parameter_evidence typed in
  let semantic_locals = Local_type_resolution.function_locals local_function in
  let rec loop ordinal cursor locations_rev by_symbol bindings parameters
      semantic_locals local_inputs =
    match bindings with
    | [] ->
        if parameters <> [] then
          Error
            (invalid_input
               ~origin:(Symbol.origin function_symbol)
               "function frame input is missing resolved parameters")
        else if semantic_locals <> [] || local_inputs <> [] then
          Error
            (invalid_input
               ~origin:(Symbol.origin function_symbol)
               "function frame input is missing resolved locals")
        else
          Result.map
            (fun frame_size ->
              {
                symbol = function_symbol;
                scope = function_scope;
                item_index = Function_binding_index.function_item_index indexed;
                locations = List.rev locations_rev;
                frame_size;
                by_symbol;
              })
            (finalize_frame_size function_symbol cursor)
    | binding :: rest ->
        Result.bind
          (validate_binding_common table function_scope ordinal binding)
          (fun () ->
            let symbol = Function_binding_index.binding_symbol binding in
            let key = symbol_number symbol in
            if Int_map.mem key by_symbol then
              Error
                (invalid_input ~origin:(Symbol.origin symbol)
                   "a function frame binding identity is repeated")
            else
              match Function_binding_index.binding_kind binding with
              | Function_binding_index.Named_parameter
              | Function_binding_index.Variadic_argc
              | Function_binding_index.Variadic_argv -> (
                  match parameters with
                  | [] ->
                      Error
                        (invalid_input ~origin:(Symbol.origin symbol)
                           "a function frame binding has no type evidence")
                  | evidence :: parameter_rest ->
                      Result.bind
                        (parameter_location table aggregate_layouts typed
                           binding evidence) (fun location ->
                          loop (ordinal + 1) cursor
                            (location :: locations_rev)
                            (Int_map.add key location by_symbol)
                            rest parameter_rest semantic_locals local_inputs))
              | Function_binding_index.Automatic_local
              | Function_binding_index.Static_local -> (
                  if parameters <> [] then
                    Error
                      (invalid_input ~origin:(Symbol.origin symbol)
                         "a local appears before all resolved parameters")
                  else
                    match (semantic_locals, local_inputs) with
                    | semantic_local :: semantic_rest, input :: input_rest ->
                        if input.local != semantic_local then
                          Error
                            (invalid_input ~origin:(Symbol.origin symbol)
                               "local frame evidence has a different semantic \
                                identity")
                        else
                          Result.bind
                            (local_location table aggregate_layouts
                               ~function_item:
                                 (Function_binding_index.function_item_index
                                    indexed)
                               cursor binding input)
                            (fun (location, cursor) ->
                              loop (ordinal + 1) cursor
                                (location :: locations_rev)
                                (Int_map.add key location by_symbol)
                                rest [] semantic_rest input_rest)
                    | [], [] ->
                        Error
                          (invalid_input ~origin:(Symbol.origin symbol)
                             "a function frame binding has no local type \
                              evidence")
                    | [], _ :: _ | _ :: _, [] ->
                        Error
                          (invalid_input ~origin:(Symbol.origin symbol)
                             "local frame evidence does not match the resolved \
                              locals")))
  in
  loop 0 0L [] Int_map.empty bindings parameter_evidence semantic_locals
    input.locals

let validate_parent table parent =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input "function frame parent belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "function frames require a module scope")
  else Ok ()

let validate_aggregate_layouts table aggregate_layouts =
  let rec loop = function
    | [] -> Ok ()
    | layout :: rest ->
        if not (Symbol_table.owns_symbol table layout.Aggregate_layout.symbol)
        then
          Error
            (invalid_input ~origin:layout.origin
               "aggregate frame evidence belongs to another symbol table")
        else loop rest
  in
  if not (Aggregate_layout.owns_table aggregate_layouts table) then
    Error (invalid_input "aggregate frame evidence uses another symbol table")
  else loop (Aggregate_layout.layouts aggregate_layouts)

let layout ~table ~parent ~aggregate_layouts inputs =
  Result.bind (validate_parent table parent) (fun () ->
      Result.bind (validate_aggregate_layouts table aggregate_layouts)
        (fun () ->
          let rec loop previous_item seen functions_rev by_symbol = function
            | [] -> Ok { table; functions = List.rev functions_rev; by_symbol }
            | input :: rest ->
                Result.bind
                  (validate_function_identity table parent previous_item seen
                     input) (fun (item, seen) ->
                    Result.bind (build_function table aggregate_layouts input)
                      (fun function_ ->
                        loop item seen
                          (function_ :: functions_rev)
                          (Int_map.add
                             (symbol_number function_.symbol)
                             function_ by_symbol)
                          rest))
          in
          loop (-1) Int_set.empty [] Int_map.empty inputs))
