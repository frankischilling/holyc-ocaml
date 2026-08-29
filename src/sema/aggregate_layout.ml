type unary_operator = Identity | Negate | Logical_not | Bitwise_not

type binary_operator =
  | Power
  | Shift_left
  | Shift_right
  | Multiply
  | Divide
  | Modulo
  | Bit_and
  | Bit_xor
  | Bit_or
  | Add
  | Subtract
  | Less
  | Greater
  | Less_equal
  | Greater_equal
  | Equal
  | Not_equal
  | Logical_and
  | Logical_xor
  | Logical_or

type dependency_kind =
  | Identifier_dependency
  | Sizeof_dependency
  | Offset_dependency
  | Defined_dependency
  | Call_dependency
  | Aggregate_dependency

type expression =
  | Integer_expression of { value : int64; origin : Symbol.origin }
  | Floating_expression of { value : float; origin : Symbol.origin }
  | Current_position_expression of Symbol.origin
  | Unary_expression of {
      operator : unary_operator;
      operand : expression;
      origin : Symbol.origin;
    }
  | Binary_expression of {
      operator : binary_operator;
      left : expression;
      right : expression;
      origin : Symbol.origin;
    }
  | Dependency_expression of {
      dependency_kind : dependency_kind;
      detail : string;
      origin : Symbol.origin;
    }
  | Unsupported_expression of { description : string; origin : Symbol.origin }

type expression_context = Array_dimension | Aggregate_offset

type dimension = {
  dimension_expression : expression option;
  dimension_origin : Symbol.origin;
}

type member_input = {
  member_symbol : Symbol.t;
  member_path : int list;
  member_declarator_index : int;
  member_origin : Symbol.origin;
  member_type : Type.t;
  member_is_function_pointer : bool;
  member_dimensions : dimension list;
}

type item =
  | Field of member_input
  | Offset_directive of expression
  | Anonymous_union of { union_origin : Symbol.origin; union_items : item list }
  | Empty_member of Symbol.origin

type aggregate_kind = Class | Union
type base_input = { base_symbol : Symbol.t; base_origin : Symbol.origin }

type aggregate_input = {
  aggregate_symbol : Symbol.t;
  aggregate_scope : Symbol_table.scope;
  aggregate_kind : aggregate_kind;
  aggregate_item_index : int;
  aggregate_origin : Symbol.origin;
  aggregate_base : base_input option;
  aggregate_items : item list;
}

type signedness = Signed | Unsigned | Not_applicable

type member_layout = {
  symbol : Symbol.t;
  path : int list;
  declarator_index : int;
  origin : Symbol.origin;
  offset : int64;
  size : int64;
  element_size : int64;
  dimensions : int64 list;
  signedness : signedness;
  alignment : int;
}

type base_layout = {
  symbol : Symbol.t;
  origin : Symbol.origin;
  offset : int64;
  size : int64;
}

type aggregate_layout = {
  symbol : Symbol.t;
  kind : aggregate_kind;
  item_index : int;
  origin : Symbol.origin;
  size : int64;
  alignment : int;
  negative_offset : int64;
  base : base_layout option;
  members : member_layout list;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

type t = {
  table : Symbol_table.t;
  layouts : aggregate_layout list;
  by_symbol : aggregate_layout Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_dependency of {
      dependency_kind : dependency_kind;
      detail : string;
    }
  | Invalid_array_dimension of int64
  | Division_by_zero
  | Signed_division_overflow
  | Non_finite_layout_value
  | Numeric_conversion_overflow
  | Metadata_overflow of string
  | Invalid_layout_expression of string

type error = {
  code : string;
  kind : error_kind;
  origin : Symbol.origin option;
  message : string;
}

let dependency_kind_name = function
  | Identifier_dependency -> "identifier"
  | Sizeof_dependency -> "sizeof"
  | Offset_dependency -> "offset"
  | Defined_dependency -> "defined"
  | Call_dependency -> "function call"
  | Aggregate_dependency -> "aggregate layout"

let signedness_name = function
  | Signed -> "signed"
  | Unsigned -> "unsigned"
  | Not_applicable -> "not-applicable"

let make_error ?origin code kind message = { code; kind; origin; message }

let invalid_input ?origin message =
  make_error ?origin "HCSEMA0001" (Invalid_input message) message

let unresolved origin dependency_kind detail =
  make_error ~origin "HCSEMA0002"
    (Unresolved_dependency { dependency_kind; detail })
    (Printf.sprintf "aggregate layout needs the unresolved %s %s"
       (dependency_kind_name dependency_kind)
       detail)

let invalid_dimension origin value =
  make_error ~origin "HCSEMA0003" (Invalid_array_dimension value)
    (Printf.sprintf "array dimension %Ld is negative" value)

let division_by_zero origin =
  make_error ~origin "HCSEMA0004" Division_by_zero
    "aggregate layout expression divides by zero"

let division_overflow origin =
  make_error ~origin "HCSEMA0005" Signed_division_overflow
    "aggregate layout expression overflows when dividing I64_MIN by -1"

let non_finite origin =
  make_error ~origin "HCSEMA0006" Non_finite_layout_value
    "aggregate layout expression produced a non-finite floating value"

let conversion_overflow origin =
  make_error ~origin "HCSEMA0007" Numeric_conversion_overflow
    "aggregate layout expression does not fit in a signed 64-bit value"

let metadata_overflow origin detail =
  make_error ~origin "HCSEMA0008" (Metadata_overflow detail)
    (Printf.sprintf "aggregate layout overflows while calculating %s" detail)

let invalid_expression origin description =
  make_error ~origin "HCSEMA0009" (Invalid_layout_expression description)
    (Printf.sprintf "%s cannot be evaluated in a closed aggregate layout"
       description)

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin
let error_message error = error.message
let error_to_string error = Printf.sprintf "%s: %s" error.code error.message

type number = Integer of int64 | Floating of float

let expression_origin = function
  | Integer_expression { origin; _ }
  | Floating_expression { origin; _ }
  | Current_position_expression origin
  | Unary_expression { origin; _ }
  | Binary_expression { origin; _ }
  | Dependency_expression { origin; _ }
  | Unsupported_expression { origin; _ } -> origin

let truthy = function
  | Integer value -> not (Int64.equal value 0L)
  | Floating value -> not (Int64.equal (Int64.bits_of_float value) 0L)

let boolean value = Integer (if value then 1L else 0L)

let as_float = function
  | Integer value -> Int64.to_float value
  | Floating value -> value

let common left right =
  match (left, right) with
  | Integer left, Integer right -> `Integer (left, right)
  | _ -> `Floating (as_float left, as_float right)

let raw_common left right =
  match common left right with
  | `Integer (left, right) -> (`Integer, left, right)
  | `Floating (left, right) ->
      (`Floating, Int64.bits_of_float left, Int64.bits_of_float right)

let from_raw kind bits =
  match kind with
  | `Integer -> Integer bits
  | `Floating -> Floating (Int64.float_of_bits bits)

let shift_count value = Int64.logand value 63L |> Int64.to_int

let evaluate_unary operator value =
  match (operator, value) with
  | Identity, value -> value
  | Negate, Integer value -> Integer (Int64.neg value)
  | Negate, Floating value -> Floating (-.value)
  | Logical_not, value -> boolean (not (truthy value))
  | Bitwise_not, Integer value -> Integer (Int64.lognot value)
  | Bitwise_not, Floating value ->
      Integer (Int64.bits_of_float value |> Int64.lognot)

let compare_numbers operator left right =
  match common left right with
  | `Integer (left, right) -> (
      let comparison = Int64.compare left right in
      match operator with
      | Equal -> Int64.equal left right
      | Not_equal -> not (Int64.equal left right)
      | Less -> comparison < 0
      | Greater -> comparison > 0
      | Less_equal -> comparison <= 0
      | Greater_equal -> comparison >= 0
      | _ -> invalid_arg "expected a comparison operator")
  | `Floating (left, right) -> (
      match operator with
      | Equal ->
          Int64.equal (Int64.bits_of_float left) (Int64.bits_of_float right)
      | Not_equal ->
          not
            (Int64.equal (Int64.bits_of_float left) (Int64.bits_of_float right))
      | Less -> left < right
      | Greater -> left > right
      | Less_equal -> left <= right
      | Greater_equal -> left >= right
      | _ -> invalid_arg "expected a comparison operator")

let evaluate_division ~remainder origin left right =
  match common left right with
  | `Integer (left, right) ->
      if Int64.equal right 0L then Error (division_by_zero origin)
      else if Int64.equal left Int64.min_int && Int64.equal right (-1L) then
        Error (division_overflow origin)
      else if remainder then Ok (Integer (Int64.rem left right))
      else Ok (Integer (Int64.div left right))
  | `Floating (left, right) ->
      if remainder then Ok (Floating (mod_float left right))
      else Ok (Floating (left /. right))

let evaluate_eager_binary operator origin left right =
  match operator with
  | Power -> Ok (Floating (as_float left ** as_float right))
  | Shift_left | Shift_right ->
      let kind, left, right = raw_common left right in
      let count = shift_count right in
      let bits =
        match operator with
        | Shift_left -> Int64.shift_left left count
        | Shift_right -> Int64.shift_right left count
        | _ -> assert false
      in
      Ok (from_raw kind bits)
  | Multiply -> (
      match common left right with
      | `Integer (left, right) -> Ok (Integer (Int64.mul left right))
      | `Floating (left, right) -> Ok (Floating (left *. right)))
  | Divide -> evaluate_division ~remainder:false origin left right
  | Modulo -> evaluate_division ~remainder:true origin left right
  | Bit_and | Bit_xor | Bit_or ->
      let kind, left, right = raw_common left right in
      let bits =
        match operator with
        | Bit_and -> Int64.logand left right
        | Bit_xor -> Int64.logxor left right
        | Bit_or -> Int64.logor left right
        | _ -> assert false
      in
      Ok (from_raw kind bits)
  | Add | Subtract -> (
      match common left right with
      | `Integer (left, right) ->
          if operator = Add then Ok (Integer (Int64.add left right))
          else Ok (Integer (Int64.sub left right))
      | `Floating (left, right) ->
          if operator = Add then Ok (Floating (left +. right))
          else Ok (Floating (left -. right)))
  | Less | Greater | Less_equal | Greater_equal | Equal | Not_equal ->
      Ok (boolean (compare_numbers operator left right))
  | Logical_xor -> Ok (boolean (truthy left <> truthy right))
  | Logical_and | Logical_or ->
      invalid_arg "short-circuit operators are evaluated separately"

let rec evaluate_number current_position = function
  | Integer_expression { value; _ } -> Ok (Integer value)
  | Floating_expression { value; _ } -> Ok (Floating value)
  | Current_position_expression _ -> Ok (Integer current_position)
  | Dependency_expression { dependency_kind; detail; origin } ->
      Error (unresolved origin dependency_kind detail)
  | Unsupported_expression { description; origin } ->
      Error (invalid_expression origin description)
  | Unary_expression { operator; operand; _ } ->
      Result.map (evaluate_unary operator)
        (evaluate_number current_position operand)
  | Binary_expression { operator; left; right; origin } ->
      Result.bind (evaluate_number current_position left) (fun left ->
          match operator with
          | Logical_and when not (truthy left) -> Ok (boolean false)
          | Logical_or when truthy left -> Ok (boolean true)
          | Logical_and | Logical_or ->
              Result.map
                (fun right -> boolean (truthy right))
                (evaluate_number current_position right)
          | _ ->
              Result.bind (evaluate_number current_position right) (fun right ->
                  evaluate_eager_binary operator origin left right))

let float_to_i64 origin value =
  if not (Float.is_finite value) then Error (non_finite origin)
  else
    let lower = Int64.to_float Int64.min_int in
    let upper = 9223372036854775808.0 in
    if value < lower || value >= upper then Error (conversion_overflow origin)
    else Ok (Int64.of_float value)

let evaluate_expression ~context ~current_position expression =
  Result.bind (evaluate_number current_position expression) (function
    | Integer value -> Ok value
    | Floating value -> (
        match context with
        | Array_dimension -> float_to_i64 (expression_origin expression) value
        | Aggregate_offset ->
            if Float.is_finite value then Ok (Int64.bits_of_float value)
            else Error (non_finite (expression_origin expression))))

let symbol_key symbol = Symbol.id symbol |> Symbol.Id.to_int

let same_scope left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let checked_add origin detail left right =
  if
    Int64.compare right 0L > 0
    && Int64.compare left (Int64.sub Int64.max_int right) > 0
    || Int64.compare right 0L < 0
       && Int64.compare left (Int64.sub Int64.min_int right) < 0
  then Error (metadata_overflow origin detail)
  else Ok (Int64.add left right)

let checked_multiply_nonnegative origin detail left right =
  if Int64.compare left 0L < 0 || Int64.compare right 0L < 0 then
    Error (invalid_input ~origin (detail ^ " cannot be negative"))
  else if
    (not (Int64.equal right 0L))
    && Int64.compare left (Int64.div Int64.max_int right) > 0
  then Error (metadata_overflow origin detail)
  else Ok (Int64.mul left right)

let checked_negative_magnitude origin value =
  if Int64.equal value Int64.min_int then
    Error (metadata_overflow origin "the negative member displacement")
  else Ok (Int64.neg value)

let primitive_signedness primitive =
  match (Primitive_type.info primitive).signedness with
  | Primitive_type.Signed -> Signed
  | Primitive_type.Unsigned -> Unsigned
  | Primitive_type.Not_applicable -> Not_applicable

let element_shape (previous : aggregate_layout Int_map.t)
    (member : member_input) =
  if
    member.member_is_function_pointer
    || Type.pointer_depth member.member_type > 0
  then Ok (8L, Unsigned)
  else
    match Type.base member.member_type with
    | Type.Primitive (_, primitive) ->
        Ok
          ( Int64.of_int (Primitive_type.info primitive).byte_size,
            primitive_signedness primitive )
    | Type.Aggregate symbol -> (
        match Int_map.find_opt (symbol_key symbol) previous with
        | Some layout -> Ok (layout.size, Not_applicable)
        | None ->
            Error
              (unresolved member.member_origin Aggregate_dependency
                 (Symbol.name symbol)))

let evaluate_dimensions current_position dimensions =
  let rec loop index total values_rev = function
    | [] -> Ok (total, List.rev values_rev)
    | { dimension_expression; dimension_origin } :: rest ->
        let value =
          match dimension_expression with
          | None when index = 0 -> Ok 0L
          | None ->
              Error
                (invalid_input ~origin:dimension_origin
                   "only the first array dimension can be empty")
          | Some expression ->
              evaluate_expression ~context:Array_dimension ~current_position
                expression
        in
        Result.bind value (fun value ->
            if Int64.compare value 0L < 0 then
              Error (invalid_dimension dimension_origin value)
            else
              Result.bind
                (checked_multiply_nonnegative dimension_origin
                   "the array element count" total value) (fun total ->
                  loop (index + 1) total (value :: values_rev) rest))
  in
  loop 0 1L [] dimensions

type layout_state = {
  size : int64;
  negative_offset : int64;
  members_rev : member_layout list;
}

let update_negative_offset origin state position =
  if Int64.compare position 0L >= 0 then Ok state
  else
    Result.map
      (fun magnitude ->
        {
          state with
          negative_offset = Int64.max state.negative_offset magnitude;
        })
      (checked_negative_magnitude origin position)

let lay_out_member (previous : aggregate_layout Int_map.t) mode union_base state
    member =
  let current_position =
    match mode with
    | Class -> state.size
    | Union -> union_base
  in
  Result.bind (element_shape previous member) (fun (element_size, signedness) ->
      Result.bind
        (evaluate_dimensions current_position member.member_dimensions)
        (fun (total_count, dimensions) ->
          Result.bind
            (checked_multiply_nonnegative member.member_origin
               "the member storage size" element_size total_count) (fun size ->
              let offset = current_position in
              let placed_size =
                match mode with
                | Class ->
                    checked_add member.member_origin "the class size" offset
                      size
                | Union ->
                    Result.map
                      (fun member_end -> Int64.max state.size member_end)
                      (checked_add member.member_origin "the union member end"
                         offset size)
              in
              Result.map
                (fun aggregate_size ->
                  let layout =
                    {
                      symbol = member.member_symbol;
                      path = member.member_path;
                      declarator_index = member.member_declarator_index;
                      origin = member.member_origin;
                      offset;
                      size;
                      element_size;
                      dimensions;
                      signedness;
                      alignment = 1;
                    }
                  in
                  {
                    state with
                    size = aggregate_size;
                    members_rev = layout :: state.members_rev;
                  })
                placed_size)))

let rec lay_out_items (previous : aggregate_layout Int_map.t) mode union_base
    state items =
  match items with
  | [] -> Ok state
  | item :: rest -> (
      match item with
      | Empty_member _ -> lay_out_items previous mode union_base state rest
      | Field member ->
          Result.bind (lay_out_member previous mode union_base state member)
            (fun state -> lay_out_items previous mode union_base state rest)
      | Offset_directive expression ->
          let current_position =
            match mode with
            | Class -> state.size
            | Union -> union_base
          in
          Result.bind
            (evaluate_expression ~context:Aggregate_offset ~current_position
               expression) (fun position ->
              Result.bind
                (update_negative_offset
                   (expression_origin expression)
                   state position)
                (fun state ->
                  match mode with
                  | Class ->
                      lay_out_items previous mode union_base
                        { state with size = position }
                        rest
                  | Union -> lay_out_items previous mode position state rest))
      | Anonymous_union { union_items; _ } ->
          Result.bind
            (lay_out_items previous Union state.size state union_items)
            (fun state -> lay_out_items previous mode union_base state rest))

let validate_member table aggregate_scope seen (member : member_input) =
  let key = symbol_key member.member_symbol in
  if not (Symbol_table.owns_symbol table member.member_symbol) then
    Error
      (invalid_input ~origin:member.member_origin
         "aggregate member belongs to a different symbol table")
  else if
    not (Symbol.equal_kind (Symbol.kind member.member_symbol) Symbol.Member)
  then
    Error
      (invalid_input ~origin:member.member_origin
         "aggregate layout received a symbol that is not a member")
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id member.member_symbol)
         (Symbol_table.scope_id aggregate_scope))
  then
    Error
      (invalid_input ~origin:member.member_origin
         "aggregate member belongs to the wrong aggregate scope")
  else if member.member_path = [] then
    Error
      (invalid_input ~origin:member.member_origin
         "aggregate member path cannot be empty")
  else if List.exists (fun index -> index < 0) member.member_path then
    Error
      (invalid_input ~origin:member.member_origin
         "aggregate member path cannot contain a negative index")
  else if member.member_declarator_index < 0 then
    Error
      (invalid_input ~origin:member.member_origin
         "aggregate member declarator index cannot be negative")
  else if Int_set.mem key seen then
    Error
      (invalid_input ~origin:member.member_origin
         "aggregate layout received the same member symbol twice")
  else
    match Type.base member.member_type with
    | Type.Aggregate symbol when not (Symbol_table.owns_symbol table symbol) ->
        Error
          (invalid_input ~origin:member.member_origin
             "aggregate member type belongs to a different symbol table")
    | Type.Aggregate _ | Type.Primitive _ -> Ok (Int_set.add key seen)

let rec validate_items table scope seen = function
  | [] -> Ok seen
  | Empty_member _ :: rest | Offset_directive _ :: rest ->
      validate_items table scope seen rest
  | Field member :: rest ->
      Result.bind (validate_member table scope seen member) (fun seen ->
          validate_items table scope seen rest)
  | Anonymous_union { union_items; _ } :: rest ->
      Result.bind (validate_items table scope seen union_items) (fun seen ->
          validate_items table scope seen rest)

let validate_aggregate table parent previous_item_index seen_symbols input =
  let key = symbol_key input.aggregate_symbol in
  if not (Symbol_table.owns_symbol table input.aggregate_symbol) then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate belongs to a different symbol table")
  else if
    not
      (Symbol.equal_kind
         (Symbol.kind input.aggregate_symbol)
         Symbol.Aggregate_type)
  then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate layout received a symbol that is not an aggregate type")
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id input.aggregate_symbol)
         (Symbol_table.scope_id parent))
  then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate does not belong to the module scope")
  else if not (Symbol_table.owns_scope table input.aggregate_scope) then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate scope belongs to a different symbol table")
  else if
    Symbol_table.scope_kind input.aggregate_scope <> Symbol_table.Aggregate
  then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate layout needs an aggregate scope")
  else if
    match Symbol_table.parent input.aggregate_scope with
    | Some scope -> not (same_scope scope parent)
    | None -> true
  then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate scope does not belong to the module")
  else if input.aggregate_item_index <= previous_item_index then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate layouts must follow source order")
  else if Int_set.mem key seen_symbols then
    Error
      (invalid_input ~origin:input.aggregate_origin
         "aggregate layout received the same aggregate symbol twice")
  else
    match input.aggregate_base with
    | Some base
      when (not (Symbol_table.owns_symbol table base.base_symbol))
           || not
                (Symbol.equal_kind
                   (Symbol.kind base.base_symbol)
                   Symbol.Aggregate_type) ->
        Error
          (invalid_input ~origin:base.base_origin
             "aggregate base is not an aggregate type from this symbol table")
    | None | Some _ ->
        Result.map
          (fun _ -> (input.aggregate_item_index, Int_set.add key seen_symbols))
          (validate_items table input.aggregate_scope Int_set.empty
             input.aggregate_items)

let validate_inputs table parent inputs =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input "aggregate layout module belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "aggregate layout parent must be a module scope")
  else
    let rec loop previous_item_index seen_symbols = function
      | [] -> Ok ()
      | input :: rest ->
          Result.bind
            (validate_aggregate table parent previous_item_index seen_symbols
               input) (fun (item_index, seen_symbols) ->
              loop item_index seen_symbols rest)
    in
    loop (-1) Int_set.empty inputs

let base_layout (previous : aggregate_layout Int_map.t)
    (input : aggregate_input) =
  match input.aggregate_base with
  | None -> Ok None
  | Some base -> (
      match Int_map.find_opt (symbol_key base.base_symbol) previous with
      | None ->
          Error
            (unresolved base.base_origin Aggregate_dependency
               (Symbol.name base.base_symbol))
      | Some layout ->
          Ok
            (Some
               {
                 symbol = base.base_symbol;
                 origin = base.base_origin;
                 offset = 0L;
                 size = layout.size;
               }))

let lay_out_aggregate (previous : aggregate_layout Int_map.t)
    (input : aggregate_input) =
  Result.bind (base_layout previous input) (fun base ->
      let initial_size =
        match base with
        | None -> 0L
        | Some base -> base.size
      in
      let state =
        { size = initial_size; negative_offset = 0L; members_rev = [] }
      in
      let union_base = 0L in
      Result.bind
        (lay_out_items previous input.aggregate_kind union_base state
           input.aggregate_items) (fun state ->
          Result.map
            (fun size ->
              {
                symbol = input.aggregate_symbol;
                kind = input.aggregate_kind;
                item_index = input.aggregate_item_index;
                origin = input.aggregate_origin;
                size;
                alignment = 1;
                negative_offset = state.negative_offset;
                base;
                members = List.rev state.members_rev;
              })
            (checked_add input.aggregate_origin "the final aggregate size"
               state.size state.negative_offset)))

let layout ~table ~parent inputs =
  Result.bind (validate_inputs table parent inputs) (fun () ->
      let rec loop by_symbol layouts_rev = function
        | [] -> Ok { table; layouts = List.rev layouts_rev; by_symbol }
        | input :: rest ->
            Result.bind (lay_out_aggregate by_symbol input) (fun layout ->
                loop
                  (Int_map.add (symbol_key layout.symbol) layout by_symbol)
                  (layout :: layouts_rev) rest)
      in
      loop Int_map.empty [] inputs)

let layouts result = result.layouts
let owns_table result table = result.table == table

let find result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else
    match Int_map.find_opt (symbol_key symbol) result.by_symbol with
    | Some layout when layout.symbol == symbol -> Some layout
    | Some _ | None -> None
