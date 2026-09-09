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

type 'query expression =
  | Selected_query_expression of 'query
  | Integer_expression of { value : int64; origin : Symbol.origin }
  | Unsigned_integer_expression of { value : int64; origin : Symbol.origin }
  | Floating_expression of { value : float; origin : Symbol.origin }
  | Current_position_expression of Symbol.origin
  | Unary_expression of {
      operator : unary_operator;
      operand : 'query expression;
      origin : Symbol.origin;
    }
  | Binary_expression of {
      operator : binary_operator;
      left : 'query expression;
      right : 'query expression;
      origin : Symbol.origin;
    }
  | Dependency_expression of {
      dependency_kind : dependency_kind;
      detail : string;
      origin : Symbol.origin;
    }
  | Unsupported_expression of { description : string; origin : Symbol.origin }

type expression_context = Array_dimension | Aggregate_offset

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

type number = Integer of int64 | Unsigned_integer of int64 | Floating of float

let expression_origin ~query_origin = function
  | Selected_query_expression query -> query_origin query
  | Integer_expression { origin; _ }
  | Unsigned_integer_expression { origin; _ }
  | Floating_expression { origin; _ }
  | Current_position_expression origin
  | Unary_expression { origin; _ }
  | Binary_expression { origin; _ }
  | Dependency_expression { origin; _ }
  | Unsupported_expression { origin; _ } -> origin

let truthy = function
  | Integer value | Unsigned_integer value -> not (Int64.equal value 0L)
  | Floating value -> not (Int64.equal (Int64.bits_of_float value) 0L)

let boolean value = Integer (if value then 1L else 0L)

let as_float = function
  (* OptLib.HC:125-136 converts the signed IC payload when folding a mixed
     floating expression, including an immediate whose source class is U64. *)
  | Integer value | Unsigned_integer value -> Int64.to_float value
  | Floating value -> value

let common left right =
  match (left, right) with
  | Integer left, Integer right -> `Integer (false, left, right)
  | ( (Integer left | Unsigned_integer left),
      (Integer right | Unsigned_integer right) ) -> `Integer (true, left, right)
  | _ -> `Floating (as_float left, as_float right)

let raw_common left right =
  match common left right with
  | `Integer (unsigned, left, right) -> (`Integer unsigned, left, right)
  | `Floating (left, right) ->
      (`Floating, Int64.bits_of_float left, Int64.bits_of_float right)

let from_raw kind bits =
  match kind with
  | `Integer false -> Integer bits
  | `Integer true -> Unsigned_integer bits
  | `Floating -> Floating (Int64.float_of_bits bits)

let shift_count value = Int64.logand value 63L |> Int64.to_int

let evaluate_unary operator value =
  match (operator, value) with
  | Identity, value -> value
  (* OptPass012.HC:180-191 changes internal U64 unary minus to I64.
     Complement also always produces internal I64 (:153-160). *)
  | Negate, (Integer value | Unsigned_integer value) ->
      Integer (Int64.neg value)
  | Negate, Floating value -> Floating (-.value)
  | Logical_not, Integer value -> boolean (Int64.equal value 0L)
  | Logical_not, Unsigned_integer value ->
      Unsigned_integer (if Int64.equal value 0L then 1L else 0L)
  | Logical_not, (Floating _ as value) ->
      Floating (if truthy value then 0.0 else 1.0)
  | Bitwise_not, (Integer value | Unsigned_integer value) ->
      Integer (Int64.lognot value)
  | Bitwise_not, Floating value ->
      Integer (Int64.bits_of_float value |> Int64.lognot)

let compare_numbers operator left right =
  match common left right with
  | `Integer (unsigned, left, right) -> (
      let comparison =
        if unsigned then Int64.unsigned_compare left right
        else Int64.compare left right
      in
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
  | `Integer (unsigned, left, right) ->
      if Int64.equal right 0L then Error (division_by_zero origin)
      else if unsigned then
        Ok
          (Unsigned_integer
             (if remainder then Int64.unsigned_rem left right
              else Int64.unsigned_div left right))
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
        | Shift_right ->
            if kind = `Integer true then Int64.shift_right_logical left count
            else Int64.shift_right left count
        | _ -> assert false
      in
      Ok (from_raw kind bits)
  | Multiply -> (
      match common left right with
      | `Integer (unsigned, left, right) ->
          Ok (from_raw (`Integer unsigned) (Int64.mul left right))
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
      | `Integer (unsigned, left, right) ->
          let bits =
            if operator = Add then Int64.add left right
            else Int64.sub left right
          in
          Ok (from_raw (`Integer unsigned) bits)
      | `Floating (left, right) ->
          if operator = Add then Ok (Floating (left +. right))
          else Ok (Floating (left -. right)))
  | Less | Greater | Less_equal | Greater_equal | Equal | Not_equal ->
      Ok (boolean (compare_numbers operator left right))
  | Logical_xor -> Ok (boolean (truthy left <> truthy right))
  | Logical_and | Logical_or ->
      invalid_arg "short-circuit operators are evaluated separately"

let rec evaluate_number ~query_origin ~query_value ~consume current_position
    expression =
  let evaluate_number = evaluate_number ~query_origin ~query_value ~consume in
  Result.bind (consume ()) (fun () ->
      match expression with
      | Selected_query_expression query -> (
          match query_value query with
          | Some value -> Ok (Integer value)
          | None ->
              Error
                (invalid_expression
                   (expression_origin ~query_origin
                      (Selected_query_expression query))
                   "selected query has no checked constant metadata"))
      | Integer_expression { value; _ } -> Ok (Integer value)
      | Unsigned_integer_expression { value; _ } -> Ok (Unsigned_integer value)
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
                  Result.bind (evaluate_number current_position right)
                    (fun right ->
                      evaluate_eager_binary operator origin left right)))

let float_to_i64 origin value =
  if not (Float.is_finite value) then Error (non_finite origin)
  else
    let lower = Int64.to_float Int64.min_int in
    let upper = 9223372036854775808.0 in
    if value < lower || value >= upper then Error (conversion_overflow origin)
    else Ok (Int64.of_float value)

let evaluate_expression ?(consume = fun () -> Ok ()) ~query_origin ~query_value
    ~context ~current_position expression =
  Result.bind
    (evaluate_number ~query_origin ~query_value ~consume current_position
       expression) (function
    | Integer value | Unsigned_integer value -> Ok value
    | Floating value -> (
        match context with
        | Array_dimension ->
            float_to_i64 (expression_origin ~query_origin expression) value
        | Aggregate_offset ->
            if Float.is_finite value then Ok (Int64.bits_of_float value)
            else Error (non_finite (expression_origin ~query_origin expression))
        ))

let rec equal ~equal_query (left : 'query expression)
    (right : 'query expression) =
  let equal = equal ~equal_query in
  match (left, right) with
  | Selected_query_expression left, Selected_query_expression right ->
      equal_query left right
  | Floating_expression left, Floating_expression right ->
      Int64.bits_of_float left.value = Int64.bits_of_float right.value
      && left.origin = right.origin
  | Unary_expression left, Unary_expression right ->
      left.operator = right.operator
      && left.origin = right.origin
      && equal left.operand right.operand
  | Binary_expression left, Binary_expression right ->
      left.operator = right.operator
      && left.origin = right.origin && equal left.left right.left
      && equal left.right right.right
  | _ -> left = right

let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let unary = function
  | Frontend.Ast.Unary_plus -> Some Identity
  | Frontend.Ast.Unary_minus -> Some Negate
  | Frontend.Ast.Logical_not -> Some Logical_not
  | Frontend.Ast.Bitwise_not -> Some Bitwise_not
  | Frontend.Ast.Dereference
  | Frontend.Ast.Address_of
  | Frontend.Ast.Pre_increment
  | Frontend.Ast.Pre_decrement -> None

let binary (operator : Frontend.Operator.binary_operator) =
  match operator.ic_name with
  | "IC_POWER" -> Some Power
  | "IC_SHL" -> Some Shift_left
  | "IC_SHR" -> Some Shift_right
  | "IC_MUL" -> Some Multiply
  | "IC_DIV" -> Some Divide
  | "IC_MOD" -> Some Modulo
  | "IC_AND" -> Some Bit_and
  | "IC_XOR" -> Some Bit_xor
  | "IC_OR" -> Some Bit_or
  | "IC_ADD" -> Some Add
  | "IC_SUB" -> Some Subtract
  | "IC_LESS" -> Some Less
  | "IC_GREATER" -> Some Greater
  | "IC_LESS_EQU" -> Some Less_equal
  | "IC_GREATER_EQU" -> Some Greater_equal
  | "IC_EQU_EQU" -> Some Equal
  | "IC_NOT_EQU" -> Some Not_equal
  | "IC_AND_AND" -> Some Logical_and
  | "IC_XOR_XOR" -> Some Logical_xor
  | "IC_OR_OR" -> Some Logical_or
  | _ -> None

let unsupported description location =
  Unsupported_expression { description; origin = origin location }

let dependency dependency_kind detail location =
  Dependency_expression { dependency_kind; detail; origin = origin location }

let is_comparison (operator : Frontend.Operator.binary_operator) =
  match operator.ic_name with
  | "IC_LESS"
  | "IC_GREATER"
  | "IC_LESS_EQU"
  | "IC_GREATER_EQU"
  | "IC_EQU_EQU"
  | "IC_NOT_EQU" -> true
  | _ -> false

let is_ungrouped_comparison = function
  | Frontend.Ast.Binary_expression expression ->
      is_comparison expression.binary_operator_spec
  | _ -> false

let literal_expression ~allow_floating description literal =
  let literal_origin = origin literal.Frontend.Ast.literal_location in
  match literal.literal_value with
  | Frontend.Ast.Integer_value value ->
      (* PrsExp.HC:679-685 selects internal U64 when the payload's high bit is set. *)
      if value < 0L then
        Unsigned_integer_expression { value; origin = literal_origin }
      else Integer_expression { value; origin = literal_origin }
  | Frontend.Ast.Float_value value when allow_floating ->
      Floating_expression { value; origin = literal_origin }
  | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ ->
      unsupported description literal.literal_location

let rec comparison_chain_location = function
  | Frontend.Ast.Binary_expression expression
    when is_comparison expression.binary_operator_spec
         && (is_ungrouped_comparison expression.binary_left
            || is_ungrouped_comparison expression.binary_right) ->
      Some expression.binary_location
  | Frontend.Ast.Binary_expression expression -> (
      match comparison_chain_location expression.binary_left with
      | Some _ as location -> location
      | None -> comparison_chain_location expression.binary_right)
  | Frontend.Ast.Prefix_expression prefix ->
      comparison_chain_location prefix.prefix_operand
  | Frontend.Ast.Parenthesized_expression grouped ->
      comparison_chain_location grouped.grouped_expression
  | _ -> None

let rec convert_ast ~allow_floating ~query_expression ~queries ast =
  let convert = convert_ast ~allow_floating ~query_expression ~queries in
  match ast with
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _
    when List.exists (fun query -> query_expression query == ast) queries ->
      Selected_query_expression
        (List.find (fun query -> query_expression query == ast) queries)
  | Frontend.Ast.Integer_literal literal ->
      literal_expression ~allow_floating "integer literal" literal
  | Frontend.Ast.Character_literal literal ->
      literal_expression ~allow_floating "character literal" literal
  | Frontend.Ast.Float_literal literal ->
      literal_expression ~allow_floating "floating literal" literal
  | Frontend.Ast.String_literal literal ->
      unsupported "string literal" literal.literal_location
  | Frontend.Ast.Identifier_expression identifier ->
      dependency Identifier_dependency
        (Printf.sprintf "`%s`" identifier.spelling)
        identifier.location
  | Frontend.Ast.Current_position_expression operator ->
      Current_position_expression (origin operator.operator_location)
  | Frontend.Ast.Sizeof_expression sizeof ->
      dependency Sizeof_dependency
        (Printf.sprintf "for `%s`" sizeof.sizeof_target.spelling)
        sizeof.sizeof_location
  | Frontend.Ast.Offset_expression offset ->
      let path =
        offset.offset_target.spelling
        :: List.map
             (fun member -> member.Frontend.Ast.offset_member_name.spelling)
             offset.offset_members
        |> String.concat "."
      in
      dependency Offset_dependency
        (Printf.sprintf "for `%s`" path)
        offset.offset_location
  | Frontend.Ast.Defined_expression defined ->
      dependency Defined_dependency
        (Printf.sprintf "for `%s`"
           defined.defined_operand.defined_operand_spelling)
        defined.defined_location
  | Frontend.Ast.Parenthesized_expression grouped ->
      convert grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix -> (
      match unary prefix.prefix_operator_kind with
      | Some operator ->
          Unary_expression
            {
              operator;
              operand = convert prefix.prefix_operand;
              origin = origin prefix.prefix_operator.operator_location;
            }
      | None ->
          unsupported
            (Printf.sprintf "prefix operator `%s`"
               prefix.prefix_operator.operator_spelling)
            prefix.prefix_location)
  | Frontend.Ast.Binary_expression expression -> (
      match binary expression.binary_operator_spec with
      | Some operator ->
          Binary_expression
            {
              operator;
              left = convert expression.binary_left;
              right = convert expression.binary_right;
              origin = origin expression.binary_operator.operator_location;
            }
      | None ->
          unsupported
            (Printf.sprintf "operator `%s`"
               expression.binary_operator.operator_spelling)
            expression.binary_location)
  | Frontend.Ast.Call_expression call ->
      dependency Call_dependency "expression" call.call_location
  | Frontend.Ast.Postfix_expression postfix ->
      unsupported
        (Printf.sprintf "postfix operator `%s`"
           postfix.postfix_operator.operator_spelling)
        postfix.postfix_location
  | Frontend.Ast.Postfix_cast_expression cast ->
      unsupported "postfix cast" cast.cast_location
  | Frontend.Ast.Index_expression index ->
      unsupported "index expression" index.index_location
  | Frontend.Ast.Member_expression member ->
      unsupported "member expression" member.member_location

let of_ast ?(allow_floating = true) ~query_expression ~queries ast =
  (* PrsExp/OptPass012 preserve operands for native comparison chains.
     Reject those before evaluation, including inside short-circuit branches. *)
  match comparison_chain_location ast with
  | Some location -> unsupported "unparenthesized chained comparison" location
  | None -> convert_ast ~allow_floating ~query_expression ~queries ast
