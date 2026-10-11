type token =
  | Integer of int64
  | Floating of float
  | Name of string
  | Type of string
  | Operator of string
  | Punctuation of char

type value = { word : int64; absolute : bool }
type error = { token_index : int; message : string }
type scalar_class = { raw_type : int; internal : bool }
type scalar = { bits : int64; class_ : scalar_class }
type unary = Identity | Negate | Logical_not | Complement
type binary = Generated.Operator_tables.binary_operator

type expression =
  | Scalar of scalar
  | Grouped of expression
  | Unary of unary * int * expression
  | Binary of binary * int * expression * expression
  | Comparison_chain of expression * (binary * int * expression) list
  | Cast of scalar_class * expression

type work =
  | Evaluate of expression
  | Apply_unary of unary * int
  | Apply_binary of binary * int
  | Apply_cast of scalar_class
  | Begin_chain of (binary * int * expression) list
  | Continue_chain of
      binary * int * scalar * bool * (binary * int * expression) list

exception Invalid_expression of error

let fail token_index message =
  raise (Invalid_expression { token_index; message })

let signed = { raw_type = 10; internal = true }
let unsigned = { raw_type = 11; internal = true }
let floating = { raw_type = 14; internal = true }
let scalar class_ bits = { bits; class_ }
let boolean value = scalar signed (if value then 1L else 0L)
let is_floating value = value.class_.raw_type = floating.raw_type
let is_unsigned value = value.class_.raw_type land 1 <> 0
let truthy value = not (Int64.equal value.bits 0L)

(* PrsExp.HC:679-685 classifies a literal whose high bit is set as U64. A
   resolved assembler address instead has RT_PTR, the signed I64 raw type. *)
let integer bits =
  scalar (if Int64.compare bits 0L < 0 then unsigned else signed) bits

let primitive_class = function
  | "Bool" | "I8" | "I8i" -> Some { raw_type = 4; internal = true }
  | "U8" | "U8i" -> Some { raw_type = 5; internal = true }
  | "I16" -> Some { raw_type = 6; internal = false }
  | "I16i" -> Some { raw_type = 6; internal = true }
  | "U16" -> Some { raw_type = 7; internal = false }
  | "U16i" -> Some { raw_type = 7; internal = true }
  | "I32" -> Some { raw_type = 8; internal = false }
  | "I32i" -> Some { raw_type = 8; internal = true }
  | "U32" -> Some { raw_type = 9; internal = false }
  | "U32i" -> Some { raw_type = 9; internal = true }
  | "I64" -> Some { signed with internal = false }
  | "I64i" -> Some signed
  | "U64" -> Some { unsigned with internal = false }
  | "U64i" -> Some unsigned
  | "F64" | "F64i" -> Some floating
  | _ -> None

let token_spelling = function
  | Name name | Type name | Operator name -> name
  | Punctuation byte -> String.make 1 byte
  | Integer _ -> "integer"
  | Floating _ -> "floating literal"

let binary_operator token =
  match token with
  | Operator _ | Punctuation _ ->
      let spelling = token_spelling token in
      List.find_opt
        (fun (entry : binary) -> String.equal entry.spelling spelling)
        Generated.Operator_tables.binary_operators
  | Integer _ | Floating _ | Name _ | Type _ -> None

let is_comparison (operator : binary) =
  match operator.ic_name with
  | "IC_EQU_EQU"
  | "IC_NOT_EQU"
  | "IC_LESS"
  | "IC_GREATER"
  | "IC_LESS_EQU"
  | "IC_GREATER_EQU" -> true
  | _ -> false

let power_expression operator index left right =
  (* PrsExp.HC:133-161 consumes pending unary operators from the inside out.
     The first unary minus encountered defers itself and every outer prefix
     until after POWER. Grouping has already consumed its unary operators. *)
  let rec insert = function
    | Unary (unary, unary_index, operand) -> (
        match insert operand with
        | Some operand -> Some (Unary (unary, unary_index, operand))
        | None when unary = Negate ->
            Some
              (Unary
                 (Negate, unary_index, Binary (operator, index, operand, right)))
        | None -> None)
    | _ -> None
  in
  match insert left with
  | Some expression -> expression
  | None -> Binary (operator, index, left, right)

let as_float value =
  if is_floating value then Int64.float_of_bits value.bits
  else
    (* OptLib.HC:125-136 converts the signed immediate payload even when its
       source class is unsigned. *)
    Int64.to_float value.bits

let common_class left right =
  if left.class_.raw_type > right.class_.raw_type then left.class_
  else right.class_

let common_raw left right =
  let class_ = common_class left right in
  if class_.raw_type = floating.raw_type then
    ( class_,
      Int64.bits_of_float (as_float left),
      Int64.bits_of_float (as_float right) )
  else (class_, left.bits, right.bits)

let floating_result index result =
  if Float.is_finite result then scalar floating (Int64.bits_of_float result)
  else
    fail index
      "non-finite floating assembler arithmetic requires runtime evaluation"

let power_result index left right =
  let base = as_float left and exponent = as_float right in
  let base_bits = Int64.bits_of_float base in
  (* SYS_POW checks raw positive zero before its FPU path, takes the absolute
     base, then restores its sign from the truncated exponent's low bit:
     Kernel/KMathA.HC:461-512. This differs from a host pow on negative bases. *)
  if Int64.equal (Int64.bits_of_float exponent) 0L then
    scalar floating (Int64.bits_of_float 1.0)
  else if Int64.equal base_bits 0L then scalar floating 0L
  else if
    (not (Float.is_finite base && Float.is_finite exponent))
    || Int64.equal base_bits Int64.min_int
  then
    fail index
      "non-finite or negative-zero assembler POWER requires the original \
       floating runtime"
  else
    let negative =
      if base_bits < 0L then (
        let i64_limit = 9_223_372_036_854_775_808.0 in
        if exponent < -.i64_limit || exponent >= i64_limit then
          fail index
            "assembler POWER exponent exceeds the original signed \
             sign-conversion range";
        Int64.logand (Int64.of_float (Float.trunc exponent)) 1L <> 0L)
      else false
    in
    let magnitude = Float.abs base ** exponent in
    floating_result index (if negative then -.magnitude else magnitude)

let compare_values (operator : binary) index left right =
  if is_floating left || is_floating right then
    let left = as_float left and right = as_float right in
    match operator.ic_name with
    | "IC_EQU_EQU" ->
        Int64.equal (Int64.bits_of_float left) (Int64.bits_of_float right)
    | "IC_NOT_EQU" ->
        not (Int64.equal (Int64.bits_of_float left) (Int64.bits_of_float right))
    | _ -> (
        if not (Float.is_finite left && Float.is_finite right) then
          fail index
            "ordered non-finite floating comparisons require runtime evaluation";
        match operator.ic_name with
        | "IC_LESS" -> left < right
        | "IC_GREATER" -> left > right
        | "IC_LESS_EQU" -> left <= right
        | "IC_GREATER_EQU" -> left >= right
        | _ -> fail index "unsupported assembler comparison")
  else
    let order =
      if is_unsigned left || is_unsigned right then
        Int64.unsigned_compare left.bits right.bits
      else Int64.compare left.bits right.bits
    in
    match operator.ic_name with
    | "IC_EQU_EQU" -> Int64.equal left.bits right.bits
    | "IC_NOT_EQU" -> not (Int64.equal left.bits right.bits)
    | "IC_LESS" -> order < 0
    | "IC_GREATER" -> order > 0
    | "IC_LESS_EQU" -> order <= 0
    | "IC_GREATER_EQU" -> order >= 0
    | _ -> fail index "unsupported assembler comparison"

let comparison_middle left right =
  (* IC_PUSH_CMP retains the comparison's computation class for its shared
     operand: OptPass012.HC:141-151,809-822. A preceding unsigned or floating
     operand can therefore affect the next comparison in the same chain. *)
  if is_floating left || is_floating right then
    scalar floating (Int64.bits_of_float (as_float right))
  else
    scalar
      (if is_unsigned left || is_unsigned right then unsigned else signed)
      right.bits

let apply_unary operator _index operand =
  match operator with
  | Identity -> operand
  | Complement ->
      (* IC_COM changes ic_class2 to I64, but preserves ic_class for later
         operations: OptPass012.HC:153-160 and OptLib.HC:103-122. *)
      { operand with bits = Int64.lognot operand.bits }
  | Logical_not ->
      (* IC_NOT tests the raw word, including the sign bit of floating zero.
         It preserves the operand class (OptPass012.HC:163-179, BackB.HC:10-28). *)
      { operand with bits = (if truthy operand then 0L else 1L) }
  | Negate ->
      if is_floating operand then
        scalar floating (Int64.logxor operand.bits Int64.min_int)
      else
        let class_ =
          if operand.class_.internal && is_unsigned operand then
            { operand.class_ with raw_type = operand.class_.raw_type - 1 }
          else operand.class_
        in
        scalar class_ (Int64.neg operand.bits)

let apply_binary (operator : binary) index left right =
  let class_, left_bits, right_bits = common_raw left right in
  let result bits = scalar class_ bits in
  let float_binary operation =
    floating_result index (operation (as_float left) (as_float right))
  in
  match operator.ic_name with
  | "IC_POWER" -> power_result index left right
  | "IC_SHL" ->
      let count = Int64.to_int (Int64.logand right_bits 63L) in
      result (Int64.shift_left left_bits count)
  | "IC_SHR" ->
      let count = Int64.to_int (Int64.logand right_bits 63L) in
      let bits =
        if
          class_.raw_type <> floating.raw_type
          && (is_unsigned left || is_unsigned right)
        then Int64.shift_right_logical left_bits count
        else Int64.shift_right left_bits count
      in
      result bits
  | "IC_AND" -> result (Int64.logand left_bits right_bits)
  | "IC_OR" -> result (Int64.logor left_bits right_bits)
  | "IC_XOR" -> result (Int64.logxor left_bits right_bits)
  | "IC_ADD" ->
      if class_.raw_type = floating.raw_type then float_binary ( +. )
      else result (Int64.add left_bits right_bits)
  | "IC_SUB" ->
      if class_.raw_type = floating.raw_type then float_binary ( -. )
      else result (Int64.sub left_bits right_bits)
  | "IC_MUL" ->
      if class_.raw_type = floating.raw_type then float_binary ( *. )
      else result (Int64.mul left_bits right_bits)
  | "IC_DIV" | "IC_MOD" ->
      let remainder = String.equal operator.ic_name "IC_MOD" in
      if class_.raw_type = floating.raw_type then
        float_binary (if remainder then mod_float else ( /. ))
      else if Int64.equal right_bits 0L then
        fail index "integer division by zero in assembler expression"
      else if is_unsigned left || is_unsigned right then
        result
          (if remainder then Int64.unsigned_rem left_bits right_bits
           else Int64.unsigned_div left_bits right_bits)
      else if
        Int64.equal left_bits Int64.min_int && Int64.equal right_bits (-1L)
      then
        fail index "signed integer division overflows in assembler expression"
      else
        result
          (if remainder then Int64.rem left_bits right_bits
           else Int64.div left_bits right_bits)
  | "IC_AND_AND" ->
      boolean
        ((not (Int64.equal left_bits 0L)) && not (Int64.equal right_bits 0L))
  | "IC_OR_OR" ->
      boolean
        ((not (Int64.equal left_bits 0L)) || not (Int64.equal right_bits 0L))
  | "IC_XOR_XOR" ->
      boolean (Int64.equal left_bits 0L <> Int64.equal right_bits 0L)
  | _ when is_comparison operator ->
      boolean (compare_values operator index left right)
  | _ ->
      fail index
        (Printf.sprintf "operator %S requires assembler source execution"
           operator.spelling)

let evaluate_tree expression =
  let rec run pending values =
    match (pending, values) with
    | [], [ result ] -> result
    | Evaluate (Scalar value) :: rest, _ -> run rest (value :: values)
    | Evaluate (Grouped expression) :: rest, _ ->
        run (Evaluate expression :: rest) values
    | Evaluate (Unary (operator, index, operand)) :: rest, _ ->
        run (Evaluate operand :: Apply_unary (operator, index) :: rest) values
    | Evaluate (Binary (operator, index, left, right)) :: rest, _ ->
        run
          (Evaluate left :: Evaluate right
          :: Apply_binary (operator, index)
          :: rest)
          values
    | Evaluate (Cast (class_, operand)) :: rest, _ ->
        run (Evaluate operand :: Apply_cast class_ :: rest) values
    | Evaluate (Comparison_chain (first, reversed)) :: rest, _ ->
        run (Evaluate first :: Begin_chain (List.rev reversed) :: rest) values
    | Apply_unary (operator, index) :: rest, operand :: remaining ->
        run rest (apply_unary operator index operand :: remaining)
    | Apply_binary (operator, index) :: rest, right :: left :: remaining ->
        run rest (apply_binary operator index left right :: remaining)
    | Apply_cast class_ :: rest, operand :: remaining ->
        (* Immediate casts change the raw class, not the payload, including
           narrow integer casts: OptPass012.HC:87-96 and BackLib.HC:454-505. *)
        run rest ({ operand with class_ } :: remaining)
    | ( Begin_chain ((operator, index, expression) :: links) :: rest,
        previous :: remaining ) ->
        run
          (Evaluate expression
          :: Continue_chain (operator, index, previous, true, links)
          :: rest)
          remaining
    | ( Continue_chain (operator, index, previous, accepted, links) :: rest,
        current :: remaining ) -> (
        let compared = compare_values operator index previous current in
        let accepted = accepted && compared in
        match links with
        | [] -> run rest (boolean accepted :: remaining)
        | (operator, index, expression) :: links ->
            let middle = comparison_middle previous current in
            run
              (Evaluate expression
              :: Continue_chain (operator, index, middle, accepted, links)
              :: rest)
              remaining)
    | _ -> assert false
  in
  run [ Evaluate expression ] []

let evaluate ~position ~resolve tokens =
  let tokens = Array.of_list tokens in
  let length = Array.length tokens in
  let cursor = ref 0 and absolute = ref false in
  let peek () = if !cursor < length then Some tokens.(!cursor) else None in
  let at byte = function
    | Punctuation actual -> Char.equal actual byte
    | Operator spelling -> String.equal spelling (String.make 1 byte)
    | _ -> false
  in
  let expect byte =
    match peek () with
    | Some token when at byte token -> incr cursor
    | _ ->
        fail !cursor (Printf.sprintf "expected %C in assembler expression" byte)
  in
  let check_depth depth =
    if depth > 256 then
      fail !cursor "assembler expression nesting exceeds 256 levels"
  in
  let rec parse depth minimum equal_requires_right =
    check_depth depth;
    let left = prefix depth in
    tail depth minimum equal_requires_right left
  and prefix depth =
    check_depth depth;
    let index = !cursor in
    let expression =
      match peek () with
      | Some (Integer bits) ->
          incr cursor;
          Scalar (integer bits)
      | Some (Floating value) ->
          incr cursor;
          Scalar (scalar floating (Int64.bits_of_float value))
      | Some (Name name) -> (
          incr cursor;
          match resolve name with
          | Some (bits, is_absolute) ->
              absolute := !absolute <> is_absolute;
              Scalar (scalar signed bits)
          | None ->
              fail index (Printf.sprintf "unresolved assembler value %S" name))
      | Some (Operator "$$") ->
          incr cursor;
          absolute := not !absolute;
          Scalar (scalar signed position)
      | Some token when at '(' token ->
          incr cursor;
          let expression = parse (depth + 1) 0 false in
          expect ')';
          Grouped expression
      | Some token when List.mem (token_spelling token) [ "+"; "-"; "!"; "~" ]
        ->
          let unary =
            match token_spelling token with
            | "+" -> Identity
            | "-" -> Negate
            | "!" -> Logical_not
            | _ -> Complement
          in
          incr cursor;
          Unary (unary, index, prefix (depth + 1))
      | Some token when at '*' token || at '&' token ->
          fail index
            "memory and address-of expressions require source execution"
      | Some token ->
          fail index
            (Printf.sprintf "expected assembler scalar, found %S"
               (token_spelling token))
      | None -> fail index "expected an assembler scalar expression"
    in
    postfix expression
  and postfix expression =
    match peek () with
    | Some token when at '(' token -> (
        let index = !cursor in
        incr cursor;
        match peek () with
        | Some (Type name) -> (
            match primitive_class name with
            | Some class_ ->
                incr cursor;
                expect ')';
                postfix (Cast (class_, expression))
            | None ->
                fail !cursor
                  (Printf.sprintf
                     "postfix type %S is not a closed primitive assembler value"
                     name))
        | Some (Name name) ->
            fail !cursor
              (Printf.sprintf
                 "postfix name %S has no original primitive type receipt" name)
        | _ ->
            fail index
              "calls are unavailable in closed assembler expressions; expected \
               a postfix primitive type")
    | Some token
      when List.mem (token_spelling token) [ "["; "."; "->"; "++"; "--" ] ->
        fail !cursor
          "memory, member and update expressions require source execution"
    | _ -> expression
  and tail depth minimum equal_requires_right left =
    match Option.bind (peek ()) binary_operator with
    | Some operator ->
        let power = 0x40 - operator.precedence_value in
        let binds =
          power > minimum
          || power = minimum
             && ((not equal_requires_right)
                || operator.association = Generated.Operator_tables.Right)
        in
        if not binds then left
        else
          let index = !cursor in
          if String.equal operator.precedence_name "PREC_ASSIGN" then
            fail index "assignments are unavailable in closed assembler values";
          incr cursor;
          let right = parse (depth + 1) power true in
          let combined =
            if String.equal operator.ic_name "IC_POWER" then
              power_expression operator index left right
            else if is_comparison operator then
              match left with
              | Comparison_chain (first, links) ->
                  Comparison_chain (first, (operator, index, right) :: links)
              | _ -> Comparison_chain (left, [ (operator, index, right) ])
            else Binary (operator, index, left, right)
          in
          tail depth minimum equal_requires_right combined
    | None -> left
  in
  try
    let expression = parse 0 0 false in
    if !cursor <> length then
      fail !cursor
        (Printf.sprintf "unexpected token %S after assembler expression"
           (token_spelling tokens.(!cursor)));
    let result = evaluate_tree expression in
    Ok { word = result.bits; absolute = !absolute }
  with Invalid_expression error -> Error error
