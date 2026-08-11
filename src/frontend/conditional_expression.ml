type integer = { bits : int64; unsigned : bool }
type value = Integer of integer | Floating of float

type problem = {
  code : string;
  message : string;
  primary : Common.Span.t;
  secondary : Common.Diagnostic.related list;
  notes : string list;
  help : string option;
}

type failure =
  | Lexer_diagnostic of Common.Diagnostic.t
  | Problem of { problem : problem; lookahead : Token.t option }

type unary = Complement | Logical_not | Negate | Identity

type expression =
  | Literal of value
  | Unary of unary * Common.Span.t * expression
  | Binary of Operator.binary_operator * Common.Span.t * expression * expression
  | Comparison_chain of
      expression * (Operator.binary_operator * Common.Span.t * expression) list
  | Grouped of expression

type parsed = { expression : expression; lookahead : Token.t }

type parser = {
  opener : Common.Span.t;
  max_nodes : int;
  next : unit -> Lexer.item;
  symbol_defined : Token.t -> bool;
  mutable nodes : int;
  mutable current : Token.t option;
}

exception Abort of failure

let problem ?(secondary = []) ?(notes = []) ?help ~code ~message primary =
  { code; message; primary; secondary; notes; help }

let abort ?lookahead item =
  raise (Abort (Problem { problem = item; lookahead }))

let current parser =
  match parser.current with
  | Some token -> token
  | None -> invalid_arg "conditional expression parser has no current token"

let advance parser =
  match parser.next () with
  | Lexer.Token token -> parser.current <- Some token
  | Lexer.Diagnostic item -> raise (Abort (Lexer_diagnostic item))

let count_node parser token =
  if parser.nodes >= parser.max_nodes then
    abort
      (problem ~code:"HCPP0026"
         ~message:
           (Printf.sprintf "#if expression exceeds the node limit of %d"
              parser.max_nodes)
         ~help:"Raise the expression node limit only for trusted source."
         token.Token.span)
  else parser.nodes <- parser.nodes + 1

let integer bits = Integer { bits; unsigned = Int64.compare bits 0L < 0 }

let boolean value =
  Integer { bits = (if value then 1L else 0L); unsigned = false }

let truthy = function
  | Integer value -> not (Int64.equal value.bits 0L)
  | Floating value -> not (Int64.equal (Int64.bits_of_float value) 0L)

let token_text token =
  match token.Token.value with
  | Token.Text text | Token.Bytes text -> Some text
  | Token.No_value | Token.Int64 _ | Token.Float64 _ -> None

let token_spelling token =
  Option.value (token_text token) ~default:token.Token.raw

let expected_expression parser token =
  let boundary =
    match token.Token.kind with
    | Token_kind.Eof | Token_kind.Punctuation '#' -> true
    | _ -> false
  in
  let primary = if boundary then parser.opener else token.Token.span in
  let secondary =
    if boundary && Common.Span.compare primary token.Token.span <> 0 then
      [
        {
          Common.Diagnostic.span = token.span;
          message = "input continues here";
        };
      ]
    else []
  in
  abort
    ?lookahead:(if boundary then Some token else None)
    (problem ~secondary ~code:"HCPP0021"
       ~message:"expected a constant expression after #if" primary)

let unsupported_term token =
  let spelling = token_spelling token in
  let message =
    match token.Token.kind with
    | Token_kind.Identifier ->
        Printf.sprintf "identifier %S requires compile-time execution in #if"
          spelling
    | Token_kind.Keyword keyword ->
        Printf.sprintf "%s requires semantic or runtime state in #if"
          (Keyword.spelling keyword)
    | Token_kind.String -> "a string value requires runtime storage in #if"
    | Token_kind.Operator Operator.Current_position ->
        "$$ requires the compiler output position in #if"
    | _ -> Printf.sprintf "%S is not a constant #if expression term" spelling
  in
  abort
    (problem ~code:"HCPP0022" ~message
       ~help:
         "Use literals or definition-expanded constants until the compile-time \
          VM is available."
       token.Token.span)

let punctuation token expected =
  match token.Token.kind with
  | Token_kind.Punctuation actual -> Char.equal actual expected
  | _ -> false

let operator_spelling token =
  match token.Token.kind with
  | Token_kind.Punctuation byte -> Some (String.make 1 byte)
  | Token_kind.Operator operator -> Some (Operator.spelling operator)
  | _ -> None

let binary_operator token =
  match operator_spelling token with
  | None -> None
  | Some spelling ->
      List.find_opt
        (fun (entry : Operator.binary_operator) ->
          String.equal entry.spelling spelling)
        Operator.binary_operators

let binding_power (operator : Operator.binary_operator) =
  0x40 - operator.precedence_value

let is_assignment (operator : Operator.binary_operator) =
  String.equal operator.precedence_name "PREC_ASSIGN"

let is_comparison (operator : Operator.binary_operator) =
  match operator.ic_name with
  | "IC_EQU_EQU"
  | "IC_NOT_EQU"
  | "IC_LESS"
  | "IC_GREATER_EQU"
  | "IC_GREATER"
  | "IC_LESS_EQU" -> true
  | _ -> false

let unsupported_continuation token =
  match token.Token.kind with
  | Token_kind.Punctuation ('(' | '[' | '.') -> true
  | Token_kind.Operator
      ( Operator.Increment
      | Operator.Decrement
      | Operator.Arrow
      | Operator.Double_colon ) -> true
  | _ -> false

let rec parse_expression parser minimum_power equal_requires_right =
  let left = parse_prefix parser in
  parse_binary parser minimum_power equal_requires_right left

and parse_prefix parser =
  let token = current parser in
  count_node parser token;
  match (token.Token.kind, token.Token.value) with
  | Token_kind.Integer, Token.Int64 bits
  | Token_kind.Character, Token.Int64 bits ->
      advance parser;
      Literal (integer bits)
  | Token_kind.Float, Token.Float64 value ->
      advance parser;
      Literal (Floating value)
  | Token_kind.Punctuation '(', _ ->
      let opening = token.Token.span in
      advance parser;
      let expression = parse_expression parser 0 false in
      let closing = current parser in
      if not (punctuation closing ')') then
        abort ~lookahead:closing
          (problem
             ~secondary:
               [
                 {
                   Common.Diagnostic.span = opening;
                   message = "this parenthesized expression starts here";
                 };
               ]
             ~code:"HCPP0023"
             ~message:"expected ')' to close the #if expression"
             closing.Token.span);
      advance parser;
      Grouped expression
  | Token_kind.Punctuation '~', _ ->
      advance parser;
      Unary (Complement, token.span, parse_prefix parser)
  | Token_kind.Punctuation '!', _ ->
      advance parser;
      Unary (Logical_not, token.span, parse_prefix parser)
  | Token_kind.Punctuation '-', _ ->
      advance parser;
      Unary (Negate, token.span, parse_prefix parser)
  | Token_kind.Punctuation '+', _ ->
      advance parser;
      Unary (Identity, token.span, parse_prefix parser)
  | Token_kind.Keyword Keyword.Defined, _ -> parse_defined parser token
  | (Token_kind.Eof | Token_kind.Punctuation '#'), _ ->
      expected_expression parser token
  | (Token_kind.Identifier | Token_kind.Keyword _ | Token_kind.String), _ ->
      unsupported_term token
  | Token_kind.Operator Operator.Current_position, _ -> unsupported_term token
  | _ -> expected_expression parser token

and parse_defined parser _keyword =
  advance parser;
  let rec openings spans =
    let token = current parser in
    if punctuation token '(' then (
      count_node parser token;
      advance parser;
      openings (token.Token.span :: spans))
    else spans
  in
  let opening_spans = openings [] in
  let operand = current parser in
  let present =
    match operand.Token.kind with
    | Token_kind.Identifier -> parser.symbol_defined operand
    | _ -> false
  in
  advance parser;
  let rec close = function
    | [] -> ()
    | opening :: rest ->
        let token = current parser in
        if not (punctuation token ')') then
          abort ~lookahead:token
            (problem
               ~secondary:
                 [
                   {
                     Common.Diagnostic.span = opening;
                     message = "this defined operand starts here";
                   };
                 ]
               ~code:"HCPP0023"
               ~message:"expected ')' after the defined operand"
               token.Token.span);
        advance parser;
        close rest
  in
  close opening_spans;
  Literal (boolean present)

and parse_binary parser minimum_power equal_requires_right left =
  let token = current parser in
  match binary_operator token with
  | Some operator when is_assignment operator ->
      abort
        (problem ~code:"HCPP0022"
           ~message:
             (Printf.sprintf
                "assignment operator %S is not available in constant #if \
                 expressions"
                operator.spelling)
           ~help:
             "Use a value expression here; assignments require compile-time \
              execution."
           token.Token.span)
  | Some operator ->
      let power = binding_power operator in
      let binds =
        power > minimum_power
        || power = minimum_power
           && ((not equal_requires_right)
              || operator.association = Operator.Right)
      in
      if not binds then left
      else (
        count_node parser token;
        advance parser;
        let right = parse_expression parser power true in
        let combined =
          if String.equal operator.ic_name "IC_POWER" then
            match left with
            | Unary (Negate, span, operand) ->
                Unary
                  (Negate, span, Binary (operator, token.span, operand, right))
            | _ -> Binary (operator, token.span, left, right)
          else if is_comparison operator then
            match left with
            | Comparison_chain (first, links) ->
                Comparison_chain (first, (operator, token.span, right) :: links)
            | _ -> Comparison_chain (left, [ (operator, token.span, right) ])
          else Binary (operator, token.span, left, right)
        in
        parse_binary parser minimum_power equal_requires_right combined)
  | None ->
      if unsupported_continuation token then unsupported_term token else left

let parse ~opener ~max_nodes ~next ~symbol_defined () =
  let parser =
    { opener; max_nodes; next; symbol_defined; nodes = 0; current = None }
  in
  try
    advance parser;
    let expression = parse_expression parser 0 false in
    Ok { expression; lookahead = current parser }
  with Abort failure -> Error failure

let lookahead parsed = parsed.lookahead
let integer_to_float value = Int64.to_float value.bits

let as_float = function
  | Integer value -> integer_to_float value
  | Floating value -> value

type common =
  | Common_integer of integer * integer
  | Common_float of float * float

let common left right =
  match (left, right) with
  | Floating left, Floating right -> Common_float (left, right)
  | Floating left, Integer right -> Common_float (left, integer_to_float right)
  | Integer left, Floating right -> Common_float (integer_to_float left, right)
  | Integer left, Integer right ->
      let unsigned = left.unsigned || right.unsigned in
      Common_integer ({ left with unsigned }, { right with unsigned })

let raw_common left right =
  match common left right with
  | Common_integer (left, right) ->
      (`Integer left.unsigned, left.bits, right.bits)
  | Common_float (left, right) ->
      (`Float, Int64.bits_of_float left, Int64.bits_of_float right)

let from_raw kind bits =
  match kind with
  | `Integer unsigned -> Integer { bits; unsigned }
  | `Float -> Floating (Int64.float_of_bits bits)

let from_shift_raw kind bits =
  match kind with
  | `Integer _ -> Integer { bits; unsigned = false }
  | `Float -> Floating (Int64.float_of_bits bits)

let comparison_result value = boolean value

let compare_integer operator left right =
  let comparison =
    if left.unsigned then Int64.unsigned_compare left.bits right.bits
    else Int64.compare left.bits right.bits
  in
  match operator with
  | "IC_EQU_EQU" -> Int64.equal left.bits right.bits
  | "IC_NOT_EQU" -> not (Int64.equal left.bits right.bits)
  | "IC_LESS" -> comparison < 0
  | "IC_GREATER_EQU" -> comparison >= 0
  | "IC_GREATER" -> comparison > 0
  | "IC_LESS_EQU" -> comparison <= 0
  | _ -> invalid_arg "expected an integer comparison"

let compare_float operator left right =
  match operator with
  | "IC_EQU_EQU" ->
      Int64.equal (Int64.bits_of_float left) (Int64.bits_of_float right)
  | "IC_NOT_EQU" ->
      not (Int64.equal (Int64.bits_of_float left) (Int64.bits_of_float right))
  | "IC_LESS" -> left < right
  | "IC_GREATER_EQU" -> left >= right
  | "IC_GREATER" -> left > right
  | "IC_LESS_EQU" -> left <= right
  | _ -> invalid_arg "expected a floating comparison"

let compare_values operator left right =
  match common left right with
  | Common_integer (left, right) ->
      compare_integer operator.Operator.ic_name left right
  | Common_float (left, right) ->
      compare_float operator.Operator.ic_name left right

let division_problem span =
  problem ~code:"HCPP0025" ~message:"integer division by zero in #if" span

let division_overflow span =
  problem ~code:"HCPP0027" ~message:"signed integer division overflows in #if"
    span

let evaluate_integer_division ~remainder span left right =
  if Int64.equal right.bits 0L then Error (division_problem span)
  else if
    (not left.unsigned)
    && Int64.equal left.bits Int64.min_int
    && Int64.equal right.bits (-1L)
  then Error (division_overflow span)
  else
    let bits =
      if left.unsigned then
        if remainder then Int64.unsigned_rem left.bits right.bits
        else Int64.unsigned_div left.bits right.bits
      else if remainder then Int64.rem left.bits right.bits
      else Int64.div left.bits right.bits
    in
    Ok (Integer { bits; unsigned = left.unsigned })

let shift_count bits = Int64.logand bits 63L |> Int64.to_int

let evaluate_binary operator span left right =
  match operator.Operator.ic_name with
  | "IC_POWER" -> Ok (Floating (as_float left ** as_float right))
  | "IC_SHL" ->
      let kind, left, right = raw_common left right in
      Ok (from_shift_raw kind (Int64.shift_left left (shift_count right)))
  | "IC_SHR" ->
      let kind, left, right = raw_common left right in
      let count = shift_count right in
      let bits =
        match kind with
        | `Integer true -> Int64.shift_right_logical left count
        | `Integer false | `Float -> Int64.shift_right left count
      in
      Ok (from_shift_raw kind bits)
  | "IC_MUL" -> (
      match common left right with
      | Common_integer (left, right) ->
          Ok
            (Integer
               {
                 bits = Int64.mul left.bits right.bits;
                 unsigned = left.unsigned;
               })
      | Common_float (left, right) -> Ok (Floating (left *. right)))
  | "IC_DIV" -> (
      match common left right with
      | Common_integer (left, right) ->
          evaluate_integer_division ~remainder:false span left right
      | Common_float (left, right) -> Ok (Floating (left /. right)))
  | "IC_MOD" -> (
      match common left right with
      | Common_integer (left, right) ->
          evaluate_integer_division ~remainder:true span left right
      | Common_float (left, right) -> Ok (Floating (mod_float left right)))
  | "IC_AND" | "IC_OR" | "IC_XOR" ->
      let kind, left, right = raw_common left right in
      let bits =
        match operator.ic_name with
        | "IC_AND" -> Int64.logand left right
        | "IC_OR" -> Int64.logor left right
        | "IC_XOR" -> Int64.logxor left right
        | _ -> assert false
      in
      Ok (from_raw kind bits)
  | "IC_ADD" | "IC_SUB" -> (
      match common left right with
      | Common_integer (left, right) ->
          let bits =
            if String.equal operator.ic_name "IC_ADD" then
              Int64.add left.bits right.bits
            else Int64.sub left.bits right.bits
          in
          Ok (Integer { bits; unsigned = left.unsigned })
      | Common_float (left, right) ->
          if String.equal operator.ic_name "IC_ADD" then
            Ok (Floating (left +. right))
          else Ok (Floating (left -. right)))
  | "IC_EQU_EQU"
  | "IC_NOT_EQU"
  | "IC_LESS"
  | "IC_GREATER_EQU"
  | "IC_GREATER"
  | "IC_LESS_EQU" -> Ok (comparison_result (compare_values operator left right))
  | "IC_AND_AND" -> Ok (boolean (truthy left && truthy right))
  | "IC_OR_OR" -> Ok (boolean (truthy left || truthy right))
  | "IC_XOR_XOR" -> Ok (boolean (Bool.equal (truthy left) (not (truthy right))))
  | name ->
      Error
        (problem ~code:"HCPP0022"
           ~message:
             (Printf.sprintf
                "operator %s is not available in constant #if expressions" name)
           span)

let evaluate_unary operator value =
  match (operator, value) with
  | Complement, Integer value ->
      Integer { bits = Int64.lognot value.bits; unsigned = false }
  | Complement, Floating value ->
      Integer
        { bits = Int64.bits_of_float value |> Int64.lognot; unsigned = false }
  | Logical_not, Integer value -> boolean (Int64.equal value.bits 0L)
  | Logical_not, Floating value ->
      Floating
        (Int64.float_of_bits (if truthy (Floating value) then 0L else 1L))
  | Negate, Integer value ->
      Integer { bits = Int64.neg value.bits; unsigned = false }
  | Negate, Floating value -> Floating (-.value)
  | Identity, value -> value

let rec evaluate_expression = function
  | Literal value -> Ok value
  | Grouped expression -> evaluate_expression expression
  | Unary (operator, _, expression) ->
      Result.map (evaluate_unary operator) (evaluate_expression expression)
  | Binary (operator, span, left, right) ->
      Result.bind (evaluate_expression left) (fun left ->
          Result.bind (evaluate_expression right) (fun right ->
              evaluate_binary operator span left right))
  | Comparison_chain (first, links) ->
      Result.bind (evaluate_expression first) (fun first ->
          let rec loop previous result = function
            | [] -> Ok (boolean result)
            | (operator, _, expression) :: rest ->
                Result.bind (evaluate_expression expression) (fun current ->
                    loop current
                      (result && compare_values operator previous current)
                      rest)
          in
          loop first true (List.rev links))

let evaluate parsed = evaluate_expression parsed.expression
