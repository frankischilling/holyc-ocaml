type mode = Use16 | Use32 | Use64

type token =
  | Register of Register.t
  | Size of int
  | Integer of int64
  | Floating of int64
  | String_literal of string
  | Type_name of string
  | Type_definition of { name : string; token_count : int }
  | Expression_open_bracket
  | Expression_close_bracket
  | Symbol of string
  | Import_symbol of string
  | Value_symbol of string
  | Address_symbol of { name : string; absolute : bool option }
  | Variable_symbol of {
      name : string;
      parameters : bool list option;
      variadic : bool;
    }
  | Function_symbol of string
  | Function_call of { name : string; parameters : bool list; variadic : bool }
  | Operator of string
  | Punctuation of char
  | Opaque of string

type error = { token_index : int; message : string }

type immediate = {
  expression : token list;
  value : int64 option;
  symbols : string list;
  absolute : bool;
  absolute_known : bool;
}

type t = {
  mode_ : mode;
  tokens_ : token list;
  size_ : int;
  segment_ : Register.t option;
  register1_ : Register.t option;
  register2_ : Register.t option;
  scale_ : int64;
  indirect_ : bool;
  just_segment_ : bool;
  immediate_ : immediate option;
  immediate_direct_ : bool;
  immediate_size_ : int;
  relative_ : bool;
}

let mode_bits = function
  | Use16 -> 16
  | Use32 -> 32
  | Use64 -> 64

let mode operand = operand.mode_
let tokens operand = operand.tokens_
let size operand = operand.size_
let segment operand = operand.segment_
let register1 operand = operand.register1_
let register2 operand = operand.register2_
let scale operand = operand.scale_
let indirect operand = operand.indirect_
let just_segment operand = operand.just_segment_
let immediate operand = operand.immediate_

let resolve_immediate ~value ~absolute operand =
  match operand.immediate_ with
  | None -> operand
  | Some immediate ->
      let size_ =
        if not operand.immediate_direct_ then operand.size_
        else if
          operand.immediate_size_ <= 1 && (not operand.relative_) && absolute
        then if operand.mode_ = Use16 then 2 else 4
        else operand.immediate_size_
      in
      {
        operand with
        size_;
        immediate_ =
          Some
            {
              immediate with
              value = Some value;
              absolute;
              absolute_known = true;
            };
      }

let resolve_address ~absolute operand =
  match operand.immediate_ with
  | None -> operand
  | Some immediate ->
      let resolved =
        resolve_immediate
          ~value:(Option.value ~default:0L immediate.value)
          ~absolute operand
      in
      {
        resolved with
        immediate_ = Some { immediate with absolute; absolute_known = true };
      }

let unresolved operand =
  match operand.immediate_ with
  | Some { value = None; _ } -> true
  | None | Some { value = Some _; _ } -> false

let immediate_present operand =
  match operand.immediate_ with
  | None -> false
  | Some { value = Some 0L; _ } when Option.is_some operand.register1_ -> false
  | Some _ -> true

let register_number = function
  | None -> -1
  | Some register -> Register.number register

let numeric_value = function
  | Some { value = Some value; _ } -> value
  | _ -> 0L

exception Invalid of error

let invalid token_index message = raise (Invalid { token_index; message })

let token_spelling = function
  | Register register -> Register.spelling register
  | Size size -> Printf.sprintf "size(%d)" size
  | Integer word -> Int64.to_string word
  | Floating bits -> Printf.sprintf "float(0x%Lx)" bits
  | String_literal bytes -> Printf.sprintf "%S" bytes
  | Expression_open_bracket -> "["
  | Expression_close_bracket -> "]"
  | Function_call { name; _ }
  | Variable_symbol { name; _ }
  | Type_definition { name; _ }
  | Address_symbol { name; _ } -> name
  | Symbol name
  | Import_symbol name
  | Value_symbol name
  | Function_symbol name
  | Type_name name
  | Operator name
  | Opaque name -> name
  | Punctuation punctuation -> String.make 1 punctuation

let is punctuation = function
  | Punctuation value -> Char.equal value punctuation
  | Operator value -> String.equal value (String.make 1 punctuation)
  | _ -> false

type expression_type = Scalar_type | Pointer_type | Unresolved_type

type expression_value = {
  word : int64 option;
  unsigned : bool option;
  symbols_ : string list;
  absolute_count : int;
  absolute_known : bool;
  shifts : int;
  lvalue : bool option;
  modifiers : bool;
  callable : bool;
  direct_function : bool;
  modifiers_terminated : bool;
  type_ : expression_type;
  call_shape : (bool list * bool) option;
  parameter_default : bool;
}

let deferred =
  {
    word = None;
    unsigned = None;
    symbols_ = [];
    absolute_count = 0;
    absolute_known = true;
    shifts = 0;
    lvalue = Some false;
    modifiers = false;
    callable = false;
    direct_function = false;
    modifiers_terminated = false;
    type_ = Unresolved_type;
    call_shape = None;
    parameter_default = false;
  }

let known ?(unsigned = false) word =
  {
    deferred with
    word = Some word;
    unsigned = Some unsigned;
    type_ = Scalar_type;
  }

let symbolic ?(absolute_count = 1) name =
  { deferred with symbols_ = [ name ]; absolute_count; lvalue = None }

let joined left right =
  {
    deferred with
    unsigned =
      (match (left.unsigned, right.unsigned) with
      | Some left, Some right -> Some (left || right)
      | _ -> None);
    symbols_ = left.symbols_ @ right.symbols_;
    absolute_count = left.absolute_count + right.absolute_count;
    absolute_known = left.absolute_known && right.absolute_known;
    shifts = left.shifts + right.shifts;
    type_ =
      (if left.type_ = Scalar_type && right.type_ = Scalar_type then Scalar_type
       else Unresolved_type);
  }

let expression ~offset ~complete expression =
  let source = Array.of_list expression in
  let length = Array.length source in
  let cursor = ref 0 in
  let peek () = if !cursor < length then Some source.(!cursor) else None in
  let take () =
    let token = source.(!cursor) in
    incr cursor;
    token
  in
  let fail message = invalid (offset + !cursor) message in
  let expect punctuation =
    match peek () with
    | Some token when is punctuation token -> incr cursor
    | _ ->
        fail (Printf.sprintf "expected %C in assembler expression" punctuation)
  in
  let unary operation value =
    let word =
      match (operation, value.word) with
      | "+", word -> word
      | "-", Some word -> Some (Int64.neg word)
      | "~", Some word -> Some (Int64.lognot word)
      | "!", Some word -> Some (if word = 0L then 1L else 0L)
      | _ -> None
    in
    let unsigned =
      match operation with
      | "-" -> Some false
      | "~" -> None
      | "!" | "+" -> value.unsigned
      | _ -> None
    in
    let lvalue =
      match operation with
      | "*" -> Some true
      | "++" | "--" -> None
      | _ -> Some false
    in
    let type_ =
      match operation with
      | "&" -> Pointer_type
      | "*" -> Unresolved_type
      | _ -> value.type_
    in
    {
      value with
      word;
      unsigned;
      lvalue;
      callable = operation = "*";
      direct_function = false;
      call_shape = None;
      type_;
    }
  in
  let binary (operator : Generated.Operator_tables.binary_operator) left right =
    let result = joined left right in
    let unsupported () = result in
    match (left.word, right.word) with
    | Some left_word, Some right_word -> (
        let make word = { result with word = Some word } in
        match operator.spelling with
        | "+" -> make (Int64.add left_word right_word)
        | "-" -> make (Int64.sub left_word right_word)
        | "*" -> make (Int64.mul left_word right_word)
        | "&" -> make (Int64.logand left_word right_word)
        | "|" -> make (Int64.logor left_word right_word)
        | "^" -> make (Int64.logxor left_word right_word)
        | "/" | "%" -> (
            match result.unsigned with
            | Some unsigned
              when right_word = 0L
                   || (not unsigned) && left_word = Int64.min_int
                      && right_word = -1L -> unsupported ()
            | Some unsigned ->
                let word =
                  match (operator.spelling, unsigned) with
                  | "/", false -> Int64.div left_word right_word
                  | "%", false -> Int64.rem left_word right_word
                  | "/", true -> Int64.unsigned_div left_word right_word
                  | _ -> Int64.unsigned_rem left_word right_word
                in
                make word
            | None -> unsupported ())
        | "<<" | ">>" -> (
            if result.shifts <> 0 then unsupported ()
            else
              let count = Int64.to_int (Int64.logand right_word 63L) in
              match (operator.spelling, result.unsigned) with
              | "<<", _ ->
                  {
                    result with
                    word = Some (Int64.shift_left left_word count);
                    unsigned = None;
                    shifts = 1;
                  }
              | ">>", Some unsigned ->
                  let word =
                    if unsigned then Int64.shift_right_logical left_word count
                    else Int64.shift_right left_word count
                  in
                  { result with word = Some word; unsigned = None; shifts = 1 }
              | _ -> unsupported ())
        | _ -> unsupported ())
    | _ -> result
  in
  let rec parse depth minimum =
    if depth > 256 then fail "assembler expression nesting exceeds 256 levels";
    let left = primary depth in
    tail depth minimum left
  and primary depth =
    if depth > 256 then fail "assembler expression nesting exceeds 256 levels";
    let value =
      match peek () with
      | None -> fail "expected an assembler expression"
      | Some token
        when List.mem (token_spelling token)
               [ "+"; "-"; "~"; "!"; "&"; "*"; "++"; "--" ] ->
          let operation = token_spelling (take ()) in
          unary operation (primary (depth + 1))
      | Some (Integer value) ->
          incr cursor;
          known ~unsigned:(value < 0L) value
      | Some (Floating _) ->
          incr cursor;
          { deferred with type_ = Scalar_type }
      | Some (String_literal _) ->
          while
            match peek () with
            | Some (String_literal _) -> true
            | _ -> false
          do
            incr cursor
          done;
          { deferred with type_ = Pointer_type }
      | Some (Symbol (("sizeof" | "offset" | "defined") as query)) ->
          incr cursor;
          let wrappers = ref 0 in
          while Option.fold ~none:false ~some:(is '(') (peek ()) do
            incr wrappers;
            incr cursor
          done;
          let names = ref [ query ] in
          if query = "defined" then
            match peek () with
            | None -> fail "expected token after defined"
            | Some _ -> names := token_spelling (take ()) :: !names
          else (
            (match peek () with
            | Some
                ( Symbol name
                | Import_symbol name
                | Value_symbol name
                | Function_symbol name
                | Type_name name ) ->
                incr cursor;
                names := name :: !names
            | Some (Function_call { name; _ }) ->
                incr cursor;
                names := name :: !names
            | Some (Variable_symbol { name; _ }) ->
                incr cursor;
                names := name :: !names
            | Some (Address_symbol { name; _ }) ->
                incr cursor;
                names := name :: !names
            | Some (Size width) ->
                incr cursor;
                names := Printf.sprintf "size(%d)" width :: !names
            | _ ->
                fail "expected an identifier in assembler size or offset query");
            let members = ref 0 in
            while Option.fold ~none:false ~some:(is '.') (peek ()) do
              incr cursor;
              incr members;
              match peek () with
              | Some (Symbol name) ->
                  incr cursor;
                  names := name :: !names
              | _ ->
                  fail "expected member name in assembler size or offset query"
            done;
            if query = "offset" && !members = 0 then
              fail "offset requires a member name";
            if query = "sizeof" then
              while Option.fold ~none:false ~some:(is '*') (peek ()) do
                incr cursor
              done);
          for _ = 1 to !wrappers do
            expect ')'
          done;
          {
            deferred with
            unsigned = Some false;
            symbols_ = List.rev !names;
            type_ = Scalar_type;
          }
      | Some (Symbol name) ->
          incr cursor;
          symbolic name
      | Some (Import_symbol name) ->
          incr cursor;
          symbolic ~absolute_count:0 name
      | Some (Value_symbol name) ->
          incr cursor;
          {
            (symbolic ~absolute_count:0 name) with
            lvalue = Some true;
            modifiers = true;
            callable = true;
          }
      | Some (Address_symbol { name; absolute }) ->
          incr cursor;
          {
            (symbolic
               ~absolute_count:(if absolute = Some true then 1 else 0)
               name)
            with
            absolute_known = Option.is_some absolute;
            lvalue = Some false;
            type_ = Pointer_type;
          }
      | Some (Variable_symbol { name; parameters; variadic }) ->
          incr cursor;
          {
            (symbolic ~absolute_count:0 name) with
            lvalue = Some true;
            modifiers = true;
            callable = Option.is_some parameters;
            call_shape =
              Option.map (fun parameters -> (parameters, variadic)) parameters;
          }
      | Some (Function_symbol name) ->
          incr cursor;
          if not (Option.fold ~none:false ~some:(is '(') (peek ())) then
            fail
              "a parenthesis-free function call requires the source parameter \
               shape";
          {
            (symbolic ~absolute_count:0 name) with
            lvalue = Some false;
            modifiers = true;
            callable = true;
            direct_function = true;
          }
      | Some (Function_call { name; parameters; variadic }) ->
          incr cursor;
          let arguments = source_call (depth + 1) ~parameters ~variadic in
          let result =
            List.fold_left joined (symbolic ~absolute_count:0 name) arguments
          in
          {
            result with
            lvalue = Some false;
            modifiers = true;
            callable = false;
            direct_function = false;
          }
      | Some (Type_name name) ->
          incr cursor;
          if not (Option.fold ~none:false ~some:(is '.') (peek ())) then
            fail "a class offset requires a member name";
          let members = ref [ name ] in
          while Option.fold ~none:false ~some:(is '.') (peek ()) do
            incr cursor;
            match peek () with
            | Some (Symbol name) ->
                incr cursor;
                members := name :: !members
            | _ -> fail "expected member name in assembler class offset"
          done;
          { deferred with symbols_ = List.rev !members; type_ = Scalar_type }
      | Some (Operator "$$") ->
          incr cursor;
          symbolic "$$"
      | Some token when is '(' token ->
          incr cursor;
          let value = parse (depth + 1) 0 in
          expect ')';
          {
            value with
            modifiers = true;
            direct_function = false;
            modifiers_terminated = false;
          }
      | Some (Register _) ->
          fail "a register is not an assembler immediate expression"
      | Some (Size _) -> fail "a size prefix requires an operand"
      | Some (Opaque _) ->
          fail "unsupported literal or token in assembler expression"
      | Some _ -> fail "expected an assembler expression term"
    in
    postfix depth value
  and postfix depth value =
    if value.modifiers_terminated then value
    else
      match peek () with
      | Some token when is '.' token || token_spelling token = "->" -> (
          if not value.modifiers then
            fail
              "this assembler expression term does not permit a member modifier";
          if value.type_ = Scalar_type then
            fail "a scalar assembler expression has no members";
          incr cursor;
          match peek () with
          | Some (Symbol name) ->
              incr cursor;
              postfix depth
                {
                  value with
                  word = None;
                  unsigned = None;
                  symbols_ = value.symbols_ @ [ name ];
                  lvalue = None;
                  callable = true;
                  direct_function = false;
                  call_shape = None;
                  type_ = Unresolved_type;
                }
          | _ ->
              fail
                "expected member name after assembler expression member \
                 operator")
      | Some token when is '(' token && Option.is_some value.call_shape ->
          let parameters, variadic = Option.get value.call_shape in
          let arguments = source_call (depth + 1) ~parameters ~variadic in
          let result = List.fold_left joined value arguments in
          postfix depth
            {
              result with
              word = None;
              unsigned = None;
              modifiers = true;
              lvalue = Some false;
              callable = false;
              direct_function = false;
              call_shape = None;
              type_ = Unresolved_type;
            }
      | Some token when is '(' token ->
          incr cursor;
          let arguments = ref [] in
          let cast = ref None in
          (match peek () with
          | Some (Size _ | Type_name _) when not value.direct_function ->
              let target = cast_type (depth + 1) ~parameter:false in
              cast := Some target;
              arguments := [ target ];
              expect ')'
          | _ ->
              if not value.callable then
                fail
                  "this assembler expression term requires a postfix type, not \
                   call arguments";
              let finished = ref false in
              while not !finished do
                (match peek () with
                | Some token when is ',' token -> incr cursor
                | Some token when is ')' token ->
                    incr cursor;
                    finished := true
                | _ -> (
                    arguments := parse (depth + 1) 0 :: !arguments;
                    match peek () with
                    | Some token when is ',' token -> incr cursor
                    | Some token when is ')' token ->
                        incr cursor;
                        finished := true
                    | _ ->
                        fail "expected ',' or ')' in assembler expression call"));
                if !cursor >= length && not !finished then
                  fail "missing ')' in assembler expression call"
              done);
          let was_cast = Option.is_some !cast in
          let callable =
            Option.fold ~none:false ~some:(fun target -> target.callable) !cast
          in
          let type_ =
            Option.fold ~none:Unresolved_type
              ~some:(fun target -> target.type_)
              !cast
          in
          let call_shape =
            Option.bind !cast (fun target -> target.call_shape)
          in
          let result = List.fold_left joined value (List.rev !arguments) in
          postfix depth
            {
              result with
              word = None;
              unsigned = None;
              modifiers = true;
              lvalue = (if was_cast then value.lvalue else Some false);
              callable;
              direct_function = false;
              call_shape;
              type_;
            }
      | Some Expression_open_bracket ->
          if not value.modifiers then
            fail
              "this assembler expression term does not permit an index modifier";
          if value.type_ = Scalar_type then
            fail "a scalar assembler expression is not an array or pointer";
          incr cursor;
          let index = parse (depth + 1) 0 in
          (match peek () with
          | Some Expression_close_bracket -> incr cursor
          | _ -> fail "expected ']' after assembler expression index");
          postfix depth
            {
              (joined value index) with
              word = None;
              unsigned = None;
              modifiers = true;
              lvalue = None;
              callable = true;
            }
      | Some token when List.mem (token_spelling token) [ "++"; "--" ] ->
          if not value.modifiers then
            fail
              "this assembler expression term does not permit an increment \
               modifier";
          incr cursor;
          postfix depth
            {
              value with
              word = None;
              unsigned = None;
              lvalue = None;
              callable = false;
              direct_function = false;
              modifiers_terminated = true;
            }
      | _ -> value
  and tail depth minimum left =
    match peek () with
    | None -> left
    | Some token -> (
        let spelling = token_spelling token in
        match
          List.find_opt
            (fun (candidate : Generated.Operator_tables.binary_operator) ->
              candidate.spelling = spelling)
            Generated.Operator_tables.binary_operators
        with
        | Some operator when 0x100 - operator.precedence_value >= minimum ->
            if
              operator.precedence_name = "PREC_ASSIGN"
              && left.lvalue = Some false
            then fail "assignment requires an assembler expression lvalue";
            incr cursor;
            let binding = 0x100 - operator.precedence_value in
            let right_minimum =
              match operator.association with
              | Generated.Operator_tables.Right -> binding
              | Generated.Operator_tables.Left
              | Generated.Operator_tables.Unspecified -> binding + 1
            in
            let right = parse (depth + 1) right_minimum in
            tail depth minimum (binary operator left right)
        | _ -> left)
  and cast_type depth ~parameter =
    if depth > 256 then fail "assembler type nesting exceeds 256 levels";
    let is_name = function
      | Symbol _
      | Import_symbol _
      | Value_symbol _
      | Address_symbol _
      | Variable_symbol _
      | Function_symbol _
      | Function_call _
      | Type_name _
      | Register _
      | Size _ -> true
      | _ -> false
    in
    let rec qualifiers () =
      match peek () with
      | Some (Symbol "reg") ->
          incr cursor;
          (match peek () with
          | Some (Register register) when Register.kind register = Register.R64
            -> incr cursor
          | _ -> ());
          qualifiers ()
      | Some (Symbol "noreg") ->
          incr cursor;
          qualifiers ()
      | _ -> ()
    in
    if parameter then qualifiers ();
    let result =
      ref
        (match peek () with
        | Some (Type_name name) ->
            incr cursor;
            symbolic ~absolute_count:0 name
        | Some (Size _) ->
            incr cursor;
            deferred
        | _ -> fail "expected a visible type in assembler cast")
    in
    if parameter then qualifiers ();
    let stars () =
      let count = ref 0 in
      while Option.fold ~none:false ~some:(is '*') (peek ()) do
        incr cursor;
        incr count;
        if !count > 4 then
          fail "assembler type exceeds the original four pointer layers"
      done;
      !count
    in
    let has_type_definition = ref false in
    let rec pointer_layers () =
      let count = stars () in
      match peek () with
      | Some (Type_definition { name; token_count }) ->
          if token_count < 1 || token_count > length - !cursor then
            fail "invalid prevalidated assembler type-definition span";
          if not (is '}' source.(!cursor + token_count - 1)) then
            fail "an assembler type-definition span must end at its body close";
          has_type_definition := true;
          cursor := !cursor + token_count;
          result := joined !result (symbolic ~absolute_count:0 name);
          pointer_layers ()
      | _ -> count
    in
    let pointer_count = pointer_layers () in
    let parameter_mode = parameter && not !has_type_definition in
    let named = ref false and callback = ref false in
    let callback_shape = ref None in
    (if Option.fold ~none:false ~some:(is '(') (peek ()) then (
       callback := true;
       incr cursor;
       if stars () = 0 then
         fail "expected '*' in assembler function-pointer type";
       (if parameter then
          match peek () with
          | Some token when is_name token ->
              incr cursor;
              named := true
          | _ -> ());
       expect ')';
       expect '(';
       let signature = cast_parameters (depth + 1) in
       callback_shape := signature.call_shape;
       result := joined !result signature)
     else if parameter then
       match peek () with
       | Some token when is_name token ->
           incr cursor;
           named := true
       | _ -> ());
    let dimensions = ref 0 in
    while Option.fold ~none:false ~some:(is '[') (peek ()) do
      if parameter_mode then
        fail "the original compiler forbids arrays in function parameters";
      incr cursor;
      (if !dimensions = 0 && Option.fold ~none:false ~some:(is ']') (peek ())
       then incr cursor
       else
         let dimension = parse (depth + 1) 0 in
         (match dimension.word with
         | Some value when value < 0L ->
             fail "negative assembler cast array dimension"
         | _ -> ());
         expect ']';
         result := joined !result dimension);
      incr dimensions
    done;
    let parameter_default =
      parameter && Option.fold ~none:false ~some:(is '=') (peek ())
    in
    if parameter_default then (
      if parameter_mode && not (!named || !callback) then
        fail "an anonymous ordinary function parameter cannot have a default";
      incr cursor;
      match peek () with
      | Some (Symbol "lastclass") -> incr cursor
      | _ -> result := joined !result (parse (depth + 1) 0));
    {
      !result with
      modifiers = true;
      callable = !callback;
      call_shape = !callback_shape;
      parameter_default;
      type_ =
        (if pointer_count > 0 || !callback || !dimensions > 0 then Pointer_type
         else if !has_type_definition then Unresolved_type
         else Scalar_type);
    }
  and cast_parameters depth =
    if depth > 256 then
      fail "assembler function type nesting exceeds 256 levels";
    let finish result parameters variadic =
      { result with call_shape = Some (List.rev parameters, variadic) }
    in
    let rec parameters result shapes =
      match peek () with
      | Some token when is ';' token ->
          incr cursor;
          parameters result shapes
      | Some token when is ')' token ->
          incr cursor;
          finish result shapes false
      | _ -> (
          let rec qualifiers () =
            match peek () with
            | Some (Symbol "reg") ->
                incr cursor;
                (match peek () with
                | Some (Register register)
                  when Register.kind register = Register.R64 -> incr cursor
                | _ -> ());
                qualifiers ()
            | Some (Symbol "noreg") ->
                incr cursor;
                qualifiers ()
            | _ -> ()
          in
          qualifiers ();
          match peek () with
          | Some token when token_spelling token = "..." ->
              incr cursor;
              expect ')';
              finish result shapes true
          | _ -> (
              let parameter = cast_type (depth + 1) ~parameter:true in
              let result = joined result parameter in
              let shapes = parameter.parameter_default :: shapes in
              match peek () with
              | Some token when is ',' token || is ';' token ->
                  incr cursor;
                  parameters result shapes
              | Some token when is ')' token ->
                  incr cursor;
                  finish result shapes false
              | _ ->
                  fail
                    "expected a parameter separator or ')' in assembler \
                     function type"))
    in
    parameters deferred []
  and source_call depth ~parameters ~variadic =
    if depth > 256 then
      fail "assembler function call nesting exceeds 256 levels";
    let parenthesized = Option.fold ~none:false ~some:(is '(') (peek ()) in
    if parenthesized then incr cursor;
    let arguments = ref [] and first = ref true in
    let separator () =
      if (not !first) && parenthesized then
        match peek () with
        | Some token when is ',' token -> incr cursor
        | Some token when is ')' token -> ()
        | _ -> fail "expected ',' in assembler function call"
    in
    List.iter
      (fun has_default ->
        separator ();
        let omitted =
          has_default
          && ((not parenthesized)
             || Option.fold ~none:false
                  ~some:(fun token -> is ')' token || is ',' token)
                  (peek ()))
        in
        if not omitted then arguments := parse (depth + 1) 0 :: !arguments;
        first := false)
      parameters;
    if parenthesized then (
      if variadic then
        while not (Option.fold ~none:false ~some:(is ')') (peek ())) do
          if not !first then expect ',';
          arguments := parse (depth + 1) 0 :: !arguments;
          first := false
        done;
      expect ')');
    List.rev !arguments
  in
  let value = parse 0 0 in
  if complete && !cursor <> length then
    fail "unexpected token after assembler expression";
  ( {
      expression = Array.to_list (Array.sub source 0 !cursor);
      value = value.word;
      symbols = value.symbols_;
      absolute = value.absolute_count land 1 <> 0;
      absolute_known = value.absolute_known;
    },
    !cursor )

let parse ~mode ~relative tokens =
  try
    let source = Array.of_list tokens in
    let length = Array.length source in
    let cursor = ref 0 in
    let size = ref 0 and segment = ref None in
    let register1 = ref None and register2 = ref None in
    let scale = ref 1L and indirect = ref false and just_segment = ref false in
    let immediate = ref None in
    let immediate_direct = ref false and immediate_size = ref 0 in
    let at () = if !cursor < length then Some source.(!cursor) else None in
    let fail message = invalid !cursor message in
    let slice first last =
      Array.to_list (Array.sub source first (last - first))
    in
    let add_expression ~direct first last =
      if Option.is_some !immediate then
        invalid first
          "already one immediate or displacement in assembler operand";
      if first = last then
        invalid first "expected an immediate or displacement expression";
      let parsed, consumed =
        expression ~offset:first ~complete:direct (slice first last)
      in
      immediate := Some parsed;
      immediate_direct := direct;
      immediate_size := !size;
      cursor := first + consumed;
      if
        direct && !size <= 1 && (not relative) && parsed.absolute_known
        && parsed.absolute
      then size := if mode = Use16 then 2 else 4
    in
    let rec prefixes () =
      match at () with
      | Some (Size width) ->
          if not (List.mem width [ 1; 2; 4; 8 ]) then
            fail "invalid assembler size prefix";
          size := width;
          incr cursor;
          prefixes ()
      | Some (Register register) when Register.kind register = Register.Segment
        ->
          segment := Some register;
          incr cursor;
          if !cursor < length && is ':' source.(!cursor) then (
            incr cursor;
            prefixes ())
          else just_segment := true
      | _ -> ()
    in
    prefixes ();
    (if not !just_segment then
       match at () with
       | None -> fail "expected an assembler operand"
       | Some (Register register) ->
           register1 := Some register;
           (size :=
              match Register.kind register with
              | Register.R8 -> 1
              | Register.R16 -> 2
              | Register.R32 -> 4
              | Register.R64 | Register.Float_stack | Register.Mm | Register.Xmm
                -> 8
              | Register.Segment -> assert false);
           incr cursor
       | _ ->
           let first = !cursor in
           let nesting = ref 0 in
           while
             !cursor < length && not (!nesting = 0 && is '[' source.(!cursor))
           do
             if is '(' source.(!cursor) then incr nesting
             else if is ')' source.(!cursor) then decr nesting;
             incr cursor
           done;
           if !cursor > first then add_expression ~direct:true first !cursor;
           if !cursor < length then (
             indirect := true;
             incr cursor;
             let done_ = ref false in
             while not !done_ do
               match at () with
               | None -> fail "missing ']' in assembler address"
               | Some token when is ']' token ->
                   incr cursor;
                   done_ := true
               | Some (Register register)
                 when List.mem (Register.kind register)
                        [ Register.R16; Register.R32; Register.R64 ] ->
                   if !register1 = None then
                     if Register.number register land 7 = 4 then (
                       let dummy =
                         match Register.find "RSP" with
                         | Some register -> register
                         | None -> assert false
                       in
                       register1 := Some dummy;
                       register2 := Some register)
                     else register1 := Some register
                   else register2 := Some register;
                   incr cursor
               | Some token when is '*' token -> (
                   incr cursor;
                   match at () with
                   | Some (Integer value) ->
                       scale := value;
                       incr cursor;
                       if Option.is_some !register2 then (
                         let previous = !register1 in
                         register1 := !register2;
                         register2 := previous)
                   | _ -> fail "expected an integer scale factor after '*'")
               | Some token when is '+' token -> incr cursor
               | Some _ ->
                   let first = !cursor in
                   let depth = ref 0 in
                   while
                     !cursor < length
                     && not (!depth = 0 && is ']' source.(!cursor))
                   do
                     if is '(' source.(!cursor) then incr depth
                     else if is ')' source.(!cursor) then decr depth;
                     incr cursor
                   done;
                   add_expression ~direct:false first !cursor
             done));
    if !cursor <> length then fail "unexpected token after assembler operand";
    Ok
      {
        mode_ = mode;
        tokens_ = tokens;
        size_ = !size;
        segment_ = !segment;
        register1_ = !register1;
        register2_ = !register2;
        scale_ = !scale;
        indirect_ = !indirect;
        just_segment_ = !just_segment;
        immediate_ = !immediate;
        immediate_direct_ = !immediate_direct;
        immediate_size_ = !immediate_size;
        relative_ = relative;
      }
  with Invalid error -> Error error

let argument_mask operand =
  let bit number = Int64.shift_left 1L number in
  if operand.just_segment_ then
    match operand.segment_ with
    | Some register ->
        let specific =
          match Register.number register with
          | 0 -> 42
          | 1 -> 45
          | 2 -> 40
          | 3 -> 41
          | 4 -> 43
          | 5 -> 44
          | _ -> assert false
        in
        Int64.logor (bit specific) (bit 39)
    | None -> 0L
  else if
    Option.fold ~none:false
      ~some:(fun register -> Register.kind register = Register.Float_stack)
      operand.register1_
  then
    if register_number operand.register1_ = 0 then Int64.logor (bit 46) (bit 47)
    else bit 47
  else
    let initial =
      match operand.size_ with
      | 0 -> 0x3FF0FFFFFEL
      | 1 -> 0x1110111112L
      | 2 -> 0x2220222224L
      | 4 -> 0x0440444448L
      | 8 -> 0x0880888880L
      | _ -> 0L
    in
    let mask =
      if operand.mode_ = Use64 then Int64.logand initial 0xFF0FFFFFFFL
      else initial
    in
    if Option.is_some operand.register2_ || operand.scale_ <> 1L then
      Int64.logand mask 0x0000FF0000L
    else
      let present = immediate_present operand in
      let mask =
        Int64.logand mask
          (if operand.indirect_ then
             if present then 0x00FFFF0000L else 0x000FFF0000L
           else if present then 0x000F000FFEL
           else 0x3F0FFFF000L)
      in
      let mask =
        if Option.is_some operand.segment_ then Int64.logand mask 0x00FFFF0000L
        else mask
      in
      let register = register_number operand.register1_ in
      let mask =
        if register = -1 then
          if operand.indirect_ then Int64.logand mask 0x00FFFF0000L
          else
            let value = numeric_value operand.immediate_ in
            let filter =
              if value < 0L then
                if value >= -128L then 0x8FEL
                else if value >= -32768L then 0x8EEL
                else if value >= -2147483648L then 0x8CEL
                else 0x88EL
              else if value <= 127L then 0xFFEL
              else if value <= 255L then 0xFEEL
              else if value <= 32767L then 0xEEEL
              else if value <= 65535L then 0xECEL
              else if value <= 2147483647L then 0xCCEL
              else if value <= 4294967295L then 0xC8EL
              else 0x88EL
            in
            Int64.logand mask filter
        else
          let mask = Int64.logand mask 0x3F00FFF000L in
          if operand.indirect_ then mask else Int64.logand mask 0xFFFF0FFFFFL
      in
      let excluded =
        match register with
        | 0 -> 0x3000000000L
        | 1 -> 0x2F00000000L
        | 2 -> 0x1F00000000L
        | _ -> 0x3F00000000L
      in
      Int64.logand mask (Int64.lognot excluded)
