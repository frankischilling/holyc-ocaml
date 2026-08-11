type t = {
  source : Common.Source_file.t;
  contents : string;
  mode : Token.mode;
  mutable offset : int;
  mutable emitted_eof : bool;
}

type item = Token of Token.t | Diagnostic of Common.Diagnostic.t

let create ?(mode = Token.Raw) source =
  {
    source;
    contents = Common.Source_file.contents source;
    mode;
    offset = 0;
    emitted_eof = false;
  }

let source_id lexer = Common.Source_file.id lexer.source
let offset lexer = lexer.offset
let source_length lexer = String.length lexer.contents
let at_end lexer = lexer.offset >= source_length lexer

let peek lexer distance =
  let index = lexer.offset + distance in
  if index < source_length lexer then Some lexer.contents.[index] else None

let advance lexer =
  match peek lexer 0 with
  | None -> None
  | Some byte ->
      lexer.offset <- lexer.offset + 1;
      Some byte

let raw lexer start stop = String.sub lexer.contents start (stop - start)

let span lexer start stop =
  Common.Span.unsafe_make ~source:(source_id lexer) ~start ~stop

let make_diagnostic lexer ?help ~code ~message ~start ~stop () =
  Common.Diagnostic.make ?help ~code ~severity:Common.Diagnostic.Error ~message
    ~primary:(span lexer start stop) ()

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\x1f' -> true
  | _ -> false

let is_ascii_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_identifier_start byte =
  is_ascii_letter byte || Char.equal byte '_' || Char.equal byte '@'
  || Char.code byte >= 128

let is_identifier_continue byte =
  is_identifier_start byte
  ||
  match byte with
  | '0' .. '9' -> true
  | _ -> false

let trivia lexer kind start =
  let stop = lexer.offset in
  { Trivia.kind; raw = raw lexer start stop; span = span lexer start stop }

let rec skip_trivia lexer accumulated =
  match (peek lexer 0, peek lexer 1) with
  | Some byte, _ when is_whitespace byte ->
      let start = lexer.offset in
      while Option.fold ~none:false ~some:is_whitespace (peek lexer 0) do
        ignore (advance lexer)
      done;
      skip_trivia lexer (trivia lexer Whitespace start :: accumulated)
  | Some '/', Some '/' ->
      let start = lexer.offset in
      lexer.offset <- lexer.offset + 2;
      while
        Option.fold ~none:false
          ~some:(fun byte -> not (Char.equal byte '\n'))
          (peek lexer 0)
      do
        ignore (advance lexer)
      done;
      skip_trivia lexer (trivia lexer Line_comment start :: accumulated)
  | Some '/', Some '*' ->
      let start = lexer.offset in
      lexer.offset <- lexer.offset + 2;
      let depth = ref 1 in
      while !depth > 0 && not (at_end lexer) do
        match (peek lexer 0, peek lexer 1) with
        | Some '/', Some '*' ->
            lexer.offset <- lexer.offset + 2;
            incr depth
        | Some '*', Some '/' ->
            lexer.offset <- lexer.offset + 2;
            decr depth
        | _ -> ignore (advance lexer)
      done;
      if !depth = 0 then
        skip_trivia lexer (trivia lexer Block_comment start :: accumulated)
      else
        let diagnostic =
          make_diagnostic lexer ~code:"HCLEX0002"
            ~message:"unterminated block comment" ~start ~stop:lexer.offset ()
        in
        (List.rev accumulated, Some diagnostic)
  | Some '$', Some next when not (Char.equal next '$') ->
      let start = lexer.offset in
      ignore (advance lexer);
      while
        Option.fold ~none:false
          ~some:(fun byte -> not (Char.equal byte '$'))
          (peek lexer 0)
      do
        ignore (advance lexer)
      done;
      if at_end lexer then
        let diagnostic =
          make_diagnostic lexer ~code:"HCLEX0007"
            ~message:"unterminated dollar comment" ~start ~stop:lexer.offset ()
        in
        (List.rev accumulated, Some diagnostic)
      else (
        ignore (advance lexer);
        skip_trivia lexer (trivia lexer Dollar_comment start :: accumulated))
  | _ -> (List.rev accumulated, None)

let make_token lexer leading_trivia ~kind ~value start =
  let stop = lexer.offset in
  {
    Token.kind;
    raw = raw lexer start stop;
    value;
    span = span lexer start stop;
    origin = { frame = source_id lexer; generated_from = None };
    leading_trivia;
    mode = lexer.mode;
  }

let hex_digit byte =
  match Char.uppercase_ascii byte with
  | '0' .. '9' as digit -> Some (Char.code digit - Char.code '0')
  | 'A' .. 'F' as digit -> Some (Char.code digit - Char.code 'A' + 10)
  | _ -> None

let decimal_digit = function
  | '0' .. '9' as digit -> Some (Char.code digit - Char.code '0')
  | _ -> None

let binary_digit = function
  | '0' -> Some 0
  | '1' -> Some 1
  | _ -> None

let consume_digits lexer ~digit ~base initial =
  let value = ref initial in
  let count = ref 0 in
  while
    Option.fold ~none:false
      ~some:(fun byte -> Option.is_some (digit byte))
      (peek lexer 0)
  do
    let byte = Option.get (advance lexer) in
    let item = Option.get (digit byte) in
    value := Common.Int64_ops.mul_add ~base !value ~digit:item;
    incr count
  done;
  (!value, !count)

let scan_number lexer leading_trivia =
  let start = lexer.offset in
  match peek lexer 0 with
  | Some '.' ->
      ignore (advance lexer);
      let value, fraction_digits =
        consume_digits lexer ~digit:decimal_digit ~base:10L 0L
      in
      let exponent = ref 0 in
      let negative_exponent = ref false in
      (match peek lexer 0 with
      | Some ('e' | 'E') ->
          ignore (advance lexer);
          (match peek lexer 0 with
          | Some '-' ->
              ignore (advance lexer);
              negative_exponent := true
          | _ -> ());
          let exponent_value, _ =
            consume_digits lexer ~digit:decimal_digit ~base:10L 0L
          in
          exponent := Int64.to_int exponent_value
      | _ -> ());
      let power =
        (if !negative_exponent then - !exponent else !exponent)
        - fraction_digits
      in
      let float_value = Int64.to_float value *. (10. ** float_of_int power) in
      make_token lexer leading_trivia ~kind:Token_kind.Float
        ~value:(Token.Float64 float_value) start
  | Some first -> (
      ignore (advance lexer);
      let initial = Int64.of_int (Option.get (decimal_digit first)) in
      let prefixed =
        match peek lexer 0 with
        | Some ('x' | 'X') -> Some (hex_digit, 16L)
        | Some ('b' | 'B') -> Some (binary_digit, 2L)
        | _ -> None
      in
      match prefixed with
      | Some (digit, base) ->
          ignore (advance lexer);
          let value, _ = consume_digits lexer ~digit ~base initial in
          make_token lexer leading_trivia ~kind:Token_kind.Integer
            ~value:(Token.Int64 value) start
      | None ->
          let integer, _ =
            consume_digits lexer ~digit:decimal_digit ~base:10L initial
          in
          let fraction_digits = ref 0 in
          let is_float = ref false in
          let value = ref integer in
          (match (peek lexer 0, peek lexer 1) with
          | Some '.', Some '.' -> ()
          | Some '.', _ ->
              is_float := true;
              ignore (advance lexer);
              let with_fraction, count =
                consume_digits lexer ~digit:decimal_digit ~base:10L !value
              in
              value := with_fraction;
              fraction_digits := count
          | _ -> ());
          let exponent = ref 0 in
          let negative_exponent = ref false in
          (match peek lexer 0 with
          | Some ('e' | 'E') ->
              is_float := true;
              ignore (advance lexer);
              (match peek lexer 0 with
              | Some '-' ->
                  ignore (advance lexer);
                  negative_exponent := true
              | _ -> ());
              let exponent_value, _ =
                consume_digits lexer ~digit:decimal_digit ~base:10L 0L
              in
              exponent := Int64.to_int exponent_value
          | _ -> ());
          if !is_float then
            let power =
              (if !negative_exponent then - !exponent else !exponent)
              - !fraction_digits
            in
            let float_value =
              Int64.to_float !value *. (10. ** float_of_int power)
            in
            make_token lexer leading_trivia ~kind:Token_kind.Float
              ~value:(Token.Float64 float_value) start
          else
            make_token lexer leading_trivia ~kind:Token_kind.Integer
              ~value:(Token.Int64 !value) start)
  | None -> assert false

let decoded_byte lexer =
  match peek lexer 0 with
  | None -> None
  | Some '\\' -> (
      ignore (advance lexer);
      match peek lexer 0 with
      | None -> Some (Char.code '\\')
      | Some byte -> (
          match byte with
          | '0' ->
              ignore (advance lexer);
              Some 0
          | '\'' | '`' | '"' | '\\' ->
              ignore (advance lexer);
              Some (Char.code byte)
          | 'd' ->
              ignore (advance lexer);
              Some (Char.code '$')
          | 'n' ->
              ignore (advance lexer);
              Some (Char.code '\n')
          | 'r' ->
              ignore (advance lexer);
              Some (Char.code '\r')
          | 't' ->
              ignore (advance lexer);
              Some (Char.code '\t')
          | 'x' | 'X' ->
              ignore (advance lexer);
              let value = ref 0 in
              let count = ref 0 in
              while
                !count < 2
                && Option.fold ~none:false
                     ~some:(fun item -> Option.is_some (hex_digit item))
                     (peek lexer 0)
              do
                let item =
                  Option.get (advance lexer) |> hex_digit |> Option.get
                in
                value := (!value lsl 4) lor item;
                incr count
              done;
              Some !value
          | _ -> Some (Char.code '\\')))
  | Some '$' ->
      ignore (advance lexer);
      (match peek lexer 0 with
      | Some '$' -> ignore (advance lexer)
      | _ -> ());
      Some (Char.code '$')
  | Some byte ->
      ignore (advance lexer);
      Some (Char.code byte)

let scan_string lexer leading_trivia =
  let start = lexer.offset in
  ignore (advance lexer);
  let decoded = Buffer.create 32 in
  let rec loop () =
    match peek lexer 0 with
    | None ->
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0003"
             ~message:"unterminated string literal" ~start ~stop:lexer.offset ())
    | Some '\x00' ->
        ignore (advance lexer);
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0006"
             ~message:"embedded NUL byte in source" ~start:(lexer.offset - 1)
             ~stop:lexer.offset ())
    | Some '"' ->
        ignore (advance lexer);
        Token
          (make_token lexer leading_trivia ~kind:Token_kind.String
             ~value:(Token.Bytes (Buffer.contents decoded))
             start)
    | Some _ ->
        let byte = Option.get (decoded_byte lexer) in
        Buffer.add_char decoded (Char.chr byte);
        loop ()
  in
  loop ()

let recover_character_literal lexer =
  while
    Option.fold ~none:false
      ~some:(fun byte -> not (Char.equal byte '\''))
      (peek lexer 0)
  do
    ignore (advance lexer)
  done;
  match peek lexer 0 with
  | Some '\'' -> ignore (advance lexer)
  | _ -> ()

let scan_character lexer leading_trivia =
  let start = lexer.offset in
  ignore (advance lexer);
  let value = ref 0L in
  let count = ref 0 in
  let rec loop () =
    match peek lexer 0 with
    | None ->
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0004"
             ~message:"unterminated character literal" ~start ~stop:lexer.offset
             ())
    | Some '\x00' ->
        ignore (advance lexer);
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0006"
             ~message:"embedded NUL byte in source" ~start:(lexer.offset - 1)
             ~stop:lexer.offset ())
    | Some '\'' ->
        ignore (advance lexer);
        Token
          (make_token lexer leading_trivia ~kind:Token_kind.Character
             ~value:(Token.Int64 !value) start)
    | Some _ when !count >= 8 ->
        recover_character_literal lexer;
        Diagnostic
          (make_diagnostic lexer ~code:"HCLEX0005"
             ~message:"character literal exceeds eight bytes" ~start
             ~stop:lexer.offset ())
    | Some _ ->
        let byte = Option.get (decoded_byte lexer) in
        value :=
          Int64.logor !value (Int64.shift_left (Int64.of_int byte) (!count * 8));
        incr count;
        loop ()
  in
  loop ()

let scan_identifier lexer leading_trivia =
  let start = lexer.offset in
  ignore (advance lexer);
  while Option.fold ~none:false ~some:is_identifier_continue (peek lexer 0) do
    ignore (advance lexer)
  done;
  let text = raw lexer start lexer.offset in
  match Keyword.find text with
  | Some keyword ->
      make_token lexer leading_trivia ~kind:(Token_kind.Keyword keyword)
        ~value:(Token.Text text) start
  | None ->
      make_token lexer leading_trivia ~kind:Token_kind.Identifier
        ~value:(Token.Text text) start

let punctuation = function
  | '!'
  | '%'
  | '&'
  | '('
  | ')'
  | '*'
  | '+'
  | ','
  | '-'
  | '/'
  | ':'
  | ';'
  | '<'
  | '='
  | '>'
  | '?'
  | '['
  | ']'
  | '^'
  | '{'
  | '|'
  | '}'
  | '~'
  | '`'
  | '#'
  | '.' -> true
  | _ -> false

let scan_operator_or_punctuation lexer leading_trivia =
  let start = lexer.offset in
  match Operator.find_prefix lexer.contents ~offset:lexer.offset with
  | Some (operator, width) ->
      lexer.offset <- lexer.offset + width;
      Token
        (make_token lexer leading_trivia ~kind:(Token_kind.Operator operator)
           ~value:Token.No_value start)
  | None ->
      let byte = Option.get (advance lexer) in
      Token
        (make_token lexer leading_trivia ~kind:(Token_kind.Punctuation byte)
           ~value:Token.No_value start)

let eof_token lexer leading_trivia =
  let start = lexer.offset in
  lexer.emitted_eof <- true;
  make_token lexer leading_trivia ~kind:Token_kind.Eof ~value:Token.No_value
    start

let next lexer =
  if lexer.emitted_eof then Token (eof_token lexer [])
  else
    let leading_trivia, trivia_error = skip_trivia lexer [] in
    match trivia_error with
    | Some diagnostic -> Diagnostic diagnostic
    | None -> (
        match peek lexer 0 with
        | None -> Token (eof_token lexer leading_trivia)
        | Some '\x00' ->
            let start = lexer.offset in
            ignore (advance lexer);
            Diagnostic
              (make_diagnostic lexer ~code:"HCLEX0006"
                 ~message:"embedded NUL byte in source" ~start
                 ~stop:lexer.offset ())
        | Some byte when is_identifier_start byte ->
            Token (scan_identifier lexer leading_trivia)
        | Some '0' .. '9' -> Token (scan_number lexer leading_trivia)
        | Some '.' -> (
            match peek lexer 1 with
            | Some '0' .. '9' -> Token (scan_number lexer leading_trivia)
            | _ -> scan_operator_or_punctuation lexer leading_trivia)
        | Some '"' -> scan_string lexer leading_trivia
        | Some '\'' -> scan_character lexer leading_trivia
        | Some byte when punctuation byte || Char.equal byte '$' ->
            scan_operator_or_punctuation lexer leading_trivia
        | Some byte ->
            let start = lexer.offset in
            ignore (advance lexer);
            Diagnostic
              (make_diagnostic lexer ~code:"HCLEX0001"
                 ~message:
                   (Printf.sprintf "invalid source byte 0x%02x" (Char.code byte))
                 ~help:
                   "Remove the byte or place it inside a string or character \
                    literal."
                 ~start ~stop:lexer.offset ()))

let lex_all ?mode source =
  let lexer = create ?mode source in
  let rec loop tokens diagnostics =
    match next lexer with
    | Token token when token.Token.kind = Token_kind.Eof ->
        let tokens = List.rev (token :: tokens) in
        if diagnostics = [] then Ok tokens else Error (List.rev diagnostics)
    | Token token -> loop (token :: tokens) diagnostics
    | Diagnostic diagnostic -> loop tokens (diagnostic :: diagnostics)
  in
  loop [] []
