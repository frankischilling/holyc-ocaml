type named_constant = { name : string; value : int; source_line : int }
type sequence_kind = Token of named_constant | Block_comment | Line_comment

type dual_sequence = {
  group : int;
  spelling : string;
  kind : sequence_kind;
  source_line : int;
}

type operator_origin =
  | Dual_table of int
  | Shift_assignment
  | Dot_sequence
  | Current_position

type operator = {
  spelling : string;
  token_name : string option;
  token_id : int option;
  origin : operator_origin;
  source_line : int;
}

type association = Unspecified | Left | Right

type binary_operator = {
  spelling : string;
  token_name : string option;
  token_id : int;
  precedence_name : string;
  precedence_value : int;
  association : association;
  ic_name : string;
  ic_id : int;
  source_line : int;
}

type tables = {
  tokens : named_constant list;
  association_flags : named_constant list;
  precedences : named_constant list;
  dual_sequences : dual_sequence list;
  operators : operator list;
  binary_operators : binary_operator list;
}

type error = { line : int option; message : string }

type raw_dual = {
  group : int;
  first : char;
  second : char;
  token_name : string option;
  source_line : int;
}

type token_reference = Character of char | Named of string

type raw_binary = {
  token : token_reference;
  precedence_name : string;
  association_name : string option;
  ic_name : string;
  source_line : int;
}

type cinit_tables = { dual : raw_dual list; binary : raw_binary list }
type cinit_state = Before_dual | In_dual of int | In_binary | After_binary

let error ?line message = Error { line; message }

let error_to_string (problem : error) =
  match problem.line with
  | None -> problem.message
  | Some line -> Printf.sprintf "line %d: %s" line problem.message

let normalize_checkout_line_endings source =
  let length = String.length source in
  let buffer = Buffer.create length in
  let rec copy offset =
    if offset < length then
      if
        Char.equal source.[offset] '\r'
        && offset + 1 < length
        && Char.equal source.[offset + 1] '\n'
      then (
        Buffer.add_char buffer '\n';
        copy (offset + 2))
      else (
        Buffer.add_char buffer source.[offset];
        copy (offset + 1))
  in
  copy 0;
  Buffer.contents buffer

let verify_sha256 ~expected source =
  let actual =
    normalize_checkout_line_endings source
    |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
  in
  if String.equal actual expected then Ok ()
  else
    error
      (Printf.sprintf "source SHA-256 is %s, but the manifest requires %s"
         actual expected)

let starts_with ~prefix text =
  let prefix_length = String.length prefix in
  String.length text >= prefix_length
  && String.sub text 0 prefix_length = prefix

let find_substring ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then None
    else if String.sub text offset needle_length = needle then Some offset
    else search (offset + 1)
  in
  if needle_length = 0 then Some 0 else search 0

let split_once ~needle text =
  match find_substring ~needle text with
  | None -> None
  | Some offset ->
      let left = String.sub text 0 offset in
      let right_offset = offset + String.length needle in
      let right =
        String.sub text right_offset (String.length text - right_offset)
      in
      Some (left, right)

let trim = String.trim

let strip_comment line =
  match find_substring ~needle:"//" line with
  | None -> line
  | Some offset -> String.sub line 0 offset

let words text =
  let length = String.length text in
  let is_space = function
    | ' ' | '\t' -> true
    | _ -> false
  in
  let rec skip offset =
    if offset < length && is_space text.[offset] then skip (offset + 1)
    else offset
  in
  let rec take offset =
    if offset < length && not (is_space text.[offset]) then take (offset + 1)
    else offset
  in
  let rec collect offset found =
    let first = skip offset in
    if first = length then List.rev found
    else
      let last = take first in
      collect last (String.sub text first (last - first) :: found)
  in
  collect 0 []

let without_horizontal_whitespace text =
  String.to_seq text
  |> Seq.filter (function
    | ' ' | '\t' -> false
    | _ -> true)
  |> String.of_seq

let parse_number text = try Some (int_of_string text) with Failure _ -> None

let parse_definitions ~prefix source =
  let source = normalize_checkout_line_endings source in
  let lines = String.split_on_char '\n' source in
  let rec collect line_number found = function
    | [] -> Ok (List.rev found)
    | line :: rest -> (
        let fields = line |> strip_comment |> words in
        match fields with
        | [ "#define"; name; value ] when starts_with ~prefix name -> (
            match parse_number value with
            | Some value ->
                collect (line_number + 1)
                  ({ name; value; source_line = line_number } :: found)
                  rest
            | None ->
                error ~line:line_number
                  (Printf.sprintf "%s has invalid numeric value %S" name value))
        | "#define" :: name :: _ when starts_with ~prefix name ->
            error ~line:line_number
              (Printf.sprintf "%s must have one numeric value" name)
        | _ -> collect (line_number + 1) found rest)
  in
  collect 1 [] lines

let duplicate_by project entries =
  let seen = Hashtbl.create (List.length entries) in
  let rec find = function
    | [] -> None
    | entry :: rest ->
        let key = project entry in
        if Hashtbl.mem seen key then Some entry
        else (
          Hashtbl.add seen key ();
          find rest)
  in
  find entries

let expected_tokens =
  [
    ("TK_EOF", 0x000);
    ("TK_SUPERSCRIPT", 0x001);
    ("TK_SUBSCRIPT", 0x002);
    ("TK_NORMALSCRIPT", 0x003);
    ("TK_IDENT", 0x100);
    ("TK_STR", 0x101);
    ("TK_I64", 0x102);
    ("TK_CHAR_CONST", 0x103);
    ("TK_F64", 0x104);
    ("TK_PLUS_PLUS", 0x105);
    ("TK_MINUS_MINUS", 0x106);
    ("TK_DEREFERENCE", 0x107);
    ("TK_DBL_COLON", 0x108);
    ("TK_SHL", 0x109);
    ("TK_SHR", 0x10A);
    ("TK_EQU_EQU", 0x10B);
    ("TK_NOT_EQU", 0x10C);
    ("TK_LESS_EQU", 0x10D);
    ("TK_GREATER_EQU", 0x10E);
    ("TK_AND_AND", 0x10F);
    ("TK_OR_OR", 0x110);
    ("TK_XOR_XOR", 0x111);
    ("TK_SHL_EQU", 0x112);
    ("TK_SHR_EQU", 0x113);
    ("TK_MUL_EQU", 0x114);
    ("TK_DIV_EQU", 0x115);
    ("TK_AND_EQU", 0x116);
    ("TK_OR_EQU", 0x117);
    ("TK_XOR_EQU", 0x118);
    ("TK_ADD_EQU", 0x119);
    ("TK_SUB_EQU", 0x11A);
    ("TK_IF", 0x11B);
    ("TK_IFDEF", 0x11C);
    ("TK_IFNDEF", 0x11D);
    ("TK_IFAOT", 0x11E);
    ("TK_IFJIT", 0x11F);
    ("TK_ENDIF", 0x120);
    ("TK_ELSE", 0x121);
    ("TK_MOD_EQU", 0x122);
    ("TK_DOT_DOT", 0x123);
    ("TK_ELLIPSIS", 0x124);
    ("TK_INS_BIN", 0x125);
    ("TK_INS_BIN_SIZE", 0x126);
    ("TK_TKS_NUM", 0x127);
  ]

let expected_association_flags =
  [ ("ASSOCF_LEFT", 1); ("ASSOCF_RIGHT", 2); ("ASSOC_MASK", 3) ]

let expected_precedences =
  [
    ("PREC_NULL", 0x00);
    ("PREC_TERM", 0x04);
    ("PREC_UNARY_POST", 0x08);
    ("PREC_UNARY_PRE", 0x0C);
    ("PREC_EXP", 0x10);
    ("PREC_MUL", 0x14);
    ("PREC_AND", 0x18);
    ("PREC_XOR", 0x1C);
    ("PREC_OR", 0x20);
    ("PREC_ADD", 0x24);
    ("PREC_CMP", 0x28);
    ("PREC_CMP2", 0x2C);
    ("PREC_AND_AND", 0x30);
    ("PREC_XOR_XOR", 0x34);
    ("PREC_OR_OR", 0x38);
    ("PREC_ASSIGN", 0x3C);
    ("PREC_MAX", 0x40);
  ]

let validate_constants label expected (actual : named_constant list) =
  match duplicate_by (fun entry -> entry.name) actual with
  | Some entry ->
      error ~line:entry.source_line
        (Printf.sprintf "%s %s appears more than once" label entry.name)
  | None ->
      let rec compare (expected : (string * int) list)
          (actual : named_constant list) =
        match (expected, actual) with
        | [], [] -> Ok ()
        | (name, _) :: _, [] ->
            error (Printf.sprintf "%s table is missing %s" label name)
        | [], entry :: _ ->
            error ~line:entry.source_line
              (Printf.sprintf "%s table contains unexpected %s" label entry.name)
        | (name, value) :: expected_rest, entry :: actual_rest ->
            if not (String.equal name entry.name) then
              error ~line:entry.source_line
                (Printf.sprintf "%s table requires %s here, but found %s" label
                   name entry.name)
            else if value <> entry.value then
              error ~line:entry.source_line
                (Printf.sprintf "%s must have value 0x%X, but found 0x%X" name
                   value entry.value)
            else compare expected_rest actual_rest
      in
      compare expected actual

let named name constants =
  match
    List.find_opt
      (fun (entry : named_constant) -> String.equal entry.name name)
      constants
  with
  | Some entry -> entry
  | None -> invalid_arg (Printf.sprintf "missing validated constant %s" name)

let parse_char_literal ~line text =
  if
    String.length text = 3
    && Char.equal text.[0] '\''
    && Char.equal text.[2] '\''
  then Ok text.[1]
  else
    error ~line (Printf.sprintf "expected a one-byte character, found %S" text)

let parse_dual_assignment group line_number line =
  let compact = without_horizontal_whitespace line in
  match split_once ~needle:"]=" compact with
  | None -> error ~line:line_number "dual-token assignment is missing `]=`"
  | Some (left, right) -> (
      if not (starts_with ~prefix:"d[" left) then
        error ~line:line_number "dual-token assignment must start with `d[`"
      else
        let first_text = String.sub left 2 (String.length left - 2) in
        let right =
          if
            String.length right > 0
            && Char.equal right.[String.length right - 1] ';'
          then String.sub right 0 (String.length right - 1)
          else right
        in
        match parse_char_literal ~line:line_number first_text with
        | Error _ as result -> result
        | Ok first -> (
            match split_once ~needle:"<<16+" right with
            | None -> (
                match parse_char_literal ~line:line_number right with
                | Error _ as result -> result
                | Ok second ->
                    Ok
                      {
                        group;
                        first;
                        second;
                        token_name = None;
                        source_line = line_number;
                      })
            | Some (token_name, second_text) -> (
                match parse_char_literal ~line:line_number second_text with
                | Error _ as result -> result
                | Ok second ->
                    if not (starts_with ~prefix:"TK_" token_name) then
                      error ~line:line_number
                        (Printf.sprintf "unknown dual-token result %S"
                           token_name)
                    else
                      Ok
                        {
                          group;
                          first;
                          second;
                          token_name = Some token_name;
                          source_line = line_number;
                        })))

let parse_token_reference ~line text =
  if starts_with ~prefix:"TK_" text then Ok (Named text)
  else
    match parse_char_literal ~line text with
    | Ok byte -> Ok (Character byte)
    | Error _ ->
        error ~line (Printf.sprintf "unknown binary token reference %S" text)

let parse_binary_assignment line_number line =
  let compact = without_horizontal_whitespace line in
  match split_once ~needle:"]=" compact with
  | None -> error ~line:line_number "binary operator assignment is missing `]=`"
  | Some (left, right) -> (
      if not (starts_with ~prefix:"d[" left) then
        error ~line:line_number
          "binary operator assignment must start with `d[`"
      else
        let token_text = String.sub left 2 (String.length left - 2) in
        let right =
          if
            String.length right > 0
            && Char.equal right.[String.length right - 1] ';'
          then String.sub right 0 (String.length right - 1)
          else right
        in
        match
          ( parse_token_reference ~line:line_number token_text,
            split_once ~needle:"<<16+" right )
        with
        | (Error _ as result), _ -> result
        | Ok _, None ->
            error ~line:line_number
              "binary operator assignment is missing the `<<16+IC_*` encoding"
        | Ok token, Some (precedence_expression, ic_name) -> (
            if not (starts_with ~prefix:"IC_" ic_name) then
              error ~line:line_number
                (Printf.sprintf "unknown binary IC reference %S" ic_name)
            else
              let expression =
                let length = String.length precedence_expression in
                if
                  length >= 2
                  && Char.equal precedence_expression.[0] '('
                  && Char.equal precedence_expression.[length - 1] ')'
                then String.sub precedence_expression 1 (length - 2)
                else precedence_expression
              in
              let pieces = String.split_on_char '+' expression in
              match pieces with
              | [ precedence_name ] ->
                  Ok
                    {
                      token;
                      precedence_name;
                      association_name = None;
                      ic_name;
                      source_line = line_number;
                    }
              | [ precedence_name; association_name ] ->
                  Ok
                    {
                      token;
                      precedence_name;
                      association_name = Some association_name;
                      ic_name;
                      source_line = line_number;
                    }
              | _ ->
                  error ~line:line_number
                    (Printf.sprintf "invalid precedence expression %S"
                       precedence_expression)))

let dual_header group =
  Printf.sprintf "cmp.dual_U16_tokens%d=d=CAlloc(sizeof(U32)*TK_TKS_NUM);" group

let binary_header = "cmp.binary_ops=d=CAlloc(sizeof(U32)*TK_TKS_NUM);"

let parse_cinit source =
  let source = normalize_checkout_line_endings source in
  let lines = String.split_on_char '\n' source in
  let rec read line_number state dual binary = function
    | [] -> (
        match state with
        | After_binary -> Ok { dual = List.rev dual; binary = List.rev binary }
        | _ -> error "CmpFillTables operator sections are incomplete")
    | line :: rest -> (
        let compact =
          line |> strip_comment |> trim |> without_horizontal_whitespace
        in
        let next state dual binary =
          read (line_number + 1) state dual binary rest
        in
        if String.equal compact (dual_header 1) then
          if state = Before_dual then next (In_dual 1) dual binary
          else error ~line:line_number "dual-token table 1 is out of order"
        else if String.equal compact (dual_header 2) then
          if state = In_dual 1 then next (In_dual 2) dual binary
          else error ~line:line_number "dual-token table 2 is out of order"
        else if String.equal compact (dual_header 3) then
          if state = In_dual 2 then next (In_dual 3) dual binary
          else error ~line:line_number "dual-token table 3 is out of order"
        else if String.equal compact binary_header then
          if state = In_dual 3 then next In_binary dual binary
          else error ~line:line_number "binary operator table is out of order"
        else
          match state with
          | Before_dual | After_binary -> next state dual binary
          | In_dual group ->
              if String.equal compact "" then next state dual binary
              else if starts_with ~prefix:"d[" compact then
                match parse_dual_assignment group line_number compact with
                | Error _ as result -> result
                | Ok entry -> next state (entry :: dual) binary
              else
                error ~line:line_number
                  (Printf.sprintf "unexpected statement in dual-token table %d"
                     group)
          | In_binary ->
              if String.equal compact "" then next state dual binary
              else if String.equal compact "}" then
                next After_binary dual binary
              else if starts_with ~prefix:"d[" compact then
                match parse_binary_assignment line_number compact with
                | Error _ as result -> result
                | Ok entry -> next state dual (entry :: binary)
              else
                error ~line:line_number
                  "unexpected statement in the binary operator table")
  in
  read 1 Before_dual [] [] lines

let dual_key (entry : raw_dual) =
  (entry.group, entry.first, entry.second, entry.token_name)

let expected_dual =
  [
    (1, '!', '=', Some "TK_NOT_EQU");
    (1, '&', '&', Some "TK_AND_AND");
    (1, '*', '=', Some "TK_MUL_EQU");
    (1, '+', '+', Some "TK_PLUS_PLUS");
    (1, '-', '>', Some "TK_DEREFERENCE");
    (1, '/', '*', None);
    (1, ':', ':', Some "TK_DBL_COLON");
    (1, '<', '=', Some "TK_LESS_EQU");
    (1, '=', '=', Some "TK_EQU_EQU");
    (1, '>', '=', Some "TK_GREATER_EQU");
    (1, '^', '=', Some "TK_XOR_EQU");
    (1, '|', '|', Some "TK_OR_OR");
    (1, '%', '=', Some "TK_MOD_EQU");
    (2, '&', '=', Some "TK_AND_EQU");
    (2, '+', '=', Some "TK_ADD_EQU");
    (2, '-', '-', Some "TK_MINUS_MINUS");
    (2, '/', '/', None);
    (2, '<', '<', Some "TK_SHL");
    (2, '>', '>', Some "TK_SHR");
    (2, '^', '^', Some "TK_XOR_XOR");
    (2, '|', '=', Some "TK_OR_EQU");
    (3, '-', '=', Some "TK_SUB_EQU");
    (3, '/', '=', Some "TK_DIV_EQU");
  ]

let validate_dual (entries : raw_dual list) =
  let rec compare expected (actual : raw_dual list) =
    match (expected, actual) with
    | [], [] -> Ok ()
    | (group, first, second, _) :: _, [] ->
        error
          (Printf.sprintf "dual-token table is missing group %d spelling %c%c"
             group first second)
    | [], entry :: _ ->
        error ~line:entry.source_line
          (Printf.sprintf "unexpected dual-token spelling %c%c" entry.first
             entry.second)
    | expected_entry :: expected_rest, entry :: actual_rest ->
        if dual_key entry <> expected_entry then
          let group, first, second, _ = expected_entry in
          error ~line:entry.source_line
            (Printf.sprintf
               "dual-token table requires group %d spelling %c%c here, but \
                found group %d spelling %c%c"
               group first second entry.group entry.first entry.second)
        else compare expected_rest actual_rest
  in
  compare expected_dual entries

let token_key = function
  | Character byte -> String.make 1 byte
  | Named name -> name

let expected_binary =
  [
    ("`", "PREC_EXP", Some "ASSOCF_RIGHT", "IC_POWER");
    ("TK_SHL", "PREC_EXP", Some "ASSOCF_LEFT", "IC_SHL");
    ("TK_SHR", "PREC_EXP", Some "ASSOCF_LEFT", "IC_SHR");
    ("*", "PREC_MUL", None, "IC_MUL");
    ("/", "PREC_MUL", Some "ASSOCF_LEFT", "IC_DIV");
    ("%", "PREC_MUL", Some "ASSOCF_LEFT", "IC_MOD");
    ("&", "PREC_AND", None, "IC_AND");
    ("^", "PREC_XOR", None, "IC_XOR");
    ("|", "PREC_OR", None, "IC_OR");
    ("+", "PREC_ADD", None, "IC_ADD");
    ("-", "PREC_ADD", Some "ASSOCF_LEFT", "IC_SUB");
    ("<", "PREC_CMP", None, "IC_LESS");
    (">", "PREC_CMP", None, "IC_GREATER");
    ("TK_LESS_EQU", "PREC_CMP", None, "IC_LESS_EQU");
    ("TK_GREATER_EQU", "PREC_CMP", None, "IC_GREATER_EQU");
    ("TK_EQU_EQU", "PREC_CMP2", None, "IC_EQU_EQU");
    ("TK_NOT_EQU", "PREC_CMP2", None, "IC_NOT_EQU");
    ("TK_AND_AND", "PREC_AND_AND", None, "IC_AND_AND");
    ("TK_XOR_XOR", "PREC_XOR_XOR", None, "IC_XOR_XOR");
    ("TK_OR_OR", "PREC_OR_OR", None, "IC_OR_OR");
    ("=", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_ASSIGN");
    ("TK_SHL_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_SHL_EQU");
    ("TK_SHR_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_SHR_EQU");
    ("TK_MUL_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_MUL_EQU");
    ("TK_DIV_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_DIV_EQU");
    ("TK_MOD_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_MOD_EQU");
    ("TK_AND_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_AND_EQU");
    ("TK_OR_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_OR_EQU");
    ("TK_XOR_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_XOR_EQU");
    ("TK_ADD_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_ADD_EQU");
    ("TK_SUB_EQU", "PREC_ASSIGN", Some "ASSOCF_RIGHT", "IC_SUB_EQU");
  ]

let validate_binary (entries : raw_binary list) =
  let key (entry : raw_binary) =
    ( token_key entry.token,
      entry.precedence_name,
      entry.association_name,
      entry.ic_name )
  in
  let rec compare expected (actual : raw_binary list) =
    match (expected, actual) with
    | [], [] -> Ok ()
    | (token, _, _, _) :: _, [] ->
        error (Printf.sprintf "binary operator table is missing %s" token)
    | [], entry :: _ ->
        error ~line:entry.source_line
          (Printf.sprintf "unexpected binary operator %s"
             (token_key entry.token))
    | expected_entry :: expected_rest, entry :: actual_rest ->
        if key entry <> expected_entry then
          let token, precedence, _, ic_name = expected_entry in
          error ~line:entry.source_line
            (Printf.sprintf
               "binary operator table requires %s with %s and %s here" token
               precedence ic_name)
        else compare expected_rest actual_rest
  in
  compare expected_binary entries

let required_ic_values =
  [
    ("IC_SHL", 0x2B);
    ("IC_SHR", 0x2C);
    ("IC_POWER", 0x2F);
    ("IC_MUL", 0x30);
    ("IC_DIV", 0x31);
    ("IC_MOD", 0x32);
    ("IC_AND", 0x33);
    ("IC_OR", 0x34);
    ("IC_XOR", 0x35);
    ("IC_ADD", 0x36);
    ("IC_SUB", 0x37);
    ("IC_EQU_EQU", 0x3A);
    ("IC_NOT_EQU", 0x3B);
    ("IC_LESS", 0x3C);
    ("IC_GREATER_EQU", 0x3D);
    ("IC_GREATER", 0x3E);
    ("IC_LESS_EQU", 0x3F);
    ("IC_AND_AND", 0x41);
    ("IC_OR_OR", 0x42);
    ("IC_XOR_XOR", 0x43);
    ("IC_ASSIGN", 0x44);
    ("IC_SHL_EQU", 0x47);
    ("IC_SHR_EQU", 0x48);
    ("IC_MUL_EQU", 0x49);
    ("IC_DIV_EQU", 0x4A);
    ("IC_MOD_EQU", 0x4B);
    ("IC_AND_EQU", 0x4C);
    ("IC_OR_EQU", 0x4D);
    ("IC_XOR_EQU", 0x4E);
    ("IC_ADD_EQU", 0x4F);
    ("IC_SUB_EQU", 0x50);
  ]

let validate_required_constants label expected actual =
  let rec check = function
    | [] -> Ok ()
    | (name, value) :: rest -> (
        match
          List.find_opt
            (fun (entry : named_constant) -> String.equal entry.name name)
            actual
        with
        | None -> error (Printf.sprintf "%s is missing %s" label name)
        | Some entry when entry.value <> value ->
            error ~line:entry.source_line
              (Printf.sprintf "%s must have value 0x%X, but found 0x%X" name
                 value entry.value)
        | Some _ -> check rest)
  in
  check expected

let lines_equal_to source expected =
  source |> normalize_checkout_line_endings |> String.split_on_char '\n'
  |> List.mapi (fun index line ->
      (index + 1, without_horizontal_whitespace (trim line)))
  |> List.filter_map (fun (line, text) ->
      if String.equal text expected then Some line else None)

let require_lines source expected count description =
  let lines = lines_equal_to source expected in
  if List.length lines = count then Ok lines
  else
    error
      (Printf.sprintf "%s must appear %d time%s, but found %d" description count
         (if count = 1 then "" else "s")
         (List.length lines))

let parse_lex_specials source =
  match
    require_lines source "cc->token=TK_DOT_DOT;" 1 "TK_DOT_DOT assignment"
  with
  | Error _ as result -> result
  | Ok dot_lines -> (
      match
        require_lines source "cc->token=TK_ELLIPSIS;" 1 "TK_ELLIPSIS assignment"
      with
      | Error _ as result -> result
      | Ok ellipsis_lines -> (
          match
            require_lines source "i=TK_SHL_EQU;" 1 "TK_SHL_EQU assignment"
          with
          | Error _ as result -> result
          | Ok shl_lines -> (
              match
                require_lines source "i=TK_SHR_EQU;" 1 "TK_SHR_EQU assignment"
              with
              | Error _ as result -> result
              | Ok shr_lines -> (
                  match
                    require_lines source "cc->token='$$';" 2
                      "dollar token assignment"
                  with
                  | Error _ as result -> result
                  | Ok dollar_lines ->
                      Ok
                        ( List.hd dot_lines,
                          List.hd ellipsis_lines,
                          List.hd shl_lines,
                          List.hd shr_lines,
                          List.hd dollar_lines )))))

let dual_sequences tokens (entries : raw_dual list) =
  List.map
    (fun (entry : raw_dual) ->
      let spelling =
        String.init 2 (function
          | 0 -> entry.first
          | _ -> entry.second)
      in
      let kind =
        match entry.token_name with
        | Some name -> Token (named name tokens)
        | None when Char.equal entry.second '*' -> Block_comment
        | None -> Line_comment
      in
      { group = entry.group; spelling; kind; source_line = entry.source_line })
    entries

let operator_order =
  [
    "TK_SHL_EQU";
    "TK_SHR_EQU";
    "TK_ELLIPSIS";
    "TK_PLUS_PLUS";
    "TK_MINUS_MINUS";
    "TK_DEREFERENCE";
    "TK_DBL_COLON";
    "TK_SHL";
    "TK_SHR";
    "TK_EQU_EQU";
    "TK_NOT_EQU";
    "TK_LESS_EQU";
    "TK_GREATER_EQU";
    "TK_AND_AND";
    "TK_OR_OR";
    "TK_XOR_XOR";
    "TK_MUL_EQU";
    "TK_DIV_EQU";
    "TK_MOD_EQU";
    "TK_AND_EQU";
    "TK_OR_EQU";
    "TK_XOR_EQU";
    "TK_ADD_EQU";
    "TK_SUB_EQU";
    "TK_DOT_DOT";
    "CURRENT_POSITION";
  ]

let build_operators tokens (dual : dual_sequence list)
    (dot_line, ellipsis_line, shl_line, shr_line, dollar_line) =
  let dual_token name =
    List.find_opt
      (fun (entry : dual_sequence) ->
        match entry.kind with
        | Token token -> String.equal token.name name
        | Block_comment | Line_comment -> false)
      dual
  in
  let special name =
    match name with
    | "TK_SHL_EQU" -> Some ("<<=", Shift_assignment, shl_line)
    | "TK_SHR_EQU" -> Some (">>=", Shift_assignment, shr_line)
    | "TK_DOT_DOT" -> Some ("..", Dot_sequence, dot_line)
    | "TK_ELLIPSIS" -> Some ("...", Dot_sequence, ellipsis_line)
    | "CURRENT_POSITION" -> Some ("$$", Current_position, dollar_line)
    | _ -> None
  in
  List.map
    (fun name ->
      match special name with
      | Some (spelling, origin, source_line)
        when String.equal name "CURRENT_POSITION" ->
          { spelling; token_name = None; token_id = None; origin; source_line }
      | Some (spelling, origin, source_line) ->
          let token = named name tokens in
          {
            spelling;
            token_name = Some token.name;
            token_id = Some token.value;
            origin;
            source_line;
          }
      | None -> (
          match dual_token name with
          | Some entry ->
              let token = named name tokens in
              {
                spelling = entry.spelling;
                token_name = Some token.name;
                token_id = Some token.value;
                origin = Dual_table entry.group;
                source_line = entry.source_line;
              }
          | None ->
              invalid_arg (Printf.sprintf "missing validated operator %s" name)))
    operator_order

let association flags = function
  | None -> Unspecified
  | Some "ASSOCF_LEFT" ->
      ignore (named "ASSOCF_LEFT" flags);
      Left
  | Some "ASSOCF_RIGHT" ->
      ignore (named "ASSOCF_RIGHT" flags);
      Right
  | Some name ->
      invalid_arg (Printf.sprintf "unknown validated association %s" name)

let operator_spelling (operators : operator list) name =
  match
    List.find_opt
      (fun (entry : operator) -> entry.token_name = Some name)
      operators
  with
  | Some entry -> entry.spelling
  | None -> invalid_arg (Printf.sprintf "missing operator spelling for %s" name)

let build_binary tokens precedences flags ic_constants operators
    (entries : raw_binary list) =
  List.map
    (fun (entry : raw_binary) ->
      let spelling, token_name, token_id =
        match entry.token with
        | Character byte -> (String.make 1 byte, None, Char.code byte)
        | Named name ->
            let token = named name tokens in
            (operator_spelling operators name, Some name, token.value)
      in
      let precedence = named entry.precedence_name precedences in
      let ic = named entry.ic_name ic_constants in
      {
        spelling;
        token_name;
        token_id;
        precedence_name = precedence.name;
        precedence_value = precedence.value;
        association = association flags entry.association_name;
        ic_name = ic.name;
        ic_id = ic.value;
        source_line = entry.source_line;
      })
    entries

let parse ~kernel_source ~compiler_source ~cinit_source ~lex_source =
  match parse_definitions ~prefix:"TK_" kernel_source with
  | Error _ as result -> result
  | Ok tokens -> (
      match validate_constants "token" expected_tokens tokens with
      | Error _ as result -> result
      | Ok () -> (
          match parse_definitions ~prefix:"ASSOC" compiler_source with
          | Error _ as result -> result
          | Ok association_flags -> (
              match
                validate_constants "association" expected_association_flags
                  association_flags
              with
              | Error _ as result -> result
              | Ok () -> (
                  match parse_definitions ~prefix:"PREC_" compiler_source with
                  | Error _ as result -> result
                  | Ok precedences -> (
                      match
                        validate_constants "precedence" expected_precedences
                          precedences
                      with
                      | Error _ as result -> result
                      | Ok () -> (
                          match
                            parse_definitions ~prefix:"IC_" compiler_source
                          with
                          | Error _ as result -> result
                          | Ok ic_constants -> (
                              match
                                validate_required_constants "IC table"
                                  required_ic_values ic_constants
                              with
                              | Error _ as result -> result
                              | Ok () -> (
                                  match parse_cinit cinit_source with
                                  | Error _ as result -> result
                                  | Ok cinit -> (
                                      match validate_dual cinit.dual with
                                      | Error _ as result -> result
                                      | Ok () -> (
                                          match
                                            validate_binary cinit.binary
                                          with
                                          | Error _ as result -> result
                                          | Ok () -> (
                                              match
                                                parse_lex_specials lex_source
                                              with
                                              | Error _ as result -> result
                                              | Ok lex_specials ->
                                                  let dual_sequences =
                                                    dual_sequences tokens
                                                      cinit.dual
                                                  in
                                                  let operators =
                                                    build_operators tokens
                                                      dual_sequences
                                                      lex_specials
                                                  in
                                                  let binary_operators =
                                                    build_binary tokens
                                                      precedences
                                                      association_flags
                                                      ic_constants operators
                                                      cinit.binary
                                                  in
                                                  Ok
                                                    {
                                                      tokens;
                                                      association_flags;
                                                      precedences;
                                                      dual_sequences;
                                                      operators;
                                                      binary_operators;
                                                    })))))))))))
