type kind = Language | Assembly

type entry = {
  kind : kind;
  spelling : string;
  templeos_id : int;
  source_line : int;
}

type register_kind = R8 | R16 | R32 | R64 | Segment | Float_stack | Mm | Xmm

type register = {
  register_kind : register_kind;
  register_type : int;
  spelling : string;
  register_number : int;
  source_line : int;
}

type instruction = {
  entry_index : int;
  opcode_bytes : int list;
  flags : int;
  slash_value : int;
  uasm_slash_value : int;
  opcode_modifier : int;
  argument1 : int;
  argument2 : int;
  size1 : int;
  size2 : int;
  source_line : int;
}

type opcode_alias = { spelling : string; source_line : int }

type opcode = {
  spelling : string;
  instructions : instruction list;
  aliases : opcode_alias list;
  source_line : int;
}

type tables = {
  registers : register list;
  language : entry list;
  assembly : entry list;
  opcodes : opcode list;
}

type error = { line : int option; message : string }

let error ?line message = Error { line; message }

let error_to_string error =
  match error.line with
  | None -> error.message
  | Some line -> Printf.sprintf "line %d: %s" line error.message

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

type token_kind =
  | Identifier of string
  | Integer of int
  | Symbol of char
  | Double_dollar
  | End

type token = { token_kind : token_kind; line : int }

let is_identifier_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_identifier_byte byte =
  is_identifier_start byte
  ||
  match byte with
  | '0' .. '9' -> true
  | _ -> false

let is_hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

let is_decimal_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_symbol = function
  | ',' | ';' | ':' | '+' | '/' | '!' | '&' | '%' | '=' | '`' | '^' | '*' ->
      true
  | _ -> false

let tokenize source =
  let length = String.length source in
  let rec skip_block start_line depth offset line =
    if offset >= length then error ~line:start_line "unterminated block comment"
    else if
      offset + 1 < length
      && Char.equal source.[offset] '/'
      && Char.equal source.[offset + 1] '*'
    then skip_block start_line (depth + 1) (offset + 2) line
    else if
      offset + 1 < length
      && Char.equal source.[offset] '*'
      && Char.equal source.[offset + 1] '/'
    then
      if depth = 1 then Ok (offset + 2, line)
      else skip_block start_line (depth - 1) (offset + 2) line
    else if Char.equal source.[offset] '\n' then
      skip_block start_line depth (offset + 1) (line + 1)
    else skip_block start_line depth (offset + 1) line
  in
  let rec take_while predicate offset =
    if offset < length && predicate source.[offset] then
      take_while predicate (offset + 1)
    else offset
  in
  let rec scan offset line found =
    if offset >= length then Ok (List.rev ({ token_kind = End; line } :: found))
    else
      match source.[offset] with
      | ' ' | '\t' | '\r' | '\x1f' -> scan (offset + 1) line found
      | '\n' -> scan (offset + 1) (line + 1) found
      | '/' when offset + 1 < length && Char.equal source.[offset + 1] '/' ->
          let stop =
            take_while (fun byte -> not (Char.equal byte '\n')) offset
          in
          scan stop line found
      | '/' when offset + 1 < length && Char.equal source.[offset + 1] '*' -> (
          match skip_block line 1 (offset + 2) line with
          | Error _ as problem -> problem
          | Ok (next, next_line) -> scan next next_line found)
      | byte when is_identifier_start byte ->
          let stop = take_while is_identifier_byte (offset + 1) in
          let spelling = String.sub source offset (stop - offset) in
          scan stop line ({ token_kind = Identifier spelling; line } :: found)
      | '0' .. '9' as first -> (
          let is_hexadecimal =
            Char.equal first '0'
            && offset + 1 < length
            && (Char.equal source.[offset + 1] 'x'
               || Char.equal source.[offset + 1] 'X')
          in
          let stop =
            if is_hexadecimal then take_while is_hex_digit (offset + 2)
            else take_while is_decimal_digit (offset + 1)
          in
          if is_hexadecimal && stop = offset + 2 then
            error ~line "hexadecimal integer has no digits"
          else
            let spelling = String.sub source offset (stop - offset) in
            match int_of_string_opt spelling with
            | None -> error ~line (Printf.sprintf "invalid integer %S" spelling)
            | Some value ->
                scan stop line ({ token_kind = Integer value; line } :: found))
      | '$' when offset + 1 < length && Char.equal source.[offset + 1] '$' ->
          scan (offset + 2) line ({ token_kind = Double_dollar; line } :: found)
      | byte when is_symbol byte ->
          scan (offset + 1) line ({ token_kind = Symbol byte; line } :: found)
      | byte ->
          error ~line
            (Printf.sprintf "unexpected byte 0x%02X in opcode data"
               (Char.code byte))
  in
  scan 0 1 []

type cursor = { tokens : token array; mutable offset : int }

let current cursor = cursor.tokens.(cursor.offset)
let advance cursor = cursor.offset <- cursor.offset + 1

let token_description = function
  | Identifier spelling -> Printf.sprintf "identifier %S" spelling
  | Integer value -> Printf.sprintf "integer %d" value
  | Symbol symbol -> Printf.sprintf "%C" symbol
  | Double_dollar -> "$$"
  | End -> "end of file"

let unexpected token expected =
  error ~line:token.line
    (Printf.sprintf "expected %s, but found %s" expected
       (token_description token.token_kind))

let take_identifier cursor expected =
  let token = current cursor in
  match token.token_kind with
  | Identifier spelling ->
      advance cursor;
      Ok (spelling, token.line)
  | _ -> unexpected token expected

let take_integer cursor expected =
  let token = current cursor in
  match token.token_kind with
  | Integer value ->
      advance cursor;
      Ok (value, token.line)
  | _ -> unexpected token expected

let take_symbol cursor expected =
  let token = current cursor in
  match token.token_kind with
  | Symbol actual when Char.equal actual expected ->
      advance cursor;
      Ok ()
  | _ -> unexpected token (Printf.sprintf "%C" expected)

let register_kind = function
  | "R8" -> Some R8
  | "R16" -> Some R16
  | "R32" -> Some R32
  | "R64" -> Some R64
  | "SEG" -> Some Segment
  | "FSTK" -> Some Float_stack
  | "MM" -> Some Mm
  | "XMM" -> Some Xmm
  | _ -> None

let register_type = function
  | R8 -> 1
  | R16 -> 2
  | R32 -> 3
  | R64 -> 4
  | Segment -> 5
  | Float_stack -> 6
  | Mm -> 7
  | Xmm -> 8

let opcode_modifier = function
  | "NO" -> Some 0
  | "CB" -> Some 1
  | "CW" -> Some 2
  | "CD" -> Some 3
  | "CP" -> Some 4
  | "IB" -> Some 5
  | "IW" -> Some 6
  | "ID" -> Some 7
  | _ -> None

let argument_names =
  [|
    "NONE";
    "REL8";
    "REL16";
    "REL32";
    "IMM8";
    "IMM16";
    "IMM32";
    "IMM64";
    "UIMM8";
    "UIMM16";
    "UIMM32";
    "UIMM64";
    "R8";
    "R16";
    "R32";
    "R64";
    "RM8";
    "RM16";
    "RM32";
    "RM64";
    "M8";
    "M16";
    "M32";
    "M64";
    "M1632";
    "M16N32";
    "M16N16";
    "M32N32";
    "MOFFS8";
    "MOFFS16";
    "MOFFS32";
    "MOFFS64";
    "AL";
    "AX";
    "EAX";
    "RAX";
    "CL";
    "DX";
    " ";
    "SREG";
    "SS";
    "DS";
    "ES";
    "FS";
    "GS";
    "CS";
    "ST0";
    "STI";
    "MM";
    "MM32";
    "MM64";
    "XMM";
    "XMM32";
    "XMM64";
    "XMM128";
    "XMM0";
  |]

let argument_id spelling =
  let rec find index =
    if index = Array.length argument_names then None
    else if String.equal argument_names.(index) spelling then Some index
    else find (index + 1)
  in
  find 0

let bit_is_set mask bit = Int64.logand mask (Int64.shift_left 1L bit) <> 0L

let argument_size argument =
  if bit_is_set 0x1110111112L argument then 8
  else if bit_is_set 0x2220222224L argument then 16
  else if bit_is_set 0x0440444448L argument then 32
  else if bit_is_set 0x0880888880L argument then 64
  else 0

let instruction_flag = function
  | '!' -> Some 0x008
  | '&' -> Some 0x010
  | '%' -> Some 0x020
  | '=' -> Some 0x040
  | '`' -> Some 0x080
  | '^' -> Some 0x100
  | '*' -> Some 0x200
  | _ -> None

let parse_slash_target cursor =
  let token = current cursor in
  match token.token_kind with
  | Integer value when value >= 0 && value < 8 ->
      advance cursor;
      Ok value
  | Identifier "R" ->
      advance cursor;
      Ok 8
  | Identifier "I" ->
      advance cursor;
      Ok 9
  | _ -> unexpected token "a slash value from 0 through 7, R, or I"

let parse_argument cursor =
  let token = current cursor in
  match token.token_kind with
  | Identifier spelling -> (
      match argument_id spelling with
      | None ->
          error ~line:token.line
            (Printf.sprintf "unknown ST_ARG_TYPES name %S" spelling)
      | Some argument ->
          advance cursor;
          Ok (Some argument))
  | _ -> Ok None

let parse_instruction cursor entry_index =
  let source_line = (current cursor).line in
  let rec take_bytes found =
    match (current cursor).token_kind with
    | Integer value ->
        let line = (current cursor).line in
        if value < 0 || value > 0xff then
          error ~line
            (Printf.sprintf "opcode byte %d is outside 0 through 255" value)
        else if List.length found = 4 then
          error ~line "an instruction form cannot contain more than four bytes"
        else (
          advance cursor;
          take_bytes (value :: found))
    | _ -> Ok (List.rev found)
  in
  match take_bytes [] with
  | Error _ as problem -> problem
  | Ok opcode_bytes -> (
      let comma =
        match (current cursor).token_kind with
        | Symbol ',' ->
            advance cursor;
            true
        | Symbol ';' when opcode_bytes <> [] -> false
        | _ -> false
      in
      if (not comma) && opcode_bytes = [] then
        unexpected (current cursor) "an opcode byte or comma"
      else if
        (not comma)
        &&
        match (current cursor).token_kind with
        | Symbol ';' -> false
        | _ -> true
      then unexpected (current cursor) "a comma after the opcode bytes"
      else
        let flags = ref 0 in
        let slash_value = ref 11 in
        let modifier = ref 0 in
        let rec take_flags () =
          let token = current cursor in
          match token.token_kind with
          | Identifier spelling -> (
              match opcode_modifier spelling with
              | None -> Ok ()
              | Some value ->
                  modifier := value;
                  advance cursor;
                  take_flags ())
          | Integer 16 ->
              flags := !flags lor 0x001;
              advance cursor;
              take_flags ()
          | Integer 32 ->
              flags := !flags lor 0x002;
              advance cursor;
              take_flags ()
          | Symbol '+' -> (
              flags := !flags lor 0x004;
              advance cursor;
              match parse_slash_target cursor with
              | Error _ as problem -> problem
              | Ok value ->
                  slash_value := value;
                  take_flags ())
          | Symbol '/' -> (
              advance cursor;
              match parse_slash_target cursor with
              | Error _ as problem -> problem
              | Ok value ->
                  slash_value := value;
                  take_flags ())
          | Symbol symbol -> (
              match instruction_flag symbol with
              | None -> Ok ()
              | Some value ->
                  flags := !flags lor value;
                  advance cursor;
                  take_flags ())
          | Double_dollar ->
              flags := !flags lor 0x400;
              advance cursor;
              take_flags ()
          | _ -> Ok ()
        in
        match take_flags () with
        | Error _ as problem -> problem
        | Ok () -> (
            match parse_argument cursor with
            | Error _ as problem -> problem
            | Ok argument1 -> (
                match parse_argument cursor with
                | Error _ as problem -> problem
                | Ok argument2 ->
                    let argument1 = Option.value argument1 ~default:0 in
                    let argument2 = Option.value argument2 ~default:0 in
                    let uasm_slash_value =
                      if !flags land 0x200 <> 0 && !slash_value <> 9 then 10
                      else !slash_value
                    in
                    Ok
                      {
                        entry_index;
                        opcode_bytes;
                        flags = !flags;
                        slash_value = !slash_value;
                        uasm_slash_value;
                        opcode_modifier = !modifier;
                        argument1;
                        argument2;
                        size1 = argument_size argument1;
                        size2 = argument_size argument2;
                        source_line;
                      })))

let parse_register cursor register_kind =
  match take_identifier cursor "a register spelling" with
  | Error _ as problem -> problem
  | Ok (spelling, source_line) -> (
      match take_integer cursor "a register number" with
      | Error _ as problem -> problem
      | Ok (register_number, line) -> (
          if register_number < 0 || register_number > 0xff then
            error ~line
              (Printf.sprintf "register number %d is outside 0 through 255"
                 register_number)
          else
            match take_symbol cursor ';' with
            | Error _ as problem -> problem
            | Ok () ->
                Ok
                  {
                    register_kind;
                    register_type = register_type register_kind;
                    spelling;
                    register_number;
                    source_line;
                  }))

let parse_keyword cursor kind =
  let tag =
    match kind with
    | Language -> "KEYWORD"
    | Assembly -> "ASM_KEYWORD"
  in
  match take_identifier cursor (tag ^ " spelling") with
  | Error _ as problem -> problem
  | Ok (spelling, source_line) -> (
      match take_integer cursor (tag ^ " numeric ID") with
      | Error _ as problem -> problem
      | Ok (templeos_id, _) -> (
          match take_symbol cursor ';' with
          | Error _ as problem -> problem
          | Ok () -> Ok { kind; spelling; templeos_id; source_line }))

let parse_opcode cursor =
  match take_identifier cursor "an opcode spelling" with
  | Error _ as problem -> problem
  | Ok (spelling, source_line) ->
      let rec take_aliases found =
        match (current cursor).token_kind with
        | Identifier alias ->
            let source_line = (current cursor).line in
            advance cursor;
            take_aliases ({ spelling = alias; source_line } :: found)
        | Symbol ';' when found <> [] ->
            advance cursor;
            Ok (List.rev found)
        | Symbol ';' ->
            error ~line:(current cursor).line
              "an opcode alias list cannot be empty"
        | _ -> unexpected (current cursor) "an opcode alias or semicolon"
      in
      let rec take_instructions found =
        match (current cursor).token_kind with
        | Symbol ';' ->
            advance cursor;
            Ok
              {
                spelling;
                instructions = List.rev found;
                aliases = [];
                source_line;
              }
        | Symbol ':' -> (
            advance cursor;
            match take_aliases [] with
            | Error _ as problem -> problem
            | Ok aliases ->
                Ok
                  {
                    spelling;
                    instructions = List.rev found;
                    aliases;
                    source_line;
                  })
        | End ->
            error ~line:(current cursor).line
              (Printf.sprintf "opcode %S has no terminating semicolon" spelling)
        | _ -> (
            if List.length found = 32 then
              error ~line:(current cursor).line
                (Printf.sprintf "opcode %S exceeds the 32-form source limit"
                   spelling)
            else
              match parse_instruction cursor (List.length found) with
              | Error _ as problem -> problem
              | Ok instruction -> take_instructions (instruction :: found))
      in
      take_instructions []

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

let validate_unique label (entries : entry list) =
  match duplicate_by (fun (entry : entry) -> entry.templeos_id) entries with
  | Some entry ->
      error ~line:entry.source_line
        (Printf.sprintf "%s ID %d appears more than once" label
           entry.templeos_id)
  | None -> (
      match duplicate_by (fun (entry : entry) -> entry.spelling) entries with
      | Some entry ->
          error ~line:entry.source_line
            (Printf.sprintf "%s spelling %S appears more than once" label
               entry.spelling)
      | None -> Ok ())

let validate_range label first last (entries : entry list) =
  let rec check expected (remaining : entry list) =
    match remaining with
    | [] when expected = last + 1 -> Ok ()
    | [] ->
        error
          (Printf.sprintf "%s table ended before required ID %d" label expected)
    | entry :: _ when expected > last ->
        error ~line:entry.source_line
          (Printf.sprintf "%s table contains an extra ID %d" label
             entry.templeos_id)
    | entry :: rest when entry.templeos_id = expected ->
        check (expected + 1) rest
    | entry :: _ ->
        error ~line:entry.source_line
          (Printf.sprintf "%s table requires ID %d here, but found %d" label
             expected entry.templeos_id)
  in
  if first > last then invalid_arg "invalid expected opcode table range";
  check first entries

type named = { name : string; line : int }

let validate_registers (registers : register list) =
  match
    duplicate_by
      (fun item -> item.name)
      (List.map
         (fun (register : register) ->
           { name = register.spelling; line = register.source_line })
         registers)
  with
  | None -> Ok ()
  | Some item ->
      error ~line:item.line
        (Printf.sprintf "register spelling %S appears more than once" item.name)

let validate_opcodes (opcodes : opcode list) =
  let names =
    List.concat_map
      (fun (opcode : opcode) ->
        { name = opcode.spelling; line = opcode.source_line }
        :: List.map
             (fun (alias : opcode_alias) ->
               { name = alias.spelling; line = alias.source_line })
             opcode.aliases)
      opcodes
  in
  match duplicate_by (fun item -> item.name) names with
  | None -> Ok ()
  | Some item ->
      error ~line:item.line
        (Printf.sprintf "opcode spelling %S appears more than once" item.name)

let validate tables =
  match validate_registers tables.registers with
  | Error _ as result -> result
  | Ok () -> (
      match validate_unique "language keyword" tables.language with
      | Error _ as result -> result
      | Ok () -> (
          match validate_unique "assembler directive" tables.assembly with
          | Error _ as result -> result
          | Ok () -> (
              match validate_range "language keyword" 0 47 tables.language with
              | Error _ as result -> result
              | Ok () -> (
                  match
                    validate_range "assembler directive" 64 88 tables.assembly
                  with
                  | Error _ as result -> result
                  | Ok () -> validate_opcodes tables.opcodes))))

type section = Registers | Language_keywords | Assembly_keywords | Opcodes

let parse source =
  match tokenize source with
  | Error _ as problem -> problem
  | Ok tokens ->
      let cursor = { tokens = Array.of_list tokens; offset = 0 } in
      let rec records section registers language assembly opcodes =
        let token = current cursor in
        match token.token_kind with
        | End ->
            let tables =
              {
                registers = List.rev registers;
                language = List.rev language;
                assembly = List.rev assembly;
                opcodes = List.rev opcodes;
              }
            in
            validate tables |> Result.map (fun () -> tables)
        | Identifier tag -> (
            advance cursor;
            match register_kind tag with
            | Some register_kind when section = Registers -> (
                match parse_register cursor register_kind with
                | Error _ as problem -> problem
                | Ok register ->
                    records Registers (register :: registers) language assembly
                      opcodes)
            | Some _ ->
                error ~line:token.line
                  "register records must precede keyword and opcode records"
            | None when String.equal tag "KEYWORD" -> (
                if section = Assembly_keywords || section = Opcodes then
                  error ~line:token.line
                    "language keyword records must stay in their ordered \
                     section"
                else
                  match parse_keyword cursor Language with
                  | Error _ as problem -> problem
                  | Ok entry ->
                      records Language_keywords registers (entry :: language)
                        assembly opcodes)
            | None when String.equal tag "ASM_KEYWORD" -> (
                if section = Opcodes then
                  error ~line:token.line
                    "assembler directive records must precede opcode records"
                else
                  match parse_keyword cursor Assembly with
                  | Error _ as problem -> problem
                  | Ok entry ->
                      records Assembly_keywords registers language
                        (entry :: assembly) opcodes)
            | None when String.equal tag "OPCODE" -> (
                match parse_opcode cursor with
                | Error _ as problem -> problem
                | Ok opcode ->
                    records Opcodes registers language assembly
                      (opcode :: opcodes))
            | None ->
                error ~line:token.line
                  (Printf.sprintf "unknown opcode-table statement %S" tag))
        | _ -> unexpected token "a top-level opcode-table statement"
      in
      records Registers [] [] [] []
