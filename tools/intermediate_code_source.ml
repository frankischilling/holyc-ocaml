type argument_count = Zero | One | Two | Variable
type structural_type = Null | Dereference | Assignment | Comparison

type entry = {
  source_name : string;
  constructor_name : string;
  display_name : string;
  code : int;
  argument_count : argument_count;
  result_count : int;
  structural_type : structural_type;
  pops_float : bool;
  prevents_constant_folding : bool;
  definition_line : int;
  metadata_line : int;
}

type tables = { entries : entry list; count : int; count_line : int }
type error = { path : string option; line : int option; message : string }
type definition = { name : string; value : int; line : int }

type opcode_definition = {
  source_name : string;
  constructor_name : string;
  code : int;
  definition_line : int;
}

type raw_metadata = {
  argument_name : string;
  result_count : int;
  structural_name : string;
  pops_float : bool;
  prevents_constant_folding : bool;
  display_name : string;
  metadata_line : int;
}

type cursor = { text : string; mutable offset : int; mutable line : int }

let error ?path ?line message = Error { path; line; message }

let error_to_string problem =
  match (problem.path, problem.line) with
  | None, None -> problem.message
  | Some path, None -> Printf.sprintf "%s: %s" path problem.message
  | None, Some line -> Printf.sprintf "line %d: %s" line problem.message
  | Some path, Some line -> Printf.sprintf "%s:%d: %s" path line problem.message

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

let split_comment line =
  let length = String.length line in
  let rec find offset =
    if offset + 1 >= length then None
    else if Char.equal line.[offset] '/' && Char.equal line.[offset + 1] '/'
    then Some offset
    else find (offset + 1)
  in
  match find 0 with
  | None -> line
  | Some offset -> String.sub line 0 offset

let relevant_name name =
  starts_with ~prefix:"IS_" name
  || starts_with ~prefix:"IST_" name
  || starts_with ~prefix:"IC_" name

let parse_definition line_number line =
  match words (split_comment line) with
  | [ "#define"; name; value ] when relevant_name name -> (
      match int_of_string_opt value with
      | Some value -> Ok (Some { name; value; line = line_number })
      | None ->
          error ~path:"Compiler/CompilerA.HH" ~line:line_number
            (Printf.sprintf "%s has invalid numeric value %S" name value))
  | "#define" :: name :: _ when relevant_name name ->
      error ~path:"Compiler/CompilerA.HH" ~line:line_number
        (Printf.sprintf "%s must have one numeric value" name)
  | _ -> Ok None

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

let validate_named_definitions ~label expected (actual : definition list) =
  match
    duplicate_by (fun (definition : definition) -> definition.name) actual
  with
  | Some definition ->
      error ~path:"Compiler/CompilerA.HH" ~line:definition.line
        (Printf.sprintf "%s %s is defined more than once" label definition.name)
  | None ->
      let rec compare (expected : (string * int) list)
          (actual : definition list) =
        match (expected, actual) with
        | [], [] -> Ok ()
        | (name, _) :: _, [] ->
            error ~path:"Compiler/CompilerA.HH"
              (Printf.sprintf "%s table is missing %s" label name)
        | [], definition :: _ ->
            error ~path:"Compiler/CompilerA.HH" ~line:definition.line
              (Printf.sprintf "%s table contains unexpected %s" label
                 definition.name)
        | ( (expected_name, expected_value) :: expected_rest,
            definition :: actual_rest ) ->
            if not (String.equal expected_name definition.name) then
              error ~path:"Compiler/CompilerA.HH" ~line:definition.line
                (Printf.sprintf "%s table requires %s here, but found %s" label
                   expected_name definition.name)
            else if definition.value <> expected_value then
              error ~path:"Compiler/CompilerA.HH" ~line:definition.line
                (Printf.sprintf "%s must have value %d, but found %d"
                   expected_name expected_value definition.value)
            else compare expected_rest actual_rest
      in
      compare expected actual

let constructor_name (definition : definition) =
  let prefix = "IC_" in
  let raw =
    String.sub definition.name (String.length prefix)
      (String.length definition.name - String.length prefix)
  in
  let valid = function
    | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  if String.equal raw "" || not (String.for_all valid raw) then
    error ~path:"Compiler/CompilerA.HH" ~line:definition.line
      (Printf.sprintf "%s cannot be represented as an OCaml constructor"
         definition.name)
  else Ok ("Ic_" ^ String.lowercase_ascii raw)

let validate_opcodes (definitions : definition list) count count_line =
  if count <> 0xB9 then
    error ~path:"Compiler/CompilerA.HH" ~line:count_line
      (Printf.sprintf "IC_ICS_NUM must be 0xB9, but found 0x%X" count)
  else if List.length definitions <> count then
    error ~path:"Compiler/CompilerA.HH"
      (Printf.sprintf "IC_ICS_NUM is %d, but found %d opcode definitions" count
         (List.length definitions))
  else
    match duplicate_by (fun definition -> definition.name) definitions with
    | Some definition ->
        error ~path:"Compiler/CompilerA.HH" ~line:definition.line
          (Printf.sprintf "opcode %s is defined more than once" definition.name)
    | None -> (
        match duplicate_by (fun definition -> definition.value) definitions with
        | Some definition ->
            error ~path:"Compiler/CompilerA.HH" ~line:definition.line
              (Printf.sprintf "opcode value 0x%02X is assigned more than once"
                 definition.value)
        | None -> (
            let rec convert expected found = function
              | [] -> Ok (List.rev found)
              | definition :: rest -> (
                  if definition.value <> expected then
                    error ~path:"Compiler/CompilerA.HH" ~line:definition.line
                      (Printf.sprintf
                         "opcode table requires value 0x%02X here, but %s uses \
                          0x%02X"
                         expected definition.name definition.value)
                  else
                    match constructor_name definition with
                    | Error _ as result -> result
                    | Ok constructor_name ->
                        convert (expected + 1)
                          ({
                             source_name = definition.name;
                             constructor_name;
                             code = definition.value;
                             definition_line = definition.line;
                           }
                          :: found)
                          rest)
            in
            match convert 0 [] definitions with
            | Error _ as result -> result
            | Ok opcodes -> (
                match
                  duplicate_by (fun opcode -> opcode.constructor_name) opcodes
                with
                | None -> Ok opcodes
                | Some opcode ->
                    error ~path:"Compiler/CompilerA.HH"
                      ~line:opcode.definition_line
                      (Printf.sprintf
                         "OCaml constructor %s would be generated more than \
                          once"
                         opcode.constructor_name))))

let parse_compiler source =
  let lines =
    normalize_checkout_line_endings source |> String.split_on_char '\n'
  in
  let rec collect line_number arguments structural opcodes count = function
    | [] -> Ok (List.rev arguments, List.rev structural, List.rev opcodes, count)
    | line :: rest -> (
        match parse_definition line_number line with
        | Error _ as result -> result
        | Ok None ->
            collect (line_number + 1) arguments structural opcodes count rest
        | Ok (Some definition) ->
            if starts_with ~prefix:"IST_" definition.name then
              collect (line_number + 1) arguments (definition :: structural)
                opcodes count rest
            else if starts_with ~prefix:"IS_" definition.name then
              collect (line_number + 1) (definition :: arguments) structural
                opcodes count rest
            else if String.equal definition.name "IC_ICS_NUM" then
              match count with
              | Some _ ->
                  error ~path:"Compiler/CompilerA.HH" ~line:line_number
                    "IC_ICS_NUM is defined more than once"
              | None ->
                  collect (line_number + 1) arguments structural opcodes
                    (Some (definition.value, definition.line))
                    rest
            else
              collect (line_number + 1) arguments structural
                (definition :: opcodes) count rest)
  in
  match collect 1 [] [] [] None lines with
  | Error _ as result -> result
  | Ok (_, _, _, None) ->
      error ~path:"Compiler/CompilerA.HH" "IC_ICS_NUM is missing"
  | Ok (arguments, structural, opcodes, Some (count, count_line)) -> (
      match
        validate_named_definitions ~label:"argument shape"
          [ ("IS_0_ARG", 0); ("IS_1_ARG", 1); ("IS_2_ARG", 2); ("IS_V_ARG", 3) ]
          arguments
      with
      | Error _ as result -> result
      | Ok () -> (
          match
            validate_named_definitions ~label:"structural type"
              [
                ("IST_NULL", 0);
                ("IST_DEREF", 1);
                ("IST_ASSIGN", 2);
                ("IST_CMP", 3);
              ]
              structural
          with
          | Error _ as result -> result
          | Ok () -> (
              match validate_opcodes opcodes count count_line with
              | Error _ as result -> result
              | Ok opcodes -> Ok (opcodes, count, count_line))))

let compact_whitespace text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (function
      | ' ' | '\t' | '\n' | '\r' -> ()
      | byte -> Buffer.add_char buffer byte)
    text;
  Buffer.contents buffer

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let validate_struct_layout source =
  let compact = compact_whitespace source in
  let expected =
    "classCIntermediateStruct{U8arg_cnt,res_cnt,type;Boolfpop,not_const,pad[3];U8*name;};"
  in
  if contains ~needle:expected compact then Ok ()
  else
    error ~path:"Compiler/CompilerA.HH"
      "CIntermediateStruct no longer has the audited field layout"

let current cursor =
  if cursor.offset >= String.length cursor.text then None
  else Some cursor.text.[cursor.offset]

let advance cursor =
  match current cursor with
  | None -> ()
  | Some byte ->
      cursor.offset <- cursor.offset + 1;
      if Char.equal byte '\n' then cursor.line <- cursor.line + 1

let rec skip_whitespace cursor =
  match current cursor with
  | Some (' ' | '\t' | '\n') ->
      advance cursor;
      skip_whitespace cursor
  | _ -> ()

let cursor_starts_with cursor needle =
  let remaining = String.length cursor.text - cursor.offset in
  String.length needle <= remaining
  && String.sub cursor.text cursor.offset (String.length needle) = needle

let expect_byte cursor expected description =
  match current cursor with
  | Some found when Char.equal found expected ->
      advance cursor;
      Ok ()
  | _ ->
      error ~path:"Compiler/CInit.HC" ~line:cursor.line
        (Printf.sprintf "expected %s" description)

let identifier cursor =
  let is_start = function
    | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_rest = function
    | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' -> true
    | _ -> false
  in
  match current cursor with
  | Some byte when is_start byte ->
      let first = cursor.offset in
      advance cursor;
      while Option.fold ~none:false ~some:is_rest (current cursor) do
        advance cursor
      done;
      Ok (String.sub cursor.text first (cursor.offset - first))
  | _ ->
      error ~path:"Compiler/CInit.HC" ~line:cursor.line "expected an identifier"

let decimal cursor description =
  let first = cursor.offset in
  while
    Option.fold ~none:false
      ~some:(function
        | '0' .. '9' -> true
        | _ -> false)
      (current cursor)
  do
    advance cursor
  done;
  if cursor.offset = first then
    error ~path:"Compiler/CInit.HC" ~line:cursor.line
      (Printf.sprintf "expected %s" description)
  else
    let text = String.sub cursor.text first (cursor.offset - first) in
    match int_of_string_opt text with
    | Some value -> Ok value
    | None ->
        error ~path:"Compiler/CInit.HC" ~line:cursor.line
          (Printf.sprintf "%s is out of range" description)

let quoted_name cursor =
  match expect_byte cursor '"' "an opening quote for the display name" with
  | Error _ as result -> result
  | Ok () ->
      let first = cursor.offset in
      let rec finish () =
        match current cursor with
        | None | Some '\n' ->
            error ~path:"Compiler/CInit.HC" ~line:cursor.line
              "intermediate-code display name has no closing quote"
        | Some '"' ->
            let name = String.sub cursor.text first (cursor.offset - first) in
            advance cursor;
            if String.equal name "" then
              error ~path:"Compiler/CInit.HC" ~line:cursor.line
                "intermediate-code display name cannot be empty"
            else Ok name
        | Some '\\' ->
            error ~path:"Compiler/CInit.HC" ~line:cursor.line
              "intermediate-code display names cannot contain escapes"
        | Some _ ->
            advance cursor;
            finish ()
      in
      finish ()

let ( let* ) result continuation =
  match result with
  | Error _ as result -> result
  | Ok value -> continuation value

let comma cursor description =
  skip_whitespace cursor;
  expect_byte cursor ',' ("`,` " ^ description)

let boolean cursor =
  let* name = identifier cursor in
  match name with
  | "FALSE" -> Ok false
  | "TRUE" -> Ok true
  | _ ->
      error ~path:"Compiler/CInit.HC" ~line:cursor.line
        (Printf.sprintf "expected TRUE or FALSE, but found %s" name)

let padding cursor position =
  let* value = decimal cursor "a padding byte" in
  if value = 0 then Ok ()
  else
    error ~path:"Compiler/CInit.HC" ~line:cursor.line
      (Printf.sprintf "padding field %d must remain zero" position)

let parse_metadata_record cursor =
  let metadata_line = cursor.line in
  let* () = expect_byte cursor '{' "`{` at the start of a metadata record" in
  skip_whitespace cursor;
  let* argument_name = identifier cursor in
  let* () = comma cursor "after the argument shape" in
  skip_whitespace cursor;
  let* result_count = decimal cursor "a result count" in
  let* () = comma cursor "after the result count" in
  skip_whitespace cursor;
  let* structural_name = identifier cursor in
  let* () = comma cursor "after the structural type" in
  skip_whitespace cursor;
  let* pops_float = boolean cursor in
  let* () = comma cursor "after fpop" in
  skip_whitespace cursor;
  let* prevents_constant_folding = boolean cursor in
  let* () = comma cursor "after not_const" in
  skip_whitespace cursor;
  let* () = padding cursor 1 in
  let* () = comma cursor "after padding field 1" in
  skip_whitespace cursor;
  let* () = padding cursor 2 in
  let* () = comma cursor "after padding field 2" in
  skip_whitespace cursor;
  let* () = padding cursor 3 in
  let* () = comma cursor "after padding field 3" in
  skip_whitespace cursor;
  let* display_name = quoted_name cursor in
  skip_whitespace cursor;
  let* () = expect_byte cursor '}' "`}` at the end of a metadata record" in
  Ok
    {
      argument_name;
      result_count;
      structural_name;
      pops_float;
      prevents_constant_folding;
      display_name;
      metadata_line;
    }

let find_table_start source =
  let marker = "CIntermediateStruct intermediate_code_table[IC_ICS_NUM]={" in
  let marker_length = String.length marker in
  let source_length = String.length source in
  let rec search offset line found =
    if offset + marker_length > source_length then
      match found with
      | None ->
          error ~path:"Compiler/CInit.HC"
            "intermediate_code_table declaration is missing"
      | Some position -> Ok position
    else if String.sub source offset marker_length = marker then
      match found with
      | Some _ ->
          error ~path:"Compiler/CInit.HC" ~line
            "intermediate_code_table is declared more than once"
      | None ->
          search (offset + marker_length) line
            (Some (offset + marker_length, line))
    else
      let next_line =
        if Char.equal source.[offset] '\n' then line + 1 else line
      in
      search (offset + 1) next_line found
  in
  search 0 1 None

let parse_metadata source =
  let source = normalize_checkout_line_endings source in
  match find_table_start source with
  | Error _ as result -> result
  | Ok (offset, line) ->
      let cursor = { text = source; offset; line } in
      let rec records found =
        skip_whitespace cursor;
        if cursor_starts_with cursor "};" then (
          advance cursor;
          advance cursor;
          Ok (List.rev found))
        else
          let* record = parse_metadata_record cursor in
          skip_whitespace cursor;
          let* () =
            expect_byte cursor ',' "`,` after an intermediate-code record"
          in
          records (record :: found)
      in
      records []

let argument_count metadata =
  match metadata.argument_name with
  | "IS_0_ARG" -> Ok Zero
  | "IS_1_ARG" -> Ok One
  | "IS_2_ARG" -> Ok Two
  | "IS_V_ARG" -> Ok Variable
  | name ->
      error ~path:"Compiler/CInit.HC" ~line:metadata.metadata_line
        (Printf.sprintf "unknown argument shape %s" name)

let structural_type metadata =
  match metadata.structural_name with
  | "IST_NULL" -> Ok Null
  | "IST_DEREF" -> Ok Dereference
  | "IST_ASSIGN" -> Ok Assignment
  | "IST_CMP" -> Ok Comparison
  | name ->
      error ~path:"Compiler/CInit.HC" ~line:metadata.metadata_line
        (Printf.sprintf "unknown structural type %s" name)

let combine opcodes count count_line metadata =
  if List.length metadata <> count then
    error ~path:"Compiler/CInit.HC"
      (Printf.sprintf
         "intermediate_code_table requires %d records, but found %d" count
         (List.length metadata))
  else
    match duplicate_by (fun record -> record.display_name) metadata with
    | Some record ->
        error ~path:"Compiler/CInit.HC" ~line:record.metadata_line
          (Printf.sprintf "display name %S appears more than once"
             record.display_name)
    | None ->
        let rec entries found opcodes metadata =
          match (opcodes, metadata) with
          | [], [] -> Ok (List.rev found)
          | opcode :: opcode_rest, record :: metadata_rest ->
              if record.result_count < 0 || record.result_count > 1 then
                error ~path:"Compiler/CInit.HC" ~line:record.metadata_line
                  (Printf.sprintf "%s has unsupported result count %d"
                     opcode.source_name record.result_count)
              else
                let* argument_count = argument_count record in
                let* structural_type = structural_type record in
                entries
                  ({
                     source_name = opcode.source_name;
                     constructor_name = opcode.constructor_name;
                     display_name = record.display_name;
                     code = opcode.code;
                     argument_count;
                     result_count = record.result_count;
                     structural_type;
                     pops_float = record.pops_float;
                     prevents_constant_folding =
                       record.prevents_constant_folding;
                     definition_line = opcode.definition_line;
                     metadata_line = record.metadata_line;
                   }
                  :: found)
                  opcode_rest metadata_rest
          | _ -> assert false
        in
        let* entries = entries [] opcodes metadata in
        Ok { entries; count; count_line }

let parse ~compiler_source ~cinit_source =
  let compiler_source = normalize_checkout_line_endings compiler_source in
  let* () = validate_struct_layout compiler_source in
  let* opcodes, count, count_line = parse_compiler compiler_source in
  let* metadata = parse_metadata cinit_source in
  combine opcodes count count_line metadata
