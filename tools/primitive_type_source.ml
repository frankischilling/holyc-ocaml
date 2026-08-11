type raw_type = {
  name : string;
  templeos_id : int;
  not_implemented : bool;
  fictitious : bool;
  source_line : int;
  source_comment : string option;
}

type raw_alias = {
  name : string;
  target_name : string;
  templeos_id : int;
  source_line : int;
  source_comment : string option;
}

type public_union = {
  storage_spelling : string;
  public_spelling : string;
  source_line : int;
}

type internal_type = {
  raw_name : string;
  byte_size : int;
  spelling : string;
  source_line : int;
}

type tables = {
  raw_types : raw_type list;
  pointer_alias : raw_alias;
  raw_types_count : int;
  unsigned_flag : int;
  raw_group_mask : int;
  public_unions : public_union list;
  internal_types : internal_type list;
}

type error = { line : int option; message : string }

type definition = {
  name : string;
  value : int;
  line : int;
  comment : string option;
}

type cursor = { text : string; mutable offset : int; mutable line : int }

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

let trim = String.trim

let starts_with ~prefix text =
  let prefix_length = String.length prefix in
  String.length text >= prefix_length
  && String.sub text 0 prefix_length = prefix

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

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
  | None -> (line, None)
  | Some offset ->
      let code = String.sub line 0 offset in
      let comment =
        String.sub line (offset + 2) (length - offset - 2) |> trim
      in
      (code, if String.equal comment "" then None else Some comment)

let parse_number text = try Some (int_of_string text) with Failure _ -> None

let relevant_definition_name name =
  starts_with ~prefix:"RT_" name
  || String.equal name "RTF_UNSIGNED"
  || String.equal name "RTG_MASK"

let parse_definition line_number line =
  let code, comment = split_comment line in
  match words code with
  | [ "#define"; name; value ] when relevant_definition_name name -> (
      match parse_number value with
      | Some value -> Ok (Some { name; value; line = line_number; comment })
      | None ->
          error ~line:line_number
            (Printf.sprintf "raw type definition %s has invalid value %S" name
               value))
  | "#define" :: name :: _ when relevant_definition_name name ->
      error ~line:line_number
        (Printf.sprintf
           "raw type definition %s must have one numeric value before its line \
            comment"
           name)
  | _ -> Ok None

let parse_public_union line_number line =
  match words (fst (split_comment line)) with
  | [ storage_spelling; "union"; public_spelling ]
    when starts_with ~prefix:"RT_" storage_spelling |> not ->
      let expected_public = [ "U16"; "I16"; "U32"; "I32"; "U64"; "I64" ] in
      if List.mem public_spelling expected_public then
        Ok
          (Some { storage_spelling; public_spelling; source_line = line_number })
      else Ok None
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

let expected_definitions =
  [
    ("RT_I0", 2);
    ("RT_U0", 3);
    ("RT_I8", 4);
    ("RT_U8", 5);
    ("RT_I16", 6);
    ("RT_U16", 7);
    ("RT_I32", 8);
    ("RT_U32", 9);
    ("RT_I64", 10);
    ("RT_PTR", 10);
    ("RT_U64", 11);
    ("RT_F32", 12);
    ("RT_UF32", 13);
    ("RT_F64", 14);
    ("RT_UF64", 15);
    ("RT_RTS_NUM", 16);
    ("RTF_UNSIGNED", 1);
    ("RTG_MASK", 0xFF);
  ]

let expected_public_unions =
  [
    ("U16i", "U16");
    ("I16i", "I16");
    ("U32i", "U32");
    ("I32i", "I32");
    ("U64i", "U64");
    ("I64i", "I64");
  ]

let validate_definition_order (definitions : definition list) =
  match duplicate_by (fun definition -> definition.name) definitions with
  | Some definition ->
      error ~line:definition.line
        (Printf.sprintf "raw type definition %s appears more than once"
           definition.name)
  | None ->
      let rec compare (expected : (string * int) list)
          (actual : definition list) =
        match (expected, actual) with
        | [], [] -> Ok ()
        | (expected_name, _) :: _, [] ->
            error (Printf.sprintf "raw type table is missing %s" expected_name)
        | [], definition :: _ ->
            error ~line:definition.line
              (Printf.sprintf "raw type table contains unexpected %s"
                 definition.name)
        | ( (expected_name, expected_value) :: expected_rest,
            definition :: actual_rest ) ->
            if not (String.equal definition.name expected_name) then
              error ~line:definition.line
                (Printf.sprintf "raw type table requires %s here, but found %s"
                   expected_name definition.name)
            else if definition.value <> expected_value then
              error ~line:definition.line
                (Printf.sprintf "%s must have value %d, but found %d"
                   expected_name expected_value definition.value)
            else compare expected_rest actual_rest
      in
      compare expected_definitions definitions

let validate_public_unions (unions : public_union list) =
  match duplicate_by (fun union -> union.public_spelling) unions with
  | Some union ->
      error ~line:union.source_line
        (Printf.sprintf "public integer union %s appears more than once"
           union.public_spelling)
  | None ->
      let rec compare (expected : (string * string) list)
          (actual : public_union list) =
        match (expected, actual) with
        | [], [] -> Ok ()
        | (_, public_spelling) :: _, [] ->
            error
              (Printf.sprintf "public integer union %s is missing"
                 public_spelling)
        | [], union :: _ ->
            error ~line:union.source_line
              (Printf.sprintf "unexpected public integer union %s"
                 union.public_spelling)
        | ( (expected_storage, expected_public) :: expected_rest,
            union :: actual_rest ) ->
            if
              not
                (String.equal union.storage_spelling expected_storage
                && String.equal union.public_spelling expected_public)
            then
              error ~line:union.source_line
                (Printf.sprintf
                   "public integer union requires `%s union %s` here, but \
                    found `%s union %s`"
                   expected_storage expected_public union.storage_spelling
                   union.public_spelling)
            else compare expected_rest actual_rest
      in
      compare expected_public_unions unions

let definition name (definitions : definition list) =
  List.find (fun definition -> String.equal definition.name name) definitions

let status_from_comment (definition : definition) =
  let comment = Option.value ~default:"" definition.comment in
  ( contains ~needle:"Not implemented" comment,
    contains ~needle:"Fictitious" comment )

let validate_status (definitions : definition list) =
  let expected =
    [
      ("RT_I0", false, false);
      ("RT_U0", false, false);
      ("RT_I8", false, false);
      ("RT_U8", false, false);
      ("RT_I16", false, false);
      ("RT_U16", false, false);
      ("RT_I32", false, false);
      ("RT_U32", false, false);
      ("RT_I64", false, false);
      ("RT_U64", false, false);
      ("RT_F32", true, false);
      ("RT_UF32", true, true);
      ("RT_F64", false, false);
      ("RT_UF64", false, true);
    ]
  in
  let rec check = function
    | [] -> Ok ()
    | (name, expected_not_implemented, expected_fictitious) :: rest ->
        let found = definition name definitions in
        let not_implemented, fictitious = status_from_comment found in
        if
          not_implemented <> expected_not_implemented
          || fictitious <> expected_fictitious
        then
          error ~line:found.line
            (Printf.sprintf
               "%s availability markers no longer match the pinned source" name)
        else check rest
  in
  check expected

let parse_kernel source =
  let source = normalize_checkout_line_endings source in
  let lines = String.split_on_char '\n' source in
  let rec collect line_number definitions unions = function
    | [] -> Ok (List.rev definitions, List.rev unions)
    | line :: rest -> (
        match parse_definition line_number line with
        | Error _ as result -> result
        | Ok definition -> (
            match parse_public_union line_number line with
            | Error _ as result -> result
            | Ok union ->
                collect (line_number + 1)
                  (Option.to_list definition @ definitions)
                  (Option.to_list union @ unions)
                  rest))
  in
  match collect 1 [] [] lines with
  | Error _ as result -> result
  | Ok (definitions, unions) -> (
      match validate_definition_order definitions with
      | Error _ as result -> result
      | Ok () -> (
          match validate_public_unions unions with
          | Error _ as result -> result
          | Ok () -> (
              match validate_status definitions with
              | Error _ as result -> result
              | Ok () -> Ok (definitions, unions))))

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
  | _ -> error ~line:cursor.line (Printf.sprintf "expected %s" description)

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
  | _ -> error ~line:cursor.line "expected an identifier"

let decimal cursor =
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
  if cursor.offset = first then error ~line:cursor.line "expected a byte size"
  else
    let value = String.sub cursor.text first (cursor.offset - first) in
    match int_of_string_opt value with
    | Some value -> Ok value
    | None -> error ~line:cursor.line "internal type byte size is out of range"

let quoted_identifier cursor =
  match expect_byte cursor '"' "an opening quote" with
  | Error _ as result -> result
  | Ok () ->
      let first = cursor.offset in
      let rec find_end () =
        match current cursor with
        | None | Some '\n' ->
            error ~line:cursor.line "internal type name has no closing quote"
        | Some '"' ->
            let spelling =
              String.sub cursor.text first (cursor.offset - first)
            in
            advance cursor;
            if String.equal spelling "" then
              error ~line:cursor.line "internal type name cannot be empty"
            else Ok spelling
        | Some '\\' ->
            error ~line:cursor.line
              "internal type names cannot contain escape sequences"
        | Some _ ->
            advance cursor;
            find_end ()
      in
      find_end ()

let bind result continuation =
  match result with
  | Error _ as result -> result
  | Ok value -> continuation value

let parse_internal_record cursor =
  let source_line = cursor.line in
  bind (expect_byte cursor '{' "`{` at the start of an internal type record")
    (fun () ->
      skip_whitespace cursor;
      bind (identifier cursor) (fun raw_name ->
          skip_whitespace cursor;
          bind (expect_byte cursor ',' "`,` after the raw type name") (fun () ->
              skip_whitespace cursor;
              bind (decimal cursor) (fun byte_size ->
                  skip_whitespace cursor;
                  bind (expect_byte cursor ',' "`,` after the byte size")
                    (fun () ->
                      skip_whitespace cursor;
                      bind (quoted_identifier cursor) (fun spelling ->
                          skip_whitespace cursor;
                          bind
                            (expect_byte cursor '}'
                               "`}` at the end of an internal type record")
                            (fun () ->
                              Ok { raw_name; byte_size; spelling; source_line })))))))

let find_table_start source =
  let marker = "internal_types_table[INTERNAL_TYPES_NUM]={" in
  let marker_length = String.length marker in
  let source_length = String.length source in
  let rec search offset line found =
    if offset + marker_length > source_length then
      match found with
      | None -> error "internal_types_table declaration is missing"
      | Some position -> Ok position
    else if String.sub source offset marker_length = marker then
      match found with
      | Some _ -> error ~line "internal_types_table is declared more than once"
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

let parse_internal_records source offset line =
  let cursor = { text = source; offset; line } in
  let rec records found =
    skip_whitespace cursor;
    if cursor_starts_with cursor "};" then (
      advance cursor;
      advance cursor;
      Ok (List.rev found))
    else
      bind (parse_internal_record cursor) (fun entry ->
          skip_whitespace cursor;
          match current cursor with
          | Some ',' ->
              advance cursor;
              records (entry :: found)
          | Some '}' when cursor_starts_with cursor "};" ->
              records (entry :: found)
          | _ ->
              error ~line:cursor.line
                "expected `,` or `};` after an internal type record")
  in
  records []

let internal_type_count source =
  let lines = String.split_on_char '\n' source in
  let rec find line_number found = function
    | [] -> (
        match found with
        | None -> error "INTERNAL_TYPES_NUM is missing"
        | Some value -> Ok value)
    | line :: rest -> (
        match words (fst (split_comment line)) with
        | [ "#define"; "INTERNAL_TYPES_NUM"; value ] -> (
            match found with
            | Some _ ->
                error ~line:line_number
                  "INTERNAL_TYPES_NUM is defined more than once"
            | None -> (
                match int_of_string_opt value with
                | Some value -> find (line_number + 1) (Some value) rest
                | None ->
                    error ~line:line_number
                      "INTERNAL_TYPES_NUM must be a decimal integer"))
        | _ -> find (line_number + 1) found rest)
  in
  find 1 None lines

let expected_internal_types =
  [
    ("RT_I0", 0, "I0i");
    ("RT_I0", 0, "I0");
    ("RT_U0", 0, "U0i");
    ("RT_U0", 0, "U0");
    ("RT_I8", 1, "I8i");
    ("RT_I8", 1, "I8");
    ("RT_I8", 1, "Bool");
    ("RT_U8", 1, "U8i");
    ("RT_U8", 1, "U8");
    ("RT_I16", 2, "I16i");
    ("RT_U16", 2, "U16i");
    ("RT_I32", 4, "I32i");
    ("RT_U32", 4, "U32i");
    ("RT_I64", 8, "I64i");
    ("RT_U64", 8, "U64i");
    ("RT_F64", 8, "F64i");
    ("RT_F64", 8, "F64");
  ]

let validate_internal_types count (entries : internal_type list) =
  if count <> 17 then
    error (Printf.sprintf "INTERNAL_TYPES_NUM must be 17, but found %d" count)
  else
    match duplicate_by (fun entry -> entry.spelling) entries with
    | Some entry ->
        error ~line:entry.source_line
          (Printf.sprintf "internal type spelling %S appears more than once"
             entry.spelling)
    | None ->
        let rec compare (expected : (string * int * string) list)
            (actual : internal_type list) =
          match (expected, actual) with
          | [], [] -> Ok ()
          | (_, _, spelling) :: _, [] ->
              error (Printf.sprintf "internal type %s is missing" spelling)
          | [], entry :: _ ->
              error ~line:entry.source_line
                (Printf.sprintf "unexpected internal type %s" entry.spelling)
          | ( (raw_name, byte_size, spelling) :: expected_rest,
              entry :: actual_rest ) ->
              if
                not
                  (String.equal entry.raw_name raw_name
                  && entry.byte_size = byte_size
                  && String.equal entry.spelling spelling)
              then
                error ~line:entry.source_line
                  (Printf.sprintf
                     "internal type table requires {%s,%d,%S} here, but found \
                      {%s,%d,%S}"
                     raw_name byte_size spelling entry.raw_name entry.byte_size
                     entry.spelling)
              else compare expected_rest actual_rest
        in
        compare expected_internal_types entries

let parse_cinit source =
  let source = normalize_checkout_line_endings source in
  bind (internal_type_count source) (fun count ->
      bind (find_table_start source) (fun (offset, line) ->
          bind (parse_internal_records source offset line) (fun entries ->
              bind (validate_internal_types count entries) (fun () ->
                  Ok entries))))

let raw_type_of_definition (definition : definition) =
  let not_implemented, fictitious = status_from_comment definition in
  {
    name = definition.name;
    templeos_id = definition.value;
    not_implemented;
    fictitious;
    source_line = definition.line;
    source_comment = definition.comment;
  }

let parse ~kernel_source ~cinit_source =
  bind (parse_kernel kernel_source) (fun (definitions, public_unions) ->
      bind (parse_cinit cinit_source) (fun internal_types ->
          let raw_names =
            [
              "RT_I0";
              "RT_U0";
              "RT_I8";
              "RT_U8";
              "RT_I16";
              "RT_U16";
              "RT_I32";
              "RT_U32";
              "RT_I64";
              "RT_U64";
              "RT_F32";
              "RT_UF32";
              "RT_F64";
              "RT_UF64";
            ]
          in
          let raw_types =
            List.map
              (fun name ->
                definition name definitions |> raw_type_of_definition)
              raw_names
          in
          let pointer = definition "RT_PTR" definitions in
          let pointer_alias =
            {
              name = pointer.name;
              target_name = "RT_I64";
              templeos_id = pointer.value;
              source_line = pointer.line;
              source_comment = pointer.comment;
            }
          in
          Ok
            {
              raw_types;
              pointer_alias;
              raw_types_count = (definition "RT_RTS_NUM" definitions).value;
              unsigned_flag = (definition "RTF_UNSIGNED" definitions).value;
              raw_group_mask = (definition "RTG_MASK" definitions).value;
              public_unions;
              internal_types;
            }))
