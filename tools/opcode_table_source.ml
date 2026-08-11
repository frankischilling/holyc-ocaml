type kind = Language | Assembly

type entry = {
  kind : kind;
  spelling : string;
  templeos_id : int;
  source_line : int;
}

type tables = {
  language : entry list;
  assembly : entry list;
}

type error = {
  line : int option;
  message : string;
}

type parsed_line = Record of entry | Blank | Other of string | Malformed of error
type state = Before_records | Language_records | Assembly_records | After_records

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
        Char.equal source.[offset] '\r' && offset + 1 < length
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
      (Printf.sprintf "source SHA-256 is %s, but the manifest requires %s" actual
         expected)

let strip_carriage_return line =
  let length = String.length line in
  if length > 0 && Char.equal line.[length - 1] '\r' then
    String.sub line 0 (length - 1)
  else line

let words line =
  let length = String.length line in
  let is_space = function ' ' | '\t' -> true | _ -> false in
  let rec skip_spaces offset =
    if offset < length && is_space line.[offset] then skip_spaces (offset + 1)
    else offset
  in
  let rec take_word offset =
    if offset < length && not (is_space line.[offset]) then take_word (offset + 1)
    else offset
  in
  let rec collect offset found =
    let start = skip_spaces offset in
    if start = length then List.rev found
    else
      let stop = take_word start in
      collect stop (String.sub line start (stop - start) :: found)
  in
  collect 0 []

let valid_spelling spelling =
  let is_letter = function
    | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_rest byte = is_letter byte || match byte with '0' .. '9' -> true | _ -> false in
  let length = String.length spelling in
  length > 0 && is_letter spelling.[0]
  && String.for_all is_rest spelling

let parse_id field =
  let length = String.length field in
  if length < 2 || not (Char.equal field.[length - 1] ';') then None
  else
    let digits = String.sub field 0 (length - 1) in
    int_of_string_opt digits

let classify_line source_line line =
  let line = strip_carriage_return line in
  match words line with
  | [] -> Blank
  | ("KEYWORD" | "ASM_KEYWORD" as tag) :: fields -> (
      match fields with
      | [ spelling; id_field ] when valid_spelling spelling -> (
          match parse_id id_field with
          | Some templeos_id ->
              let kind = if String.equal tag "KEYWORD" then Language else Assembly in
              Record { kind; spelling; templeos_id; source_line }
          | None ->
              Malformed
                {
                  line = Some source_line;
                  message =
                    Printf.sprintf
                      "%s record %S must end with one decimal ID and a semicolon"
                      tag spelling;
                })
      | _ ->
          Malformed
            {
              line = Some source_line;
              message =
                Printf.sprintf
                  "%s records must have the form `%s spelling decimal-id;`" tag
                  tag;
            })
  | first :: _ -> Other first

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

let validate_unique label entries =
  match duplicate_by (fun entry -> entry.templeos_id) entries with
  | Some entry ->
      error ~line:entry.source_line
        (Printf.sprintf "%s ID %d appears more than once" label entry.templeos_id)
  | None -> (
      match duplicate_by (fun entry -> entry.spelling) entries with
      | Some entry ->
          error ~line:entry.source_line
            (Printf.sprintf "%s spelling %S appears more than once" label
               entry.spelling)
      | None -> Ok ())

let validate_range label first last entries =
  let rec check expected = function
    | [] when expected = last + 1 -> Ok ()
    | [] ->
        error
          (Printf.sprintf "%s table ended before required ID %d" label expected)
    | entry :: _ when expected > last ->
        error ~line:entry.source_line
          (Printf.sprintf "%s table contains an extra ID %d" label
             entry.templeos_id)
    | entry :: rest when entry.templeos_id = expected -> check (expected + 1) rest
    | entry :: _ ->
        error ~line:entry.source_line
          (Printf.sprintf "%s table requires ID %d here, but found %d" label
             expected entry.templeos_id)
  in
  if first > last then invalid_arg "invalid expected opcode table range";
  check first entries

let validate tables =
  match validate_unique "language keyword" tables.language with
  | Error _ as result -> result
  | Ok () -> (
      match validate_unique "assembler directive" tables.assembly with
      | Error _ as result -> result
      | Ok () -> (
          match validate_range "language keyword" 0 47 tables.language with
          | Error _ as result -> result
          | Ok () -> validate_range "assembler directive" 64 88 tables.assembly))

let parse source =
  let lines = String.split_on_char '\n' source in
  let rec read line_number state language assembly = function
    | [] ->
        let tables =
          { language = List.rev language; assembly = List.rev assembly }
        in
        validate tables |> Result.map (fun () -> tables)
    | line :: rest -> (
        match classify_line line_number line with
        | Malformed problem -> Error problem
        | Blank -> read (line_number + 1) state language assembly rest
        | Record entry -> (
            match (state, entry.kind) with
            | Before_records, Language
            | Language_records, Language ->
                read (line_number + 1) Language_records (entry :: language)
                  assembly rest
            | (Before_records | Language_records), Assembly ->
                read (line_number + 1) Assembly_records language
                  (entry :: assembly) rest
            | Assembly_records, Assembly ->
                read (line_number + 1) Assembly_records language
                  (entry :: assembly) rest
            | Assembly_records, Language
            | After_records, (Language | Assembly) ->
                error ~line:line_number
                  "keyword records must stay in their single ordered table sections")
        | Other first -> (
            match state with
            | Before_records ->
                read (line_number + 1) state language assembly rest
            | Language_records ->
                error ~line:line_number
                  (Printf.sprintf
                     "unexpected %S statement inside the language keyword table"
                     first)
            | Assembly_records when String.equal first "OPCODE" ->
                read (line_number + 1) After_records language assembly rest
            | Assembly_records ->
                error ~line:line_number
                  (Printf.sprintf
                     "unexpected %S statement inside the assembler directive table"
                     first)
            | After_records ->
                read (line_number + 1) state language assembly rest))
  in
  read 1 Before_records [] [] lines
