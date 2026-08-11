type source_reference = { path : string; line : int }

type option_entry = {
  name : string;
  bit_index : int;
  initially_enabled : bool;
  definition_line : int;
  source_comment : string option;
  consumers : source_reference list;
}

type gap = { first : int; last : int }

type api = {
  option_line : int;
  get_option_line : int;
  bequ_line : int;
  state_expression : string;
  set_returns_previous : bool;
}

type tables = {
  options : option_entry list;
  gaps : gap list;
  default_source_line : int;
  echo_mask_line : int;
  api : api;
}

type error = { path : string option; line : int option; message : string }

type definition = {
  name : string;
  bit_index : int;
  line : int;
  comment : string option;
}

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

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let trim = String.trim

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

let expected_names =
  [
    "OPTf_ECHO";
    "OPTf_TRACE";
    "OPTf_WARN_UNUSED_VAR";
    "OPTf_WARN_PAREN";
    "OPTf_WARN_DUP_TYPES";
    "OPTf_WARN_HEADER_MISMATCH";
    "OPTf_EXTERNS_TO_IMPORTS";
    "OPTf_KEEP_PRIVATE";
    "OPTf_NO_REG_VAR";
    "OPTf_GLBLS_ON_DATA_HEAP";
    "OPTf_NO_BUILTIN_CONST";
    "OPTf_USE_IMM64";
  ]

let parse_definition line_number line =
  let code, comment = split_comment line in
  match words code with
  | [ "#define"; name; value ] when starts_with ~prefix:"OPTf_" name -> (
      match int_of_string_opt value with
      | Some bit_index ->
          Ok (Some { name; bit_index; line = line_number; comment })
      | None ->
          error ~path:"Kernel/KernelA.HH" ~line:line_number
            (Printf.sprintf "%s has invalid bit index %S" name value))
  | "#define" :: name :: _ when starts_with ~prefix:"OPTf_" name ->
      error ~path:"Kernel/KernelA.HH" ~line:line_number
        (Printf.sprintf "%s must have one numeric bit index" name)
  | _ -> Ok None

let parse_echo_mask line_number line =
  match words (fst (split_comment line)) with
  | [ "#define"; "OPTF_ECHO"; "(1<<OPTf_ECHO)" ] -> Ok (Some line_number)
  | "#define" :: "OPTF_ECHO" :: _ ->
      error ~path:"Kernel/KernelA.HH" ~line:line_number
        "OPTF_ECHO must remain `(1<<OPTf_ECHO)`"
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

let validate_definition_order definitions =
  match duplicate_by (fun definition -> definition.name) definitions with
  | Some definition ->
      error ~path:"Kernel/KernelA.HH" ~line:definition.line
        (Printf.sprintf "%s is defined more than once" definition.name)
  | None -> (
      match
        duplicate_by (fun definition -> definition.bit_index) definitions
      with
      | Some definition ->
          error ~path:"Kernel/KernelA.HH" ~line:definition.line
            (Printf.sprintf
               "compiler option bit 0x%X is assigned more than once"
               definition.bit_index)
      | None ->
          let rec compare names entries previous_index =
            match (names, entries) with
            | [], [] -> Ok ()
            | name :: _, [] ->
                error ~path:"Kernel/KernelA.HH"
                  (Printf.sprintf "compiler option table is missing %s" name)
            | [], definition :: _ ->
                error ~path:"Kernel/KernelA.HH" ~line:definition.line
                  (Printf.sprintf "compiler option table contains unexpected %s"
                     definition.name)
            | name :: names, definition :: entries ->
                if not (String.equal name definition.name) then
                  error ~path:"Kernel/KernelA.HH" ~line:definition.line
                    (Printf.sprintf
                       "compiler option table requires %s here, but found %s"
                       name definition.name)
                else if definition.bit_index < 0 || definition.bit_index > 63
                then
                  error ~path:"Kernel/KernelA.HH" ~line:definition.line
                    (Printf.sprintf
                       "%s bit index %d is outside the 64-bit option field"
                       definition.name definition.bit_index)
                else if definition.bit_index <= previous_index then
                  error ~path:"Kernel/KernelA.HH" ~line:definition.line
                    "compiler option bit indices must increase in source order"
                else compare names entries definition.bit_index
          in
          compare expected_names definitions (-1))

let definition name definitions =
  List.find (fun definition -> String.equal definition.name name) definitions

let require_comment definitions name marker =
  let found = definition name definitions in
  let comment = Option.value ~default:"" found.comment in
  if contains ~needle:marker comment then Ok ()
  else
    error ~path:"Kernel/KernelA.HH" ~line:found.line
      (Printf.sprintf "%s must retain the source note %S" name marker)

let validate_comments definitions =
  let requirements =
    [
      ("OPTf_WARN_UNUSED_VAR", "Applied to funs, not stmts");
      ("OPTf_NO_REG_VAR", "Applied to funs, not stmts");
      ("OPTf_NO_BUILTIN_CONST", "Applied to funs, not stmts");
      ("OPTf_USE_IMM64", "Not completely implemented");
    ]
  in
  let rec check = function
    | [] -> Ok ()
    | (name, marker) :: rest -> (
        match require_comment definitions name marker with
        | Error _ as result -> result
        | Ok () -> check rest)
  in
  check requirements

let parse_kernel source =
  let lines =
    normalize_checkout_line_endings source |> String.split_on_char '\n'
  in
  let rec collect line_number definitions echo_lines = function
    | [] -> Ok (List.rev definitions, List.rev echo_lines)
    | line :: rest -> (
        match parse_definition line_number line with
        | Error _ as result -> result
        | Ok parsed_definition -> (
            match parse_echo_mask line_number line with
            | Error _ as result -> result
            | Ok echo_line ->
                collect (line_number + 1)
                  (Option.to_list parsed_definition @ definitions)
                  (Option.to_list echo_line @ echo_lines)
                  rest))
  in
  match collect 1 [] [] lines with
  | Error _ as result -> result
  | Ok (definitions, echo_lines) -> (
      match validate_definition_order definitions with
      | Error _ as result -> result
      | Ok () -> (
          match validate_comments definitions with
          | Error _ as result -> result
          | Ok () -> (
              match echo_lines with
              | [ echo_line ] -> Ok (definitions, echo_line)
              | [] -> error ~path:"Kernel/KernelA.HH" "OPTF_ECHO is missing"
              | _ ->
                  error ~path:"Kernel/KernelA.HH"
                    "OPTF_ECHO is defined more than once")))

let find_offsets ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset found =
    if offset + needle_length > text_length then List.rev found
    else if String.sub text offset needle_length = needle then
      search (offset + needle_length) (offset :: found)
    else search (offset + 1) found
  in
  if needle_length = 0 then [] else search 0 []

let line_of_offset text target =
  let rec count offset line =
    if offset >= target then line
    else if Char.equal text.[offset] '\n' then count (offset + 1) (line + 1)
    else count (offset + 1) line
  in
  count 0 1

let parse_default_term line term =
  let term = trim term in
  let prefix = "1<<" in
  if starts_with ~prefix term then
    let name =
      String.sub term (String.length prefix) (String.length term - 3)
    in
    if starts_with ~prefix:"OPTf_" name then Ok name
    else
      error ~path:"Compiler/Lex.HC" ~line
        (Printf.sprintf "default option term %S does not name an OPTf_* bit"
           term)
  else
    error ~path:"Compiler/Lex.HC" ~line
      (Printf.sprintf "default option term %S must use `1<<OPTf_*`" term)

let parse_defaults definitions source =
  let source = normalize_checkout_line_endings source in
  let marker = "cc->opts=" in
  match find_offsets ~needle:marker source with
  | [] ->
      error ~path:"Compiler/Lex.HC"
        "the initial `cc->opts` assignment is missing"
  | _ :: _ :: _ ->
      error ~path:"Compiler/Lex.HC"
        "the initial `cc->opts` assignment is ambiguous"
  | [ offset ] -> (
      let line = line_of_offset source offset in
      let expression_start = offset + String.length marker in
      let rec find_end cursor =
        if cursor >= String.length source then None
        else if Char.equal source.[cursor] ';' then Some cursor
        else find_end (cursor + 1)
      in
      match find_end expression_start with
      | None ->
          error ~path:"Compiler/Lex.HC" ~line
            "the initial `cc->opts` assignment has no semicolon"
      | Some expression_end -> (
          let expression =
            String.sub source expression_start
              (expression_end - expression_start)
          in
          let terms = String.split_on_char '|' expression in
          let rec parse found = function
            | [] -> Ok (List.rev found)
            | term :: rest -> (
                match parse_default_term line term with
                | Error _ as result -> result
                | Ok name -> parse (name :: found) rest)
          in
          match parse [] terms with
          | Error _ as result -> result
          | Ok names -> (
              let known name =
                List.exists
                  (fun definition -> String.equal definition.name name)
                  definitions
              in
              match List.find_opt (fun name -> not (known name)) names with
              | Some name ->
                  error ~path:"Compiler/Lex.HC" ~line
                    (Printf.sprintf "default state references unknown option %s"
                       name)
              | None -> (
                  match duplicate_by Fun.id names with
                  | Some name ->
                      error ~path:"Compiler/Lex.HC" ~line
                        (Printf.sprintf
                           "default state enables %s more than once" name)
                  | None ->
                      let expected =
                        [ "OPTf_WARN_UNUSED_VAR"; "OPTf_WARN_HEADER_MISMATCH" ]
                      in
                      if names = expected then Ok (names, line)
                      else
                        error ~path:"Compiler/Lex.HC" ~line
                          "the initial option state no longer enables \
                           WARN_UNUSED_VAR and WARN_HEADER_MISMATCH"))))

let unique_line ~path signature source =
  let lines =
    normalize_checkout_line_endings source |> String.split_on_char '\n'
  in
  let found =
    List.mapi
      (fun index line ->
        if String.equal (trim line) signature then Some (index + 1) else None)
      lines
    |> List.filter_map Fun.id
  in
  match found with
  | [ line ] -> Ok line
  | [] ->
      error ~path
        (Printf.sprintf "required declaration %S is missing" signature)
  | _ ->
      error ~path
        (Printf.sprintf "required declaration %S appears more than once"
           signature)

let parse_api cmisc_source bequ_source =
  match
    unique_line ~path:"Compiler/CMisc.HC" "Bool Option(I64 num,Bool val)"
      cmisc_source
  with
  | Error _ as result -> result
  | Ok option_line -> (
      match
        unique_line ~path:"Compiler/CMisc.HC" "Bool GetOption(I64 num)"
          cmisc_source
      with
      | Error _ as result -> result
      | Ok get_option_line -> (
          let cmisc = normalize_checkout_line_endings cmisc_source in
          if
            not
              (contains ~needle:"return BEqu(&Fs->last_cc->opts,num,val);" cmisc)
          then
            error ~path:"Compiler/CMisc.HC" ~line:option_line
              "Option must update `Fs->last_cc->opts` through BEqu"
          else if
            not (contains ~needle:"return Bt(&Fs->last_cc->opts,num);" cmisc)
          then
            error ~path:"Compiler/CMisc.HC" ~line:get_option_line
              "GetOption must read `Fs->last_cc->opts` through Bt"
          else
            let bequ = normalize_checkout_line_endings bequ_source in
            match find_offsets ~needle:"_BEQU::" bequ with
            | [ bequ_offset ] ->
                let bequ_line = line_of_offset bequ bequ_offset in
                let body_end =
                  match find_offsets ~needle:"_LBEQU::" bequ with
                  | [ offset ] when offset > bequ_offset -> offset
                  | _ -> String.length bequ
                in
                let body =
                  String.sub bequ bequ_offset (body_end - bequ_offset)
                in
                if
                  contains ~needle:"BTS\tU64 [RBX],RCX" body
                  && contains ~needle:"BTR\tU64 [RBX],RCX" body
                  && contains ~needle:"ADC\tAL,0" body
                then
                  Ok
                    {
                      option_line;
                      get_option_line;
                      bequ_line;
                      state_expression = "Fs->last_cc->opts";
                      set_returns_previous = true;
                    }
                else
                  error ~path:"Kernel/KUtils.HC" ~line:bequ_line
                    "_BEQU no longer returns the previous bit through the \
                     carry flag"
            | [] -> error ~path:"Kernel/KUtils.HC" "_BEQU is missing"
            | _ -> error ~path:"Kernel/KUtils.HC" "_BEQU appears more than once"
          ))

type scan_state =
  | Line_comment
  | Block_comment of int
  | String_literal
  | Char_literal

let is_identifier_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_identifier_rest = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' -> true
  | _ -> false

let scan_option_identifiers ~path source =
  let source = normalize_checkout_line_endings source in
  let length = String.length source in
  let rec normal offset line found =
    if offset >= length then Ok (List.rev found)
    else
      match source.[offset] with
      | '\n' -> normal (offset + 1) (line + 1) found
      | '/' when offset + 1 < length && Char.equal source.[offset + 1] '/' ->
          scan Line_comment (offset + 2) line found
      | '/' when offset + 1 < length && Char.equal source.[offset + 1] '*' ->
          scan (Block_comment 1) (offset + 2) line found
      | '"' -> scan String_literal (offset + 1) line found
      | '\'' -> scan Char_literal (offset + 1) line found
      | byte when is_identifier_start byte ->
          let rec identifier_end cursor =
            if cursor < length && is_identifier_rest source.[cursor] then
              identifier_end (cursor + 1)
            else cursor
          in
          let last = identifier_end (offset + 1) in
          let name = String.sub source offset (last - offset) in
          let found =
            if starts_with ~prefix:"OPTf_" name || String.equal name "OPTF_ECHO"
            then (name, line) :: found
            else found
          in
          normal last line found
      | _ -> normal (offset + 1) line found
  and scan state offset line found =
    if offset >= length then
      match state with
      | Block_comment _ ->
          error ~path ~line
            "unterminated block comment while scanning option consumers"
      | String_literal ->
          error ~path ~line
            "unterminated string while scanning option consumers"
      | Char_literal ->
          error ~path ~line
            "unterminated character literal while scanning option consumers"
      | Line_comment -> Ok (List.rev found)
    else
      match state with
      | Line_comment ->
          if Char.equal source.[offset] '\n' then
            normal (offset + 1) (line + 1) found
          else scan Line_comment (offset + 1) line found
      | Block_comment depth ->
          if
            Char.equal source.[offset] '/'
            && offset + 1 < length
            && Char.equal source.[offset + 1] '*'
          then scan (Block_comment (depth + 1)) (offset + 2) line found
          else if
            Char.equal source.[offset] '*'
            && offset + 1 < length
            && Char.equal source.[offset + 1] '/'
          then
            if depth = 1 then normal (offset + 2) line found
            else scan (Block_comment (depth - 1)) (offset + 2) line found
          else if Char.equal source.[offset] '\n' then
            scan (Block_comment depth) (offset + 1) (line + 1) found
          else scan (Block_comment depth) (offset + 1) line found
      | (String_literal | Char_literal) as literal ->
          let delimiter = if literal = String_literal then '"' else '\'' in
          if Char.equal source.[offset] '\\' then
            if offset + 1 >= length then
              error ~path ~line
                "unfinished escape while scanning option consumers"
            else
              let next_line =
                if Char.equal source.[offset + 1] '\n' then line + 1 else line
              in
              scan literal (offset + 2) next_line found
          else if Char.equal source.[offset] delimiter then
            normal (offset + 1) line found
          else if Char.equal source.[offset] '\n' then
            error ~path ~line
              "literal crosses a line while scanning option consumers"
          else scan literal (offset + 1) line found
  in
  normal 0 1 []

let collect_consumers definitions default_line consumers =
  let canonical_name = function
    | "OPTF_ECHO" -> "OPTf_ECHO"
    | name -> name
  in
  let known name =
    List.exists
      (fun definition -> String.equal definition.name name)
      definitions
  in
  let rec scan_sources found = function
    | [] -> Ok (List.rev found)
    | (path, source) :: rest -> (
        match scan_option_identifiers ~path source with
        | Error _ as result -> result
        | Ok identifiers -> (
            let identifiers =
              List.map
                (fun (name, line) -> (canonical_name name, line))
                identifiers
            in
            match
              List.find_opt (fun (name, _) -> not (known name)) identifiers
            with
            | Some (name, line) ->
                error ~path ~line
                  (Printf.sprintf
                     "consumer references unknown compiler option %s" name)
            | None ->
                let references =
                  identifiers
                  |> List.filter (fun (_, line) ->
                      not
                        (String.equal path "Compiler/Lex.HC"
                        && line = default_line))
                  |> List.map (fun (name, line) -> (name, { path; line }))
                in
                scan_sources (List.rev_append references found) rest))
  in
  scan_sources [] consumers

let gaps definitions =
  let rec collect found = function
    | left :: (right :: _ as rest) ->
        let found =
          if left.bit_index + 1 < right.bit_index then
            { first = left.bit_index + 1; last = right.bit_index - 1 } :: found
          else found
        in
        collect found rest
    | [] | [ _ ] -> List.rev found
  in
  collect [] definitions

let parse ~kernel_source ~lex_source ~cmisc_source ~bequ_source ~consumers =
  match parse_kernel kernel_source with
  | Error _ as result -> result
  | Ok (definitions, echo_mask_line) -> (
      match parse_defaults definitions lex_source with
      | Error _ as result -> result
      | Ok (default_names, default_source_line) -> (
          match parse_api cmisc_source bequ_source with
          | Error _ as result -> result
          | Ok api -> (
              match
                collect_consumers definitions default_source_line consumers
              with
              | Error _ as result -> result
              | Ok references -> (
                  let options =
                    List.map
                      (fun definition ->
                        let consumers =
                          references
                          |> List.filter_map (fun (name, reference) ->
                              if String.equal name definition.name then
                                Some reference
                              else None)
                        in
                        {
                          name = definition.name;
                          bit_index = definition.bit_index;
                          initially_enabled =
                            List.mem definition.name default_names;
                          definition_line = definition.line;
                          source_comment = definition.comment;
                          consumers;
                        })
                      definitions
                  in
                  match
                    List.find_opt (fun option -> option.consumers = []) options
                  with
                  | Some option ->
                      error
                        (Printf.sprintf "no audited compiler consumer uses %s"
                           option.name)
                  | None ->
                      Ok
                        {
                          options;
                          gaps = gaps definitions;
                          default_source_line;
                          echo_mask_line;
                          api;
                        }))))
