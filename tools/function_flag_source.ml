type source_reference = { path : string; line : int }

type flag_entry = {
  name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

type group_entry = {
  name : string;
  mask : int64;
  members : string list;
  definition_line : int;
  consumers : source_reference list;
}

type transition_operation =
  | Add_bits of int64
  | Replace_preserving of { keep_mask : int64; add_mask : int64 }

type transition_entry = {
  name : string;
  spelling : string;
  operation : transition_operation;
  sources : source_reference list;
}

type behavior = {
  symbol_flag_transfer : source_reference;
  public_type_transfer : source_reference;
  automatic_ret1 : source_reference;
  variadic_declaration : source_reference;
  variadic_optimizer : source_reference;
  caller_cleanup : source_reference;
  try_cleanup : source_reference;
  internal_dispatch : source_reference;
  internal_clobber : source_reference;
  symbol_lookup_exclusion : source_reference;
  interrupt_restore : source_reference;
  interrupt_return : source_reference;
  interrupt_error_code : source_reference;
  callee_cleanup : source_reference;
  interrupt_save : source_reference;
}

type tables = {
  shared_flags : flag_entry list;
  function_flags : flag_entry list;
  staging_flags : flag_entry list;
  groups : group_entry list;
  transitions : transition_entry list;
  behavior : behavior;
}

type error = { path : string option; line : int option; message : string }

type definition = {
  name : string;
  expression : string;
  value : int64;
  line : int;
}

type expression_token =
  | Integer of int64
  | Identifier of string
  | Left_parenthesis
  | Right_parenthesis
  | Bit_or
  | Shift_left

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

let unique_offset ~path ~description ~needle text =
  match find_offsets ~needle text with
  | [ offset ] -> Ok offset
  | [] -> error ~path (Printf.sprintf "%s is missing" description)
  | _ -> error ~path (Printf.sprintf "%s appears more than once" description)

let line_of_offset text target =
  let rec count offset line =
    if offset >= target then line
    else if Char.equal text.[offset] '\n' then count (offset + 1) (line + 1)
    else count (offset + 1) line
  in
  count 0 1

let is_identifier_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_identifier_rest = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' -> true
  | _ -> false

let is_hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

let tokenize_expression ~path ~line expression =
  let length = String.length expression in
  let rec scan offset found =
    if offset = length then Ok (Array.of_list (List.rev found))
    else
      match expression.[offset] with
      | ' ' | '\t' -> scan (offset + 1) found
      | '(' -> scan (offset + 1) (Left_parenthesis :: found)
      | ')' -> scan (offset + 1) (Right_parenthesis :: found)
      | '|' -> scan (offset + 1) (Bit_or :: found)
      | '<'
        when offset + 1 < length && Char.equal expression.[offset + 1] '<' ->
          scan (offset + 2) (Shift_left :: found)
      | ('0' .. '9' as first) ->
          let hexadecimal =
            Char.equal first '0'
            && offset + 1 < length
            && (Char.equal expression.[offset + 1] 'x'
               || Char.equal expression.[offset + 1] 'X')
          in
          let start = if hexadecimal then offset + 2 else offset in
          let valid = if hexadecimal then is_hex_digit else function '0' .. '9' -> true | _ -> false in
          let rec finish cursor =
            if cursor < length && valid expression.[cursor] then
              finish (cursor + 1)
            else cursor
          in
          let last = finish start in
          if last = start then
            error ~path ~line
              (Printf.sprintf "invalid integer in flag expression %S" expression)
          else
            let spelling = String.sub expression offset (last - offset) in
            (match Int64.of_string_opt spelling with
            | Some value -> scan last (Integer value :: found)
            | None ->
                error ~path ~line
                  (Printf.sprintf "integer %S does not fit in 64 bits" spelling))
      | byte when is_identifier_start byte ->
          let rec finish cursor =
            if cursor < length && is_identifier_rest expression.[cursor] then
              finish (cursor + 1)
            else cursor
          in
          let last = finish (offset + 1) in
          let name = String.sub expression offset (last - offset) in
          scan last (Identifier name :: found)
      | byte ->
          error ~path ~line
            (Printf.sprintf "unsupported %C in flag expression %S" byte expression)
  in
  scan 0 []

let evaluate_expression ~path ~line ~environment expression =
  match tokenize_expression ~path ~line expression with
  | Error _ as result -> result
  | Ok tokens ->
      let length = Array.length tokens in
      let rec parse_or offset =
        match parse_shift offset with
        | Error _ as result -> result
        | Ok (left, offset) -> parse_or_tail left offset
      and parse_or_tail left offset =
        if offset < length && tokens.(offset) = Bit_or then
          match parse_shift (offset + 1) with
          | Error _ as result -> result
          | Ok (right, offset) ->
              parse_or_tail (Int64.logor left right) offset
        else Ok (left, offset)
      and parse_shift offset =
        match parse_atom offset with
        | Error _ as result -> result
        | Ok (left, offset) ->
            if offset < length && tokens.(offset) = Shift_left then
              match parse_atom (offset + 1) with
              | Error _ as result -> result
              | Ok (right, offset) ->
                  if Int64.compare right 0L < 0 || Int64.compare right 63L > 0
                  then error ~path ~line "flag shift must be between 0 and 63"
                  else
                    Ok
                      ( Int64.shift_left left (Int64.to_int right),
                        offset )
            else Ok (left, offset)
      and parse_atom offset =
        if offset >= length then error ~path ~line "flag expression ends early"
        else
          match tokens.(offset) with
          | Integer value -> Ok (value, offset + 1)
          | Identifier name -> (
              match List.assoc_opt name environment with
              | Some value -> Ok (value, offset + 1)
              | None ->
                  error ~path ~line
                    (Printf.sprintf "flag expression references unknown %s" name))
          | Left_parenthesis -> (
              match parse_or (offset + 1) with
              | Error _ as result -> result
              | Ok (value, offset) ->
                  if offset < length && tokens.(offset) = Right_parenthesis then
                    Ok (value, offset + 1)
                  else error ~path ~line "flag expression is missing ')'" )
          | Right_parenthesis | Bit_or | Shift_left ->
              error ~path ~line "flag expression has an operator where a value is required"
      in
      if length = 0 then error ~path ~line "flag expression is empty"
      else
        match parse_or 0 with
        | Error _ as result -> result
        | Ok (value, offset) when offset = length -> Ok value
        | Ok _ -> error ~path ~line "flag expression has trailing input"

let split_comment line =
  match find_offsets ~needle:"//" line with
  | [] -> line
  | offset :: _ -> String.sub line 0 offset

let compact_expression expression =
  let buffer = Buffer.create (String.length expression) in
  String.iter
    (function
      | ' ' | '\t' -> ()
      | byte -> Buffer.add_char buffer byte)
    expression;
  Buffer.contents buffer

let parse_define line_number line =
  let line = split_comment line |> String.trim in
  let prefix = "#define" in
  if not (starts_with ~prefix line) then None
  else
    let length = String.length line in
    let rec skip cursor =
      if cursor < length && (Char.equal line.[cursor] ' ' || Char.equal line.[cursor] '\t') then
        skip (cursor + 1)
      else cursor
    in
    let name_start = skip (String.length prefix) in
    let rec name_end cursor =
      if cursor < length && is_identifier_rest line.[cursor] then
        name_end (cursor + 1)
      else cursor
    in
    let name_last = name_end name_start in
    if name_last = name_start then None
    else
      let expression_start = skip name_last in
      let name = String.sub line name_start (name_last - name_start) in
      let expression =
        String.sub line expression_start (length - expression_start)
        |> String.trim
      in
      Some (name, expression, line_number)

let relevant_kernel_name name =
  starts_with ~prefix:"Cf_" name || starts_with ~prefix:"Ff_" name

let relevant_compiler_name name =
  starts_with ~prefix:"FSF_" name || starts_with ~prefix:"FSG_FUN_" name

let collect_definitions ~path ~relevant ~environment source =
  let lines =
    normalize_checkout_line_endings source |> String.split_on_char '\n'
  in
  let rec collect line_number environment definitions = function
    | [] -> Ok (List.rev definitions)
    | line :: rest -> (
        match parse_define line_number line with
        | Some (name, expression, line) when relevant name ->
            if List.mem_assoc name environment then
              error ~path ~line (Printf.sprintf "%s is defined more than once" name)
            else (
              match evaluate_expression ~path ~line ~environment expression with
              | Error _ as result -> result
              | Ok value ->
                  let definition = { name; expression; value; line } in
                  collect (line_number + 1) ((name, value) :: environment)
                    (definition :: definitions) rest)
        | _ -> collect (line_number + 1) environment definitions rest)
  in
  collect 1 environment [] lines

let expected_shared = [ ("Cf_EXTERN", 0); ("Cf_INTERNAL_TYPE", 1) ]

let expected_function =
  [
    ("Ff_INTERRUPT", 8);
    ("Ff_HASERRCODE", 9);
    ("Ff_ARGPOP", 10);
    ("Ff_NOARGPOP", 11);
    ("Ff_INTERNAL", 12);
    ("Ff__EXTERN", 13);
    ("Ff_DOT_DOT_DOT", 14);
    ("Ff_RET1", 15);
  ]

let expected_staging =
  [
    ("FSF_PUBLIC", 0x001L);
    ("FSF_ASM", 0x002L);
    ("FSF_STATIC", 0x004L);
    ("FSF__", 0x008L);
    ("FSF_INTERRUPT", 0x100L);
    ("FSF_HASERRCODE", 0x200L);
    ("FSF_ARGPOP", 0x400L);
    ("FSF_NOARGPOP", 0x800L);
  ]

let expected_groups =
  [
    ( "FSG_FUN_FLAGS1",
      0xF00L,
      [ "FSF_INTERRUPT"; "FSF_HASERRCODE"; "FSF_ARGPOP"; "FSF_NOARGPOP" ] );
    ("FSG_FUN_FLAGS2", 0xF01L, [ "FSG_FUN_FLAGS1"; "FSF_PUBLIC" ]);
  ]

let expected_kernel_expressions =
  [
    ("Cf_EXTERN", "0");
    ("Cf_INTERNAL_TYPE", "1");
    ("Ff_INTERRUPT", "8");
    ("Ff_HASERRCODE", "9");
    ("Ff_ARGPOP", "10");
    ("Ff_NOARGPOP", "11");
    ("Ff_INTERNAL", "12");
    ("Ff__EXTERN", "13");
    ("Ff_DOT_DOT_DOT", "14");
    ("Ff_RET1", "15");
  ]

let expected_compiler_expressions =
  [
    ("FSF_PUBLIC", "0x01");
    ("FSF_ASM", "0x02");
    ("FSF_STATIC", "0x04");
    ("FSF__", "0x08");
    ("FSF_INTERRUPT", "(1<<Ff_INTERRUPT)");
    ("FSF_HASERRCODE", "(1<<Ff_HASERRCODE)");
    ("FSF_ARGPOP", "(1<<Ff_ARGPOP)");
    ("FSF_NOARGPOP", "(1<<Ff_NOARGPOP)");
    ( "FSG_FUN_FLAGS1",
      "(FSF_INTERRUPT|FSF_HASERRCODE|FSF_ARGPOP|FSF_NOARGPOP)" );
    ("FSG_FUN_FLAGS2", "(FSG_FUN_FLAGS1|FSF_PUBLIC)");
  ]

let validate_expression_forms ~path expected definitions =
  let rec check expected definitions =
    match (expected, definitions) with
    | [], [] -> Ok ()
    | (name, _) :: _, [] ->
        error ~path (Printf.sprintf "function flag table is missing %s" name)
    | [], definition :: _ ->
        error ~path ~line:definition.line
          (Printf.sprintf "function flag table contains unexpected %s" definition.name)
    | (name, expression) :: expected, definition :: definitions ->
        if not (String.equal name definition.name) then
          error ~path ~line:definition.line
            (Printf.sprintf "function flag table requires %s here, but found %s"
               name definition.name)
        else if
          not
            (String.equal (compact_expression expression)
               (compact_expression definition.expression))
        then
          error ~path ~line:definition.line
            (Printf.sprintf "%s must retain source expression %S" name expression)
        else check expected definitions
  in
  check expected definitions

let validate_exact_definitions ~path expected definitions =
  let rec compare expected definitions =
    match (expected, definitions) with
    | [], [] -> Ok ()
    | (name, _) :: _, [] ->
        error ~path (Printf.sprintf "function flag table is missing %s" name)
    | [], definition :: _ ->
        error ~path ~line:definition.line
          (Printf.sprintf "function flag table contains unexpected %s" definition.name)
    | (name, value) :: expected, definition :: definitions ->
        if not (String.equal name definition.name) then
          error ~path ~line:definition.line
            (Printf.sprintf "function flag table requires %s here, but found %s"
               name definition.name)
        else if definition.value <> Int64.of_int value then
          error ~path ~line:definition.line
            (Printf.sprintf "%s must remain bit index %d" name value)
        else compare expected definitions
  in
  compare expected definitions

let validate_exact_masks ~path expected definitions =
  let rec compare expected definitions =
    match (expected, definitions) with
    | [], [] -> Ok ()
    | (name, _) :: _, [] ->
        error ~path (Printf.sprintf "parser flag table is missing %s" name)
    | [], definition :: _ ->
        error ~path ~line:definition.line
          (Printf.sprintf "parser flag table contains unexpected %s" definition.name)
    | (name, value) :: expected, definition :: definitions ->
        if not (String.equal name definition.name) then
          error ~path ~line:definition.line
            (Printf.sprintf "parser flag table requires %s here, but found %s"
               name definition.name)
        else if definition.value <> value then
          error ~path ~line:definition.line
            (Printf.sprintf "%s evaluates to 0x%Lx, expected 0x%Lx" name
               definition.value value)
        else compare expected definitions
  in
  compare expected definitions

let bit_index_of_mask ~path definition =
  let mask = definition.value in
  if Int64.compare mask 0L <= 0 || Int64.logand mask (Int64.pred mask) <> 0L then
    error ~path ~line:definition.line
      (Printf.sprintf "%s must evaluate to one bit" definition.name)
  else
    let rec find bit value =
      if Int64.equal value 1L then bit
      else find (bit + 1) (Int64.shift_right_logical value 1)
    in
    Ok (find 0 mask)

let compact source =
  let buffer = Buffer.create (String.length source) in
  String.iter
    (function
      | ' ' | '\t' | '\r' | '\n' -> ()
      | byte -> Buffer.add_char buffer byte)
    source;
  Buffer.contents buffer

let require_compact ~path ~anchor ~snippet source =
  let normalized = normalize_checkout_line_endings source in
  let compacted = compact normalized in
  match find_offsets ~needle:snippet compacted with
  | [] -> error ~path (Printf.sprintf "required source behavior near %S is missing" anchor)
  | _ :: _ :: _ ->
      error ~path (Printf.sprintf "required source behavior near %S is ambiguous" anchor)
  | [ _ ] -> (
      match find_offsets ~needle:anchor normalized with
      | [ offset ] -> Ok { path; line = line_of_offset normalized offset }
      | [] -> error ~path (Printf.sprintf "source anchor %S is missing" anchor)
      | _ -> error ~path (Printf.sprintf "source anchor %S appears more than once" anchor))

let require_line_occurrences ~path ~line_text ~count source =
  let normalized = normalize_checkout_line_endings source in
  let lines = String.split_on_char '\n' normalized in
  let found =
    List.mapi
      (fun index line ->
        if String.equal (String.trim line) line_text then
          Some { path; line = index + 1 }
        else None)
      lines
    |> List.filter_map Fun.id
  in
  if List.length found = count then Ok found
  else
    error ~path
      (Printf.sprintf "%S must appear on exactly %d source lines" line_text count)

type scan_state =
  | Line_comment
  | Block_comment of int
  | String_literal
  | Character_literal

let relevant_identifier name =
  starts_with ~prefix:"Cf_" name
  || starts_with ~prefix:"Ff_" name
  || starts_with ~prefix:"FSF_" name
  || starts_with ~prefix:"FSG_FUN_" name

let scan_consumers ~known ~path source =
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
      | '\'' -> scan Character_literal (offset + 1) line found
      | byte when is_identifier_start byte ->
          let rec finish cursor =
            if cursor < length && is_identifier_rest source.[cursor] then
              finish (cursor + 1)
            else cursor
          in
          let last = finish (offset + 1) in
          let name = String.sub source offset (last - offset) in
          if relevant_identifier name then
            if List.mem name known then
              normal last line ((name, { path; line }) :: found)
            else
              error ~path ~line
                (Printf.sprintf "source uses unknown function flag %s" name)
          else normal last line found
      | _ -> normal (offset + 1) line found
  and scan state offset line found =
    if offset >= length then
      match state with
      | Line_comment -> Ok (List.rev found)
      | Block_comment _ -> error ~path ~line "unterminated block comment while scanning function flags"
      | String_literal -> error ~path ~line "unterminated string while scanning function flags"
      | Character_literal -> error ~path ~line "unterminated character literal while scanning function flags"
    else
      match state with
      | Line_comment ->
          if Char.equal source.[offset] '\n' then normal (offset + 1) (line + 1) found
          else scan Line_comment (offset + 1) line found
      | Block_comment depth ->
          if Char.equal source.[offset] '/' && offset + 1 < length && Char.equal source.[offset + 1] '*' then
            scan (Block_comment (depth + 1)) (offset + 2) line found
          else if Char.equal source.[offset] '*' && offset + 1 < length && Char.equal source.[offset + 1] '/' then
            if depth = 1 then normal (offset + 2) line found
            else scan (Block_comment (depth - 1)) (offset + 2) line found
          else if Char.equal source.[offset] '\n' then
            scan (Block_comment depth) (offset + 1) (line + 1) found
          else scan (Block_comment depth) (offset + 1) line found
      | String_literal | Character_literal as literal ->
          let terminator = match literal with String_literal -> '"' | _ -> '\'' in
          if Char.equal source.[offset] '\\' && offset + 1 < length then
            scan literal (offset + 2) line found
          else if Char.equal source.[offset] terminator then
            normal (offset + 1) line found
          else if Char.equal source.[offset] '\n' then
            error ~path ~line "literal crosses a line while scanning function flags"
          else scan literal (offset + 1) line found
  in
  normal 0 1 []

let consumers_for name references =
  List.filter_map
    (fun (found_name, reference) ->
      if String.equal name found_name then Some reference else None)
    references

let apply_transition transition mask =
  match transition.operation with
  | Add_bits bits -> Int64.logor mask bits
  | Replace_preserving { keep_mask; add_mask } ->
      Int64.logor (Int64.logand mask keep_mask) add_mask

let parse ~kernel_source ~compiler_source ~prs_stmt_source ~prs_var_source
    ~prs_exp_source ~opt_pass3_source ~opt_pass6_source ~opt_pass789a_source
    ~fun_seg_source =
  let ( let* ) result continuation = Result.bind result continuation in
  let kernel_path = "Kernel/KernelA.HH" in
  let compiler_path = "Compiler/CompilerA.HH" in
  let* kernel_definitions =
    collect_definitions ~path:kernel_path ~relevant:relevant_kernel_name
      ~environment:[] kernel_source
  in
  let shared_definitions = List.filteri (fun index _ -> index < 2) kernel_definitions in
  let function_definitions = List.filteri (fun index _ -> index >= 2) kernel_definitions in
  let* () = validate_exact_definitions ~path:kernel_path expected_shared shared_definitions in
  let* () = validate_exact_definitions ~path:kernel_path expected_function function_definitions in
  let* () =
    validate_expression_forms ~path:kernel_path expected_kernel_expressions
      kernel_definitions
  in
  let kernel_compact = compact (normalize_checkout_line_endings kernel_source) in
  let* () =
    let class_marker = "publicclassCHashClass:CHashSrcSym{" in
    let function_marker = "publicclassCHashFun:CHashClass{" in
    let* class_offset =
      unique_offset ~path:kernel_path ~description:"CHashClass declaration"
        ~needle:class_marker kernel_compact
    in
    let* function_offset =
      unique_offset ~path:kernel_path ~description:"CHashFun declaration"
        ~needle:function_marker kernel_compact
    in
    if function_offset <= class_offset then
      error ~path:kernel_path "CHashFun must follow and inherit CHashClass"
    else
      let class_section =
        String.sub kernel_compact class_offset (function_offset - class_offset)
      in
      if contains ~needle:"U16flags;" class_section then Ok ()
      else error ~path:kernel_path "CHashClass must retain its U16 flag field"
  in
  let kernel_environment = List.map (fun item -> (item.name, item.value)) kernel_definitions in
  let* compiler_definitions =
    collect_definitions ~path:compiler_path ~relevant:relevant_compiler_name
      ~environment:kernel_environment compiler_source
  in
  let staging_definitions = List.filteri (fun index _ -> index < 8) compiler_definitions in
  let group_definitions = List.filteri (fun index _ -> index >= 8) compiler_definitions in
  let* () = validate_exact_masks ~path:compiler_path expected_staging staging_definitions in
  let* () =
    validate_exact_masks ~path:compiler_path
      (List.map (fun (name, value, _) -> (name, value)) expected_groups)
      group_definitions
  in
  let* () =
    validate_expression_forms ~path:compiler_path expected_compiler_expressions
      compiler_definitions
  in
  let known = List.map (fun item -> item.name) (kernel_definitions @ compiler_definitions) in
  let consumer_sources =
    [
      ("Compiler/PrsStmt.HC", prs_stmt_source);
      ("Compiler/PrsVar.HC", prs_var_source);
      ("Compiler/PrsExp.HC", prs_exp_source);
      ("Compiler/OptPass3.HC", opt_pass3_source);
      ("Compiler/OptPass6.HC", opt_pass6_source);
      ("Compiler/OptPass789A.HC", opt_pass789a_source);
      ("Kernel/FunSeg.HC", fun_seg_source);
    ]
  in
  let rec scan_all found = function
    | [] -> Ok found
    | (path, source) :: rest ->
        let* references = scan_consumers ~known ~path source in
        scan_all (found @ references) rest
  in
  let* references = scan_all [] consumer_sources in
  let flag_from_index definition =
    let bit_index = Int64.to_int definition.value in
    {
      name = definition.name;
      bit_index;
      mask = Int64.shift_left 1L bit_index;
      definition_line = definition.line;
      consumers = consumers_for definition.name references;
    }
  in
  let rec staging_entries found = function
    | [] -> Ok (List.rev found)
    | definition :: rest ->
        let* bit_index = bit_index_of_mask ~path:compiler_path definition in
        let entry =
          {
            name = definition.name;
            bit_index;
            mask = definition.value;
            definition_line = definition.line;
            consumers = consumers_for definition.name references;
          }
        in
        staging_entries (entry :: found) rest
  in
  let* staging_flags = staging_entries [] staging_definitions in
  let groups =
    List.map2
      (fun definition (_, _, members) ->
        {
          name = definition.name;
          mask = definition.value;
          members;
          definition_line = definition.line;
          consumers = consumers_for definition.name references;
        })
      group_definitions expected_groups
  in
  let keep_function_and_public_and_asm = 0xF03L in
  let transition_specs =
    [
      ( "Static",
        "static",
        Replace_preserving { keep_mask = 0x002L; add_mask = 0x004L },
        "case KW_STATIC:",
        "caseKW_STATIC:fsp_flags=FSF_STATIC|fsp_flags&FSF_ASM;" );
      ( "Interrupt",
        "interrupt",
        Replace_preserving
          { keep_mask = keep_function_and_public_and_asm; add_mask = 0x900L },
        "case KW_INTERRUPT:",
        "caseKW_INTERRUPT:fsp_flags=FSF_INTERRUPT|FSF_NOARGPOP|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);" );
      ( "Has_error_code",
        "haserrcode",
        Replace_preserving
          { keep_mask = keep_function_and_public_and_asm; add_mask = 0x200L },
        "case KW_HASERRCODE:",
        "caseKW_HASERRCODE:fsp_flags=FSF_HASERRCODE|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);" );
      ( "Argument_pop",
        "argpop",
        Replace_preserving
          { keep_mask = keep_function_and_public_and_asm; add_mask = 0x400L },
        "case KW_ARGPOP:",
        "caseKW_ARGPOP:fsp_flags=FSF_ARGPOP|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);" );
      ( "No_argument_pop",
        "noargpop",
        Replace_preserving
          { keep_mask = keep_function_and_public_and_asm; add_mask = 0x800L },
        "case KW_NOARGPOP:",
        "caseKW_NOARGPOP:fsp_flags=FSF_NOARGPOP|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);" );
      ( "Public",
        "public",
        Replace_preserving
          { keep_mask = keep_function_and_public_and_asm; add_mask = 0x001L },
        "case KW_PUBLIC:",
        "caseKW_PUBLIC:fsp_flags=FSF_PUBLIC|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);" );
    ]
  in
  let rec transitions found = function
    | [] -> Ok (List.rev found)
    | (name, spelling, operation, anchor, snippet) :: rest ->
        let* source =
          require_compact ~path:"Compiler/PrsStmt.HC" ~anchor ~snippet
            prs_stmt_source
        in
        transitions ({ name; spelling; operation; sources = [ source ] } :: found) rest
  in
  let* transitions = transitions [] transition_specs in
  let* underscore_sources =
    require_line_occurrences ~path:"Compiler/PrsStmt.HC"
      ~line_text:"fsp_flags|=FSF__;" ~count:2 prs_stmt_source
  in
  let transitions =
    transitions
    @ [
        {
          name = "Underscore_name";
          spelling = "leading underscore in _extern or _import";
          operation = Add_bits 0x008L;
          sources = underscore_sources;
        };
      ]
  in
  let behavior_ref ~path ~anchor ~snippet source =
    require_compact ~path ~anchor ~snippet source
  in
  let* symbol_flag_transfer =
    behavior_ref ~path:"Compiler/PrsStmt.HC"
      ~anchor:"tmpf->flags|=fsp_flags&FSG_FUN_FLAGS1;"
      ~snippet:"tmpf->flags|=fsp_flags&FSG_FUN_FLAGS1;" prs_stmt_source
  in
  let* public_type_transfer =
    behavior_ref ~path:"Compiler/PrsStmt.HC"
      ~anchor:"BEqu(&tmpf->type,HTf_PUBLIC,fsp_flags&FSF_PUBLIC);"
      ~snippet:"BEqu(&tmpf->type,HTf_PUBLIC,fsp_flags&FSF_PUBLIC);"
      prs_stmt_source
  in
  let* automatic_ret1 =
    behavior_ref ~path:"Compiler/PrsStmt.HC"
      ~anchor:"if (0<tmpf->arg_cnt<<3<=I16_MAX"
      ~snippet:"if(0<tmpf->arg_cnt<<3<=I16_MAX&&!Bt(&tmpf->flags,Ff_DOT_DOT_DOT))LBts(&tmpf->flags,Ff_RET1);"
      prs_stmt_source
  in
  let* variadic_declaration =
    behavior_ref ~path:"Compiler/PrsVar.HC"
      ~anchor:"Bts(&tmpf->flags,Ff_DOT_DOT_DOT);"
      ~snippet:"Bts(&tmpf->flags,Ff_DOT_DOT_DOT);" prs_var_source
  in
  let* variadic_optimizer =
    behavior_ref ~path:"Compiler/OptPass3.HC"
      ~anchor:"if (Bt(&cc->htc.fun->flags,Ff_DOT_DOT_DOT))"
      ~snippet:"if(Bt(&cc->htc.fun->flags,Ff_DOT_DOT_DOT))member_cnt+=2;"
      opt_pass3_source
  in
  let* caller_cleanup =
    behavior_ref ~path:"Compiler/PrsExp.HC"
      ~anchor:"if ((Bt(&tmpf->flags,Ff_RET1)"
      ~snippet:"if((Bt(&tmpf->flags,Ff_RET1)||Bt(&tmpf->flags,Ff_ARGPOP))&&!Bt(&tmpf->flags,Ff_NOARGPOP)){"
      prs_exp_source
  in
  let* try_cleanup =
    behavior_ref ~path:"Compiler/PrsStmt.HC"
      ~anchor:"if ((Bt(&tmp_try->flags,Ff_RET1)"
      ~snippet:"if((Bt(&tmp_try->flags,Ff_RET1)||Bt(&tmp_try->flags,Ff_ARGPOP))&&!Bt(&tmp_try->flags,Ff_NOARGPOP))"
      prs_stmt_source
  in
  let* internal_dispatch =
    behavior_ref ~path:"Compiler/PrsExp.HC"
      ~anchor:"if (Bt(&tmpf->flags,Ff_INTERNAL))"
      ~snippet:"if(Bt(&tmpf->flags,Ff_INTERNAL))ICAdd(cc,tmpf->exe_addr,0,tmpf->return_class);"
      prs_exp_source
  in
  let* internal_clobber =
    behavior_ref ~path:"Compiler/OptPass6.HC"
      ~anchor:"if (Bt(&tmpf->flags,Ff_INTERNAL))"
      ~snippet:"if(Bt(&tmpf->flags,Ff_INTERNAL))clobbered_stk_tmp_mask=0;"
      opt_pass6_source
  in
  let* symbol_lookup_exclusion =
    behavior_ref ~path:"Kernel/FunSeg.HC"
      ~anchor:"!Bt(&tmpex(CHashFun *)->flags,Ff_INTERNAL))"
      ~snippet:"if(!Bt(&tmpex(CHashFun*)->flags,Cf_EXTERN)&&!Bt(&tmpex(CHashFun*)->flags,Ff_INTERNAL))"
      fun_seg_source
  in
  let* interrupt_restore =
    behavior_ref ~path:"Compiler/OptPass789A.HC"
      ~anchor:"ICPopRegs(tmpi,REGG_CLOBBERED"
      ~snippet:"if(Bt(&cc->htc.fun->flags,Ff_INTERRUPT))ICPopRegs("
      opt_pass789a_source
  in
  let* interrupt_return =
    behavior_ref ~path:"Compiler/OptPass789A.HC"
      ~anchor:"if (cc->htc.fun && Bt(&cc->htc.fun->flags,Ff_INTERRUPT)) {"
      ~snippet:"if(cc->htc.fun&&Bt(&cc->htc.fun->flags,Ff_INTERRUPT)){if(Bt(&cc->htc.fun->flags,Ff_HASERRCODE))"
      opt_pass789a_source
  in
  let* interrupt_error_code =
    behavior_ref ~path:"Compiler/OptPass789A.HC"
      ~anchor:"if (Bt(&cc->htc.fun->flags,Ff_HASERRCODE))"
      ~snippet:"if(Bt(&cc->htc.fun->flags,Ff_HASERRCODE))ICAddRSP(tmpi,8);"
      opt_pass789a_source
  in
  let* callee_cleanup =
    behavior_ref ~path:"Compiler/OptPass789A.HC"
      ~anchor:"(Bt(&cc->htc.fun->flags,Ff_RET1) ||"
      ~snippet:"(Bt(&cc->htc.fun->flags,Ff_RET1)||Bt(&cc->htc.fun->flags,Ff_ARGPOP))&&!Bt(&cc->htc.fun->flags,Ff_NOARGPOP)){ICU8(tmpi,0xC2);ICU16(tmpi,cc->htc.fun->arg_cnt<<3);"
      opt_pass789a_source
  in
  let* interrupt_save =
    behavior_ref ~path:"Compiler/OptPass789A.HC"
      ~anchor:"ICPushRegs(tmpi,REGG_CLOBBERED"
      ~snippet:"if(Bt(&cc->htc.fun->flags,Ff_INTERRUPT))ICPushRegs("
      opt_pass789a_source
  in
  Ok
    {
      shared_flags = List.map flag_from_index shared_definitions;
      function_flags = List.map flag_from_index function_definitions;
      staging_flags;
      groups;
      transitions;
      behavior =
        {
          symbol_flag_transfer;
          public_type_transfer;
          automatic_ret1;
          variadic_declaration;
          variadic_optimizer;
          caller_cleanup;
          try_cleanup;
          internal_dispatch;
          internal_clobber;
          symbol_lookup_exclusion;
          interrupt_restore;
          interrupt_return;
          interrupt_error_code;
          callee_cleanup;
          interrupt_save;
        };
    }
