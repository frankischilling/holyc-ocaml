type source_reference = { path : string; line : int }

type flag_entry = {
  source_name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

type behavior_entry = {
  id : string;
  description : string;
  source : source_reference;
}

type tables = { flags : flag_entry list; behaviors : behavior_entry list }
type error = { path : string option; line : int option; message : string }
type definition = { name : string; value : int64; line : int }

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

let split_comment line =
  match find_offsets ~needle:"//" line with
  | [] -> line
  | offset :: _ -> String.sub line 0 offset

let is_identifier_byte = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' -> true
  | _ -> false

let parse_define line_number line =
  let line = split_comment line |> String.trim in
  let prefix = "#define" in
  if not (starts_with ~prefix line) then None
  else
    let length = String.length line in
    let rec skip cursor =
      if
        cursor < length
        && (Char.equal line.[cursor] ' ' || Char.equal line.[cursor] '\t')
      then skip (cursor + 1)
      else cursor
    in
    let name_start = skip (String.length prefix) in
    let rec name_end cursor =
      if cursor < length && is_identifier_byte line.[cursor] then
        name_end (cursor + 1)
      else cursor
    in
    let name_limit = name_end name_start in
    if name_limit = name_start then None
    else
      let value_start = skip name_limit in
      let name = String.sub line name_start (name_limit - name_start) in
      let value =
        String.sub line value_start (length - value_start) |> String.trim
      in
      Some (name, value, line_number)

let parse_literal ~path ~line name spelling =
  match Int64.of_string_opt spelling with
  | Some value -> Ok { name; value; line }
  | None ->
      error ~path ~line
        (Printf.sprintf "%s must retain a direct integer literal, found %S" name
           spelling)

let collect_definitions ~path source =
  let lines =
    normalize_checkout_line_endings source |> String.split_on_char '\n'
  in
  let rec collect line_number found = function
    | [] -> Ok (List.rev found)
    | line :: rest -> (
        match parse_define line_number line with
        | Some (name, spelling, line) when starts_with ~prefix:"MLF_" name -> (
            match parse_literal ~path ~line name spelling with
            | Error _ as result -> result
            | Ok definition ->
                collect (line_number + 1) (definition :: found) rest)
        | _ -> collect (line_number + 1) found rest)
  in
  collect 1 [] lines

let expected_flags =
  [
    ("MLF_DFT_AVAILABLE", 1L);
    ("MLF_LASTCLASS", 2L);
    ("MLF_STR_DFT_AVAILABLE", 4L);
    ("MLF_FUN", 8L);
    ("MLF_DOT_DOT_DOT", 16L);
    ("MLF_NO_UNUSED_WARN", 32L);
    ("MLF_STATIC", 64L);
  ]

let validate_definitions ~path actual =
  let rec check expected actual =
    match (expected, actual) with
    | [], [] -> Ok ()
    | (name, _) :: _, [] ->
        error ~path (Printf.sprintf "member-list flag table is missing %s" name)
    | [], definition :: _ ->
        error ~path ~line:definition.line
          (Printf.sprintf "member-list flag table contains unexpected %s"
             definition.name)
    | (name, value) :: expected_rest, definition :: actual_rest ->
        if not (String.equal name definition.name) then
          error ~path ~line:definition.line
            (Printf.sprintf "member-list flag table requires %s here, found %s"
               name definition.name)
        else if not (Int64.equal value definition.value) then
          error ~path ~line:definition.line
            (Printf.sprintf "%s evaluates to 0x%Lx, expected 0x%Lx" name
               definition.value value)
        else check expected_rest actual_rest
  in
  check expected_flags actual

let compact source =
  let buffer = Buffer.create (String.length source) in
  String.iter
    (function
      | ' ' | '\t' | '\r' | '\n' -> ()
      | byte -> Buffer.add_char buffer byte)
    source;
  Buffer.contents buffer

type behavior_spec = {
  id : string;
  description : string;
  path : string;
  anchor : string;
  snippet : string;
}

let require_behavior sources spec =
  match List.assoc_opt spec.path sources with
  | None -> error (Printf.sprintf "internal source %s is unavailable" spec.path)
  | Some source -> (
      let normalized = normalize_checkout_line_endings source in
      let compacted = compact normalized in
      let compact_snippet = compact spec.snippet in
      let snippet_offsets = find_offsets ~needle:compact_snippet compacted in
      let anchor_offsets = find_offsets ~needle:spec.anchor normalized in
      if snippet_offsets = [] then
        error ~path:spec.path
          (Printf.sprintf "required member-list behavior %S is missing" spec.id)
      else if List.length snippet_offsets > 1 then
        error ~path:spec.path
          (Printf.sprintf "required member-list behavior %S is ambiguous"
             spec.id)
      else
        match anchor_offsets with
        | [ offset ] ->
            Ok
              {
                id = spec.id;
                description = spec.description;
                source =
                  { path = spec.path; line = line_of_offset normalized offset };
              }
        | [] ->
            error ~path:spec.path
              (Printf.sprintf "source anchor %S is missing" spec.anchor)
        | _ ->
            error ~path:spec.path
              (Printf.sprintf "source anchor %S is ambiguous" spec.anchor))

let behavior_specs =
  [
    {
      id = "anonymous-slot-no-warning";
      description = "an unnamed argument slot suppresses its unused warning";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->flags|=MLF_NO_UNUSED_WARN;";
      snippet = "tmpm->flags|=MLF_NO_UNUSED_WARN;";
    };
    {
      id = "varargs-argc";
      description = "the synthesized argc slot carries the variadic marker";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->str=StrNew(\"argc\");";
      snippet =
        "tmpm->flags=MLF_DOT_DOT_DOT; \
         tmpm->member_class=cmp.internal_types[RT_I64]; \
         tmpm->str=StrNew(\"argc\");";
    };
    {
      id = "varargs-argv";
      description = "the synthesized argv slot carries the variadic marker";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->str=StrNew(\"argv\");";
      snippet =
        "tmpm->flags=MLF_DOT_DOT_DOT; \
         tmpm->member_class=cmp.internal_types[RT_I64]; \
         tmpm->str=StrNew(\"argv\");";
    };
    {
      id = "static-assignment";
      description = "a static local member record receives MLF_STATIC";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->flags|=MLF_STATIC;";
      snippet =
        "if (mode.u8[1]==PRS1B_STATIC_LOCAL_VAR) { tmpm->flags|=MLF_STATIC; \
         tmpm->reg=REG_NONE; }";
    };
    {
      id = "callback-assignment";
      description = "a returned callback record receives MLF_FUN";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->flags|=MLF_FUN;";
      snippet = "if (tmpm->fun_ptr) tmpm->flags|=MLF_FUN;";
    };
    {
      id = "lastclass-assignment";
      description = "a lastclass default receives MLF_LASTCLASS";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->flags|=MLF_LASTCLASS;";
      snippet =
        "if (PrsKeyWord(cc)==KW_LASTCLASS) { tmpm->flags|=MLF_LASTCLASS; \
         Lex(cc); }";
    };
    {
      id = "string-default-assignment";
      description = "a string-backed default receives its ownership marker";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->flags|=MLF_STR_DFT_AVAILABLE;";
      snippet =
        "if (cc->flags & CCF_HAS_MISC_DATA) { \
         tmpm->dft_val=StrNew(tmpm->dft_val); \
         tmpm->flags|=MLF_STR_DFT_AVAILABLE; }";
    };
    {
      id = "default-assignment";
      description =
        "every accepted argument default receives its availability marker";
      path = "Compiler/PrsVar.HC";
      anchor = "tmpm->flags|=MLF_DFT_AVAILABLE;";
      snippet = "tmpm->flags|=MLF_DFT_AVAILABLE;";
    };
    {
      id = "default-header-compare";
      description = "function header comparison checks default availability";
      path = "Compiler/LexLib.HC";
      anchor =
        "if (tmpm1->flags&MLF_DFT_AVAILABLE || tmpm2->flags&MLF_DFT_AVAILABLE) \
         {";
      snippet =
        "if (tmpm1->flags&MLF_DFT_AVAILABLE || tmpm2->flags&MLF_DFT_AVAILABLE) \
         {";
    };
    {
      id = "string-default-header-compare";
      description = "function header comparison distinguishes string defaults";
      path = "Compiler/LexLib.HC";
      anchor = "if (tmpm1->flags&MLF_STR_DFT_AVAILABLE) {";
      snippet =
        "if (tmpm1->flags&MLF_STR_DFT_AVAILABLE) { if \
         (StrCmp(tmpm1->dft_val,tmpm2->dft_val)) return FALSE; }";
    };
    {
      id = "string-default-delete";
      description = "member-list deletion frees an owned string default";
      path = "Compiler/LexLib.HC";
      anchor =
        "if (tmpm->flags & MLF_STR_DFT_AVAILABLE)\n      Free(tmpm->dft_val);";
      snippet = "if (tmpm->flags & MLF_STR_DFT_AVAILABLE) Free(tmpm->dft_val);";
    };
    {
      id = "callback-delete";
      description = "member-list deletion releases an owned callback record";
      path = "Compiler/LexLib.HC";
      anchor =
        "if (tmpm->flags & MLF_FUN)\n\
        \      HashDel(tmpm->fun_ptr-tmpm->fun_ptr->ptr_stars_cnt);";
      snippet =
        "if (tmpm->flags & MLF_FUN) \
         HashDel(tmpm->fun_ptr-tmpm->fun_ptr->ptr_stars_cnt);";
    };
    {
      id = "string-default-size";
      description = "member-list sizing includes an owned string default";
      path = "Compiler/LexLib.HC";
      anchor =
        "if (tmpm->flags & MLF_STR_DFT_AVAILABLE)\n\
        \      res+=MSize2(tmpm->dft_val);";
      snippet =
        "if (tmpm->flags & MLF_STR_DFT_AVAILABLE) res+=MSize2(tmpm->dft_val);";
    };
    {
      id = "callback-size";
      description = "member-list sizing includes an owned callback record";
      path = "Compiler/LexLib.HC";
      anchor =
        "if (tmpm->flags & MLF_FUN)\n\
        \      res+=HashEntrySize2(tmpm->fun_ptr-tmpm->fun_ptr->ptr_stars_cnt);";
      snippet =
        "if (tmpm->flags & MLF_FUN) \
         res+=HashEntrySize2(tmpm->fun_ptr-tmpm->fun_ptr->ptr_stars_cnt);";
    };
    {
      id = "unused-warning-consumer";
      description =
        "function completion honors the unused-warning suppression bit";
      path = "Compiler/PrsStmt.HC";
      anchor = "if (tmpm->flags & MLF_NO_UNUSED_WARN) {";
      snippet = "if (tmpm->flags & MLF_NO_UNUSED_WARN) {";
    };
    {
      id = "no-warn-directive";
      description = "the no-warning statement marks a selected local";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpm->flags|=MLF_NO_UNUSED_WARN;";
      snippet = "tmpm->flags|=MLF_NO_UNUSED_WARN;";
    };
    {
      id = "call-default-selection";
      description = "call parsing selects an available omitted argument default";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpm->flags & MLF_DFT_AVAILABLE &&";
      snippet =
        "if (tmpm->flags & MLF_DFT_AVAILABLE && (cc->token==')' || \
         cc->token==',' || !needs_right_paren)) {";
    };
    {
      id = "call-lastclass-selection";
      description = "call parsing substitutes the current last class";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpm->flags & MLF_LASTCLASS && last_class)";
      snippet =
        "if (tmpm->flags & MLF_LASTCLASS && last_class) \
         dft_val=(last_class-last_class->ptr_stars_cnt)->str;";
    };
    {
      id = "call-reference-default";
      description =
        "AOT calls materialize string and lastclass defaults as references";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpm->flags & (MLF_STR_DFT_AVAILABLE|MLF_LASTCLASS) &&";
      snippet =
        "if (tmpm->flags & (MLF_STR_DFT_AVAILABLE|MLF_LASTCLASS) && \
         cc->flags&CCF_AOT_COMPILE) {";
    };
    {
      id = "varargs-call";
      description = "call parsing recognizes the synthesized variadic slots";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpm && tmpm->flags & MLF_DOT_DOT_DOT) {";
      snippet = "if (tmpm && tmpm->flags & MLF_DOT_DOT_DOT) {";
    };
    {
      id = "callback-local-expression";
      description = "a callback local supplies its stored function signature";
      path = "Compiler/PrsExp.HC";
      anchor =
        "if (tmpm->flags & MLF_FUN && !(cc->flags&CCF_ASM_EXPRESSIONS)) {";
      snippet =
        "if (tmpm->flags & MLF_FUN && !(cc->flags&CCF_ASM_EXPRESSIONS)) { \
         PrsPopDeref(ps); cc->flags|=CCF_FUN_EXP; \
         PrsPush2(ps,tmpm->fun_ptr-tmpm->fun_ptr->ptr_stars_cnt); }";
    };
    {
      id = "static-local-expression";
      description = "a static local selects its static storage path";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpm->flags&MLF_STATIC) {";
      snippet = "if (tmpm->flags&MLF_STATIC) {";
    };
    {
      id = "callback-member-expression";
      description = "a callback field supplies its stored function signature";
      path = "Compiler/PrsExp.HC";
      anchor = "if(tmpm->flags & MLF_FUN) {";
      snippet =
        "if(tmpm->flags & MLF_FUN) { PrsPopDeref(ps); \
         PrsPush2(ps,tmpm->fun_ptr-tmpm->fun_ptr->ptr_stars_cnt); \
         cc->flags|=CCF_FUN_EXP; }";
    };
  ]

let collect_behaviors sources =
  let rec collect found = function
    | [] -> Ok (List.rev found)
    | spec :: rest -> (
        match require_behavior sources spec with
        | Error _ as result -> result
        | Ok behavior -> collect (behavior :: found) rest)
  in
  collect [] behavior_specs

let references_for ids (behaviors : behavior_entry list) =
  behaviors
  |> List.filter (fun (behavior : behavior_entry) -> List.mem behavior.id ids)
  |> List.map (fun (behavior : behavior_entry) -> behavior.source)

let consumer_ids = function
  | "MLF_DFT_AVAILABLE" ->
      [
        "default-assignment"; "default-header-compare"; "call-default-selection";
      ]
  | "MLF_LASTCLASS" ->
      [
        "lastclass-assignment";
        "call-lastclass-selection";
        "call-reference-default";
      ]
  | "MLF_STR_DFT_AVAILABLE" ->
      [
        "string-default-assignment";
        "string-default-header-compare";
        "string-default-delete";
        "string-default-size";
        "call-reference-default";
      ]
  | "MLF_FUN" ->
      [
        "callback-assignment";
        "callback-delete";
        "callback-size";
        "callback-local-expression";
        "callback-member-expression";
      ]
  | "MLF_DOT_DOT_DOT" -> [ "varargs-argc"; "varargs-argv"; "varargs-call" ]
  | "MLF_NO_UNUSED_WARN" ->
      [
        "anonymous-slot-no-warning";
        "unused-warning-consumer";
        "no-warn-directive";
      ]
  | "MLF_STATIC" -> [ "static-assignment"; "static-local-expression" ]
  | name -> invalid_arg ("unknown member-list flag " ^ name)

let bit_index mask =
  let rec find index value =
    if Int64.equal value 1L then index
    else find (index + 1) (Int64.shift_right_logical value 1)
  in
  find 0 mask

let make_flags definitions behaviors =
  List.map
    (fun definition ->
      {
        source_name = definition.name;
        bit_index = bit_index definition.value;
        mask = definition.value;
        definition_line = definition.line;
        consumers = references_for (consumer_ids definition.name) behaviors;
      })
    definitions

let ( let* ) result continuation = Result.bind result continuation

let parse ~kernel_source ~lex_lib_source ~prs_var_source ~prs_stmt_source
    ~prs_exp_source =
  let kernel_path = "Kernel/KernelA.HH" in
  let* definitions = collect_definitions ~path:kernel_path kernel_source in
  let* () = validate_definitions ~path:kernel_path definitions in
  let sources =
    [
      ("Compiler/LexLib.HC", lex_lib_source);
      ("Compiler/PrsVar.HC", prs_var_source);
      ("Compiler/PrsStmt.HC", prs_stmt_source);
      ("Compiler/PrsExp.HC", prs_exp_source);
    ]
  in
  let* behaviors = collect_behaviors sources in
  Ok { flags = make_flags definitions behaviors; behaviors }
