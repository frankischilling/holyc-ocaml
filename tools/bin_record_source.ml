type source_reference = { path : string; line : int }
type width = Width_0 | Width_8 | Width_16 | Width_32 | Width_64

type source_status =
  | Source_active
  | Source_fictitious
  | Source_not_implemented
  | Source_not_really_used

type category =
  | Terminator
  | Import
  | Export
  | Absolute_addresses
  | Code_heap
  | Data_heap
  | Main

type leading_value =
  | No_leading_value
  | Patch_offset
  | Export_value
  | Entry_count
  | Main_offset

type name_mode =
  | No_name_field
  | Required_name
  | First_name_then_inherited
  | Optional_export_name
  | Empty_name

type payload =
  | No_payload
  | U32_offsets
  | I32_size_then_u32_offsets
  | I64_size_then_u32_offsets

type relocation_kind = Relative | Immediate

type relocation = {
  kind : relocation_kind;
  width : width;
  displacement_bias : int;
}

type pass1_action =
  | Pass1_stop
  | Resolve_import
  | Register_relative_export
  | Register_immediate_export
  | Apply_module_base_u32
  | Allocate_code
  | Allocate_zeroed_code
  | Allocate_data
  | Allocate_zeroed_data
  | Pass1_ignore

type pass2_action =
  | Pass2_stop
  | Execute_main
  | Skip_u32_offsets
  | Skip_i32_size_and_u32_offsets
  | Skip_i64_size_and_u32_offsets
  | Pass2_ignore

type entry = {
  name : string;
  code : int;
  status : source_status;
  category : category;
  leading_value : leading_value;
  name_mode : name_mode;
  payload : payload;
  relocation : relocation option;
  pass1 : pass1_action;
  pass2 : pass2_action;
  definition_line : int;
  consumers : source_reference list;
}

type header_field = {
  name : string;
  source_type : string;
  width_bytes : int;
  offset : int;
  definition_line : int;
}

type adjustment_operation = Add | Subtract

type adjustment = {
  name : string;
  code : int;
  width : width;
  operation : adjustment_operation;
  definition_line : int;
  consumers : source_reference list;
}

type behavior = {
  header_layout : source_reference;
  header_write : source_reference;
  module_validation : source_reference;
  module_base : source_reference;
  import_grouping : source_reference;
  import_patches : source_reference;
  export_registration : source_reference;
  absolute_patch : source_reference;
  code_heap_patch : source_reference;
  data_heap_patch : source_reference;
  main_execution : source_reference;
  pass_order : source_reference;
  patch_termination : source_reference;
  boot_patch_table : source_reference;
  boot_absolute_patch : source_reference;
  jit_adjustments : source_reference;
  aot_adjustments : source_reference;
}

type tables = {
  signature_spelling : string;
  signature_value : int32;
  signature_line : int;
  header_size : int;
  header_fields : header_field list;
  immediate_not_relative_mask : int;
  immediate_not_relative_line : int;
  entries : entry list;
  reserved_codes : int list;
  adjustments : adjustment list;
  behavior : behavior;
}

type error = { path : string option; line : int option; message : string }

type raw_definition = {
  name : string;
  expression : string;
  comment : string option;
  line : int;
}

let required_source_paths =
  [
    "Kernel/KernelA.HH";
    "Kernel/KLoad.HC";
    "Kernel/KStart16.HC";
    "Kernel/KStart32.HC";
    "Compiler/CMain.HC";
    "Compiler/AsmResolve.HC";
    "Compiler/Asm.HC";
    "Compiler/PrsVar.HC";
    "Compiler/BackFA.HC";
    "Compiler/BackLib.HC";
    "Compiler/BackC.HC";
    "Compiler/OptPass3.HC";
    "Compiler/OptPass789A.HC";
  ]

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

let width_bytes = function
  | Width_0 -> 0
  | Width_8 -> 1
  | Width_16 -> 2
  | Width_32 -> 4
  | Width_64 -> 8

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

let source_for sources path =
  match
    List.filter (fun (candidate, _) -> String.equal candidate path) sources
  with
  | [ (_, source) ] -> Ok (normalize_checkout_line_endings source)
  | [] -> error ~path "required source was not supplied"
  | _ -> error ~path "required source was supplied more than once"

let validate_source_set sources =
  let supplied = List.map fst sources in
  let rec check_required = function
    | [] -> Ok ()
    | path :: rest ->
        if List.mem path supplied then check_required rest
        else error ~path "required source was not supplied"
  in
  let rec check_extra = function
    | [] -> Ok ()
    | path :: rest ->
        if List.mem path required_source_paths then check_extra rest
        else error ~path "unexpected source was supplied"
  in
  let ( let* ) result continuation = Result.bind result continuation in
  let* () = check_required required_source_paths in
  check_extra supplied

let split_comment text =
  match find_offsets ~needle:"//" text with
  | [] -> (String.trim text, None)
  | offset :: _ ->
      let expression = String.sub text 0 offset |> String.trim in
      let comment =
        String.sub text (offset + 2) (String.length text - offset - 2)
        |> String.trim
      in
      (expression, Some comment)

let parse_define line_number line =
  let prefix = "#define" in
  let trimmed = String.trim line in
  if not (starts_with ~prefix trimmed) then None
  else
    let length = String.length trimmed in
    let rec skip cursor =
      if
        cursor < length
        && (Char.equal trimmed.[cursor] ' ' || Char.equal trimmed.[cursor] '\t')
      then skip (cursor + 1)
      else cursor
    in
    let name_start = skip (String.length prefix) in
    let rec finish_name cursor =
      if cursor < length && is_identifier_rest trimmed.[cursor] then
        finish_name (cursor + 1)
      else cursor
    in
    let name_end = finish_name name_start in
    if name_end = name_start then None
    else
      let expression_start = skip name_end in
      let rest =
        String.sub trimmed expression_start (length - expression_start)
      in
      let expression, comment = split_comment rest in
      Some
        {
          name = String.sub trimmed name_start (name_end - name_start);
          expression;
          comment;
          line = line_number;
        }

let collect_definitions ~path ~relevant source =
  let lines = String.split_on_char '\n' source in
  let rec collect line_number seen found = function
    | [] -> Ok (List.rev found)
    | line :: rest -> (
        match parse_define line_number line with
        | Some definition when relevant definition.name ->
            if List.mem definition.name seen then
              error ~path ~line:line_number
                (Printf.sprintf "%s is defined more than once" definition.name)
            else
              collect (line_number + 1) (definition.name :: seen)
                (definition :: found) rest
        | _ -> collect (line_number + 1) seen found rest)
  in
  collect 1 [] [] lines

let parse_decimal ~path definition =
  match Int64.of_string_opt definition.expression with
  | Some value -> Ok value
  | None ->
      error ~path ~line:definition.line
        (Printf.sprintf "%s has unsupported value %S" definition.name
           definition.expression)

let expected_entries =
  [
    ("IET_END", 0);
    ("IET_REL_I0", 2);
    ("IET_IMM_U0", 3);
    ("IET_REL_I8", 4);
    ("IET_IMM_U8", 5);
    ("IET_REL_I16", 6);
    ("IET_IMM_U16", 7);
    ("IET_REL_I32", 8);
    ("IET_IMM_U32", 9);
    ("IET_REL_I64", 10);
    ("IET_IMM_I64", 11);
    ("IET_REL32_EXPORT", 16);
    ("IET_IMM32_EXPORT", 17);
    ("IET_REL64_EXPORT", 18);
    ("IET_IMM64_EXPORT", 19);
    ("IET_ABS_ADDR", 20);
    ("IET_CODE_HEAP", 21);
    ("IET_ZEROED_CODE_HEAP", 22);
    ("IET_DATA_HEAP", 23);
    ("IET_ZEROED_DATA_HEAP", 24);
    ("IET_MAIN", 25);
  ]

let expected_adjustments =
  [
    ("AAT_ADD_U8", 0);
    ("AAT_SUB_U8", 1);
    ("AAT_ADD_U16", 2);
    ("AAT_SUB_U16", 3);
    ("AAT_ADD_U32", 4);
    ("AAT_SUB_U32", 5);
    ("AAT_ADD_U64", 6);
    ("AAT_SUB_U64", 7);
  ]

let compare_definitions ~path ~kind expected definitions =
  let ( let* ) result continuation = Result.bind result continuation in
  let rec compare expected definitions =
    match (expected, definitions) with
    | [], [] -> Ok ()
    | (name, _) :: _, [] ->
        error ~path (Printf.sprintf "%s table is missing %s" kind name)
    | [], definition :: _ ->
        error ~path ~line:definition.line
          (Printf.sprintf "%s table contains unexpected %s" kind definition.name)
    | (name, value) :: expected_rest, definition :: definition_rest ->
        if not (String.equal name definition.name) then
          error ~path ~line:definition.line
            (Printf.sprintf "%s table requires %s here, but found %s" kind name
               definition.name)
        else
          let* parsed = parse_decimal ~path definition in
          if parsed <> Int64.of_int value then
            error ~path ~line:definition.line
              (Printf.sprintf "%s must remain %d" name value)
          else if not (String.equal definition.expression (string_of_int value))
          then
            error ~path ~line:definition.line
              (Printf.sprintf "%s must retain source expression %S" name
                 (string_of_int value))
          else compare expected_rest definition_rest
  in
  compare expected definitions

let source_status name =
  match name with
  | "IET_REL_I0" | "IET_IMM_U0" -> Source_fictitious
  | "IET_REL64_EXPORT" | "IET_IMM64_EXPORT" -> Source_not_implemented
  | "IET_CODE_HEAP" | "IET_ZEROED_CODE_HEAP" | "IET_ZEROED_DATA_HEAP" ->
      Source_not_really_used
  | _ -> Source_active

let expected_comment = function
  | Source_active -> None
  | Source_fictitious -> Some "Fictitious"
  | Source_not_implemented -> Some "Not implemented"
  | Source_not_really_used -> Some "Not really used"

let validate_entry_comments ~path definitions =
  let rec validate = function
    | [] -> Ok ()
    | definition :: rest ->
        let expected = expected_comment (source_status definition.name) in
        if definition.comment = expected then validate rest
        else
          error ~path ~line:definition.line
            (Printf.sprintf "%s must retain source status comment %s"
               definition.name
               (match expected with
               | None -> "<none>"
               | Some text -> text))
  in
  validate definitions

let count_trimmed_lines text source =
  String.split_on_char '\n' source
  |> List.fold_left
       (fun count line ->
         if String.equal (String.trim line) text then count + 1 else count)
       0

let parse_signature ~path definitions =
  match definitions with
  | [ definition ] ->
      if not (String.equal definition.name "BIN_SIGNATURE_VAL") then
        error ~path ~line:definition.line "binary signature definition changed"
      else
        let spelling = definition.expression in
        let length = String.length spelling in
        if
          length <> 6
          || (not (Char.equal spelling.[0] '\''))
          || not (Char.equal spelling.[length - 1] '\'')
        then
          error ~path ~line:definition.line
            "BIN_SIGNATURE_VAL must remain a four-byte character constant"
        else
          let value = ref 0L in
          for index = 0 to 3 do
            value :=
              Int64.logor !value
                (Int64.shift_left
                   (Int64.of_int (Char.code spelling.[index + 1]))
                   (index * 8))
          done;
          if not (String.equal spelling "'TOSB'") then
            error ~path ~line:definition.line
              "BIN_SIGNATURE_VAL must remain 'TOSB'"
          else Ok (spelling, Int64.to_int32 !value, definition.line)
  | [] -> error ~path "BIN_SIGNATURE_VAL is missing"
  | definition :: _ ->
      error ~path ~line:definition.line
        "BIN_SIGNATURE_VAL is defined more than once"

type lexical_state =
  | Line_comment
  | Block_comment of int
  | String_literal
  | Character_literal

let mask_non_code ~path source =
  let source = normalize_checkout_line_endings source in
  let masked = Bytes.of_string source in
  let length = String.length source in
  let blank offset =
    if offset < length && not (Char.equal source.[offset] '\n') then
      Bytes.set masked offset ' '
  in
  let rec normal offset line =
    if offset >= length then Ok (Bytes.to_string masked)
    else
      match source.[offset] with
      | '/' when offset + 1 < length && Char.equal source.[offset + 1] '/' ->
          blank offset;
          blank (offset + 1);
          scan Line_comment (offset + 2) line
      | '/' when offset + 1 < length && Char.equal source.[offset + 1] '*' ->
          blank offset;
          blank (offset + 1);
          scan (Block_comment 1) (offset + 2) line
      | '"' ->
          blank offset;
          scan String_literal (offset + 1) line
      | '\'' ->
          blank offset;
          scan Character_literal (offset + 1) line
      | '\n' -> normal (offset + 1) (line + 1)
      | _ -> normal (offset + 1) line
  and scan state offset line =
    if offset >= length then
      match state with
      | Line_comment -> Ok (Bytes.to_string masked)
      | Block_comment _ -> error ~path ~line "unterminated block comment"
      | String_literal -> error ~path ~line "unterminated string literal"
      | Character_literal -> error ~path ~line "unterminated character literal"
    else
      match state with
      | Line_comment ->
          if Char.equal source.[offset] '\n' then normal (offset + 1) (line + 1)
          else (
            blank offset;
            scan Line_comment (offset + 1) line)
      | Block_comment depth ->
          if
            Char.equal source.[offset] '/'
            && offset + 1 < length
            && Char.equal source.[offset + 1] '*'
          then (
            blank offset;
            blank (offset + 1);
            scan (Block_comment (depth + 1)) (offset + 2) line)
          else if
            Char.equal source.[offset] '*'
            && offset + 1 < length
            && Char.equal source.[offset + 1] '/'
          then (
            blank offset;
            blank (offset + 1);
            if depth = 1 then normal (offset + 2) line
            else scan (Block_comment (depth - 1)) (offset + 2) line)
          else (
            blank offset;
            scan (Block_comment depth) (offset + 1)
              (if Char.equal source.[offset] '\n' then line + 1 else line))
      | (String_literal | Character_literal) as literal ->
          let terminator =
            match literal with
            | String_literal -> '"'
            | _ -> '\''
          in
          if Char.equal source.[offset] '\n' then
            error ~path ~line "literal crosses a line"
          else if Char.equal source.[offset] '\\' && offset + 1 < length then (
            blank offset;
            blank (offset + 1);
            scan literal (offset + 2) line)
          else if Char.equal source.[offset] terminator then (
            blank offset;
            normal (offset + 1) line)
          else (
            blank offset;
            scan literal (offset + 1) line)
  in
  normal 0 1

let compact_code source =
  let buffer = Buffer.create (String.length source) in
  String.iter
    (function
      | ' ' | '\t' | '\r' | '\n' -> ()
      | byte -> Buffer.add_char buffer byte)
    source;
  Buffer.contents buffer

let require_compact ~path ~anchor ~snippet source =
  let ( let* ) result continuation = Result.bind result continuation in
  let* masked = mask_non_code ~path source in
  let compacted = compact_code masked in
  match find_offsets ~needle:snippet compacted with
  | [] ->
      error ~path
        (Printf.sprintf "required source behavior near %S is missing" anchor)
  | _ :: _ :: _ ->
      error ~path
        (Printf.sprintf "required source behavior near %S is ambiguous" anchor)
  | [ _ ] -> (
      match find_offsets ~needle:anchor source with
      | [ offset ] -> Ok { path; line = line_of_offset source offset }
      | [] -> error ~path (Printf.sprintf "source anchor %S is missing" anchor)
      | _ ->
          error ~path
            (Printf.sprintf "source anchor %S appears more than once" anchor))

let require_compact_count ~path ~description ~snippet ~count source =
  let ( let* ) result continuation = Result.bind result continuation in
  let* masked = mask_non_code ~path source in
  let actual =
    compact_code masked |> find_offsets ~needle:snippet |> List.length
  in
  if actual = count then Ok ()
  else
    error ~path
      (Printf.sprintf "%s must appear %d times, but appears %d times"
         description count actual)

let expected_header_fields =
  [
    ("jmp", "U16", 2);
    ("module_align_bits", "U8", 1);
    ("reserved", "U8", 1);
    ("bin_signature", "U32", 4);
    ("org", "I64", 8);
    ("patch_table_offset", "I64", 8);
    ("file_size", "I64", 8);
  ]

let parse_header ~path source =
  let ( let* ) result continuation = Result.bind result continuation in
  let* header_layout =
    require_compact ~path ~anchor:"class CBinFile"
      ~snippet:
        "classCBinFile{U16jmp;U8module_align_bits,reserved;U32bin_signature;I64org,patch_table_offset,file_size;};"
      source
  in
  let* class_start =
    unique_offset ~path ~description:"CBinFile declaration"
      ~needle:"class CBinFile" source
  in
  let tail =
    String.sub source class_start (String.length source - class_start)
  in
  let* class_end_relative =
    match find_offsets ~needle:"};" tail with
    | offset :: _ -> Ok (offset + 2)
    | [] ->
        error ~path ~line:header_layout.line
          "CBinFile declaration is incomplete"
  in
  let block = String.sub tail 0 class_end_relative in
  let rec fields offset found = function
    | [] -> Ok (List.rev found, offset)
    | (name, source_type, width_bytes) :: rest ->
        let* relative =
          unique_offset ~path
            ~description:(Printf.sprintf "CBinFile.%s" name)
            ~needle:name block
        in
        let definition_line = line_of_offset source (class_start + relative) in
        fields (offset + width_bytes)
          ({ name; source_type; width_bytes; offset; definition_line } :: found)
          rest
  in
  let* header_fields, header_size = fields 0 [] expected_header_fields in
  if header_size <> 32 then error ~path "CBinFile must remain 32 bytes"
  else Ok (header_layout, header_fields, header_size)

let relevant_symbol name =
  starts_with ~prefix:"IET_" name || starts_with ~prefix:"AAT_" name

let scan_symbols ~known ~path source =
  let ( let* ) result continuation = Result.bind result continuation in
  let* masked = mask_non_code ~path source in
  let length = String.length masked in
  let rec scan offset line found =
    if offset >= length then Ok (List.rev found)
    else
      match masked.[offset] with
      | '\n' -> scan (offset + 1) (line + 1) found
      | byte when is_identifier_start byte ->
          let rec finish cursor =
            if cursor < length && is_identifier_rest masked.[cursor] then
              finish (cursor + 1)
            else cursor
          in
          let last = finish (offset + 1) in
          let name = String.sub masked offset (last - offset) in
          if relevant_symbol name then
            if List.mem name known then
              scan last line ((name, { path; line }) :: found)
            else
              error ~path ~line
                (Printf.sprintf "source uses unknown BIN symbol %s" name)
          else scan last line found
      | _ -> scan (offset + 1) line found
  in
  scan 0 1 []

let compare_reference (left : source_reference) (right : source_reference) =
  match String.compare left.path right.path with
  | 0 -> Int.compare left.line right.line
  | order -> order

let deduplicate_references references =
  references |> List.sort_uniq compare_reference

let consumers_for ~definition_line ~definition_path name references =
  references
  |> List.filter_map (fun (found_name, (reference : source_reference)) ->
      if
        String.equal name found_name
        && not
             (String.equal reference.path definition_path
             && reference.line = definition_line)
      then Some reference
      else None)
  |> deduplicate_references

let width_of_import name =
  if
    starts_with ~prefix:"IET_REL_I0" name
    || starts_with ~prefix:"IET_IMM_U0" name
  then Width_0
  else if
    starts_with ~prefix:"IET_REL_I8" name
    || starts_with ~prefix:"IET_IMM_U8" name
  then Width_8
  else if
    starts_with ~prefix:"IET_REL_I16" name
    || starts_with ~prefix:"IET_IMM_U16" name
  then Width_16
  else if
    starts_with ~prefix:"IET_REL_I32" name
    || starts_with ~prefix:"IET_IMM_U32" name
  then Width_32
  else Width_64

let entry_metadata name =
  if
    starts_with ~prefix:"IET_REL_I" name
    || starts_with ~prefix:"IET_IMM_U" name
    || String.equal name "IET_IMM_I64"
  then
    let width = width_of_import name in
    let kind =
      if starts_with ~prefix:"IET_REL_" name then Relative else Immediate
    in
    ( Import,
      Patch_offset,
      First_name_then_inherited,
      No_payload,
      Some
        {
          kind;
          width;
          displacement_bias =
            (match kind with
            | Relative -> width_bytes width
            | Immediate -> 0);
        },
      Resolve_import,
      Pass2_ignore )
  else
    match name with
    | "IET_END" ->
        ( Terminator,
          No_leading_value,
          No_name_field,
          No_payload,
          None,
          Pass1_stop,
          Pass2_stop )
    | "IET_REL32_EXPORT" | "IET_REL64_EXPORT" ->
        ( Export,
          Export_value,
          Required_name,
          No_payload,
          None,
          Register_relative_export,
          Pass2_ignore )
    | "IET_IMM32_EXPORT" | "IET_IMM64_EXPORT" ->
        ( Export,
          Export_value,
          Required_name,
          No_payload,
          None,
          Register_immediate_export,
          Pass2_ignore )
    | "IET_ABS_ADDR" ->
        ( Absolute_addresses,
          Entry_count,
          Empty_name,
          U32_offsets,
          None,
          Apply_module_base_u32,
          Skip_u32_offsets )
    | "IET_CODE_HEAP" ->
        ( Code_heap,
          Entry_count,
          Optional_export_name,
          I32_size_then_u32_offsets,
          None,
          Allocate_code,
          Skip_i32_size_and_u32_offsets )
    | "IET_ZEROED_CODE_HEAP" ->
        ( Code_heap,
          Entry_count,
          Optional_export_name,
          I32_size_then_u32_offsets,
          None,
          Allocate_zeroed_code,
          Skip_i32_size_and_u32_offsets )
    | "IET_DATA_HEAP" ->
        ( Data_heap,
          Entry_count,
          Optional_export_name,
          I64_size_then_u32_offsets,
          None,
          Allocate_data,
          Skip_i64_size_and_u32_offsets )
    | "IET_ZEROED_DATA_HEAP" ->
        ( Data_heap,
          Entry_count,
          Optional_export_name,
          I64_size_then_u32_offsets,
          None,
          Allocate_zeroed_data,
          Skip_i64_size_and_u32_offsets )
    | "IET_MAIN" ->
        ( Main,
          Main_offset,
          Empty_name,
          No_payload,
          None,
          Pass1_ignore,
          Execute_main )
    | _ -> invalid_arg ("unclassified BIN entry " ^ name)

let adjustment_metadata name =
  let operation =
    if starts_with ~prefix:"AAT_ADD_" name then Add else Subtract
  in
  let width =
    if String.ends_with ~suffix:"U8" name then Width_8
    else if String.ends_with ~suffix:"U16" name then Width_16
    else if String.ends_with ~suffix:"U32" name then Width_32
    else Width_64
  in
  (width, operation)

let behavior_sources ~kernel_source ~loader_source ~writer_source
    ~kstart16_source ~kstart32_source =
  let ( let* ) result continuation = Result.bind result continuation in
  let adjustment_switch =
    "caseAAT_ADD_U8:*ptr(U8*)+=rip2;break;caseAAT_SUB_U8:*ptr(U8*)-=rip2;break;caseAAT_ADD_U16:*ptr(U16*)+=rip2;break;caseAAT_SUB_U16:*ptr(U16*)-=rip2;break;caseAAT_ADD_U32:*ptr(U32*)+=rip2;break;caseAAT_SUB_U32:*ptr(U32*)-=rip2;break;caseAAT_ADD_U64:*ptr(I64*)+=rip2;break;caseAAT_SUB_U64:*ptr(I64*)-=rip2;break;"
  in
  let* () =
    require_compact_count ~path:"Compiler/CMain.HC"
      ~description:"complete AOT adjustment switch" ~snippet:adjustment_switch
      ~count:2 writer_source
  in
  let* header_layout =
    require_compact ~path:"Kernel/KernelA.HH" ~anchor:"class CBinFile"
      ~snippet:
        "classCBinFile{U16jmp;U8module_align_bits,reserved;U32bin_signature;I64org,patch_table_offset,file_size;};"
      kernel_source
  in
  let* header_write =
    require_compact ~path:"Compiler/CMain.HC"
      ~anchor:"bfh->jmp=0xEB+256*(sizeof(CBinFile)-2);"
      ~snippet:
        "bfh->reserved=0;bfh->bin_signature=BIN_SIGNATURE_VAL;bfh->org=tmpaot->org;bfh->module_align_bits=tmpaot->max_align_bits;bfh->patch_table_offset=sizeof(CBinFile)+aot_U8s;bfh->file_size=size;"
      writer_source
  in
  let* module_validation =
    require_compact ~path:"Kernel/KLoad.HC"
      ~anchor:"module_align=1<<bfh->module_align_bits;"
      ~snippet:
        "module_align=1<<bfh->module_align_bits;if(!module_align||bfh->bin_signature!=BIN_SIGNATURE_VAL)"
      loader_source
  in
  let* module_base =
    require_compact ~path:"Kernel/KLoad.HC"
      ~anchor:"module_base=bfh_addr(U8 *)+sizeof(CBinFile);"
      ~snippet:"module_base=bfh_addr(U8*)+sizeof(CBinFile);" loader_source
  in
  let* import_grouping =
    require_compact ~path:"Kernel/KLoad.HC" ~anchor:"if (!first) {"
      ~snippet:"if(!first){*_src=st_ptr-5;return;}else{first=FALSE;"
      loader_source
  in
  let* import_patches =
    require_compact ~path:"Kernel/KLoad.HC" ~anchor:"case IET_REL_I8:"
      ~snippet:
        "caseIET_REL_I8:*ptr2(U8*)=i-ptr2-1;break;caseIET_IMM_U8:*ptr2(U8*)=i;break;caseIET_REL_I16:*ptr2(U16*)=i-ptr2-2;break;caseIET_IMM_U16:*ptr2(U16*)=i;break;caseIET_REL_I32:*ptr2(U32*)=i-ptr2-4;break;caseIET_IMM_U32:*ptr2(U32*)=i;break;caseIET_REL_I64:*ptr2(I64*)=i-ptr2-8;break;caseIET_IMM_I64:*ptr2(I64*)=i;break;"
      loader_source
  in
  let* export_registration =
    require_compact ~path:"Kernel/KLoad.HC"
      ~anchor:"if (etype==IET_IMM32_EXPORT||etype==IET_IMM64_EXPORT)"
      ~snippet:
        "if(etype==IET_IMM32_EXPORT||etype==IET_IMM64_EXPORT)tmpex->val=i;elsetmpex->val=i+module_base;"
      loader_source
  in
  let* absolute_patch =
    require_compact ~path:"Kernel/KLoad.HC" ~anchor:"*ptr2(U32 *)+=module_base;"
      ~snippet:"ptr2=module_base+*src(U32*)++;*ptr2(U32*)+=module_base;"
      loader_source
  in
  let* code_heap_patch =
    require_compact ~path:"Kernel/KLoad.HC"
      ~anchor:"ptr3=MAlloc(*src(I32 *)++,Fs->code_heap);"
      ~snippet:
        "caseIET_CODE_HEAP:ptr3=MAlloc(*src(I32*)++,Fs->code_heap);break;caseIET_ZEROED_CODE_HEAP:ptr3=CAlloc(*src(I32*)++,Fs->code_heap);break;"
      loader_source
  in
  let* data_heap_patch =
    require_compact ~path:"Kernel/KLoad.HC"
      ~anchor:"ptr3=MAlloc(*src(I64 *)++);"
      ~snippet:
        "caseIET_DATA_HEAP:ptr3=MAlloc(*src(I64*)++);break;caseIET_ZEROED_DATA_HEAP:ptr3=CAlloc(*src(I64*)++);break;"
      loader_source
  in
  let* main_execution =
    require_compact ~path:"Kernel/KLoad.HC" ~anchor:"case IET_MAIN:"
      ~snippet:"caseIET_MAIN:Call(i+module_base);break;" loader_source
  in
  let* pass_order =
    require_compact ~path:"Kernel/KLoad.HC"
      ~anchor:"LoadPass1(bfh_addr(U8 *)+bfh_addr->patch_table_offset"
      ~snippet:
        "LoadPass1(bfh_addr(U8*)+bfh_addr->patch_table_offset,module_base,ld_flags);if(!(ld_flags&LDF_JUST_LOAD))LoadPass2(bfh_addr(U8*)+bfh_addr->patch_table_offset,module_base,ld_flags);"
      loader_source
  in
  let* patch_termination =
    require_compact ~path:"Compiler/CMain.HC" ~anchor:"*ptr++=IET_END;"
      ~snippet:
        "*ptr++=IET_END;MemSet(ptr,0,16);i=ptr-patch_table;size=(sizeof(CBinFile)+aot_U8s+i+15)&-16;"
      writer_source
  in
  let* boot_patch_table =
    require_compact ~path:"Kernel/KStart16.HC"
      ~anchor:"ADD\tEDX,U32 GS:[CBinFile.patch_table_offset]"
      ~snippet:
        "MOVGS,DXMOVEDX,EAXADDEDX,U32GS:[CBinFile.patch_table_offset]SUBEDX,sizeof(CBinFile)"
      kstart16_source
  in
  let* boot_absolute_patch =
    require_compact ~path:"Kernel/KStart32.HC"
      ~anchor:"MOV\tECX,U32 CPatchTableAbsAddr.abs_addres_cnt[ESI]"
      ~snippet:
        "MOVECX,U32CPatchTableAbsAddr.abs_addres_cnt[ESI]LEAESI,U32CPatchTableAbsAddr.abs_addres[ESI]@@05:LODSDADDEAX,EDIADDU32[EAX],EDILOOP@@05"
      kstart32_source
  in
  let* jit_adjustments =
    require_compact ~path:"Compiler/CMain.HC"
      ~anchor:"U0 CmpFixUpJITAsm(CCmpCtrl *cc,CAOT *tmpaot)"
      ~snippet:
        "U0CmpFixUpJITAsm(CCmpCtrl*cc,CAOT*tmpaot){I64i,rip2=tmpaot->buf+tmpaot->rip,*str=NULL;"
      writer_source
  in
  let* aot_adjustments =
    require_compact ~path:"Compiler/CMain.HC"
      ~anchor:"U0 CmpFixUpAOTAsm(CCmpCtrl *cc,CAOT *tmpaot)"
      ~snippet:
        "U0CmpFixUpAOTAsm(CCmpCtrl*cc,CAOT*tmpaot){CAOTCtrl*aotc=cc->aotc;I64i,rip2=tmpaot->rip+cc->aotc->rip;"
      writer_source
  in
  Ok
    {
      header_layout;
      header_write;
      module_validation;
      module_base;
      import_grouping;
      import_patches;
      export_registration;
      absolute_patch;
      code_heap_patch;
      data_heap_patch;
      main_execution;
      pass_order;
      patch_termination;
      boot_patch_table;
      boot_absolute_patch;
      jit_adjustments;
      aot_adjustments;
    }

let parse ~sources =
  let ( let* ) result continuation = Result.bind result continuation in
  let* () = validate_source_set sources in
  let* kernel_source = source_for sources "Kernel/KernelA.HH" in
  let* loader_source = source_for sources "Kernel/KLoad.HC" in
  let* kstart16_source = source_for sources "Kernel/KStart16.HC" in
  let* kstart32_source = source_for sources "Kernel/KStart32.HC" in
  let* writer_source = source_for sources "Compiler/CMain.HC" in
  let kernel_path = "Kernel/KernelA.HH" in
  let* entry_definitions =
    collect_definitions ~path:kernel_path
      ~relevant:(starts_with ~prefix:"IET_")
      kernel_source
  in
  let* () =
    compare_definitions ~path:kernel_path ~kind:"BIN entry" expected_entries
      entry_definitions
  in
  let* () = validate_entry_comments ~path:kernel_path entry_definitions in
  if count_trimmed_lines "//reserved" kernel_source <> 2 then
    error ~path:kernel_path "BIN entry table must retain both reserved gaps"
  else
    let* adjustment_definitions =
      collect_definitions ~path:kernel_path
        ~relevant:(starts_with ~prefix:"AAT_")
        kernel_source
    in
    let* () =
      compare_definitions ~path:kernel_path ~kind:"AOT adjustment"
        expected_adjustments adjustment_definitions
    in
    let* signature_definitions =
      collect_definitions ~path:kernel_path
        ~relevant:(String.equal "BIN_SIGNATURE_VAL")
        kernel_source
    in
    let* signature_spelling, signature_value, signature_line =
      parse_signature ~path:kernel_path signature_definitions
    in
    let* immediate_definitions =
      collect_definitions ~path:kernel_path
        ~relevant:(String.equal "IEF_IMM_NOT_REL")
        kernel_source
    in
    let* immediate_definition =
      match immediate_definitions with
      | [ definition ] -> Ok definition
      | [] -> error ~path:kernel_path "IEF_IMM_NOT_REL is missing"
      | definition :: _ ->
          error ~path:kernel_path ~line:definition.line
            "IEF_IMM_NOT_REL is defined more than once"
    in
    let* immediate_mask =
      parse_decimal ~path:kernel_path immediate_definition
    in
    if
      immediate_mask <> 1L
      || not (String.equal immediate_definition.expression "1")
    then
      error ~path:kernel_path ~line:immediate_definition.line
        "IEF_IMM_NOT_REL must remain 1"
    else
      let* _, header_fields, header_size =
        parse_header ~path:kernel_path kernel_source
      in
      let known =
        List.map fst expected_entries @ List.map fst expected_adjustments
      in
      let rec scan_all found = function
        | [] -> Ok (List.concat (List.rev found))
        | path :: rest ->
            let* source = source_for sources path in
            let* references = scan_symbols ~known ~path source in
            scan_all (references :: found) rest
      in
      let* references = scan_all [] required_source_paths in
      let entries =
        List.map2
          (fun (name, code) definition ->
            let ( category,
                  leading_value,
                  name_mode,
                  payload,
                  relocation,
                  pass1,
                  pass2 ) =
              entry_metadata name
            in
            {
              name;
              code;
              status = source_status name;
              category;
              leading_value;
              name_mode;
              payload;
              relocation;
              pass1;
              pass2;
              definition_line = definition.line;
              consumers =
                consumers_for ~definition_line:definition.line
                  ~definition_path:kernel_path name references;
            })
          expected_entries entry_definitions
      in
      let adjustments =
        List.map2
          (fun (name, code) definition ->
            let width, operation = adjustment_metadata name in
            {
              name;
              code;
              width;
              operation;
              definition_line = definition.line;
              consumers =
                consumers_for ~definition_line:definition.line
                  ~definition_path:kernel_path name references;
            })
          expected_adjustments adjustment_definitions
      in
      let* behavior =
        behavior_sources ~kernel_source ~loader_source ~writer_source
          ~kstart16_source ~kstart32_source
      in
      Ok
        {
          signature_spelling;
          signature_value;
          signature_line;
          header_size;
          header_fields;
          immediate_not_relative_mask = Int64.to_int immediate_mask;
          immediate_not_relative_line = immediate_definition.line;
          entries;
          reserved_codes = [ 1; 12; 13; 14; 15 ];
          adjustments;
          behavior;
        }
