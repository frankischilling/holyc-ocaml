type source_reference = { path : string; line : int }

type flag_entry = {
  bit_name : string option;
  mask_name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

type global_type = {
  index_name : string;
  mask_name : string;
  type_index : int;
  type_mask : int64;
  index_definition_line : int;
  mask_definition_line : int;
}

type behavior_entry = {
  id : string;
  description : string;
  source : source_reference;
}

type tables = {
  global_type : global_type;
  hash_flags : flag_entry list;
  global_flags : flag_entry list;
  behaviors : behavior_entry list;
}

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

let is_identifier_rest = function
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
      if cursor < length && is_identifier_rest line.[cursor] then
        name_end (cursor + 1)
      else cursor
    in
    let name_last = name_end name_start in
    if name_last = name_start then None
    else
      let value_start = skip name_last in
      let name = String.sub line name_start (name_last - name_start) in
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

let collect_definitions ~path ~relevant source =
  let lines =
    normalize_checkout_line_endings source |> String.split_on_char '\n'
  in
  let rec collect line_number found = function
    | [] -> Ok (List.rev found)
    | line :: rest -> (
        match parse_define line_number line with
        | Some (name, spelling, line) when relevant name -> (
            match parse_literal ~path ~line name spelling with
            | Error _ as result -> result
            | Ok definition ->
                collect (line_number + 1) (definition :: found) rest)
        | _ -> collect (line_number + 1) found rest)
  in
  collect 1 [] lines

let validate_exact ~path expected actual =
  let rec check expected actual =
    match (expected, actual) with
    | [], [] -> Ok ()
    | (name, _) :: _, [] ->
        error ~path
          (Printf.sprintf "global record flag table is missing %s" name)
    | [], definition :: _ ->
        error ~path ~line:definition.line
          (Printf.sprintf "global record flag table contains unexpected %s"
             definition.name)
    | (name, value) :: expected_rest, definition :: actual_rest ->
        if not (String.equal name definition.name) then
          error ~path ~line:definition.line
            (Printf.sprintf
               "global record flag table requires %s here, found %s" name
               definition.name)
        else if not (Int64.equal value definition.value) then
          error ~path ~line:definition.line
            (Printf.sprintf "%s evaluates to 0x%Lx, expected 0x%Lx" name
               definition.value value)
        else check expected_rest actual_rest
  in
  check expected actual

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
          (Printf.sprintf "required global-record behavior %S is missing"
             spec.id)
      else if List.length snippet_offsets > 1 then
        error ~path:spec.path
          (Printf.sprintf "required global-record behavior %S is ambiguous"
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
      id = "private-from-option";
      description = "OPTf_KEEP_PRIVATE marks source-backed hash records private";
      path = "Kernel/KHashB.HC";
      anchor = "if (Bt(&cc->opts,OPTf_KEEP_PRIVATE))";
      snippet = "if (Bt(&cc->opts,OPTf_KEEP_PRIVATE)) h->type|=HTF_PRIVATE;";
    };
    {
      id = "public-from-modifier";
      description = "the final parser staging mask publishes a global record";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->type|=HTF_PUBLIC;";
      snippet = "if (fsp_flags&FSF_PUBLIC) tmpg->type|=HTF_PUBLIC;";
    };
    {
      id = "alternate-extern-export";
      description = "an AOT alternate extern is an exported global record";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->data_addr_rip=val;";
      snippet =
        "case PRS0__EXTERN: if (cc->flags&CCF_AOT_COMPILE) { \
         tmpg=CAlloc(sizeof(CHashGlblVar)); tmpg->data_addr_rip=val; \
         tmpg->type=HTT_GLBL_VAR | HTF_EXPORT;";
    };
    {
      id = "alternate-extern-alias";
      description = "an alternate extern does not own its bound storage";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->flags|=GVF_ALIAS;";
      snippet = "tmpg->flags|=GVF_ALIAS;";
    };
    {
      id = "import-record";
      description = "AOT imports carry both hash and global import state";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->type=HTT_GLBL_VAR | HTF_IMPORT;";
      snippet = "tmpg->type=HTT_GLBL_VAR | HTF_IMPORT;";
    };
    {
      id = "jit-extern-unresolved";
      description = "a plain JIT extern starts unresolved";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->type=HTT_GLBL_VAR|HTF_UNRESOLVED;";
      snippet = "tmpg->type=HTT_GLBL_VAR|HTF_UNRESOLVED;";
    };
    {
      id = "aot-code-heap-export";
      description = "an ordinary AOT code-heap global is exported";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->data_addr_rip=aotc->rip;";
      snippet =
        "tmpg->data_addr_rip=aotc->rip; tmpg->type=HTT_GLBL_VAR | HTF_EXPORT; \
         if (tmpex && tmpex->type & HTT_GLBL_VAR) has_alias=TRUE;";
    };
    {
      id = "aot-data-heap";
      description = "an AOT data-heap definition carries GVF_DATA_HEAP";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmphg=tmpg->heap_glbl=CAlloc(sizeof(CAOTHeapGlbl));";
      snippet =
        "tmphg=tmpg->heap_glbl=CAlloc(sizeof(CAOTHeapGlbl)); tmphg->size=j; \
         tmphg->str=StrNew(st); tmphg->next=aotc->heap_glbls; \
         aotc->heap_glbls=tmphg; tmpg->flags=GVF_DATA_HEAP;";
    };
    {
      id = "jit-data-heap";
      description = "a JIT data-heap definition carries GVF_DATA_HEAP";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->data_addr=MAlloc(j);";
      snippet = "tmpg->data_addr=MAlloc(j); tmpg->flags=GVF_DATA_HEAP;";
    };
    {
      id = "global-import-flag";
      description = "ordinary and alternate imports carry GVF_IMPORT";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->flags|=GVF_IMPORT;";
      snippet = "tmpg->flags|=GVF_IMPORT;";
    };
    {
      id = "global-extern-flag";
      description = "only a plain extern carries GVF_EXTERN";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->flags|=GVF_EXTERN;";
      snippet = "tmpg->flags|=GVF_EXTERN;";
    };
    {
      id = "function-pointer-flag";
      description = "function-pointer globals carry GVF_FUN";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->flags|=GVF_FUN;";
      snippet = "tmpg->fun_ptr=tmpf_fun_ptr; tmpg->flags|=GVF_FUN;";
    };
    {
      id = "array-flag";
      description = "array globals carry GVF_ARRAY";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpg->flags|=GVF_ARRAY;";
      snippet = "if (is_array) tmpg->flags|=GVF_ARRAY;";
    };
    {
      id = "alias-transfer";
      description = "a superseded global record becomes an alias";
      path = "Compiler/PrsStmt.HC";
      anchor = "tmpex(CHashGlblVar *)->flags|=GVF_ALIAS;";
      snippet =
        "tmpex(CHashGlblVar *)->flags|=GVF_ALIAS; tmpex(CHashGlblVar \
         *)->data_addr=tmpg->data_addr; tmpex(CHashGlblVar \
         *)->data_addr_rip=tmpg->data_addr_rip;";
    };
    {
      id = "extern-value-slot";
      description = "HashVal returns an extern record's address slot";
      path = "Kernel/KHashB.HC";
      anchor = "if (tmph(CHashGlblVar *)->flags&GVF_EXTERN)";
      snippet =
        "if (tmph(CHashGlblVar *)->flags&GVF_EXTERN) return &tmph(CHashGlblVar \
         *)->data_addr; else return tmph(CHashGlblVar *)->data_addr;";
    };
    {
      id = "alias-does-not-own-data";
      description = "deleting an alias record does not free its data address";
      path = "Kernel/KHashB.HC";
      anchor = "if (!(tmph(CHashGlblVar *)->flags&GVF_ALIAS))";
      snippet =
        "if (!(tmph(CHashGlblVar *)->flags&GVF_ALIAS)) Free(tmph(CHashGlblVar \
         *)->data_addr);";
    };
    {
      id = "map-omits-import-private";
      description = "map output omits imported and private records";
      path = "Compiler/CHash.HC";
      anchor =
        "if (tmph->src_link && !(tmph->type & (HTF_IMPORT | HTF_PRIVATE)))";
      snippet =
        "if (tmph->src_link && !(tmph->type & (HTF_IMPORT | HTF_PRIVATE))) {";
    };
    {
      id = "aot-import-publication";
      description = "AOT resolution emits used import records";
      path = "Compiler/AsmResolve.HC";
      anchor = "if (tmpex->type & (HTF_IMPORT|HTF_GOTO_LABEL)) {";
      snippet = "if (tmpex->type & (HTF_IMPORT|HTF_GOTO_LABEL)) {";
    };
    {
      id = "aot-export-publication";
      description = "AOT resolution emits an export for HTF_EXPORT";
      path = "Compiler/AsmResolve.HC";
      anchor = "if (tmpex->type & HTF_EXPORT) {";
      snippet =
        "if (tmpex->type & HTF_EXPORT) { \
         tmpie=CAlloc(sizeof(CAOTImportExport)); tmpie->type=IET_REL32_EXPORT;";
    };
    {
      id = "expression-array";
      description = "global expression parsing preserves array dimensions";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpg->flags&GVF_ARRAY) {";
      snippet = "if (tmpg->flags&GVF_ARRAY) { *_tmpad=tmpg->dim.next;";
    };
    {
      id = "expression-aot-import";
      description = "AOT global access lowers imports separately";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpg->flags & GVF_IMPORT)";
      snippet =
        "if (tmpg->flags & GVF_IMPORT) ICAdd(cc,IC_ADDR_IMPORT,tmpg,tmpc);";
    };
    {
      id = "expression-aot-data-heap";
      description = "AOT global access lowers data-heap storage separately";
      path = "Compiler/PrsExp.HC";
      anchor = "ICAdd(cc,IC_HEAP_GLBL,tmpg->heap_glbl,tmpc);";
      snippet =
        "if (tmpg->flags&GVF_DATA_HEAP) \
         ICAdd(cc,IC_HEAP_GLBL,tmpg->heap_glbl,tmpc);";
    };
    {
      id = "expression-jit-extern";
      description = "JIT extern access dereferences the address slot";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpg->flags & GVF_EXTERN) {";
      snippet =
        "if (tmpg->flags & GVF_EXTERN) { cc->abs_cnts.externs++; \
         ICAdd(cc,IC_IMM_I64,&tmpg->data_addr,tmpc); \
         ICAdd(cc,IC_DEREF,0,tmpc);";
    };
    {
      id = "expression-function-pointer";
      description = "a GVF_FUN record is exposed as a callable expression";
      path = "Compiler/PrsExp.HC";
      anchor = "if (tmpg->flags & GVF_FUN) {";
      snippet = "if (tmpg->flags & GVF_FUN) { PrsPopDeref(ps);";
    };
    {
      id = "documented-jit-extern";
      description = "the linkage guide identifies JIT extern binding";
      path = "Doc/ScopingLinkage.DD";
      anchor = "$FG,2$extern$FG$ binds a new";
      snippet = "$FG,2$extern$FG$ binds a new";
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

let expected_type_index = [ ("HTt_GLBL_VAR", 3L) ]
let expected_type_mask = [ ("HTT_GLBL_VAR", 0x8L) ]

let expected_hash_bits =
  [
    ("HTf_PRIVATE", 23L);
    ("HTf_PUBLIC", 24L);
    ("HTf_EXPORT", 25L);
    ("HTf_IMPORT", 26L);
    ("HTf_IMM", 27L);
    ("HTf_GOTO_LABEL", 28L);
    ("HTf_RESOLVED", 29L);
    ("HTf_UNRESOLVED", 30L);
    ("HTf_LOCAL", 31L);
  ]

let expected_hash_masks =
  [
    ("HTF_PRIVATE", 0x00800000L);
    ("HTF_PUBLIC", 0x01000000L);
    ("HTF_EXPORT", 0x02000000L);
    ("HTF_IMPORT", 0x04000000L);
    ("HTF_IMM", 0x08000000L);
    ("HTF_GOTO_LABEL", 0x10000000L);
    ("HTF_RESOLVE", 0x20000000L);
    ("HTF_UNRESOLVED", 0x40000000L);
    ("HTF_LOCAL", 0x80000000L);
  ]

let expected_global_masks =
  [
    ("GVF_FUN", 1L);
    ("GVF_IMPORT", 2L);
    ("GVF_EXTERN", 4L);
    ("GVF_DATA_HEAP", 8L);
    ("GVF_ALIAS", 16L);
    ("GVF_ARRAY", 32L);
  ]

let hash_consumer_ids = function
  | "HTF_PRIVATE" -> [ "private-from-option"; "map-omits-import-private" ]
  | "HTF_PUBLIC" -> [ "public-from-modifier" ]
  | "HTF_EXPORT" ->
      [
        "alternate-extern-export";
        "aot-code-heap-export";
        "aot-export-publication";
      ]
  | "HTF_IMPORT" ->
      [ "import-record"; "map-omits-import-private"; "aot-import-publication" ]
  | "HTF_GOTO_LABEL" -> [ "aot-import-publication" ]
  | "HTF_RESOLVE" -> [ "aot-export-publication" ]
  | "HTF_UNRESOLVED" -> [ "jit-extern-unresolved" ]
  | "HTF_IMM" | "HTF_LOCAL" -> []
  | name -> invalid_arg ("unknown hash flag " ^ name)

let global_consumer_ids = function
  | "GVF_FUN" -> [ "function-pointer-flag"; "expression-function-pointer" ]
  | "GVF_IMPORT" -> [ "global-import-flag"; "expression-aot-import" ]
  | "GVF_EXTERN" ->
      [ "global-extern-flag"; "extern-value-slot"; "expression-jit-extern" ]
  | "GVF_DATA_HEAP" ->
      [ "aot-data-heap"; "jit-data-heap"; "expression-aot-data-heap" ]
  | "GVF_ALIAS" ->
      [ "alternate-extern-alias"; "alias-transfer"; "alias-does-not-own-data" ]
  | "GVF_ARRAY" -> [ "array-flag"; "expression-array" ]
  | name -> invalid_arg ("unknown global flag " ^ name)

let pair_hash_flags bits masks behaviors =
  let rec pair found bits masks =
    match (bits, masks) with
    | [], [] -> Ok (List.rev found)
    | bit :: bit_rest, mask :: mask_rest ->
        let expected_mask = Int64.shift_left 1L (Int64.to_int bit.value) in
        if not (Int64.equal expected_mask mask.value) then
          error ~path:"Kernel/KernelA.HH" ~line:mask.line
            (Printf.sprintf "%s no longer matches bit %Ld from %s" mask.name
               bit.value bit.name)
        else
          pair
            ({
               bit_name = Some bit.name;
               mask_name = mask.name;
               bit_index = Int64.to_int bit.value;
               mask = mask.value;
               definition_line = mask.line;
               consumers =
                 references_for (hash_consumer_ids mask.name) behaviors;
             }
            :: found)
            bit_rest mask_rest
    | [], _ :: _ | _ :: _, [] ->
        error ~path:"Kernel/KernelA.HH"
          "hash flag bit and mask tables have different lengths"
  in
  pair [] bits masks

let bit_index mask =
  let rec find index value =
    if Int64.equal value 1L then index
    else find (index + 1) (Int64.shift_right_logical value 1)
  in
  find 0 mask

let make_global_flags definitions behaviors =
  List.map
    (fun definition ->
      {
        bit_name = None;
        mask_name = definition.name;
        bit_index = bit_index definition.value;
        mask = definition.value;
        definition_line = definition.line;
        consumers =
          references_for (global_consumer_ids definition.name) behaviors;
      })
    definitions

let ( let* ) result continuation = Result.bind result continuation

let parse ~kernel_source ~prs_stmt_source ~prs_exp_source ~khash_source
    ~chash_source ~asm_resolve_source ~scoping_source =
  let kernel_path = "Kernel/KernelA.HH" in
  let collect relevant =
    collect_definitions ~path:kernel_path ~relevant kernel_source
  in
  let type_index_name name = String.equal name "HTt_GLBL_VAR" in
  let type_mask_name name = String.equal name "HTT_GLBL_VAR" in
  let hash_bit_name name = starts_with ~prefix:"HTf_" name in
  let hash_mask_name name = starts_with ~prefix:"HTF_" name in
  let global_mask_name name = starts_with ~prefix:"GVF_" name in
  let* type_indexes = collect type_index_name in
  let* () = validate_exact ~path:kernel_path expected_type_index type_indexes in
  let* type_masks = collect type_mask_name in
  let* () = validate_exact ~path:kernel_path expected_type_mask type_masks in
  let* hash_bits = collect hash_bit_name in
  let* () = validate_exact ~path:kernel_path expected_hash_bits hash_bits in
  let* hash_masks = collect hash_mask_name in
  let* () = validate_exact ~path:kernel_path expected_hash_masks hash_masks in
  let* global_masks = collect global_mask_name in
  let* () =
    validate_exact ~path:kernel_path expected_global_masks global_masks
  in
  let sources =
    [
      ("Compiler/PrsStmt.HC", prs_stmt_source);
      ("Compiler/PrsExp.HC", prs_exp_source);
      ("Kernel/KHashB.HC", khash_source);
      ("Compiler/CHash.HC", chash_source);
      ("Compiler/AsmResolve.HC", asm_resolve_source);
      ("Doc/ScopingLinkage.DD", scoping_source);
    ]
  in
  let* behaviors = collect_behaviors sources in
  let* hash_flags = pair_hash_flags hash_bits hash_masks behaviors in
  let type_index = List.hd type_indexes in
  let type_mask = List.hd type_masks in
  if
    not
      (Int64.equal type_mask.value
         (Int64.shift_left 1L (Int64.to_int type_index.value)))
  then
    error ~path:kernel_path ~line:type_mask.line
      "HTT_GLBL_VAR no longer matches HTt_GLBL_VAR"
  else
    Ok
      {
        global_type =
          {
            index_name = type_index.name;
            mask_name = type_mask.name;
            type_index = Int64.to_int type_index.value;
            type_mask = type_mask.value;
            index_definition_line = type_index.line;
            mask_definition_line = type_mask.line;
          };
        hash_flags;
        global_flags = make_global_flags global_masks behaviors;
        behaviors;
      }
