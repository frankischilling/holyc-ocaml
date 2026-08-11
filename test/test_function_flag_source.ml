module Source = Function_flag_source

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let replace_once text ~needle ~replacement =
  let needle_length = String.length needle in
  let rec find offset =
    if offset + needle_length > String.length text then
      Alcotest.failf "test fixture does not contain %S" needle
    else if String.sub text offset needle_length = needle then offset
    else find (offset + 1)
  in
  let offset = find 0 in
  String.sub text 0 offset ^ replacement
  ^ String.sub text (offset + needle_length)
      (String.length text - offset - needle_length)

let kernel_source =
  String.concat "\n"
    [
      "#define Cf_EXTERN 0";
      "#define Cf_INTERNAL_TYPE 1";
      "public class CHashClass:CHashSrcSym";
      "{";
      "  U16 flags;";
      "};";
      "#define Ff_INTERRUPT 8";
      "#define Ff_HASERRCODE 9";
      "#define Ff_ARGPOP 10";
      "#define Ff_NOARGPOP 11";
      "#define Ff_INTERNAL 12";
      "#define Ff__EXTERN 13";
      "#define Ff_DOT_DOT_DOT 14";
      "#define Ff_RET1 15";
      "public class CHashFun:CHashClass";
      "{";
      "};";
      "";
    ]

let compiler_source =
  String.concat "\n"
    [
      "#define FSF_PUBLIC 0x01";
      "#define FSF_ASM 0x02";
      "#define FSF_STATIC 0x04";
      "#define FSF__ 0x08";
      "#define FSF_INTERRUPT (1<<Ff_INTERRUPT)";
      "#define FSF_HASERRCODE (1<<Ff_HASERRCODE)";
      "#define FSF_ARGPOP (1<<Ff_ARGPOP)";
      "#define FSF_NOARGPOP (1<<Ff_NOARGPOP)";
      "#define FSG_FUN_FLAGS1 \
       (FSF_INTERRUPT|FSF_HASERRCODE|FSF_ARGPOP|FSF_NOARGPOP)";
      "#define FSG_FUN_FLAGS2 (FSG_FUN_FLAGS1|FSF_PUBLIC)";
      "";
    ]

let prs_stmt_source =
  String.concat "\n"
    [
      "tmpf->flags|=fsp_flags&FSG_FUN_FLAGS1;";
      "BEqu(&tmpf->type,HTf_PUBLIC,fsp_flags&FSF_PUBLIC);";
      "if (0<tmpf->arg_cnt<<3<=I16_MAX && !Bt(&tmpf->flags,Ff_DOT_DOT_DOT))";
      "  LBts(&tmpf->flags,Ff_RET1);";
      "if ((Bt(&tmp_try->flags,Ff_RET1) ||";
      "  Bt(&tmp_try->flags,Ff_ARGPOP)) && !Bt(&tmp_try->flags,Ff_NOARGPOP))";
      "  use_cleanup();";
      "case KW_STATIC:";
      "  fsp_flags=FSF_STATIC|fsp_flags&FSF_ASM;";
      "case KW_INTERRUPT:";
      "  fsp_flags=FSF_INTERRUPT|FSF_NOARGPOP|";
      "    fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);";
      "case KW_HASERRCODE:";
      "  fsp_flags=FSF_HASERRCODE|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);";
      "case KW_ARGPOP:";
      "  fsp_flags=FSF_ARGPOP|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);";
      "case KW_NOARGPOP:";
      "  fsp_flags=FSF_NOARGPOP|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);";
      "case KW_PUBLIC:";
      "  fsp_flags=FSF_PUBLIC|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);";
      "if (*name=='_')";
      "  fsp_flags|=FSF__;";
      "if (*import_name=='_')";
      "  fsp_flags|=FSF__;";
      "Bts(&tmpf->flags,Ff_INTERNAL);";
      "Bts(&tmpf->flags,Ff__EXTERN);";
      "Bt(&tmpf->flags,Cf_EXTERN);";
      "";
    ]

let prs_var_source = "Bts(&tmpf->flags,Ff_DOT_DOT_DOT);\n"

let prs_exp_source =
  String.concat "\n"
    [
      "if (Bt(&tmpf->flags,Ff_INTERNAL))";
      "  ICAdd(cc,tmpf->exe_addr,0,tmpf->return_class);";
      "if ((Bt(&tmpf->flags,Ff_RET1) || Bt(&tmpf->flags,Ff_ARGPOP)) &&";
      "    !Bt(&tmpf->flags,Ff_NOARGPOP)) {";
      "  use_cleanup();";
      "}";
      "Bt(&tmpc->flags,Cf_INTERNAL_TYPE);";
      "";
    ]

let opt_pass3_source =
  String.concat "\n"
    [ "if (Bt(&cc->htc.fun->flags,Ff_DOT_DOT_DOT))"; "  member_cnt+=2;"; "" ]

let opt_pass6_source =
  String.concat "\n"
    [ "if (Bt(&tmpf->flags,Ff_INTERNAL))"; "  clobbered_stk_tmp_mask=0;"; "" ]

let opt_pass789a_source =
  String.concat "\n"
    [
      "if (Bt(&cc->htc.fun->flags,Ff_INTERRUPT))";
      "  ICPopRegs(tmpi,REGG_CLOBBERED);";
      "if (cc->htc.fun && Bt(&cc->htc.fun->flags,Ff_INTERRUPT)) {";
      "  if (Bt(&cc->htc.fun->flags,Ff_HASERRCODE))";
      "    ICAddRSP(tmpi,8);";
      "} else if (cc->htc.fun && cc->htc.fun->arg_cnt &&";
      "    (Bt(&cc->htc.fun->flags,Ff_RET1) ||";
      "     Bt(&cc->htc.fun->flags,Ff_ARGPOP)) &&";
      "    !Bt(&cc->htc.fun->flags,Ff_NOARGPOP)) {";
      "  ICU8(tmpi,0xC2);";
      "  ICU16(tmpi,cc->htc.fun->arg_cnt<<3);";
      "}";
      "if (Bt(&cc->htc.fun->flags,Ff_INTERRUPT))";
      "  ICPushRegs(tmpi,REGG_CLOBBERED);";
      "";
    ]

let fun_seg_source =
  String.concat "\n"
    [
      "if (!Bt(&tmpex(CHashFun *)->flags,Cf_EXTERN) &&";
      "    !Bt(&tmpex(CHashFun *)->flags,Ff_INTERNAL))";
      "  use_symbol();";
      "";
    ]

let parse ?(kernel = kernel_source) ?(compiler = compiler_source)
    ?(prs_stmt = prs_stmt_source) ?(prs_var = prs_var_source)
    ?(prs_exp = prs_exp_source) ?(opt_pass3 = opt_pass3_source)
    ?(opt_pass6 = opt_pass6_source) ?(opt_pass789a = opt_pass789a_source)
    ?(fun_seg = fun_seg_source) () =
  Source.parse ~kernel_source:kernel ~compiler_source:compiler
    ~prs_stmt_source:prs_stmt ~prs_var_source:prs_var ~prs_exp_source:prs_exp
    ~opt_pass3_source:opt_pass3 ~opt_pass6_source:opt_pass6
    ~opt_pass789a_source:opt_pass789a ~fun_seg_source:fun_seg

let parse_ok ?kernel ?compiler ?prs_stmt ?prs_var ?prs_exp ?opt_pass3 ?opt_pass6
    ?opt_pass789a ?fun_seg () =
  match
    parse ?kernel ?compiler ?prs_stmt ?prs_var ?prs_exp ?opt_pass3 ?opt_pass6
      ?opt_pass789a ?fun_seg ()
  with
  | Ok tables -> tables
  | Error problem -> Alcotest.fail (Source.error_to_string problem)

let expect_error ?kernel ?compiler ?prs_stmt ?prs_var ?prs_exp ?opt_pass3
    ?opt_pass6 ?opt_pass789a ?fun_seg needle =
  match
    parse ?kernel ?compiler ?prs_stmt ?prs_var ?prs_exp ?opt_pass3 ?opt_pass6
      ?opt_pass789a ?fun_seg ()
  with
  | Ok _ -> Alcotest.failf "expected an error containing %S" needle
  | Error problem ->
      let rendered = Source.error_to_string problem in
      Alcotest.(check bool) "error text" true (contains ~needle rendered)

let complete_registry () =
  let tables = parse_ok () in
  Alcotest.(check int) "shared flag count" 2 (List.length tables.shared_flags);
  Alcotest.(check int) "stored flag count" 8 (List.length tables.function_flags);
  Alcotest.(check int) "staging flag count" 8 (List.length tables.staging_flags);
  Alcotest.(check int) "group count" 2 (List.length tables.groups);
  Alcotest.(check int) "transition count" 7 (List.length tables.transitions);
  Alcotest.(check (list int))
    "stored bits"
    [ 8; 9; 10; 11; 12; 13; 14; 15 ]
    (List.map
       (fun (entry : Source.flag_entry) -> entry.bit_index)
       tables.function_flags)

let parser_masks_and_groups () =
  let tables = parse_ok () in
  Alcotest.(check (list int64))
    "staging masks"
    [ 0x1L; 0x2L; 0x4L; 0x8L; 0x100L; 0x200L; 0x400L; 0x800L ]
    (List.map
       (fun (entry : Source.flag_entry) -> entry.mask)
       tables.staging_flags);
  Alcotest.(check (list int64))
    "group masks" [ 0xF00L; 0xF01L ]
    (List.map (fun (entry : Source.group_entry) -> entry.mask) tables.groups)

let source_transitions () =
  let tables = parse_ok () in
  let find name =
    List.find
      (fun (entry : Source.transition_entry) -> String.equal entry.name name)
      tables.transitions
  in
  Alcotest.(check int64)
    "interrupt adds noargpop" 0x903L
    (Source.apply_transition (find "Interrupt") 0x003L);
  Alcotest.(check int64)
    "static keeps only assembly" 0x006L
    (Source.apply_transition (find "Static") 0xFFFL);
  Alcotest.(check int64)
    "underscore is additive" 0x1008L
    (Source.apply_transition (find "Underscore_name") 0x1000L)

let rejects_duplicate_definition () =
  expect_error
    ~kernel:("#define Cf_EXTERN 0\n" ^ kernel_source)
    "Cf_EXTERN is defined more than once"

let rejects_missing_definition () =
  let changed =
    replace_once kernel_source ~needle:"#define Ff_RET1 15\n" ~replacement:""
  in
  expect_error ~kernel:changed "missing Ff_RET1"

let rejects_reordered_definition () =
  let changed =
    replace_once kernel_source
      ~needle:"#define Ff_INTERRUPT 8\n#define Ff_HASERRCODE 9"
      ~replacement:"#define Ff_HASERRCODE 9\n#define Ff_INTERRUPT 8"
  in
  expect_error ~kernel:changed "requires Ff_INTERRUPT here"

let rejects_changed_bit_index () =
  let changed =
    replace_once kernel_source ~needle:"#define Ff_RET1 15"
      ~replacement:"#define Ff_RET1 16"
  in
  expect_error ~kernel:changed "must remain bit index 15"

let rejects_changed_inheritance () =
  let changed =
    replace_once kernel_source ~needle:"public class CHashFun:CHashClass"
      ~replacement:"public class CHashFun:CHashSrcSym"
  in
  expect_error ~kernel:changed "CHashFun declaration is missing"

let rejects_changed_flag_storage () =
  let changed =
    replace_once kernel_source ~needle:"  U16 flags;"
      ~replacement:"  U32 flags;"
  in
  expect_error ~kernel:changed "CHashClass must retain its U16 flag field"

let rejects_unknown_expression_name () =
  let changed =
    replace_once compiler_source ~needle:"(1<<Ff_INTERRUPT)"
      ~replacement:"(1<<Ff_UNKNOWN)"
  in
  expect_error ~compiler:changed "references unknown Ff_UNKNOWN"

let rejects_equivalent_expression_rewrite () =
  let changed =
    replace_once compiler_source ~needle:"(1<<Ff_INTERRUPT)"
      ~replacement:"0x100"
  in
  expect_error ~compiler:changed "must retain source expression"

let rejects_unsupported_expression () =
  let changed =
    replace_once compiler_source ~needle:"#define FSF_PUBLIC 0x01"
      ~replacement:"#define FSF_PUBLIC 0x01+1"
  in
  expect_error ~compiler:changed "unsupported '+'"

let rejects_changed_group () =
  let changed =
    replace_once compiler_source
      ~needle:"FSF_INTERRUPT|FSF_HASERRCODE|FSF_ARGPOP|FSF_NOARGPOP"
      ~replacement:"FSF_INTERRUPT|FSF_HASERRCODE|FSF_ARGPOP"
  in
  expect_error ~compiler:changed "FSG_FUN_FLAGS1 evaluates"

let rejects_changed_modifier_transition () =
  let changed =
    replace_once prs_stmt_source ~needle:"FSF_INTERRUPT|FSF_NOARGPOP|"
      ~replacement:"FSF_INTERRUPT|"
  in
  expect_error ~prs_stmt:changed
    "required source behavior near \"case KW_INTERRUPT:\" is missing"

let rejects_changed_ret1_condition () =
  let changed =
    replace_once prs_stmt_source ~needle:"0<tmpf->arg_cnt<<3<=I16_MAX"
      ~replacement:"tmpf->arg_cnt<<3<=I16_MAX"
  in
  expect_error ~prs_stmt:changed "required source behavior"

let ignores_noncode_flag_names () =
  let changed =
    prs_exp_source
    ^ "// Ff_UNKNOWN\n/* FSF_UNKNOWN */\n\"Cf_UNKNOWN\";\n'Ff_UNKNOWN';\n"
  in
  ignore (parse_ok ~prs_exp:changed ())

let rejects_unknown_consumer () =
  expect_error
    ~prs_exp:(prs_exp_source ^ "Ff_UNKNOWN;\n")
    "unknown function flag Ff_UNKNOWN"

let rejects_unterminated_consumer_comment () =
  expect_error
    ~prs_exp:(prs_exp_source ^ "/* Ff_RET1")
    "unterminated block comment"

let rejects_checksum_mismatch () =
  match Source.verify_sha256 ~expected:(String.make 64 '0') kernel_source with
  | Ok () -> Alcotest.fail "expected a checksum mismatch"
  | Error problem ->
      Alcotest.(check bool)
        "checksum diagnostic" true
        (contains ~needle:"source SHA-256" (Source.error_to_string problem))

let normalizes_checkout_line_endings () =
  let expected =
    "911169ddaaf146aff539f58c26c489af3b892dff0fe283c1c264c65ae5aa59a2"
  in
  Alcotest.(check bool)
    "LF checksum" true
    (Result.is_ok (Source.verify_sha256 ~expected "a\nb\n"));
  Alcotest.(check bool)
    "CRLF checksum" true
    (Result.is_ok (Source.verify_sha256 ~expected "a\r\nb\r\n"))

let deterministic_parse () =
  Alcotest.(check bool) "same tables" true (parse () = parse ())

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned path = read_file ("../third_party/TempleOS/" ^ path)

let parses_pinned_sources () =
  let tables =
    match
      Source.parse
        ~kernel_source:(pinned "Kernel/KernelA.HH")
        ~compiler_source:(pinned "Compiler/CompilerA.HH")
        ~prs_stmt_source:(pinned "Compiler/PrsStmt.HC")
        ~prs_var_source:(pinned "Compiler/PrsVar.HC")
        ~prs_exp_source:(pinned "Compiler/PrsExp.HC")
        ~opt_pass3_source:(pinned "Compiler/OptPass3.HC")
        ~opt_pass6_source:(pinned "Compiler/OptPass6.HC")
        ~opt_pass789a_source:(pinned "Compiler/OptPass789A.HC")
        ~fun_seg_source:(pinned "Kernel/FunSeg.HC")
    with
    | Ok tables -> tables
    | Error problem -> Alcotest.fail (Source.error_to_string problem)
  in
  Alcotest.(check int)
    "pinned stored flags" 8
    (List.length tables.function_flags);
  let interrupt = List.hd tables.function_flags in
  Alcotest.(check (list (pair string int)))
    "interrupt consumers"
    [
      ("Compiler/OptPass789A.HC", 389);
      ("Compiler/OptPass789A.HC", 405);
      ("Compiler/OptPass789A.HC", 703);
    ]
    (List.map
       (fun (reference : Source.source_reference) ->
         (reference.path, reference.line))
       interrupt.consumers);
  Alcotest.(check (pair string int))
    "caller cleanup source"
    ("Compiler/PrsExp.HC", 572)
    (tables.behavior.caller_cleanup.path, tables.behavior.caller_cleanup.line)

let tests =
  [
    Alcotest.test_case "complete registry" `Quick complete_registry;
    Alcotest.test_case "parser masks and groups" `Quick parser_masks_and_groups;
    Alcotest.test_case "source transitions" `Quick source_transitions;
    Alcotest.test_case "duplicate definition" `Quick
      rejects_duplicate_definition;
    Alcotest.test_case "missing definition" `Quick rejects_missing_definition;
    Alcotest.test_case "definition order" `Quick rejects_reordered_definition;
    Alcotest.test_case "bit index" `Quick rejects_changed_bit_index;
    Alcotest.test_case "function inheritance" `Quick rejects_changed_inheritance;
    Alcotest.test_case "flag storage" `Quick rejects_changed_flag_storage;
    Alcotest.test_case "unknown expression name" `Quick
      rejects_unknown_expression_name;
    Alcotest.test_case "expression provenance" `Quick
      rejects_equivalent_expression_rewrite;
    Alcotest.test_case "expression grammar" `Quick
      rejects_unsupported_expression;
    Alcotest.test_case "group expression" `Quick rejects_changed_group;
    Alcotest.test_case "modifier transition" `Quick
      rejects_changed_modifier_transition;
    Alcotest.test_case "RET1 condition" `Quick rejects_changed_ret1_condition;
    Alcotest.test_case "comments and literals" `Quick ignores_noncode_flag_names;
    Alcotest.test_case "unknown consumer" `Quick rejects_unknown_consumer;
    Alcotest.test_case "unterminated consumer comment" `Quick
      rejects_unterminated_consumer_comment;
    Alcotest.test_case "checksum mismatch" `Quick rejects_checksum_mismatch;
    Alcotest.test_case "checkout line endings" `Quick
      normalizes_checkout_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
    Alcotest.test_case "pinned sources" `Quick parses_pinned_sources;
  ]
