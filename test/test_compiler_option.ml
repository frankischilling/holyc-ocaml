module Option = Holyc_lib.Compiler_option

let exact_registry () =
  Alcotest.(check (list string))
    "source names"
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
    (List.map Option.to_string Option.all);
  Alcotest.(check (list int))
    "bit indices"
    [ 0; 1; 16; 17; 18; 19; 32; 33; 34; 35; 36; 37 ]
    (List.map (fun option -> (Option.info option).bit_index) Option.all)

let typed_lookups () =
  Alcotest.(check bool)
    "name lookup" true
    (Option.of_string "OPTf_KEEP_PRIVATE" = Some Option.Keep_private);
  Alcotest.(check bool)
    "bit lookup" true
    (Option.of_bit_index 0x24 = Some Option.No_builtin_const);
  Alcotest.(check bool) "gap lookup" true (Option.of_bit_index 7 = None);
  Alcotest.(check bool) "negative lookup" true (Option.of_bit_index (-1) = None);
  Alcotest.(check bool)
    "unknown name" true
    (Option.of_string "OPTf_UNKNOWN" = None)

let defaults_use_target_masks () =
  Alcotest.(check int64) "known mask" 0x3f000f0003L Option.known_mask;
  Alcotest.(check int64) "initial mask" 0x90000L Option.initial_mask;
  Alcotest.(check bool)
    "unused warning enabled" true
    (Option.is_enabled ~mask:Option.initial_mask Option.Warn_unused_var);
  Alcotest.(check bool)
    "header warning enabled" true
    (Option.is_enabled ~mask:Option.initial_mask Option.Warn_header_mismatch);
  Alcotest.(check bool)
    "trace disabled" false
    (Option.is_enabled ~mask:Option.initial_mask Option.Trace)

let pure_set_returns_previous_state () =
  let enabled, previous = Option.set ~mask:0L Option.Trace true in
  Alcotest.(check bool) "previously disabled" false previous;
  Alcotest.(check int64) "trace mask" 2L enabled;
  let disabled, previous = Option.set ~mask:enabled Option.Trace false in
  Alcotest.(check bool) "previously enabled" true previous;
  Alcotest.(check int64) "trace cleared" 0L disabled

let scopes_and_source_status () =
  let externs = Option.info Option.Externs_to_imports in
  Alcotest.(check bool)
    "extern parsing option" true
    (List.mem Option.Parsing externs.phases);
  Alcotest.(check bool)
    "extern linkage option" true
    (List.mem Option.Linkage externs.phases);
  let no_reg = Option.info Option.No_reg_var in
  Alcotest.(check bool)
    "optimizer option" true
    (List.mem Option.Optimization no_reg.phases);
  let globals = Option.info Option.Globals_on_data_heap in
  Alcotest.(check bool)
    "allocation option" true
    (List.mem Option.Allocation globals.phases);
  Alcotest.(check bool)
    "linkage option" true
    (List.mem Option.Linkage globals.phases);
  let use_imm64 = Option.info Option.Use_imm64 in
  Alcotest.(check bool)
    "immediate optimization option" true
    (List.mem Option.Optimization use_imm64.phases);
  Alcotest.(check bool)
    "immediate emission option" true
    (List.mem Option.Code_emission use_imm64.phases);
  Alcotest.(check bool)
    "source marks USE_IMM64 incomplete" true
    (use_imm64.source_status = Option.Source_marked_incomplete)

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Option.reference_commit;
  Alcotest.(check int) "source count" 16 (List.length Option.sources);
  Alcotest.(check (list (pair int int)))
    "intentional gaps"
    [ (2, 15); (20, 31) ]
    Option.intentional_gaps;
  Alcotest.(check string)
    "controller state" "Fs->last_cc->opts" Option.api.state_expression;
  Alcotest.(check bool)
    "Option returns previous state" true Option.api.set_returns_previous;
  Alcotest.(check (pair string int))
    "BEqu source" ("Kernel/KUtils.HC", 88)
    (Option.api.bit_set_source.path, Option.api.bit_set_source.line)

let consumer_lines () =
  let use_imm64 = Option.info Option.Use_imm64 in
  Alcotest.(check (list (pair string int)))
    "relocation consumers"
    [ ("Compiler/OptPass789A.HC", 359); ("Compiler/BackFA.HC", 285) ]
    (List.map
       (fun (reference : Option.source_reference) ->
         (reference.path, reference.line))
       use_imm64.consumers);
  let echo = Option.info Option.Echo in
  Alcotest.(check bool)
    "lexical echo consumer" true
    (List.exists
       (fun (reference : Option.source_reference) ->
         String.equal reference.path "Compiler/Lex.HC" && reference.line = 257)
       echo.consumers)

let tests =
  [
    Alcotest.test_case "exact registry" `Quick exact_registry;
    Alcotest.test_case "typed lookups" `Quick typed_lookups;
    Alcotest.test_case "default mask" `Quick defaults_use_target_masks;
    Alcotest.test_case "pure set" `Quick pure_set_returns_previous_state;
    Alcotest.test_case "scope and source status" `Quick scopes_and_source_status;
    Alcotest.test_case "provenance" `Quick provenance;
    Alcotest.test_case "consumer lines" `Quick consumer_lines;
  ]
