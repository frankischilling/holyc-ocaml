module Flag = Holyc_lib.Function_flag

let shared_registry () =
  Alcotest.(check (list string))
    "shared names" [ "Cf_EXTERN"; "Cf_INTERNAL_TYPE" ]
    (List.map Flag.Shared.to_source_name Flag.Shared.all);
  Alcotest.(check (list int))
    "shared bits" [ 0; 1 ]
    (List.map Flag.Shared.to_bit_index Flag.Shared.all)

let stored_registry () =
  Alcotest.(check (list string))
    "stored names"
    [
      "Ff_INTERRUPT";
      "Ff_HASERRCODE";
      "Ff_ARGPOP";
      "Ff_NOARGPOP";
      "Ff_INTERNAL";
      "Ff__EXTERN";
      "Ff_DOT_DOT_DOT";
      "Ff_RET1";
    ]
    (List.map Flag.Stored.to_source_name Flag.Stored.all);
  Alcotest.(check (list int))
    "stored bits" [ 8; 9; 10; 11; 12; 13; 14; 15 ]
    (List.map Flag.Stored.to_bit_index Flag.Stored.all);
  List.iter
    (fun stored ->
      Alcotest.(check bool)
        "stored round trip" true
        (Flag.Stored.of_source_name (Flag.Stored.to_source_name stored)
        = Some stored))
    Flag.Stored.all

let staging_registry () =
  Alcotest.(check (list int64))
    "staging masks"
    [ 0x1L; 0x2L; 0x4L; 0x8L; 0x100L; 0x200L; 0x400L; 0x800L ]
    (List.map Flag.Staging.to_mask Flag.Staging.all);
  Alcotest.(check int64)
    "stored group" 0xF00L
    (Flag.Group.to_mask Flag.Group.Function_flags);
  Alcotest.(check int64)
    "public group" 0xF01L
    (Flag.Group.to_mask Flag.Group.Function_and_public_flags);
  Alcotest.(check (list string))
    "source group terms"
    [ "FSG_FUN_FLAGS1"; "FSF_PUBLIC" ]
    (Flag.Group.info Flag.Group.Function_and_public_flags).source_terms

let expected_modifier mask = function
  | Flag.Modifier.Static -> Int64.logor (Int64.logand mask 0x2L) 0x4L
  | Flag.Modifier.Interrupt -> Int64.logor (Int64.logand mask 0xF03L) 0x900L
  | Flag.Modifier.Has_error_code ->
      Int64.logor (Int64.logand mask 0xF03L) 0x200L
  | Flag.Modifier.Argument_pop ->
      Int64.logor (Int64.logand mask 0xF03L) 0x400L
  | Flag.Modifier.No_argument_pop ->
      Int64.logor (Int64.logand mask 0xF03L) 0x800L
  | Flag.Modifier.Public -> Int64.logor (Int64.logand mask 0xF03L) 0x1L
  | Flag.Modifier.Underscore_name -> Int64.logor mask 0x8L

let exhaustive_modifier_transitions () =
  for value = 0 to 0xFFF do
    let mask = Int64.of_int value in
    List.iter
      (fun modifier ->
        let expected = expected_modifier mask modifier in
        let actual = Flag.apply_modifier ~mask modifier in
        if not (Int64.equal expected actual) then
          Alcotest.failf "modifier %s changed 0x%Lx to 0x%Lx, expected 0x%Lx"
            (Flag.Modifier.to_spelling modifier) mask actual expected)
      Flag.Modifier.all
  done

let staging_transfer_keeps_concepts_separate () =
  let staged = 0xF0FL in
  Alcotest.(check int64)
    "stored transfer" 0xF00L (Flag.stored_mask_of_staging staged);
  Alcotest.(check bool) "public is type state" true (Flag.public_requested staged);
  Alcotest.(check bool) "assembly is parser state" true (Flag.assembly_mode staged);
  Alcotest.(check bool)
    "public is not stored" true
    (Flag.stored_of_staging Flag.Staging.Public = None);
  Alcotest.(check bool)
    "interrupt maps to stored bit" true
    (Flag.stored_of_staging Flag.Staging.Interrupt = Some Flag.Stored.Interrupt)

let ret1_boundaries () =
  let check name expected argument_count variadic =
    Alcotest.(check bool)
      name expected (Flag.derives_ret1 ~argument_count ~variadic)
  in
  check "negative" false (-1L) false;
  check "zero" false 0L false;
  check "one argument" true 1L false;
  check "largest RET imm16 argument block" true 4095L false;
  check "too large" false 4096L false;
  check "variadic" false 1L true

let caller_cleanup_truth_table () =
  let bit flag = Flag.Stored.to_mask flag in
  for state = 0 to 7 do
    let ret1 = state land 1 <> 0 in
    let argpop = state land 2 <> 0 in
    let noargpop = state land 4 <> 0 in
    let mask =
      Int64.logor
        (if ret1 then bit Flag.Stored.Ret1 else 0L)
        (Int64.logor
           (if argpop then bit Flag.Stored.Argument_pop else 0L)
           (if noargpop then bit Flag.Stored.No_argument_pop else 0L))
    in
    Alcotest.(check bool)
      "cleanup predicate" ((ret1 || argpop) && not noargpop)
      (Flag.caller_expects_callee_pop ~stored_mask:mask)
  done

let interrupt_and_internal_predicates () =
  let interrupt = Flag.Stored.to_mask Flag.Stored.Interrupt in
  let has_error = Flag.Stored.to_mask Flag.Stored.Has_error_code in
  Alcotest.(check bool)
    "interrupt alone" false
    (Flag.interrupt_discards_error_code ~stored_mask:interrupt);
  Alcotest.(check bool)
    "interrupt error code" true
    (Flag.interrupt_discards_error_code
       ~stored_mask:(Int64.logor interrupt has_error));
  Alcotest.(check bool)
    "haserrcode without interrupt" false
    (Flag.interrupt_discards_error_code ~stored_mask:has_error);
  Alcotest.(check bool)
    "internal" true
    (Flag.is_internal ~stored_mask:(Flag.Stored.to_mask Flag.Stored.Internal))

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Flag.reference_commit;
  Alcotest.(check int) "source count" 9 (List.length Flag.sources);
  Alcotest.(check (pair string int))
    "automatic RET1" ("Compiler/PrsStmt.HC", 116)
    ( Flag.behavior_sources.automatic_ret1.path,
      Flag.behavior_sources.automatic_ret1.line );
  Alcotest.(check (pair string int))
    "caller cleanup" ("Compiler/PrsExp.HC", 572)
    ( Flag.behavior_sources.caller_cleanup.path,
      Flag.behavior_sources.caller_cleanup.line );
  Alcotest.(check (pair string int))
    "interrupt save" ("Compiler/OptPass789A.HC", 704)
    ( Flag.behavior_sources.interrupt_save.path,
      Flag.behavior_sources.interrupt_save.line )

let consumer_metadata () =
  let interrupt = Flag.Stored.info Flag.Stored.Interrupt in
  Alcotest.(check (list (pair string int)))
    "interrupt consumers"
    [
      ("Compiler/OptPass789A.HC", 389);
      ("Compiler/OptPass789A.HC", 405);
      ("Compiler/OptPass789A.HC", 703);
    ]
    (List.map
       (fun (reference : Flag.source_reference) ->
         (reference.path, reference.line))
       interrupt.consumers);
  let underscore = Flag.Modifier.info Flag.Modifier.Underscore_name in
  Alcotest.(check int) "underscore paths" 2 (List.length underscore.sources)

let tests =
  [
    Alcotest.test_case "shared registry" `Quick shared_registry;
    Alcotest.test_case "stored registry" `Quick stored_registry;
    Alcotest.test_case "staging registry" `Quick staging_registry;
    Alcotest.test_case "modifier transitions" `Quick exhaustive_modifier_transitions;
    Alcotest.test_case "staging transfer" `Quick
      staging_transfer_keeps_concepts_separate;
    Alcotest.test_case "RET1 boundaries" `Quick ret1_boundaries;
    Alcotest.test_case "caller cleanup" `Quick caller_cleanup_truth_table;
    Alcotest.test_case "interrupt and internal" `Quick
      interrupt_and_internal_predicates;
    Alcotest.test_case "provenance" `Quick provenance;
    Alcotest.test_case "consumer metadata" `Quick consumer_metadata;
  ]
