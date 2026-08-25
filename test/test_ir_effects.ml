module Effects = Holyc_lib.Ir_effects
module Opcode = Holyc_lib.Ir_opcode

let access_testable =
  Alcotest.testable
    (fun formatter access ->
      Format.pp_print_string formatter (Effects.access_name access))
    ( = )

let check_access message expected actual =
  Alcotest.check access_testable message expected actual

let complete_table () =
  Alcotest.(check int) "checked opcodes" 185 (List.length Opcode.all);
  List.iter
    (fun opcode ->
      let classification = Effects.classify opcode in
      Alcotest.(check bool)
        (Opcode.to_source_name opcode ^ " has a rendering")
        true
        (String.length (Effects.human classification) > 0);
      Alcotest.(check bool)
        (Opcode.to_source_name opcode ^ " keeps not_const")
        (Opcode.info opcode).prevents_constant_folding
        classification.prevents_constant_folding)
    Opcode.all

let memory_families () =
  List.iter
    (fun opcode ->
      check_access
        (Opcode.to_source_name opcode)
        Effects.Read (Effects.classify opcode).memory)
    [ Opcode.Ic_deref; Opcode.Ic_bt; Opcode.Ic_br_bt; Opcode.Ic_strlen ];
  List.iter
    (fun opcode ->
      check_access
        (Opcode.to_source_name opcode)
        Effects.Read_write (Effects.classify opcode).memory)
    [
      Opcode.Ic_deref_pp;
      Opcode.Ic__pp;
      Opcode.Ic_pp_;
      Opcode.Ic_add_equ;
      Opcode.Ic_bts;
      Opcode.Ic_br_not_btc;
      Opcode.Ic_swap_i64;
      Opcode.Ic_que_rem;
      Opcode.Ic_mov;
    ];
  check_access "plain assignment" Effects.Write
    (Effects.classify Opcode.Ic_assign).memory;
  check_access "queue initialization" Effects.Write
    (Effects.classify Opcode.Ic_que_init).memory

let calls_and_assembly () =
  let calls =
    [
      (Opcode.Ic_sub_call, Effects.Local_call);
      (Opcode.Ic_call, Effects.Direct_call);
      (Opcode.Ic_call_indirect, Effects.Indirect_call);
      (Opcode.Ic_call_indirect2, Effects.Indirect_call);
      (Opcode.Ic_call_import, Effects.Import_call);
      (Opcode.Ic_call_extern, Effects.Extern_call);
    ]
  in
  List.iter
    (fun (opcode, expected_call) ->
      let classification = Effects.classify opcode in
      check_access
        (Opcode.to_source_name opcode ^ " memory")
        Effects.Opaque classification.memory;
      check_access
        (Opcode.to_source_name opcode ^ " machine")
        Effects.Opaque classification.machine;
      Alcotest.(check bool)
        (Opcode.to_source_name opcode ^ " call")
        true
        (classification.call = expected_call))
    calls;
  let assembly = Effects.classify Opcode.Ic_asm in
  Alcotest.(check bool) "inline assembly" true assembly.inline_assembly;
  List.iter
    (check_access "opaque assembly domain" Effects.Opaque)
    [
      assembly.memory;
      assembly.stack;
      assembly.machine;
      assembly.port;
      assembly.cache_tlb;
      assembly.clock;
    ]

let stack_and_frames () =
  check_access "call start writes saved-register stack" Effects.Write
    (Effects.classify Opcode.Ic_call_start).stack;
  check_access "call end reads saved-register stack" Effects.Read
    (Effects.classify Opcode.Ic_call_end).stack;
  check_access "enter updates frame stack" Effects.Read_write
    (Effects.classify Opcode.Ic_enter).stack;
  check_access "return reads return address" Effects.Read
    (Effects.classify Opcode.Ic_ret).stack;
  check_access "push writes stack" Effects.Write
    (Effects.classify Opcode.Ic_push).stack;
  check_access "pop reads stack" Effects.Read
    (Effects.classify Opcode.Ic_pop).stack;
  check_access "ADD_RSP mutates stack pointer" Effects.Write
    (Effects.classify Opcode.Ic_add_rsp).machine

let hardware_domains () =
  List.iter
    (fun opcode ->
      check_access
        (Opcode.to_source_name opcode)
        Effects.Read (Effects.classify opcode).port)
    [ Opcode.Ic_in_u8; Opcode.Ic_in_u16; Opcode.Ic_in_u32 ];
  List.iter
    (fun opcode ->
      check_access
        (Opcode.to_source_name opcode)
        Effects.Write (Effects.classify opcode).port)
    [ Opcode.Ic_out_u8; Opcode.Ic_out_u16; Opcode.Ic_out_u32 ];
  check_access "cache flush" Effects.Write
    (Effects.classify Opcode.Ic_clflush).cache_tlb;
  check_access "page invalidation" Effects.Write
    (Effects.classify Opcode.Ic_invlpg).cache_tlb;
  check_access "timestamp read" Effects.Read
    (Effects.classify Opcode.Ic_rdtsc).clock

let machine_state () =
  List.iter
    (fun opcode ->
      check_access
        (Opcode.to_source_name opcode)
        Effects.Read (Effects.classify opcode).machine)
    [
      Opcode.Ic_fs;
      Opcode.Ic_gs;
      Opcode.Ic_get_rflags;
      Opcode.Ic_carry;
      Opcode.Ic_get_rax;
      Opcode.Ic_get_rbp;
      Opcode.Ic_get_rsp;
      Opcode.Ic_rip;
    ];
  List.iter
    (fun opcode ->
      check_access
        (Opcode.to_source_name opcode)
        Effects.Write (Effects.classify opcode).machine)
    [
      Opcode.Ic_set_rflags;
      Opcode.Ic_set_rax;
      Opcode.Ic_set_rbp;
      Opcode.Ic_set_rsp;
    ]

let fold_barrier_is_independent () =
  let absolute = Effects.classify Opcode.Ic_abs_addr in
  Alcotest.(check bool)
    "absolute address blocks source folding" true
    absolute.prevents_constant_folding;
  Alcotest.(check bool)
    "absolute address has no runtime side effect" false
    (Effects.has_observable_effect absolute);
  Alcotest.(check bool)
    "absolute address is not a runtime reorder barrier" false
    (Effects.is_reorder_barrier absolute);
  let addition = Effects.classify Opcode.Ic_add in
  Alcotest.(check bool)
    "addition is source-foldable" false addition.prevents_constant_folding;
  Alcotest.(check bool)
    "addition is not observable" false
    (Effects.has_observable_effect addition)

let conservative_barriers () =
  List.iter
    (fun opcode ->
      Alcotest.(check bool)
        (Opcode.to_source_name opcode ^ " is a barrier")
        true
        (Effects.is_reorder_barrier (Effects.classify opcode)))
    [
      Opcode.Ic_deref;
      Opcode.Ic_assign;
      Opcode.Ic_call;
      Opcode.Ic_asm;
      Opcode.Ic_in_u8;
      Opcode.Ic_out_u8;
      Opcode.Ic_clflush;
      Opcode.Ic_invlpg;
      Opcode.Ic_get_rflags;
      Opcode.Ic_rdtsc;
    ];
  Alcotest.(check bool)
    "integer multiplication can move" false
    (Effects.is_reorder_barrier (Effects.classify Opcode.Ic_mul))

let deterministic_names () =
  Alcotest.(check (list string))
    "access names"
    [ "none"; "read"; "write"; "read-write"; "opaque" ]
    (List.map Effects.access_name
       [
         Effects.None;
         Effects.Read;
         Effects.Write;
         Effects.Read_write;
         Effects.Opaque;
       ]);
  Alcotest.(check (list string))
    "call names"
    [ "none"; "local"; "direct"; "indirect"; "import"; "extern" ]
    (List.map Effects.call_name
       [
         Effects.No_call;
         Effects.Local_call;
         Effects.Direct_call;
         Effects.Indirect_call;
         Effects.Import_call;
         Effects.Extern_call;
       ]);
  Alcotest.(check bool) "opaque reads" true (Effects.may_read Effects.Opaque);
  Alcotest.(check bool) "opaque writes" true (Effects.may_write Effects.Opaque);
  Alcotest.(check bool)
    "read does not write" false
    (Effects.may_write Effects.Read);
  Alcotest.(check string)
    "call rendering"
    "memory=opaque stack=read-write machine=opaque port=none cache-tlb=none \
     clock=none call=direct asm=false fold-barrier=true"
    (Effects.human (Effects.classify Opcode.Ic_call))

let tests =
  [
    Alcotest.test_case "complete table" `Quick complete_table;
    Alcotest.test_case "memory families" `Quick memory_families;
    Alcotest.test_case "calls and assembly" `Quick calls_and_assembly;
    Alcotest.test_case "stack and frames" `Quick stack_and_frames;
    Alcotest.test_case "hardware domains" `Quick hardware_domains;
    Alcotest.test_case "machine state" `Quick machine_state;
    Alcotest.test_case "fold barrier is independent" `Quick
      fold_barrier_is_independent;
    Alcotest.test_case "conservative barriers" `Quick conservative_barriers;
    Alcotest.test_case "deterministic names" `Quick deterministic_names;
  ]
