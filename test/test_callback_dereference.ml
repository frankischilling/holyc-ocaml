open Holyc_lib
module R = Semantic_function_call_expression_result
module S = Semantic_function_call_resolution
module L = Ir_expression_lowering
module Seq = Ir_instruction_sequence
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

(* Pinned TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c uses an explicit
   star before fp_compare in Kernel/QSort.HC:16,18 and before global callbacks
   in Kernel/KTask.HC:295,497. PrsPopDeref removes that pending operation before
   PrsFunCall loads the cell; the star must not become a second memory load. *)

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let some label = function
  | Some value -> value
  | None -> Alcotest.fail label

let prepare mode text =
  Test_function_call_conversion_policy.prepare ~mode
    ~path:"callback-dereference.HC" text

let function_named results name =
  Test_function_call_expression_result.function_named results name

let indirect_calls function_ =
  R.function_calls function_
  |> List.map (function
    | R.Indirect_call_result call -> call
    | _ -> Alcotest.fail "expected an indirect callback call")

let source_call call =
  call |> R.indirect_source
  |> Semantic_function_call_conversion_policy.indirect_source
  |> S.indirect_source

let rec ungroup result =
  match S.argument_expression_kind (R.result_source result) with
  | S.Parenthesized_expression source ->
      let operand = R.result_operand result |> some "missing grouped operand" in
      Alcotest.(check bool)
        "grouping keeps its exact original operand" true
        (R.result_source operand == source);
      ungroup operand
  | _ -> result

let semantic_cancellation_keeps_original_callback () =
  let text =
    "I64 Caller(I64 (*fp_compare)(I64 e1,I64 e2)){return (*fp_compare)(20,22);}"
  in
  List.iter
    (fun mode ->
      let _, results =
        prepare mode text |> Test_function_call_expression_result.analyze
      in
      let call = function_named results "Caller" |> indirect_calls |> List.hd in
      let source = source_call call in
      (match S.call_callee_form source with
      | S.Dereferenced_identifier_callee 1 -> ()
      | form ->
          Alcotest.failf "expected one-star callee, got %s"
            (S.callee_form_name form));
      let callee =
        R.indirect_callee_result call |> some "missing checked callee"
      in
      Alcotest.(check bool)
        "the original dereference remains the checked callee tree" true
        (R.result_source callee
        == some "missing retained callee value" (S.call_callee_value source));
      Alcotest.(check bool)
        "canceled dereference remains callback storage" true
        (R.result_is_callback_storage callee);
      let canceled = ungroup callee in
      let operand =
        R.result_canceled_callback_operand canceled
        |> some "one-star callback dereference was not canceled"
      in
      Alcotest.(check bool)
        "cancellation returns the original callback identifier" true
        (match S.argument_expression_kind (R.result_source operand) with
        | S.Bound_identifier_expression _ -> true
        | _ -> false);
      Alcotest.(check bool)
        "canceled result retains the exact callback declarator" true
        (some "missing canceled callback declarator"
           (R.result_callback_pointer callee)
        == some "missing original callback declarator"
             (R.result_callback_pointer operand));
      Alcotest.(check bool)
        "the original identifier is not itself a canceled expression" true
        (Option.is_none (R.result_canceled_callback_operand operand)))
    modes

let cancellation_stops_at_grouping_or_another_star () =
  let text = "I64 Caller(I64 (*p)(I64 n)){(*(p))(40);return (**p)(40);}" in
  List.iter
    (fun mode ->
      let _, results =
        prepare mode text |> Test_function_call_expression_result.analyze
      in
      let calls = function_named results "Caller" |> indirect_calls in
      Alcotest.(check int)
        "two remaining-dereference calls" 2 (List.length calls);
      List.iter
        (fun call ->
          let callee =
            R.indirect_callee_result call
            |> some "missing remaining callee"
            |> ungroup
          in
          Alcotest.(check bool)
            "grouping or a remaining star is not canceled" true
            (Option.is_none (R.result_canceled_callback_operand callee)))
        calls)
    modes

let update_typing_keeps_canceled_storage () =
  let text =
    "extern I64 Target(I64 a,I64 b);I64 Caller(I64 (*p)(I64 n)){return \
     Target(++*p,(*p)--);}"
  in
  List.iter
    (fun mode ->
      let _, results =
        prepare mode text |> Test_function_call_expression_result.analyze
      in
      let roots =
        Test_function_call_expression_result.root_results results "Caller"
      in
      Alcotest.(check int)
        "prefix and postfix update results" 2 (List.length roots);
      List.iter
        (fun root ->
          let source =
            Test_function_call_expression_result.update_operand root
          in
          let canceled =
            Test_function_call_expression_result.result_for_source results
              source
            |> ungroup
          in
          let operand =
            R.result_canceled_callback_operand canceled
            |> some "update lost the canceled callback operand"
          in
          Alcotest.(check bool)
            "update typing preserves original callback storage" true
            (R.result_is_callback_storage operand
            && Option.get (R.result_callback_pointer canceled)
               == Option.get (R.result_callback_pointer operand)))
        roots)
    modes

let lowering_uses_one_original_cell_load () =
  let module T = Test_ir_frame_address_lowering in
  let text =
    "I64 Caller(I64 (*fp_compare)(I64 e1,I64 e2)){fp_compare(20,22);return \
     (*fp_compare)(20,22);}"
  in
  let opcode_names lowered =
    L.sequence lowered |> Seq.instructions
    |> List.map (fun instruction ->
        instruction |> Seq.description |> fun description ->
        (Ir_opcode.info description.opcode).source_name)
  in
  List.iter
    (fun mode ->
      let frames, results = T.analyze ~compilation_mode:mode text in
      let function_ = T.function_named results "Caller" in
      let frame = T.frame_for frames function_ in
      let calls = indirect_calls function_ in
      Alcotest.(check int)
        "plain and explicit callback calls" 2 (List.length calls);
      let lower call frame =
        match
          L.lower_indirect_callee ~frame ~instruction_id:(T.instruction_id 53)
            ~value_id:(T.value_id 81) call
        with
        | Ok (L.Lowered lowered) -> lowered
        | Ok L.Unsupported_expression ->
            Alcotest.fail "one-star callback callee lowering was unsupported"
        | Error errors ->
            Alcotest.fail
              (Test_ir_expression_lowering.show_sequence_errors errors)
      in
      let plain = lower (List.hd calls) frame in
      let explicit = lower (List.nth calls 1) frame in
      let expected =
        [
          "IC_RBP"; "IC_IMM_I64"; "IC_ADD"; "IC_DEREF"; "IC_SET_RAX"; "IC_NOP2";
        ]
      in
      Alcotest.(check (list string))
        "plain callback snapshot" expected (opcode_names plain);
      Alcotest.(check (list string))
        "QSort-shaped star adds no second load" expected (opcode_names explicit);
      let dereferences =
        L.sequence explicit |> Seq.instructions
        |> List.filter (fun instruction ->
            (Seq.description instruction).opcode = Ir_opcode.Ic_deref)
      in
      Alcotest.(check int)
        "explicit one-star callee loads its callback cell once" 1
        (List.length dereferences);
      let foreign_frames, foreign_results =
        T.analyze ~compilation_mode:mode text
      in
      let foreign_frame =
        T.frame_for foreign_frames (T.function_named foreign_results "Caller")
      in
      match
        L.lower_indirect_callee ~frame:foreign_frame
          ~instruction_id:(T.instruction_id 53) ~value_id:(T.value_id 81)
          (List.nth calls 1)
      with
      | Error [ error ] ->
          Alcotest.(check string)
            "foreign frame cannot authorize canceled callback storage"
            "HCIRL0004" error.Seq.code
      | _ ->
          Alcotest.fail
            "foreign frame supplied authority for a canceled callback callee")
    modes

let public_run mode text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"callback-dereference-public.HC"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  let report =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  let execution =
    integer_program_report_outcome report
    |> Result.map_error diagnostics
    |> checked
    |> fun checked -> checked.value
  in
  let word =
    VM.final_value execution |> some "public source has no final value"
  in
  Alcotest.(check int64) "public source result" 42L word.bits;
  (execution, integer_program_report_output_bytes report)

let public_ir_and_source_match_pinned_shapes () =
  let qsort callee =
    "I64 Compare(I64 e1,I64 e2){return e1-e2;}"
    ^ "I64 QSortStep(I64 (*fp_compare)(I64 e1,I64 e2)){return " ^ callee
    ^ "(40,40)+42;}QSortStep(&Compare);"
  in
  let ktask =
    "I64 palette;U0 SetPalette(){palette=42;}U0 (*fp_set_std_palette)();"
    ^ "I64 Run(){fp_set_std_palette=&SetPalette;(*fp_set_std_palette)();"
    ^ "return palette;}Run();"
  in
  List.iter
    (fun mode ->
      let plain, plain_output = public_run mode (qsort "fp_compare") in
      let explicit, explicit_output = public_run mode (qsort "(*fp_compare)") in
      Alcotest.(check int)
        "QSort-shaped canceled star adds no public IR work"
        (VM.executed_steps plain)
        (VM.executed_steps explicit);
      Alcotest.(check string)
        "QSort-shaped callback has no output" plain_output explicit_output;
      let _, output = public_run mode ktask in
      Alcotest.(check string)
        "KTask-shaped global callback has no output" "" output)
    modes

let tests =
  [
    Alcotest.test_case "one-star semantic cancellation keeps callback identity"
      `Quick semantic_cancellation_keeps_original_callback;
    Alcotest.test_case "grouping and another star remain dereferences" `Quick
      cancellation_stops_at_grouping_or_another_star;
    Alcotest.test_case "update typing retains the canceled callback operand"
      `Quick update_typing_keeps_canceled_storage;
    Alcotest.test_case "one-star lowering uses the original callback cell load"
      `Quick lowering_uses_one_original_cell_load;
    Alcotest.test_case "public IR executes QSort and KTask callback spellings"
      `Quick public_ir_and_source_match_pinned_shapes;
  ]
