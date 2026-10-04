open Holyc_lib
module R = Semantic_function_call_expression_result
module S = Semantic_function_call_resolution
module L = Ir_expression_lowering
module Seq = Ir_instruction_sequence
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let target = "I64 Target(I64 n){return n+2;}"

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

let rec check_index_chain expected_depth result =
  if expected_depth = 0 then
    Alcotest.(check bool)
      "indexed cancellation reaches its original callback identifier" true
      (match S.argument_expression_kind (R.result_source result) with
      | S.Bound_identifier_expression _
      | S.Top_level_bound_identifier_expression _ -> true
      | _ -> false)
  else
    match S.argument_expression_kind (R.result_source result) with
    | S.Index_expression source ->
        let base, index =
          R.result_index_operands result
          |> some "indexed callback lost its operands"
        in
        Alcotest.(check bool)
          "indexed callback keeps its exact base source" true
          (R.result_source base == S.index_base source);
        Alcotest.(check bool)
          "indexed callback keeps its exact subscript source" true
          (R.result_source index == S.index_value source);
        Alcotest.(check bool)
          "indexed callback subscript keeps integer conversion" true
          (R.result_intrinsic_conversion index = R.Result_to_int);
        check_index_chain (expected_depth - 1) base
    | kind ->
        Alcotest.failf "expected indexed callback depth %d, got %s"
          expected_depth
          (S.argument_expression_kind_name kind)

let indexed_cancellation_keeps_original_typed_operand () =
  let text =
    "I64 (*Global)(I64 first=1,I64 required,I64 last=3,...)[2][3];\n\
     I64 Caller(){\n\
     I64 (*automatic)(I64 first=1,I64 required,I64 last=3,...)[2];\n\
     static I64 (*stored)(I64 first=1,I64 required,I64 last=3,...)[2][2];\n\
     (*automatic[1])(,2,,4);(*stored[0][1])(,2,,4);\n\
     return (*Global[1][2])(,2,,4);}"
  in
  List.iter
    (fun mode ->
      let _, results =
        prepare mode text |> Test_function_call_expression_result.analyze
      in
      let calls = function_named results "Caller" |> indirect_calls in
      Alcotest.(check int)
        "one- and two-dimensional explicit-star callback calls" 3
        (List.length calls);
      Alcotest.(check (list string))
        "indexed explicit-star calls retain their original callback roots"
        [ "automatic"; "stored"; "Global" ]
        (List.map (fun call -> S.call_callee_name (source_call call)) calls);
      List.iter2
        (fun expected_depth call ->
          let resolution =
            call |> R.indirect_source
            |> Semantic_function_call_conversion_policy.indirect_source
          in
          let source = S.indirect_source resolution in
          Alcotest.(check bool)
            "indexed explicit-star call remains a computed callee" true
            (S.call_callee_form source = S.Member_callee);
          let original =
            S.call_computed_callee source
            |> some "indexed explicit-star call lost its computed callee"
          in
          let wrapped =
            R.indirect_callee_result call
            |> some "indexed explicit-star call lost its checked callee"
          in
          Alcotest.(check bool)
            "checked callee retains the exact original prefix tree" true
            (R.result_source wrapped == original);
          let canceled = ungroup wrapped in
          let indexed =
            R.result_canceled_callback_operand canceled
            |> some "indexed callback dereference was not canceled"
          in
          (match S.argument_expression_kind (R.result_source canceled) with
          | S.Prefix_expression prefix ->
              Alcotest.(check bool)
                "the canceled star immediately wraps the indexed callback" true
                (S.prefix_operator prefix = S.Dereference
                && S.prefix_operand prefix == R.result_source indexed)
          | _ -> Alcotest.fail "indexed canceled callee is not a prefix tree");
          Alcotest.(check bool)
            "canceled indexed callee remains callback storage" true
            (R.result_is_callback_storage canceled
            && R.result_is_callback_storage indexed);
          let pointer =
            R.result_callback_pointer indexed
            |> some "indexed callback lost its declarator"
          in
          Alcotest.(check bool)
            "canceled indexed callee retains the exact callback header" true
            (pointer
             == some "canceled indexed callback lost its declarator"
                  (R.result_callback_pointer canceled)
            && pointer == S.callable_pointer (S.indirect_callable resolution));
          check_index_chain expected_depth indexed)
        [ 1; 2; 2 ] calls;
      let fixed = List.hd calls |> R.indirect_fixed_results in
      Alcotest.(check (list string))
        "explicit-star indexed call keeps the stored defaults and header"
        [ "default"; "provided"; "default" ]
        (List.map
           (fun fixed ->
             match R.fixed_path fixed with
             | R.Provided_result _ -> "provided"
             | R.Declared_default_result _ -> "default")
           fixed))
    modes

let indexed_grouping_and_ordinary_pointer_do_not_cancel () =
  let text =
    "I64 Sink(I64 a,I64 b,I64 c){return a+b+c;}I64 Caller(I64 **ordinary){I64 \
     (*p)(I64 n)[2];return Sink(*ordinary[0],*(p[1]),*(p)[1]);}"
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
        "ordinary and grouped indexed dereferences stay separate" 3
        (List.length roots);
      let ordinary = List.hd roots in
      let grouped = List.nth roots 1 in
      let grouped_base = List.nth roots 2 in
      List.iter
        (fun (label, result) ->
          Alcotest.(check bool)
            (label ^ " does not gain callback cancellation")
            true
            (Option.is_none (R.result_canceled_callback_operand result)))
        [
          ("ordinary pointer index", ordinary);
          ("grouped callback index", grouped);
          ("grouped callback bracket base", grouped_base);
        ];
      Alcotest.(check bool)
        "ordinary pointer shape is only a structural cancellation candidate"
        true
        (Option.is_some
           (S.callback_cancellation_operand (R.result_source ordinary)));
      Alcotest.(check bool)
        "group under the star is not even a structural cancellation candidate"
        true
        (Option.is_none
           (S.callback_cancellation_operand (R.result_source grouped)));
      Alcotest.(check bool)
        "grouped bracket base is not a structural cancellation candidate" true
        (Option.is_none
           (S.callback_cancellation_operand (R.result_source grouped_base)));
      (match S.argument_expression_kind (R.result_source ordinary) with
      | S.Prefix_expression prefix ->
          Alcotest.(check bool)
            "ordinary indexed dereference still wraps its index directly" true
            (S.prefix_operator prefix = S.Dereference
            &&
            match S.argument_expression_kind (S.prefix_operand prefix) with
            | S.Index_expression _ -> true
            | _ -> false)
      | _ -> Alcotest.fail "ordinary indexed dereference lost its prefix");
      (match S.argument_expression_kind (R.result_source grouped) with
      | S.Prefix_expression prefix ->
          Alcotest.(check bool)
            "group under the star remains the cancellation boundary" true
            (S.prefix_operator prefix = S.Dereference
            &&
            match S.argument_expression_kind (S.prefix_operand prefix) with
            | S.Parenthesized_expression _ -> true
            | _ -> false)
      | _ -> Alcotest.fail "grouped callback dereference lost its prefix");
      match S.argument_expression_kind (R.result_source grouped_base) with
      | S.Prefix_expression prefix -> (
          match S.argument_expression_kind (S.prefix_operand prefix) with
          | S.Index_expression index ->
              Alcotest.(check bool)
                "grouped callback bracket base stays grouped below its index"
                true
                (match S.argument_expression_kind (S.index_base index) with
                | S.Parenthesized_expression _ -> true
                | _ -> false)
          | _ -> Alcotest.fail "grouped callback base lost its index")
      | _ -> Alcotest.fail "grouped callback base lost its dereference prefix")
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

let indexed_reads_stores_and_updates_use_original_cells () =
  let rows =
    [
      ( "automatic callback copy",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
           n)[2],(*q)(I64 n);p[1]=&Target;N=0;q=*p[Index()];return \
           (q==&Target)*40+N+1;}Run();" );
      ( "static two-dimensional callback equality",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){static I64 (*p)(I64 \
           n)[2][2];p[1][1]=&Target;N=0;return \
           ((*p[Index()][Index()])==&Target)*40+N;}Run();" );
      ( "automatic callback store",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
           n)[2];N=0;*p[Index()]=&Target;return p[1](39)+N;}Run();" );
      ( "automatic numeric update result",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
         n)[2];p[1]=34;N=0;return (++*p[Index()])+N-1;}Run();" );
      ( "static two-dimensional numeric update",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){static I64 (*p)(I64 \
         n)[2][2];p[1][1]=34;N=0;*p[Index()][Index()]+=1;return \
         (p[1][1]==42)*40+N;}Run();" );
      ( "pointer-return callback numeric update",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){F64 **(*p)(I64 \
         n)[2];p[1]=34;N=0;return (++*p[Index()])+N-1;}Run();" );
      ( "signed add consumer keeps callback stride and raw sign",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((++*p[Index()])+one)<0)*40+(p[1]==0x7ffffffffffffff8)+N;}Run();" );
      ( "unsigned add consumer keeps callback stride and U64 shift",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((++*p[Index()])+one)>>63)==1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "signed subtract consumer keeps callback stride",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7fffffffffffffff;N=0;return \
         (((++*p[Index()])-one)>0)*40+(p[1]==0x8000000000000007)+N;}Run();" );
      ( "unsigned subtract consumer keeps callback stride",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7fffffffffffffff;N=0;return \
         (((((++*p[Index()])-one)>>63)==0)*40)+(p[1]==0x8000000000000007)+N;}Run();"
      );
      ( "nested signed add subtract chain keeps update result class",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((((++*p[Index()])+one)-one)+one)>>63)==-1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "nested unsigned add subtract chain keeps U64 class",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((((++*p[Index()])+one)-one)+one)>>63)==1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "unsigned add comparison keeps U64 domain",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         ((((++*p[Index()])+one)>0)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();" );
      ( "nested U64 chain retains class across signed RHS",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;I64 signed_one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((((++*p[Index()])+one)-signed_one)+signed_one)>>63)==1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "grouped postfix update result keeps callback arithmetic",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=34;N=0;return \
         ((((*p[Index()])++)+one)==42)*40+(p[1]==42)+N;}Run();" );
      ( "compound update result keeps callback arithmetic",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         two=2;p[1]=18;N=0;return \
         (((*p[Index()]+=1)+two)==42)*40+(p[1]==26)+N;}Run();" );
      ( "top-level global callback equality",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 (*P)(I64 \
           n)[2];P[1]=&Target;N=0;((*P[Index()])==&Target)*40+N+1;" );
      ( "top-level global callback store",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 (*P)(I64 \
           n)[2];N=0;*P[Index()]=&Target;P[1](39)+N;" );
      ( "top-level global numeric update",
        "I64 N;I64 Index(){N++;return 1;}I64 (*P)(I64 \
         n)[2];P[1]=34;N=0;*P[Index()]+=1;(P[1]==42)*40+N+1;" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter (fun (_, source) -> ignore (public_run mode source)) rows;
      let make update =
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return (((((" ^ update
        ^ ")+one)-one)+one)>>63==-1)*40+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      in
      let plain, _ = public_run mode (make "++p[Index()]") in
      let explicit, _ = public_run mode (make "++*p[Index()]") in
      Alcotest.(check int)
        "indexed canceled update star adds no public IR work"
        (VM.executed_steps plain)
        (VM.executed_steps explicit))
    modes

let indexed_calls_keep_capture_defaults_and_exact_work () =
  let capture =
    "extern U0 PutChars(U64 ch);I64 N;I64 (*p)(I64 a,I64 b)[2];"
    ^ "I64 Old(I64 a,I64 b){PutChars('C');return a+b+N-1;}"
    ^ "I64 New(I64 a,I64 b){PutChars('N');return 99;}"
    ^ "I64 Index(){N++;PutChars('I');return 1;}"
    ^ "I64 Left(){PutChars('L');return 20;}"
    ^ "I64 Right(){p[1]=&New;PutChars('R');return 22;}"
    ^ "I64 Run(){p[1]=&Old;N=0;return (*p[Index()])(Left(),Right());}Run();"
  in
  let defaults =
    "I64 Target(I64 n=17){return n;}I64 Run(){I64 (*p)(I64 \
     n=42)[2];p[1]=&Target;return (*p[1])();}Run();"
  in
  let two_dimensional =
    target
    ^ "I64 Run(){static I64 (*p)(I64 n)[2][2];p[1][1]=&Target;return \
       (*p[1][1])(40);}Run();"
  in
  let ordered_two_dimensional =
    "extern U0 PutChars(U64 ch);I64 Target(I64 n){return n+2;}I64 \
     Row(){PutChars('R');return 1;}I64 Column(){PutChars('C');return 1;}I64 \
     Run(){I64 (*p)(I64 n)[2][2];p[1][1]=&Target;return \
     (*p[Row()][Column()])(40);}Run();"
  in
  let top_level = target ^ "I64 (*P)(I64 n)[2];P[1]=&Target;(*P[1])(40);" in
  List.iter
    (fun mode ->
      let _, output = public_run mode capture in
      Alcotest.(check string)
        "indexed star captures the callee before reversed arguments" "IRLC"
        output;
      ignore (public_run mode defaults);
      ignore (public_run mode two_dimensional);
      let _, ordered_output = public_run mode ordered_two_dimensional in
      Alcotest.(check string)
        "two-dimensional indexed star evaluates row before column once" "RC"
        ordered_output;
      ignore (public_run mode top_level);
      let make callee =
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
           n)[2];p[1]=&Target;N=0;return " ^ callee ^ "(40)+N-1;}Run();"
      in
      let plain, _ = public_run mode (make "p[Index()]") in
      let explicit, _ = public_run mode (make "(*p[Index()])") in
      Alcotest.(check int)
        "indexed canceled star adds no public IR work" (VM.executed_steps plain)
        (VM.executed_steps explicit))
    modes

let indexed_call_faults_keep_exact_effect_order () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 N;I64 Index(I64 \
     i){N++;PutChars('I');return i;}I64 Arg(){PutChars('A');return 40;}"
  in
  let rows =
    [
      ( "bounds before arguments",
        "I64 Run(){I64 (*p)(I64 n)[2];N=0;return (*p[Index(2)])(Arg());}Run();",
        "HCIRVM0019",
        "I" );
      ( "uninitialized cell before arguments",
        "I64 Run(){I64 (*p)(I64 n)[2];N=0;return (*p[Index(1)])(Arg());}Run();",
        "HCIRVM0012",
        "I" );
      ( "numeric callback after arguments",
        "I64 Run(){I64 (*p)(I64 n)[2];p[1]=123;N=0;return \
         (*p[Index(1)])(Arg());}Run();",
        "HCIRVM0024",
        "IA" );
      ( "owned callback update after RHS effects",
        "I64 Target(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
         n)[2];p[1]=&Target;N=0;*p[Index(1)]+=Arg();return 42;}Run();",
        "HCIRVM0024",
        "IA" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, code, output) ->
          ignore
            (Test_integer_output.run ~mode (prefix ^ source)
            |> Test_integer_output.fault ~output code))
        rows)
    modes

let tests =
  [
    Alcotest.test_case "one-star semantic cancellation keeps callback identity"
      `Quick semantic_cancellation_keeps_original_callback;
    Alcotest.test_case
      "indexed cancellation keeps original typed callback and indices" `Quick
      indexed_cancellation_keeps_original_typed_operand;
    Alcotest.test_case
      "indexed grouping and ordinary pointers remain real dereferences" `Quick
      indexed_grouping_and_ordinary_pointer_do_not_cancel;
    Alcotest.test_case "grouping and another star remain dereferences" `Quick
      cancellation_stops_at_grouping_or_another_star;
    Alcotest.test_case "update typing retains the canceled callback operand"
      `Quick update_typing_keeps_canceled_storage;
    Alcotest.test_case "one-star lowering uses the original callback cell load"
      `Quick lowering_uses_one_original_cell_load;
    Alcotest.test_case "public IR executes QSort and KTask callback spellings"
      `Quick public_ir_and_source_match_pinned_shapes;
    Alcotest.test_case "indexed reads stores and updates reuse original cells"
      `Quick indexed_reads_stores_and_updates_use_original_cells;
    Alcotest.test_case "indexed calls keep capture defaults and exact work"
      `Quick indexed_calls_keep_capture_defaults_and_exact_work;
    Alcotest.test_case "indexed call faults keep exact effect order" `Quick
      indexed_call_faults_keep_exact_effect_order;
  ]
