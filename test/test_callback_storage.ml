open Holyc_lib
module R = Semantic_function_call_expression_result
module S = Semantic_function_call_resolution
module F = Semantic_function_type_resolution
module G = Semantic_global_type_resolution

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let prepare mode text =
  Test_function_call_conversion_policy.prepare ~mode ~path:"callback-storage.HC"
    text

let check_word_pointer depth = function
  | None -> Alcotest.fail "callback storage has no machine type"
  | Some type_ -> (
      Alcotest.(check int)
        "callback indirection count" depth
        (Semantic_type.pointer_depth type_);
      match Semantic_type.base type_ with
      | Semantic_type.Primitive
          (Semantic_type.Internal_storage, Primitive_type.I64) -> ()
      | _ -> Alcotest.fail "callback storage must use internal RT_PTR")

let statements results =
  Test_function_call_expression_result.function_named results "Caller"
  |> R.function_expression_statements
  |> List.map R.expression_statement_value

let assignment result =
  match R.result_binary_operands result with
  | Some pair -> pair
  | None -> Alcotest.fail "expected original assignment operands"

let check_storage left =
  Alcotest.(check bool)
    "positive scalar callback storage" true
    (R.result_is_callback_storage left);
  check_word_pointer 1 (R.result_storage_type left);
  check_word_pointer 1 (R.result_computation_type left);
  Alcotest.(check string)
    "callback address has integer class" "integer-result"
    (R.result_class left |> R.result_class_name);
  R.result_callback_pointer left |> Option.get

let local_parameter_static_global_and_member_storage () =
  let text =
    "F64 Target(I64 n){return n;}F64 (*Global)(I64 n);\n\
     class Base {F64 (*member)(I64 n)[2];};class Box:Base {};\n\
     I64 Caller(F64 (*parameter)(I64 n),Box *box){\n\
     F64 (*local)(I64 n);static F64 (*saved)(I64 n);\n\
     F64 (*array)(I64 n)[2][3];\n\
     local=&Target;parameter=&Target;saved=&Target;Global=&Target;\n\
     array[1][2]=&Target;box->member[1]=&Target;(local)=&Target;return 0;}"
  in
  List.iter
    (fun mode ->
      let source = prepare mode text in
      let _, results = Test_function_call_expression_result.analyze source in
      let values = statements results in
      Alcotest.(check int)
        "all original assignments are checked" 7 (List.length values);
      List.iter
        (fun result ->
          let left, right = assignment result in
          ignore (check_storage left);
          Alcotest.(check string)
            "return type remains separate" "F64"
            (Test_function_call_expression_result.type_name left);
          Alcotest.(check string)
            "address assignment has no float conversion" "none"
            (R.result_intrinsic_conversion right |> R.intrinsic_conversion_name);
          Alcotest.(check string)
            "assignment executes as an integer" "integer-result"
            (R.result_class result |> R.result_class_name);
          check_word_pointer 1 (R.result_type result);
          Alcotest.(check bool)
            "assignment clears callable expression metadata" true
            (Option.is_none (R.result_callback_pointer result)))
        values;
      let global =
        source.global_types |> G.globals
        |> List.find (fun global ->
            G.global_symbol global |> Semantic_symbol.name = "Global")
      in
      let pointer =
        match G.global_declarator_kind global with
        | G.Function_pointer pointer -> pointer
        | G.Object -> Alcotest.fail "expected a callback declarator"
      in
      let left, _ = assignment (List.nth values 3) in
      Alcotest.(check bool)
        "global result retains the original signature object" true
        (Option.get (R.result_callback_pointer left) == pointer))
    modes

let storage_updates_and_conversions () =
  List.iter
    (fun mode ->
      let source =
        prepare mode
          "F64 Target(I64 n){return n;}I64 Caller(){F64 (*p)(I64 n);\n\
           p=&Target;p=1.5;p+=1;p+=1.5;p<<=1;p++;--p;&p;return 0;}"
      in
      let _, results = Test_function_call_expression_result.analyze source in
      let values = statements results in
      Alcotest.(check int)
        "assignment, updates and address are checked" 8 (List.length values);
      let left, right = assignment (List.nth values 1) in
      ignore (check_storage left);
      Alcotest.(check string)
        "float address input converts to an integer" "ICF_RES_TO_INT"
        (R.result_intrinsic_conversion right |> R.intrinsic_conversion_name);
      Alcotest.(check string)
        "integer compound assignment stays integer" "integer-result"
        (R.result_execution_class (List.nth values 2)
        |> Option.get |> R.result_class_name);
      Alcotest.(check string)
        "float arithmetic retains its execution class" "f64-result"
        (R.result_execution_class (List.nth values 3)
        |> Option.get |> R.result_class_name);
      List.iter
        (fun index ->
          let result = List.nth values index in
          ignore (check_storage (R.result_operand result |> Option.get));
          check_word_pointer 1 (R.result_type result);
          Alcotest.(check bool)
            "update result is a value, not a callback cell" false
            (R.result_is_callback_storage result))
        [ 5; 6 ];
      check_word_pointer 2 (R.result_type (List.nth values 7)))
    modes

let return_types_do_not_change_cell_storage () =
  List.iter
    (fun mode ->
      List.iter
        (fun return_type ->
          List.iter
            (fun depth ->
              let text =
                Printf.sprintf
                  "class Result {};I64 Caller(){%s (%sp)(I64 n);p=0;return 0;}"
                  return_type (String.make depth '*')
              in
              let _, results =
                prepare mode text
                |> Test_function_call_expression_result.analyze
              in
              let left, _ = statements results |> List.hd |> assignment in
              Alcotest.(check string)
                "callback return type retained" return_type
                (Test_function_call_expression_result.type_name left);
              check_word_pointer depth (R.result_storage_type left);
              Alcotest.(check bool)
                "all declared callback cells are writable" true
                (R.result_is_callback_storage left))
            [ 1; 2; 3; 4 ])
        [ "U0"; "I8"; "I64"; "F64"; "Result" ])
    modes

let arrays_require_complete_storage_selection () =
  let cases =
    [
      "I64 Caller(){F64 (*p)(I64)[2];p=0;return 0;}";
      "I64 Caller(){F64 (*p)(I64)[2][3];p[0]=0;return 0;}";
      "I64 Caller(){F64 (*p)(I64)[2];p++;return 0;}";
      "class Box{F64 (*p)(I64)[2];};I64 Caller(Box *b){b->p=0;return 0;}";
      "F64 Target(I64 n){return n;}I64 Caller(){(&Target)=0;return 0;}";
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let source = prepare mode text in
          let policies =
            Test_function_call_conversion_policy.analyze source
            |> Test_function_call_conversion_policy.checked_policy
          in
          match
            Holyc_lib.type_function_call_expressions source.session
              ~members:source.members ~policies
          with
          | Ok _ ->
              Alcotest.fail
                "non-cell callback assignment or update was accepted"
          | Error error ->
              Alcotest.(check string)
                "stable storage diagnostic" "HCSEMA0046" (R.error_code error))
        cases)
    modes

let top_level_storage_and_exact_signature () =
  List.iter
    (fun mode ->
      let source =
        prepare mode
          "F64 Target(I64 n){return n;}F64 (*p)(I64 n=40,...);\n\
           F64 (*array)(I64 n)[2][3];p=&Target;array[1][2]=&Target;p++;&p;"
      in
      let _, _, _, results = Test_top_level_expression_result.analyze source in
      let values =
        results |> R.top_level_statements
        |> List.concat_map R.top_level_statement_roots
        |> List.map R.top_level_root_value
      in
      Alcotest.(check int)
        "top-level callback storage roots" 4 (List.length values);
      let left, _ = List.hd values |> assignment in
      let pointer = check_storage left in
      let signature = F.function_pointer_signature pointer in
      Alcotest.(check bool)
        "original variadic marker is retained" true
        (Option.is_some (F.signature_variadic_origin signature));
      let parameter = F.signature_parameters signature |> List.hd in
      Alcotest.(check bool)
        "original default is retained" true
        (Option.is_some (F.parameter_default parameter));
      let array_left, _ = List.nth values 1 |> assignment in
      ignore (check_storage array_left);
      check_word_pointer 2 (R.result_type (List.nth values 3)))
    modes

let callback_frame_addresses_use_the_original_declarator () =
  let module Frame = Semantic_function_frame_layout in
  let module L = Ir_frame_address_lowering in
  let module T = Test_ir_frame_address_lowering in
  let text =
    "I64 Caller(F64 (*argument)(I64 n=40,...)){\n\
     F64 (*scalar)(I64 n);F64 (*matrix)(I64 n)[2][3];\n\
     scalar;argument;matrix;return 0;}"
  in
  List.iter
    (fun mode ->
      let frames, results = T.analyze ~compilation_mode:mode text in
      let function_ = T.function_named results "Caller" in
      let frame = T.frame_for frames function_ in
      let values = T.expression_values function_ in
      Alcotest.(check int)
        "three original callback frame cells" 3 (List.length values);
      List.iter
        (fun result ->
          let location = T.location_for_result frame result in
          Alcotest.(check bool)
            "frame retains the exact checked callback declarator" true
            (Option.get (R.result_callback_pointer result)
            == Option.get (Frame.location_callback_pointer location));
          check_word_pointer 1
            (Frame.location_storage_type location |> Result.to_option);
          let lowered = T.lower_result frame result in
          check_word_pointer 2 (Some (L.result_type lowered));
          Alcotest.(check string)
            "callback return type is not overwritten" "F64"
            (Test_function_call_expression_result.type_name result))
        values;
      let foreign_frames, foreign_results =
        T.analyze ~compilation_mode:mode text
      in
      let foreign_function = T.function_named foreign_results "Caller" in
      let foreign_frame = T.frame_for foreign_frames foreign_function in
      match
        L.lower ~instruction_id:(T.instruction_id 0) ~value_id:(T.value_id 0)
          ~frame:foreign_frame (List.hd values)
      with
      | Error [ error ] ->
          Alcotest.(check string)
            "same-name foreign frame is rejected" "HCIRL0004"
            error.Ir_instruction_sequence.code
      | _ -> Alcotest.fail "a foreign callback frame supplied address authority")
    modes

let checked_callees_retain_original_values_before_arguments () =
  let text =
    "F64 (*Global)(I64 n);class Box {F64 (*member)(I64 n);};\n\
     I64 Caller(F64 (*parameter)(I64 n),Box *box){\n\
     F64 (*local)(I64 n);static F64 (*saved)(I64 n);\n\
     F64 (*array)(I64 n)[2][3];\n\
     local(40);parameter(40);saved(40);Global(40);\n\
     array[1][2](40);box->member(40);(local)(40);return 0;}"
  in
  List.iter
    (fun mode ->
      let _, results =
        prepare mode text |> Test_function_call_expression_result.analyze
      in
      let calls =
        Test_function_call_expression_result.function_named results "Caller"
        |> R.function_calls
        |> List.filter_map (function
          | R.Indirect_call_result call -> Some call
          | _ -> None)
      in
      Alcotest.(check int) "all seven callback callees" 7 (List.length calls);
      List.iter
        (fun call ->
          let resolution =
            call |> R.indirect_source
            |> Semantic_function_call_conversion_policy.indirect_source
          in
          let source = S.indirect_source resolution in
          let callee = R.indirect_callee_result call |> Option.get in
          Alcotest.(check bool)
            "exact original callee expression" true
            (R.result_source callee == Option.get (S.call_callee_value source));
          let pointer = check_storage callee in
          Alcotest.(check bool)
            "exact selected callback declarator" true
            (pointer == S.callable_pointer (S.indirect_callable resolution));
          List.iter
            (fun fixed ->
              match R.fixed_path fixed with
              | R.Provided_result argument ->
                  Alcotest.(check bool)
                    "callee typed before the argument" true
                    (R.Id.compare (R.result_id callee) (R.result_id argument)
                    < 0)
              | R.Declared_default_result _ -> ())
            (R.indirect_fixed_results call);
          match S.call_callee_form source with
          | S.Member_callee ->
              Alcotest.(check bool)
                "member form retains its computed tree" true
                (Option.is_some (S.call_computed_callee source))
          | S.Identifier_callee | S.Dereferenced_identifier_callee _ ->
              Alcotest.(check bool)
                "identifier keeps its original form" true
                (Option.is_none (S.call_computed_callee source)))
        calls)
    modes

let top_level_scalar_callback_retains_original_callee () =
  List.iter
    (fun mode ->
      let source = prepare mode "F64 (*Global)(I64 n);(Global)(40);" in
      let _, _, _, results = Test_top_level_expression_result.analyze source in
      let call = R.top_level_global_callback_calls results |> List.hd in
      let callee = R.top_level_global_callback_callee_result call in
      let tree = R.top_level_global_callback_source call in
      Alcotest.(check bool)
        "exact top-level callee tree" true
        (R.result_source callee
        == Semantic_top_level_expression_tree.call_callee_expression tree);
      let pointer = check_storage callee in
      Alcotest.(check bool)
        "exact top-level callback signature" true
        (pointer
        == S.callable_pointer (R.top_level_global_callback_callable call));
      let result =
        R.top_level_all_results results
        |> List.find (fun result ->
            R.Id.equal (R.result_id result)
              (R.top_level_global_callback_result_id call))
      in
      Alcotest.(check bool)
        "invocation signature retains the original header" true
        (Option.get (R.result_callback_call_pointer result) == pointer);
      Alcotest.(check bool)
        "call result does not acquire callback storage metadata" true
        (Option.is_none (R.result_callback_pointer result));
      Alcotest.(check string)
        "call keeps its declared return domain" "F64"
        (Test_function_call_expression_result.type_name result);
      let module C = Ir_callback_source in
      Alcotest.(check bool)
        "original top-level call belongs to its exact batch" true
        (C.top_level_member (C.Global call) results);
      let copy = Obj.obj (Obj.dup (Obj.repr call)) in
      Alcotest.(check bool)
        "copied call metadata grants no batch membership" false
        (C.top_level_member (C.Global copy) results);
      let _, _, _, foreign = Test_top_level_expression_result.analyze source in
      Alcotest.(check bool)
        "equal foreign batch grants no call membership" false
        (C.top_level_member (C.Global call) foreign))
    modes

let callback_loads_use_physical_words () =
  let module T = Test_ir_frame_address_lowering in
  let module L = Ir_expression_lowering in
  let module Seq = Ir_instruction_sequence in
  let text =
    "class Box {I64 n;};I64 Caller(F64 (*argument)(I64 n)){\n\
     U0 (*void_callback)(I64 n);Box (*aggregate_callback)(I64 n);\n\
     F64 (**two)(I64 n);F64 (***three)(I64 n);\n\
     (argument);void_callback;aggregate_callback;two;three;return 0;}"
  in
  List.iter
    (fun mode ->
      let frames, results = T.analyze ~compilation_mode:mode text in
      let function_ = T.function_named results "Caller" in
      let frame = T.frame_for frames function_ in
      let values = T.expression_values function_ in
      List.iter2
        (fun depth result ->
          match
            L.lower_typed_result ~frame ~instruction_id:(T.instruction_id 0)
              ~value_id:(T.value_id 0) result
          with
          | Ok (L.Lowered lowered) ->
              check_word_pointer depth (Some (L.result_type lowered));
              let instructions =
                L.sequence lowered |> Seq.instructions
                |> List.map Seq.description
              in
              let load = List.hd (List.rev instructions) in
              Alcotest.(check bool)
                "callback is loaded once from its cell" true
                (load.opcode = Ir_opcode.Ic_deref
                && List.length
                     (List.filter
                        (fun (d : Seq.description) ->
                          d.opcode = Ir_opcode.Ic_deref)
                        instructions)
                   = 1);
              check_word_pointer depth load.target_type
          | Error errors ->
              Alcotest.fail
                (Test_ir_expression_lowering.show_sequence_errors errors)
          | Ok L.Unsupported_expression ->
              Alcotest.fail "original callback cell was not lowered")
        [ 1; 1; 1; 2; 3 ] values;
      let foreign_frames, foreign_results =
        T.analyze ~compilation_mode:mode text
      in
      let foreign_frame =
        T.frame_for foreign_frames (T.function_named foreign_results "Caller")
      in
      match
        L.lower_typed_result ~frame:foreign_frame
          ~instruction_id:(T.instruction_id 0) ~value_id:(T.value_id 0)
          (List.hd values)
      with
      | Error [ error ] ->
          Alcotest.(check string)
            "foreign frame cannot load a callee" "HCIRL0004" error.Seq.code
      | _ -> Alcotest.fail "another frame supplied callback load authority")
    modes

let callback_parameter_default_is_an_integer_address () =
  List.iter
    (fun mode ->
      let _, results =
        prepare mode
          "U0 Target(F64 (*callback)(I64 n)=0);I64 Caller(){Target();return 0;}"
        |> Test_function_call_expression_result.analyze
      in
      let fixed =
        Test_function_call_expression_result.only_direct results "Caller"
        |> R.direct_fixed_results |> List.hd
      in
      match R.fixed_path fixed with
      | R.Declared_default_result default ->
          Alcotest.(check string)
            "callback default is integer storage" "integer-result"
            (R.declared_default_class default |> R.result_class_name);
          let type_ = R.declared_default_type default in
          Alcotest.(check bool)
            "callback return type remains F64" true
            (Semantic_type.pointer_depth type_ = 0
            && Semantic_type.base type_
               = Semantic_type.Primitive
                   (Semantic_type.Public_spelling, Primitive_type.F64));
          check_word_pointer 1 (R.declared_default_storage_type default)
      | R.Provided_result _ -> Alcotest.fail "expected a callback default")
    modes

let callback_callee_snapshot_matches_prs_fun_call () =
  let module T = Test_ir_frame_address_lowering in
  let module L = Ir_expression_lowering in
  let module Seq = Ir_instruction_sequence in
  List.iter
    (fun mode ->
      let text = "I64 Caller(F64 (*p)(I64 n)){(p)(40);return 0;}" in
      let frames, results = T.analyze ~compilation_mode:mode text in
      let function_ = T.function_named results "Caller" in
      let frame = T.frame_for frames function_ in
      let call =
        R.function_calls function_ |> List.hd |> function
        | R.Indirect_call_result call -> call
        | _ -> Alcotest.fail "expected the original callback call"
      in
      let lower ?(instruction = 53) ?(value = 81) frame =
        L.lower_indirect_callee ~frame
          ~instruction_id:(T.instruction_id instruction)
          ~value_id:(T.value_id value) call
      in
      let lowered =
        match lower frame with
        | Ok (L.Lowered lowered) -> lowered
        | Error errors ->
            Alcotest.fail
              (Test_ir_expression_lowering.show_sequence_errors errors)
        | Ok L.Unsupported_expression ->
            Alcotest.fail "callee snapshot was unsupported"
      in
      let instructions =
        L.sequence lowered |> Seq.instructions |> List.map Seq.description
      in
      Alcotest.(check (list string))
        "original callee precedes the call-start boundary"
        [
          "IC_RBP"; "IC_IMM_I64"; "IC_ADD"; "IC_DEREF"; "IC_SET_RAX"; "IC_NOP2";
        ]
        (List.map
           (fun (d : Seq.description) -> (Ir_opcode.info d.opcode).source_name)
           instructions);
      let load = List.nth instructions 3
      and set_rax = List.nth instructions 4
      and nop = List.nth instructions 5 in
      check_word_pointer 0 load.target_type;
      check_word_pointer 0 (Some (L.result_type lowered));
      Alcotest.(check bool)
        "RAX consumes the original loaded callee" true
        (set_rax.operands = [ L.result_value lowered ]
        && load.result = Some { Seq.value_id = L.result_value lowered });
      Alcotest.(check bool)
        "source bookkeeping is balanced" true
        (nop.payload = Some (Seq.Integer 1L));
      Alcotest.(check (list int))
        "consecutive instruction identities" [ 53; 54; 55; 56; 57; 58 ]
        (List.map
           (fun (d : Seq.description) ->
             Seq.Instruction_id.to_int d.instruction_id)
           instructions);
      Alcotest.(check (pair int int))
        "snapshot has no fabricated value producers" (59, 85)
        ( Seq.Instruction_id.to_int (L.next_instruction_id lowered),
          Seq.Value_id.to_int (L.next_value_id lowered) );
      let callee = Option.get (R.indirect_callee_result call) in
      check_word_pointer 1 (R.result_storage_type callee);
      Alcotest.(check string)
        "callee return type is retained" "F64"
        (Test_function_call_expression_result.type_name callee);
      (match lower ~instruction:(max_int - 6) ~value:(max_int - 4) frame with
      | Ok (L.Lowered lowered) ->
          Alcotest.(check (pair int int))
            "exact identifier capacity" (max_int, max_int)
            ( Seq.Instruction_id.to_int (L.next_instruction_id lowered),
              Seq.Value_id.to_int (L.next_value_id lowered) )
      | _ ->
          Alcotest.fail "exact callee snapshot identity capacity was rejected");
      List.iter
        (fun (instruction, value) ->
          match lower ~instruction ~value frame with
          | Error errors ->
              Alcotest.(check bool)
                "one-below identity capacity is diagnosed" true
                (List.exists
                   (fun (error : Seq.error) -> error.code = "HCIRL0005")
                   errors)
          | _ -> Alcotest.fail "callee snapshot exceeded its identity capacity")
        [ (max_int - 5, 81); (53, max_int - 3) ];
      let foreign_frames, foreign_results =
        T.analyze ~compilation_mode:mode text
      in
      let foreign_frame =
        T.frame_for foreign_frames (T.function_named foreign_results "Caller")
      in
      match lower foreign_frame with
      | Error [ error ] ->
          Alcotest.(check string)
            "foreign frame cannot snapshot a callee" "HCIRL0004" error.Seq.code
      | _ -> Alcotest.fail "foreign frame supplied an indirect callee snapshot")
    modes

let scalar_call_rejects_another_callee_value () =
  List.iter
    (fun mode ->
      let _, results =
        prepare mode
          "I64 Caller(I64 (*p)(I64 n),I64 (*q)(I64 n)){p(40);q(40);return 0;}"
        |> Test_function_call_expression_result.analyze
      in
      let calls =
        Test_function_call_expression_result.function_named results "Caller"
        |> R.function_calls
        |> List.map (function
          | R.Indirect_call_result call ->
              R.indirect_source call
              |> Semantic_function_call_conversion_policy.indirect_source
              |> S.indirect_source
          | _ -> Alcotest.fail "expected checked callback calls")
      in
      let source = List.hd calls and other = List.nth calls 1 in
      match
        S.make_call ~index:(S.call_index source)
          ~callee_occurrence_index:(S.call_callee_occurrence_index source)
          ~callee_name:(S.call_callee_name source)
          ~callee_origin:(S.call_callee_origin source)
          ~callee_form:(S.call_callee_form source)
          ?callable:(S.call_callable source)
          ?callee_value:(S.call_callee_value other)
          ~origin:(S.call_origin source) ~syntax:(S.call_syntax source)
          (S.call_arguments source)
      with
      | Error message ->
          Alcotest.(check string)
            "a matching signature is not callee ownership"
            "function call callee value does not retain its bound occurrence \
             and form"
            message
      | Ok _ -> Alcotest.fail "another callback supplied the callee value")
    modes

let owned_function_address_execution () =
  let module G = Test_integer_globals in
  let module T = Test_integer_functions in
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (G.run ~mode text |> T.expect expected))
        [
          ("I64 Target(I64 n){return n+2;}(&Target==&Target);", 1L);
          ("I64 A(){return 1;}I64 B(){return 1;}(&A==&B);", 0L);
          ("I64 A(){return 1;}I64 B(){return 1;}(&A!=&B);", 1L);
          ("I64 A(){return 1;}(&A!=0);", 1L);
          ("I64 A(){return 1;}(0==&A);", 0L);
          ("I64 A(){return 1;}I64 P;P=&A;(P==&A);", 1L);
          ("I64 A(){return 1;}U64 P;P=&A;(P==&A);", 1L);
          ("I64 A(){return 1;}I64 P,Q;P=&A;Q=P;(Q==&A);", 1L);
          ("I64 A(){return 1;}I64 Check(){I64 p=&A;return p==&A;}Check();", 1L);
          ("I64 A(){return 1;}I64 Check(I64 p){return p==&A;}Check(&A);", 1L);
          ("I64 A(){return 1;}(&A)(U64)==&A;", 1L);
          ("I64 A(){return 1;}&A;42;", 42L);
        ])
    modes

let scalar_callback_storage_execution () =
  let module G = Test_integer_globals in
  let module T = Test_integer_functions in
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (G.run ~mode text |> T.expect expected))
        [
          ( "I64 A(){return 1;}I64 Check(){I64 (*p)();p=&A;return \
             p==&A;}Check();",
            1L );
          ( "I64 A(){return 1;}I64 Check(){F64 (*p)();p=&A;return \
             p==&A;}Check();",
            1L );
          ( "I64 A(){return 1;}I64 Check(){U0 (*p)();p=&A;return p==&A;}Check();",
            1L );
          ( "I64 A(){return 1;}I64 Check(){I64 (*p)(),(*q)();p=&A;q=p;return \
             q==&A;}Check();",
            1L );
          ( "I64 A(){return 1;}I64 Check(){I64 (*p)();(p)=&A;return \
             p==&A;}Check();",
            1L );
          ("I64 Check(){I64 (*p)();p=0;return p==0;}Check();", 1L);
          ( "I64 A(){return 1;}I64 Check(){I64 (*p)();p=&A;p=0;return \
             p==0;}Check();",
            1L );
          ( "I64 A(){return 1;}I64 Check(I64 (*p)()){return p==&A;}Check(&A);",
            1L );
          ( "I64 A(){return 1;}I64 Check(F64 (*p)()){return p==&A;}Check(&A);",
            1L );
          ( "I64 A(){return 1;}I64 Check(U0 (*p)()){p=&A;return p==&A;}Check(0);",
            1L );
        ])
    modes

let static_callback_storage_execution () =
  let cases =
    [
      ( "I64 Add(I64 n){return n+2;}I64 Run(I64 set){static I64 (*p)(I64 \
         n);if(set)p=&Add;return p(40);}Run(1);Run(0);",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){static I64 (*p)(I64 n),(*q)(I64 \
         n);p=&Add;q=p;p=0;return q(40);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){static I64 (*p)(I64 \
         n);p=&Add;return p(p=0);}Run();",
        2L );
      ( "I64 Add(I64 n=17){return n;}I64 Run(){static I64 (*p)(I64 \
         n=42);p=&Add;return p();}Run();",
        42L );
      ( "I64 Sum(I64 n,...){return n+argc+argv[0]+argv[1];}I64 Run(){static \
         I64 (*p)(I64 n,...);p=&Sum;return p(37,1,2);}Run();",
        42L );
      ( "I64 A(){return 1;}I64 B(){return 2;}I64 Left(){static I64 \
         (*p)();p=&A;return p();}I64 Right(){static I64 (*p)();p=&B;return \
         p();}Left()*100+Right();",
        102L );
      ( "I64 A(){return 1;}I64 Run(){static F64 (*p)();p=&A;return p==&A;}Run();",
        1L );
      ( "I64 A(){return 1;}I64 Run(){static U0 (*p)();p=&A;return p==&A;}Run();",
        1L );
      ( "class Box{I64 n;};I64 A(){return 1;}I64 Run(){static Box \
         (*p)();p=&A;return p==&A;}Run();",
        1L );
      ( "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 n=0;static I64 \
         (*p)(I64 a,I64 b);p=&Take;return p(++n,++n);}Run();",
        21L );
      ( "I64 Add(I64 n){return n+2;}I64 Bad(I64 a,I64 b){return a+b;}I64 \
         Run(){static I64 (*p)(I64 n);p=&Bad;p=&Add;return p(40);}Run();",
        42L );
      ( "I64 Walk(I64 n){static I64 (*p)(I64 n);p=&Walk;if(n)return \
         p(n-1)+1;return 0;}Walk(8);",
        8L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          ignore
            (Test_integer_globals.run ~mode source
            |> Test_integer_functions.expect expected))
        cases)
    modes

let static_callback_reached_faults () =
  let prefix =
    "extern U0 Print(U8 *fmt,...);I64 Bad(I64 a,I64 b){return a+b;}I64 \
     Side(){Print(\"arg\");return 40;}"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (initial, code) ->
          let source =
            prefix ^ "I64 Run(){static I64 (*p)(I64 n);p=" ^ initial
            ^ ";return p(Side());}Print(\"before\");Run();"
          in
          ignore
            (Test_integer_output.run ~mode source
            |> Test_integer_output.fault ~output:"beforearg" code))
        [ ("0", "HCIRVM0024"); ("123", "HCIRVM0024"); ("&Bad", "HCIRVM0014") ];
      ignore
        (Test_integer_globals.run ~mode
           "I64 Run(){static I64 (*p)(I64 n);p=0;if(0)p(40);return 42;}Run();"
        |> Test_integer_functions.expect 42L);
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);U0 Write(I64 n){Print(\"%d\",n);}I64 \
            Run(){static U0 (*p)(I64 n);p=&Write;p(42);return 42;}Run();"
        |> Test_integer_output.expect "42"))
    modes;

  List.iter
    (fun (mode, code, output) ->
      ignore
        (Test_integer_output.run ~mode
           (prefix
          ^ "I64 Run(){static I64 (*p)(I64 n);return \
             p(Side());}Print(\"before\");Run();")
        |> Test_integer_output.fault ~output code))
    [
      (Preprocessor.Jit, "HCIRVM0012", "before");
      (Preprocessor.Aot, "HCIRVM0024", "beforearg");
    ]

let callback_array_storage_execution () =
  let cases =
    [
      ( "I64 Add(I64 n){return n+2;}I64 Run(){@I64 (*p)(I64 \
         n)[2];p[1]=&Add;return p[1](40);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){@I64 (*p)(I64 \
         n)[2][3];p[1][2]=&Add;return p[1][2](40);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){@I64 (*p)(I64 \
         n)[2];p[0]=&Add;p[1]=p[0];p[0]=0;return p[1](40);}Run();",
        42L );
      ( "I64 Add(I64 n=17){return n;}I64 Run(){@I64 (*p)(I64 \
         n=42)[2];p[1]=&Add;return p[1]();}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){@I64 (*p)(I64 \
         n)[2];p[1]=&Add;return p[1](p[1]=0);}Run();",
        2L );
      ( "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 n=0;@I64 (*p)(I64 \
         a,I64 b)[2];p[1]=&Take;return p[1](++n,++n);}Run();",
        21L );
      ( "I64 Sum(I64 n,...){return n+argc+argv[0]+argv[1];}I64 Run(){@I64 \
         (*p)(I64 n,...)[2];p[1]=&Sum;return p[1](37,1,2);}Run();",
        42L );
      ( "I64 A(){return 1;}I64 Run(){@F64 (*p)()[2];p[1]=&A;return \
         p[1]==&A;}Run();",
        1L );
      ( "class Box{I64 n;};I64 A(){return 1;}I64 Run(){@Box \
         (*p)()[2];p[1]=&A;return p[1]==&A;}Run();",
        1L );
      ( "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n)){return \
         p(40);}I64 Run(){@I64 (*p)(I64 n)[2];p[1]=&Add;return \
         Apply(p[1]);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){@I64 (*p)(I64 \
         n)[2];p[1]=&Add;return (p[1])(p[1](38));}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Bad(I64 a,I64 b){return a+b;}I64 \
         Run(){@I64 (*p)(I64 n)[2];p[1]=&Bad;p[1]=&Add;return p[1](40);}Run();",
        42L );
      ( "I64 Walk(I64 n){@I64 (*p)(I64 n)[2];p[1]=&Walk;if(n)return \
         p[1](n-1)+1;return 0;}Walk(8);",
        8L );
      ( "I64 Run(){@I64 (*p)(I64 n)[2];p[1]=0;if(0)p[1](40);return 42;}Run();",
        42L );
      ( "I64 Add(I64 n=99){return n;}I64 Run(){@I64 (*q)(I64 n=17);@I64 \
         (*p)(I64 n=42)[2];q=&Add;p[1]=q;return p[1]()*100+q();}Run();",
        4217L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun storage ->
          List.iter
            (fun (source, expected) ->
              let source =
                String.split_on_char '@' source |> String.concat storage
              in
              let result = Test_integer_globals.run ~mode source in
              (match result with
              | Error (error :: _) ->
                  Alcotest.failf "%s: %s at %d: %s" source error.Diagnostic.code
                    error.primary.start error.message
              | _ -> ());
              ignore (result |> Test_integer_functions.expect expected))
            cases)
        [ ""; "static " ])
    modes

let callback_array_reached_faults () =
  let prefix =
    "extern U0 Print(U8 *fmt,...);I64 Bad(I64 a,I64 b){return a+b;}I64 \
     Index(){Print(\"index\");return 1;}I64 Side(){Print(\"arg\");return 40;}"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun storage ->
          List.iter
            (fun (initial, code) ->
              ignore
                (Test_integer_output.run ~mode
                   (prefix ^ "I64 Run(){" ^ storage ^ "I64 (*p)(I64 n)[2];p[1]="
                  ^ initial
                  ^ ";return p[Index()](Side());}Print(\"before\");Run();")
                |> Test_integer_output.fault ~output:"beforeindexarg" code))
            [
              ("0", "HCIRVM0024"); ("123", "HCIRVM0024"); ("&Bad", "HCIRVM0014");
            ];
          let code, output =
            if storage = "static " && mode = Preprocessor.Aot then
              ("HCIRVM0024", "beforeindexarg")
            else ("HCIRVM0012", "beforeindex")
          in
          ignore
            (Test_integer_output.run ~mode
               (prefix ^ "I64 Run(){" ^ storage
              ^ "I64 (*p)(I64 n)[2];return \
                 p[Index()](Side());}Print(\"before\");Run();")
            |> Test_integer_output.fault ~output code);
          ignore
            (Test_integer_output.run ~mode
               ("extern U0 Print(U8 *fmt,...);U0 Write(I64 \
                 n){Print(\"%d\",n);}I64 Run(){" ^ storage
              ^ "U0 (*p)(I64 n)[2];p[1]=&Write;p[1](42);return 42;}Run();")
            |> Test_integer_output.expect "42"))
        [ ""; "static " ])
    modes

let callback_array_bounds () =
  List.iter
    (fun mode ->
      List.iter
        (fun storage ->
          List.iter
            (fun (index, code) ->
              ignore
                (Test_integer_output.run ~mode
                   ("extern U0 Print(U8 *fmt,...);I64 \
                     Side(){Print(\"arg\");return 40;}I64 Run(){" ^ storage
                  ^ "I64 (*p)(I64 n)[2];return p[" ^ index
                  ^ "](Side());}Print(\"before\");Run();")
                |> Test_integer_output.fault ~output:"before" code))
            [
              ("2", "HCIRVM0019");
              ("-1", "HCIRVM0019");
              ("0x7FFFFFFFFFFFFFFF", "HCIRVM0020");
            ])
        [ ""; "static " ])
    modes

let callback_array_retained_storage () =
  List.iter
    (fun mode ->
      ignore
        (Test_integer_globals.run ~mode
           "I64 Add(I64 n){return n+2;}I64 Run(I64 set){static I64 (*p)(I64 \
            n)[2][3];if(set)p[1][2]=&Add;return p[1][2](40);}Run(1);Run(0);"
        |> Test_integer_functions.expect 42L);
      Alcotest.(check string)
        "automatic callback arrays do not outlive their activation" "HCIRVM0012"
        (Test_integer_functions.first_error
           (Test_integer_globals.run ~mode
              "I64 Add(I64 n){return n+2;}I64 Run(I64 set){I64 (*p)(I64 \
               n)[2];if(set)p[1]=&Add;return p[1](40);}Run(1);Run(0);"))
          .code)
    modes;
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit
       "I64 A(){return 1;}I64 Run(I64 set){static I64 \
        (*p)()[2];if(set)p[1]=&A;return p[1]();}Run(1);I64 A(){return \
        2;}Run(0)*100+A();"
    |> Test_integer_functions.expect 102L)

let callback_array_defaults_evaluate_once () =
  List.iter
    (fun storage ->
      ignore
        (Test_integer_output.run ~mode:Preprocessor.Jit
           ("extern U0 Print(U8 *fmt,...);I64 D(){Print(\"default\");return \
             40;}I64 Add(I64 n){return n+2;}I64 Run(){" ^ storage
          ^ "I64 (*p)(I64 n=D())[2];p[1]=&Add;return p[1]();}Run()+Run();")
        |> Test_integer_output.expect ~value:(Some 84L) "default"))
    [ ""; "static " ]

let callback_array_unsupported_shapes () =
  List.iter
    (fun mode ->
      List.iter
        (fun storage ->
          List.iter
            (fun (source, code) ->
              let source =
                String.split_on_char '@' source |> String.concat storage
              in
              Alcotest.(check string)
                "unsupported callback array source" code
                (Test_integer_functions.first_error
                   (Test_integer_globals.run ~mode source))
                  .code)
            [
              ("I64 Run(){@I64 (*p)()[2]=0;return 42;}Run();", "HCPARSE0137");
              ( "I64 Run(){@I64 (**p)()[2];return 42;}Run();",
                if storage = "" then "HCIRVM0011" else "HCRUN0001" );
              ("I64 Run(){@I64 (*p)()[2][3];return p[1]();}Run();", "HCSEMA0039");
              ("I64 Run(){@I64 (*p)()[2];return p[1][0]();}Run();", "HCSEMA0039");
              ( "F64 A(){return 42.0;}I64 Run(){@F64 (*p)()[2];p[1]=&A;return \
                 p[1]();}Run();",
                "HCIRVM0014" );
            ])
        [ ""; "static " ];
      ignore
        (Test_integer_globals.run ~mode "I64 (*p)()[2];42;"
        |> Test_integer_functions.expect 42L))
    modes

let callback_array_exact_budgets () =
  List.iter
    (fun mode ->
      List.iter
        (fun storage ->
          let source =
            "I64 Add(I64 n){return n+2;}I64 Run(){" ^ storage
            ^ "I64 (*p)(I64 n)[2][3];p[1][2]=&Add;return p[1][2](40);}Run();"
          in
          let steps =
            Test_integer_globals.run ~mode source
            |> Test_integer_functions.expect 42L
            |> Ir_integer_interpreter.executed_steps
          in
          let frame_bytes = if storage = "" then 56 else 8 in
          let persistent_bytes = if storage = "" then 1 else 48 in
          ignore
            (Test_integer_globals.run ~mode ~max_steps:steps
               ~max_frame_bytes:frame_bytes ~max_global_bytes:persistent_bytes
               ~max_call_depth:2 source
            |> Test_integer_functions.expect 42L);
          List.iter
            (fun (code, result) ->
              Alcotest.(check string)
                "one below callback array budget" code
                (Test_integer_functions.first_error result).code)
            [
              ( "HCIRVM0007",
                Test_integer_globals.run ~mode ~max_steps:(steps - 1) source );
              ( "HCIRVM0011",
                Test_integer_globals.run ~mode
                  ~max_frame_bytes:(frame_bytes - 1) source );
              ( "HCIRVM0015",
                Test_integer_globals.run ~mode ~max_call_depth:1 source );
            ];
          if storage <> "" then
            Alcotest.(check string)
              "one below persistent callback array extent" "HCIRVM0016"
              (Test_integer_functions.first_error
                 (Test_integer_globals.run ~mode ~max_global_bytes:47 source))
                .code)
        [ ""; "static " ])
    modes

let function_address_receipts () =
  let module G = Test_integer_globals in
  let module C = Ir_runtime_call_context in
  let module Seq = Ir_instruction_sequence in
  let module Graph = Ir_block_graph in
  let source = "I64 A(){return 1;}I64 Check(){return &A==&A;}Check();&A==&A;" in
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode source in
      let foreign = G.compile ~mode source in
      let context = integer_program_runtime_calls compiled in
      let publications =
        integer_program_globals compiled
        |> Holyc_lib__Ir.Integer_globals.function_publications
      in
      let owners =
        (C.Entry, integer_program_entry compiled)
        :: List.map
             (fun definition ->
               ( C.Function definition.Ir_integer_interpreter.body,
                 Ir_function_body.x87 definition.body ))
             (integer_program_functions compiled)
      in
      let count = ref 0 in
      List.iter
        (fun (owner, graph) ->
          let addresses =
            C.original_function_addresses context ~owner |> Option.get
          in
          let foreign_addresses =
            C.original_function_addresses
              (integer_program_runtime_calls foreign)
              ~owner
          in
          Ir_x87_stack.graph graph |> Graph.blocks
          |> List.iter (fun block ->
              Graph.instructions block |> Seq.instructions
              |> List.iter (fun instruction ->
                  let description = Seq.description instruction in
                  match C.original_function_address addresses description with
                  | None -> ()
                  | Some address ->
                      incr count;
                      Alcotest.(check bool)
                        "original registered link" true
                        (List.exists
                           (fun link ->
                             Holyc_lib__Ir.Retained_function.same link
                               (C.function_address_link address))
                           publications);
                      let declaration =
                        C.function_address_declaration address
                      in
                      Alcotest.(check bool)
                        "original checked declaration" true
                        (Option.get
                           (R.result_function_declaration
                              (C.function_address_source address))
                        == declaration);
                      Alcotest.(check bool)
                        "original definition body" true
                        (List.exists
                           (fun definition ->
                             Option.fold ~none:false
                               ~some:(fun body ->
                                 body == definition.Ir_integer_interpreter.body)
                               (C.function_address_body address)
                             && Option.get
                                  (Ir_function_body.definition_declaration
                                     definition.body)
                                == declaration)
                           (integer_program_functions compiled));
                      let copy =
                        {
                          description with
                          Seq.operands = List.map Fun.id description.operands;
                        }
                      in
                      Alcotest.(check bool)
                        "copied producer has no receipt" true
                        (Option.is_none
                           (C.original_function_address addresses copy));
                      Alcotest.(check bool)
                        "foreign context has no receipt" true
                        (Option.is_none
                           (Option.bind foreign_addresses (fun addresses ->
                                C.original_function_address addresses
                                  description))))))
        owners;
      Alcotest.(check int)
        "two original body and two original entry addresses" 4 !count)
    modes

let function_address_graph_ownership () =
  let module G = Test_integer_globals in
  let module VM = Ir_integer_interpreter in
  let module Seq = Ir_instruction_sequence in
  let module Graph = Ir_block_graph in
  let module C = Ir_runtime_call_context in
  let source = "I64 A(){return 1;}(&A==&A);" in
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode source in
      let execute ?runtime_calls () =
        VM.execute_program
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ?runtime_calls
          ~functions:(integer_program_functions compiled)
          ~max_steps:1000 ~max_frame_bytes:64 ~max_call_depth:2
          (integer_program_entry compiled)
      in
      let rejects label = function
        | Ok _ -> Alcotest.fail label
        | Error errors ->
            Alcotest.(check bool)
              label true
              (List.for_all
                 (fun (error : VM.error) ->
                   error.stage = VM.Preflight && error.executed_steps = 0)
                 errors)
      in
      rejects "raw symbolic graph cannot resolve executable addresses"
        (execute ());
      let context = integer_program_runtime_calls compiled in
      let addresses =
        C.original_function_addresses context ~owner:C.Entry |> Option.get
      in
      let cell =
        integer_program_entry compiled
        |> Ir_x87_stack.graph |> Graph.blocks
        |> List.find_map (fun block ->
            let rec find = function
              | [] -> None
              | instruction :: rest as cell ->
                  if
                    Option.is_some
                      (C.original_function_address addresses
                         (Seq.description instruction))
                  then Some cell
                  else find rest
            in
            Graph.instructions block |> Seq.instructions |> find)
        |> Option.get
      in
      let original = Seq.description (List.hd cell) in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr
           { original with Seq.operands = List.map Fun.id original.operands });
      Alcotest.(check bool)
        "copied instruction invalidates complete address graph" true
        (Option.is_none (C.original_function_addresses context ~owner:C.Entry));
      rejects "copied instruction cannot execute with original context"
        (execute ~runtime_calls:context ()))
    modes

let function_address_limits_and_reached_faults () =
  let module G = Test_integer_globals in
  let module T = Test_integer_functions in
  let module VM = Ir_integer_interpreter in
  let source =
    "I64 A(){return 1;}I64 Check(){I64 (*p)();p=&A;return p==&A;}Check();"
  in
  List.iter
    (fun mode ->
      let result = G.run ~mode source |> T.expect 1L in
      let steps = VM.executed_steps result in
      ignore (G.run ~mode ~max_steps:steps source |> T.expect 1L);
      Alcotest.(check string)
        "one below actual instruction budget" "HCIRVM0007"
        (T.first_error (G.run ~mode ~max_steps:(steps - 1) source)).code;
      ignore (G.run ~mode ~max_frame_bytes:8 source |> T.expect 1L);
      Alcotest.(check string)
        "one below callback cell frame allocation" "HCIRVM0011"
        (T.first_error (G.run ~mode ~max_frame_bytes:7 source)).code;
      let report =
        Test_integer_output.run ~mode
          "extern U0 Print(U8 *fmt,...);I64 A(){return \
           1;}Print(\"kept\");&A==1;"
      in
      ignore (Test_integer_output.fault ~output:"kept" "HCIRVM0024" report);
      Alcotest.(check string)
        "reached unsupported numeric address retains earlier output" "kept"
        (integer_program_report_output_bytes report))
    modes

let replaced_function_address_keeps_original_body () =
  let source =
    "I64 A(){return 1;}I64 P;P=&A;I64 Old(){return P==&A;}I64 A(){return \
     2;}(Old()*100+(P!=&A));"
  in
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit source
    |> Test_integer_functions.expect 101L);
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit
       "I64 A(){return 1;}I64 P;P=&A;I64 Old(){I64 (*p)();p=P;return p();}I64 \
        A(){return 2;}Old()*100+A();"
    |> Test_integer_functions.expect 102L)

let aot_definition_snapshots_keep_original_bodies () =
  let module T = Test_integer_functions in
  let module G = Test_integer_globals in
  let module B = Ir_function_body in
  let module VM = Ir_integer_interpreter in
  List.iter
    (fun source ->
      ignore (G.run ~mode:Preprocessor.Jit source |> T.expect 42L);
      let result = G.run ~mode:Preprocessor.Aot source |> T.expect 42L in
      let steps = VM.executed_steps result in
      ignore
        (G.run ~mode:Preprocessor.Aot ~max_steps:steps source |> T.expect 42L);
      Alcotest.(check string)
        "one below original-body execution work" "HCIRVM0007"
        (T.first_error
           (G.run ~mode:Preprocessor.Aot ~max_steps:(steps - 1) source))
          .code)
    [
      "I64 Add(I64 n){return n+1;}I64 (*P)(I64 n)=&Add;I64 Add(I64 n){return \
       n+2;}P(41);";
      "I64 Add(I64 n){return n+1;}I64 (*P)(I64 n)=&Add;I64 Add(I64 n){return \
       n+2;}I64 (*Q)(I64 n)=&Add;I64 Check(){return P(41)+Q(40)-42;}Check();";
      "I64 Add(I64 n){return n+1;}I64 Early(){return Add(41);}I64 Add(I64 \
       n){return n+2;}I64 Check(){return Early()+Add(40)-42;}Check();";
      "I64 Add(I64 n){return n+2;}I64 (*P)(I64 n)=&Add;I64 Add(I64 n,I64 \
       m){return n+m;}P(40);";
    ];
  let source =
    "I64 Add(I64 n){return n+1;}I64 (*P)(I64 n)=&Add;I64 Add(I64 n){return \
     n+2;}P(41);"
  in
  let compiled = G.compile ~mode:Preprocessor.Aot source in
  let functions = integer_program_functions compiled in
  let first = List.hd functions and second = List.nth functions 1 in
  Alcotest.(check bool)
    "checked definitions share their canonical callable symbol" true
    (B.callable_symbol first.body == B.callable_symbol second.body);
  Alcotest.(check bool)
    "each original body keeps a distinct source symbol" false
    (Semantic_symbol.Id.equal
       (Semantic_symbol.id (B.symbol first.body))
       (Semantic_symbol.id (B.symbol second.body)));
  Alcotest.(check bool)
    "each original body keeps a distinct checked declaration" false
    (Option.get (B.definition_declaration first.body)
    == Option.get (B.definition_declaration second.body));
  let execute ?runtime_calls functions =
    VM.execute_program ?runtime_calls
      ~globals:(integer_program_globals compiled)
      ~initialization:(integer_program_initialization compiled)
      ~functions ~max_steps:1000 ~max_frame_bytes:64 ~max_call_depth:4
      (integer_program_entry compiled)
  in
  let reject label result =
    match result with
    | Ok _ -> Alcotest.fail label
    | Error errors ->
        Alcotest.(check bool)
          label true
          (List.for_all
             (fun (error : VM.error) ->
               error.stage = VM.Preflight && error.executed_steps = 0)
             errors)
  in
  let repeated = execute (first :: functions) in
  reject "a repeated body still rejects before execution" repeated;
  Alcotest.(check string)
    "repeated body identity still rejects" "HCIRVM0014"
    (List.hd (Result.get_error repeated)).code;
  let context = integer_program_runtime_calls compiled in
  reject "reordered original bodies lose their complete bundle authority"
    (execute ~runtime_calls:context (List.rev functions));
  let other = G.compile ~mode:Preprocessor.Aot source in
  reject "equal-source foreign bodies cannot substitute original definitions"
    (execute ~runtime_calls:context (integer_program_functions other));
  reject "the original body still requires its original frame"
    (execute ~runtime_calls:context
       [ { first with frame = second.frame }; second ])

let checked_callback_invocation () =
  let cases =
    [
      ( "I64 Narrow(U8 n){return n;}I64 Run(){I64 (*p)(U8 n);p=&Narrow;return \
         p(298);}Run();",
        42L );
      ( "U64 Add(U64 n){return n+2;}I64 Run(){U64 (*p)(U64 n);p=&Add;return \
         p(40);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Invoke(I64 (*q)(I64 n),I64 n){return \
         q(n);}I64 Run(){I64 (*p)(I64 (*q)(I64 n),I64 n);p=&Invoke;return \
         p(&Add,40);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Base(){return 40;}I64 Run(){I64 \
         (*p)(I64 n),(*q)();p=&Add;q=&Base;return p(q());}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Take(I64 a,I64 b){return a+b;}I64 \
         Run(){I64 (*p)(I64 n);p=&Add;return Take(p(38),2);}Run();",
        42L );
      ( "I64 Add(){return 42;}I64 Run(){I64 (*p)();p=&Add;return p();}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n);p=&Add;return \
         p(p=0);}Run();",
        2L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n);p=&Add;return \
         p(p(38));}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n);p=&Add;return \
         (p)(40);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Bad(I64 a,I64 b){return a+b;}I64 \
         Apply(I64 (*p)(I64 n),I64 n){p=&Add;return p(n);}Apply(&Bad,40);",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n);p=&Add;return \
         p(40);}Run();",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n),I64 n){return \
         p(n);}Apply(&Add,40);",
        42L );
      ( "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n),(*q)(I64 \
         n);p=&Add;q=p;return q(40);}Run();",
        42L );
      ( "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 n=0;I64 (*p)(I64 \
         a,I64 b);p=&Take;return p(++n,++n);}Run();",
        21L );
      ( "U0 Write(I64 n){Print(\"%d\",n);}I64 Run(){U0 (*p)(I64 \
         n);p=&Write;p(42);return 42;}Run();",
        42L );
      ( "I64 Sum(I64 n,...){if(!argc)return n;return \
         n+argc+argv[0]+argv[1];}I64 Run(){I64 (*p)(I64 n,...);p=&Sum;return \
         p(37,1,2)+p(0);}Run();",
        42L );
      ( "I64 Walk(I64 n){I64 (*p)(I64 n);p=&Walk;if(n)return p(n-1)+1;return \
         0;}Walk(42);",
        42L );
      ( "I64 Take(I64 a,I64 b){return a*10+b;}I64 Run(){I64 (*p)(I64 a,I64 \
         b);p=&Take;return p(Take(1,2),Take(3,4));}Run();",
        154L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let source = "extern U0 Print(U8 *fmt,...);" ^ source in
          ignore
            (Test_integer_globals.run ~mode ~max_call_depth:64 source
            |> Test_integer_functions.expect expected))
        cases)
    modes

let callback_reached_faults_and_effects () =
  let prefix =
    "extern U0 Print(U8 *fmt,...);I64 Bad(I64 a,I64 b){return a+b;}I64 \
     Side(){Print(\"arg\");return 40;}"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (initial, code) ->
          let source =
            prefix ^ "I64 Run(){I64 (*p)(I64 n);p=" ^ initial
            ^ ";return p(Side());}Print(\"before\");Run();"
          in
          let report = Test_integer_output.run ~mode source in
          let error =
            Test_integer_output.fault ~output:"beforearg" code report
          in
          Alcotest.(check bool)
            "target failure occurs after argument effects" true
            (List.mem "stage=execution" error.notes
            && not (List.mem "executed_steps=0" error.notes)))
        [ ("0", "HCIRVM0024"); ("123", "HCIRVM0024"); ("&Bad", "HCIRVM0014") ];
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);U0 Write(I64 n){Print(\"%d\",n);}I64 \
            Run(){U0 (*p)(I64 n);p=&Write;p(42);return 42;}Run();"
        |> Test_integer_output.expect "42");
      ignore
        (Test_integer_globals.run ~mode
           "I64 Run(){I64 (*p)(I64 n);p=0;if(0)p(40);return 42;}Run();"
        |> Test_integer_functions.expect 42L))
    modes

let callback_exact_budgets () =
  let source =
    "I64 Add(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n);p=&Add;return \
     p(40);}Run();"
  in
  List.iter
    (fun mode ->
      let result =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 42L
      in
      let steps = Ir_integer_interpreter.executed_steps result in
      ignore
        (Test_integer_globals.run ~mode ~max_steps:steps ~max_frame_bytes:16
           ~max_call_depth:2 source
        |> Test_integer_functions.expect 42L);
      List.iter
        (fun (code, result) ->
          Alcotest.(check string)
            "one below actual callback budget" code
            (Test_integer_functions.first_error result).code)
        [
          ( "HCIRVM0007",
            Test_integer_globals.run ~mode ~max_steps:(steps - 1) source );
          ( "HCIRVM0011",
            Test_integer_globals.run ~mode ~max_frame_bytes:15 source );
          ("HCIRVM0015", Test_integer_globals.run ~mode ~max_call_depth:1 source);
        ])
    modes

let callback_graph_ownership_for ?(dimensions = "") ?(element = "") storage =
  let module VM = Ir_integer_interpreter in
  let module C = Ir_runtime_call_context in
  let module Graph = Ir_block_graph in
  let module Seq = Ir_instruction_sequence in
  let source =
    "I64 Add(I64 n){return n+2;}I64 Run(){" ^ storage ^ "I64 (*p)(I64 n)"
    ^ dimensions ^ ";p" ^ element ^ "=&Add;return p" ^ element ^ "(40);}Run();"
  in
  List.iter
    (fun mode ->
      let compiled = Test_integer_globals.compile ~mode source in
      let context = integer_program_runtime_calls compiled in
      let body =
        integer_program_functions compiled
        |> List.find (fun (definition : VM.function_definition) ->
            Semantic_symbol.name (Ir_function_body.symbol definition.body)
            = "Run")
        |> fun definition -> definition.body
      in
      let callback =
        C.original_callback_calls context ~owner:(C.Function body)
        |> Option.get |> List.hd
      in
      Alcotest.(check bool)
        "original loaded instruction supplies receipt" true
        (Option.fold ~none:false ~some:(( == ) callback)
           (C.find_callback_load context ~owner:(C.Function body)
              callback.callback_load));
      Alcotest.(check bool)
        "copied loaded instruction supplies no receipt" true
        (Option.is_none
           (C.find_callback_load context ~owner:(C.Function body)
              {
                callback.callback_load with
                Seq.operands = List.map Fun.id callback.callback_load.operands;
              }));
      let execute ?runtime_calls () =
        VM.execute_program
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~functions:(integer_program_functions compiled)
          ?runtime_calls ~max_steps:10000 ~max_frame_bytes:1024
          ~max_call_depth:16
          (integer_program_entry compiled)
      in
      let reject label = function
        | Ok _ -> Alcotest.fail label
        | Error errors ->
            Alcotest.(check bool)
              label true
              (List.for_all
                 (fun (error : VM.error) ->
                   error.stage = VM.Preflight && error.executed_steps = 0)
                 errors)
      in
      reject "raw callback graph has no source authority" (execute ());
      let foreign = Test_integer_globals.compile ~mode source in
      reject "foreign callback graph cannot provide original authority"
        (execute ~runtime_calls:(integer_program_runtime_calls foreign) ());
      let rec find = function
        | [] -> None
        | instruction :: rest as cell ->
            if (Seq.description instruction).opcode = Ir_opcode.Ic_call_indirect
            then Some cell
            else find rest
      in
      let cell =
        Ir_function_body.body body |> Graph.blocks
        |> List.find_map (fun block ->
            Graph.instructions block |> Seq.instructions |> find)
        |> Option.get
      in
      let original = Seq.description (List.hd cell) in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr
           { original with Seq.operands = List.map Fun.id original.operands });
      Alcotest.(check bool)
        "equal copy invalidates complete callback owner" true
        (Option.is_none
           (C.original_callback_calls context ~owner:(C.Function body)));
      reject "changed callback graph cannot retain original authority"
        (execute ~runtime_calls:context ()))
    modes

let callback_graph_ownership () = callback_graph_ownership_for ""
let static_callback_graph_ownership () = callback_graph_ownership_for "static "

let callback_array_graph_ownership () =
  List.iter
    (callback_graph_ownership_for ~dimensions:"[2][3]" ~element:"[1][2]")
    [ ""; "static " ]

let static_callback_limits_and_boundaries () =
  let source =
    "I64 Add(I64 n){return n+2;}I64 Run(){static I64 (*p)(I64 n);p=&Add;return \
     p(40);}Run();"
  in
  List.iter
    (fun mode ->
      let result =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 42L
      in
      let steps = Ir_integer_interpreter.executed_steps result in
      ignore
        (Test_integer_globals.run ~mode ~max_steps:steps ~max_global_bytes:8
           ~max_frame_bytes:8 ~max_call_depth:2 source
        |> Test_integer_functions.expect 42L);
      List.iter
        (fun (code, result) ->
          Alcotest.(check string)
            "one below static callback budget" code
            (Test_integer_functions.first_error result).code)
        [
          ( "HCIRVM0007",
            Test_integer_globals.run ~mode ~max_steps:(steps - 1) source );
          ( "HCIRVM0016",
            Test_integer_globals.run ~mode ~max_global_bytes:7 source );
          ( "HCIRVM0011",
            Test_integer_globals.run ~mode ~max_frame_bytes:7 source );
          ("HCIRVM0015", Test_integer_globals.run ~mode ~max_call_depth:1 source);
        ];
      List.iter
        (fun (text, code) ->
          Alcotest.(check string)
            "static callback boundary" code
            (Test_integer_functions.first_error
               (Test_integer_globals.run ~mode text))
              .code)
        [
          ("I64 Run(){static I64 (*p)()=0;return 42;}Run();", "HCPARSE0137");
          ("I64 Run(){static I64 (**p)()[2];return 42;}Run();", "HCRUN0001");
          ("I64 Run(){static I64 (**p)();return 42;}Run();", "HCRUN0001");
        ])
    modes;
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit
       "I64 A(){return 1;}I64 Run(I64 set){static I64 \
        (*p)();if(set)p=&A;return p();}Run(1);I64 A(){return \
        2;}Run(0)*100+A();"
    |> Test_integer_functions.expect 102L)

let callback_parameter_defaults_execute () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          ignore
            (Test_integer_globals.run ~mode source
            |> Test_integer_functions.expect expected))
        [
          ("I64 Check(I64 (*p)()=0){return p==0;}Check();", 1L);
          ("I64 Check(F64 (*p)()=0){return p==0;}Check();", 1L);
          ("I64 Check(U0 (*p)()=0){return p==0;}Check();", 1L);
          ("I64 Check(I64 *(*p)()=0){return p==0;}Check();", 1L);
          ("I64 Check(I64 (*p)()=40+2){return p==42;}Check();", 1L);
          ("I64 Check(I64 (*p)()=0,I64 n=42){return (p==0)*n;}Check();", 42L);
          ("I64 Check(I64 n=42,I64 (*p)()=0){return (p==0)*n;}Check();", 42L);
          ( "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 \
             n)=0){p=&Add;return p(40);}Apply();",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n)=0){return \
             p(40);}Apply(&Add);",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 \
             n)=0){p=&Add;return p(40);}I64 Run(){return Apply();}Run();",
            42L );
          ( "I64 Check(I64 (*p)()=0){return p==0;}I64 Run(){return \
             Check()+Check();}Run();",
            2L );
        ])
    modes

let callback_defaults_preserve_declaration_effects () =
  let source =
    "I64 Count;Count=0;I64 Check(I64 (*p)()=(Count=Count+1)){return \
     p==1;}Check()+Check()+Count;"
  in
  ignore (Test_integer_globals.run source |> Test_integer_functions.expect 3L);
  let source =
    "extern U0 Print(U8 *fmt,...);I64 Default(){Print(\"default\");return \
     0;}I64 Check(I64 (*p)()=Default()){return p==0;}Check()+Check();"
  in
  ignore
    (Test_integer_output.run source
    |> Test_integer_output.expect ~value:(Some 2L) "default")

let callback_defaults_keep_original_header_and_limits () =
  ignore
    (Test_integer_globals.run
       "I64 A(){return 1;}I64 Check(I64 (*p)()=0){return p==0;}I64 Run(){I64 \
        (*p)();p=&A;return p();}I64 A(){return 2;}Run()*100+A();"
    |> Test_integer_functions.expect 102L);
  ignore
    (Test_integer_globals.run
       "I64 Check(I64 (*p)()=0){return p==0;}I64 Old(){return Check();}I64 \
        Check(I64 (*p)()=123){return p==123;}Old()*10+Check();"
    |> Test_integer_functions.expect 11L);
  List.iter
    (fun mode ->
      let source = "I64 Check(I64 (*p)()=0){return p==0;}Check();" in
      let result =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 1L
      in
      let steps = Ir_integer_interpreter.executed_steps result in
      ignore
        (Test_integer_globals.run ~mode ~max_steps:steps source
        |> Test_integer_functions.expect 1L);
      Alcotest.(check string)
        "one fewer step cannot finish" "HCIRVM0007"
        (Test_integer_functions.first_error
           (Test_integer_globals.run ~mode ~max_steps:(steps - 1) source))
          .code;
      Alcotest.(check string)
        "multistar callback default is outside word storage" "HCRUN0001"
        (Test_integer_functions.first_error
           (Test_integer_globals.run ~mode
              "I64 Check(I64 (**p)()=0){return 42;}Check();"))
          .code;
      Alcotest.(check string)
        "aggregate default signature still needs layout admission"
        (if mode = Preprocessor.Jit then "HCRUN0004" else "HCEVAL0003")
        (Test_integer_functions.first_error
           (Test_integer_globals.run ~mode
              "class Box {I64 n;};I64 Check(Box (*p)()=0){return p==0;}Check();"))
          .code;
      Alcotest.(check string)
        "owned-code defaults need their original value receipt"
        (if mode = Preprocessor.Jit then "HCRUN0001" else "HCRUN0006")
        (Test_integer_functions.first_error
           (Test_integer_globals.run ~mode
              "I64 A(){return 42;}I64 Apply(I64 (*p)()=&A){return p();}Apply();"))
          .code)
    modes

let callback_default_words_have_no_executable_authority () =
  List.iter
    (fun mode ->
      List.iter
        (fun initial ->
          let source =
            "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"arg\");return \
             40;}I64 Apply(I64 (*p)(I64 n)=" ^ initial
            ^ "){return p(Side());}Print(\"before\");Apply();"
          in
          let error =
            Test_integer_output.run ~mode source
            |> Test_integer_output.fault ~output:"beforearg" "HCIRVM0024"
          in
          Alcotest.(check bool)
            "numeric default fails after argument effects" true
            (List.mem "stage=execution" error.notes
            && not (List.mem "executed_steps=0" error.notes)))
        [ "0"; "123" ])
    modes

let callback_default_producers_require_original_ownership () =
  let module VM = Ir_integer_interpreter in
  let module C = Ir_runtime_call_context in
  let module Seq = Ir_instruction_sequence in
  List.iter
    (fun mode ->
      let source = "I64 Check(I64 (*p)()=0){return p==0;}Check();" in
      let compiled =
        match mode with
        | Preprocessor.Aot -> Test_integer_globals.compile ~mode source
        | Preprocessor.Jit ->
            let session, config, source =
              Test_integer_functions.inputs ~mode source
            in
            let report =
              compile_integer_program_report session ~config ~source
            in
            ignore
              (Test_integer_functions.checked
                 (integer_program_compilation_result report));
            integer_program_compilation_units report
            |> List.find (fun unit_ ->
                integer_program_runtime_calls unit_ |> fun context ->
                C.original_prepared_defaults context ~owner:C.Entry
                |> Option.fold ~none:false ~some:(fun items -> items <> []))
      in
      let context = integer_program_runtime_calls compiled in
      let original =
        C.original_prepared_defaults context ~owner:C.Entry
        |> Option.get |> List.hd
      in
      check_word_pointer 1 original.Seq.target_type;
      Alcotest.(check bool)
        "original default has checked receipt" true
        (C.is_prepared_default context ~owner:C.Entry original.instruction_id);
      let execute ?runtime_calls () =
        VM.execute_program
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~functions:(integer_program_functions compiled)
          ?runtime_calls ~max_steps:10000 ~max_frame_bytes:1024
          ~max_call_depth:16
          (integer_program_entry compiled)
      in
      let reject label = function
        | Ok _ -> Alcotest.fail label
        | Error errors ->
            Alcotest.(check bool)
              label true
              (List.for_all
                 (fun (error : VM.error) ->
                   error.stage = VM.Preflight && error.executed_steps = 0)
                 errors)
      in
      reject "raw default graph has no saved-default authority" (execute ());
      let rec find = function
        | [] -> None
        | item :: rest as cell ->
            if Seq.description item == original then Some cell else find rest
      in
      let cell =
        integer_program_entry compiled
        |> Ir_x87_stack.graph |> Ir_block_graph.blocks
        |> List.find_map (fun block ->
            Ir_block_graph.instructions block |> Seq.instructions |> find)
        |> Option.get
      in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr
           { original with Seq.operands = List.map Fun.id original.operands });
      Alcotest.(check bool)
        "copied default loses complete graph authority" true
        (Option.is_none (C.original_prepared_defaults context ~owner:C.Entry));
      Alcotest.(check bool)
        "copied default ID supplies no receipt" false
        (C.is_prepared_default context ~owner:C.Entry original.instruction_id);
      reject "copied default cannot execute" (execute ~runtime_calls:context ()))
    modes

let anonymous_signature_defaults_execute () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          ignore
            (Test_integer_globals.run ~mode source
            |> Test_integer_functions.expect expected))
        [
          ( "I64 A(I64 n=41){return n+2;}I64 Run(){I64 (*p)(I64 \
             n=40);p=&A;return p();}Run();",
            42L );
          ( "I64 A(I64 n=41){return n+2;}I64 Apply(I64 (*p)(I64 n=40)){return \
             p();}Apply(&A);",
            42L );
          ( "I64 Check(I64 (*inner)()){return inner==0;}I64 Run(){I64 (*p)(I64 \
             (*inner)()=0);p=&Check;return p();}Run();",
            1L );
          ( "I64 A(I64 a,I64 b){return a+b;}I64 Run(){I64 (*p)(I64 a=40,I64 \
             b=99);p=&A;return p(,2);}Run();",
            42L );
          ( "I64 A(I64 n,...){return n+argc;}I64 Run(){I64 (*p)(I64 \
             n=40,...);p=&A;return p(,1,2);}Run();",
            42L );
          ( "I64 A(I8 n){return n+40;}I64 Run(){I64 (*p)(I8 n=258);p=&A;return \
             p();}Run();",
            42L );
          ("I64 Check(I64 (*p)(I64 n=42)=0){return p==0;}Check();", 1L);
          ( "I64 A(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n=99);p=&A;return \
             p(40);}Run();",
            42L );
          ( "I64 A(I64 a,I64 b){return a+b;}I64 Run(){I64 (*p)(I64 a=40,I64 \
             b=2);p=&A;return p();}Run();",
            42L );
          ( "I64 A(I64 n){return n+2;}I64 Run(){I64 (*p)(;I64 \
             n=40;);p=&A;return p();}Run();",
            42L );
          ( "I64 A(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 \
             n=40)=0){p=&A;return p();}Apply();",
            42L );
          ( "U0 A(I64 n){I64 x=n;}I64 Run(){U0 (*p)(I64 n=40);p=&A;p();return \
             42;}Run();",
            42L );
        ])
    modes;
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit
       "I64 A(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n=40);p=&A;return \
        p();}I64 Old(){return Run();}I64 Run(){I64 (*p)(I64 n=1);p=&A;return \
        p();}Old()*100+Run();"
    |> Test_integer_functions.expect 4203L)

let anonymous_defaults_preserve_declaration_effects () =
  ignore
    (Test_integer_globals.run
       "I64 Count;Count=0;I64 A(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
        n=(Count=Count+1));p=&A;return p();}Run()+Run()+Count;"
    |> Test_integer_functions.expect 3L);
  ignore
    (Test_integer_globals.run
       "I64 Count;Count=0;I64 Apply(I64 (*p)(I64 n=(Count=Count+1))){return \
        7;}Count;"
    |> Test_integer_functions.expect 1L);
  ignore
    (Test_integer_output.run
       "extern U0 Print(U8 *fmt,...);I64 D(){Print(\"default\");return 40;}I64 \
        A(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n=D());p=&A;return \
        p();}Run()+Run();"
    |> Test_integer_output.expect ~value:(Some 84L) "default")

let anonymous_default_words_retain_reached_faults () =
  List.iter
    (fun mode ->
      let source =
        "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"argument\");return \
         2;}I64 Run(){I64 (*p)(I64 a=40,I64 b=99);p=0;return \
         p(,Side());}Run();"
      in
      let report = Test_integer_output.run ~mode source in
      Alcotest.(check string)
        "reached null target retains argument output" "argument"
        (integer_program_report_output_bytes report);
      Alcotest.(check string)
        "numeric word grants no executable owner" "HCIRVM0024"
        (Test_integer_functions.first_error
           (integer_program_report_outcome report))
          .code;
      let source =
        "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"argument\");return \
         2;}I64 A(I64 a){return a;}I64 Run(){I64 (*p)(I64 a=40,I64 \
         b=99);p=&A;return p(,Side());}Run();"
      in
      let report = Test_integer_output.run ~mode source in
      Alcotest.(check string)
        "reached mismatch retains argument output" "argument"
        (integer_program_report_output_bytes report);
      Alcotest.(check string)
        "anonymous signature is checked against reached owner" "HCIRVM0014"
        (Test_integer_functions.first_error
           (integer_program_report_outcome report))
          .code)
    modes;
  let report =
    Test_integer_output.run
      "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"default\");return \
       0;}I64 Run(){I64 (*p)(I64 n=42/Side());return 42;}42;"
  in
  Alcotest.(check string)
    "failed default retains earlier declaration output" "default"
    (integer_program_report_output_bytes report);
  Alcotest.(check string)
    "failed default prevents signature admission" "HCIRVM0009"
    (Test_integer_functions.first_error (integer_program_report_outcome report))
      .code

let anonymous_default_limits_and_boundaries () =
  List.iter
    (fun mode ->
      let source =
        "I64 A(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n=40);p=&A;return \
         p();}Run();"
      in
      let result =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 42L
      in
      let steps = Ir_integer_interpreter.executed_steps result in
      ignore
        (Test_integer_globals.run ~mode ~max_steps:steps source
        |> Test_integer_functions.expect 42L);
      Alcotest.(check string)
        "one fewer runtime step fails" "HCIRVM0007"
        (Test_integer_functions.first_error
           (Test_integer_globals.run ~mode ~max_steps:(steps - 1) source))
          .code;
      ignore
        (Test_integer_output.run ~mode ~max_initializer_steps:3 source
        |> Test_integer_output.expect "");
      Alcotest.(check string)
        "one fewer default preparation step fails" "HCIRVM0007"
        (Test_integer_functions.first_error
           (Test_integer_output.run ~mode ~max_initializer_steps:2 source
           |> integer_program_report_outcome))
          .code;
      Alcotest.(check bool)
        "F64 expressions need their own preparation consumer" true
        (Result.is_error
           (Test_integer_globals.run ~mode
              "I64 Run(){I64 (*p)(F64 n=1.5);return 42;}42;"));
      Alcotest.(check bool)
        "owned-code defaults cannot become numeric bits" true
        (Result.is_error
           (Test_integer_globals.run ~mode
              "I64 A(){return 42;}I64 Run(){I64 (*p)(I64 (*f)()=&A);return \
               42;}42;"));
      Alcotest.(check bool)
        "ordinary $$ needs instruction-address authority" true
        (Result.is_error
           (Test_integer_globals.run ~mode
              "I64 A(I64 n){return n;}I64 Run(){I64 (*p)(I64 n=$$);p=&A;return \
               p();}Run();")))
    modes

let anonymous_position_defaults_execute () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          ignore
            (Test_integer_globals.run ~mode source
            |> Test_integer_functions.expect 42L))
        [
          "class Box{I64 (*p)(I64 a=$$,I64 b=$$+34);};sizeof(Box)+34;";
          "class Box{I64 (*p)(I64 (*q)(I64 a,I64 b)=$$,I64 \
           n=$$);};sizeof(Box)+34;";
          "class Box{I64 (*p)(I64 (*q)(I64 a,I64 b;)=$$,I64 \
           n=$$);};sizeof(Box)+34;";
          "class Box{I64 (*p)(;;;I64 a=$$;;;I64 b=$$+34;;;);};sizeof(Box)+34;";
          "class Box{I64 (*p)(I64 (*q)(I64 a,I64 b,...)=$$);};sizeof(Box)+34;";
          "class Box{F64 (*p)(I64 a=$$,I8 b=$$+250)[2];};sizeof(Box)+26;";
          "class Box{U8 pad[3];I64 (*p)(I64 a=$$,I64 b=$$+34);};sizeof(Box)+31;";
          "union Box{I64 (*p)(I64 a=$$,I64 b=$$+34);U8 \
           pad[17];};sizeof(Box)+25;";
        ])
    modes

let anonymous_position_defaults_evaluate_once () =
  ignore
    (Test_integer_globals.run
       "I64 Count;Count=0;I64 D(I64 n){Count=Count+1+n;return n+34;}class \
        Box{I64 (*p)(I64 a=$$,I64 b=D($$));};Count;"
    |> Test_integer_functions.expect 9L);
  ignore
    (Test_integer_output.run
       "extern U0 Print(U8 *fmt,...);I64 D(I64 n){Print(\"default\");return \
        n+34;}class Box{I64 (*p)(I64 a=$$,I64 b=D($$));};42;"
    |> Test_integer_output.expect ~value:(Some 42L) "default")

let anonymous_position_defaults_preserve_reached_faults () =
  let report =
    Test_integer_output.run
      "extern U0 Print(U8 *fmt,...);I64 D(I64 n){Print(\"default\");return \
       n-n;}class Box{I64 (*p)(I64 a=$$,I64 b=42/D($$));};42;"
  in
  Alcotest.(check string)
    "failed class positional default retains output" "default"
    (integer_program_report_output_bytes report);
  Alcotest.(check string)
    "failed default prevents class signature admission" "HCIRVM0009"
    (Test_integer_functions.first_error (integer_program_report_outcome report))
      .code

let anonymous_position_defaults_charge_exact_work () =
  List.iter
    (fun mode ->
      let source = "class Box{I64 (*p)(I64 a=$$,I64 b=$$+34);};42;" in
      let result =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 42L
      in
      let steps = Ir_integer_interpreter.executed_steps result in
      ignore
        (Test_integer_globals.run ~mode ~max_steps:steps source
        |> Test_integer_functions.expect 42L);
      Alcotest.(check string)
        "one fewer outer step fails" "HCIRVM0007"
        (Test_integer_functions.first_error
           (Test_integer_globals.run ~mode ~max_steps:(steps - 1) source))
          .code;
      ignore
        (Test_integer_output.run ~mode ~max_initializer_steps:8 source
        |> Test_integer_output.expect ~value:(Some 42L) "");
      Alcotest.(check string)
        "one fewer class position preparation step fails" "HCIRVM0007"
        (Test_integer_functions.first_error
           (Test_integer_output.run ~mode ~max_initializer_steps:7 source
           |> integer_program_report_outcome))
          .code)
    modes

let ordinary_position_defaults_retain_instruction_addresses () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          Alcotest.(check bool)
            "ordinary $$ is not a member-size integer" true
            (Result.is_error (Test_integer_globals.run ~mode source)))
        [
          "I64 A(I64 a=$$,I64 b=$$+34){return a+b;}A();";
          "I64 A(I64 n){return n;}I64 Run(){I64 (*p)(I64 n=$$);p=&A;return \
           p();}Run();";
          "class Box{I64 (*p)(I64 n=$$);};I64 A(I64 n=$$){return n;}A();";
          "union Box{I64 (*p)(I64 n=$$);};I64 A(I64 n=$$){return n;}A();";
        ])
    modes

let anonymous_defaults_require_original_producers () =
  let module C = Ir_runtime_call_context in
  let module P = Ir_prepared_callback_default in
  let module VM = Ir_integer_interpreter in
  let module Seq = Ir_instruction_sequence in
  let module Graph = Ir_block_graph in
  List.iter
    (fun mode ->
      let source =
        "I64 Check(I64 (*inner)()){return inner==0;}I64 Run(){I64 (*p)(I64 \
         (*inner)()=0);p=&Check;return p();}Run();"
      in
      let compiled =
        match mode with
        | Preprocessor.Aot -> Test_integer_globals.compile ~mode source
        | Jit ->
            let session, config, source =
              Test_integer_functions.inputs ~mode source
            in
            let report =
              compile_integer_program_report session ~config ~source
            in
            ignore
              (Test_integer_functions.checked
                 (integer_program_compilation_result report));
            integer_program_compilation_units report
            |> List.find (fun unit_ ->
                integer_program_functions unit_
                |> List.exists (fun (d : VM.function_definition) ->
                    Semantic_symbol.name (Ir_function_body.symbol d.body)
                    = "Run"))
      in
      let context = integer_program_runtime_calls compiled in
      let body =
        integer_program_functions compiled
        |> List.find (fun (d : VM.function_definition) ->
            Semantic_symbol.name (Ir_function_body.symbol d.body) = "Run")
        |> fun d -> d.body
      in
      let callback =
        C.original_callback_calls context ~owner:(C.Function body)
        |> Option.get |> List.hd
      in
      let prepared =
        callback.callback_arguments
        |> List.find_map C.argument_prepared_callback_default
        |> Option.get
      in
      Alcotest.(check int64) "saved anonymous member word" 0L (P.bits prepared);
      let header = P.header prepared in
      let pointer = callback.callback_pointer in
      Alcotest.(check bool)
        "original anonymous declarator is retained" true
        (Option.fold ~none:false
           ~some:(( == ) header.Parser.callback_pointer)
           (F.function_pointer_source pointer));
      let parameter =
        F.function_pointer_signature pointer
        |> F.signature_parameters |> List.hd
      in
      Alcotest.(check bool)
        "original pointer and member own the default" true
        (P.matches prepared ~pointer ~parameter);
      let ast = header.callback_pointer in
      let copied_ast =
        Ast.make_function_pointer_declarator
          ~declarator_opening_parenthesis:ast.declarator_opening_parenthesis
          ~indirection_layers:ast.indirection_layers
          ~declarator_closing_parenthesis:ast.declarator_closing_parenthesis
          ~signature_opening_parenthesis:ast.signature_opening_parenthesis
          ~signature_parameters:(List.map Fun.id ast.signature_parameters)
          ~signature_empty_parameter_entries:
            ast.signature_empty_parameter_entries
          ~signature_variadic:ast.signature_variadic
          ~signature_closing_parenthesis:ast.signature_closing_parenthesis
          ~function_pointer_location:ast.function_pointer_location
      in
      let copied_pointer =
        F.make_source_function_pointer ~source:copied_ast
          ~origin:(F.function_pointer_origin pointer)
          ~opening_origin:(F.function_pointer_opening_origin pointer)
          ~indirection_origins:(F.function_pointer_indirection_origins pointer)
          ~closing_origin:(F.function_pointer_closing_origin pointer)
          ~signature:(F.function_pointer_signature pointer)
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "equal signature does not adopt original saved member" false
        (P.matches prepared ~pointer:copied_pointer ~parameter);
      let original =
        C.original_prepared_defaults context ~owner:(C.Function body)
        |> Option.get |> List.hd
      in
      check_word_pointer 1 original.target_type;
      let rec find = function
        | [] -> None
        | item :: rest as cell ->
            if Seq.description item == original then Some cell else find rest
      in
      let cell =
        Ir_function_body.body body |> Graph.blocks
        |> List.find_map (fun block ->
            Graph.instructions block |> Seq.instructions |> find)
        |> Option.get
      in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr
           { original with Seq.operands = List.map Fun.id original.operands });
      Alcotest.(check bool)
        "copied default invalidates its complete owner" true
        (Option.is_none
           (C.original_prepared_defaults context ~owner:(C.Function body)));
      let rejected =
        VM.execute_program
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~functions:(integer_program_functions compiled)
          ~runtime_calls:context ~max_steps:10000 ~max_frame_bytes:1024
          ~max_call_depth:16
          (integer_program_entry compiled)
      in
      match rejected with
      | Ok _ -> Alcotest.fail "copied anonymous default executed"
      | Error errors ->
          Alcotest.(check bool)
            "copied graph fails before execution" true
            (List.for_all
               (fun (e : VM.error) ->
                 e.stage = VM.Preflight && e.executed_steps = 0)
               errors))
    modes

let global_callback_storage_execution () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) ->
          ignore
            (Test_integer_globals.run ~mode text
            |> Test_integer_functions.expect expected))
        [
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);I64 \
             Run(){p=&Add;return p(40);}Run();",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2][3];I64 \
             Run(){p[1][2]=&Add;return p[1][2](40);}Run();",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n),(*q)(I64 n);I64 \
             Run(){p=&Add;q=p;p=0;return q(40);}Run();",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);I64 Run(){return \
             p(40);}p=&Add;Run();",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2];I64 Apply(I64 \
             (*q)(I64 n)){return q(40);}I64 Run(){p[1]=&Add;return \
             Apply(p[1]);}Run();",
            42L );
          ( "I64 Add(I64 n=17){return n;}I64 (*p)(I64 n=42);I64 \
             Run(){p=&Add;return p();}Run();",
            42L );
          ( "I64 Add(I64 n=17){return n;}I64 (*p)(I64 n=42)[2];I64 \
             Run(){p[1]=&Add;return p[1]();}Run();",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);I64 \
             Run(){p=&Add;return p(p=0);}Run();",
            2L );
          ( "I64 Take(I64 a,I64 b){return a*10+b;}I64 (*p)(I64 a,I64 b);I64 \
             Run(){I64 n=0;p=&Take;return p(++n,++n);}Run();",
            21L );
          ( "I64 Sum(I64 n,...){return n+argc+argv[0]+argv[1];}I64 (*p)(I64 \
             n,...);I64 Run(){p=&Sum;return p(37,1,2);}Run();",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 Bad(I64 a,I64 b){return a+b;}I64 \
             (*p)(I64 n);I64 Run(){p=&Bad;p=&Add;return p(40);}Run();",
            42L );
          ("I64 (*p)(I64 n);I64 Run(){p=0;if(0)p(40);return 42;}Run();", 42L);
          ( "noargpop I64 Add(I64 n){return n+2;}noargpop I64 (*p)(I64 n);I64 \
             Run(){p=&Add;return p(40);}Run();",
            42L );
          ( "argpop I64 Add(){return 42;}argpop I64 (*p)();I64 \
             Run(){p=&Add;return p();}Run();",
            42L );
        ];
      List.iter
        (fun return_type ->
          ignore
            (Test_integer_globals.run ~mode
               ("class Box{I64 n;};I64 A(){return 1;}" ^ return_type
              ^ " (*p)()[2];I64 Run(){p[1]=&A;return p[1]==&A;}Run();")
            |> Test_integer_functions.expect 1L);
          ignore
            (Test_integer_globals.run ~mode
               ("class Box{I64 n;};" ^ return_type ^ " (*p)()[2][3];sizeof(p);")
            |> Test_integer_functions.expect 48L))
        [ "I64"; "F64"; "U0"; "Box" ])
    modes;
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit
       "I64 A(){return 1;}I64 (*p)();I64 Set(){p=&A;return 0;}I64 \
        Read(){return p();}Set();I64 A(){return 2;}Read()*100+A();"
    |> Test_integer_functions.expect 102L);
  ignore
    (Test_integer_output.run ~mode:Preprocessor.Jit
       "extern U0 Print(U8 *fmt,...);I64 D(){Print(\"default\");return 40;}I64 \
        Add(I64 n){return n+2;}I64 (*p)(I64 n=D())[2];I64 \
        Run(){p[1]=&Add;return p[1]();}Run()+Run();"
    |> Test_integer_output.expect ~value:(Some 84L) "default")

let global_callback_initializers_execute () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          ignore
            (Test_integer_globals.run ~mode source
            |> Test_integer_functions.expect 42L))
        [
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)=&Add;p(40);";
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 \
           n)[2][2]={{&Add,0},{0,&Add}};p[1][1](40);";
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)=&Add;I64 (*q)(I64 \
           n)=p;p=0;q(40);";
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 \
           n)[2]={&Add,p[0]};p[0]=0;p[1](40);";
          "I64 Add(I64 n=17){return n+2;}I64 (*p)(I64 n=40)=&Add;p();";
          "noargpop I64 Add(I64 n){return n+2;}noargpop I64 (*p)(I64 \
           n)=&Add;p(40);";
          "I64 Sum(I64 n,...){return n+argc+argv[0]+argv[1];}I64 (*p)(I64 \
           n,...)=&Sum;p(37,1,2);";
        ];
      List.iter
        (fun return_type ->
          ignore
            (Test_integer_globals.run ~mode
               (return_type
              ^ " (*p)()[2]={0xFFFFFFFFFFFFFFFF,0x8000000000000000};I64 \
                 Check(){if(p[0]==-1&&p[1]==0x8000000000000000)return \
                 42;return 0;}Check();")
            |> Test_integer_functions.expect 42L))
        [
          "I8";
          "U8";
          "I16";
          "U16";
          "I32";
          "U32";
          "I64";
          "U64";
          "F64";
          "U0";
          "I64 *";
          "I64 ****";
        ];
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);I64 Seed(I64 n){Print(\"A\");return \
            n+2;}I64 (*q)(I64 n)=&Seed;I64 (*p)()=q(15);I64 \
            Check(){if(p==17)return 42;return 0;}Check();"
        |> Test_integer_output.expect "A");
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);U0 Write(I64 n){Print(\"%d\",n);}U0 \
            (*p)(I64 n)=&Write;p(42);42;"
        |> Test_integer_output.expect "42"))
    modes;
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit
       "I64 Add(I64 n){return n+1;}I64 (*p)(I64 n)=&Add;I64 Add(I64 n){return \
        n+2;}p(41);"
    |> Test_integer_functions.expect 42L)

let global_callback_initializer_faults_and_limits () =
  List.iter
    (fun mode ->
      let prefix =
        "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"arg\");return 40;}"
      in
      List.iter
        (fun (body, code) ->
          ignore
            (Test_integer_output.run ~mode (prefix ^ body)
            |> Test_integer_output.fault ~output:"arg" code))
        [
          ("I64 (*p)(I64 n)=0;I64 N=p(Side());42;", "HCIRVM0024");
          ("I64 (*p)(I64 n)=17;I64 N=p(Side());42;", "HCIRVM0024");
          ( "I64 Bad(I64 a,I64 b){return a+b;}I64 (*p)(I64 n)=&Bad;I64 \
             N=p(Side());42;",
            "HCIRVM0014" );
        ];
      let source =
        "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2]={&Add,0};p[0](40);"
      in
      let steps =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 42L
        |> Ir_integer_interpreter.executed_steps
      in
      ignore
        (Test_integer_output.run ~mode ~max_steps:steps ~max_global_bytes:16
           ~max_frame_bytes:8 ~max_call_depth:1 source
        |> Test_integer_output.expect "");
      List.iter
        (fun (code, report) -> ignore (Test_integer_output.fault code report))
        [
          ( "HCIRVM0007",
            Test_integer_output.run ~mode ~max_steps:(steps - 1) source );
          ( "HCIRVM0016",
            Test_integer_output.run ~mode ~max_global_bytes:15 source );
          ("HCIRVM0011", Test_integer_output.run ~mode ~max_frame_bytes:7 source);
        ])
    modes

let global_callback_reached_faults () =
  let prefix =
    "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"arg\");return 40;}"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (body, code, output) ->
          ignore
            (Test_integer_output.run ~mode (prefix ^ body)
            |> Test_integer_output.fault ~output code))
        [
          ( "I64 (*p)(I64 n);I64 Run(){p=0;return \
             p(Side());}Print(\"before\");Run();",
            "HCIRVM0024",
            "beforearg" );
          ( "I64 (*p)(I64 n);I64 Run(){return \
             p(Side());}Print(\"before\");Run();",
            (if mode = Preprocessor.Jit then "HCIRVM0012" else "HCIRVM0024"),
            if mode = Preprocessor.Jit then "before" else "beforearg" );
          ( "I64 (*p)(I64 n)[2];I64 Run(){return \
             p[2](Side());}Print(\"before\");Run();",
            "HCIRVM0019",
            "before" );
          ( "I64 (*p)(I64 n)[2];I64 Run(){return \
             p[-1](Side());}Print(\"before\");Run();",
            "HCIRVM0019",
            "before" );
          ( "I64 (*p)(I64 n)[2];I64 Run(){return \
             p[0x7FFFFFFFFFFFFFFF](Side());}Print(\"before\");Run();",
            "HCIRVM0020",
            "before" );
          ( "I64 Add(I64 n){return n+2;}noargpop I64 (*p)(I64 n);I64 \
             Run(){p=&Add;return p(Side());}Print(\"before\");Run();",
            "HCIRVM0014",
            "beforearg" );
          ( "I64 Bad(I64 a,I64 b){return a+b;}I64 (*p)(I64 n);I64 \
             Run(){p=&Bad;return p(Side());}Print(\"before\");Run();",
            "HCIRVM0014",
            "beforearg" );
        ])
    modes

let global_callback_exact_budgets () =
  let source =
    "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2][3];I64 \
     Run(){p[1][2]=&Add;return p[1][2](40);}Run();"
  in
  List.iter
    (fun mode ->
      let steps =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 42L
        |> Ir_integer_interpreter.executed_steps
      in
      ignore
        (Test_integer_globals.run ~mode ~max_steps:steps ~max_global_bytes:48
           ~max_frame_bytes:8 ~max_call_depth:2 source
        |> Test_integer_functions.expect 42L);
      List.iter
        (fun (code, result) ->
          Alcotest.(check string)
            "one below global callback budget" code
            (Test_integer_functions.first_error result).code)
        [
          ( "HCIRVM0007",
            Test_integer_globals.run ~mode ~max_steps:(steps - 1) source );
          ( "HCIRVM0016",
            Test_integer_globals.run ~mode ~max_global_bytes:47 source );
          ( "HCIRVM0011",
            Test_integer_globals.run ~mode ~max_frame_bytes:7 source );
          ("HCIRVM0015", Test_integer_globals.run ~mode ~max_call_depth:1 source);
        ])
    modes

let global_callback_graph_ownership () =
  let module VM = Ir_integer_interpreter in
  let module C = Ir_runtime_call_context in
  let source =
    "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2];I64 Run(){p[1]=&Add;return \
     p[1](40);}Run();"
  in
  List.iter
    (fun mode ->
      let compiled = Test_integer_globals.compile ~mode source in
      let context = integer_program_runtime_calls compiled in
      let execute ?runtime_calls () =
        VM.execute_program
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~functions:(integer_program_functions compiled)
          ?runtime_calls ~max_steps:10000 ~max_frame_bytes:1024
          ~max_call_depth:16
          (integer_program_entry compiled)
      in
      let reject = function
        | Ok _ -> Alcotest.fail "foreign global callback graph was accepted"
        | Error errors ->
            Alcotest.(check bool)
              "failure precedes execution" true
              (List.for_all
                 (fun (error : VM.error) ->
                   error.stage = VM.Preflight && error.executed_steps = 0)
                 errors)
      in
      reject (execute ());
      let foreign = Test_integer_globals.compile ~mode source in
      reject (execute ~runtime_calls:(integer_program_runtime_calls foreign) ());
      let body =
        integer_program_functions compiled
        |> List.find (fun (definition : VM.function_definition) ->
            Semantic_symbol.name (Ir_function_body.symbol definition.body)
            = "Run")
        |> fun definition -> definition.body
      in
      let rec find = function
        | [] -> None
        | instruction :: rest as cell ->
            if
              (Ir_instruction_sequence.description instruction).opcode
              = Ir_opcode.Ic_call_indirect
            then Some cell
            else find rest
      in
      let cell =
        Ir_function_body.body body |> Ir_block_graph.blocks
        |> List.find_map (fun block ->
            Ir_block_graph.instructions block
            |> Ir_instruction_sequence.instructions |> find)
        |> Option.get
      in
      let original = Ir_instruction_sequence.description (List.hd cell) in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr
           { original with operands = List.map Fun.id original.operands });
      Alcotest.(check bool)
        "copied global graph loses original authority" true
        (Option.is_none
           (C.original_callback_calls context ~owner:(C.Function body)));
      reject (execute ~runtime_calls:context ()))
    modes

let top_level_callback_execution () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          ignore
            (Test_integer_globals.run ~mode source
            |> Test_integer_functions.expect expected))
        [
          ("I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;p(40);", 42L);
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 \
             n)[2][3];p[1][2]=&Add;p[1][2](40);",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n),(*q)(I64 \
             n);p=&Add;q=p;p=0;q(40);",
            42L );
          ("I64 Add(I64 n=17){return n;}I64 (*p)(I64 n=42);p=&Add;p();", 42L);
          ( "I64 Add(I64 n=17){return n;}I64 (*p)(I64 n=42)[2];p[1]=&Add;p[1]();",
            42L );
          ("I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;p(p=0);", 2L);
          ( "I64 Take(I64 a,I64 b){return a*10+b;}I64 (*p)(I64 a,I64 b);I64 \
             n;n=0;p=&Take;p(++n,++n);",
            21L );
          ( "I64 Sum(I64 n,...){return n+argc+argv[0]+argv[1];}I64 (*p)(I64 \
             n,...);p=&Sum;p(37,1,2);",
            42L );
          ( "I64 Sum(I64 n,...){return n+argc;}I64 (*p)(I64 n,...);p=&Sum;p(42);",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 Take(I64 n){return n;}I64 (*p)(I64 \
             n);p=&Add;Take(p(40));",
            42L );
          ("I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;p(p(38));", 42L);
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 \
             n);p=&Add;switch(p(40)){case 42:42;break;default:1;}",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 \
             n);p=&Add;if(p(40)==42)42;else 1;",
            42L );
          ( "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);I64 \
             n;n=0;p=&Add;while(n<4)n=p(n);n+38;",
            42L );
          ( "I64 (*p)(I64 n);I64 Walk(I64 n){if(n)return p(n-1)+1;return \
             0;}p=&Walk;p(8);",
            8L );
          ( "noargpop I64 Add(I64 n){return n+2;}noargpop I64 (*p)(I64 \
             n);p=&Add;p(40);",
            42L );
          ("argpop I64 Add(){return 42;}argpop I64 (*p)();p=&Add;p();", 42L);
          ("I64 (*p)(I64 n);p=0;if(0)p(40);42;", 42L);
        ];
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);U0 Write(I64 n){Print(\"%d\",n);}U0 \
            (*p)(I64 n)[2];p[1]=&Write;p[1](42);42;"
        |> Test_integer_output.expect ~value:(Some 42L) "42");
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);U0 Write(I64 n){Print(\"%d\",n);}U0 \
            (*p)(I64 n);p=&Write;p(42);"
        |> Test_integer_output.expect ~value:None "42");
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);I64 Add(I64 n){return n+2;}I64 \
            (*p)(I64 n);p=&Add;Print(\"%d\",p(40));42;"
        |> Test_integer_output.expect ~value:(Some 42L) "42");
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);I64 Add(I64 n){return n+2;}I64 \
            (*p)(I64 n);p=&Add;\"%d\",p(40);42;"
        |> Test_integer_output.expect ~value:(Some 42L) "42"))
    modes;
  ignore
    (Test_integer_globals.run ~mode:Preprocessor.Jit
       "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n=40);p=&Add;p();I64 Add(I64 \
        n){return n+100;}p();"
    |> Test_integer_functions.expect 42L)

let top_level_callback_reached_faults () =
  let prefix =
    "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"arg\");return 40;}"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code, output) ->
          ignore
            (Test_integer_output.run ~mode (prefix ^ source)
            |> Test_integer_output.fault ~output code))
        [
          ( "I64 (*p)(I64 n);p=0;Print(\"before\");p(Side());",
            "HCIRVM0024",
            "beforearg" );
          ( "I64 (*p)(I64 n);p=123;Print(\"before\");p(Side());",
            "HCIRVM0024",
            "beforearg" );
          ( "I64 (*p)(I64 n);Print(\"before\");p(Side());",
            (if mode = Preprocessor.Jit then "HCIRVM0012" else "HCIRVM0024"),
            if mode = Preprocessor.Jit then "before" else "beforearg" );
          ( "I64 (*p)(I64 n)[2];Print(\"before\");p[2](Side());",
            "HCIRVM0019",
            "before" );
          ( "I64 (*p)(I64 n)[2];Print(\"before\");p[-1](Side());",
            "HCIRVM0019",
            "before" );
          ( "I64 (*p)(I64 n)[2];Print(\"before\");p[0x7FFFFFFFFFFFFFFF](Side());",
            "HCIRVM0020",
            "before" );
          ( "I64 Add(I64 n){return n+2;}noargpop I64 (*p)(I64 \
             n);p=&Add;Print(\"before\");p(Side());",
            "HCIRVM0014",
            "beforearg" );
          ( "I64 Bad(I64 a,I64 b){return a+b;}I64 (*p)(I64 \
             n);p=&Bad;Print(\"before\");p(Side());",
            "HCIRVM0014",
            "beforearg" );
        ])
    modes

let top_level_callback_exact_budgets () =
  let source =
    "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2][3];p[1][2]=&Add;p[1][2](40);"
  in
  List.iter
    (fun mode ->
      let steps =
        Test_integer_globals.run ~mode source
        |> Test_integer_functions.expect 42L
        |> Ir_integer_interpreter.executed_steps
      in
      ignore
        (Test_integer_globals.run ~mode ~max_steps:steps ~max_global_bytes:48
           ~max_frame_bytes:8 ~max_call_depth:1 source
        |> Test_integer_functions.expect 42L);
      List.iter
        (fun (code, result) ->
          Alcotest.(check string)
            "one below entry callback budget" code
            (Test_integer_functions.first_error result).code)
        [
          ( "HCIRVM0007",
            Test_integer_globals.run ~mode ~max_steps:(steps - 1) source );
          ( "HCIRVM0016",
            Test_integer_globals.run ~mode ~max_global_bytes:47 source );
          ( "HCIRVM0011",
            Test_integer_globals.run ~mode ~max_frame_bytes:7 source );
          ( "HCIRVM0015",
            Test_integer_globals.run ~mode ~max_call_depth:1
              "I64 (*p)(I64 n);I64 Walk(I64 n){if(n)return p(n-1);return \
               42;}p=&Walk;p(1);" );
        ])
    modes

let top_level_callback_graph_ownership () =
  let module VM = Ir_integer_interpreter in
  let module C = Ir_runtime_call_context in
  let module Seq = Ir_instruction_sequence in
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let compiled = Test_integer_globals.compile ~mode source in
          let context = integer_program_runtime_calls compiled in
          let callback =
            C.original_callback_calls context ~owner:C.Entry
            |> Option.get |> List.hd
          in
          Alcotest.(check bool)
            "entry load has its original receipt" true
            (Option.fold ~none:false ~some:(( == ) callback)
               (C.find_callback_load context ~owner:C.Entry
                  callback.callback_load));
          Alcotest.(check bool)
            "copied entry load has no receipt" true
            (Option.is_none
               (C.find_callback_load context ~owner:C.Entry
                  {
                    callback.callback_load with
                    Seq.operands =
                      List.map Fun.id callback.callback_load.operands;
                  }));
          let execute ?runtime_calls () =
            VM.execute_program
              ~globals:(integer_program_globals compiled)
              ~initialization:(integer_program_initialization compiled)
              ~functions:(integer_program_functions compiled)
              ?runtime_calls ~max_steps:10000 ~max_frame_bytes:1024
              ~max_call_depth:16
              (integer_program_entry compiled)
          in
          let reject = function
            | Ok _ -> Alcotest.fail "foreign entry callback graph was accepted"
            | Error errors ->
                Alcotest.(check bool)
                  "rejected before effects" true
                  (List.for_all
                     (fun (error : VM.error) ->
                       error.stage = VM.Preflight && error.executed_steps = 0)
                     errors)
          in
          reject (execute ());
          let foreign = Test_integer_globals.compile ~mode source in
          reject
            (execute ~runtime_calls:(integer_program_runtime_calls foreign) ());
          let graph = integer_program_entry compiled |> Ir_x87_stack.graph in
          let rec find = function
            | [] -> None
            | instruction :: rest as cell ->
                if
                  (Seq.description instruction).opcode
                  = Ir_opcode.Ic_call_indirect
                then Some cell
                else find rest
          in
          let cell =
            Ir_block_graph.blocks graph
            |> List.find_map (fun block ->
                Ir_block_graph.instructions block |> Seq.instructions |> find)
            |> Option.get
          in
          let original = Seq.description (List.hd cell) in
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr
               {
                 original with
                 Seq.operands = List.map Fun.id original.operands;
               });
          Alcotest.(check bool)
            "copied entry graph loses its complete receipt" true
            (Option.is_none (C.original_callback_calls context ~owner:C.Entry));
          reject (execute ~runtime_calls:context ()))
        [
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n);p=&Add;p(40);";
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2];p[1]=&Add;p[1](40);";
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)=&Add;I64 N=p(40);N;";
          "I64 Add(I64 n){return n+2;}I64 (*p)(I64 n)[2]={&Add,0};I64 \
           N=p[0](40);N;";
        ])
    modes

let ordinary_callback_flags_preserve_selected_cleanup () =
  List.iter
    (fun mode ->
      List.iter
        (fun flags ->
          let text =
            Printf.sprintf
              "%s I64 Add(I64 n){return n+2;}%s I64 (*p)(I64 n);p=&Add;p(40);"
              flags flags
          in
          ignore
            (Test_integer_globals.run ~mode text
            |> Test_integer_functions.expect 42L);
          ignore
            (Test_integer_globals.run ~mode
               (Printf.sprintf
                  "I64 Add(I64 n){return n+2;}I64 Caller(){%s I64 (*p)(I64 \
                   n);p=&Add;return p(40);}Caller();"
                  flags)
            |> Test_integer_functions.expect 42L))
        [
          "argpop noargpop";
          "noargpop argpop";
          "haserrcode";
          "haserrcode argpop noargpop";
        ];
      ignore
        (Test_integer_output.run ~mode
           "extern U0 Print(U8 *fmt,...);I64 Side(){Print(\"arg\");return \
            40;}argpop noargpop I64 Add(I64 n){return n+2;}I64 (*p)(I64 \
            n);p=&Add;Print(\"before\");p(Side());"
        |> Test_integer_output.fault ~output:"beforearg" "HCIRVM0014"))
    modes

let tests =
  [
    Alcotest.test_case "global callback initializers retain words and code"
      `Quick global_callback_initializers_execute;
    Alcotest.test_case
      "global callback initializer faults retain effects and limits" `Quick
      global_callback_initializer_faults_and_limits;
    Alcotest.test_case "ordinary callback flags retain selected cleanup" `Quick
      ordinary_callback_flags_preserve_selected_cleanup;
    Alcotest.test_case
      "top-level callbacks execute original scalar and indexed calls" `Quick
      top_level_callback_execution;
    Alcotest.test_case "top-level callback faults preserve reached effects"
      `Quick top_level_callback_reached_faults;
    Alcotest.test_case "top-level callbacks charge exact execution and storage"
      `Quick top_level_callback_exact_budgets;
    Alcotest.test_case "top-level callbacks require their original entry graphs"
      `Quick top_level_callback_graph_ownership;
    Alcotest.test_case
      "global callbacks retain storage, flags and original code" `Quick
      global_callback_storage_execution;
    Alcotest.test_case "global callback faults preserve reached effects" `Quick
      global_callback_reached_faults;
    Alcotest.test_case "global callbacks charge exact persistent storage" `Quick
      global_callback_exact_budgets;
    Alcotest.test_case "global callbacks require original whole graphs" `Quick
      global_callback_graph_ownership;
    Alcotest.test_case "callback arrays invoke original stored code" `Quick
      callback_array_storage_execution;
    Alcotest.test_case
      "callback array faults preserve index and argument effects" `Quick
      callback_array_reached_faults;
    Alcotest.test_case "callback array addresses keep their owned extent" `Quick
      callback_array_bounds;
    Alcotest.test_case
      "callback arrays retain static cells and original JIT bodies" `Quick
      callback_array_retained_storage;
    Alcotest.test_case "callback array defaults execute once at declaration"
      `Quick callback_array_defaults_evaluate_once;
    Alcotest.test_case "callback arrays retain unsupported shape boundaries"
      `Quick callback_array_unsupported_shapes;
    Alcotest.test_case "callback arrays charge exact storage and execution work"
      `Quick callback_array_exact_budgets;
    Alcotest.test_case "callback array graphs require original source owners"
      `Quick callback_array_graph_ownership;
    Alcotest.test_case "static callback cells retain stored executable owners"
      `Quick static_callback_storage_execution;
    Alcotest.test_case "static callback faults follow original argument effects"
      `Quick static_callback_reached_faults;
    Alcotest.test_case "static callback graphs require original source owners"
      `Quick static_callback_graph_ownership;
    Alcotest.test_case "static callback limits and unsupported preparation"
      `Quick static_callback_limits_and_boundaries;
    Alcotest.test_case
      "class callback position defaults prepare original lexical writes" `Quick
      anonymous_position_defaults_execute;
    Alcotest.test_case "position defaults execute once at declaration" `Quick
      anonymous_position_defaults_evaluate_once;
    Alcotest.test_case "position defaults preserve reached faults and output"
      `Quick anonymous_position_defaults_preserve_reached_faults;
    Alcotest.test_case
      "position defaults charge actual preparation and runtime work" `Quick
      anonymous_position_defaults_charge_exact_work;
    Alcotest.test_case
      "ordinary default positions retain instruction-address semantics" `Quick
      ordinary_position_defaults_retain_instruction_addresses;
    Alcotest.test_case
      "anonymous signature defaults materialize their own values" `Quick
      anonymous_signature_defaults_execute;
    Alcotest.test_case "anonymous defaults execute once during declaration"
      `Quick anonymous_defaults_preserve_declaration_effects;
    Alcotest.test_case "anonymous default calls preserve reached effects" `Quick
      anonymous_default_words_retain_reached_faults;
    Alcotest.test_case
      "anonymous defaults preserve limits and explicit boundaries" `Quick
      anonymous_default_limits_and_boundaries;
    Alcotest.test_case
      "anonymous defaults require exact original producer ownership" `Quick
      anonymous_defaults_require_original_producers;
    Alcotest.test_case
      "callback parameter defaults materialize original word storage" `Quick
      callback_parameter_defaults_execute;
    Alcotest.test_case "callback defaults evaluate once at their declaration"
      `Quick callback_defaults_preserve_declaration_effects;
    Alcotest.test_case
      "callback defaults retain their original header and budgets" `Quick
      callback_defaults_keep_original_header_and_limits;
    Alcotest.test_case "numeric callback defaults grant no executable authority"
      `Quick callback_default_words_have_no_executable_authority;
    Alcotest.test_case "callback defaults require physical original producers"
      `Quick callback_default_producers_require_original_ownership;
    Alcotest.test_case "callback target faults retain argument effects" `Quick
      callback_reached_faults_and_effects;
    Alcotest.test_case "callback invocation preserves exact runtime limits"
      `Quick callback_exact_budgets;
    Alcotest.test_case "callback invocation requires original graph ownership"
      `Quick callback_graph_ownership;
    Alcotest.test_case
      "checked local and parameter callbacks invoke owned bodies" `Quick
      checked_callback_invocation;
    Alcotest.test_case
      "local, parameter, static, global and member callback cells" `Quick
      local_parameter_static_global_and_member_storage;
    Alcotest.test_case "callback storage updates and conversions" `Quick
      storage_updates_and_conversions;
    Alcotest.test_case "return types and callback indirection are separate"
      `Quick return_types_do_not_change_cell_storage;
    Alcotest.test_case "partial arrays and function addresses are not cells"
      `Quick arrays_require_complete_storage_selection;
    Alcotest.test_case "top-level callback storage retains its signature" `Quick
      top_level_storage_and_exact_signature;
    Alcotest.test_case "callback frame addresses retain declaration identity"
      `Quick callback_frame_addresses_use_the_original_declarator;
    Alcotest.test_case "original callback values precede argument typing" `Quick
      checked_callees_retain_original_values_before_arguments;
    Alcotest.test_case "top-level scalar callback retains its callee" `Quick
      top_level_scalar_callback_retains_original_callee;
    Alcotest.test_case "callback loads use physical words" `Quick
      callback_loads_use_physical_words;
    Alcotest.test_case "callback parameter defaults use integer storage" `Quick
      callback_parameter_default_is_an_integer_address;
    Alcotest.test_case "callee snapshot follows original PrsFunCall" `Quick
      callback_callee_snapshot_matches_prs_fun_call;
    Alcotest.test_case "a scalar call rejects another callee value" `Quick
      scalar_call_rejects_another_callee_value;
    Alcotest.test_case "owned function addresses execute and retain identity"
      `Quick owned_function_address_execution;
    Alcotest.test_case "scalar callback cells store owned executable values"
      `Quick scalar_callback_storage_execution;
    Alcotest.test_case "function address receipts retain original owners" `Quick
      function_address_receipts;
    Alcotest.test_case
      "function address graphs reject copied and missing authority" `Quick
      function_address_graph_ownership;
    Alcotest.test_case
      "function address storage preserves limits and reached effects" `Quick
      function_address_limits_and_reached_faults;
    Alcotest.test_case "replaced function address keeps its original body"
      `Quick replaced_function_address_keeps_original_body;
    Alcotest.test_case "AOT definition snapshots keep original bodies" `Quick
      aot_definition_snapshots_keep_original_bodies;
  ]
