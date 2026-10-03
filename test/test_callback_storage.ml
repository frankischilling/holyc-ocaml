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
        == S.callable_pointer (R.top_level_global_callback_callable call)))
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

let tests =
  [
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
  ]
