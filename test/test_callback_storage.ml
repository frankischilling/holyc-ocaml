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
  ]
