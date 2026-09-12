open Holyc_lib
module D = Task_declarations
module N = Semantic_function_record_phase
module T = Test_task_declarations

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let observed_admission_alias () =
  List.iter
    (fun (source, same_identity) ->
      let session, runtime, ledger = T.runtime_setup () in
      let output, events = T.parse session ledger source in
      let ast = Test_parser.expect_ast output in
      let original =
        List.find_map
          (function
            | Parser.Function_declared p -> Some p
            | _ -> None)
          events
        |> Option.get
      in
      let completed =
        List.find_map
          (function
            | Parser.Function_header_completed h -> Some h
            | _ -> None)
          events
        |> Option.get
      in
      let before = D.function_record_snapshot ledger original |> expect in
      let declaration_command = D.seal ledger ast |> expect in
      let program =
        (compile_integer_task_ast ~task:runtime ~declaration_command session
           ~config:(T.config ()) ast
        |> expect)
          .value
      in
      T.execute_runtime_ok runtime program;
      D.observe_admission ledger (T.admission runtime program |> Option.get)
      |> checked;
      let entry = T.visible session "F" in
      Alcotest.(check bool)
        "runtime publication retains exact completed frontend entry" true
        (Option.fold ~none:false
           ~some:(( == ) completed.Parser.completed_entry)
           (Symbol_visibility.function_alias_original entry));
      let output, events = T.parse session ledger "I64 F(I64 n);" in
      ignore (Test_parser.expect_ast output);
      let next =
        List.find_map
          (function
            | Parser.Function_declared p -> Some p
            | _ -> None)
          events
        |> Option.get
      in
      let after = D.function_record_snapshot ledger next |> expect in
      Alcotest.(check bool)
        "runtime alias preserves native allocation decision" same_identity
        (N.same_identity before after);
      Alcotest.(check (option int))
        "completed replacement count remains known" (Some 1)
        (N.argument_count after))
    [ ("extern I64 F();", true); ("I64 F(){return 42;}", false) ]

let tests =
  [
    Alcotest.test_case "observed runtime publication retains native aliases"
      `Quick observed_admission_alias;
  ]
