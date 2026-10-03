open Holyc_lib
module Helpers = Test_integer_goto_execution

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let parse mode text =
  let _, _, result = Test_parser.parse_string ~compilation_mode:mode text in
  Test_parser.expect_ast result

let functions () =
  List.iter
    (fun mode ->
      List.iter
        (fun primitive ->
          let name = Primitive_type.to_string primitive in
          let text = Printf.sprintf "I64 %s(){return 42;}%s();" name name in
          let ast = parse mode text in
          (match ast.items with
          | [
           Ast.Function_definition definition; Ast.Top_level_statement statement;
          ] -> (
              Alcotest.(check string)
                "original function name" name definition.name.spelling;
              let expression =
                (Test_parser.expect_expression_statement statement)
                  .expression_statement_expression
              in
              match expression with
              | Ast.Call_expression call ->
                  Alcotest.(check string)
                    "call uses the shadow" name
                    (Test_parser.expect_identifier_expression call.call_callee)
                      .spelling
              | _ ->
                  Alcotest.fail "primitive-named function became a declaration")
          | _ -> Alcotest.fail "unexpected primitive-named function source");
          let _, _, result =
            Helpers.run ~mode ~path:"primitive-shadow.hc" text
          in
          ignore (Helpers.expect_word name 42L result))
        Primitive_type.all)
    modes

let storage () =
  List.iter
    (fun mode ->
      List.iter
        (fun primitive ->
          let name = Primitive_type.to_string primitive in
          List.iter
            (fun text ->
              ignore (parse mode text);
              let _, _, result =
                Helpers.run ~mode ~path:"primitive-shadow.hc" text
              in
              ignore (Helpers.expect_word text 42L result))
            [
              Printf.sprintf "I64 %s=41;%s++;(%s);" name name name;
              Printf.sprintf "I64 F(){I64 %s=41;%s++;return (%s);}F();" name
                name name;
              Printf.sprintf "I64 F(I64 %s){%s++;return (%s);}F(41);" name name
                name;
              Printf.sprintf
                "#define VALUE %s\n\
                 I64 F(){I64 %s=41;VALUE++;return (VALUE);}F();"
                name name;
            ])
        Primitive_type.all)
    modes

let restored_types () =
  List.iter
    (fun mode ->
      let text = "I64 F(){I64 U64=41;{U64++;}return (U64);};U64 value=1;F();" in
      let _, _, result = Helpers.run ~mode ~path:"primitive-restore.hc" text in
      ignore
        (Helpers.expect_word "local shadow ends at function exit" 42L result);
      let ast = parse mode "I64 F(I64 F64){return (F64);};F64 value;" in
      (match List.rev ast.items with
      | Ast.Global_variable variable :: _ ->
          Alcotest.(check bool)
            "F64 type is restored after parameter scope" true
            (Primitive_type.equal Primitive_type.F64
               (Test_parser.expect_primitive_specifier variable.type_specifier)
                 .primitive)
      | _ -> Alcotest.fail "restored F64 did not form a variable");
      let ast = parse mode "class U64{I64 field;};U64 value;" in
      match List.rev ast.items with
      | Ast.Global_variable variable :: _ ->
          ignore
            (Test_parser.expect_named_specifier "U64" variable.type_specifier)
      | _ -> Alcotest.fail "new aggregate did not replace the public type")
    modes

let unshadowed_types () =
  List.iter
    (fun mode ->
      List.iter
        (fun primitive ->
          let name = Primitive_type.to_string primitive in
          let ast = parse mode (Printf.sprintf "%s value;42(%s);" name name) in
          match ast.items with
          | [ Ast.Global_variable variable; Ast.Top_level_statement statement ]
            -> (
              Alcotest.(check bool)
                "original declared primitive" true
                (Primitive_type.equal primitive
                   (Test_parser.expect_primitive_specifier
                      variable.type_specifier)
                     .primitive);
              match
                (Test_parser.expect_expression_statement statement)
                  .expression_statement_expression
              with
              | Ast.Postfix_cast_expression cast ->
                  Alcotest.(check bool)
                    "original postfix primitive" true
                    (Primitive_type.equal primitive
                       (Test_parser.expect_primitive_specifier cast.cast_type)
                         .primitive)
              | _ -> Alcotest.fail "unshadowed primitive lost its cast")
          | _ -> Alcotest.fail "unshadowed primitive lost its declaration")
        Primitive_type.all)
    modes

let rejected_type_uses () =
  List.iter
    (fun mode ->
      let _, _, output =
        Test_parser.parse_string ~compilation_mode:mode "I64 U64=42;U64 value;"
      in
      Alcotest.(check bool)
        "selected global cannot form a type" true
        (Option.is_none output.ast);
      Alcotest.(check string)
        "global shadow diagnostic" "HCPARSE0047"
        (Test_parser.first_diagnostic output).code;
      List.iter
        (fun text ->
          (* Callback-free parsing has no live post-body resume observation.
             The source executor retains the original selected local token. *)
          ignore (parse mode text);
          let _, _, result =
            Helpers.run ~mode ~path:"primitive-lookahead.hc" text
          in
          let error = Helpers.first_error result in
          Alcotest.(check string)
            "original local selection survives teardown" "HCPARSE0001"
            error.code)
        [
          "I64 F(){I64 U64=42;return (U64);}U64 value;F();";
          "I64 F(I64 F64){return (F64);}F64 value;";
        ])
    modes

let tests =
  [
    Alcotest.test_case "primitive-named functions and original call syntax"
      `Quick functions;
    Alcotest.test_case "globals, locals, parameters and replacement input"
      `Quick storage;
    Alcotest.test_case "restored types and newer aggregate identity" `Quick
      restored_types;
    Alcotest.test_case "unshadowed declarations and postfix casts" `Quick
      unshadowed_types;
    Alcotest.test_case "shadowed type uses and original post-body selection"
      `Quick rejected_type_uses;
  ]
