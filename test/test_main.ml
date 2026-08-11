let () =
  Alcotest.run "holyc"
    [
      ("source", Test_source.tests);
      ("lexer", Test_lexer.tests);
      ("diagnostic", Test_diagnostic.tests);
      ("version", Test_version.tests);
      ("opcode table source", Test_opcode_table_source.tests);
      ("primitive type source", Test_primitive_type_source.tests);
      ("primitive type", Test_primitive_type.tests);
      ("operator table source", Test_operator_table_source.tests);
      ("operator table", Test_operator_table.tests);
      ("compiler option source", Test_compiler_option_source.tests);
      ("compiler option", Test_compiler_option.tests);
      ("intermediate code source", Test_intermediate_code_source.tests);
      ("intermediate code", Test_intermediate_code.tests);
    ]
