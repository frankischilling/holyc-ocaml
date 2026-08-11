let () =
  Alcotest.run "holyc"
    [
      ("source", Test_source.tests);
      ("lexer", Test_lexer.tests);
      ("preprocessor", Test_preprocessor.tests);
      ("conditional expressions", Test_conditional_expression.tests);
      ("assert directives", Test_assert_directive.tests);
      ("help directives", Test_help_directive.tests);
      ("symbol visibility", Test_symbol_visibility.tests);
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
      ("function flag source", Test_function_flag_source.tests);
      ("function flag", Test_function_flag.tests);
      ("BIN record source", Test_bin_record_source.tests);
      ("BIN record", Test_bin_record.tests);
    ]
