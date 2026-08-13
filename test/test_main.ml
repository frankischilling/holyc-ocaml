let () =
  Alcotest.run "holyc"
    [
      ("source", Test_source.tests);
      ("lexer", Test_lexer.tests);
      ("preprocessor", Test_preprocessor.tests);
      ("parser", Test_parser.tests);
      ("conditional expressions", Test_conditional_expression.tests);
      ("assert directives", Test_assert_directive.tests);
      ("help directives", Test_help_directive.tests);
      ("predefined values", Test_predefined.tests);
      ("symbol visibility", Test_symbol_visibility.tests);
      ("semantic symbol table", Test_semantic_symbol_table.tests);
      ("semantic declaration collection", Test_declaration_collection.tests);
      ("semantic member collection", Test_member_collection.tests);
      ("semantic function collection", Test_function_collection.tests);
      ("semantic label resolution", Test_label_resolution.tests);
      ("semantic aggregate resolution", Test_aggregate_resolution.tests);
      ("diagnostic", Test_diagnostic.tests);
      ("version", Test_version.tests);
      ("corpus", Test_corpus.tests);
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
