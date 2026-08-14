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
      ( "semantic aggregate header resolution",
        Test_aggregate_header_resolution.tests );
      ( "semantic aggregate member type resolution",
        Test_member_type_resolution.tests );
      ("semantic aggregate layout", Test_aggregate_layout.tests);
      ("semantic aggregate member index", Test_aggregate_member_index.tests);
      ("semantic aggregate layout dump", Test_aggregate_layout_dump.tests);
      ("semantic function type resolution", Test_function_type_resolution.tests);
      ("semantic global type resolution", Test_global_type_resolution.tests);
      ("semantic local type resolution", Test_local_type_resolution.tests);
      ("semantic function binding index", Test_function_binding_index.tests);
      ( "semantic function expression binding",
        Test_function_expression_binding.tests );
      ( "semantic local warning analysis",
        Test_local_warning_analysis.tests );
      ( "semantic module expression binding",
        Test_module_expression_binding.tests );
      ("semantic outer expression binding", Test_outer_expression_binding.tests);
      ( "semantic global initializer binding",
        Test_global_initializer_binding.tests );
      ("semantic global dimension binding", Test_global_dimension_binding.tests);
      ("semantic function default binding", Test_function_default_binding.tests);
      ("semantic function identity resolution", Test_function_resolution.tests);
      ( "semantic function record classification",
        Test_function_record_classification.tests );
      ("semantic global record resolution", Test_global_resolution.tests);
      ( "semantic global record classification",
        Test_global_record_classification.tests );
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
      ("global record flag source", Test_global_record_flag_source.tests);
      ("global record flag", Test_global_record_flag.tests);
      ("member-list flag source", Test_member_flag_source.tests);
      ("member-list flag", Test_member_flag.tests);
      ("BIN record source", Test_bin_record_source.tests);
      ("BIN record", Test_bin_record.tests);
    ]
