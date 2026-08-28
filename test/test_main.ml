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
      ("semantic local warning analysis", Test_local_warning_analysis.tests);
      ( "semantic module expression binding",
        Test_module_expression_binding.tests );
      ( "semantic top-level expression binding",
        Test_top_level_expression_binding.tests );
      ( "semantic top-level outer expression binding",
        Test_top_level_outer_expression_binding.tests );
      ( "semantic top-level expression tree",
        Test_top_level_expression_tree.tests );
      ( "semantic top-level statement validation",
        Test_top_level_statement_validation.tests );
      ( "semantic top-level identifier resolution",
        Test_top_level_identifier_resolution.tests );
      ( "semantic top-level expression results",
        Test_top_level_expression_result.tests );
      ( "semantic top-level condition results",
        Test_top_level_condition_result.tests );
      ( "semantic top-level switch selector results",
        Test_top_level_switch_selector_result.tests );
      ( "semantic top-level switch case results",
        Test_top_level_switch_case_result.tests );
      ("semantic outer expression binding", Test_outer_expression_binding.tests);
      ( "semantic global initializer binding",
        Test_global_initializer_binding.tests );
      ("semantic global dimension binding", Test_global_dimension_binding.tests);
      ("semantic function default binding", Test_function_default_binding.tests);
      ("semantic function identity resolution", Test_function_resolution.tests);
      ("semantic function header analysis", Test_function_header_analysis.tests);
      ("semantic function call resolution", Test_function_call_resolution.tests);
      ( "semantic function call conversion policy",
        Test_function_call_conversion_policy.tests );
      ( "semantic function call expression results",
        Test_function_call_expression_result.tests );
      ( "semantic implicit output target resolution",
        Test_implicit_output_target_resolution.tests );
      ( "semantic top-level implicit output target resolution",
        Test_top_level_implicit_output_target_resolution.tests );
      ( "semantic top-level implicit output argument binding",
        Test_top_level_implicit_output_argument_binding.tests );
      ( "semantic implicit output argument binding",
        Test_implicit_output_argument_binding.tests );
      ( "semantic function call conversion decision",
        Test_function_call_conversion_decision.tests );
      ("semantic extern-to-import rewriting", Test_externs_to_imports.tests);
      ("semantic global data-heap policy", Test_globals_on_data_heap.tests);
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
      ("IR instruction sequence", Test_ir_instruction_sequence.tests);
      ("IR control flow", Test_ir_control_flow.tests);
      ("IR block graph", Test_ir_block_graph.tests);
      ("IR effects", Test_ir_effects.tests);
      ("IR x87 stack", Test_ir_x87_stack.tests);
      ("IR function body", Test_ir_function_body.tests);
      ("IR top-level body", Test_ir_top_level_body.tests);
      ("IR literal lowering", Test_ir_literal_lowering.tests);
      ("IR expression lowering", Test_ir_expression_lowering.tests);
      ( "IR expression statement lowering",
        Test_ir_expression_statement_lowering.tests );
      ("IR condition lowering", Test_ir_condition_lowering.tests);
      ("function flag source", Test_function_flag_source.tests);
      ("function flag", Test_function_flag.tests);
      ("global record flag source", Test_global_record_flag_source.tests);
      ("global record flag", Test_global_record_flag.tests);
      ("member-list flag source", Test_member_flag_source.tests);
      ("member-list flag", Test_member_flag.tests);
      ("BIN record source", Test_bin_record_source.tests);
      ("BIN record", Test_bin_record.tests);
    ]
