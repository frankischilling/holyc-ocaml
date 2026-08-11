let () =
  Alcotest.run "holyc"
    [
      ("source", Test_source.tests);
      ("lexer", Test_lexer.tests);
      ("diagnostic", Test_diagnostic.tests);
      ("version", Test_version.tests);
      ("opcode table source", Test_opcode_table_source.tests);
    ]
