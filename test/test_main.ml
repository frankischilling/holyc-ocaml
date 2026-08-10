let () =
  Alcotest.run "holyc"
    [
      ("source", Test_source.tests);
      ("lexer", Test_lexer.tests);
      ("diagnostic", Test_diagnostic.tests);
    ]
