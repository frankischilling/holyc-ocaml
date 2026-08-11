// Assertion behavior follows TempleOS Compiler/Lex.HC at
// c26482bb6ad3f80106d28504ec5db3c6a360732c.

#assert 2*3 == 6
"assertion passed\n";

#assert 0
"preprocessing continues after the warning\n";
