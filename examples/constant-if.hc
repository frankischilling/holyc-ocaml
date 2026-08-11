// Conditional expression behavior follows TempleOS Compiler/Lex.HC and
// Compiler/PrsExp.HC at c26482bb6ad3f80106d28504ec5db3c6a360732c.

#define BUILD_LEVEL 3

#if defined(I64i) && BUILD_LEVEL`2 == 9
"constant condition selected\n";
#else
"unexpected branch\n";
#endif
