/* Mode selection follows Compiler/Lex.HC at TempleOS commit
   c26482bb6ad3f80106d28504ec5db3c6a360732c. */
#ifjit
#define SELECTED_MODE "jit"
#else
#define SELECTED_MODE "aot"
#endif

SELECTED_MODE
