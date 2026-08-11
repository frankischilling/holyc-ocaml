// Reference: TempleOS c26482b, Compiler/Lex.HC KW_IFDEF and KW_IFNDEF.
#define FEATURE_FLAG enabled

#ifdef FEATURE_FLAG
"definition branch\n";
#else
"unreachable definition branch\n";
#endif

#ifdef I64i
"internal type branch\n";
#endif

#ifndef MISSING_SYMBOL
"missing symbol branch\n";
#endif
