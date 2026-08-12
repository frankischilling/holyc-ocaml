# HolyC language notes

Every compatibility statement on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Primitive types

The current semantic API recognizes these 12 spellings:

| HolyC spelling | Raw code | Raw ID | Bytes | Signedness | Declaration form |
| --- | --- | ---: | ---: | --- | --- |
| `I0` | `RT_I0` | 2 | 0 | Signed | Internal type |
| `I8` | `RT_I8` | 4 | 1 | Signed | Internal type |
| `I16` | `RT_I16` | 6 | 2 | Signed | Public union over `I16i` |
| `I32` | `RT_I32` | 8 | 4 | Signed | Public union over `I32i` |
| `I64` | `RT_I64` | 10 | 8 | Signed | Public union over `I64i` |
| `U0` | `RT_U0` | 3 | 0 | Unsigned | Internal type |
| `U8` | `RT_U8` | 5 | 1 | Unsigned | Internal type |
| `U16` | `RT_U16` | 7 | 2 | Unsigned | Public union over `U16i` |
| `U32` | `RT_U32` | 9 | 4 | Unsigned | Public union over `U32i` |
| `U64` | `RT_U64` | 11 | 8 | Unsigned | Public union over `U64i` |
| `F64` | `RT_F64` | 14 | 8 | Not applicable | Internal type |
| `Bool` | `RT_I8` | 4 | 1 | Not applicable | Internal type using `I8i` storage |

`I0` and `U0` are distinct zero-sized types. `Doc/HolyC.DD` describes `U0` as void-like while stressing that its size is zero. The semantic model retains both types rather than folding either into an OCaml unit value.

`Bool` is also distinct. It shares the `RT_I8` code and one-byte storage with `I8`, but lookup returns a separate `Bool` constructor. Later conversion rules must make any relationship explicit.

## Internal names and public integer unions

`Compiler/CInit.HC` registers names ending in `i` as internal storage types. `I0`, `U0`, `I8`, `U8`, `F64`, and `Bool` also appear directly in that table. The larger public integer names do not. They are unions in `Kernel/KernelA.HH` whose leading type selects `I16i`, `U16i`, `I32i`, `U32i`, `I64i`, or `U64i` storage.

Those unions expose subinteger members such as bytes and words. The current API records which primitive comes from a public union and preserves the storage spelling and declaration line. Member offsets and subinteger access semantics belong to the later aggregate-layout work and are not implemented yet.

Internal spellings such as `I32i` are not accepted by `Primitive_type.of_spelling`. They remain available only as generated source facts used to describe storage.

## Unavailable floating slots

The raw table reserves three slots that are not supported HolyC primitives:

| Raw code | Raw ID | Pinned source marker |
| --- | ---: | --- |
| `RT_F32` | 12 | Not implemented |
| `RT_UF32` | 13 | Not implemented, fictitious |
| `RT_UF64` | 15 | Fictitious |

The generator preserves these records so changes in the reference cannot pass unnoticed. The semantic API has no `F32`, `UF32`, or `UF64` constructor, and lookup rejects all three spellings.

## Pointer raw representation

`RT_PTR` has raw ID 10, the same ID as signed `RT_I64`. The source comment explains that signed representation permits negative error codes. `Primitive_type.pointer_representation` records the alias for later pointer work, but `I64` values do not thereby become pointers.

## Primitive declarations, prototypes, and definitions

The current parser grammar is deliberately narrow:

```text
module             := item*
item               := global-declaration | bound-function-prototype
                    | unbound-function-definition
                    | top-level-statement
global-declaration := declaration-modifier* declaration-binding? primitive declarator ("," declarator)* ";"
bound-function-prototype := declaration-modifier* declaration-binding primitive return-declarator
                            "(" parameter-list? ")" ";"
unbound-function-definition := declaration-modifier* primitive return-declarator
                               "(" parameter-list? ")" function-body
function-body      := statement-sequence | end-of-input
declaration-modifier := "public" | "static" | "interrupt" | "haserrcode"
                      | "argpop" | "noargpop"
declaration-binding := ordinary-binding | alternate-binding identifier
                     | intern-binding core-expression
ordinary-binding   := "extern" | "import"
alternate-binding  := "_extern" | "_import"
intern-binding     := "_intern"
declarator         := pointer-star{0,4} identifier array-dimension*
return-declarator  := pointer-star{0,4} identifier
parameter-list     := variadic | parameter ("," parameter)* ("," variadic)?
parameter          := register-qualifier* primitive register-qualifier*
                      pointer-star{0,4} parameter-declarator? parameter-default?
parameter-declarator := identifier | function-pointer-declarator
function-pointer-declarator := "(" pointer-star{1,4} identifier? ")"
                               "(" parameter-list? ")"
array-dimension    := "[" core-expression? "]"
parameter-default  := "=" (core-expression | "lastclass")
top-level-statement := statement-sequence
statement-sequence := comma* (statement (comma+ statement)* comma*)?
statement          := ";" | statement-expression statement-terminator
                    | print-statement | put-chars-statement
                    | compound-statement | if-statement | while-statement
                    | do-while-statement | for-statement
                    | goto-statement | label-statement | lock-statement
                    | try-catch-statement | break-statement | return-statement
compound-statement := "{" statement-sequence* "}"
if-statement       := "if" "(" core-expression ")" required-statement-sequence
                      ("else" required-statement-sequence)?
while-statement    := "while" "(" core-expression ")" required-statement-sequence
do-while-statement := "do" required-statement-sequence "while"
                      "(" core-expression ")" ";"
for-statement      := "for" "(" required-statement-sequence
                      core-expression ";" required-statement-sequence?
                      ")" required-statement-sequence
goto-statement     := "goto" identifier statement-terminator
label-statement    := unresolved-identifier ":"
lock-statement     := "lock" required-statement-sequence
try-catch-statement := "try" required-statement-sequence
                       "catch" required-statement-sequence
break-statement    := "break" statement-terminator
return-statement   := "return" (";" | core-expression statement-terminator)
required-statement-sequence := statement-sequence containing at least one statement
statement-expression := core-expression not beginning with a string or character literal
statement-terminator := ";" | comma-boundary
comma-boundary     := a comma retained by statement-sequence
print-statement     := string-marker print-fixed-argument ("," core-expression)* ";"
put-chars-statement := character-marker character-fixed-argument
                       (";" | comma-boundary)
core-expression    := postfix-expression
                    | prefix-operator core-expression
                    | core-expression binary-operator core-expression
postfix-expression := ordinary-primary chain-suffix* postfix-update?
                    | sizeof-expression
                      (primitive-cast-suffix chain-suffix* postfix-update?)?
                    | offset-expression
                      (primitive-cast-suffix chain-suffix* postfix-update?)?
                    | defined-expression
                      (primitive-cast-suffix chain-suffix* postfix-update?)?
ordinary-primary   := literal | identifier | "$$" | "(" core-expression ")"
sizeof-expression  := "sizeof" open-parenthesis{n} identifier
                      ("." identifier)* pointer-star* close-parenthesis{n}
offset-expression  := "offset" open-parenthesis{n} identifier
                      ("." identifier)+ close-parenthesis{n}
defined-expression := "defined" open-parenthesis{n} source-token
                      close-parenthesis{n}
source-token        := one non-EOF lexical token other than "("
chain-suffix        := call-suffix | index-suffix | member-suffix
                     | primitive-cast-suffix
call-suffix        := "(" ")"
                    | "(" call-slot ("," call-slot)* ")"
call-slot          := core-expression?
index-suffix       := "[" core-expression "]"
member-suffix      := "." identifier | "->" identifier
primitive-cast-suffix := "(" primitive pointer-star{0,4} ")"
postfix-update     := "++" | "--"
prefix-operator    := "+" | "-" | "!" | "~" | "*" | "&" | "++" | "--"
variadic           := register-qualifier* "..."
register-qualifier := "noreg" | "reg" u64-register?
u64-register       := RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI
                    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
pointer-star       := "*"
primitive          := I0 | I8 | I16 | I32 | I64
                    | U0 | U8 | U16 | U32 | U64 | F64 | Bool
```

Empty and comment-only inputs are valid modules. Declarations retain source order, the exact modifier, binding, type, and identifier spellings, and ordered source segments. A comma-separated group has one shared modifier list, optional binding, and primitive type, plus a source-linked declarator for every name. Each declaration prefix is a separate ordered AST node. An ordinary `extern` or `import` token becomes a binding node without a target. `_extern` and `_import` store the following target identifier in that node, separate from the HolyC name being declared. `_intern` instead stores a source-positioned expression target. A type-safe target variant keeps these three cases distinct. Each declarator records its terminating comma or semicolon explicitly. Each pointer star is a separate layer numbered from one through four, and numbering starts again for every declarator. A modifier, binding, target, layer, bracket, or delimiter keeps its raw spelling, generated frame, invocation span, and definition span. Includes and definitions run through the integrated stream before parsing. A parser diagnostic produced inside an include retains its include stack, while a diagnostic on definition-generated text records both the expansion and declaration sites.

`Compiler/PrsStmt.HC:PrsStmt` handles visibility, storage, and calling modifiers before it recognizes a binding. The staged assignments are order-sensitive. `public` and each calling modifier retain calling, public, and assembly state. `interrupt` also sets `FSF_NOARGPOP`. `static` keeps assembly state and clears the other staged bits. The AST preserves this source order, and tests fold the nodes through the checked `Function_flag.apply_modifier` model. It does not apply the resulting calling convention or interrupt behavior.

Ordinary `KW_EXTERN` and `KW_IMPORT` select `PRS0_EXTERN` and `PRS0_IMPORT`. The `_extern` and `_import` paths consume a target identifier before the type and select `PRS0__EXTERN` or `PRS0__IMPORT`. `KW__INTERN` instead calls `LexExpressionI64` and selects `PRS0__INTERN`. Its function path assigns the evaluated value to `exe_addr`, sets `Ff_INTERNAL`, and clears `Cf_EXTERN`. `PrsGlblVarLst` rejects both import modes unless the compile controller is AOT; `_intern` has no such gate. The current syntax tree preserves the ordered prefix tokens, binding mode, symbol target, or unevaluated expression target. It does not look up a target, resolve an alias, evaluate an `_intern` address, set internal-function state, allocate storage, or emit an import. A missing alternate target reports `HCPARSE0007`. The routine calls `PrsType` again after a comma while passing the saved base class and mode. `Compiler/PrsVar.HC:PrsType` counts stars and rejects a depth greater than `PTR_STARS_NUM`; the pinned `Kernel/KernelA.HH` sets that value to four. `Compiler/PrsLib.HC:PrsClassNew` creates the matching depth-zero through depth-four class records. The syntax tree records the pointer layers and list boundaries, but semantic pointer types, layout, dereference behavior, and conversions are not implemented yet.

After the identifier, `PrsType` calls `PrsArrayDims`. That routine reads dimensions in source order, accepts an empty expression only in the first pair of brackets, evaluates every sized dimension through `LexExpressionI64`, and requires a closing `]`. The parser mirrors the syntax without performing the evaluation. Each dimension stores both brackets and either a core expression or an explicit unsized state, so `[]` and `[0]` cannot collapse into the same AST. `HCPARSE0022` rejects an empty later dimension, and `HCPARSE0023` reports a missing closing bracket. Dimension expressions use the same checked operator table and 256-level hosted limit as parameter defaults. Negative-size rejection, total element counts, alignment, allocation, and initializers belong to semantic analysis and layout.

The current list grammar accepts forms found in the pinned corpus such as `I64 prime_range,my_mp_cnt,pending;`, `I32 *arg1,*arg2;`, `U8 *ptr,**idx;`, `public extern U8 *rev_bits_table,*set_bits_table;`, `public extern U16 mon_start_days1[12];`, `_extern MEM_BOOT_BASE U32 mem_boot_base;`, and `_intern Bsf I64 Bsf(I64 bit_field_val);`. It also accepts expression dimensions such as `I64 cmp_type_flags_src_code[(DOCT_TYPES_NUM+63)/64];`. Bound prototypes may use ordered and repeated `public`, `static`, `interrupt`, `haserrcode`, `argpop`, or `noargpop` prefixes. Both extern spellings and `_intern` work in every compile mode, while both import spellings work only in AOT mode. Each completed name is published before the parser asks the integrated stream for the next token, so a following conditional can observe it even when the directive appears after a comma. A JIT `import` or `_import` receives `HCPARSE0006` before its declarator is parsed, so the rejected name cannot become visible.

When a bound declaration name is followed by `(`, `Compiler/PrsStmt.HC:PrsGlblVarLst` calls `PrsFunJoin` instead of creating a global variable. `PrsFunJoin` delegates the parentheses and members to `Compiler/PrsVar.HC:PrsVarLst` in `PRS1_FUN_ARG` mode. That path admits omitted parameter names, consumes commas between parameters, and sends `TK_ELLIPSIS` through `PrsDotDotDot`, which establishes `Ff_DOT_DOT_DOT` and the later `argc` and `argv` members. It rejects array dimensions in function arguments.

`PrsVarLst` accepts `reg` and `noreg` in two loops: before a parameter type and immediately after it, before any pointer star or name. `reg` sets `REG_ALLOC`; `noreg` sets `REG_NONE`. A following identifier belongs to `reg` only when `DefineMatch` finds it in `ST_U64_REGS`. The pinned definition contains exactly `RAX`, `RCX`, `RDX`, `RBX`, `RSP`, `RBP`, `RSI`, `RDI`, and `R8` through `R15`. Other register spellings from `OpCodes.DD`, such as `EAX` or `R8u64`, remain available as parameter names in this context. The OCaml parser derives the accepted set from the first R64 record for each register number in the checked opcode database instead of maintaining another register list.

Repeated qualifiers are valid. TempleOS overwrites the working `_reg` value as it reads them, so the final qualifier controls later allocation. The AST keeps every qualifier in source order, records whether it appeared before or after the type, and retains an optional explicit-register identifier. This preserves enough information for the semantic pass to reproduce the overwrite while diagnostics and dumps still show the original spelling. A qualifier may also precede a terminal `...`, matching the `_reg` argument passed to `PrsDotDotDot`. A qualifier after a pointer star receives `HCPARSE0013` instead of being silently moved.

The corresponding `Function_prototype` AST node requires one of the five supported bindings and a semicolon. It keeps the ordered prefix nodes, return type and pointer layers separate from the function name, then records both parentheses, every primitive parameter, its register qualifiers and pointer layers, optional parameter names and defaults, commas, a distinct terminal variadic marker, and the semicolon. `extern U8 *CmdLinePmt();`, `public _extern _SYS_HLT U0 SysHlt();`, and `_intern Bsf I64 Bsf(I64 bit_field_val);` are pinned examples. `public _extern _CALL I64 Call(U8 *machine_code);` supplies a named pointer parameter. Accepted prototypes enter the streaming symbol environment as functions only after the complete semicolon is present.

An unbound name followed by `(` takes the `PrsFun` path instead. `PrsFun` shares `PrsFunJoin` for the signature, publishes the function, and calls `PrsStmt` once for the body. The `Function_definition` AST node therefore keeps the same modifiers, return type, pointer layers, name, parameter list, and variadic marker as a prototype, followed by any statement shape the parser currently supports. The function is visible before the first body token is requested, which permits a recursive call and lets a following `#ifdef` observe it. Header and body tokens may come from different include or definition frames without losing either origin.

The source statement parser treats a lone semicolon as an empty statement and exits cleanly when it encounters EOF. Those cases remain distinct in the AST: `U0 Empty();` has an explicit empty-statement body, while `U0 End()` at EOF has no body node. A bound form such as `extern U0 Prototype();` is still a prototype, and a following block is rejected rather than reclassified as a definition. Local declarations and function scopes are not implemented, so the accepted body is limited to the statement forms listed above.

`Compiler/PrsVar.HC:PrsType` treats `(` followed by one through four stars as a function-pointer declarator. Stars before that parenthesis belong to the callback's return type; stars inside it record the function-pointer indirection. An optional name appears before the closing parenthesis, and `PrsFunJoin` parses the following empty, fixed, or variadic signature. Because that signature returns to `PrsVarLst`, its parameters may contain another function-pointer declarator. The AST represents this recursion directly and retains all four parentheses, each star, names, nested parameters, commas, and definition provenance. The hosted parser rejects a thirty-third nested function-pointer type with `HCPARSE0017`; the pinned corpus does not approach this safety limit.

In `PRS1B_FUN_ARG` mode, `PrsVarLst` treats `=` as the start of a default. It first checks for the `lastclass` keyword and otherwise calls `LexExpression2Bin`. It does this for each parameter independently, so a required parameter may follow one with a default. The OCaml parser keeps all three cases distinct: no source default, an ordinary expression default, or a `lastclass` default. Each default retains its equals sign, and the `lastclass` form retains the keyword spelling and location. An explicit `0` is an integer-literal node and is never confused with either absence or `lastclass`. The same grammar applies inside nested function-pointer signatures.

The core-expression parser accepts integer, floating, character, multi-character, and string literals; identifiers; `$$`; grouped expressions; named `sizeof` and `offset` terms; the eight audited prefix operators; every binary operator in the pinned table below; parenthesized call suffixes; bracket index suffixes; direct or pointer member suffixes; primitive postfix casts; and postfix `++` or `--`. Parameter defaults, array dimensions, call arguments, indexes, and `_intern` targets call this same parser with context-specific stopping rules and diagnostic wording. It uses the table's numeric precedence and association rather than C precedence. Calls, indexes, member accesses, primitive casts, `sizeof`, `offset`, and a terminal postfix update bind before every binary operator. Calls, indexes, members, and primitive casts may repeat in any order after an ordinary primary expression. `IC_POWER` receives the special handling from `PrsExpression2`, so ``-2`2`` is represented as ``-(2`2)``. Each term, postfix delimiter, operator, cast target, and member identifier keeps its source span, generated source, and definition origin. A fixed limit of 256 nested expression parses rejects adversarial input with `HCPARSE0021`.

`Compiler/PrsExp.HC:PrsFunCall` walks fixed parameters in declaration order. When a parameter has `MLF_DFT_AVAILABLE`, a comma or the closing parenthesis selects its default instead of parsing an expression. This permits the documented `Test(,3);` form and corpus calls such as `PrsStmt(cc,,,cmp_flags)`. The AST records every slot as either omitted or provided, attaches each comma to the slot before it, and gives an omitted slot a zero-width insertion location. Empty `Target()` has no slots, while `Target(,)` has two omitted slots. The syntax tree therefore never conflates a hole with an explicit integer zero.

`Compiler/PrsExp.HC:PrsUnaryModifier` handles `[` as a term-level suffix, parses its contents with `PrsExpression`, requires `]`, and returns to the modifier state. This permits repeated and mixed forms such as `table[Outer(,3)][j]` and `callbacks[0](value)`. The AST stores the base, both brackets, and the complete index expression. It does not reproduce the source routine's type checks, array-dimension tracking, element-size multiplication, address addition, dereference state, or lvalue result; those depend on resolved types and lowering.

The dot and `TK_DEREFERENCE` branches of `PrsUnaryModifier` share a member-name path and return to the same modifier state. The AST therefore distinguishes `object.field` from `pointer->field` and supports chains such as `Factory().nodes[0]->callback(1).result`. It keeps the complete base expression, direct or pointer access kind, original operator, and member identifier. This syntax slice does not perform `MemberFind`, validate the base type, choose address versus value behavior, compute a member offset, apply inheritance, dereference a pointer, or classify an lvalue.

The postfix `TK_PLUS_PLUS` and `TK_MINUS_MINUS` branches in `PrsUnaryModifier` set `CCF_POSTINC` or `CCF_POSTDEC`, assign `PREC_UNARY_POST`, consume the token, and return `PE_DEREFERENCE` instead of returning to the modifier loop. `PrsExpression2` then selects `IC__PP` or `IC__MM`. The AST mirrors that boundary with a distinct postfix node containing the full operand, the update kind, and the source-positioned operator. It accepts forms such as `*ptr++`, `object.field--`, and `counter++ + 1`. A call, index, member access, or second update after `++` or `--` receives `HCPARSE0028`; the parser does not silently attach that suffix to the update result. Modifiable-lvalue checks, type rules, mutation, old-value production, and IC lowering remain semantic work.

The `(` branch in `PrsUnaryModifier` is a function call when `CCF_FUN_EXP` is set. Otherwise it reads a type with `PrsType` in `PRS0_TYPECAST` mode, emits `IC_HOLYC_TYPECAST`, and returns to the modifier loop. HolyC therefore writes `ptr(U8 *)`, not the C-style `(U8 *)ptr`; `PrsUnaryTerm` rejects the latter with “Use TempleOS postfix typecasting.” The current syntax pass recognizes all 12 checked primitive spellings with zero through four pointer stars and records the operand, both parentheses, target type, and each star. A primitive token after `(` selects a cast; other tokens continue through the call grammar. Casts may repeat or combine with calls, indexes, members, and a later terminal update. `HCPARSE0029` rejects C-style prefix casts, and `HCPARSE0030` reports a primitive cast without its closing parenthesis. Resolved class, union, array, and function-pointer cast targets, source warnings, conversion behavior, and lowering remain future work.

`PrsUnaryTerm` handles `KW_SIZEOF` separately from ordinary C-style expression parsing. It consumes any number of opening parentheses, requires one identifier, follows every dot-separated member name, consumes every trailing pointer star, and then requires one closing parenthesis for each opening parenthesis. The pointer loop is inside `PrsSizeOf`, so it does not use the four-star limit from `PrsType`. `Doc/HolyC.DD` says that `sizeof` accepts only one member level, but the executable parser loops across repeated dots. The AST follows that loop and records the complete member path. Once the term is complete, `PrsUnaryTerm` returns `PE_MAYBE_MODIFIERS`. A following `(` is therefore a postfix type cast, not a call. Calls, indexes, members, and updates may follow only after that cast returns to the ordinary modifier loop. `HCPARSE0031` reports a missing named target, `HCPARSE0032` reports a dot without a member, `HCPARSE0033` reports unmatched wrapper parentheses, and `HCPARSE0034` reports a direct suffix that the source state cannot accept. This pass leaves the target unresolved and does not calculate its size, adjust a local use count, or emit `IC_IMM_I64`.

The adjacent `KW_OFFSET` branch uses the same wrapper and modifier rules but calls `PrsOffsetOf`. That routine requires a named local, class, or global root followed by at least one dot and member name. It adds each resolved member offset and follows the member class while dots remain. The OCaml AST keeps the unresolved root and complete member path, following the executable loop even though `Doc/HolyC.DD` describes one member level. Pointer stars are not part of this term. `HCPARSE0035` reports a missing named root, `HCPARSE0036` reports a missing first dot, `HCPARSE0037` reports a dot without a member, `HCPARSE0038` reports unmatched wrappers, and `HCPARSE0039` reports a forbidden direct suffix. Root and member lookup, local-use accounting, layout arithmetic, and `IC_IMM_I64` emission remain semantic and lowering work.

The following `KW_DEFINED` branch also accepts any number of wrapper pairs, but its operand is exactly one token. The pinned lexer represents language keywords and ordinary names as `TK_IDENT`; the parser therefore marks both as name-shaped. Other tokens remain explicit non-name operands because the source consumes them and emits false. A later semantic pass must resolve a name through the hash or local-variable entry, leave an unresolved name false, and emit the I64 Boolean. This syntax pass does not guess that result. The term returns `PE_MAYBE_MODIFIERS`, so it shares the primitive-cast boundary used by `sizeof` and `offset`. `HCPARSE0040` reports end of input before the operand, `HCPARSE0041` reports unmatched wrappers, and `HCPARSE0042` reports a forbidden direct suffix.

This slice parses but does not execute default, dimension, `_intern`, call, index, member, postfix update, primitive cast, `sizeof`, `offset`, or `defined` expressions. It does not apply type or cast conversions, copy string defaults, set member flags, resolve a direct or indirect callee, check argument types or counts, validate holes against declared defaults, distinguish fixed from variadic arguments, fill omitted arguments, substitute the previous explicit argument's class name for `lastclass`, evaluate array sizes, validate an index or member base, scale an index, calculate member layout, dereference an element or pointer member, apply an increment or decrement, resolve a `sizeof` or `offset` target, calculate a size or member offset, resolve a `defined` name or choose its Boolean result, assign executable addresses, or set internal-function state. Calls without parentheses and resolved nonprimitive cast targets remain outside this core expression slice and receive `HCPARSE0020` with wording for the active context. `HCPARSE0024` reports a missing comma between call arguments, `HCPARSE0025` reports an unclosed call, `HCPARSE0026` reports an unclosed index, `HCPARSE0027` reports a member operator without a following identifier, `HCPARSE0028` reports a suffix after a terminal postfix update, `HCPARSE0029` rejects C-style prefix casts, `HCPARSE0030` reports an unclosed primitive postfix cast, `HCPARSE0031` through `HCPARSE0034` cover malformed `sizeof` forms, `HCPARSE0035` through `HCPARSE0039` cover malformed `offset` forms, and `HCPARSE0040` through `HCPARSE0042` cover malformed `defined` forms. Array parameters remain unavailable. Function definitions establish a function-wide parser lookup context for parameters, variadic `argc` and `argv`, and primitive local names, but that context is not a semantic scope and carries no storage or type-checking state. Global initializers, parenthesized function-pointer globals, function-pointer members and locals, member arrays, class and union locals, aggregate initializers, and other unsupported declarator forms are not stored as raw tokens. General unsupported global syntax uses `HCPARSE0001` through `HCPARSE0003`; a fifth star uses `HCPARSE0004`, a parenthesized function-pointer global uses `HCPARSE0005`, malformed function-pointer parameter punctuation uses `HCPARSE0014`, and a missing alternate target uses `HCPARSE0007`. A modifier or binding without a supported primitive type receives `HCPARSE0001` at the following token. Recovery advances to a semicolon or EOF, and the public result contains no AST after any error.

`holyc parse` and `holyc dump-ast` emit the same `holyc-ast-v1` human or JSON representation. The library exposes `parse`, `parse_with_config`, and `parse_detailed`; the detailed form keeps nonfatal preprocessor warnings beside a successful AST.

## Primitive local declarations

Inside a function, `Compiler/PrsStmt.HC:PrsStmt` routes a known primitive type to `PrsVarLst` instead of the global declaration path. A leading `static` selects the static-local mode. Automatic locals may place `reg` or `noreg` after the shared type on each declarator, and `reg` may consume one canonical U64 register name. `PrsVarLst` then reads pointer stars, the name, array dimensions, an optional scalar initializer, and either a comma or the final semicolon. It calls `MemberAdd` before parsing the initializer, so a new name is visible in its own initializer and in later declarators.

The AST keeps the storage class, shared primitive type, and every declarator in source order. Each declarator records its own register request, optional explicit register, pointer layers, array dimensions, initializer, and delimiter. `Adam/Snd/SndMath.HC` supplies a comma group with repeated `reg`; `Demo/Asm/AsmAndC1.HC` and `Demo/Asm/AsmAndC2.HC` show `noreg` and explicit `R15`; and `Kernel/KMisc.HC` supplies a grouped static initializer. Named parameters enter the same parser context before the body. Variadic functions also expose `argc` and `argv`, matching the synthetic members created by `PrsDotDotDot`.

The lookup context spans the whole parsed function, including nested blocks. The parser uses it to route later identifiers and symbol conditionals; it does not assign offsets, allocate storage, enforce block lifetime, resolve shadowing semantics, honor a register request, evaluate an initializer, or calculate array sizes. Class and union locals, local function-pointer declarators, and aggregate initializers remain unsupported. `HCPARSE0098` through `HCPARSE0104` report malformed or unsupported local forms without returning a successful AST. Static locals reject the automatic after-type `reg` and `noreg` grammar with `HCPARSE0099`.

## Top-level statement sequences

`PrsStmt` skips leading commas before selecting a statement form. It accepts a lone semicolon, parses an ordinary expression, and returns only when the next token is not a comma. A comma may replace the semicolon after an expression, and repeated commas are skipped before the next statement. The same loop applies when a semicolon-terminated statement is followed by a comma.

The AST keeps ordinary expression, empty, `goto`, label, `break`, and `return` statements and comma-linked sequences as different shapes. A sequence records every leading comma, every statement in source order, and every following comma. Expressions, `goto`, `break`, and value-bearing returns record whether they ended with their own semicolon or at another valid boundary. A group containing only commas has no semantic statements but still keeps those source locations, matching the pinned loop's behavior at end of input.

At top level, a known global or function name can begin an expression statement. An identifier absent from the current symbol environment becomes a label only when the next token is `:`; otherwise it stays on the unresolved declaration path. This prevents a known global or function followed by `:` from silently changing meaning. Labels and `goto` statements also work after sequence commas. The same sequence representation is used recursively inside compound blocks, conditional branches, loop bodies, and function definitions. Within a function, a known primitive type can begin a local declaration at the same sequence boundaries. `HCPARSE0047` reports a missing expression-statement terminator. `HCPARSE0048` reports another unsupported form at a comma boundary.

## Compound statements

When `PrsStmt` sees `{`, it advances past the opening brace and calls itself until it reaches `}` or end of input. This makes empty and nested blocks ordinary statement forms. After consuming the closing brace, a comma continues the surrounding `PrsStmt` loop; without that comma, the current statement group ends.

The AST records both braces, every child statement in source order, and one location spanning the complete block. A block may appear at top level or inside another block. Its children use the same expression, empty, output-literal, sequence, conditional, loop, `goto`, label, `break`, `return`, and block nodes as top-level statements. Include and definition expansions retain their normal origins on either brace.

This is a syntactic boundary only. A block does not create a semantic scope yet, whether it appears at top level, inside another block, or as a function body. Primitive locals in a function join its function-wide parser context even when they appear in a nested block. Storage, lifetime, lowering, and execution remain unimplemented. `HCPARSE0049` reports end of input before a closing brace and points back to the opener. `HCPARSE0050` reports a top-level closing brace without a matching opener. A comma immediately before `}` still expects another statement and receives `HCPARSE0048`, matching the source loop rather than treating the comma as an optional trailing delimiter. The hosted parser accepts at most 256 nested blocks and reports `HCPARSE0051` beyond that point. This fixed guard is a denial-of-service limit, not a TempleOS syntax rule.

## Conditional statements

`Compiler/PrsStmt.HC:PrsIf` requires `(` immediately after `if`, parses one ordinary expression, requires `)`, and then calls `PrsStmt` for the then branch. If the following keyword is `else`, it calls `PrsStmt` again for that branch. Recursive statement parsing gives an `else` to the nearest unmatched `if` without a separate disambiguation rule.

The AST records the `if` keyword, both parentheses, the condition, the complete then branch, and an optional else clause with its own keyword and branch. A branch may be an empty, expression, output, sequence, block, loop, `break`, `return`, or nested conditional statement. Comma-linked branch statements use the same sequence node as top-level and block input. JIT and AOT modes share this syntax, and tokens supplied by an include or definition keep their normal provenance.

This pass does not convert the condition to a Boolean, resolve names, validate branch types, build control flow, lower `IC_BR_ZERO`, or execute either branch. `HCPARSE0052` and `HCPARSE0053` report missing condition parentheses. `HCPARSE0054` and `HCPARSE0056` report missing then and else branches, while `HCPARSE0055` reports an unmatched `else`. The hosted parser accepts at most 256 nested conditional statements and reports `HCPARSE0057` beyond that point. This guard limits recursive parsing of untrusted input; it is not a TempleOS language limit.

## While statements

`Compiler/PrsStmt.HC:PrsWhile` requires `(` immediately after `while`, parses one ordinary expression, requires `)`, and calls `PrsStmt` for the body. The source emits a condition branch, parses the body with the loop's break label, and emits a jump back to the condition. The current parser preserves this syntax without claiming those executable semantics.

The AST records the `while` keyword, both parentheses, the condition, and the complete body. A body may use any currently supported statement form, including blocks, comma-linked sequences, nested loops, and conditionals. Recursive parsing keeps the usual dangling-`else` behavior: an `else` after an `if` whose body is a loop belongs to that `if`, while an `else` inside a loop body belongs to the nearest inner `if`. JIT and AOT modes share the node, and include or definition frames keep their normal provenance.

`HCPARSE0058` and `HCPARSE0059` report missing condition parentheses, while `HCPARSE0060` reports a missing body. The hosted parser accepts at most 256 nested loop statements and reports `HCPARSE0061` beyond that point. The count is shared with `do ... while` so mixed nesting cannot bypass the guard. This is a denial-of-service limit, not a TempleOS language rule. Break-target validation, condition truth conversion, label creation, control-flow construction, semantic checks, IR lowering, and execution remain unimplemented.

## Do-while statements

`Compiler/PrsStmt.HC:PrsDoWhile` emits the loop-entry label before it asks `PrsStmt` to parse the body. It then requires `while`, an opening parenthesis, one ordinary expression, a closing parenthesis, and a final semicolon. The source branches back to the entry label when the condition is nonzero, so the body precedes the condition both syntactically and at execution time.

The AST records the `do` and `while` keywords separately, the complete body, both condition parentheses, the condition, and the required final semicolon. The body uses the common statement path and therefore accepts blocks, comma-linked sequences, pre-test or post-test loops, conditionals, output statements, ordinary expressions, and empty statements. Nearest-`if` else association remains unchanged across the loop boundary. JIT and AOT modes use the same syntax, and every token retains include or definition provenance.

`HCPARSE0062` reports a missing or comma-only body. `HCPARSE0063` through `HCPARSE0066` cover a missing `while`, opening parenthesis, closing parenthesis, or final semicolon. An empty condition uses the shared `HCPARSE0018` expression diagnostic. Mixed `while` and `do ... while` nesting shares the 256-level `HCPARSE0061` guard. Break-target validation, condition truth conversion, label creation, `IC_BR_NOT_ZERO`, semantic checks, IR lowering, and execution remain unimplemented.

## For statements

`Compiler/PrsStmt.HC:PrsFor` requires `(`, then delegates the initializer to the ordinary statement parser. This makes a lone semicolon a valid empty initializer. The condition is a required expression followed by a required semicolon. The update is optional; when present, `PrsFor` calls `PrsStmt` with semicolon checking disabled, so the update stops at `)` and may retain comma-linked statements. The loop body returns to the normal statement path.

The AST keeps the keyword, both parentheses, initializer statement, condition, condition semicolon, optional update statement, and body as separate fields with source provenance. An absent update remains `None` rather than an invented empty statement. Bodies and header statements use the common recursive representation, so existing blocks, conditionals, loops, output statements, expressions, empty statements, comma-linked sequences, and primitive local declarations keep their usual shapes. A local declaration may therefore occupy the initializer when the `for` appears inside a function.

`HCPARSE0067` reports a missing opening parenthesis. `HCPARSE0068` reports a missing initializer statement, while an empty condition uses `HCPARSE0018`. `HCPARSE0069` reports the missing condition semicolon, `HCPARSE0070` reports an invalid update boundary or missing closing parenthesis, and `HCPARSE0071` reports a missing or comma-only body. All three loop forms share the 256-level `HCPARSE0061` hosted guard. Break-target validation, condition truth conversion, internal control-flow label creation, deferred update control flow, semantic checks, IR lowering, and execution remain unimplemented.

## Break statements

The `KW_BREAK` branch in `Compiler/PrsStmt.HC:PrsStmt` consumes the keyword, requires an active `lb_break`, emits `IC_JMP` to that label, and then joins the common statement terminator path. That path accepts `;` or a statement comma when semicolon checking is active. `PrsFor` disables semicolon checking for its update, so that boundary stops before `)` instead of inventing a terminator. `Compiler/CompilerA.HH` assigns `KW_BREAK` value 19, and `Doc/HolyC.DD` uses `break;` throughout its switch examples.

The AST retains the keyword, an optional semicolon, and the complete statement location. A comma-terminated `break` has no semicolon and remains the first element of the ordinary statement-sequence node. A `break` parsed as a `for` update also has no semicolon. Blocks, loops, conditionals, top-level tooling mode, includes, and definition expansions all use the same node and preserve their normal provenance.

This parser intentionally accepts the source shape before it knows whether a loop or switch encloses it. The semantic control-flow pass must reproduce the source `lb_break` check and reject a missing target. `HCPARSE0072` reports a token other than `;` or `,` at an active statement boundary. Recovery stops before an enclosing block close so the parser does not add a false missing-brace diagnostic. Jump-label creation, IR lowering, and execution remain unimplemented.

## Return statements

The `KW_RETURN` branch in `Compiler/PrsStmt.HC:PrsStmt` first requires a current function. It emits one `SysUntry` call for each active `try` region, then treats an immediate semicolon as a valueless return. Otherwise it parses an expression, emits `IC_RETURN_VAL`, marks the function as having a return, and jumps to the function's leave label. The branch finishes through the common statement terminator, so a value-bearing return may end at `;`, at a statement comma, or before `)` in a semicolon-free `for` update. A valueless form requires its immediate semicolon; `return,` is not shorthand for `return;`.

The AST retains the keyword, an optional value expression, an optional semicolon, and the complete statement location. It accepts the syntax at top level for tooling and in every recursive statement context, while preserving include and definition provenance. This is not a claim that top-level `return` is valid HolyC: semantic analysis must enforce the source function-context check. It must also validate the value against the declared return type, reproduce active-`try` unwinding, set function metadata, resolve the leave label, and lower `IC_RETURN_VAL` and the jump. `HCPARSE0073` reports an invalid boundary after a parsed value, and `HCPARSE0074` reports a missing value when the token after `return` cannot begin one. Recovery leaves an enclosing `}` available to the block parser. None of the semantic, lowering, or execution behavior is implemented yet.

## Goto statements and function labels

The `KW_GOTO` branch in `Compiler/PrsStmt.HC:PrsStmt` requires an identifier, finds or creates a `CMT_GOTO_LABEL`, increments its use count, emits `IC_JMP`, and joins the common statement terminator. The unresolved-identifier branch finds or creates the same label record, rejects a second definition, marks the label defined, emits `IC_LABEL`, and requires `:`. A local variable remains on the expression path. `COCGoToLabelFind` searches both language and assembly label records within the current code-control list, while `COCDel` detects unresolved and unused labels. `Doc/ScopingLinkage.DD` records function scope and the source compiler's confusing failure when a label collides with a global object.

The AST uses separate `goto_statement` and `label_statement` nodes. A goto keeps its keyword, identifier target, optional semicolon, and complete location; a label keeps its name, colon, and location. The shared comma sequence records a comma boundary, and a goto in a `for` update stops before `)` without inventing a semicolon. An absent name immediately followed by `:` selects the label path. Known globals and functions retain expression routing, which preserves the source collision instead of quietly accepting a label. Include and definition expansions keep their normal origins in both dump formats. `HCPARSE0075` reports a missing or nonidentifier target, and `HCPARSE0076` reports a token other than `;` or `,` at an active boundary.

Top-level label syntax is accepted only as a tooling representation until function definitions exist. Native HolyC rejects a language label without a current function. Semantic work must assign stable function-scoped label IDs, resolve forward and backward references, diagnose duplicate, unresolved, unused, and colliding names, and lower `IC_LABEL` and `IC_JMP`. No binding, control-flow construction, lowering, or execution is claimed by this parser slice.

## Lock statements

`Compiler/PrsStmt.HC:PrsStmt` consumes `lock`, increments `CCmpCtrl.lock_cnt`, parses one ordinary statement, and then restores the count. This accepts a compound statement such as `lock {value++;}` and the unbraced `lock value++;` form used by `Demo/MultiCore/Lock.HC`. Because the nested call is the normal statement parser, a lock may contain another lock or a comma-linked statement sequence. `Compiler/PrsLib.HC:ICAdd` adds `ICF_LOCK` to instructions created while the count is nonzero.

The AST retains the `lock` keyword, its recursively parsed body, and their complete combined location. It does not flatten the body because later IR lowering needs the lexical region. The syntax is available in JIT and AOT parser modes, at top level for tooling, and inside every implemented recursive statement context. Include and definition expansions retain their ordinary provenance in human and JSON dumps.

`HCPARSE0077` reports a missing or comma-only body. An independent 256-level `HCPARSE0078` hosted guard prevents unbounded recursion through repeated unbraced locks; no corresponding TempleOS language limit was found. This parser slice does not mark IR instructions with `ICF_LOCK`, determine whether an operation accepts an x86 `LOCK` prefix, select machine instructions, or execute multicore code.

## Switch statements

`Compiler/PrsStmt.HC:PrsSwitch` accepts two header forms. `switch (expression)` is the bounded form associated with `IC_SWITCH`, while `switch [expression]` is the no-bound form associated with `IC_NOBOUND_SWITCH`. Both forms require a braced body. The body may contain ordinary statements, `case:`, `case expression:`, inclusive `case low...high:`, and `default:`. `KW_DFT` is the pinned compiler's internal keyword name; the spelling entered in HolyC source is `default`. An implicit case starts at zero or advances from the previous case. The source compiler also swaps a reversed range before populating its jump table.

`start:` and `end:` delimit a sub-switch region. The pinned compiler keeps a stack of these regions, creates a separate break label for each one, and treats statements before the first case in a region as its front porch. The language guide restricts control transfers out of that front porch. The AST preserves this structure as recursive elements instead of flattening `start` and `end` into ordinary labels. It also keeps the selected delimiter mode, expression, braces, each case pattern, default labels, ordinary statements, and full source order. Switches can appear wherever the shared statement parser is currently available, including blocks, loops, `for` updates, conditionals, locks, and try or catch bodies. JIT and AOT syntax is identical, and include or definition frames keep the provenance of delimiters and labels.

The switch diagnostic codes from `HCPARSE0084` to `HCPARSE0097`, with `HCPARSE0089` unused, report the hosted depth limits and malformed headers, braces, ranges, colons, or sub-switch boundaries. Recovery stops at the next structural label or enclosing switch close, but a recovered error still removes the public AST. This slice does not evaluate case expressions, assign implicit values, swap range endpoints, reject duplicates, validate a default, enforce front-porch transfer rules, bind `break`, create jump tables, emit switch or sub-call ICs, lower control flow, or execute a switch.

## Try and catch statements

`Compiler/PrsStmt.HC:PrsTryBlk` compiles a required statement after `try`, requires `catch`, and then compiles another required statement. Both calls use the ordinary statement parser with an incremented active-try count, so braces are optional and either side may contain another pair or a comma-linked sequence. The compiler surrounds the first body with calls to `SysTry` and `SysUntry`, installs handler labels, and uses the active count when a `return` must leave nested try regions. `Compiler/PrsExp.HC` and `Kernel/Job.HC` contain unbraced forms, while `Demo/Exceptions.HC` contains braced and nested forms. `Doc/HolyC.DD` also notes that lowercase `throw` is an ordinary function call, not a separate statement keyword.

The AST retains the `try` keyword, recursively parsed body, `catch` keyword, recursively parsed handler, and complete paired location. It accepts the syntax in JIT and AOT modes, at top level for tooling, and in every implemented recursive statement context. Definition and include frames keep their usual provenance. The parser does not invent an optional handler or flatten either side.

`HCPARSE0079`, `HCPARSE0080`, and `HCPARSE0081` report a missing body, missing `catch`, or missing handler. A standalone `catch` reaches the pinned compiler's generic `Missing expression` path; the hosted parser preserves rejection but uses the more useful `HCPARSE0082` message. An independent 256-level `HCPARSE0083` guard bounds recursive unbraced pairs, with no matching source-language limit identified. This slice does not emit `SysTry` or `SysUntry`, allocate handler labels, transfer exceptions, unwind returns, lower `throw` calls, or execute exception code.

## Statement-position output literals

`PrsStmt` treats a string or character literal in statement position as a call shorthand. Strings select `Print`, and characters select `PutChars`. The current parser implements this syntax for top-level items and recursively inside compound blocks, conditional branches, and loop bodies. HolyC permits executable top-level statements without a conventional `main`.

The marker determines how the fixed argument begins:

- A nonempty marker starts the fixed expression. For example, `"value";` passes that string expression, while `'A'+1;` passes the complete addition expression.
- An empty string marker is consumed before the format expression, as in `"" fmt,name;`.
- An empty character marker is consumed before the character expression, as in `'' value;`.
- A `Print` statement may retain further comma-led expressions before its required semicolon.
- `PutChars` has one fixed argument. A following comma ends that statement and continues the enclosing statement sequence.

The AST records an explicit `Print` or `PutChars` target, the original marker, whether the fixed expression began at or after that marker, every additional `Print` argument and comma, an optional semicolon where the source permits a sequence comma, and all source origins. Top-level declarations, prototypes, and statements remain in one source-ordered item list in JIT and AOT modes. Blocks retain the same ordering among their children. This is syntax support only. Function-body statements, runtime symbol resolution, argument checking, call lowering, and top-level execution are not implemented yet.

## Operators and precedence

The generated operator table follows `Compiler/CInit.HC:CmpFillTables`. The binary entries appear in these source bands:

| Precedence constant | Value | Operators |
| --- | ---: | --- |
| `PREC_EXP` | `0x10` | `` ` ``, `<<`, `>>` |
| `PREC_MUL` | `0x14` | `*`, `/`, `%` |
| `PREC_AND` | `0x18` | `&` |
| `PREC_XOR` | `0x1C` | `^` |
| `PREC_OR` | `0x20` | `\|` |
| `PREC_ADD` | `0x24` | `+`, `-` |
| `PREC_CMP` | `0x28` | `<`, `>`, `<=`, `>=` |
| `PREC_CMP2` | `0x2C` | `==`, `!=` |
| `PREC_AND_AND` | `0x30` | `&&` |
| `PREC_XOR_XOR` | `0x34` | `^^` |
| `PREC_OR_OR` | `0x38` | `\|\|` |
| `PREC_ASSIGN` | `0x3C` | `=`, `<<=`, `>>=`, `*=`, `/=`, `%=`, `&=`, `\|=`, `^=`, `+=`, `-=` |

Power and assignments carry `ASSOCF_RIGHT`. Shifts, division, modulo, and subtraction carry `ASSOCF_LEFT`. Other records have no association flag in the source, so the API reports `Unspecified`. The default-expression parser treats only `ASSOCF_RIGHT` as right-associative; both `Left` and `Unspecified` group to the left, matching the loop in `PrsExpression2` without inventing a C rule.

The backtick operator maps to `IC_POWER`. HolyC logical XOR is `^^` and maps to `IC_XOR_XOR`. No ternary operator appears in the table, matching the language documentation.

The prose precedence list in `Doc/HolyC.DD` omits `%=`. The executable table includes `TK_MOD_EQU` at `PREC_ASSIGN`, mapped to `IC_MOD_EQU`; that source record is authoritative.

Compound recognition is not a flat list in TempleOS. Three ordered arrays resolve shared prefixes, while `Lex` handles `<<=`, `>>=`, `..`, `...`, and `$$` separately. The current lexer consumes the generated spellings in longest-first order, but the generated audit data retains each original recognition group and source line.

## Compiler options

Compiler options are bit indices stored in `CCmpCtrl.opts`. `Compiler/Lex.HC:CmpCtrlNew` initially enables the two warning bits at indices 16 and 19, producing the mask `0x90000`. The source leaves bits 2 through 15 and 20 through 31 unused.

| Source name | Bit | Default | Observed phase | Source note |
| --- | ---: | --- | --- | --- |
| `OPTf_ECHO` | 0 | Off | Lexing | None |
| `OPTf_TRACE` | 1 | Off | Parsing and trace output | None |
| `OPTf_WARN_UNUSED_VAR` | 16 | On | Function diagnostics | Applied to functions, not statements |
| `OPTf_WARN_PAREN` | 17 | Off | Diagnostics | Warns about unnecessary parentheses |
| `OPTf_WARN_DUP_TYPES` | 18 | Off | Parsing and diagnostics | Warns about duplicate local type statements |
| `OPTf_WARN_HEADER_MISMATCH` | 19 | On | Function declarations | None |
| `OPTf_EXTERNS_TO_IMPORTS` | 32 | Off | Parsing and linkage | None |
| `OPTf_KEEP_PRIVATE` | 33 | Off | Symbol registration | None |
| `OPTf_NO_REG_VAR` | 34 | Off | Optimization | Applied to functions, not statements |
| `OPTf_GLBLS_ON_DATA_HEAP` | 35 | Off | Allocation | None |
| `OPTf_NO_BUILTIN_CONST` | 36 | Off | Code emission | Applied to functions, not statements |
| `OPTf_USE_IMM64` | 37 | Off | Optimization and code emission | Not completely implemented in the pinned source |

HolyC's `Option(bit, state)` changes the current compile controller and returns the bit's previous state. `GetOption(bit)` reads that controller. Nested compilation copies its parent's option mask, so source can temporarily change an option around a declaration or function and then restore the previous value. The pinned kernel, startup, and demo sources use this pattern for linkage, private symbols, global allocation, warning control, and optimization boundaries.

`Sema.Compiler_option` currently provides the checked registry, typed lookup, the initial mask, and a pure form of the previous-state update. It does not yet execute `Option` in HolyC input or change parser, optimizer, or backend behavior.

## Current boundary

The implemented language specification supplies immutable primitive type facts, operator tables, compiler-option facts, and syntax for primitive globals, bound prototypes, unbound primitive function definitions, primitive automatic and static locals, recursive compound blocks, `if` and `else`, all three loop forms, `goto`, function labels, `lock`, both switch modes and structural sub-switches, paired `try`/`catch`, `break`, `return`, and `Print` and `PutChars` shorthand statements. It covers ordered declaration prefixes, ordinary, alternate-name, and `_intern` bindings, pointers through depth four, primitive global and local array suffixes, scalar local initializers, parameter and automatic-local register qualifiers, recursive function-pointer parameters, varargs, and core expressions in `_intern` targets, dimensions, parameter defaults, local initializers, conditions, switch labels, and output arguments. It enforces the source parser's AOT gate for both import spellings and accepts `_intern` plus every implemented statement form in JIT and AOT modes. It does not resolve bindings, create semantic block or function scopes, allocate local storage, evaluate `_intern` addresses or initializers, apply internal-function or other linkage and storage effects, build semantic pointer or function types, calculate array layout, honor register allocation requests, fill ordinary call arguments, resolve or lower implicit output calls, bind goto targets, validate labels or break targets, evaluate switch cases or build jump tables, implement exception transfer, execute top-level code, or support global and aggregate initializers, class or union locals, local function-pointer declarators, remaining control flow, classes, unions, promotions, conversions, aggregate layout, floating execution, compiler-option effects, or semantic diagnostics. Those claims remain absent from the compatibility report until their own source-grounded tests pass.
