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

## Primitive declarations and bound prototypes

The current parser grammar is deliberately narrow:

```text
module             := item*
item               := global-declaration | bound-function-prototype
global-declaration := declaration-modifier* declaration-binding? primitive declarator ("," declarator)* ";"
bound-function-prototype := declaration-modifier* declaration-binding primitive return-declarator
                            "(" parameter-list? ")" ";"
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
parameter-default  := "=" core-expression
core-expression    := postfix-expression
                    | prefix-operator core-expression
                    | core-expression binary-operator core-expression
postfix-expression := primary-expression chain-suffix* postfix-update?
primary-expression := literal | identifier | "$$" | "(" core-expression ")"
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

`Compiler/PrsVar.HC:PrsType` treats `(` followed by one through four stars as a function-pointer declarator. Stars before that parenthesis belong to the callback's return type; stars inside it record the function-pointer indirection. An optional name appears before the closing parenthesis, and `PrsFunJoin` parses the following empty, fixed, or variadic signature. Because that signature returns to `PrsVarLst`, its parameters may contain another function-pointer declarator. The AST represents this recursion directly and retains all four parentheses, each star, names, nested parameters, commas, and definition provenance. The hosted parser rejects a thirty-third nested function-pointer type with `HCPARSE0017`; the pinned corpus does not approach this safety limit.

In `PRS1B_FUN_ARG` mode, `PrsVarLst` treats `=` as the start of a default and calls `LexExpression2Bin`. It does this for each parameter independently, so a required parameter may follow one with a default. The OCaml parser keeps that distinction directly: `default = None` means no source default, while `Some` retains the equals sign and expression. An explicit `0` is an integer-literal node and is never confused with absence. The same grammar applies inside nested function-pointer signatures.

The core-expression parser accepts integer, floating, character, multi-character, and string literals; identifiers; `$$`; grouped expressions; the eight audited prefix operators; every binary operator in the pinned table below; parenthesized call suffixes; bracket index suffixes; direct or pointer member suffixes; primitive postfix casts; and postfix `++` or `--`. Parameter defaults, array dimensions, call arguments, indexes, and `_intern` targets call this same parser with context-specific stopping rules and diagnostic wording. It uses the table's numeric precedence and association rather than C precedence. Calls, indexes, member accesses, primitive casts, and a terminal postfix update bind before every binary operator. Calls, indexes, members, and primitive casts may repeat in any order after a primary expression. `IC_POWER` receives the special handling from `PrsExpression2`, so ``-2`2`` is represented as ``-(2`2)``. Each term, postfix delimiter, operator, cast target, and member identifier keeps its source span, generated source, and definition origin. A fixed limit of 256 nested expression parses rejects adversarial input with `HCPARSE0021`.

`Compiler/PrsExp.HC:PrsFunCall` walks fixed parameters in declaration order. When a parameter has `MLF_DFT_AVAILABLE`, a comma or the closing parenthesis selects its default instead of parsing an expression. This permits the documented `Test(,3);` form and corpus calls such as `PrsStmt(cc,,,cmp_flags)`. The AST records every slot as either omitted or provided, attaches each comma to the slot before it, and gives an omitted slot a zero-width insertion location. Empty `Target()` has no slots, while `Target(,)` has two omitted slots. The syntax tree therefore never conflates a hole with an explicit integer zero.

`Compiler/PrsExp.HC:PrsUnaryModifier` handles `[` as a term-level suffix, parses its contents with `PrsExpression`, requires `]`, and returns to the modifier state. This permits repeated and mixed forms such as `table[Outer(,3)][j]` and `callbacks[0](value)`. The AST stores the base, both brackets, and the complete index expression. It does not reproduce the source routine's type checks, array-dimension tracking, element-size multiplication, address addition, dereference state, or lvalue result; those depend on resolved types and lowering.

The dot and `TK_DEREFERENCE` branches of `PrsUnaryModifier` share a member-name path and return to the same modifier state. The AST therefore distinguishes `object.field` from `pointer->field` and supports chains such as `Factory().nodes[0]->callback(1).result`. It keeps the complete base expression, direct or pointer access kind, original operator, and member identifier. This syntax slice does not perform `MemberFind`, validate the base type, choose address versus value behavior, compute a member offset, apply inheritance, dereference a pointer, or classify an lvalue.

The postfix `TK_PLUS_PLUS` and `TK_MINUS_MINUS` branches in `PrsUnaryModifier` set `CCF_POSTINC` or `CCF_POSTDEC`, assign `PREC_UNARY_POST`, consume the token, and return `PE_DEREFERENCE` instead of returning to the modifier loop. `PrsExpression2` then selects `IC__PP` or `IC__MM`. The AST mirrors that boundary with a distinct postfix node containing the full operand, the update kind, and the source-positioned operator. It accepts forms such as `*ptr++`, `object.field--`, and `counter++ + 1`. A call, index, member access, or second update after `++` or `--` receives `HCPARSE0028`; the parser does not silently attach that suffix to the update result. Modifiable-lvalue checks, type rules, mutation, old-value production, and IC lowering remain semantic work.

The `(` branch in `PrsUnaryModifier` is a function call when `CCF_FUN_EXP` is set. Otherwise it reads a type with `PrsType` in `PRS0_TYPECAST` mode, emits `IC_HOLYC_TYPECAST`, and returns to the modifier loop. HolyC therefore writes `ptr(U8 *)`, not the C-style `(U8 *)ptr`; `PrsUnaryTerm` rejects the latter with “Use TempleOS postfix typecasting.” The current syntax pass recognizes all 12 checked primitive spellings with zero through four pointer stars and records the operand, both parentheses, target type, and each star. A primitive token after `(` selects a cast; other tokens continue through the call grammar. Casts may repeat or combine with calls, indexes, members, and a later terminal update. `HCPARSE0029` rejects C-style prefix casts, and `HCPARSE0030` reports a primitive cast without its closing parenthesis. Resolved class, union, array, and function-pointer cast targets, source warnings, conversion behavior, and lowering remain future work.

This slice parses but does not execute default, dimension, `_intern`, call, index, member, postfix update, or primitive cast expressions. It does not apply type or cast conversions, copy string defaults, set member flags, resolve a direct or indirect callee, check argument types or counts, validate holes against declared defaults, distinguish fixed from variadic arguments, fill omitted arguments, evaluate array sizes, validate an index or member base, scale an index, calculate member layout, dereference an element or pointer member, apply an increment or decrement, assign executable addresses, set internal-function state, or accept the separate `lastclass` default path. Calls without parentheses, resolved nonprimitive cast targets, `sizeof`, and `offset` remain outside this core expression slice and receive `HCPARSE0020` with wording for the active context. `HCPARSE0024` reports a missing comma between call arguments, `HCPARSE0025` reports an unclosed call, `HCPARSE0026` reports an unclosed index, `HCPARSE0027` reports a member operator without a following identifier, `HCPARSE0028` reports a suffix after a terminal postfix update, `HCPARSE0029` rejects C-style prefix casts, and `HCPARSE0030` reports an unclosed primitive postfix cast. Requested parameter registers, array parameters, and unbound function bodies also remain unavailable. Global initializers, parenthesized function-pointer globals, function-pointer members and locals, local and member arrays, and other global declarator forms are not stored as raw tokens. General unsupported global syntax uses `HCPARSE0001` through `HCPARSE0003`; a fifth star uses `HCPARSE0004`, a parenthesized function-pointer global uses `HCPARSE0005`, malformed function-pointer parameter punctuation uses `HCPARSE0014`, and a missing alternate target uses `HCPARSE0007`. A modifier or binding without a supported primitive type receives `HCPARSE0001` at the following token. Recovery advances to a semicolon or EOF, and the public result contains no AST after any error.

`holyc parse` and `holyc dump-ast` emit the same `holyc-ast-v1` human or JSON representation. The library exposes `parse`, `parse_with_config`, and `parse_detailed`; the detailed form keeps nonfatal preprocessor warnings beside a successful AST.

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

The implemented language specification supplies immutable primitive type facts, operator tables, compiler-option facts, and syntax for primitive globals and bound prototypes. It covers ordered declaration prefixes, ordinary, alternate-name, and `_intern` bindings, pointers through depth four, primitive global array suffixes, parameter qualifiers, recursive function-pointer parameters, varargs, and core expressions in `_intern` targets, dimensions, and parameter defaults. It enforces the source parser's AOT gate for both import spellings and accepts `_intern` in JIT and AOT modes. It does not resolve bindings, evaluate `_intern` addresses, apply internal-function or other linkage and storage effects, build semantic pointer or function types, evaluate other expressions, calculate array layout, fill call arguments, or support initializers, non-global arrays, function bodies, general expressions, statements, classes, unions, promotions, conversions, aggregate layout, floating execution, compiler-option effects, or semantic diagnostics. Those claims remain absent from the compatibility report until their own source-grounded tests pass.
