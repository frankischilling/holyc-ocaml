open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %s"
        (diagnostics
        |> List.map (fun diagnostic ->
            Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
              diagnostic.message)
        |> String.concat ", ")

let config mode = checked (Preprocessor.Config.create ~compilation_mode:mode ())

type prepared = {
  session : Session.t;
  declarations : Semantic_declaration_collection.t;
  function_types : Semantic_function_type_resolution.t;
}

let prepare ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let function_types =
    checked
      (Holyc_lib.resolve_function_types session ~declarations ~aggregates
         ~functions ast)
  in
  { session; declarations; function_types }

let semantic_mode = function
  | Preprocessor.Jit -> Semantic_function_resolution.Jit
  | Preprocessor.Aot -> Semantic_function_resolution.Aot

let facts prepared kinds =
  let functions =
    Semantic_function_type_resolution.functions prepared.function_types
  in
  if List.length functions <> List.length kinds then
    Alcotest.fail "test declaration kinds do not match the function batch";
  List.map2
    (fun function_ kind ->
      checked (Semantic_function_resolution.make_declaration ~function_ ~kind))
    functions kinds

let resolve prepared mode kinds =
  checked
    (Semantic_function_resolution.resolve
       ~table:(Session.semantic_symbols prepared.session)
       ~parent:(Semantic_declaration_collection.scope prepared.declarations)
       ~compilation_mode:(semantic_mode mode) (facts prepared kinds))

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let identity_ids resolution =
  Semantic_function_resolution.identities resolution
  |> List.map (fun identity ->
      Semantic_function_resolution.identity_symbol identity |> symbol_id)

let declaration_identity_ids resolution =
  Semantic_function_resolution.declarations resolution
  |> List.map (fun declaration ->
      Semantic_function_resolution.resolved_declaration_identity_symbol
        declaration
      |> symbol_id)

let site_item_index site =
  site |> Semantic_function_resolution.declaration_site_function
  |> Semantic_function_type_resolution.function_item_index

let replaced_item_indexes resolution =
  Semantic_function_resolution.declarations resolution
  |> List.map (fun declaration ->
      Semantic_function_resolution.resolved_declaration_replaced_header
        declaration
      |> Option.map site_item_index)

let final_states resolution =
  Semantic_function_resolution.identities resolution
  |> List.map (fun identity ->
      Semantic_function_resolution.identity_state identity
      |> Semantic_function_resolution.state_name)

let jit_join_and_shadow_matrix () =
  let mode = Preprocessor.Jit in
  let prepared =
    prepare ~mode ~path:"jit-function-identities.HC"
      "extern U0 Work(I64 first);\n\
       extern U0 Work(I64 second);\n\
       U0 Work(I64 third){}\n\
       U0 Work(I64 fourth){}\n\
       extern U0 Work(I64 fifth);\n\
       U0 Work(I64 sixth){}"
  in
  let resolution =
    resolve prepared mode
      [
        Semantic_function_resolution.Extern;
        Extern;
        Definition;
        Definition;
        Extern;
        Definition;
      ]
  in
  let ids = identity_ids resolution in
  Alcotest.(check int) "three JIT identities" 3 (List.length ids);
  Alcotest.(check (list int))
    "JIT joins only the newest unresolved extern"
    [
      List.nth ids 0;
      List.nth ids 0;
      List.nth ids 0;
      List.nth ids 1;
      List.nth ids 2;
      List.nth ids 2;
    ]
    (declaration_identity_ids resolution);
  Alcotest.(check (list (option int)))
    "joined declarations retain the replaced header"
    [ None; Some 0; Some 1; None; None; Some 4 ]
    (replaced_item_indexes resolution);
  Alcotest.(check (list string))
    "each JIT identity ends resolved"
    [ "resolved"; "resolved"; "resolved" ]
    (final_states resolution);
  Alcotest.(check string)
    "mode is explicit" "jit"
    (Semantic_function_resolution.compilation_mode resolution
    |> Semantic_function_resolution.compilation_mode_name)

let aot_import_barrier_matrix () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"aot-function-identities.HC"
      "extern U0 Work(I64 first);\n\
       U0 Work(I64 second){}\n\
       U0 Work(I64 third){}\n\
       import U0 Work(I64 fourth);\n\
       U0 Work(I64 fifth){}\n\
       import U0 Work(I64 sixth);\n\
       import U0 Work(I64 seventh);"
  in
  let resolution =
    resolve prepared mode
      [
        Semantic_function_resolution.Extern;
        Definition;
        Definition;
        Import;
        Definition;
        Import;
        Import;
      ]
  in
  let ids = identity_ids resolution in
  Alcotest.(check int) "three AOT identities" 3 (List.length ids);
  Alcotest.(check (list int))
    "AOT joins nonimports and starts after each import barrier"
    [
      List.nth ids 0;
      List.nth ids 0;
      List.nth ids 0;
      List.nth ids 0;
      List.nth ids 1;
      List.nth ids 1;
      List.nth ids 2;
    ]
    (declaration_identity_ids resolution);
  Alcotest.(check (list (option int)))
    "AOT replacement chain follows source order"
    [ None; Some 0; Some 1; Some 2; None; Some 4; None ]
    (replaced_item_indexes resolution);
  Alcotest.(check (list string))
    "each final AOT header is imported"
    [ "imported"; "imported"; "imported" ]
    (final_states resolution)

let explicit_bindings_resolve () =
  let prepared =
    prepare ~path:"explicit-function-bindings.HC" "U0 Bound(){} U0 Internal(){}"
  in
  let resolution =
    resolve prepared Preprocessor.Jit
      [
        Semantic_function_resolution.Bound_extern;
        Semantic_function_resolution.Intern;
      ]
  in
  Alcotest.(check (list string))
    "bound extern and intern are resolved" [ "resolved"; "resolved" ]
    (final_states resolution);
  Alcotest.(check (list string))
    "declaration spellings remain distinct"
    [ "bound-extern"; "intern" ]
    (Semantic_function_resolution.declarations resolution
    |> List.map Semantic_function_resolution.resolved_declaration_site
    |> List.map Semantic_function_resolution.declaration_site_kind
    |> List.map Semantic_function_resolution.declaration_kind_name)

let invalid_batches_do_not_mutate () =
  let prepared =
    prepare ~path:"invalid-function-identities.HC"
      "extern U0 First(); extern U0 Second();"
  in
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  let functions =
    Semantic_function_type_resolution.functions prepared.function_types
  in
  let first = List.nth functions 0 in
  let second = List.nth functions 1 in
  let fact function_ =
    checked
      (Semantic_function_resolution.make_declaration ~function_
         ~kind:Semantic_function_resolution.Extern)
  in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let reject label mode declarations =
    Alcotest.(check bool)
      label true
      (Semantic_function_resolution.resolve ~table ~parent
         ~compilation_mode:mode declarations
      |> Result.is_error);
    Alcotest.(check int)
      (label ^ " preserves scopes")
      scope_count
      (Semantic_symbol_table.all_scopes table |> List.length);
    Alcotest.(check int)
      (label ^ " preserves symbols")
      symbol_count
      (Semantic_symbol_table.all_symbols table |> List.length)
  in
  reject "reversed source order" Semantic_function_resolution.Jit
    [ fact second; fact first ];
  reject "repeated declaration symbol" Semantic_function_resolution.Jit
    [ fact first; fact first ];
  reject "JIT import" Semantic_function_resolution.Jit
    [
      checked
        (Semantic_function_resolution.make_declaration ~function_:first
           ~kind:Semantic_function_resolution.Import);
    ];
  let other = prepare ~path:"foreign-function-identities.HC" "U0 Foreign(){}" in
  let foreign =
    Semantic_function_type_resolution.functions other.function_types |> List.hd
  in
  reject "foreign symbol table" Semantic_function_resolution.Jit
    [ fact foreign ]

let deterministic_resolution () =
  let prepared =
    prepare ~path:"deterministic-function-identities.HC"
      "extern U0 Stable(I64 old); U0 Stable(I64 current){}"
  in
  let kinds =
    [
      Semantic_function_resolution.Extern;
      Semantic_function_resolution.Definition;
    ]
  in
  let first = resolve prepared Preprocessor.Jit kinds in
  let second = resolve prepared Preprocessor.Jit kinds in
  Alcotest.(check (list int))
    "identity mapping is deterministic"
    (declaration_identity_ids first)
    (declaration_identity_ids second);
  Alcotest.(check (list (option int)))
    "replacement mapping is deterministic"
    (replaced_item_indexes first)
    (replaced_item_indexes second)

let tests =
  [
    Alcotest.test_case "JIT join and shadow matrix" `Quick
      jit_join_and_shadow_matrix;
    Alcotest.test_case "AOT import barrier matrix" `Quick
      aot_import_barrier_matrix;
    Alcotest.test_case "explicit bindings resolve" `Quick
      explicit_bindings_resolve;
    Alcotest.test_case "invalid batches do not mutate" `Quick
      invalid_batches_do_not_mutate;
    Alcotest.test_case "deterministic resolution" `Quick
      deterministic_resolution;
  ]
