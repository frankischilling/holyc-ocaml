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

let externs_to_imports_mask =
  Compiler_option.set ~mask:Compiler_option.initial_mask
    Compiler_option.Externs_to_imports true
  |> fst

type prepared = {
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  function_types : Semantic_function_type_resolution.t;
  global_types : Semantic_global_type_resolution.t;
}

let prepare ~mode ~path contents =
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
  let global_types =
    checked
      (Holyc_lib.resolve_global_types session ~declarations ~aggregates ast)
  in
  { session; ast; declarations; function_types; global_types }

let resolve_functions ?compiler_option_mask prepared mode =
  checked
    (Holyc_lib.resolve_function_identities ?compiler_option_mask
       prepared.session ~declarations:prepared.declarations
       ~functions:prepared.function_types ~compilation_mode:mode prepared.ast)

let resolve_globals ?compiler_option_mask prepared mode =
  checked
    (Holyc_lib.resolve_global_records ?compiler_option_mask prepared.session
       ~declarations:prepared.declarations ~globals:prepared.global_types
       ~compilation_mode:mode prepared.ast)

let function_sites resolution =
  Semantic_function_resolution.declarations resolution
  |> List.map Semantic_function_resolution.resolved_declaration_site

let kind_names projection values name =
  values |> List.map projection |> List.map name

let function_kind_names projection sites =
  kind_names projection sites Semantic_function_resolution.declaration_kind_name

let global_kind_names projection records =
  kind_names projection records Semantic_global_resolution.declaration_kind_name

let function_identity_ids resolution =
  Semantic_function_resolution.declarations resolution
  |> List.map (fun declaration ->
      declaration
      |> Semantic_function_resolution.resolved_declaration_identity_symbol
      |> Semantic_symbol.id |> Semantic_symbol.Id.to_int)

let function_records classification =
  Semantic_function_record_classification.declarations classification
  |> List.map
       Semantic_function_record_classification.classified_declaration_record

let global_records = Semantic_global_resolution.records

let global_record_index records symbol =
  let wanted = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int in
  let rec find index = function
    | [] -> Alcotest.fail "global alias target is absent from its record batch"
    | record :: rest ->
        let current =
          record |> Semantic_global_resolution.global_record_symbol
          |> Semantic_symbol.id |> Semantic_symbol.Id.to_int
        in
        if current = wanted then index else find (index + 1) rest
  in
  find 0 records

let global_alias_indexes resolution =
  let records = global_records resolution in
  records
  |> List.map (fun record ->
      Semantic_global_resolution.global_record_alias_target record
      |> Option.map (global_record_index records))

let function_rewrite_precedes_identity_resolution () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"function-externs-to-imports.HC"
      "extern U0 Same();\n\
       U0 Same(){}\n\
       _extern REMOTE_FUN U0 Bound();\n\
       import U0 Direct();\n\
       _import REMOTE_DIRECT U0 Alternate();\n\
       _intern 7 U0 Internal();"
  in
  let resolution =
    resolve_functions ~compiler_option_mask:externs_to_imports_mask prepared
      mode
  in
  let replay =
    resolve_functions ~compiler_option_mask:externs_to_imports_mask prepared
      mode
  in
  let sites = function_sites resolution in
  Alcotest.(check (list string))
    "source kinds remain inspectable"
    [ "extern"; "definition"; "bound-extern"; "import"; "import"; "intern" ]
    (function_kind_names
       Semantic_function_resolution.declaration_site_source_kind sites);
  Alcotest.(check (list string))
    "extern forms become imports before joining"
    [ "import"; "definition"; "import"; "import"; "import"; "intern" ]
    (function_kind_names Semantic_function_resolution.declaration_site_kind
       sites);
  let identities = function_identity_ids resolution in
  Alcotest.(check bool)
    "converted import is an AOT identity barrier" true
    (List.nth identities 0 <> List.nth identities 1);
  Alcotest.(check (list int))
    "function rewrite is deterministic" identities
    (function_identity_ids replay);
  Alcotest.(check (list bool))
    "each site retains the enabled option snapshot"
    [ true; true; true; true; true; true ]
    (sites
    |> List.map
         Semantic_function_resolution.declaration_site_compiler_option_mask
    |> List.map (fun mask ->
        Compiler_option.is_enabled ~mask Compiler_option.Externs_to_imports));
  let classification =
    checked
      (Holyc_lib.classify_function_records prepared.session ~resolution
         prepared.ast)
  in
  Alcotest.(check (list (option string)))
    "loader names use local and alternate spellings"
    [
      Some "Same";
      None;
      Some "REMOTE_FUN";
      Some "Direct";
      Some "REMOTE_DIRECT";
      None;
    ]
    (function_records classification
    |> List.map Semantic_function_record_classification.import_name);
  Alcotest.(check (list string))
    "converted records use import call policy"
    [
      "aot-import-call";
      "direct-executable-call";
      "aot-import-call";
      "aot-import-call";
      "aot-import-call";
      "internal-operation";
    ]
    (function_records classification
    |> List.map Semantic_function_record_classification.call_access
    |> List.map Semantic_function_record_classification.call_access_name);
  Alcotest.(check bool)
    "a contradictory classifier snapshot is rejected" true
    (Holyc_lib.classify_function_records
       ~compiler_option_mask:Compiler_option.initial_mask prepared.session
       ~resolution prepared.ast
    |> Result.is_error)

let global_rewrite_retains_aot_aliasing () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"global-externs-to-imports.HC"
      "extern I64 Same;\n\
       I64 Same;\n\
       _extern REMOTE_GLOBAL I64 Bound;\n\
       I64 Bound;\n\
       import I64 Direct;\n\
       _import REMOTE_DIRECT I64 Alternate;\n\
       _intern 7 I64 Internal;\n\
       I64 Defined;"
  in
  let resolution =
    resolve_globals ~compiler_option_mask:externs_to_imports_mask prepared mode
  in
  let replay =
    resolve_globals ~compiler_option_mask:externs_to_imports_mask prepared mode
  in
  let records = global_records resolution in
  Alcotest.(check (list string))
    "global source kinds remain inspectable"
    [
      "extern";
      "definition";
      "alternate-extern";
      "definition";
      "import";
      "alternate-import";
      "intern";
      "definition";
    ]
    (global_kind_names Semantic_global_resolution.global_record_source_kind
       records);
  Alcotest.(check (list string))
    "global extern forms become imports"
    [
      "import";
      "definition";
      "alternate-import";
      "definition";
      "import";
      "alternate-import";
      "intern";
      "definition";
    ]
    (global_kind_names Semantic_global_resolution.global_record_kind records);
  Alcotest.(check (list (option int)))
    "AOT definitions still redirect their immediate prior records"
    [ Some 1; None; Some 3; None; None; None; None; None ]
    (global_alias_indexes resolution);
  Alcotest.(check (list (option int)))
    "global rewrite is deterministic"
    (global_alias_indexes resolution)
    (global_alias_indexes replay);
  let classification =
    checked
      (Holyc_lib.classify_global_records prepared.session ~resolution
         prepared.ast)
  in
  let classified =
    Semantic_global_record_classification.records classification
  in
  Alcotest.(check (list (option string)))
    "global loader names retain exact spellings"
    [
      Some "Same";
      None;
      Some "REMOTE_GLOBAL";
      None;
      Some "Direct";
      Some "REMOTE_DIRECT";
      None;
      None;
    ]
    (classified |> List.map Semantic_global_record_classification.import_name);
  Alcotest.(check (list string))
    "converted records use import reference policy"
    [
      "aot-import-reference";
      "aot-code-heap-reference";
      "aot-import-reference";
      "aot-code-heap-reference";
      "aot-import-reference";
      "aot-import-reference";
      "aot-code-heap-reference";
      "aot-code-heap-reference";
    ]
    (classified
    |> List.map Semantic_global_record_classification.value_access
    |> List.map Semantic_global_record_classification.value_access_name);
  Alcotest.(check bool)
    "a contradictory global classifier snapshot is rejected" true
    (Holyc_lib.classify_global_records
       ~compiler_option_mask:Compiler_option.initial_mask prepared.session
       ~resolution prepared.ast
    |> Result.is_error)

let disabled_option_preserves_extern_kinds () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"disabled-externs-to-imports.HC"
      "extern U0 PlainFun(); _extern REMOTE_FUN U0 BoundFun();\n\
       extern I64 PlainGlobal; _extern REMOTE_GLOBAL I64 BoundGlobal;"
  in
  let function_sites = resolve_functions prepared mode |> function_sites in
  Alcotest.(check (list string))
    "function extern kinds are unchanged"
    [ "extern"; "bound-extern" ]
    (function_kind_names Semantic_function_resolution.declaration_site_kind
       function_sites);
  let globals = resolve_globals prepared mode |> global_records in
  Alcotest.(check (list string))
    "global extern kinds are unchanged"
    [ "extern"; "alternate-extern" ]
    (global_kind_names Semantic_global_resolution.global_record_kind globals)

let converted_imports_are_rejected_in_jit () =
  let mode = Preprocessor.Jit in
  let prepared =
    prepare ~mode ~path:"jit-externs-to-imports.HC"
      "extern U0 ImportedFun(); extern I64 ImportedGlobal;"
  in
  let table = Session.semantic_symbols prepared.session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (result reject string))
    "function rewrite uses the import mode guard"
    (Error "semantic function imports require AOT compilation mode")
    (Holyc_lib.resolve_function_identities
       ~compiler_option_mask:externs_to_imports_mask prepared.session
       ~declarations:prepared.declarations ~functions:prepared.function_types
       ~compilation_mode:mode prepared.ast);
  Alcotest.(check (result reject string))
    "global rewrite uses the import mode guard"
    (Error "semantic global imports require AOT compilation mode")
    (Holyc_lib.resolve_global_records
       ~compiler_option_mask:externs_to_imports_mask prepared.session
       ~declarations:prepared.declarations ~globals:prepared.global_types
       ~compilation_mode:mode prepared.ast);
  Alcotest.(check int)
    "failed rewrites preserve scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "failed rewrites preserve symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let low_level_snapshots_and_aggregate_forwards () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"per-declaration-externs-to-imports.HC"
      "extern class Forward; extern union Overlay;\n\
       extern U0 FirstFun(); extern U0 SecondFun();\n\
       extern I64 FirstGlobal; extern I64 SecondGlobal;"
  in
  let forwards =
    prepared.ast.items
    |> List.filter_map (function
      | Ast.Aggregate_forward_declaration declaration ->
          Some (declaration.name.spelling, declaration.binding.kind)
      | Ast.Aggregate_definition _
      | Ast.Global_variable _
      | Ast.Global_declaration _
      | Ast.Function_prototype _
      | Ast.Function_definition _
      | Ast.Top_level_statement _ -> None)
  in
  Alcotest.(check (list string))
    "extern aggregate forwards bypass the option rewrite"
    [ "Forward"; "Overlay" ]
    (forwards
    |> List.filter_map (fun (name, kind) ->
        match kind with
        | Ast.Extern -> Some name
        | Ast.Import | Ast.Intern -> None));
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  let functions =
    Semantic_function_type_resolution.functions prepared.function_types
  in
  let function_declarations =
    [
      checked
        (Semantic_function_resolution.make_declaration_with_options
           ~compiler_option_mask:externs_to_imports_mask
           ~function_:(List.nth functions 0)
           ~kind:Semantic_function_resolution.Extern);
      checked
        (Semantic_function_resolution.make_declaration
           ~function_:(List.nth functions 1)
           ~kind:Semantic_function_resolution.Extern);
    ]
  in
  let function_resolution =
    checked
      (Semantic_function_resolution.resolve ~table ~parent
         ~compilation_mode:Semantic_function_resolution.Aot
         function_declarations)
  in
  Alcotest.(check (list string))
    "function snapshots are declaration-local" [ "import"; "extern" ]
    (function_resolution |> function_sites
    |> function_kind_names Semantic_function_resolution.declaration_site_kind);
  let default_globals = resolve_globals prepared mode |> global_records in
  let global_declarations =
    default_globals
    |> List.mapi (fun index record ->
        let declaration =
          Semantic_global_resolution.global_record_declaration record
        in
        let compiler_option_mask =
          if index = 0 then externs_to_imports_mask
          else Compiler_option.initial_mask
        in
        checked
          (Semantic_global_resolution.make_declaration ~compiler_option_mask
             ~global:(Semantic_global_resolution.declaration_global declaration)
             ~storage:
               (Semantic_global_resolution.declaration_storage declaration)
             ?binding:
               (Semantic_global_resolution.declaration_binding declaration)
             ()))
  in
  let global_resolution =
    checked
      (Semantic_global_resolution.resolve ~table ~parent
         ~compilation_mode:Semantic_global_resolution.Aot global_declarations)
  in
  Alcotest.(check (list string))
    "global snapshots are declaration-local" [ "import"; "extern" ]
    (global_resolution |> global_records
    |> global_kind_names Semantic_global_resolution.global_record_kind)

let tests =
  [
    Alcotest.test_case "function rewrite precedes identity resolution" `Quick
      function_rewrite_precedes_identity_resolution;
    Alcotest.test_case "global rewrite retains AOT aliasing" `Quick
      global_rewrite_retains_aot_aliasing;
    Alcotest.test_case "disabled option preserves extern kinds" `Quick
      disabled_option_preserves_extern_kinds;
    Alcotest.test_case "converted imports are rejected in JIT" `Quick
      converted_imports_are_rejected_in_jit;
    Alcotest.test_case "local snapshots and aggregate forwards" `Quick
      low_level_snapshots_and_aggregate_forwards;
  ]
