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
  ast : Ast.module_;
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
  { session; ast; declarations; function_types }

let resolve prepared mode =
  checked
    (Holyc_lib.resolve_function_identities prepared.session
       ~declarations:prepared.declarations ~functions:prepared.function_types
       ~compilation_mode:mode prepared.ast)

let classify ?compiler_option_mask prepared resolution =
  checked
    (Holyc_lib.classify_function_records ?compiler_option_mask prepared.session
       ~resolution prepared.ast)

let declarations = Semantic_function_record_classification.declarations
let identities = Semantic_function_record_classification.identities

let declaration_records classification =
  declarations classification
  |> List.map
       Semantic_function_record_classification.classified_declaration_record

let identity_records classification =
  identities classification
  |> List.map Semantic_function_record_classification.classified_identity_record

let declaration_names classification =
  declarations classification
  |> List.map (fun declaration ->
      declaration
      |> Semantic_function_record_classification.classified_declaration_source
      |> Semantic_function_resolution.resolved_declaration_site
      |> Semantic_function_resolution.declaration_site_function
      |> Semantic_function_type_resolution.function_symbol
      |> Semantic_symbol.name)

let identity_names classification =
  identities classification
  |> List.map (fun identity ->
      identity
      |> Semantic_function_record_classification.classified_identity_source
      |> Semantic_function_resolution.identity_symbol |> Semantic_symbol.name)

let hex_masks projection records =
  records |> List.map projection |> List.map (Printf.sprintf "0x%Lx")

let names projection render records =
  records |> List.map projection |> List.map render

let function_masks records =
  hex_masks Semantic_function_record_classification.function_flag_mask records

let hash_masks records =
  hex_masks Semantic_function_record_classification.hash_flag_mask records

let call_names records =
  names Semantic_function_record_classification.call_access
    Semantic_function_record_classification.call_access_name records

let hash_value_names records =
  names Semantic_function_record_classification.hash_value_access
    Semantic_function_record_classification.hash_value_access_name records

let runtime_names records =
  names Semantic_function_record_classification.runtime_lookup
    Semantic_function_record_classification.runtime_lookup_name records

let map_names records =
  names Semantic_function_record_classification.map_visibility
    Semantic_function_record_classification.map_visibility_name records

let resolution_names records =
  names Semantic_function_record_classification.aot_resolution
    Semantic_function_record_classification.aot_resolution_name records

let publication_names records =
  names Semantic_function_record_classification.aot_publication
    Semantic_function_record_classification.aot_publication_name records

let aot_binding_matrix () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"aot-function-record-flags.HC"
      "extern U0 External();\n\
       _extern _REMOTE U0 Bound();\n\
       import U0 Imported();\n\
       _import _REMOTE_IMPORT U0 Aliased();\n\
       _intern 42 U0 Internal();\n\
       U0 Defined(){}"
  in
  let classification = resolve prepared mode |> classify prepared in
  let records = declaration_records classification in
  Alcotest.(check (list string))
    "declaration order"
    [ "External"; "Bound"; "Imported"; "Aliased"; "Internal"; "Defined" ]
    (declaration_names classification);
  Alcotest.(check (list string))
    "function flags"
    [ "0x1"; "0x2000"; "0x1"; "0x1"; "0x1000"; "0x0" ]
    (function_masks records);
  Alcotest.(check (list string))
    "hash flags"
    [ "0x0"; "0x20000000"; "0x4000000"; "0x4000000"; "0x0"; "0x22000000" ]
    (hash_masks records);
  Alcotest.(check (list string))
    "call access"
    [
      "aot-extern-call";
      "direct-executable-call";
      "aot-import-call";
      "aot-import-call";
      "internal-operation";
      "direct-executable-call";
    ]
    (call_names records);
  Alcotest.(check (list string))
    "hash values"
    [
      "function-record";
      "executable-address";
      "function-record";
      "function-record";
      "executable-address";
      "executable-address";
    ]
    (hash_value_names records);
  Alcotest.(check (list string))
    "runtime lookup"
    [
      "omitted-extern";
      "visible";
      "omitted-extern";
      "omitted-extern";
      "omitted-internal";
      "visible";
    ]
    (runtime_names records);
  Alcotest.(check (list (option string)))
    "import names"
    [ None; None; Some "Imported"; Some "_REMOTE_IMPORT"; None; None ]
    (records |> List.map Semantic_function_record_classification.import_name);
  Alcotest.(check (list string))
    "AOT resolution"
    [ "none"; "resolve"; "none"; "none"; "none"; "resolve" ]
    (resolution_names records);
  Alcotest.(check (list string))
    "publication intent"
    [ "none"; "none"; "import"; "import"; "none"; "export" ]
    (publication_names records)

let jit_binding_matrix () =
  let mode = Preprocessor.Jit in
  let prepared =
    prepare ~mode ~path:"jit-function-record-flags.HC"
      "extern U0 External();\n\
       _extern _REMOTE U0 Bound();\n\
       _intern 42 U0 Internal();\n\
       U0 Defined(){}"
  in
  let classification = resolve prepared mode |> classify prepared in
  let records = declaration_records classification in
  Alcotest.(check (list string))
    "function flags"
    [ "0x1"; "0x2000"; "0x1000"; "0x0" ]
    (function_masks records);
  Alcotest.(check (list string))
    "JIT hash flags"
    [ "0x0"; "0x0"; "0x0"; "0x0" ]
    (hash_masks records);
  Alcotest.(check (list string))
    "call access"
    [
      "jit-extern-address-slot-call";
      "direct-executable-call";
      "internal-operation";
      "direct-executable-call";
    ]
    (call_names records);
  Alcotest.(check (list string))
    "JIT never publishes AOT records"
    [ "none"; "none"; "none"; "none" ]
    (publication_names records)

let joined_headers_accumulate_only_source_mutations () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"joined-function-record-flags.HC"
      "interrupt public extern U0 Carry(I64 value);\n\
       argpop extern U0 Carry(...);\n\
       U0 Carry(){}"
  in
  let classification = resolve prepared mode |> classify prepared in
  let records = declaration_records classification in
  Alcotest.(check (list string))
    "one identity is reused" [ "Carry" ]
    (identity_names classification);
  Alcotest.(check (list string))
    "stored bits accumulate, but joined staging does not transfer"
    [ "0x8901"; "0xc901"; "0xc900" ]
    (function_masks records);
  Alcotest.(check (list bool))
    "public is replaced on every header" [ true; false; false ]
    (records |> List.map Semantic_function_record_classification.is_public);
  Alcotest.(check bool)
    "joined argpop was not copied" false
    (let final = List.hd (identity_records classification) in
     Semantic_function_record_classification.Stored_flag.is_set
       ~mask:(Semantic_function_record_classification.stored_flag_mask final)
       Semantic_function_record_classification.Stored_flag.Argument_pop);
  Alcotest.(check string)
    "definition resolves the record" "0xc900"
    (identity_records classification |> function_masks |> List.hd)

let import_precedence_preserves_combined_state () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"function-import-precedence.HC"
      "U0 Same(){}\nextern U0 Same();\nimport U0 Same();\nU0 Same(){}"
  in
  let classification = resolve prepared mode |> classify prepared in
  let records = declaration_records classification in
  Alcotest.(check (list string))
    "import creates the next identity barrier" [ "Same"; "Same" ]
    (identity_names classification);
  Alcotest.(check (list string))
    "definition, extern, and import retain one resolved record"
    [ "0x0"; "0x0"; "0x0"; "0x0" ]
    (function_masks records);
  Alcotest.(check (list string))
    "hash bits accumulate"
    [ "0x22000000"; "0x22000000"; "0x26000000"; "0x22000000" ]
    (hash_masks records);
  Alcotest.(check (list string))
    "a resolved imported record still calls its executable address"
    [
      "direct-executable-call";
      "direct-executable-call";
      "direct-executable-call";
      "direct-executable-call";
    ]
    (call_names records);
  Alcotest.(check (list string))
    "import publication wins over resolve and export"
    [ "export"; "export"; "import"; "export" ]
    (publication_names records);
  Alcotest.(check (list string))
    "import suppresses the resolve branch"
    [ "resolve"; "resolve"; "shadowed-by-import"; "resolve" ]
    (resolution_names records)

let public_private_and_combined_masks () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"function-public-private.HC"
      "public static extern U0 First();\nstatic public extern U0 Second();"
  in
  let keep_private_mask, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Keep_private true
  in
  let classification =
    resolve prepared mode
    |> classify ~compiler_option_mask:keep_private_mask prepared
  in
  let records = declaration_records classification in
  Alcotest.(check (list string))
    "ordered public staging"
    [ "0x800000"; "0x1800000" ]
    (hash_masks records);
  Alcotest.(check (list string))
    "function hash type remains separate"
    [ "0x800040"; "0x1800040" ]
    (hex_masks Semantic_function_record_classification.combined_hash_mask
       records);
  Alcotest.(check (list string))
    "private records are omitted from maps"
    [ "omitted-private"; "omitted-private" ]
    (map_names records)

let per_declaration_private_state_is_sticky () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"function-private-snapshots.HC"
      "extern U0 Sticky(); extern U0 Sticky(); U0 Sticky(){}"
  in
  let resolution = resolve prepared mode in
  let keep_private_mask, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Keep_private true
  in
  let state option_mask =
    Semantic_function_record_classification.make_declaration_state
      ~staging_mask:0L ~compiler_option_mask:option_mask ()
  in
  let classification =
    checked
      (Semantic_function_record_classification.classify resolution
         [
           state Compiler_option.initial_mask;
           state keep_private_mask;
           state Compiler_option.initial_mask;
         ])
  in
  let records = declaration_records classification in
  Alcotest.(check (list string))
    "private state only accumulates"
    [ "0x0"; "0x800000"; "0x22800000" ]
    (hash_masks records);
  Alcotest.(check bool)
    "final identity stays private" true
    (identity_records classification
    |> List.hd |> Semantic_function_record_classification.is_private)

let invalid_inputs_are_pure () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"invalid-function-record-flags.HC"
      "extern U0 First(); import U0 Second();"
  in
  let resolution = resolve prepared mode in
  let table = Session.semantic_symbols prepared.session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let state ?import_name staging_mask =
    Semantic_function_record_classification.make_declaration_state ~staging_mask
      ~compiler_option_mask:Compiler_option.initial_mask ?import_name ()
  in
  Alcotest.(check bool)
    "too few states are rejected" true
    (Semantic_function_record_classification.classify resolution [ state 0L ]
    |> Result.is_error);
  Alcotest.(check bool)
    "missing import name is rejected" true
    (Semantic_function_record_classification.classify resolution
       [ state 0L; state 0L ]
    |> Result.is_error);
  Alcotest.(check bool)
    "unknown staging bits are rejected" true
    (Semantic_function_record_classification.classify resolution
       [ state 0x100000L; state ~import_name:"Second" 0L ]
    |> Result.is_error);
  let unrelated =
    prepare ~mode ~path:"unrelated-function-record-flags.HC" "U0 Other(){}"
  in
  Alcotest.(check bool)
    "unrelated AST is rejected" true
    (Holyc_lib.classify_function_records prepared.session ~resolution
       unrelated.ast
    |> Result.is_error);
  Alcotest.(check int)
    "scope count unchanged" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "symbol count unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let deterministic_classification () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"deterministic-function-record-flags.HC"
      "public extern U0 Stable(I64 value); U0 Stable(I64 value){}"
  in
  let resolution = resolve prepared mode in
  let first = classify prepared resolution |> declaration_records in
  let second = classify prepared resolution |> declaration_records in
  Alcotest.(check (list string))
    "function masks" (function_masks first) (function_masks second);
  Alcotest.(check (list string))
    "hash masks" (hash_masks first) (hash_masks second);
  Alcotest.(check (list string))
    "call access" (call_names first) (call_names second)

let tests =
  [
    Alcotest.test_case "AOT binding matrix" `Quick aot_binding_matrix;
    Alcotest.test_case "JIT binding matrix" `Quick jit_binding_matrix;
    Alcotest.test_case "joined header accumulation" `Quick
      joined_headers_accumulate_only_source_mutations;
    Alcotest.test_case "import publication precedence" `Quick
      import_precedence_preserves_combined_state;
    Alcotest.test_case "public private and combined masks" `Quick
      public_private_and_combined_masks;
    Alcotest.test_case "per-declaration private snapshots" `Quick
      per_declaration_private_state_is_sticky;
    Alcotest.test_case "invalid inputs preserve semantic state" `Quick
      invalid_inputs_are_pure;
    Alcotest.test_case "deterministic classification" `Quick
      deterministic_classification;
  ]
