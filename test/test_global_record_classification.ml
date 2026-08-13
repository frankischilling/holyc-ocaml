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
  globals : Semantic_global_type_resolution.t;
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
  let globals =
    checked
      (Holyc_lib.resolve_global_types session ~declarations ~aggregates ast)
  in
  { session; ast; declarations; globals }

let resolve prepared mode =
  checked
    (Holyc_lib.resolve_global_records prepared.session
       ~declarations:prepared.declarations ~globals:prepared.globals
       ~compilation_mode:mode prepared.ast)

let classify ?compiler_option_mask prepared resolution =
  checked
    (Holyc_lib.classify_global_records ?compiler_option_mask prepared.session
       ~resolution prepared.ast)

let records = Semantic_global_record_classification.records

let names classification =
  records classification
  |> List.map Semantic_global_record_classification.classified_record_source
  |> List.map Semantic_global_resolution.global_record_symbol
  |> List.map Semantic_symbol.name

let hex_masks projection classification =
  records classification |> List.map projection
  |> List.map (Printf.sprintf "0x%Lx")

let access_names classification =
  records classification
  |> List.map Semantic_global_record_classification.value_access
  |> List.map Semantic_global_record_classification.value_access_name

let cleanup_names classification =
  records classification
  |> List.map Semantic_global_record_classification.cleanup
  |> List.map Semantic_global_record_classification.cleanup_name

let map_names classification =
  records classification
  |> List.map Semantic_global_record_classification.map_visibility
  |> List.map Semantic_global_record_classification.map_visibility_name

let publication_names classification =
  records classification
  |> List.map Semantic_global_record_classification.aot_publication
  |> List.map Semantic_global_record_classification.aot_publication_name

let aot_binding_and_modifier_matrix () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"aot-global-flags.HC"
      "I64 Definition;\n\
       public I64 PublicDefinition;\n\
       public static I64 PublicThenStatic;\n\
       static public I64 StaticThenPublic;\n\
       extern I64 External;\n\
       _extern REMOTE I64 Bound;\n\
       import I64 Imported;\n\
       _import REMOTE_IMPORT I64 Aliased;\n\
       _intern 42 I64 Internal;\n\
       I64 Array[2];\n\
       U0 (*Callback)(I64 value);"
  in
  let classification = resolve prepared mode |> classify prepared in
  Alcotest.(check (list string))
    "record order"
    [
      "Definition";
      "PublicDefinition";
      "PublicThenStatic";
      "StaticThenPublic";
      "External";
      "Bound";
      "Imported";
      "Aliased";
      "Internal";
      "Array";
      "Callback";
    ]
    (names classification);
  Alcotest.(check (list string))
    "hash flags"
    [
      "0x2000000";
      "0x3000000";
      "0x2000000";
      "0x3000000";
      "0x0";
      "0x2000000";
      "0x4000000";
      "0x4000000";
      "0x2000000";
      "0x2000000";
      "0x2000000";
    ]
    (hex_masks Semantic_global_record_classification.hash_flag_mask
       classification);
  Alcotest.(check (list string))
    "global flags"
    [
      "0x0";
      "0x0";
      "0x0";
      "0x0";
      "0x4";
      "0x10";
      "0x2";
      "0x2";
      "0x0";
      "0x20";
      "0x1";
    ]
    (hex_masks Semantic_global_record_classification.global_flag_mask
       classification);
  Alcotest.(check (list string))
    "value access"
    [
      "aot-code-heap-reference";
      "aot-code-heap-reference";
      "aot-code-heap-reference";
      "aot-code-heap-reference";
      "aot-extern-unimplemented";
      "aot-code-heap-reference";
      "aot-import-reference";
      "aot-import-reference";
      "aot-code-heap-reference";
      "aot-code-heap-reference";
      "aot-code-heap-reference";
    ]
    (access_names classification);
  Alcotest.(check (list (option string)))
    "import names"
    [
      None;
      None;
      None;
      None;
      None;
      None;
      Some "Imported";
      Some "REMOTE_IMPORT";
      None;
      None;
      None;
    ]
    (records classification
    |> List.map Semantic_global_record_classification.import_name);
  Alcotest.(check (list string))
    "publication intent"
    [
      "export";
      "export";
      "export";
      "export";
      "none";
      "export";
      "import";
      "import";
      "export";
      "export";
      "export";
    ]
    (publication_names classification)

let jit_binding_matrix () =
  let mode = Preprocessor.Jit in
  let prepared =
    prepare ~mode ~path:"jit-global-flags.HC"
      "I64 Definition;\n\
       extern I64 External;\n\
       _extern REMOTE I64 Bound;\n\
       _intern 42 I64 Internal;\n\
       I64 Array[2];\n\
       U0 (*Callback)();"
  in
  let classification = resolve prepared mode |> classify prepared in
  Alcotest.(check (list string))
    "hash flags"
    [ "0x0"; "0x40000000"; "0x0"; "0x0"; "0x0"; "0x0" ]
    (hex_masks Semantic_global_record_classification.hash_flag_mask
       classification);
  Alcotest.(check (list string))
    "global flags"
    [ "0x0"; "0x4"; "0x10"; "0x0"; "0x20"; "0x1" ]
    (hex_masks Semantic_global_record_classification.global_flag_mask
       classification);
  Alcotest.(check (list string))
    "value access"
    [
      "jit-direct-address";
      "jit-extern-address-slot";
      "jit-direct-address";
      "jit-direct-address";
      "jit-direct-address";
      "jit-direct-address";
    ]
    (access_names classification);
  Alcotest.(check (list string))
    "JIT never emits AOT records"
    [ "none"; "none"; "none"; "none"; "none"; "none" ]
    (publication_names classification)

let explicit_storage_and_private_state () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"global-storage-flags.HC"
      "I64 Code; public I64 Heap;"
  in
  let globals = Semantic_global_type_resolution.globals prepared.globals in
  let declaration global storage =
    checked (Semantic_global_resolution.make_declaration ~global ~storage ())
  in
  let resolution =
    checked
      (Semantic_global_resolution.resolve
         ~table:(Session.semantic_symbols prepared.session)
         ~parent:(Semantic_declaration_collection.scope prepared.declarations)
         ~compilation_mode:Semantic_global_resolution.Aot
         [
           declaration (List.nth globals 0) Semantic_global_resolution.Code_heap;
           declaration (List.nth globals 1) Semantic_global_resolution.Data_heap;
         ])
  in
  let keep_private_mask, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Keep_private true
  in
  let public_mask =
    Function_flag.apply_modifier ~mask:0L Function_flag.Modifier.Public
  in
  let states =
    [
      Semantic_global_record_classification.make_record_state ~staging_mask:0L
        ~compiler_option_mask:Compiler_option.initial_mask;
      Semantic_global_record_classification.make_record_state
        ~staging_mask:public_mask ~compiler_option_mask:keep_private_mask;
    ]
  in
  let classification =
    checked (Semantic_global_record_classification.classify resolution states)
  in
  Alcotest.(check (list string))
    "hash flags"
    [ "0x2000000"; "0x1800000" ]
    (hex_masks Semantic_global_record_classification.hash_flag_mask
       classification);
  Alcotest.(check (list string))
    "combined hash type"
    [ "0x2000008"; "0x1800008" ]
    (hex_masks Semantic_global_record_classification.combined_hash_mask
       classification);
  Alcotest.(check (list string))
    "global flags" [ "0x0"; "0x8" ]
    (hex_masks Semantic_global_record_classification.global_flag_mask
       classification);
  Alcotest.(check (list string))
    "value access"
    [ "aot-code-heap-reference"; "aot-data-heap-reference" ]
    (access_names classification);
  Alcotest.(check (list string))
    "map visibility"
    [ "visible"; "omitted-private" ]
    (map_names classification);
  Alcotest.(check (list string))
    "publication" [ "export"; "none" ]
    (publication_names classification);
  let heap_state =
    List.nth (records classification) 1
    |> Semantic_global_record_classification.classified_record_state
  in
  Alcotest.(check int64)
    "option snapshot" keep_private_mask
    (Semantic_global_record_classification.record_state_compiler_option_mask
       heap_state)

let import_private_map_combination () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"private-import.HC"
      "import I64 Imported;"
  in
  let keep_private_mask, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Keep_private true
  in
  let classification =
    resolve prepared Preprocessor.Aot
    |> classify ~compiler_option_mask:keep_private_mask prepared
  in
  Alcotest.(check (list string))
    "hash flags" [ "0x4800000" ]
    (hex_masks Semantic_global_record_classification.hash_flag_mask
       classification);
  Alcotest.(check (list string))
    "both map filters"
    [ "omitted-import-and-private" ]
    (map_names classification)

let alias_ownership_follows_both_source_paths () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"global-alias-flags.HC"
      "extern I64 Same; I64 Same; _extern REMOTE I64 Bound;"
  in
  let classification = resolve prepared Preprocessor.Aot |> classify prepared in
  Alcotest.(check (list string))
    "global flags" [ "0x14"; "0x0"; "0x10" ]
    (hex_masks Semantic_global_record_classification.global_flag_mask
       classification);
  Alcotest.(check (list string))
    "cleanup ownership"
    [
      "preserve-aliased-data-address";
      "free-data-address";
      "preserve-aliased-data-address";
    ]
    (cleanup_names classification);
  Alcotest.(check (list string))
    "hash flags"
    [ "0x0"; "0x2000000"; "0x2000000" ]
    (hex_masks Semantic_global_record_classification.hash_flag_mask
       classification)

let grouped_and_attached_modifiers_keep_source_order () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"grouped-global-flags.HC"
      "public I64 first,*second;\n\
       public class Pair { I64 value; } third,fourth;"
  in
  let classification = resolve prepared Preprocessor.Aot |> classify prepared in
  Alcotest.(check (list string))
    "record order"
    [ "first"; "second"; "third"; "fourth" ]
    (names classification);
  Alcotest.(check (list bool))
    "group modifiers apply to every record" [ true; true; true; true ]
    (records classification
    |> List.map Semantic_global_record_classification.is_public)

let invalid_inputs_are_pure () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"invalid-global-flags.HC"
      "I64 First; I64 Second;"
  in
  let resolution = resolve prepared Preprocessor.Aot in
  let table = Session.semantic_symbols prepared.session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let one_state =
    [
      Semantic_global_record_classification.make_record_state ~staging_mask:0L
        ~compiler_option_mask:Compiler_option.initial_mask;
    ]
  in
  Alcotest.(check bool)
    "too few states are rejected" true
    (Semantic_global_record_classification.classify resolution one_state
    |> Result.is_error);
  let unrelated = prepare ~path:"unrelated-global-flags.HC" "I64 Other;" in
  Alcotest.(check bool)
    "unrelated AST is rejected" true
    (Holyc_lib.classify_global_records prepared.session ~resolution
       unrelated.ast
    |> Result.is_error);
  Alcotest.(check int)
    "scope count unchanged" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "symbol count unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let deterministic_classification () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"deterministic-global-flags.HC"
      "public extern I64 Stable; I64 Stable;"
  in
  let resolution = resolve prepared Preprocessor.Aot in
  let first = classify prepared resolution in
  let second = classify prepared resolution in
  Alcotest.(check (list string))
    "hash masks"
    (hex_masks Semantic_global_record_classification.hash_flag_mask first)
    (hex_masks Semantic_global_record_classification.hash_flag_mask second);
  Alcotest.(check (list string))
    "global masks"
    (hex_masks Semantic_global_record_classification.global_flag_mask first)
    (hex_masks Semantic_global_record_classification.global_flag_mask second)

let tests =
  [
    Alcotest.test_case "AOT binding and modifier matrix" `Quick
      aot_binding_and_modifier_matrix;
    Alcotest.test_case "JIT binding matrix" `Quick jit_binding_matrix;
    Alcotest.test_case "storage and private snapshots" `Quick
      explicit_storage_and_private_state;
    Alcotest.test_case "private import map state" `Quick
      import_private_map_combination;
    Alcotest.test_case "alias ownership" `Quick
      alias_ownership_follows_both_source_paths;
    Alcotest.test_case "grouped and attached modifiers" `Quick
      grouped_and_attached_modifiers_keep_source_order;
    Alcotest.test_case "invalid inputs preserve semantic state" `Quick
      invalid_inputs_are_pure;
    Alcotest.test_case "deterministic classification" `Quick
      deterministic_classification;
  ]
