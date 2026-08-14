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
  let globals =
    checked
      (Holyc_lib.resolve_global_types session ~declarations ~aggregates ast)
  in
  { session; ast; declarations; globals }

let enable mask option = Compiler_option.set ~mask option true |> fst

let data_heap_mask =
  enable Compiler_option.initial_mask Compiler_option.Globals_on_data_heap

let data_heap_and_import_mask =
  enable data_heap_mask Compiler_option.Externs_to_imports

let resolve ?compiler_option_mask prepared mode =
  Holyc_lib.resolve_global_records ?compiler_option_mask prepared.session
    ~declarations:prepared.declarations ~globals:prepared.globals
    ~compilation_mode:mode prepared.ast

let resolved ?compiler_option_mask prepared mode =
  resolve ?compiler_option_mask prepared mode |> checked

let records resolution = Semantic_global_resolution.records resolution

let record_names resolution =
  records resolution
  |> List.map Semantic_global_resolution.global_record_symbol
  |> List.map Semantic_symbol.name

let storage_names resolution =
  records resolution
  |> List.map Semantic_global_resolution.global_record_storage
  |> List.map Semantic_global_resolution.storage_name

let classification prepared resolution =
  checked
    (Holyc_lib.classify_global_records prepared.session ~resolution prepared.ast)

let classified_records classification =
  Semantic_global_record_classification.records classification

let option_snapshots resolution option =
  records resolution
  |> List.map Semantic_global_resolution.global_record_declaration
  |> List.map Semantic_global_resolution.declaration_compiler_option_mask
  |> List.map (fun mask -> Compiler_option.is_enabled ~mask option)

let aot_binding_matrix_uses_data_heap_only_for_definitions () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"aot-global-data-heap.HC"
      "public I64 Defined;\n\
       _intern 42 I64 Internal;\n\
       extern I64 External;\n\
       _extern REMOTE I64 Bound;\n\
       import I64 Imported;\n\
       _import REMOTE_IMPORT I64 Alternate;\n\
       class Pair { I64 value; } Attached;\n\
       I64 First,*Second;"
  in
  let resolution =
    resolved ~compiler_option_mask:data_heap_mask prepared mode
  in
  Alcotest.(check (list string))
    "source order"
    [
      "Defined";
      "Internal";
      "External";
      "Bound";
      "Imported";
      "Alternate";
      "Attached";
      "First";
      "Second";
    ]
    (record_names resolution);
  Alcotest.(check (list string))
    "binding-specific records bypass the allocation option"
    [
      "data-heap";
      "data-heap";
      "code-heap";
      "code-heap";
      "code-heap";
      "code-heap";
      "data-heap";
      "data-heap";
      "data-heap";
    ]
    (storage_names resolution);
  Alcotest.(check (list bool))
    "each declaration retains bit 35"
    (List.init 9 (Fun.const true))
    (option_snapshots resolution Compiler_option.Globals_on_data_heap);
  let classification = classification prepared resolution in
  Alcotest.(check (list bool))
    "GVF_DATA_HEAP follows resolved storage"
    [ true; true; false; false; false; false; true; true; true ]
    (classified_records classification
    |> List.map Semantic_global_record_classification.global_flag_mask
    |> List.map (fun mask ->
        Global_record_flag.Global_flag.is_set ~mask
          Global_record_flag.Global_flag.Data_heap));
  Alcotest.(check (list string))
    "AOT value access"
    [
      "aot-data-heap-reference";
      "aot-data-heap-reference";
      "aot-extern-unimplemented";
      "aot-code-heap-reference";
      "aot-import-reference";
      "aot-import-reference";
      "aot-data-heap-reference";
      "aot-data-heap-reference";
      "aot-data-heap-reference";
    ]
    (classified_records classification
    |> List.map Semantic_global_record_classification.value_access
    |> List.map Semantic_global_record_classification.value_access_name);
  Alcotest.(check (list string))
    "data-heap definitions are not AOT exports"
    [
      "none";
      "none";
      "none";
      "export";
      "import";
      "import";
      "none";
      "none";
      "none";
    ]
    (classified_records classification
    |> List.map Semantic_global_record_classification.aot_publication
    |> List.map Semantic_global_record_classification.aot_publication_name)

let converted_externs_stay_on_the_import_record_path () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"converted-extern-data-heap.HC"
      "extern I64 Plain; _extern REMOTE I64 Alternate; I64 Defined;"
  in
  let resolution =
    resolved ~compiler_option_mask:data_heap_and_import_mask prepared mode
  in
  Alcotest.(check (list string))
    "effective kinds"
    [ "import"; "alternate-import"; "definition" ]
    (records resolution
    |> List.map Semantic_global_resolution.global_record_kind
    |> List.map Semantic_global_resolution.declaration_kind_name);
  Alcotest.(check (list string))
    "converted extern storage"
    [ "code-heap"; "code-heap"; "data-heap" ]
    (storage_names resolution);
  let classified =
    resolution |> classification prepared |> classified_records
  in
  Alcotest.(check (list (option string)))
    "loader names"
    [ Some "Plain"; Some "REMOTE"; None ]
    (List.map Semantic_global_record_classification.import_name classified);
  Alcotest.(check (list string))
    "publication intent"
    [ "import"; "import"; "none" ]
    (classified
    |> List.map Semantic_global_record_classification.aot_publication
    |> List.map Semantic_global_record_classification.aot_publication_name)

let jit_data_heap_retains_initializers () =
  let mode = Preprocessor.Jit in
  let prepared =
    prepare ~mode ~path:"jit-initialized-data-heap.HC"
      "I64 Scalar=1; I64 Braced={2}; _intern 42 I64 Internal; extern I64 \
       External;"
  in
  let resolution =
    resolved ~compiler_option_mask:data_heap_mask prepared mode
  in
  Alcotest.(check (list string))
    "JIT storage"
    [ "data-heap"; "data-heap"; "data-heap"; "code-heap" ]
    (storage_names resolution);
  let globals = Semantic_global_type_resolution.globals prepared.globals in
  Alcotest.(check (list bool))
    "initializer boundaries remain available"
    [ true; true; false; false ]
    (List.map
       (fun global ->
         Semantic_global_type_resolution.global_initializer global
         |> Option.is_some)
       globals);
  let classified =
    resolution |> classification prepared |> classified_records
  in
  Alcotest.(check (list string))
    "JIT access policy"
    [
      "jit-direct-address";
      "jit-direct-address";
      "jit-direct-address";
      "jit-extern-address-slot";
    ]
    (classified
    |> List.map Semantic_global_record_classification.value_access
    |> List.map Semantic_global_record_classification.value_access_name)

let aot_data_heap_rejects_initializers_without_mutation () =
  let reject path source name =
    let prepared = prepare ~mode:Preprocessor.Aot ~path source in
    let table = Session.semantic_symbols prepared.session in
    let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
    let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
    Alcotest.(check (result reject string))
      (name ^ " initializer")
      (Error
         (Printf.sprintf "AOT data-heap global %S cannot have an initializer"
            name))
      (resolve ~compiler_option_mask:data_heap_mask prepared Preprocessor.Aot);
    Alcotest.(check int)
      (name ^ " scope count") scope_count
      (Semantic_symbol_table.all_scopes table |> List.length);
    Alcotest.(check int)
      (name ^ " symbol count") symbol_count
      (Semantic_symbol_table.all_symbols table |> List.length)
  in
  reject "aot-scalar-data-heap.HC" "I64 Scalar=1;" "Scalar";
  reject "aot-braced-data-heap.HC" "I64 Braced={2};" "Braced";
  let code_heap =
    prepare ~mode:Preprocessor.Aot ~path:"aot-code-heap-initializer.HC"
      "I64 Allowed=1;"
  in
  Alcotest.(check (list string))
    "the disabled option keeps initialized AOT globals on the code heap"
    [ "code-heap" ]
    (resolved code_heap Preprocessor.Aot |> storage_names)

let aot_data_heap_keeps_same_name_restriction () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"aot-repeated-data-heap.HC"
      "extern I64 Same; I64 Same;"
  in
  Alcotest.(check (result reject string))
    "a preceding global still triggers the source's unimplemented case"
    (Error
       "AOT data-heap global definitions cannot follow a same-name global; the \
        pinned compiler reports this case as unimplemented")
    (resolve ~compiler_option_mask:data_heap_mask prepared Preprocessor.Aot)

let per_declaration_snapshots_and_classifier_guard () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"split-global-storage-options.HC"
      "I64 Heap; I64 Code;"
  in
  let globals = Semantic_global_type_resolution.globals prepared.globals in
  let declarations =
    [
      checked
        (Semantic_global_resolution.make_declaration
           ~compiler_option_mask:data_heap_mask ~global:(List.nth globals 0) ());
      checked
        (Semantic_global_resolution.make_declaration
           ~global:(List.nth globals 1) ());
    ]
  in
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  let resolution =
    checked
      (Semantic_global_resolution.resolve ~table ~parent
         ~compilation_mode:Semantic_global_resolution.Aot declarations)
  in
  Alcotest.(check (list string))
    "declaration-local storage"
    [ "data-heap"; "code-heap" ]
    (storage_names resolution);
  let states =
    [
      Semantic_global_record_classification.make_record_state ~staging_mask:0L
        ~compiler_option_mask:data_heap_mask;
      Semantic_global_record_classification.make_record_state ~staging_mask:0L
        ~compiler_option_mask:Compiler_option.initial_mask;
    ]
  in
  let first =
    checked (Semantic_global_record_classification.classify resolution states)
  in
  let second =
    checked (Semantic_global_record_classification.classify resolution states)
  in
  Alcotest.(check (list string))
    "classification is deterministic"
    (first |> classified_records
    |> List.map Semantic_global_record_classification.value_access
    |> List.map Semantic_global_record_classification.value_access_name)
    (second |> classified_records
    |> List.map Semantic_global_record_classification.value_access
    |> List.map Semantic_global_record_classification.value_access_name);
  let mismatched_states =
    [
      Semantic_global_record_classification.make_record_state ~staging_mask:0L
        ~compiler_option_mask:Compiler_option.initial_mask;
      List.nth states 1;
    ]
  in
  Alcotest.(check (result reject string))
    "the classifier rejects a contradictory storage option"
    (Error
       "global record classification has a different globals-on-data-heap \
        state than global resolution")
    (Semantic_global_record_classification.classify resolution mismatched_states)

let tests =
  [
    Alcotest.test_case "AOT binding and storage matrix" `Quick
      aot_binding_matrix_uses_data_heap_only_for_definitions;
    Alcotest.test_case "converted extern storage" `Quick
      converted_externs_stay_on_the_import_record_path;
    Alcotest.test_case "JIT initialized data heap" `Quick
      jit_data_heap_retains_initializers;
    Alcotest.test_case "AOT initializer restriction" `Quick
      aot_data_heap_rejects_initializers_without_mutation;
    Alcotest.test_case "AOT repeated-name restriction" `Quick
      aot_data_heap_keeps_same_name_restriction;
    Alcotest.test_case "declaration-local storage snapshots" `Quick
      per_declaration_snapshots_and_classifier_guard;
  ]
