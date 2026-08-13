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

let parse session ~path contents =
  let source = Session.add_source session ~path ~contents in
  (source, Holyc_lib.parse session ~source |> expect_ast)

let entry_names collection =
  Semantic_declaration_collection.entries collection
  |> List.map (fun entry ->
      Semantic_declaration_collection.entry_symbol entry |> Semantic_symbol.name)

let entry_kinds collection =
  Semantic_declaration_collection.entries collection
  |> List.map (fun entry ->
      Semantic_declaration_collection.entry_kind entry
      |> Semantic_declaration_collection.declaration_kind_name)

let entry_symbol_kinds collection =
  Semantic_declaration_collection.entries collection
  |> List.map (fun entry ->
      Semantic_declaration_collection.entry_symbol entry
      |> Semantic_symbol.kind |> Semantic_symbol.kind_name)

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let option_exists predicate = function
  | Some value -> predicate value
  | None -> false

let scope_id scope =
  Semantic_symbol_table.scope_id scope |> Semantic_symbol.Scope_id.to_int

let top_level_shapes () =
  let source =
    "extern class Forward;\n\
     extern union Choice;\n\
     class Forward { I64 value; } first_obj, second_obj;\n\
     union Choice { I64 bits; };\n\
     I64 single;\n\
     I64 group_a, group_b;\n\
     extern I64 Proto(I64 value);\n\
     I64 Defined(I64 value) { return value; }\n\
     \"ignored\";"
  in
  let session = Session.create () in
  let _, ast = parse session ~path:"top-level.HC" source in
  let collection = checked (Holyc_lib.collect_declarations session ast) in
  let scope = Semantic_declaration_collection.scope collection in
  Alcotest.(check string)
    "module scope kind" "module"
    (Semantic_symbol_table.scope_kind scope
    |> Semantic_symbol_table.scope_kind_name);
  Alcotest.(check (option string))
    "module scope name" (Some "top-level.HC")
    (Semantic_symbol_table.scope_name scope);
  Alcotest.(check (list string))
    "source-ordered declarations"
    [
      "Forward";
      "Choice";
      "Forward";
      "first_obj";
      "second_obj";
      "Choice";
      "single";
      "group_a";
      "group_b";
      "Proto";
      "Defined";
    ]
    (entry_names collection);
  Alcotest.(check (list string))
    "declaration shapes"
    [
      "aggregate-forward";
      "aggregate-forward";
      "aggregate-definition";
      "aggregate-attached-global";
      "aggregate-attached-global";
      "aggregate-definition";
      "global-variable";
      "global-variable";
      "global-variable";
      "function-prototype";
      "function-definition";
    ]
    (entry_kinds collection);
  Alcotest.(check (list string))
    "semantic kinds"
    [
      "aggregate-type";
      "aggregate-type";
      "aggregate-type";
      "global-variable";
      "global-variable";
      "aggregate-type";
      "global-variable";
      "global-variable";
      "global-variable";
      "function";
      "function";
    ]
    (entry_symbol_kinds collection);
  let entries = Semantic_declaration_collection.entries collection in
  Alcotest.(check (list int))
    "module item positions"
    [ 0; 1; 2; 2; 2; 3; 4; 5; 5; 6; 7 ]
    (List.map Semantic_declaration_collection.entry_item_index entries);
  Alcotest.(check (list (option int)))
    "group positions"
    [ None; None; None; Some 0; Some 1; None; None; Some 0; Some 1; None; None ]
    (List.map Semantic_declaration_collection.entry_declarator_index entries)

let lookup_order_and_parent () =
  let session = Session.create () in
  let _, ast =
    parse session ~path:"lookup.HC"
      "extern class Item;\nclass Item { I64 value; };"
  in
  let collection = checked (Holyc_lib.collect_declarations session ast) in
  let table = Session.semantic_symbols session in
  let scope = Semantic_declaration_collection.scope collection in
  let entries = Semantic_declaration_collection.entries collection in
  let forward =
    List.nth entries 0 |> Semantic_declaration_collection.entry_symbol
  in
  let definition =
    List.nth entries 1 |> Semantic_declaration_collection.entry_symbol
  in
  let lookup name kinds instance =
    checked
      (Semantic_symbol_table.lookup table ~scope ~name ~kinds ~instance ())
  in
  Alcotest.(check (option int))
    "definition is newest"
    (Some (symbol_id definition))
    (lookup "Item" [ Semantic_symbol.Aggregate_type ] 1 |> Option.map symbol_id);
  Alcotest.(check (option int))
    "forward remains available"
    (Some (symbol_id forward))
    (lookup "Item" [ Semantic_symbol.Aggregate_type ] 2 |> Option.map symbol_id);
  Alcotest.(check bool)
    "internal type is inherited" true
    (lookup "I64i" [ Semantic_symbol.Internal_type ] 1 |> Option.is_some);
  Alcotest.(check bool)
    "module name does not leak into task scope" true
    (checked
       (Semantic_symbol_table.lookup_local table
          ~scope:(Semantic_symbol_table.root table)
          ~name:"Item"
          ~kinds:[ Semantic_symbol.Aggregate_type ]
          ())
    |> Option.is_none)

let generated_provenance () =
  let session = Session.create () in
  let root, ast =
    parse session ~path:"generated.HC" "#define NAME generated_value\nI64 NAME;"
  in
  let collection = checked (Holyc_lib.collect_declarations session ast) in
  let symbol =
    Semantic_declaration_collection.entries collection
    |> List.hd |> Semantic_declaration_collection.entry_symbol
  in
  Alcotest.(check string)
    "expanded name" "generated_value"
    (Semantic_symbol.name symbol);
  match Semantic_symbol.origin symbol with
  | Semantic_symbol.Source_location origin ->
      Alcotest.(check bool)
        "generated frame differs from root" false
        (Source_id.equal origin.span.source (Source_file.id root));
      Alcotest.(check bool)
        "invocation is retained" true
        (option_exists
           (fun span -> Source_id.equal span.Span.source (Source_file.id root))
           origin.generated_from);
      Alcotest.(check bool)
        "definition is retained" true
        (option_exists
           (fun span -> Source_id.equal span.Span.source (Source_file.id root))
           origin.defined_at);
      Alcotest.(check bool)
        "source segments survive" true
        (origin.source_segments <> [])
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expanded declaration lost its source location"

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let included_provenance () =
  let directory = Filename.temp_dir "holyc-sema-collection-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let included_path = Filename.concat directory "decl.HC" in
      write_file root_path "#include \"decl\"";
      write_file included_path "I64 included_value;";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked
          (Preprocessor.Config.create ~working_directory:directory
             ~include_roots:[ directory ] ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let collection = checked (Holyc_lib.collect_declarations session ast) in
      let symbol =
        Semantic_declaration_collection.entries collection
        |> List.hd |> Semantic_declaration_collection.entry_symbol
      in
      match Semantic_symbol.origin symbol with
      | Semantic_symbol.Source_location origin -> (
          match
            Source_manager.find (Session.sources session) origin.span.source
          with
          | None -> Alcotest.fail "included source is not registered"
          | Some included ->
              Alcotest.(check string)
                "included declaration display path" "decl"
                (Source_file.display_path included);
              Alcotest.(check (option string))
                "module retains root path" (Some root_path)
                (Semantic_declaration_collection.scope collection
                |> Semantic_symbol_table.scope_name))
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "included declaration lost its source location")

let repeated_modules_and_rejected_source () =
  let session = Session.create () in
  let _, first_ast = parse session ~path:"first.HC" "I64 first;" in
  let _, second_ast = parse session ~path:"second.HC" "I64 second;" in
  let first = checked (Holyc_lib.collect_declarations session first_ast) in
  let second = checked (Holyc_lib.collect_declarations session second_ast) in
  Alcotest.(check (list int))
    "module scope IDs" [ 1; 2 ]
    [
      Semantic_declaration_collection.scope first |> scope_id;
      Semantic_declaration_collection.scope second |> scope_id;
    ];
  Alcotest.(check (list int))
    "declaration IDs" [ 17; 18 ]
    [
      Semantic_declaration_collection.entries first
      |> List.hd |> Semantic_declaration_collection.entry_symbol |> symbol_id;
      Semantic_declaration_collection.entries second
      |> List.hd |> Semantic_declaration_collection.entry_symbol |> symbol_id;
    ];
  let table = Session.semantic_symbols session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let source_id = Source_id.of_int 9_999 |> checked in
  let span =
    Span.make ~source:source_id ~start:0 ~stop:0 ~length:0 |> checked
  in
  let unregistered = Ast.make_module ~source:source_id ~span ~items:[] in
  Alcotest.(check bool)
    "unknown source is rejected" true
    (Holyc_lib.collect_declarations session unregistered |> Result.is_error);
  Alcotest.(check int)
    "rejection preserves scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "rejection preserves symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let sources = Session.sources session in
  let human = Semantic_symbol_table.human sources table in
  let json = Semantic_symbol_table.json sources table in
  Alcotest.(check string)
    "human dump is deterministic" human
    (Semantic_symbol_table.human sources table);
  Alcotest.(check string)
    "JSON dump is deterministic" json
    (Semantic_symbol_table.json sources table)

let declaration_metadata_validation () =
  let make ?(name = "manual") ?(item_index = 0) ?declarator_index () =
    Semantic_declaration_collection.make_declaration ~name
      ~declaration_kind:Semantic_declaration_collection.Global_variable
      ~origin:(Semantic_symbol.Synthesized "test declaration") ~item_index
      ?declarator_index ()
  in
  Alcotest.(check bool)
    "empty name is rejected" true
    (make ~name:"" () |> Result.is_error);
  Alcotest.(check bool)
    "negative item index is rejected" true
    (make ~item_index:(-1) () |> Result.is_error);
  Alcotest.(check bool)
    "negative declarator index is rejected" true
    (make ~declarator_index:(-1) () |> Result.is_error);
  let table = Semantic_symbol_table.create () in
  let collection =
    checked
      (Semantic_declaration_collection.collect ~table ~module_name:"manual.HC"
         [ checked (make ()) ])
  in
  Alcotest.(check int)
    "first module scope ID" 1
    (Semantic_declaration_collection.scope collection |> scope_id);
  Alcotest.(check int)
    "first manual symbol ID" 0
    (Semantic_declaration_collection.entries collection
    |> List.hd |> Semantic_declaration_collection.entry_symbol |> symbol_id)

let tests =
  [
    Alcotest.test_case "top-level shapes" `Quick top_level_shapes;
    Alcotest.test_case "lookup order and parent" `Quick lookup_order_and_parent;
    Alcotest.test_case "generated provenance" `Quick generated_provenance;
    Alcotest.test_case "included provenance" `Quick included_provenance;
    Alcotest.test_case "repeated modules and rejected source" `Quick
      repeated_modules_and_rejected_source;
    Alcotest.test_case "declaration metadata validation" `Quick
      declaration_metadata_validation;
  ]
