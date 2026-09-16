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

let parse ?(mode = Preprocessor.Jit) session ~path contents =
  let source = Session.add_source session ~path ~contents in
  Holyc_lib.parse_with_config session ~config:(config mode) ~source
  |> expect_ast

let resolve session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let resolution =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  (declarations, resolution)

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let identity_ids resolution =
  Semantic_aggregate_resolution.identities resolution
  |> List.map (fun identity ->
      Semantic_aggregate_resolution.identity_symbol identity |> symbol_id)

let identity_names resolution =
  Semantic_aggregate_resolution.identities resolution
  |> List.map (fun identity ->
      Semantic_aggregate_resolution.identity_symbol identity
      |> Semantic_symbol.name)

let identity_kinds resolution =
  Semantic_aggregate_resolution.identities resolution
  |> List.map (fun identity ->
      Semantic_aggregate_resolution.identity_kind identity
      |> Semantic_aggregate_resolution.aggregate_kind_name)

let declaration_identity_ids resolution =
  Semantic_aggregate_resolution.declarations resolution
  |> List.map (fun declaration ->
      Semantic_aggregate_resolution.resolved_declaration_identity_symbol
        declaration
      |> symbol_id)

let declaration_sites resolution =
  Semantic_aggregate_resolution.declarations resolution
  |> List.map Semantic_aggregate_resolution.resolved_declaration_site

let simple_completion () =
  let session = Session.create () in
  let ast =
    parse session ~path:"completion.HC"
      "extern class Item; class Item { I64 value; };"
  in
  let declarations, resolution = resolve session ast in
  let aggregate_entries =
    Semantic_declaration_collection.entries declarations
    |> List.filter (fun entry ->
        match Semantic_declaration_collection.entry_kind entry with
        | Semantic_declaration_collection.Aggregate_forward
        | Semantic_declaration_collection.Aggregate_definition -> true
        | _ -> false)
  in
  let forward_symbol =
    List.nth aggregate_entries 0 |> Semantic_declaration_collection.entry_symbol
  in
  let definition_symbol =
    List.nth aggregate_entries 1 |> Semantic_declaration_collection.entry_symbol
  in
  Alcotest.(check int)
    "one aggregate identity remains" 1
    (Semantic_aggregate_resolution.identities resolution |> List.length);
  let identity =
    Semantic_aggregate_resolution.identities resolution |> List.hd
  in
  Alcotest.(check int)
    "the first forward is canonical" (symbol_id forward_symbol)
    (Semantic_aggregate_resolution.identity_symbol identity |> symbol_id);
  Alcotest.(check bool)
    "the earlier forward site is retained" true
    (Semantic_aggregate_resolution.identity_forward identity
    |> Option.map (fun site ->
        Semantic_aggregate_resolution.declaration_site_symbol site
        |> Semantic_symbol.id
        |> Semantic_symbol.Id.equal (Semantic_symbol.id forward_symbol))
    |> Option.value ~default:false);
  Alcotest.(check (list int))
    "both declarations map to the first forward"
    [ symbol_id forward_symbol; symbol_id forward_symbol ]
    (declaration_identity_ids resolution);
  Alcotest.(check int)
    "the definition remains a distinct declaration site"
    (symbol_id definition_symbol)
    (List.nth (declaration_sites resolution) 1
    |> Semantic_aggregate_resolution.declaration_site_symbol |> symbol_id);
  Alcotest.(check (list string))
    "both declaration roles remain visible"
    [ "forward"; "definition" ]
    (declaration_sites resolution
    |> List.map Semantic_aggregate_resolution.declaration_site_kind
    |> List.map Semantic_aggregate_resolution.declaration_kind_name);
  Alcotest.(check (list int))
    "both module positions remain visible" [ 0; 1 ]
    (declaration_sites resolution
    |> List.map Semantic_aggregate_resolution.declaration_site_item_index);
  Alcotest.(check (list int))
    "the identity keeps its forward position" [ 0 ]
    (Semantic_aggregate_resolution.identities resolution
    |> List.map Semantic_aggregate_resolution.identity_first_item_index)

let newest_forward_and_kind () =
  let session = Session.create () in
  let ast =
    parse session ~path:"newest.HC"
      "extern class Item; extern union Item; class Item { I64 value; };"
  in
  let declarations, resolution = resolve session ast in
  let aggregate_entries =
    Semantic_declaration_collection.entries declarations
    |> List.filter (fun entry ->
        match Semantic_declaration_collection.entry_kind entry with
        | Semantic_declaration_collection.Aggregate_forward
        | Semantic_declaration_collection.Aggregate_definition -> true
        | _ -> false)
  in
  let newest_forward_symbol =
    List.nth aggregate_entries 1 |> Semantic_declaration_collection.entry_symbol
  in
  let older_forward_symbol =
    List.nth aggregate_entries 0 |> Semantic_declaration_collection.entry_symbol
  in
  Alcotest.(check (list string))
    "each forward starts an identity" [ "Item"; "Item" ]
    (identity_names resolution);
  Alcotest.(check (list string))
    "the definition kind wins for its identity" [ "class"; "class" ]
    (identity_kinds resolution);
  let identities = Semantic_aggregate_resolution.identities resolution in
  let first = List.nth identities 0 in
  let second = List.nth identities 1 in
  Alcotest.(check int)
    "the older repeated forward keeps its own fresh identity"
    (symbol_id older_forward_symbol)
    (Semantic_aggregate_resolution.identity_symbol first |> symbol_id);
  Alcotest.(check bool)
    "the older forward stays unresolved" true
    (Semantic_aggregate_resolution.identity_definition first |> Option.is_none);
  Alcotest.(check string)
    "the completed forward retains its union spelling" "union"
    (Semantic_aggregate_resolution.identity_forward second
    |> Option.get
    |> Semantic_aggregate_resolution.declaration_site_aggregate_kind
    |> Semantic_aggregate_resolution.aggregate_kind_name);
  Alcotest.(check int)
    "mixed-kind completion keeps the newest forward identity"
    (symbol_id newest_forward_symbol)
    (Semantic_aggregate_resolution.identity_symbol second |> symbol_id);
  Alcotest.(check (list int))
    "only the newest forward maps to the definition"
    [
      List.nth (identity_ids resolution) 0;
      List.nth (identity_ids resolution) 1;
      List.nth (identity_ids resolution) 1;
    ]
    (declaration_identity_ids resolution)

let stable_type_reference_and_shadow () =
  let session = Session.create () in
  let ast =
    parse session ~path:"stable-identity.HC"
      "extern class Item; class Item {}; class Item {};"
  in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregate_entries =
    Semantic_declaration_collection.entries declarations
    |> List.filter (fun entry ->
        match Semantic_declaration_collection.entry_kind entry with
        | Semantic_declaration_collection.Aggregate_forward
        | Semantic_declaration_collection.Aggregate_definition -> true
        | _ -> false)
  in
  let forward_symbol =
    List.nth aggregate_entries 0 |> Semantic_declaration_collection.entry_symbol
  in
  let first_definition_symbol =
    List.nth aggregate_entries 1 |> Semantic_declaration_collection.entry_symbol
  in
  let shadow_symbol =
    List.nth aggregate_entries 2 |> Semantic_declaration_collection.entry_symbol
  in
  let retained =
    checked
      (Semantic_type.make_aggregate ~symbol:forward_symbol ~pointer_depth:1)
  in
  let resolution =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let identities = Semantic_aggregate_resolution.identities resolution in
  let completed = List.nth identities 0 in
  let shadow = List.nth identities 1 in
  Alcotest.(check int)
    "completion preserves the already selected forward symbol"
    (symbol_id forward_symbol)
    (Semantic_aggregate_resolution.identity_symbol completed |> symbol_id);
  Alcotest.(check bool)
    "completion does not replace the identity with its definition site" true
    (Semantic_aggregate_resolution.identity_symbol completed
    != first_definition_symbol);
  let completed_type =
    checked
      (Semantic_type.make_aggregate
         ~symbol:(Semantic_aggregate_resolution.identity_symbol completed)
         ~pointer_depth:1)
  in
  Alcotest.(check bool)
    "an old aggregate pointer type remains exactly equal after completion" true
    (Semantic_type.equal retained completed_type);
  Alcotest.(check int)
    "a later resolved definition starts a fresh identity"
    (symbol_id shadow_symbol)
    (Semantic_aggregate_resolution.identity_symbol shadow |> symbol_id);
  let shadow_type =
    checked
      (Semantic_type.make_aggregate
         ~symbol:(Semantic_aggregate_resolution.identity_symbol shadow)
         ~pointer_depth:1)
  in
  Alcotest.(check bool)
    "the fresh shadow identity does not weaken aggregate type equality" false
    (Semantic_type.equal retained shadow_type)

let retained_identity_hint () =
  let table = Semantic_symbol_table.create () in
  let root = Semantic_symbol_table.root table in
  let scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:root
         ~kind:Semantic_symbol_table.Module ~name:"retained.HC" ())
  in
  let symbol origin =
    checked
      (Semantic_symbol_table.add table ~scope ~name:"Item"
         ~kind:Semantic_symbol.Aggregate_type
         ~origin:(Semantic_symbol.Synthesized origin))
  in
  let forward = symbol "forward" in
  let definition = symbol "definition" in
  let fact =
    checked
      (Semantic_aggregate_resolution.make_retained_declaration
         ~symbol:definition ~identity_symbol:forward
         ~declaration_kind:Semantic_aggregate_resolution.Definition
         ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index:1)
  in
  let resolution =
    checked
      (Semantic_aggregate_resolution.resolve ~table ~parent:scope [ fact ])
  in
  let identity =
    Semantic_aggregate_resolution.identities resolution |> List.hd
  in
  let site = declaration_sites resolution |> List.hd in
  Alcotest.(check int)
    "a definition-only retained view keeps its earlier canonical identity"
    (symbol_id forward)
    (Semantic_aggregate_resolution.identity_symbol identity |> symbol_id);
  Alcotest.(check int)
    "the retained definition site stays distinct from its identity"
    (symbol_id definition)
    (Semantic_aggregate_resolution.declaration_site_symbol site |> symbol_id)

let shadowing_definitions_and_late_forward () =
  let session = Session.create () in
  let ast =
    parse session ~path:"shadowing.HC"
      "class Item { I64 first; };\n\
       union Item { I64 second; };\n\
       extern class Item;\n\
       union Item { I64 third; };"
  in
  let _, resolution = resolve session ast in
  Alcotest.(check (list int))
    "three identities follow their creation sites" [ 0; 1; 2 ]
    (Semantic_aggregate_resolution.identities resolution
    |> List.map Semantic_aggregate_resolution.identity_first_item_index);
  Alcotest.(check (list string))
    "definitions keep their effective kinds"
    [ "class"; "union"; "union" ]
    (identity_kinds resolution);
  let identities = Semantic_aggregate_resolution.identities resolution in
  Alcotest.(check bool)
    "the first definition has no forward" true
    (List.nth identities 0 |> Semantic_aggregate_resolution.identity_forward
   |> Option.is_none);
  Alcotest.(check bool)
    "the late forward is completed" true
    (List.nth identities 2 |> Semantic_aggregate_resolution.identity_definition
   |> Option.is_some);
  let ids = identity_ids resolution in
  Alcotest.(check (list int))
    "the late pair shares its forward identity"
    [ List.nth ids 0; List.nth ids 1; List.nth ids 2; List.nth ids 2 ]
    (declaration_identity_ids resolution)

let modes_and_determinism () =
  List.iter
    (fun mode ->
      let session = Session.create () in
      let ast =
        parse session ~mode ~path:"modes.HC"
          "extern class A; class A { I64 x; }; extern union B;"
      in
      let declarations = checked (Holyc_lib.collect_declarations session ast) in
      let table = Session.semantic_symbols session in
      let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
      let symbol_count =
        Semantic_symbol_table.all_symbols table |> List.length
      in
      let first =
        checked (Holyc_lib.resolve_aggregates session ~declarations ast)
      in
      let second =
        checked (Holyc_lib.resolve_aggregates session ~declarations ast)
      in
      Alcotest.(check (list int))
        "repeated resolution is deterministic" (identity_ids first)
        (identity_ids second);
      Alcotest.(check int)
        "resolution creates no scope" scope_count
        (Semantic_symbol_table.all_scopes table |> List.length);
      Alcotest.(check int)
        "resolution creates no symbol" symbol_count
        (Semantic_symbol_table.all_symbols table |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let generated_provenance () =
  let session = Session.create () in
  let ast =
    parse session ~path:"generated-aggregate.HC"
      "#define NAME Generated\nextern class NAME;\nclass NAME { I64 value; };"
  in
  let _, resolution = resolve session ast in
  List.iter
    (fun site ->
      match
        Semantic_aggregate_resolution.declaration_site_symbol site
        |> Semantic_symbol.origin
      with
      | Semantic_symbol.Source_location origin ->
          Alcotest.(check bool)
            "definition expansion keeps its invocation" true
            (Option.is_some origin.generated_from);
          Alcotest.(check bool)
            "definition expansion keeps its definition" true
            (Option.is_some origin.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected generated aggregate provenance")
    (declaration_sites resolution)

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
  let directory = Filename.temp_dir "holyc-aggregate-resolution-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "aggregate.HC" in
      write_file root_path "#include \"aggregate\"";
      write_file include_path
        "extern class Included; class Included { I64 value; };";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let _, resolution = resolve session ast in
      List.iter
        (fun site ->
          match
            Semantic_aggregate_resolution.declaration_site_symbol site
            |> Semantic_symbol.origin
          with
          | Semantic_symbol.Source_location origin ->
              let source =
                Source_manager.find (Session.sources session) origin.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "included site keeps its canonical source" "aggregate.HC"
                (Source_file.path source |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included aggregate provenance")
        (declaration_sites resolution))

let mismatched_inputs () =
  let session = Session.create () in
  let ast = parse session ~path:"original.HC" "extern class A; class A {};" in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let other_ast =
    parse session ~path:"other.HC" "extern class A; class A {};"
  in
  let table = Session.semantic_symbols session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check bool)
    "a declaration collection from another AST is rejected" true
    (Holyc_lib.resolve_aggregates session ~declarations other_ast
    |> Result.is_error);
  Alcotest.(check int)
    "an AST mismatch adds no scope" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "an AST mismatch adds no symbol" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let foreign_session = Session.create () in
  Alcotest.(check bool)
    "a declaration collection cannot cross sessions" true
    (Holyc_lib.resolve_aggregates foreign_session ~declarations ast
    |> Result.is_error)

let low_level_validation () =
  let table = Semantic_symbol_table.create () in
  let root = Semantic_symbol_table.root table in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:root
         ~kind:Semantic_symbol_table.Module ~name:"manual.HC" ())
  in
  let symbol name =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name
         ~kind:Semantic_symbol.Aggregate_type
         ~origin:(Semantic_symbol.Synthesized (name ^ " aggregate")))
  in
  let first = symbol "A" in
  let second = symbol "A" in
  let third = symbol "A" in
  let forward =
    checked
      (Semantic_aggregate_resolution.make_declaration ~symbol:first
         ~declaration_kind:Semantic_aggregate_resolution.Forward
         ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index:0)
  in
  let definition =
    checked
      (Semantic_aggregate_resolution.make_declaration ~symbol:second
         ~declaration_kind:Semantic_aggregate_resolution.Definition
         ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index:1)
  in
  let newer_forward =
    checked
      (Semantic_aggregate_resolution.make_declaration ~symbol:second
         ~declaration_kind:Semantic_aggregate_resolution.Forward
         ~aggregate_kind:Semantic_aggregate_resolution.Union ~item_index:1)
  in
  let stale_hint_definition =
    checked
      (Semantic_aggregate_resolution.make_retained_declaration ~symbol:third
         ~identity_symbol:first
         ~declaration_kind:Semantic_aggregate_resolution.Definition
         ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index:2)
  in
  Alcotest.(check bool)
    "a forward cannot import an earlier canonical identity" true
    (Semantic_aggregate_resolution.make_retained_declaration ~symbol:first
       ~identity_symbol:first
       ~declaration_kind:Semantic_aggregate_resolution.Forward
       ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index:0
    |> Result.is_error);
  Alcotest.(check bool)
    "repeated input symbols are rejected" true
    (Semantic_aggregate_resolution.resolve ~table ~parent:module_scope
       [ forward; forward ]
    |> Result.is_error);
  Alcotest.(check bool)
    "reversed item order is rejected" true
    (Semantic_aggregate_resolution.resolve ~table ~parent:module_scope
       [ definition; forward ]
    |> Result.is_error);
  Alcotest.(check bool)
    "a definition cannot bypass the newest forward with a stale identity hint"
    true
    (Semantic_aggregate_resolution.resolve ~table ~parent:module_scope
       [ forward; newer_forward; stale_hint_definition ]
    |> Result.is_error);
  let other_table = Semantic_symbol_table.create () in
  Alcotest.(check bool)
    "a foreign module scope is rejected" true
    (Semantic_aggregate_resolution.resolve ~table:other_table
       ~parent:module_scope [ forward; definition ]
    |> Result.is_error);
  let global =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"value"
         ~kind:Semantic_symbol.Global_variable
         ~origin:(Semantic_symbol.Synthesized "test global"))
  in
  Alcotest.(check bool)
    "a nonaggregate fact is rejected" true
    (Semantic_aggregate_resolution.make_declaration ~symbol:global
       ~declaration_kind:Semantic_aggregate_resolution.Forward
       ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index:2
    |> Result.is_error)

let tests =
  [
    Alcotest.test_case "simple completion" `Quick simple_completion;
    Alcotest.test_case "newest forward and kind" `Quick newest_forward_and_kind;
    Alcotest.test_case "shadowing definitions and late forward" `Quick
      shadowing_definitions_and_late_forward;
    Alcotest.test_case "stable completed type identity and fresh shadow" `Quick
      stable_type_reference_and_shadow;
    Alcotest.test_case "retained definition-only canonical identity" `Quick
      retained_identity_hint;
    Alcotest.test_case "modes and determinism" `Quick modes_and_determinism;
    Alcotest.test_case "generated provenance" `Quick generated_provenance;
    Alcotest.test_case "included provenance" `Quick included_provenance;
    Alcotest.test_case "mismatched inputs" `Quick mismatched_inputs;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
  ]
