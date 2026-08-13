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

type semantic_results = {
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  headers : Semantic_aggregate_header_resolution.t;
}

let resolve session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  { declarations; aggregates; headers }

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let headers results =
  Semantic_aggregate_header_resolution.headers results.headers

let header_named results name =
  headers results
  |> List.find (fun header ->
      Semantic_aggregate_header_resolution.header_symbol header
      |> Semantic_symbol.name |> String.equal name)

let required_backing header =
  match Semantic_aggregate_header_resolution.header_backing header with
  | Some backing -> backing
  | None -> Alcotest.fail "expected an aggregate backing"

let required_base header =
  match Semantic_aggregate_header_resolution.header_base header with
  | Some base -> base
  | None -> Alcotest.fail "expected an aggregate base"

let aggregate_target type_ =
  match Semantic_type.base type_ with
  | Semantic_type.Aggregate symbol -> symbol
  | Semantic_type.Primitive _ -> Alcotest.fail "expected an aggregate type"

let check_primitive_backing ~form ~primitive backing =
  let resolved_type =
    Semantic_aggregate_header_resolution.backing_type backing
  in
  (match Semantic_type.base resolved_type with
  | Semantic_type.Primitive (actual_form, actual_primitive) ->
      Alcotest.(check string)
        "primitive form" (Semantic_type.primitive_form_name form)
        (Semantic_type.primitive_form_name actual_form);
      Alcotest.(check string)
        "primitive value" (Primitive_type.to_string primitive)
        (Primitive_type.to_string actual_primitive)
  | Semantic_type.Aggregate _ ->
      Alcotest.fail "expected a primitive aggregate backing");
  resolved_type

let primitive_and_internal_backings () =
  let session = Session.create () in
  let ast =
    parse session ~path:"primitive-backing.HC"
      "I64 union PublicWord { I64 value; };\n\
       I64i union StorageWord { I64i value; };"
  in
  let results = resolve session ast in
  let public_backing = header_named results "PublicWord" |> required_backing in
  let storage_backing =
    header_named results "StorageWord" |> required_backing
  in
  Alcotest.(check string)
    "public spelling is retained" "I64"
    (Semantic_aggregate_header_resolution.backing_spelling public_backing);
  Alcotest.(check string)
    "storage spelling is retained" "I64i"
    (Semantic_aggregate_header_resolution.backing_spelling storage_backing);
  let public_type =
    check_primitive_backing ~form:Semantic_type.Public_spelling
      ~primitive:Primitive_type.I64 public_backing
  in
  let storage_type =
    check_primitive_backing ~form:Semantic_type.Internal_storage
      ~primitive:Primitive_type.I64 storage_backing
  in
  Alcotest.(check int)
    "public backing is not a pointer" 0
    (Semantic_type.pointer_depth public_type);
  Alcotest.(check int)
    "storage backing is not a pointer" 0
    (Semantic_type.pointer_depth storage_type)

let pointer_depths () =
  let session = Session.create () in
  let ast =
    parse session ~path:"pointer-backing.HC"
      "I64 class Depth0 {};\n\
       I64 * class Depth1 {};\n\
       I64 ** class Depth2 {};\n\
       I64 *** class Depth3 {};\n\
       I64 **** class Depth4 {};"
  in
  let results = resolve session ast in
  Alcotest.(check (list int))
    "all source pointer depths are preserved" [ 0; 1; 2; 3; 4 ]
    (headers results
    |> List.map (fun header ->
        header |> required_backing
        |> Semantic_aggregate_header_resolution.backing_type
        |> Semantic_type.pointer_depth));
  Alcotest.(check bool)
    "the checked type rejects a fifth pointer star" true
    (Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
       ~primitive:Primitive_type.I64 ~pointer_depth:5
    |> Result.is_error);
  Alcotest.(check bool)
    "Bool is not an intrinsic storage spelling" true
    (Semantic_type.make_primitive ~form:Semantic_type.Internal_storage
       ~primitive:Primitive_type.Bool ~pointer_depth:0
    |> Result.is_error)

let backing_uses_prepublication_state () =
  let session = Session.create () in
  let ast =
    parse session ~path:"backing-publication.HC"
      "class Cell {}; Cell class Cell {};"
  in
  let results = resolve session ast in
  let first = List.nth (headers results) 0 in
  let second = List.nth (headers results) 1 in
  let target =
    second |> required_backing
    |> Semantic_aggregate_header_resolution.backing_type |> aggregate_target
  in
  Alcotest.(check int)
    "the backing binds to the older identity"
    (first |> Semantic_aggregate_header_resolution.header_symbol |> symbol_id)
    (symbol_id target);
  Alcotest.(check bool)
    "the new definition has a distinct identity" true
    (not
       (Semantic_symbol.Id.equal
          (first |> Semantic_aggregate_header_resolution.header_symbol
         |> Semantic_symbol.id)
          (second |> Semantic_aggregate_header_resolution.header_symbol
         |> Semantic_symbol.id)))

let base_uses_postpublication_state () =
  let session = Session.create () in
  let ast = parse session ~path:"base-publication.HC" "class Self : Self {};" in
  let results = resolve session ast in
  let header = List.hd (headers results) in
  let owner =
    Semantic_aggregate_header_resolution.header_symbol header |> symbol_id
  in
  let base =
    header |> required_base
    |> Semantic_aggregate_header_resolution.base_symbol |> symbol_id
  in
  Alcotest.(check int)
    "the current identity is visible while its base is read" owner base

let newest_forward_is_canonical () =
  let session = Session.create () in
  let ast =
    parse session ~path:"forward-base.HC"
      "extern class Node;\n\
       extern class Node;\n\
       class Child : Node {};\n\
       class Node {};"
  in
  let results = resolve session ast in
  let child = header_named results "Child" in
  let node = header_named results "Node" in
  let base_symbol =
    child |> required_base
    |> Semantic_aggregate_header_resolution.base_symbol
  in
  Alcotest.(check int)
    "the base uses the completed newest forward"
    (node |> Semantic_aggregate_header_resolution.header_symbol |> symbol_id)
    (symbol_id base_symbol);
  let node_identities =
    Semantic_aggregate_resolution.identities results.aggregates
    |> List.filter (fun identity ->
        identity |> Semantic_aggregate_resolution.identity_symbol
        |> Semantic_symbol.name |> String.equal "Node")
  in
  Alcotest.(check int) "both forwards keep identities" 2
    (List.length node_identities);
  Alcotest.(check bool)
    "the older forward remains unresolved" true
    (List.hd node_identities
    |> Semantic_aggregate_resolution.identity_definition |> Option.is_none)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_provenance () =
  let session = Session.create () in
  let ast =
    parse session ~path:"generated-header.HC"
      "#define STORAGE I64i\n\
       #define ROOT Root\n\
       class Root {};\n\
       STORAGE class Generated : ROOT {};"
  in
  let header = resolve session ast |> fun results -> header_named results "Generated" in
  let backing_origin =
    header |> required_backing
    |> Semantic_aggregate_header_resolution.backing_spelling_origin
    |> source_origin
  in
  let base_origin =
    header |> required_base
    |> Semantic_aggregate_header_resolution.base_name_origin |> source_origin
  in
  List.iter
    (fun origin ->
      Alcotest.(check bool)
        "the expansion invocation is retained" true
        (Option.is_some origin.Semantic_symbol.generated_from);
      Alcotest.(check bool)
        "the definition site is retained" true
        (Option.is_some origin.Semantic_symbol.defined_at))
    [ backing_origin; base_origin ]

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
  let directory = Filename.temp_dir "holyc-aggregate-headers-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "aggregate.HC" in
      write_file root_path "#include \"aggregate\"";
      write_file include_path
        "class IncludedRoot {}; I64i class Included : IncludedRoot {};";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let header = resolve session ast |> fun results -> header_named results "Included" in
      let origins =
        [
          (header |> required_backing
          |> Semantic_aggregate_header_resolution.backing_spelling_origin);
          (header |> required_base
          |> Semantic_aggregate_header_resolution.base_name_origin);
        ]
      in
      List.iter
        (fun site_origin ->
          let site_origin = source_origin site_origin in
          let source =
            Source_manager.find (Session.sources session) site_origin.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "the included site keeps its source" "aggregate.HC"
            (Source_file.path source |> Filename.basename))
        origins)

let modes_determinism_and_purity () =
  List.iter
    (fun mode ->
      let session = Session.create () in
      let ast =
        parse session ~mode ~path:"header-modes.HC"
          "I64i class Root {}; class Child : Root {};"
      in
      let declarations =
        checked (Holyc_lib.collect_declarations session ast)
      in
      let aggregates =
        checked (Holyc_lib.resolve_aggregates session ~declarations ast)
      in
      let table = Session.semantic_symbols session in
      let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
      let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
      let resolve () =
        checked
          (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates
             ast)
      in
      let signature resolution =
        Semantic_aggregate_header_resolution.headers resolution
        |> List.map (fun header ->
            ( Semantic_aggregate_header_resolution.header_item_index header,
              header
              |> Semantic_aggregate_header_resolution.header_symbol
              |> symbol_id ))
      in
      let first = resolve () in
      let second = resolve () in
      Alcotest.(check (list (pair int int)))
        "repeated header resolution is deterministic" (signature first)
        (signature second);
      Alcotest.(check int)
        "header resolution creates no scope" scope_count
        (Semantic_symbol_table.all_scopes table |> List.length);
      Alcotest.(check int)
        "header resolution creates no symbol" symbol_count
        (Semantic_symbol_table.all_symbols table |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let mismatched_inputs () =
  let session = Session.create () in
  let ast = parse session ~path:"original-header.HC" "class A {};" in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let other_ast = parse session ~path:"other-header.HC" "class A {};" in
  let other_declarations =
    checked (Holyc_lib.collect_declarations session other_ast)
  in
  let other_aggregates =
    checked
      (Holyc_lib.resolve_aggregates session ~declarations:other_declarations
         other_ast)
  in
  let table = Session.semantic_symbols session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check bool)
    "a different AST is rejected" true
    (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates
       other_ast
    |> Result.is_error);
  Alcotest.(check bool)
    "a different reconciliation is rejected" true
    (Holyc_lib.resolve_aggregate_headers session ~declarations
       ~aggregates:other_aggregates ast
    |> Result.is_error);
  let foreign_session = Session.create () in
  Alcotest.(check bool)
    "header facts cannot cross sessions" true
    (Holyc_lib.resolve_aggregate_headers foreign_session ~declarations
       ~aggregates ast
    |> Result.is_error);
  Alcotest.(check int)
    "rejected inputs create no scope" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "rejected inputs create no symbol" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let low_level_validation () =
  let table = Semantic_symbol_table.create () in
  let root = Semantic_symbol_table.root table in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:root
         ~kind:Semantic_symbol_table.Module ~name:"manual.HC" ())
  in
  let aggregate name =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name
         ~kind:Semantic_symbol.Aggregate_type
         ~origin:(Semantic_symbol.Synthesized (name ^ " aggregate")))
  in
  let make_header symbol item_index =
    checked
      (Semantic_aggregate_header_resolution.make_header ~symbol
         ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index
         ~origin:(Semantic_symbol.Synthesized "aggregate definition")
         ~keyword_origin:(Semantic_symbol.Synthesized "class keyword")
         ~backing:None ~base:None)
  in
  let first = aggregate "First" |> fun symbol -> make_header symbol 0 in
  let second = aggregate "Second" |> fun symbol -> make_header symbol 1 in
  Alcotest.(check bool)
    "reversed headers are rejected" true
    (Semantic_aggregate_header_resolution.resolve ~table ~parent:module_scope
       [ second; first ]
    |> Result.is_error);
  Alcotest.(check bool)
    "repeated header symbols are rejected" true
    (Semantic_aggregate_header_resolution.resolve ~table ~parent:module_scope
       [ first; first ]
    |> Result.is_error);
  let global =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"value"
         ~kind:Semantic_symbol.Global_variable
         ~origin:(Semantic_symbol.Synthesized "test global"))
  in
  Alcotest.(check bool)
    "a global cannot become an aggregate type" true
    (Semantic_type.make_aggregate ~symbol:global ~pointer_depth:0
    |> Result.is_error);
  Alcotest.(check bool)
    "a global cannot become an aggregate base" true
    (Semantic_aggregate_header_resolution.make_base_site ~spelling:"value"
       ~origin:(Semantic_symbol.Synthesized "base site")
       ~colon_origin:(Semantic_symbol.Synthesized "base colon")
       ~name_origin:(Semantic_symbol.Synthesized "base name") ~symbol:global
    |> Result.is_error);
  let pointer_type =
    checked
      (Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
         ~primitive:Primitive_type.I64 ~pointer_depth:1)
  in
  Alcotest.(check bool)
    "pointer provenance must match pointer depth" true
    (Semantic_aggregate_header_resolution.make_backing_site ~spelling:"I64"
       ~origin:(Semantic_symbol.Synthesized "pointer backing")
       ~spelling_origin:(Semantic_symbol.Synthesized "pointer type")
       ~pointer_origins:[] ~resolved_type:pointer_type
    |> Result.is_error);
  let foreign_table = Semantic_symbol_table.create () in
  let foreign_scope =
    checked
      (Semantic_symbol_table.create_scope foreign_table
         ~parent:(Semantic_symbol_table.root foreign_table)
         ~kind:Semantic_symbol_table.Module ~name:"foreign.HC" ())
  in
  let foreign =
    checked
      (Semantic_symbol_table.add foreign_table ~scope:foreign_scope
         ~name:"Foreign" ~kind:Semantic_symbol.Aggregate_type
         ~origin:(Semantic_symbol.Synthesized "foreign aggregate"))
  in
  let foreign_type =
    checked (Semantic_type.make_aggregate ~symbol:foreign ~pointer_depth:0)
  in
  let foreign_backing =
    checked
      (Semantic_aggregate_header_resolution.make_backing_site
         ~spelling:"Foreign"
         ~origin:(Semantic_symbol.Synthesized "foreign use")
         ~spelling_origin:(Semantic_symbol.Synthesized "foreign name")
         ~pointer_origins:[]
         ~resolved_type:foreign_type)
  in
  let owner = aggregate "Owner" in
  let foreign_header =
    checked
      (Semantic_aggregate_header_resolution.make_header ~symbol:owner
         ~aggregate_kind:Semantic_aggregate_resolution.Class ~item_index:2
         ~origin:(Semantic_symbol.Synthesized "owner definition")
         ~keyword_origin:(Semantic_symbol.Synthesized "class keyword")
         ~backing:(Some foreign_backing) ~base:None)
  in
  Alcotest.(check bool)
    "a foreign backing target is rejected" true
    (Semantic_aggregate_header_resolution.resolve ~table ~parent:module_scope
       [ foreign_header ]
    |> Result.is_error)

let tests =
  [
    Alcotest.test_case "primitive and internal backings" `Quick
      primitive_and_internal_backings;
    Alcotest.test_case "pointer depths" `Quick pointer_depths;
    Alcotest.test_case "backing prepublication" `Quick
      backing_uses_prepublication_state;
    Alcotest.test_case "base postpublication" `Quick
      base_uses_postpublication_state;
    Alcotest.test_case "newest forward is canonical" `Quick
      newest_forward_is_canonical;
    Alcotest.test_case "generated provenance" `Quick generated_provenance;
    Alcotest.test_case "included provenance" `Quick included_provenance;
    Alcotest.test_case "modes, determinism, and purity" `Quick
      modes_determinism_and_purity;
    Alcotest.test_case "mismatched inputs" `Quick mismatched_inputs;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
  ]
