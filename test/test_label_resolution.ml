open Holyc_lib

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-labels-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

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
  Holyc_lib.parse session ~source |> expect_ast

let collect session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let labels = checked (Holyc_lib.resolve_labels session ~functions ast) in
  (declarations, functions, labels)

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let function_name function_ =
  Semantic_label_resolution.function_symbol function_ |> Semantic_symbol.name

let resolved_function name resolution =
  Semantic_label_resolution.functions resolution
  |> List.find (fun function_ -> String.equal (function_name function_) name)

let label_symbol_count session =
  Session.semantic_symbols session
  |> Semantic_symbol_table.all_symbols
  |> List.filter (fun symbol ->
      Semantic_symbol.equal_kind
        (Semantic_symbol.kind symbol)
        Semantic_symbol.Label)
  |> List.length

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let make_function table module_scope name =
  let symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized (name ^ " function")))
  in
  let scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:module_scope
         ~kind:Semantic_symbol_table.Function ~name ())
  in
  (symbol, scope)

let definition ?(kind = Semantic_label_resolution.Language_label) name index =
  checked
    (Semantic_label_resolution.make_definition ~name ~definition_kind:kind
       ~origin:(Semantic_symbol.Synthesized (name ^ " definition"))
       ~occurrence_index:index)

let goto name index =
  checked
    (Semantic_label_resolution.make_goto ~name
       ~origin:(Semantic_symbol.Synthesized (name ^ " goto"))
       ~occurrence_index:index)

let label_names function_ =
  Semantic_label_resolution.function_labels function_
  |> List.map (fun label ->
      Semantic_label_resolution.label_symbol label |> Semantic_symbol.name)

let stable_identities_and_source_order () =
  let table = Semantic_symbol_table.create () in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table
         ~parent:(Semantic_symbol_table.root table)
         ~kind:Semantic_symbol_table.Module ~name:"labels.HC" ())
  in
  let function_symbol, function_scope =
    make_function table module_scope "Flow"
  in
  let facts =
    checked
      (Semantic_label_resolution.make_function ~symbol:function_symbol
         ~scope:function_scope ~item_index:0
         [
           goto "later" 0;
           definition "back" 1;
           goto "back" 2;
           definition "later" 3;
           goto "later" 4;
           definition "asm_target" 5
             ~kind:Semantic_label_resolution.Assembly_global_label;
         ])
  in
  let resolution =
    checked (Semantic_label_resolution.resolve ~table [ facts ])
  in
  let function_ = Semantic_label_resolution.functions resolution |> List.hd in
  Alcotest.(check string)
    "function identity" "Flow"
    (Semantic_label_resolution.function_symbol function_ |> Semantic_symbol.name);
  Alcotest.(check (list string))
    "first occurrence controls stable label order"
    [ "later"; "back"; "asm_target" ]
    (label_names function_);
  let labels = Semantic_label_resolution.function_labels function_ in
  Alcotest.(check (list int))
    "goto counts do not include definitions" [ 2; 1; 0 ]
    (List.map Semantic_label_resolution.label_goto_count labels);
  Alcotest.(check (list int))
    "assembly definitions suppress the unused warning" [ 2; 1; 1 ]
    (List.map Semantic_label_resolution.label_use_count labels);
  Alcotest.(check (list string))
    "definition kinds remain available"
    [ "language"; "language"; "assembly-global" ]
    (List.map
       (fun label ->
         Semantic_label_resolution.label_definition_kind label
         |> Semantic_label_resolution.definition_kind_name)
       labels);
  Alcotest.(check (list int))
    "first occurrence indexes are retained" [ 0; 1; 5 ]
    (List.map Semantic_label_resolution.label_first_occurrence_index labels);
  let occurrences = Semantic_label_resolution.function_occurrences function_ in
  let later_id =
    Semantic_label_resolution.label_symbol (List.nth labels 0) |> symbol_id
  in
  Alcotest.(check (list int))
    "every later occurrence resolves to one label identity"
    [ later_id; later_id; later_id ]
    (occurrences
    |> List.filter (fun occurrence ->
        Semantic_label_resolution.occurrence_symbol occurrence
        |> Semantic_symbol.name |> String.equal "later")
    |> List.map (fun occurrence ->
        Semantic_label_resolution.occurrence_symbol occurrence |> symbol_id));
  Alcotest.(check (list string))
    "occurrence order and kinds stay source shaped"
    [
      "goto-reference";
      "definition:language";
      "goto-reference";
      "definition:language";
      "goto-reference";
      "definition:assembly-global";
    ]
    (List.map
       (fun occurrence ->
         Semantic_label_resolution.occurrence_kind occurrence
         |> Semantic_label_resolution.occurrence_kind_name)
       occurrences);
  let sources = Source_manager.create () in
  let human = Semantic_symbol_table.human sources table in
  let json = Semantic_symbol_table.json sources table in
  Alcotest.(check string)
    "human symbol dump remains deterministic" human
    (Semantic_symbol_table.human sources table);
  Alcotest.(check string)
    "JSON symbol dump remains deterministic" json
    (Semantic_symbol_table.json sources table)

let rejected_batches_do_not_mutate () =
  let table = Semantic_symbol_table.create () in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table
         ~parent:(Semantic_symbol_table.root table)
         ~kind:Semantic_symbol_table.Module ~name:"invalid-labels.HC" ())
  in
  let function_symbol, function_scope =
    make_function table module_scope "Bad"
  in
  let function_fact occurrences =
    checked
      (Semantic_label_resolution.make_function ~symbol:function_symbol
         ~scope:function_scope ~item_index:0 occurrences)
  in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let duplicate =
    function_fact [ definition "twice" 0; definition "twice" 1 ]
  in
  Alcotest.(check bool)
    "duplicate definitions are rejected" true
    (Semantic_label_resolution.resolve ~table [ duplicate ] |> Result.is_error);
  Alcotest.(check int)
    "duplicate rejection precedes symbol insertion" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let missing = function_fact [ goto "missing" 0 ] in
  Alcotest.(check bool)
    "undefined goto targets are rejected" true
    (Semantic_label_resolution.resolve ~table [ missing ] |> Result.is_error);
  Alcotest.(check int)
    "undefined-target rejection precedes symbol insertion" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  Alcotest.(check bool)
    "out-of-order occurrences are rejected by the checked constructor" true
    (Semantic_label_resolution.make_function ~symbol:function_symbol
       ~scope:function_scope ~item_index:0
       [ definition "late" 2; definition "early" 1 ]
    |> Result.is_error);
  let other_table = Semantic_symbol_table.create () in
  let other_module =
    checked
      (Semantic_symbol_table.create_scope other_table
         ~parent:(Semantic_symbol_table.root other_table)
         ~kind:Semantic_symbol_table.Module ~name:"other.HC" ())
  in
  let foreign_symbol, foreign_scope =
    make_function other_table other_module "Foreign"
  in
  let foreign =
    checked
      (Semantic_label_resolution.make_function ~symbol:foreign_symbol
         ~scope:foreign_scope ~item_index:0
         [ definition "owned_elsewhere" 0 ])
  in
  Alcotest.(check bool)
    "foreign function inputs are rejected" true
    (Semantic_label_resolution.resolve ~table [ foreign ] |> Result.is_error);
  Alcotest.(check int)
    "foreign rejection does not change this table" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let ast_forward_and_backward_resolution () =
  let session = Session.create () in
  let ast =
    parse session ~path:"flow.HC"
      "U0 Flow(){goto later;back:goto back;later:goto later;}"
  in
  let _, _, resolution = collect session ast in
  let function_ = resolved_function "Flow" resolution in
  Alcotest.(check (list string))
    "AST traversal preserves first occurrence order" [ "later"; "back" ]
    (label_names function_);
  let labels = Semantic_label_resolution.function_labels function_ in
  Alcotest.(check (list int))
    "forward and backward gotos retain their counts" [ 2; 1 ]
    (List.map Semantic_label_resolution.label_goto_count labels);
  let occurrences = Semantic_label_resolution.function_occurrences function_ in
  Alcotest.(check (list int))
    "AST occurrence indexes are contiguous" [ 0; 1; 2; 3; 4 ]
    (List.map Semantic_label_resolution.occurrence_index occurrences);
  List.iter
    (fun occurrence ->
      match Semantic_label_resolution.occurrence_origin occurrence with
      | Semantic_symbol.Source_location source ->
          Alcotest.(check bool)
            "AST occurrence keeps at least one source segment" true
            (source.source_segments <> [])
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected an AST source origin")
    occurrences

let recursive_statement_order () =
  let source =
    "U0 Nested(I64 active){\n\
     root:\n\
     {block_label:}\n\
     if(active){then_label:}else{else_label:}\n\
     while(active){while_label:break;}\n\
     do {do_label:break;} while(active);\n\
     for(;active;goto for_label){for_label:break;}\n\
     goto sequence_label,sequence_label:,goto sequence_label;\n\
     lock {lock_label:}\n\
     try {try_label:} catch {catch_label:}\n\
     switch(active){case 0:switch_label:\n\
     start:case 1:subswitch_label:end:}\n\
     }"
  in
  let session = Session.create () in
  let ast = parse session ~path:"nested-labels.HC" source in
  let _, _, resolution = collect session ast in
  let function_ = resolved_function "Nested" resolution in
  Alcotest.(check (list string))
    "every recursive statement route contributes in source order"
    [
      "root";
      "block_label";
      "then_label";
      "else_label";
      "while_label";
      "do_label";
      "for_label";
      "sequence_label";
      "lock_label";
      "try_label";
      "catch_label";
      "switch_label";
      "subswitch_label";
    ]
    (label_names function_);
  let labels = Semantic_label_resolution.function_labels function_ in
  let find name =
    List.find
      (fun label ->
        Semantic_label_resolution.label_symbol label
        |> Semantic_symbol.name |> String.equal name)
      labels
  in
  Alcotest.(check int)
    "the for-update goto resolves into its body" 1
    (find "for_label" |> Semantic_label_resolution.label_goto_count);
  Alcotest.(check int)
    "the sequence retains both gotos" 2
    (find "sequence_label" |> Semantic_label_resolution.label_goto_count)

let assembly_definitions_share_the_namespace () =
  let source =
    "U0 Cross(){\n\
     goto ASM_TARGET;\n\
     asm {\n\
     ASM_TARGET:\n\
     ASM_TARGET:\n\
     @@local:\n\
     EXPORTED::\n\
     }\n\
     }"
  in
  let session = Session.create () in
  let ast = parse session ~path:"assembly-labels.HC" source in
  let _, _, resolution = collect session ast in
  let function_ = resolved_function "Cross" resolution in
  Alcotest.(check (list string))
    "language and assembly definitions share one order"
    [ "ASM_TARGET"; "@@local"; "EXPORTED" ]
    (label_names function_);
  let labels = Semantic_label_resolution.function_labels function_ in
  Alcotest.(check (list string))
    "assembly label forms stay distinct"
    [ "assembly-global"; "assembly-local"; "assembly-exported-global" ]
    (List.map
       (fun label ->
         Semantic_label_resolution.label_definition_kind label
         |> Semantic_label_resolution.definition_kind_name)
       labels);
  Alcotest.(check (list int))
    "each assembly definition carries its warning-suppression use" [ 2; 1; 1 ]
    (List.map Semantic_label_resolution.label_use_count labels);
  Alcotest.(check (list int))
    "same-address assembly repeats remain pending address validation"
    [ 2; 1; 1 ]
    (List.map Semantic_label_resolution.label_definition_count labels)

let invalid_ast_batches_do_not_add_labels () =
  let cases =
    [
      ("top-level goto", "goto missing;", "outside a function body");
      ("top-level label", "outside:", "outside a function body");
      ( "undefined function target",
        "U0 Missing(){goto nowhere;}",
        "is not defined" );
      ( "duplicate language target",
        "U0 Duplicate(){same:same:}",
        "defined more than once" );
      ( "language and assembly collision",
        "U0 Mixed(){same:asm {\nsame:\n}}",
        "defined more than once" );
    ]
  in
  List.iter
    (fun (name, source, expected) ->
      let session = Session.create () in
      let ast = parse session ~path:(name ^ ".HC") source in
      let declarations = checked (Holyc_lib.collect_declarations session ast) in
      let functions =
        checked (Holyc_lib.collect_functions session ~declarations ast)
      in
      let before = label_symbol_count session in
      let result = Holyc_lib.resolve_labels session ~functions ast in
      let message =
        match result with
        | Error message -> message
        | Ok _ -> Alcotest.failf "%s unexpectedly resolved" name
      in
      Alcotest.(check bool)
        (name ^ " has a specific failure")
        true
        (contains message expected);
      Alcotest.(check int)
        (name ^ " does not insert labels")
        before
        (label_symbol_count session))
    cases

let separate_function_scopes_and_mismatched_inputs () =
  let session = Session.create () in
  let ast =
    parse session ~path:"separate.HC"
      "extern U0 Proto();U0 One(){same:}U0 Two(){goto same;same:}"
  in
  let declarations, functions, resolution = collect session ast in
  let resolved = Semantic_label_resolution.functions resolution in
  Alcotest.(check (list string))
    "prototypes and definitions stay aligned" [ "Proto"; "One"; "Two" ]
    (List.map function_name resolved);
  Alcotest.(check (list int))
    "a prototype has no labels" [ 0; 1; 1 ]
    (List.map
       (fun function_ ->
         Semantic_label_resolution.function_labels function_ |> List.length)
       resolved);
  let one = resolved_function "One" resolution in
  let two = resolved_function "Two" resolution in
  let one_id =
    Semantic_label_resolution.function_labels one
    |> List.hd |> Semantic_label_resolution.label_symbol |> symbol_id
  in
  let two_id =
    Semantic_label_resolution.function_labels two
    |> List.hd |> Semantic_label_resolution.label_symbol |> symbol_id
  in
  Alcotest.(check bool)
    "the same spelling gets a different identity in each function" true
    (one_id <> two_id);
  let other_ast =
    parse session ~path:"other-functions.HC" "U0 Other(){done:}"
  in
  let before = label_symbol_count session in
  Alcotest.(check bool)
    "a function collection from another AST is rejected" true
    (Holyc_lib.resolve_labels session ~functions other_ast |> Result.is_error);
  Alcotest.(check int)
    "the mismatched AST does not add labels" before
    (label_symbol_count session);
  ignore declarations

let definition_provenance_and_foreign_ownership () =
  let session = Session.create () in
  let ast =
    parse session ~path:"generated-labels.HC"
      "#define JUMP goto destination\n\
       #define DEST destination:\n\
       U0 Generated(){JUMP;DEST}"
  in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let resolution = checked (Holyc_lib.resolve_labels session ~functions ast) in
  let function_ = resolved_function "Generated" resolution in
  List.iter
    (fun occurrence ->
      match Semantic_label_resolution.occurrence_origin occurrence with
      | Semantic_symbol.Source_location source ->
          Alcotest.(check bool)
            "definition-backed occurrences keep their definition origin" true
            (Option.is_some source.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected definition-backed source provenance")
    (Semantic_label_resolution.function_occurrences function_);
  let foreign_session = Session.create () in
  let before = label_symbol_count foreign_session in
  Alcotest.(check bool)
    "a function collection cannot cross session ownership" true
    (Holyc_lib.resolve_labels foreign_session ~functions ast |> Result.is_error);
  Alcotest.(check int)
    "foreign ownership fails before label insertion" before
    (label_symbol_count foreign_session)

let included_source_provenance () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let included_file = Filename.concat root "labels.HC" in
      let root_contents = "#include \"labels\"" in
      write_file root_file root_contents;
      write_file included_file "U0 Included(){goto done;done:}";
      let session = Session.create () in
      let source =
        Session.add_source session ~path:root_file ~contents:root_contents
      in
      let config =
        checked (Preprocessor.Config.create ~working_directory:root ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let _, _, resolution = collect session ast in
      let function_ = resolved_function "Included" resolution in
      List.iter
        (fun occurrence ->
          match Semantic_label_resolution.occurrence_origin occurrence with
          | Semantic_symbol.Source_location source ->
              let source_file =
                Source_manager.find (Session.sources session) source.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "included occurrences retain their requested display path"
                "labels"
                (Source_file.display_path source_file |> Filename.basename);
              Alcotest.(check string)
                "included occurrences retain their canonical source" "labels.HC"
                (Source_file.path source_file |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included source provenance")
        (Semantic_label_resolution.function_occurrences function_))

let tests =
  [
    Alcotest.test_case "stable identities and source order" `Quick
      stable_identities_and_source_order;
    Alcotest.test_case "rejected batches do not mutate" `Quick
      rejected_batches_do_not_mutate;
    Alcotest.test_case "AST forward and backward resolution" `Quick
      ast_forward_and_backward_resolution;
    Alcotest.test_case "recursive statement order" `Quick
      recursive_statement_order;
    Alcotest.test_case "assembly definitions share the namespace" `Quick
      assembly_definitions_share_the_namespace;
    Alcotest.test_case "invalid AST batches do not add labels" `Quick
      invalid_ast_batches_do_not_add_labels;
    Alcotest.test_case "separate function scopes and mismatched inputs" `Quick
      separate_function_scopes_and_mismatched_inputs;
    Alcotest.test_case "definition provenance and foreign ownership" `Quick
      definition_provenance_and_foreign_ownership;
    Alcotest.test_case "included source provenance" `Quick
      included_source_provenance;
  ]
