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

let collect session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  (declarations, functions)

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let scope_id scope =
  Semantic_symbol_table.scope_id scope |> Semantic_symbol.Scope_id.to_int

let function_name function_ =
  Semantic_function_collection.function_symbol function_ |> Semantic_symbol.name

let entries function_ = Semantic_function_collection.function_entries function_

let entry_names function_ =
  entries function_
  |> List.map (fun entry ->
      Semantic_function_collection.entry_symbol entry |> Semantic_symbol.name)

let entry_kinds function_ =
  entries function_
  |> List.map (fun entry ->
      Semantic_function_collection.entry_kind entry
      |> Semantic_function_collection.binding_kind_name)

let local_entries function_ =
  entries function_
  |> List.filter (fun entry ->
      Semantic_function_collection.entry_parameter_index entry |> Option.is_none)

let signature_slots_and_scopes () =
  let source =
    "extern I64 Proto(I64 named,I64,U8 *tail,...);\n\
     I64 Defined(I64 first,I64,I64 third,...){return first;}"
  in
  let session = Session.create () in
  let _, ast = parse session ~path:"signatures.HC" source in
  let declarations, collection = collect session ast in
  let functions = Semantic_function_collection.functions collection in
  Alcotest.(check (list string))
    "function order" [ "Proto"; "Defined" ]
    (List.map function_name functions);
  Alcotest.(check (list int))
    "module item indexes" [ 0; 1 ]
    (List.map Semantic_function_collection.function_item_index functions);
  let proto = List.nth functions 0 in
  let defined = List.nth functions 1 in
  List.iter
    (fun function_ ->
      Alcotest.(check (list string))
        (function_name function_ ^ " parameter names")
        (if String.equal (function_name function_) "Proto" then
           [ "named"; "tail"; "argc"; "argv" ]
         else [ "first"; "third"; "argc"; "argv" ])
        (entry_names function_);
      Alcotest.(check (list (option int)))
        (function_name function_ ^ " parameter indexes")
        [ Some 0; Some 2; Some 3; Some 4 ]
        (entries function_
        |> List.map Semantic_function_collection.entry_parameter_index);
      Alcotest.(check (list string))
        (function_name function_ ^ " binding kinds")
        [
          "named-parameter"; "named-parameter"; "variadic-argc"; "variadic-argv";
        ]
        (entry_kinds function_);
      Alcotest.(check (list string))
        (function_name function_ ^ " symbol kinds")
        [ "parameter"; "parameter"; "parameter"; "parameter" ]
        (entries function_
        |> List.map (fun entry ->
            Semantic_function_collection.entry_symbol entry
            |> Semantic_symbol.kind |> Semantic_symbol.kind_name)))
    functions;
  let declaration_function_ids =
    Semantic_declaration_collection.entries declarations
    |> List.filter_map (fun entry ->
        match Semantic_declaration_collection.entry_kind entry with
        | Semantic_declaration_collection.Function_prototype
        | Semantic_declaration_collection.Function_definition ->
            Some
              (Semantic_declaration_collection.entry_symbol entry |> symbol_id)
        | Semantic_declaration_collection.Aggregate_forward
        | Semantic_declaration_collection.Aggregate_definition
        | Semantic_declaration_collection.Aggregate_attached_global
        | Semantic_declaration_collection.Global_variable -> None)
  in
  Alcotest.(check (list int))
    "function scopes retain top-level identities" declaration_function_ids
    (List.map
       (fun function_ ->
         Semantic_function_collection.function_symbol function_ |> symbol_id)
       functions);
  Alcotest.(check (list int))
    "function scope IDs" [ 2; 3 ]
    (List.map
       (fun function_ ->
         Semantic_function_collection.function_scope function_ |> scope_id)
       functions);
  Alcotest.(check int)
    "prototype has no locals" 0
    (local_entries proto |> List.length);
  Alcotest.(check int)
    "definition has no declared locals" 0
    (local_entries defined |> List.length)

let recursive_local_order_and_storage () =
  let source =
    "U0 Contexts(I64 marker){\n\
     I64 root_a=0,root_b=1;\n\
     static I64 stored_a=0,stored_b=1;\n\
     for(;marker;marker--){I64 for_local=0;break;}\n\
     if(marker){I64 then_local=0;}else{static I64 else_local=0;}\n\
     1,I64 sequence_a=0,sequence_b=1;\n\
     lock {I64 lock_local=0;}\n\
     try {I64 try_local=0;} catch {I64 catch_local=0;}\n\
     while(marker){I64 while_local=0;break;}\n\
     do {I64 do_local=0;break;} while(marker);\n\
     switch(marker){case 0:{I64 switch_local=0;}\n\
     start:case 1:{I64 subswitch_local=0;}end:}\n\
     }"
  in
  let session = Session.create () in
  let _, ast = parse session ~path:"recursive-locals.HC" source in
  let _, collection = collect session ast in
  let function_ =
    Semantic_function_collection.functions collection |> List.hd
  in
  let locals = local_entries function_ in
  Alcotest.(check (list string))
    "locals follow parser order"
    [
      "root_a";
      "root_b";
      "stored_a";
      "stored_b";
      "for_local";
      "then_local";
      "else_local";
      "sequence_a";
      "sequence_b";
      "lock_local";
      "try_local";
      "catch_local";
      "while_local";
      "do_local";
      "switch_local";
      "subswitch_local";
    ]
    (locals
    |> List.map (fun entry ->
        Semantic_function_collection.entry_symbol entry |> Semantic_symbol.name)
    );
  Alcotest.(check (list int))
    "local declaration indexes"
    [ 0; 0; 1; 1; 2; 3; 4; 5; 5; 6; 7; 8; 9; 10; 11; 12 ]
    (locals
    |> List.map (fun entry ->
        Semantic_function_collection.entry_local_declaration_index entry
        |> Option.get));
  Alcotest.(check (list int))
    "grouped declarator indexes"
    [ 0; 1; 0; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0 ]
    (locals
    |> List.map (fun entry ->
        Semantic_function_collection.entry_declarator_index entry |> Option.get)
    );
  Alcotest.(check (list string))
    "storage classes"
    [
      "automatic-local";
      "automatic-local";
      "static-local";
      "static-local";
      "automatic-local";
      "automatic-local";
      "static-local";
      "automatic-local";
      "automatic-local";
      "automatic-local";
      "automatic-local";
      "automatic-local";
      "automatic-local";
      "automatic-local";
      "automatic-local";
      "automatic-local";
    ]
    (List.map
       (fun entry ->
         Semantic_function_collection.entry_kind entry
         |> Semantic_function_collection.binding_kind_name)
       locals)

let repeated_names_use_one_function_scope () =
  let session = Session.create () in
  let _, ast =
    parse session ~path:"shadowing.HC"
      "U0 Shadow(I64 value){{I64 value=1;}if(value){static I64 value=2;}}"
  in
  let declarations, collection = collect session ast in
  let function_ =
    Semantic_function_collection.functions collection |> List.hd
  in
  let function_entries = entries function_ in
  Alcotest.(check (list string))
    "repeated names survive collection"
    [ "value"; "value"; "value" ]
    (entry_names function_);
  Alcotest.(check int)
    "nested blocks create no semantic block scopes" 3
    (Semantic_symbol_table.all_scopes (Session.semantic_symbols session)
    |> List.length);
  let ids =
    List.map
      (fun entry ->
        Semantic_function_collection.entry_symbol entry |> symbol_id)
      function_entries
  in
  let table = Session.semantic_symbols session in
  let scope = Semantic_function_collection.function_scope function_ in
  let lookup instance =
    checked
      (Semantic_symbol_table.lookup_local table ~scope ~name:"value"
         ~kinds:[ Semantic_symbol.Parameter; Semantic_symbol.Local_variable ]
         ~instance ())
    |> Option.map symbol_id
  in
  Alcotest.(check (option int))
    "newest nested local wins"
    (Some (List.nth ids 2))
    (lookup 1);
  Alcotest.(check (option int))
    "older nested local remains addressable"
    (Some (List.nth ids 1))
    (lookup 2);
  Alcotest.(check (option int))
    "parameter remains addressable"
    (Some (List.nth ids 0))
    (lookup 3);
  Alcotest.(check int)
    "function scope is parented by its module"
    (Semantic_declaration_collection.scope declarations |> scope_id)
    (Semantic_symbol_table.parent scope |> Option.get |> scope_id)

let generated_and_variadic_provenance () =
  let session = Session.create () in
  let root, ast =
    parse session ~path:"generated-bindings.HC"
      "#define PARAM generated_parameter\n\
       #define LOCAL generated_local\n\
       U0 Generated(I64 PARAM,...){I64 LOCAL;}"
  in
  let _, collection = collect session ast in
  let function_ =
    Semantic_function_collection.functions collection |> List.hd
  in
  let binding name =
    entries function_
    |> List.find (fun entry ->
        Semantic_function_collection.entry_symbol entry
        |> Semantic_symbol.name |> String.equal name)
    |> Semantic_function_collection.entry_symbol
  in
  List.iter
    (fun name ->
      match Semantic_symbol.origin (binding name) with
      | Semantic_symbol.Source_location origin ->
          Alcotest.(check bool)
            (name ^ " leaves the root frame")
            false
            (Source_id.equal origin.span.source (Source_file.id root));
          Alcotest.(check bool)
            (name ^ " retains its invocation")
            true
            (Option.is_some origin.generated_from);
          Alcotest.(check bool)
            (name ^ " retains its definition")
            true
            (Option.is_some origin.defined_at);
          Alcotest.(check bool)
            (name ^ " retains source segments")
            true
            (origin.source_segments <> [])
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.failf "%s lost its generated source location" name)
    [ "generated_parameter"; "generated_local" ];
  let argc_origin = Semantic_symbol.origin (binding "argc") in
  let argv_origin = Semantic_symbol.origin (binding "argv") in
  Alcotest.(check bool)
    "argc and argv share the ellipsis origin" true
    (argc_origin = argv_origin);
  match argc_origin with
  | Semantic_symbol.Source_location origin ->
      Alcotest.(check bool)
        "ellipsis remains in the root source" true
        (Source_id.equal origin.span.source (Source_file.id root));
      Alcotest.(check int)
        "ellipsis spans three bytes" 3 (Span.length origin.span);
      Alcotest.(check bool)
        "direct ellipsis has no generated origin" true
        (Option.is_none origin.generated_from
        && Option.is_none origin.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "variadic symbols lost the ellipsis location"

let distinct_declarations_and_absent_body () =
  let session = Session.create () in
  let _, ast =
    parse session ~path:"declaration-kinds.HC"
      "extern U0 Repeat(I64 prototype);\n\
       U0 Repeat(I64 definition){I64 local;}\n\
       U0 Absent(I64 lone)"
  in
  let _, collection = collect session ast in
  let functions = Semantic_function_collection.functions collection in
  Alcotest.(check (list string))
    "prototype and definitions keep separate scopes"
    [ "Repeat"; "Repeat"; "Absent" ]
    (List.map function_name functions);
  Alcotest.(check (list (list string)))
    "each declaration keeps its own bindings"
    [ [ "prototype" ]; [ "definition"; "local" ]; [ "lone" ] ]
    (List.map entry_names functions);
  Alcotest.(check (list int))
    "scope IDs remain stable" [ 2; 3; 4 ]
    (List.map
       (fun function_ ->
         Semantic_function_collection.function_scope function_ |> scope_id)
       functions);
  Alcotest.(check int)
    "an absent body adds no locals" 0
    (List.nth functions 2 |> local_entries |> List.length)

let mismatched_inputs_do_not_mutate () =
  let session = Session.create () in
  let _, first = parse session ~path:"first.HC" "U0 First(){I64 one;}" in
  let _, second = parse session ~path:"second.HC" "U0 Second(){I64 two;}" in
  let declarations = checked (Holyc_lib.collect_declarations session first) in
  let table = Session.semantic_symbols session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check bool)
    "a collection from another AST is rejected" true
    (Holyc_lib.collect_functions session ~declarations second |> Result.is_error);
  Alcotest.(check int)
    "AST mismatch preserves scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "AST mismatch preserves symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let other_session = Session.create () in
  let _, other_ast =
    parse other_session ~path:"other.HC" "U0 Other(){I64 local;}"
  in
  let other_declarations =
    checked (Holyc_lib.collect_declarations other_session other_ast)
  in
  Alcotest.(check bool)
    "a module scope from another table is rejected" true
    (Holyc_lib.collect_functions session ~declarations:other_declarations
       other_ast
    |> Result.is_error);
  Alcotest.(check int)
    "foreign scope preserves scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "foreign scope preserves symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let kind_session = Session.create () in
  let _, prototype_ast =
    parse kind_session ~path:"kind.HC" "extern U0 Same(I64 value);"
  in
  let prototype =
    match prototype_ast.items with
    | [ Ast.Function_prototype prototype ] -> prototype
    | _ -> Alcotest.fail "expected one prototype for the kind mismatch"
  in
  let location = prototype.name.location in
  let origin =
    Semantic_symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  let wrong_declaration =
    checked
      (Semantic_declaration_collection.make_declaration
         ~name:prototype.name.spelling
         ~declaration_kind:Semantic_declaration_collection.Function_definition
         ~origin ~item_index:0 ())
  in
  let kind_table = Session.semantic_symbols kind_session in
  let wrong_collection =
    checked
      (Semantic_declaration_collection.collect ~table:kind_table
         ~module_name:"kind.HC" [ wrong_declaration ])
  in
  let kind_scope_count =
    Semantic_symbol_table.all_scopes kind_table |> List.length
  in
  let kind_symbol_count =
    Semantic_symbol_table.all_symbols kind_table |> List.length
  in
  Alcotest.(check bool)
    "a prototype paired with a definition entry is rejected" true
    (Holyc_lib.collect_functions kind_session ~declarations:wrong_collection
       prototype_ast
    |> Result.is_error);
  Alcotest.(check int)
    "kind mismatch preserves scopes" kind_scope_count
    (Semantic_symbol_table.all_scopes kind_table |> List.length);
  Alcotest.(check int)
    "kind mismatch preserves symbols" kind_symbol_count
    (Semantic_symbol_table.all_symbols kind_table |> List.length)

let low_level_validation_and_dumps () =
  let origin = Semantic_symbol.Synthesized "function collection test" in
  let named ?(name = "parameter") index =
    Semantic_function_collection.make_named_parameter ~name ~origin
      ~parameter_index:index
  in
  let local ?(declaration_index = 0) ?(declarator_index = 0) () =
    Semantic_function_collection.make_local ~name:"local" ~origin
      ~storage:Semantic_function_collection.Automatic ~declaration_index
      ~declarator_index
  in
  Alcotest.(check bool)
    "empty parameter name is rejected" true
    (named ~name:"" 0 |> Result.is_error);
  Alcotest.(check bool)
    "negative parameter index is rejected" true
    (named (-1) |> Result.is_error);
  Alcotest.(check bool)
    "invalid origins are rejected" true
    (Semantic_function_collection.make_named_parameter ~name:"bad"
       ~origin:(Semantic_symbol.Synthesized "") ~parameter_index:0
    |> Result.is_error);
  Alcotest.(check bool)
    "negative local declaration index is rejected" true
    (local ~declaration_index:(-1) () |> Result.is_error);
  Alcotest.(check bool)
    "negative local declarator index is rejected" true
    (local ~declarator_index:(-1) () |> Result.is_error);
  let table = Semantic_symbol_table.create () in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table
         ~parent:(Semantic_symbol_table.root table)
         ~kind:Semantic_symbol_table.Module ~name:"manual.HC" ())
  in
  let function_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"Manual"
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "manual function"))
  in
  let ordinary_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"ordinary"
         ~kind:Semantic_symbol.Global_variable
         ~origin:(Semantic_symbol.Synthesized "ordinary symbol"))
  in
  Alcotest.(check bool)
    "a non-function owner is rejected" true
    (Semantic_function_collection.make_function ~symbol:ordinary_symbol
       ~item_index:0 []
    |> Result.is_error);
  let reversed_parameters = [ checked (named 1); checked (named 0) ] in
  let malformed =
    checked
      (Semantic_function_collection.make_function ~symbol:function_symbol
         ~item_index:0 reversed_parameters)
  in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check bool)
    "out-of-order parameters are rejected" true
    (Semantic_function_collection.collect ~table ~parent:module_scope
       [ malformed ]
    |> Result.is_error);
  Alcotest.(check int)
    "batch validation precedes scope creation" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "batch validation precedes symbol insertion" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let other_table = Semantic_symbol_table.create () in
  let other_module =
    checked
      (Semantic_symbol_table.create_scope other_table
         ~parent:(Semantic_symbol_table.root other_table)
         ~kind:Semantic_symbol_table.Module ~name:"other.HC" ())
  in
  let foreign_symbol =
    checked
      (Semantic_symbol_table.add other_table ~scope:other_module ~name:"Foreign"
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "foreign function"))
  in
  let foreign =
    checked
      (Semantic_function_collection.make_function ~symbol:foreign_symbol
         ~item_index:0 [])
  in
  Alcotest.(check bool)
    "a same-numbered foreign symbol is rejected" true
    (Semantic_function_collection.collect ~table ~parent:module_scope
       [ foreign ]
    |> Result.is_error);
  Alcotest.(check int)
    "foreign symbol rejection preserves scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "foreign symbol rejection preserves symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let valid =
    checked
      (Semantic_function_collection.make_function ~symbol:function_symbol
         ~item_index:0
         [ checked (named 0); checked (local ()) ])
  in
  ignore
    (checked
       (Semantic_function_collection.collect ~table ~parent:module_scope
          [ valid ]));
  let sources = Source_manager.create () in
  let human = Semantic_symbol_table.human sources table in
  let json = Semantic_symbol_table.json sources table in
  Alcotest.(check string)
    "human symbol dump is deterministic" human
    (Semantic_symbol_table.human sources table);
  Alcotest.(check string)
    "JSON symbol dump is deterministic" json
    (Semantic_symbol_table.json sources table);
  Alcotest.(check bool)
    "human dump contains the function scope" true
    (String.split_on_char '\n' human
    |> List.exists
         (String.equal "scope 2 kind=function name=\"Manual\" parent=1"))

let tests =
  [
    Alcotest.test_case "signature slots and scopes" `Quick
      signature_slots_and_scopes;
    Alcotest.test_case "recursive local order and storage" `Quick
      recursive_local_order_and_storage;
    Alcotest.test_case "repeated names use one function scope" `Quick
      repeated_names_use_one_function_scope;
    Alcotest.test_case "generated and variadic provenance" `Quick
      generated_and_variadic_provenance;
    Alcotest.test_case "distinct declarations and absent body" `Quick
      distinct_declarations_and_absent_body;
    Alcotest.test_case "mismatched inputs do not mutate" `Quick
      mismatched_inputs_do_not_mutate;
    Alcotest.test_case "low-level validation and dumps" `Quick
      low_level_validation_and_dumps;
  ]
