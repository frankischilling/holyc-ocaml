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
  Holyc_lib.parse session ~source |> expect_ast

let collect session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let members = checked (Holyc_lib.collect_members session ~declarations ast) in
  (declarations, members)

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let scope_id scope =
  Semantic_symbol_table.scope_id scope |> Semantic_symbol.Scope_id.to_int

let option_exists predicate = function
  | Some value -> predicate value
  | None -> false

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else search (index + 1)
  in
  needle_length = 0 || search 0

let aggregate_names collection =
  Semantic_member_collection.aggregates collection
  |> List.map (fun aggregate ->
      Semantic_member_collection.aggregate_symbol aggregate
      |> Semantic_symbol.name)

let entry_names aggregate =
  Semantic_member_collection.aggregate_entries aggregate
  |> List.map (fun entry ->
      Semantic_member_collection.entry_symbol entry |> Semantic_symbol.name)

let entry_paths aggregate =
  Semantic_member_collection.aggregate_entries aggregate
  |> List.map Semantic_member_collection.entry_member_path

let entry_declarator_indexes aggregate =
  Semantic_member_collection.aggregate_entries aggregate
  |> List.map Semantic_member_collection.entry_declarator_index

let direct_grouped_and_anonymous_members () =
  let source =
    "class Node {\n\
     I64 first, second;\n\
     union { U8 byte; union { I16 nested_a, nested_b; }; };\n\
     $$=16;\n\
     ;\n\
     I64 tail doc \"tail field\";\n\
     };\n\
     union Payload { I64 number; U8 bytes[8]; };\n\
     class Empty {};"
  in
  let session = Session.create () in
  let ast = parse session ~path:"members.HC" source in
  let declarations, collection = collect session ast in
  Alcotest.(check (list string))
    "aggregate order"
    [ "Node"; "Payload"; "Empty" ]
    (aggregate_names collection);
  let aggregates = Semantic_member_collection.aggregates collection in
  let node = List.nth aggregates 0 in
  let payload = List.nth aggregates 1 in
  let empty = List.nth aggregates 2 in
  Alcotest.(check (list string))
    "flattened Node members"
    [ "first"; "second"; "byte"; "nested_a"; "nested_b"; "tail" ]
    (entry_names node);
  Alcotest.(check (list (list int)))
    "member paths"
    [ [ 0 ]; [ 0 ]; [ 1; 0 ]; [ 1; 1; 0 ]; [ 1; 1; 0 ]; [ 4 ] ]
    (entry_paths node);
  Alcotest.(check (list int))
    "grouped declarator indexes" [ 0; 1; 0; 0; 1; 0 ]
    (entry_declarator_indexes node);
  Alcotest.(check (list string))
    "union members" [ "number"; "bytes" ] (entry_names payload);
  Alcotest.(check int)
    "empty aggregate has no members" 0
    (Semantic_member_collection.aggregate_entries empty |> List.length);
  Alcotest.(check (list int))
    "aggregate item indexes" [ 0; 1; 2 ]
    (List.map Semantic_member_collection.aggregate_item_index aggregates);
  Alcotest.(check (list int))
    "aggregate scope IDs" [ 2; 3; 4 ]
    (List.map
       (fun aggregate ->
         Semantic_member_collection.aggregate_scope aggregate |> scope_id)
       aggregates);
  Alcotest.(check int)
    "only task, module, and aggregate scopes exist" 5
    (Semantic_symbol_table.all_scopes (Session.semantic_symbols session)
    |> List.length);
  let top_level_aggregate_ids =
    Semantic_declaration_collection.entries declarations
    |> List.filter (fun entry ->
        Semantic_declaration_collection.entry_kind entry
        = Semantic_declaration_collection.Aggregate_definition)
    |> List.map (fun entry ->
        Semantic_declaration_collection.entry_symbol entry |> symbol_id)
  in
  Alcotest.(check (list int))
    "member scopes retain top-level aggregate identities"
    top_level_aggregate_ids
    (List.map
       (fun aggregate ->
         Semantic_member_collection.aggregate_symbol aggregate |> symbol_id)
       aggregates)

let repeated_names_and_lookup () =
  let session = Session.create () in
  let ast =
    parse session ~path:"duplicates.HC"
      "class Repeated { I64 pad, pad; I64 reserved; I64 reserved; I64 value; \
       I64 value; };"
  in
  let _, collection = collect session ast in
  let aggregate = Semantic_member_collection.aggregates collection |> List.hd in
  let entries = Semantic_member_collection.aggregate_entries aggregate in
  Alcotest.(check (list string))
    "every repeated declaration survives collection"
    [ "pad"; "pad"; "reserved"; "reserved"; "value"; "value" ]
    (entry_names aggregate);
  let values =
    entries
    |> List.filter (fun entry ->
        Semantic_member_collection.entry_symbol entry
        |> Semantic_symbol.name |> String.equal "value")
    |> List.map (fun entry ->
        Semantic_member_collection.entry_symbol entry |> symbol_id)
  in
  let table = Session.semantic_symbols session in
  let scope = Semantic_member_collection.aggregate_scope aggregate in
  let lookup instance =
    checked
      (Semantic_symbol_table.lookup_local table ~scope ~name:"value"
         ~kinds:[ Semantic_symbol.Member ] ~instance ())
    |> Option.map symbol_id
  in
  Alcotest.(check (option int))
    "newest repeated member wins"
    (Some (List.nth values 1))
    (lookup 1);
  Alcotest.(check (option int))
    "older repeated member remains addressable"
    (Some (List.nth values 0))
    (lookup 2);
  Alcotest.(check bool)
    "internal types remain visible" true
    (checked
       (Semantic_symbol_table.lookup table ~scope ~name:"I64i"
          ~kinds:[ Semantic_symbol.Internal_type ]
          ())
    |> Option.is_some);
  let module_scope =
    Semantic_member_collection.aggregate_scope aggregate
    |> Semantic_symbol_table.parent |> Option.get
  in
  Alcotest.(check bool)
    "members do not leak into the module" true
    (checked
       (Semantic_symbol_table.lookup_local table ~scope:module_scope
          ~name:"value" ~kinds:[ Semantic_symbol.Member ] ())
    |> Option.is_none)

let inheritance_remains_unresolved () =
  let session = Session.create () in
  let ast =
    parse session ~path:"inheritance.HC"
      "class Base { I64 base_value; }; class Derived:Base { I64 own_value; };"
  in
  let _, collection = collect session ast in
  let aggregates = Semantic_member_collection.aggregates collection in
  let base = List.nth aggregates 0 in
  let derived = List.nth aggregates 1 in
  Alcotest.(check (list string))
    "base direct members" [ "base_value" ] (entry_names base);
  Alcotest.(check (list string))
    "derived direct members" [ "own_value" ] (entry_names derived);
  Alcotest.(check bool)
    "collection does not invent base-scope lookup" true
    (checked
       (Semantic_symbol_table.lookup
          (Session.semantic_symbols session)
          ~scope:(Semantic_member_collection.aggregate_scope derived)
          ~name:"base_value" ~kinds:[ Semantic_symbol.Member ] ())
    |> Option.is_none)

let generated_member_provenance () =
  let session = Session.create () in
  let root =
    Session.add_source session ~path:"generated-member.HC"
      ~contents:"#define FIELD generated_field\nclass Box { I64 FIELD; };"
  in
  let ast = Holyc_lib.parse session ~source:root |> expect_ast in
  let _, collection = collect session ast in
  let symbol =
    Semantic_member_collection.aggregates collection
    |> List.hd |> Semantic_member_collection.aggregate_entries |> List.hd
    |> Semantic_member_collection.entry_symbol
  in
  Alcotest.(check string)
    "expanded member name" "generated_field"
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
      Alcotest.fail "expanded member lost its source location"

let mismatched_inputs_do_not_mutate () =
  let session = Session.create () in
  let first = parse session ~path:"first.HC" "class First { I64 one; };" in
  let second = parse session ~path:"second.HC" "class Second { I64 two; };" in
  let declarations = checked (Holyc_lib.collect_declarations session first) in
  let table = Session.semantic_symbols session in
  let initial_scope_count =
    Semantic_symbol_table.all_scopes table |> List.length
  in
  let initial_symbol_count =
    Semantic_symbol_table.all_symbols table |> List.length
  in
  Alcotest.(check bool)
    "a collection from another AST is rejected" true
    (Holyc_lib.collect_members session ~declarations second |> Result.is_error);
  Alcotest.(check int)
    "AST mismatch preserves scopes" initial_scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "AST mismatch preserves symbols" initial_symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let other_session = Session.create () in
  let other_ast =
    parse other_session ~path:"other.HC" "class Other { I64 member; };"
  in
  let other_declarations =
    checked (Holyc_lib.collect_declarations other_session other_ast)
  in
  Alcotest.(check bool)
    "a module scope from another table is rejected" true
    (Holyc_lib.collect_members session ~declarations:other_declarations
       other_ast
    |> Result.is_error);
  Alcotest.(check int)
    "foreign scope preserves scopes" initial_scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "foreign scope preserves symbols" initial_symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let low_level_validation () =
  let make_member ?(name = "member") ?(member_path = [ 0 ])
      ?(declarator_index = 0) () =
    Semantic_member_collection.make_member ~name
      ~origin:(Semantic_symbol.Synthesized "member collection test")
      ~member_path ~declarator_index
  in
  Alcotest.(check bool)
    "empty name is rejected" true
    (make_member ~name:"" () |> Result.is_error);
  Alcotest.(check bool)
    "empty path is rejected" true
    (make_member ~member_path:[] () |> Result.is_error);
  Alcotest.(check bool)
    "negative path index is rejected" true
    (make_member ~member_path:[ 0; -1 ] () |> Result.is_error);
  Alcotest.(check bool)
    "negative declarator index is rejected" true
    (make_member ~declarator_index:(-1) () |> Result.is_error);
  let table = Semantic_symbol_table.create () in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table
         ~parent:(Semantic_symbol_table.root table)
         ~kind:Semantic_symbol_table.Module ~name:"manual.HC" ())
  in
  let aggregate_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"Manual"
         ~kind:Semantic_symbol.Aggregate_type
         ~origin:(Semantic_symbol.Synthesized "manual aggregate"))
  in
  let ordinary_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"ordinary"
         ~kind:Semantic_symbol.Global_variable
         ~origin:(Semantic_symbol.Synthesized "ordinary symbol"))
  in
  Alcotest.(check bool)
    "non-aggregate owner is rejected" true
    (Semantic_member_collection.make_aggregate ~symbol:ordinary_symbol
       ~item_index:0 []
    |> Result.is_error);
  let out_of_order =
    [ checked (make_member ~member_path:[ 1 ] ()); checked (make_member ()) ]
  in
  let aggregate =
    checked
      (Semantic_member_collection.make_aggregate ~symbol:aggregate_symbol
         ~item_index:0 out_of_order)
  in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check bool)
    "out-of-order members are rejected" true
    (Semantic_member_collection.collect ~table ~parent:module_scope
       [ aggregate ]
    |> Result.is_error);
  Alcotest.(check int)
    "validation happens before scope creation" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "validation happens before member insertion" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  Alcotest.(check bool)
    "task scope cannot contain aggregate scopes" true
    (Semantic_member_collection.collect ~table
       ~parent:(Semantic_symbol_table.root table)
       []
    |> Result.is_error)

let deterministic_symbol_dumps () =
  let session = Session.create () in
  let ast =
    parse session ~path:"deterministic.HC"
      "class One { I64 first; union { U8 byte; U16 word; }; }; class Two {};"
  in
  ignore (collect session ast);
  let table = Session.semantic_symbols session in
  let sources = Session.sources session in
  let human = Semantic_symbol_table.human sources table in
  let json = Semantic_symbol_table.json sources table in
  Alcotest.(check string)
    "human dump is deterministic" human
    (Semantic_symbol_table.human sources table);
  Alcotest.(check string)
    "JSON dump is deterministic" json
    (Semantic_symbol_table.json sources table);
  Alcotest.(check bool)
    "human dump contains aggregate scopes" true
    (String.split_on_char '\n' human
    |> List.exists (String.equal "scope 2 kind=aggregate name=\"One\" parent=1")
    );
  Alcotest.(check bool)
    "human dump contains member symbols" true
    (String.split_on_char '\n' human
    |> List.exists (fun line ->
        contains ~needle:"name=\"first\" kind=member origin=deterministic.HC:"
          line))

let tests =
  [
    Alcotest.test_case "direct, grouped, and anonymous members" `Quick
      direct_grouped_and_anonymous_members;
    Alcotest.test_case "repeated names and lookup" `Quick
      repeated_names_and_lookup;
    Alcotest.test_case "inheritance remains unresolved" `Quick
      inheritance_remains_unresolved;
    Alcotest.test_case "generated member provenance" `Quick
      generated_member_provenance;
    Alcotest.test_case "mismatched inputs do not mutate" `Quick
      mismatched_inputs_do_not_mutate;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
    Alcotest.test_case "deterministic symbol dumps" `Quick
      deterministic_symbol_dumps;
  ]
