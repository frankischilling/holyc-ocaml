open Holyc_lib

let checked result =
  match result with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let contains_text text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

let symbol_id symbol =
  Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let scope_id scope =
  Semantic_symbol_table.scope_id scope |> Semantic_symbol.Scope_id.to_int

let stable_ids () =
  let table = Semantic_symbol_table.create ~root_name:"task" () in
  let root = Semantic_symbol_table.root table in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:root
         ~kind:Semantic_symbol_table.Module ~name:"Compiler" ())
  in
  let function_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:module_scope
         ~kind:Semantic_symbol_table.Function ~name:"Lex" ())
  in
  Alcotest.(check (list int))
    "scope creation order" [ 0; 1; 2 ]
    (Semantic_symbol_table.all_scopes table |> List.map scope_id);
  let first =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"Lex"
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "test declaration"))
  in
  let second =
    checked
      (Semantic_symbol_table.add table ~scope:function_scope ~name:"cc"
         ~kind:Semantic_symbol.Parameter
         ~origin:(Semantic_symbol.Synthesized "test parameter"))
  in
  Alcotest.(check (list int))
    "symbol creation order" [ 0; 1 ]
    (Semantic_symbol_table.all_symbols table |> List.map symbol_id);
  Alcotest.(check int) "first ID" 0 (symbol_id first);
  Alcotest.(check int) "second ID" 1 (symbol_id second)

let newest_and_nth_lookup () =
  let table = Semantic_symbol_table.create () in
  let root = Semantic_symbol_table.root table in
  let old_function =
    checked
      (Semantic_symbol_table.add table ~scope:root ~name:"DrawIt"
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "old function"))
  in
  let global =
    checked
      (Semantic_symbol_table.add table ~scope:root ~name:"DrawIt"
         ~kind:Semantic_symbol.Global_variable
         ~origin:(Semantic_symbol.Synthesized "global"))
  in
  let new_function =
    checked
      (Semantic_symbol_table.add table ~scope:root ~name:"DrawIt"
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "new function"))
  in
  let lookup kinds instance =
    checked
      (Semantic_symbol_table.lookup_local table ~scope:root ~name:"DrawIt"
         ~kinds ~instance ())
    |> Option.get
  in
  Alcotest.(check int)
    "newest function" (symbol_id new_function)
    (lookup [ Semantic_symbol.Function ] 1 |> symbol_id);
  Alcotest.(check int)
    "older function" (symbol_id old_function)
    (lookup [ Semantic_symbol.Function ] 2 |> symbol_id);
  Alcotest.(check int)
    "kind-filtered global" (symbol_id global)
    (lookup [ Semantic_symbol.Global_variable ] 1 |> symbol_id)

let chained_lookup () =
  let table = Semantic_symbol_table.create ~root_name:"parent-task" () in
  let parent_task = Semantic_symbol_table.root table in
  let parent_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:parent_task ~name:"Value"
         ~kind:Semantic_symbol.Global_variable
         ~origin:(Semantic_symbol.Synthesized "parent task"))
  in
  let child_task =
    checked
      (Semantic_symbol_table.create_scope table ~parent:parent_task
         ~kind:Semantic_symbol_table.Task ~name:"child-task" ())
  in
  let child_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:child_task ~name:"Value"
         ~kind:Semantic_symbol.Global_variable
         ~origin:(Semantic_symbol.Synthesized "child task"))
  in
  let function_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:child_task
         ~kind:Semantic_symbol_table.Function ~name:"Entry" ())
  in
  let lookup instance =
    checked
      (Semantic_symbol_table.lookup table ~scope:function_scope ~name:"Value"
         ~kinds:[ Semantic_symbol.Global_variable ] ~instance ())
    |> Option.get
  in
  Alcotest.(check int)
    "child shadows parent" (symbol_id child_symbol) (lookup 1 |> symbol_id);
  Alcotest.(check int)
    "nth match reaches parent" (symbol_id parent_symbol)
    (lookup 2 |> symbol_id);
  Alcotest.(check bool)
    "local lookup does not walk parents" true
    (checked
       (Semantic_symbol_table.lookup_local table ~scope:function_scope
          ~name:"Value" ~kinds:[ Semantic_symbol.Global_variable ] ())
    |> Option.is_none)

let rejected_lookup_is_nonmutating () =
  let table = Semantic_symbol_table.create () in
  let foreign = Semantic_symbol_table.create () in
  let foreign_root = Semantic_symbol_table.root foreign in
  Alcotest.(check bool)
    "foreign add fails" true
    (Semantic_symbol_table.add table ~scope:foreign_root ~name:"Wrong"
       ~kind:Semantic_symbol.Function
       ~origin:(Semantic_symbol.Synthesized "foreign")
    |> Result.is_error);
  Alcotest.(check bool)
    "zero instance fails" true
    (Semantic_symbol_table.lookup table ~scope:(Semantic_symbol_table.root table)
       ~name:"Name" ~kinds:[ Semantic_symbol.Function ] ~instance:0 ()
    |> Result.is_error);
  Alcotest.(check bool)
    "empty kind set fails" true
    (Semantic_symbol_table.lookup table ~scope:(Semantic_symbol_table.root table)
       ~name:"Name" ~kinds:[] ()
    |> Result.is_error);
  let first =
    checked
      (Semantic_symbol_table.add table ~scope:(Semantic_symbol_table.root table)
         ~name:"Right" ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "valid"))
  in
  Alcotest.(check int) "failed add did not consume an ID" 0 (symbol_id first)

let deterministic_dumps () =
  let session = Session.create () in
  let sources = Session.sources session in
  let source =
    Session.add_source session ~path:"generated.HC" ~contents:"GeneratedName"
  in
  let span =
    Span.make ~source:(Source_file.id source) ~length:13 ~start:0 ~stop:13
    |> Result.get_ok
  in
  let table = Semantic_symbol_table.create ~root_name:"task" () in
  let root = Semantic_symbol_table.root table in
  ignore
    (checked
       (Semantic_symbol_table.add table ~scope:root ~name:"GeneratedName"
          ~kind:Semantic_symbol.Generated
          ~origin:
            (Semantic_symbol.Source_location
               {
                 span;
                 source_segments = [ span ];
                 generated_from = Some span;
                 defined_at = Some span;
               })));
  let human = Semantic_symbol_table.human sources table in
  Alcotest.(check string)
    "repeatable human dump" human
    (Semantic_symbol_table.human sources table);
  Alcotest.(check bool)
    "versioned human dump" true
    (String.starts_with ~prefix:"holyc-semantic-symbol-table-v1\n" human);
  Alcotest.(check bool)
    "source location" true
    (contains_text human
       "symbol 0 scope=0 name=\"GeneratedName\" kind=generated \
        origin=generated.HC:1:1..1:14\n");
  Alcotest.(check bool)
    "generated provenance" true
    (contains_text human "  generated-from=generated.HC:1:1..1:14\n");
  let json = Semantic_symbol_table.json sources table in
  Alcotest.(check string)
    "repeatable JSON dump" json
    (Semantic_symbol_table.json sources table);
  let open Yojson.Safe.Util in
  let parsed = Yojson.Safe.from_string json in
  Alcotest.(check string)
    "JSON schema" "holyc-semantic-symbol-table-v1"
    (parsed |> member "schema" |> to_string);
  let origin =
    parsed |> member "symbols" |> index 0 |> member "origin"
  in
  Alcotest.(check string)
    "source origin" "source-location" (origin |> member "kind" |> to_string);
  Alcotest.(check string)
    "source path" "generated.HC"
    (origin |> member "span" |> member "path" |> to_string)

let session_seed () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let root = Semantic_symbol_table.root table in
  let internal_types =
    Semantic_symbol_table.all_symbols table
    |> List.filter (fun symbol ->
        Semantic_symbol.equal_kind (Semantic_symbol.kind symbol)
          Semantic_symbol.Internal_type)
  in
  Alcotest.(check int)
    "pinned internal types" 17 (List.length internal_types);
  let i64 =
    checked
      (Semantic_symbol_table.lookup table ~scope:root ~name:"I64i"
         ~kinds:[ Semantic_symbol.Internal_type ] ())
  in
  Alcotest.(check bool) "I64i is seeded" true (Option.is_some i64)

let tests =
  [
    Alcotest.test_case "stable IDs" `Quick stable_ids;
    Alcotest.test_case "newest and nth lookup" `Quick newest_and_nth_lookup;
    Alcotest.test_case "chained lookup" `Quick chained_lookup;
    Alcotest.test_case "rejected lookup is nonmutating" `Quick
      rejected_lookup_is_nonmutating;
    Alcotest.test_case "deterministic dumps" `Quick deterministic_dumps;
    Alcotest.test_case "session seed" `Quick session_seed;
  ]
