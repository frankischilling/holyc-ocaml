open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

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

let tests =
  [
    Alcotest.test_case "stable identities and source order" `Quick
      stable_identities_and_source_order;
    Alcotest.test_case "rejected batches do not mutate" `Quick
      rejected_batches_do_not_mutate;
  ]
