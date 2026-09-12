open Holyc_lib
module O = Semantic_outer_environment
module F = Semantic_function_resolution
module K = Semantic_function_record_classification
module P = Test_pending_function_resolution

let checked result = result |> Result.map_error O.error_to_string |> P.checked

let metadata records declaration =
  O.make_function_metadata ~records ~declaration |> checked

let fixture () =
  let table, namespace, _, function_, pending_resolution =
    P.fixture "I64 F(I64 n=42){return n;}"
  in
  let pending = P.single pending_resolution in
  let pending_records = P.classify pending_resolution in
  let completed_resolution =
    F.complete_pending ~table ~namespace ~pending ~function_ |> P.checked
  in
  let completed = P.single completed_resolution in
  let completed_records =
    P.classify ~previous:(K.declarations pending_records) completed_resolution
  in
  let old = metadata pending_records pending in
  let current = metadata completed_records completed in
  let entry =
    O.make_function_entry ~entry_index:0 ~function_metadata:current |> checked
  in
  let primary =
    O.make_table ~table_kind:(O.Jit_task 0) ~table_index:0 [ entry ] |> checked
  in
  let assembler =
    O.make_table ~table_kind:O.Assembler ~table_index:1 [] |> checked
  in
  let environment =
    O.create ~table ~compilation_mode:O.Jit [ primary; assembler ] |> checked
  in
  (table, namespace, function_, old, current, entry, primary, environment)

let historical_binding () =
  let _, _, _, old, current, entry, primary, environment = fixture () in
  let extended, versions =
    O.with_function_versions environment ~table:primary [ old ] |> checked
  in
  let historical = List.hd versions in
  let binding = O.binding_for_entry extended historical |> Option.get in
  Alcotest.(check bool)
    "historical binding belongs to exact primary table" true
    (O.binding_table binding == primary);
  Alcotest.(check bool)
    "extended environment owns captured version" true
    (O.owns_binding extended binding);
  Alcotest.(check bool)
    "original environment does not own captured version" false
    (O.owns_binding environment binding);
  Alcotest.(check bool)
    "original environment cannot bind historical entry" true
    (Option.is_none (O.binding_for_entry environment historical));
  Alcotest.(check bool)
    "name lookup retains current entry" true
    (O.binding_entry (Option.get (O.find extended "F")) == entry);
  Alcotest.(check bool)
    "record lookup retains current entry" true
    (O.binding_entry
       (Option.get (O.find_record extended ~name:"F" ~record_kind:O.Function))
    == entry);
  Alcotest.(check int)
    "history is absent from primary entries" 1
    (List.length (O.table_entries primary));
  Alcotest.(check int)
    "historical index follows primary entries" 1 (O.entry_index historical);
  let copy =
    O.make_function_entry ~entry_index:1 ~function_metadata:old |> checked
  in
  Alcotest.(check bool)
    "copied entry grants no binding" true
    (Option.is_none (O.binding_for_entry extended copy));
  let extended_again, newest =
    O.with_function_versions extended ~table:primary [ current ] |> checked
  in
  Alcotest.(check int)
    "later history has unique index" 2
    (O.entry_index (List.hd newest));
  Alcotest.(check bool)
    "extensions preserve prior captured bindings" true
    (O.owns_binding extended_again binding);
  P.rejected "history cannot repeat a declaration"
    (O.with_function_versions extended_again ~table:primary [ old ]);
  P.rejected "one batch cannot repeat a declaration"
    (O.with_function_versions environment ~table:primary [ old; old ]);
  let copied_table =
    O.make_table ~table_kind:(O.Jit_task 0) ~table_index:0 [ entry ] |> checked
  in
  P.rejected "equal reconstructed table has no authority"
    (O.with_function_versions environment ~table:copied_table [ old ])

let reject_unrelated () =
  let table, namespace, function_, _, _, _, primary, environment = fixture () in
  let parent = Semantic_declaration_collection.namespace_scope namespace in
  let independent mode =
    let fact = F.make_declaration ~function_ ~kind:F.Extern |> P.checked in
    let resolution =
      F.resolve ~table ~parent ~compilation_mode:mode [ fact ] |> P.checked
    in
    metadata (P.classify resolution) (P.single resolution)
  in
  P.rejected "same physical identity without joined ancestry is unauthorized"
    (O.with_function_versions environment ~table:primary [ independent F.Jit ]);
  P.rejected "another compilation mode is unauthorized"
    (O.with_function_versions environment ~table:primary [ independent F.Aot ]);
  let _, _, _, foreign, _, _, _, _ = fixture () in
  P.rejected "same spelling from another semantic table is unauthorized"
    (O.with_function_versions environment ~table:primary [ foreign ]);
  Alcotest.(check int)
    "failed extension does not mutate primary table" 1
    (List.length (O.table_entries primary))

let reject_reclassified_versions () =
  let prepared =
    Test_function_record_classification.prepare ~path:"versions.HC"
      "extern I64 F(I64 n);I64 F(I64 n){return n;}"
  in
  let resolution =
    Test_function_record_classification.resolve prepared Preprocessor.Jit
  in
  let state mask =
    K.make_declaration_state ~staging_mask:mask
      ~compiler_option_mask:Compiler_option.initial_mask ()
  in
  let records = K.classify resolution [ state 0L; state 0L ] |> P.checked in
  let old_declaration = List.hd (F.declarations resolution) in
  let current_declaration = List.nth (F.declarations resolution) 1 in
  let old = metadata records old_declaration in
  let current = metadata records current_declaration in
  let entry =
    O.make_function_entry ~entry_index:0 ~function_metadata:current |> checked
  in
  let primary =
    O.make_table ~table_kind:(O.Jit_task 0) ~table_index:0 [ entry ] |> checked
  in
  let assembler =
    O.make_table ~table_kind:O.Assembler ~table_index:1 [] |> checked
  in
  let environment =
    O.create
      ~table:(Session.semantic_symbols prepared.session)
      ~compilation_mode:O.Jit [ primary; assembler ]
    |> checked
  in
  ignore (O.with_function_versions environment ~table:primary [ old ] |> checked);
  let public =
    Function_flag.apply_modifier ~mask:0L Function_flag.Modifier.Public
  in
  let fake_records =
    K.classify resolution [ state public; state 0L ] |> P.checked
  in
  let fake_old = metadata fake_records old_declaration in
  Alcotest.(check bool)
    "reclassification changes historical Public flag" true
    (K.is_public
       (K.classified_declaration_record
          (O.function_classified_declaration fake_old)));
  P.rejected "same declaration cannot replace its original classified record"
    (O.with_function_versions environment ~table:primary [ fake_old ]);
  let reconstructed =
    K.classify resolution [ state 0L; state 0L ] |> P.checked
  in
  P.rejected "equal reconstructed classification is not original authority"
    (O.with_function_versions environment ~table:primary
       [ metadata reconstructed old_declaration ]);
  P.rejected "current declaration also requires its original classification"
    (O.with_function_versions environment ~table:primary
       [ metadata reconstructed current_declaration ])

let tests =
  [
    Alcotest.test_case
      "historical function entries retain exact binding authority" `Quick
      historical_binding;
    Alcotest.test_case
      "historical function entries require original joined ancestry" `Quick
      reject_unrelated;
    Alcotest.test_case
      "historical function entries reject rebuilt classifications" `Quick
      reject_reclassified_versions;
  ]
