open Holyc_lib
module V = Symbol_visibility
module E = V.Environment

let checked = Test_symbol_visibility.checked

let shape name : V.function_call_shape =
  {
    parameters = [ { parameter_name = Some name; has_default = false } ];
    variadic = false;
  }

let immutable_alias () =
  let environment = E.create () in
  let original_shape = shape "old" and refreshed_shape = shape "new" in
  let origin = V.Pinned_source { path = "original.HC"; line = 4 } in
  let original =
    E.add ~origin ~function_call_shape:original_shape environment ~name:"F"
      ~kind:V.Function ()
  in
  let before = E.all environment in
  E.validate_function_alias environment ~original_entry:original |> checked;
  Alcotest.(check bool)
    "successful preflight is readonly" true
    (List.for_all2 ( == ) before (E.all environment));
  let alias =
    E.add_function_alias ~function_call_shape:refreshed_shape environment
      ~original_entry:original ()
    |> checked
  in
  Alcotest.(check bool)
    "alias retains exact immutable original" true
    (Option.get (V.function_alias_original alias) == original);
  Alcotest.(check string) "alias name derives from original" "F" (V.name alias);
  Alcotest.(check bool)
    "alias origin derives from exact original" true
    (V.origin alias == V.origin original);
  Alcotest.(check bool)
    "alias is a fresh Function entry" true
    (alias != original
    && V.id alias <> V.id original
    && V.kind alias = V.Function);
  Alcotest.(check int)
    "preflight did not consume an identity"
    (V.id original + 1)
    (V.id alias);
  Alcotest.(check bool)
    "alias refresh does not modify original call shape" true
    (Option.get (V.function_call_shape original) == original_shape
    && Option.get (V.function_call_shape alias) == refreshed_shape);
  let next =
    E.add_function_alias environment ~original_entry:alias () |> checked
  in
  Alcotest.(check bool)
    "nested alias retains immediate ancestry" true
    (Option.get (V.function_alias_original next) == alias);
  Alcotest.(check bool)
    "omitted shape inherits original syntax metadata" true
    (Option.get (V.function_call_shape next) == refreshed_shape)

let ownership_and_kind () =
  let root = E.create () in
  let owner = E.task_view root and foreign = E.task_view root in
  let original = E.add owner ~name:"F" ~kind:V.Function () in
  List.iter
    (fun environment ->
      let before = E.all environment in
      Alcotest.(check bool)
        "foreign writer fails readonly alias preflight" true
        (Result.is_error
           (E.validate_function_alias environment ~original_entry:original));
      Alcotest.(check bool)
        "foreign writer cannot alias an original entry" true
        (Result.is_error
           (E.add_function_alias environment ~original_entry:original ()));
      Alcotest.(check bool)
        "rejection leaves all entries unchanged" true
        (List.for_all2 ( == ) before (E.all environment)))
    [ root; foreign; E.create () ];
  let baseline = E.add root ~name:"Baseline" ~kind:V.Function () in
  Alcotest.(check bool)
    "visible baseline is not owned by task writer" true
    (Result.is_error (E.add_function_alias owner ~original_entry:baseline ()));
  let variable = E.add owner ~name:"Variable" ~kind:V.Global_variable () in
  Alcotest.(check bool)
    "non-function entry cannot become function alias" true
    (Result.is_error (E.add_function_alias owner ~original_entry:variable ()))

let copied_environment () =
  let environment = E.create () in
  let original = E.add environment ~name:"F" ~kind:V.Function () in
  let alias =
    E.add_function_alias environment ~original_entry:original () |> checked
  in
  let copy = E.copy environment in
  let newer =
    E.add_function_alias ~function_call_shape:(shape "new") environment
      ~original_entry:alias ()
    |> checked
  in
  Alcotest.(check bool)
    "copy retains earlier lookup object" true
    (Option.get (E.find_function copy "F") == alias);
  Alcotest.(check bool)
    "copy retains exact immutable alias link" true
    (Option.get
       (V.function_alias_original (Option.get (E.find_function copy "F")))
    == original);
  let copied_alias =
    E.add_function_alias copy ~original_entry:alias () |> checked
  in
  Alcotest.(check bool)
    "copy may alias its physically present owned entry" true
    (Option.get (V.function_alias_original copied_alias) == alias);
  Alcotest.(check bool)
    "copy mutation does not alter original environment" true
    (Option.get (E.find_function environment "F") == newer);
  Alcotest.(check bool)
    "copy cannot alias later entry absent from its store" true
    (Result.is_error (E.add_function_alias copy ~original_entry:newer ()))

let generic_clone_and_completion () =
  let environment = E.create () in
  let original = E.add environment ~name:"F" ~kind:V.Function () in
  let copied_before = E.copy environment in
  let completed =
    E.complete_function_header environment ~entry:original
      ~function_call_shape:(shape "n")
    |> checked
  in
  Alcotest.(check bool)
    "completion is not an explicit alias" true
    (Option.is_none (V.function_alias_original completed));
  Alcotest.(check bool)
    "replaced provisional object cannot authorize alias" true
    (Result.is_error
       (E.add_function_alias environment ~original_entry:original ()));
  let alias =
    E.add_function_alias environment ~original_entry:completed () |> checked
  in
  Alcotest.(check bool)
    "completed original authorizes alias" true
    (Option.get (V.function_alias_original alias) == completed);
  ignore
    (E.add_function_alias copied_before ~original_entry:original () |> checked);
  let clone =
    E.add ~origin:(V.origin completed)
      ?function_call_shape:(V.function_call_shape completed)
      environment ~name:(V.name completed) ~kind:(V.kind completed) ()
  in
  Alcotest.(check bool)
    "equal name origin and shape cannot forge alias ancestry" true
    (Option.is_none (V.function_alias_original clone));
  let provisional_alias =
    E.add_function_alias copied_before ~original_entry:original () |> checked
  in
  let completed_alias =
    E.complete_function_header copied_before ~entry:provisional_alias
      ~function_call_shape:(shape "alias")
    |> checked
  in
  Alcotest.(check bool)
    "header completion preserves immutable alias ancestry" true
    (Option.get (V.function_alias_original completed_alias) == original)

let tests =
  [
    Alcotest.test_case "function aliases preserve exact immutable ancestry"
      `Quick immutable_alias;
    Alcotest.test_case "alias creation checks exact ownership presence and kind"
      `Quick ownership_and_kind;
    Alcotest.test_case "environment copies preserve links and isolate mutations"
      `Quick copied_environment;
    Alcotest.test_case "generic entries never fabricate alias ancestry" `Quick
      generic_clone_and_completion;
  ]
