open Holyc_lib

let contains_text text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

let checked result =
  match result with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let source_kind_bits () =
  let open Symbol_visibility in
  let kinds =
    [
      Export_system_symbol;
      Import_system_symbol;
      Definition;
      Global_variable;
      Class;
      Internal_type;
      Function;
      Word;
      Dictionary_word;
      Keyword;
      Assembly_keyword;
      Opcode;
      Register;
      File;
      Module;
      Help_file;
      Frame_pointer;
    ]
  in
  Alcotest.(check (list int))
    "HTT bit assignments"
    [
      0x00001;
      0x00002;
      0x00004;
      0x00008;
      0x00010;
      0x00020;
      0x00040;
      0x00080;
      0x00100;
      0x00200;
      0x00400;
      0x00800;
      0x01000;
      0x02000;
      0x04000;
      0x08000;
      0x10000;
    ]
    (List.map kind_bit kinds)

let session_builtins () =
  let session = Session.create () in
  let symbols = Session.symbols session in
  let entries = Symbol_visibility.Environment.all symbols in
  Alcotest.(check int) "checked built-in entries" 576 (List.length entries);
  List.iter
    (fun (name, expected_kind) ->
      match Symbol_visibility.Environment.find_preprocessor symbols name with
      | Symbol_visibility.Present entry ->
          Alcotest.(check bool)
            (name ^ " kind") true
            (Symbol_visibility.kind entry = expected_kind)
      | Symbol_visibility.Absent -> Alcotest.failf "%s should be present" name
      | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "%s should not be locally shadowed" name)
    [
      ("ifjit", Symbol_visibility.Keyword);
      ("ALIGN", Symbol_visibility.Assembly_keyword);
      ("I64i", Symbol_visibility.Internal_type);
      ("I64", Symbol_visibility.Class);
      ("RAX", Symbol_visibility.Register);
      ("FS", Symbol_visibility.Register);
      ("ST3", Symbol_visibility.Register);
      ("MM7", Symbol_visibility.Register);
      ("XMM7", Symbol_visibility.Register);
      ("MOV", Symbol_visibility.Opcode);
      ("JZ", Symbol_visibility.Opcode);
      ("SAL", Symbol_visibility.Opcode);
    ];
  let first = List.hd entries in
  let last = List.hd (List.rev entries) in
  Alcotest.(check int) "first stable ID" 0 (Symbol_visibility.id first);
  Alcotest.(check int) "last stable ID" 575 (Symbol_visibility.id last);
  Alcotest.(check string)
    "last seeded spelling" "I64"
    (Symbol_visibility.name last)

let import_filtering () =
  let symbols = Symbol_visibility.Environment.create () in
  ignore
    (Symbol_visibility.Environment.add symbols ~name:"OnlyImport"
       ~kind:Symbol_visibility.Import_system_symbol ());
  (match
     Symbol_visibility.Environment.find_preprocessor symbols "OnlyImport"
   with
  | Symbol_visibility.Absent -> ()
  | _ -> Alcotest.fail "an import must not satisfy the default hash mask");
  let function_entry =
    Symbol_visibility.Environment.add symbols ~name:"Both"
      ~kind:Symbol_visibility.Function ()
  in
  ignore
    (Symbol_visibility.Environment.add symbols ~name:"Both"
       ~kind:Symbol_visibility.Import_system_symbol ());
  match Symbol_visibility.Environment.find_preprocessor symbols "Both" with
  | Symbol_visibility.Present entry ->
      Alcotest.(check int)
        "masked lookup reaches older function"
        (Symbol_visibility.id function_entry)
        (Symbol_visibility.id entry)
  | _ -> Alcotest.fail "the non-import entry should remain visible"

let local_shadowing () =
  let symbols = Symbol_visibility.Environment.create () in
  ignore
    (Symbol_visibility.Environment.add symbols ~name:"Value"
       ~kind:Symbol_visibility.Global_variable ());
  let outer = Symbol_visibility.Environment.begin_local_context symbols in
  checked (Symbol_visibility.Environment.add_local symbols outer ~name:"Value");
  (match Symbol_visibility.Environment.find_preprocessor symbols "Value" with
  | Symbol_visibility.Shadowed_by_local -> ()
  | _ -> Alcotest.fail "the local variable should suppress hash lookup");
  let local_dump =
    Symbol_visibility.Environment.dump (Source_manager.create ()) symbols
  in
  Alcotest.(check bool)
    "local context dump" true
    (contains_text local_dump "local-context 0\n  local name=\"Value\"\n");
  let inner = Symbol_visibility.Environment.begin_local_context symbols in
  Alcotest.(check bool)
    "contexts end in stack order" true
    (Symbol_visibility.Environment.end_local_context symbols outer
    |> Result.is_error);
  checked (Symbol_visibility.Environment.end_local_context symbols inner);
  checked (Symbol_visibility.Environment.end_local_context symbols outer);
  match Symbol_visibility.Environment.find_preprocessor symbols "Value" with
  | Symbol_visibility.Present _ -> ()
  | _ -> Alcotest.fail "the global should reappear after the local context"

let function_call_shapes () =
  let symbols = Symbol_visibility.Environment.create () in
  let shape : Symbol_visibility.function_call_shape =
    {
      parameters =
        [
          { parameter_name = Some "first"; has_default = true };
          { parameter_name = None; has_default = false };
        ];
      variadic = true;
    }
  in
  let entry =
    Symbol_visibility.Environment.add symbols ~name:"Callable"
      ~kind:Symbol_visibility.Function ~function_call_shape:shape ()
  in
  (match Symbol_visibility.function_call_shape entry with
  | Some retained ->
      Alcotest.(check int)
        "fixed parameter count" 2
        (List.length retained.parameters);
      Alcotest.(check bool) "variadic marker" true retained.variadic;
      Alcotest.(check (option string))
        "parameter name" (Some "first")
        (List.hd retained.parameters).parameter_name;
      Alcotest.(check bool)
        "default availability" true (List.hd retained.parameters).has_default
  | None -> Alcotest.fail "the function call shape was not retained");
  Alcotest.check_raises "nonfunction call shape is rejected"
    (Invalid_argument "only function symbols may carry a function call shape")
    (fun () ->
      ignore
        (Symbol_visibility.Environment.add symbols ~name:"NotCallable"
           ~kind:Symbol_visibility.Global_variable ~function_call_shape:shape ()))

let deterministic_dump () =
  let session = Session.create () in
  let source = Session.add_source session ~path:"visibility.HC" ~contents:"X" in
  let span =
    Span.make ~source:(Source_file.id source) ~length:1 ~start:0 ~stop:1
    |> Result.get_ok
  in
  let symbols = Session.symbols session in
  ignore
    (Symbol_visibility.Environment.add symbols ~name:"UserFunction"
       ~kind:Symbol_visibility.Function
       ~origin:(Symbol_visibility.Source_span span) ());
  let first =
    Symbol_visibility.Environment.dump (Session.sources session) symbols
  in
  let second =
    Symbol_visibility.Environment.dump (Session.sources session) symbols
  in
  Alcotest.(check string) "repeatable dump" first second;
  Alcotest.(check bool)
    "versioned header" true
    (contains_text first "holyc-symbol-visibility-v2\n");
  Alcotest.(check bool)
    "reference commit" true
    (contains_text first Version.reference_commit);
  Alcotest.(check bool)
    "source origin" true
    (contains_text first
       "symbol 576 name=\"UserFunction\" kind=function \
        origin=visibility.HC:1:1..1:2")

let deterministic_json () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"visibility-json.HC" ~contents:"Callable"
  in
  let span =
    Span.make ~source:(Source_file.id source) ~length:8 ~start:0 ~stop:8
    |> Result.get_ok
  in
  let symbols = Session.symbols session in
  let shape : Symbol_visibility.function_call_shape =
    {
      parameters =
        [
          { parameter_name = Some "first"; has_default = true };
          { parameter_name = None; has_default = false };
        ];
      variadic = true;
    }
  in
  ignore
    (Symbol_visibility.Environment.add symbols ~name:"Callable"
       ~kind:Symbol_visibility.Function ~function_call_shape:shape
       ~origin:
         (Symbol_visibility.Source_location
            {
              span;
              source_segments = [ span ];
              generated_from = Some span;
              defined_at = Some span;
            })
       ());
  let local = Symbol_visibility.Environment.begin_local_context symbols in
  checked (Symbol_visibility.Environment.add_local symbols local ~name:"zeta");
  checked (Symbol_visibility.Environment.add_local symbols local ~name:"alpha");
  let first =
    Symbol_visibility.Environment.json (Session.sources session) symbols
  in
  let second =
    Symbol_visibility.Environment.json (Session.sources session) symbols
  in
  Alcotest.(check string) "repeatable JSON" first second;
  let json = Yojson.Safe.from_string first in
  let source_only =
    Symbol_visibility.Environment.json ~source_only:true
      (Session.sources session) symbols
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "JSON schema" "holyc-symbol-visibility-v2"
    (json |> member "schema" |> to_string);
  Alcotest.(check string)
    "JSON reference commit" Version.reference_commit
    (json |> member "reference_commit" |> to_string);
  let callable = json |> member "symbols" |> to_list |> List.rev |> List.hd in
  Alcotest.(check string)
    "source location origin" "source-location"
    (callable |> member "origin" |> member "kind" |> to_string);
  Alcotest.(check int)
    "two fixed parameters" 2
    (callable |> member "call_shape" |> member "parameters" |> to_list
   |> List.length);
  Alcotest.(check bool)
    "variadic shape" true
    (callable |> member "call_shape" |> member "variadic" |> to_bool);
  Alcotest.(check (list string))
    "local names are sorted" [ "alpha"; "zeta" ]
    (json |> member "local_contexts" |> index 0 |> member "names" |> to_list
   |> List.map to_string);
  let source_symbols = source_only |> member "symbols" |> to_list in
  Alcotest.(check int) "source-only entry count" 1 (List.length source_symbols);
  Alcotest.(check string)
    "source-only entry" "Callable"
    (source_symbols |> List.hd |> member "name" |> to_string)

let provisional_function_completion () =
  let module E = Symbol_visibility.Environment in
  let environment = E.create () in
  let entry = E.add environment ~name:"F" ~kind:Symbol_visibility.Function () in
  let copied = E.copy environment in
  let foreign = E.create () in
  let shape = Symbol_visibility.{ parameters = []; variadic = false } in
  let reject target candidate =
    match
      E.complete_function_header target ~entry:candidate
        ~function_call_shape:shape
    with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "invalid function completion accepted"
  in
  reject foreign entry;
  let newer =
    E.add environment ~name:"F" ~kind:Symbol_visibility.Global_variable ()
  in
  let completed =
    E.complete_function_header environment ~entry ~function_call_shape:shape
    |> checked
  in
  reject environment entry;
  reject environment completed;
  reject environment newer;
  Alcotest.(check bool)
    "copied environment retains provisional snapshot" true
    (E.find_function copied "F" = Some entry
    && Option.is_none (Symbol_visibility.function_call_shape entry));
  Alcotest.(check bool)
    "kind-filtered lookup selects completed function" true
    (Option.get (E.find_function environment "F") == completed);
  Alcotest.(check bool)
    "newer global remains ordinary lookup winner" true
    (match E.find_preprocessor environment "F" with
    | Symbol_visibility.Present selected -> selected == newer
    | _ -> false);
  Alcotest.(check int)
    "completion does not duplicate publication" 2
    (List.length (E.all environment));
  Alcotest.(check int)
    "identity retained"
    (Symbol_visibility.id entry)
    (Symbol_visibility.id completed)

let task_views_and_detached_snapshots () =
  let module E = Symbol_visibility.Environment in
  let root = E.create () in
  let baseline = E.add root ~name:"Baseline" ~kind:Symbol_visibility.Class () in
  let task = E.task_view root and other = E.task_view root in
  let original = E.add task ~name:"F" ~kind:Symbol_visibility.Function () in
  let hidden = E.add other ~name:"F" ~kind:Symbol_visibility.Function () in
  let copied = E.copy task in
  let shape = Symbol_visibility.{ parameters = []; variadic = false } in
  List.iter
    (fun target ->
      Alcotest.(check bool)
        "another writer cannot complete a visible provisional entry" true
        (E.complete_function_header target ~entry:original
           ~function_call_shape:shape
        |> Result.is_error))
    [ root; other ];
  let completed =
    E.complete_function_header task ~entry:original ~function_call_shape:shape
    |> checked
  in
  let same_entries message expected actual =
    Alcotest.(check bool)
      message true
      (List.length expected = List.length actual
      && List.for_all2 ( == ) expected actual)
  in
  same_entries "root keeps publication order without replacing a shadow"
    [ baseline; completed; hidden ]
    (E.all root);
  same_entries "copy keeps only the original visible immutable snapshots"
    [ baseline; original ] (E.all copied);
  same_entries "owner sees baseline and own completed entry"
    [ baseline; completed ] (E.all task);
  let copied_completion =
    E.complete_function_header copied ~entry:original
      ~function_call_shape:{ shape with variadic = true }
    |> checked
  in
  Alcotest.(check bool)
    "detached completion changes neither root nor owner" true
    (Option.get (E.find_function copied "F") == copied_completion
    && Option.get (E.find_function task "F") == completed);
  let context = E.begin_local_context task in
  ignore (E.add_local task context ~name:"Baseline" |> checked);
  Alcotest.(check bool)
    "local contexts belong to the view" true
    (E.find_preprocessor task "Baseline" = Symbol_visibility.Shadowed_by_local
    && E.find_preprocessor root "Baseline" = Symbol_visibility.Present baseline
    && E.find_preprocessor other "Baseline" = Symbol_visibility.Present baseline
    );
  ignore (E.end_local_context task context |> checked);
  let later = E.add root ~name:"Later" ~kind:Symbol_visibility.Class () in
  Alcotest.(check bool)
    "baseline updates are shared, snapshots stay detached" true
    (E.find_preprocessor task "Later" = Symbol_visibility.Present later
    && E.find_preprocessor copied "Later" = Symbol_visibility.Absent)

let tests =
  [
    Alcotest.test_case
      "task views preserve writer ownership and detached copies" `Quick
      task_views_and_detached_snapshots;
    Alcotest.test_case
      "provisional completion retains owner order and snapshots" `Quick
      provisional_function_completion;
    Alcotest.test_case "source hash bits" `Quick source_kind_bits;
    Alcotest.test_case "session built-ins" `Quick session_builtins;
    Alcotest.test_case "import filtering" `Quick import_filtering;
    Alcotest.test_case "local shadowing" `Quick local_shadowing;
    Alcotest.test_case "function call shapes" `Quick function_call_shapes;
    Alcotest.test_case "deterministic dump" `Quick deterministic_dump;
    Alcotest.test_case "deterministic JSON" `Quick deterministic_json;
  ]
