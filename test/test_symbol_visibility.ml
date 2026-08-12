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
  Alcotest.(check int) "checked built-in entries" 570 (List.length entries);
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
  Alcotest.(check int) "last stable ID" 569 (Symbol_visibility.id last);
  Alcotest.(check string)
    "last seeded spelling" "MOV_RAX_CR4"
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
        "fixed parameter count" 2 (List.length retained.parameters);
      Alcotest.(check bool) "variadic marker" true retained.variadic;
      Alcotest.(check (option string))
        "parameter name" (Some "first")
        (List.hd retained.parameters).parameter_name;
      Alcotest.(check bool)
        "default availability" true
        (List.hd retained.parameters).has_default
  | None -> Alcotest.fail "the function call shape was not retained");
  Alcotest.check_raises "nonfunction call shape is rejected"
    (Invalid_argument "only function symbols may carry a function call shape")
    (fun () ->
      ignore
        (Symbol_visibility.Environment.add symbols ~name:"NotCallable"
           ~kind:Symbol_visibility.Global_variable ~function_call_shape:shape
           ()))

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
    (contains_text first "holyc-symbol-visibility-v1\n");
  Alcotest.(check bool)
    "source origin" true
    (contains_text first
       "symbol 570 name=\"UserFunction\" kind=function \
        origin=visibility.HC:1:1..1:2")

let tests =
  [
    Alcotest.test_case "source hash bits" `Quick source_kind_bits;
    Alcotest.test_case "session built-ins" `Quick session_builtins;
    Alcotest.test_case "import filtering" `Quick import_filtering;
    Alcotest.test_case "local shadowing" `Quick local_shadowing;
    Alcotest.test_case "function call shapes" `Quick function_call_shapes;
    Alcotest.test_case "deterministic dump" `Quick deterministic_dump;
  ]
