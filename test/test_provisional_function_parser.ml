open Holyc_lib

(* Missing member publication leaves nested source execution unable to observe
   the parameter before its default starts. These fixtures use real #exe entry
   and the real parser, independently of the semantic ledger. *)
let sink declaration : Parser.command_sink =
  {
    checkpoint = None;
    reference = None;
    implicit_output = None;
    query = None;
    declaration = Some declaration;
    dimension_count = None;
    command = (fun _ -> Ok ());
    resume = (fun () -> Ok ());
  }

let parse ?(on_enter = fun () -> ()) declaration source =
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~same_task:true ~commands:(sink declaration)
      ~on_enter
      ~configure:(fun _ execution ->
        { execution with Parser.commands = sink declaration })
      source
  in
  parsed

let event_name = function
  | Parser.Function_declared _ -> "function"
  | Parser.Function_parameter_declared _ -> "parameter"
  | Parser.Parameter_default_completed _ -> "default"
  | Parser.Function_parameter_completed _ -> "completion"
  | Parser.Function_variadic_started _ -> "ellipsis"
  | Parser.Function_variadic_completed _ -> "variadic"
  | Parser.Function_header_completed _ -> "header"
  | Parser.Function_body_completed _ -> "body"
  | _ -> "member"

let activity = function
  | Parser.Function_declared receipt ->
      Some (fun () -> Parser.function_publication_is_current receipt)
  | Parser.Function_parameter_declared receipt ->
      Some (fun () -> Parser.function_parameter_is_current receipt)
  | Parser.Parameter_default_completed receipt ->
      Some (fun () -> Parser.parameter_default_is_current receipt)
  | Parser.Function_parameter_completed receipt ->
      Some (fun () -> Parser.function_parameter_completion_is_current receipt)
  | Parser.Function_variadic_started receipt ->
      Some (fun () -> Parser.function_variadic_start_is_current receipt)
  | Parser.Function_variadic_completed receipt ->
      Some (fun () -> Parser.function_variadic_completion_is_current receipt)
  | Parser.Function_header_completed receipt ->
      Some (fun () -> Parser.function_header_is_current receipt)
  | _ -> None

let same label left right = Alcotest.(check bool) label true (left == right)

let phase_order source expected () =
  let seen = ref [] in
  let record name = seen := name :: !seen in
  let declaration event =
    record (event_name event);
    Ok ()
  in
  let parsed = parse ~on_enter:(fun () -> record "exe") declaration source in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (list string))
    "native member and lexer phase order" expected (List.rev !seen)

let nested_observer () =
  let head = ref None and default = ref None and completed = ref None in
  let header = ref None and entries = ref 0 in
  let declaration event =
    (match event with
    | Parser.Function_parameter_declared receipt -> head := Some receipt
    | Parser.Parameter_default_completed receipt -> default := Some receipt
    | Parser.Function_parameter_completed receipt -> completed := Some receipt
    | Parser.Function_header_completed receipt -> header := Some receipt
    | _ -> ());
    Ok ()
  in
  let on_enter () =
    incr entries;
    match !entries with
    | 1 -> ()
    | 2 ->
        let head = Option.get !head in
        Alcotest.(check string)
          "nested default input sees original name" "n"
          (Option.get head.parameter_name).spelling;
        Alcotest.(check bool)
          "default unavailable before input" true
          (Option.is_none !default && Option.is_none !completed);
        Alcotest.(check bool)
          "head callback already released" false
          (Parser.function_parameter_is_current head)
    | 3 ->
        let receipt = Option.get !completed and value = Option.get !default in
        same "close lookahead sees completed original head" (Option.get !head)
          receipt.parameter_publication;
        same "close lookahead sees exact original default" value.default_ast
          (Option.get receipt.parameter_ast.default);
        same "default keeps original member" value.default_parameter
          receipt.parameter_publication;
        Alcotest.(check bool)
          "header still provisional" true (Option.is_none !header)
    | _ -> Alcotest.fail "unexpected source execution"
  in
  let parsed =
    parse ~on_enter declaration
      {|#exe {I64 F(I64 n=#exe {}40)#exe {}{return n;}}|}
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check int) "all nested entries executed" 3 !entries;
  same "header retains original completion" (Option.get !completed)
    (List.hd (Option.get !header).parameter_completions)

let exact_prototype () =
  let heads = ref [] and completions = ref [] and defaults = ref [] in
  let header = ref None and ellipsis = ref None and active = ref [] in
  let declaration event =
    Option.iter
      (fun current ->
        Alcotest.(check bool)
          "only original callback is active" true (current ());
        List.iter
          (fun previous ->
            Alcotest.(check bool)
              "preceding callback released" false (previous ()))
          !active;
        active := current :: !active)
      (activity event);
    (match event with
    | Parser.Function_parameter_declared receipt -> heads := receipt :: !heads
    | Parser.Function_parameter_completed receipt ->
        completions := receipt :: !completions
    | Parser.Parameter_default_completed receipt ->
        defaults := receipt :: !defaults
    | Parser.Function_variadic_started receipt -> ellipsis := Some receipt
    | Parser.Function_variadic_completed receipt ->
        same "both variadic phases retain the same receipt"
          (Option.get !ellipsis) receipt
    | Parser.Function_header_completed receipt -> header := Some receipt
    | _ -> ());
    Ok ()
  in
  let parsed =
    parse declaration
      {|extern I64 F(;;reg I64 *p;I64 (*callback)(U8 n=2,;;),;U8 *name=lastclass,...);|}
  in
  let ast = Test_parser.expect_ast parsed in
  let prototype = Test_parser.expect_one_prototype ast in
  let header = Option.get !header in
  let heads = List.rev !heads and completions = List.rev !completions in
  Alcotest.(check int)
    "callback children do not publish outer members" 3 (List.length heads);
  same "header retains original parameter list" prototype.parameters
    header.parameters;
  List.iteri
    (fun index (parameter : Ast.function_parameter) ->
      let head = List.nth heads index
      and completed = List.nth completions index in
      Alcotest.(check int) "source member index" index head.parameter_index;
      same "completion keeps head" head completed.parameter_publication;
      same "completion keeps original final AST" parameter
        completed.parameter_ast;
      same "header keeps original completion" completed
        (List.nth header.parameter_completions index);
      same "register qualifiers" parameter.register_qualifiers
        head.parameter_register_qualifiers;
      same "type child" parameter.type_specifier head.parameter_type_specifier;
      same "pointer children" parameter.pointer_layers
        head.parameter_pointer_layers;
      same "name child" parameter.name head.parameter_name;
      same "recursive callback child" parameter.function_pointer
        head.parameter_function_pointer;
      if index = 0 then
        Alcotest.(check bool)
          "first member has no predecessor" true
          (Option.is_none head.parameter_predecessor)
      else
        same "member predecessor is the prior accepted completion"
          (List.nth completions (index - 1))
          (Option.get head.parameter_predecessor))
    prototype.parameters;
  Alcotest.(check int)
    "callback default stays inside its original AST" 1 (List.length !defaults);
  let callback =
    Option.get (List.nth prototype.parameters 1).function_pointer
  in
  Alcotest.(check bool)
    "nested callback default retained" true
    (Option.is_some (List.hd callback.signature_parameters).default);
  let variadic = Option.get !ellipsis in
  same "variadic original marker"
    (Option.get prototype.variadic)
    variadic.variadic_marker;
  same "header original variadic receipt" variadic
    (Option.get header.variadic_publication);
  same "variadic member predecessor" (List.nth completions 2)
    (Option.get variadic.variadic_parameter_predecessor);
  List.iter
    (fun current ->
      Alcotest.(check bool) "stale callback released" false (current ()))
    !active

exception Observer_failure

let callback_failure exceptional target source expected_entries () =
  let entered = ref 0 and saved = ref None and rejected = ref false in
  let declaration event =
    if !rejected then Alcotest.fail "rejected callback allowed a later event";
    if event_name event = target then (
      rejected := true;
      let current = Option.get (activity event) in
      saved := Some current;
      Alcotest.(check bool) "failing callback starts current" true (current ());
      if exceptional then raise Observer_failure
      else
        let primary =
          match event with
          | Parser.Function_declared receipt ->
              receipt.function_name.location.span
          | Parser.Function_parameter_declared receipt ->
              receipt.parameter_function.function_name.location.span
          | Parser.Function_parameter_completed receipt ->
              receipt.parameter_ast.location.span
          | Parser.Function_variadic_started receipt
          | Parser.Function_variadic_completed receipt ->
              receipt.variadic_marker.location.span
          | _ -> Alcotest.fail "unexpected rejected declaration phase"
        in
        Error
          [
            Diagnostic.make ~code:"TESTMEMBER" ~severity:Diagnostic.Error
              ~primary ~message:"member observer rejected" ();
          ])
    else Ok ()
  in
  let run () = parse ~on_enter:(fun () -> incr entered) declaration source in
  if exceptional then
    match run () with
    | _ -> Alcotest.fail "observer exception must escape"
    | exception Observer_failure -> ()
  else
    Alcotest.(check bool)
      "rejection stops parser" true
      (Parser.has_errors (run ()));
  Alcotest.(check bool)
    "failed callback cannot be borrowed later" false
    ((Option.get !saved) ());
  Alcotest.(check int)
    "failure stops before next lexer execution" expected_entries !entered;
  ignore
    (Test_parser.expect_ast
       (parse (fun _ -> Ok ()) {|I64 Recovered(I64 n=1);|}))

let malformed source expected () =
  let events = ref [] and entered = ref 0 in
  let parsed =
    parse
      ~on_enter:(fun () -> incr entered)
      (fun event ->
        events := event_name event :: !events;
        Ok ())
      source
  in
  Alcotest.(check bool)
    "malformed delimiter rejected" true (Parser.has_errors parsed);
  Alcotest.(check (list string))
    "native phases before delimiter failure" expected (List.rev !events);
  Alcotest.(check int) "malformed delimiter stops later directive" 0 !entered

let tests =
  [
    Alcotest.test_case "member exists before default input and close lookahead"
      `Quick
      (phase_order {|I64 F(I64 n=#exe {}40)#exe {}{return n;}|}
         [
           "function";
           "parameter";
           "exe";
           "default";
           "completion";
           "exe";
           "header";
           "body";
         ]);
    Alcotest.test_case "variadic flag precedes lookahead and members follow it"
      `Quick
      (phase_order {|I64 F(...#exe {})#exe {}{return argc;}|}
         [ "function"; "ellipsis"; "exe"; "variadic"; "exe"; "header"; "body" ]);
    Alcotest.test_case "type lookahead precedes original member publication"
      `Quick
      (phase_order {|I64 F(I64 n#exe {}=#exe {}40){return n;}|}
         [
           "function";
           "exe";
           "parameter";
           "exe";
           "default";
           "completion";
           "header";
           "body";
         ]);
    Alcotest.test_case "omitted variadic close preserves native member phase"
      `Quick
      (phase_order {|I64 F(...#exe {}{return argc;}|}
         [ "function"; "ellipsis"; "exe"; "variadic"; "header"; "body" ]);
    Alcotest.test_case "empty parameter entries allocate no members" `Quick
      (phase_order {|extern I64 F(;;;);|} [ "function"; "header" ]);
    Alcotest.test_case "nested source sees exact pending parameter phases"
      `Quick nested_observer;
    Alcotest.test_case "prototype receipts preserve recursive original children"
      `Quick exact_prototype;
    Alcotest.test_case
      "member survives malformed delimiter without false completion" `Quick
      (malformed {|I64 F(I64 n:#exe {});|} [ "function"; "parameter" ]);
    Alcotest.test_case "default completes before malformed delimiter rejection"
      `Quick
      (malformed {|I64 F(I64 n=40:#exe {});|}
         [ "function"; "parameter"; "default" ]);
  ]
  @ List.concat_map
      (fun (target, source, entries) ->
        List.map
          (fun exceptional ->
            Alcotest.test_case
              (target
              ^
              if exceptional then " exception releases callback"
              else " rejection releases callback")
              `Quick
              (callback_failure exceptional target source entries))
          [ false; true ])
      [
        ("function", {|I64 F(#exe {}I64 n);|}, 0);
        ("parameter", {|I64 F(I64 n=#exe {}40);|}, 0);
        ("completion", {|I64 F(I64 n=40,#exe {}I64 m);|}, 0);
        ("ellipsis", {|I64 F(...#exe {})#exe {};|}, 0);
        ("variadic", {|I64 F(...#exe {})#exe {};|}, 1);
      ]
