open Holyc_lib
module V = Symbol_visibility
module P = Preprocessor

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let check = Alcotest.(check bool)

let stream ?symbols ?definitions ?observe session contents =
  let source =
    Session.add_source session ~path:"definition-selection.hc" ~contents
  in
  P.create ?lexical_lookup:observe ~sources:(Session.sources session)
    ~definitions:
      (Option.value definitions ~default:(Session.definitions session))
    ~symbols:(Option.value symbols ~default:(Session.symbols session))
    ~config:(P.Config.create () |> checked)
    source

let next stream =
  match P.next stream with
  | Lexer.Token token -> token
  | Lexer.Diagnostic diagnostic -> Alcotest.fail diagnostic.Diagnostic.message

let raw stream = (next stream).Token.raw

let publication_and_order () =
  let session = Session.create () in
  let symbols = Session.symbols session in
  let observations = ref [] in
  let input =
    stream session "#define M 7\nM M\n#define M 9\nM" ~observe:(fun lookup ->
        observations := lookup :: !observations)
  in
  Alcotest.(check string) "original replacement" "7" (raw input);
  let definition =
    Definition.Environment.find (Session.definitions session) "M" |> Option.get
  in
  let entry =
    match V.Environment.find_preprocessor symbols "M" with
    | V.Present entry -> entry
    | _ -> Alcotest.fail "missing ordered source definition"
  in
  check "definition kind" true (V.kind entry = V.Definition);
  check "physical replacement payload" true
    (Option.fold ~none:false ~some:(( == ) definition)
       (V.definition_payload entry));
  let selected =
    List.find
      (fun lookup ->
        (P.lexical_lookup_token lookup).raw = "M"
        && Option.is_some (P.lexical_lookup_definition lookup))
      !observations
  in
  check "lexical observation selects the definition entry" true
    (match P.lexical_lookup_selection selected with
    | V.Present selected -> selected == entry
    | _ -> false);
  let function_entry =
    V.Environment.add symbols ~name:"M" ~kind:V.Function ()
  in
  Alcotest.(check string) "newer function prevents expansion" "M" (raw input);
  Alcotest.(check string) "later definition wins again" "9" (raw input);
  check "function-only join lookup still selects original function" true
    (Option.fold ~none:false ~some:(( == ) function_entry)
       (V.Environment.find_function symbols "M"));
  check "saved entry retains original bytes" true
    (Option.fold ~none:false
       ~some:(fun definition -> Definition.replacement definition = "7")
       (V.definition_payload entry))

let every_nondefinition_kind_suppresses () =
  List.iter
    (fun kind ->
      let session = Session.create () in
      let symbols = Session.symbols session in
      let input = stream session "#define M 7\nM M" in
      Alcotest.(check string) "first expansion" "7" (raw input);
      ignore (V.Environment.add symbols ~name:"M" ~kind ());
      Alcotest.(check string)
        ("selected " ^ V.kind_name kind ^ " prevents expansion")
        "M" (raw input))
    [
      V.Function;
      V.Class;
      V.Global_variable;
      V.Keyword;
      V.Export_system_symbol;
      V.Internal_type;
      V.Register;
      V.Word;
      V.Dictionary_word;
      V.Assembly_keyword;
      V.Opcode;
      V.File;
      V.Module;
      V.Help_file;
      V.Frame_pointer;
    ];
  let session = Session.create () in
  let input = stream session "#define M 7\nM M" in
  Alcotest.(check string) "before masked import" "7" (raw input);
  ignore
    (V.Environment.add (Session.symbols session) ~name:"M"
       ~kind:V.Import_system_symbol ());
  Alcotest.(check string)
    "excluded import does not displace selected definition" "7" (raw input)

let local_shadow_restores () =
  let session = Session.create () in
  let symbols = Session.symbols session in
  let input = stream session "#define M 7\nM M M" in
  Alcotest.(check string) "before local" "7" (raw input);
  let local = V.Environment.begin_local_context symbols in
  V.Environment.add_local symbols local ~name:"M" |> checked;
  Alcotest.(check string)
    "member selection suppresses expansion" "M" (raw input);
  V.Environment.end_local_context symbols local |> checked;
  Alcotest.(check string) "restored hash selection expands" "7" (raw input)

let original_selection_survives_observer_mutation () =
  let session = Session.create () in
  let symbols = Session.symbols session in
  let changed = ref false in
  let input =
    stream session "#define M 7\nM M" ~observe:(fun lookup ->
        if
          (P.lexical_lookup_token lookup).raw = "M"
          && Option.is_some (P.lexical_lookup_definition lookup)
          && not !changed
        then (
          changed := true;
          ignore (V.Environment.add symbols ~name:"M" ~kind:V.Function ())))
  in
  Alcotest.(check string)
    "original raw selection expands exact old payload" "7" (raw input);
  Alcotest.(check string)
    "later lexer read selects intervening function" "M" (raw input)

let definition_writer_ownership () =
  let session = Session.create () in
  let input = stream session "#define M 7\nM" in
  ignore (next input);
  let definitions = Session.definitions session in
  let definition = Definition.Environment.find definitions "M" |> Option.get in
  check "original definition writer owns object" true
    (Definition.Environment.owns definitions definition);
  let other = Definition.Environment.create () in
  check "foreign store rejects same spelling object" false
    (Definition.Environment.owns other definition);
  check "foreign publication rejects" true
    (Result.is_error
       (V.Environment.add_definition (V.Environment.create ())
          ~definitions:other ~definition));
  let child = Definition.Environment.task_view definitions in
  check "baseline visibility is not child writer ownership" false
    (Definition.Environment.owns child definition);
  check "visible baseline cannot be republished by child" true
    (Result.is_error
       (V.Environment.add_definition (V.Environment.create ())
          ~definitions:child ~definition));
  let plain =
    V.Environment.add (Session.symbols session) ~name:"M" ~kind:V.Definition ()
  in
  check "plain registration does not acquire replacement payload" true
    (Option.is_none (V.definition_payload plain))

let task_definition_visibility () =
  let session = Session.create () in
  let base_symbols = Session.symbols session in
  let base_definitions = Session.definitions session in
  let left_symbols = V.Environment.task_view base_symbols in
  let right_symbols = V.Environment.task_view base_symbols in
  let left_definitions = Definition.Environment.task_view base_definitions in
  let right_definitions = Definition.Environment.task_view base_definitions in
  let left =
    stream session "#define M 7\nM" ~symbols:left_symbols
      ~definitions:left_definitions
  in
  Alcotest.(check string) "own task definition expands" "7" (raw left);
  let right =
    stream session "M" ~symbols:right_symbols ~definitions:right_definitions
  in
  Alcotest.(check string) "sibling cannot see definition" "M" (raw right);
  let copied =
    stream session "M"
      ~symbols:(V.Environment.copy left_symbols)
      ~definitions:(Definition.Environment.copy left_definitions)
  in
  Alcotest.(check string)
    "copied environment retains exact immutable replacement" "7" (raw copied)

let source_locals_and_parameters () =
  List.iter
    (fun contents ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"source-definition-shadow.hc" ~contents
      in
      let config = P.Config.create ~compilation_mode:P.Jit () |> checked in
      let report =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      match integer_program_report_outcome report with
      | Error diagnostics ->
          Alcotest.fail
            (String.concat "; "
               (List.map
                  (fun diagnostic -> diagnostic.Diagnostic.message)
                  diagnostics))
      | Ok result ->
          Alcotest.(check (option int64))
            "original member selection executes" (Some 42L)
            (Ir_integer_interpreter.final_value result.value
            |> Option.map (fun word -> word.Ir_integer_interpreter.bits)))
    [
      "I64 F(){I64 n=42;\n#define n 7\nreturn n;}F();";
      "I64 F(I64 n){\n#define n 7\nreturn n;}F(42);";
    ]

let tests =
  [
    Alcotest.test_case "ordered definition publication and saved payload" `Quick
      publication_and_order;
    Alcotest.test_case "all selected nondefinition kinds suppress expansion"
      `Quick every_nondefinition_kind_suppresses;
    Alcotest.test_case "local suppression and restored selection" `Quick
      local_shadow_restores;
    Alcotest.test_case "original raw selection survives observer mutation"
      `Quick original_selection_survives_observer_mutation;
    Alcotest.test_case "definition writer and foreign publication" `Quick
      definition_writer_ownership;
    Alcotest.test_case "task visibility and immutable copied payload" `Quick
      task_definition_visibility;
    Alcotest.test_case "source locals and parameters suppress macro expansion"
      `Quick source_locals_and_parameters;
  ]
