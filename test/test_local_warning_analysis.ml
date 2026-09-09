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

let config mode = checked (Preprocessor.Config.create ~compilation_mode:mode ())

type prepared = {
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  function_types : Semantic_function_type_resolution.t;
  local_types : Semantic_local_type_resolution.t;
  bindings : Semantic_function_binding_index.t;
  expressions : Semantic_function_expression_binding.t;
}

let prepare ?(mode = Preprocessor.Jit) ?capture_queries ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    match capture_queries with
    | None ->
        Holyc_lib.parse_with_config session ~config:(config mode) ~source
        |> expect_ast
    | Some query ->
        let commands : Parser.command_sink =
          {
            checkpoint = None;
            reference = None;
            query = Some query;
            declaration = None;
            dimension_count = None;
            command = (fun _ -> Ok ());
            resume = (fun () -> Ok ());
          }
        in
        Parser.parse ~commands ~sources:(Session.sources session)
          ~symbols:(Session.symbols session)
          ~definitions:(Session.definitions session)
          ~config:(config mode) source
        |> Test_parser.expect_ast
  in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let function_types =
    checked
      (Holyc_lib.resolve_function_types session ~declarations ~aggregates
         ~functions ast)
  in
  let local_types =
    checked
      (Holyc_lib.resolve_local_types session ~declarations ~aggregates
         ~functions ast)
  in
  let bindings =
    checked
      (Holyc_lib.index_function_bindings session ~declarations ~functions
         ~function_types ~local_types)
  in
  let expressions =
    checked
      (Holyc_lib.resolve_function_expressions session ~declarations ~functions
         ~local_types ~bindings ast)
  in
  {
    session;
    ast;
    declarations;
    function_types;
    local_types;
    bindings;
    expressions;
  }

let analyze ?compiler_option_mask prepared =
  Holyc_lib.analyze_local_warnings ?compiler_option_mask prepared.session
    ~declarations:prepared.declarations ~function_types:prepared.function_types
    ~local_types:prepared.local_types ~bindings:prepared.bindings
    ~expressions:prepared.expressions prepared.ast
  |> checked

let function_named result name =
  Semantic_local_warning_analysis.functions result
  |> List.find (fun function_ ->
      function_ |> Semantic_local_warning_analysis.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let binding_name binding =
  binding |> Semantic_local_warning_analysis.binding_source
  |> fun (source : Semantic_function_binding_index.binding) ->
  Semantic_symbol.name source.symbol

let binding_named function_ name =
  Semantic_local_warning_analysis.function_bindings function_
  |> List.find (fun binding -> String.equal (binding_name binding) name)

let warning_signature warning =
  ( Semantic_local_warning_analysis.warning_code warning,
    warning |> Semantic_local_warning_analysis.warning_kind
    |> Semantic_local_warning_analysis.warning_kind_name,
    warning |> Semantic_local_warning_analysis.warning_binding_symbol
    |> Semantic_symbol.name )

let counts_flags_and_thresholds () =
  let prepared =
    prepare ~path:"local-warning-thresholds.HC"
      "U0 Diagnose(I64 used,I64 suppressed,I64 noisy,...){\n\
       used;\n\
       no_warn suppressed;\n\
       no_warn noisy;\n\
       noisy;\n\
       no_warn argc,argv;\n\
       I64 automatic;\n\
       static I64 stored;\n\
       no_warn stored;\n\
       }"
  in
  let result = analyze prepared in
  let function_ = function_named result "Diagnose" in
  let counts name =
    let binding = binding_named function_ name in
    ( Semantic_local_warning_analysis.binding_ordinary_use_count binding,
      Semantic_local_warning_analysis.binding_suppression_count binding,
      Semantic_local_warning_analysis.binding_source_use_count binding )
  in
  Alcotest.(check (triple int int int)) "ordinary use" (1, 0, 1) (counts "used");
  Alcotest.(check (triple int int int))
    "suppressed unused" (0, 1, 1) (counts "suppressed");
  Alcotest.(check (triple int int int))
    "unneeded suppression threshold" (1, 1, 2) (counts "noisy");
  Alcotest.(check (triple int int int))
    "unused automatic" (0, 0, 0) (counts "automatic");
  let argc = binding_named function_ "argc" in
  Alcotest.(check bool)
    "argc keeps its variadic flag" true
    (Semantic_local_warning_analysis.binding_has_flag argc Member_flag.Variadic);
  Alcotest.(check bool)
    "argc gains the no-warning flag" true
    (Semantic_local_warning_analysis.binding_has_flag argc
       Member_flag.No_unused_warning);
  let stored = binding_named function_ "stored" in
  Alcotest.(check bool)
    "static local keeps its storage flag" true
    (Semantic_local_warning_analysis.binding_has_flag stored Member_flag.Static);
  Alcotest.(check bool)
    "static local gains the no-warning flag" true
    (Semantic_local_warning_analysis.binding_has_flag stored
       Member_flag.No_unused_warning);
  Alcotest.(check (list (triple string string string)))
    "warning order and thresholds"
    [
      ("HCSEMA0035", "unneeded-no-warn", "noisy");
      ("HCSEMA0034", "unused-variable", "automatic");
    ]
    (Semantic_local_warning_analysis.function_warnings function_
    |> List.map warning_signature);
  Alcotest.(check (list string))
    "plain messages"
    [
      "unneeded no_warn for \"noisy\" in function \"Diagnose\"";
      "unused variable \"automatic\" in function \"Diagnose\"";
    ]
    (Semantic_local_warning_analysis.function_warnings function_
    |> List.map Semantic_local_warning_analysis.warning_message)

let options_repeats_and_prototypes () =
  let prepared =
    prepare ~path:"local-warning-options.HC"
      "extern U0 Prototype(I64 prototype_arg);\n\
       U0 Options(I64 unused,I64 repeated,I64 _anon_){\n\
       no_warn repeated;no_warn repeated;\n\
       no_warn _anon_;_anon_;\n\
       }"
  in
  let disabled_mask, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Warn_unused_var false
  in
  let result = analyze ~compiler_option_mask:disabled_mask prepared in
  let prototype = function_named result "Prototype" in
  Alcotest.(check bool)
    "prototype does not receive body warnings" false
    (Semantic_local_warning_analysis.function_is_definition prototype);
  Alcotest.(check (list string))
    "prototype warnings" []
    (Semantic_local_warning_analysis.function_warnings prototype
    |> List.map Semantic_local_warning_analysis.warning_code);
  let function_ = function_named result "Options" in
  Alcotest.(check (list (triple string string string)))
    "unneeded suppression ignores the unused option"
    [ ("HCSEMA0035", "unneeded-no-warn", "repeated") ]
    (Semantic_local_warning_analysis.function_warnings function_
    |> List.map warning_signature);
  let repeated = binding_named function_ "repeated" in
  Alcotest.(check (pair int int))
    "repeated suppressions contribute twice" (2, 2)
    ( Semantic_local_warning_analysis.binding_suppression_count repeated,
      Semantic_local_warning_analysis.binding_source_use_count repeated );
  let anonymous = binding_named function_ "_anon_" in
  Alcotest.(check int)
    "anonymous source counter crosses the threshold" 2
    (Semantic_local_warning_analysis.binding_source_use_count anonymous);
  Alcotest.(check bool)
    "the _anon_ spelling remains exempt" true
    (Semantic_local_warning_analysis.function_warnings function_
    |> List.for_all (fun warning ->
        warning |> Semantic_local_warning_analysis.warning_binding_symbol
        |> Semantic_symbol.name
        |> fun name -> not (String.equal name "_anon_")))

let initializer_resets_only_the_declared_local () =
  let result =
    prepare ~path:"local-warning-initializers.HC"
      "U0 Initializers(I64 earlier){I64 \
       ordinary_self=ordinary_self,query_self=sizeof(query_self),later=offset(earlier.member);}"
    |> analyze
  in
  let function_ = function_named result "Initializers" in
  let counts name =
    binding_named function_ name
    |> Semantic_local_warning_analysis.binding_source_use_count
  in
  Alcotest.(check int) "earlier local use survives" 1 (counts "earlier");
  Alcotest.(check int) "ordinary self-use is reset" 0 (counts "ordinary_self");
  Alcotest.(check int) "query self-use is reset" 0 (counts "query_self");
  Alcotest.(check int) "later initializer reset" 0 (counts "later");
  Alcotest.(check (list string))
    "only the declared locals are unused"
    [ "ordinary_self"; "query_self"; "later" ]
    (Semantic_local_warning_analysis.function_warnings function_
    |> List.map (fun warning ->
        warning |> Semantic_local_warning_analysis.warning_binding_symbol
        |> Semantic_symbol.name))

let specialized_queries_count_as_uses () =
  let result =
    prepare ~path:"local-warning-specialized-queries.HC"
      "U0 QueryCounts(I64 size_only,I64 offset_only,I64 defined_only,I64 \
       suppressed,I64 ordinary,I64 numeric){I64 \
       member;sizeof(size_only.member);offset(offset_only.member);defined(defined_only);no_warn \
       suppressed;sizeof(suppressed.member);ordinary;defined(0);}"
    |> analyze
  in
  let function_ = function_named result "QueryCounts" in
  let counts name =
    let binding = binding_named function_ name in
    ( Semantic_local_warning_analysis.binding_ordinary_use_count binding,
      ( Semantic_local_warning_analysis.binding_query_use_count binding,
        Semantic_local_warning_analysis.binding_suppression_count binding,
        Semantic_local_warning_analysis.binding_source_use_count binding ) )
  in
  let query_only name =
    Alcotest.(check (pair int (triple int int int)))
      (name ^ " query count")
      (0, (1, 0, 1))
      (counts name)
  in
  List.iter query_only [ "size_only"; "offset_only"; "defined_only" ];
  Alcotest.(check (pair int (triple int int int)))
    "query plus suppression crosses the unneeded threshold"
    (0, (1, 1, 2))
    (counts "suppressed");
  Alcotest.(check (pair int (triple int int int)))
    "ordinary use remains separate"
    (1, (0, 0, 1))
    (counts "ordinary");
  Alcotest.(check (pair int (triple int int int)))
    "non-name defined operand contributes no use"
    (0, (0, 0, 0))
    (counts "numeric");
  Alcotest.(check (pair int (triple int int int)))
    "member spelling contributes no use"
    (0, (0, 0, 0))
    (counts "member");
  Alcotest.(check (list (triple string string string)))
    "warning thresholds include query uses"
    [
      ("HCSEMA0035", "unneeded-no-warn", "suppressed");
      ("HCSEMA0034", "unused-variable", "numeric");
      ("HCSEMA0034", "unused-variable", "member");
    ]
    (Semantic_local_warning_analysis.function_warnings function_
    |> List.map warning_signature)

let selected_query_uses () =
  List.iter
    (fun (contents, expected) ->
      let receipts = ref [] in
      let capture_queries = function
        | Parser.Query_completed receipt ->
            receipts := receipt :: !receipts;
            Ok ()
        | _ -> Ok ()
      in
      let prepared =
        prepare ~capture_queries ~path:"selected-query-uses.HC" contents
      in
      let table = Session.semantic_symbols prepared.session in
      let indexed =
        List.hd (Semantic_function_binding_index.functions prepared.bindings)
      in
      let events =
        List.rev !receipts
        |> List.map (fun receipt ->
            let selection =
              Semantic_query_selection.make ~table ~receipt () |> checked
            in
            let operand =
              match receipt.Parser.query_root.query_node with
              | Parser.Defined_target operand -> operand
              | _ -> Alcotest.fail "expected defined query"
            in
            let location = operand.defined_operand_location in
            let origin =
              Semantic_symbol.Source_location
                {
                  span = location.span;
                  source_segments = location.source_segments;
                  generated_from = location.generated_from;
                  defined_at = location.defined_at;
                }
            in
            Semantic_function_expression_binding.make_selected_name_query
              ~selection
              ~role:Semantic_function_expression_binding.Defined_operand
              ~name:operand.defined_operand_spelling ~origin
            |> checked)
      in
      let locals =
        Semantic_function_binding_index.function_bindings indexed
        |> List.filter_map (fun binding ->
            match
              ( binding.Semantic_function_binding_index.local_declaration_index,
                binding.local_declarator_index )
            with
            | Some declaration_index, Some declarator_index ->
                Some
                  (Semantic_function_expression_binding.make_local_publication
                     ~name:(Semantic_symbol.name binding.symbol)
                     ~origin:(Semantic_symbol.origin binding.symbol)
                     ~declaration_index ~declarator_index
                  |> checked)
            | _ -> None)
      in
      let input =
        Semantic_function_expression_binding.make_function
          ~symbol:(Semantic_function_binding_index.function_symbol indexed)
          ~scope:(Semantic_function_binding_index.function_scope indexed)
          ~item_index:
            (Semantic_function_binding_index.function_item_index indexed)
          (events @ locals)
        |> checked
      in
      (if expected = 1 then
         let empty_input : Semantic_function_binding_index.function_input =
           {
             function_symbol =
               Semantic_function_binding_index.function_symbol indexed;
             function_scope =
               Semantic_function_binding_index.function_scope indexed;
             function_item_index =
               Semantic_function_binding_index.function_item_index indexed;
             function_bindings = [];
           }
         in
         let empty_bindings =
           Semantic_function_binding_index.build ~table
             ~parent:
               (Semantic_declaration_collection.scope prepared.declarations)
             [ empty_input ]
           |> Result.map_error Semantic_function_binding_index.error_to_string
           |> checked
         in
         Alcotest.(check bool)
           "saved local cannot borrow a missing source binding" true
           (Semantic_function_expression_binding.resolve ~table
              ~parent:
                (Semantic_declaration_collection.scope prepared.declarations)
              ~bindings:empty_bindings [ input ]
           |> Result.is_error));
      let expressions =
        Semantic_function_expression_binding.resolve ~table
          ~parent:(Semantic_declaration_collection.scope prepared.declarations)
          ~bindings:prepared.bindings [ input ]
        |> Result.map_error Semantic_function_expression_binding.error_to_string
        |> checked
      in
      let result = analyze { prepared with expressions } in
      let binding = binding_named (function_named result "F") "n" in
      Alcotest.(check int)
        "only parser-selected local receives query use" expected
        (Semantic_local_warning_analysis.binding_query_use_count binding))
    [
      ("I64 F(I64 n){return defined n;}", 1);
      ("I64 F(){defined n;I64 n;}", 0);
      ("I64 n;I64 F(){defined n;I64 n;}", 0);
    ]

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let mode_and_generated_provenance () =
  let source =
    "#define TARGET local\nU0 Generated(){I64 local;no_warn TARGET;}"
  in
  let signature mode =
    let function_ =
      prepare ~mode ~path:"local-warning-generated.HC" source |> analyze
      |> fun result -> function_named result "Generated"
    in
    let binding = binding_named function_ "local" in
    let origins =
      Semantic_local_warning_analysis.binding_suppression_origins binding
    in
    match origins with
    | [ origin ] ->
        let source = source_origin origin in
        Alcotest.(check bool)
          "generated origin" true
          (Option.is_some source.generated_from);
        Alcotest.(check bool)
          "definition origin" true
          (Option.is_some source.defined_at);
        ( Semantic_local_warning_analysis.binding_effective_flag_mask binding,
          Semantic_local_warning_analysis.binding_source_use_count binding,
          Semantic_local_warning_analysis.function_warnings function_
          |> List.map warning_signature )
    | _ -> Alcotest.fail "expected one generated suppression origin"
  in
  Alcotest.(check (triple int64 int (list (triple string string string))))
    "JIT and AOT warning facts"
    (signature Preprocessor.Jit)
    (signature Preprocessor.Aot)

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let included_provenance () =
  let directory = Filename.temp_dir "holyc-local-warnings-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "warnings.HC" in
      let root_contents = "#include \"warnings\"" in
      write_file root_path root_contents;
      write_file include_path "U0 Included(){I64 local;no_warn local;}";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let declarations = checked (Holyc_lib.collect_declarations session ast) in
      let aggregates =
        checked (Holyc_lib.resolve_aggregates session ~declarations ast)
      in
      let functions =
        checked (Holyc_lib.collect_functions session ~declarations ast)
      in
      let function_types =
        checked
          (Holyc_lib.resolve_function_types session ~declarations ~aggregates
             ~functions ast)
      in
      let local_types =
        checked
          (Holyc_lib.resolve_local_types session ~declarations ~aggregates
             ~functions ast)
      in
      let bindings =
        checked
          (Holyc_lib.index_function_bindings session ~declarations ~functions
             ~function_types ~local_types)
      in
      let expressions =
        checked
          (Holyc_lib.resolve_function_expressions session ~declarations
             ~functions ~local_types ~bindings ast)
      in
      let result =
        checked
          (Holyc_lib.analyze_local_warnings session ~declarations
             ~function_types ~local_types ~bindings ~expressions ast)
      in
      let origin =
        result |> fun result ->
        function_named result "Included" |> fun function_ ->
        binding_named function_ "local"
        |> Semantic_local_warning_analysis.binding_suppression_origins
        |> List.hd |> source_origin
      in
      let source_file =
        Source_manager.find (Session.sources session) origin.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included suppression keeps its canonical source" "warnings.HC"
        (Source_file.path source_file |> Filename.basename))

let result_signature result =
  Semantic_local_warning_analysis.functions result
  |> List.map (fun function_ ->
      ( function_ |> Semantic_local_warning_analysis.function_symbol
        |> Semantic_symbol.name,
        Semantic_local_warning_analysis.function_bindings function_
        |> List.map (fun binding ->
            ( binding_name binding,
              Semantic_local_warning_analysis.binding_effective_flag_mask
                binding,
              Semantic_local_warning_analysis.binding_ordinary_use_count binding,
              Semantic_local_warning_analysis.binding_query_use_count binding,
              Semantic_local_warning_analysis.binding_suppression_count binding
            )),
        Semantic_local_warning_analysis.function_warnings function_
        |> List.map warning_signature ))

let purity_and_validation () =
  let prepared =
    prepare ~path:"local-warning-purity.HC"
      "U0 Stable(I64 used){used;I64 unused;}"
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = analyze prepared in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = analyze prepared in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check bool)
    "deterministic repeated analysis" true
    (result_signature first = result_signature second);
  Alcotest.(check (pair int int))
    "symbol table stays unchanged" (before, before) (middle, after);
  let unknown_option = Int64.shift_left 1L 63 in
  (match
     Holyc_lib.analyze_local_warnings ~compiler_option_mask:unknown_option
       prepared.session ~declarations:prepared.declarations
       ~function_types:prepared.function_types ~local_types:prepared.local_types
       ~bindings:prepared.bindings ~expressions:prepared.expressions
       prepared.ast
   with
  | Ok _ -> Alcotest.fail "expected an unknown option bit to fail"
  | Error message ->
      Alcotest.(check bool)
        "stable option error" true
        (String.starts_with ~prefix:"HCSEMA0033: " message));
  let other =
    prepare ~path:"local-warning-other.HC" "U0 Other(I64 value){value;}"
  in
  (match
     Holyc_lib.analyze_local_warnings prepared.session
       ~declarations:prepared.declarations ~function_types:other.function_types
       ~local_types:prepared.local_types ~bindings:prepared.bindings
       ~expressions:prepared.expressions prepared.ast
   with
  | Ok _ -> Alcotest.fail "expected foreign function types to fail"
  | Error message ->
      Alcotest.(check bool)
        "stable ownership error" true
        (String.starts_with ~prefix:"HCSEMA0033: " message));
  Alcotest.(check int)
    "failed analysis does not add symbols" before
    (Semantic_symbol_table.all_symbols table |> List.length)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let pinned_warning_rules () =
  let statements = read_file "../third_party/TempleOS/Compiler/PrsStmt.HC" in
  let lexer = read_file "../third_party/TempleOS/Compiler/LexLib.HC" in
  let variables = read_file "../third_party/TempleOS/Compiler/PrsVar.HC" in
  let expressions = read_file "../third_party/TempleOS/Compiler/PrsExp.HC" in
  List.iter
    (fun rule -> Alcotest.(check bool) rule true (contains statements rule))
    [
      "tmpm->flags|=MLF_NO_UNUSED_WARN;";
      "tmpm->use_cnt>1&&StrCmp(tmpm->str,\"_anon_\")";
      "!tmpm->use_cnt && GetOption(OPTf_WARN_UNUSED_VAR)";
    ];
  Alcotest.(check bool)
    "member lookup increments the counter" true
    (contains lexer "tmpm->use_cnt++;");
  Alcotest.(check bool)
    "initializer parsing resets the declared local" true
    (contains variables "tmpm->use_cnt=0;");
  Alcotest.(check bool)
    "specialized roots can resolve locals" true
    (contains expressions "tmpm=cc->local_var_entry");
  Alcotest.(check bool)
    "member spellings undo incidental local lookups" true
    (contains expressions "cc->local_var_entry->use_cnt--;");
  Alcotest.(check bool)
    "defined accepts a resolved local" true
    (contains expressions "(cc->hash_entry || cc->local_var_entry)")

let tests =
  [
    Alcotest.test_case "selected queries preserve exact local use counts" `Quick
      selected_query_uses;
    Alcotest.test_case "counts, flags, and warning thresholds" `Quick
      counts_flags_and_thresholds;
    Alcotest.test_case "options, repeats, and prototypes" `Quick
      options_repeats_and_prototypes;
    Alcotest.test_case "initializer resets only the declared local" `Quick
      initializer_resets_only_the_declared_local;
    Alcotest.test_case "specialized queries count as uses" `Quick
      specialized_queries_count_as_uses;
    Alcotest.test_case "mode and generated provenance" `Quick
      mode_and_generated_provenance;
    Alcotest.test_case "included provenance" `Quick included_provenance;
    Alcotest.test_case "purity and validation" `Quick purity_and_validation;
    Alcotest.test_case "pinned warning rules" `Quick pinned_warning_rules;
  ]
