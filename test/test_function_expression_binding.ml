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

let config = checked (Preprocessor.Config.create ())

let parse session ~path contents =
  let source = Session.add_source session ~path ~contents in
  Holyc_lib.parse_with_config session ~config ~source |> expect_ast

type inputs = {
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  functions : Semantic_function_collection.t;
  local_types : Semantic_local_type_resolution.t;
  bindings : Semantic_function_binding_index.t;
}

let inputs ~path contents =
  let session = Session.create () in
  let ast = parse session ~path contents in
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
  { session; ast; declarations; functions; local_types; bindings }

let resolve inputs =
  Holyc_lib.resolve_function_expressions inputs.session
    ~declarations:inputs.declarations ~functions:inputs.functions
    ~local_types:inputs.local_types ~bindings:inputs.bindings inputs.ast

let prepare ~path contents = inputs ~path contents |> resolve |> checked

let function_named result name =
  Semantic_function_expression_binding.functions result
  |> List.find (fun function_ ->
      function_ |> Semantic_function_expression_binding.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let occurrences function_ =
  Semantic_function_expression_binding.function_occurrences function_

let binding_of_occurrence occurrence =
  match
    Semantic_function_expression_binding.occurrence_resolution occurrence
  with
  | Semantic_function_expression_binding.Nonlocal_candidate -> None
  | Semantic_function_expression_binding.Function_binding binding ->
      Some binding

let occurrence_signature occurrence =
  let name = Semantic_function_expression_binding.occurrence_name occurrence in
  let binding_name =
    binding_of_occurrence occurrence
    |> Option.map (fun (binding : Semantic_function_binding_index.binding) ->
        Semantic_symbol.name binding.symbol)
  in
  (name, binding_name)

let signature function_ = List.map occurrence_signature (occurrences function_)

let parameters_variadics_and_nonlocals () =
  let result =
    prepare ~path:"parameter-expression-bindings.HC"
      "U0 Work(I64 first,...){first;argc;argv;1+outside;}"
  in
  let function_ = function_named result "Work" in
  Alcotest.(check (list (pair string (option string))))
    "body identifiers"
    [
      ("first", Some "first");
      ("argc", Some "argc");
      ("argv", Some "argv");
      ("outside", None);
    ]
    (signature function_);
  Alcotest.(check (list int))
    "source occurrence order" [ 0; 1; 2; 3 ]
    (occurrences function_
    |> List.map Semantic_function_expression_binding.occurrence_index);
  Alcotest.(check (list string))
    "parameter binding kinds"
    [ "named-parameter"; "variadic-argc"; "variadic-argv" ]
    (occurrences function_
    |> List.filter_map binding_of_occurrence
    |> List.map (fun (binding : Semantic_function_binding_index.binding) ->
        Semantic_function_binding_index.binding_kind_name binding.kind))

let declaration_publication_timing () =
  let result =
    prepare ~path:"local-publication-timing.HC"
      "U0 Timing(){1+value;I64 value[value]=value;value;}"
  in
  let function_ = function_named result "Timing" in
  Alcotest.(check (list (pair string (option string))))
    "declarator boundary"
    [
      ("value", None);
      ("value", None);
      ("value", Some "value");
      ("value", Some "value");
    ]
    (signature function_)

let comma_declarator_order () =
  let result =
    prepare ~path:"comma-local-publication.HC"
      "U0 Declarators(){I64 first=first,second[first]=second;first;second;}"
  in
  let function_ = function_named result "Declarators" in
  Alcotest.(check (list (pair string (option string))))
    "comma declarators publish separately"
    [
      ("first", Some "first");
      ("first", Some "first");
      ("second", Some "second");
      ("first", Some "first");
      ("second", Some "second");
    ]
    (signature function_)

let function_wide_scope_and_global_shadow () =
  let result =
    prepare ~path:"function-wide-local.HC"
      "I64 value;U0 Scope(){value;{I64 value;value;}value;}"
  in
  let function_ = function_named result "Scope" in
  Alcotest.(check (list (pair string (option string))))
    "remainder scope"
    [ ("value", None); ("value", Some "value"); ("value", Some "value") ]
    (signature function_);
  let resolved =
    occurrences function_ |> List.filter_map binding_of_occurrence
  in
  match resolved with
  | [ first; second ] ->
      Alcotest.(check bool)
        "later sibling selects the nested local" true
        (Semantic_symbol.Id.equal
           (Semantic_symbol.id first.symbol)
           (Semantic_symbol.id second.symbol))
  | _ -> Alcotest.fail "expected two local resolutions"

let permitted_repeats_keep_first () =
  let semantic_inputs =
    inputs ~path:"permitted-expression-binding-repeats.HC"
      "U0 Repeats(I64 pad){I64 pad;pad;I64 reserved;I64 reserved;reserved;I64 \
       _anon_;I64 _anon_;_anon_;}"
  in
  let result = resolve semantic_inputs |> checked in
  let function_ = function_named result "Repeats" in
  let indexed =
    Semantic_function_binding_index.functions semantic_inputs.bindings
    |> List.find (fun indexed ->
        indexed |> Semantic_function_binding_index.function_symbol
        |> Semantic_symbol.name |> String.equal "Repeats")
  in
  let indexed_bindings =
    Semantic_function_binding_index.function_bindings indexed
  in
  List.iter2
    (fun name occurrence ->
      let first =
        List.find
          (fun (binding : Semantic_function_binding_index.binding) ->
            String.equal (Semantic_symbol.name binding.symbol) name)
          indexed_bindings
      in
      match binding_of_occurrence occurrence with
      | None -> Alcotest.failf "expected %s to resolve" name
      | Some selected ->
          Alcotest.(check bool)
            (name ^ " selects its first source binding")
            true
            (Semantic_symbol.Id.equal
               (Semantic_symbol.id first.symbol)
               (Semantic_symbol.id selected.symbol)))
    [ "pad"; "reserved"; "_anon_" ]
    (occurrences function_)

let excluded_identifier_roles () =
  let source =
    "class C {I64 field;};\n\
     U0 Excluded(C obj){\n\
     I64 field;\n\
     obj.field;\n\
     sizeof(C.field);\n\
     offset(C.field);\n\
     defined(field);\n\
     hidden:\n\
     asm { MOV RAX,field; }\n\
     }"
  in
  let result = prepare ~path:"excluded-identifier-roles.HC" source in
  let function_ = function_named result "Excluded" in
  Alcotest.(check (list (pair string (option string))))
    "only ordinary expression identifiers"
    [ ("obj", Some "obj") ]
    (signature function_)

let statement_expression_order () =
  let source =
    "U0 Walk(I64 a,I64 b){\n\
     if(a)b;\n\
     while(a)b;\n\
     do a;while(b);\n\
     for(a;b;a)b;\n\
     switch(a){case b:a;case a...b:b;dft:a;}\n\
     try {a;}catch {b;}\n\
     lock {a;}\n\
     return b;\n\
     }"
  in
  let result = prepare ~path:"statement-expression-order.HC" source in
  let function_ = function_named result "Walk" in
  let names =
    occurrences function_
    |> List.map Semantic_function_expression_binding.occurrence_name
  in
  Alcotest.(check (list string))
    "statement source order"
    [
      "a";
      "b";
      "a";
      "b";
      "a";
      "b";
      "a";
      "b";
      "a";
      "b";
      "a";
      "b";
      "a";
      "a";
      "b";
      "b";
      "a";
      "a";
      "b";
      "a";
      "b";
    ]
    names;
  Alcotest.(check bool)
    "all statement references bind parameters" true
    (occurrences function_
    |> List.for_all (fun occurrence ->
        Option.is_some (binding_of_occurrence occurrence)))

let suppression_and_initializer_events () =
  let result =
    prepare ~path:"no-warn-binding-events.HC"
      "U0 Count(I64 parameter){parameter;no_warn parameter;I64 \
       local=local;no_warn local;local;}"
  in
  let function_ = function_named result "Count" in
  let binding_name (binding : Semantic_function_binding_index.binding) =
    Semantic_symbol.name binding.symbol
  in
  Alcotest.(check (list string))
    "ordinary uses stay separate"
    [ "parameter"; "local"; "local" ]
    (occurrences function_
    |> List.map Semantic_function_expression_binding.occurrence_name);
  let suppressions =
    Semantic_function_expression_binding.function_suppressions function_
  in
  Alcotest.(check (list (triple int string string)))
    "suppression bindings"
    [ (0, "parameter", "parameter"); (1, "local", "local") ]
    (suppressions
    |> List.map (fun suppression ->
        ( Semantic_function_expression_binding.suppression_index suppression,
          Semantic_function_expression_binding.suppression_name suppression,
          suppression
          |> Semantic_function_expression_binding.suppression_binding
          |> binding_name )));
  let resets =
    Semantic_function_expression_binding.function_initializer_use_resets
      function_
  in
  Alcotest.(check (list (pair int string)))
    "initializer reset binding" [ (0, "local") ]
    (resets
    |> List.map (fun reset ->
        ( Semantic_function_expression_binding.initializer_use_reset_index
            reset,
          reset
          |> Semantic_function_expression_binding.initializer_use_reset_binding
          |> binding_name )));
  let event_signature = function
    | Semantic_function_expression_binding.Bound_use occurrence ->
        "use:" ^ Semantic_function_expression_binding.occurrence_name occurrence
    | Semantic_function_expression_binding.No_warn_suppression suppression ->
        "suppress:"
        ^ Semantic_function_expression_binding.suppression_name suppression
    | Semantic_function_expression_binding.Initializer_use_reset reset ->
        "reset:"
        ^ (reset
          |> Semantic_function_expression_binding.initializer_use_reset_binding
          |> binding_name)
  in
  Alcotest.(check (list string))
    "source event order"
    [
      "use:parameter";
      "suppress:parameter";
      "use:local";
      "reset:local";
      "suppress:local";
      "use:local";
    ]
    (Semantic_function_expression_binding.function_binding_events function_
    |> List.map event_signature)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_provenance_and_purity () =
  let semantic_inputs =
    inputs ~path:"generated-expression-binding.HC"
      "#define USE generated\nU0 Generated(I64 generated){USE;}"
  in
  let table = Session.semantic_symbols semantic_inputs.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve semantic_inputs |> checked in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = resolve semantic_inputs |> checked in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  let first_function = function_named first "Generated" in
  let second_function = function_named second "Generated" in
  Alcotest.(check (list (pair string (option string))))
    "deterministic repeated resolution" (signature first_function)
    (signature second_function);
  Alcotest.(check (pair int int))
    "symbol table stays unchanged" (before, before) (middle, after);
  match occurrences first_function with
  | [ occurrence ] ->
      let source =
        source_origin
          (Semantic_function_expression_binding.occurrence_origin occurrence)
      in
      Alcotest.(check bool)
        "macro invocation provenance" true
        (Option.is_some source.generated_from);
      Alcotest.(check bool)
        "macro definition provenance" true
        (Option.is_some source.defined_at)
  | _ -> Alcotest.fail "expected one generated occurrence"

let expect_error_code expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable error code" expected
        (Semantic_function_expression_binding.error_code error)

let validation_errors () =
  let semantic_inputs =
    inputs ~path:"expression-binding-validation.HC" "U0 Validate(){I64 local;}"
  in
  let table = Session.semantic_symbols semantic_inputs.session in
  let parent =
    Semantic_declaration_collection.scope semantic_inputs.declarations
  in
  let indexed =
    Semantic_function_binding_index.functions semantic_inputs.bindings
    |> List.hd
  in
  let symbol = Semantic_function_binding_index.function_symbol indexed in
  let scope = Semantic_function_binding_index.function_scope indexed in
  let item_index =
    Semantic_function_binding_index.function_item_index indexed
  in
  let missing =
    checked
      (Semantic_function_expression_binding.make_function ~symbol ~scope
         ~item_index [])
  in
  expect_error_code "HCSEMA0019"
    (Semantic_function_expression_binding.resolve ~table ~parent
       ~bindings:semantic_inputs.bindings [ missing ]);
  let wrong_publication =
    checked
      (Semantic_function_expression_binding.make_local_publication ~name:"other"
         ~origin:(Semantic_symbol.Synthesized "mismatched local publication")
         ~declaration_index:0 ~declarator_index:0)
  in
  let mismatched =
    checked
      (Semantic_function_expression_binding.make_function ~symbol ~scope
         ~item_index [ wrong_publication ])
  in
  expect_error_code "HCSEMA0018"
    (Semantic_function_expression_binding.resolve ~table ~parent
       ~bindings:semantic_inputs.bindings [ mismatched ]);
  let indexed_local =
    Semantic_function_binding_index.function_bindings indexed
    |> List.find (fun (binding : Semantic_function_binding_index.binding) ->
        Option.is_some binding.local_declaration_index)
  in
  let publication =
    checked
      (Semantic_function_expression_binding.make_local_publication
         ~name:(Semantic_symbol.name indexed_local.symbol)
         ~origin:(Semantic_symbol.origin indexed_local.symbol)
         ~declaration_index:(Option.get indexed_local.local_declaration_index)
         ~declarator_index:(Option.get indexed_local.local_declarator_index))
  in
  let missing_suppression =
    checked
      (Semantic_function_expression_binding.make_no_warn_suppression
         ~name:"missing"
         ~origin:(Semantic_symbol.Synthesized "missing no_warn target"))
  in
  let bad_suppression =
    checked
      (Semantic_function_expression_binding.make_function ~symbol ~scope
         ~item_index [ missing_suppression; publication ])
  in
  expect_error_code "HCSEMA0020"
    (Semantic_function_expression_binding.resolve ~table ~parent
       ~bindings:semantic_inputs.bindings [ bad_suppression ]);
  let wrong_reset =
    checked
      (Semantic_function_expression_binding.make_initializer_use_reset
         ~name:"other"
         ~origin:(Semantic_symbol.Synthesized "mismatched initializer reset")
         ~declaration_index:(Option.get indexed_local.local_declaration_index)
         ~declarator_index:(Option.get indexed_local.local_declarator_index))
  in
  let bad_reset =
    checked
      (Semantic_function_expression_binding.make_function ~symbol ~scope
         ~item_index [ publication; wrong_reset ])
  in
  expect_error_code "HCSEMA0021"
    (Semantic_function_expression_binding.resolve ~table ~parent
       ~bindings:semantic_inputs.bindings [ bad_reset ]);
  let foreign = Semantic_symbol_table.create () in
  expect_error_code "HCSEMA0017"
    (Semantic_function_expression_binding.resolve ~table
       ~parent:(Semantic_symbol_table.root foreign)
       ~bindings:semantic_inputs.bindings [ missing ]);
  let other_ast =
    parse semantic_inputs.session ~path:"different-expression-binding.HC"
      "U0 Different(){}"
  in
  match
    Holyc_lib.resolve_function_expressions semantic_inputs.session
      ~declarations:semantic_inputs.declarations
      ~functions:semantic_inputs.functions
      ~local_types:semantic_inputs.local_types
      ~bindings:semantic_inputs.bindings other_ast
  with
  | Ok _ -> Alcotest.fail "expected the mismatched AST to be rejected"
  | Error message ->
      Alcotest.(check bool)
        "driver uses the stable validation family" true
        (String.starts_with ~prefix:"HCSEMA0017: " message)

let tests =
  [
    Alcotest.test_case "parameters, variadics, and nonlocals" `Quick
      parameters_variadics_and_nonlocals;
    Alcotest.test_case "declaration publication timing" `Quick
      declaration_publication_timing;
    Alcotest.test_case "comma declarator order" `Quick comma_declarator_order;
    Alcotest.test_case "function-wide scope and global shadow" `Quick
      function_wide_scope_and_global_shadow;
    Alcotest.test_case "permitted repeats keep first" `Quick
      permitted_repeats_keep_first;
    Alcotest.test_case "excluded identifier roles" `Quick
      excluded_identifier_roles;
    Alcotest.test_case "statement expression order" `Quick
      statement_expression_order;
    Alcotest.test_case "suppression and initializer events" `Quick
      suppression_and_initializer_events;
    Alcotest.test_case "generated provenance and purity" `Quick
      generated_provenance_and_purity;
    Alcotest.test_case "validation errors" `Quick validation_errors;
  ]
