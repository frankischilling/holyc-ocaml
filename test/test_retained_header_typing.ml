open Holyc_lib
module C = Semantic_declaration_collection
module R = Semantic_compiler_record
module FC = Semantic_function_collection
module FT = Semantic_function_type_resolution
module Driver_collection = Holyc_lib__Driver__Function_collection
module Driver_types = Holyc_lib__Driver__Function_type_resolution

let checked = Test_declaration_collection.checked

let origin (identifier : Ast.identifier) =
  let location = identifier.location in
  Semantic_symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type fixture = {
  table : Semantic_symbol_table.t;
  header : Parser.completed_function_header;
  collected_header : FC.collected_function;
  typed_header : FT.resolved_function;
  ast : Ast.module_;
  declarations : C.t;
  aggregates : Semantic_aggregate_resolution.t;
}

let one_definition (module_ : Ast.module_) =
  match module_.items with
  | [ Ast.Function_definition definition ] -> definition
  | _ -> Alcotest.fail "expected one function definition"

let declaration_view namespace publication (ast : Ast.module_) =
  let name, declaration_kind =
    match ast.items with
    | [ Ast.Function_definition definition ] ->
        (definition.name, C.Function_definition)
    | [ Ast.Function_prototype prototype ] ->
        (prototype.name, C.Function_prototype)
    | _ -> Alcotest.fail "expected one function declaration"
  in
  let declaration =
    C.make_declaration ~name:name.spelling ~declaration_kind
      ~origin:(origin name) ~item_index:0 ()
    |> checked
  in
  C.view namespace [ (publication, declaration) ] |> checked

let parse_fixture text =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let source =
    Session.add_source session ~path:"retained-header-typing.HC" ~contents:text
  in
  let publication = ref None in
  let retained = ref None in
  let header = ref None in
  let commands : Parser.command_sink =
    {
      checkpoint = None;
      reference = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      declaration =
        Some
          (fun event ->
            (match event with
            | Parser.Function_declared source ->
                publication :=
                  Some (C.publish_function namespace source |> checked)
            | Parser.Function_header_completed source ->
                let declaration =
                  R.declare_function ~table ~namespace (Option.get !publication)
                    source
                  |> checked
                in
                header := Some source;
                retained :=
                  Some
                    (Driver_types.resolve_completed_header_with_collection
                       ~table ~namespace declaration
                    |> checked)
            | _ -> ());
            Ok ());
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create () |> checked)
      source
  in
  let ast = Option.get parsed.ast in
  let publication = Option.get !publication in
  let collected_header, typed_header = Option.get !retained in
  let declarations = declaration_view namespace publication ast in
  let aggregates =
    Semantic_aggregate_resolution.resolve ~table
      ~parent:(C.namespace_scope namespace)
      []
    |> checked
  in
  {
    table;
    header = Option.get !header;
    collected_header;
    typed_header;
    ast;
    declarations;
    aggregates;
  }

let function_only collection = FC.functions collection |> List.hd
let typed_only resolution = FT.functions resolution |> List.hd

let parameter_entries function_ =
  FC.function_entries function_
  |> List.filter (fun entry -> Option.is_some (FC.entry_parameter_index entry))

let local_names function_ =
  FC.function_entries function_
  |> List.filter_map (fun entry ->
      match FC.entry_local_declaration_index entry with
      | None -> None
      | Some _ -> Some (FC.entry_symbol entry |> Semantic_symbol.name))

let retains_header_while_adding_locals () =
  let fixture =
    parse_fixture
      "I64 F(I64 n=40,U0 (**handler)(U8 *node,I64=lastclass,U8 *(*nested)(I64 \
       value,...),...),...){I64 local=n;static I64 saved=0;return n;}"
  in
  let header_parameters = parameter_entries fixture.collected_header in
  let collection =
    Driver_collection.collect
      ~retained_headers:[ fixture.collected_header ]
      ~table:fixture.table ~declarations:fixture.declarations fixture.ast
    |> checked
  in
  let completed = function_only collection in
  Alcotest.(check bool)
    "completion keeps the exact parameter scope" true
    (FC.function_scope completed == FC.function_scope fixture.collected_header);
  List.iter2
    (fun expected actual ->
      Alcotest.(check bool)
        "completion keeps the exact parameter symbol" true
        (FC.entry_symbol expected == FC.entry_symbol actual))
    header_parameters
    (parameter_entries completed);
  Alcotest.(check (list string))
    "body locals append in source order" [ "local"; "saved" ]
    (local_names completed);
  let resolution =
    Driver_types.resolve ~retained_headers:[ fixture.typed_header ]
      ~table:fixture.table ~declarations:fixture.declarations
      ~aggregates:fixture.aggregates ~functions:collection fixture.ast
    |> checked
  in
  let completed_type = typed_only resolution in
  Alcotest.(check bool)
    "completion keeps the exact resolved function" true
    (completed_type == fixture.typed_header);
  Alcotest.(check bool)
    "completion keeps the exact recursive signature" true
    (FT.function_signature completed_type
    == FT.function_signature fixture.typed_header);
  List.iter2
    (fun source parameter ->
      Alcotest.(check bool)
        "typed parameters keep the completed-header AST child" true
        (Option.get (FT.parameter_source parameter) == source))
    fixture.header.parameters
    (FT.function_signature completed_type |> FT.signature_parameters);
  Alcotest.(check bool)
    "variadic bindings remain the retained object" true
    (FT.function_variadic_bindings completed_type
    == FT.function_variadic_bindings fixture.typed_header)

let reconstructed_source_rejects_before_allocation () =
  let fixture = parse_fixture "I64 F(I64 n,...){I64 local=n;return n;}" in
  let definition = one_definition fixture.ast in
  let parameters =
    match definition.parameters with
    | first :: rest ->
        Ast.make_function_parameter
          ~register_qualifiers:first.register_qualifiers
          ~type_specifier:first.type_specifier
          ~pointer_layers:first.pointer_layers ~name:first.name
          ~function_pointer:first.function_pointer ~default:first.default
          ~delimiter:first.delimiter ~location:first.location
        :: rest
    | [] -> Alcotest.fail "expected a named parameter"
  in
  let reconstructed_definition =
    Ast.make_function_definition ~modifiers:definition.modifiers
      ~return_type:definition.return_type
      ~return_pointer_layers:definition.return_pointer_layers
      ~name:definition.name ~opening_parenthesis:definition.opening_parenthesis
      ~parameters ~empty_parameter_entries:definition.empty_parameter_entries
      ~variadic:definition.variadic
      ~closing_parenthesis:definition.closing_parenthesis ~body:definition.body
      ~location:definition.location
  in
  let reconstructed =
    Ast.make_module ~source:fixture.ast.source ~span:fixture.ast.span
      ~items:[ Ast.Function_definition reconstructed_definition ]
  in
  let scopes = Semantic_symbol_table.all_scopes fixture.table |> List.length in
  let symbols =
    Semantic_symbol_table.all_symbols fixture.table |> List.length
  in
  Alcotest.(check bool)
    "a copied parameter AST cannot consume a retained collection" true
    (Driver_collection.collect
       ~retained_headers:[ fixture.collected_header ]
       ~table:fixture.table ~declarations:fixture.declarations reconstructed
    |> Result.is_error);
  Alcotest.(check int)
    "copied source allocates no function scope" scopes
    (Semantic_symbol_table.all_scopes fixture.table |> List.length);
  Alcotest.(check int)
    "copied source allocates no local symbols" symbols
    (Semantic_symbol_table.all_symbols fixture.table |> List.length);
  ignore
    (Driver_collection.collect
       ~retained_headers:[ fixture.collected_header ]
       ~table:fixture.table ~declarations:fixture.declarations fixture.ast
    |> checked)

let repeated_and_foreign_reuse_reject () =
  let fixture = parse_fixture "I64 F(I64 n){I64 local=n;return n;}" in
  let collection =
    Driver_collection.collect
      ~retained_headers:[ fixture.collected_header ]
      ~table:fixture.table ~declarations:fixture.declarations fixture.ast
    |> checked
  in
  let resolution =
    Driver_types.resolve ~retained_headers:[ fixture.typed_header ]
      ~table:fixture.table ~declarations:fixture.declarations
      ~aggregates:fixture.aggregates ~functions:collection fixture.ast
    |> checked
  in
  Alcotest.(check bool)
    "a retained collection completes only once" true
    (Driver_collection.collect
       ~retained_headers:[ fixture.collected_header ]
       ~table:fixture.table ~declarations:fixture.declarations fixture.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "a retained type completes only once" true
    (Driver_types.resolve ~retained_headers:[ fixture.typed_header ]
       ~table:fixture.table ~declarations:fixture.declarations
       ~aggregates:fixture.aggregates ~functions:collection fixture.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "first type completion returned its retained object" true
    (typed_only resolution == fixture.typed_header);
  let foreign = parse_fixture "I64 F(I64 n){I64 other=n;return n;}" in
  let scopes = Semantic_symbol_table.all_scopes fixture.table |> List.length in
  let symbols =
    Semantic_symbol_table.all_symbols fixture.table |> List.length
  in
  Alcotest.(check bool)
    "a foreign retained collection is rejected" true
    (Driver_collection.collect
       ~retained_headers:[ foreign.collected_header ]
       ~table:fixture.table ~declarations:fixture.declarations fixture.ast
    |> Result.is_error);
  Alcotest.(check int)
    "foreign rejection allocates no scope" scopes
    (Semantic_symbol_table.all_scopes fixture.table |> List.length);
  Alcotest.(check int)
    "foreign rejection allocates no symbol" symbols
    (Semantic_symbol_table.all_symbols fixture.table |> List.length)

let rejects_header_substitution fixture replacement =
  let reconstructed =
    Ast.make_module ~source:fixture.ast.source ~span:fixture.ast.span
      ~items:[ replacement ]
  in
  let scopes = Semantic_symbol_table.all_scopes fixture.table |> List.length in
  let symbols =
    Semantic_symbol_table.all_symbols fixture.table |> List.length
  in
  Alcotest.(check bool)
    "substituted header is rejected before collection" true
    (Driver_collection.collect
       ~retained_headers:[ fixture.collected_header ]
       ~table:fixture.table ~declarations:fixture.declarations reconstructed
    |> Result.is_error);
  Alcotest.(check int)
    "rejected header allocates no scope" scopes
    (Semantic_symbol_table.all_scopes fixture.table |> List.length);
  Alcotest.(check int)
    "rejected header allocates no local symbols" symbols
    (Semantic_symbol_table.all_symbols fixture.table |> List.length);
  let functions =
    Driver_collection.collect
      ~retained_headers:[ fixture.collected_header ]
      ~table:fixture.table ~declarations:fixture.declarations fixture.ast
    |> checked
  in
  Alcotest.(check bool)
    "substituted header is independently rejected by typing" true
    (Driver_types.resolve ~retained_headers:[ fixture.typed_header ]
       ~table:fixture.table ~declarations:fixture.declarations
       ~aggregates:fixture.aggregates ~functions reconstructed
    |> Result.is_error);
  let typed =
    Driver_types.resolve ~retained_headers:[ fixture.typed_header ]
      ~table:fixture.table ~declarations:fixture.declarations
      ~aggregates:fixture.aggregates ~functions fixture.ast
    |> checked |> typed_only
  in
  Alcotest.(check bool)
    "original header remains reusable after rejection" true
    (typed == fixture.typed_header)

let modifier_substitutions_reject_before_allocation () =
  List.iter
    (fun copy ->
      let fixture =
        parse_fixture "public I64 F(I64 n){I64 local=n;return n;}"
      in
      let definition = one_definition fixture.ast in
      let modifiers =
        if copy then
          List.map
            (fun (modifier : Ast.declaration_modifier) ->
              Ast.make_declaration_modifier ~kind:modifier.kind
                ~spelling:modifier.spelling ~location:modifier.location)
            definition.modifiers
        else []
      in
      let replacement =
        Ast.make_function_definition ~modifiers
          ~return_type:definition.return_type
          ~return_pointer_layers:definition.return_pointer_layers
          ~name:definition.name
          ~opening_parenthesis:definition.opening_parenthesis
          ~parameters:definition.parameters
          ~empty_parameter_entries:definition.empty_parameter_entries
          ~variadic:definition.variadic
          ~closing_parenthesis:definition.closing_parenthesis
          ~body:definition.body ~location:definition.location
      in
      rejects_header_substitution fixture (Ast.Function_definition replacement))
    [ false; true ]

let binding_substitutions_reject_before_allocation () =
  List.iter
    (fun copy ->
      let fixture = parse_fixture "_extern _REMOTE I64 F(I64 n);" in
      let prototype =
        match fixture.ast.items with
        | [ Ast.Function_prototype prototype ] -> prototype
        | _ -> Alcotest.fail "expected one function prototype"
      in
      let original = prototype.binding in
      let binding =
        Ast.make_declaration_binding ~kind:original.kind
          ~spelling:original.spelling ~location:original.location
          ~target:
            (if copy then original.target
             else Ast.Symbol_binding_target prototype.name)
      in
      let replacement =
        Ast.make_function_prototype ~modifiers:prototype.modifiers ~binding
          ~return_type:prototype.return_type
          ~return_pointer_layers:prototype.return_pointer_layers
          ~name:prototype.name
          ~opening_parenthesis:prototype.opening_parenthesis
          ~parameters:prototype.parameters
          ~empty_parameter_entries:prototype.empty_parameter_entries
          ~variadic:prototype.variadic
          ~closing_parenthesis:prototype.closing_parenthesis
          ~semicolon:prototype.semicolon ~location:prototype.location
      in
      rejects_header_substitution fixture (Ast.Function_prototype replacement))
    [ false; true ]

let empty_parameter_substitutions_reject_before_allocation () =
  List.iter
    (fun text ->
      List.iter
        (fun copy ->
          let fixture = parse_fixture text in
          let entries = fixture.header.empty_parameter_entries in
          Alcotest.(check bool)
            "fixture retains empty parameter entries" true (entries <> []);
          let empty_parameter_entries =
            if copy then
              List.map
                (fun (entry : Ast.empty_parameter_entry) ->
                  Ast.make_empty_parameter_entry
                    ~preceding_parameter_count:entry.preceding_parameter_count
                    ~delimiter:entry.empty_parameter_delimiter)
                entries
            else []
          in
          let replacement =
            match fixture.ast.items with
            | [ Ast.Function_definition definition ] ->
                Ast.Function_definition
                  (Ast.make_function_definition ~modifiers:definition.modifiers
                     ~return_type:definition.return_type
                     ~return_pointer_layers:definition.return_pointer_layers
                     ~name:definition.name
                     ~opening_parenthesis:definition.opening_parenthesis
                     ~parameters:definition.parameters ~empty_parameter_entries
                     ~variadic:definition.variadic
                     ~closing_parenthesis:definition.closing_parenthesis
                     ~body:definition.body ~location:definition.location)
            | [ Ast.Function_prototype prototype ] ->
                Ast.Function_prototype
                  (Ast.make_function_prototype ~modifiers:prototype.modifiers
                     ~binding:prototype.binding
                     ~return_type:prototype.return_type
                     ~return_pointer_layers:prototype.return_pointer_layers
                     ~name:prototype.name
                     ~opening_parenthesis:prototype.opening_parenthesis
                     ~parameters:prototype.parameters ~empty_parameter_entries
                     ~variadic:prototype.variadic
                     ~closing_parenthesis:prototype.closing_parenthesis
                     ~semicolon:prototype.semicolon ~location:prototype.location)
            | _ -> Alcotest.fail "expected one function declaration"
          in
          rejects_header_substitution fixture replacement)
        [ false; true ])
    [ "I64 F(;;I64 n){I64 local=n;return n;}"; "extern I64 F(;;I64 n);" ]

let replace_closing fixture closing_parenthesis =
  match fixture.ast.items with
  | [ Ast.Function_definition definition ] ->
      Ast.Function_definition
        (Ast.make_function_definition ~modifiers:definition.modifiers
           ~return_type:definition.return_type
           ~return_pointer_layers:definition.return_pointer_layers
           ~name:definition.name
           ~opening_parenthesis:definition.opening_parenthesis
           ~parameters:definition.parameters
           ~empty_parameter_entries:definition.empty_parameter_entries
           ~variadic:definition.variadic ~closing_parenthesis
           ~body:definition.body ~location:definition.location)
  | [ Ast.Function_prototype prototype ] ->
      Ast.Function_prototype
        (Ast.make_function_prototype ~modifiers:prototype.modifiers
           ~binding:prototype.binding ~return_type:prototype.return_type
           ~return_pointer_layers:prototype.return_pointer_layers
           ~name:prototype.name
           ~opening_parenthesis:prototype.opening_parenthesis
           ~parameters:prototype.parameters
           ~empty_parameter_entries:prototype.empty_parameter_entries
           ~variadic:prototype.variadic ~closing_parenthesis
           ~semicolon:prototype.semicolon ~location:prototype.location)
  | _ -> Alcotest.fail "expected one function header"

let closing_substitutions () =
  List.iter
    (fun text ->
      List.iter
        (fun copy ->
          let fixture = parse_fixture text in
          let closing =
            match fixture.header.closing_parenthesis with
            | None ->
                Some
                  fixture.header.function_publication
                    .function_opening_parenthesis
            | Some location ->
                if copy then
                  Some
                    (Ast.make_location ?generated_from:location.generated_from
                       ?defined_at:location.defined_at ~span:location.span
                       ~source_segments:location.source_segments ())
                else None
          in
          rejects_header_substitution fixture (replace_closing fixture closing))
        [ false; true ])
    [
      "I64 F(I64 n,...){I64 local=n;return n;}";
      "I64 F(I64 n,...{I64 local=n;return n;}";
      "extern I64 F(I64 n,...);";
      "extern I64 F(I64 n,...;";
    ]

let original_closing_rewrapped () =
  List.iter
    (fun text ->
      let fixture = parse_fixture text in
      let closing =
        Option.map (fun original -> original) fixture.header.closing_parenthesis
      in
      let ast =
        Ast.make_module ~source:fixture.ast.source ~span:fixture.ast.span
          ~items:[ replace_closing fixture closing ]
      in
      let functions =
        Driver_collection.collect
          ~retained_headers:[ fixture.collected_header ]
          ~table:fixture.table ~declarations:fixture.declarations ast
        |> checked
      in
      let typed =
        Driver_types.resolve ~retained_headers:[ fixture.typed_header ]
          ~table:fixture.table ~declarations:fixture.declarations
          ~aggregates:fixture.aggregates ~functions ast
        |> checked |> typed_only
      in
      Alcotest.(check bool)
        "original close or absence retains the typed header" true
        (typed == fixture.typed_header))
    [
      "I64 F(I64 n,...){return n;}";
      "extern I64 F(I64 n,...);";
      "I64 F(I64 n,...{return n;}";
      "extern I64 F(I64 n,...;";
    ]

let tests =
  [
    Alcotest.test_case "retained header gains locals once" `Quick
      retains_header_while_adding_locals;
    Alcotest.test_case "copied source rejects before allocation" `Quick
      reconstructed_source_rejects_before_allocation;
    Alcotest.test_case "repeated and foreign reuse reject" `Quick
      repeated_and_foreign_reuse_reject;
    Alcotest.test_case "modifier substitutions reject before allocation" `Quick
      modifier_substitutions_reject_before_allocation;
    Alcotest.test_case "binding substitutions reject before allocation" `Quick
      binding_substitutions_reject_before_allocation;
    Alcotest.test_case "empty parameter substitutions reject before allocation"
      `Quick empty_parameter_substitutions_reject_before_allocation;
    Alcotest.test_case "closing token substitutions reject before allocation"
      `Quick closing_substitutions;
    Alcotest.test_case
      "closing option wrapper does not replace original evidence" `Quick
      original_closing_rewrapped;
  ]
