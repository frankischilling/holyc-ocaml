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

let parse ?(mode = Preprocessor.Jit) session ~path contents =
  let source = Session.add_source session ~path ~contents in
  Holyc_lib.parse_with_config session ~config:(config mode) ~source
  |> expect_ast

type semantic_results = {
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  function_bindings : Semantic_function_collection.t;
  function_types : Semantic_function_type_resolution.t;
}

let resolve session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let function_bindings =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let function_types =
    checked
      (Holyc_lib.resolve_function_types session ~declarations ~aggregates
         ~functions:function_bindings ast)
  in
  { declarations; aggregates; function_bindings; function_types }

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let function_named results name =
  Semantic_function_type_resolution.functions results.function_types
  |> List.find (fun function_ ->
      function_ |> Semantic_function_type_resolution.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let parameters function_ =
  function_ |> Semantic_function_type_resolution.function_signature
  |> Semantic_function_type_resolution.signature_parameters

let parameter_at function_ index = List.nth (parameters function_) index
let resolved_type reference = Semantic_type_reference.resolved_type reference

let check_parameter_mask description expected parameter =
  let actual =
    Semantic_function_type_resolution.parameter_flag_mask parameter
  in
  Alcotest.(check int64) description expected actual;
  Member_flag.all
  |> List.iter (fun flag ->
      Alcotest.(check bool)
        (Printf.sprintf "%s: %s" description (Member_flag.to_source_name flag))
        (Member_flag.is_set ~mask:expected flag)
        (Semantic_function_type_resolution.parameter_has_flag parameter flag))

let check_synthetic_mask description expected binding =
  let actual =
    Semantic_function_type_resolution.synthetic_binding_flag_mask binding
  in
  Alcotest.(check int64) description expected actual;
  Member_flag.all
  |> List.iter (fun flag ->
      Alcotest.(check bool)
        (Printf.sprintf "%s: %s" description (Member_flag.to_source_name flag))
        (Member_flag.is_set ~mask:expected flag)
        (Semantic_function_type_resolution.synthetic_binding_has_flag binding
           flag))

let parameter_type parameter =
  parameter |> Semantic_function_type_resolution.parameter_type_reference
  |> resolved_type

let check_primitive_type ~form ~primitive ~pointer_depth type_ =
  Alcotest.(check int)
    "pointer depth" pointer_depth
    (Semantic_type.pointer_depth type_);
  match Semantic_type.base type_ with
  | Semantic_type.Primitive (actual_form, actual_primitive) ->
      Alcotest.(check string)
        "primitive form"
        (Semantic_type.primitive_form_name form)
        (Semantic_type.primitive_form_name actual_form);
      Alcotest.(check string)
        "primitive"
        (Primitive_type.to_string primitive)
        (Primitive_type.to_string actual_primitive)
  | Semantic_type.Aggregate _ -> Alcotest.fail "expected a primitive type"

let aggregate_target type_ =
  match Semantic_type.base type_ with
  | Semantic_type.Aggregate symbol -> symbol
  | Semantic_type.Primitive _ -> Alcotest.fail "expected an aggregate type"

let primitive_intrinsic_pointers_and_gaps () =
  let session = Session.create () in
  let ast =
    parse session ~path:"function-type-primitives.HC"
      "extern I64i *Kinds(I64 plain,I64,U8 *byte,I16i **storage,F64 ****wide);"
  in
  let function_ =
    resolve session ast |> fun results -> function_named results "Kinds"
  in
  check_primitive_type ~form:Semantic_type.Internal_storage
    ~primitive:Primitive_type.I64 ~pointer_depth:1
    (function_ |> Semantic_function_type_resolution.function_return_type
   |> resolved_type);
  let parameters = parameters function_ in
  Alcotest.(check int) "fixed slot count" 5 (List.length parameters);
  Alcotest.(check (list (option string)))
    "unnamed slot remains present"
    [ Some "plain"; None; Some "byte"; Some "storage"; Some "wide" ]
    (List.map Semantic_function_type_resolution.parameter_name parameters);
  [
    (Semantic_type.Public_spelling, Primitive_type.I64, 0);
    (Semantic_type.Public_spelling, Primitive_type.I64, 0);
    (Semantic_type.Public_spelling, Primitive_type.U8, 1);
    (Semantic_type.Internal_storage, Primitive_type.I16, 2);
    (Semantic_type.Public_spelling, Primitive_type.F64, 4);
  ]
  |> List.iteri (fun index (form, primitive, pointer_depth) ->
      check_primitive_type ~form ~primitive ~pointer_depth
        (List.nth parameters index |> parameter_type));
  Alcotest.(check (list int))
    "named symbols keep original slots" [ 0; 2; 3; 4 ]
    (function_ |> Semantic_function_type_resolution.function_parameter_bindings
    |> List.map Semantic_function_type_resolution.parameter_binding_index)

let identity_with_first_item results item_index =
  Semantic_aggregate_resolution.identities results.aggregates
  |> List.find (fun identity ->
      Semantic_aggregate_resolution.identity_first_item_index identity
      = item_index)
  |> Semantic_aggregate_resolution.identity_symbol

let aggregate_visibility_and_shadowing () =
  let session = Session.create () in
  let ast =
    parse session ~path:"function-type-aggregates.HC"
      "class First {}; extern class Later; extern First *Before(First first, \
       Later *later); class Later {}; class First {}; extern First After(First \
       current);"
  in
  let results = resolve session ast in
  let before = function_named results "Before" in
  let after = function_named results "After" in
  let before_return =
    before |> Semantic_function_type_resolution.function_return_type
    |> resolved_type |> aggregate_target
  in
  let before_first =
    parameter_at before 0 |> parameter_type |> aggregate_target
  in
  let before_later =
    parameter_at before 1 |> parameter_type |> aggregate_target
  in
  let after_return =
    after |> Semantic_function_type_resolution.function_return_type
    |> resolved_type |> aggregate_target
  in
  let after_parameter =
    parameter_at after 0 |> parameter_type |> aggregate_target
  in
  Alcotest.(check int)
    "earlier identity supplies the first return"
    (identity_with_first_item results 0 |> symbol_id)
    (symbol_id before_return);
  Alcotest.(check int)
    "the same earlier identity supplies its parameter" (symbol_id before_return)
    (symbol_id before_first);
  Alcotest.(check int)
    "a forward keeps the identity completed later"
    (identity_with_first_item results 1 |> symbol_id)
    (symbol_id before_later);
  Alcotest.(check int)
    "the later declaration shadows the earlier identity"
    (identity_with_first_item results 4 |> symbol_id)
    (symbol_id after_return);
  Alcotest.(check int)
    "the shadowed identity also supplies the later parameter"
    (symbol_id after_return)
    (symbol_id after_parameter)

let declaration_kinds_and_callback_depths () =
  let session = Session.create () in
  let ast =
    parse session ~path:"function-type-depths.HC"
      "extern U0 Depths(I64 (*one)(),I64 (**two)(),I64 (***three)(),I64 \
       (****four)()); U0 Depths(I64 (*one)(),I64 (**two)(),I64 \
       (***three)(),I64 (****four)()){}"
  in
  let results = resolve session ast in
  let functions =
    Semantic_function_type_resolution.functions results.function_types
  in
  Alcotest.(check int)
    "prototype and definition both resolve" 2 (List.length functions);
  Alcotest.(check (list int))
    "declarations keep module order" [ 0; 1 ]
    (List.map Semantic_function_type_resolution.function_item_index functions);
  Alcotest.(check bool)
    "declarations keep separate scopes" true
    (not
       (Semantic_symbol.Scope_id.equal
          (List.nth functions 0
         |> Semantic_function_type_resolution.function_scope
         |> Semantic_symbol_table.scope_id)
          (List.nth functions 1
         |> Semantic_function_type_resolution.function_scope
         |> Semantic_symbol_table.scope_id)));
  functions
  |> List.iter (fun function_ ->
      Alcotest.(check (list int))
        "callback indirection covers the pinned range" [ 1; 2; 3; 4 ]
        (parameters function_
        |> List.map (fun parameter ->
            match
              Semantic_function_type_resolution.parameter_declarator_kind
                parameter
            with
            | Semantic_function_type_resolution.Function_pointer pointer ->
                pointer
                |> Semantic_function_type_resolution
                   .function_pointer_indirection_origins |> List.length
            | Semantic_function_type_resolution.Object ->
                Alcotest.fail "expected a callback parameter")))

let expect_callback parameter =
  match
    Semantic_function_type_resolution.parameter_declarator_kind parameter
  with
  | Semantic_function_type_resolution.Function_pointer pointer -> pointer
  | Semantic_function_type_resolution.Object ->
      Alcotest.fail "expected a callback parameter"

let default_kind parameter =
  match Semantic_function_type_resolution.parameter_default parameter with
  | None -> "none"
  | Some (Semantic_function_type_resolution.Expression_default _) ->
      "expression"
  | Some (Semantic_function_type_resolution.Lastclass_default _) -> "lastclass"

let recursive_callbacks_defaults_and_varargs () =
  let session = Session.create () in
  let ast =
    parse session ~path:"function-type-callbacks.HC"
      "class Node {}; extern U8 *Complex(I64 first=1,I64 middle,I64 \
       last=lastclass,U0 (**handler)(Node *node,I64=lastclass,U8 \
       *(*nested)(I64 value,...),...),...);"
  in
  let function_ =
    resolve session ast |> fun results -> function_named results "Complex"
  in
  check_primitive_type ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.U8 ~pointer_depth:1
    (function_ |> Semantic_function_type_resolution.function_return_type
   |> resolved_type);
  Alcotest.(check (list string))
    "defaults retain non-trailing positions"
    [ "expression"; "none"; "lastclass"; "none" ]
    (parameters function_ |> List.map default_kind);
  Alcotest.(check (list bool))
    "every fixed parameter keeps its comma before varargs"
    [ true; true; true; true ]
    (parameters function_
    |> List.map (fun parameter ->
        parameter
        |> Semantic_function_type_resolution.parameter_delimiter_origin
        |> Option.is_some));
  let handler = parameter_at function_ 3 |> expect_callback in
  Alcotest.(check int)
    "handler callback indirection" 2
    (handler
   |> Semantic_function_type_resolution.function_pointer_indirection_origins
   |> List.length);
  let handler_signature =
    Semantic_function_type_resolution.function_pointer_signature handler
  in
  let handler_parameters =
    Semantic_function_type_resolution.signature_parameters handler_signature
  in
  Alcotest.(check (list (option string)))
    "callback slots retain names and gaps"
    [ Some "node"; None; Some "nested" ]
    (List.map Semantic_function_type_resolution.parameter_name
       handler_parameters);
  Alcotest.(check string)
    "unnamed callback default" "lastclass"
    (List.nth handler_parameters 1 |> default_kind);
  Alcotest.(check bool)
    "handler keeps its terminal ellipsis" true
    (handler_signature
   |> Semantic_function_type_resolution.signature_variadic_origin
   |> Option.is_some);
  let nested = List.nth handler_parameters 2 |> expect_callback in
  check_primitive_type ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.U8 ~pointer_depth:1
    (List.nth handler_parameters 2 |> parameter_type);
  Alcotest.(check int)
    "nested callback indirection" 1
    (nested
   |> Semantic_function_type_resolution.function_pointer_indirection_origins
   |> List.length);
  Alcotest.(check bool)
    "nested callback keeps its terminal ellipsis" true
    (nested |> Semantic_function_type_resolution.function_pointer_signature
   |> Semantic_function_type_resolution.signature_variadic_origin
   |> Option.is_some);
  let variadic =
    function_ |> Semantic_function_type_resolution.function_variadic_bindings
    |> Option.get
  in
  let argc = Semantic_function_type_resolution.variadic_argc variadic in
  let argv = Semantic_function_type_resolution.variadic_argv variadic in
  Alcotest.(check (list string))
    "synthetic symbol names" [ "argc"; "argv" ]
    [
      argc |> Semantic_function_type_resolution.synthetic_binding_symbol
      |> Semantic_symbol.name;
      argv |> Semantic_function_type_resolution.synthetic_binding_symbol
      |> Semantic_symbol.name;
    ];
  Alcotest.(check (list int))
    "synthetic slots follow fixed arguments" [ 4; 5 ]
    [
      Semantic_function_type_resolution.synthetic_binding_index argc;
      Semantic_function_type_resolution.synthetic_binding_index argv;
    ];
  [ argc; argv ]
  |> List.iter (fun binding ->
      check_primitive_type ~form:Semantic_type.Internal_storage
        ~primitive:Primitive_type.I64 ~pointer_depth:0
        (Semantic_function_type_resolution.synthetic_binding_type binding));
  Alcotest.(check bool)
    "argc is scalar" true
    (Semantic_function_type_resolution.synthetic_binding_shape argc
    = Semantic_function_type_resolution.Scalar);
  match Semantic_function_type_resolution.synthetic_binding_shape argv with
  | Semantic_function_type_resolution.Array
      { source_extent; compiler_placeholder_extent } ->
      Alcotest.(check (option int))
        "argv has no source extent" None source_extent;
      Alcotest.(check int)
        "argv retains the compiler placeholder" 127 compiler_placeholder_extent
  | Semantic_function_type_resolution.Scalar ->
      Alcotest.fail "expected argv to retain an array shape"

let parameter_flags_follow_pinned_assignments () =
  let session = Session.create () in
  let ast =
    parse session ~path:"function-parameter-flags.HC"
      "extern U0 Flags(I64 named,I64,I64 numeric=1,I64 current=lastclass,U8 \
       *mask=\"*\",U0 (*callback)(I64 \
       nested=\"nested\",I64=lastclass,...),...);"
  in
  let function_ =
    resolve session ast |> fun results -> function_named results "Flags"
  in
  Alcotest.(check (list int64))
    "fixed parameters receive exact member-list masks"
    [ 0L; 0x20L; 0x1L; 0x3L; 0x5L; 0x8L ]
    (parameters function_
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  parameters function_
  |> List.iter2
       (fun expected parameter ->
         check_parameter_mask "fixed parameter mask" expected parameter)
       [ 0L; 0x20L; 0x1L; 0x3L; 0x5L; 0x8L ];
  let callback = parameter_at function_ 5 |> expect_callback in
  let callback_parameters =
    callback |> Semantic_function_type_resolution.function_pointer_signature
    |> Semantic_function_type_resolution.signature_parameters
  in
  Alcotest.(check (list int64))
    "nested callback parameters derive their own masks" [ 0x5L; 0x23L ]
    (callback_parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  List.iter2
    (fun expected parameter ->
      check_parameter_mask "nested callback parameter mask" expected parameter)
    [ 0x5L; 0x23L ] callback_parameters;
  check_parameter_mask "outer callback excludes nested defaults" 0x8L
    (parameter_at function_ 5);
  let variadic =
    function_ |> Semantic_function_type_resolution.function_variadic_bindings
    |> Option.get
  in
  check_synthetic_mask "argc variadic mask" 0x10L
    (Semantic_function_type_resolution.variadic_argc variadic);
  check_synthetic_mask "argv variadic mask" 0x10L
    (Semantic_function_type_resolution.variadic_argv variadic)

let string_defaults_are_found_recursively () =
  let session = Session.create () in
  let ast =
    parse session ~path:"recursive-string-defaults.HC"
      "extern U0 Defaults(U8 *direct=\"direct\",U8 \
       *nested=&Factory(,(\"nested\"))(U8 *)[0].field++ + 1,U8 *right=1 + \
       \"right\",U8 *callee=(\"callee\")(),I64 \
       plain=Factory(,1)(I64)[0].field++ + 1);"
  in
  let function_ =
    resolve session ast |> fun results -> function_named results "Defaults"
  in
  Alcotest.(check (list int64))
    "only string-bearing defaults receive owned-string metadata"
    [ 0x5L; 0x5L; 0x5L; 0x5L; 0x1L ]
    (parameters function_
    |> List.map Semantic_function_type_resolution.parameter_flag_mask)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_signature_provenance () =
  let session = Session.create () in
  let ast =
    parse session ~path:"generated-function-type.HC"
      "#define TYPE I64i\n\
       #define STAR *\n\
       #define NAME generated\n\
       #define DEFAULT \"*\"\n\
       extern TYPE STAR Generated(U8 STAR NAME=DEFAULT,...);"
  in
  let function_ =
    resolve session ast |> fun results -> function_named results "Generated"
  in
  let return_reference =
    Semantic_function_type_resolution.function_return_type function_
  in
  let return_origin =
    Semantic_type_reference.spelling_origin return_reference |> source_origin
  in
  let pointer_origin =
    Semantic_type_reference.pointer_origins return_reference
    |> List.hd |> source_origin
  in
  let parameter_origin =
    parameter_at function_ 0
    |> Semantic_function_type_resolution.parameter_name_origin |> Option.get
    |> source_origin
  in
  let parameter = parameter_at function_ 0 in
  check_parameter_mask "generated string default mask" 0x5L parameter;
  let default_origin =
    match Semantic_function_type_resolution.parameter_default parameter with
    | Some
        (Semantic_function_type_resolution.Expression_default
           { expression_origin; _ }) -> source_origin expression_origin
    | None | Some (Semantic_function_type_resolution.Lastclass_default _) ->
        Alcotest.fail "expected a generated expression default"
  in
  [ return_origin; pointer_origin; parameter_origin; default_origin ]
  |> List.iter (fun (provenance : Semantic_symbol.source_origin) ->
      Alcotest.(check bool)
        "generated source keeps its invocation" true
        (Option.is_some provenance.generated_from);
      Alcotest.(check bool)
        "generated source keeps its definition" true
        (Option.is_some provenance.defined_at))

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

let included_signature_provenance () =
  let directory = Filename.temp_dir "holyc-function-types-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "signatures.HC" in
      write_file root_path "#include \"signatures\"";
      write_file include_path
        "class Included {}; extern Included Use(Included value,U8 *mask=\"*\");";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let function_ =
        resolve session ast |> fun results -> function_named results "Use"
      in
      let site =
        function_ |> Semantic_function_type_resolution.function_return_type
        |> Semantic_type_reference.spelling_origin |> source_origin
      in
      let source =
        Source_manager.find (Session.sources session) site.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "the return type keeps its included source" "signatures.HC"
        (Source_file.path source |> Filename.basename);
      let parameter = parameter_at function_ 1 in
      check_parameter_mask "included string default mask" 0x5L parameter;
      let default_site =
        match Semantic_function_type_resolution.parameter_default parameter with
        | Some
            (Semantic_function_type_resolution.Expression_default
               { expression_origin; _ }) -> source_origin expression_origin
        | None | Some (Semantic_function_type_resolution.Lastclass_default _) ->
            Alcotest.fail "expected an included expression default"
      in
      let default_source =
        Source_manager.find (Session.sources session) default_site.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "the default keeps its included source" "signatures.HC"
        (Source_file.path default_source |> Filename.basename))

let type_signature function_ =
  let type_text reference =
    let type_ = resolved_type reference in
    let base =
      match Semantic_type.base type_ with
      | Semantic_type.Primitive (form, primitive) ->
          Printf.sprintf "%s:%s"
            (Semantic_type.primitive_form_name form)
            (Primitive_type.to_string primitive)
      | Semantic_type.Aggregate symbol ->
          Printf.sprintf "aggregate:%s" (Semantic_symbol.name symbol)
    in
    Printf.sprintf "%s:%d" base (Semantic_type.pointer_depth type_)
  in
  let rec parameter_text parameter =
    let kind =
      match
        Semantic_function_type_resolution.parameter_declarator_kind parameter
      with
      | Semantic_function_type_resolution.Object -> "object"
      | Semantic_function_type_resolution.Function_pointer pointer ->
          let nested =
            pointer
            |> Semantic_function_type_resolution.function_pointer_signature
            |> Semantic_function_type_resolution.signature_parameters
            |> List.map parameter_text |> String.concat ","
          in
          Printf.sprintf "callback%d(%s)"
            (pointer
           |> Semantic_function_type_resolution
              .function_pointer_indirection_origins |> List.length)
            nested
    in
    Printf.sprintf "%d:%s:%s:%s:flags=%Ld"
      (Semantic_function_type_resolution.parameter_index parameter)
      (Semantic_function_type_resolution.parameter_name parameter
      |> Option.value ~default:"_")
      (parameter |> Semantic_function_type_resolution.parameter_type_reference
     |> type_text)
      kind
      (Semantic_function_type_resolution.parameter_flag_mask parameter)
  in
  let fixed =
    Printf.sprintf "%s=%s(%s)"
      (function_ |> Semantic_function_type_resolution.function_symbol
     |> Semantic_symbol.name)
      (function_ |> Semantic_function_type_resolution.function_return_type
     |> type_text)
      (parameters function_ |> List.map parameter_text |> String.concat ",")
  in
  match
    Semantic_function_type_resolution.function_variadic_bindings function_
  with
  | None -> fixed
  | Some variadic ->
      let argc = Semantic_function_type_resolution.variadic_argc variadic in
      let argv = Semantic_function_type_resolution.variadic_argv variadic in
      Printf.sprintf "%s;varargs=%Ld,%Ld" fixed
        (Semantic_function_type_resolution.synthetic_binding_flag_mask argc)
        (Semantic_function_type_resolution.synthetic_binding_flag_mask argv)

let modes_determinism_and_purity () =
  let source =
    "class Node {}; extern Node *Walk(Node *node,U8 \
     *mask=\"*\",I64=lastclass,U0 (*visit)(Node *value),...);"
  in
  let summaries =
    [ Preprocessor.Jit; Preprocessor.Aot ]
    |> List.map (fun mode ->
        let session = Session.create () in
        let ast = parse session ~mode ~path:"function-type-modes.HC" source in
        let results = resolve session ast in
        let table = Session.semantic_symbols session in
        let scope_count =
          Semantic_symbol_table.all_scopes table |> List.length
        in
        let symbol_count =
          Semantic_symbol_table.all_symbols table |> List.length
        in
        let summarize resolution =
          Semantic_function_type_resolution.functions resolution
          |> List.map type_signature
        in
        let first = summarize results.function_types in
        let second =
          checked
            (Holyc_lib.resolve_function_types session
               ~declarations:results.declarations ~aggregates:results.aggregates
               ~functions:results.function_bindings ast)
          |> summarize
        in
        Alcotest.(check (list string))
          "repeated resolution is deterministic" first second;
        Alcotest.(check int)
          "resolution creates no scope" scope_count
          (Semantic_symbol_table.all_scopes table |> List.length);
        Alcotest.(check int)
          "resolution creates no symbol" symbol_count
          (Semantic_symbol_table.all_symbols table |> List.length);
        first)
  in
  Alcotest.(check (list string))
    "JIT and AOT signature types agree" (List.nth summaries 0)
    (List.nth summaries 1)

type inputs = {
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  functions : Semantic_function_collection.t;
}

let inputs session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  { ast; declarations; aggregates; functions }

let resolve_inputs session values ast =
  Holyc_lib.resolve_function_types session ~declarations:values.declarations
    ~aggregates:values.aggregates ~functions:values.functions ast

let mismatched_inputs_do_not_mutate () =
  let session = Session.create () in
  let first_ast =
    parse session ~path:"first-function-types.HC"
      "class A {}; extern A First(A value);"
  in
  let second_ast =
    parse session ~path:"second-function-types.HC"
      "class B {}; extern B Second(B value);"
  in
  let first = inputs session first_ast in
  let second = inputs session second_ast in
  let table = Session.semantic_symbols session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check bool)
    "another AST is rejected" true
    (resolve_inputs session first second_ast |> Result.is_error);
  Alcotest.(check bool)
    "another declaration collection is rejected" true
    (Holyc_lib.resolve_function_types session ~declarations:second.declarations
       ~aggregates:first.aggregates ~functions:first.functions first.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "another aggregate reconciliation is rejected" true
    (Holyc_lib.resolve_function_types session ~declarations:first.declarations
       ~aggregates:second.aggregates ~functions:first.functions first.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "another function collection is rejected" true
    (Holyc_lib.resolve_function_types session ~declarations:first.declarations
       ~aggregates:first.aggregates ~functions:second.functions first.ast
    |> Result.is_error);
  Alcotest.(check int)
    "rejected batches preserve scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "rejected batches preserve symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let foreign_session = Session.create () in
  let foreign_ast =
    parse foreign_session ~path:"foreign-function-types.HC"
      "class A {}; extern A First(A value);"
  in
  let foreign = inputs foreign_session foreign_ast in
  Alcotest.(check bool)
    "a foreign session is rejected" true
    (Holyc_lib.resolve_function_types session ~declarations:foreign.declarations
       ~aggregates:foreign.aggregates ~functions:foreign.functions foreign.ast
    |> Result.is_error)

let synthesized description = Semantic_symbol.Synthesized description

let low_level_validation () =
  let table = Semantic_symbol_table.create () in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table
         ~parent:(Semantic_symbol_table.root table)
         ~kind:Semantic_symbol_table.Module ~name:"low-level.HC" ())
  in
  let function_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"Low"
         ~kind:Semantic_symbol.Function ~origin:(synthesized "function"))
  in
  let function_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:module_scope
         ~kind:Semantic_symbol_table.Function ~name:"Low" ())
  in
  let parameter_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:function_scope ~name:"value"
         ~kind:Semantic_symbol.Parameter ~origin:(synthesized "parameter"))
  in
  let i64 =
    checked
      (Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
         ~primitive:Primitive_type.I64 ~pointer_depth:0)
  in
  let reference =
    checked
      (Semantic_type_reference.make ~spelling:"I64"
         ~spelling_origin:(synthesized "type") ~pointer_origins:[]
         ~resolved_type:i64)
  in
  let parameter =
    checked
      (Semantic_function_type_resolution.make_parameter ~index:0
         ~origin:(synthesized "parameter") ~name:"value"
         ~name_origin:(synthesized "parameter") ~type_reference:reference
         ~declarator_kind:Semantic_function_type_resolution.Object ~default:None
         ())
  in
  let signature =
    checked
      (Semantic_function_type_resolution.make_signature
         ~opening_origin:(synthesized "open") ~parameters:[ parameter ]
         ~closing_origin:(synthesized "close") ())
  in
  let binding =
    checked
      (Semantic_function_type_resolution.make_parameter_binding
         ~parameter_index:0 ~symbol:parameter_symbol)
  in
  let function_ =
    checked
      (Semantic_function_type_resolution.make_function ~symbol:function_symbol
         ~scope:function_scope ~item_index:0 ~return_type:reference ~signature
         ~parameter_bindings:[ binding ] ~variadic_bindings:None)
  in
  ignore
    (checked
       (Semantic_function_type_resolution.resolve ~table ~parent:module_scope
          [ function_ ]));
  Alcotest.(check bool)
    "a callback needs indirection" true
    (Semantic_function_type_resolution.make_function_pointer
       ~origin:(synthesized "callback") ~opening_origin:(synthesized "open")
       ~indirection_origins:[] ~closing_origin:(synthesized "close") ~signature
    |> Result.is_error);
  Alcotest.(check bool)
    "signature slots cannot skip" true
    (let gap =
       checked
         (Semantic_function_type_resolution.make_parameter ~index:1
            ~origin:(synthesized "gap") ~type_reference:reference
            ~declarator_kind:Semantic_function_type_resolution.Object
            ~default:None ())
     in
     Semantic_function_type_resolution.make_signature
       ~opening_origin:(synthesized "open") ~parameters:[ gap ]
       ~closing_origin:(synthesized "close") ()
     |> Result.is_error);
  Alcotest.(check bool)
    "a comma is required before another slot" true
    (let second =
       checked
         (Semantic_function_type_resolution.make_parameter ~index:1
            ~origin:(synthesized "second") ~type_reference:reference
            ~declarator_kind:Semantic_function_type_resolution.Object
            ~default:None ())
     in
     Semantic_function_type_resolution.make_signature
       ~opening_origin:(synthesized "open") ~parameters:[ parameter; second ]
       ~closing_origin:(synthesized "close") ()
     |> Result.is_error);
  Alcotest.(check bool)
    "type spelling must match its resolved identity" true
    (Semantic_type_reference.make ~spelling:"U64"
       ~spelling_origin:(synthesized "wrong type") ~pointer_origins:[]
       ~resolved_type:i64
    |> Result.is_error);
  let argc_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:function_scope ~name:"argc"
         ~kind:Semantic_symbol.Parameter ~origin:(synthesized "ellipsis"))
  in
  let internal_i64 =
    checked
      (Semantic_type.make_primitive ~form:Semantic_type.Internal_storage
         ~primitive:Primitive_type.I64 ~pointer_depth:0)
  in
  Alcotest.(check bool)
    "argc cannot use argv's array shape" true
    (Semantic_function_type_resolution.make_synthetic_binding
       Semantic_function_type_resolution.Argc ~symbol:argc_symbol
       ~parameter_index:1 ~resolved_type:internal_i64
       ~shape:
         (Semantic_function_type_resolution.Array
            { source_extent = None; compiler_placeholder_extent = 127 })
    |> Result.is_error);
  let foreign_table = Semantic_symbol_table.create () in
  let foreign_module =
    checked
      (Semantic_symbol_table.create_scope foreign_table
         ~parent:(Semantic_symbol_table.root foreign_table)
         ~kind:Semantic_symbol_table.Module ~name:"foreign.HC" ())
  in
  let foreign_aggregate =
    checked
      (Semantic_symbol_table.add foreign_table ~scope:foreign_module
         ~name:"Foreign" ~kind:Semantic_symbol.Aggregate_type
         ~origin:(synthesized "foreign aggregate"))
  in
  let foreign_type =
    checked
      (Semantic_type.make_aggregate ~symbol:foreign_aggregate ~pointer_depth:0)
  in
  let foreign_reference =
    checked
      (Semantic_type_reference.make ~spelling:"Foreign"
         ~spelling_origin:(synthesized "foreign type")
         ~pointer_origins:[] ~resolved_type:foreign_type)
  in
  let foreign_function =
    checked
      (Semantic_function_type_resolution.make_function ~symbol:function_symbol
         ~scope:function_scope ~item_index:0 ~return_type:foreign_reference
         ~signature ~parameter_bindings:[ binding ] ~variadic_bindings:None)
  in
  Alcotest.(check bool)
    "a foreign aggregate target is rejected" true
    (Semantic_function_type_resolution.resolve ~table ~parent:module_scope
       [ foreign_function ]
    |> Result.is_error);
  Alcotest.(check bool)
    "a task scope cannot host function type facts" true
    (Semantic_function_type_resolution.resolve ~table
       ~parent:(Semantic_symbol_table.root table)
       []
    |> Result.is_error)

let tests =
  [
    Alcotest.test_case "primitive, intrinsic, pointers, and gaps" `Quick
      primitive_intrinsic_pointers_and_gaps;
    Alcotest.test_case "aggregate visibility and shadowing" `Quick
      aggregate_visibility_and_shadowing;
    Alcotest.test_case "declaration kinds and callback depths" `Quick
      declaration_kinds_and_callback_depths;
    Alcotest.test_case "callbacks, defaults, and varargs" `Quick
      recursive_callbacks_defaults_and_varargs;
    Alcotest.test_case "source-derived parameter flags" `Quick
      parameter_flags_follow_pinned_assignments;
    Alcotest.test_case "recursive string-default classification" `Quick
      string_defaults_are_found_recursively;
    Alcotest.test_case "generated signature provenance" `Quick
      generated_signature_provenance;
    Alcotest.test_case "included signature provenance" `Quick
      included_signature_provenance;
    Alcotest.test_case "modes, determinism, and purity" `Quick
      modes_determinism_and_purity;
    Alcotest.test_case "mismatched inputs do not mutate" `Quick
      mismatched_inputs_do_not_mutate;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
  ]
