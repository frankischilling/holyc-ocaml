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

let request_code request =
  Semantic_register_request.effective [ request ]
  |> Semantic_register_request.source_code

let request_codes requests = List.map request_code requests
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

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned_source path =
  [ "third_party/TempleOS"; "../third_party/TempleOS" ]
  |> List.map (fun root -> Filename.concat root path)
  |> List.find_opt Sys.file_exists
  |> function
  | Some source -> read_file source
  | None -> Alcotest.failf "pinned source is unavailable: %s" path

let contains text fragment =
  let fragment_length = String.length fragment in
  let rec search offset =
    if offset + fragment_length > String.length text then false
    else if String.sub text offset fragment_length = fragment then true
    else search (offset + 1)
  in
  fragment_length = 0 || search 0

let pinned_string_default_declarations () =
  let fixtures =
    [
      ( "Kernel/KernelC.HH",
        "public extern I64 Dir(U8 *files_find_mask=\"*\",Bool full=FALSE);",
        "public extern I64 Dir(U8 *files_find_mask=\"*\",Bool full=FALSE);",
        "Dir",
        [ 0x5L; 0x1L ] );
      ( "Adam/ABlkDev/ADskA.HC",
        "public Bool Copy(U8 *src_files_find_mask,U8 \
         *dst_files_find_mask=\".\")",
        "public Bool Copy(U8 *src_files_find_mask,U8 \
         *dst_files_find_mask=\".\"){}",
        "Copy",
        [ 0L; 0x5L ] );
      ( "Adam/ABlkDev/ADskB.HC",
        "public I64 Zip(U8 *files_find_mask=\"*\",U8 *fu_flags=NULL)",
        "public I64 Zip(U8 *files_find_mask=\"*\",U8 *fu_flags=NULL){}",
        "Zip",
        [ 0x5L; 0x1L ] );
    ]
  in
  fixtures
  |> List.iter (fun (path, fragment, source, name, expected) ->
      Alcotest.(check bool)
        (path ^ " retains the audited declaration")
        true
        (contains (pinned_source path) fragment);
      let session = Session.create () in
      let function_ =
        parse session ~path source |> resolve session |> fun results ->
        function_named results name
      in
      Alcotest.(check (list int64))
        (path ^ " parameter masks")
        expected
        (parameters function_
        |> List.map Semantic_function_type_resolution.parameter_flag_mask))

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
       ()
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

let register_constructor_states_and_validation () =
  let origin = synthesized "register request" in
  let register_origin = synthesized "explicit register" in
  let make ?explicit_register ?explicit_register_number
      ?explicit_register_origin kind spelling =
    checked
      (Semantic_register_request.make ~kind
         ~position:Semantic_register_request.Before_type ~spelling ~origin
         ?explicit_register ?explicit_register_number ?explicit_register_origin
         ())
  in
  let allocate = make Semantic_register_request.Allocate "reg" in
  let disable = make Semantic_register_request.Disable "noreg" in
  let explicit =
    make ~explicit_register:"R8" ~explicit_register_number:8
      ~explicit_register_origin:register_origin
      Semantic_register_request.Allocate "reg"
  in
  Alcotest.(check (list int))
    "TempleOS register source codes" [ -128; 33; 32; 8 ]
    ([
       Semantic_register_request.effective [];
       Semantic_register_request.effective [ allocate ];
       Semantic_register_request.effective [ disable ];
       Semantic_register_request.effective [ explicit ];
     ]
    |> List.map Semantic_register_request.source_code);
  Alcotest.(check (list string))
    "selection names"
    [ "unspecified"; "reg"; "noreg"; "R8" ]
    ([
       Semantic_register_request.Unspecified;
       Semantic_register_request.Allocatable;
       Semantic_register_request.Disabled;
       Semantic_register_request.effective [ explicit ];
     ]
    |> List.map Semantic_register_request.selection_name);
  let rejects label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  rejects "the qualifier spelling must match its kind"
    (Semantic_register_request.make ~kind:Semantic_register_request.Allocate
       ~position:Semantic_register_request.Before_type ~spelling:"noreg" ~origin
       ());
  rejects "an explicit spelling needs its number and source location"
    (Semantic_register_request.make ~kind:Semantic_register_request.Allocate
       ~position:Semantic_register_request.Before_type ~spelling:"reg" ~origin
       ~explicit_register:"RAX" ());
  rejects "an explicit number needs its spelling and source location"
    (Semantic_register_request.make ~kind:Semantic_register_request.Allocate
       ~position:Semantic_register_request.Before_type ~spelling:"reg" ~origin
       ~explicit_register_number:0 ());
  rejects "noreg cannot request a physical register"
    (Semantic_register_request.make ~kind:Semantic_register_request.Disable
       ~position:Semantic_register_request.Before_type ~spelling:"noreg" ~origin
       ~explicit_register:"RAX" ~explicit_register_number:0
       ~explicit_register_origin:register_origin ());
  rejects "an empty explicit register is invalid"
    (Semantic_register_request.make ~kind:Semantic_register_request.Allocate
       ~position:Semantic_register_request.Before_type ~spelling:"reg" ~origin
       ~explicit_register:"" ~explicit_register_number:0
       ~explicit_register_origin:register_origin ());
  rejects "a non-U64 register is invalid"
    (Semantic_register_request.make ~kind:Semantic_register_request.Allocate
       ~position:Semantic_register_request.Before_type ~spelling:"reg" ~origin
       ~explicit_register:"EAX" ~explicit_register_number:0
       ~explicit_register_origin:register_origin ());
  rejects "an explicit register number must match its spelling"
    (Semantic_register_request.make ~kind:Semantic_register_request.Allocate
       ~position:Semantic_register_request.Before_type ~spelling:"reg" ~origin
       ~explicit_register:"R15" ~explicit_register_number:14
       ~explicit_register_origin:register_origin ());
  rejects "variadic requests require an ellipsis"
    (Semantic_function_type_resolution.make_signature
       ~opening_origin:(synthesized "opening parenthesis")
       ~parameters:[] ~variadic_register_requests:[ allocate ]
       ~closing_origin:(synthesized "closing parenthesis")
       ())

let fixed_variadic_and_recursive_register_requests () =
  let session = Session.create () in
  let ast =
    parse session ~path:"parameter-register-requests.HC"
      "extern U0 Qualified(I64 plain,reg I64 allocate,noreg U8 stack,\n\
       I64 reg R15 exact,reg RAX U16,reg R14 noreg I32 reg R13 last,\n\
       I64 (*callback)(reg R11 noreg U8 **value));\n\
       extern U0 Varargs(reg R12 noreg ...);"
  in
  let results = resolve session ast in
  let qualified = function_named results "Qualified" in
  let parameters = parameters qualified in
  Alcotest.(check (list (option string)))
    "named and unnamed parameters"
    [
      Some "plain";
      Some "allocate";
      Some "stack";
      Some "exact";
      None;
      Some "last";
      Some "callback";
    ]
    (List.map Semantic_function_type_resolution.parameter_name parameters);
  Alcotest.(check (list (list int)))
    "ordered parameter requests"
    [ []; [ 33 ]; [ 32 ]; [ 15 ]; [ 0 ]; [ 14; 32; 13 ]; [] ]
    (parameters
    |> List.map (fun parameter ->
        parameter
        |> Semantic_function_type_resolution.parameter_register_requests
        |> request_codes));
  Alcotest.(check (list int))
    "the last parameter request wins"
    [ -128; 33; 32; 15; 0; 13; -128 ]
    (parameters
    |> List.map (fun parameter ->
        parameter
        |> Semantic_function_type_resolution.parameter_register_selection
        |> Semantic_register_request.source_code));
  let last_requests =
    List.nth parameters 5
    |> Semantic_function_type_resolution.parameter_register_requests
  in
  Alcotest.(check (list string))
    "before-type and after-type positions survive"
    [ "before-type"; "before-type"; "after-type" ]
    (List.map
       (fun request ->
         request |> Semantic_register_request.position
         |> Semantic_register_request.position_name)
       last_requests);
  let callback_signature =
    match
      List.nth parameters 6
      |> Semantic_function_type_resolution.parameter_declarator_kind
    with
    | Semantic_function_type_resolution.Function_pointer pointer ->
        Semantic_function_type_resolution.function_pointer_signature pointer
    | Semantic_function_type_resolution.Object ->
        Alcotest.fail "expected a recursive callback parameter"
  in
  let callback_parameter =
    callback_signature |> Semantic_function_type_resolution.signature_parameters
    |> List.hd
  in
  Alcotest.(check (list int))
    "recursive callback requests" [ 11; 32 ]
    (callback_parameter
   |> Semantic_function_type_resolution.parameter_register_requests
   |> request_codes);
  Alcotest.(check int)
    "recursive callback final request" 32
    (callback_parameter
   |> Semantic_function_type_resolution.parameter_register_selection
   |> Semantic_register_request.source_code);
  let varargs = function_named results "Varargs" in
  let signature =
    Semantic_function_type_resolution.function_signature varargs
  in
  Alcotest.(check (list int))
    "variadic marker requests" [ 12; 32 ]
    (signature
   |> Semantic_function_type_resolution.signature_variadic_register_requests
   |> request_codes);
  let bindings =
    varargs |> Semantic_function_type_resolution.function_variadic_bindings
    |> Option.get
  in
  let argc = Semantic_function_type_resolution.variadic_argc bindings in
  let argv = Semantic_function_type_resolution.variadic_argv bindings in
  [ ("argc", argc); ("argv", argv) ]
  |> List.iter (fun (name, binding) ->
      Alcotest.(check (list int))
        (name ^ " receives the marker requests")
        [ 12; 32 ]
        (binding
       |> Semantic_function_type_resolution.synthetic_binding_register_requests
       |> request_codes);
      Alcotest.(check int)
        (name ^ " uses the last marker request")
        32
        (binding
       |> Semantic_function_type_resolution.synthetic_binding_register_selection
       |> Semantic_register_request.source_code))

let every_u64_parameter_register () =
  let expected =
    [
      ("RAX", 0);
      ("RCX", 1);
      ("RDX", 2);
      ("RBX", 3);
      ("RSP", 4);
      ("RBP", 5);
      ("RSI", 6);
      ("RDI", 7);
      ("R8", 8);
      ("R9", 9);
      ("R10", 10);
      ("R11", 11);
      ("R12", 12);
      ("R13", 13);
      ("R14", 14);
      ("R15", 15);
    ]
  in
  Alcotest.(check (list (pair string int)))
    "canonical ST_U64_REGS order" expected
    Semantic_register_request.canonical_u64_registers;
  let source_parameters =
    expected
    |> List.mapi (fun index (register, _) ->
        Printf.sprintf "I64 reg %s value%d" register index)
    |> String.concat ","
  in
  let session = Session.create () in
  let ast =
    parse session ~path:"all-parameter-registers.HC"
      ("extern U0 Every(" ^ source_parameters ^ ");")
  in
  let function_ =
    resolve session ast |> fun results -> function_named results "Every"
  in
  let actual =
    parameters function_
    |> List.map (fun parameter ->
        parameter
        |> Semantic_function_type_resolution.parameter_register_selection
        |> function
        | Semantic_register_request.Explicit register ->
            ( Semantic_register_request.explicit_register_spelling register,
              Semantic_register_request.explicit_register_number register )
        | Semantic_register_request.Unspecified
        | Semantic_register_request.Allocatable
        | Semantic_register_request.Disabled ->
            Alcotest.fail "expected an explicit U64 register")
  in
  Alcotest.(check (list (pair string int)))
    "explicit register spellings and numbers" expected actual

let register_request_provenance () =
  let generated_session = Session.create () in
  let generated_ast =
    parse generated_session ~path:"generated-parameter-register.HC"
      "#define BEFORE reg R14\n\
       #define AFTER noreg\n\
       extern U0 Generated(BEFORE I64 AFTER value);"
  in
  let generated_parameter =
    resolve generated_session generated_ast |> fun results ->
    function_named results "Generated" |> fun function_ ->
    parameter_at function_ 0
  in
  let generated_requests =
    Semantic_function_type_resolution.parameter_register_requests
      generated_parameter
  in
  Alcotest.(check (list int))
    "generated requests retain their values" [ 14; 32 ]
    (request_codes generated_requests);
  generated_requests
  |> List.iter (fun request ->
      let provenance =
        request |> Semantic_register_request.origin |> source_origin
      in
      Alcotest.(check bool)
        "generated request keeps its invocation" true
        (Option.is_some provenance.generated_from);
      Alcotest.(check bool)
        "generated request keeps its definition" true
        (Option.is_some provenance.defined_at);
      request |> Semantic_register_request.explicit_register
      |> Option.iter (fun register ->
          let register_provenance =
            register |> Semantic_register_request.explicit_register_origin
            |> source_origin
          in
          Alcotest.(check bool)
            "generated explicit register keeps its invocation" true
            (Option.is_some register_provenance.generated_from);
          Alcotest.(check bool)
            "generated explicit register keeps its definition" true
            (Option.is_some register_provenance.defined_at)));
  let directory = Filename.temp_dir "holyc-parameter-register-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "qualified.HC" in
      write_file root_path "#include \"qualified\"";
      write_file include_path "extern U0 Included(reg R13 I64 value);";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let request =
        resolve session ast |> fun results ->
        function_named results "Included" |> fun function_ ->
        parameter_at function_ 0
        |> Semantic_function_type_resolution.parameter_register_requests
        |> List.hd
      in
      Alcotest.(check int) "included request value" 13 (request_code request);
      let site = request |> Semantic_register_request.origin |> source_origin in
      let request_source =
        Source_manager.find (Session.sources session) site.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included request keeps its source" "qualified.HC"
        (Source_file.path request_source |> Filename.basename);
      let register_source =
        request |> Semantic_register_request.explicit_register |> Option.get
        |> Semantic_register_request.explicit_register_origin |> source_origin
        |> fun origin ->
        Source_manager.find (Session.sources session) origin.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included explicit register keeps its source" "qualified.HC"
        (Source_file.path register_source |> Filename.basename))

let register_requests_are_deterministic_but_not_header_identity () =
  let session = Session.create () in
  let ast =
    parse session ~path:"parameter-register-headers.HC"
      "extern U0 Same(reg R15 I64 value);\nextern U0 Same(noreg I64 value);"
  in
  let values = inputs session ast in
  let table = Session.semantic_symbols session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let resolve_once () = checked (resolve_inputs session values ast) in
  let summarize resolution =
    Semantic_function_type_resolution.functions resolution
    |> List.map (fun function_ ->
        function_ |> parameters |> List.hd
        |> Semantic_function_type_resolution.parameter_register_selection
        |> Semantic_register_request.source_code)
  in
  let first = resolve_once () in
  let second = resolve_once () in
  Alcotest.(check (list int))
    "replayed request values" [ 15; 32 ] (summarize first);
  Alcotest.(check (list int))
    "replay is deterministic" (summarize first) (summarize second);
  Alcotest.(check int)
    "resolution preserves scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "resolution preserves symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let identities =
    checked
      (Holyc_lib.resolve_function_identities session
         ~declarations:values.declarations ~functions:first
         ~compilation_mode:Preprocessor.Jit ast)
  in
  Alcotest.(check int)
    "register requests do not split a function identity" 1
    (Semantic_function_resolution.identities identities |> List.length);
  Alcotest.(check int)
    "both headers remain visible to reconciliation" 2
    (Semantic_function_resolution.declarations identities |> List.length)

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
    Alcotest.test_case "pinned string-default declarations" `Quick
      pinned_string_default_declarations;
    Alcotest.test_case "generated signature provenance" `Quick
      generated_signature_provenance;
    Alcotest.test_case "included signature provenance" `Quick
      included_signature_provenance;
    Alcotest.test_case "modes, determinism, and purity" `Quick
      modes_determinism_and_purity;
    Alcotest.test_case "mismatched inputs do not mutate" `Quick
      mismatched_inputs_do_not_mutate;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
    Alcotest.test_case "register constructor states and validation" `Quick
      register_constructor_states_and_validation;
    Alcotest.test_case "fixed, variadic, and recursive register requests" `Quick
      fixed_variadic_and_recursive_register_requests;
    Alcotest.test_case "all explicit U64 parameter registers" `Quick
      every_u64_parameter_register;
    Alcotest.test_case "register request provenance" `Quick
      register_request_provenance;
    Alcotest.test_case "register request replay and header identity" `Quick
      register_requests_are_deterministic_but_not_header_identity;
  ]
