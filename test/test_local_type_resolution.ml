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
  session : Session.t;
  aggregates : Semantic_aggregate_resolution.t;
  local_types : Semantic_local_type_resolution.t;
  symbol_count_before : int;
  symbol_count_after : int;
}

let resolve session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let table = Session.semantic_symbols session in
  let symbol_count_before =
    Semantic_symbol_table.all_symbols table |> List.length
  in
  let local_types =
    checked
      (Holyc_lib.resolve_local_types session ~declarations ~aggregates
         ~functions ast)
  in
  let symbol_count_after =
    Semantic_symbol_table.all_symbols table |> List.length
  in
  { session; aggregates; local_types; symbol_count_before; symbol_count_after }

let prepare ?mode ~path contents =
  let session = Session.create () in
  let ast = parse ?mode session ~path contents in
  resolve session ast

let function_named results name =
  Semantic_local_type_resolution.functions results.local_types
  |> List.find (fun function_ ->
      function_ |> Semantic_local_type_resolution.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let locals function_ = Semantic_local_type_resolution.function_locals function_

let local_named function_ name =
  locals function_
  |> List.find (fun local ->
      local |> Semantic_local_type_resolution.local_symbol
      |> Semantic_symbol.name |> String.equal name)

let check_local_mask description expected local =
  Alcotest.(check int64)
    description expected
    (Semantic_local_type_resolution.local_flag_mask local);
  Member_flag.all
  |> List.iter (fun flag ->
      Alcotest.(check bool)
        (Printf.sprintf "%s: %s" description (Member_flag.to_source_name flag))
        (Member_flag.is_set ~mask:expected flag)
        (Semantic_local_type_resolution.local_has_flag local flag))

let resolved_type local =
  local |> Semantic_local_type_resolution.local_type_reference
  |> Semantic_type_reference.resolved_type

let check_primitive ~form ~primitive ~pointer_depth local =
  let type_ = resolved_type local in
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

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let source_text results site =
  let site = source_origin site in
  let source =
    Source_manager.find (Session.sources results.session) site.span.source
    |> Option.get
  in
  String.sub
    (Source_file.contents source)
    site.span.start (Span.length site.span)

let declaration_shapes () =
  let results =
    prepare ~path:"local-type-shapes.HC"
      "U0 Shapes(){\n\
       I64 reg R15 first,*second;\n\
       I64 noreg disabled;\n\
       U8 bytes[],matrix[2][0];\n\
       static I64i stored=3,values={1,{2}};\n\
       }"
  in
  let function_ = function_named results "Shapes" in
  Alcotest.(check (list string))
    "local source order"
    [ "first"; "second"; "disabled"; "bytes"; "matrix"; "stored"; "values" ]
    (locals function_
    |> List.map (fun local ->
        local |> Semantic_local_type_resolution.local_symbol
        |> Semantic_symbol.name));
  Alcotest.(check (list int))
    "declaration order" [ 0; 0; 1; 2; 2; 3; 3 ]
    (locals function_
    |> List.map Semantic_local_type_resolution.local_declaration_index);
  Alcotest.(check (list int))
    "declarator order" [ 0; 1; 0; 0; 1; 0; 1 ]
    (locals function_
    |> List.map Semantic_local_type_resolution.local_declarator_index);
  List.iter2
    (fun local expected ->
      let name =
        local |> Semantic_local_type_resolution.local_symbol
        |> Semantic_symbol.name
      in
      check_local_mask (name ^ " member-list mask") expected local)
    (locals function_)
    [ 0L; 0L; 0L; 0L; 0L; 0x40L; 0x40L ];
  Alcotest.(check (list string))
    "declaration delimiters"
    [
      "comma";
      "semicolon";
      "semicolon";
      "comma";
      "semicolon";
      "comma";
      "semicolon";
    ]
    (locals function_
    |> List.map (fun local ->
        local |> Semantic_local_type_resolution.local_delimiter
        |> Semantic_local_type_resolution.delimiter_kind
        |> Semantic_local_type_resolution.delimiter_kind_name));
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.I64 ~pointer_depth:0
    (local_named function_ "first");
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.I64 ~pointer_depth:1
    (local_named function_ "second");
  check_primitive ~form:Semantic_type.Internal_storage
    ~primitive:Primitive_type.I64 ~pointer_depth:0
    (local_named function_ "stored");
  let first_request =
    local_named function_ "first"
    |> Semantic_local_type_resolution.local_register_requests |> List.hd
  in
  Alcotest.(check string)
    "explicit register request" "R15"
    (first_request
   |> Semantic_local_type_resolution.register_request_explicit_register
   |> Option.get);
  Alcotest.(check (option int))
    "explicit register number" (Some 15)
    (Semantic_local_type_resolution.register_request_explicit_register_number
       first_request);
  Alcotest.(check string)
    "register allocation request" "reg"
    (first_request |> Semantic_local_type_resolution.register_request_kind
   |> Semantic_local_type_resolution.register_request_kind_name);
  Alcotest.(check string)
    "register request position" "after-type"
    (first_request |> Semantic_local_type_resolution.register_request_position
   |> Semantic_local_type_resolution.register_position_name);
  let disabled_request =
    local_named function_ "disabled"
    |> Semantic_local_type_resolution.local_register_requests |> List.hd
  in
  Alcotest.(check string)
    "register allocation disabled" "noreg"
    (disabled_request |> Semantic_local_type_resolution.register_request_kind
   |> Semantic_local_type_resolution.register_request_kind_name);
  Alcotest.(check (option string))
    "noreg has no explicit register" None
    (Semantic_local_type_resolution.register_request_explicit_register
       disabled_request);
  let matrix_dimensions =
    local_named function_ "matrix"
    |> Semantic_local_type_resolution.local_array_dimensions
  in
  Alcotest.(check (list int))
    "matrix dimensions" [ 0; 1 ]
    (List.map Semantic_local_type_resolution.array_dimension_index
       matrix_dimensions);
  Alcotest.(check (list string))
    "matrix expressions" [ "2"; "0" ]
    (matrix_dimensions
    |> List.map (fun dimension ->
        dimension
        |> Semantic_local_type_resolution.array_dimension_expression_origin
        |> Option.get |> source_text results));
  let bytes_dimension =
    local_named function_ "bytes"
    |> Semantic_local_type_resolution.local_array_dimensions |> List.hd
  in
  Alcotest.(check bool)
    "the first array dimension can be empty" true
    (bytes_dimension
   |> Semantic_local_type_resolution.array_dimension_expression_origin
   |> Option.is_none);
  Alcotest.(check (list string))
    "static initializer shapes" [ "scalar"; "braced" ]
    ([ "stored"; "values" ]
    |> List.map (fun name ->
        local_named function_ name
        |> Semantic_local_type_resolution.local_initializer |> Option.get
        |> Semantic_local_type_resolution.initializer_kind
        |> Semantic_local_type_resolution.initializer_kind_name));
  [ "stored"; "values" ]
  |> List.iter (fun name ->
      let local = local_named function_ name in
      Alcotest.(check string)
        (name ^ " storage") "static"
        (local |> Semantic_local_type_resolution.local_storage
       |> Semantic_local_type_resolution.storage_name);
      Alcotest.(check int)
        (name ^ " static provenance")
        1
        (local |> Semantic_local_type_resolution.local_storage_origins
       |> List.length));
  Alcotest.(check int)
    "resolution does not add symbols" results.symbol_count_before
    results.symbol_count_after

let identity_at results item_index =
  Semantic_aggregate_resolution.identities results.aggregates
  |> List.find (fun identity ->
      Semantic_aggregate_resolution.identity_first_item_index identity
      = item_index)
  |> Semantic_aggregate_resolution.identity_symbol

let aggregate_target local =
  match Semantic_type.base (resolved_type local) with
  | Semantic_type.Aggregate symbol -> symbol
  | Semantic_type.Primitive _ -> Alcotest.fail "expected an aggregate type"

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let aggregate_visibility () =
  let results =
    prepare ~path:"local-type-aggregates.HC"
      "class Node {};\n\
       U0 Before(){Node value,*pointer;}\n\
       class Node {};\n\
       U0 After(){Node newest;}"
  in
  let before = function_named results "Before" in
  let after = function_named results "After" in
  let first_identity = identity_at results 0 in
  let second_identity = identity_at results 2 in
  [ "value"; "pointer" ]
  |> List.iter (fun name ->
      Alcotest.(check int)
        (name ^ " uses the earlier identity")
        (symbol_id first_identity)
        (local_named before name |> aggregate_target |> symbol_id));
  Alcotest.(check int)
    "the later function uses the shadowing identity"
    (symbol_id second_identity)
    (local_named after "newest" |> aggregate_target |> symbol_id);
  Alcotest.(check int)
    "aggregate pointer depth" 1
    (local_named before "pointer"
    |> resolved_type |> Semantic_type.pointer_depth)

let function_wide_traversal () =
  let results =
    prepare ~path:"local-type-traversal.HC"
      "U0 Walk(){\n\
       I64 outer;\n\
       {U8 nested;}\n\
       if(1){I16 branch;}else{I32 alternate;}\n\
       while(0) I64 loop;\n\
       try {I64 tried;} catch {I64 caught;}\n\
       }"
  in
  let function_ = function_named results "Walk" in
  Alcotest.(check (list string))
    "nested statements keep function-wide declaration order"
    [ "outer"; "nested"; "branch"; "alternate"; "loop"; "tried"; "caught" ]
    (locals function_
    |> List.map (fun local ->
        local |> Semantic_local_type_resolution.local_symbol
        |> Semantic_symbol.name));
  Alcotest.(check (list int))
    "nested declarations use one consecutive namespace" [ 0; 1; 2; 3; 4; 5; 6 ]
    (locals function_
    |> List.map Semantic_local_type_resolution.local_declaration_index);
  let scope = Semantic_local_type_resolution.function_scope function_ in
  locals function_
  |> List.iter (fun local ->
      Alcotest.(check bool)
        "every local belongs to the function scope" true
        (Semantic_symbol.Scope_id.equal
           (local |> Semantic_local_type_resolution.local_symbol
          |> Semantic_symbol.scope_id)
           (Semantic_symbol_table.scope_id scope)))

let expect_callback local =
  match Semantic_local_type_resolution.local_declarator_kind local with
  | Semantic_local_type_resolution.Function_pointer pointer -> pointer
  | Semantic_local_type_resolution.Object ->
      Alcotest.fail "expected a function-pointer local"

let callback_types () =
  let results =
    prepare ~path:"local-type-callbacks.HC"
      "class Node {};\n\
       U0 Callbacks(){\n\
       U8 reg R9 *(**handler)(Node *node,I64=lastclass,U8 *(*nested)(reg R8 \
       noreg I64 value=\"nested\",...),...);\n\
       }"
  in
  let local =
    function_named results "Callbacks" |> fun f -> local_named f "handler"
  in
  check_local_mask "automatic callback member-list mask" 0x8L local;
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.U8 ~pointer_depth:1 local;
  let pointer = expect_callback local in
  Alcotest.(check int)
    "callback declarator indirection" 2
    (pointer
   |> Semantic_local_type_resolution.function_pointer_indirection_origins
   |> List.length);
  let signature =
    Semantic_local_type_resolution.function_pointer_signature pointer
  in
  let parameters =
    Semantic_function_type_resolution.signature_parameters signature
  in
  Alcotest.(check (list (option string)))
    "callback parameters retain names and gaps"
    [ Some "node"; None; Some "nested" ]
    (List.map Semantic_function_type_resolution.parameter_name parameters);
  Alcotest.(check (list int64))
    "local callback parameters retain source-derived masks" [ 0L; 0x23L; 0x8L ]
    (parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  Alcotest.(check bool)
    "callback keeps its terminal ellipsis" true
    (signature |> Semantic_function_type_resolution.signature_variadic_origin
   |> Option.is_some);
  let default =
    List.nth parameters 1 |> Semantic_function_type_resolution.parameter_default
  in
  Alcotest.(check bool)
    "callback keeps the lastclass default" true
    (match default with
    | Some (Semantic_function_type_resolution.Lastclass_default _) -> true
    | None | Some (Semantic_function_type_resolution.Expression_default _) ->
        false);
  let nested =
    match
      List.nth parameters 2
      |> Semantic_function_type_resolution.parameter_declarator_kind
    with
    | Semantic_function_type_resolution.Function_pointer pointer -> pointer
    | Semantic_function_type_resolution.Object ->
        Alcotest.fail "expected a nested callback"
  in
  Alcotest.(check int)
    "nested callback indirection" 1
    (nested
   |> Semantic_function_type_resolution.function_pointer_indirection_origins
   |> List.length);
  Alcotest.(check (list int64))
    "nested local callback keeps its string-default mask" [ 0x5L ]
    (nested |> Semantic_function_type_resolution.function_pointer_signature
   |> Semantic_function_type_resolution.signature_parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  let nested_parameter =
    nested |> Semantic_function_type_resolution.function_pointer_signature
    |> Semantic_function_type_resolution.signature_parameters |> List.hd
  in
  Alcotest.(check (list int))
    "nested local callback keeps ordered register requests" [ 8; 32 ]
    (nested_parameter
   |> Semantic_function_type_resolution.parameter_register_requests
    |> List.map (fun request ->
        Semantic_register_request.effective [ request ]
        |> Semantic_register_request.source_code));
  Alcotest.(check int)
    "nested local callback uses the last register request" 32
    (nested_parameter
   |> Semantic_function_type_resolution.parameter_register_selection
   |> Semantic_register_request.source_code);
  let request =
    Semantic_local_type_resolution.local_register_requests local |> List.hd
  in
  Alcotest.(check (option string))
    "callback keeps its requested register" (Some "R9")
    (Semantic_local_type_resolution.register_request_explicit_register request)

let local_member_flags () =
  let results =
    prepare ~path:"local-member-flags.HC"
      "U0 Flags(){I64 automatic; U0 (*callback)(); static I64 first,second; \
       static U0 (*stored_callback)(I64=lastclass); }"
  in
  let function_ = function_named results "Flags" in
  [
    ("automatic", 0L);
    ("callback", 0x8L);
    ("first", 0x40L);
    ("second", 0x40L);
    ("stored_callback", 0x48L);
  ]
  |> List.iter (fun (name, expected) ->
      check_local_mask
        (name ^ " exact member-list mask")
        expected
        (local_named function_ name));
  let nested = local_named function_ "stored_callback" |> expect_callback in
  Alcotest.(check (list int64))
    "callback parameter flags do not alter the static callback" [ 0x23L ]
    (nested |> Semantic_local_type_resolution.function_pointer_signature
   |> Semantic_function_type_resolution.signature_parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask)

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

let pinned_static_local_declarations () =
  let fixtures =
    [
      ( "Kernel/KMisc.HC",
        "static I64 time_stamp_start=0,timer_start=0,HPET_start=0;",
        "U0 Timer(){static I64 time_stamp_start=0,timer_start=0,HPET_start=0;}",
        "Timer",
        [ 0x40L; 0x40L; 0x40L ] );
      ( "Adam/WinMgr.HC",
        "static CD3I64 single_ms={0,0,0};",
        "class CD3I64 {}; U0 Mouse(){static CD3I64 single_ms={0,0,0};}",
        "Mouse",
        [ 0x40L ] );
      ( "Adam/Gr/SpriteMesh.HC",
        "static I64 cpu_num=0;",
        "U0 Mesh(){static I64 cpu_num=0;}",
        "Mesh",
        [ 0x40L ] );
    ]
  in
  fixtures
  |> List.iter (fun (path, fragment, source, function_name, expected) ->
      Alcotest.(check bool)
        (path ^ " retains the audited declaration")
        true
        (contains (pinned_source path) fragment);
      let function_ =
        prepare ~path source |> fun results ->
        function_named results function_name
      in
      Alcotest.(check (list int64))
        (path ^ " local masks") expected
        (locals function_
        |> List.map Semantic_local_type_resolution.local_flag_mask))

let generated_provenance () =
  let results =
    prepare ~path:"generated-local-types.HC"
      "#define TYPE I64i\n\
       #define QUAL reg R14\n\
       #define NAME generated\n\
       #define STORAGE static\n\
       #define STATIC_NAME stored\n\
       U0 Generated(){TYPE QUAL NAME; STORAGE TYPE STATIC_NAME;}"
  in
  let function_ = function_named results "Generated" in
  let local = local_named function_ "generated" in
  let stored = local_named function_ "stored" in
  check_local_mask "generated static member-list mask" 0x40L stored;
  let request =
    Semantic_local_type_resolution.local_register_requests local |> List.hd
  in
  let origins =
    [
      local |> Semantic_local_type_resolution.local_symbol
      |> Semantic_symbol.origin;
      local |> Semantic_local_type_resolution.local_type_reference
      |> Semantic_type_reference.spelling_origin;
      request |> Semantic_local_type_resolution.register_request_origin;
      request
      |> Semantic_local_type_resolution
         .register_request_explicit_register_origin |> Option.get;
      stored |> Semantic_local_type_resolution.local_symbol
      |> Semantic_symbol.origin;
      stored |> Semantic_local_type_resolution.local_storage_origins |> List.hd;
    ]
  in
  origins
  |> List.iter (fun site ->
      let site = source_origin site in
      Alcotest.(check bool)
        "generated source keeps its invocation" true
        (Option.is_some site.generated_from);
      Alcotest.(check bool)
        "generated source keeps its definition" true
        (Option.is_some site.defined_at))

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

let included_static_provenance () =
  let directory = Filename.temp_dir "holyc-local-flags-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "locals.HC" in
      write_file root_path "#include \"locals\"";
      write_file include_path "U0 Included(){static I64 stored;}";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let results = resolve session ast in
      let local =
        function_named results "Included" |> fun function_ ->
        local_named function_ "stored"
      in
      check_local_mask "included static member-list mask" 0x40L local;
      let storage_site =
        local |> Semantic_local_type_resolution.local_storage_origins |> List.hd
        |> source_origin
      in
      let storage_source =
        Source_manager.find (Session.sources session) storage_site.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "the static token keeps its included source" "locals.HC"
        (Source_file.path storage_source |> Filename.basename))

let modes_determinism_and_purity () =
  let source =
    "class Value {}; U0 Same(){Value reg R12 *item; static I64 count=1;}"
  in
  let summarize mode =
    let results = prepare ~mode ~path:"local-type-modes.HC" source in
    let function_ = function_named results "Same" in
    let rows =
      locals function_
      |> List.map (fun local ->
          ( ( local |> Semantic_local_type_resolution.local_symbol
              |> Semantic_symbol.name,
              local |> Semantic_local_type_resolution.local_storage
              |> Semantic_local_type_resolution.storage_name,
              Semantic_type.pointer_depth (resolved_type local) ),
            Semantic_local_type_resolution.local_flag_mask local ))
    in
    Alcotest.(check int)
      "resolution remains read-only" results.symbol_count_before
      results.symbol_count_after;
    rows
  in
  let jit_first = summarize Preprocessor.Jit in
  let jit_second = summarize Preprocessor.Jit in
  let aot = summarize Preprocessor.Aot in
  let row = Alcotest.(pair (triple string string int) int64) in
  Alcotest.(check (list row))
    "repeated JIT resolution is deterministic" jit_first jit_second;
  Alcotest.(check (list row))
    "local type facts do not depend on JIT or AOT mode" jit_first aot

let synthesized label = Semantic_symbol.Synthesized label

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
  let local_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:function_scope ~name:"value"
         ~kind:Semantic_symbol.Local_variable ~origin:(synthesized "value"))
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
  let delimiter =
    Semantic_local_type_resolution.make_delimiter
      ~kind:Semantic_local_type_resolution.Semicolon
      ~origin:(synthesized "semicolon")
  in
  let local =
    checked
      (Semantic_local_type_resolution.make_local ~symbol:local_symbol
         ~declaration_index:0 ~declarator_index:0
         ~declaration_origin:(synthesized "declaration")
         ~declarator_origin:(synthesized "declarator")
         ~storage:Semantic_local_type_resolution.Automatic ~storage_origins:[]
         ~type_reference:reference ~register_requests:[]
         ~declarator_kind:Semantic_local_type_resolution.Object
         ~array_dimensions:[] ~initial_value:None ~delimiter ())
  in
  check_local_mask "low-level automatic object mask" 0L local;
  let function_ =
    checked
      (Semantic_local_type_resolution.make_function ~symbol:function_symbol
         ~scope:function_scope ~item_index:0 [ local ])
  in
  ignore
    (checked
       (Semantic_local_type_resolution.resolve ~table ~parent:module_scope
          [ function_ ]));
  Alcotest.(check bool)
    "noreg cannot name a register" true
    (Semantic_local_type_resolution.make_register_request
       ~kind:Semantic_local_type_resolution.Disable
       ~position:Semantic_local_type_resolution.After_type ~spelling:"noreg"
       ~origin:(synthesized "noreg") ~explicit_register:"R8"
       ~explicit_register_origin:(synthesized "R8") ()
    |> Result.is_error);
  Alcotest.(check bool)
    "only the first array dimension can be empty" true
    (Semantic_local_type_resolution.make_array_dimension ~index:1
       ~origin:(synthesized "dimension") ~opening_origin:(synthesized "open")
       ~closing_origin:(synthesized "close") ()
    |> Result.is_error);
  Alcotest.(check bool)
    "static storage needs source provenance" true
    (Semantic_local_type_resolution.make_local ~symbol:local_symbol
       ~declaration_index:0 ~declarator_index:0
       ~declaration_origin:(synthesized "declaration")
       ~declarator_origin:(synthesized "declarator")
       ~storage:Semantic_local_type_resolution.Static ~storage_origins:[]
       ~type_reference:reference ~register_requests:[]
       ~declarator_kind:Semantic_local_type_resolution.Object
       ~array_dimensions:[] ~initial_value:None ~delimiter ()
    |> Result.is_error);
  Alcotest.(check bool)
    "a task scope cannot host local type facts" true
    (Semantic_local_type_resolution.resolve ~table
       ~parent:(Semantic_symbol_table.root table)
       []
    |> Result.is_error)

let tests =
  [
    Alcotest.test_case "declaration shapes" `Quick declaration_shapes;
    Alcotest.test_case "aggregate visibility" `Quick aggregate_visibility;
    Alcotest.test_case "function-wide traversal" `Quick function_wide_traversal;
    Alcotest.test_case "callback types" `Quick callback_types;
    Alcotest.test_case "source-derived member-list flags" `Quick
      local_member_flags;
    Alcotest.test_case "pinned static-local declarations" `Quick
      pinned_static_local_declarations;
    Alcotest.test_case "generated provenance" `Quick generated_provenance;
    Alcotest.test_case "included static provenance" `Quick
      included_static_provenance;
    Alcotest.test_case "modes, determinism, and purity" `Quick
      modes_determinism_and_purity;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
  ]
