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

type prepared = {
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  globals : Semantic_global_type_resolution.t;
}

let resolve session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let globals =
    checked
      (Holyc_lib.resolve_global_types session ~declarations ~aggregates ast)
  in
  { session; ast; declarations; aggregates; globals }

let prepare ?mode ~path contents =
  let session = Session.create () in
  let ast = parse ?mode session ~path contents in
  resolve session ast

let globals prepared = Semantic_global_type_resolution.globals prepared.globals

let global_named prepared name =
  globals prepared
  |> List.find (fun global ->
      global |> Semantic_global_type_resolution.global_symbol
      |> Semantic_symbol.name |> String.equal name)

let type_reference global =
  Semantic_global_type_resolution.global_type_reference global

let resolved_type global =
  global |> type_reference |> Semantic_type_reference.resolved_type

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let check_primitive ~form ~primitive ~pointer_depth global =
  let type_ = resolved_type global in
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

let aggregate_target global =
  match Semantic_type.base (resolved_type global) with
  | Semantic_type.Aggregate symbol -> symbol
  | Semantic_type.Primitive _ -> Alcotest.fail "expected an aggregate type"

let expect_function_pointer global =
  match Semantic_global_type_resolution.global_declarator_kind global with
  | Semantic_global_type_resolution.Function_pointer pointer -> pointer
  | Semantic_global_type_resolution.Object ->
      Alcotest.fail "expected a function-pointer global"

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let source_text session site =
  let site = source_origin site in
  let source =
    Source_manager.find (Session.sources session) site.span.source |> Option.get
  in
  String.sub
    (Source_file.contents source)
    site.span.start (Span.length site.span)

let primitive_pointers_arrays_and_initializers () =
  let prepared =
    prepare ~path:"global-type-primitives.HC"
      "I64 zero,*one,**two,***three,****four;\n\
       I64i storage;\n\
       U8 bytes[],matrix[2][0],value=3,values={1,{2}};"
  in
  Alcotest.(check (list string))
    "global order"
    [
      "zero";
      "one";
      "two";
      "three";
      "four";
      "storage";
      "bytes";
      "matrix";
      "value";
      "values";
    ]
    (globals prepared
    |> List.map (fun global ->
        global |> Semantic_global_type_resolution.global_symbol
        |> Semantic_symbol.name));
  [ "zero"; "one"; "two"; "three"; "four" ]
  |> List.iteri (fun pointer_depth name ->
      let global = global_named prepared name in
      check_primitive ~form:Semantic_type.Public_spelling
        ~primitive:Primitive_type.I64 ~pointer_depth global;
      Alcotest.(check int)
        (name ^ " pointer provenance")
        pointer_depth
        (global |> type_reference |> Semantic_type_reference.pointer_origins
       |> List.length));
  check_primitive ~form:Semantic_type.Internal_storage
    ~primitive:Primitive_type.I64 ~pointer_depth:0
    (global_named prepared "storage");
  Alcotest.(check (list int))
    "module item order"
    [ 0; 0; 0; 0; 0; 1; 2; 2; 2; 2 ]
    (globals prepared
    |> List.map Semantic_global_type_resolution.global_item_index);
  Alcotest.(check (list (option int)))
    "declarator order"
    [
      Some 0;
      Some 1;
      Some 2;
      Some 3;
      Some 4;
      None;
      Some 0;
      Some 1;
      Some 2;
      Some 3;
    ]
    (globals prepared
    |> List.map Semantic_global_type_resolution.global_declarator_index);
  Alcotest.(check (list string))
    "declaration delimiters"
    [
      "comma";
      "comma";
      "comma";
      "comma";
      "semicolon";
      "semicolon";
      "comma";
      "comma";
      "comma";
      "semicolon";
    ]
    (globals prepared
    |> List.map (fun global ->
        global |> Semantic_global_type_resolution.global_delimiter
        |> Semantic_global_type_resolution.delimiter_kind
        |> Semantic_global_type_resolution.delimiter_kind_name));
  let bytes_dimensions =
    global_named prepared "bytes"
    |> Semantic_global_type_resolution.global_array_dimensions
  in
  Alcotest.(check int) "empty array rank" 1 (List.length bytes_dimensions);
  Alcotest.(check bool)
    "the first dimension is empty" true
    (List.hd bytes_dimensions
   |> Semantic_global_type_resolution.array_dimension_expression_origin
   |> Option.is_none);
  let matrix_dimensions =
    global_named prepared "matrix"
    |> Semantic_global_type_resolution.global_array_dimensions
  in
  Alcotest.(check (list int))
    "matrix dimension indexes" [ 0; 1 ]
    (List.map Semantic_global_type_resolution.array_dimension_index
       matrix_dimensions);
  Alcotest.(check (list bool))
    "explicit zero remains an expression" [ true; true ]
    (List.map
       (fun dimension ->
         dimension
         |> Semantic_global_type_resolution.array_dimension_expression_origin
         |> Option.is_some)
       matrix_dimensions);
  Alcotest.(check (list string))
    "array expressions retain their exact source" [ "2"; "0" ]
    (matrix_dimensions
    |> List.map (fun dimension ->
        dimension
        |> Semantic_global_type_resolution.array_dimension_expression_origin
        |> Option.get
        |> source_text prepared.session));
  Alcotest.(check (list string))
    "initializer shapes" [ "scalar"; "braced" ]
    ([ "value"; "values" ]
    |> List.map (fun name ->
        global_named prepared name
        |> Semantic_global_type_resolution.global_initializer |> Option.get
        |> Semantic_global_type_resolution.initializer_kind
        |> Semantic_global_type_resolution.initializer_kind_name))

let identity_at prepared item_index =
  Semantic_aggregate_resolution.identities prepared.aggregates
  |> List.find (fun identity ->
      Semantic_aggregate_resolution.identity_first_item_index identity
      = item_index)
  |> Semantic_aggregate_resolution.identity_symbol

let aggregate_visibility_and_attached_globals () =
  let prepared =
    prepare ~path:"global-type-aggregates.HC"
      "extern class Node;\n\
       Node *before;\n\
       class Node { I64 value; } \
       attached,*attached_pointer,attached_array[2],attached_value={1};\n\
       Node after;\n\
       class Node { U8 byte; };\n\
       Node newest;\n\
       union Bits { I64 raw; } bits,*bits_pointer,bits_array[1],bits_value=0;"
  in
  let first_node = identity_at prepared 0 in
  let newest_node = identity_at prepared 4 in
  let bits = identity_at prepared 6 in
  [
    "before";
    "attached";
    "attached_pointer";
    "attached_array";
    "attached_value";
    "after";
  ]
  |> List.iter (fun name ->
      Alcotest.(check int)
        (name ^ " canonical identity")
        (symbol_id first_node)
        (global_named prepared name |> aggregate_target |> symbol_id));
  Alcotest.(check int)
    "the later definition shadows the completed identity"
    (symbol_id newest_node)
    (global_named prepared "newest" |> aggregate_target |> symbol_id);
  [ "bits"; "bits_pointer"; "bits_array"; "bits_value" ]
  |> List.iter (fun name ->
      Alcotest.(check int)
        (name ^ " union identity") (symbol_id bits)
        (global_named prepared name |> aggregate_target |> symbol_id));
  Alcotest.(check (list (option int)))
    "class attached declarator indexes"
    [ Some 0; Some 1; Some 2; Some 3 ]
    ([ "attached"; "attached_pointer"; "attached_array"; "attached_value" ]
    |> List.map (fun name ->
        global_named prepared name
        |> Semantic_global_type_resolution.global_declarator_index));
  Alcotest.(check int)
    "attached pointer depth" 1
    (global_named prepared "attached_pointer"
    |> resolved_type |> Semantic_type.pointer_depth);
  Alcotest.(check int)
    "attached array rank" 1
    (global_named prepared "attached_array"
    |> Semantic_global_type_resolution.global_array_dimensions |> List.length);
  Alcotest.(check string)
    "attached initializer" "braced"
    (global_named prepared "attached_value"
    |> Semantic_global_type_resolution.global_initializer |> Option.get
    |> Semantic_global_type_resolution.initializer_kind
    |> Semantic_global_type_resolution.initializer_kind_name)

let parameter_names signature =
  signature |> Semantic_function_type_resolution.signature_parameters
  |> List.map Semantic_function_type_resolution.parameter_name

let function_pointer_shapes () =
  let prepared =
    prepare ~path:"global-type-callbacks.HC"
      "class Node {};\n\
       U8 *(*one)(); U8 *(**two)(); U8 *(***three)(); U8 *(****four)();\n\
       U0 (**handler)(Node *node,I64=lastclass,U8 *(*nested)(reg R11 noreg I64 \
       value=\"nested\",...),...);"
  in
  [ "one"; "two"; "three"; "four" ]
  |> List.iteri (fun index name ->
      let global = global_named prepared name in
      check_primitive ~form:Semantic_type.Public_spelling
        ~primitive:Primitive_type.U8 ~pointer_depth:1 global;
      Alcotest.(check int)
        (name ^ " declarator indirection")
        (index + 1)
        (global |> expect_function_pointer
       |> Semantic_global_type_resolution.function_pointer_indirection_origins
       |> List.length));
  let handler = global_named prepared "handler" |> expect_function_pointer in
  Alcotest.(check bool)
    "a callback global remains a global-variable symbol" true
    ( global_named prepared "handler"
    |> Semantic_global_type_resolution.global_symbol |> Semantic_symbol.kind
    |> fun kind ->
      Semantic_symbol.equal_kind kind Semantic_symbol.Global_variable );
  let handler_origin =
    Semantic_global_type_resolution.function_pointer_origin handler
    |> source_origin
  in
  Alcotest.(check bool)
    "ordinary callback provenance is not generated" true
    (Option.is_none handler_origin.generated_from);
  Alcotest.(check int)
    "handler declarator indirection" 2
    (handler
   |> Semantic_global_type_resolution.function_pointer_indirection_origins
   |> List.length);
  let signature =
    Semantic_global_type_resolution.function_pointer_signature handler
  in
  Alcotest.(check (list (option string)))
    "callback slots retain names and gaps"
    [ Some "node"; None; Some "nested" ]
    (parameter_names signature);
  Alcotest.(check bool)
    "outer callback is variadic" true
    (signature |> Semantic_function_type_resolution.signature_variadic_origin
   |> Option.is_some);
  let parameters =
    Semantic_function_type_resolution.signature_parameters signature
  in
  Alcotest.(check (list int64))
    "global callback parameters retain source-derived masks" [ 0L; 0x23L; 0x8L ]
    (parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  let node_type =
    List.nth parameters 0
    |> Semantic_function_type_resolution.parameter_type_reference
    |> Semantic_type_reference.resolved_type
  in
  Alcotest.(check int)
    "node pointer depth" 1
    (Semantic_type.pointer_depth node_type);
  (match Semantic_type.base node_type with
  | Semantic_type.Aggregate symbol ->
      Alcotest.(check string)
        "node identity" "Node"
        (Semantic_symbol.name symbol)
  | Semantic_type.Primitive _ -> Alcotest.fail "expected a Node parameter");
  (match
     List.nth parameters 1
     |> Semantic_function_type_resolution.parameter_default
   with
  | Some (Semantic_function_type_resolution.Lastclass_default _) -> ()
  | Some (Semantic_function_type_resolution.Expression_default _) | None ->
      Alcotest.fail "expected a non-trailing lastclass default");
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
  let nested_signature =
    Semantic_function_type_resolution.function_pointer_signature nested
  in
  Alcotest.(check (list (option string)))
    "nested callback parameters" [ Some "value" ]
    (parameter_names nested_signature);
  Alcotest.(check (list int64))
    "nested global callback keeps its string-default mask" [ 0x5L ]
    (nested_signature |> Semantic_function_type_resolution.signature_parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  let nested_parameter =
    nested_signature |> Semantic_function_type_resolution.signature_parameters
    |> List.hd
  in
  Alcotest.(check (list int))
    "nested global callback keeps ordered register requests" [ 11; 32 ]
    (nested_parameter
   |> Semantic_function_type_resolution.parameter_register_requests
    |> List.map (fun request ->
        Semantic_register_request.effective [ request ]
        |> Semantic_register_request.source_code));
  Alcotest.(check int)
    "nested global callback uses the last register request" 32
    (nested_parameter
   |> Semantic_function_type_resolution.parameter_register_selection
   |> Semantic_register_request.source_code);
  Alcotest.(check bool)
    "nested callback is variadic" true
    (nested_signature
   |> Semantic_function_type_resolution.signature_variadic_origin
   |> Option.is_some)

let binding_modes_determinism_and_purity () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"global-type-bindings.HC"
      "extern I64 external; import U8 *imported;\n\
       _extern REMOTE I16 alternate; _import REMOTE2 U16 remote;"
  in
  let table = Session.semantic_symbols prepared.session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list string))
    "bound globals all receive types"
    [ "external"; "imported"; "alternate"; "remote" ]
    (globals prepared
    |> List.map (fun global ->
        global |> Semantic_global_type_resolution.global_symbol
        |> Semantic_symbol.name));
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.U8 ~pointer_depth:1
    (global_named prepared "imported");
  let again =
    checked
      (Holyc_lib.resolve_global_types prepared.session
         ~declarations:prepared.declarations ~aggregates:prepared.aggregates
         prepared.ast)
  in
  let signature resolution =
    Semantic_global_type_resolution.globals resolution
    |> List.map (fun global ->
        let type_ = resolved_type global in
        Printf.sprintf "%s:%d:%d"
          (global |> Semantic_global_type_resolution.global_symbol
         |> Semantic_symbol.name)
          (Semantic_global_type_resolution.global_item_index global)
          (Semantic_type.pointer_depth type_))
  in
  Alcotest.(check (list string))
    "resolution is deterministic"
    (signature prepared.globals)
    (signature again);
  Alcotest.(check int)
    "successful resolution preserves scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "successful resolution preserves symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let generated_provenance () =
  let prepared =
    prepare ~path:"generated-global-types.HC"
      "#define RET U8\n\
       #define RETSTAR *\n\
       #define FPSTARS **\n\
       #define PARAM I64i\n\
       #define SIZE 2\n\
       RET RETSTAR (FPSTARS generated)(PARAM value,...)[SIZE];"
  in
  let global = global_named prepared "generated" in
  let pointer = expect_function_pointer global in
  let signature =
    Semantic_global_type_resolution.function_pointer_signature pointer
  in
  let parameter =
    Semantic_function_type_resolution.signature_parameters signature |> List.hd
  in
  let dimension =
    Semantic_global_type_resolution.global_array_dimensions global |> List.hd
  in
  let origins =
    [
      global |> type_reference |> Semantic_type_reference.spelling_origin;
      global |> type_reference |> Semantic_type_reference.pointer_origins
      |> List.hd;
      pointer
      |> Semantic_global_type_resolution.function_pointer_indirection_origins
      |> List.hd;
      parameter |> Semantic_function_type_resolution.parameter_type_reference
      |> Semantic_type_reference.spelling_origin;
      dimension
      |> Semantic_global_type_resolution.array_dimension_expression_origin
      |> Option.get;
    ]
  in
  origins
  |> List.iter (fun site ->
      let site = source_origin site in
      Alcotest.(check bool)
        "generated token keeps its invocation" true
        (Option.is_some site.generated_from);
      Alcotest.(check bool)
        "generated token keeps its definition" true
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

let included_provenance () =
  let directory = Filename.temp_dir "holyc-global-types-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "globals.HC" in
      write_file root_path "#include \"globals\"";
      write_file include_path "U0 (*included)(I64 value)[3];";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let global =
        resolve session ast |> fun prepared -> global_named prepared "included"
      in
      let pointer = expect_function_pointer global in
      let parameter =
        pointer |> Semantic_global_type_resolution.function_pointer_signature
        |> Semantic_function_type_resolution.signature_parameters |> List.hd
      in
      let dimension =
        Semantic_global_type_resolution.global_array_dimensions global
        |> List.hd
      in
      [
        global |> type_reference |> Semantic_type_reference.spelling_origin;
        Semantic_global_type_resolution.function_pointer_origin pointer;
        parameter |> Semantic_function_type_resolution.parameter_origin;
        Semantic_global_type_resolution.array_dimension_origin dimension;
      ]
      |> List.iter (fun site ->
          let site = source_origin site in
          let source =
            Source_manager.find (Session.sources session) site.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "semantic provenance keeps the included source" "globals.HC"
            (Source_file.path source |> Filename.basename)))

let mismatched_inputs_and_visibility_fail_without_mutation () =
  let primary =
    prepare ~path:"primary-global-types.HC" "class Box {}; Box value;"
  in
  let foreign =
    prepare ~path:"foreign-global-types.HC" "class Other {}; Other other;"
  in
  let table = Session.semantic_symbols primary.session in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let reject label result =
    Alcotest.(check bool) label true (Result.is_error result);
    Alcotest.(check int)
      (label ^ " preserves scopes")
      scope_count
      (Semantic_symbol_table.all_scopes table |> List.length);
    Alcotest.(check int)
      (label ^ " preserves symbols")
      symbol_count
      (Semantic_symbol_table.all_symbols table |> List.length)
  in
  reject "foreign aggregate reconciliation"
    (Holyc_lib.resolve_global_types primary.session
       ~declarations:primary.declarations ~aggregates:foreign.aggregates
       primary.ast);
  reject "unrelated AST"
    (Holyc_lib.resolve_global_types primary.session
       ~declarations:primary.declarations ~aggregates:primary.aggregates
       foreign.ast);
  let empty_aggregates =
    checked
      (Semantic_aggregate_resolution.resolve ~table
         ~parent:(Semantic_declaration_collection.scope primary.declarations)
         [])
  in
  reject "missing aggregate reconciliation"
    (Holyc_lib.resolve_global_types primary.session
       ~declarations:primary.declarations ~aggregates:empty_aggregates
       primary.ast)

let synthesized label = Semantic_symbol.Synthesized label

let low_level_validation () =
  let table = Semantic_symbol_table.create () in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table
         ~parent:(Semantic_symbol_table.root table)
         ~kind:Semantic_symbol_table.Module ~name:"low-level.HC" ())
  in
  let add_global name =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name
         ~kind:Semantic_symbol.Global_variable ~origin:(synthesized name))
  in
  let i64 =
    checked
      (Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
         ~primitive:Primitive_type.I64 ~pointer_depth:0)
  in
  let reference =
    checked
      (Semantic_type_reference.make ~spelling:"I64"
         ~spelling_origin:(synthesized "I64") ~pointer_origins:[]
         ~resolved_type:i64)
  in
  let semicolon =
    Semantic_global_type_resolution.make_delimiter
      ~kind:Semantic_global_type_resolution.Semicolon
      ~origin:(synthesized "semicolon")
  in
  let comma =
    Semantic_global_type_resolution.make_delimiter
      ~kind:Semantic_global_type_resolution.Comma ~origin:(synthesized "comma")
  in
  let make ?declarator_index ?(delimiter = semicolon) symbol item_index =
    checked
      (Semantic_global_type_resolution.make_global ~symbol ~item_index
         ?declarator_index ~declarator_origin:(synthesized "declarator")
         ~type_reference:reference
         ~declarator_kind:Semantic_global_type_resolution.Object
         ~array_dimensions:[] ~initial_value:None ~delimiter ())
  in
  let first_symbol = add_global "first" in
  let second_symbol = add_global "second" in
  let first = make first_symbol 0 in
  let second = make second_symbol 1 in
  let repeated = make first_symbol 1 in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let reject label globals =
    Alcotest.(check bool)
      label true
      (Semantic_global_type_resolution.resolve ~table ~parent:module_scope
         globals
      |> Result.is_error);
    Alcotest.(check int)
      (label ^ " preserves scopes")
      scope_count
      (Semantic_symbol_table.all_scopes table |> List.length);
    Alcotest.(check int)
      (label ^ " preserves symbols")
      symbol_count
      (Semantic_symbol_table.all_symbols table |> List.length)
  in
  reject "reversed source order" [ second; first ];
  reject "repeated symbols" [ first; repeated ];
  let group_first = make ~declarator_index:0 ~delimiter:comma first_symbol 0 in
  let group_second = make ~declarator_index:1 second_symbol 0 in
  ignore
    (checked
       (Semantic_global_type_resolution.resolve ~table ~parent:module_scope
          [ group_first; group_second ]));
  reject "a grouped declaration needs a comma"
    [ make ~declarator_index:0 first_symbol 0; group_second ];
  reject "a final comma is unterminated" [ group_first ];
  Alcotest.(check bool)
    "array indexes cannot be negative" true
    (Semantic_global_type_resolution.make_array_dimension ~index:(-1)
       ~origin:(synthesized "dimension") ~opening_origin:(synthesized "open")
       ~closing_origin:(synthesized "close") ()
    |> Result.is_error);
  Alcotest.(check bool)
    "only the first array dimension can be empty" true
    (Semantic_global_type_resolution.make_array_dimension ~index:1
       ~origin:(synthesized "dimension") ~opening_origin:(synthesized "open")
       ~closing_origin:(synthesized "close") ()
    |> Result.is_error);
  let foreign_table = Semantic_symbol_table.create () in
  let foreign_module =
    checked
      (Semantic_symbol_table.create_scope foreign_table
         ~parent:(Semantic_symbol_table.root foreign_table)
         ~kind:Semantic_symbol_table.Module ~name:"foreign.HC" ())
  in
  let foreign_symbol =
    checked
      (Semantic_symbol_table.add foreign_table ~scope:foreign_module
         ~name:"foreign" ~kind:Semantic_symbol.Global_variable
         ~origin:(synthesized "foreign"))
  in
  reject "foreign global symbol" [ make foreign_symbol 0 ];
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
         ~spelling_origin:(synthesized "Foreign") ~pointer_origins:[]
         ~resolved_type:foreign_type)
  in
  let foreign_parameter =
    checked
      (Semantic_function_type_resolution.make_parameter ~index:0
         ~origin:(synthesized "parameter") ~type_reference:foreign_reference
         ~declarator_kind:Semantic_function_type_resolution.Object ~default:None
         ())
  in
  let foreign_signature =
    checked
      (Semantic_function_type_resolution.make_signature
         ~opening_origin:(synthesized "opening")
         ~parameters:[ foreign_parameter ]
         ~closing_origin:(synthesized "closing") ())
  in
  let foreign_pointer =
    checked
      (Semantic_function_type_resolution.make_function_pointer
         ~origin:(synthesized "callback")
         ~opening_origin:(synthesized "declarator opening")
         ~indirection_origins:[ synthesized "star" ]
         ~closing_origin:(synthesized "declarator closing")
         ~signature:foreign_signature)
  in
  let foreign_callback =
    checked
      (Semantic_global_type_resolution.make_global ~symbol:first_symbol
         ~item_index:0
         ~declarator_origin:(synthesized "callback declarator")
         ~type_reference:reference
         ~declarator_kind:
           (Semantic_global_type_resolution.Function_pointer foreign_pointer)
         ~array_dimensions:[] ~initial_value:None ~delimiter:semicolon ())
  in
  reject "foreign recursive callback type" [ foreign_callback ];
  Alcotest.(check bool)
    "type spelling must match the resolved primitive" true
    (Semantic_type_reference.make ~spelling:"U64"
       ~spelling_origin:(synthesized "wrong spelling")
       ~pointer_origins:[] ~resolved_type:i64
    |> Result.is_error);
  Alcotest.(check bool)
    "a task scope cannot host global type facts" true
    (Semantic_global_type_resolution.resolve ~table
       ~parent:(Semantic_symbol_table.root table)
       []
    |> Result.is_error)

let tests =
  [
    Alcotest.test_case "primitive pointers, arrays, and initializers" `Quick
      primitive_pointers_arrays_and_initializers;
    Alcotest.test_case "aggregate visibility and attached globals" `Quick
      aggregate_visibility_and_attached_globals;
    Alcotest.test_case "function-pointer shapes" `Quick function_pointer_shapes;
    Alcotest.test_case "bindings, determinism, and purity" `Quick
      binding_modes_determinism_and_purity;
    Alcotest.test_case "generated provenance" `Quick generated_provenance;
    Alcotest.test_case "included provenance" `Quick included_provenance;
    Alcotest.test_case "mismatches and visibility do not mutate" `Quick
      mismatched_inputs_and_visibility_fail_without_mutation;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
  ]
