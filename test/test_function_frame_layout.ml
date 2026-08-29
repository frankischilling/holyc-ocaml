open Holyc_lib
module Frame = Semantic_function_frame_layout

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
  aggregates : Semantic_aggregate_resolution.t;
  aggregate_headers : Semantic_aggregate_header_resolution.t;
  aggregate_members : Semantic_member_type_resolution.t;
  aggregate_layouts : Semantic_aggregate_layout.t;
  functions : Semantic_function_collection.t;
  function_types : Semantic_function_type_resolution.t;
  local_types : Semantic_local_type_resolution.t;
  bindings : Semantic_function_binding_index.t;
}

let prepare ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let aggregate_headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  let collected_members =
    checked (Holyc_lib.collect_members session ~declarations ast)
  in
  let aggregate_members =
    checked
      (Holyc_lib.resolve_member_types session ~declarations ~aggregates
         ~headers:aggregate_headers ~members:collected_members ast)
  in
  let aggregate_layouts =
    checked
      (Holyc_lib.layout_aggregates session ~declarations ~aggregates
         ~headers:aggregate_headers ~members:aggregate_members ast)
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
  {
    session;
    ast;
    declarations;
    aggregates;
    aggregate_headers;
    aggregate_members;
    aggregate_layouts;
    functions;
    function_types;
    local_types;
    bindings;
  }

let symbol_name symbol = Semantic_symbol.name symbol
let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let contains_substring text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search offset =
    if offset + fragment_length > text_length then false
    else if String.sub text offset fragment_length = fragment then true
    else search (offset + 1)
  in
  search 0

let layout prepared =
  Holyc_lib.layout_function_frames prepared.session
    ~declarations:prepared.declarations ~bindings:prepared.bindings
    ~function_types:prepared.function_types ~local_types:prepared.local_types
    ~aggregate_layouts:prepared.aggregate_layouts prepared.ast
  |> checked

let frame_named frames name =
  Frame.functions frames
  |> List.find (fun frame ->
      frame |> Frame.function_symbol |> symbol_name |> String.equal name)

let locations frame = Frame.function_locations frame

let location_named frame name =
  locations frame
  |> List.find (fun location ->
      location |> Frame.location_symbol |> symbol_name |> String.equal name)

let location_names frame =
  locations frame
  |> List.map (fun location -> location |> Frame.location_symbol |> symbol_name)

let slot location =
  match Frame.location_frame_slot location with
  | Some slot -> slot
  | None ->
      Alcotest.failf "expected %s to have an RBP-relative frame slot"
        (location |> Frame.location_symbol |> symbol_name)

let displacement location = location |> slot |> Frame.frame_slot_displacement
let slot_size location = location |> slot |> Frame.frame_slot_size

let check_location_lists frame ~names ~kinds ~displacements ~sizes ~alignments =
  let actual = locations frame in
  Alcotest.(check (list string))
    "location source order" names (location_names frame);
  Alcotest.(check (list string))
    "location kinds" kinds
    (actual
    |> List.map (fun location ->
        location |> Frame.location_kind |> Frame.location_kind_name));
  Alcotest.(check (list int64))
    "RBP displacements" displacements
    (List.map displacement actual);
  Alcotest.(check (list int64))
    "allocated sizes" sizes
    (List.map Frame.location_allocated_size actual);
  Alcotest.(check (list int))
    "applied alignments" alignments
    (List.map Frame.location_alignment actual)

let fixed_parameters_and_variadic_slots () =
  let prepared =
    prepare ~path:"frame-parameters.HC"
      "U0 Inputs(I8 byte,I32 word,I64 wide,...){ }\n\
       U0 Gap(I8 named,I32,I64 tail,...){ }"
  in
  let frame = prepared |> layout |> fun frames -> frame_named frames "Inputs" in
  let all = locations frame in
  Alcotest.(check (list string))
    "fixed parameters precede compiler varargs"
    [ "byte"; "word"; "wide"; "argc"; "argv" ]
    (location_names frame);
  Alcotest.(check (list string))
    "parameter location kinds"
    [
      "named-parameter";
      "named-parameter";
      "named-parameter";
      "variadic-argc";
      "variadic-argv";
    ]
    (all
    |> List.map (fun location ->
        location |> Frame.location_kind |> Frame.location_kind_name));
  Alcotest.(check (list int64))
    "saved RBP and return address add sixteen bytes"
    [ 16L; 24L; 32L; 40L; 48L ]
    (List.map displacement all);
  Alcotest.(check (list int64))
    "every argument consumes one eight-byte slot" [ 8L; 8L; 8L; 8L; 8L ]
    (List.map slot_size all);
  Alcotest.(check (list int64))
    "declared scalar widths do not change argument allocation"
    [ 8L; 8L; 8L; 8L; 8L ]
    (List.map Frame.location_allocated_size all);
  let argc = location_named frame "argc" in
  Alcotest.(check string)
    "argc is scalar" "scalar"
    (argc |> Frame.location_value_shape |> Frame.value_shape_name);
  Alcotest.(check (list int64))
    "argc has no dimensions" []
    (argc |> Frame.location_dimensions |> List.map Frame.dimension_value);
  let argv = location_named frame "argv" in
  Alcotest.(check string)
    "argv retains array shape" "array"
    (argv |> Frame.location_value_shape |> Frame.value_shape_name);
  let argv_dimensions = Frame.location_dimensions argv in
  Alcotest.(check (list string))
    "argv dimension provenance"
    [ "compiler-placeholder-extent" ]
    (argv_dimensions
    |> List.map (fun dimension ->
        dimension |> Frame.dimension_kind |> Frame.dimension_kind_name));
  Alcotest.(check (list int64))
    "argv placeholder shape" [ 127L ]
    (List.map Frame.dimension_value argv_dimensions);
  Alcotest.(check int64)
    "argv placeholder does not multiply its slot" 8L
    (Frame.location_allocated_size argv);
  let gap = prepared |> layout |> fun frames -> frame_named frames "Gap" in
  Alcotest.(check (list string))
    "unnamed fixed slot has no invented binding"
    [ "named"; "tail"; "argc"; "argv" ]
    (location_names gap);
  Alcotest.(check (list int64))
    "unnamed slot still advances varargs cursor" [ 16L; 32L; 40L; 48L ]
    (locations gap |> List.map displacement)

let local_size_alignment_classes () =
  let prepared =
    prepare ~path:"frame-local-alignments.HC"
      "U0 Sizes(){I8 one;I16 two;I32 four;I64 eight;U8 many[9];}\n\
       U0 Worked(){I8 a;I32 b;I64 c;}"
  in
  let frames = layout prepared in
  let sizes = frame_named frames "Sizes" in
  check_location_lists sizes
    ~names:[ "one"; "two"; "four"; "eight"; "many" ]
    ~kinds:
      [
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
      ]
    ~displacements:[ -1L; -4L; -8L; -16L; -32L ]
    ~sizes:[ 1L; 2L; 4L; 8L; 9L ] ~alignments:[ 1; 2; 4; 8; 8 ];
  Alcotest.(check int64)
    "more-than-eight placement rounds the frame" 32L
    (Frame.function_frame_size sizes);
  let worked = frame_named frames "Worked" in
  check_location_lists worked ~names:[ "a"; "b"; "c" ]
    ~kinds:[ "automatic-local"; "automatic-local"; "automatic-local" ]
    ~displacements:[ -1L; -8L; -16L ] ~sizes:[ 1L; 4L; 8L ]
    ~alignments:[ 1; 4; 8 ];
  Alcotest.(check int64)
    "I8/I32/I64 final frame" 16L
    (Frame.function_frame_size worked)

let primitive_pointer_callback_aggregate_union_and_array_sizes () =
  let prepared =
    prepare ~path:"frame-type-sizes.HC"
      "class Triple {U8 bytes[3];};\n\
       union Choice {U8 bytes[12];};\n\
       U0 Types(){I16 primitive;I64 *pointer;I64 (*callback)(I64 value);\n\
       Triple object;Choice union_value;U16 array[3];}"
  in
  let frame = prepared |> layout |> fun frames -> frame_named frames "Types" in
  check_location_lists frame
    ~names:
      [ "primitive"; "pointer"; "callback"; "object"; "union_value"; "array" ]
    ~kinds:
      [
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
      ]
    ~displacements:[ -2L; -16L; -24L; -28L; -40L; -48L ]
    ~sizes:[ 2L; 8L; 8L; 3L; 12L; 6L ]
    ~alignments:[ 2; 8; 8; 2; 8; 4 ];
  Alcotest.(check (list int64))
    "primitive, pointer, callback, aggregate, and array element sizes"
    [ 2L; 8L; 8L; 3L; 12L; 2L ]
    (locations frame |> List.map Frame.location_element_size);
  Alcotest.(check (list string))
    "declarator shapes"
    [ "object"; "object"; "function-pointer"; "object"; "object"; "object" ]
    (locations frame
    |> List.map (fun location ->
        location |> Frame.location_declarator_shape
        |> Frame.declarator_shape_name));
  Alcotest.(check (list string))
    "scalar and array value shapes"
    [ "scalar"; "scalar"; "scalar"; "scalar"; "scalar"; "array" ]
    (locations frame
    |> List.map (fun location ->
        location |> Frame.location_value_shape |> Frame.value_shape_name));
  let pointer = location_named frame "pointer" in
  Alcotest.(check int)
    "object pointer depth" 1
    (pointer |> Frame.location_checked_type |> Semantic_type.pointer_depth);
  let array = location_named frame "array" in
  Alcotest.(check (list string))
    "array extent provenance" [ "source-extent" ]
    (array |> Frame.location_dimensions
    |> List.map (fun dimension ->
        dimension |> Frame.dimension_kind |> Frame.dimension_kind_name));
  Alcotest.(check (list int64))
    "array extent" [ 3L ]
    (array |> Frame.location_dimensions |> List.map Frame.dimension_value);
  Alcotest.(check int64)
    "completed type frame size" 48L
    (Frame.function_frame_size frame);
  let incomplete_pointer =
    prepare ~path:"frame-incomplete-pointer.HC"
      "extern class Later;U0 Pointer(){Later *value;}class Later {I64 field;};"
  in
  let pointer_frame =
    incomplete_pointer |> layout |> fun frames -> frame_named frames "Pointer"
  in
  Alcotest.(check int64)
    "pointer does not require a prior aggregate layout" 8L
    (location_named pointer_frame "value" |> Frame.location_element_size);
  let later_by_value =
    prepare ~path:"frame-later-by-value.HC"
      "extern class Later;U0 Value(){Later value;}class Later {I64 field;};"
  in
  Alcotest.(check bool)
    "earlier by-value use cannot consume a later layout" true
    (match
       Holyc_lib.layout_function_frames later_by_value.session
         ~declarations:later_by_value.declarations
         ~bindings:later_by_value.bindings
         ~function_types:later_by_value.function_types
         ~local_types:later_by_value.local_types
         ~aggregate_layouts:later_by_value.aggregate_layouts later_by_value.ast
     with
    | Error message -> String.starts_with ~prefix:"HCSEMA0074: " message
    | Ok _ -> false)

let mixed_declarators_register_gaps_static_and_zero_size () =
  let prepared =
    prepare ~path:"frame-mixed-storage.HC"
      "U0 Mixed(){I8 a,b;I32 c,*d;I16 e[3],f;}\n\
       U0 Storage(){I8 before;I64 reg R15 register_gap;static I64 stored;\n\
       I8 after;I0 zero;}"
  in
  let frames = layout prepared in
  let mixed = frame_named frames "Mixed" in
  check_location_lists mixed
    ~names:[ "a"; "b"; "c"; "d"; "e"; "f" ]
    ~kinds:
      [
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
        "automatic-local";
      ]
    ~displacements:[ -1L; -2L; -8L; -16L; -24L; -26L ]
    ~sizes:[ 1L; 1L; 4L; 8L; 6L; 2L ] ~alignments:[ 1; 1; 4; 8; 4; 2 ];
  Alcotest.(check int64)
    "mixed declarator frame rounds to eight" 32L
    (Frame.function_frame_size mixed);
  let storage = frame_named frames "Storage" in
  Alcotest.(check (list string))
    "static local remains in source order"
    [ "before"; "register_gap"; "stored"; "after"; "zero" ]
    (location_names storage);
  Alcotest.(check (list string))
    "automatic and static distinction"
    [
      "automatic-local";
      "automatic-local";
      "static-local";
      "automatic-local";
      "automatic-local";
    ]
    (locations storage
    |> List.map (fun location ->
        location |> Frame.location_kind |> Frame.location_kind_name));
  let before = location_named storage "before" in
  let register_gap = location_named storage "register_gap" in
  let stored = location_named storage "stored" in
  let after = location_named storage "after" in
  let zero = location_named storage "zero" in
  Alcotest.(check int64)
    "small local before reg request" (-1L) (displacement before);
  Alcotest.(check int64)
    "reg request keeps pre-optimization stack gap" (-16L)
    (displacement register_gap);
  Alcotest.(check (option int64))
    "static local has no RBP displacement" None
    (Frame.location_frame_slot stored
    |> Option.map Frame.frame_slot_displacement);
  Alcotest.(check bool)
    "static exact-identity lookup is present" true
    (Frame.find_location storage (Frame.location_symbol stored)
    |> Option.is_some);
  Alcotest.(check int)
    "static metadata keeps eight-byte alignment" 8
    (Frame.location_alignment stored);
  Alcotest.(check int64)
    "static storage does not advance cursor" (-17L) (displacement after);
  Alcotest.(check int64)
    "zero-sized local shares current displacement" (-17L) (displacement zero);
  Alcotest.(check int64)
    "zero-sized allocation" 0L
    (Frame.location_allocated_size zero);
  Alcotest.(check int64) "zero-sized slot" 0L (slot_size zero);
  Alcotest.(check int64)
    "storage frame final eight-byte rounding" 24L
    (Frame.function_frame_size storage)

let exact_identity_lookup_and_source_order () =
  let prepared =
    prepare ~path:"frame-identities.HC"
      "U0 First(){I8 value;}U0 Second(){I64 lead;I8 value;}\n\
       U0 Repeat(){I8 pad;I64 pad;}"
  in
  let frames = layout prepared in
  Alcotest.(check (list string))
    "function source order"
    [ "First"; "Second"; "Repeat" ]
    (Frame.functions frames
    |> List.map (fun frame -> frame |> Frame.function_symbol |> symbol_name));
  let first = frame_named frames "First" in
  let second = frame_named frames "Second" in
  let first_value = location_named first "value" in
  let second_value = location_named second "value" in
  Alcotest.(check bool)
    "same spelling has distinct symbols" true
    (symbol_id (Frame.location_symbol first_value)
    <> symbol_id (Frame.location_symbol second_value));
  Alcotest.(check int64) "first function value" (-1L) (displacement first_value);
  Alcotest.(check int64)
    "second function value" (-9L)
    (displacement second_value);
  Alcotest.(check (option int))
    "foreign exact identity is absent" None
    (Frame.find_location second (Frame.location_symbol first_value)
    |> Option.map (fun location ->
        location |> Frame.location_symbol |> symbol_id));
  let indexed_first =
    Semantic_function_binding_index.functions prepared.bindings
    |> List.find (fun function_ ->
        function_ |> Semantic_function_binding_index.function_symbol
        |> symbol_name |> String.equal "First")
  in
  let rebuilt_bindings =
    Semantic_function_binding_index.function_bindings indexed_first
    |> List.map (fun binding ->
        ({
           binding_symbol =
             Semantic_function_binding_index.binding_symbol binding;
           binding_kind = Semantic_function_binding_index.binding_kind binding;
           parameter_index =
             Semantic_function_binding_index.binding_parameter_index binding;
           local_declaration_index =
             Semantic_function_binding_index.binding_local_declaration_index
               binding;
           local_declarator_index =
             Semantic_function_binding_index.binding_local_declarator_index
               binding;
         }
          : Semantic_function_binding_index.binding_input))
  in
  let rebuilt_index =
    Semantic_function_binding_index.build
      ~table:(Session.semantic_symbols prepared.session)
      ~parent:(Semantic_declaration_collection.scope prepared.declarations)
      [
        {
          Semantic_function_binding_index.function_symbol =
            Semantic_function_binding_index.function_symbol indexed_first;
          function_scope =
            Semantic_function_binding_index.function_scope indexed_first;
          function_item_index =
            Semantic_function_binding_index.function_item_index indexed_first;
          function_bindings = rebuilt_bindings;
        };
      ]
    |> function
    | Ok index -> index
    | Error error ->
        Alcotest.fail (Semantic_function_binding_index.error_to_string error)
  in
  let rebuilt_binding =
    Semantic_function_binding_index.functions rebuilt_index
    |> List.hd |> Semantic_function_binding_index.function_bindings |> List.hd
  in
  let original_binding = Frame.location_binding first_value in
  Alcotest.(check bool)
    "retained binding object finds its location" true
    (Frame.find_binding_location first original_binding |> Option.is_some);
  Alcotest.(check bool)
    "rebuilt binding cannot alias retained location" true
    (Frame.find_binding_location first rebuilt_binding |> Option.is_none);
  let repeated = frame_named frames "Repeat" in
  let pads =
    locations repeated
    |> List.filter (fun location ->
        location |> Frame.location_symbol |> symbol_name |> String.equal "pad")
  in
  Alcotest.(check (list int64))
    "same-function repeated spelling locations" [ -1L; -16L ]
    (List.map displacement pads);
  Alcotest.(check bool)
    "repeated spelling identities remain distinct" true
    (match pads with
    | [ first_pad; second_pad ] ->
        symbol_id (Frame.location_symbol first_pad)
        <> symbol_id (Frame.location_symbol second_pad)
    | _ -> false);
  List.iter
    (fun location ->
      let symbol = Frame.location_symbol location in
      Alcotest.(check (option int))
        "each exact identity finds itself"
        (Some (symbol_id symbol))
        (Frame.find_location repeated symbol
        |> Option.map (fun found -> found |> Frame.location_symbol |> symbol_id)
        ))
    pads

let deterministic_accessors_and_dumps () =
  let source =
    "extern U0 Prototype(I64 value);U0 Empty(){}U0 Parameters(I64 value){}\n\
     U0 Locals(){I8 byte;}U0 Mixed(I64 argument){I8 local;}"
  in
  let render mode =
    let prepared = prepare ~mode ~path:"frame-dump.HC" source in
    let frames = layout prepared in
    let human =
      Semantic_function_frame_layout_dump.human
        (Session.sources prepared.session)
        frames
    in
    let json =
      Semantic_function_frame_layout_dump.json
        (Session.sources prepared.session)
        frames
    in
    (prepared, frames, human, json)
  in
  let prepared, frames, human, json = render Preprocessor.Jit in
  Alcotest.(check string)
    "repeat human dump" human
    (Semantic_function_frame_layout_dump.human
       (Session.sources prepared.session)
       frames);
  Alcotest.(check string)
    "repeat JSON dump" json
    (Semantic_function_frame_layout_dump.json
       (Session.sources prepared.session)
       frames);
  let _, _, aot_human, aot_json = render Preprocessor.Aot in
  Alcotest.(check string) "JIT and AOT human dump" human aot_human;
  Alcotest.(check string) "JIT and AOT JSON dump" json aot_json;
  Alcotest.(check (list (pair string int64)))
    "empty/parameter/local/mixed accessors"
    [ ("Empty", 0L); ("Parameters", 0L); ("Locals", 8L); ("Mixed", 8L) ]
    (Frame.functions frames
    |> List.map (fun frame ->
        ( frame |> Frame.function_symbol |> symbol_name,
          Frame.function_frame_size frame )));
  Alcotest.(check (option int))
    "prototype has no function-frame layout" None
    (Frame.functions frames
    |> List.find_opt (fun frame ->
        frame |> Frame.function_symbol |> symbol_name
        |> String.equal "Prototype")
    |> Option.map Frame.function_item_index);
  let parsed = Yojson.Safe.from_string json in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "dump schema" "holyc-function-frame-layout-v1"
    (parsed |> member "schema" |> to_string);
  Alcotest.(check string)
    "dump reference commit" Version.reference_commit
    (parsed |> member "reference_commit" |> to_string);
  let functions = parsed |> member "functions" |> to_list in
  Alcotest.(check (list string))
    "dump function order"
    [ "Empty"; "Parameters"; "Locals"; "Mixed" ]
    (functions
    |> List.map (fun function_ -> function_ |> member "name" |> to_string));
  Alcotest.(check (list int))
    "dump location counts" [ 0; 1; 1; 2 ]
    (functions
    |> List.map (fun function_ ->
        function_ |> member "locations" |> to_list |> List.length));
  Alcotest.(check bool)
    "human dump includes empty, parameter-only, local-only, and mixed functions"
    true
    (contains_substring human "name=\"Empty\""
    && contains_substring human "name=\"Parameters\""
    && contains_substring human "name=\"Locals\""
    && contains_substring human "name=\"Mixed\"")

let indexed_function_named prepared name =
  Semantic_function_binding_index.functions prepared.bindings
  |> List.find (fun function_ ->
      function_ |> Semantic_function_binding_index.function_symbol
      |> symbol_name |> String.equal name)

let typed_function_named prepared name =
  Semantic_function_type_resolution.functions prepared.function_types
  |> List.find (fun function_ ->
      function_ |> Semantic_function_type_resolution.function_symbol
      |> symbol_name |> String.equal name)

let local_function_named prepared name =
  Semantic_local_type_resolution.functions prepared.local_types
  |> List.find (fun function_ ->
      function_ |> Semantic_local_type_resolution.function_symbol |> symbol_name
      |> String.equal name)

let local_named function_ name =
  Semantic_local_type_resolution.function_locals function_
  |> List.find (fun local ->
      local |> Semantic_local_type_resolution.local_symbol |> symbol_name
      |> String.equal name)

let integer_expression origin value =
  Semantic_aggregate_layout.Integer_expression { value; origin }

let dependency_expression origin detail =
  Semantic_aggregate_layout.Dependency_expression
    {
      dependency_kind = Semantic_aggregate_layout.Identifier_dependency;
      detail;
      origin;
    }

let local_input ?dimension_expressions local =
  let semantic_dimensions =
    Semantic_local_type_resolution.local_array_dimensions local
  in
  let expressions =
    match dimension_expressions with
    | Some expressions -> expressions
    | None -> List.map (fun _ -> Frame.Empty_dimension) semantic_dimensions
  in
  if List.length semantic_dimensions <> List.length expressions then
    Alcotest.fail
      "test setup supplied the wrong number of dimension expressions";
  let dimensions =
    List.map2
      (fun dimension expression ->
        ({
           dimension;
           expression_origin =
             Semantic_local_type_resolution.array_dimension_expression_origin
               dimension;
           expression;
         }
          : Frame.dimension_input))
      semantic_dimensions expressions
  in
  ({ local; dimensions } : Frame.local_input)

let core_input ?typed_function ?local_function ?locals prepared name =
  let indexed_function = indexed_function_named prepared name in
  let typed_function =
    Option.value typed_function ~default:(typed_function_named prepared name)
  in
  let local_function =
    Option.value local_function ~default:(local_function_named prepared name)
  in
  let locals =
    match locals with
    | Some locals -> locals
    | None ->
        Semantic_local_type_resolution.function_locals local_function
        |> List.map local_input
  in
  ({ indexed_function; typed_function; local_function; locals }
    : Frame.function_input)

let core_layout prepared inputs =
  Frame.layout
    ~table:(Session.semantic_symbols prepared.session)
    ~parent:(Semantic_declaration_collection.scope prepared.declarations)
    ~aggregate_layouts:prepared.aggregate_layouts inputs

let expect_frame_error expected_code predicate = function
  | Ok _ ->
      Alcotest.failf "expected %s without a partial frame layout" expected_code
  | Error error ->
      Alcotest.(check string)
        "stable frame error code" expected_code (Frame.error_code error);
      Alcotest.(check bool)
        "specific frame error kind" true
        (predicate (Frame.error_kind error));
      Alcotest.(check bool)
        "diagnostic includes its code" true
        (String.starts_with ~prefix:(expected_code ^ ": ")
           (Frame.error_to_string error))

let invalid_input_error = function
  | Frame.Invalid_input _ -> true
  | _ -> false

let malformed_foreign_function_and_binding_evidence () =
  let prepared =
    prepare ~path:"frame-malformed.HC"
      "U0 First(){I8 a;I64 b;}U0 Second(){I16 value;}"
  in
  let first = core_input prepared "First" in
  let second = core_input prepared "Second" in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared [ first; first ]);
  let missing_locals = { first with locals = [] } in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared [ missing_locals ]);
  let duplicated_location =
    match first.locals with
    | first_local :: _ -> { first with locals = [ first_local; first_local ] }
    | [] -> Alcotest.fail "test setup expected local evidence"
  in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared [ duplicated_location ]);
  let reversed_bindings = { first with locals = List.rev first.locals } in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared [ reversed_bindings ]);
  let mismatched_function =
    { first with typed_function = second.typed_function }
  in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared [ mismatched_function ]);
  let foreign = prepare ~path:"frame-foreign.HC" "U0 Foreign(){I8 value;}" in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared [ core_input foreign "Foreign" ]);
  expect_frame_error "HCSEMA0069" invalid_input_error
    (Frame.layout
       ~table:(Session.semantic_symbols prepared.session)
       ~parent:(Semantic_declaration_collection.scope prepared.declarations)
       ~aggregate_layouts:foreign.aggregate_layouts [ first ])

let rebuild_local ?declaration_index ?declarator_index ?storage ?delimiter local
    =
  let module Local = Semantic_local_type_resolution in
  let storage = Option.value storage ~default:(Local.local_storage local) in
  let declaration_index =
    Option.value declaration_index
      ~default:(Local.local_declaration_index local)
  in
  let declarator_index =
    Option.value declarator_index ~default:(Local.local_declarator_index local)
  in
  let storage_origins, register_requests =
    match storage with
    | Local.Automatic -> ([], Local.local_register_requests local)
    | Local.Static -> ([ Local.local_declaration_origin local ], [])
  in
  Local.make_local ~symbol:(Local.local_symbol local) ~declaration_index
    ~declarator_index
    ~declaration_origin:(Local.local_declaration_origin local)
    ~declarator_origin:(Local.local_declarator_origin local)
    ~storage ~storage_origins
    ~type_reference:(Local.local_type_reference local)
    ~register_requests
    ~declarator_kind:(Local.local_declarator_kind local)
    ~array_dimensions:(Local.local_array_dimensions local)
    ~initial_value:(Local.local_initializer local)
    ~delimiter:(Option.value delimiter ~default:(Local.local_delimiter local))
    ()
  |> checked

let resolved_local_function prepared name locals =
  let module Local = Semantic_local_type_resolution in
  let original = local_function_named prepared name in
  let declaration =
    Local.make_function
      ~symbol:(Local.function_symbol original)
      ~scope:(Local.function_scope original)
      ~item_index:(Local.function_item_index original)
      locals
    |> checked
  in
  Local.resolve
    ~table:(Session.semantic_symbols prepared.session)
    ~parent:(Semantic_declaration_collection.scope prepared.declarations)
    [ declaration ]
  |> checked |> Local.functions |> List.hd

let storage_and_source_position_mismatches () =
  let storage_prepared =
    prepare ~path:"frame-storage-mismatch.HC" "U0 Storage(){I8 value;}"
  in
  let original_function = local_function_named storage_prepared "Storage" in
  let original = local_named original_function "value" in
  let forged_static =
    rebuild_local ~storage:Semantic_local_type_resolution.Static original
  in
  let forged_function =
    resolved_local_function storage_prepared "Storage" [ forged_static ]
  in
  let forged_input =
    core_input ~local_function:forged_function
      ~locals:[ local_input forged_static ]
      storage_prepared "Storage"
  in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout storage_prepared [ forged_input ]);
  let position_prepared =
    prepare ~path:"frame-position-mismatch.HC" "U0 Position(){I8 a,b;}"
  in
  let position_function = local_function_named position_prepared "Position" in
  let a = local_named position_function "a" in
  let b = local_named position_function "b" in
  let a_delimiter =
    Semantic_local_type_resolution.make_delimiter
      ~kind:Semantic_local_type_resolution.Semicolon
      ~origin:
        (a |> Semantic_local_type_resolution.local_delimiter
       |> Semantic_local_type_resolution.delimiter_origin)
  in
  let forged_a = rebuild_local ~delimiter:a_delimiter a in
  let forged_b = rebuild_local ~declaration_index:1 ~declarator_index:0 b in
  let forged_function =
    resolved_local_function position_prepared "Position" [ forged_a; forged_b ]
  in
  let forged_input =
    core_input ~local_function:forged_function
      ~locals:[ local_input forged_a; local_input forged_b ]
      position_prepared "Position"
  in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout position_prepared [ forged_input ])

let malformed_dimensions_overflow_and_incomplete_aggregates () =
  let prepared =
    prepare ~path:"frame-dimension-errors.HC" "U0 Arrays(){I64 values[1];}"
  in
  let local_function = local_function_named prepared "Arrays" in
  let values = local_named local_function "values" in
  let dimension =
    values |> Semantic_local_type_resolution.local_array_dimensions |> List.hd
  in
  let origin =
    Semantic_local_type_resolution.array_dimension_origin dimension
  in
  let input expression =
    core_input
      ~locals:[ local_input ~dimension_expressions:[ expression ] values ]
      prepared "Arrays"
  in
  expect_frame_error "HCSEMA0070"
    (function
      | Frame.Unresolved_local_extent _ -> true
      | _ -> false)
    (core_layout prepared
       [
         input
           (Frame.Closed_expression
              (dependency_expression origin "unresolved extent"));
       ]);
  expect_frame_error "HCSEMA0071"
    (function
      | Frame.Non_integral_local_extent _ -> true
      | _ -> false)
    (core_layout prepared
       [
         input
           (Frame.Non_integral_expression { detail = "floating extent"; origin });
       ]);
  expect_frame_error "HCSEMA0072"
    (function
      | Frame.Invalid_local_extent _ -> true
      | _ -> false)
    (core_layout prepared
       [ input (Frame.Closed_expression (integer_expression origin (-1L))) ]);
  let missing_dimensions =
    core_input
      ~locals:[ ({ local = values; dimensions = [] } : Frame.local_input) ]
      prepared "Arrays"
  in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared [ missing_dimensions ]);
  let mismatched_expression_origin =
    ({
       local = values;
       dimensions =
         [
           {
             dimension;
             expression_origin =
               Some (Semantic_symbol.Synthesized "foreign extent expression");
             expression = Frame.Closed_expression (integer_expression origin 1L);
           };
         ];
     }
      : Frame.local_input)
  in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared
       [ core_input ~locals:[ mismatched_expression_origin ] prepared "Arrays" ]);
  expect_frame_error "HCSEMA0073"
    (function
      | Frame.Metadata_overflow _ -> true
      | _ -> false)
    (core_layout prepared
       [
         input
           (Frame.Closed_expression (integer_expression origin Int64.max_int));
       ]);
  let foreign_prepared =
    prepare ~path:"frame-foreign-dimension.HC" "U0 Foreign(){I64 values[1];}"
  in
  let foreign_dimension =
    local_function_named foreign_prepared "Foreign" |> fun function_ ->
    local_named function_ "values"
    |> Semantic_local_type_resolution.local_array_dimensions |> List.hd
  in
  let foreign_dimension_input =
    ({
       local = values;
       dimensions =
         [
           {
             dimension = foreign_dimension;
             expression_origin = Some origin;
             expression = Frame.Closed_expression (integer_expression origin 1L);
           };
         ];
     }
      : Frame.local_input)
  in
  expect_frame_error "HCSEMA0069" invalid_input_error
    (core_layout prepared
       [ core_input ~locals:[ foreign_dimension_input ] prepared "Arrays" ]);
  let first_empty =
    prepare ~path:"frame-first-empty-dimension.HC"
      "U0 FirstEmpty(){I64 values[];}"
  in
  let first_empty_frame =
    first_empty |> layout |> fun frames -> frame_named frames "FirstEmpty"
  in
  let first_empty_values = location_named first_empty_frame "values" in
  Alcotest.(check (list int64))
    "first empty extent is retained as zero" [ 0L ]
    (first_empty_values |> Frame.location_dimensions
    |> List.map Frame.dimension_value);
  Alcotest.(check int64)
    "first empty extent has zero allocation" 0L
    (Frame.location_allocated_size first_empty_values);
  Alcotest.(check int64)
    "first empty extent does not grow the frame" 0L
    (Frame.function_frame_size first_empty_frame);
  let later_empty =
    prepare ~path:"frame-later-empty-evidence.HC"
      "U0 LaterEmpty(){I64 values[2][3];}"
  in
  let later_function = local_function_named later_empty "LaterEmpty" in
  let later_values = local_named later_function "values" in
  let later_dimensions =
    Semantic_local_type_resolution.local_array_dimensions later_values
  in
  let expression_for index dimension =
    if index = 0 then
      Frame.Closed_expression
        (integer_expression
           (Semantic_local_type_resolution.array_dimension_origin dimension)
           2L)
    else Frame.Empty_dimension
  in
  let later_input =
    local_input
      ~dimension_expressions:(List.mapi expression_for later_dimensions)
      later_values
  in
  expect_frame_error "HCSEMA0072"
    (function
      | Frame.Invalid_local_extent _ -> true
      | _ -> false)
    (core_layout later_empty
       [ core_input ~locals:[ later_input ] later_empty "LaterEmpty" ]);
  let incomplete =
    prepare ~path:"frame-incomplete-aggregate.HC"
      "extern class Later;U0 Incomplete(){Later value;}"
  in
  let result =
    Holyc_lib.layout_function_frames incomplete.session
      ~declarations:incomplete.declarations ~bindings:incomplete.bindings
      ~function_types:incomplete.function_types
      ~local_types:incomplete.local_types
      ~aggregate_layouts:incomplete.aggregate_layouts incomplete.ast
  in
  Alcotest.(check bool)
    "driver rejects incomplete aggregate without a layout" true
    (match result with
    | Error message -> String.starts_with ~prefix:"HCSEMA0074: " message
    | Ok _ -> false)

let final_value_integrality_for_floating_extents () =
  let accepted =
    prepare ~path:"frame-integral-floating-extents.HC"
      "U0 FloatingLiteral(){I8 values[2.0];}\n\
       U0 FloatingComparison(){I8 values[1.5 < 2.0];}\n\
       U0 IntegralPower(){I8 values[2`3];}"
  in
  let frames = layout accepted in
  let literal =
    frame_named frames "FloatingLiteral" |> fun frame ->
    location_named frame "values"
  in
  let comparison =
    frame_named frames "FloatingComparison" |> fun frame ->
    location_named frame "values"
  in
  let power =
    frame_named frames "IntegralPower" |> fun frame ->
    location_named frame "values"
  in
  Alcotest.(check (list int64))
    "integral floating literal extent" [ 2L ]
    (literal |> Frame.location_dimensions |> List.map Frame.dimension_value);
  Alcotest.(check int64)
    "integral floating literal allocation" 2L
    (Frame.location_allocated_size literal);
  Alcotest.(check (list int64))
    "floating comparison result extent" [ 1L ]
    (comparison |> Frame.location_dimensions |> List.map Frame.dimension_value);
  Alcotest.(check int64)
    "floating comparison allocation" 1L
    (Frame.location_allocated_size comparison);
  Alcotest.(check (list int64))
    "integral power result extent" [ 8L ]
    (power |> Frame.location_dimensions |> List.map Frame.dimension_value);
  let power_size = Frame.location_allocated_size power in
  Alcotest.(check int64) "integral power allocation" 8L power_size;
  let expect_nonintegral ~path source description =
    let prepared = prepare ~path source in
    Alcotest.(check bool)
      description true
      (match
         Holyc_lib.layout_function_frames prepared.session
           ~declarations:prepared.declarations ~bindings:prepared.bindings
           ~function_types:prepared.function_types
           ~local_types:prepared.local_types
           ~aggregate_layouts:prepared.aggregate_layouts prepared.ast
       with
      | Error message -> String.starts_with ~prefix:"HCSEMA0071: " message
      | Ok _ -> false)
  in
  expect_nonintegral ~path:"frame-fractional-literal.HC"
    "U0 FractionalLiteral(){I8 values[1.5];}"
    "fractional floating literal is nonintegral";
  expect_nonintegral ~path:"frame-fractional-power.HC"
    "U0 FractionalPower(){I8 values[2`-1];}"
    "all-integer fractional power is nonintegral"

let tests =
  [
    Alcotest.test_case "fixed +16/+24/+32 parameters and variadic slots" `Quick
      fixed_parameters_and_variadic_slots;
    Alcotest.test_case "1/2/4/8/>8 local alignment and I8/I32/I64 sequence"
      `Quick local_size_alignment_classes;
    Alcotest.test_case
      "primitive, pointer, callback, class, union, and array sizes" `Quick
      primitive_pointer_callback_aggregate_union_and_array_sizes;
    Alcotest.test_case
      "mixed declarators, reg gaps, static storage, and zero size" `Quick
      mixed_declarators_register_gaps_static_and_zero_size;
    Alcotest.test_case "exact same-spelling identities and source order" `Quick
      exact_identity_lookup_and_source_order;
    Alcotest.test_case
      "deterministic empty/parameter/local/mixed accessors and dumps" `Quick
      deterministic_accessors_and_dumps;
    Alcotest.test_case "malformed function and binding evidence" `Quick
      malformed_foreign_function_and_binding_evidence;
    Alcotest.test_case "storage and source-position mismatches" `Quick
      storage_and_source_position_mismatches;
    Alcotest.test_case
      "dimension mismatches, unresolved and nonintegral extents, overflow, and \
       incomplete aggregates"
      `Quick malformed_dimensions_overflow_and_incomplete_aggregates;
    Alcotest.test_case "final-value integrality for floating extents" `Quick
      final_value_integrality_for_floating_extents;
  ]
