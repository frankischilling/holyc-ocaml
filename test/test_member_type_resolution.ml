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

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned_lines path ~first ~last =
  [ "third_party/TempleOS"; "../third_party/TempleOS" ]
  |> List.map (fun root -> Filename.concat root path)
  |> List.find_opt Sys.file_exists
  |> function
  | None -> Alcotest.failf "pinned source is unavailable: %s" path
  | Some source ->
      read_file source |> String.split_on_char '\n'
      |> List.filteri (fun index _ -> index + 1 >= first && index + 1 <= last)
      |> String.concat "\n"

type semantic_results = {
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  headers : Semantic_aggregate_header_resolution.t;
  members : Semantic_member_collection.t;
  member_types : Semantic_member_type_resolution.t;
}

let resolve session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  let members = checked (Holyc_lib.collect_members session ~declarations ast) in
  let member_types =
    checked
      (Holyc_lib.resolve_member_types session ~declarations ~aggregates ~headers
         ~members ast)
  in
  { declarations; aggregates; headers; members; member_types }

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let aggregate_named results name =
  Semantic_member_type_resolution.aggregates results.member_types
  |> List.find (fun aggregate ->
      aggregate |> Semantic_member_type_resolution.aggregate_symbol
      |> Semantic_symbol.name |> String.equal name)

let member_named aggregate name =
  Semantic_member_type_resolution.aggregate_members aggregate
  |> List.find (fun member ->
      member |> Semantic_member_type_resolution.member_symbol
      |> Semantic_symbol.name |> String.equal name)

let aggregate_at results item_index =
  Semantic_member_type_resolution.aggregates results.member_types
  |> List.find (fun aggregate ->
      Semantic_member_type_resolution.aggregate_item_index aggregate
      = item_index)

let member_names aggregate =
  Semantic_member_type_resolution.aggregate_members aggregate
  |> List.map (fun member ->
      member |> Semantic_member_type_resolution.member_symbol
      |> Semantic_symbol.name)

let member_type member =
  member |> Semantic_member_type_resolution.member_type_reference
  |> Semantic_member_type_resolution.type_reference_type

let expect_callback member =
  match Semantic_member_type_resolution.member_declarator_kind member with
  | Semantic_member_type_resolution.Function_pointer pointer -> pointer
  | Semantic_member_type_resolution.Object ->
      Alcotest.fail "expected a callback member"

let check_member_flags ~callback member =
  let expected =
    if callback then Member_flag.to_mask Member_flag.Function_pointer else 0L
  in
  Alcotest.(check int64)
    "member-list flag mask" expected
    (Semantic_member_type_resolution.member_flag_mask member);
  Member_flag.all
  |> List.iter (fun flag ->
      Alcotest.(check bool)
        (Member_flag.to_source_name flag)
        (callback && flag = Member_flag.Function_pointer)
        (Semantic_member_type_resolution.member_has_flag member flag))

let callback_parameters pointer =
  pointer |> Semantic_member_type_resolution.function_pointer_signature
  |> Semantic_function_type_resolution.signature_parameters

let parameter_type parameter =
  parameter |> Semantic_function_type_resolution.parameter_type_reference
  |> Semantic_type_reference.resolved_type

let aggregate_target member =
  match member |> member_type |> Semantic_type.base with
  | Semantic_type.Aggregate symbol -> symbol
  | Semantic_type.Primitive _ -> Alcotest.fail "expected an aggregate type"

let check_primitive ~form ~primitive ~pointer_depth member =
  let type_ = member_type member in
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

let primitive_intrinsic_and_grouped_pointers () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-primitives.HC"
      "class Fields { I64 plain, *one, **two, ***three, ****four; I64i \
       storage; }; union Choice { U64 wide; U8 byte; };"
  in
  let results = resolve session ast in
  let fields = aggregate_named results "Fields" in
  Alcotest.(check (list string))
    "member order"
    [ "plain"; "one"; "two"; "three"; "four"; "storage" ]
    (member_names fields);
  [ "plain"; "one"; "two"; "three"; "four" ]
  |> List.iteri (fun pointer_depth name ->
      let member = member_named fields name in
      check_primitive ~form:Semantic_type.Public_spelling
        ~primitive:Primitive_type.I64 ~pointer_depth member;
      Alcotest.(check int)
        (name ^ " pointer origins")
        pointer_depth
        (member |> Semantic_member_type_resolution.member_type_reference
       |> Semantic_member_type_resolution.type_reference_pointer_origins
       |> List.length));
  let storage = member_named fields "storage" in
  check_primitive ~form:Semantic_type.Internal_storage
    ~primitive:Primitive_type.I64 ~pointer_depth:0 storage;
  Alcotest.(check string)
    "intrinsic spelling" "I64i"
    (storage |> Semantic_member_type_resolution.member_type_reference
   |> Semantic_member_type_resolution.type_reference_spelling);
  let choice = aggregate_named results "Choice" in
  Alcotest.(check (list string))
    "union members" [ "wide"; "byte" ] (member_names choice);
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.U64 ~pointer_depth:0
    (member_named choice "wide");
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.U8 ~pointer_depth:0
    (member_named choice "byte")

let identity_with_first_item results item_index =
  Semantic_aggregate_resolution.identities results.aggregates
  |> List.find (fun identity ->
      Semantic_aggregate_resolution.identity_first_item_index identity
      = item_index)
  |> Semantic_aggregate_resolution.identity_symbol

let self_forward_and_shadowed_types () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-named-types.HC"
      "class First {}; extern class Later; class Holder { First first; Later \
       *later; Holder *self; }; class Later {}; class First {};"
  in
  let results = resolve session ast in
  let holder = aggregate_named results "Holder" in
  let first_target = member_named holder "first" |> aggregate_target in
  let later_target = member_named holder "later" |> aggregate_target in
  let self_target = member_named holder "self" |> aggregate_target in
  Alcotest.(check int)
    "an earlier definition remains visible"
    (identity_with_first_item results 0 |> symbol_id)
    (symbol_id first_target);
  Alcotest.(check int)
    "a forward use keeps the identity completed later"
    (identity_with_first_item results 1 |> symbol_id)
    (symbol_id later_target);
  Alcotest.(check int)
    "the current aggregate is visible to its members"
    (identity_with_first_item results 2 |> symbol_id)
    (symbol_id self_target);
  Alcotest.(check bool)
    "the later same-name definition does not capture the earlier use" true
    (not
       (Semantic_symbol.Id.equal
          (Semantic_symbol.id first_target)
          (identity_with_first_item results 4 |> Semantic_symbol.id)))

let repeated_forward_uses_newest_identity () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-repeated-forwards.HC"
      "extern class Node; extern class Node; class Box { Node *node; }; class \
       Node {};"
  in
  let results = resolve session ast in
  let target =
    aggregate_named results "Box" |> fun aggregate ->
    member_named aggregate "node" |> aggregate_target
  in
  let identities =
    Semantic_aggregate_resolution.identities results.aggregates
    |> List.filter (fun identity ->
        identity |> Semantic_aggregate_resolution.identity_symbol
        |> Semantic_symbol.name |> String.equal "Node")
  in
  let older = List.nth identities 0 in
  let newest = List.nth identities 1 in
  Alcotest.(check int)
    "the newest forward is selected"
    (newest |> Semantic_aggregate_resolution.identity_symbol |> symbol_id)
    (symbol_id target);
  Alcotest.(check bool)
    "the selected forward is completed" true
    (newest |> Semantic_aggregate_resolution.identity_definition
   |> Option.is_some);
  Alcotest.(check bool)
    "the older forward remains unresolved" true
    (older |> Semantic_aggregate_resolution.identity_definition
   |> Option.is_none)

let members_use_postpublication_identity () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-publication.HC"
      "class Word {}; Word class Word { Word *self; };"
  in
  let results = resolve session ast in
  let headers = Semantic_aggregate_header_resolution.headers results.headers in
  let second_header = List.nth headers 1 in
  let backing =
    match Semantic_aggregate_header_resolution.header_backing second_header with
    | Some backing -> backing
    | None -> Alcotest.fail "expected a same-name aggregate backing"
  in
  let backing_target =
    match
      backing |> Semantic_aggregate_header_resolution.backing_type
      |> Semantic_type.base
    with
    | Semantic_type.Aggregate symbol -> symbol
    | Semantic_type.Primitive _ ->
        Alcotest.fail "expected an aggregate backing target"
  in
  let self_target =
    aggregate_at results 1 |> fun aggregate ->
    member_named aggregate "self" |> aggregate_target
  in
  Alcotest.(check int)
    "the backing uses the older prepublication identity"
    (identity_with_first_item results 0 |> symbol_id)
    (symbol_id backing_target);
  Alcotest.(check int)
    "the member uses the current postpublication identity"
    (identity_with_first_item results 1 |> symbol_id)
    (symbol_id self_target)

let anonymous_union_paths_and_arrays () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-arrays.HC"
      "class Shape { union { I64 scalar; union { U8 bytes[4][2], flexible[]; \
       }; }; };"
  in
  let results = resolve session ast in
  let shape = aggregate_named results "Shape" in
  Alcotest.(check (list string))
    "anonymous unions keep flattened source order"
    [ "scalar"; "bytes"; "flexible" ]
    (member_names shape);
  let paths =
    Semantic_member_type_resolution.aggregate_members shape
    |> List.map Semantic_member_type_resolution.member_path
  in
  Alcotest.(check (list (list int)))
    "anonymous-union paths"
    [ [ 0; 0 ]; [ 0; 1; 0 ]; [ 0; 1; 0 ] ]
    paths;
  let bytes = member_named shape "bytes" in
  let flexible = member_named shape "flexible" in
  Alcotest.(check int)
    "two array dimensions are retained" 2
    (Semantic_member_type_resolution.member_array_dimension_origins bytes
    |> List.length);
  Alcotest.(check int)
    "an omitted first extent is retained as one dimension" 1
    (Semantic_member_type_resolution.member_array_dimension_origins flexible
    |> List.length)

let member_flag_masks () =
  let signatures =
    [ Preprocessor.Jit; Preprocessor.Aot ]
    |> List.map (fun mode ->
        let session = Session.create () in
        let ast =
          parse session ~mode ~path:"member-flags.HC"
            "class Flags { I64 value,(*direct)(I64 input=1); union { U8 byte; \
             U0 (*nested)(I64 value,...); }; };"
        in
        let aggregate =
          resolve session ast |> fun result -> aggregate_named result "Flags"
        in
        let facts =
          [
            ("value", false); ("direct", true); ("byte", false); ("nested", true);
          ]
          |> List.map (fun (name, callback) ->
              let member = member_named aggregate name in
              check_member_flags ~callback member;
              (name, Semantic_member_type_resolution.member_flag_mask member))
        in
        facts)
  in
  Alcotest.(check (list (pair string int64)))
    "JIT and AOT member masks agree" (List.hd signatures)
    (List.nth signatures 1)

let callback_return_and_indirection () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-callbacks.HC"
      "class Node {}; class Callbacks { I64 *(*invoke)(Node \
       *node,I64=1,I64=lastclass,U8 *(*nested)(reg R10 noreg I64 \
       value=\"nested\",...),...); I64 (**chain)(I64 value); };"
  in
  let results = resolve session ast in
  let node = aggregate_named results "Node" in
  let callbacks = aggregate_named results "Callbacks" in
  let invoke = member_named callbacks "invoke" in
  let chain = member_named callbacks "chain" in
  check_member_flags ~callback:true invoke;
  check_member_flags ~callback:true chain;
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.I64 ~pointer_depth:1 invoke;
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.I64 ~pointer_depth:0 chain;
  let callback_depth member =
    member |> expect_callback
    |> Semantic_member_type_resolution.function_pointer_indirection_origins
    |> List.length
  in
  Alcotest.(check int)
    "return pointer is separate from callback indirection" 1
    (callback_depth invoke);
  Alcotest.(check int)
    "callback indirection retains both stars" 2 (callback_depth chain);
  let invoke_pointer = expect_callback invoke in
  let signature =
    Semantic_member_type_resolution.function_pointer_signature invoke_pointer
  in
  Alcotest.(check (list (option string)))
    "callback slots retain names and gaps"
    [ Some "node"; None; None; Some "nested" ]
    (signature |> Semantic_function_type_resolution.signature_parameters
    |> List.map Semantic_function_type_resolution.parameter_name);
  Alcotest.(check bool)
    "outer callback keeps its terminal ellipsis" true
    (signature |> Semantic_function_type_resolution.signature_variadic_origin
   |> Option.is_some);
  let parameters =
    Semantic_function_type_resolution.signature_parameters signature
  in
  Alcotest.(check (list int64))
    "member callback parameters retain source-derived masks"
    [ 0L; 0x21L; 0x23L; 0x8L ]
    (parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  let node_type = List.nth parameters 0 |> parameter_type in
  Alcotest.(check int)
    "self parameter pointer depth" 1
    (Semantic_type.pointer_depth node_type);
  (match Semantic_type.base node_type with
  | Semantic_type.Aggregate symbol ->
      Alcotest.(check int)
        "self parameter uses the canonical identity"
        (node |> Semantic_member_type_resolution.aggregate_symbol |> symbol_id)
        (symbol_id symbol)
  | Semantic_type.Primitive _ -> Alcotest.fail "expected a Node parameter");
  (match
     List.nth parameters 1
     |> Semantic_function_type_resolution.parameter_default
   with
  | Some (Semantic_function_type_resolution.Expression_default _) -> ()
  | None | Some (Semantic_function_type_resolution.Lastclass_default _) ->
      Alcotest.fail "expected a non-trailing expression default");
  (match
     List.nth parameters 2
     |> Semantic_function_type_resolution.parameter_default
   with
  | Some (Semantic_function_type_resolution.Lastclass_default _) -> ()
  | None | Some (Semantic_function_type_resolution.Expression_default _) ->
      Alcotest.fail "expected a non-trailing lastclass default");
  let nested =
    match
      List.nth parameters 3
      |> Semantic_function_type_resolution.parameter_declarator_kind
    with
    | Semantic_function_type_resolution.Function_pointer pointer -> pointer
    | Semantic_function_type_resolution.Object ->
        Alcotest.fail "expected a nested callback"
  in
  Alcotest.(check int)
    "nested return pointer remains outside its callback indirection" 1
    (List.nth parameters 3 |> parameter_type |> Semantic_type.pointer_depth);
  Alcotest.(check int)
    "nested callback indirection" 1
    (nested
   |> Semantic_function_type_resolution.function_pointer_indirection_origins
   |> List.length);
  let nested_signature =
    Semantic_function_type_resolution.function_pointer_signature nested
  in
  Alcotest.(check (list (option string)))
    "nested callback parameter" [ Some "value" ]
    (nested_signature |> Semantic_function_type_resolution.signature_parameters
    |> List.map Semantic_function_type_resolution.parameter_name);
  Alcotest.(check (list int64))
    "nested member callback keeps its string-default mask" [ 0x5L ]
    (nested_signature |> Semantic_function_type_resolution.signature_parameters
    |> List.map Semantic_function_type_resolution.parameter_flag_mask);
  let nested_parameter =
    nested_signature |> Semantic_function_type_resolution.signature_parameters
    |> List.hd
  in
  Alcotest.(check (list int))
    "nested member callback keeps ordered register requests" [ 10; 32 ]
    (nested_parameter
   |> Semantic_function_type_resolution.parameter_register_requests
    |> List.map (fun request ->
        Semantic_register_request.effective [ request ]
        |> Semantic_register_request.source_code));
  Alcotest.(check int)
    "nested member callback uses the last register request" 32
    (nested_parameter
   |> Semantic_function_type_resolution.parameter_register_selection
   |> Semantic_register_request.source_code);
  Alcotest.(check bool)
    "nested callback keeps its terminal ellipsis" true
    (nested_signature
   |> Semantic_function_type_resolution.signature_variadic_origin
   |> Option.is_some);
  [
    Semantic_member_type_resolution.function_pointer_opening_origin
      invoke_pointer;
    Semantic_member_type_resolution.function_pointer_closing_origin
      invoke_pointer;
    Semantic_function_type_resolution.signature_opening_origin signature;
    Semantic_function_type_resolution.signature_closing_origin signature;
  ]
  |> List.iter (function
    | Semantic_symbol.Source_location _ -> ()
    | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
        Alcotest.fail "expected callback punctuation to retain source origins")

let callback_parameter_pointer_depths_and_visibility () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-callback-parameter-depths.HC"
      "class Earlier {}; class Depths { U0 (*visit)(I64 zero,I64 *one,I64 \
       **two,I64 ***three,I64 ****four,Earlier *earlier,Depths *self); };"
  in
  let results = resolve session ast in
  let earlier = aggregate_named results "Earlier" in
  let depths = aggregate_named results "Depths" in
  let parameters =
    member_named depths "visit" |> expect_callback |> callback_parameters
  in
  Alcotest.(check (list int))
    "ordinary callback parameter pointer depths" [ 0; 1; 2; 3; 4 ]
    (parameters
    |> List.filteri (fun index _ -> index < 5)
    |> List.map (fun parameter ->
        parameter |> parameter_type |> Semantic_type.pointer_depth));
  let check_identity index aggregate =
    match List.nth parameters index |> parameter_type |> Semantic_type.base with
    | Semantic_type.Aggregate symbol ->
        Alcotest.(check int)
          "callback parameter aggregate identity"
          (aggregate |> Semantic_member_type_resolution.aggregate_symbol
         |> symbol_id)
          (symbol_id symbol)
    | Semantic_type.Primitive _ ->
        Alcotest.fail "expected an aggregate callback parameter"
  in
  check_identity 5 earlier;
  check_identity 6 depths

let pinned_callback_member_signatures () =
  let check_aggregate_parameter parameter ~name ~pointer_depth =
    let type_ = parameter_type parameter in
    Alcotest.(check int)
      (name ^ " pointer depth") pointer_depth
      (Semantic_type.pointer_depth type_);
    match Semantic_type.base type_ with
    | Semantic_type.Aggregate symbol ->
        Alcotest.(check string)
          (name ^ " aggregate") name
          (Semantic_symbol.name symbol)
    | Semantic_type.Primitive _ ->
        Alcotest.failf "expected %s to be an aggregate parameter" name
  in
  let check_primitive_parameter parameter ~primitive ~pointer_depth =
    let type_ = parameter_type parameter in
    Alcotest.(check int)
      "primitive parameter pointer depth" pointer_depth
      (Semantic_type.pointer_depth type_);
    match Semantic_type.base type_ with
    | Semantic_type.Primitive (form, actual) ->
        Alcotest.(check string)
          "primitive parameter form"
          (Semantic_type.primitive_form_name Semantic_type.Public_spelling)
          (Semantic_type.primitive_form_name form);
        Alcotest.(check string)
          "primitive parameter"
          (Primitive_type.to_string primitive)
          (Primitive_type.to_string actual)
    | Semantic_type.Aggregate _ ->
        Alcotest.fail "expected a primitive callback parameter"
  in
  let math_ode =
    "extern class CTask; extern class CMass; extern class CSpring;\n"
    ^ pinned_lines "Kernel/KernelA.HH" ~first:251 ~last:289
  in
  let doc_entry =
    "class CDocEntryBase {};\n\
     extern class CDoc;\n\
     extern class CTask;\n\
     extern class CDocBin;\n"
    ^ pinned_lines "Kernel/KernelA.HH" ~first:1191 ~last:1220
    ^ "\n"
    ^ pinned_lines "Kernel/KernelA.HH" ~first:1222 ~last:1222
  in
  let key_devices = pinned_lines "Kernel/KernelA.HH" ~first:3754 ~last:3771 in
  [ Preprocessor.Jit; Preprocessor.Aot ]
  |> List.iter (fun mode ->
      let math_session = Session.create () in
      let math =
        parse math_session ~mode ~path:"Kernel/KernelA.HH:251-289" math_ode
        |> resolve math_session
      in
      let math_class = aggregate_named math "CMathODE" in
      let derive_member = member_named math_class "derive" in
      check_member_flags ~callback:true derive_member;
      let derive_parameters =
        derive_member |> expect_callback |> callback_parameters
      in
      Alcotest.(check (list (option string)))
        "CMathODE.derive parameter names"
        [ Some "o"; Some "t"; Some "state"; Some "DstateDt" ]
        (List.map Semantic_function_type_resolution.parameter_name
           derive_parameters);
      check_aggregate_parameter
        (List.nth derive_parameters 0)
        ~name:"CMathODE" ~pointer_depth:1;
      check_primitive_parameter
        (List.nth derive_parameters 1)
        ~primitive:Primitive_type.F64 ~pointer_depth:0;
      [ 2; 3 ]
      |> List.iter (fun index ->
          check_primitive_parameter
            (List.nth derive_parameters index)
            ~primitive:Primitive_type.F64 ~pointer_depth:1);
      let mp_derive =
        member_named math_class "mp_derive"
        |> expect_callback |> callback_parameters
      in
      Alcotest.(check int)
        "CMathODE.mp_derive parameter count" 5 (List.length mp_derive);
      check_primitive_parameter (List.nth mp_derive 2)
        ~primitive:Primitive_type.I64 ~pointer_depth:0;
      let doc_session = Session.create () in
      let doc =
        parse doc_session ~mode ~path:"Kernel/KernelA.HH:1191-1222" doc_entry
        |> resolve doc_session
      in
      let doc_class = aggregate_named doc "CDocEntry" in
      let left_member = member_named doc_class "left_cb" in
      check_member_flags ~callback:true left_member;
      let left_parameters =
        left_member |> expect_callback |> callback_parameters
      in
      check_aggregate_parameter
        (List.nth left_parameters 0)
        ~name:"CDoc" ~pointer_depth:1;
      check_aggregate_parameter
        (List.nth left_parameters 1)
        ~name:"CDocEntry" ~pointer_depth:1;
      let tag_member = member_named doc_class "tag_cb" in
      check_member_flags ~callback:true tag_member;
      check_primitive ~form:Semantic_type.Public_spelling
        ~primitive:Primitive_type.U8 ~pointer_depth:1 tag_member;
      let tag_parameters =
        tag_member |> expect_callback |> callback_parameters
      in
      check_aggregate_parameter
        (List.nth tag_parameters 2)
        ~name:"CTask" ~pointer_depth:1;
      let key_session = Session.create () in
      let key =
        parse key_session ~mode ~path:"Kernel/KernelA.HH:3754-3771" key_devices
        |> resolve key_session
      in
      let callbacks = aggregate_named key "CKeyDevGlbls" in
      let control_member = member_named callbacks "fp_ctrl_alt_cbs" in
      check_member_flags ~callback:true control_member;
      let control = expect_callback control_member in
      Alcotest.(check int)
        "key callback indirection" 2
        (control
       |> Semantic_member_type_resolution.function_pointer_indirection_origins
       |> List.length);
      let control_parameters = callback_parameters control in
      Alcotest.(check (list (option string)))
        "key callback parameter" [ Some "sc" ]
        (List.map Semantic_function_type_resolution.parameter_name
           control_parameters);
      check_primitive_parameter
        (List.hd control_parameters)
        ~primitive:Primitive_type.I64 ~pointer_depth:0)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_type_provenance () =
  let session = Session.create () in
  let ast =
    parse session ~path:"generated-member-type.HC"
      "#define STORAGE I64i\n\
       #define STAR *\n\
       #define ARG payload\n\
       class Box { STORAGE value; STORAGE (*visit)(STORAGE STAR ARG,...); };"
  in
  let box =
    resolve session ast |> fun results -> aggregate_named results "Box"
  in
  let member = member_named box "value" in
  let type_origin =
    member |> Semantic_member_type_resolution.member_type_reference
    |> Semantic_member_type_resolution.type_reference_spelling_origin
    |> source_origin
  in
  Alcotest.(check bool)
    "the macro invocation is retained" true
    (Option.is_some type_origin.generated_from);
  Alcotest.(check bool)
    "the definition site is retained" true
    (Option.is_some type_origin.defined_at);
  let parameter =
    let visit = member_named box "visit" in
    check_member_flags ~callback:true visit;
    visit |> expect_callback |> callback_parameters |> List.hd
  in
  let generated_origins =
    [
      parameter |> Semantic_function_type_resolution.parameter_type_reference
      |> Semantic_type_reference.spelling_origin;
      parameter |> Semantic_function_type_resolution.parameter_type_reference
      |> Semantic_type_reference.pointer_origins |> List.hd;
      parameter |> Semantic_function_type_resolution.parameter_name_origin
      |> Option.get;
    ]
  in
  generated_origins
  |> List.iter (fun origin ->
      let site = source_origin origin in
      Alcotest.(check bool)
        "generated signature spelling keeps its invocation" true
        (Option.is_some site.generated_from);
      Alcotest.(check bool)
        "generated signature spelling keeps its definition" true
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

let included_type_provenance () =
  let directory = Filename.temp_dir "holyc-member-types-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "members.HC" in
      write_file root_path "#include \"members\"";
      write_file include_path
        "class Included { I64 value; Included *(*copy)(Included *input); };";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let included =
        resolve session ast |> fun results -> aggregate_named results "Included"
      in
      let member = member_named included "value" in
      let site =
        member |> Semantic_member_type_resolution.member_type_reference
        |> Semantic_member_type_resolution.type_reference_spelling_origin
        |> source_origin
      in
      let source =
        Source_manager.find (Session.sources session) site.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "the member type keeps its included source" "members.HC"
        (Source_file.path source |> Filename.basename);
      let parameter_site =
        let copy = member_named included "copy" in
        check_member_flags ~callback:true copy;
        copy |> expect_callback |> callback_parameters |> List.hd
        |> Semantic_function_type_resolution.parameter_type_reference
        |> Semantic_type_reference.spelling_origin |> source_origin
      in
      let parameter_source =
        Source_manager.find (Session.sources session) parameter_site.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "the callback parameter keeps its included source" "members.HC"
        (Source_file.path parameter_source |> Filename.basename))

let member_signature member =
  let type_text type_ =
    let base =
      match Semantic_type.base type_ with
      | Semantic_type.Primitive (form, primitive) ->
          Printf.sprintf "%s:%s"
            (Semantic_type.primitive_form_name form)
            (Primitive_type.to_string primitive)
      | Semantic_type.Aggregate symbol ->
          Printf.sprintf "aggregate:%s:%d"
            (Semantic_symbol.name symbol)
            (symbol_id symbol)
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
          Printf.sprintf "callback:%d(%s)"
            (pointer
           |> Semantic_function_type_resolution
              .function_pointer_indirection_origins |> List.length)
            (pointer
           |> Semantic_function_type_resolution.function_pointer_signature
           |> Semantic_function_type_resolution.signature_parameters
           |> List.map parameter_text |> String.concat ",")
    in
    Printf.sprintf "%d:%s:%s:%s"
      (Semantic_function_type_resolution.parameter_index parameter)
      (Semantic_function_type_resolution.parameter_name parameter
      |> Option.value ~default:"_")
      (parameter |> parameter_type |> type_text)
      kind
  in
  let type_ = member_type member in
  let kind =
    match Semantic_member_type_resolution.member_declarator_kind member with
    | Semantic_member_type_resolution.Object -> "object"
    | Semantic_member_type_resolution.Function_pointer pointer ->
        Printf.sprintf "callback:%d(%s)"
          (pointer
         |> Semantic_member_type_resolution.function_pointer_indirection_origins
         |> List.length)
          (pointer |> callback_parameters |> List.map parameter_text
         |> String.concat ",")
  in
  Printf.sprintf "%s:%s:%s:%d:0x%Lx"
    (member |> Semantic_member_type_resolution.member_symbol
   |> Semantic_symbol.name)
    (type_text type_) kind
    (Semantic_member_type_resolution.member_array_dimension_origins member
    |> List.length)
    (Semantic_member_type_resolution.member_flag_mask member)

let modes_determinism_and_purity () =
  let signatures =
    [ Preprocessor.Jit; Preprocessor.Aot ]
    |> List.map (fun mode ->
        let session = Session.create () in
        let ast =
          parse session ~mode ~path:"member-type-modes.HC"
            "class Node { Node *next; I64 (*visit)(Node *node); U8 bytes[3]; };"
        in
        let results = resolve session ast in
        let table = Session.semantic_symbols session in
        let scope_count =
          Semantic_symbol_table.all_scopes table |> List.length
        in
        let symbol_count =
          Semantic_symbol_table.all_symbols table |> List.length
        in
        let signature resolution =
          Semantic_member_type_resolution.aggregates resolution
          |> List.concat_map Semantic_member_type_resolution.aggregate_members
          |> List.map member_signature
        in
        let first = signature results.member_types in
        let second =
          checked
            (Holyc_lib.resolve_member_types session
               ~declarations:results.declarations ~aggregates:results.aggregates
               ~headers:results.headers ~members:results.members ast)
          |> signature
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
    "JIT and AOT member types agree" (List.nth signatures 0)
    (List.nth signatures 1)

type inputs = {
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  headers : Semantic_aggregate_header_resolution.t;
  members : Semantic_member_collection.t;
}

let inputs session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  let members = checked (Holyc_lib.collect_members session ~declarations ast) in
  { ast; declarations; aggregates; headers; members }

let resolve_inputs session values ast =
  Holyc_lib.resolve_member_types session ~declarations:values.declarations
    ~aggregates:values.aggregates ~headers:values.headers
    ~members:values.members ast

let mismatched_inputs_do_not_mutate () =
  let session = Session.create () in
  let first_ast =
    parse session ~path:"first-member-types.HC" "class A { I64 a; };"
  in
  let second_ast =
    parse session ~path:"second-member-types.HC" "class B { U8 b; };"
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
    (Holyc_lib.resolve_member_types session ~declarations:second.declarations
       ~aggregates:first.aggregates ~headers:first.headers
       ~members:first.members first.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "another aggregate reconciliation is rejected" true
    (Holyc_lib.resolve_member_types session ~declarations:first.declarations
       ~aggregates:second.aggregates ~headers:first.headers
       ~members:first.members first.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "another header resolution is rejected" true
    (Holyc_lib.resolve_member_types session ~declarations:first.declarations
       ~aggregates:first.aggregates ~headers:second.headers
       ~members:first.members first.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "another member collection is rejected" true
    (Holyc_lib.resolve_member_types session ~declarations:first.declarations
       ~aggregates:first.aggregates ~headers:first.headers
       ~members:second.members first.ast
    |> Result.is_error);
  let foreign_session = Session.create () in
  Alcotest.(check bool)
    "semantic inputs cannot cross sessions" true
    (resolve_inputs foreign_session first first.ast |> Result.is_error);
  let clone_session = Session.create () in
  let clone_ast =
    parse clone_session ~path:"first-member-types.HC" "class A { I64 a; };"
  in
  let clone = inputs clone_session clone_ast in
  Alcotest.(check bool)
    "colliding header IDs cannot cross sessions" true
    (Holyc_lib.resolve_member_types session ~declarations:first.declarations
       ~aggregates:first.aggregates ~headers:clone.headers
       ~members:first.members first.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "colliding member IDs cannot cross sessions" true
    (Holyc_lib.resolve_member_types session ~declarations:first.declarations
       ~aggregates:first.aggregates ~headers:first.headers
       ~members:clone.members first.ast
    |> Result.is_error);
  Alcotest.(check int)
    "rejected inputs create no scope" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "rejected inputs create no symbol" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let low_level_validation () =
  let synthesized text = Semantic_symbol.Synthesized text in
  let public_i64 pointer_depth =
    checked
      (Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
         ~primitive:Primitive_type.I64 ~pointer_depth)
  in
  let reference ?(spelling = "I64") ?(pointer_origins = []) type_ =
    Semantic_member_type_resolution.make_type_reference ~spelling
      ~spelling_origin:(synthesized "type spelling")
      ~pointer_origins ~resolved_type:type_
  in
  Alcotest.(check bool)
    "a mismatched spelling is rejected" true
    (reference ~spelling:"U64" (public_i64 0) |> Result.is_error);
  Alcotest.(check bool)
    "pointer provenance must match the type" true
    (reference (public_i64 1) |> Result.is_error);
  let empty_signature =
    checked
      (Semantic_function_type_resolution.make_signature
         ~opening_origin:(synthesized "signature opening")
         ~parameters:[]
         ~closing_origin:(synthesized "signature closing")
         ())
  in
  let make_pointer indirection_origins =
    Semantic_member_type_resolution.make_function_pointer
      ~origin:(synthesized "callback")
      ~opening_origin:(synthesized "declarator opening")
      ~indirection_origins
      ~closing_origin:(synthesized "declarator closing")
      ~signature:empty_signature
  in
  Alcotest.(check bool)
    "a callback requires indirection" true
    (make_pointer [] |> Result.is_error);
  Alcotest.(check bool)
    "a callback rejects a fifth pointer star" true
    (make_pointer
       (List.init 5 (fun index -> synthesized (Printf.sprintf "star %d" index)))
    |> Result.is_error);
  let table = Semantic_symbol_table.create () in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table
         ~parent:(Semantic_symbol_table.root table)
         ~kind:Semantic_symbol_table.Module ~name:"manual.HC" ())
  in
  let aggregate_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"Manual"
         ~kind:Semantic_symbol.Aggregate_type ~origin:(synthesized "aggregate"))
  in
  let aggregate_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:module_scope
         ~kind:Semantic_symbol_table.Aggregate ~name:"Manual" ())
  in
  let add_member name =
    checked
      (Semantic_symbol_table.add table ~scope:aggregate_scope ~name
         ~kind:Semantic_symbol.Member
         ~origin:(synthesized (name ^ " member")))
  in
  let type_reference = checked (reference (public_i64 0)) in
  let make_member ?(path = [ 0 ]) ?(declarator_index = 0) symbol =
    checked
      (Semantic_member_type_resolution.make_member ~symbol ~member_path:path
         ~declarator_index ~declarator_origin:(synthesized "declarator")
         ~type_reference ~declarator_kind:Semantic_member_type_resolution.Object
         ~array_dimension_origins:[])
  in
  let member = add_member "value" |> make_member in
  let aggregate =
    checked
      (Semantic_member_type_resolution.make_aggregate ~symbol:aggregate_symbol
         ~scope:aggregate_scope ~item_index:0 [ member ])
  in
  ignore
    (checked
       (Semantic_member_type_resolution.resolve ~table ~parent:module_scope
          [ aggregate ]));
  let global =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"global"
         ~kind:Semantic_symbol.Global_variable ~origin:(synthesized "global"))
  in
  Alcotest.(check bool)
    "a global cannot stand in for a member" true
    (Semantic_member_type_resolution.make_member ~symbol:global
       ~member_path:[ 0 ] ~declarator_index:0
       ~declarator_origin:(synthesized "declarator") ~type_reference
       ~declarator_kind:Semantic_member_type_resolution.Object
       ~array_dimension_origins:[]
    |> Result.is_error);
  let second = add_member "second" |> make_member ~path:[ 1 ] in
  let third = add_member "third" |> make_member ~path:[ 0 ] in
  let out_of_order =
    checked
      (Semantic_member_type_resolution.make_aggregate ~symbol:aggregate_symbol
         ~scope:aggregate_scope ~item_index:0 [ second; third ])
  in
  Alcotest.(check bool)
    "out-of-order member paths are rejected" true
    (Semantic_member_type_resolution.resolve ~table ~parent:module_scope
       [ out_of_order ]
    |> Result.is_error);
  let wrong_named_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:module_scope
         ~kind:Semantic_symbol_table.Aggregate ~name:"Wrong" ())
  in
  let wrong_named_aggregate =
    checked
      (Semantic_member_type_resolution.make_aggregate ~symbol:aggregate_symbol
         ~scope:wrong_named_scope ~item_index:0 [])
  in
  Alcotest.(check bool)
    "an aggregate scope must name its owner" true
    (Semantic_member_type_resolution.resolve ~table ~parent:module_scope
       [ wrong_named_aggregate ]
    |> Result.is_error);
  let duplicate =
    checked
      (Semantic_member_type_resolution.make_aggregate ~symbol:aggregate_symbol
         ~scope:aggregate_scope ~item_index:0
         [
           member;
           make_member ~path:[ 1 ]
             (Semantic_member_type_resolution.member_symbol member);
         ])
  in
  Alcotest.(check bool)
    "repeated member symbols are rejected" true
    (Semantic_member_type_resolution.resolve ~table ~parent:module_scope
       [ duplicate ]
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
      (Semantic_member_type_resolution.make_type_reference ~spelling:"Foreign"
         ~spelling_origin:(synthesized "foreign type")
         ~pointer_origins:[] ~resolved_type:foreign_type)
  in
  let foreign_member =
    checked
      (Semantic_member_type_resolution.make_member
         ~symbol:(add_member "foreign_value")
         ~member_path:[ 2 ] ~declarator_index:0
         ~declarator_origin:(synthesized "declarator")
         ~type_reference:foreign_reference
         ~declarator_kind:Semantic_member_type_resolution.Object
         ~array_dimension_origins:[])
  in
  let foreign_fact =
    checked
      (Semantic_member_type_resolution.make_aggregate ~symbol:aggregate_symbol
         ~scope:aggregate_scope ~item_index:0 [ foreign_member ])
  in
  Alcotest.(check bool)
    "a foreign aggregate type is rejected" true
    (Semantic_member_type_resolution.resolve ~table ~parent:module_scope
       [ foreign_fact ]
    |> Result.is_error);
  let foreign_parameter =
    checked
      (Semantic_function_type_resolution.make_parameter ~index:0
         ~origin:(synthesized "foreign parameter")
         ~type_reference:foreign_reference
         ~declarator_kind:Semantic_function_type_resolution.Object ~default:None
         ())
  in
  let foreign_signature =
    checked
      (Semantic_function_type_resolution.make_signature
         ~opening_origin:(synthesized "foreign signature opening")
         ~parameters:[ foreign_parameter ]
         ~closing_origin:(synthesized "foreign signature closing")
         ())
  in
  let foreign_pointer =
    checked
      (Semantic_member_type_resolution.make_function_pointer
         ~origin:(synthesized "foreign callback")
         ~opening_origin:(synthesized "foreign declarator opening")
         ~indirection_origins:[ synthesized "foreign callback star" ]
         ~closing_origin:(synthesized "foreign declarator closing")
         ~signature:foreign_signature)
  in
  let foreign_callback =
    checked
      (Semantic_member_type_resolution.make_member
         ~symbol:(add_member "foreign_callback")
         ~member_path:[ 3 ] ~declarator_index:0
         ~declarator_origin:(synthesized "foreign callback declarator")
         ~type_reference
         ~declarator_kind:
           (Semantic_member_type_resolution.Function_pointer foreign_pointer)
         ~array_dimension_origins:[])
  in
  let foreign_callback_fact =
    checked
      (Semantic_member_type_resolution.make_aggregate ~symbol:aggregate_symbol
         ~scope:aggregate_scope ~item_index:0 [ foreign_callback ])
  in
  Alcotest.(check bool)
    "a foreign recursive callback type is rejected" true
    (Semantic_member_type_resolution.resolve ~table ~parent:module_scope
       [ foreign_callback_fact ]
    |> Result.is_error);
  let first_parameter =
    checked
      (Semantic_function_type_resolution.make_parameter ~index:0
         ~origin:(synthesized "first parameter")
         ~type_reference
         ~declarator_kind:Semantic_function_type_resolution.Object ~default:None
         ())
  in
  let second_parameter =
    checked
      (Semantic_function_type_resolution.make_parameter ~index:1
         ~origin:(synthesized "second parameter")
         ~type_reference
         ~declarator_kind:Semantic_function_type_resolution.Object ~default:None
         ())
  in
  Alcotest.(check bool)
    "callback parameter delimiters must match their position" true
    (Semantic_function_type_resolution.make_signature
       ~opening_origin:(synthesized "malformed signature opening")
       ~parameters:[ first_parameter; second_parameter ]
       ~closing_origin:(synthesized "malformed signature closing")
       ()
    |> Result.is_error);
  Alcotest.(check bool)
    "a task scope cannot host member type facts" true
    (Semantic_member_type_resolution.resolve ~table
       ~parent:(Semantic_symbol_table.root table)
       []
    |> Result.is_error)

let tests =
  [
    Alcotest.test_case "primitive, intrinsic, and grouped pointers" `Quick
      primitive_intrinsic_and_grouped_pointers;
    Alcotest.test_case "self, forward, and shadowed types" `Quick
      self_forward_and_shadowed_types;
    Alcotest.test_case "newest repeated forward" `Quick
      repeated_forward_uses_newest_identity;
    Alcotest.test_case "member postpublication identity" `Quick
      members_use_postpublication_identity;
    Alcotest.test_case "anonymous unions and array origins" `Quick
      anonymous_union_paths_and_arrays;
    Alcotest.test_case "member-list flag masks" `Quick member_flag_masks;
    Alcotest.test_case "callback return and indirection" `Quick
      callback_return_and_indirection;
    Alcotest.test_case "callback parameter depths and visibility" `Quick
      callback_parameter_pointer_depths_and_visibility;
    Alcotest.test_case "pinned callback member signatures" `Quick
      pinned_callback_member_signatures;
    Alcotest.test_case "generated type provenance" `Quick
      generated_type_provenance;
    Alcotest.test_case "included type provenance" `Quick
      included_type_provenance;
    Alcotest.test_case "modes, determinism, and purity" `Quick
      modes_determinism_and_purity;
    Alcotest.test_case "mismatched inputs do not mutate" `Quick
      mismatched_inputs_do_not_mutate;
    Alcotest.test_case "low-level validation" `Quick low_level_validation;
  ]
