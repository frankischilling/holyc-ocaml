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

let callback_return_and_indirection () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-callbacks.HC"
      "class Callbacks { I64 *(*invoke)(U8 arg); I64 (**chain)(I64 value); };"
  in
  let results = resolve session ast in
  let callbacks = aggregate_named results "Callbacks" in
  let invoke = member_named callbacks "invoke" in
  let chain = member_named callbacks "chain" in
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.I64 ~pointer_depth:1 invoke;
  check_primitive ~form:Semantic_type.Public_spelling
    ~primitive:Primitive_type.I64 ~pointer_depth:0 chain;
  let callback_depth member =
    match Semantic_member_type_resolution.member_declarator_kind member with
    | Semantic_member_type_resolution.Object ->
        Alcotest.fail "expected a callback member"
    | Semantic_member_type_resolution.Function_pointer pointer ->
        pointer
        |> Semantic_member_type_resolution.function_pointer_indirection_origins
        |> List.length
  in
  Alcotest.(check int)
    "return pointer is separate from callback indirection" 1
    (callback_depth invoke);
  Alcotest.(check int)
    "callback indirection retains both stars" 2 (callback_depth chain)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_type_provenance () =
  let session = Session.create () in
  let ast =
    parse session ~path:"generated-member-type.HC"
      "#define STORAGE I64i\nclass Box { STORAGE value; };"
  in
  let member =
    resolve session ast |> fun results ->
    aggregate_named results "Box" |> fun aggregate ->
    member_named aggregate "value"
  in
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
    (Option.is_some type_origin.defined_at)

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
      write_file include_path "class Included { I64 value; };";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let member =
        resolve session ast |> fun results ->
        aggregate_named results "Included" |> fun aggregate ->
        member_named aggregate "value"
      in
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
        (Source_file.path source |> Filename.basename))

let member_signature member =
  let type_ = member_type member in
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
  let kind =
    match Semantic_member_type_resolution.member_declarator_kind member with
    | Semantic_member_type_resolution.Object -> "object"
    | Semantic_member_type_resolution.Function_pointer pointer ->
        Printf.sprintf "callback:%d"
          (pointer
         |> Semantic_member_type_resolution.function_pointer_indirection_origins
         |> List.length)
  in
  Printf.sprintf "%s:%s:%d:%s:%d"
    (member |> Semantic_member_type_resolution.member_symbol
   |> Semantic_symbol.name)
    base
    (Semantic_type.pointer_depth type_)
    kind
    (Semantic_member_type_resolution.member_array_dimension_origins member
    |> List.length)

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
  Alcotest.(check bool)
    "a callback requires indirection" true
    (Semantic_member_type_resolution.make_function_pointer
       ~origin:(synthesized "callback") ~indirection_origins:[]
    |> Result.is_error);
  Alcotest.(check bool)
    "a callback rejects a fifth pointer star" true
    (Semantic_member_type_resolution.make_function_pointer
       ~origin:(synthesized "callback")
       ~indirection_origins:
         (List.init 5 (fun index ->
              synthesized (Printf.sprintf "star %d" index)))
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
    Alcotest.test_case "callback return and indirection" `Quick
      callback_return_and_indirection;
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
