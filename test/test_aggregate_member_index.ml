open Holyc_lib
open Semantic_aggregate_member_index

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

let parse ?(mode = Preprocessor.Jit) session ~path contents =
  let source = Session.add_source session ~path ~contents in
  let config = checked (Preprocessor.Config.create ~compilation_mode:mode ()) in
  Holyc_lib.parse_with_config session ~config ~source |> expect_ast

type results = {
  declarations : Semantic_declaration_collection.t;
  headers : Semantic_aggregate_header_resolution.t;
  member_types : Semantic_member_type_resolution.t;
  layouts : Semantic_aggregate_layout.t;
  index : Semantic_aggregate_member_index.t;
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
  let collected =
    checked (Holyc_lib.collect_members session ~declarations ast)
  in
  let member_types =
    checked
      (Holyc_lib.resolve_member_types session ~declarations ~aggregates ~headers
         ~members:collected ast)
  in
  let layouts =
    checked
      (Holyc_lib.layout_aggregates session ~declarations ~aggregates ~headers
         ~members:member_types ast)
  in
  let index =
    checked
      (Holyc_lib.index_aggregate_members session ~declarations ~headers
         ~members:member_types ~layouts)
  in
  { declarations; headers; member_types; layouts; index }

let aggregate_named results name =
  Semantic_aggregate_member_index.aggregates results.index
  |> List.find (fun aggregate ->
      Semantic_symbol.name aggregate.symbol |> String.equal name)

let lookup results aggregate name =
  checked
    (Semantic_aggregate_member_index.lookup results.index
       ~aggregate:aggregate.symbol ~name
    |> Result.map_error Semantic_aggregate_member_index.error_to_string)

let expect_lookup results aggregate name =
  match lookup results aggregate name with
  | Some found -> found
  | None ->
      Alcotest.failf "expected member %s.%s"
        (Semantic_symbol.name aggregate.symbol)
        name

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let direct_and_missing_lookup () =
  let session = Session.create () in
  let ast =
    parse session ~path:"direct-member-index.HC"
      "class Direct { U8 first; I64 values[2]; I64 (*callback)(I64 value); };"
  in
  let results = resolve session ast in
  let direct = aggregate_named results "Direct" in
  let first = expect_lookup results direct "first" in
  Alcotest.(check int) "direct depth" 0 first.inheritance_depth;
  Alcotest.(check int64) "direct offset" 0L first.member.layout.offset;
  Alcotest.(check int64) "direct size" 1L first.member.layout.size;
  Alcotest.(check string)
    "direct signedness" "unsigned"
    (Semantic_aggregate_layout.signedness_name first.member.layout.signedness);
  (match Semantic_type.base first.member.member_type with
  | Semantic_type.Primitive (_, primitive) ->
      Alcotest.(check string)
        "direct type" "U8"
        (Primitive_type.to_string primitive)
  | Semantic_type.Aggregate _ -> Alcotest.fail "expected a primitive member");
  Alcotest.(check bool) "ordinary member" false first.member.is_function_pointer;
  Alcotest.(check bool)
    "ordinary member has no callback signature" true
    (Semantic_aggregate_member_index.member_function_pointer first.member
    |> Option.is_none);
  let values = expect_lookup results direct "values" in
  Alcotest.(check (list int64))
    "array dimensions" [ 2L ] values.member.layout.dimensions;
  Alcotest.(check int64) "array size" 16L values.member.layout.size;
  let callback = expect_lookup results direct "callback" in
  Alcotest.(check bool)
    "callback marker" true callback.member.is_function_pointer;
  Alcotest.(check int64) "callback storage" 8L callback.member.layout.size;
  let callback_signature =
    callback.member |> Semantic_aggregate_member_index.member_function_pointer
    |> Option.get
    |> Semantic_function_type_resolution.function_pointer_signature
  in
  Alcotest.(check int)
    "callback signature keeps its fixed slot" 1
    (callback_signature
   |> Semantic_function_type_resolution.signature_parameters |> List.length);
  Alcotest.(check (option int))
    "missing member" None
    (lookup results direct "missing"
    |> Option.map (fun found -> symbol_id found.member.symbol))

let anonymous_union_paths_survive () =
  let session = Session.create () in
  let ast =
    parse session ~path:"anonymous-member-index.HC"
      "class Nested { I8 head; union { I16 short; union { U8 bytes[3]; }; }; };"
  in
  let results = resolve session ast in
  let nested = aggregate_named results "Nested" in
  let short = expect_lookup results nested "short" in
  let bytes = expect_lookup results nested "bytes" in
  Alcotest.(check (list int))
    "one anonymous level" [ 1; 0 ] short.member.layout.path;
  Alcotest.(check (list int))
    "two anonymous levels" [ 1; 1; 0 ] bytes.member.layout.path;
  Alcotest.(check int64) "anonymous union start" 1L short.member.layout.offset;
  Alcotest.(check int64)
    "nested anonymous union start" 3L bytes.member.layout.offset

let chained_inheritance_lookup () =
  let session = Session.create () in
  let ast =
    parse session ~path:"inherited-member-index.HC"
      "class Base { I8 base; }; class Mid : Base { I16 middle; }; class Leaf : \
       Mid { I32 leaf; };"
  in
  let results = resolve session ast in
  let base = aggregate_named results "Base" in
  let mid = aggregate_named results "Mid" in
  let leaf = aggregate_named results "Leaf" in
  let inherited_base = expect_lookup results leaf "base" in
  let inherited_middle = expect_lookup results leaf "middle" in
  let direct_leaf = expect_lookup results leaf "leaf" in
  Alcotest.(check int) "two base edges" 2 inherited_base.inheritance_depth;
  Alcotest.(check int) "one base edge" 1 inherited_middle.inheritance_depth;
  Alcotest.(check int) "direct member depth" 0 direct_leaf.inheritance_depth;
  Alcotest.(check int)
    "base declaring aggregate" (symbol_id base.symbol)
    (symbol_id inherited_base.declaring_aggregate);
  Alcotest.(check int)
    "middle declaring aggregate" (symbol_id mid.symbol)
    (symbol_id inherited_middle.declaring_aggregate);
  Alcotest.(check int64)
    "base offset remains absolute" 0L inherited_base.member.layout.offset;
  Alcotest.(check int64)
    "middle offset remains absolute" 1L inherited_middle.member.layout.offset;
  Alcotest.(check int64)
    "leaf follows base size" 3L direct_leaf.member.layout.offset

let index_error session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  let collected =
    checked (Holyc_lib.collect_members session ~declarations ast)
  in
  let member_types =
    checked
      (Holyc_lib.resolve_member_types session ~declarations ~aggregates ~headers
         ~members:collected ast)
  in
  let layouts =
    checked
      (Holyc_lib.layout_aggregates session ~declarations ~aggregates ~headers
         ~members:member_types ast)
  in
  match
    Holyc_lib.index_aggregate_members session ~declarations ~headers
      ~members:member_types ~layouts
  with
  | Ok _ -> Alcotest.fail "expected aggregate member indexing to fail"
  | Error message -> message

let check_prefix prefix value =
  Alcotest.(check bool) prefix true (String.starts_with ~prefix value)

let ordinary_duplicates_are_rejected () =
  let direct_session = Session.create () in
  let direct =
    parse direct_session ~path:"direct-member-duplicate.HC"
      "class Bad { I8 value; I16 value; };"
  in
  check_prefix "HCSEMA0012:" (index_error direct_session direct);
  let inherited_session = Session.create () in
  let inherited =
    parse inherited_session ~path:"inherited-member-duplicate.HC"
      "class Base { I8 value; }; class Bad : Base { I16 value; };"
  in
  check_prefix "HCSEMA0012:" (index_error inherited_session inherited);
  let case_session = Session.create () in
  let case_sensitive =
    parse case_session ~path:"padding-case-duplicate.HC"
      "class Bad { I8 Pad; I16 Pad; };"
  in
  check_prefix "HCSEMA0012:" (index_error case_session case_sensitive)

let padding_exceptions_keep_first_direct_member () =
  let session = Session.create () in
  let ast =
    parse session ~path:"permitted-member-duplicates.HC"
      "class Base { I8 pad; I16 reserved; I32 _anon_; }; class Derived : Base \
       { U8 pad; U16 pad; U32 reserved; I64 _anon_; };"
  in
  let results = resolve session ast in
  let derived = aggregate_named results "Derived" in
  let direct name =
    derived.direct_members
    |> List.filter (fun (member : Semantic_aggregate_member_index.member) ->
        Semantic_symbol.name member.symbol |> String.equal name)
  in
  let pad_members = direct "pad" in
  Alcotest.(check int) "two direct pads" 2 (List.length pad_members);
  let pad = expect_lookup results derived "pad" in
  Alcotest.(check int) "derived member wins" 0 pad.inheritance_depth;
  Alcotest.(check int)
    "first direct pad wins"
    (List.hd pad_members |> fun member -> symbol_id member.symbol)
    (symbol_id pad.member.symbol);
  Alcotest.(check int64) "first derived pad offset" 7L pad.member.layout.offset;
  let reserved = expect_lookup results derived "reserved" in
  Alcotest.(check int) "derived reserved wins" 0 reserved.inheritance_depth;
  let anonymous = expect_lookup results derived "_anon_" in
  Alcotest.(check int)
    "derived anonymous name wins" 0 anonymous.inheritance_depth

let low_level_input aggregate layout =
  let facts = Semantic_member_type_resolution.aggregate_members aggregate in
  let members = layout.Semantic_aggregate_layout.members in
  let aggregate_members =
    List.map2
      (fun fact member_layout ->
        let reference =
          Semantic_member_type_resolution.member_type_reference fact
        in
        let member_function_pointer =
          match Semantic_member_type_resolution.member_declarator_kind fact with
          | Semantic_member_type_resolution.Object -> None
          | Semantic_member_type_resolution.Function_pointer pointer ->
              Some pointer
        in
        {
          member_type =
            Semantic_member_type_resolution.type_reference_type reference;
          member_function_pointer;
          member_layout;
        })
      facts members
  in
  {
    aggregate_scope = Semantic_member_type_resolution.aggregate_scope aggregate;
    aggregate_layout = layout;
    aggregate_members;
  }

let low_level_inputs results =
  List.map2 low_level_input
    (Semantic_member_type_resolution.aggregates results.member_types)
    (Semantic_aggregate_layout.layouts results.layouts)

let low_level_failures_are_typed_and_pure () =
  let session = Session.create () in
  let ast =
    parse session ~path:"member-index-validation.HC"
      "class Base { I8 base; I16 other; }; class Derived : Base { I16 value; };"
  in
  let results = resolve session ast in
  let table = Session.semantic_symbols session in
  let parent = Semantic_declaration_collection.scope results.declarations in
  let inputs = low_level_inputs results in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let check_error expected = function
    | Ok _ -> Alcotest.failf "expected %s" expected
    | Error error ->
        Alcotest.(check string)
          "stable semantic code" expected
          (Semantic_aggregate_member_index.error_code error)
  in
  check_error "HCSEMA0011"
    (Semantic_aggregate_member_index.build ~table ~parent [ List.nth inputs 1 ]);
  let base = List.hd inputs in
  check_error "HCSEMA0010"
    (Semantic_aggregate_member_index.build ~table ~parent [ base; base ]);
  let reversed =
    { base with aggregate_members = List.rev base.aggregate_members }
  in
  check_error "HCSEMA0010"
    (Semantic_aggregate_member_index.build ~table ~parent [ reversed ]);
  let foreign_session = Session.create () in
  let foreign_ast =
    parse foreign_session ~path:"member-index-foreign.HC"
      "class Foreign { I8 member; I64 (*callback)(Foreign value); };"
  in
  let foreign = resolve foreign_session foreign_ast in
  let foreign_symbol = (aggregate_named foreign "Foreign").symbol in
  let foreign_type =
    checked
      (Semantic_type.make_aggregate ~symbol:foreign_symbol ~pointer_depth:0)
  in
  let foreign_member_type =
    match base.aggregate_members with
    | [] -> Alcotest.fail "expected a base member"
    | member :: rest ->
        {
          base with
          aggregate_members = { member with member_type = foreign_type } :: rest;
        }
  in
  check_error "HCSEMA0010"
    (Semantic_aggregate_member_index.build ~table ~parent
       [ foreign_member_type ]);
  let foreign_pointer =
    let foreign_aggregate = aggregate_named foreign "Foreign" in
    expect_lookup foreign foreign_aggregate "callback" |> fun lookup ->
    Semantic_aggregate_member_index.member_function_pointer lookup.member
    |> Option.get
  in
  let foreign_callback_type =
    match base.aggregate_members with
    | [] -> Alcotest.fail "expected a base member"
    | member :: rest ->
        {
          base with
          aggregate_members =
            { member with member_function_pointer = Some foreign_pointer }
            :: rest;
        }
  in
  check_error "HCSEMA0010"
    (Semantic_aggregate_member_index.build ~table ~parent
       [ foreign_callback_type ]);
  Alcotest.(check int)
    "failed indexes do not add symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let derived = aggregate_named results "Derived" in
  check_error "HCSEMA0010"
    (Semantic_aggregate_member_index.lookup results.index
       ~aggregate:derived.symbol ~name:"");
  let base_only =
    checked
      (Semantic_aggregate_member_index.build ~table ~parent [ List.hd inputs ]
      |> Result.map_error Semantic_aggregate_member_index.error_to_string)
  in
  check_error "HCSEMA0013"
    (Semantic_aggregate_member_index.lookup base_only ~aggregate:derived.symbol
       ~name:"value")

let mismatched_high_level_inputs_are_rejected () =
  let first_session = Session.create () in
  let first_ast =
    parse first_session ~path:"member-index-first.HC" "class First { I8 one; };"
  in
  let first = resolve first_session first_ast in
  let other_session = Session.create () in
  let other_ast =
    parse other_session ~path:"member-index-other.HC" "class Other { I8 two; };"
  in
  let other = resolve other_session other_ast in
  let rejected label result =
    match result with
    | Ok _ -> Alcotest.failf "expected mismatched %s to fail" label
    | Error message -> message
  in
  check_prefix "HCSEMA0010:"
    (rejected "declarations"
       (Holyc_lib.index_aggregate_members first_session
          ~declarations:other.declarations ~headers:first.headers
          ~members:first.member_types ~layouts:first.layouts));
  check_prefix "HCSEMA0010:"
    (rejected "headers"
       (Holyc_lib.index_aggregate_members first_session
          ~declarations:first.declarations ~headers:other.headers
          ~members:first.member_types ~layouts:first.layouts));
  check_prefix "HCSEMA0010:"
    (rejected "member types"
       (Holyc_lib.index_aggregate_members first_session
          ~declarations:first.declarations ~headers:first.headers
          ~members:other.member_types ~layouts:first.layouts));
  check_prefix "HCSEMA0010:"
    (rejected "layouts"
       (Holyc_lib.index_aggregate_members first_session
          ~declarations:first.declarations ~headers:first.headers
          ~members:first.member_types ~layouts:other.layouts))

let modes_and_repeat_runs_are_deterministic () =
  let source =
    "class Base { I8 pad; I16 value; }; class Derived : Base { U8 pad; I32 \
     tail; };"
  in
  let signature mode =
    let session = Session.create () in
    let ast = parse session ~mode ~path:"member-index-mode.HC" source in
    let results = resolve session ast in
    let make_signature index =
      Semantic_aggregate_member_index.aggregates index
      |> List.map (fun aggregate ->
          ( Semantic_symbol.name aggregate.symbol,
            Option.map Semantic_symbol.name aggregate.base_symbol,
            List.map
              (fun (member : Semantic_aggregate_member_index.member) ->
                ( Semantic_symbol.name member.symbol,
                  member.layout.offset,
                  member.layout.size ))
              aggregate.direct_members ))
    in
    let first = make_signature results.index in
    let second =
      checked
        (Holyc_lib.index_aggregate_members session
           ~declarations:results.declarations ~headers:results.headers
           ~members:results.member_types ~layouts:results.layouts)
      |> make_signature
    in
    Alcotest.(
      check
        (list
           (triple string (option string) (list (triple string int64 int64)))))
      "repeat member index" first second;
    first
  in
  let jit = signature Preprocessor.Jit in
  let aot = signature Preprocessor.Aot in
  Alcotest.(
    check
      (list (triple string (option string) (list (triple string int64 int64)))))
    "JIT and AOT member indexes" jit aot

let tests =
  [
    Alcotest.test_case "direct and missing lookup" `Quick
      direct_and_missing_lookup;
    Alcotest.test_case "anonymous union paths" `Quick
      anonymous_union_paths_survive;
    Alcotest.test_case "chained inheritance lookup" `Quick
      chained_inheritance_lookup;
    Alcotest.test_case "ordinary duplicate rejection" `Quick
      ordinary_duplicates_are_rejected;
    Alcotest.test_case "padding duplicate exceptions" `Quick
      padding_exceptions_keep_first_direct_member;
    Alcotest.test_case "typed low-level failures" `Quick
      low_level_failures_are_typed_and_pure;
    Alcotest.test_case "mismatched high-level inputs" `Quick
      mismatched_high_level_inputs_are_rejected;
    Alcotest.test_case "modes and deterministic repeats" `Quick
      modes_and_repeat_runs_are_deterministic;
  ]
