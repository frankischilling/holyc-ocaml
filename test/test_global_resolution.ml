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

type prepared = {
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  globals : Semantic_global_type_resolution.t;
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
  let globals =
    checked
      (Holyc_lib.resolve_global_types session ~declarations ~aggregates ast)
  in
  { session; ast; declarations; globals }

let resolve prepared mode =
  checked
    (Holyc_lib.resolve_global_records prepared.session
       ~declarations:prepared.declarations ~globals:prepared.globals
       ~compilation_mode:mode prepared.ast)

let records resolution = Semantic_global_resolution.records resolution
let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let record_symbol_ids resolution =
  records resolution
  |> List.map Semantic_global_resolution.global_record_symbol
  |> List.map symbol_id

let index_of_symbol records symbol =
  let id = symbol_id symbol in
  let rec find index = function
    | [] -> Alcotest.fail "alias target is not part of the global record batch"
    | record :: rest ->
        if
          record |> Semantic_global_resolution.global_record_symbol |> symbol_id
          = id
        then index
        else find (index + 1) rest
  in
  find 0 records

let alias_indexes resolution =
  let all = records resolution in
  all
  |> List.map (fun record ->
      Semantic_global_resolution.global_record_alias_target record
      |> Option.map (index_of_symbol all))

let record_names resolution =
  records resolution
  |> List.map Semantic_global_resolution.global_record_symbol
  |> List.map Semantic_symbol.name

let record_kinds resolution =
  records resolution
  |> List.map Semantic_global_resolution.global_record_kind
  |> List.map Semantic_global_resolution.declaration_kind_name

let record_states resolution =
  records resolution
  |> List.map Semantic_global_resolution.global_record_state
  |> List.map Semantic_global_resolution.state_name

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let source_text session origin =
  let origin = source_origin origin in
  let source =
    Source_manager.find (Session.sources session) origin.span.source
    |> Option.get
  in
  String.sub
    (Source_file.contents source)
    origin.span.start (Span.length origin.span)

let driver_retains_every_binding_form () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"global-binding-records.HC"
      "extern I64 Forward;\n\
       _extern REMOTE I64 Bound;\n\
       import I64 Imported;\n\
       _import REMOTE_IMPORT I64 Aliased;\n\
       _intern 42 I64 Internal;\n\
       I64 Defined;\n\
       class Pair { I64 value; } Attached;"
  in
  let resolution = resolve prepared mode in
  Alcotest.(check (list string))
    "source order"
    [
      "Forward";
      "Bound";
      "Imported";
      "Aliased";
      "Internal";
      "Defined";
      "Attached";
    ]
    (record_names resolution);
  Alcotest.(check (list string))
    "binding forms"
    [
      "extern";
      "alternate-extern";
      "import";
      "alternate-import";
      "intern";
      "definition";
      "definition";
    ]
    (record_kinds resolution);
  Alcotest.(check (list string))
    "AOT record states"
    [
      "declared-extern";
      "bound-extern";
      "imported";
      "imported";
      "defined";
      "defined";
      "defined";
    ]
    (record_states resolution);
  Alcotest.(check (list string))
    "the driver uses the current code-heap default"
    [
      "code-heap";
      "code-heap";
      "code-heap";
      "code-heap";
      "code-heap";
      "code-heap";
      "code-heap";
    ]
    (records resolution
    |> List.map Semantic_global_resolution.global_record_storage
    |> List.map Semantic_global_resolution.storage_name);
  let bindings =
    records resolution
    |> List.map Semantic_global_resolution.global_record_declaration
    |> List.map Semantic_global_resolution.declaration_binding
  in
  Alcotest.(check (list (option string)))
    "binding spellings"
    [
      Some "extern";
      Some "_extern";
      Some "import";
      Some "_import";
      Some "_intern";
      None;
      None;
    ]
    (List.map
       (Option.map Semantic_global_resolution.source_binding_spelling)
       bindings);
  Alcotest.(check (list (option string)))
    "target kinds"
    [
      Some "none";
      Some "symbol";
      Some "none";
      Some "symbol";
      Some "expression";
      None;
      None;
    ]
    (List.map
       (Option.map (fun binding ->
            binding |> Semantic_global_resolution.source_binding_target
            |> Semantic_global_resolution.binding_target_kind
            |> Semantic_global_resolution.binding_target_kind_name))
       bindings);
  let target_text binding =
    binding |> Option.get |> Semantic_global_resolution.source_binding_target
    |> Semantic_global_resolution.binding_target_origin |> Option.get
    |> source_text prepared.session
  in
  Alcotest.(check string)
    "alternate extern target" "REMOTE"
    (target_text (List.nth bindings 1));
  Alcotest.(check string)
    "alternate import target" "REMOTE_IMPORT"
    (target_text (List.nth bindings 3));
  Alcotest.(check string)
    "intern expression target" "42"
    (target_text (List.nth bindings 4));
  Alcotest.(check string)
    "mode is explicit" "aot"
    (Semantic_global_resolution.compilation_mode resolution
    |> Semantic_global_resolution.compilation_mode_name)

let jit_aliases_only_the_newest_plain_extern () =
  let mode = Preprocessor.Jit in
  let prepared =
    prepare ~mode ~path:"jit-global-records.HC"
      "extern I64 Value;\n\
       extern I32 Value;\n\
       I64 Value;\n\
       I64 Value;\n\
       _extern REMOTE I64 Bound;\n\
       I64 Bound;\n\
       extern I64 Last;\n\
       I64 Last;"
  in
  let resolution = resolve prepared mode in
  Alcotest.(check int)
    "every declaration remains a record" 8
    (List.length (records resolution));
  Alcotest.(check (list (option int)))
    "only newest unresolved plain externs receive alias edges"
    [ None; Some 2; None; None; None; None; Some 7; None ]
    (alias_indexes resolution);
  Alcotest.(check (list string))
    "JIT states retain source binding"
    [
      "unresolved-extern";
      "unresolved-extern";
      "defined";
      "defined";
      "bound-extern";
      "defined";
      "unresolved-extern";
      "defined";
    ]
    (record_states resolution);
  let symbols = record_symbol_ids resolution in
  Alcotest.(check int)
    "same-name declarations have different records" 8
    (symbols |> List.sort_uniq Int.compare |> List.length)

let aot_aliases_the_immediate_prior_record () =
  let mode = Preprocessor.Aot in
  let prepared =
    prepare ~mode ~path:"aot-global-records.HC"
      "extern I64 External; I64 External;\n\
       import I64 Imported; I64 Imported;\n\
       _extern REMOTE I64 Bound; I64 Bound;\n\
       _import REMOTE_IMPORT I64 Alternate; I64 Alternate;\n\
       _intern 1 I64 Internal; I64 Internal;\n\
       I64 Defined; I64 Defined;\n\
       I64 Chain; I64 Chain; I64 Chain;"
  in
  let resolution = resolve prepared mode in
  Alcotest.(check (list (option int)))
    "AOT edges retain each immediate predecessor"
    [
      Some 1;
      None;
      Some 3;
      None;
      Some 5;
      None;
      Some 7;
      None;
      Some 9;
      None;
      Some 11;
      None;
      Some 13;
      Some 14;
      None;
    ]
    (alias_indexes resolution);
  Alcotest.(check (list string))
    "all source records remain present"
    [
      "External";
      "External";
      "Imported";
      "Imported";
      "Bound";
      "Bound";
      "Alternate";
      "Alternate";
      "Internal";
      "Internal";
      "Defined";
      "Defined";
      "Chain";
      "Chain";
      "Chain";
    ]
    (record_names resolution)

let aot_alias_does_not_compare_types () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"aot-global-type-mismatch.HC"
      "extern I8 Mixed; F64 Mixed;"
  in
  let resolution = resolve prepared Preprocessor.Aot in
  Alcotest.(check (list (option int)))
    "the source alias rule does not add a header check" [ Some 1; None ]
    (alias_indexes resolution)

let data_heap_policy_follows_option_snapshot () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"aot-data-heap-records.HC"
      "I64 First; I64 First;"
  in
  let globals = Semantic_global_type_resolution.globals prepared.globals in
  let data_heap_mask, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Globals_on_data_heap true
  in
  let declaration ?compiler_option_mask global =
    checked
      (Semantic_global_resolution.make_declaration ?compiler_option_mask ~global
         ())
  in
  let first = List.nth globals 0 in
  let second = List.nth globals 1 in
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  let one =
    checked
      (Semantic_global_resolution.resolve ~table ~parent
         ~compilation_mode:Semantic_global_resolution.Aot
         [ declaration ~compiler_option_mask:data_heap_mask first ])
  in
  Alcotest.(check (list string))
    "a first data-heap definition is representable" [ "data-heap" ]
    (records one
    |> List.map Semantic_global_resolution.global_record_storage
    |> List.map Semantic_global_resolution.storage_name);
  Alcotest.(check bool)
    "a repeated AOT data-heap definition is rejected" true
    (Semantic_global_resolution.resolve ~table ~parent
       ~compilation_mode:Semantic_global_resolution.Aot
       [
         declaration first;
         declaration ~compiler_option_mask:data_heap_mask second;
       ]
    |> Result.is_error)

let grouped_and_attached_globals_keep_order () =
  let prepared =
    prepare ~path:"grouped-global-records.HC"
      "class Pair { I64 value; } first,second;\n\
       extern I64 third,third;\n\
       I64 third;"
  in
  let resolution = resolve prepared Preprocessor.Jit in
  Alcotest.(check (list string))
    "record order"
    [ "first"; "second"; "third"; "third"; "third" ]
    (record_names resolution);
  Alcotest.(check (list string))
    "attached globals are definitions and the binding spans the comma group"
    [ "definition"; "definition"; "extern"; "extern"; "definition" ]
    (record_kinds resolution);
  Alcotest.(check (list (option int)))
    "declarator positions"
    [ Some 0; Some 1; Some 0; Some 1; None ]
    (records resolution
    |> List.map Semantic_global_resolution.global_record_global
    |> List.map Semantic_global_type_resolution.global_declarator_index);
  Alcotest.(check (list (option int)))
    "the last repeated declarator is the JIT alias candidate"
    [ None; None; None; Some 4; None ]
    (alias_indexes resolution)

let invalid_source_bindings_are_rejected () =
  let prepared = prepare ~path:"binding-shape.HC" "I64 Value;" in
  let origin =
    prepared.globals |> Semantic_global_type_resolution.globals |> List.hd
    |> Semantic_global_type_resolution.global_symbol |> Semantic_symbol.origin
  in
  let symbol_target =
    checked
      (Semantic_global_resolution.make_symbol_binding_target ~name:"REMOTE"
         ~origin)
  in
  let expression_target =
    Semantic_global_resolution.make_expression_binding_target ~origin
  in
  let reject label kind spelling target =
    Alcotest.(check bool)
      label true
      (Semantic_global_resolution.make_source_binding ~kind ~spelling ~origin
         ~target
      |> Result.is_error)
  in
  reject "ordinary extern cannot name a target"
    Semantic_global_resolution.Extern_binding "extern" symbol_target;
  reject "alternate extern requires a target"
    Semantic_global_resolution.Extern_binding "_extern"
    Semantic_global_resolution.no_binding_target;
  reject "imports cannot use expression targets"
    Semantic_global_resolution.Import_binding "_import" expression_target;
  reject "intern requires an expression"
    Semantic_global_resolution.Intern_binding "_intern" symbol_target;
  Alcotest.(check bool)
    "empty target names are rejected" true
    (Semantic_global_resolution.make_symbol_binding_target ~name:"" ~origin
    |> Result.is_error)

let invalid_batches_and_driver_inputs_do_not_mutate () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"invalid-global-records.HC"
      "import I64 First; I64 Second;"
  in
  let aot = resolve prepared Preprocessor.Aot in
  let declarations =
    records aot |> List.map Semantic_global_resolution.global_record_declaration
  in
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  let scope_count = Semantic_symbol_table.all_scopes table |> List.length in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let reject label mode declarations =
    Alcotest.(check bool)
      label true
      (Semantic_global_resolution.resolve ~table ~parent ~compilation_mode:mode
         declarations
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
  reject "JIT import" Semantic_global_resolution.Jit declarations;
  reject "reversed source order" Semantic_global_resolution.Aot
    (List.rev declarations);
  reject "repeated declaration symbol" Semantic_global_resolution.Aot
    [ List.hd declarations; List.hd declarations ];
  let other = prepare ~path:"foreign-global-record.HC" "I64 Foreign;" in
  let foreign =
    resolve other Preprocessor.Jit
    |> records |> List.hd
    |> Semantic_global_resolution.global_record_declaration
  in
  reject "foreign symbol table" Semantic_global_resolution.Aot [ foreign ];
  let foreign_parent =
    Semantic_declaration_collection.scope other.declarations
  in
  Alcotest.(check bool)
    "a foreign module scope is rejected" true
    (Semantic_global_resolution.resolve ~table ~parent:foreign_parent
       ~compilation_mode:Semantic_global_resolution.Aot declarations
    |> Result.is_error);
  Alcotest.(check bool)
    "the driver rejects a JIT import mode mismatch" true
    (Holyc_lib.resolve_global_records prepared.session
       ~declarations:prepared.declarations ~globals:prepared.globals
       ~compilation_mode:Preprocessor.Jit prepared.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "an unrelated AST is rejected" true
    (Holyc_lib.resolve_global_records prepared.session
       ~declarations:prepared.declarations ~globals:prepared.globals
       ~compilation_mode:Preprocessor.Aot other.ast
    |> Result.is_error);
  Alcotest.(check bool)
    "foreign global types are rejected" true
    (Holyc_lib.resolve_global_records prepared.session
       ~declarations:prepared.declarations ~globals:other.globals
       ~compilation_mode:Preprocessor.Aot prepared.ast
    |> Result.is_error);
  Alcotest.(check int)
    "driver failures preserve scopes" scope_count
    (Semantic_symbol_table.all_scopes table |> List.length);
  Alcotest.(check int)
    "driver failures preserve symbols" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let deterministic_resolution () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"deterministic-global-records.HC"
      "extern I64 Stable; I64 Stable; I64 Stable;"
  in
  let first = resolve prepared Preprocessor.Aot in
  let second = resolve prepared Preprocessor.Aot in
  Alcotest.(check (list int))
    "record symbols are deterministic" (record_symbol_ids first)
    (record_symbol_ids second);
  Alcotest.(check (list (option int)))
    "alias edges are deterministic" (alias_indexes first) (alias_indexes second)

let tests =
  [
    Alcotest.test_case "driver retains every binding form" `Quick
      driver_retains_every_binding_form;
    Alcotest.test_case "JIT aliases newest plain extern" `Quick
      jit_aliases_only_the_newest_plain_extern;
    Alcotest.test_case "AOT aliases immediate prior record" `Quick
      aot_aliases_the_immediate_prior_record;
    Alcotest.test_case "AOT alias keeps type mismatches" `Quick
      aot_alias_does_not_compare_types;
    Alcotest.test_case "data-heap policy follows option snapshot" `Quick
      data_heap_policy_follows_option_snapshot;
    Alcotest.test_case "grouped and attached record order" `Quick
      grouped_and_attached_globals_keep_order;
    Alcotest.test_case "invalid source binding shapes" `Quick
      invalid_source_bindings_are_rejected;
    Alcotest.test_case "invalid batches preserve tables" `Quick
      invalid_batches_and_driver_inputs_do_not_mutate;
    Alcotest.test_case "deterministic resolution" `Quick
      deterministic_resolution;
  ]
