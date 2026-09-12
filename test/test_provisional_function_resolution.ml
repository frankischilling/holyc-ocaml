open Holyc_lib
module N = Semantic_function_record_phase
module C = Semantic_declaration_collection
module H = Semantic_function_type_resolution
module R = Semantic_function_resolution
module D = Holyc_lib__Driver.Function_type_resolution

let checked = Test_declaration_collection.checked
let single resolution = List.hd (R.declarations resolution)

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let same label a b = Alcotest.(check bool) label true (a == b)

type fixture = {
  table : Semantic_symbol_table.t;
  namespace : C.namespace;
  samples : N.snapshot list;
  final_snapshot : N.snapshot;
  source : Semantic_compiler_record.declared_function;
  ordinary : H.resolved_function;
  events : (Parser.declaration_event * N.snapshot) list;
  headers :
    (N.snapshot
    * Semantic_compiler_record.declared_function
    * H.resolved_function)
    list;
}

let fixture ?(contents = "I64 F(I64 n=#exe {}40)#exe {}{return n;}") () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let records = ref [] and samples = ref [] and header = ref None in
  let events = ref [] and headers = ref [] in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        let record = N.begin_header registry publication source |> checked in
        records := record :: !records;
        events := (event, N.snapshot record) :: !events
    | _ -> (
        let current =
          List.find (fun record -> N.event_belongs record event) !records
        in
        N.observe current event |> checked;
        events := (event, N.snapshot current) :: !events;
        match event with
        | Parser.Function_header_completed source ->
            let snapshot = N.snapshot current in
            let declared =
              Semantic_compiler_record.declare_function ~table ~namespace
                (N.publication snapshot) source
              |> checked
            in
            let ordinary =
              D.resolve_completed_header ~table ~namespace declared |> checked
            in
            header := Some (snapshot, declared, ordinary);
            headers := (snapshot, declared, ordinary) :: !headers
        | _ -> ()));
    Ok ()
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true
      ~commands:(Test_provisional_function_parser.sink declaration)
      ~configure:(fun _ execution ->
        {
          execution with
          Parser.commands = Test_provisional_function_parser.sink declaration;
        })
      ~on_enter:(fun () -> samples := N.snapshot (List.hd !records) :: !samples)
      contents
  in
  ignore (Test_parser.expect_ast parsed);
  let final_snapshot, source, ordinary = Option.get !header in
  {
    table;
    namespace;
    samples = List.rev !samples;
    final_snapshot;
    source;
    ordinary;
    events = List.rev !events;
    headers = List.rev !headers;
  }

let typed ?scope f snapshot =
  D.resolve_provisional_call ?scope ~table:f.table ~namespace:f.namespace
    (N.call_shape snapshot |> checked)
  |> checked

let resolve ?(previous = []) ?(record_heads = []) f fact =
  R.resolve ~previous ~record_heads ~table:f.table
    ~parent:(C.namespace_scope f.namespace)
    ~compilation_mode:R.Jit [ fact ]

let initial f =
  let function_ = typed f (List.hd f.samples) in
  let fact =
    R.make_provisional_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~function_
    |> checked
  in
  (function_, resolve f fact |> checked |> single)

let progress f function_ pending =
  let snapshot = List.nth f.samples 1 in
  let function_ = typed ~scope:(H.function_scope function_) f snapshot in
  let transition =
    N.transition ~earlier:(List.hd f.samples) ~later:snapshot |> checked
  in
  let fact =
    R.make_provisional_advance
      ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
      ~namespace:f.namespace ~pending ~current:pending ~transition ~function_ ()
    |> checked
  in
  (function_, fact, resolve ~previous:[ pending ] f fact |> checked |> single)

let header_fact f pending current =
  let earlier =
    Option.get
      (R.declaration_site_native_snapshot (R.resolved_declaration_site current))
  in
  let transition = N.transition ~earlier ~later:f.final_snapshot |> checked in
  let callable_function =
    typed ~scope:(H.function_scope f.ordinary) f f.final_snapshot
  in
  R.make_header_advance ~table:f.table ~namespace:f.namespace ~pending ~current
    ~transition ~source:f.source ~function_:f.ordinary ~callable_function

let complete_lifecycle () =
  let f = fixture () in
  let projection, initial = initial f in
  let _, _, progressed = progress f projection initial in
  let site = R.resolved_declaration_site progressed in
  Alcotest.(check bool)
    "progress remains explicitly provisional" true
    (R.declaration_site_phase site = R.Provisional);
  same "source phase link is independent of body completion" initial
    (Option.get (R.resolved_declaration_phase_source progressed));
  Alcotest.(check bool)
    "phase progress never grants body completion" true
    (Option.is_none (R.resolved_declaration_completion_source progressed));
  let fact = header_fact f progressed progressed |> checked in
  let header = resolve ~previous:[ progressed ] f fact |> checked |> single in
  Alcotest.(check bool)
    "real header is a distinct source phase" true
    (R.declaration_site_phase (R.resolved_declaration_site header)
    = R.Completed_header);
  same "real body owner is the exact ordinary typed source" f.ordinary
    (R.declaration_site_function (R.resolved_declaration_site header));
  Alcotest.(check bool)
    "native callable projection stays distinct from body owner" true
    (R.resolved_declaration_header header != f.ordinary
    && Option.is_some
         (H.function_provisional_call (R.resolved_declaration_header header)));
  same "header keeps native record identity"
    (R.resolved_declaration_identity_symbol initial)
    (R.resolved_declaration_identity_symbol header);
  reject "call projection cannot be used to publish the body"
    (R.make_completion_declaration ~table:f.table ~namespace:f.namespace
       ~pending:header
       ~function_:(R.resolved_declaration_header header));
  let body =
    R.complete_pending ~table:f.table ~namespace:f.namespace ~pending:header
      ~function_:f.ordinary
    |> checked |> single
  in
  Alcotest.(check bool)
    "body advances only from exact real header" true
    (R.declaration_site_phase (R.resolved_declaration_site body)
    = R.Completed_body);
  same "body retains current callable shape"
    (R.resolved_declaration_header header)
    (R.resolved_declaration_header body)

let reject_fabrication () =
  let f = fixture () in
  let projection, pending = initial f in
  let later = List.nth f.samples 1 in
  let transition =
    N.transition ~earlier:(List.hd f.samples) ~later |> checked
  in
  let same_scope = typed ~scope:(H.function_scope projection) f later in
  let foreign_namespace = C.create_namespace ~table:f.table () |> checked in
  reject "foreign namespace cannot advance original source"
    (R.make_provisional_advance
       ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
       ~namespace:foreign_namespace ~pending ~current:pending ~transition
       ~function_:same_scope ());
  let substituted_scope = typed f later in
  reject "matching original source cannot silently replace call-only scope"
    (R.make_provisional_advance
       ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
       ~namespace:f.namespace ~pending ~current:pending ~transition
       ~function_:substituted_scope ());
  reject "transition cannot attach a projection from its earlier phase"
    (R.make_provisional_advance
       ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
       ~namespace:f.namespace ~pending ~current:pending ~transition
       ~function_:projection ());
  reject "backward phase has no native proof"
    (N.transition ~earlier:later ~later:(List.hd f.samples));
  reject "unchanged source phase has no forward proof"
    (N.transition ~earlier:later ~later);
  let foreign = fixture () in
  let foreign_function = typed foreign (List.nth foreign.samples 1) in
  reject "equal-looking foreign publication cannot replace original source"
    (R.make_provisional_advance
       ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
       ~namespace:f.namespace ~pending ~current:pending ~transition
       ~function_:foreign_function ());
  let fact =
    R.make_provisional_advance
      ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
      ~namespace:f.namespace ~pending ~current:pending ~transition
      ~function_:same_scope ()
    |> checked
  in
  let advanced = resolve ~previous:[ pending ] f fact |> checked |> single in
  reject "previously constructed phase fact cannot replay"
    (resolve ~previous:[ pending ] f fact);
  reject "consumed source/current cannot create a second advance"
    (R.make_provisional_advance
       ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
       ~namespace:f.namespace ~pending ~current:pending ~transition
       ~function_:same_scope ());
  let generic =
    R.make_pending_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~source:f.source
      ~function_:f.ordinary
    |> checked
  in
  reject "generic completed-header maker cannot bypass phase authority"
    (resolve ~previous:[ advanced ] f generic);
  let original = header_fact f advanced advanced |> checked in
  ignore (resolve ~previous:[ advanced ] f original |> checked)

let nested_source_and_current () =
  let f = fixture ~contents:"I64 F()#exe {I64 F(){}}{}" () in
  let beginnings =
    List.filter_map
      (function
        | Parser.Function_declared _, snapshot -> Some snapshot
        | _ -> None)
      f.events
  in
  let outer_start, inner_start =
    match beginnings with
    | [ outer; inner ] -> (outer, inner)
    | _ -> Alcotest.fail "expected nested source publications"
  in
  let outer_projection, outer = initial { f with samples = [ outer_start ] } in
  let inner_projection = typed f inner_start in
  let unchecked_join =
    R.make_provisional_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask
      ~function_:inner_projection
    |> checked
  in
  reject "new source cannot reuse a native head through ordinary name joining"
    (resolve ~previous:[ outer ] f unchecked_join);
  let transition =
    N.transition ~earlier:outer_start ~later:inner_start |> checked
  in
  let fact =
    R.make_provisional_advance
      ~compiler_option_mask:Compiler_option.initial_mask ~table:f.table
      ~namespace:f.namespace ~current:outer ~transition
      ~function_:inner_projection ()
    |> checked
  in
  let inner = resolve ~previous:[ outer ] f fact |> checked |> single in
  Alcotest.(check bool)
    "new nested source is not a repeated source-phase projection" true
    (Option.is_none (R.resolved_declaration_phase_source inner));
  same "nested source advances exact current native head" outer
    (Option.get (R.resolved_declaration_phase_current inner));
  let inner_snapshot, inner_source, inner_ordinary = List.hd f.headers in
  let inner_fixture =
    {
      f with
      final_snapshot = inner_snapshot;
      source = inner_source;
      ordinary = inner_ordinary;
    }
  in
  let fact = header_fact inner_fixture inner inner |> checked in
  let inner_header = resolve ~previous:[ inner ] f fact |> checked |> single in
  let inner_body =
    R.complete_pending ~table:f.table ~namespace:f.namespace
      ~pending:inner_header ~function_:inner_ordinary
    |> checked |> single
  in
  let transition =
    N.transition ~earlier:inner_snapshot ~later:f.final_snapshot |> checked
  in
  let callable_function =
    typed ~scope:(H.function_scope f.ordinary) f f.final_snapshot
  in
  let fact =
    R.make_header_advance ~table:f.table ~namespace:f.namespace ~pending:outer
      ~current:inner_body ~transition ~source:f.source ~function_:f.ordinary
      ~callable_function
    |> checked
  in
  let outer_header =
    resolve ~previous:[ inner_body ] f fact |> checked |> single
  in
  same "returning outer source retains its own source ancestry" outer
    (Option.get (R.resolved_declaration_phase_source outer_header));
  same "returning outer source retains nested current record" inner_body
    (Option.get (R.resolved_declaration_phase_current outer_header));
  same "native callable snapshot still names inner installed header"
    (N.source inner_start)
    (N.native_source
       (N.shape_snapshot
          (Option.get
             (H.function_provisional_call
                (R.resolved_declaration_header outer_header)))));
  Alcotest.(check bool)
    "outer header cannot undo nested executable state" true
    (R.declaration_site_state (R.resolved_declaration_site outer_header)
    = R.Resolved);
  ignore outer_projection

let stale_native_head () =
  let f =
    fixture ~contents:"I64 F(I64 n=#exe {}40)#exe {}{return n;}I64 F(){}" ()
  in
  let projection, pending = initial f in
  let _, _, _ = progress f projection pending in
  let ordinary =
    R.make_pending_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~source:f.source
      ~function_:f.ordinary
    |> checked
  in
  reject "ordinary new source cannot reuse a consumed native head"
    (resolve ~previous:[ pending ] f ordinary)

let fresh_native_allocation () =
  let f =
    fixture ~contents:"I64 F(I64 n=#exe {}40)#exe {}{return n;}I64 F()#exe {}{}"
      ()
  in
  let _, previous = initial f in
  let fresh = typed f (List.nth f.samples 2) in
  let fact =
    R.make_provisional_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~function_:fresh
    |> checked
  in
  let current = resolve ~previous:[ previous ] f fact |> checked |> single in
  same "fresh native allocation retains its own declaration identity"
    (H.function_symbol fresh)
    (R.resolved_declaration_identity_symbol current);
  Alcotest.(check bool)
    "fresh allocation cannot join an older unresolved semantic head" true
    (Option.is_none (R.resolved_declaration_retained_predecessor current))

let native_aot_rejection () =
  let f = fixture () in
  let function_ = typed f (List.hd f.samples) in
  let fact =
    R.make_provisional_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~function_
    |> checked
  in
  reject "JIT native provisional evidence cannot enter AOT resolution"
    (R.resolve ~table:f.table
       ~parent:(C.namespace_scope f.namespace)
       ~compilation_mode:R.Aot [ fact ]);
  let ordinary =
    R.make_pending_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~source:f.source
      ~function_:f.ordinary
    |> checked
  in
  ignore
    (R.resolve ~table:f.table
       ~parent:(C.namespace_scope f.namespace)
       ~compilation_mode:R.Aot [ ordinary ]
    |> checked)

let tests =
  [
    Alcotest.test_case
      "provisional progress real header and body have separate authority" `Quick
      complete_lifecycle;
    Alcotest.test_case
      "native forward proof rejects substitution backward and replay" `Quick
      reject_fabrication;
    Alcotest.test_case
      "returning source preserves nested current header and executable state"
      `Quick nested_source_and_current;
    Alcotest.test_case "ordinary resolution rejects consumed native heads"
      `Quick stale_native_head;
    Alcotest.test_case
      "fresh native allocations cannot follow semantic-only reuse" `Quick
      fresh_native_allocation;
    Alcotest.test_case
      "native phase evidence cannot enter ordinary AOT resolution" `Quick
      native_aot_rejection;
  ]
