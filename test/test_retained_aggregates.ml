open Holyc_lib
module O = Test_integer_output
module C = Semantic_declaration_collection
module Record = Holyc_lib__Sema.Compiler_record

let checked = Test_declaration_collection.checked

let source_gates () =
  List.iter
    (fun mode ->
      List.iter
        (fun text -> ignore (O.run ~mode text |> O.expect ""))
        [
          {|#exe {class Pair {I64 a;I64 b;};StreamPrint("%d;",sizeof(Pair)+26);}|};
          {|#exe {class Pair {I64 a;I64 b;};}#exe {StreamPrint("%d;",sizeof(Pair)+26);}|};
          {|#exe {class Pair {I64 a;I64 b;};I64 Saved(){return sizeof(Pair);};class Pair {U8 small;};StreamPrint("%d;",Saved()+sizeof(Pair)+25);}|};
          {|#exe {class Packed {U8 a;I16 b;};union Wide {U8 bytes[2];I64 word;};StreamPrint("%d;",sizeof(Packed)+sizeof(Wide)+31);}|};
          {|#exe {class Mixed {I64 first;union {U8 byte;I32 word;}U8 last;};StreamPrint("%d;",sizeof(Mixed)+29);}|};
          {|#exe {class Rows {U8 bytes[2][3];};I64 values[sizeof(Rows)];StreamPrint("%d;",sizeof(values)-6);}|};
          {|#exe {class Pair {I64 a;I64 b;};class Sized {U8 bytes[sizeof(Pair)];};StreamPrint("%d;",sizeof(Sized)+26);}|};
          {|#exe {class Pair {I64 a;I64 b;};class Sized {U8 bytes[sizeof(Pair) #exe {class Pair {U8 b;};}];};StreamPrint("%d;",sizeof(Sized)+sizeof(Pair)+25);}|};
          {|#exe {extern class Pair;I64 Before=sizeof(Pair);class Pair {I64 a;I64 b;};StreamPrint("%d;",Before+sizeof(Pair)+26);}|};
          {|#exe {class Empty {};StreamPrint("%d;",sizeof(Empty)+42);}|};
        ])
    Test_integer_globals.modes;
  ignore
    (O.run ~mode:Preprocessor.Jit
       {|class Pair {I64 a;I64 b;};#exe {StreamPrint("%d;",sizeof(Pair)+26);}|}
    |> O.expect "")

let boundaries () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode {|#exe {class Base {I64 a;};class Child : Base {U8 b;};}|}
        |> O.fault "HCRUN0001");
      ignore
        (O.run ~mode {|#exe {class A {I64 x;};class B {A a;};}|}
        |> O.fault "HCRUN0001");
      ignore
        (O.run ~mode {|#exe {class A {$$=8;I64 x;};}|} |> O.fault "HCRUN0001");
      ignore
        (O.run ~mode {|#exe {class A {U0 x[9223372036854775807][2];};}|}
        |> O.fault "HCRUN0001");
      ignore
        (O.run ~mode {|#exe {class Bad {I64 a[-1];};}|} |> O.fault "HCRUN0004"))
    Test_integer_globals.modes

let partial_sizes () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.expect ""))
        [
          {|#exe {I64 Seen=0;class Pair #exe {Seen=sizeof(Pair);} {I64 a;#exe {Seen+=sizeof(Pair);} I64 b;}#exe {Seen+=sizeof(Pair);};StreamPrint("%d;",Seen+18);}|};
          {|#exe {I64 Seen=0;class Pair {I64 a #exe {Seen=sizeof(Pair);};#exe {Seen+=sizeof(Pair);} I64 b;};StreamPrint("%d;",Seen+34);}|};
          {|#exe {class Row {I64 a;U8 b[sizeof(Row)];I64 c;};StreamPrint("%d;",sizeof(Row)+18);}|};
          {|#exe {class Zero {I64 values[9223372036854775807][0];};StreamPrint("%d;",sizeof(Zero)+42);}|};
          {|#exe {I64 Seen=0;class Mixed {I16 head;union {U8 a;#exe {Seen=sizeof(Mixed);} I64 b;#exe {Seen+=sizeof(Mixed);} }U8 tail;};StreamPrint("%d;",Seen+sizeof(Mixed)+18);}|};
          {|#exe {I64 Seen=0;union U {U8 a;#exe {Seen=sizeof(U);} I64 b;#exe {Seen+=sizeof(U);} I32 c;};StreamPrint("%d;",Seen+sizeof(U)+25);}|};
          {|#exe {I64 Seen=0;extern class Pair #exe {Seen=sizeof(Pair);};StreamPrint("%d;",Seen+42);}|};
          {|#exe {class Pair {I64 a;#exe {I64 Saved(){return sizeof(Pair);}} I64 b;};StreamPrint("%d;",Saved()+sizeof(Pair)+18);}|};
          {|#exe {class Pair {I64 a;#exe {I64 Saved(){return sizeof(Pair);}class Pair {U8 b;};}I64 c;};StreamPrint("%d;",Saved()+sizeof(Pair)+33);}|};
        ])
    Test_integer_globals.modes;
  ignore
    (O.run ~mode:Preprocessor.Jit
       {|I64 Seen=0;class Pair #exe {Seen=sizeof(Pair);} {I64 a;#exe {Seen+=sizeof(Pair);}I64 b;}#exe {Seen+=sizeof(Pair);};Seen+18;|}
    |> O.expect "")

let source_authority () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let other = C.create_namespace ~table () |> checked in
  let publication = ref None and completion = ref None and observed = ref 0 in
  let reject label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  let declaration = function
    | Parser.Aggregate_declared source ->
        Alcotest.(check bool)
          "publication callback is live" true
          (Parser.aggregate_publication_is_current source);
        publication := Some (C.publish_aggregate namespace source |> checked);
        incr observed;
        Ok ()
    | Parser.Aggregate_completed receipt ->
        let publication = Option.get !publication in
        reject "another namespace cannot acquire original aggregate layout"
          (Record.complete_aggregate ~table ~namespace:other publication receipt);
        let symbol = C.publication_symbol publication in
        let forged =
          C.publish namespace
            ~name:(Semantic_symbol.name symbol)
            ~kind:Semantic_symbol.Aggregate_type
            ~origin:(Semantic_symbol.origin symbol)
          |> checked
        in
        reject "equal declaration metadata has no original source association"
          (Record.complete_aggregate ~table ~namespace forged receipt);
        ignore
          (Record.complete_aggregate ~table ~namespace publication receipt
          |> checked);
        completion := Some receipt;
        incr observed;
        Ok ()
    | _ -> Ok ()
  in
  let source =
    Session.add_source session ~path:"aggregate-authority.hc"
      ~contents:"class Pair {I64 a;I64 b;};"
  in
  let commands = Test_provisional_function_parser.sink declaration in
  ignore
    (Parser.parse ~commands ~sources:(Session.sources session)
       ~symbols:(Session.symbols session)
       ~definitions:(Session.definitions session)
       ~config:(Preprocessor.Config.create () |> checked)
       source
    |> Test_parser.expect_ast);
  Alcotest.(check int) "both original callbacks ran" 2 !observed;
  let receipt = Option.get !completion in
  reject "expired completion cannot recompute class metadata"
    (Record.complete_aggregate ~table ~namespace (Option.get !publication)
       receipt);
  reject "expired publication cannot mint another association"
    (C.publish_aggregate namespace receipt.aggregate_publication)

let phase_authority () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let other = C.create_namespace ~table () |> checked in
  let publication = ref None and progress = ref None and skipped = ref None in
  let foreign = ref None and last = ref None and phases = ref 0 in
  let reject = Test_task_declarations.reject in
  let advance = Record.advance_aggregate ~dimensions:(fun _ -> None) in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let pub = C.publish_aggregate namespace source |> checked in
        reject "partial metadata cannot use another namespace"
          (Record.begin_aggregate ~table ~namespace:other pub);
        publication := Some pub;
        progress :=
          Some (Record.begin_aggregate ~table ~namespace pub |> checked);
        skipped := Some (Record.begin_aggregate ~table ~namespace pub |> checked);
        Ok ()
    | Parser.Aggregate_advanced phase ->
        let current = Option.get !progress in
        let before = Record.aggregate_metadata current |> checked in
        Option.iter
          (fun foreign ->
            reject "another aggregate's phase is rejected"
              (advance foreign phase))
          !foreign;
        if Option.is_some phase.phase_predecessor then
          reject
            "missing predecessor is rejected while the original callback is \
             live"
            (advance (Option.get !skipped) phase);
        ignore (advance current phase |> checked);
        let after = Record.aggregate_metadata current |> checked in
        reject "live phase cannot be applied twice" (advance current phase);
        Alcotest.(check bool)
          "failed replay leaves the authentic snapshot unchanged" true
          (Record.aggregate_metadata current |> checked == after);
        (match phase.phase_step with
        | Parser.Aggregate_member_prepared _ ->
            Alcotest.(check bool)
              "successful member placement creates a new immutable snapshot"
              false (before == after)
        | _ -> ());
        last := Some phase;
        incr phases;
        Ok ()
    | Parser.Aggregate_completed receipt ->
        let pub = Option.get !publication in
        reject "completion cannot omit the original phase chain"
          (Record.complete_aggregate ~progress:(Option.get !skipped) ~table
             ~namespace pub receipt);
        ignore
          (Record.complete_aggregate ~progress:(Option.get !progress) ~table
             ~namespace pub receipt
          |> checked);
        reject "completed progress cannot be consumed again"
          (Record.complete_aggregate ~progress:(Option.get !progress) ~table
             ~namespace pub receipt);
        foreign := !skipped;
        Ok ()
    | _ -> Ok ()
  in
  let source =
    Session.add_source session ~path:"aggregate-phases.hc"
      ~contents:"class Pair {I64 a;union {I64 b;}U8 c;};class Empty {};"
  in
  let commands = Test_provisional_function_parser.sink declaration in
  ignore
    (Parser.parse ~commands ~sources:(Session.sources session)
       ~symbols:(Session.symbols session)
       ~definitions:(Session.definitions session)
       ~config:(Preprocessor.Config.create () |> checked)
       source
    |> Test_parser.expect_ast);
  Alcotest.(check int) "all member and union boundaries observed" 7 !phases;
  let phase = Option.get !last in
  Alcotest.(check bool)
    "phase lifetime ends on return" false
    (Parser.aggregate_phase_is_current phase);
  reject "expired phase cannot advance an unfinished progress object"
    (advance (Option.get !skipped) phase)

let snapshot_authority () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let progress = ref None
  and first_snapshot = ref None
  and first_read = ref None in
  let current_read = ref None and completed = ref [] and last_root = ref None in
  let reads = ref 0 in
  let reject = Test_task_declarations.reject in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        progress :=
          Some (Record.begin_aggregate ~table ~namespace publication |> checked);
        Ok ()
    | Parser.Aggregate_advanced phase ->
        Record.advance_aggregate
          ~dimensions:(fun _ -> None)
          (Option.get !progress) phase
        |> checked;
        Ok ()
    | _ -> Ok ()
  in
  let query = function
    | Parser.Query_root root ->
        let snapshot =
          Record.aggregate_metadata (Option.get !progress) |> checked
        in
        Option.iter
          (fun stale ->
            reject "stale snapshot cannot authorize a later live query"
              (Record.read_sizeof ~table ~root stale))
          !first_snapshot;
        let read = Record.read_sizeof ~table ~root snapshot |> checked in
        if !reads = 0 then (
          first_snapshot := Some snapshot;
          first_read := Some read);
        current_read := Some read;
        last_root := Some root;
        incr reads;
        Ok ()
    | Parser.Query_completed receipt ->
        completed := (receipt, Option.get !current_read) :: !completed;
        Ok ()
    | _ -> Ok ()
  in
  let commands =
    {
      (Test_provisional_function_parser.sink declaration) with
      query = Some query;
    }
  in
  let source =
    Session.add_source session ~path:"aggregate-snapshots.hc"
      ~contents:
        "class Pair {I64 a Tag sizeof(Pair);I64 b Tag \
         sizeof(Pair);};sizeof(Pair);"
  in
  ignore
    (Parser.parse ~commands ~sources:(Session.sources session)
       ~symbols:(Session.symbols session)
       ~definitions:(Session.definitions session)
       ~config:(Preprocessor.Config.create () |> checked)
       source
    |> Test_parser.expect_ast);
  Alcotest.(check int) "three original queries" 3 !reads;
  Alcotest.(check int64)
    "consumed partial value remains frozen" 8L
    (Record.sizeof_value (Option.get !first_read) ~pointer:false);
  List.iter
    (fun (receipt, read) ->
      ignore (Record.complete_sizeof ~table ~receipt read |> checked))
    !completed;
  reject "even the current snapshot cannot bind an expired query root"
    (Record.read_sizeof ~table ~root:(Option.get !last_root)
       (Record.aggregate_metadata (Option.get !progress) |> checked))

let command_authority () =
  let module D = Task_declarations in
  let module T = Test_task_declarations in
  let session, ledger = T.setup () in
  let output, events =
    T.parse session ledger "class Pair {I64 a;I64 b;};class Byte {U8 a;};"
  in
  let ast = Test_parser.expect_ast output in
  let first = List.nth ast.items 0 and second = List.nth ast.items 1 in
  List.iter
    (fun items ->
      T.reject "aggregate declarations require their original whole command"
        (D.seal ledger (T.copy_module ast items)))
    [ [ second; first ]; [ first; first ]; [ first ]; ast.items ];
  let command = D.seal ledger ast |> T.expect in
  let collection =
    D.collection ~table:(Session.semantic_symbols session) ~ast command
    |> T.expect
  in
  Alcotest.(check int)
    "both original aggregate publications retained" 2
    (List.length (C.entries collection));
  List.iter
    (fun event ->
      T.reject "aggregate event replay cannot republish layout"
        (D.observe ledger event))
    events;
  Alcotest.(check bool)
    "failed substitutions preserve original seal" true
    (D.seal ledger ast |> T.expect == command)

let failed_forward () =
  let module T = Test_task_declarations in
  let session, ledger = T.setup () in
  let output, events = T.parse session ledger "extern class Missing" in
  Alcotest.(check bool)
    "malformed forward still fails parsing" true (Parser.has_errors output);
  Alcotest.(check int)
    "only the reached publication is emitted" 1 (List.length events);
  match events with
  | [ Parser.Aggregate_declared source ] ->
      Alcotest.(check bool)
        "failed publication lifetime ends" false
        (Parser.aggregate_publication_is_current source)
  | _ -> Alcotest.fail "malformed forward acquired completion"

let tests =
  [
    Alcotest.test_case "completed layouts survive directives and replacements"
      `Quick source_gates;
    Alcotest.test_case "dependent layouts remain explicit" `Quick boundaries;
    Alcotest.test_case "partial sizes follow original member phases" `Quick
      partial_sizes;
    Alcotest.test_case "metadata requires original live aggregate receipts"
      `Quick source_authority;
    Alcotest.test_case "partial metadata authenticates every original phase"
      `Quick phase_authority;
    Alcotest.test_case "new queries reject stale partial snapshots" `Quick
      snapshot_authority;
    Alcotest.test_case "aggregate commands reject substitution and replay"
      `Quick command_authority;
    Alcotest.test_case "malformed forward cannot complete" `Quick failed_forward;
  ]
