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
        (O.run ~mode
           {|#exe {class Pair {I64 a;}#exe {StreamPrint("%d;",sizeof(Pair));};}|}
        |> O.fault "HCRUN0004");
      ignore
        (O.run ~mode {|#exe {class Base {I64 a;};class Child : Base {U8 b;};}|}
        |> O.fault "HCRUN0001");
      ignore
        (O.run ~mode {|#exe {class A {I64 x;};class B {A a;};}|}
        |> O.fault "HCRUN0001");
      ignore
        (O.run ~mode {|#exe {class A {$$=8;I64 x;};}|} |> O.fault "HCRUN0001");
      ignore
        (O.run ~mode {|#exe {class Bad {I64 a[-1];};}|} |> O.fault "HCRUN0004"))
    Test_integer_globals.modes

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
    Alcotest.test_case "partial and dependent layouts remain explicit" `Quick
      boundaries;
    Alcotest.test_case "metadata requires original live aggregate receipts"
      `Quick source_authority;
    Alcotest.test_case "aggregate commands reject substitution and replay"
      `Quick command_authority;
    Alcotest.test_case "malformed forward cannot complete" `Quick failed_forward;
  ]
