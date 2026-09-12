open Holyc_lib

let same label left right = Alcotest.(check bool) label true (left == right)

let shape defaults : Symbol_visibility.function_call_shape =
  {
    parameters =
      List.mapi
        (fun index has_default ->
          {
            Symbol_visibility.parameter_name = Some (string_of_int index);
            has_default;
          })
        defaults;
    variadic = false;
  }

let parse ?(on_enter = fun () -> ()) ?(reference = fun _ -> Ok ())
    ?selected_shape call source =
  let session = Session.create () in
  ignore
    (Symbol_visibility.Environment.add (Session.symbols session) ~name:"F"
       ~kind:Symbol_visibility.Function ?function_call_shape:selected_shape ());
  let commands : Parser.command_sink =
    {
      checkpoint = None;
      reference = Some reference;
      call = Some call;
      implicit_output = None;
      query = None;
      declaration = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands ~on_enter source
  in
  parsed

let expression parsed =
  match (Test_parser.expect_ast parsed).Ast.items with
  | [ Ast.Top_level_statement (Ast.Expression_statement statement) ] ->
      statement.expression_statement_expression
  | _ -> Alcotest.fail "expected one expression statement"

let phases_and_identity () =
  let seen = ref [] and reference = ref None and started = ref None in
  let emitted = ref None and entered = ref 0 in
  let record event = seen := event :: !seen in
  let call : Parser.direct_call_sink =
    {
      start =
        (fun receipt ->
          record "start";
          Alcotest.(check bool)
            "start is live" true
            (Parser.call_start_is_current receipt);
          same "original reference retained" (Option.get !reference)
            receipt.call_reference;
          same "callee retains original identifier"
            (Parser.selected_identifier receipt.call_reference)
            (match receipt.call_callee with
            | Ast.Identifier_expression id -> id
            | _ -> assert false);
          started := Some receipt;
          Ok (Some (shape [ false ])));
      emit =
        (fun receipt ->
          record "emit";
          Alcotest.(check bool)
            "emission is live" true
            (Parser.call_emission_is_current receipt);
          same "original call start retained" (Option.get !started)
            receipt.call_start;
          Alcotest.(check bool)
            "start cannot replay in emission" false
            (Parser.call_start_is_current receipt.call_start);
          emitted := Some receipt;
          Ok ());
    }
  in
  let parsed =
    parse call ~selected_shape:(shape [])
      ~reference:(fun receipt ->
        reference := Some receipt;
        record "reference";
        Ok ())
      ~on_enter:(fun () ->
        incr entered;
        record ("exe" ^ string_of_int !entered))
      "F#exe {}(#exe {}7)#exe {};"
  in
  let node = expression parsed in
  let receipt = Option.get !emitted in
  same "completed receipt owns exact expression" node receipt.call_expression;
  let ast = Test_parser.expect_call_expression node in
  same "call owns exact original callee" (Option.get !started).call_callee
    ast.call_callee;
  (match ast.call_syntax with
  | Ast.Parenthesized_call syntax ->
      same "opening location is original child"
        (Option.get (Option.get !started).call_opening_parenthesis)
        syntax.opening_parenthesis
  | _ -> Alcotest.fail "expected parentheses");
  Alcotest.(check (list string))
    "original lexer phases"
    [ "reference"; "exe1"; "start"; "exe2"; "exe3"; "emit" ]
    (List.rev !seen);
  Alcotest.(check bool)
    "emission cannot replay" false
    (Parser.call_emission_is_current receipt)

let capture_before_opening () =
  let entered = ref 0 and available = ref (shape []) and count = ref (-1) in
  let call : Parser.direct_call_sink =
    {
      start =
        (fun _ ->
          count := List.length !available.parameters;
          Ok (Some !available));
      emit = (fun _ -> Ok ());
    }
  in
  let parsed =
    parse call ~selected_shape:(shape [])
      ~on_enter:(fun () ->
        incr entered;
        available := shape (if !entered = 1 then [ false ] else []))
      "F#exe {}(#exe {}7);"
  in
  ignore (expression parsed);
  Alcotest.(check int)
    "after-name shape captured before opening lookahead" 1 !count;
  Alcotest.(check int) "both directives ran" 2 !entered

let surplus_stops () =
  let entered = ref 0 and emitted = ref 0 in
  let call : Parser.direct_call_sink =
    {
      start = (fun _ -> Ok (Some (shape [])));
      emit =
        (fun _ ->
          incr emitted;
          Ok ());
    }
  in
  let parsed = parse call ~on_enter:(fun () -> incr entered) "F(7#exe {});" in
  Alcotest.(check bool)
    "surplus argument rejected" true (Parser.has_errors parsed);
  Alcotest.(check int) "surplus expression was never traversed" 0 !entered;
  Alcotest.(check int) "failed call never emitted" 0 !emitted

let parenthesis_free_defaults () =
  let call : Parser.direct_call_sink =
    {
      start =
        (fun receipt ->
          Alcotest.(check bool)
            "no invented opening" true
            (Option.is_none receipt.call_opening_parenthesis);
          Ok (Some (shape [ true; false; true ])));
      emit = (fun _ -> Ok ());
    }
  in
  let ast =
    Test_parser.expect_call_expression (expression (parse call "F 7;"))
  in
  Test_parser.expect_parenthesis_free_call ast;
  Alcotest.(check int) "fixed slots retained" 3 (List.length ast.call_arguments);
  Alcotest.(check (list bool))
    "defaults omitted in place" [ true; false; true ]
    (List.map
       (fun (arg : Ast.call_argument) ->
         arg.call_argument_value = Ast.Omitted_call_argument)
       ast.call_arguments)

let provisional_is_call () =
  let starts = ref 0 in
  let call : Parser.direct_call_sink =
    {
      start =
        (fun _ ->
          incr starts;
          Ok None);
      emit = (fun _ -> Ok ());
    }
  in
  let parsed = parse call "F(I64);" in
  let ast = Test_parser.expect_call_expression (expression parsed) in
  Alcotest.(check int)
    "type token stays in the call argument grammar" 1
    (List.length ast.call_arguments);
  (match (List.hd ast.call_arguments).call_argument_value with
  | Ast.Provided_call_argument (Ast.Identifier_expression identifier) ->
      Alcotest.(check string)
        "original hosted type-name token retained" "I64" identifier.spelling
  | _ -> Alcotest.fail "expected the original identifier argument");
  Alcotest.(check int)
    "provisional direct function started before type lookahead" 1 !starts

let function_address_cast selected_shape () =
  let starts = ref 0 and emissions = ref 0 and selected = ref None in
  let entered = ref 0 in
  let call : Parser.direct_call_sink =
    {
      start =
        (fun _ ->
          incr starts;
          Ok None);
      emit =
        (fun _ ->
          incr emissions;
          Ok ());
    }
  in
  let parsed =
    parse call ?selected_shape
      ~reference:(fun reference ->
        selected := Some reference;
        Ok ())
      ~on_enter:(fun () -> incr entered)
      "&F#exe {}(I64)#exe {};"
  in
  Alcotest.(check int) "address does not start a direct call" 0 !starts;
  Alcotest.(check int) "address does not emit a direct call" 0 !emissions;
  let cast = Test_parser.expect_postfix_cast_expression (expression parsed) in
  let address = Test_parser.expect_prefix_expression cast.cast_operand in
  Alcotest.(check bool)
    "cast applies to the function address" true
    (address.prefix_operator_kind = Ast.Address_of);
  (match address.prefix_operand with
  | Ast.Identifier_expression identifier ->
      same "address retains original selected identifier" identifier
        (Parser.selected_identifier (Option.get !selected))
  | _ -> Alcotest.fail "expected the function identifier inside its address");
  Alcotest.(check int) "address and cast retain existing lookahead" 2 !entered

let ordinary_address_cast () =
  let call : Parser.direct_call_sink =
    {
      start = (fun _ -> Alcotest.fail "an ordinary address cannot start a call");
      emit = (fun _ -> Alcotest.fail "an ordinary address cannot emit a call");
    }
  in
  let address =
    Test_parser.expect_prefix_expression
      (expression (parse call "&value(I64);"))
  in
  Alcotest.(check bool)
    "ordinary address keeps unary precedence" true
    (address.prefix_operator_kind = Ast.Address_of);
  let cast =
    Test_parser.expect_postfix_cast_expression address.prefix_operand
  in
  match cast.cast_operand with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string)
        "ordinary cast operand" "value" identifier.spelling
  | _ -> Alcotest.fail "expected the ordinary identifier operand"

let function_address_without_callbacks provisional () =
  let session = Session.create () in
  if provisional then
    ignore
      (Symbol_visibility.Environment.add (Session.symbols session) ~name:"F"
         ~kind:Symbol_visibility.Function ());
  let source =
    Session.add_source session ~path:"plain-address.HC"
      ~contents:(if provisional then "&F(I64);" else "I64 F(); &F(I64);")
  in
  let parsed =
    Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Test_parser.config (Sys.getcwd ()))
      source
  in
  let expression =
    match List.rev (Test_parser.expect_ast parsed).Ast.items with
    | Ast.Top_level_statement (Ast.Expression_statement statement) :: _ ->
        statement.expression_statement_expression
    | _ -> Alcotest.fail "expected the address expression after any declaration"
  in
  let cast = Test_parser.expect_postfix_cast_expression expression in
  let address = Test_parser.expect_prefix_expression cast.cast_operand in
  Alcotest.(check bool)
    "plain parser casts the function address" true
    (address.prefix_operator_kind = Ast.Address_of);
  match address.prefix_operand with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string)
        "plain parser keeps direct function name" "F" identifier.spelling
  | _ -> Alcotest.fail "plain parser must keep the function address, not a call"

let supplied_arguments variadic defaults source omissions commas () =
  let call : Parser.direct_call_sink =
    {
      start = (fun _ -> Ok (Some { (shape defaults) with variadic }));
      emit = (fun _ -> Ok ());
    }
  in
  let ast =
    Test_parser.expect_call_expression (expression (parse call source))
  in
  Alcotest.(check (list bool))
    "native provided/default slots" omissions
    (List.map
       (fun (arg : Ast.call_argument) ->
         arg.call_argument_value = Ast.Omitted_call_argument)
       ast.call_arguments);
  Alcotest.(check (list bool))
    "only consumed delimiters attached" commas
    (List.map
       (fun (arg : Ast.call_argument) -> Option.is_some arg.following_comma)
       ast.call_arguments)

let malformed_shape variadic defaults source expected_entries () =
  let entered = ref 0 and emitted = ref 0 in
  let call : Parser.direct_call_sink =
    {
      start = (fun _ -> Ok (Some { (shape defaults) with variadic }));
      emit =
        (fun _ ->
          incr emitted;
          Ok ());
    }
  in
  let parsed = parse call ~on_enter:(fun () -> incr entered) source in
  Alcotest.(check bool)
    "malformed supplied grammar rejected" true (Parser.has_errors parsed);
  Alcotest.(check int)
    "rejection preserves native lexer boundary" expected_entries !entered;
  Alcotest.(check int) "no emission on malformed call" 0 !emitted

exception Observer_failure

let callback_failure exceptional at_start () =
  let entered = ref 0 and active = ref None in
  let fail current expression =
    active := Some current;
    Alcotest.(check bool) "failing callback is live" true (current ());
    if exceptional then raise Observer_failure;
    Error
      [
        Diagnostic.make ~code:"TESTCALL" ~severity:Diagnostic.Error
          ~primary:(Ast.expression_location expression).span
          ~message:"call rejected" ();
      ]
  in
  let call : Parser.direct_call_sink =
    {
      start =
        (fun receipt ->
          if at_start then
            fail
              (fun () -> Parser.call_start_is_current receipt)
              receipt.call_callee
          else Ok (Some (shape [])));
      emit =
        (fun receipt ->
          fail
            (fun () -> Parser.call_emission_is_current receipt)
            receipt.call_expression);
    }
  in
  let run () =
    parse call
      ~on_enter:(fun () -> incr entered)
      (if at_start then "F(#exe {});" else "F()#exe {};#exe {}")
  in
  if exceptional then
    match run () with
    | _ -> Alcotest.fail "exception must escape"
    | exception Observer_failure -> ()
  else
    Alcotest.(check bool)
      "callback rejection stops parse" true
      (Parser.has_errors (run ()));
  Alcotest.(check bool)
    "failed callback released" false
    ((Option.get !active) ());
  Alcotest.(check int)
    "no later lexer read"
    (if at_start then 0 else 1)
    !entered

let tests =
  [
    Alcotest.test_case "direct call phases and original AST identity" `Quick
      phases_and_identity;
    Alcotest.test_case "shape captured between name and opening lookahead"
      `Quick capture_before_opening;
    Alcotest.test_case "zero-count surplus fails before argument traversal"
      `Quick surplus_stops;
    Alcotest.test_case "parenthesis-free defaults use supplied shape" `Quick
      parenthesis_free_defaults;
    Alcotest.test_case "provisional function remains a call" `Quick
      provisional_is_call;
    Alcotest.test_case
      "provisional function address casts without call receipts" `Quick
      (function_address_cast None);
    Alcotest.test_case "shaped function address casts without call receipts"
      `Quick
      (function_address_cast (Some (shape [])));
    Alcotest.test_case "ordinary address retains unary cast precedence" `Quick
      ordinary_address_cast;
    Alcotest.test_case "declared function address casts in a plain parser"
      `Quick
      (function_address_without_callbacks false);
    Alcotest.test_case "provisional function address casts in a plain parser"
      `Quick
      (function_address_without_callbacks true);
    Alcotest.test_case "closing parenthesis supplies all remaining defaults"
      `Quick
      (supplied_arguments false [ true; true ] "F();" [ true; true ]
         [ false; false ]);
    Alcotest.test_case "fixed omitted slots preserve native comma" `Quick
      (supplied_arguments false [ true; true ] "F(,);" [ true; true ]
         [ true; false ]);
    Alcotest.test_case "variadic traversal starts after fixed members" `Quick
      (supplied_arguments true [ false ] "F(1,2,3);" [ false; false; false ]
         [ true; true; false ]);
    Alcotest.test_case "zero-fixed variadic traversal accepts first argument"
      `Quick
      (supplied_arguments true [] "F(1,2);" [ false; false ] [ true; false ]);
    Alcotest.test_case "surplus comma is not consumed" `Quick
      (malformed_shape false [ false ] "F(1,#exe {}2);" 0);
    Alcotest.test_case "required argument cannot be omitted" `Quick
      (malformed_shape false [ false ] "F(,#exe {});" 0);
    Alcotest.test_case "variadic argument cannot be omitted" `Quick
      (malformed_shape true [] "F(,#exe {});" 0);
    Alcotest.test_case "trailing variadic comma requires an expression" `Quick
      (malformed_shape true [ false ] "F(1,2,)#exe {};" 0);
    Alcotest.test_case "missing fixed separator stops before later lookahead"
      `Quick
      (malformed_shape false [ false; false ] "F(1 2#exe {});" 0);
  ]
  @ List.concat_map
      (fun at_start ->
        List.map
          (fun exceptional ->
            Alcotest.test_case
              ((if at_start then "start" else "emission")
              ^ if exceptional then " exception" else " rejection")
              `Quick
              (callback_failure exceptional at_start))
          [ false; true ])
      [ true; false ]
