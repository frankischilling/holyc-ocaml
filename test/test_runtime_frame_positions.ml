open Holyc_lib
module O = Test_integer_output
module VM = Ir_integer_interpreter

let layout_positions () =
  List.iter
    (fun mode ->
      List.iter
        (fun (body, position) ->
          let source =
            Printf.sprintf
              {|#exe {I64 N=3;class Span {$$=100+ #exe {I64 Marker(){%s return 0;};} $$;};StreamPrint("%%d;",sizeof(Span)+Marker()-%d);}|}
              body (58 + position)
          in
          ignore (O.run ~mode source |> O.expect ""))
        [
          ("U8 a[N];U8 b;", -4);
          ("U8 a;U8 b[N];U8 c;", -4);
          ("U8 a;U16 b[N];U8 c;", -8);
          ("U8 a;I32 b[N];U8 c;", -16);
          ("U8 a;I64 b[N];U8 c;", -32);
          ("I64 a[N];U8 b;I64 c;U8 d;", -40);
          ("U8 a[N][2];U8 b;", -8);
          ("I64 a[N],b[N];U8 c;", -48);
          ("U8 a[N],b[N];", 0);
          ("U8 a[N];static I64 b=0;U8 c;", -4);
          ("U8 a;static U8 b[N];U8 c;", -1);
          ("U8 a[N];{U16 b[N];}U8 c;", -12);
        ])
    Test_integer_globals.modes

let once_and_storage () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           {|#exe {
 I64 N=1;
 class Span {$$=66+ #exe {
   I64 Marker(){U8 head;I64 values[++N];U8 tail;
     values[0]=20;values[1]=22;return values[0]+values[1];};
 } $$;};
 StreamPrint("%d;",sizeof(Span)+Marker()+N-44);
}|}
        |> O.expect "");
      ignore
        (O.run ~mode
           {|#exe {
 extern U0 Print(U8 *fmt,...);
 I64 N=0;
 I64 Count(){Print("B");return ++N+1;};
 class Span {$$=66+ #exe {
   I64 Marker(){U8 head;I64 values[Count()];U8 tail;
     values[0]=20;values[1]=22;return values[0]+values[1];};
 } $$;};
 N=10;
 StreamPrint("%d;",sizeof(Span)+Marker()+Marker()+N-94);
}|}
        |> O.expect "B");
      ignore
        (O.run ~mode
           {|#exe {extern U0 Print(U8 *fmt,...);I64 N=7;
 class A {$$=N+ #exe {Print("A");I64 Noise(){I64 x[N];I64 y;return 0;};} $$+91;};
 StreamPrint("%d;",sizeof(A)+Noise());}|}
        |> O.expect "A"))
    Test_integer_globals.modes

let lookahead_positions () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.expect ""))
        [
          {|#exe {I64 N=1;class Span {$$=100+ #exe {I64 Marker(){U8 a[N #exe {N=3;}];U8 b;return 0;};} $$;};StreamPrint("%d;",sizeof(Span)+N+Marker()-57);}|};
          {|#exe {I64 N=3;class Span {$$=100+ #exe {I64 Marker(){U8 a[N];U8 #exe {class Nested {$$=19;};} b;return 0;};} $$;};StreamPrint("%d;",sizeof(Span)+Marker()-77);}|};
          {|#exe {I64 N=3;class Span {$$=100+ #exe {I64 Marker(){U8 a[N] #exe {class Nested {$$=19;};};return 0;};} $$;};StreamPrint("%d;",sizeof(Span)+Marker()-77);}|};
          {|#exe {I64 N=3;class Span {$$=100+ #exe {I64 Marker(){U8 a[N] #exe {class Nested {$$=19;};};U8 b;return 0;};} $$;};StreamPrint("%d;",sizeof(Span)+Marker()-54);}|};
        ])
    Test_integer_globals.modes

let derived_layouts () =
  List.iter
    (fun (offset, tail) ->
      let source =
        "#exe {I64 N=2;class Span {$$=" ^ offset
        ^ "+ #exe {I64 Marker(){I64 data[N];U8 last;return 0;};} $$;};" ^ tail
        ^ "StreamPrint(\"%d;\",F());}"
      in
      let session, config, source = Test_integer_functions.inputs source in
      let report =
        run_integer_program_report ~max_steps:10000 session ~config ~source
      in
      ignore (O.expect "" report);
      let definitions =
        integer_program_report_task_units report
        |> List.concat_map integer_program_functions
        |> List.filter (fun (definition : VM.function_definition) ->
            Semantic_symbol.name (Ir_function_body.symbol definition.body) = "F")
      in
      Alcotest.(check bool)
        "derived function was compiled" true (definitions <> []);
      List.iter
        (fun (definition : VM.function_definition) ->
          let dependencies =
            Ir_function_body.dimension_dependencies definition.body
          in
          Alcotest.(check bool)
            "derived layout retains its original runtime dimension" true
            (dependencies <> []);
          let first = List.hd dependencies in
          Alcotest.(check bool)
            "all derived reads retain the same original preparation" true
            (List.for_all (( == ) first) dependencies);
          match
            VM.execute_function ~max_steps:100 ~max_frame_bytes:1024
              ~frame:definition.frame ~arguments:[] definition.body
          with
          | Ok _ -> Alcotest.fail "frame-derived size escaped its owning task"
          | Error errors ->
              Alcotest.(check bool)
                "standalone execution rejects before any instruction" true
                (List.exists
                   (fun error ->
                     error.VM.code = "HCIRVM0026" && error.executed_steps = 0)
                   errors))
        definitions)
    [
      ("58", "I64 F(){return sizeof(Span);};");
      ("N+56", "I64 F(){return sizeof(Span);};");
      ("58", "class B {$$=sizeof(Span);};I64 F(){return sizeof(B);};");
      ("N+56", "class B {$$=sizeof(Span);};I64 F(){return sizeof(B);};");
      ("58", "U8 bytes[sizeof(Span)];I64 F(){return sizeof(bytes);};");
      ("58", "I64 F(){U8 bytes[sizeof(Span)];return sizeof(bytes);};");
      ("58", "I64 F(){static U8 bytes[sizeof(Span)];return 42;};");
    ]

let unsupported_storage () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           {|#exe {I64 N=3;class Span {$$=74+ #exe {I64 Marker(){U8 head;I64 *data[N];U8 last;return 0;};} $$;};StreamPrint("%d;",sizeof(Span)+Marker());}|}
        |> O.fault "HCIRVM0011");
      let diagnostic =
        O.run ~mode
          {|#exe {I64 N=2;class Span {$$=58+ #exe {I64 Marker(){I64 data[N];U8 last;return 0;};} $$;};class B {U8 data[sizeof(Span)];};StreamPrint("42;");}|}
        |> O.fault "HCRUN0001"
      in
      Alcotest.(check string)
        "runtime-dependent members still need original layout admission"
        "retained aggregate runtime bounds require original runtime layout \
         admission"
        diagnostic.message)
    Test_integer_globals.modes

let bounded_dependency_chain () =
  let module Frame = Holyc_lib__Sema.Function_frame_layout in
  let chain_length = 16 in
  let declarations =
    List.init chain_length (fun index ->
        Printf.sprintf "U8 a%d[sizeof(a%d)-sizeof(a%d)+1];" (index + 1) index
          index)
    |> String.concat ""
  in
  let text =
    Printf.sprintf
      {|#exe {I64 N=1;class Span {$$=%d+ #exe {I64 Marker(){U8 a0[N];%s U8 last;a%d[0]=42;return a%d[0];};} $$;};StreamPrint("%%d;",sizeof(Span)+Marker()-42);}|}
      (43 + chain_length) declarations chain_length chain_length
  in
  let session, config, source = Test_integer_functions.inputs text in
  let report =
    run_integer_program_report ~max_steps:10000 session ~config ~source
  in
  ignore (O.expect "" report);
  let definitions =
    integer_program_report_task_units report
    |> List.concat_map integer_program_functions
    |> List.filter (fun (definition : VM.function_definition) ->
        Semantic_symbol.name (Ir_function_body.symbol definition.body)
        = "Marker")
  in
  Alcotest.(check bool) "chain function was compiled" true (definitions <> []);
  List.iter
    (fun (definition : VM.function_definition) ->
      let dimensions =
        Frame.function_locations definition.frame
        |> List.concat_map Frame.location_dimensions
      in
      Alcotest.(check int)
        "all original array extents are present" (chain_length + 1)
        (List.length dimensions);
      List.iter
        (fun dimension ->
          Alcotest.(check int)
            "repeated size queries retain one original execution dependency" 1
            (List.length (Frame.dimension_runtime_dependencies dimension)))
        dimensions)
    definitions

let failed_bounds () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           {|#exe {extern U0 Print(U8 *fmt,...);I64 N=0;
 I64 Count(){Print("B");return 1/N;};
 class Span {$$=100+ #exe {I64 Marker(){U8 data[Count()];U8 last;return 0;};} $$;};
 Print("unreached");}|}
        |> O.fault ~output:"B" "HCIRVM0009");
      ignore
        (O.run ~mode
           {|#exe {extern U0 Print(U8 *fmt,...);
 I64 Count(){Print("B");return -1;};
 class Span {$$=100+ #exe {I64 Marker(){U8 data[Count()];U8 last;return 0;};} $$;};
 Print("unreached");}|}
        |> O.fault ~output:"B" "HCRUN0004"))
    Test_integer_globals.modes

let unexecuted_position_dimension () =
  let module C = Semantic_declaration_collection in
  let module N = Semantic_function_record_phase in
  let module Record = Semantic_compiler_record in
  let checked = Test_declaration_collection.checked in
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let positions =
    Record.create_compiler_positions ~sources:(Session.sources session)
  in
  let task = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace task namespace |> checked;
  let records = ref [] and dimensions = ref [] in
  let progress = ref None and offset_checks = ref 0 in
  let reject label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        let record = N.begin_header registry publication source |> checked in
        records := (source, record) :: !records
    | Parser.Function_position_written receipt ->
        Record.record_function_position positions
          (List.assq receipt.position_function !records)
          receipt
        |> checked
    | Parser.Function_local_allocated receipt ->
        let local_dimensions =
          match receipt.allocation_local.local_source with
          | Parser.Local_variable source ->
              List.map
                (fun source -> List.assq source !dimensions)
                source.local_array_dimensions
          | _ -> []
        in
        Record.record_local_allocation ~table ~namespace
          ~dimensions:local_dimensions positions
          (List.assq receipt.allocation_function !records)
          receipt
        |> checked
    | Parser.Array_dimension_completed receipt ->
        (* This proposes the same count/work as the literal bound, without
           executing a dimension in the task. Layout metadata is still allowed. *)
        let proposed =
          Record.propose_runtime_dimension ~namespace
            ~preparation:receipt.dimension_preparation ~count:3L ~work:1
          |> checked
        in
        let dimension =
          Record.complete_runtime_dimension ~table ~receipt ~queries:[] proposed
          |> checked
        in
        dimensions := (receipt.dimension_ast, dimension) :: !dimensions
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        progress :=
          Some
            (Record.begin_aggregate ~compiler_positions:positions ~table
               ~namespace publication
            |> checked)
    | Parser.Aggregate_advanced phase ->
        let current = Option.get !progress in
        (match phase.phase_step with
        | Parser.Aggregate_offset_reached _ ->
            let result, work =
              VM.prepare_task_aggregate_offset task ~table ~namespace
                ~queries:[] current phase
            in
            (match result with
            | Ok _ ->
                Alcotest.fail
                  "unexecuted dimension authorized a captured position"
            | Error message ->
                Alcotest.(check string)
                  "owning execution is required before numeric work"
                  "runtime array extent requires its owning task's successful \
                   original evaluation"
                  message);
            Alcotest.(check int) "rejected offset spends no preparation" 0 work;
            Alcotest.(check int)
              "rejected offset leaves task preparation unchanged" 0
              (VM.task_initializer_steps task);
            Alcotest.(check int)
              "rejected offset executes no instructions" 0
              (VM.task_executed_steps task);
            let metadata, _ =
              Record.prepare_aggregate_offset ~table ~namespace ~max_work:3
                ~queries:[] current phase
            in
            let metadata = checked metadata in
            Alcotest.(check int64)
              "layout metadata retains its derived bits" 42L
              (Record.aggregate_offset_value metadata);
            Alcotest.(check bool)
              "closed arithmetic retains runtime dependencies" true
              (Record.aggregate_offset_is_runtime metadata);
            reject "derived metadata cannot be charged as isolated closed work"
              (VM.charge_isolated_aggregate_offsets task ~table [ metadata ]);
            Alcotest.(check int)
              "isolated rejection also preserves preparation" 0
              (VM.task_initializer_steps task);
            incr offset_checks
        | _ -> ());
        Record.advance_aggregate
          ~dimensions:(fun source -> List.assoc_opt source !dimensions)
          current phase
        |> checked
    | _ ->
        List.iter
          (fun (_, record) ->
            if N.event_belongs record event then
              N.observe record event |> checked)
          !records);
    Ok ()
  in
  let commands = Test_provisional_function_parser.sink declaration in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      ~configure:(fun _ execution -> { execution with Parser.commands })
      "class Span {$$=46+ #exe {I64 Marker(){U8 data[3];U8 last;return 0;};} \
       $$;};"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check int) "original derived offset was checked" 1 !offset_checks

let tests =
  [
    Alcotest.test_case "runtime local extents determine aligned positions"
      `Quick layout_positions;
    Alcotest.test_case "bounds execute once and storage uses the saved extent"
      `Quick once_and_storage;
    Alcotest.test_case "lookahead and local iteration preserve original writes"
      `Quick lookahead_positions;
    Alcotest.test_case "derived sizes retain their owning dimension execution"
      `Quick derived_layouts;
    Alcotest.test_case "unimplemented storage retains explicit boundaries"
      `Quick unsupported_storage;
    Alcotest.test_case "dependent size chains retain bounded proof lists" `Quick
      bounded_dependency_chain;
    Alcotest.test_case "captured positions require actual dimension execution"
      `Quick unexecuted_position_dimension;
    Alcotest.test_case "failed bounds retain effects without later allocation"
      `Quick failed_bounds;
  ]
