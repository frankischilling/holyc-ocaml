open Holyc_lib
module Cases = Aggregate_member_cases
module Callbacks = Callback_field_cases
module Refs = Reference_return_cases
module Arrays = Aggregate_array_cases
module Pointers = Aggregate_pointer_cases
module Casts = Aggregate_cast_cases
module Inherited = Inherited_aggregate_cases
module Backed = Backed_aggregate_cases
module Default = Default_aggregate_cases
module Parameters = Class_parameter_cases
module Returns = Class_return_cases
module Persistent = Persistent_aggregate_cases
module Defaults = Class_default_cases
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"aggregate-members.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  (session, config, source)

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (describe errors)

let run ?max_global_bytes ?max_frame_bytes ?max_initializer_steps
    ?(max_steps = 100_000) mode contents =
  let session, config, source = inputs mode contents in
  run_integer_program_report ?max_global_bytes ?max_frame_bytes
    ?max_initializer_steps session ~config ~source ~max_steps

let value expected output report =
  let result = (integer_program_report_outcome report |> checked).value in
  Alcotest.(check (option int64))
    "independent expected word" (Some expected)
    (Option.map (fun w -> w.VM.bits) (VM.final_value result));
  Alcotest.(check string)
    "reached output" output
    (integer_program_report_output_bytes report);
  result

let discarded_reference_returns () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, source) ->
          let report = run mode source in
          let result =
            (integer_program_report_outcome report |> checked).value
          in
          Alcotest.(check bool)
            name true
            (Option.is_none (VM.final_value result));
          Alcotest.(check string)
            "discarded return has no output" ""
            (integer_program_report_output_bytes report))
        Refs.discarded_values)
    modes

let failure code output report =
  (match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors));
  Alcotest.(check string)
    "fault preserves reached output" output
    (integer_program_report_output_bytes report)

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, source, code, output) ->
          let report = run mode source in
          Alcotest.(check bool)
            name true
            (match integer_program_report_outcome report with
            | Error errors ->
                List.exists (fun (d : Diagnostic.t) -> d.code = code) errors
            | Ok _ -> false);
          failure code output report)
        (Refs.faults @ Callbacks.faults @ Cases.faults @ Cases.pointer_faults
       @ Arrays.faults @ Pointers.faults @ Inherited.faults @ Backed.faults
       @ Backed.source_faults @ Default.faults @ Parameters.faults
       @ Returns.faults @ Persistent.faults @ Persistent.pointer_faults
       @ Casts.faults);
      List.iter
        (fun (definition, bytes) ->
          ignore
            (value 42L ""
               (run mode (Cases.extent_source definition (bytes - 1))));
          failure "HCIRVM0019" ""
            (run mode (Cases.extent_source definition bytes)))
        Cases.extents;
      List.iter
        (fun ((_, _, bytes) as extent) ->
          ignore
            (value 42L "" (run mode (Arrays.extent_source extent (bytes - 1))));
          failure "HCIRVM0019" "" (run mode (Arrays.extent_source extent bytes)))
        Arrays.extents)
    modes

let quotas () =
  let check ?(modes = modes) source_contents =
    List.iter
      (fun mode ->
        let session, config, source = inputs mode source_contents in
        let report = compile_integer_program_report session ~config ~source in
        let compiled =
          match
            (integer_program_compilation_result report |> checked).value
          with
          | Isolated program -> [ program ]
          | Stateful _ -> integer_program_compilation_units report
        in
        let frame_bytes =
          List.concat_map integer_program_functions compiled
          |> List.map (fun (f : VM.function_definition) ->
              Int64.add
                (Semantic_function_frame_layout.function_frame_size f.frame)
                (Int64.of_int
                   (8 * List.length (Ir_function_body.parameters f.body)))
              |> Int64.to_int)
          |> List.fold_left max 0
        in
        let frame_bytes =
          if source_contents = Refs.quota_source then 16 else frame_bytes
        in
        let baseline = value 42L "" (run mode source_contents) in
        let steps = VM.executed_steps baseline in
        let preparation = VM.compiled_initializer_steps baseline in
        ignore
          (value 42L ""
             (run ~max_frame_bytes:frame_bytes ~max_steps:steps
                ~max_initializer_steps:(max 1 preparation) mode source_contents));
        failure "HCIRVM0011" ""
          (run ~max_frame_bytes:(frame_bytes - 1) mode source_contents);
        failure "HCIRVM0007" ""
          (run ~max_steps:(steps - 1) mode source_contents);
        if preparation > 1 then
          failure "HCIRVM0007" ""
            (run ~max_initializer_steps:(preparation - 1) mode source_contents))
      modes
  in
  List.iter (check ~modes)
    [
      Cases.quota_source;
      Refs.quota_source;
      Cases.pointer_quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
      Backed.quota_source;
      Default.quota_source;
      Casts.quota_source;
      Parameters.quota_source;
      Returns.quota_source;
    ];
  check ~modes:[ Preprocessor.Jit ] Cases.retained_nested_limits

let persistent_storage () =
  List.iter
    (fun mode ->
      ignore
        (value 42L "" (run ~max_global_bytes:14 mode Persistent.quota_source));
      failure "HCIRVM0016" ""
        (run ~max_global_bytes:13 mode Persistent.quota_source))
    modes;
  List.iter
    (fun (_, source, code, output) ->
      failure code output (run Preprocessor.Jit source))
    Persistent.jit_faults;
  List.iter
    (fun (_, source, expected, output) ->
      ignore (value expected output (run Preprocessor.Aot source)))
    Persistent.aot_values

let foreign_frame () =
  let check source_contents =
    List.iter
      (fun mode ->
        let compile () =
          let session, config, source = inputs mode source_contents in
          (compile_integer_program session ~config ~source |> checked).value
        in
        let original = compile () and foreign = compile () in
        let functions =
          List.map2
            (fun (a : VM.function_definition) (b : VM.function_definition) ->
              { a with frame = b.frame })
            (integer_program_functions original)
            (integer_program_functions foreign)
        in
        match
          VM.execute_program
            ~runtime_calls:(integer_program_runtime_calls original)
            ~globals:(integer_program_globals original)
            ~initialization:(integer_program_initialization original)
            ~max_steps:100_000 ~max_frame_bytes:1024 ~max_call_depth:16
            ~functions
            (integer_program_entry original)
        with
        | Ok _ -> Alcotest.fail "equal names and sizes admitted a foreign frame"
        | Error errors ->
            List.iter
              (fun (e : VM.error) ->
                Alcotest.(check int)
                  "ownership failure precedes execution" 0 e.executed_steps)
              errors)
      modes
  in
  List.iter check
    [
      Cases.quota_source;
      Refs.quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
      Backed.quota_source;
      Default.quota_source;
      Casts.quota_source;
      Parameters.quota_source;
      Returns.quota_source;
      Persistent.quota_source;
    ]

let field_proofs () =
  let check contents =
    List.iter
      (fun mode ->
        let _, own, invalid =
          Aggregate_member_fixture.controls ~contents mode
        in
        let execute definition =
          VM.execute_function ~max_steps:100_000 ~max_frame_bytes:1024
            ~frame:definition.VM.frame ~arguments:[] definition.body
        in
        (match execute own with
        | Ok value ->
            Alcotest.(check (option int64))
              "rebuilt own proof executes" (Some 42L)
              (match VM.termination value with
              | VM.Returned word -> Option.map (fun w -> w.VM.bits) word
              | _ -> None)
        | Error errors ->
            Alcotest.fail
              (String.concat "; "
                 (List.map
                    (fun (e : VM.error) -> e.code ^ ": " ^ e.message)
                    errors)));
        List.iter
          (fun (name, definition) ->
            match execute definition with
            | Ok _ -> Alcotest.fail (name ^ " executed")
            | Error errors ->
                List.iter
                  (fun (e : VM.error) ->
                    Alcotest.(check int)
                      (name ^ " precedes storage")
                      0 e.executed_steps)
                  errors)
          invalid)
      modes
  in
  List.iter check
    [
      Aggregate_member_fixture.contents;
      Callbacks.proof_source;
      Cases.pointer_proof_source;
      Cases.pointer_local_proof_source;
      Aggregate_member_fixture.local_contents;
      Aggregate_member_fixture.local_pointer_contents;
      Aggregate_member_fixture.local_backing_contents;
      Aggregate_member_fixture.index_contents;
      Aggregate_member_fixture.dynamic_index_contents;
      Automatic_aggregate_cases.initializer_proof_source;
      Arrays.proof_source;
      Pointers.proof_source;
      Inherited.proof_source;
      Backed.proof_source;
      Default.proof_source;
      Casts.proof_source;
    ]

let persistent_ownership () =
  let module Globals = Holyc_lib__Ir.Integer_globals in
  let module Shape = Holyc_lib__Ir.Integer_storage_shape in
  let module Frame = Semantic_function_frame_layout in
  List.iter
    (fun mode ->
      let compile () =
        Aggregate_member_fixture.compile ~contents:Persistent.quota_source mode
      in
      let original, foreign = (compile (), compile ()) in
      let globals = integer_program_globals original in
      let slot = List.hd (Globals.slots globals) in
      let storage = Globals.global_storage slot in
      let shape = Globals.slot_shape slot in
      Alcotest.(check int) "source object count" 2 (Shape.element_count shape);
      Alcotest.(check int) "physical byte cell count" 6 (Shape.cell_count shape);
      Alcotest.(check int) "selected class stride" 3 (Shape.element_size shape);
      Alcotest.(check bool)
        "exact global shape owner" true
        (Shape.matches shape
           ~symbol:(Globals.storage_symbol storage)
           ~type_:(Globals.storage_type storage)
           ~dimensions:[ 2L ]);
      Alcotest.(check bool)
        "foreign equal-looking shape owner" false
        (Shape.matches shape
           ~symbol:
             (Globals.slot_symbol
                (List.hd (Globals.slots (integer_program_globals foreign))))
           ~type_:(Globals.storage_type storage)
           ~dimensions:[ 2L ]);
      Alcotest.(check bool)
        "another source extent cannot reuse the shape" false
        (Shape.matches shape
           ~symbol:(Globals.storage_symbol storage)
           ~type_:(Globals.storage_type storage)
           ~dimensions:[ 3L ]);
      let definition = Aggregate_member_fixture.first_definition original in
      let foreign_definition =
        Aggregate_member_fixture.first_definition foreign
      in
      let location =
        Frame.function_locations definition.frame
        |> List.find (fun location ->
            Frame.location_kind location = Frame.Static_local)
      in
      Alcotest.(check bool)
        "original static location supplies a byte shape" true
        (Result.is_ok (Shape.create_static ~frame:definition.frame ~location));
      Alcotest.(check bool)
        "another frame cannot claim the static location" true
        (Result.is_error
           (Shape.create_static ~frame:foreign_definition.frame ~location));
      match
        VM.execute_program
          ~globals:(integer_program_globals foreign)
          ~initialization:(integer_program_initialization original)
          ~runtime_calls:(integer_program_runtime_calls original)
          ~max_steps:100_000 ~max_frame_bytes:1024 ~max_call_depth:16
          ~functions:(integer_program_functions original)
          (integer_program_entry original)
      with
      | Ok _ ->
          Alcotest.fail "foreign persistent roots acquired source authority"
      | Error errors ->
          List.iter
            (fun (e : VM.error) ->
              Alcotest.(check int)
                "foreign persistent root rejects before work" 0 e.executed_steps)
            errors)
    modes

let class_parameter_ownership () =
  let module Body = Ir_function_body in
  let module Frame = Semantic_function_frame_layout in
  List.iter
    (fun mode ->
      let compile () =
        Aggregate_member_fixture.compile ~contents:Parameters.example mode
      in
      let own = compile () |> Aggregate_member_fixture.first_definition in
      let foreign = compile () |> Aggregate_member_fixture.first_definition in
      let parameter = List.hd (Body.parameters own.body) in
      let location =
        Frame.find_location own.frame (Body.member_symbol parameter)
        |> Option.get
      in
      Alcotest.(check int64)
        "nominal class extent" 3L
        (Frame.location_element_size location);
      Alcotest.(check int64)
        "physical parameter slot" 8L
        (Frame.location_allocated_size location);
      let view =
        Body.parameter_aggregate_value_type own.body parameter |> Option.get
      in
      Alcotest.(check bool)
        "original U16 raw view" true
        (match Semantic_type.base view with
        | Semantic_type.Primitive (_, Primitive_type.U16) ->
            Semantic_type.pointer_depth view = 0
        | _ -> false);
      Alcotest.(check bool)
        "foreign member cannot select a class view" true
        (Option.is_none
           (Body.parameter_aggregate_value_type own.body
              (List.hd (Body.parameters foreign.body))));
      let callback =
        Aggregate_member_fixture.compile
          ~contents:"class Box{I64 word;};I64 F(Box (*p)()){return 42;}" mode
        |> Aggregate_member_fixture.first_definition
      in
      Alcotest.(check bool)
        "callback return metadata cannot supply class parameter ABI" true
        (Option.is_none
           (Body.parameter_aggregate_value_type callback.body
              (List.hd (Body.parameters callback.body))));
      let raw = Aggregate_member_fixture.rebuild own Fun.id in
      Alcotest.(check bool)
        "raw graph lacks class ABI owner" true
        (Option.is_none
           (Body.parameter_aggregate_value_type raw.body
              (List.hd (Body.parameters raw.body))));
      let execute definition =
        VM.execute_function ~max_steps:100_000 ~max_frame_bytes:1024
          ~frame:definition.VM.frame ~arguments:[ 0x070021L; 9L ]
          definition.body
      in
      (match execute own with
      | Ok result ->
          Alcotest.(check (option int64))
            "full incoming word reaches class members" (Some 42L)
            (match VM.termination result with
            | VM.Returned word -> Option.map (fun w -> w.VM.bits) word
            | _ -> None)
      | Error _ -> Alcotest.fail "owned class parameter failed direct execution");
      match execute raw with
      | Ok _ -> Alcotest.fail "raw graph acquired a class parameter root"
      | Error errors ->
          List.iter
            (fun (e : VM.error) ->
              Alcotest.(check int)
                "class ABI rejection precedes execution" 0 e.executed_steps)
            errors)
    modes

let class_return_ownership () =
  let module Body = Ir_function_body in
  List.iter
    (fun mode ->
      let own =
        Aggregate_member_fixture.compile
          ~contents:"U16 class Box{U16 low;};Box F(){return 0x1002a;}F();" mode
        |> Aggregate_member_fixture.first_definition
      in
      let raw = Aggregate_member_fixture.rebuild own Fun.id in
      Alcotest.(check bool)
        "original body retains its nominal class" true
        (match Semantic_type.base (Body.return_type own.body) with
        | Semantic_type.Aggregate _ -> true
        | _ -> false);
      Alcotest.(check bool)
        "bound body selects its original U16 return view" true
        (match Semantic_type.base (Body.return_value_type own.body) with
        | Semantic_type.Primitive (_, Primitive_type.U16) ->
            Semantic_type.pointer_depth (Body.return_value_type own.body) = 0
        | _ -> false);
      Alcotest.(check bool)
        "raw body cannot supply a class return ABI" true
        (Semantic_type.equal
           (Body.return_type raw.body)
           (Body.return_value_type raw.body));
      let execute definition =
        VM.execute_function ~max_steps:100_000 ~max_frame_bytes:1024
          ~frame:definition.VM.frame ~arguments:[] definition.body
      in
      (match execute own with
      | Ok result ->
          Alcotest.(check (option int64))
            "class return keeps the register word above its prefix"
            (Some 0x1002aL)
            (match VM.termination result with
            | VM.Returned word -> Option.map (fun w -> w.VM.bits) word
            | _ -> None)
      | Error _ -> Alcotest.fail "owned class return failed direct execution");
      match execute raw with
      | Ok _ -> Alcotest.fail "raw graph acquired class return authority"
      | Error errors ->
          List.iter
            (fun (e : VM.error) ->
              Alcotest.(check int)
                "return ABI rejection precedes execution" 0 e.executed_steps)
            errors)
    modes

let boundaries () =
  ignore (value 42L "" (run Preprocessor.Jit Inherited.lookahead_source));
  ignore (value 42L "" (run Preprocessor.Aot Inherited.lookahead_source));
  List.iter
    (fun mode ->
      List.iter
        (fun (name, contents) ->
          Alcotest.(check bool)
            name true
            (Result.is_error
               (integer_program_report_outcome (run mode contents))))
        (Refs.unsupported @ Cases.unsupported @ Cases.pointer_unsupported
       @ Arrays.unsupported @ Pointers.unsupported @ Inherited.unsupported
       @ Backed.unsupported @ Default.unsupported @ Casts.unsupported
        @ (if mode = Preprocessor.Aot then
             [ List.hd Inherited.source_unsupported ]
           else [])
        @ Parameters.unsupported @ Returns.unsupported @ Persistent.unsupported
        ))
    modes

let class_default_boundaries () =
  List.iter
    (fun (_, source) ->
      Alcotest.(check bool)
        "source default boundary" true
        (Result.is_error
           (integer_program_report_outcome (run Preprocessor.Aot source))))
    Defaults.unsupported;
  List.iter
    (fun mode -> failure "HCIRVM0019" "" (run mode Defaults.extent_source))
    modes

let () =
  Alcotest.run "aggregate members"
    [
      ( "source class defaults",
        List.map
          (fun (name, source, expected, output) ->
            Alcotest.test_case name `Quick (fun () ->
                List.iter
                  (fun mode -> ignore (value expected output (run mode source)))
                  modes))
          Defaults.all_values );
      ( "retained class context",
        List.map
          (fun (name, source, expected, output) ->
            Alcotest.test_case name `Quick (fun () ->
                ignore (value expected output (run Preprocessor.Jit source))))
          (Defaults.jit_values @ Inherited.retained_values
         @ Pointers.retained_view_values @ Cases.retained_nested_values
         @ Cases.pointer_retained_values @ Persistent.retained_values
         @ Casts.retained_values @ Backed.assignment_values) );
      ( "assignment source domains",
        [
          Alcotest.test_case "AOT task completion retains the outer class"
            `Quick (fun () ->
              failure "HCSEMA0046" ""
                (run Preprocessor.Aot Backed.isolated_assignment_source);
              ignore
                (value 43L ""
                   (run Preprocessor.Aot Backed.isolated_assignment_value_source)));
        ] );
      ( "values",
        List.map
          (fun (name, source, expected, output) ->
            Alcotest.test_case name `Quick (fun () ->
                List.iter
                  (fun mode -> ignore (value expected output (run mode source)))
                  modes))
          (Refs.values @ Refs.width_values @ Callbacks.values
         @ Callbacks.width_values @ Cases.values @ Cases.pointer_values
         @ Cases.pointer_width_matrix @ Cases.nested_values @ Cases.view_matrix
         @ Arrays.values @ Arrays.view_matrix @ Pointers.values
         @ Pointers.view_matrix @ Inherited.values @ Inherited.view_matrix
         @ Backed.values @ Backed.source_values @ Default.values
         @ Parameters.values @ Parameters.view_matrix @ Returns.values
         @ Returns.view_matrix @ Returns.warning_values @ Persistent.values
         @ Persistent.pointer_values @ Casts.values) );
      ( "storage",
        [
          Alcotest.test_case
            "owned returns can be discarded without reading bytes" `Quick
            discarded_reference_returns;
          Alcotest.test_case "source class default boundaries" `Quick
            class_default_boundaries;
          Alcotest.test_case
            "persistent byte roots, initialization and exact storage limits"
            `Quick persistent_storage;
          Alcotest.test_case "unknown bytes, extents and fresh activations"
            `Quick faults;
          Alcotest.test_case "exact frame and instruction limits" `Quick quotas;
          Alcotest.test_case "foreign frame rejected before touching bytes"
            `Quick foreign_frame;
          Alcotest.test_case "persistent shapes require exact source owners"
            `Quick persistent_ownership;
          Alcotest.test_case
            "class parameter slot requires its original ABI owner" `Quick
            class_parameter_ownership;
          Alcotest.test_case "class return requires its original ABI owner"
            `Quick class_return_ownership;
          Alcotest.test_case
            "selected field proof cannot be borrowed or altered" `Quick
            field_proofs;
          Alcotest.test_case "unadmitted layouts, copies and numeric ownership"
            `Quick boundaries;
        ] );
    ]
