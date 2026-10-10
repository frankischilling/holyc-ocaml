open Holyc_lib
module Cases = Aggregate_member_cases
module Arrays = Aggregate_array_cases
module Pointers = Aggregate_pointer_cases
module Inherited = Inherited_aggregate_cases
module Backed = Backed_aggregate_cases
module Default = Default_aggregate_cases
module Parameters = Class_parameter_cases
module Returns = Class_return_cases
module Defaults = Class_default_cases
module Saved_default = Holyc_lib__Ir.Prepared_parameter_default
module Default_preparation = Holyc_lib__Driver.Native_default_preparation
module P = X86_64_program
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-aggregate-members.hc" ~contents
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

let run ?max_frame_bytes ?(max_steps = 100_000) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ?max_frame_bytes session ~config ~source ~max_steps

let value expected output report =
  let native = (Native_program.outcome report |> checked).value in
  Alcotest.(check (option int64))
    "independent native word" (Some expected)
    (Option.map (fun w -> w.P.bits) native.execution.final_value);
  Alcotest.(check string)
    "native output" output
    (Native_program.output_bytes report);
  native

let case contents expected output () =
  List.iter
    (fun mode ->
      let result = value expected output (run mode contents) in
      let _, interpreted =
        Native_scalar_fixture.execute_source ~mode ~contents () |> function
        | Ok value -> value
        | Error e -> Alcotest.fail e
      in
      Alcotest.(check (option int64))
        "same closed IR, independent expected word" (Some expected)
        (Option.map (fun w -> w.VM.bits) (VM.final_value interpreted));
      Alcotest.(check int)
        "same closed IR instruction work"
        (VM.executed_steps interpreted)
        result.execution.executed_steps)
    modes

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, contents, code, output) ->
          let report = run mode contents in
          (match Native_program.outcome report with
          | Ok _ -> Alcotest.fail ("expected " ^ code)
          | Error errors ->
              Alcotest.(check bool)
                (describe errors) true
                (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors));
          Alcotest.(check string)
            "native fault preserves reached output" output
            (Native_program.output_bytes report))
        (Cases.faults @ Arrays.faults @ Pointers.faults @ Inherited.faults
       @ Backed.faults @ Default.faults @ Parameters.faults @ Returns.faults);
      List.iter
        (fun (definition, bytes) ->
          ignore
            (value 42L ""
               (run mode (Cases.extent_source definition (bytes - 1))));
          let report = run mode (Cases.extent_source definition bytes) in
          match Native_program.outcome report with
          | Ok _ ->
              Alcotest.fail
                "aggregate layout admitted bytes beyond its exact size"
          | Error errors ->
              Alcotest.(check bool)
                (describe errors) true
                (List.exists
                   (fun (d : Diagnostic.t) -> d.code = "HCIRVM0019")
                   errors))
        Cases.extents;
      List.iter
        (fun ((_, _, bytes) as extent) ->
          ignore
            (value 42L "" (run mode (Arrays.extent_source extent (bytes - 1))));
          match
            Native_program.outcome
              (run mode (Arrays.extent_source extent bytes))
          with
          | Ok _ ->
              Alcotest.fail "root array admitted bytes past its total extent"
          | Error errors ->
              Alcotest.(check bool)
                (describe errors) true
                (List.exists
                   (fun (d : Diagnostic.t) -> d.code = "HCIRVM0019")
                   errors))
        Arrays.extents)
    modes

let quotas () =
  let check source_contents =
    List.iter
      (fun mode ->
        let baseline = value 42L "" (run mode source_contents) in
        let steps = baseline.execution.executed_steps in
        let fixture =
          Native_scalar_fixture.compile ~mode ~path:"aggregate-quota.hc"
            ~contents:source_contents ()
          |> checked
        in
        let frame =
          Holyc_lib__Driver.Integer_unit.functions fixture.unit_
          |> List.map (fun (f : VM.function_definition) ->
              Int64.add
                (Semantic_function_frame_layout.function_frame_size f.frame)
                (Int64.of_int
                   (8 * List.length (Ir_function_body.parameters f.body)))
              |> Int64.to_int)
          |> List.fold_left max 0
        in
        ignore
          (value 42L ""
             (run ~max_frame_bytes:frame ~max_steps:steps mode source_contents));
        let fault kind report =
          match Native_program.native_outcome report with
          | Some (P.Fault f) ->
              Alcotest.(check bool)
                "one below reaches native guard" true (f.kind = kind)
          | _ -> Alcotest.fail "one below did not fault"
        in
        fault P.Frame_limit_exceeded
          (run ~max_frame_bytes:(frame - 1) mode source_contents);
        fault P.Step_limit_exceeded
          (run ~max_steps:(steps - 1) mode source_contents))
      modes
  in
  List.iter check
    [
      Cases.quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
      Backed.quota_source;
      Default.quota_source;
      Parameters.quota_source;
      Returns.quota_source;
      Defaults.quota_source;
    ]

let images () =
  let check source_contents =
    List.iter
      (fun mode ->
        List.iter
          (fun status_abi ->
            let compile ?max_stack_bytes ?max_code_bytes () =
              let session, config, source = inputs mode source_contents in
              Native_program.compile ?max_stack_bytes ?max_code_bytes
                ~status_abi session ~config ~source
            in
            let image = (compile () |> checked).value in
            let frame = P.frame_bytes image and code = P.code_bytes image in
            ignore
              (compile ~max_stack_bytes:frame ~max_code_bytes:code () |> checked);
            Alcotest.(check bool)
              "byte initialization metadata stack one below" true
              (Result.is_error (compile ~max_stack_bytes:(frame - 1) ()));
            Alcotest.(check bool)
              "encoded image bytes one below" true
              (Result.is_error (compile ~max_code_bytes:(code - 1) ()));
            let host =
              match Native_program_execution.platform () with
              | Native_program_execution.Windows_x86_64 -> P.Windows_x64
              | _ -> P.System_v_x64
            in
            if status_abi = host then
              for _ = 1 to 2 do
                match
                  Native_program_execution.execute ~max_steps:100_000 image
                with
                | Ok (P.Completed result) ->
                    Alcotest.(check int64)
                      "fresh object image" 42L
                      (Option.get result.final_value).bits
                | _ -> Alcotest.fail "native object image failed"
              done)
          [ P.Windows_x64; P.System_v_x64 ])
      modes
  in
  List.iter check
    [
      Cases.quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
      Backed.quota_source;
      Default.quota_source;
      Parameters.quota_source;
      Returns.quota_source;
      Defaults.quota_source;
    ]

let field_proofs () =
  let check contents =
    List.iter
      (fun mode ->
        let original, own, invalid =
          Aggregate_member_fixture.controls ~contents mode
        in
        let compile definition =
          P.compile_callable ~max_ir_instructions:4096 ~max_code_bytes:65_536
            ~runtime_calls:(integer_program_runtime_calls original)
            ~initialization:(integer_program_initialization original)
            ~entry:(integer_program_entry original)
            ~functions:
              (definition :: List.tl (integer_program_functions original))
            ()
        in
        (match compile (Aggregate_member_fixture.first_definition original) with
        | Ok image -> (
            match Native_program_execution.execute ~max_steps:100_000 image with
            | Ok (P.Completed value) ->
                Alcotest.(check (option int64))
                  "original sealed field proof executes" (Some 42L)
                  (Option.map (fun w -> w.P.bits) value.final_value)
            | _ -> Alcotest.fail "original sealed field proof did not execute")
        | Error errors ->
            Alcotest.fail
              (String.concat "; "
                 (List.map
                    (fun (e : P.error) -> e.code ^ ": " ^ e.message)
                    errors)));
        List.iter
          (fun (name, definition) ->
            Alcotest.(check bool)
              (name ^ " rejects before image creation")
              true
              (Result.is_error (compile definition)))
          (("rebuilt graph retains no sealed source authority", own) :: invalid))
      modes
  in
  List.iter check
    [
      Aggregate_member_fixture.contents;
      Arrays.proof_source;
      Pointers.proof_source;
      Inherited.proof_source;
      Backed.proof_source;
      Default.proof_source;
    ]

let boundaries () =
  List.iter
    (fun mode ->
      match Native_program.outcome (run mode Inherited.lookahead_source) with
      | Ok _ -> Alcotest.fail "isolated native #exe remains unsupported"
      | Error errors ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists (fun (d : Diagnostic.t) -> d.code = "HCPP0008") errors))
    modes;
  List.iter
    (fun mode ->
      List.iter
        (fun (name, contents) ->
          Alcotest.(check bool)
            name true
            (Result.is_error (Native_program.outcome (run mode contents))))
        (Cases.unsupported @ Arrays.unsupported @ Pointers.unsupported
       @ Inherited.source_unsupported @ Backed.unsupported @ Default.unsupported
       @ Parameters.unsupported @ Returns.unsupported);
      let original =
        Aggregate_member_fixture.compile ~contents:Returns.quota_source mode
      in
      let raw =
        Aggregate_member_fixture.first_definition original |> fun definition ->
        Aggregate_member_fixture.rebuild definition Fun.id
      in
      List.iter
        (fun status_abi ->
          Alcotest.(check bool)
            "raw class return body rejects before native image creation" true
            (Result.is_error
               (P.compile_callable ~status_abi ~max_ir_instructions:4096
                  ~max_code_bytes:65_536
                  ~runtime_calls:(integer_program_runtime_calls original)
                  ~initialization:(integer_program_initialization original)
                  ~entry:(integer_program_entry original)
                  ~functions:[ raw ] ())))
        [ P.Windows_x64; P.System_v_x64 ])
    modes;
  let session, config, source = inputs Preprocessor.Jit Cases.quota_source in
  let report =
    Native_source_execution.evaluate session ~config ~source ~max_steps:100_000
  in
  let result = Native_source_execution.outcome report |> checked in
  Alcotest.(check (option int64))
    "original retained class owns its native frame" (Some 42L)
    (Option.map
       (fun word -> word.Native_source_execution.bits)
       result.value.final_value)

let class_default_proofs () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, contents) ->
          Alcotest.(check bool)
            "unsupported native default" true
            (Result.is_error (Native_program.outcome (run mode contents))))
        Defaults.native_unsupported;
      List.iter
        (fun (_, contents, _, _) ->
          Alcotest.(check bool)
            "ordinary native prototypes retain their source gate" true
            (Result.is_error (Native_program.outcome (run mode contents))))
        Defaults.prototype_values;
      let fixture =
        Native_scalar_fixture.compile ~mode ~path:"class-default-proof.hc"
          ~contents:
            (let _, source, _, _ = List.nth Defaults.values 5 in
             source)
          ()
        |> checked
      in
      let saved = List.hd fixture.prepared_defaults in
      Alcotest.(check (option int64))
        "saved word retains its guard bytes" (Some 0x0714L)
        (Saved_default.word_bits saved);
      Alcotest.(check bool)
        "nominal class remains selected" true
        (match Semantic_type.base (Saved_default.type_ saved) with
        | Semantic_type.Aggregate _ -> true
        | _ -> false);
      let word =
        Semantic_type.make_primitive ~form:Internal_storage ~primitive:I64
          ~pointer_depth:0
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "saved value has a full signed word class" true
        (Semantic_type.equal word (Saved_default.value_type saved));
      let publication = Saved_default.publication saved
      and header = Saved_default.header saved
      and receipt = Saved_default.receipt saved
      and value = Saved_default.value saved in
      Alcotest.(check bool)
        "a raw saved number cannot invent selected class metadata" true
        (Result.is_error
           (Saved_default.create_value ~publication ~header ~receipt ~value));
      let wrong =
        List.nth fixture.completions 1
        |> Default_preparation.execution |> VM.default_constant_authority
        |> Holyc_lib__Sema.Default_fragment.authorized_fragment
      in
      Alcotest.(check bool)
        "another original default cannot lend its class fragment" true
        (Result.is_error
           (Saved_default.create_value_selected ~fragment:wrong ~publication
              ~header ~receipt ~value ()));
      let quota =
        Native_scalar_fixture.compile ~max_initializer_steps:5 ~mode
          ~path:"class-default-quota.hc" ~contents:Defaults.quota_source ()
        |> checked
      in
      Alcotest.(check int)
        "original expression charges five preparation steps" 5
        quota.preparation_steps;
      Alcotest.(check int)
        "a narrow class default retains eight saved bytes" 8 quota.default_bytes;
      Alcotest.(check bool)
        "preparation cannot exceed its step limit" true
        (Result.is_error
           (Native_scalar_fixture.compile ~max_initializer_steps:4 ~mode
              ~path:"class-default-quota.hc" ~contents:Defaults.quota_source ()));
      Alcotest.(check bool)
        "the saved byte limit cannot clamp a class word" true
        (Result.is_error
           (Native_scalar_fixture.compile ~max_default_bytes:7 ~mode
              ~path:"class-default-quota.hc" ~contents:Defaults.quota_source ())))
    modes

let run_task ?max_code_bytes ?max_ir_instructions ?max_initializer_steps
    ?max_default_bytes ?max_frame_bytes ?max_steps contents =
  let session, config, source = inputs Preprocessor.Jit contents in
  Native_source_execution.evaluate ?max_ir_instructions ?max_initializer_steps
    ?max_default_bytes ?max_frame_bytes
    ~max_code_bytes:(Option.value ~default:524_288 max_code_bytes)
    ~max_steps:(Option.value ~default:100_000 max_steps)
    session ~config ~source

let task_value expected output report =
  let result = Native_source_execution.outcome report |> checked in
  Alcotest.(check (option int64))
    "independent retained native word" (Some expected)
    (Option.map
       (fun word -> word.Native_source_execution.bits)
       result.value.final_value);
  Alcotest.(check string)
    "retained native output" output
    (Native_source_execution.output_bytes report);
  Alcotest.(check int)
    "zero interpreted runtime instructions" 0
    (Option.get (Native_source_execution.source_progress report)).runtime
      .executed_steps;
  List.iter
    (fun (fragment : Native_source_execution.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (P.Completed _)) -> ()
      | _ -> Alcotest.fail "original class fragment did not complete natively")
    (Native_source_execution.fragments report)

let task_case contents expected output () =
  task_value expected output (run_task contents)

let task_failure code report =
  match Native_source_execution.outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error diagnostics ->
      Alcotest.(check bool)
        (describe diagnostics) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) diagnostics)

let task_limits () =
  let baseline = run_task Defaults.quota_source in
  task_value 42L "" baseline;
  let bytes, ir =
    List.fold_left
      (fun (bytes, ir) (fragment : Native_source_execution.fragment) ->
        (bytes + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0)
      (Native_source_execution.fragments baseline)
  in
  let steps = Native_source_execution.executed_steps baseline
  and preparation = Native_source_execution.preparation_steps baseline in
  task_value 42L ""
    (run_task ~max_code_bytes:bytes ~max_ir_instructions:ir ~max_steps:steps
       ~max_initializer_steps:preparation ~max_default_bytes:8
       Defaults.quota_source);
  task_failure "HCBACK0005"
    (run_task ~max_code_bytes:(bytes - 1) Defaults.quota_source);
  task_failure "HCBACK0001"
    (run_task ~max_ir_instructions:(ir - 1) Defaults.quota_source);
  task_failure "HCIRVM0007"
    (run_task ~max_steps:(steps - 1) Defaults.quota_source);
  task_failure "HCIRVM0007"
    (run_task ~max_initializer_steps:(preparation - 1) Defaults.quota_source);
  task_failure "HCIRVM0011"
    (run_task ~max_default_bytes:7 Defaults.quota_source);
  task_failure "HCIRVM0019" (run_task Defaults.extent_source)

let task_values =
  Cases.values @ Cases.view_matrix @ Arrays.values @ Arrays.view_matrix
  @ Pointers.values @ Pointers.view_matrix @ Inherited.values
  @ Inherited.view_matrix @ Inherited.retained_values @ Backed.values
  @ Default.values @ Parameters.values @ Parameters.view_matrix @ Returns.values
  @ Returns.view_matrix @ Returns.warning_values @ Defaults.native_values
  @ [ List.hd Defaults.prototype_values ]
  @ Defaults.jit_values

let task_boundaries () =
  List.iter
    (fun (name, contents, _, _) ->
      if List.mem name Cases.retained_nested_class_boundaries then
        task_failure "HCRUN0001" (run_task contents))
    task_values;
  let _, original_prototype_call, _, _ = List.nth Defaults.prototype_values 1 in
  task_failure "HCBACK0002" (run_task original_prototype_call)

let () =
  Alcotest.run "native aggregate members"
    [
      ( "retained classes",
        List.map
          (fun (name, contents, expected, output) ->
            Alcotest.test_case name `Quick (task_case contents expected output))
          (List.filter
             (fun (name, _, _, _) ->
               not (List.mem name Cases.retained_nested_class_boundaries))
             task_values) );
      ( "retained limits",
        [
          Alcotest.test_case "exact original class code, work and saved words"
            `Quick task_limits;
          Alcotest.test_case
            "nested selections and earlier prototype ABI remain bounded" `Quick
            task_boundaries;
        ] );
      ( "class defaults",
        List.map
          (fun (name, contents, expected, output) ->
            Alcotest.test_case name `Quick (case contents expected output))
          Defaults.native_values );
      ( "values",
        List.map
          (fun (name, contents, expected, output) ->
            Alcotest.test_case name `Quick (case contents expected output))
          (Cases.values @ Cases.view_matrix @ Arrays.values @ Arrays.view_matrix
         @ Pointers.values @ Pointers.view_matrix @ Inherited.values
         @ Inherited.view_matrix @ Backed.values @ Default.values
         @ Parameters.values @ Parameters.view_matrix @ Returns.values
         @ Returns.view_matrix @ Returns.warning_values) );
      ( "storage",
        [
          Alcotest.test_case
            "original class defaults, quotas and source boundaries" `Quick
            class_default_proofs;
          Alcotest.test_case "unknown bytes, bounds and independent activations"
            `Quick faults;
          Alcotest.test_case "exact runtime frame and instruction limits" `Quick
            quotas;
          Alcotest.test_case "both ABIs, exact image limits and fresh execution"
            `Quick images;
          Alcotest.test_case "sealed graphs retain their exact field proofs"
            `Quick field_proofs;
          Alcotest.test_case "metadata and numeric bits do not grant storage"
            `Quick boundaries;
        ] );
    ]
