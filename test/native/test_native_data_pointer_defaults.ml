open Holyc_lib
module Cases = Data_pointer_default_cases
module Native = Native_source_execution
module VM = Ir_integer_interpreter

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (describe errors)

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-data-defaults.hc" ~contents:text
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:Jit () with
    | Ok value -> value
    | Error error -> Alcotest.fail error
  in
  (session, source, config)

let run ?(max_steps = 100_000) ?max_initializer_steps ?max_default_bytes
    ?max_literal_bytes ?max_code_bytes ?max_ir_instructions ?max_frame_bytes
    ?max_call_depth text =
  let session, source, config = inputs text in
  Native.evaluate ?max_initializer_steps ?max_default_bytes ?max_literal_bytes
    ?max_code_bytes ?max_ir_instructions ?max_frame_bytes ?max_call_depth
    session ~source ~config ~max_steps

let value expected report =
  let result = (Native.outcome report |> checked).value in
  Alcotest.(check (option int64))
    "native result" (Some expected)
    (Option.map (fun word -> word.Native.bits) result.final_value);
  Alcotest.(check int)
    "no interpreted instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (X86_64_program.Completed _)) -> ()
      | _ -> Alcotest.fail "fragment did not execute natively")
    (Native.fragments report)

let diagnostic code report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)
  | Ok _ -> Alcotest.fail ("expected " ^ code)

let cases fixtures () =
  List.iter
    (fun (text, expected) ->
      let native = run ~max_code_bytes:262_144 text in
      value expected native;
      let session, source, config = inputs text in
      let interpreted =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      let result =
        (integer_program_report_outcome interpreted |> checked).value
      in
      Alcotest.(check (option int64))
        "independent interpreter result" (Some expected)
        (Option.map (fun word -> word.VM.bits) (VM.final_value result));
      Alcotest.(check string)
        "independent interpreter output"
        (integer_program_report_output_bytes interpreted)
        (Native.output_bytes native))
    fixtures

let faults () =
  List.iter (fun (source, code) -> diagnostic code (run source)) Cases.faults

let quotas () =
  let text = "I64 F(U8 *p=\"(\"){return ++*p;}F();F();" in
  let baseline = run text in
  value 42L baseline;
  let code, ir =
    List.fold_left
      (fun (code, ir) (f : Native.fragment) ->
        (code + f.image.code_bytes, ir + f.image.ir_instructions))
      (0, 0)
      (Native.fragments baseline)
  in
  value 42L
    (run
       ~max_steps:(Native.executed_steps baseline)
       ~max_initializer_steps:(Native.preparation_steps baseline)
       ~max_default_bytes:8 ~max_literal_bytes:4 ~max_code_bytes:code
       ~max_ir_instructions:ir text);
  diagnostic "HCIRVM0011" (run ~max_default_bytes:7 text);
  diagnostic "HCIRVM0011" (run ~max_literal_bytes:3 text);
  diagnostic "HCIRVM0007"
    (run ~max_initializer_steps:(Native.preparation_steps baseline - 1) text);
  diagnostic "HCIRVM0007"
    (run ~max_steps:(Native.executed_steps baseline - 1) text);
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) text);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text)

let capture_authority () =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Dispatch = Task.Native_dispatch in
  let module Request = Task.Native_default in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Runtime = Holyc_lib__Runtime.Native_program_execution in
  let module Image = X86_64_program in
  let module Saved = Holyc_lib__Ir.Saved_parameter_value in
  let module Storage = Holyc_lib__Backend.X86_64_global_storage in
  let unwrap = function
    | Ok x -> x
    | Error error -> Alcotest.fail error
  in
  let compiled = function
    | Ok x -> x
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (e : Image.error) -> e.message) errors))
  in
  let rejects label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  let layout () =
    Image.create_task_layout_with_literals ~max_global_bytes:8
      ~max_literal_bytes:64
    |> compiled
  in
  let original_layout = layout () in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:256 original_layout |> unwrap
  in
  let foreign =
    Runtime.create_task_arena ~max_arena_bytes:256 (layout ()) |> unwrap
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let execute image =
    let retained = Runtime.retain_task_fragment arena image |> unwrap in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> unwrap)
      (fun () -> Runtime.execute_retained_budget_report budget retained)
  in
  let complete report =
    match Runtime.outcome report |> unwrap with
    | Image.Completed result -> result
    | Image.Fault _ -> Alcotest.fail "unexpected native fault"
  in
  let native_default request =
    rejects "another domain cannot claim the original header"
      (Domain.join (Domain.spawn (fun () -> Request.claim request)));
    let host, other =
      match Runtime.platform () with
      | Runtime.Windows_x86_64 -> (Image.Windows_x64, Image.System_v_x64)
      | _ -> (Image.System_v_x64, Image.Windows_x64)
    in
    ignore
      (Image.compile_task_default ~status_abi:other ~layout:original_layout
         request
      |> compiled);
    let image =
      Image.compile_task_default ~status_abi:host ~layout:original_layout
        request
      |> compiled
    in
    let original = Option.get (Image.data_default image) in
    let data = Option.get (Saved.data_source original) in
    let offset =
      Storage.find_saved_data (Option.get (Image.task_snapshot image)) data
      |> Option.get
    in
    rejects "metadata cannot authorize an unevaluated pointer"
      (fst
         (Runtime.finish_task_data_default arena image original
            ~max_copy_steps:64));
    for site = 1 to Image.ir_instructions image do
      rejects "each status site rejects another task descriptor offset"
        (Image.decode_runtime_status image ~max_steps:100_000 ~kind:0L ~site:0L
           ~executed_steps:1L
           ~value_site:(Int64.of_int (-200_000 - site))
           ~bits:(Int64.of_int (offset + 1)))
    done;
    let before = (Runtime.budget_progress budget).executed_steps in
    let result = execute image |> complete in
    let captured = Option.get result.captured_data in
    Alcotest.(check (option int64))
      "capture exposes no host pointer bits" None (Saved.word_bits captured);
    rejects "foreign arena cannot acquire a native capture"
      (fst
         (Runtime.finish_task_data_default foreign image captured
            ~max_copy_steps:64));
    let forged =
      Saved.data
        ~source:(Saved.data_expression data)
        ~type_:(Saved.data_type data)
      |> unwrap
    in
    rejects "equal source and type cannot replace the actual capture"
      (fst
         (Runtime.finish_task_data_default arena image forged ~max_copy_steps:64));
    Gc.full_major ();
    let saved, copy_steps =
      Runtime.finish_task_data_default arena image captured ~max_copy_steps:64
    in
    let saved = saved |> unwrap in
    rejects "native capture cannot complete twice"
      (fst
         (Runtime.finish_task_data_default arena image captured
            ~max_copy_steps:64));
    Request.record_steps request
      ((Runtime.budget_progress budget).executed_steps - before + copy_steps)
    |> unwrap;
    Ok saved
  in
  let dispatch : Dispatch.t =
    {
      execute_initializer = (fun _ -> Alcotest.fail "unexpected initializer");
      execute_command =
        (fun request ->
          let image =
            Image.compile_task_command ~layout:original_layout request
            |> compiled
          in
          let result = execute image |> complete in
          Ok
            (Dispatch.Captured
               (Option.map
                  (fun (word : Image.word) ->
                    match word.type_ with
                    | Image.I64 -> Dispatch.I64 word.bits
                    | U64 -> Dispatch.U64 word.bits)
                  result.final_value)));
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena foreign |> unwrap;
      Runtime.release_task_arena arena |> unwrap)
    (fun () ->
      let session, source, config =
        inputs "I64 F(U8 *p=\"*\"){return p[0];}F();"
      in
      let report =
        Source.run ~native_dispatch:dispatch ~native_default session ~source
          ~config ~max_steps:100_000
      in
      Source.outcome report |> checked |> ignore;
      Alcotest.(check bool)
        "authentic capture remains executable after rejected copies" true
        (Source.native_final_value report = Some (Dispatch.I64 42L)))

let () =
  Alcotest.run "Native data pointer defaults"
    [
      ( "defaults",
        [
          Alcotest.test_case "all original/view scalar read classes" `Quick
            (cases Cases.read_matrix);
          Alcotest.test_case "all original/view scalar write classes" `Quick
            (cases Cases.write_matrix);
          Alcotest.test_case "original objects, headers and activations" `Quick
            (cases Cases.ownership);
          Alcotest.test_case "copied strings, views and providers" `Quick
            (cases Cases.strings);
          Alcotest.test_case "reached bounds and unknown bytes" `Quick faults;
          Alcotest.test_case "exact and one-below cumulative quotas" `Quick
            quotas;
          Alcotest.test_case "actual native capture, foreign arenas and replay"
            `Quick capture_authority;
        ] );
    ]
