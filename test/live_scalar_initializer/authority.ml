open Holyc_lib
module Dispatch = Holyc_lib__Driver.Integer_task.Native_dispatch
module Source = Holyc_lib__Driver.Integer_source_execution
module Program = Holyc_lib__Ir.Initializer_fragment_program
module Capture = Holyc_lib__Ir.Native_scalar_capture
module Receipt = Holyc_lib__Ir.Native_initializer_copy_receipt
module Image = X86_64_program
module Runtime = Native_program_execution

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject name result =
  Alcotest.(check bool) name true (Result.is_error result)

let compiled = function
  | Ok image -> image
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (e : Image.error) -> e.code ^ ": " ^ e.message)
              errors))

let source_copy ?(finish = true) () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-scalar-copy-authority.hc"
      ~contents:
        {|extern class Item;class Box{Item item;$$=8;};
I64 Complete(){return 0x12a;}
Box object={Complete()#exe{class Item{U16 value;};}};
I64 F(){return object.item.value-256;}F();|}
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let layout = Image.create_task_layout ~max_global_bytes:128 |> compiled in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:256 layout |> checked
  in
  let foreign_layout =
    Image.create_task_layout ~max_global_bytes:128 |> compiled
  in
  let foreign_arena =
    Runtime.create_task_arena ~max_arena_bytes:256 foreign_layout |> checked
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved = ref None in
  let execute image =
    let retained = Runtime.retain_task_fragment arena image |> checked in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> checked)
      (fun () ->
        let report = Runtime.execute_retained_budget_report budget retained in
        let result =
          match Runtime.outcome report |> checked with
          | Image.Completed value -> value
          | Image.Fault _ -> Alcotest.fail "unexpected native scalar copy fault"
        in
        (result, Runtime.value_captured report))
  in
  let dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun request ->
          let program = Dispatch.initializer_program request in
          List.iter
            (fun status_abi ->
              ignore
                (Image.compile_task_initializer ~status_abi ~layout request
                |> compiled))
            [ Image.Windows_x64; Image.System_v_x64 ];
          let image =
            Image.compile_task_initializer ~layout request |> compiled
          in
          let before = (Runtime.budget_progress budget).executed_steps in
          ignore (execute image);
          let work = (Runtime.budget_progress budget).executed_steps - before in
          if Program.is_scalar_evaluation program then (
            reject "foreign image cannot remove the original scalar capture"
              (Runtime.finish_task_initializer arena
                 (Obj.obj (Obj.dup (Obj.repr image))));
            reject "another domain cannot remove the original scalar capture"
              (Domain.join
                 (Domain.spawn (fun () ->
                      Runtime.finish_task_initializer arena image)));
            let capture =
              Runtime.finish_task_initializer arena image |> checked
            in
            reject "scalar capture can be removed only once"
              (Runtime.finish_task_initializer arena image);
            reject "foreign program cannot supply a scalar word"
              (Capture.consume capture
                 ~program:(Obj.obj (Obj.dup (Obj.repr program)))
                 ~work);
            reject "scalar copy cannot substitute the reached native work"
              (Dispatch.prepare_initializer_copy request capture ~work:(work + 1));
            let token =
              Dispatch.prepare_initializer_copy request capture ~work |> checked
            in
            reject "original scalar result cannot prepare twice"
              (Dispatch.prepare_initializer_copy request capture ~work);
            reject "another arena cannot copy the original scalar word"
              (Runtime.copy_task_initializer foreign_arena request token);
            reject "another domain cannot copy the original scalar word"
              (Domain.join
                 (Domain.spawn (fun () ->
                      Runtime.copy_task_initializer arena request token)));
            Dispatch.check_initializer_copy request token |> checked;
            if finish then (
              let receipt =
                Runtime.copy_task_initializer arena request token |> checked
              in
              reject "copy entry cannot repeat before completion"
                (Runtime.copy_task_initializer arena request token);
              Gc.full_major ();
              Dispatch.finish_initializer_copy request token receipt |> checked;
              reject "successful native copy cannot complete twice"
                (Dispatch.finish_initializer_copy request token receipt);
              reject "successful native copy cannot write again"
                (Runtime.copy_task_initializer arena request token));
            saved := Some (request, token));
          Ok ());
      execute_command =
        (fun request ->
          let image = Image.compile_task_command ~layout request |> compiled in
          let result, captured = execute image in
          Ok
            (if captured then
               Dispatch.Captured
                 (Option.map
                    (fun (word : Image.word) ->
                      match word.type_ with
                      | Image.I64 -> Dispatch.I64 word.bits
                      | U64 -> Dispatch.U64 word.bits)
                    result.final_value)
             else Dispatch.Unchanged));
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena arena |> checked;
      Runtime.release_task_arena foreign_arena |> checked)
    (fun () ->
      let report =
        Source.run ~native_dispatch:dispatch session ~config ~source
          ~max_steps:100_000
      in
      let errors =
        match Source.outcome report with
        | Ok _ when finish ->
            Alcotest.(check bool)
              "actual copied bytes reach the later command" true
              (Source.native_final_value report = Some (Dispatch.I64 42L));
            []
        | Ok _ -> Alcotest.fail "missing native copy was accepted"
        | Error errors when not finish -> errors
        | Error errors ->
            Alcotest.fail
              (String.concat "; "
                 (List.map
                    (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                    errors))
      in
      if not finish then
        Alcotest.(check bool)
          "unfinished copy rejects before publication" true
          (List.exists (fun (d : Diagnostic.t) -> d.code = "HCRUN0006") errors);
      let request, token = Option.get !saved in
      reject "expired parser request cannot copy into any arena"
        (Runtime.copy_task_initializer arena request token))

let static_source_copy ?(finish = true) () =
  let module Request = Holyc_lib__Driver.Integer_task.Native_static_initializer
  in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"static-scalar-copy-authority.hc"
      ~contents:
        {|extern class Item;class Box{Item item;$$=8;};
I64 F(){static Box object={0x12a#exe{class Item{U16 value;};}};return object.item.value-256;}F();|}
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let layout = Image.create_task_layout ~max_global_bytes:128 |> compiled in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:256 layout |> checked
  in
  let foreign_layout =
    Image.create_task_layout ~max_global_bytes:128 |> compiled
  in
  let foreign =
    Runtime.create_task_arena ~max_arena_bytes:256 foreign_layout |> checked
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let saved = ref None in
  let execute image =
    let retained = Runtime.retain_task_fragment arena image |> checked in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> checked)
      (fun () ->
        let report = Runtime.execute_retained_budget_report budget retained in
        let result =
          match Runtime.outcome report |> checked with
          | Image.Completed result -> result
          | Image.Fault _ -> Alcotest.fail "unexpected static capture fault"
        in
        (result, Runtime.value_captured report))
  in
  let dispatch : Dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "unexpected global initializer");
      execute_command =
        (fun request ->
          let result, captured =
            execute (Image.compile_task_command ~layout request |> compiled)
          in
          Ok
            (if captured then
               Dispatch.Captured
                 (Option.map
                    (fun (word : Image.word) ->
                      match word.type_ with
                      | Image.I64 -> Dispatch.I64 word.bits
                      | U64 -> Dispatch.U64 word.bits)
                    result.final_value)
             else Dispatch.Unchanged));
    }
  in
  let initialize request =
    let program = Request.program request in
    List.iter
      (fun status_abi ->
        ignore
          (Image.compile_task_static_initializer ~status_abi ~layout request
          |> compiled))
      [ Image.Windows_x64; Image.System_v_x64 ];
    let image =
      Image.compile_task_static_initializer ~layout request |> compiled
    in
    let before = (Runtime.budget_progress budget).executed_steps in
    ignore (execute image);
    let work = (Runtime.budget_progress budget).executed_steps - before in
    Request.record_steps request work |> checked;
    reject "foreign static image cannot remove an actual capture"
      (Runtime.finish_task_static_initializer arena
         (Obj.obj (Obj.dup (Obj.repr image))));
    reject "foreign domain cannot remove the static capture"
      (Domain.join
         (Domain.spawn (fun () ->
              Runtime.finish_task_static_initializer arena image)));
    let capture =
      Runtime.finish_task_static_initializer arena image |> checked
    in
    reject "static capture can be removed only once"
      (Runtime.finish_task_static_initializer arena image);
    reject "another static program cannot supply the original word"
      (Capture.consume capture
         ~program:(Obj.obj (Obj.dup (Obj.repr program)))
         ~work);
    reject "static native work cannot be substituted"
      (Request.prepare_copy request capture ~work:(work + 1));
    let token = Request.prepare_copy request capture ~work |> checked in
    reject "static scalar copy cannot prepare twice"
      (Request.prepare_copy request capture ~work);
    reject "foreign arena cannot receive the original static copy"
      (Runtime.copy_task_static_initializer foreign request token);
    reject "foreign domain cannot write the original static copy"
      (Domain.join
         (Domain.spawn (fun () ->
              Runtime.copy_task_static_initializer arena request token)));
    Request.check_copy request token |> checked;
    if finish then (
      let receipt =
        Runtime.copy_task_static_initializer arena request token |> checked
      in
      reject "static copy entry cannot repeat before completion"
        (Runtime.copy_task_static_initializer arena request token);
      Gc.full_major ();
      Request.finish_copy request token receipt |> checked;
      reject "static copy completion cannot repeat"
        (Request.finish_copy request token receipt);
      reject "completed static copy cannot write again"
        (Runtime.copy_task_static_initializer arena request token));
    saved := Some (request, token);
    Ok ()
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena arena |> checked;
      Runtime.release_task_arena foreign |> checked)
    (fun () ->
      let report =
        Source.run ~native_dispatch:dispatch
          ~native_static_allocation:(fun request ->
            Runtime.allocate_task_static arena request |> checked;
            Ok ())
          ~native_static_initializer:initialize session ~config ~source
          ~max_steps:100_000
      in
      (match Source.outcome report with
      | Ok _ when finish ->
          Alcotest.(check bool)
            "published static retains actual copied bytes" true
            (Source.native_final_value report = Some (Dispatch.I64 42L))
      | Error errors when not finish ->
          Alcotest.(check bool)
            "missing static copy cannot publish its frame" true
            (List.exists
               (fun (d : Diagnostic.t) -> d.code = "HCRUN0006")
               errors)
      | Error errors ->
          Alcotest.fail
            (String.concat "; "
               (List.map
                  (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                  errors))
      | Ok _ -> Alcotest.fail "missing static copy was accepted");
      let request, token = Option.get !saved in
      reject "expired static parser receipt cannot write another byte"
        (Runtime.copy_task_static_initializer arena request token))

type raw_arena

external create_arena : int -> raw_arena = "holyc_native_create_task_arena"

external admit_arena :
  raw_arena ->
  int ->
  int ->
  (int * string) list ->
  Holyc_lib__Ir.Native_source_suspension.t option ->
  int = "holyc_native_task_arena_admit"

external release_arena : raw_arena -> unit = "holyc_native_release_task_arena"

external raw_copy :
  raw_arena ->
  int * int * int * string ->
  Holyc_lib__Ir.Native_source_suspension.t option ->
  'a ->
  'a Receipt.t = "holyc_native_task_initializer_copy"

let copy_receipts () =
  let arena = create_arena 40 in
  let token = ref 42 in
  Fun.protect
    ~finally:(fun () -> release_arena arena)
    (fun () ->
      ignore (admit_arena arena 0 40 [] None);
      let receipt = raw_copy arena (40, 0, 32, "AB") None token in
      reject "equal-looking token cannot consume another actual copy"
        (Receipt.consume receipt ~copy:(ref 42) ~bytes:2);
      reject "actual copied byte count cannot be replaced"
        (Receipt.consume receipt ~copy:token ~bytes:1);
      Gc.full_major ();
      Receipt.consume receipt ~copy:token ~bytes:2 |> checked;
      reject "actual copy receipt is consumed once"
        (Receipt.consume receipt ~copy:token ~bytes:2);
      let empty = raw_copy arena (40, 0, 32, "") None token in
      Receipt.consume empty ~copy:token ~bytes:0 |> checked;
      let expired = raw_copy arena (40, 2, 30, "CD") None token in
      release_arena arena;
      reject "receipt retains the original arena lifetime"
        (Receipt.consume expired ~copy:token ~bytes:2));
  Domain.join
    (Domain.spawn (fun () ->
         for _ = 1 to 100 do
           let arena = create_arena 40 in
           ignore (admit_arena arena 0 40 [] None);
           ignore (raw_copy arena (40, 0, 32, "AB") None (ref 42));
           release_arena arena
         done;
         Gc.full_major ()));
  Gc.full_major ();
  ignore (Runtime.platform ())
