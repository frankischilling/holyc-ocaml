open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution

type raw_retained

external raw_retain : Obj.t -> raw_retained = "holyc_native_retain_program"
external raw_release : raw_retained -> unit = "holyc_native_release_program"

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let compile ?status_abi mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-retained.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  match Native_program.compile ?status_abi session ~config ~source with
  | Ok result -> result.value
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
              errors))

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let expect_value expected report =
  match Runtime.outcome report |> checked with
  | Program.Completed execution ->
      let word = Option.get execution.final_value in
      Alcotest.(check int64) "actual retained native value" expected word.bits;
      execution.executed_steps
  | Program.Fault fault ->
      Alcotest.failf "unexpected native fault at instruction %d"
        fault.instruction_id

let expect_fault kind report =
  match Runtime.outcome report |> checked with
  | Program.Fault fault ->
      Alcotest.(check bool)
        "actual retained native fault" true (fault.kind = kind);
      Alcotest.(check bool) "reached native work" true (fault.executed_steps > 0);
      fault.executed_steps
  | Program.Completed _ -> Alcotest.fail "expected a reached native fault"

let with_retained image action =
  let retained = Runtime.retain image |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () -> action retained)

let run ?(max_steps = 10000) retained =
  Runtime.execute_retained_report ~max_steps retained

let persistent_original_storage () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let image = compile mode source in
          with_retained image (fun retained ->
              for index = 1 to 3 do
                ignore (run retained |> expect_value (Int64.of_int (40 + index)));
                Gc.compact ()
              done);
          ignore
            (Runtime.execute_report ~max_steps:10000 image |> expect_value 41L))
        [
          "I64 G=40;++G;";
          "I64 F(){static I64 n=40;return ++n;}F();";
          "U8 a[2]={40,9};++a[0];a[0]+a[1]-9;";
          "I64 F(){U8 *p=\"(\";return ++p[0];}F();";
          "I64 N=40;I64 (*P)()=0;I64 Add(){return ++N;}if(N==40)P=&Add;P();";
        ])
    modes

let reached_faults_keep_writes () =
  List.iter
    (fun mode ->
      let image =
        compile mode
          "extern U0 Print(U8 *fmt,...);I64 G=0;I64 \
           F(){++G;if(G==1){Print(\"A\");return 1/(G-1);}return G+40;}F();"
      in
      with_retained image (fun retained ->
          let fault = run retained in
          ignore (fault |> expect_fault Program.Division_by_zero);
          Alcotest.(check string)
            "reached fault keeps its output" "A"
            (Runtime.output_bytes fault);
          let next = run retained in
          ignore (next |> expect_value 42L);
          Alcotest.(check string)
            "next activation has fresh output" ""
            (Runtime.output_bytes next);
          ignore (run retained |> expect_value 43L));
      let image = compile mode "I64 G=40;G++;G;" in
      let steps =
        Runtime.execute_report ~max_steps:10000 image |> expect_value 41L
      in
      with_retained image (fun retained ->
          ignore
            (run ~max_steps:(steps - 1) retained
            |> expect_fault Program.Step_limit_exceeded);
          Alcotest.(check int)
            "exact quota recovers with retained mutation" steps
            (run ~max_steps:steps retained |> expect_value 42L);
          ignore (run ~max_steps:steps retained |> expect_value 43L)))
    modes

let fresh_capture_and_original_owners () =
  List.iter
    (fun mode ->
      let image =
        compile mode
          "extern U0 Print(U8 *fmt,...);I64 N=40;I64 (*P)()=0;I64 \
           Add(){Print(\"A\");return ++N;}if(N==40)P=&Add;P();"
      in
      with_retained image (fun retained ->
          let first = run retained in
          let steps = first |> expect_value 41L in
          let work = Runtime.output_work first in
          Alcotest.(check string)
            "first native capture" "A"
            (Runtime.output_bytes first);
          Gc.full_major ();
          Gc.compact ();
          for index = 2 to 4 do
            let next = run ~max_steps:steps retained in
            ignore (next |> expect_value (Int64.of_int (40 + index)));
            Alcotest.(check string)
              "fresh per-activation capture" "A"
              (Runtime.output_bytes next);
            Alcotest.(check int)
              "fresh output work budget" work (Runtime.output_work next)
          done))
    modes

let original_aot_entry_load_regions () =
  let image =
    compile Preprocessor.Aot
      "extern U0 Print(U8 *fmt,...);I64 G=40;I64 Seed(){Print(\"A\");return \
       ++G;}I64 N=Seed();N;"
  in
  with_retained image (fun retained ->
      for index = 1 to 3 do
        let report = run retained in
        ignore (report |> expect_value (Int64.of_int (40 + index)));
        Alcotest.(check string)
          "original entry reaches its load code again" "A"
          (Runtime.output_bytes report)
      done);
  let image =
    compile Preprocessor.Aot
      "extern U0 Print(U8 *fmt,...);I64 G=0;I64 \
       Seed(){++G;Print(\"A\");if(G==1) return 1/(G-1);return 40+G;}I64 \
       N=Seed();N;"
  in
  with_retained image (fun retained ->
      let fault = run retained in
      ignore (fault |> expect_fault Program.Division_by_zero);
      Alcotest.(check string)
        "load fault retains reached output" "A"
        (Runtime.output_bytes fault);
      ignore (run retained |> expect_value 42L))

let separate_arenas_and_expiry () =
  List.iter
    (fun mode ->
      let image = compile mode "I64 G=40;++G;" in
      with_retained image (fun first ->
          with_retained image (fun second ->
              ignore (run first |> expect_value 41L);
              ignore (run first |> expect_value 42L);
              ignore (run second |> expect_value 41L);
              Runtime.release first |> checked;
              Runtime.release first |> checked;
              (match Runtime.outcome (run first) with
              | Error message ->
                  Alcotest.(check string)
                    "released image rejects before entry"
                    "retained native image has been released" message
              | Ok _ -> Alcotest.fail "released native code executed");
              ignore (run second |> expect_value 42L))))
    modes

let original_image_bounds () =
  List.iter
    (fun mode ->
      let image = compile mode "I64 G=40;++G;" in
      let bytes = Program.global_bytes image in
      let stack = Program.entry_stack_bytes image in
      List.iter
        (fun result ->
          match result with
          | Error _ -> ()
          | Ok retained ->
              Runtime.release retained |> checked;
              Alcotest.fail "retention exceeded its original image bound")
        [
          Runtime.retain ~max_global_bytes:(bytes - 1) image;
          Runtime.retain ~max_active_stack_bytes:(stack - 1) image;
          Runtime.retain ~max_literal_bytes:0 image;
        ];
      let retained =
        Runtime.retain ~max_global_bytes:bytes ~max_active_stack_bytes:stack
          image
        |> checked
      in
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked)
        (fun () ->
          let bad =
            Runtime.execute_retained_report ~max_global_bytes:(bytes - 1)
              ~max_steps:1000 retained
          in
          (match Runtime.outcome bad with
          | Error _ -> ()
          | Ok _ -> Alcotest.fail "retained activation escaped its global quota");
          ignore (run retained |> expect_value 41L));
      let foreign =
        match Runtime.platform () with
        | Runtime.Windows_x86_64 -> Program.System_v_x64
        | Runtime.Linux_x86_64 -> Program.Windows_x64
        | Runtime.Unsupported -> Alcotest.fail "unsupported native host"
      in
      match Runtime.retain (compile ~status_abi:foreign mode "42;") with
      | Error _ -> ()
      | Ok retained ->
          Runtime.release retained |> checked;
          Alcotest.fail "foreign ABI image acquired an executable mapping")
    modes

let recursive_activation_quota_recovery () =
  List.iter
    (fun mode ->
      let image =
        compile mode
          "I64 G=0;I64 R(I64 n){if(n)return R(n-1);return 42;}++G;R(2)+G-1;"
      in
      List.iter
        (fun (kind, fail) ->
          with_retained image (fun retained ->
              ignore (fail retained |> expect_fault kind);
              ignore (run retained |> expect_value 43L);
              ignore (run retained |> expect_value 44L)))
        [
          ( Program.Call_depth_exceeded,
            fun retained ->
              Runtime.execute_retained_report ~max_call_depth:1 ~max_steps:1000
                retained );
          ( Program.Frame_limit_exceeded,
            fun retained ->
              Runtime.execute_retained_report ~max_frame_bytes:1 ~max_steps:1000
                retained );
          ( Program.Native_stack_limit_exceeded,
            fun retained ->
              Runtime.execute_retained_report
                ~max_active_stack_bytes:(Program.entry_stack_bytes image)
                ~max_steps:1000 retained );
        ])
    modes

let shared_owner_across_domains () =
  let image =
    compile Preprocessor.Jit
      "I64 G=40;I64 F(){I64 i=0;while(i<10000)i++;return ++G;}F();"
  in
  let steps =
    Runtime.execute_report ~max_steps:1_000_000 image |> expect_value 41L
  in
  with_retained image (fun retained ->
      let activate () =
        List.init 20 (fun _ ->
            match Runtime.outcome (run ~max_steps:steps retained) with
            | Error message ->
                Alcotest.(check string)
                  "overlapping entry rejects before mutation"
                  "retained native image is already active" message;
                None
            | Ok (Program.Completed execution) ->
                Some (Option.get execution.final_value).bits
            | Ok (Program.Fault _) ->
                Alcotest.fail "shared retained native activation faulted")
        |> List.filter_map Fun.id
      in
      let first = Domain.spawn activate and second = Domain.spawn activate in
      Gc.full_major ();
      let join domain =
        try Ok (Domain.join domain) with error -> Error error
      in
      let first_words = join first and second_words = join second in
      let words =
        (match (first_words, second_words) with
          | Ok first, Ok second -> first @ second
          | Error error, _ | _, Error error -> raise error)
        |> List.sort Int64.compare
      in
      Alcotest.(check bool)
        "at least one real native activation completes" true (words <> []);
      List.iteri
        (fun index word ->
          Alcotest.(check int64)
            "accepted activations have distinct native writes"
            (Int64.of_int (41 + index))
            word)
        words)

let zero_data_and_collection () =
  List.iter
    (fun mode ->
      let image = compile mode "I64 F(I64 n){return n+2;}F(40);" in
      Alcotest.(check int)
        "no persistent data" 0
        (String.length (Program.global_image image));
      let weak = Weak.create 24 in
      for index = 0 to 23 do
        let retained = Runtime.retain image |> checked in
        ignore (run retained |> expect_value 42L);
        Weak.set weak index (Some retained);
        if index mod 2 = 0 then Runtime.release retained |> checked
      done;
      Gc.full_major ();
      Gc.compact ();
      for index = 0 to 23 do
        Alcotest.(check bool)
          "unreachable native owner collected" false (Weak.check weak index)
      done;
      with_retained image (fun retained ->
          ignore (run retained |> expect_value 42L)))
    modes

let closed_entry_images () =
  let pressure count =
    let expression = ref (string_of_int count) in
    for value = count - 1 downto 1 do
      expression := Printf.sprintf "%d+(%s)" value !expression
    done;
    !expression ^ ";"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected, spilled, large) ->
          let image = compile mode source in
          Alcotest.(check int)
            "closed entry has no named functions" 0
            (Program.function_count image);
          Alcotest.(check int)
            "closed entry has no arena" 0
            (String.length (Program.global_image image));
          Alcotest.(check bool)
            "original spill frame" spilled
            (Program.frame_bytes image > 0);
          Alcotest.(check bool)
            "original small or large unwind allocation" large
            (Program.frame_bytes image > 128);
          let stack = Program.entry_stack_bytes image in
          Alcotest.(check int)
            "closed entry charges its CALL and spill frame"
            (8 + Program.frame_bytes image)
            stack;
          let steps =
            Runtime.execute_report ~max_steps:10000 image
            |> expect_value expected
          in
          let retained =
            Runtime.retain ~max_active_stack_bytes:stack image |> checked
          in
          Fun.protect
            ~finally:(fun () -> Runtime.release retained |> checked)
            (fun () ->
              for _ = 1 to 3 do
                Gc.compact ();
                let report =
                  Runtime.execute_retained_report ~max_active_stack_bytes:stack
                    ~max_steps:steps retained
                in
                Alcotest.(check int)
                  "same actual closed-entry work" steps
                  (report |> expect_value expected);
                Alcotest.(check string)
                  "closed entry captures no output" ""
                  (Runtime.output_bytes report);
                Alcotest.(check int)
                  "closed entry has no output work" 0
                  (Runtime.output_work report)
              done;
              ignore
                (run ~max_steps:(steps - 1) retained
                |> expect_fault Program.Step_limit_exceeded);
              ignore (run ~max_steps:steps retained |> expect_value expected);
              match
                Runtime.execute_retained_report
                  ~max_active_stack_bytes:(stack - 1) ~max_steps:steps retained
                |> Runtime.outcome
              with
              | Error _ -> ()
              | Ok _ -> Alcotest.fail "closed entry escaped its stack quota");
          match Runtime.retain ~max_active_stack_bytes:(stack - 1) image with
          | Error _ -> ()
          | Ok retained ->
              Runtime.release retained |> checked;
              Alcotest.fail "closed entry mapped beyond its stack quota")
        [
          ("42;", 42L, false, false);
          (pressure 14, 105L, true, false);
          (pressure 48, 1176L, true, true);
        ])
    modes

let closed_entry_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, kind) ->
          let image = compile mode source in
          let steps =
            Runtime.execute_report ~max_steps:10000 image |> expect_fault kind
          in
          with_retained image (fun retained ->
              for _ = 1 to 3 do
                let report = run ~max_steps:steps retained in
                Alcotest.(check int)
                  "same original closed-entry fault work" steps
                  (report |> expect_fault kind);
                Alcotest.(check string)
                  "fault captures no output" ""
                  (Runtime.output_bytes report)
              done;
              ignore
                (run ~max_steps:(steps - 1) retained
                |> expect_fault Program.Step_limit_exceeded);
              ignore (run ~max_steps:steps retained |> expect_fault kind)))
        [
          ("42/0;", Program.Division_by_zero);
          ("42%0;", Program.Division_by_zero);
          ( "1+(2+(3+(4+(5+(6+(7+(8+(9+(10+(11+(12+(13+(14/0)))))))))))));",
            Program.Division_by_zero );
        ])
    modes

let closed_entry_bridge_guards () =
  let abi =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> 1
    | Runtime.Linux_x86_64 -> 2
    | Runtime.Unsupported -> Alcotest.fail "unsupported native host"
  in
  let reject image message ?code ?functions ?stack ?(storage = (0, 0, 0, "")) ()
      =
    let code = Option.value code ~default:(Program.code image) in
    let functions =
      Option.value functions
        ~default:(Array.of_list (Program.windows_unwind_functions image))
    in
    let stack = Option.value stack ~default:(Program.entry_stack_bytes image) in
    Alcotest.check_raises "closed-entry bridge rejects before mapping"
      (Invalid_argument message) (fun () ->
        let retained =
          raw_retain (Obj.repr (code, functions, abi, stack, storage))
        in
        raw_release retained)
  in
  List.iter
    (fun mode ->
      let frameless = compile mode "42;" in
      let spilled =
        compile mode "1+(2+(3+(4+(5+(6+(7+(8+(9+(10+(11+(12+(13+14))))))))))));"
      in
      List.iter
        (fun image ->
          let code = Program.code image in
          let functions =
            Array.of_list (Program.windows_unwind_functions image)
          in
          let stack = Program.entry_stack_bytes image in
          let retained =
            raw_retain (Obj.repr (code, functions, abi, stack, (0, 0, 0, "")))
          in
          raw_release retained;
          reject image
            "native program entry stack metadata does not match its unwind \
             frame"
            ~stack:(stack + 8) ();
          reject image "retained native closed entry cannot own a data arena"
            ~storage:(1, 0, 0, "\000") ();
          let first, last, unwind = functions.(0) in
          reject image
            "retained native closed entry range does not cover its code"
            ~functions:[| (first + 1, last, unwind) |]
            ();
          reject image
            "retained native closed entry range does not cover its code"
            ~functions:[| (first, last - 1, unwind) |]
            ();
          let changed = Bytes.of_string code in
          let capture = if Program.frame_bytes image = 0 then 0 else 7 in
          Bytes.set changed (capture + 2)
            (Char.chr (if abi = 1 then 0xfb else 0xcb));
          reject image
            "retained native closed entry has another status prologue"
            ~code:(Bytes.to_string changed) ())
        [ frameless; spilled ];
      let first, last, unwind =
        List.hd (Program.windows_unwind_functions spilled)
      in
      let changed = Bytes.of_string (Program.code spilled) in
      Bytes.set changed 3 (Char.chr (Char.code (Bytes.get changed 3) lxor 16));
      reject spilled
        "retained native closed entry code disagrees with its spill frame"
        ~code:(Bytes.to_string changed) ();
      let mutate index value message =
        let changed = Bytes.of_string unwind in
        Bytes.set changed index (Char.chr value);
        reject spilled message
          ~functions:[| (first, last, Bytes.to_string changed) |]
          ()
      in
      mutate 0 9 "retained native closed entry unwind header is malformed";
      mutate 3 1 "retained native closed entry unwind header is malformed";
      mutate 4 6 "retained native closed entry unwind header is malformed";
      mutate 2 0 "retained native closed entry unwind allocation is malformed";
      mutate 5 0 "retained native closed entry unwind allocation is malformed";
      mutate 5 0x12
        "retained native closed entry unwind allocation exceeds its frame bound";
      let large_small = "\001\007\002\000\007\001\001\000" in
      reject spilled
        "retained native closed entry unwind allocation is not shortest"
        ~functions:[| (first, last, large_small) |]
        ())
    modes

let () =
  if Runtime.platform () = Runtime.Unsupported then
    failwith "retained native tests require Windows or Linux x86-64";
  Alcotest.run "Retained native images"
    [
      ( "retained images",
        [
          Alcotest.test_case "original storage and code survive activations"
            `Quick persistent_original_storage;
          Alcotest.test_case "reached faults retain writes and recover" `Quick
            reached_faults_keep_writes;
          Alcotest.test_case "fresh capture preserves original callback owners"
            `Quick fresh_capture_and_original_owners;
          Alcotest.test_case "original AOT entry executes its load regions"
            `Quick original_aot_entry_load_regions;
          Alcotest.test_case "separate arenas and released executable ownership"
            `Quick separate_arenas_and_expiry;
          Alcotest.test_case "original image bounds and foreign ABI rejection"
            `Quick original_image_bounds;
          Alcotest.test_case "recursive activation quotas retain reached writes"
            `Quick recursive_activation_quota_recovery;
          Alcotest.test_case "shared native owners survive domain entry and GC"
            `Quick shared_owner_across_domains;
          Alcotest.test_case "zero-data images and collected owners" `Quick
            zero_data_and_collection;
          Alcotest.test_case "closed entries preserve their original frames"
            `Quick closed_entry_images;
          Alcotest.test_case "closed-entry faults retain exact native work"
            `Quick closed_entry_faults;
          Alcotest.test_case
            "closed-entry bridge guards reject changed metadata" `Quick
            closed_entry_bridge_guards;
        ] );
    ]
