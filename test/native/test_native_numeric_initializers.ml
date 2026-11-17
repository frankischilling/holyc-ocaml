open Holyc_lib
module Image = X86_64_program
module Task = Native_source_execution

type target = Isolated | Source_task

type observation = {
  context : string;
  outcome : (Image.word_type * int64, Diagnostic.t list) result;
  preparation : int;
  executed : int;
  output : string;
}

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (item : Diagnostic.t) -> item.code ^ ": " ^ item.message)
  |> String.concat "; "

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let targets = [ Isolated; Source_task ]

let run ?(max_steps = 100_000) ?max_initializer_steps ~target ~mode text =
  let context =
    (match target with
      | Isolated -> "host-jit"
      | Source_task -> "host-jit-task")
    ^ " "
    ^ (match mode with
      | Preprocessor.Jit -> "jit"
      | Preprocessor.Aot -> "aot")
    ^ ": " ^ text
  in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-numeric-initializers.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  match target with
  | Isolated ->
      let report =
        Native_program.evaluate ~max_code_bytes:1_048_576 ?max_initializer_steps
          session ~config ~source ~max_steps
      in
      let outcome =
        Native_program.outcome report
        |> Result.map
             (fun (result : Native_program.result Native_program.checked) ->
               let word = Option.get result.value.execution.final_value in
               (word.type_, word.bits))
      in
      {
        context;
        outcome;
        preparation = Native_program.preparation_steps report;
        executed =
          Option.value (Native_program.executed_steps report) ~default:0;
        output = Native_program.output_bytes report;
      }
  | Source_task ->
      let report =
        Task.evaluate ~max_code_bytes:1_048_576 ?max_initializer_steps session
          ~config ~source ~max_steps
      in
      Option.iter
        (fun progress ->
          Alcotest.(check int)
            "source preparation does not substitute interpreted execution" 0
            progress.Integer_task.runtime.executed_steps)
        (Task.source_progress report);
      let outcome =
        Task.outcome report
        |> Result.map (fun (result : Task.result Task.checked) ->
            let word = Option.get result.value.final_value in
            (word.type_, word.bits))
      in
      {
        context;
        outcome;
        preparation = Task.preparation_steps report;
        executed = Task.executed_steps report;
        output = Task.output_bytes report;
      }

let expect ?(output = "") type_ bits report =
  let actual_type, actual_bits =
    report.outcome
    |> Result.map_error (fun errors ->
        report.context ^ ": " ^ diagnostics errors)
    |> checked
  in
  Alcotest.(check bool)
    "original numeric result class" true (actual_type = type_);
  Alcotest.(check int64) report.context bits actual_bits;
  Alcotest.(check string) "reached output" output report.output;
  Alcotest.(check bool) "native instructions executed" true (report.executed > 0)

let reject ?code report =
  match report.outcome with
  | Ok _ ->
      Alcotest.fail ("initializer unexpectedly completed: " ^ report.context)
  | Error errors ->
      Alcotest.(check bool) "failure retains diagnostics" true (errors <> []);
      Option.iter
        (fun code ->
          Alcotest.(check bool)
            (report.context ^ ": " ^ diagnostics errors)
            true
            (List.exists (fun (item : Diagnostic.t) -> item.code = code) errors))
        code

let all_targets action =
  List.iter
    (fun mode -> List.iter (fun target -> action target mode) targets)
    modes

let reference_targets action =
  List.iter
    (fun mode ->
      List.iter
        (fun target ->
          if mode = Preprocessor.Aot || target = Source_task then
            action target mode)
        targets)
    modes

(* PrsVarInit calls the expression, converts the materialized word, then copies
   tmpc->size bytes (pinned PrsVar.HC:97-107). These fixed words are independent
   expectations; no host-float calculation supplies the answers. *)
let closed_layouts () =
  let cases =
    [
      ("F64 value=1.5;value;", 0x3ff8_0000_0000_0000L);
      ("F64 value=42;value;", 0x4045_0000_0000_0000L);
      ("F64 value=1.5<2.0;value;", 0x3ff0_0000_0000_0000L);
      ("F64 value=-0.0;value;", Int64.min_int);
      ("F64 values[2][2]={{1.5,2},{3.75,-0.0}};values[1][1];", Int64.min_int);
      ( "F64 Read(){static F64 values[2][2]={{1.5,2},{3.75,-0.0}};return \
         values[0][1];}Read();Read();",
        0x4000_0000_0000_0000L );
      ( "class Box{U8 tag;F64 values[2];I8 small;U8 tail;};Box \
         box={65,{1.5,2},255.75,66};F64 Read(){if(box.tag!=65 || box.small!=-1 \
         || box.tail!=66)return 0.0;return box.values[1];}Read();",
        0x4000_0000_0000_0000L );
      ( "class Box{U8 tag;F64 values[2];I8 small;U8 tail;};F64 Read(){static \
         Box box={65,{1.5,2},255.75,66};if(box.tag!=65 || box.small!=-1 || \
         box.tail!=66)return 0.0;return box.values[0];}Read();Read();",
        0x3ff8_0000_0000_0000L );
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode source |> expect Image.F64 bits)
        cases;
      run ~target ~mode "I64 value=1.5<2.0;value;" |> expect Image.I64 1L)

let integer_storage_widths () =
  let cases =
    [
      ("Bool", "255.75", -1L);
      ("I8", "255.75", -1L);
      ("U8", "257.75", 1L);
      ("I16", "65535.75", -1L);
      ("U16", "65578.75", 42L);
      ("I32", "4294967295.75", -1L);
      ("U32", "4294967295.75", 4_294_967_295L);
      ("I64", "-42.75", -42L);
      ("U64", "-1.75", -1L);
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (type_, literal, bits) ->
          List.iter
            (fun source -> run ~target ~mode source |> expect Image.I64 bits)
            [
              type_ ^ " value=" ^ literal ^ ";I64 Read(){return value;}Read();";
              "I64 Read(){static " ^ type_ ^ " value=" ^ literal
              ^ ";return value;}Read();Read();";
            ])
        cases)

let reference_destinations_stay_separate () =
  let sources expression =
    List.concat_map
      (fun field ->
        let header = "class Box{U8 tag;" ^ field ^ "U8 tail;};" in
        [
          header ^ "Box object={7," ^ expression ^ ",9};42;";
          header ^ "I64 Read(){static Box object={7," ^ expression
          ^ ",9};return 42;}Read();";
        ])
      [ "I64 (*value)(I64);"; "I64 *value;" ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun source -> run ~target ~mode source |> reject)
        (sources "1.5"));
  reference_targets (fun target mode ->
      List.iter
        (fun source ->
          run ~target ~mode ("F64 Seed(){return 1.5;}" ^ source) |> reject)
        (sources "Seed()"));
  List.iter
    (fun source ->
      run ~target:Source_task ~mode:Preprocessor.Jit source
      |> expect Image.I64 1L)
    [
      "F64 target;class Box{U8 *p;};Box object={(&target)(U8*)};I64 F(){return \
       object.p==(&target)(U8*);}F();";
      "F64 target;class Box{U8 *p;};I64 F(){static Box \
       object={(&target)(U8*)};return object.p==(&target)(U8*);}F();F();";
    ]

let integer_to_f64_words () =
  let cases =
    [
      ("9007199254740993", 0x4340_0000_0000_0000L);
      ("9007199254740995", 0x4340_0000_0000_0002L);
      ("-9007199254740995", 0xc340_0000_0000_0002L);
      ("0xffffffffffffffff", 0xbff0_0000_0000_0000L);
      ("0x8000000000000000", 0xc3e0_0000_0000_0000L);
      ("0x7fffffffffffffff", 0x43e0_0000_0000_0000L);
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (word, bits) ->
          run ~target ~mode ("F64 value=" ^ word ^ ";value;")
          |> expect Image.F64 bits)
        cases);
  reference_targets (fun target mode ->
      List.iter
        (fun (word, bits) ->
          run ~target ~mode
            ("U64 Seed(){return " ^ word ^ ";}F64 value=Seed();value;")
          |> expect Image.F64 bits)
        cases)

let referenced_numeric_leaves () =
  let cases =
    [
      ( "I64 Seed(){return 42;}F64 value=Seed();value;",
        Image.F64,
        0x4045_0000_0000_0000L );
      ( "I64 Seed(){return 42;}F64 value=Seed()+1;value;",
        Image.F64,
        0x4045_8000_0000_0000L );
      ("I64 seed=42;F64 value=seed+1;value;", Image.F64, 0x4045_8000_0000_0000L);
      ( "F64 Seed(){return 1.5;}F64 value=Seed()<2.0;value;",
        Image.F64,
        0x3ff0_0000_0000_0000L );
      ("F64 Seed(){return 42.75;}I64 value=Seed();value;", Image.I64, 42L);
      ( "F64 Seed(){return 257.75;}U8 values[2]={Seed(),Seed()};I64 \
         Read(){return values[0]+values[1];}Read();",
        Image.I64,
        2L );
      ( "I64 Seed(){return 42;}F64 Read(){static F64 value=Seed();return \
         value;}Read();Read();",
        Image.F64,
        0x4045_0000_0000_0000L );
      ( "F64 Seed(){return 1.5;}F64 Read(){static F64 value=Seed()<2.0;return \
         value;}Read();Read();",
        Image.F64,
        0x3ff0_0000_0000_0000L );
      ( "F64 Seed(){return 257.75;}I64 Read(){static U8 value=Seed();return \
         value;}Read();Read();",
        Image.I64,
        1L );
      ( "I64 Seed(){return 42;}class Box{U8 tag;F64 value;U8 tail;};Box \
         box={65,Seed(),66};F64 Read(){if(box.tag!=65 || box.tail!=66)return \
         0.0;return box.value;}Read();",
        Image.F64,
        0x4045_0000_0000_0000L );
      ( "F64 Seed(){return 257.75;}class Box{U8 tag;U8 value;U8 tail;};I64 \
         Read(){static Box box={65,Seed(),66};if(box.tag!=65 || \
         box.tail!=66)return -1;return box.value;}Read();Read();",
        Image.I64,
        1L );
    ]
    @ List.concat_map
        (fun type_ ->
          [
            ( "F64 Seed(){return 42.75;}" ^ type_
              ^ " values[2]={Seed(),Seed()};I64 Read(){return "
              ^ "values[0]+values[1];}Read();",
              Image.I64,
              84L );
            ( "F64 Seed(){return 42.75;}I64 Read(){static " ^ type_
              ^ " values[2]={Seed(),Seed()};return values[0]+values[1];}"
              ^ "Read();Read();",
              Image.I64,
              84L );
          ])
        [ "I16"; "I32"; "I64" ]
  in
  reference_targets (fun target mode ->
      List.iter
        (fun (source, type_, bits) ->
          run ~target ~mode source |> expect type_ bits)
        cases)

let materialized_f64_words () =
  reference_targets (fun target mode ->
      List.iter
        (fun bits ->
          let producer =
            Printf.sprintf
              "F64 Bits(){F64 value;(&value)(U64*)[0]=0x%016Lx;return value;}"
              bits
          in
          List.iter
            (fun consumer ->
              run ~target ~mode (producer ^ consumer) |> expect Image.F64 bits)
            [
              "F64 value=Bits();value;";
              "F64 Read(){static F64 value=Bits();return value;}Read();";
            ];
          if bits <> Int64.min_int then
            run ~target ~mode (producer ^ "I64 value=Bits();value;")
            |> expect Image.I64 Int64.min_int)
        [
          Int64.min_int;
          0x7ff0_0000_0000_0042L;
          0x7ff0_0000_0000_0000L;
          0xfff0_0000_0000_0000L;
          0x43e0_0000_0000_0001L;
        ])

let declaration_effects () =
  let source =
    "extern U0 PutChars(U64 ch);I64 Seed(U64 ch){PutChars(ch);return 42;}F64 \
     Fraction(U64 ch){PutChars(ch);return 257.75;}F64 first=Seed('A');I64 \
     Unused(){static U8 value=Fraction('B');return value;}F64 Read(){static \
     F64 value=Seed('C');return value;}Read();Read();42;"
  in
  reference_targets (fun target mode ->
      let report = run ~target ~mode source in
      expect ~output:"ABC" Image.I64 42L report;
      if mode = Preprocessor.Aot then
        Alcotest.(check int)
          "AOT calls execute at load time" 0 report.preparation
      else
        Alcotest.(check bool)
          "JIT declarations charge preparation" true (report.preparation > 0));
  let malformed =
    "extern U0 PutChars(U64 ch);F64 Seed(){PutChars('A');return 257.75;}I64 \
     Unused(){static U8 value=Seed();return value;}I64 Broken("
  in
  let report = run ~target:Source_task ~mode:Preprocessor.Jit malformed in
  reject report;
  Alcotest.(check string)
    "unused static runs before later syntax failure" "A" report.output;
  Alcotest.(check bool)
    "syntax failure retains completed preparation" true (report.preparation > 1)

let initializer_faults () =
  let prefix =
    "extern U0 PutChars(U64 ch);F64 Good(){PutChars('A');return 257.75;}F64 \
     Bad(){PutChars('B');I64 zero=0;I64 q=7/zero;return 258.75;}"
  in
  reference_targets (fun target mode ->
      List.iter
        (fun tail ->
          let report = run ~target ~mode (prefix ^ tail) in
          reject ~code:"HCIRVM0009" report;
          Alcotest.(check string)
            "later initializer fault preserves output" "AB" report.output;
          if mode = Preprocessor.Aot then
            Alcotest.(check int)
              "load fault has no preparation charge" 0 report.preparation
          else
            Alcotest.(check bool)
              "live fault retains reached preparation" true
              (report.preparation > 1))
        [
          "U8 values[2]={Good(),Bad()};PutChars('C');42;";
          "I64 Unused(){static U8 values[2]={Good(),Bad()};return \
           values[0];}PutChars('C');42;";
        ])

let preparation_and_runtime_limits () =
  all_targets (fun target mode ->
      let source = "F64 value=42;value;" in
      let full = run ~target ~mode source in
      expect Image.F64 0x4045_0000_0000_0000L full;
      Alcotest.(check bool)
        "numeric preparation includes destination copy" true
        (full.preparation > 8);
      run ~target ~mode ~max_initializer_steps:full.preparation
        ~max_steps:full.executed source
      |> expect Image.F64 0x4045_0000_0000_0000L;
      let short_copy =
        run ~target ~mode ~max_initializer_steps:(full.preparation - 1) source
      in
      reject ~code:"HCIRVM0007" short_copy;
      Alcotest.(check bool)
        "failed copy respects preparation allowance" true
        (short_copy.preparation <= full.preparation - 1);
      let short_steps =
        run ~target ~mode ~max_steps:(full.executed - 1) source
      in
      reject ~code:"HCIRVM0007" short_steps;
      Alcotest.(check int)
        "runtime one-below allowance" (full.executed - 1) short_steps.executed);
  List.iter
    (fun declaration ->
      let source =
        "extern U0 PutChars(U64 ch);F64 Seed(){PutChars('A');return 257.75;}"
        ^ declaration
      in
      let target = Source_task and mode = Preprocessor.Jit in
      let full = run ~target ~mode source in
      expect ~output:"A" Image.I64 42L full;
      let failed =
        run ~target ~mode ~max_initializer_steps:(full.preparation - 1) source
      in
      reject ~code:"HCIRVM0007" failed;
      Alcotest.(check string)
        "conversion copy fault keeps expression effects" "A" failed.output;
      Alcotest.(check int)
        "one-byte copy fails after completed expression" (full.preparation - 1)
        failed.preparation)
    [
      "U8 value=Seed();42;";
      "I64 Unused(){static U8 value=Seed();return value;}42;";
    ]

(* PrsVar.HC:97-107 converts after Call has materialized a binary64 result.
   Its nonconstant AOT branch instead emits IC_ASSIGN; OptPass012 and BackFA
   apply RES_TO_INT to the extended arithmetic result before a binary64 store. *)
let preparation_and_load_precision () =
  reference_targets (fun target mode ->
      let bits =
        if mode = Preprocessor.Jit then 9_007_199_254_740_992L
        else 9_007_199_254_740_993L
      in
      List.iter
        (fun declaration ->
          run ~target ~mode
            ("F64 Seed(){return 9007199254740992.0;}" ^ declaration)
          |> expect Image.I64 bits)
        [
          "I64 value=Seed()+1.0;value;";
          "I64 Read(){static I64 value=Seed()+1.0;return value;}Read();";
        ])

let closed_capture_authority () =
  let module VM = Holyc_lib__Ir.Integer_interpreter in
  let module Runtime = Native_program_execution in
  let module Capture = Holyc_lib__Ir.Native_scalar_capture in
  let module Program = Holyc_lib__Ir.Numeric_initializer_program in
  let module Calls = Holyc_lib__Ir.Runtime_call_context in
  let module Typing = Holyc_lib__Driver.Initializer_fragment_typing in
  let module Declarations = Holyc_lib__Sema.Declaration_collection in
  let module Global = Holyc_lib__Sema.Initializer_fragment in
  let module Static = Holyc_lib__Sema.Static_initializer_fragment in
  let rejected label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  List.iter
    (fun (text, saved_bits) ->
      List.iter
        (fun scenario ->
          let session = Session.create () in
          let source =
            Session.add_source session ~path:"numeric-initializer-capture.hc"
              ~contents:text
          in
          let config =
            Preprocessor.Config.create ~compilation_mode:Aot () |> checked
          in
          let preparation_steps = ref 0
          and switch_work = ref 0
          and dimension_work = ref 0
          and default_bytes = ref 0 in
          let saved_request = ref None in
          let native_initializer request =
            saved_request := Some request;
            let copied_request = Obj.obj (Obj.dup (Obj.repr request)) in
            rejected "copied numeric request cannot claim original authority"
              (VM.check_native_constant copied_request);
            let program = VM.native_constant_program request in
            if scenario = `Valid then (
              let source = Program.source program in
              let namespace =
                match source with
                | Program.Global (authority, _) ->
                    Global.authorized_namespace authority
                | Program.Static (fragment, _) -> Static.namespace fragment
              in
              let typing =
                Typing.create_aot_context
                  ~table:(Session.semantic_symbols session)
                  ~parent:(Declarations.namespace_scope namespace)
                |> checked
              in
              let retyped =
                (match source with
                  | Program.Global (authority, _) ->
                      Typing.prepare typing
                        (Global.authorized_fragment authority)
                  | Program.Static (fragment, _) ->
                      Typing.prepare_static typing fragment)
                |> checked
              in
              let entry = Program.entry program in
              let initialization = Program.initialization program in
              let foreign_calls =
                Calls.create ~records:(Typing.records typing)
                  ~function_sources:(Typing.function_sources typing)
                  ~top_level:retyped ~initialization ~entry ~entry_calls:[]
                  ~functions:[]
                |> Result.map_error diagnostics
                |> checked
              in
              Alcotest.(check bool)
                "foreign typed batch still matches the original call-free graph"
                true
                (Calls.matches foreign_calls ~entry
                   ~initialization:(Some initialization) ~functions:[]);
              rejected "numeric initializer retains its exact typed batch"
                (Program.create ~source ~globals:(Program.globals program)
                   ~lowered:(Program.lowered program) ~initialization
                   ~runtime_calls:foreign_calls));
            let image =
              Image.compile_source_initializer ~max_ir_instructions:4096
                ~max_code_bytes:65_536 request
              |> Result.map_error (fun errors ->
                  errors
                  |> List.map (fun (item : Image.error) -> item.message)
                  |> String.concat "; ")
              |> checked
            in
            let retained = Runtime.retain image |> checked in
            Fun.protect
              ~finally:(fun () -> Runtime.release retained |> checked)
              (fun () ->
                let budget =
                  Runtime.create_budget ~max_steps:100_000 () |> checked
                in
                let report =
                  Runtime.execute_retained_budget_report budget retained
                in
                ignore (Runtime.outcome report |> checked);
                let work = (Runtime.budget_progress budget).executed_steps in
                let capture =
                  Option.get (Runtime.source_initializer_capture report)
                in
                rejected "numeric initializer entry cannot replay"
                  (VM.claim_native_constant request);
                rejected "numeric initializer image cannot execute twice"
                  (Runtime.outcome
                     (Runtime.execute_retained_budget_report budget retained));
                Alcotest.(check int)
                  "rejected replay consumes no native work" work
                  (Runtime.budget_progress budget).executed_steps;
                let copied_program = Obj.obj (Obj.dup (Obj.repr program)) in
                rejected "copied numeric program cannot consume capture"
                  (Capture.consume capture ~program:copied_program ~work);
                rejected "wrong work cannot consume numeric capture"
                  (Capture.consume capture ~program ~work:(work + 1));
                rejected "foreign domain cannot finish numeric preparation"
                  (Domain.join
                     (Domain.spawn (fun () ->
                          VM.finish_native_constant request ~steps:work
                            (Some capture))));
                Gc.full_major ();
                Gc.compact ();
                if scenario = `Released then Runtime.release retained |> checked;
                let finished =
                  VM.finish_native_constant request ~steps:work
                    (if scenario = `Missing then None else Some capture)
                in
                (match scenario with
                | `Released ->
                    rejected "released numeric image cannot publish" finished
                | `Missing | `Valid -> finished |> checked);
                (if scenario = `Valid then
                   let receipt =
                     VM.native_constant_receipt request |> checked
                   in
                   let value =
                     VM.constant_receipt_result receipt
                     |> VM.final_value |> Option.get
                   in
                   Alcotest.(check int64)
                     "receipt transports actual converted declaration word"
                     saved_bits value.bits);
                rejected "numeric initializer completion cannot replay"
                  (VM.finish_native_constant request ~steps:work (Some capture));
                Ok ())
          in
          let compiled =
            Native_program.compile_with_preparation ~native_initializer
              ~preparation_steps ~switch_work ~dimension_work ~default_bytes
              session ~config ~source
          in
          let request =
            match !saved_request with
            | Some request -> request
            | None ->
                let reason =
                  match compiled with
                  | Ok _ -> "compilation completed without its native callback"
                  | Error errors -> diagnostics errors
                in
                Alcotest.fail
                  ("native initializer callback was not entered: " ^ reason)
          in
          rejected "numeric request expires with its parser callback"
            (VM.check_native_constant request);
          Alcotest.(check bool)
            "reached numeric work remains charged" true (!preparation_steps > 0);
          match scenario with
          | `Valid ->
              ignore (compiled |> Result.map_error diagnostics |> checked)
          | `Missing | `Released ->
              rejected "unproved numeric capture cannot complete compilation"
                compiled)
        [ `Valid; `Missing; `Released ])
    [
      ("F64 value=42;value;", 0x4045_0000_0000_0000L);
      ("I64 Read(){static I64 value=42.75;return value;}Read();", 42L);
    ]

let () =
  if not (Native_execution.x87_supported ()) then
    print_endline
      "native numeric initializer tests: x87 unsupported on this host, skipped"
  else
    Alcotest.run "Native numeric initializers"
      [
        ( "declarations",
          [
            Alcotest.test_case "closed globals, arrays and packed static leaves"
              `Quick closed_layouts;
            Alcotest.test_case "convert before each integer storage width"
              `Quick integer_storage_widths;
            Alcotest.test_case
              "signed full-word conversion and nearest-even ties" `Quick
              integer_to_f64_words;
            Alcotest.test_case "live and AOT load numeric leaves" `Quick
              referenced_numeric_leaves;
            Alcotest.test_case
              "materialized F64 bits and exceptional conversion" `Quick
              materialized_f64_words;
            Alcotest.test_case "declaration and load conversion precision"
              `Quick preparation_and_load_precision;
            Alcotest.test_case "unused static timing and later parser failure"
              `Quick declaration_effects;
            Alcotest.test_case
              "later faults retain original initializer effects" `Quick
              initializer_faults;
            Alcotest.test_case "preparation, copy and runtime limits" `Quick
              preparation_and_runtime_limits;
            Alcotest.test_case "native initializer capture ownership and replay"
              `Quick closed_capture_authority;
            Alcotest.test_case
              "F64 does not acquire reference destination authority" `Quick
              reference_destinations_stay_separate;
          ] );
      ]
