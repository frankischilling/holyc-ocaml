open Holyc_lib
module Image = X86_64_program
module Source = Native_source_execution
module Encoder = Holyc_lib__Backend.X86_64_encoder

type target = Isolated | Source_task

type observation = {
  context : string;
  outcome : (Image.word_type * int64, Diagnostic.t list) result;
  steps : int;
  output : string;
}

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  List.map
    (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
    errors
  |> String.concat "; "

let run ?(max_steps = 100_000) ~target ~mode contents =
  let context =
    (match target with
      | Isolated -> "host-jit"
      | Source_task -> "host-jit-task")
    ^ " "
    ^ (match mode with
      | Preprocessor.Jit -> "jit"
      | Preprocessor.Aot -> "aot")
    ^ ": " ^ contents
  in
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-f64-extended.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  match target with
  | Isolated ->
      let report =
        Native_program.evaluate ~max_code_bytes:1_048_576 session ~config
          ~source ~max_steps
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
        steps = Option.value (Native_program.executed_steps report) ~default:0;
        output = Native_program.output_bytes report;
      }
  | Source_task ->
      let report =
        Source.evaluate ~max_code_bytes:1_048_576 session ~config ~source
          ~max_steps
      in
      Option.iter
        (fun progress ->
          Alcotest.(check int)
            "no interpreted task work" 0
            progress.Integer_task.runtime.executed_steps)
        (Source.source_progress report);
      let outcome =
        Source.outcome report
        |> Result.map (fun (result : Source.result Source.checked) ->
            let word = Option.get result.value.final_value in
            (word.type_, word.bits))
      in
      {
        context;
        outcome;
        steps = Source.executed_steps report;
        output = Source.output_bytes report;
      }

let all_targets action =
  List.iter
    (fun mode ->
      List.iter (fun target -> action target mode) [ Isolated; Source_task ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let value report =
  let word =
    report.outcome
    |> Result.map_error (fun errors ->
        report.context ^ ": " ^ diagnostics errors)
    |> checked
  in
  Alcotest.(check bool) "actual native work" true (report.steps > 0);
  word

let expect ?(output = "") type_ bits report =
  let actual_type, actual_bits = value report in
  Alcotest.(check bool) "result class" true (actual_type = type_);
  Alcotest.(check int64) report.context bits actual_bits;
  Alcotest.(check string) "reached output" output report.output

let bits_source =
  "F64 Bits(U64 word){F64 value;(&value)(U64*)[0]=word;return value;}"

let modulo_source = "F64 Mod(F64 left,F64 right){return left%right;}"

(* Templates.HC:23-37 uses FPREM, repeats C2 and stores binary64. These expected
   words follow exact binary arithmetic; no host remainder supplies them. *)
let modulo_values () =
  let cases =
    [
      ("Mod(7.5,2.0);", 0x3ff8_0000_0000_0000L);
      ("Mod(-7.5,2.0);", 0xbff8_0000_0000_0000L);
      ("Mod(7.5,-2.0);", 0x3ff8_0000_0000_0000L);
      ("Mod(-7.5,-2.0);", 0xbff8_0000_0000_0000L);
      ("Mod(4.0,2.0);", 0L);
      ("Mod(-4.0,2.0);", Int64.min_int);
      ("Mod(Bits(0x8000000000000000),2.0);", Int64.min_int);
      ("Mod(7.5,Bits(0x7ff0000000000000));", 0x401e_0000_0000_0000L);
      ("Mod(Bits(0x7fefffffffffffff),3.0);", 0x4000_0000_0000_0000L);
      ("Mod(1.0,Bits(1));", 0L);
      ("Mod(7.5,2.0);Mod(9.75,2.0);", 0x3ffc_0000_0000_0000L);
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode (bits_source ^ modulo_source ^ source)
          |> expect Image.F64 bits)
        cases)

let modulo_materialization () =
  let cases =
    [
      ( "F64 Mod(I64 left,F64 right){return \
         left%right;}Mod(9007199254740993,3.0);",
        Image.F64,
        0x4000_0000_0000_0000L );
      ( "F64 Mod(F64 left,I64 right){return \
         left%right;}Mod(9007199254740992.0,9007199254740993);",
        Image.F64,
        0L );
      ( "F64 Mod(F64 a,F64 b){return (a+b)%3.0;}Mod(9007199254740992.0,1.0);",
        Image.F64,
        0x4000_0000_0000_0000L );
      ( "I64 Mod(F64 left,F64 right){return left%right;}Mod(7.5,2.0);",
        Image.I64,
        1L );
      ( "F64 Both(F64 a,F64 b,F64 c,F64 d){return \
         (a%b)+(c%d);}Both(7.5,2.0,9.75,2.0);",
        Image.F64,
        0x400a_0000_0000_0000L );
      ( modulo_source
        ^ "F64 Repeat(){I64 n;F64 \
           value=0.0;for(n=0;n<40;n++)value=Mod(7.5,2.0);return \
           value;}Repeat();",
        Image.F64,
        0x3ff8_0000_0000_0000L );
      ( modulo_source
        ^ "class Box{U8 tag;F64 value;};F64 Read(){Box box;F64 \
           values[2];values[0]=Mod(7.5,2.0);values[1]=Mod(-7.5,2.0);box.value=Mod(9.75,2.0);return \
           box.value;}Read();",
        Image.F64,
        0x3ffc_0000_0000_0000L );
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, type_, bits) ->
          run ~target ~mode source |> expect type_ bits)
        cases)

let modulo_invalid () =
  all_targets (fun target mode ->
      List.iter
        (fun source ->
          let report =
            run ~target ~mode (bits_source ^ modulo_source ^ source)
          in
          let type_, bits = value report in
          Alcotest.(check bool)
            "invalid operation retains F64 result" true (type_ = Image.F64);
          Alcotest.(check bool)
            "masked invalid operation produces NaN" true
            (Int64.logand bits 0x7ff0_0000_0000_0000L = 0x7ff0_0000_0000_0000L
            && Int64.logand bits 0x000f_ffff_ffff_ffffL <> 0L);
          Alcotest.(check string) "no output" "" report.output)
        [
          "Mod(1.0,0.0);"; "Mod(0.0,0.0);"; "Mod(Bits(0x7ff0000000000000),2.0);";
        ])

(* BackFB.HC:59-100 and 163-200 select FLDPI for an immediate update RHS.
   Its extended value exceeds stored binary64 pi by 565 * 2^-62.
   BackLib.HC:423-440 disables that selection with the function's Option(36). *)
let compound_builtin_options () =
  let cases =
    [
      ( "F64 value=-3.141592653589793;F64 \
         Read(){value+=3.141592653589793;return value;}Read();",
        0x3ca1_a800_0000_0000L );
      ( "F64 Read(){static F64 \
         value=-3.141592653589793;value+=3.141592653589793;return \
         value;}Read();",
        0x3ca1_a800_0000_0000L );
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode source |> expect Image.F64 bits)
        cases);
  let option_cases =
    [
      ( "F64 value=-3.141592653589793;#exe{Option(36,1);}F64 \
         Read(){value+=3.141592653589793;return value;}I64 \
         marker;#exe{Option(36,0);}Read();",
        0L );
      ( "F64 value=-3.141592653589793;F64 \
         Read(){value+=3.141592653589793;return value;}I64 \
         marker;#exe{Option(36,1);}Read();",
        0x3ca1_a800_0000_0000L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bits) ->
          run ~target:Source_task ~mode source |> expect Image.F64 bits)
        option_cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

(* OptLib.HC:535-539 and OptPass789A.HC:65-71 retain a converted integer RHS
   in ST0. ICFOpEqu still stores binary64 before a later read of its destination. *)
let compound_precision () =
  let cases =
    [
      ( "F64 value=-9007199254740992.0;I64 delta=9007199254740993;F64 \
         Read(){value+=delta;return value;}Read();",
        0x3ff0_0000_0000_0000L );
      ( "F64 value=9007199254740992.0;I64 delta=1;F64 \
         Read(){value+=delta;return value-9007199254740992.0;}Read();",
        0L );
      ( "F64 Read(){static F64 value=9007199254740992.0;value+=1.0;return \
         value-9007199254740992.0;}Read();",
        0L );
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode source |> expect Image.F64 bits)
        cases)

(* BackFA.HC:549-605 rounds an old integer destination through ICFCvt2,
   materializes FPREM, then truncates and stores an integer destination.
   MOD has no extended RHS link; 2^53+1 therefore reaches it as 2^53. *)
let modulo_updates () =
  let cases =
    [
      ( "F64 value=7.5;F64 Read(){value%=2.0;return value;}Read();",
        Image.F64,
        0x3ff8_0000_0000_0000L );
      ( "F64 Read(){static F64 value=-7.5;value%=2.0;return value;}Read();",
        Image.F64,
        0xbff8_0000_0000_0000L );
      ( "F64 value=-4.0;F64 Read(){value%=2.0;return value;}Read();",
        Image.F64,
        Int64.min_int );
      ( "I64 value=9007199254740993;I64 Read(){value%=3.0;return value;}Read();",
        Image.I64,
        2L );
      ( "I64 Read(){static I16 value=-7;value%=2.75;return value;}Read();",
        Image.I64,
        -1L );
      ( "F64 value=9007199254740992.0;I64 divisor=9007199254740993;F64 \
         Read(){value%=divisor;return value;}Read();",
        Image.F64,
        0L );
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, type_, bits) ->
          run ~target ~mode source |> expect type_ bits)
        cases)

(* OptLib.HC:124-139 promotes mixed operands before BackA.HC:573-600 shifts
   their raw register words. F64 is signed for the selected right-shift form. *)
let floating_shifts () =
  let cases =
    [
      ( "F64 Shift(F64 value,F64 count){return value<<count;}Shift(1.5,Bits(1));",
        0x7ff0_0000_0000_0000L );
      ( "F64 Shift(F64 value,F64 count){return \
         value>>count;}Shift(Bits(0x8000000000000000),Bits(1));",
        0xc000_0000_0000_0000L );
      ( "F64 Shift(F64 value,F64 count){return \
         value<<count;}Shift(1.5,Bits(64));",
        0x3ff8_0000_0000_0000L );
      ( "F64 Shift(F64 value,F64 count){return \
         value<<count;}Shift(1.5,Bits(65));",
        0x7ff0_0000_0000_0000L );
      ( "F64 Shift(F64 value,F64 count){return \
         value>>count;}Shift(Bits(0x8000000000000000),Bits(63));",
        -1L );
      ( "F64 Shift(F64 value){return value<<1;}Shift(1.5);",
        0x3ff8_0000_0000_0000L );
      ( "F64 Shift(F64 value,I64 count){return value<<count;}Shift(1.5,1);",
        0x3ff8_0000_0000_0000L );
      ( "F64 Shift(I64 value,F64 count){return value<<count;}Shift(3,Bits(1));",
        0x8010_0000_0000_0000L );
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode (bits_source ^ source) |> expect Image.F64 bits)
        cases)

(* OptPass012.HC:888-894 truncates a floating compound RHS to an integer.
   BackC.HC:539-599 then edits the destination's raw word. BackA.HC:603-658
   uses the signed shift form for F64 and the machine's low six count bits. *)
let floating_compound_words () =
  let cases =
    [
      ( "F64 value=1.5;F64 Read(){value|=1;return value;}Read();",
        0x3ff8_0000_0000_0001L );
      ( "F64 Read(){static F64 value=1.5;value^=1.75;return value;}Read();",
        0x3ff8_0000_0000_0001L );
      ( "F64 value;F64 \
         Read(){value=Bits(0x3ff8000000000001);value&=-2.75;return \
         value;}Read();",
        0x3ff8_0000_0000_0000L );
      ( "F64 value=1.5;F64 Read(){value<<=1;return value;}Read();",
        0x7ff0_0000_0000_0000L );
      ( "F64 value=1.5;F64 Read(){value<<=1.75;return value;}Read();",
        0x7ff0_0000_0000_0000L );
      ( "F64 value=1.5;F64 Read(){value<<=65;return value;}Read();",
        0x7ff0_0000_0000_0000L );
      ( "F64 Read(){static F64 value=-0.0;value>>=1.75;return value;}Read();",
        0xc000_0000_0000_0000L );
      ("F64 value=-0.0;F64 Read(){value>>=63;return value;}Read();", -1L);
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode (bits_source ^ source) |> expect Image.F64 bits)
        cases)

(* OptPass012.HC:180-188 folds immediate unary minus before comparison links.
   The 2^53 boundary distinguishes an extended left operand from a stored one.
   A converted return must produce the source's truncated integer word. *)
let negative_literal_boundaries () =
  let cases =
    [
      ( "F64 value=-3.0;I64 Read(){if(value!=-3.0)return -1;return 42;}Read();",
        42L );
      ("F64 value=-3.0;I64 Read(){return value==-3.0;}Read();", 1L);
      ("I64 Read(){return -3.0==-3.0;}Read();", 1L);
      ( "F64 a=-9007199254740992.0,b=-1.0;I64 Read(){return \
         (a+b)<-9007199254740992.0;}Read();",
        1L );
      ("F64 value=-0.0;I64 Read(){return value==-0.0;}Read();", 1L);
      ("I64 Read(){return -3.75;}Read();", -3L);
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode source |> expect Image.I64 bits)
        cases)

(* BackFB.HC:123-126 reads a BY_VAL result back from its destination, whereas
   the indirect path at 220-222 exposes its RAX supplier. An observed update
   cannot claim the former result without the original address-mode proof. *)
let unproved_compound_results () =
  let expect_rejection report =
    match report.outcome with
    | Ok _ -> Alcotest.fail (report.context ^ ": unproved result was admitted")
    | Error errors ->
        Alcotest.(check bool)
          (report.context ^ ": " ^ diagnostics errors)
          true
          (List.exists
             (fun (error : Diagnostic.t) ->
               error.code = "HCBACK0002"
               && String.ends_with
                    ~suffix:
                      "addressed floating compound result requires its \
                       original native address mode and register supplier"
                    error.message)
             errors);
        Alcotest.(check string) "rejected update has no output" "" report.output
  in
  let cases =
    List.map
      (fun operator ->
        "F64 value=1.5;F64 Read(F64 *p){return (*p" ^ operator
        ^ "2.0);}Read(&value);")
      [ "+="; "-="; "*="; "/=" ]
    @ [
        "F64 value=1.5;F64 Read(){return (*(&value)+=0.5);}Read();";
        "F64 values[2]={1.5,2.5};F64 Read(){return \
         (*((values+1)-1)+=0.5);}Read();";
        "F64 values[2]={1.5,2.5};F64 Read(I64 i){return \
         values[i]*=2.0;}Read(1);";
        "I16 values[2]={1,2};I64 Read(I64 i){return values[i]+=2.5;}Read(1);";
      ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun source -> run ~target ~mode source |> expect_rejection)
        cases);
  (* KernelA.HH:1555 defines the public option as 0x23. A JIT data-heap
     address has no code-heap low-address guarantee; AOT follows another path. *)
  run ~target:Source_task ~mode:Preprocessor.Jit
    "#exe{Option(35,1);}F64 value;F64 Read(){value=1.5;return \
     value+=2.0;}Read();"
  |> expect_rejection

(* OptPass012:376-398 reassociates only nested multiplication; 559-614 and
   661-688 combine only additive children. Mixed operator pairs keep their
   original extended links. These exact powers of two expose an unwanted store. *)
let literal_operator_boundaries () =
  let cases =
    [
      ( "F64 Read(F64 value){return \
         (value+1.0)*2.0-value;}Read(9007199254740992.0);",
        0x4340_0000_0000_0001L );
      ( "F64 Read(F64 value){return \
         (value*2.0)+1.0-value;}Read(4503599627370496.0);",
        0x4330_0000_0000_0001L );
      ( "F64 Read(F64 value){return \
         1.0+(value*2.0)-value;}Read(4503599627370496.0);",
        0x4330_0000_0000_0001L );
      ( "F64 Read(F64 value){return \
         (value*2.0)-1.0-value;}Read(9007199254740992.0);",
        0x433f_ffff_ffff_ffffL );
      ( "F64 Read(F64 value){return (value/2.0)*2.0;}Read(1.5);",
        0x3ff8_0000_0000_0000L );
      ( "F64 Read(F64 value){return (value%2.0)+1.0;}Read(7.5);",
        0x4004_0000_0000_0000L );
    ]
  in
  let unproved =
    [
      "F64 Read(F64 value){return (value+0.5)+0.5;}Read(1.5);";
      "F64 Read(F64 value){return (value-0.5)-0.5;}Read(1.5);";
      "F64 Read(F64 value){return (value*2.0)*3.0;}Read(1.5);";
      "F64 Read(F64 value){return (1.0*value)*2.0;}Read(1.5);";
    ]
  in
  all_targets (fun target mode ->
      List.iter
        (fun (source, bits) ->
          run ~target ~mode source |> expect Image.F64 bits)
        cases;
      List.iter
        (fun source ->
          let report = run ~target ~mode source in
          match report.outcome with
          | Ok _ ->
              Alcotest.fail (report.context ^ ": unproved fold was admitted")
          | Error errors ->
              Alcotest.(check bool)
                (report.context ^ ": " ^ diagnostics errors)
                true
                (List.exists
                   (fun (error : Diagnostic.t) ->
                     error.code = "HCBACK0002"
                     && String.ends_with
                          ~suffix:
                            "native F64 linked region requires unsupported \
                             source constant normalization"
                          error.message)
                   errors))
        unproved)

let reached_work () =
  let source =
    "extern U0 Print(U8 *format,...);F64 Left(){Print(\"L\");return 7.5;}F64 \
     Right(){Print(\"R\");return 2.0;}" ^ modulo_source ^ "Mod(Left(),Right());"
  in
  all_targets (fun target mode ->
      let baseline = run ~target ~mode source in
      expect ~output:"RL" Image.F64 0x3ff8_0000_0000_0000L baseline;
      run ~target ~mode ~max_steps:baseline.steps source
      |> expect ~output:"RL" Image.F64 0x3ff8_0000_0000_0000L;
      let limited = run ~target ~mode ~max_steps:(baseline.steps - 1) source in
      Alcotest.(check bool)
        "one-below instruction limit fails" true
        (Result.is_error limited.outcome);
      Alcotest.(check string)
        "reached argument output survives" "RL" limited.output)

let opcode_bytes () =
  let bytes values =
    String.init (List.length values) (fun i -> Char.chr (List.nth values i))
  in
  let check name expected instruction =
    Alcotest.(check string) name (bytes expected) (Encoder.encode instruction)
  in
  check "pinned FPREM" [ 0xd9; 0xf8 ] Encoder.Fprem;
  check "pinned FSTSW AX without WAIT" [ 0xdf; 0xe0 ] Encoder.Fstsw_ax;
  let scratch = Encoder.stack_slot ~offset:0 |> checked in
  check "nonpop FST binary64"
    [ 0xdd; 0x94; 0x24; 0; 0; 0; 0 ]
    (Encoder.Fst64 (Encoder.X87_stack scratch));
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-modulo-capability.hc"
      ~contents:(modulo_source ^ "Mod(7.5,2.0);")
  in
  let config = Preprocessor.Config.create () |> checked in
  let image =
    Native_program.compile session ~source ~config
    |> Result.map_error diagnostics
    |> checked
  in
  Alcotest.(check bool)
    "remainder image requires x87" true
    (Image.requires_x87 image.value)

let () =
  if not (Native_execution.x87_supported ()) then
    print_endline "native extended F64 tests: unsupported x87 host, skipped"
  else
    Alcotest.run "Native extended F64 operators"
      [
        ( "operators",
          [
            Alcotest.test_case "pinned instruction bytes and capability" `Quick
              opcode_bytes;
            Alcotest.test_case "remainder values and complete reductions" `Quick
              modulo_values;
            Alcotest.test_case "remainder materialization and storage" `Quick
              modulo_materialization;
            Alcotest.test_case "masked remainder faults" `Quick modulo_invalid;
            Alcotest.test_case "compound builtin option snapshots" `Quick
              compound_builtin_options;
            Alcotest.test_case "compound conversion and store precision" `Quick
              compound_precision;
            Alcotest.test_case "remainder updates and destination conversion"
              `Quick modulo_updates;
            Alcotest.test_case "floating shifts use original words" `Quick
              floating_shifts;
            Alcotest.test_case "compound raw words and integer counts" `Quick
              floating_compound_words;
            Alcotest.test_case
              "negative literal comparison and return boundaries" `Quick
              negative_literal_boundaries;
            Alcotest.test_case
              "observed compounds require source result suppliers" `Quick
              unproved_compound_results;
            Alcotest.test_case "literal folding respects child operators" `Quick
              literal_operator_boundaries;
            Alcotest.test_case "reached effects and exact instruction limits"
              `Quick reached_work;
          ] );
      ]
