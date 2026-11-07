open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Runtime = Native_program_execution

(* Source-derived expectations, not TempleOS runtime captures. The pin is
   c26482bb6ad3f80106d28504ec5db3c6a360732c. OptPass789A:567-595 and
   BackB:164-198 use materialized integer words for equality. Templates:39-85,
   280-322,456-498 select different unordered predicates for value comparisons.
   OptLib:230-245,357-372 and BackFB:226-426 fuse direct conditions before
   selecting their predicate. None of the answers below use host floats. *)
let require condition message = if not condition then failwith message

let checked = function
  | Ok x -> x
  | Error e -> failwith e

let diagnostic_text ds =
  String.concat "; "
    (List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) ds)

let read path =
  let ch = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () -> really_input_string ch (in_channel_length ch))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-compare-f64-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let ch = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out ch)
        (fun () -> output_string ch contents);
      action path)

let compiler = Sys.argv.(1)
let native = Array.exists (( = ) "--native") Sys.argv
let modes = [ "jit"; "aot" ]
let targets = [ "host-jit"; "host-jit-task" ]
let count = ref 0

let invoke ?(status = 0) ?(options = []) ~target ~mode label source =
  incr count;
  temporary ".hc" source (fun path ->
      temporary ".out" "" (fun output ->
          temporary ".err" "" (fun errors ->
              let out =
                Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err =
                Unix.openfile errors [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                Array.of_list
                  ([
                     compiler;
                     "run";
                     "--format=json";
                     "--target=" ^ target;
                     "--mode=" ^ mode;
                     "--code-byte-limit=1048576";
                   ]
                  @ options @ [ path ])
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out;
                    Unix.close err)
                  (fun () ->
                    Unix.create_process compiler args Unix.stdin out err)
              in
              let _, actual = Unix.waitpid [] pid in
              let text = read output and errors = read errors in
              require
                (actual = Unix.WEXITED status && errors = "")
                (label ^ " " ^ target ^ " " ^ mode ^ ": " ^ text ^ "; stderr="
               ^ errors);
              Yojson.Safe.from_string text)))

let successful label bits report =
  let word = member "final_value" report in
  require
    (member "outcome" report = `String "success"
    && member "type" word = `String "i64"
    && member "bits" word = `String (Printf.sprintf "0x%016Lx" bits))
    (label ^ ": expected " ^ Int64.to_string bits ^ "; actual "
    ^ Yojson.Safe.to_string report)

let rejected label report =
  require
    (member "outcome" report = `String "error"
    && report |> member "diagnostics" |> to_list
       |> List.exists (fun d -> member "code" d <> `Null))
    (label ^ ": unsupported source was admitted")

let operators = [ "<"; ">="; ">"; "<="; "=="; "!=" ]
let ordered = [ "<"; ">="; ">"; "<=" ]
let f body args = "I64 F(F64 a,F64 b){" ^ body ^ "}F(" ^ args ^ ");"
let expression expr args = f ("return " ^ expr ^ ";") args
let nan = "F64 N(F64 a,F64 b){return a/b;}"

let scalar_cases =
  List.concat_map
    (fun (label, prefix, args, answers) ->
      List.map2
        (fun op answer ->
          ( label ^ " " ^ op,
            prefix ^ expression ("a" ^ op ^ "b") args,
            Int64.of_int answer ))
        operators answers)
    [
      ("finite less", "", "2.0,3.0", [ 1; 0; 0; 1; 0; 1 ]);
      ("finite greater", "", "3.0,2.0", [ 0; 1; 1; 0; 0; 1 ]);
      ("equal finite", "", "2.0,2.0", [ 0; 1; 0; 1; 1; 0 ]);
      ("signed zeros", "", "0.0,-0.0", [ 0; 1; 0; 1; 0; 1 ]);
      ("infinity", nan, "N(1.0,0.0),2.0", [ 0; 1; 1; 0; 0; 1 ]);
      ("NaN left", nan, "N(0.0,0.0),1.0", [ 1; 0; 1; 0; 0; 1 ]);
      ("NaN right", nan, "1.0,N(0.0,0.0)", [ 1; 0; 1; 0; 0; 1 ]);
      ("same NaN bits", nan, "N(0.0,0.0),N(0.0,0.0)", [ 1; 0; 1; 0; 1; 0 ]);
    ]

(* Each expected vector follows the actual FCOMIP operand order and carry/zero
   tests in the pinned templates. The direct branch vector is deliberately not
   copied from the value vector. *)
let context_cases =
  List.concat_map
    (fun (label, left, right, values, conditions) ->
      List.concat
        (List.mapi
           (fun i op ->
             let expr = left ^ op ^ right in
             let value = List.nth values i
             and condition = List.nth conditions i in
             let source body = nan ^ f body "N(0.0,0.0),1.0" in
             let case role body expected =
               ( label ^ " " ^ op ^ " " ^ role,
                 source body,
                 Int64.of_int expected )
             in
             [
               case "value" ("return " ^ expr ^ ";") value;
               case "stored condition"
                 ("I64 t=" ^ expr ^ ";if(t)return 1;return 0;")
                 value;
               case "if" ("if(" ^ expr ^ ")return 1;return 0;") condition;
               case "negated if"
                 ("if(!(" ^ expr ^ "))return 1;return 0;")
                 (1 - condition);
               case "double negated if"
                 ("if(!!(" ^ expr ^ "))return 1;return 0;")
                 condition;
               case "while"
                 ("while(" ^ expr ^ "){return 1;}return 0;")
                 condition;
               case "for" ("for(;" ^ expr ^ ";){return 1;}return 0;") condition;
               case "do while"
                 ("I64 k=0;do{k++;if(k==2)return 2;}while(" ^ expr
                ^ ");return k;")
                 (1 + condition);
             ])
           ordered))
    [
      ("unlinked", "a", "b", [ 1; 0; 1; 0 ], [ 0; 1; 1; 0 ]);
      ("linked left", "(a+b)", "b", [ 0; 1; 1; 0 ], [ 0; 1; 1; 0 ]);
      ("linked right", "b", "(a+b)", [ 1; 0; 0; 1 ], [ 1; 0; 0; 1 ]);
    ]

let precision_cases =
  List.map
    (fun (label, body, expected) ->
      (label, f body "9007199254740992.0,1.0", expected))
    [
      ("left extended comparison", "return (a+b)>a;", 1L);
      ("right extended comparison", "return a<(a+b);", 1L);
      ("two arithmetic children", "return (a+b)<(a+b);", 1L);
      ("equality materializes arithmetic", "return (a+b)==a;", 1L);
      ("stored sum rounds", "F64 t=a+b;return t>a;", 0L);
      ("direct extended comparison", "if((a+b)>a)return 1;return 0;", 1L);
      ( "repeated linked left cleanup",
        "I64 k=0;while(k<32){if((a+b)>a)k++;else return -1;}return k;",
        32L );
      ( "repeated linked right cleanup",
        "I64 k=0;while(k<32){if(a<(a+b))k++;else return -1;}return k;",
        32L );
      ( "repeated materialized value cleanup",
        "I64 k=0;I64 t=0;while(k<32){t=(a+b)>a;if(t)k++;else return -1;}return \
         k;",
        32L );
      ("value comparison literal zero", "return b>0.0;", 1L);
      ("direct comparison literal one", "if(b>=1.0)return 1;return 0;", 1L);
    ]
  @ [
      ( "runtime integer left",
        "I64 F(I64 n,F64 x){return n>x;}F(9007199254740993,9007199254740992.0);",
        1L );
      ( "runtime integer right",
        "I64 F(I64 n,F64 x){return x<n;}F(9007199254740993,9007199254740992.0);",
        1L );
      ( "literal integer rounds",
        "I64 F(F64 x){return 9007199254740993>x;}F(9007199254740992.0);",
        0L );
      ( "mixed equality materializes",
        "I64 F(I64 n,F64 x){return \
         n==x;}F(9007199254740993,9007199254740992.0);",
        1L );
      ( "unreachable comparison is validated",
        "I64 F(F64 x){return x<1.0;}42;",
        42L );
    ]

let register_cases =
  List.map
    (fun qualifier ->
      ( "F64 placement " ^ qualifier,
        "I64 F(F64 " ^ qualifier
        ^ " a,F64 b){return (a+b)>a;}F(9007199254740992.0,1.0);",
        1L ))
    [ ""; "noreg"; "reg"; "reg RSI"; "reg R10"; "reg R12" ]
  @ [
      ( "mixed explicit narrow word",
        "I64 F(F64 reg R10 a,F64 noreg b){I8 reg R12 x=255;return \
         (a<b)+x;}F(1.0,2.0);",
        256L );
      ( "mixed noreg narrow store",
        "I64 F(F64 reg R10 a,F64 noreg b){I8 noreg x=255;return \
         (a<b)+x;}F(1.0,2.0);",
        0L );
      ( "forced F64 pressure spills narrow integer",
        "I64 F(F64 reg f0,F64 reg f1,F64 reg f2,F64 reg f3,F64 reg f4,F64 reg \
         f5,F64 reg f6,F64 reg f7,F64 reg f8,I8 x){if(f0<f1)x++;return \
         x;}F(1.0,2.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,127);",
        -128L );
    ]

let mixed_nan_cases =
  List.concat_map
    (fun (label, parameters, expr, args, answers) ->
      List.concat
        (List.mapi
           (fun i op ->
             let comparison = expr op and expected = List.nth answers i in
             let source body =
               nan ^ "I64 F(" ^ parameters ^ "){" ^ body ^ "}F(" ^ args ^ ");"
             in
             [
               ( label ^ op ^ " value",
                 source ("return " ^ comparison ^ ";"),
                 Int64.of_int expected );
               ( label ^ op ^ " if",
                 source ("if(" ^ comparison ^ ")return 1;return 0;"),
                 Int64.of_int expected );
               ( label ^ op ^ " do",
                 source
                   ("I64 k=0;do{k++;if(k==2)return 2;}while(" ^ comparison
                  ^ ");return k;"),
                 Int64.of_int (expected + 1) );
             ])
           ordered))
    [
      ( "runtime integer left NaN",
        "I64 n,F64 x",
        (fun op -> "n" ^ op ^ "x"),
        "1,N(0.0,0.0)",
        [ 0; 1; 1; 0 ] );
      ( "runtime integer right NaN",
        "F64 x,I64 n",
        (fun op -> "x" ^ op ^ "n"),
        "N(0.0,0.0),1",
        [ 1; 0; 0; 1 ] );
    ]

let equality_condition_cases =
  List.concat_map
    (fun (label, prefix, args, equal) ->
      List.concat_map
        (fun (op, expected) ->
          let expr = "a" ^ op ^ "b" in
          List.map
            (fun (role, body, result) ->
              (label ^ op ^ role, prefix ^ f body args, Int64.of_int result))
            [
              ("direct if", "if(" ^ expr ^ ")return 1;return 0;", expected);
              ( "negated if",
                "if(!(" ^ expr ^ "))return 1;return 0;",
                1 - expected );
              ( "do",
                "I64 k=0;do{k++;if(k==2)return 2;}while(" ^ expr ^ ");return k;",
                1 + expected );
            ])
        [ ("==", equal); ("!=", 1 - equal) ])
    [
      ("signed zero equality", "", "0.0,-0.0", 0);
      ("same NaN equality", nan, "N(0.0,0.0),N(0.0,0.0)", 1);
    ]
  @ [
      ( "direct equality materializes sum",
        f "if((a+b)==a)return 1;return 0;" "9007199254740992.0,1.0",
        1L );
      ( "ordinary arithmetic consumes comparison value",
        nan ^ f "return (a<b)+0;" "N(0.0,0.0),1.0",
        1L );
      ( "stored equality materializes sum",
        f "I64 t=(a+b)==a;if(t)return 1;return 0;" "9007199254740992.0,1.0",
        1L );
    ]

(* OptLib:141-160 changes original < and >= against a direct positive-zero
   immediate and PUBLIC F64 frame dereference to signed integer word compares.
   x87 0/0 supplies the architectural negative indefinite NaN here; its sign is
   part of the source-derived fixture, not inferred from this backend's output. *)
let signed_zero_normalization_cases =
  List.concat_map
    (fun (label, prefix, argument) ->
      List.concat_map
        (fun (expr, expected) ->
          List.map
            (fun (role, body, result) ->
              ( label ^ " " ^ expr ^ " " ^ role,
                prefix ^ f body (argument ^ ",1.0"),
                Int64.of_int result ))
            [
              ("value", "return " ^ expr ^ ";", expected);
              ("if", "if(" ^ expr ^ ")return 1;return 0;", expected);
              ( "do",
                "I64 k=0;do{k++;if(k==2)return 2;}while(" ^ expr ^ ");return k;",
                1 + expected );
            ])
        [
          ("a<0.0", 1);
          ("a>=0.0", 0);
          ("0.0<a", 0);
          ("0.0>=a", 1);
          ("a<0", 1);
          ("a>=0", 0);
          ("0<a", 0);
          ("0>=a", 1);
        ])
    [
      ("signed raw negative zero", "", "-0.0");
      ("signed raw negative NaN", nan, "N(0.0,0.0)");
    ]
  @ [
      ( "negative-zero divisor preserves sign",
        f "return (a/(-0.0))<b;" "1.0,0.0",
        1L );
      ( "negative-zero divisor direct condition",
        f "if((a/(-0.0))<b)return 1;return 0;" "1.0,0.0",
        1L );
      ( "negative-zero divisor reversed comparison",
        f "return b>(a/(-0.0));" "1.0,0.0",
        1L );
      ( "negative-zero divisor complement",
        f "return (a/(-0.0))>=b;" "1.0,0.0",
        0L );
      ( "runtime negative-zero divisor control",
        "F64 Neg(F64 x){return -x;}I64 F(F64 a,F64 b,F64 c){return \
         (a/b)<c;}F(1.0,Neg(0.0),0.0);",
        1L );
      ( "F64i read stays floating against positive zero",
        "I64 F(F64i a){return a<0.0;}F(-0.0);",
        0L );
      ( "F64i greater equal stays floating",
        "I64 F(F64i a){if(a>=0.0)return 1;return 0;}F(-0.0);",
        1L );
      ( "greater stays floating against positive zero",
        expression "a>0.0" "-0.0,1.0",
        0L );
      ( "less equal stays floating against positive zero",
        expression "a<=0.0" "-0.0,1.0",
        1L );
      ( "greater NaN stays floating against positive zero",
        nan ^ expression "a>0.0" "N(0.0,0.0),1.0",
        1L );
      ( "less equal NaN stays floating against positive zero",
        nan ^ expression "a<=0.0" "N(0.0,0.0),1.0",
        0L );
      ( "negative zero literal stays floating",
        expression "a<(-0.0)" "-0.0,1.0",
        0L );
      ( "negative zero literal equal stays floating",
        expression "a>=(-0.0)" "-0.0,1.0",
        1L );
    ]

let example =
  Array.to_list Sys.argv
  |> List.find_opt (fun path -> Filename.check_suffix path ".hc")
  |> Option.value
       ~default:
         (if Sys.file_exists "examples/native-float-comparisons.hc" then
            "examples/native-float-comparisons.hc"
          else "../examples/native-float-comparisons.hc")

let cases =
  scalar_cases @ context_cases @ precision_cases @ register_cases
  @ mixed_nan_cases @ equality_condition_cases @ signed_zero_normalization_cases
  @ [ ("maintained example", read example, 1L) ]

let excluded =
  [
    ( "mixed integer read versus float zero less",
      "I64 F(I64 n){return n<0.0;}F(-1);" );
    ( "mixed integer read versus float zero greater equal",
      "I64 F(I64 n){return n>=0.0;}F(-1);" );
    ( "mixed integer read versus reversed float zero less",
      "I64 F(I64 n){if(0.0<n)return 1;return 0;}F(-1);" );
    ( "mixed integer read versus reversed float zero greater equal",
      "I64 F(I64 n){if(0.0>=n)return 1;return 0;}F(-1);" );
    ("unproved constant comparison", "I64 F(){return 1.0<2.0;}F();");
    ("unproved constant branch", "I64 F(){if(1.0<2.0)return 1;return 0;}F();");
    ( "comparison result converted to F64",
      "F64 F(F64 a,F64 b){return a<b;}F(1.0,2.0);" );
    ( "comparison chain",
      "I64 F(F64 a,F64 b,F64 c){return a<b<c;}F(1.0,2.0,3.0);" );
    ("short circuit", "I64 F(F64 a,F64 b){return a<b&&b>a;}F(1.0,2.0);");
    ("floating global", "F64 G=1.0;I64 F(F64 x){return x<G;}F(0.0);");
    ("compound update", "I64 F(F64 a,F64 b){a+=b;return a>b;}F(1.0,2.0);");
    ( "floating callback",
      "I64 F(F64 a,F64 b){return a<b;}I64 (*p)(F64 a,F64 b)=&F;p(1.0,2.0);" );
    ("suffix bitcast", "I64 F(F64 a){return a==0x7ff8000000000042(F64);}F(1.0);");
    ( "unproved condition add-zero wrapper",
      "I64 F(F64 a,F64 b){if((a<b)+0)return 1;return 0;}F(1.0,2.0);" );
    ( "unproved condition multiply-one wrapper",
      "I64 F(F64 a,F64 b){if((a<b)*1)return 1;return 0;}F(1.0,2.0);" );
    ( "unproved condition divide-one wrapper",
      "I64 F(F64 a,F64 b){if((a<b)/1)return 1;return 0;}F(1.0,2.0);" );
    ( "unproved condition subtract-zero wrapper",
      "I64 F(F64 a,F64 b){if((a<b)-0)return 1;return 0;}F(1.0,2.0);" );
    ( "unproved negated condition wrapper",
      "I64 F(F64 a,F64 b){if(!((a<b)+0))return 1;return 0;}F(1.0,2.0);" );
    ( "unproved negated arithmetic condition",
      "I64 F(F64 a,F64 b){if((!(a<b))+0)return 1;return 0;}F(1.0,2.0);" );
    ( "unproved normalized dereference zero",
      "I64 F(F64 a){return (a+0.0)<0.0;}F(-0.0);" );
    ("unproved computed zero", "I64 F(F64 a){return a<(1.0-1.0);}F(-0.0);");
    ( "unproved branch PI builtin",
      "I64 F(F64 a){if(a<3.141592653589793)return 1;return 0;}F(1.0);" );
  ]

let compile ?max_ir_instructions ?max_code_bytes mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-float-comparison.hc" ~contents
  in
  let config =
    Preprocessor.Config.create
      ~compilation_mode:
        (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
      ()
    |> checked
  in
  Native_program.compile ?max_ir_instructions ?max_code_bytes session ~config
    ~source

let image mode source =
  (compile mode source |> Result.map_error diagnostic_text |> checked).value

let capabilities mode =
  List.iter
    (fun (source, x87) ->
      let compiled = image mode source in
      require
        (Image.requires_x87 compiled = x87)
        "comparison capability follows emitted instructions";
      require
        (not (Image.requires_fisttp compiled))
        "I64 comparison needs no FISTTP")
    [
      (expression "a==b" "1.0,2.0", false);
      (expression "a!=b" "1.0,2.0", false);
      (expression "a<0.0" "1.0,2.0", false);
      (expression "0>=a" "1.0,2.0", false);
      (f "if(a<0)return 1;return 0;" "1.0,2.0", false);
      ("I64 F(F64i a){return a<0.0;}F(1.0);", true);
      ("I64 F(I64 n,F64 a){return n==a;}F(1,1.0);", true);
    ]

let completed expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      let word = Option.get result.final_value in
      require
        (word.type_ = Image.I64 && word.bits = expected)
        "retained comparison value/type";
      result.executed_steps
  | Image.Fault _ -> failwith "unexpected comparison fault"

let fault steps report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      require
        (result.kind = Image.Step_limit_exceeded
        && result.executed_steps = steps
        && Option.is_some result.span)
        "comparison quota kind, site or charge";
      result
  | Image.Completed _ -> failwith "missing comparison quota fault"

let retained mode =
  List.iter
    (fun body ->
      let source = f body "9007199254740992.0,1.0" in
      let compiled = image mode source in
      require (Image.requires_x87 compiled) "ordered comparison x87 capability";
      require
        (not (Image.requires_fisttp compiled))
        "ordered comparison needs no FISTTP";
      let ir = Image.ir_instructions compiled
      and code = Image.code_bytes compiled in
      ignore
        (compile ~max_ir_instructions:ir ~max_code_bytes:code mode source
        |> Result.map_error diagnostic_text
        |> checked);
      require
        (Result.is_error (compile ~max_ir_instructions:(ir - 1) mode source))
        "IR quota";
      require
        (Result.is_error (compile ~max_code_bytes:(code - 1) mode source))
        "code quota";
      let handle = Runtime.retain compiled |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release handle |> checked)
        (fun () ->
          let run steps =
            Runtime.execute_retained_report ~max_steps:steps handle
          in
          let steps = run 100000 |> completed 1L in
          (* Every earlier charge is exercised, including the linked producer,
           comparison, hosted branch and return. Each fault is followed by a
           full run on the same image to check fault unwind and repeatability.
           The separate 32-iteration cases exercise x87 cleanup within an entry. *)
          for quota = 1 to steps - 1 do
            ignore (run quota |> fault quota);
            ignore (run steps |> completed 1L)
          done;
          for _ = 1 to 12 do
            ignore (run steps |> completed 1L)
          done))
    [ "return (a+b)>a;"; "if((a+b)>a)return 1;return 0;" ]

let quota mode target =
  let source = f "if((a+b)>a)return 1;return 0;" "9007199254740992.0,1.0" in
  let baseline = invoke ~target ~mode "quota baseline" source in
  successful "quota baseline" 1L baseline;
  let steps = member "executed_steps" baseline |> to_int in
  invoke ~target ~mode
    ~options:[ "--step-limit=" ^ string_of_int steps ]
    "exact quota" source
  |> successful "exact quota" 1L;
  let report =
    invoke ~status:1 ~target ~mode
      ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
      "one below quota" source
  in
  rejected "one below quota" report;
  require (member "executed_steps" report = `Int (steps - 1)) "CLI quota charge"

let failures = ref []

let check label action =
  try action ()
  with exn -> failures := (label ^ ": " ^ Printexc.to_string exn) :: !failures

let () =
  List.iter
    (fun mode ->
      check (mode ^ " comparison capabilities") (fun () -> capabilities mode);
      List.iter
        (fun (label, source, _) ->
          check
            (mode ^ " compile " ^ label)
            (fun () -> ignore (image mode source)))
        cases;
      check (mode ^ " IR exclusion") (fun () ->
          invoke ~status:1 ~target:"ir" ~mode "IR exclusion"
            (expression "a<b" "1.0,2.0")
          |> rejected "IR exclusion");
      if native && Native_execution.x87_supported () then
        check (mode ^ " retained recovery") (fun () -> retained mode);
      List.iter
        (fun target ->
          List.iter
            (fun (label, source) ->
              check
                (mode ^ target ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode label source |> rejected label))
            excluded;
          if native && Native_execution.x87_supported () then (
            List.iter
              (fun (label, source, expected) ->
                check
                  (mode ^ target ^ label)
                  (fun () ->
                    invoke ~target ~mode label source
                    |> successful label expected))
              cases;
            check (mode ^ target ^ " quotas") (fun () -> quota mode target)))
        targets)
    modes;
  Printf.printf "native F64 comparisons: %d CLI reports, %d failures\n" !count
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
