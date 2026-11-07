open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Runtime = Native_program_execution

(* Independent source deductions, NOT a TempleOS runtime capture/oracle.
   Reference c26482bb: PrsLib:257-270 runs OptPass012 twice; OptPass012
   normalizes right *1 and +/-0 before OptLib:535-554 selects a producer.
   CInit:52,65-72 excludes unary minus from fpop. BackFA:6-59 authenticates
   the final template event/orientation; BackFB:488-594 adds identity-copy
   barriers. Two arithmetic children materialize the left and link the right.
   All numeric expected bits below are fixed before backend execution.
   Deep source trees stress materialized homes, not an eight-deep x87 tree. *)
let require condition message = if not condition then failwith message

let read path =
  let ch = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () -> really_input_string ch (in_channel_length ch))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-numeric-f64-" suffix in
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

(* All expectations are deductions from the pinned compiler, not fresh TempleOS
   captures. Runtime FILD is signed; FISTTP truncates and returns integer
   indefinite for masked invalid operations. Scientific literals below avoid
   Lex.HC's signed-accumulator wrapping across long decimal mantissas. *)
let normalized_cases =
  List.concat_map
    (fun (expression, deleted) ->
      List.map
        (fun (argument, raw_bits, integer_bits) ->
          ( "normalized " ^ expression ^ " " ^ argument,
            "I64 F(I64 n){return " ^ expression ^ ";}F(" ^ argument ^ ");",
            "i64",
            if deleted then raw_bits else integer_bits ))
        [
          ("9007199254740993", 0x4340000000000000L, 9007199254740992L);
          ("-9007199254740993", 0xc340000000000000L, -9007199254740992L);
          ("-3", 0xc008000000000000L, -3L);
          ("0x7fffffffffffffff", 0x43e0000000000000L, Int64.min_int);
          ("0x8000000000000000", 0xc3e0000000000000L, Int64.min_int);
          ("0", 0L, 0L);
        ])
    [
      ("n*1.0", true);
      ("n+0.0", true);
      ("0.0+n", true);
      ("n-0.0", true);
      ("n/1.0", false);
      ("1.0*n", false);
    ]

let cases =
  [
    ( "runtime left extended",
      "I64 F(I64 n,F64 y){return n+y;}F(9007199254740993,-9007199254740992.0);",
      "i64",
      1L );
    ( "explicit F64 register mixed conversion",
      "F64 F(F64 reg R10 x,I64 y){return \
       x+y;}F(-9007199254740992.0,9007199254740993);",
      "f64",
      0x3ff0000000000000L );
    ( "explicit F64 register integer return",
      "I64 F(F64 reg R10 x){return x;}F(3.75);",
      "i64",
      3L );
    ( "runtime right reversed",
      "I64 F(F64 y,I64 n){return y-n;}F(9007199254740992.0,9007199254740993);",
      "i64",
      -1L );
    ( "computed integer extended",
      "I64 F(I64 n,F64 y){return \
       (n+1)+y;}F(9007199254740992,-9007199254740992.0);",
      "i64",
      1L );
    ( "computed right extended",
      "I64 F(I64 n,F64 y){return \
       y-(n+1);}F(9007199254740992,9007199254740992.0);",
      "i64",
      -1L );
    ( "literal rounded",
      "I64 F(F64 y){return 9007199254740993+y;}F(-9007199254740992.0);",
      "i64",
      0L );
    ( "negative literal rounded",
      "I64 F(F64 y){return -9007199254740993+y;}F(9007199254740992.0);",
      "i64",
      0L );
    ( "floating return boundary",
      "F64 F(I64 n){return n;}F(9007199254740993);",
      "f64",
      0x4340000000000000L );
    ("negative truncation", "I64 F(F64 n){return n;}F(-3.75);", "i64", -3L);
    ("positive truncation", "I64 F(F64 n){return n;}F(3.75);", "i64", 3L);
    ("negative zero", "I64 F(F64 n){return n;}F(-0.0);", "i64", 0L);
    ( "signed runtime unsigned word",
      "F64 F(U64 n){return n;}F(0xffffffffffffffff);",
      "f64",
      0xbff0000000000000L );
    ( "signed minimum runtime word",
      "F64 F(U64 n){return n;}F(0x8000000000000000);",
      "f64",
      0xc3e0000000000000L );
    ( "narrow signed stack",
      "I64 F(F64 n){I8 noreg a=n;return a;}F(255.75);",
      "i64",
      -1L );
    ( "narrow unsigned stack",
      "I64 F(F64 n){U8 noreg a=n;return a;}F(-3.75);",
      "i64",
      253L );
    ( "narrow forced register",
      "I64 F(F64 n){I8 reg R12 a=n;return a;}F(255.75);",
      "i64",
      255L );
    ( "fixed narrow parameter",
      "I64 F(I8 n){return n;}I64 G(F64 n){return F(n);}G(255.75);",
      "i64",
      -1L );
    ( "forced float register",
      "I64 F(F64 reg RSI x){return x;}F(1.5);",
      "i64",
      1L );
    ( "linked floating result conversion",
      "I64 F(F64 a,F64 b){return (a+b)-a;}F(9007199254740992.0,1.0);",
      "i64",
      1L );
    ( "unreachable proved conversion",
      "I64 F(F64 a,F64 b){return (a+b)-a;}42;",
      "i64",
      42L );
    ("integer local", "I64 F(F64 n){I64 a=n;return a;}F(-3.75);", "i64", -3L);
    ( "floating local",
      "F64 F(I64 n){F64 a=n;return a;}F(3);",
      "f64",
      0x4008000000000000L );
    ( "integer assignment",
      "I64 F(F64 n){I64 a=0;a=n;return a;}F(-3.75);",
      "i64",
      -3L );
    ( "floating assignment",
      "F64 F(I64 n){F64 a=0.0;a=n;return a;}F(3);",
      "f64",
      0x4008000000000000L );
    ( "fixed float boundary",
      "F64 F(F64 n){return n;}F64 G(I64 n){return F(n);}G(9007199254740993);",
      "f64",
      0x4340000000000000L );
    ( "fixed integer boundary",
      "I64 F(I64 n){return n;}I64 G(F64 n){return F(n);}G(-3.75);",
      "i64",
      -3L );
    ("mixed multiply", "I64 F(I64 n,F64 y){return n*y;}F(3,2.5);", "i64", 7L);
    ( "mixed divide reversed",
      "I64 F(F64 y,I64 n){return y/n;}F(7.5,2);",
      "i64",
      3L );
    ( "positive overflow indefinite",
      "I64 F(F64 n){return n;}F(9223372036854775807e0);",
      "i64",
      Int64.min_int );
    ( "negative overflow indefinite",
      "F64 Offset(F64 n){return n-2048e0;}I64 F(F64 n){return \
       n;}F(Offset(-9223372036854775807e0));",
      "i64",
      Int64.min_int );
    ( "positive infinity indefinite",
      "I64 F(F64 n){return n;}F(1.0/0.0);",
      "i64",
      Int64.min_int );
    ( "NaN indefinite",
      "I64 F(F64 n){return n;}F(0.0/0.0);",
      "i64",
      Int64.min_int );
    ( "ordinary familiar spelling",
      "F64 ToF64(I64 x){return 7.5;}ToF64(3);",
      "f64",
      0x401e000000000000L );
    ( "explicit ToF64 alias",
      "_intern 28 F64 Convert(I64 x);Convert(3);",
      "f64",
      0x4008000000000000L );
    ( "explicit negative ToI64",
      "_intern 27 I64 Convert(F64 x);Convert(-3.75);",
      "i64",
      -3L );
    ( "explicit ToI64 alias",
      "_intern 27 I64 Convert(F64 x);Convert(3.75);",
      "i64",
      3L );
    ( "explicit arithmetic argument rounds",
      "_intern 27 I64 C(F64 x);I64 F(F64 a,F64 b){return \
       C(a+b);}F(9007199254740992.0,1.0);",
      "i64",
      9007199254740992L );
    ( "implicit arithmetic return retains",
      "I64 F(F64 a,F64 b){return a+b;}F(9007199254740992.0,1.0);",
      "i64",
      9007199254740993L );
    ( "explicit nested arithmetic argument",
      "_intern 27 I64 C(F64 x);I64 F(F64 a,F64 b,F64 c){return \
       C((a+b)+c);}F(9007199254740992.0,1.0,-9007199254740992.0);",
      "i64",
      1L );
    ( "ordinary same spelling arithmetic argument",
      "I64 ToI64(F64 x){return 7;}I64 F(F64 a,F64 b){return \
       ToI64(a+b);}F(9007199254740992.0,1.0);",
      "i64",
      7L );
    ( "explicit materialized arithmetic consumer",
      "_intern 28 F64 C(I64 x);I64 F(I64 n,F64 y){return \
       C(n)+y;}F(9007199254740993,-9007199254740992.0);",
      "i64",
      0L );
    ( "ordinary same spelling arithmetic consumer",
      "F64 ToF64(I64 x){return 7.5;}F64 F(I64 n,F64 y){return \
       ToF64(n)+y;}F(3,1.0);",
      "f64",
      0x4021000000000000L );
    ( "explicit materialized reversed consumer",
      "_intern 28 F64 C(I64 x);I64 F(I64 n,F64 y){return \
       y+C(n);}F(9007199254740993,-9007199254740992.0);",
      "i64",
      0L );
    ( "explicit integer arithmetic consumer",
      "_intern 27 I64 C(F64 x);I64 F(F64 a,I64 n){return C(a)+n;}F(3.75,7);",
      "i64",
      10L );
    ( "explicit float consumer without return conversion",
      "_intern 28 F64 C(I64 x);F64 F(I64 n,F64 y){return \
       C(n)+y;}F(9007199254740993,-9007199254740992.0);",
      "f64",
      0L );
  ]

(* Boundary mantissas use exponent zero so the pinned lexer multiplies by
   exact one. Nonzero exponents depend on its generated x87 Pow10 table;
   a host decimal parser is not an oracle for those literals. *)
let cross_family_normalized_cases =
  List.concat_map
    (fun expression ->
      let to_float =
        List.map
          (fun (argument, bits) ->
            ( "normalized I64 to F64 " ^ expression ^ " " ^ argument,
              "F64 F(I64 n){return " ^ expression ^ ";}F(" ^ argument ^ ");",
              "f64",
              bits ))
          [
            ("9007199254740993", 0x4340000000000000L);
            ("-9007199254740993", 0xc340000000000000L);
            ("0x7fffffffffffffff", 0x43e0000000000000L);
            ("0x8000000000000000", 0xc3e0000000000000L);
            ("-3", 0xc008000000000000L);
            ("0", 0L);
          ]
      and to_integer =
        List.map
          (fun (argument, bits) ->
            ( "normalized F64 to I64 " ^ expression ^ " " ^ argument,
              (if argument = "Offset(-9223372036854775807e0)" then
                 "F64 Offset(F64 n){return n-2048e0;}"
               else "")
              ^ "I64 F(F64 n){return " ^ expression ^ ";}F(" ^ argument ^ ");",
              "i64",
              bits ))
          [
            ("3.75", 3L);
            ("-3.75", -3L);
            ("-0.0", 0L);
            ("9223372036854775807e0", Int64.min_int);
            ("-9223372036854775807e0", Int64.min_int);
            ("9223372036854774784e0", 0x7ffffffffffffc00L);
            ("1.0/0.0", Int64.min_int);
            ("-1e999", Int64.min_int);
            ("0.0/0.0", Int64.min_int);
            ("Offset(-9223372036854775807e0)", Int64.min_int);
          ]
      in
      to_float @ to_integer)
    [ "n*1.0"; "n+0.0"; "0.0+n"; "n-0.0"; "n/1.0"; "1.0*n" ]

let cases = cases @ normalized_cases @ cross_family_normalized_cases

let excluded =
  [
    ( "negative zero add is not normalized positive zero",
      "I64 F(I64 n){return n+(-0.0);}F(9007199254740993);" );
    ( "negative zero subtract is not normalized positive zero",
      "I64 F(I64 n){return n-(-0.0);}F(9007199254740993);" );
    ( "identity in assignment remains unproved",
      "I64 F(I64 n){I64 a=n*1.0;return a;}F(3);" );
    ( "identity in nested arithmetic remains unproved",
      "I64 F(I64 n,F64 y){return (n*1.0)+y;}F(3,2.0);" );
    ( "unsigned identity source remains unproved",
      "I64 F(U64 n){return n*1.0;}F(3);" );
    ( "computed identity source remains unproved",
      "I64 F(I64 n){return (n+2)*1.0;}F(3);" );
    ( "explicit consumer with arithmetic sibling",
      "_intern 28 F64 C(I64 x);F64 F(I64 n,F64 a,F64 b){return \
       C(n)+(a+b);}F(3,1.0,2.0);" );
    ( "opposite family intrinsic arithmetic consumer",
      "_intern 27 I64 C(F64 x);F64 F(F64 a,F64 b){return C(a)+b;}F(3.75,1.0);"
    );
    ( "nested opposite numeric intrinsics",
      "_intern 27 I64 A(F64 x);_intern 28 F64 B(I64 x);I64 F(I64 n){return \
       A(B(n));}F(3);" );
    ( "integer producer multiply alias",
      "I64 F(I64 n,F64 a,F64 b){return \
       (n*1)+(a+b);}F(9007199254740993,-9007199254740992.0,0.0);" );
    ( "integer producer add alias",
      "I64 F(I64 n,F64 a,F64 b){return \
       (n+0)+(a+b);}F(9007199254740993,-9007199254740992.0,0.0);" );
    ( "integer producer subtract alias",
      "I64 F(I64 n,F64 a,F64 b){return \
       (n-0)+(a+b);}F(9007199254740993,-9007199254740992.0,0.0);" );
    ( "integer producer synthesized identity",
      "I64 F(I64 n,F64 a,F64 b){return \
       ((n+1)-1)+(a+b);}F(9007199254740993,-9007199254740992.0,0.0);" );
    ( "bit reinterpretation is not conversion",
      "F64 F(I64 n){return n(F64);}F(3);" );
    ("explicit wrong source family", "_intern 28 F64 C(I64 x);C(3.75);");
    ("unsupported intrinsic opcode", "_intern 999 F64 C(I64 x);C(3);");
    ("wrong intrinsic result", "_intern 28 I64 C(I64 x);C(3);");
    ("wrong intrinsic arity", "_intern 28 F64 C(I64 x,I64 y);C(3,4);");
    ("wrong intrinsic prototype", "_intern 28 F64 C(F64 x);C(3.75);");
  ]

let require_rejected label report =
  require
    (member "outcome" report = `String "error")
    (label ^ ": unsupported conversion was admitted")

let checked = function
  | Ok value -> value
  | Error _ -> failwith "unexpected checked-image rejection"

let compile ?max_ir_instructions ?max_code_bytes mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"numeric-retained.hc" ~contents
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

let normalization_capabilities mode =
  List.iter
    (fun (source, expected) ->
      let image = (compile mode source |> checked).value in
      require (Image.requires_x87 image) "normalized recipe requires x87";
      require
        (Image.requires_fisttp image = expected)
        "normalized physical FISTTP requirement")
    [
      ("I64 F(I64 n){return n*1.0;}F(9007199254740993);", false);
      ("I64 F(I64 n){return n/1.0;}F(9007199254740993);", true);
      ("I64 F(F64 n){return n;}F(3.75);", true);
      ("F64 F(I64 n){return n*1.0;}F(9007199254740993);", false);
      ("F64 F(I64 n){return n/1.0;}F(9007199254740993);", false);
      ("I64 F(F64 n){return n*1.0;}F(3.75);", true);
      ("I64 F(F64 n){return n/1.0;}F(3.75);", true);
    ]

let retained ?(case = List.hd cases) mode =
  let _, source, kind, expected = case in
  let image = (compile mode source |> checked).value in
  require (Image.requires_x87 image) "conversion capability missing";
  let float_image =
    (compile mode "F64 F(I64 n){return n;}F(3);" |> checked).value
  in
  require
    (Image.requires_x87 float_image && not (Image.requires_fisttp float_image))
    "integer-to-float image unnecessarily requires SSE3";
  let ir = Image.ir_instructions image and code = Image.code_bytes image in
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:code mode source |> checked);
  require
    (Result.is_error (compile ~max_ir_instructions:(ir - 1) mode source))
    "IR one-below admitted";
  require
    (Result.is_error (compile ~max_code_bytes:(code - 1) mode source))
    "code one-below admitted";
  if Image.requires_fisttp image && not (Native_execution.fisttp_supported ())
  then (
    require
      (Result.is_error (Runtime.retain image))
      "unsupported retained FISTTP admitted";
    require
      (Result.is_error
         (Runtime.execute_report ~max_steps:100000 image |> Runtime.outcome))
      "unsupported direct FISTTP admitted")
  else
    let handle = Runtime.retain image |> checked in
    Fun.protect
      ~finally:(fun () -> Runtime.release handle |> checked)
      (fun () ->
        let run ?max_frame_bytes steps =
          Runtime.execute_retained_report ?max_frame_bytes ~max_steps:steps
            handle
        in
        let completed report =
          match Runtime.outcome report |> checked with
          | Image.Completed result ->
              let word = Option.get result.final_value in
              require
                ((word.type_ = if kind = "f64" then Image.F64 else Image.I64)
                && word.bits = expected)
                "retained conversion result";
              result.executed_steps
          | _ -> failwith "unexpected retained conversion fault"
        in
        let fault kind report =
          match Runtime.outcome report |> checked with
          | Image.Fault result ->
              require (result.kind = kind) "wrong conversion fault";
              result.executed_steps
          | _ -> failwith "missing conversion fault"
        in
        let steps = run 100000 |> completed in
        ignore (run steps |> completed);
        require
          (run (steps - 1) |> fault Image.Step_limit_exceeded = steps - 1)
          "step quota charge";
        ignore (run steps |> completed);
        let rec minimum_frame low high =
          if low = high then low
          else
            let middle = (low + high) / 2 in
            match
              Runtime.outcome (run ~max_frame_bytes:middle steps) |> checked
            with
            | Image.Completed _ -> minimum_frame low middle
            | Image.Fault f when f.kind = Image.Frame_limit_exceeded ->
                minimum_frame (middle + 1) high
            | _ -> failwith "unexpected frame probe outcome"
        in
        let frame = minimum_frame 0 4096 in
        require (frame > 0 && frame < 4096) "bounded conversion frame";
        ignore (run ~max_frame_bytes:frame steps |> completed);
        ignore
          (run ~max_frame_bytes:(frame - 1) steps
          |> fault Image.Frame_limit_exceeded);
        ignore (run ~max_frame_bytes:frame steps |> completed))

let failures = ref []

let check label action =
  try action ()
  with exn -> failures := (label ^ ": " ^ Printexc.to_string exn) :: !failures

let () =
  List.iter
    (fun mode ->
      check (mode ^ " normalized capability metadata") (fun () ->
          normalization_capabilities mode);
      if native && Native_execution.x87_supported () then
        check (mode ^ " retained conversions and quotas") (fun () ->
            retained mode;
            retained
              ~case:
                (List.find
                   (fun (label, _, _, _) ->
                     label = "explicit nested arithmetic argument")
                   cases)
              mode;
            retained
              ~case:
                (List.find
                   (fun (label, _, _, _) ->
                     label = "explicit materialized arithmetic consumer")
                   cases)
              mode;
            List.iter
              (fun case -> retained ~case mode)
              (List.filter
                 (fun (label, _, _, _) ->
                   label = "normalized n*1.0 9007199254740993"
                   || label = "normalized n/1.0 9007199254740993")
                 normalized_cases);
            List.iter
              (fun case -> retained ~case mode)
              (List.filter
                 (fun (label, _, _, _) ->
                   List.mem label
                     [
                       "normalized I64 to F64 n*1.0 9007199254740993";
                       "normalized I64 to F64 n/1.0 9007199254740993";
                       "normalized F64 to I64 n*1.0 -3.75";
                       "normalized F64 to I64 n/1.0 -3.75";
                     ])
                 cross_family_normalized_cases));
      check (mode ^ " IR exclusion") (fun () ->
          invoke ~status:1 ~target:"ir" ~mode "IR exclusion"
            (let _, s, _, _ = List.hd cases in
             s)
          |> require_rejected "IR exclusion");
      List.iter
        (fun target ->
          List.iter
            (fun (label, source) ->
              check
                (mode ^ target ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode label source
                  |> require_rejected label))
            excluded;
          if native && Native_execution.x87_supported () then
            List.iter
              (fun (label, source, kind, bits) ->
                check
                  (mode ^ target ^ label)
                  (fun () ->
                    let needs_fisttp =
                      Image.requires_fisttp
                        (compile mode source |> checked).value
                    in
                    let unavailable =
                      needs_fisttp && not (Native_execution.fisttp_supported ())
                    in
                    let report =
                      invoke
                        ~status:(if unavailable then 1 else 0)
                        ~target ~mode label source
                    in
                    if unavailable then require_rejected label report
                    else
                      let actual = member "final_value" report in
                      require
                        (member "outcome" report = `String "success"
                        && member "type" actual = `String kind
                        && member "bits" actual
                           = `String (Printf.sprintf "0x%016Lx" bits))
                        (label ^ ": " ^ Yojson.Safe.to_string actual)))
              cases)
        targets)
    modes;
  Printf.printf "native numeric conversions: %d CLI reports, %d failures\n"
    !count (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
