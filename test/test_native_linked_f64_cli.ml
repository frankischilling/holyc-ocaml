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
  let path = Filename.temp_file "holyc-linked-f64-" suffix in
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
    && member "type" word = `String "f64"
    && member "bits" word = `String (Printf.sprintf "0x%016Lx" bits)
    &&
    match member "value" word with
    | `String _ -> true
    | _ -> false)
    (label ^ ": expected "
    ^ Printf.sprintf "0x%016Lx" bits
    ^ "; actual " ^ Yojson.Safe.to_string word)

let rejected label report =
  require
    (member "outcome" report = `String "error"
    && report |> member "diagnostics" |> to_list
       |> List.exists (fun d ->
           match member "code" d with
           | `String s -> String.length s > 0
           | _ -> false))
    (label ^ ": " ^ Yojson.Safe.to_string report)

let large = "9007199254740992.0,1.0"
let function_source body = "F64 F(F64 a,F64 b){" ^ body ^ "}F(" ^ large ^ ");"
let expression expr = function_source ("return " ^ expr ^ ";")

let precision_cases =
  [
    ("left link", expression "(a+b)-a", 0x3ff0000000000000L);
    ("right reversed link", expression "a-(a+b)", 0xbff0000000000000L);
    ("two children asymmetric", expression "(a+b)-(a+b)", 0xbff0000000000000L);
    ("store barrier", function_source "F64 t=a+b;return t-a;", 0L);
    ("unary barrier", expression "-(a+b)+a", 0L);
    ( "right one normalized away",
      expression "((a+b)*1.0)-a",
      0x3ff0000000000000L );
    ("left one copy barrier", expression "(1.0*(a+b))-a", 0L);
    ("divide one copy barrier", expression "((a+b)/1.0)-a", 0L);
    ( "right zero normalized away",
      expression "((a+b)+0.0)-a",
      0x3ff0000000000000L );
    ( "left zero normalized away",
      expression "(0.0+(a+b))-a",
      0x3ff0000000000000L );
    ( "subtract zero normalized away",
      expression "((a+b)-0.0)-a",
      0x3ff0000000000000L );
    ( "multiply divide chain",
      expression "(((a+b)*2.0)/2.0)-a",
      0x3ff0000000000000L );
    ("division precision", expression "(a+b)/a-1.0", 0x3ca0000000000000L);
    ( "reversed division precision",
      expression "a/(a+b)-1.0",
      0xbca0000000000000L );
    ( "argument boundary",
      "F64 Id(F64 x){return x;}" ^ expression "Id(a+b)-a",
      0L );
    ( "return boundary",
      "F64 Add(F64 x,F64 y){return x+y;}" ^ expression "Add(a,b)-a",
      0L );
    ( "assignment then linked region",
      function_source "F64 t=a+b;return (t+b)-a;",
      0x3ff0000000000000L );
  ]

let normalized_sibling_cases =
  List.map
    (fun expr ->
      ( "normalized sibling " ^ expr,
        "F64 F(F64 a,F64 b,F64 c){return " ^ expr
        ^ ";}F(9007199254740992.0,1.0,0.0);",
        0x3ff0000000000000L ))
    [ "((a+b)+(c+0.0))-a"; "((a+b)+(c*1.0))-a"; "((a+b)+(0.0+c))-a" ]

let orientation_cases =
  List.map
    (fun (label, expr, bits) ->
      ( label,
        "F64 F(F64 a,F64 b,F64 c){return " ^ expr ^ ";}F(8.0,2.0,20.0);",
        bits ))
    [
      ("left add", "(a+b)+c", 0x403e000000000000L);
      ("right add", "c+(a+b)", 0x403e000000000000L);
      ("left subtract", "(a+b)-c", 0xc024000000000000L);
      ("right subtract", "c-(a+b)", 0x4024000000000000L);
      ("left multiply", "(a+b)*c", 0x4069000000000000L);
      ("right multiply", "c*(a+b)", 0x4069000000000000L);
      ("left divide", "(a+b)/c", 0x3fe0000000000000L);
      ("right divide", "c/(a+b)", 0x4000000000000000L);
      ("inner subtract", "c/(a-b)", 0x400aaaaaaaaaaaabL);
      ("inner multiply", "c-(a*b)", 0x4010000000000000L);
      ("inner divide", "c-(a/b)", 0x4030000000000000L);
    ]

let pressure_cases =
  let rec balanced depth =
    if depth = 0 then "x"
    else "(" ^ balanced (depth - 1) ^ "+" ^ balanced (depth - 1) ^ ")"
  in
  let rec chain left n =
    if n = 0 then "x"
    else if left then "(" ^ chain left (n - 1) ^ "+x)"
    else "(x+" ^ chain left (n - 1) ^ ")"
  in
  let make label expr =
    (label, "F64 F(F64 x){return " ^ expr ^ ";}F(1.0);", 0x4050000000000000L)
  in
  [
    make "64-leaf balanced" (balanced 6);
    make "64-leaf left chain" (chain true 63);
    make "64-leaf right chain" (chain false 63);
    ( "integer loop regions",
      "F64 F(I64 n,F64 a,F64 b){F64 t=0.0;while(n){t=(a+b)-a;n--;}return \
       t;}F(3," ^ large ^ ");",
      0x3ff0000000000000L );
    ( "integer branch regions",
      "F64 F(I64 n,F64 a,F64 b){if(n)return (a+b)-a;return a-(a+b);}F(0,"
      ^ large ^ ");",
      0xbff0000000000000L );
    ( "recursion region boundary",
      "F64 F(I64 n,F64 a,F64 b){if(n)return F(n-1,a,b);return (a+b)-a;}F(3,"
      ^ large ^ ");",
      0x3ff0000000000000L );
  ]

let cases =
  precision_cases @ normalized_sibling_cases @ orientation_cases
  @ pressure_cases

let excluded =
  [
    (* Pinned normalization would produce 1.0, but this phase has no
       authenticated literal-reassociation proof. Rejection is intentional. *)
    ("literal reassociation", expression "((a+0.5)+0.5)-a");
    (* Both constant-only subtrees normalize to 1.0 in the reference,
       giving 2.0 overall. This phase must reject without folding proof,
       never accept the incorrect 1.0 obtained by losing the linked value. *)
    ("constant division identity", expression "((a+b)+(1.0/1.0))-a");
    ("constant multiplication identity", expression "((a+b)+(1.0*1.0))-a");
    ("linked comparison", function_source "return (a+b)<a;");
    ("linked remainder", expression "(a+b)%a");
    ("linked update", function_source "a+=(a+b);return a;");
  ]

let config mode =
  Preprocessor.Config.create
    ~compilation_mode:
      (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
    ()
  |> checked

let compile ?max_ir_instructions ?max_code_bytes mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-linked-f64.hc" ~contents
  in
  Native_program.compile ?max_ir_instructions ?max_code_bytes session
    ~config:(config mode) ~source

let image mode source =
  compile mode source |> Result.map_error diagnostic_text |> checked |> fun x ->
  x.value

let completed expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      let word = Option.get result.final_value in
      require
        (word.type_ = Image.F64 && word.bits = expected)
        "retained F64 payload/type";
      result.executed_steps
  | Image.Fault _ -> failwith "unexpected retained F64 fault"

let fault expected report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      require (result.kind = expected) "wrong retained F64 fault";
      result
  | Image.Completed _ ->
      failwith
        (match expected with
        | Image.Step_limit_exceeded -> "expected retained F64 step fault"
        | Image.Frame_limit_exceeded -> "expected retained F64 frame fault"
        | _ -> "expected retained F64 fault")

let retained mode =
  let source = expression "(a+b)-a" in
  let compiled = image mode source in
  require (Image.requires_x87 compiled) "linked image lost x87 capability";
  let ir = Image.ir_instructions compiled
  and code = Image.code_bytes compiled in
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:code mode source
    |> Result.map_error diagnostic_text
    |> checked);
  List.iter
    (fun result ->
      require (Result.is_error result) "one-below compile quota admitted")
    [
      compile ~max_ir_instructions:(ir - 1) mode source;
      compile ~max_code_bytes:(code - 1) mode source;
    ];
  let retained = Runtime.retain compiled |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () ->
      let run ?max_frame_bytes n =
        Runtime.execute_retained_report ?max_frame_bytes ~max_steps:n retained
      in
      let steps = run 100000 |> completed 0x3ff0000000000000L in
      for _ = 1 to 3 do
        ignore (run steps |> completed 0x3ff0000000000000L)
      done;
      let failed = run (steps - 1) |> fault Image.Step_limit_exceeded in
      require
        (failed.executed_steps = steps - 1 && Option.is_some failed.span)
        "linked quota source site/charge";
      ignore (run steps |> completed 0x3ff0000000000000L);
      (* Two ordinary eight-byte source parameters, independently of machine spills. *)
      ignore (run ~max_frame_bytes:16 steps |> completed 0x3ff0000000000000L);
      ignore (run ~max_frame_bytes:15 steps |> fault Image.Frame_limit_exceeded);
      ignore (run steps |> completed 0x3ff0000000000000L));
  (* Same names/path and expression root shape in independent source owners must
     not reuse a previous source's constant bits or normalized producer plan. *)
  let other =
    image mode "F64 F(F64 a,F64 b){return (a+b)-a;}F(9007199254740992.0,2.0);"
  in
  ignore
    (Runtime.execute_report ~max_steps:100000 other
    |> completed 0x4000000000000000L);
  ignore
    (Runtime.execute_report ~max_steps:100000 compiled
    |> completed 0x3ff0000000000000L)

let arithmetic_fault_recovery mode =
  List.iter
    (fun body ->
      let source = "I64 G=0;" ^ function_source body in
      let retained = image mode source |> Runtime.retain |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked)
        (fun () ->
          let run () =
            Runtime.execute_retained_report ~max_steps:100000 retained
          in
          let failed = run () |> fault Image.Division_by_zero in
          require
            (failed.executed_steps > 0 && Option.is_some failed.span)
            "integer fault around linked region lost source site";
          ignore (run () |> completed 0x3ff0000000000000L);
          ignore (run () |> completed 0x3ff0000000000000L)))
    [
      "G++;I64 z=G-1;I64 q=1/z;return (a+b)-a;";
      "F64 t=(a+b)-a;G++;I64 z=G-1;I64 q=1/z;return t;";
    ]

let quota mode target =
  let source = expression "(a+b)-a" in
  let baseline = invoke ~target ~mode "linked baseline" source in
  successful "linked baseline" 0x3ff0000000000000L baseline;
  let steps = baseline |> member "executed_steps" |> to_int in
  invoke ~target ~mode
    ~options:[ "--step-limit=" ^ string_of_int steps ]
    "exact steps" source
  |> successful "exact steps" 0x3ff0000000000000L;
  let failed =
    invoke ~status:1 ~target ~mode
      ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
      "one-below steps" source
  in
  rejected "one-below steps" failed;
  require
    (member "executed_steps" failed = `Int (steps - 1))
    "linked CLI exact fault charge"

let failures = ref []

let check label action =
  try action ()
  with error ->
    failures := (label ^ ": " ^ Printexc.to_string error) :: !failures

let () =
  List.iter
    (fun mode ->
      check (mode ^ " IR boundary") (fun () ->
          invoke ~status:1 ~target:"ir" ~mode "IR unsupported"
            (expression "(a+b)-a")
          |> rejected "IR unsupported");
      List.iter
        (fun target ->
          List.iter
            (fun (label, source) ->
              check
                (mode ^ " " ^ target ^ " " ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode label source |> rejected label))
            excluded)
        targets;
      if native then
        if Native_execution.x87_supported () then (
          List.iter
            (fun target ->
              List.iter
                (fun (label, source, bits) ->
                  check
                    (mode ^ " " ^ target ^ " " ^ label)
                    (fun () ->
                      let report = invoke ~target ~mode label source in
                      successful label bits report;
                      if target = "host-jit-task" then
                        require
                          (report |> member "native" |> member "fragments"
                         |> to_list
                          |> List.exists (fun f ->
                              member "outcome" f = `String "success"))
                          "linked result bypassed normal task publication"))
                cases;
              check
                (mode ^ " " ^ target ^ " quota")
                (fun () -> quota mode target))
            targets;
          check (mode ^ " retained") (fun () -> retained mode);
          check (mode ^ " integer faults") (fun () ->
              arithmetic_fault_recovery mode))
        else
          List.iter
            (fun target ->
              check
                (mode ^ " " ^ target ^ " unavailable")
                (fun () ->
                  invoke ~status:1 ~target ~mode "x87 unavailable"
                    (expression "(a+b)-a")
                  |> rejected "x87 unavailable"))
            targets)
    modes;
  Printf.printf "native linked F64: %d CLI reports (%s), %d failures\n" !count
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
