open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Runtime = Native_program_execution

(* Independent expectations: pinned TempleOS c26482bb Compiler/BackFB.HC,
   ICFMul/ICFDiv/ICFAdd/ICFSub and ICFOp, and BackFA.HC floating moves.
   Hexadecimal answers below are exact IEEE binary64 values, not runtime
   captures. In particular the zero-operand shortcuts copy raw words, whereas
   arithmetic follows x87 under FNINIT (CW 0x037f). No precision-link parity is
   claimed for nested floating operations. *)
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
  let path = Filename.temp_file "holyc-native-f64-" suffix in
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

let example_path =
  Array.to_list Sys.argv
  |> List.find_opt (fun path -> Filename.check_suffix path ".hc")
  |> Option.value
       ~default:
         (if Sys.file_exists "examples/native-scalar-f64.hc" then
            "examples/native-scalar-f64.hc"
          else "../examples/native-scalar-f64.hc")

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
    (label ^ ": " ^ Yojson.Safe.to_string report)

let rejected label report =
  require
    (member "outcome" report = `String "error"
    && report |> member "diagnostics" |> to_list
       |> List.exists (fun d ->
           match member "code" d with
           | `String s -> String.length s > 0
           | _ -> false))
    (label ^ ": " ^ Yojson.Safe.to_string report)

let cases =
  [
    ("literal", "1.5;", 0x3ff8000000000000L);
    ( "local copy",
      "F64 Id(F64 x){F64 y=x;return y;}Id(1.5);",
      0x3ff8000000000000L );
    ( "assignment",
      "F64 Id(F64 x){F64 y=0.0;y=x;return y;}Id(1.5);",
      0x3ff8000000000000L );
    ("add", "F64 F(F64 a,F64 b){return a+b;}F(1.5,2.25);", 0x400e000000000000L);
    ( "stored precision boundary",
      "F64 F(F64 a,F64 b){F64 t=a+b;return t-a;}F(9007199254740992.0,1.0);",
      0L );
    ( "subtract",
      "F64 F(F64 a,F64 b){return a-b;}F(2.0,5.0);",
      0xc008000000000000L );
    ( "reverse subtract",
      "F64 F(F64 a,F64 b){return b-a;}F(2.0,5.0);",
      0x4008000000000000L );
    ( "multiply",
      "F64 F(F64 a,F64 b){return a*b;}F(1.5,2.0);",
      0x4008000000000000L );
    ("divide", "F64 F(F64 a,F64 b){return a/b;}F(7.0,2.0);", 0x400c000000000000L);
    ( "reverse divide",
      "F64 F(F64 a,F64 b){return b/a;}F(2.0,7.0);",
      0x400c000000000000L );
    ("negative zero", "F64 Neg(F64 x){return -x;}Neg(0.0);", Int64.min_int);
    ("negative", "F64 Neg(F64 x){return -x;}Neg(1.5);", 0xbff8000000000000L);
    ( "infinity",
      "F64 F(F64 a,F64 b){return a/b;}F(1.0,0.0);",
      0x7ff0000000000000L );
    ( "direct call",
      "F64 Id(F64 x){return x;}F64 F(F64 x){return Id(x);}F(1.5);",
      0x3ff8000000000000L );
    ( "branch",
      "F64 F(I64 n,F64 x){F64 y=2.25;if(n)y=x;return y;}F(1,1.5);",
      0x3ff8000000000000L );
    ( "loop",
      "F64 F(I64 n,F64 x){F64 y=0.0;while(n){y=x;n--;}return y;}F(3,1.5);",
      0x3ff8000000000000L );
    ( "recursion",
      "F64 F(I64 n,F64 x){if(n)return F(n-1,x);return x;}F(3,1.5);",
      0x3ff8000000000000L );
  ]

let cases =
  cases
  @ [
      ( "loop arithmetic",
        "F64 F(I64 n,F64 x){while(n){x=x+1.0;n--;}return x;}F(3,1.5);",
        0x4012000000000000L );
      ( "branch arithmetic",
        "F64 F(I64 n,F64 x){if(n)x=x+2.0;else x=x-2.0;return x;}F(0,1.5);",
        0xbfe0000000000000L );
    ]

let copy_cases =
  List.map
    (fun expr ->
      ( "raw zero copy " ^ expr,
        "F64 Neg(F64 x){return -x;}F64 Copy(F64 x){return " ^ expr
        ^ ";}Copy(Neg(0.0));",
        Int64.min_int ))
    [ "x*1.0"; "1.0*x"; "x/1.0"; "x+0.0"; "0.0+x"; "x-0.0" ]

(* PrsUnaryModifier scales subscripts by the selected class size; ICDeref and
   ICAssign transport RT_F64 with eight-byte ICMov operands. These expectations
   are source-derived, not new TempleOS runtime captures. *)
let storage_cases =
  [
    ( "address without reading an uninitialized pointee",
      "F64 F(){F64 x;F64 *p=&x;*p=1.5;return x;}F();",
      0x3ff8000000000000L );
    ( "pointer parameter",
      "F64 Read(F64 *p){return *p;}F64 F(){F64 x=1.5;return Read(&x);}F();",
      0x3ff8000000000000L );
    ( "array arithmetic",
      "F64 F(){F64 x[2];x[0]=1.5;x[1]=2.25;return x[0]+x[1];}F();",
      0x400e000000000000L );
    ( "multidimensional array",
      "F64 F(){F64 x[2][3];x[1][2]=3.75;return x[1][2];}F();",
      0x400e000000000000L );
    ( "saved pointer keeps its original element",
      "F64 F(){F64 x[2];x[0]=1.5;x[1]=2.25;I64 i=0;F64 *p=&x[i];i=1;return \
       *p;}F();",
      0x3ff8000000000000L );
    ( "packed member pointer",
      "class Box{U8 tag;F64 x;};F64 F(){Box b;b.tag=7;F64 \
       *p=&b.x;*p=1.5;return b.x;}F();",
      0x3ff8000000000000L );
    ( "packed member array",
      "class Box{U8 tag;F64 x[2];U8 tail;};F64 F(){Box \
       b;b.tag=7;b.tail=9;b.x[0]=1.5;b.x[1]=2.25;return b.x[1];}F();",
      0x4002000000000000L );
    ( "owned floating pointer field",
      "class Box{F64 *p;};F64 F(){F64 x;Box b;b.p=&x;*b.p=1.5;return x;}F();",
      0x3ff8000000000000L );
    ( "array negative zero transport",
      "F64 F(F64 x){F64 a[2];a[1]=x;return a[1];}F(-0.0);",
      Int64.min_int );
    ( "recursive borrowed pointer",
      "F64 Read(I64 n,F64 *p){if(n)return Read(n-1,p);return *p;}F64 F(){F64 \
       x=1.5;return Read(3,&x);}F();",
      0x3ff8000000000000L );
    ( "pointer addition scales by eight bytes",
      "F64 F(){F64 x[2];x[0]=1.5;x[1]=2.25;F64 *p=&x[0];return *(p+1);}F();",
      0x4002000000000000L );
    ( "pointer subtraction scales by eight bytes",
      "F64 F(){F64 x[2];x[0]=1.5;x[1]=2.25;F64 *p=&x[1];return *(p-1);}F();",
      0x3ff8000000000000L );
    ( "raw packed union transport preserves signaling NaN",
      "union Box{F64 value;U64 bits;};F64 F(){Box \
       b;b.bits=0x7ff0000000000042;return b.value;}F();",
      0x7ff0000000000042L );
  ]

let storage_faults =
  [
    ( "uninitialized array leaf",
      "F64 F(){F64 x[2];return x[0];}F();",
      "HCIRVM0012" );
    ( "one-past array write",
      "F64 F(){F64 x[2];x[2]=1.5;return 42.0;}F();",
      "HCIRVM0019" );
    ( "retired local pointer",
      "F64 *Bad(){F64 x=1.5;return &x;}F64 F(){F64 *p=Bad();return *p;}F();",
      "HCIRVM0018" );
    ( "one-past pointer dereference",
      "F64 F(){F64 x[2];F64 *p=&x[0];return *(p+2);}F();",
      "HCIRVM0019" );
    ( "difference between separate floating objects",
      "I64 F(){F64 x[2];F64 y[2];F64 *p=&x[0];F64 *q=&y[0];return q-p;}F();",
      "HCIRVM0018" );
    ( "restored floating pointer bits do not restore ownership",
      "union Box{F64 *p;U64 bits;};F64 F(){F64 x=1.5;Box b;b.p=&x;U64 \
       old=b.bits;b.bits=old;return *b.p;}F();",
      "HCIRVM0018" );
    ( "floating pointer union cannot change pointee type",
      "union Box{F64 *p;I64 *q;};I64 F(){F64 x=1.5;Box b;b.p=&x;return \
       *b.q;}F();",
      "HCIRVM0018" );
    ( "floating global writes invalidate overlapped pointer ownership",
      "class Box{I64 *p;};F64 value;I64 F(){Box *b=(&value)(Box*);I64 \
       x=42;b->p=&x;F64 saved=value;value=1.5;value=saved;return *b->p;}F();",
      "HCIRVM0018" );
    ( "floating writes cannot restore callback ownership",
      "class Box{F64 (*p)(F64 x);};F64 Id(F64 x){return x;}F64 Run(){Box \
       b;b.p=&Id;F64 *alias=(&b)(F64*);F64 \
       saved=*alias;*alias=1.5;*alias=saved;return b.p(1.5);}Run();",
      "HCIRVM0024" );
  ]

let storage_word_cases =
  [
    ( "integer callback with floating argument",
      "I64 Answer(F64 x){return 42;}I64 (*f)(F64 x);f=&Answer;f(1.5);",
      42L );
    ( "same-object pointer difference",
      "I64 F(){F64 x[3];F64 *p=&x[0];F64 *q=&x[2];return q-p;}F();",
      2L );
    ( "one-past formation and reverse difference",
      "I64 F(){F64 x[3];F64 *p=&x[0];F64 *q=p+3;return p-q;}F();",
      -3L );
  ]

let persistent_cases =
  [
    ( "persistent scalar assignment",
      "F64 value;value=1.5;value;",
      0x3ff8000000000000L );
    ( "persistent array assignment",
      "F64 values[2];values[0]=1.5;values[1]=2.25;values[0]+values[1];",
      0x400e000000000000L );
    ( "persistent scalar address",
      "F64 value;F64 F(){F64 *p=&value;*p=1.5;return value;}F();",
      0x3ff8000000000000L );
    ( "static scalar retains prior writes",
      "F64 F(I64 set){static F64 x;if(set)x=1.5;return x;}F(1);F(0);",
      0x3ff8000000000000L );
    ( "persistent byte view preserves signaling NaN",
      "F64 value;(&value)(U64*)[0]=0x7ff0000000000042;value;",
      0x7ff0000000000042L );
  ]

let persistent_jit_cases =
  [
    ("live scalar initializer", "F64 value=1.5;value;", 0x3ff8000000000000L);
    ( "live array initializer",
      "F64 values[2]={1.5,2.25};values[0]+values[1];",
      0x400e000000000000L );
    ( "live static initializer",
      "F64 F(){static F64 x=1.5;return x;}F();",
      0x3ff8000000000000L );
    ( "live static array initializer",
      "F64 F(){static F64 x[2]={1.5,2.25};return x[0]+x[1];}F();",
      0x400e000000000000L );
    ( "saved function retains original floating global",
      "F64 value=1.5;F64 Read(){return value;}F64 value=2.25;Read();",
      0x3ff8000000000000L );
    ( "live floating callback initializer",
      "F64 Id(F64 x){return x;}F64 (*f)(F64 x)=&Id;f(1.5);",
      0x3ff8000000000000L );
    ( "live floating static callback initializer",
      "F64 Id(F64 x){return x;}F64 Run(){static F64 (*f)(F64 x)=&Id;return \
       f(1.5);}Run();",
      0x3ff8000000000000L );
  ]

let callback_cases =
  [
    ( "floating callback without arguments",
      "F64 Value(){return 1.5;}F64 (*f)();f=&Value;f();",
      0x3ff8000000000000L );
    ( "floating callback with saved integer default",
      "F64 Value(I64 x){return 1.5;}F64 (*f)(I64 x=42);f=&Value;f();",
      0x3ff8000000000000L );
    ( "void callback transports floating argument",
      "U0 Accept(F64 x){}F64 Run(){U0 (*f)(F64 x);f=&Accept;f(1.5);return \
       2.25;}Run();",
      0x4002000000000000L );
    ( "floating callback borrows original pointer",
      "F64 Read(F64 *p){return *p;}F64 Run(){F64 x=1.5;F64 (*f)(F64 \
       *p);f=&Read;return f(&x);}Run();",
      0x3ff8000000000000L );
    ( "floating callback materialized result",
      "F64 Id(F64 x){return x;}F64 Run(){F64 (*f)(F64 x);f=&Id;F64 \
       saved=f(1.5);return saved+2.25;}Run();",
      0x400e000000000000L );
    ( "floating callback argument and result",
      "F64 Id(F64 x){return x;}F64 (*f)(F64 x);f=&Id;f(1.5);",
      0x3ff8000000000000L );
    ( "floating callback arithmetic",
      "F64 Add(F64 a,F64 b){return a+b;}F64 (*f)(F64 a,F64 \
       b);f=&Add;f(1.5,2.25);",
      0x400e000000000000L );
    ( "floating callback fixed mixed arguments",
      "F64 Add(F64 a,I64 b){return a+b;}F64 (*f)(F64 a,I64 b);f=&Add;f(1.5,2);",
      0x400c000000000000L );
    ( "floating callback automatic slot",
      "F64 Id(F64 x){return x;}F64 Run(){F64 (*f)(F64 x);f=&Id;return \
       f(1.5);}Run();",
      0x3ff8000000000000L );
    ( "floating callback static slot",
      "F64 Id(F64 x){return x;}F64 Run(){static F64 (*f)(F64 x);f=&Id;return \
       f(1.5);}Run();",
      0x3ff8000000000000L );
    ( "floating callback field",
      "F64 Id(F64 x){return x;}class Box{F64 (*f)(F64 x);};F64 Run(){Box \
       b;b.f=&Id;return b.f(1.5);}Run();",
      0x3ff8000000000000L );
    ( "floating callback array",
      "F64 Id(F64 x){return x;}F64 (*f)(F64 x)[2];f[1]=&Id;f[1](1.5);",
      0x3ff8000000000000L );
    ( "floating callback argument forwarding",
      "F64 Id(F64 x){return x;}F64 Apply(F64 (*f)(F64 x),F64 value){return \
       f(value);}Apply(&Id,1.5);",
      0x3ff8000000000000L );
    ( "floating callback preserves signaling NaN",
      "F64 Id(F64 x){return x;}F64 Run(){F64 \
       x;(&x)(U64*)[0]=0x7ff0000000000042;F64 (*f)(F64 x);f=&Id;return \
       f(x);}Run();",
      0x7ff0000000000042L );
    ( "floating callback snapshots callee before arguments",
      "F64 First(F64 x){return x;}F64 Second(F64 x){return x+2.25;}F64 \
       (*p)(F64 x);F64 Switch(){p=&Second;return 1.5;}p=&First;p(Switch());",
      0x3ff8000000000000L );
    ( "floating callback recursion",
      "F64 (*p)(F64 x,I64 n);F64 Next(F64 x,I64 n){if(n)return p(x,n-1);return \
       x;}p=&Next;p(1.5,3);",
      0x3ff8000000000000L );
  ]

let excluded =
  [
    ( "unproved literal reassociation",
      "F64 F(F64 a){return (a+0.5)+0.5;}F(1.0);" );
    ("remainder", "F64 F(F64 a,F64 b){return a%b;}F(1.0,2.0);");
    ("compound update", "F64 F(F64 x){x+=1.0;return x;}F(1.0);");
    ("deeper pointer", "F64 F(F64 **x){return **x;}42;");
    ("pointer array", "F64 F(){F64 *x[2];return 1.0;}42;");
    ("global pointer", "F64 *x;42;");
    ("static pointer", "F64 F(){static F64 *x;return 1.0;}42;");
    ("default", "F64 F(F64 x=1.0){return x;}42;");
    ( "floating variadic callback argument",
      "I64 First(I64 x,...){return x;}I64 (*f)(I64 x,...);f=&First;f(42,1.0);"
    );
    ("unsupported fixed register", "F64 F(){F64 reg RAX x=1.0;return x;}42;");
    ("float format", "extern U0 Print(U8 *fmt,...);Print(\"%f\",1.0);");
    ("shift", "F64 F(F64 x){return x<<1;}F(1.0);");
    ("power", "F64 F(F64 x){return x**2.0;}F(1.0);");
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
    Session.add_source session ~path:"native-scalar-f64.hc" ~contents
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
  let source = "F64 Add(F64 a,F64 b){F64 y=a+b;return y;}Add(1.5,2.25);" in
  let expected = 0x400e000000000000L and frame = 24 in
  let compiled = image mode source in
  let ir = Image.ir_instructions compiled
  and code = Image.code_bytes compiled in
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:code mode source
    |> Result.map_error diagnostic_text
    |> checked);
  List.iter
    (fun result ->
      require (Result.is_error result)
        "one-below native emission quota accepted")
    [
      compile ~max_ir_instructions:(ir - 1) mode source;
      compile ~max_code_bytes:(code - 1) mode source;
    ];
  let retained = Runtime.retain compiled |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () ->
      let run ?max_frame_bytes steps =
        Runtime.execute_retained_report ?max_frame_bytes ~max_steps:steps
          retained
      in
      let steps = run 100000 |> completed expected in
      for _ = 1 to 3 do
        Gc.compact ();
        ignore (run steps |> completed expected)
      done;
      let exhausted = run (steps - 1) |> fault Image.Step_limit_exceeded in
      require
        (exhausted.executed_steps = steps - 1 && Option.is_some exhausted.span)
        "step fault lost exact charge/source site";
      ignore (run steps |> completed expected);
      (* Two eight-byte F64 parameters and one eight-byte F64 local.
         This quota is source frame storage, not the machine spill frame. *)
      ignore (run ~max_frame_bytes:frame steps |> completed expected);
      ignore
        (run ~max_frame_bytes:(frame - 1) steps
        |> fault Image.Frame_limit_exceeded);
      ignore (run steps |> completed expected))

let retained_persistent mode =
  let compiled =
    image mode
      "I64 seen=0;F64 value;F64 F(){if(seen)return value;value=1.5;seen=1;F64 \
       saved=value;value=2.25;return saved;}F();"
  in
  (* Ordinary executions have fresh arenas; an explicitly retained image keeps
     its original words and byte initialization state across activations. *)
  for _ = 1 to 2 do
    ignore
      (Runtime.execute_report ~max_steps:100000 compiled
      |> completed 0x3ff8000000000000L)
  done;
  let retained = Runtime.retain compiled |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () ->
      let run steps =
        Runtime.execute_retained_report ~max_steps:steps retained
      in
      ignore (run 100000 |> completed 0x3ff8000000000000L);
      let steps = run 100000 |> completed 0x4002000000000000L in
      for _ = 1 to 3 do
        Gc.compact ();
        ignore (run steps |> completed 0x4002000000000000L)
      done;
      ignore (run (steps - 1) |> fault Image.Step_limit_exceeded);
      ignore (run steps |> completed 0x4002000000000000L))

let retained_callback mode =
  let compiled =
    image mode "F64 Id(F64 x){return x;}F64 (*f)(F64 x);f=&Id;f(1.5);"
  in
  let retained = Runtime.retain compiled |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () ->
      let run steps =
        Runtime.execute_retained_report ~max_steps:steps retained
      in
      let steps = run 100000 |> completed 0x3ff8000000000000L in
      for _ = 1 to 3 do
        Gc.compact ();
        ignore (run steps |> completed 0x3ff8000000000000L)
      done;
      ignore (run (steps - 1) |> fault Image.Step_limit_exceeded);
      ignore (run steps |> completed 0x3ff8000000000000L))

let persistent_limits mode target =
  let source = "F64 value;value=1.5;value;" in
  invoke ~target ~mode
    ~options:[ "--global-byte-limit=8" ]
    "exact persistent extent" source
  |> successful "exact persistent extent" 0x3ff8000000000000L;
  invoke ~status:1 ~target ~mode
    ~options:[ "--global-byte-limit=7" ]
    "one-below persistent extent" source
  |> rejected "one-below persistent extent";
  if mode = "jit" && target = "host-jit-task" then (
    let source = "F64 value=1.5;value;" in
    invoke ~target ~mode
      ~options:[ "--initializer-step-limit=11" ]
      "exact floating copy allowance" source
    |> successful "exact floating copy allowance" 0x3ff8000000000000L;
    invoke ~status:1 ~target ~mode
      ~options:[ "--initializer-step-limit=10" ]
      "one-below floating copy allowance" source
    |> rejected "one-below floating copy allowance")

let cli_step_quota ?(source = "F64 Add(F64 a,F64 b){return a+b;}Add(1.5,2.25);")
    mode target =
  let baseline = invoke ~target ~mode "step baseline" source in
  successful "step baseline" 0x400e000000000000L baseline;
  let steps = baseline |> member "executed_steps" |> to_int in
  invoke ~target ~mode
    ~options:[ "--step-limit=" ^ string_of_int steps ]
    "exact step quota" source
  |> successful "exact step quota" 0x400e000000000000L;
  let failed =
    invoke ~status:1 ~target ~mode
      ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
      "one-below step quota" source
  in
  rejected "one-below step quota" failed;
  require
    (member "executed_steps" failed = `Int (steps - 1))
    "CLI fault step charge";
  require
    (failed |> member "diagnostics" |> to_list
    |> List.exists (fun d ->
        let primary = member "primary" d in
        match (member "path" primary, member "line" primary) with
        | `String path, `Int line -> path <> "" && line > 0
        | _ -> false))
    "CLI fault lost source location"

let capability mode =
  let pure = image mode "F64 Id(F64 x){return x;}Id(1.5);" in
  require
    (not (Image.requires_x87 pure))
    "raw F64 transport must not invent x87 use";
  let persistent = image mode "F64 value;value=1.5;value;" in
  require
    (not (Image.requires_x87 persistent))
    "persistent F64 bit transport must not invent x87 use";
  let callback =
    image mode "F64 Id(F64 x){return x;}F64 (*f)(F64 x);f=&Id;f(1.5);"
  in
  require
    (not (Image.requires_x87 callback))
    "fixed F64 callback transport must not invent x87 use";
  let source =
    Bytes.of_string
      "U0 Consume(F64 x){}I64 F(F64 a,F64 b){Consume(a+b);return \
       42;}F(1.5,2.25);"
  in
  let compiled = image mode (Bytes.to_string source) in
  require
    (Image.requires_x87 compiled)
    "integer final value hid executed x87 operation";
  let exported = Bytes.of_string (Image.code compiled) in
  Bytes.fill exported 0 (Bytes.length exported) '\000';
  Bytes.fill source 0 (Bytes.length source) ' ';
  require
    (Image.requires_x87 compiled)
    "opaque image capability changed after copying code/source";
  require
    (Image.code compiled <> Bytes.to_string exported)
    "exported code copy changed opaque image"

let () =
  List.iter
    (fun mode ->
      let arithmetic = "F64 Add(F64 a,F64 b){return a+b;}Add(1.5,2.25);" in
      invoke ~status:1 ~target:"ir" ~mode "IR is not an F64 arithmetic oracle"
        arithmetic
      |> rejected "IR float boundary";
      List.iter
        (fun target ->
          List.iter
            (fun (label, source) ->
              invoke ~status:1 ~target ~mode label source |> rejected label)
            excluded)
        targets;
      capability mode;
      if native then (
        let has_x87 = Native_execution.x87_supported () in
        let admitted =
          if has_x87 then
            cases @ copy_cases @ storage_cases @ persistent_cases
            @ callback_cases
          else
            List.filter
              (fun (name, _, _) ->
                List.mem name
                  [
                    "literal";
                    "local copy";
                    "assignment";
                    "direct call";
                    "branch";
                    "loop";
                    "recursion";
                  ])
              cases
        in
        List.iter
          (fun target ->
            if has_x87 then
              invoke ~target ~mode "maintained example" (read example_path)
              |> successful "maintained example" 0x400e000000000000L;
            (if has_x87 then
               let path =
                 if Sys.file_exists "examples/native-f64-storage.hc" then
                   "examples/native-f64-storage.hc"
                 else "../examples/native-f64-storage.hc"
               in
               invoke ~target ~mode "maintained floating storage example"
                 (read path)
               |> successful "maintained floating storage example"
                    0x400e000000000000L);
            (if has_x87 then
               let path =
                 if Sys.file_exists "examples/native-f64-callbacks.hc" then
                   "examples/native-f64-callbacks.hc"
                 else "../examples/native-f64-callbacks.hc"
               in
               invoke ~target ~mode "maintained floating callback example"
                 (read path)
               |> successful "maintained floating callback example"
                    0x400e000000000000L);
            List.iter
              (fun (label, source, expected) ->
                let report = invoke ~target ~mode label source in
                successful label expected report;
                if target = "host-jit-task" then
                  require
                    (report |> member "native" |> member "fragments" |> to_list
                    |> List.exists (fun fragment ->
                        member "outcome" fragment = `String "success"))
                    (label ^ ": no normal task fragment activation");
                if label = "infinity" then
                  require
                    (report |> member "final_value" |> member "value"
                   = `String "inf")
                    "infinity must be JSON string inf")
              admitted;
            List.iter
              (fun (label, source, code) ->
                let report = invoke ~status:1 ~target ~mode label source in
                rejected label report;
                require
                  (report |> member "diagnostics" |> to_list
                  |> List.exists (fun d -> member "code" d = `String code))
                  (label ^ ": wrong storage fault"))
              storage_faults;
            List.iter
              (fun (label, source, expected) ->
                let report = invoke ~target ~mode label source in
                require
                  (member "outcome" report = `String "success"
                  && report |> member "final_value" |> member "type"
                     = `String "i64"
                  && report |> member "final_value" |> member "bits"
                     = `String (Printf.sprintf "0x%016Lx" expected))
                  (label ^ ": wrong pointer difference"))
              storage_word_cases;
            if has_x87 && target = "host-jit-task" && mode = "jit" then (
              List.iter
                (fun (label, source, bits) ->
                  invoke ~target ~mode label source |> successful label bits)
                persistent_jit_cases;
              let path =
                if Sys.file_exists "examples/native-persistent-f64.hc" then
                  "examples/native-persistent-f64.hc"
                else "../examples/native-persistent-f64.hc"
              in
              let report =
                invoke ~target ~mode "persistent floating example" (read path)
              in
              successful "persistent floating example" 0x400e000000000000L
                report;
              require
                (member "output_hex" report = `String "41")
                "persistent static preparation must print A once";
              let unused =
                invoke ~target ~mode "unused floating static initializer"
                  "extern U0 Print(U8 *fmt,...);F64 \
                   Setup(){Print(\"A\");return 1.5;}F64 Unused(){static F64 \
                   value=Setup();return value;}42;"
              in
              require
                (member "outcome" unused = `String "success"
                && member "output_hex" unused = `String "41"
                && unused |> member "final_value" |> member "bits"
                   = `String "0x000000000000002a")
                "unused floating static must initialize before publication";
              List.iter
                (fun declaration ->
                  let source =
                    "extern U0 Print(U8 *fmt,...);F64 \
                     Good(){Print(\"A\");return 1.5;}F64 \
                     Fail(){Print(\"B\");I64 z=0;I64 q=7/z;return 2.25;}"
                    ^ declaration
                  in
                  let report =
                    invoke ~status:1 ~target ~mode
                      "late floating initializer fault" source
                  in
                  rejected "late floating initializer fault" report;
                  require
                    (member "output_hex" report = `String "4142"
                    && member "compiled_initializer_steps" report |> to_int > 8
                    && member "diagnostics" report
                       |> to_list
                       |> List.exists (fun d ->
                           member "code" d = `String "HCIRVM0009"))
                    "late floating initializer fault lost prior effects or work")
                [
                  "F64 values[2]={Good(),Fail()};Print(\"C\");42;";
                  "F64 Unused(){static F64 values[2]={Good(),Fail()};return \
                   values[0];}Print(\"C\");42;";
                ]);
            List.iter
              (fun source ->
                if mode = "jit" then (
                  let report =
                    invoke ~status:1 ~target ~mode "persistent unread bytes"
                      source
                  in
                  rejected "persistent unread bytes" report;
                  require
                    (report |> member "diagnostics" |> to_list
                    |> List.exists (fun d ->
                        member "code" d = `String "HCIRVM0012"))
                    "persistent unread bytes lost the initialization guard")
                else
                  invoke ~target ~mode "AOT persistent zero image" source
                  |> successful "AOT persistent zero image" 0L)
              [ "F64 value;value;"; "F64 value;(&value)(U8*)[0]=0;value;" ];
            if target <> "host-jit-task" || mode <> "jit" then
              List.iter
                (fun source ->
                  invoke ~status:1 ~target ~mode
                    "floating preparation remains separate" source
                  |> rejected "floating preparation remains separate")
                [
                  "F64 value=1.5;value;";
                  "F64 F(){static F64 value=1.5;return value;}F();";
                ];
            if has_x87 then
              let report =
                invoke ~target ~mode "NaN rendering"
                  "F64 F(F64 a,F64 b){return a/b;}F(0.0,0.0);"
              in
              let word = member "final_value" report in
              let bits = Int64.of_string (word |> member "bits" |> to_string) in
              require
                (member "outcome" report = `String "success"
                && member "type" word = `String "f64"
                && Int64.logand bits 0x7ff0000000000000L = 0x7ff0000000000000L
                && Int64.logand bits 0x000fffffffffffffL <> 0L
                && (member "value" word = `String "nan"
                   || member "value" word = `String "-nan"))
                "NaN must retain a floating payload and string display"
            else
              invoke ~status:1 ~target ~mode "x87 capability rejection"
                arithmetic
              |> rejected "x87 unavailable")
          targets;
        if has_x87 then (
          retained mode;
          retained_persistent mode;
          retained_callback mode;
          List.iter (persistent_limits mode) targets;
          List.iter (cli_step_quota mode) targets;
          List.iter
            (cli_step_quota
               ~source:
                 "F64 Add(F64 a,F64 b){return a+b;}F64 (*f)(F64 a,F64 \
                  b);f=&Add;f(1.5,2.25);"
               mode)
            targets;
          List.iter
            (cli_step_quota
               ~source:
                 "F64 F(){F64 x[2];x[0]=1.5;x[1]=2.25;return x[0]+x[1];}F();"
               mode)
            targets)))
    modes;
  Printf.printf "native scalar F64: %d CLI reports (%s)\n" !count
    (if native then "native" else "admission")
