open Yojson.Safe.Util
open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution

(* Source-derived expectations, not new TempleOS runtime captures.
   OptPass4.HC:351-405 makes a direct frame assignment by-value. BackC.HC:162-170
   stores and then reloads that destination. BackLib.HC:509-525 retains register
   words but extends memory reads according to the destination's declared type.
   Do not derive expected values by running another hosted executor. *)
let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-consumed-stack-assignment-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 3 && Sys.argv.(2) = "--native"
let modes = [ "jit"; "aot" ]
let reports = ref 0
let retained_recoveries = ref 0
let targets = if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]

let invoke ?(status = 0) ?(options = []) ~target ~mode label source =
  incr reports;
  temporary ".hc" source (fun path ->
      temporary ".out" "" (fun output ->
          temporary ".err" "" (fun errors ->
              let out_fd =
                Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
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
                   ]
                  @ options @ [ path ])
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler args Unix.stdin out_fd err_fd)
              in
              let _, actual = Unix.waitpid [] pid in
              let output = read output and errors = read errors in
              require
                (actual = Unix.WEXITED status && errors = "")
                (label ^ " " ^ target ^ " " ^ mode ^ ": stdout=" ^ output
               ^ "; stderr=" ^ errors ^ "; source=" ^ source);
              Yojson.Safe.from_string output)))

let value label expected report =
  require
    (member "outcome" report = `String "success"
    && member "bits" (member "final_value" report)
       = `String (Printf.sprintf "0x%016Lx" expected))
    (label ^ ": expected " ^ Int64.to_string expected ^ "; "
    ^ Yojson.Safe.to_string report)

let fault label code report =
  require
    (member "outcome" report = `String "error"
    && report |> member "diagnostics" |> to_list
       |> List.exists (fun diagnostic ->
           member "code" diagnostic = `String code))
    (label ^ ": " ^ Yojson.Safe.to_string report)

type integer_type = { name : string; bits : int; signed : bool }

let integer_types =
  List.concat_map
    (fun bits ->
      [
        { name = "I" ^ string_of_int bits; bits; signed = true };
        { name = "U" ^ string_of_int bits; bits; signed = false };
      ])
    [ 8; 16; 32; 64 ]

let normalize type_ word =
  let shift = 64 - type_.bits in
  let low_bits = Int64.shift_left word shift in
  if type_.signed then Int64.shift_right low_bits shift
  else Int64.shift_right_logical low_bits shift

let qualifiers = [ ""; "reg"; "noreg"; "reg R12" ]
let locations = [ "parameter"; "local" ]

let stored ~disabled type_ qualifier word =
  if qualifier = "noreg" || (disabled && qualifier <> "reg R12") then
    normalize type_ word
  else word

let boundaries type_ =
  [
    ( "wrap to zero",
      if type_.bits = 64 then -1L
      else Int64.pred (Int64.shift_left 1L type_.bits) );
    ("sign bit", Int64.pred (Int64.shift_left 1L (type_.bits - 1)));
  ]

let definition type_ qualifier location body =
  if location = "parameter" then
    Printf.sprintf "I64 F(%s %s x,I64 a){%s}" type_.name qualifier body
  else Printf.sprintf "I64 F(I64 a){%s %s x=0;%s}" type_.name qualifier body

let call location seed =
  Printf.sprintf "F(%s0x%Lx)" (if location = "parameter" then "0," else "") seed

let conditional word = if word = 0L then -1L else word

let consumers =
  [
    ("return", "return x=a+1;", Fun.id);
    ("arithmetic", "return (x=a+1)+7;", fun word -> Int64.add word 7L);
    ("named argument", "return G(x=a+1);", fun word -> Int64.add word 7L);
    ("if", "if(x=a+1)return x;return -1;", conditional);
    ("while", "while(x=a+1)return x;return -1;", conditional);
    ( "do",
      "I64 noreg n=1;do {if(n==0)return x;n--;}while(x=a+1);return -1;",
      conditional );
    ("for", "for(;x=a+1;)return x;return -1;", conditional);
    ( "comparison",
      "return (x=a+1)==0;",
      fun word -> if word = 0L then 1L else 0L );
  ]

type case = { label : string; source : string; expected : int64 }

let case label source expected = { label; source; expected }

let width_cases =
  List.concat_map
    (fun type_ ->
      List.concat_map
        (fun qualifier ->
          List.concat_map
            (fun location ->
              List.concat_map
                (fun (boundary, seed) ->
                  List.map
                    (fun (consumer, body, consume) ->
                      let label =
                        String.concat " "
                          [
                            type_.name; qualifier; location; boundary; consumer;
                          ]
                      in
                      let source =
                        "I64 G(I64 value){return value+7;}"
                        ^ definition type_ qualifier location body
                        ^ call location seed ^ ";"
                      in
                      case label source
                        (consume
                           (stored ~disabled:false type_ qualifier
                              (Int64.succ seed))))
                    consumers)
                (boundaries type_))
            locations)
        qualifiers)
    integer_types

(* #exe executes option writes and definitions in a single source task.
   Native_program.compile has no directive executor: only the public IR and
   host-jit-task paths run these cases, in both outer source modes. *)
let option_cases =
  List.concat_map
    (fun type_ ->
      List.concat_map
        (fun qualifier ->
          List.concat_map
            (fun location ->
              List.concat_map
                (fun disabled ->
                  List.map
                    (fun change_after_definition ->
                      let boundary, seed = List.hd (boundaries type_) in
                      let before = if disabled then 1 else 0 in
                      let after =
                        if change_after_definition then
                          Printf.sprintf "Option(34,%d);" (1 - before)
                        else ""
                      in
                      let source =
                        Printf.sprintf
                          "#exe {Option(34,%d);%s%sStreamPrint(\"%%d;\",%s);}"
                          before
                          (definition type_ qualifier location
                             "if(x=a+1)return x;return -1;")
                          after (call location seed)
                      in
                      case
                        (Printf.sprintf "%s %s %s %s NO_REG_VAR=%d later=%b"
                           type_.name qualifier location boundary before
                           change_after_definition)
                        source
                        (conditional
                           (stored ~disabled type_ qualifier (Int64.succ seed))))
                    [ false; true ])
                [ false; true ])
            locations)
        qualifiers)
    integer_types

let boundary_cases =
  [
    case "pointer assignment keeps current RHS result"
      "I8 Global=0;I64 F(I8 *p,I64 a){return *p=a+1;}F(&Global,255);" 256L;
    case "global assignment keeps current RHS result"
      "I8 Global=0;I64 F(I64 a){return Global=a+1;}F(255);" 256L;
    case "escaped frame assignment keeps current RHS result"
      "I64 F(I8 x,I64 a){I8 *p=&x;return x=a+1;}F(0,255);" 256L;
    case "unknown pressure keeps current condition result"
      "I64 F(I8 x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
       h){if(x=a+1)return x+b+c+d+e+f+g+h;return -1;}F(0,255,0,0,0,0,0,0,0);"
      0L;
    case "nested assignment remains outside the direct RHS proof"
      "I64 F(I8 noreg x,I8 noreg y,I64 a){return x=(y=a+1);}F(0,0,255);" 256L;
    case "consumed multiplication keeps its separate result rule"
      "I64 F(I8 noreg x,I64 a){return x*=a;}F(127,2);" 254L;
    case "stack result and stored value survive a named call"
      "I64 G(I64 n){return n+1;}I64 F(I8 noreg x,I64 a){return \
       G(x=a+1)+x;}F(0,127);"
      (-255L);
    case "selected result and stored value survive a named call"
      "I64 G(I64 n){return n+1;}I64 F(I8 reg R12 x,I64 a){return \
       G(x=a+1)+x;}F(0,127);"
      257L;
    case "assignment in a for update crosses a backedge"
      "I64 F(I8 noreg x,I64 a){I64 noreg n=0;for(;n<2;x=a+n){n++;}return \
       x;}F(0,127);"
      (-127L);
    case "assignment in a while condition crosses a backedge"
      "I64 F(I8 noreg x,I64 a){I64 noreg n=0;while(x=a+n){n++;if(n==2)return \
       x;}return -1;}F(0,127);"
      (-128L);
    case "maintained consumed stack assignment example"
      (read "../examples/register-stack-assignment.hc")
      514L;
  ]

let recursive_source qualifier =
  Printf.sprintf
    "I64 F(I8 %s x,I64 n){if(n)return (x=n+255)+F(0,n-1);return x=257;}F(0,3);"
    qualifier

let checked label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ error)

let compile mode source =
  let session = Session.create () in
  let input =
    Session.add_source session ~path:"consumed-stack-assignment.hc"
      ~contents:source
  in
  let config =
    Preprocessor.Config.create
      ~compilation_mode:
        (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
      ()
    |> checked "configuration"
  in
  match Native_program.compile session ~config ~source:input with
  | Ok image -> image.value
  | Error diagnostics ->
      failwith
        (source ^ ": "
        ^ String.concat "; "
            (List.map
               (fun (diagnostic : Diagnostic.t) ->
                 diagnostic.code ^ ": " ^ diagnostic.message)
               diagnostics))

let completed label expected = function
  | Program.Completed execution ->
      require
        (Option.map
           (fun (word : Program.word) -> word.bits)
           execution.final_value
        = Some expected)
        (label ^ ": wrong retained-image result");
      execution.executed_steps
  | Program.Fault _ -> failwith (label ^ ": unexpected retained-image fault")

let with_retained mode source action =
  let retained = Runtime.retain (compile mode source) |> checked "retain" in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked "release")
    (fun () -> action retained)

let execute retained max_steps =
  Runtime.execute_retained_report ~max_steps retained
  |> Runtime.outcome |> checked "execute retained"

let retained_fault_recovery mode =
  List.iter
    (fun qualifier ->
      let label = qualifier ^ " consumed assignment recursion " ^ mode in
      let expected = if qualifier = "noreg" then 4L else 1028L in
      with_retained mode (recursive_source qualifier) (fun retained ->
          let steps = execute retained 100_000 |> completed label expected in
          let owned_faults = ref 0 in
          for limit = 1 to steps - 1 do
            (match execute retained limit with
            | Program.Fault fault ->
                require
                  (fault.kind = Program.Step_limit_exceeded
                  && fault.executed_steps = limit)
                  (label ^ ": wrong bounded recursion fault");
                if fault.function_name = Some "F" then incr owned_faults
            | Program.Completed _ ->
                failwith (label ^ ": bounded recursion unexpectedly completed"));
            ignore (execute retained steps |> completed label expected);
            incr retained_recoveries
          done;
          require (!owned_faults > 0)
            (label ^ ": no fault occurred in the assignment-owning function"));
      List.iter
        (fun after_assignment ->
          let body =
            if after_assignment then "return (x=a+1)/d;" else "return x=a/d;"
          in
          let source =
            Printf.sprintf
              "I64 Attempts=0;I64 F(I8 %s x,I64 a,I64 \
               d){%s}Attempts++;F(0,%d,Attempts-1);"
              qualifier body
              (if after_assignment then 127 else 128)
          in
          with_retained mode source (fun retained ->
              (match execute retained 100_000 with
              | Program.Fault fault ->
                  require
                    (fault.kind = Program.Division_by_zero
                    && fault.function_name = Some "F")
                    (label ^ ": wrong retained assignment division fault")
              | Program.Completed _ ->
                  failwith (label ^ ": expected the first invocation to fault"));
              let second = if qualifier = "noreg" then -128L else 128L in
              let third =
                if after_assignment then Int64.div second 2L else 64L
              in
              ignore (execute retained 100_000 |> completed label second);
              ignore (execute retained 100_000 |> completed label third);
              incr retained_recoveries))
        [ false; true ])
    qualifiers

let cli_faults mode =
  List.iter
    (fun qualifier ->
      List.iter
        (fun location ->
          List.iter
            (fun (operator, argument, divisor, code) ->
              let definition =
                if location = "parameter" then
                  Printf.sprintf "I64 F(I8 %s x,I64 a,I64 d){return x=a%s d;}"
                    qualifier operator
                else
                  Printf.sprintf "I64 F(I64 a,I64 d){I8 %s x=0;return x=a%s d;}"
                    qualifier operator
              in
              let source =
                definition
                ^ Printf.sprintf "F(%s%s,%s);"
                    (if location = "parameter" then "0," else "")
                    argument divisor
              in
              let label =
                String.concat " " [ qualifier; location; operator; code ]
              in
              List.iter
                (fun target ->
                  invoke ~status:1 ~target ~mode label source
                  |> fault label code)
                targets)
            [
              ("/", "128", "0", "HCIRVM0009");
              ("%", "128", "0", "HCIRVM0009");
              ("/", "0x8000000000000000", "-1", "HCIRVM0010");
              ("%", "0x8000000000000000", "-1", "HCIRVM0010");
            ])
        locations)
    qualifiers

let run_case mode targets case =
  let label = case.label ^ " " ^ mode in
  let baseline = ref None in
  List.iter
    (fun target ->
      let report = invoke ~target ~mode label case.source in
      value (label ^ " " ^ target) case.expected report;
      require
        (member "output_hex" report = `String "")
        (label ^ ": unexpected captured output");
      let bits = member "bits" (member "final_value" report) in
      match !baseline with
      | None -> baseline := Some bits
      | Some expected ->
          require (bits = expected) (label ^ ": IR/native bits differ"))
    targets

let () =
  let recursion_cases =
    List.map
      (fun qualifier ->
        case
          (qualifier ^ " consumed assignment across recursion")
          (recursive_source qualifier)
          (if qualifier = "noreg" then 4L else 1028L))
      qualifiers
  in
  List.iter
    (fun mode ->
      List.iter (run_case mode targets)
        (width_cases @ boundary_cases @ recursion_cases);
      List.iter
        (run_case mode (if native then [ "ir"; "host-jit-task" ] else [ "ir" ]))
        option_cases;
      cli_faults mode;
      if native then retained_fault_recovery mode)
    modes;
  Printf.printf
    "Consumed stack assignment checks passed (%d CLI reports, %d same-mapping \
     fault recoveries).\n"
    !reports !retained_recoveries
