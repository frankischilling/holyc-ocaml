open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* Independent pinned-source expectations, not a new runtime oracle:
   TempleOS c26482bb OptPass3:30-46 reserves explicit registers first, excludes
   ordinary F64 from candidates, and seeds plain reg with I64_MAX/2.
   OptPass3:536-615 applies NO_REG_VAR only to candidates and opens the
   R12/R13 bank above six surviving candidates. Narrow selected variables
   retain full words; noreg stores retain declared width. Exact placements
   below include source-derived whole-function ties as well as explicit and
   sole-candidate placements. *)
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
  let path = Filename.temp_file "holyc-f64-register-" suffix in
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

let config mode =
  Preprocessor.Config.create
    ~compilation_mode:
      (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
    ()
  |> checked

let compile ?max_ir_instructions ?max_code_bytes mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-f64-register.hc" ~contents
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

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let section_instructions label name sections =
  let symbols = sections |> member "symbols" |> to_list in
  let symbol =
    match
      List.find_opt (fun symbol -> member "name" symbol = `String name) symbols
    with
    | Some symbol -> symbol
    | None ->
        failwith
          (label ^ ": missing function " ^ name ^ "; available symbols: "
          ^ String.concat ", "
              (List.map
                 (fun symbol -> member "name" symbol |> to_string)
                 symbols))
  in
  let address = member "definition" symbol in
  let section_id = member "section" address |> to_int in
  let start = member "offset" address |> to_int in
  let section =
    match
      sections |> member "sections" |> to_list
      |> List.find_opt (fun section -> member "id" section = `Int section_id)
    with
    | Some section -> section
    | None ->
        failwith
          (label ^ ": function " ^ name ^ " has no section "
         ^ string_of_int section_id)
  in
  let extent =
    sections |> member "mappings" |> to_list
    |> List.filter (fun mapping -> member "address" mapping = address)
    |> List.fold_left
         (fun extent mapping -> max extent (member "size" mapping |> to_int))
         0
  in
  let code = section |> member "bytes" |> to_string |> bytes in
  match
    Disassembly.decode ~mode:Asm_operand.Use64 ~position:(Int64.of_int start)
      (String.sub code start extent)
  with
  | Ok decoded -> Disassembly.instructions decoded
  | Error error ->
      failwith
        (Printf.sprintf "%s: decode at byte %d: %s" label error.offset
           error.message)

let function_instructions label name image =
  section_instructions label name
    (Image.native_sections image |> Holyc_lib__Asm.Native_sections.to_yojson)

let frame_moves ~save register instructions =
  List.filter
    (fun (instruction : Disassembly.instruction) ->
      instruction.opcode = "MOV"
      &&
      match instruction.operands with
      | [ destination; source ] ->
          let memory, reg =
            if save then (destination.text, source.text)
            else (source.text, destination.text)
          in
          reg = "U64 " ^ register && String.ends_with ~suffix:"[RBP]" memory
      | _ -> false)
    instructions

let operands (instruction : Disassembly.instruction) =
  List.map
    (fun (operand : Disassembly.operand) -> operand.text)
    instruction.operands

let entry_pairs instructions =
  let rec collect result = function
    | (load : Disassembly.instruction) :: store :: rest -> (
        match load.operands with
        | [ destination; source ]
          when List.mem load.opcode [ "MOV"; "MOVSX"; "MOVZX"; "MOVSXD" ]
               && List.mem destination.text [ "U64 RAX"; "U32 EAX" ]
               && String.ends_with ~suffix:"[RBP]" source.text
               && Option.fold ~none:false ~some:(fun n -> n >= 16L) source.value
          -> collect ((source, store) :: result) (store :: rest)
        | _ -> collect result (store :: rest))
    | _ -> List.rev result
  in
  collect [] instructions

let registers = [ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15" ]

type case = {
  label : string;
  source : string;
  kind : string;
  bits : int64;
  entries : (int * string) list option;
  required : string list;
  forbidden : int list;
}

let case ?entries ?(required = []) ?(forbidden = []) ?(kind = "f64") label
    source bits =
  { label; source; kind; bits; entries; required; forbidden }

let value case report =
  if case.kind = "f64" then successful case.label case.bits report;
  let word = member "final_value" report in
  require
    (member "outcome" report = `String "success"
    && member "type" word = `String case.kind
    && member "bits" word = `String (Printf.sprintf "0x%016Lx" case.bits))
    (case.label ^ ": " ^ Yojson.Safe.to_string word)

let mapping instructions =
  entry_pairs instructions
  |> List.filter_map (fun ((source : Disassembly.operand), store) ->
      match operands store with
      | [ destination; "U64 RAX" ] when store.opcode = "MOV" ->
          List.find_map
            (fun register ->
              if destination = "U64 " ^ register then
                Some
                  ((Int64.to_int (Option.get source.value) - 16) / 8, register)
              else None)
            registers
      | _ -> None)
  |> List.sort_uniq compare

let placement ?(name = "F") case image =
  let instructions = function_instructions case.label name image in
  let actual = mapping instructions in
  Option.iter
    (fun expected ->
      require
        (actual = List.sort_uniq compare expected)
        (case.label ^ ": physical parameter mapping differs"))
    case.entries;
  List.iter
    (fun index ->
      require
        (not (List.mem_assoc index actual))
        (case.label ^ ": ordinary/noreg F64 unexpectedly selected"))
    case.forbidden;
  List.iter
    (fun register ->
      require
        (frame_moves ~save:true register instructions <> []
        && frame_moves ~save:false register instructions <> [])
        (case.label ^ ": missing physical save/restore for " ^ register);
      require
        (List.exists
           (fun (i : Disassembly.instruction) ->
             i.opcode = "MOV"
             &&
             match i.operands with
             | [ destination; source ] ->
                 destination.text = "U64 " ^ register && source.size = 64
             | _ -> false)
           instructions)
        (case.label ^ ": missing full-word register write " ^ register))
    case.required

let explicit_cases =
  List.concat_map
    (fun register ->
      let parameter arg =
        "F64 F(F64 reg " ^ register ^ " x){return x;}F(" ^ arg ^ ");"
      in
      [
        case
          ~entries:[ (0, register) ]
          ~required:[ register ]
          (register ^ " raw parameter")
          (parameter "1.5") 0x3ff8000000000000L;
        case
          ~entries:[ (0, register) ]
          ~required:[ register ]
          (register ^ " positive zero")
          (parameter "0.0") 0L;
        case
          ~entries:[ (0, register) ]
          ~required:[ register ]
          (register ^ " negative zero")
          ("F64 Neg(F64 x){return -x;}" ^ parameter "Neg(0.0)")
          Int64.min_int;
        case
          ~entries:[ (0, register) ]
          ~required:[ register ] (register ^ " infinity")
          ("F64 Div(F64 x,F64 y){return x/y;}" ^ parameter "Div(1.0,0.0)")
          0x7ff0000000000000L;
        case ~required:[ register ] ~forbidden:[ 0 ] (register ^ " local copy")
          ("F64 F(F64 x){F64 reg " ^ register ^ " y=x;return y;}F(1.5);")
          0x3ff8000000000000L;
        case
          ~entries:[ (0, register) ]
          ~required:[ register ] ~forbidden:[ 1 ]
          (register ^ " linked parameter")
          ("F64 F(F64 reg " ^ register
         ^ " a,F64 b){return (a+b)-a;}F(9007199254740992.0,1.0);")
          0x3ff0000000000000L;
        case ~required:[ register ] ~forbidden:[ 0; 1 ]
          (register ^ " materialized local")
          ("F64 F(F64 a,F64 b){F64 reg " ^ register
         ^ " t=a+b;return t-a;}F(9007199254740992.0,1.0);")
          0L;
      ])
    registers

let mixed_cases =
  [
    case
      ~entries:[ (1, "RSI") ]
      ~required:[ "RSI" ] ~forbidden:[ 0 ] "ordinary F64 is noncandidate"
      "F64 F(F64 x,I64 n){if(n)return x;return 2.25;}F(1.5,1);"
      0x3ff8000000000000L;
    case
      ~entries:[ (0, "RSI") ]
      ~required:[ "RSI" ] "plain reg F64 single winner"
      "F64 F(F64 reg x){return x;}F(1.5);" 0x3ff8000000000000L;
    case ~kind:"i64" ~forbidden:[ 1 ] "mixed I8 selected full word"
      "I64 F(I8 x,F64 y){x+=1;return x;}F(127,1.5);" 128L;
    case ~kind:"i64" ~entries:[] ~forbidden:[ 0; 1 ] "mixed I8 noreg width"
      "I64 F(I8 noreg x,F64 y){x+=1;return x;}F(127,1.5);" (-128L);
    case ~kind:"i64" ~forbidden:[ 0 ] "mixed I8 selected local"
      "I64 F(F64 y){I8 x=127;x+=1;return x;}F(1.5);" 128L;
    case ~kind:"i64" ~entries:[] ~forbidden:[ 0 ] "mixed I8 noreg local"
      "I64 F(F64 y){I8 noreg x=127;x+=1;return x;}F(1.5);" (-128L);
    case ~kind:"i64"
      ~entries:[ (0, "R12"); (1, "R13") ]
      ~required:[ "R12"; "R13" ] "explicit mixed widths"
      "I64 F(F64 reg R12 y,I8 reg R13 x){x+=1;return x;}F(1.5,127);" 128L;
    case
      ~entries:[ (0, "R10"); (1, "RSI") ]
      ~required:[ "R10"; "RSI" ] "unused explicit F64 reserves register"
      "F64 F(F64 reg R10 unused,F64 reg x){return x;}F(2.25,1.5);"
      0x3ff8000000000000L;
  ]

let width_cases =
  List.concat_map
    (fun (type_, initial, selected, stored) ->
      [
        case ~kind:"i64"
          ~entries:[ (0, "RSI") ]
          ~required:[ "RSI" ] ~forbidden:[ 1 ]
          (type_ ^ " mixed selected boundary")
          (Printf.sprintf "I64 F(%s x,F64 y){x+=1;return x;}F(%Ld,1.5);" type_
             initial)
          selected;
        case ~kind:"i64" ~entries:[] ~forbidden:[ 0; 1 ]
          (type_ ^ " mixed noreg boundary")
          (Printf.sprintf "I64 F(%s noreg x,F64 y){x+=1;return x;}F(%Ld,1.5);"
             type_ initial)
          stored;
      ])
    [
      ("I8", 127L, 128L, -128L);
      ("U8", 255L, 256L, 0L);
      ("I16", 32767L, 32768L, -32768L);
      ("U16", 65535L, 65536L, 0L);
      ("I32", 2147483647L, 2147483648L, -2147483648L);
      ("U32", 4294967295L, 4294967296L, 0L);
    ]

let branch_pressure_source n =
  let params = List.init n (fun i -> "F64 reg f" ^ string_of_int i) in
  "I64 F("
  ^ String.concat "," (params @ [ "I8 x"; "I64 noreg branch" ])
  ^ "){if(branch)x++;else x--;return x;}F("
  ^ String.concat "," (List.init n (fun _ -> "1.5") @ [ "127"; "1" ])
  ^ ");"

(* Nine unused forced F64 members keep seed=I64_MAX/2, while x scores six
   from two branch updates and the return. QSort2b gives
   f6,f5,f8,f7,f4,f1,f0,f3,f2,x; the first eight consume both banks.
   f2 and x spill, so the reached I8 increment stores -128, not 128. *)
let branch_pressure =
  case ~kind:"i64"
    ~entries:
      [
        (6, "R12");
        (5, "R13");
        (8, "RSI");
        (7, "RDI");
        (4, "R10");
        (1, "R11");
        (0, "R14");
        (3, "R15");
      ]
    ~required:registers ~forbidden:[ 2; 9; 10 ]
    "whole-function branch pressure retains unused forced F64 seeds"
    (branch_pressure_source 9) (-128L)

let all_explicit =
  let names = List.mapi (fun i r -> ("x" ^ string_of_int i, r)) registers in
  case
    ~entries:(List.mapi (fun i r -> (i, r)) registers)
    ~required:registers "all eight explicit F64 registers"
    ("F64 F("
    ^ String.concat ","
        (List.map (fun (n, r) -> "F64 reg " ^ r ^ " " ^ n) names)
    ^ "){return "
    ^ String.concat "+" (List.map fst names)
    ^ ";}F(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0);")
    0x4042000000000000L

let call_cases =
  List.concat_map
    (fun register ->
      [
        case
          ~entries:[ (0, register) ]
          ~required:[ register ]
          (register ^ " preserved across child")
          ("F64 Child(F64 reg " ^ register ^ " x){return x;}F64 F(F64 reg "
         ^ register ^ " x){F64 y=Child(2.25);return x;}F(1.5);")
          0x3ff8000000000000L;
        case
          ~entries:[ (1, register) ]
          ~required:[ register ] (register ^ " recursion")
          ("F64 F(I64 noreg n,F64 reg " ^ register
         ^ " x){F64 y=0.0;if(n){y=F(n-1,2.25);return x;}return x;}F(3,1.5);")
          0x3ff8000000000000L;
      ])
    [ "R10"; "R11" ]

(* Raw payload injection is deliberately unavailable: the public native API
   executes authenticated whole-source entries, not arbitrary argument words.
   These sources exercise two constructible payloads only. Repeated exact
   halving gives 2^-1023 (binary64 0008000000000000). BackFB.HC ICFDiv emits
   x87 division; FNINIT masks invalid, so dynamic 0/0 produces the x87
   indefinite quiet NaN, binary64 fff8000000000000 after FSTP64. Neither case
   claims arbitrary NaN-payload or signaling-NaN transport qualification. *)
let payload_cases =
  List.concat_map
    (fun register ->
      let transport =
        "F64 Copy(F64 x){F64 reg " ^ register ^ " y=x;return y;}F64 F(F64 reg "
        ^ register ^ " x){return Copy(x);}"
      in
      [
        case
          ~entries:[ (0, register) ]
          ~required:[ register ]
          (register ^ " subnormal parameter local call return")
          ("F64 Tiny(F64 x,I64 n){while(n){x=x/2.0;n--;}return x;}" ^ transport
         ^ "F(Tiny(1.0,1023));")
          0x0008000000000000L;
        case
          ~entries:[ (0, register) ]
          ~required:[ register ]
          (register ^ " quiet NaN parameter local call return")
          ("F64 MakeNaN(F64 a,F64 b){return a/b;}" ^ transport
         ^ "F(MakeNaN(0.0,0.0));")
          0xfff8000000000000L;
      ])
    registers

let cases =
  explicit_cases @ mixed_cases @ width_cases
  @ [ all_explicit; branch_pressure ]
  @ call_cases @ payload_cases

let excluded =
  [
    ( "mixed duplicate explicit register",
      "F64 F(F64 reg R10 x,I64 reg R10 n){return x;}F(1.5,1);" );
    ( "mixed duplicate explicit local",
      "F64 F(F64 x){F64 reg R11 y=x;I64 reg R11 n=1;return y;}F(1.5);" );
    ("F64 update", "F64 F(F64 reg RSI x){x+=1.0;return x;}F(1.5);");
    ("unsupported register", "F64 F(F64 reg RAX x){return x;}F(1.5);");
  ]

let directive_placement mode (case : case) body =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let label = case.label ^ " " ^ mode ^ " directive" in
  let diagnostics_checked = function
    | Ok value -> value
    | Error diagnostics ->
        failwith
          (label ^ ": "
          ^ String.concat "; "
              (List.map
                 (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                 diagnostics))
  in
  let image_checked = function
    | Ok value -> value
    | Error errors ->
        failwith
          (label ^ ": "
          ^ String.concat "; "
              (List.map (fun (e : Image.error) -> e.message) errors))
  in
  let layout =
    Image.create_task_layout_with_literals ~max_global_bytes:4096
      ~max_literal_bytes:4096
    |> image_checked
  in
  let arena = Runtime.create_task_arena layout |> checked in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked in
  let observed = ref false in
  let execute request =
    let image =
      Image.compile_task_command ~max_code_bytes:1_048_576 ~layout request
      |> image_checked
    in
    let source_callback =
      Option.map
        (fun callback scope operation ->
          Some (callback scope operation |> checked |> diagnostics_checked))
        (Image.source_callback image)
    in
    let retained = Runtime.retain_task_fragment arena image |> checked in
    let report =
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked)
        (fun () ->
          Runtime.execute_retained_budget_report ?source_callback budget
            retained)
    in
    let result = Runtime.outcome report |> checked in
    let sections =
      Image.native_sections image |> Holyc_lib__Asm.Native_sections.to_yojson
    in
    let has_f =
      sections |> member "symbols" |> to_list
      |> List.exists (fun symbol -> member "name" symbol |> to_string = "F")
    in
    let captured_expected =
      match result with
      | Image.Completed execution ->
          Runtime.value_captured report
          && Option.map
               (fun (word : Image.word) -> word.bits)
               execution.final_value
             = Some case.bits
      | Image.Fault _ -> false
    in
    if has_f && captured_expected then (
      placement case image;
      observed := true);
    match result with
    | Image.Completed execution ->
        Ok
          (if Runtime.value_captured report then
             Task.Native_dispatch.Captured
               (Option.map
                  (fun (word : Image.word) ->
                    match word.type_ with
                    | Image.I64 -> Task.Native_dispatch.I64 word.bits
                    | Image.U64 -> Task.Native_dispatch.U64 word.bits
                    | Image.F64 -> Task.Native_dispatch.F64 word.bits)
                  execution.final_value)
           else Task.Native_dispatch.Unchanged)
    | Image.Fault _ -> failwith (label ^ ": unexpected native directive fault")
  in
  Fun.protect
    ~finally:(fun () -> Runtime.release_task_arena arena |> checked)
    (fun () ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"scored-register-options.hc"
          ~contents:("#exe {" ^ body ^ "}")
      in
      let config =
        Preprocessor.Config.create
          ~compilation_mode:
            (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
          ()
        |> checked
      in
      let report =
        Source.run
          ~native_dispatch:
            {
              Task.Native_dispatch.execute_command = execute;
              execute_initializer =
                (fun _ -> failwith (label ^ ": unexpected initializer"));
            }
          session ~source ~config ~max_steps:100_000
      in
      ignore (Source.outcome report |> diagnostics_checked);
      require !observed (label ^ ": original native F was never captured");
      require
        ((Runtime.budget_progress budget).error = None)
        (label ^ ": source callback left an unverified budget"))

let option_cases =
  [
    ( case
        ~entries:[ (0, "R12") ]
        ~required:[ "R12" ] "explicit F64 under NO_REG_VAR" ""
        0x3ff8000000000000L,
      "Option(34,1);F64 F(F64 reg R12 x){return x;}F(1.5);" );
    ( case
        ~entries:[ (0, "R12") ]
        ~required:[ "R12" ] ~forbidden:[ 1 ]
        "unused explicit reserves under NO_REG_VAR" "" 0x3ff8000000000000L,
      "Option(34,1);F64 F(F64 reg R12 unused,F64 reg x){return x;}F(2.25,1.5);"
    );
    ( case ~entries:[] ~forbidden:[ 0 ]
        "plain reg F64 proven stack under NO_REG_VAR" "" 0x3ff8000000000000L,
      "Option(34,1);F64 F(F64 reg x){return x;}F(1.5);" );
  ]

let retained mode =
  let source =
    "F64 F(F64 reg R10 a,F64 reg R11 b){return \
     (a+b)-a;}F(9007199254740992.0,1.0);"
  in
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
        "one-below placement compile quota accepted")
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
      let steps = run 100000 |> completed 0x3ff0000000000000L in
      for _ = 1 to 3 do
        ignore (run steps |> completed 0x3ff0000000000000L)
      done;
      let failed = run (steps - 1) |> fault Image.Step_limit_exceeded in
      require
        (failed.executed_steps = steps - 1 && Option.is_some failed.span)
        "placed quota lost source/charge";
      ignore (run ~max_frame_bytes:16 steps |> completed 0x3ff0000000000000L);
      ignore (run ~max_frame_bytes:15 steps |> fault Image.Frame_limit_exceeded);
      ignore (run steps |> completed 0x3ff0000000000000L));
  let source =
    "I64 G=0;I64 Next(){return G++;}F64 F(F64 reg R10 a,F64 reg R11 b){I64 \
     z=Next();I64 q=1/z;return (a+b)-a;}F(9007199254740992.0,1.0);"
  in
  let retained = image mode source |> Runtime.retain |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () ->
      let run () = Runtime.execute_retained_report ~max_steps:100000 retained in
      ignore (run () |> fault Image.Division_by_zero);
      ignore (run () |> completed 0x3ff0000000000000L);
      ignore (run () |> completed 0x3ff0000000000000L))

let source_isolation mode =
  let make register =
    case
      ~entries:[ (0, register) ]
      ~required:[ register ]
      ("source-isolated " ^ register)
      ("F64 F(F64 reg " ^ register ^ " x){return x;}F(1.5);")
      0x3ff8000000000000L
  in
  let first_case = make "R10" and second_case = make "R11" in
  let first = image mode first_case.source in
  let saved_code = Image.code first in
  let copied_source = Bytes.of_string first_case.source in
  Bytes.fill copied_source 0 (Bytes.length copied_source) ' ';
  let second = image mode second_case.source in
  placement first_case first;
  placement second_case second;
  require
    (Image.code first = saved_code)
    "later source owner changed earlier physical placement";
  if native then (
    ignore
      (Runtime.execute_report ~max_steps:100000 first
      |> completed first_case.bits);
    ignore
      (Runtime.execute_report ~max_steps:100000 second
      |> completed second_case.bits))

let failures = ref []

let check label action =
  try action ()
  with error ->
    failures := (label ^ ": " ^ Printexc.to_string error) :: !failures

let () =
  List.iter
    (fun mode ->
      check (mode ^ " source isolation") (fun () -> source_isolation mode);
      List.iter
        (fun case ->
          check
            (mode ^ " " ^ case.label)
            (fun () ->
              let compiled = image mode case.source in
              placement case compiled;
              if
                String.ends_with ~suffix:"parameter local call return"
                  case.label
              then
                placement ~name:"Copy"
                  { case with entries = Some []; forbidden = [ 0 ] }
                  compiled;
              if native then
                List.iter
                  (fun target ->
                    if
                      Native_execution.x87_supported ()
                      || not (Image.requires_x87 compiled)
                    then
                      invoke ~target ~mode case.label case.source |> value case
                    else
                      invoke ~status:1 ~target ~mode case.label case.source
                      |> rejected "x87 capability")
                  targets))
        cases;
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
      if native then (
        (* This original native-dispatch observer is a JIT-only public API;
           ordinary CLI cases above still run both outer compilation modes. *)
        if mode = "jit" then
          List.iter
            (fun (case, body) ->
              check
                (mode ^ " " ^ case.label)
                (fun () -> directive_placement mode case body))
            option_cases;
        if Native_execution.x87_supported () then
          check (mode ^ " retained") (fun () -> retained mode)))
    modes;
  Printf.printf "native F64 registers: %d CLI reports (%s), %d failures\n"
    !count
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
