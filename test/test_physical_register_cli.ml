open Yojson.Safe.Util
open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* Pinned TempleOS PrsVar.HC:449-459 records the explicitly named register.
   OptPass3.HC:18-51 preserves it, while the register-variable arithmetic in
   that pass uses a full word even for narrow integer declarations. Parameter
   entry still performs the declared-width conversion. These are source-derived
   expectations, not a new TempleOS runtime or byte-for-byte compiler oracle. *)
let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-physical-register-" suffix in
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
let reports = ref 0
let placements = ref 0
let retained_recoveries = ref 0
let registers = [ "R12"; "R13"; "R14"; "R15" ]
let modes = [ "jit"; "aot" ]

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
              let text = read output and error_text = read errors in
              require
                (actual = Unix.WEXITED status && error_text = "")
                (label ^ " " ^ target ^ " " ^ mode ^ ": stdout=" ^ text
               ^ "; stderr=" ^ error_text);
              Yojson.Safe.from_string text)))

let value label expected report =
  require
    (member "outcome" report = `String "success"
    && member "bits" (member "final_value" report)
       = `String (Printf.sprintf "0x%016Lx" expected))
    (label ^ ": " ^ Yojson.Safe.to_string report)

let fault label code report =
  require
    (member "outcome" report = `String "error"
    && report |> member "diagnostics" |> to_list
       |> List.exists (fun diagnostic ->
           member "code" diagnostic = `String code))
    (label ^ ": " ^ Yojson.Safe.to_string report)

type case = { name : string; source : string; expected : int64 }

let case name source expected = { name; source; expected }

let integer_types =
  [
    ("I8", 127L, 255L, -1L);
    ("U8", 255L, 511L, 255L);
    ("I16", 32767L, 65535L, -1L);
    ("U16", 65535L, 131071L, 65535L);
    ("I32", 2147483647L, 4294967295L, -1L);
    ("U32", 4294967295L, 8589934591L, 4294967295L);
    ("I64", 0x7ffffffffffffffdL, Int64.min_int, Int64.min_int);
    ("U64", 0xfffffffffffffffdL, -1L, -1L);
  ]

let width_cases =
  List.concat_map
    (fun register ->
      List.concat_map
        (fun (type_, high, incoming, converted) ->
          [
            case
              (type_ ^ " " ^ register ^ " parameter update")
              (Printf.sprintf "I64 F(%s reg %s x){x+=3;return x;}F(0x%Lx);"
                 type_ register high)
              (Int64.add high 3L);
            case
              (type_ ^ " " ^ register ^ " parameter conversion")
              (Printf.sprintf "I64 F(%s reg %s x){x+=3;return x;}F(0x%Lx);"
                 type_ register incoming)
              (Int64.add converted 3L);
            case
              (type_ ^ " " ^ register ^ " local word initialization")
              (Printf.sprintf
                 "I64 F(I64 seed){%s reg %s x=seed;x+=3;return x;}F(0x%Lx);"
                 type_ register incoming)
              (Int64.add incoming 3L);
          ])
        integer_types)
    registers

let register_cases =
  List.concat_map
    (fun (register, next) ->
      [
        case
          (register ^ " arithmetic updates")
          (Printf.sprintf
             "I64 F(I64 reg %s x,I64 y){I64 noreg two=2;I64 noreg three=3;I64 \
              noreg rem=17;I64 noreg mask=31;I64 noreg \
              toggle=7;x+=y;x-=1;x*=3;x/=two;x%%=rem;x<<=2;x>>=1;x|=three;x&=mask;x^=toggle;return \
              x;}F(10,5);"
             register)
          12L;
        case
          (register ^ " discarded prefix and postfix updates")
          (Printf.sprintf
             "I64 F(I64 reg %s x){I64 noreg a=x;x++;++x;I64 noreg b=x;I64 \
              noreg c=x;x--;--x;I64 noreg d=x;return x+a+b+c+d;}F(7);"
             register)
          39L;
        case
          (register ^ " same-register child")
          (Printf.sprintf
             "I64 Child(I64 reg %s y){y*=3;return y+1;}I64 F(I64 reg %s \
              x){return x+Child(x+2)+x;}F(7);"
             register register)
          42L;
        case
          (register ^ " different-register child")
          (Printf.sprintf
             "I64 Child(I64 reg %s y){y*=3;return y+1;}I64 F(I64 reg %s \
              x){return x+Child(x+2)+x;}F(7);"
             next register)
          42L;
        case
          (register ^ " recursive parameter")
          (Printf.sprintf
             "I64 F(I64 reg %s x){if(x<=0)return 1;return F(x-1)+x;}F(6);"
             register)
          22L;
        case
          (register ^ " recursive local")
          (Printf.sprintf
             "I64 F(I64 n){I64 reg %s x=n+10;if(n)return F(n-1)+x;return \
              x;}F(3);"
             register)
          46L;
        case
          (register ^ " loop and branch merges")
          (Printf.sprintf
             "I64 F(I64 reg %s x,I64 n){I64 noreg \
              i=0;while(i<n){if(i&1)x-=i;else x+=i;i++;}return x;}F(40,5);"
             register)
          42L;
        case
          (register ^ " early return and later invocation")
          (Printf.sprintf
             "I64 F(I64 reg %s x,I64 n){while(n){x++;if(x==21)return \
              x;n--;}return x;}F(19,8)+F(40,2)-21;"
             register)
          42L;
        case
          (register ^ " scalar stack neighbor")
          (Printf.sprintf
             "I64 F(I64 reg %s x){I64 noreg y=5;y+=2;x+=y;return x+y;}F(28);"
             register)
          42L;
      ])
    [ ("R12", "R13"); ("R13", "R14"); ("R14", "R15"); ("R15", "R12") ]

let recursive_source =
  "I64 F(I64 reg R12 x,I64 reg R13 n){I64 reg R14 a=x+2;I64 reg R15 \
   b=x+3;if(n)return F(x+1,n-1)+x+a+b;return x+a+b;}F(1,3);"

let all_register_cases =
  [
    case "four live parameters"
      "I64 F(I64 reg R12 a,I64 reg R13 b,I64 reg R14 c,I64 reg R15 \
       d){a+=b;b+=c;c+=d;d+=a;return a+b+c+d;}F(1,2,3,4);"
      22L;
    case "four live locals"
      "I64 F(I64 seed){I64 reg R12 a=seed;I64 reg R13 b=a+1;I64 reg R14 \
       c=b+1;I64 reg R15 d=c+1;a+=b;b+=c;c+=d;d+=a;return a+b+c+d;}F(1);"
      22L;
    case "four live words survive repeated all-register callees"
      "I64 Child(I64 reg R12 a,I64 reg R13 b,I64 reg R14 c,I64 reg R15 \
       d){a*=2;b*=3;c*=4;d*=5;return a+b+c+d;}I64 F(I64 reg R12 a,I64 reg R13 \
       b,I64 reg R14 c,I64 reg R15 d){return \
       a+b+c+d+Child(d,c,b,a)+Child(a,b,c,d);}F(1,2,3,4);"
      80L;
    case "four live words survive recursion" recursive_source 50L;
    case "four-register repeated top-level calls"
      "I64 F(I64 reg R12 a,I64 reg R13 b,I64 reg R14 c,I64 reg R15 d){return \
       a+b+c+d;}I64 sum=0;I64 i;for(i=0;i<5;i++)sum+=F(i,2,3,4);sum;"
      55L;
    case "four-register branch and loop"
      "I64 F(I64 reg R12 a,I64 reg R13 b,I64 reg R14 c,I64 reg R15 d){I64 \
       noreg i=0;for(;i<4;i++){if(i&1){a+=b;c+=d;}else{b+=c;d+=a;}}return \
       a+b+c+d;}F(1,2,3,4);"
      62L;
    case "four-register mixed parameter widths"
      "I64 F(I8 reg R12 a,U16 reg R13 b,I32 reg R14 c,U64 reg R15 \
       d){a+=2;b+=2;c+=2;d+=2;return a+b+c+d;}F(127,65535,2147483647,3);"
      2147549320L;
  ]

(* Physical placement consumes the existing bounded source proof. These
   constructs remain outside it, even when their general integer semantics are
   otherwise supported. Do not silently grant a forced register on fallback. *)
let unsupported_cases =
  [
    ( "literal divide/remainder/bitwise compound operands",
      "I64 F(I64 reg R12 x,I64 \
       y){x+=y;x-=1;x*=3;x/=2;x%=17;x<<=2;x>>=1;x|=3;x&=31;x^=7;return \
       x;}F(10,5);" );
    ( "consumed unary-update initializers",
      "I64 F(I64 reg R12 x){I64 noreg a=x++;I64 noreg b=++x;I64 noreg \
       c=x--;I64 noreg d=--x;return x+a+b+c+d;}F(7);" );
    ( "addressable neighbor and pointer member",
      "I64 F(I64 reg R12 x){I64 noreg y=5;I64 *p=&y;*p+=2;x+=*p;return \
       x+y;}F(28);" );
    ( "call-valued local initializer",
      "I64 Child(I64 reg R12 y){return y+1;}I64 F(I64 reg R12 x){I64 noreg \
       saved=Child(x);return x+saved;}F(20);" );
  ]

let bounded_rejections mode =
  List.iter
    (fun (label, source) ->
      List.iter
        (fun target ->
          invoke ~status:1 ~target ~mode label source
          |> fault label "HCBACK0002")
        [ "host-jit"; "host-jit-task" ])
    unsupported_cases

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let function_instructions label name report =
  let sections =
    report |> member "native" |> member "image" |> member "native_sections"
  in
  let symbol =
    sections |> member "symbols" |> to_list
    |> List.find (fun symbol -> member "name" symbol = `String name)
  in
  let address = member "definition" symbol in
  let section_id = member "section" address |> to_int in
  let start = member "offset" address |> to_int in
  let section =
    sections |> member "sections" |> to_list
    |> List.find (fun section -> member "id" section = `Int section_id)
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

let check_placement mode register =
  let label = register ^ " actual parameter placement" in
  let source = Printf.sprintf "I64 F(I64 reg %s x){return x;}F(42);" register in
  let report = invoke ~target:"host-jit" ~mode label source in
  value label 42L report;
  let instructions = function_instructions label "F" report in
  let has_move dest source =
    List.exists
      (fun (instruction : Disassembly.instruction) ->
        instruction.opcode = "MOV"
        && List.map
             (fun (operand : Disassembly.operand) -> operand.text)
             instruction.operands
           = [ "U64 " ^ dest; "U64 " ^ source ])
      instructions
  in
  require (has_move register "RAX")
    (label ^ ": parameter never enters " ^ register);
  require (has_move "RAX" register) (label ^ ": return never reads " ^ register);
  let memory_move ~save =
    List.exists
      (fun (instruction : Disassembly.instruction) ->
        instruction.opcode = "MOV"
        &&
        match instruction.operands with
        | [ dest; source ] ->
            let memory, reg =
              if save then (dest.text, source.text) else (source.text, dest.text)
            in
            reg = "U64 " ^ register
            && String.contains memory '['
            && String.ends_with ~suffix:"[RBP]" memory
        | _ -> false)
      instructions
  in
  require (memory_move ~save:true) (label ^ ": missing nonvolatile save");
  require (memory_move ~save:false) (label ^ ": missing nonvolatile restore");
  incr placements

(* Enough ordinary locals make F, rather than its caller, determine the
   largest private frame. The otherwise-identical noreg control isolates the
   four saved nonvolatile words from ordinary parameter/local frame storage. *)
let quota_source physical =
  let parameters =
    List.mapi
      (fun index register ->
        Printf.sprintf "I64 %s %c"
          (if physical then "reg " ^ register else "noreg")
          (Char.chr (Char.code 'a' + index)))
      registers
    |> String.concat ","
  in
  let locals =
    List.init 8 (fun index ->
        Printf.sprintf "I64 noreg q%d=%c+1;" index
          (Char.chr (Char.code 'a' + (index mod 4))))
    |> String.concat ""
  in
  let result =
    "a+b+c+d+"
    ^ (List.init 8 (fun index -> "q" ^ string_of_int index) |> String.concat "+")
  in
  Printf.sprintf "I64 F(%s){%sreturn %s;}F(10,11,12,9);" parameters locals
    result

let decoded_frame_allocation label report =
  let instruction =
    function_instructions label "F" report
    |> List.find (fun (instruction : Disassembly.instruction) ->
        instruction.opcode = "SUB"
        &&
        match instruction.operands with
        | [ destination; _ ] -> destination.text = "U64 RSP"
        | _ -> false)
  in
  match instruction.operands with
  | [ _; size ] -> (
      match size.value with
      | Some bytes when bytes > 0L && bytes <= Int64.of_int max_int ->
          Int64.to_int bytes
      | _ -> failwith (label ^ ": missing positive stack allocation"))
  | _ -> failwith (label ^ ": invalid stack allocation instruction")

let exact_stack_quotas mode =
  let physical_source = quota_source true
  and stack_source = quota_source false in
  let baseline label source =
    let report = invoke ~target:"host-jit" ~mode label source in
    value label 134L report;
    let image = member "image" (member "native" report) in
    let private_bytes = member "frame_bytes" image |> to_int in
    let entry_bytes = member "entry_stack_bytes" image |> to_int in
    let function_bytes = decoded_frame_allocation label report in
    require
      (private_bytes = function_bytes)
      (label ^ ": fixture's callee must determine private-frame limit");
    (* Each callable activation adds its return address and saved RBP to its
       decoded allocation. The root's full footprint comes from image metadata. *)
    (private_bytes, entry_bytes, entry_bytes + (2 * 8) + function_bytes)
  in
  let physical_private, physical_entry, physical_active =
    baseline "physical-register quota baseline" physical_source
  and stack_private, stack_entry, stack_active =
    baseline "stack-register quota control" stack_source
  in
  let save_bytes = List.length registers * 8 in
  require
    (physical_entry = stack_entry)
    "register save slots must not change the caller's entry frame";
  require
    (physical_private = stack_private + save_bytes)
    "four register saves must add four words to the private frame";
  require
    (physical_active = stack_active + save_bytes)
    "active-stack charge must include all four register saves";
  let options private_bytes active_bytes =
    [
      "--stack-byte-limit=" ^ string_of_int private_bytes;
      "--active-stack-byte-limit=" ^ string_of_int active_bytes;
    ]
  in
  List.iter
    (fun target ->
      let succeed label source private_bytes active_bytes =
        invoke ~target ~mode
          ~options:(options private_bytes active_bytes)
          label source
        |> value label 134L
      in
      let reject label code private_bytes active_bytes =
        invoke ~status:1 ~target ~mode
          ~options:(options private_bytes active_bytes)
          label physical_source
        |> fault label code
      in
      succeed "exact private and active physical-register limits"
        physical_source physical_private physical_active;
      reject "one-below physical-register private frame" "HCBACK0004"
        (physical_private - 1) physical_active;
      reject "one-below physical-register active stack" "HCNATIVE0006"
        physical_private (physical_active - 1);
      reject "private allowance without register saves" "HCBACK0004"
        stack_private physical_active;
      reject "active allowance without register saves" "HCNATIVE0006"
        physical_private stack_active;
      succeed "exact stack-only control limits" stack_source stack_private
        stack_active)
    [ "host-jit"; "host-jit-task" ]

let checked label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ error)

let compile mode source =
  let session = Session.create () in
  let input =
    Session.add_source session ~path:"physical-register-recovery.hc"
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
        (String.concat "; "
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
  let image = compile mode source in
  let retained = Runtime.retain image |> checked "retain" in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked "release")
    (fun () -> action retained)

let execute retained max_steps =
  Runtime.execute_retained_report ~max_steps retained
  |> Runtime.outcome |> checked "execute retained"

let retained_fault_recovery mode =
  with_retained mode recursive_source (fun retained ->
      let steps =
        execute retained 100_000 |> completed "recursive baseline" 50L
      in
      let owned_faults = ref 0 in
      for limit = 1 to steps - 1 do
        (match execute retained limit with
        | Program.Fault fault ->
            require
              (fault.kind = Program.Step_limit_exceeded)
              "wrong bounded recursion fault";
            require (fault.executed_steps = limit) "wrong fault work";
            if fault.function_name = Some "F" then incr owned_faults
        | Program.Completed _ ->
            failwith "bounded recursion unexpectedly completed");
        ignore
          (execute retained 100_000 |> completed "same mapping after fault" 50L);
        incr retained_recoveries
      done;
      require (!owned_faults > 0)
        "no checked fault occurred inside register-owning function");
  with_retained mode
    "I64 Attempts=0;I64 F(I64 reg R12 x,I64 noreg count){I64 reg R13 y=x+1;I64 \
     reg R14 z=x+2;I64 reg R15 w=x+3;return \
     (x+y+z+w)/(count-1);}Attempts++;F(9,Attempts);" (fun retained ->
      (match execute retained 100_000 with
      | Program.Fault fault ->
          require
            (fault.kind = Program.Division_by_zero
            && fault.function_name = Some "F")
            "expected division fault inside four-register function"
      | Program.Completed _ ->
          failwith "first retained invocation did not divide by zero");
      ignore
        (execute retained 100_000
        |> completed "same mapping after division fault" 42L);
      ignore
        (execute retained 100_000 |> completed "third retained invocation" 21L);
      incr retained_recoveries)

let () =
  let cases =
    width_cases @ register_cases @ all_register_cases
    @ [
        case "maintained physical register call example"
          (read "../examples/physical-register-calls.hc")
          42L;
      ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun case ->
          let baseline = invoke ~target:"ir" ~mode case.name case.source in
          value case.name case.expected baseline;
          if native then
            List.iter
              (fun target ->
                let report = invoke ~target ~mode case.name case.source in
                value (case.name ^ " " ^ target) case.expected report;
                require
                  (member "final_value" report
                  |> member "bits"
                  = (member "final_value" baseline |> member "bits"))
                  (case.name ^ ": IR/native bits differ"))
              [ "host-jit"; "host-jit-task" ])
        cases;
      List.iter
        (fun register ->
          List.iter
            (fun target ->
              let label = register ^ " recursive division fault" in
              let source =
                Printf.sprintf
                  "I64 F(I64 reg %s x){if(x)return F(x-1)+x;return 42/x;}F(3);"
                  register
              in
              invoke ~status:1 ~target ~mode label source
              |> fault label "HCIRVM0009";
              invoke ~target ~mode "following invocation" recursive_source
              |> value "following invocation" 50L)
            (if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]))
        registers;
      if native then (
        List.iter (check_placement mode) registers;
        exact_stack_quotas mode;
        bounded_rejections mode;
        retained_fault_recovery mode))
    modes;
  Printf.printf
    "Physical register checks passed (%d CLI reports, %d decoded placements, \
     %d same-mapping fault recoveries).\n"
    !reports !placements !retained_recoveries
