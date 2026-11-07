open Yojson.Safe.Util
open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* This exercises the conservative fallback proof outside the scored subset,
   not a general allocator
   or a byte-for-byte TempleOS compiler oracle. OptPass3.HC:22-48 reserves
   explicit registers even when unused and seeds plain reg with I64_MAX/2.
   CInit.HC:244-256 and OptPass3.HC:557-601 establish the first free general
   slot only while at most six possible candidates remain. *)
let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-automatic-register-" suffix in
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
let exclusions = ref 0
let supported = [ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15" ]
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
    (Program.native_sections image |> Holyc_lib__Asm.Native_sections.to_yojson)

let assert_placement label register instructions =
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
    (label ^ ": source word never enters " ^ register);
  require
    (List.exists
       (fun (instruction : Disassembly.instruction) ->
         instruction.opcode = "MOV"
         &&
         match instruction.operands with
         | [ destination; source ] ->
             source.text = "U64 " ^ register
             && not (String.contains destination.text '[')
         | _ -> false)
       instructions)
    (label ^ ": expression never reads " ^ register);
  let memory_moves ~save =
    List.filter
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
  let saves = memory_moves ~save:true and restores = memory_moves ~save:false in
  require (List.length saves = 1) (label ^ ": expected one nonvolatile save");
  require
    (List.length restores = 1)
    (label ^ ": expected one nonvolatile restore");
  let rex = if register = "RSI" || register = "RDI" then 0x48 else 0x4c in
  List.iter
    (fun (instruction : Disassembly.instruction) ->
      require
        (String.length instruction.bytes >= 3
        && Char.code instruction.bytes.[0] = rex)
        (label ^ ": save/restore has the wrong REX.W/R prefix"))
    (saves @ restores);
  incr placements

let checked label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ error)

let compile mode source =
  let session = Session.create () in
  let input =
    Session.add_source session ~path:"automatic-register-placement.hc"
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
  let image = compile mode source in
  let retained = Runtime.retain image |> checked "retain" in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked "release")
    (fun () -> action image retained)

let execute retained max_steps =
  Runtime.execute_retained_report ~max_steps retained
  |> Runtime.outcome |> checked "execute retained"

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

let assert_bytes label register instructions =
  assert_placement label register instructions;
  let number =
    List.assoc register
      [
        ("RSI", 6);
        ("RDI", 7);
        ("R10", 10);
        ("R11", 11);
        ("R14", 14);
        ("R15", 15);
      ]
  in
  let entry =
    Printf.sprintf "%02x89%02x"
      (0x48 lor (number lsr 3))
      (0xc0 lor (number land 7))
  in
  require
    (List.exists
       (fun (instruction : Disassembly.instruction) ->
         instruction.bytes = bytes entry)
       instructions)
    (label ^ ": missing actual RAX-to-" ^ register ^ " entry/write bytes");
  require
    (List.exists
       (fun (instruction : Disassembly.instruction) ->
         let code = instruction.bytes in
         instruction.opcode = "MOV"
         && String.length code = 3
         && Char.code code.[0] land 0xfe = 0x48 lor ((number land 8) lsr 1)
         && Char.code code.[1] = 0x89
         && Char.code code.[2] land 0xf8 = 0xc0 lor ((number land 7) lsl 3))
       instructions)
    (label ^ ": missing actual expression-read bytes for " ^ register);
  let save = List.hd (frame_moves ~save:true register instructions)
  and restore = List.hd (frame_moves ~save:false register instructions) in
  List.iter
    (fun (opcode, instruction) ->
      let code = instruction.Disassembly.bytes in
      let rex = 0x48 lor ((number land 8) lsr 1) in
      require
        (String.length code >= 4
        && Char.code code.[0] = rex
        && Char.code code.[1] = opcode
        && Char.code code.[2] land 0x3f = 0x05 lor ((number land 7) lsl 3))
        (label ^ ": wrong source register in actual save/restore bytes"))
    [ (0x89, save); (0x8b, restore) ];
  require
    (String.sub save.bytes 2 (String.length save.bytes - 2)
    = String.sub restore.bytes 2 (String.length restore.bytes - 2))
    (label ^ ": restore does not address its original save slot")

let assert_saved_registers label expected instructions =
  let actual =
    List.filter
      (fun register -> frame_moves ~save:true register instructions <> [])
      supported
  in
  require
    (List.sort compare actual = List.sort compare expected)
    (label ^ ": source-register saves were " ^ String.concat "," actual
   ^ ", expected " ^ String.concat "," expected);
  if expected = [] then incr exclusions

type case = {
  name : string;
  source : string;
  expected : int64;
  register : string option;
  saved : string list;
  output : string;
  children : (string * string) list;
  directive_body : string option;
  options : string list;
}

let case ?register ?(saved = []) ?(output = "") ?(children = []) ?directive_body
    ?(options = []) name source expected =
  let saved = Option.fold ~none:saved ~some:(fun r -> r :: saved) register in
  {
    name;
    source;
    expected;
    register;
    saved;
    output;
    children;
    directive_body;
    options;
  }

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
    (fun qualifier ->
      List.concat_map
        (fun (type_, high, incoming, converted) ->
          let label = type_ ^ " " ^ qualifier in
          [
            case ~register:"RSI"
              (label ^ " parameter word update")
              (Printf.sprintf "I64 F(%s %s x){x+=3;return x;}F(0x%Lx);" type_
                 qualifier high)
              (Int64.add high 3L);
            case ~register:"RSI"
              (label ^ " declared parameter entry width")
              (Printf.sprintf "I64 F(%s %s x){x+=3;return x;}F(0x%Lx);" type_
                 qualifier incoming)
              (Int64.add converted 3L);
            case ~register:"RSI"
              (label ^ " local word initialization")
              (Printf.sprintf
                 "I64 F(I64 noreg seed){%s %s x=seed;x+=3;return x;}F(0x%Lx);"
                 type_ qualifier incoming)
              (Int64.add incoming 3L);
          ])
        integer_types)
    [ ""; "reg" ]

let reservation_sets =
  [
    ("RSI", []);
    ("RDI", [ "RSI" ]);
    ("R10", [ "RSI"; "RDI" ]);
    ("R11", [ "RSI"; "RDI"; "R10" ]);
    ("R14", [ "RSI"; "RDI"; "R10"; "R11" ]);
    ("R15", [ "RSI"; "RDI"; "R10"; "R11"; "R14" ]);
  ]

let reserved_parameters registers =
  List.mapi
    (fun index register ->
      Printf.sprintf "I64 reg %s reserved%d," register index)
    registers
  |> String.concat ""

let reserved_arguments registers =
  String.concat "" (List.map (fun _ -> "0,") registers)

let recursive_source registers =
  Printf.sprintf "I64 F(%sI64 x){if(x<=0)return 1;return F(%sx-1)+x;}F(%s6);"
    (reserved_parameters registers)
    (reserved_arguments registers)
    (reserved_arguments registers)

let reservation_cases =
  List.concat_map
    (fun (register, reservations) ->
      [
        case ~register
          (register ^ " follows unused explicit parameters")
          (Printf.sprintf "I64 F(%sI64 x){x+=3;return x;}F(%s39);"
             (reserved_parameters reservations)
             (reserved_arguments reservations))
          42L;
        case ~register
          (register ^ " sole local after unused reservations")
          (Printf.sprintf
             "I64 F(%sI64 noreg seed){I64 x=seed;x+=3;return x;}F(%s39);"
             (reserved_parameters reservations)
             (reserved_arguments reservations))
          42L;
        case ~register
          (register ^ " recursive automatic parameter")
          (recursive_source reservations)
          22L;
      ])
    reservation_sets
  @ [
      case ~register:"RSI" "R12/R13 reservations do not consume general slots"
        "I64 F(I64 reg R12 a,I64 reg R13 b,I64 x){return x;}F(0,0,42);" 42L;
      case ~register:"RDI" ~saved:[ "RSI" ]
        "used explicit RSI before automatic RDI"
        "I64 F(I64 reg RSI a,I64 x){return x+a;}F(12,30);" 42L;
    ]

let priority_cases =
  [
    case ~register:"RSI"
      "branch fallback keeps sole forced parameter ahead of five ordinary \
       candidates"
      "I64 F(I64 a,I64 b,I64 c,I64 d,I64 e,I64 reg x){if(a<0)return x;return \
       x+a+b+c+d+e;}F(1,2,3,4,5,27);"
      42L;
    case ~register:"RSI"
      "sole forced parameter with five unused ordinary members"
      "I64 F(I64 a,I64 b,I64 c,I64 d,I64 e,I64 reg x){return \
       x;}F(1,2,3,4,5,42);"
      42L;
    case ~register:"RSI"
      "branch fallback keeps sole forced local ahead of ordinary members"
      "I64 F(I64 a){I64 b=a;I64 c=a;I64 d=a;I64 e=a;I64 reg \
       x=a+1;if(a<0)return x;return x+a+b+c+d+e;}F(7);"
      43L;
    case ~register:"RDI" "branch fallback respects unused explicit reservation"
      "I64 F(I64 reg RSI reserved,I64 a,I64 reg x){if(a<0)return x;return \
       x+a;}F(0,12,30);"
      42L;
    case ~register:"RSI"
      "branch fallback keeps forced target ahead of unknown operand"
      "I64 F(I8 reg x,I64 count){if(count<0)return x;x<<=count;return \
       x;}F(127,1);"
      254L;
    case ~register:"RSI"
      ~directive_body:
        "Option(34,0);I64 F(I8 x){x+=3;return x;}Option(34,1);F(127);"
      "later NO_REG_VAR change does not disable original selection"
      "#exe {Option(34,0);I64 F(I8 x){x+=3;return \
       x;}Option(34,1);StreamPrint(\"%d;\",F(127));}"
      130L;
  ]

let negative_cases =
  [
    case "branch fallback does not invent a two-member tie order"
      "I64 F(I64 x,I64 y){if(x<0)return y;return x+y;}F(20,22);" 42L;
    case "branch fallback does not invent unequal scores"
      "I64 F(I64 x,I64 y){if(x<0)return y;return x+x+x+y;}F(10,12);" 42L;
    case "branch fallback does not invent a forced tie order"
      "I64 F(I64 reg x,I64 reg y){if(x<0)return y;return x+y;}F(20,22);" 42L;
    case "branch fallback counts unused forced member"
      "I64 F(I64 reg x,I64 reg unused){if(x<0)return 0;return x;}F(42,0);" 42L;
    case "branch fallback counts unused ordinary member"
      "I64 F(I64 x,I64 unused){if(x<0)return 0;return x;}F(42,0);" 42L;
    case "branch fallback counts unused initialized local"
      "I64 F(){I64 unused=0;I64 x=42;if(x<0)return 0;return x;}F();" 42L;
    case "branch fallback counts specialized operand"
      "I64 F(I8 x,I64 count){if(count<0)return 0;x<<=count;return x;}F(127,1);"
      254L;
    case "branch fallback counts six unused ordinary members"
      "I64 F(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 reg x){if(x<0)return \
       0;return x;}F(0,0,0,0,0,0,42);"
      42L;
    case "branch fallback counts six unused initialized locals"
      "I64 F(){I64 a=0;I64 b=0;I64 c=0;I64 d=0;I64 e=0;I64 f=0;I64 reg \
       x=42;if(x<0)return 0;return x;}F();"
      42L;
    case
      "branch fallback counts six unused members beside narrow forced candidate"
      "I64 F(I8 reg x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f){if(x<0)return \
       0;x<<=1;return x;}F(127,0,0,0,0,0,0);"
      254L;
    case "noreg remains a declared-width stack parameter"
      "I64 F(I8 noreg x){x+=3;return x;}F(127);" (-126L);
    case ~directive_body:"Option(34,1);I64 F(I8 x){x+=3;return x;}F(127);"
      "NO_REG_VAR disables automatic placement"
      "#exe {Option(34,1);I64 F(I8 x){x+=3;return \
       x;}StreamPrint(\"%d;\",F(127));}"
      (-126L);
    case ~directive_body:"Option(34,1);I64 F(I8 reg x){x+=3;return x;}F(127);"
      "NO_REG_VAR disables sole plain reg placement"
      "#exe {Option(34,1);I64 F(I8 reg x){x+=3;return \
       x;}StreamPrint(\"%d;\",F(127));}"
      (-126L);
    case
      ~directive_body:
        "Option(34,1);I64 F(I8 x){x+=3;return x;}Option(34,0);F(127);"
      "later NO_REG_VAR change does not replace original stack selection"
      "#exe {Option(34,1);I64 F(I8 x){x+=3;return \
       x;}Option(34,0);StreamPrint(\"%d;\",F(127));}"
      (-126L);
    case ~register:"R10" "first free R10 is physically selected"
      "I64 F(I64 reg RSI a,I64 reg RDI b,I64 x){return x;}F(0,0,42);" 42L;
    case ~register:"R11" "first free R11 is physically selected"
      "I64 F(I64 reg RSI a,I64 reg RDI b,I64 reg R10 c,I64 x){return \
       x;}F(0,0,0,42);"
      42L;
    case "exhausted general bank does not invent a later register"
      "I64 F(I64 reg RSI a,I64 reg RDI b,I64 reg R10 c,I64 reg R11 d,I64 reg \
       R14 e,I64 reg R15 f,I64 x){return x;}F(0,0,0,0,0,0,42);"
      42L;
  ]

let control_cases =
  [
    case ~register:"RSI" "recursive local with stack-only depth"
      "I64 F(I64 noreg n){I64 x=n+10;if(n)return F(n-1)+x;return x;}F(3);" 46L;
    case ~register:"RSI"
      ~children:[ ("Child", "RSI") ]
      "automatic value survives same-register child"
      "I64 Child(I64 y){y*=3;return y+1;}I64 F(I64 x){return \
       x+Child(x+2)+x;}F(7);"
      42L;
    case ~register:"RSI"
      ~children:[ ("Child", "RDI") ]
      "automatic value survives different-register child"
      "I64 Child(I64 reg RSI reserved,I64 y){y*=3;return y+1;}I64 F(I64 \
       x){return x+Child(0,x+2)+x;}F(7);"
      42L;
    case ~register:"RSI" "automatic loop updates and branch merges"
      "I64 F(I64 x,I64 noreg n){I64 noreg i=0;while(i<n){if(i&1)x-=i;else \
       x+=i;i++;}return x;}F(40,5);"
      42L;
    case ~register:"RSI" "automatic early return and repeated invocation"
      "I64 F(I64 x,I64 noreg n){while(n){x++;if(x==21)return x;n--;}return \
       x;}F(19,8)+F(40,2)-21;"
      42L;
    case ~register:"RSI" "automatic value survives ordinary addressable frame"
      "I64 Bridge(I64 n){I64 local=n;I64 *p=&local;*p+=1;return *p-1;}I64 \
       F(I64 x){return x+Bridge(x)+x;}F(14);"
      42L;
    case ~register:"RSI" ~output:"A14;"
      ~options:[ "--code-byte-limit=1048576" ]
      "automatic value survives Print host bridge"
      "extern U0 Print(U8 *fmt,...);I64 Bridge(I64 n){Print(\"A%d;\",n);return \
       n;}I64 F(I64 x){return x+Bridge(x)+x;}F(14);"
      42L;
    case ~register:"RDI" ~output:"A14;"
      ~options:[ "--code-byte-limit=1048576" ]
      "automatic RDI survives Print host bridge"
      "extern U0 Print(U8 *fmt,...);I64 Bridge(I64 n){Print(\"A%d;\",n);return \
       n;}I64 F(I64 reg RSI reserved,I64 x){return x+Bridge(x)+x;}F(0,14);"
      42L;
    case ~register:"RSI"
      ~children:[ ("Child", "RDI") ]
      "maintained automatic placement example"
      (read "../examples/automatic-register-placement.hc")
      42L;
  ]

let retained_fault_recovery mode =
  List.iter
    (fun (register, reservations) ->
      let label = register ^ " retained automatic recursion" in
      with_retained mode (recursive_source reservations) (fun image retained ->
          assert_bytes label register (function_instructions label "F" image);
          let steps = execute retained 100_000 |> completed label 22L in
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
            ignore (execute retained steps |> completed label 22L);
            incr retained_recoveries
          done;
          require (!owned_faults > 0)
            (label ^ ": no fault occurred inside the register-owning function"));
      let source =
        Printf.sprintf
          "I64 Attempts=0;I64 F(%sI64 x,I64 noreg count){return \
           x/(count-1);}Attempts++;F(%s42,Attempts);"
          (reserved_parameters reservations)
          (reserved_arguments reservations)
      in
      with_retained mode source (fun image retained ->
          assert_bytes label register (function_instructions label "F" image);
          (match execute retained 100_000 with
          | Program.Fault fault ->
              require
                (fault.kind = Program.Division_by_zero
                && fault.function_name = Some "F")
                (label ^ ": wrong first retained division fault")
          | Program.Completed _ ->
              failwith (label ^ ": first retained division did not fault"));
          ignore (execute retained 100_000 |> completed label 42L);
          ignore (execute retained 100_000 |> completed label 21L);
          incr retained_recoveries))
    reservation_sets

let cli_faults mode =
  List.iter
    (fun (register, reservations) ->
      let label = register ^ " automatic recursive division fault" in
      let source =
        Printf.sprintf
          "I64 F(%sI64 x){if(x)return F(%sx-1)+x;return 42/x;}F(%s3);"
          (reserved_parameters reservations)
          (reserved_arguments reservations)
          (reserved_arguments reservations)
      in
      List.iter
        (fun target ->
          invoke ~status:1 ~target ~mode label source
          |> fault label "HCIRVM0009")
        (if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]))
    reservation_sets

(* Native_program.compile deliberately has no #exe executor. The public CLI
   below exercises directives in both outer modes through host-jit-task. The
   low-level byte harness requires JIT task mode: run the same definition/option
   ordering in that original directive task and inspect the fragment that
   actually calls F. Keep the original source callback; options are not replaced
   by test metadata. AOT directive execution is checked through the public CLI. *)
let directive_placement mode case body =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let label = case.name ^ " " ^ mode ^ " directive" in
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
              (List.map (fun (e : Program.error) -> e.message) errors))
  in
  let layout =
    Program.create_task_layout_with_literals ~max_global_bytes:4096
      ~max_literal_bytes:4096
    |> image_checked
  in
  let arena = Runtime.create_task_arena layout |> checked label in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> checked label in
  let observed = ref false in
  let execute request =
    let image =
      Program.compile_task_command ~max_code_bytes:1_048_576 ~layout request
      |> image_checked
    in
    let source_callback =
      Option.map
        (fun callback scope operation ->
          Some (callback scope operation |> checked label |> diagnostics_checked))
        (Program.source_callback image)
    in
    let retained = Runtime.retain_task_fragment arena image |> checked label in
    let report =
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked label)
        (fun () ->
          Runtime.execute_retained_budget_report ?source_callback budget
            retained)
    in
    let result = Runtime.outcome report |> checked label in
    let sections =
      Program.native_sections image |> Holyc_lib__Asm.Native_sections.to_yojson
    in
    let has_f =
      sections |> member "symbols" |> to_list
      |> List.exists (fun symbol -> member "name" symbol |> to_string = "F")
    in
    let captured_expected =
      match result with
      | Program.Completed execution ->
          Runtime.value_captured report
          && Option.map
               (fun (word : Program.word) -> word.bits)
               execution.final_value
             = Some case.expected
      | Program.Fault _ -> false
    in
    if has_f && captured_expected then (
      ignore (completed label case.expected result);
      let instructions = function_instructions label "F" image in
      Option.iter
        (fun register -> assert_bytes label register instructions)
        case.register;
      assert_saved_registers label case.saved instructions;
      observed := true);
    match result with
    | Program.Completed execution ->
        Ok
          (if Runtime.value_captured report then
             Task.Native_dispatch.Captured
               (Option.map
                  (fun (word : Program.word) ->
                    match word.type_ with
                    | Program.I64 -> Task.Native_dispatch.I64 word.bits
                    | Program.U64 -> Task.Native_dispatch.U64 word.bits)
                  execution.final_value)
           else Task.Native_dispatch.Unchanged)
    | Program.Fault _ -> failwith (label ^ ": unexpected native directive fault")
  in
  Fun.protect
    ~finally:(fun () -> Runtime.release_task_arena arena |> checked label)
    (fun () ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"automatic-register-options.hc"
          ~contents:("#exe {" ^ body ^ "}")
      in
      let config =
        Preprocessor.Config.create
          ~compilation_mode:
            (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
          ()
        |> checked label
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

let () =
  let cases =
    width_cases @ reservation_cases @ priority_cases @ negative_cases
    @ control_cases
  in
  List.iter
    (fun mode ->
      List.iter
        (fun case ->
          let label = case.name ^ " " ^ mode in
          (match case.directive_body with
          | Some body ->
              if native && mode = "jit" then directive_placement mode case body
          | None ->
              let image = compile mode case.source in
              let instructions = function_instructions label "F" image in
              Option.iter
                (fun register -> assert_bytes label register instructions)
                case.register;
              assert_saved_registers label case.saved instructions;
              List.iter
                (fun (name, register) ->
                  let instructions = function_instructions label name image in
                  assert_bytes (label ^ " " ^ name) register instructions;
                  assert_saved_registers
                    (label ^ " " ^ name)
                    [ register ] instructions)
                case.children);
          let baseline =
            invoke ~options:case.options ~target:"ir" ~mode label case.source
          in
          value label case.expected baseline;
          let check_output report =
            require
              (report |> member "output_hex" |> to_string |> bytes = case.output)
              (label ^ ": wrong captured output")
          in
          check_output baseline;
          if native then
            List.iter
              (fun target ->
                let report =
                  invoke ~options:case.options ~target ~mode label case.source
                in
                value label case.expected report;
                check_output report;
                require
                  (member "bits" (member "final_value" report)
                  = member "bits" (member "final_value" baseline))
                  (label ^ ": IR/native bits differ"))
              (if Option.is_some case.directive_body then [ "host-jit-task" ]
               else [ "host-jit"; "host-jit-task" ]))
        cases;
      cli_faults mode;
      if native then retained_fault_recovery mode)
    modes;
  Printf.printf
    "Automatic register checks passed (%d CLI reports, %d decoded placements, \
     %d unplaced cases, %d same-mapping fault recoveries).\n"
    !reports !placements !exclusions !retained_recoveries
