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
let original_registers = [ "R12"; "R13"; "R14"; "R15" ]
let six_registers = [ "RSI"; "RDI" ] @ original_registers
let registers = [ "R10"; "R11" ] @ six_registers
let bridge_checks = ref 0
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
          (register ^ " unpinned child")
          (Printf.sprintf
             "I64 Child(I64 noreg y){return y*3+1;}I64 F(I64 reg %s x){return \
              x+Child(x+2)+x;}F(7);"
             register)
          42L;
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
    [
      ("R10", "R11");
      ("R11", "RSI");
      ("RSI", "RDI");
      ("RDI", "R12");
      ("R12", "R13");
      ("R13", "R14");
      ("R14", "R15");
      ("R15", "RSI");
    ]

let recursive_source =
  "I64 F(I64 reg R12 x,I64 reg R13 n){I64 reg R14 a=x+2;I64 reg R15 \
   b=x+3;if(n)return F(x+1,n-1)+x+a+b;return x+a+b;}F(1,3);"

let six_recursive_source =
  "I64 F(I64 reg RSI x,I64 reg RDI n){I64 reg R12 a=x+1;I64 reg R13 b=x+2;I64 \
   reg R14 c=x+3;I64 reg R15 d=x+4;if(n)return F(x+1,n-1)+x+a+b+c+d;return \
   x+a+b+c+d;}F(1,3);"

let runtime_recursive_source =
  "I64 F(I64 reg R10 x,I64 reg R11 n){if(n)return F(x+1,n-1)+x+n;return \
   x+n;}F(1,3);"

let all_register_cases =
  [
    case "R10 R11 live across recursion" runtime_recursive_source 16L;
    case "eight live parameters"
      "I64 F(I64 reg R10 a,I64 reg R11 b,I64 reg RSI c,I64 reg RDI d,I64 reg \
       R12 e,I64 reg R13 f,I64 reg R14 g,I64 reg R15 h){return \
       a+b+c+d+e+f+g+h;}F(1,2,3,4,5,6,7,8);"
      36L;
    case "six live parameters"
      "I64 F(I64 reg RSI a,I64 reg RDI b,I64 reg R12 c,I64 reg R13 d,I64 reg \
       R14 e,I64 reg R15 f){a+=b;b+=c;c+=d;d+=e;e+=f;f+=a;return \
       a+b+c+d+e+f;}F(1,2,3,4,5,6);"
      44L;
    case "six live locals"
      "I64 F(I64 seed){I64 reg RSI a=seed;I64 reg RDI b=a+1;I64 reg R12 \
       c=b+1;I64 reg R13 d=c+1;I64 reg R14 e=d+1;I64 reg R15 \
       f=e+1;a+=b;b+=c;c+=d;d+=e;e+=f;f+=a;return a+b+c+d+e+f;}F(1);"
      44L;
    case "six live words survive repeated all-register callees"
      "I64 Child(I64 reg RSI a,I64 reg RDI b,I64 reg R12 c,I64 reg R13 d,I64 \
       reg R14 e,I64 reg R15 f){a*=2;b*=3;c*=4;d*=5;e*=6;f*=7;return \
       a+b+c+d+e+f;}I64 F(I64 reg RSI a,I64 reg RDI b,I64 reg R12 c,I64 reg \
       R13 d,I64 reg R14 e,I64 reg R15 f){return \
       a+b+c+d+e+f+Child(f,e,d,c,b,a)+Child(a,b,c,d,e,f)+a+b+c+d+e+f;}F(1,2,3,4,5,6);"
      231L;
    case "six live words survive recursion" six_recursive_source 90L;
    case "six-register mixed parameter widths"
      "I64 F(I8 reg RSI a,U8 reg RDI b,I16 reg R12 c,U16 reg R13 d,I32 reg R14 \
       e,U64 reg R15 f){a+=2;b+=2;c+=2;d+=2;e+=2;f+=2;return \
       a+b+c+d+e+f;}F(127,255,32767,65535,2147483647,3);"
      2147582346L;
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

(* The pinned body stays in the bounded integer source proof. Providers,
   pointers and callback cells belong only to an ordinary defined callee, so a
   passing result cannot be explained by a stack-selected caller fallback. *)
let pinned_caller registers =
  let names =
    List.mapi
      (fun index _ -> String.make 1 (Char.chr (Char.code 'a' + index)))
      registers
  in
  let parameters =
    List.map2
      (fun register name -> "I64 reg " ^ register ^ " " ^ name)
      registers names
    |> String.concat ","
  in
  let sum = String.concat "+" names in
  Printf.sprintf "I64 F(%s){return %s+Bridge(%s)+%s;}" parameters sum sum sum

let bridge_register_sets =
  [
    [ "RSI" ];
    [ "RDI" ];
    [ "RSI"; "RDI" ];
    [ "R10" ];
    [ "R11" ];
    [ "R10"; "R11" ];
    six_registers;
    registers;
  ]

let bridge_arguments registers =
  match registers with
  | [ _ ] -> ("14", 14L)
  | [ _; _ ] -> ("6,8", 14L)
  | _ ->
      let values = List.mapi (fun i _ -> i + 1) registers in
      ( String.concat "," (List.map string_of_int values),
        Int64.of_int (List.fold_left ( + ) 0 values) )

let bridge_headers = "extern U0 PutChars(U64 ch);extern U0 Print(U8 *fmt,...);"

let ordinary_bridges =
  [
    ( "reference entry, address and exit",
      "I64 Bridge(I64 n){I64 local=n;I64 *p=&local;*p+=1;return *p-1;}",
      "" );
    ( "ordinary addressable callee",
      "I64 Target(I64 n){return n;}I64 Bridge(I64 n){I64 (*p)(I64 \
       n);p=&Target;return p(n);}",
      "" );
    ("direct PutChars", "I64 Bridge(I64 n){PutChars('A');return n;}", "A");
    ( "addressable PutChars",
      "I64 Bridge(I64 n){U0 (*p)(U64 ch);p=&PutChars;p('A');return n;}",
      "A" );
    ("direct Print", "I64 Bridge(I64 n){Print(\"A%d;\",n);return n;}", "number");
    ( "addressable Print",
      "I64 Bridge(I64 n){U0 (*p)(U8 *fmt,...);p=&Print;p(\"A%d;\",n);return n;}",
      "number" );
  ]

let source_headers =
  bridge_headers
  ^ "extern U0 StreamPrint(U8 *fmt,...);extern I64 StreamExePrint(U8 *fmt,...);"
  ^ "extern U8 GetOption(I64 num);extern U8 Option(I64 num,U8 val);"

let source_bridges =
  [
    ( "direct GetOption and SetOption",
      "I64 Bridge(I64 n){Option(33,1);if(GetOption(33)!=1)return \
       0;Option(33,0);if(GetOption(33))return 0;return n;}" );
    ( "addressable GetOption and SetOption",
      "I64 Bridge(I64 n){U8 (*read)(I64 num);U8 (*write)(I64 num,U8 \
       val);read=&GetOption;write=&Option;write(33,1);if(read(33)!=1)return \
       0;write(33,0);if(read(33))return 0;return n;}" );
    ( "direct formatted source",
      "I64 Bridge(I64 n){return StreamExePrint(\"%d;\",n);}" );
    ( "addressable formatted source",
      "I64 Bridge(I64 n){I64 (*p)(U8 *fmt,...);p=&StreamExePrint;return \
       p(\"%d;\",n);}" );
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
  ]

(* Whole-function source/call receipts now prove this entry initializer.
   F and Child each own explicit R12; saved stays noreg. Child(20)=21,
   and the caller's preserved x contributes 20 again, producing 41. *)
let call_initializer_case =
  case "proved call-valued local initializer preserves explicit R12"
    "I64 Child(I64 reg R12 y){return y+1;}I64 F(I64 reg R12 x){I64 noreg \
     saved=Child(x);return x+saved;}F(20);"
    41L

(* x retains its explicit R12 request. p has three balanced direct accesses,
   score 6, and takes RSI; y remains noreg addressable storage. *)
let owned_pointer_neighbor_case =
  case "addressable neighbor and pointer member preserve explicit R12"
    "I64 F(I64 reg R12 x){I64 noreg y=5;I64 *p=&y;*p+=2;x+=*p;return \
     x+y;}F(28);"
    42L

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

let function_instructions label name report =
  section_instructions label name
    (report |> member "native" |> member "image" |> member "native_sections")

let assert_placement ?(parameter = true) label register instructions =
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
  require
    (if parameter then has_move register "RAX"
     else
       List.exists
         (fun (instruction : Disassembly.instruction) ->
           instruction.opcode = "MOV"
           &&
           match instruction.operands with
           | [ destination; source ] ->
               destination.text = "U64 " ^ register
               && source.size = 64
               && not (String.contains source.text '[')
           | _ -> false)
         instructions)
    (label ^ ": member never enters " ^ register);
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
  require
    (List.length saves = 1)
    (label ^ ": expected one private register save");
  require
    (List.length restores = 1)
    (label ^ ": expected one private register restore");
  let save = List.hd saves and restore = List.hd restores in
  require
    (String.sub save.bytes 2 (String.length save.bytes - 2)
    = String.sub restore.bytes 2 (String.length restore.bytes - 2))
    (label ^ ": register restore does not use its private save slot");
  let number =
    List.assoc register
      [
        ("RSI", 6);
        ("RDI", 7);
        ("R10", 10);
        ("R11", 11);
        ("R12", 12);
        ("R13", 13);
        ("R14", 14);
        ("R15", 15);
      ]
  in
  let rex = if register = "RSI" || register = "RDI" then 0x48 else 0x4c in
  List.iter
    (fun (instruction : Disassembly.instruction) ->
      require
        (String.length instruction.bytes >= 3
        && Char.code instruction.bytes.[0] = rex
        && (Char.code instruction.bytes.[2] lsr 3) land 7 = number land 7)
        (label ^ ": save/restore has the wrong REX.W/R prefix"))
    (saves @ restores);
  incr placements

let check_placement mode register =
  let label = register ^ " actual parameter placement" in
  let source = Printf.sprintf "I64 F(I64 reg %s x){return x;}F(42);" register in
  let report = invoke ~target:"host-jit" ~mode label source in
  value label 42L report;
  assert_placement label register (function_instructions label "F" report)

let output label expected report =
  let actual = report |> member "output_hex" |> to_string |> bytes in
  require (actual = expected) (label ^ ": wrong captured bridge output")

let check_bridge_placement ~owned_reference label registers report =
  let instructions = function_instructions label "F" report in
  List.iter
    (fun register -> assert_placement label register instructions)
    registers;
  let bridge = function_instructions label "Bridge" report in
  (* This source helper now has independent owned-member evidence: p scores
     six and enters RSI; n scores two and enters RDI. Caller overlap is legal
     only because both homes are saved/restored and the caller result survives. *)
  if owned_reference then (
    assert_placement ~parameter:false (label ^ " pointer p") "RSI" bridge;
    assert_placement (label ^ " parameter n") "RDI" bridge);
  let forbidden =
    if owned_reference then
      List.filter (fun r -> r <> "RSI" && r <> "RDI") registers
    else registers
  in
  require
    (not
       (List.exists
          (fun (instruction : Disassembly.instruction) ->
            instruction.opcode = "MOV"
            &&
            match instruction.operands with
            | [ destination; source ] ->
                List.mem source.text (List.map (( ^ ) "U64 ") forbidden)
                && String.ends_with ~suffix:"[RBP]" destination.text
            | _ -> false)
          bridge))
    (label ^ ": helper unexpectedly owns a pinned-register frame save");
  incr bridge_checks

let addressable_provider name =
  name = "addressable PutChars" || name = "addressable Print"

let ordinary_bridge_cases mode =
  List.iter
    (fun registers ->
      let arguments, sum = bridge_arguments registers in
      List.iter
        (fun (name, bridge, expected_output) ->
          let label = String.concat "/" registers ^ " through " ^ name in
          let source =
            if addressable_provider name then
              (if mode = "jit" then source_headers else "")
              ^ "#exe {" ^ bridge ^ pinned_caller registers
              ^ "StreamPrint(\"%d;\",F(" ^ arguments ^ "));}"
            else
              bridge_headers ^ bridge ^ pinned_caller registers ^ "F("
              ^ arguments ^ ");"
          in
          let expected = Int64.mul 3L sum in
          let expected_output =
            if expected_output = "number" then Printf.sprintf "A%Ld;" sum
            else expected_output
          in
          List.iter
            (fun target ->
              let report =
                invoke
                  ~options:[ "--code-byte-limit=1048576" ]
                  ~target ~mode label source
              in
              value label expected report;
              output label expected_output report;
              if target = "host-jit" then
                check_bridge_placement
                  ~owned_reference:(name = "reference entry, address and exit")
                  label registers report)
            (if not native then [ "ir" ]
             else if addressable_provider name then [ "ir"; "host-jit-task" ]
             else [ "ir"; "host-jit"; "host-jit-task" ]))
        ordinary_bridges)
    bridge_register_sets

let source_bridge_limits mode label source baseline expected =
  let code, ir =
    baseline |> member "native" |> member "fragments" |> to_list
    |> List.fold_left
         (fun (code, ir) fragment ->
           ( code + (fragment |> member "code_bytes" |> to_int),
             ir + (fragment |> member "ir_instructions" |> to_int) ))
         (0, 0)
  in
  let steps = baseline |> member "executed_steps" |> to_int in
  let options code ir steps =
    [
      "--code-byte-limit=" ^ string_of_int code;
      "--ir-instruction-limit=" ^ string_of_int ir;
      "--step-limit=" ^ string_of_int steps;
    ]
  in
  invoke ~target:"host-jit-task" ~mode ~options:(options code ir steps)
    (label ^ " exact resource bounds")
    source
  |> value label expected;
  List.iter
    (fun (name, diagnostic, code, ir, steps) ->
      invoke ~status:1 ~target:"host-jit-task" ~mode
        ~options:(options code ir steps) (label ^ name) source
      |> fault label diagnostic)
    [
      (" one-below code", "HCBACK0005", code - 1, ir, steps);
      (" one-below IR", "HCBACK0001", code, ir - 1, steps);
      (" one-below steps", "HCIRVM0007", code, ir, steps - 1);
    ]

let source_bridge_cases mode =
  List.iter
    (fun registers ->
      let arguments, sum = bridge_arguments registers in
      List.iter
        (fun (name, bridge) ->
          let label = String.concat "/" registers ^ " through " ^ name in
          let source =
            (if mode = "jit" then source_headers else "")
            ^ "#exe {" ^ bridge ^ pinned_caller registers
            ^ "StreamPrint(\"%d;\",F(" ^ arguments ^ "));}"
          in
          List.iter
            (fun target ->
              let report =
                invoke
                  ~options:[ "--code-byte-limit=1048576" ]
                  ~target ~mode label source
              in
              value label (Int64.mul 3L sum) report;
              output label "" report;
              if
                target = "host-jit-task"
                && (registers = [ "RSI"; "RDI" ] || registers = [ "R10"; "R11" ])
              then
                source_bridge_limits mode label source report (Int64.mul 3L sum))
            (if native then [ "ir"; "host-jit-task" ] else [ "ir" ]))
        source_bridges)
    bridge_register_sets

let source_bridge_faults mode =
  List.iter
    (fun (name, bridge, code) ->
      let source =
        "#exe {" ^ bridge ^ pinned_caller registers
        ^ "Print(\"before;\");F(1,2,3,4,5,6,7,8);Print(\"after;\");}42;"
      in
      List.iter
        (fun target ->
          let report =
            invoke ~status:1 ~target ~mode
              ~options:[ "--code-byte-limit=1048576" ]
              name source
          in
          fault name code report;
          output name "before;" report)
        (if native then [ "ir"; "host-jit-task" ] else [ "ir" ]))
    [
      ( "direct GetOption fault below eight pinned registers",
        "I64 Bridge(I64 n){GetOption(64);return n;}",
        "HCEVAL0003" );
      ( "direct SetOption fault below eight pinned registers",
        "I64 Bridge(I64 n){Option(64,1);return n;}",
        "HCEVAL0003" );
      ( "addressable GetOption fault below eight pinned registers",
        "I64 Bridge(I64 n){U8 (*p)(I64 num);p=&GetOption;p(64);return n;}",
        "HCEVAL0003" );
      ( "addressable SetOption fault below eight pinned registers",
        "I64 Bridge(I64 n){U8 (*p)(I64 num,U8 val);p=&Option;p(64,1);return n;}",
        "HCEVAL0003" );
      ( "direct formatted source fault below eight pinned registers",
        "I64 Bridge(I64 n){return StreamExePrint(\"%f\",n);}",
        "HCIRVM0024" );
      ( "addressable formatted source fault below eight pinned registers",
        "I64 Bridge(I64 n){I64 (*p)(U8 *fmt,...);p=&StreamExePrint;return \
         p(\"%f\",n);}",
        "HCIRVM0024" );
    ]

(* Enough ordinary locals make F, rather than its caller, determine the
   largest private frame. The otherwise-identical noreg control isolates the
   saved nonvolatile words from ordinary parameter/local frame storage. *)
let quota_source registers physical =
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
          (Char.chr (Char.code 'a' + (index mod List.length registers))))
    |> String.concat ""
  in
  let result =
    (List.mapi
       (fun index _ -> String.make 1 (Char.chr (Char.code 'a' + index)))
       registers
    |> String.concat "+")
    ^ "+"
    ^ (List.init 8 (fun index -> "q" ^ string_of_int index) |> String.concat "+")
  in
  let arguments =
    match List.length registers with
    | 4 -> "10,11,12,9"
    | 6 -> "10,11,12,9,13,14"
    | 8 -> "10,11,12,9,13,14,15,16"
    | _ -> failwith "unexpected quota register count"
  in
  Printf.sprintf "I64 F(%s){%sreturn %s;}F(%s);" parameters locals result
    arguments

let decoded_frame_allocation label report =
  let instruction =
    function_instructions label "F" report
    |> List.find_opt (fun (instruction : Disassembly.instruction) ->
        instruction.opcode = "SUB"
        &&
        match instruction.operands with
        | [ destination; _ ] -> destination.text = "U64 RSP"
        | _ -> false)
    |> function
    | Some instruction -> instruction
    | None -> failwith (label ^ ": F has no decoded stack allocation")
  in
  match instruction.operands with
  | [ _; size ] -> (
      match size.value with
      | Some bytes when bytes > 0L && bytes <= Int64.of_int max_int ->
          Int64.to_int bytes
      | _ -> failwith (label ^ ": missing positive stack allocation"))
  | _ -> failwith (label ^ ": invalid stack allocation instruction")

let exact_stack_quotas mode registers expected =
  let physical_source = quota_source registers true
  and stack_source = quota_source registers false in
  let baseline label source =
    let report = invoke ~target:"host-jit" ~mode label source in
    value label expected report;
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
    "register saves must add exactly one word each to the private frame";
  require
    (physical_active = stack_active + save_bytes)
    "active-stack charge must include all register saves";
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
        |> value label expected
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

(* Decode Windows unwind slots rather than searching arbitrary bytes: extension
   slots contain offsets, not opcodes. R10/R11 are private-convention saves,
   never Windows nonvolatile registers, even in their owning functions. *)
let check_volatile_unwind image =
  List.iter
    (fun (_, _, info) ->
      let byte n = Char.code info.[n] in
      let slots = byte 2 in
      let rec walk slot =
        if slot < slots then (
          let code = byte (5 + (2 * slot)) in
          let operation = code land 15 and register = code lsr 4 in
          if operation = 0 || operation = 4 || operation = 5 then
            require
              (register <> 10 && register <> 11)
              "Windows unwind incorrectly describes volatile R10/R11 as \
               nonvolatile";
          let width =
            match operation with
            | 1 -> if register = 0 then 2 else 3
            | 4 | 8 -> 2
            | 5 | 9 -> 3
            | _ -> 1
          in
          require (slot + width <= slots) "truncated Windows unwind operation";
          walk (slot + width))
      in
      walk 0)
    (Program.windows_unwind_functions image)

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
  check_volatile_unwind image;
  let retained = Runtime.retain image |> checked "retain" in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked "release")
    (fun () -> action retained)

let execute retained max_steps =
  Runtime.execute_retained_report ~max_steps retained
  |> Runtime.outcome |> checked "execute retained"

(* Observe the original source-task image before execution. CLI task reports
   retain metadata rather than bytes. This harness checks the actual F bytes,
   transports formatted source to a bounded test callback, and runs the real
   option callback. The CLI cases above separately execute the generated source. *)
let source_bridge_placements () =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Scope = Holyc_lib__Ir.Native_source_suspension in
  let image_checked = function
    | Ok image -> image
    | Error errors ->
        failwith
          (String.concat "; "
             (List.map (fun (error : Program.error) -> error.message) errors))
  in
  let diagnostics_checked = function
    | Ok result -> result
    | Error errors ->
        failwith
          (String.concat "; "
             (List.map (fun (error : Diagnostic.t) -> error.message) errors))
  in
  List.iter
    (fun registers ->
      let arguments, sum = bridge_arguments registers in
      List.iter
        (fun (name, bridge) ->
          let label = String.concat "/" registers ^ " actual source " ^ name in
          let layout =
            Program.create_task_layout_with_literals ~max_global_bytes:4096
              ~max_literal_bytes:4096
            |> image_checked
          in
          let arena = Runtime.create_task_arena layout |> checked label in
          let budget =
            Runtime.create_budget ~max_steps:100_000 () |> checked label
          in
          let observed = ref false and calls = ref 0 in
          let execute request =
            let image =
              Program.compile_task_command ~max_code_bytes:1_048_576 ~layout
                request
              |> image_checked
            in
            check_volatile_unwind image;
            let sections =
              Program.native_sections image
              |> Holyc_lib__Asm.Native_sections.to_yojson
            in
            let has_caller =
              sections |> member "symbols" |> to_list
              |> List.exists (fun symbol -> member "name" symbol = `String "F")
            in
            (* Declarations can emit a fragment containing F without calling
               it. Pair the original bytes with the fragment that actually
               crosses the bridge and captures F's result below. *)
            let calls_before = !calls in
            let scopes = ref [] in
            let callback scope operation =
              incr calls;
              Scope.check scope |> checked label;
              require
                (Scope.owns_request scope operation |> checked label)
                (label ^ ": callback lost its original request");
              scopes := scope :: !scopes;
              Gc.full_major ();
              Gc.compact ();
              match operation with
              | Scope.Execute_source contents ->
                  require
                    (contents = Printf.sprintf "%Ld;" sum)
                    (label ^ ": wrong formatted source at host boundary");
                  Some sum
              | Scope.Read_option _ | Scope.Write_option _ ->
                  let original =
                    match Program.source_callback image with
                    | Some callback -> callback
                    | None ->
                        failwith
                          (label
                         ^ ": option fragment has no original source callback")
                  in
                  Some
                    (original scope operation |> checked label
                   |> diagnostics_checked)
            in
            let retained =
              Runtime.retain_task_fragment arena image |> checked label
            in
            let report =
              Fun.protect
                ~finally:(fun () -> Runtime.release retained |> checked label)
                (fun () ->
                  Runtime.execute_retained_budget_report
                    ~source_callback:callback budget retained)
            in
            List.iter
              (fun scope ->
                require
                  (Result.is_error (Scope.check scope))
                  (label ^ ": source callback scope survived return"))
              !scopes;
            let result = Runtime.outcome report |> checked label in
            let crossed_bridge =
              !calls > calls_before
              || (addressable_provider name && Runtime.output_bytes report <> "")
            in
            if crossed_bridge then (
              let actual =
                match result with
                | Program.Completed execution ->
                    Option.fold ~none:"none"
                      ~some:(fun (word : Program.word) ->
                        Printf.sprintf "0x%016Lx" word.bits)
                      execution.final_value
                | Program.Fault _ -> "fault"
              in
              let evidence =
                Printf.sprintf
                  "%s: has_caller=%b captured=%b actual=%s expected=0x%016Lx"
                  label has_caller
                  (Runtime.value_captured report)
                  actual (Int64.mul 3L sum)
              in
              require (has_caller && Runtime.value_captured report) evidence;
              ignore (completed evidence (Int64.mul 3L sum) result);
              let instructions = section_instructions label "F" sections in
              List.iter
                (fun register -> assert_placement label register instructions)
                registers;
              observed := true;
              incr bridge_checks);
            match result with
            | Program.Completed execution ->
                Ok
                  (if Runtime.value_captured report then
                     Task.Native_dispatch.Captured
                       (Option.map
                          (fun (word : Program.word) ->
                            match word.type_ with
                            | Program.I64 -> Task.Native_dispatch.I64 word.bits
                            | Program.U64 -> Task.Native_dispatch.U64 word.bits
                            | Program.F64 -> Task.Native_dispatch.F64 word.bits)
                          execution.final_value)
                   else Task.Native_dispatch.Unchanged)
            | Program.Fault _ ->
                failwith (label ^ ": unexpected source fragment fault")
          in
          Fun.protect
            ~finally:(fun () ->
              Runtime.release_task_arena arena |> checked label)
            (fun () ->
              let session = Session.create () in
              let source =
                Session.add_source session ~path:"physical-source-bridge.hc"
                  ~contents:
                    (source_headers ^ "#exe {" ^ bridge
                   ^ pinned_caller registers ^ "F(" ^ arguments ^ ");}")
              in
              let config =
                Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
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
              require
                (!observed && (!calls > 0 || addressable_provider name))
                (label ^ ": no original pinned caller crossed the host bridge");
              require
                ((Runtime.budget_progress budget).error = None)
                (label ^ ": source callback left an unverified budget")))
        (source_bridges
        @ List.filter_map
            (fun (name, bridge, _) ->
              if addressable_provider name then Some (name, bridge) else None)
            ordinary_bridges))
    bridge_register_sets

let retained_bridge_recovery mode =
  List.iter
    (fun (name, bridge, expected_output) ->
      let label = "eight-register retained " ^ name in
      let source =
        bridge_headers ^ bridge ^ pinned_caller registers
        ^ "F(1,2,3,4,5,6,7,8);"
      in
      with_retained mode source (fun retained ->
          let run ?(max_steps = 100_000) ?max_output_bytes
              ?max_active_stack_bytes () =
            Runtime.execute_retained_report ~max_steps ?max_output_bytes
              ?max_active_stack_bytes retained
          in
          let first = run () in
          let steps =
            Runtime.outcome first |> checked label |> completed label 108L
          in
          let expected_output =
            if expected_output = "number" then "A36;" else expected_output
          in
          require
            (Runtime.output_bytes first = expected_output)
            (label ^ ": baseline output");
          let recover () =
            let report = run ~max_steps:steps () in
            ignore
              (Runtime.outcome report |> checked label |> completed label 108L);
            require
              (Runtime.output_bytes report = expected_output)
              (label ^ ": recovery output");
            incr retained_recoveries
          in
          for limit = 1 to steps - 1 do
            (match
               Runtime.outcome (run ~max_steps:limit ()) |> checked label
             with
            | Program.Fault fault ->
                require
                  (fault.kind = Program.Step_limit_exceeded
                  && fault.executed_steps = limit)
                  (label ^ ": incorrect bridge step fault")
            | Program.Completed _ ->
                failwith (label ^ ": bounded bridge completed"));
            recover ()
          done;
          if String.length expected_output > 1 then (
            let report = run ~max_output_bytes:1 () in
            (match Runtime.outcome report |> checked label with
            | Program.Fault fault ->
                require
                  (fault.kind = Program.Output_limit_exceeded)
                  (label ^ ": incorrect bridge output fault")
            | Program.Completed _ ->
                failwith (label ^ ": bridge ignored output bound"));
            recover ());
          (* The service scratch is shared fixed storage, not another activation.
           Find the exact admitted peak rather than pinning its current size. *)
          let rec minimum_stack lower upper =
            if upper - lower = 1 then upper
            else
              let middle = lower + ((upper - lower) / 2) in
              match Runtime.outcome (run ~max_active_stack_bytes:middle ()) with
              | Ok (Program.Completed _) -> minimum_stack lower middle
              | Ok (Program.Fault fault)
                when fault.kind = Program.Native_stack_limit_exceeded ->
                  minimum_stack middle upper
              | Error _ -> minimum_stack middle upper
              | _ ->
                  failwith
                    (label ^ ": unrelated fault while deriving stack limit")
          in
          let stack = minimum_stack 0 Runtime.default_active_stack_bytes in
          ignore
            (Runtime.outcome (run ~max_active_stack_bytes:stack ())
            |> checked label |> completed label 108L);
          (match
             Runtime.outcome (run ~max_active_stack_bytes:(stack - 1) ())
             |> checked label
           with
          | Program.Fault fault ->
              require
                (fault.kind = Program.Native_stack_limit_exceeded)
                (label ^ ": wrong one-below bridge stack fault")
          | Program.Completed _ ->
              failwith (label ^ ": one-below stack completed"));
          recover ()))
    (List.filter
       (fun (name, _, _) -> not (addressable_provider name))
       ordinary_bridges)

let retained_fault_recovery mode =
  List.iter
    (fun (source, expected) ->
      with_retained mode source (fun retained ->
          let steps =
            execute retained 100_000 |> completed "recursive baseline" expected
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
              (execute retained 100_000
              |> completed "same mapping after fault" expected);
            incr retained_recoveries
          done;
          require (!owned_faults > 0)
            "no checked fault occurred inside register-owning function"))
    [
      (recursive_source, 50L);
      (six_recursive_source, 90L);
      (runtime_recursive_source, 16L);
    ];
  with_retained mode
    "I64 Attempts=0;I64 F(I64 reg R10 x,I64 reg R11 y,I64 noreg count){return \
     (x+y)/(count-1);}Attempts++;F(19,23,Attempts);" (fun retained ->
      (match execute retained 100_000 with
      | Program.Fault fault ->
          require
            (fault.kind = Program.Division_by_zero
            && fault.function_name = Some "F")
            "expected division fault inside R10/R11 owner"
      | Program.Completed _ -> failwith "R10/R11 division fault missing");
      ignore
        (execute retained 100_000
        |> completed "R10/R11 mapping after division fault" 42L);
      ignore
        (execute retained 100_000
        |> completed "R10/R11 third retained invocation" 21L);
      incr retained_recoveries);
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
  if native then source_bridge_placements ();
  let cases =
    width_cases @ register_cases @ all_register_cases
    @ [
        call_initializer_case;
        owned_pointer_neighbor_case;
        case "maintained physical register pair example"
          (read "../examples/physical-register-pair.hc")
          42L;
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
                (* Task reports need not expose a standalone native image. *)
                if case == call_initializer_case && target = "host-jit" then
                  List.iter
                    (fun name ->
                      assert_placement
                        (case.name ^ " " ^ name)
                        "R12"
                        (function_instructions case.name name report))
                    [ "F"; "Child" ];
                if case == owned_pointer_neighbor_case && target = "host-jit"
                then
                  List.iter
                    (fun register ->
                      assert_placement ~parameter:(register = "R12") case.name
                        register
                        (function_instructions case.name "F" report))
                    [ "R12"; "RSI" ];
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
      ordinary_bridge_cases mode;
      source_bridge_cases mode;
      source_bridge_faults mode;
      if native then (
        List.iter (check_placement mode) registers;
        exact_stack_quotas mode original_registers 134L;
        exact_stack_quotas mode six_registers 167L;
        exact_stack_quotas mode registers 208L;
        bounded_rejections mode;
        retained_fault_recovery mode;
        retained_bridge_recovery mode))
    modes;
  Printf.printf
    "Physical register checks passed (%d CLI reports, %d decoded placements, \
     %d bridge placements, %d same-mapping fault recoveries).\n"
    !reports !placements !bridge_checks !retained_recoveries
