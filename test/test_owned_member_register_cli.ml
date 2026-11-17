open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* Source-derived expectations, not fresh TempleOS runtime captures.
   Reference: c26482bb6ad3f80106d28504ec5db3c6a360732c.
   OptPass3.HC:29-47 constructs primitive candidates, including pointer members.
   Each direct starting-offset read/write/update scores twice with net LEA 0
   (:119-135,149-163,179-199,223-245). A genuine address scores once and retains
   LEA +1; IC_ADDR survives OptPass012:1186 and OptPass3:496, including *(&x).
   QSort.HC:60-87 supplies the unstable copied-pivot ordering. Sort precedes
   zero trimming and address filtering (:538-553); pointer survivors contribute
   to pressure but never enter the R12/R13 scan (:557-582). General homes follow
   RSI,RDI,R10,R11,R14,R15 (:590-595). All expected maps below are literal
   independent deductions, never queried from the production allocator. *)
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
  let path = Filename.temp_file "holyc-owned-member-" suffix in
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

let compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes mode contents
    =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"owned-member-registers.hc" ~contents
  in
  Native_program.compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes
    session ~config:(config mode) ~source

let image mode source =
  compile mode source |> Result.map_error diagnostic_text |> checked |> fun x ->
  x.value

let fault expected report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      require (result.kind = expected) "wrong retained owned-member fault";
      result
  | Image.Completed _ ->
      failwith
        (match expected with
        | Image.Step_limit_exceeded ->
            "expected retained owned-member step fault"
        | Image.Frame_limit_exceeded ->
            "expected retained owned-member frame fault"
        | _ -> "expected retained owned-member fault")

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
  entries : (int * string) list;
  stores : (int * int) list;
  saved : string list;
  locals : (string * string option) list;
  unknown : string list;
}

let case ?(kind = "i64") ?(stores = []) ?saved ?(locals = []) ?(unknown = [])
    label source bits entries =
  let saved =
    Option.value saved ~default:(List.map snd entries |> List.sort_uniq compare)
  in
  { label; source; kind; bits; entries; stores; saved; locals; unknown }

let value case report =
  let word = member "final_value" report in
  require
    (member "outcome" report = `String "success"
    && member "type" word = `String case.kind
    && member "bits" word = `String (Printf.sprintf "0x%016Lx" case.bits))
    (case.label ^ ": " ^ Yojson.Safe.to_string report)

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

let placement_instructions case instructions =
  let show entries =
    String.concat ", "
      (List.map (fun (i, r) -> Printf.sprintf "%d:%s" i r) entries)
  in
  let actual = mapping instructions in
  require
    (actual = List.sort_uniq compare case.entries)
    (case.label ^ ": actual parameter map " ^ show actual ^ "; expected "
   ^ show case.entries);
  List.iter
    (fun (index, width) ->
      require
        (List.exists
           (fun (instruction : Disassembly.instruction) ->
             instruction.opcode = "MOV"
             &&
             match instruction.operands with
             | [ destination; _ ] ->
                 String.ends_with ~suffix:"[RBP]" destination.text
                 && destination.value = Some (Int64.of_int (16 + (index * 8)))
                 && destination.size = width
             | _ -> false)
           instructions)
        (Printf.sprintf "%s: parameter %d has no %d-bit spill store" case.label
           index width))
    case.stores;
  let saved =
    List.filter (fun r -> frame_moves ~save:true r instructions <> []) registers
  in
  require
    (List.sort compare saved = List.sort_uniq compare case.saved)
    (case.label ^ ": saved " ^ String.concat "," saved ^ "; expected "
    ^ String.concat "," case.saved);
  List.iter
    (fun register ->
      let saves = frame_moves ~save:true register instructions
      and restores = frame_moves ~save:false register instructions in
      require
        (List.length saves = 1 && List.length restores = 1)
        (case.label ^ ": expected one physical save/restore for " ^ register);
      let save = List.hd saves and restore = List.hd restores in
      require
        (String.sub save.bytes 2 (String.length save.bytes - 2)
        = String.sub restore.bytes 2 (String.length restore.bytes - 2))
        (case.label ^ ": save/restore frame homes differ for " ^ register))
    case.saved

let placement case image =
  placement_instructions case (function_instructions case.label "F" image)

(* The complete equal-score sort orders are 7:q,r,s,p,a,b,c;
   8:q,p,s,r,b,a,d,c; 9:q,p,s,r,e,b,a,d,c. R12/R13 scan those
   sequences for NONPOINTERS without consuming pointer positions. *)
let mixed_map = function
  | 7 ->
      [
        (0, "R12");
        (1, "R13");
        (4, "RSI");
        (5, "RDI");
        (6, "R10");
        (3, "R11");
        (2, "R14");
      ]
  | 8 ->
      [
        (1, "R12");
        (0, "R13");
        (5, "RSI");
        (4, "RDI");
        (7, "R10");
        (6, "R11");
        (3, "R14");
        (2, "R15");
      ]
  | 9 ->
      [
        (4, "R12");
        (1, "R13");
        (6, "RSI");
        (5, "RDI");
        (8, "R10");
        (7, "R11");
        (0, "R14");
        (3, "R15");
      ]
  | _ -> invalid_arg "missing literal mixed map"

let letters first count =
  List.init count (fun i -> String.make 1 (Char.chr (Char.code first + i)))

(* Each scalar has one direct update and one direct return read. Each pointer
   has one direct read feeding the pointee update and one feeding the pointee
   read. Thus both member kinds have two occurrences, score 4 and LEA 0; pointee
   dereferences do not add direct frame occurrences for the pointer member. *)
let mixed_case ?(scalar_type = "I8") count =
  let scalars = letters 'a' (count - 4) and pointers = letters 'p' 4 in
  let global_names = List.init 4 (fun i -> "Value" ^ string_of_int i) in
  let source =
    String.concat "" (List.map (fun n -> "I64 " ^ n ^ "=10;") global_names)
    ^ "I64 F("
    ^ String.concat ","
        (List.map (fun n -> scalar_type ^ " " ^ n) scalars
        @ List.map (fun n -> "I64 *" ^ n) pointers)
    ^ "){"
    ^ String.concat "" (List.map (fun n -> n ^ "+=1;") scalars)
    ^ String.concat "" (List.map (fun n -> "*" ^ n ^ "+=1;") pointers)
    ^ "return "
    ^ String.concat "+" (scalars @ List.map (( ^ ) "*") pointers)
    ^ ";}F("
    ^ String.concat ","
        (List.map (fun _ -> "127") scalars @ List.map (( ^ ) "&") global_names)
    ^ ");"
  in
  case
    ~stores:(if count = 9 then [ (2, 8) ] else [])
    (Printf.sprintf "%d mixed %s/pointer survivors" count scalar_type)
    source
    (Int64.of_int (((count - 4) * 128) + 44 - if count = 9 then 256 else 0))
    (mixed_map count)

let all_pointers =
  let names = letters 'a' 9 in
  let globals = List.map (fun n -> "Value_" ^ n) names in
  case "nine pointers use only the six general homes"
    (String.concat "" (List.map (fun n -> "I64 " ^ n ^ "=10;") globals)
    ^ "I64 F("
    ^ String.concat "," (List.map (fun n -> "I64 *" ^ n) names)
    ^ "){"
    ^ String.concat "" (List.map (fun n -> "*" ^ n ^ "+=1;") names)
    ^ "return "
    ^ String.concat "+" (List.map (( ^ ) "*") names)
    ^ ";}F("
    ^ String.concat "," (List.map (( ^ ) "&") globals)
    ^ ");")
    99L
    [ (6, "RSI"); (5, "RDI"); (8, "R10"); (7, "R11"); (4, "R14"); (1, "R15") ]

(* Copy fixture: p and q each score 4, hence q then p. The dead-rebinding
   fixture has two p reads and four q occurrences (initializer, dead write,
   pointee update and returned pointee), so p scores 4 and q scores 8. All
   occurrences are retained before OptPass4's later dead-code removal. *)
let pointer_cases =
  [
    case
      ~locals:[ ("q", Some "RSI") ]
      "pointer copies preserve aliased descriptor ownership"
      "I64 V=40;I64 F(I64 *p){I64 *q=p;*q+=2;return *p;}F(&V);" 42L
      [ (0, "RDI") ]
      ~saved:[ "RSI"; "RDI" ];
    case
      ~locals:[ ("q", Some "RSI") ]
      "dead pointer rebinding still contributes source scores"
      "I64 V=40;I64 F(I64 *p){I64 *q=p;if(0)q=p;*q+=2;return *q;}F(&V);" 42L
      [ (0, "RDI") ]
      ~saved:[ "RSI"; "RDI" ];
    case "indexed pointer update preserves descriptor bounds"
      "I64 V=40;I64 F(I64 *p,I64 noreg index){p[index]+=2;return \
       p[index];}F(&V,0);"
      42L
      [ (0, "RSI") ];
    case "pointer arguments and selected narrow scalar survive named call"
      "I64 V=42;I64 Read(I64 *q){return *q;}I64 F(I8 x,I64 *p){x+=1;return \
       x+Read(p);}F(127,&V);"
      170L
      [ (0, "RSI"); (1, "RDI") ];
    case "selected descriptors and narrow scalars survive recursion"
      "I64 V=42;I64 F(I8 x,I64 *p,I64 noreg n){if(n)return \
       F(x,p,n-1);x+=1;return x+*p;}F(127,&V,2);"
      170L
      [ (0, "RSI"); (1, "RDI") ];
    case
      ~locals:[ ("q", Some "RSI") ]
      "pointer rebind copies private descriptors rather than aliasing homes"
      "I64 A=40;I64 B=90;I64 F(I64 *p,I64 *r){I64 *q=p;p=r;*q+=2;return \
       *q;}F(&A,&B);"
      42L
      [ (0, "RDI"); (1, "R10") ]
      ~saved:[ "RSI"; "RDI"; "R10" ];
    case "explicit R12 pointer request overrides automatic nonpointer filtering"
      "I64 V=42;I64 F(I64 reg R12 *p){return *p;}F(&V);" 42L
      [ (0, "R12") ];
    case "explicit general pointer home leaves the other general bank available"
      "I64 V=42;I64 F(I64 reg RDI *p,I8 x){x+=1;return x+*p;}F(&V,127);" 170L
      [ (0, "RDI"); (1, "RSI") ];
  ]

(* First address fixture: x has a genuine address and one direct read,
   score 3 / LEA 1; y and p each have score 4 / LEA 0. Sorting precedes x's
   exclusion, leaving p then y. A local x initializer adds 2 but cannot cancel
   the address balance. These are source ledgers, not hosted load counts. *)
let address_cases =
  [
    case
      ~locals:[ ("p", Some "RSI") ]
      ~saved:[ "RSI"; "RDI" ]
      "genuine address retains parameter width while independent members \
       allocate"
      "I64 F(I8 x,I8 y){I8 *p=&x;*p+=1;y+=1;return x+y;}F(127,127);" 0L
      [ (1, "RDI") ];
    case
      ~locals:[ ("p", Some "RSI") ]
      ~saved:[ "RSI"; "RDI" ]
      "plain reg address escape remains stack without losing pointer allocation"
      "I64 F(I8 reg x,I8 y){I8 *p=&x;*p+=1;y+=1;return x+y;}F(127,127);" 0L
      [ (1, "RDI") ];
    case "helper address argument retains scalar stack home"
      "I64 Bump(I8 *p){*p+=1;return 0;}I64 F(I8 x,I8 y){Bump(&x);y+=1;return \
       x+y;}F(127,127);"
      0L
      [ (1, "RSI") ];
    case
      ~stores:[ (0, 8) ]
      "constant-dead helper address excludes the member before dead-code \
       removal"
      "I64 Touch(I8 *p){return *p;}I64 F(I8 x,I8 \
       y){if(0)Touch(&x);x+=1;y+=1;return x+y;}F(127,127);"
      0L
      [ (1, "RSI") ];
    case
      ~stores:[ (0, 8) ]
      "star address does not cancel surviving source IC_ADDR"
      "I64 F(I8 x,I8 y){x+=1;y+=1;return *(&x)+y;}F(127,127);" 0L
      [ (1, "RSI") ];
    case
      ~locals:[ ("x", None); ("y", Some "RSI"); ("p", Some "RDI") ]
      ~saved:[ "RSI"; "RDI" ]
      "address-taken local stays narrow next to allocated local"
      "I64 F(){I8 x=127;I8 y=127;I8 *p=&x;*p+=1;y+=1;return x+y;}F();" 0L [];
    case
      "address filtering reduces seven scored members to six general survivors"
      "I64 Touch(I8 *p){return *p;}I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 \
       x){if(0)Touch(&x);a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;return \
       a+b+c+d+e+f;}F(127,127,127,127,127,127,0);"
      768L
      [ (3, "RSI"); (4, "RDI"); (5, "R10"); (0, "R11"); (1, "R14"); (2, "R15") ];
  ]

(* CInit.HC:8 declares Bool as RT_I8 with size 1. OptPass3 suppresses its
   widening warning (:575,603), not signed-byte entry normalization or the
   selected register's full-word update. Bool is not a canonical 0/1 type. *)
let bool_cases =
  [
    case "Bool non-one parameter is a signed byte before allocation"
      "I64 F(Bool x){return x;}F(255);" (-1L)
      [ (0, "RSI") ];
    case "selected Bool keeps widened result rather than canonical truth"
      "I64 F(Bool x){x+=1;return x;}F(127);" 128L
      [ (0, "RSI") ];
    case
      ~stores:[ (0, 8) ]
      "noreg Bool wraps as signed I8"
      "I64 F(Bool noreg x){x+=1;return x;}F(127);" (-128L) [];
    case "Bool value two is not normalized to true"
      "I64 F(Bool x){return x;}F(2);" 2L
      [ (0, "RSI") ];
    case "Bool input 128 sign extends before a widened update"
      "I64 F(Bool x){x+=1;return x;}F(128);" (-127L)
      [ (0, "RSI") ];
    case
      ~locals:[ ("x", Some "RSI") ]
      ~saved:[ "RSI" ] "selected Bool local retains non-one widened result"
      "I64 F(){Bool x=127;x+=1;return x;}F();" 128L [];
    case
      ~locals:[ ("x", None) ]
      "noreg Bool local stores a signed byte"
      "I64 F(){Bool noreg x=127;x+=1;return x;}F();" (-128L) [];
  ]

let uninitialized_cases =
  [
    case
      ~locals:[ ("unused", Some "RSI") ]
      ~saved:[ "RSI" ] "zero-use forced scalar receives a home without a read"
      "I64 F(){I64 reg unused;return 42;}F();" 42L [];
    case
      ~locals:[ ("unused", Some "RSI") ]
      ~saved:[ "RSI" ]
      "unreachable uninitialized forced declaration remains a candidate"
      "I64 F(){if(0){I64 reg unused;}return 42;}F();" 42L [];
    case
      ~locals:[ ("unused", Some "RSI") ]
      ~saved:[ "RSI" ] "zero-use forced pointer receives a descriptor home"
      "I64 F(){I64 reg * unused;return 42;}F();" 42L [];
    case
      ~locals:[ ("unused", None) ]
      ~unknown:[ "unused" ]
      "zero-use ordinary local is trimmed after membership authentication"
      "I64 F(){I64 unused;return 42;}F();" 42L [];
    case
      ~locals:[ ("x", Some "RSI") ]
      ~saved:[ "RSI" ] "later assignment initializes an allocated scalar"
      "I64 F(){I8 x;x=127;x+=1;return x;}F();" 128L [];
    case
      ~locals:[ ("x", None); ("p", Some "RDI"); ("y", Some "RSI") ]
      ~saved:[ "RSI"; "RDI" ]
      "address-only uninitialized scalar is initialized through owned pointer"
      "I64 F(){I8 x;I8 *p=&x;I8 y=127;*p=127;*p+=1;y+=1;return x+y;}F();" 0L [];
    case
      ~locals:[ ("unused", Some "R12") ]
      ~saved:[ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14" ]
      "unused forced local participates in pressure before reads are permitted"
      "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f){I64 reg \
       unused;a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;return \
       a+b+c+d+e+f;}F(127,127,127,127,127,127);"
      768L
      [ (4, "R13"); (5, "RSI"); (3, "RDI"); (0, "R10"); (1, "R11"); (2, "R14") ];
  ]

let cases =
  List.map (mixed_case ~scalar_type:"I8") [ 7; 8; 9 ]
  @ [ mixed_case ~scalar_type:"Bool" 9; all_pointers ]
  @ pointer_cases @ address_cases @ bool_cases @ uninitialized_cases

module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Symbol = Semantic_symbol

let lowered mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"owned-member-proof.hc" ~contents
  in
  match compile_integer_program session ~config:(config mode) ~source with
  | Error diagnostics -> failwith (contents ^ ": " ^ diagnostic_text diagnostics)
  | Ok program ->
      let fn =
        integer_program_functions program.value
        |> List.find (fun (fn : Ir_integer_interpreter.function_definition) ->
            Symbol.name (Body.symbol fn.body) = "F")
      in
      (integer_program_runtime_calls program.value, fn)

let register_number r =
  List.assoc r
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

let semantic_placement mode case =
  let runtime, fn = lowered mode case.source in
  let proof =
    match
      Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body
    with
    | Some p -> p
    | None -> failwith (case.label ^ ": missing owned-member proof")
  in
  let assert_location location expected =
    let symbol = Frame.location_symbol location in
    let expected = Option.map register_number expected in
    let selection =
      if Option.is_some expected then Analysis.Register_candidate
      else if List.mem (Symbol.name symbol) case.unknown then Analysis.Unknown
      else Analysis.Stack
    in
    require
      (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol
       = expected
      && Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
         = Some selection)
      (case.label ^ ": wrong source-verified member home for "
     ^ Symbol.name symbol)
  in
  Frame.function_locations fn.frame
  |> List.filter (fun location ->
      Frame.location_kind location = Frame.Named_parameter)
  |> List.iteri (fun index location ->
      assert_location location (List.assoc_opt index case.entries));
  List.iter
    (fun (name, expected) ->
      let location =
        Frame.function_locations fn.frame
        |> List.find (fun location ->
            Symbol.name (Frame.location_symbol location) = name)
      in
      assert_location location expected)
    case.locals

let diagnostic code report =
  require
    (member "outcome" report = `String "error"
    && List.exists
         (fun d -> member "code" d = `String code)
         (member "diagnostics" report |> to_list))
    ("expected " ^ code ^ ": " ^ Yojson.Safe.to_string report)

let fault_cases =
  [
    ( "uninitialized selected scalar is not readable",
      "I64 F(){I64 reg x;return x;}F();",
      "HCIRVM0012" );
    ( "uninitialized selected pointer is not a descriptor",
      "I64 F(){I64 reg * p;return *p;}F();",
      "HCIRVM0012" );
    ( "initialized descriptor does not initialize the target",
      "I64 F(){I64 x;I64 *p=&x;return *p;}F();",
      "HCIRVM0012" );
    ( "selected pointer one-past dereference retains object bounds",
      "I64 V=42;I64 F(I64 *p){return p[1];}F(&V);",
      "HCIRVM0019" );
    ( "selected pointer negative index retains object bounds",
      "I64 V=42;I64 F(I64 *p){return p[-1];}F(&V);",
      "HCIRVM0019" );
  ]

let rejection_cases =
  [
    ( "explicit address-taken scalar",
      "I64 F(I64 reg RSI x){I64 *p=&x;return *p;}F(42);" );
    ( "explicit address retained through star-address",
      "I64 F(I64 reg RSI x){return *(&x);}F(42);" );
    ( "explicit address retained in constant-dead source arm",
      "I64 Touch(I64 *p){return *p;}I64 F(I64 reg RSI x){if(0)Touch(&x);return \
       x;}F(42);" );
    ( "owned pointer cannot be exposed as raw integer",
      "I64 V=42;I64 F(I64 *p){return p(I64);}F(&V);" );
    ( "raw integer cannot fabricate a descriptor",
      "I64 F(){I64 *p=42;return *p;}F();" );
    ( "reserved instruction register remains deferred",
      "I64 F(I64 reg RAX a){return a;}F(42);" );
    ( "pointer return cannot smuggle an expired local descriptor",
      "I64 *Make(){I64 x=42;return &x;}I64 F(){I64 *p=Make();return *p;}F();" );
    ( "pointer-to-pointer address requires its separate ownership proof",
      "I64 V=42;I64 F(I64 *p){I64 **q=&p;return **q;}F(&V);" );
  ]

let completed_word expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      require
        (Option.map (fun (word : Image.word) -> word.bits) result.final_value
        = Some expected)
        "wrong retained owned-member result";
      result.executed_steps
  | Image.Fault _ -> failwith "unexpected retained owned-member fault"

let with_retained mode source action =
  let compiled = image mode source in
  let retained = Runtime.retain compiled |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () -> action compiled retained)

let exact_limits mode =
  let fixture = List.nth pointer_cases 4 in
  with_retained mode fixture.source (fun compiled retained ->
      placement fixture compiled;
      let ir = Image.ir_instructions compiled
      and code = Image.code_bytes compiled
      and stack = Image.frame_bytes compiled in
      ignore
        (compile ~max_ir_instructions:ir ~max_code_bytes:code
           ~max_stack_bytes:stack mode fixture.source
        |> Result.map_error diagnostic_text
        |> checked);
      require
        (Result.is_error
           (compile ~max_ir_instructions:(ir - 1) mode fixture.source)
        && Result.is_error
             (compile ~max_code_bytes:(code - 1) mode fixture.source)
        && Result.is_error
             (compile ~max_stack_bytes:(stack - 1) mode fixture.source))
        "owned-member compile budgets accept one below exact size";
      let run ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes max_steps
          =
        Runtime.execute_retained_report ?max_frame_bytes ?max_call_depth
          ?max_active_stack_bytes ~max_steps retained
      in
      let steps = run 100000 |> completed_word 170L in
      ignore
        (run ~max_frame_bytes:72 ~max_call_depth:3 steps |> completed_word 170L);
      ignore (run ~max_frame_bytes:71 steps |> fault Image.Frame_limit_exceeded);
      ignore (run ~max_call_depth:2 steps |> fault Image.Call_depth_exceeded);
      let short = run (steps - 1) |> fault Image.Step_limit_exceeded in
      require
        (short.executed_steps = steps - 1 && Option.is_some short.span)
        "owned-member step fault lost exact charge or source span";
      (* Physical frame expansion is backend-private, unlike the logical 3x24
       source-frame budget. Find its first admitted byte, then check both sides
       and recovery on the same original sealed image. *)
      let rec first_success low high =
        if low = high then low
        else
          let mid = low + ((high - low) / 2) in
          match
            Runtime.outcome (run ~max_active_stack_bytes:mid steps) |> checked
          with
          | Image.Completed _ -> first_success low mid
          | Image.Fault f when f.kind = Image.Native_stack_limit_exceeded ->
              first_success (mid + 1) high
          | Image.Fault _ -> failwith "unexpected active-stack boundary fault"
      in
      let active = first_success 1 Runtime.default_active_stack_bytes in
      ignore (run ~max_active_stack_bytes:active steps |> completed_word 170L);
      ignore
        (run ~max_active_stack_bytes:(active - 1) steps
        |> fault Image.Native_stack_limit_exceeded);
      for _ = 1 to 3 do
        ignore (run steps |> completed_word 170L)
      done)

let recoveries mode =
  List.iter
    (fun (source, kind, expected) ->
      with_retained mode source (fun _ retained ->
          let run () =
            Runtime.execute_retained_report ~max_steps:100000 retained
          in
          let failed = run () |> fault kind in
          require
            (Option.is_some failed.span && failed.function_name = Some "F")
            "owned-pointer fault lost function/source ownership";
          ignore (run () |> completed_word expected)))
    [
      ( "I64 Attempts=0;I64 V=42;I64 F(I64 *p,I64 noreg i){return \
         p[i];}F(&V,1-Attempts++);",
        Image.Address_out_of_bounds,
        42L );
      ( "I64 Attempts=0;I64 F(I64 noreg yes){I64 x;I64 \
         *p=&x;if(yes)*p=42;return *p;}F(Attempts++);",
        Image.Uninitialized_read,
        42L );
      ( "I64 Attempts=0;I64 V=42;I64 F(I64 *p,I64 noreg divisor){return \
         *p/divisor;}F(&V,Attempts++);",
        Image.Division_by_zero,
        42L );
    ]

let public_limits mode =
  let fixture = List.nth pointer_cases 4 in
  List.iter
    (fun target ->
      let report = invoke ~target ~mode fixture.label fixture.source in
      value fixture report;
      let steps = member "executed_steps" report |> to_int in
      value fixture
        (invoke ~target ~mode fixture.label fixture.source
           ~options:
             [
               "--step-limit=" ^ string_of_int steps;
               "--frame-byte-limit=72";
               "--call-depth-limit=3";
             ]);
      List.iter
        (fun (option, code) ->
          invoke ~status:1 ~target ~mode ~options:[ option ] fixture.label
            fixture.source
          |> diagnostic code)
        [
          ("--step-limit=" ^ string_of_int (steps - 1), "HCIRVM0007");
          ("--frame-byte-limit=71", "HCIRVM0011");
          ("--call-depth-limit=2", "HCIRVM0015");
        ])
    [ "ir"; "host-jit"; "host-jit-task" ]

let failures = ref []

let check label action =
  try action ()
  with error ->
    failures := (label ^ ": " ^ Printexc.to_string error) :: !failures

let () =
  List.iter
    (fun mode ->
      List.iter
        (fun case ->
          check
            (mode ^ " " ^ case.label)
            (fun () ->
              semantic_placement mode case;
              let compiled = image mode case.source in
              placement case compiled;
              List.iter
                (fun target ->
                  let report = invoke ~target ~mode case.label case.source in
                  value case report;
                  if target = "host-jit" then
                    placement_instructions case
                      (section_instructions case.label "F"
                         (report |> member "native" |> member "image"
                        |> member "native_sections")))
                (if native then [ "ir"; "host-jit"; "host-jit-task" ]
                 else [ "ir" ])))
        cases;
      List.iter
        (fun (label, source, code) ->
          List.iter
            (fun target ->
              check
                (mode ^ " " ^ target ^ " " ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode label source |> diagnostic code))
            (if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]))
        fault_cases;
      List.iter
        (fun (label, source) ->
          List.iter
            (fun target ->
              check
                (mode ^ " " ^ target ^ " " ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode label source |> rejected label))
            ((if
                List.mem label
                  [
                    "explicit address-taken scalar";
                    "explicit address retained through star-address";
                    "explicit address retained in constant-dead source arm";
                    "reserved instruction register remains deferred";
                  ]
              then [ "ir" ]
              else [])
            @ if native then [ "host-jit"; "host-jit-task" ] else []))
        rejection_cases;
      if native then (
        check (mode ^ " exact owned-member quotas") (fun () ->
            exact_limits mode);
        check (mode ^ " retained fault recovery") (fun () -> recoveries mode);
        check (mode ^ " public exact limits") (fun () -> public_limits mode)))
    modes;
  Printf.printf
    "Owned-member registers: %d cases, %d CLI reports (%s), %d failures\n"
    (List.length cases) !count
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
