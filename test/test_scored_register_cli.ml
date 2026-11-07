open Yojson.Safe.Util
open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* Source-derived expectations from TempleOS c26482bb, not new runtime captures.
   OptPass3.HC scores each direct frame occurrence twice and seeds plain reg
   with I64_MAX/2. Kernel/QSort.HC:60-87 supplies the unstable tie order. Sorting
   includes zero-score members; only then does OptPass3.HC:538-553 trim them.
   More than six survivors opens R12/R13 before RSI,RDI,R10,R11,R14,R15.
   All eight allocated slots have physical full-word homes in this backend. *)
let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-scored-register-" suffix in
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
let private_homes = ref 0
let stack_homes = ref 0
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
                  @ [ "--code-byte-limit=1048576" ]
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

let checked label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ error)

let compile mode source =
  let session = Session.create () in
  let input =
    Session.add_source session ~path:"scored-register-pressure.hc"
      ~contents:source
  in
  let config =
    Preprocessor.Config.create
      ~compilation_mode:
        (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
      ()
    |> checked "configuration"
  in
  match
    Native_program.compile ~max_code_bytes:1_048_576 session ~config
      ~source:input
  with
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

let operands (instruction : Disassembly.instruction) =
  List.map
    (fun (operand : Disassembly.operand) -> operand.text)
    instruction.operands

let parameter_offset index = Int64.of_int (16 + (8 * index))

let frame_operand offset (operand : Disassembly.operand) =
  String.ends_with ~suffix:"[RBP]" operand.text && operand.value = Some offset

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

type case = {
  name : string;
  source : string;
  expected : int64;
  entries : (int * string) list;
  private_entries : int list;
  stack_entries : (int * int) list;
  saved : string list;
  directive_body : string option;
}

let case ?(entries = []) ?(private_entries = []) ?(stack_entries = []) ?saved
    ?directive_body name source expected =
  let saved =
    Option.value saved ~default:(List.map snd entries |> List.sort_uniq compare)
  in
  {
    name;
    source;
    expected;
    entries;
    private_entries;
    stack_entries;
    saved;
    directive_body;
  }

let assert_mapping label case instructions =
  let pairs = entry_pairs instructions in
  let actual =
    List.filter_map
      (fun ((source : Disassembly.operand), (store : Disassembly.instruction))
         ->
        match operands store with
        | [ destination; "U64 RAX" ] when store.opcode = "MOV" ->
            List.find_map
              (fun register ->
                if destination = "U64 " ^ register then
                  Some
                    ( Int64.to_int (Int64.sub (Option.get source.value) 16L) / 8,
                      register )
                else None)
              supported
        | _ -> None)
      pairs
  in
  let show pairs =
    String.concat ","
      (List.map
         (fun (index, register) -> Printf.sprintf "%d:%s" index register)
         pairs)
  in
  require
    (List.sort_uniq compare actual = List.sort_uniq compare case.entries)
    (label ^ ": parameter entry mapping " ^ show actual ^ "; expected "
   ^ show case.entries);
  List.iter
    (fun (_, register) ->
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
      let expected =
        bytes
          (Printf.sprintf "%02x89%02x"
             (0x48 lor (number lsr 3))
             (0xc0 lor (number land 7)))
      in
      require
        (List.exists
           (fun (_, (instruction : Disassembly.instruction)) ->
             instruction.bytes = expected)
           pairs)
        (label ^ ": missing REX.W entry MOV for " ^ register);
      incr placements)
    case.entries;
  List.iter
    (fun index ->
      require
        (List.exists
           (fun (source, (store : Disassembly.instruction)) ->
             frame_operand (parameter_offset index) source
             && store.opcode = "MOV"
             &&
             match store.operands with
             | [ destination; word ] ->
                 frame_operand (parameter_offset index) destination
                 && destination.size = 64 && word.text = "U64 RAX"
             | _ -> false)
           pairs)
        (Printf.sprintf
           "%s: parameter %d lacks its full-word private entry home" label index);
      incr private_homes)
    case.private_entries;
  List.iter
    (fun (index, width) ->
      require
        (List.exists
           (fun (instruction : Disassembly.instruction) ->
             instruction.opcode = "MOV"
             &&
             match instruction.operands with
             | [ destination; _ ] ->
                 frame_operand (parameter_offset index) destination
                 && destination.size = width
             | _ -> false)
           instructions)
        (Printf.sprintf "%s: spilled parameter %d lacks its %d-bit store" label
           index width);
      incr stack_homes)
    case.stack_entries;
  let actual_saved =
    List.filter (fun r -> frame_moves ~save:true r instructions <> []) supported
  in
  require
    (List.sort compare actual_saved = List.sort compare case.saved)
    (label ^ ": saves "
    ^ String.concat "," actual_saved
    ^ "; expected "
    ^ String.concat "," case.saved);
  List.iter
    (fun register ->
      let saves = frame_moves ~save:true register instructions
      and restores = frame_moves ~save:false register instructions in
      require
        (List.length saves = 1 && List.length restores = 1)
        (label ^ ": expected one save and restore for " ^ register);
      let save = List.hd saves and restore = List.hd restores in
      require
        (String.sub save.bytes 2 (String.length save.bytes - 2)
        = String.sub restore.bytes 2 (String.length restore.bytes - 2))
        (label ^ ": register restore changed its frame slot"))
    case.saved;
  if case.saved = [] then incr exclusions

type integer_type = { name : string; bits : int; signed : bool }

let integer_types =
  List.concat_map
    (fun bits ->
      List.map
        (fun signed ->
          {
            name = (if signed then "I" else "U") ^ string_of_int bits;
            bits;
            signed;
          })
        [ true; false ])
    [ 8; 16; 32; 64 ]

let normalize type_ word =
  let shift = 64 - type_.bits in
  let low = Int64.shift_left word shift in
  if type_.signed then Int64.shift_right low shift
  else Int64.shift_right_logical low shift

let high type_ =
  if type_.bits = 64 then if type_.signed then Int64.max_int else -1L
  else
    Int64.pred
      (Int64.shift_left 1L (type_.bits - if type_.signed then 1 else 0))

let names count = List.init count (fun i -> String.make 1 (Char.chr (97 + i)))

(* These literal orders were traced through the pinned copied-pivot QSort2b.
   Do not obtain expected placements from Scored_register_allocation. *)
let equal_order = function
  | 2 -> [ 1; 0 ]
  | 6 -> [ 3; 4; 5; 0; 1; 2 ]
  | 7 -> [ 4; 5; 6; 3; 0; 1; 2 ]
  | 8 -> [ 5; 4; 7; 6; 1; 0; 3; 2 ]
  | 9 -> [ 6; 5; 8; 7; 4; 1; 0; 3; 2 ]
  | _ -> invalid_arg "unsupported literal tie fixture"

let assignments count =
  let bank =
    (if count > 6 then [ "R12"; "R13" ] else [])
    @ [ "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ]
  in
  List.mapi
    (fun rank index -> (index, List.nth_opt bank rank))
    (equal_order count)

let weighted_expression weights =
  List.mapi
    (fun index weight ->
      Printf.sprintf "%s*%d" (List.nth (names 9) index) weight)
    weights
  |> String.concat "+"

let pressure_definition ?(qualifier = "") ?(updates = "+=1") type_ count weights
    =
  let members = names count in
  Printf.sprintf "I64 F(%s){%sreturn %s;}"
    (String.concat ","
       (List.map
          (fun name -> type_.name ^ " " ^ qualifier ^ " " ^ name)
          members))
    (String.concat ""
       (List.map
          (fun name ->
            name
            ^ (if updates = "self assignment" then "=" ^ name ^ "+1"
               else updates)
            ^ ";")
          members))
    (weighted_expression weights)

let pressure_call count incoming =
  "F("
  ^ String.concat ","
      (List.init count (fun _ -> Printf.sprintf "0x%Lx" incoming))
  ^ ")"

let pressure_case ?(qualifier = "") ?(updates = "+=1") ?incoming ?weights type_
    count =
  let incoming = Option.value incoming ~default:(high type_) in
  let weights =
    Option.value weights ~default:(List.init count (fun i -> i + 1))
  in
  let allocations = assignments count in
  let next = Int64.succ (normalize type_ incoming) in
  let expected =
    List.mapi
      (fun index weight ->
        let word =
          if List.assoc index allocations = None then normalize type_ next
          else next
        in
        Int64.mul word (Int64.of_int weight))
      weights
    |> List.fold_left Int64.add 0L
  in
  let entries =
    List.filter_map
      (fun (index, register) ->
        Option.bind register (fun r ->
            if List.mem r supported then Some (index, r) else None))
      allocations
  in
  let private_entries = [] in
  let stack_entries =
    List.filter_map
      (fun (index, register) ->
        if register = None then Some (index, type_.bits) else None)
      allocations
  in
  let name =
    Printf.sprintf "%s %s %d tied parameters %s input 0x%Lx weights %s"
      type_.name qualifier count updates incoming
      (String.concat "," (List.map string_of_int weights))
  in
  case ~entries ~private_entries ~stack_entries name
    (pressure_definition ~qualifier ~updates type_ count weights
    ^ pressure_call count incoming
    ^ ";")
    expected

let width_cases =
  List.concat_map
    (fun type_ ->
      List.map (pressure_case type_) [ 2; 6; 7; 8; 9 ]
      @ [
          pressure_case ~incoming:(if type_.bits = 64 then 42L else -1L) type_ 9;
          pressure_case ~qualifier:"reg" type_ 9;
        ])
    integer_types

let observation_cases =
  List.concat_map
    (fun type_ ->
      List.init 9 (fun observed ->
          pressure_case
            ~weights:(List.init 9 (fun i -> if i = observed then 2 else 1))
            type_ 9)
      @ List.map
          (fun updates -> pressure_case ~updates type_ 9)
          [ "++"; "self assignment" ])
    (List.filter (fun type_ -> type_.bits = 8) integer_types)

let allocation_cases =
  [
    case
      ~entries:[ (2, "RSI"); (3, "RDI") ]
      "zero-score candidates stay in sort before trimming"
      "I64 F(I64 a,I64 b,I64 c,I64 d){return c+d;}F(0,0,20,22);" 42L;
    case
      ~entries:[ (0, "RSI"); (1, "RDI") ]
      "unequal read counts beat declaration order"
      "I64 F(I64 a,I64 b){return a+a+a+b;}F(10,12);" 42L;
    case
      ~entries:[ (1, "RSI"); (0, "RDI") ]
      "two plain reg candidates use the unstable tie order"
      "I64 F(I8 reg a,I8 reg b){a+=1;b+=1;return a+b;}F(127,127);" 256L;
    case
      ~entries:[ (1, "RSI"); (0, "RDI") ]
      "plain reg priority outranks more ordinary reads"
      "I64 F(I64 a,I64 reg b){return a+a+a+a+b;}F(10,2);" 42L;
    case
      ~entries:[ (0, "RSI"); (2, "R10") ]
      "unused plain reg consumes its allocated general slot"
      "I64 F(I8 reg a,I8 reg unused,I8 b){a+=1;b+=1;return a+b;}F(127,0,127);"
      256L;
    case
      ~entries:[ (6, "RSI") ]
      "unused ordinary parameters do not activate R12 R13"
      "I64 F(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I8 reg g){g+=1;return \
       g;}F(0,0,0,0,0,0,127);"
      128L;
    case
      ~entries:[ (0, "RSI") ]
      "one used ordinary parameter survives zero trimming"
      "I64 F(I8 a,I64 unused){a+=1;return a;}F(127,0);" 128L;
    case
      ~entries:[ (0, "RSI") ]
      ~stack_entries:[ (1, 8) ]
      "noreg is excluded from scoring and preserves declared width"
      "I64 F(I8 a,I8 noreg b){a+=1;b+=1;return a+b;}F(127,127);" 0L;
    case ~saved:[ "RSI"; "RDI" ]
      "initializer counts even for an otherwise unused local"
      "I64 F(){I64 unused=0;I8 a=127;a+=1;return a;}F();" 128L;
    case
      ~saved:[ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15" ]
      "initialized equal-score locals open the nonpointer bank"
      "I64 F(){I8 a=127;I8 b=127;I8 c=127;I8 d=127;I8 e=127;I8 f=127;I8 \
       g=127;I8 h=127;I8 \
       i=127;a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;g+=1;h+=1;i+=1;return \
       a+b*2+c*3+d*4+e*5+f*6+g*7+h*8+i*9;}F();"
      4992L;
    case
      ~entries:
        [
          (8, "R12");
          (7, "R13");
          (6, "RSI");
          (5, "RDI");
          (2, "R14");
          (1, "R15");
          (4, "R10");
          (3, "R11");
        ]
      ~stack_entries:[ (0, 8) ]
      "unequal scores identify the lowest scoring spill"
      ("I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I8 h,I8 \
        i){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;g+=1;h+=1;i+=1;return "
      ^ String.concat "+"
          (List.mapi
             (fun i name ->
               "(" ^ String.concat "+" (List.init (i + 1) (fun _ -> name)) ^ ")")
             (names 9))
      ^ ";}F(127,127,127,127,127,127,127,127,127);")
      5504L;
    case
      ~entries:[ (3, "RSI"); (0, "R14"); (1, "R15"); (4, "R10"); (5, "R11") ]
      ~stack_entries:[ (2, 8) ]
      "six survivors do not open R12 R13 when general slots are reserved"
      "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I64 reg RDI \
       r){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;return \
       a+b+c+d+e+f;}F(127,127,127,127,127,127,0);"
      512L;
    case
      ~entries:
        [
          (4, "R13");
          (5, "RSI");
          (6, "RDI");
          (1, "R14");
          (2, "R15");
          (3, "R10");
          (0, "R11");
        ]
      "unused explicit R12 reserves the first nonpointer slot"
      "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I64 reg R12 \
       r){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;g+=1;return \
       a+b+c+d+e+f+g;}F(127,127,127,127,127,127,127,0);"
      896L;
    case
      ~entries:
        [
          (4, "RSI"); (5, "RDI"); (0, "R14"); (1, "R15"); (6, "R10"); (3, "R11");
        ]
      ~stack_entries:[ (2, 8) ]
      "unused explicit R12 R13 reservations force a seven-member spill"
      "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I64 reg R12 r,I64 reg R13 \
       s){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;g+=1;return \
       a+b+c+d+e+f+g;}F(127,127,127,127,127,127,127,0,0);"
      640L;
    case
      ~entries:[ (4, "R13"); (5, "RDI"); (0, "R15"); (6, "R10"); (3, "R11") ]
      ~stack_entries:[ (1, 8); (2, 8) ]
      "unused reservations in both banks preserve winner order"
      "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I64 reg R12 r,I64 reg RSI \
       s,I64 reg R14 t){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;g+=1;return \
       a+b*2+c*3+d*4+e*5+f*6+g*7;}F(127,127,127,127,127,127,127,0,0,0);"
      2304L;
    case
      ~entries:
        [
          (4, "R13"); (5, "RSI"); (3, "RDI"); (2, "R14"); (0, "R10"); (1, "R11");
        ]
      "unused plain reg opens and consumes the nonpointer bank"
      "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 reg \
       unused){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;return \
       a+b+c+d+e+f;}F(127,127,127,127,127,127,0);"
      768L;
  ]

let fallback_cases =
  [
    case "branch keeps two-candidate allocation conservative"
      "I64 F(I64 a,I64 b){if(a<0)return b;return a+b;}F(20,22);" 42L;
    case "loop keeps two-candidate allocation conservative"
      "I64 F(I64 a,I64 b){while(a<20)a++;return a+b;}F(19,22);" 42L;
    case "named call keeps two-candidate allocation conservative"
      "I64 G(I64 noreg x){return x;}I64 F(I64 a,I64 b){return G(a)+b;}F(20,22);"
      42L;
    case "cast keeps two-candidate allocation conservative"
      "I64 F(I64 a,I64 b){return a(I8)+b;}F(20,22);" 42L;
    case "owned pointer keeps allocation conservative"
      "I64 F(I64 a,I64 b){I64 *p=&a;return *p+b;}F(20,22);" 42L;
    case "aggregate neighbor keeps allocation conservative"
      "class Pair {I64 value;};I64 F(I64 a,I64 b){Pair p;p.value=a;return \
       p.value+b;}F(20,22);"
      42L;
    case "static neighbor keeps allocation conservative"
      "I64 F(I64 a,I64 b){static I64 s=0;s+=1;return a+b+s;}F(19,22);" 42L;
    case "consumed assignment keeps physical ordering conservative"
      "I64 F(I64 a,I64 b){return (a=b)+a;}F(0,21);" 42L;
  ]

let option_cases =
  List.concat_map
    (fun qualifier ->
      let definition =
        Printf.sprintf "I64 F(I8 %s a,I8 %s b){a+=1;b+=1;return a+b;}" qualifier
          qualifier
      in
      List.map
        (fun (name, before, after, expected, entries, stack_entries) ->
          let body = before ^ definition ^ after ^ "F(127,127);" in
          case ~entries ~stack_entries ~directive_body:body
            (qualifier ^ " " ^ name)
            ("#exe {" ^ before ^ definition ^ after
           ^ "StreamPrint(\"%d;\",F(127,127));}")
            expected)
        [
          ( "NO_REG_VAR disables both scored candidates",
            "Option(34,1);",
            "",
            -256L,
            [],
            [ (0, 8); (1, 8) ] );
          ( "later enable does not replace definition options",
            "Option(34,1);",
            "Option(34,0);",
            -256L,
            [],
            [ (0, 8); (1, 8) ] );
          ( "later disable preserves original scored mapping",
            "Option(34,0);",
            "Option(34,1);",
            256L,
            [ (1, "RSI"); (0, "RDI") ],
            [] );
        ])
    [ ""; "reg" ]
  @ [
      case
        ~entries:[ (0, "R12") ]
        ~stack_entries:[ (1, 8) ]
        ~directive_body:
          "Option(34,1);I64 F(I8 reg R12 a,I8 reg b){a+=1;b+=1;return \
           a+b;}F(127,127);"
        "explicit register overrides NO_REG_VAR while plain reg remains stack"
        "#exe {Option(34,1);I64 F(I8 reg R12 a,I8 reg b){a+=1;b+=1;return \
         a+b;}StreamPrint(\"%d;\",F(127,127));}"
        0L;
    ]

let example_case =
  case
    ~entries:
      [
        (6, "R12");
        (5, "R13");
        (8, "RSI");
        (7, "RDI");
        (0, "R14");
        (3, "R15");
        (4, "R10");
        (1, "R11");
      ]
    ~stack_entries:[ (2, 8) ]
    "maintained scored pressure example"
    (read "../examples/scored-register-pressure.hc")
    4992L

let outer_call_case =
  let type_ = List.hd integer_types in
  let weights = List.init 9 (fun i -> i + 1) in
  let definition = pressure_definition type_ 9 weights in
  {
    example_case with
    name = "outer explicit register survives two scored pressure calls";
    source =
      definition
      ^ "I64 Outer(I64 reg R12 keeper){return \
         keeper+F(127,127,127,127,127,127,127,127,127)+keeper+F(127,127,127,127,127,127,127,127,127)+keeper;}Outer(14);";
    expected = 10026L;
  }

let retained_fault_recovery mode =
  let label = "scored pressure retained image " ^ mode in
  with_retained mode outer_call_case.source (fun image retained ->
      assert_mapping label outer_call_case
        (function_instructions label "F" image);
      let steps =
        execute retained 100_000 |> completed label outer_call_case.expected
      in
      let limits =
        [ 1; 2; steps - 1 ]
        @ List.init 31 (fun i -> max 1 (steps * (i + 1) / 32))
        |> List.sort_uniq compare
      in
      let owned = ref 0 in
      List.iter
        (fun limit ->
          (match execute retained limit with
          | Program.Fault fault ->
              require
                (fault.kind = Program.Step_limit_exceeded
                && fault.executed_steps = limit)
                (label ^ ": wrong bounded fault");
              if fault.function_name = Some "F" then incr owned
          | Program.Completed _ -> failwith (label ^ ": bounded run completed"));
          ignore
            (execute retained steps |> completed label outer_call_case.expected);
          incr retained_recoveries)
        limits;
      require (!owned > 0) (label ^ ": no fault interrupted scored F"));
  let definition =
    pressure_definition (List.hd integer_types) 9 (List.init 9 (fun i -> i + 1))
  in
  let division =
    String.sub definition 0 (String.length definition - 2) ^ "/(count-1);}"
  in
  (* The extra noreg parameter does not enter the scored candidate list. *)
  let division =
    let close = String.index division ')' in
    String.sub division 0 close
    ^ ",I64 noreg count"
    ^ String.sub division close (String.length division - close)
  in
  let source =
    "I64 Attempts=0;" ^ division
    ^ "Attempts++;F(127,127,127,127,127,127,127,127,127,Attempts);"
  in
  with_retained mode source (fun image retained ->
      assert_mapping label example_case (function_instructions label "F" image);
      (match execute retained 100_000 with
      | Program.Fault fault ->
          require
            (fault.kind = Program.Division_by_zero
            && fault.function_name = Some "F")
            (label ^ ": expected scored-function division fault")
      | Program.Completed _ -> failwith (label ^ ": division did not fault"));
      ignore (execute retained 100_000 |> completed label 4992L);
      incr retained_recoveries)

(* Native_program.compile deliberately has no #exe executor. The public CLI
   below exercises directives in both outer modes through host-jit-task. The
   low-level byte harness requires JIT task mode: run the same definition/option
   ordering in that original directive task and inspect the fragment that
   actually calls F. Keep the original source callback; options are not replaced
   by test metadata. AOT directive execution is checked through the public CLI. *)
let directive_placement mode (case : case) body =
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
      assert_mapping label case instructions;
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
                    | Program.U64 -> Task.Native_dispatch.U64 word.bits
                    | Program.F64 -> Task.Native_dispatch.F64 word.bits)
                  execution.final_value)
           else Task.Native_dispatch.Unchanged)
    | Program.Fault _ -> failwith (label ^ ": unexpected native directive fault")
  in
  Fun.protect
    ~finally:(fun () -> Runtime.release_task_arena arena |> checked label)
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
    width_cases @ observation_cases @ allocation_cases @ fallback_cases
    @ option_cases
    @ [ example_case; outer_call_case ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (case : case) ->
          let label = case.name ^ " " ^ mode in
          (match case.directive_body with
          | Some body ->
              if native && mode = "jit" then directive_placement mode case body
          | None ->
              let image = compile mode case.source in
              assert_mapping label case (function_instructions label "F" image));
          let targets =
            if not native then [ "ir" ]
            else if Option.is_some case.directive_body then
              [ "ir"; "host-jit-task" ]
            else [ "ir"; "host-jit"; "host-jit-task" ]
          in
          List.iter
            (fun target ->
              invoke ~target ~mode label case.source
              |> value label case.expected)
            targets)
        cases;
      let source = "I64 F(I8 x){x+=1;return x/(x-x);}F(127);" in
      let label = "scored public arithmetic fault " ^ mode in
      let image = compile mode source in
      let expected = case ~entries:[ (0, "RSI") ] label source 0L in
      assert_mapping label expected (function_instructions label "F" image);
      List.iter
        (fun target ->
          invoke ~status:1 ~target ~mode label source
          |> fault label "HCIRVM0009")
        (if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]);
      if native then retained_fault_recovery mode)
    modes;
  Printf.printf
    "Scored register checks passed (%d CLI reports, %d member-specific \
     physical entries, %d private full-word entries, %d declared-width stores, \
     %d conservative cases, %d same-image fault recoveries).\n"
    !reports !placements !private_homes !stack_homes !exclusions
    !retained_recoveries
