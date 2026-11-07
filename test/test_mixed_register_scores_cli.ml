open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* SOURCE DEDUCTIONS, not TempleOS runtime captures. Reference commit:
   c26482bb6ad3f80106d28504ec5db3c6a360732c.
   PrsExp.HC:201-208,800-802 emits each direct read/write separately;
   PrsVar.HC:607-615 reparses a local initializer as an assignment.
   OptPass3.HC:119-135,187-199,238-245 scores each direct access twice.
   OptPass3.HC:29-42 excludes ordinary F64 BEFORE sorting and seeds plain
   reg with I64_MAX/2. QSort.HC:55-83 supplies unstable equal-score swaps.
   OptPass3.HC:536-612 trims after sorting and opens R12/R13 only above
   six surviving candidates, without subtracting explicit reservations.
   OptPass012 and BackFB identity rewrites preserve the source occurrences
   in this bounded homogeneous-F64 grammar. Self-move elimination is later.
   All expected member maps below are literal source deductions. No expected
   result or placement is obtained from the project's allocator. *)
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
  let path = Filename.temp_file "holyc-mixed-register-scores-" suffix in
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
    Session.add_source session ~path:"mixed-register-scores.hc" ~contents
  in
  Native_program.compile ?max_ir_instructions ?max_code_bytes session
    ~config:(config mode) ~source

let image mode source =
  compile mode source |> Result.map_error diagnostic_text |> checked |> fun x ->
  x.value

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
  entries : (int * string) list;
  stores : (int * int) list;
  saved : string list;
}

let case ?(kind = "i64") ?(stores = []) ?saved label source bits entries =
  let saved =
    Option.value saved ~default:(List.map snd entries |> List.sort_uniq compare)
  in
  { label; source; kind; bits; entries; stores; saved }

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

let placement case image =
  let instructions = function_instructions case.label "F" image in
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

let names = [ "a"; "b"; "x"; "c"; "d"; "e"; "f"; "g"; "h" ]

(* Literal QSort2b orders at equal score: 6 -> c,d,e,a,b,x;
   7 -> d,e,f,c,a,b,x; 8 -> e,d,g,f,b,a,c,x;
   9 -> f,e,h,g,d,b,a,c,x. *)
let equal_entries = function
  | 6 ->
      [ (3, "RSI"); (4, "RDI"); (5, "R10"); (0, "R11"); (1, "R14"); (2, "R15") ]
  | 7 ->
      [
        (4, "R12");
        (5, "R13");
        (6, "RSI");
        (3, "RDI");
        (0, "R10");
        (1, "R11");
        (2, "R14");
      ]
  | 8 ->
      [
        (5, "R12");
        (4, "R13");
        (7, "RSI");
        (6, "RDI");
        (1, "R10");
        (0, "R11");
        (3, "R14");
        (2, "R15");
      ]
  | 9 ->
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
  | _ -> invalid_arg "missing independently traced tie fixture"

(* Discarded x changes only x from seed+4 to seed+6. The remaining unstable
   tie order is f,e,g,h,b,a,d,c, not the old equal-order suffix. *)
let promoted_entries =
  [
    (2, "R12");
    (6, "R13");
    (5, "RSI");
    (7, "RDI");
    (8, "R10");
    (1, "R11");
    (0, "R14");
    (4, "R15");
  ]

let pressure_source ?(count = 9) ?(discard = false) ?(locals = false)
    ?(suffix_parameter = "") ?(suffix_argument = "") ?(returned = "x") type_
    initial =
  let members = List.filteri (fun i _ -> i < count) names in
  let parameters = if locals then [ "a"; "b"; "x" ] else members in
  let definition name =
    (if name = "x" then type_ else "F64") ^ " reg " ^ name
  in
  let body =
    if locals then
      "a=a;b=b;"
      ^ String.concat ""
          (List.mapi
             (fun i name ->
               Printf.sprintf "F64 reg %s=%d.5;%s;" name (i + 3) name)
             [ "c"; "d"; "e"; "f"; "g"; "h" ])
    else
      String.concat ""
        (List.filter_map
           (fun name ->
             if name = "x" then None else Some (name ^ "=" ^ name ^ ";"))
           members)
  in
  "I64 F("
  ^ String.concat "," (List.map definition parameters)
  ^ suffix_parameter ^ "){" ^ body ^ "x+=1;"
  ^ (if discard then "x;" else "")
  ^ "return " ^ returned ^ ";}F("
  ^ String.concat ","
      (List.map (fun n -> if n = "x" then initial else "1.5") parameters)
  ^ suffix_argument ^ ");"

let pressure_case ?(count = 9) ?(discard = false) ?(locals = false) type_
    initial selected spilled width =
  let entries = if discard then promoted_entries else equal_entries count in
  let saved = List.map snd entries in
  let stores =
    if locals then []
    else if discard then [ (3, 64) ]
    else if count = 9 then [ (2, width) ]
    else []
  in
  case ~stores ~saved
    (Printf.sprintf "%s %d equal-score members%s%s" type_ count
       (if discard then " plus discarded x" else "")
       (if locals then " with c-h initializers" else ""))
    (pressure_source ~count ~discard ~locals type_ initial)
    (if discard || count < 9 then selected else spilled)
    (if locals then List.filter (fun (i, _) -> i < 3) entries else entries)

let pressure_cases =
  List.concat_map
    (fun (type_, initial, selected, spilled, width) ->
      List.map
        (fun count -> pressure_case ~count type_ initial selected spilled width)
        [ 6; 7; 8; 9 ]
      @ [ pressure_case ~discard:true type_ initial selected spilled width ])
    [
      ("I8", "127", 128L, -128L, 8);
      ("U8", "255", 256L, 0L, 8);
      ("I16", "32767", 32768L, -32768L, 16);
      ("U16", "65535", 65536L, 0L, 16);
      ("I32", "2147483647", 2147483648L, -2147483648L, 32);
      ("U32", "4294967295", 4294967296L, 0L, 32);
    ]
  @ List.map
      (fun discard ->
        pressure_case ~locals:true ~discard "I8" "127" 128L (-128L) 8)
      [ false; true ]

let reservation_parameters =
  "F64 reg RSI r0,F64 reg RDI r1,F64 reg R10 r2,F64 reg R11 r3,F64 reg R14 \
   r4,F64 reg R15 r5"

let general_entries first =
  List.mapi
    (fun i r -> (first + i, r))
    [ "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ]

let score_cases =
  [
    case ~kind:"f64" "self assignment retains both score accesses"
      "F64 F(F64 reg x,F64 reg y){x=x;return y;}F(1.5,2.25);"
      0x4002000000000000L
      [ (0, "RSI"); (1, "RDI") ];
    case ~kind:"f64" ~saved:[ "RSI"; "RDI" ]
      "unused literal initializer counts as a write"
      "F64 F(F64 reg a){F64 reg b=1.5;return a;}F(2.25);" 0x4002000000000000L
      [ (0, "RDI") ];
    case ~kind:"f64" "unused forced parameter retains seed"
      "F64 F(F64 reg unused,F64 reg a){return a;}F(1.5,2.25);"
      0x4002000000000000L
      [ (0, "RDI"); (1, "RSI") ];
    case "unused forced F64 outranks ordinary integer reads"
      "I64 F(F64 reg unused,I8 x){x+=1;return x;}F(1.5,127);" 128L
      [ (0, "RSI"); (1, "RDI") ];
    case ~kind:"f64" "ordinary F64 excluded before unstable sorting"
      "F64 F(F64 ignored,F64 reg a,F64 reg b,F64 reg c){a;b;return \
       c;}F(9.5,1.5,2.25,3.5);"
      0x400c000000000000L
      [ (3, "RSI"); (2, "RDI"); (1, "R10") ];
    case ~kind:"f64" "used ordinary F64 still excluded before sorting"
      "F64 F(F64 ignored,F64 reg a,F64 reg b,F64 reg \
       c){ignored;ignored;ignored;a;b;return c;}F(9.5,1.5,2.25,3.5);"
      0x400c000000000000L
      [ (3, "RSI"); (2, "RDI"); (1, "R10") ];
    case ~kind:"f64" "noreg F64 excluded before unstable sorting"
      "F64 F(F64 noreg ignored,F64 reg a,F64 reg b,F64 reg c){a;b;return \
       c;}F(9.5,1.5,2.25,3.5);"
      0x400c000000000000L
      [ (3, "RSI"); (2, "RDI"); (1, "R10") ];
    case "ordinary zero-score integer records stay in mixed sort"
      "I64 F(F64 ignored,I64 a,I64 b,I64 c,I64 d){return c+d;}F(1.5,0,0,20,22);"
      42L
      [ (3, "RSI"); (4, "RDI") ];
    case ~kind:"f64"
      ~stores:[ (0, 64) ]
      "sole forced F64 spills with six general reservations"
      ("F64 F(F64 reg x," ^ reservation_parameters
     ^ "){x=x;return x;}F(1.5,1.0,1.0,1.0,1.0,1.0,1.0);")
      0x3ff8000000000000L (general_entries 1);
    case ~kind:"f64" "one explicit reservation preserves three-way order"
      "F64 F(F64 reg RDI held,F64 reg a,F64 reg b,F64 reg c){a;b;return \
       c;}F(4.5,1.5,2.25,3.5);"
      0x400c000000000000L
      [ (0, "RDI"); (3, "RSI"); (2, "R10"); (1, "R11") ];
    case
      ~stores:[ (2, 8) ]
      "both banks skip explicit reservations without lowering threshold"
      (pressure_source ~count:7
         ~suffix_parameter:",F64 reg R12 held0,F64 reg RDI held1"
         ~suffix_argument:",2.25,3.5" "I8" "127")
      (-128L)
      [
        (4, "R13");
        (5, "RSI");
        (6, "R10");
        (3, "R11");
        (0, "R14");
        (1, "R15");
        (7, "R12");
        (8, "RDI");
      ];
  ]

let capacity_fixtures =
  (* Forced parameters are unused, but retain their seed and consume slots.
     Full-word F64 entries materialize those unused selected members too. *)
  List.map
    (fun (count, forced, x, y, bits) ->
      let fs = List.init count (fun i -> "f" ^ string_of_int i) in
      let source =
        "I64 F("
        ^ String.concat "," (List.map (fun n -> "F64 reg " ^ n) fs)
        ^ ",I8 x,I8 y){x++;y++;return x+y;}F("
        ^ String.concat "," (List.init count (fun _ -> "1.5"))
        ^ ",127,127);"
      in
      let register_name = function
        | 6 -> "RSI"
        | 7 -> "RDI"
        | number when number >= 10 && number <= 15 -> "R" ^ string_of_int number
        | _ -> invalid_arg "unexpected literal capacity home"
      in
      let entries =
        List.mapi (fun i r -> (i, register_name r)) forced
        @ (count + 1, register_name y)
          :: Option.fold ~none:[]
               ~some:(fun r -> [ (count, register_name r) ])
               x
      in
      let fixture =
        case
          ~stores:(if x = None then [ (count, 8) ] else [])
          (Printf.sprintf "%d unused forced F64 seeds before live I8 pair" count)
          source bits entries
      in
      (fixture, List.map2 (fun name reg -> (name, reg)) fs forced, x, y))
    [
      (4, [ 10; 11; 6; 7 ], Some 15, 14, 256L);
      (5, [ 7; 10; 6; 12; 13 ], Some 14, 11, 256L);
      (6, [ 7; 10; 11; 12; 13; 6 ], Some 15, 14, 256L);
      (7, [ 10; 11; 14; 7; 12; 13; 6 ], None, 15, 0L);
    ]

let cases =
  pressure_cases @ score_cases
  @ List.map (fun (case, _, _, _) -> case) capacity_fixtures

module Exact = Holyc_lib__Ir.Exact_register_scores
module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence
module Symbol = Semantic_symbol

let lower_result mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"mixed-score-proof.hc" ~contents
  in
  compile_integer_program session ~config:(config mode) ~source

let function_from_program program =
  integer_program_functions program
  |> List.find (fun (fn : Ir_integer_interpreter.function_definition) ->
      Symbol.name (Body.symbol fn.body) = "F")

let lowered mode contents =
  match lower_result mode contents with
  | Error diagnostics -> failwith (contents ^ ": " ^ diagnostic_text diagnostics)
  | Ok program -> function_from_program program.value

let location fn name =
  Frame.function_locations fn.Ir_integer_interpreter.frame
  |> List.find (fun m -> Symbol.name (Frame.location_symbol m) = name)

let occurrences fn =
  match Exact.analyze ~frame:fn.Ir_integer_interpreter.frame ~body:fn.body with
  | None -> failwith "missing exact source occurrence proof"
  | Some result ->
      List.map (fun (m, n) -> (Symbol.name (Frame.location_symbol m), n)) result

let assert_occurrences label fn expected =
  let actual = occurrences fn in
  let show xs =
    String.concat "," (List.map (fun (n, k) -> n ^ ":" ^ string_of_int k) xs)
  in
  require (actual = expected)
    (label ^ ": source access counts/order " ^ show actual ^ "; expected "
   ^ show expected)

let assert_member_map label fn entries spilled =
  let proof =
    match
      Analysis.analyze ~frame:fn.Ir_integer_interpreter.frame ~body:fn.body
    with
    | Some proof -> proof
    | None -> failwith (label ^ ": no allocation witness")
  in
  List.iter
    (fun (name, register) ->
      let symbol = Frame.location_symbol (location fn name) in
      require
        (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
         = Some Analysis.Register_candidate
        && Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
             ~symbol
           = Some register)
        (label ^ ": wrong exact member home for " ^ name))
    entries;
  List.iter
    (fun name ->
      let symbol = Frame.location_symbol (location fn name) in
      require
        (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
         = Some Analysis.Stack
        && Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
             ~symbol
           = None)
        (label ^ ": missing exact stack decision for " ^ name))
    spilled

let exact_source_cases mode =
  List.iter
    (fun (fixture, forced, x, y) ->
      let fn = lowered mode fixture.source in
      assert_occurrences fixture.label fn
        (List.map (fun (n, _) -> (n, 0)) forced @ [ ("x", 2); ("y", 2) ]);
      let selected =
        forced
        @ [ ("y", y) ]
        @ Option.fold ~none:[] ~some:(fun r -> [ ("x", r) ]) x
      in
      assert_member_map fixture.label fn selected
        (if x = None then [ "x" ] else []))
    capacity_fixtures;
  List.iter
    (fun locals ->
      List.iter
        (fun discard ->
          let source = pressure_source ~locals ~discard "I8" "127" in
          let fn = lowered mode source in
          assert_occurrences "nine-member source order" fn
            (List.map (fun n -> (n, if n = "x" && discard then 3 else 2)) names);
          let entries, spilled =
            if discard then
              ( [
                  ("x", 12);
                  ("f", 13);
                  ("e", 6);
                  ("g", 7);
                  ("h", 10);
                  ("b", 11);
                  ("a", 14);
                  ("d", 15);
                ],
                [ "c" ] )
            else
              ( [
                  ("f", 12);
                  ("e", 13);
                  ("h", 6);
                  ("g", 7);
                  ("d", 10);
                  ("b", 11);
                  ("a", 14);
                  ("c", 15);
                ],
                [ "x" ] )
          in
          assert_member_map "nine-member literal source map" fn entries spilled)
        [ false; true ])
    [ false; true ];
  List.iter
    (fun (label, source, expected, entries, spilled) ->
      let fn = lowered mode source in
      assert_occurrences label fn expected;
      assert_member_map label fn entries spilled)
    [
      ( "self assignment counts before dead-move elimination",
        "F64 F(F64 reg x,F64 reg y){x=x;return y;}42;",
        [ ("x", 2); ("y", 1) ],
        [ ("x", 6); ("y", 7) ],
        [] );
      ( "unused initialization counts",
        "F64 F(F64 reg a){F64 reg b=1.5;return a;}42;",
        [ ("a", 1); ("b", 1) ],
        [ ("a", 7); ("b", 6) ],
        [] );
      ( "unused forced parameter has zero accesses",
        "F64 F(F64 reg unused,F64 reg a){return a;}42;",
        [ ("unused", 0); ("a", 1) ],
        [ ("a", 6) ],
        [] );
      ( "ordinary F64 is a counted source member, not a candidate",
        "F64 F(F64 ignored,F64 reg a,F64 reg b,F64 reg \
         c){ignored;ignored;ignored;a;b;return c;}42;",
        [ ("ignored", 3); ("a", 1); ("b", 1); ("c", 1) ],
        [ ("c", 6); ("b", 7); ("a", 10) ],
        [ "ignored" ] );
    ];
  (* These checks concern score access survival, not new native arithmetic
     qualification. Reassociation can remain outside the float executor. *)
  List.iter
    (fun expression ->
      let source =
        "F64 F(F64 reg a,F64 reg b){" ^ expression ^ ";return b;}42;"
      in
      let fn = lowered mode source in
      assert_occurrences expression fn [ ("a", 1); ("b", 1) ];
      assert_member_map expression fn [ ("b", 6); ("a", 7) ] [])
    [
      "(a)";
      "+a";
      "-a";
      "a+0.0";
      "0.0+a";
      "a-0.0";
      "a*1.0";
      "1.0*a";
      "a/1.0";
      "a*0.0";
      "0.0*a";
      "a+(-0.0)";
      "(a*2.0)*4.0";
      "(a+1.0)+2.0";
      "(a-1.0)-2.0";
    ];
  List.iter
    (fun source ->
      let fn = lowered mode source in
      require
        (Exact.analyze ~frame:fn.frame ~body:fn.body = None)
        ("unsupported exact source unexpectedly acquired scores: " ^ source))
    [
      "F64 F(F64 reg a,I64 noreg n){if(n)return a;return a;}42;";
      "F64 Child(F64 x){return x;}F64 F(F64 reg a){return Child(a);}42;";
      "F64 F(F64 reg a,F64 reg b){return a=b;}42;";
    ];
  (* Lowering may already reject these forms. If it exposes a body, that body
     still must not receive exact F64 scoring authority. *)
  List.iter
    (fun source ->
      match lower_result mode source with
      | Error _ -> ()
      | Ok program ->
          let fn = function_from_program program.value in
          require
            (Exact.analyze ~frame:fn.frame ~body:fn.body = None)
            ("outside exact mixed grammar: " ^ source))
    [
      "F64 F(F64 reg a){a+=1.0;return a;}42;";
      "F64 F(F64 reg a){a++;return a;}42;";
      "I64 F(F64 reg a){return a;}42;";
      "F64 F(I64 reg a){return a;}42;";
      "F64 F(F64 reg a){return a(F64);}42;";
      "I64 F(F64 reg a,F64 reg b){return a<b;}42;";
      "I64 F(F64 reg a,F64 reg b){return a&&b;}42;";
    ]

let authority mode =
  let source = pressure_source ~locals:true "I8" "127" in
  let original = lowered mode source and foreign = lowered mode source in
  let proof =
    Option.get (Analysis.analyze ~frame:original.frame ~body:original.body)
  in
  let symbol = Frame.location_symbol (location original "a") in
  require
    (Analysis.selection proof ~frame:foreign.frame ~body:original.body ~symbol
     = None
    && Analysis.selection proof ~frame:original.frame ~body:foreign.body ~symbol
       = None
    && Analysis.physical_register proof ~frame:foreign.frame ~body:foreign.body
         ~symbol
       = None)
    "foreign frame/body accepted a mixed score witness";
  let items fn =
    Body.body fn.Ir_integer_interpreter.body
    |> Graph.entry |> Graph.instructions |> Seq.instructions
    |> List.map Seq.description
  in
  let mutations =
    [
      ( "member order",
        fun fn ->
          Obj.set_field
            (Obj.repr fn.Ir_integer_interpreter.frame)
            4
            (Obj.repr (List.rev (Frame.function_locations fn.frame))) );
      ( "raw F64 initializer bits",
        fun fn ->
          let literal =
            List.find
              (fun (d : Seq.description) -> d.opcode = Ir_opcode.Ic_imm_f64)
              (items fn)
          in
          Obj.set_field (Obj.repr literal) 5
            (Obj.repr (Some (Seq.Float_bits Int64.min_int))) );
      ( "shared direct read address",
        fun fn ->
          let reads =
            List.filter
              (fun (d : Seq.description) -> d.opcode = Ir_opcode.Ic_deref)
              (items fn)
          in
          Obj.set_field
            (Obj.repr (List.nth reads 1))
            2
            (Obj.repr (List.hd reads).operands) );
      ( "missing direct read span",
        fun fn ->
          let read =
            List.find
              (fun (d : Seq.description) -> d.opcode = Ir_opcode.Ic_deref)
              (items fn)
          in
          Obj.set_field (Obj.repr read) 7 (Obj.repr None) );
      ( "changed original statement order",
        fun fn ->
          let typed =
            Option.get (Body.definition_source fn.Ir_integer_interpreter.body)
          in
          let source =
            Option.get
              (Semantic_function_call_expression_result.function_source_body
                 typed)
          in
          match source.body with
          | Some (Ast.Block_statement block) ->
              Obj.set_field (Obj.repr block) 1
                (Obj.repr (List.rev block.block_statements))
          | _ -> failwith "missing source block" );
    ]
  in
  List.iter
    (fun (label, mutate) ->
      let fn = lowered mode source in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = Frame.location_symbol (location fn "a") in
      require
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol
        = Some 14)
        (label ^ ": original mixed source lacks expected R14 home");
      mutate fn;
      require
        (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol = None
        && Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
             ~symbol
           = None
        && Exact.analyze ~frame:fn.frame ~body:fn.body = None
        && Analysis.analyze ~frame:fn.frame ~body:fn.body = None)
        (label ^ ": mutation retained or regenerated mixed allocation authority"))
    mutations

let completed_word expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      require
        (Option.map (fun (word : Image.word) -> word.bits) result.final_value
        = Some expected)
        "wrong retained mixed-register result";
      result.executed_steps
  | Image.Fault _ -> failwith "unexpected retained mixed-register fault"

let retained mode =
  let source = pressure_source "I8" "127" in
  let compiled = image mode source in
  let ir = Image.ir_instructions compiled
  and code = Image.code_bytes compiled in
  ignore
    (compile ~max_ir_instructions:ir ~max_code_bytes:code mode source
    |> Result.map_error diagnostic_text
    |> checked);
  require
    (Result.is_error (compile ~max_ir_instructions:(ir - 1) mode source)
    && Result.is_error (compile ~max_code_bytes:(code - 1) mode source))
    "one-below mixed placement compile quota accepted";
  let retained = Runtime.retain compiled |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () ->
      let run ?max_frame_bytes steps =
        Runtime.execute_retained_report ?max_frame_bytes ~max_steps:steps
          retained
      in
      let steps = run 100000 |> completed_word (-128L) in
      for _ = 1 to 3 do
        ignore (run steps |> completed_word (-128L))
      done;
      let failed = run (steps - 1) |> fault Image.Step_limit_exceeded in
      require
        (failed.executed_steps = steps - 1 && Option.is_some failed.span)
        "mixed quota lost original span or step charge";
      ignore (run ~max_frame_bytes:72 steps |> completed_word (-128L));
      ignore (run ~max_frame_bytes:71 steps |> fault Image.Frame_limit_exceeded);
      ignore (run steps |> completed_word (-128L)));
  let source =
    "I64 G=0;"
    ^ pressure_source ~suffix_parameter:",I64 noreg divisor"
        ~suffix_argument:",G++" ~returned:"x/divisor" "I8" "127"
  in
  let retained = image mode source |> Runtime.retain |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () ->
      let run () = Runtime.execute_retained_report ~max_steps:100000 retained in
      ignore (run () |> fault Image.Division_by_zero);
      ignore (run () |> completed_word (-128L));
      ignore (run () |> completed_word (-64L)))

let source_isolation mode =
  let first_case = pressure_case "I8" "127" 128L (-128L) 8
  and second_case = pressure_case ~discard:true "I8" "127" 128L (-128L) 8 in
  let first = image mode first_case.source in
  let saved_code = Image.code first in
  let second = image mode second_case.source in
  placement first_case first;
  placement second_case second;
  require
    (Image.code first = saved_code)
    "later discarded-read source changed earlier allocation image";
  if native then (
    ignore
      (Runtime.execute_report ~max_steps:100000 first |> completed_word (-128L));
    ignore
      (Runtime.execute_report ~max_steps:100000 second |> completed_word 128L))

let failures = ref []

let check label action =
  try action ()
  with error ->
    failures := (label ^ ": " ^ Printexc.to_string error) :: !failures

let () =
  List.iter
    (fun mode ->
      check (mode ^ " source access survival") (fun () ->
          exact_source_cases mode);
      check (mode ^ " authority") (fun () -> authority mode);
      check (mode ^ " source isolation") (fun () -> source_isolation mode);
      List.iter
        (fun case ->
          check
            (mode ^ " " ^ case.label)
            (fun () ->
              let compiled = image mode case.source in
              placement case compiled;
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
      if native then
        check (mode ^ " retained quotas and recovery") (fun () -> retained mode))
    modes;
  Printf.printf
    "mixed register scores: %d cases, %d CLI reports (%s), %d failures\n"
    (List.length cases) !count
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
