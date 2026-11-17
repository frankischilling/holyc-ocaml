open Holyc_lib
open Yojson.Safe.Util
module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Symbol = Semantic_symbol
module Image = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* Source deductions, not new TempleOS captures. Pinned reference:
   c26482bb6ad3f80106d28504ec5db3c6a360732c. OptPass3.HC:21-49
   walks every member, including unused and duplicate names, without a count
   cutoff; :119-135,187-199,238-245 scores each direct access twice. :536-612
   sorts, trims zero scores, then allocates two nonpointer and six general
   registers. CInit.HC:244-257 fixes the bank order. OptPass4.HC:196-219
   substitutes selected members; OptPass789A.HC:698-719 converts incoming
   parameters before preserving their full register value. LexLib.HC:67-84,
   103-147 resolves the first duplicate name; OptPass3.HC:29-36 gives an
   explicit register to its last source owner. Expected maps and values below
   are derived from those rules, never from the production analyzer. *)
let require condition message = if not condition then failwith message

let checked = function
  | Ok value -> value
  | Error error -> failwith error

let diagnostics ds =
  String.concat "; "
    (List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) ds)

let compiler = Sys.argv.(1)
let native = Array.exists (( = ) "--native") Sys.argv
let modes = [ "jit"; "aot" ]
let reports = ref 0
let registers = [ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15" ]

let number name =
  List.assoc name
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

let config mode =
  Preprocessor.Config.create
    ~compilation_mode:
      (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
    ()
  |> checked

let input contents =
  let session = Session.create () in
  (session, Session.add_source session ~path:"allocation-pressure.hc" ~contents)

let compile ?(max_stack_bytes = 131072) ?(max_code_bytes = 1048576)
    ?(max_ir_instructions = 65536) ?status_abi mode contents =
  let session, source = input contents in
  Native_program.compile ~max_stack_bytes ~max_code_bytes ~max_ir_instructions
    ?status_abi session ~config:(config mode) ~source

let image ?status_abi mode source =
  (compile ?status_abi mode source |> Result.map_error diagnostics |> checked)
    .value

let lowered mode contents =
  let session, source = input contents in
  let program =
    compile_integer_program session ~config:(config mode) ~source
    |> Result.map_error diagnostics
    |> checked
  in
  let fn =
    integer_program_functions program.value
    |> List.find (fun (fn : Ir_integer_interpreter.function_definition) ->
        Symbol.name (Body.symbol fn.body) = "F")
  in
  (integer_program_runtime_calls program.value, fn)

let temporary suffix contents f =
  let path = Filename.temp_file "holyc-allocation-pressure-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let ch = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out ch)
        (fun () -> output_string ch contents);
      f path)

let read path =
  let ch = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () -> really_input_string ch (in_channel_length ch))

let unique_options options =
  let key option = String.split_on_char '=' option |> List.hd in
  List.fold_right
    (fun option kept ->
      if List.exists (fun later -> key later = key option) kept then kept
      else option :: kept)
    options []

let invoke ?(status = 0) ?(options = []) ~target ~mode source =
  incr reports;
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
                   ]
                  @ unique_options
                      ([
                         "--stack-byte-limit=131072";
                         "--code-byte-limit=1048576";
                         "--ir-instruction-limit=65536";
                         "--active-stack-byte-limit=524288";
                       ]
                      @ options)
                  @ [ path ])
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
                (target ^ "/" ^ mode ^ ": " ^ text ^ "; stderr=" ^ errors);
              Yojson.Safe.from_string text)))

let value ?(kind = "i64") expected report =
  require
    (member "outcome" report = `String "success"
    && report |> member "final_value" |> member "type" = `String kind
    && report |> member "final_value" |> member "bits"
       = `String (Printf.sprintf "0x%016Lx" expected))
    (Yojson.Safe.to_string report)

let diagnostic code report =
  require
    (member "outcome" report = `String "error"
    && report |> member "diagnostics" |> to_list
       |> List.exists (fun d -> member "code" d = `String code))
    ("expected " ^ code ^ ": " ^ Yojson.Safe.to_string report)

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let instructions image =
  let sections =
    Image.native_sections image |> Holyc_lib__Asm.Native_sections.to_yojson
  in
  let symbol =
    sections |> member "symbols" |> to_list
    |> List.find (fun s -> member "name" s = `String "F")
  in
  let address = member "definition" symbol in
  let offset = address |> member "offset" |> to_int in
  let section =
    sections |> member "sections" |> to_list
    |> List.find (fun s -> member "id" s = member "section" address)
  in
  let size =
    sections |> member "mappings" |> to_list
    |> List.filter (fun m -> member "address" m = address)
    |> List.fold_left (fun n m -> max n (member "size" m |> to_int)) 0
  in
  let code = section |> member "bytes" |> to_string |> bytes in
  match
    Disassembly.decode ~mode:Asm_operand.Use64 ~position:(Int64.of_int offset)
      (String.sub code offset size)
  with
  | Ok decoded -> Disassembly.instructions decoded
  | Error e -> failwith e.message

let operands (i : Disassembly.instruction) =
  List.map (fun (o : Disassembly.operand) -> o.text) i.operands

let parameter_map code =
  let rec collect = function
    | (load : Disassembly.instruction) :: store :: rest ->
        let pair =
          match (load.operands, operands store) with
          | [ dst; src ], [ home; "U64 RAX" ]
            when List.mem load.opcode [ "MOV"; "MOVSX"; "MOVZX"; "MOVSXD" ]
                 && List.mem dst.text [ "U64 RAX"; "U32 EAX" ]
                 && String.ends_with ~suffix:"[RBP]" src.text
                 && Option.fold ~none:false ~some:(fun n -> n >= 16L) src.value
                 && store.opcode = "MOV" ->
              List.find_map
                (fun r ->
                  if home = "U64 " ^ r then
                    Some ((Int64.to_int (Option.get src.value) - 16) / 8, r)
                  else None)
                registers
          | _ -> None
        in
        Option.to_list pair @ collect (store :: rest)
    | _ -> []
  in
  collect code |> List.sort_uniq compare

let frame_moves save r code =
  List.filter
    (fun (i : Disassembly.instruction) ->
      i.opcode = "MOV"
      &&
      match i.operands with
      | [ dst; src ] ->
          let memory, reg = if save then (dst, src) else (src, dst) in
          reg.text = "U64 " ^ r && String.ends_with ~suffix:"[RBP]" memory.text
      | _ -> false)
    code

let allocated_bytes code =
  List.find_map
    (fun (i : Disassembly.instruction) ->
      match i.operands with
      | [ dst; size ] when i.opcode = "SUB" && dst.text = "U64 RSP" ->
          Option.map Int64.to_int size.value
      | _ -> None)
    code
  |> Option.get

module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence

type fixture = {
  label : string;
  source : string;
  expected : int64;
  homes : (int * string) list;
  members : int;
  blocks : int option;
  ir : int option;
  floating : bool;
  large : bool;
}

let fixture ?blocks ?ir ?(floating = false) ?(large = false) label source
    expected members homes =
  { label; source; expected; members; homes; blocks; ir; floating; large }

let repeat n text = String.concat "" (List.init n (fun _ -> text))
let names n = List.init n (fun i -> "v" ^ string_of_int i)

(* Balanced grouping deliberately keeps expression depth logarithmic. These
   regressions vary graph width, not the separately documented nesting limit. *)
let rec sum = function
  | [] -> "0"
  | [ x ] -> x
  | xs ->
      let midpoint = List.length xs / 2 in
      let left = List.filteri (fun i _ -> i < midpoint) xs
      and right = List.filteri (fun i _ -> i >= midpoint) xs in
      "(" ^ sum left ^ "+" ^ sum right ^ ")"

let unused n =
  names n |> List.map (fun name -> "I64 " ^ name ^ ";") |> String.concat ""

let ranked count =
  let members = names count in
  let weighted =
    members
    |> List.mapi (fun i name ->
        List.init (if i < 8 then 9 - i else 1) (fun _ -> name))
    |> List.concat |> sum
  in
  (* All initializers and updates contribute the same score. The first eight
     have 9..2 final reads, every remaining member has one. Therefore the
     winners and order do not depend on an unstable equal-score permutation.
     Selected I8 homes retain 128; every spill stores -128. Sum(9..2) = 44. *)
  fixture
    (Printf.sprintf "%d ranked narrow members" count)
    ("I64 F(){"
    ^ String.concat "" (List.map (fun name -> "I8 " ^ name ^ "=127;") members)
    ^ String.concat "" (List.map (fun name -> name ^ "+=1;") members)
    ^ "return " ^ weighted ^ ";}F();")
    (Int64.of_int (128 * (52 - count)))
    count
    (List.mapi
       (fun i register -> (i, register))
       [ "R12"; "R13"; "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ])

(* Literal permutations independently traced from Kernel/QSort.HC:55-83,
   using equal scores except the zero-score member at source position16.
   The zero remains in the sort input; removing it first changes the winners. *)
let equal_with_zero count =
  let winners =
    match count with
    | 31 -> [ 20; 21; 22; 19; 15; 17; 18; 23 ]
    | 32 -> [ 20; 21; 22; 19; 31; 17; 18; 23 ]
    | 33 -> [ 21; 20; 23; 22; 17; 32; 19; 18 ]
    | _ -> invalid_arg "missing independent zero-member permutation"
  in
  let active = names count |> List.filteri (fun i _ -> i <> 16) in
  let declarations =
    names count
    |> List.mapi (fun i name -> "I8 " ^ name ^ if i = 16 then ";" else "=127;")
    |> String.concat ""
  in
  fixture
    (Printf.sprintf "%d equal-score members retain middle zero before sorting"
       count)
    ("I64 F(){" ^ declarations
    ^ String.concat "" (List.map (fun name -> name ^ "+=1;") active)
    ^ "return " ^ sum active ^ ";}F();")
    (Int64.of_int (128 * (17 - count)))
    count
    (List.combine winners
       [ "R12"; "R13"; "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ])

let unused_forced =
  fixture "128 members retain unused forced owner"
    ("I64 F(I8 x){" ^ unused 126 ^ "I64 reg keep;x+=1;return x;}F(127);")
    128L 128
    [ (0, "RDI"); (127, "RSI") ]

let duplicates =
  fixture "128 duplicate members retain first lookup and last register owner"
    ("I64 F(I8 reg R12,"
    ^ String.concat "," (List.init 126 (fun _ -> "I64"))
    ^ ",I8 reg R12 winner){_anon_=128;return _anon_+winner;}F(0,"
    ^ String.concat "," (List.init 126 (fun _ -> "0"))
    ^ ",7);")
    (-121L) 128
    [ (127, "R12") ]

let mixed =
  fixture ~floating:true ~large:true
    "128 mixed members preserve F64 and pointer through a call"
    ("I64 V=2;F64 Child(F64 reg y){return y+1.0;}"
   ^ "F64 F(F64 reg x,I64 *p,I8 y){I64 noreg scratch[5000];" ^ unused 124
   ^ "scratch[0]=20;scratch[4999]=22;y+=1;"
   ^ "if(*p!=2)return 0.0;if(*p!=2)return 0.0;if(*p!=2)return 0.0;"
   ^ "if(y!=128)return 0.0;if(scratch[0]+scratch[4999]!=42)return 0.0;"
   ^ "return Child(x)+x;}F(1.5,&V,127);")
    (Int64.bits_of_float 4.0) 128
    [ (0, "RSI"); (1, "RDI"); (2, "R10") ]

let recursive =
  fixture ~floating:true ~large:true
    "128 mixed members preserve independent recursive large frames"
    ("I64 V=2;F64 F(F64 reg x,I64 *p,I64 noreg n){I64 noreg scratch[5000];"
   ^ unused 123
   ^ "scratch[0]=20;scratch[4999]=22;if(n){F64 noreg result=F(x,p,n-1);"
   ^ "if(*p==2)return result+x;return 0.0;}"
   ^ "if(scratch[0]+scratch[4999]==42)return x;return 0.0;}F(1.5,&V,1);")
    (Int64.bits_of_float 3.0) 128
    [ (0, "RSI"); (1, "RDI") ]

let block_boundary blocks =
  let labels = List.init (blocks - 3) (fun i -> "L" ^ string_of_int i) in
  let body =
    labels
    |> List.mapi (fun i label ->
        label ^ ":"
        ^
        if i = List.length labels - 1 then "return x;"
        else "goto " ^ List.nth labels (i + 1) ^ ";")
    |> String.concat ""
  in
  fixture ~blocks
    (Printf.sprintf "%d authenticated blocks" blocks)
    ("I64 F(I8 x){x+=1;goto L0;" ^ body ^ "}F(127);")
    128L 1
    [ (0, "RSI") ]

let instruction_boundary ir =
  (* The even nonpadding function contributes 14 lowered instructions; each
     discarded immediate contributes 2. Unary negation adds the odd instruction.
     These exact assertions must fail if canonical lowering changes. *)
  fixture ~ir
    (Printf.sprintf "%d authenticated instructions" ir)
    ("I64 F(I8 x){"
    ^ repeat ((ir - 14) / 2) "0;"
    ^ "x+=1;return "
    ^ (if ir mod 2 = 0 then "x" else "-x")
    ^ ";}F(127);")
    (if ir mod 2 = 0 then 128L else -128L)
    1
    [ (0, "RSI") ]

let shallow =
  fixture "1500 shallow updates exceed the former statement allowance"
    ("I64 F(I8 x){" ^ repeat 1500 "x+=1;" ^ "return x;}F(127);")
    1627L 1
    [ (0, "RSI") ]

let all_cases =
  List.map ranked [ 31; 32; 33; 128 ]
  @ List.map equal_with_zero [ 31; 32; 33 ]
  @ [ unused_forced; duplicates; mixed; recursive; shallow ]
  @ List.map block_boundary [ 64; 65 ]
  @ List.map instruction_boundary [ 512; 513 ]

let cases =
  match Sys.getenv_opt "HOLYC_PRESSURE_CASE" with
  | None -> all_cases
  | Some label ->
      let selected =
        List.filter (fun fixture -> fixture.label = label) all_cases
      in
      require (selected <> []) ("unknown pressure fixture: " ^ label);
      selected

let semantic mode fixture =
  let runtime, fn = lowered mode fixture.source in
  let locations = Frame.function_locations fn.frame in
  let blocks = Graph.blocks (Body.body fn.body) in
  let instructions =
    List.fold_left
      (fun count block ->
        count + List.length (Seq.instructions (Graph.instructions block)))
      0 blocks
  in
  require
    (List.length locations = fixture.members)
    (Printf.sprintf "fixture has %d members, expected %d"
       (List.length locations) fixture.members);
  Option.iter
    (fun expected ->
      require
        (List.length blocks = expected)
        (Printf.sprintf "fixture has %d blocks, expected %d"
           (List.length blocks) expected))
    fixture.blocks;
  Option.iter
    (fun expected ->
      require (instructions = expected)
        (Printf.sprintf "fixture has %d instructions, expected %d" instructions
           expected))
    fixture.ir;
  let proof =
    Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body
    |> function
    | Some proof -> proof
    | None -> failwith "missing complete scaling proof"
  in
  List.iteri
    (fun index location ->
      let symbol = Frame.location_symbol location in
      let expected = List.assoc_opt index fixture.homes |> Option.map number in
      (* A query reauthenticates the complete source receipt. Check all homes
         and representative spills for128-member stress cases rather than
         doing128 whole-source authentications. Boundary cases check everyone;
         decoded entry maps and execution still cover the complete function. *)
      if
        fixture.members <= 33 || Option.is_some expected
        || List.mem index [ 0; 1; 2; 7; 8; 15; 16; 31; 32; 63; 64; 126; 127 ]
      then
        require
          (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
             ~symbol
          = expected)
          (Printf.sprintf "wrong source home for member %d (%s)" index
             (Symbol.name symbol)))
    locations;
  fn

let physical fixture fn image =
  let code = instructions image in
  let parameter_homes =
    Frame.function_locations fn.Ir_integer_interpreter.frame
    |> List.mapi (fun i location -> (i, location))
    |> List.filter_map (fun (i, location) ->
        if Frame.location_kind location = Frame.Named_parameter then
          Option.map
            (fun register -> (i, register))
            (List.assoc_opt i fixture.homes)
        else None)
  in
  require
    (parameter_map code = List.sort compare parameter_homes)
    "decoded parameter entry homes differ from independent source map";
  let saved =
    List.filter (fun register -> frame_moves true register code <> []) registers
  in
  require
    (List.sort compare saved
    = (List.map snd fixture.homes |> List.sort_uniq compare))
    "wrong physical saved owner set";
  List.iter
    (fun register ->
      let saves = frame_moves true register code
      and restores = frame_moves false register code in
      require
        (List.length saves = 1 && List.length restores = 1)
        ("nonunique save/restore for " ^ register);
      require
        ((List.hd (List.hd saves).operands).value
       = (List.nth (List.hd restores).operands 1).value)
        ("different save/restore slots for " ^ register))
    saved;
  if fixture.large then
    require (allocated_bytes code > 65528) "large fixture lost compact frame"

let completed expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      require
        (Option.map (fun (word : Image.word) -> word.bits) result.final_value
        = Some expected)
        "wrong retained pressure result";
      result.executed_steps
  | Image.Fault _ -> failwith "unexpected retained pressure fault"

let fault kind report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      require
        (result.kind = kind && Option.is_some result.span)
        "wrong pressure fault or missing source span";
      result
  | Image.Completed _ -> failwith "expected retained pressure fault"

let with_retained image action =
  let retained = Runtime.retain image |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () -> action retained)

let quota_fixture =
  fixture ~large:true "wide large-frame resource owner"
    ("I64 F(I8 x){I64 noreg scratch[5000];" ^ unused 126
   ^ "scratch[0]=20;scratch[4999]=22;x+=1;return \
      x+scratch[0]+scratch[4999];}F(127);")
    170L 128
    [ (0, "RSI") ]

let quotas mode =
  let fixture = quota_fixture in
  let fn = semantic mode fixture in
  let image = image mode fixture.source in
  physical fixture fn image;
  let private_bytes = allocated_bytes (instructions image) in
  require
    (private_bytes = Image.frame_bytes image)
    "F does not own largest private frame";
  let active = Image.entry_stack_bytes image + 16 + private_bytes in
  (* One named fixed parameter occupies eight bytes, even though its declared
     I8 value uses only one; semantic local size comes from checked layout. *)
  let semantic_bytes = Int64.to_int (Frame.function_frame_size fn.frame) + 8 in
  let code = Image.code_bytes image and ir = Image.ir_instructions image in
  ignore
    (compile ~max_stack_bytes:private_bytes ~max_code_bytes:code
       ~max_ir_instructions:ir mode fixture.source
    |> Result.map_error diagnostics
    |> checked);
  List.iter
    (fun (label, result) ->
      require (Result.is_error result) (label ^ " one-below quota admitted"))
    [
      ( "private stack",
        compile ~max_stack_bytes:(private_bytes - 1) mode fixture.source );
      ("code", compile ~max_code_bytes:(code - 1) mode fixture.source);
      ("IR", compile ~max_ir_instructions:(ir - 1) mode fixture.source);
    ];
  with_retained image (fun retained ->
      let run ?max_frame_bytes ?max_active_stack_bytes steps =
        Runtime.execute_retained_report ?max_frame_bytes ?max_active_stack_bytes
          ~max_steps:steps retained
      in
      let steps =
        run ~max_active_stack_bytes:active 100000 |> completed fixture.expected
      in
      ignore
        (run ~max_frame_bytes:semantic_bytes ~max_active_stack_bytes:active
           steps
        |> completed fixture.expected);
      ignore
        (run ~max_frame_bytes:(semantic_bytes - 1)
           ~max_active_stack_bytes:active steps
        |> fault Image.Frame_limit_exceeded);
      ignore
        (run ~max_active_stack_bytes:(active - 1) steps
        |> fault Image.Native_stack_limit_exceeded);
      let short =
        run ~max_active_stack_bytes:active (steps - 1)
        |> fault Image.Step_limit_exceeded
      in
      require (short.executed_steps = steps - 1) "step quota lost exact charge";
      for _ = 1 to 3 do
        ignore
          (run ~max_frame_bytes:semantic_bytes ~max_active_stack_bytes:active
             steps
          |> completed fixture.expected)
      done);
  List.iter
    (fun target ->
      let options =
        [
          "--frame-byte-limit=" ^ string_of_int semantic_bytes;
          "--stack-byte-limit=" ^ string_of_int private_bytes;
          "--active-stack-byte-limit=" ^ string_of_int active;
        ]
      in
      invoke ~options ~target ~mode fixture.source |> value fixture.expected;
      List.iter
        (fun (option, code) ->
          invoke ~status:1 ~options:(options @ [ option ]) ~target ~mode
            fixture.source
          |> diagnostic code)
        ([
           ( "--frame-byte-limit=" ^ string_of_int (semantic_bytes - 1),
             "HCIRVM0011" );
         ]
        @
        if target = "ir" then []
        else
          [
            ( "--stack-byte-limit=" ^ string_of_int (private_bytes - 1),
              "HCBACK0004" );
            ( "--active-stack-byte-limit=" ^ string_of_int (active - 1),
              "HCNATIVE0006" );
          ]))
    [ "ir"; "host-jit"; "host-jit-task" ]

let recoveries mode =
  let source =
    "I64 Attempts=0;I64 F(I8 x,I64 noreg divisor){I64 noreg scratch[5000];"
    ^ unused 125
    ^ "scratch[0]=42;x+=1;return scratch[0]/divisor+x;}F(127,Attempts++);"
  in
  with_retained (image mode source) (fun retained ->
      let run () =
        Runtime.execute_retained_report ~max_steps:100000
          ~max_active_stack_bytes:524288 retained
      in
      let failed = run () |> fault Image.Division_by_zero in
      require
        (failed.function_name = Some "F")
        "pressure fault lost owning function";
      ignore (run () |> completed 170L))

(* The parser builds a left-associated chain in its operator loop without
   consuming grouping recursion. Parentheses would hit its own 256-level
   guard first and would not exercise allocation-proof exhaustion. *)
let depth_source = "I64 F(I8 x){x+=1;return x" ^ repeat 300 "+0" ^ ";}F(127);"

let depth_exhaustion mode =
  (* This ordinary, non-explicit I8 must reject when proof is exhausted.
     Returning -128 by quietly falling back to stack storage would be wrong. *)
  List.iter
    (fun target ->
      invoke ~status:1 ~target ~mode depth_source
      |> diagnostic (if target = "ir" then "HCIRVM0007" else "HCBACK0001"))
    (if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ])

let failures = ref []
let checks = ref 0

let check label action =
  incr checks;
  let timed = Sys.getenv_opt "HOLYC_PRESSURE_TIMING" = Some "1" in
  let start = Unix.gettimeofday () in
  if timed then Printf.eprintf "START %s\n%!" label;
  (try action ()
   with error ->
     let message = label ^ ": " ^ Printexc.to_string error in
     failures := message :: !failures;
     if timed then Printf.eprintf "FAIL %s\n%!" message);
  if timed then
    Printf.eprintf "END %s %.3fs\n%!" label (Unix.gettimeofday () -. start)

let smoke_manifest () =
  let targets fixture =
    if fixture.floating then [ "host-jit"; "host-jit-task" ]
    else [ "ir"; "host-jit"; "host-jit-task" ]
  in
  let strings values = `List (List.map (fun value -> `String value) values) in
  let positive fixture =
    `Assoc
      [
        ("label", `String fixture.label);
        ("source", `String fixture.source);
        ("targets", strings (targets fixture));
        ("expected_bits", `String (Printf.sprintf "0x%016Lx" fixture.expected));
        ("expected_type", `String (if fixture.floating then "f64" else "i64"));
      ]
  in
  let depth =
    `Assoc
      [
        ("label", `String "ordinary I8 proof-depth exhaustion");
        ("source", `String depth_source);
        ("targets", strings [ "ir"; "host-jit"; "host-jit-task" ]);
        ( "diagnostics",
          `Assoc
            [
              ("ir", `String "HCIRVM0007");
              ("host-jit", `String "HCBACK0001");
              ("host-jit-task", `String "HCBACK0001");
            ] );
      ]
  in
  `Assoc
    [
      ("schema", `String "holyc-allocation-scaling-installed-smokes-v1");
      ("reference_commit", `String "c26482bb6ad3f80106d28504ec5db3c6a360732c");
      ("expected_implementation_commit", `Null);
      ("modes", strings modes);
      ( "options",
        strings
          [
            "--stack-byte-limit=131072";
            "--code-byte-limit=1048576";
            "--ir-instruction-limit=65536";
            "--active-stack-byte-limit=524288";
          ] );
      ( "cases",
        `List
          (List.map positive
             [
               ranked 33;
               unused_forced;
               duplicates;
               mixed;
               block_boundary 65;
               instruction_boundary 513;
             ]
          @ [ depth ]) );
    ]

let () =
  if Array.exists (( = ) "--manifest") Sys.argv then (
    print_endline (Yojson.Safe.pretty_to_string (smoke_manifest ()));
    exit 0);
  List.iter
    (fun mode ->
      List.iter
        (fun fixture ->
          check
            (mode ^ " " ^ fixture.label)
            (fun () ->
              let fn = semantic mode fixture in
              let compiled = image mode fixture.source in
              physical fixture fn compiled;
              (* The 1500-update stress program has already paid native emission.
                 Execute that same image instead of compiling it twice more
                 through the CLI. Other fixtures cover both public routes. *)
              if native && fixture == shallow then
                with_retained compiled (fun retained ->
                    ignore
                      (Runtime.execute_retained_report ~max_steps:100000
                         ~max_active_stack_bytes:524288 retained
                      |> completed fixture.expected));
              List.iter
                (fun target ->
                  invoke ~target ~mode fixture.source
                  |> value
                       ~kind:(if fixture.floating then "f64" else "i64")
                       fixture.expected)
                ((if fixture.floating then [] else [ "ir" ])
                @
                if native && fixture != shallow then
                  [ "host-jit"; "host-jit-task" ]
                else [])))
        cases;
      check (mode ^ " exhausted ordinary narrow proof rejects") (fun () ->
          depth_exhaustion mode);
      if native then (
        check (mode ^ " exact independent quotas") (fun () -> quotas mode);
        check (mode ^ " same-image fault recovery") (fun () -> recoveries mode)))
    modes;
  Printf.printf
    "Allocation pressure: %d cases, %d checks, %d CLI reports (%s), %d failures\n"
    (List.length cases) !checks !reports
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
