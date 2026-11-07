open Holyc_lib
open Yojson.Safe.Util
module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Symbol = Semantic_symbol
module Image = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* Independent source deductions, not fresh TempleOS captures. Pinned revision:
   c26482bb6ad3f80106d28504ec5db3c6a360732c. OptPass3:36-42 keeps dimensioned
   candidates and reg seeds; :193-197 scores the FIRST containing range, while
   :129-134/:156-162 cancel only the FIRST same-start member. QSort2b:60-87 is
   copied-pivot and unstable. OptPass3:538-553 sorts BEFORE trimming/filtering;
   :557-582 counts arrays in j but skips dimensions for R12/R13; :590-595 also
   skips local arrays for the general bank. PrsExp:772-803/:1055-1099 supplies
   array addresses/indexing. These maps are literal independent expectations.
   Neither a hosted array load count nor C array decay is an allocation oracle. *)
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
  ( session,
    Session.add_source session ~path:"array-member-registers.hc" ~contents )

let compile ?(max_stack_bytes = 131072) ?(max_code_bytes = 1048576)
    ?max_ir_instructions ?status_abi mode contents =
  let session, source = input contents in
  Native_program.compile ~max_stack_bytes ~max_code_bytes ?max_ir_instructions
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
  let path = Filename.temp_file "holyc-array-member-" suffix in
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

let value expected report =
  require
    (member "outcome" report = `String "success"
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
            when load.opcode = "MOV" && dst.text = "U64 RAX"
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

(* Compact probe checks use decoded operands and branch targets, not a copied
   large byte helper. The allocation is decoded, never inferred from 5000*8. *)
let signed32 value =
  Option.map (fun n -> Int64.of_int32 (Int64.to_int32 n)) value

let compact_probe code =
  let frame = allocated_bytes code in
  require (frame > 65528) "combined fixture no longer crosses compact threshold";
  let rec loop = function
    | (start : Disassembly.instruction)
      :: touch :: step :: bound :: back :: rest
      when start.opcode = "MOV"
           && List.hd (operands start) = "U64 RAX"
           && signed32 (List.nth start.operands 1).value = Some (-4096L) ->
        require
          (touch.opcode = "TEST"
          && operands touch = [ "U64 [RSP+RAX*1]"; "U64 RAX" ]
          && step.opcode = "SUB"
          && List.hd (operands step) = "U64 RAX"
          && (List.nth step.operands 1).value = Some 4096L
          && bound.opcode = "CMP"
          && List.hd (operands bound) = "U64 RAX"
          && signed32 (List.nth bound.operands 1).value
             = Some (Int64.of_int (-(frame / 4096 * 4096)))
          && List.mem back.opcode [ "JGE"; "JNL" ]
          && back.jump_target = Some touch.address)
          "compact probe has wrong step, bound, or backward target";
        let allocation =
          if frame mod 4096 = 0 then List.hd rest
          else
            let tail = List.hd rest in
            require
              (tail.opcode = "TEST"
              && String.ends_with ~suffix:"[RSP]" (List.hd tail.operands).text
              && signed32 (List.hd tail.operands).value
                 = Some (Int64.of_int (-frame))
              && (List.nth tail.operands 1).text = "U64 RAX")
              "compact probe lost exact final partial-page touch";
            List.nth rest 1
        in
        require
          (allocation.opcode = "SUB"
          && List.hd (operands allocation) = "U64 RSP"
          && (List.nth allocation.operands 1).value = Some (Int64.of_int frame)
          )
          "compact probe must complete before its owning allocation"
    | _ :: rest -> loop rest
    | [] ->
        failwith
          ("combined frame has no decoded compact page-probe loop: "
          ^ String.concat "; "
              (List.map
                 (fun i -> Disassembly.text i)
                 (List.filteri (fun i _ -> i < 12) code)))
  in
  loop code;
  frame

type case = {
  label : string;
  source : string;
  expected : int64;
  homes : (string * string) list;
  parameters : (int * string) list option;
  large : bool;
  native_only : bool;
}

let case ?parameters ?(large = false) ?(native_only = false) label source
    expected homes =
  { label; source; expected; homes; parameters; large; native_only }

let letters n = List.init n (fun i -> String.make 1 (Char.chr (97 + i)))

let weighted n =
  letters n
  |> List.mapi (fun i name ->
      "(" ^ String.concat "+" (List.init (n - i) (fun _ -> name)) ^ ")")
  |> String.concat "+"

let params n = letters n |> List.map (( ^ ) "I64 ") |> String.concat ","
let args n = List.init n (fun _ -> "1") |> String.concat ","

let general =
  List.combine (letters 6) [ "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ]

let pressure =
  List.combine (letters 6) [ "R12"; "R13"; "RSI"; "RDI"; "R10"; "R11" ]

let entries homes = List.mapi (fun i n -> (i, List.assoc n homes)) (letters 6)

let weighted_source type_ qualifier dimensions accesses result =
  Printf.sprintf "I64 F(%s){%s %s scratch%s;%sreturn %s%s;}F(%s);" (params 6)
    type_ qualifier dimensions accesses (weighted 6) result (args 6)

let unused type_ qualifier expected =
  case ~parameters:(entries expected)
    ~large:(type_ = "I64" || type_ = "F64")
    (type_ ^ " " ^ qualifier ^ " unused complete-member array")
    (weighted_source type_ qualifier "[5000]" "" "")
    21L expected

let initial_cases =
  List.concat_map
    (fun type_ ->
      [
        unused type_ "noreg" general;
        unused type_ "" general;
        unused type_ "reg" pressure;
      ])
    [ "I64" ]
  @ List.map
      (fun type_ -> unused type_ "reg" pressure)
      [ "Bool"; "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "U64" ]
  @ [
      case ~parameters:(entries pressure)
        "unused multidimensional array retains dimensions and forced pressure"
        (weighted_source "I64" "reg" "[2][3]" "" "")
        21L pressure;
    ]

let equal_cases =
  List.map
    (fun (qualifier, homes) ->
      case ~parameters:(entries homes)
        (qualifier ^ " equal-score array sort before filtering")
        (Printf.sprintf "I64 F(%s){I64 %s scratch[2];return a+b+c+d+e+f;}F(%s);"
           (params 6) qualifier (args 6))
        6L homes)
    [
      ( "noreg",
        [
          ("d", "RSI");
          ("e", "RDI");
          ("f", "R10");
          ("a", "R11");
          ("b", "R14");
          ("c", "R15");
        ] );
      ( "",
        [
          ("d", "RSI");
          ("e", "RDI");
          ("f", "R10");
          ("a", "R11");
          ("b", "R14");
          ("c", "R15");
        ] );
      ( "reg",
        [
          ("e", "R12");
          ("f", "R13");
          ("d", "RSI");
          ("a", "RDI");
          ("b", "R10");
          ("c", "R11");
        ] );
    ]

(* A zero-score array between equal-score source members changes the unstable
   sort even though it is trimmed later. Parameters read twice and locals with
   one initializer/read each all score four. Noreg omits scratch and yields
   d,e,f,a,b,c; ordinary scratch sorts f,d,e,a,b,c,scratch before trimming. *)
let middle_zero_cases =
  List.map
    (fun (qualifier, homes) ->
      case
        ~parameters:[ (0, "R11"); (1, "R14"); (2, "R15") ]
        (qualifier ^ " middle zero array preserves sort-before-trim ordering")
        ("I64 F(I64 a,I64 b,I64 c){I64 " ^ qualifier
       ^ " scratch[2];I64 d=10,e=20,f=30;return a+a+b+b+c+c+d+e+f;}F(1,1,1);")
        66L homes)
    [
      ( "noreg",
        [
          ("d", "RSI");
          ("e", "RDI");
          ("f", "R10");
          ("a", "R11");
          ("b", "R14");
          ("c", "R15");
        ] );
      ( "",
        [
          ("f", "RSI");
          ("d", "RDI");
          ("e", "R10");
          ("a", "R11");
          ("b", "R14");
          ("c", "R15");
        ] );
    ]

let access_cases =
  List.map
    (fun (label, qualifier, index, homes) ->
      case ~parameters:(entries homes) label
        (weighted_source "I64" qualifier "[2]"
           ("scratch[" ^ index ^ "]=0;")
           ("+scratch[" ^ index ^ "]"))
        21L homes)
    [
      ("noreg same-start array does not compete", "noreg", "0", general);
      ("ordinary same-start array contributes pressure", "", "0", pressure);
      ("forced same-start array contributes pressure", "reg", "0", pressure);
      ("ordinary interior array is address-filtered", "", "1", general);
      ( "forced interior array is address-filtered despite seed",
        "reg",
        "1",
        general );
    ]
  @ [
      case
        "explicit element address filters array but preserves scalar and \
         pointer homes"
        "I64 F(I8 x){I64 scratch[2];I64 *p=&scratch[0];*p=42;x+=1;return \
         x+*p;}F(127);"
        170L
        [ ("p", "RSI"); ("x", "RDI") ];
      case "scalar array index retains its own access score"
        "I64 F(I8 x,I64 index){I64 scratch[2];scratch[index]=20;x+=1;return \
         scratch[index]+x;}F(127,1);"
        148L
        [ ("index", "RSI"); ("x", "RDI") ];
      case "unsigned word array index retains its own access score"
        "I64 F(I8 x,U64 index){I64 scratch[2];scratch[index]=20;x+=1;return \
         scratch[index]+x;}F(127,1);"
        148L
        [ ("index", "RSI"); ("x", "RDI") ];
      case "noreg active array permits neighboring widened scalar"
        "I64 F(I8 x){I64 noreg \
         scratch[2];scratch[0]=20;scratch[1]=22;x+=1;return \
         scratch[0]+scratch[1]+x;}F(127);"
        170L
        [ ("x", "RSI") ];
    ]

let narrow_update_cases =
  List.map
    (fun (index, homes) ->
      let element = "scratch[" ^ index ^ "]" in
      case
        ("U8 element updates at index " ^ index
       ^ " retain width beside six widened I8 homes")
        (Printf.sprintf
           "I64 F(%s){U8 scratch[2];%s=255;%s++;%s+=1;%sreturn %s+%s;}F(%s);"
           (letters 6 |> List.map (( ^ ) "I8 ") |> String.concat ",")
           element element element
           (letters 6
           |> List.map (fun name -> name ^ "+=1;")
           |> String.concat "")
           (weighted 6) element
           (List.init 6 (fun _ -> "127") |> String.concat ","))
        2689L homes)
    [ ("0", pressure); ("1", general) ]

let all_homes =
  List.combine (letters 8)
    [ "R12"; "R13"; "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ]

let recursive_all =
  case ~large:true
    ~parameters:
      (List.mapi (fun i n -> (i + 1, List.assoc n all_homes)) (letters 8))
    "all eight automatic banks survive recursive large array owner"
    (Printf.sprintf
       "I64 F(I64 noreg n,%s){I64 noreg \
        scratch[5000];scratch[0]=2;scratch[4999]=4;if(n)return \
        F(n-1,%s)+%s+scratch[0]+scratch[4999];return \
        %s+scratch[0]+scratch[4999];}F(1,%s);"
       (params 8)
       (String.concat "," (letters 8))
       (weighted 8) (weighted 8) (args 8))
    84L all_homes

let preservation_cases =
  [
    recursive_all;
    case ~large:true
      "scalar and descriptor survive named child next to large array"
      "I64 V=2;I64 Read(I64 *q){return *q;}I64 F(I8 x,I64 *p){I64 noreg \
       scratch[5000];scratch[0]=10;x+=1;return \
       x+Read(p)+*p+scratch[0];}F(127,&V);"
      142L
      [ ("p", "RSI"); ("x", "RDI") ];
    case ~large:true ~native_only:true
      "forced F64 and pointer survive named child next to large array"
      "I64 V=2;F64 Child(F64 reg y){return y+1.0;}F64 F(F64 reg x,I64 *p){I64 \
       noreg \
       scratch[5000];scratch[0]=20;scratch[4999]=22;if(scratch[0]+scratch[4999]+*p==44)return \
       Child(x)+x;return 0.0;}F(1.5,&V);"
      (Int64.bits_of_float 4.0)
      [ ("x", "RSI"); ("p", "RDI") ];
    case ~large:true ~native_only:true
      "forced F64 and pointer survive recursive large array owner"
      "I64 V=2;F64 F(F64 reg x,I64 *p,I64 noreg n){I64 noreg \
       scratch[5000];scratch[0]=20;scratch[4999]=22;if(n){F64 noreg \
       result=F(x,p,n-1);if(*p==2)return result+x;return \
       0.0;}if(scratch[0]+scratch[4999]==42)return x;return 0.0;}F(1.5,&V,1);"
      (Int64.bits_of_float 3.0)
      [ ("x", "RSI"); ("p", "RDI") ];
  ]

let cases =
  initial_cases @ equal_cases @ middle_zero_cases @ access_cases
  @ narrow_update_cases @ preservation_cases

let semantic mode case =
  let runtime, fn = lowered mode case.source in
  let proof =
    Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body
    |> function
    | Some p -> p
    | None -> failwith "missing complete array-member proof"
  in
  Frame.function_locations fn.frame
  |> List.iter (fun location ->
      let symbol = Frame.location_symbol location in
      let expected =
        List.assoc_opt (Symbol.name symbol) case.homes |> Option.map number
      in
      require
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol
        = expected)
        ("wrong proved physical home for " ^ Symbol.name symbol);
      if Frame.location_dimensions location <> [] then
        require
          (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
          <> Some Analysis.Register_candidate)
          "array received an automatic register home");
  fn

let physical case image =
  let code = instructions image in
  Option.iter
    (fun expected ->
      require
        (parameter_map code = List.sort compare expected)
        "decoded parameter homes differ from literal source oracle")
    case.parameters;
  let saved = List.filter (fun r -> frame_moves true r code <> []) registers in
  require
    (List.sort compare saved
    = (List.map snd case.homes |> List.sort_uniq compare))
    ("wrong saved register owners: " ^ String.concat "," saved);
  List.iter
    (fun r ->
      let saves = frame_moves true r code
      and restores = frame_moves false r code in
      require
        (List.length saves = 1 && List.length restores = 1)
        ("missing unique save/restore for " ^ r);
      let save = List.hd saves and restore = List.hd restores in
      require
        ((List.hd save.operands).value = (List.nth restore.operands 1).value)
        ("save/restore offset differs for " ^ r))
    saved;
  if case.large then ignore (compact_probe code)

(* Read unwind slots as operations, skipping their extension words. Source-owned
   R10/R11 saves are real but Windows-volatile, so they must not be advertised as
   unwind nonvolatiles. Validate F's offsets against its decoded save instructions. *)
let windows_unwind fixture image =
  let code = instructions image in
  let first = Int64.to_int (List.hd code).address in
  let _, _, info =
    Image.windows_unwind_functions image
    |> List.find (fun (start, _, _) -> start = first)
  in
  let byte i = Char.code info.[i] in
  let word i = byte i lor (byte (i + 1) lsl 8) in
  let slots = byte 2 in
  let rec walk slot saves =
    if slot = slots then saves
    else (
      require (slot < slots) "truncated Windows array-frame unwind slots";
      let cursor = 4 + (2 * slot) in
      let operation = byte (cursor + 1) land 15
      and register = byte (cursor + 1) lsr 4 in
      let width =
        match operation with
        | 1 -> if register = 0 then 2 else 3
        | 4 | 8 -> 2
        | 5 | 9 -> 3
        | _ -> 1
      in
      require
        (slot + width <= slots)
        "truncated Windows array-frame unwind operand";
      if operation = 0 || operation = 4 || operation = 5 then
        require
          (register <> 10 && register <> 11)
          "Windows unwind misclassified source-owned volatile R10/R11";
      let saves =
        if operation = 4 then
          (register, word (cursor + 2) * 8, byte cursor) :: saves
        else saves
      in
      walk (slot + width) saves)
  in
  let saves = walk 0 [] in
  let expected =
    fixture.homes |> List.map snd |> List.sort_uniq compare
    |> List.filter (fun r -> r <> "R10" && r <> "R11")
  in
  require
    (List.map (fun (r, _, _) -> r) saves
    |> List.sort compare
    = (List.map number expected |> List.sort compare))
    "Windows unwind lost or invented a source-owned nonvolatile save";
  let frame = allocated_bytes code in
  List.iter
    (fun r ->
      let save = frame_moves true r code |> List.hd in
      let offset =
        signed32 (List.hd save.operands).value |> Option.get |> Int64.to_int
      in
      let _, scaled, endpoint =
        List.find (fun (n, _, _) -> n = number r) saves
      in
      require
        (scaled = frame + offset
        && endpoint
           = Int64.to_int save.address - first + String.length save.bytes)
        (Printf.sprintf
           "Windows unwind wrong source save extent for %s: encoded offset=%d \
            endpoint=%d; decoded offset=%d endpoint=%d bytes=%S"
           r scaled endpoint (frame + offset)
           (Int64.to_int save.address - first + String.length save.bytes)
           save.bytes))
    expected

let exclusions =
  [
    ( "zero-sized source range",
      "I64 F(I8 x){I64 scratch[0];x+=1;return x;}F(127);" );
    ( "zero-sized range after scalar",
      "I64 F(I8 x){I64 y;I64 scratch[0];x+=1;return x;}F(127);" );
    ( "explicit array request",
      "I64 F(I8 x){I64 reg RSI scratch[2];x+=1;return x;}F(127);" );
    ( "aggregate local",
      "class C{I64 value;};I64 F(I8 x){C scratch;x+=1;return x;}F(127);" );
    ("callback local", "I64 F(I8 x){I64 (*callback)();x+=1;return x;}F(127);");
    ("array of pointers", "I64 F(I8 x){I64 *scratch[2];x+=1;return x;}F(127);");
    ( "bare array decay",
      "I64 F(I8 x){I64 scratch[2];I64 *p=scratch;x+=1;return x;}F(127);" );
    ( "computed scalar array index",
      "I64 F(I8 x,I64 noreg i){I64 scratch[2];scratch[i+1]=42;x+=1;return \
       x;}F(127,0);" );
    ( "multidimensional indexed access",
      "I64 F(I8 x){I64 scratch[2][2];scratch[0][0]=42;x+=1;return x;}F(127);" );
  ]
  @ List.concat_map
      (fun index ->
        List.map
          (fun (label, tail) ->
            ( label ^ " index " ^ index,
              "I64 F(I8 x){I64 noreg scratch[2];I64 y=0;x+=1;" ^ tail index
              ^ "}F(127);" ))
          [
            ( "dead out-of-range address",
              fun i -> "if(0)scratch[" ^ i ^ "]=0;return x;" );
            ( "after-return out-of-range address",
              fun i -> "return x;scratch[" ^ i ^ "]=0;" );
          ])
      [ "-1"; "2" ]

let exclusion mode source =
  let runtime, fn = lowered mode source in
  require
    (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body = None)
    "unproved array domain acquired allocation evidence"

let rejected_sources =
  [
    (* Pinned PrsVar.HC:255-257 explicitly rejects '[' under PRS1B_FUN_ARG.
       The optimizer's positive-offset dimension branches do not grant public
       fixed-array formal syntax. HCPARSE0011 is source-correct. *)
    ("fixed array formal", "I64 F(I64 a[2],I8 x){x+=1;return x;}", "HCPARSE0011");
    ( "narrow dynamic index still requires separate conversion lowering",
      "I64 F(I8 i){I64 noreg scratch[200];scratch[128]=42;i++;return \
       scratch[i];}F(127);",
      "HCRUN0003" );
    ( "Bool dynamic index still requires separate conversion lowering",
      "I64 F(I8 x,Bool index){I64 scratch[2];scratch[index]=20;x+=1;return \
       scratch[index]+x;}F(127,1);",
      "HCRUN0003" );
    ( "explicit array request has no executable physical home",
      "I64 F(I8 x){I64 reg RSI scratch[2];x+=1;return x;}F(127);",
      "HCBACK0002" );
    ( "mixed array-read numeric conversion remains separately unproved",
      "F64 F(F64 reg x){I64 noreg scratch[2];scratch[0]=42;return \
       x+scratch[0];}F(1.5);",
      "HCBACK0002" );
  ]
  @ List.map
      (fun qualifier ->
        ( "unsupported F64 array " ^ qualifier,
          weighted_source "F64" qualifier "[5000]" "" "",
          "HCRUN0001" ))
      [ "noreg"; ""; "reg" ]

let rejected_source mode (label, source, code) =
  match compile mode source with
  | Error ds ->
      require
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) ds)
        (label ^ ": expected " ^ code ^ ": " ^ diagnostics ds)
  | Ok _ -> failwith (label ^ ": unsupported source was admitted")

let public_faults =
  [
    ( "uninitialized array next to selected scalar",
      "I64 F(I8 x){I64 scratch[2];x+=1;return x+scratch[0];}F(127);",
      "HCIRVM0012" );
    ( "dynamic negative array index retains bounds",
      "I64 F(I8 x,I64 i){I64 scratch[2];scratch[0]=42;x+=1;return \
       x+scratch[i];}F(127,-1);",
      "HCIRVM0019" );
    ( "explicit element address retains whole-array bound",
      "I64 F(I8 x){I64 scratch[2];I64 *p=&scratch[0];*p=42;x+=1;return \
       x+p[2];}F(127);",
      "HCIRVM0019" );
  ]

let completed expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      require
        (Option.map (fun (word : Image.word) -> word.bits) result.final_value
        = Some expected)
        "wrong retained array result";
      result.executed_steps
  | Image.Fault f ->
      failwith
        ("unexpected retained array fault: "
        ^
        match f.kind with
        | Image.Frame_limit_exceeded -> "semantic frame"
        | Image.Native_stack_limit_exceeded -> "native stack"
        | Image.Step_limit_exceeded -> "step limit"
        | _ -> "other")

let fault kind report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      require
        (result.kind = kind && Option.is_some result.span)
        "wrong retained array fault or lost source span";
      result
  | Image.Completed _ -> failwith "expected retained array fault"

let with_retained image action =
  let retained = Runtime.retain image |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release retained |> checked)
    (fun () -> action retained)

let limits mode =
  let fixture = unused "I64" "noreg" general in
  let fn = semantic mode fixture in
  let image = image mode fixture.source in
  let private_bytes = compact_probe (instructions image) in
  require
    (private_bytes = Image.frame_bytes image)
    "F must own largest private frame";
  let control_source =
    Printf.sprintf "I64 F(%s){I64 noreg scratch[5000];return %s;}F(%s);"
      (letters 6 |> List.map (( ^ ) "I64 noreg ") |> String.concat ",")
      (weighted 6) (args 6)
  in
  let control =
    (compile mode control_source |> Result.map_error diagnostics |> checked)
      .value
  in
  require
    (Image.entry_stack_bytes image = Image.entry_stack_bytes control
    && private_bytes = allocated_bytes (instructions control) + (6 * 8))
    "array-owner physical save words were lost from independent private quota";
  let active = Image.entry_stack_bytes image + 16 + private_bytes in
  let parameter_bytes =
    Frame.function_locations fn.frame
    |> List.filter (fun loc -> Frame.location_kind loc = Frame.Named_parameter)
    |> List.length |> ( * ) 8
  in
  let semantic_bytes =
    (Frame.function_frame_size fn.frame |> Int64.to_int) + parameter_bytes
  in
  let code = Image.code_bytes image and ir = Image.ir_instructions image in
  ignore
    (compile ~max_stack_bytes:private_bytes ~max_code_bytes:code
       ~max_ir_instructions:ir mode fixture.source
    |> Result.map_error diagnostics
    |> checked);
  List.iter
    (fun result ->
      require (Result.is_error result) "one-below compile quota admitted")
    [
      compile ~max_stack_bytes:(private_bytes - 1) mode fixture.source;
      compile ~max_code_bytes:(code - 1) mode fixture.source;
      compile ~max_ir_instructions:(ir - 1) mode fixture.source;
    ];
  with_retained image (fun retained ->
      let run ?max_frame_bytes ?max_active_stack_bytes steps =
        Runtime.execute_retained_report ?max_frame_bytes ?max_active_stack_bytes
          ~max_steps:steps retained
      in
      let steps = run ~max_active_stack_bytes:active 100000 |> completed 21L in
      ignore
        (run ~max_frame_bytes:semantic_bytes ~max_active_stack_bytes:active
           steps
        |> completed 21L);
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
      require
        (short.executed_steps = steps - 1)
        "array exact step quota lost its charge";
      for _ = 1 to 3 do
        ignore (run ~max_active_stack_bytes:active steps |> completed 21L)
      done);
  List.iter
    (fun target ->
      let opts =
        [
          "--frame-byte-limit=" ^ string_of_int semantic_bytes;
          "--stack-byte-limit=" ^ string_of_int private_bytes;
          "--active-stack-byte-limit=" ^ string_of_int active;
        ]
      in
      invoke ~options:opts ~target ~mode fixture.source |> value 21L;
      List.iter
        (fun (option, code) ->
          invoke ~status:1 ~options:(opts @ [ option ]) ~target ~mode
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
  List.iter
    (fun (source, kind) ->
      let image = image mode source in
      with_retained image (fun retained ->
          let run () =
            Runtime.execute_retained_report ~max_steps:100000
              ~max_active_stack_bytes:524288 retained
          in
          let failed = run () |> fault kind in
          require
            (failed.function_name = Some "F")
            "array fault lost owning function";
          ignore (run () |> completed 170L)))
    [
      ( "I64 Attempts=0;I64 F(I8 x,I64 index){I64 noreg \
         scratch[5000];scratch[4999]=42;x+=1;return \
         scratch[index]+x;}F(127,5000-Attempts++);",
        Image.Address_out_of_bounds );
      ( "I64 Attempts=0;I64 F(I8 x,I64 noreg yes){I64 noreg \
         scratch[5000];if(yes)scratch[4999]=42;x+=1;return \
         scratch[4999]+x;}F(127,Attempts++);",
        Image.Uninitialized_read );
      ( "I64 Attempts=0;I64 F(I8 x,I64 noreg divisor){I64 noreg \
         scratch[5000];scratch[4999]=42;x+=1;return \
         scratch[4999]/divisor+x;}F(127,Attempts++);",
        Image.Division_by_zero );
    ]

let failures = ref []

let check label action =
  try action ()
  with error ->
    failures := (label ^ ": " ^ Printexc.to_string error) :: !failures

let () =
  List.iter
    (fun mode ->
      List.iter
        (fun fixture ->
          check
            (mode ^ " " ^ fixture.label)
            (fun () ->
              ignore (semantic mode fixture);
              physical fixture (image mode fixture.source);
              (* Encoding another ABI is local metadata/byte coverage, not Windows
         execution. The real Windows host lane remains a separate CI check. *)
              if fixture == recursive_all then (
                let encoded =
                  image ~status_abi:Image.Windows_x64 mode fixture.source
                in
                physical fixture encoded;
                windows_unwind fixture encoded);
              List.iter
                (fun target ->
                  invoke ~target ~mode fixture.source |> value fixture.expected)
                ((if fixture.native_only then [] else [ "ir" ])
                @ if native then [ "host-jit"; "host-jit-task" ] else [])))
        cases;
      List.iter
        (fun (label, source) ->
          check (mode ^ " excludes " ^ label) (fun () -> exclusion mode source))
        exclusions;
      List.iter
        (fun ((label, _, _) as rejected) ->
          check
            (mode ^ " rejects " ^ label)
            (fun () -> rejected_source mode rejected))
        rejected_sources;
      List.iter
        (fun (label, source, code) ->
          List.iter
            (fun target ->
              check
                (mode ^ " " ^ target ^ " " ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode source |> diagnostic code))
            (if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]))
        public_faults;
      List.iter
        (fun target ->
          check
            (mode ^ " " ^ target ^ " published array-member example")
            (fun () ->
              invoke ~target ~mode
                (read "../examples/array-member-registers.hc")
              |> value 42L))
        (if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]);
      if native then (
        check (mode ^ " exact independent quotas") (fun () -> limits mode);
        check (mode ^ " same-image fault recovery") (fun () -> recoveries mode)))
    modes;
  Printf.printf
    "Array-member registers: %d cases, %d exclusions, %d rejections, %d CLI \
     reports (%s), %d failures\n"
    (List.length cases) (List.length exclusions)
    (List.length rejected_sources)
    !reports
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
