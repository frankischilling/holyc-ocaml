open Holyc_lib
open Yojson.Safe.Util
module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Scores = Holyc_lib__Ir.Exact_register_scores
module Lower = Holyc_lib__Ir.Integer_program_lowering
module Typed = Semantic_function_call_expression_result
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Symbol = Semantic_symbol
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence
module O = Ir_opcode
module Image = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler

(* Independent deductions from TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
   PrsExp:728-747 permits grouped indices, but clears array-base state;
   :1069-1099 emits one dimension stride and ADD per bracket. PrsVar:247-281
   builds dimension products. OptPass012:308-375,485-557 retains the variable
   subtree of x*0, removes x*1/x+0, and folds immediate-only arithmetic. It
   does not authorize flattening every address tree. OptPass3:193-197 scores
   the surviving range LEA; :129-134,156-162,239-245 cancels only an exact
   member-start consumer. The constants below are source deductions, not new
   TempleOS captures or maps obtained from the production analyzer. *)
let require condition message = if not condition then failwith message

let checked = function
  | Ok value -> value
  | Error error -> failwith error

let diagnostics ds =
  String.concat "; "
    (List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) ds)

let compiler = Sys.argv.(1)
let native = Array.exists (( = ) "--native") Sys.argv

let example_path =
  Array.to_list Sys.argv |> List.tl |> List.tl
  |> List.find_opt (fun argument ->
      not (String.starts_with ~prefix:"--" argument))
  |> Option.value
       ~default:
         (if Sys.file_exists "../examples/local-index-normalization.hc" then
            "../examples/local-index-normalization.hc"
          else "examples/local-index-normalization.hc")

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
    Session.add_source session ~path:"local-index-registers.hc" ~contents )

let compile ?(max_stack_bytes = 131072) ?(max_code_bytes = 1048576)
    ?max_ir_instructions ?status_abi mode contents =
  let session, source = input contents in
  Native_program.compile ~max_stack_bytes ~max_code_bytes ?max_ir_instructions
    ?status_abi session ~config:(config mode) ~source

let image ?status_abi mode contents =
  (compile ?status_abi mode contents |> Result.map_error diagnostics |> checked)
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
  let path = Filename.temp_file "holyc-local-index-" suffix in
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
              let defaults =
                [
                  "--stack-byte-limit=131072";
                  "--code-byte-limit=1048576";
                  "--active-stack-byte-limit=524288";
                ]
              in
              let key option = List.hd (String.split_on_char '=' option) in
              let defaults =
                List.filter
                  (fun d -> not (List.exists (fun o -> key o = key d) options))
                  defaults
              in
              let argv =
                Array.of_list
                  ([
                     compiler;
                     "run";
                     "--format=json";
                     "--target=" ^ target;
                     "--mode=" ^ mode;
                   ]
                  @ defaults @ options @ [ path ])
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out;
                    Unix.close err)
                  (fun () ->
                    Unix.create_process compiler argv Unix.stdin out err)
              in
              let _, actual = Unix.waitpid [] pid in
              let text = read output and stderr = read errors in
              require
                (actual = Unix.WEXITED status && stderr = "")
                (target ^ "/" ^ mode ^ ": " ^ text ^ "; stderr=" ^ stderr);
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
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

let instructions image =
  let sections =
    Image.native_sections image |> Holyc_lib__Asm.Native_sections.to_yojson
  in
  let address =
    sections |> member "symbols" |> to_list
    |> List.find (fun s -> member "name" s = `String "F")
    |> member "definition"
  in
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
  | Error error -> failwith error.message

let operands (i : Disassembly.instruction) =
  List.map (fun (o : Disassembly.operand) -> o.text) i.operands

let frame_moves save register code =
  List.filter
    (fun (i : Disassembly.instruction) ->
      i.opcode = "MOV"
      &&
      match i.operands with
      | [ dst; src ] ->
          let memory, reg = if save then (dst, src) else (src, dst) in
          reg.text = "U64 " ^ register
          && String.ends_with ~suffix:"[RBP]" memory.text
      | _ -> false)
    code

let parameter_map code =
  let rec pairs = function
    | (load : Disassembly.instruction) :: store :: rest ->
        let pair =
          match (load.operands, operands store) with
          | [ dst; src ], [ home; "U64 RAX" ]
            when load.opcode = "MOVSX" && dst.text = "U64 RAX"
                 && String.starts_with ~prefix:"U8 " src.text
                 && String.ends_with ~suffix:"[RBP]" src.text
                 && Option.fold ~none:false ~some:(fun x -> x >= 16L) src.value
                 && store.opcode = "MOV" ->
              List.find_map
                (fun r ->
                  if home = "U64 " ^ r then
                    Some ((Int64.to_int (Option.get src.value) - 16) / 8, r)
                  else None)
                registers
          | _ -> None
        in
        Option.to_list pair @ pairs (store :: rest)
    | _ -> []
  in
  pairs code |> List.sort_uniq compare

let allocated code =
  List.find_map
    (fun (i : Disassembly.instruction) ->
      match i.operands with
      | [ dst; n ] when i.opcode = "SUB" && dst.text = "U64 RSP" ->
          Option.map Int64.to_int n.value
      | _ -> None)
    code
  |> Option.get

let signed32 value =
  Option.map (fun n -> Int64.of_int32 (Int64.to_int32 n)) value

let compact_probe code =
  let frame = allocated code in
  require (frame > 65528) "fixture no longer crosses compact-probe threshold";
  let rec scan = function
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
          "wrong compact probe step, bound, or back edge";
        let allocation =
          if frame mod 4096 = 0 then List.hd rest
          else
            let tail = List.hd rest in
            require
              (tail.opcode = "TEST"
              && String.ends_with ~suffix:"[RSP]" (List.hd tail.operands).text
              && signed32 (List.hd tail.operands).value
                 = Some (Int64.of_int (-frame)))
              "wrong compact probe final partial page";
            List.nth rest 1
        in
        require
          (allocation.opcode = "SUB"
          && List.hd (operands allocation) = "U64 RSP"
          && (List.nth allocation.operands 1).value = Some (Int64.of_int frame)
          )
          "probe must finish before the owning allocation";
        frame
    | _ :: rest -> scan rest
    | [] -> failwith "missing compact probe"
  in
  scan code

type fixture = {
  label : string;
  source : string;
  expected : int64;
  homes : (string * string) list;
  parameters : (int * string) list option;
  ledger : (string * int64 * int64) list option;
  dimensions : int64 list;
  element_size : int64;
  strides : int64 list option;
  byte_offsets : int64 list option;
  large : bool;
  native_only : bool;
}

let fixture ?parameters ?ledger ?strides ?byte_offsets
    ?(dimensions = [ 2L; 3L ]) ?(element_size = 8L) ?(large = false)
    ?(native_only = false) label source expected homes =
  {
    label;
    source;
    expected;
    homes;
    parameters;
    ledger;
    dimensions;
    element_size;
    strides;
    byte_offsets;
    large;
    native_only;
  }

let letters = [ "a"; "b"; "c"; "d"; "e"; "f" ]
let general = List.combine letters [ "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ]
let pressure = List.combine letters [ "R12"; "R13"; "RSI"; "RDI"; "R10"; "R11" ]
let weighted = "a+a+a+a+a+a+b+b+b+b+b+c+c+c+c+d+d+d+e+e+f"
let updates = "a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;"
let narrow_parameters = "I8 a,I8 b,I8 c,I8 d,I8 e,I8 f"
let entries homes = List.mapi (fun i name -> (i, List.assoc name homes)) letters

let scalar_ledger =
  [
    ("a", 14L, 0L);
    ("b", 12L, 0L);
    ("c", 10L, 0L);
    ("d", 8L, 0L);
    ("e", 6L, 0L);
    ("f", 4L, 0L);
  ]

let indexed ?(type_ = "I64") ?(element_size = 8L) ?(dimensions = [ 4L ])
    ?(i_value = 1L) ?(strides = [ 8L; 8L ]) label suffix start i_reads j_reads =
  let shape = String.concat "" (List.map (Printf.sprintf "[%Ld]") dimensions) in
  let access = "scratch" ^ suffix in
  let homes = if start then pressure else general in
  fixture ~parameters:(entries homes) ~dimensions ~element_size ~strides
    ~ledger:
      (scalar_ledger
      @ [
          ("i", i_reads, 0L);
          ("j", j_reads, 0L);
          ("scratch", (if start then 4L else 2L), if start then 0L else 2L);
        ])
    label
    (Printf.sprintf
       "I64 F(%s,I64 noreg i,I64 noreg j){%s reg scratch%s;%s=42;%sreturn \
        %s+%s;}F(127,127,127,127,127,127,%Ld,2);"
       narrow_parameters type_ shape access updates weighted access i_value)
    2730L homes

let grouped_cases =
  [
    indexed "grouped literal zero" "[(0)]" true 0L 0L;
    indexed "computed constant zero" "[1-1]" true 0L 0L;
    indexed "large constants cancel before bounds check" "[100-100]" true 0L 0L;
    indexed "nested constant folding" "[((2*3)-(4+2))]" true 0L 0L;
    indexed "unary plus zero" "[+0]" true 0L 0L;
    indexed "constant product zero" "[0*9]" true 0L 0L;
    indexed "computed interior" "[1+1]" false 0L 0L;
    indexed "unary minus constant" "[-(1-2)]" false 0L 0L;
    indexed "grouped variable" "[(i)]" false 4L 0L;
    indexed "right zero identity" "[i+0]" false 4L 0L;
    indexed "left zero identity" "[0+i]" false 4L 0L;
    indexed "subtract zero identity" "[i-0]" false 4L 0L;
    indexed "right product identity" "[i*1]" false 4L 0L;
    indexed "left product identity" "[1*i]" false 4L 0L;
    indexed "multiply by zero retains index read and dynamic LEA" "[i*0]" false
      4L 0L;
    indexed "nested constant reassociation retains index" "[(i+1)-1]" false 4L
      0L;
    indexed ~i_value:0L ~type_:"U8" ~element_size:1L ~strides:[ 1L; 1L ]
      "byte sign-bit multiplier retains source read" "[i*0x8000000000000000]"
      false 4L 0L;
    indexed "repeated index occurrence" "[i+i]" false 8L 0L;
    indexed ~type_:"U8" ~element_size:1L ~strides:[ 1L; 1L ]
      "byte stride multiply-one preserves grouped index" "[(i+0)]" false 4L 0L;
  ]

let multidimensional_cases =
  List.map
    (fun (label, suffix, start, i, j) ->
      indexed ~dimensions:[ 2L; 3L ] ~strides:[ 24L; 8L; 24L; 8L ] label suffix
        start i j)
    [
      ("two-dimensional exact start", "[0][0]", true, 0L, 0L);
      ("two-dimensional grouped computed start", "[(1-1)][(2-2)]", true, 0L, 0L);
      ("two-dimensional offset forty", "[1][2]", false, 0L, 0L);
      ("two-dimensional both dynamic", "[i][j]", false, 4L, 4L);
      ("zero prefix dynamic suffix", "[0][j]", false, 0L, 4L);
      ("constant prefix dynamic suffix", "[1][j]", false, 0L, 4L);
      ("dynamic prefix zero suffix", "[i][0]", false, 4L, 0L);
      ("dynamic prefix constant suffix", "[i][2]", false, 4L, 0L);
      ("dynamic zero product is not constant prefix", "[i*0][0]", false, 4L, 0L);
    ]
  @ [
      indexed ~dimensions:[ 2L; 3L; 4L ]
        ~strides:[ 96L; 32L; 8L; 96L; 32L; 8L ]
        "three-dimensional full chain" "[(i)][j-1][1+2]" false 4L 4L;
      indexed ~type_:"U8" ~element_size:1L ~dimensions:[ 3L; 2L ]
        ~strides:[ 2L; 1L; 2L; 1L ]
        "byte matrix prefix addition retains phase order" "[i+1][1]" false 4L 0L;
      indexed ~type_:"U8" ~element_size:1L ~dimensions:[ 3L; 2L ]
        ~strides:[ 2L; 1L; 2L; 1L ] "byte matrix dynamic prefix constant suffix"
        "[i][1]" false 4L 0L;
      indexed ~type_:"U8" ~element_size:1L ~dimensions:[ 3L; 2L ]
        ~strides:[ 2L; 1L; 2L; 1L ]
        "byte matrix zero product preserves source read" "[i*0][0]" false 4L 0L;
    ]

let large =
  fixture ~parameters:(entries general) ~dimensions:[ 100L; 50L ] ~large:true
    "large normalized local chain and unequal widened parameters"
    (Printf.sprintf
       "I64 F(%s){I64 noreg \
        scratch[100][50];scratch[(1-1)][(2-2)]=20;scratch[99][48+1]=22;%sreturn \
        %s+scratch[0][0]+scratch[99][49];}F(127,127,127,127,127,127);"
       narrow_parameters updates weighted)
    2730L general

let composed_cases =
  [
    fixture
      ~strides:[ 24L; 8L; 24L; 8L; 24L; 8L; 24L; 8L ]
      ~byte_offsets:[ 0L; 40L; 0L; 40L ]
      "constant zero and forty byte offsets remain distinct"
      "I64 F(){I64 scratch[2][3];scratch[0][0]=20;scratch[1][2]=22;return \
       scratch[0][0]+scratch[1][2];}F();"
      42L [];
    fixture ~dimensions:[ 4L ]
      "multiply-zero scalar reads decide equal-score homes"
      "I64 F(I8 x,I64 i){I64 reg scratch[4];scratch[i*0]=20;x+=1;return \
       scratch[i*0]+x;}F(127,1);"
      148L
      [ ("i", "RSI"); ("x", "RDI") ];
    fixture "full computed address preserves owned pointer"
      "I64 F(I8 x){I64 scratch[2][3];I64 \
       *p=&scratch[(1)][1+1];*p=42;x+=1;return x+*p;}F(127);"
      170L
      [ ("p", "RSI"); ("x", "RDI") ];
    fixture ~dimensions:[ 4L ]
      ~ledger:[ ("x", 4L, 0L); ("scratch", 1L, 1L); ("p", 6L, 0L) ]
      "computed start address does not cancel its LEA"
      "I64 F(I8 x){I64 scratch[4];I64 *p=&scratch[1-1];*p=42;x+=1;return \
       x+*p;}F(127);"
      170L
      [ ("p", "RSI"); ("x", "RDI") ];
    fixture ~dimensions:[ 4L ] ~strides:[ 8L; 8L ]
      ~ledger:[ ("x", 4L, 0L); ("i", 2L, 0L); ("scratch", 3L, 1L) ]
      "nested fixed calls retain index preliminary optimizer passes"
      "I64 Id(I64 value){return value;}I64 F(I8 x,I64 i){I64 \
       scratch[4];scratch[0]=42;x+=1;return \
       x+Id(Id(scratch[(i*2)*(1+1)]));}F(127,0);"
      170L
      [ ("x", "RSI"); ("i", "RDI") ];
    fixture ~dimensions:[ 2L ] ~element_size:1L ~strides:[ 1L; 1L ]
      ~ledger:[ ("x", 4L, 0L); ("i", 2L, 0L); ("scratch", 3L, 1L) ]
      "standalone nested shifts remain unmerged at the source pass boundary"
      "I64 F(I8 x,I64 i){U8 scratch[2];scratch[0]=42;x+=1;return \
       x+scratch[((i*2)*(1+1))*0x4000000000000000];}F(127,0);"
      170L
      [ ("x", "RSI"); ("i", "RDI") ];
    large;
    fixture ~dimensions:[ 100L; 50L ] ~large:true
      "recursive normalized arrays preserve scalar homes"
      (Printf.sprintf
         "I64 F(I64 noreg n,%s){I64 noreg \
          scratch[100][50];scratch[(0)][0]=20;scratch[99][48+1]=22;if(n)return \
          F(n-1,a,b,c,d,e,f)+%s+scratch[0][0]+scratch[99][49];return \
          %s+scratch[0][0]+scratch[99][49];}F(1,1,1,1,1,1,1);"
         (String.concat "," (List.map (( ^ ) "I64 ") letters))
         weighted weighted)
      126L general;
    fixture ~dimensions:[ 100L; 50L ] ~large:true ~native_only:true
      "F64 and descriptor survive child beside normalized local arrays"
      "I64 V=2;F64 Child(F64 reg y){return y+1.0;}F64 F(F64 reg x,I64 *p){I64 \
       noreg \
       scratch[100][50];scratch[(0)][0]=20;scratch[99][48+1]=22;if(scratch[0][0]+scratch[99][49]+*p==44)return \
       Child(x)+x;return 0.0;}F(1.5,&V);"
      (Int64.bits_of_float 4.0)
      [ ("x", "RSI"); ("p", "RDI") ];
  ]

let unreachable_cases =
  List.map
    (fun (label, before, after) ->
      fixture ~parameters:(entries pressure) label
        (Printf.sprintf
           "I64 F(%s){I64 scratch[2][3];%s%sreturn \
            %s;%s}F(127,127,127,127,127,127);"
           narrow_parameters before updates weighted after)
        2688L pressure)
    [
      ( "constant-dead grouped chain still supplies balanced pressure",
        "if(0)scratch[(1-1)][0]=42;",
        "" );
      ( "zero-trip grouped chain still supplies balanced pressure",
        "while(0)scratch[(1-1)][0]=42;",
        "" );
      ( "after-return grouped chain still supplies balanced pressure",
        "",
        "scratch[(1-1)][0]=42;" );
    ]

let element_updates =
  fixture ~parameters:(entries general) ~element_size:1L
    ~ledger:
      (scalar_ledger @ [ ("i", 8L, 0L); ("j", 8L, 0L); ("scratch", 4L, 4L) ])
    ~strides:[ 3L; 1L; 3L; 1L; 3L; 1L; 3L; 1L ]
    "computed multidimensional U8 updates retain element width"
    (Printf.sprintf
       "I64 F(%s,I64 noreg i,I64 noreg j){U8 reg \
        scratch[2][3];scratch[(i)][j+0]=255;scratch[i][j]++;scratch[i][j]+=1;%sreturn \
        %s+scratch[i][j];}F(127,127,127,127,127,127,1,2);"
       narrow_parameters updates weighted)
    2689L general

let cases =
  grouped_cases @ multidimensional_cases @ composed_cases @ unreachable_cases
  @ [ element_updates ]

(* These matrix fixtures have only uninitialized declarations, expression roots
   and one return. Reconstruct their source-ordered roots directly from typed
   results. Expected score and LEA totals above remain literal and independent. *)
let flat_statements body =
  let source = Body.definition_source body |> Option.get in
  require (Typed.function_conditions source = []) "ledger fixture is not flat";
  let span value =
    match Typed.result_origin value with
    | Symbol.Source_location location -> location.span.start
    | _ -> failwith "missing source location"
  in
  let roots =
    List.map
      (fun root ->
        ( span (Typed.expression_statement_value root),
          Lower.Expression (Typed.expression_statement_value root) ))
      (Typed.function_expression_statements source)
    @ List.map
        (fun root ->
          (span (Typed.initializer_value root), Lower.Initialize root))
        (Typed.function_initializers source)
    @ List.map
        (fun root ->
          (span (Typed.return_value root |> Option.get), Lower.Return root))
        (Typed.function_returns source)
  in
  List.sort (fun (a, _) (b, _) -> compare a b) roots |> List.map snd

let stride_graph fn expected =
  let code =
    Graph.blocks (Body.body fn.Ir_integer_interpreter.body)
    |> List.concat_map (fun b ->
        Graph.instructions b |> Seq.instructions |> List.map Seq.description)
  in
  let producer id =
    List.find
      (fun (d : Seq.description) ->
        Option.fold ~none:false
          ~some:(fun r -> Seq.Value_id.equal r.Seq.value_id id)
          d.result)
      code
  in
  let actual =
    List.filter_map
      (fun (d : Seq.description) ->
        if d.opcode <> O.Ic_mul then None
        else
          match d.operands with
          | [ stride; _ ] -> (
              let stride = producer stride in
              match (stride.payload, stride.target_type) with
              | Some (Seq.Integer n), Some type_
                when Semantic_type.pointer_depth type_ > 0 -> Some n
              | _ -> None)
          | _ -> None)
      code
  in
  require (actual = expected)
    ("wrong ordered dimension strides: "
    ^ String.concat "," (List.map Int64.to_string actual))

(* For the constant-offset fixture, evaluate only canonical immediate pointer
   arithmetic rooted at RBP. No parameter value, optimizer recipe, or allocation
   answer is involved. The four final consumers must address 0,40,0,40 bytes. *)
let constant_byte_offsets fn expected =
  let location =
    Frame.function_locations fn.Ir_integer_interpreter.frame
    |> List.find (fun l -> Symbol.name (Frame.location_symbol l) = "scratch")
  in
  let start =
    Frame.location_frame_slot location
    |> Option.get |> Frame.frame_slot_displacement
  in
  let code =
    Graph.blocks (Body.body fn.body)
    |> List.concat_map (fun b ->
        Graph.instructions b |> Seq.instructions |> List.map Seq.description)
  in
  let producer id =
    List.find
      (fun (d : Seq.description) ->
        Option.fold ~none:false
          ~some:(fun r -> Seq.Value_id.equal r.Seq.value_id id)
          d.result)
      code
  in
  let rec constant depth id =
    require (depth > 0) "constant address graph exceeded fixture depth";
    let d = producer id in
    match (d.opcode, d.payload, d.operands) with
    | O.Ic_rbp, _, [] -> 0L
    | O.Ic_imm_i64, Some (Seq.Integer n), [] -> n
    | (O.Ic_add | O.Ic_mul), _, [ left; right ] ->
        let left = constant (depth - 1) left
        and right = constant (depth - 1) right in
        if d.opcode = O.Ic_add then Int64.add left right
        else Int64.mul left right
    | _ -> failwith "constant fixture has nonconstant address arithmetic"
  in
  let actual =
    List.filter_map
      (fun (d : Seq.description) ->
        match (d.opcode, d.operands) with
        | (O.Ic_assign | O.Ic_deref), address :: _ ->
            Some (Int64.sub (constant 32 address) start)
        | _ -> None)
      code
  in
  require (actual = expected)
    "constant full chains lost their exact byte offsets"

let semantic mode fixture =
  let runtime, fn = lowered mode fixture.source in
  let proof =
    Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body
    |> Option.get
  in
  Frame.function_locations fn.frame
  |> List.iter (fun location ->
      let symbol = Frame.location_symbol location in
      let expected =
        List.assoc_opt (Symbol.name symbol) fixture.homes |> Option.map number
      in
      require
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol
        = expected)
        ("wrong physical home for " ^ Symbol.name symbol);
      if Symbol.name symbol = "scratch" then (
        require
          (List.map Frame.dimension_value (Frame.location_dimensions location)
           = fixture.dimensions
          && Frame.location_element_size location = fixture.element_size
          && Frame.location_allocated_size location
             = List.fold_left Int64.mul fixture.element_size fixture.dimensions
          && Frame.location_alignment location
             =
             let size =
               List.fold_left Int64.mul fixture.element_size fixture.dimensions
             in
             (* PrsVar:596-606 aligns the total object, not its element. *)
             if size >= 8L then 8
             else if size >= 4L then 4
             else if size >= 2L then 2
             else 1)
          (Printf.sprintf
             "array metadata dimensions=%s element=%Ld allocation=%Ld \
              alignment=%d"
             (String.concat ","
                (List.map
                   (fun d -> Int64.to_string (Frame.dimension_value d))
                   (Frame.location_dimensions location)))
             (Frame.location_element_size location)
             (Frame.location_allocated_size location)
             (Frame.location_alignment location));
        require
          (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
          <> Some Analysis.Register_candidate)
          "array acquired a physical home"));
  Option.iter
    (fun expected ->
      let actual =
        Scores.analyze_members ~frame:fn.frame ~body:fn.body
          ~statements:(flat_statements fn.body)
        |> Option.get
        |> List.map (fun (s : Scores.member_score) ->
            ( Symbol.name (Frame.location_symbol s.location),
              s.score,
              s.lea_balance ))
      in
      require (actual = expected)
        ("wrong source-phase score/LEA ledger: "
        ^ String.concat ";"
            (List.map
               (fun (n, s, b) -> Printf.sprintf "%s=%Ld/%Ld" n s b)
               actual)))
    fixture.ledger;
  Option.iter (stride_graph fn) fixture.strides;
  Option.iter (constant_byte_offsets fn) fixture.byte_offsets;
  fn

let physical fixture image =
  let code = instructions image in
  Option.iter
    (fun expected ->
      require
        (parameter_map code = List.sort compare expected)
        ("decoded parameter homes differ from independent literal map: "
        ^ String.concat "; "
            (List.map
               (fun (i, r) -> Printf.sprintf "%d:%s" i r)
               (parameter_map code))))
    fixture.parameters;
  let saved = List.filter (fun r -> frame_moves true r code <> []) registers in
  require
    (List.sort compare saved
    = (List.map snd fixture.homes |> List.sort_uniq compare))
    "wrong source-register save owners";
  List.iter
    (fun r ->
      let saves = frame_moves true r code
      and restores = frame_moves false r code in
      require
        (List.length saves = 1 && List.length restores = 1)
        ("non-unique source-register save/restore for " ^ r);
      require
        ((List.hd (List.hd saves).operands).value
       = (List.nth (List.hd restores).operands 1).value)
        ("source-register save/restore slot differs for " ^ r))
    saved;
  if fixture.large then ignore (compact_probe code)

let windows_metadata fixture image =
  let code = instructions image in
  let first = Int64.to_int (List.hd code).address in
  let _, _, info =
    Image.windows_unwind_functions image
    |> List.find (fun (start, _, _) -> start = first)
  in
  let byte i = Char.code info.[i] in
  let word i = byte i lor (byte (i + 1) lsl 8) in
  let count = byte 2 in
  let rec decode slot saves =
    if slot = count then saves
    else (
      require (slot < count) "truncated Windows unwind slots";
      let p = 4 + (2 * slot) in
      let op = byte (p + 1) land 15 and reg = byte (p + 1) lsr 4 in
      let width =
        match op with
        | 1 -> if reg = 0 then 2 else 3
        | 4 | 8 -> 2
        | 5 | 9 -> 3
        | _ -> 1
      in
      require (slot + width <= count) "truncated Windows unwind operand";
      if List.mem op [ 0; 4; 5 ] then
        require
          (reg <> 10 && reg <> 11)
          "volatile source bank is unwind nonvolatile";
      let saves =
        if op = 4 then (reg, word (p + 2) * 8, byte p) :: saves else saves
      in
      decode (slot + width) saves)
  in
  let saves = decode 0 [] in
  let expected =
    List.map snd fixture.homes |> List.sort_uniq compare
    |> List.filter (fun r -> r <> "R10" && r <> "R11")
  in
  require
    (List.map (fun (r, _, _) -> r) saves
    |> List.sort compare
    = (List.map number expected |> List.sort compare))
    "wrong normalized-array Windows source-register owners";
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
        (scaled = allocated code + offset
        && endpoint
           = Int64.to_int save.address - first + String.length save.bytes)
        ("wrong normalized-array Windows save extent for " ^ r))
    expected

let exclusions =
  [
    ( "grouped array base",
      "I64 F(I8 x){I64 scratch[2][3];(scratch)[0][0]=42;x+=1;return x;}F(127);"
    );
    ( "grouped partial chain",
      "I64 F(I8 x){I64 scratch[2][3];(scratch[0])[0]=42;x+=1;return x;}F(127);"
    );
    ( "partial address escapes",
      "I64 F(I8 x){I64 scratch[2][3];I64 *p=scratch[0];x+=1;return x;}F(127);"
    );
    ( "array base escapes",
      "I64 F(I8 x){I64 scratch[2][3];I64 *p=scratch;x+=1;return x;}F(127);" );
    ( "index call",
      "I64 Index(){return 0;}I64 F(I8 x){I64 \
       scratch[2][3];scratch[Index()][0]=42;x+=1;return x;}F(127);" );
    ( "index update",
      "I64 F(I8 x,I64 i){I64 scratch[2][3];scratch[i++][0]=42;x+=1;return \
       x;}F(127,0);" );
    ( "index assignment",
      "I64 F(I8 x,I64 i){I64 scratch[2][3];scratch[i=0][0]=42;x+=1;return \
       x;}F(127,1);" );
    ( "index cast",
      "I64 F(I8 x,I64 i){I64 scratch[2][3];scratch[i(I64)][0]=42;x+=1;return \
       x;}F(127,0);" );
    ( "nested fixed-call context merges unsupported shift count sixty-four",
      "I64 Id(I64 value){return value;}I64 F(I8 x,I64 i){U8 \
       scratch[2];scratch[0]=42;x+=1;return \
       x+Id(Id(scratch[((i*2)*(1+1))*0x4000000000000000]));}F(127,0);" );
  ]
  @ List.concat_map
      (fun suffix ->
        List.map
          (fun (label, tail) ->
            ( label ^ suffix,
              "I64 F(I8 x,I64 noreg i){I64 noreg scratch[2][3];I64 y=0;x+=1;"
              ^ tail ("scratch" ^ suffix)
              ^ "}F(127,0);" ))
          [
            ("dead out-of-range ", fun a -> "if(0)" ^ a ^ "=0;return x;");
            ("after-return out-of-range ", fun a -> "return x;" ^ a ^ "=0;");
          ])
      [ "[-(1)][0]"; "[1+1][0]"; "[0][-(1)]"; "[0][1+2]"; "[i*0][-1]" ]

let exclusion mode label source =
  (* Publicly unsupported grouped/partial forms may fail before allocation.
     If they lower, none may silently acquire a whole-source allocation proof. *)
  let session, input = input source in
  match compile_integer_program session ~config:(config mode) ~source:input with
  | Error ds ->
      require
        (not
           (String.starts_with ~prefix:"dead " label
           || String.starts_with ~prefix:"after-return " label))
        ("out-of-range exclusion did not reach allocation: " ^ diagnostics ds)
  | Ok program ->
      let fn =
        integer_program_functions program.value
        |> List.find (fun (f : Ir_integer_interpreter.function_definition) ->
            Symbol.name (Body.symbol f.body) = "F")
      in
      require
        (Analysis.analyze_with_runtime
           ~runtime:(integer_program_runtime_calls program.value)
           ~frame:fn.frame ~body:fn.body
        = None)
        "excluded source acquired whole-function allocation authority"

let rejected_sources =
  [
    ( "fixed-array formal is source-rejected",
      "I64 F(I64 a[2][3]){return a[0][0];}",
      "HCPARSE0011" );
    ( "narrow dynamic index remains unsupported",
      "I64 F(I8 i){I64 scratch[2][3];scratch[(i)][0]=42;return \
       scratch[0][0];}F(0);",
      "HCRUN0003" );
    ( "Bool dynamic index remains unsupported",
      "I64 F(Bool i){I64 scratch[2][3];scratch[(i)][0]=42;return \
       scratch[0][0];}F(0);",
      "HCRUN0003" );
    ( "F64 array storage remains unsupported",
      "I64 F(){F64 scratch[2][3];return 42;}F();",
      "HCRUN0001" );
    ( "mixed array-read float arithmetic remains unsupported",
      "F64 F(F64 reg x){I64 scratch[2][3];scratch[0][0]=42;return \
       x+scratch[0][0];}F(1.5);",
      "HCBACK0002" );
  ]

let completed expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      require
        (Option.map (fun (w : Image.word) -> w.bits) result.final_value
        = Some expected)
        "wrong retained normalized-index result";
      result.executed_steps
  | Image.Fault _ -> failwith "unexpected retained normalized-index fault"

let fault expected report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      require
        (result.kind = expected && Option.is_some result.span)
        "wrong normalized-index fault or missing source span";
      result
  | Image.Completed _ -> failwith "expected normalized-index fault"

let retained image action =
  let handle = Runtime.retain image |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release handle |> checked)
    (fun () -> action handle)

let limits mode =
  let fn = semantic mode large in
  let image = image mode large.source in
  let private_bytes = compact_probe (instructions image) in
  require
    (private_bytes = Image.frame_bytes image)
    "F must own largest private frame";
  let control_source =
    Printf.sprintf
      "I64 F(%s){I64 noreg \
       scratch[100][50];scratch[(1-1)][(2-2)]=20;scratch[99][48+1]=22;%sreturn \
       %s+scratch[0][0]+scratch[99][49];}F(127,127,127,127,127,127);"
      (String.concat "," (List.map (( ^ ) "I8 noreg ") letters))
      updates weighted
  in
  let control =
    (compile mode control_source |> Result.map_error diagnostics |> checked)
      .value
  in
  require
    (Image.entry_stack_bytes image = Image.entry_stack_bytes control
    && private_bytes = allocated (instructions control) + (6 * 8))
    "source-register saves must consume six distinct private quota words";
  let semantic_bytes =
    Int64.to_int (Frame.function_frame_size fn.frame) + (6 * 8)
  in
  let active = Image.entry_stack_bytes image + 16 + private_bytes in
  let code = Image.code_bytes image and ir = Image.ir_instructions image in
  ignore
    (compile ~max_stack_bytes:private_bytes ~max_code_bytes:code
       ~max_ir_instructions:ir mode large.source
    |> Result.map_error diagnostics
    |> checked);
  List.iter
    (fun result ->
      require (Result.is_error result) "one-below compile quota admitted")
    [
      compile ~max_stack_bytes:(private_bytes - 1) mode large.source;
      compile ~max_code_bytes:(code - 1) mode large.source;
      compile ~max_ir_instructions:(ir - 1) mode large.source;
    ];
  retained image (fun handle ->
      let run ?max_frame_bytes ?max_active_stack_bytes steps =
        Runtime.execute_retained_report ?max_frame_bytes ?max_active_stack_bytes
          ~max_steps:steps handle
      in
      let steps =
        run ~max_active_stack_bytes:active 100000 |> completed large.expected
      in
      ignore
        (run ~max_frame_bytes:semantic_bytes ~max_active_stack_bytes:active
           steps
        |> completed large.expected);
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
        "exact step quota lost a charge";
      for _ = 1 to 3 do
        ignore
          (run ~max_frame_bytes:semantic_bytes ~max_active_stack_bytes:active
             steps
          |> completed large.expected)
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
      invoke ~target ~mode ~options large.source |> value large.expected;
      List.iter
        (fun (option, code) ->
          let key s = List.hd (String.split_on_char '=' s) in
          let options =
            option :: List.filter (fun old -> key old <> key option) options
          in
          invoke ~status:1 ~target ~mode ~options large.source
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
      retained (image mode source) (fun handle ->
          let run () =
            Runtime.execute_retained_report ~max_steps:100000
              ~max_active_stack_bytes:524288 handle
          in
          let failed = run () |> fault kind in
          require
            (failed.function_name = Some "F")
            "fault lost normalized-array owner";
          for _ = 1 to 3 do
            ignore (run () |> completed 170L)
          done))
    [
      ( "I64 Attempts=0;I64 F(I8 x,I64 i){I64 noreg \
         scratch[100][50];scratch[99][49]=42;x+=1;return \
         scratch[(i+0)][48+1]+x;}F(127,100-(Attempts++!=0));",
        Image.Address_out_of_bounds );
      ( "I64 Attempts=0;I64 F(I8 x,I64 noreg yes){I64 noreg \
         scratch[100][50];if(yes)scratch[99][49]=42;x+=1;return \
         scratch[99][48+1]+x;}F(127,Attempts++);",
        Image.Uninitialized_read );
      ( "I64 Attempts=0;I64 F(I8 x,I64 noreg divisor){I64 noreg \
         scratch[100][50];scratch[99][49]=42;x+=1;return \
         scratch[99][48+1]/divisor+x;}F(127,(Attempts++!=0));",
        Image.Division_by_zero );
    ]

let public_faults =
  [
    ( "uninitialized multidimensional read",
      "I64 F(I8 x){I64 scratch[2][3];x+=1;return x+scratch[1][1+1];}F(127);",
      "HCIRVM0012" );
    ( "dynamic negative prefix",
      "I64 F(I8 x,I64 i){I64 scratch[2][3];scratch[0][0]=42;x+=1;return \
       x+scratch[(i)][0];}F(127,-1);",
      "HCIRVM0019" );
    ( "dynamic positive suffix out of whole extent",
      "I64 F(I8 x,I64 i){I64 scratch[2][3];scratch[0][0]=42;x+=1;return \
       x+scratch[1][i+0];}F(127,3);",
      "HCIRVM0019" );
  ]

let failures = ref []

let check label action =
  try action ()
  with error ->
    failures := (label ^ ": " ^ Printexc.to_string error) :: !failures

let targets = if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]

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
              if fixture == large then (
                (* Foreign ABI bytes and metadata only, not Windows execution. *)
                let encoded =
                  image ~status_abi:Image.Windows_x64 mode fixture.source
                in
                physical fixture encoded;
                windows_metadata fixture encoded);
              List.iter
                (fun target ->
                  invoke ~target ~mode fixture.source |> value fixture.expected)
                (if fixture.native_only then List.filter (( <> ) "ir") targets
                 else targets)))
        cases;
      List.iter
        (fun (label, source) ->
          check
            (mode ^ " excludes " ^ label)
            (fun () -> exclusion mode label source))
        exclusions;
      List.iter
        (fun (label, source, code) ->
          check
            (mode ^ " rejects " ^ label)
            (fun () ->
              match compile mode source with
              | Error ds ->
                  require
                    (List.exists (fun (d : Diagnostic.t) -> d.code = code) ds)
                    ("expected " ^ code ^ ": " ^ diagnostics ds)
              | Ok _ -> failwith "unproved execution domain admitted"))
        rejected_sources;
      List.iter
        (fun (label, source, code) ->
          List.iter
            (fun target ->
              check
                (mode ^ " " ^ target ^ " " ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode source |> diagnostic code))
            targets)
        public_faults;
      List.iter
        (fun target ->
          check
            (mode ^ " " ^ target ^ " maintained local-index example")
            (fun () -> invoke ~target ~mode (read example_path) |> value 42L))
        targets;
      if native then (
        check (mode ^ " exact independent resource limits") (fun () ->
            limits mode);
        check (mode ^ " same-image fault recovery") (fun () -> recoveries mode)))
    [ "jit"; "aot" ];
  Printf.printf
    "Normalized local indices: %d cases, %d exclusions, %d rejections, %d CLI \
     reports (%s), %d failures\n"
    (List.length cases) (List.length exclusions)
    (List.length rejected_sources)
    !reports
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
