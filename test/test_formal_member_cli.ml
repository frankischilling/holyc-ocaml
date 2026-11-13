open Holyc_lib
open Yojson.Safe.Util
module Image = X86_64_program
module Runtime = Native_program_execution
module Disassembly = Asm_disassembler
module Frame = Semantic_function_frame_layout
module Types = Semantic_function_type_resolution
module Bindings = Semantic_function_binding_index
module Symbol = Semantic_symbol

(* Source-derived assertions, not fresh TempleOS runtime captures.
   Reference: c26482bb6ad3f80106d28504ec5db3c6a360732c.
   PrsVar.HC:339-344 creates _anon_ only at ',', ';' or ')'. Every fixed
   formal is counted and gets eight bytes, including U0/I0 (529,619-628;
   PrsStmt:114-123). PrsDotDotDot:378-402 appends argc/argv after fixed slots.
   LexLib.HC:67-84,103-147 permits duplicate _anon_ and finds the first.
   PrsVar:607-614 reparses a local initializer as assignment to that lookup,
   but resets the newly declared member's warning count. OptPass3:29-36
   overwrites explicit register ownership in source order; OptPass4:198-215
   leaves shadowed members on stack. OptPass789A:712-716 loads only surviving
   positive-offset owners. Expected values/maps below are independently
   deduced, never obtained from the production allocation algorithm. *)
let require condition message = if not condition then failwith message

let checked = function
  | Ok value -> value
  | Error error -> failwith error

let diagnostic_text diagnostics =
  String.concat "; "
    (List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) diagnostics)

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-formal-member-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let compiler = Sys.argv.(1)
let native = Array.exists (( = ) "--native") Sys.argv
let targets = if native then [ "ir"; "host-jit"; "host-jit-task" ] else [ "ir" ]
let modes = [ "jit"; "aot" ]
let reports = ref 0
let placements = ref 0
let metadata_checks = ref 0

let invoke ?(status = 0) ?(options = []) ~target ~mode label source =
  incr reports;
  temporary ".hc" source (fun input ->
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
                     "--code-byte-limit=1048576";
                   ]
                  @ options @ [ input ])
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
              let text = read output and errors = read errors in
              require
                (actual = Unix.WEXITED status && errors = "")
                (label ^ " " ^ target ^ " " ^ mode ^ ": " ^ text ^ "; stderr="
               ^ errors);
              Yojson.Safe.from_string text)))

let config mode =
  Preprocessor.Config.create
    ~compilation_mode:
      (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
    ()
  |> checked

let compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes mode contents
    =
  let session = Session.create () in
  let source = Session.add_source session ~path:"formal-member.hc" ~contents in
  Native_program.compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes
    session ~config:(config mode) ~source

let image mode source =
  (compile mode source |> Result.map_error diagnostic_text |> checked).value

let value ?(kind = "i64") label expected report =
  let word = member "final_value" report in
  require
    (member "outcome" report = `String "success"
    && member "type" word = `String kind
    && member "bits" word = `String (Printf.sprintf "0x%016Lx" expected))
    (label ^ ": " ^ Yojson.Safe.to_string report)

let rejected ?code ?(parser = false) label report =
  let diagnostics = member "diagnostics" report |> to_list in
  require
    (member "outcome" report = `String "error"
    && List.exists (fun d -> member "severity" d = `String "error") diagnostics
    )
    (label ^ ": " ^ Yojson.Safe.to_string report);
  let codes = List.map (fun d -> member "code" d |> to_string) diagnostics in
  require
    (List.exists (String.starts_with ~prefix:"HCPARSE") codes = parser)
    (label ^ ": unexpected rejection phase: " ^ String.concat "," codes);
  Option.iter
    (fun code ->
      require (List.mem code codes)
        (label ^ ": missing " ^ code ^ ": " ^ Yojson.Safe.to_string report))
    code

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let instructions label sections =
  let symbol =
    sections |> member "symbols" |> to_list
    |> List.find (fun symbol -> member "name" symbol = `String "F")
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
         (fun size mapping -> max size (member "size" mapping |> to_int))
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
        (Printf.sprintf "%s: decode byte %d: %s" label error.offset
           error.message)

let registers = [ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15" ]

let entry_mapping instructions =
  let rec loop result = function
    | (load : Disassembly.instruction)
      :: (store : Disassembly.instruction)
      :: rest ->
        let entry =
          match (load.operands, store.operands) with
          | [ destination; source ], [ target; value ]
            when List.mem load.opcode [ "MOV"; "MOVSX"; "MOVZX"; "MOVSXD" ]
                 && List.mem destination.text [ "U64 RAX"; "U32 EAX" ]
                 && String.ends_with ~suffix:"[RBP]" source.text
                 && Option.fold ~none:false
                      ~some:(fun n -> n >= 16L)
                      source.value
                 && store.opcode = "MOV" && value.text = "U64 RAX" ->
              List.find_map
                (fun register ->
                  if target.text = "U64 " ^ register then
                    Some
                      ( (Int64.to_int (Option.get source.value) - 16) / 8,
                        register )
                  else None)
                registers
          | _ -> None
        in
        loop
          (Option.fold ~none:result ~some:(fun entry -> entry :: result) entry)
          (store :: rest)
    | _ -> List.sort_uniq compare result
  in
  loop [] instructions

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

type placement = { entries : (int * string) list; saved : string list }

type case = {
  label : string;
  source : string;
  kind : string;
  bits : int64;
  placement : placement option;
  native_only : bool;
  module_boundary : bool;
}

let case ?(kind = "i64") ?(native_only = false) ?(module_boundary = false)
    ?entries ?saved label source bits =
  let placement =
    Option.map
      (fun entries ->
        {
          entries;
          saved =
            Option.value saved
              ~default:(List.map snd entries |> List.sort_uniq compare);
        })
      entries
  in
  {
    label;
    source;
    kind;
    bits;
    placement;
    native_only = native_only || kind = "f64";
    module_boundary;
  }

let check_placement case instructions =
  Option.iter
    (fun expected ->
      incr placements;
      let actual = entry_mapping instructions in
      let show entries =
        String.concat ","
          (List.map (fun (i, r) -> Printf.sprintf "%d:%s" i r) entries)
      in
      require
        (actual = List.sort_uniq compare expected.entries)
        (case.label ^ ": entry map " ^ show actual ^ ", expected "
       ^ show expected.entries);
      let saved =
        List.filter
          (fun register -> frame_moves ~save:true register instructions <> [])
          registers
      in
      require
        (List.sort compare saved = List.sort_uniq compare expected.saved)
        (case.label ^ ": saved " ^ String.concat "," saved ^ ", expected "
        ^ String.concat "," expected.saved);
      List.iter
        (fun register ->
          require
            (List.length (frame_moves ~save:true register instructions) = 1
            && List.length (frame_moves ~save:false register instructions) = 1)
            (case.label ^ ": expected one save and restore for " ^ register))
        expected.saved)
    case.placement

let scalar_cases =
  [
    case
      ~entries:[ (0, "RSI") ]
      "one unnamed formal is readable" "I64 F(I64){return _anon_;}F(42);" 42L;
    case
      ~entries:[ (0, "RSI") ]
      "repeated unnamed formals read first"
      "I64 F(I64,I64){return _anon_;}F(11,22);" 11L;
    case
      ~entries:[ (0, "RSI") ]
      "implicit before explicit _anon_"
      "I64 F(I64,I64 _anon_){return _anon_;}F(11,22);" 11L;
    case
      ~entries:[ (0, "RSI") ]
      "explicit before implicit _anon_"
      "I64 F(I64 _anon_,I64){return _anon_;}F(11,22);" 11L;
    case
      ~entries:[ (0, "RSI") ]
      "three explicit _anon_ formals"
      "I64 F(I64 _anon_,I64 _anon_,I64 _anon_){return _anon_;}F(11,22,33);" 11L;
    case
      ~entries:[ (1, "RSI") ]
      "unused unnamed slot precedes named argument"
      "I64 F(I64,I64 answer){return answer;}F(999,42);" 42L;
    case
      ~entries:[ (1, "R12"); (3, "R13") ]
      "unnamed slots preserve true positions"
      "I64 F(I64 noreg,I64 reg R12 named,I64 noreg,I64 reg R13 tail){return \
       _anon_+named+tail;}F(11,20,999,11);"
      42L;
    case ~module_boundary:true
      "prototype unnamed members are replaced by named definition"
      "extern I64 F(I64,I64);I64 F(I64 left,I64 right){return \
       left+right;}F(20,22);"
      42L;
    case ~module_boundary:true
      "prototype named members are replaced by unnamed definition"
      "extern I64 F(I64 left,I64 right);I64 F(I64,I64 right){return \
       _anon_+right;}F(20,22);"
      42L;
    case "explicit _anon_ default" "I64 F(I64 _anon_=42){return _anon_;}F();"
      42L;
    case "explicit _anon_ default before anonymous slot"
      "I64 F(I64 _anon_=40,I64){return _anon_+2;}F(,999);" 42L;
    case "default of later _anon_ does not replace first lookup"
      "I64 F(I64,I64 _anon_=22){return _anon_;}F(11);" 11L;
    case
      ~entries:[ (0, "RSI") ]
      "unnamed signed entry normalizes before word update"
      "I64 F(I8){_anon_+=1;return _anon_;}F(255);" 0L;
    case
      ~entries:[ (0, "RSI") ]
      "unnamed automatic narrow member widens"
      "I64 F(I8){_anon_+=1;return _anon_;}F(127);" 128L;
    case ~entries:[] "unnamed noreg member keeps declared width"
      "I64 F(I8 noreg){_anon_+=1;return _anon_;}F(127);" (-128L);
    case
      ~entries:[ (0, "RSI") ]
      "unnamed pointer retains descriptor"
      "I64 Value=42;I64 F(I64 *){return *_anon_;}F(&Value);" 42L;
    case "unnamed pointer updates owned pointee"
      "I64 Value=40;I64 F(I64 *){*_anon_+=2;return *_anon_;}F(&Value);" 42L;
    case "only empty parentheses mean zero fixed formals"
      "I64 F(){return 42;}F();" 42L;
    case "anonymous formal takes precedence over global spelling"
      "I64 _anon_=99;I64 F(I64){return _anon_;}F(42);" 42L;
    case "sizeof uses first narrow formal rather than new local"
      "I64 F(I8){I64 _anon_=37;return sizeof(_anon_);}F(11);" 1L;
    case "sizeof uses first wide local rather than narrow duplicate"
      "I64 F(){I64 _anon_=11;I8 _anon_=22;return sizeof(_anon_);}F();" 8L;
  ]

let initializer_cases =
  [
    case
      ~entries:[ (0, "RSI") ]
      "new local initializer assigns unnamed formal"
      "I64 F(I64){I64 _anon_=37;return _anon_;}F(11);" 37L;
    case "new local initializer assigns explicit _anon_ formal"
      "I64 F(I64 _anon_){I64 _anon_=37;return _anon_;}F(11);" 37L;
    case ~entries:[] ~saved:[ "RSI" ]
      "second local initializer assigns first local"
      "I64 F(){I64 _anon_=11;I64 _anon_=22;return _anon_;}F();" 22L;
    case "duplicate local initializer RHS reads first formal"
      "I64 F(I64){I64 _anon_=_anon_+31;return _anon_;}F(11);" 42L;
    case "duplicate local initializer RHS reads first local"
      "I64 F(){I64 _anon_=11;I64 _anon_=_anon_+31;return _anon_;}F();" 42L;
    case "three duplicate initializers target first local"
      "I64 F(){I64 _anon_=11;I64 _anon_=_anon_+20;I64 _anon_=_anon_+11;return \
       _anon_;}F();"
      42L;
    case ~entries:[] "first noreg signed type owns assignment conversion"
      "I64 F(I8 noreg){I64 _anon_=128;return _anon_;}F(0);" (-128L);
    case
      ~entries:[ (0, "RSI") ]
      "first register signed target owns widened assignment"
      "I64 F(I8){I64 _anon_=128;return _anon_;}F(0);" 128L;
    case ~entries:[] "new narrow type cannot truncate earlier full-width target"
      "I64 F(I64 noreg){I8 _anon_=258;return _anon_;}F(0);" 258L;
    case "duplicate local narrow type cannot truncate first local"
      "I64 F(){I64 noreg _anon_=0;I8 _anon_=258;return _anon_;}F();" 258L;
    case "pad duplicate initializer targets first scalar"
      "I64 F(){I64 noreg pad=0;I8 pad=258;return pad;}F();" 258L;
    case "reserved duplicate initializer targets first narrow scalar"
      "I64 F(){I8 noreg reserved=0;I64 reserved=128;return reserved;}F();"
      (-128L);
    case ~native_only:true
      "first integer target truncates floating duplicate initializer"
      "I64 F(I64 noreg){F64 _anon_=3e0/2e0;return _anon_;}F(0);" 1L;
    case ~kind:"f64"
      "first floating target keeps fractional integer-declared initializer"
      "F64 F(F64 noreg){I64 _anon_=3e0/2e0;return _anon_;}F(0e0);"
      0x3ff8000000000000L;
    case ~kind:"f64" "first floating target types RHS lookup"
      "F64 F(F64 noreg){I64 _anon_=_anon_+1e0;return _anon_;}F(3e0/2e0);"
      0x4004000000000000L;
    case ~kind:"f64" "first floating local remains initializer target"
      "F64 F(){F64 _anon_=3e0/2e0;I64 _anon_=_anon_+1e0;return _anon_;}F();"
      0x4004000000000000L;
    case "duplicate member identity survives recursive activations"
      "I64 F(I64,I64 noreg n){I64 _anon_=_anon_+1;if(n)return \
       F(_anon_,n-1)+_anon_;return _anon_;}F(10,2);"
      36L;
  ]

(* The forced first member precedes six score-4 candidates. QSort2b leaves
   [0;4;5;6;1;2;3]. An ordinary unused first member is sorted before trimming,
   leaving [4;5;6;3;1;2]. Neither map compacts argument indices. *)
let pressure_cases =
  [
    case
      ~entries:
        [
          (4, "R13"); (5, "RSI"); (6, "RDI"); (1, "R10"); (2, "R11"); (3, "R14");
        ]
      "unused forced unnamed formal participates in pressure"
      "I64 F(I64 reg,I8 a,I8 b,I8 c,I8 d,I8 e,I8 \
       f){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;return \
       a+b+c+d+e+f;}F(999,127,127,127,127,127,127);"
      768L;
    case
      ~entries:
        [
          (4, "RSI"); (5, "RDI"); (6, "R10"); (3, "R11"); (1, "R14"); (2, "R15");
        ]
      "unused ordinary unnamed formal is sorted before trimming"
      "I64 F(I64,I8 a,I8 b,I8 c,I8 d,I8 e,I8 \
       f){a+=1;b+=1;c+=1;d+=1;e+=1;f+=1;return \
       a+b+c+d+e+f;}F(999,127,127,127,127,127,127);"
      768L;
    case
      ~entries:[ (1, "RDI") ]
      "unused explicit unnamed formal reserves general home"
      "I64 F(I64 reg RSI,I8 answer){answer+=1;return answer;}F(999,127);" 128L;
  ]

(* A source-zero final duplicate owner reserves its register but has no proved
   readable home in this bounded implementation. Its optional unused prologue
   load/save is omitted. These checks distinguish that projection from the
   source owner table and never demand TempleOS byte identity for unused code. *)
let duplicate_cases =
  List.concat_map
    (fun register ->
      [
        case ~entries:[]
          (register ^ " later named request shadows unnamed member")
          (Printf.sprintf
             "I64 F(I8 reg %s,I8 reg %s winner){_anon_=128;return \
              _anon_;}F(0,0);"
             register register)
          (-128L);
        case
          ~entries:[ (1, register) ]
          (register ^ " later unnamed request shadows named member")
          (Printf.sprintf
             "I64 F(I8 reg %s first,I8 reg %s){_anon_=128;return \
              _anon_;}F(0,0);"
             register register)
          128L;
        case ~entries:[]
          (register ^ " three unnamed requests retain first lookup")
          (Printf.sprintf
             "I64 F(I8 reg %s,I8 reg %s,I8 reg %s){_anon_=128;return \
              _anon_;}F(0,0,0);"
             register register register)
          (-128L);
        case ~entries:[]
          (register ^ " local winner is not loaded from shadowed formal")
          (Printf.sprintf
             "I64 F(I8 reg %s){I64 reg %s spare;_anon_=128;return _anon_;}F(0);"
             register register)
          (-128L);
        case ~entries:[]
          (register ^ " duplicate local initializer writes shadowed formal")
          (Printf.sprintf
             "I64 F(I8 reg %s){I64 reg %s _anon_=128;return _anon_;}F(0);"
             register register)
          (-128L);
        case ~entries:[] ~saved:[ register ]
          (register ^ " used local winner has no parameter entry load")
          (Printf.sprintf
             "I64 F(I8 reg %s){I64 reg %s winner=7;_anon_=128;return \
              _anon_+winner;}F(0);"
             register register)
          (-121L);
      ])
    registers

let named_duplicate_cases =
  [
    case ~entries:[] "named shadow keeps declared width"
      "I64 F(I8 reg R12 a,I8 reg R12 b){a=128;return a;}F(0,0);" (-128L);
    case
      ~entries:[ (1, "R12") ]
      "named winner keeps full word"
      "I64 F(I8 reg R12 a,I8 reg R12 b){b=128;return b;}F(0,0);" 128L;
    case ~entries:[] "named local winner does not initialize from parameter"
      "I64 F(I8 reg R12 a){I64 reg R12 b;a=128;return a;}F(0);" (-128L);
    case ~kind:"f64" "shadowed F64 remains a floating stack value"
      "F64 F(F64 reg R12 a,F64 reg R12 b){a=a+1e0;return a;}F(3e0/2e0,9e0);"
      0x4004000000000000L;
    case ~kind:"f64" "shadowed F64 survives unused narrow winner"
      "F64 F(F64 reg R12 a,I8 reg R12 b){return a;}F(3e0/2e0,0);"
      0x3ff8000000000000L;
    case "named duplicate requests survive recursive calls"
      "I64 F(I64 reg R12 a,I64 reg R12 b){if(b)return F(a+1,b-1)+a;return \
       a;}F(10,2);"
      33L;
    case "duplicate owned pointer shadow retains descriptor"
      "I64 Value=40;I64 F(I64 reg R12 *a,I64 reg R12 *b){*a+=2;return \
       *a;}F(&Value,&Value);"
      42L;
    case
      ~entries:[ (1, "R12") ]
      "pointer winner owns register beside narrow shadow"
      "I64 Value=170;I64 F(I8 reg R12 x,I64 reg R12 *p){x=128;return \
       x+*p;}F(0,&Value);"
      42L;
    case
      ~entries:[ (1, "R12") ]
      "narrow winner owns register beside pointer shadow"
      "I64 Value=-86;I64 F(I64 reg R12 *p,I8 reg R12 x){x=128;return \
       *p+x;}F(&Value,0);"
      42L;
  ]

let example_case =
  let path =
    [
      "examples/formal-member-identity.hc";
      "../examples/formal-member-identity.hc";
    ]
    |> List.find Sys.file_exists
  in
  (* First lookup 11 + redirected initializer 37 + shadow/winner -128+128. *)
  case "checked-in formal member example" (read path) 48L

let cases =
  scalar_cases @ initializer_cases @ pressure_cases @ duplicate_cases
  @ named_duplicate_cases @ [ example_case ]

let parse mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"formal-member-metadata.hc" ~contents
  in
  let ast =
    parse_with_config session ~config:(config mode) ~source
    |> Result.map_error diagnostic_text
    |> checked
  in
  (session, ast)

let metadata mode source =
  let session, ast = parse mode source in
  let declarations = collect_declarations session ast |> checked in
  let aggregates = resolve_aggregates session ~declarations ast |> checked in
  let headers =
    resolve_aggregate_headers session ~declarations ~aggregates ast |> checked
  in
  let members = collect_members session ~declarations ast |> checked in
  let members =
    resolve_member_types session ~declarations ~aggregates ~headers ~members ast
    |> checked
  in
  let aggregate_layouts =
    layout_aggregates session ~declarations ~aggregates ~headers ~members ast
    |> checked
  in
  let functions = collect_functions session ~declarations ast |> checked in
  let function_types =
    resolve_function_types session ~declarations ~aggregates ~functions ast
    |> checked
  in
  let local_types =
    resolve_local_types session ~declarations ~aggregates ~functions ast
    |> checked
  in
  let bindings =
    index_function_bindings session ~declarations ~functions ~function_types
      ~local_types
    |> checked
  in
  let frames =
    layout_function_frames session ~declarations ~bindings ~function_types
      ~local_types ~aggregate_layouts ast
    |> checked
  in
  let typed = Types.functions function_types |> List.hd in
  let frame = Frame.functions frames |> List.hd in
  (typed, bindings, frame)

let metadata_case mode source expected_names fixed_count variadic =
  incr metadata_checks;
  let typed, bindings, frame = metadata mode source in
  let fixed = Types.function_signature typed |> Types.signature_parameters in
  require (List.length fixed = fixed_count) (source ^ ": wrong fixed arity");
  require
    (List.map Types.parameter_index fixed = List.init fixed_count Fun.id)
    (source ^ ": nonconsecutive fixed indices");
  let fixed_bindings = Types.function_parameter_bindings typed in
  require
    (List.map Types.parameter_binding_index fixed_bindings
    = List.init fixed_count Fun.id)
    (source ^ ": anonymous formal binding omitted");
  let locations = Frame.function_locations frame in
  require
    (List.map
       (fun location -> Symbol.name (Frame.location_symbol location))
       locations
    = expected_names)
    (source ^ ": incorrect source member order");
  let symbols = List.map Frame.location_symbol locations in
  let ids =
    List.map (fun symbol -> Symbol.Id.to_int (Symbol.id symbol)) symbols
  in
  require
    (List.length (List.sort_uniq compare ids) = List.length ids)
    (source ^ ": distinct members share an identity");
  List.iteri
    (fun index location ->
      let slot = Frame.location_frame_slot location |> Option.get in
      require
        (Frame.frame_slot_displacement slot = Int64.of_int (16 + (8 * index))
        && Frame.frame_slot_size slot = 8L
        && Frame.location_allocated_size location = 8L)
        (source ^ ": wrong eight-byte source slot at index "
       ^ string_of_int index))
    locations;
  (match Types.function_variadic_bindings typed with
  | None -> require (not variadic) (source ^ ": missing variadic members")
  | Some members ->
      require
        (variadic
        && Types.synthetic_binding_index (Types.variadic_argc members)
           = fixed_count
        && Types.synthetic_binding_index (Types.variadic_argv members)
           = fixed_count + 1)
        (source ^ ": wrong variadic indices"));
  let first =
    List.find_opt (fun symbol -> Symbol.name symbol = "_anon_") symbols
  in
  let lookup =
    Bindings.lookup bindings
      ~function_:(Types.function_symbol typed)
      ~name:"_anon_"
    |> Result.map_error Bindings.error_to_string
    |> checked
  in
  require
    (match (lookup, first) with
    | None, None -> true
    | Some binding, Some symbol -> Bindings.binding_symbol binding == symbol
    | _ -> false)
    (source ^ ": _anon_ lookup does not retain first member");
  List.iter2
    (fun parameter binding ->
      let index = Types.parameter_index parameter in
      let symbol = Types.parameter_binding_symbol binding in
      require
        (symbol == List.nth symbols index)
        (source ^ ": signature/frame identity mismatch");
      let source_parameter = Types.parameter_source parameter |> Option.get in
      require
        (Types.parameter_name parameter
        = Option.map
            (fun (name : Ast.identifier) -> name.spelling)
            source_parameter.name)
        (source ^ ": source spelling was rewritten");
      require
        (Types.parameter_has_flag parameter Member_flag.No_unused_warning
        = Option.is_none source_parameter.name)
        (source ^ ": unused-warning suppression differs from source spelling"))
    fixed fixed_bindings

let metadata_cases mode =
  List.iter
    (fun (source, names, count, variadic) ->
      metadata_case mode source names count variadic)
    [
      ("I64 F(){}", [], 0, false);
      ("I64 F(;;;){}", [], 0, false);
      ("I64 F(U0){}", [ "_anon_" ], 1, false);
      ("I64 F(I0){}", [ "_anon_" ], 1, false);
      ("I64 F(U0,I0,I64 named){}", [ "_anon_"; "_anon_"; "named" ], 3, false);
      ( "I64 F(I64,I64 _anon_,I64,I64 tail){}",
        [ "_anon_"; "_anon_"; "_anon_"; "tail" ],
        4,
        false );
      ( "I64 F(I64 _anon_,I64,I64 tail){}",
        [ "_anon_"; "_anon_"; "tail" ],
        3,
        false );
      ( "I64 F(I64,I64 named,I64,...){}",
        [ "_anon_"; "named"; "_anon_"; "argc"; "argv" ],
        3,
        true );
      ("I64 F(U0,...){}", [ "_anon_"; "argc"; "argv" ], 1, true);
      ("I64 F(I64 (*)()=0){}", [ "_anon_" ], 1, false);
    ]

let parser_cases mode =
  List.iter
    (fun source ->
      let session = Session.create () in
      let input =
        Session.add_source session ~path:"anonymous-default-rejected.hc"
          ~contents:source
      in
      match parse_with_config session ~config:(config mode) ~source:input with
      | Ok _ -> failwith ("anonymous scalar/pointer default parsed: " ^ source)
      | Error diagnostics ->
          require
            (List.exists
               (fun (d : Diagnostic.t) ->
                 String.starts_with ~prefix:"HCPARSE" d.code)
               diagnostics)
            (source ^ ": expected parser rejection, got "
            ^ diagnostic_text diagnostics))
    [
      "I64 F(I64=42);";
      "I64 F(I64 *=0);";
      "I64 F(I64=lastclass);";
      "I64 F(I64 (*callback)(I64=42));";
      "I64 F(I64 (*callback)(I64 *=0));";
    ];
  List.iter
    (fun source -> ignore (parse mode source))
    [
      "I64 F(I64 _anon_=42);";
      "I64 F(I64 *_anon_=0);";
      "I64 F(I64 (*)()=0);";
      "I64 F(I64 (*callback)(I64 (*)()=0));";
    ]

let recursive =
  case "recursive anonymous slots and retained quotas"
    "I64 F(I64,I64 noreg n,I64){if(n)return F(_anon_+1,n-1,999)+_anon_;return \
     _anon_;}F(10,2,999);"
    33L

let unsupported =
  [
    ("U0 formal is not a void-empty signature", "I64 F(U0){return 42;}F();");
    ("I0 formal is not a void-empty signature", "I64 F(I0){return 42;}F();");
    ("U0 formal execution remains unproved", "I64 F(U0){return 42;}F(0);");
    ("I0 formal execution remains unproved", "I64 F(I0){return 42;}F(0);");
    ( "one unnamed formal still requires one argument",
      "I64 F(I64){return 42;}F();" );
    ( "both unnamed formals contribute fixed arity",
      "I64 F(I64,I64){return _anon_;}F(11);" );
    ( "explicit address-taking stays unsupported",
      "I64 F(I64 reg R12){return *(&_anon_);}F(42);" );
    ( "reserved register stays unsupported",
      "I64 F(I64 reg RAX){return _anon_;}F(42);" );
  ]

let runtime_fault expected report =
  match Runtime.outcome report |> checked with
  | Image.Fault result ->
      require (result.kind = expected) "wrong retained formal/member fault";
      result
  | Image.Completed _ -> failwith "expected retained formal/member fault"

let completed expected report =
  match Runtime.outcome report |> checked with
  | Image.Completed result ->
      require
        (Option.map (fun (word : Image.word) -> word.bits) result.final_value
        = Some expected)
        "wrong retained formal/member value";
      result.executed_steps
  | Image.Fault _ -> failwith "unexpected retained formal/member fault"

let retained mode source action =
  let compiled = image mode source in
  let runtime = Runtime.retain compiled |> checked in
  Fun.protect
    ~finally:(fun () -> Runtime.release runtime |> checked)
    (fun () -> action compiled runtime)

let limits mode =
  (* Three fixed slots, including both anonymous formals, times three active
     F invocations give an exact logical frame cost of 72 bytes. *)
  List.iter
    (fun target ->
      let initial = invoke ~target ~mode recursive.label recursive.source in
      value recursive.label recursive.bits initial;
      let steps = member "executed_steps" initial |> to_int in
      invoke ~target ~mode recursive.label recursive.source
        ~options:
          [
            "--step-limit=" ^ string_of_int steps;
            "--frame-byte-limit=72";
            "--call-depth-limit=3";
          ]
      |> value recursive.label recursive.bits;
      List.iter
        (fun (option, code) ->
          invoke ~status:1 ~target ~mode recursive.label recursive.source
            ~options:[ option ]
          |> rejected ~code recursive.label)
        [
          ("--step-limit=" ^ string_of_int (steps - 1), "HCIRVM0007");
          ("--frame-byte-limit=71", "HCIRVM0011");
          ("--call-depth-limit=2", "HCIRVM0015");
        ])
    targets;
  if native then
    retained mode recursive.source (fun compiled runtime ->
        let ir = Image.ir_instructions compiled
        and code = Image.code_bytes compiled
        and stack = Image.frame_bytes compiled in
        ignore
          (compile ~max_ir_instructions:ir ~max_code_bytes:code
             ~max_stack_bytes:stack mode recursive.source
          |> Result.map_error diagnostic_text
          |> checked);
        List.iter
          (fun (label, result) ->
            require (Result.is_error result)
              (label ^ " accepts one below exact size"))
          [
            ("IR", compile ~max_ir_instructions:(ir - 1) mode recursive.source);
            ("code", compile ~max_code_bytes:(code - 1) mode recursive.source);
            ("frame", compile ~max_stack_bytes:(stack - 1) mode recursive.source);
          ];
        let run ?max_frame_bytes ?max_call_depth max_steps =
          Runtime.execute_retained_report ?max_frame_bytes ?max_call_depth
            ~max_steps runtime
        in
        let steps = run 100000 |> completed recursive.bits in
        ignore
          (run ~max_frame_bytes:72 ~max_call_depth:3 steps
          |> completed recursive.bits);
        ignore
          (run ~max_frame_bytes:71 steps
          |> runtime_fault Image.Frame_limit_exceeded);
        ignore
          (run ~max_call_depth:2 steps
          |> runtime_fault Image.Call_depth_exceeded);
        let short =
          run (steps - 1) |> runtime_fault Image.Step_limit_exceeded
        in
        require
          (short.executed_steps = steps - 1 && Option.is_some short.span)
          "formal/member step fault lost exact charge/source span";
        for _ = 1 to 3 do
          ignore (run steps |> completed recursive.bits)
        done)

let recoveries mode =
  List.iter
    (fun (source, kind, expected) ->
      retained mode source (fun _ runtime ->
          let run () =
            Runtime.execute_retained_report ~max_steps:100000 runtime
          in
          let failed = run () |> runtime_fault kind in
          require
            (failed.function_name = Some "F" && Option.is_some failed.span)
            "formal/member recovery fault lost source owner";
          ignore (run () |> completed expected)))
    [
      ( "I64 Attempts=0;I64 F(I64){I64 _anon_=42/_anon_;return \
         _anon_;}F(Attempts++);",
        Image.Division_by_zero,
        42L );
      ( "I64 Attempts=0;I64 Value=42;I64 F(I64 *,I64 noreg index){return \
         _anon_[index];}F(&Value,1-Attempts++);",
        Image.Address_out_of_bounds,
        42L );
      ( "I64 Attempts=0;I64 F(I64 reg R12,I64 reg R12 winner){return \
         _anon_/winner;}F(42,Attempts++);",
        Image.Division_by_zero,
        42L );
    ]

let warning_count code report =
  member "diagnostics" report
  |> to_list
  |> List.filter (fun diagnostic ->
      member "code" diagnostic = `String code
      && member "severity" diagnostic = `String "warning")
  |> List.length

let warnings mode =
  (* Warning producer checks are separate from execution/placement assertions.
     Implicit _anon_ alone carries MLF_NO_UNUSED_WARN. MemberAdd's lookup
     increments the first duplicate member before the new-local reset. *)
  List.iter
    (fun target ->
      List.iter
        (fun (label, source, expected) ->
          let report =
            invoke ~target ~mode label
              ((if target = "host-jit" then ""
                else "#exe {Option(16,1);Option(18,0);}")
              ^ source)
          in
          require
            (warning_count "HCSEMA0034" report = expected)
            (label ^ ": wrong unused-member warnings: "
            ^ Yojson.Safe.to_string report))
        [
          ("implicit anonymous is exempt", "I64 F(I64){return 42;}F(0);", 0);
          ( "explicit anonymous is not exempt",
            "I64 F(I64 _anon_){return 42;}F(0);",
            1 );
          ( "duplicate publication uses first explicit member",
            "I64 F(I64 _anon_,I64){return 42;}F(0,0);",
            0 );
          ( "later explicit member remains unused",
            "I64 F(I64,I64 _anon_){return 42;}F(0,0);",
            1 );
          ( "later explicit duplicate remains unused",
            "I64 F(I64 _anon_,I64 _anon_){return 42;}F(0,0);",
            1 );
          ( "initializer resets new local rather than target",
            "I64 F(){I64 _anon_=11;I64 _anon_=22;return 42;}F();",
            1 );
          ( "initializer reset preserves earlier formal use",
            "I64 F(I64 _anon_){I64 _anon_=37;return 42;}F(11);",
            1 );
        ];
      List.iter
        (fun (label, source, expected) ->
          let report = invoke ~target ~mode label source in
          require
            (warning_count "HCREG0001" report = expected)
            (label ^ ": conflicts were not warned once: "
            ^ Yojson.Safe.to_string report))
        [
          ( "one original register conflict",
            "I64 F(I64 reg R12 a,I64 reg R12 b){return \
             a+b;}F(20,22);F(20,22);F(20,22);",
            1 );
          ( "two original register conflicts",
            "I64 F(I64 reg R12 a,I64 reg R12 b,I64 reg R12 c){return \
             a+b+c;}F(10,11,21);F(10,11,21);",
            2 );
          ( "anonymous original register conflict",
            "I64 F(I64 reg R12,I64 reg R12){return _anon_;}F(42,0);F(42,0);",
            1 );
        ])
    targets

let option_cases mode =
  List.iter
    (fun target ->
      List.iter
        (fun (source, expected) ->
          invoke ~target ~mode "NO_REG_VAR duplicate requests"
            ("#exe {Option(34,1);}" ^ source)
          |> value "NO_REG_VAR duplicate requests" expected)
        [
          ("I64 F(I8 reg R12 a,I8 reg R12 b){a=128;return a;}F(0,0);", -128L);
          ("I64 F(I8 reg R12 a,I8 reg R12 b){b=128;return b;}F(0,0);", 128L);
          ( "I64 F(I8 reg R12,I8 reg R12){_anon_=128;return _anon_;}F(0,0);",
            -128L );
        ])
    (List.filter (( <> ) "host-jit") targets)

let failures = ref []

let check label action =
  try action ()
  with error ->
    failures := (label ^ ": " ^ Printexc.to_string error) :: !failures

let () =
  List.iter
    (fun mode ->
      check (mode ^ " formal metadata") (fun () -> metadata_cases mode);
      check (mode ^ " anonymous default parser distinction") (fun () ->
          parser_cases mode);
      List.iter
        (fun case ->
          check
            (mode ^ " compiled placement " ^ case.label)
            (fun () ->
              Option.iter
                (fun _ ->
                  let compiled = image mode case.source in
                  check_placement case
                    (Image.native_sections compiled
                    |> Holyc_lib__Asm.Native_sections.to_yojson
                    |> instructions case.label))
                case.placement);
          List.iter
            (fun target ->
              check
                (mode ^ " " ^ target ^ " " ^ case.label)
                (fun () ->
                  let module_boundary =
                    case.module_boundary
                    && (target = "host-jit"
                       || (target = "host-jit-task" && mode = "aot"))
                  in
                  let report =
                    invoke
                      ~status:(if module_boundary then 1 else 0)
                      ~target ~mode case.label case.source
                  in
                  if module_boundary then
                    rejected ~code:"HCRUN0001" case.label report
                  else value ~kind:case.kind case.label case.bits report;
                  if target = "host-jit" then
                    Option.iter
                      (fun _ ->
                        check_placement case
                          (report |> member "native" |> member "image"
                         |> member "native_sections" |> instructions case.label
                          ))
                      case.placement))
            (if case.native_only then
               if native then [ "host-jit"; "host-jit-task" ] else []
             else targets))
        cases;
      List.iter
        (fun (label, source) ->
          List.iter
            (fun target ->
              check
                (mode ^ " " ^ target ^ " " ^ label)
                (fun () ->
                  invoke ~status:1 ~target ~mode label source
                  |> rejected
                       ~parser:
                         ((not
                             (target = "host-jit-task" && mode = "jit"
                             && List.mem label
                                  [
                                    "U0 formal is not a void-empty signature";
                                    "I0 formal is not a void-empty signature";
                                  ]))
                         && List.mem label
                              [
                                "U0 formal is not a void-empty signature";
                                "I0 formal is not a void-empty signature";
                                "one unnamed formal still requires one argument";
                                "both unnamed formals contribute fixed arity";
                              ])
                       label))
            targets)
        unsupported;
      check (mode ^ " anonymous IR variadic slots") (fun () ->
          invoke ~target:"ir" ~mode "anonymous IR variadic slots"
            "I64 F(I64,...){return _anon_+argc;}F(40,1,2);"
          |> value "anonymous IR variadic slots" 42L);
      check (mode ^ " exact formal/member limits") (fun () -> limits mode);
      check (mode ^ " original warning producers") (fun () -> warnings mode);
      check (mode ^ " NO_REG_VAR duplicate ownership") (fun () ->
          option_cases mode);
      if native then
        check (mode ^ " retained formal/member fault recovery") (fun () ->
            recoveries mode))
    modes;
  Printf.printf
    "Formal/member resolution: %d cases, %d CLI reports, %d placement checks, \
     %d metadata checks (%s), %d failures\n"
    (List.length cases) !reports !placements !metadata_checks
    (if native then "native" else "admission")
    (List.length !failures);
  if !failures <> [] then failwith (String.concat "\n" (List.rev !failures))
