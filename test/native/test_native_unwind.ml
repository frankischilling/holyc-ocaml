open Holyc_lib
module Image = X86_64_expression
module Program = X86_64_program
module Encoder = X86_64_encoder
module Runtime = Native_execution
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module X87 = Ir_x87_stack
module Opcode = Ir_opcode
module Type = Semantic_type

external probe_windows_unwind : string -> string -> int -> unit
  = "holyc_test_native_unwind"

external probe_windows_program_unwind :
  string -> (int * int * string) array -> unit
  = "holyc_test_native_program_unwind"

external execute_encoded_probe :
  string -> string -> int -> int64 * int64 * int64
  = "holyc_native_execute_image"

type retained_probe

external retain_probe :
  string * (int * int * string) array * int * int * (int * int * int * string) ->
  retained_probe = "holyc_native_retain_program"

external release_probe : retained_probe -> unit = "holyc_native_release_program"

let require condition message = if not condition then failwith message

let diagnostic_errors errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let require_ok show = function
  | Ok value -> value
  | Error errors -> failwith (show errors)

let sequence_error (error : Sequence.error) = error.code ^ ": " ^ error.message

let native_errors errors =
  errors
  |> List.map (fun (error : Image.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let mode_name = function
  | Preprocessor.Jit -> "jit"
  | Preprocessor.Aot -> "aot"

let right_nested count =
  require (count >= 1) "right-nested source needs at least one literal";
  let expression = ref (string_of_int count) in
  for value = count - 1 downto 1 do
    expression := Printf.sprintf "%d+(%s)" value !expression
  done;
  !expression ^ ";"

let compile_source ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-unwind-pressure.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  Native_expression.compile session ~config ~source
  |> require_ok diagnostic_errors

let compile_program_source ?(max_stack_bytes = 4088) ?max_code_bytes ~mode
    contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-program-unwind.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  Native_program.compile ~max_stack_bytes ?max_code_bytes session ~config
    ~source
  |> require_ok diagnostic_errors
  |> fun checked -> checked.value

let instruction_id id =
  Sequence.Instruction_id.of_int id |> require_ok sequence_error

let value_id id = Sequence.Value_id.of_int id |> require_ok sequence_error
let block_id id = Sequence.Block_id.of_int id |> require_ok sequence_error

let i64 =
  Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive_type.I64
    ~pointer_depth:0
  |> require_ok Fun.id

let description ?(operands = []) ?result ?target_type ?payload id opcode :
    Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands = List.map value_id operands;
    result = Option.map (fun id -> { Sequence.value_id = value_id id }) result;
    target_type;
    payload;
    flags = 0L;
    span = None;
  }

let pressure_image ?(max_stack_bytes = 4088) count =
  let imm id bits =
    description ~result:id ~target_type:i64 ~payload:(Sequence.Integer bits) id
      Opcode.Ic_imm_i64
  in
  let add id left right =
    description ~operands:[ left; right ] ~result:id ~target_type:i64 id
      Opcode.Ic_add
  in
  let return_value id operand =
    description ~operands:[ operand ] ~target_type:i64 id Opcode.Ic_return_val
  in
  let definitions =
    List.init count (fun id -> imm id (Int64.of_int (id + 1)))
  in
  let rec reduce next accumulator = function
    | [] ->
        [ return_value next accumulator; description (next + 1) Opcode.Ic_ret ]
    | operand :: rest ->
        add next accumulator operand :: reduce (next + 1) next rest
  in
  let instructions =
    definitions @ reduce count 0 (List.init (count - 1) (fun id -> id + 1))
  in
  let graph =
    Graph.create ~entry:(block_id 0)
      [ { Graph.block_id = block_id 0; instructions } ]
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : Graph.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
    |> X87.verify
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : X87.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
  in
  Image.compile ~max_stack_bytes ~max_ir_instructions:4096 ~max_code_bytes:65536
    graph
  |> require_ok native_errors

let div_pressure_image ~status_abi count =
  require (count >= 2) "division pressure image needs at least two literals";
  let imm id bits =
    description ~result:id ~target_type:i64 ~payload:(Sequence.Integer bits) id
      Opcode.Ic_imm_i64
  in
  let binary id opcode left right =
    description ~operands:[ left; right ] ~result:id ~target_type:i64 id opcode
  in
  let return_value id operand =
    description ~operands:[ operand ] ~target_type:i64 id Opcode.Ic_return_val
  in
  let definitions =
    List.init count (fun id -> imm id (Int64.of_int (id + 1)))
  in
  let division_id = count in
  let rec reduce next accumulator = function
    | [] ->
        [ return_value next accumulator; description (next + 1) Opcode.Ic_ret ]
    | operand :: rest ->
        binary next Opcode.Ic_add accumulator operand
        :: reduce (next + 1) next rest
  in
  let instructions =
    definitions
    @ [ binary division_id Opcode.Ic_div 0 1 ]
    @ reduce (division_id + 1) division_id
        (List.init (count - 2) (fun id -> id + 2))
  in
  let graph =
    Graph.create ~entry:(block_id 0)
      [ { Graph.block_id = block_id 0; instructions } ]
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : Graph.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
    |> X87.verify
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : X87.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
  in
  Image.compile ~status_abi ~max_ir_instructions:4096 ~max_code_bytes:65536
    graph
  |> require_ok native_errors

let byte text index = Char.code text.[index]

let uint32_le text offset =
  byte text offset
  lor (byte text (offset + 1) lsl 8)
  lor (byte text (offset + 2) lsl 16)
  lor (byte text (offset + 3) lsl 24)

let string_of_bytes bytes =
  String.init (List.length bytes) (fun index -> Char.chr (List.nth bytes index))

let probe_size bytes =
  if bytes < 4096 then 0
  else if bytes <= 65528 then 8 * ((bytes + 4095) / 4096)
  else 27 + if bytes mod 4096 = 0 then 0 else 8

let int32_bytes value =
  List.init 4 (fun index -> (value lsr (index * 8)) land 0xff)

let expected_probe frame =
  let tail distance = [ 0x48; 0x85; 0x84; 0x24 ] @ int32_bytes (-distance) in
  if frame < 4096 then ""
  else if frame <= 65528 then
    List.init
      ((frame + 4095) / 4096)
      (fun index -> tail (min frame ((index + 1) * 4096)))
    |> List.concat |> string_of_bytes
  else
    ([
       0x48;
       0xc7;
       0xc0;
       0x00;
       0xf0;
       0xff;
       0xff;
       0x48;
       0x85;
       0x04;
       0x04;
       0x48;
       0x81;
       0xe8;
       0x00;
       0x10;
       0x00;
       0x00;
       0x48;
       0x81;
       0xf8;
     ]
    @ int32_bytes (-(frame / 4096 * 4096))
    @ [ 0x7d; 0xec ]
    @ if frame mod 4096 = 0 then [] else tail frame)
    |> string_of_bytes

let check_probe_code label frame code offset =
  let expected = expected_probe frame in
  require
    (String.length expected = probe_size frame
    && String.length code >= offset + String.length expected
    && String.sub code offset (String.length expected) = expected)
    (label ^ ": exact page-probe instructions, loop bound and branch target")

let expected_unwind frame_bytes =
  require
    (frame_bytes >= 8 && frame_bytes <= 524280 && frame_bytes mod 16 = 8)
    "test fixture frame is outside the bounded ABI shape";
  if frame_bytes <= 128 then
    string_of_bytes
      [
        0x01;
        7 + probe_size frame_bytes;
        0x01;
        0x00;
        7 + probe_size frame_bytes;
        (((frame_bytes - 8) / 8) lsl 4) lor 0x02;
        0x00;
        0x00;
      ]
  else
    let scaled = frame_bytes / 8 in
    string_of_bytes
      [
        0x01;
        7 + probe_size frame_bytes;
        0x02;
        0x00;
        7 + probe_size frame_bytes;
        0x01;
        scaled land 0xff;
        (scaled lsr 8) land 0xff;
      ]

let check_frame_code label frame_bytes code =
  let length = String.length code in
  let prefix = probe_size frame_bytes in
  require (length >= prefix + 15) (label ^ ": frameful image is too short");
  check_probe_code label frame_bytes code 0;
  require
    (byte code prefix = 0x48
    && byte code (prefix + 1) = 0x81
    && byte code (prefix + 2) = 0xec)
    (label ^ ": prologue is not SUB RSP,imm32");
  require
    (uint32_le code (prefix + 3) = frame_bytes)
    (label ^ ": prologue allocation differs from frame_bytes");
  let epilogue = length - 8 in
  require
    (byte code epilogue = 0x48
    && byte code (epilogue + 1) = 0x81
    && byte code (epilogue + 2) = 0xc4)
    (label ^ ": epilogue is not ADD RSP,imm32");
  require
    (uint32_le code (epilogue + 3) = frame_bytes)
    (label ^ ": epilogue deallocation differs from frame_bytes");
  require
    (byte code (length - 1) = 0xc3)
    (label ^ ": frameful image does not end in RET")

let expected_callable_unwind ?(saves = []) frame =
  require
    (frame >= 0 && frame <= 524272 && frame mod 16 = 0)
    "test fixture callable frame is outside the bounded ABI shape";
  require (frame <> 0 || saves = []) "frameless fixture cannot save registers";
  let allocation_end = if frame = 0 then 4 else 11 + probe_size frame in
  let prologue_end = allocation_end + (7 * List.length saves) in
  require (prologue_end <= 255) "callable prologue endpoint exceeds one byte";
  let save_codes =
    List.mapi
      (fun index (register, offset) ->
        require
          (register >= 12 && register <= 15 && offset < 0 && offset >= -frame
          && offset mod 8 = 0)
          "invalid callable register-save fixture";
        let scaled = (frame + offset) / 8 in
        [
          allocation_end + (7 * (index + 1));
          (register lsl 4) lor 4;
          scaled land 0xff;
          (scaled lsr 8) land 0xff;
        ])
      saves
    |> List.rev |> List.concat
  in
  let allocation_codes =
    if frame = 0 then []
    else if frame <= 128 then
      [ allocation_end; (((frame / 8) - 1) lsl 4) lor 2 ]
    else
      let scaled = frame / 8 in
      [ allocation_end; 1; scaled land 0xff; (scaled lsr 8) land 0xff ]
  in
  let codes = save_codes @ allocation_codes @ [ 1; 0x50 ] in
  let count = List.length codes / 2 in
  string_of_bytes
    ([ 1; prologue_end; count; 0 ]
    @ codes
    @ if count mod 2 = 0 then [] else [ 0; 0 ])

let check_callable_frame_code ?(saves = []) label frame code
    (first, last, unwind) =
  require
    (first >= 0 && last <= String.length code && last - first >= 6)
    (label ^ ": callable owner range");
  require
    (String.sub code first 4 = string_of_bytes [ 0x55; 0x48; 0x89; 0xe5 ])
    (label ^ ": callable PUSH RBP; MOV RBP,RSP");
  require
    (unwind = expected_callable_unwind ~saves frame)
    (label ^ ": exact callable Version 1 unwind bytes");
  check_probe_code label frame code (first + 4);
  if frame <> 0 then (
    let allocation = first + 4 + probe_size frame in
    require
      (last - first >= 4 + probe_size frame + 7 + 9
      && String.sub code allocation 3 = string_of_bytes [ 0x48; 0x81; 0xec ]
      && uint32_le code (allocation + 3) = frame)
      (label ^ ": callable allocation endpoint");
    require
      (String.sub code (last - 9) 3 = string_of_bytes [ 0x48; 0x81; 0xc4 ]
      && uint32_le code (last - 6) = frame)
      (label ^ ": callable frame deallocation"));
  require
    (String.sub code (last - 2) 2 = string_of_bytes [ 0x5d; 0xc3 ])
    (label ^ ": callable POP RBP; RET");
  let allocation_end = first + if frame = 0 then 4 else 11 + probe_size frame in
  let restore_begin = last - 9 - (7 * List.length saves) in
  List.iteri
    (fun index (register, offset) ->
      let modrm = 0x85 lor ((register - 8) lsl 3) in
      let instruction opcode =
        string_of_bytes ([ 0x4c; opcode; modrm ] @ int32_bytes offset)
      in
      require
        (String.sub code (allocation_end + (7 * index)) 7 = instruction 0x89
        && String.sub code (restore_begin + (7 * index)) 7 = instruction 0x8b)
        (label ^ ": matching ascending nonvolatile save and restore"))
    saves

let check_unwind_copy label expected image =
  let first = Image.windows_unwind_info image in
  let second = Image.windows_unwind_info image in
  require (first = expected && second = expected) (label ^ ": unwind bytes");
  if String.length first > 0 then
    Bytes.set (Bytes.unsafe_of_string first) 0 '\xff';
  require
    (second = expected && Image.windows_unwind_info image = expected)
    (label ^ ": unwind getter must return independent immutable copies")

let check_compiled_image ~windows ~label ~expected_frame image =
  require
    (Image.frame_bytes image = expected_frame)
    (Printf.sprintf "%s: expected frame %d, got %d" label expected_frame
       (Image.frame_bytes image));
  let code = Image.code image in
  let unwind = Image.windows_unwind_info image in
  if expected_frame = 0 then (
    require (unwind = "") (label ^ ": frameless image must have no unwind blob");
    check_unwind_copy label "" image)
  else
    let expected = expected_unwind expected_frame in
    require
      (String.length unwind = 8)
      (label ^ ": bounded frame must use the fixed eight-byte unwind blob");
    require (unwind = expected) (label ^ ": exact Version 1 unwind encoding");
    check_frame_code label expected_frame code;
    check_unwind_copy label expected image;
    if windows then probe_windows_unwind code expected expected_frame

let check_source_image ~windows ~mode ~literals ~expected_frame =
  let label =
    Printf.sprintf "%s %d live literals / %d-byte frame" (mode_name mode)
      literals expected_frame
  in
  check_compiled_image ~windows ~label ~expected_frame
    (compile_source ~mode (right_nested literals))

let status_abi_name = function
  | Image.Windows_x64 -> "windows"
  | Image.System_v_x64 -> "system-v"

let check_fault_image ~windows ~status_abi ~literals frame_shape =
  let image = div_pressure_image ~status_abi literals in
  let frame = Image.frame_bytes image in
  let label =
    Printf.sprintf "%s fault-capable %d live literals / %d-byte frame"
      (status_abi_name status_abi)
      literals frame
  in
  require
    (Image.status_abi image = Some status_abi)
    (label ^ ": fault-capable image lost its status ABI");
  (match frame_shape with
  | `Zero -> require (frame = 0) (label ^ ": expected a register-only image")
  | `Small ->
      require
        (frame >= 8 && frame <= 128)
        (label ^ ": expected a small bounded spill frame")
  | `Large ->
      require
        (frame > 128 && frame <= Image.hard_max_stack_bytes)
        (label ^ ": expected a large bounded spill frame"));
  check_compiled_image ~windows ~label ~expected_frame:frame image;
  let code = Image.code image in
  let capture = Encoder.encode (Encoder.Capture_status status_abi) in
  let capture_offset = if frame = 0 then 0 else 7 in
  require
    (String.length code >= capture_offset + String.length capture
    && String.sub code capture_offset (String.length capture) = capture)
    (label ^ ": status pointer capture must be the first body instruction");
  require
    (byte code (String.length code - 1) = 0xc3)
    (label ^ ": fault-capable image must return through the common epilogue")

let program_status_abi_name = function
  | Program.Windows_x64 -> "windows"
  | Program.System_v_x64 -> "system-v"

let check_program_unwind ?(max_stack_bytes = 4088) ~windows ~status_abi ~live
    expected_frame =
  let image =
    Program.compile ~max_stack_bytes ~status_abi ~max_ir_instructions:4096
      ~max_code_bytes:1048576
      (Test_native_program.pressure_graph live)
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : Program.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
  in
  let label =
    Printf.sprintf "program %s %d live values / %d-byte frame"
      (program_status_abi_name status_abi)
      live expected_frame
  in
  require
    (Program.status_abi image = status_abi)
    (label ^ ": program image lost its mandatory context ABI");
  require
    (Program.frame_bytes image = expected_frame)
    (Printf.sprintf "%s: expected frame %d, got %d" label expected_frame
       (Program.frame_bytes image));
  let code = Program.code image in
  let unwind = Program.windows_unwind_info image in
  (if expected_frame = 0 then
     require (unwind = "") (label ^ ": frameless program has unwind metadata")
   else
     let expected = expected_unwind expected_frame in
     require (unwind = expected) (label ^ ": exact program unwind bytes");
     check_frame_code label expected_frame code;
     if windows then probe_windows_unwind code expected expected_frame);
  let first = Program.windows_unwind_info image in
  let second = Program.windows_unwind_info image in
  if String.length first > 0 then
    Bytes.set (Bytes.unsafe_of_string first) 0 '\xff';
  require
    (Program.windows_unwind_info image = second)
    (label ^ ": program unwind getter must return immutable copies");
  let capture = Encoder.encode (Encoder.Capture_status status_abi) in
  let capture_offset =
    if expected_frame = 0 then 0 else 7 + probe_size expected_frame
  in
  require
    (String.length code >= capture_offset + String.length capture
    && String.sub code capture_offset (String.length capture) = capture)
    (label ^ ": context capture must follow the optional stack allocation");
  if expected_frame > 4088 && windows = (status_abi = Program.Windows_x64) then (
    (match Native_program_execution.execute ~max_steps:4096 image with
    | Ok (Program.Completed { final_value = Some word; _ }) ->
        require
          (word.bits = Int64.of_int (live * (live + 1) / 2))
          "large closed frame result"
    | _ -> failwith "large closed frame did not execute");
    let owners = Array.of_list (Program.windows_unwind_functions image) in
    let identity code =
      ( code,
        owners,
        (if windows then 1 else 2),
        Program.entry_stack_bytes image,
        (0, 0, 0, "") )
    in
    release_probe (retain_probe (identity code));
    let bad = Bytes.of_string code in
    Bytes.set bad 1 '\x84';
    match retain_probe (identity (Bytes.to_string bad)) with
    | retained ->
        release_probe retained;
        failwith "closed frame admitted a missing probe"
    | exception Invalid_argument _ -> ())

let check_callable_program_unwind ~windows ~mode =
  let image =
    compile_program_source ~mode
      "I64 Add(I64 left,I64 right){I64 sum=left+right;return sum;}\n\
       I64 Wrap(I64 value){I64 keep=2;return keep+Add(value,20);}\n\
       Wrap(20);"
  in
  let label = mode_name mode ^ " callable program" in
  require (Program.function_count image = 2) (label ^ ": named function count");
  let code = Program.code image in
  let functions = Program.windows_unwind_functions image in
  require
    (List.length functions = Program.function_count image + 1)
    (label ^ ": unwind table must cover entry and every named owner");
  let previous_end = ref 0 in
  List.iteri
    (fun index (begin_offset, end_offset, unwind) ->
      require
        (begin_offset = !previous_end
        && end_offset > begin_offset
        && end_offset <= String.length code)
        (label ^ ": function ranges must be ordered, disjoint and contiguous");
      require (unwind <> "") (label ^ ": callable owner has no unwind metadata");
      if index = 0 then
        require
          (unwind = Program.windows_unwind_info image)
          (label ^ ": legacy unwind getter must expose the entry owner record");
      previous_end := end_offset)
    functions;
  require
    (!previous_end = String.length code)
    (label ^ ": unwind owner ranges must cover the complete code image");
  let first = Program.windows_unwind_functions image in
  let second = Program.windows_unwind_functions image in
  List.iter
    (fun (_, _, unwind) ->
      if unwind <> "" then Bytes.set (Bytes.unsafe_of_string unwind) 0 '\xff')
    first;
  require
    (second = Program.windows_unwind_functions image)
    (label ^ ": per-owner unwind getter must return immutable copies");
  if windows then probe_windows_program_unwind code (Array.of_list functions)

let check_retained_probe_validation ~windows image =
  let code = Program.code image in
  let owners = Array.of_list (Program.windows_unwind_functions image) in
  let abi = if windows then 1 else 2 in
  let retain code owners =
    retain_probe
      (code, owners, abi, Program.entry_stack_bytes image, (0, 0, 0, ""))
  in
  release_probe (retain code owners);
  let rejected label code owners =
    match retain code owners with
    | handle ->
        release_probe handle;
        failwith (label ^ ": corrupt frame admitted")
    | exception Invalid_argument _ -> ()
  in
  let begin_offset, _, _ = owners.(1) in
  let bad = Bytes.of_string code in
  Bytes.set bad (begin_offset + 5) '\x84';
  rejected "missing first probe" (Bytes.to_string bad) owners;
  let bad = Bytes.of_string code in
  Bytes.blit_string code (begin_offset + 8) bad (begin_offset + 16) 4;
  rejected "repeated page probe" (Bytes.to_string bad) owners;
  let bad_owners = Array.copy owners in
  let first, last, unwind = bad_owners.(1) in
  let bad_unwind = Bytes.of_string unwind in
  Bytes.set bad_unwind 1 '\x0b';
  Bytes.set bad_unwind 4 '\x0b';
  bad_owners.(1) <- (first, last, Bytes.to_string bad_unwind);
  rejected "stale allocation unwind offset" code bad_owners

let check_compact_probe_validation ~windows ~code ~owners ~entry_stack_bytes
    ~owner_index ~probe_offset ~frame =
  require (frame > 65528) "compact mutation fixture needs a large frame";
  let retain code owners =
    retain_probe
      ( code,
        owners,
        (if windows then 1 else 2),
        entry_stack_bytes,
        (0, 0, 0, "") )
  in
  release_probe (retain code owners);
  let rejected label code owners =
    match retain code owners with
    | handle ->
        release_probe handle;
        failwith (label ^ ": corrupt compact frame admitted")
    | exception Invalid_argument _ -> ()
  in
  let mutate label offset value =
    let bad = Bytes.of_string code in
    Bytes.set bad (probe_offset + offset) (Char.chr value);
    rejected label (Bytes.to_string bad) owners
  in
  mutate "altered loop bound" 21 (byte code (probe_offset + 21) lxor 0x10);
  mutate "altered loop target" 26 0xed;
  mutate "reversed loop condition" 25 0x7c;
  mutate "reversed probe direction" 13 0xc0;
  mutate "altered probe stride" 15 0x20;
  mutate "altered probe index" 10 0x0c;
  if frame mod 4096 <> 0 then (
    let bad = Bytes.of_string code in
    Bytes.fill bad (probe_offset + 27) 8 '\x90';
    rejected "missing partial-page probe" (Bytes.to_string bad) owners);
  let corrupt_unwind label change =
    let bad_owners = Array.copy owners in
    let first, last, unwind = bad_owners.(owner_index) in
    let bad = Bytes.of_string unwind in
    change bad;
    bad_owners.(owner_index) <- (first, last, Bytes.to_string bad);
    rejected label code bad_owners
  in
  corrupt_unwind "stale compact allocation endpoint" (fun bad ->
      let endpoint = Char.code (Bytes.get bad 1) - 1 in
      Bytes.set bad 1 (Char.chr endpoint);
      Bytes.set bad 4 (Char.chr endpoint));
  corrupt_unwind "mismatched scaled allocation" (fun bad ->
      Bytes.set bad 6 (Char.chr (Char.code (Bytes.get bad 6) lxor 2)))

let check_encoded_boundary_frames ~windows =
  let encode instructions =
    List.map Encoder.encode instructions |> String.concat ""
  in
  let capture =
    Encoder.Capture_status
      (if windows then Encoder.Windows_x64 else Encoder.System_v_x64)
  in
  require
    (expected_unwind 524280 = string_of_bytes [ 1; 42; 2; 0; 42; 1; 0xff; 0xff ])
    "maximum spill frame must use scaled UWOP_ALLOC_LARGE 0xffff";
  require
    (expected_callable_unwind 524272
    = string_of_bytes [ 1; 46; 3; 0; 46; 1; 0xfe; 0xff; 1; 0x50; 0; 0 ])
    "maximum callable frame must use scaled UWOP_ALLOC_LARGE 0xfffe";
  List.iter
    (fun frame ->
      let stack = Encoder.stack_frame ~bytes:frame |> require_ok Fun.id in
      let code =
        encode
          [
            Encoder.Alloc_stack stack;
            capture;
            Encoder.Mov_imm64 (Encoder.Rax, 42L);
            Encoder.Free_stack stack;
            Encoder.Ret;
          ]
      in
      let unwind = expected_unwind frame in
      let label = Printf.sprintf "encoded %d-byte spill boundary" frame in
      check_frame_code label frame code;
      require
        (byte unwind 1
        = String.length (Encoder.encode (Encoder.Alloc_stack stack)))
        (label ^ ": allocation endpoint differs from encoded prologue");
      if windows then probe_windows_unwind code unwind frame;
      (* Execute only trusted, unmodified ABI-neutral encoder fixtures. The
         malformed variants below remain admission-only tests. *)
      let execution_code =
        encode
          [
            Encoder.Alloc_stack stack;
            Encoder.Mov_imm64 (Encoder.Rax, 42L);
            Encoder.Free_stack stack;
            Encoder.Ret;
          ]
      in
      check_frame_code label frame execution_code;
      require
        (execute_encoded_probe execution_code unwind 0 = (42L, 0L, 0L))
        (label ^ ": native guard growth must return 42 without a fault");
      if frame > 65528 then
        check_compact_probe_validation ~windows ~code
          ~owners:[| (0, String.length code, unwind) |]
          ~entry_stack_bytes:(frame + 8) ~owner_index:0 ~probe_offset:0 ~frame)
    [ 65528; 65544; 524280 ];
  List.iter
    (fun frame ->
      let stack = Encoder.call_frame ~bytes:frame |> require_ok Fun.id in
      let code =
        encode
          [
            Encoder.Push_rbp;
            Encoder.Mov_rbp_rsp;
            Encoder.Alloc_call_frame stack;
            capture;
            Encoder.Mov_imm64 (Encoder.Rax, 42L);
            Encoder.Free_call_frame stack;
            Encoder.Pop_rbp;
            Encoder.Ret;
          ]
      in
      let unwind = expected_callable_unwind frame in
      let owner = (0, String.length code, unwind) in
      let label = Printf.sprintf "encoded %d-byte callable boundary" frame in
      check_callable_frame_code label frame code owner;
      require
        (byte unwind 1
        = 4 + String.length (Encoder.encode (Encoder.Alloc_call_frame stack)))
        (label ^ ": allocation endpoint differs from encoded prologue");
      if windows then probe_windows_program_unwind code [| owner |];
      if frame > 65528 then
        check_compact_probe_validation ~windows ~code ~owners:[| owner |]
          ~entry_stack_bytes:(frame + 16) ~owner_index:0 ~probe_offset:4 ~frame)
    [ 65520; 65536; 65552; 524272 ]

let check_nonvolatile_probe_validation ~windows ~frame ~saves ~code ~unwind =
  let owners = [| (0, String.length code, unwind) |] in
  let retain code owners =
    retain_probe
      (code, owners, (if windows then 1 else 2), frame + 16, (0, 0, 0, ""))
  in
  release_probe (retain code owners);
  let rejected label code owners =
    match retain code owners with
    | handle ->
        release_probe handle;
        failwith (label ^ ": corrupt nonvolatile save admitted")
    | exception Invalid_argument _ -> ()
  in
  let code_mutation label offset value =
    let bad = Bytes.of_string code in
    Bytes.set bad offset (Char.chr value);
    rejected label (Bytes.to_string bad) owners
  in
  let metadata_mutation label change =
    let bad = Bytes.of_string unwind in
    change bad;
    rejected label code [| (0, String.length code, Bytes.to_string bad) |]
  in
  let count = List.length saves in
  let allocation_end = 11 + probe_size frame in
  let restore_begin = String.length code - 9 - (7 * count) in
  code_mutation "missing nonvolatile store" allocation_end 0x90;
  code_mutation "wrong nonvolatile store opcode" (allocation_end + 1) 0x8b;
  code_mutation "wrong nonvolatile store register" (allocation_end + 2)
    (byte code (allocation_end + 2) lxor 8);
  code_mutation "unaligned nonvolatile store slot" (allocation_end + 3)
    (byte code (allocation_end + 3) lxor 1);
  code_mutation "missing nonvolatile restore" restore_begin 0x90;
  code_mutation "wrong nonvolatile restore opcode" (restore_begin + 1) 0x89;
  code_mutation "wrong nonvolatile restore register" (restore_begin + 2)
    (byte code (restore_begin + 2) lxor 8);
  code_mutation "wrong nonvolatile restore slot" (restore_begin + 3)
    (byte code (restore_begin + 3) lxor 8);
  metadata_mutation "full prologue excludes register saves" (fun bad ->
      Bytes.set bad 1 (Char.chr allocation_end));
  metadata_mutation "allocation endpoint includes register saves" (fun bad ->
      Bytes.set bad (4 + (4 * count)) (Bytes.get bad 1));
  metadata_mutation "save endpoint precedes completed store" (fun bad ->
      Bytes.set bad 4 (Char.chr (byte unwind 4 - 1)));
  metadata_mutation "save endpoint follows completed store" (fun bad ->
      Bytes.set bad 4 (Char.chr (byte unwind 4 + 1)));
  metadata_mutation "SAVE_NONVOL describes a volatile register" (fun bad ->
      Bytes.set bad 5 '\x04');
  metadata_mutation "unsupported SAVE_NONVOL_FAR" (fun bad ->
      Bytes.set bad 5 (Char.chr (byte unwind 5 land 0xf0 lor 5)));
  metadata_mutation "save offset addresses caller frame" (fun bad ->
      let outside = frame / 8 in
      Bytes.set bad 6 (Char.chr (outside land 0xff));
      Bytes.set bad 7 (Char.chr ((outside lsr 8) land 0xff)));
  if count > 1 then (
    metadata_mutation "save metadata is not reverse instruction order"
      (fun bad ->
        Bytes.blit_string unwind 4 bad 8 4;
        Bytes.blit_string unwind 8 bad 4 4);
    metadata_mutation "duplicate nonvolatile register" (fun bad ->
        Bytes.set bad 5 (Bytes.get bad 9));
    (* Keep both the machine instructions and metadata mutually consistent:
       the second save must still not overwrite the first register's slot. *)
    let duplicate_code = Bytes.of_string code in
    Bytes.blit_string code (allocation_end + 3) duplicate_code
      (allocation_end + 10) 4;
    Bytes.blit_string code (restore_begin + 3) duplicate_code
      (restore_begin + 10) 4;
    let duplicate_unwind = Bytes.of_string unwind in
    let first_slot = 6 + (4 * (count - 1)) in
    Bytes.blit_string unwind first_slot duplicate_unwind (first_slot - 4) 2;
    rejected "duplicate nonvolatile save slot"
      (Bytes.to_string duplicate_code)
      [| (0, String.length code, Bytes.to_string duplicate_unwind) |]);
  if byte unwind 2 mod 2 <> 0 then
    metadata_mutation "nonzero nonvolatile unwind padding" (fun bad ->
        Bytes.set bad (Bytes.length bad - 1) '\x01')

let check_encoded_nonvolatile_frames ~windows =
  let register = function
    | 12 -> Encoder.R12
    | 13 -> Encoder.R13
    | 14 -> Encoder.R14
    | 15 -> Encoder.R15
    | _ -> invalid_arg "nonvolatile fixture register"
  in
  let capture =
    Encoder.Capture_status
      (if windows then Encoder.Windows_x64 else Encoder.System_v_x64)
  in
  let check frame saves =
    let stack = Encoder.call_frame ~bytes:frame |> require_ok Fun.id in
    let encode = List.map Encoder.encode in
    let access (reg, offset) =
      (register reg, Encoder.frame_slot ~offset |> require_ok Fun.id)
    in
    let save_instructions =
      List.map
        (fun saved ->
          let reg, slot = access saved in
          Encoder.Store_frame (slot, reg))
        saves
    in
    let restores =
      List.map
        (fun saved ->
          let reg, slot = access saved in
          Encoder.Load_frame (reg, slot))
        saves
    in
    let clobbers =
      List.map
        (fun (reg, _) -> Encoder.Mov_imm64 (register reg, Int64.of_int (-reg)))
        saves
    in
    let code =
      encode
        ([
           Encoder.Push_rbp; Encoder.Mov_rbp_rsp; Encoder.Alloc_call_frame stack;
         ]
        @ save_instructions @ [ capture ] @ clobbers
        @ [ Encoder.Mov_imm64 (Encoder.Rax, 42L) ]
        @ restores
        @ [ Encoder.Free_call_frame stack; Encoder.Pop_rbp; Encoder.Ret ])
      |> String.concat ""
    in
    let unwind = expected_callable_unwind ~saves frame in
    let owner = (0, String.length code, unwind) in
    let label =
      Printf.sprintf "encoded %d-byte callable / %d nonvolatile saves" frame
        (List.length saves)
    in
    check_callable_frame_code ~saves label frame code owner;
    if windows then probe_windows_program_unwind code [| owner |];
    check_nonvolatile_probe_validation ~windows ~frame ~saves ~code ~unwind
  in
  require
    (expected_callable_unwind
       ~saves:[ (12, -32); (13, -8); (14, -48); (15, -16) ]
       64
    = string_of_bytes
        [
          1;
          39;
          10;
          0;
          39;
          0xf4;
          6;
          0;
          32;
          0xe4;
          2;
          0;
          25;
          0xd4;
          7;
          0;
          18;
          0xc4;
          4;
          0;
          11;
          0x72;
          1;
          0x50;
        ])
    "four-register unwind literal uses reverse endpoints and final-RSP slots";
  require
    (expected_callable_unwind
       ~saves:[ (12, -32); (13, -8); (14, -48); (15, -16) ]
       524272
    = string_of_bytes
        [
          1;
          74;
          11;
          0;
          74;
          0xf4;
          0xfc;
          0xff;
          67;
          0xe4;
          0xf8;
          0xff;
          60;
          0xd4;
          0xfd;
          0xff;
          53;
          0xc4;
          0xfa;
          0xff;
          46;
          1;
          0xfe;
          0xff;
          1;
          0x50;
          0;
          0;
        ])
    "maximum-frame save slots retain both scaled bytes and unwind padding";
  (* Individual and sparse masks prevent an implicit R12-first/contiguous-bank
     assumption. Alternating offsets expose accidental register/slot swaps. *)
  List.iter (fun reg -> check 64 [ (reg, -24) ]) [ 12; 13; 14; 15 ];
  check 64 [ (12, -48); (15, -8) ];
  List.iter
    (fun frame -> check frame [ (12, -32); (13, -8); (14, -48); (15, -16) ])
    [ 64; 128; 144; 65520; 65536; 65552; 524272 ]

let check_compiled_nonvolatile_unwind ~windows ~mode =
  let image =
    compile_program_source ~mode
      "I64 G(reg R12 I64 a,reg R13 I64 b,reg R14 I64 c,reg R15 I64 \
       d){a+=10;b+=20;c+=30;d+=40;return a+b+c+d;}\n\
       I64 F(reg R12 I64 a,reg R13 I64 b,reg R14 I64 c,reg R15 I64 \
       d){G(1,2,3,4);return a+b+c+d;}F(10,11,12,9);"
  in
  let code = Program.code image in
  let owners = Array.of_list (Program.windows_unwind_functions image) in
  require (Array.length owners = 3) "nonvolatile fixture needs three owners";
  Array.iteri
    (fun index ((first, last, unwind) as owner) ->
      if index <> 0 then (
        let label =
          Printf.sprintf "%s physical-register owner %d" (mode_name mode) index
        in
        let frame = uint32_le code (last - 6) in
        let allocation_end = first + 11 + probe_size frame in
        let saves =
          List.init 4 (fun save ->
              let offset =
                uint32_le code (allocation_end + (7 * save) + 3)
                |> Int32.of_int |> Int32.to_int
              in
              (12 + save, offset))
        in
        require
          (byte unwind 1 = allocation_end - first + 28)
          (label ^ ": full prologue must include all four register saves");
        check_callable_frame_code ~saves label frame code owner))
    owners;
  if windows then probe_windows_program_unwind code owners;
  match Native_program_execution.execute ~max_steps:4096 image with
  | Ok (Program.Completed { final_value = Some word; _ }) ->
      require (word.bits = 42L)
        "nested call failed to preserve physical registers"
  | _ -> failwith "physical-register unwind fixture did not execute"

let check_compiled_compact_callable ~windows ~mode ~elements ~frame =
  let image =
    compile_program_source ~max_stack_bytes:524280 ~max_code_bytes:1048576 ~mode
      (Printf.sprintf
         "I64 F(){I64 a[%d];a[0]=20;a[%d]=22;return a[0]+a[%d];}F();" elements
         (elements - 1) (elements - 1))
  in
  let label =
    Printf.sprintf "%s compact callable / %d-byte frame" (mode_name mode) frame
  in
  require (Program.frame_bytes image = frame) (label ^ ": exact frame size");
  let code = Program.code image in
  let owners = Array.of_list (Program.windows_unwind_functions image) in
  require (Array.length owners = 2) (label ^ ": entry and named owner");
  check_callable_frame_code label frame code owners.(1);
  let first, _, _ = owners.(1) in
  check_compact_probe_validation ~windows ~code ~owners
    ~entry_stack_bytes:(Program.entry_stack_bytes image)
    ~owner_index:1 ~probe_offset:(first + 4) ~frame;
  if windows then probe_windows_program_unwind code owners

let () =
  let windows =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> true
    | Runtime.Linux_x86_64 -> false
    | Runtime.Unsupported ->
        failwith
          "native unwind tests explicitly require Windows x86-64 or Linux \
           x86-64"
  in
  check_encoded_boundary_frames ~windows;
  check_encoded_nonvolatile_frames ~windows;
  List.iter
    (fun mode ->
      check_source_image ~windows ~mode ~literals:7 ~expected_frame:0;
      check_source_image ~windows ~mode ~literals:8 ~expected_frame:8;
      check_source_image ~windows ~mode ~literals:9 ~expected_frame:24;
      check_source_image ~windows ~mode ~literals:23 ~expected_frame:136)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun status_abi ->
      check_fault_image ~windows ~status_abi ~literals:2 `Zero;
      check_fault_image ~windows ~status_abi ~literals:8 `Small;
      check_fault_image ~windows ~status_abi ~literals:24 `Large)
    [ Image.Windows_x64; Image.System_v_x64 ];
  List.iter
    (fun status_abi ->
      check_program_unwind ~windows ~status_abi ~live:2 0;
      check_program_unwind ~windows ~status_abi ~live:6 8;
      check_program_unwind ~windows ~status_abi ~live:23 152;
      check_program_unwind ~windows ~status_abi ~live:516 4088;
      check_program_unwind ~max_stack_bytes:16384 ~windows ~status_abi
        ~live:1030 8200)
    [ Program.Windows_x64; Program.System_v_x64 ];
  List.iter
    (fun mode ->
      check_callable_program_unwind ~windows ~mode;
      check_compiled_nonvolatile_unwind ~windows ~mode)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  (* 518 simultaneous values need all 511 permitted spill slots. This also
     exercises the largest Version 1 large-allocation encoding without crossing
     the one-page probing boundary. *)
  let maximum = pressure_image 518 in
  require
    (Image.status_abi maximum = None)
    "legacy non-fault image must not publish a private status ABI";
  check_compiled_image ~windows
    ~label:"verified IR 518 live values / 4088-byte frame" ~expected_frame:4088
    maximum;
  let large = pressure_image ~max_stack_bytes:16384 1030 in
  check_compiled_image ~windows ~label:"probed expression frame"
    ~expected_frame:8184 large;
  require (Runtime.execute large = Ok 530965L) "large expression frame result";
  List.iter
    (fun mode ->
      let image =
        compile_program_source ~max_stack_bytes:32768 ~mode
          "I64 F(){I64 a[1024];a[0]=20;a[1023]=22;return a[0]+a[1023];}F();"
      in
      require
        (Program.frame_bytes image = 16528)
        "large callable frame exact size";
      check_retained_probe_validation ~windows image;
      if windows then
        probe_windows_program_unwind (Program.code image)
          (Array.of_list (Program.windows_unwind_functions image)))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun mode ->
      check_compiled_compact_callable ~windows ~mode ~elements:4087 ~frame:65536;
      check_compiled_compact_callable ~windows ~mode ~elements:4088 ~frame:65552)
    [ Preprocessor.Jit; Preprocessor.Aot ]
