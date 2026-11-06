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

let compile_program_source ?(max_stack_bytes = 4088) ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-program-unwind.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  Native_program.compile ~max_stack_bytes session ~config ~source
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

let probe_size bytes = if bytes < 4096 then 0 else 8 * ((bytes + 4095) / 4096)

let expected_unwind frame_bytes =
  require
    (frame_bytes >= 8 && frame_bytes <= 65528 && frame_bytes mod 16 = 8)
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
    (fun mode -> check_callable_program_unwind ~windows ~mode)
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
    [ Preprocessor.Jit; Preprocessor.Aot ]
