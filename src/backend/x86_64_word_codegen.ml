module Sequence = Ir.Instruction_sequence
module Graph = Ir.Block_graph
module Opcode = Ir.Opcode
module Type = Sema.Type
module Primitive = Sema.Primitive_type
module Computation = Sema.Integer_computation_class
module Scalar = Ir.Integer_scalar_storage
module Encoder = X86_64_encoder
module Runtime = Ir.Runtime_call_context
module Defaults = Driver.Native_parameter_defaults
module Prepared_default = Ir.Prepared_parameter_default
module Function = Ir.Function_body
module Headers = Sema.Function_type_resolution
module Frame = Sema.Function_frame_layout
module Symbol = Sema.Symbol
module Value_map = Map.Make (Sequence.Value_id)
module Value_set = Set.Make (Sequence.Value_id)
module Instruction_set = Set.Make (Sequence.Instruction_id)
module Block_map = Map.Make (Sequence.Block_id)
module Block_set = Set.Make (Sequence.Block_id)
module Int_map = Map.Make (Int)

type word_type = I64 | U64
type status_abi = Encoder.status_abi = Windows_x64 | System_v_x64
type arithmetic_operation = Divide | Remainder
type error = { code : string; message : string; span : Common.Span.t option }

type arithmetic_fault_site = {
  site : int;
  operation : arithmetic_operation;
  instruction_id : int;
  position : int;
  span : Common.Span.t option;
  signed : bool;
}

type fault_site = arithmetic_fault_site

type program_owner =
  | Entry_owner
  | Function_owner of { function_id : int; function_name : string }

type expression_image = {
  encoded : bytes;
  word_type : word_type;
  ir_count : int;
  machine_count : int;
  peak : int;
  frame_size : int;
  unwind_info : bytes;
  status_abi : status_abi option;
  fault_sites : fault_site list;
}

let hard_ir_limit = 100_000
let hard_max_stack_bytes = 4088
let hard_block_limit = 100_000

let validate_limits ~max_ir_instructions ~max_code_bytes =
  let invalid message =
    Error [ { code = "HCBACK0001"; message; span = None } ]
  in
  if max_ir_instructions <= 0 || max_ir_instructions > hard_ir_limit then
    invalid
      (Printf.sprintf "max_ir_instructions must be between 1 and %d"
         hard_ir_limit)
  else if max_code_bytes <= 0 || max_code_bytes > Encoder.max_code_bytes then
    invalid
      (Printf.sprintf "max_code_bytes must be between 1 and %d"
         Encoder.max_code_bytes)
  else Ok ()

let validate_stack_limit ~max_stack_bytes =
  if max_stack_bytes < 0 || max_stack_bytes > hard_max_stack_bytes then
    Error
      [
        {
          code = "HCBACK0001";
          message =
            Printf.sprintf "max_stack_bytes must be between 0 and %d"
              hard_max_stack_bytes;
          span = None;
        };
      ]
  else Ok ()

let validate_block_limit ~max_blocks =
  if max_blocks <= 0 || max_blocks > hard_block_limit then
    Error
      [
        {
          code = "HCBACK0001";
          message =
            Printf.sprintf "max_blocks must be between 1 and %d"
              hard_block_limit;
          span = None;
        };
      ]
  else Ok ()

exception Rejected of error

let reject ?span code message = raise (Rejected { code; message; span })

let malformed (description : Sequence.description) message =
  reject ?span:description.span "HCBACK0003"
    (Printf.sprintf "%s: %s" (Opcode.to_source_name description.opcode) message)

let unsupported (description : Sequence.description) message =
  reject ?span:description.span "HCBACK0002"
    (Printf.sprintf "%s: %s" (Opcode.to_source_name description.opcode) message)

type scalar_value = { word_type : word_type; byte_size : int }

let scalar_value ?(allow_public = false) type_ =
  let form_allowed =
    match Type.base type_ with
    | Type.Primitive (Type.Internal_storage, _) -> true
    | Type.Primitive (Type.Public_spelling, _) -> allow_public
    | Type.Aggregate _ -> false
  in
  if not form_allowed then None
  else
    Option.map
      (fun scalar ->
        {
          word_type = (if Scalar.is_unsigned scalar then U64 else I64);
          byte_size = Scalar.byte_size scalar;
        })
      (Scalar.of_type type_)

let checked_scalar ?(allow_public = false) description type_ =
  if Type.pointer_depth type_ <> 0 then
    unsupported description "native expressions do not support pointer values";
  match scalar_value ~allow_public type_ with
  | Some scalar -> scalar
  | None ->
      unsupported description
        (if allow_public then
           "native callable programs require nonzero scalar integer values"
         else "native expressions require internal I64 or U64 values")

let checked_word ?(allow_public = false) description type_ =
  let scalar = checked_scalar ~allow_public description type_ in
  if scalar.byte_size <> 8 then
    unsupported description
      (if allow_public then
         "this native operation requires scalar I64 or U64 values"
       else "native expressions require internal I64 or U64 values");
  scalar.word_type

type value = {
  value_id : Sequence.Value_id.t;
  declared_type : Type.t;
  computation_type : Type.t;
  mutable last_use : int;
}

type frame_access = {
  frame_offset : int;
  frame_bytes : int;
  frame_word : word_type;
  initialized_flag_offset : int option;
}

type direct_call = {
  callee_index : int;
  activation_bytes : int;
  argument_stage_slots : int array;
  result_stage_slot : int option;
}

type frame_update =
  | Update_binary of Encoder.binary
  | Update_shift of Encoder.shift
  | Update_division of arithmetic_operation

type operation =
  | Load_immediate of value * int64
  | Apply_unary of Encoder.unary * value * value
  | Apply_binary of Encoder.binary * value * value * value
  | Apply_shift of Encoder.shift * value * value * value
  | Apply_division of
      arithmetic_operation * word_type * fault_site * value * value * value
  | Apply_comparison of Encoder.condition * value * value * value
  | Apply_logical_not of value * value
  | Apply_logical of Encoder.binary * value * value * value
  | Apply_word_view of value * value
  | Frame_tick
  | Load_frame_value of frame_access * value
  | Store_frame_value of frame_access * value * value
  | Update_frame_value of
      frame_access
      * frame_update
      * value option
      * bool
      * value
      * word_type
      * fault_site option
  | Call_start
  | Direct_call of direct_call
  | Call_cleanup
  | Call_end of int * value
  | Call_end_void
  | Return_value of value
  | Return
  | Discard_value of value * word_type
  | Discard_void
  | Jump_to of Sequence.Block_id.t
  | Branch_zero of value * Sequence.Block_id.t
  | Branch_not_zero of value * Sequence.Block_id.t
  | Switch_to of value * value * Sequence.Block_id.t list
  | End_stream

type prepared_instruction = {
  operation : operation;
  span : Common.Span.t option;
  site : int option;
  push_stage : (value * int) option;
}

type kind =
  | Immediate_kind
  | Unary_kind of Encoder.unary
  | Binary_kind of Encoder.binary
  | Shift_kind of [ `Left | `Right ]
  | Division_kind of arithmetic_operation
  | Comparison_kind of Encoder.condition * Encoder.condition
  | Logical_not_kind
  | Logical_kind of Encoder.binary
  | Word_view_kind

let opcode_kind = function
  | Opcode.Ic_imm_i64 -> Some Immediate_kind
  | Opcode.Ic_unary_minus -> Some (Unary_kind Encoder.Neg)
  | Opcode.Ic_com -> Some (Unary_kind Encoder.Not)
  | Opcode.Ic_not -> Some Logical_not_kind
  | Opcode.Ic_and_and -> Some (Logical_kind Encoder.And)
  | Opcode.Ic_or_or -> Some (Logical_kind Encoder.Or)
  | Opcode.Ic_xor_xor -> Some (Logical_kind Encoder.Xor)
  | Opcode.Ic_holyc_typecast -> Some Word_view_kind
  | Opcode.Ic_add -> Some (Binary_kind Encoder.Add)
  | Opcode.Ic_sub -> Some (Binary_kind Encoder.Sub)
  | Opcode.Ic_mul -> Some (Binary_kind Encoder.Imul)
  | Opcode.Ic_and -> Some (Binary_kind Encoder.And)
  | Opcode.Ic_or -> Some (Binary_kind Encoder.Or)
  | Opcode.Ic_xor -> Some (Binary_kind Encoder.Xor)
  | Opcode.Ic_shl -> Some (Shift_kind `Left)
  | Opcode.Ic_shr -> Some (Shift_kind `Right)
  | Opcode.Ic_div -> Some (Division_kind Divide)
  | Opcode.Ic_mod -> Some (Division_kind Remainder)
  | Opcode.Ic_equ_equ -> Some (Comparison_kind (Encoder.E, Encoder.E))
  | Opcode.Ic_not_equ -> Some (Comparison_kind (Encoder.NE, Encoder.NE))
  | Opcode.Ic_less -> Some (Comparison_kind (Encoder.L, Encoder.B))
  | Opcode.Ic_greater_equ -> Some (Comparison_kind (Encoder.GE, Encoder.AE))
  | Opcode.Ic_greater -> Some (Comparison_kind (Encoder.G, Encoder.A))
  | Opcode.Ic_less_equ -> Some (Comparison_kind (Encoder.LE, Encoder.BE))
  | _ -> None

let single_block verified =
  let graph = Ir.X87_stack.graph verified in
  match Graph.blocks graph with
  | [ block ] ->
      if
        not
          (Sequence.Block_id.equal (Graph.block_id block)
             (Graph.block_id (Graph.entry graph)))
      then reject "HCBACK0003" "the sole block must be the graph entry";
      if Graph.successors block <> [] then
        reject "HCBACK0002" "native expressions do not support graph edges";
      block
  | [] -> reject "HCBACK0003" "native expressions require an entry block"
  | _ -> reject "HCBACK0002" "native expressions require exactly one block"

let bounded_length ~max_ir_instructions instructions =
  let rec count length = function
    | [] -> length
    | instruction :: remaining ->
        if length = max_ir_instructions then
          reject ?span:(Sequence.description instruction).span "HCBACK0001"
            (Printf.sprintf
               "IR instruction count exceeds max_ir_instructions (%d)"
               max_ir_instructions);
        count (length + 1) remaining
  in
  count 0 instructions

let validate_identity instruction_ids (description : Sequence.description) =
  if Instruction_set.mem description.instruction_id !instruction_ids then
    malformed description "instruction ID is defined more than once";
  instruction_ids :=
    Instruction_set.add description.instruction_id !instruction_ids;
  match description.span with
  | Some span when span.start < 0 || span.stop < span.start ->
      malformed description "source span is invalid"
  | Some _ | None -> ()

let operand values description position value_id =
  match Value_map.find_opt value_id !values with
  | None ->
      malformed description
        (Printf.sprintf "value %%%d has no earlier definition"
           (Sequence.Value_id.to_int value_id))
  | Some value ->
      value.last_use <- position;
      value

let define values description position (result : Sequence.value_definition)
    declared_type computation_type =
  if Value_map.mem result.value_id !values then
    malformed description
      (Printf.sprintf "value %%%d is defined more than once"
         (Sequence.Value_id.to_int result.value_id));
  let value =
    {
      value_id = result.value_id;
      declared_type;
      computation_type;
      last_use = position;
    }
  in
  values := Value_map.add result.value_id value !values;
  value

let require_type ?(allow_public = false) description expected actual =
  let matches =
    if allow_public then
      Type.equal (Computation.forward expected) (Computation.forward actual)
    else Type.equal expected actual
  in
  if not matches then
    malformed description
      (Printf.sprintf "target type %s does not match required class %s"
         (Sequence.type_name actual)
         (Sequence.type_name expected))

let promoted_type description left right =
  let raw_id type_ =
    match Type.base type_ with
    | Type.Primitive (_, primitive) -> (Primitive.info primitive).raw_id
    | Type.Aggregate _ ->
        malformed description "operand has no integer computation class"
  in
  let selected =
    if raw_id left.computation_type >= raw_id right.computation_type then
      left.computation_type
    else right.computation_type
  in
  Computation.forward selected

let prepare_word_operation ?(allow_public = false) ?(allow_narrow = false)
    ~values ~fault_sites ~position ~site (description : Sequence.description) =
  let operand = operand values description position in
  let define = define values description position in
  let require_type = require_type ~allow_public in
  let checked_value type_ =
    if allow_narrow then
      (checked_scalar ~allow_public description type_).word_type
    else checked_word ~allow_public description type_
  in
  match opcode_kind description.opcode with
  | None -> None
  | Some kind ->
      let shape_error () =
        malformed description
          "invalid operands, result, target type, or payload"
      in
      let shape =
        ( description.operands,
          description.result,
          description.target_type,
          description.payload )
      in
      let operation =
        match (kind, shape) with
        | ( Immediate_kind,
            ([], Some result, Some target_type, Some (Sequence.Integer bits)) )
          ->
            let _ = checked_value target_type in
            let value =
              define result target_type (Computation.forward target_type)
            in
            Load_immediate (value, bits)
        | Unary_kind unary, ([ operand_id ], Some result, Some target_type, None)
          ->
            let input = operand operand_id in
            let computation_type =
              match unary with
              | Encoder.Neg ->
                  ignore (checked_value target_type);
                  let expected = Computation.negate input.computation_type in
                  require_type description expected target_type;
                  Computation.forward target_type
              | Encoder.Not ->
                  let word =
                    checked_word ~allow_public description target_type
                  in
                  if word <> I64 then
                    malformed description "complement must declare internal I64";
                  Computation.forward input.computation_type
            in
            let result = define result target_type computation_type in
            Apply_unary (unary, input, result)
        | Logical_not_kind, ([ operand_id ], Some result, Some target_type, None)
          ->
            let _ = checked_value target_type in
            let input = operand operand_id in
            let computation_type = Computation.forward input.computation_type in
            require_type description computation_type target_type;
            let result = define result target_type computation_type in
            Apply_logical_not (input, result)
        | ( Logical_kind binary,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            if checked_word ~allow_public description target_type <> I64 then
              malformed description
                "binary logical values must declare internal I64";
            let left = operand left_id in
            let right = operand right_id in
            let result =
              define result target_type (Computation.forward target_type)
            in
            Apply_logical (binary, left, right, result)
        | ( Word_view_kind,
            ( [ operand_id ],
              Some result,
              Some target_type,
              Some (Sequence.Integer 0L) ) ) ->
            let _ = checked_word ~allow_public description target_type in
            let input = operand operand_id in
            let result =
              define result target_type (Computation.declared target_type)
            in
            Apply_word_view (input, result)
        | Word_view_kind, ([ _ ], Some _, Some _, Some (Sequence.Integer 1L)) ->
            unsupported description
              "native expressions do not support parenthesized casts"
        | ( Binary_kind binary,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            let _ = checked_value target_type in
            let left = operand left_id in
            let right = operand right_id in
            require_type description
              (promoted_type description left right)
              target_type;
            let result =
              define result target_type (Computation.forward target_type)
            in
            Apply_binary (binary, left, right, result)
        | ( Shift_kind direction,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            let word = checked_value target_type in
            let left = operand left_id in
            let right = operand right_id in
            require_type description
              (promoted_type description left right)
              target_type;
            let shift =
              match (direction, word) with
              | `Left, (I64 | U64) -> Encoder.Shl
              | `Right, I64 -> Encoder.Sar
              | `Right, U64 -> Encoder.Shr
            in
            let result =
              define result target_type (Computation.forward target_type)
            in
            Apply_shift (shift, left, right, result)
        | ( Division_kind arithmetic_operation,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            let word = checked_value target_type in
            let left = operand left_id in
            let right = operand right_id in
            require_type description
              (promoted_type description left right)
              target_type;
            let result =
              define result target_type (Computation.forward target_type)
            in
            let fault_site =
              {
                site;
                operation = arithmetic_operation;
                instruction_id =
                  Sequence.Instruction_id.to_int description.instruction_id;
                position;
                span = description.span;
                signed = word = I64;
              }
            in
            fault_sites := fault_site :: !fault_sites;
            Apply_division
              (arithmetic_operation, word, fault_site, left, right, result)
        | ( Comparison_kind (signed, unsigned),
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            if checked_word ~allow_public description target_type <> I64 then
              malformed description "comparison must declare internal I64";
            let left = operand left_id in
            let right = operand right_id in
            let condition =
              match
                if allow_narrow then
                  (checked_scalar ~allow_public description
                     (promoted_type description left right))
                    .word_type
                else
                  checked_word ~allow_public description
                    (promoted_type description left right)
              with
              | I64 -> signed
              | U64 -> unsigned
            in
            let result =
              define result target_type (Computation.forward target_type)
            in
            Apply_comparison (condition, left, right, result)
        | _ -> shape_error ()
      in
      Some operation

let preflight ~count instructions =
  if count < 3 then
    reject "HCBACK0003"
      "native expressions require a value followed by IC_RETURN_VAL and IC_RET";
  let values = ref Value_map.empty in
  let instruction_ids = ref Instruction_set.empty in
  let prepared = ref [] in
  let fault_sites = ref [] in
  let return_type = ref None in
  List.iteri
    (fun position instruction ->
      let description = Sequence.description instruction in
      validate_identity instruction_ids description;
      if not (Int64.equal description.flags 0L) then
        unsupported description
          "native expressions require zero instruction flags";
      let classified =
        match opcode_kind description.opcode with
        | Some kind -> `Word kind
        | None when description.opcode = Opcode.Ic_return_val -> `Return_value
        | None when description.opcode = Opcode.Ic_ret -> `Return
        | None ->
            unsupported description
              "opcode is outside the native expression subset"
      in
      let operation =
        if position < count - 2 then
          match classified with
          | `Return_value | `Return ->
              malformed description
                "return instructions must be the exact terminal pair"
          | `Word _ ->
              Option.get
                (prepare_word_operation ~values ~fault_sites ~position
                   ~site:(position + 1) description)
        else if position = count - 2 then
          match classified with
          | `Return_value -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, Some target_type, None ->
                  let word = checked_word description target_type in
                  let input = operand values description position operand_id in
                  require_type description input.declared_type target_type;
                  return_type := Some word;
                  Return_value input
              | _ ->
                  malformed description
                    "invalid operands, result, target type, or payload")
          | `Word _ | `Return ->
              malformed description
                "penultimate instruction must be IC_RETURN_VAL"
        else
          match classified with
          | `Return -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, None -> Return
              | _ ->
                  malformed description
                    "invalid operands, result, target type, or payload")
          | `Word _ | `Return_value ->
              malformed description "last instruction must be IC_RET"
      in
      prepared :=
        { operation; span = description.span; site = None; push_stage = None }
        :: !prepared)
    instructions;
  match !return_type with
  | Some return_type -> (List.rev !prepared, return_type, List.rev !fault_sites)
  | None -> reject "HCBACK0003" "native expression has no return value"

type allocation = {
  instructions : Encoder.instruction list;
  code_size : int;
  machine_count : int;
  peak : int;
  frame_size : int;
  unwind_info : bytes;
}

type branch_kind = Unconditional | Equal | Not_equal

type planned_item =
  | Planned_instruction of Encoder.instruction
  | Planned_branch of branch_kind * int
  | Planned_call of int
  | Planned_callee_stack of Encoder.register * int
  | Planned_label of int

type fault_block = { label : int; kind_value : int; site_value : int }
type value_source = Register_source of int | Slot_source of int

let branch_instruction kind displacement =
  match kind with
  | Unconditional -> Encoder.Jump displacement
  | Equal -> Encoder.Jump_equal displacement
  | Not_equal -> Encoder.Jump_not_equal displacement

let planned_size = function
  | Planned_instruction instruction -> Encoder.size instruction
  | Planned_branch (kind, _) -> Encoder.size (branch_instruction kind 0L)
  | Planned_call _ -> Encoder.size (Encoder.Call 0L)
  | Planned_callee_stack (register, _) ->
      Encoder.size (Encoder.Mov_imm64 (register, 0L))
  | Planned_label _ -> 0

let resolve_plan ?(callee_labels = [||]) ?(callee_stack_bytes = [||]) plan =
  let labels = Hashtbl.create (List.length plan) in
  let offset = ref 0 in
  List.iter
    (function
      | Planned_label label ->
          if Hashtbl.mem labels label then
            reject "HCBACK0003" "native expression plan defines a label twice";
          Hashtbl.add labels label !offset
      | item -> offset := !offset + planned_size item)
    plan;
  let code_size = !offset in
  let offset = ref 0 in
  let machine_count = ref 0 in
  let reversed = ref [] in
  List.iter
    (function
      | Planned_label _ -> ()
      | Planned_instruction instruction ->
          reversed := instruction :: !reversed;
          incr machine_count;
          offset := !offset + Encoder.size instruction
      | Planned_branch (kind, label) ->
          let size = Encoder.size (branch_instruction kind 0L) in
          let target =
            match Hashtbl.find_opt labels label with
            | Some target -> target
            | None ->
                reject "HCBACK0003"
                  "native expression branch targets an undefined label"
          in
          let displacement = Int64.of_int (target - (!offset + size)) in
          if
            Int64.compare displacement (-0x80000000L) < 0
            || Int64.compare displacement 0x7fffffffL > 0
          then
            reject "HCBACK0005" "native expression branch exceeds rel32 range";
          reversed := branch_instruction kind displacement :: !reversed;
          incr machine_count;
          offset := !offset + size
      | Planned_call callee_index ->
          if callee_index < 0 || callee_index >= Array.length callee_labels then
            reject "HCBACK0003" "native call targets an unknown function";
          let size = Encoder.size (Encoder.Call 0L) in
          let target_label = callee_labels.(callee_index) in
          let target =
            match Hashtbl.find_opt labels target_label with
            | Some target -> target
            | None ->
                reject "HCBACK0003"
                  "native call targets a function with no machine label"
          in
          let displacement = Int64.of_int (target - (!offset + size)) in
          if
            Int64.compare displacement (-0x80000000L) < 0
            || Int64.compare displacement 0x7fffffffL > 0
          then reject "HCBACK0005" "native call exceeds rel32 range";
          reversed := Encoder.Call displacement :: !reversed;
          incr machine_count;
          offset := !offset + size
      | Planned_callee_stack (register, callee_index) ->
          if callee_index < 0 || callee_index >= Array.length callee_stack_bytes
          then
            reject "HCBACK0003"
              "native call stack accounting names an unknown function";
          let instruction =
            Encoder.Mov_imm64
              (register, Int64.of_int callee_stack_bytes.(callee_index))
          in
          reversed := instruction :: !reversed;
          incr machine_count;
          offset := !offset + Encoder.size instruction)
    plan;
  (List.rev !reversed, code_size, !machine_count)

let align_up value alignment = (value + alignment - 1) / alignment * alignment

let frame_bytes_for_slots slots =
  if slots = 0 then 0 else align_up ((slots * 8) + 8) 16 - 8

let build_windows_unwind_info frame_size =
  if frame_size = 0 then Bytes.create 0
  else
    let info = Bytes.make 8 '\000' in
    let set index value = Bytes.set info index (Char.chr value) in
    (* Conventional Windows x64 UNWIND_INFO version 1. The fixed prologue is
       seven bytes, so its one stack-allocation unwind code ends at offset 7. *)
    set 0 1;
    set 1 7;
    set 3 0;
    set 4 7;
    (if frame_size <= 128 then (
       let opinfo = (frame_size / 8) - 1 in
       set 2 1;
       set 5 ((opinfo lsl 4) lor 2))
     else
       let scaled = frame_size / 8 in
       set 2 2;
       set 5 1;
       set 6 (scaled land 0xff);
       set 7 ((scaled lsr 8) land 0xff));
    info

let build_callable_windows_unwind_info frame_size =
  let has_allocation = frame_size <> 0 in
  let large_allocation = frame_size > 128 in
  let info = Bytes.make (if large_allocation then 12 else 8) '\000' in
  let set index value = Bytes.set info index (Char.chr value) in
  (* Windows x64 UNWIND_INFO version 1. PUSH RBP ends at byte 1 and MOV
     RBP,RSP at byte 4. A callable allocation, when present, is the fixed
     seven-byte SUB RSP,imm32 ending at byte 11. RSP stays fixed after that
     allocation, so the unwind record does not establish a frame register;
     MOV RBP,RSP needs no unwind code. Codes are recorded in reverse prologue
     order and padded to an even slot count. *)
  set 0 1;
  set 1 (if has_allocation then 11 else 4);
  set 3 0;
  (if not has_allocation then (
     set 2 1;
     set 4 1;
     set 5 (5 lsl 4))
   else if frame_size <= 128 then (
     set 2 2;
     set 4 11;
     set 5 ((((frame_size / 8) - 1) lsl 4) lor 2);
     set 6 1;
     set 7 (5 lsl 4))
   else
     let scaled = frame_size / 8 in
     set 2 3;
     set 4 11;
     set 5 1;
     set 6 (scaled land 0xff);
     set 7 ((scaled lsr 8) land 0xff);
     set 8 1;
     set 9 (5 lsl 4));
  info

type label_supply = { mutable next_label : int }

type control_mode =
  | Expression_control of { epilogue_label : int option }
  | Program_control of { block_labels : int Block_map.t; epilogue_label : int }
  | Callable_control of {
      block_labels : int Block_map.t;
      epilogue_label : int;
      is_entry : bool;
      home_slots : int;
    }

type callable_frame_layout = { rbp_bytes : int; fixed_stack_slots : int }

type body_allocation = {
  plan : planned_item list;
  peak : int;
  frame_size : int;
  fault_blocks : fault_block list;
}

let make_label_supply () = { next_label = 0 }

let fresh_label supply =
  let label = supply.next_label in
  supply.next_label <- label + 1;
  label

let encoder_frame span bytes =
  match Encoder.stack_frame ~bytes with
  | Ok frame -> frame
  | Error message -> reject ?span "HCBACK0003" message

let encoder_call_frame span bytes =
  match Encoder.call_frame ~bytes with
  | Ok frame -> frame
  | Error message -> reject ?span "HCBACK0003" message

let encoder_frame_slot span offset =
  match Encoder.frame_slot ~offset with
  | Ok slot -> slot
  | Error message -> reject ?span "HCBACK0003" message

let encoder_scalar_frame_slot span offset =
  match Encoder.scalar_frame_slot ~offset with
  | Ok slot -> slot
  | Error message -> reject ?span "HCBACK0003" message

let narrow_frame_width ?span = function
  | 1 -> Encoder.Frame8
  | 2 -> Encoder.Frame16
  | 4 -> Encoder.Frame32
  | _ -> reject ?span "HCBACK0003" "native scalar frame width is invalid"

let load_frame_scalar span destination access =
  if access.frame_bytes = 8 then
    Encoder.Load_frame (destination, encoder_frame_slot span access.frame_offset)
  else
    Encoder.Load_frame_narrow
      ( destination,
        encoder_scalar_frame_slot span access.frame_offset,
        narrow_frame_width ?span access.frame_bytes,
        if access.frame_word = I64 then Encoder.Sign_extend
        else Encoder.Zero_extend )

let store_frame_scalar span access source =
  if access.frame_bytes = 8 then
    Encoder.Store_frame (encoder_frame_slot span access.frame_offset, source)
  else
    Encoder.Store_frame_narrow
      ( encoder_scalar_frame_slot span access.frame_offset,
        narrow_frame_width ?span access.frame_bytes,
        source )

let allocate_body ?callable_frame ~max_stack_bytes ~reserved_registers ~supply
    ~mode prepared =
  let registers = Array.of_list Encoder.registers in
  let register_index expected =
    let rec find index =
      if index = Array.length registers then
        reject "HCBACK0003" "native register set is missing a fixed register"
      else if registers.(index) = expected then index
      else find (index + 1)
    in
    find 0
  in
  let rax = register_index Encoder.Rax in
  let rcx = register_index Encoder.Rcx in
  let rdx = register_index Encoder.Rdx in
  let r8 = register_index Encoder.R8 in
  let reserved = List.map register_index reserved_registers in
  let owners : value option array = Array.make (Array.length registers) None in
  let slots : value option array = Array.make (hard_max_stack_bytes / 8) None in
  let slot_high_water = ref 0 in
  let planned = ref [] in
  let fault_blocks = ref [] in
  let peak = ref 0 in
  let emit _span instruction =
    planned := Planned_instruction instruction :: !planned
  in
  let emit_branch kind label =
    planned := Planned_branch (kind, label) :: !planned
  in
  let mark label = planned := Planned_label label :: !planned in
  let same_value left right =
    Sequence.Value_id.equal left.value_id right.value_id
  in
  let find_register value =
    let rec find index =
      if index = Array.length owners then None
      else
        match owners.(index) with
        | Some owner when same_value owner value -> Some index
        | Some _ | None -> find (index + 1)
    in
    find 0
  in
  let find_slot value =
    let rec find index =
      if index = !slot_high_water then None
      else
        match slots.(index) with
        | Some owner when same_value owner value -> Some index
        | Some _ | None -> find (index + 1)
    in
    find 0
  in
  let fixed_stack_slots =
    Option.fold ~none:0
      ~some:(fun layout -> layout.fixed_stack_slots)
      callable_frame
  in
  let frame_size_for_spills spill_slots =
    match callable_frame with
    | None -> frame_bytes_for_slots spill_slots
    | Some layout ->
        let bytes =
          layout.rbp_bytes + ((layout.fixed_stack_slots + spill_slots) * 8)
        in
        if bytes = 0 then 0 else align_up bytes 16
  in
  let encoder_slot span index =
    match Encoder.stack_slot ~offset:((fixed_stack_slots + index) * 8) with
    | Ok slot -> slot
    | Error message -> reject ?span "HCBACK0003" message
  in
  let fixed_stack_slot span index =
    match Encoder.stack_slot ~offset:(index * 8) with
    | Ok slot -> slot
    | Error message -> reject ?span "HCBACK0003" message
  in
  let staged_stack_slot span stage =
    match mode with
    | Callable_control { home_slots; _ } ->
        fixed_stack_slot span (home_slots + stage)
    | Expression_control _ | Program_control _ ->
        reject ?span "HCBACK0003"
          "native call staging is outside callable allocation"
  in
  let note_peak ?(temporaries = []) () =
    let occupied = ref 0 in
    Array.iteri
      (fun index owner ->
        if
          Option.is_some owner || List.mem index temporaries
          || List.mem index reserved
        then incr occupied)
      owners;
    peak := max !peak !occupied
  in
  note_peak ();
  let release_where predicate =
    Array.iteri
      (fun index owner ->
        match owner with
        | Some value when predicate value -> owners.(index) <- None
        | Some _ | None -> ())
      owners;
    for index = 0 to !slot_high_water - 1 do
      match slots.(index) with
      | Some value when predicate value -> slots.(index) <- None
      | Some _ | None -> ()
    done
  in
  let release_before position =
    release_where (fun value -> value.last_use < position)
  in
  let release_through position =
    release_where (fun value -> value.last_use <= position)
  in
  let allocate_slot span value =
    let rec find index =
      if index = !slot_high_water then None
      else if Option.is_none slots.(index) then Some index
      else find (index + 1)
    in
    let index =
      match find 0 with
      | Some index -> index
      | None ->
          let candidate_slots = !slot_high_water + 1 in
          let required = frame_size_for_spills candidate_slots in
          if required > max_stack_bytes then
            reject ?span "HCBACK0004"
              (Printf.sprintf
                 "native spill frame requires %d bytes, exceeding \
                  max_stack_bytes (%d)"
                 required max_stack_bytes);
          let index = !slot_high_water in
          slot_high_water := candidate_slots;
          index
    in
    slots.(index) <- Some value;
    (index, encoder_slot span index)
  in
  let spill_register span index =
    match owners.(index) with
    | None -> reject ?span "HCBACK0003" "cannot spill an unowned register"
    | Some value ->
        let _, slot = allocate_slot span value in
        emit span (Encoder.Store_stack (slot, registers.(index)));
        owners.(index) <- None
  in
  let spill_all_registers span =
    Array.iteri
      (fun index owner ->
        if Option.is_some owner && not (List.mem index reserved) then
          spill_register span index)
      owners
  in
  let excluded index extra = List.mem index reserved || List.mem index extra in
  let find_empty ~excluded:extra =
    let rec find index =
      if index = Array.length owners then None
      else if excluded index extra || Option.is_some owners.(index) then
        find (index + 1)
      else Some index
    in
    find 0
  in
  let relocate_register span index ~excluded =
    match owners.(index) with
    | None -> ()
    | Some owner -> (
        match find_empty ~excluded:(index :: excluded) with
        | Some destination ->
            emit span (Encoder.Mov (registers.(destination), registers.(index)));
            owners.(destination) <- Some owner;
            owners.(index) <- None;
            note_peak ()
        | None -> spill_register span index)
  in
  let choose_victim span ~protected ~excluded:extra =
    let best = ref None in
    Array.iteri
      (fun index owner ->
        if not (List.mem index protected || excluded index extra) then
          match owner with
          | None -> ()
          | Some value -> (
              match !best with
              | None -> best := Some (index, value.last_use)
              | Some (_, last_use) when value.last_use > last_use ->
                  best := Some (index, value.last_use)
              | Some _ -> ()))
      owners;
    match !best with
    | Some (index, _) -> index
    | None ->
        reject ?span "HCBACK0004"
          "native expression has no spillable register outside current operands"
  in
  let acquire_empty span ~protected ~excluded:extra =
    let rec find index =
      if index = Array.length owners then None
      else if excluded index extra then find (index + 1)
      else if Option.is_none owners.(index) then Some index
      else find (index + 1)
    in
    match find 0 with
    | Some index -> index
    | None ->
        let index = choose_victim span ~protected ~excluded:extra in
        spill_register span index;
        index
  in
  let acquire_destination span position ~protected ~excluded:extra =
    let rec find index =
      if index = Array.length owners then None
      else if excluded index extra then find (index + 1)
      else
        match owners.(index) with
        | None -> Some index
        | Some owner when owner.last_use = position -> Some index
        | Some _ -> find (index + 1)
    in
    match find 0 with
    | Some index -> index
    | None ->
        let index = choose_victim span ~protected ~excluded:extra in
        spill_register span index;
        index
  in
  let add_unique index indices =
    if List.mem index indices then indices else indices @ [ index ]
  in
  let ensure_inputs span values =
    let protected =
      List.fold_left
        (fun protected value ->
          match find_register value with
          | Some index -> add_unique index protected
          | None -> protected)
        [] values
    in
    let rec load protected reversed = function
      | [] -> (List.rev reversed, protected)
      | value :: remaining -> (
          match find_register value with
          | Some index ->
              load (add_unique index protected) (index :: reversed) remaining
          | None -> (
              match find_slot value with
              | None ->
                  reject ?span "HCBACK0003"
                    "prepared operand has no live register or spill slot"
              | Some slot_index ->
                  let destination =
                    acquire_empty span ~protected ~excluded:[]
                  in
                  let slot = encoder_slot span slot_index in
                  emit span (Encoder.Load_stack (registers.(destination), slot));
                  slots.(slot_index) <- None;
                  owners.(destination) <- Some value;
                  note_peak ();
                  load
                    (add_unique destination protected)
                    (destination :: reversed) remaining))
    in
    load protected [] values
  in
  let copy_value_to span value destination =
    match find_register value with
    | Some source ->
        if source <> destination then
          emit span (Encoder.Mov (registers.(destination), registers.(source)))
    | None -> (
        match find_slot value with
        | Some slot_index ->
            emit span
              (Encoder.Load_stack
                 (registers.(destination), encoder_slot span slot_index))
        | None ->
            reject ?span "HCBACK0003"
              "prepared shift operand has no live register or spill slot")
  in
  let copy_count_to_rcx span ~position ~shared_with_left count =
    match find_register count with
    | Some source when source = rcx -> ()
    | Some source -> emit span (Encoder.Mov (Encoder.Rcx, registers.(source)))
    | None -> (
        match find_slot count with
        | Some slot_index ->
            emit span
              (Encoder.Load_stack (Encoder.Rcx, encoder_slot span slot_index));
            if count.last_use = position && not shared_with_left then
              slots.(slot_index) <- None
        | None ->
            reject ?span "HCBACK0003"
              "prepared shift count has no live register or spill slot")
  in
  let locate_value span value =
    match find_register value with
    | Some index -> Register_source index
    | None -> (
        match find_slot value with
        | Some index -> Slot_source index
        | None ->
            reject ?span "HCBACK0003"
              "prepared arithmetic operand has no live register or spill slot")
  in
  let copy_source_to span source destination =
    match source with
    | Register_source source ->
        if source <> destination then
          emit span (Encoder.Mov (registers.(destination), registers.(source)))
    | Slot_source slot ->
        emit span
          (Encoder.Load_stack (registers.(destination), encoder_slot span slot))
  in
  let move_owner span source destination =
    match (owners.(source), owners.(destination)) with
    | Some owner, None ->
        emit span (Encoder.Mov (registers.(destination), registers.(source)));
        owners.(destination) <- Some owner;
        owners.(source) <- None;
        note_peak ()
    | Some _, Some _ ->
        reject ?span "HCBACK0003" "fixed-register staging target is occupied"
    | None, _ ->
        reject ?span "HCBACK0003" "fixed-register staging source is unowned"
  in
  let is_fixed index = index = rax || index = rcx || index = rdx in
  let prepare_division_operands span position left right =
    let move_dying_to_target target value =
      if Option.is_none owners.(target) && value.last_use = position then
        match find_register value with
        | Some source when source <> target && not (List.mem source reserved) ->
            move_owner span source target;
            true
        | Some _ | None -> false
      else false
    in
    let move_dying_safe_to_rdx value =
      if Option.is_none owners.(rdx) && value.last_use = position then
        match find_register value with
        | Some source
          when (not (is_fixed source)) && not (List.mem source reserved) ->
            move_owner span source rdx;
            true
        | Some _ | None -> false
      else false
    in
    let rec stage_available () =
      let changed = ref false in
      if move_dying_to_target rax left then changed := true;
      if (not (same_value left right)) && move_dying_to_target rcx right then
        changed := true;
      if Option.is_none owners.(rdx) then
        if move_dying_safe_to_rdx left then changed := true
        else if (not (same_value left right)) && move_dying_safe_to_rdx right
        then changed := true;
      if !changed then stage_available ()
    in
    stage_available ();
    let fixed = [ rax; rcx; rdx ] in
    List.iter
      (fun index ->
        stage_available ();
        match owners.(index) with
        | Some owner
          when (same_value owner left || same_value owner right)
               && owner.last_use = position -> ()
        | Some _ ->
            relocate_register span index ~excluded:fixed;
            stage_available ()
        | None -> ())
      fixed;
    let left_source = locate_value span left in
    let right_source = locate_value span right in
    (if same_value left right then (
       copy_source_to span left_source rax;
       emit span (Encoder.Mov (Encoder.Rcx, Encoder.Rax)))
     else
       match (left_source, right_source) with
       | Register_source left, Register_source right
         when left = rcx && right = rax ->
           emit span (Encoder.Mov (Encoder.Rdx, Encoder.Rcx));
           emit span (Encoder.Mov (Encoder.Rcx, Encoder.Rax));
           emit span (Encoder.Mov (Encoder.Rax, Encoder.Rdx))
       | _, Register_source right when right = rax ->
           copy_source_to span right_source rcx;
           copy_source_to span left_source rax
       | Register_source left, _ when left = rcx ->
           copy_source_to span left_source rax;
           copy_source_to span right_source rcx
       | _ ->
           copy_source_to span left_source rax;
           copy_source_to span right_source rcx);
    owners.(rax) <- None;
    owners.(rcx) <- None;
    owners.(rdx) <- None;
    note_peak ~temporaries:[ rax; rcx; rdx ] ()
  in
  let active_push = ref None in
  let assign position destination value =
    owners.(destination) <- Some value;
    note_peak ();
    match !active_push with
    | Some pushed when same_value pushed value ->
        release_where (fun candidate ->
            candidate.last_use <= position && not (same_value candidate value))
    | Some _ | None -> release_through position
  in
  let target_label target =
    match mode with
    | Program_control { block_labels; _ } | Callable_control { block_labels; _ }
      -> (
        match Block_map.find_opt target block_labels with
        | Some label -> label
        | None ->
            reject "HCBACK0003"
              "native program control target has no machine label")
    | Expression_control _ ->
        reject "HCBACK0003" "native expression contains program control"
  in
  List.iteri
    (fun position (instruction : prepared_instruction) ->
      release_before position;
      active_push := Option.map fst instruction.push_stage;
      let emit = emit instruction.span in
      (match mode with
      | Expression_control _ -> ()
      | Program_control _ | Callable_control _ ->
          let site =
            match instruction.site with
            | Some site -> site
            | None ->
                reject ?span:instruction.span "HCBACK0003"
                  "native program instruction has no dense execution site"
          in
          let step_fault = fresh_label supply in
          fault_blocks :=
            { label = step_fault; kind_value = 3; site_value = site }
            :: !fault_blocks;
          emit (Encoder.Test Encoder.R10);
          emit_branch Equal step_fault;
          emit (Encoder.Dec Encoder.R10));
      (match instruction.operation with
      | Load_immediate (result, bits) ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          emit (Encoder.Mov_imm64 (registers.(destination), bits));
          assign position destination result
      | Apply_unary (unary, input, result) ->
          let inputs, protected = ensure_inputs instruction.span [ input ] in
          let source = List.hd inputs in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          if destination <> source then
            emit (Encoder.Mov (registers.(destination), registers.(source)));
          emit (Encoder.Unary (unary, registers.(destination)));
          assign position destination result
      | Apply_binary (binary, left, right, result) ->
          let inputs, protected =
            ensure_inputs instruction.span [ left; right ]
          in
          let left, right =
            match inputs with
            | [ left; right ] -> (left, right)
            | _ -> assert false
          in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          let target = registers.(destination) in
          if destination = left then
            emit (Encoder.Binary (binary, target, registers.(right)))
          else if destination = right then (
            emit (Encoder.Binary (binary, target, registers.(left)));
            if binary = Encoder.Sub then
              emit (Encoder.Unary (Encoder.Neg, target)))
          else (
            emit (Encoder.Mov (target, registers.(left)));
            emit (Encoder.Binary (binary, target, registers.(right))));
          assign position destination result
      | Apply_shift (shift, left, count, result) ->
          let count_register = find_register count in
          (match owners.(rcx) with
          | None -> ()
          | Some owner when same_value owner count -> ()
          | Some owner when same_value owner left && owner.last_use = position
            -> ()
          | Some _ ->
              relocate_register instruction.span rcx
                ~excluded:(Option.to_list count_register));
          let left_register = find_register left in
          let count_register = find_register count in
          let destination_before_count =
            match left_register with
            | Some source when source = rcx && not (same_value left count) ->
                let excluded =
                  rcx
                  :: Option.fold ~none:[]
                       ~some:(fun index -> [ index ])
                       count_register
                in
                let destination =
                  acquire_destination instruction.span position
                    ~protected:[ rcx ] ~excluded
                in
                copy_value_to instruction.span left destination;
                owners.(rcx) <- None;
                Some destination
            | Some _ | None -> None
          in
          copy_count_to_rcx instruction.span ~position
            ~shared_with_left:(same_value left count) count;
          let destination =
            match destination_before_count with
            | Some destination -> destination
            | None ->
                let protected =
                  match find_register left with
                  | Some index -> [ index ]
                  | None -> []
                in
                acquire_destination instruction.span position ~protected
                  ~excluded:[ rcx ]
          in
          if Option.is_none destination_before_count then
            copy_value_to instruction.span left destination;
          note_peak ~temporaries:[ rcx; destination ] ();
          emit (Encoder.Shift_cl (shift, registers.(destination)));
          assign position destination result
      | Apply_division (arithmetic_operation, word, site, left, right, result)
        ->
          prepare_division_operands instruction.span position left right;
          let zero_label = fresh_label supply in
          fault_blocks :=
            { label = zero_label; kind_value = 1; site_value = site.site }
            :: !fault_blocks;
          emit (Encoder.Test Encoder.Rcx);
          emit_branch Equal zero_label;
          (match word with
          | U64 ->
              emit Encoder.Zero_edx;
              emit Encoder.Div_rcx
          | I64 ->
              let overflow_label = fresh_label supply in
              let safe_label = fresh_label supply in
              fault_blocks :=
                {
                  label = overflow_label;
                  kind_value = 2;
                  site_value = site.site;
                }
                :: !fault_blocks;
              emit (Encoder.Mov_imm64 (Encoder.Rdx, Int64.min_int));
              emit (Encoder.Cmp (Encoder.Rax, Encoder.Rdx));
              emit_branch Not_equal safe_label;
              emit (Encoder.Cmp_imm8 (Encoder.Rcx, -1));
              emit_branch Equal overflow_label;
              mark safe_label;
              emit Encoder.Cqo;
              emit Encoder.Idiv_rcx);
          assign position
            (match arithmetic_operation with
            | Divide -> rax
            | Remainder -> rdx)
            result
      | Apply_comparison (condition, left, right, result) ->
          let inputs, protected =
            ensure_inputs instruction.span [ left; right ]
          in
          let left, right =
            match inputs with
            | [ left; right ] -> (left, right)
            | _ -> assert false
          in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          let target = registers.(destination) in
          emit (Encoder.Cmp (registers.(left), registers.(right)));
          emit (Encoder.Setcc (condition, target));
          emit (Encoder.Movzx8 (target, target));
          assign position destination result
      | Apply_logical_not (input, result) ->
          let inputs, protected = ensure_inputs instruction.span [ input ] in
          let source = List.hd inputs in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          let target = registers.(destination) in
          emit (Encoder.Test registers.(source));
          emit (Encoder.Setcc (Encoder.E, target));
          emit (Encoder.Movzx8 (target, target));
          assign position destination result
      | Apply_logical (binary, left, right, result) ->
          let inputs, protected =
            ensure_inputs instruction.span [ left; right ]
          in
          let left, right =
            match inputs with
            | [ left; right ] -> (left, right)
            | _ -> assert false
          in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          let scratch =
            acquire_destination instruction.span position ~protected
              ~excluded:[ destination ]
          in
          let first, second =
            if destination = right then (right, left) else (left, right)
          in
          let truth source target =
            emit (Encoder.Test registers.(source));
            emit (Encoder.Setcc (Encoder.NE, registers.(target)));
            emit (Encoder.Movzx8 (registers.(target), registers.(target)))
          in
          note_peak ~temporaries:[ destination; scratch ] ();
          truth first destination;
          truth second scratch;
          emit
            (Encoder.Binary
               (binary, registers.(destination), registers.(scratch)));
          assign position destination result
      | Apply_word_view (input, result) ->
          let inputs, protected = ensure_inputs instruction.span [ input ] in
          let source = List.hd inputs in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          if destination <> source then
            emit (Encoder.Mov (registers.(destination), registers.(source)));
          assign position destination result
      | Frame_tick -> release_through position
      | Load_frame_value (access, result) ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          let target = registers.(destination) in
          Option.iter
            (fun flag_offset ->
              let site = Option.get instruction.site in
              let uninitialized = fresh_label supply in
              fault_blocks :=
                { label = uninitialized; kind_value = 7; site_value = site }
                :: !fault_blocks;
              emit
                (Encoder.Load_frame
                   (target, encoder_frame_slot instruction.span flag_offset));
              emit (Encoder.Test target);
              emit_branch Equal uninitialized)
            access.initialized_flag_offset;
          emit (load_frame_scalar instruction.span target access);
          assign position destination result
      | Store_frame_value (access, input, result) ->
          let inputs, protected = ensure_inputs instruction.span [ input ] in
          let source = List.hd inputs in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          if destination <> source then
            emit (Encoder.Mov (registers.(destination), registers.(source)));
          emit
            (store_frame_scalar instruction.span access registers.(destination));
          Option.iter
            (fun flag_offset ->
              let scratch =
                acquire_empty instruction.span ~protected:[ destination ]
                  ~excluded:[]
              in
              emit (Encoder.Mov_imm64 (registers.(scratch), 1L));
              emit
                (Encoder.Store_frame
                   ( encoder_frame_slot instruction.span flag_offset,
                     registers.(scratch) ));
              owners.(scratch) <- None)
            access.initialized_flag_offset;
          assign position destination result
      | Update_frame_value
          (access, update, input, old_result, result, word, arithmetic_site) ->
          spill_all_registers instruction.span;
          Option.iter
            (fun flag_offset ->
              let site = Option.get instruction.site in
              let uninitialized = fresh_label supply in
              fault_blocks :=
                { label = uninitialized; kind_value = 7; site_value = site }
                :: !fault_blocks;
              emit
                (Encoder.Load_frame
                   (Encoder.Rax, encoder_frame_slot instruction.span flag_offset));
              emit (Encoder.Test Encoder.Rax);
              emit_branch Equal uninitialized)
            access.initialized_flag_offset;
          emit (load_frame_scalar instruction.span Encoder.Rax access);
          if old_result then emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          (match input with
          | Some input -> copy_value_to instruction.span input rcx
          | None -> emit (Encoder.Mov_imm64 (Encoder.Rcx, 1L)));
          let computed_index =
            match update with
            | Update_binary binary ->
                emit (Encoder.Binary (binary, Encoder.Rax, Encoder.Rcx));
                rax
            | Update_shift shift ->
                emit (Encoder.Shift_cl (shift, Encoder.Rax));
                rax
            | Update_division arithmetic_operation -> (
                let site = Option.get arithmetic_site in
                let zero_label = fresh_label supply in
                fault_blocks :=
                  { label = zero_label; kind_value = 1; site_value = site.site }
                  :: !fault_blocks;
                emit (Encoder.Test Encoder.Rcx);
                emit_branch Equal zero_label;
                (match word with
                | U64 ->
                    emit Encoder.Zero_edx;
                    emit Encoder.Div_rcx
                | I64 ->
                    let overflow_label = fresh_label supply in
                    let safe_label = fresh_label supply in
                    fault_blocks :=
                      {
                        label = overflow_label;
                        kind_value = 2;
                        site_value = site.site;
                      }
                      :: !fault_blocks;
                    emit (Encoder.Mov_imm64 (Encoder.Rdx, Int64.min_int));
                    emit (Encoder.Cmp (Encoder.Rax, Encoder.Rdx));
                    emit_branch Not_equal safe_label;
                    emit (Encoder.Cmp_imm8 (Encoder.Rcx, -1));
                    emit_branch Equal overflow_label;
                    mark safe_label;
                    emit Encoder.Cqo;
                    emit Encoder.Idiv_rcx);
                match arithmetic_operation with
                | Divide -> rax
                | Remainder -> rdx)
          in
          emit
            (store_frame_scalar instruction.span access
               registers.(computed_index));
          if (not old_result) && Option.is_none input && access.frame_bytes < 8
          then
            emit
              (load_frame_scalar instruction.span registers.(computed_index)
                 access);
          note_peak
            ~temporaries:
              (if old_result then [ rax; rcx; rdx; r8 ] else [ rax; rcx; rdx ])
            ();
          assign position (if old_result then r8 else computed_index) result
      | Call_start | Call_cleanup -> release_through position
      | Direct_call call ->
          let site = Option.get instruction.site in
          let epilogue =
            match mode with
            | Callable_control { epilogue_label; _ } -> epilogue_label
            | Expression_control _ | Program_control _ ->
                reject ?span:instruction.span "HCBACK0003"
                  "direct native call is outside callable allocation"
          in
          spill_all_registers instruction.span;
          Array.iteri
            (fun fixed_index stage ->
              emit
                (Encoder.Load_stack
                   (Encoder.Rax, staged_stack_slot instruction.span stage));
              emit
                (Encoder.Store_stack
                   (fixed_stack_slot instruction.span fixed_index, Encoder.Rax)))
            call.argument_stage_slots;
          let depth_fault = fresh_label supply in
          let frame_fault = fresh_label supply in
          let stack_fault = fresh_label supply in
          fault_blocks :=
            { label = depth_fault; kind_value = 4; site_value = site }
            :: { label = frame_fault; kind_value = 5; site_value = site }
            :: { label = stack_fault; kind_value = 6; site_value = site }
            :: !fault_blocks;
          (* Guard all three activation quotas before mutating any of them. *)
          emit (Encoder.Load_context (Encoder.Rax, 56));
          emit (Encoder.Test Encoder.Rax);
          emit_branch Equal depth_fault;
          emit (Encoder.Load_context (Encoder.Rax, 48));
          emit
            (Encoder.Mov_imm64 (Encoder.Rcx, Int64.of_int call.activation_bytes));
          emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Setcc (Encoder.B, Encoder.Rdx));
          emit (Encoder.Movzx8 (Encoder.Rdx, Encoder.Rdx));
          emit (Encoder.Test Encoder.Rdx);
          emit_branch Not_equal frame_fault;
          emit (Encoder.Load_context (Encoder.Rax, 64));
          planned :=
            Planned_callee_stack (Encoder.Rcx, call.callee_index) :: !planned;
          emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Setcc (Encoder.B, Encoder.Rdx));
          emit (Encoder.Movzx8 (Encoder.Rdx, Encoder.Rdx));
          emit (Encoder.Test Encoder.Rdx);
          emit_branch Not_equal stack_fault;
          (* Reserve the callee's semantic depth/frame and physical stack. *)
          emit (Encoder.Load_context (Encoder.Rax, 56));
          emit (Encoder.Dec Encoder.Rax);
          emit (Encoder.Store_context (56, Encoder.Rax));
          emit (Encoder.Load_context (Encoder.Rax, 48));
          emit
            (Encoder.Mov_imm64 (Encoder.Rcx, Int64.of_int call.activation_bytes));
          emit (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Store_context (48, Encoder.Rax));
          emit (Encoder.Load_context (Encoder.Rax, 64));
          planned :=
            Planned_callee_stack (Encoder.Rcx, call.callee_index) :: !planned;
          emit (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Store_context (64, Encoder.Rax));
          planned := Planned_call call.callee_index :: !planned;
          (* Save RAX before quota restoration/status inspection clobbers it. *)
          Option.iter
            (fun stage ->
              emit
                (Encoder.Store_stack
                   (staged_stack_slot instruction.span stage, Encoder.Rax)))
            call.result_stage_slot;
          emit (Encoder.Load_context (Encoder.Rax, 56));
          emit (Encoder.Mov_imm64 (Encoder.Rcx, 1L));
          emit (Encoder.Binary (Encoder.Add, Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Store_context (56, Encoder.Rax));
          emit (Encoder.Load_context (Encoder.Rax, 48));
          emit
            (Encoder.Mov_imm64 (Encoder.Rcx, Int64.of_int call.activation_bytes));
          emit (Encoder.Binary (Encoder.Add, Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Store_context (48, Encoder.Rax));
          emit (Encoder.Load_context (Encoder.Rax, 64));
          planned :=
            Planned_callee_stack (Encoder.Rcx, call.callee_index) :: !planned;
          emit (Encoder.Binary (Encoder.Add, Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Store_context (64, Encoder.Rax));
          emit (Encoder.Load_context (Encoder.Rax, 0));
          emit (Encoder.Test Encoder.Rax);
          emit_branch Not_equal epilogue;
          release_through position
      | Call_end (stage, result) ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          emit
            (Encoder.Load_stack
               ( registers.(destination),
                 staged_stack_slot instruction.span stage ));
          assign position destination result
      | Call_end_void -> release_through position
      | Return_value input ->
          let inputs, _ = ensure_inputs instruction.span [ input ] in
          let source = List.hd inputs in
          if registers.(source) <> Encoder.Rax then (
            if Option.is_some owners.(0) then
              reject ?span:instruction.span "HCBACK0003"
                "return register still owns a different live value";
            emit (Encoder.Mov (Encoder.Rax, registers.(source)));
            owners.(0) <- Some input;
            note_peak ());
          release_through position
      | Return -> (
          match mode with
          | Expression_control { epilogue_label = Some label } ->
              emit_branch Unconditional label
          | Expression_control { epilogue_label = None } -> emit Encoder.Ret
          | Callable_control { epilogue_label; is_entry = false; _ } ->
              emit_branch Unconditional epilogue_label
          | Program_control _ | Callable_control { is_entry = true; _ } ->
              reject ?span:instruction.span "HCBACK0003"
                "native program contains expression return")
      | Discard_value (input, _) -> (
          match mode with
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program discard"
          | Program_control _ | Callable_control { is_entry = true; _ } ->
              let site = Option.get instruction.site in
              let inputs, _ = ensure_inputs instruction.span [ input ] in
              let source = List.hd inputs in
              emit (Encoder.Store_context (40, registers.(source)));
              emit (Encoder.Store_context_imm (32, site));
              release_through position
          | Callable_control { is_entry = false; _ } -> release_through position
          )
      | Discard_void -> (
          match mode with
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains no-value discard"
          | Program_control _ | Callable_control { is_entry = true; _ } ->
              emit (Encoder.Store_context_imm (40, 0));
              emit (Encoder.Store_context_imm (32, 0));
              release_through position
          | Callable_control { is_entry = false; _ } -> release_through position
          )
      | Jump_to target -> (
          match mode with
          | Program_control _ | Callable_control _ ->
              emit_branch Unconditional (target_label target)
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program jump")
      | Branch_zero (input, target) | Branch_not_zero (input, target) -> (
          match mode with
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program branch"
          | Program_control _ | Callable_control _ ->
              let inputs, _ = ensure_inputs instruction.span [ input ] in
              let source = List.hd inputs in
              emit (Encoder.Test registers.(source));
              emit_branch
                (match instruction.operation with
                | Branch_zero _ -> Equal
                | Branch_not_zero _ -> Not_equal
                | _ -> assert false)
                (target_label target);
              release_through position)
      | Switch_to (adjusted, range, targets) -> (
          match mode with
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program switch"
          | Program_control _ | Callable_control _ -> (
              let inputs, _ =
                ensure_inputs instruction.span [ adjusted; range ]
              in
              let adjusted_register = List.nth inputs 0 in
              let range_register = List.nth inputs 1 in
              let adjusted_register = registers.(adjusted_register) in
              let range_register = registers.(range_register) in
              match targets with
              | default_target :: (_ :: _ as entries) ->
                  (* Match the pinned bounded switch: compare the already-adjusted
                     selector as an unsigned word, then dispatch only in-range
                     values. Reuse the dead range register for the bounds bit. *)
                  emit (Encoder.Cmp (adjusted_register, range_register));
                  emit (Encoder.Setcc (Encoder.AE, range_register));
                  emit (Encoder.Movzx8 (range_register, range_register));
                  emit (Encoder.Test range_register);
                  emit_branch Not_equal (target_label default_target);
                  let rec emit_entries = function
                    | [] -> assert false
                    | [ target ] ->
                        emit_branch Unconditional (target_label target)
                    | target :: rest ->
                        emit (Encoder.Test adjusted_register);
                        emit_branch Equal (target_label target);
                        emit (Encoder.Dec adjusted_register);
                        emit_entries rest
                  in
                  emit_entries entries;
                  release_through position
              | [] | [ _ ] ->
                  reject ?span:instruction.span "HCBACK0003"
                    "native IC_SWITCH has no bounded target table"))
      | End_stream -> (
          match mode with
          | Program_control { epilogue_label; _ } ->
              emit_branch Unconditional epilogue_label
          | Callable_control { epilogue_label; is_entry = true; _ } ->
              emit_branch Unconditional epilogue_label
          | Callable_control { is_entry = false; _ } ->
              reject ?span:instruction.span "HCBACK0003"
                "native source function contains stream end"
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains stream end"));
      Option.iter
        (fun (value, stage) ->
          let inputs, _ = ensure_inputs instruction.span [ value ] in
          let source = List.hd inputs in
          emit
            (Encoder.Store_stack
               (staged_stack_slot instruction.span stage, registers.(source))))
        instruction.push_stage;
      release_through position;
      active_push := None)
    prepared;
  {
    plan = List.rev !planned;
    peak = !peak;
    frame_size = frame_size_for_spills !slot_high_water;
    fault_blocks = List.rev !fault_blocks;
  }

let allocate ~max_stack_bytes ~status_abi prepared =
  let supply = make_label_supply () in
  let epilogue_label = Option.map (fun _ -> fresh_label supply) status_abi in
  let reserved_registers =
    match status_abi with
    | None -> []
    | Some _ -> [ Encoder.R11 ]
  in
  let body =
    allocate_body ~max_stack_bytes ~reserved_registers ~supply
      ~mode:(Expression_control { epilogue_label })
      prepared
  in
  let frame_size = body.frame_size in
  let plan =
    match (status_abi, epilogue_label) with
    | None, None -> (
        if frame_size = 0 then body.plan
        else
          let frame = encoder_frame None frame_size in
          match List.rev body.plan with
          | Planned_instruction Encoder.Ret :: reversed_prefix ->
              Planned_instruction (Encoder.Alloc_stack frame)
              :: (List.rev reversed_prefix
                 @ [
                     Planned_instruction (Encoder.Free_stack frame);
                     Planned_instruction Encoder.Ret;
                   ])
          | _ ->
              reject "HCBACK0003"
                "native expression allocation lost terminal RET")
    | Some abi, Some epilogue ->
        let frame =
          if frame_size = 0 then None else Some (encoder_frame None frame_size)
        in
        let prefix =
          match frame with
          | None -> [ Planned_instruction (Encoder.Capture_status abi) ]
          | Some frame ->
              [
                Planned_instruction (Encoder.Alloc_stack frame);
                Planned_instruction (Encoder.Capture_status abi);
              ]
        in
        let fault_plan =
          body.fault_blocks
          |> List.concat_map (fun block ->
              [
                Planned_label block.label;
                Planned_instruction (Encoder.Store_status_site block.site_value);
                Planned_instruction (Encoder.Store_status_kind block.kind_value);
                Planned_branch (Unconditional, epilogue);
              ])
        in
        let suffix =
          Planned_label epilogue
          ::
          (match frame with
          | None -> [ Planned_instruction Encoder.Ret ]
          | Some frame ->
              [
                Planned_instruction (Encoder.Free_stack frame);
                Planned_instruction Encoder.Ret;
              ])
        in
        prefix @ body.plan @ fault_plan @ suffix
    | Some _, None | None, Some _ ->
        reject "HCBACK0003"
          "native expression fault epilogue state is inconsistent"
  in
  let instructions, code_size, machine_count = resolve_plan plan in
  {
    instructions;
    code_size;
    machine_count;
    peak = body.peak;
    frame_size;
    unwind_info = build_windows_unwind_info frame_size;
  }

type program_site = {
  site : int;
  owner : program_owner;
  block_id : int;
  instruction_id : int;
  position : int;
  global_position : int;
  span : Common.Span.t option;
  arithmetic : (arithmetic_operation * bool) option;
  value_type : word_type option;
  call_site : bool;
  uninitialized_read_site : bool;
}

type program_image = {
  encoded : bytes;
  ir_count : int;
  machine_count : int;
  peak : int;
  frame_size : int;
  unwind_info : bytes;
  unwind_functions : (int * int * bytes) list;
  status_abi : status_abi;
  block_count : int;
  function_count : int;
  entry_stack_bytes : int;
  sites : program_site list;
}

type prepared_program_block = {
  program_block_id : Sequence.Block_id.t;
  program_instructions : prepared_instruction list;
  program_fallthrough : Sequence.Block_id.t option;
}

let same_block_ids left right =
  List.length left = List.length right
  && List.for_all2 Sequence.Block_id.equal left right

let ordered_unique_block_ids targets =
  let _, reversed =
    List.fold_left
      (fun (seen, result) target ->
        if Block_set.mem target seen then (seen, result)
        else (Block_set.add target seen, target :: result))
      (Block_set.empty, []) targets
  in
  List.rev reversed

let target_from_description (description : Sequence.description) =
  match description.payload with
  | Some (Sequence.Block target) -> target
  | _ -> malformed description "control instruction requires one block target"

let checked_target graph description =
  let target = target_from_description description in
  if Option.is_none (Graph.find_block graph target) then
    malformed description "control instruction targets an unknown block";
  target

let checked_switch_shape graph description =
  let shape =
    match Sequence.bounded_switch_shape description with
    | Ok shape -> shape
    | Error message -> malformed description message
  in
  List.iter
    (fun target ->
      if Option.is_none (Graph.find_block graph target) then
        malformed description "IC_SWITCH targets an unknown block")
    shape.targets;
  shape

let preflight_program graph =
  let blocks = Graph.blocks graph in
  let instruction_ids = ref Instruction_set.empty in
  let global_position = ref 0 in
  let sites_rev = ref [] in
  let prepared_blocks_rev = ref [] in
  let rec visit_blocks = function
    | [] -> ()
    | block :: remaining ->
        let block_id = Graph.block_id block in
        let values = ref Value_map.empty in
        let arithmetic_sites = ref [] in
        let prepared_rev = ref [] in
        let descriptions = Graph.instructions block |> Sequence.instructions in
        List.iteri
          (fun position instruction ->
            let description = Sequence.description instruction in
            validate_identity instruction_ids description;
            let site = !global_position + 1 in
            let operation, value_type =
              if Option.is_some (opcode_kind description.opcode) then (
                if description.flags <> 0L then
                  malformed description
                    "native program word producers require zero instruction \
                     flags";
                match
                  prepare_word_operation ~values ~fault_sites:arithmetic_sites
                    ~position ~site description
                with
                | Some operation -> (operation, None)
                | None -> assert false)
              else
                match description.opcode with
                | Opcode.Ic_end_exp -> (
                    if description.flags <> 0x200L then
                      malformed description
                        "IC_END_EXP requires flags=0x000000200";
                    match
                      ( description.operands,
                        description.result,
                        description.target_type,
                        description.payload )
                    with
                    | [ operand_id ], None, None, None ->
                        let input =
                          operand values description position operand_id
                        in
                        let word =
                          checked_word description input.declared_type
                        in
                        (Discard_value (input, word), Some word)
                    | _ ->
                        malformed description
                          "invalid operands, result, target type, or payload")
                | Opcode.Ic_jmp -> (
                    if description.flags <> 0L then
                      malformed description
                        "IC_JMP requires zero instruction flags";
                    match
                      ( description.operands,
                        description.result,
                        description.target_type )
                    with
                    | [], None, None ->
                        (Jump_to (checked_target graph description), None)
                    | _ ->
                        malformed description
                          "invalid operands, result, target type, or payload")
                | Opcode.Ic_br_zero | Opcode.Ic_br_not_zero -> (
                    if description.flags <> 0L then
                      malformed description
                        "native program branches require zero instruction flags";
                    match
                      ( description.operands,
                        description.result,
                        description.target_type )
                    with
                    | [ operand_id ], None, None ->
                        let input =
                          operand values description position operand_id
                        in
                        ignore (checked_word description input.declared_type);
                        let target = checked_target graph description in
                        ( (if description.opcode = Opcode.Ic_br_zero then
                             Branch_zero (input, target)
                           else Branch_not_zero (input, target)),
                          None )
                    | _ ->
                        malformed description
                          "invalid operands, result, target type, or payload")
                | Opcode.Ic_switch ->
                    let shape = checked_switch_shape graph description in
                    let adjusted =
                      operand values description position shape.adjusted_index
                    in
                    let range =
                      operand values description position shape.range_value
                    in
                    if checked_word description adjusted.declared_type <> I64
                    then
                      malformed description
                        "IC_SWITCH adjusted index must be internal I64";
                    if checked_word description range.declared_type <> I64 then
                      malformed description
                        "IC_SWITCH range must be internal I64";
                    (Switch_to (adjusted, range, shape.targets), None)
                | Opcode.Ic_end -> (
                    if description.flags <> 0L then
                      malformed description
                        "IC_END requires zero instruction flags";
                    match
                      ( description.operands,
                        description.result,
                        description.target_type,
                        description.payload )
                    with
                    | [], None, None, None -> (End_stream, None)
                    | _ ->
                        malformed description
                          "invalid operands, result, target type, or payload")
                | _ ->
                    unsupported description
                      "opcode is outside the native program subset"
            in
            let arithmetic =
              match operation with
              | Apply_division (operation, word, _, _, _, _) ->
                  Some (operation, word = I64)
              | _ -> None
            in
            sites_rev :=
              {
                site;
                owner = Entry_owner;
                block_id = Sequence.Block_id.to_int block_id;
                instruction_id =
                  Sequence.Instruction_id.to_int description.instruction_id;
                position;
                global_position = !global_position;
                span = description.span;
                arithmetic;
                value_type;
                call_site = false;
                uninitialized_read_site = false;
              }
              :: !sites_rev;
            prepared_rev :=
              {
                operation;
                span = description.span;
                site = Some site;
                push_stage = None;
              }
              :: !prepared_rev;
            incr global_position)
          descriptions;
        let next =
          match remaining with
          | next :: _ -> Some (Graph.block_id next)
          | [] -> None
        in
        let final_operation =
          match !prepared_rev with
          | instruction :: _ -> Some instruction.operation
          | [] -> None
        in
        let explicit_target =
          match final_operation with
          | Some
              ( Jump_to target
              | Branch_zero (_, target)
              | Branch_not_zero (_, target) ) -> Some target
          | Some End_stream | Some _ | None -> None
        in
        let switch_targets =
          match final_operation with
          | Some (Switch_to (_, _, targets)) ->
              Some (ordered_unique_block_ids targets)
          | _ -> None
        in
        let fallthrough =
          match final_operation with
          | Some (Jump_to _ | Switch_to _ | End_stream) -> None
          | Some (Branch_zero _ | Branch_not_zero _) | Some _ | None -> next
        in
        let expected_successors =
          match switch_targets with
          | Some targets -> targets
          | None -> (
              match (explicit_target, fallthrough) with
              | None, None -> []
              | Some target, None -> [ target ]
              | None, Some target -> [ target ]
              | Some target, Some next when Sequence.Block_id.equal target next
                -> [ target ]
              | Some target, Some next -> [ target; next ])
        in
        if not (same_block_ids (Graph.successors block) expected_successors)
        then
          reject "HCBACK0003"
            (Printf.sprintf
               "native program block ^b%d has inconsistent checked successors"
               (Sequence.Block_id.to_int block_id));
        prepared_blocks_rev :=
          {
            program_block_id = block_id;
            program_instructions = List.rev !prepared_rev;
            program_fallthrough = fallthrough;
          }
          :: !prepared_blocks_rev;
        visit_blocks remaining
  in
  visit_blocks blocks;
  (List.rev !prepared_blocks_rev, List.rev !sites_rev, !global_position)

type callable_slot = {
  slot_type : Type.t;
  slot_word : word_type;
  access : frame_access;
}

type callable_return_kind =
  | Callable_word_return of scalar_value
  | Callable_void_return

type callable_function_info = {
  definition : Ir.Integer_interpreter.function_definition;
  owner : program_owner;
  parameter_count : int;
  parameter_types : Type.t array;
  return_kind : callable_return_kind;
  rbp_bytes : int;
  activation_bytes : int;
  frame_slots : callable_slot Int_map.t;
  init_flag_offsets : int list;
}

type frame_term =
  | Frame_base of Type.t
  | Frame_offset of Type.t * int
  | Frame_address of callable_slot

type callable_call_phase = Collecting | Needs_cleanup | Needs_end

type callable_call_scope = {
  call : Runtime.call;
  callee_index : int;
  activation_bytes : int;
  stage_base : int;
  result_stage : int option;
  argument_stages : int array;
  pushed : bool array;
  mutable phase : callable_call_phase;
}

type prepared_callable_body = {
  callable_blocks : prepared_program_block list;
  callable_sites : program_site list;
  callable_ir_count : int;
  callable_block_count : int;
  callable_home_slots : int;
  callable_stage_slots : int;
}

type allocated_callable_body = {
  body_plan : planned_item list;
  body_frame_size : int;
  body_peak : int;
  body_unwind : bytes;
}

let int_of_frame_size ?span value =
  if
    Int64.compare value 0L < 0 || Int64.compare value (Int64.of_int max_int) > 0
  then
    reject ?span "HCBACK0004" "native function frame size exceeds host limits";
  Int64.to_int value

let int_of_frame_displacement ?span value =
  if
    Int64.compare value (Int64.of_int min_int) < 0
    || Int64.compare value (Int64.of_int max_int) > 0
  then
    reject ?span "HCBACK0003"
      "native frame displacement exceeds the checked host integer range";
  Int64.to_int value

let source_scalar ?span label type_ =
  match Scalar.of_type type_ with
  | Some scalar ->
      {
        word_type = (if Scalar.is_unsigned scalar then U64 else I64);
        byte_size = Scalar.byte_size scalar;
      }
  | None ->
      reject ?span "HCBACK0002" (label ^ " must be a nonzero scalar integer")

let source_return_kind ?span type_ =
  if Type.pointer_depth type_ = 0 then
    match Type.base type_ with
    | Type.Primitive (_, Primitive.U0) -> Callable_void_return
    | _ ->
        Callable_word_return
          (source_scalar ?span "native source function return type" type_)
  else
    Callable_word_return
      (source_scalar ?span "native source function return type" type_)

let callable_frame_update opcode word =
  match opcode with
  | Opcode.Ic_add_equ -> Some (Update_binary Encoder.Add, false, true)
  | Opcode.Ic_sub_equ -> Some (Update_binary Encoder.Sub, false, true)
  | Opcode.Ic_mul_equ -> Some (Update_binary Encoder.Imul, false, true)
  | Opcode.Ic_div_equ -> Some (Update_division Divide, false, true)
  | Opcode.Ic_mod_equ -> Some (Update_division Remainder, false, true)
  | Opcode.Ic_and_equ -> Some (Update_binary Encoder.And, false, true)
  | Opcode.Ic_or_equ -> Some (Update_binary Encoder.Or, false, true)
  | Opcode.Ic_xor_equ -> Some (Update_binary Encoder.Xor, false, true)
  | Opcode.Ic_shl_equ -> Some (Update_shift Encoder.Shl, false, true)
  | Opcode.Ic_shr_equ ->
      Some
        ( Update_shift (if word = I64 then Encoder.Sar else Encoder.Shr),
          false,
          true )
  | Opcode.Ic_pp_ -> Some (Update_binary Encoder.Add, false, false)
  | Opcode.Ic_mm_ -> Some (Update_binary Encoder.Sub, false, false)
  | Opcode.Ic__pp -> Some (Update_binary Encoder.Add, true, false)
  | Opcode.Ic__mm -> Some (Update_binary Encoder.Sub, true, false)
  | _ -> None

let prepare_callable_function ~max_stack_bytes
    (definition : Ir.Integer_interpreter.function_definition) =
  let body = definition.body in
  let frame = definition.frame in
  let span = Function.span body in
  if not (Function.definition_matches_frame body frame) then
    reject ?span "HCBACK0003"
      "native source function body does not match its checked frame";
  if Option.is_none (Function.definition_declaration body) then
    reject ?span "HCBACK0003"
      "native source functions require their original checked definition owner";
  let allowed_flags =
    Sema.Function_flag.Stored.to_mask Sema.Function_flag.Stored.Ret1
  in
  if
    Int64.logand (Function.stored_flags body) (Int64.lognot allowed_flags) <> 0L
  then
    reject ?span "HCBACK0002"
      "native source functions do not admit explicit calling-convention flags";
  let return_kind = source_return_kind ?span (Function.return_type body) in
  let local_frame_bytes =
    int_of_frame_size ?span (Frame.function_frame_size frame)
  in
  if local_frame_bytes > max_stack_bytes then
    reject ?span "HCBACK0004"
      (Printf.sprintf
         "native source function semantic frame requires %d bytes, exceeding \
          max_stack_bytes (%d)"
         local_frame_bytes max_stack_bytes);
  let slots = ref Int_map.empty in
  let slot_ranges = ref Int_map.empty in
  let add_slot offset bytes slot =
    let limit = offset + bytes in
    let before, exact, after = Int_map.split offset !slot_ranges in
    let overlaps_before =
      match Int_map.max_binding_opt before with
      | Some (other_offset, other_bytes) -> other_offset + other_bytes > offset
      | None -> false
    in
    let overlaps_after =
      match Int_map.min_binding_opt after with
      | Some (other_offset, _) -> other_offset < limit
      | None -> false
    in
    if
      Option.is_some exact || Int_map.mem offset !slots || overlaps_before
      || overlaps_after
    then
      reject ?span "HCBACK0003"
        "native source function has overlapping checked frame slots";
    slots := Int_map.add offset slot !slots;
    slot_ranges := Int_map.add offset bytes !slot_ranges
  in
  let parameters = Function.parameters body in
  List.iteri
    (fun index member ->
      if Function.member_position member <> index then
        reject
          ?span:(Function.member_span member)
          "HCBACK0003"
          "native source function parameter positions are inconsistent";
      let type_ = Function.member_type member in
      let scalar =
        source_scalar ?span:(Function.member_span member) "parameter" type_
      in
      let location =
        match Frame.find_location frame (Function.member_symbol member) with
        | Some location -> location
        | None ->
            reject
              ?span:(Function.member_span member)
              "HCBACK0003"
              "native source parameter has no checked frame location"
      in
      let register_ok =
        match Frame.location_register_selection location with
        | Sema.Register_request.Unspecified | Sema.Register_request.Disabled ->
            true
        | Sema.Register_request.Allocatable | Sema.Register_request.Explicit _
          -> false
      in
      if
        Frame.location_kind location <> Frame.Named_parameter
        || (not register_ok)
        || Frame.location_declarator_shape location <> Frame.Object
        || Frame.location_value_shape location <> Frame.Scalar
        || Frame.location_dimensions location <> []
        || (not (Type.equal (Frame.location_checked_type location) type_))
        || Frame.location_element_size location <> Int64.of_int scalar.byte_size
        || Frame.location_allocated_size location <> 8L
        || Frame.location_alignment location <> 8
      then
        reject
          ?span:(Function.member_span member)
          "HCBACK0002"
          "native source parameters require fixed scalar integer ABI slots";
      let slot =
        match Frame.location_frame_slot location with
        | Some slot -> slot
        | None ->
            reject
              ?span:(Function.member_span member)
              "HCBACK0003" "native source parameter has no physical frame slot"
      in
      let expected = 16 + (8 * index) in
      let actual =
        int_of_frame_displacement
          ?span:(Function.member_span member)
          (Frame.frame_slot_displacement slot)
      in
      if actual <> expected || Frame.frame_slot_size slot <> 8L then
        reject
          ?span:(Function.member_span member)
          "HCBACK0003"
          "native source parameter displacement disagrees with the private ABI";
      add_slot actual 8
        {
          slot_type = type_;
          slot_word = scalar.word_type;
          access =
            {
              frame_offset = actual;
              frame_bytes = scalar.byte_size;
              frame_word = scalar.word_type;
              initialized_flag_offset = None;
            };
        })
    parameters;
  let locals = Function.locals body in
  let init_flag_offsets_rev = ref [] in
  List.iteri
    (fun index member ->
      let type_ = Function.member_type member in
      let scalar =
        source_scalar
          ?span:(Function.member_span member)
          "automatic local" type_
      in
      let location =
        match Frame.find_location frame (Function.member_symbol member) with
        | Some location -> location
        | None ->
            reject
              ?span:(Function.member_span member)
              "HCBACK0003"
              "native automatic local has no checked frame location"
      in
      let register_ok =
        match Frame.location_register_selection location with
        | Sema.Register_request.Unspecified | Sema.Register_request.Disabled ->
            true
        | Sema.Register_request.Allocatable | Sema.Register_request.Explicit _
          -> false
      in
      if
        Frame.location_kind location <> Frame.Automatic_local
        || (not register_ok)
        || Frame.location_declarator_shape location <> Frame.Object
        || Frame.location_value_shape location <> Frame.Scalar
        || Frame.location_dimensions location <> []
        || (not (Type.equal (Frame.location_checked_type location) type_))
        || Frame.location_element_size location <> Int64.of_int scalar.byte_size
        || Frame.location_allocated_size location
           <> Int64.of_int scalar.byte_size
        || Frame.location_alignment location <> scalar.byte_size
      then
        reject
          ?span:(Function.member_span member)
          "HCBACK0002"
          "native source locals require automatic scalar integer stack storage";
      let slot =
        match Frame.location_frame_slot location with
        | Some slot -> slot
        | None ->
            reject
              ?span:(Function.member_span member)
              "HCBACK0003" "native automatic local has no physical frame slot"
      in
      let actual =
        int_of_frame_displacement
          ?span:(Function.member_span member)
          (Frame.frame_slot_displacement slot)
      in
      if
        actual > -scalar.byte_size
        || actual < -local_frame_bytes
        || actual mod scalar.byte_size <> 0
        || Frame.frame_slot_size slot <> Int64.of_int scalar.byte_size
      then
        reject
          ?span:(Function.member_span member)
          "HCBACK0003" "native automatic local has an invalid RBP displacement";
      let flag_offset = -(local_frame_bytes + (8 * (index + 1))) in
      init_flag_offsets_rev := flag_offset :: !init_flag_offsets_rev;
      add_slot actual scalar.byte_size
        {
          slot_type = type_;
          slot_word = scalar.word_type;
          access =
            {
              frame_offset = actual;
              frame_bytes = scalar.byte_size;
              frame_word = scalar.word_type;
              initialized_flag_offset = Some flag_offset;
            };
        })
    locals;
  let rbp_bytes = local_frame_bytes + (8 * List.length locals) in
  if rbp_bytes > max_stack_bytes then
    reject ?span "HCBACK0004"
      (Printf.sprintf
         "native source function frame plus initialization state requires %d \
          bytes, exceeding max_stack_bytes (%d)"
         rbp_bytes max_stack_bytes);
  let function_id = Function.function_id body |> Function.Function_id.to_int in
  {
    definition;
    owner =
      Function_owner
        { function_id; function_name = Symbol.name (Function.symbol body) };
    parameter_count = List.length parameters;
    parameter_types =
      Array.of_list
        (List.map (fun member -> Function.member_type member) parameters);
    return_kind;
    rbp_bytes;
    activation_bytes = local_frame_bytes + (8 * List.length parameters);
    frame_slots = !slots;
    init_flag_offsets = List.rev !init_flag_offsets_rev;
  }

let validate_callable_parameter_defaults ~parameter_defaults functions =
  let header_has_default header =
    header |> Sema.Function_type_resolution.function_signature
    |> Sema.Function_type_resolution.signature_parameters
    |> List.exists (fun parameter ->
        Option.is_some
          (Sema.Function_type_resolution.parameter_default parameter))
  in
  Array.iter
    (fun info ->
      let body = info.definition.body in
      let span = Function.span body in
      let declaration =
        match Function.definition_declaration body with
        | Some declaration -> declaration
        | None ->
            reject ?span "HCBACK0003"
              "native source functions require their original checked \
               definition owner"
      in
      let selected_header =
        Sema.Function_resolution.resolved_declaration_header declaration
      in
      let source_header =
        declaration |> Sema.Function_resolution.resolved_declaration_site
        |> Sema.Function_resolution.declaration_site_function
      in
      if
        (header_has_default selected_header || header_has_default source_header)
        && Option.is_none parameter_defaults
      then
        reject ?span "HCBACK0002"
          "native source functions do not admit parameter defaults")
    functions

let callable_callee_index functions call =
  let symbol = Runtime.symbol call in
  let declaration = Runtime.declaration call in
  let rec find index =
    if index = Array.length functions then None
    else
      let body = functions.(index).definition.body in
      if
        Function.callable_symbol body == symbol
        &&
        match Function.definition_declaration body with
        | Some candidate -> candidate == declaration
        | None -> false
      then Some index
      else find (index + 1)
  in
  find 0

let validate_callable_returns graph return_kind =
  match return_kind with
  | Callable_void_return -> ()
  | Callable_word_return _ ->
      let blocks =
        List.fold_left
          (fun index block -> Block_map.add (Graph.block_id block) block index)
          Block_map.empty (Graph.blocks graph)
      in
      let pending = Queue.create () in
      let visited = ref Block_map.empty in
      let enqueue block_id has_value =
        let bit = if has_value then 2 else 1 in
        let previous =
          Option.value (Block_map.find_opt block_id !visited) ~default:0
        in
        if previous land bit = 0 then (
          visited := Block_map.add block_id (previous lor bit) !visited;
          Queue.add (block_id, has_value) pending)
      in
      enqueue (Graph.block_id (Graph.entry graph)) false;
      while not (Queue.is_empty pending) do
        let block_id, incoming = Queue.take pending in
        let block = Block_map.find block_id blocks in
        let has_value = ref incoming in
        List.iter
          (fun instruction ->
            let description = Sequence.description instruction in
            match description.opcode with
            | Opcode.Ic_return_val -> has_value := true
            | Opcode.Ic_ret ->
                if not !has_value then
                  reject ?span:description.span "HCBACK0002"
                    "native source function has a reachable return without its \
                     own word value"
            | Opcode.Ic_jmp -> ()
            | _ ->
                (* Only empty blocks and unconditional transfers may carry RAX from
               the checked source RETURN_VAL to its shared return block. Every
               other IR instruction requires a subsequent value return. *)
                has_value := false)
          (Graph.instructions block |> Sequence.instructions);
        List.iter
          (fun successor -> enqueue successor !has_value)
          (Graph.successors block)
      done

let preflight_callable_graph ~runtime_calls ~parameter_defaults ~functions
    ~runtime_owner ~owner ~frame_slots ~expected_return ~is_entry ~next_site
    graph =
  let blocks = Graph.blocks graph in
  let instruction_ids = ref Instruction_set.empty in
  let sites_rev = ref [] in
  let home_slots = ref 0 in
  let stage_high_water = ref 0 in
  let ir_count = ref 0 in
  let define_frame frame_values values void_values description result term =
    if
      Value_map.mem result.Sequence.value_id !frame_values
      || Value_map.mem result.Sequence.value_id !values
      || Value_set.mem result.Sequence.value_id !void_values
    then
      malformed description
        (Printf.sprintf "value %%%d is defined more than once"
           (Sequence.Value_id.to_int result.Sequence.value_id));
    frame_values := Value_map.add result.Sequence.value_id term !frame_values
  in
  let frame_operand frame_values description id =
    match Value_map.find_opt id !frame_values with
    | Some value -> value
    | None ->
        malformed description
          (Printf.sprintf "frame value %%%d has no earlier definition"
             (Sequence.Value_id.to_int id))
  in
  let visit_block block next =
    let block_id = Graph.block_id block in
    let values = ref Value_map.empty in
    let frame_values = ref Value_map.empty in
    let void_values = ref Value_set.empty in
    let arithmetic_sites = ref [] in
    let prepared_rev = ref [] in
    let calls = ref [] in
    let stage_cursor = ref 0 in
    let descriptions = Graph.instructions block |> Sequence.instructions in
    let terminal_position = List.length descriptions - 1 in
    List.iteri
      (fun position instruction ->
        let raw = Sequence.description instruction in
        validate_identity instruction_ids raw;
        let site = !next_site + 1 in
        incr next_site;
        incr ir_count;
        let pushes = Int64.logand raw.flags 0x2000L <> 0L in
        let description =
          if pushes then
            { raw with flags = Int64.logand raw.flags (Int64.lognot 0x2000L) }
          else raw
        in
        Option.iter
          (fun result ->
            if
              Value_map.mem result.Sequence.value_id !values
              || Value_map.mem result.Sequence.value_id !frame_values
              || Value_set.mem result.Sequence.value_id !void_values
            then
              malformed description
                (Printf.sprintf "value %%%d is defined more than once"
                   (Sequence.Value_id.to_int result.Sequence.value_id)))
          description.result;
        let operation, value_type =
          match description.opcode with
          | Opcode.Ic_call_start ->
              if
                description.flags <> 0L || description.operands <> []
                || Option.is_some description.result
                || Option.is_some description.target_type
              then malformed description "invalid IC_CALL_START shape";
              let call =
                match
                  Runtime.find_start runtime_calls ~owner:runtime_owner
                    description.instruction_id
                with
                | Some call -> call
                | None ->
                    malformed description
                      "IC_CALL_START has no sealed runtime-call site"
              in
              if
                Option.is_some (Runtime.provider call)
                || Option.is_some (Runtime.retained_function call)
                || Runtime.call_opcode call <> Opcode.Ic_call
              then
                unsupported description
                  "native callable programs require fixed direct source calls";
              let callee_index =
                match callable_callee_index functions call with
                | Some index -> index
                | None ->
                    malformed description
                      "direct call has no exact callable source definition"
              in
              let callee = functions.(callee_index) in
              if
                not
                  (Type.equal (Runtime.return_type call)
                     (Function.return_type callee.definition.body))
              then
                malformed description
                  "direct call return type disagrees with its source definition";
              let arguments = Runtime.arguments call in
              if List.length arguments <> callee.parameter_count then
                malformed description
                  "direct call fixed argument count disagrees with its source \
                   definition";
              let seen = Array.make callee.parameter_count false in
              List.iter
                (fun argument ->
                  match Runtime.argument_role argument with
                  | Runtime.Fixed index
                    when index >= 0
                         && index < callee.parameter_count
                         && not seen.(index) ->
                      seen.(index) <- true;
                      if
                        not
                          (Type.equal
                             (Runtime.argument_target_type argument)
                             callee.parameter_types.(index))
                      then
                        malformed description
                          "direct call argument target type disagrees with its \
                           parameter"
                  | Runtime.Fixed _
                  | Runtime.Variadic_count
                  | Runtime.Variadic _ ->
                      unsupported description
                        "native callable programs require non-variadic fixed \
                         arguments")
                arguments;
              if not (Array.for_all Fun.id seen) then
                malformed description
                  "direct call does not cover every fixed parameter";
              let stage_base = !stage_cursor in
              let result_stage =
                match callee.return_kind with
                | Callable_word_return _ ->
                    Some (stage_base + callee.parameter_count)
                | Callable_void_return -> None
              in
              stage_cursor :=
                stage_base + callee.parameter_count
                + if Option.is_some result_stage then 1 else 0;
              stage_high_water := max !stage_high_water !stage_cursor;
              home_slots := max !home_slots callee.parameter_count;
              let scope =
                {
                  call;
                  callee_index;
                  activation_bytes = callee.activation_bytes;
                  stage_base;
                  result_stage;
                  argument_stages =
                    Array.init callee.parameter_count (fun index ->
                        stage_base + index);
                  pushed = Array.make callee.parameter_count false;
                  phase = Collecting;
                }
              in
              calls := scope :: !calls;
              (Call_start, None)
          | Opcode.Ic_call -> (
              match !calls with
              | scope :: _ when scope.phase = Collecting ->
                  if
                    description.flags <> 0L || description.operands <> []
                    || Option.is_some description.result
                    || (not
                          (Sequence.Instruction_id.equal
                             description.instruction_id
                             (Runtime.call_instruction scope.call)))
                    || not (Array.for_all Fun.id scope.pushed)
                  then
                    malformed description "invalid fixed direct IC_CALL shape";
                  (match description.payload with
                  | Some (Sequence.Symbol symbol)
                    when symbol == Runtime.symbol scope.call -> ()
                  | _ ->
                      malformed description
                        "IC_CALL target symbol is inconsistent");
                  (match description.target_type with
                  | Some type_
                    when Type.equal type_ (Runtime.return_type scope.call) -> ()
                  | _ ->
                      malformed description
                        "IC_CALL target type is inconsistent");
                  scope.phase <- Needs_cleanup;
                  ( Direct_call
                      {
                        callee_index = scope.callee_index;
                        activation_bytes = scope.activation_bytes;
                        argument_stage_slots = Array.copy scope.argument_stages;
                        result_stage_slot = scope.result_stage;
                      },
                    None )
              | _ ->
                  malformed description
                    "IC_CALL is outside a collecting call scope")
          | Opcode.Ic_add_rsp | Opcode.Ic_add_rsp1 -> (
              match !calls with
              | scope :: _ when scope.phase = Needs_cleanup ->
                  if
                    description.flags <> 0L || description.operands <> []
                    || Option.is_some description.result
                    || description.opcode <> Runtime.cleanup_opcode scope.call
                    || not
                         (Sequence.Instruction_id.equal
                            description.instruction_id
                            (Runtime.cleanup_instruction scope.call))
                  then malformed description "invalid direct-call cleanup shape";
                  (match description.payload with
                  | Some (Sequence.Integer bytes)
                    when bytes = Runtime.cleanup_bytes scope.call -> ()
                  | _ ->
                      malformed description
                        "direct-call cleanup byte count is inconsistent");
                  (match description.target_type with
                  | Some type_
                    when Type.equal type_ (Runtime.return_type scope.call) -> ()
                  | _ ->
                      malformed description
                        "direct-call cleanup target type is inconsistent");
                  scope.phase <- Needs_end;
                  (Call_cleanup, None)
              | _ ->
                  malformed description
                    "call cleanup is outside a reached direct call")
          | Opcode.Ic_call_end -> (
              match !calls with
              | scope :: remaining when scope.phase = Needs_end -> (
                  if
                    description.flags <> 0L || description.operands <> []
                    || not
                         (Sequence.Instruction_id.equal
                            description.instruction_id (Runtime.last scope.call))
                  then malformed description "invalid IC_CALL_END shape";
                  (match description.payload with
                  | Some (Sequence.Symbol symbol)
                    when symbol == Runtime.symbol scope.call -> ()
                  | _ ->
                      malformed description
                        "IC_CALL_END target symbol is inconsistent");
                  let result, target_type =
                    match (description.result, description.target_type) with
                    | Some result, Some target_type
                      when Sequence.Value_id.equal result.value_id
                             (Runtime.result_value scope.call)
                           && Type.equal target_type
                                (Runtime.return_type scope.call) ->
                        (result, target_type)
                    | _ ->
                        malformed description
                          "IC_CALL_END result is inconsistent"
                  in
                  calls := remaining;
                  stage_cursor := scope.stage_base;
                  let callee = functions.(scope.callee_index) in
                  match callee.return_kind with
                  | Callable_word_return scalar ->
                      let actual =
                        checked_scalar ~allow_public:true description
                          target_type
                      in
                      if
                        actual.byte_size <> scalar.byte_size
                        || actual.word_type <> scalar.word_type
                      then
                        malformed description
                          "IC_CALL_END scalar return class is inconsistent";
                      let value =
                        define values description position result target_type
                          (Computation.declared target_type)
                      in
                      let stage =
                        match scope.result_stage with
                        | Some stage -> stage
                        | None ->
                            malformed description
                              "word-returning call has no result stage"
                      in
                      (Call_end (stage, value), None)
                  | Callable_void_return ->
                      if Option.is_some scope.result_stage then
                        malformed description
                          "U0 call unexpectedly has a result stage";
                      void_values := Value_set.add result.value_id !void_values;
                      (Call_end_void, None))
              | _ ->
                  malformed description
                    "IC_CALL_END is outside a completed call scope")
          | Opcode.Ic_rbp -> (
              if is_entry then
                unsupported description
                  "native entry does not expose an RBP object frame";
              if
                description.flags <> 0L || description.operands <> []
                || Option.is_some description.payload
              then malformed description "invalid IC_RBP shape";
              match (description.result, description.target_type) with
              | Some result, Some target_type
                when Type.pointer_depth target_type = 1 ->
                  define_frame frame_values values void_values description
                    result (Frame_base target_type);
                  (Frame_tick, None)
              | _ -> malformed description "IC_RBP requires one pointer result")
          | Opcode.Ic_imm_i64
            when Option.fold ~none:false
                   ~some:(fun type_ -> Type.pointer_depth type_ = 1)
                   description.target_type -> (
              if description.flags <> 0L || description.operands <> [] then
                malformed description "invalid frame displacement immediate";
              match
                ( description.result,
                  description.target_type,
                  description.payload )
              with
              | Some result, Some target_type, Some (Sequence.Integer offset) ->
                  let offset =
                    int_of_frame_displacement ?span:description.span offset
                  in
                  define_frame frame_values values void_values description
                    result
                    (Frame_offset (target_type, offset));
                  (Frame_tick, None)
              | _ ->
                  malformed description "invalid frame displacement immediate")
          | Opcode.Ic_add
            when Option.fold ~none:false
                   ~some:(fun type_ -> Type.pointer_depth type_ = 1)
                   description.target_type -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid frame address addition";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ base_id; offset_id ], Some result, Some target_type -> (
                  match
                    ( frame_operand frame_values description base_id,
                      frame_operand frame_values description offset_id )
                  with
                  | Frame_base base_type, Frame_offset (offset_type, offset)
                    when Type.equal base_type target_type
                         && Type.equal offset_type target_type -> (
                      match Int_map.find_opt offset frame_slots with
                      | Some slot -> (
                          match Type.pointer_to slot.slot_type with
                          | Ok expected when Type.equal expected target_type ->
                              define_frame frame_values values void_values
                                description result (Frame_address slot);
                              (Frame_tick, None)
                          | _ ->
                              malformed description
                                "frame address pointee type is inconsistent")
                      | None ->
                          malformed description
                            "frame address displacement names no checked slot")
                  | _ ->
                      malformed description
                        "frame address operands are inconsistent")
              | _ -> malformed description "invalid frame address addition")
          | Opcode.Ic_deref -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid scalar frame load";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ address_id ], Some result, Some target_type -> (
                  match frame_operand frame_values description address_id with
                  | Frame_address slot
                    when Type.equal target_type slot.slot_type ->
                      ignore
                        (checked_scalar ~allow_public:true description
                           target_type);
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      (Load_frame_value (slot.access, value), None)
                  | _ ->
                      malformed description
                        "scalar frame load does not name its exact slot")
              | _ -> malformed description "invalid scalar frame load")
          | Opcode.Ic_assign -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid scalar frame assignment";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ address_id; input_id ], Some result, Some target_type -> (
                  match frame_operand frame_values description address_id with
                  | Frame_address slot
                    when Type.equal target_type slot.slot_type ->
                      let input =
                        operand values description position input_id
                      in
                      ignore
                        (checked_scalar ~allow_public:true description
                           input.declared_type);
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      (Store_frame_value (slot.access, input, value), None)
                  | _ ->
                      malformed description
                        "scalar assignment does not name its exact slot")
              | _ -> malformed description "invalid scalar frame assignment")
          | Opcode.Ic_add_equ
          | Opcode.Ic_sub_equ
          | Opcode.Ic_mul_equ
          | Opcode.Ic_div_equ
          | Opcode.Ic_mod_equ
          | Opcode.Ic_and_equ
          | Opcode.Ic_or_equ
          | Opcode.Ic_xor_equ
          | Opcode.Ic_shl_equ
          | Opcode.Ic_shr_equ
          | Opcode.Ic_pp_
          | Opcode.Ic_mm_
          | Opcode.Ic__pp
          | Opcode.Ic__mm -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid scalar frame update";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | address_id :: operands, Some result, Some target_type -> (
                  match frame_operand frame_values description address_id with
                  | Frame_address slot
                    when Type.equal target_type slot.slot_type ->
                      let update, old_result, expects_operand =
                        Option.get
                          (callable_frame_update description.opcode
                             slot.slot_word)
                      in
                      let input =
                        match (expects_operand, operands) with
                        | true, [ input_id ] ->
                            let input =
                              operand values description position input_id
                            in
                            ignore
                              (checked_scalar ~allow_public:true description
                                 input.declared_type);
                            Some input
                        | false, [] -> None
                        | _ ->
                            malformed description
                              "invalid scalar update operands"
                      in
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      let arithmetic_site =
                        match update with
                        | Update_division arithmetic_operation ->
                            let fault_site =
                              {
                                site;
                                operation = arithmetic_operation;
                                instruction_id =
                                  Sequence.Instruction_id.to_int
                                    description.instruction_id;
                                position;
                                span = description.span;
                                signed = slot.slot_word = I64;
                              }
                            in
                            arithmetic_sites := fault_site :: !arithmetic_sites;
                            Some fault_site
                        | Update_binary _ | Update_shift _ -> None
                      in
                      ( Update_frame_value
                          ( slot.access,
                            update,
                            input,
                            old_result,
                            value,
                            slot.slot_word,
                            arithmetic_site ),
                        None )
                  | _ ->
                      malformed description
                        "scalar update does not name its exact slot")
              | _ -> malformed description "invalid scalar frame update")
          | Opcode.Ic_end_exp -> (
              if description.flags <> 0x200L then
                malformed description "IC_END_EXP requires flags=0x000000200";
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, None, None ->
                  if Value_set.mem operand_id !void_values then
                    (Discard_void, None)
                  else
                    let input =
                      operand values description position operand_id
                    in
                    let word =
                      (checked_scalar ~allow_public:true description
                         input.declared_type)
                        .word_type
                    in
                    ( Discard_value (input, word),
                      if is_entry then Some word else None )
              | _ -> malformed description "invalid IC_END_EXP shape")
          | Opcode.Ic_jmp ->
              if
                description.flags <> 0L || description.operands <> []
                || Option.is_some description.result
                || Option.is_some description.target_type
              then malformed description "invalid IC_JMP shape"
              else (Jump_to (checked_target graph description), None)
          | Opcode.Ic_br_zero | Opcode.Ic_br_not_zero -> (
              if
                description.flags <> 0L
                || Option.is_some description.result
                || Option.is_some description.target_type
              then malformed description "invalid native branch shape";
              match description.operands with
              | [ operand_id ] ->
                  let input = operand values description position operand_id in
                  ignore
                    (checked_scalar ~allow_public:true description
                       input.declared_type);
                  let target = checked_target graph description in
                  ( (if description.opcode = Opcode.Ic_br_zero then
                       Branch_zero (input, target)
                     else Branch_not_zero (input, target)),
                    None )
              | _ -> malformed description "invalid native branch shape")
          | Opcode.Ic_switch ->
              let shape = checked_switch_shape graph description in
              let adjusted =
                operand values description position shape.adjusted_index
              in
              let range =
                operand values description position shape.range_value
              in
              if checked_word description adjusted.declared_type <> I64 then
                malformed description
                  "IC_SWITCH adjusted index must be internal I64";
              if checked_word description range.declared_type <> I64 then
                malformed description "IC_SWITCH range must be internal I64";
              (Switch_to (adjusted, range, shape.targets), None)
          | Opcode.Ic_return_val when not is_entry -> (
              if
                description.flags <> 0L
                || Option.is_some description.result
                || Option.is_some description.payload
              then malformed description "invalid IC_RETURN_VAL shape";
              match
                (description.operands, description.target_type, expected_return)
              with
              | [ operand_id ], Some target_type, Some expected
                when Type.equal target_type expected -> (
                  let input = operand values description position operand_id in
                  match source_return_kind ?span:description.span expected with
                  | Callable_void_return ->
                      unsupported description
                        "U0 source functions cannot return a word value"
                  | Callable_word_return _ ->
                      ignore
                        (checked_scalar ~allow_public:true description
                           input.declared_type);
                      (Return_value input, None))
              | _ ->
                  malformed description
                    "IC_RETURN_VAL disagrees with function return type")
          | Opcode.Ic_ret when not is_entry ->
              if
                description.flags <> 0L || description.operands <> []
                || Option.is_some description.result
                || Option.is_some description.target_type
                || Option.is_some description.payload
              then malformed description "invalid IC_RET shape"
              else if position <> terminal_position then
                malformed description
                  "IC_RET must terminate its native source function block"
              else (Return, None)
          | Opcode.Ic_end when is_entry ->
              if
                description.flags <> 0L || description.operands <> []
                || Option.is_some description.result
                || Option.is_some description.target_type
                || Option.is_some description.payload
              then malformed description "invalid IC_END shape"
              else (End_stream, None)
          | _ when Option.is_some (opcode_kind description.opcode) -> (
              if description.flags <> 0L then
                malformed description
                  "native word producer has unsupported flags";
              match
                prepare_word_operation ~allow_public:true ~allow_narrow:true
                  ~values ~fault_sites:arithmetic_sites ~position ~site
                  description
              with
              | Some operation -> (operation, None)
              | None -> assert false)
          | _ ->
              unsupported description
                "opcode is outside the native callable source subset"
        in
        let push_stage =
          if not pushes then None
          else
            let result =
              match raw.result with
              | Some result -> result
              | None -> malformed raw "ICF_PUSH_RES requires a produced value"
            in
            let value =
              match Value_map.find_opt result.value_id !values with
              | Some value -> value
              | None ->
                  malformed raw "ICF_PUSH_RES requires a scalar word result"
            in
            match !calls with
            | scope :: _ when scope.phase = Collecting -> (
                let argument =
                  Runtime.arguments scope.call
                  |> List.find_opt (fun argument ->
                      Sequence.Instruction_id.equal
                        (Runtime.argument_producer argument)
                        raw.instruction_id)
                in
                match argument with
                | Some argument
                  when Sequence.Value_id.equal
                         (Runtime.argument_value argument)
                         result.value_id -> (
                    match Runtime.argument_role argument with
                    | Runtime.Fixed index
                      when index >= 0
                           && index < Array.length scope.pushed
                           && not scope.pushed.(index) ->
                        (match Runtime.argument_prepared_default argument with
                        | None -> ()
                        | Some prepared -> (
                            match parameter_defaults with
                            | None ->
                                unsupported raw
                                  "native callable programs do not admit \
                                   prepared parameter defaults"
                            | Some proof ->
                                let header = Runtime.header scope.call in
                                let parameter =
                                  header |> Headers.function_signature
                                  |> Headers.signature_parameters
                                  |> fun parameters ->
                                  List.nth_opt parameters index
                                in
                                (match parameter with
                                | Some parameter
                                  when Defaults.admits proof ~prepared ~header
                                         ~parameter -> ()
                                | _ ->
                                    malformed raw
                                      "prepared parameter default is outside \
                                       its sealed native source authority");
                                if
                                  raw.opcode <> Opcode.Ic_imm_i64
                                  || raw.operands <> [] || raw.flags <> 0x2000L
                                  || raw.payload
                                     <> Some
                                          (Sequence.Integer
                                             (Prepared_default.bits prepared))
                                  || not
                                       (Option.fold ~none:false
                                          ~some:
                                            (Type.equal
                                               (Prepared_default.type_ prepared))
                                          raw.target_type)
                                then
                                  malformed raw
                                    "prepared parameter default producer \
                                     differs from its sealed declaration-time \
                                     value"));
                        (match raw.target_type with
                        | Some type_
                          when Type.equal
                                 (Runtime.argument_source_type argument)
                                 type_ -> ()
                        | _ ->
                            malformed raw
                              "pushed argument source type is inconsistent");
                        ignore
                          (checked_scalar ~allow_public:true raw
                             (Runtime.argument_target_type argument));
                        ignore
                          (checked_scalar ~allow_public:true raw
                             value.declared_type);
                        scope.pushed.(index) <- true;
                        value.last_use <- max value.last_use position;
                        Some (value, scope.argument_stages.(index))
                    | Runtime.Fixed _
                    | Runtime.Variadic_count
                    | Runtime.Variadic _ ->
                        malformed raw "pushed argument role is inconsistent")
                | _ ->
                    malformed raw "pushed result has no exact runtime argument")
            | _ -> malformed raw "ICF_PUSH_RES has no collecting outer call"
        in
        let arithmetic =
          match operation with
          | Apply_division (arithmetic_operation, word, _, _, _, _) ->
              Some (arithmetic_operation, word = I64)
          | Update_frame_value
              (_, Update_division arithmetic_operation, _, _, _, word, _) ->
              Some (arithmetic_operation, word = I64)
          | _ -> None
        in
        sites_rev :=
          {
            site;
            owner;
            block_id = Sequence.Block_id.to_int block_id;
            instruction_id = Sequence.Instruction_id.to_int raw.instruction_id;
            position;
            global_position = site - 1;
            span = raw.span;
            arithmetic;
            value_type;
            call_site =
              (match operation with
              | Direct_call _ -> true
              | _ -> false);
            uninitialized_read_site =
              (match operation with
              | Load_frame_value ({ initialized_flag_offset = Some _; _ }, _) ->
                  true
              | Update_frame_value
                  ({ initialized_flag_offset = Some _; _ }, _, _, _, _, _, _) ->
                  true
              | _ -> false);
          }
          :: !sites_rev;
        prepared_rev :=
          { operation; span = raw.span; site = Some site; push_stage }
          :: !prepared_rev)
      descriptions;
    if !calls <> [] then
      reject "HCBACK0003"
        (Printf.sprintf "native callable block ^b%d ends inside a direct call"
           (Sequence.Block_id.to_int block_id));
    let final_operation =
      match !prepared_rev with
      | instruction :: _ -> Some instruction.operation
      | [] -> None
    in
    let explicit_target =
      match final_operation with
      | Some
          ( Jump_to target
          | Branch_zero (_, target)
          | Branch_not_zero (_, target) ) -> Some target
      | _ -> None
    in
    let switch_targets =
      match final_operation with
      | Some (Switch_to (_, _, targets)) ->
          Some (ordered_unique_block_ids targets)
      | _ -> None
    in
    let fallthrough =
      match final_operation with
      | Some (Jump_to _ | Switch_to _ | End_stream | Return) -> None
      | _ -> next
    in
    let expected_successors =
      match switch_targets with
      | Some targets -> targets
      | None -> (
          match (explicit_target, fallthrough) with
          | None, None -> []
          | Some target, None | None, Some target -> [ target ]
          | Some target, Some next when Sequence.Block_id.equal target next ->
              [ target ]
          | Some target, Some next -> [ target; next ])
    in
    if not (same_block_ids (Graph.successors block) expected_successors) then
      reject "HCBACK0003"
        (Printf.sprintf
           "native callable block ^b%d has inconsistent checked successors"
           (Sequence.Block_id.to_int block_id));
    (if expected_successors = [] then
       match (is_entry, final_operation) with
       | true, Some End_stream | false, Some Return -> ()
       | false, _ ->
           reject "HCBACK0002"
             (Printf.sprintf
                "native source function block ^b%d can fall through without a \
                 value return"
                (Sequence.Block_id.to_int block_id))
       | true, _ ->
           reject "HCBACK0003" "native callable entry terminates without IC_END");
    {
      program_block_id = block_id;
      program_instructions = List.rev !prepared_rev;
      program_fallthrough = fallthrough;
    }
  in
  let rec visit reversed = function
    | [] -> List.rev reversed
    | block :: remaining ->
        let next =
          match remaining with
          | next :: _ -> Some (Graph.block_id next)
          | [] -> None
        in
        visit (visit_block block next :: reversed) remaining
  in
  let callable_blocks = visit [] blocks in
  (if not is_entry then
     let return_kind =
       match expected_return with
       | Some type_ -> source_return_kind type_
       | None -> reject "HCBACK0003" "native source function has no return type"
     in
     validate_callable_returns graph return_kind);
  {
    callable_blocks;
    callable_sites = List.rev !sites_rev;
    callable_ir_count = !ir_count;
    callable_block_count = List.length blocks;
    callable_home_slots = !home_slots;
    callable_stage_slots = !stage_high_water;
  }

let bounded_program_counts ~max_ir_instructions ~max_blocks graph =
  let rec blocks count ir = function
    | [] -> (count, ir)
    | block :: remaining ->
        if count = max_blocks then
          reject "HCBACK0001"
            (Printf.sprintf "block count exceeds max_blocks (%d)" max_blocks);
        let rec instructions ir = function
          | [] -> ir
          | instruction :: rest ->
              if ir = max_ir_instructions then
                reject ?span:(Sequence.description instruction).span
                  "HCBACK0001"
                  (Printf.sprintf
                     "IR instruction count exceeds max_ir_instructions (%d)"
                     max_ir_instructions);
              instructions (ir + 1) rest
        in
        let ir =
          Graph.instructions block |> Sequence.instructions |> instructions ir
        in
        blocks (count + 1) ir remaining
  in
  blocks 0 0 (Graph.blocks graph)

let bounded_callable_counts ~max_ir_instructions ~max_blocks graphs =
  let block_count = ref 0 in
  let ir_count = ref 0 in
  List.iter
    (fun graph ->
      List.iter
        (fun block ->
          if !block_count = max_blocks then
            reject "HCBACK0001"
              (Printf.sprintf "block count exceeds max_blocks (%d)" max_blocks);
          incr block_count;
          Graph.instructions block |> Sequence.instructions
          |> List.iter (fun instruction ->
              if !ir_count = max_ir_instructions then
                reject ?span:(Sequence.description instruction).span
                  "HCBACK0001"
                  (Printf.sprintf
                     "IR instruction count exceeds max_ir_instructions (%d)"
                     max_ir_instructions);
              incr ir_count))
        (Graph.blocks graph))
    graphs;
  (!block_count, !ir_count)

let validate_switch_code_floor ~max_code_bytes ~label block_groups =
  let used = ref 0 in
  let charge targets =
    let entries = List.length targets - 1 in
    if entries <= 0 then
      reject "HCBACK0003" "native IC_SWITCH has no bounded target table";
    (* The generated bounded dispatch necessarily contains one out-of-range
       conditional branch, one final target jump, and one conditional branch
       for every table entry except the last. This intentionally ignores the
       compare/test/decrement instructions, so it is a strict lower bound that
       can reject an impossible code quota before allocating the linear plan. *)
    let minimum = 11 + (6 * (entries - 1)) in
    if minimum > max_code_bytes - !used then
      reject "HCBACK0005"
        (Printf.sprintf
           "%s switch dispatch exceeds max_code_bytes before allocation" label);
    used := !used + minimum
  in
  List.iter
    (fun blocks ->
      List.iter
        (fun block ->
          List.iter
            (fun instruction ->
              match instruction.operation with
              | Switch_to (_, _, targets) -> charge targets
              | _ -> ())
            block.program_instructions)
        blocks)
    block_groups

let plan_label_offsets plan =
  let labels = Hashtbl.create (List.length plan) in
  let offset = ref 0 in
  List.iter
    (function
      | Planned_label label -> Hashtbl.replace labels label !offset
      | item -> offset := !offset + planned_size item)
    plan;
  (labels, !offset)

let default_status_abi () =
  if Sys.os_type = "Win32" then Windows_x64 else System_v_x64

let compile_expression ?status_abi ?(max_stack_bytes = hard_max_stack_bytes)
    ~max_ir_instructions ~max_code_bytes verified =
  match validate_limits ~max_ir_instructions ~max_code_bytes with
  | Error errors -> Error errors
  | Ok () -> (
      match validate_stack_limit ~max_stack_bytes with
      | Error errors -> Error errors
      | Ok () -> (
          try
            let block = single_block verified in
            let instructions =
              Graph.instructions block |> Sequence.instructions
            in
            (* Count with a bound before constructing any value or instruction
               maps. Sparse IDs never determine an allocation size. *)
            let ir_count = bounded_length ~max_ir_instructions instructions in
            let prepared, word_type, fault_sites =
              preflight ~count:ir_count instructions
            in
            let image_status_abi =
              match fault_sites with
              | [] -> None
              | _ ->
                  Some
                    (Option.value status_abi ~default:(default_status_abi ()))
            in
            (* Allocation runs only after the entire IR has passed preflight. *)
            let allocation =
              allocate ~max_stack_bytes ~status_abi:image_status_abi prepared
            in
            if allocation.code_size > max_code_bytes then
              reject "HCBACK0005" "native expression exceeds max_code_bytes";
            match
              Encoder.encode_all ~max_code_bytes allocation.instructions
            with
            | Error message -> reject "HCBACK0005" message
            | Ok encoded ->
                if String.length encoded <> allocation.code_size then
                  reject "HCBACK0003"
                    "encoded length does not match the allocation plan";
                Ok
                  {
                    encoded = Bytes.of_string encoded;
                    word_type;
                    ir_count;
                    machine_count = allocation.machine_count;
                    peak = allocation.peak;
                    frame_size = allocation.frame_size;
                    unwind_info = Bytes.copy allocation.unwind_info;
                    status_abi = image_status_abi;
                    fault_sites;
                  }
          with Rejected error -> Error [ error ]))

let compile_program ?status_abi ?(max_stack_bytes = hard_max_stack_bytes)
    ?(max_blocks = 4096) ~max_ir_instructions ~max_code_bytes verified =
  match validate_limits ~max_ir_instructions ~max_code_bytes with
  | Error errors -> Error errors
  | Ok () -> (
      match validate_stack_limit ~max_stack_bytes with
      | Error errors -> Error errors
      | Ok () -> (
          match validate_block_limit ~max_blocks with
          | Error errors -> Error errors
          | Ok () -> (
              try
                let graph = Ir.X87_stack.graph verified in
                let block_count, ir_count =
                  bounded_program_counts ~max_ir_instructions ~max_blocks graph
                in
                if block_count = 0 then
                  reject "HCBACK0003" "native program requires an entry block";
                let prepared_blocks, sites, preflight_ir_count =
                  preflight_program graph
                in
                if preflight_ir_count <> ir_count then
                  reject "HCBACK0003"
                    "native program preflight instruction count is inconsistent";
                validate_switch_code_floor ~max_code_bytes
                  ~label:"native program" [ prepared_blocks ];
                let abi =
                  Option.value status_abi ~default:(default_status_abi ())
                in
                let supply = make_label_supply () in
                let block_labels =
                  List.fold_left
                    (fun labels block ->
                      Block_map.add block.program_block_id (fresh_label supply)
                        labels)
                    Block_map.empty prepared_blocks
                in
                let epilogue = fresh_label supply in
                let block_plan_rev = ref [] in
                let fault_blocks_rev = ref [] in
                let frame_size = ref 0 in
                let peak = ref 0 in
                List.iter
                  (fun block ->
                    let label =
                      match
                        Block_map.find_opt block.program_block_id block_labels
                      with
                      | Some label -> label
                      | None ->
                          reject "HCBACK0003"
                            "native program block has no machine label"
                    in
                    let allocation =
                      allocate_body ~max_stack_bytes
                        ~reserved_registers:[ Encoder.R10; Encoder.R11 ]
                        ~supply
                        ~mode:
                          (Program_control
                             { block_labels; epilogue_label = epilogue })
                        block.program_instructions
                    in
                    peak := max !peak allocation.peak;
                    frame_size := max !frame_size allocation.frame_size;
                    fault_blocks_rev :=
                      List.rev_append allocation.fault_blocks !fault_blocks_rev;
                    let block_plan = Planned_label label :: allocation.plan in
                    let block_plan =
                      match block.program_fallthrough with
                      | None -> block_plan
                      | Some target ->
                          let target_label =
                            match Block_map.find_opt target block_labels with
                            | Some target -> target
                            | None ->
                                reject "HCBACK0003"
                                  "native program fallthrough target has no \
                                   machine label"
                          in
                          block_plan
                          @ [ Planned_branch (Unconditional, target_label) ]
                    in
                    block_plan_rev := List.rev_append block_plan !block_plan_rev)
                  prepared_blocks;
                let body_plan = List.rev !block_plan_rev in
                let fault_blocks = List.rev !fault_blocks_rev in
                let frame =
                  if !frame_size = 0 then None
                  else Some (encoder_frame None !frame_size)
                in
                let prefix =
                  (match frame with
                    | None -> []
                    | Some frame ->
                        [ Planned_instruction (Encoder.Alloc_stack frame) ])
                  @ [
                      Planned_instruction (Encoder.Capture_status abi);
                      Planned_instruction
                        (Encoder.Load_context (Encoder.R10, 16));
                    ]
                in
                let entry_id = Graph.entry graph |> Graph.block_id in
                let entry_label =
                  match Block_map.find_opt entry_id block_labels with
                  | Some label -> label
                  | None ->
                      reject "HCBACK0003"
                        "native program entry has no machine label"
                in
                let prefix =
                  prefix @ [ Planned_branch (Unconditional, entry_label) ]
                in
                let fault_plan =
                  fault_blocks
                  |> List.concat_map (fun block ->
                      [
                        Planned_label block.label;
                        Planned_instruction
                          (Encoder.Store_context_imm (8, block.site_value));
                        Planned_instruction
                          (Encoder.Store_context_imm (0, block.kind_value));
                        Planned_branch (Unconditional, epilogue);
                      ])
                in
                let suffix =
                  [
                    Planned_label epilogue;
                    Planned_instruction (Encoder.Load_context (Encoder.Rax, 16));
                    Planned_instruction
                      (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.R10));
                    Planned_instruction
                      (Encoder.Store_context (24, Encoder.Rax));
                  ]
                  @
                  match frame with
                  | None -> [ Planned_instruction Encoder.Ret ]
                  | Some frame ->
                      [
                        Planned_instruction (Encoder.Free_stack frame);
                        Planned_instruction Encoder.Ret;
                      ]
                in
                let plan = prefix @ body_plan @ fault_plan @ suffix in
                let instructions, code_size, machine_count =
                  resolve_plan plan
                in
                if code_size > max_code_bytes then
                  reject "HCBACK0005" "native program exceeds max_code_bytes";
                match Encoder.encode_all ~max_code_bytes instructions with
                | Error message -> reject "HCBACK0005" message
                | Ok encoded ->
                    if String.length encoded <> code_size then
                      reject "HCBACK0003"
                        "encoded length does not match the native program plan";
                    Ok
                      {
                        encoded = Bytes.of_string encoded;
                        ir_count;
                        machine_count;
                        peak = max 3 !peak;
                        frame_size = !frame_size;
                        unwind_info =
                          Bytes.copy (build_windows_unwind_info !frame_size);
                        unwind_functions =
                          [
                            ( 0,
                              code_size,
                              Bytes.copy (build_windows_unwind_info !frame_size)
                            );
                          ];
                        status_abi = abi;
                        block_count;
                        function_count = 0;
                        entry_stack_bytes = 8 + !frame_size;
                        sites;
                      }
              with Rejected error -> Error [ error ])))

let compile_callable ?status_abi ?(max_stack_bytes = hard_max_stack_bytes)
    ?(max_blocks = 4096) ?parameter_defaults ~max_ir_instructions
    ~max_code_bytes ~runtime_calls ~initialization ~entry ~functions () =
  let globals = Ir.Global_initialization.globals initialization in
  if
    Ir.Integer_globals.byte_size globals <> 0
    || Ir.Integer_globals.has_initializers globals
    || Ir.Global_initialization.regions initialization <> []
    || Ir.Global_initialization.static_regions initialization <> []
    || Ir.Global_initialization.publications initialization <> []
    || Ir.Global_initialization.prepared_steps initialization <> 0
  then
    Error
      [
        {
          code = "HCBACK0002";
          message =
            "native callable programs require an empty initialization context \
             without storage or preparation work";
          span = None;
        };
      ]
  else if functions = [] then
    if
      Runtime.matches runtime_calls ~entry ~initialization:(Some initialization)
        ~functions:[]
    then
      match parameter_defaults with
      | Some proof
        when not
               (Defaults.matches proof ~globals ~runtime_calls ~initialization
                  ~entry ~functions:[]) ->
          Error
            [
              {
                code = "HCBACK0003";
                message =
                  "native parameter-default authority belongs to another \
                   callable bundle";
                span = None;
              };
            ]
      | None | Some _ ->
          compile_program ?status_abi ~max_stack_bytes ~max_blocks
            ~max_ir_instructions ~max_code_bytes entry
    else
      Error
        [
          {
            code = "HCBACK0003";
            message =
              "native callable entry disagrees with its sealed runtime-call \
               context";
            span = None;
          };
        ]
  else
    match validate_limits ~max_ir_instructions ~max_code_bytes with
    | Error errors -> Error errors
    | Ok () -> (
        match validate_stack_limit ~max_stack_bytes with
        | Error errors -> Error errors
        | Ok () -> (
            match validate_block_limit ~max_blocks with
            | Error errors -> Error errors
            | Ok () -> (
                try
                  let function_bodies =
                    List.map
                      (fun (definition :
                             Ir.Integer_interpreter.function_definition) ->
                        definition.body)
                      functions
                  in
                  if
                    not
                      (Runtime.matches runtime_calls ~entry
                         ~initialization:(Some initialization)
                         ~functions:function_bodies)
                  then
                    reject "HCBACK0003"
                      "native callable bundle disagrees with its sealed \
                       runtime-call context";
                  Option.iter
                    (fun proof ->
                      if
                        not
                          (Defaults.matches proof ~globals ~runtime_calls
                             ~initialization ~entry ~functions)
                      then
                        reject "HCBACK0003"
                          "native parameter-default authority belongs to \
                           another callable bundle")
                    parameter_defaults;
                  let entry_graph = Ir.X87_stack.graph entry in
                  let function_graphs =
                    List.map
                      (fun (definition :
                             Ir.Integer_interpreter.function_definition) ->
                        Ir.X87_stack.graph (Function.x87 definition.body))
                      functions
                  in
                  let graphs = entry_graph :: function_graphs in
                  let block_count, ir_count =
                    bounded_callable_counts ~max_ir_instructions ~max_blocks
                      graphs
                  in
                  if block_count = 0 then
                    reject "HCBACK0003"
                      "native callable bundle requires an entry block";
                  let function_infos =
                    functions
                    |> List.map (prepare_callable_function ~max_stack_bytes)
                    |> Array.of_list
                  in
                  let next_site = ref 0 in
                  let entry_prepared =
                    preflight_callable_graph ~runtime_calls ~parameter_defaults
                      ~functions:function_infos ~runtime_owner:Runtime.Entry
                      ~owner:Entry_owner ~frame_slots:Int_map.empty
                      ~expected_return:None ~is_entry:true ~next_site
                      entry_graph
                  in
                  let function_prepared =
                    Array.map
                      (fun info ->
                        let body = info.definition.body in
                        preflight_callable_graph ~runtime_calls
                          ~parameter_defaults ~functions:function_infos
                          ~runtime_owner:(Runtime.Function body)
                          ~owner:info.owner ~frame_slots:info.frame_slots
                          ~expected_return:(Some (Function.return_type body))
                          ~is_entry:false ~next_site
                          (Ir.X87_stack.graph (Function.x87 body)))
                      function_infos
                  in
                  validate_callable_parameter_defaults ~parameter_defaults
                    function_infos;
                  let preflight_ir_count =
                    entry_prepared.callable_ir_count
                    + Array.fold_left
                        (fun total body -> total + body.callable_ir_count)
                        0 function_prepared
                  in
                  let preflight_block_count =
                    entry_prepared.callable_block_count
                    + Array.fold_left
                        (fun total body -> total + body.callable_block_count)
                        0 function_prepared
                  in
                  if
                    preflight_ir_count <> ir_count
                    || preflight_block_count <> block_count
                    || !next_site <> ir_count
                  then
                    reject "HCBACK0003"
                      "native callable preflight counts are inconsistent";
                  validate_switch_code_floor ~max_code_bytes
                    ~label:"native callable program"
                    (entry_prepared.callable_blocks
                    :: Array.to_list
                         (Array.map
                            (fun body -> body.callable_blocks)
                            function_prepared));
                  let sites =
                    entry_prepared.callable_sites
                    @ (Array.to_list function_prepared
                      |> List.concat_map (fun body -> body.callable_sites))
                  in
                  let abi =
                    Option.value status_abi ~default:(default_status_abi ())
                  in
                  let supply = make_label_supply () in
                  let function_labels =
                    Array.init (Array.length function_infos) (fun _ ->
                        fresh_label supply)
                  in
                  let allocate_graph ~graph ~prepared ~rbp_bytes
                      ~init_flag_offsets ~is_entry ~start_label =
                    let fixed_stack_slots =
                      prepared.callable_home_slots
                      + prepared.callable_stage_slots
                    in
                    let base_bytes = rbp_bytes + (fixed_stack_slots * 8) in
                    let base_frame =
                      if base_bytes = 0 then 0 else align_up base_bytes 16
                    in
                    if base_frame > max_stack_bytes || base_frame > 4080 then
                      reject "HCBACK0004"
                        (Printf.sprintf
                           "native callable fixed frame requires %d bytes, \
                            exceeding max_stack_bytes (%d)"
                           base_frame max_stack_bytes);
                    let block_labels =
                      List.fold_left
                        (fun labels block ->
                          Block_map.add block.program_block_id
                            (fresh_label supply) labels)
                        Block_map.empty prepared.callable_blocks
                    in
                    let epilogue = fresh_label supply in
                    let block_plan_rev = ref [] in
                    let fault_blocks_rev = ref [] in
                    let frame_size = ref base_frame in
                    let peak = ref 0 in
                    List.iter
                      (fun block ->
                        let label =
                          match
                            Block_map.find_opt block.program_block_id
                              block_labels
                          with
                          | Some label -> label
                          | None ->
                              reject "HCBACK0003"
                                "native callable block has no machine label"
                        in
                        let allocation =
                          allocate_body
                            ~callable_frame:{ rbp_bytes; fixed_stack_slots }
                            ~max_stack_bytes
                            ~reserved_registers:[ Encoder.R10; Encoder.R11 ]
                            ~supply
                            ~mode:
                              (Callable_control
                                 {
                                   block_labels;
                                   epilogue_label = epilogue;
                                   is_entry;
                                   home_slots = prepared.callable_home_slots;
                                 })
                            block.program_instructions
                        in
                        if allocation.frame_size > 4080 then
                          reject "HCBACK0004"
                            "native callable frame exceeds the single \
                             guard-page-safe allocation bound";
                        frame_size := max !frame_size allocation.frame_size;
                        peak := max !peak allocation.peak;
                        fault_blocks_rev :=
                          List.rev_append allocation.fault_blocks
                            !fault_blocks_rev;
                        let block_plan =
                          Planned_label label :: allocation.plan
                        in
                        let block_plan =
                          match block.program_fallthrough with
                          | None -> block_plan
                          | Some target ->
                              let target_label =
                                match
                                  Block_map.find_opt target block_labels
                                with
                                | Some target -> target
                                | None ->
                                    reject "HCBACK0003"
                                      "native callable fallthrough target has \
                                       no machine label"
                              in
                              block_plan
                              @ [ Planned_branch (Unconditional, target_label) ]
                        in
                        block_plan_rev :=
                          List.rev_append block_plan !block_plan_rev)
                      prepared.callable_blocks;
                    let frame =
                      if !frame_size = 0 then None
                      else Some (encoder_call_frame None !frame_size)
                    in
                    let prefix =
                      (match start_label with
                        | None -> []
                        | Some label -> [ Planned_label label ])
                      @ [
                          Planned_instruction Encoder.Push_rbp;
                          Planned_instruction Encoder.Mov_rbp_rsp;
                        ]
                      @ (match frame with
                        | None -> []
                        | Some frame ->
                            [
                              Planned_instruction
                                (Encoder.Alloc_call_frame frame);
                            ])
                      @ (if is_entry then
                           [
                             Planned_instruction (Encoder.Capture_status abi);
                             Planned_instruction
                               (Encoder.Load_context (Encoder.R10, 16));
                           ]
                         else [])
                      @ List.concat_map
                          (fun flag_offset ->
                            [
                              Planned_instruction
                                (Encoder.Mov_imm64 (Encoder.Rax, 0L));
                              Planned_instruction
                                (Encoder.Store_frame
                                   ( encoder_frame_slot None flag_offset,
                                     Encoder.Rax ));
                            ])
                          init_flag_offsets
                    in
                    let entry_id = Graph.entry graph |> Graph.block_id in
                    let graph_entry_label =
                      match Block_map.find_opt entry_id block_labels with
                      | Some label -> label
                      | None ->
                          reject "HCBACK0003"
                            "native callable graph entry has no machine label"
                    in
                    let prefix =
                      prefix
                      @ [ Planned_branch (Unconditional, graph_entry_label) ]
                    in
                    let fault_plan =
                      List.rev !fault_blocks_rev
                      |> List.concat_map (fun block ->
                          [
                            Planned_label block.label;
                            Planned_instruction
                              (Encoder.Store_context_imm (8, block.site_value));
                            Planned_instruction
                              (Encoder.Store_context_imm (0, block.kind_value));
                            Planned_branch (Unconditional, epilogue);
                          ])
                    in
                    let suffix =
                      [ Planned_label epilogue ]
                      @ (if is_entry then
                           [
                             Planned_instruction
                               (Encoder.Load_context (Encoder.Rax, 16));
                             Planned_instruction
                               (Encoder.Binary
                                  (Encoder.Sub, Encoder.Rax, Encoder.R10));
                             Planned_instruction
                               (Encoder.Store_context (24, Encoder.Rax));
                           ]
                         else [])
                      @ (match frame with
                        | None -> []
                        | Some frame ->
                            [
                              Planned_instruction
                                (Encoder.Free_call_frame frame);
                            ])
                      @ [
                          Planned_instruction Encoder.Pop_rbp;
                          Planned_instruction Encoder.Ret;
                        ]
                    in
                    {
                      body_plan =
                        prefix @ List.rev !block_plan_rev @ fault_plan @ suffix;
                      body_frame_size = !frame_size;
                      body_peak = !peak;
                      body_unwind =
                        build_callable_windows_unwind_info !frame_size;
                    }
                  in
                  let entry_allocated =
                    allocate_graph ~graph:entry_graph ~prepared:entry_prepared
                      ~rbp_bytes:0 ~init_flag_offsets:[] ~is_entry:true
                      ~start_label:None
                  in
                  let functions_allocated =
                    Array.mapi
                      (fun index info ->
                        allocate_graph
                          ~graph:
                            (Ir.X87_stack.graph
                               (Function.x87 info.definition.body))
                          ~prepared:function_prepared.(index)
                          ~rbp_bytes:info.rbp_bytes
                          ~init_flag_offsets:info.init_flag_offsets
                          ~is_entry:false
                          ~start_label:(Some function_labels.(index)))
                      function_infos
                  in
                  let callee_stack_bytes =
                    Array.map
                      (fun body -> 16 + body.body_frame_size)
                      functions_allocated
                  in
                  let plan =
                    entry_allocated.body_plan
                    @ (Array.to_list functions_allocated
                      |> List.concat_map (fun body -> body.body_plan))
                  in
                  let label_offsets, planned_code_size =
                    plan_label_offsets plan
                  in
                  let instructions, code_size, machine_count =
                    resolve_plan ~callee_labels:function_labels
                      ~callee_stack_bytes plan
                  in
                  if code_size <> planned_code_size then
                    reject "HCBACK0003"
                      "native callable label accounting disagrees with plan \
                       resolution";
                  if code_size > max_code_bytes then
                    reject "HCBACK0005"
                      "native callable program exceeds max_code_bytes";
                  let function_starts =
                    Array.map
                      (fun label ->
                        match Hashtbl.find_opt label_offsets label with
                        | Some offset -> offset
                        | None ->
                            reject "HCBACK0003"
                              "native callable function has no resolved start \
                               offset")
                      function_labels
                  in
                  let unwind_functions =
                    let entry_end =
                      if Array.length function_starts = 0 then code_size
                      else function_starts.(0)
                    in
                    let entry_record =
                      (0, entry_end, Bytes.copy entry_allocated.body_unwind)
                    in
                    let named =
                      Array.to_list
                        (Array.mapi
                           (fun index body ->
                             let begin_offset = function_starts.(index) in
                             let end_offset =
                               if index + 1 < Array.length function_starts then
                                 function_starts.(index + 1)
                               else code_size
                             in
                             ( begin_offset,
                               end_offset,
                               Bytes.copy body.body_unwind ))
                           functions_allocated)
                    in
                    entry_record :: named
                  in
                  match Encoder.encode_all ~max_code_bytes instructions with
                  | Error message -> reject "HCBACK0005" message
                  | Ok encoded ->
                      if String.length encoded <> code_size then
                        reject "HCBACK0003"
                          "encoded length does not match the native callable \
                           plan";
                      let peak =
                        Array.fold_left
                          (fun peak body -> max peak body.body_peak)
                          entry_allocated.body_peak functions_allocated
                      in
                      let frame_size =
                        Array.fold_left
                          (fun size body -> max size body.body_frame_size)
                          entry_allocated.body_frame_size functions_allocated
                      in
                      Ok
                        {
                          encoded = Bytes.of_string encoded;
                          ir_count;
                          machine_count;
                          peak = max 5 peak;
                          frame_size;
                          unwind_info = Bytes.copy entry_allocated.body_unwind;
                          unwind_functions;
                          status_abi = abi;
                          block_count;
                          function_count = Array.length function_infos;
                          entry_stack_bytes =
                            16 + entry_allocated.body_frame_size;
                          sites;
                        }
                with Rejected error -> Error [ error ])))

let expression_code (compiled : expression_image) =
  Bytes.to_string compiled.encoded

let expression_word_type (compiled : expression_image) = compiled.word_type
let expression_ir_instructions (compiled : expression_image) = compiled.ir_count

let expression_machine_instructions (compiled : expression_image) =
  compiled.machine_count

let expression_register_peak (compiled : expression_image) = compiled.peak
let expression_frame_bytes (compiled : expression_image) = compiled.frame_size

let expression_windows_unwind_info (compiled : expression_image) =
  Bytes.to_string compiled.unwind_info

let expression_status_abi (compiled : expression_image) = compiled.status_abi
let expression_fault_sites (compiled : expression_image) = compiled.fault_sites
let program_code (compiled : program_image) = Bytes.to_string compiled.encoded

let program_windows_unwind_info (compiled : program_image) =
  Bytes.to_string compiled.unwind_info

let program_windows_unwind_functions (compiled : program_image) =
  List.map
    (fun (begin_offset, end_offset, unwind) ->
      (begin_offset, end_offset, Bytes.to_string (Bytes.copy unwind)))
    compiled.unwind_functions

let program_status_abi (compiled : program_image) = compiled.status_abi
let program_ir_instructions (compiled : program_image) = compiled.ir_count

let program_machine_instructions (compiled : program_image) =
  compiled.machine_count

let program_register_peak (compiled : program_image) = compiled.peak
let program_frame_bytes (compiled : program_image) = compiled.frame_size
let program_block_count (compiled : program_image) = compiled.block_count
let program_function_count (compiled : program_image) = compiled.function_count

let program_entry_stack_bytes (compiled : program_image) =
  compiled.entry_stack_bytes

let program_sites (compiled : program_image) = compiled.sites
