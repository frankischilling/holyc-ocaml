module Sequence = Ir.Instruction_sequence
module Graph = Ir.Block_graph
module Opcode = Ir.Opcode
module Type = Sema.Type
module Primitive = Sema.Primitive_type
module Computation = Sema.Integer_computation_class
module Encoder = X86_64_encoder
module Value_map = Map.Make (Sequence.Value_id)
module Instruction_set = Set.Make (Sequence.Instruction_id)
module Block_map = Map.Make (Sequence.Block_id)

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

let checked_word description type_ =
  if Type.pointer_depth type_ <> 0 then
    unsupported description "native expressions do not support pointer values";
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, Primitive.I64) -> I64
  | Type.Primitive (Type.Internal_storage, Primitive.U64) -> U64
  | _ ->
      unsupported description
        "native expressions require internal I64 or U64 values"

type value = {
  value_id : Sequence.Value_id.t;
  declared_type : Type.t;
  computation_type : Type.t;
  mutable last_use : int;
}

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
  | Return_value of value
  | Return
  | Discard_value of value * word_type
  | Jump_to of Sequence.Block_id.t
  | Branch_zero of value * Sequence.Block_id.t
  | Branch_not_zero of value * Sequence.Block_id.t
  | End_stream

type prepared_instruction = {
  operation : operation;
  span : Common.Span.t option;
  site : int option;
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

let require_type description expected actual =
  if not (Type.equal expected actual) then
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

let prepare_word_operation ~values ~fault_sites ~position ~site
    (description : Sequence.description) =
  let operand = operand values description position in
  let define = define values description position in
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
            let _ = checked_word description target_type in
            let value =
              define result target_type (Computation.forward target_type)
            in
            Load_immediate (value, bits)
        | Unary_kind unary, ([ operand_id ], Some result, Some target_type, None)
          ->
            let word = checked_word description target_type in
            let input = operand operand_id in
            let computation_type =
              match unary with
              | Encoder.Neg ->
                  let expected = Computation.negate input.computation_type in
                  require_type description expected target_type;
                  Computation.forward target_type
              | Encoder.Not ->
                  if word <> I64 then
                    malformed description "complement must declare internal I64";
                  Computation.forward input.computation_type
            in
            let result = define result target_type computation_type in
            Apply_unary (unary, input, result)
        | Logical_not_kind, ([ operand_id ], Some result, Some target_type, None)
          ->
            let _ = checked_word description target_type in
            let input = operand operand_id in
            let computation_type = Computation.forward input.computation_type in
            require_type description computation_type target_type;
            let result = define result target_type computation_type in
            Apply_logical_not (input, result)
        | ( Logical_kind binary,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            if checked_word description target_type <> I64 then
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
            let _ = checked_word description target_type in
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
            let _ = checked_word description target_type in
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
            let word = checked_word description target_type in
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
            let word = checked_word description target_type in
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
            if checked_word description target_type <> I64 then
              malformed description "comparison must declare internal I64";
            let left = operand left_id in
            let right = operand right_id in
            let condition =
              match
                checked_word description (promoted_type description left right)
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
        { operation; span = description.span; site = None } :: !prepared)
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
  | Planned_label _ -> 0

let resolve_plan plan =
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
          offset := !offset + size)
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

type label_supply = { mutable next_label : int }

type control_mode =
  | Expression_control of { epilogue_label : int option }
  | Program_control of { block_labels : int Block_map.t; epilogue_label : int }

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

let allocate_body ~max_stack_bytes ~reserved_registers ~supply ~mode prepared =
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
  let encoder_slot span index =
    match Encoder.stack_slot ~offset:(index * 8) with
    | Ok slot -> slot
    | Error message -> reject ?span "HCBACK0003" message
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
          let required = frame_bytes_for_slots candidate_slots in
          if required > max_stack_bytes then
            reject ?span "HCBACK0004"
              (Printf.sprintf
                 "native expression spill frame requires %d bytes, exceeding \
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
  let assign position destination value =
    owners.(destination) <- Some value;
    note_peak ();
    release_through position
  in
  let target_label target =
    match mode with
    | Program_control { block_labels; _ } -> (
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
      let emit = emit instruction.span in
      (match mode with
      | Expression_control _ -> ()
      | Program_control _ ->
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
      match instruction.operation with
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
          | Program_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native program contains expression return")
      | Discard_value (input, _) -> (
          match mode with
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program discard"
          | Program_control _ ->
              let site = Option.get instruction.site in
              let inputs, _ = ensure_inputs instruction.span [ input ] in
              let source = List.hd inputs in
              emit (Encoder.Store_context (40, registers.(source)));
              emit (Encoder.Store_context_imm (32, site));
              release_through position)
      | Jump_to target -> (
          match mode with
          | Program_control _ -> emit_branch Unconditional (target_label target)
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program jump")
      | Branch_zero (input, target) | Branch_not_zero (input, target) -> (
          match mode with
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program branch"
          | Program_control _ ->
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
      | End_stream -> (
          match mode with
          | Program_control { epilogue_label; _ } ->
              emit_branch Unconditional epilogue_label
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains stream end"))
    prepared;
  {
    plan = List.rev !planned;
    peak = !peak;
    frame_size = frame_bytes_for_slots !slot_high_water;
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
  block_id : int;
  instruction_id : int;
  position : int;
  global_position : int;
  span : Common.Span.t option;
  arithmetic : (arithmetic_operation * bool) option;
  value_type : word_type option;
}

type program_image = {
  encoded : bytes;
  ir_count : int;
  machine_count : int;
  peak : int;
  frame_size : int;
  unwind_info : bytes;
  status_abi : status_abi;
  block_count : int;
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

let target_from_description (description : Sequence.description) =
  match description.payload with
  | Some (Sequence.Block target) -> target
  | _ -> malformed description "control instruction requires one block target"

let checked_target graph description =
  let target = target_from_description description in
  if Option.is_none (Graph.find_block graph target) then
    malformed description "control instruction targets an unknown block";
  target

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
                block_id = Sequence.Block_id.to_int block_id;
                instruction_id =
                  Sequence.Instruction_id.to_int description.instruction_id;
                position;
                global_position = !global_position;
                span = description.span;
                arithmetic;
                value_type;
              }
              :: !sites_rev;
            prepared_rev :=
              { operation; span = description.span; site = Some site }
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
        let fallthrough =
          match final_operation with
          | Some (Jump_to _ | End_stream) -> None
          | Some (Branch_zero _ | Branch_not_zero _) | Some _ | None -> next
        in
        let expected_successors =
          match (explicit_target, fallthrough) with
          | None, None -> []
          | Some target, None -> [ target ]
          | None, Some target -> [ target ]
          | Some target, Some next when Sequence.Block_id.equal target next ->
              [ target ]
          | Some target, Some next -> [ target; next ]
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
                        status_abi = abi;
                        block_count;
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

let program_status_abi (compiled : program_image) = compiled.status_abi
let program_ir_instructions (compiled : program_image) = compiled.ir_count

let program_machine_instructions (compiled : program_image) =
  compiled.machine_count

let program_register_peak (compiled : program_image) = compiled.peak
let program_frame_bytes (compiled : program_image) = compiled.frame_size
let program_block_count (compiled : program_image) = compiled.block_count
let program_sites (compiled : program_image) = compiled.sites
