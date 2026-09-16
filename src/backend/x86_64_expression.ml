module Sequence = Ir.Instruction_sequence
module Graph = Ir.Block_graph
module Opcode = Ir.Opcode
module Type = Sema.Type
module Primitive = Sema.Primitive_type
module Computation = Sema.Integer_computation_class
module Encoder = X86_64_encoder
module Value_map = Map.Make (Sequence.Value_id)
module Instruction_set = Set.Make (Sequence.Instruction_id)

type word_type = I64 | U64
type error = { code : string; message : string; span : Common.Span.t option }

type t = {
  encoded : bytes;
  word_type : word_type;
  ir_count : int;
  machine_count : int;
  peak : int;
  frame_size : int;
  unwind_info : bytes;
}

let hard_ir_limit = 100_000
let hard_max_stack_bytes = 4088

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
  | Apply_comparison of Encoder.condition * value * value * value
  | Apply_logical_not of value * value
  | Apply_logical of Encoder.binary * value * value * value
  | Apply_word_view of value * value
  | Return_value of value
  | Return

type prepared_instruction = {
  operation : operation;
  span : Common.Span.t option;
}

type kind =
  | Immediate_kind
  | Unary_kind of Encoder.unary
  | Binary_kind of Encoder.binary
  | Shift_kind of [ `Left | `Right ]
  | Comparison_kind of Encoder.condition * Encoder.condition
  | Logical_not_kind
  | Logical_kind of Encoder.binary
  | Word_view_kind
  | Return_value_kind
  | Return_kind

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
  | Opcode.Ic_equ_equ -> Some (Comparison_kind (Encoder.E, Encoder.E))
  | Opcode.Ic_not_equ -> Some (Comparison_kind (Encoder.NE, Encoder.NE))
  | Opcode.Ic_less -> Some (Comparison_kind (Encoder.L, Encoder.B))
  | Opcode.Ic_greater_equ -> Some (Comparison_kind (Encoder.GE, Encoder.AE))
  | Opcode.Ic_greater -> Some (Comparison_kind (Encoder.G, Encoder.A))
  | Opcode.Ic_less_equ -> Some (Comparison_kind (Encoder.LE, Encoder.BE))
  | Opcode.Ic_return_val -> Some Return_value_kind
  | Opcode.Ic_ret -> Some Return_kind
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

let preflight ~count instructions =
  if count < 3 then
    reject "HCBACK0003"
      "native expressions require a value followed by IC_RETURN_VAL and IC_RET";
  let values = ref Value_map.empty in
  let instruction_ids = ref Instruction_set.empty in
  let prepared = ref [] in
  let return_type = ref None in
  let operand description position value_id =
    match Value_map.find_opt value_id !values with
    | None ->
        malformed description
          (Printf.sprintf "value %%%d has no earlier definition"
             (Sequence.Value_id.to_int value_id))
    | Some value ->
        (* An instruction index, rather than a decrementing use counter,
           handles repeated operands without releasing their register early. *)
        value.last_use <- position;
        value
  in
  let define description position (result : Sequence.value_definition)
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
  in
  let require_type description expected actual =
    if not (Type.equal expected actual) then
      malformed description
        (Printf.sprintf "target type %s does not match required class %s"
           (Sequence.type_name actual)
           (Sequence.type_name expected))
  in
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
  in
  List.iteri
    (fun position instruction ->
      let description = Sequence.description instruction in
      if Instruction_set.mem description.instruction_id !instruction_ids then
        malformed description "instruction ID is defined more than once";
      instruction_ids :=
        Instruction_set.add description.instruction_id !instruction_ids;
      (match description.span with
      | Some span when span.start < 0 || span.stop < span.start ->
          malformed description "source span is invalid"
      | Some _ | None -> ());
      if not (Int64.equal description.flags 0L) then
        unsupported description
          "native expressions require zero instruction flags";
      let kind =
        match opcode_kind description.opcode with
        | Some kind -> kind
        | None ->
            unsupported description
              "opcode is outside the native expression subset"
      in
      if position < count - 2 then
        match kind with
        | Return_value_kind | Return_kind ->
            malformed description
              "return instructions must be the exact terminal pair"
        | Immediate_kind
        | Unary_kind _
        | Binary_kind _
        | Shift_kind _
        | Comparison_kind _
        | Logical_not_kind
        | Logical_kind _
        | Word_view_kind -> ()
      else if position = count - 2 then (
        if kind <> Return_value_kind then
          malformed description "penultimate instruction must be IC_RETURN_VAL")
      else if kind <> Return_kind then
        malformed description "last instruction must be IC_RET";
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
              define description position result target_type
                (Computation.forward target_type)
            in
            Load_immediate (value, bits)
        | Unary_kind unary, ([ operand_id ], Some result, Some target_type, None)
          ->
            let word = checked_word description target_type in
            let input = operand description position operand_id in
            let computation_type =
              match unary with
              | Encoder.Neg ->
                  let expected = Computation.negate input.computation_type in
                  require_type description expected target_type;
                  Computation.forward target_type
              | Encoder.Not ->
                  if word <> I64 then
                    malformed description "complement must declare internal I64";
                  (* IC_COM declares I64 but keeps its operand's forwarded
                     computation class for enclosing operators. *)
                  Computation.forward input.computation_type
            in
            let result =
              define description position result target_type computation_type
            in
            Apply_unary (unary, input, result)
        | Logical_not_kind, ([ operand_id ], Some result, Some target_type, None)
          ->
            let _ = checked_word description target_type in
            let input = operand description position operand_id in
            let computation_type = Computation.forward input.computation_type in
            require_type description computation_type target_type;
            let result =
              define description position result target_type computation_type
            in
            Apply_logical_not (input, result)
        | ( Logical_kind binary,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            if checked_word description target_type <> I64 then
              malformed description
                "binary logical values must declare internal I64";
            let left = operand description position left_id in
            let right = operand description position right_id in
            let result =
              define description position result target_type
                (Computation.forward target_type)
            in
            Apply_logical (binary, left, right, result)
        | ( Word_view_kind,
            ( [ operand_id ],
              Some result,
              Some target_type,
              Some (Sequence.Integer 0L) ) ) ->
            let _ = checked_word description target_type in
            let input = operand description position operand_id in
            let result =
              define description position result target_type
                (Computation.declared target_type)
            in
            Apply_word_view (input, result)
        | Word_view_kind, ([ _ ], Some _, Some _, Some (Sequence.Integer 1L)) ->
            unsupported description
              "native expressions do not support parenthesized casts"
        | ( Binary_kind binary,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            let _ = checked_word description target_type in
            let left = operand description position left_id in
            let right = operand description position right_id in
            require_type description
              (promoted_type description left right)
              target_type;
            let result =
              define description position result target_type
                (Computation.forward target_type)
            in
            Apply_binary (binary, left, right, result)
        | ( Shift_kind direction,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            let word = checked_word description target_type in
            let left = operand description position left_id in
            let right = operand description position right_id in
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
              define description position result target_type
                (Computation.forward target_type)
            in
            Apply_shift (shift, left, right, result)
        | ( Comparison_kind (signed, unsigned),
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            if checked_word description target_type <> I64 then
              malformed description "comparison must declare internal I64";
            let left = operand description position left_id in
            let right = operand description position right_id in
            (* COM may declare I64 while forwarding U64. Select the condition
               from the operand computation classes before the comparison
               produces its independent I64 Boolean result. *)
            let condition =
              match
                checked_word description (promoted_type description left right)
              with
              | I64 -> signed
              | U64 -> unsigned
            in
            let result =
              define description position result target_type
                (Computation.forward target_type)
            in
            Apply_comparison (condition, left, right, result)
        | Return_value_kind, ([ operand_id ], None, Some target_type, None) ->
            let word = checked_word description target_type in
            let input = operand description position operand_id in
            require_type description input.declared_type target_type;
            return_type := Some word;
            Return_value input
        | Return_kind, ([], None, None, None) -> Return
        | _ -> shape_error ()
      in
      prepared := { operation; span = description.span } :: !prepared)
    instructions;
  match !return_type with
  | Some return_type -> (List.rev !prepared, return_type)
  | None -> reject "HCBACK0003" "native expression has no return value"

type allocation = {
  instructions : Encoder.instruction list;
  code_size : int;
  machine_count : int;
  peak : int;
  frame_size : int;
  unwind_info : bytes;
}

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

let allocate ~max_stack_bytes prepared =
  let registers = Array.of_list Encoder.registers in
  let rcx =
    let rec find index =
      if index = Array.length registers then
        reject "HCBACK0003" "native register set does not contain RCX"
      else if registers.(index) = Encoder.Rcx then index
      else find (index + 1)
    in
    find 0
  in
  let owners : value option array = Array.make (Array.length registers) None in
  let slots : value option array = Array.make (hard_max_stack_bytes / 8) None in
  let slot_high_water = ref 0 in
  let planned = ref [] in
  let peak = ref 0 in
  let emit _span instruction = planned := instruction :: !planned in
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
  let encoder_frame span bytes =
    match Encoder.stack_frame ~bytes with
    | Ok frame -> frame
    | Error message -> reject ?span "HCBACK0003" message
  in
  let note_peak ?(temporaries = []) () =
    let occupied = ref 0 in
    Array.iteri
      (fun index owner ->
        if Option.is_some owner || List.mem index temporaries then incr occupied)
      owners;
    peak := max !peak !occupied
  in
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
  let find_empty ~excluded =
    let rec find index =
      if index = Array.length owners then None
      else if List.mem index excluded || Option.is_some owners.(index) then
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
  let choose_victim span ~protected ~excluded =
    let best = ref None in
    Array.iteri
      (fun index owner ->
        if not (List.mem index protected || List.mem index excluded) then
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
  let acquire_empty span ~protected ~excluded =
    let rec find index =
      if index = Array.length owners then None
      else if List.mem index excluded then find (index + 1)
      else if Option.is_none owners.(index) then Some index
      else find (index + 1)
    in
    match find 0 with
    | Some index -> index
    | None ->
        let index = choose_victim span ~protected ~excluded in
        spill_register span index;
        index
  in
  let acquire_destination span position ~protected ~excluded =
    let rec find index =
      if index = Array.length owners then None
      else if List.mem index excluded then find (index + 1)
      else
        match owners.(index) with
        | None -> Some index
        | Some owner when owner.last_use = position -> Some index
        | Some _ -> find (index + 1)
    in
    match find 0 with
    | Some index -> index
    | None ->
        let index = choose_victim span ~protected ~excluded in
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
            (* Once a dying distinct count is in CL its spill slot can serve a
               later eviction needed to obtain the non-RCX result register. A
               duplicate left/count still needs its slot as the left source. *)
            if count.last_use = position && not shared_with_left then
              slots.(slot_index) <- None
        | None ->
            reject ?span "HCBACK0003"
              "prepared shift count has no live register or spill slot")
  in
  let assign position destination value =
    owners.(destination) <- Some value;
    note_peak ();
    release_through position
  in
  List.iteri
    (fun position (instruction : prepared_instruction) ->
      release_before position;
      let emit = emit instruction.span in
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
          (* Capture both source locations before changing any owner. This
             also covers the same value appearing in both operand positions. *)
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
            (* When only the right operand can be reused, -(right-left)
               preserves left-right modulo 2^64 without a temporary register.
               Flags are not values in this subset. *)
            if binary = Encoder.Sub then
              emit (Encoder.Unary (Encoder.Neg, target)))
          else (
            emit (Encoder.Mov (target, registers.(left)));
            emit (Encoder.Binary (binary, target, registers.(right))));
          assign position destination result
      | Apply_shift (shift, left, count, result) ->
          let count_register = find_register count in
          (* RCX is an architectural input to D3 shifts. Preserve an unrelated
             live owner, and preserve a shared left value before replacing CL.
             A left value whose final use is this shift is captured below before
             RCX is overwritten. Count ownership already in RCX stays in place. *)
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
                (* The dying left value must leave RCX before the count arrives.
                   A shared left was relocated above and cannot take this path. *)
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
          (* Both values are still intact when CMP produces the flags. Only
             then may SETcc overwrite a dying input, including the right one.
             MOVZX clears every stale bit above the selected low byte. *)
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
          (* Normalize the input occupying the destination first. Otherwise a
             dying right input could be overwritten before its TEST. Logical
             operations are commutative, including when both inputs coincide. *)
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
            (* RAX is the first register in the public allocation order. All
               other values have expired by the exact terminal return pair. *)
            if Option.is_some owners.(0) then
              reject ?span:instruction.span "HCBACK0003"
                "return register still owns a different live value";
            emit (Encoder.Mov (Encoder.Rax, registers.(source)));
            owners.(0) <- Some input;
            note_peak ());
          release_through position
      | Return -> emit Encoder.Ret)
    prepared;
  let body = List.rev !planned in
  let frame_size = frame_bytes_for_slots !slot_high_water in
  let instructions =
    if frame_size = 0 then body
    else
      let frame = encoder_frame None frame_size in
      match List.rev body with
      | Encoder.Ret :: reversed_prefix ->
          Encoder.Alloc_stack frame
          :: (List.rev reversed_prefix
             @ [ Encoder.Free_stack frame; Encoder.Ret ])
      | _ ->
          reject "HCBACK0003" "native expression allocation lost terminal RET"
  in
  {
    instructions;
    code_size =
      List.fold_left
        (fun total item -> total + Encoder.size item)
        0 instructions;
    machine_count = List.length instructions;
    peak = !peak;
    frame_size;
    unwind_info = build_windows_unwind_info frame_size;
  }

let compile ?(max_stack_bytes = hard_max_stack_bytes) ~max_ir_instructions
    ~max_code_bytes verified =
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
            let prepared, word_type = preflight ~count:ir_count instructions in
            (* Allocation runs only after the entire IR has passed preflight. *)
            let allocation = allocate ~max_stack_bytes prepared in
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
                  }
          with Rejected error -> Error [ error ]))

let code (compiled : t) = Bytes.to_string compiled.encoded
let value_type (compiled : t) = compiled.word_type
let ir_instructions (compiled : t) = compiled.ir_count
let machine_instructions (compiled : t) = compiled.machine_count
let register_peak (compiled : t) = compiled.peak
let frame_bytes (compiled : t) = compiled.frame_size
let windows_unwind_info (compiled : t) = Bytes.to_string compiled.unwind_info
