module Sequence = Instruction_sequence
module Graph = Block_graph
module Opcode = Opcode
module Block_id = Sequence.Block_id
module Instruction_id = Sequence.Instruction_id

type conversion_direction = To_f64 | To_int

type slot_kind =
  | Operand_2_conversion of conversion_direction
  | Operand_1_conversion of conversion_direction
  | Operation
  | Result_conversion of conversion_direction

type slot_trace = {
  index : int;
  kind : slot_kind;
  suppress_push : bool;
  suppress_pop : bool;
  before_depth : int;
  after_depth : int;
}

type instruction_trace = {
  block_id : Block_id.t;
  instruction_id : Instruction_id.t;
  before_depth : int;
  after_depth : int;
  opcode_pops_float : bool;
  use_f64 : bool;
  use_int : bool;
  push_comparison : bool;
  pop_comparison : bool;
  slots : slot_trace list;
}

type error = {
  code : string;
  message : string;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

module Block_map = Map.Make (struct
  type t = Block_id.t

  let compare = Block_id.compare
end)

module Error_set = Set.Make (String)

type t = { graph : Graph.t; trace : instruction_trace list }

let max_depth = 8
let pops_float opcode = (Opcode.info opcode).pops_float
let res_to_f64 = 0x000000001L
let res_to_int = 0x000000002L
let arg1_to_f64 = 0x000000004L
let arg1_to_int = 0x000000008L
let arg2_to_f64 = 0x000000010L
let arg2_to_int = 0x000000020L
let use_f64 = 0x000000040L
let use_int = 0x000000100L
let push_cmp = 0x000040000L
let pop_cmp = 0x000080000L
let dont_push_shift = 21
let dont_pop_shift = 24
let suppression_width = 3
let suppression_mask = 0x7L
let has flags mask = Int64.logand flags mask <> 0L

let field flags shift =
  Int64.(to_int (logand suppression_mask (shift_right_logical flags shift)))

let block_number id = Block_id.to_int id
let instruction_number id = Instruction_id.to_int id

let error ?block_id ?instruction_id ?span code message =
  {
    code;
    message;
    block_id = Option.map block_number block_id;
    instruction_id = Option.map instruction_number instruction_id;
    span;
  }

let conversion_name = function
  | To_f64 -> "to-f64"
  | To_int -> "to-int"

let slot_kind_name = function
  | Operand_2_conversion direction -> "operand-2-" ^ conversion_name direction
  | Operand_1_conversion direction -> "operand-1-" ^ conversion_name direction
  | Operation -> "operation"
  | Result_conversion direction -> "result-" ^ conversion_name direction

let is_comparison = function
  | Opcode.Ic_equ_equ
  | Ic_not_equ
  | Ic_less
  | Ic_greater_equ
  | Ic_greater
  | Ic_less_equ
  | Ic_br_equ_equ
  | Ic_br_not_equ
  | Ic_br_less
  | Ic_br_greater_equ
  | Ic_br_greater
  | Ic_br_less_equ
  | Ic_br_equ_equ2
  | Ic_br_not_equ2
  | Ic_br_less2
  | Ic_br_greater_equ2
  | Ic_br_greater2
  | Ic_br_less_equ2 -> true
  | _ -> false

let is_exit = function
  | Opcode.Ic_return_val | Ic_return_val2 | Ic_leave | Ic_ret | Ic_end -> true
  | _ -> false

let has_float_target description =
  match description.Sequence.target_type with
  | Some target when Sema.Type.pointer_depth target = 0 -> (
      match Sema.Type.base target with
      | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> true
      | Primitive (_, _) | Aggregate _ -> false)
  | Some _ | None -> false

let target_float_operation description =
  has_float_target description
  &&
  match description.Sequence.opcode with
  | Opcode.Ic_unary_minus
  | Ic_mul_equ
  | Ic_div_equ
  | Ic_mod_equ
  | Ic_add_equ
  | Ic_sub_equ -> true
  | _ -> false

let operation_uses_x87 description =
  let flags = description.Sequence.flags in
  if has flags use_int then false
  else
    pops_float description.opcode
    || (has flags use_f64 && is_comparison description.opcode)
    || target_float_operation description

let conversion_slot flags to_float to_int kind =
  if has flags to_float then [ kind To_f64 ]
  else if has flags to_int then [ kind To_int ]
  else []

let slots description =
  let flags = description.Sequence.flags in
  conversion_slot flags arg2_to_f64 arg2_to_int (fun direction ->
      Operand_2_conversion direction)
  @ conversion_slot flags arg1_to_f64 arg1_to_int (fun direction ->
      Operand_1_conversion direction)
  @ (if operation_uses_x87 description then [ Operation ] else [])
  @
  if description.opcode = Opcode.Ic_push_cmp then []
  else
    conversion_slot flags res_to_f64 res_to_int (fun direction ->
        Result_conversion direction)

let conflicting flags left right = has flags left && has flags right

let malformed_flag_errors block_id description slot_count =
  let flags = description.Sequence.flags in
  let instruction_id = description.Sequence.instruction_id in
  let make message =
    error ~block_id ~instruction_id ?span:description.span "HCIR0021" message
  in
  let conflicts =
    [
      (res_to_f64, res_to_int, "result");
      (arg1_to_f64, arg1_to_int, "operand 1");
      (arg2_to_f64, arg2_to_int, "operand 2");
      (use_f64, use_int, "operation");
    ]
    |> List.filter_map (fun (left, right, subject) ->
        if conflicting flags left right then
          Some
            (make (subject ^ " requests incompatible float and integer paths"))
        else None)
  in
  let comparison_errors =
    if
      (has flags push_cmp || has flags pop_cmp)
      && not (is_comparison description.opcode)
    then
      [
        make
          (Printf.sprintf "%s carries comparison-transfer flags"
             (Opcode.to_source_name description.opcode));
      ]
    else []
  in
  let count_errors =
    if slot_count > suppression_width then
      [
        make
          (Printf.sprintf
             "%s needs %d x87 operations, but its flags encode only %d"
             (Opcode.to_source_name description.opcode)
             slot_count suppression_width);
      ]
    else []
  in
  let active_mask =
    if slot_count <= 0 then 0
    else if slot_count >= suppression_width then (1 lsl suppression_width) - 1
    else (1 lsl slot_count) - 1
  in
  let push_bits = field flags dont_push_shift in
  let pop_bits = field flags dont_pop_shift in
  let unused = push_bits lor pop_bits land lnot active_mask in
  let suppression_errors =
    if unused <> 0 then
      [
        make
          (Printf.sprintf
             "%s suppresses an x87 operation that it does not contain"
             (Opcode.to_source_name description.opcode));
      ]
    else []
  in
  conflicts @ comparison_errors @ count_errors @ suppression_errors

let static_errors graph =
  List.concat_map
    (fun block ->
      let block_id = Graph.block_id block in
      Graph.instructions block |> Sequence.instructions
      |> List.concat_map (fun instruction ->
          let description = Sequence.description instruction in
          malformed_flag_errors block_id description
            (List.length (slots description))))
    (Graph.blocks graph)

let slot_effect block_id description depth index kind =
  let flags = description.Sequence.flags in
  let suppress_push = field flags dont_push_shift land (1 lsl index) <> 0 in
  let suppress_pop = field flags dont_pop_shift land (1 lsl index) <> 0 in
  let pushed = if suppress_push then depth else depth + 1 in
  let after_depth = if suppress_pop then pushed else pushed - 1 in
  let errors =
    if pushed < 1 && not suppress_pop then
      [
        error ~block_id ~instruction_id:description.instruction_id
          ?span:description.span "HCIR0022"
          (Printf.sprintf "%s underflows the x87 stack at %s slot %d"
             (Opcode.to_source_name description.opcode)
             (slot_kind_name kind) index);
      ]
    else if pushed > max_depth then
      [
        error ~block_id ~instruction_id:description.instruction_id
          ?span:description.span "HCIR0023"
          (Printf.sprintf "%s raises the x87 depth to %d; the limit is %d"
             (Opcode.to_source_name description.opcode)
             pushed max_depth);
      ]
    else []
  in
  ( {
      index;
      kind;
      suppress_push;
      suppress_pop;
      before_depth = depth;
      after_depth;
    },
    after_depth,
    errors )

let boundary_errors block_id description after_depth =
  let opcode = description.Sequence.opcode in
  let instruction_id = description.instruction_id in
  let make boundary depth =
    error ~block_id ~instruction_id ?span:description.span "HCIR0025"
      (Printf.sprintf "%s reaches %s with x87 depth %d; depth must be zero"
         (Opcode.to_source_name opcode)
         boundary depth)
  in
  if is_exit opcode && after_depth <> 0 then
    [ make "a function exit" after_depth ]
  else []

let verify_block block incoming_depth =
  let block_id = Graph.block_id block in
  let depth = ref incoming_depth in
  let traces = ref [] in
  let errors = ref [] in
  List.iter
    (fun instruction ->
      let description = Sequence.description instruction in
      let before_depth = !depth in
      let slot_traces = ref [] in
      List.iteri
        (fun index kind ->
          let slot, after_depth, slot_errors =
            slot_effect block_id description !depth index kind
          in
          depth := after_depth;
          slot_traces := slot :: !slot_traces;
          errors := List.rev_append slot_errors !errors)
        (slots description);
      errors :=
        List.rev_append (boundary_errors block_id description !depth) !errors;
      traces :=
        {
          block_id;
          instruction_id = description.instruction_id;
          before_depth;
          after_depth = !depth;
          opcode_pops_float = pops_float description.opcode;
          use_f64 = has description.flags use_f64;
          use_int = has description.flags use_int;
          push_comparison = has description.flags push_cmp;
          pop_comparison = has description.flags pop_cmp;
          slots = List.rev !slot_traces;
        }
        :: !traces)
    (Graph.instructions block |> Sequence.instructions);
  (!depth, List.rev !traces, List.rev !errors)

let verify_depths graph =
  let incoming = ref Block_map.empty in
  let traces = ref Block_map.empty in
  let errors = ref [] in
  let merge_errors = ref Error_set.empty in
  let queue = Queue.create () in
  let enqueue id depth =
    match Block_map.find_opt id !incoming with
    | None ->
        incoming := Block_map.add id depth !incoming;
        Queue.add id queue
    | Some expected when expected = depth -> ()
    | Some expected ->
        let key = Printf.sprintf "%d:%d:%d" (block_number id) expected depth in
        if not (Error_set.mem key !merge_errors) then (
          merge_errors := Error_set.add key !merge_errors;
          errors :=
            error ~block_id:id "HCIR0024"
              (Printf.sprintf
                 "block ^b%d receives x87 depths %d and %d from different edges"
                 (block_number id) expected depth)
            :: !errors)
  in
  let drain () =
    while not (Queue.is_empty queue) do
      let id = Queue.take queue in
      match Graph.find_block graph id with
      | None -> ()
      | Some block ->
          let depth = Block_map.find id !incoming in
          let outgoing, block_trace, block_errors = verify_block block depth in
          traces := Block_map.add id block_trace !traces;
          errors := List.rev_append block_errors !errors;
          List.iter
            (fun successor -> enqueue successor outgoing)
            (Graph.successors block)
    done
  in
  enqueue (Graph.entry graph |> Graph.block_id) 0;
  drain ();
  List.iter
    (fun block ->
      let id = Graph.block_id block in
      if not (Block_map.mem id !incoming) then (
        enqueue id 0;
        drain ()))
    (Graph.blocks graph);
  let ordered_trace =
    Graph.blocks graph
    |> List.concat_map (fun block ->
        Block_map.find_opt (Graph.block_id block) !traces
        |> Option.value ~default:[])
  in
  (ordered_trace, List.rev !errors)

let verify graph =
  match static_errors graph with
  | _ :: _ as errors -> Error errors
  | [] -> (
      let trace, errors = verify_depths graph in
      match errors with
      | _ :: _ -> Error errors
      | [] -> Ok { graph; trace })

let graph verified = verified.graph
let trace verified = verified.trace

let human verified =
  let buffer = Buffer.create 512 in
  Printf.bprintf buffer "holyc-ir-x87-v1 reference=%s\n"
    Sequence.reference_commit;
  List.iter
    (fun (instruction : instruction_trace) ->
      Printf.bprintf buffer "^b%d !i%d depth=%d->%d"
        (block_number instruction.block_id)
        (instruction_number instruction.instruction_id)
        instruction.before_depth instruction.after_depth;
      Printf.bprintf buffer
        " fpop=%b use-f64=%b use-int=%b cmp-push=%b cmp-pop=%b"
        instruction.opcode_pops_float instruction.use_f64 instruction.use_int
        instruction.push_comparison instruction.pop_comparison;
      List.iter
        (fun slot ->
          Printf.bprintf buffer " %d:%s%s%s:%d->%d" slot.index
            (slot_kind_name slot.kind)
            (if slot.suppress_push then ":no-push" else "")
            (if slot.suppress_pop then ":no-pop" else "")
            slot.before_depth slot.after_depth)
        instruction.slots;
      Buffer.add_char buffer '\n')
    verified.trace;
  Buffer.contents buffer
