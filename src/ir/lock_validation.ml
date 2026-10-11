module Sequence = Instruction_sequence
module Value_map = Map.Make (Sequence.Value_id)

type t = { constants : int64 Value_map.t; branch_only : bool Value_map.t }

let fullword = function
  | Some type_ when Sema.Type.pointer_depth type_ = 0 -> (
      match Sema.Type.base type_ with
      | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) -> true
      | _ -> false)
  | _ -> false

let of_instructions instructions =
  List.fold_left
    (fun proof instruction ->
      let description = Sequence.description instruction in
      let constants =
        match
          ( description.opcode,
            description.operands,
            description.result,
            description.payload )
        with
        | Opcode.Ic_imm_i64, [], Some result, Some (Sequence.Integer bits)
          when fullword description.target_type
               && Int64.logand
                    (Lock_semantics.clear_flag description.flags)
                    (Int64.lognot 0x2000L)
                  = 0L -> Value_map.add result.value_id bits proof.constants
        | _ -> proof.constants
      in
      let branch =
        match (description.opcode, description.operands) with
        | (Opcode.Ic_br_zero | Ic_br_not_zero), [ _ ] -> true
        | _ -> false
      in
      let branch_only =
        List.fold_left
          (fun uses value ->
            Value_map.add value (branch && not (Value_map.mem value uses)) uses)
          proof.branch_only description.operands
      in
      { constants; branch_only })
    { constants = Value_map.empty; branch_only = Value_map.empty }
    instructions

let of_graph graph =
  Block_graph.blocks graph
  |> List.concat_map (fun block ->
      Block_graph.instructions block |> Sequence.instructions)
  |> of_instructions

let signed_immediate bits = bits >= -0x80000000L && bits <= 0x7fffffffL

let scalar_prefix_invalid proof (description : Sequence.description) =
  let constant value = Value_map.find_opt value proof.constants in
  let safe_immediate =
    fullword description.target_type
    &&
    match (description.opcode, description.operands) with
    | Opcode.Ic_add, [ left; right ] ->
        Option.fold ~none:false ~some:signed_immediate (constant left)
        || Option.fold ~none:false ~some:signed_immediate (constant right)
    | Opcode.Ic_sub, [ _; right ] ->
        Option.fold ~none:false
          ~some:(fun bits -> signed_immediate (Int64.neg bits))
          (constant right)
    | Opcode.Ic_and, [ _; _ ] ->
        Option.fold ~none:false
          ~some:(fun result ->
            Value_map.find_opt result.Sequence.value_id proof.branch_only
            = Some true)
          description.result
    | _ -> false
  in
  Lock_semantics.scalar_prefix_invalid description.opcode && not safe_immediate
