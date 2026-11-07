(* TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
   OptLib.HC:535-554 and OptPass3.HC:165-228 select links from actual producers;
   BackFA.HC:339-518 selects operand orientation and materializes unlinked
   operations; BackFB.HC:488-594 handles bit-preserving identities. No implicit
   conversions or linked producers are admitted by this initial plan. *)
module S = Ir.Instruction_sequence
module G = Ir.Block_graph
module O = Ir.Opcode
module V = Map.Make (S.Value_id)
module I = Map.Make (S.Instruction_id)

type load = Bits | Zero | One

type operation =
  | Negate
  | Add
  | Subtract
  | Multiply
  | Divide
  | Copy_left
  | Copy_right

type orientation = Left_first | Right_first | Bit_copy

type entry = {
  instruction_id : S.Instruction_id.t;
  result_id : S.Value_id.t;
  operands : S.Value_id.t list;
  loads : load list;
  operation : operation;
  live_before : S.Value_id.t list;
  live_after : S.Value_id.t list;
  peak_depth : int;
  orientation : orientation;
  live_steps : S.Value_id.t list list;
}

type error = { message : string; span : Common.Span.t option }
type t = { graph : G.t; entries : (S.description * entry) I.t; peak : int }

let is_f64 type_ =
  Sema.Type.pointer_depth type_ = 0
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> true
  | _ -> false

let arithmetic = function
  | O.Ic_unary_minus -> Some Negate
  | Ic_add -> Some Add
  | Ic_sub -> Some Subtract
  | Ic_mul -> Some Multiply
  | Ic_div -> Some Divide
  | _ -> None

let prepare graph =
  let definitions = ref V.empty in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          let d = S.description instruction in
          Option.iter
            (fun (r : S.value_definition) ->
              definitions := V.add r.value_id d !definitions)
            d.result)
        (S.instructions (G.instructions block)))
    (G.blocks graph);
  let errors = ref [] in
  let entries = ref I.empty in
  let peak = ref 0 in
  let fail (d : S.description) message =
    errors := { message; span = d.span } :: !errors
  in
  let producer id = V.find_opt id !definitions in
  let float_value id =
    Option.fold ~none:false
      ~some:(fun (d : S.description) ->
        Option.fold ~none:false ~some:is_f64 d.target_type)
      (producer id)
  in
  let literal id =
    match producer id with
    | Some { opcode = O.Ic_imm_f64; payload = Some (S.Float_bits bits); _ } ->
        Some bits
    | _ -> None
  in
  let literal_load d id =
    match literal id with
    | Some bits when Int64.logand bits 0x7fffffffffffffffL = 0L -> Zero
    | Some 0x3ff0000000000000L -> One
    | Some
        ( 0x400921fb54442d18L
        | 0x400a934f0979a371L
        | 0x3ff71547652b82feL
        | 0x3fd34413509f79ffL
        | 0x3fe62e42fefa39efL ) ->
        fail d
          "native F64 arithmetic builtin constants require original \
           compiler-option evidence";
        Bits
    | _ -> Bits
  in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          let d = S.description instruction in
          let target_float =
            Option.fold ~none:false ~some:is_f64 d.target_type
          in
          (match (d.opcode, d.target_type) with
          | (O.Ic_addr | Ic_holyc_typecast), Some type_
            when Sema.Type.pointer_depth type_ > 0
                 &&
                 match Sema.Type.base type_ with
                 | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> true
                 | _ -> false ->
              fail d "native F64 pointer values are unsupported"
          | _ -> ());
          let touches_float =
            target_float || List.exists float_value d.operands
          in
          if touches_float then (
            (* Conversion, comparison and suppression flags are not a source link
         producer. A forged or merely depth-balanced suppression cannot admit
         an extended value. PUSH and END_EXP are transport flags only. *)
            let allowed_flags =
              if d.opcode = O.Ic_end_exp then 0x200L else 0x2000L
            in
            if Int64.logand d.flags (Int64.lognot allowed_flags) <> 0L then
              fail d
                "native F64 conversions and unproved x87 links are unsupported";
            match arithmetic d.opcode with
            | Some operation -> (
                if
                  (not target_float)
                  || not (List.for_all float_value d.operands)
                then
                  fail d
                    "native F64 arithmetic requires exclusively F64 operands";
                List.iter
                  (fun id ->
                    match producer id with
                    | Some p when Option.is_some (arithmetic p.opcode) ->
                        fail d
                          "native F64 linked arithmetic requires an original \
                           extended-value link proof"
                    | _ -> ())
                  d.operands;
                let loads = List.map (literal_load d) d.operands in
                (* BackFB.HC:488-594 selects bit copies for these identities before
             the general x87 template, preserving even signaling NaN bits. *)
                let operation =
                  match (operation, d.operands) with
                  | Add, [ l; _ ] when literal l = Some 0L -> Copy_right
                  | (Add | Subtract), [ _; r ] when literal r = Some 0L ->
                      Copy_left
                  | Multiply, [ l; _ ] when literal l = Some 0x3ff0000000000000L
                    -> Copy_right
                  | (Multiply | Divide), [ _; r ]
                    when literal r = Some 0x3ff0000000000000L -> Copy_left
                  | _ -> operation
                in
                let depth =
                  match (operation, loads) with
                  | (Copy_left | Copy_right), _ -> 0
                  | Negate, _ -> 1
                  | _, [ Bits; _ ] | _, [ _; Bits ] -> 1
                  | _ -> 2
                in
                match d.result with
                | None ->
                    fail d
                      "native F64 arithmetic has no original result identity"
                | Some result ->
                    let orientation, live_steps =
                      match (operation, d.operands, loads) with
                      | (Copy_left | Copy_right), _, _ -> (Bit_copy, [ [] ])
                      | Negate, [ input ], _ ->
                          ( Left_first,
                            [ []; [ input ]; [ result.value_id ]; [] ] )
                      | _, [ _; right ], [ Bits; (Zero | One) ] ->
                          ( Right_first,
                            [ []; [ right ]; [ result.value_id ]; [] ] )
                      | _, [ left; _ ], [ _; Bits ] ->
                          (Left_first, [ []; [ left ]; [ result.value_id ]; [] ])
                      | _, [ left; right ], _ ->
                          ( Left_first,
                            [
                              [];
                              [ left ];
                              [ right; left ];
                              [ result.value_id ];
                              [];
                            ] )
                      | _ -> (Left_first, [])
                    in
                    let entry =
                      {
                        instruction_id = d.instruction_id;
                        result_id = result.value_id;
                        operands = d.operands;
                        loads;
                        operation;
                        live_before = [];
                        live_after = [];
                        peak_depth = depth;
                        orientation;
                        live_steps;
                      }
                    in
                    entries := I.add d.instruction_id (d, entry) !entries;
                    peak := max !peak depth)
            | None -> (
                match d.opcode with
                | O.Ic_imm_f64
                | Ic_deref
                | Ic_assign
                | Ic_call
                | Ic_call_indirect2
                | Ic_add_rsp
                | Ic_add_rsp1
                | Ic_call_end
                | Ic_return_val
                | Ic_end_exp
                | Ic_set_rax -> ()
                | _ ->
                    fail d
                      (O.to_source_name d.opcode
                     ^ " is outside the native scalar F64 transport subset"))))
        (S.instructions (G.instructions block)))
    (G.blocks graph);
  match List.rev !errors with
  | [] -> Ok { graph; entries = !entries; peak = !peak }
  | errors -> Error errors

let find plan ~graph description =
  if graph != plan.graph then None
  else
    match I.find_opt description.S.instruction_id plan.entries with
    | Some (original, entry) when original = description -> Some entry
    | _ -> None

let peak_depth plan = plan.peak
