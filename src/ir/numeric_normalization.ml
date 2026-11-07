module S = Instruction_sequence
module G = Block_graph
module O = Opcode

type scalar_class = Integer64 | Float64

type phase =
  | Mixed_operands
  | Identity_replaced
  | Identity_normalized
  | Return_classified
  | Operand_migrated

type state = {
  phase : phase;
  input_class : scalar_class;
  input_to_float : bool;
  raw_class : scalar_class;
  class2 : scalar_class;
  stacked_class : scalar_class;
  producer_to_float : bool;
  producer_to_integer : bool;
  return_operand_to_float : bool;
  arithmetic_operand_to_float : bool;
}

type event =
  | Load_signed_integer
  | Store_binary64
  | Copy_binary64
  | Load_binary64
  | Store_integer_truncate

type position = Argument1 | Argument2 | Instruction | Result

type step = {
  owner : Instruction_sequence.Instruction_id.t;
  position : position;
  event : event;
}

type recipe = Deleted_float_identity | Materialized_copy_barrier

type t = {
  root : S.Instruction_id.t;
  input_ : S.Value_id.t;
  result_ : S.Value_id.t;
  recipe_ : recipe;
  states_ : state list;
  steps_ : step list;
  eliminated_ : S.Instruction_id.t list;
  valid_ : G.t -> bool;
}

let instruction t = t.root
let input t = t.input_
let result t = t.result_
let recipe t = t.recipe_
let states t = t.states_
let steps t = t.steps_
let events t = List.map (fun step -> step.event) t.steps_
let eliminated t = t.eliminated_
let valid t ~graph = t.valid_ graph

let scalar primitive = function
  | Some type_ -> (
      Sema.Type.pointer_depth type_ = 0
      &&
      match Sema.Type.base type_ with
      | Sema.Type.Primitive (_, actual) ->
          Sema.Primitive_type.equal actual primitive
      | _ -> false)
  | None -> false

let prepare ~runtime ~body ~frame graph =
  let authenticate () =
    graph == Function_body.body body
    && Runtime_call_context.matches_graph runtime
         ~owner:(Runtime_call_context.Function body) graph
    && Register_variable_analysis.authenticates_mixed_scalar_source ~runtime
         ~body ~frame
  in
  if not (authenticate ()) then []
  else
    let code () =
      List.concat_map
        (fun block ->
          List.map S.description (S.instructions (G.instructions block)))
        (G.blocks graph)
    in
    let original = code () in
    let producer id =
      List.find_opt
        (fun (d : S.description) ->
          Option.fold ~none:false ~some:(fun r -> r.S.value_id = id) d.result)
        original
    in
    let users id =
      List.filter (fun (d : S.description) -> List.mem id d.operands) original
    in
    let one_user id expected =
      match users id with
      | [ actual ] -> actual == expected
      | _ -> false
    in
    let literal id bits =
      match producer id with
      | Some d ->
          d.opcode = O.Ic_imm_f64 && d.flags = 0L
          && scalar Sema.Primitive_type.F64 d.target_type
          && d.payload = Some (S.Float_bits bits)
      | None -> false
    in
    List.filter_map
      (fun (root : S.description) ->
        let shape =
          match (root.opcode, root.operands) with
          | (O.Ic_add | Ic_sub), [ n; zero ] when literal zero 0L ->
              Some (n, Deleted_float_identity)
          | O.Ic_add, [ zero; n ] when literal zero 0L ->
              Some (n, Deleted_float_identity)
          | O.Ic_mul, [ n; one ] when literal one 0x3ff0000000000000L ->
              Some (n, Deleted_float_identity)
          | O.Ic_mul, [ one; n ] when literal one 0x3ff0000000000000L ->
              Some (n, Materialized_copy_barrier)
          | O.Ic_div, [ n; one ] when literal one 0x3ff0000000000000L ->
              Some (n, Materialized_copy_barrier)
          | _ -> None
        in
        match (shape, root.result) with
        | Some (input_, recipe_), Some result
          when root.flags = 2L
               && scalar Sema.Primitive_type.F64 root.target_type -> (
            match (producer input_, users result.value_id) with
            | Some input, [ return ]
              when input.opcode = O.Ic_deref && input.flags = 1L
                   && scalar Sema.Primitive_type.I64 input.target_type
                   && one_user input_ root
                   && return.opcode = O.Ic_return_val
                   && return.flags = 0L
                   && return.operands = [ result.value_id ]
                   && scalar Sema.Primitive_type.I64 return.target_type ->
                (* OptLib:124-139 installs the input conversion before pass2.
               OptPass012:376-384,560-566,588-594,662-668 eliminate identities.
               Its grouped end:715-721 updates the SURVIVOR's class2, but
               :1268 stacks the CURRENT (deleted) operator's class2. Never
               collapse these two classes. Return conversion is later, at
               OptPass3:249-256, and compares raw ic_class only. *)
                let deleted = recipe_ = Deleted_float_identity in
                let state phase raw_class class2 producer_to_float
                    producer_to_integer return_operand_to_float =
                  {
                    phase;
                    input_class = Integer64;
                    input_to_float = phase <> Operand_migrated;
                    raw_class;
                    class2;
                    stacked_class = Float64;
                    producer_to_float;
                    producer_to_integer;
                    return_operand_to_float;
                    arithmetic_operand_to_float =
                      phase = Operand_migrated && not deleted;
                  }
                in
                let states_ =
                  [
                    state Mixed_operands Float64 Float64 false false false;
                    state Identity_replaced
                      (if deleted then Integer64 else Float64)
                      Float64 deleted false false;
                    state Identity_normalized
                      (if deleted then Integer64 else Float64)
                      (if deleted then Integer64 else Float64)
                      deleted false false;
                    state Return_classified
                      (if deleted then Integer64 else Float64)
                      (if deleted then Integer64 else Float64)
                      deleted (not deleted) false;
                    state Operand_migrated
                      (if deleted then Integer64 else Float64)
                      (if deleted then Integer64 else Float64)
                      false (not deleted) deleted;
                  ]
                in
                (* Pass5 migrates MOV/REG conversion. The deleted case reaches
               RETURN's FILD/FSTP then raw RAX move, with no FISTTP. BackFB:
               488-515 copy templates instead form FALSE,FALSE link barriers. *)
                let argument_position =
                  if List.hd root.operands = input_ then Argument1
                  else Argument2
                in
                let step owner position event = { owner; position; event } in
                let steps_ =
                  List.map
                    (step
                       (if deleted then return.instruction_id
                        else root.instruction_id)
                       (if deleted then Argument1 else argument_position))
                    [ Load_signed_integer; Store_binary64 ]
                  @
                  if deleted then []
                  else
                    [
                      step root.instruction_id Instruction Copy_binary64;
                      step root.instruction_id Result Load_binary64;
                      step root.instruction_id Result Store_integer_truncate;
                    ]
                in
                let eliminated_ =
                  if deleted then
                    root.instruction_id
                    :: List.filter_map
                         (fun id ->
                           if id = input_ then None
                           else
                             Option.map
                               (fun (d : S.description) -> d.instruction_id)
                               (producer id))
                         root.operands
                  else []
                in
                Some
                  {
                    root = root.instruction_id;
                    input_;
                    result_ = result.value_id;
                    recipe_;
                    states_;
                    steps_;
                    eliminated_;
                    valid_ =
                      (fun current ->
                        current == graph && authenticate ()
                        &&
                        let now = code () in
                        List.length now = List.length original
                        && List.for_all2 ( == ) original now);
                  }
            | _ -> None)
        | _ -> None)
      original
