module Sequence = Instruction_sequence
module Graph = Block_graph
module X87 = X87_stack
module Block_id = Sequence.Block_id
module Instruction_id = Sequence.Instruction_id
module Value_id = Sequence.Value_id
module Frame = Sema.Function_frame_layout
module Function = Function_body
module Type = Sema.Type
module Offset_map = Map.Make (Int64)

module Value_map = Map.Make (struct
  type t = Value_id.t

  let compare = Value_id.compare
end)

module Block_map = Map.Make (struct
  type t = Block_id.t

  let compare = Block_id.compare
end)

type word_type = I64 | U64
type word = { type_ : word_type; bits : int64 }
type function_definition = { frame : Frame.function_layout; body : Function.t }

type frame_slot = {
  slot_type : Type.t;
  word_type : word_type;
  initial : word option;
}

type frame_context = {
  layout : Frame.function_layout;
  slots : frame_slot array;
  offsets : int Offset_map.t;
  return_type : Type.t;
}

type termination = Stream_end | Returned of word option
type error_stage = Configuration | Preflight | Execution

type error = {
  stage : error_stage;
  code : string;
  message : string;
  executed_steps : int;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
  function_id : int option;
  function_name : string option;
  initializer_phase : Global_initialization.phase option;
  initializer_symbol_id : int option;
  initializer_name : string option;
}

type t = {
  termination_ : termination;
  executed_steps_ : int;
  final_value_ : word option;
  compiled_initializer_steps_ : int;
}

type prepared_operand = { value_id : Value_id.t; expected_type : word_type }
type unary_operation = Complement | Logical_not | Negate

type comparison_operation =
  | Equal
  | Not_equal
  | Less
  | Greater_equal
  | Greater
  | Less_equal

type logical_operation = Logical_and | Logical_or | Logical_xor

type binary_operation =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_left
  | Shift_right
  | Compare of comparison_operation
  | Logical of logical_operation

type branch_condition = Zero | Not_zero
type storage_location = Frame_slot of int | Global_slot of int

type prepared_operation =
  | Call_start
  | Call of int
  | Call_cleanup
  | Call_end of Value_id.t * word_type
  | Frame_address_tick
  | Load_slot of storage_location * Value_id.t
  | Store_slot of storage_location * prepared_operand * Value_id.t * word_type
  | Update_slot of
      storage_location
      * binary_operation
      * prepared_operand option
      * bool
      * Value_id.t
      * word_type
  | Immediate of Value_id.t * word
  | Unary of unary_operation * prepared_operand * Value_id.t * word_type
  | Word_view of prepared_operand * Value_id.t * word_type
  | Binary of
      binary_operation
      * prepared_operand
      * prepared_operand
      * Value_id.t
      * word_type
  | Discard of prepared_operand
  | Return_value of prepared_operand * word_type
  | Jump of int
  | Branch of branch_condition * prepared_operand * int
  | Return
  | End

type prepared_instruction = {
  instruction_id : Instruction_id.t;
  span : Common.Span.t option;
  operation : prepared_operation;
  push_result : prepared_operand option;
}

type prepared_block = {
  block_id : Block_id.t;
  instructions : prepared_instruction array;
  fallthrough : int option;
}

type prepared = {
  blocks : prepared_block array;
  entry_index : int;
  initial_slots : word option array;
  is_function : bool;
  owner : (int * string) option;
}

type callee = {
  callee_index : int;
  callee_symbol : Sema.Symbol.t;
  callee_return_type : Type.t;
  parameter_types : word_type array;
  cleanup_opcode : Opcode.t;
  frame_bytes : int;
}

type call_phase = Collecting of int | Needs_cleanup | Needs_end
type checked_call = { callee : callee; phase : call_phase }

type opcode_kind =
  | Global_address_kind
  | Frame_address_kind
  | Load_slot_kind
  | Store_slot_kind
  | Update_slot_kind of binary_operation
  | Increment_slot_kind of binary_operation * bool
  | Immediate_kind
  | Unary_kind of unary_operation
  | Word_view_kind
  | Binary_kind of binary_operation
  | Discard_kind
  | Return_value_kind
  | Jump_kind
  | Branch_kind of branch_condition
  | Return_kind
  | End_kind

type declared_type =
  | Supported of word_type * Type.t
  | Frame_base of Type.t
  | Frame_offset of Type.t * int64
  | Frame_address of int
  | Global_address of Integer_globals.storage_slot
  | Unsupported

let reference_commit = Sequence.reference_commit

let make_error ?block_id ?instruction_id ?span ~stage ~executed_steps code
    message =
  {
    stage;
    code;
    message;
    executed_steps;
    block_id = Option.map Block_id.to_int block_id;
    instruction_id = Option.map Instruction_id.to_int instruction_id;
    span;
    function_id = None;
    function_name = None;
    initializer_phase = None;
    initializer_symbol_id = None;
    initializer_name = None;
  }

let identify_initializer region error =
  match region with
  | None -> error
  | Some region ->
      let symbol = Global_initialization.storage_symbol region in
      {
        error with
        initializer_phase = Some (Global_initialization.storage_phase region);
        initializer_symbol_id =
          Some (Sema.Symbol.id symbol |> Sema.Symbol.Id.to_int);
        initializer_name = Some (Sema.Symbol.name symbol);
      }

let preflight_error block_id (description : Sequence.description) code message =
  make_error ~stage:Preflight ~executed_steps:0 ~block_id
    ~instruction_id:description.instruction_id ?span:description.span code
    message

let opcode_kind = function
  | Opcode.Ic_imm_i64 -> Some Immediate_kind
  | Opcode.Ic_com -> Some (Unary_kind Complement)
  | Opcode.Ic_not -> Some (Unary_kind Logical_not)
  | Opcode.Ic_unary_minus -> Some (Unary_kind Negate)
  | Opcode.Ic_holyc_typecast -> Some Word_view_kind
  | Opcode.Ic_add -> Some (Binary_kind Add)
  | Opcode.Ic_sub -> Some (Binary_kind Subtract)
  | Opcode.Ic_mul -> Some (Binary_kind Multiply)
  | Opcode.Ic_div -> Some (Binary_kind Divide)
  | Opcode.Ic_mod -> Some (Binary_kind Remainder)
  | Opcode.Ic_and -> Some (Binary_kind Bitwise_and)
  | Opcode.Ic_or -> Some (Binary_kind Bitwise_or)
  | Opcode.Ic_xor -> Some (Binary_kind Bitwise_xor)
  | Opcode.Ic_shl -> Some (Binary_kind Shift_left)
  | Opcode.Ic_shr -> Some (Binary_kind Shift_right)
  | Opcode.Ic_equ_equ -> Some (Binary_kind (Compare Equal))
  | Opcode.Ic_not_equ -> Some (Binary_kind (Compare Not_equal))
  | Opcode.Ic_less -> Some (Binary_kind (Compare Less))
  | Opcode.Ic_greater_equ -> Some (Binary_kind (Compare Greater_equal))
  | Opcode.Ic_greater -> Some (Binary_kind (Compare Greater))
  | Opcode.Ic_less_equ -> Some (Binary_kind (Compare Less_equal))
  | Opcode.Ic_and_and -> Some (Binary_kind (Logical Logical_and))
  | Opcode.Ic_or_or -> Some (Binary_kind (Logical Logical_or))
  | Opcode.Ic_xor_xor -> Some (Binary_kind (Logical Logical_xor))
  | Opcode.Ic_end_exp -> Some Discard_kind
  | Opcode.Ic_return_val -> Some Return_value_kind
  | Opcode.Ic_jmp -> Some Jump_kind
  | Opcode.Ic_br_zero -> Some (Branch_kind Zero)
  | Opcode.Ic_br_not_zero -> Some (Branch_kind Not_zero)
  | Opcode.Ic_ret -> Some Return_kind
  | Opcode.Ic_end -> Some End_kind
  | _ -> None

let update_kind = function
  | Opcode.Ic_add_equ -> Some (Update_slot_kind Add)
  | Opcode.Ic_sub_equ -> Some (Update_slot_kind Subtract)
  | Opcode.Ic_mul_equ -> Some (Update_slot_kind Multiply)
  | Opcode.Ic_div_equ -> Some (Update_slot_kind Divide)
  | Opcode.Ic_mod_equ -> Some (Update_slot_kind Remainder)
  | Opcode.Ic_and_equ -> Some (Update_slot_kind Bitwise_and)
  | Opcode.Ic_or_equ -> Some (Update_slot_kind Bitwise_or)
  | Opcode.Ic_xor_equ -> Some (Update_slot_kind Bitwise_xor)
  | Opcode.Ic_shl_equ -> Some (Update_slot_kind Shift_left)
  | Opcode.Ic_shr_equ -> Some (Update_slot_kind Shift_right)
  | Opcode.Ic_pp_ -> Some (Increment_slot_kind (Add, false))
  | Opcode.Ic_mm_ -> Some (Increment_slot_kind (Subtract, false))
  | Opcode.Ic__pp -> Some (Increment_slot_kind (Add, true))
  | Opcode.Ic__mm -> Some (Increment_slot_kind (Subtract, true))
  | _ -> None

let scalar_word_type ~allow_public type_ =
  if Sema.Type.pointer_depth type_ <> 0 then None
  else
    match Sema.Type.base type_ with
    | Sema.Type.Primitive (form, primitive)
      when (allow_public || form = Sema.Type.Internal_storage)
           && Sema.Primitive_type.equal primitive Sema.Primitive_type.I64 ->
        Some I64
    | Sema.Type.Primitive (form, primitive)
      when (allow_public || form = Sema.Type.Internal_storage)
           && Sema.Primitive_type.equal primitive Sema.Primitive_type.U64 ->
        Some U64
    | Sema.Type.Primitive _ | Sema.Type.Aggregate _ -> None

let producer_word_type type_ = scalar_word_type ~allow_public:false type_
let return_word_type type_ = scalar_word_type ~allow_public:true type_

let frame_context ?globals ~max_frame_bytes ~frame ~arguments function_ =
  let invalid message =
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0
          ?span:(Function.span function_) "HCIRVM0011" message;
      ]
  in
  let locations = Frame.function_locations frame in
  let of_kind kind =
    List.filter (fun item -> Frame.location_kind item = kind) locations
  in
  let parameters = of_kind Frame.Named_parameter in
  let locals = of_kind Frame.Automatic_local in
  let statics = of_kind Frame.Static_local in
  let statics_match =
    List.for_all
      (fun location ->
        match
          Option.bind globals (fun globals ->
              Integer_globals.find_static globals
                (Frame.location_symbol location))
        with
        | Some slot ->
            Integer_globals.static_frame slot == frame
            && Integer_globals.static_location slot == location
            && Integer_globals.static_compiler_options slot
               = Function.compiler_options function_
        | None -> false)
      statics
  in
  let members_match members locations =
    List.length members = List.length locations
    && List.for_all2
         (fun (position, member) location ->
           Function.member_position member = position
           && Function.member_symbol member == Frame.location_symbol location
           && Type.equal
                (Function.member_type member)
                (Frame.location_checked_type location))
         (List.mapi (fun index member -> (index, member)) members)
         locations
  in
  let parameter_count = List.length parameters in
  let frame_size = Frame.function_frame_size frame in
  let allowed_flags =
    Int64.logor
      (Sema.Function_flag.Stored.to_mask Ret1)
      (Int64.logor
         (Sema.Function_flag.Stored.to_mask Argument_pop)
         (Sema.Function_flag.Stored.to_mask No_argument_pop))
  in
  if
    Function.symbol function_ != Frame.function_symbol frame
    || not
         (Sema.Symbol.Scope_id.equal
            (Function.function_scope function_)
            (Sema.Symbol_table.scope_id (Frame.function_scope frame)))
  then
    invalid
      "the named function and frame have different symbol or scope identities"
  else if
    not
      (members_match (Function.parameters function_) parameters
      && members_match (Function.locals function_) locals)
  then
    invalid "the named function members disagree with the exact checked frame"
  else if
    List.length locations
    <> parameter_count + List.length locals + List.length statics
    || (not statics_match)
    || Int64.logand
         (Function.stored_flags function_)
         (Int64.lognot allowed_flags)
       <> 0L
  then
    invalid
      "execution requires ordinary parameters, automatic scalar locals and \
       exact function-owned persistent statics"
  else if Option.is_none (return_word_type (Function.return_type function_))
  then invalid "the function return type is outside scalar I64/U64 execution"
  else if List.length arguments <> parameter_count then
    invalid "the argument word count does not match the checked parameters"
  else if
    parameter_count > max_frame_bytes / 8
    || frame_size < 0L
    || frame_size > Int64.of_int (max_frame_bytes - (parameter_count * 8))
    || List.length locations > Sys.max_array_length
  then invalid "the checked function frame exceeds max_frame_bytes"
  else
    let locations =
      List.filter
        (fun location -> Frame.location_kind location <> Frame.Static_local)
        locations
    in
    let arguments = ref arguments in
    let slots_rev = ref []
    and offsets = ref Offset_map.empty
    and error = ref None in
    List.iteri
      (fun index location ->
        match
          ( return_word_type (Frame.location_checked_type location),
            Frame.location_frame_slot location )
        with
        | Some word_type, Some slot
          when Frame.location_declarator_shape location = Frame.Object
               && Frame.location_value_shape location = Frame.Scalar
               && Frame.location_allocated_size location = 8L
               && Frame.frame_slot_size slot = 8L ->
            let offset = Frame.frame_slot_displacement slot in
            if Offset_map.mem offset !offsets then
              error := Some "the checked frame contains overlapping slots"
            else
              let initial =
                match (Frame.location_kind location, !arguments) with
                | Frame.Named_parameter, bits :: rest ->
                    arguments := rest;
                    Some { type_ = word_type; bits }
                | _ -> None
              in
              offsets := Offset_map.add offset index !offsets;
              slots_rev :=
                {
                  slot_type = Frame.location_checked_type location;
                  word_type;
                  initial;
                }
                :: !slots_rev
        | _ ->
            error :=
              Some
                "the checked frame contains an unsupported storage type or \
                 shape")
      locations;
    match !error with
    | Some message -> invalid message
    | None ->
        Ok
          {
            layout = frame;
            slots = Array.of_list (List.rev !slots_rev);
            offsets = !offsets;
            return_type = Function.return_type function_;
          }

let frame_pointer type_ =
  Type.pointer_depth type_ = 1
  &&
  match Type.base type_ with
  | Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) -> true
  | _ -> false

let address_slot context types (description : Sequence.description) =
  match (description.opcode, description.operands, description.target_type) with
  | Opcode.Ic_add, [ base; displacement ], Some target_type -> (
      match
        (Value_map.find_opt base types, Value_map.find_opt displacement types)
      with
      | Some (Frame_base base_type), Some (Frame_offset (offset_type, offset))
        when Type.equal base_type target_type
             && Type.equal offset_type target_type -> (
          match Offset_map.find_opt offset context.offsets with
          | Some index -> (
              match Type.pointer_to context.slots.(index).slot_type with
              | Ok pointer when Type.equal pointer target_type ->
                  Frame_address index
              | _ -> Unsupported)
          | None -> Unsupported)
      | _ -> Unsupported)
  | _ -> Unsupported

let storage_allowed frame initialization instruction slot =
  match Integer_globals.storage_frame slot with
  | None -> true
  | Some owner ->
      Option.fold ~none:false ~some:(fun frame -> frame.layout == owner) frame
      || Option.fold ~none:false
           ~some:(fun context ->
             match Global_initialization.find_storage context instruction with
             | Some region ->
                 Option.fold ~none:false
                   ~some:(fun frame -> frame == owner)
                   (Global_initialization.storage_frame region)
             | None -> false)
           initialization

let global_address frame globals initialization
    (description : Sequence.description) =
  match (globals, description.payload, description.target_type) with
  | Some globals, Some (Sequence.Symbol symbol), Some type_ -> (
      match Integer_globals.find_storage globals symbol with
      | Some slot
        when description.opcode = Integer_globals.storage_opcode slot
             && storage_allowed frame initialization description.instruction_id
                  slot -> (
          match Type.pointer_to (Integer_globals.storage_type slot) with
          | Ok expected when Type.equal type_ expected -> Some slot
          | _ -> None)
      | _ -> None)
  | _ -> None

let declared_types ?frame ?globals ?initialization ?(allow_calls = false) block
    =
  Graph.instructions block |> Sequence.instructions
  |> List.fold_left
       (fun types instruction ->
         let description = Sequence.description instruction in
         match description.result with
         | None -> types
         | Some result ->
             let declared =
               match
                 global_address frame globals initialization description
               with
               | Some slot -> Global_address slot
               | None -> (
                   match description.target_type with
                   | Some type_ -> (
                       if allow_calls && description.opcode = Opcode.Ic_call_end
                       then
                         match return_word_type type_ with
                         | Some word_type -> Supported (word_type, type_)
                         | None -> Unsupported
                       else
                         match (frame, description.opcode) with
                         | _, opcode
                           when (Option.is_some frame || Option.is_some globals)
                                && (opcode = Opcode.Ic_deref
                                  || opcode = Opcode.Ic_assign
                                   || Option.is_some (update_kind opcode)) -> (
                             match return_word_type type_ with
                             | Some word_type -> Supported (word_type, type_)
                             | None -> Unsupported)
                         | Some _, Opcode.Ic_rbp when frame_pointer type_ ->
                             Frame_base type_
                         | Some _, Opcode.Ic_imm_i64 when frame_pointer type_
                           -> (
                             match description.payload with
                             | Some (Sequence.Integer offset) ->
                                 Frame_offset (type_, offset)
                             | _ -> Unsupported)
                         | Some context, Opcode.Ic_add when frame_pointer type_
                           -> address_slot context types description
                         | _, opcode
                           when (Option.is_some frame || Option.is_some globals
                               || allow_calls)
                                &&
                                match opcode_kind opcode with
                                | Some (Unary_kind _ | Binary_kind _) -> true
                                | _ -> false -> (
                             match return_word_type type_ with
                             | Some word_type -> Supported (word_type, type_)
                             | None -> Unsupported)
                         | _ -> (
                             match producer_word_type type_ with
                             | Some word_type -> Supported (word_type, type_)
                             | None -> Unsupported))
                   | None -> Unsupported)
             in
             Value_map.add result.value_id declared types)
       Value_map.empty

let operand_of_value types value_id =
  match Value_map.find_opt value_id types with
  | Some (Supported (expected_type, _)) -> Some { value_id; expected_type }
  | Some
      ( Unsupported
      | Frame_base _
      | Frame_offset _
      | Frame_address _
      | Global_address _ )
  | None -> None

let valid_unary_type types operation operand_id result_type =
  match Value_map.find_opt operand_id types with
  | Some (Supported (_, operand_type)) -> (
      let internal_i64 type_ =
        Type.pointer_depth type_ = 0
        &&
        match Type.base type_ with
        | Type.Primitive (Type.Internal_storage, Sema.Primitive_type.I64) ->
            true
        | _ -> false
      in
      match (operation, Type.base operand_type) with
      | Complement, _
      | Negate, Type.Primitive (Type.Internal_storage, Sema.Primitive_type.U64)
        -> internal_i64 result_type
      | (Negate | Logical_not), _ -> Type.equal operand_type result_type)
  | _ -> false

let promoted_word_type left right =
  match (left, right) with
  | I64, I64 -> I64
  | I64, U64 | U64, I64 | U64, U64 -> U64

let expected_binary_result_type operation left right =
  match operation with
  | Compare _ | Logical _ -> I64
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_left
  | Shift_right -> promoted_word_type left right

let shift_count bits = Int64.to_int (Int64.logand bits 63L)

let comparison_order left right =
  match promoted_word_type left.type_ right.type_ with
  | I64 -> Int64.compare left.bits right.bits
  | U64 -> Int64.unsigned_compare left.bits right.bits

let comparison_bits operation left right =
  let predicate =
    match operation with
    | Equal -> Int64.equal left.bits right.bits
    | Not_equal -> not (Int64.equal left.bits right.bits)
    | Less -> comparison_order left right < 0
    | Greater_equal -> comparison_order left right >= 0
    | Greater -> comparison_order left right > 0
    | Less_equal -> comparison_order left right <= 0
  in
  if predicate then 1L else 0L

let logical_bits operation left right =
  let left = not (Int64.equal left.bits 0L) in
  let right = not (Int64.equal right.bits 0L) in
  let predicate =
    match operation with
    | Logical_and -> left && right
    | Logical_or -> left || right
    | Logical_xor -> left <> right
  in
  if predicate then 1L else 0L

let divide_bits ~opcode ~remainder result_type left right =
  if Int64.equal right.bits 0L then
    Error ("HCIRVM0009", opcode ^ " divisor is zero")
  else if
    result_type = I64
    && Int64.equal left.bits Int64.min_int
    && Int64.equal right.bits (-1L)
  then
    (* BackA.HC:ICDiv and ICMod both execute IDIV. Its quotient overflows
       even when only the remainder would be consumed. *)
    Error ("HCIRVM0010", opcode ^ " signed quotient overflows I64")
  else
    let operation =
      match (result_type, remainder) with
      | I64, false -> Int64.div
      | I64, true -> Int64.rem
      | U64, false -> Int64.unsigned_div
      | U64, true -> Int64.unsigned_rem
    in
    Ok (operation left.bits right.bits)

let binary_bits ?(compound = false) operation left right result_type =
  match operation with
  | Divide ->
      divide_bits
        ~opcode:(if compound then "IC_DIV_EQU" else "IC_DIV")
        ~remainder:false result_type left right
  | Remainder ->
      divide_bits
        ~opcode:(if compound then "IC_MOD_EQU" else "IC_MOD")
        ~remainder:true result_type left right
  | Add -> Ok (Int64.add left.bits right.bits)
  | Subtract -> Ok (Int64.sub left.bits right.bits)
  | Multiply -> Ok (Int64.mul left.bits right.bits)
  | Bitwise_and -> Ok (Int64.logand left.bits right.bits)
  | Bitwise_or -> Ok (Int64.logor left.bits right.bits)
  | Bitwise_xor -> Ok (Int64.logxor left.bits right.bits)
  | Shift_left -> Ok (Int64.shift_left left.bits (shift_count right.bits))
  | Shift_right ->
      let shift =
        match result_type with
        | I64 -> Int64.shift_right
        | U64 -> Int64.shift_right_logical
      in
      Ok (shift left.bits (shift_count right.bits))
  | Compare comparison -> Ok (comparison_bits comparison left right)
  | Logical logical -> Ok (logical_bits logical left right)

let malformed block_id description =
  preflight_error block_id description "HCIRVM0004"
    (Printf.sprintf "%s has malformed operands, result, target type, or payload"
       (Opcode.to_source_name description.Sequence.opcode))

let unsupported_type block_id description =
  preflight_error block_id description "HCIRVM0005"
    (Printf.sprintf "%s uses an unsupported word type"
       (Opcode.to_source_name description.Sequence.opcode))

let invalid_type_matrix block_id description =
  preflight_error block_id description "HCIRVM0006"
    (Printf.sprintf "%s has an invalid operand/result word-type relationship"
       (Opcode.to_source_name description.Sequence.opcode))

let prepare_instruction ?frame ?globals ?initialization ?(allow_public = false)
    block_index types block_id (description : Sequence.description) =
  let kind =
    match (frame, description.opcode) with
    | _, (Opcode.Ic_imm_i64 | Opcode.Ic_abs_addr)
      when Option.is_some globals
           &&
           match description.payload with
           | Some (Sequence.Symbol _) -> true
           | _ -> false -> Some Global_address_kind
    | Some _, Opcode.Ic_rbp -> Some Frame_address_kind
    | Some _, (Opcode.Ic_imm_i64 | Opcode.Ic_add)
      when Option.fold ~none:false
             ~some:(fun type_ -> Type.pointer_depth type_ > 0)
             description.target_type -> Some Frame_address_kind
    | _, Opcode.Ic_deref when Option.is_some frame || Option.is_some globals ->
        Some Load_slot_kind
    | _, Opcode.Ic_assign when Option.is_some frame || Option.is_some globals ->
        Some Store_slot_kind
    | _, opcode
      when (Option.is_some frame || Option.is_some globals)
           && Option.is_some (update_kind opcode) -> update_kind opcode
    | _ -> opcode_kind description.opcode
  in
  match kind with
  | None ->
      Error
        (preflight_error block_id description "HCIRVM0002"
           (Printf.sprintf "%s is outside the bounded integer interpreter"
              (Opcode.to_source_name description.opcode)))
  | Some kind ->
      let required_flags =
        match kind with
        | Discard_kind -> 0x000000200L
        | _ -> 0L
      in
      if description.flags <> required_flags then
        Error
          (preflight_error block_id description "HCIRVM0003"
             (Printf.sprintf "%s requires flags=0x%09Lx"
                (Opcode.to_source_name description.opcode)
                required_flags))
      else
        let operation =
          match kind with
          | Global_address_kind -> (
              match (description.operands, description.result) with
              | [], Some result -> (
                  match Value_map.find_opt result.value_id types with
                  | Some (Global_address _) -> Ok Frame_address_tick
                  | _ -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Frame_address_kind -> (
              match description.result with
              | Some result -> (
                  match
                    ( description.opcode,
                      description.operands,
                      description.payload,
                      Value_map.find_opt result.value_id types )
                  with
                  | Opcode.Ic_rbp, [], None, Some (Frame_base _)
                  | ( Opcode.Ic_imm_i64,
                      [],
                      Some (Sequence.Integer _),
                      Some (Frame_offset _) )
                  | Opcode.Ic_add, [ _; _ ], None, Some (Frame_address _) ->
                      Ok Frame_address_tick
                  | _ -> Error (malformed block_id description))
              | None -> Error (malformed block_id description))
          | Load_slot_kind
          | Store_slot_kind
          | Update_slot_kind _
          | Increment_slot_kind _ -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | address :: operands, Some result, Some target_type, None -> (
                  let slot =
                    match (frame, Value_map.find_opt address types) with
                    | Some context, Some (Frame_address index) ->
                        let slot = context.slots.(index) in
                        Some (Frame_slot index, slot.slot_type, slot.word_type)
                    | _, Some (Global_address slot)
                      when storage_allowed frame initialization
                             description.instruction_id slot -> (
                        let type_ = Integer_globals.storage_type slot in
                        match return_word_type type_ with
                        | Some word_type ->
                            Some
                              ( Global_slot (Integer_globals.storage_index slot),
                                type_,
                                word_type )
                        | None -> None)
                    | _ -> None
                  in
                  match slot with
                  | Some (location, slot_type, word_type)
                    when Type.equal slot_type target_type -> (
                      match (kind, operands) with
                      | Load_slot_kind, [] ->
                          Ok (Load_slot (location, result.value_id))
                      | Store_slot_kind, [ operand ] -> (
                          match operand_of_value types operand with
                          | Some operand ->
                              Ok
                                (Store_slot
                                   ( location,
                                     operand,
                                     result.value_id,
                                     word_type ))
                          | None ->
                              Error (invalid_type_matrix block_id description))
                      | Update_slot_kind operation, [ operand ] -> (
                          match operand_of_value types operand with
                          | Some operand ->
                              Ok
                                (Update_slot
                                   ( location,
                                     operation,
                                     Some operand,
                                     false,
                                     result.value_id,
                                     word_type ))
                          | None ->
                              Error (invalid_type_matrix block_id description))
                      | Increment_slot_kind (operation, old_result), [] ->
                          Ok
                            (Update_slot
                               ( location,
                                 operation,
                                 None,
                                 old_result,
                                 result.value_id,
                                 word_type ))
                      | _ -> Error (malformed block_id description))
                  | _ -> Error (invalid_type_matrix block_id description))
              | _ -> Error (malformed block_id description))
          | Immediate_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], Some result, Some type_, Some (Sequence.Integer bits) -> (
                  match producer_word_type type_ with
                  | Some type_ ->
                      Ok (Immediate (result.value_id, { type_; bits }))
                  | None -> Error (unsupported_type block_id description))
              | _ -> Error (malformed block_id description))
          | Unary_kind unary -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], Some result, Some result_type, None -> (
                  match
                    scalar_word_type
                      ~allow_public:
                        (Option.is_some frame || Option.is_some globals
                       || allow_public)
                      result_type
                  with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match operand_of_value types operand_id with
                      | None -> Error (invalid_type_matrix block_id description)
                      | Some operand ->
                          let valid =
                            Option.fold ~none:false
                              ~some:(valid_unary_type types unary operand_id)
                              description.target_type
                          in
                          if valid then
                            Ok
                              (Unary
                                 (unary, operand, result.value_id, result_type))
                          else Error (invalid_type_matrix block_id description))
                  )
              | _ -> Error (malformed block_id description))
          | Word_view_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | ( [ operand_id ],
                  Some result,
                  Some result_type,
                  Some (Sequence.Integer (0L | 1L)) ) -> (
                  match producer_word_type result_type with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match operand_of_value types operand_id with
                      | None -> Error (invalid_type_matrix block_id description)
                      | Some operand ->
                          Ok (Word_view (operand, result.value_id, result_type))
                      ))
              | _ -> Error (malformed block_id description))
          | Binary_kind binary -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ left_id; right_id ], Some result, Some result_type, None -> (
                  match
                    scalar_word_type
                      ~allow_public:
                        (Option.is_some frame || Option.is_some globals
                       || allow_public)
                      result_type
                  with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match
                        ( operand_of_value types left_id,
                          operand_of_value types right_id )
                      with
                      | Some left, Some right
                        when result_type
                             = expected_binary_result_type binary
                                 left.expected_type right.expected_type ->
                          Ok
                            (Binary
                               ( binary,
                                 left,
                                 right,
                                 result.value_id,
                                 result_type ))
                      | Some _, Some _ | None, _ | _, None ->
                          Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Discard_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, None, None -> (
                  match operand_of_value types operand_id with
                  | Some operand -> Ok (Discard operand)
                  | None -> Error (invalid_type_matrix block_id description))
              | _ -> Error (malformed block_id description))
          | Return_value_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, Some target_type, None
                when Option.fold ~none:true
                       ~some:(fun context ->
                         Type.equal context.return_type target_type)
                       frame -> (
                  match return_word_type target_type with
                  | None -> Error (unsupported_type block_id description)
                  | Some target_type -> (
                      match operand_of_value types operand_id with
                      | Some operand
                        when Option.is_some frame
                             || operand.expected_type = target_type ->
                          Ok (Return_value (operand, target_type))
                      | Some _ | None ->
                          Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Jump_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, Some (Sequence.Block target) -> (
                  match Block_map.find_opt target block_index with
                  | Some target -> Ok (Jump target)
                  | None -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Branch_kind condition -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, None, Some (Sequence.Block target) -> (
                  match
                    ( operand_of_value types operand_id,
                      Block_map.find_opt target block_index )
                  with
                  | Some operand, Some target ->
                      Ok (Branch (condition, operand, target))
                  | None, _ -> Error (invalid_type_matrix block_id description)
                  | Some _, None -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Return_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, None -> Ok Return
              | _ -> Error (malformed block_id description))
          | End_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, None when Option.is_none frame -> Ok End
              | _ -> Error (malformed block_id description))
        in
        Result.map
          (fun operation ->
            {
              instruction_id = description.instruction_id;
              span = description.span;
              operation;
              push_result = None;
            })
          operation

let prepare ?frame ?globals ?initialization ?callees graph =
  let source_blocks = Graph.blocks graph in
  let block_count = List.length source_blocks in
  let block_index =
    source_blocks
    |> List.mapi (fun index block -> (Graph.block_id block, index))
    |> List.fold_left
         (fun map (block_id, index) -> Block_map.add block_id index map)
         Block_map.empty
  in
  let errors_rev = ref [] in
  let blocks =
    source_blocks
    |> List.mapi (fun index block ->
        let block_id = Graph.block_id block in
        let types =
          declared_types ?frame ?globals ?initialization
            ~allow_calls:(Option.is_some callees) block
        in
        let instructions_rev = ref [] in
        let calls = ref [] in
        let call_error description message =
          preflight_error block_id description "HCIRVM0014" message
        in
        let call_instruction description operation =
          Ok
            {
              instruction_id = description.Sequence.instruction_id;
              span = description.span;
              operation;
              push_result = None;
            }
        in
        let prepare_call (description : Sequence.description) =
          let no_operands =
            description.operands = [] && description.flags = 0L
          in
          let target_matches callee =
            Option.fold ~none:false
              ~some:(Type.equal callee.callee_return_type)
              description.target_type
          in
          match (description.opcode, !calls) with
          | Opcode.Ic_call_start, stack
            when no_operands && description.result = None
                 && description.target_type = None -> (
              match description.payload with
              | Some (Sequence.Symbol symbol) -> (
                  match
                    Option.value callees ~default:[]
                    |> List.find_opt (fun callee ->
                        callee.callee_symbol == symbol)
                  with
                  | Some callee
                    when match stack with
                         | [] | { phase = Collecting _; _ } :: _ -> true
                         | _ -> false ->
                      calls := { callee; phase = Collecting 0 } :: stack;
                      call_instruction description Call_start
                  | _ ->
                      Error
                        (call_error description
                           "direct call has no matching executable definition \
                            or valid enclosing call"))
              | _ -> Error (malformed block_id description))
          | Opcode.Ic_call, { callee; phase = Collecting count } :: rest
            when no_operands && description.result = None
                 && target_matches callee -> (
              match description.payload with
              | Some (Sequence.Symbol symbol)
                when symbol == callee.callee_symbol
                     && count = Array.length callee.parameter_types ->
                  calls := { callee; phase = Needs_cleanup } :: rest;
                  call_instruction description (Call callee.callee_index)
              | _ ->
                  Error
                    (call_error description
                       "call target or pushed argument count disagrees with \
                        its definition"))
          | ( (Opcode.Ic_add_rsp | Opcode.Ic_add_rsp1),
              { callee; phase = Needs_cleanup } :: rest )
            when no_operands && description.result = None
                 && target_matches callee
                 && description.opcode = callee.cleanup_opcode -> (
              match description.payload with
              | Some (Sequence.Integer bytes)
                when bytes
                     = Int64.mul 8L
                         (Int64.of_int (Array.length callee.parameter_types)) ->
                  calls := { callee; phase = Needs_end } :: rest;
                  call_instruction description Call_cleanup
              | _ ->
                  Error
                    (call_error description
                       "call cleanup does not match its fixed argument slots"))
          | Opcode.Ic_call_end, { callee; phase = Needs_end } :: rest
            when no_operands && target_matches callee -> (
              match
                ( description.payload,
                  description.result,
                  return_word_type callee.callee_return_type )
              with
              | Some (Sequence.Symbol symbol), Some result, Some type_
                when symbol == callee.callee_symbol ->
                  calls := rest;
                  call_instruction description
                    (Call_end (result.value_id, type_))
              | _ ->
                  Error
                    (call_error description
                       "call end does not match its checked target and result"))
          | ( ( Opcode.Ic_call_start
              | Opcode.Ic_call
              | Opcode.Ic_call_end
              | Opcode.Ic_add_rsp
              | Opcode.Ic_add_rsp1 ),
              _ ) ->
              Error
                (call_error description
                   "direct call instructions have an invalid order, type or \
                    shape")
          | _, { phase = Needs_cleanup | Needs_end; _ } :: _ ->
              Error
                (call_error description
                   "direct call cleanup and call end must follow the call")
          | _ ->
              prepare_instruction ?frame ?globals ?initialization
                ~allow_public:true block_index types block_id description
        in
        Graph.instructions block |> Sequence.instructions
        |> List.iter (fun instruction ->
            let description = Sequence.description instruction in
            let pushes =
              Option.is_some callees
              && Int64.logand description.flags 0x2000L <> 0L
            in
            let checked_description =
              if pushes then
                {
                  description with
                  flags = Int64.logand description.flags (Int64.lognot 0x2000L);
                }
              else description
            in
            match
              if Option.is_some callees then prepare_call checked_description
              else
                prepare_instruction ?frame ?globals ?initialization block_index
                  types block_id description
            with
            | Ok prepared ->
                let control_transfer =
                  match prepared.operation with
                  | Jump _ | Branch _ | Return | End -> true
                  | _ -> false
                in
                if control_transfer && !calls <> [] then
                  errors_rev :=
                    call_error description
                      "call protocol cannot cross a basic-block boundary"
                    :: !errors_rev;
                let push_result =
                  if not pushes then None
                  else
                    match (description.result, !calls) with
                    | ( Some result,
                        ({ callee; phase = Collecting count } as call) :: rest )
                      when count < Array.length callee.parameter_types -> (
                        match operand_of_value types result.value_id with
                        | Some operand ->
                            calls :=
                              { call with phase = Collecting (count + 1) }
                              :: rest;
                            Some operand
                        | None ->
                            errors_rev :=
                              call_error description
                                "pushed argument is not a checked integer word"
                              :: !errors_rev;
                            None)
                    | _ ->
                        errors_rev :=
                          call_error description
                            "argument push has no matching fixed parameter"
                          :: !errors_rev;
                        None
                in
                instructions_rev :=
                  { prepared with push_result } :: !instructions_rev
            | Error error -> errors_rev := error :: !errors_rev);
        if !calls <> [] then
          errors_rev :=
            make_error ~stage:Preflight ~executed_steps:0 ~block_id "HCIRVM0014"
              "basic block ends inside an incomplete direct call"
            :: !errors_rev;
        {
          block_id;
          instructions = Array.of_list (List.rev !instructions_rev);
          fallthrough =
            (if index + 1 < block_count then Some (index + 1) else None);
        })
    |> Array.of_list
  in
  match List.rev !errors_rev with
  | _ :: _ as errors -> Error errors
  | [] -> (
      let entry_id = Graph.entry graph |> Graph.block_id in
      match Block_map.find_opt entry_id block_index with
      | Some entry_index ->
          let initial_slots =
            Option.fold ~none:[||]
              ~some:(fun context ->
                Array.map (fun slot -> slot.initial) context.slots)
              frame
          in
          Ok
            {
              blocks;
              entry_index;
              initial_slots;
              is_function = Option.is_some frame;
              owner = None;
            }
      | None ->
          Error
            [
              make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0004"
                "the verified graph entry is unavailable";
            ])

let runtime_error ?instruction block executed_steps code message =
  match instruction with
  | None ->
      make_error ~stage:Execution ~executed_steps ~block_id:block.block_id code
        message
  | Some instruction ->
      make_error ~stage:Execution ~executed_steps ~block_id:block.block_id
        ~instruction_id:instruction.instruction_id ?span:instruction.span code
        message

type call_scope = { arguments_rev : word list; returned_word : word option }

type caller = {
  saved_program : prepared;
  saved_block : int;
  saved_instruction : int;
  saved_values : word Value_map.t;
  saved_slots : word option array;
  saved_return : word option;
  saved_calls : call_scope list;
}

let execute_prepared ?(callees = [||]) ?(max_frame_bytes = Int.max_int)
    ?(max_call_depth = Int.max_int) ?(capture_last = false) ?initialization
    ?(global_words = [||]) ~max_steps program =
  let current_block = ref program.entry_index in
  let current_instruction = ref 0 in
  let values = ref Value_map.empty in
  let slots = ref (Array.copy program.initial_slots) in
  let program = ref program in
  let callers = ref [] in
  let calls = ref [] in
  let depth = ref 0 in
  let live_frame_bytes = ref (Array.length !slots * 8) in
  let final_value = ref None in
  let pending_return = ref None in
  let steps = ref 0 in
  let completed = ref None in
  let failed = ref None in
  let active_initializer = ref None in
  let transfer target =
    current_block := target;
    current_instruction := 0;
    values := Value_map.empty
  in
  let require_operand block instruction operand =
    match Value_map.find_opt operand.value_id !values with
    | Some word when word.type_ = operand.expected_type -> Some word
    | Some _ | None ->
        failed :=
          Some
            (runtime_error ~instruction block !steps "HCIRVM0008"
               "a prepared operand is unavailable or has the wrong word type");
        None
  in
  while Option.is_none !completed && Option.is_none !failed do
    if !current_block < 0 || !current_block >= Array.length !program.blocks then
      failed :=
        Some
          (make_error ~stage:Execution ~executed_steps:!steps "HCIRVM0008"
             "the prepared block cursor is out of bounds")
    else
      let block = !program.blocks.(!current_block) in
      if !current_instruction >= Array.length block.instructions then
        match block.fallthrough with
        | Some target -> transfer target
        | None ->
            failed :=
              Some
                (runtime_error block !steps "HCIRVM0008"
                   "execution reached an impossible final-block fallthrough")
      else
        let instruction = block.instructions.(!current_instruction) in
        let () =
          if not !program.is_function then
            active_initializer :=
              Option.bind initialization (fun context ->
                  Global_initialization.find_storage context
                    instruction.instruction_id)
        in
        if !steps >= max_steps then
          failed :=
            Some
              (runtime_error ~instruction block !steps "HCIRVM0007"
                 "the bounded integer execution step limit was exhausted")
        else (
          steps := !steps + 1;
          current_instruction := !current_instruction + 1;
          (match instruction.operation with
          | Call_start ->
              calls := { arguments_rev = []; returned_word = None } :: !calls
          | Call_cleanup -> ()
          | Call index -> (
              match !calls with
              | scope :: _ when index >= 0 && index < Array.length callees ->
                  let callee, body = callees.(index) in
                  if !depth >= max_call_depth then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0015"
                           "the integer call depth limit was exhausted")
                  else if
                    callee.frame_bytes > max_frame_bytes - !live_frame_bytes
                  then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0011"
                           "the active function frames exceed the frame byte \
                            limit")
                  else (
                    callers :=
                      {
                        saved_program = !program;
                        saved_block = !current_block;
                        saved_instruction = !current_instruction;
                        saved_values = !values;
                        saved_slots = !slots;
                        saved_return = !pending_return;
                        saved_calls = !calls;
                      }
                      :: !callers;
                    incr depth;
                    live_frame_bytes := !live_frame_bytes + callee.frame_bytes;
                    let initialized = Array.copy body.initial_slots in
                    scope.arguments_rev
                    |> List.iteri (fun position word ->
                        initialized.(position) <-
                          Some
                            {
                              type_ = callee.parameter_types.(position);
                              bits = word.bits;
                            });
                    program := body;
                    slots := initialized;
                    values := Value_map.empty;
                    pending_return := None;
                    calls := [];
                    current_block := body.entry_index;
                    current_instruction := 0)
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared direct call has no available caller scope"))
          | Call_end (result, type_) -> (
              match !calls with
              | { returned_word = Some word; _ } :: rest when word.type_ = type_
                ->
                  calls := rest;
                  values := Value_map.add result word !values
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "direct call did not supply its declared return word"))
          | Frame_address_tick -> ()
          | Load_slot (location, result) -> (
              let storage, index, message =
                match location with
                | Frame_slot index ->
                    ( !slots,
                      index,
                      "the reached frame slot has not been initialized" )
                | Global_slot index ->
                    ( global_words,
                      index,
                      "hosted execution reached an uninitialized JIT \
                       persistent object" )
              in
              match storage.(index) with
              | Some word -> values := Value_map.add result word !values
              | None ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0012"
                         message))
          | Store_slot (location, operand, result, type_) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some operand ->
                  let word = { type_; bits = operand.bits } in
                  let storage, index =
                    match location with
                    | Frame_slot index -> (!slots, index)
                    | Global_slot index -> (global_words, index)
                  in
                  storage.(index) <- Some word;
                  values := Value_map.add result word !values)
          | Update_slot (location, operation, operand, old_result, result, type_)
            -> (
              let right =
                match operand with
                | None -> Some { type_; bits = 1L }
                | Some operand -> require_operand block instruction operand
              in
              match right with
              | None -> ()
              | Some right -> (
                  let storage, index, message =
                    match location with
                    | Frame_slot index ->
                        ( !slots,
                          index,
                          "the reached frame slot has not been initialized" )
                    | Global_slot index ->
                        ( global_words,
                          index,
                          "hosted execution reached an uninitialized JIT \
                           persistent object" )
                  in
                  match storage.(index) with
                  | None ->
                      failed :=
                        Some
                          (runtime_error ~instruction block !steps "HCIRVM0012"
                             message)
                  | Some old -> (
                      (* BackA.HC/BackB.HC read at the update, after the RHS.
                         Failed arithmetic must not publish a store or result. *)
                      match
                        binary_bits ~compound:true operation old right type_
                      with
                      | Error (code, message) ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps code
                                 message)
                      | Ok bits ->
                          let word = { type_; bits } in
                          storage.(index) <- Some word;
                          values :=
                            Value_map.add result
                              (if old_result then old else word)
                              !values)))
          | Immediate (result, word) ->
              values := Value_map.add result word !values
          | Unary (operation, operand, result, result_type) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some operand ->
                  let bits =
                    match operation with
                    | Complement -> Int64.lognot operand.bits
                    | Logical_not ->
                        if Int64.equal operand.bits 0L then 1L else 0L
                    | Negate -> Int64.neg operand.bits
                  in
                  values :=
                    Value_map.add result { type_ = result_type; bits } !values)
          | Word_view (operand, result, result_type) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some operand ->
                  values :=
                    Value_map.add result
                      { type_ = result_type; bits = operand.bits }
                      !values)
          | Binary (operation, left, right, result, result_type) -> (
              match require_operand block instruction left with
              | None -> ()
              | Some left -> (
                  match require_operand block instruction right with
                  | None -> ()
                  | Some right -> (
                      match binary_bits operation left right result_type with
                      | Ok bits ->
                          values :=
                            Value_map.add result
                              { type_ = result_type; bits }
                              !values
                      | Error (code, message) ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps code
                                 message))))
          | Discard operand ->
              let value = require_operand block instruction operand in
              if
                capture_last && (not !program.is_function)
                && Option.is_none !active_initializer
              then final_value := value
          | Return_value (operand, type_) -> (
              match require_operand block instruction operand with
              | Some word -> pending_return := Some { type_; bits = word.bits }
              | None -> ())
          | Jump target -> transfer target
          | Branch (condition, operand, target) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some word -> (
                  let is_zero = Int64.equal word.bits 0L in
                  let take_target =
                    match condition with
                    | Zero -> is_zero
                    | Not_zero -> not is_zero
                  in
                  if take_target then transfer target
                  else
                    match block.fallthrough with
                    | Some fallthrough -> transfer fallthrough
                    | None ->
                        failed :=
                          Some
                            (runtime_error ~instruction block !steps
                               "HCIRVM0008"
                               "a conditional branch has no physical \
                                fallthrough")))
          | Return when !program.is_function && Option.is_none !pending_return
            ->
              failed :=
                Some
                  (runtime_error ~instruction block !steps "HCIRVM0013"
                     "the integer function returned without a value")
          | Return -> (
              match !callers with
              | [] -> completed := Some (Returned !pending_return)
              | caller :: rest -> (
                  let returned_word = !pending_return in
                  callers := rest;
                  decr depth;
                  live_frame_bytes :=
                    !live_frame_bytes - (Array.length !slots * 8);
                  program := caller.saved_program;
                  current_block := caller.saved_block;
                  current_instruction := caller.saved_instruction;
                  values := caller.saved_values;
                  slots := caller.saved_slots;
                  pending_return := caller.saved_return;
                  calls :=
                    match caller.saved_calls with
                    | scope :: rest -> { scope with returned_word } :: rest
                    | [] -> []))
          | End -> completed := Some Stream_end);
          if Option.is_none !failed then
            Option.iter
              (fun operand ->
                match (require_operand block instruction operand, !calls) with
                | Some word, scope :: rest ->
                    calls :=
                      { scope with arguments_rev = word :: scope.arguments_rev }
                      :: rest
                | _ ->
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0008"
                           "prepared argument push has no active call"))
              instruction.push_result)
  done;
  match (!failed, !completed) with
  | Some error, _ ->
      let error =
        match !program.owner with
        | None -> error
        | Some (function_id, function_name) ->
            {
              error with
              function_id = Some function_id;
              function_name = Some function_name;
            }
      in
      Error [ identify_initializer !active_initializer error ]
  | None, Some termination ->
      Ok
        {
          termination_ = termination;
          executed_steps_ = !steps;
          final_value_ = !final_value;
          compiled_initializer_steps_ =
            Option.fold ~none:0 ~some:Global_initialization.prepared_steps
              initialization;
        }
  | None, None ->
      Error
        [
          make_error ~stage:Execution ~executed_steps:!steps "HCIRVM0008"
            "bounded integer execution stopped without a result";
        ]

let execute ~max_steps checked =
  if max_steps <= 0 then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps must be greater than zero";
      ]
  else
    match prepare (X87.graph checked) with
    | Error errors -> Error errors
    | Ok program -> execute_prepared ~max_steps program

let execute_function ~max_steps ~max_frame_bytes ~frame ~arguments function_ =
  if max_steps <= 0 || max_frame_bytes <= 0 then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps and max_frame_bytes must be greater than zero";
      ]
  else
    match frame_context ~max_frame_bytes ~frame ~arguments function_ with
    | Error errors -> Error errors
    | Ok frame -> (
        match prepare ~frame (Function.body function_) with
        | Error errors -> Error errors
        | Ok program ->
            execute_prepared ~max_steps
              {
                program with
                owner =
                  Some
                    ( Function.Function_id.to_int
                        (Function.function_id function_),
                      Sema.Symbol.name (Function.symbol function_) );
              })

let execute_program ?globals ?initialization ?(max_global_bytes = 1_048_576)
    ~max_steps ~max_frame_bytes ~max_call_depth ~functions checked =
  let ( let* ) = Result.bind in
  if
    max_steps <= 0 || max_frame_bytes <= 0 || max_call_depth <= 0
    || max_global_bytes <= 0
  then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps, max_frame_bytes, max_call_depth and max_global_bytes \
           must be greater than zero";
      ]
  else if
    Option.fold ~none:false
      ~some:(fun globals ->
        Integer_globals.byte_size globals > max_global_bytes)
      globals
  then
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0016"
          "program global storage exceeds the global byte limit";
      ]
  else if
    match (globals, initialization) with
    | Some globals, Some context ->
        not (Global_initialization.matches context ~globals ~entry:checked)
    | None, Some _ -> true
    | Some globals, None -> Integer_globals.has_initializers globals
    | None, None -> false
  then
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0017"
          "global initializer execution requires its matching checked \
           initialization context";
      ]
  else
    let owner body =
      ( Function.Function_id.to_int (Function.function_id body),
        Sema.Symbol.name (Function.symbol body) )
    in
    let identify body error =
      let function_id, function_name = owner body in
      {
        error with
        function_id = Some function_id;
        function_name = Some function_name;
      }
    in
    let rec summaries index symbols ids rev = function
      | [] -> Ok (List.rev rev)
      | ({ frame; body } : function_definition) :: rest ->
          let symbol = Function.symbol body in
          let function_id =
            Function.Function_id.to_int (Function.function_id body)
          in
          let parameter_count = List.length (Function.parameters body) in
          if
            List.exists
              (fun other ->
                Sema.Symbol.Id.equal (Sema.Symbol.id other)
                  (Sema.Symbol.id symbol))
              symbols
            || List.mem function_id ids
          then
            Error
              [
                identify body
                  (make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0014"
                     "integer program definitions have duplicate function or \
                      symbol identities");
              ]
          else if parameter_count > max_frame_bytes / 8 then
            Error
              [
                identify body
                  (make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0011"
                     "function parameters exceed the frame byte limit");
              ]
          else
            let* context =
              frame_context ?globals ~max_frame_bytes ~frame
                ~arguments:(List.init parameter_count (fun _ -> 0L))
                body
              |> Result.map_error (List.map (identify body))
            in
            let callee =
              {
                callee_index = index;
                callee_symbol = symbol;
                callee_return_type = Function.return_type body;
                parameter_types =
                  Array.init parameter_count (fun position ->
                      context.slots.(position).word_type);
                cleanup_opcode =
                  (if
                     Sema.Function_flag.caller_expects_callee_pop
                       ~stored_mask:(Function.stored_flags body)
                   then Opcode.Ic_add_rsp1
                   else Opcode.Ic_add_rsp);
                frame_bytes = Array.length context.slots * 8;
              }
            in
            summaries (index + 1) (symbol :: symbols) (function_id :: ids)
              ((callee, context, body) :: rev)
              rest
    in
    let* summaries = summaries 0 [] [] [] functions in
    let calls ?caller graph =
      Graph.blocks graph
      |> List.concat_map (fun block ->
          Graph.instructions block |> Sequence.instructions
          |> List.filter_map (fun instruction ->
              let description = Sequence.description instruction in
              match (description.opcode, description.payload) with
              | Opcode.Ic_call, Some (Sequence.Symbol symbol) ->
                  Some (Graph.block_id block, description, symbol, caller)
              | _ -> None))
    in
    let rec available region declaring_index visited = function
      | [] -> Ok ()
      | (_, _, symbol, _) :: rest
        when List.exists (fun prior -> prior == symbol) visited ->
          available region declaring_index visited rest
      | (block_id, description, symbol, caller) :: rest -> (
          match
            List.find_opt
              (fun (callee, _, _) -> callee.callee_symbol == symbol)
              summaries
          with
          | Some (_, context, body)
            when Frame.function_item_index context.layout < declaring_index ->
              available region declaring_index (symbol :: visited)
                (calls ~caller:body (Function.body body) @ rest)
          | _ ->
              let error =
                preflight_error block_id description "HCIRVM0017"
                  "JIT static initializer calls a function whose definition is \
                   not yet published"
              in
              let error =
                Option.fold ~none:error
                  ~some:(fun body -> identify body error)
                  caller
              in
              Error [ identify_initializer (Some region) error ])
    in
    let* () =
      Option.fold ~none:[] ~some:Global_initialization.storage_regions
        initialization
      |> List.fold_left
           (fun result region ->
             let* () = result in
             match
               ( Global_initialization.storage_phase region,
                 Global_initialization.storage_frame region )
             with
             | Global_initialization.Compile_initializer, Some frame ->
                 let called =
                   calls (X87.graph checked)
                   |> List.filter (fun (_, description, _, _) ->
                       Instruction_id.compare
                         description.Sequence.instruction_id
                         (Global_initialization.storage_first region)
                       >= 0
                       && Instruction_id.compare description.instruction_id
                            (Global_initialization.storage_last region)
                          <= 0)
                 in
                 available region (Frame.function_item_index frame) [] called
             | _ -> Ok ())
           (Ok ())
    in
    let callees = List.map (fun (callee, _, _) -> callee) summaries in
    let rec bodies rev = function
      | [] -> Ok (Array.of_list (List.rev rev))
      | (callee, frame, body) :: rest ->
          let* program =
            prepare ~frame ?globals ~callees (Function.body body)
            |> Result.map_error (List.map (identify body))
          in
          bodies
            ((callee, { program with owner = Some (owner body) }) :: rev)
            rest
    in
    let* programs = bodies [] summaries in
    let* entry =
      prepare ?globals ?initialization ~callees (X87.graph checked)
      |> Result.map_error
           (List.map (fun (error : error) ->
                let region =
                  Option.bind initialization (fun context ->
                      Option.bind error.instruction_id (fun id ->
                          match Instruction_id.of_int id with
                          | Ok id ->
                              Global_initialization.find_storage context id
                          | Error _ -> None))
                in
                identify_initializer region error))
    in
    let global_words =
      Option.fold ~none:[] ~some:Integer_globals.storage_slots globals
      |> List.map (fun slot ->
          Option.map
            (fun bits ->
              let type_ =
                match return_word_type (Integer_globals.storage_type slot) with
                | Some type_ -> type_
                | None -> assert false
              in
              { type_; bits })
            (Integer_globals.storage_initial_bits slot))
      |> Array.of_list
    in
    execute_prepared ~callees:programs ~max_frame_bytes ~max_call_depth
      ?initialization ~global_words ~capture_last:true ~max_steps entry

let termination execution = execution.termination_
let executed_steps execution = execution.executed_steps_
let final_value execution = execution.final_value_
let compiled_initializer_steps execution = execution.compiled_initializer_steps_

let word_type_name = function
  | I64 -> "i64"
  | U64 -> "u64"

let termination_name = function
  | Stream_end -> "stream-end"
  | Returned None -> "returned:none"
  | Returned (Some word) ->
      Printf.sprintf "returned:%s:0x%016Lx"
        (word_type_name word.type_)
        word.bits

let human execution =
  Printf.sprintf
    "holyc-ir-integer-execution-v1 reference=%s\nsteps=%d\ntermination=%s\n"
    reference_commit execution.executed_steps_
    (termination_name execution.termination_)
