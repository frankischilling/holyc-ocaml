module Sequence = Instruction_sequence
module Graph = Block_graph
module X87 = X87_stack
module Block_id = Sequence.Block_id
module Instruction_id = Sequence.Instruction_id
module Value_id = Sequence.Value_id
module Frame = Sema.Function_frame_layout
module Function = Function_body
module Type = Sema.Type
module Runtime = Runtime_call_context
module Output = Integer_output
module Offset_map = Map.Make (Int64)

module Value_map = Map.Make (struct
  type t = Value_id.t

  let compare = Value_id.compare
end)

module Block_map = Map.Make (struct
  type t = Block_id.t

  let compare = Block_id.compare
end)

module Instruction_map = Map.Make (struct
  type t = Instruction_id.t

  let compare = Instruction_id.compare
end)

type word_type = I64 | U64
type word = { type_ : word_type; bits : int64 }
type return_kind = Word_return of word_type | Void_return
type function_definition = { frame : Frame.function_layout; body : Function.t }

type stored_type =
  | Stored_word of word_type
  | Stored_byte
  | Stored_pointer of Type.t

type runtime_value =
  | Runtime_word of word
  | Runtime_pointer of runtime_address
  | Runtime_offset of int64
  | Runtime_void

and runtime_address = {
  pointer_storage : runtime_storage;
  pointer_base : int;
  pointer_count : int;
  pointer_element_bytes : int;
  pointer_extent_bytes : int64;
  pointer_offset : int64;
  pointer_pointee : Type.t;
}

and runtime_storage = {
  cells : runtime_value option array;
  mutable live : bool;
  unknown_message : string;
}

type frame_slot = {
  slot_type : Type.t;
  stored_type : stored_type;
  initial : runtime_value option;
  object_count : int;
  strides : int64 list;
}

type frame_context = {
  layout : Frame.function_layout;
  slots : frame_slot array;
  offsets : int Offset_map.t;
  return_type : Type.t;
  allocated_bytes : int;
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

type report = {
  outcome_ : (t, error list) result;
  output_bytes_ : string;
  output_work_ : int;
}

let report_outcome report = report.outcome_
let report_output_bytes report = report.output_bytes_
let report_output_work report = report.output_work_

type literal_region = { literal_base : int; literal_count : int }

type literal_context = {
  literal_graph : Graph.t;
  literal_regions : literal_region Instruction_map.t;
}

type literal_image = {
  mutable literal_byte_count : int;
  mutable literal_chunks_rev : (int * string) list;
}

type prepared_operand = { value_id : Value_id.t; expected_type : word_type }
type prepared_pointer = { pointer_value : Value_id.t; pointer_type : Type.t }

type prepared_value =
  | Word_operand of prepared_operand
  | Pointer_operand of prepared_pointer

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

type storage_location =
  | Frame_slot of int * int
  | Global_slot of int
  | Literal_slot of int * int
  | Indirect_slot of prepared_pointer
  | Indexed_slot of prepared_pointer

type prepared_operation =
  | Call_start
  | Call of int
  | Runtime_call of Runtime.call * stored_type array
  | Call_cleanup
  | Call_end of Value_id.t * word_type
  | Call_end_void of Value_id.t
  | Frame_address_tick
  | Scale_index of prepared_operand * int64 * Value_id.t
  | Index_address of storage_location * Value_id.t * Value_id.t * Type.t
  | Materialize_address of storage_location * Value_id.t * Type.t
  | Load_slot of storage_location * Value_id.t
  | Store_slot of storage_location * prepared_value * Value_id.t * stored_type
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
  | Discard of prepared_value
  | Discard_void of Value_id.t
  | Return_value of prepared_operand * word_type
  | Jump of int
  | Branch of branch_condition * prepared_operand * int
  | Return
  | End

type prepared_instruction = {
  instruction_id : Instruction_id.t;
  span : Common.Span.t option;
  operation : prepared_operation;
  push_result : prepared_value option;
  capture_discard : bool;
}

type prepared_block = {
  block_id : Block_id.t;
  instructions : prepared_instruction array;
  fallthrough : int option;
}

type prepared = {
  blocks : prepared_block array;
  entry_index : int;
  initial_slots : runtime_value option array;
  initial_frame_bytes : int;
  is_function : bool;
  required_return : return_kind option;
  owner : (int * string) option;
}

type callee = {
  callee_index : int;
  callee_symbol : Sema.Symbol.t;
  callee_definition : Sema.Function_resolution.resolved_declaration option;
  callee_return_type : Type.t;
  parameter_types : stored_type array;
  cleanup_opcode : Opcode.t;
  frame_bytes : int;
}

type call_phase = Collecting of int | Needs_cleanup | Needs_end

type checked_call = {
  callee : callee;
  site : Runtime.call option;
  remaining_arguments : Runtime.argument list option;
  phase : call_phase;
}

type opcode_kind =
  | Literal_address_kind
  | Scale_index_kind
  | Index_address_kind
  | Pointer_address_kind
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
  | Pointer_value of Type.t
  | Supported of word_type * Type.t
  | Void_value
  | Frame_base of Type.t
  | Frame_offset of Type.t * int64
  | Frame_address of int
  | Global_address of Integer_globals.storage_slot
  | Index_offset of Type.t * int64 * prepared_operand
  | Indexed_address of Type.t * int64 list
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

let checked_return_kind type_ =
  match return_word_type type_ with
  | Some word -> Some (Word_return word)
  | None when Type.pointer_depth type_ = 0 -> (
      match Type.base type_ with
      | Type.Primitive (_, Sema.Primitive_type.U0) -> Some Void_return
      | _ -> None)
  | None -> None

(* A byte expression retains its checked raw class and full register bits.
   Only storage narrows it; the public execution result remains I64/U64. *)
let scalar_value_type ~allow_byte ~allow_public type_ =
  match scalar_word_type ~allow_public type_ with
  | Some _ as word -> word
  | None when allow_byte && Type.pointer_depth type_ = 0 -> (
      match Type.base type_ with
      | Type.Primitive (form, Sema.Primitive_type.U8)
        when allow_public || form = Type.Internal_storage -> Some U64
      | _ -> None)
  | None -> None

let scalar_element_bytes type_ =
  if Type.pointer_depth type_ <> 0 then None
  else
    match Type.base type_ with
    | Type.Primitive (_, Sema.Primitive_type.U8) -> Some 1
    | Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) -> Some 8
    | _ -> None

let scalar_pointer_type type_ =
  Type.pointer_depth type_ = 1
  &&
  match Type.base type_ with
  | Type.Primitive (_, (Sema.Primitive_type.I64 | U64 | U8)) -> true
  | _ -> false

let literal_pointer_type type_ =
  Type.pointer_depth type_ = 1
  &&
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, Sema.Primitive_type.U8) -> true
  | _ -> false

let stored_type type_ =
  match return_word_type type_ with
  | Some word -> Some (Stored_word word)
  | None when scalar_element_bytes type_ = Some 1 -> Some Stored_byte
  | None when scalar_pointer_type type_ -> Some (Stored_pointer type_)
  | None -> None

let stored_bytes = function
  | Stored_byte -> 1
  | Stored_word _ | Stored_pointer _ -> 8

let frame_context ?globals ?(pointer_arguments = false) ~max_frame_bytes ~frame
    ~arguments function_ =
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
    || (not (Function.definition_matches_frame function_ frame))
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
  else if Option.is_none (checked_return_kind (Function.return_type function_))
  then invalid "the function return type is outside I64/U64/U0 execution"
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
    let prepared_rev = ref []
    and total_cells = ref 0L
    and total_bytes = ref 0L
    and error = ref None in
    let allocated_bytes = Int64.to_int frame_size + (parameter_count * 8) in
    let max_cells = Int64.of_int (min Sys.max_array_length max_frame_bytes) in
    List.iter
      (fun location ->
        let dimensions = Frame.location_dimensions location in
        let storage_kind = stored_type (Frame.location_checked_type location) in
        let rec array_strides = function
          | [] ->
              Option.map
                (fun kind -> (Int64.of_int (stored_bytes kind), []))
                storage_kind
          | dimension :: rest -> (
              match array_strides rest with
              | Some (bytes, strides) ->
                  let count = Frame.dimension_value dimension in
                  if count <= 0L || count > Int64.div Int64.max_int bytes then
                    None
                  else Some (Int64.mul count bytes, bytes :: strides)
              | None -> None)
        in
        match
          ( storage_kind,
            Frame.location_frame_slot location,
            array_strides dimensions )
        with
        | Some stored_type, Some slot, Some (bytes, strides)
          when Frame.location_declarator_shape location = Frame.Object
               && Frame.location_element_size location
                  = Int64.of_int (stored_bytes stored_type)
               && (Frame.location_kind location <> Frame.Named_parameter
                  || stored_type <> Stored_byte)
               && Frame.location_allocated_size location = bytes
               && Frame.frame_slot_size slot = bytes
               && (dimensions = []
                   && Frame.location_value_shape location = Frame.Scalar
                  || dimensions <> []
                     && Frame.location_value_shape location = Frame.Array
                     && Frame.location_kind location = Frame.Automatic_local
                     &&
                     match stored_type with
                     | Stored_word _ | Stored_byte -> true
                     | _ -> false) ->
            let offset = Frame.frame_slot_displacement slot in
            let count =
              Int64.div bytes (Int64.of_int (stored_bytes stored_type))
            in
            if
              count > Int64.sub max_cells !total_cells
              || bytes > Int64.sub (Int64.of_int allocated_bytes) !total_bytes
            then
              error := Some "the flattened frame exceeds the cell or byte limit"
            else
              let initial =
                match (Frame.location_kind location, !arguments) with
                | Frame.Named_parameter, bits :: rest -> (
                    arguments := rest;
                    match stored_type with
                    | Stored_word type_ -> Some (Runtime_word { type_; bits })
                    | Stored_byte -> assert false
                    | Stored_pointer _ ->
                        if not pointer_arguments then
                          error :=
                            Some
                              "integer argument bits cannot supply a pointer \
                               parameter";
                        None)
                | _ -> None
              in
              let entry =
                {
                  slot_type = Frame.location_checked_type location;
                  stored_type;
                  initial;
                  object_count = Int64.to_int count;
                  strides;
                }
              in
              prepared_rev :=
                (Int64.to_int !total_cells, offset, entry) :: !prepared_rev;
              total_cells := Int64.add !total_cells count;
              total_bytes := Int64.add !total_bytes bytes
        | _ ->
            error :=
              Some
                "the checked frame contains an unsupported storage type or \
                 shape")
      locations;
    match !error with
    | Some message -> invalid message
    | None -> (
        let slots =
          Array.make
            (Int64.to_int !total_cells)
            {
              slot_type = Function.return_type function_;
              stored_type = Stored_word I64;
              initial = None;
              object_count = 1;
              strides = [];
            }
        in
        let offsets = ref Offset_map.empty in
        List.iter
          (fun (base, offset, entry) ->
            if Offset_map.mem offset !offsets then
              error := Some "the checked frame contains overlapping roots";
            offsets := Offset_map.add offset base !offsets;
            Array.fill slots base entry.object_count entry)
          (List.rev !prepared_rev);
        match !error with
        | Some message -> invalid message
        | None ->
            Ok
              {
                layout = frame;
                slots;
                offsets = !offsets;
                return_type = Function.return_type function_;
                allocated_bytes;
              })

let frame_pointer type_ =
  (Type.pointer_depth type_ = 1 || Type.pointer_depth type_ = 2)
  &&
  match Type.base type_ with
  | Type.Primitive (_, (Sema.Primitive_type.I64 | U64 | U8)) -> true
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

let index_offset types (description : Sequence.description) =
  match (description.operands, description.target_type) with
  | [ stride_id; value_id ], Some pointer when scalar_pointer_type pointer -> (
      match
        (Value_map.find_opt stride_id types, Value_map.find_opt value_id types)
      with
      | ( Some (Frame_offset (stride_type, stride)),
          Some (Supported (expected_type, index_type)) )
        when Type.equal pointer stride_type
             && stride > 0L
             && Option.is_some (return_word_type index_type) ->
          Index_offset (pointer, stride, { value_id; expected_type })
      | _ -> Unsupported)
  | _ -> Unsupported

let indexed_address frame types (description : Sequence.description) =
  match (description.operands, description.target_type) with
  | [ base; offset ], Some pointer when scalar_pointer_type pointer -> (
      let strides =
        match Value_map.find_opt base types with
        | Some (Frame_address index) ->
            Option.bind frame (fun context ->
                let slot = context.slots.(index) in
                match Type.pointer_to slot.slot_type with
                | Ok expected when Type.equal expected pointer ->
                    Some slot.strides
                | _ -> None)
        | Some (Indexed_address (expected, strides))
          when Type.equal expected pointer -> Some strides
        | Some (Pointer_value expected) when Type.equal expected pointer ->
            Option.bind
              (Result.to_option (Type.dereference pointer))
              (fun pointee ->
                Option.map
                  (fun width -> [ Int64.of_int width ])
                  (scalar_element_bytes pointee))
        | _ -> None
      in
      match (strides, Value_map.find_opt offset types) with
      | Some (stride :: remaining), Some (Index_offset (expected, actual, _))
        when stride = actual && Type.equal expected pointer ->
          Indexed_address (pointer, remaining)
      | _ -> Unsupported)
  | _ -> Unsupported

let declared_types ?frame ?globals ?literals ?initialization
    ?(allow_calls = false) block =
  let memory_enabled =
    Option.is_some frame || Option.is_some globals || Option.is_some literals
  in
  let allow_byte = Option.is_some frame || Option.is_some literals in
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
                       if
                         Option.is_some literals
                         && description.opcode = Opcode.Ic_str_const
                         && literal_pointer_type type_
                       then Pointer_value type_
                       else if
                         memory_enabled && scalar_pointer_type type_
                         && (description.opcode = Opcode.Ic_addr
                            || description.opcode = Opcode.Ic_deref
                            || description.opcode = Opcode.Ic_assign)
                       then Pointer_value type_
                       else if
                         allow_calls && description.opcode = Opcode.Ic_call_end
                       then
                         match checked_return_kind type_ with
                         | Some (Word_return word_type) ->
                             Supported (word_type, type_)
                         | Some Void_return -> Void_value
                         | None -> Unsupported
                       else
                         match (frame, description.opcode) with
                         | _, opcode
                           when memory_enabled
                                && (opcode = Opcode.Ic_deref
                                  || opcode = Opcode.Ic_assign
                                   || Option.is_some (update_kind opcode)) -> (
                             match
                               scalar_value_type ~allow_byte ~allow_public:true
                                 type_
                             with
                             | Some word_type -> Supported (word_type, type_)
                             | None -> Unsupported)
                         | Some _, Opcode.Ic_rbp when frame_pointer type_ ->
                             Frame_base type_
                         | _, Opcode.Ic_imm_i64
                           when frame_pointer type_ && memory_enabled -> (
                             match description.payload with
                             | Some (Sequence.Integer offset) ->
                                 Frame_offset (type_, offset)
                             | _ -> Unsupported)
                         | _, Opcode.Ic_mul
                           when scalar_pointer_type type_ && memory_enabled ->
                             index_offset types description
                         | _, Opcode.Ic_add when frame_pointer type_ -> (
                             match indexed_address frame types description with
                             | Indexed_address _ as indexed -> indexed
                             | _ -> (
                                 match frame with
                                 | Some context ->
                                     address_slot context types description
                                 | None -> Unsupported))
                         | _, opcode
                           when (memory_enabled || allow_calls)
                                &&
                                match opcode_kind opcode with
                                | Some (Unary_kind _ | Binary_kind _) -> true
                                | _ -> false -> (
                             match
                               scalar_value_type ~allow_byte ~allow_public:true
                                 type_
                             with
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
      ( Pointer_value _
      | Void_value
      | Unsupported
      | Frame_base _
      | Frame_offset _
      | Frame_address _
      | Index_offset _
      | Indexed_address _
      | Global_address _ )
  | None -> None

let pointer_operand_of_value types pointer_value =
  match Value_map.find_opt pointer_value types with
  | Some (Pointer_value pointer_type) -> Some { pointer_value; pointer_type }
  | _ -> None

let memory_operand_of_value types id =
  match operand_of_value types id with
  | Some word -> Some (Word_operand word)
  | None ->
      Option.map
        (fun p -> Pointer_operand p)
        (pointer_operand_of_value types id)

let value_matches stored operand =
  match (stored, operand) with
  | (Stored_word _ | Stored_byte), Word_operand _ -> true
  | Stored_pointer expected, Pointer_operand actual ->
      Type.compatible_u8_pointer expected actual.pointer_type
  | _ -> false

let storage_operand ?(allow_array = false) frame initialization types
    instruction address =
  match (frame, Value_map.find_opt address types) with
  | Some context, Some (Frame_address index) ->
      let slot = context.slots.(index) in
      if slot.strides <> [] && not allow_array then None
      else
        Some
          ( Frame_slot (index, slot.object_count),
            slot.slot_type,
            slot.stored_type )
  | _, Some (Global_address slot)
    when storage_allowed frame initialization instruction slot ->
      let type_ = Integer_globals.storage_type slot in
      Option.map
        (fun kind ->
          (Global_slot (Integer_globals.storage_index slot), type_, kind))
        (stored_type type_)
  | _, Some (Indexed_address (pointer_type, remaining))
    when allow_array || remaining = [] -> (
      match Type.dereference pointer_type with
      | Ok pointee ->
          Option.map
            (fun stored ->
              ( Indexed_slot { pointer_value = address; pointer_type },
                pointee,
                stored ))
            (stored_type pointee)
      | Error _ -> None)
  | _, Some (Pointer_value pointer_type) -> (
      match Type.dereference pointer_type with
      | Ok pointee ->
          Option.map
            (fun stored ->
              ( Indirect_slot { pointer_value = address; pointer_type },
                pointee,
                stored ))
            (stored_type pointee)
      | Error _ -> None)
  | _ -> None

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
      | Negate, Type.Primitive (_, Sema.Primitive_type.U8) ->
          (* OptPass012.HC:180-192 changes U8 negation to I8, outside this
             storage slice. Preserve the boundary even for forged IR. *)
          false
      | Complement, _
      | Negate, Type.Primitive (Type.Internal_storage, Sema.Primitive_type.U64)
        -> internal_i64 result_type
      | (Negate | Logical_not), _ -> Type.equal operand_type result_type)
  | _ -> false

let promoted_word_type left right =
  match (left, right) with
  | I64, I64 -> I64
  | I64, U64 | U64, I64 | U64, U64 -> U64

let valid_binary_result_type types operation left right result_type =
  match operation with
  | Compare _ | Logical _ -> return_word_type result_type = Some I64
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_left
  | Shift_right -> (
      let raw_id type_ =
        match Type.base type_ with
        | Type.Primitive (_, primitive) when Type.pointer_depth type_ = 0 ->
            Some (Sema.Primitive_type.info primitive).raw_id
        | _ -> None
      in
      match (Value_map.find_opt left types, Value_map.find_opt right types) with
      | Some (Supported (_, left)), Some (Supported (_, right)) -> (
          match (raw_id left, raw_id right, raw_id result_type) with
          | Some left, Some right, Some actual -> actual = max left right
          | _ -> false)
      | _ -> false)

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

let fresh_literal_image () = { literal_byte_count = 0; literal_chunks_rev = [] }

let collect_literals ~max_literal_bytes image graph =
  let ( let* ) = Result.bind in
  (* Each owner gets a fresh map, even if two definitions share a graph object.
     Only immutable payloads are retained here; byte cells are allocated after
     every owner and instruction has passed preflight. *)
  let* literal_regions =
    Graph.blocks graph
    |> List.fold_left
         (fun result block ->
           let* regions = result in
           let block_id = Graph.block_id block in
           Graph.instructions block |> Sequence.instructions
           |> List.fold_left
                (fun result instruction ->
                  let* regions = result in
                  let description = Sequence.description instruction in
                  if description.opcode <> Opcode.Ic_str_const then Ok regions
                  else
                    match
                      ( description.operands,
                        description.result,
                        description.target_type,
                        description.payload )
                    with
                    | [], Some _, Some type_, Some (Sequence.Bytes bytes) ->
                        if not (literal_pointer_type type_) then
                          Error
                            [
                              preflight_error block_id description "HCIRVM0005"
                                "IC_STR_CONST requires internal-storage U8*";
                            ]
                        else
                          let remaining =
                            min max_literal_bytes Sys.max_array_length
                            - image.literal_byte_count
                          in
                          let length = String.length bytes in
                          if length >= remaining then
                            Error
                              [
                                preflight_error block_id description
                                  "HCIRVM0021"
                                  "owned string literals exceed the literal \
                                   byte limit or host array capacity";
                              ]
                          else
                            let literal_count = length + 1 in
                            let literal_base = image.literal_byte_count in
                            image.literal_byte_count <-
                              literal_base + literal_count;
                            image.literal_chunks_rev <-
                              (literal_base, bytes) :: image.literal_chunks_rev;
                            Ok
                              (Instruction_map.add description.instruction_id
                                 { literal_base; literal_count }
                                 regions)
                    | _ -> Error [ malformed block_id description ])
                (Ok regions))
         (Ok Instruction_map.empty)
  in
  Ok { literal_graph = graph; literal_regions }

let prepare_instruction ?frame ?globals ?literals ?initialization
    ?(allow_public = false) block_index types block_id
    (description : Sequence.description) =
  let memory_enabled =
    Option.is_some frame || Option.is_some globals || Option.is_some literals
  in
  let allow_byte = Option.is_some frame || Option.is_some literals in
  let produced =
    Option.bind description.result (fun result ->
        Value_map.find_opt result.value_id types)
  in
  let kind =
    match (frame, description.opcode) with
    | _, Opcode.Ic_str_const when Option.is_some literals ->
        Some Literal_address_kind
    | _, Opcode.Ic_mul
      when match produced with
           | Some (Index_offset _) -> true
           | _ -> false -> Some Scale_index_kind
    | _, Opcode.Ic_add
      when match produced with
           | Some (Indexed_address _) -> true
           | _ -> false -> Some Index_address_kind
    | _, Opcode.Ic_addr when memory_enabled -> Some Pointer_address_kind
    | _, (Opcode.Ic_imm_i64 | Opcode.Ic_abs_addr)
      when Option.is_some globals
           &&
           match description.payload with
           | Some (Sequence.Symbol _) -> true
           | _ -> false -> Some Global_address_kind
    | Some _, Opcode.Ic_rbp -> Some Frame_address_kind
    | _, (Opcode.Ic_imm_i64 | Opcode.Ic_add)
      when memory_enabled
           && Option.fold ~none:false
                ~some:(fun type_ -> Type.pointer_depth type_ > 0)
                description.target_type -> Some Frame_address_kind
    | _, Opcode.Ic_deref when memory_enabled -> Some Load_slot_kind
    | _, Opcode.Ic_assign when memory_enabled -> Some Store_slot_kind
    | _, opcode when memory_enabled && Option.is_some (update_kind opcode) ->
        update_kind opcode
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
          | Literal_address_kind -> (
              match
                ( literals,
                  description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | ( Some context,
                  [],
                  Some result,
                  Some pointer,
                  Some (Sequence.Bytes _) )
                when literal_pointer_type pointer -> (
                  match
                    ( Instruction_map.find_opt description.instruction_id
                        context.literal_regions,
                      Type.dereference pointer )
                  with
                  | Some region, Ok pointee ->
                      Ok
                        (Materialize_address
                           ( Literal_slot
                               (region.literal_base, region.literal_count),
                             result.value_id,
                             pointee ))
                  | _ -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Scale_index_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.payload,
                  produced )
              with
              | ( [ _; _ ],
                  Some result,
                  None,
                  Some (Index_offset (_, stride, operand)) ) ->
                  Ok (Scale_index (operand, stride, result.value_id))
              | _ -> Error (malformed block_id description))
          | Index_address_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.payload,
                  produced )
              with
              | ( [ base; offset ],
                  Some result,
                  None,
                  Some (Indexed_address (pointer, _)) ) -> (
                  match
                    ( storage_operand ~allow_array:true frame initialization
                        types description.instruction_id base,
                      Type.dereference pointer )
                  with
                  | ( Some (location, actual, (Stored_word _ | Stored_byte)),
                      Ok pointee )
                    when Type.equal actual pointee ->
                      Ok
                        (Index_address
                           (location, offset, result.value_id, pointee))
                  | _ -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Pointer_address_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ address ], Some result, Some target_type, None
                when scalar_pointer_type target_type -> (
                  match
                    storage_operand ~allow_array:true frame initialization types
                      description.instruction_id address
                  with
                  | Some (location, pointee, (Stored_word _ | Stored_byte)) -> (
                      match Type.pointer_to pointee with
                      | Ok expected when Type.equal expected target_type ->
                          Ok
                            (Materialize_address
                               (location, result.value_id, pointee))
                      | _ -> Error (invalid_type_matrix block_id description))
                  | _ -> Error (invalid_type_matrix block_id description))
              | _ -> Error (malformed block_id description))
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
                    storage_operand frame initialization types
                      description.instruction_id address
                  in
                  match slot with
                  | Some (location, slot_type, stored_type)
                    when Type.equal slot_type target_type -> (
                      match (kind, operands) with
                      | Load_slot_kind, [] ->
                          Ok (Load_slot (location, result.value_id))
                      | Store_slot_kind, [ operand ] -> (
                          match memory_operand_of_value types operand with
                          | Some operand when value_matches stored_type operand
                            ->
                              Ok
                                (Store_slot
                                   ( location,
                                     operand,
                                     result.value_id,
                                     stored_type ))
                          | _ ->
                              Error (invalid_type_matrix block_id description))
                      | Update_slot_kind operation, [ operand ]
                        when match stored_type with
                             | Stored_word _ -> true
                             | _ -> false -> (
                          let word_type =
                            match stored_type with
                            | Stored_word t -> t
                            | _ -> assert false
                          in
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
                      | Increment_slot_kind (operation, old_result), []
                        when match stored_type with
                             | Stored_word _ -> true
                             | _ -> false ->
                          let word_type =
                            match stored_type with
                            | Stored_word t -> t
                            | _ -> assert false
                          in
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
                    scalar_value_type ~allow_byte
                      ~allow_public:(memory_enabled || allow_public)
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
                      | Some operand
                        when match Value_map.find_opt operand_id types with
                             | Some (Supported (_, type_)) ->
                                 Option.is_some (return_word_type type_)
                             | _ -> false ->
                          Ok (Word_view (operand, result.value_id, result_type))
                      | Some _ ->
                          Error (invalid_type_matrix block_id description)))
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
                    scalar_value_type ~allow_byte
                      ~allow_public:(memory_enabled || allow_public)
                      result_type
                  with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match
                        ( operand_of_value types left_id,
                          operand_of_value types right_id )
                      with
                      | Some left, Some right
                        when Option.fold ~none:false
                               ~some:
                                 (valid_binary_result_type types binary left_id
                                    right_id)
                               description.target_type ->
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
                  match Value_map.find_opt operand_id types with
                  | Some Void_value -> Ok (Discard_void operand_id)
                  | _ -> (
                      match memory_operand_of_value types operand_id with
                      | Some (Word_operand _ as operand) -> Ok (Discard operand)
                      | Some (Pointer_operand _ as operand)
                        when Option.is_some frame || Option.is_some literals ->
                          Ok (Discard operand)
                      | _ -> Error (invalid_type_matrix block_id description)))
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
              capture_discard = true;
            })
          operation

let prepare ?frame ?globals ?literals ?initialization ?callees ?runtime_calls
    ?(runtime_owner = Runtime.Entry) graph =
  let ( let* ) = Result.bind in
  let* () =
    match literals with
    | Some context when context.literal_graph != graph ->
        Error
          [
            make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0004"
              "literal storage requires its exact checked instruction graph";
          ]
    | _ -> Ok ()
  in
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
          declared_types ?frame ?globals ?literals ?initialization
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
              capture_discard = true;
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
          let site_id site select =
            Option.fold ~none:true
              ~some:(fun site ->
                Instruction_id.equal (select site) description.instruction_id)
              site
          in
          let runtime_callee site =
            let argument_type argument =
              let type_ = Runtime.argument_target_type argument in
              match
                scalar_value_type ~allow_byte:true ~allow_public:true type_
              with
              | Some word -> Some (Stored_word word)
              | None when scalar_pointer_type type_ ->
                  Some (Stored_pointer type_)
              | None -> None
            in
            let arguments = List.rev (Runtime.arguments site) in
            let types = List.filter_map argument_type arguments in
            if
              List.length types <> List.length arguments
              || List.length types > Sys.max_array_length
            then None
            else
              Some
                {
                  callee_index = -1;
                  callee_symbol = Runtime.symbol site;
                  callee_definition = None;
                  callee_return_type = Runtime.return_type site;
                  parameter_types = Array.of_list types;
                  cleanup_opcode = Runtime.cleanup_opcode site;
                  frame_bytes = 0;
                }
          in
          match (description.opcode, !calls) with
          | Opcode.Ic_call_start, stack
            when no_operands && description.result = None
                 && description.target_type = None -> (
              match description.payload with
              | Some (Sequence.Symbol symbol) -> (
                  let site =
                    Option.bind runtime_calls (fun context ->
                        Runtime.find_start context ~owner:runtime_owner
                          description.instruction_id)
                  in
                  let selected =
                    match site with
                    | Some site when Option.is_some (Runtime.provider site) ->
                        runtime_callee site
                    | Some site when Runtime.call_opcode site <> Opcode.Ic_call
                      -> None
                    | _ ->
                        Option.value callees ~default:[]
                        |> List.find_opt (fun callee ->
                            callee.callee_symbol == symbol
                            &&
                            match (site, callee.callee_definition) with
                            | Some site, Some declaration ->
                                Runtime.declaration site == declaration
                            | _ -> true)
                  in
                  match selected with
                  | Some callee
                    when callee.callee_symbol == symbol
                         &&
                         match stack with
                         | [] | { phase = Collecting _; _ } :: _ -> true
                         | _ -> false ->
                      calls :=
                        {
                          callee;
                          site;
                          remaining_arguments =
                            Option.map Runtime.arguments site;
                          phase = Collecting 0;
                        }
                        :: stack;
                      call_instruction description Call_start
                  | _ ->
                      Error
                        (call_error description
                           "direct call has no matching executable definition \
                            or valid enclosing call"))
              | _ -> Error (malformed block_id description))
          | ( (Opcode.Ic_call | Opcode.Ic_call_indirect2 | Opcode.Ic_call_extern),
              ({ callee; site; phase = Collecting count; _ } as call) :: rest )
            when no_operands && description.result = None
                 && target_matches callee
                 && description.opcode
                    = Option.fold ~none:Opcode.Ic_call ~some:Runtime.call_opcode
                        site
                 && site_id site Runtime.call_instruction -> (
              match description.payload with
              | Some (Sequence.Symbol symbol)
                when symbol == callee.callee_symbol
                     && count = Array.length callee.parameter_types ->
                  calls := { call with phase = Needs_cleanup } :: rest;
                  let operation =
                    match site with
                    | Some site when Option.is_some (Runtime.provider site) ->
                        Runtime_call (site, callee.parameter_types)
                    | _ -> Call callee.callee_index
                  in
                  call_instruction description operation
              | _ ->
                  Error
                    (call_error description
                       "call target or pushed argument count disagrees with \
                        its definition"))
          | ( (Opcode.Ic_add_rsp | Opcode.Ic_add_rsp1),
              ({ callee; site; phase = Needs_cleanup; _ } as call) :: rest )
            when no_operands && description.result = None
                 && target_matches callee
                 && description.opcode = callee.cleanup_opcode
                 && site_id site Runtime.cleanup_instruction -> (
              match description.payload with
              | Some (Sequence.Integer bytes)
                when bytes
                     = Int64.mul 8L
                         (Int64.of_int (Array.length callee.parameter_types)) ->
                  calls := { call with phase = Needs_end } :: rest;
                  call_instruction description Call_cleanup
              | _ ->
                  Error
                    (call_error description
                       "call cleanup does not match its fixed argument slots"))
          | Opcode.Ic_call_end, { callee; site; phase = Needs_end; _ } :: rest
            when no_operands && target_matches callee
                 && site_id site Runtime.last
                 && Option.fold ~none:true
                      ~some:(fun site ->
                        Option.fold ~none:false
                          ~some:(fun result ->
                            Value_id.equal result.Sequence.value_id
                              (Runtime.result_value site))
                          description.result)
                      site -> (
              match
                ( description.payload,
                  description.result,
                  checked_return_kind callee.callee_return_type )
              with
              | ( Some (Sequence.Symbol symbol),
                  Some result,
                  Some (Word_return type_) )
                when symbol == callee.callee_symbol ->
                  calls := rest;
                  call_instruction description
                    (Call_end (result.value_id, type_))
              | Some (Sequence.Symbol symbol), Some result, Some Void_return
                when symbol == callee.callee_symbol ->
                  calls := rest;
                  call_instruction description (Call_end_void result.value_id)
              | _ ->
                  Error
                    (call_error description
                       "call end does not match its checked target and result"))
          | ( ( Opcode.Ic_call_start
              | Opcode.Ic_call
              | Opcode.Ic_call_indirect2
              | Opcode.Ic_call_extern
              | Opcode.Ic_call_import
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
              prepare_instruction ?frame ?globals ?literals ?initialization
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
                prepare_instruction ?frame ?globals ?literals ?initialization
                  block_index types block_id description
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
                        ({
                           callee;
                           remaining_arguments;
                           phase = Collecting count;
                           _;
                         } as call)
                        :: rest )
                      when count < Array.length callee.parameter_types -> (
                        match memory_operand_of_value types result.value_id with
                        | Some operand
                          when value_matches
                                 callee.parameter_types.(Array.length
                                                           callee
                                                             .parameter_types
                                                         - 1 - count)
                                 operand
                               && Option.fold ~none:true
                                    ~some:(function
                                      | [] -> false
                                      | argument :: _ ->
                                          Instruction_id.equal
                                            (Runtime.argument_producer argument)
                                            description.instruction_id
                                          && Value_id.equal
                                               (Runtime.argument_value argument)
                                               result.value_id
                                          && Option.fold ~none:false
                                               ~some:
                                                 (Type.equal
                                                    (Runtime
                                                     .argument_source_type
                                                       argument))
                                               description.target_type)
                                    remaining_arguments ->
                            calls :=
                              {
                                call with
                                phase = Collecting (count + 1);
                                remaining_arguments =
                                  Option.map
                                    (function
                                      | [] -> []
                                      | _ :: rest -> rest)
                                    remaining_arguments;
                              }
                              :: rest;
                            Some operand
                        | _ ->
                            errors_rev :=
                              call_error description
                                "pushed argument does not match its checked \
                                 word or pointer parameter"
                              :: !errors_rev;
                            None)
                    | _ ->
                        errors_rev :=
                          call_error description
                            "argument push has no matching fixed parameter"
                          :: !errors_rev;
                        None
                in
                let implicit_discard =
                  Option.fold ~none:false
                    ~some:(fun context ->
                      Runtime.is_implicit_discard context ~owner:runtime_owner
                        description.instruction_id)
                    runtime_calls
                in
                if
                  implicit_discard
                  &&
                  match prepared.operation with
                  | Discard _ | Discard_void _ -> false
                  | _ -> true
                then
                  errors_rev :=
                    call_error description
                      "implicit output metadata does not identify a checked \
                       discard"
                    :: !errors_rev;
                instructions_rev :=
                  {
                    prepared with
                    push_result;
                    capture_discard = not implicit_discard;
                  }
                  :: !instructions_rev
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
              initial_frame_bytes =
                Option.fold ~none:0
                  ~some:(fun context -> context.allocated_bytes)
                  frame;
              is_function = Option.is_some frame;
              required_return =
                Option.bind frame (fun context ->
                    checked_return_kind context.return_type);
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

type call_completion = Pending | Completed_void | Completed_word of word

type call_scope = {
  arguments_rev : runtime_value list;
  completion : call_completion;
}

type caller = {
  saved_program : prepared;
  saved_block : int;
  saved_instruction : int;
  saved_values : runtime_value Value_map.t;
  saved_slots : runtime_storage;
  saved_return : word option;
  saved_calls : call_scope list;
}

let execute_prepared ?(callees = [||]) ?(max_frame_bytes = Int.max_int)
    ?(max_call_depth = Int.max_int) ?(capture_last = false) ?initialization
    ?(global_words = [||]) ?literal_image ?output ~max_steps program =
  let current_block = ref program.entry_index in
  let current_instruction = ref 0 in
  let values = ref Value_map.empty in
  let make_storage cells unknown_message =
    { cells = Array.copy cells; live = true; unknown_message }
  in
  let frame_storage cells =
    make_storage cells "the reached frame slot has not been initialized"
  in
  let global_storage =
    make_storage
      (Array.map (Option.map (fun word -> Runtime_word word)) global_words)
      "hosted execution reached an uninitialized JIT persistent object"
  in
  let literal_storage =
    let cells =
      match literal_image with
      | None -> [||]
      | Some image ->
          let cells =
            Array.make image.literal_byte_count
              (Some (Runtime_word { type_ = U64; bits = 0L }))
          in
          List.iter
            (fun (base, bytes) ->
              String.iteri
                (fun index byte ->
                  cells.(base + index) <-
                    Some
                      (Runtime_word
                         { type_ = U64; bits = Int64.of_int (Char.code byte) }))
                bytes)
            image.literal_chunks_rev;
          cells
    in
    {
      cells;
      live = true;
      unknown_message =
        "owned string literal byte is unexpectedly uninitialized";
    }
  in
  let slots = ref (frame_storage program.initial_slots) in
  let program = ref program in
  let callers = ref [] in
  let calls = ref [] in
  let depth = ref 0 in
  let live_frame_bytes = ref !program.initial_frame_bytes in
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
    | Some (Runtime_word word) when word.type_ = operand.expected_type ->
        Some word
    | Some _ | None ->
        failed :=
          Some
            (runtime_error ~instruction block !steps "HCIRVM0008"
               "a prepared operand is unavailable or has the wrong word type");
        None
  in
  let address_bounds ~one_past block instruction address =
    let offset = address.pointer_offset in
    let width = Int64.of_int address.pointer_element_bytes in
    if
      offset < 0L
      || Int64.rem offset width <> 0L
      ||
      if one_past then offset > address.pointer_extent_bytes
      else offset > Int64.sub address.pointer_extent_bytes width
    then (
      failed :=
        Some
          (runtime_error ~instruction block !steps "HCIRVM0019"
             "indexed address is outside its declared object extent");
      false)
    else true
  in
  let require_pointer ?(bounded = true) block instruction operand =
    match Value_map.find_opt operand.pointer_value !values with
    | Some (Runtime_pointer address) -> (
        match Type.dereference operand.pointer_type with
        | Ok expected
          when Type.equal expected address.pointer_pointee
               && scalar_element_bytes expected
                  = Some address.pointer_element_bytes
               && address.pointer_element_bytes > 0
               && Int64.of_int address.pointer_count
                  <= Int64.div Int64.max_int
                       (Int64.of_int address.pointer_element_bytes)
               && address.pointer_extent_bytes
                  = Int64.mul
                      (Int64.of_int address.pointer_count)
                      (Int64.of_int address.pointer_element_bytes)
               && address.pointer_storage.live && address.pointer_base >= 0
               && address.pointer_count > 0
               && address.pointer_count
                  <= Array.length address.pointer_storage.cells
               && address.pointer_base
                  <= Array.length address.pointer_storage.cells
                     - address.pointer_count ->
            if
              (not bounded)
              || address_bounds ~one_past:true block instruction address
            then Some address
            else None
        | _ ->
            failed :=
              Some
                (runtime_error ~instruction block !steps "HCIRVM0018"
                   "pointer does not identify a live object of its checked \
                    pointee type");
            None)
    | _ ->
        failed :=
          Some
            (runtime_error ~instruction block !steps "HCIRVM0018"
               "prepared pointer value is unavailable or invalid");
        None
  in
  let require_value block instruction = function
    | Word_operand operand ->
        Option.map
          (fun word -> Runtime_word word)
          (require_operand block instruction operand)
    | Pointer_operand operand ->
        Option.map
          (fun address -> Runtime_pointer address)
          (require_pointer block instruction operand)
  in
  let coerce_value expected = function
    | Runtime_offset _ | Runtime_void -> None
    | Runtime_word word -> (
        match expected with
        | Stored_word type_ -> Some (Runtime_word { type_; bits = word.bits })
        | Stored_byte ->
            Some
              (Runtime_word { type_ = U64; bits = Int64.logand word.bits 255L })
        | _ -> None)
    | Runtime_pointer address -> (
        match expected with
        | Stored_pointer type_ -> (
            match
              (Type.pointer_to address.pointer_pointee, Type.dereference type_)
            with
            | Ok actual, Ok pointer_pointee
              when Type.compatible_u8_pointer type_ actual
                   && address.pointer_storage.live ->
                Some (Runtime_pointer { address with pointer_pointee })
            | _ -> None)
        | _ -> None)
  in
  let resolve_address block instruction location pointer_pointee =
    let root pointer_storage pointer_base pointer_count =
      Option.map
        (fun pointer_element_bytes ->
          {
            pointer_storage;
            pointer_base;
            pointer_count;
            pointer_element_bytes;
            pointer_extent_bytes =
              Int64.mul
                (Int64.of_int pointer_count)
                (Int64.of_int pointer_element_bytes);
            pointer_offset = 0L;
            pointer_pointee;
          })
        (scalar_element_bytes pointer_pointee)
    in
    match location with
    | Frame_slot (base, count) -> root !slots base count
    | Global_slot base -> root global_storage base 1
    | Literal_slot (base, count) -> root literal_storage base count
    | Indirect_slot operand -> require_pointer block instruction operand
    | Indexed_slot operand ->
        require_pointer ~bounded:false block instruction operand
  in
  let resolve_location block instruction = function
    | Frame_slot (index, _) -> Some (!slots, index)
    | Global_slot index -> Some (global_storage, index)
    | Literal_slot (index, _) -> Some (literal_storage, index)
    | Indirect_slot operand | Indexed_slot operand ->
        Option.bind (require_pointer ~bounded:false block instruction operand)
          (fun address ->
            if address_bounds ~one_past:false block instruction address then
              Some
                ( address.pointer_storage,
                  address.pointer_base
                  + Int64.to_int
                      (Int64.div address.pointer_offset
                         (Int64.of_int address.pointer_element_bytes)) )
            else None)
  in
  let read_output_byte block instruction address relative =
    let error code message =
      Error (runtime_error ~instruction block !steps code message)
    in
    let storage = address.pointer_storage in
    if
      (not storage.live)
      || address.pointer_element_bytes <> 1
      || scalar_element_bytes address.pointer_pointee <> Some 1
      || address.pointer_base < 0 || address.pointer_count <= 0
      || address.pointer_count > Array.length storage.cells
      || address.pointer_base
         > Array.length storage.cells - address.pointer_count
      || address.pointer_extent_bytes <> Int64.of_int address.pointer_count
    then
      error "HCIRVM0018"
        "output pointer does not identify a live owned U8 object"
    else if address.pointer_offset < 0L || relative < 0L then
      error "HCIRVM0019" "output scan is outside its declared object extent"
    else if relative > Int64.sub Int64.max_int address.pointer_offset then
      error "HCIRVM0020" "output scan offset exceeds the integer address range"
    else
      let offset = Int64.add address.pointer_offset relative in
      if offset >= address.pointer_extent_bytes then
        error "HCIRVM0019" "output scan is outside its declared object extent"
      else
        match storage.cells.(address.pointer_base + Int64.to_int offset) with
        | Some (Runtime_word { type_ = U64; bits })
          when bits >= 0L && bits <= 255L -> Ok (Char.chr (Int64.to_int bits))
        | None -> error "HCIRVM0012" storage.unknown_message
        | Some _ ->
            error "HCIRVM0008" "output scan reached an invalid byte cell"
  in
  let invoke_output block instruction site parameter_types scope =
    let provider_name =
      match Runtime.provider site with
      | Some Runtime.Print -> "Print"
      | Some Runtime.Put_chars -> "PutChars"
      | None -> "runtime output"
    in
    let provider_message message =
      if
        String.starts_with ~prefix:(provider_name ^ " ") message
        || String.starts_with ~prefix:(provider_name ^ ":") message
      then message
      else provider_name ^ ": " ^ message
    in
    let make_provider_error code message =
      runtime_error ~instruction block !steps code (provider_message message)
    in
    let error code message = Error (make_provider_error code message) in
    if !depth >= max_call_depth then
      error "HCIRVM0015" "the runtime call depth limit was exhausted"
    else if
      Array.length parameter_types > (max_frame_bytes - !live_frame_bytes) / 8
    then
      error "HCIRVM0011"
        "runtime argument slots exceed the active frame byte limit"
    else if List.length scope.arguments_rev <> Array.length parameter_types then
      error "HCIRVM0008" "prepared runtime argument count is inconsistent"
    else
      let rec arguments index rev = function
        | [] -> Ok (List.rev rev)
        | value :: rest -> (
            match coerce_value parameter_types.(index) value with
            | Some value -> arguments (index + 1) (value :: rev) rest
            | None ->
                error "HCIRVM0008"
                  "runtime argument disagrees with its checked slot")
      in
      let ( let* ) = Result.bind in
      let* arguments = arguments 0 [] scope.arguments_rev in
      match output with
      | None -> error "HCIRVM0008" "prepared runtime call has no output state"
      | Some output -> (
          let provider_error = function
            | Output.Memory error ->
                { error with message = provider_message error.message }
            | Output.Output_limit ->
                make_provider_error "HCIRVM0022"
                  "runtime output exceeds the output byte limit"
            | Output.Work_limit ->
                make_provider_error "HCIRVM0023"
                  "runtime output work limit was exhausted"
            | Output.Offset_overflow ->
                make_provider_error "HCIRVM0020"
                  "output scan offset exceeds the integer address range"
            | Output.Invalid_format message ->
                make_provider_error "HCIRVM0024" message
            | Output.Invalid_argument message ->
                make_provider_error "HCIRVM0025" message
          in
          match (Runtime.provider site, arguments) with
          | Some Runtime.Put_chars, [ Runtime_word word ] ->
              Output.put_chars output word.bits
              |> Result.map_error provider_error
          | ( Some Runtime.Print,
              Runtime_pointer format :: Runtime_word count :: tail )
            when count.type_ = I64
                 && count.bits = Int64.of_int (List.length tail) ->
              let rec variadic rev = function
                | [] -> Ok (Array.of_list (List.rev rev))
                | Runtime_word word :: rest ->
                    variadic (Output.Word word.bits :: rev) rest
                | Runtime_pointer address :: rest ->
                    variadic (Output.Pointer address :: rev) rest
                | (Runtime_offset _ | Runtime_void) :: _ ->
                    error "HCIRVM0008"
                      "prepared variadic output argument is invalid"
              in
              let* arguments = variadic [] tail in
              Output.print output
                ~read_byte:(read_output_byte block instruction)
                ~format arguments
              |> Result.map_error provider_error
          | _ ->
              error "HCIRVM0008"
                "prepared runtime provider arguments are inconsistent")
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
              calls := { arguments_rev = []; completion = Pending } :: !calls
          | Call_cleanup -> ()
          | Runtime_call (site, parameter_types) -> (
              match !calls with
              | ({ completion = Pending; _ } as scope) :: rest -> (
                  match
                    invoke_output block instruction site parameter_types scope
                  with
                  | Ok () ->
                      calls :=
                        { scope with completion = Completed_void } :: rest
                  | Error error -> failed := Some error)
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared runtime call has no pending caller scope"))
          | Call index -> (
              match !calls with
              | ({ completion = Pending; _ } as scope) :: _
                when index >= 0 && index < Array.length callees ->
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
                    let initialized = frame_storage body.initial_slots in
                    scope.arguments_rev
                    |> List.iteri (fun position value ->
                        match
                          coerce_value callee.parameter_types.(position) value
                        with
                        | Some value ->
                            initialized.cells.(position) <- Some value
                        | None ->
                            failed :=
                              Some
                                (runtime_error ~instruction block !steps
                                   "HCIRVM0008"
                                   "prepared argument disagrees with its \
                                    checked parameter"));
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
              | { completion = Completed_word word; _ } :: rest
                when word.type_ = type_ ->
                  calls := rest;
                  values := Value_map.add result (Runtime_word word) !values
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "direct call did not supply its declared return word"))
          | Call_end_void result -> (
              match !calls with
              | { completion = Completed_void; _ } :: rest ->
                  calls := rest;
                  values := Value_map.add result Runtime_void !values
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared U0 call did not complete without a value"))
          | Frame_address_tick -> ()
          | Scale_index (operand, stride, result) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some index ->
                  if
                    (index.type_ = U64 && index.bits < 0L)
                    || index.bits > Int64.div Int64.max_int stride
                    || index.bits < Int64.div Int64.min_int stride
                  then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0020"
                           "index byte scaling exceeds the hosted signed \
                            address range")
                  else
                    values :=
                      Value_map.add result
                        (Runtime_offset (Int64.mul index.bits stride))
                        !values)
          | Index_address (location, offset, result, pointee) -> (
              match
                ( resolve_address block instruction location pointee,
                  Value_map.find_opt offset !values )
              with
              | Some address, Some (Runtime_offset delta) ->
                  if
                    delta > 0L
                    && address.pointer_offset > Int64.sub Int64.max_int delta
                    || delta < 0L
                       && address.pointer_offset < Int64.sub Int64.min_int delta
                  then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0020"
                           "index address addition exceeds the hosted signed \
                            address range")
                  else
                    values :=
                      Value_map.add result
                        (Runtime_pointer
                           {
                             address with
                             pointer_offset =
                               Int64.add address.pointer_offset delta;
                           })
                        !values
              | None, _ -> ()
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared index offset is unavailable"))
          | Materialize_address (location, result, pointer_pointee) -> (
              match
                resolve_address block instruction location pointer_pointee
              with
              | Some address
                when address_bounds ~one_past:true block instruction address ->
                  values :=
                    Value_map.add result (Runtime_pointer address) !values
              | _ -> ())
          | Load_slot (location, result) -> (
              match resolve_location block instruction location with
              | None -> ()
              | Some (storage, index) -> (
                  match storage.cells.(index) with
                  | Some value -> values := Value_map.add result value !values
                  | None ->
                      failed :=
                        Some
                          (runtime_error ~instruction block !steps "HCIRVM0012"
                             storage.unknown_message)))
          | Store_slot (location, operand, result, type_) -> (
              match require_value block instruction operand with
              | None -> ()
              | Some operand -> (
                  match
                    ( coerce_value type_ operand,
                      resolve_location block instruction location )
                  with
                  | Some value, Some (storage, index) ->
                      storage.cells.(index) <- Some value;
                      let expression_value =
                        match (type_, operand) with
                        | Stored_byte, Runtime_word word ->
                            Runtime_word { type_ = U64; bits = word.bits }
                        | _ -> value
                      in
                      values := Value_map.add result expression_value !values
                  | _, None -> ()
                  | None, _ ->
                      failed :=
                        Some
                          (runtime_error ~instruction block !steps "HCIRVM0008"
                             "prepared store disagrees with its checked \
                              storage type")))
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
                  match resolve_location block instruction location with
                  | None -> ()
                  | Some (storage, index) -> (
                      match storage.cells.(index) with
                      | None ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps
                                 "HCIRVM0012" storage.unknown_message)
                      | Some
                          (Runtime_pointer _ | Runtime_offset _ | Runtime_void)
                        ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps
                                 "HCIRVM0008"
                                 "scalar update reached a pointer-valued slot")
                      | Some (Runtime_word old) -> (
                          (* Read the original object after RHS effects, including calls through aliases. *)
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
                              storage.cells.(index) <- Some (Runtime_word word);
                              values :=
                                Value_map.add result
                                  (Runtime_word
                                     (if old_result then old else word))
                                  !values))))
          | Immediate (result, word) ->
              values := Value_map.add result (Runtime_word word) !values
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
                    Value_map.add result
                      (Runtime_word { type_ = result_type; bits })
                      !values)
          | Word_view (operand, result, result_type) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some operand ->
                  values :=
                    Value_map.add result
                      (Runtime_word { type_ = result_type; bits = operand.bits })
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
                              (Runtime_word { type_ = result_type; bits })
                              !values
                      | Error (code, message) ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps code
                                 message))))
          | Discard operand ->
              let value =
                Option.bind (require_value block instruction operand) (function
                  | Runtime_word word -> Some word
                  | Runtime_pointer _ | Runtime_offset _ | Runtime_void -> None)
              in
              if
                capture_last && instruction.capture_discard
                && (not !program.is_function)
                && Option.is_none !active_initializer
              then final_value := value
          | Discard_void value_id -> (
              match Value_map.find_opt value_id !values with
              | Some Runtime_void ->
                  if
                    capture_last && instruction.capture_discard
                    && (not !program.is_function)
                    && Option.is_none !active_initializer
                  then final_value := None
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared no-value result is unavailable or invalid"))
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
          | Return
            when match !program.required_return with
                 | Some (Word_return _) -> Option.is_none !pending_return
                 | Some Void_return | None -> false ->
              failed :=
                Some
                  (runtime_error ~instruction block !steps "HCIRVM0013"
                     "the integer function returned without a value")
          | Return -> (
              let completion =
                match (!program.required_return, !pending_return) with
                | (Some Void_return | None), None -> Some Completed_void
                | Some (Word_return type_), Some word when word.type_ = type_ ->
                    Some (Completed_word word)
                | None, Some word -> Some (Completed_word word)
                | _ -> None
              in
              match completion with
              | None ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "function completion disagrees with its checked \
                          return kind")
              | Some completion -> (
                  !slots.live <- false;
                  match !callers with
                  | [] -> completed := Some (Returned !pending_return)
                  | caller :: rest -> (
                      callers := rest;
                      decr depth;
                      live_frame_bytes :=
                        !live_frame_bytes - !program.initial_frame_bytes;
                      program := caller.saved_program;
                      current_block := caller.saved_block;
                      current_instruction := caller.saved_instruction;
                      values := caller.saved_values;
                      slots := caller.saved_slots;
                      pending_return := caller.saved_return;
                      calls :=
                        match caller.saved_calls with
                        | scope :: rest -> { scope with completion } :: rest
                        | [] -> [])))
          | End -> completed := Some Stream_end);
          if Option.is_none !failed then
            Option.iter
              (fun operand ->
                match (require_value block instruction operand, !calls) with
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
  !slots.live <- false;
  List.iter (fun caller -> caller.saved_slots.live <- false) !callers;
  global_storage.live <- false;
  literal_storage.live <- false;
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

let execute_function ?(max_literal_bytes = 1_048_576) ~max_steps
    ~max_frame_bytes ~frame ~arguments function_ =
  let ( let* ) = Result.bind in
  if max_steps <= 0 || max_frame_bytes <= 0 || max_literal_bytes <= 0 then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps, max_frame_bytes and max_literal_bytes must be greater \
           than zero";
      ]
  else
    match frame_context ~max_frame_bytes ~frame ~arguments function_ with
    | Error errors -> Error errors
    | Ok frame -> (
        let function_id =
          Function.Function_id.to_int (Function.function_id function_)
        and function_name = Sema.Symbol.name (Function.symbol function_) in
        let identify error =
          {
            error with
            function_id = Some function_id;
            function_name = Some function_name;
          }
        in
        let literal_image = fresh_literal_image () in
        let* literals =
          collect_literals ~max_literal_bytes literal_image
            (Function.body function_)
          |> Result.map_error (List.map identify)
        in
        match
          prepare ~frame ~literals (Function.body function_)
          |> Result.map_error (List.map identify)
        with
        | Error errors -> Error errors
        | Ok program ->
            execute_prepared ~literal_image ~max_steps
              { program with owner = Some (function_id, function_name) })

let execute_program_with_output ?runtime_calls ~output ?globals ?initialization
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576) ~max_steps
    ~max_frame_bytes ~max_call_depth ~functions checked =
  let ( let* ) = Result.bind in
  if
    max_steps <= 0 || max_frame_bytes <= 0 || max_call_depth <= 0
    || max_global_bytes <= 0 || max_literal_bytes <= 0
  then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps, max_frame_bytes, max_call_depth, max_global_bytes and \
           max_literal_bytes must be greater than zero";
      ]
  else if
    Option.fold ~none:false
      ~some:(fun context ->
        not
          (Runtime.matches context ~entry:checked ~initialization
             ~functions:
               (List.map
                  (fun (definition : function_definition) -> definition.body)
                  functions)))
      runtime_calls
  then
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0014"
          "runtime call metadata requires its exact entry and function bodies";
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
          let symbol = Function.callable_symbol body in
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
              frame_context ?globals ~pointer_arguments:true ~max_frame_bytes
                ~frame
                ~arguments:(List.init parameter_count (fun _ -> 0L))
                body
              |> Result.map_error (List.map (identify body))
            in
            let callee =
              {
                callee_index = index;
                callee_symbol = symbol;
                callee_definition = Function.definition_declaration body;
                callee_return_type = Function.return_type body;
                parameter_types =
                  Array.init parameter_count (fun position ->
                      context.slots.(position).stored_type);
                cleanup_opcode =
                  (if
                     Sema.Function_flag.caller_expects_callee_pop
                       ~stored_mask:(Function.stored_flags body)
                   then Opcode.Ic_add_rsp1
                   else Opcode.Ic_add_rsp);
                frame_bytes = context.allocated_bytes;
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
    let literal_image = fresh_literal_image () in
    let rec bodies rev = function
      | [] -> Ok (Array.of_list (List.rev rev))
      | (callee, frame, body) :: rest ->
          let* literals =
            collect_literals ~max_literal_bytes literal_image
              (Function.body body)
            |> Result.map_error (List.map (identify body))
          in
          let* program =
            prepare ~frame ?globals ~literals ~callees ?runtime_calls
              ~runtime_owner:(Runtime.Function body) (Function.body body)
            |> Result.map_error (List.map (identify body))
          in
          bodies
            ((callee, { program with owner = Some (owner body) }) :: rev)
            rest
    in
    let* programs = bodies [] summaries in
    let identify_entry (error : error) =
      let region =
        Option.bind initialization (fun context ->
            Option.bind error.instruction_id (fun id ->
                match Instruction_id.of_int id with
                | Ok id -> Global_initialization.find_storage context id
                | Error _ -> None))
      in
      identify_initializer region error
    in
    let* literals =
      collect_literals ~max_literal_bytes literal_image (X87.graph checked)
      |> Result.map_error (List.map identify_entry)
    in
    let* entry =
      prepare ?globals ~literals ?initialization ~callees ?runtime_calls
        (X87.graph checked)
      |> Result.map_error (List.map identify_entry)
    in
    let global_words =
      Option.fold ~none:[] ~some:Integer_globals.storage_slots globals
      |> List.map (fun slot ->
          Option.map
            (fun bits ->
              let type_ =
                match
                  scalar_value_type ~allow_byte:true ~allow_public:true
                    (Integer_globals.storage_type slot)
                with
                | Some type_ -> type_
                | None -> assert false
              in
              { type_; bits })
            (Integer_globals.storage_initial_bits slot))
      |> Array.of_list
    in
    execute_prepared ~callees:programs ~max_frame_bytes ~max_call_depth
      ?initialization ~global_words ~literal_image ~output ~capture_last:true
      ~max_steps entry

let execute_program_report ?runtime_calls ?globals ?initialization
    ?max_global_bytes ?max_literal_bytes ?(max_output_bytes = 1_048_576)
    ?(max_output_work = 1_048_576) ~max_steps ~max_frame_bytes ~max_call_depth
    ~functions checked =
  if
    max_output_bytes <= 0 || max_output_work <= 0
    || max_output_bytes > Sys.max_string_length
  then
    {
      outcome_ =
        Error
          [
            make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
              "output limits must be positive and max_output_bytes must fit a \
               host string";
          ];
      output_bytes_ = "";
      output_work_ = 0;
    }
  else
    let output = Output.create ~max_output_bytes ~max_output_work in
    let outcome_ =
      execute_program_with_output ?runtime_calls ~output ?globals
        ?initialization ?max_global_bytes ?max_literal_bytes ~max_steps
        ~max_frame_bytes ~max_call_depth ~functions checked
    in
    {
      outcome_;
      output_bytes_ = Output.contents output;
      output_work_ = Output.work output;
    }

let execute_program ?runtime_calls ?globals ?initialization ?max_global_bytes
    ?max_literal_bytes ?max_output_bytes ?max_output_work ~max_steps
    ~max_frame_bytes ~max_call_depth ~functions checked =
  execute_program_report ?runtime_calls ?globals ?initialization
    ?max_global_bytes ?max_literal_bytes ?max_output_bytes ?max_output_work
    ~max_steps ~max_frame_bytes ~max_call_depth ~functions checked
  |> report_outcome

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
