module Sequence = Ir.Instruction_sequence
module Graph = Ir.Block_graph
module Opcode = Ir.Opcode
module Type = Sema.Type
module Primitive = Sema.Primitive_type
module Computation = Sema.Integer_computation_class
module Scalar = Ir.Integer_scalar_storage
module Encoder = X86_64_encoder
module Global_storage = X86_64_global_storage
module Literal_storage = X86_64_literal_storage
module Print_codegen = X86_64_print_format
module Runtime = Ir.Runtime_call_context
module Intrinsic = Ir.Integer_intrinsic
module Defaults = Driver.Native_parameter_defaults
module Prepared_default = Ir.Prepared_parameter_default
module Prepared_callback_default = Ir.Prepared_callback_default
module Function = Ir.Function_body
module Headers = Sema.Function_type_resolution
module Frame = Sema.Function_frame_layout
module Symbol = Sema.Symbol
module Value_map = Map.Make (Sequence.Value_id)
module Value_set = Set.Make (Sequence.Value_id)
module Instruction_set = Set.Make (Sequence.Instruction_id)
module Instruction_map = Map.Make (Sequence.Instruction_id)
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
let hard_max_global_bytes = Global_storage.hard_max_global_bytes

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

let validate_global_limit ~max_global_bytes =
  Global_storage.validate_global_limit ~max_global_bytes
  |> Result.map_error
       (List.map (fun (error : Global_storage.error) ->
            { code = error.code; message = error.message; span = error.span }))

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

(* Runtime references retain the original object and its byte initialization
   state. Each reached view has its own descriptor snapshot. *)
let checked_reference description type_ =
  if Type.pointer_depth type_ <> 1 then
    unsupported description "native references require one scalar indirection";
  match Type.dereference type_ with
  | Ok pointee ->
      (pointee, checked_scalar ~allow_public:true description pointee)
  | Error message -> malformed description message

let compatible_reference target source =
  Type.equal target source
  || Type.compatible_u8_pointer target source
  || Type.pointer_depth target = 1
     && Type.pointer_depth source = 1
     &&
     match (Type.base target, Type.base source) with
     | Type.Primitive (_, Primitive.I64), Type.Primitive (_, Primitive.I64) ->
         true
     | _ -> false

let checked_copy description target source =
  if Type.pointer_depth target = 0 then (
    ignore (checked_scalar ~allow_public:true description target);
    ignore (checked_scalar ~allow_public:true description source))
  else (
    ignore (checked_reference description target);
    if not (compatible_reference target source) then
      malformed description
        "native reference copy requires its exact pointer type")

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
  mutable code_owner_offset : int option;
  mutable reference_descriptor_offset : int option;
}

type frame_access = {
  frame_offset : int;
  frame_bytes : int;
  frame_word : word_type;
  initialized_flag_offset : int option;
}

type arena_access = {
  arena_offset : int;
  arena_bytes : int;
  arena_word : word_type;
  arena_extent_bytes : int;
  initialized_flag_offset : int;
}

type reference_access = {
  reference : value;
  scalar : scalar_value;
  offset : value option;
}

type frame_reference_origin = { access : frame_access; extent_bytes : int }

type variadic_reference_origin = {
  data_offset : int;
  count_offset : int;
  maximum_count : int;
}

type reference_origin =
  | Frame_reference of frame_reference_origin
  | Variadic_reference of variadic_reference_origin
  | Arena_reference of arena_access
  | Literal_reference of Literal_storage.region

type reference_table = Frame_table of int | Arena_table of int
type indexed_object_access = { origin : reference_origin; offset : value }

let reference_scalar = function
  | Frame_reference origin ->
      {
        word_type = origin.access.frame_word;
        byte_size = origin.access.frame_bytes;
      }
  | Arena_reference access ->
      { word_type = access.arena_word; byte_size = access.arena_bytes }
  | Variadic_reference _ -> { word_type = I64; byte_size = 8 }
  | Literal_reference _ -> { word_type = U64; byte_size = 1 }

let reference_extent = function
  | Frame_reference origin -> origin.extent_bytes
  | Variadic_reference origin -> origin.maximum_count * 8
  | Arena_reference access -> access.arena_extent_bytes
  | Literal_reference region -> Literal_storage.byte_count region

type index_add_base =
  | Index_zero
  | Index_value of value
  | Index_reference of value

type direct_call = {
  callee_index : int;
  activation_bytes : int;
  argument_stage_slots : int array;
  argument_owner_stages : int option array;
  result_stage_slot : int option;
  named_slot_stage : int option;
  provider_arguments : (int * Print_codegen.argument_kind array) option;
}

type indirect_call = {
  captured_stage : int;
  owned_targets : int list ref;
  target_call : int -> bool * direct_call;
}

type frame_update =
  | Update_binary of Encoder.binary
  | Update_shift of Encoder.shift
  | Update_division of arithmetic_operation

type callback_access =
  | Callback_frame of frame_access * int
  | Callback_arena of arena_access * int
  | Callback_indexed of indexed_object_access * reference_table

type operation =
  | Load_immediate of value * int64
  | Load_saved_data of int * value
  | Load_function_address of value * int
  | Load_function_slot_cursor of value * Global_storage.function_slot
  | Load_function_slot of value * Global_storage.function_slot * value
  | Load_undefined_function_address of value
  | Apply_unary of Encoder.unary * value * value
  | Apply_binary of Encoder.binary * value * value * value
  | Apply_shift of Encoder.shift * value * value * value
  | Apply_constant_shift of Encoder.shift * value * int64 * value
  | Apply_division of
      arithmetic_operation * word_type * fault_site * value * value * value
  | Apply_comparison of Encoder.condition * value * value * value
  | Apply_code_comparison of Encoder.condition * value * value * value
  | Apply_reference_comparison of Encoder.condition * value * value * value
  | Apply_reference_ordering of Encoder.condition * value * value * value
  | Apply_reference_difference of value * value * value
  | Apply_logical_not of value * value
  | Apply_logical of Encoder.binary * value * value * value
  | Apply_word_view of value * value
  | Frame_tick
  | Scale_index of word_type * int64 * value * value
  | Apply_index_offset of Encoder.binary * index_add_base * value * value
  | Materialize_reference of
      reference_origin * reference_table * value option * value
  | Materialize_existing_reference of reference_access * value
  | Load_reference_value of reference_access * value
  | Store_reference_value of reference_access * value * value
  | Update_reference_value of
      reference_access
      * frame_update
      * value option
      * bool
      * value
      * word_type
      * fault_site option
  | Load_indexed_object_value of indexed_object_access * value
  | Load_code_indexed of indexed_object_access * reference_table * value
  | Store_indexed_object_value of indexed_object_access * value * value
  | Store_code_indexed of
      indexed_object_access * reference_table * value * value
  | Update_indexed_object_value of
      indexed_object_access
      * frame_update
      * value option
      * bool
      * value
      * word_type
      * fault_site option
  | Load_frame_value of frame_access * value
  | Load_reference_frame of frame_access * value
  | Load_code_frame of frame_access * int * value
  | Store_frame_value of frame_access * value * value
  | Store_reference_frame of frame_access * int * value * value
  | Store_code_frame of frame_access * int * value * value
  | Update_frame_value of
      frame_access
      * frame_update
      * value option
      * bool
      * value
      * word_type
      * fault_site option
  | Load_arena_value of arena_access * value
  | Load_code_arena of arena_access * int * value
  | Store_arena_value of arena_access * value * value
  | Store_code_arena of arena_access * int * value * value
  | Update_arena_value of
      arena_access
      * frame_update
      * value option
      * bool
      * value
      * word_type
      * fault_site option
  | Update_callback_value of
      callback_access
      * frame_update
      * value option
      * bool
      * value
      * fault_site option
  | Call_start
  | Call_capture of value * int
  | Extern_signature_fault
  | Undefined_extern_call
  | Direct_call of direct_call
  | Indirect_call of indirect_call
  | Put_chars of int
  | Print_output of Print_codegen.t
  | Internal_strlen of value * int
  | Internal_mod_u64 of value * value * int
  | Internal_bit of Intrinsic.bit * value * value * scalar_value * int
  | Internal_swap of value * scalar_value * value * scalar_value * int
  | Internal_integer of Intrinsic.unary * value * int
  | Internal_binary of Intrinsic.binary * value * value * int
  | Call_cleanup
  | Call_end of int * value
  | Call_end_void
  | Return_value of value
  | Return
  | Discard_value of value * word_type
  | Discard_callback_default of value
  | Discard_data_default of value * int
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
  push_stage : (value * int * int option * int option) option;
}

type kind =
  | Immediate_kind
  | Unary_kind of Encoder.unary
  | Binary_kind of Encoder.binary
  | Shift_kind of [ `Left | `Right ]
  | Constant_shift_kind of [ `Left | `Right ]
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
  | Opcode.Ic_shl_const -> Some (Constant_shift_kind `Left)
  | Opcode.Ic_shr_const -> Some (Constant_shift_kind `Right)
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
      code_owner_offset = None;
      reference_descriptor_offset = None;
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
        | ( Constant_shift_kind direction,
            ( [ input_id ],
              Some result,
              Some target_type,
              Some (Sequence.Integer count) ) ) ->
            let word = checked_word description target_type in
            let input = operand input_id in
            ignore (checked_word ~allow_public description input.declared_type);
            require_type description
              (Computation.forward input.computation_type)
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
            Apply_constant_shift (shift, input, count, result)
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

type branch_kind = Unconditional | Equal | Not_equal | Below | Less | Overflow

let fold_switch_runs f initial targets =
  let rec visit acc target count = function
    | next :: rest when Sequence.Block_id.compare target next = 0 ->
        visit acc target (count + 1) rest
    | next :: rest -> visit (f acc target count false) next 1 rest
    | [] -> f acc target count true
  in
  match targets with
  | [] -> initial
  | target :: rest -> visit initial target 1 rest

type planned_item =
  | Planned_instruction of Encoder.instruction
  | Planned_branch of branch_kind * int
  | Planned_call of int
  | Planned_function_address of Encoder.register * int
  | Planned_callee_stack of Encoder.register * int
  | Planned_label of int

type fault_block = { label : int; kind_value : int; site_value : int }
type value_source = Register_source of int | Slot_source of int

let branch_instruction kind displacement =
  match kind with
  | Unconditional -> Encoder.Jump displacement
  | Equal -> Encoder.Jump_equal displacement
  | Not_equal -> Encoder.Jump_not_equal displacement
  | Below -> Encoder.Jump_below displacement
  | Less -> Encoder.Jump_less displacement
  | Overflow -> Encoder.Jump_overflow displacement

let planned_size = function
  | Planned_instruction instruction -> Encoder.size instruction
  | Planned_branch (kind, _) -> Encoder.size (branch_instruction kind 0L)
  | Planned_call _ -> Encoder.size (Encoder.Call 0L)
  | Planned_function_address (register, _) ->
      Encoder.size (Encoder.Address_code_relative (register, 0L))
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
          offset := !offset + Encoder.size instruction
      | Planned_function_address (register, callee_index) ->
          if callee_index < 0 || callee_index >= Array.length callee_labels then
            reject "HCBACK0003" "native code address names an unknown function";
          let target =
            match Hashtbl.find_opt labels callee_labels.(callee_index) with
            | Some offset -> offset
            | None ->
                reject "HCBACK0003"
                  "native code address has no owned function label"
          in
          let size =
            Encoder.size (Encoder.Address_code_relative (register, 0L))
          in
          let displacement = Int64.of_int (target - (!offset + size)) in
          if displacement < -0x80000000L || displacement > 0x7fffffffL then
            reject "HCBACK0005" "native code address exceeds RIP disp32 range";
          reversed :=
            Encoder.Address_code_relative (register, displacement) :: !reversed;
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

let encoder_arena_slot span offset =
  match Encoder.arena_slot ~offset with
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

let load_arena_scalar span destination access =
  if access.arena_bytes = 8 then
    Encoder.Load_arena (destination, encoder_arena_slot span access.arena_offset)
  else
    Encoder.Load_arena_narrow
      ( destination,
        encoder_arena_slot span access.arena_offset,
        narrow_frame_width ?span access.arena_bytes,
        if access.arena_word = I64 then Encoder.Sign_extend
        else Encoder.Zero_extend )

let store_arena_scalar span access source =
  if access.arena_bytes = 8 then
    Encoder.Store_arena (encoder_arena_slot span access.arena_offset, source)
  else
    Encoder.Store_arena_narrow
      ( encoder_arena_slot span access.arena_offset,
        narrow_frame_width ?span access.arena_bytes,
        source )

let load_arena_flag span destination access =
  Encoder.Load_arena
    (destination, encoder_arena_slot span access.initialized_flag_offset)

let store_arena_flag span source access =
  Encoder.Store_arena
    (encoder_arena_slot span access.initialized_flag_offset, source)

let load_reference_scalar span destination base scalar =
  if scalar.byte_size = 8 then Encoder.Load_indirect (destination, base, 0)
  else
    Encoder.Load_indirect_narrow
      ( destination,
        base,
        narrow_frame_width ?span scalar.byte_size,
        if scalar.word_type = I64 then Encoder.Sign_extend
        else Encoder.Zero_extend )

let store_reference_scalar span base scalar source =
  if scalar.byte_size = 8 then Encoder.Store_indirect (base, source)
  else
    Encoder.Store_indirect_narrow
      (base, narrow_frame_width ?span scalar.byte_size, source)

type callable_code_owner = {
  callable_owner_id : int;
  callable_owner_address : int;
  callable_owner_target : int;
}

let print_argument_kind type_ =
  if Type.pointer_depth type_ = 0 then Print_codegen.Word
  else
    match Type.base type_ with
    | Type.Primitive (_, Primitive.U8) -> Print_codegen.Unsigned_byte_pointer
    | Type.Primitive (_, Primitive.I8) -> Print_codegen.Signed_byte_pointer
    | _ -> Print_codegen.Other_pointer

let allocate_body ?callable_frame ?(shared_values = [])
    ?(provider_entry_start = Int.max_int) ?(function_code_owners = [||])
    ?undefined_code_owner ?(status_abi = Encoder.System_v_x64) ~max_stack_bytes
    ~reserved_registers ~supply ~mode prepared =
  let function_owner index =
    if index < Array.length function_code_owners then
      function_code_owners.(index)
    else None
  in
  let function_owner_word index =
    match function_owner index with
    | None -> Int64.of_int (index + 1)
    | Some owner -> Int64.of_int owner.callable_owner_id
  in
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
  let shared_count = List.length shared_values in
  if shared_count > hard_max_stack_bytes / 8 then
    reject "HCBACK0004" "native shared-value frame exceeds the stack limit";
  let slot_high_water = ref shared_count in
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
  let shared_slots =
    List.mapi
      (fun index value -> (value.value_id, (index, value)))
      shared_values
    |> List.fold_left
         (fun map (id, entry) -> Value_map.add id entry map)
         Value_map.empty
  in
  let shared_slot value = Value_map.find_opt value.value_id shared_slots in
  (* Graph dominance prevents loading a shared home before its producer. *)
  List.iteri (fun index value -> slots.(index) <- Some value) shared_values;
  if frame_size_for_spills shared_count > max_stack_bytes then
    reject "HCBACK0004" "native shared-value frame exceeds max_stack_bytes";
  let published = ref Value_set.empty in
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
      match shared_slot value with
      | Some (index, _) -> index
      | None -> (
          match find shared_count with
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
              index)
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
      else if excluded index extra || List.mem index protected then
        find (index + 1)
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
  let emit_update span update word (arithmetic_site : fault_site option) =
    let emit = emit span in
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
              { label = overflow_label; kind_value = 2; site_value = site.site }
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
  let fault_label kind site =
    let label = fresh_label supply in
    fault_blocks :=
      { label; kind_value = kind; site_value = site } :: !fault_blocks;
    label
  in
  let emit_doubles span register count =
    for _ = 1 to count do
      emit span (Encoder.Binary (Encoder.Add, register, register))
    done
  in
  let emit_bounds span site ~one_past ~scalar ~offset ~extent =
    let bounds_fault = fault_label 10 site in
    emit span (Encoder.Test offset);
    emit_branch Less bounds_fault;
    if not one_past then (
      emit span (Encoder.Mov_imm64 (Encoder.Rax, Int64.of_int scalar.byte_size));
      emit span (Encoder.Binary (Encoder.Sub, extent, Encoder.Rax));
      emit_branch Below bounds_fault);
    emit span (Encoder.Cmp (extent, offset));
    emit_branch Below bounds_fault
  in
  let emit_flag_check span site scalar ~flag_base ~offset =
    let initialized = fresh_label supply in
    let uninitialized = fault_label 7 site in
    emit span (Encoder.Test flag_base);
    emit_branch Equal initialized;
    emit span (Encoder.Binary (Encoder.Sub, flag_base, offset));
    for byte = 0 to scalar.byte_size - 1 do
      if byte > 0 then emit span (Encoder.Dec flag_base);
      emit span
        (Encoder.Load_indirect_narrow
           (Encoder.Rax, flag_base, Encoder.Frame8, Encoder.Zero_extend));
      emit span (Encoder.Test Encoder.Rax);
      emit_branch Equal uninitialized
    done;
    mark initialized
  in
  let emit_flag_store span scalar ~flag_base ~offset =
    let no_flag = fresh_label supply in
    emit span (Encoder.Test flag_base);
    emit_branch Equal no_flag;
    emit span (Encoder.Binary (Encoder.Sub, flag_base, offset));
    emit span (Encoder.Mov_imm64 (Encoder.Rax, 1L));
    for byte = 0 to scalar.byte_size - 1 do
      if byte > 0 then emit span (Encoder.Dec flag_base);
      emit span
        (Encoder.Store_indirect_narrow (flag_base, Encoder.Frame8, Encoder.Rax))
    done;
    mark no_flag
  in
  let initialized_pattern bytes =
    let pattern = ref 0L in
    for byte = 0 to bytes - 1 do
      pattern := Int64.logor !pattern (Int64.shift_left 1L ((7 - byte) * 8))
    done;
    !pattern
  in
  let check_full_flag span target bytes uninitialized =
    (* Whole original scalar reads need every byte, including after partial
       stores through a narrower view. No source-visible flag bits are exposed. *)
    let target_index =
      Array.to_list registers
      |> List.find_index (fun register -> register = target)
      |> Option.get
    in
    let scratch = acquire_empty span ~protected:[ target_index ] ~excluded:[] in
    emit span
      (Encoder.Mov_imm64 (registers.(scratch), initialized_pattern bytes));
    emit span (Encoder.Cmp (target, registers.(scratch)));
    emit_branch Not_equal uninitialized;
    owners.(scratch) <- None
  in
  let emit_reference_data span target = function
    | Variadic_reference origin ->
        emit span
          (Encoder.Address_frame
             (target, encoder_scalar_frame_slot span origin.data_offset))
    | Frame_reference origin ->
        emit span
          (Encoder.Address_frame
             (target, encoder_scalar_frame_slot span origin.access.frame_offset))
    | Arena_reference access ->
        emit span
          (Encoder.Address_arena
             (target, encoder_arena_slot span access.arena_offset))
    | Literal_reference region ->
        emit span
          (Encoder.Address_arena
             ( target,
               encoder_arena_slot span (Literal_storage.data_offset region) ))
  in
  let emit_reference_flag span target = function
    | Frame_reference origin -> (
        match origin.access.initialized_flag_offset with
        | Some offset ->
            emit span
              (Encoder.Address_frame
                 (target, encoder_scalar_frame_slot span (offset + 7)))
        | None -> emit span (Encoder.Mov_imm64 (target, 0L)))
    | Arena_reference access ->
        emit span
          (Encoder.Address_arena
             ( target,
               encoder_arena_slot span (access.initialized_flag_offset + 7) ))
    | Literal_reference _ | Variadic_reference _ ->
        emit span (Encoder.Mov_imm64 (target, 0L))
  in
  (* The original tail extent is independent of writes to the source argc cell. *)
  let emit_reference_extent span target = function
    | Variadic_reference origin ->
        emit span
          (Encoder.Load_frame
             (target, encoder_frame_slot span origin.count_offset));
        emit_doubles span target 3
    | origin ->
        emit span
          (Encoder.Mov_imm64 (target, Int64.of_int (reference_extent origin)))
  in
  List.iteri
    (fun position (instruction : prepared_instruction) ->
      release_before position;
      active_push :=
        Option.map (fun (value, _, _, _) -> value) instruction.push_stage;
      let emit = emit instruction.span in
      let load_owner target value =
        match value.code_owner_offset with
        | None -> emit (Encoder.Mov_imm64 (target, 0L))
        | Some offset ->
            emit
              (Encoder.Load_frame
                 (target, encoder_frame_slot instruction.span offset))
      in
      let emit_function_address target index =
        match function_owner index with
        | None ->
            planned := Planned_function_address (target, index) :: !planned
        | Some owner ->
            emit
              (Encoder.Load_arena
                 ( target,
                   encoder_arena_slot instruction.span
                     owner.callable_owner_address ))
      in
      let require_numeric_owner ?(protected = []) value =
        if Option.is_some value.code_owner_offset then (
          let invalid = fresh_label supply in
          fault_blocks :=
            {
              label = invalid;
              kind_value = 25;
              site_value = Option.get instruction.site;
            }
            :: !fault_blocks;
          let scratch =
            acquire_empty instruction.span ~protected ~excluded:[]
          in
          load_owner registers.(scratch) value;
          emit (Encoder.Test registers.(scratch));
          emit_branch Not_equal invalid;
          owners.(scratch) <- None)
      in
      let publish_indexed_owner input result =
        (* RCX still holds the checked byte offset. Callback elements and their
           private owners are both eight bytes wide. Frame metadata grows
           downwards; arena metadata grows upwards. Publish before [assign]
           releases the index, and leave the data result in RAX intact. *)
        let home =
          match instruction.operation with
          | Load_code_indexed (_, home, _) | Store_code_indexed (_, home, _, _)
            -> Some home
          | _ -> None
        in
        Option.iter
          (fun home ->
            (match home with
            | Frame_table offset ->
                emit
                  (Encoder.Address_frame
                     (Encoder.Rdx, encoder_frame_slot instruction.span offset));
                emit (Encoder.Binary (Encoder.Sub, Encoder.Rdx, Encoder.Rcx))
            | Arena_table offset ->
                emit
                  (Encoder.Address_arena
                     (Encoder.Rdx, encoder_arena_slot instruction.span offset));
                emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx)));
            (match input with
            | None -> emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 0))
            | Some input ->
                load_owner Encoder.R8 input;
                emit
                  (Encoder.Store_indirect_offset (Encoder.Rdx, 0, Encoder.R8)));
            emit
              (Encoder.Store_frame
                 ( encoder_frame_slot instruction.span
                     (Option.get result.code_owner_offset),
                   Encoder.R8 )))
          home
      in
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
      | Load_function_address (result, callee_index) ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          emit_function_address registers.(destination) callee_index;
          assign position destination result
      | Load_function_slot_cursor (result, slot) ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          emit
            (Encoder.Address_arena
               ( registers.(destination),
                 encoder_arena_slot instruction.span
                   (Global_storage.function_slot_address slot) ));
          assign position destination result
      | Load_function_slot (cursor, _, result) ->
          let inputs, protected = ensure_inputs instruction.span [ cursor ] in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          emit
            (Encoder.Load_indirect
               (registers.(destination), registers.(List.hd inputs), 0));
          assign position destination result
      | Load_undefined_function_address result ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          let owner = Option.get undefined_code_owner in
          emit
            (Encoder.Load_arena
               ( registers.(destination),
                 encoder_arena_slot instruction.span
                   (Global_storage.undefined_code_owner_address owner) ));
          assign position destination result
      | Apply_unary (unary, input, result) ->
          require_numeric_owner input;
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
          require_numeric_owner left;
          require_numeric_owner right;
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
      | Apply_constant_shift (shift, input, count, result) ->
          require_numeric_owner input;
          let inputs, protected = ensure_inputs instruction.span [ input ] in
          let source = List.hd inputs in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          if destination <> source then
            emit (Encoder.Mov (registers.(destination), registers.(source)));
          emit (Encoder.Shift_immediate (shift, registers.(destination), count));
          assign position destination result
      | Apply_shift (shift, left, count, result) ->
          require_numeric_owner left;
          require_numeric_owner count;
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
          require_numeric_owner left;
          require_numeric_owner right;
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
          require_numeric_owner left;
          require_numeric_owner right;
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
      | Apply_code_comparison (condition, left, right, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span left rdx;
          copy_value_to instruction.span right r8;
          load_owner Encoder.Rax left;
          load_owner Encoder.Rcx right;
          let plain_left = fresh_label supply
          and both_plain = fresh_label supply
          and both_owned = fresh_label supply
          and different = fresh_label supply
          and complete = fresh_label supply
          and invalid = fresh_label supply in
          fault_blocks :=
            {
              label = invalid;
              kind_value = 21;
              site_value = Option.get instruction.site;
            }
            :: !fault_blocks;
          emit (Encoder.Test Encoder.Rax);
          emit_branch Equal plain_left;
          emit (Encoder.Test Encoder.Rcx);
          emit_branch Not_equal both_owned;
          emit (Encoder.Test Encoder.R8);
          emit_branch Not_equal invalid;
          emit_branch Unconditional different;
          mark plain_left;
          emit (Encoder.Test Encoder.Rcx);
          emit_branch Equal both_plain;
          emit (Encoder.Test Encoder.Rdx);
          emit_branch Not_equal invalid;
          emit_branch Unconditional different;
          mark both_owned;
          emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx));
          emit (Encoder.Setcc (condition, Encoder.Rax));
          emit (Encoder.Movzx8 (Encoder.Rax, Encoder.Rax));
          emit_branch Unconditional complete;
          mark both_plain;
          emit (Encoder.Cmp (Encoder.Rdx, Encoder.R8));
          emit (Encoder.Setcc (condition, Encoder.Rax));
          emit (Encoder.Movzx8 (Encoder.Rax, Encoder.Rax));
          emit_branch Unconditional complete;
          mark different;
          emit
            (Encoder.Mov_imm64
               (Encoder.Rax, if condition = Encoder.E then 0L else 1L));
          mark complete;
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Apply_reference_comparison (condition, left, right, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span left rdx;
          copy_value_to instruction.span right rcx;
          let different = fresh_label supply in
          let complete = fresh_label supply in
          (* Separate address sites can own separate descriptor snapshots. The
             original data/flags/offset/extent identify the same live object
             independently of which table produced its descriptor. *)
          List.iter
            (fun offset ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, offset));
              emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rcx, offset));
              emit (Encoder.Cmp (Encoder.Rax, Encoder.R8));
              emit_branch Not_equal different)
            [ 0; 8; 16; 24 ];
          let same = if condition = Encoder.E then 1L else 0L in
          emit (Encoder.Mov_imm64 (Encoder.Rax, same));
          emit_branch Unconditional complete;
          mark different;
          emit (Encoder.Mov_imm64 (Encoder.Rax, Int64.sub 1L same));
          mark complete;
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Apply_reference_ordering (condition, left, right, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span left rdx;
          copy_value_to instruction.span right rcx;
          let mismatch = fresh_label supply in
          fault_blocks :=
            {
              label = mismatch;
              kind_value = 17;
              site_value = Option.get instruction.site;
            }
            :: !fault_blocks;
          List.iter
            (fun offset ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, offset));
              emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rcx, offset));
              emit (Encoder.Cmp (Encoder.Rax, Encoder.R8));
              emit_branch Not_equal mismatch)
            [ 0; 8; 24 ];
          emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, 16));
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rcx, 16));
          emit (Encoder.Cmp (Encoder.Rax, Encoder.R8));
          emit (Encoder.Setcc (condition, Encoder.Rax));
          emit (Encoder.Movzx8 (Encoder.Rax, Encoder.Rax));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Apply_reference_difference (left, right, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span left rdx;
          copy_value_to instruction.span right rcx;
          let mismatch = fresh_label supply in
          fault_blocks :=
            {
              label = mismatch;
              kind_value = 18;
              site_value = Option.get instruction.site;
            }
            :: !fault_blocks;
          List.iter
            (fun offset ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, offset));
              emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rcx, offset));
              emit (Encoder.Cmp (Encoder.Rax, Encoder.R8));
              emit_branch Not_equal mismatch)
            [ 0; 8; 24 ];
          emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, 16));
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rcx, 16));
          emit (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.R8));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Apply_logical_not (input, result) ->
          require_numeric_owner input;
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
          require_numeric_owner left;
          require_numeric_owner right;
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
      | Scale_index (word, stride, index, result) ->
          spill_all_registers instruction.span;
          let overflow = fault_label 8 (Option.get instruction.site) in
          copy_value_to instruction.span index rax;
          (match word with
          | U64 ->
              emit (Encoder.Test Encoder.Rax);
              emit_branch Less overflow
          | I64 -> ());
          emit (Encoder.Mov_imm64 (Encoder.Rcx, stride));
          emit (Encoder.Binary (Encoder.Imul, Encoder.Rax, Encoder.Rcx));
          emit_branch Overflow overflow;
          note_peak ~temporaries:[ rax; rcx ] ();
          assign position rax result
      | Apply_index_offset (operation, base, delta, result) ->
          spill_all_registers instruction.span;
          let overflow = fault_label 9 (Option.get instruction.site) in
          (match base with
          | Index_zero -> emit (Encoder.Mov_imm64 (Encoder.Rax, 0L))
          | Index_value offset -> copy_value_to instruction.span offset rax
          | Index_reference reference ->
              copy_value_to instruction.span reference rdx;
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, 16)));
          copy_value_to instruction.span delta rcx;
          emit (Encoder.Binary (operation, Encoder.Rax, Encoder.Rcx));
          emit_branch Overflow overflow;
          note_peak ~temporaries:[ rax; rcx; rdx ] ();
          assign position rax result
      | Materialize_reference (origin, table, target_offset, result) ->
          spill_all_registers instruction.span;
          let scalar = reference_scalar origin in
          (match target_offset with
          | None -> emit (Encoder.Mov_imm64 (Encoder.Rcx, 0L))
          | Some offset ->
              copy_value_to instruction.span offset rcx;
              emit_reference_extent instruction.span Encoder.R8 origin;
              emit_bounds instruction.span
                (Option.get instruction.site)
                ~one_past:true ~scalar ~offset:Encoder.Rcx ~extent:Encoder.R8);
          (match table with
          | Frame_table offset ->
              emit
                (Encoder.Address_frame
                   (Encoder.Rdx, encoder_frame_slot instruction.span offset))
          | Arena_table offset ->
              emit
                (Encoder.Address_arena
                   (Encoder.Rdx, encoder_arena_slot instruction.span offset)));
          emit_reference_data instruction.span Encoder.Rax origin;
          emit (Encoder.Store_indirect_offset (Encoder.Rdx, 0, Encoder.Rax));
          emit_reference_flag instruction.span Encoder.Rax origin;
          emit (Encoder.Store_indirect_offset (Encoder.Rdx, 8, Encoder.Rax));
          emit (Encoder.Store_indirect_offset (Encoder.Rdx, 16, Encoder.Rcx));
          emit_reference_extent instruction.span Encoder.Rax origin;
          emit (Encoder.Store_indirect_offset (Encoder.Rdx, 24, Encoder.Rax));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rdx result
      | Materialize_existing_reference (access, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span access.reference rdx;
          (match access.offset with
          | None -> emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16))
          | Some offset ->
              copy_value_to instruction.span offset rcx;
              emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
              emit_bounds instruction.span
                (Option.get instruction.site)
                ~one_past:true ~scalar:access.scalar ~offset:Encoder.Rcx
                ~extent:Encoder.R8);
          emit
            (Encoder.Address_frame
               ( Encoder.R8,
                 encoder_frame_slot instruction.span
                   (Option.get result.reference_descriptor_offset) ));
          List.iter
            (fun offset ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, offset));
              emit
                (Encoder.Store_indirect_offset (Encoder.R8, offset, Encoder.Rax)))
            [ 0; 8; 24 ];
          emit (Encoder.Store_indirect_offset (Encoder.R8, 16, Encoder.Rcx));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position r8 result
      | Load_reference_value (access, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span access.reference rdx;
          (match access.offset with
          | None -> emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16))
          | Some offset -> copy_value_to instruction.span offset rcx);
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
          emit_bounds instruction.span
            (Option.get instruction.site)
            ~one_past:false ~scalar:access.scalar ~offset:Encoder.Rcx
            ~extent:Encoder.R8;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 8));
          emit (Encoder.Load_indirect (Encoder.Rdx, Encoder.Rdx, 0));
          emit_flag_check instruction.span
            (Option.get instruction.site)
            access.scalar ~flag_base:Encoder.R8 ~offset:Encoder.Rcx;
          emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx));
          emit
            (load_reference_scalar instruction.span Encoder.Rax Encoder.Rdx
               access.scalar);
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Store_reference_value (access, input, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span access.reference rdx;
          (match access.offset with
          | None -> emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16))
          | Some offset -> copy_value_to instruction.span offset rcx);
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
          emit_bounds instruction.span
            (Option.get instruction.site)
            ~one_past:false ~scalar:access.scalar ~offset:Encoder.Rcx
            ~extent:Encoder.R8;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 8));
          emit (Encoder.Load_indirect (Encoder.Rdx, Encoder.Rdx, 0));
          emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx));
          copy_value_to instruction.span input rax;
          emit
            (store_reference_scalar instruction.span Encoder.Rdx access.scalar
               Encoder.Rax);
          emit_flag_store instruction.span access.scalar ~flag_base:Encoder.R8
            ~offset:Encoder.Rcx;
          copy_value_to instruction.span input rax;
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Load_indexed_object_value (access, result)
      | Load_code_indexed (access, _, result) ->
          spill_all_registers instruction.span;
          let scalar = reference_scalar access.origin in
          copy_value_to instruction.span access.offset rcx;
          emit_reference_extent instruction.span Encoder.R8 access.origin;
          emit_bounds instruction.span
            (Option.get instruction.site)
            ~one_past:false ~scalar ~offset:Encoder.Rcx ~extent:Encoder.R8;
          emit_reference_flag instruction.span Encoder.R8 access.origin;
          emit_reference_data instruction.span Encoder.Rdx access.origin;
          emit_flag_check instruction.span
            (Option.get instruction.site)
            scalar ~flag_base:Encoder.R8 ~offset:Encoder.Rcx;
          emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx));
          emit
            (load_reference_scalar instruction.span Encoder.Rax Encoder.Rdx
               scalar);
          publish_indexed_owner None result;
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Store_indexed_object_value (access, input, result)
      | Store_code_indexed (access, _, input, result) ->
          spill_all_registers instruction.span;
          let scalar = reference_scalar access.origin in
          copy_value_to instruction.span access.offset rcx;
          emit_reference_extent instruction.span Encoder.R8 access.origin;
          emit_bounds instruction.span
            (Option.get instruction.site)
            ~one_past:false ~scalar ~offset:Encoder.Rcx ~extent:Encoder.R8;
          emit_reference_flag instruction.span Encoder.R8 access.origin;
          emit_reference_data instruction.span Encoder.Rdx access.origin;
          emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx));
          copy_value_to instruction.span input rax;
          emit
            (store_reference_scalar instruction.span Encoder.Rdx scalar
               Encoder.Rax);
          emit_flag_store instruction.span scalar ~flag_base:Encoder.R8
            ~offset:Encoder.Rcx;
          copy_value_to instruction.span input rax;
          publish_indexed_owner (Some input) result;
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position rax result
      | Load_reference_frame (access, result) ->
          spill_all_registers instruction.span;
          Option.iter
            (fun offset ->
              let uninitialized = fault_label 7 (Option.get instruction.site) in
              emit
                (Encoder.Load_frame
                   (Encoder.Rax, encoder_frame_slot instruction.span offset));
              check_full_flag instruction.span Encoder.Rax 8 uninitialized)
            access.initialized_flag_offset;
          emit (load_frame_scalar instruction.span Encoder.Rdx access);
          emit
            (Encoder.Address_frame
               ( Encoder.R8,
                 encoder_frame_slot instruction.span
                   (Option.get result.reference_descriptor_offset) ));
          List.iter
            (fun offset ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, offset));
              emit
                (Encoder.Store_indirect_offset (Encoder.R8, offset, Encoder.Rax)))
            [ 0; 8; 16; 24 ];
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position r8 result
      | Store_reference_frame (access, home, input, result) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span input rdx;
          emit
            (Encoder.Address_frame
               (Encoder.R8, encoder_frame_slot instruction.span home));
          List.iter
            (fun offset ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, offset));
              emit
                (Encoder.Store_indirect_offset (Encoder.R8, offset, Encoder.Rax)))
            [ 0; 8; 16; 24 ];
          emit (store_frame_scalar instruction.span access Encoder.R8);
          Option.iter
            (fun offset ->
              emit (Encoder.Mov_imm64 (Encoder.Rax, initialized_pattern 8));
              emit
                (Encoder.Store_frame
                   (encoder_frame_slot instruction.span offset, Encoder.Rax)))
            access.initialized_flag_offset;
          emit
            (Encoder.Address_frame
               ( Encoder.R8,
                 encoder_frame_slot instruction.span
                   (Option.get result.reference_descriptor_offset) ));
          List.iter
            (fun offset ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, offset));
              emit
                (Encoder.Store_indirect_offset (Encoder.R8, offset, Encoder.Rax)))
            [ 0; 8; 16; 24 ];
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position r8 result
      | Load_frame_value (access, result) | Load_code_frame (access, _, result)
        ->
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
              check_full_flag instruction.span target access.frame_bytes
                uninitialized)
            access.initialized_flag_offset;
          emit (load_frame_scalar instruction.span target access);
          assign position destination result
      | Store_frame_value (access, input, result)
      | Store_code_frame (access, _, input, result) ->
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
              emit
                (Encoder.Mov_imm64
                   (registers.(scratch), initialized_pattern access.frame_bytes));
              emit
                (Encoder.Store_frame
                   ( encoder_frame_slot instruction.span flag_offset,
                     registers.(scratch) ));
              owners.(scratch) <- None)
            access.initialized_flag_offset;
          assign position destination result
      | Load_arena_value (access, result) | Load_code_arena (access, _, result)
        ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          let target = registers.(destination) in
          let site = Option.get instruction.site in
          let uninitialized = fresh_label supply in
          fault_blocks :=
            { label = uninitialized; kind_value = 7; site_value = site }
            :: !fault_blocks;
          emit (load_arena_flag instruction.span target access);
          check_full_flag instruction.span target access.arena_bytes
            uninitialized;
          emit (load_arena_scalar instruction.span target access);
          assign position destination result
      | Store_arena_value (access, input, result)
      | Store_code_arena (access, _, input, result) ->
          let inputs, protected = ensure_inputs instruction.span [ input ] in
          let source = List.hd inputs in
          let destination =
            acquire_destination instruction.span position ~protected
              ~excluded:[]
          in
          if destination <> source then
            emit (Encoder.Mov (registers.(destination), registers.(source)));
          emit
            (store_arena_scalar instruction.span access registers.(destination));
          let scratch =
            acquire_empty instruction.span ~protected:[ destination ]
              ~excluded:[]
          in
          emit
            (Encoder.Mov_imm64
               (registers.(scratch), initialized_pattern access.arena_bytes));
          emit (store_arena_flag instruction.span registers.(scratch) access);
          owners.(scratch) <- None;
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
              check_full_flag instruction.span Encoder.Rax access.frame_bytes
                uninitialized)
            access.initialized_flag_offset;
          emit (load_frame_scalar instruction.span Encoder.Rax access);
          if old_result then emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          Option.iter (require_numeric_owner ~protected:[ rax; rdx; r8 ]) input;
          (match input with
          | Some input -> copy_value_to instruction.span input rcx
          | None -> emit (Encoder.Mov_imm64 (Encoder.Rcx, 1L)));
          let computed_index =
            emit_update instruction.span update word arithmetic_site
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
      | Update_callback_value
          (access, update, input, old_result, result, arithmetic_site) ->
          spill_all_registers instruction.span;
          let site = Option.get instruction.site in
          let owned = fresh_label supply in
          fault_blocks :=
            { label = owned; kind_value = 22; site_value = site }
            :: !fault_blocks;
          (match access with
          | Callback_frame (access, owner_offset) ->
              Option.iter
                (fun offset ->
                  let uninitialized = fault_label 7 site in
                  emit
                    (Encoder.Load_frame
                       (Encoder.Rax, encoder_frame_slot instruction.span offset));
                  check_full_flag instruction.span Encoder.Rax
                    access.frame_bytes uninitialized)
                access.initialized_flag_offset;
              emit
                (Encoder.Address_frame
                   ( Encoder.Rdx,
                     encoder_scalar_frame_slot instruction.span
                       access.frame_offset ));
              emit
                (Encoder.Load_frame
                   ( Encoder.Rax,
                     encoder_frame_slot instruction.span owner_offset ))
          | Callback_arena (access, owner_offset) ->
              let uninitialized = fault_label 7 site in
              emit (load_arena_flag instruction.span Encoder.Rax access);
              check_full_flag instruction.span Encoder.Rax access.arena_bytes
                uninitialized;
              emit
                (Encoder.Address_arena
                   ( Encoder.Rdx,
                     encoder_arena_slot instruction.span access.arena_offset ));
              emit
                (Encoder.Load_arena
                   ( Encoder.Rax,
                     encoder_arena_slot instruction.span owner_offset ))
          | Callback_indexed (access, home) ->
              let scalar = reference_scalar access.origin in
              copy_value_to instruction.span access.offset rcx;
              emit_reference_extent instruction.span Encoder.R8 access.origin;
              emit_bounds instruction.span site ~one_past:false ~scalar
                ~offset:Encoder.Rcx ~extent:Encoder.R8;
              emit_reference_flag instruction.span Encoder.R8 access.origin;
              emit_reference_data instruction.span Encoder.Rdx access.origin;
              emit_flag_check instruction.span site scalar ~flag_base:Encoder.R8
                ~offset:Encoder.Rcx;
              emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx));
              (match home with
              | Frame_table offset ->
                  emit
                    (Encoder.Address_frame
                       (Encoder.R8, encoder_frame_slot instruction.span offset));
                  emit (Encoder.Binary (Encoder.Sub, Encoder.R8, Encoder.Rcx))
              | Arena_table offset ->
                  emit
                    (Encoder.Address_arena
                       (Encoder.R8, encoder_arena_slot instruction.span offset));
                  emit (Encoder.Binary (Encoder.Add, Encoder.R8, Encoder.Rcx)));
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.R8, 0)));
          emit (Encoder.Test Encoder.Rax);
          emit_branch Not_equal owned;
          emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, 0));
          if old_result then emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          Option.iter (require_numeric_owner ~protected:[ rax; rdx; r8 ]) input;
          (match input with
          | Some input -> copy_value_to instruction.span input rcx
          | None -> emit (Encoder.Mov_imm64 (Encoder.Rcx, 8L)));
          let target =
            match update with
            | Update_division _ ->
                emit (Encoder.Mov (Encoder.R8, Encoder.Rdx));
                Encoder.R8
            | Update_binary _ | Update_shift _ -> Encoder.Rdx
          in
          let computed_index =
            emit_update instruction.span update I64 arithmetic_site
          in
          emit
            (Encoder.Store_indirect_offset
               (target, 0, registers.(computed_index)));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position (if old_result then r8 else computed_index) result
      | Update_arena_value
          (access, update, input, old_result, result, word, arithmetic_site) ->
          spill_all_registers instruction.span;
          let site = Option.get instruction.site in
          let uninitialized = fresh_label supply in
          fault_blocks :=
            { label = uninitialized; kind_value = 7; site_value = site }
            :: !fault_blocks;
          emit (load_arena_flag instruction.span Encoder.Rax access);
          check_full_flag instruction.span Encoder.Rax access.arena_bytes
            uninitialized;
          emit (load_arena_scalar instruction.span Encoder.Rax access);
          if old_result then emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          Option.iter (require_numeric_owner ~protected:[ rax; rdx; r8 ]) input;
          (match input with
          | Some input -> copy_value_to instruction.span input rcx
          | None -> emit (Encoder.Mov_imm64 (Encoder.Rcx, 1L)));
          let computed_index =
            emit_update instruction.span update word arithmetic_site
          in
          emit
            (store_arena_scalar instruction.span access
               registers.(computed_index));
          if (not old_result) && Option.is_none input && access.arena_bytes < 8
          then
            emit
              (load_arena_scalar instruction.span registers.(computed_index)
                 access);
          note_peak
            ~temporaries:
              (if old_result then [ rax; rcx; rdx; r8 ] else [ rax; rcx; rdx ])
            ();
          assign position (if old_result then r8 else computed_index) result
      | Update_indexed_object_value
          (access, update, input, old_result, result, word, arithmetic_site) ->
          spill_all_registers instruction.span;
          let scalar = reference_scalar access.origin in
          copy_value_to instruction.span access.offset rcx;
          emit_reference_extent instruction.span Encoder.R8 access.origin;
          emit_bounds instruction.span
            (Option.get instruction.site)
            ~one_past:false ~scalar ~offset:Encoder.Rcx ~extent:Encoder.R8;
          emit_reference_flag instruction.span Encoder.R8 access.origin;
          emit_reference_data instruction.span Encoder.Rdx access.origin;
          emit_flag_check instruction.span
            (Option.get instruction.site)
            scalar ~flag_base:Encoder.R8 ~offset:Encoder.Rcx;
          emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx));
          emit
            (load_reference_scalar instruction.span Encoder.Rax Encoder.Rdx
               scalar);
          if old_result then emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          Option.iter (require_numeric_owner ~protected:[ rax; rdx; r8 ]) input;
          (match input with
          | Some input -> copy_value_to instruction.span input rcx
          | None -> emit (Encoder.Mov_imm64 (Encoder.Rcx, 1L)));
          let target =
            match update with
            | Update_division _ ->
                emit (Encoder.Mov (Encoder.R8, Encoder.Rdx));
                Encoder.R8
            | Update_binary _ | Update_shift _ -> Encoder.Rdx
          in
          let computed_index =
            emit_update instruction.span update word arithmetic_site
          in
          emit
            (store_reference_scalar instruction.span target scalar
               registers.(computed_index));
          if (not old_result) && Option.is_none input && scalar.byte_size < 8
          then
            emit
              (load_reference_scalar instruction.span registers.(computed_index)
                 target scalar);
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position (if old_result then r8 else computed_index) result
      | Update_reference_value
          (access, update, input, old_result, result, word, arithmetic_site) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span access.reference rdx;
          (match access.offset with
          | None -> emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16))
          | Some offset -> copy_value_to instruction.span offset rcx);
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
          emit_bounds instruction.span
            (Option.get instruction.site)
            ~one_past:false ~scalar:access.scalar ~offset:Encoder.Rcx
            ~extent:Encoder.R8;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 8));
          emit (Encoder.Load_indirect (Encoder.Rdx, Encoder.Rdx, 0));
          emit_flag_check instruction.span
            (Option.get instruction.site)
            access.scalar ~flag_base:Encoder.R8 ~offset:Encoder.Rcx;
          emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rcx));
          emit
            (load_reference_scalar instruction.span Encoder.Rax Encoder.Rdx
               access.scalar);
          if old_result then emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          Option.iter (require_numeric_owner ~protected:[ rax; rdx; r8 ]) input;
          (match input with
          | Some input -> copy_value_to instruction.span input rcx
          | None -> emit (Encoder.Mov_imm64 (Encoder.Rcx, 1L)));
          let target =
            match update with
            | Update_division _ ->
                emit (Encoder.Mov (Encoder.R8, Encoder.Rdx));
                Encoder.R8
            | Update_binary _ | Update_shift _ -> Encoder.Rdx
          in
          let computed_index =
            emit_update instruction.span update word arithmetic_site
          in
          emit
            (store_reference_scalar instruction.span target access.scalar
               registers.(computed_index));
          if
            (not old_result) && Option.is_none input
            && access.scalar.byte_size < 8
          then
            emit
              (load_reference_scalar instruction.span registers.(computed_index)
                 target access.scalar);
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          assign position (if old_result then r8 else computed_index) result
      | Call_start | Call_cleanup -> release_through position
      | Internal_integer (operation, input, result_stage) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span input rax;
          (match operation with
          | Intrinsic.To_upper ->
              let complete = fresh_label supply in
              emit (Encoder.Mov_imm64 (Encoder.Rcx, 97L));
              emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx));
              emit_branch Less complete;
              emit (Encoder.Mov_imm64 (Encoder.Rcx, 122L));
              emit (Encoder.Cmp (Encoder.Rcx, Encoder.Rax));
              emit_branch Less complete;
              emit (Encoder.Mov_imm64 (Encoder.Rcx, 32L));
              emit (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.Rcx));
              mark complete
          | Intrinsic.To_bool ->
              emit (Encoder.Test Encoder.Rax);
              emit (Encoder.Setcc (Encoder.NE, Encoder.Rax));
              emit (Encoder.Movzx8 (Encoder.Rax, Encoder.Rax))
          | Intrinsic.Absolute ->
              let negative = fresh_label supply in
              let complete = fresh_label supply in
              emit (Encoder.Test Encoder.Rax);
              emit_branch Less negative;
              emit_branch Unconditional complete;
              mark negative;
              emit (Encoder.Unary (Encoder.Neg, Encoder.Rax));
              mark complete
          | Intrinsic.Sign ->
              let negative = fresh_label supply in
              let complete = fresh_label supply in
              emit (Encoder.Test Encoder.Rax);
              emit_branch Equal complete;
              emit_branch Less negative;
              emit (Encoder.Mov_imm64 (Encoder.Rax, 1L));
              emit_branch Unconditional complete;
              mark negative;
              emit (Encoder.Mov_imm64 (Encoder.Rax, -1L));
              mark complete
          | Intrinsic.Scan_forward | Scan_reverse ->
              let nonzero = fresh_label supply in
              let complete = fresh_label supply in
              emit (Encoder.Test Encoder.Rax);
              emit_branch Not_equal nonzero;
              emit (Encoder.Mov_imm64 (Encoder.Rax, -1L));
              emit_branch Unconditional complete;
              mark nonzero;
              emit
                (match operation with
                | Intrinsic.Scan_forward ->
                    Encoder.Bsf (Encoder.Rax, Encoder.Rax)
                | _ -> Encoder.Bsr (Encoder.Rax, Encoder.Rax));
              mark complete
          | Intrinsic.Square_i64 | Square_u64 ->
              emit (Encoder.Binary (Encoder.Imul, Encoder.Rax, Encoder.Rax)));
          emit
            (Encoder.Store_stack
               (staged_stack_slot instruction.span result_stage, Encoder.Rax));
          note_peak ~temporaries:[ rax; rcx ] ();
          release_through position
      | Internal_binary (operation, left, right, result_stage) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span left rax;
          copy_value_to instruction.span right rcx;
          let complete = fresh_label supply in
          (match operation with
          | Intrinsic.Min_i64 | Min_u64 ->
              emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx))
          | Intrinsic.Max_i64 | Max_u64 ->
              emit (Encoder.Cmp (Encoder.Rcx, Encoder.Rax)));
          emit_branch Equal complete;
          emit_branch
            (match operation with
            | Intrinsic.Min_i64 | Max_i64 -> Less
            | Min_u64 | Max_u64 -> Below)
            complete;
          emit (Encoder.Mov (Encoder.Rax, Encoder.Rcx));
          mark complete;
          emit
            (Encoder.Store_stack
               (staged_stack_slot instruction.span result_stage, Encoder.Rax));
          note_peak ~temporaries:[ rax; rcx ] ();
          release_through position
      | Internal_bit (operation, reference, index, scalar, result_stage) ->
          spill_all_registers instruction.span;
          let site = Option.get instruction.site in
          let bounds_fault = fault_label 10 site in
          let stage = staged_stack_slot instruction.span result_stage in
          copy_value_to instruction.span index rax;
          emit (Encoder.Test Encoder.Rax);
          emit_branch Less bounds_fault;
          emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          emit (Encoder.Mov_imm64 (Encoder.Rcx, 7L));
          emit (Encoder.Binary (Encoder.And, Encoder.R8, Encoder.Rcx));
          emit (Encoder.Store_stack (stage, Encoder.R8));
          emit (Encoder.Mov_imm64 (Encoder.Rcx, 3L));
          emit (Encoder.Shift_cl (Encoder.Shr, Encoder.Rax));
          (* Quantize relative to the current view, which may begin at an
             unaligned byte inside its original object. *)
          emit (Encoder.Mov (Encoder.R8, Encoder.Rax));
          emit
            (Encoder.Mov_imm64 (Encoder.Rcx, Int64.of_int (scalar.byte_size - 1)));
          emit (Encoder.Binary (Encoder.And, Encoder.R8, Encoder.Rcx));
          emit_doubles instruction.span Encoder.R8 3;
          emit (Encoder.Load_stack (Encoder.Rcx, stage));
          emit (Encoder.Binary (Encoder.Or, Encoder.R8, Encoder.Rcx));
          emit (Encoder.Store_stack (stage, Encoder.R8));
          emit
            (Encoder.Mov_imm64 (Encoder.Rcx, Int64.of_int (-scalar.byte_size)));
          emit (Encoder.Binary (Encoder.And, Encoder.Rax, Encoder.Rcx));
          copy_value_to instruction.span reference rdx;
          emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16));
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
          emit_bounds instruction.span site ~one_past:true ~scalar
            ~offset:Encoder.Rcx ~extent:Encoder.R8;
          emit (Encoder.Binary (Encoder.Add, Encoder.Rcx, Encoder.Rax));
          emit_branch Overflow (fault_label 9 site);
          emit_bounds instruction.span site ~one_past:false ~scalar
            ~offset:Encoder.Rcx ~extent:Encoder.R8;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 8));
          emit_flag_check instruction.span site scalar ~flag_base:Encoder.R8
            ~offset:Encoder.Rcx;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 0));
          emit (Encoder.Binary (Encoder.Add, Encoder.R8, Encoder.Rcx));
          emit
            (load_reference_scalar instruction.span Encoder.Rdx Encoder.R8
               scalar);
          emit (Encoder.Load_stack (Encoder.Rcx, stage));
          let native_operation =
            match operation with
            | Intrinsic.Test_bit -> Encoder.Bt
            | Set_bit -> Encoder.Bts
            | Reset_bit -> Encoder.Btr
            | Complement_bit -> Encoder.Btc
          in
          emit (Encoder.Bit (native_operation, Encoder.Rdx, Encoder.Rcx));
          emit (Encoder.Mov_imm64 (Encoder.Rax, 0L));
          emit (Encoder.Setcc (Encoder.B, Encoder.Rax));
          if operation <> Intrinsic.Test_bit then
            emit
              (store_reference_scalar instruction.span Encoder.R8 scalar
                 Encoder.Rdx);
          emit (Encoder.Store_stack (stage, Encoder.Rax));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          release_through position
      | Internal_swap (left, left_scalar, right, right_scalar, scratch_stage) ->
          spill_all_registers instruction.span;
          let site = Option.get instruction.site in
          let address_stage = staged_stack_slot instruction.span scratch_stage
          and value_stage =
            staged_stack_slot instruction.span (scratch_stage + 1)
          in
          let load reference scalar =
            copy_value_to instruction.span reference rdx;
            emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16));
            emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
            emit_bounds instruction.span site ~one_past:false ~scalar
              ~offset:Encoder.Rcx ~extent:Encoder.R8;
            emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 8));
            emit_flag_check instruction.span site scalar ~flag_base:Encoder.R8
              ~offset:Encoder.Rcx;
            emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 0));
            emit (Encoder.Binary (Encoder.Add, Encoder.R8, Encoder.Rcx));
            emit
              (load_reference_scalar instruction.span Encoder.Rdx Encoder.R8
                 scalar)
          in
          load left left_scalar;
          emit (Encoder.Store_stack (address_stage, Encoder.R8));
          emit (Encoder.Store_stack (value_stage, Encoder.Rdx));
          load right right_scalar;
          (* BackC.HC reads both cells before writing the second, then the first. *)
          emit (Encoder.Load_stack (Encoder.Rax, value_stage));
          emit
            (store_reference_scalar instruction.span Encoder.R8 right_scalar
               Encoder.Rax);
          emit (Encoder.Load_stack (Encoder.R8, address_stage));
          emit
            (store_reference_scalar instruction.span Encoder.R8 left_scalar
               Encoder.Rdx);
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          release_through position
      | Internal_mod_u64 (reference, divisor, result_stage) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span reference rdx;
          emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16));
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
          let site = Option.get instruction.site in
          let scalar = { word_type = U64; byte_size = 8 } in
          emit_bounds instruction.span site ~one_past:false ~scalar
            ~offset:Encoder.Rcx ~extent:Encoder.R8;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 8));
          emit_flag_check instruction.span site scalar ~flag_base:Encoder.R8
            ~offset:Encoder.Rcx;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 0));
          emit (Encoder.Binary (Encoder.Add, Encoder.R8, Encoder.Rcx));
          emit (Encoder.Load_indirect (Encoder.Rax, Encoder.R8, 0));
          copy_value_to instruction.span divisor rcx;
          emit (Encoder.Test Encoder.Rcx);
          emit_branch Equal (fault_label 1 site);
          emit Encoder.Zero_edx;
          emit Encoder.Div_rcx;
          emit (Encoder.Store_indirect (Encoder.R8, Encoder.Rax));
          emit
            (Encoder.Store_stack
               (staged_stack_slot instruction.span result_stage, Encoder.Rdx));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          release_through position
      | Internal_strlen (reference, result_stage) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span reference rdx;
          emit (Encoder.Load_indirect (Encoder.Rcx, Encoder.Rdx, 16));
          let site = Option.get instruction.site in
          let bounds_fault = fault_label 10 site in
          let step_fault = fault_label 3 site in
          let probe = fresh_label supply in
          let in_bounds = fresh_label supply in
          let complete = fresh_label supply in
          mark probe;
          emit (Encoder.Test Encoder.Rcx);
          emit_branch Less bounds_fault;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 24));
          emit (Encoder.Cmp (Encoder.Rcx, Encoder.R8));
          emit_branch Below in_bounds;
          emit_branch Unconditional bounds_fault;
          mark in_bounds;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 8));
          emit_flag_check instruction.span site
            { word_type = U64; byte_size = 1 }
            ~flag_base:Encoder.R8 ~offset:Encoder.Rcx;
          emit (Encoder.Load_indirect (Encoder.R8, Encoder.Rdx, 0));
          emit (Encoder.Binary (Encoder.Add, Encoder.R8, Encoder.Rcx));
          emit
            (Encoder.Load_indirect_narrow
               (Encoder.Rax, Encoder.R8, Encoder.Frame8, Encoder.Zero_extend));
          emit (Encoder.Test Encoder.Rax);
          emit_branch Equal complete;
          (* The ordinary instruction tick pays for the first probe. Each
             further byte, including the terminator, consumes another step
             before accessing storage. This leaves output work untouched. *)
          emit (Encoder.Mov_imm64 (Encoder.Rax, 1L));
          emit (Encoder.Binary (Encoder.Add, Encoder.Rcx, Encoder.Rax));
          emit (Encoder.Test Encoder.R10);
          emit_branch Equal step_fault;
          emit (Encoder.Dec Encoder.R10);
          emit_branch Unconditional probe;
          mark complete;
          emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, 16));
          emit (Encoder.Binary (Encoder.Sub, Encoder.Rcx, Encoder.Rax));
          emit
            (Encoder.Store_stack
               (staged_stack_slot instruction.span result_stage, Encoder.Rcx));
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          release_through position
      | Print_output call ->
          spill_all_registers instruction.span;
          Print_codegen.emit
            {
              status_abi;
              instruction = emit;
              fresh = (fun () -> fresh_label supply);
              mark;
              branch =
                (fun branch target ->
                  emit_branch
                    (match branch with
                    | Print_codegen.Always -> Unconditional
                    | Print_codegen.Equal -> Equal
                    | Print_codegen.Not_equal -> Not_equal
                    | Print_codegen.Below -> Below
                    | Print_codegen.Less -> Less
                    | Print_codegen.Overflow -> Overflow)
                    target);
              fault =
                (fun kind -> fault_label kind (Option.get instruction.site));
              slot = staged_stack_slot instruction.span;
            }
            call;
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          release_through position
      | Put_chars stage ->
          spill_all_registers instruction.span;
          let site = Option.get instruction.site in
          let depth_fault = fault_label 4 site in
          let frame_fault = fault_label 5 site in
          let output_fault = fault_label 11 site in
          let work_fault = fault_label 12 site in
          (* The inlined provider holds one U64 ABI slot and one semantic call
             depth for this instruction. No other guest call can run inside it. *)
          emit (Encoder.Load_context (Encoder.Rcx, 56));
          emit (Encoder.Test Encoder.Rcx);
          emit_branch Equal depth_fault;
          emit (Encoder.Load_context (Encoder.Rcx, 48));
          emit (Encoder.Cmp_imm8 (Encoder.Rcx, 8));
          emit_branch Below frame_fault;
          emit
            (Encoder.Load_stack
               (Encoder.Rax, staged_stack_slot instruction.span stage));
          let loop = fresh_label supply in
          let shift = fresh_label supply in
          let complete = fresh_label supply in
          let charge () =
            emit (Encoder.Load_context (Encoder.Rcx, 96));
            emit (Encoder.Test Encoder.Rcx);
            emit_branch Equal work_fault;
            emit (Encoder.Dec Encoder.Rcx);
            emit (Encoder.Store_context (96, Encoder.Rcx))
          in
          mark loop;
          emit (Encoder.Test Encoder.Rax);
          emit_branch Equal complete;
          charge ();
          emit (Encoder.Mov (Encoder.Rdx, Encoder.Rax));
          emit (Encoder.Mov_imm64 (Encoder.R8, 255L));
          emit (Encoder.Binary (Encoder.And, Encoder.Rdx, Encoder.R8));
          emit (Encoder.Test Encoder.Rdx);
          emit_branch Equal shift;
          charge ();
          emit (Encoder.Load_context (Encoder.Rcx, 88));
          emit (Encoder.Test Encoder.Rcx);
          emit_branch Equal output_fault;
          emit (Encoder.Dec Encoder.Rcx);
          emit (Encoder.Store_context (88, Encoder.Rcx));
          emit (Encoder.Load_context (Encoder.R8, 80));
          emit (Encoder.Load_context (Encoder.Rcx, 104));
          emit (Encoder.Binary (Encoder.Add, Encoder.R8, Encoder.Rcx));
          emit
            (Encoder.Store_indirect_narrow
               (Encoder.R8, Encoder.Frame8, Encoder.Rdx));
          emit (Encoder.Mov_imm64 (Encoder.Rdx, 1L));
          emit (Encoder.Binary (Encoder.Add, Encoder.Rcx, Encoder.Rdx));
          emit (Encoder.Store_context (104, Encoder.Rcx));
          mark shift;
          emit (Encoder.Mov_imm64 (Encoder.Rcx, 8L));
          emit (Encoder.Shift_cl (Encoder.Shr, Encoder.Rax));
          emit_branch Unconditional loop;
          mark complete;
          note_peak ~temporaries:[ rax; rcx; rdx; r8 ] ();
          release_through position
      | Call_capture (captured, stage) ->
          let inputs, _ = ensure_inputs instruction.span [ captured ] in
          emit
            (Encoder.Store_stack
               ( staged_stack_slot instruction.span stage,
                 registers.(List.hd inputs) ));
          let scratch =
            acquire_empty instruction.span ~protected:inputs ~excluded:[]
          in
          load_owner registers.(scratch) captured;
          emit
            (Encoder.Store_stack
               ( staged_stack_slot instruction.span (stage + 1),
                 registers.(scratch) ));
          owners.(scratch) <- None;
          release_through position
      | Extern_signature_fault ->
          emit_branch Unconditional
            (fault_label 24 (Option.get instruction.site));
          release_through position
      | Undefined_extern_call ->
          emit_branch Unconditional
            (fault_label 23 (Option.get instruction.site));
          release_through position
      | Direct_call _ | Indirect_call _ ->
          spill_all_registers instruction.span;
          let emit_target ?captured_stage call =
            let site = Option.get instruction.site in
            let epilogue =
              match mode with
              | Callable_control { epilogue_label; _ } -> epilogue_label
              | Expression_control _ | Program_control _ ->
                  reject ?span:instruction.span "HCBACK0003"
                    "direct native call is outside callable allocation"
            in
            Array.iteri
              (fun fixed_index stage ->
                emit
                  (Encoder.Load_stack
                     (Encoder.Rax, staged_stack_slot instruction.span stage));
                emit
                  (Encoder.Store_stack
                     (fixed_stack_slot instruction.span fixed_index, Encoder.Rax)))
              call.argument_stage_slots;
            let owner_index = ref (Array.length call.argument_stage_slots) in
            Array.iter
              (Option.iter (fun stage ->
                   emit
                     (Encoder.Load_stack
                        (Encoder.Rax, staged_stack_slot instruction.span stage));
                   emit
                     (Encoder.Store_stack
                        ( fixed_stack_slot instruction.span !owner_index,
                          Encoder.Rax ));
                   incr owner_index))
              call.argument_owner_stages;
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
              (Encoder.Mov_imm64
                 (Encoder.Rcx, Int64.of_int call.activation_bytes));
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
              (Encoder.Mov_imm64
                 (Encoder.Rcx, Int64.of_int call.activation_bytes));
            emit (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.Rcx));
            emit (Encoder.Store_context (48, Encoder.Rax));
            emit (Encoder.Load_context (Encoder.Rax, 64));
            planned :=
              Planned_callee_stack (Encoder.Rcx, call.callee_index) :: !planned;
            emit (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.Rcx));
            emit (Encoder.Store_context (64, Encoder.Rax));
            if call.callee_index >= provider_entry_start then
              emit (Encoder.Store_context_imm (8, site));
            Option.iter
              (fun (kind_stage, kinds) ->
                Array.iteri
                  (fun index kind ->
                    emit
                      (Encoder.Mov_imm64
                         (Encoder.Rax, Print_codegen.argument_kind_tag kind));
                    emit
                      (Encoder.Store_stack
                         ( staged_stack_slot instruction.span
                             (kind_stage + index),
                           Encoder.Rax )))
                  kinds;
                emit
                  (Encoder.Address_stack
                     (Encoder.Rdx, fixed_stack_slot instruction.span 0));
                emit
                  (Encoder.Address_stack
                     (Encoder.R8, staged_stack_slot instruction.span kind_stage));
                emit
                  (Encoder.Mov_imm64
                     (Encoder.Rcx, Int64.of_int (Array.length kinds))))
              call.provider_arguments;
            (match captured_stage with
            | None -> (
                match call.named_slot_stage with
                | None -> planned := Planned_call call.callee_index :: !planned
                | Some stage ->
                    planned :=
                      Planned_function_address (Encoder.Rax, call.callee_index)
                      :: !planned;
                    emit
                      (Encoder.Store_stack
                         (staged_stack_slot instruction.span stage, Encoder.Rax));
                    emit
                      (Encoder.Call_stack
                         (staged_stack_slot instruction.span stage)))
            | Some stage ->
                emit
                  (Encoder.Call_stack (staged_stack_slot instruction.span stage)));
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
              (Encoder.Mov_imm64
                 (Encoder.Rcx, Int64.of_int call.activation_bytes));
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
            if call.callee_index >= provider_entry_start then
              emit (Encoder.Store_context_imm (8, 0));
            ()
          in
          (match instruction.operation with
          | Direct_call call -> emit_target call
          | Indirect_call indirect ->
              let site = Option.get instruction.site in
              let invalid = fresh_label supply
              and mismatch = fresh_label supply
              and complete = fresh_label supply in
              fault_blocks :=
                { label = invalid; kind_value = 19; site_value = site }
                :: { label = mismatch; kind_value = 20; site_value = site }
                :: !fault_blocks;
              let targets =
                List.map
                  (fun index -> (index, fresh_label supply))
                  !(indirect.owned_targets)
              in
              emit
                (Encoder.Load_stack
                   ( Encoder.Rax,
                     staged_stack_slot instruction.span indirect.captured_stage
                   ));
              List.iter
                (fun (index, label) ->
                  let next = fresh_label supply in
                  emit
                    (Encoder.Load_stack
                       ( Encoder.Rcx,
                         staged_stack_slot instruction.span
                           (indirect.captured_stage + 1) ));
                  emit
                    (Encoder.Mov_imm64 (Encoder.Rdx, function_owner_word index));
                  emit (Encoder.Cmp (Encoder.Rcx, Encoder.Rdx));
                  emit_branch Not_equal next;
                  emit_function_address Encoder.Rcx index;
                  emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx));
                  emit_branch Equal label;
                  mark next)
                targets;
              Option.iter
                (fun owner ->
                  let next = fresh_label supply in
                  emit
                    (Encoder.Load_stack
                       ( Encoder.Rcx,
                         staged_stack_slot instruction.span
                           (indirect.captured_stage + 1) ));
                  emit
                    (Encoder.Mov_imm64
                       ( Encoder.Rdx,
                         Int64.of_int
                           (Global_storage.undefined_code_owner_id owner) ));
                  emit (Encoder.Cmp (Encoder.Rcx, Encoder.Rdx));
                  emit_branch Not_equal next;
                  emit
                    (Encoder.Load_arena
                       ( Encoder.Rcx,
                         encoder_arena_slot instruction.span
                           (Global_storage.undefined_code_owner_address owner)
                       ));
                  emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx));
                  emit_branch Not_equal invalid;
                  let stack_fault = fault_label 6 site in
                  emit (Encoder.Load_context (Encoder.Rcx, 64));
                  emit (Encoder.Mov_imm64 (Encoder.Rdx, 16L));
                  emit (Encoder.Cmp (Encoder.Rcx, Encoder.Rdx));
                  emit (Encoder.Setcc (Encoder.B, Encoder.R8));
                  emit (Encoder.Movzx8 (Encoder.R8, Encoder.R8));
                  emit (Encoder.Test Encoder.R8);
                  emit_branch Not_equal stack_fault;
                  emit (Encoder.Binary (Encoder.Sub, Encoder.Rcx, Encoder.Rdx));
                  emit (Encoder.Store_context (64, Encoder.Rcx));
                  emit (Encoder.Store_context_imm (8, site));
                  emit
                    (Encoder.Call_stack
                       (staged_stack_slot instruction.span
                          indirect.captured_stage));
                  emit (Encoder.Load_context (Encoder.Rcx, 64));
                  emit (Encoder.Mov_imm64 (Encoder.Rdx, 16L));
                  emit (Encoder.Binary (Encoder.Add, Encoder.Rcx, Encoder.Rdx));
                  emit (Encoder.Store_context (64, Encoder.Rcx));
                  let epilogue =
                    match mode with
                    | Callable_control { epilogue_label; _ } -> epilogue_label
                    | _ ->
                        reject "HCBACK0003"
                          "native undefined callback lacks its original \
                           callable entry"
                  in
                  emit_branch Unconditional epilogue;
                  mark next)
                undefined_code_owner;
              emit_branch Unconditional invalid;
              List.iter
                (fun (index, label) ->
                  mark label;
                  let matches, call = indirect.target_call index in
                  if matches then (
                    emit_target ~captured_stage:indirect.captured_stage call;
                    emit_branch Unconditional complete)
                  else emit_branch Unconditional mismatch)
                targets;
              mark complete
          | _ -> assert false);
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
          require_numeric_owner input;
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
      | Load_saved_data (offset, result) ->
          let destination =
            acquire_destination instruction.span position ~protected:[]
              ~excluded:[]
          in
          emit
            (Encoder.Address_arena
               ( registers.(destination),
                 encoder_arena_slot instruction.span offset ));
          assign position destination result
      | Discard_data_default (input, offset) ->
          spill_all_registers instruction.span;
          copy_value_to instruction.span input rdx;
          emit
            (Encoder.Address_arena
               (Encoder.R8, encoder_arena_slot instruction.span offset));
          List.iter
            (fun field ->
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, field));
              emit
                (Encoder.Store_indirect_offset (Encoder.R8, field, Encoder.Rax)))
            [ 0; 8; 16; 24 ];
          emit (Encoder.Store_context_imm (40, offset));
          emit
            (Encoder.Store_context_imm
               (32, -(200_000 + Option.get instruction.site)));
          note_peak ~temporaries:[ rax; rdx; r8 ] ();
          release_through position
      | Discard_callback_default input -> (
          match mode with
          | Callable_control { is_entry = true; _ } ->
              let site = Option.get instruction.site in
              let invalid = fault_label 25 site in
              let complete = fresh_label supply
              and numeric = fresh_label supply in
              let inputs, _ = ensure_inputs instruction.span [ input ] in
              let source = List.hd inputs in
              let tag =
                acquire_empty instruction.span ~protected:inputs ~excluded:[]
              in
              load_owner registers.(tag) input;
              let target =
                acquire_empty instruction.span ~protected:(tag :: inputs)
                  ~excluded:[]
              in
              emit (Encoder.Test registers.(tag));
              emit_branch Equal numeric;
              Array.iteri
                (fun index owner ->
                  Option.iter
                    (fun _ ->
                      let next = fresh_label supply in
                      emit
                        (Encoder.Mov_imm64
                           (registers.(target), function_owner_word index));
                      emit (Encoder.Cmp (registers.(tag), registers.(target)));
                      emit_branch Not_equal next;
                      emit_function_address registers.(target) index;
                      emit
                        (Encoder.Cmp (registers.(source), registers.(target)));
                      emit_branch Not_equal invalid;
                      emit (Encoder.Store_context (40, registers.(tag)));
                      emit (Encoder.Store_context_imm (32, -(100_000 + site)));
                      emit_branch Unconditional complete;
                      mark next)
                    owner)
                function_code_owners;
              Option.iter
                (fun owner ->
                  let next = fresh_label supply in
                  emit
                    (Encoder.Mov_imm64
                       ( registers.(target),
                         Int64.of_int
                           (Global_storage.undefined_code_owner_id owner) ));
                  emit (Encoder.Cmp (registers.(tag), registers.(target)));
                  emit_branch Not_equal next;
                  emit
                    (Encoder.Load_arena
                       ( registers.(target),
                         encoder_arena_slot instruction.span
                           (Global_storage.undefined_code_owner_address owner)
                       ));
                  emit (Encoder.Cmp (registers.(source), registers.(target)));
                  emit_branch Not_equal invalid;
                  emit (Encoder.Store_context (40, registers.(tag)));
                  emit (Encoder.Store_context_imm (32, -(100_000 + site)));
                  emit_branch Unconditional complete;
                  mark next)
                undefined_code_owner;
              emit_branch Unconditional invalid;
              mark numeric;
              emit (Encoder.Store_context (40, registers.(source)));
              emit (Encoder.Store_context_imm (32, site));
              mark complete;
              owners.(tag) <- None;
              owners.(target) <- None;
              release_through position
          | _ ->
              reject ?span:instruction.span "HCBACK0003"
                "owned default capture requires its task entry")
      | Discard_value (input, _) -> (
          match mode with
          | Expression_control _ ->
              reject ?span:instruction.span "HCBACK0003"
                "native expression contains program discard"
          | Program_control _ | Callable_control { is_entry = true; _ } ->
              let site = Option.get instruction.site in
              let inputs, _ = ensure_inputs instruction.span [ input ] in
              let source = List.hd inputs in
              (match input.code_owner_offset with
              | None ->
                  emit (Encoder.Store_context (40, registers.(source)));
                  emit (Encoder.Store_context_imm (32, site))
              | Some _ ->
                  let owned = fresh_label supply
                  and complete = fresh_label supply in
                  let scratch =
                    acquire_empty instruction.span ~protected:inputs
                      ~excluded:[]
                  in
                  load_owner registers.(scratch) input;
                  emit (Encoder.Test registers.(scratch));
                  emit_branch Not_equal owned;
                  emit (Encoder.Store_context (40, registers.(source)));
                  emit (Encoder.Store_context_imm (32, site));
                  emit_branch Unconditional complete;
                  mark owned;
                  emit (Encoder.Store_context_imm (40, 0));
                  emit (Encoder.Store_context_imm (32, -site));
                  mark complete;
                  owners.(scratch) <- None);
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
              emit
                (Encoder.Store_context_imm (32, -Option.get instruction.site));
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
                  fold_switch_runs
                    (fun () target count last ->
                      if last then
                        emit_branch Unconditional (target_label target)
                      else if count = 1 then (
                        emit (Encoder.Test adjusted_register);
                        emit_branch Equal (target_label target);
                        emit (Encoder.Dec adjusted_register))
                      else
                        (* Earlier runs have been subtracted from the unsigned
                           in-range index. Compare against this run's length,
                           then advance only when its branch is not taken. *)
                        let length = Int64.of_int count in
                        emit (Encoder.Mov_imm64 (range_register, length));
                        emit (Encoder.Cmp (adjusted_register, range_register));
                        emit_branch Below (target_label target);
                        emit
                          (Encoder.Binary
                             (Encoder.Sub, adjusted_register, range_register)))
                    () entries;
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
      (* Ownership homes are private snapshots of reached producers. They never
         alias source-visible cells and survive nested calls and register spills. *)
      let publish_owner result load =
        let scratch =
          acquire_empty instruction.span ~protected:[] ~excluded:[]
        in
        load registers.(scratch);
        emit
          (Encoder.Store_frame
             ( encoder_frame_slot instruction.span
                 (Option.get result.code_owner_offset),
               registers.(scratch) ));
        owners.(scratch) <- None
      in
      (match instruction.operation with
      | Load_function_address (result, index) ->
          publish_owner result (fun target ->
              emit (Encoder.Mov_imm64 (target, function_owner_word index)))
      | Load_function_slot (_, slot, result) ->
          publish_owner result (fun target ->
              emit
                (Encoder.Load_arena
                   ( target,
                     encoder_arena_slot instruction.span
                       (Global_storage.function_slot_owner_offset slot) )))
      | Load_undefined_function_address result ->
          publish_owner result (fun target ->
              emit
                (Encoder.Mov_imm64
                   ( target,
                     Int64.of_int
                       (Global_storage.undefined_code_owner_id
                          (Option.get undefined_code_owner)) )))
      | Load_code_frame (_, offset, result) ->
          publish_owner result (fun target ->
              emit
                (Encoder.Load_frame
                   (target, encoder_frame_slot instruction.span offset)))
      | Store_code_frame (_, offset, input, result) ->
          publish_owner result (fun target ->
              load_owner target input;
              emit
                (Encoder.Store_frame
                   (encoder_frame_slot instruction.span offset, target)))
      | Load_code_arena (_, offset, result) ->
          publish_owner result (fun target ->
              emit
                (Encoder.Load_arena
                   (target, encoder_arena_slot instruction.span offset)))
      | Store_code_arena (_, offset, input, result) ->
          publish_owner result (fun target ->
              load_owner target input;
              emit
                (Encoder.Store_arena
                   (encoder_arena_slot instruction.span offset, target)))
      | Apply_word_view (input, result)
        when Option.is_some result.code_owner_offset ->
          publish_owner result (fun target -> load_owner target input)
      | _ -> ());
      Option.iter
        (fun (value, stage, owner_stage, reference_stage) ->
          if Option.is_none owner_stage then require_numeric_owner value;
          let inputs, _ = ensure_inputs instruction.span [ value ] in
          let source = List.hd inputs in
          (match reference_stage with
          | None ->
              emit
                (Encoder.Store_stack
                   (staged_stack_slot instruction.span stage, registers.(source)))
          | Some offset ->
              spill_all_registers instruction.span;
              copy_value_to instruction.span value rdx;
              emit
                (Encoder.Address_frame
                   (Encoder.R8, encoder_frame_slot instruction.span offset));
              List.iter
                (fun byte ->
                  emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, byte));
                  emit
                    (Encoder.Store_indirect_offset
                       (Encoder.R8, byte, Encoder.Rax)))
                [ 0; 8; 16; 24 ];
              emit
                (Encoder.Store_stack
                   (staged_stack_slot instruction.span stage, Encoder.R8)));
          Option.iter
            (fun stage ->
              let scratch =
                acquire_empty instruction.span ~protected:inputs ~excluded:[]
              in
              load_owner registers.(scratch) value;
              emit
                (Encoder.Store_stack
                   ( staged_stack_slot instruction.span stage,
                     registers.(scratch) ));
              owners.(scratch) <- None)
            owner_stage)
        instruction.push_stage;
      (* Publish each reached shared producer before the next branch. Its
         permanent home belongs to this activation and survives calls. *)
      Array.iteri
        (fun register owner ->
          match owner with
          | Some value when not (Value_set.mem value.value_id !published) -> (
              match shared_slot value with
              | Some (index, _) ->
                  emit
                    (Encoder.Store_stack
                       ( encoder_slot instruction.span index,
                         registers.(register) ));
                  published := Value_set.add value.value_id !published
              | None -> ())
          | _ -> ())
        owners;
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
  callback_call_site : bool;
  undefined_extern_site : bool;
  extern_signature_site : bool;
  code_comparison_site : bool;
  code_update_site : bool;
  code_word_escape_site : bool;
  no_value_capture_site : bool;
  callback_capture_site : bool;
  data_capture_site : bool;
  uninitialized_read_site : bool;
  index_scale_site : bool;
  index_addition_site : bool;
  address_bounds_site : bool;
  pointer_ordering_site : bool;
  pointer_difference_site : bool;
  output_site : bool;
  atomic_output_site : bool;
  stream_print_site : bool;
  stream_exe_site : bool;
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
  private_function_count : int;
  entry_stack_bytes : int;
  global_bytes : int;
  literal_bytes : int;
  arena_metadata_bytes : int;
  global_image : string;
  task_zero_bytes : int option;
  task_snapshot : Global_storage.task_snapshot option;
  code_owner_bindings : (int * int * int * int * int) list;
  function_slot_bindings : (int * int) list;
  has_output : bool;
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

let source_layout blocks =
  let position = ref 0 in
  let positions = ref Instruction_map.empty in
  let rec next_blocks map = function
    | [] -> map
    | block :: remaining ->
        Sequence.instructions (Graph.instructions block)
        |> List.iter (fun instruction ->
            positions :=
              Instruction_map.add
                (Sequence.description instruction).instruction_id !position
                !positions;
            incr position);
        let next =
          match remaining with
          | [] -> None
          | next :: _ -> Some (Graph.block_id next)
        in
        next_blocks (Block_map.add (Graph.block_id block) next map) remaining
  in
  let next = next_blocks Block_map.empty blocks in
  (next, !positions, !position)

let source_order_blocks blocks prepared =
  let index =
    List.fold_left
      (fun map block -> Block_map.add block.program_block_id block map)
      Block_map.empty prepared
  in
  List.map (fun block -> Block_map.find (Graph.block_id block) index) blocks

let external_values graph =
  let owners =
    List.fold_left
      (fun map block ->
        Sequence.instructions (Graph.instructions block)
        |> List.fold_left
             (fun map instruction ->
               match (Sequence.description instruction).result with
               | None -> map
               | Some result ->
                   Value_map.add result.value_id (Graph.block_id block) map)
             map)
      Value_map.empty (Graph.blocks graph)
  in
  List.fold_left
    (fun set block ->
      Sequence.instructions (Graph.instructions block)
      |> List.fold_left
           (fun set instruction ->
             List.fold_left
               (fun set value ->
                 if
                   Sequence.Block_id.equal
                     (Value_map.find value owners)
                     (Graph.block_id block)
                 then set
                 else Value_set.add value set)
               set (Sequence.description instruction).operands)
           set)
    Value_set.empty (Graph.blocks graph)

let shared_runtime_values ~external_ids ~extra values =
  let shared =
    Value_set.fold
      (fun id map ->
        match Value_map.find_opt id values with
        | None -> map
        | Some value -> Value_map.add id value map)
      external_ids extra
  in
  Value_map.bindings shared
  |> List.map (fun (_, value) ->
      value.last_use <- Int.max_int;
      value)

let preflight_program graph =
  let blocks = Graph.blocks graph in
  let next_blocks, positions, instruction_count = source_layout blocks in
  let values = ref Value_map.empty in
  let instruction_ids = ref Instruction_set.empty in
  let global_position = ref 0 in
  let sites_rev = ref [] in
  let prepared_blocks_rev = ref [] in
  let rec visit_blocks = function
    | [] -> ()
    | block :: remaining ->
        let block_id = Graph.block_id block in
        let arithmetic_sites = ref [] in
        let prepared_rev = ref [] in
        let descriptions = Graph.instructions block |> Sequence.instructions in
        List.iteri
          (fun position instruction ->
            let description = Sequence.description instruction in
            global_position :=
              Instruction_map.find description.instruction_id positions;
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
                extern_signature_site = false;
                undefined_extern_site = false;
                callback_call_site = false;
                code_comparison_site = false;
                code_update_site = false;
                no_value_capture_site =
                  (match operation with
                  | Discard_void -> true
                  | Discard_value (input, _) ->
                      Option.is_some input.code_owner_offset
                  | _ -> false);
                code_word_escape_site = false;
                callback_capture_site = false;
                data_capture_site = false;
                uninitialized_read_site = false;
                index_scale_site = false;
                index_addition_site = false;
                address_bounds_site = false;
                pointer_ordering_site = false;
                pointer_difference_site = false;
                output_site = false;
                atomic_output_site = false;
                stream_print_site = false;
                stream_exe_site = false;
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
        let next = Block_map.find block_id next_blocks in
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
  visit_blocks (Graph.definition_order graph);
  let shared_values =
    shared_runtime_values ~external_ids:(external_values graph)
      ~extra:Value_map.empty !values
  in
  ( source_order_blocks blocks !prepared_blocks_rev,
    List.sort
      (fun left right -> Int.compare left.global_position right.global_position)
      !sites_rev,
    instruction_count,
    shared_values )

type callable_slot = {
  slot_type : Type.t;
  slot_word : word_type;
  slot_dimensions : int64 list;
  slot_element_count : int;
  slot_extent_bytes : int;
  access : frame_access;
  callback : Headers.function_pointer option;
  slot_owner_offset : int option;
  slot_reference_offset : int option;
  owned_targets : int list ref option;
}

type callable_return_kind =
  | Callable_word_return of scalar_value
  | Callable_void_return

type callable_function_source = {
  source_definition : Ir.Integer_interpreter.function_definition;
  source_runtime_calls : Runtime.t;
  source_globals : Ir.Integer_globals.t;
  source_functions : Ir.Integer_interpreter.function_definition list;
  source_historical : bool;
}

type callable_function_info = {
  definition : Ir.Integer_interpreter.function_definition;
  runtime_calls : Runtime.t;
  source_globals : Ir.Integer_globals.t;
  owner : program_owner;
  parameter_types : Type.t array;
  parameter_callbacks : Headers.function_pointer option array;
  variadic : (variadic_reference_origin * Type.t) option;
  return_kind : callable_return_kind;
  rbp_bytes : int;
  activation_bytes : int;
  frame_slots : callable_slot Int_map.t;
  init_flag_offsets : int list;
}

type indexed_object = {
  object_origin : reference_origin;
  object_type : Type.t;
  object_element_count : int;
  object_code :
    (Headers.function_pointer * reference_table * int list ref) option;
}

type indexed_root =
  | Indexed_object_root of indexed_object
  | Indexed_reference_root of reference_access

type index_offset_term = {
  index_pointer_type : Type.t;
  index_stride : int64;
  index_offset : value;
}

type indexed_address_term = {
  indexed_root : indexed_root;
  indexed_offset : value;
  indexed_pointer_type : Type.t;
  indexed_remaining_strides : int64 list;
}

type frame_term =
  | Frame_base of Type.t
  | Frame_offset of Type.t * int
  | Frame_address of callable_slot
  | Variadic_address of variadic_reference_origin * Type.t
  | Global_address of Global_storage.slot
  | Reference_address of reference_access * Type.t
  | Index_offset of index_offset_term
  | Indexed_address of indexed_address_term

type callable_call_phase =
  | Collecting
  | Needs_cleanup
  | Needs_saved_cleanup
  | Needs_end

type callable_target =
  | Source_function of int
  | Mismatched_extern of int
  | Undefined_extern of int
  | Put_chars_provider
  | Print_provider of Runtime.provider

type callable_call_scope = {
  call : Runtime.call option;
  callback_call : Runtime.callback_call option;
  captured_stage : int option;
  owned_targets : int list ref option;
  target : callable_target;
  return_kind : callable_return_kind;
  activation_bytes : int;
  stage_base : int;
  result_stage : int option;
  argument_stages : int array;
  argument_owner_stages : int option array;
  argument_types : Type.t array;
  argument_callbacks : Headers.function_pointer option array;
  fixed_count : int;
  variadic_count : int64 option;
  scratch_stage : int;
  pushed : bool array;
  mutable phase : callable_call_phase;
}

let scope_arguments scope =
  match (scope.call, scope.callback_call) with
  | Some call, None -> Runtime.arguments call
  | None, Some callback -> callback.callback_arguments
  | _ -> reject "HCBACK0003" "native call has no unique original receipt"

type callable_intrinsic_scope = {
  intrinsic : Runtime.intrinsic;
  intrinsic_stage : int;
  ordinary_depth : int;
  mutable intrinsic_executed : bool;
}

let call_argument_index ~fixed_count ~variadic_count = function
  | Runtime.Fixed index when index >= 0 && index < fixed_count -> Some index
  | Runtime.Variadic_count when Option.is_some variadic_count ->
      Some fixed_count
  | Runtime.Variadic index when index >= 0 -> (
      match variadic_count with
      | Some count when Int64.of_int index < count ->
          Some (fixed_count + 1 + index)
      | _ -> None)
  | Runtime.Fixed _ | Runtime.Variadic_count | Runtime.Variadic _ -> None

let callback_print_shape (callback : Runtime.callback_call) =
  let primitive type_ depth primitive =
    Type.pointer_depth type_ = depth
    &&
    match Type.base type_ with
    | Type.Primitive (_, actual) -> Sema.Primitive_type.equal actual primitive
    | _ -> false
  in
  Option.is_some callback.callback_variadic_count
  && (not callback.callback_callee_pop)
  && primitive callback.callback_return_type 0 Sema.Primitive_type.U0
  &&
  match callback.callback_fixed_types with
  | [ type_ ] -> primitive type_ 1 Sema.Primitive_type.U8
  | _ -> false

let callable_argument_types ?(allow_pointer_tail = false) description
    ~max_stack_bytes ~fixed ~variadic_count arguments =
  let fixed_count = Array.length fixed in
  let count =
    match variadic_count with
    | None -> fixed_count
    | Some count
      when count >= 0L
           && count <= Int64.of_int ((max_stack_bytes / 8) - fixed_count - 1) ->
        fixed_count + 1 + Int64.to_int count
    | Some _ ->
        reject ?span:description.Sequence.span "HCBACK0004"
          "native word-tail argument staging exceeds max_stack_bytes"
  in
  if List.length arguments <> count then
    malformed description
      "native call argument count disagrees with its receipt";
  let types = Array.of_list (List.map Runtime.argument_target_type arguments) in
  let seen = Array.make count false in
  List.iter
    (fun argument ->
      match
        call_argument_index ~fixed_count ~variadic_count
          (Runtime.argument_role argument)
      with
      | Some index when not seen.(index) ->
          seen.(index) <- true;
          let type_ = Runtime.argument_target_type argument in
          (if index < fixed_count then (
             if not (Type.equal type_ fixed.(index)) then
               malformed description
                 "native fixed argument type disagrees with its parameter")
           else if
             allow_pointer_tail && index > fixed_count
             && Type.pointer_depth type_ > 0
           then ignore (checked_reference description type_)
           else
             let scalar = checked_scalar ~allow_public:true description type_ in
             if
               index = fixed_count
               && (scalar.word_type <> I64 || scalar.byte_size <> 8)
             then
               malformed description
                 "native hidden count requires its original I64 type");
          types.(index) <- type_
      | _ ->
          malformed description
            "native call has duplicate or invalid argument roles")
    arguments;
  if not (Array.for_all Fun.id seen) then
    malformed description "native call is missing a physical argument slot";
  types

let callable_callback_arguments description (callback : Runtime.callback_call)
    argument_types =
  let parameters =
    callback.callback_pointer |> Headers.function_pointer_signature
    |> Headers.signature_parameters |> Array.of_list
  in
  if Array.length parameters <> List.length callback.callback_fixed_types then
    malformed description
      "native callback fixed parameters disagree with their original header";
  Array.mapi
    (fun index type_ ->
      let pointer =
        if index >= Array.length parameters then None
        else
          let parameter = parameters.(index) in
          if Headers.parameter_index parameter <> index then
            malformed description
              "native callback parameter positions are inconsistent";
          match Headers.parameter_declarator_kind parameter with
          | Headers.Object -> None
          | Headers.Function_pointer pointer ->
              let register_ok =
                match Headers.parameter_register_selection parameter with
                | Sema.Register_request.Unspecified
                | Sema.Register_request.Disabled -> true
                | Sema.Register_request.Allocatable
                | Sema.Register_request.Explicit _ -> false
              in
              if
                (not register_ok)
                || List.length
                     (Headers.function_pointer_indirection_origins pointer)
                   <> 1
              then
                unsupported description
                  "native callback parameters require an original one-level \
                   callback declarator without explicit register selection";
              (match Headers.function_pointer_storage_type pointer with
              | Ok storage_type when Type.equal storage_type type_ -> ()
              | _ ->
                  malformed description
                    "native callback argument storage differs from its \
                     original nested declarator");
              Some pointer
      in
      if Option.is_none pointer then
        if Type.pointer_depth type_ <> 0 then
          ignore (checked_reference description type_)
        else ignore (checked_scalar ~allow_public:true description type_);
      pointer)
    argument_types

let callable_callback_matches (callback : Runtime.callback_call) ~argument_types
    ~argument_callbacks ~fixed_count (callee : callable_function_info) =
  let body = callee.definition.body in
  let flags = Function.stored_flags body in
  let module F = Generated.Function_flags.Stored in
  let callee_pop =
    (F.is_set ~mask:flags F.Ret1 || F.is_set ~mask:flags F.Argument_pop)
    && not (F.is_set ~mask:flags F.No_argument_pop)
  in
  Type.equal (Function.return_type body) callback.callback_return_type
  && callee_pop = callback.callback_callee_pop
  && Option.is_some callee.variadic
     = Option.is_some callback.callback_variadic_count
  && Array.length callee.parameter_types = fixed_count
  && Array.for_all2 Type.equal callee.parameter_types
       (Array.sub argument_types 0 fixed_count)
  && Array.for_all2
       (fun actual expected -> Option.is_some actual = Option.is_some expected)
       callee.parameter_callbacks
       (Array.sub argument_callbacks 0 fixed_count)
  && (Option.is_none callee.variadic
     || Array.for_all
          (fun type_ -> Type.pointer_depth type_ = 0)
          (Array.sub argument_types (fixed_count + 1)
             (Array.length argument_types - fixed_count - 1)))

type prepared_callable_body = {
  callable_blocks : prepared_program_block list;
  callable_sites : program_site list;
  callable_ir_count : int;
  callable_block_count : int;
  callable_home_slots : int;
  callable_stage_slots : int;
  callable_reference_bytes : int;
  callable_shared_values : value list;
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

let source_slot_scalar ?span label type_ =
  if Type.pointer_depth type_ = 0 then source_scalar ?span label type_
  else if Type.pointer_depth type_ = 1 then
    match Type.dereference type_ with
    | Ok pointee ->
        ignore (source_scalar ?span label pointee);
        { word_type = U64; byte_size = 8 }
    | Error message -> reject ?span "HCBACK0002" message
  else
    reject ?span "HCBACK0002"
      "native slots require at most one scalar indirection"

let arena_access slot =
  let scalar = Global_storage.scalar slot in
  {
    arena_offset = Global_storage.data_offset slot;
    arena_bytes = Scalar.byte_size scalar;
    arena_word = (if Scalar.is_unsigned scalar then U64 else I64);
    arena_extent_bytes = Global_storage.extent_bytes slot;
    initialized_flag_offset = Global_storage.flag_offset slot;
  }

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

let prepare_callable_function ~allow_runtime_layout ~max_stack_bytes
    ~maximum_variadic_count (source : callable_function_source) =
  let definition = source.source_definition in
  let body = definition.body in
  let frame = definition.frame in
  let span = Function.span body in
  if not (Function.definition_matches_frame body frame) then
    reject ?span "HCBACK0003"
      "native source function body does not match its checked frame";
  if Option.is_none (Function.definition_declaration body) then
    reject ?span "HCBACK0003"
      "native source functions require their original checked definition owner";
  let variadic_bindings =
    Headers.function_variadic_bindings (Frame.function_header frame)
  in
  let allowed_flags =
    if Option.is_some variadic_bindings then
      Int64.logor Function.ordinary_calling_flag_mask
        (Sema.Function_flag.Stored.to_mask Variadic)
    else Function.ordinary_calling_flag_mask
  in
  if
    Int64.logand (Function.stored_flags body) (Int64.lognot allowed_flags) <> 0L
  then
    reject ?span "HCBACK0002"
      "native source functions require ordinary calling flags";
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
  let metadata_count = ref 0 in
  let init_flag_offsets_rev = ref [] in
  let reserve_metadata () =
    if !metadata_count >= (max_stack_bytes - local_frame_bytes) / 8 then
      reject ?span "HCBACK0004"
        "native frame ownership and initialization state exceed max_stack_bytes";
    incr metadata_count;
    -(local_frame_bytes + (8 * !metadata_count))
  in
  let parameters = Function.parameters body in
  List.iteri
    (fun index member ->
      if Function.member_position member <> index then
        reject
          ?span:(Function.member_span member)
          "HCBACK0003"
          "native source function parameter positions are inconsistent";
      let declared_type = Function.member_type member in
      let location =
        match Frame.find_location frame (Function.member_symbol member) with
        | Some location -> location
        | None ->
            reject
              ?span:(Function.member_span member)
              "HCBACK0003"
              "native source parameter has no checked frame location"
      in
      let callback = Frame.location_callback_pointer location in
      let type_ = Frame.location_storage_type location |> Result.get_ok in
      let scalar =
        source_slot_scalar ?span:(Function.member_span member) "parameter" type_
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
        || (Frame.location_declarator_shape location
           <>
           if Option.is_some callback then Frame.Function_pointer
           else Frame.Object)
        || Frame.location_value_shape location <> Frame.Scalar
        || Frame.location_dimensions location <> []
        || (not
              (Type.equal (Frame.location_checked_type location) declared_type))
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
          slot_dimensions = [];
          slot_element_count = 1;
          slot_extent_bytes = scalar.byte_size;
          callback;
          slot_owner_offset = Option.map (fun _ -> reserve_metadata ()) callback;
          slot_reference_offset =
            (if Option.is_none callback && Type.pointer_depth type_ > 0 then (
               for _ = 1 to 3 do
                 ignore (reserve_metadata ())
               done;
               Some (reserve_metadata ()))
             else None);
          owned_targets = Option.map (fun _ -> ref []) callback;
          access =
            {
              frame_offset = actual;
              frame_bytes = scalar.byte_size;
              frame_word = scalar.word_type;
              initialized_flag_offset = None;
            };
        })
    parameters;
  let synthetic_locations kind =
    Frame.function_locations frame
    |> List.filter (fun location -> Frame.location_kind location = kind)
  in
  let variadic =
    match
      ( variadic_bindings,
        synthetic_locations Frame.Variadic_argc,
        synthetic_locations Frame.Variadic_argv )
    with
    | None, [], [] -> None
    | Some bindings, [ argc ], [ argv ] ->
        let check location binding kind expected =
          let type_ = Frame.location_checked_type location in
          let scalar = source_slot_scalar ?span "variadic binding" type_ in
          let register_ok =
            match Frame.location_register_selection location with
            | Sema.Register_request.Unspecified | Sema.Register_request.Disabled
              -> true
            | _ -> false
          in
          let slot =
            match Frame.location_frame_slot location with
            | Some slot -> slot
            | None ->
                reject ?span "HCBACK0003"
                  "native variadic binding has no original frame slot"
          in
          if
            Frame.location_symbol location
            != Headers.synthetic_binding_symbol binding
            || (not (Type.equal type_ (Headers.synthetic_binding_type binding)))
            || Frame.location_kind location <> kind
            || (not register_ok) || scalar.word_type <> I64
            || scalar.byte_size <> 8
            || Frame.location_allocated_size location <> 8L
            || Frame.frame_slot_size slot <> 8L
            || Frame.frame_slot_displacement slot <> Int64.of_int expected
          then
            reject ?span "HCBACK0003"
              "native variadic bindings disagree with the original checked \
               frame";
          type_
        in
        let argc_offset = 16 + (8 * List.length parameters) in
        let argc_type =
          check argc
            (Headers.variadic_argc bindings)
            Frame.Variadic_argc argc_offset
        in
        let argv_type =
          check argv
            (Headers.variadic_argv bindings)
            Frame.Variadic_argv (argc_offset + 8)
        in
        if
          Frame.location_value_shape argc <> Frame.Scalar
          || Frame.location_value_shape argv <> Frame.Array
        then
          reject ?span "HCBACK0003"
            "native variadic count/vector shape is inconsistent";
        add_slot argc_offset 8
          {
            slot_type = argc_type;
            slot_word = I64;
            slot_dimensions = [];
            slot_element_count = 1;
            slot_extent_bytes = 8;
            callback = None;
            slot_owner_offset = None;
            slot_reference_offset = None;
            owned_targets = None;
            access =
              {
                frame_offset = argc_offset;
                frame_bytes = 8;
                frame_word = I64;
                initialized_flag_offset = None;
              };
          };
        Some
          ( {
              data_offset = argc_offset + 8;
              count_offset = reserve_metadata ();
              maximum_count = maximum_variadic_count;
            },
            argv_type )
    | _ ->
        reject ?span "HCBACK0003"
          "native variadic bindings lack their original count/vector pair"
  in
  let locals = Function.locals body in
  List.iter
    (fun member ->
      let location =
        match Frame.find_location frame (Function.member_symbol member) with
        | Some location -> location
        | None ->
            reject
              ?span:(Function.member_span member)
              "HCBACK0003"
              "native automatic local has no checked frame location"
      in
      let callback = Frame.location_callback_pointer location in
      let type_ =
        match callback with
        | None -> Function.member_type member
        | Some pointer ->
            Headers.function_pointer_storage_type pointer |> Result.get_ok
      in
      let scalar =
        source_slot_scalar
          ?span:(Function.member_span member)
          "automatic local" type_
      in
      let dimensions = Frame.location_dimensions location in
      let counts = List.map Frame.dimension_value dimensions in
      let elements, object_bytes =
        if counts = [] then (1, scalar.byte_size)
        else (
          if
            (Type.pointer_depth type_ <> 0 && Option.is_none callback)
            || (not (Frame.location_source_dimensions_checked location))
            || List.exists
                 (fun dimension ->
                   Frame.dimension_kind dimension <> Frame.Source_extent
                   || (not allow_runtime_layout)
                      && (Frame.dimension_runtime_dependencies dimension <> []
                         || Frame.dimension_offset_dependencies dimension <> []
                         ))
                 dimensions
          then
            reject ?span "HCBACK0002"
              "native automatic arrays require original admitted scalar \
               dimensions";
          let shape_type =
            if Option.is_some callback then
              Type.make_primitive ~form:Type.Public_spelling
                ~primitive:Primitive.I64 ~pointer_depth:0
              |> Result.get_ok
            else type_
          in
          match
            Ir.Integer_storage_shape.create ~type_:shape_type ~dimensions:counts
          with
          | Ok shape ->
              ( Ir.Integer_storage_shape.element_count shape,
                Ir.Integer_storage_shape.byte_size shape )
          | Error _ ->
              reject ?span "HCBACK0002"
                "native automatic array extent is invalid or overflows")
      in
      (* Charge before constructing the per-element initialization metadata. *)
      if
        object_bytes > local_frame_bytes
        || elements
           > ((max_stack_bytes - local_frame_bytes) / 8) - !metadata_count
      then
        reject ?span "HCBACK0004"
          "native frame and per-element initialization state exceed \
           max_stack_bytes";
      let alignment =
        if object_bytes >= 8 then 8
        else if object_bytes >= 4 then 4
        else if object_bytes >= 2 then 2
        else 1
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
        || (Frame.location_declarator_shape location
           <>
           if Option.is_some callback then Frame.Function_pointer
           else Frame.Object)
        || (Frame.location_value_shape location
           <> if counts = [] then Frame.Scalar else Frame.Array)
        || (not
              (Type.equal
                 (Frame.location_checked_type location)
                 (if Option.is_some callback then Function.member_type member
                  else type_)))
        || Frame.location_element_size location <> Int64.of_int scalar.byte_size
        || Frame.location_allocated_size location <> Int64.of_int object_bytes
        || Frame.location_alignment location <> alignment
      then
        reject
          ?span:(Function.member_span member)
          "HCBACK0002"
          "native source locals require automatic scalar integer or array \
           stack storage";
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
        actual > -object_bytes
        || actual < -local_frame_bytes
        || actual mod alignment <> 0
        || Frame.frame_slot_size slot <> Int64.of_int object_bytes
      then
        reject
          ?span:(Function.member_span member)
          "HCBACK0003" "native automatic local has an invalid RBP displacement";
      let flag_offset = reserve_metadata () in
      init_flag_offsets_rev := flag_offset :: !init_flag_offsets_rev;
      for _ = 2 to elements do
        init_flag_offsets_rev := reserve_metadata () :: !init_flag_offsets_rev
      done;
      let slot_owner_offset =
        Option.map
          (fun _ ->
            let offset = reserve_metadata () in
            for _ = 2 to elements do
              ignore (reserve_metadata ())
            done;
            offset)
          callback
      in
      add_slot actual object_bytes
        {
          slot_type = type_;
          slot_word = scalar.word_type;
          slot_dimensions = counts;
          slot_element_count = elements;
          slot_extent_bytes = object_bytes;
          callback;
          slot_owner_offset;
          slot_reference_offset =
            (if Option.is_none callback && Type.pointer_depth type_ > 0 then (
               for _ = 1 to 3 do
                 ignore (reserve_metadata ())
               done;
               Some (reserve_metadata ()))
             else None);
          owned_targets = Option.map (fun _ -> ref []) callback;
          access =
            {
              frame_offset = actual;
              frame_bytes = scalar.byte_size;
              frame_word = scalar.word_type;
              initialized_flag_offset = Some flag_offset;
            };
        })
    locals;
  let rbp_bytes = local_frame_bytes + (8 * !metadata_count) in
  if rbp_bytes > max_stack_bytes then
    reject ?span "HCBACK0004"
      (Printf.sprintf
         "native source function frame plus initialization state requires %d \
          bytes, exceeding max_stack_bytes (%d)"
         rbp_bytes max_stack_bytes);
  let function_id = Function.function_id body |> Function.Function_id.to_int in
  {
    definition;
    runtime_calls = source.source_runtime_calls;
    source_globals = source.source_globals;
    owner =
      Function_owner
        { function_id; function_name = Symbol.name (Function.symbol body) };
    parameter_types =
      Array.of_list
        (List.map
           (fun member ->
             Frame.find_location frame (Function.member_symbol member)
             |> Option.get |> Frame.location_storage_type |> Result.get_ok)
           parameters);
    parameter_callbacks =
      Array.of_list
        (List.map
           (fun member ->
             Frame.find_location frame (Function.member_symbol member)
             |> Option.get |> Frame.location_callback_pointer)
           parameters);
    variadic;
    return_kind;
    rbp_bytes;
    activation_bytes =
      (local_frame_bytes
      + (8 * List.length parameters)
      + if Option.is_some variadic then 8 else 0);
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

let retained_link_matches_definition link
    (definition : Ir.Integer_interpreter.function_definition) =
  let metadata = Ir.Retained_function.metadata link in
  let declaration = Sema.Outer_environment.function_declaration metadata in
  Function.callable_symbol definition.body == Ir.Retained_function.symbol link
  && Function.definition_matches_frame definition.body definition.frame
  &&
  match Function.definition_declaration definition.body with
  | Some candidate -> candidate == declaration
  | None -> false

let callable_self_call_matches_definition ~runtime_owner call
    (definition : Ir.Integer_interpreter.function_definition) =
  match runtime_owner with
  | Runtime.Function owner_body when owner_body == definition.body -> (
      Runtime.call_opcode call = Opcode.Ic_call_indirect2
      && Function.definition_matches_frame definition.body definition.frame
      && Function.callable_symbol definition.body == Runtime.symbol call
      &&
      match Function.definition_declaration definition.body with
      | Some candidate ->
          let selected = Runtime.declaration call in
          candidate == selected
          || Sema.Function_resolution.is_joined_successor ~earlier:selected
               ~later:candidate
      | None -> false)
  | Runtime.Entry | Runtime.Function _ -> false

let callable_callee_index ~allow_task_self_call ~runtime_owner functions call =
  let symbol = Runtime.symbol call in
  let declaration = Runtime.declaration call in
  let rec find index =
    if index = Array.length functions then None
    else
      let definition = functions.(index).definition in
      let body = definition.body in
      if
        allow_task_self_call
        && callable_self_call_matches_definition ~runtime_owner call definition
        ||
        match Runtime.retained_function call with
        | Some link -> retained_link_matches_definition link definition
        | None -> (
            Function.callable_symbol body == symbol
            &&
            match Function.definition_declaration body with
            | Some candidate -> candidate == declaration
            | None -> false)
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

let preflight_callable_graph ~runtime_calls ~source_globals
    ~allow_retained_functions ~task_dynamic_code_words ~task_owned_targets
    ~capture_callback_default ~capture_data_default ~slot_root_runtime_calls
    ~slot_bindings ~task_snapshot ~parameter_defaults ~functions
    ~provider_entries ~code_edges ~indirect_code_edges ~arena_code_cells
    ~global_storage ~literal_storage ~runtime_owner ~owner
    ~(frame_slots : callable_slot Int_map.t) ~variadic ~expected_return
    ~is_entry ~rbp_bytes ~max_stack_bytes ~next_site graph =
  let function_addresses =
    match
      Runtime.original_function_addresses runtime_calls ~owner:runtime_owner
    with
    | Some addresses -> addresses
    | None ->
        reject "HCBACK0003"
          "native function addresses require the original sealed graph"
  in
  let function_slot_addresses =
    match
      Runtime.original_function_slot_addresses runtime_calls
        ~owner:runtime_owner
    with
    | Some addresses -> addresses
    | None ->
        reject "HCBACK0003"
          "native function slots require the original sealed graph"
  in
  let reference_bytes = ref 0 in
  let blocks = Graph.blocks graph in
  let next_blocks, positions, instruction_count = source_layout blocks in
  let first_site = !next_site in
  next_site := first_site + instruction_count;
  let values = ref Value_map.empty in
  let frame_values = ref Value_map.empty in
  let void_values = ref Value_set.empty in
  (* Static target sets bound dispatch; private dynamic owner words distinguish
     original code from numeric bits through callback cells and parameters. *)
  let code_values = ref Value_map.empty in
  let zero_values = ref Value_set.empty in
  let word_code_values = ref Value_set.empty in
  let code_cells =
    Int_map.filter_map
      (fun _ (slot : callable_slot) -> slot.owned_targets)
      frame_slots
  in
  Int_map.iter
    (fun _ targets ->
      targets := List.sort_uniq Int.compare (!targets @ task_owned_targets))
    code_cells;
  let code_source value = Value_map.find_opt value.value_id !code_values in
  let arena_targets slot =
    let offset = Global_storage.data_offset slot in
    match Int_map.find_opt offset !arena_code_cells with
    | Some targets -> targets
    | None ->
        let targets = ref task_owned_targets in
        arena_code_cells := Int_map.add offset targets !arena_code_cells;
        targets
  in
  let mark_code value targets =
    if Option.is_none value.code_owner_offset then (
      if max_stack_bytes - rbp_bytes - !reference_bytes < 8 then
        reject "HCBACK0004"
          "native code ownership snapshots exceed max_stack_bytes";
      reference_bytes := !reference_bytes + 8;
      value.code_owner_offset <- Some (-(rbp_bytes + !reference_bytes)));
    code_values := Value_map.add value.value_id targets !code_values
  in
  let is_zero value = Value_set.mem value.value_id !zero_values in
  let has_code_word_view value =
    Value_set.mem value.value_id !word_code_values
  in
  let check_update_operand description input =
    match code_source input with
    | Some _ when has_code_word_view input -> ()
    | Some _ ->
        unsupported description
          "numeric updates require an original callback word view"
    | None ->
        ignore
          (checked_scalar ~allow_public:true description input.declared_type)
  in
  let callbacks =
    match
      Runtime.original_callback_calls runtime_calls ~owner:runtime_owner
    with
    | Some callbacks -> callbacks
    | None -> reject "HCBACK0003" "native callbacks require the original graph"
  in
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
  let internal_frame_value frame_values values void_values description position
      result target_type make_term =
    let value =
      {
        value_id = result.Sequence.value_id;
        declared_type = target_type;
        computation_type = target_type;
        last_use = position;
        code_owner_offset = None;
        reference_descriptor_offset = None;
      }
    in
    define_frame frame_values values void_values description result
      (make_term value);
    value
  in
  let reserve_reference_table (description : Sequence.description) =
    let descriptor_bytes = 32 in
    let available = max_stack_bytes - rbp_bytes - !reference_bytes in
    if available < descriptor_bytes then
      reject ?span:description.span "HCBACK0004"
        (Printf.sprintf "native reference snapshot exceeds max_stack_bytes (%d)"
           max_stack_bytes);
    let bytes = descriptor_bytes in
    let table_offset = -(rbp_bytes + !reference_bytes + bytes) in
    reference_bytes := !reference_bytes + bytes;
    table_offset
  in
  let mark_reference description value =
    if Option.is_none value.reference_descriptor_offset then
      value.reference_descriptor_offset <-
        Some (reserve_reference_table description)
  in
  let frame_reference_origin slot =
    { access = slot.access; extent_bytes = slot.slot_extent_bytes }
  in
  let slot_strides slot =
    let rec layout = function
      | [] -> (Int64.of_int slot.access.frame_bytes, [])
      | count :: rest ->
          let bytes, strides = layout rest in
          (Int64.mul count bytes, bytes :: strides)
    in
    let bytes, strides = layout slot.slot_dimensions in
    if bytes <> Int64.of_int slot.slot_extent_bytes then
      reject "HCBACK0003"
        "native array strides disagree with checked frame extent";
    strides
  in
  let touch position value = value.last_use <- max value.last_use position in
  let touch_reference position access = touch position access.reference in
  let touch_indexed position indexed =
    touch position indexed.indexed_offset;
    match indexed.indexed_root with
    | Indexed_object_root _ -> ()
    | Indexed_reference_root access -> touch_reference position access
  in
  let frame_operand frame_values description id =
    match Value_map.find_opt id !frame_values with
    | Some value -> value
    | None ->
        malformed description
          (Printf.sprintf "frame value %%%d has no earlier definition"
             (Sequence.Value_id.to_int id))
  in
  let address_operand frame_values values description position id =
    match Value_map.find_opt id !frame_values with
    | Some (Index_offset _) ->
        malformed description
          "scaled index offset cannot be used as a canonical address"
    | Some (Indexed_address indexed as value) ->
        touch_indexed position indexed;
        value
    | Some (Reference_address (access, _) as value) ->
        touch_reference position access;
        value
    | Some value -> value
    | None ->
        let reference = operand values description position id in
        let pointee, scalar =
          checked_reference description reference.declared_type
        in
        Reference_address ({ reference; scalar; offset = None }, pointee)
  in
  let load_code description raw position result target_type pointer storage_type
      targets operation =
    let owns_load =
      match
        Runtime.find_callback_load runtime_calls ~owner:runtime_owner raw
      with
      | Some callback -> pointer == callback.callback_pointer
      | None -> Type.equal target_type storage_type
    in
    if not owns_load then
      malformed description
        "callback load differs from its original storage header";
    let value =
      define values description position result target_type
        (Computation.forward target_type)
    in
    mark_code value targets;
    (* Task cells may hold numeric words or executable owners. Admission of a
       word-shaped consumer does not erase the dynamic owner; native return,
       discard and ordinary argument paths inspect it at the reached site. *)
    if task_dynamic_code_words then
      word_code_values := Value_set.add value.value_id !word_code_values;
    (operation value, None)
  in
  let store_code description position result target_type input_id targets
      operation =
    let input = operand values description position input_id in
    let source =
      match code_source input with
      | Some targets -> targets
      | None ->
          ignore
            (checked_scalar ~allow_public:true description input.declared_type);
          ref []
    in
    code_edges := (targets, source) :: !code_edges;
    let value =
      define values description position result target_type
        (Computation.forward target_type)
    in
    mark_code value source;
    if Option.is_none (code_source input) || has_code_word_view input then
      word_code_values := Value_set.add value.value_id !word_code_values;
    if is_zero input then
      zero_values := Value_set.add value.value_id !zero_values;
    (operation input value, None)
  in
  let update_callback description position result target_type storage_type
      access operands site arithmetic_sites =
    if not (Type.equal target_type storage_type) then
      malformed description "callback update changes its original storage type";
    let update, old_result, expects_operand =
      Option.get (callable_frame_update description.opcode I64)
    in
    let input =
      match (expects_operand, operands) with
      | true, [ input_id ] ->
          let input = operand values description position input_id in
          check_update_operand description input;
          Some input
      | false, [] -> None
      | _ -> malformed description "invalid callback update operands"
    in
    (* The reached operation requires an unowned numeric cell. Its result is
       therefore an ordinary word; the original storage type remains on the
       checked instruction and never authorizes an object reference. *)
    let numeric_type =
      Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive.I64
        ~pointer_depth:0
      |> Result.get_ok
    in
    let value =
      define values description position result numeric_type numeric_type
    in
    word_code_values := Value_set.add value.value_id !word_code_values;
    let arithmetic_site =
      match update with
      | Update_division operation ->
          let fault_site =
            {
              site;
              operation;
              instruction_id =
                Sequence.Instruction_id.to_int description.instruction_id;
              position;
              span = description.span;
              signed = true;
            }
          in
          arithmetic_sites := fault_site :: !arithmetic_sites;
          Some fault_site
      | Update_binary _ | Update_shift _ -> None
    in
    ( Update_callback_value
        (access, update, input, old_result, value, arithmetic_site),
      None )
  in
  let visit_block block next =
    let block_id = Graph.block_id block in
    let arithmetic_sites = ref [] in
    let prepared_rev = ref [] in
    let calls = ref [] in
    let intrinsics = ref [] in
    let stage_cursor = ref 0 in
    let descriptions = Graph.instructions block |> Sequence.instructions in
    let terminal_position = List.length descriptions - 1 in
    List.iteri
      (fun position instruction ->
        let raw = Sequence.description instruction in
        if
          List.exists
            (fun scope ->
              not (Runtime.intrinsic_producer_matches scope.intrinsic raw))
            !intrinsics
        then
          malformed raw
            "internal argument producer differs from its sealed source record";
        validate_identity instruction_ids raw;
        let site =
          first_site + Instruction_map.find raw.instruction_id positions + 1
        in
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
        if
          List.exists
            (fun id -> Value_map.mem id !code_values)
            description.operands
          && (not
                (List.mem description.opcode
                   [
                     Opcode.Ic_assign;
                     Ic_equ_equ;
                     Ic_not_equ;
                     Ic_holyc_typecast;
                     Ic_end_exp;
                     Ic_set_rax;
                   ]))
          && (not
                (Option.fold ~none:false
                   ~some:(fun (_, _, expects_operand) -> expects_operand)
                   (callable_frame_update description.opcode I64)
                && List.for_all
                     (fun id ->
                       (not (Value_map.mem id !code_values))
                       || Option.fold ~none:false ~some:has_code_word_view
                            (Value_map.find_opt id !values))
                     description.operands))
          && (not
                ((match opcode_kind description.opcode with
                   | Some
                       ( Unary_kind _
                       | Logical_not_kind
                       | Logical_kind _
                       | Binary_kind _
                       | Constant_shift_kind _
                       | Shift_kind _
                       | Division_kind _
                       | Comparison_kind _ ) -> true
                   | _ -> false)
                && List.for_all
                     (fun id ->
                       (not (Value_map.mem id !code_values))
                       || Option.fold ~none:false ~some:has_code_word_view
                            (Value_map.find_opt id !values))
                     description.operands))
          && not
               (task_dynamic_code_words
               && description.opcode = Opcode.Ic_return_val
               && List.for_all
                    (fun id ->
                      match Value_map.find_opt id !values with
                      | Some value -> has_code_word_view value
                      | None -> false)
                    description.operands)
        then
          unsupported description
            "native owned code values require callback storage, equality or a \
             full-word view";
        let operation, value_type =
          match description.opcode with
          | Opcode.Ic_mul
            when List.exists
                   (fun id -> Value_map.mem id !code_values)
                   description.operands -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ left_id; right_id ], Some result, Some target_type, None ->
                  let left = operand values description position left_id
                  and right = operand values description position right_id in
                  List.iter (check_update_operand description) [ left; right ];
                  ignore
                    (checked_word ~allow_public:true description target_type);
                  let numeric_input input =
                    if Type.pointer_depth input.computation_type = 0 then input
                    else
                      {
                        input with
                        computation_type =
                          Type.make_primitive ~form:Type.Internal_storage
                            ~primitive:Primitive.I64 ~pointer_depth:0
                          |> Result.get_ok;
                      }
                  in
                  require_type ~allow_public:true description
                    (promoted_type description (numeric_input left)
                       (numeric_input right))
                    target_type;
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  (Apply_binary (Encoder.Imul, left, right, value), None)
              | _ ->
                  malformed description "invalid callback word multiplication")
          | (Opcode.Ic_equ_equ | Opcode.Ic_not_equ)
            when List.exists
                   (fun id -> Value_map.mem id !code_values)
                   description.operands -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ left_id; right_id ], Some result, Some target_type ->
                  let left = operand values description position left_id
                  and right = operand values description position right_id in
                  List.iter
                    (fun input ->
                      if
                        Option.is_none (code_source input)
                        && not (has_code_word_view input)
                      then
                        ignore
                          (checked_scalar ~allow_public:true description
                             input.declared_type))
                    [ left; right ];
                  ignore (checked_word description target_type);
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  ( Apply_code_comparison
                      ( (if description.opcode = Opcode.Ic_equ_equ then Encoder.E
                         else Encoder.NE),
                        left,
                        right,
                        value ),
                    None )
              | _ -> malformed description "invalid owned code comparison")
          | Opcode.Ic_holyc_typecast
            when List.exists
                   (fun id -> Value_map.mem id !code_values)
                   description.operands -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | ( [ input_id ],
                  Some result,
                  Some target_type,
                  Some (Sequence.Integer (0L | 1L)) ) ->
                  ignore
                    (checked_word ~allow_public:true description target_type);
                  let input = operand values description position input_id in
                  let value =
                    define values description position result target_type
                      (Computation.declared target_type)
                  in
                  mark_code value (Option.get (code_source input));
                  word_code_values :=
                    Value_set.add value.value_id !word_code_values;
                  (Apply_word_view (input, value), None)
              | _ ->
                  unsupported description
                    "native owned code values require a full-word integer view")
          | Opcode.Ic_set_rax
            when Option.is_some
                   (Runtime.find_callback_capture runtime_calls
                      ~owner:runtime_owner description.instruction_id) ->
              (Frame_tick, None)
          | Opcode.Ic_holyc_typecast
            when Option.fold ~none:false
                   ~some:(fun type_ -> Type.pointer_depth type_ > 0)
                   description.target_type -> (
              if description.flags <> 0L then
                malformed description "invalid primitive pointer cast flags";
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | ( [ id ],
                  Some result,
                  Some target_type,
                  Some (Sequence.Integer (0L | 1L)) ) ->
                  let input = operand values description position id in
                  if
                    Option.is_none input.reference_descriptor_offset
                    || Option.is_some input.code_owner_offset
                  then
                    unsupported description
                      "native primitive pointer casts require an owned data \
                       reference";
                  ignore (checked_reference description input.declared_type);
                  let _, scalar = checked_reference description target_type in
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  mark_reference description value;
                  ( Materialize_existing_reference
                      ({ reference = input; scalar; offset = None }, value),
                    None )
              | _ ->
                  malformed description "invalid primitive pointer cast shape")
          | Opcode.Ic_nop2
            when List.exists
                   (fun callback ->
                     Sequence.Instruction_id.to_int
                       callback.Runtime.callback_first
                     = Sequence.Instruction_id.to_int description.instruction_id
                       + 1)
                   callbacks -> (Frame_tick, None)
          | Opcode.Ic_call_start
            when Option.is_some
                   (Runtime.find_callback_start runtime_calls
                      ~owner:runtime_owner description.instruction_id) ->
              let callback =
                Option.get
                  (Runtime.find_callback_start runtime_calls
                     ~owner:runtime_owner description.instruction_id)
              in
              if
                List.exists
                  (fun argument ->
                    Option.is_some (Runtime.argument_prepared_default argument))
                  callback.callback_arguments
              then
                unsupported description
                  "anonymous callback arguments cannot carry named default \
                   evidence";
              let captured =
                operand values description position
                  callback.callback_capture_value
              in
              let targets =
                match code_source captured with
                | Some targets -> targets
                | None ->
                    malformed description
                      "callback callee has no owned local cell"
              in
              let parameter_types =
                callable_argument_types description ~max_stack_bytes
                  ~allow_pointer_tail:true
                  ~fixed:(Array.of_list callback.callback_fixed_types)
                  ~variadic_count:callback.callback_variadic_count
                  callback.callback_arguments
              in
              let argument_callbacks =
                callable_callback_arguments description callback parameter_types
              in
              let count = Array.length parameter_types in
              let return_kind =
                source_return_kind ?span:description.span
                  callback.callback_return_type
              in
              let stage_base = !stage_cursor in
              let argument_end = ref (stage_base + count) in
              let argument_owner_stages =
                Array.map
                  (Option.map (fun _ ->
                       let stage = !argument_end in
                       incr argument_end;
                       stage))
                  argument_callbacks
              in
              let owner_count = !argument_end - stage_base - count in
              let captured_stage = !argument_end in
              let result_stage =
                match return_kind with
                | Callable_word_return _ -> Some (captured_stage + 2)
                | Callable_void_return -> None
              in
              let kind_stage =
                captured_stage + 2
                + Option.fold ~none:0 ~some:(fun _ -> 1) result_stage
              in
              let kind_count =
                if callback_print_shape callback then max 0 (count - 2) else 0
              in
              stage_cursor := kind_stage + kind_count;
              if !stage_cursor > max_stack_bytes / 8 then
                reject ?span:description.span "HCBACK0004"
                  "native callback staging exceeds the private frame limit";
              stage_high_water := max !stage_high_water !stage_cursor;
              home_slots := max !home_slots (count + owner_count);
              calls :=
                {
                  call = None;
                  callback_call = Some callback;
                  captured_stage = Some captured_stage;
                  owned_targets = Some targets;
                  target = Source_function (-1);
                  return_kind;
                  activation_bytes = 0;
                  stage_base;
                  result_stage;
                  argument_stages = Array.init count (fun i -> stage_base + i);
                  argument_owner_stages;
                  argument_types = parameter_types;
                  argument_callbacks;
                  fixed_count = List.length callback.callback_fixed_types;
                  variadic_count = callback.callback_variadic_count;
                  scratch_stage = kind_stage;
                  pushed = Array.make count false;
                  phase = Collecting;
                }
                :: !calls;
              (Call_capture (captured, captured_stage), None)
          | Opcode.Ic_push_regs -> (
              match !calls with
              | { callback_call = Some callback; phase = Collecting; _ } :: _
                when Sequence.Instruction_id.equal description.instruction_id
                       callback.callback_save -> (Frame_tick, None)
              | _ ->
                  malformed description
                    "native callback save has no original scope")
          | Opcode.Ic_call_indirect -> (
              match !calls with
              | ({ callback_call = Some callback; phase = Collecting; _ } as
                 scope)
                :: _
                when Sequence.Instruction_id.equal description.instruction_id
                       callback.callback_instruction
                     && Array.for_all Fun.id scope.pushed ->
                  let argument_stage_slots = Array.copy scope.argument_stages in
                  let target_call callee_index =
                    let matches, activation_bytes =
                      if callee_index < Array.length functions then
                        let callee = functions.(callee_index) in
                        ( callable_callback_matches callback
                            ~argument_types:scope.argument_types
                            ~argument_callbacks:scope.argument_callbacks
                            ~fixed_count:scope.fixed_count callee,
                          callee.activation_bytes
                          + 8
                            * Option.fold ~none:0 ~some:Int64.to_int
                                scope.variadic_count )
                      else
                        let receipt =
                          provider_entries.(callee_index
                                            - Array.length functions)
                        in
                        ( Runtime.function_slot_address_matches_callback receipt
                            callback,
                          Array.length scope.argument_types * 8 )
                    in
                    ( matches,
                      {
                        callee_index;
                        activation_bytes;
                        argument_stage_slots;
                        argument_owner_stages =
                          Array.copy scope.argument_owner_stages;
                        result_stage_slot = scope.result_stage;
                        named_slot_stage = None;
                        provider_arguments =
                          (if
                             matches
                             && callee_index >= Array.length functions
                             && Runtime.function_slot_address_provider
                                  provider_entries.(callee_index
                                                    - Array.length functions)
                                |> function
                                | Some
                                    ( Runtime.Print
                                    | Runtime.Stream_print
                                    | Runtime.Stream_exe_print ) -> true
                                | _ -> false
                           then
                             Some
                               ( scope.scratch_stage,
                                 Array.sub scope.argument_types 2
                                   (Array.length scope.argument_types - 2)
                                 |> Array.map print_argument_kind )
                           else None);
                      } )
                  in
                  scope.phase <- Needs_cleanup;
                  ( Indirect_call
                      {
                        captured_stage = Option.get scope.captured_stage;
                        owned_targets = Option.get scope.owned_targets;
                        target_call;
                      },
                    None )
              | _ ->
                  malformed description
                    "native indirect call has no complete original scope")
          | (Opcode.Ic_add_rsp | Opcode.Ic_add_rsp1)
            when match !calls with
                 | { callback_call = Some _; _ } :: _ -> true
                 | _ -> false -> (
              match !calls with
              | ({ callback_call = Some callback; phase = Needs_cleanup; _ } as
                 scope)
                :: _
                when Sequence.Instruction_id.equal description.instruction_id
                       callback.callback_cleanup ->
                  scope.phase <-
                    (if Option.is_some callback.callback_saved_cleanup then
                       Needs_saved_cleanup
                     else Needs_end);
                  (Call_cleanup, None)
              | ({
                   callback_call = Some callback;
                   phase = Needs_saved_cleanup;
                   _;
                 } as scope)
                :: _
                when Option.fold ~none:false
                       ~some:
                         (Sequence.Instruction_id.equal
                            description.instruction_id)
                       callback.callback_saved_cleanup ->
                  scope.phase <- Needs_end;
                  (Call_cleanup, None)
              | _ ->
                  malformed description
                    "native callback cleanup has no original scope")
          | Opcode.Ic_call_end
            when match !calls with
                 | { callback_call = Some callback; _ } :: _ ->
                     Sequence.Instruction_id.equal description.instruction_id
                       callback.callback_last
                 | _ -> false -> (
              match !calls with
              | {
                  callback_call = Some callback;
                  phase = Needs_end;
                  result_stage;
                  stage_base;
                  _;
                }
                :: rest
                when Sequence.Instruction_id.equal description.instruction_id
                       callback.callback_last -> (
                  calls := rest;
                  stage_cursor := stage_base;
                  let result = Option.get description.result in
                  let target_type = callback.callback_return_type in
                  match result_stage with
                  | Some stage ->
                      let value =
                        define values description position result target_type
                          (Computation.declared target_type)
                      in
                      (Call_end (stage, value), None)
                  | None ->
                      void_values := Value_set.add result.value_id !void_values;
                      (Call_end_void, None))
              | _ ->
                  malformed description
                    "native callback end has no original scope")
          | Opcode.Ic_call_start
            when Option.is_some
                   (Runtime.find_intrinsic_start runtime_calls
                      ~owner:runtime_owner description.instruction_id) ->
              let intrinsic =
                Option.get
                  (Runtime.find_intrinsic_start runtime_calls
                     ~owner:runtime_owner description.instruction_id)
              in
              if
                pushes || description.flags <> 0L || description.operands <> []
                || Option.is_some description.result
                || Option.is_some description.target_type
              then malformed description "invalid internal call-start shape";
              (match description.payload with
              | Some (Sequence.Symbol symbol)
                when symbol == Runtime.intrinsic_symbol intrinsic -> ()
              | _ ->
                  malformed description
                    "internal call start changed its selected symbol");
              if not (Intrinsic.supports (Runtime.intrinsic_opcode intrinsic))
              then
                unsupported description
                  "native internal operation is outside the checked subset";
              (match
                 source_return_kind ?span:description.span
                   (Runtime.intrinsic_return_type intrinsic)
               with
              | (Callable_word_return _ | Callable_void_return)
                when Intrinsic.result_matches
                       (Runtime.intrinsic_opcode intrinsic)
                       (Runtime.intrinsic_return_type intrinsic) -> ()
              | _ ->
                  malformed description
                    "internal operation must retain its declared return type");
              let stage_count =
                if
                  Option.is_some
                    (Intrinsic.swap_size (Runtime.intrinsic_opcode intrinsic))
                then 2
                else 1
              in
              if stage_count > (max_stack_bytes / 8) - !stage_cursor then
                reject ?span:description.span "HCBACK0004"
                  (if stage_count = 2 then
                     "native swap staging exceeds the private frame limit"
                   else "native internal result exceeds the private frame limit");
              intrinsics :=
                {
                  intrinsic;
                  intrinsic_stage = !stage_cursor;
                  ordinary_depth = List.length !calls;
                  intrinsic_executed = false;
                }
                :: !intrinsics;
              stage_cursor := !stage_cursor + stage_count;
              stage_high_water := max !stage_high_water !stage_cursor;
              (Call_start, None)
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
              (match description.payload with
              | Some (Sequence.Symbol symbol) when symbol == Runtime.symbol call
                -> ()
              | _ ->
                  malformed description
                    "IC_CALL_START has another selected function symbol");
              if
                Option.is_some (Runtime.retained_function call)
                && not allow_retained_functions
              then
                unsupported description
                  "native calls do not admit retained source functions";
              let slot_binding =
                List.find_opt
                  (fun binding ->
                    Ir.Integer_interpreter.native_slot_binding_matches binding
                      ~root_runtime_calls:slot_root_runtime_calls ~runtime_calls
                      ~owner:runtime_owner ~globals:source_globals call)
                  slot_bindings
              in
              let slot_source =
                Option.bind slot_binding
                  Ir.Integer_interpreter.native_slot_binding_source
              in
              let slot_index =
                Option.map
                  (fun source ->
                    let index = ref None in
                    Array.iteri
                      (fun i info ->
                        if
                          info.definition.body
                          == source.Ir.Integer_interpreter.source_definition
                               .body
                          && info.definition.frame
                             == source.source_definition.frame
                          && info.runtime_calls == source.source_runtime_calls
                        then index := Some i)
                      functions;
                    match !index with
                    | Some i -> i
                    | None ->
                        malformed description
                          "native extern slot body is absent from its original \
                           source closure")
                  slot_source
              in
              let slot_fixed =
                Runtime.header call |> Headers.function_signature
                |> Headers.signature_parameters
                |> List.map (fun parameter ->
                    Headers.parameter_type_reference parameter
                    |> Sema.Type_reference.resolved_type)
                |> Array.of_list
              in
              let slot_matches =
                Option.fold ~none:true
                  ~some:(fun index ->
                    let callee = functions.(index) in
                    let module F = Generated.Function_flags.Stored in
                    let flags = Function.stored_flags callee.definition.body in
                    let callee_pop =
                      (F.is_set ~mask:flags F.Ret1
                      || F.is_set ~mask:flags F.Argument_pop)
                      && not (F.is_set ~mask:flags F.No_argument_pop)
                    in
                    Type.equal (Runtime.return_type call)
                      (Function.return_type callee.definition.body)
                    && (Runtime.cleanup_opcode call
                       =
                       if callee_pop then Opcode.Ic_add_rsp1
                       else Opcode.Ic_add_rsp)
                    && Option.is_some callee.variadic
                       = Option.is_some (Runtime.variadic_count call)
                    && Array.length slot_fixed
                       = Array.length callee.parameter_types
                    && Array.for_all2 Type.equal slot_fixed
                         callee.parameter_types)
                  slot_index
              in
              let provider =
                if Option.is_some slot_index then None
                else Runtime.provider call
              in
              let target, parameter_types, return_kind, activation_bytes =
                if
                  Option.is_some slot_binding
                  && (Option.is_none slot_index || not slot_matches)
                  && Option.is_none provider
                then (
                  let fixed = slot_fixed in
                  Array.iter
                    (fun type_ ->
                      if Type.pointer_depth type_ = 0 then
                        ignore
                          (checked_scalar ~allow_public:true description type_)
                      else ignore (checked_reference description type_))
                    fixed;
                  let arguments =
                    callable_argument_types description ~max_stack_bytes ~fixed
                      ~variadic_count:(Runtime.variadic_count call)
                      (Runtime.arguments call)
                  in
                  ( (if slot_matches then Undefined_extern (Array.length fixed)
                     else Mismatched_extern (Array.length fixed)),
                    arguments,
                    source_return_kind ?span:description.span
                      (Runtime.return_type call),
                    0 ))
                else
                  match provider with
                  | Some Runtime.Put_chars ->
                      if
                        Runtime.call_opcode call <> Opcode.Ic_call_indirect2
                        && Runtime.call_opcode call <> Opcode.Ic_call_extern
                        || Option.is_some (Runtime.variadic_count call)
                      then
                        malformed description
                          "native PutChars requires its original fixed extern \
                           call";
                      if
                        Option.is_none slot_binding
                        && Array.exists
                             (fun info ->
                               Symbol.name
                                 (Function.callable_symbol info.definition.body)
                               = Symbol.name (Runtime.symbol call))
                             functions
                      then
                        unsupported description
                          "native PutChars provider calls cannot coexist with \
                           a source body for that name; joined extern \
                           publication requires retained source execution";
                      let argument =
                        match Runtime.arguments call with
                        | [ argument ]
                          when Runtime.argument_role argument = Runtime.Fixed 0
                          -> argument
                        | _ ->
                            malformed description
                              "native PutChars requires its one original \
                               argument"
                      in
                      let parameter_type =
                        Runtime.argument_target_type argument
                      in
                      let scalar =
                        checked_scalar ~allow_public:true description
                          parameter_type
                      in
                      if scalar.byte_size <> 8 || scalar.word_type <> U64 then
                        malformed description
                          "native PutChars argument must retain its U64 slot";
                      let return_kind =
                        source_return_kind ?span:description.span
                          (Runtime.return_type call)
                      in
                      if return_kind <> Callable_void_return then
                        malformed description "native PutChars must complete U0";
                      (Put_chars_provider, [| parameter_type |], return_kind, 8)
                  | Some
                      (( Runtime.Print
                       | Runtime.Stream_print
                       | Runtime.Stream_exe_print ) as provider) ->
                      let count =
                        match Runtime.variadic_count call with
                        | Some count
                          when count >= 0L
                               && count
                                  <= Int64.of_int ((max_stack_bytes / 8) - 2) ->
                            Int64.to_int count
                        | Some _ ->
                            reject ?span:description.span "HCBACK0004"
                              "native Print argument staging exceeds the \
                               private frame limit"
                        | None ->
                            malformed description
                              "native Print requires its original variadic \
                               count"
                      in
                      if
                        Runtime.call_opcode call <> Opcode.Ic_call_indirect2
                        && Runtime.call_opcode call <> Opcode.Ic_call_extern
                      then
                        malformed description
                          "native Print requires its original extern call \
                           opcode";
                      if
                        Option.is_none slot_binding
                        && Array.exists
                             (fun info ->
                               Symbol.name
                                 (Function.callable_symbol info.definition.body)
                               = Symbol.name (Runtime.symbol call))
                             functions
                      then
                        unsupported description
                          "native Print provider calls cannot coexist with a \
                           source body for that name; joined extern \
                           publication requires retained source execution";
                      let arguments = Runtime.arguments call in
                      if List.length arguments <> count + 2 then
                        malformed description
                          "native Print argument count is inconsistent";
                      let parameter_types =
                        Array.make (count + 2) (Runtime.return_type call)
                      in
                      let present = Array.make (count + 2) false in
                      List.iter
                        (fun argument ->
                          match
                            call_argument_index ~fixed_count:1
                              ~variadic_count:(Runtime.variadic_count call)
                              (Runtime.argument_role argument)
                          with
                          | Some index
                            when index >= 0
                                 && index < count + 2
                                 && not present.(index) ->
                              let type_ =
                                Runtime.argument_target_type argument
                              in
                              present.(index) <- true;
                              parameter_types.(index) <- type_;
                              if index = 0 then
                                let pointee, _ =
                                  checked_reference description type_
                                in
                                match Type.base pointee with
                                | Type.Primitive (_, Primitive.U8) -> ()
                                | _ ->
                                    malformed description
                                      "native Print format must retain its U8 \
                                       pointer type"
                              else if index = 1 then (
                                let scalar = checked_scalar description type_ in
                                if
                                  scalar.byte_size <> 8
                                  || scalar.word_type <> I64
                                then
                                  malformed description
                                    "native Print count must retain internal \
                                     I64")
                              else if Type.pointer_depth type_ = 0 then
                                ignore
                                  (checked_scalar ~allow_public:true description
                                     type_)
                              else ignore (checked_reference description type_)
                          | _ ->
                              malformed description
                                "native Print argument role is duplicated or \
                                 outside its captured tail")
                        arguments;
                      if not (Array.for_all Fun.id present) then
                        malformed description
                          "native Print is missing an argument slot";
                      let return_kind =
                        source_return_kind ?span:description.span
                          (Runtime.return_type call)
                      in
                      let return_matches =
                        match (provider, return_kind) with
                        | Runtime.Stream_exe_print, Callable_word_return scalar
                          -> scalar.word_type = I64 && scalar.byte_size = 8
                        | ( (Runtime.Print | Runtime.Stream_print),
                            Callable_void_return ) -> true
                        | _ -> false
                      in
                      if not return_matches then
                        malformed description
                          "native formatter return disagrees with its original \
                           provider";
                      ( Print_provider provider,
                        parameter_types,
                        return_kind,
                        (count + 2) * 8 )
                  | None ->
                      let task_self_call =
                        allow_retained_functions
                        && Runtime.call_opcode call = Opcode.Ic_call_indirect2
                        && Array.exists
                             (fun info ->
                               callable_self_call_matches_definition
                                 ~runtime_owner call info.definition)
                             functions
                      in
                      if
                        Runtime.call_opcode call <> Opcode.Ic_call
                        && (not task_self_call) && Option.is_none slot_index
                      then
                        unsupported description
                          "native callable programs require fixed direct \
                           source calls or the checked PutChars provider";
                      let callee_index =
                        match
                          match slot_index with
                          | Some index -> Some index
                          | None ->
                              callable_callee_index
                                ~allow_task_self_call:allow_retained_functions
                                ~runtime_owner functions call
                        with
                        | Some index -> index
                        | None ->
                            malformed description
                              "direct call has no exact callable source \
                               definition"
                      in
                      let callee = functions.(callee_index) in
                      Option.iter
                        (fun link ->
                          if
                            not
                              (Option.is_some slot_index
                              || retained_link_matches_definition link
                                   callee.definition
                              || allow_retained_functions
                                 && callable_self_call_matches_definition
                                      ~runtime_owner call callee.definition)
                          then
                            malformed description
                              "retained direct call selected another source \
                               body, frame or declaration")
                        (Runtime.retained_function call);
                      if
                        not
                          (Type.equal (Runtime.return_type call)
                             (Function.return_type callee.definition.body))
                      then
                        malformed description
                          "direct call return type disagrees with its source \
                           definition";
                      if
                        Option.is_some callee.variadic
                        <> Option.is_some (Runtime.variadic_count call)
                      then
                        malformed description
                          "direct call variadic shape disagrees with its \
                           original body";
                      ( Source_function callee_index,
                        callable_argument_types description ~max_stack_bytes
                          ~fixed:callee.parameter_types
                          ~variadic_count:(Runtime.variadic_count call)
                          (Runtime.arguments call),
                        callee.return_kind,
                        callee.activation_bytes
                        + 8
                          * Option.fold ~none:0 ~some:Int64.to_int
                              (Runtime.variadic_count call) )
              in
              let parameter_count = Array.length parameter_types in
              let fixed_count =
                match target with
                | Source_function index ->
                    Array.length functions.(index).parameter_types
                | Undefined_extern fixed_count | Mismatched_extern fixed_count
                  -> fixed_count
                | Put_chars_provider | Print_provider _ -> 1
              in
              let variadic_count = Runtime.variadic_count call in
              let parameter_callbacks =
                match target with
                | Source_function index ->
                    Array.init parameter_count (fun position ->
                        if position < fixed_count then
                          functions.(index).parameter_callbacks.(position)
                        else None)
                | Undefined_extern _
                | Mismatched_extern _
                | Put_chars_provider
                | Print_provider _ -> Array.make parameter_count None
              in
              let arguments = Runtime.arguments call in
              if List.length arguments <> parameter_count then
                malformed description
                  "direct call fixed argument count disagrees with its source \
                   definition";
              let seen = Array.make parameter_count false in
              List.iter
                (fun argument ->
                  match
                    call_argument_index ~fixed_count ~variadic_count
                      (Runtime.argument_role argument)
                  with
                  | Some index
                    when index >= 0 && index < parameter_count
                         && not seen.(index) ->
                      seen.(index) <- true;
                      if
                        not
                          (Type.equal
                             (Runtime.argument_target_type argument)
                             parameter_types.(index))
                      then
                        malformed description
                          "direct call argument target type disagrees with its \
                           parameter"
                  | Some _ | None ->
                      unsupported description
                        "native call argument roles disagree with their \
                         original signature")
                arguments;
              if not (Array.for_all Fun.id seen) then
                malformed description
                  "direct call does not cover every physical argument slot";
              let stage_base = !stage_cursor in
              let argument_end = ref (stage_base + parameter_count) in
              let argument_owner_stages =
                Array.map
                  (Option.map (fun _ ->
                       let stage = !argument_end in
                       incr argument_end;
                       stage))
                  parameter_callbacks
              in
              let owner_count = !argument_end - stage_base - parameter_count in
              let result_stage =
                match return_kind with
                | Callable_word_return _ -> Some !argument_end
                | Callable_void_return -> None
              in
              let scratch_stage =
                !argument_end + if Option.is_some result_stage then 1 else 0
              in
              let scratch_count =
                if
                  match target with
                  | Print_provider _ -> true
                  | _ -> false
                then Print_codegen.scratch_slots (parameter_count - 2)
                else if Option.is_some slot_index && slot_matches then 1
                else 0
              in
              if scratch_count > (max_stack_bytes / 8) - scratch_stage then
                reject ?span:description.span "HCBACK0004"
                  "native call arguments and formatter scratch exceed the \
                   private frame limit";
              stage_cursor := scratch_stage + scratch_count;
              stage_high_water := max !stage_high_water !stage_cursor;
              if
                match target with
                | Print_provider _ -> false
                | _ -> true
              then home_slots := max !home_slots (parameter_count + owner_count);
              let scope =
                {
                  call = Some call;
                  callback_call = None;
                  captured_stage = None;
                  owned_targets = None;
                  target;
                  return_kind;
                  activation_bytes;
                  stage_base;
                  result_stage;
                  argument_stages =
                    Array.init parameter_count (fun index -> stage_base + index);
                  argument_owner_stages;
                  argument_types = parameter_types;
                  argument_callbacks = parameter_callbacks;
                  fixed_count;
                  variadic_count;
                  scratch_stage;
                  pushed = Array.make parameter_count false;
                  phase = Collecting;
                }
              in
              calls := scope :: !calls;
              (Call_start, None)
          | opcode when Intrinsic.supports opcode -> (
              match
                ( !intrinsics,
                  Runtime.find_intrinsic_instruction runtime_calls
                    ~owner:runtime_owner description.instruction_id )
              with
              | scope :: _, Some intrinsic
                when scope.intrinsic == intrinsic
                     && Runtime.intrinsic_opcode intrinsic = opcode
                     && (not scope.intrinsic_executed)
                     && scope.ordinary_depth = List.length !calls ->
                  let arguments = Runtime.intrinsic_arguments intrinsic in
                  if
                    pushes || description.flags <> 0L
                    || description.operands
                       <> List.map Runtime.argument_value arguments
                    || Option.is_some description.result
                    || Option.is_some description.payload
                    || not
                         (Option.fold ~none:false
                            ~some:
                              (Type.equal
                                 (Runtime.intrinsic_return_type intrinsic))
                            description.target_type)
                  then malformed description "invalid checked internal shape";
                  let inputs =
                    List.mapi
                      (fun index argument ->
                        let input =
                          operand values description position
                            (Runtime.argument_value argument)
                        in
                        if
                          not
                            (Type.equal input.declared_type
                               (Runtime.argument_source_type argument))
                        then
                          malformed description
                            "internal operation changed its checked argument \
                             producer type";
                        let target = Runtime.argument_target_type argument in
                        if Option.is_some (Intrinsic.swap_size opcode) then (
                          if
                            not
                              (Intrinsic.swap_pointer opcode input.declared_type)
                          then
                            malformed description
                              "swap requires original matching-width scalar \
                               pointers";
                          ignore
                            (checked_reference description input.declared_type))
                        else if
                          Option.is_some (Intrinsic.bit opcode) && index = 0
                        then (
                          if not (Intrinsic.bit_pointer input.declared_type)
                          then
                            malformed description
                              "pointed bit operation requires an original \
                               scalar object pointer";
                          ignore
                            (checked_reference description input.declared_type))
                        else if opcode = Opcode.Ic_mod_u64 && index = 0 then (
                          if not (Intrinsic.mod_u64_pointer input.declared_type)
                          then
                            malformed description
                              "IC_MOD_U64 requires an original I64/U64 object \
                               pointer";
                          ignore
                            (checked_reference description input.declared_type))
                        else checked_copy description target input.declared_type;
                        if not (Intrinsic.argument_matches opcode ~index target)
                        then
                          malformed description
                            "internal operation changed its declared parameter";
                        input)
                      arguments
                  in
                  let operation =
                    match
                      ( opcode,
                        Intrinsic.unary opcode,
                        Intrinsic.binary opcode,
                        inputs )
                    with
                    | opcode, _, _, [ left; right ]
                      when Option.is_some (Intrinsic.swap_size opcode) ->
                        let _, left_scalar =
                          checked_reference description left.declared_type
                        and _, right_scalar =
                          checked_reference description right.declared_type
                        in
                        Internal_swap
                          ( left,
                            left_scalar,
                            right,
                            right_scalar,
                            scope.intrinsic_stage )
                    | opcode, _, _, [ pointed; index ]
                      when Option.is_some (Intrinsic.bit opcode) ->
                        let _, scalar =
                          checked_reference description pointed.declared_type
                        in
                        Internal_bit
                          ( Option.get (Intrinsic.bit opcode),
                            pointed,
                            index,
                            scalar,
                            scope.intrinsic_stage )
                    | Opcode.Ic_mod_u64, _, _, [ pointed; divisor ] ->
                        Internal_mod_u64
                          (pointed, divisor, scope.intrinsic_stage)
                    | Opcode.Ic_strlen, _, _, [ input ] ->
                        Internal_strlen (input, scope.intrinsic_stage)
                    | _, Some operation, None, [ input ] ->
                        Internal_integer
                          (operation, input, scope.intrinsic_stage)
                    | _, None, Some operation, [ left; right ] ->
                        Internal_binary
                          (operation, left, right, scope.intrinsic_stage)
                    | _ ->
                        malformed description
                          "internal operation lost its sealed arguments"
                  in
                  scope.intrinsic_executed <- true;
                  (operation, None)
              | _ ->
                  malformed description
                    "internal operation has no exact collecting internal call \
                     scope")
          | Opcode.Ic_call | Opcode.Ic_call_indirect2 | Opcode.Ic_call_extern
            -> (
              match !calls with
              | scope :: _ when scope.phase = Collecting ->
                  if
                    description.flags <> 0L || description.operands <> []
                    || description.opcode
                       <> Runtime.call_opcode (Option.get scope.call)
                    || Option.is_some description.result
                    || (not
                          (Sequence.Instruction_id.equal
                             description.instruction_id
                             (Runtime.call_instruction (Option.get scope.call))))
                    || not (Array.for_all Fun.id scope.pushed)
                  then
                    malformed description "invalid fixed direct IC_CALL shape";
                  (match description.payload with
                  | Some (Sequence.Symbol symbol)
                    when symbol == Runtime.symbol (Option.get scope.call) -> ()
                  | _ ->
                      malformed description
                        "IC_CALL target symbol is inconsistent");
                  (match description.target_type with
                  | Some type_
                    when Type.equal type_
                           (Runtime.return_type (Option.get scope.call)) -> ()
                  | _ ->
                      malformed description
                        "IC_CALL target type is inconsistent");
                  scope.phase <- Needs_cleanup;
                  ( (match scope.target with
                    | Source_function callee_index ->
                        Direct_call
                          {
                            callee_index;
                            activation_bytes = scope.activation_bytes;
                            argument_stage_slots =
                              Array.copy scope.argument_stages;
                            argument_owner_stages =
                              Array.copy scope.argument_owner_stages;
                            result_stage_slot = scope.result_stage;
                            provider_arguments = None;
                            named_slot_stage =
                              (if
                                 List.exists
                                   (fun binding ->
                                     Ir.Integer_interpreter
                                     .native_slot_binding_matches binding
                                       ~root_runtime_calls:
                                         slot_root_runtime_calls ~runtime_calls
                                       ~owner:runtime_owner
                                       ~globals:source_globals
                                       (Option.get scope.call))
                                   slot_bindings
                               then Some scope.scratch_stage
                               else None);
                          }
                    | Undefined_extern _ -> Undefined_extern_call
                    | Mismatched_extern _ -> Extern_signature_fault
                    | Put_chars_provider -> Put_chars scope.argument_stages.(0)
                    | Print_provider provider ->
                        let tail_types =
                          Array.sub scope.argument_types 2
                            (Array.length scope.argument_types - 2)
                        in
                        let kinds =
                          Array.map
                            (fun type_ ->
                              if Type.pointer_depth type_ = 0 then
                                Print_codegen.Word
                              else
                                match Type.base type_ with
                                | Type.Primitive (_, Primitive.U8) ->
                                    Print_codegen.Unsigned_byte_pointer
                                | Type.Primitive (_, Primitive.I8) ->
                                    Print_codegen.Signed_byte_pointer
                                | _ -> Print_codegen.Other_pointer)
                            tail_types
                        in
                        Print_output
                          {
                            Print_codegen.target =
                              (match provider with
                              | Runtime.Print -> Print_codegen.Task_output
                              | Runtime.Stream_print -> Print_codegen.Generation
                              | Runtime.Stream_exe_print ->
                                  Print_codegen.Formatted_source
                              | Runtime.Put_chars -> assert false);
                            format_stage = scope.argument_stages.(0);
                            arguments_stage = scope.stage_base + 2;
                            argument_kinds = kinds;
                            scratch_stage = scope.scratch_stage;
                            activation_bytes = scope.activation_bytes;
                          }),
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
                    || description.opcode
                       <> Runtime.cleanup_opcode (Option.get scope.call)
                    || not
                         (Sequence.Instruction_id.equal
                            description.instruction_id
                            (Runtime.cleanup_instruction (Option.get scope.call)))
                  then malformed description "invalid direct-call cleanup shape";
                  (match description.payload with
                  | Some (Sequence.Integer bytes)
                    when bytes = Runtime.cleanup_bytes (Option.get scope.call)
                    -> ()
                  | _ ->
                      malformed description
                        "direct-call cleanup byte count is inconsistent");
                  (match description.target_type with
                  | Some type_
                    when Type.equal type_
                           (Runtime.return_type (Option.get scope.call)) -> ()
                  | _ ->
                      malformed description
                        "direct-call cleanup target type is inconsistent");
                  scope.phase <- Needs_end;
                  (Call_cleanup, None)
              | _ ->
                  malformed description
                    "call cleanup is outside a reached direct call")
          | Opcode.Ic_call_end
            when Option.is_some
                   (Runtime.find_intrinsic_end runtime_calls
                      ~owner:runtime_owner description.instruction_id) -> (
              let intrinsic =
                Option.get
                  (Runtime.find_intrinsic_end runtime_calls ~owner:runtime_owner
                     description.instruction_id)
              in
              match !intrinsics with
              | scope :: rest
                when scope.intrinsic == intrinsic
                     && scope.intrinsic_executed
                     && scope.ordinary_depth = List.length !calls -> (
                  if description.flags <> 0L || description.operands <> [] then
                    malformed description "invalid internal call-end shape";
                  (match description.payload with
                  | Some (Sequence.Symbol symbol)
                    when symbol == Runtime.intrinsic_symbol intrinsic -> ()
                  | _ ->
                      malformed description
                        "internal call end changed its selected symbol");
                  let result, target_type =
                    match (description.result, description.target_type) with
                    | Some result, Some target_type
                      when Sequence.Value_id.equal result.value_id
                             (Runtime.intrinsic_result_value intrinsic)
                           && Type.equal target_type
                                (Runtime.intrinsic_return_type intrinsic) ->
                        (result, target_type)
                    | _ ->
                        malformed description
                          "internal call end changed its declared result"
                  in
                  intrinsics := rest;
                  stage_cursor := scope.intrinsic_stage;
                  match
                    source_return_kind ?span:description.span target_type
                  with
                  | Callable_word_return _ ->
                      let value =
                        define values description position result target_type
                          (Computation.declared target_type)
                      in
                      (Call_end (scope.intrinsic_stage, value), None)
                  | Callable_void_return ->
                      void_values := Value_set.add result.value_id !void_values;
                      (Call_end_void, None))
              | _ ->
                  malformed description
                    "internal call end has no completed intrinsic scope")
          | Opcode.Ic_call_end -> (
              match !calls with
              | scope :: remaining when scope.phase = Needs_end -> (
                  if
                    description.flags <> 0L || description.operands <> []
                    || not
                         (Sequence.Instruction_id.equal
                            description.instruction_id
                            (Runtime.last (Option.get scope.call)))
                  then malformed description "invalid IC_CALL_END shape";
                  (match description.payload with
                  | Some (Sequence.Symbol symbol)
                    when symbol == Runtime.symbol (Option.get scope.call) -> ()
                  | _ ->
                      malformed description
                        "IC_CALL_END target symbol is inconsistent");
                  let result, target_type =
                    match (description.result, description.target_type) with
                    | Some result, Some target_type
                      when Sequence.Value_id.equal result.value_id
                             (Runtime.result_value (Option.get scope.call))
                           && Type.equal target_type
                                (Runtime.return_type (Option.get scope.call)) ->
                        (result, target_type)
                    | _ ->
                        malformed description
                          "IC_CALL_END result is inconsistent"
                  in
                  calls := remaining;
                  stage_cursor := scope.stage_base;
                  match scope.return_kind with
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
          | Opcode.Ic_str_const -> (
              match
                ( description.result,
                  description.target_type,
                  Literal_storage.find literal_storage ~owner:runtime_owner
                    ~graph description.instruction_id )
              with
              | Some result, Some target_type, Some region ->
                  ignore (checked_reference description target_type);
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  mark_reference description value;
                  ( Materialize_reference
                      ( Literal_reference region,
                        Arena_table (Literal_storage.table_offset region),
                        None,
                        value ),
                    None )
              | _ ->
                  malformed description
                    "literal producer has no exact owned byte region")
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
                when Type.pointer_depth target_type >= 1
                     && Type.pointer_depth target_type <= 2 ->
                  define_frame frame_values values void_values description
                    result (Frame_base target_type);
                  (Frame_tick, None)
              | _ -> malformed description "IC_RBP requires one pointer result")
          | Opcode.Ic_imm_i64
            when Option.is_some
                   (Runtime.original_function_slot_address
                      function_slot_addresses raw) -> (
              let receipt =
                Option.get
                  (Runtime.original_function_slot_address
                     function_slot_addresses raw)
              in
              if Runtime.function_slot_address_cursor receipt != raw then
                malformed description
                  "native slot cursor is not its original producer";
              let slot =
                match
                  Option.bind task_snapshot (fun snapshot ->
                      Global_storage.find_function_slot snapshot receipt)
                with
                | Some slot -> slot
                | None ->
                    unsupported description
                      "native function slot requires original task storage"
              in
              match (description.result, description.target_type) with
              | Some result, Some target_type ->
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  (Load_function_slot_cursor (value, slot), None)
              | _ ->
                  malformed description
                    "native slot cursor lost its original value")
          | Opcode.Ic_deref
            when Option.is_some
                   (Runtime.original_function_slot_address
                      function_slot_addresses raw) -> (
              let receipt =
                Option.get
                  (Runtime.original_function_slot_address
                     function_slot_addresses raw)
              in
              let slot =
                match
                  Option.bind task_snapshot (fun snapshot ->
                      Global_storage.find_function_slot snapshot receipt)
                with
                | Some slot -> slot
                | None ->
                    unsupported description
                      "native function slot requires original task storage"
              in
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ cursor_id ], Some result, Some target_type ->
                  let cursor = operand values description position cursor_id in
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  mark_code value (ref task_owned_targets);
                  (Load_function_slot (cursor, slot, value), None)
              | _ ->
                  malformed description
                    "native slot dereference lost its original value")
          | (Opcode.Ic_imm_i64 | Opcode.Ic_abs_addr)
            when Option.is_some
                   (Runtime.original_function_address function_addresses raw)
            -> (
              let receipt =
                Option.get
                  (Runtime.original_function_address function_addresses raw)
              in
              let callee_index =
                match
                  Array.find_index
                    (fun function_ ->
                      retained_link_matches_definition
                        (Runtime.function_address_link receipt)
                        function_.definition)
                    functions
                with
                | Some index -> index
                | None -> (
                    match
                      Array.find_index
                        (fun provider ->
                          Option.fold ~none:false
                            ~some:
                              (Ir.Retained_function.same
                                 (Runtime.function_address_link receipt))
                            (Runtime.function_slot_address_link provider))
                        provider_entries
                    with
                    | Some index -> Array.length functions + index
                    | None ->
                        malformed description
                          "native function address body is outside the sealed \
                           image")
              in
              match (description.result, description.target_type) with
              | Some result, Some target_type ->
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  mark_code value (ref [ callee_index ]);
                  (Load_function_address (value, callee_index), None)
              | _ -> malformed description "invalid native function address")
          | Opcode.Ic_imm_i64
            when match raw.payload with
                 | Some (Sequence.Saved_parameter_default prepared) ->
                     Option.is_some
                       (Ir.Saved_parameter_value.data_source
                          (Prepared_default.value prepared))
                 | Some (Sequence.Saved_callback_default prepared) ->
                     Option.is_some
                       (Ir.Saved_parameter_value.data_source
                          (Prepared_callback_default.value prepared))
                 | _ -> false -> (
              if
                not
                  (List.exists
                     (fun original -> original == raw)
                     (Option.value ~default:[]
                        (Runtime.original_prepared_defaults runtime_calls
                           ~owner:runtime_owner)))
              then
                malformed description
                  "native saved data has no original producer";
              let saved =
                match raw.payload with
                | Some (Sequence.Saved_parameter_default prepared) ->
                    Prepared_default.value prepared
                | Some (Sequence.Saved_callback_default prepared) ->
                    Prepared_callback_default.value prepared
                | _ -> assert false
              in
              let data =
                Option.get (Ir.Saved_parameter_value.data_source saved)
              in
              let offset =
                match
                  Option.bind task_snapshot (fun snapshot ->
                      Global_storage.find_saved_data snapshot data)
                with
                | Some offset -> offset
                | None ->
                    malformed description
                      "native saved data lost its original task-owned capture"
              in
              match (description.result, description.target_type) with
              | Some result, Some target_type
                when Type.equal target_type
                       (Ir.Saved_parameter_value.data_type data) ->
                  ignore (checked_reference description target_type);
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  mark_reference description value;
                  (Load_saved_data (offset, value), None)
              | _ ->
                  malformed description
                    "native saved data lost its original pointer view")
          | Opcode.Ic_imm_i64
            when match raw.payload with
                 | Some (Sequence.Saved_parameter_default prepared) ->
                     Option.is_some
                       (Prepared_default.undefined_callback_source prepared)
                 | Some (Sequence.Saved_callback_default prepared) ->
                     Option.is_some
                       (Prepared_callback_default.undefined_callback_source
                          prepared)
                 | _ -> false -> (
              if
                not
                  (List.exists
                     (fun original -> original == raw)
                     (Option.value ~default:[]
                        (Runtime.original_prepared_defaults runtime_calls
                           ~owner:runtime_owner)))
              then
                malformed description
                  "native undefined default has no original saved producer";
              if
                Option.is_none
                  (Option.bind task_snapshot
                     Global_storage.task_undefined_code_owner)
              then
                malformed description
                  "native undefined default lost its original task entry";
              match (description.result, description.target_type) with
              | Some result, Some target_type ->
                  let word_type =
                    Type.make_primitive ~form:Internal_storage ~primitive:I64
                      ~pointer_depth:0
                    |> Result.get_ok
                  in
                  let value =
                    define values description position result target_type
                      word_type
                  in
                  mark_code value (ref task_owned_targets);
                  (Load_undefined_function_address value, None)
              | _ ->
                  malformed description
                    "native undefined default lost its original callback value")
          | Opcode.Ic_imm_i64
            when raw.flags = 0x2000L
                 &&
                 match !calls with
                 | scope :: _ when scope.phase = Collecting ->
                     List.exists
                       (fun argument ->
                         Sequence.Instruction_id.equal
                           (Runtime.argument_producer argument)
                           raw.instruction_id
                         && (Option.is_some
                               (Runtime.argument_prepared_default argument)
                            || Option.is_some
                                 (Runtime.argument_prepared_callback_default
                                    argument))
                         &&
                         match Runtime.argument_role argument with
                         | Runtime.Fixed index
                           when index >= 0 && index < scope.fixed_count ->
                             Option.is_some scope.argument_callbacks.(index)
                         | _ -> false)
                       (scope_arguments scope)
                 | _ -> false -> (
              (* Only an original saved callback argument is a numeric code
                 word here. Ordinary pointer immediates remain frame addresses;
                 the exact default proof and producer are checked below before
                 any native allocation or emission. *)
              match
                ( description.result,
                  description.target_type,
                  description.payload )
              with
              | Some result, Some target_type, Some (Sequence.Integer bits)
                when description.operands = [] ->
                  let word_type =
                    Type.make_primitive ~form:Internal_storage ~primitive:I64
                      ~pointer_depth:0
                    |> Result.get_ok
                  in
                  let value =
                    define values description position result target_type
                      word_type
                  in
                  word_code_values :=
                    Value_set.add value.value_id !word_code_values;
                  (Load_immediate (value, bits), None)
              | _ ->
                  malformed description "invalid saved callback-word argument")
          | (Opcode.Ic_imm_i64 | Opcode.Ic_abs_addr)
            when Option.fold ~none:false
                   ~some:(fun type_ ->
                     Type.pointer_depth type_ = 1
                     || Type.pointer_depth type_ = 2)
                   description.target_type
                 &&
                 match description.payload with
                 | Some (Sequence.Symbol _ | Sequence.Retained_global _) -> true
                 | _ -> false -> (
              if description.flags <> 0L || description.operands <> [] then
                malformed description "invalid native global address producer";
              match
                ( description.result,
                  description.target_type,
                  description.payload )
              with
              | Some result, Some target_type, Some payload -> (
                  let selected =
                    match payload with
                    | Sequence.Symbol symbol -> (
                        match
                          Global_storage.find_symbol_from_source global_storage
                            ~source_globals symbol
                        with
                        | Some _ as slot -> slot
                        | None
                          when Global_storage.globals global_storage
                               == source_globals ->
                            Global_storage.find_symbol global_storage symbol
                        | None -> None)
                    | Sequence.Retained_global reference -> (
                        match
                          Global_storage.find_retained_from_source
                            global_storage ~source_globals reference
                        with
                        | Some _ as slot -> slot
                        | None
                          when Global_storage.globals global_storage
                               == source_globals ->
                            Global_storage.find_retained global_storage
                              reference
                        | None -> None)
                    | _ -> None
                  in
                  match selected with
                  | None ->
                      malformed description
                        "global address symbol is absent from the exact sealed \
                         storage layout"
                  | Some slot ->
                      let source_slot = Global_storage.source_slot slot in
                      let symbol = Global_storage.symbol slot in
                      let expected_type =
                        match Type.pointer_to (Global_storage.type_ slot) with
                        | Ok type_ -> type_
                        | Error _ ->
                            malformed description
                              "global address slot has no checked pointer type"
                      in
                      if
                        Ir.Integer_globals.storage_symbol source_slot != symbol
                        || Ir.Integer_globals.storage_opcode source_slot
                           <> description.opcode
                        || (not (Type.equal expected_type target_type))
                        || not
                             (Global_storage.owns_address ~source_globals slot
                                runtime_owner)
                      then
                        malformed description
                          "global address opcode, type or exact slot owner is \
                           inconsistent";
                      define_frame frame_values values void_values description
                        result (Global_address slot);
                      (Frame_tick, None))
              | _ ->
                  malformed description "invalid native global address producer"
              )
          | Opcode.Ic_abs_addr
            when Option.fold ~none:false
                   ~some:(fun type_ -> Type.pointer_depth type_ = 1)
                   description.target_type ->
              malformed description
                "native absolute address requires an exact storage symbol"
          | Opcode.Ic_imm_i64
            when Option.fold ~none:false
                   ~some:(fun type_ ->
                     Type.pointer_depth type_ = 1
                     || Type.pointer_depth type_ = 2)
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
          | Opcode.Ic_mul
            when Option.fold ~none:false
                   ~some:(fun type_ ->
                     Type.pointer_depth type_ = 1
                     || Type.pointer_depth type_ = 2)
                   description.target_type -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid native index scaling";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ stride_id; index_id ], Some result, Some target_type ->
                  if
                    not
                      (Type.pointer_depth target_type = 2
                      && Type.base target_type
                         = Type.Primitive (Type.Internal_storage, Primitive.I64)
                      )
                  then ignore (checked_reference description target_type);
                  let stride_type, stride =
                    match frame_operand frame_values description stride_id with
                    | Frame_offset (stride_type, stride) -> (stride_type, stride)
                    | _ ->
                        malformed description
                          "index scale requires its checked constant stride"
                  in
                  if stride <= 0 || not (Type.equal stride_type target_type)
                  then
                    malformed description
                      "index scale stride or pointer type is inconsistent";
                  let index = operand values description position index_id in
                  let index_word =
                    (checked_scalar ~allow_public:true description
                       index.computation_type)
                      .word_type
                  in
                  let scaled =
                    internal_frame_value frame_values values void_values
                      description position result target_type (fun offset ->
                        Index_offset
                          {
                            index_pointer_type = target_type;
                            index_stride = Int64.of_int stride;
                            index_offset = offset;
                          })
                  in
                  ( Scale_index (index_word, Int64.of_int stride, index, scaled),
                    None )
              | _ -> malformed description "invalid native index scaling")
          | (Opcode.Ic_add | Opcode.Ic_sub)
            when Option.fold ~none:false
                   ~some:(fun type_ ->
                     Type.pointer_depth type_ = 1
                     || Type.pointer_depth type_ = 2)
                   description.target_type -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid frame address addition";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ base_id; offset_id ], Some result, Some target_type -> (
                  match Value_map.find_opt offset_id !frame_values with
                  | Some (Frame_offset (offset_type, offset))
                    when description.opcode = Opcode.Ic_add -> (
                      match Value_map.find_opt base_id !frame_values with
                      | Some (Frame_base base_type)
                        when Type.equal base_type target_type
                             && Type.equal offset_type target_type -> (
                          match Int_map.find_opt offset frame_slots with
                          | Some slot -> (
                              match Type.pointer_to slot.slot_type with
                              | Ok expected when Type.equal expected target_type
                                ->
                                  define_frame frame_values values void_values
                                    description result (Frame_address slot);
                                  (Frame_tick, None)
                              | _ ->
                                  malformed description
                                    "frame address pointee type is inconsistent"
                              )
                          | None
                            when Option.fold ~none:false
                                   ~some:(fun (origin, _) ->
                                     origin.data_offset = offset)
                                   variadic -> (
                              let origin, type_ = Option.get variadic in
                              match Type.pointer_to type_ with
                              | Ok expected when Type.equal expected target_type
                                ->
                                  define_frame frame_values values void_values
                                    description result
                                    (Variadic_address (origin, type_));
                                  (Frame_tick, None)
                              | _ ->
                                  malformed description
                                    "variadic address pointee type is \
                                     inconsistent")
                          | None ->
                              malformed description
                                "frame address displacement names no checked \
                                 slot")
                      | _ ->
                          malformed description
                            "frame address operands are inconsistent")
                  | Some (Index_offset scaled) ->
                      if not (Type.equal scaled.index_pointer_type target_type)
                      then
                        malformed description
                          "indexed address changes its checked pointer type";
                      touch position scaled.index_offset;
                      let root, base, strides =
                        match Value_map.find_opt base_id !frame_values with
                        | Some (Variadic_address (origin, type_)) ->
                            (match Type.pointer_to type_ with
                            | Ok expected when Type.equal expected target_type
                              -> ()
                            | _ ->
                                malformed description
                                  "indexed variadic base changes its checked \
                                   pointer type");
                            ( Indexed_object_root
                                {
                                  object_origin = Variadic_reference origin;
                                  object_type = type_;
                                  object_element_count = origin.maximum_count;
                                  object_code = None;
                                },
                              Index_zero,
                              [ 8L ] )
                        | Some (Frame_address slot) ->
                            if slot.slot_dimensions = [] then
                              malformed description
                                "indexed frame base has no checked array \
                                 dimensions";
                            let expected =
                              match Type.pointer_to slot.slot_type with
                              | Ok expected -> expected
                              | Error message -> malformed description message
                            in
                            if not (Type.equal expected target_type) then
                              malformed description
                                "indexed frame base changes its checked \
                                 pointer type";
                            ( Indexed_object_root
                                {
                                  object_origin =
                                    Frame_reference
                                      (frame_reference_origin slot);
                                  object_type = slot.slot_type;
                                  object_element_count = slot.slot_element_count;
                                  object_code =
                                    Option.map
                                      (fun pointer ->
                                        ( pointer,
                                          Frame_table
                                            (Option.get slot.slot_owner_offset),
                                          Option.get slot.owned_targets ))
                                      slot.callback;
                                },
                              Index_zero,
                              slot_strides slot )
                        | Some (Global_address slot) ->
                            if Global_storage.dimensions slot = [] then
                              malformed description
                                "indexed persistent base has no checked array \
                                 dimensions";
                            let expected =
                              match
                                Type.pointer_to (Global_storage.type_ slot)
                              with
                              | Ok expected -> expected
                              | Error message -> malformed description message
                            in
                            if not (Type.equal expected target_type) then
                              malformed description
                                "indexed persistent base changes its checked \
                                 pointer type";
                            ( Indexed_object_root
                                {
                                  object_origin =
                                    Arena_reference (arena_access slot);
                                  object_type = Global_storage.type_ slot;
                                  object_element_count =
                                    Global_storage.element_count slot;
                                  object_code =
                                    Option.map
                                      (fun pointer ->
                                        ( pointer,
                                          Arena_table
                                            (Option.get
                                               (Global_storage.code_owner_offset
                                                  slot)),
                                          arena_targets slot ))
                                      (Global_storage.callback slot);
                                },
                              Index_zero,
                              Global_storage.strides slot )
                        | Some (Indexed_address indexed) ->
                            if
                              not
                                (Type.equal indexed.indexed_pointer_type
                                   target_type)
                            then
                              malformed description
                                "indexed base changes its checked pointer type";
                            touch_indexed position indexed;
                            ( indexed.indexed_root,
                              Index_value indexed.indexed_offset,
                              indexed.indexed_remaining_strides )
                        | Some _ ->
                            malformed description
                              "indexed base is not a checked array address"
                        | None ->
                            let reference =
                              operand values description position base_id
                            in
                            if
                              not
                                (Type.equal reference.declared_type target_type)
                            then
                              malformed description
                                "indexed pointer base changes its checked type";
                            let _, scalar =
                              checked_reference description
                                reference.declared_type
                            in
                            let access = { reference; scalar; offset = None } in
                            ( Indexed_reference_root access,
                              Index_reference reference,
                              [ Int64.of_int scalar.byte_size ] )
                      in
                      let remaining =
                        match strides with
                        | stride :: remaining
                          when Int64.equal stride scaled.index_stride ->
                            remaining
                        | _ ->
                            malformed description
                              "index stride disagrees with the original object \
                               dimensions"
                      in
                      let indexed_offset =
                        internal_frame_value frame_values values void_values
                          description position result target_type (fun offset ->
                            Indexed_address
                              {
                                indexed_root = root;
                                indexed_offset = offset;
                                indexed_pointer_type = target_type;
                                indexed_remaining_strides = remaining;
                              })
                      in
                      ( Apply_index_offset
                          ( (if description.opcode = Opcode.Ic_sub then
                               Encoder.Sub
                             else Encoder.Add),
                            base,
                            scaled.index_offset,
                            indexed_offset ),
                        None )
                  | _ ->
                      malformed description
                        "frame address addition has no checked displacement or \
                         index")
              | _ -> malformed description "invalid frame address addition")
          | Opcode.Ic_addr -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid native reference producer";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ address_id ], Some result, Some target_type -> (
                  let pointee, _ = checked_reference description target_type in
                  let address =
                    address_operand frame_values values description position
                      address_id
                  in
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  mark_reference description value;
                  let materialize origin _element_count offset =
                    let table_offset =
                      Option.get value.reference_descriptor_offset
                    in
                    ( Materialize_reference
                        (origin, Frame_table table_offset, offset, value),
                      None )
                  in
                  match address with
                  | Variadic_address (origin, actual)
                    when Type.equal pointee actual ->
                      materialize (Variadic_reference origin)
                        origin.maximum_count None
                  | Frame_address slot when Option.is_some slot.callback ->
                      unsupported description
                        "native callback cell addresses cannot escape"
                  | Frame_address slot when Type.equal pointee slot.slot_type ->
                      materialize
                        (Frame_reference (frame_reference_origin slot))
                        slot.slot_element_count None
                  | Global_address slot
                    when Option.is_some (Global_storage.callback slot) ->
                      unsupported description
                        "native callback cell addresses cannot escape"
                  | Global_address slot
                    when Type.equal pointee (Global_storage.type_ slot) ->
                      materialize
                        (Arena_reference (arena_access slot))
                        (Global_storage.element_count slot)
                        None
                  | Reference_address (access, actual)
                    when Type.equal pointee actual ->
                      (Materialize_existing_reference (access, value), None)
                  | Indexed_address
                      {
                        indexed_root =
                          Indexed_object_root { object_code = Some _; _ };
                        _;
                      } ->
                      unsupported description
                        "native callback array addresses cannot escape"
                  | Indexed_address indexed -> (
                      match Type.dereference indexed.indexed_pointer_type with
                      | Error message -> malformed description message
                      | Ok actual when Type.equal pointee actual -> (
                          match indexed.indexed_root with
                          | Indexed_object_root object_ ->
                              materialize object_.object_origin
                                object_.object_element_count
                                (Some indexed.indexed_offset)
                          | Indexed_reference_root access ->
                              ( Materialize_existing_reference
                                  ( {
                                      access with
                                      offset = Some indexed.indexed_offset;
                                    },
                                    value ),
                                None ))
                      | Ok _ ->
                          malformed description
                            "indexed reference changes its checked pointee type"
                      )
                  | _ ->
                      malformed description
                        "reference producer does not name its exact scalar \
                         object")
              | _ -> malformed description "invalid native reference producer")
          | Opcode.Ic_deref -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid scalar frame load";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ address_id ], Some result, Some target_type -> (
                  match
                    address_operand frame_values values description position
                      address_id
                  with
                  | Frame_address slot
                    when Option.is_some slot.callback
                         && slot.slot_dimensions = [] ->
                      load_code description raw position result target_type
                        (Option.get slot.callback) slot.slot_type
                        (Option.get slot.owned_targets) (fun value ->
                          Load_code_frame
                            ( slot.access,
                              Option.get slot.slot_owner_offset,
                              value ))
                  | Global_address slot
                    when Option.is_some (Global_storage.callback slot)
                         && Global_storage.dimensions slot = [] ->
                      load_code description raw position result target_type
                        (Option.get (Global_storage.callback slot))
                        (Global_storage.type_ slot)
                        (arena_targets slot)
                        (fun value ->
                          Load_code_arena
                            ( arena_access slot,
                              Option.get (Global_storage.code_owner_offset slot),
                              value ))
                  | Indexed_address indexed
                    when indexed.indexed_remaining_strides = []
                         &&
                         match indexed.indexed_root with
                         | Indexed_object_root { object_code = Some _; _ } ->
                             true
                         | _ -> false ->
                      let object_ =
                        match indexed.indexed_root with
                        | Indexed_object_root object_ -> object_
                        | _ -> assert false
                      in
                      let pointer, home, targets =
                        Option.get object_.object_code
                      in
                      load_code description raw position result target_type
                        pointer object_.object_type targets (fun value ->
                          Load_code_indexed
                            ( {
                                origin = object_.object_origin;
                                offset = indexed.indexed_offset;
                              },
                              home,
                              value ))
                  | Frame_address slot
                    when slot.slot_dimensions = []
                         && Type.equal target_type slot.slot_type ->
                      checked_copy description target_type target_type;
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      if Type.pointer_depth target_type = 0 then
                        (Load_frame_value (slot.access, value), None)
                      else (
                        mark_reference description value;
                        (Load_reference_frame (slot.access, value), None))
                  | Global_address slot
                    when Global_storage.dimensions slot = []
                         && Type.equal target_type (Global_storage.type_ slot)
                    ->
                      ignore
                        (checked_scalar ~allow_public:true description
                           target_type);
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      (Load_arena_value (arena_access slot, value), None)
                  | Reference_address (access, pointee)
                    when Type.equal target_type pointee ->
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      (Load_reference_value (access, value), None)
                  | Indexed_address indexed
                    when indexed.indexed_remaining_strides = [] -> (
                      match indexed.indexed_root with
                      | Indexed_object_root object_
                        when Type.equal target_type object_.object_type ->
                          let value =
                            define values description position result
                              target_type
                              (Computation.forward target_type)
                          in
                          ( Load_indexed_object_value
                              ( {
                                  origin = object_.object_origin;
                                  offset = indexed.indexed_offset;
                                },
                                value ),
                            None )
                      | Indexed_reference_root access -> (
                          match
                            Type.dereference indexed.indexed_pointer_type
                          with
                          | Ok pointee when Type.equal target_type pointee ->
                              let value =
                                define values description position result
                                  target_type
                                  (Computation.forward target_type)
                              in
                              ( Load_reference_value
                                  ( {
                                      access with
                                      offset = Some indexed.indexed_offset;
                                    },
                                    value ),
                                None )
                          | _ ->
                              malformed description
                                "indexed scalar load changes its checked \
                                 pointee type")
                      | _ ->
                          malformed description
                            "indexed scalar load changes its checked frame type"
                      )
                  | _ ->
                      malformed description
                        "scalar load does not name its exact checked slot")
              | _ -> malformed description "invalid scalar load")
          | Opcode.Ic_assign -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid scalar frame assignment";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ address_id; input_id ], Some result, Some target_type -> (
                  match
                    address_operand frame_values values description position
                      address_id
                  with
                  | Frame_address slot
                    when Option.is_some slot.callback
                         && slot.slot_dimensions = [] ->
                      if not (Type.equal target_type slot.slot_type) then
                        malformed description
                          "callback store changes its original storage type";
                      store_code description position result target_type
                        input_id (Option.get slot.owned_targets)
                        (fun input value ->
                          Store_code_frame
                            ( slot.access,
                              Option.get slot.slot_owner_offset,
                              input,
                              value ))
                  | Global_address slot
                    when Option.is_some (Global_storage.callback slot)
                         && Global_storage.dimensions slot = [] ->
                      if
                        not (Type.equal target_type (Global_storage.type_ slot))
                      then
                        malformed description
                          "callback store changes its original storage type";
                      store_code description position result target_type
                        input_id (arena_targets slot) (fun input value ->
                          Store_code_arena
                            ( arena_access slot,
                              Option.get (Global_storage.code_owner_offset slot),
                              input,
                              value ))
                  | Indexed_address indexed
                    when indexed.indexed_remaining_strides = []
                         &&
                         match indexed.indexed_root with
                         | Indexed_object_root { object_code = Some _; _ } ->
                             true
                         | _ -> false ->
                      let object_ =
                        match indexed.indexed_root with
                        | Indexed_object_root object_ -> object_
                        | _ -> assert false
                      in
                      if not (Type.equal target_type object_.object_type) then
                        malformed description
                          "callback store changes its original storage type";
                      let _, home, targets = Option.get object_.object_code in
                      store_code description position result target_type
                        input_id targets (fun input value ->
                          Store_code_indexed
                            ( {
                                origin = object_.object_origin;
                                offset = indexed.indexed_offset;
                              },
                              home,
                              input,
                              value ))
                  | Frame_address slot
                    when slot.slot_dimensions = []
                         && Type.equal target_type slot.slot_type ->
                      let input =
                        operand values description position input_id
                      in
                      checked_copy description target_type input.declared_type;
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      if Type.pointer_depth target_type = 0 then
                        (Store_frame_value (slot.access, input, value), None)
                      else (
                        mark_reference description value;
                        ( Store_reference_frame
                            ( slot.access,
                              Option.get slot.slot_reference_offset,
                              input,
                              value ),
                          None ))
                  | Global_address slot
                    when Global_storage.dimensions slot = []
                         && Type.equal target_type (Global_storage.type_ slot)
                    ->
                      let input =
                        operand values description position input_id
                      in
                      checked_copy description target_type input.declared_type;
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      (Store_arena_value (arena_access slot, input, value), None)
                  | Reference_address (access, pointee)
                    when Type.equal target_type pointee ->
                      let input =
                        operand values description position input_id
                      in
                      checked_copy description target_type input.declared_type;
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      (Store_reference_value (access, input, value), None)
                  | Indexed_address indexed
                    when indexed.indexed_remaining_strides = [] -> (
                      let input =
                        operand values description position input_id
                      in
                      checked_copy description target_type input.declared_type;
                      let value =
                        define values description position result target_type
                          (Computation.forward target_type)
                      in
                      match indexed.indexed_root with
                      | Indexed_object_root object_
                        when Type.equal target_type object_.object_type ->
                          ( Store_indexed_object_value
                              ( {
                                  origin = object_.object_origin;
                                  offset = indexed.indexed_offset;
                                },
                                input,
                                value ),
                            None )
                      | Indexed_reference_root access -> (
                          match
                            Type.dereference indexed.indexed_pointer_type
                          with
                          | Ok pointee when Type.equal target_type pointee ->
                              ( Store_reference_value
                                  ( {
                                      access with
                                      offset = Some indexed.indexed_offset;
                                    },
                                    input,
                                    value ),
                                None )
                          | _ ->
                              malformed description
                                "indexed assignment changes its checked \
                                 pointee type")
                      | _ ->
                          malformed description
                            "indexed assignment changes its checked frame type")
                  | _ ->
                      malformed description
                        "scalar assignment does not name its exact slot")
              | _ -> malformed description "invalid scalar assignment")
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
                  match
                    address_operand frame_values values description position
                      address_id
                  with
                  | Frame_address slot
                    when Option.is_some slot.callback
                         && slot.slot_dimensions = [] ->
                      update_callback description position result target_type
                        slot.slot_type
                        (Callback_frame
                           (slot.access, Option.get slot.slot_owner_offset))
                        operands site arithmetic_sites
                  | Global_address slot
                    when Option.is_some (Global_storage.callback slot)
                         && Global_storage.dimensions slot = [] ->
                      update_callback description position result target_type
                        (Global_storage.type_ slot)
                        (Callback_arena
                           ( arena_access slot,
                             Option.get (Global_storage.code_owner_offset slot)
                           ))
                        operands site arithmetic_sites
                  | Indexed_address
                      {
                        indexed_root =
                          Indexed_object_root
                            ({ object_code = Some (_, home, _); _ } as object_);
                        indexed_offset;
                        indexed_remaining_strides = [];
                        _;
                      } ->
                      update_callback description position result target_type
                        object_.object_type
                        (Callback_indexed
                           ( {
                               origin = object_.object_origin;
                               offset = indexed_offset;
                             },
                             home ))
                        operands site arithmetic_sites
                  | Frame_address slot
                    when slot.slot_dimensions = []
                         && Type.equal target_type slot.slot_type ->
                      ignore
                        (checked_scalar ~allow_public:true description
                           target_type);
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
                            check_update_operand description input;
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
                  | Global_address slot
                    when Global_storage.dimensions slot = []
                         && Type.equal target_type (Global_storage.type_ slot)
                    ->
                      let access = arena_access slot in
                      let update, old_result, expects_operand =
                        Option.get
                          (callable_frame_update description.opcode
                             access.arena_word)
                      in
                      let input =
                        match (expects_operand, operands) with
                        | true, [ input_id ] ->
                            let input =
                              operand values description position input_id
                            in
                            check_update_operand description input;
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
                                signed = access.arena_word = I64;
                              }
                            in
                            arithmetic_sites := fault_site :: !arithmetic_sites;
                            Some fault_site
                        | Update_binary _ | Update_shift _ -> None
                      in
                      ( Update_arena_value
                          ( access,
                            update,
                            input,
                            old_result,
                            value,
                            access.arena_word,
                            arithmetic_site ),
                        None )
                  | Reference_address (access, pointee)
                    when Type.equal target_type pointee ->
                      let update, old_result, expects_operand =
                        Option.get
                          (callable_frame_update description.opcode
                             access.scalar.word_type)
                      in
                      let input =
                        match (expects_operand, operands) with
                        | true, [ input_id ] ->
                            let input =
                              operand values description position input_id
                            in
                            check_update_operand description input;
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
                                signed = access.scalar.word_type = I64;
                              }
                            in
                            arithmetic_sites := fault_site :: !arithmetic_sites;
                            Some fault_site
                        | Update_binary _ | Update_shift _ -> None
                      in
                      ( Update_reference_value
                          ( access,
                            update,
                            input,
                            old_result,
                            value,
                            access.scalar.word_type,
                            arithmetic_site ),
                        None )
                  | Indexed_address indexed
                    when indexed.indexed_remaining_strides = [] -> (
                      let word, operation =
                        match indexed.indexed_root with
                        | Indexed_object_root object_
                          when Type.equal target_type object_.object_type ->
                            ( (reference_scalar object_.object_origin).word_type,
                              `Object object_ )
                        | Indexed_reference_root access -> (
                            match
                              Type.dereference indexed.indexed_pointer_type
                            with
                            | Ok pointee when Type.equal target_type pointee ->
                                (access.scalar.word_type, `Reference access)
                            | _ ->
                                malformed description
                                  "indexed update changes its checked pointee \
                                   type")
                        | _ ->
                            malformed description
                              "indexed update changes its checked frame type"
                      in
                      let update, old_result, expects_operand =
                        Option.get
                          (callable_frame_update description.opcode word)
                      in
                      let input =
                        match (expects_operand, operands) with
                        | true, [ input_id ] ->
                            let input =
                              operand values description position input_id
                            in
                            check_update_operand description input;
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
                                signed = word = I64;
                              }
                            in
                            arithmetic_sites := fault_site :: !arithmetic_sites;
                            Some fault_site
                        | Update_binary _ | Update_shift _ -> None
                      in
                      match operation with
                      | `Object object_ ->
                          ( Update_indexed_object_value
                              ( {
                                  origin = object_.object_origin;
                                  offset = indexed.indexed_offset;
                                },
                                update,
                                input,
                                old_result,
                                value,
                                word,
                                arithmetic_site ),
                            None )
                      | `Reference access ->
                          ( Update_reference_value
                              ( {
                                  access with
                                  offset = Some indexed.indexed_offset;
                                },
                                update,
                                input,
                                old_result,
                                value,
                                word,
                                arithmetic_site ),
                            None ))
                  | _ ->
                      malformed description
                        "scalar update does not name its exact slot")
              | _ -> malformed description "invalid scalar update")
          | Opcode.Ic_sub
          | Opcode.Ic_equ_equ
          | Opcode.Ic_not_equ
          | Opcode.Ic_less
          | Opcode.Ic_less_equ
          | Opcode.Ic_greater
          | Opcode.Ic_greater_equ
            when List.exists
                   (fun id ->
                     Option.fold ~none:false
                       ~some:(fun (value : value) ->
                         Type.pointer_depth value.declared_type > 0)
                       (Value_map.find_opt id !values))
                   description.operands -> (
              if description.flags <> 0L || Option.is_some description.payload
              then malformed description "invalid native pointer comparison";
              match
                ( description.operands,
                  description.result,
                  description.target_type )
              with
              | [ left_id; right_id ], Some result, Some target_type ->
                  let left = operand values description position left_id
                  and right = operand values description position right_id in
                  ignore (checked_reference description left.declared_type);
                  ignore (checked_reference description right.declared_type);
                  if
                    (not
                       (compatible_reference left.declared_type
                          right.declared_type))
                    || checked_word description target_type <> I64
                  then
                    malformed description
                      "invalid native pointer comparison types";
                  let value =
                    define values description position result target_type
                      (Computation.forward target_type)
                  in
                  let operation =
                    match description.opcode with
                    | Opcode.Ic_equ_equ ->
                        Apply_reference_comparison
                          (Encoder.E, left, right, value)
                    | Opcode.Ic_sub ->
                        Apply_reference_difference (left, right, value)
                    | Opcode.Ic_not_equ ->
                        Apply_reference_comparison
                          (Encoder.NE, left, right, value)
                    | Opcode.Ic_less ->
                        Apply_reference_ordering (Encoder.L, left, right, value)
                    | Opcode.Ic_less_equ ->
                        Apply_reference_ordering (Encoder.LE, left, right, value)
                    | Opcode.Ic_greater ->
                        Apply_reference_ordering (Encoder.G, left, right, value)
                    | Opcode.Ic_greater_equ ->
                        Apply_reference_ordering (Encoder.GE, left, right, value)
                    | _ -> assert false
                  in
                  (operation, None)
              | _ ->
                  malformed description
                    "invalid native pointer comparison shape")
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
                  let operation, value_type =
                    if Value_set.mem operand_id !void_values then
                      (Discard_void, None)
                    else
                      let input =
                        operand values description position operand_id
                      in
                      if Option.is_some (code_source input) then
                        if is_entry && capture_callback_default then
                          (Discard_callback_default input, Some I64)
                        else if
                          task_dynamic_code_words && has_code_word_view input
                        then
                          ( Discard_value (input, I64),
                            if is_entry then Some I64 else None )
                        else (Discard_void, None)
                      else if Type.pointer_depth input.declared_type <> 0 then (
                        ignore
                          (checked_reference description input.declared_type);
                        match (capture_data_default, task_snapshot) with
                        | Some data, Some snapshot when is_entry ->
                            let offset =
                              Option.get
                                (Global_storage.find_saved_data snapshot data)
                            in
                            (Discard_data_default (input, offset), None)
                        | _ -> (Discard_void, None))
                      else
                        let word =
                          (checked_scalar ~allow_public:true description
                             input.declared_type)
                            .word_type
                        in
                        ( Discard_value (input, word),
                          if is_entry then Some word else None )
                  in
                  if
                    Runtime.is_implicit_discard runtime_calls
                      ~owner:runtime_owner description.instruction_id
                  then (Frame_tick, None)
                  else (operation, value_type)
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
                      if
                        not (task_dynamic_code_words && has_code_word_view input)
                      then
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
        (match operation with
        | Load_immediate (value, 0L) ->
            zero_values := Value_set.add value.value_id !zero_values
        | Store_frame_value (access, input, _)
          when Option.is_some (code_source input) ->
            if not (Int_map.mem access.frame_offset code_cells) then
              unsupported description
                "native owned code values cannot escape into ordinary integer \
                 cells"
        | Store_arena_value (_, input, _)
        | Store_reference_value (_, input, _)
        | Store_indexed_object_value (_, input, _)
          when Option.is_some (code_source input) ->
            unsupported description
              "native owned code values cannot escape local callback storage"
        | _ -> ());
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
                  scope_arguments scope
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
                    match
                      call_argument_index ~fixed_count:scope.fixed_count
                        ~variadic_count:scope.variadic_count
                        (Runtime.argument_role argument)
                    with
                    | Some index
                      when index >= 0
                           && index < Array.length scope.pushed
                           && not scope.pushed.(index) ->
                        if
                          Option.is_some scope.callback_call
                          && Option.is_some
                               (Runtime.argument_prepared_default argument)
                        then
                          unsupported raw
                            "anonymous callback arguments cannot carry named \
                             default evidence";
                        (match Runtime.argument_prepared_default argument with
                        | None -> ()
                        | Some prepared -> (
                            match parameter_defaults with
                            | None ->
                                unsupported raw
                                  "native callable programs do not admit \
                                   prepared parameter defaults"
                            | Some proof ->
                                let header =
                                  Runtime.header (Option.get scope.call)
                                in
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
                                  || (not
                                        (match
                                           ( Prepared_default.word_bits prepared,
                                             raw.payload )
                                         with
                                        | ( Some bits,
                                            Some (Sequence.Integer actual) ) ->
                                            Int64.equal bits actual
                                        | ( None,
                                            Some
                                              (Sequence.Saved_parameter_default
                                                 original) ) ->
                                            original == prepared
                                        | _ -> false))
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
                        (match
                           Runtime.argument_prepared_callback_default argument
                         with
                        | None -> ()
                        | Some prepared ->
                            let pointer =
                              match scope.callback_call with
                              | Some callback -> callback.callback_pointer
                              | None ->
                                  malformed raw
                                    "anonymous default has no original \
                                     callback scope"
                            in
                            let parameter =
                              pointer |> Headers.function_pointer_signature
                              |> Headers.signature_parameters
                              |> fun parameters -> List.nth_opt parameters index
                            in
                            (match (parameter_defaults, parameter) with
                            | Some proof, Some parameter
                              when Defaults.admits_callback proof ~prepared
                                     ~pointer ~parameter -> ()
                            | _ ->
                                malformed raw
                                  "anonymous default is outside its sealed \
                                   native source authority");
                            if
                              raw.opcode <> Opcode.Ic_imm_i64
                              || raw.operands <> [] || raw.flags <> 0x2000L
                              || (not
                                    (match
                                       ( Prepared_callback_default.word_bits
                                           prepared,
                                         raw.payload )
                                     with
                                    | Some bits, Some (Sequence.Integer actual)
                                      -> Int64.equal bits actual
                                    | ( None,
                                        Some
                                          (Sequence.Saved_callback_default
                                             original) ) -> original == prepared
                                    | _ -> false))
                              || not
                                   (Option.fold ~none:false
                                      ~some:
                                        (Type.equal
                                           (Prepared_callback_default.type_
                                              prepared))
                                      raw.target_type)
                            then
                              malformed raw
                                "anonymous default producer differs from its \
                                 sealed declaration-time value");
                        (match raw.target_type with
                        | Some type_
                          when Type.equal
                                 (Runtime.argument_source_type argument)
                                 type_ -> ()
                        | _ ->
                            malformed raw
                              "pushed argument source type is inconsistent");
                        if Option.is_some scope.argument_owner_stages.(index)
                        then (
                          if
                            Option.is_none (code_source value)
                            && not (has_code_word_view value)
                          then
                            ignore
                              (checked_scalar ~allow_public:true raw
                                 value.declared_type);
                          Option.iter
                            (fun source ->
                              let destination callee =
                                Int_map.find
                                  (16 + (8 * index))
                                  callee.frame_slots
                                |> fun slot -> Option.get slot.owned_targets
                              in
                              match scope.callback_call with
                              | Some callback ->
                                  Array.iteri
                                    (fun callee_index callee ->
                                      if
                                        callable_callback_matches callback
                                          ~argument_types:scope.argument_types
                                          ~argument_callbacks:
                                            scope.argument_callbacks
                                          ~fixed_count:scope.fixed_count callee
                                      then
                                        indirect_code_edges :=
                                          ( Option.get scope.owned_targets,
                                            callee_index,
                                            destination callee,
                                            source )
                                          :: !indirect_code_edges)
                                    functions
                              | None ->
                                  let callee_index =
                                    match scope.target with
                                    | Source_function index -> index
                                    | Undefined_extern _
                                    | Mismatched_extern _
                                    | Put_chars_provider
                                    | Print_provider _ -> assert false
                                  in
                                  code_edges :=
                                    ( destination functions.(callee_index),
                                      source )
                                    :: !code_edges)
                            (code_source value))
                        else if
                          Option.is_some (code_source value)
                          && not (has_code_word_view value)
                        then
                          unsupported raw
                            "native code values require a callback parameter"
                        else if has_code_word_view value then
                          ignore
                            (checked_scalar ~allow_public:true raw
                               (Runtime.argument_target_type argument))
                        else
                          checked_copy raw
                            (Runtime.argument_target_type argument)
                            value.declared_type;
                        (match Runtime.argument_role argument with
                        | Runtime.Variadic_count ->
                            if
                              raw.opcode <> Opcode.Ic_imm_i64
                              || raw.operands <> [] || raw.flags <> 0x2000L
                              || raw.payload
                                 <> Option.map
                                      (fun count -> Sequence.Integer count)
                                      scope.variadic_count
                            then
                              malformed raw
                                "native variadic count producer lost its \
                                 original captured argument count"
                        | Runtime.Fixed _ | Runtime.Variadic _ -> ());
                        scope.pushed.(index) <- true;
                        value.last_use <- max value.last_use position;
                        Some
                          ( value,
                            scope.argument_stages.(index),
                            scope.argument_owner_stages.(index),
                            if Option.is_some value.reference_descriptor_offset
                            then Some (reserve_reference_table raw)
                            else None )
                    | Some _ | None ->
                        malformed raw "pushed argument role is inconsistent")
                | _ ->
                    malformed raw "pushed result has no exact runtime argument")
            | _ -> malformed raw "ICF_PUSH_RES has no collecting outer call"
        in
        let arithmetic =
          match operation with
          | Internal_mod_u64 _ -> Some (Remainder, false)
          | Apply_division (arithmetic_operation, word, _, _, _, _) ->
              Some (arithmetic_operation, word = I64)
          | Update_callback_value
              (_, Update_division arithmetic_operation, _, _, _, _) ->
              Some (arithmetic_operation, true)
          | Update_frame_value
              (_, Update_division arithmetic_operation, _, _, _, word, _) ->
              Some (arithmetic_operation, word = I64)
          | Update_reference_value
              (_, Update_division arithmetic_operation, _, _, _, word, _)
          | Update_indexed_object_value
              (_, Update_division arithmetic_operation, _, _, _, word, _)
          | Update_arena_value
              (_, Update_division arithmetic_operation, _, _, _, word, _) ->
              Some (arithmetic_operation, word = I64)
          | _ -> None
        in
        let provider_print_site =
          match operation with
          | Indirect_call indirect ->
              Array.exists
                (fun (index, receipt) ->
                  ( Runtime.function_slot_address_provider receipt |> function
                    | Some
                        ( Runtime.Print
                        | Runtime.Stream_print
                        | Runtime.Stream_exe_print ) -> true
                    | _ -> false )
                  && fst (indirect.target_call (Array.length functions + index)))
                (Array.mapi
                   (fun index receipt -> (index, receipt))
                   provider_entries)
          | _ -> false
        in
        let provider_stream_site provider =
          match operation with
          | Print_output call ->
              call.target
              =
              if provider = Runtime.Stream_print then Print_codegen.Generation
              else Print_codegen.Formatted_source
          | Indirect_call indirect ->
              Array.exists
                (fun (index, receipt) ->
                  Runtime.function_slot_address_provider receipt = Some provider
                  && fst (indirect.target_call (Array.length functions + index)))
                (Array.mapi
                   (fun index receipt -> (index, receipt))
                   provider_entries)
          | _ -> false
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
              | Extern_signature_fault
              | Undefined_extern_call
              | Direct_call _
              | Indirect_call _
              | Put_chars _
              | Print_output _ -> true
              | _ -> false);
            extern_signature_site =
              (match operation with
              | Extern_signature_fault -> true
              | _ -> false);
            undefined_extern_site =
              (match operation with
              | Undefined_extern_call | Indirect_call _ -> true
              | _ -> false);
            callback_call_site =
              (match operation with
              | Indirect_call _ -> true
              | _ -> false);
            code_comparison_site =
              (match operation with
              | Apply_code_comparison _ -> true
              | _ -> false);
            callback_capture_site =
              (match operation with
              | Discard_callback_default _ -> true
              | _ -> false);
            data_capture_site =
              (match operation with
              | Discard_data_default _ -> true
              | _ -> false);
            no_value_capture_site =
              (match operation with
              | Discard_void -> true
              | Discard_value (input, _) ->
                  Option.is_some input.code_owner_offset
              | _ -> false);
            code_word_escape_site =
              (match operation with
                | Return_value input -> Option.is_some input.code_owner_offset
                | Discard_callback_default _ -> true
                | Apply_unary (_, input, _)
                | Apply_constant_shift (_, input, _, _)
                | Apply_logical_not (input, _) ->
                    Option.is_some input.code_owner_offset
                | Apply_binary (_, left, right, _)
                | Apply_shift (_, left, right, _)
                | Apply_division (_, _, _, left, right, _)
                | Apply_comparison (_, left, right, _)
                | Apply_logical (_, left, right, _) ->
                    Option.is_some left.code_owner_offset
                    || Option.is_some right.code_owner_offset
                | Update_frame_value (_, _, input, _, _, _, _)
                | Update_arena_value (_, _, input, _, _, _, _)
                | Update_reference_value (_, _, input, _, _, _, _)
                | Update_indexed_object_value (_, _, input, _, _, _, _)
                | Update_callback_value (_, _, input, _, _, _) ->
                    Option.fold ~none:false
                      ~some:(fun input ->
                        Option.is_some input.code_owner_offset)
                      input
                | _ -> false)
              || Option.fold ~none:false
                   ~some:(fun (value, _, owner_stage, _) ->
                     Option.is_none owner_stage
                     && Option.is_some value.code_owner_offset)
                   push_stage;
            code_update_site =
              (match operation with
              | Update_callback_value _ -> true
              | _ -> false);
            uninitialized_read_site =
              (match operation with
              | Load_frame_value ({ initialized_flag_offset = Some _; _ }, _)
              | Load_reference_frame ({ initialized_flag_offset = Some _; _ }, _)
              | Load_code_frame ({ initialized_flag_offset = Some _; _ }, _, _)
                -> true
              | Update_frame_value
                  ({ initialized_flag_offset = Some _; _ }, _, _, _, _, _, _) ->
                  true
              | Update_callback_value
                  ( Callback_frame ({ initialized_flag_offset = Some _; _ }, _),
                    _,
                    _,
                    _,
                    _,
                    _ )
              | Update_callback_value (Callback_arena _, _, _, _, _, _)
              | Update_callback_value (Callback_indexed _, _, _, _, _, _) ->
                  true
              | Load_arena_value _
              | Load_code_arena _
              | Update_arena_value _
              | Load_indexed_object_value _
              | Load_code_indexed _
              | Update_indexed_object_value _
              | Load_reference_value _
              | Update_reference_value _
              | Internal_mod_u64 _
              | Internal_bit _
              | Internal_swap _
              | Internal_strlen _
              | Print_output _ -> true
              | Indirect_call _ when provider_print_site -> true
              | _ -> false);
            index_scale_site =
              (match operation with
              | Scale_index _ -> true
              | _ -> false);
            index_addition_site =
              (match operation with
              | Apply_index_offset _ | Internal_bit _ | Print_output _ -> true
              | Indirect_call _ when provider_print_site -> true
              | _ -> false);
            address_bounds_site =
              (match operation with
              | Update_callback_value (Callback_indexed _, _, _, _, _, _) ->
                  true
              | Materialize_reference (_, _, Some _, _)
              | Materialize_existing_reference ({ offset = Some _; _ }, _)
              | Load_reference_value _
              | Store_reference_value _
              | Update_reference_value _
              | Load_indexed_object_value _
              | Load_code_indexed _
              | Store_indexed_object_value _
              | Store_code_indexed _
              | Update_indexed_object_value _
              | Internal_mod_u64 _
              | Internal_bit _
              | Internal_swap _
              | Internal_strlen _
              | Print_output _ -> true
              | Indirect_call _ when provider_print_site -> true
              | Materialize_reference (_, _, None, _)
              | Materialize_existing_reference ({ offset = None; _ }, _)
              | _ -> false);
            pointer_ordering_site =
              (match operation with
              | Apply_reference_ordering _ -> true
              | _ -> false);
            pointer_difference_site =
              (match operation with
              | Apply_reference_difference _ -> true
              | _ -> false);
            output_site =
              (match operation with
              | Put_chars _ | Print_output _ -> true
              | Indirect_call _ -> Array.length provider_entries > 0
              | _ -> false);
            stream_print_site = provider_stream_site Runtime.Stream_print;
            stream_exe_site = provider_stream_site Runtime.Stream_exe_print;
            atomic_output_site =
              (match operation with
              | Print_output _ -> true
              | Indirect_call _ when provider_print_site -> true
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
    if !intrinsics <> [] then
      reject "HCBACK0003"
        (Printf.sprintf
           "native callable block ^b%d ends inside an internal call"
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
        let next = Block_map.find (Graph.block_id block) next_blocks in
        visit (visit_block block next :: reversed) remaining
  in
  let callable_blocks =
    visit [] (Graph.definition_order graph) |> source_order_blocks blocks
  in
  let external_ids = external_values graph in
  let add value map = Value_map.add value.value_id value map in
  let add_reference reference map =
    let map = add reference.reference map in
    Option.fold ~none:map ~some:(fun value -> add value map) reference.offset
  in
  let extra =
    Value_set.fold
      (fun id map ->
        match Value_map.find_opt id !frame_values with
        | Some (Reference_address (reference, _)) -> add_reference reference map
        | Some (Index_offset offset) -> add offset.index_offset map
        | Some (Indexed_address indexed) -> (
            let map = add indexed.indexed_offset map in
            match indexed.indexed_root with
            | Indexed_object_root _ -> map
            | Indexed_reference_root reference -> add_reference reference map)
        | _ -> map)
      external_ids Value_map.empty
  in
  let callable_shared_values =
    shared_runtime_values ~external_ids ~extra !values
  in
  (if not is_entry then
     let return_kind =
       match expected_return with
       | Some type_ -> source_return_kind type_
       | None -> reject "HCBACK0003" "native source function has no return type"
     in
     validate_callable_returns graph return_kind);
  {
    callable_blocks;
    callable_sites =
      List.sort
        (fun left right ->
          Int.compare left.global_position right.global_position)
        !sites_rev;
    callable_ir_count = !ir_count;
    callable_block_count = List.length blocks;
    callable_home_slots = !home_slots;
    callable_stage_slots = !stage_high_water;
    callable_reference_bytes = !reference_bytes;
    callable_shared_values;
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

let callable_definition_matches_call
    (definition : Ir.Integer_interpreter.function_definition) call =
  Function.callable_symbol definition.body == Runtime.symbol call
  &&
  match Function.definition_declaration definition.body with
  | Some declaration -> declaration == Runtime.declaration call
  | None -> false

let collect_task_callable_sources ~max_ir_instructions ~max_blocks ~globals
    ~runtime_calls ~entry ~functions ~retained_function_source
    ~retained_slot_binding ~retained_slot_address_binding
    ~retained_slot_address_refresh ~prior_function_slots ~prior_code_owners
    ~prior_provider_code_owners =
  let slot_bindings = ref [] in
  let slot_address_bindings = ref [] in
  let code_owners = ref [] in
  let root_runtime_calls = runtime_calls in
  let queue = Queue.create () in
  let collected_rev = ref [] in
  let admitted_blocks = ref 0 in
  let admitted_ir = ref 0 in
  let charge_graph graph =
    List.iter
      (fun block ->
        if !admitted_blocks = max_blocks then
          reject "HCBACK0001"
            (Printf.sprintf "block count exceeds max_blocks (%d)" max_blocks);
        incr admitted_blocks;
        Graph.instructions block |> Sequence.instructions
        |> List.iter (fun instruction ->
            if !admitted_ir = max_ir_instructions then
              reject ?span:(Sequence.description instruction).span "HCBACK0001"
                (Printf.sprintf
                   "IR instruction count exceeds max_ir_instructions (%d)"
                   max_ir_instructions);
            incr admitted_ir))
      (Graph.blocks graph)
  in
  let find_body body =
    List.find_opt
      (fun source -> source.source_definition.body == body)
      !collected_rev
  in
  let add source =
    let definition = source.source_definition in
    let body = definition.body in
    match find_body body with
    | Some prior ->
        if
          prior.source_definition.frame != definition.frame
          || prior.source_runtime_calls != source.source_runtime_calls
          || prior.source_globals != source.source_globals
        then
          reject ?span:(Function.span body) "HCBACK0003"
            "native task callable body has conflicting original source \
             provenance"
    | None ->
        charge_graph (Function.body body);
        collected_rev := source :: !collected_rev;
        Queue.add source queue
  in
  let current_source definition =
    {
      source_definition = definition;
      source_runtime_calls = runtime_calls;
      source_globals = globals;
      source_functions = functions;
      source_historical = false;
    }
  in
  charge_graph (Ir.X87_stack.graph entry);
  List.iter (fun definition -> add (current_source definition)) functions;
  let exact_local source_functions call =
    List.find_opt
      (fun definition -> callable_definition_matches_call definition call)
      source_functions
  in
  let validate_resolved link
      (source : Ir.Integer_interpreter.task_function_source) =
    let definition = source.source_definition in
    let body = definition.body in
    if not (Ir.Integer_globals.same_task_storage globals source.source_globals)
    then
      reject ?span:(Function.span body) "HCBACK0003"
        "retained native task function belongs to another original task storage";
    if not (retained_link_matches_definition link definition) then
      reject ?span:(Function.span body) "HCBACK0003"
        "retained native task function does not match its exact source body, \
         frame and declaration";
    if
      not
        (List.exists
           (fun (candidate : Ir.Integer_interpreter.function_definition) ->
             candidate.body == body && candidate.frame == definition.frame)
           source.source_functions)
    then
      reject ?span:(Function.span body) "HCBACK0003"
        "retained native task function is absent from its original callable \
         bundle";
    {
      source_definition = definition;
      source_runtime_calls = source.source_runtime_calls;
      source_globals = source.source_globals;
      source_functions = source.source_functions;
      source_historical = true;
    }
  in
  let resolve_retained link =
    match retained_function_source link with
    | Ok source -> validate_resolved link source
    | Error message ->
        reject "HCBACK0003"
          ("retained native task function source resolution failed: " ^ message)
  in
  let own_source link source =
    add source;
    if
      not
        (List.exists
           (fun (candidate, _) -> Ir.Retained_function.same candidate link)
           !code_owners)
    then code_owners := (link, source.source_definition) :: !code_owners
  in
  List.iter
    (fun owner ->
      let link = Global_storage.code_owner_link owner in
      match retained_function_source link with
      | Ok source -> own_source link (validate_resolved link source)
      | Error _ -> ())
    prior_code_owners;
  let add_slot_address binding =
    let module VM = Ir.Integer_interpreter in
    let receipt = VM.native_slot_address_binding_receipt binding in
    if
      not
        (VM.native_slot_address_binding_matches binding ~root_runtime_calls
           ~runtime_calls:(VM.native_slot_address_binding_runtime_calls binding)
           ~owner:(VM.native_slot_address_binding_owner binding)
           ~globals:(VM.native_slot_address_binding_globals binding)
           receipt)
    then reject "HCBACK0003" "native slot address has another source generation";
    slot_address_bindings := binding :: !slot_address_bindings;
    match VM.native_slot_address_binding_source binding with
    | Some (link, source) -> own_source link (validate_resolved link source)
    | None ->
        if
          Option.is_none (VM.native_slot_address_binding_local_owner binding)
          && Option.is_some (Runtime.function_slot_address_provider receipt)
          && not
               (List.mem
                  (Runtime.function_slot_address_provider receipt)
                  [
                    Some Runtime.Put_chars;
                    Some Runtime.Print;
                    Some Runtime.Stream_print;
                    Some Runtime.Stream_exe_print;
                  ])
        then
          reject "HCBACK0002"
            "native callback addresses for hosted output providers require \
             their own checked entries";
        Option.iter
          (fun body ->
            match
              List.find_opt
                (fun (definition : Ir.Integer_interpreter.function_definition)
                   -> definition.body == body)
                functions
            with
            | None ->
                reject "HCBACK0003"
                  "native own slot address has no original body"
            | Some definition -> (
                match
                  List.find_opt
                    (fun link ->
                      retained_link_matches_definition link definition)
                    (Ir.Integer_globals.function_publications globals)
                with
                | None ->
                    reject "HCBACK0003"
                      "native own slot address has no body publication"
                | Some link -> own_source link (current_source definition)))
          (VM.native_slot_address_binding_local_owner binding)
  in
  List.iter
    (fun slot ->
      match
        retained_slot_address_refresh
          (Global_storage.function_slot_binding slot)
      with
      | Ok binding -> add_slot_address binding
      | Error message -> reject "HCBACK0003" message)
    prior_function_slots;
  let source_for_call ~runtime_calls ~source_globals ~source_functions
      ~historical call =
    match Runtime.retained_function call with
    | Some link -> (
        match exact_local source_functions call with
        | Some definition when retained_link_matches_definition link definition
          ->
            {
              source_definition = definition;
              source_runtime_calls = runtime_calls;
              source_globals;
              source_functions;
              source_historical = historical;
            }
        | Some _ | None -> resolve_retained link)
    | None -> (
        match exact_local source_functions call with
        | Some definition ->
            {
              source_definition = definition;
              source_runtime_calls = runtime_calls;
              source_globals;
              source_functions;
              source_historical = historical;
            }
        | None ->
            reject "HCBACK0003"
              "native direct call has no exact source definition in its \
               original callable bundle")
  in
  let exact_self_call ~owner ~source_functions call =
    match owner with
    | Runtime.Entry -> None
    | Runtime.Function owner_body ->
        List.find_opt
          (fun (definition : Ir.Integer_interpreter.function_definition) ->
            definition.body == owner_body
            && callable_self_call_matches_definition ~runtime_owner:owner call
                 definition)
          source_functions
  in
  let scan ~runtime_calls ~source_globals ~source_functions ~historical ~owner
      graph =
    let slot_addresses =
      match Runtime.original_function_slot_addresses runtime_calls ~owner with
      | Some addresses -> addresses
      | None ->
          reject "HCBACK0003"
            "native task slot address graph lost its original producers"
    in
    let addresses =
      match Runtime.original_function_addresses runtime_calls ~owner with
      | Some addresses -> addresses
      | None ->
          reject "HCBACK0003"
            "native task function address context is not its original sealed \
             graph"
    in
    (match Runtime.original_callback_calls runtime_calls ~owner with
    | Some _ -> ()
    | None ->
        reject "HCBACK0003"
          "native task callback context is not its original sealed graph");
    List.iter
      (fun block ->
        Graph.instructions block |> Sequence.instructions
        |> List.iter (fun instruction ->
            let raw = Sequence.description instruction in
            Option.iter
              (fun receipt ->
                if Runtime.function_slot_address_cursor receipt == raw then
                  match
                    retained_slot_address_binding ~runtime_calls ~owner receipt
                  with
                  | Ok binding ->
                      if
                        not
                          (Ir.Integer_interpreter
                           .native_slot_address_binding_matches binding
                             ~root_runtime_calls ~runtime_calls ~owner
                             ~globals:source_globals receipt)
                      then
                        reject ?span:raw.span "HCBACK0003"
                          "native slot resolver returned another original \
                           address";
                      add_slot_address binding
                  | Error message -> reject ?span:raw.span "HCBACK0003" message)
              (Runtime.original_function_slot_address slot_addresses raw);
            (if historical then
               let source_storage =
                 match raw.payload with
                 | Some (Sequence.Symbol symbol) ->
                     Ir.Integer_globals.find_storage source_globals symbol
                 | Some (Sequence.Retained_global reference) ->
                     Ir.Integer_globals.retained_slot source_globals reference
                 | _ -> None
               in
               Option.iter
                 (fun storage ->
                   let original_static =
                     match
                       ( owner,
                         Ir.Integer_globals.find_static source_globals
                           (Ir.Integer_globals.storage_symbol storage) )
                     with
                     | Runtime.Function body, Some slot ->
                         Option.is_some
                           (Ir.Integer_globals.static_source_allocation slot)
                         && Function.definition_matches_frame body
                              (Ir.Integer_globals.static_frame slot)
                         && Ir.Integer_globals.same_task_storage globals
                              source_globals
                         && List.for_all
                              (Ir.Integer_globals.static_root_executed slot)
                              (Ir.Integer_globals.static_initializers slot)
                     | _ -> false
                   in
                   if
                     Option.is_some (Ir.Integer_globals.storage_frame storage)
                     && not original_static
                   then
                     reject ?span:raw.span "HCBACK0002"
                       "retained native task functions do not yet admit \
                        historical static storage")
                 source_storage);
            Option.iter
              (fun receipt ->
                let link = Runtime.function_address_link receipt in
                if
                  not
                    (List.exists
                       (fun owner ->
                         let original =
                           Global_storage.provider_code_owner_binding owner
                           |> Ir.Integer_interpreter
                              .native_slot_address_binding_receipt
                         in
                         Option.fold ~none:false
                           ~some:(Ir.Retained_function.same link)
                           (Runtime.function_slot_address_link original))
                       prior_provider_code_owners)
                then
                  let source =
                    match
                      List.find_opt
                        (retained_link_matches_definition link)
                        source_functions
                    with
                    | Some definition ->
                        {
                          source_definition = definition;
                          source_runtime_calls = runtime_calls;
                          source_globals;
                          source_functions;
                          source_historical = historical;
                        }
                    | None -> resolve_retained link
                  in
                  own_source link source)
              (Runtime.original_function_address addresses raw);
            if raw.opcode = Opcode.Ic_call_start then
              match
                Runtime.find_start runtime_calls ~owner raw.instruction_id
              with
              | None
                when Option.is_some
                       (Runtime.find_callback_start runtime_calls ~owner
                          raw.instruction_id)
                     || Option.is_some
                          (Runtime.find_intrinsic_start runtime_calls ~owner
                             raw.instruction_id) -> ()
              | None ->
                  reject ?span:raw.span "HCBACK0003"
                    "native task direct call is absent from its original \
                     sealed runtime context"
              | Some call -> (
                  let self_call =
                    Runtime.call_opcode call = Opcode.Ic_call_indirect2
                    && Option.is_some
                         (exact_self_call ~owner ~source_functions call)
                  in
                  if
                    Runtime.call_opcode call = Opcode.Ic_call_indirect2
                    && not self_call
                  then (
                    let binding =
                      match
                        retained_slot_binding ~runtime_calls ~owner call
                      with
                      | Ok binding -> binding
                      | Error message ->
                          reject ?span:raw.span "HCBACK0003" message
                    in
                    if
                      not
                        (Ir.Integer_interpreter.native_slot_binding_matches
                           binding ~root_runtime_calls ~runtime_calls ~owner
                           ~globals:source_globals call)
                    then
                      reject ?span:raw.span "HCBACK0003"
                        "native extern slot resolver returned another original \
                         call";
                    slot_bindings := binding :: !slot_bindings;
                    Option.iter
                      (fun (source :
                             Ir.Integer_interpreter.task_function_source) ->
                        if
                          not
                            (Ir.Integer_globals.same_task_storage globals
                               source.source_globals)
                        then
                          reject ?span:raw.span "HCBACK0003"
                            "native extern slot body belongs to another task";
                        let definition = source.source_definition in
                        if
                          (not
                             (Function.definition_matches_frame definition.body
                                definition.frame))
                          || not
                               (List.exists
                                  (fun (candidate :
                                         Ir.Integer_interpreter
                                         .function_definition) ->
                                    candidate.body == definition.body
                                    && candidate.frame == definition.frame)
                                  source.source_functions)
                        then
                          reject ?span:raw.span "HCBACK0003"
                            "native extern slot body has another original \
                             source bundle";
                        add
                          {
                            source_definition = definition;
                            source_runtime_calls = source.source_runtime_calls;
                            source_globals = source.source_globals;
                            source_functions = source.source_functions;
                            source_historical = true;
                          })
                      (Ir.Integer_interpreter.native_slot_binding_source binding))
                  else
                    match Runtime.provider call with
                    | Some _
                      when historical
                           && Runtime.provider call <> Some Runtime.Print
                           && Runtime.provider call <> Some Runtime.Put_chars
                           && Runtime.provider call <> Some Runtime.Stream_print
                           && Runtime.provider call
                              <> Some Runtime.Stream_exe_print ->
                        reject ?span:raw.span "HCBACK0002"
                          "retained native task functions currently require \
                           fixed direct integer or U0 calls"
                    | Some _ -> ()
                    | None ->
                        if
                          Runtime.call_opcode call <> Opcode.Ic_call
                          && not self_call
                        then
                          reject ?span:raw.span "HCBACK0002"
                            "retained native task function closure requires \
                             original direct or extern-slot calls";
                        if not self_call then
                          add
                            (source_for_call ~runtime_calls ~source_globals
                               ~source_functions ~historical call))))
      (Graph.blocks graph)
  in
  scan ~runtime_calls ~source_globals:globals ~source_functions:functions
    ~historical:false ~owner:Runtime.Entry (Ir.X87_stack.graph entry);
  while not (Queue.is_empty queue) do
    let source = Queue.take queue in
    let body = source.source_definition.body in
    scan ~runtime_calls:source.source_runtime_calls
      ~source_globals:source.source_globals
      ~source_functions:source.source_functions
      ~historical:source.source_historical ~owner:(Runtime.Function body)
      (Ir.X87_stack.graph (Function.x87 body))
  done;
  ( List.rev !collected_rev,
    List.rev !slot_bindings,
    List.rev !code_owners,
    List.rev !slot_address_bindings )

let validate_switch_code_floor ~max_code_bytes ~label block_groups =
  let used = ref 0 in
  let charge targets =
    let runs =
      match targets with
      | _ :: (_ :: _ as entries) ->
          fold_switch_runs (fun count _ _ _ -> count + 1) 0 entries
      | _ -> reject "HCBACK0003" "native IC_SWITCH has no bounded target table"
    in
    (* The generated bounded dispatch necessarily contains one out-of-range
       conditional branch, one final target jump, and one conditional branch
       for every contiguous destination run except the last. The scan does not
       allocate runs. Ignoring their arithmetic instructions gives a strict
       lower bound before allocating the machine instruction plan. *)
    let minimum = 11 + (6 * (runs - 1)) in
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
              | Indirect_call call ->
                  let count = List.length !(call.owned_targets) in
                  let available = max_code_bytes - !used in
                  if available < 5 || count > (available - 5) / 16 then
                    reject "HCBACK0005"
                      (label
                     ^ " callback dispatch exceeds max_code_bytes before \
                        allocation");
                  used := !used + 5 + (16 * count)
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
                let prepared_blocks, sites, preflight_ir_count, shared_values =
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
                      allocate_body ~status_abi:abi ~shared_values
                        ~max_stack_bytes
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
                        private_function_count = 0;
                        entry_stack_bytes = 8 + !frame_size;
                        global_bytes = 0;
                        literal_bytes = 0;
                        arena_metadata_bytes = 0;
                        global_image = "";
                        task_zero_bytes = None;
                        task_snapshot = None;
                        code_owner_bindings = [];
                        function_slot_bindings = [];
                        has_output = false;
                        sites;
                      }
              with Rejected error -> Error [ error ])))

let compile_callable_internal ?task_snapshot ?retained_parameter_default
    ?retained_callback_default ?(capture_callback_default = false)
    ?capture_data_default ?retained_function_source ?retained_slot_binding
    ?retained_slot_address_binding ?retained_slot_address_refresh ?status_abi
    ?(max_stack_bytes = hard_max_stack_bytes) ?(max_blocks = 4096)
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576)
    ?parameter_defaults ?global_initializers ~max_ir_instructions
    ~max_code_bytes ~runtime_calls ~initialization ~entry ~functions () =
  let globals = Ir.Global_initialization.globals initialization in
  let ( let* ) = Result.bind in
  let* () =
    match global_initializers with
    | Some proof
      when not
             (Driver.Native_global_initializers.matches proof ~runtime_calls
                ~initialization ~entry ~functions) ->
        Error
          [
            {
              code = "HCBACK0003";
              message =
                "native global preparation belongs to another callable bundle";
              span = None;
            };
          ]
    | _ -> Ok ()
  in
  let* () = validate_limits ~max_ir_instructions ~max_code_bytes in
  let* () = validate_stack_limit ~max_stack_bytes in
  let* () = validate_block_limit ~max_blocks in
  let* () =
    Literal_storage.validate_limit ~max_literal_bytes
    |> Result.map_error
         (List.map (fun (error : Literal_storage.error) ->
              { code = error.code; message = error.message; span = error.span }))
  in
  let entry_graph = Ir.X87_stack.graph entry in
  let current_function_bodies =
    List.map
      (fun (definition : Ir.Integer_interpreter.function_definition) ->
        definition.body)
      functions
  in
  let* ( callable_sources,
         slot_bindings,
         code_owner_sources,
         slot_address_bindings ) =
    match task_snapshot with
    | Some snapshot -> (
        if
          not
            (Runtime.matches runtime_calls ~entry
               ~initialization:(Some initialization)
               ~functions:current_function_bodies)
        then
          Error
            [
              {
                code = "HCBACK0003";
                message =
                  "native task callable entry disagrees with its sealed \
                   runtime-call context";
                span = None;
              };
            ]
        else
          match
            ( retained_function_source,
              retained_slot_binding,
              retained_slot_address_binding,
              retained_slot_address_refresh )
          with
          | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None ->
              Error
                [
                  {
                    code = "HCBACK0003";
                    message =
                      "native task callable compilation has no retained \
                       function source resolver";
                    span = None;
                  };
                ]
          | ( Some retained_function_source,
              Some retained_slot_binding,
              Some retained_slot_address_binding,
              Some retained_slot_address_refresh ) -> (
              try
                Ok
                  (collect_task_callable_sources ~max_ir_instructions
                     ~max_blocks ~globals ~runtime_calls ~entry ~functions
                     ~retained_function_source ~retained_slot_binding
                     ~retained_slot_address_binding
                     ~retained_slot_address_refresh
                     ~prior_function_slots:
                       (Global_storage.task_function_slots snapshot)
                     ~prior_code_owners:
                       (Global_storage.task_code_owners snapshot)
                     ~prior_provider_code_owners:
                       (Global_storage.task_provider_code_owners snapshot))
              with Rejected error -> Error [ error ]))
    | None ->
        Ok
          ( List.map
              (fun definition ->
                {
                  source_definition = definition;
                  source_runtime_calls = runtime_calls;
                  source_globals = globals;
                  source_functions = functions;
                  source_historical = false;
                })
              functions,
            [],
            [],
            [] )
  in
  let function_graphs =
    List.map
      (fun source -> Function.body source.source_definition.body)
      callable_sources
  in
  let graphs = entry_graph :: function_graphs in
  let* block_count, ir_count =
    try Ok (bounded_callable_counts ~max_ir_instructions ~max_blocks graphs)
    with Rejected error -> Error [ error ]
  in
  let* global_storage =
    (match (task_snapshot, global_initializers) with
      | Some snapshot, None
        when Option.is_none parameter_defaults
             && Global_storage.task_snapshot_matches snapshot ~initialization
                  ~entry -> Ok (Global_storage.task_snapshot_storage snapshot)
      | Some _, _ ->
          Error
            [
              {
                Global_storage.code = "HCBACK0003";
                message =
                  "native task fragment has another storage snapshot or \
                   callable bundle";
                span = None;
              };
            ]
      | None, None ->
          Global_storage.create ~functions ~max_global_bytes ~initialization
            ~entry
      | None, Some initializers ->
          Global_storage.create_prepared ~functions ~initializers
            ~max_global_bytes ~initialization ~entry)
    |> Result.map_error
         (List.map (fun (error : Global_storage.error) ->
              { code = error.code; message = error.message; span = error.span }))
  in
  let literal_errors =
    List.map (fun (error : Literal_storage.error) ->
        { code = error.code; message = error.message; span = error.span })
  in
  let* task_snapshot, global_storage, literal_storage =
    match task_snapshot with
    | None ->
        let* literals =
          Literal_storage.create ~max_literal_bytes ~max_arena_bytes:33_554_432
            ~arena_prefix_bytes:(Global_storage.arena_bytes global_storage)
            ~runtime_calls ~initialization ~entry ~functions
          |> Result.map_error literal_errors
        in
        Ok (None, global_storage, literals)
    | Some snapshot ->
        let* snapshot =
          Global_storage.append_task_code_owners snapshot code_owner_sources
          |> Result.map_error
               (List.map (fun (error : Global_storage.error) ->
                    {
                      code = error.code;
                      message = error.message;
                      span = error.span;
                    }))
        in
        let* snapshot =
          Global_storage.append_task_provider_code_owners snapshot
            slot_address_bindings
          |> Result.map_error
               (List.map (fun (error : Global_storage.error) ->
                    {
                      code = error.code;
                      message = error.message;
                      span = error.span;
                    }))
        in
        let* snapshot =
          Global_storage.append_task_function_slots snapshot
            slot_address_bindings
          |> Result.map_error
               (List.map (fun (error : Global_storage.error) ->
                    {
                      code = error.code;
                      message = error.message;
                      span = error.span;
                    }))
        in
        let candidates =
          (Runtime.Entry, entry_graph, runtime_calls)
          :: List.map
               (fun source ->
                 ( Runtime.Function source.source_definition.body,
                   Function.body source.source_definition.body,
                   source.source_runtime_calls ))
               callable_sources
        in
        let* sources, work =
          List.fold_left
            (fun result (owner, graph, runtime_calls) ->
              let* sources, work = result in
              let instructions =
                Graph.blocks graph
                |> List.concat_map (fun block ->
                    Graph.instructions block |> Sequence.instructions)
              in
              if
                not
                  (List.exists
                     (fun instruction ->
                       (Sequence.description instruction).opcode
                       = Opcode.Ic_str_const)
                     instructions)
              then Ok (sources, work)
              else
                let* source =
                  Literal_storage.source ~runtime_calls ~owner ~graph
                  |> Result.map_error literal_errors
                in
                Ok (source :: sources, work + List.length instructions))
            (Ok ([], 0))
            candidates
        in
        let* snapshot =
          Global_storage.append_task_literals snapshot
            ~sources:(List.rev sources) ~work
          |> Result.map_error
               (List.map (fun (error : Global_storage.error) ->
                    {
                      code = error.code;
                      message = error.message;
                      span = error.span;
                    }))
        in
        let* snapshot =
          match capture_data_default with
          | None -> Ok snapshot
          | Some data ->
              Global_storage.append_task_saved_data snapshot data
              |> Result.map_error
                   (List.map (fun (error : Global_storage.error) ->
                        {
                          code = error.code;
                          message = error.message;
                          span = error.span;
                        }))
        in
        Ok
          ( Some snapshot,
            Global_storage.task_snapshot_storage snapshot,
            Global_storage.task_snapshot_literals snapshot )
  in
  let* parameter_defaults =
    match
      (task_snapshot, retained_parameter_default, retained_callback_default)
    with
    | Some _, Some available, Some available_callback ->
        Defaults.create_task ~globals ~runtime_calls ~initialization ~entry
          ~functions
          ~sources:
            (List.map
               (fun (source : callable_function_source) ->
                 ( source.source_globals,
                   source.source_definition,
                   source.source_runtime_calls ))
               callable_sources)
          ~available ~available_callback
        |> Result.map Option.some
        |> Result.map_error (fun message ->
            [ { code = "HCBACK0002"; message; span = None } ])
    | Some _, _, _ ->
        Error
          [
            {
              code = "HCBACK0002";
              message =
                "native task defaults require both original saved-default \
                 consumers";
              span = None;
            };
          ]
    | None, _, _ -> Ok parameter_defaults
  in
  let global_arena_bytes = Global_storage.arena_bytes global_storage in
  let global_image =
    if Option.is_some task_snapshot then ""
    else Global_storage.image global_storage
  in
  let has_storage =
    global_arena_bytes <> 0
    || (not (Global_storage.is_empty global_storage))
    || not (Literal_storage.is_empty literal_storage)
  in
  let entry_has_calls =
    Graph.blocks entry_graph
    |> List.exists (fun block ->
        Graph.instructions block |> Sequence.instructions
        |> List.exists (fun instruction ->
            (Sequence.description instruction).opcode = Opcode.Ic_call_start))
  in
  if callable_sources = [] && (not has_storage) && not entry_has_calls then
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
          |> Result.map (fun image -> { image with task_snapshot })
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
    try
      if
        not
          (Runtime.matches runtime_calls ~entry
             ~initialization:(Some initialization)
             ~functions:current_function_bodies)
      then
        reject "HCBACK0003"
          "native callable bundle disagrees with its sealed runtime-call \
           context";
      Option.iter
        (fun proof ->
          if
            not
              (Defaults.matches proof ~globals ~runtime_calls ~initialization
                 ~entry ~functions)
          then
            reject "HCBACK0003"
              "native parameter-default authority belongs to another callable \
               bundle")
        parameter_defaults;
      if block_count = 0 then
        reject "HCBACK0003" "native callable bundle requires an entry block";
      (* Bound private descriptor capacity from sealed call receipts. Runtime
         bounds always use each activation's captured count, including zero. *)
      let maximum_variadic_count = ref 0 in
      let collect_count = function
        | None -> ()
        | Some count
          when count >= 0L && count <= Int64.of_int (max_stack_bytes / 8) ->
            maximum_variadic_count :=
              max !maximum_variadic_count (Int64.to_int count)
        | Some _ ->
            reject "HCBACK0004" "native word-tail count exceeds max_stack_bytes"
      in
      let collect_graph runtime_calls owner graph =
        List.iter
          (fun block ->
            Sequence.instructions (Graph.instructions block)
            |> List.iter (fun instruction ->
                let id = (Sequence.description instruction).instruction_id in
                (match Runtime.find_start runtime_calls ~owner id with
                | Some call when Option.is_none (Runtime.provider call) ->
                    collect_count (Runtime.variadic_count call)
                | _ -> ());
                Option.iter
                  (fun callback ->
                    collect_count callback.Runtime.callback_variadic_count)
                  (Runtime.find_callback_start runtime_calls ~owner id)))
          (Graph.blocks graph)
      in
      collect_graph runtime_calls Runtime.Entry entry_graph;
      List.iter
        (fun source ->
          let definition = source.source_definition in
          collect_graph source.source_runtime_calls
            (Runtime.Function definition.body)
            (Ir.X87_stack.graph (Function.x87 definition.body)))
        callable_sources;
      let function_infos =
        callable_sources
        |> List.map
             (prepare_callable_function
                ~allow_runtime_layout:(Option.is_some task_snapshot)
                ~max_stack_bytes ~maximum_variadic_count:!maximum_variadic_count)
        |> Array.of_list
      in
      let function_code_owners =
        let source_owners =
          Array.map
            (fun info ->
              Option.bind task_snapshot (fun snapshot ->
                  List.find_opt
                    (fun owner ->
                      (Global_storage.code_owner_definition owner).body
                      == info.definition.body)
                    (Global_storage.task_code_owners snapshot)
                  |> Option.map (fun owner ->
                      {
                        callable_owner_id = Global_storage.code_owner_id owner;
                        callable_owner_address =
                          Global_storage.code_owner_address owner;
                        callable_owner_target =
                          Global_storage.code_owner_target owner;
                      })))
            function_infos
        in
        let provider_owners =
          Option.fold ~none:[] ~some:Global_storage.task_provider_code_owners
            task_snapshot
        in
        Array.append source_owners
          (Array.of_list
             (List.map
                (fun owner ->
                  Some
                    {
                      callable_owner_id =
                        Global_storage.provider_code_owner_id owner;
                      callable_owner_address =
                        Global_storage.provider_code_owner_address owner;
                      callable_owner_target =
                        Global_storage.provider_code_owner_target owner;
                    })
                provider_owners))
      in
      let provider_entries =
        Option.fold ~none:[] ~some:Global_storage.task_provider_code_owners
          task_snapshot
        |> List.map (fun owner ->
            Global_storage.provider_code_owner_binding owner
            |> Ir.Integer_interpreter.native_slot_address_binding_receipt)
        |> Array.of_list
      in
      let undefined_code_owner =
        Option.bind task_snapshot Global_storage.task_undefined_code_owner
      in
      let private_function_count =
        Array.length provider_entries
        + if Option.is_some undefined_code_owner then 1 else 0
      in
      let task_owned_targets =
        Array.to_list
          (Array.mapi
             (fun index owner -> Option.map (fun _ -> index) owner)
             function_code_owners)
        |> List.filter_map Fun.id
      in
      let next_site = ref 0 in
      let code_edges = ref [] in
      let indirect_code_edges = ref [] in
      let arena_code_cells = ref Int_map.empty in
      let entry_prepared =
        preflight_callable_graph ~runtime_calls ~source_globals:globals
          ~allow_retained_functions:(Option.is_some task_snapshot)
          ~task_dynamic_code_words:(Option.is_some task_snapshot)
          ~task_owned_targets ~capture_callback_default ~capture_data_default
          ~slot_root_runtime_calls:runtime_calls ~slot_bindings ~task_snapshot
          ~parameter_defaults ~code_edges ~indirect_code_edges ~arena_code_cells
          ~functions:function_infos ~provider_entries ~global_storage
          ~literal_storage ~runtime_owner:Runtime.Entry ~owner:Entry_owner
          ~frame_slots:Int_map.empty ~variadic:None ~expected_return:None
          ~is_entry:true ~rbp_bytes:0 ~max_stack_bytes ~next_site entry_graph
      in
      let function_prepared =
        Array.map
          (fun info ->
            let body = info.definition.body in
            preflight_callable_graph ~runtime_calls:info.runtime_calls
              ~source_globals:info.source_globals
              ~allow_retained_functions:(Option.is_some task_snapshot)
              ~task_dynamic_code_words:(Option.is_some task_snapshot)
              ~task_owned_targets ~capture_callback_default:false
              ~capture_data_default:None ~slot_root_runtime_calls:runtime_calls
              ~slot_bindings ~task_snapshot ~parameter_defaults ~code_edges
              ~indirect_code_edges ~arena_code_cells ~functions:function_infos
              ~provider_entries ~global_storage ~literal_storage
              ~runtime_owner:(Runtime.Function body) ~owner:info.owner
              ~frame_slots:info.frame_slots ~variadic:info.variadic
              ~expected_return:(Some (Function.return_type body))
              ~is_entry:false ~rbp_bytes:info.rbp_bytes ~max_stack_bytes
              ~next_site
              (Ir.X87_stack.graph (Function.x87 body)))
          function_infos
      in
      validate_callable_parameter_defaults ~parameter_defaults function_infos;
      if
        Option.is_none parameter_defaults
        && Defaults.requires_callback_proof ~globals ~functions
      then
        reject "HCBACK0002"
          "native callback declarations require original default preparation \
           authority";
      (* Resolve copies and fixed-parameter transfers across the entire original
         bundle before dispatch budgeting or machine allocation. An indirect
         transfer reaches a parameter cell only when its captured callee can own
         that original body. Cycles retain only original address producers. *)
      let changed = ref true in
      while !changed do
        changed := false;
        List.iter
          (fun (cell, source) ->
            let merged = List.sort_uniq Int.compare (!cell @ !source) in
            if merged <> !cell then (
              cell := merged;
              changed := true))
          !code_edges;
        List.iter
          (fun (targets, callee_index, cell, source) ->
            if List.mem callee_index !targets then
              let merged = List.sort_uniq Int.compare (!cell @ !source) in
              if merged <> !cell then (
                cell := merged;
                changed := true))
          !indirect_code_edges
      done;
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
        reject "HCBACK0003" "native callable preflight counts are inconsistent";
      validate_switch_code_floor ~max_code_bytes
        ~label:"native callable program"
        (entry_prepared.callable_blocks
        :: Array.to_list
             (Array.map (fun body -> body.callable_blocks) function_prepared));
      let sites =
        entry_prepared.callable_sites
        @ (Array.to_list function_prepared
          |> List.concat_map (fun body -> body.callable_sites))
      in
      let abi = Option.value status_abi ~default:(default_status_abi ()) in
      let supply = make_label_supply () in
      let function_labels =
        Array.init
          (Array.length function_infos + Array.length provider_entries)
          (fun _ -> fresh_label supply)
      in
      let allocate_graph ~graph ~prepared ~rbp_bytes ~init_flag_offsets
          ~parameter_owner_offsets ~variadic ~is_entry ~start_label =
        let rbp_bytes = rbp_bytes + prepared.callable_reference_bytes in
        let fixed_stack_slots =
          prepared.callable_home_slots + prepared.callable_stage_slots
        in
        let base_bytes = rbp_bytes + (fixed_stack_slots * 8) in
        let base_frame = if base_bytes = 0 then 0 else align_up base_bytes 16 in
        if base_frame > max_stack_bytes || base_frame > 4080 then
          reject "HCBACK0004"
            (Printf.sprintf
               "native callable fixed frame requires %d bytes, exceeding \
                max_stack_bytes (%d)"
               base_frame max_stack_bytes);
        let block_labels =
          List.fold_left
            (fun labels block ->
              Block_map.add block.program_block_id (fresh_label supply) labels)
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
              match Block_map.find_opt block.program_block_id block_labels with
              | Some label -> label
              | None ->
                  reject "HCBACK0003"
                    "native callable block has no machine label"
            in
            let allocation =
              allocate_body ~status_abi:abi
                ~shared_values:prepared.callable_shared_values
                ~function_code_owners ?undefined_code_owner
                ~provider_entry_start:(Array.length function_infos)
                ~callable_frame:{ rbp_bytes; fixed_stack_slots }
                ~max_stack_bytes
                ~reserved_registers:
                  (if has_storage then [ Encoder.R9; Encoder.R10; Encoder.R11 ]
                   else [ Encoder.R10; Encoder.R11 ])
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
                "native callable frame exceeds the single guard-page-safe \
                 allocation bound";
            frame_size := max !frame_size allocation.frame_size;
            peak := max !peak allocation.peak;
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
                          "native callable fallthrough target has no machine \
                           label"
                  in
                  block_plan @ [ Planned_branch (Unconditional, target_label) ]
            in
            block_plan_rev := List.rev_append block_plan !block_plan_rev)
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
                [ Planned_instruction (Encoder.Alloc_call_frame frame) ])
          @ (if is_entry then
               [
                 Planned_instruction (Encoder.Capture_status abi);
                 Planned_instruction (Encoder.Load_context (Encoder.R10, 16));
               ]
             else [])
          @ (if is_entry && has_storage then
               [ Planned_instruction (Encoder.Load_context (Encoder.R9, 72)) ]
             else [])
          @ List.concat_map
              (fun flag_offset ->
                [
                  Planned_instruction (Encoder.Mov_imm64 (Encoder.Rax, 0L));
                  Planned_instruction
                    (Encoder.Store_frame
                       (encoder_frame_slot None flag_offset, Encoder.Rax));
                ])
              init_flag_offsets
          @ (match variadic with
            | None -> []
            | Some (origin, _) ->
                [
                  Planned_instruction
                    (Encoder.Load_frame
                       ( Encoder.Rax,
                         encoder_frame_slot None (origin.data_offset - 8) ));
                  Planned_instruction
                    (Encoder.Store_frame
                       (encoder_frame_slot None origin.count_offset, Encoder.Rax));
                ])
          @ List.concat_map
              (fun (incoming_offset, owner_offset) ->
                (match variadic with
                  | None ->
                      [
                        Planned_instruction
                          (Encoder.Load_frame
                             ( Encoder.Rax,
                               encoder_frame_slot None incoming_offset ));
                      ]
                  | Some (origin, _) ->
                      [
                        Planned_instruction
                          (Encoder.Load_frame
                             ( Encoder.Rax,
                               encoder_frame_slot None origin.count_offset ));
                        Planned_instruction
                          (Encoder.Binary (Encoder.Add, Encoder.Rax, Encoder.Rax));
                        Planned_instruction
                          (Encoder.Binary (Encoder.Add, Encoder.Rax, Encoder.Rax));
                        Planned_instruction
                          (Encoder.Binary (Encoder.Add, Encoder.Rax, Encoder.Rax));
                        Planned_instruction
                          (Encoder.Address_frame
                             ( Encoder.Rcx,
                               encoder_scalar_frame_slot None incoming_offset ));
                        Planned_instruction
                          (Encoder.Binary (Encoder.Add, Encoder.Rcx, Encoder.Rax));
                        Planned_instruction
                          (Encoder.Load_indirect (Encoder.Rax, Encoder.Rcx, 0));
                      ])
                @ [
                    Planned_instruction
                      (Encoder.Store_frame
                         (encoder_frame_slot None owner_offset, Encoder.Rax));
                  ])
              parameter_owner_offsets
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
          prefix @ [ Planned_branch (Unconditional, graph_entry_label) ]
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
                 Planned_instruction (Encoder.Load_context (Encoder.Rax, 16));
                 Planned_instruction
                   (Encoder.Binary (Encoder.Sub, Encoder.Rax, Encoder.R10));
                 Planned_instruction (Encoder.Store_context (24, Encoder.Rax));
               ]
             else [])
          @ (match frame with
            | None -> []
            | Some frame ->
                [ Planned_instruction (Encoder.Free_call_frame frame) ])
          @ [
              Planned_instruction Encoder.Pop_rbp;
              Planned_instruction Encoder.Ret;
            ]
        in
        {
          body_plan = prefix @ List.rev !block_plan_rev @ fault_plan @ suffix;
          body_frame_size = !frame_size;
          body_peak = !peak;
          body_unwind = build_callable_windows_unwind_info !frame_size;
        }
      in
      let entry_allocated =
        allocate_graph ~graph:entry_graph ~prepared:entry_prepared ~rbp_bytes:0
          ~init_flag_offsets:[] ~parameter_owner_offsets:[] ~is_entry:true
          ~variadic:None ~start_label:None
      in
      let functions_allocated =
        Array.mapi
          (fun index info ->
            let incoming_index =
              ref
                (Array.length info.parameter_types
                + if Option.is_some info.variadic then 1 else 0)
            in
            let parameter_owner_offsets =
              Array.to_list
                (Array.mapi
                   (fun parameter_index callback ->
                     Option.map
                       (fun _ ->
                         let incoming_offset = 16 + (8 * !incoming_index) in
                         incr incoming_index;
                         let slot =
                           Int_map.find
                             (16 + (8 * parameter_index))
                             info.frame_slots
                         in
                         (incoming_offset, Option.get slot.slot_owner_offset))
                       callback)
                   info.parameter_callbacks)
              |> List.filter_map Fun.id
            in
            allocate_graph
              ~graph:(Ir.X87_stack.graph (Function.x87 info.definition.body))
              ~prepared:function_prepared.(index) ~rbp_bytes:info.rbp_bytes
              ~init_flag_offsets:info.init_flag_offsets ~is_entry:false
              ~parameter_owner_offsets ~variadic:info.variadic
              ~start_label:(Some function_labels.(index)))
          function_infos
      in
      let provider_allocated =
        Array.mapi
          (fun index receipt ->
            if
              Runtime.function_slot_address_provider receipt |> function
              | Some
                  ( Runtime.Print
                  | Runtime.Stream_print
                  | Runtime.Stream_exe_print ) -> true
              | _ -> false
            then (
              let frame_size =
                align_up ((4 + Print_codegen.provider_scratch_slots) * 8) 16
              in
              if frame_size > max_stack_bytes || frame_size > 4080 then
                reject "HCBACK0004"
                  "native Print entry exceeds its private scratch frame";
              let frame = encoder_call_frame None frame_size in
              let complete = fresh_label supply in
              let faults = ref [] in
              let plan = ref [] in
              let emit instruction =
                plan := Planned_instruction instruction :: !plan
              in
              let mark label = plan := Planned_label label :: !plan in
              let branch kind label =
                plan := Planned_branch (kind, label) :: !plan
              in
              let slot index =
                Encoder.stack_slot ~offset:(index * 8) |> Result.get_ok
              in
              let fault kind =
                match List.assoc_opt kind !faults with
                | Some label -> label
                | None ->
                    let label = fresh_label supply in
                    faults := (kind, label) :: !faults;
                    label
              in
              mark function_labels.(Array.length function_infos + index);
              emit Encoder.Push_rbp;
              emit Encoder.Mov_rbp_rsp;
              emit (Encoder.Alloc_call_frame frame);
              (* RDX names this caller's original outgoing format/count/tail
                 table, R8 its checked kind tags, RCX the sealed tail count. *)
              emit (Encoder.Store_stack (slot 1, Encoder.Rcx));
              emit (Encoder.Store_stack (slot 3, Encoder.R8));
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, 8));
              emit (Encoder.Cmp (Encoder.Rax, Encoder.Rcx));
              branch Not_equal (fault 14);
              emit (Encoder.Load_indirect (Encoder.Rax, Encoder.Rdx, 0));
              emit (Encoder.Store_stack (slot 0, Encoder.Rax));
              emit (Encoder.Mov_imm64 (Encoder.Rax, 16L));
              emit (Encoder.Binary (Encoder.Add, Encoder.Rdx, Encoder.Rax));
              emit (Encoder.Store_stack (slot 2, Encoder.Rdx));
              Print_codegen.emit_provider
                {
                  status_abi = abi;
                  instruction = emit;
                  fresh = (fun () -> fresh_label supply);
                  mark;
                  branch =
                    (fun kind label ->
                      branch
                        (match kind with
                        | Print_codegen.Always -> Unconditional
                        | Print_codegen.Equal -> Equal
                        | Print_codegen.Not_equal -> Not_equal
                        | Print_codegen.Below -> Below
                        | Print_codegen.Less -> Less
                        | Print_codegen.Overflow -> Overflow)
                        label);
                  fault;
                  slot;
                }
                {
                  Print_codegen.target =
                    (match Runtime.function_slot_address_provider receipt with
                    | Some Runtime.Print -> Print_codegen.Task_output
                    | Some Runtime.Stream_print -> Print_codegen.Generation
                    | Some Runtime.Stream_exe_print ->
                        Print_codegen.Formatted_source
                    | _ -> assert false);
                  format_stage = 0;
                  count_stage = 1;
                  arguments_stage = 2;
                  kinds_stage = 3;
                  scratch_stage = 4;
                };
              branch Unconditional complete;
              List.iter
                (fun (kind, label) ->
                  mark label;
                  emit (Encoder.Store_context_imm (0, kind));
                  branch Unconditional complete)
                !faults;
              mark complete;
              emit (Encoder.Free_call_frame frame);
              emit Encoder.Pop_rbp;
              emit Encoder.Ret;
              {
                body_plan = List.rev !plan;
                body_frame_size = frame_size;
                body_peak = 4;
                body_unwind = build_callable_windows_unwind_info frame_size;
              })
            else
              let loop = fresh_label supply in
              let shift = fresh_label supply in
              let complete = fresh_label supply in
              let output_fault = fresh_label supply in
              let work_fault = fresh_label supply in
              let plan = ref [] in
              let emit instruction =
                plan := Planned_instruction instruction :: !plan
              in
              let mark label = plan := Planned_label label :: !plan in
              let branch kind label =
                plan := Planned_branch (kind, label) :: !plan
              in
              let charge () =
                emit (Encoder.Load_context (Encoder.Rcx, 96));
                emit (Encoder.Test Encoder.Rcx);
                branch Equal work_fault;
                emit (Encoder.Dec Encoder.Rcx);
                emit (Encoder.Store_context (96, Encoder.Rcx))
              in
              mark function_labels.(Array.length function_infos + index);
              emit Encoder.Push_rbp;
              emit Encoder.Mov_rbp_rsp;
              emit
                (Encoder.Load_frame (Encoder.Rax, encoder_frame_slot None 16));
              mark loop;
              emit (Encoder.Test Encoder.Rax);
              branch Equal complete;
              charge ();
              emit (Encoder.Mov (Encoder.Rdx, Encoder.Rax));
              emit (Encoder.Mov_imm64 (Encoder.R8, 255L));
              emit (Encoder.Binary (Encoder.And, Encoder.Rdx, Encoder.R8));
              emit (Encoder.Test Encoder.Rdx);
              branch Equal shift;
              charge ();
              emit (Encoder.Load_context (Encoder.Rcx, 88));
              emit (Encoder.Test Encoder.Rcx);
              branch Equal output_fault;
              emit (Encoder.Dec Encoder.Rcx);
              emit (Encoder.Store_context (88, Encoder.Rcx));
              emit (Encoder.Load_context (Encoder.R8, 80));
              emit (Encoder.Load_context (Encoder.Rcx, 104));
              emit (Encoder.Binary (Encoder.Add, Encoder.R8, Encoder.Rcx));
              emit
                (Encoder.Store_indirect_narrow
                   (Encoder.R8, Encoder.Frame8, Encoder.Rdx));
              emit (Encoder.Mov_imm64 (Encoder.Rdx, 1L));
              emit (Encoder.Binary (Encoder.Add, Encoder.Rcx, Encoder.Rdx));
              emit (Encoder.Store_context (104, Encoder.Rcx));
              mark shift;
              emit (Encoder.Mov_imm64 (Encoder.Rcx, 8L));
              emit (Encoder.Shift_cl (Encoder.Shr, Encoder.Rax));
              branch Unconditional loop;
              mark output_fault;
              emit (Encoder.Store_context_imm (0, 11));
              branch Unconditional complete;
              mark work_fault;
              emit (Encoder.Store_context_imm (0, 12));
              mark complete;
              emit Encoder.Pop_rbp;
              emit Encoder.Ret;
              {
                body_plan = List.rev !plan;
                body_frame_size = 0;
                body_peak = 4;
                body_unwind = build_callable_windows_unwind_info 0;
              })
          provider_entries
      in
      let functions_allocated =
        Array.append functions_allocated provider_allocated
      in
      let callee_stack_bytes =
        Array.map (fun body -> 16 + body.body_frame_size) functions_allocated
      in
      let plan =
        entry_allocated.body_plan
        @ (Array.to_list functions_allocated
          |> List.concat_map (fun body -> body.body_plan))
      in
      let label_offsets, planned_code_size = plan_label_offsets plan in
      let instructions, code_size, machine_count =
        resolve_plan ~callee_labels:function_labels ~callee_stack_bytes plan
      in
      if code_size <> planned_code_size then
        reject "HCBACK0003"
          "native callable label accounting disagrees with plan resolution";
      if code_size > max_code_bytes then
        reject "HCBACK0005" "native callable program exceeds max_code_bytes";
      let function_starts =
        Array.map
          (fun label ->
            match Hashtbl.find_opt label_offsets label with
            | Some offset -> offset
            | None ->
                reject "HCBACK0003"
                  "native callable function has no resolved start offset")
          function_labels
      in
      let code_owner_bindings =
        Array.to_list
          (Array.mapi
             (fun index owner ->
               Option.map
                 (fun owner ->
                   ( owner.callable_owner_id,
                     owner.callable_owner_address,
                     owner.callable_owner_target,
                     index + 1 ))
                 owner)
             function_code_owners)
        |> List.filter_map Fun.id
        |> fun bindings ->
        (match undefined_code_owner with
          | None -> bindings
          | Some owner ->
              ( Global_storage.undefined_code_owner_id owner,
                Global_storage.undefined_code_owner_address owner,
                Global_storage.undefined_code_owner_target owner,
                Array.length function_infos + Array.length provider_entries + 1
              )
              :: bindings)
        |> List.sort (fun (left, _, _, _) (right, _, _, _) ->
            Int.compare left right)
        |> List.mapi (fun leaf (id, address, target, body) ->
            ( id,
              address,
              target,
              body,
              Array.length function_infos + 1 + private_function_count + leaf ))
      in
      let function_slot_bindings =
        match task_snapshot with
        | None -> []
        | Some snapshot ->
            List.map
              (fun slot ->
                let module VM = Ir.Integer_interpreter in
                let original =
                  VM.native_slot_address_binding_receipt
                    (Global_storage.function_slot_binding slot)
                in
                let symbol receipt =
                  Sema.Function_resolution.resolved_declaration_identity_symbol
                    (Runtime.function_slot_address_declaration receipt)
                in
                let body binding =
                  match VM.native_slot_address_binding_source binding with
                  | Some (_, source) -> Some source.source_definition.body
                  | None -> VM.native_slot_address_binding_local_owner binding
                in
                let selected =
                  List.find_map
                    (fun binding ->
                      if
                        symbol (VM.native_slot_address_binding_receipt binding)
                        == symbol original
                      then body binding
                      else None)
                    slot_address_bindings
                in
                let id =
                  match selected with
                  | Some body -> (
                      match
                        List.find_opt
                          (fun owner ->
                            (Global_storage.code_owner_definition owner).body
                            == body)
                          (Global_storage.task_code_owners snapshot)
                      with
                      | Some owner -> Global_storage.code_owner_id owner
                      | None ->
                          reject "HCBACK0003"
                            "native slot body lacks its original executable \
                             owner")
                  | None -> (
                      match
                        List.find_opt
                          (fun owner ->
                            let receipt =
                              Global_storage.provider_code_owner_binding owner
                              |> VM.native_slot_address_binding_receipt
                            in
                            Runtime.function_slot_address_declaration receipt
                            == Runtime.function_slot_address_declaration
                                 original)
                          (Global_storage.task_provider_code_owners snapshot)
                      with
                      | Some owner ->
                          Global_storage.provider_code_owner_id owner
                      | None ->
                          Global_storage.undefined_code_owner_id
                            (Option.get undefined_code_owner))
                in
                (Global_storage.function_slot_address slot, id))
              (Global_storage.task_function_slots snapshot)
            |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
      in
      let undefined_instructions =
        match undefined_code_owner with
        | None -> []
        | Some _ ->
            [
              Encoder.Push_rbp;
              Encoder.Mov_rbp_rsp;
              Encoder.Store_context_imm (0, 23);
              Encoder.Pop_rbp;
              Encoder.Ret;
            ]
      in
      let undefined_bytes =
        match Encoder.encode_all ~max_code_bytes undefined_instructions with
        | Ok code -> Bytes.of_string code
        | Error message -> reject "HCBACK0005" message
      in
      let leaf_start = code_size + Bytes.length undefined_bytes in
      if
        Array.length function_infos
        + private_function_count
        + List.length code_owner_bindings
        >= 100_000
      then
        reject "HCBACK0001"
          "native source bodies and private entries exceed the unwind table \
           bound";
      let leaf_bytes = Bytes.create (8 * List.length code_owner_bindings) in
      List.iteri
        (fun index (_, _, target, _, _) ->
          let offset = index * 8 in
          Bytes.set leaf_bytes offset '\x90';
          Bytes.set leaf_bytes (offset + 1) '\x41';
          Bytes.set leaf_bytes (offset + 2) '\xff';
          Bytes.set leaf_bytes (offset + 3) '\xa1';
          Bytes.set_int32_le leaf_bytes (offset + 4) (Int32.of_int target))
        code_owner_bindings;
      if
        leaf_start > max_code_bytes
        || Bytes.length leaf_bytes > max_code_bytes - leaf_start
      then reject "HCBACK0005" "native stable entries exceed max_code_bytes";
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
                 (begin_offset, end_offset, Bytes.copy body.body_unwind))
               functions_allocated)
        in
        let leaves =
          List.mapi
            (fun index _ ->
              ( leaf_start + (8 * index),
                leaf_start + (8 * (index + 1)),
                Bytes.of_string "\001\000\000\000" ))
            code_owner_bindings
        in
        let private_entries =
          match undefined_code_owner with
          | None -> []
          | Some _ ->
              [ (code_size, leaf_start, build_callable_windows_unwind_info 0) ]
        in
        (entry_record :: named) @ private_entries @ leaves
      in
      match Encoder.encode_all ~max_code_bytes instructions with
      | Error message -> reject "HCBACK0005" message
      | Ok encoded ->
          if String.length encoded <> code_size then
            reject "HCBACK0003"
              "encoded length does not match the native callable plan";
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
              encoded =
                Bytes.concat Bytes.empty
                  [ Bytes.of_string encoded; undefined_bytes; leaf_bytes ];
              ir_count;
              machine_count =
                machine_count
                + List.length undefined_instructions
                + List.length code_owner_bindings;
              peak = max (if has_storage then 6 else 5) peak;
              frame_size;
              unwind_info = Bytes.copy entry_allocated.body_unwind;
              unwind_functions;
              status_abi = abi;
              block_count;
              function_count = Array.length function_infos;
              private_function_count;
              entry_stack_bytes = 16 + entry_allocated.body_frame_size;
              global_bytes = Global_storage.global_bytes global_storage;
              literal_bytes =
                Option.fold
                  ~none:(Literal_storage.literal_bytes literal_storage)
                  ~some:Global_storage.task_snapshot_literal_bytes task_snapshot;
              arena_metadata_bytes =
                (global_arena_bytes
                - Global_storage.global_bytes global_storage
                +
                if Option.is_some task_snapshot then
                  -Option.fold
                     ~none:(Literal_storage.literal_bytes literal_storage)
                     ~some:Global_storage.task_snapshot_literal_bytes
                     task_snapshot
                else Literal_storage.metadata_bytes literal_storage);
              global_image =
                (if Option.is_some task_snapshot then ""
                 else global_image ^ Literal_storage.image literal_storage);
              task_zero_bytes =
                Option.map (fun _ -> global_arena_bytes) task_snapshot;
              task_snapshot;
              code_owner_bindings;
              function_slot_bindings;
              has_output = List.exists (fun site -> site.output_site) sites;
              sites;
            }
    with Rejected error -> Error [ error ]

let compile_callable ?status_abi ?max_stack_bytes ?max_blocks ?max_global_bytes
    ?max_literal_bytes ?parameter_defaults ?global_initializers
    ~max_ir_instructions ~max_code_bytes ~runtime_calls ~initialization ~entry
    ~functions () =
  compile_callable_internal ?status_abi ?max_stack_bytes ?max_blocks
    ?max_global_bytes ?max_literal_bytes ?parameter_defaults
    ?global_initializers ~max_ir_instructions ~max_code_bytes ~runtime_calls
    ~initialization ~entry ~functions ()

let compile_task_fragment ?status_abi ?max_stack_bytes ?max_blocks
    ?capture_callback_default ?capture_data_default ~task_snapshot
    ~max_ir_instructions ~max_code_bytes ~runtime_calls
    ~retained_function_source ~retained_slot_binding ~retained_parameter_default
    ~retained_callback_default ~retained_slot_address_binding
    ~retained_slot_address_refresh ~initialization ~entry ~functions () =
  compile_callable_internal ~task_snapshot ~retained_parameter_default
    ~retained_callback_default ?capture_callback_default ?capture_data_default
    ~retained_function_source ~retained_slot_binding
    ~retained_slot_address_binding ~retained_slot_address_refresh ?status_abi
    ?max_stack_bytes ?max_blocks
    ~max_global_bytes:Global_storage.hard_max_global_bytes ~max_ir_instructions
    ~max_code_bytes ~runtime_calls ~initialization ~entry ~functions ()

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

let program_code_bytes (compiled : program_image) =
  Bytes.length compiled.encoded

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
let program_has_output (compiled : program_image) = compiled.has_output
let program_global_bytes (compiled : program_image) = compiled.global_bytes
let program_literal_bytes (compiled : program_image) = compiled.literal_bytes

let program_arena_metadata_bytes (compiled : program_image) =
  compiled.arena_metadata_bytes

let program_arena_bytes (compiled : program_image) =
  match compiled.task_zero_bytes with
  | Some count -> count
  | None -> String.length compiled.global_image

let program_global_image (compiled : program_image) =
  match compiled.task_zero_bytes with
  | Some count -> String.make count '\000'
  | None -> Bytes.to_string (Bytes.of_string compiled.global_image)

let program_task_snapshot (compiled : program_image) = compiled.task_snapshot

let program_code_owner_bindings (compiled : program_image) =
  compiled.code_owner_bindings

let program_private_function_count (compiled : program_image) =
  compiled.private_function_count

let program_function_slot_bindings (compiled : program_image) =
  compiled.function_slot_bindings
