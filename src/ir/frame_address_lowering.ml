module Sequence = Instruction_sequence
module Result = Sema.Function_call_expression_result
module Source = Sema.Function_call_resolution
module Module_binding = Sema.Module_expression_binding
module Binding = Sema.Function_binding_index
module Frame = Sema.Function_frame_layout
module Type = Sema.Type

type t = {
  sequence_ : Sequence.t;
  result_value_ : Sequence.Value_id.t;
  result_type_ : Type.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_location

type checked_location = {
  slot : Frame.frame_slot;
  address_type : Type.t;
  span : Common.Span.t;
}

let reference_commit = Opcode.reference_commit

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error ?span message = lowering_error ?span "HCIRL0004" message

let result_span result =
  match Result.result_origin result with
  | Sema.Symbol.Source_location location -> Some location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ -> None

let complete_result_span result =
  match result_span result with
  | Some span -> Ok span
  | None ->
      Error
        (metadata_error "frame-bound identifier has no complete source location")

let location_kind_matches_binding location binding =
  match (Frame.location_kind location, Binding.binding_kind binding) with
  | Frame.Named_parameter, Binding.Named_parameter
  | Frame.Variadic_argc, Binding.Variadic_argc
  | Frame.Variadic_argv, Binding.Variadic_argv
  | Frame.Automatic_local, Binding.Automatic_local
  | Frame.Static_local, Binding.Static_local -> true
  | ( ( Frame.Named_parameter
      | Frame.Variadic_argc
      | Frame.Variadic_argv
      | Frame.Automatic_local
      | Frame.Static_local ),
      ( Binding.Named_parameter
      | Binding.Variadic_argc
      | Binding.Variadic_argv
      | Binding.Automatic_local
      | Binding.Static_local ) ) -> false

let checked_shape result identifier location =
  let rank = Source.bound_identifier_array_rank identifier in
  let dimensions = Frame.location_dimensions location in
  match
    ( Frame.location_declarator_shape location,
      Frame.location_value_shape location,
      Source.bound_identifier_shape identifier )
  with
  | Frame.Object, Frame.Scalar, Source.Object_value when rank = 0 -> (
      match Result.result_category result with
      | Result.Object_value | Result.Lvalue -> Ok ()
      | Result.Address_value
      | Result.Array_value
      | Result.Callback_value
      | Result.Function_value
      | Result.Offset_value
      | Result.Unavailable ->
          Error "scalar frame identifier has an inconsistent result category")
  | Frame.Object, Frame.Array, Source.Array_value
    when rank = List.length dimensions && rank > 0 -> (
      match Result.result_category result with
      | Result.Array_value -> Ok ()
      | Result.Object_value
      | Result.Address_value
      | Result.Callback_value
      | Result.Function_value
      | Result.Offset_value
      | Result.Lvalue
      | Result.Unavailable ->
          Error "array frame identifier has an inconsistent result category")
  | Frame.Function_pointer, Frame.Scalar, Source.Function_pointer_value
    when rank = 0 -> (
      match Result.result_category result with
      | Result.Callback_value -> Ok ()
      | Result.Object_value
      | Result.Address_value
      | Result.Array_value
      | Result.Function_value
      | Result.Offset_value
      | Result.Lvalue
      | Result.Unavailable ->
          Error "callback frame identifier has an inconsistent result category")
  | Frame.Function_pointer, Frame.Array, Source.Array_value
    when rank = List.length dimensions && rank > 0 -> (
      match Result.result_category result with
      | Result.Array_value -> Ok ()
      | Result.Object_value
      | Result.Address_value
      | Result.Callback_value
      | Result.Function_value
      | Result.Offset_value
      | Result.Lvalue
      | Result.Unavailable ->
          Error
            "callback-array frame identifier has an inconsistent result \
             category")
  | ( (Frame.Object | Frame.Function_pointer),
      (Frame.Scalar | Frame.Array),
      ( Source.Object_value
      | Source.Array_value
      | Source.Function_pointer_value
      | Source.Direct_function_value ) ) ->
      Error "bound identifier and frame location shapes disagree"

let checked_non_static_location ~span result identifier location location_type =
  match Frame.location_frame_slot location with
  | None ->
      Error (metadata_error ?span "non-static frame location has no frame slot")
  | Some slot -> (
      let displacement = Frame.frame_slot_displacement slot in
      let kind = Frame.location_kind location in
      if kind = Frame.Automatic_local && Int64.compare displacement 0L > 0 then
        Error
          (metadata_error ?span
             "automatic frame location has a positive displacement")
      else if
        kind <> Frame.Automatic_local && Int64.compare displacement 0L <= 0
      then
        Error
          (metadata_error ?span
             "parameter frame location has a nonpositive displacement")
      else if
        Source.bound_identifier_shape identifier = Source.Direct_function_value
      then Ok None
      else
        match checked_shape result identifier location with
        | Error message -> Error (metadata_error ?span message)
        | Ok () -> (
            if Frame.location_declarator_shape location = Frame.Function_pointer
            then Ok None
            else if
              kind = Frame.Named_parameter
              && Frame.location_value_shape location = Frame.Array
            then Ok None
            else
              match Type.pointer_to location_type with
              | Error _ -> Ok None
              | Ok address_type -> (
                  match complete_result_span result with
                  | Error _ as error -> error
                  | Ok span -> Ok (Some { slot; address_type; span }))))

let checked_local_location frame result identifier occurrence binding =
  let span = result_span result in
  match Frame.find_binding_location frame binding with
  | None ->
      Error
        (metadata_error ?span
           "bound identifier is absent from the supplied function frame")
  | Some location -> (
      let location_type = Frame.location_checked_type location in
      let identifier_type = Source.bound_identifier_type identifier in
      let binding_symbol = Binding.binding_symbol binding in
      if Frame.location_binding location != binding then
        Error
          (metadata_error ?span
             "frame location retains a different binding identity")
      else if Frame.location_symbol location != binding_symbol then
        Error
          (metadata_error ?span
             "frame location retains a different symbol identity")
      else if
        not
          (String.equal
             (Module_binding.occurrence_name occurrence)
             (Sema.Symbol.name binding_symbol))
      then
        Error
          (metadata_error ?span
             "bound occurrence and frame symbol names disagree")
      else if not (location_kind_matches_binding location binding) then
        Error
          (metadata_error ?span
             "frame location and bound identifier storage kinds disagree")
      else if not (Type.equal location_type identifier_type) then
        Error
          (metadata_error ?span
             "frame location and bound identifier types disagree")
      else
        match Result.result_type result with
        | None ->
            Error
              (metadata_error ?span
                 "bound identifier does not have a checked result type")
        | Some result_type when not (Type.equal result_type identifier_type) ->
            Error
              (metadata_error ?span
                 "bound identifier source and result types disagree")
        | Some _
          when Result.result_array_rank result
               <> Source.bound_identifier_array_rank identifier ->
            Error
              (metadata_error ?span
                 "bound identifier source and result array ranks disagree")
        | Some _
          when Option.is_some (Result.result_function_declaration result)
               || Option.is_some (Result.result_function_address_path result) ->
            Error
              (metadata_error ?span
                 "frame-bound object retains direct-function metadata")
        | Some _ -> (
            match Frame.location_kind location with
            | Frame.Static_local -> (
                match checked_shape result identifier location with
                | Ok () -> Ok None
                | Error message -> Error (metadata_error ?span message))
            | Frame.Named_parameter
            | Frame.Variadic_argc
            | Frame.Variadic_argv
            | Frame.Automatic_local ->
                checked_non_static_location ~span result identifier location
                  location_type))

let checked_location frame result =
  let source = Result.result_source result in
  match Source.argument_expression_kind source with
  | Source.Bound_identifier_expression identifier -> (
      let occurrence = Source.bound_identifier_occurrence identifier in
      let source_origin = Source.argument_expression_origin source in
      if
        Result.result_origin result <> source_origin
        || Module_binding.occurrence_origin occurrence <> source_origin
      then
        Error
          (metadata_error ?span:(result_span result)
             "bound identifier origins disagree across semantic results")
      else
        match Module_binding.occurrence_resolution occurrence with
        | Module_binding.Local_binding binding ->
            checked_local_location frame result identifier occurrence binding
        | Module_binding.Module_binding _ | Module_binding.Outer_candidate ->
            Ok None)
  | Source.Integer_literal _
  | Source.Float_literal _
  | Source.Character_literal _
  | Source.String_literal _
  | Source.Parenthesized_expression _
  | Source.Prefix_expression _
  | Source.Postfix_expression _
  | Source.Postfix_cast_expression _
  | Source.Binary_expression _
  | Source.Index_expression _
  | Source.Member_access_expression _
  | Source.Aggregate_offset_base_expression _
  | Source.Top_level_bound_identifier_expression _
  | Source.Sizeof_expression _
  | Source.Standalone_offset_expression _
  | Source.Defined_expression _
  | Source.Unresolved_expression _ -> Ok None

let allocate_instruction_ids ~span start =
  let current = Sequence.Instruction_id.to_int start in
  if current > Int.max_int - 3 then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate a frame address because the host integer range is \
          exhausted")
  else
    match
      ( Sequence.Instruction_id.of_int (current + 1),
        Sequence.Instruction_id.of_int (current + 2),
        Sequence.Instruction_id.of_int (current + 3) )
    with
    | Ok immediate, Ok add, Ok next -> Ok (start, immediate, add, next)
    | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error

let allocate_value_ids ~span start =
  let current = Sequence.Value_id.to_int start in
  if current > Int.max_int - 3 then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate a frame-address value because the host integer range \
          is exhausted")
  else
    match
      ( Sequence.Value_id.of_int (current + 1),
        Sequence.Value_id.of_int (current + 2),
        Sequence.Value_id.of_int (current + 3) )
    with
    | Ok displacement, Ok address, Ok next ->
        Ok (start, displacement, address, next)
    | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error

let description ~instruction_id ~opcode ~operands ~result_value ~target_type
    ~payload ~span =
  {
    Sequence.instruction_id;
    opcode;
    operands;
    result = Some { Sequence.value_id = result_value };
    target_type = Some target_type;
    payload;
    flags = 0L;
    span = Some span;
  }

let lower_supported ~instruction_id ~value_id checked =
  match
    ( allocate_instruction_ids ~span:checked.span instruction_id,
      allocate_value_ids ~span:checked.span value_id )
  with
  | Error error, _ | _, Error error -> Error [ error ]
  | ( Ok (rbp_id, immediate_id, add_id, next_instruction_id_),
      Ok (rbp_value, immediate_value, address_value, next_value_id_) ) -> (
      let displacement = Frame.frame_slot_displacement checked.slot in
      let descriptions =
        [
          description ~instruction_id:rbp_id ~opcode:Opcode.Ic_rbp ~operands:[]
            ~result_value:rbp_value ~target_type:checked.address_type
            ~payload:None ~span:checked.span;
          description ~instruction_id:immediate_id ~opcode:Opcode.Ic_imm_i64
            ~operands:[] ~result_value:immediate_value
            ~target_type:checked.address_type
            ~payload:(Some (Sequence.Integer displacement)) ~span:checked.span;
          description ~instruction_id:add_id ~opcode:Opcode.Ic_add
            ~operands:[ rbp_value; immediate_value ]
            ~result_value:address_value ~target_type:checked.address_type
            ~payload:None ~span:checked.span;
        ]
      in
      match Sequence.create descriptions with
      | Error errors -> Error errors
      | Ok sequence_ ->
          Ok
            (Lowered
               {
                 sequence_;
                 result_value_ = address_value;
                 result_type_ = checked.address_type;
                 next_instruction_id_;
                 next_value_id_;
               }))

let lower ~instruction_id ~value_id ~frame result =
  match checked_location frame result with
  | Error error -> Error [ error ]
  | Ok None -> Ok Unsupported_location
  | Ok (Some checked) -> lower_supported ~instruction_id ~value_id checked

let sequence lowered = lowered.sequence_
let result_value lowered = lowered.result_value_
let result_type lowered = lowered.result_type_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  Printf.sprintf
    "holyc-ir-frame-address-v1 reference=%s\n\
     address=%%v%d address-type=%s next-instruction=%d next-value=%d\n\
     %s"
    reference_commit
    (Sequence.Value_id.to_int lowered.result_value_)
    (Sequence.type_name lowered.result_type_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Sequence.human_body lowered.sequence_)
