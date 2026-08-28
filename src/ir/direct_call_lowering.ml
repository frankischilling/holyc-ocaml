module Sequence = Instruction_sequence
module Result = Sema.Function_call_expression_result
module Resolution = Sema.Function_call_resolution
module Target = Sema.Function_call_target_classification
module Records = Sema.Function_record_classification

type t = {
  sequence_ : Sequence.t;
  result_value_ : Sequence.Value_id.t;
  result_type_ : Sema.Type.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_call

let reference_commit = Opcode.reference_commit

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error ?span message = lowering_error ?span "HCIRL0004" message

let span_of_origin = function
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      Error (metadata_error "direct call has no complete source location")

let allocate_instruction_ids ~span instruction_id =
  let first = Sequence.Instruction_id.to_int instruction_id in
  if first > Int.max_int - 4 then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate a direct call because the host integer range is \
          exhausted")
  else
    let make offset = Sequence.Instruction_id.of_int (first + offset) in
    match (make 1, make 2, make 3, make 4) with
    | Ok second, Ok third, Ok fourth, Ok next ->
        Ok (instruction_id, second, third, fourth, next)
    | Error error, _, _, _
    | _, Error error, _, _
    | _, _, Error error, _
    | _, _, _, Error error -> Error error

let next_value_id ~span value_id =
  let value = Sequence.Value_id.to_int value_id in
  if value = Int.max_int then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate a direct-call result because the host integer range \
          is exhausted")
  else Sequence.Value_id.of_int (value + 1)

let target_resolution target =
  target |> Target.source |> Result.direct_source
  |> Sema.Function_call_conversion_policy.direct_source

let matching_resolution target result =
  match Result.result_call_resolution result with
  | Some (Resolution.Direct_call direct) -> direct == target_resolution target
  | Some (Resolution.Indirect_call _ | Resolution.Deferred_call _) | None ->
      false

let zero_parameter_call target =
  let typed = Target.source target in
  let direct = target_resolution target in
  let header = Resolution.direct_active_header direct in
  let signature = Sema.Function_type_resolution.function_signature header in
  Resolution.direct_fixed_arguments direct = []
  && Int64.equal (Resolution.direct_variadic_count direct) 0L
  && Result.direct_fixed_results typed = []
  && Result.direct_variadic_results typed = []
  && Sema.Function_type_resolution.signature_parameters signature = []
  && Option.is_none
       (Sema.Function_type_resolution.function_variadic_bindings header)

let description ~instruction_id ~opcode ~target_type ~payload ~span ?result () =
  {
    Sequence.instruction_id;
    opcode;
    operands = [];
    result;
    target_type;
    payload;
    flags = 0L;
    span = Some span;
  }

let lower_supported ~span ~instruction_id ~value_id ~target result_type =
  match
    (allocate_instruction_ids ~span instruction_id, next_value_id ~span value_id)
  with
  | Error error, _ | _, Error error -> Error [ error ]
  | ( Ok (start_id, call_id, cleanup_id, end_id, next_instruction_id_),
      Ok next_value_id_ ) -> (
      let direct = target_resolution target in
      let symbol = Resolution.direct_target_symbol direct in
      let record = Target.record target in
      let cleanup =
        if
          Sema.Function_flag.caller_expects_callee_pop
            ~stored_mask:(Records.stored_flag_mask record)
        then Opcode.Ic_add_rsp1
        else Opcode.Ic_add_rsp
      in
      let symbol_payload = Some (Sequence.Symbol symbol) in
      let items =
        [
          description ~instruction_id:start_id ~opcode:Opcode.Ic_call_start
            ~target_type:None ~payload:symbol_payload ~span ();
          description ~instruction_id:call_id ~opcode:Opcode.Ic_call
            ~target_type:(Some result_type) ~payload:symbol_payload ~span ();
          description ~instruction_id:cleanup_id ~opcode:cleanup
            ~target_type:(Some result_type)
            ~payload:(Some (Sequence.Integer 0L)) ~span ();
          description ~instruction_id:end_id ~opcode:Opcode.Ic_call_end
            ~target_type:(Some result_type) ~payload:symbol_payload ~span
            ~result:{ Sequence.value_id } ();
        ]
      in
      match Sequence.create items with
      | Error errors -> Error errors
      | Ok sequence_ ->
          Ok
            (Lowered
               {
                 sequence_;
                 result_value_ = value_id;
                 result_type_ = result_type;
                 next_instruction_id_;
                 next_value_id_;
               }))

let lower ~instruction_id ~value_id ~target result =
  match span_of_origin (Result.result_origin result) with
  | Error error -> Error [ error ]
  | Ok span -> (
      if not (matching_resolution target result) then
        Error
          [
            metadata_error ~span
              "direct-call expression and target classification disagree";
          ]
      else if Target.call_access target <> Records.Direct_executable_call then
        Ok Unsupported_call
      else if not (zero_parameter_call target) then Ok Unsupported_call
      else
        match Result.result_type result with
        | None ->
            Error
              [ metadata_error ~span "direct call has no checked result type" ]
        | Some result_type ->
            lower_supported ~span ~instruction_id ~value_id ~target result_type)

let sequence lowered = lowered.sequence_
let result_value lowered = lowered.result_value_
let result_type lowered = lowered.result_type_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  Printf.sprintf
    "holyc-ir-direct-call-v1 reference=%s\n\
     result=%%v%d:%s next-instruction=%d next-value=%d\n\
     %s"
    reference_commit
    (Sequence.Value_id.to_int lowered.result_value_)
    (Sequence.type_name lowered.result_type_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Sequence.human_body lowered.sequence_)
