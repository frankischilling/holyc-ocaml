module Sequence = Instruction_sequence
module Expression = Expression_lowering
module Result = Sema.Function_call_expression_result
module Resolution = Sema.Function_call_resolution
module Policy = Sema.Function_call_conversion_policy
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

type call_shape =
  | Provided_parameters of {
      arguments : Result.expression_result list;
      variadic_count_type : Sema.Type.t option;
    }
  | Unsupported_shape
  | Inconsistent_shape of string

type lowered_argument =
  | Argument_lowered of
      Sequence.description list
      * Sequence.Instruction_id.t
      * Sequence.Value_id.t
  | Unsupported_argument

let reference_commit = Opcode.reference_commit

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error ?span message = lowering_error ?span "HCIRL0004" message

let span_of_origin = function
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      Error (metadata_error "direct call has no complete source location")

let next_instruction_id ~span instruction_id =
  let current = Sequence.Instruction_id.to_int instruction_id in
  if current = Int.max_int then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate a direct call because the host integer range is \
          exhausted")
  else Sequence.Instruction_id.of_int (current + 1)

let allocate_tail_instruction_ids ~span call_id =
  let call = Sequence.Instruction_id.to_int call_id in
  if call > Int.max_int - 3 then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate a direct call because the host integer range is \
          exhausted")
  else
    let make offset = Sequence.Instruction_id.of_int (call + offset) in
    match (make 1, make 2, make 3) with
    | Ok cleanup, Ok end_, Ok next -> Ok (call_id, cleanup, end_, next)
    | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error

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

let call_shape target =
  let typed = Target.source target in
  let direct = target_resolution target in
  let header = Resolution.direct_active_header direct in
  let signature = Sema.Function_type_resolution.function_signature header in
  let fixed_arguments = Resolution.direct_fixed_arguments direct in
  let fixed_results = Result.direct_fixed_results typed in
  let parameters =
    Sema.Function_type_resolution.signature_parameters signature
  in
  let variadic_count = Resolution.direct_variadic_count direct in
  let variadic_results = Result.direct_variadic_results typed in
  let variadic_count_type =
    header |> Sema.Function_type_resolution.function_variadic_bindings
    |> Option.map (fun bindings ->
        bindings |> Sema.Function_type_resolution.variadic_argc
        |> Sema.Function_type_resolution.synthetic_binding_type)
  in
  if
    not
      (Int64.equal variadic_count (Int64.of_int (List.length variadic_results)))
  then
    Inconsistent_shape "direct-call variadic count and typed results disagree"
  else if Option.is_none variadic_count_type && variadic_results <> [] then
    Inconsistent_shape "nonvariadic direct call retains typed variadic results"
  else if variadic_results <> [] then Unsupported_shape
  else
    let rec provided rev fixed_arguments fixed_results parameters =
      match (fixed_arguments, fixed_results, parameters) with
      | [], [], [] ->
          Provided_parameters { arguments = List.rev rev; variadic_count_type }
      | source :: arguments, fixed :: results, _ :: parameters -> (
          let retained_source =
            fixed |> Result.fixed_source |> Policy.fixed_source
          in
          if retained_source != source then
            Inconsistent_shape
              "direct-call fixed argument and typed result disagree"
          else
            match Result.fixed_path fixed with
            | Result.Provided_result argument ->
                provided (argument :: rev) arguments results parameters
            | Result.Declared_default_result _ -> Unsupported_shape)
      | _ ->
          Inconsistent_shape
            "direct-call fixed arguments, typed results, and parameters \
             disagree"
    in
    provided [] fixed_arguments fixed_results parameters

let description ~instruction_id ~opcode ~target_type ~payload ~span ?result
    ?(flags = 0L) () =
  {
    Sequence.instruction_id;
    opcode;
    operands = [];
    result;
    target_type;
    payload;
    flags;
    span = Some span;
  }

let push_result_flag = 0x000002000L

let mark_argument_result ~span result_value descriptions =
  let matches = ref 0 in
  let descriptions =
    List.map
      (fun description ->
        match description.Sequence.result with
        | Some result when Sequence.Value_id.equal result.value_id result_value
          ->
            incr matches;
            {
              description with
              Sequence.flags = Int64.logor description.flags push_result_flag;
            }
        | Some _ | None -> description)
      descriptions
  in
  if !matches = 1 then Ok descriptions
  else
    Error
      (metadata_error ~span
         "direct-call argument lowering has no unique result producer")

let lower_arguments ~span ~instruction_id ~value_id arguments =
  let rec loop rev_descriptions instruction_id value_id = function
    | [] ->
        Ok
          (Argument_lowered (List.rev rev_descriptions, instruction_id, value_id))
    | argument :: rest -> (
        match
          Expression.lower_typed_result ~instruction_id ~value_id argument
        with
        | Error errors -> Error errors
        | Ok Expression.Unsupported_expression -> Ok Unsupported_argument
        | Ok (Expression.Lowered lowered) -> (
            let descriptions =
              lowered |> Expression.sequence |> Sequence.instructions
              |> List.map Sequence.description
            in
            match
              mark_argument_result ~span
                (Expression.result_value lowered)
                descriptions
            with
            | Error error -> Error [ error ]
            | Ok descriptions ->
                loop
                  (List.rev_append descriptions rev_descriptions)
                  (Expression.next_instruction_id lowered)
                  (Expression.next_value_id lowered)
                  rest))
  in
  loop [] instruction_id value_id arguments

let lower_variadic_count ~span ~instruction_id ~value_id = function
  | None -> Ok ([], instruction_id, value_id)
  | Some target_type -> (
      match
        (next_instruction_id ~span instruction_id, next_value_id ~span value_id)
      with
      | Error error, _ | _, Error error -> Error error
      | Ok next_instruction_id, Ok next_value_id ->
          Ok
            ( [
                description ~instruction_id ~opcode:Opcode.Ic_imm_i64
                  ~target_type:(Some target_type)
                  ~payload:(Some (Sequence.Integer 0L)) ~span
                  ~result:{ Sequence.value_id } ~flags:push_result_flag ();
              ],
              next_instruction_id,
              next_value_id ))

let lower_supported ~span ~instruction_id ~value_id ~target ~arguments
    ~variadic_count_type result_type =
  let start_id = instruction_id in
  match next_instruction_id ~span instruction_id with
  | Error error -> Error [ error ]
  | Ok argument_instruction_id -> (
      match
        lower_arguments ~span ~instruction_id:argument_instruction_id ~value_id
          arguments
      with
      | Error _ as error -> error
      | Ok Unsupported_argument -> Ok Unsupported_call
      | Ok
          (Argument_lowered
             (argument_descriptions, count_instruction_id, count_value_id)) -> (
          match
            lower_variadic_count ~span ~instruction_id:count_instruction_id
              ~value_id:count_value_id variadic_count_type
          with
          | Error error -> Error [ error ]
          | Ok (count_descriptions, call_id, call_result_value) -> (
              match
                ( allocate_tail_instruction_ids ~span call_id,
                  next_value_id ~span call_result_value )
              with
              | Error error, _ | _, Error error -> Error [ error ]
              | ( Ok (call_id, cleanup_id, end_id, next_instruction_id_),
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
                  let cleanup_bytes =
                    let argument_count = Int64.of_int (List.length arguments) in
                    let slot_count =
                      if Option.is_some variadic_count_type then
                        Int64.succ argument_count
                      else argument_count
                    in
                    Int64.mul slot_count 8L
                  in
                  let items =
                    [
                      description ~instruction_id:start_id
                        ~opcode:Opcode.Ic_call_start ~target_type:None
                        ~payload:symbol_payload ~span ();
                    ]
                    @ argument_descriptions @ count_descriptions
                    @ [
                        description ~instruction_id:call_id
                          ~opcode:Opcode.Ic_call ~target_type:(Some result_type)
                          ~payload:symbol_payload ~span ();
                        description ~instruction_id:cleanup_id ~opcode:cleanup
                          ~target_type:(Some result_type)
                          ~payload:(Some (Sequence.Integer cleanup_bytes)) ~span
                          ();
                        description ~instruction_id:end_id
                          ~opcode:Opcode.Ic_call_end
                          ~target_type:(Some result_type)
                          ~payload:symbol_payload ~span
                          ~result:{ Sequence.value_id = call_result_value }
                          ();
                      ]
                  in
                  match Sequence.create items with
                  | Error errors -> Error errors
                  | Ok sequence_ ->
                      Ok
                        (Lowered
                           {
                             sequence_;
                             result_value_ = call_result_value;
                             result_type_ = result_type;
                             next_instruction_id_;
                             next_value_id_;
                           })))))

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
      else
        match call_shape target with
        | Unsupported_shape -> Ok Unsupported_call
        | Inconsistent_shape message -> Error [ metadata_error ~span message ]
        | Provided_parameters { arguments; variadic_count_type } -> (
            match Result.result_type result with
            | None ->
                Error
                  [
                    metadata_error ~span
                      "direct call has no checked result type";
                  ]
            | Some result_type ->
                lower_supported ~span ~instruction_id ~value_id ~target
                  ~arguments ~variadic_count_type result_type))

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
