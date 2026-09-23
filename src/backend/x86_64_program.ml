module Codegen = X86_64_word_codegen

type word_type = X86_64_expression.word_type = I64 | U64
type status_abi = X86_64_encoder.status_abi = Windows_x64 | System_v_x64

type error = X86_64_expression.error = {
  code : string;
  message : string;
  span : Common.Span.t option;
}

type word = { type_ : word_type; bits : int64 }

type fault_kind =
  | Division_by_zero
  | Signed_division_overflow
  | Step_limit_exceeded
  | Call_depth_exceeded
  | Frame_limit_exceeded
  | Native_stack_limit_exceeded
  | Uninitialized_read
  | Index_scale_overflow
  | Index_addition_overflow
  | Address_out_of_bounds
  | Output_limit_exceeded
  | Output_work_limit_exceeded

type arithmetic_operation = X86_64_expression.arithmetic_operation =
  | Divide
  | Remainder

type fault = {
  kind : fault_kind;
  operation : arithmetic_operation option;
  block_id : int;
  instruction_id : int;
  position : int;
  global_position : int;
  span : Common.Span.t option;
  executed_steps : int;
  function_id : int option;
  function_name : string option;
}

type execution = { executed_steps : int; final_value : word option }
type outcome = Completed of execution | Fault of fault
type t = { image : Codegen.program_image }

let hard_max_stack_bytes = Codegen.hard_max_stack_bytes

let project_error (error : Codegen.error) : error =
  { code = error.code; message = error.message; span = error.span }

let project_errors = List.map project_error

let validate_limits ~max_ir_instructions ~max_code_bytes =
  Codegen.validate_limits ~max_ir_instructions ~max_code_bytes
  |> Result.map_error project_errors

let validate_stack_limit ~max_stack_bytes =
  Codegen.validate_stack_limit ~max_stack_bytes
  |> Result.map_error project_errors

let validate_block_limit ~max_blocks =
  Codegen.validate_block_limit ~max_blocks |> Result.map_error project_errors

let compile ?status_abi ?max_stack_bytes ?max_blocks ~max_ir_instructions
    ~max_code_bytes verified =
  Codegen.compile_program ?status_abi ?max_stack_bytes ?max_blocks
    ~max_ir_instructions ~max_code_bytes verified
  |> Result.map_error project_errors
  |> Result.map (fun image -> { image })

let compile_callable ?status_abi ?max_stack_bytes ?max_blocks ?max_global_bytes
    ?max_literal_bytes ?parameter_defaults ?global_initializers
    ~max_ir_instructions ~max_code_bytes ~runtime_calls ~initialization ~entry
    ~functions () =
  Codegen.compile_callable ?status_abi ?max_stack_bytes ?max_blocks
    ?max_global_bytes ?max_literal_bytes ?parameter_defaults
    ?global_initializers ~max_ir_instructions ~max_code_bytes ~runtime_calls
    ~initialization ~entry ~functions ()
  |> Result.map_error project_errors
  |> Result.map (fun image -> { image })

let code compiled = Codegen.program_code compiled.image
let code_bytes compiled = Codegen.program_code_bytes compiled.image

let windows_unwind_info compiled =
  Codegen.program_windows_unwind_info compiled.image

let windows_unwind_functions compiled =
  Codegen.program_windows_unwind_functions compiled.image

let status_abi compiled = Codegen.program_status_abi compiled.image
let ir_instructions compiled = Codegen.program_ir_instructions compiled.image

let machine_instructions compiled =
  Codegen.program_machine_instructions compiled.image

let register_peak compiled = Codegen.program_register_peak compiled.image
let frame_bytes compiled = Codegen.program_frame_bytes compiled.image
let block_count compiled = Codegen.program_block_count compiled.image
let function_count compiled = Codegen.program_function_count compiled.image
let has_output compiled = Codegen.program_has_output compiled.image

let entry_stack_bytes compiled =
  Codegen.program_entry_stack_bytes compiled.image

let project_word_type = function
  | Codegen.I64 -> I64
  | Codegen.U64 -> U64

let project_operation = function
  | Codegen.Divide -> Divide
  | Codegen.Remainder -> Remainder

let project_owner = function
  | Codegen.Entry_owner -> (None, None)
  | Codegen.Function_owner { function_id; function_name } ->
      (Some function_id, Some function_name)

let site_by_value (compiled : t) site =
  if Int64.compare site 1L < 0 || Int64.compare site 100_000L > 0 then
    Error "native program status site is outside the checked IR bound"
  else
    let site = Int64.to_int site in
    match
      List.find_opt
        (fun (candidate : Codegen.program_site) -> candidate.site = site)
        (Codegen.program_sites compiled.image)
    with
    | Some site -> Ok site
    | None -> Error "native program status names an unknown execution site"

let decode_runtime_status (compiled : t) ~max_steps ~kind ~site ~executed_steps
    ~value_site ~bits =
  if max_steps <= 0 then Error "native program max_steps must be positive"
  else
    let budget = Int64.of_int max_steps in
    if
      Int64.compare executed_steps 0L < 0
      || Int64.compare executed_steps budget > 0
    then Error "native program executed_steps is outside the supplied budget"
    else
      let executed_steps_int = Int64.to_int executed_steps in
      let final_value =
        if Int64.equal value_site 0L then
          if Int64.equal bits 0L then Ok None
          else Error "native program status has bits without a value site"
        else
          match site_by_value compiled value_site with
          | Error _ as error -> error
          | Ok candidate -> (
              match (candidate.owner, candidate.value_type) with
              | Codegen.Entry_owner, Some type_ ->
                  Ok (Some { type_ = project_word_type type_; bits })
              | Codegen.Entry_owner, None ->
                  Error
                    "native program value site does not identify an IC_END_EXP"
              | Codegen.Function_owner _, _ ->
                  Error "native program value site belongs to a source function"
              )
      in
      match final_value with
      | Error _ as error -> error
      | Ok final_value -> (
          if Int64.equal kind 0L then
            if not (Int64.equal site 0L) then
              Error "native program success status has a fault site"
            else if executed_steps_int < 1 then
              Error "native program success status has no executed instruction"
            else
              Ok
                (Completed { executed_steps = executed_steps_int; final_value })
          else if Int64.equal site 0L then
            Error "native program fault status has no execution site"
          else
            match site_by_value compiled site with
            | Error _ as error -> error
            | Ok candidate ->
                let make_fault kind operation =
                  let function_id, function_name =
                    project_owner candidate.owner
                  in
                  Ok
                    (Fault
                       {
                         kind;
                         operation;
                         block_id = candidate.block_id;
                         instruction_id = candidate.instruction_id;
                         position = candidate.position;
                         global_position = candidate.global_position;
                         span = candidate.span;
                         executed_steps = executed_steps_int;
                         function_id;
                         function_name;
                       })
                in
                if Int64.equal kind 1L then
                  if executed_steps_int < 1 then
                    Error
                      "native program arithmetic fault did not consume its \
                       instruction"
                  else
                    match candidate.arithmetic with
                    | Some (operation, _) ->
                        make_fault Division_by_zero
                          (Some (project_operation operation))
                    | None ->
                        Error
                          "native program zero-divisor status names a \
                           non-arithmetic site"
                else if Int64.equal kind 2L then
                  if executed_steps_int < 1 then
                    Error
                      "native program arithmetic fault did not consume its \
                       instruction"
                  else
                    match candidate.arithmetic with
                    | Some (operation, true) ->
                        make_fault Signed_division_overflow
                          (Some (project_operation operation))
                    | Some (_, false) ->
                        Error
                          "native program signed-overflow status names an \
                           unsigned site"
                    | None ->
                        Error
                          "native program signed-overflow status names a \
                           non-arithmetic site"
                else if Int64.equal kind 3L then
                  if not (Int64.equal executed_steps budget) then
                    Error
                      "native program step-limit status does not equal the \
                       supplied budget"
                  else make_fault Step_limit_exceeded None
                else if Int64.equal kind 4L then
                  if not candidate.call_site then
                    Error
                      "native program call-depth status names a non-call site"
                  else if executed_steps_int < 1 then
                    Error
                      "native program call-depth fault did not consume its \
                       IC_CALL"
                  else make_fault Call_depth_exceeded None
                else if Int64.equal kind 5L then
                  if not candidate.call_site then
                    Error
                      "native program frame-limit status names a non-call site"
                  else if executed_steps_int < 1 then
                    Error
                      "native program frame-limit fault did not consume its \
                       IC_CALL"
                  else make_fault Frame_limit_exceeded None
                else if Int64.equal kind 6L then
                  if (not candidate.call_site) || candidate.output_site then
                    Error
                      "native program native-stack status names a non-call site"
                  else if executed_steps_int < 1 then
                    Error
                      "native program native-stack fault did not consume its \
                       IC_CALL"
                  else make_fault Native_stack_limit_exceeded None
                else if Int64.equal kind 7L then
                  if not candidate.uninitialized_read_site then
                    Error
                      "native program uninitialized-read status names a \
                       non-load site"
                  else if executed_steps_int < 1 then
                    Error
                      "native program uninitialized read did not consume its \
                       instruction"
                  else make_fault Uninitialized_read None
                else if Int64.equal kind 8L then
                  if not candidate.index_scale_site then
                    Error
                      "native program index-scale status names a non-scaling \
                       site"
                  else if executed_steps_int < 1 then
                    Error
                      "native program index scaling fault did not consume its \
                       instruction"
                  else make_fault Index_scale_overflow None
                else if Int64.equal kind 9L then
                  if not candidate.index_addition_site then
                    Error
                      "native program index-addition status names a \
                       non-index-addition site"
                  else if executed_steps_int < 1 then
                    Error
                      "native program index addition fault did not consume its \
                       instruction"
                  else make_fault Index_addition_overflow None
                else if Int64.equal kind 10L then
                  if not candidate.address_bounds_site then
                    Error
                      "native program address-bounds status names a site \
                       without a bounds check"
                  else if executed_steps_int < 1 then
                    Error
                      "native program address bounds fault did not consume its \
                       instruction"
                  else make_fault Address_out_of_bounds None
                else if Int64.equal kind 11L || Int64.equal kind 12L then
                  if not candidate.output_site then
                    Error "native program output status names a non-output site"
                  else if executed_steps_int < 1 then
                    Error
                      "native program output fault did not consume its call \
                       instruction"
                  else
                    make_fault
                      (if Int64.equal kind 11L then Output_limit_exceeded
                       else Output_work_limit_exceeded)
                      None
                else Error "native program status has an unknown fault kind")

let validate_global_limit ~max_global_bytes =
  Codegen.validate_global_limit ~max_global_bytes
  |> Result.map_error project_errors

let validate_literal_limit ~max_literal_bytes =
  X86_64_literal_storage.validate_limit ~max_literal_bytes
  |> Result.map_error
       (List.map (fun (error : X86_64_literal_storage.error) ->
            { code = error.code; message = error.message; span = error.span }))

let global_bytes compiled = Codegen.program_global_bytes compiled.image
let literal_bytes compiled = Codegen.program_literal_bytes compiled.image

let arena_metadata_bytes compiled =
  Codegen.program_arena_metadata_bytes compiled.image

let global_image compiled = Codegen.program_global_image compiled.image
