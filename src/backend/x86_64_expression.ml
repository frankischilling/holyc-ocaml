module Codegen = X86_64_word_codegen

type word_type = Codegen.word_type = I64 | U64
type status_abi = Codegen.status_abi = Windows_x64 | System_v_x64
type arithmetic_operation = Codegen.arithmetic_operation = Divide | Remainder
type arithmetic_fault_kind = Division_by_zero | Signed_division_overflow

type arithmetic_fault = {
  kind : arithmetic_fault_kind;
  operation : arithmetic_operation;
  instruction_id : int;
  position : int;
  span : Common.Span.t option;
}

type error = Codegen.error = {
  code : string;
  message : string;
  span : Common.Span.t option;
}

type t = Codegen.expression_image

let hard_max_stack_bytes = Codegen.hard_max_stack_bytes
let validate_limits = Codegen.validate_limits
let validate_stack_limit = Codegen.validate_stack_limit
let compile = Codegen.compile_expression
let code = Codegen.expression_code
let value_type = Codegen.expression_word_type
let ir_instructions = Codegen.expression_ir_instructions
let machine_instructions = Codegen.expression_machine_instructions
let register_peak = Codegen.expression_register_peak
let frame_bytes = Codegen.expression_frame_bytes
let windows_unwind_info = Codegen.expression_windows_unwind_info
let status_abi = Codegen.expression_status_abi

let arithmetic_fault_error (fault : arithmetic_fault) =
  let opcode =
    match fault.operation with
    | Divide -> "IC_DIV"
    | Remainder -> "IC_MOD"
  in
  match fault.kind with
  | Division_by_zero ->
      {
        code = "HCNATIVE0004";
        message = opcode ^ " divisor is zero";
        span = fault.span;
      }
  | Signed_division_overflow ->
      {
        code = "HCNATIVE0005";
        message = opcode ^ " signed quotient overflows I64";
        span = fault.span;
      }

let decode_runtime_status (compiled : t) ~kind ~site =
  if Int64.equal kind 0L then
    if Int64.equal site 0L then Ok None
    else Error "native arithmetic status has a site without a fault kind"
  else if Int64.equal site 0L then
    Error "native arithmetic status has a fault kind without a site"
  else
    let fault_kind =
      if Int64.equal kind 1L then Ok Division_by_zero
      else if Int64.equal kind 2L then Ok Signed_division_overflow
      else Error "native arithmetic status has an unknown fault kind"
    in
    match fault_kind with
    | Error _ as error -> error
    | Ok fault_kind -> (
        if Int64.compare site 1L < 0 || Int64.compare site 100_000L > 0 then
          Error "native arithmetic status site is outside the checked IR bound"
        else
          let site_value = Int64.to_int site in
          match
            List.find_opt
              (fun (candidate : Codegen.arithmetic_fault_site) ->
                candidate.site = site_value)
              (Codegen.expression_fault_sites compiled)
          with
          | None -> Error "native arithmetic status names an unknown fault site"
          | Some candidate ->
              if fault_kind = Signed_division_overflow && not candidate.signed
              then
                Error
                  "native arithmetic status reports signed overflow at an \
                   unsigned division site"
              else
                Ok
                  (Some
                     {
                       kind = fault_kind;
                       operation = candidate.operation;
                       instruction_id = candidate.instruction_id;
                       position = candidate.position;
                       span = candidate.span;
                     }))
