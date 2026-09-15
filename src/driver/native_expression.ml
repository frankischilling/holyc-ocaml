module Image = Backend.X86_64_expression
module Native = Runtime.Native_execution

type result = { image : Image.t; bits : int64; platform : Native.platform }

let ( let* ) = Result.bind

let compile ?(max_ir_instructions = 4096) ?(max_code_bytes = 65536)
    ?(max_stack_bytes = Image.hard_max_stack_bytes) session ~config ~source =
  let span = Integer_source.source_span source in
  let errors =
    List.map (fun (error : Image.error) ->
        Integer_source.diagnostic
          ~span:(Option.value error.span ~default:span)
          error.code error.message)
  in
  let* () =
    Image.validate_limits ~max_ir_instructions ~max_code_bytes
    |> Result.map_error errors
  in
  let* () =
    Image.validate_stack_limit ~max_stack_bytes |> Result.map_error errors
  in
  let* graph = Integer_expression.lower session ~config ~source in
  Image.compile ~max_ir_instructions ~max_code_bytes ~max_stack_bytes graph
  |> Result.map_error errors

let evaluate ?max_ir_instructions ?max_code_bytes ?max_stack_bytes session
    ~config ~source =
  let* image =
    compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes session
      ~config ~source
  in
  let platform = Native.platform () in
  match Native.execute image with
  | Ok bits -> Ok { image; bits; platform }
  | Error message ->
      Error
        [
          Integer_source.diagnostic
            ~span:(Integer_source.source_span source)
            (if platform = Native.Unsupported then "HCNATIVE0001"
             else "HCNATIVE0002")
            message;
        ]
