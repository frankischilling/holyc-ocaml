type platform = Native_execution.platform =
  | Windows_x86_64
  | Linux_x86_64
  | Unsupported

module Image = Backend.X86_64_program

external execute_program_image :
  string -> string -> int -> int -> int64 * int64 * int64 * int64 * int64
  = "holyc_native_execute_program"

let platform = Native_execution.platform
let platform_name = Native_execution.platform_name

let execute ~max_steps image =
  if max_steps <= 0 then
    Error "native program max_steps must be greater than zero"
  else
    let host = platform () in
    let abi = Image.status_abi image in
    match (host, abi) with
    | Unsupported, _ ->
        Error
          "native execution requires Windows or Linux x86-64 with 64-bit \
           pointers"
    | Windows_x86_64, Image.Windows_x64 | Linux_x86_64, Image.System_v_x64 -> (
        try
          let abi_code =
            match abi with
            | Image.Windows_x64 -> 1
            | Image.System_v_x64 -> 2
          in
          let kind, site, executed_steps, value_site, bits =
            execute_program_image (Image.code image)
              (Image.windows_unwind_info image)
              abi_code max_steps
          in
          match
            Image.decode_runtime_status image ~max_steps ~kind ~site
              ~executed_steps ~value_site ~bits
          with
          | Ok _ as result -> result
          | Error message ->
              Error ("native program status integrity failure: " ^ message)
        with Failure message | Invalid_argument message -> Error message)
    | _ -> Error "native program status ABI does not match this process"
