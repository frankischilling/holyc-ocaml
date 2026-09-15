type platform = Windows_x86_64 | Linux_x86_64 | Unsupported

external native_platform : unit -> int = "holyc_native_platform"

external execute_image : string -> string -> int64
  = "holyc_native_execute_image"

let platform () =
  match native_platform () with
  | 1 -> Windows_x86_64
  | 2 -> Linux_x86_64
  | _ -> Unsupported

let platform_name = function
  | Windows_x86_64 -> "windows-x86_64"
  | Linux_x86_64 -> "linux-x86_64"
  | Unsupported -> "unsupported"

let execute image =
  match platform () with
  | Unsupported ->
      Error
        "native execution requires Windows or Linux x86-64 with 64-bit pointers"
  | Windows_x86_64 | Linux_x86_64 -> (
      try
        Ok
          (execute_image
             (Backend.X86_64_expression.code image)
             (Backend.X86_64_expression.windows_unwind_info image))
      with Failure message | Invalid_argument message -> Error message)
