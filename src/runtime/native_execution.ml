type platform = Windows_x86_64 | Linux_x86_64 | Unsupported

module Image = Backend.X86_64_expression

type execution_outcome = Returned of int64 | Fault of Image.arithmetic_fault

external native_platform : unit -> int = "holyc_native_platform"

external execute_image : string -> string -> int -> int64 * int64 * int64
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

let execute_detailed image =
  let host = platform () in
  let abi = Image.status_abi image in
  let abi_matches =
    match (host, abi) with
    | _, None
    | Windows_x86_64, Some Image.Windows_x64
    | Linux_x86_64, Some Image.System_v_x64 -> true
    | _ -> false
  in
  match host with
  | Unsupported ->
      Error
        "native execution requires Windows or Linux x86-64 with 64-bit pointers"
  | _ when not abi_matches ->
      Error "native image status ABI does not match this process"
  | Windows_x86_64 | Linux_x86_64 -> (
      try
        let abi_code =
          match abi with
          | None -> 0
          | Some Image.Windows_x64 -> 1
          | Some Image.System_v_x64 -> 2
        in
        let bits, kind, site =
          execute_image (Image.code image)
            (Image.windows_unwind_info image)
            abi_code
        in
        match Image.decode_runtime_status image ~kind ~site with
        | Ok None -> Ok (Returned bits)
        | Ok (Some fault) -> Ok (Fault fault)
        | Error message -> Error ("native status integrity failure: " ^ message)
      with Failure message | Invalid_argument message -> Error message)

let execute image =
  match execute_detailed image with
  | Ok (Returned bits) -> Ok bits
  | Ok (Fault fault) ->
      let error = Image.arithmetic_fault_error fault in
      Error (error.code ^ ": " ^ error.message)
  | Error _ as error -> error
