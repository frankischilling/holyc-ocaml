type platform = Native_execution.platform =
  | Windows_x86_64
  | Linux_x86_64
  | Unsupported

module Image = Backend.X86_64_program

external execute_program_image :
  string -> string -> int -> int -> int64 * int64 * int64 * int64 * int64
  = "holyc_native_execute_program"

external execute_program_functions :
  string ->
  (int * int * string) array ->
  int ->
  int * int * int * int * int ->
  int64 * int64 * int64 * int64 * int64
  = "holyc_native_execute_program_functions"

external execute_program_storage :
  string ->
  (int * int * string) array ->
  int ->
  int * int * int * int * int * int * int ->
  int * int * int * string ->
  int64 * int64 * int64 * int64 * int64 = "holyc_native_execute_program_storage"

let platform = Native_execution.platform
let platform_name = Native_execution.platform_name
let hard_max_active_stack_bytes = 65_536
let hard_max_global_bytes = 16_777_216
let hard_max_literal_bytes = 16_777_216
let hard_max_arena_bytes = 33_554_432

let execute ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128)
    ?(max_active_stack_bytes = hard_max_active_stack_bytes)
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576) ~max_steps
    image =
  if max_steps <= 0 then
    Error "native program max_steps must be greater than zero"
  else if max_frame_bytes <= 0 then
    Error "native program max_frame_bytes must be greater than zero"
  else if max_call_depth <= 0 then
    Error "native program max_call_depth must be greater than zero"
  else if
    max_active_stack_bytes <= 0
    || max_active_stack_bytes > hard_max_active_stack_bytes
  then
    Error
      (Printf.sprintf
         "native program max_active_stack_bytes must be between 1 and %d"
         hard_max_active_stack_bytes)
  else if max_global_bytes <= 0 || max_global_bytes > hard_max_global_bytes then
    Error
      (Printf.sprintf "native program max_global_bytes must be between 1 and %d"
         hard_max_global_bytes)
  else if max_literal_bytes <= 0 || max_literal_bytes > hard_max_literal_bytes
  then
    Error
      (Printf.sprintf
         "native program max_literal_bytes must be between 1 and %d"
         hard_max_literal_bytes)
  else
    let entry_stack_bytes = Image.entry_stack_bytes image in
    if entry_stack_bytes <= 0 then
      Error "native program image has invalid entry stack metadata"
    else if entry_stack_bytes > max_active_stack_bytes then
      Error
        (Printf.sprintf
           "native program entry stack (%d bytes) exceeds \
            max_active_stack_bytes (%d)"
           entry_stack_bytes max_active_stack_bytes)
    else
      let function_count = Image.function_count image in
      let unwind_functions = Image.windows_unwind_functions image in
      if function_count < 0 then
        Error "native program image has an invalid function count"
      else if function_count > 100_000 then
        Error "native program image exceeds the callable function-count bound"
      else if List.length unwind_functions <> function_count + 1 then
        Error "native program image has inconsistent unwind function metadata"
      else
        let global_bytes = Image.global_bytes image in
        let literal_bytes = Image.literal_bytes image in
        let metadata_bytes = Image.arena_metadata_bytes image in
        let global_image = Image.global_image image in
        let arena_bytes = String.length global_image in
        if global_bytes < 0 then
          Error "native program image has a negative global byte count"
        else if global_bytes > hard_max_global_bytes then
          Error "native program image exceeds the hard global byte bound"
        else if global_bytes > max_global_bytes then
          Error
            (Printf.sprintf
               "native program global storage (%d bytes) exceeds \
                max_global_bytes (%d)"
               global_bytes max_global_bytes)
        else if literal_bytes < 0 || literal_bytes > hard_max_literal_bytes then
          Error "native program image has an invalid literal byte count"
        else if literal_bytes > max_literal_bytes then
          Error
            (Printf.sprintf
               "native program literal storage (%d bytes) exceeds \
                max_literal_bytes (%d)"
               literal_bytes max_literal_bytes)
        else if metadata_bytes < 0 || metadata_bytes > hard_max_arena_bytes then
          Error
            "native program image has an invalid private metadata byte count"
        else if arena_bytes > hard_max_arena_bytes then
          Error "native program private arena exceeds the hard host byte bound"
        else if global_bytes = 0 && literal_bytes = 0 && arena_bytes <> 0 then
          Error
            "native program without persistent data has a nonempty private \
             arena image"
        else if arena_bytes <> global_bytes + literal_bytes + metadata_bytes
        then
          Error
            "native program private arena image disagrees with its declared \
             data and metadata"
        else
          let host = platform () in
          let abi = Image.status_abi image in
          match (host, abi) with
          | Unsupported, _ ->
              Error
                "native execution requires Windows or Linux x86-64 with 64-bit \
                 pointers"
          | Windows_x86_64, Image.Windows_x64 | Linux_x86_64, Image.System_v_x64
            -> (
              try
                let abi_code =
                  match abi with
                  | Image.Windows_x64 -> 1
                  | Image.System_v_x64 -> 2
                in
                let kind, site, executed_steps, value_site, bits =
                  if arena_bytes > 0 then
                    execute_program_storage (Image.code image)
                      (Array.of_list unwind_functions)
                      abi_code
                      ( max_steps,
                        max_frame_bytes,
                        max_call_depth,
                        max_active_stack_bytes,
                        entry_stack_bytes,
                        max_global_bytes,
                        max_literal_bytes )
                      (global_bytes, literal_bytes, metadata_bytes, global_image)
                  else if function_count = 0 then
                    execute_program_image (Image.code image)
                      (Image.windows_unwind_info image)
                      abi_code max_steps
                  else
                    execute_program_functions (Image.code image)
                      (Array.of_list unwind_functions)
                      abi_code
                      ( max_steps,
                        max_frame_bytes,
                        max_call_depth,
                        max_active_stack_bytes,
                        entry_stack_bytes )
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
