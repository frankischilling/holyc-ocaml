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

external execute_program_output :
  string ->
  (int * int * string) array ->
  int ->
  int * int * int * int * int * int * int * int * int ->
  int * int * int * string ->
  (int64 * int64 * int64 * int64 * int64) * string * int
  = "holyc_native_execute_program_output"

type retained_handle

external retain_program :
  string * (int * int * string) array * int * int * (int * int * int * string) ->
  retained_handle = "holyc_native_retain_program"

external release_program : retained_handle -> unit
  = "holyc_native_release_program"

external execute_retained_program :
  retained_handle ->
  int * int * int * int * int * int * int * int * int ->
  (int64 * int64 * int64 * int64 * int64) * string * int
  = "holyc_native_execute_retained_program"

external execute_retained_budget_program :
  retained_handle ->
  int * int * int * int * int * int * int * int * int ->
  int * int * int ->
  bool ref ->
  (int64 * int64 * int64 * int64 * int64) * string * int
  = "holyc_native_execute_retained_budget_program"

type retained = {
  image_ : Image.t;
  handle_ : retained_handle;
  released_ : bool Atomic.t;
}

let platform = Native_execution.platform
let platform_name = Native_execution.platform_name
let hard_max_active_stack_bytes = 65_536
let hard_max_global_bytes = 16_777_216
let hard_max_literal_bytes = 16_777_216
let hard_max_arena_bytes = 33_554_432
let hard_max_output_bytes = 16_777_216

type report = {
  outcome_ : (Image.outcome, string) result;
  output_bytes_ : string;
  output_work_ : int;
}

let outcome report = report.outcome_
let output_bytes report = report.output_bytes_
let output_work report = report.output_work_

let error_report message =
  { outcome_ = Error message; output_bytes_ = ""; output_work_ = 0 }

let execute_report_internal ?retained ?consumed ?entered
    ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128)
    ?(max_active_stack_bytes = hard_max_active_stack_bytes)
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576)
    ?(max_output_bytes = 1_048_576) ?(max_output_work = 1_048_576) ~max_steps
    image =
  let prior_steps, prior_output, prior_work =
    Option.value ~default:(0, 0, 0) consumed
  in
  if max_steps <= 0 then
    error_report "native program max_steps must be greater than zero"
  else if max_frame_bytes <= 0 then
    error_report "native program max_frame_bytes must be greater than zero"
  else if max_call_depth <= 0 then
    error_report "native program max_call_depth must be greater than zero"
  else if
    max_active_stack_bytes <= 0
    || max_active_stack_bytes > hard_max_active_stack_bytes
  then
    error_report
      (Printf.sprintf
         "native program max_active_stack_bytes must be between 1 and %d"
         hard_max_active_stack_bytes)
  else if max_global_bytes <= 0 || max_global_bytes > hard_max_global_bytes then
    error_report
      (Printf.sprintf "native program max_global_bytes must be between 1 and %d"
         hard_max_global_bytes)
  else if max_literal_bytes <= 0 || max_literal_bytes > hard_max_literal_bytes
  then
    error_report
      (Printf.sprintf
         "native program max_literal_bytes must be between 1 and %d"
         hard_max_literal_bytes)
  else if max_output_bytes <= 0 || max_output_bytes > hard_max_output_bytes then
    error_report
      (Printf.sprintf "native program max_output_bytes must be between 1 and %d"
         hard_max_output_bytes)
  else if max_output_work <= 0 then
    error_report "native program max_output_work must be greater than zero"
  else if
    Option.is_some consumed
    && (Option.is_none retained || Option.is_none entered)
    || prior_steps < 0 || prior_steps > max_steps || prior_output < 0
    || prior_output > max_output_bytes
    || prior_work < 0
    || prior_work > max_output_work
    || prior_output > prior_work
  then error_report "retained native consumed budget exceeds its limits"
  else
    let entry_stack_bytes = Image.entry_stack_bytes image in
    if entry_stack_bytes <= 0 then
      error_report "native program image has invalid entry stack metadata"
    else if entry_stack_bytes > max_active_stack_bytes then
      error_report
        (Printf.sprintf
           "native program entry stack (%d bytes) exceeds \
            max_active_stack_bytes (%d)"
           entry_stack_bytes max_active_stack_bytes)
    else
      let function_count = Image.function_count image in
      let unwind_functions = Image.windows_unwind_functions image in
      if function_count < 0 then
        error_report "native program image has an invalid function count"
      else if function_count > 100_000 then
        error_report
          "native program image exceeds the callable function-count bound"
      else if List.length unwind_functions <> function_count + 1 then
        error_report
          "native program image has inconsistent unwind function metadata"
      else
        let global_bytes = Image.global_bytes image in
        let literal_bytes = Image.literal_bytes image in
        let metadata_bytes = Image.arena_metadata_bytes image in
        let global_image = Image.global_image image in
        let arena_bytes = String.length global_image in
        if global_bytes < 0 then
          error_report "native program image has a negative global byte count"
        else if global_bytes > hard_max_global_bytes then
          error_report "native program image exceeds the hard global byte bound"
        else if global_bytes > max_global_bytes then
          error_report
            (Printf.sprintf
               "native program global storage (%d bytes) exceeds \
                max_global_bytes (%d)"
               global_bytes max_global_bytes)
        else if literal_bytes < 0 || literal_bytes > hard_max_literal_bytes then
          error_report "native program image has an invalid literal byte count"
        else if literal_bytes > max_literal_bytes then
          error_report
            (Printf.sprintf
               "native program literal storage (%d bytes) exceeds \
                max_literal_bytes (%d)"
               literal_bytes max_literal_bytes)
        else if metadata_bytes < 0 || metadata_bytes > hard_max_arena_bytes then
          error_report
            "native program image has an invalid private metadata byte count"
        else if arena_bytes > hard_max_arena_bytes then
          error_report
            "native program private arena exceeds the hard host byte bound"
        else if global_bytes = 0 && literal_bytes = 0 && arena_bytes <> 0 then
          error_report
            "native program without persistent data has a nonempty private \
             arena image"
        else if arena_bytes <> global_bytes + literal_bytes + metadata_bytes
        then
          error_report
            "native program private arena image disagrees with its declared \
             data and metadata"
        else
          let host = platform () in
          let abi = Image.status_abi image in
          match (host, abi) with
          | Unsupported, _ ->
              error_report
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
                let status, captured, work =
                  if Option.is_some retained then
                    let limits =
                      ( max_steps,
                        max_frame_bytes,
                        max_call_depth,
                        max_active_stack_bytes,
                        entry_stack_bytes,
                        max_global_bytes,
                        max_literal_bytes,
                        max_output_bytes,
                        max_output_work )
                    in
                    match consumed with
                    | None ->
                        execute_retained_program (Option.get retained) limits
                    | Some consumed ->
                        execute_retained_budget_program (Option.get retained)
                          limits consumed (Option.get entered)
                  else if Image.has_output image then
                    execute_program_output (Image.code image)
                      (Array.of_list unwind_functions)
                      abi_code
                      ( max_steps,
                        max_frame_bytes,
                        max_call_depth,
                        max_active_stack_bytes,
                        entry_stack_bytes,
                        max_global_bytes,
                        max_literal_bytes,
                        max_output_bytes,
                        max_output_work )
                      (global_bytes, literal_bytes, metadata_bytes, global_image)
                  else
                    let status =
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
                          ( global_bytes,
                            literal_bytes,
                            metadata_bytes,
                            global_image )
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
                    (status, "", 0)
                in
                let kind, site, executed_steps, value_site, bits = status in
                let decoded =
                  Image.decode_runtime_status image ~max_steps ~kind ~site
                    ~executed_steps ~value_site ~bits
                in
                let atomic_fault =
                  match decoded with
                  | Ok (Image.Fault fault) -> fault.atomic_output
                  | Ok (Image.Completed _) | Error _ -> false
                in
                let captured_length = String.length captured in
                let available_output = max_output_bytes - prior_output in
                let available_work = max_output_work - prior_work in
                let output_status_valid =
                  captured_length <= available_output
                  && work >= 0 && work <= available_work
                  && captured_length <= work
                  && Int64.compare executed_steps (Int64.of_int prior_steps)
                     >= 0
                  && ((not
                         (Int64.equal executed_steps (Int64.of_int prior_steps)))
                     || (captured_length = 0 && work = 0))
                  &&
                  if Int64.equal kind 11L then
                    atomic_fault || captured_length = available_output
                  else if Int64.equal kind 12L then work = available_work
                  else true
                in
                if not output_status_valid then
                  error_report
                    "native program status integrity failure: output counters \
                     disagree with the returned fault status"
                else
                  match decoded with
                  | Ok outcome_ ->
                      {
                        outcome_ = Ok outcome_;
                        output_bytes_ = captured;
                        output_work_ = work;
                      }
                  | Error message ->
                      error_report
                        ("native program status integrity failure: " ^ message)
              with Failure message | Invalid_argument message ->
                error_report message)
          | _ ->
              error_report
                "native program status ABI does not match this process"

let retain ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576)
    ?(max_active_stack_bytes = hard_max_active_stack_bytes) image =
  let abi = Image.status_abi image in
  let abi_code =
    match (platform (), abi) with
    | Windows_x86_64, Image.Windows_x64 -> Some 1
    | Linux_x86_64, Image.System_v_x64 -> Some 2
    | _ -> None
  in
  if max_global_bytes <= 0 || max_global_bytes > hard_max_global_bytes then
    Error "retained native max_global_bytes is outside the host bound"
  else if max_literal_bytes <= 0 || max_literal_bytes > hard_max_literal_bytes
  then Error "retained native max_literal_bytes is outside the host bound"
  else if
    max_active_stack_bytes <= 0
    || max_active_stack_bytes > hard_max_active_stack_bytes
  then Error "retained native max_active_stack_bytes is outside the host bound"
  else if Image.global_bytes image > max_global_bytes then
    Error "retained native image exceeds max_global_bytes"
  else if Image.literal_bytes image > max_literal_bytes then
    Error "retained native image exceeds max_literal_bytes"
  else if Image.entry_stack_bytes image > max_active_stack_bytes then
    Error "retained native entry exceeds max_active_stack_bytes"
  else if
    Image.function_count image < 0
    || Image.function_count image > 100_000
    || List.length (Image.windows_unwind_functions image)
       <> Image.function_count image + 1
  then Error "retained native image has inconsistent callable metadata"
  else
    match abi_code with
    | None -> Error "retained native status ABI does not match this host"
    | Some abi_code -> (
        try
          let handle_ =
            retain_program
              ( Image.code image,
                Array.of_list (Image.windows_unwind_functions image),
                abi_code,
                Image.entry_stack_bytes image,
                ( Image.global_bytes image,
                  Image.literal_bytes image,
                  Image.arena_metadata_bytes image,
                  Image.global_image image ) )
          in
          Ok { image_ = image; handle_; released_ = Atomic.make false }
        with Failure message | Invalid_argument message -> Error message)

let release retained =
  if Atomic.get retained.released_ then Ok ()
  else
    try
      release_program retained.handle_;
      Atomic.set retained.released_ true;
      Ok ()
    with Failure message | Invalid_argument message -> Error message

let execute_retained_report ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes ?max_global_bytes ?max_literal_bytes
    ?max_output_bytes ?max_output_work ~max_steps retained =
  if Atomic.get retained.released_ then
    error_report "retained native image has been released"
  else
    execute_report_internal ~retained:retained.handle_ ?max_frame_bytes
      ?max_call_depth ?max_active_stack_bytes ?max_global_bytes
      ?max_literal_bytes ?max_output_bytes ?max_output_work ~max_steps
      retained.image_

type budget_state = {
  steps_ : int;
  bytes_ : int;
  work_ : int;
  chunks_ : string list;
  error_ : string option;
}

type budget = {
  max_steps_ : int;
  max_output_bytes_ : int;
  max_output_work_ : int;
  active_ : bool Atomic.t;
  state_ : budget_state Atomic.t;
}

type budget_progress = {
  executed_steps : int;
  output_byte_length : int;
  output_work : int;
  output_bytes : string;
  error : string option;
}

let create_budget ?(max_output_bytes = 1_048_576) ?(max_output_work = 1_048_576)
    ~max_steps () =
  if max_steps <= 0 then
    Error "native budget max_steps must be greater than zero"
  else if max_output_bytes <= 0 || max_output_bytes > hard_max_output_bytes then
    Error "native budget max_output_bytes is outside the host bound"
  else if max_output_work <= 0 then
    Error "native budget max_output_work must be greater than zero"
  else
    Ok
      {
        max_steps_ = max_steps;
        max_output_bytes_ = max_output_bytes;
        max_output_work_ = max_output_work;
        active_ = Atomic.make false;
        state_ =
          Atomic.make
            { steps_ = 0; bytes_ = 0; work_ = 0; chunks_ = []; error_ = None };
      }

let copy_string value = Bytes.to_string (Bytes.of_string value)

let budget_progress budget =
  let state = Atomic.get budget.state_ in
  {
    executed_steps = state.steps_;
    output_byte_length = state.bytes_;
    output_work = state.work_;
    output_bytes = copy_string (String.concat "" (List.rev state.chunks_));
    error = state.error_;
  }

(* Newest chunks come first, with strictly increasing lengths. Coalescing
   bounds retained list metadata and avoids copying the full output prefix on
   every one-byte activation. Reports never share writable backing with it. *)
let append_capture captured chunks =
  if captured = "" then chunks
  else
    let rec append current = function
      | prior :: rest when String.length prior <= String.length current ->
          append (prior ^ current) rest
      | rest -> current :: rest
    in
    append (copy_string captured) chunks

let execute_retained_budget_report ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes ?max_global_bytes ?max_literal_bytes budget retained
    =
  if not (Atomic.compare_and_set budget.active_ false true) then
    error_report "retained native budget is already active"
  else
    Fun.protect
      ~finally:(fun () -> Atomic.set budget.active_ false)
      (fun () ->
        let state = Atomic.get budget.state_ in
        match state.error_ with
        | Some message -> error_report message
        | None when Atomic.get retained.released_ ->
            error_report "retained native image has been released"
        | None -> (
            let entered = ref false in
            let poisoned =
              {
                state with
                error_ =
                  Some
                    "retained native budget is unavailable after an unverified \
                     activation";
              }
            in
            try
              let report =
                execute_report_internal ~retained:retained.handle_
                  ~consumed:(state.steps_, state.bytes_, state.work_)
                  ~entered ?max_frame_bytes ?max_call_depth
                  ?max_active_stack_bytes ?max_global_bytes ?max_literal_bytes
                  ~max_steps:budget.max_steps_
                  ~max_output_bytes:budget.max_output_bytes_
                  ~max_output_work:budget.max_output_work_ retained.image_
              in
              (match report.outcome_ with
              | Error _ -> if !entered then Atomic.set budget.state_ poisoned
              | Ok outcome ->
                  let steps_ =
                    match outcome with
                    | Image.Completed execution -> execution.executed_steps
                    | Image.Fault fault -> fault.executed_steps
                  in
                  let next =
                    {
                      steps_;
                      bytes_ = state.bytes_ + String.length report.output_bytes_;
                      work_ = state.work_ + report.output_work_;
                      chunks_ =
                        append_capture report.output_bytes_ state.chunks_;
                      error_ = None;
                    }
                  in
                  Atomic.set budget.state_ next);
              report
            with exception_ ->
              if !entered then Atomic.set budget.state_ poisoned;
              raise exception_))

let execute_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?max_global_bytes ?max_literal_bytes ?max_output_bytes ?max_output_work
    ~max_steps image =
  execute_report_internal ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes ?max_global_bytes ?max_literal_bytes
    ?max_output_bytes ?max_output_work ~max_steps image

let execute ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?max_global_bytes ?max_literal_bytes ?max_output_bytes ?max_output_work
    ~max_steps image =
  execute_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?max_global_bytes ?max_literal_bytes ?max_output_bytes ?max_output_work
    ~max_steps image
  |> outcome
