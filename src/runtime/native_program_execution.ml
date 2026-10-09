type platform = Native_execution.platform =
  | Windows_x86_64
  | Linux_x86_64
  | Unsupported

module Image = Backend.X86_64_program
module Task_storage = Backend.X86_64_global_storage

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
type task_arena_handle
type source_bridge
type source_checkpoint

external create_source_checkpoint :
  Ir.Native_source_suspension.t -> unit ref -> source_checkpoint
  = "holyc_native_source_checkpoint_create"

external consume_source_checkpoint :
  source_checkpoint ->
  Ir.Native_source_suspension.t ->
  unit ref ->
  int64
  * int
  * string
  * Ir.Integer_interpreter.native_generation Ir.Native_generation_capture.t
  = "holyc_native_source_checkpoint_consume"

type source_callback_state =
  (source_bridge -> int64 option) * bool ref * exn ref * unit ref

external suspension_owns_budget_raw :
  Ir.Native_source_suspension.t -> unit ref -> bool
  = "holyc_native_source_suspension_owns_budget"

external suspension_owns_arena_raw :
  Ir.Native_source_suspension.t -> task_arena_handle -> bool
  = "holyc_native_source_suspension_owns_arena"

external retain_program :
  string * (int * int * string) array * int * int * (int * int * int * string) ->
  retained_handle = "holyc_native_retain_program"

external retain_task_fragment_program :
  string
  * (int * int * string) array
  * int
  * int
  * (int * int * int * int)
  * (int * int * int * int * int) array
  * (int * int) array ->
  retained_handle = "holyc_native_retain_task_fragment"

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

external create_task_arena_handle : int -> task_arena_handle
  = "holyc_native_create_task_arena"

external admit_task_arena :
  task_arena_handle ->
  int ->
  int ->
  (int * string) list ->
  Ir.Native_source_suspension.t option ->
  int = "holyc_native_task_arena_admit"

external release_task_arena_handle : task_arena_handle -> unit
  = "holyc_native_release_task_arena"

external copy_task_static_bytes :
  task_arena_handle ->
  int * int * int * string ->
  Ir.Native_source_suspension.t option ->
  int = "holyc_native_task_static_copy"

external read_task_default_string :
  task_arena_handle ->
  int * int * int * int ->
  Ir.Native_source_suspension.t option ->
  int * int * string = "holyc_native_read_task_default_string"

external read_task_default_address_offset :
  task_arena_handle -> int * int -> Ir.Native_source_suspension.t option -> int
  = "holyc_native_read_task_default_address_offset"

external bind_task_default_string :
  task_arena_handle ->
  int * int * int * int ->
  Ir.Native_source_suspension.t option ->
  unit = "holyc_native_bind_task_default_string"

external execute_retained_budget_task_program :
  retained_handle ->
  task_arena_handle
  * int
  * (Ir.Integer_interpreter.native_generation
    * bool
    * int
    * int
    * Ir.Integer_interpreter.native_generation Ir.Native_generation_capture.t
      option
      ref
    * source_callback_state option
    * Ir.Integer_output.byte_budget)
  * (Ir.Native_source_suspension.t option * unit ref) ->
  int * int * int * int * int * int * int * int * int ->
  int * int * int ->
  bool ref ->
  (int64 * int64 * int64 * int64 * int64) * string * int
  = "holyc_native_execute_retained_budget_task_program"

external execute_retained_budget_scalar_program :
  retained_handle ->
  task_arena_handle
  * int
  * (Ir.Integer_interpreter.native_generation
    * bool
    * int
    * int
    * Ir.Integer_interpreter.native_generation Ir.Native_generation_capture.t
      option
      ref
    * source_callback_state option
    * Ir.Integer_output.byte_budget)
  * (Ir.Native_source_suspension.t option * unit ref) ->
  int * int * int * int * int * int * int * int * int ->
  int * int * int ->
  bool ref * 'program ->
  ((int64 * int64 * int64 * int64 * int64) * string * int)
  * 'program Ir.Native_scalar_capture.t option
  = "holyc_native_execute_retained_budget_binding_program"

external bind_task_entries :
  retained_handle ->
  task_arena_handle * int ->
  Ir.Native_source_suspension.t option ->
  bool = "holyc_native_bind_task_entries"

type scalar_capture =
  | Binding_capture of Ir.Native_internal_binding_capture.t
  | Dimension_capture of
      Ir.Dimension_fragment_program.t Ir.Native_scalar_capture.t
  | Offset_capture of Ir.Offset_fragment_program.t Ir.Native_scalar_capture.t

type task_arena = {
  layout_ : Task_storage.task_layout;
  handle_ : task_arena_handle;
  max_arena_bytes_ : int;
  admitted_bytes_ : int Atomic.t;
  budget_owner_ : unit ref option Atomic.t;
  arena_lease_ : bool Atomic.t;
  arena_revoked_ : bool Atomic.t;
  arena_released_ : bool Atomic.t;
  code_mappings_ : (retained_handle * Image.t * bool) list Atomic.t;
  data_capture_ : (Image.t * Ir.Saved_parameter_value.t) option Atomic.t;
  scalar_capture_ : (Image.t * scalar_capture) option Atomic.t;
  data_comparison_owner_ : unit ref;
}

type task_execution_binding = {
  task_arena_ : task_arena;
  task_required_arena_bytes_ : int;
  task_budget_identity_ : unit ref;
}

type retained_storage =
  | Private_storage
  | Shared_task_storage of { arena : task_arena; required_arena_bytes : int }

type retained = {
  image_ : Image.t;
  handle_ : retained_handle;
  storage_ : retained_storage;
  lease_ : bool Atomic.t;
  revoked_ : bool Atomic.t;
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
  value_captured_ : bool;
  generation_capture_ :
    Ir.Integer_interpreter.native_generation Ir.Native_generation_capture.t
    option;
}

let outcome report = report.outcome_
let output_bytes report = report.output_bytes_
let output_work report = report.output_work_
let generation_capture report = report.generation_capture_
let value_captured report = report.value_captured_

let error_report message =
  {
    outcome_ = Error message;
    output_bytes_ = "";
    output_work_ = 0;
    value_captured_ = false;
    generation_capture_ = None;
  }

let bind_task_budget arena identity =
  let rec bind () =
    match Atomic.get arena.budget_owner_ with
    | Some owner ->
        if owner == identity then Ok ()
        else Error "native task arena belongs to another cumulative budget"
    | None ->
        if Atomic.compare_and_set arena.budget_owner_ None (Some identity) then
          Ok ()
        else bind ()
  in
  bind ()

let acquire_lease lease message =
  if Atomic.compare_and_set lease false true then Ok () else Error message

let release_lease lease = Atomic.set lease false

let acquire_arena_lease ?scope (arena : task_arena) =
  try
    let borrowed =
      Option.fold ~none:false
        ~some:(fun scope -> suspension_owns_arena_raw scope arena.handle_)
        scope
    in
    if borrowed then
      if Atomic.get arena.arena_lease_ then Ok true
      else Error "native suspended arena lost its runtime lease"
    else
      Result.map
        (fun () -> false)
        (acquire_lease arena.arena_lease_ "native task arena is already active")
  with Failure message | Invalid_argument message -> Error message

let release_arena_lease arena borrowed =
  if not borrowed then release_lease arena.arena_lease_

let execute_report_internal ?scope ?retained ?consumed ?entered ?task_binding
    ?source_callback_state ?consumed_after_source ?(max_frame_bytes = 1_048_576)
    ?(max_call_depth = 128)
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
    || Option.is_some task_binding
       && (Option.is_none retained || Option.is_none consumed
         || Option.is_none entered)
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
      else if
        List.length unwind_functions
        <> function_count + 1
           + Image.private_function_count image
           + List.length (Image.code_owner_bindings image)
      then
        error_report
          "native program image has inconsistent unwind function metadata"
      else
        let global_bytes = Image.global_bytes image in
        let literal_bytes = Image.literal_bytes image in
        let metadata_bytes = Image.arena_metadata_bytes image in
        let arena_bytes = Image.arena_bytes image in
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
        else if
          global_bytes = 0 && literal_bytes = 0 && arena_bytes <> 0
          && Image.code_owner_bindings image = []
        then
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
                let activation =
                  match task_binding with
                  | None -> Ok ()
                  | Some binding -> (
                      let required_arena_bytes =
                        binding.task_required_arena_bytes_
                      in
                      if required_arena_bytes <> arena_bytes then
                        Error
                          "native task fragment arena extent disagrees with \
                           its sealed image"
                      else
                        let ( let* ) = Result.bind in
                        let* () = Image.check_task_request image in
                        let* () =
                          bind_task_budget binding.task_arena_
                            binding.task_budget_identity_
                        in
                        let* () = Image.check_task_activation image in
                        if Image.code_owner_bindings image = [] then Ok ()
                        else
                          match retained with
                          | None ->
                              Error
                                "native task entries require their retained \
                                 mapped image"
                          | Some handle ->
                              let prior =
                                Atomic.get binding.task_arena_.code_mappings_
                              in
                              let canonical_bytes =
                                List.fold_left
                                  (fun size (_, image, _) ->
                                    size + Image.code_bytes image)
                                  0 prior
                              in
                              if
                                Image.code_bytes image
                                > 16_777_216 - canonical_bytes
                              then
                                Error
                                  "persistent native code exceeds the host \
                                   mapping bound"
                              else
                                let canonical =
                                  bind_task_entries handle
                                    ( binding.task_arena_.handle_,
                                      required_arena_bytes )
                                    scope
                                in
                                Atomic.set binding.task_arena_.code_mappings_
                                  ((handle, image, canonical) :: prior);
                                Ok ())
                in
                match activation with
                | Error message -> error_report message
                | Ok () -> (
                    let scalar_capture = ref None in
                    let generated_capture = ref None in
                    let generation = Image.generation image in
                    let generation_state =
                      Option.map
                        (fun target ->
                          match
                            Ir.Integer_interpreter.native_generation_limits
                              target
                          with
                          | Error message -> invalid_arg message
                          | Ok (active, available, capacity) ->
                              ( target,
                                active,
                                available,
                                capacity,
                                generated_capture,
                                source_callback_state,
                                match
                                  Ir.Integer_interpreter
                                  .native_generation_byte_budget target
                                with
                                | Ok bytes -> bytes
                                | Error message -> invalid_arg message ))
                        generation
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
                        match (consumed, task_binding) with
                        | None, None ->
                            execute_retained_program (Option.get retained)
                              limits
                        | Some consumed, None ->
                            execute_retained_budget_program
                              (Option.get retained) limits consumed
                              (Option.get entered)
                        | Some consumed, Some binding -> (
                            let task =
                              ( binding.task_arena_.handle_,
                                binding.task_required_arena_bytes_,
                                Option.get generation_state,
                                (scope, binding.task_budget_identity_) )
                            in
                            match Image.scalar_program image with
                            | None ->
                                execute_retained_budget_task_program
                                  (Option.get retained) task limits consumed
                                  (Option.get entered)
                            | Some (Image.Internal_binding program) ->
                                let report, capture =
                                  execute_retained_budget_scalar_program
                                    (Option.get retained) task limits consumed
                                    (Option.get entered, program)
                                in
                                scalar_capture :=
                                  Option.map
                                    (fun value -> Binding_capture value)
                                    capture;
                                report
                            | Some (Image.Dimension program) ->
                                let report, capture =
                                  execute_retained_budget_scalar_program
                                    (Option.get retained) task limits consumed
                                    (Option.get entered, program)
                                in
                                scalar_capture :=
                                  Option.map
                                    (fun value -> Dimension_capture value)
                                    capture;
                                report
                            | Some (Image.Offset program) ->
                                let report, capture =
                                  execute_retained_budget_scalar_program
                                    (Option.get retained) task limits consumed
                                    (Option.get entered, program)
                                in
                                scalar_capture :=
                                  Option.map
                                    (fun value -> Offset_capture value)
                                    capture;
                                report)
                        | None, Some _ ->
                            invalid_arg
                              "native task fragment requires a cumulative \
                               budget"
                      else if Image.has_output image then
                        let global_image = Image.global_image image in
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
                          ( global_bytes,
                            literal_bytes,
                            metadata_bytes,
                            global_image )
                      else
                        let status =
                          if arena_bytes > 0 then
                            let global_image = Image.global_image image in
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
                    let prior_steps, prior_output, prior_work =
                      match consumed_after_source with
                      | None -> (prior_steps, prior_output, prior_work)
                      | Some observe -> observe ()
                    in
                    let available_output = max_output_bytes - prior_output in
                    let available_work = max_output_work - prior_work in
                    let output_status_valid =
                      captured_length <= available_output
                      && work >= 0 && work <= available_work
                      && captured_length <= work
                      && Int64.compare executed_steps (Int64.of_int prior_steps)
                         >= 0
                      && ((not
                             (Int64.equal executed_steps
                                (Int64.of_int prior_steps)))
                         || (captured_length = 0 && work = 0))
                      &&
                      if Int64.equal kind 11L then
                        atomic_fault || captured_length = available_output
                      else if Int64.equal kind 12L then work = available_work
                      else true
                    in
                    if not output_status_valid then
                      error_report
                        "native program status integrity failure: output \
                         counters disagree with the returned fault status"
                    else
                      match decoded with
                      | Ok outcome_ ->
                          (match (generation, !generated_capture) with
                          | Some target, Some capture -> (
                              match
                                Ir.Integer_interpreter
                                .complete_native_generation ?scope target
                                  capture
                              with
                              | Ok () -> ()
                              | Error message -> invalid_arg message)
                          | None, None -> ()
                          | _ ->
                              invalid_arg
                                "native generation returned without its \
                                 original capture");
                          (match task_binding with
                          | Some binding ->
                              let capture =
                                match outcome_ with
                                | Image.Completed completed ->
                                    Option.map
                                      (fun value -> (image, value))
                                      completed.captured_data
                                | Image.Fault _ -> None
                              in
                              Atomic.set binding.task_arena_.data_capture_
                                capture;
                              let internal =
                                match outcome_ with
                                | Image.Completed { final_value = Some _; _ } ->
                                    Option.map
                                      (fun value -> (image, value))
                                      !scalar_capture
                                | _ -> None
                              in
                              Atomic.set binding.task_arena_.scalar_capture_
                                internal
                          | None -> ());
                          {
                            outcome_ = Ok outcome_;
                            output_bytes_ = captured;
                            output_work_ = work;
                            value_captured_ = not (Int64.equal value_site 0L);
                            generation_capture_ = !generated_capture;
                          }
                      | Error message ->
                          error_report
                            ("native program status integrity failure: "
                           ^ message))
              with Failure message | Invalid_argument message ->
                error_report message)
          | _ ->
              error_report
                "native program status ABI does not match this process"

let retained_identity_prefix ?(max_global_bytes = 1_048_576)
    ?(max_literal_bytes = 1_048_576)
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
          + Image.private_function_count image
          + List.length (Image.code_owner_bindings image)
  then Error "retained native image has inconsistent callable metadata"
  else
    match abi_code with
    | None -> Error "retained native status ABI does not match this host"
    | Some abi_code ->
        Ok
          ( Image.code image,
            Array.of_list (Image.windows_unwind_functions image),
            abi_code,
            Image.entry_stack_bytes image,
            Image.global_bytes image,
            Image.literal_bytes image,
            Image.arena_metadata_bytes image )

let retained_identity ?max_global_bytes ?max_literal_bytes
    ?max_active_stack_bytes image =
  Result.map
    (fun ( code,
           functions,
           abi_code,
           entry_stack_bytes,
           global_bytes,
           literal_bytes,
           metadata_bytes ) ->
      ( code,
        functions,
        abi_code,
        entry_stack_bytes,
        (global_bytes, literal_bytes, metadata_bytes, Image.global_image image)
      ))
    (retained_identity_prefix ?max_global_bytes ?max_literal_bytes
       ?max_active_stack_bytes image)

let retained_task_identity ?max_global_bytes ?max_literal_bytes
    ?max_active_stack_bytes image =
  Result.map
    (fun ( code,
           functions,
           abi_code,
           entry_stack_bytes,
           global_bytes,
           literal_bytes,
           metadata_bytes ) ->
      ( code,
        functions,
        abi_code,
        entry_stack_bytes,
        (global_bytes, literal_bytes, metadata_bytes, Image.arena_bytes image),
        Array.of_list (Image.code_owner_bindings image),
        Array.of_list (Image.function_slot_bindings image) ))
    (retained_identity_prefix ?max_global_bytes ?max_literal_bytes
       ?max_active_stack_bytes image)

let retain_identity ~storage ~retain_host image identity =
  try
    let handle_ = retain_host identity in
    Ok
      {
        image_ = image;
        handle_;
        storage_ = storage;
        lease_ = Atomic.make false;
        revoked_ = Atomic.make false;
        released_ = Atomic.make false;
      }
  with Failure message | Invalid_argument message -> Error message

let retain_checked ?max_global_bytes ?max_literal_bytes ?max_active_stack_bytes
    ~storage ~retain_host image =
  match
    retained_identity ?max_global_bytes ?max_literal_bytes
      ?max_active_stack_bytes image
  with
  | Error _ as error -> error
  | Ok identity -> retain_identity ~storage ~retain_host image identity

let retain ?max_global_bytes ?max_literal_bytes ?max_active_stack_bytes image =
  match Image.task_snapshot image with
  | Some _ ->
      Error
        "native task fragment requires retain_task_fragment and its shared \
         task arena"
  | None ->
      retain_checked ?max_global_bytes ?max_literal_bytes
        ?max_active_stack_bytes ~storage:Private_storage
        ~retain_host:retain_program image

let create_task_arena ?(max_arena_bytes = hard_max_arena_bytes) layout =
  if max_arena_bytes <= 0 || max_arena_bytes > hard_max_arena_bytes then
    Error
      (Printf.sprintf
         "native task arena max_arena_bytes must be between 1 and %d"
         hard_max_arena_bytes)
  else
    match platform () with
    | Unsupported ->
        Error
          "native execution requires Windows or Linux x86-64 with 64-bit \
           pointers"
    | Windows_x86_64 | Linux_x86_64 -> (
        try
          let handle_ = create_task_arena_handle max_arena_bytes in
          match Task_storage.claim_task_arena layout with
          | Ok () ->
              Ok
                {
                  layout_ = layout;
                  handle_;
                  max_arena_bytes_ = max_arena_bytes;
                  admitted_bytes_ = Atomic.make 0;
                  budget_owner_ = Atomic.make None;
                  arena_lease_ = Atomic.make false;
                  arena_revoked_ = Atomic.make false;
                  arena_released_ = Atomic.make false;
                  code_mappings_ = Atomic.make [];
                  data_capture_ = Atomic.make None;
                  scalar_capture_ = Atomic.make None;
                  data_comparison_owner_ = ref ();
                }
          | Error message ->
              let release_error =
                try
                  release_task_arena_handle handle_;
                  None
                with Failure detail | Invalid_argument detail -> Some detail
              in
              Error
                (match release_error with
                | None -> message
                | Some detail ->
                    message ^ "; temporary arena release: " ^ detail)
        with Failure message | Invalid_argument message -> Error message)

let release_task_arena arena =
  if Atomic.get arena.arena_released_ then Ok ()
  else
    match
      acquire_lease arena.arena_lease_ "native task arena is already active"
    with
    | Error _ as error -> error
    | Ok () ->
        Fun.protect
          ~finally:(fun () -> release_lease arena.arena_lease_)
          (fun () ->
            if Atomic.get arena.arena_released_ then Ok ()
            else
              try
                release_task_arena_handle arena.handle_;
                List.iter
                  (fun (handle, _, _) -> release_program handle)
                  (Atomic.get arena.code_mappings_);
                Atomic.set arena.code_mappings_ [];
                Atomic.set arena.arena_revoked_ true;
                Atomic.set arena.arena_released_ true;
                Ok ()
              with
              | Invalid_argument message -> Error message
              | Failure message ->
                  Atomic.set arena.arena_revoked_ true;
                  Error message)

let admit_task_snapshot_locked ?scope arena snapshot =
  if not (Task_storage.task_snapshot_matches_layout snapshot arena.layout_) then
    Error "native task fragment belongs to another task arena layout"
  else
    let required_arena_bytes =
      Task_storage.task_snapshot_arena_bytes snapshot
    in
    if required_arena_bytes > arena.max_arena_bytes_ then
      Error
        "native task fragment storage exceeds its reserved task arena capacity"
    else
      let admitted = Atomic.get arena.admitted_bytes_ in
      if required_arena_bytes < admitted then
        Error
          "native task fragment snapshot precedes already admitted task storage"
      else if required_arena_bytes = admitted then Ok required_arena_bytes
      else
        try
          let observed =
            admit_task_arena arena.handle_ admitted required_arena_bytes
              (Task_storage.task_snapshot_initializations_since snapshot
                 ~arena_prefix_bytes:admitted)
              scope
          in
          if observed <> required_arena_bytes then
            Error
              "native task arena admission returned an inconsistent prefix \
               length"
          else (
            Atomic.set arena.admitted_bytes_ observed;
            Ok observed)
        with Failure message | Invalid_argument message -> Error message

let finish_task_internal_binding ?scope arena image =
  let ( let* ) = Result.bind in
  let* borrowed = acquire_arena_lease ?scope arena in
  Fun.protect
    ~finally:(fun () -> release_arena_lease arena borrowed)
    (fun () ->
      let reached = Atomic.get arena.scalar_capture_ in
      match
        (Image.task_snapshot image, Image.internal_binding image, reached)
      with
      | Some snapshot, Some _, Some (entered, Binding_capture captured)
        when (not (Atomic.get arena.arena_revoked_))
             && entered == image
             && Task_storage.task_snapshot_matches_layout snapshot arena.layout_
             && Atomic.compare_and_set arena.scalar_capture_ reached None ->
          Ok captured
      | _ ->
          Error
            "native scalar capture has another original image, kind or arena")

let finish_task_dimension ?scope arena image =
  let ( let* ) = Result.bind in
  let* borrowed = acquire_arena_lease ?scope arena in
  Fun.protect
    ~finally:(fun () -> release_arena_lease arena borrowed)
    (fun () ->
      let reached = Atomic.get arena.scalar_capture_ in
      match (Image.task_snapshot image, Image.dimension image, reached) with
      | Some snapshot, Some _, Some (entered, Dimension_capture captured)
        when (not (Atomic.get arena.arena_revoked_))
             && entered == image
             && Task_storage.task_snapshot_matches_layout snapshot arena.layout_
             && Atomic.compare_and_set arena.scalar_capture_ reached None ->
          Ok captured
      | _ ->
          Error
            "native scalar capture has another original image, kind or arena")

let finish_task_offset ?scope arena image =
  let ( let* ) = Result.bind in
  let* borrowed = acquire_arena_lease ?scope arena in
  Fun.protect
    ~finally:(fun () -> release_arena_lease arena borrowed)
    (fun () ->
      let reached = Atomic.get arena.scalar_capture_ in
      match (Image.task_snapshot image, Image.offset image, reached) with
      | Some snapshot, Some _, Some (entered, Offset_capture captured)
        when (not (Atomic.get arena.arena_revoked_))
             && entered == image
             && Task_storage.task_snapshot_matches_layout snapshot arena.layout_
             && Atomic.compare_and_set arena.scalar_capture_ reached None ->
          Ok captured
      | _ ->
          Error
            "native scalar capture has another original image, kind or arena")

let finish_task_data_default ?scope ?max_copy_bytes arena image captured
    ~max_copy_steps =
  let failure message = (Error message, 0) in
  match acquire_arena_lease ?scope arena with
  | Error message -> failure message
  | Ok borrowed ->
      Fun.protect
        ~finally:(fun () -> release_arena_lease arena borrowed)
        (fun () ->
          let reached = Atomic.get arena.data_capture_ in
          match (Image.task_snapshot image, Image.data_default image) with
          | Some snapshot, Some original
            when (not (Atomic.get arena.arena_revoked_))
                 && Task_storage.task_snapshot_matches_layout snapshot
                      arena.layout_
                 && Ir.Saved_parameter_value.same original captured
                 && Option.fold ~none:false
                      ~some:(fun (entered, value) ->
                        entered == image
                        && Ir.Saved_parameter_value.same value captured)
                      reached
                 && Atomic.compare_and_set arena.data_capture_ reached None -> (
              let data =
                Option.get (Ir.Saved_parameter_value.data_source original)
              in
              match Task_storage.find_saved_data snapshot data with
              | None ->
                  failure
                    "HCIRVM0026: native saved data lost its original task \
                     descriptor"
              | Some descriptor -> (
                  let capture_comparison value =
                    let offset =
                      read_task_default_address_offset arena.handle_
                        (Atomic.get arena.admitted_bytes_, descriptor)
                        scope
                    in
                    Ir.Saved_parameter_value.with_native_data_comparison
                      ~owner:arena.data_comparison_owner_ ~offset
                      ~live:(fun () -> not (Atomic.get arena.arena_revoked_))
                      value
                  in
                  if not (Image.data_default_has_misc_data image) then
                    try (capture_comparison original, 0)
                    with Failure message | Invalid_argument message ->
                      failure message
                  else
                    let attempted_work = ref 0 in
                    try
                      let code, work, bytes =
                        read_task_default_string arena.handle_
                          ( Atomic.get arena.admitted_bytes_,
                            descriptor,
                            max_copy_steps,
                            min
                              (Task_storage.saved_literal_remaining snapshot)
                              (Option.value ~default:hard_max_literal_bytes
                                 max_copy_bytes) )
                          scope
                      in
                      attempted_work := work;
                      let result =
                        let ( let* ) = Result.bind in
                        let* () =
                          match code with
                          | 0 -> Ok ()
                          | 1 ->
                              Error
                                "HCIRVM0007: saved string default copy exceeds \
                                 the initializer work limit"
                          | 2 ->
                              Error
                                "HCIRVM0011: saved string default copy exceeds \
                                 the cumulative literal byte limit"
                          | 3 ->
                              Error
                                "HCIRVM0019: saved string default has no \
                                 terminator in its original object"
                          | 4 ->
                              Error
                                "HCIRVM0012: saved string default reads an \
                                 uninitialized original byte"
                          | _ ->
                              Error
                                "HCIRVM0026: native saved string returned an \
                                 invalid capture status"
                        in
                        let* snapshot, offset =
                          Task_storage.append_task_saved_string snapshot ~data
                            ~bytes
                          |> Result.map_error (fun errors ->
                              errors
                              |> List.map (fun (error : Task_storage.error) ->
                                  error.code ^ ": " ^ error.message)
                              |> String.concat "; ")
                        in
                        let* prefix =
                          admit_task_snapshot_locked ?scope arena snapshot
                        in
                        bind_task_default_string arena.handle_
                          (prefix, descriptor, offset, String.length bytes)
                          scope;
                        let* value =
                          Ir.Saved_parameter_value.with_string_default ~bytes
                            original
                        in
                        capture_comparison value
                      in
                      (result, work)
                    with Failure message | Invalid_argument message ->
                      (Error message, !attempted_work)))
          | _ ->
              failure
                "HCIRVM0026: native saved data has another original image, \
                 capture or arena")

let allocate_task_static ?scope ?max_global_bytes arena request =
  let module Request = Driver.Integer_task.Native_static_allocation in
  let ( let* ) = Result.bind in
  let* borrowed = acquire_arena_lease ?scope arena in
  Fun.protect
    ~finally:(fun () -> release_arena_lease arena borrowed)
    (fun () ->
      if Atomic.get arena.arena_revoked_ then
        Error "native task arena has been released"
      else
        let* () = Request.check request in
        let* reservation =
          Task_storage.reserve_static arena.layout_ request
          |> Result.map_error (fun errors ->
              errors
              |> List.map (fun (error : Task_storage.error) ->
                  error.code ^ ": " ^ error.message)
              |> String.concat "; ")
        in
        let required =
          Task_storage.static_reservation_arena_bytes reservation
        in
        let admitted = Atomic.get arena.admitted_bytes_ in
        if
          Task_storage.task_layout_global_bytes arena.layout_
          > Option.value ~default:hard_max_global_bytes max_global_bytes
        then
          Error
            "HCBACK0001: native source catalogs exceed the cumulative global \
             byte limit"
        else if required > arena.max_arena_bytes_ then
          Error "HCBACK0001: native static exceeds reserved arena capacity"
        else if required < admitted then
          Error "native static reservation precedes already admitted storage"
        else
          let* () =
            Task_storage.check_static_reservation reservation
              ~layout:arena.layout_ ~request
          in
          let* () = Request.claim request in
          try
            let observed =
              admit_task_arena arena.handle_ admitted required
                (Task_storage.static_reservation_initializations_since
                   reservation ~arena_prefix_bytes:admitted)
                scope
            in
            if observed <> required then (
              Atomic.set arena.arena_revoked_ true;
              Error
                "native static allocation returned an inconsistent arena prefix")
            else (
              Atomic.set arena.admitted_bytes_ observed;
              Ok ())
          with Failure message | Invalid_argument message -> Error message)

let copy_task_static ?scope arena request =
  let module Request = Driver.Integer_task.Native_static_copy in
  let ( let* ) = Result.bind in
  let* borrowed = acquire_arena_lease ?scope arena in
  Fun.protect
    ~finally:(fun () -> release_arena_lease arena borrowed)
    (fun () ->
      if Atomic.get arena.arena_revoked_ then
        Error "native task arena has been released"
      else
        let* () = Request.check request in
        let* copy =
          Task_storage.prepare_static_copy arena.layout_ request
            ~admitted_arena_bytes:(Atomic.get arena.admitted_bytes_)
        in
        let* () =
          Task_storage.check_static_copy copy ~layout:arena.layout_ ~request
        in
        let payload = Task_storage.static_copy_payload copy in
        let _, _, _, bytes = payload in
        let* () = Request.claim request in
        try
          let observed = copy_task_static_bytes arena.handle_ payload scope in
          if observed = String.length bytes then Ok ()
          else (
            Atomic.set arena.arena_revoked_ true;
            Error "native static copy returned an inconsistent byte count")
        with Failure message | Invalid_argument message -> Error message)

let retain_task_fragment ?scope ?max_global_bytes ?max_literal_bytes
    ?max_active_stack_bytes arena image =
  match Image.task_snapshot image with
  | None -> Error "ordinary native image has no shared task storage snapshot"
  | Some snapshot -> (
      match acquire_arena_lease ?scope arena with
      | Error _ as error -> error
      | Ok borrowed ->
          Fun.protect
            ~finally:(fun () -> release_arena_lease arena borrowed)
            (fun () ->
              if Atomic.get arena.arena_revoked_ then
                Error "native task arena has been released"
              else if
                not
                  (Task_storage.task_snapshot_matches_layout snapshot
                     arena.layout_)
              then
                Error
                  "native task fragment belongs to another task arena layout"
              else if
                Task_storage.task_snapshot_global_bytes snapshot
                <> Image.global_bytes image
                || Task_storage.task_snapshot_literal_bytes snapshot
                   <> Image.literal_bytes image
              then
                Error
                  "native task fragment logical storage disagrees with its \
                   snapshot"
              else if
                Task_storage.task_snapshot_arena_bytes snapshot
                <> Image.arena_bytes image
              then
                Error
                  "native task fragment arena extent disagrees with its task \
                   snapshot"
              else
                let ( let* ) = Result.bind in
                let* () = Image.check_task_request image in
                let* identity =
                  retained_task_identity ?max_global_bytes ?max_literal_bytes
                    ?max_active_stack_bytes image
                in
                let* required_arena_bytes =
                  admit_task_snapshot_locked ?scope arena snapshot
                in
                retain_identity
                  ~storage:(Shared_task_storage { arena; required_arena_bytes })
                  ~retain_host:retain_task_fragment_program image identity))

let release retained =
  if Atomic.get retained.released_ then Ok ()
  else
    match
      acquire_lease retained.lease_ "retained native image is already active"
    with
    | Error _ as error -> error
    | Ok () ->
        Fun.protect
          ~finally:(fun () -> release_lease retained.lease_)
          (fun () ->
            if Atomic.get retained.released_ then Ok ()
            else
              try
                (match retained.storage_ with
                | Shared_task_storage { arena; _ }
                  when List.exists
                         (fun (handle, _, _) -> handle == retained.handle_)
                         (Atomic.get arena.code_mappings_) -> ()
                | Private_storage | Shared_task_storage _ ->
                    release_program retained.handle_);
                Atomic.set retained.revoked_ true;
                Atomic.set retained.released_ true;
                Ok ()
              with
              | Invalid_argument message -> Error message
              | Failure message ->
                  Atomic.set retained.revoked_ true;
                  Error message)

let execute_retained_report ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes ?max_global_bytes ?max_literal_bytes
    ?max_output_bytes ?max_output_work ~max_steps retained =
  if Atomic.get retained.revoked_ then
    error_report "retained native image has been released"
  else
    match
      acquire_lease retained.lease_ "retained native image is already active"
    with
    | Error message -> error_report message
    | Ok () ->
        Fun.protect
          ~finally:(fun () -> release_lease retained.lease_)
          (fun () ->
            if Atomic.get retained.revoked_ then
              error_report "retained native image has been released"
            else
              match retained.storage_ with
              | Shared_task_storage _ ->
                  error_report
                    "native task fragment requires its cumulative shared task \
                     budget"
              | Private_storage ->
                  execute_report_internal ~retained:retained.handle_
                    ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
                    ?max_global_bytes ?max_literal_bytes ?max_output_bytes
                    ?max_output_work ~max_steps retained.image_)

type budget_state = {
  steps_ : int;
  bytes_ : int;
  work_ : int;
  chunks_ : string list;
  error_ : string option;
}

type budget = {
  identity_ : unit ref;
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
        identity_ = ref ();
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

let budget_output_bytes budget = (budget_progress budget).output_bytes

let suspension_owns_budget scope budget =
  try Ok (suspension_owns_budget_raw scope budget.identity_)
  with Failure message | Invalid_argument message -> Error message

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

let capture_suffix chunks length =
  let bytes = Bytes.create length in
  let rec copy remaining = function
    | _ when remaining = 0 -> ()
    | [] -> invalid_arg "native output history lost its admitted suffix"
    | chunk :: rest ->
        let count = min remaining (String.length chunk) in
        Bytes.blit_string chunk
          (String.length chunk - count)
          bytes (remaining - count) count;
        copy (remaining - count) rest
  in
  copy length chunks;
  Bytes.to_string bytes

let execute_retained_budget_report ?scope ?max_activation_steps ?max_frame_bytes
    ?max_call_depth ?max_active_stack_bytes ?max_global_bytes ?max_literal_bytes
    ?source_callback budget retained =
  let callback_exception_seen = ref false in
  let callback_exception_value = ref Not_found in
  let source_callback_state =
    Option.map
      (fun callback ->
        ( (fun owner ->
            let scope, contents = Ir.Native_source_suspension.open_raw owner in
            let checkpoint = create_source_checkpoint scope budget.identity_ in
            let steps, work, output, capture =
              consume_source_checkpoint checkpoint scope budget.identity_
            in
            let state = Atomic.get budget.state_ in
            if
              Int64.compare steps (Int64.of_int state.steps_) < 0
              || Int64.compare steps (Int64.of_int budget.max_steps_) > 0
              || work < 0
              || work > budget.max_output_work_ - state.work_
              || String.length output > budget.max_output_bytes_ - state.bytes_
            then
              invalid_arg
                "native source checkpoint exceeds its cumulative budget";
            (match Image.generation retained.image_ with
            | None ->
                invalid_arg
                  "native source checkpoint has no original generation"
            | Some generation -> (
                match
                  Ir.Integer_interpreter.admit_native_generation_prefix
                    generation ~scope capture
                with
                | Ok () -> ()
                | Error message -> invalid_arg message));
            Atomic.set budget.state_
              {
                steps_ = Int64.to_int steps;
                bytes_ = state.bytes_ + String.length output;
                work_ = state.work_ + work;
                chunks_ = append_capture output state.chunks_;
                error_ = state.error_;
              };
            callback scope contents),
          callback_exception_seen,
          callback_exception_value,
          budget.identity_ ))
      source_callback
  in
  let admission () =
    match scope with
    | None ->
        Result.map
          (fun () -> (false, budget.max_steps_))
          (acquire_lease budget.active_
             "retained native budget is already active")
    | Some scope ->
        let ( let* ) = Result.bind in
        let* () =
          match retained.storage_ with
          | Private_storage ->
              Error "scoped native child requires its original task image"
          | Shared_task_storage _ -> Ok ()
        in
        let* owned = suspension_owns_budget scope budget in
        if not owned then
          Error "native source scope belongs to another cumulative budget"
        else if not (Atomic.get budget.active_) then
          Error "native suspended budget lost its runtime lease"
        else
          let* remaining, _, _, _ = Ir.Native_source_suspension.limits scope in
          Ok (true, remaining)
  in
  if
    Option.fold ~none:false ~some:(fun limit -> limit <= 0) max_activation_steps
  then error_report "native activation step allowance must be positive"
  else
    match admission () with
    | Error message -> error_report message
    | Ok (borrowed_budget, physical_remaining) ->
        Fun.protect
          ~finally:(fun () ->
            if not borrowed_budget then Atomic.set budget.active_ false)
          (fun () ->
            let state = Atomic.get budget.state_ in
            match state.error_ with
            | Some message -> error_report message
            | None when Atomic.get retained.revoked_ ->
                error_report "retained native image has been released"
            | None -> (
                let run task_binding =
                  let entered = ref false in
                  let verified = ref false in
                  let poisoned =
                    {
                      state with
                      error_ =
                        Some
                          "retained native budget is unavailable after an \
                           unverified activation";
                    }
                  in
                  try
                    let report =
                      execute_report_internal ?scope ~retained:retained.handle_
                        ?source_callback_state
                        ~consumed_after_source:(fun () ->
                          let current = Atomic.get budget.state_ in
                          (current.steps_, current.bytes_, current.work_))
                        ~consumed:(state.steps_, state.bytes_, state.work_)
                        ~entered ?task_binding ?max_frame_bytes ?max_call_depth
                        ?max_active_stack_bytes ?max_global_bytes
                        ?max_literal_bytes
                        ~max_steps:
                          (state.steps_
                          + min physical_remaining
                              (min
                                 (budget.max_steps_ - state.steps_)
                                 (Option.value ~default:budget.max_steps_
                                    max_activation_steps)))
                        ~max_output_bytes:budget.max_output_bytes_
                        ~max_output_work:budget.max_output_work_ retained.image_
                    in
                    (match report.outcome_ with
                    | Error _ ->
                        if !entered then
                          Atomic.set budget.state_
                            {
                              (Atomic.get budget.state_) with
                              error_ = poisoned.error_;
                            }
                    | Ok outcome ->
                        let steps_ =
                          match outcome with
                          | Image.Completed execution ->
                              execution.executed_steps
                          | Image.Fault fault -> fault.executed_steps
                        in
                        let current = Atomic.get budget.state_ in
                        let next =
                          {
                            steps_;
                            bytes_ =
                              current.bytes_
                              + String.length report.output_bytes_;
                            work_ = current.work_ + report.output_work_;
                            chunks_ =
                              append_capture report.output_bytes_
                                current.chunks_;
                            error_ = current.error_;
                          }
                        in
                        Atomic.set budget.state_ next;
                        verified := true);
                    if !callback_exception_seen then
                      raise !callback_exception_value;
                    let current = Atomic.get budget.state_ in
                    let length = current.bytes_ - state.bytes_ in
                    let work = current.work_ - state.work_ in
                    if
                      length = String.length report.output_bytes_
                      && work = report.output_work_
                    then report
                    else
                      {
                        report with
                        output_bytes_ = capture_suffix current.chunks_ length;
                        output_work_ = work;
                      }
                  with exception_ ->
                    if !entered && not !verified then
                      Atomic.set budget.state_
                        {
                          (Atomic.get budget.state_) with
                          error_ = poisoned.error_;
                        };
                    raise exception_
                in
                let run_with_retained_lease () =
                  if Atomic.get retained.revoked_ then
                    error_report "retained native image has been released"
                  else
                    match retained.storage_ with
                    | Private_storage -> run None
                    | Shared_task_storage { arena; required_arena_bytes } -> (
                        match acquire_arena_lease ?scope arena with
                        | Error message -> error_report message
                        | Ok borrowed ->
                            Fun.protect
                              ~finally:(fun () ->
                                release_arena_lease arena borrowed)
                              (fun () ->
                                if Atomic.get arena.arena_revoked_ then
                                  error_report
                                    "native task arena has been released"
                                else if
                                  Atomic.get arena.admitted_bytes_
                                  < required_arena_bytes
                                then
                                  error_report
                                    "native task fragment requires unadmitted \
                                     task storage"
                                else
                                  match Atomic.get arena.budget_owner_ with
                                  | Some owner when owner != budget.identity_ ->
                                      error_report
                                        "native task arena belongs to another \
                                         cumulative budget"
                                  | None | Some _ ->
                                      run
                                        (Some
                                           {
                                             task_arena_ = arena;
                                             task_required_arena_bytes_ =
                                               required_arena_bytes;
                                             task_budget_identity_ =
                                               budget.identity_;
                                           })))
                in
                match
                  acquire_lease retained.lease_
                    "retained native image is already active"
                with
                | Error message -> error_report message
                | Ok () ->
                    Fun.protect
                      ~finally:(fun () -> release_lease retained.lease_)
                      run_with_retained_lease))

let execute_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?max_global_bytes ?max_literal_bytes ?max_output_bytes ?max_output_work
    ~max_steps image =
  match Image.task_snapshot image with
  | Some _ ->
      error_report
        "native task fragment requires retain_task_fragment and its shared \
         task arena"
  | None ->
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
