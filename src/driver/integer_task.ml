module VM = Ir.Integer_interpreter

type native_source_callback =
  Ir.Native_source_suspension.t ->
  string ->
  ((int64, Common.Diagnostic.t list) result, string) result

let native_source_callback ~generation ~valid handler =
  Option.map
    (fun execute scope contents ->
      if not (valid ()) then
        Error "native source callback requires its entered original request"
      else
        VM.with_native_source_suspension generation ~scope (fun _ ->
            execute contents))
    handler

module Native_dispatch = struct
  type word = I64 of int64 | U64 of int64
  type capture = Unchanged | Captured of word option
  type entry_state = Offered | Claiming | Entered | Closed

  type initializer_request = {
    initializer_task : VM.task_state;
    initializer_attempt : VM.initializer_attempt;
    initializer_execution : Ir.Initializer_fragment_program.execution;
    initializer_program_ : Ir.Initializer_fragment_program.t;
    initializer_generation_ : VM.native_generation;
    initializer_source_handler : VM.stream_exe_print option;
    initializer_domain : Domain.id;
    initializer_state : entry_state Atomic.t;
  }

  type command_request = {
    command_task : VM.task_state;
    command_program_ : Integer_unit.compiled;
    command_generation_ : VM.native_generation;
    command_source_handler : VM.stream_exe_print option;
    command_domain : Domain.id;
    command_state : entry_state Atomic.t;
    command_attempt : VM.native_program_attempt option Atomic.t;
  }

  type t = {
    execute_initializer :
      initializer_request -> (unit, Common.Diagnostic.t list) result;
    execute_command :
      command_request -> (capture, Common.Diagnostic.t list) result;
  }

  let initializer_generation request = request.initializer_generation_
  let command_generation request = request.command_generation_
  let initializer_program request = request.initializer_program_
  let command_program request = request.command_program_
  let owns_domain expected = Domain.self () = expected

  let check_initializer_request request =
    if not (owns_domain request.initializer_domain) then
      Error "native initializer request belongs to another execution domain"
    else if Atomic.get request.initializer_state <> Offered then
      Error "native initializer request was already entered or closed"
    else
      VM.check_native_task_initializer request.initializer_task
        request.initializer_attempt request.initializer_execution
        request.initializer_program_

  let claim_initializer_request request =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    if not (Atomic.compare_and_set request.initializer_state Offered Claiming)
    then Error "native initializer request was already claimed"
    else
      match
        VM.claim_native_task_initializer request.initializer_task
          request.initializer_attempt request.initializer_execution
          request.initializer_program_
      with
      | Ok () ->
          Atomic.set request.initializer_state Entered;
          Ok ()
      | Error message ->
          Atomic.set request.initializer_state Closed;
          Error message

  let initializer_function_source request link =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    match VM.task_native_function_source request.initializer_task link with
    | Some source -> Ok source
    | None ->
        Error "native initializer request has no exact admitted function source"

  let initializer_slot_binding request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    let program = request.initializer_program_ in
    VM.task_native_slot_binding request.initializer_task
      ~root_runtime_calls:
        (Ir.Initializer_fragment_program.runtime_calls program)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Initializer_fragment_program.initialization program))
      ~runtime_calls ~owner call

  let initializer_slot_address_binding request ~runtime_calls ~owner address =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    let program = request.initializer_program_ in
    VM.task_native_slot_address_binding request.initializer_task
      ~root_runtime_calls:
        (Ir.Initializer_fragment_program.runtime_calls program)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Initializer_fragment_program.initialization program))
      ~runtime_calls ~owner address

  let initializer_slot_address_refresh request binding =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    let program = request.initializer_program_ in
    VM.refresh_native_slot_address_binding request.initializer_task
      ~root_runtime_calls:
        (Ir.Initializer_fragment_program.runtime_calls program)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Initializer_fragment_program.initialization program))
      binding

  let initializer_provider_available request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    VM.task_native_provider_available request.initializer_task ~runtime_calls
      ~owner call

  let check_command_request request =
    if not (owns_domain request.command_domain) then
      Error "native command request belongs to another execution domain"
    else if Atomic.get request.command_state <> Offered then
      Error "native command request was already entered or closed"
    else
      let program = request.command_program_ in
      VM.check_native_task_program request.command_task
        ~runtime_calls:(Integer_unit.runtime_calls program)
        ~globals:(Integer_unit.globals program)
        ~initialization:(Integer_unit.initialization program)
        ~functions:(Integer_unit.functions program)
        (Integer_unit.entry program)

  let claim_command_request request =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    if not (Atomic.compare_and_set request.command_state Offered Claiming) then
      Error "native command request was already claimed"
    else
      let program = request.command_program_ in
      match
        VM.claim_native_task_program request.command_task
          ~runtime_calls:(Integer_unit.runtime_calls program)
          ~globals:(Integer_unit.globals program)
          ~initialization:(Integer_unit.initialization program)
          ~functions:(Integer_unit.functions program)
          (Integer_unit.entry program)
      with
      | Ok attempt ->
          Atomic.set request.command_attempt (Some attempt);
          Atomic.set request.command_state Entered;
          Ok ()
      | Error message ->
          Atomic.set request.command_state Closed;
          Error message

  let command_function_source request link =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    match VM.task_native_function_source request.command_task link with
    | Some source -> Ok source
    | None ->
        Error "native command request has no exact admitted function source"

  let command_slot_binding request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    let program = request.command_program_ in
    VM.task_native_slot_binding request.command_task
      ~root_runtime_calls:(Integer_unit.runtime_calls program)
      ~root_globals:(Integer_unit.globals program)
      ~runtime_calls ~owner call

  let command_slot_address_binding request ~runtime_calls ~owner address =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    let program = request.command_program_ in
    VM.task_native_slot_address_binding request.command_task
      ~root_runtime_calls:(Integer_unit.runtime_calls program)
      ~root_globals:(Integer_unit.globals program)
      ~runtime_calls ~owner address

  let command_slot_address_refresh request binding =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    let program = request.command_program_ in
    VM.refresh_native_slot_address_binding request.command_task
      ~root_runtime_calls:(Integer_unit.runtime_calls program)
      ~root_globals:(Integer_unit.globals program)
      binding

  let command_provider_available request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    VM.task_native_provider_available request.command_task ~runtime_calls ~owner
      call

  let initializer_parameter_default request ~globals ~header ~parameter prepared
      =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    VM.task_native_parameter_default request.initializer_task ~globals ~header
      ~parameter prepared

  let initializer_callback_default request ~globals ~pointer ~parameter prepared
      =
    let ( let* ) = Result.bind in
    let* () = check_initializer_request request in
    VM.task_native_callback_default request.initializer_task ~globals ~pointer
      ~parameter prepared

  let create_initializer ?(use_active_stream = true) ?stream_exe_print ~task
      ~attempt ~execution ~program () =
    {
      initializer_task = task;
      initializer_attempt = attempt;
      initializer_execution = execution;
      initializer_program_ = program;
      initializer_generation_ =
        VM.native_task_generation ~use_active_stream task;
      initializer_source_handler = stream_exe_print;
      initializer_domain = Domain.self ();
      initializer_state = Atomic.make Offered;
    }

  let command_parameter_default request ~globals ~header ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    VM.task_native_parameter_default request.command_task ~globals ~header
      ~parameter prepared

  let command_callback_default request ~globals ~pointer ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check_command_request request in
    VM.task_native_callback_default request.command_task ~globals ~pointer
      ~parameter prepared

  let create_command ?(use_active_stream = true) ?stream_exe_print ~task
      ~program () =
    {
      command_task = task;
      command_program_ = program;
      command_generation_ = VM.native_task_generation ~use_active_stream task;
      command_source_handler = stream_exe_print;
      command_domain = Domain.self ();
      command_state = Atomic.make Offered;
      command_attempt = Atomic.make None;
    }

  let initializer_entered request =
    Atomic.get request.initializer_state = Entered

  let command_entered request = Atomic.get request.command_state = Entered

  let initializer_source_callback request =
    native_source_callback ~generation:request.initializer_generation_
      ~valid:(fun () ->
        owns_domain request.initializer_domain && initializer_entered request)
      request.initializer_source_handler

  let command_source_callback request =
    native_source_callback ~generation:request.command_generation_
      ~valid:(fun () ->
        owns_domain request.command_domain && command_entered request)
      request.command_source_handler

  let command_attempt request = Atomic.get request.command_attempt
  let close_initializer request = Atomic.set request.initializer_state Closed
  let close_command request = Atomic.set request.command_state Closed
end

module Native_static_allocation = struct
  type phase = Offered | Claiming | Entered | Closed

  type request = {
    task : VM.task_state;
    allocation_ : Ir.Integer_static_allocation.t;
    view : Ir.Integer_globals.task_view;
    context_ : Ir.Integer_globals.t;
    domain : Domain.id;
    phase : phase Atomic.t;
  }

  type t = request -> (unit, Common.Diagnostic.t list) result

  let allocation request = request.allocation_
  let context request = request.context_

  let check request =
    if Domain.self () <> request.domain then
      Error "native static allocation belongs to another execution domain"
    else if Atomic.get request.phase <> Offered then
      Error "native static allocation was already claimed or closed"
    else
      VM.check_native_static_allocation request.task request.allocation_
        request.view

  let claim request =
    let ( let* ) = Result.bind in
    let* () = check request in
    if Atomic.compare_and_set request.phase Offered Claiming then (
      Atomic.set request.phase Entered;
      Ok ())
    else Error "native static allocation was already claimed"

  let create task allocation_ =
    let ( let* ) = Result.bind in
    let* view = VM.task_snapshot task in
    let* context_ =
      Ir.Integer_globals.static_allocation_context view allocation_
    in
    let request =
      {
        task;
        allocation_;
        view;
        context_;
        domain = Domain.self ();
        phase = Atomic.make Offered;
      }
    in
    let* () = check request in
    Ok request

  let entered request = Atomic.get request.phase = Entered
  let close request = Atomic.set request.phase Closed
end

module Native_static_initializer = struct
  type phase = Offered | Entered | Closed

  type request = {
    task : VM.task_state;
    program_ : Ir.Static_initializer_program.t;
    generation_ : VM.native_generation;
    source_handler : VM.stream_exe_print option;
    domain : Domain.id;
    phase : phase Atomic.t;
  }

  type t = request -> (unit, Common.Diagnostic.t list) result

  let generation request = request.generation_
  let program request = request.program_

  let check request =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Offered
    then
      Error
        "native static initializer belongs to another domain or was already \
         claimed"
    else VM.check_native_static_initializer request.task request.program_

  let claim request =
    let ( let* ) = Result.bind in
    let* () = check request in
    if Atomic.compare_and_set request.phase Offered Entered then Ok ()
    else Error "native static initializer was already claimed"

  let function_source request link =
    let ( let* ) = Result.bind in
    let* () = check request in
    match VM.task_native_function_source request.task link with
    | Some source -> Ok source
    | None ->
        Error "native static initializer lacks its admitted original callee"

  let slot_binding request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_binding request.task
      ~root_runtime_calls:
        (Ir.Static_initializer_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Static_initializer_program.initialization request.program_))
      ~runtime_calls ~owner call

  let slot_address_binding request ~runtime_calls ~owner address =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Static_initializer_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Static_initializer_program.initialization request.program_))
      ~runtime_calls ~owner address

  let slot_address_refresh request binding =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.refresh_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Static_initializer_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Static_initializer_program.initialization request.program_))
      binding

  let provider_available request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_provider_available request.task ~runtime_calls ~owner call

  let parameter_default request ~globals ~header ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_parameter_default request.task ~globals ~header ~parameter
      prepared

  let callback_default request ~globals ~pointer ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_callback_default request.task ~globals ~pointer ~parameter
      prepared

  let create ?(use_active_stream = true) ?stream_exe_print task program_ =
    {
      task;
      program_;
      generation_ = VM.native_task_generation ~use_active_stream task;
      source_handler = stream_exe_print;
      domain = Domain.self ();
      phase = Atomic.make Offered;
    }

  let entered request = Atomic.get request.phase = Entered

  let source_callback request =
    native_source_callback ~generation:request.generation_
      ~valid:(fun () -> Domain.self () = request.domain && entered request)
      request.source_handler

  let close request = Atomic.set request.phase Closed
end

module Native_default = struct
  type phase = Offered | Claiming | Entered | Closed

  type request = {
    task : VM.task_state;
    attempt : VM.default_attempt;
    program_ : Ir.Default_fragment_program.t;
    generation_ : VM.native_generation;
    source_handler : VM.stream_exe_print option;
    domain : Domain.id;
    phase : phase Atomic.t;
  }

  type t =
    request -> (Ir.Saved_parameter_value.t, Common.Diagnostic.t list) result

  let generation request = request.generation_
  let program request = request.program_

  let check request =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Offered
    then Error "native default belongs to another domain or was already claimed"
    else
      VM.check_native_task_default request.task request.attempt request.program_

  let claim request =
    let ( let* ) = Result.bind in
    let* () = check request in
    if Atomic.compare_and_set request.phase Offered Claiming then (
      match
        VM.claim_native_task_default request.task request.attempt
          request.program_
      with
      | Ok () ->
          Atomic.set request.phase Entered;
          Ok ()
      | Error _ as error ->
          Atomic.set request.phase Closed;
          error)
    else Error "native default was already claimed"

  let function_source request link =
    let ( let* ) = Result.bind in
    let* () = check request in
    match VM.task_native_function_source request.task link with
    | Some source -> Ok source
    | None -> Error "native default lacks its admitted original callee"

  let slot_binding request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_binding request.task
      ~root_runtime_calls:
        (Ir.Default_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Default_fragment_program.initialization request.program_))
      ~runtime_calls ~owner call

  let slot_address_binding request ~runtime_calls ~owner address =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Default_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Default_fragment_program.initialization request.program_))
      ~runtime_calls ~owner address

  let slot_address_refresh request binding =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.refresh_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Default_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Default_fragment_program.initialization request.program_))
      binding

  let provider_available request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_provider_available request.task ~runtime_calls ~owner call

  let parameter_default request ~globals ~header ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_parameter_default request.task ~globals ~header ~parameter
      prepared

  let callback_default request ~globals ~pointer ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_callback_default request.task ~globals ~pointer ~parameter
      prepared

  let initializer_remaining request =
    VM.task_initializer_limit request.task
    - VM.task_initializer_steps request.task

  let record_steps request steps =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Entered
    then Error "native default work requires its entered original request"
    else VM.record_native_default_steps request.task request.attempt steps

  let create ?(use_active_stream = true) ?stream_exe_print task attempt program_
      =
    {
      task;
      attempt;
      program_;
      generation_ = VM.native_task_generation ~use_active_stream task;
      source_handler = stream_exe_print;
      domain = Domain.self ();
      phase = Atomic.make Offered;
    }

  let entered request = Atomic.get request.phase = Entered

  let source_callback request =
    native_source_callback ~generation:request.generation_
      ~valid:(fun () -> Domain.self () = request.domain && entered request)
      request.source_handler

  let close request = Atomic.set request.phase Closed
end

module Native_internal_binding = struct
  type phase = Offered | Claiming | Entered | Closed

  type request = {
    task : VM.task_state;
    attempt : VM.internal_binding_attempt;
    program_ : Ir.Internal_binding_fragment_program.t;
    generation_ : VM.native_generation;
    source_handler : VM.stream_exe_print option;
    domain : Domain.id;
    phase : phase Atomic.t;
  }

  type t =
    request ->
    (Ir.Native_internal_binding_capture.t, Common.Diagnostic.t list) result

  let generation request = request.generation_
  let program request = request.program_

  let check request =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Offered
    then
      Error
        "native internal binding belongs to another domain or was already \
         claimed"
    else
      VM.check_native_task_internal_binding request.task request.attempt
        request.program_

  let claim request =
    let ( let* ) = Result.bind in
    let* () = check request in
    if Atomic.compare_and_set request.phase Offered Claiming then (
      match
        VM.claim_native_task_internal_binding request.task request.attempt
          request.program_
      with
      | Ok () ->
          Atomic.set request.phase Entered;
          Ok ()
      | Error _ as error ->
          Atomic.set request.phase Closed;
          error)
    else Error "native internal binding was already claimed"

  let function_source request link =
    let ( let* ) = Result.bind in
    let* () = check request in
    match VM.task_native_function_source request.task link with
    | Some source -> Ok source
    | None -> Error "native internal binding lacks its admitted original callee"

  let slot_binding request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_binding request.task
      ~root_runtime_calls:
        (Ir.Internal_binding_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Internal_binding_fragment_program.initialization request.program_))
      ~runtime_calls ~owner call

  let slot_address_binding request ~runtime_calls ~owner address =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Internal_binding_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Internal_binding_fragment_program.initialization request.program_))
      ~runtime_calls ~owner address

  let slot_address_refresh request binding =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.refresh_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Internal_binding_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Internal_binding_fragment_program.initialization request.program_))
      binding

  let provider_available request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_provider_available request.task ~runtime_calls ~owner call

  let parameter_default request ~globals ~header ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_parameter_default request.task ~globals ~header ~parameter
      prepared

  let callback_default request ~globals ~pointer ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_callback_default request.task ~globals ~pointer ~parameter
      prepared

  let initializer_remaining request =
    VM.task_initializer_limit request.task
    - VM.task_initializer_steps request.task

  let record_steps request steps =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Entered
    then
      Error "native internal binding work requires its entered original request"
    else
      VM.record_native_internal_binding_steps request.task request.attempt steps

  let create ?(use_active_stream = true) ?stream_exe_print task attempt program_
      =
    {
      task;
      attempt;
      program_;
      generation_ = VM.native_task_generation ~use_active_stream task;
      source_handler = stream_exe_print;
      domain = Domain.self ();
      phase = Atomic.make Offered;
    }

  let entered request = Atomic.get request.phase = Entered

  let source_callback request =
    native_source_callback ~generation:request.generation_
      ~valid:(fun () -> Domain.self () = request.domain && entered request)
      request.source_handler

  let close request = Atomic.set request.phase Closed
end

module Native_dimension = struct
  type phase = Offered | Claiming | Entered | Closed

  type request = {
    task : VM.task_state;
    attempt : VM.dimension_attempt;
    program_ : Ir.Dimension_fragment_program.t;
    generation_ : VM.native_generation;
    source_handler : VM.stream_exe_print option;
    domain : Domain.id;
    phase : phase Atomic.t;
  }

  type t =
    request ->
    ( Ir.Dimension_fragment_program.t Ir.Native_scalar_capture.t,
      Common.Diagnostic.t list )
    result

  let generation request = request.generation_
  let program request = request.program_

  let check request =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Offered
    then
      Error "native dimension belongs to another domain or was already claimed"
    else
      VM.check_native_task_dimension request.task request.attempt
        request.program_

  let claim request =
    let ( let* ) = Result.bind in
    let* () = check request in
    if Atomic.compare_and_set request.phase Offered Claiming then (
      match
        VM.claim_native_task_dimension request.task request.attempt
          request.program_
      with
      | Ok () ->
          Atomic.set request.phase Entered;
          Ok ()
      | Error _ as error ->
          Atomic.set request.phase Closed;
          error)
    else Error "native dimension was already claimed"

  let function_source request link =
    let ( let* ) = Result.bind in
    let* () = check request in
    match VM.task_native_function_source request.task link with
    | Some source -> Ok source
    | None -> Error "native dimension lacks its admitted original callee"

  let slot_binding request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_binding request.task
      ~root_runtime_calls:
        (Ir.Dimension_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Dimension_fragment_program.initialization request.program_))
      ~runtime_calls ~owner call

  let slot_address_binding request ~runtime_calls ~owner address =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Dimension_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Dimension_fragment_program.initialization request.program_))
      ~runtime_calls ~owner address

  let slot_address_refresh request binding =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.refresh_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Dimension_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Dimension_fragment_program.initialization request.program_))
      binding

  let provider_available request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_provider_available request.task ~runtime_calls ~owner call

  let parameter_default request ~globals ~header ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_parameter_default request.task ~globals ~header ~parameter
      prepared

  let callback_default request ~globals ~pointer ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_callback_default request.task ~globals ~pointer ~parameter
      prepared

  let initializer_remaining request =
    VM.task_initializer_limit request.task
    - VM.task_initializer_steps request.task

  let record_steps request steps =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Entered
    then Error "native dimension work requires its entered original request"
    else VM.record_native_dimension_steps request.task request.attempt steps

  let create ?(use_active_stream = true) ?stream_exe_print task attempt program_
      =
    {
      task;
      attempt;
      program_;
      generation_ = VM.native_task_generation ~use_active_stream task;
      source_handler = stream_exe_print;
      domain = Domain.self ();
      phase = Atomic.make Offered;
    }

  let entered request = Atomic.get request.phase = Entered

  let source_callback request =
    native_source_callback ~generation:request.generation_
      ~valid:(fun () -> Domain.self () = request.domain && entered request)
      request.source_handler

  let close request = Atomic.set request.phase Closed
end

module Native_offset = struct
  type phase = Offered | Claiming | Entered | Closed

  type request = {
    task : VM.task_state;
    attempt : VM.offset_attempt;
    program_ : Ir.Offset_fragment_program.t;
    generation_ : VM.native_generation;
    source_handler : VM.stream_exe_print option;
    domain : Domain.id;
    phase : phase Atomic.t;
  }

  type t =
    request ->
    ( Ir.Offset_fragment_program.t Ir.Native_scalar_capture.t,
      Common.Diagnostic.t list )
    result

  let generation request = request.generation_
  let program request = request.program_

  let check request =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Offered
    then Error "native offset belongs to another domain or was already claimed"
    else
      VM.check_native_task_offset request.task request.attempt request.program_

  let claim request =
    let ( let* ) = Result.bind in
    let* () = check request in
    if Atomic.compare_and_set request.phase Offered Claiming then (
      match
        VM.claim_native_task_offset request.task request.attempt
          request.program_
      with
      | Ok () ->
          Atomic.set request.phase Entered;
          Ok ()
      | Error _ as error ->
          Atomic.set request.phase Closed;
          error)
    else Error "native offset was already claimed"

  let function_source request link =
    let ( let* ) = Result.bind in
    let* () = check request in
    match VM.task_native_function_source request.task link with
    | Some source -> Ok source
    | None -> Error "native offset lacks its admitted original callee"

  let slot_binding request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_binding request.task
      ~root_runtime_calls:
        (Ir.Offset_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Offset_fragment_program.initialization request.program_))
      ~runtime_calls ~owner call

  let slot_address_binding request ~runtime_calls ~owner address =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Offset_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Offset_fragment_program.initialization request.program_))
      ~runtime_calls ~owner address

  let slot_address_refresh request binding =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.refresh_native_slot_address_binding request.task
      ~root_runtime_calls:
        (Ir.Offset_fragment_program.runtime_calls request.program_)
      ~root_globals:
        (Ir.Global_initialization.globals
           (Ir.Offset_fragment_program.initialization request.program_))
      binding

  let provider_available request ~runtime_calls ~owner call =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_provider_available request.task ~runtime_calls ~owner call

  let parameter_default request ~globals ~header ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_parameter_default request.task ~globals ~header ~parameter
      prepared

  let callback_default request ~globals ~pointer ~parameter prepared =
    let ( let* ) = Result.bind in
    let* () = check request in
    VM.task_native_callback_default request.task ~globals ~pointer ~parameter
      prepared

  let initializer_remaining request =
    VM.task_initializer_limit request.task
    - VM.task_initializer_steps request.task

  let record_steps request steps =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Entered
    then Error "native offset work requires its entered original request"
    else VM.record_native_offset_steps request.task request.attempt steps

  let create ?(use_active_stream = true) ?stream_exe_print task attempt program_
      =
    {
      task;
      attempt;
      program_;
      generation_ = VM.native_task_generation ~use_active_stream task;
      source_handler = stream_exe_print;
      domain = Domain.self ();
      phase = Atomic.make Offered;
    }

  let entered request = Atomic.get request.phase = Entered

  let source_callback request =
    native_source_callback ~generation:request.generation_
      ~valid:(fun () -> Domain.self () = request.domain && entered request)
      request.source_handler

  let close request = Atomic.set request.phase Closed
end

module Native_static_copy = struct
  type phase = Offered | Claiming | Entered | Closed

  type request = {
    task : VM.task_state;
    destination_ : Ir.Static_initializer_destination.t;
    domain : Domain.id;
    phase : phase Atomic.t;
  }

  type t = request -> (unit, Common.Diagnostic.t list) result

  let destination request = request.destination_

  let check request =
    if Domain.self () <> request.domain || Atomic.get request.phase <> Offered
    then
      Error
        "native static copy belongs to another domain or was already claimed"
    else VM.check_native_static_copy request.task request.destination_

  let claim request =
    let ( let* ) = Result.bind in
    let* () = check request in
    if Atomic.compare_and_set request.phase Offered Claiming then (
      match VM.begin_native_static_copy request.task request.destination_ with
      | Ok () ->
          Atomic.set request.phase Entered;
          Ok ()
      | Error _ as error ->
          Atomic.set request.phase Closed;
          error)
    else Error "native static copy was already claimed"

  let create task destination_ =
    { task; destination_; domain = Domain.self (); phase = Atomic.make Offered }

  let entered request = Atomic.get request.phase = Entered
  let close request = Atomic.set request.phase Closed
end

type stream = VM.task_stream

type progress = {
  runtime : VM.task_progress;
  dimension_work : int;
  switch_work : int;
}

type t = {
  session : Session.t;
  config : Frontend.Preprocessor.Config.t;
  state : VM.task_state;
  declarations : Task_declarations.t;
  identity : unit ref;
  native_dispatch : Native_dispatch.t option;
  native_static_allocation : Native_static_allocation.t option;
  native_static_initializer : Native_static_initializer.t option;
  native_static_copy : Native_static_copy.t option;
  native_default : Native_default.t option;
  native_dimension : Native_dimension.t option;
  native_offset : Native_offset.t option;
  native_internal_binding : Native_internal_binding.t option;
  mutable commands : (Frontend.Ast.module_ * command) list;
  compiled_rev : Integer_unit.compiled list ref;
  compiler_tasks : t list ref;
}

and command = {
  owner : unit ref;
  program : Integer_unit.compiled;
  span : Common.Span.t;
  source_metadata_only : bool;
  mutable frontend_pending : bool;
}

type saved_compiler = {
  compiler_session : Session.t;
  compiler_declarations : Task_declarations.t;
  mutable compiler_task : t option;
}

let saved_compiler session ~ledger =
  {
    compiler_session = session;
    compiler_declarations = ledger;
    compiler_task = None;
  }

let create ?compiler_positions ?max_switch_work ?switch_budget ?max_steps
    ?max_initializer_steps ?max_global_bytes ?max_literal_bytes ?max_frame_bytes
    ?max_call_depth ?max_output_bytes ?max_output_work ?max_generated_bytes
    ?max_stream_depth ?native_dispatch ?native_static_allocation
    ?native_static_initializer ?native_static_copy ?native_default
    ?native_dimension ?native_offset ?native_internal_binding session =
  let session = Session.task_frontend session in
  let config =
    match Frontend.Preprocessor.Config.create ~compilation_mode:Jit () with
    | Ok config -> config
    | Error message -> invalid_arg message
  in
  VM.create_task_state ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work ?max_generated_bytes ?max_stream_depth
    ~native_storage_authority:(Option.is_some native_dispatch)
    ~table:(Session.semantic_symbols session)
    ()
  |> fun result ->
  Result.bind result (fun state ->
      Task_declarations.create ?compiler_positions ?max_switch_work
        ?switch_budget ~runtime:state session
      |> Result.map (fun declarations ->
          {
            session;
            config;
            state;
            declarations;
            identity = ref ();
            native_dispatch;
            native_static_allocation;
            native_static_initializer;
            native_static_copy;
            native_default;
            native_dimension;
            native_offset;
            native_internal_binding;
            commands = [];
            compiled_rev = ref [];
            compiler_tasks = ref [];
          }))

let frontend task = task.session

let provider_source task =
  let session = task.session in
  let symbols = Session.symbols session in
  Frontend.Symbol_visibility.Environment.without_locals symbols (fun () ->
      let storage primitive =
        (Common.Primitive_type.info primitive).storage_spelling
      in
      let i64 = storage Common.Primitive_type.I64
      and u0 = storage Common.Primitive_type.U0
      and u8 = storage Common.Primitive_type.U8
      and u64 = storage Common.Primitive_type.U64 in
      let headers =
        [
          ( "StreamExePrint",
            Printf.sprintf "extern %s StreamExePrint(%s *fmt,...);" i64 u8 );
          ( "StreamPrint",
            Printf.sprintf "extern %s StreamPrint(%s *fmt,...);" u0 u8 );
          ("Print", Printf.sprintf "extern %s Print(%s *fmt,...);" u0 u8);
          ("PutChars", Printf.sprintf "extern %s PutChars(%s ch);" u0 u64);
        ]
        |> List.filter_map (fun (name, header) ->
            match
              Frontend.Symbol_visibility.Environment.find_preprocessor symbols
                name
            with
            | Absent -> Some header
            | Present _ | Shadowed_by_local -> None)
        |> String.concat "\n"
      in
      if headers = "" then None
      else
        Some
          (Session.add_source session ~path:"<hosted-task-providers>"
             ~contents:headers))

let adopt_source_with_promotion promote ?max_steps ?max_initializer_steps
    ?max_global_bytes ?max_literal_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work ?max_generated_bytes ?max_stream_depth
    ?native_dispatch ?native_static_allocation ?native_static_initializer
    ?native_static_copy ?native_default ?native_dimension ?native_offset
    ?native_internal_binding session ~source ~ledger =
  let ( let* ) = Result.bind in
  let* config = Frontend.Preprocessor.Config.create ~compilation_mode:Jit () in
  let* state =
    VM.create_task_state ?max_steps ?max_initializer_steps ?max_global_bytes
      ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
      ?max_output_work ?max_generated_bytes ?max_stream_depth
      ~native_storage_authority:(Option.is_some native_dispatch)
      ~table:(Session.semantic_symbols session)
      ()
  in
  let* () = promote ledger ~runtime:state session ~source in
  Ok
    {
      session;
      config;
      state;
      declarations = ledger;
      identity = ref ();
      native_dispatch;
      native_static_allocation;
      native_static_initializer;
      native_static_copy;
      native_default;
      native_dimension;
      native_offset;
      native_internal_binding;
      commands = [];
      compiled_rev = ref [];
      compiler_tasks = ref [];
    }

let adopt_source = adopt_source_with_promotion Task_declarations.promote_source

let adopt_source_for_activation =
  adopt_source_with_promotion Task_declarations.promote_source_for_activation

let output_bytes task = VM.task_output_bytes task.state
let output_work task = VM.task_output_work task.state
let generated_bytes task = VM.task_generated_bytes task.state
let executed_steps task = VM.task_executed_steps task.state
let initializer_steps task = VM.task_initializer_steps task.state

let synchronize_preparation_work task ~work =
  let before = VM.task_initializer_steps task.state in
  if work < before || work > VM.task_initializer_limit task.state then
    Error "source task work exceeds its cumulative preparation allowance"
  else (
    VM.record_task_preparation task.state ~before ~steps:(work - before);
    Ok ())

let observe_source_offset task ledger event =
  Task_declarations.observe ~offset_runtime:task.state ledger event

let dimension_work task = Task_declarations.dimension_work task.declarations
let switch_work task = Task_declarations.switch_work task.declarations

let progress task =
  {
    runtime = VM.task_progress task.state;
    dimension_work = dimension_work task;
    switch_work = switch_work task;
  }

let admit_global task publication =
  Task_declarations.admit_global task.declarations ~runtime:task.state
    publication

let prepare_initializer_context task receipt =
  let ( let* ) = Result.bind in
  let span =
    receipt.Frontend.Parser.leaf_initializer.initializer_owner.global_name
      .location
      .span
  in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority =
    Task_declarations.initializer_fragment_authority task.declarations
      ~runtime:task.state ~task_view receipt
  in
  let fragment = Sema.Initializer_fragment.authorized_fragment authority in
  let* context =
    Initializer_fragment_typing.create_context
      ~table:(Session.semantic_symbols task.session)
      ~parent:(Task_declarations.initializer_scope task.declarations)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare context fragment |> diagnose
  in
  Ok (context, authority, task_view, typed)

let prepare_initializer task receipt =
  prepare_initializer_context task receipt
  |> Result.map (fun (_, _, _, typed) -> typed)

let prepare_default_context task receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.default_ast.location.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority =
    Task_declarations.default_fragment_authority task.declarations
      ~runtime:task.state ~task_view receipt
  in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let* context =
    Initializer_fragment_typing.create_context
      ~table:(Session.semantic_symbols task.session)
      ~parent:(Task_declarations.initializer_scope task.declarations)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare_default context fragment |> diagnose
  in
  Ok (context, authority, task_view, typed)

let prepare_callback_default_context task receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.callback_default_ast.location.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority =
    Task_declarations.callback_default_fragment_authority task.declarations
      ~runtime:task.state ~task_view receipt
  in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let* context =
    Initializer_fragment_typing.create_context
      ~table:(Session.semantic_symbols task.session)
      ~parent:(Task_declarations.initializer_scope task.declarations)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare_default context fragment |> diagnose
  in
  Ok (context, authority, task_view, typed)

let prepare_parameter_default task receipt =
  prepare_default_context task receipt
  |> Result.map (fun (_, _, _, typed) -> typed)

let prepare_source_default task ~session ~ledger receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.default_ast.location.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* authority =
    Task_declarations.begin_source_default ledger ~runtime:task.state receipt
  in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let* () =
    if
      Expression_facts.contains_string_literal
        (Sema.Default_fragment.expression fragment)
    then
      Error
        "HCRUN0006: defaults containing string storage require native \
         owned-default preparation" |> diagnose
    else Ok ()
  in
  let* context =
    Initializer_fragment_typing.create_aot_context
      ~table:(Session.semantic_symbols session)
      ~parent:(Task_declarations.initializer_scope ledger)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare_default context fragment |> diagnose
  in
  let* destination =
    Ir.Default_fragment_destination.create_source typed |> diagnose
  in
  let before = VM.task_initializer_steps task.state in
  let* classification, _steps =
    Integer_initializers.prepare_default ~runtime:task.state ~authority
      ~on_progress:(fun steps ->
        VM.record_task_preparation task.state ~before ~steps)
      ~max_steps:(VM.task_initializer_limit task.state - before)
      ~top_calls:[] destination
  in
  let* result =
    match classification with
    | Integer_initializers.Prepared_default result -> Ok result
    | Scheduled_default ->
        Error
          "HCRUN0006: AOT default requires proven output relocation and \
           callable authority" |> diagnose
  in

  Task_declarations.finish_source_default ledger result

let prepare_source_callback_default task ~session ~ledger receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.callback_default_ast.location.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* authority =
    Task_declarations.begin_source_callback_default ledger ~runtime:task.state
      receipt
  in
  let fragment = Sema.Default_fragment.authorized_fragment authority in
  let* () =
    if
      Expression_facts.contains_string_literal
        (Sema.Default_fragment.expression fragment)
    then
      Error
        "HCRUN0006: defaults containing string storage require native \
         owned-default preparation" |> diagnose
    else Ok ()
  in
  let* context =
    Initializer_fragment_typing.create_aot_context
      ~table:(Session.semantic_symbols session)
      ~parent:(Task_declarations.initializer_scope ledger)
    |> diagnose
  in
  let* typed =
    Initializer_fragment_typing.prepare_default context fragment |> diagnose
  in
  let* destination =
    Ir.Default_fragment_destination.create_source typed |> diagnose
  in
  let before = VM.task_initializer_steps task.state in
  let* classification, _steps =
    Integer_initializers.prepare_default ~runtime:task.state ~authority
      ~on_progress:(fun steps ->
        VM.record_task_preparation task.state ~before ~steps)
      ~max_steps:(VM.task_initializer_limit task.state - before)
      ~top_calls:[] destination
  in
  let* result =
    match classification with
    | Integer_initializers.Prepared_default result -> Ok result
    | Scheduled_default ->
        Error
          "HCRUN0006: AOT default requires proven output relocation and \
           callable authority" |> diagnose
  in

  Task_declarations.finish_source_callback_default ledger result

let prepare_initializer_destination_context task ~destination receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.leaf_initializer.initializer_equals.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* context, authority, task_view, typed =
    prepare_initializer_context task receipt
  in
  let* declaration =
    match Ir.Integer_initializer_layout.declared_owner destination with
    | Some declaration -> Ok declaration
    | None ->
        Error "initializer destination has no original declared layout"
        |> diagnose
  in
  let* reference, slot =
    match
      VM.admitted_publication_for_symbol task.state
        (Sema.Compiler_record.declared_global_symbol declaration)
    with
    | Some (VM.Admitted_declared_global (reference, slot)) ->
        Ok (reference, slot)
    | _ ->
        Error "initializer destination has no retained declared object"
        |> diagnose
  in
  let* destination =
    Ir.Initializer_fragment_destination.create ~task_view ~reference ~slot
      ~layout:destination typed
    |> diagnose
  in
  Ok (context, authority, destination)

let prepare_initializer_destination task ~destination receipt =
  prepare_initializer_destination_context task ~destination receipt
  |> Result.map (fun (_, _, destination) -> destination)

let lower_initializer_fragment task ~destination receipt =
  let ( let* ) = Result.bind in
  let* context, authority, destination =
    prepare_initializer_destination_context task ~destination receipt
  in
  Initializer_fragment_lowering.lower ~context ~authority destination

let execute_initializer_leaf ?(use_active_stream = true) ?stream_exe_print task
    receipt =
  let ( let* ) = Result.bind in
  let* attempt =
    Task_declarations.begin_initializer_attempt task.declarations
      ~runtime:task.state receipt
  in
  let outcome =
    let destination = VM.initializer_attempt_destination attempt in
    let* context, authority, destination =
      prepare_initializer_destination_context task ~destination receipt
    in
    let* execution =
      Initializer_fragment_lowering.prepare ~context ~authority
        ~runtime:task.state destination
    in
    match task.native_dispatch with
    | None ->
        VM.execute_task_initializer ~use_active_stream ?stream_exe_print
          task.state attempt execution
        |> Result.map_error
             (Integer_execution_diagnostics.of_errors
                ~span:
                  receipt.Frontend.Parser.leaf_initializer.initializer_equals
                    .span)
    | Some dispatch ->
        let module Program = Ir.Initializer_fragment_program in
        let span =
          receipt.Frontend.Parser.leaf_initializer.initializer_equals.span
        in
        let diagnose result =
          Result.map_error
            (fun message -> [ Integer_source.message_diagnostic ~span message ])
            result
        in
        let* program =
          match Program.execution_code execution with
          | Program.Scheduled program -> Ok program
          | Program.Prepared _ ->
              Initializer_fragment_lowering.lower ~context ~authority
                destination
        in
        let request =
          Native_dispatch.create_initializer ~use_active_stream
            ?stream_exe_print ~task:task.state ~attempt ~execution ~program ()
        in
        Fun.protect
          ~finally:(fun () -> Native_dispatch.close_initializer request)
          (fun () ->
            try
              match dispatch.execute_initializer request with
              | Error diagnostics -> Error diagnostics
              | Ok () when Native_dispatch.initializer_entered request ->
                  VM.complete_native_task_initializer task.state attempt
                    execution program
                  |> diagnose
              | Ok () ->
                  Error
                    [
                      Integer_source.diagnostic ~span "HCIRVM0026"
                        "native initializer callback returned without claiming \
                         its original entry";
                    ]
            with exn ->
              ignore (VM.fail_task_initializer_attempt task.state attempt);
              raise exn)
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_initializer_attempt task.state attempt)
  | Ok () -> ());
  outcome

let execute_default_destination ~use_active_stream ?stream_exe_print task
    ~attempt ~context ~authority destination =
  let ( let* ) = Result.bind in
  let span = Ir.Default_fragment_destination.span destination in
  match task.native_default with
  | Some evaluate ->
      let* program =
        Default_fragment_lowering.lower_native ~context ~authority destination
      in
      let request =
        Native_default.create ~use_active_stream ?stream_exe_print task.state
          attempt program
      in
      Fun.protect
        ~finally:(fun () -> Native_default.close request)
        (fun () ->
          let* value = evaluate request in
          if Native_default.entered request then
            VM.complete_native_task_default task.state attempt program value
            |> Result.map_error (fun message ->
                [ Integer_source.message_diagnostic ~span message ])
          else
            Error
              [
                Integer_source.diagnostic ~span "HCIRVM0026"
                  "native default returned without claiming its original \
                   expression";
              ])
  | None when Option.is_some task.native_dispatch ->
      Error
        [
          Integer_source.diagnostic ~span "HCRUN0006"
            "native task defaults require a native consumer";
        ]
  | None ->
      let* execution =
        Default_fragment_lowering.prepare ~context ~authority
          ~runtime:task.state destination
      in
      VM.execute_task_default ~use_active_stream ?stream_exe_print task.state
        attempt execution
      |> Result.map_error (Integer_execution_diagnostics.of_errors ~span)

let execute_parameter_default ?(use_active_stream = true) ?stream_exe_print task
    receipt =
  let ( let* ) = Result.bind in
  let* attempt =
    Task_declarations.begin_default_attempt task.declarations
      ~runtime:task.state receipt
  in
  let outcome =
    let* context, authority, task_view, typed =
      prepare_default_context task receipt
    in
    let span = receipt.Frontend.Parser.default_ast.location.span in
    let* destination =
      Ir.Default_fragment_destination.create ~task_view typed
      |> Result.map_error (fun message ->
          [ Integer_source.message_diagnostic ~span message ])
    in
    execute_default_destination ~use_active_stream ?stream_exe_print task
      ~attempt ~context ~authority destination
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_default task.state attempt)
  | Ok () -> ());
  outcome

let execute_callback_default ?(use_active_stream = true) ?stream_exe_print task
    receipt =
  let ( let* ) = Result.bind in
  let* attempt =
    Task_declarations.begin_callback_default_attempt task.declarations
      ~runtime:task.state receipt
  in
  let outcome =
    let* context, authority, task_view, typed =
      prepare_callback_default_context task receipt
    in
    let span = receipt.Frontend.Parser.callback_default_ast.location.span in
    let* destination =
      Ir.Default_fragment_destination.create ~task_view typed
      |> Result.map_error (fun message ->
          [ Integer_source.message_diagnostic ~span message ])
    in
    execute_default_destination ~use_active_stream ?stream_exe_print task
      ~attempt ~context ~authority destination
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_default task.state attempt)
  | Ok () -> ());
  outcome

let execute_runtime_dimension ?(use_active_stream = true) ?stream_exe_print task
    receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.dimension_opening.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority, attempt =
    Task_declarations.begin_runtime_dimension task.declarations
      ~runtime:task.state ~task_view receipt
  in
  let outcome =
    let fragment = Sema.Dimension_fragment.authorized_fragment authority in
    let* context =
      Initializer_fragment_typing.create_context
        ~table:(Session.semantic_symbols task.session)
        ~parent:(Task_declarations.initializer_scope task.declarations)
      |> diagnose
    in
    let* typed =
      Initializer_fragment_typing.prepare_dimension context fragment |> diagnose
    in
    let* destination =
      Ir.Dimension_fragment_destination.create ~task_view typed |> diagnose
    in
    match task.native_dimension with
    | Some evaluate ->
        let* program =
          Dimension_fragment_lowering.lower_native ~context ~authority
            destination
        in
        let request =
          Native_dimension.create ~use_active_stream ?stream_exe_print
            task.state attempt program
        in
        Fun.protect
          ~finally:(fun () -> Native_dimension.close request)
          (fun () ->
            let* capture = evaluate request in
            if Native_dimension.entered request then
              VM.complete_native_task_dimension task.state attempt program
                capture
              |> diagnose
            else
              Error
                [
                  Integer_source.diagnostic ~span "HCIRVM0026"
                    "native dimension returned without claiming its original \
                     expression";
                ])
    | None when Option.is_some task.native_dispatch ->
        Error
          [
            Integer_source.diagnostic ~span "HCRUN0006"
              "native task execution requires its dimension adapter";
          ]
    | None ->
        let* execution =
          Dimension_fragment_lowering.prepare ~context ~authority
            ~runtime:task.state destination
        in
        VM.execute_task_dimension ~use_active_stream ?stream_exe_print
          task.state attempt execution
        |> Result.map_error (Integer_execution_diagnostics.of_errors ~span)
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_dimension task.state attempt)
  | Ok () -> ());
  let finished =
    Task_declarations.finish_runtime_dimension task.declarations
      ~runtime:task.state ~succeeded:(Result.is_ok outcome) receipt
  in
  let* () = outcome in
  finished

let execute_runtime_internal_binding ?(use_active_stream = true)
    ?stream_exe_print task receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.binding_ast.location.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority, attempt =
    Task_declarations.begin_runtime_internal_binding task.declarations
      ~runtime:task.state ~task_view receipt
  in
  let outcome =
    let fragment =
      Sema.Internal_binding_fragment.authorized_fragment authority
    in
    let* context =
      Initializer_fragment_typing.create_context
        ~table:(Session.semantic_symbols task.session)
        ~parent:(Task_declarations.initializer_scope task.declarations)
      |> diagnose
    in
    let* typed =
      Initializer_fragment_typing.prepare_internal_binding context fragment
      |> diagnose
    in
    let* destination =
      Ir.Internal_binding_fragment_destination.create ~task_view typed
      |> diagnose
    in
    match task.native_internal_binding with
    | Some evaluate ->
        let* program =
          Internal_binding_fragment_lowering.lower_native ~context ~authority
            destination
        in
        let request =
          Native_internal_binding.create ~use_active_stream ?stream_exe_print
            task.state attempt program
        in
        Fun.protect
          ~finally:(fun () -> Native_internal_binding.close request)
          (fun () ->
            let* capture = evaluate request in
            if Native_internal_binding.entered request then
              VM.complete_native_task_internal_binding task.state attempt
                program capture
              |> diagnose
            else
              Error
                [
                  Integer_source.diagnostic ~span "HCIRVM0026"
                    "native internal binding returned without claiming its \
                     original expression";
                ])
    | None when Option.is_some task.native_dispatch ->
        Error
          [
            Integer_source.diagnostic ~span "HCRUN0006"
              "native task execution requires its internal binding adapter";
          ]
    | None ->
        let* execution =
          Internal_binding_fragment_lowering.prepare ~context ~authority
            ~runtime:task.state destination
        in
        VM.execute_task_internal_binding ~use_active_stream ?stream_exe_print
          task.state attempt execution
        |> Result.map_error (Integer_execution_diagnostics.of_errors ~span)
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_internal_binding task.state attempt)
  | Ok () -> ());
  let finished =
    Task_declarations.finish_runtime_internal_binding task.declarations
      ~runtime:task.state ~succeeded:(Result.is_ok outcome) receipt
  in
  let* () = outcome in
  finished

let execute_runtime_offset ?(use_active_stream = true) ?stream_exe_print task
    receipt =
  let ( let* ) = Result.bind in
  let span = receipt.Frontend.Parser.phase_location.span in
  let diagnose result =
    Result.map_error
      (fun message -> [ Integer_source.message_diagnostic ~span message ])
      result
  in
  let before = VM.task_initializer_steps task.state in
  let* task_view = VM.task_snapshot task.state |> diagnose in
  let* authority, attempt =
    Task_declarations.begin_runtime_offset task.declarations ~runtime:task.state
      ~task_view receipt
  in
  let outcome =
    let fragment = Sema.Offset_fragment.authorized_fragment authority in
    let* context =
      Initializer_fragment_typing.create_context
        ~table:(Session.semantic_symbols task.session)
        ~parent:(Task_declarations.initializer_scope task.declarations)
      |> diagnose
    in
    let* typed =
      Initializer_fragment_typing.prepare_offset context fragment |> diagnose
    in
    let* destination =
      Ir.Offset_fragment_destination.create ~task_view typed |> diagnose
    in
    match task.native_offset with
    | Some evaluate ->
        let* program =
          Offset_fragment_lowering.lower_native ~context ~authority destination
        in
        let request =
          Native_offset.create ~use_active_stream ?stream_exe_print task.state
            attempt program
        in
        Fun.protect
          ~finally:(fun () -> Native_offset.close request)
          (fun () ->
            let* capture = evaluate request in
            if Native_offset.entered request then
              VM.complete_native_task_offset task.state attempt program capture
              |> diagnose
            else
              Error
                [
                  Integer_source.diagnostic ~span "HCIRVM0026"
                    "native offset returned without claiming its original \
                     expression";
                ])
    | None when Option.is_some task.native_dispatch ->
        Error
          [
            Integer_source.diagnostic ~span "HCRUN0006"
              "native task execution requires its offset adapter";
          ]
    | None ->
        let* execution =
          Offset_fragment_lowering.prepare ~context ~authority
            ~runtime:task.state destination
        in
        VM.execute_task_offset ~use_active_stream ?stream_exe_print task.state
          attempt execution
        |> Result.map_error (Integer_execution_diagnostics.of_errors ~span)
  in
  (match outcome with
  | Error _ -> ignore (VM.fail_task_offset task.state attempt)
  | Ok () -> ());
  let finished =
    Task_declarations.finish_runtime_offset task.declarations
      ~runtime:task.state ~before ~succeeded:(Result.is_ok outcome) receipt
  in
  let* () = outcome in
  finished

let observe_initializer_internal ?(use_active_stream = true) ?stream_exe_print
    task event =
  let ( let* ) = Result.bind in
  let native_reject span work =
    Error
      [
        Integer_source.diagnostic ~span "HCRUN0006"
          ("native task execution does not yet support live " ^ work);
      ]
  in
  (match event with
    | Frontend.Parser.Static_initializer_preparing receipt -> (
        let span =
          receipt.static_allocation.allocation_function.function_name.location
            .span
        in
        let diagnose result =
          Result.map_error
            (fun message -> [ Integer_source.message_diagnostic ~span message ])
            result
        in
        let* task_view = VM.task_snapshot task.state |> diagnose in
        let* allocation, fragment =
          Task_declarations.task_static_fragment task.declarations
            ~runtime:task.state ~task_view receipt
        in
        let* context =
          Initializer_fragment_typing.create_context
            ~table:(Session.semantic_symbols task.session)
            ~parent:(Task_declarations.initializer_scope task.declarations)
          |> diagnose
        in
        let* typed =
          Initializer_fragment_typing.prepare_static context fragment
          |> diagnose
        in
        let* destination =
          Ir.Static_initializer_destination.create ~allocation ~task_view
            ~cursor:(Ir.Integer_static_allocation.cursor allocation)
            typed
          |> diagnose
        in
        match Ir.Static_initializer_destination.copy_byte_count destination with
        | Some _ when Option.is_none task.native_dispatch ->
            VM.execute_task_static_copy task.state destination |> diagnose
        | Some _ -> (
            match task.native_static_copy with
            | None -> native_reject span "static string copies"
            | Some copy ->
                let request =
                  Native_static_copy.create task.state destination
                in
                Fun.protect
                  ~finally:(fun () -> Native_static_copy.close request)
                  (fun () ->
                    let* () = copy request in
                    if Native_static_copy.entered request then
                      VM.complete_native_static_copy task.state destination
                      |> diagnose
                    else
                      Error
                        [
                          Integer_source.diagnostic ~span "HCIRVM0026"
                            "native static copy returned without claiming its \
                             original leaf";
                        ]))
        | None when Option.is_none task.native_dispatch ->
            let* program =
              Static_initializer_lowering.lower ~runtime:task.state ~context
                destination
            in
            VM.execute_task_static_initializer ~use_active_stream
              ?stream_exe_print task.state program
            |> Result.map_error (Integer_execution_diagnostics.of_errors ~span)
        | None -> (
            match task.native_static_initializer with
            | None -> native_reject span "static scalar initializers"
            | Some initialize ->
                let* program =
                  Static_initializer_lowering.lower ~context destination
                in
                let request =
                  Native_static_initializer.create ~use_active_stream
                    ?stream_exe_print task.state program
                in
                Fun.protect
                  ~finally:(fun () -> Native_static_initializer.close request)
                  (fun () ->
                    let* () = initialize request in
                    if Native_static_initializer.entered request then
                      VM.complete_native_static_initializer task.state program
                      |> diagnose
                    else
                      Error
                        [
                          Integer_source.diagnostic ~span "HCIRVM0026"
                            "native static initializer returned without \
                             claiming its original entry";
                        ])))
    | Frontend.Parser.Internal_binding_preparing receipt ->
        execute_runtime_internal_binding ~use_active_stream ?stream_exe_print
          task receipt
    | Frontend.Parser.Aggregate_advanced receipt
      when Task_declarations.offset_requires_runtime receipt ->
        execute_runtime_offset ~use_active_stream ?stream_exe_print task receipt
    | Frontend.Parser.Array_dimension_preparing receipt
      when Task_declarations.dimension_requires_runtime receipt ->
        execute_runtime_dimension ~use_active_stream ?stream_exe_print task
          receipt
    | Frontend.Parser.Function_local_allocated receipt
      when receipt.allocation_storage = Frontend.Ast.Static_local -> (
        let* allocation =
          Task_declarations.declare_static_symbol task.declarations
            ~runtime:task.state receipt
        in
        match task.native_static_allocation with
        | None -> Ok ()
        | Some allocate ->
            let span =
              receipt.allocation_function.function_name.location.span
            in
            let* request =
              Native_static_allocation.create task.state allocation
              |> Result.map_error (fun message ->
                  [ Integer_source.message_diagnostic ~span message ])
            in
            Fun.protect
              ~finally:(fun () -> Native_static_allocation.close request)
              (fun () ->
                let* () = allocate request in
                if Native_static_allocation.entered request then Ok ()
                else
                  Error
                    [
                      Integer_source.diagnostic ~span "HCIRVM0026"
                        "native static allocator returned without claiming its \
                         original request";
                    ]))
    | Frontend.Parser.Global_declared publication ->
        admit_global task publication
    | Frontend.Parser.Global_initializer_started start ->
        Task_declarations.begin_initializer_runtime task.declarations
          ~runtime:task.state start
    | Frontend.Parser.Global_initializer_delimiter_completed receipt ->
        Task_declarations.observe_initializer_delimiter task.declarations
          ~runtime:task.state receipt
    | Frontend.Parser.Global_initializer_leaf_completed receipt ->
        execute_initializer_leaf ~use_active_stream ?stream_exe_print task
          receipt
    | Frontend.Parser.Callback_default_completed receipt -> (
        match receipt.callback_default_ast.value with
        | Frontend.Ast.Expression_default _ ->
            execute_callback_default ~use_active_stream ?stream_exe_print task
              receipt
        | Lastclass_default _ -> Ok ())
    | Frontend.Parser.Callback_signature_completed header ->
        Task_declarations.complete_callback_defaults_runtime task.declarations
          ~runtime:task.state header
    | Frontend.Parser.Parameter_default_completed receipt -> (
        match receipt.default_ast.value with
        | Frontend.Ast.Expression_default _ ->
            execute_parameter_default ~use_active_stream ?stream_exe_print task
              receipt
        | Frontend.Ast.Lastclass_default _ -> Ok ())
    | Frontend.Parser.Function_header_completed header ->
        Result.bind
          (Task_declarations.complete_defaults_runtime task.declarations
             ~runtime:task.state header) (fun () ->
            Task_declarations.admit_function_header task.declarations
              ~runtime:task.state header)
    | Frontend.Parser.Global_completed (_, completed)
      when Option.is_some completed.global_initial_value ->
        Task_declarations.complete_initializer_runtime task.declarations
          ~runtime:task.state event
    | _ -> Ok ())
  |> fun prepared ->
  Result.bind prepared (fun () ->
      Task_declarations.admit_function_phase task.declarations
        ~runtime:task.state event)
  |> Result.map_error
       (List.map (fun (error : Common.Diagnostic.t) ->
            if
              error.code = "HCRUN0004"
              && String.starts_with ~prefix:"HC" error.message
              && String.contains error.message ':'
            then
              let decoded =
                Integer_source.message_diagnostic ~span:error.primary
                  error.message
              in
              { error with code = decoded.code; message = decoded.message }
            else error))

let observe_initializer task event = observe_initializer_internal task event
let compiled_units task = List.rev !(task.compiled_rev)

let compile_isolated task ~source_command session ~config parsed =
  match task.native_dispatch with
  | None ->
      Integer_unit.compile_source_in_task_budget ~task:task.state
        ~source_command session ~config parsed
  | Some _ ->
      let span = (Option.get parsed.Frontend.Parser.ast).span in
      Error
        [
          Integer_source.diagnostic ~span "HCIRVM0026"
            "native source tasks cannot compile isolated interpreter programs";
        ]

let execute_isolated task program =
  VM.execute_isolated_program_in_task task.state
    ~runtime_calls:(Integer_unit.runtime_calls program)
    ~globals:(Integer_unit.globals program)
    ~initialization:(Integer_unit.initialization program)
    ~functions:(Integer_unit.functions program)
    (Integer_unit.entry program)

let begin_stream task = VM.begin_task_stream task.state
let finish_stream task stream = VM.finish_task_stream task.state stream
let abort_stream task stream = VM.abort_task_stream task.state stream

let same_item left right =
  let open Frontend.Ast in
  match (left, right) with
  | Aggregate_forward_declaration left, Aggregate_forward_declaration right ->
      left == right
  | Aggregate_definition left, Aggregate_definition right -> left == right
  | Global_variable left, Global_variable right -> left == right
  | Global_declaration left, Global_declaration right -> left == right
  | Function_prototype left, Function_prototype right -> left == right
  | Function_definition left, Function_definition right -> left == right
  | Top_level_statement left, Top_level_statement right ->
      statement_location left == statement_location right
  | _ -> false

let same_syntax (left : Frontend.Ast.module_) (right : Frontend.Ast.module_) =
  left == right
  || left.items <> []
     && List.length left.items = List.length right.items
     && List.for_all2 same_item left.items right.items

let source_metadata_only (ast : Frontend.Ast.module_) =
  let open Frontend.Ast in
  List.for_all
    (function
      | Aggregate_forward_declaration _
      | Aggregate_definition _
      | Global_variable _
      | Global_declaration _
      | Function_prototype _ -> true
      | Function_definition _ | Top_level_statement _ -> false)
    ast.items

let compile_ast_internal ?declaration_command task (ast : Frontend.Ast.module_)
    =
  match
    List.find_opt (fun (source, _) -> same_syntax source ast) task.commands
  with
  | Some (_, command) -> Ok command
  | None
    when List.exists
           (fun (source, _) ->
             List.exists
               (fun prior -> List.exists (same_item prior) ast.items)
               source.Frontend.Ast.items)
           task.commands ->
      Error
        [
          Integer_source.diagnostic ~span:ast.span "HCIRVM0026"
            "parsed syntax already belongs to another compiled task command";
        ]
  | None ->
      let ( let* ) = Result.bind in
      let* checked =
        Integer_unit.compile_task_ast ~task:task.state ?declaration_command
          task.session ~config:task.config ast
      in
      let command =
        {
          owner = task.identity;
          program = checked.Integer_unit.value;
          span = ast.span;
          source_metadata_only =
            Option.is_some declaration_command && source_metadata_only ast;
          frontend_pending = Option.is_none declaration_command;
        }
      in
      task.commands <- (ast, command) :: task.commands;
      task.compiled_rev := command.program :: !(task.compiled_rev);
      Ok command

let compile_ast task ast = compile_ast_internal task ast

let compile_source_ast task ast =
  Result.bind (Task_declarations.seal task.declarations ast)
    (fun declaration_command ->
      compile_ast_internal ~declaration_command task ast)

let execute_internal ?(use_active_stream = true) ?stream_exe_print task command
    =
  let program = command.program in
  if task.identity != command.owner then
    Error
      [
        Integer_source.diagnostic ~span:command.span "HCIRVM0026"
          "compiled command belongs to another task";
      ]
  else
    let outcome =
      VM.execute_task_program ~use_active_stream ?stream_exe_print task.state
        ~runtime_calls:(Integer_unit.runtime_calls program)
        ~globals:(Integer_unit.globals program)
        ~initialization:(Integer_unit.initialization program)
        ~functions:(Integer_unit.functions program)
        (Integer_unit.entry program)
      |> Result.map_error
           (Integer_execution_diagnostics.of_errors ~span:command.span)
    in
    let publication =
      if not command.frontend_pending then Ok ()
      else
        match
          VM.task_admission task.state
            ~globals:(Integer_unit.globals program)
            ~entry:(Integer_unit.entry program)
        with
        | None -> Ok ()
        | Some receipt ->
            Task_declarations.observe_admission task.declarations receipt
            |> Result.map_error (fun message ->
                [
                  Integer_source.diagnostic ~span:command.span "HCRUN0004"
                    message;
                ])
            |> Result.map (fun () -> command.frontend_pending <- false)
    in
    match (outcome, publication) with
    | Ok value, Ok () -> Ok value
    | Error errors, Ok () | Ok _, Error errors -> Error errors
    | Error errors, Error publication_errors ->
        Error (errors @ publication_errors)

let native_word_of_vm (word : VM.word) =
  match word.type_ with
  | VM.I64 -> Native_dispatch.I64 word.bits
  | VM.U64 -> Native_dispatch.U64 word.bits

let native_word_for_vm = function
  | Native_dispatch.I64 bits -> (VM.I64, bits)
  | Native_dispatch.U64 bits -> (VM.U64, bits)

let execute_source ?(use_active_stream = true) ?stream_exe_print task command =
  if task.identity != command.owner then
    Error
      [
        Integer_source.diagnostic ~span:command.span "HCIRVM0026"
          "compiled command belongs to another task";
      ]
  else
    match task.native_dispatch with
    | None ->
        execute_internal ~use_active_stream ?stream_exe_print task command
        |> Result.map (fun execution ->
            Option.map native_word_of_vm (VM.final_value execution))
    | Some _ when command.frontend_pending ->
        Error
          [
            Integer_source.diagnostic ~span:command.span "HCIRVM0026"
              "native source dispatch requires an original parser resume \
               command";
          ]
    | Some _ when command.source_metadata_only -> (
        let program = command.program in
        let diagnose result =
          Result.map_error
            (fun message ->
              [ Integer_source.message_diagnostic ~span:command.span message ])
            result
        in
        match
          VM.claim_native_task_program task.state
            ~runtime_calls:(Integer_unit.runtime_calls program)
            ~globals:(Integer_unit.globals program)
            ~initialization:(Integer_unit.initialization program)
            ~functions:(Integer_unit.functions program)
            (Integer_unit.entry program)
        with
        | Error message ->
            VM.fail_native_task_program_before_entry task.state;
            Error
              [ Integer_source.message_diagnostic ~span:command.span message ]
        | Ok attempt -> (
            match
              VM.complete_native_task_program task.state attempt ~captured:false
                ~final_value:None
              |> diagnose
            with
            | Ok () -> Ok None
            | Error diagnostics ->
                ignore (VM.fail_native_task_program task.state attempt);
                Error diagnostics))
    | Some dispatch ->
        let request =
          Native_dispatch.create_command ~use_active_stream ?stream_exe_print
            ~task:task.state ~program:command.program ()
        in
        let diagnose result =
          Result.map_error
            (fun message ->
              [ Integer_source.message_diagnostic ~span:command.span message ])
            result
        in
        let fail_request () =
          match Native_dispatch.command_attempt request with
          | Some attempt ->
              ignore (VM.fail_native_task_program task.state attempt)
          | None -> VM.fail_native_task_program_before_entry task.state
        in
        Fun.protect
          ~finally:(fun () -> Native_dispatch.close_command request)
          (fun () ->
            try
              match dispatch.execute_command request with
              | Error diagnostics ->
                  fail_request ();
                  Error diagnostics
              | Ok capture -> (
                  let captured, value =
                    match capture with
                    | Native_dispatch.Unchanged -> (false, None)
                    | Native_dispatch.Captured value -> (true, value)
                  in
                  match
                    ( Native_dispatch.command_entered request,
                      Native_dispatch.command_attempt request )
                  with
                  | true, Some attempt -> (
                      let settled =
                        VM.complete_native_task_program task.state attempt
                          ~captured
                          ~final_value:(Option.map native_word_for_vm value)
                        |> diagnose
                      in
                      match settled with
                      | Ok () -> Ok value
                      | Error diagnostics ->
                          fail_request ();
                          Error diagnostics)
                  | _ ->
                      fail_request ();
                      Error
                        [
                          Integer_source.diagnostic ~span:command.span
                            "HCIRVM0026"
                            "native command callback returned without claiming \
                             its original entry";
                        ])
            with exn ->
              fail_request ();
              raise exn)

let execute task command = execute_internal task command

let native_final_value task =
  match task.native_dispatch with
  | None -> None
  | Some _ ->
      let progress = VM.task_progress task.state in
      Option.map native_word_of_vm progress.final_value

let stream_diagnostics span message =
  let code, detail =
    match String.index_opt message ':' with
    | Some separator ->
        ( String.sub message 0 separator,
          String.sub message (separator + 1)
            (String.length message - separator - 1)
          |> String.trim )
    | None -> ("HCIRVM0027", message)
  in
  [ Integer_source.diagnostic ~span code detail ]

let activate_source task ~span =
  Task_declarations.activate_source task.declarations ~runtime:task.state ~span
    ~declaration:(observe_initializer task) ~command:(fun ast ->
      Result.bind (compile_source_ast task ast) (fun command ->
          execute_source task command |> Result.map ignore))

let result task ~sequence =
  VM.task_result task.state ~sequence
  |> Result.map_error (fun message ->
      [
        Integer_source.diagnostic
          ~span:sequence.Frontend.Parser.sequence_ast.span "HCRUN0004" message;
      ])

let execution_commands ?(use_active_stream = true) ?stream_exe_print task span
    ~active ~execute_command =
  let ( let* ) = Result.bind in
  let context = ref None in
  let sequence = ref None in
  let final_value = ref None in
  let aborted = ref false in
  let invalid () =
    Error
      (stream_diagnostics span
         "HCIRVM0027: parser executor does not own the active source context")
  in
  let active () = if !aborted then invalid () else active () in
  let owns candidate =
    match !context with
    | Some owner -> owner == candidate
    | None -> false
  in
  let reading candidate =
    let* () = active () in
    if owns candidate && Option.is_none !sequence then Ok () else invalid ()
  in
  let command_context (start : Frontend.Parser.command_start) =
    start.command_context
  in
  let reference selection =
    let* () =
      reading (Frontend.Parser.selected_command selection |> command_context)
    in
    Task_declarations.observe_execution_reference task.declarations selection
  in
  let query event =
    let open Frontend.Parser in
    let root =
      match event with
      | Query_root root -> root
      | Query_member_started start -> start.member_start_root
      | Query_member member -> member.query_member_root
      | Query_completed completed -> completed.query_root
    in
    let* () = reading root.query_command.command_context in
    Task_declarations.observe_query task.declarations event
  in
  let declaration event =
    let open Frontend.Parser in
    let start =
      match event with
      | Internal_binding_preparing p -> p.binding_command
      | Aggregate_declared p -> p.aggregate_header.declaration_command
      | Aggregate_advanced p ->
          p.phase_aggregate.aggregate_header.declaration_command
      | Aggregate_completed p ->
          p.aggregate_publication.aggregate_header.declaration_command
      | Array_dimension_preparing preparation ->
          preparation.dimension_owner.dimensions_command
      | Array_dimension_completed completed ->
          completed.dimension_preparation.dimension_owner.dimensions_command
      | Switch_case_preparing preparation ->
          preparation.switch_owner.switch_command
      | Switch_case_completed completed ->
          completed.completed_case_owner.switch_command
      | Switch_completed completed -> completed.switch_owner.switch_command
      | Global_declared publication | Global_completed (publication, _) ->
          publication.global_header.declaration_command
      | Global_initializer_started start ->
          start.initializer_owner.global_header.declaration_command
      | Global_initializer_leaf_completed leaf ->
          leaf.leaf_initializer.initializer_owner.global_header
            .declaration_command
      | Global_initializer_delimiter_completed delimiter ->
          delimiter.delimiter_initializer.initializer_owner.global_header
            .declaration_command
      | Function_declared publication ->
          publication.function_header.declaration_command
      | Function_position_written p ->
          p.position_function.function_header.declaration_command
      | Static_initializer_preparing p ->
          p.static_allocation.allocation_function.function_header
            .declaration_command
      | Static_initializer_completed p ->
          p.static_completed_start.static_start_allocation.allocation_function
            .function_header
            .declaration_command
      | Function_local_allocated p ->
          p.allocation_function.function_header.declaration_command
      | Function_parameter_declared p ->
          p.parameter_function.function_header.declaration_command
      | Function_parameter_completed p ->
          p.parameter_publication.parameter_function.function_header
            .declaration_command
      | Function_variadic_started p | Function_variadic_completed p ->
          p.variadic_function.function_header.declaration_command
      | Callback_position_written p ->
          p.callback_position_signature.callback_command
      | Callback_signature_started p -> p.callback_command
      | Callback_parameter_declared p ->
          p.callback_parameter_signature.callback_command
      | Callback_parameter_completed p ->
          p.callback_parameter_publication.callback_parameter_signature
            .callback_command
      | Callback_default_completed p ->
          p.callback_default_signature.callback_command
      | Callback_signature_completed p ->
          p.callback_signature_publication.callback_command
      | Parameter_default_completed receipt ->
          receipt.default_function.function_header.declaration_command
      | Function_header_completed header | Function_body_completed (header, _)
        -> header.function_publication.function_header.declaration_command
    in
    let* () = reading start.command_context in
    let* () = Task_declarations.observe task.declarations event in
    observe_initializer_internal ~use_active_stream ?stream_exe_print task event
  in
  let dimension_count (completed : Frontend.Parser.completed_array_dimension) =
    let* () =
      reading
        completed.dimension_preparation.dimension_owner.dimensions_command
          .command_context
    in
    Task_declarations.grammar_dimension_count task.declarations completed
  in
  let commands : Frontend.Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            let open Frontend.Parser in
            let candidate =
              match event with
              | Sequence_started candidate | Sequence_aborted candidate ->
                  candidate
              | Command_started start -> start.command_context
              | Command_completed completed | Command_resumed completed ->
                  completed.command_start.command_context
              | Sequence_completed completed -> completed.sequence_context
            in
            let* () =
              match event with
              | Sequence_aborted _ when owns candidate && not !aborted ->
                  (* Source cleanup must survive an earlier explicit buffer
                     abort. It grants no compilation or execution progress. *)
                  Ok ()
              | _ -> active ()
            in
            let* () =
              match event with
              | Sequence_started _ when Option.is_none !context ->
                  context := Some candidate;
                  Ok ()
              | Sequence_started _ -> invalid ()
              | Sequence_aborted _ when owns candidate -> Ok ()
              | _ -> reading candidate
            in
            let* () =
              Task_declarations.observe_command task.declarations event
            in
            match event with
            | Frontend.Parser.Command_resumed completed ->
                let ast = completed.command_ast in
                let* command = compile_source_ast task ast in
                let* value = execute_command command in
                final_value := value;
                Ok ()
            | Frontend.Parser.Sequence_completed completed ->
                sequence := Some completed;
                Ok ()
            | Frontend.Parser.Sequence_aborted _ ->
                aborted := true;
                Ok ()
            | _ -> Ok ());
      query = Some query;
      call =
        Some
          {
            implicit =
              Some
                {
                  arguments =
                    (fun selection ->
                      let* () =
                        reading
                          (Frontend.Parser.implicit_command selection)
                            .command_context
                      in
                      Task_declarations.observe_implicit_arguments
                        task.declarations selection);
                  emission =
                    (fun selection ->
                      let* () =
                        reading
                          (Frontend.Parser.implicit_command selection)
                            .command_context
                      in
                      Task_declarations.observe_implicit_emission
                        task.declarations selection);
                };
            start =
              (fun receipt ->
                let* () =
                  reading
                    (Frontend.Parser.selected_command
                       receipt.Frontend.Parser.call_reference)
                      .command_context
                in
                Task_declarations.observe_call_start task.declarations receipt);
            emit =
              (fun receipt ->
                let* () =
                  reading
                    (Frontend.Parser.selected_command
                       receipt.Frontend.Parser.call_start.call_reference)
                      .command_context
                in
                Task_declarations.observe_call_emission task.declarations
                  receipt);
          };
      implicit_output =
        Some
          (fun selection ->
            let* () =
              reading
                (Frontend.Parser.implicit_command selection).command_context
            in
            let* () =
              Task_declarations.observe_implicit_output task.declarations
                selection
            in
            Task_declarations.validate_implicit_output task.declarations
              selection ~execution:true);
      reference = Some reference;
      declaration = Some declaration;
      dimension_count = Some dimension_count;
      command = (fun _ -> active ());
      resume = active;
    }
  in
  ( commands,
    fun () ->
      let* () = active () in
      match !sequence with
      | Some completed
        when owns completed.sequence_context
             && Frontend.Parser.sequence_accepted completed ->
          Ok (completed, !final_value)
      | _ ->
          Error
            (stream_diagnostics span
               "HCIRVM0027: source sequence has not been accepted") )

let rec stream_executor ?saved_compiler ?(allow_stream_exe_print = true) task
    span =
  let ( let* ) = Result.bind in
  let* stream =
    begin_stream task |> Result.map_error (stream_diagnostics span)
  in
  let closed = ref false in
  let active () =
    if !closed || not (VM.task_stream_is_active task.state stream) then
      Error
        (stream_diagnostics span
           "HCIRVM0027: parser executor does not own the active stream context")
    else Ok ()
  in
  let stream_exe_print =
    if allow_stream_exe_print then
      Some (run_stream_exe_source ?saved_compiler task ~active ~span)
    else None
  in
  let execute_command command =
    match task.native_dispatch with
    | Some _ -> execute_source ?stream_exe_print task command
    | None ->
        execute_internal ?stream_exe_print task command
        |> Result.map (fun execution ->
            Option.map native_word_of_vm (VM.final_value execution))
  in
  let commands, completed =
    execution_commands ?stream_exe_print task span ~active ~execute_command
  in
  Ok
    Frontend.Parser.
      {
        definitions = Session.definitions task.session;
        symbols = Session.symbols task.session;
        commands;
        finish =
          (fun () ->
            let* _, _ = completed () in
            let* generated =
              finish_stream task stream
              |> Result.map_error (stream_diagnostics span)
            in
            closed := true;
            Ok generated);
        abort =
          (fun () ->
            match abort_stream task stream with
            | Ok () -> closed := true
            | Error _ -> ());
      }

and run_stream_exe_source ?saved_compiler task ~active ~span contents =
  let ( let* ) = Result.bind in
  let* () = active () in
  let* suspension =
    Task_declarations.parser_suspension task.declarations
    |> Result.map_error (stream_diagnostics span)
  in
  let* enclosing =
    Frontend.Parser.suspension_enclosing_context suspension
    |> Result.map_error (stream_diagnostics span)
  in
  let* target =
    match saved_compiler with
    | None ->
        if
          Frontend.Parser.context_environment enclosing
          == Session.symbols task.session
        then Ok task
        else
          Error
            (stream_diagnostics span
               "saved compiler tables require their original namespace adapter")
    | Some saved -> (
        let* _ =
          Task_declarations.saved_compiler_context saved.compiler_declarations
            ~session:saved.compiler_session ~suspension
          |> Result.map_error (stream_diagnostics span)
        in
        match saved.compiler_task with
        | Some target when VM.task_shares_resources task.state target.state ->
            Ok target
        | Some _ ->
            Error
              (stream_diagnostics span
                 "saved compiler execution has another original resource owner")
        | None ->
            let* state =
              VM.create_compiler_namespace_task task.state
                ~table:(Session.semantic_symbols saved.compiler_session)
              |> Result.map_error (stream_diagnostics span)
            in
            let* declarations =
              Task_declarations.create_saved_compiler_runtime
                saved.compiler_declarations ~session:saved.compiler_session
                ~suspension ~runtime:state
              |> Result.map_error (stream_diagnostics span)
            in
            let target =
              {
                task with
                session = saved.compiler_session;
                state;
                declarations;
                identity = ref ();
                commands = [];
              }
            in
            saved.compiler_task <- Some target;
            task.compiler_tasks := target :: !(task.compiler_tasks);
            Ok target)
  in
  let execute suspension source =
    let* sequence, final_value =
      run_input_execution ~suspension ~enclosing ~stream_task:task
        ~use_active_stream:false ~active target ~source
    in
    let* () =
      VM.check_task_suspended_completion target.state ~suspension sequence
      |> Result.map_error (fun message ->
          [
            Integer_source.diagnostic
              ~span:(Integer_source.source_span source)
              "HCRUN0004" message;
          ])
    in
    let* () = active () in
    Ok final_value
  in
  let* suspension =
    if target == task then Ok suspension
    else
      match provider_source target with
      | None -> Ok suspension
      | Some providers ->
          let* _ = execute suspension providers in
          Task_declarations.parser_suspension task.declarations
          |> Result.map_error (stream_diagnostics span)
  in
  let source =
    Session.add_source target.session ~path:"<StreamExePrint>" ~contents
  in
  Frontend.Symbol_visibility.Environment.without_locals
    (Session.symbols target.session) (fun () ->
      let* final_value = execute suspension source in
      Ok
        (Option.fold ~none:0L
           ~some:(function
             | Native_dispatch.I64 bits | Native_dispatch.U64 bits -> bits)
           final_value))

and run_input_execution ?suspension ?enclosing ?stream_task
    ?(use_active_stream = true) ?(active = fun () -> Ok ()) task ~source =
  let ( let* ) = Result.bind in
  let* () =
    match
      Common.Source_manager.find
        (Session.sources task.session)
        (Common.Source_file.id source)
    with
    | Some registered when registered == source -> Ok ()
    | _ ->
        Error
          [
            Integer_source.diagnostic
              ~span:(Integer_source.source_span source)
              "HCRUN0004" "task input is not the exact registered source";
          ]
  in
  let execute_command command =
    match task.native_dispatch with
    | Some _ -> execute_source ~use_active_stream task command
    | None ->
        execute_internal ~use_active_stream task command
        |> Result.map (fun execution ->
            Option.map native_word_of_vm (VM.final_value execution))
  in
  let commands, completed =
    execution_commands task
      (Integer_source.source_span source)
      ~use_active_stream ~active ~execute_command
  in
  let execute_stream =
    match stream_task with
    | None -> stream_executor task
    | Some stream_task when stream_task == task -> stream_executor task
    | Some stream_task ->
        let saved =
          {
            compiler_session = task.session;
            compiler_declarations = task.declarations;
            compiler_task = Some task;
          }
        in
        stream_executor ~saved_compiler:saved stream_task
  in
  let* parsed =
    match (suspension, enclosing) with
    | None, _ ->
        Ok
          (Frontend.Parser.parse ~commands ~execute_stream
             ~sources:(Session.sources task.session)
             ~definitions:(Session.definitions task.session)
             ~symbols:(Session.symbols task.session)
             ~config:task.config source)
    | Some suspension, None ->
        Frontend.Parser.parse_suspended suspension ~commands ~execute_stream
          ~sources:(Session.sources task.session)
          ~definitions:(Session.definitions task.session)
          ~symbols:(Session.symbols task.session)
          ~config:task.config source
        |> Result.map_error (fun message ->
            [
              Integer_source.diagnostic
                ~span:(Integer_source.source_span source)
                "HCRUN0004" message;
            ])
    | Some suspension, Some enclosing ->
        Frontend.Parser.parse_suspended_enclosing suspension ~enclosing
          ~commands ~execute_stream
          ~sources:(Session.sources task.session)
          ~definitions:(Session.definitions task.session)
          ~symbols:(Session.symbols task.session)
          ~config:task.config source
        |> Result.map_error (fun message ->
            [
              Integer_source.diagnostic
                ~span:(Integer_source.source_span source)
                "HCRUN0004" message;
            ])
  in
  match parsed.ast with
  | None -> Error parsed.diagnostics
  | Some _ -> completed ()

let run_input ?suspension task ~source =
  run_input_execution ?suspension task ~source |> Result.map fst

let run task ~source =
  let ( let* ) = Result.bind in
  let* sequence = run_input task ~source in
  VM.task_input_result task.state ~sequence
  |> Result.map_error (fun message ->
      [
        Integer_source.diagnostic
          ~span:(Integer_source.source_span source)
          "HCRUN0004" message;
      ])

let run_suspended task ~source =
  let ( let* ) = Result.bind in
  let diagnose result =
    Result.map_error
      (fun message ->
        [
          Integer_source.diagnostic
            ~span:(Integer_source.source_span source)
            "HCRUN0004" message;
        ])
      result
  in
  let* suspension =
    Task_declarations.parser_suspension task.declarations |> diagnose
  in
  let* sequence = run_input ~suspension task ~source in
  VM.check_task_suspended_completion task.state ~suspension sequence |> diagnose
