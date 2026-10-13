module Source = Driver.Integer_source_execution
module Task = Driver.Integer_task
module Dispatch = Task.Native_dispatch
module Image = Backend.X86_64_program
module Native = Runtime.Native_program_execution

type word = { type_ : Image.word_type; bits : int64 }
type result = { final_value : word option }
type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }

type fragment_kind =
  | Initializer
  | Default
  | Internal_binding
  | Dimension
  | Switch_case
  | Offset
  | Metadata
  | Command
  | Aot_module

type image = {
  status_abi : Image.status_abi;
  ir_instructions : int;
  code_bytes : int;
  global_bytes : int;
  global_arena_bytes : int;
  literal_bytes : int;
  arena_metadata_bytes : int;
  entry_stack_bytes : int;
  function_count : int;
}

type fragment = {
  kind : fragment_kind;
  image : image;
  native_outcome : (Image.outcome, string) Stdlib.result option;
}

type switch_execution_meter = {
  remaining : unit -> int;
  charge_work : int -> (unit, string) Stdlib.result;
  span : Common.Span.t;
}

type initializer_execution_meter = {
  started_steps : int;
  remaining : unit -> int;
  initializer_span : Common.Span.t;
}

type static_copy = {
  cell_offset : int;
  byte_offset : int;
  byte_count : int;
  outcome : (unit, string) Stdlib.result;
}

type report = {
  outcome_ : (result checked, Common.Diagnostic.t list) Stdlib.result;
  fragments_ : fragment list;
  static_copies_ : static_copy list;
  platform_ : Native.platform;
  executed_steps_ : int;
  preparation_steps_ : int;
  default_bytes_ : int;
  dimension_work_ : int;
  switch_work_ : int;
  output_bytes_ : string;
  output_work_ : int;
  source_progress_ : Task.progress option;
  compiler_exceptions_ : Frontend.Parser.compiler_exception list;
}

let ( let* ) = Result.bind

let word_of_dispatch = function
  | Dispatch.I64 bits -> { type_ = Image.I64; bits }
  | Dispatch.U64 bits -> { type_ = Image.U64; bits }

let dispatch_of_word (word : Image.word) =
  match word.type_ with
  | Image.I64 -> Dispatch.I64 word.bits
  | Image.U64 -> Dispatch.U64 word.bits

let describe_image image =
  {
    status_abi = Image.status_abi image;
    ir_instructions = Image.ir_instructions image;
    code_bytes = Image.code_bytes image;
    global_bytes = Image.global_bytes image;
    global_arena_bytes = Image.arena_bytes image;
    literal_bytes = Image.literal_bytes image;
    arena_metadata_bytes = Image.arena_metadata_bytes image;
    entry_stack_bytes = Image.entry_stack_bytes image;
    function_count = Image.function_count image;
  }

let diagnostic ~span code message =
  Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
    ~primary:span ()

let image_errors ~span errors =
  List.map
    (fun (error : Image.error) ->
      diagnostic
        ~span:(Option.value ~default:span error.span)
        error.code error.message)
    errors

let evaluate ?(max_ir_instructions = 4096) ?(max_code_bytes = 65_536)
    ?(max_stack_bytes = Image.hard_max_stack_bytes) ?(max_blocks = 4096)
    ?(max_initializer_steps = 100_000) ?(max_default_bytes = 65_536)
    ?(max_switch_work = 100_000) ?(max_dimension_work = 100_000)
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576)
    ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128)
    ?(max_output_bytes = 1_048_576) ?(max_output_work = 1_048_576)
    ?(max_active_stack_bytes = Native.hard_max_active_stack_bytes) ?status_abi
    session ~config ~source ~max_steps =
  let span = Driver.Integer_source.source_span source in
  let platform_ = Native.platform () in
  let host_error message =
    [ Native_program.host_diagnostic ~span platform_ message ]
  in
  let validate =
    let errors = image_errors ~span in
    let* () =
      if
        max_steps <= 0 || max_initializer_steps <= 0 || max_default_bytes <= 0
        || max_switch_work <= 0 || max_dimension_work <= 0
        || max_frame_bytes <= 0 || max_call_depth <= 0 || max_output_work <= 0
        || max_output_bytes <= 0
        || max_output_bytes > Native.hard_max_output_bytes
        || Frontend.Preprocessor.Config.max_generated_bytes config
           > Native.hard_max_output_bytes
        || max_active_stack_bytes <= 0
        || max_active_stack_bytes > Native.hard_max_active_stack_bytes
      then
        Error
          [
            diagnostic ~span "HCIRVM0001"
              "native source task resource limits are outside their positive \
               host bounds";
          ]
      else Ok ()
    in
    let* () =
      Image.validate_limits ~max_ir_instructions ~max_code_bytes
      |> Result.map_error errors
    in
    let* () =
      Image.validate_stack_limit ~max_stack_bytes |> Result.map_error errors
    in
    let* () =
      Image.validate_block_limit ~max_blocks |> Result.map_error errors
    in
    let* () =
      Image.validate_global_limit ~max_global_bytes |> Result.map_error errors
    in
    let* () =
      Image.validate_literal_limit ~max_literal_bytes |> Result.map_error errors
    in
    let* () =
      match (platform_, status_abi) with
      | Native.Unsupported, _ ->
          Error
            (host_error "native source execution is unsupported on this host")
      | Native.Windows_x86_64, Some Image.System_v_x64
      | Native.Linux_x86_64, Some Image.Windows_x64 ->
          Error (host_error "native source task ABI does not match the host")
      | _ -> Ok ()
    in
    let* layout =
      Image.create_task_layout_with_literals ~max_global_bytes
        ~max_literal_bytes
      |> Result.map_error errors
    in
    let* budget =
      Native.create_budget ~max_steps ~max_output_bytes ~max_output_work ()
      |> Result.map_error host_error
    in
    let max_arena_bytes =
      let global_bound =
        min Native.hard_max_arena_bytes (9 * max_global_bytes)
      in
      let literal_bound =
        min Native.hard_max_arena_bytes (65 * max_literal_bytes)
      in
      let saved_bound =
        32 * min (Native.hard_max_arena_bytes / 32) (max_default_bytes / 8)
      in
      global_bound
      + min
          (Native.hard_max_arena_bytes - global_bound)
          (literal_bound + saved_bound
          + min 1_600_000 (16 * max_ir_instructions))
    in
    let* arena =
      Native.create_task_arena ~max_arena_bytes layout
      |> Result.map_error host_error
    in
    Ok (layout, budget, arena)
  in
  match validate with
  | Error diagnostics ->
      {
        outcome_ = Error diagnostics;
        fragments_ = [];
        static_copies_ = [];
        platform_;
        executed_steps_ = 0;
        preparation_steps_ = 0;
        default_bytes_ = 0;
        dimension_work_ = 0;
        switch_work_ = 0;
        output_bytes_ = "";
        output_work_ = 0;
        source_progress_ = None;
        compiler_exceptions_ = [];
      }
  | Ok (layout, budget, arena) ->
      let contexts = ref [ (None, layout, arena) ] in
      let source_scopes = ref [] in
      let current_scope () =
        match !source_scopes with
        | [] -> None
        | scope :: _ -> Some scope
      in
      let context globals =
        match
          List.find_opt
            (fun (owner, _, _) ->
              Option.fold ~none:false
                ~some:(fun owner ->
                  Ir.Integer_globals.same_task_storage owner globals)
                owner)
            !contexts
        with
        | Some (_, layout, arena) -> Ok (layout, arena)
        | None -> (
            match !contexts with
            | (None, layout, arena) :: rest ->
                contexts := (Some globals, layout, arena) :: rest;
                Ok (layout, arena)
            | _ ->
                let* layout =
                  Image.create_task_layout_with_literals ~max_global_bytes
                    ~max_literal_bytes
                  |> Result.map_error (image_errors ~span)
                in
                let* arena =
                  Native.create_task_arena layout |> Result.map_error host_error
                in
                contexts := !contexts @ [ (Some globals, layout, arena) ];
                Ok (layout, arena))
      in
      let task_storage_bytes () =
        List.fold_left
          (fun (globals, literals) (_, layout, _) ->
            ( globals
              + Backend.X86_64_global_storage.task_layout_global_bytes layout,
              literals
              + Backend.X86_64_global_storage.task_layout_literal_bytes layout
            ))
          (0, 0) !contexts
      in
      let check_storage () =
        let globals, literals = task_storage_bytes () in
        if globals > max_global_bytes then
          Error
            [
              diagnostic ~span "HCBACK0001"
                "native source catalogs exceed the cumulative global byte limit";
            ]
        else if literals > max_literal_bytes then
          Error
            [
              diagnostic ~span "HCBACK0001"
                "native source catalogs exceed the cumulative literal byte \
                 limit";
            ]
        else Ok ()
      in
      let fragments = ref [] in
      let other_global_bytes arena =
        List.fold_left
          (fun bytes (_, layout, owner) ->
            if owner == arena then bytes
            else
              bytes
              + Backend.X86_64_global_storage.task_layout_global_bytes layout)
          0 !contexts
      in
      let literal_remaining () =
        max_literal_bytes
        - List.fold_left
            (fun bytes (_, layout, _) ->
              bytes
              + Backend.X86_64_global_storage.task_layout_literal_bytes layout)
            0 !contexts
      in
      let static_copies = ref [] in
      let default_bytes = ref 0 in
      let emitted_bytes = ref 0 in
      let emitted_ir = ref 0 in
      let cleanup_errors = ref [] in
      let source_report = ref None in
      let compiler_exceptions_rev = ref [] in
      let compiler_exception exception_ =
        compiler_exceptions_rev := exception_ :: !compiler_exceptions_rev
      in
      let aot_task = ref None in
      let aot_preparation = ref 0 in
      let aot_switch_work = ref 0 in
      let aot_dimension_work = ref 0 in
      let remaining compile =
        if !emitted_bytes >= max_code_bytes then
          Error
            [
              diagnostic ~span "HCBACK0005"
                "native source fragments exceed the cumulative code byte limit";
            ]
        else if !emitted_ir >= max_ir_instructions then
          Error
            [
              diagnostic ~span "HCBACK0001"
                "native source fragments exceed the cumulative IR instruction \
                 limit";
            ]
        else
          let* image =
            compile
              ~max_ir_instructions:(max_ir_instructions - !emitted_ir)
              ~max_code_bytes:(max_code_bytes - !emitted_bytes)
            |> Result.map_error (image_errors ~span)
          in
          emitted_bytes := !emitted_bytes + Image.code_bytes image;
          emitted_ir := !emitted_ir + Image.ir_instructions image;
          Ok image
      in
      let active_switch_execution : switch_execution_meter option ref =
        ref None
      in
      let initializer_executions = ref [] in
      let native_steps () = (Native.budget_progress budget).executed_steps in
      let pending_initializer_steps () =
        let current = native_steps () in
        List.fold_left
          (fun work (phase : initializer_execution_meter) ->
            let pending = current - phase.started_steps in
            if pending < 0 || pending > max_int - work then
              invalid_arg
                "native initializer reservation moved its physical step \
                 boundary";
            work + pending)
          0 !initializer_executions
      in
      let physical_initializer_remaining () =
        match !initializer_executions with
        | [] -> None
        | phase :: _ ->
            Some
              (max 0 (phase.remaining ()) / List.length !initializer_executions)
      in
      let with_initializer_execution ~span ~remaining image run =
        let phase =
          {
            started_steps = native_steps ();
            remaining;
            initializer_span = span;
          }
        in
        let saved = !initializer_executions in
        initializer_executions := phase :: saved;
        Fun.protect
          ~finally:(fun () -> initializer_executions := saved)
          (fun () ->
            match Image.generation image with
            | None ->
                Error
                  [
                    diagnostic ~span "HCIRVM0026"
                      "native initializer has no original source generation";
                  ]
            | Some generation ->
                let* result =
                  Ir.Integer_interpreter.with_native_initializer_reservation
                    generation ~pending:pending_initializer_steps run
                  |> Result.map_error (fun message ->
                      [ Driver.Integer_source.message_diagnostic ~span message ])
                in
                result)
      in
      let execute ?max_activation_steps ?(arena = arena) kind image =
        let* () = check_storage () in
        let switch_meter = !active_switch_execution in
        let* max_activation_steps =
          let switch_limit =
            Option.map
              (fun (meter : switch_execution_meter) -> meter.remaining ())
              switch_meter
          and initializer_limit = physical_initializer_remaining () in
          let available =
            match (switch_limit, initializer_limit) with
            | None, None -> None
            | Some limit, None | None, Some limit -> Some limit
            | Some switch, Some preparation -> Some (min switch preparation)
          in
          match available with
          | None -> Ok max_activation_steps
          | Some available when available <= 0 ->
              let switch_exhausted =
                Option.fold ~none:false
                  ~some:(fun (meter : switch_execution_meter) ->
                    meter.remaining () = 0)
                  switch_meter
              in
              let fault_span =
                if switch_exhausted then
                  Option.fold ~none:span
                    ~some:(fun meter -> meter.span)
                    switch_meter
                else
                  match !initializer_executions with
                  | phase :: _ -> phase.initializer_span
                  | [] -> span
              in
              Error
                [
                  diagnostic ~span:fault_span
                    (if switch_exhausted then "HCSW0003" else "HCIRVM0007")
                    (if switch_exhausted then
                       "switch case preparation exhausted max_switch_work"
                     else
                       "the task initializer preparation step limit was \
                        exhausted");
                ]
          | Some available ->
              Ok
                (Some
                   (min available
                      (Option.value ~default:available max_activation_steps)))
        in
        let accounted_steps =
          ref (Native.budget_progress budget).executed_steps
        in
        let charge_switch_work () =
          let current = (Native.budget_progress budget).executed_steps in
          let steps = current - !accounted_steps in
          let* () =
            match switch_meter with
            | None -> Ok ()
            | Some meter ->
                meter.charge_work steps
                |> Result.map_error (fun message ->
                    [
                      Driver.Integer_source.message_diagnostic ~span:meter.span
                        message;
                    ])
          in
          accounted_steps := current;
          Ok ()
        in
        let scope = current_scope () in
        let metadata = describe_image image in
        let active_stream_context =
          Option.fold ~none:false
            ~some:(fun generation ->
              match
                Ir.Integer_interpreter.native_generation_limits generation
              with
              | Ok (active, _, _) -> active
              | Error _ -> false)
            (Image.generation image)
        in
        let retained =
          match kind with
          | Aot_module ->
              Native.retain ~max_global_bytes ~max_literal_bytes
                ~max_active_stack_bytes image
          | _ ->
              Native.retain_task_fragment ?scope ~max_global_bytes
                ~max_literal_bytes ~max_active_stack_bytes arena image
        in
        match retained with
        | Error message ->
            fragments :=
              { kind; image = metadata; native_outcome = None } :: !fragments;
            Error (host_error message)
        | Ok retained -> (
            let release_error = ref None in
            let source_errors = ref None in
            let source_callback =
              Option.map
                (fun callback scope contents ->
                  let saved = !source_scopes in
                  source_scopes := scope :: saved;
                  Fun.protect
                    ~finally:(fun () -> source_scopes := saved)
                    (fun () ->
                      let reached =
                        Fun.protect
                          ~finally:(fun () ->
                            accounted_steps :=
                              (Native.budget_progress budget).executed_steps)
                          (fun () ->
                            let* () = charge_switch_work () in
                            let* result =
                              callback scope contents
                              |> Result.map_error host_error
                            in
                            result)
                      in
                      let limited =
                        let switch =
                          Option.map
                            (fun (meter : switch_execution_meter) ->
                              meter.remaining ())
                            switch_meter
                        and preparation = physical_initializer_remaining () in
                        match (switch, preparation) with
                        | None, None -> Ok ()
                        | Some limit, None | None, Some limit ->
                            Ir.Native_source_suspension.restrict_steps scope
                              ~remaining:limit
                            |> Result.map_error host_error
                        | Some switch, Some preparation ->
                            Ir.Native_source_suspension.restrict_steps scope
                              ~remaining:(min switch preparation)
                            |> Result.map_error host_error
                      in
                      let reached =
                        let* value = reached in
                        let* () = limited in
                        Ok value
                      in
                      match reached with
                      | Ok value -> Some value
                      | Error diagnostics ->
                          source_errors := Some diagnostics;
                          None))
                (Image.source_callback image)
            in
            let execution =
              Fun.protect
                ~finally:(fun () ->
                  match Native.release retained with
                  | Ok () -> ()
                  | Error message -> release_error := Some message)
                (fun () ->
                  Native.execute_retained_budget_report ?scope
                    ?max_activation_steps ?source_callback ~max_frame_bytes
                    ~max_call_depth ~max_active_stack_bytes ~max_global_bytes
                    ~max_literal_bytes budget retained)
            in
            let native_outcome = Native.outcome execution in
            fragments :=
              { kind; image = metadata; native_outcome = Some native_outcome }
              :: !fragments;
            let reached =
              let* () = charge_switch_work () in
              match native_outcome with
              | Error message -> Error (host_error message)
              | Ok (Image.Fault fault) ->
                  let error =
                    Native_program.fault_diagnostic ~fallback:span fault
                  in
                  let error =
                    if
                      fault.kind = Image.Stream_exe_context_required
                      && active_stream_context
                    then
                      {
                        error with
                        message =
                          "native StreamExePrint requires the synchronous \
                           parser bridge";
                      }
                    else error
                  in
                  Error (Option.value !source_errors ~default:[] @ [ error ])
              | Ok (Image.Completed completed) ->
                  Ok (completed, Native.value_captured execution)
            in
            let reached =
              match switch_meter with
              | Some meter when meter.remaining () = 0 ->
                  Result.map_error
                    (List.map (fun (item : Common.Diagnostic.t) ->
                         if item.code = "HCIRVM0007" then
                           {
                             item with
                             code = "HCSW0003";
                             message =
                               "switch case preparation exhausted \
                                max_switch_work";
                           }
                         else item))
                    reached
              | _ -> reached
            in
            match (reached, !release_error) with
            | reached, None -> reached
            | Ok _, Some message -> Error (host_error message)
            | Error diagnostics, Some message ->
                Error (diagnostics @ host_error message))
      in
      let native_dispatch : Dispatch.t =
        {
          execute_initializer =
            (fun request ->
              let* layout, arena =
                context
                  (Dispatch.initializer_program request
                  |> Ir.Initializer_fragment_program.destination
                  |> Ir.Initializer_fragment_destination.globals)
              in
              let* image =
                remaining (fun ~max_ir_instructions ~max_code_bytes ->
                    Image.compile_task_initializer ?status_abi ~max_stack_bytes
                      ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                      request)
              in
              let before = (Native.budget_progress budget).executed_steps in
              let* _completed, _ = execute ~arena Initializer image in
              let reached =
                (Native.budget_progress budget).executed_steps - before
              in
              if
                not
                  (Ir.Initializer_fragment_program.is_scalar_evaluation
                     (Dispatch.initializer_program request))
              then Ok ()
              else
                let diagnose result =
                  Result.map_error
                    (fun message ->
                      let span =
                        Dispatch.initializer_program request
                        |> Ir.Initializer_fragment_program.destination
                        |> Ir.Initializer_fragment_destination.span
                      in
                      [ Driver.Integer_source.message_diagnostic ~span message ])
                    result
                in
                let* capture =
                  Native.finish_task_initializer ?scope:(current_scope ()) arena
                    image
                  |> diagnose
                in
                let* token =
                  Dispatch.prepare_initializer_copy request capture
                    ~work:reached
                  |> diagnose
                in
                let* receipt =
                  Native.copy_task_initializer ?scope:(current_scope ()) arena
                    request token
                  |> diagnose
                in
                Dispatch.finish_initializer_copy request token receipt
                |> diagnose);
          execute_command =
            (fun request ->
              let* layout, arena =
                context
                  (Dispatch.command_program request
                  |> Driver.Integer_unit.globals)
              in
              let* image =
                remaining (fun ~max_ir_instructions ~max_code_bytes ->
                    Image.compile_task_command ?status_abi ~max_stack_bytes
                      ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                      request)
              in
              let* execution, captured = execute ~arena Command image in
              Ok
                (if captured then
                   Dispatch.Captured
                     (Option.map dispatch_of_word execution.final_value)
                 else Dispatch.Unchanged));
        }
      in
      let outcome_ =
        let native_static_initializer request =
          let* layout, arena =
            context
              (Task.Native_static_initializer.program request
              |> Ir.Static_initializer_program.destination
              |> Ir.Static_initializer_destination.globals)
          in
          let* image =
            remaining (fun ~max_ir_instructions ~max_code_bytes ->
                Image.compile_task_static_initializer ?status_abi
                  ~max_stack_bytes ~max_blocks ~max_ir_instructions
                  ~max_code_bytes ~layout request)
          in
          let before = (Native.budget_progress budget).executed_steps in
          let module Request = Task.Native_static_initializer in
          let dynamic =
            Ir.Static_initializer_program.is_scalar_evaluation
              (Request.program request)
          in
          let initializer_span =
            Request.program request |> Ir.Static_initializer_program.destination
            |> Ir.Static_initializer_destination.span
          in
          let outcome =
            if not dynamic then execute ~arena Initializer image
            else
              with_initializer_execution ~span:initializer_span
                ~remaining:(fun () -> Request.initializer_remaining request)
                image
                (fun () -> execute ~arena Initializer image)
          in
          let work = (Native.budget_progress budget).executed_steps - before in
          let* () =
            if (not dynamic) || (work = 0 && Result.is_error outcome) then Ok ()
            else
              Request.record_steps request work
              |> Result.map_error (fun message ->
                  [
                    Driver.Integer_source.message_diagnostic
                      ~span:initializer_span message;
                  ])
          in
          let* _ = outcome in
          if
            not
              (Ir.Static_initializer_program.is_scalar_evaluation
                 (Request.program request))
          then Ok ()
          else
            let diagnose result =
              Result.map_error
                (fun message ->
                  let span =
                    Request.program request
                    |> Ir.Static_initializer_program.destination
                    |> Ir.Static_initializer_destination.span
                  in
                  [ Driver.Integer_source.message_diagnostic ~span message ])
                result
            in
            let* capture =
              Native.finish_task_static_initializer ?scope:(current_scope ())
                arena image
              |> diagnose
            in
            let* token =
              Request.prepare_copy request capture ~work |> diagnose
            in
            let* receipt =
              Native.copy_task_static_initializer ?scope:(current_scope ())
                arena request token
              |> diagnose
            in
            Request.finish_copy request token receipt |> diagnose
        in
        let native_default request =
          let module Request = Task.Native_default in
          let* layout, arena =
            context
              (Request.program request
             |> Ir.Default_fragment_program.destination
             |> Ir.Default_fragment_destination.globals)
          in
          let span =
            Request.program request |> Ir.Default_fragment_program.destination
            |> Ir.Default_fragment_destination.span
          in
          let allowance = Request.initializer_remaining request in
          if max_default_bytes - !default_bytes < 8 then
            Error
              [
                diagnostic ~span "HCIRVM0011"
                  "native saved-default payload exceeds max_default_bytes";
              ]
          else if allowance <= 0 then
            Error
              [
                diagnostic ~span "HCIRVM0007"
                  "the task default preparation step limit was exhausted";
              ]
          else
            let* image =
              remaining (fun ~max_ir_instructions ~max_code_bytes ->
                  Image.compile_task_default ?status_abi ~max_stack_bytes
                    ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                    request)
            in
            let before = (Native.budget_progress budget).executed_steps in
            let outcome =
              with_initializer_execution ~span
                ~remaining:(fun () -> Request.initializer_remaining request)
                image
                (fun () ->
                  execute ~arena ~max_activation_steps:allowance Default image)
            in
            let steps =
              (Native.budget_progress budget).executed_steps - before
            in
            let copy_steps = ref 0 in
            let saved =
              let* completed, captured = outcome in
              match
                ( captured,
                  completed.final_value,
                  completed.captured_callback,
                  completed.captured_data )
              with
              | true, None, None, Some value ->
                  let result, work =
                    Native.finish_task_data_default ?scope:(current_scope ())
                      arena image value
                      ~max_copy_bytes:(max 0 (literal_remaining ()))
                      ~max_copy_steps:
                        (max 0
                           (min (allowance - steps)
                              (Request.initializer_remaining request - steps)))
                  in
                  copy_steps := work;
                  result
                  |> Result.map_error (fun message ->
                      [ Driver.Integer_source.message_diagnostic ~span message ])
              | true, None, Some value, None -> Ok value
              | true, Some word, None, None ->
                  Ok (Ir.Saved_parameter_value.word word.bits)
              | _ ->
                  Error
                    [
                      diagnostic ~span "HCIRVM0026"
                        "native default produced no captured expression value";
                    ]
            in
            let* () =
              (if steps = 0 && Result.is_error outcome then Ok ()
               else Request.record_steps request (steps + !copy_steps))
              |> Result.map_error (fun message ->
                  [ Driver.Integer_source.message_diagnostic ~span message ])
            in
            let* value = saved in
            default_bytes := !default_bytes + 8;
            Ok value
        in
        let native_internal_binding request =
          let module Request = Task.Native_internal_binding in
          let* layout, arena =
            context
              (Request.program request
             |> Ir.Internal_binding_fragment_program.destination
             |> Ir.Internal_binding_fragment_destination.globals)
          in
          let binding_span =
            Request.program request
            |> Ir.Internal_binding_fragment_program.destination
            |> Ir.Internal_binding_fragment_destination.span
          in
          let allowance = Request.initializer_remaining request in
          if allowance <= 0 then
            Error
              [
                diagnostic ~span:binding_span "HCIRVM0007"
                  "the task internal binding preparation step limit was \
                   exhausted";
              ]
          else
            let* image =
              remaining (fun ~max_ir_instructions ~max_code_bytes ->
                  Image.compile_task_internal_binding ?status_abi
                    ~max_stack_bytes ~max_blocks ~max_ir_instructions
                    ~max_code_bytes ~layout request)
            in
            let before = (Native.budget_progress budget).executed_steps in
            let outcome =
              with_initializer_execution ~span:binding_span
                ~remaining:(fun () -> Request.initializer_remaining request)
                image
                (fun () ->
                  execute ~arena ~max_activation_steps:allowance
                    Internal_binding image)
            in
            let steps =
              (Native.budget_progress budget).executed_steps - before
            in
            let* () =
              (if steps = 0 && Result.is_error outcome then Ok ()
               else Request.record_steps request steps)
              |> Result.map_error (fun message ->
                  [
                    Driver.Integer_source.message_diagnostic ~span:binding_span
                      message;
                  ])
            in
            let* completed, captured = outcome in
            match
              ( captured,
                completed.final_value,
                completed.captured_callback,
                completed.captured_data )
            with
            | true, Some _, None, None ->
                Native.finish_task_internal_binding ?scope:(current_scope ())
                  arena image
                |> Result.map_error (fun message ->
                    [
                      Driver.Integer_source.message_diagnostic
                        ~span:binding_span message;
                    ])
            | _ ->
                Error
                  [
                    diagnostic ~span:binding_span "HCIRVM0026"
                      "native internal binding produced no captured scalar \
                       expression";
                  ]
        in
        let native_dimension request =
          let module Request = Task.Native_dimension in
          let* layout, arena =
            context
              (Request.program request
             |> Ir.Dimension_fragment_program.destination
             |> Ir.Dimension_fragment_destination.globals)
          in
          let dimension_span =
            Request.program request |> Ir.Dimension_fragment_program.destination
            |> Ir.Dimension_fragment_destination.span
          in
          let allowance = Request.initializer_remaining request in
          if allowance <= 0 then
            Error
              [
                diagnostic ~span:dimension_span "HCIRVM0007"
                  "the task dimension preparation step limit was exhausted";
              ]
          else
            let* image =
              remaining (fun ~max_ir_instructions ~max_code_bytes ->
                  Image.compile_task_dimension ?status_abi ~max_stack_bytes
                    ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                    request)
            in
            let before = (Native.budget_progress budget).executed_steps in
            let outcome =
              with_initializer_execution ~span:dimension_span
                ~remaining:(fun () -> Request.initializer_remaining request)
                image
                (fun () ->
                  execute ~arena ~max_activation_steps:allowance Dimension image)
            in
            let steps =
              (Native.budget_progress budget).executed_steps - before
            in
            let* () =
              (if steps = 0 && Result.is_error outcome then Ok ()
               else Request.record_steps request steps)
              |> Result.map_error (fun message ->
                  [
                    Driver.Integer_source.message_diagnostic
                      ~span:dimension_span message;
                  ])
            in
            let* completed, captured = outcome in
            match
              ( captured,
                completed.final_value,
                completed.captured_callback,
                completed.captured_data )
            with
            | true, Some _, None, None ->
                Native.finish_task_dimension ?scope:(current_scope ()) arena
                  image
                |> Result.map_error (fun message ->
                    [
                      Driver.Integer_source.message_diagnostic
                        ~span:dimension_span message;
                    ])
            | _ ->
                Error
                  [
                    diagnostic ~span:dimension_span "HCIRVM0026"
                      "native dimension produced no captured scalar expression";
                  ]
        in
        let native_switch_case request =
          let module Request = Task.Native_switch_case in
          let module Destination = Ir.Switch_case_fragment_destination in
          let module Program = Ir.Switch_case_fragment_program in
          let* layout, arena =
            context
              (Request.program request |> Program.destination
             |> Destination.globals)
          in
          let switch_span =
            Request.program request |> Program.destination |> Destination.span
          in
          let switch_remaining = Request.switch_remaining request in
          let initializer_remaining = Request.initializer_remaining request in
          let allowance = min switch_remaining initializer_remaining in
          if allowance <= 0 then
            Error
              [
                diagnostic ~span:switch_span
                  (if switch_remaining = 0 then "HCSW0003" else "HCIRVM0007")
                  (if switch_remaining = 0 then
                     "switch case execution exhausted max_switch_work"
                   else
                     "the task initializer preparation step limit was exhausted");
              ]
          else
            let* image =
              remaining (fun ~max_ir_instructions ~max_code_bytes ->
                  Image.compile_task_switch_case ?status_abi ~max_stack_bytes
                    ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                    request)
            in
            let before = (Native.budget_progress budget).executed_steps in
            let outcome =
              let saved_meter = !active_switch_execution in
              active_switch_execution :=
                Some
                  {
                    remaining = (fun () -> Request.switch_remaining request);
                    charge_work = Request.charge_work request;
                    span = switch_span;
                  };
              Fun.protect
                ~finally:(fun () -> active_switch_execution := saved_meter)
                (fun () ->
                  with_initializer_execution ~span:switch_span
                    ~remaining:(fun () -> Request.initializer_remaining request)
                    image
                    (fun () ->
                      execute ~arena ~max_activation_steps:allowance Switch_case
                        image))
            in
            let steps =
              (Native.budget_progress budget).executed_steps - before
            in
            let* () =
              (if steps = 0 && Result.is_error outcome then Ok ()
               else Request.record_steps request steps)
              |> Result.map_error (fun message ->
                  [
                    Driver.Integer_source.message_diagnostic ~span:switch_span
                      message;
                  ])
            in
            let* completed, captured = outcome in
            match
              ( captured,
                completed.final_value,
                completed.captured_callback,
                completed.captured_data )
            with
            | true, Some _, None, None ->
                Native.finish_task_switch_case ?scope:(current_scope ()) arena
                  image
                |> Result.map_error (fun message ->
                    [
                      Driver.Integer_source.message_diagnostic ~span:switch_span
                        message;
                    ])
            | _ ->
                Error
                  [
                    diagnostic ~span:switch_span "HCIRVM0026"
                      "native switch case produced no captured scalar \
                       expression";
                  ]
        in
        let native_offset request =
          let module Request = Task.Native_offset in
          let* layout, arena =
            context
              (Request.program request |> Ir.Offset_fragment_program.destination
             |> Ir.Offset_fragment_destination.globals)
          in
          let offset_span =
            Request.program request |> Ir.Offset_fragment_program.destination
            |> Ir.Offset_fragment_destination.span
          in
          let allowance = Request.initializer_remaining request in
          if allowance <= 0 then
            Error
              [
                diagnostic ~span:offset_span "HCIRVM0007"
                  "the task offset preparation step limit was exhausted";
              ]
          else
            let* image =
              remaining (fun ~max_ir_instructions ~max_code_bytes ->
                  Image.compile_task_offset ?status_abi ~max_stack_bytes
                    ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                    request)
            in
            let before = (Native.budget_progress budget).executed_steps in
            let outcome =
              with_initializer_execution ~span:offset_span
                ~remaining:(fun () -> Request.initializer_remaining request)
                image
                (fun () ->
                  execute ~arena ~max_activation_steps:allowance Offset image)
            in
            let steps =
              (Native.budget_progress budget).executed_steps - before
            in
            let* () =
              (if steps = 0 && Result.is_error outcome then Ok ()
               else Request.record_steps request steps)
              |> Result.map_error (fun message ->
                  [
                    Driver.Integer_source.message_diagnostic ~span:offset_span
                      message;
                  ])
            in
            let* completed, captured = outcome in
            match
              ( captured,
                completed.final_value,
                completed.captured_callback,
                completed.captured_data )
            with
            | true, Some _, None, None ->
                Native.finish_task_offset ?scope:(current_scope ()) arena image
                |> Result.map_error (fun message ->
                    [
                      Driver.Integer_source.message_diagnostic ~span:offset_span
                        message;
                    ])
            | _ ->
                Error
                  [
                    diagnostic ~span:offset_span "HCIRVM0026"
                      "native offset produced no captured scalar expression";
                  ]
        in
        let native_member_metadata request =
          let module Request = Task.Native_member_metadata in
          let* layout, arena =
            context
              (Request.program request
             |> Ir.Member_metadata_fragment_program.destination
             |> Ir.Member_metadata_fragment_destination.globals)
          in
          let metadata_span =
            Request.program request
            |> Ir.Member_metadata_fragment_program.destination
            |> Ir.Member_metadata_fragment_destination.span
          in
          let allowance = Request.initializer_remaining request in
          if allowance <= 0 then
            Error
              [
                diagnostic ~span:metadata_span "HCIRVM0007"
                  "the task member metadata preparation step limit was \
                   exhausted";
              ]
          else
            let* image =
              remaining (fun ~max_ir_instructions ~max_code_bytes ->
                  Image.compile_task_member_metadata ?status_abi
                    ~max_stack_bytes ~max_blocks ~max_ir_instructions
                    ~max_code_bytes ~layout request)
            in
            let before = (Native.budget_progress budget).executed_steps in
            let outcome =
              with_initializer_execution ~span:metadata_span
                ~remaining:(fun () -> Request.initializer_remaining request)
                image
                (fun () ->
                  execute ~arena ~max_activation_steps:allowance Metadata image)
            in
            let steps =
              (Native.budget_progress budget).executed_steps - before
            in
            let* () =
              (if steps = 0 && Result.is_error outcome then Ok ()
               else Request.record_steps request steps)
              |> Result.map_error (fun message ->
                  [
                    Driver.Integer_source.message_diagnostic ~span:metadata_span
                      message;
                  ])
            in
            let* completed, captured = outcome in
            match
              ( captured,
                completed.final_value,
                completed.captured_callback,
                completed.captured_data )
            with
            | true, Some _, None, None ->
                Native.finish_task_member_metadata ?scope:(current_scope ())
                  arena image
                |> Result.map_error (fun message ->
                    [
                      Driver.Integer_source.message_diagnostic
                        ~span:metadata_span message;
                    ])
            | _ ->
                Error
                  [
                    diagnostic ~span:metadata_span "HCIRVM0026"
                      "native member metadata produced no captured scalar \
                       expression";
                  ]
        in
        let native_static_allocation request =
          let* _, arena =
            context (Task.Native_static_allocation.context request)
          in
          let* () =
            Native.allocate_task_static ?scope:(current_scope ()) arena request
              ~max_global_bytes:(max_global_bytes - other_global_bytes arena)
            |> Result.map_error (fun message ->
                [ Driver.Integer_source.message_diagnostic ~span message ])
          in
          check_storage ()
        in
        let native_static_copy request =
          let module Destination = Ir.Static_initializer_destination in
          let destination = Task.Native_static_copy.destination request in
          let* _, arena = context (Destination.globals destination) in
          let* () = check_storage () in
          let byte_count =
            Option.value ~default:0 (Destination.copy_byte_count destination)
          in
          let outcome =
            Native.copy_task_static ?scope:(current_scope ()) arena request
          in
          static_copies :=
            {
              cell_offset = Destination.cell_offset destination;
              byte_offset = Destination.byte_offset destination;
              byte_count;
              outcome;
            }
            :: !static_copies;
          outcome
          |> Result.map_error (fun message ->
              [
                Driver.Integer_source.message_diagnostic
                  ~span:(Destination.span destination)
                  message;
              ])
        in
        Fun.protect
          ~finally:(fun () ->
            List.iter
              (fun (_, _, arena) ->
                match Native.release_task_arena arena with
                | Ok () -> ()
                | Error message ->
                    cleanup_errors := !cleanup_errors @ host_error message)
              !contexts)
          (fun () ->
            match Frontend.Preprocessor.Config.compilation_mode config with
            | Jit ->
                let report =
                  Source.run ~native_dispatch ~native_static_allocation
                    ~native_static_initializer ~native_static_copy
                    ~native_default ~native_dimension ~native_switch_case
                    ~native_offset ~native_member_metadata
                    ~native_internal_binding ~max_dimension_work
                    ~max_switch_work ~max_initializer_steps ~max_global_bytes
                    ~max_literal_bytes ~max_frame_bytes ~max_call_depth
                    ~max_output_bytes ~max_output_work session ~config ~source
                    ~max_steps
                in
                source_report := Some report;
                let* checked = Source.outcome report in
                if Option.is_some (Source.program report) then
                  Error
                    [
                      diagnostic ~span "HCRUN0004"
                        "native source execution returned an isolated command";
                    ]
                else
                  Ok
                    {
                      value =
                        {
                          final_value =
                            Option.map word_of_dispatch
                              (Source.native_final_value report);
                        };
                      diagnostics = checked.Driver.Integer_unit.diagnostics;
                    }
            | Aot ->
                (* Freeze the directive task before the outer parser publishes
                 any module declarations. It owns a separate original table. *)
                let task_session = Driver.Session.fork_frontend session in
                let streams ledger preparation =
                  let saved_compiler = Task.saved_compiler session ~ledger in
                  let* task =
                    Task.create
                      ~compiler_positions:
                        (Driver.Task_declarations.compiler_positions ledger)
                      ~switch_budget:
                        (Driver.Task_declarations.switch_budget ledger)
                      ~max_steps ~max_initializer_steps ~max_global_bytes
                      ~max_literal_bytes ~max_frame_bytes ~max_call_depth
                      ~max_output_bytes ~max_output_work
                      ~max_generated_bytes:
                        (Frontend.Preprocessor.Config.max_generated_bytes config)
                      ~native_dispatch ~native_static_allocation
                      ~native_static_initializer ~native_static_copy
                      ~native_default ~native_dimension ~native_switch_case
                      ~native_offset ~native_member_metadata
                      ~native_internal_binding task_session
                    |> Result.map_error (fun message ->
                        [ diagnostic ~span "HCIRVM0001" message ])
                  in
                  aot_task := Some task;
                  let providers_installed = ref false in
                  let checkpoint () =
                    let module Preparation = Driver.Native_default_preparation
                    in
                    let work =
                      max
                        (Task.initializer_steps task)
                        (Preparation.work preparation)
                    in
                    let bytes =
                      max !default_bytes (Preparation.bytes preparation)
                    in
                    let* () =
                      Preparation.synchronize_work preparation ~work ~bytes
                      |> Result.map_error (fun message ->
                          [ diagnostic ~span "HCIRVM0007" message ])
                    in
                    default_bytes := bytes;
                    Task.synchronize_preparation_work task ~work
                    |> Result.map_error (fun message ->
                        [ diagnostic ~span "HCIRVM0007" message ])
                  in
                  let execute_stream directive =
                    let* () = checkpoint () in
                    let* () =
                      if !providers_installed then Ok ()
                      else
                        let* () = Source.install_providers task in
                        providers_installed := true;
                        Ok ()
                    in
                    Task.stream_executor ~saved_compiler
                      ~allow_stream_exe_print:true task directive
                  in
                  let remaining_code () =
                    if !emitted_bytes >= max_code_bytes then
                      Error
                        [
                          diagnostic ~span "HCBACK0005"
                            "native source fragments exceed the cumulative \
                             code byte limit";
                        ]
                    else if !emitted_ir >= max_ir_instructions then
                      Error
                        [
                          diagnostic ~span "HCBACK0001"
                            "native source fragments exceed the cumulative IR \
                             instruction limit";
                        ]
                    else
                      Ok
                        ( max_ir_instructions - !emitted_ir,
                          max_code_bytes - !emitted_bytes )
                  in
                  Ok
                    Native_program.
                      { execute_stream; checkpoint; remaining_code }
                in
                let* checked =
                  Native_program.compile_with_preparation ~compiler_exception
                    ~streams ~max_ir_instructions ~max_code_bytes
                    ~max_stack_bytes ~max_blocks ~max_initializer_steps
                    ~max_default_bytes ~max_switch_work ~max_dimension_work
                    ~max_global_bytes ~max_literal_bytes ?status_abi
                    ~preparation_steps:aot_preparation
                    ~switch_work:aot_switch_work
                    ~dimension_work:aot_dimension_work ~default_bytes session
                    ~config ~source
                in
                let complete_module =
                  let image = checked.Native_program.value in
                  emitted_bytes := !emitted_bytes + Image.code_bytes image;
                  emitted_ir := !emitted_ir + Image.ir_instructions image;
                  let task_globals, task_literals = task_storage_bytes () in
                  let* () =
                    if
                      Image.global_bytes image > max_global_bytes - task_globals
                    then
                      Error
                        [
                          diagnostic ~span "HCBACK0004"
                            "AOT module and directive task exceed the \
                             cumulative global byte limit";
                        ]
                    else if
                      Image.literal_bytes image
                      > max_literal_bytes - task_literals
                    then
                      Error
                        [
                          diagnostic ~span "HCBACK0004"
                            "AOT module and directive task exceed the \
                             cumulative literal byte limit";
                        ]
                    else Ok ()
                  in
                  let* completed, _captured = execute Aot_module image in
                  Ok
                    {
                      value =
                        {
                          final_value =
                            Option.map
                              (fun (word : Image.word) ->
                                { type_ = word.type_; bits = word.bits })
                              completed.final_value;
                        };
                      diagnostics = checked.diagnostics;
                    }
                in
                Result.map_error
                  (fun errors -> checked.diagnostics @ errors)
                  complete_module)
      in
      let outcome_ =
        match (outcome_, !cleanup_errors) with
        | outcome, [] -> outcome
        | Ok checked, errors -> Error (checked.diagnostics @ errors)
        | Error diagnostics, errors -> Error (diagnostics @ errors)
      in
      let progress = Native.budget_progress budget in
      {
        outcome_;
        fragments_ = List.rev !fragments;
        static_copies_ = List.rev !static_copies;
        platform_;
        executed_steps_ = progress.executed_steps;
        preparation_steps_ =
          (match !aot_task with
          | Some task -> max !aot_preparation (Task.initializer_steps task)
          | None ->
              Option.bind !source_report Source.preparation_work
              |> Option.value ~default:0);
        default_bytes_ = !default_bytes;
        dimension_work_ =
          (!aot_dimension_work
          +
          match !aot_task with
          | Some task -> Task.dimension_work task
          | None ->
              Option.fold ~none:0 ~some:Source.dimension_work !source_report);
        switch_work_ =
          (match !aot_task with
          | Some task -> max !aot_switch_work (Task.switch_work task)
          | None -> Option.fold ~none:0 ~some:Source.switch_work !source_report);
        output_bytes_ = Native.budget_output_bytes budget;
        output_work_ = progress.output_work;
        source_progress_ =
          (match !aot_task with
          | Some task -> Some (Task.progress task)
          | None -> Option.bind !source_report Source.progress);
        compiler_exceptions_ =
          (match !aot_task with
          | Some task ->
              List.rev !compiler_exceptions_rev @ Task.compiler_exceptions task
          | None ->
              List.rev !compiler_exceptions_rev
              @ Option.fold ~none:[] ~some:Source.compiler_exceptions
                  !source_report);
      }

let outcome report = report.outcome_
let fragments report = report.fragments_
let static_copies report = report.static_copies_
let platform report = report.platform_
let executed_steps report = report.executed_steps_
let preparation_steps report = report.preparation_steps_
let default_bytes report = report.default_bytes_
let dimension_work report = report.dimension_work_
let switch_work report = report.switch_work_
let output_bytes report = report.output_bytes_
let output_work report = report.output_work_
let source_progress report = report.source_progress_
let compiler_exceptions report = report.compiler_exceptions_
