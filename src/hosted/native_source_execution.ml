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
  | Offset
  | Command

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
      if Frontend.Preprocessor.Config.compilation_mode config <> Jit then
        Error
          [
            diagnostic ~span "HCRUN0001" "native source tasks require JIT mode";
          ]
      else if
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
      }
  | Ok (layout, budget, arena) ->
      let fragments = ref [] in
      let static_copies = ref [] in
      let default_bytes = ref 0 in
      let emitted_bytes = ref 0 in
      let emitted_ir = ref 0 in
      let cleanup_errors = ref [] in
      let source_report = ref None in
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
      let execute ?max_activation_steps kind image =
        let metadata = describe_image image in
        match
          Native.retain_task_fragment ~max_global_bytes ~max_literal_bytes
            ~max_active_stack_bytes arena image
        with
        | Error message ->
            fragments :=
              { kind; image = metadata; native_outcome = None } :: !fragments;
            Error (host_error message)
        | Ok retained -> (
            let release_error = ref None in
            let execution =
              Fun.protect
                ~finally:(fun () ->
                  match Native.release retained with
                  | Ok () -> ()
                  | Error message -> release_error := Some message)
                (fun () ->
                  Native.execute_retained_budget_report ?max_activation_steps
                    ~max_frame_bytes ~max_call_depth ~max_active_stack_bytes
                    ~max_global_bytes ~max_literal_bytes budget retained)
            in
            let native_outcome = Native.outcome execution in
            fragments :=
              { kind; image = metadata; native_outcome = Some native_outcome }
              :: !fragments;
            let reached =
              match native_outcome with
              | Error message -> Error (host_error message)
              | Ok (Image.Fault fault) ->
                  Error [ Native_program.fault_diagnostic ~fallback:span fault ]
              | Ok (Image.Completed completed) ->
                  Ok (completed, Native.value_captured execution)
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
              let* image =
                remaining (fun ~max_ir_instructions ~max_code_bytes ->
                    Image.compile_task_initializer ?status_abi ~max_stack_bytes
                      ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                      request)
              in
              execute Initializer image |> Result.map ignore);
          execute_command =
            (fun request ->
              let* image =
                remaining (fun ~max_ir_instructions ~max_code_bytes ->
                    Image.compile_task_command ?status_abi ~max_stack_bytes
                      ~max_blocks ~max_ir_instructions ~max_code_bytes ~layout
                      request)
              in
              let* execution, captured = execute Command image in
              Ok
                (if captured then
                   Dispatch.Captured
                     (Option.map dispatch_of_word execution.final_value)
                 else Dispatch.Unchanged));
        }
      in
      let outcome_ =
        let native_static_initializer request =
          let* image =
            remaining (fun ~max_ir_instructions ~max_code_bytes ->
                Image.compile_task_static_initializer ?status_abi
                  ~max_stack_bytes ~max_blocks ~max_ir_instructions
                  ~max_code_bytes ~layout request)
          in
          execute Initializer image |> Result.map ignore
        in
        let native_default request =
          let module Request = Task.Native_default in
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
              execute ~max_activation_steps:allowance Default image
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
                    Native.finish_task_data_default arena image value
                      ~max_copy_steps:(allowance - steps)
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
              execute ~max_activation_steps:allowance Internal_binding image
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
                Native.finish_task_internal_binding arena image
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
              execute ~max_activation_steps:allowance Dimension image
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
                Native.finish_task_dimension arena image
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
        let native_offset request =
          let module Request = Task.Native_offset in
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
              execute ~max_activation_steps:allowance Offset image
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
                Native.finish_task_offset arena image
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
        let native_static_allocation request =
          Native.allocate_task_static arena request
          |> Result.map_error (fun message ->
              [ Driver.Integer_source.message_diagnostic ~span message ])
        in
        let native_static_copy request =
          let module Destination = Ir.Static_initializer_destination in
          let destination = Task.Native_static_copy.destination request in
          let byte_count =
            Option.value ~default:0 (Destination.copy_byte_count destination)
          in
          let outcome = Native.copy_task_static arena request in
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
            match Native.release_task_arena arena with
            | Ok () -> ()
            | Error message -> cleanup_errors := host_error message)
          (fun () ->
            let report =
              Source.run ~native_dispatch ~native_static_allocation
                ~native_static_initializer ~native_static_copy ~native_default
                ~native_dimension ~native_offset ~native_internal_binding
                ~max_dimension_work ~max_switch_work ~max_initializer_steps
                ~max_global_bytes ~max_literal_bytes ~max_frame_bytes
                ~max_call_depth ~max_output_bytes ~max_output_work session
                ~config ~source ~max_steps
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
                })
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
          Option.bind !source_report Source.preparation_work
          |> Option.value ~default:0;
        default_bytes_ = !default_bytes;
        dimension_work_ =
          Option.fold ~none:0 ~some:Source.dimension_work !source_report;
        switch_work_ =
          Option.fold ~none:0 ~some:Source.switch_work !source_report;
        output_bytes_ = Native.budget_output_bytes budget;
        output_work_ = progress.output_work;
        source_progress_ = Option.bind !source_report Source.progress;
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
