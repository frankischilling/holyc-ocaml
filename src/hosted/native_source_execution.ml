module Source = Driver.Integer_source_execution
module Task = Driver.Integer_task
module Dispatch = Task.Native_dispatch
module Image = Backend.X86_64_program
module Native = Runtime.Native_program_execution

type word = { type_ : Image.word_type; bits : int64 }
type result = { final_value : word option }
type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }
type fragment_kind = Initializer | Command

type image = {
  status_abi : Image.status_abi;
  ir_instructions : int;
  code_bytes : int;
  global_bytes : int;
  global_arena_bytes : int;
  entry_stack_bytes : int;
  function_count : int;
}

type fragment = {
  kind : fragment_kind;
  image : image;
  native_outcome : (Image.outcome, string) Stdlib.result option;
}

type report = {
  outcome_ : (result checked, Common.Diagnostic.t list) Stdlib.result;
  fragments_ : fragment list;
  platform_ : Native.platform;
  executed_steps_ : int;
  preparation_steps_ : int;
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
      Image.create_task_layout ~max_global_bytes |> Result.map_error errors
    in
    let* budget =
      Native.create_budget ~max_steps ~max_output_bytes ~max_output_work ()
      |> Result.map_error host_error
    in
    let* arena =
      Native.create_task_arena ~max_arena_bytes:(2 * max_global_bytes) layout
      |> Result.map_error host_error
    in
    Ok (layout, budget, arena)
  in
  match validate with
  | Error diagnostics ->
      {
        outcome_ = Error diagnostics;
        fragments_ = [];
        platform_;
        executed_steps_ = 0;
        preparation_steps_ = 0;
        dimension_work_ = 0;
        switch_work_ = 0;
        output_bytes_ = "";
        output_work_ = 0;
        source_progress_ = None;
      }
  | Ok (layout, budget, arena) ->
      let fragments = ref [] in
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
      let execute kind image =
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
                  Native.execute_retained_budget_report ~max_frame_bytes
                    ~max_call_depth ~max_active_stack_bytes ~max_global_bytes
                    ~max_literal_bytes budget retained)
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
        Fun.protect
          ~finally:(fun () ->
            match Native.release_task_arena arena with
            | Ok () -> ()
            | Error message -> cleanup_errors := host_error message)
          (fun () ->
            let report =
              Source.run ~native_dispatch ~max_dimension_work ~max_switch_work
                ~max_initializer_steps ~max_global_bytes ~max_literal_bytes
                ~max_frame_bytes ~max_call_depth ~max_output_bytes
                ~max_output_work session ~config ~source ~max_steps
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
        platform_;
        executed_steps_ = progress.executed_steps;
        preparation_steps_ =
          Option.bind !source_report Source.preparation_work
          |> Option.value ~default:0;
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
let platform report = report.platform_
let executed_steps report = report.executed_steps_
let preparation_steps report = report.preparation_steps_
let dimension_work report = report.dimension_work_
let switch_work report = report.switch_work_
let output_bytes report = report.output_bytes_
let output_work report = report.output_work_
let source_progress report = report.source_progress_
