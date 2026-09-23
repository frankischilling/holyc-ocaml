module VM = Holyc_lib.Ir_integer_interpreter

type limits = {
  mode : string;
  target : string;
  steps : int;
  frame_bytes : int;
  call_depth : int;
  global_bytes : int;
  literal_bytes : int;
  initializer_steps : int;
  dimension_work : int;
  switch_work : int;
  output_bytes : int;
  output_work : int;
}

let hex bytes =
  if String.length bytes > Sys.max_string_length / 2 then
    invalid_arg
      "captured output exceeds the hexadecimal report allocation bound";
  let digits = "0123456789abcdef" in
  String.init
    (2 * String.length bytes)
    (fun index ->
      let byte = Char.code bytes.[index / 2] in
      digits.[if index mod 2 = 0 then byte lsr 4 else byte land 15])

let decimal (word : VM.word) =
  match word.type_ with
  | VM.I64 -> Int64.to_string word.bits
  | VM.U64 -> Printf.sprintf "%Lu" word.bits

let word_type (word : VM.word) =
  match word.type_ with
  | VM.I64 -> "i64"
  | VM.U64 -> "u64"

let render ~human ~session ~limits ?command_error ?report () =
  let result, diagnostics, bytes, work =
    match report with
    | None -> (None, [], "", 0)
    | Some report ->
        let result, diagnostics =
          match Holyc_lib.integer_program_report_outcome report with
          | Ok checked -> (Some checked.value, checked.diagnostics)
          | Error diagnostics -> (None, diagnostics)
        in
        ( result,
          diagnostics,
          Holyc_lib.integer_program_report_output_bytes report,
          Holyc_lib.integer_program_report_output_work report )
  in
  let output_hex = hex bytes in
  let outcome = if Option.is_some result then "success" else "error" in
  let final_value = Option.bind result VM.final_value in
  let termination =
    Option.map
      (fun result ->
        match VM.termination result with
        | VM.Stream_end -> "stream-end"
        | VM.Returned _ -> "returned")
      result
  in
  let executed_steps =
    let progress =
      Option.bind report Holyc_lib.integer_program_report_progress
    in
    match (progress, result) with
    | Some progress, _ -> Some progress.runtime.executed_steps
    | None, Some result -> Some (VM.executed_steps result)
    | None, None ->
        diagnostics
        |> List.concat_map (fun (diagnostic : Holyc_lib.Diagnostic.t) ->
            diagnostic.notes)
        |> List.find_map (fun note ->
            let prefix = "executed_steps=" in
            if String.starts_with ~prefix note then
              int_of_string_opt
                (String.sub note (String.length prefix)
                   (String.length note - String.length prefix))
            else None)
  in
  let preparation =
    match report with
    | Some report -> Holyc_lib.integer_program_report_preparation_work report
    | None -> Option.map VM.compiled_initializer_steps result
  in
  let dimension_work =
    Option.fold ~none:0 ~some:Holyc_lib.integer_program_report_dimension_work
      report
  in
  let switch_work =
    Option.fold ~none:0 ~some:Holyc_lib.integer_program_report_switch_work
      report
  in
  (if human then (
     Printf.printf
       "holyc-integer-program-v2 implementation=%s reference=%s\n\
        mode=%s target=%s arithmetic=runtime-ir\n\
        outcome=%s\n"
       Holyc_lib.Version.implementation_commit VM.reference_commit limits.mode
       limits.target outcome;
     List.iter
       (fun (name, value) -> Printf.printf "%s=%d\n" name value)
       [
         ("step-limit", limits.steps);
         ("frame-byte-limit", limits.frame_bytes);
         ("call-depth-limit", limits.call_depth);
         ("global-byte-limit", limits.global_bytes);
         ("literal-byte-limit", limits.literal_bytes);
         ("initializer-step-limit", limits.initializer_steps);
         ("dimension-work-limit", limits.dimension_work);
         ("switch-work-limit", limits.switch_work);
         ("output-byte-limit", limits.output_bytes);
         ("output-work-limit", limits.output_work);
       ];
     let optional_int name value =
       Printf.printf "%s=%s\n" name
         (Option.fold ~none:"unknown" ~some:string_of_int value)
     in
     optional_int "steps" executed_steps;
     optional_int "compiled-initializer-steps" preparation;
     Printf.printf "dimension-preparation-work=%d\n" dimension_work;
     Printf.printf "switch-preparation-work=%d\n" switch_work;
     Printf.printf "termination=%s\n" (Option.value termination ~default:"none");
     (match final_value with
     | None -> print_endline "final-value=none"
     | Some word ->
         Printf.printf "final-value=%s type=%s bits=0x%016Lx\n" (decimal word)
           (word_type word) word.bits);
     Printf.printf "output-byte-length=%d\noutput-work=%d\noutput-hex=%s\n"
       (String.length bytes) work output_hex;
     List.iter
       (fun diagnostic ->
         Holyc_lib.Diagnostic_render.human
           (Holyc_lib.Session.sources session)
           diagnostic
         |> output_string stderr)
       diagnostics;
     Option.iter
       (fun (message, _) -> Printf.eprintf "holyc: run: %s\n" message)
       command_error)
   else
     let optional convert = Option.fold ~none:`Null ~some:convert in
     `Assoc
       [
         ("schema", `String "holyc-integer-program-v2");
         ( "implementation_commit",
           `String Holyc_lib.Version.implementation_commit );
         ("reference_commit", `String VM.reference_commit);
         ("mode", `String limits.mode);
         ("target", `String limits.target);
         ("arithmetic", `String "runtime-ir");
         ("outcome", `String outcome);
         ("step_limit", `Int limits.steps);
         ("executed_steps", optional (fun n -> `Int n) executed_steps);
         ("termination", optional (fun text -> `String text) termination);
         ("frame_byte_limit", `Int limits.frame_bytes);
         ("call_depth_limit", `Int limits.call_depth);
         ("global_byte_limit", `Int limits.global_bytes);
         ("literal_byte_limit", `Int limits.literal_bytes);
         ("initializer_step_limit", `Int limits.initializer_steps);
         ("dimension_work_limit", `Int limits.dimension_work);
         ("dimension_preparation_work", `Int dimension_work);
         ("switch_work_limit", `Int limits.switch_work);
         ("switch_preparation_work", `Int switch_work);
         ("compiled_initializer_steps", optional (fun n -> `Int n) preparation);
         ("output_byte_limit", `Int limits.output_bytes);
         ("output_work_limit", `Int limits.output_work);
         ("output_byte_length", `Int (String.length bytes));
         ("output_work", `Int work);
         ("output_hex", `String output_hex);
         ( "final_value",
           optional
             (fun word ->
               `Assoc
                 [
                   ("type", `String (word_type word));
                   ("value", `String (decimal word));
                   ("bits", `String (Printf.sprintf "0x%016Lx" word.bits));
                 ])
             final_value );
         ( "diagnostics",
           `List
             (List.map
                (Holyc_lib.Diagnostic.to_yojson
                   (Holyc_lib.Session.sources session))
                diagnostics) );
         ( "command_error",
           optional
             (fun (message, code) ->
               `Assoc
                 [
                   ("message", `String message);
                   ("code", optional (fun text -> `String text) code);
                 ])
             command_error );
       ]
     |> Yojson.Safe.pretty_to_string |> print_endline);
  if Option.is_some result then 0 else 1

type native_limits = {
  ir_instructions : int;
  code_bytes : int;
  stack_bytes : int;
  blocks : int;
  active_stack_bytes : int;
  default_bytes : int;
}

let native_decimal (word : Holyc_lib.X86_64_program.word) =
  match word.type_ with
  | Holyc_lib.X86_64_program.I64 -> Int64.to_string word.bits
  | Holyc_lib.X86_64_program.U64 -> Printf.sprintf "%Lu" word.bits

let native_word_type (word : Holyc_lib.X86_64_program.word) =
  match word.type_ with
  | Holyc_lib.X86_64_program.I64 -> "i64"
  | Holyc_lib.X86_64_program.U64 -> "u64"

let render_native ~human ~session ~limits ~native_limits ?command_error ?report
    () =
  let result, diagnostics =
    match report with
    | None -> (None, [])
    | Some report -> (
        match Holyc_lib.Native_program.outcome report with
        | Ok checked -> (Some checked.value, checked.diagnostics)
        | Error diagnostics -> (None, diagnostics))
  in
  let platform =
    Option.fold
      ~none:(Holyc_lib.Native_program_execution.platform ())
      ~some:Holyc_lib.Native_program.platform report
  in
  let executed_steps =
    Option.bind report Holyc_lib.Native_program.executed_steps
  in
  let preparation_steps =
    Option.fold ~none:0 ~some:Holyc_lib.Native_program.preparation_steps report
  in
  let switch_work =
    Option.fold ~none:0 ~some:Holyc_lib.Native_program.switch_work report
  in
  let dimension_work =
    Option.fold ~none:0 ~some:Holyc_lib.Native_program.dimension_work report
  in
  let default_bytes =
    Option.fold ~none:0 ~some:Holyc_lib.Native_program.default_bytes report
  in
  let final_value =
    Option.bind result (fun (value : Holyc_lib.Native_program.result) ->
        value.execution.final_value)
  in
  let outcome = if Option.is_some result then "success" else "error" in
  let image =
    Option.map
      (fun (value : Holyc_lib.Native_program.result) -> value.image)
      result
  in
  (if human then (
     Printf.printf
       "holyc-integer-program-v2 implementation=%s reference=%s\n\
        mode=%s target=%s arithmetic=runtime-native\n\
        outcome=%s\n"
       Holyc_lib.Version.implementation_commit VM.reference_commit limits.mode
       limits.target outcome;
     List.iter
       (fun (name, value) -> Printf.printf "%s=%d\n" name value)
       [
         ("step-limit", limits.steps);
         ("frame-byte-limit", limits.frame_bytes);
         ("call-depth-limit", limits.call_depth);
         ("global-byte-limit", limits.global_bytes);
         ("literal-byte-limit", limits.literal_bytes);
         ("initializer-step-limit", limits.initializer_steps);
         ("dimension-work-limit", limits.dimension_work);
         ("switch-work-limit", limits.switch_work);
         ("output-byte-limit", limits.output_bytes);
         ("output-work-limit", limits.output_work);
       ];
     Printf.printf "steps=%s\n"
       (Option.fold ~none:"unknown" ~some:string_of_int executed_steps);
     Printf.printf "compiled-initializer-steps=%d\n" preparation_steps;
     Printf.printf "prepared-default-bytes=%d\n" default_bytes;
     Printf.printf "dimension-preparation-work=%d\n" dimension_work;
     Printf.printf "switch-preparation-work=%d\n" switch_work;
     Printf.printf "termination=%s\n"
       (if Option.is_some result then "stream-end" else "none");
     (match final_value with
     | None -> print_endline "final-value=none"
     | Some word ->
         Printf.printf "final-value=%s type=%s bits=0x%016Lx\n"
           (native_decimal word) (native_word_type word) word.bits);
     print_endline "output-byte-length=0";
     print_endline "output-work=0";
     print_endline "output-hex=";
     Printf.printf "native-platform=%s\n"
       (Holyc_lib.Native_program_execution.platform_name platform);
     Printf.printf
       "native-ir-instruction-limit=%d\n\
        native-code-byte-limit=%d\n\
        native-stack-byte-limit=%d\n\
        native-block-limit=%d\n\
        native-active-stack-byte-limit=%d\n\
        native-default-byte-limit=%d\n"
       native_limits.ir_instructions native_limits.code_bytes
       native_limits.stack_bytes native_limits.blocks
       native_limits.active_stack_bytes native_limits.default_bytes;
     Option.iter
       (fun image ->
         Printf.printf
           "native-ir-instructions=%d\n\
            native-machine-instructions=%d\n\
            native-code-bytes=%d\n\
            native-register-peak=%d\n\
            native-frame-bytes=%d\n\
            native-blocks=%d\n\
            native-functions=%d\n\
            native-entry-stack-bytes=%d\n\
            native-global-bytes=%d\n\
            native-literal-bytes=%d\n\
            native-arena-metadata-bytes=%d\n\
            native-global-arena-bytes=%d\n"
           (Holyc_lib.X86_64_program.ir_instructions image)
           (Holyc_lib.X86_64_program.machine_instructions image)
           (Holyc_lib.X86_64_program.code_bytes image)
           (Holyc_lib.X86_64_program.register_peak image)
           (Holyc_lib.X86_64_program.frame_bytes image)
           (Holyc_lib.X86_64_program.block_count image)
           (Holyc_lib.X86_64_program.function_count image)
           (Holyc_lib.X86_64_program.entry_stack_bytes image)
           (Holyc_lib.X86_64_program.global_bytes image)
           (Holyc_lib.X86_64_program.literal_bytes image)
           (Holyc_lib.X86_64_program.arena_metadata_bytes image)
           (String.length (Holyc_lib.X86_64_program.global_image image)))
       image;
     List.iter
       (fun diagnostic ->
         Holyc_lib.Diagnostic_render.human
           (Holyc_lib.Session.sources session)
           diagnostic
         |> output_string stderr)
       diagnostics;
     Option.iter
       (fun (message, code) ->
         match code with
         | Some code -> Printf.eprintf "holyc: run: %s: %s\n" code message
         | None -> Printf.eprintf "holyc: run: %s\n" message)
       command_error)
   else
     let optional convert = Option.fold ~none:`Null ~some:convert in
     let final_value =
       optional
         (fun word ->
           `Assoc
             [
               ("type", `String (native_word_type word));
               ("value", `String (native_decimal word));
               ("bits", `String (Printf.sprintf "0x%016Lx" word.bits));
             ])
         final_value
     in
     let image =
       optional
         (fun image ->
           `Assoc
             [
               ( "ir_instructions",
                 `Int (Holyc_lib.X86_64_program.ir_instructions image) );
               ( "machine_instructions",
                 `Int (Holyc_lib.X86_64_program.machine_instructions image) );
               ("code_bytes", `Int (Holyc_lib.X86_64_program.code_bytes image));
               ( "register_peak",
                 `Int (Holyc_lib.X86_64_program.register_peak image) );
               ("frame_bytes", `Int (Holyc_lib.X86_64_program.frame_bytes image));
               ("block_count", `Int (Holyc_lib.X86_64_program.block_count image));
               ( "function_count",
                 `Int (Holyc_lib.X86_64_program.function_count image) );
               ( "entry_stack_bytes",
                 `Int (Holyc_lib.X86_64_program.entry_stack_bytes image) );
               ( "global_bytes",
                 `Int (Holyc_lib.X86_64_program.global_bytes image) );
               ( "literal_bytes",
                 `Int (Holyc_lib.X86_64_program.literal_bytes image) );
               ( "arena_metadata_bytes",
                 `Int (Holyc_lib.X86_64_program.arena_metadata_bytes image) );
               ( "global_arena_bytes",
                 `Int
                   (String.length (Holyc_lib.X86_64_program.global_image image))
               );
             ])
         image
     in
     let diagnostics =
       `List
         (List.map
            (Holyc_lib.Diagnostic.to_yojson (Holyc_lib.Session.sources session))
            diagnostics)
     in
     `Assoc
       [
         ("schema", `String "holyc-integer-program-v2");
         ( "implementation_commit",
           `String Holyc_lib.Version.implementation_commit );
         ("reference_commit", `String VM.reference_commit);
         ("mode", `String limits.mode);
         ("target", `String limits.target);
         ("arithmetic", `String "runtime-native");
         ("outcome", `String outcome);
         ("step_limit", `Int limits.steps);
         ("executed_steps", optional (fun n -> `Int n) executed_steps);
         ( "termination",
           if Option.is_some result then `String "stream-end" else `Null );
         ("frame_byte_limit", `Int limits.frame_bytes);
         ("call_depth_limit", `Int limits.call_depth);
         ("global_byte_limit", `Int limits.global_bytes);
         ("literal_byte_limit", `Int limits.literal_bytes);
         ("initializer_step_limit", `Int limits.initializer_steps);
         ("dimension_work_limit", `Int limits.dimension_work);
         ("dimension_preparation_work", `Int dimension_work);
         ("switch_work_limit", `Int limits.switch_work);
         ("switch_preparation_work", `Int switch_work);
         ("compiled_initializer_steps", `Int preparation_steps);
         ("prepared_default_bytes", `Int default_bytes);
         ("output_byte_limit", `Int limits.output_bytes);
         ("output_work_limit", `Int limits.output_work);
         ("output_byte_length", `Int 0);
         ("output_work", `Int 0);
         ("output_hex", `String "");
         ("final_value", final_value);
         ("diagnostics", diagnostics);
         ( "command_error",
           optional
             (fun (message, code) ->
               `Assoc
                 [
                   ("message", `String message);
                   ("code", optional (fun text -> `String text) code);
                 ])
             command_error );
         ( "native",
           `Assoc
             [
               ( "platform",
                 `String
                   (Holyc_lib.Native_program_execution.platform_name platform)
               );
               ( "limits",
                 `Assoc
                   [
                     ("ir_instructions", `Int native_limits.ir_instructions);
                     ("code_bytes", `Int native_limits.code_bytes);
                     ("stack_bytes", `Int native_limits.stack_bytes);
                     ("blocks", `Int native_limits.blocks);
                     ( "active_stack_bytes",
                       `Int native_limits.active_stack_bytes );
                     ("default_bytes", `Int native_limits.default_bytes);
                   ] );
               ("image", image);
             ] );
       ]
     |> Yojson.Safe.pretty_to_string |> print_endline);
  if Option.is_some result then 0 else 1
