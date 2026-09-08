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
    match result with
    | Some result -> Some (VM.executed_steps result)
    | None ->
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
  let preparation = Option.map VM.compiled_initializer_steps result in
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
         ("output-byte-limit", limits.output_bytes);
         ("output-work-limit", limits.output_work);
       ];
     let optional_int name value =
       Printf.printf "%s=%s\n" name
         (Option.fold ~none:"unknown" ~some:string_of_int value)
     in
     optional_int "steps" executed_steps;
     optional_int "compiled-initializer-steps" preparation;
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
