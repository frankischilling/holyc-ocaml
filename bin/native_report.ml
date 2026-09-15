module Image = Holyc_lib.X86_64_expression
module Native = Holyc_lib.Native_execution

let decimal image bits =
  match Image.value_type image with
  | Image.I64 -> Int64.to_string bits
  | Image.U64 -> Printf.sprintf "%Lu" bits

let render ~human ~session ~mode ~max_ir_instructions ~max_code_bytes
    ?command_error ?result () =
  let value, diagnostics =
    match result with
    | Some (Ok (value : Holyc_lib.Native_expression.result)) -> (Some value, [])
    | Some (Error errors) -> (None, errors)
    | None -> (None, [])
  in
  let platform =
    Option.fold ~none:(Native.platform ())
      ~some:(fun (value : Holyc_lib.Native_expression.result) -> value.platform)
      value
  in
  (if human then (
     Option.iter
       (fun (value : Holyc_lib.Native_expression.result) ->
         print_endline (decimal value.image value.bits))
       value;
     List.iter
       (fun diagnostic ->
         Holyc_lib.Diagnostic_render.human
           (Holyc_lib.Session.sources session)
           diagnostic
         |> output_string stderr)
       diagnostics;
     Option.iter
       (fun (code, message) ->
         Printf.eprintf "holyc: eval-native: %s: %s\n" code message)
       command_error)
   else
     let final_value, image =
       match value with
       | None -> (`Null, `Null)
       | Some value ->
           let bytes = Image.code value.image in
           ( `Assoc
               [
                 ( "type",
                   `String
                     (match Image.value_type value.image with
                     | I64 -> "I64"
                     | U64 -> "U64") );
                 ("value", `String (decimal value.image value.bits));
                 ("bits", `String (Printf.sprintf "0x%016Lx" value.bits));
               ],
             `Assoc
               [
                 ("bytes_hex", `String (Run_report.hex bytes));
                 ("byte_count", `Int (String.length bytes));
                 ("ir_instructions", `Int (Image.ir_instructions value.image));
                 ( "machine_instructions",
                   `Int (Image.machine_instructions value.image) );
                 ("register_peak", `Int (Image.register_peak value.image));
                 ("frame_bytes", `Int 0);
               ] )
     in
     let diagnostics =
       Holyc_lib.Diagnostic_render.json
         (Holyc_lib.Session.sources session)
         diagnostics
       |> Yojson.Safe.from_string
     in
     `Assoc
       [
         ("schema", `String "holyc-native-expression-v1");
         ( "implementation_commit",
           `String Holyc_lib.Version.implementation_commit );
         ( "reference_commit",
           `String Holyc_lib.Ir_integer_interpreter.reference_commit );
         ("command", `String "eval-native");
         ("execution_target", `String "x86-64-native");
         ("platform", `String (Native.platform_name platform));
         ("mode", `String mode);
         ( "outcome",
           `String (if Option.is_some value then "success" else "error") );
         ( "limits",
           `Assoc
             [
               ("ir_instructions", `Int max_ir_instructions);
               ("code_bytes", `Int max_code_bytes);
             ] );
         ("image", image);
         ("final_value", final_value);
         ("diagnostics", diagnostics);
         ( "command_error",
           Option.fold ~none:`Null
             ~some:(fun (code, message) ->
               `Assoc [ ("code", `String code); ("message", `String message) ])
             command_error );
       ]
     |> Yojson.Safe.pretty_to_string |> print_endline);
  if Option.is_some value then 0 else 1
