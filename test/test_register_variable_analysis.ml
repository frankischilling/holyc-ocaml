open Holyc_lib
open Yojson.Safe.Util
module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence
module Symbol = Semantic_symbol

let read name =
  let channel = open_in_bin ("oracle/register-variable-analysis/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture stem =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "retained reference evidence"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [ (stem ^ ".hc", "payload_sha256"); (stem ^ ".capture", "capture_sha256") ];
  (match metadata |> member "boot_payload_sha256" with
  | `String expected ->
      let source = read (stem ^ ".hc") in
      let size = metadata |> member "boot_payload_bytes" |> to_int in
      Alcotest.(check bool)
        "boot payload padding is nonnegative" true
        (size >= String.length source);
      let padded = source ^ String.make (size - String.length source) ' ' in
      Alcotest.(check string)
        "exact padded boot payload" expected
        (Digestif.SHA256.digest_string padded |> Digestif.SHA256.to_hex)
  | `Null -> ()
  | _ -> failwith "invalid boot payload hash");
  let fields =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (k, v) -> (k, to_string v))
  in
  let one = List.map (fun (k, v) -> k ^ "=" ^ v) fields @ [ "POINTER_DONE" ] in
  let actual =
    read (stem ^ ".capture")
    |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  Alcotest.(check (list string))
    "two complete original observations" (one @ one) actual;
  fields

let compile mode contents =
  let session = Session.create () in
  let source = Session.add_source session ~path:"register-proof.hc" ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  let program =
    match compile_integer_program session ~config ~source with
    | Ok program -> program
    | Error diagnostics ->
        failwith
          (contents ^ ": "
          ^ String.concat "; "
              (List.map
                 (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                 diagnostics))
  in
  List.hd (integer_program_functions program.value)

let location frame name =
  Frame.function_locations frame
  |> List.find (fun item -> Symbol.name (Frame.location_symbol item) = name)

let check_incomplete stem =
  let incomplete = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "incomplete high literal reference"
    "incomplete: invalid opcode observed in JIT before compiled controls"
    (incomplete |> member "status" |> to_string);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "incomplete probe evidence hash"
        (incomplete |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [
      (stem ^ ".hc", "payload_sha256");
      (stem ^ ".capture", "capture_sha256");
      (stem ^ ".png", "screenshot_sha256");
    ];
  let incomplete_source = read (stem ^ ".hc") in
  let incomplete_size = incomplete |> member "boot_payload_bytes" |> to_int in
  Alcotest.(check bool)
    "incomplete boot payload padding" true
    (incomplete_size >= String.length incomplete_source);
  let incomplete_boot =
    incomplete_source
    ^ String.make (incomplete_size - String.length incomplete_source) ' '
  in
  Alcotest.(check string)
    "incomplete boot payload hash"
    (incomplete |> member "boot_payload_sha256" |> to_string)
    (Digestif.SHA256.digest_string incomplete_boot |> Digestif.SHA256.to_hex);
  Alcotest.(check string)
    "incomplete JIT output" "J_S=255\nJ_U=256\n"
    (read (stem ^ ".capture"))

let source_controls () =
  let fields = fixture "selection" in
  List.iter
    (fun prefix ->
      Alcotest.(check string)
        "original register parameter" "254"
        (List.assoc (prefix ^ "REG") fields);
      Alcotest.(check string)
        "original disabled register" "-2"
        (List.assoc (prefix ^ "NOREG") fields);
      Alcotest.(check string)
        "original addressed parameter" "-2"
        (List.assoc (prefix ^ "ADDR") fields);
      Alcotest.(check string)
        "original register local" "254"
        (List.assoc (prefix ^ "LOCAL") fields);
      Alcotest.(check string)
        "original disabled local" "-2"
        (List.assoc (prefix ^ "NOLOCAL") fields))
    [ "JIT_"; "AOT_" ];
  let unsigned = fixture "u8" in
  List.iter
    (fun prefix ->
      Alcotest.(check string)
        "unsigned register" "510"
        (List.assoc (prefix ^ "REG") unsigned);
      Alcotest.(check string)
        "unsigned disabled register" "254"
        (List.assoc (prefix ^ "NOREG") unsigned))
    [ "JIT_"; "AOT_" ];
  let entry = fixture "entry" in
  List.iter
    (fun field ->
      Alcotest.(check string) "signed entry" "-1" (List.assoc field entry))
    [ "JS"; "AS" ];
  List.iter
    (fun field ->
      Alcotest.(check string) "unsigned entry" "0" (List.assoc field entry))
    [ "JU"; "AU" ];
  let retained = fixture "retained" in
  Alcotest.(check string)
    "retained signed register" "254"
    (List.assoc "SIGNED" retained);
  Alcotest.(check string)
    "retained unsigned register" "510"
    (List.assoc "UNSIGNED" retained);
  let default = fixture "default" in
  Alcotest.(check string)
    "default signed register" "254"
    (List.assoc "SIGNED" default);
  Alcotest.(check string)
    "default unsigned register" "510"
    (List.assoc "UNSIGNED" default);
  let widths = fixture "widths" in
  let noreg_widths = fixture "widths-noreg" in
  let entry_widths = fixture "widths-entry" in
  let default_widths = fixture "widths-default" in
  List.iter
    (fun (name, wide, narrow, entry) ->
      List.iter
        (fun prefix ->
          Alcotest.(check string)
            "wide register result" wide
            (List.assoc (prefix ^ name) widths);
          Alcotest.(check string)
            "noreg stored result" narrow
            (List.assoc (prefix ^ name) noreg_widths);
          Alcotest.(check string)
            "wide parameter entry" entry
            (List.assoc (prefix ^ name) entry_widths))
        [ "J_"; "A_" ];
      Alcotest.(check string)
        "wide retained default" wide
        (List.assoc name default_widths))
    [
      ("S16", "65534", "-2", "-1");
      ("U16", "131070", "65534", "0");
      ("S32", "4294967294", "-2", "-1");
      ("U32", "8589934590", "4294967294", "0");
    ];
  let local = fixture "local-entry" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local register initialization" expected
            (List.assoc (prefix ^ name) local))
        [ ("S", "255"); ("U", "256"); ("S16", "65535"); ("U16", "65536") ])
    [ "J_"; "A_" ];
  let local_noreg = fixture "local-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local noreg initialization" expected
            (List.assoc (prefix ^ name) local_noreg))
        [ ("S", "-1"); ("U", "0"); ("S16", "-1"); ("U16", "0") ])
    [ "J_"; "A_" ];
  let local_wide = fixture "local-wide" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "32-bit local initialization" expected
            (List.assoc (prefix ^ name) local_wide))
        [ ("S", "4294967295"); ("U", "4294967296"); ("SN", "-1"); ("UN", "0") ])
    [ "J_"; "A_" ];
  let local_default = fixture "local-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained default" expected
        (List.assoc name local_default))
    [ ("S", "255"); ("U", "256"); ("SH", "254") ];
  let local_right = fixture "local-right" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local right shift" expected
            (List.assoc (prefix ^ name) local_right))
        [ ("S", "127"); ("U", "128"); ("SN", "-1"); ("UN", "0") ])
    [ "J_"; "A_" ];
  List.iter
    (fun (stem, signed, unsigned) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "wider local right shift" expected
                (List.assoc (prefix ^ name) fields))
            [ ("S", signed); ("U", unsigned); ("SN", "-1"); ("UN", "0") ])
        [ "J_"; "A_" ])
    [
      ("local-mid-right", "32767", "32768");
      ("local-wide-right", "2147483647", "2147483648");
    ];
  let local_default_right = fixture "local-default-right" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained default right shift" expected
        (List.assoc name local_default_right))
    [ ("S", "127"); ("U", "128"); ("SH", "63") ];
  let local_return = fixture "local-return" in
  let local_return_small = fixture "local-return-small" in
  let local_return_noreg = fixture "local-return-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "small local return" expected
            (List.assoc (prefix ^ name) local_return_small))
        [ ("S", "255"); ("U", "256"); ("S16", "65535"); ("U16", "65536") ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "noreg local return" expected
            (List.assoc (prefix ^ name) local_return_noreg))
        [ ("S", "-1"); ("U", "0"); ("S16", "-1"); ("U16", "0") ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local return" expected
            (List.assoc (prefix ^ name) local_return))
        [ ("S", "4294967295"); ("U", "4294967296"); ("SN", "-1"); ("UN", "0") ])
    [ "J_"; "A_" ];
  let local_return_init = fixture "local-return-init" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained local return" expected
        (List.assoc name local_return_init))
    [ ("S8", "255"); ("U8", "256"); ("S16", "65535"); ("S32", "4294967338") ];
  let high = fixture "local-return-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun name ->
          Alcotest.(check string)
            "high-bit local return" "-9223372036854775766"
            (List.assoc (prefix ^ name) high))
        [ "S8"; "S16"; "U16"; "S32"; "U32" ])
    [ "J_"; "A_" ];
  let local_return_default = fixture "local-return-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained direct local return" expected
        (List.assoc name local_return_default))
    [ ("S", "255"); ("U", "256"); ("SH", "127") ];
  let narrow_return = fixture "local-narrow-return" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "narrow declared local return" expected
            (List.assoc (prefix ^ name) narrow_return))
        [ ("S", "255"); ("U", "256"); ("S16", "65535"); ("U16", "65536") ])
    [ "J_"; "A_" ];
  let hidden = fixture "local-return-hidden" in
  Alcotest.(check string) "hidden callee result" "0" (List.assoc "VALUE" hidden);
  Alcotest.(check string)
    "hidden callee stored value" "0"
    (List.assoc "STORED" hidden);
  let add_byte = fixture "add-byte" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "byte compound addition" expected
            (List.assoc (prefix ^ name) add_byte))
        [ ("S", "128"); ("U", "256"); ("SN", "-128"); ("UN", "0") ])
    [ "J_"; "A_" ];
  let add_wide = fixture "add-wide" and add_noreg = fixture "add-wide-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "wide compound addition" wide
            (List.assoc (prefix ^ name) add_wide);
          Alcotest.(check string)
            "noreg compound addition" narrow
            (List.assoc (prefix ^ name) add_noreg))
        [
          ("S16", "32768", "-32768");
          ("U16", "65536", "0");
          ("S32", "2147483648", "-2147483648");
          ("U32", "4294967296", "0");
        ])
    [ "J_"; "A_" ];
  let add_default = fixture "add-default" in
  Alcotest.(check string)
    "signed retained addition" "128"
    (List.assoc "SIGNED" add_default);
  Alcotest.(check string)
    "unsigned retained addition" "256"
    (List.assoc "UNSIGNED" add_default);
  let add_wide_default = fixture "add-wide-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "wide retained addition" expected
        (List.assoc name add_wide_default))
    [
      ("S16", "32768");
      ("U16", "65536");
      ("S32", "2147483648");
      ("U32", "4294967296");
    ];
  let local_add = fixture "local-add"
  and local_add_noreg = fixture "local-add-noreg" in
  let local_add_wide = fixture "local-add-wide" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "local addition" wide
            (List.assoc (prefix ^ name) local_add);
          Alcotest.(check string)
            "noreg local addition" narrow
            (List.assoc (prefix ^ name) local_add_noreg))
        [
          ("S", "256", "0");
          ("U", "257", "1");
          ("S16", "65536", "0");
          ("U16", "65537", "1");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local addition" expected
            (List.assoc (prefix ^ name) local_add_wide))
        [ ("S", "4294967296"); ("U", "4294967297"); ("SN", "0"); ("UN", "1") ])
    [ "J_"; "A_" ];
  let local_add_default = fixture "local-add-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained addition" expected
        (List.assoc name local_add_default))
    [ ("S", "256"); ("U", "257"); ("SH", "128") ];
  let add_overflow = fixture "local-add-overflow" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local addition crossing byte bounds" expected
            (List.assoc (prefix ^ name) add_overflow))
        [ ("S", "175"); ("U", "298"); ("SN", "-81"); ("UN", "42") ])
    [ "J_"; "A_" ];
  let add_count = fixture "local-add-count" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local addition wider literal" expected
            (List.assoc (prefix ^ name) add_count))
        [ ("S", "511"); ("U", "512"); ("S16", "65791"); ("U16", "65792") ])
    [ "J_"; "A_" ];
  check_incomplete "local-add-high-fault";
  check_incomplete "local-sub-high-fault";
  let add_param_high = fixture "add-param-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high-bit parameter addition" expected
            (List.assoc (prefix ^ name) add_param_high))
        [
          ("S", "-9223372036854775681");
          ("U", "-9223372036854775553");
          ("SN", "127");
          ("UN", "255");
        ])
    [ "J_"; "A_" ];
  let add_max = fixture "local-add-max32" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "maximum proven local add literal" expected
            (List.assoc (prefix ^ name) add_max))
        [
          ("S", "2147483902");
          ("U", "2147483903");
          ("S16", "2147549182");
          ("U16", "2147549183");
        ])
    [ "J_"; "A_" ];
  let sub_byte = fixture "sub-byte" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "byte compound subtraction" expected
            (List.assoc (prefix ^ name) sub_byte))
        [ ("S", "-129"); ("U", "-1"); ("SN", "127"); ("UN", "255") ])
    [ "J_"; "A_" ];
  let sub_wide = fixture "sub-wide" and sub_noreg = fixture "sub-wide-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "wide compound subtraction" wide
            (List.assoc (prefix ^ name) sub_wide);
          Alcotest.(check string)
            "noreg compound subtraction" narrow
            (List.assoc (prefix ^ name) sub_noreg))
        [
          ("S16", "-32769", "32767");
          ("U16", "-1", "65535");
          ("S32", "-2147483649", "2147483647");
          ("U32", "-1", "4294967295");
        ])
    [ "J_"; "A_" ];
  let sub_high = fixture "sub-param-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high-bit parameter subtraction" expected
            (List.assoc (prefix ^ name) sub_high))
        [
          ("S", "-9223372036854775681");
          ("U", "-9223372036854775553");
          ("SN", "127");
          ("UN", "255");
        ])
    [ "J_"; "A_" ];
  let local_sub = fixture "local-sub"
  and local_sub_noreg = fixture "local-sub-noreg" in
  let local_sub_wide = fixture "local-sub-wide"
  and local_sub_max = fixture "local-sub-max32" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "local subtraction" wide
            (List.assoc (prefix ^ name) local_sub);
          Alcotest.(check string)
            "noreg local subtraction" narrow
            (List.assoc (prefix ^ name) local_sub_noreg))
        [
          ("S", "254", "-2");
          ("U", "255", "255");
          ("S16", "65534", "-2");
          ("U16", "65535", "65535");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local subtraction" expected
            (List.assoc (prefix ^ name) local_sub_wide))
        [
          ("S", "4294967294");
          ("U", "4294967295");
          ("SN", "-2");
          ("UN", "4294967295");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local subtraction immediate boundary" expected
            (List.assoc (prefix ^ name) local_sub_max))
        [
          ("S", "-2147483392");
          ("U", "-2147483391");
          ("S16", "-2147418112");
          ("U16", "-2147418111");
        ])
    [ "J_"; "A_" ];
  let sub_default = fixture "sub-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained subtraction" expected
        (List.assoc name sub_default))
    [ ("SIGNED", "-129"); ("UNSIGNED", "-1"); ("LOCAL", "254") ];
  let mul_byte = fixture "mul-byte" and mul_wide = fixture "mul-wide" in
  let mul_noreg = fixture "mul-wide-noreg"
  and mul_high = fixture "mul-param-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "byte multiplication" expected
            (List.assoc (prefix ^ name) mul_byte))
        [ ("S", "254"); ("U", "510"); ("SN", "-2"); ("UN", "254") ];
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "wide multiplication" wide
            (List.assoc (prefix ^ name) mul_wide);
          Alcotest.(check string)
            "noreg multiplication" narrow
            (List.assoc (prefix ^ name) mul_noreg))
        [
          ("S16", "65534", "-2");
          ("U16", "131070", "65534");
          ("S32", "4294967294", "-2");
          ("U32", "8589934590", "4294967294");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high parameter multiplication" expected
            (List.assoc (prefix ^ name) mul_high))
        [
          ("S", "-9223372036854775808");
          ("U", "-9223372036854775808");
          ("SN", "0");
          ("UN", "0");
        ])
    [ "J_"; "A_" ];
  let mul_default = fixture "mul-default"
  and mul_wide_default = fixture "mul-wide-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained multiplication" expected
        (List.assoc name mul_default))
    [ ("SIGNED", "254"); ("UNSIGNED", "510") ];
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "wide retained multiplication" expected
        (List.assoc name mul_wide_default))
    [
      ("S16", "65534");
      ("U16", "131070");
      ("S32", "4294967294");
      ("U32", "8589934590");
    ];
  let local_mul = fixture "local-mul"
  and local_noreg_mul = fixture "local-mul-noreg" in
  let local_mul_wide = fixture "local-mul-wide"
  and local_mul_max = fixture "local-mul-max32" in
  let local_mul_high = fixture "local-mul-high"
  and local_noreg_high = fixture "local-mul-noreg-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "local multiplication" wide
            (List.assoc (prefix ^ name) local_mul);
          Alcotest.(check string)
            "noreg local multiplication" narrow
            (List.assoc (prefix ^ name) local_noreg_mul))
        [
          ("S", "510", "-2");
          ("U", "512", "0");
          ("S16", "131070", "-2");
          ("U16", "131072", "0");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local multiplication" expected
            (List.assoc (prefix ^ name) local_mul_wide))
        [ ("S", "8589934590"); ("U", "8589934592"); ("SN", "-2"); ("UN", "0") ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "large local multiplication" expected
            (List.assoc (prefix ^ name) local_mul_max))
        [
          ("S", "547608329985");
          ("U", "549755813632");
          ("S16", "140735340806145");
          ("U16", "140737488289792");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high local multiplication" expected
            (List.assoc (prefix ^ name) local_mul_high);
          Alcotest.(check string)
            "high noreg multiplication" "0"
            (List.assoc (prefix ^ name) local_noreg_high))
        [
          ("S", "-9223372036854775808");
          ("U", "0");
          ("S16", "-9223372036854775808");
          ("U16", "0");
        ])
    [ "J_"; "A_" ];
  let local_mul_default = fixture "local-mul-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained multiplication" expected
        (List.assoc name local_mul_default))
    [ ("S", "510"); ("U", "512"); ("SH", "254") ];
  let high_mul_default = fixture "local-mul-high-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "high retained local multiplication" expected
        (List.assoc name high_mul_default))
    [
      ("S", "-9223372036854775808"); ("U", "0"); ("SH", "-9223372036854775808");
    ];
  List.iter
    (fun (stem, expected) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, value) ->
              Alcotest.(check string)
                "parameter division" value
                (List.assoc (prefix ^ name) fields))
            expected)
        [ "J_"; "A_" ])
    [
      ("div-byte", [ ("S", "128"); ("U", "127"); ("SN", "-128"); ("UN", "127") ]);
      ("div-round", [ ("S", "-3"); ("U", "127"); ("SN", "-3"); ("UN", "127") ]);
      ( "div-unsigned-neg",
        [ ("S", "128"); ("U", "0"); ("SN", "-128"); ("UN", "0") ] );
      ( "div-wide",
        [
          ("S16", "32768");
          ("U16", "32767");
          ("S32", "2147483648");
          ("U32", "2147483647");
        ] );
      ( "div-wide-noreg",
        [
          ("S16", "-32768");
          ("U16", "32767");
          ("S32", "-2147483648");
          ("U32", "2147483647");
        ] );
    ];
  List.iter
    (fun (stem, signed, unsigned) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "parameter-initialized local" expected
                (List.assoc (prefix ^ name) fields))
            [ ("S", signed); ("U", unsigned); ("SN", "-1"); ("UN", "0") ])
        [ "J_"; "A_" ])
    [
      ("local-param-byte", "255", "256");
      ("local-param-16", "65535", "65536");
      ("local-param-32", "4294967295", "4294967296");
    ];
  List.iter
    (fun (stem, signed, unsigned, signed_memory, unsigned_memory) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "updated parameter local" expected
                (List.assoc (prefix ^ name) fields))
            [
              ("S", signed);
              ("U", unsigned);
              ("SN", signed_memory);
              ("UN", unsigned_memory);
            ])
        [ "J_"; "A_" ])
    [
      ("local-param-add-byte", "256", "257", "0", "1");
      ("local-param-add-bound", "2147483902", "2147483903", "-2", "255");
      ("local-param-shift-byte", "510", "512", "-2", "0");
      ("local-param-add-16", "65536", "65537", "0", "1");
      ("local-param-shift-16", "131070", "131072", "-2", "0");
      ("local-param-add-32", "4294967296", "4294967297", "0", "1");
      ("local-param-shift-32", "8589934590", "8589934592", "-2", "0");
    ];
  let param_update_default = fixture "local-param-update-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained updated parameter local" expected
        (List.assoc name param_update_default))
    [ ("SIGNED", "256"); ("UNSIGNED", "512") ];
  let unsigned_parameter = fixture "local-param-unsigned" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "unsigned parameter local" expected
            (List.assoc (prefix ^ name) unsigned_parameter))
        [ ("S", "-1"); ("U", "-1"); ("SN", "-1"); ("UN", "255") ])
    [ "J_"; "A_" ];
  let param_default = fixture "local-param-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained parameter local" expected
        (List.assoc name param_default))
    [ ("SIGNED", "255"); ("UNSIGNED", "256") ];
  let div_zero = fixture "div-zero" in
  List.iter
    (fun name ->
      Alcotest.(check string)
        "original division fault" "1" (List.assoc name div_zero))
    [ "J_CAUGHT"; "A_CAUGHT" ];
  let div_default = fixture "div-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained parameter division" expected
        (List.assoc name div_default))
    [ ("SIGNED", "128"); ("UNSIGNED", "127") ];
  let bit_default = fixture "bit-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained bitwise" expected
        (List.assoc name bit_default))
    [ ("OR", "383"); ("XOR", "383"); ("AND", "255") ];
  List.iter
    (fun (operation, byte_values, narrow_values, wide_values, memory_values) ->
      let byte = fixture ("bit-" ^ operation ^ "-byte") in
      let narrow_count = fixture ("bit-" ^ operation ^ "-narrow-count") in
      let wide = fixture ("bit-" ^ operation ^ "-wide") in
      let noreg = fixture ("bit-" ^ operation ^ "-wide-noreg") in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "byte bitwise" expected
                (List.assoc (prefix ^ name) byte))
            byte_values;
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "narrow RHS bitwise" expected
                (List.assoc (prefix ^ name) narrow_count))
            narrow_values;
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "wide bitwise" expected
                (List.assoc (prefix ^ name) wide))
            wide_values;
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "noreg wide bitwise" expected
                (List.assoc (prefix ^ name) noreg))
            memory_values)
        [ "J_"; "A_" ])
    [
      ( "or",
        [ ("S", "383"); ("U", "511"); ("SN", "127"); ("UN", "255") ],
        [ ("S", "127"); ("U", "255"); ("SN", "127"); ("UN", "255") ],
        [
          ("S16", "98303");
          ("U16", "131071");
          ("S32", "6442450943");
          ("U32", "8589934591");
        ],
        [
          ("S16", "32767");
          ("U16", "65535");
          ("S32", "2147483647");
          ("U32", "4294967295");
        ] );
      ( "xor",
        [ ("S", "383"); ("U", "511"); ("SN", "127"); ("UN", "255") ],
        [ ("S", "127"); ("U", "255"); ("SN", "127"); ("UN", "255") ],
        [
          ("S16", "98303");
          ("U16", "131071");
          ("S32", "6442450943");
          ("U32", "8589934591");
        ],
        [
          ("S16", "32767");
          ("U16", "65535");
          ("S32", "2147483647");
          ("U32", "4294967295");
        ] );
      ( "and",
        [ ("S", "255"); ("U", "255"); ("SN", "-1"); ("UN", "255") ],
        [ ("S", "-1"); ("U", "255"); ("SN", "-1"); ("UN", "255") ],
        [
          ("S16", "65535");
          ("U16", "65535");
          ("S32", "4294967295");
          ("U32", "4294967295");
        ],
        [
          ("S16", "-1"); ("U16", "65535"); ("S32", "-1"); ("U32", "4294967295");
        ] );
    ];
  let option = fixture "option" in
  Alcotest.(check string)
    "original option enabled" "1"
    (List.assoc "AFTER" option);
  Alcotest.(check string)
    "original option-case result" "254"
    (List.assoc "AOT_NOOPT" option);
  let pressure = fixture "pressure" in
  List.iter
    (fun prefix ->
      Alcotest.(check string)
        "original low pressure" "254"
        (List.assoc (prefix ^ "SMALL") pressure);
      Alcotest.(check string)
        "original high pressure" "-2"
        (List.assoc (prefix ^ "LARGE") pressure))
    [ "JIT_"; "AOT_" ]

let simple_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (base, qualifier, expected) ->
          let source =
            "I64 F(" ^ base ^ " " ^ qualifier ^ " x,I64 c){x<<=c;return x;}42;"
          in
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check bool)
            "original bounded parameter selection" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some expected))
        [
          ("I8", "", Analysis.Register_candidate);
          ("I8", "noreg", Analysis.Stack);
          ("U8", "", Analysis.Register_candidate);
          ("U8", "noreg", Analysis.Stack);
          ("I16", "", Analysis.Register_candidate);
          ("I16", "noreg", Analysis.Stack);
          ("U16", "", Analysis.Register_candidate);
          ("U16", "noreg", Analysis.Stack);
          ("I32", "", Analysis.Register_candidate);
          ("I32", "noreg", Analysis.Stack);
          ("U32", "", Analysis.Register_candidate);
          ("U32", "noreg", Analysis.Stack);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_parameter_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (ty, qualifier, expected) ->
          List.iter
            (fun update ->
              let source =
                Printf.sprintf "I64 F(I64 c){%s %s x=c;%sreturn x;}42;" ty
                  qualifier update
              in
              let fn = compile mode source in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let x = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "parameter local selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                   ~symbol:x
                = Some expected))
            [ ""; "x+=1;"; "x<<=1;" ])
        (List.concat_map
           (fun ty ->
             [
               (ty, "", Analysis.Register_candidate);
               (ty, "noreg", Analysis.Stack);
             ])
           [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ]))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (qualifier, expected) ->
          let source =
            "I64 F(){I8 " ^ qualifier ^ " x=255;x<<=0;return x;}42;"
          in
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check bool)
            "bounded local selection" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some expected))
        [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_return_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (qualifier, expected) ->
          let source = "I64 F(){I8 " ^ qualifier ^ " x=255;return x;}42;" in
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let symbol = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check bool)
            "local return selection" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
            = Some expected))
        [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_ownership () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = location fn.frame "x" |> Frame.location_symbol in
          let store =
            Body.body fn.body |> Graph.blocks
            |> List.concat_map (fun block ->
                Graph.instructions block |> Seq.instructions
                |> List.map Seq.description)
            |> List.find (fun (item : Seq.description) ->
                item.opcode = Holyc_lib__Ir.Opcode.Ic_assign)
          in
          Obj.set_field (Obj.repr store) 5 (Obj.repr (Some (Seq.Integer 1L)));
          Alcotest.(check bool)
            "changed local store invalidates proof" true
            (Option.is_none
               (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x));
          Alcotest.(check bool)
            "changed local store cannot be reproved" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(){I8 x=255;x<<=0;return x;}42;";
          "I64 F(){I8 x=255;return x;}42;";
          "I64 F(I64 c){I8 x=c;return x;}42;";
          "I64 F(I64 c){I8 x=c;x+=1;return x;}42;";
          "I64 F(I64 c){I8 x=c;x<<=1;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unsupported_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "unproved allocation remains unknown" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 c){I8 *p=&x;*p<<=c;return x;}42;";
          "I64 F(){I8 x=1;x<<=1;x++;return x;}42;";
          "I64 F(){I8 x=255;x++;return x;}42;";
          "I64 F(){I8 x=1,y=2;x<<=1;return x;}42;";
          "I64 F(){I8 x=1;I8 *p=&x;*p<<=1;return x;}42;";
          "I64 F(){I8 x;x<<=1;return x;}42;";
          "I64 F(){Bool x=1;x<<=1;return x;}42;";
          "I64 F(I8 c){I8 x=c;return x;}42;";
          "I64 F(I64 c){I8 x=c;x-=1;return x;}42;";
          "I64 F(I64 c){I8 x=c;x+=2147483648;return x;}42;";
          "I64 F(I64 c){I8 x=255;return x;}42;";
          "I64 F(){I8 x=255;x/=2;return x;}42;";
          "I64 F(){I8 x=1;x|=256;return x;}42;";
          "I64 F(){I8 x=1;x^=256;return x;}42;";
          "I64 F(){I8 x=1;x&=255;return x;}42;";
          "I64 F(){I8 x=255;x+=0x8000000000000000;return x;}42;";
          "I64 F(){I8 x=255;x+=2147483648;return x;}42;";
          "I64 F(){I8 x=255;x-=0x8000000000000000;return x;}42;";
          "I64 F(){I8 x=255;x-=2147483648;return x;}42;";
          "I64 F(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e,I64 f){x<<=c;return \
           x+a+b+d+e+f;}42;";
          "I64 F(I8 x,I64 c){if(c)x<<=c;return x;}42;";
          "I64 F(I8 x,I64 c){x<<=c;return c;}42;";
          "I64 F(I8 x,I64 c){x<<=c;return 42;}42;";
          "I64 F(I8 x,I64 c){return x;x<<=c;}42;";
          "I64 F(I8 x,I64 c){"
          ^ String.concat "" (List.init 40 (fun _ -> "c;"))
          ^ "x<<=c;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let ownership () =
  List.iter
    (fun mode ->
      let source = "I64 F(I8 x,I64 c){x<<=c;return x;}42;" in
      let original = compile mode source and foreign = compile mode source in
      let proof =
        Option.get (Analysis.analyze ~frame:original.frame ~body:original.body)
      in
      let x = Frame.location_symbol (location original.frame "x") in
      Alcotest.(check bool)
        "foreign frame" true
        (Option.is_none
           (Analysis.selection proof ~frame:foreign.frame ~body:original.body
              ~symbol:x));
      Alcotest.(check bool)
        "foreign body" true
        (Option.is_none
           (Analysis.selection proof ~frame:original.frame ~body:foreign.body
              ~symbol:x));
      let cell =
        Body.body original.body |> Graph.blocks |> List.hd |> Graph.instructions
        |> Seq.instructions
      in
      let description = Seq.description (List.hd cell) in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { description with span = None });
      Alcotest.(check bool)
        "changed original graph" true
        (Option.is_none
           (Analysis.selection proof ~frame:original.frame ~body:original.body
              ~symbol:x));
      let changed = compile mode source in
      let checked =
        Option.get (Analysis.analyze ~frame:changed.frame ~body:changed.body)
      in
      let changed_x = Frame.location_symbol (location changed.frame "x") in
      let description =
        Body.body changed.body |> Graph.blocks |> List.hd |> Graph.instructions
        |> Seq.instructions |> List.hd |> Seq.description
      in
      Obj.set_field (Obj.repr description) 6 (Obj.repr 1L);
      Alcotest.(check bool)
        "in-place flag change" true
        (Option.is_none
           (Analysis.selection checked ~frame:changed.frame ~body:changed.body
              ~symbol:changed_x));
      let changed_option = compile mode source in
      let option_proof =
        Option.get
          (Analysis.analyze ~frame:changed_option.frame
             ~body:changed_option.body)
      in
      let option_x =
        Frame.location_symbol (location changed_option.frame "x")
      in
      let altered_options =
        Int64.logor
          (Body.compiler_options changed_option.body)
          (Int64.shift_left 1L 34)
      in
      Obj.set_field (Obj.repr changed_option.body) 8 (Obj.repr altered_options);
      Alcotest.(check bool)
        "changed options invalidate proof" true
        (Option.is_none
           (Analysis.selection option_proof ~frame:changed_option.frame
              ~body:changed_option.body ~symbol:option_x));
      Alcotest.(check bool)
        "nondefault option remains unproved" true
        (Option.is_none
           (Analysis.analyze ~frame:changed_option.frame
              ~body:changed_option.body));
      let changed_slot = compile mode source in
      let slot_proof =
        Option.get
          (Analysis.analyze ~frame:changed_slot.frame ~body:changed_slot.body)
      in
      let slot_location = location changed_slot.frame "x" in
      let slot_symbol = Frame.location_symbol slot_location in
      let slot = Option.get (Frame.location_frame_slot slot_location) in
      Obj.set_field (Obj.repr slot) 0 (Obj.repr 999L);
      Alcotest.(check bool)
        "changed frame displacement invalidates proof" true
        (Option.is_none
           (Analysis.selection slot_proof ~frame:changed_slot.frame
              ~body:changed_slot.body ~symbol:slot_symbol));
      let changed_source = compile mode source in
      let source_proof =
        Option.get
          (Analysis.analyze ~frame:changed_source.frame
             ~body:changed_source.body)
      in
      let source_symbol =
        Frame.location_symbol (location changed_source.frame "x")
      in
      let typed = Option.get (Body.definition_source changed_source.body) in
      let statement =
        List.hd
          (Semantic_function_call_expression_result
           .function_expression_statements typed)
      in
      let expression =
        Semantic_function_call_expression_result.expression_statement_value
          statement
        |> Semantic_function_call_expression_result.result_source
        |> Semantic_function_call_resolution.argument_expression_source
        |> Option.get
      in
      (match expression with
      | Ast.Binary_expression binary ->
          Obj.set_field (Obj.repr binary) 2
            (Obj.repr (Option.get (Operator.find_binary ">>=")))
      | _ -> failwith "expected original binary expression");
      Alcotest.(check bool)
        "changed typed source invalidates proof" true
        (Option.is_none
           (Analysis.selection source_proof ~frame:changed_source.frame
              ~body:changed_source.body ~symbol:source_symbol));
      let changed_frame = compile mode source in
      let frame_proof =
        Option.get
          (Analysis.analyze ~frame:changed_frame.frame ~body:changed_frame.body)
      in
      let original_location = location changed_frame.frame "x" in
      let original_symbol = Frame.location_symbol original_location in
      let other_symbol = Frame.location_symbol (location foreign.frame "x") in
      Obj.set_field (Obj.repr original_location) 2 (Obj.repr other_symbol);
      Alcotest.(check bool)
        "changed frame location identity" true
        (Option.is_none
           (Analysis.selection frame_proof ~frame:changed_frame.frame
              ~body:changed_frame.body ~symbol:original_symbol)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Register variable analysis"
    [
      ( "source",
        [
          Alcotest.test_case "reference controls" `Quick source_controls;
          Alcotest.test_case "bounded simple selection" `Quick simple_selection;
          Alcotest.test_case "parameter-initialized local selection" `Quick
            local_parameter_selection;
          Alcotest.test_case "bounded local selection" `Quick local_selection;
          Alcotest.test_case "local return selection" `Quick
            local_return_selection;
          Alcotest.test_case "local ownership" `Quick local_ownership;
          Alcotest.test_case "unsupported shapes" `Quick unsupported_selection;
          Alcotest.test_case "original ownership" `Quick ownership;
        ] );
    ]
