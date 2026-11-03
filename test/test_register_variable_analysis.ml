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
  let incomplete = Yojson.Safe.from_string (read "local-add-high-fault.json") in
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
      ("local-add-high-fault.hc", "payload_sha256");
      ("local-add-high-fault.capture", "capture_sha256");
      ("local-add-high-fault.png", "screenshot_sha256");
    ];
  let incomplete_source = read "local-add-high-fault.hc" in
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
    (read "local-add-high-fault.capture");
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
          "I64 F(){I8 x=255;x+=0x8000000000000000;return x;}42;";
          "I64 F(){I8 x=255;x+=2147483648;return x;}42;";
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
          Alcotest.test_case "bounded local selection" `Quick local_selection;
          Alcotest.test_case "local return selection" `Quick
            local_return_selection;
          Alcotest.test_case "local ownership" `Quick local_ownership;
          Alcotest.test_case "unsupported shapes" `Quick unsupported_selection;
          Alcotest.test_case "original ownership" `Quick ownership;
        ] );
    ]
