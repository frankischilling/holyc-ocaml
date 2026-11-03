open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/source-root-shifts/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture ?(stem = "native") () =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "retained native evidence"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [ (stem ^ ".hc", "payload_sha256"); (stem ^ ".capture", "capture_sha256") ];
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
    "two complete native observations" (one @ one) actual;
  fields

let run native mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"root-shift-oracle.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  if native then
    let report =
      Native_source_execution.evaluate ~max_code_bytes:524288 ~max_steps:100000
        session ~config ~source
    in
    match Native_source_execution.outcome report with
    | Ok result ->
        ( result.diagnostics,
          Ok
            (Option.map
               (fun w -> w.Native_source_execution.bits)
               result.value.final_value) )
    | Error diagnostics -> (diagnostics, Error ())
  else
    let report =
      run_integer_program_report session ~config ~source ~max_steps:100000
    in
    match integer_program_report_outcome report with
    | Ok result ->
        ( result.diagnostics,
          Ok
            (Option.map
               (fun w -> w.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value)) )
    | Error diagnostics -> (diagnostics, Error ())

let source_values () =
  let fields = fixture () in
  let cases =
    [
      ("LEFT", "I64 N=7;I64 O=N<<1;O;");
      ("RIGHT", "I64 N=-8;I64 O=N>>1;O;");
      ("UNSIGNED", "U64 N=-8;I64 O=N>>1;O;");
      ("64", "I64 N=7;I64 O=N<<64;O;");
      ("65", "I64 N=7;I64 O=N<<65;O;");
    ]
  in
  List.iter
    (fun (field, source) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, key) ->
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                (field ^ " original value")
                (Some (Int64.of_string (List.assoc key fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT_" ^ field); (Preprocessor.Aot, field) ])
        [ false; true ])
    cases

let original_division () =
  List.iter
    (fun (stem, source) ->
      let fields = fixture ~stem () in
      List.iter
        (fun native ->
          List.iter
            (fun (mode, field) ->
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                "original rewritten division"
                (Some (Int64.of_string (List.assoc field fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no division diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT_DIV"); (Preprocessor.Aot, "AOT_DIV") ])
        [ false; true ])
    [
      ("division", "I64 G=-3;I64 H=G/2;H;");
      ("static-division", "I64 G=-3;I64 F(){static I64 H=G/2;return H;}F();");
    ]

let original_counts () =
  let fields = fixture ~stem:"counts" () in
  List.iter
    (fun (name, count) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, prefix) ->
              let source = Printf.sprintf "I64 N=7;I64 O=N<<(%s);O;" count in
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                (name ^ " original count value")
                (Some
                   (Int64.of_string (List.assoc (prefix ^ "_" ^ name) fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no count diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT"); (Preprocessor.Aot, "AOT") ])
        [ false; true ])
    [
      ("NEG", "-1");
      ("ZERO", "0");
      ("C63", "63");
      ("C64", "64");
      ("C65", "65");
      ("C128", "128");
    ]

let original_effects () =
  let fields = fixture ~stem:"effects" () in
  List.iter
    (fun native ->
      List.iter
        (fun (mode, prefix) ->
          Alcotest.(check string)
            "original update occurred once" "8"
            (List.assoc (prefix ^ "_N") fields);
          Alcotest.(check string)
            "original shifted result" "16"
            (List.assoc (prefix ^ "_O") fields);
          Alcotest.(check string)
            "original saved result survived later mutation" "16"
            (List.assoc (prefix ^ "_SAVED") fields);
          let diagnostics, result =
            run native mode
              "I64 N=7;I64 O=(++N)<<1;I64 F(){if(N!=8)return -1;N=20;return \
               O;}F();"
          in
          Alcotest.(check (option int64))
            "source update and saved value" (Some 16L) (Result.get_ok result);
          Alcotest.(check (list string))
            "no diagnostics" []
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [ (Preprocessor.Jit, "JIT"); (Preprocessor.Aot, "AOT") ])
    [ false; true ]

let original_static () =
  let fields = fixture ~stem:"static" () in
  let source =
    "I64 N=7;I64 F(){static I64 O=(++N)<<1;return O;}I64 Before=N;I64 \
     First=F();N=20;First*100+F()+Before;"
  in
  List.iter
    (fun native ->
      List.iter
        (fun (mode, prefix) ->
          let number field =
            Int64.of_string (List.assoc (prefix ^ field) fields)
          in
          let expected =
            Int64.add
              (Int64.mul (number "FIRST") 100L)
              (Int64.add (number "SECOND") (number "N_BEFORE"))
          in
          let diagnostics, result = run native mode source in
          Alcotest.(check (option int64))
            "original static lifetime and effects" (Some expected)
            (Result.get_ok result);
          Alcotest.(check (list string))
            "no static diagnostics" []
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
    [ false; true ];
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let diagnostics, result = run false mode source in
          Alcotest.(check bool)
            "unproved static shift is rejected" true (Result.is_error result);
          Alcotest.(check bool)
            "static rejection boundary" true
            (List.exists
               (fun (d : Diagnostic.t) -> d.code = "HCRUN0006")
               diagnostics))
        [
          "I64 F(I64 p){static I64 O=p<<2;return O;}42;";
          "I64 F(I64 p){static I64 O=p>>1;return O;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let original_dynamic () =
  let fields = fixture ~stem:"dynamic" () in
  let cases =
    [
      ("LEFT", "I64 N=7,C=65;I64 O=N<<C;O;");
      ("RIGHT", "I64 N=-8,C=65;I64 O=N>>C;O;");
      ("UNSIGNED", "U64 N=-8;I64 C=65;U64 O=N>>C;O;");
      ("SAVED", "I64 N=7,C=65;I64 O=N<<C;C=1;O;");
    ]
  in
  List.iter
    (fun (field, source) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, prefix) ->
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                "original dynamic shift"
                (Some (Int64.of_string (List.assoc (prefix ^ field) fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no dynamic shift diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
        [ false; true ])
    cases;
  let fields = fixture ~stem:"static-dynamic" () in
  let source =
    "I64 N=7,C=64;I64 F(){static I64 O=(++N)<<(++C);return O;}I64 \
     A=N,B=C,D=F();C=1;A*10000+B*100+D+F();"
  in
  List.iter
    (fun native ->
      List.iter
        (fun (mode, prefix) ->
          let number key = Int64.of_string (List.assoc (prefix ^ key) fields) in
          let expected =
            Int64.add
              (Int64.mul (number "N") 10000L)
              (Int64.add
                 (Int64.mul (number "C") 100L)
                 (Int64.add (number "FIRST") (number "SAVED")))
          in
          let diagnostics, result = run native mode source in
          Alcotest.(check (option int64))
            "original dynamic static effects" (Some expected)
            (Result.get_ok result);
          Alcotest.(check (list string))
            "no static dynamic diagnostics" []
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
    [ false; true ]

let original_dynamic_counts () =
  let fields = fixture ~stem:"dynamic-counts" () in
  List.iter
    (fun (name, count) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, prefix) ->
              let source = Printf.sprintf "I64 N=7,C=%s;I64 O=N<<C;O;" count in
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                "original variable count"
                (Some (Int64.of_string (List.assoc (prefix ^ name) fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no variable count diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
        [ false; true ])
    [ ("ZERO", "0"); ("NEG", "-1"); ("63", "63"); ("64", "64"); ("128", "128") ]

let original_narrow () =
  let fields = fixture ~stem:"narrow" () in
  let cases =
    [
      ("SIGNED", "I8 N=-8;I64 O=N>>1;O;");
      ("UNSIGNED", "U8 N=248;I64 O=N>>1;O;");
      ("DYN_SIGNED", "I8 N=-8,C=1;I64 O=N>>C;O;");
      ("DYN_UNSIGNED", "U8 N=248;I8 C=1;I64 O=N>>C;O;");
    ]
  in
  List.iter
    (fun (name, source) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, prefix) ->
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                "original narrow shift"
                (Some (Int64.of_string (List.assoc (prefix ^ name) fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no narrow shift diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
        [ false; true ])
    cases;
  let fields = fixture ~stem:"narrow-widths" () in
  List.iter
    (fun (type_, initial) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, prefix) ->
              let source =
                Printf.sprintf "%s N=%s;I8 C=1;I64 O=N>>C;O;" type_ initial
              in
              let _, result = run native mode source in
              Alcotest.(check (option int64))
                "original narrow width"
                (Some (Int64.of_string (List.assoc (prefix ^ type_) fields)))
                (Result.get_ok result))
            [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
        [ false; true ])
    [
      ("I8", "-8");
      ("U8", "248");
      ("I16", "-8");
      ("U16", "65528");
      ("I32", "-8");
      ("U32", "4294967288");
    ];
  let fields = fixture ~stem:"static-narrow" () in
  let source =
    "I8 N=-8,C=0;I64 F(){static I64 O=(++N)>>(++C);return O;}I64 \
     A=N,B=C,D=F();N=7;A*10000+B*100+D+F();"
  in
  List.iter
    (fun native ->
      List.iter
        (fun (mode, prefix) ->
          let number key = Int64.of_string (List.assoc (prefix ^ key) fields) in
          let expected =
            Int64.add
              (Int64.mul (number "N") 10000L)
              (Int64.add
                 (Int64.mul (number "C") 100L)
                 (Int64.add (number "FIRST") (number "SAVED")))
          in
          let _, result = run native mode source in
          Alcotest.(check (option int64))
            "original narrow static effects" (Some expected)
            (Result.get_ok result))
        [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
    [ false; true ]

let original_narrow_left () =
  let fields = fixture ~stem:"narrow-left" () in
  let cases =
    [
      ("POS", "I8 N=127,C=1;I64 O=N<<C;O;");
      ("NEG", "I8 N=-128,C=1;I64 O=N<<C;O;");
      ("UNSIGNED", "U8 N=255;I8 C=1;I64 O=N<<C;O;");
      ("CONST", "I8 N=127;I64 O=N<<1;O;");
    ]
  in
  List.iter
    (fun (name, source) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, prefix) ->
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                "original narrow left shift"
                (Some (Int64.of_string (List.assoc (prefix ^ name) fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no narrow left shift diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
        [ false; true ])
    cases

let original_arguments () =
  let fields = fixture ~stem:"arguments" () in
  let cases =
    [
      ("MIXED", "U8 U=1;I64 N=7;I64 Get(I64 n){return n;}I64 O=Get(U<<N);O;");
      ("SIGNED", "I8 N=-8,C=1;I64 Get(I64 n){return n;}I64 O=Get(N>>C);O;");
    ]
  in
  List.iter
    (fun (name, source) ->
      List.iter
        (fun native ->
          List.iter
            (fun (mode, prefix) ->
              let diagnostics, result = run native mode source in
              Alcotest.(check (option int64))
                "original shifted call argument"
                (Some (Int64.of_string (List.assoc (prefix ^ name) fields)))
                (Result.get_ok result);
              Alcotest.(check (list string))
                "no shifted argument diagnostics" []
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
            [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
        [ false; true ])
    cases

let original_argument_order () =
  let fields = fixture ~stem:"argument-order" () in
  let source =
    "I64 N=3,C=1;I64 Pair(I64 a,I64 b){return a*100+b;}I64 \
     R=Pair((++N)<<C,(++N)<<C);R*10+N;"
  in
  List.iter
    (fun native ->
      List.iter
        (fun (mode, prefix) ->
          let number key = Int64.of_string (List.assoc (prefix ^ key) fields) in
          let expected =
            Int64.add (Int64.mul (number "RESULT") 10L) (number "N")
          in
          let diagnostics, result = run native mode source in
          Alcotest.(check (option int64))
            "original shifted argument order" (Some expected)
            (Result.get_ok result);
          Alcotest.(check (list string))
            "no argument order diagnostics" []
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [ (Preprocessor.Jit, "JIT_"); (Preprocessor.Aot, "AOT_") ])
    [ false; true ]

let original_retained_callee () =
  let fields = fixture ~stem:"retained-callee" () in
  Alcotest.(check string)
    "one original initializer call" "1"
    (List.assoc "CALLS" fields);
  let source =
    "#exe {I64 XR=7,CR=65,NR=0;I64 FR(I64 x){NR++;return x<<CR;}I64 \
     IR(){return FR(XR);}I64 SR(I64 n=IR()){return \
     n;}StreamPrint(\"%d;\",SR()+(NR!=1)*1000);}"
  in
  List.iter
    (fun native ->
      List.iter
        (fun mode ->
          let diagnostics, result = run native mode source in
          Alcotest.(check (option int64))
            "original retained shift and single call"
            (Some (Int64.of_string (List.assoc "VALUE" fields)))
            (Result.get_ok result);
          Alcotest.(check (list string))
            "no retained shift diagnostics" []
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [ Preprocessor.Jit; Preprocessor.Aot ])
    [ false; true ]

let static_owner_identity () =
  let module G = Ir_integer_globals in
  let module L = Ir_integer_program_lowering in
  let module T = Semantic_function_call_expression_result in
  let compile () =
    let session = Session.create () in
    let source =
      Session.add_source session ~path:"static-owner.hc"
        ~contents:"I64 N=7;I64 F(){static I64 O=N<<1;return O;}F();"
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:Preprocessor.Aot ()
      |> Result.get_ok
    in
    let compiled =
      compile_integer_program session ~config ~source |> Result.get_ok
    in
    let globals = integer_program_globals compiled.value in
    let slot = List.hd (G.statics globals) in
    let frame = G.static_frame slot in
    let value = Option.get (G.static_initializer slot) |> T.initializer_value in
    let span =
      match T.result_origin value with
      | Semantic_symbol.Source_location location -> location.span
      | _ -> failwith "missing source span"
    in
    (globals, frame, value, span)
  in
  let globals, frame, value, span = compile () in
  let other_globals, other_frame, other_value, _ = compile () in
  let lowered =
    L.lower_complete ~frame ~globals ~span [ L.Expression value ]
    |> Result.get_ok
  in
  let owns frame globals value =
    L.owns_framed_expression lowered ~frame ~globals ~value
  in
  Alcotest.(check bool)
    "original frame, globals and root" true (owns frame globals value);
  Alcotest.(check bool) "foreign frame" false (owns other_frame globals value);
  Alcotest.(check bool) "foreign globals" false (owns frame other_globals value);
  Alcotest.(check bool) "foreign root" false (owns frame globals other_value);
  Alcotest.(check bool)
    "framed graph is not an unframed proof" false
    (L.owns_expression lowered ~globals ~value)

let boundaries () =
  List.iter
    (fun native ->
      List.iter
        (fun mode ->
          List.iter
            (fun source ->
              let diagnostics, result = run native mode source in
              Alcotest.(check bool)
                "unproved shift remains unsupported" true
                (Result.is_error result);
              Alcotest.(check bool)
                "rejection boundary retained" true
                (List.exists
                   (fun (d : Diagnostic.t) -> d.code = "HCRUN0006")
                   diagnostics))
            [
              "I64 F(I64 p){static I64 O=p<<2;return O;}42;";
              "I64 F(I64 p){static I64 O=p>>1;return O;}42;";
            ])
        [ Preprocessor.Jit; Preprocessor.Aot ])
    [ false; true ]

let () =
  Alcotest.run "Source global initializer shifts"
    [
      ( "source",
        [
          Alcotest.test_case "original results" `Quick source_values;
          Alcotest.test_case "original rewritten division" `Quick
            original_division;
          Alcotest.test_case "original once-only update" `Quick original_effects;
          Alcotest.test_case "original full-width counts" `Quick original_counts;
          Alcotest.test_case "original static initializer" `Quick
            original_static;
          Alcotest.test_case "original dynamic shifts" `Quick original_dynamic;
          Alcotest.test_case "original dynamic counts" `Quick
            original_dynamic_counts;
          Alcotest.test_case "original narrow shifts" `Quick original_narrow;
          Alcotest.test_case "original narrow left shifts" `Quick
            original_narrow_left;
          Alcotest.test_case "original shifted argument order" `Quick
            original_argument_order;
          Alcotest.test_case "original shifted arguments" `Quick
            original_arguments;
          Alcotest.test_case "original retained callee" `Quick
            original_retained_callee;
          Alcotest.test_case "static owner identity" `Quick
            static_owner_identity;
          Alcotest.test_case "unsupported boundaries" `Quick boundaries;
        ] );
    ]
