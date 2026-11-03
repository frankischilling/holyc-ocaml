open Holyc_lib
open Yojson.Safe.Util

let read name =
  let channel = open_in_bin ("oracle/aot-zero-initializers/" ^ name) in
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
        "retained native evidence"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [ (stem ^ ".hc", "payload_sha256"); (stem ^ ".capture", "capture_sha256") ];
  let fields =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (name, value) -> (name, to_int value))
  in
  let one =
    List.map (fun (name, value) -> name ^ "=" ^ string_of_int value) fields
    @ [ "POINTER_DONE" ]
  in
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
  let source = Session.add_source session ~path:"warning-oracle.hc" ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  if native then
    let report =
      Native_source_execution.evaluate ~max_code_bytes:524288 ~max_steps:100000
        session ~config ~source
    in
    let output = Native_source_execution.output_bytes report in
    match Native_source_execution.outcome report with
    | Ok result ->
        ( result.diagnostics,
          Ok
            (Option.map
               (fun w -> w.Native_source_execution.bits)
               result.value.final_value),
          output )
    | Error diagnostics -> (diagnostics, Error (), output)
  else
    let report =
      run_integer_program_report session ~config ~source ~max_steps:100000
    in
    let output = integer_program_report_output_bytes report in
    match integer_program_report_outcome report with
    | Ok result ->
        ( result.diagnostics,
          Ok
            (Option.map
               (fun w -> w.Ir_integer_interpreter.bits)
               (Ir_integer_interpreter.final_value result.value)),
          output )
    | Error diagnostics -> (diagnostics, Error (), output)

let warning_phases () =
  let fields = fixture "warnings" in
  let count name = List.assoc name fields in
  Alcotest.(check int)
    "constant copy adds no warning" 0
    (count "CONSTANT" - count "START");
  Alcotest.(check int)
    "integer load assignment adds one warning" 1
    (count "INTEGER" - count "CONSTANT");
  Alcotest.(check int)
    "U0 load expression and assignment add two warnings" 2
    (count "VOID" - count "INTEGER");
  Alcotest.(check int)
    "static load assignment adds one warning" 1
    (count "STATIC" - count "VOID");
  Alcotest.(check int)
    "original compilation succeeds" 0 (count "COMPILE_ERRORS")

let direct_call_warning () =
  let contents =
    "I64 Get(){return 7;}class Box{U0 empty;I64 value;};Box \
     object={Get(),42};I64 F(){return object.value;}F();"
  in
  List.iter
    (fun native ->
      let diagnostics, result, _ = run native Preprocessor.Aot contents in
      Alcotest.(check (option int64))
        "load value" (Some 7L) (Result.get_ok result);
      Alcotest.(check (list string))
        "original AOT warning" [ "HCSEMA0080" ]
        (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
    [ false; true ]

let direct_call_argument_warnings () =
  let fields = fixture "warnings-argument" in
  Alcotest.(check int)
    "original argument call warning" 1
    (List.assoc "INTEGER" fields - List.assoc "CONSTANT" fields);
  Alcotest.(check int)
    "original static argument warning" 1
    (List.assoc "STATIC" fields - List.assoc "VOID" fields);
  List.iter
    (fun native ->
      List.iter
        (fun source ->
          let diagnostics, result, _ = run native Preprocessor.Aot source in
          Alcotest.(check (option int64))
            "argument load value" (Some 7L) (Result.get_ok result);
          Alcotest.(check (list string))
            "argument load warning" [ "HCSEMA0080" ]
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [
          "I64 Get(I64 n){return n;}class Box{U0 empty;I64 value;};Box \
           object={Get(7),42};I64 F(){return object.value;}F();";
          "I64 Get(I64 n){return n;}class Box{U0 empty;I64 value;};I64 \
           F(){static Box object={Get(7),42};return object.value;}F();";
        ])
    [ false; true ];
  let void_fields = fixture "warnings-void-argument" in
  Alcotest.(check int)
    "original U0 call and assignment warnings" 2
    (List.assoc "VOID" void_fields - List.assoc "INTEGER" void_fields);
  List.iter
    (fun native ->
      let diagnostics, result, _ =
        run native Preprocessor.Aot
          "U0 Get(I64 n){}class Box{U0 empty;I64 value;};Box \
           object={Get(7),42};42;"
      in
      Alcotest.(check bool)
        "U0 register result remains unsupported" true (Result.is_error result);
      Alcotest.(check (list string))
        "warnings precede unsupported result"
        [ "HCSEMA0079"; "HCSEMA0080"; "HCRUN0006" ]
        (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
    [ false; true ]

let direct_call_two_argument_warnings () =
  let fields = fixture "warnings-two-arguments" in
  Alcotest.(check int)
    "original two-argument call warning" 1
    (List.assoc "INTEGER" fields - List.assoc "CONSTANT" fields);
  List.iter
    (fun native ->
      List.iter
        (fun source ->
          let diagnostics, result, _ = run native Preprocessor.Aot source in
          Alcotest.(check (option int64))
            "two-argument load value" (Some 7L) (Result.get_ok result);
          Alcotest.(check (list string))
            "two-argument load warning" [ "HCSEMA0080" ]
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [
          "I64 Get(I64 a,I64 b){return a+b;}class Box{U0 empty;I64 value;};Box \
           object={Get(3,4),42};I64 F(){return object.value;}F();";
          "I64 Get(I64 a,I64 b){return a+b;}class Box{U0 empty;I64 value;};I64 \
           F(){static Box object={Get(3,4),42};return object.value;}F();";
        ])
    [ false; true ]

let direct_call_global_argument_warnings () =
  let fields = fixture "warnings-global-argument" in
  Alcotest.(check int)
    "original global argument warning" 1
    (List.assoc "INTEGER" fields - List.assoc "CONSTANT" fields);
  List.iter
    (fun native ->
      List.iter
        (fun source ->
          let diagnostics, result, _ = run native Preprocessor.Aot source in
          Alcotest.(check (option int64))
            "global argument load value" (Some 7L) (Result.get_ok result);
          Alcotest.(check (list string))
            "global argument load warning" [ "HCSEMA0080" ]
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [
          "I64 N=7;I64 Get(I64 n){return n;}class Box{U0 empty;I64 value;};Box \
           object={Get(N),42};I64 F(){return object.value;}F();";
          "I64 N=7;I64 Get(I64 n){return n;}class Box{U0 empty;I64 value;};I64 \
           F(){static Box object={Get(N),42};return object.value;}F();";
        ])
    [ false; true ]

let direct_call_u8_argument_warnings () =
  let fields = fixture "warnings-u8-argument" in
  Alcotest.(check int)
    "original U8 parameter warning" 1
    (List.assoc "INTEGER" fields - List.assoc "CONSTANT" fields);
  Alcotest.(check int)
    "original void U8 parameter warnings" 2
    (List.assoc "VOID" fields - List.assoc "INTEGER" fields);
  List.iter
    (fun native ->
      List.iter
        (fun source ->
          let diagnostics, result, _ = run native Preprocessor.Aot source in
          Alcotest.(check (option int64))
            "U8 argument load value" (Some 7L) (Result.get_ok result);
          Alcotest.(check (list string))
            "U8 argument load warning" [ "HCSEMA0080" ]
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [
          "I64 Get(U8 n){return n;}class Box{U0 empty;I64 value;};Box \
           object={Get(7),42};I64 F(){return object.value;}F();";
          "I64 Get(U8 n){return n;}class Box{U0 empty;I64 value;};I64 \
           F(){static Box object={Get(7),42};return object.value;}F();";
        ];
      let diagnostics, result, _ =
        run native Preprocessor.Aot
          "U0 Get(U8 n){}class Box{U0 empty;I64 value;};Box \
           object={Get(7),42};42;"
      in
      Alcotest.(check bool)
        "U0 value still unsupported" true (Result.is_error result);
      Alcotest.(check (list string))
        "U0 warnings and result boundary"
        [ "HCSEMA0079"; "HCSEMA0080"; "HCRUN0006" ]
        (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
    [ false; true ]

let direct_call_numeric_argument_warnings () =
  let fields = fixture "warnings-numeric-arguments" in
  let types =
    [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64"; "F64"; "Bool" ]
  in
  List.iteri
    (fun index type_ ->
      Alcotest.(check int)
        (type_ ^ " original cumulative warning count")
        (index + 1) (List.assoc type_ fields);
      List.iter
        (fun native ->
          let source =
            Printf.sprintf
              "I64 Get(%s n){return 7;}class Box{U0 empty;I64 value;};Box \
               object={Get(7),42};I64 F(){return object.value;}F();"
              type_
          in
          let diagnostics, result, _ = run native Preprocessor.Aot source in
          let codes = List.map (fun (d : Diagnostic.t) -> d.code) diagnostics in
          if type_ = "F64" then (
            Alcotest.(check bool)
              "F64 frame remains unsupported" true (Result.is_error result);
            Alcotest.(check string)
              "warning precedes frame boundary" "HCSEMA0080" (List.hd codes))
          else (
            Alcotest.(check (option int64))
              (type_ ^ " load value") (Some 7L) (Result.get_ok result);
            Alcotest.(check (list string))
              (type_ ^ " load warning")
              [ "HCSEMA0080"; "HCSEMA0034" ]
              codes))
        [ false; true ])
    types

let direct_call_numeric_global_warnings () =
  let fields = fixture "warnings-numeric-global" in
  let types =
    [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64"; "F64"; "Bool" ]
  in
  List.iteri
    (fun index type_ ->
      Alcotest.(check int)
        (type_ ^ " original global warning count")
        (index + 1) (List.assoc type_ fields);
      List.iter
        (fun native ->
          let source =
            Printf.sprintf
              "I64 N=7;I64 Get(%s n){return 7;}class Box{U0 empty;I64 \
               value;};Box object={Get(N),42};I64 F(){return \
               object.value;}F();"
              type_
          in
          let diagnostics, result, _ = run native Preprocessor.Aot source in
          let codes = List.map (fun (d : Diagnostic.t) -> d.code) diagnostics in
          if type_ = "F64" then (
            Alcotest.(check bool)
              "F64 frame remains unsupported" true (Result.is_error result);
            Alcotest.(check string)
              "warning precedes frame boundary" "HCSEMA0080" (List.hd codes))
          else (
            Alcotest.(check (option int64))
              (type_ ^ " global load value")
              (Some 7L) (Result.get_ok result);
            Alcotest.(check (list string))
              (type_ ^ " global load warning")
              [ "HCSEMA0080"; "HCSEMA0034" ]
              codes))
        [ false; true ])
    types

let direct_call_warning_controls () =
  List.iter
    (fun native ->
      List.iter
        (fun (mode, source) ->
          let diagnostics, result, _ = run native mode source in
          Alcotest.(check bool) "control succeeds" true (Result.is_ok result);
          Alcotest.(check (list string))
            "no false warning" []
            (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics))
        [
          ( Preprocessor.Jit,
            "I64 Get(){return 7;}class Box{U0 empty;I64 value;};Box \
             object={Get(),42};I64 F(){return object.value;}F();" );
          ( Preprocessor.Aot,
            "I64 Get(){return 7;}class Box{I64 value;};Box object={Get()};I64 \
             F(){return object.value;}F();" );
          ( Preprocessor.Aot,
            "class Box{U0 empty;I64 value;};Box object={7,42};I64 F(){return \
             object.value;}F();" );
        ])
    [ false; true ]

let controls () =
  let fields = fixture "integer" in
  Alcotest.(check int)
    "original JIT copy width" 42
    (List.assoc "JIT_INTEGER" fields);
  Alcotest.(check int)
    "original AOT assignment width" 7
    (List.assoc "AOT_INTEGER" fields);
  Alcotest.(check int)
    "original static AOT assignment" 7
    (List.assoc "AOT_STATIC" fields);
  List.iter
    (fun native ->
      List.iter
        (fun mode ->
          let diagnostics, result, output =
            run native mode
              "class Box{U0 empty;I64 value;};Box object={7,42};I64 F(){return \
               object.value;}F();"
          in
          Alcotest.(check (option int64))
            "original constant copy remains empty"
            (Some (Int64.of_int (List.assoc "AOT_CONSTANT" fields)))
            (Result.get_ok result);
          Alcotest.(check int)
            "constant has no warnings" 0 (List.length diagnostics);
          Alcotest.(check string) "constant has no effects" "" output)
        [ Preprocessor.Jit; Preprocessor.Aot ])
    [ false; true ]

let original_store_width () =
  let fields = fixture "pattern" in
  List.iter
    (fun name ->
      Alcotest.(check int64)
        "original empty copy retains all bytes" 0x112233445566772aL
        (Int64.of_int (List.assoc name fields)))
    [ "JIT_INTEGER"; "AOT_CONSTANT" ];
  List.iter
    (fun name ->
      Alcotest.(check int64)
        "AOT U0 assignment overwrites low four bytes" 0x1122334400000007L
        (Int64.of_int (List.assoc name fields)))
    [ "AOT_INTEGER"; "AOT_STATIC" ]

let boundaries () =
  List.iter
    (fun native ->
      List.iter
        (fun (contents, expected, warnings) ->
          let _, jit, _ = run native Preprocessor.Jit contents in
          Alcotest.(check (option int64))
            "JIT retains its empty copy" (Some 42L) (Result.get_ok jit);
          let diagnostics, aot, output = run native Preprocessor.Aot contents in
          (match expected with
          | Some bits ->
              Alcotest.(check (option int64))
                "original AOT integer store" (Some bits) (Result.get_ok aot);
              Alcotest.(check (list string))
                "assignment warnings" warnings
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics)
          | None ->
              Alcotest.(check bool)
                "missing words/windows cannot become stores" true
                (Result.is_error aot);
              Alcotest.(check (list string))
                "unsupported assignment boundary"
                (warnings @ [ "HCRUN0006" ])
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics));
          Alcotest.(check string) "no unrequested output" "" output)
        [
          ( "I64 Get(){return 7;}class Box{U0 empty;I64 value;};Box \
             object={Get(),42};I64 F(){return object.value;}F();",
            Some 7L,
            [ "HCSEMA0080" ] );
          ( "I64 Get(){return 7;}class Box{U0 empty;I64 value;};I64 F(){static \
             Box object={Get(),42};return object.value;}F();",
            Some 7L,
            [ "HCSEMA0080" ] );
          ( "U0 Get(){}class Box{U0 empty;I64 value;};Box \
             object={Get(),42};I64 F(){return object.value;}F();",
            None,
            [ "HCSEMA0079"; "HCSEMA0080" ] );
          ( "U0 Get(){}class Box{U0 empty;I64 value;};I64 F(){static Box \
             object={Get(),42};return object.value;}F();",
            None,
            [ "HCSEMA0079"; "HCSEMA0080" ] );
          ( "I64 Get(){return 7;}class Empty{};Empty object=Get();42;",
            None,
            [ "HCSEMA0080" ] );
        ])
    [ false; true ]

let assignment_words () =
  let cases =
    [
      ( "lookahead",
        "JIT_LOOKAHEAD",
        "AOT_LOOKAHEAD",
        {|I64 Next(I64 n){return n;}extern class Item;class Box{Item item;U8 tail;$$=8;};I64 F(){static Box object={Next(
#exe {StreamExePrint("class Item{U16 value;};");}
7),42};U8 *p=(&object)(U8*);return p[2];}F();|}
      );
      ( "pattern",
        "JIT_INTEGER",
        "AOT_INTEGER",
        "I64 Get(){return 7;}class Box{U0 empty;I64 value;};Box \
         object={Get(),0x112233445566772a};I64 F(){return object.value;}F();" );
      ( "classes",
        "JIT_EMPTY",
        "AOT_EMPTY",
        "I64 Get(){return 7;}class Empty{};class Box{Empty empty;I64 \
         value;};Box object={Get(),0x112233445566772a};I64 F(){return \
         object.value;}F();" );
      ( "classes",
        "JIT_THREE",
        "AOT_THREE",
        "I64 Get(){return 7;}class Base{U8 bytes[3];};class Leaf:Base{};class \
         Box{Leaf leaf;U8 tail[5];};Box object={Get(),{5,4,3,2,1}};I64 \
         F(){return *(&object)(I64*);}F();" );
      ( "classes",
        "JIT_THREE",
        "AOT_STATIC",
        "I64 Get(){return 7;}class Base{U8 bytes[3];};class Leaf:Base{};class \
         Box{Leaf leaf;U8 tail[5];};I64 F(){static Box \
         object={Get(),{5,4,3,2,1}};return *(&object)(I64*);}F();" );
    ]
  in
  List.iter
    (fun (stem, jit_field, aot_field, contents) ->
      let fields = fixture stem in
      List.iter
        (fun native ->
          List.iter
            (fun (mode, field) ->
              let diagnostics, result, output = run native mode contents in
              Alcotest.(check (option int64))
                "native-observed raw store width"
                (Some (Int64.of_int (List.assoc field fields)))
                (Result.get_ok result);
              let warnings =
                if
                  mode = Preprocessor.Aot
                  && (stem = "pattern"
                     || (stem = "classes" && jit_field = "JIT_EMPTY"))
                then [ "HCSEMA0080" ]
                else []
              in
              Alcotest.(check (list string))
                "assignment diagnostics" warnings
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics);
              Alcotest.(check string) "no assignment output" "" output)
            [ (Preprocessor.Jit, jit_field); (Preprocessor.Aot, aot_field) ])
        [ false; true ])
    cases

let () =
  Alcotest.run "AOT zero-byte initializer phases"
    [
      ( "source",
        [
          Alcotest.test_case "repeated native BIN load records and hashes"
            `Quick (fun () ->
              ignore (fixture "void");
              ignore (fixture "integer");
              ignore (fixture "warnings");
              ignore (fixture "warnings-argument");
              ignore (fixture "warnings-void-argument");
              ignore (fixture "warnings-two-arguments");
              ignore (fixture "warnings-global-argument");
              ignore (fixture "warnings-u8-argument");
              ignore (fixture "warnings-numeric-arguments");
              ignore (fixture "warnings-numeric-global"));
          Alcotest.test_case "AOT warning counts occur during parsing" `Quick
            warning_phases;
          Alcotest.test_case "direct AOT calls warn at a U0 destination" `Quick
            direct_call_warning;
          Alcotest.test_case "direct call warning controls" `Quick
            direct_call_warning_controls;
          Alcotest.test_case "direct literal argument warnings" `Quick
            direct_call_argument_warnings;
          Alcotest.test_case "direct two literal argument warnings" `Quick
            direct_call_two_argument_warnings;
          Alcotest.test_case "direct global argument warnings" `Quick
            direct_call_global_argument_warnings;
          Alcotest.test_case "direct U8 literal argument warnings" `Quick
            direct_call_u8_argument_warnings;
          Alcotest.test_case "direct numeric literal argument warnings" `Quick
            direct_call_numeric_argument_warnings;
          Alcotest.test_case "direct numeric global argument warnings" `Quick
            direct_call_numeric_global_warnings;
          Alcotest.test_case
            "patterned native stores retain their observed width" `Quick
            original_store_width;
          Alcotest.test_case "constant copies retain their original width"
            `Quick controls;
          Alcotest.test_case "integer assignments match original raw widths"
            `Quick assignment_words;
          Alcotest.test_case
            "bounded actual words and unsupported U0 completions" `Quick
            boundaries;
        ] );
    ]
