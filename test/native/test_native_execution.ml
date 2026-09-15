open Holyc_lib
module Fixture = Test_native_expression
module Native = X86_64_expression
module Runtime = Native_execution
module VM = Ir_integer_interpreter

let oracle label graph =
  let result =
    match VM.execute ~max_steps:100000 graph with
    | Ok result -> result
    | Error errors ->
        Alcotest.failf "%s: VM rejected fixture: %s" label
          (String.concat "; "
             (List.map
                (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
                errors))
  in
  match VM.termination result with
  | VM.Returned (Some word) -> word
  | _ -> Alcotest.failf "%s: VM did not return a full-width word" label

let execute label image =
  match Runtime.execute image with
  | Ok bits -> bits
  | Error message ->
      Alcotest.failf "%s: native execution failed: %s" label message

let check_result label (expected : VM.word) image =
  let expected_type =
    match expected.type_ with
    | VM.I64 -> Native.I64
    | VM.U64 -> Native.U64
  in
  Alcotest.(check string)
    (label ^ " result class")
    (Fixture.type_name expected_type)
    (Fixture.type_name (Native.value_type image));
  Alcotest.(check int64)
    (label ^ " all 64 return bits")
    expected.bits (execute label image)

let compare_graph label graph =
  let image = Fixture.image graph in
  Fixture.inspect_image graph image;
  let expected = oracle label graph in
  let before = Native.code image in
  check_result label expected image;
  Alcotest.(check string)
    (label ^ " execution preserves compiled bytes")
    before (Native.code image)

let edges () =
  List.iter
    (fun mode ->
      List.iter
        (fun text -> compare_graph text (Fixture.source_graph ~mode text))
        [
          "6*7;";
          "0;";
          "-1;";
          "0x8000000000000000;";
          "0xFFFFFFFFFFFFFFFF;";
          "0x7FFFFFFFFFFFFFFF+1;";
          "0xFFFFFFFFFFFFFFFF+1;";
          "(-9223372036854775807-1)-1;";
          "0xFFFFFFFFFFFFFFFF*0xFFFFFFFFFFFFFFFF;";
          "0x8000000000000000*2;";
          "-0x8000000000000000;";
          "~0x8000000000000000;";
          "(~0x8000000000000000)+2;";
          "(~(~0x8000000000000000))^7;";
          "-(~0x8000000000000000);";
          "(0xFEDCBA9876543210&0xFFFF0000FFFF0000)|0x1020304050607080;";
          "0x0123456789ABCDEF-0xFEDCBA9876543210;";
          "(100-7)-(7-100);";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let shared_values_and_types () =
  List.iter
    (fun (label, graph) -> compare_graph label graph)
    (Fixture.shared_cases ());
  List.iter
    (fun (label, graph, _) -> compare_graph label graph)
    (Fixture.class_cases ());
  compare_graph "seven live values use the extended volatile registers"
    (Fixture.pressure_graph 7)

(* The generator constructs source only. The existing VM is the result oracle. *)
let generated_sources () =
  let random = Random.State.make [| 0x642; 0x64; 0x2026 |] in
  let literal () =
    let bits = ref 0L in
    for _ = 1 to 4 do
      bits :=
        Int64.logor
          (Int64.shift_left !bits 16)
          (Int64.of_int (Random.State.int random 65536))
    done;
    Printf.sprintf "0x%016Lx" !bits
  in
  let operators = [| "+"; "-"; "*"; "&"; "|"; "^" |] in
  let rec expression depth =
    if depth = 0 then literal ()
    else
      match Random.State.int random 8 with
      | 0 -> literal ()
      | 1 -> "-(" ^ expression (depth - 1) ^ ")"
      | 2 -> "~(" ^ expression (depth - 1) ^ ")"
      | _ ->
          let left = expression (depth - 1) in
          let right = expression (depth - 1) in
          let operator = operators.(Random.State.int random 6) in
          (* Preserve a binary minus followed by unary minus as two operators,
             rather than accidentally generating a postfix -- token. *)
          "((" ^ left ^ ")" ^ operator ^ "(" ^ right ^ "))"
  in
  List.init 500 (fun index ->
      let mode =
        if index mod 2 = 0 then Preprocessor.Jit else Preprocessor.Aot
      in
      (index, mode, expression (1 + (index / 2 mod 4)) ^ ";"))

let generated_differential () =
  let cases = generated_sources () in
  Alcotest.(check int)
    "deterministic generated source count" 500 (List.length cases);
  Alcotest.(check bool)
    "a fresh seed reproduces every source and mode" true
    (cases = generated_sources ());
  List.iter
    (fun (index, mode, source) ->
      let label = Printf.sprintf "generated source %03d: %s" index source in
      compare_graph label (Fixture.source_graph ~mode source))
    cases

let repeated_execution () =
  let expressions =
    [
      "((0xFEDCBA9876543210^0x0123456789ABCDEF)*7)-42;";
      "-(6*7);";
      "~0x8000000000000000;";
    ]
  in
  let images =
    List.map
      (fun source ->
        let graph = Fixture.source_graph source in
        let image = Fixture.image graph in
        let expected = oracle source graph in
        let exported = Native.code image in
        Bytes.fill
          (Bytes.unsafe_of_string exported)
          0 (String.length exported) '\000';
        (source, expected, image))
      expressions
  in
  for round = 1 to 128 do
    List.iter
      (fun (source, expected, image) ->
        check_result
          (Printf.sprintf "repeat %d: %s" round source)
          expected image)
      images
  done

let () =
  (match Runtime.platform () with
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 -> ()
  | Runtime.Unsupported ->
      failwith
        "native-tests explicitly requires Windows x86-64 or Linux x86-64; this \
         platform is unsupported");
  Alcotest.run "native expression execution"
    [
      ( "native",
        [
          Alcotest.test_case "full-width source edge cases in JIT and AOT"
            `Quick edges;
          Alcotest.test_case "shared values, operand order and type classes"
            `Quick shared_values_and_types;
          Alcotest.test_case "500 deterministic public-source VM comparisons"
            `Quick generated_differential;
          Alcotest.test_case "repeated isolated execution and code ownership"
            `Quick repeated_execution;
        ] );
    ]
