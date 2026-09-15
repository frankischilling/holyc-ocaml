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

let compare_literal label expected_type expected_bits graph =
  let expected = oracle label graph in
  Alcotest.(check int64)
    (label ^ " independent expected bits")
    expected_bits expected.bits;
  let image = Fixture.image graph in
  Fixture.inspect_image graph image;
  Alcotest.(check string)
    (label ^ " independent expected type")
    (Fixture.type_name expected_type)
    (Fixture.type_name (Native.value_type image));
  check_result label expected image

let predicate_source_edges () =
  let cases =
    [
      ("1==1;", Native.I64, 1L);
      ("1==2;", Native.I64, 0L);
      ("1!=1;", Native.I64, 0L);
      ("1!=2;", Native.I64, 1L);
      ("-1<0;", Native.I64, 1L);
      ("1<0;", Native.I64, 0L);
      ("-1>=0;", Native.I64, 0L);
      ("0>=0;", Native.I64, 1L);
      ("1>0;", Native.I64, 1L);
      ("0>0;", Native.I64, 0L);
      ("0<=0;", Native.I64, 1L);
      ("1<=0;", Native.I64, 0L);
      ("(-9223372036854775807-1)<9223372036854775807;", Native.I64, 1L);
      ("9223372036854775807>(-9223372036854775807-1);", Native.I64, 1L);
      ("0x8000000000000000<0;", Native.I64, 0L);
      ("0x8000000000000000>=0;", Native.I64, 1L);
      ("0xFFFFFFFFFFFFFFFF>-1;", Native.I64, 0L);
      ("-1<0x8000000000000000;", Native.I64, 0L);
      ("0x8000000000000000<-1;", Native.I64, 1L);
      ("(~0x8000000000000000)<-1;", Native.I64, 1L);
      ("(~0x8000000000000000)>-1;", Native.I64, 0L);
      ("-1>(~0x8000000000000000);", Native.I64, 1L);
      ("((0x8000000000000000>0)-2)<0;", Native.I64, 1L);
      ("(0x8000000000000000>0)+41;", Native.I64, 42L);
      ("!0;", Native.I64, 1L);
      ("!(-1);", Native.I64, 0L);
      ("!0x0000000100000000;", Native.I64, 0L);
      ("!0x8000000000000000;", Native.U64, 0L);
      ("!0xFFFFFFFFFFFFFFFF;", Native.U64, 0L);
      ("!(0xFFFFFFFFFFFFFFFF+1);", Native.U64, 1L);
      ("!(~0xFFFFFFFFFFFFFFFF);", Native.U64, 1L);
      ("!(~0x8000000000000000);", Native.U64, 0L);
      ("!!0x8000000000000000;", Native.U64, 1L);
      ("(!0x8000000000000000)+(-1);", Native.U64, -1L);
      ("!(0x8000000000000000<0);", Native.I64, 1L);
      ("(1==1)+(2!=3)+(4<5)+(6>=6)+(7>6)+(8<=8)+36;", Native.I64, 42L);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected_type, expected_bits) ->
          compare_literal source expected_type expected_bits
            (Fixture.source_graph ~mode source))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let predicate_boundary_matrix () =
  (* Flipping the sign bit gives an independent unsigned ordering using only
     signed comparison. Every result is checked against both this oracle and
     the VM, including unequal high-bit patterns and equality boundaries. *)
  let relation =
    let open Ir_opcode in
    [
      (Ic_equ_equ, "==", fun order -> order = 0);
      (Ic_not_equ, "!=", fun order -> order <> 0);
      (Ic_less, "<", fun order -> order < 0);
      (Ic_greater_equ, ">=", fun order -> order >= 0);
      (Ic_greater, ">", fun order -> order > 0);
      (Ic_less_equ, "<=", fun order -> order <= 0);
    ]
  in
  let words =
    [ Int64.min_int; -1L; 0L; 1L; 256L; 0x0000000100000000L; Int64.max_int ]
  in
  let classes = [ ("I64", Fixture.i64, false); ("U64", Fixture.u64, true) ] in
  let count = ref 0 in
  List.iter
    (fun (left_name, left_type, left_unsigned) ->
      List.iter
        (fun (right_name, right_type, right_unsigned) ->
          List.iter
            (fun left ->
              List.iter
                (fun right ->
                  let ordered value =
                    if left_unsigned || right_unsigned then
                      Int64.logxor value Int64.min_int
                    else value
                  in
                  let order = Int64.compare (ordered left) (ordered right) in
                  List.iter
                    (fun (opcode, operator, predicate) ->
                      let label =
                        Printf.sprintf "%s:%016Lx %s %s:%016Lx" left_name left
                          operator right_name right
                      in
                      let graph =
                        let open Fixture in
                        single
                          [
                            imm ~type_:left_type 0 left;
                            imm ~type_:right_type 1 right;
                            binary 2 opcode 0 1;
                            return_value 3 2;
                            ret 4;
                          ]
                      in
                      compare_literal label Native.I64
                        (if predicate order then 1L else 0L)
                        graph;
                      incr count)
                    relation)
                words)
            words)
        classes)
    classes;
  Alcotest.(check int)
    "all signedness, condition and bit-pattern combinations" 1176 !count;
  List.iter
    (fun (name, type_, unsigned) ->
      List.iter
        (fun bits ->
          let graph =
            let open Fixture in
            single
              [
                imm ~type_ 0 bits;
                unary ~type_ 1 Ir_opcode.Ic_not 0;
                return_value ~type_ 2 1;
                ret 3;
              ]
          in
          compare_literal
            (Printf.sprintf "!%s:%016Lx" name bits)
            (if unsigned then Native.U64 else Native.I64)
            (if bits = 0L then 1L else 0L)
            graph)
        words)
    classes

let predicate_shared_values () =
  List.iter
    (fun (label, graph, expected) ->
      compare_literal label Native.I64 expected graph)
    (Fixture.predicate_shared_cases ());
  List.iter
    (fun (label, graph, _) -> compare_graph label graph)
    (Fixture.predicate_class_cases ());
  compare_literal "comparison preserves six inputs while allocating its result"
    Native.I64 22L
    (Fixture.predicate_pressure_graph ~logical_not:false 6);
  compare_literal "NOT preserves six inputs while allocating its result"
    Native.I64 21L
    (Fixture.predicate_pressure_graph ~logical_not:true 6)

let generated_predicate_sources () =
  let random = Random.State.make [| 0x644; 0x5052; 0x2026 |] in
  let literal () =
    let bits = ref 0L in
    for _ = 1 to 4 do
      bits :=
        Int64.logor
          (Int64.shift_left !bits 16)
          (Int64.of_int (Random.State.int random 65536))
    done;
    if Random.State.bool random then
      Printf.sprintf "(-0x%016Lx)" (Int64.logand !bits Int64.max_int)
    else Printf.sprintf "0x%016Lx" !bits
  in
  let arithmetic_operators = [| "+"; "-"; "*"; "&"; "|"; "^" |] in
  let rec arithmetic depth =
    if depth = 0 then literal ()
    else
      match Random.State.int random 5 with
      | 0 -> "~(" ^ arithmetic (depth - 1) ^ ")"
      | 1 -> "-(" ^ arithmetic (depth - 1) ^ ")"
      | _ ->
          let left = arithmetic (depth - 1) in
          let right = arithmetic (depth - 1) in
          let operator = arithmetic_operators.(Random.State.int random 6) in
          "((" ^ left ^ ")" ^ operator ^ "(" ^ right ^ "))"
  in
  let comparisons = [| "=="; "!="; "<"; ">="; ">"; "<=" |] in
  List.init 420 (fun index ->
      let left = arithmetic 2 in
      let predicate =
        if index mod 7 = 6 then "!(" ^ left ^ ")"
        else
          let right = arithmetic 2 in
          "((" ^ left ^ ")" ^ comparisons.(index mod 7) ^ "(" ^ right ^ "))"
      in
      let expression =
        match index mod 4 with
        | 0 -> predicate
        | 1 -> "!(" ^ predicate ^ ")"
        | 2 -> "((" ^ predicate ^ ")*41+1)"
        | _ -> "(((" ^ predicate ^ ")-2)<0)"
      in
      let mode =
        if index mod 2 = 0 then Preprocessor.Jit else Preprocessor.Aot
      in
      (index, mode, expression ^ ";"))

let generated_predicate_differential () =
  let cases = generated_predicate_sources () in
  Alcotest.(check int)
    "independent predicate generator count" 420 (List.length cases);
  Alcotest.(check bool)
    "predicate sources and modes reproduce from a fresh seed" true
    (cases = generated_predicate_sources ());
  List.iter
    (fun (index, mode, source) ->
      compare_graph
        (Printf.sprintf "predicate source %03d: %s" index source)
        (Fixture.source_graph ~mode source))
    cases

let compare_source_literal mode expected_type expected_bits contents =
  let label =
    (match mode with
      | Preprocessor.Jit -> "JIT "
      | Preprocessor.Aot -> "AOT ")
    ^ contents
  in
  let graph = Fixture.source_graph ~mode contents in
  let expected = oracle label graph in
  Alcotest.(check int64)
    (label ^ " independent VM result")
    expected_bits expected.bits;
  Alcotest.(check bool)
    (label ^ " independent VM class")
    (expected_type = Native.U64)
    (expected.type_ = VM.U64);
  let session, config, source = Fixture.source_inputs ~mode contents in
  let evaluated =
    Native_expression.evaluate session ~config ~source
    |> Fixture.require_ok Fixture.diagnostic_errors
  in
  Fixture.inspect_image graph evaluated.image;
  Alcotest.(check int64)
    (label ^ " public native API returns all 64 bits")
    expected_bits evaluated.bits;
  Alcotest.(check string)
    (label ^ " public native API result class")
    (Fixture.type_name expected_type)
    (Fixture.type_name (Native.value_type evaluated.image));
  Alcotest.(check bool)
    (label ^ " reports the executing platform")
    true
    (evaluated.platform = Runtime.platform ())

let logical_source_edges () =
  let cases =
    [
      ("0&&0;", Native.I64, 0L);
      ("0&&256;", Native.I64, 0L);
      ("256&&0;", Native.I64, 0L);
      ("256&&0x0000000100000000;", Native.I64, 1L);
      ("0||0;", Native.I64, 0L);
      ("0||256;", Native.I64, 1L);
      ("256||0;", Native.I64, 1L);
      ("256||0x0000000100000000;", Native.I64, 1L);
      ("0^^0;", Native.I64, 0L);
      ("0^^256;", Native.I64, 1L);
      ("256^^0;", Native.I64, 1L);
      ("256^^0x0000000100000000;", Native.I64, 0L);
      ("0x8000000000000000&&0x4000000000000000;", Native.I64, 1L);
      ("0x8000000000000000^^0x4000000000000000;", Native.I64, 0L);
      ("0xFFFFFFFFFFFFFFFF||0;", Native.I64, 1L);
      ("(3&&4)+(0||2)+(1^^1);", Native.I64, 2L);
      ("((0x8000000000000000&&1)-2)<0;", Native.I64, 1L);
      ("((0x8000000000000000||0)-2)<0;", Native.I64, 1L);
      ("((0x8000000000000000^^0)-2)<0;", Native.I64, 1L);
      ("!(0x8000000000000000^^0);", Native.I64, 0L);
      ("((~0x8000000000000000)&&256)+41;", Native.I64, 42L);
      ("(~(0x8000000000000000&&1))+1;", Native.I64, -1L);
      ("0xFFFFFFFFFFFFFFFF(I64i);", Native.I64, -1L);
      ("0x8000000000000000(I64i);", Native.I64, Int64.min_int);
      ("1(U64i);", Native.U64, 1L);
      ("0x8000000000000000(I64i)(U64i);", Native.U64, Int64.min_int);
      ("0x8000000000000000(I64i)<0;", Native.I64, 1L);
      ("0xFFFFFFFFFFFFFFFF(I64i)+1;", Native.I64, 0L);
      ("!0(U64i);", Native.U64, 1L);
      ("(!0(U64i))+(-2);", Native.U64, -1L);
      ("#define HIGH 0x8000000000000000\nHIGH&&256;", Native.I64, 1L);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, type_, bits) ->
          compare_source_literal mode type_ bits source)
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let logical_chain_sources () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bits) ->
          compare_source_literal mode Native.I64 bits source)
        (List.sort_uniq compare
           (Fixture.native_chain_sources @ Fixture.logical_source_cases ())))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let logical_and_word_view_matrix () =
  (* Table rows are false/false, false/true, true/false, true/true. This oracle
     uses literal truth tables rather than the backend's instruction plan. *)
  let operations =
    [
      (Ir_opcode.Ic_and_and, "&&", [| 0L; 0L; 0L; 1L |]);
      (Ir_opcode.Ic_or_or, "||", [| 0L; 1L; 1L; 1L |]);
      (Ir_opcode.Ic_xor_xor, "^^", [| 0L; 1L; 1L; 0L |]);
    ]
  in
  let words =
    [ 0L; 1L; 2L; 256L; 0x0000000100000000L; Int64.min_int; -1L; Int64.max_int ]
  in
  let classes =
    [ ("I64", Fixture.i64, Native.I64); ("U64", Fixture.u64, Native.U64) ]
  in
  let logical_count = ref 0 in
  let view_count = ref 0 in
  List.iter
    (fun (left_name, left_type, _) ->
      List.iter
        (fun (right_name, right_type, right_native) ->
          List.iter
            (fun left ->
              let view =
                let open Fixture in
                single
                  [
                    imm ~type_:left_type 0 left;
                    word_view ~type_:right_type 1 0;
                    return_value ~type_:right_type 2 1;
                    ret 3;
                  ]
              in
              compare_literal
                (Printf.sprintf "%s:%016Lx viewed as %s" left_name left
                   right_name)
                right_native left view;
              incr view_count;
              List.iter
                (fun right ->
                  let truth_index =
                    (if left = 0L then 0 else 2) + if right = 0L then 0 else 1
                  in
                  List.iter
                    (fun (opcode, operator, table) ->
                      let graph =
                        let open Fixture in
                        single
                          [
                            imm ~type_:left_type 0 left;
                            imm ~type_:right_type 1 right;
                            binary 2 opcode 0 1;
                            return_value 3 2;
                            ret 4;
                          ]
                      in
                      compare_literal
                        (Printf.sprintf "%s:%016Lx %s %s:%016Lx" left_name left
                           operator right_name right)
                        Native.I64 table.(truth_index) graph;
                      incr logical_count)
                    operations)
                words)
            words)
        classes)
    classes;
  Alcotest.(check int)
    "all bit-pattern, signedness and logical truth cases" 768 !logical_count;
  Alcotest.(check int)
    "all word-view bit-pattern and class pairs" 32 !view_count

let logical_shared_values () =
  List.iter
    (fun (label, graph, expected) ->
      compare_literal label Native.I64 expected graph)
    (Fixture.logical_shared_cases ());
  List.iter
    (fun (label, graph, type_, expected) ->
      compare_literal label type_ expected graph)
    (Fixture.word_view_shared_cases ());
  List.iter
    (fun (opcode, operator, both_true, _) ->
      List.iter
        (fun (duplicate, count, survivors, sum) ->
          compare_literal
            (Printf.sprintf "%s preserves %d later inputs, duplicate=%b"
               operator (List.length survivors) duplicate)
            Native.I64 (Int64.add sum both_true)
            (Fixture.logical_pressure_graph ~duplicate opcode count survivors))
        [
          (false, 5, [ 0; 1; 2; 3; 4 ], 15L);
          (false, 6, [ 0; 2; 3; 4; 5 ], 19L);
          (false, 7, [ 2; 3; 4; 5; 6 ], 25L);
          (true, 5, [ 0; 1; 2; 3; 4 ], 15L);
          (true, 6, [ 1; 2; 3; 4; 5 ], 20L);
        ])
    Fixture.logical_operations;
  compare_literal "word view preserves six live inputs" Native.I64 22L
    (Fixture.word_view_pressure_graph 6)

let generated_logical_sources () =
  let random = Random.State.make [| 0x646; 0x4c4f; 0x2026 |] in
  let literal () =
    match Random.State.int random 8 with
    | 0 -> "0"
    | 1 -> "256"
    | 2 -> "0x0000000100000000"
    | 3 -> "0x8000000000000000"
    | _ ->
        let bits = ref 0L in
        for _ = 1 to 4 do
          bits :=
            Int64.logor
              (Int64.shift_left !bits 16)
              (Int64.of_int (Random.State.int random 65536))
        done;
        Printf.sprintf "0x%016Lx" !bits
  in
  let logical = [| "&&"; "||"; "^^" |] in
  let arithmetic = [| "+"; "-"; "*"; "&"; "|"; "^" |] in
  let comparisons = [| "=="; "!="; "<"; ">="; ">"; "<=" |] in
  let binary left operator right =
    "((" ^ left ^ ")" ^ operator ^ "(" ^ right ^ "))"
  in
  let rec operand depth =
    if depth = 0 then literal ()
    else
      match Random.State.int random 6 with
      | 0 -> literal ()
      | 1 -> "!(" ^ operand (depth - 1) ^ ")"
      | 2 -> "~(" ^ operand (depth - 1) ^ ")"
      | choice ->
          let left = operand (depth - 1) in
          let right = operand (depth - 1) in
          let operators =
            if choice = 3 then arithmetic
            else if choice = 4 then comparisons
            else logical
          in
          binary left
            operators.(Random.State.int random (Array.length operators))
            right
  in
  List.init 240 (fun index ->
      let left = operand 2 in
      let right = operand 2 in
      let value = binary left logical.(index mod 3) right in
      let expression =
        match index mod 4 with
        | 0 -> value
        | 1 -> "!(" ^ value ^ ")"
        | 2 -> "((" ^ value ^ ")*41+1)"
        | _ -> "(((" ^ value ^ ")-2)<0)"
      in
      (index, expression ^ ";"))

let generated_logical_differential () =
  let cases = generated_logical_sources () in
  Alcotest.(check int)
    "independent logical source count" 240 (List.length cases);
  Alcotest.(check bool)
    "logical source generation is reproducible" true
    (cases = generated_logical_sources ());
  List.iter
    (fun mode ->
      List.iter
        (fun (index, source) ->
          compare_graph
            (Printf.sprintf "logical source %03d: %s" index source)
            (Fixture.source_graph ~mode source))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let check_spilled_graph label expected_type expected_bits graph =
  let expected = oracle label graph in
  Alcotest.(check int64)
    (label ^ " independent expected bits")
    expected_bits expected.bits;
  Alcotest.(check bool)
    (label ^ " VM class matches independent expectation")
    (expected_type = Native.U64)
    (expected.type_ = VM.U64);
  let image = Fixture.image graph in
  Fixture.inspect_image graph image;
  Alcotest.(check bool)
    (label ^ " uses an actual spill frame")
    true
    (Native.frame_bytes image > 0);
  check_result label expected image

let spill_source_pressure () =
  let cases =
    [
      (Fixture.pressure_source 7, Native.I64, 28L, 0);
      (Fixture.pressure_source 8, Native.I64, 36L, 8);
      ("1+(2+(3+(4+(5+(6+(7+14))))));", Native.I64, 42L, 8);
      ( "0x8000000000000000+(1+(2+(3+(4+(5+(6+7))))));",
        Native.U64,
        Int64.add Int64.min_int 28L,
        8 );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected_type, expected_bits, expected_frame) ->
          let label =
            (match mode with
              | Preprocessor.Jit -> "JIT spill source: "
              | Preprocessor.Aot -> "AOT spill source: ")
            ^ source
          in
          let graph = Fixture.source_graph ~mode source in
          let expected = oracle label graph in
          Alcotest.(check int64)
            (label ^ " independent expected bits")
            expected_bits expected.bits;
          Alcotest.(check bool)
            (label ^ " independent expected class")
            (expected_type = Native.U64)
            (expected.type_ = VM.U64);
          let session, config, source_file =
            Fixture.source_inputs ~mode source
          in
          let evaluated =
            Native_expression.evaluate session ~config ~source:source_file
            |> Fixture.require_ok Fixture.diagnostic_errors
          in
          Fixture.inspect_image graph evaluated.image;
          Alcotest.(check int)
            (label ^ " exact frame") expected_frame
            (Native.frame_bytes evaluated.image);
          Alcotest.(check int64)
            (label ^ " native result bits")
            expected_bits evaluated.bits;
          Alcotest.(check string)
            (label ^ " native result class")
            (Fixture.type_name expected_type)
            (Fixture.type_name (Native.value_type evaluated.image)))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let spill_operation_semantics () =
  List.iter
    (fun (label, graph, expected_type, expected_bits) ->
      check_spilled_graph label expected_type expected_bits graph)
    (Fixture.spill_semantic_cases ());
  check_spilled_graph "spill slot is reusable after a dead pressure phase"
    Native.I64 108L
    (Fixture.spill_slot_reuse_graph ())

let generated_high_pressure_sources () =
  let random = Random.State.make [| 0x648; 0x5350; 0x2026 |] in
  let operators = [| "+"; "-"; "^" |] in
  let literal index =
    match index mod 7 with
    | 0 -> "0x8000000000000000"
    | 1 -> "0xFFFFFFFFFFFFFFFF"
    | _ -> string_of_int (1 + Random.State.int random 97)
  in
  List.init 48 (fun index ->
      let count = 8 + (index mod 5) in
      let terms = Array.init count (fun term -> literal (index + term)) in
      let rec nest term =
        if term = count - 1 then terms.(term)
        else
          let operator =
            operators.(Random.State.int random (Array.length operators))
          in
          "(" ^ terms.(term) ^ operator ^ nest (term + 1) ^ ")"
      in
      (index, nest 0 ^ ";"))

let generated_high_pressure_differential () =
  let cases = generated_high_pressure_sources () in
  Alcotest.(check int)
    "deterministic high-pressure source count" 48 (List.length cases);
  Alcotest.(check bool)
    "high-pressure generator reproduces every source" true
    (cases = generated_high_pressure_sources ());
  List.iter
    (fun mode ->
      List.iter
        (fun (index, source) ->
          let label =
            Printf.sprintf "%s generated spill source %02d: %s"
              (match mode with
              | Preprocessor.Jit -> "JIT"
              | Preprocessor.Aot -> "AOT")
              index source
          in
          let graph = Fixture.source_graph ~mode source in
          let image = Fixture.image graph in
          Fixture.inspect_image graph image;
          Alcotest.(check bool)
            (label ^ " exceeds register-only pressure")
            true
            (Native.frame_bytes image > 0);
          check_result label (oracle label graph) image)
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let repeated_spill_execution () =
  let cases =
    ("right-nested source", Fixture.source_graph (Fixture.pressure_source 8))
    :: ("slot reuse", Fixture.spill_slot_reuse_graph ())
    :: List.map
         (fun (label, graph, _, _) -> (label, graph))
         (Fixture.spill_semantic_cases ())
  in
  let images =
    List.map
      (fun (label, graph) ->
        let image = Fixture.image graph in
        let expected = oracle label graph in
        let code = Native.code image in
        let unwind = Native.windows_unwind_info image in
        Alcotest.(check bool)
          (label ^ " repeated image spills")
          true
          (Native.frame_bytes image > 0);
        (label, expected, image, code, unwind))
      cases
  in
  for round = 1 to 128 do
    List.iter
      (fun (label, expected, image, code, unwind) ->
        check_result
          (Printf.sprintf "spill repeat %d: %s" round label)
          expected image;
        Alcotest.(check string)
          "repeated spill execution preserves code metadata" code
          (Native.code image);
        Alcotest.(check string)
          "repeated spill execution preserves unwind metadata" unwind
          (Native.windows_unwind_info image))
      images
  done

let repeated_execution () =
  let expressions =
    [
      "((0xFEDCBA9876543210^0x0123456789ABCDEF)*7)-42;";
      "-(6*7);";
      "~0x8000000000000000;";
      "!(~0xFFFFFFFFFFFFFFFF);";
      "(0x8000000000000000>0)+41;";
      "(256&&0x0000000100000000)+41;";
      "0x8000000000000000(I64i);";
      "(~0x8000000000000000)<-1<0;";
      "(~0x8000000000000000)>0>-1;";
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
          Alcotest.test_case
            "predicate source results and classes in JIT and AOT" `Quick
            predicate_source_edges;
          Alcotest.test_case "predicate signedness and all-bit boundary matrix"
            `Quick predicate_boundary_matrix;
          Alcotest.test_case "predicate sharing, aliases and extended registers"
            `Quick predicate_shared_values;
          Alcotest.test_case
            "420 independently generated predicate VM comparisons" `Quick
            generated_predicate_differential;
          Alcotest.test_case "logical and word-view public source execution"
            `Quick logical_source_edges;
          Alcotest.test_case "independent native comparison-chain expectations"
            `Quick logical_chain_sources;
          Alcotest.test_case
            "logical truth and word-view full-bit class matrices" `Quick
            logical_and_word_view_matrix;
          Alcotest.test_case
            "logical sharing, scratch pressure and view classes" `Quick
            logical_shared_values;
          Alcotest.test_case "240 logical sources in both preprocessing modes"
            `Quick generated_logical_differential;
          Alcotest.test_case
            "source pressure spills with exact full-width results" `Quick
            spill_source_pressure;
          Alcotest.test_case
            "spilled unary, binary, predicate, logical and lifetime semantics"
            `Quick spill_operation_semantics;
          Alcotest.test_case
            "48 deterministic high-pressure sources in both preprocessing modes"
            `Quick generated_high_pressure_differential;
          Alcotest.test_case "repeated spill execution restores the host stack"
            `Quick repeated_spill_execution;
        ] );
    ]
