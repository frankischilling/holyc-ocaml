open Holyc_lib
module G = Test_integer_globals
module F = Test_integer_functions
module Layout = Semantic_aggregate_layout

let check_extent (expression, count) =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let compiled = G.compile ~mode source in
          Alcotest.(check int)
            "declared extent bytes" (8 * count)
            (compiled |> integer_program_globals |> Ir_integer_globals.byte_size);
          ignore (G.run ~mode source |> F.expect 42L))
        [
          Printf.sprintf "I64 A[%s];A[%d]=42;A[%d];" expression (count - 1)
            (count - 1);
          Printf.sprintf "I64 F(){static I64 A[%s];A[%d]=42;return A[%d];}F();"
            expression (count - 1) (count - 1);
        ])
    G.modes

let unsigned_operations () =
  List.iter check_extent
    [
      ("(0xffffffffffffffff>0)+1", 2);
      ("(0x8000000000000000>=1)+1", 2);
      ("(0xffffffffffffffff<-1)+1", 1);
      ("(0xffffffffffffffff<=-1)+1", 2);
      ("0xffffffffffffffff/0x8000000000000000+1", 2);
      ("0xffffffffffffffff%3+1", 1);
      ("(0x8000000000000000>>63)+1", 2);
      ("((0x8000000000000000<<1)<-1)+1", 2);
      ("((0xffffffffffffffff+2)<-1)+1", 2);
    ]

let unary_result_types () =
  List.iter check_extent
    [
      ("(-0x8000000000000000<0)+1", 2);
      ("(-(0xffffffffffffffff+2)<0)+1", 2);
      ("((~(0xffffffffffffffff+1))<0)+1", 2);
      ("(!0xffffffffffffffff<-1)+1", 2);
      ("((!0.0)<<1)+1", 2);
      ("((0xffffffffffffffff>0)<-1)+1", 1);
      ("((0xffffffffffffffff&&1)<-1)+1", 1);
    ]

let fractional_extents () =
  List.iter check_extent
    [
      ("2.9", 2); ("(2`-1)+2", 2); ("1.5<2.0", 1); ("0xffffffffffffffff+3.0", 2);
    ]

let comparison_chain_boundary () =
  check_extent ("((0<1)<2)+1", 2);
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I64 A[0<1<2];42;";
          "I64 F(){static I64 A[0<1<2];return 42;}F();";
          "I64 A[1||(0<1<2)];42;";
          "I64 F(){static I64 A[1||(0<1<2)];return 42;}F();";
        ])
    G.modes

let invalid_extents () =
  List.iter
    (fun mode ->
      List.iter
        (fun dimensions ->
          List.iter
            (fun source -> ignore (F.first_error (G.run ~mode source)))
            [
              "I64 A" ^ dimensions ^ ";42;";
              "I64 F(){static I64 A" ^ dimensions ^ ";return 42;}F();";
            ])
        [
          "[-1]";
          "[-1.5]";
          "[0.5]";
          "[0xffffffffffffffff]";
          "[0x7fffffffffffffff][2]";
          "[0x100000000][0x100000000]";
          "[1/0]";
          "[(-0x8000000000000000)/(-1)]";
        ])
    G.modes

let signed_constructor_compatibility () =
  let origin = Semantic_symbol.Synthesized "signed layout API control" in
  let integer value = Layout.Integer_expression { value; origin } in
  let binary operator left right =
    Layout.Binary_expression { operator; left; right; origin }
  in
  List.iter
    (fun (expression, expected) ->
      match
        Layout.evaluate_expression ~context:Layout.Array_dimension
          ~current_position:0L expression
      with
      | Ok value ->
          Alcotest.(check int64) "explicit signed expression" expected value
      | Error error -> Alcotest.fail (Layout.error_to_string error))
    [
      (binary Layout.Less (integer (-1L)) (integer 0L), 1L);
      (binary Layout.Shift_right (integer (-8L)) (integer 1L), -4L);
      (binary Layout.Divide (integer (-9L)) (integer 2L), -4L);
      (binary Layout.Modulo (integer (-9L)) (integer 2L), -1L);
    ]

let tests =
  [
    Alcotest.test_case "unsigned extent operators" `Quick unsigned_operations;
    Alcotest.test_case "extent unary result types" `Quick unary_result_types;
    Alcotest.test_case "global and static fractional truncation" `Quick
      fractional_extents;
    Alcotest.test_case "explicit comparison chain boundary" `Quick
      comparison_chain_boundary;
    Alcotest.test_case "invalid and overflowing extents" `Quick invalid_extents;
    Alcotest.test_case "signed layout constructor compatibility" `Quick
      signed_constructor_compatibility;
  ]
