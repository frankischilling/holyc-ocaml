open Holyc_lib
open Semantic_aggregate_layout

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %s"
        (diagnostics
        |> List.map (fun diagnostic ->
            Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
              diagnostic.message)
        |> String.concat ", ")

let parse ?(mode = Preprocessor.Jit) session ~path contents =
  let source = Session.add_source session ~path ~contents in
  let config =
    checked (Preprocessor.Config.create ~compilation_mode:mode ())
  in
  Holyc_lib.parse_with_config session ~config ~source |> expect_ast

type semantic_results = {
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  headers : Semantic_aggregate_header_resolution.t;
  member_types : Semantic_member_type_resolution.t;
  layouts : Semantic_aggregate_layout.t;
}

let resolve session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  let members = checked (Holyc_lib.collect_members session ~declarations ast) in
  let member_types =
    checked
      (Holyc_lib.resolve_member_types session ~declarations ~aggregates ~headers
         ~members ast)
  in
  let layouts =
    checked
      (Holyc_lib.layout_aggregates session ~declarations ~aggregates ~headers
         ~members:member_types ast)
  in
  { declarations; aggregates; headers; member_types; layouts }

let layout_named results name =
  Semantic_aggregate_layout.layouts results.layouts
  |> List.find (fun (layout : Semantic_aggregate_layout.aggregate_layout) ->
      layout.Semantic_aggregate_layout.symbol |> Semantic_symbol.name
      |> String.equal name)

let member_named (layout : Semantic_aggregate_layout.aggregate_layout) name =
  layout.Semantic_aggregate_layout.members
  |> List.find (fun (member : Semantic_aggregate_layout.member_layout) ->
      member.Semantic_aggregate_layout.symbol |> Semantic_symbol.name
      |> String.equal name)

let check_member layout name ~offset ~size =
  let member = member_named layout name in
  Alcotest.(check int64) (name ^ " offset") offset member.offset;
  Alcotest.(check int64) (name ^ " size") size member.size;
  member

let packed_class_has_no_implicit_alignment () =
  let session = Session.create () in
  let ast =
    parse session ~path:"packed-layout.HC"
      "class Packed { U8 byte; I64 wide; U16 tail; };"
  in
  let packed = resolve session ast |> fun results -> layout_named results "Packed" in
  Alcotest.(check int64) "packed size" 11L packed.size;
  Alcotest.(check int) "aggregate alignment" 1 packed.alignment;
  ignore (check_member packed "byte" ~offset:0L ~size:1L);
  ignore (check_member packed "wide" ~offset:1L ~size:8L);
  ignore (check_member packed "tail" ~offset:9L ~size:2L)

let base_prefix_and_union_overlap () =
  let session = Session.create () in
  let ast =
    parse session ~path:"base-and-union-layout.HC"
      "class Base { I16 first; U8 second; }; class Derived : Base { I64 value; \
       }; union Choice { I64 wide; U8 byte; union { I16 nested; U8 parts[3]; \
       }; I32 tail; };"
  in
  let results = resolve session ast in
  let base = layout_named results "Base" in
  let derived = layout_named results "Derived" in
  let choice = layout_named results "Choice" in
  Alcotest.(check int64) "base size" 3L base.size;
  Alcotest.(check int64) "derived size" 11L derived.size;
  let base_layout = Option.get derived.base in
  Alcotest.(check int64) "base offset" 0L base_layout.offset;
  Alcotest.(check int64) "base prefix size" 3L base_layout.size;
  ignore (check_member derived "value" ~offset:3L ~size:8L);
  Alcotest.(check int64) "union size" 11L choice.size;
  ignore (check_member choice "wide" ~offset:0L ~size:8L);
  ignore (check_member choice "byte" ~offset:0L ~size:1L);
  ignore (check_member choice "nested" ~offset:8L ~size:2L);
  ignore (check_member choice "parts" ~offset:8L ~size:3L);
  ignore (check_member choice "tail" ~offset:0L ~size:4L)

let arrays_pointers_callbacks_and_zero_size () =
  let session = Session.create () in
  let ast =
    parse session ~path:"array-layout.HC"
      "class Storage { U8 bytes[2][3]; I64 *ptrs[4]; I64 \
       (*callback)(I64 value); I0 zero; U8 flexible[]; };"
  in
  let storage =
    resolve session ast |> fun results -> layout_named results "Storage"
  in
  Alcotest.(check int64) "aggregate size" 46L storage.size;
  let bytes = check_member storage "bytes" ~offset:0L ~size:6L in
  Alcotest.(check (list int64)) "array dimensions" [ 2L; 3L ] bytes.dimensions;
  Alcotest.(check int64) "array element size" 1L bytes.element_size;
  let pointers = check_member storage "ptrs" ~offset:6L ~size:32L in
  Alcotest.(check int64) "pointer element size" 8L pointers.element_size;
  Alcotest.(check string) "pointer signedness" "unsigned"
    (Semantic_aggregate_layout.signedness_name pointers.signedness);
  ignore (check_member storage "callback" ~offset:38L ~size:8L);
  ignore (check_member storage "zero" ~offset:46L ~size:0L);
  let flexible = check_member storage "flexible" ~offset:46L ~size:0L in
  Alcotest.(check (list int64)) "empty first dimension" [ 0L ]
    flexible.dimensions

let explicit_and_negative_offsets () =
  let session = Session.create () in
  let ast =
    parse session ~path:"explicit-offset-layout.HC"
      "class Aligned { U8 tag; $$=($$+7)&-8; I64 value; }; class Negative { \
       $$=-16; I64 first; U8 tail; }; union NegativeUnion { $$=-8; I64 value; \
       };"
  in
  let results = resolve session ast in
  let aligned = layout_named results "Aligned" in
  let negative = layout_named results "Negative" in
  let negative_union = layout_named results "NegativeUnion" in
  Alcotest.(check int64) "explicitly aligned size" 16L aligned.size;
  ignore (check_member aligned "value" ~offset:8L ~size:8L);
  Alcotest.(check int64) "negative allocation size" 9L negative.size;
  Alcotest.(check int64) "negative adjustment" 16L negative.negative_offset;
  ignore (check_member negative "first" ~offset:(-16L) ~size:8L);
  ignore (check_member negative "tail" ~offset:(-8L) ~size:1L);
  Alcotest.(check int64) "negative union size" 8L negative_union.size;
  Alcotest.(check int64) "negative union adjustment" 8L
    negative_union.negative_offset;
  ignore (check_member negative_union "value" ~offset:(-8L) ~size:8L)

let expression_origin = Semantic_symbol.Synthesized "layout expression test"

let integer value =
  Semantic_aggregate_layout.Integer_expression
    { value; origin = expression_origin }

let binary operator left right =
  Semantic_aggregate_layout.Binary_expression
    { operator; left; right; origin = expression_origin }

let unary operator operand =
  Semantic_aggregate_layout.Unary_expression
    { operator; operand; origin = expression_origin }

let evaluate expression =
  Semantic_aggregate_layout.evaluate_expression
    ~context:Semantic_aggregate_layout.Array_dimension ~current_position:13L
    expression
  |> function
  | Ok value -> value
  | Error error -> Alcotest.fail (Semantic_aggregate_layout.error_to_string error)

let closed_expression_operators () =
  let binary_cases =
    [
      ("power", Semantic_aggregate_layout.Power, 2L, 3L, 8L);
      ("shift left", Shift_left, 2L, 3L, 16L);
      ("shift right", Shift_right, -8L, 1L, -4L);
      ("multiply", Multiply, 6L, 7L, 42L);
      ("divide", Divide, 21L, 3L, 7L);
      ("modulo", Modulo, 23L, 5L, 3L);
      ("bit and", Bit_and, 6L, 3L, 2L);
      ("bit xor", Bit_xor, 6L, 3L, 5L);
      ("bit or", Bit_or, 4L, 3L, 7L);
      ("add", Add, 9L, 4L, 13L);
      ("subtract", Subtract, 9L, 4L, 5L);
      ("less", Less, 2L, 3L, 1L);
      ("greater", Greater, 3L, 2L, 1L);
      ("less equal", Less_equal, 3L, 3L, 1L);
      ("greater equal", Greater_equal, 2L, 3L, 0L);
      ("equal", Equal, 4L, 4L, 1L);
      ("not equal", Not_equal, 4L, 4L, 0L);
      ("logical and", Logical_and, 4L, 3L, 1L);
      ("logical xor", Logical_xor, 4L, 0L, 1L);
      ("logical or", Logical_or, 0L, 3L, 1L);
    ]
  in
  List.iter
    (fun (name, operator, left, right, expected) ->
      Alcotest.(check int64) name expected
        (evaluate (binary operator (integer left) (integer right))))
    binary_cases;
  Alcotest.(check int64) "unary identity" 3L
    (evaluate (unary Identity (integer 3L)));
  Alcotest.(check int64) "unary negate" (-3L)
    (evaluate (unary Negate (integer 3L)));
  Alcotest.(check int64) "logical not" 1L
    (evaluate (unary Logical_not (integer 0L)));
  Alcotest.(check int64) "bitwise not" (-2L)
    (evaluate (unary Bitwise_not (integer 1L)));
  Alcotest.(check int64) "current position" 13L
    (evaluate
       (Semantic_aggregate_layout.Current_position_expression expression_origin));
  let skipped_division =
    binary Logical_and (integer 0L)
      (binary Divide (integer 1L) (integer 0L))
  in
  Alcotest.(check int64) "logical and short-circuits" 0L
    (evaluate skipped_division);
  let power = binary Power (integer 2L) (integer 3L) in
  let raw_offset =
    Semantic_aggregate_layout.evaluate_expression
      ~context:Semantic_aggregate_layout.Aggregate_offset ~current_position:0L
      power
    |> function
    | Ok value -> value
    | Error error ->
        Alcotest.fail (Semantic_aggregate_layout.error_to_string error)
  in
  Alcotest.(check int64) "LexExpression keeps F64 bits"
    (Int64.bits_of_float 8.0) raw_offset

let check_error_code expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string) "stable semantic code" expected
        (Semantic_aggregate_layout.error_code error);
      error

let expression_failures_are_typed () =
  let dependency =
    Semantic_aggregate_layout.Dependency_expression
      {
        dependency_kind = Semantic_aggregate_layout.Identifier_dependency;
        detail = "`COUNT`";
        origin = expression_origin;
      }
  in
  let error =
    Semantic_aggregate_layout.evaluate_expression
      ~context:Semantic_aggregate_layout.Array_dimension ~current_position:0L
      dependency
    |> check_error_code "HCSEMA0002"
  in
  (match Semantic_aggregate_layout.error_kind error with
  | Semantic_aggregate_layout.Unresolved_dependency
      { dependency_kind = Identifier_dependency; detail } ->
      Alcotest.(check string) "dependency detail" "`COUNT`" detail
  | _ -> Alcotest.fail "expected an identifier dependency");
  ignore
    (Semantic_aggregate_layout.evaluate_expression
       ~context:Semantic_aggregate_layout.Array_dimension ~current_position:0L
       (binary Divide (integer 1L) (integer 0L))
    |> check_error_code "HCSEMA0004");
  ignore
    (Semantic_aggregate_layout.evaluate_expression
       ~context:Semantic_aggregate_layout.Array_dimension ~current_position:0L
       (binary Divide (integer Int64.min_int) (integer (-1L)))
    |> check_error_code "HCSEMA0005");
  ignore
    (Semantic_aggregate_layout.evaluate_expression
       ~context:Semantic_aggregate_layout.Array_dimension ~current_position:0L
       (binary Power (integer 2L) (integer 1024L))
    |> check_error_code "HCSEMA0006")

let layout_error source =
  let session = Session.create () in
  let ast = parse session ~path:"bad-layout.HC" source in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  let members = checked (Holyc_lib.collect_members session ~declarations ast) in
  let member_types =
    checked
      (Holyc_lib.resolve_member_types session ~declarations ~aggregates ~headers
         ~members ast)
  in
  match
    Holyc_lib.layout_aggregates session ~declarations ~aggregates ~headers
      ~members:member_types ast
  with
  | Ok _ -> Alcotest.fail "expected aggregate layout to fail"
  | Error message -> message

let check_prefix prefix value =
  Alcotest.(check bool) prefix true (String.starts_with ~prefix value)

let layout_failures_stay_visible () =
  check_prefix "HCSEMA0003:"
    (layout_error "class Bad { I8 values[-1]; };");
  check_prefix "HCSEMA0004:"
    (layout_error "class Bad { I8 values[1/0]; };");
  check_prefix "HCSEMA0002:"
    (layout_error "I64 COUNT; class Bad { I8 values[COUNT]; };");
  check_prefix "HCSEMA0002:"
    (layout_error "class Node { Node value; };");
  check_prefix "HCSEMA0008:"
    (layout_error "class Huge { $$=0x7fffffffffffffff; I64 value; };")

let modes_and_repeat_runs_are_deterministic () =
  let signatures =
    [ Preprocessor.Jit; Preprocessor.Aot ]
    |> List.map (fun mode ->
        let session = Session.create () in
        let ast =
          parse ~mode session ~path:"layout-modes.HC"
            "class Base { U8 tag; }; class Item : Base { U16 values[3]; };"
        in
        let results = resolve session ast in
        let signature layouts =
          Semantic_aggregate_layout.layouts layouts
          |> List.map
               (fun (layout : Semantic_aggregate_layout.aggregate_layout) ->
              ( Semantic_symbol.name layout.Semantic_aggregate_layout.symbol,
                layout.size,
                List.map
                  (fun (member : Semantic_aggregate_layout.member_layout) ->
                    ( Semantic_symbol.name member.Semantic_aggregate_layout.symbol,
                      member.offset,
                      member.size ))
                  layout.members ))
        in
        let first = signature results.layouts in
        let second =
          checked
            (Holyc_lib.layout_aggregates session
               ~declarations:results.declarations
               ~aggregates:results.aggregates ~headers:results.headers
               ~members:results.member_types ast)
          |> signature
        in
        Alcotest.(check (list (triple string int64 (list (triple string int64 int64)))))
          "repeat layout" first second;
        first)
  in
  Alcotest.(check (list (triple string int64 (list (triple string int64 int64)))))
    "JIT and AOT layout" (List.nth signatures 0) (List.nth signatures 1)

let tests =
  [
    Alcotest.test_case "packed class" `Quick
      packed_class_has_no_implicit_alignment;
    Alcotest.test_case "base prefix and union overlap" `Quick
      base_prefix_and_union_overlap;
    Alcotest.test_case "arrays, pointers, callbacks, and I0" `Quick
      arrays_pointers_callbacks_and_zero_size;
    Alcotest.test_case "explicit and negative offsets" `Quick
      explicit_and_negative_offsets;
    Alcotest.test_case "closed expression operators" `Quick
      closed_expression_operators;
    Alcotest.test_case "typed expression failures" `Quick
      expression_failures_are_typed;
    Alcotest.test_case "visible layout failures" `Quick
      layout_failures_stay_visible;
    Alcotest.test_case "modes and deterministic repeats" `Quick
      modes_and_repeat_runs_are_deterministic;
  ]
