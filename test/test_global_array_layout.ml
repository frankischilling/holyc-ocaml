open Holyc_lib
module D = Test_global_dimension_binding
module Layout = Semantic_global_array_layout
module Binding = Semantic_global_dimension_binding

let checked = D.checked

let original_expression_and_inputs () =
  let prepared = D.prepare ~path:"extent-source.hc" "I64 A[2+1];" in
  let environment = D.jit_environment prepared [] [] in
  let bindings = D.resolve prepared environment in
  let layout =
    layout_global_arrays prepared.session ~bindings prepared.ast |> checked
  in
  let record = List.hd (Layout.layouts layout) in
  Alcotest.(check (list int64))
    "original extent" [ 3L ] (Layout.dimensions record);
  let dimensions = Layout.dimension_inputs record in
  let input = List.hd dimensions in
  let expression = Option.get input.expression in
  let table = Session.semantic_symbols prepared.session in
  let accept dimensions =
    Layout.layout ~table ~bindings
      [ { Layout.global = Layout.source record; dimensions } ]
  in
  ignore (accept dimensions |> checked);
  let reject label expression =
    match accept [ { input with expression = Some expression } ] with
    | Error _ -> ()
    | Ok _ ->
        Alcotest.fail (label ^ " replaced the original dimension expression")
  in
  let literal location value =
    Ast.Integer_literal
      (Ast.make_expression_literal ~origin:Ast.Source_literal
         ~spelling:(Int64.to_string value) ~value:(Ast.Integer_value value)
         ~location)
  in
  reject "literal payload" (literal (Ast.expression_location expression) 4L);
  begin match expression with
  | Ast.Binary_expression binary ->
      let rebuild left operator right =
        Ast.Binary_expression
          (Ast.make_binary_expression ~left ~operator
             ~operator_spec:binary.binary_operator_spec ~right
             ~location:binary.binary_location)
      in
      reject "identifier-free subtree"
        (rebuild
           (literal (Ast.expression_location binary.binary_left) 3L)
           binary.binary_operator binary.binary_right);
      reject "operator payload"
        (rebuild binary.binary_left
           (Ast.make_expression_operator ~spelling:"-"
              ~location:binary.binary_operator.operator_location)
           binary.binary_right);
      reject "copied tree identity"
        (rebuild binary.binary_left binary.binary_operator binary.binary_right)
  | _ -> Alcotest.fail "expected binary source fixture"
  end;
  let foreign = D.prepare ~path:"extent-source.hc" "I64 A[2+1];" in
  begin match layout_global_arrays prepared.session ~bindings foreign.ast with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "foreign equal-span AST replaced original dimension"
  end

let tests =
  [
    Alcotest.test_case "original extent expression identity" `Quick
      original_expression_and_inputs;
  ]
