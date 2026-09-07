module Ast = Frontend.Ast
module Diagnostic = Common.Diagnostic
module Sequence = Ir.Instruction_sequence
module Expression = Ir.Expression_lowering
module Typed = Sema.Function_call_expression_result

let ( let* ) = Result.bind
let diagnostic = Integer_source.diagnostic
let source_span = Integer_source.source_span

let prepare session ~config ~span ast =
  let* typed = Integer_source.prepare session ~config ~span ast in
  match Typed.top_level_statements typed with
  | [ statement ] -> (
      match Typed.top_level_statement_roots statement with
      | [ root ] -> Ok (Typed.top_level_root_value root)
      | _ ->
          Error
            [
              diagnostic ~span "HCEVAL0003"
                "expected one checked expression root";
            ])
  | _ ->
      Error
        [
          diagnostic ~span "HCEVAL0003"
            "expected one checked expression statement";
        ]

let harness ~span typed =
  let sequence_errors errors =
    List.map
      (fun (error : Sequence.error) ->
        diagnostic
          ~span:(Option.value error.span ~default:span)
          error.code error.message)
      errors
  in
  let one result =
    Result.map_error (fun error -> sequence_errors [ error ]) result
  in
  let* instruction_id = Sequence.Instruction_id.of_int 0 |> one in
  let* value_id = Sequence.Value_id.of_int 0 |> one in
  let* lowered =
    Expression.lower_typed_result ~instruction_id ~value_id typed
    |> Result.map_error sequence_errors
  in
  match lowered with
  | Expression.Unsupported_expression ->
      Error
        [
          diagnostic ~span "HCEVAL0002"
            "expression shape is not supported by expression lowering";
        ]
  | Expression.Lowered expression ->
      let return_id = Expression.next_instruction_id expression in
      let current = Sequence.Instruction_id.to_int return_id in
      let* ret_id =
        if current = Int.max_int then
          Error
            [
              diagnostic ~span "HCIRL0005"
                "cannot allocate expression return instruction";
            ]
        else Sequence.Instruction_id.of_int (current + 1) |> one
      in
      let return_value : Sequence.description =
        {
          instruction_id = return_id;
          opcode = Ir.Opcode.Ic_return_val;
          operands = [ Expression.result_value expression ];
          result = None;
          target_type = Some (Expression.result_type expression);
          payload = None;
          flags = 0L;
          span = Some span;
        }
      in
      let ret =
        {
          return_value with
          instruction_id = ret_id;
          opcode = Ir.Opcode.Ic_ret;
          operands = [];
          target_type = None;
        }
      in
      let instructions =
        Expression.sequence expression
        |> Sequence.instructions
        |> List.map Sequence.description
      in
      let* block_id = Sequence.Block_id.of_int 0 |> one in
      let* graph =
        Ir.Block_graph.create ~entry:block_id
          [
            {
              Ir.Block_graph.block_id;
              instructions = instructions @ [ return_value; ret ];
            };
          ]
        |> Result.map_error
             (List.map (fun (error : Ir.Block_graph.error) ->
                  diagnostic
                    ~span:(Option.value error.span ~default:span)
                    error.code error.message))
      in
      Ir.X87_stack.verify graph
      |> Result.map_error
           (List.map (fun (error : Ir.X87_stack.error) ->
                diagnostic
                  ~span:(Option.value error.span ~default:span)
                  error.code error.message))

let lower session ~config ~source =
  let parsed =
    Frontend.Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  match parsed.ast with
  | None -> Error parsed.diagnostics
  | Some ast -> (
      match ast.items with
      | [ Ast.Top_level_statement (Ast.Expression_statement statement) ] ->
          let span =
            (Ast.expression_location statement.expression_statement_expression)
              .span
          in
          let* typed = prepare session ~config ~span ast in
          harness ~span typed
      | _ ->
          Error
            [
              diagnostic ~span:ast.span "HCEVAL0001"
                "expected exactly one top-level ordinary expression statement";
            ])

let evaluate session ~config ~source ~max_steps =
  let span = source_span source in
  if max_steps <= 0 then
    Error
      [ diagnostic ~span "HCIRVM0001" "max_steps must be greater than zero" ]
  else
    let* graph = lower session ~config ~source in
    Ir.Integer_interpreter.execute ~max_steps graph
    |> Result.map_error
         (List.map (fun (error : Ir.Integer_interpreter.error) ->
              diagnostic
                ~span:(Option.value error.span ~default:span)
                error.code error.message))
