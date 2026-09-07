module Ast = Frontend.Ast
module Typed = Sema.Function_call_expression_result
module Lower = Ir.Integer_program_lowering
module Span_map = Map.Make (Common.Span)

type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }

let ( let* ) = Result.bind

exception Invalid of Common.Diagnostic.t

let fail span code message =
  raise (Invalid (Integer_source.diagnostic ~span code message))

let lower session ~config ~source =
  let parsed =
    Frontend.Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  match parsed.ast with
  | None -> Error parsed.diagnostics
  | Some ast -> (
      let lowered =
        try
          let rec validate = function
            | Ast.Empty_statement _
            | Ast.Expression_statement _
            | Ast.Break_statement _ -> ()
            | Ast.Block_statement block ->
                List.iter validate block.block_statements
            | Ast.Sequence_statement sequence ->
                List.iter
                  (fun element -> validate element.Ast.sequence_statement)
                  sequence.sequence_elements
            | Ast.If_statement branch ->
                validate branch.if_then_branch;
                Option.iter
                  (fun clause -> validate clause.Ast.else_branch)
                  branch.if_else_clause
            | Ast.While_statement loop -> validate loop.while_body
            | Ast.Do_while_statement loop -> validate loop.do_body
            | Ast.For_statement loop ->
                validate loop.for_initializer;
                Option.iter validate loop.for_update;
                validate loop.for_body
            | other ->
                fail (Ast.statement_location other).span "HCRUN0001"
                  "statement is outside the integer program execution domain"
          in
          let statements =
            List.map
              (function
                | Ast.Top_level_statement statement ->
                    validate statement;
                    statement
                | _ ->
                    fail ast.span "HCRUN0001"
                      "integer programs currently accept executable top-level \
                       statements only")
              ast.items
          in
          let* typed =
            Integer_source.prepare session ~config ~span:ast.span ast
          in
          let roots =
            Typed.top_level_statements typed
            |> List.concat_map Typed.top_level_statement_roots
            |> List.fold_left
                 (fun roots root ->
                   let value = Typed.top_level_root_value root in
                   match Typed.result_origin value with
                   | Sema.Symbol.Source_location location ->
                       if Span_map.mem location.span roots then
                         fail location.span "HCRUN0004"
                           "typed source roots have duplicate locations";
                       Span_map.add location.span value roots
                   | _ ->
                       fail ast.span "HCRUN0004"
                         "typed source root has no physical source location")
                 Span_map.empty
          in
          let consumed = ref Span_map.empty in
          let expression source =
            let span = (Ast.expression_location source).span in
            match Span_map.find_opt span roots with
            | Some value ->
                if Span_map.mem span !consumed then
                  fail span "HCRUN0004"
                    "source expression uses a typed root twice";
                consumed := Span_map.add span () !consumed;
                value
            | None ->
                fail span "HCRUN0004"
                  "source expression has no matching typed root"
          in
          let rec statement = function
            | Ast.Empty_statement empty ->
                Lower.Empty empty.empty_statement_location.span
            | Ast.Expression_statement item ->
                Lower.Expression
                  (expression item.expression_statement_expression)
            | Ast.Block_statement block ->
                Lower.Block (List.map statement block.block_statements)
            | Ast.Sequence_statement sequence ->
                Lower.Block
                  (List.map
                     (fun item -> statement item.Ast.sequence_statement)
                     sequence.sequence_elements)
            | Ast.Break_statement item -> Lower.Break item.break_location.span
            | Ast.If_statement item ->
                Lower.If
                  ( expression item.if_condition,
                    statement item.if_then_branch,
                    Option.map
                      (fun clause -> statement clause.Ast.else_branch)
                      item.if_else_clause )
            | Ast.While_statement item ->
                Lower.While
                  (expression item.while_condition, statement item.while_body)
            | Ast.Do_while_statement item ->
                Lower.Do_while
                  (statement item.do_body, expression item.do_while_condition)
            | Ast.For_statement item ->
                Lower.For
                  ( statement item.for_initializer,
                    expression item.for_condition,
                    Option.map statement item.for_update,
                    statement item.for_body )
            | other ->
                fail (Ast.statement_location other).span "HCRUN0001"
                  "statement is outside the integer program execution domain"
          in
          let statements = List.map statement statements in
          if Span_map.cardinal !consumed <> Span_map.cardinal roots then
            fail ast.span "HCRUN0004"
              "integer program did not consume every typed source root";
          Lower.lower ~span:ast.span statements
        with Invalid diagnostic -> Error [ diagnostic ]
      in
      match lowered with
      | Ok value -> Ok { value; diagnostics = parsed.diagnostics }
      | Error diagnostics -> Error (parsed.diagnostics @ diagnostics))

let run session ~config ~source ~max_steps =
  let span = Integer_source.source_span source in
  if max_steps <= 0 then
    Error
      [
        Integer_source.diagnostic ~span "HCIRVM0001"
          "max_steps must be greater than zero";
      ]
  else
    let* graph = lower session ~config ~source in
    Ir.Integer_interpreter.execute ~max_steps graph.value
    |> Result.map (fun value -> { value; diagnostics = graph.diagnostics })
    |> Result.map_error
         (List.map (fun (error : Ir.Integer_interpreter.error) ->
              let stage =
                match error.stage with
                | Ir.Integer_interpreter.Configuration -> "configuration"
                | Preflight -> "preflight"
                | Execution -> "execution"
              in
              let identity name = function
                | None -> []
                | Some id -> [ Printf.sprintf "%s=%d" name id ]
              in
              Common.Diagnostic.make ~code:error.code
                ~severity:Common.Diagnostic.Error ~message:error.message
                ~primary:(Option.value error.span ~default:span)
                ~notes:
                  ([
                     "stage=" ^ stage;
                     Printf.sprintf "executed_steps=%d" error.executed_steps;
                   ]
                  @ identity "block_id" error.block_id
                  @ identity "instruction_id" error.instruction_id)
                ()))
    |> Result.map_error (fun diagnostics -> graph.diagnostics @ diagnostics)
