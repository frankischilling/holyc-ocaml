type error_kind = Explicit_return
type error = { code : string; kind : error_kind; origin : Sema.Symbol.origin }

let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Explicit_return -> "explicit return is not allowed outside a function"

let error_to_string error = error.code ^ ": " ^ error_message error

let rec statements = function
  | [] -> Ok ()
  | statement :: rest -> (
      match validate_statement statement with
      | Error _ as error -> error
      | Ok () -> statements rest)

and switch_elements = function
  | [] -> Ok ()
  | element :: rest -> (
      let result =
        match element with
        | Frontend.Ast.Switch_case_element _
        | Frontend.Ast.Switch_default_element _ -> Ok ()
        | Frontend.Ast.Switch_subswitch_element subswitch ->
            switch_elements subswitch.subswitch_elements
        | Frontend.Ast.Switch_statement_element statement ->
            validate_statement statement
      in
      match result with
      | Error _ as error -> error
      | Ok () -> switch_elements rest)

and validate_statement = function
  | Frontend.Ast.Return_statement return_ ->
      Error
        {
          code = "HCSEMA0066";
          kind = Explicit_return;
          origin = origin return_.return_keyword;
        }
  | Frontend.Ast.Block_statement block -> statements block.block_statements
  | Frontend.Ast.Do_while_statement do_while ->
      validate_statement do_while.do_body
  | Frontend.Ast.For_statement for_ -> (
      match validate_statement for_.for_initializer with
      | Error _ as error -> error
      | Ok () -> (
          match for_.for_update with
          | Some update -> (
              match validate_statement update with
              | Error _ as error -> error
              | Ok () -> validate_statement for_.for_body)
          | None -> validate_statement for_.for_body))
  | Frontend.Ast.If_statement if_ -> (
      match validate_statement if_.if_then_branch with
      | Error _ as error -> error
      | Ok () -> (
          match if_.if_else_clause with
          | None -> Ok ()
          | Some else_ -> validate_statement else_.else_branch))
  | Frontend.Ast.Lock_statement lock -> validate_statement lock.lock_body
  | Frontend.Ast.Sequence_statement sequence ->
      sequence.sequence_elements
      |> List.map (fun (element : Frontend.Ast.statement_sequence_element) ->
          element.sequence_statement)
      |> statements
  | Frontend.Ast.Switch_statement switch ->
      switch_elements switch.switch_elements
  | Frontend.Ast.Try_catch_statement try_catch -> (
      match validate_statement try_catch.try_body with
      | Error _ as error -> error
      | Ok () -> validate_statement try_catch.catch_body)
  | Frontend.Ast.While_statement while_ -> validate_statement while_.while_body
  | Frontend.Ast.Assembly_block_statement _
  | Frontend.Ast.Inline_assembly_statement _
  | Frontend.Ast.Break_statement _
  | Frontend.Ast.Empty_statement _
  | Frontend.Ast.Expression_statement _
  | Frontend.Ast.Goto_statement _
  | Frontend.Ast.Implicit_output_statement _
  | Frontend.Ast.Label_statement _
  | Frontend.Ast.Local_declaration_statement _
  | Frontend.Ast.No_warn_statement _ -> Ok ()

let validate (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.filter_map (function
    | Frontend.Ast.Top_level_statement statement -> Some statement
    | Frontend.Ast.Aggregate_forward_declaration _
    | Frontend.Ast.Aggregate_definition _
    | Frontend.Ast.Global_variable _
    | Frontend.Ast.Global_declaration _
    | Frontend.Ast.Function_prototype _
    | Frontend.Ast.Function_definition _ -> None)
  |> statements
