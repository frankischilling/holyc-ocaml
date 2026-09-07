module Ast = Frontend.Ast
module Diagnostic = Common.Diagnostic
module Sequence = Ir.Instruction_sequence
module Expression = Ir.Expression_lowering
module Typed = Sema.Function_call_expression_result

let ( let* ) = Result.bind

let diagnostic ~span code message =
  Diagnostic.make ~code ~severity:Diagnostic.Error ~message ~primary:span ()

let source_span source =
  Common.Span.unsafe_make
    ~source:(Common.Source_file.id source)
    ~start:0
    ~stop:(Common.Source_file.length source)

let message_diagnostic ~span message =
  match String.index_opt message ':' with
  | Some separator when String.starts_with ~prefix:"HC" message ->
      diagnostic ~span
        (String.sub message 0 separator)
        (String.trim
           (String.sub message (separator + 1)
              (String.length message - separator - 1)))
  | _ -> diagnostic ~span "HCEVAL0003" message

let prepare session ~config ~span ast =
  let table = Session.semantic_symbols session in
  let mode = Frontend.Preprocessor.Config.compilation_mode config in
  let checked result =
    Result.map_error
      (fun message -> [ message_diagnostic ~span message ])
      result
  in
  let* declarations =
    Semantic_collection.collect ~sources:(Session.sources session) ~table ast
    |> checked
  in
  let* aggregates =
    Aggregate_resolution.resolve ~table ~declarations ast |> checked
  in
  let* headers =
    Aggregate_header_resolution.resolve ~table ~declarations ~aggregates ast
    |> checked
  in
  let* collected_members =
    Member_collection.collect ~table ~declarations ast |> checked
  in
  let* members =
    Member_type_resolution.resolve ~table ~declarations ~aggregates ~headers
      ~members:collected_members ast
    |> checked
  in
  let* layouts =
    Aggregate_layout.layout ~table ~declarations ~aggregates ~headers ~members
      ast
    |> checked
  in
  let* members =
    Aggregate_member_index.build ~table ~declarations ~headers ~members ~layouts
    |> checked
  in
  let* collected_functions =
    Function_collection.collect ~table ~declarations ast |> checked
  in
  let* function_types =
    Function_type_resolution.resolve ~table ~declarations ~aggregates
      ~functions:collected_functions ast
    |> checked
  in
  let* local_types =
    Local_type_resolution.resolve ~table ~declarations ~aggregates
      ~functions:collected_functions ast
    |> checked
  in
  let* bindings =
    Function_binding_index.build ~table ~declarations
      ~functions:collected_functions ~function_types ~local_types
    |> checked
  in
  let* expressions =
    Function_expression_binding.resolve ~table ~declarations
      ~functions:collected_functions ~local_types ~bindings ast
    |> checked
  in
  let* global_types =
    Global_type_resolution.resolve ~table ~declarations ~aggregates ast
    |> checked
  in
  let* functions =
    Function_resolution.resolve ~table ~declarations ~functions:function_types
      ~compilation_mode:mode ast
    |> checked
  in
  let* globals =
    Global_resolution.resolve ~table ~declarations ~globals:global_types
      ~compilation_mode:mode ast
    |> checked
  in
  let* module_expressions =
    Module_expression_binding.resolve ~table ~declarations ~aggregates
      ~functions ~globals ~expressions
    |> checked
  in
  let* calls =
    Function_call_resolution.resolve ~table ~declarations ~function_types
      ~members ~local_types ~global_types ~functions
      ~expressions:module_expressions ast
    |> checked
  in
  let* policies =
    Sema.Function_call_conversion_policy.analyze ~table
      ~parent:(Sema.Declaration_collection.scope declarations)
      ~headers ~calls
    |> Result.map_error Sema.Function_call_conversion_policy.error_to_string
    |> checked
  in
  let make_table table_kind table_index =
    Sema.Outer_environment.make_table ~table_kind ~table_index []
    |> Result.map_error Sema.Outer_environment.error_to_string
    |> checked
  in
  let* tables =
    match mode with
    | Frontend.Preprocessor.Jit ->
        let* task = make_table (Sema.Outer_environment.Jit_task 0) 0 in
        let* assembler = make_table Sema.Outer_environment.Assembler 1 in
        Ok [ task; assembler ]
    | Frontend.Preprocessor.Aot ->
        let* assembler = make_table Sema.Outer_environment.Assembler 0 in
        Ok [ assembler ]
  in
  let* environment =
    Outer_expression_binding.create_environment ~table ~compilation_mode:mode
      tables
    |> checked
  in
  let* expressions =
    Top_level_expression_binding.resolve ~table ~declarations
      ~module_expressions ast
    |> checked
  in
  let* expressions =
    Sema.Top_level_outer_expression_binding.resolve ~table ~environment
      ~expressions
    |> Result.map_error Sema.Top_level_outer_expression_binding.error_to_string
    |> checked
  in
  let compilation_mode =
    match mode with
    | Frontend.Preprocessor.Jit -> Sema.Outer_environment.Jit
    | Frontend.Preprocessor.Aot -> Sema.Outer_environment.Aot
  in
  let* expressions =
    Top_level_expression_tree.build ~table ~declarations ~compilation_mode
      ~expressions ast
    |> checked
  in
  let* identifiers =
    Top_level_identifier_resolution.classify ~table ~globals:global_types
      ~functions ~expressions
    |> checked
  in
  let* typed =
    Typed.analyze_top_level ~table ~members ~policies ~identifiers expressions
    |> Result.map_error Typed.error_to_string
    |> checked
  in
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
