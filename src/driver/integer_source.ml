module Diagnostic = Common.Diagnostic
module Typed = Sema.Function_call_expression_result

type prepared = {
  top_level_ : Typed.top_level_t;
  functions_ : Typed.t;
  frames_ : Sema.Function_frame_layout.t;
  records_ : Sema.Function_record_classification.t;
}

let top_level prepared = prepared.top_level_
let functions prepared = prepared.functions_
let frames prepared = prepared.frames_
let records prepared = prepared.records_
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

let prepare_unit session ~config ~span ast =
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
  let* function_results =
    Typed.analyze ~table ~members policies
    |> Result.map_error Typed.error_to_string
    |> checked
  in
  let* frames =
    Function_frame_layout.layout ~table ~declarations ~bindings ~function_types
      ~local_types ~aggregate_layouts:layouts ast
    |> checked
  in
  let* records =
    Function_record_classification.classify ~resolution:functions ast |> checked
  in
  Ok
    {
      top_level_ = typed;
      functions_ = function_results;
      frames_ = frames;
      records_ = records;
    }

let prepare session ~config ~span ast =
  prepare_unit session ~config ~span ast |> Result.map top_level
