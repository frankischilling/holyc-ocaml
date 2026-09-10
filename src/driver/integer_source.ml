module Diagnostic = Common.Diagnostic
module Typed = Sema.Function_call_expression_result

type prepared = {
  top_level_ : Typed.top_level_t;
  functions_ : Typed.t;
  frames_ : Sema.Function_frame_layout.t;
  records_ : Sema.Function_record_classification.t;
  global_records_ : Sema.Global_record_classification.t;
  global_layouts_ : Sema.Global_array_layout.t;
  initializers_ : Sema.Global_initializer_binding.t option;
  function_outputs_ : Sema.Implicit_output_argument_binding.t;
  top_level_outputs_ : Sema.Top_level_implicit_output_argument_binding.t;
}

let top_level prepared = prepared.top_level_
let functions prepared = prepared.functions_
let frames prepared = prepared.frames_
let records prepared = prepared.records_
let global_records prepared = prepared.global_records_
let global_layouts prepared = prepared.global_layouts_
let initializers prepared = prepared.initializers_
let function_outputs prepared = prepared.function_outputs_
let top_level_outputs prepared = prepared.top_level_outputs_
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

let prepare_unit ?environment:task_environment ?declaration_command
    ?source_command ?selections ?implicit_selections
    ?(include_global_initializers = false) session ~config ~span ast =
  let table = Session.semantic_symbols session in
  let* () =
    match (declaration_command, source_command, task_environment) with
    | Some _, Some _, _ | _, Some _, Some _ ->
        Error
          [
            diagnostic ~span "HCRUN0004"
              "ordinary source and task compilation authorities cannot be \
               combined";
          ]
    | _ -> Ok ()
  in
  let query_for =
    match (declaration_command, source_command) with
    | Some command, None ->
        Some (Task_declarations.query_for ~table ~ast command)
    | None, Some command ->
        Some (Task_declarations.source_query_for ~table ~ast command)
    | None, None -> None
    | Some _, Some _ -> assert false
  in
  let queries =
    Option.map
      (fun query_for expression ->
        match query_for expression with
        | Error diagnostics ->
            Error
              (String.concat "; "
                 (List.map
                    (fun diagnostic ->
                      diagnostic.Common.Diagnostic.code ^ ": "
                      ^ diagnostic.message)
                    diagnostics))
        | Ok query -> Ok (Task_declarations.query_selection query))
      query_for
  in
  let prepared =
    let resolve =
      match (declaration_command, source_command) with
      | Some command, None ->
          Some (Task_declarations.checked_dimension_for ~table ~ast command)
      | None, Some command ->
          Some
            (Task_declarations.source_checked_dimension_for ~table ~ast command)
      | None, None -> None
      | Some _, Some _ -> assert false
    in
    Option.map
      (fun resolve dimension ->
        resolve dimension
        |> Result.map_error (fun errors ->
            String.concat "; "
              (List.map
                 (fun error ->
                   error.Common.Diagnostic.code ^ ": " ^ error.message)
                 errors)))
      resolve
  in
  let mode = Frontend.Preprocessor.Config.compilation_mode config in
  let checked result =
    Result.map_error
      (fun message -> [ message_diagnostic ~span message ])
      result
  in
  let* declarations =
    match (declaration_command, source_command) with
    | Some command, None -> Task_declarations.collection ~table ~ast command
    | None, Some command ->
        Task_declarations.source_collection ~table ~ast command
    | None, None ->
        Semantic_collection.collect ~sources:(Session.sources session) ~table
          ast
        |> checked
    | Some _, Some _ -> assert false
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
  let* function_namespace, retained_headers =
    match declaration_command with
    | None -> Ok (None, [])
    | Some command ->
        Result.map
          (fun (namespace, headers) -> (Some namespace, headers))
          (Task_declarations.retained_function_headers ~table ~ast command)
  in
  let* collected_functions =
    Function_collection.collect
      ~retained_headers:
        (List.map (fun (_, collected, _) -> collected) retained_headers)
      ~table ~declarations ast
    |> checked
  in
  let* function_types =
    Function_type_resolution.resolve
      ~retained_headers:(List.map (fun (_, _, typed) -> typed) retained_headers)
      ~table ~declarations ~aggregates ~functions:collected_functions ast
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
      ~functions:collected_functions ~local_types ~bindings ?selections ?queries
      ast
    |> checked
  in
  let* global_types =
    let initializers =
      match (declaration_command, source_command) with
      | Some command, None ->
          Some (Task_declarations.initializer_for ~table ~ast command)
      | None, Some command ->
          Some (Task_declarations.source_initializer_for ~table ~ast command)
      | None, None -> None
      | Some _, Some _ -> assert false
    in
    let initializers =
      Option.map
        (fun resolve name initial ->
          Result.map_error
            (fun diagnostics ->
              String.concat "; "
                (List.map
                   (fun diagnostic ->
                     diagnostic.Common.Diagnostic.code ^ ": "
                     ^ diagnostic.message)
                   diagnostics))
            (resolve name initial))
        initializers
    in
    Global_type_resolution.resolve ?initializers ~table ~declarations
      ~aggregates ast
    |> checked
  in
  let* previous_function_records, function_record_heads =
    let module Outer = Sema.Outer_environment in
    match task_environment with
    | None -> Ok ([], [])
    | Some environment when mode = Frontend.Preprocessor.Jit -> (
        if
          (not (Outer.owns_table environment table))
          || Outer.compilation_mode environment <> Outer.Jit
        then
          checked
            (Error "function joins require their owning JIT task environment")
        else
          match Outer.tables environment with
          | current :: _ when Outer.table_kind current = Outer.Jit_task 0 ->
              let parent = Sema.Declaration_collection.scope declarations in
              let heads =
                Outer.table_entries current
                |> List.filter_map (fun entry ->
                    if
                      Sema.Symbol.Scope_id.equal
                        (Sema.Symbol.scope_id (Outer.entry_symbol entry))
                        (Sema.Symbol_table.scope_id parent)
                    then
                      Option.map Outer.function_classified_declaration
                        (Outer.entry_function_metadata entry)
                    else None)
              in
              let _, records =
                List.fold_left
                  (fun (names, records) entry ->
                    match Outer.entry_function_metadata entry with
                    | None -> (names, records)
                    | Some metadata ->
                        let symbol = Outer.entry_symbol entry in
                        let name = Sema.Symbol.name symbol in
                        if List.mem name names then (names, records)
                        else if
                          Sema.Symbol.Scope_id.equal
                            (Sema.Symbol.scope_id symbol)
                            (Sema.Symbol_table.scope_id parent)
                        then
                          ( name :: names,
                            Outer.function_classified_declaration metadata
                            :: records )
                        else (name :: names, records))
                  ([], [])
                  (List.rev (Outer.table_entries current))
              in
              Ok (records, heads)
          | _ -> checked (Error "function joins require the current task table")
        )
    | Some _ -> Ok ([], [])
  in
  let* functions =
    Function_resolution.resolve ?namespace:function_namespace
      ~record_heads:
        (List.map
           Sema.Function_record_classification.classified_declaration_source
           function_record_heads)
      ~previous:
        (List.map
           Sema.Function_record_classification.classified_declaration_source
           previous_function_records)
      ~table ~declarations ~functions:function_types ~compilation_mode:mode ast
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
  let* environment =
    match task_environment with
    | Some environment ->
        let expected_mode =
          match mode with
          | Frontend.Preprocessor.Jit -> Sema.Outer_environment.Jit
          | Frontend.Preprocessor.Aot -> Sema.Outer_environment.Aot
        in
        if
          Sema.Outer_environment.owns_table environment table
          && Sema.Outer_environment.compilation_mode environment = expected_mode
        then Ok environment
        else
          checked
            (Error
               "HCRUN0004: task environment has a foreign owner or compilation \
                mode")
    | None ->
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
        Outer_expression_binding.create_environment ~table
          ~compilation_mode:mode tables
        |> checked
  in
  let* outer =
    match task_environment with
    | None -> Ok None
    | Some _ ->
        Outer_expression_binding.resolve ~table ~environment
          ~expressions:module_expressions
        |> checked |> Result.map Option.some
  in
  let* calls =
    Function_call_resolution.resolve ~table ~declarations ~function_types
      ~members ~local_types ~global_types ~functions
      ~expressions:module_expressions ?outer ast
    |> checked
  in
  let* policies =
    Sema.Function_call_conversion_policy.analyze ~table
      ~parent:(Sema.Declaration_collection.scope declarations)
      ~headers ~calls
    |> Result.map_error Sema.Function_call_conversion_policy.error_to_string
    |> checked
  in
  let* dimension_bindings =
    Global_dimension_binding.resolve ~table ~environment
      ~expressions:module_expressions ~globals ?selections ?queries ?prepared
      ast
    |> checked
  in
  let* global_layouts_ =
    Global_array_layout.layout ~table ~bindings:dimension_bindings ast
    |> checked
  in
  let* expressions =
    if include_global_initializers then
      Global_initializer_binding.resolve ~table ~environment
        ~expressions:module_expressions ~globals ?selections ?queries ast
      |> checked |> Result.map Option.some
    else Ok None
  in
  let initializers_ = expressions in
  let* expressions =
    Top_level_expression_binding.resolve ~table ~declarations
      ~module_expressions ?initializers:initializers_ ?selections ?queries ast
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
    Typed.analyze ~table ~members ?outer policies
    |> Result.map_error Typed.error_to_string
    |> checked
  in
  let* function_output_targets =
    (match implicit_selections with
    | None -> Sema.Implicit_output_target_resolution.resolve
    | Some selections ->
        Sema.Implicit_output_target_resolution.resolve_selected ~selections)
      ~table ~environment ~module_expressions ~function_types ~functions
      ~expressions:function_results
    |> Result.map_error Sema.Implicit_output_target_resolution.error_to_string
    |> checked
  in
  let* function_outputs_ =
    Sema.Implicit_output_argument_binding.bind ~table ~policies
      function_output_targets
    |> Result.map_error Sema.Implicit_output_argument_binding.error_to_string
    |> checked
  in
  let* top_level_output_targets =
    Sema.Top_level_implicit_output_target_resolution.resolve ~table
      ?selections:implicit_selections ~function_types ~functions typed
    |> Result.map_error
         Sema.Top_level_implicit_output_target_resolution.error_to_string
    |> checked
  in
  let* top_level_outputs_ =
    Sema.Top_level_implicit_output_argument_binding.bind ~table ~policies
      top_level_output_targets
    |> Result.map_error
         Sema.Top_level_implicit_output_argument_binding.error_to_string
    |> checked
  in
  let* frames =
    Function_frame_layout.layout ~table ~declarations ~bindings ~function_types
      ~local_types ~aggregate_layouts:layouts ?prepared ast
    |> checked
  in
  let* records =
    Function_record_classification.classify ~previous:function_record_heads
      ~resolution:functions ast
    |> checked
  in
  let* global_records =
    Global_record_classification.classify ~resolution:globals ast |> checked
  in
  Ok
    {
      global_records_ = global_records;
      global_layouts_;
      initializers_;
      top_level_ = typed;
      functions_ = function_results;
      frames_ = frames;
      records_ = records;
      function_outputs_;
      top_level_outputs_;
    }

let prepare session ~config ~span ast =
  prepare_unit session ~config ~span ast |> Result.map top_level
