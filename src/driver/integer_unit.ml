module Ast = Frontend.Ast
module Typed = Sema.Function_call_expression_result
module Lower = Ir.Integer_program_lowering
module Span_map = Map.Make (Common.Span)
module Source = Sema.Function_call_resolution
module Frame = Sema.Function_frame_layout
module Body = Ir.Function_body
module Records = Sema.Function_record_classification
module Resolution = Sema.Function_resolution

type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }

type compiled = {
  entry_ : Ir.X87_stack.t;
  globals_ : Ir.Integer_globals.t;
  initialization_ : Ir.Global_initialization.t;
  preparation_ : Integer_initializers.t;
  dimension_work_ : int;
  functions_ : Ir.Integer_interpreter.function_definition list;
  runtime_calls_ : Ir.Runtime_call_context.t;
  entry_has_calls_ : bool;
}

let entry compiled = compiled.entry_
let globals compiled = compiled.globals_
let initialization compiled = compiled.initialization_
let initializer_preparation compiled = compiled.preparation_
let dimension_preparation_work compiled = compiled.dimension_work_
let functions compiled = compiled.functions_
let runtime_calls compiled = compiled.runtime_calls_
let has_entry_calls compiled = compiled.entry_has_calls_

let human compiled =
  let entry =
    (compiled.entry_ |> Ir.X87_stack.graph |> Ir.Block_graph.human)
    ^ Ir.Integer_globals.human compiled.globals_
    ^ Ir.Global_initialization.human compiled.initialization_
    ^ Integer_initializers.human compiled.preparation_
  in
  match compiled.functions_ with
  | [] -> entry
  | functions ->
      entry
      ^ String.concat ""
          (List.map
             (fun (function_ : Ir.Integer_interpreter.function_definition) ->
               let body = function_.body in
               let definition = Body.symbol body in
               let callable = Body.callable_symbol body in
               let binding =
                 if definition == callable then ""
                 else
                   Printf.sprintf
                     "holyc-ir-function-binding-v1 reference=%s\n\
                      function=^f%d definition=@s%d callable=@s%d item=%d\n"
                     Body.reference_commit
                     (Body.function_id body |> Body.Function_id.to_int)
                     (Sema.Symbol.id definition |> Sema.Symbol.Id.to_int)
                     (Sema.Symbol.id callable |> Sema.Symbol.Id.to_int)
                     (Frame.function_item_index function_.frame)
               in
               binding ^ Body.human body)
             functions)

let ( let* ) = Result.bind

exception Invalid of Common.Diagnostic.t

let fail span code message =
  raise (Invalid (Integer_source.diagnostic ~span code message))

let compile_parsed_with_limit ?task_view ?initializer_progress
    ?declaration_command ?source_command ?retained_function_source
    ~max_initializer_steps session ~config (parsed : Frontend.Parser.output) =
  match parsed.ast with
  | None -> Error parsed.diagnostics
  | Some ast -> (
      let lowered =
        try
          let rec validate ~in_function = function
            | Ast.Empty_statement _
            | Ast.Expression_statement _
            | Ast.Implicit_output_statement _
            | Ast.Break_statement _ -> ()
            | Ast.Block_statement block ->
                List.iter (validate ~in_function) block.block_statements
            | Ast.Sequence_statement sequence ->
                List.iter
                  (fun element ->
                    validate ~in_function element.Ast.sequence_statement)
                  sequence.sequence_elements
            | Ast.If_statement branch ->
                validate ~in_function branch.if_then_branch;
                Option.iter
                  (fun clause -> validate ~in_function clause.Ast.else_branch)
                  branch.if_else_clause
            | Ast.While_statement loop -> validate ~in_function loop.while_body
            | Ast.Do_while_statement loop -> validate ~in_function loop.do_body
            | Ast.For_statement loop ->
                validate ~in_function loop.for_initializer;
                Option.iter (validate ~in_function) loop.for_update;
                validate ~in_function loop.for_body
            | (Ast.Local_declaration_statement _ | Ast.Return_statement _)
              when in_function -> ()
            | other ->
                fail (Ast.statement_location other).span "HCRUN0001"
                  "statement is outside the integer program execution domain"
          in
          let statements =
            List.filter_map
              (function
                | Ast.Top_level_statement statement ->
                    validate ~in_function:false statement;
                    Some statement
                | Ast.Global_variable _
                | Ast.Global_declaration _
                | Ast.Function_prototype _ -> None
                | Ast.Function_definition definition ->
                    (match definition.body with
                    | Some body -> validate ~in_function:true body
                    | None ->
                        fail definition.location.span "HCRUN0001"
                          "integer function definition has no source body");
                    None
                | _ ->
                    fail ast.span "HCRUN0001"
                      "declaration is outside integer program execution")
              ast.items
          in
          let* selections =
            match (declaration_command, task_view) with
            | None, _ -> Ok None
            | Some command, Some task_view ->
                Task_declarations.reference_resolver
                  ~table:(Session.semantic_symbols session)
                  ~ast ~task_view command
                |> Result.map Option.some
            | Some _, None ->
                Error
                  [
                    Integer_source.diagnostic ~span:ast.span "HCRUN0004"
                      "parser-selected compilation requires its task snapshot";
                  ]
          in
          let* prepared =
            Integer_source.prepare_unit ?declaration_command ?source_command
              ?selections
              ?environment:
                (Option.map Ir.Integer_globals.task_environment task_view)
              ~include_global_initializers:true session ~config ~span:ast.span
              ast
          in
          let typed = Integer_source.top_level prepared in
          let* globals_ =
            Ir.Integer_globals.create_with_layout
              ~layout:(Integer_source.global_layouts prepared)
              ~initializers:typed ~span:ast.span
              (Integer_source.global_records prepared)
          in
          let* globals_ =
            Ir.Integer_globals.with_statics ~span:ast.span
              ~frames:(Integer_source.frames prepared)
              ~functions:(Integer_source.functions prepared)
              ~records:(Integer_source.records prepared)
              globals_
          in
          let globals_ =
            Option.fold ~none:globals_
              ~some:(fun view ->
                Ir.Integer_globals.with_task_view view globals_)
              task_view
          in
          let* globals_ =
            if Option.is_none task_view then Ok globals_
            else
              Ir.Integer_globals.with_function_publications
                ~records:(Integer_source.records prepared)
                globals_
              |> Result.map_error (fun message ->
                  [
                    Integer_source.diagnostic ~span:ast.span "HCRUN0004" message;
                  ])
          in
          let root_map values =
            List.fold_left
              (fun roots value ->
                match Typed.result_origin value with
                | Sema.Symbol.Source_location location ->
                    if Span_map.mem location.span roots then
                      fail location.span "HCRUN0004"
                        "typed source roots have duplicate locations";
                    Span_map.add location.span value roots
                | _ ->
                    fail ast.span "HCRUN0004"
                      "typed source root has no physical source location")
              Span_map.empty values
          in
          let source_span = function
            | Sema.Symbol.Source_location location -> location.span
            | _ ->
                fail ast.span "HCRUN0004"
                  "function source evidence has no physical location"
          in
          let source_map origin values =
            List.fold_left
              (fun map value ->
                let span = source_span (origin value) in
                if Span_map.mem span map then
                  fail span "HCRUN0004" "duplicate function source evidence";
                Span_map.add span value map)
              Span_map.empty values
          in
          let lower_statements roots initializers returns outputs statements =
            let initializers =
              ref
                (List.fold_left
                   (fun map initial ->
                     let span =
                       initial |> Typed.initializer_source
                       |> Source.initializer_local
                       |> Sema.Local_type_resolution.local_declarator_origin
                       |> source_span
                     in
                     Span_map.update span
                       (fun previous ->
                         Some (initial :: Option.value previous ~default:[]))
                       map)
                   Span_map.empty initializers
                |> Span_map.map List.rev)
            in
            let returns =
              ref
                (source_map
                   (fun returned ->
                     returned |> Typed.return_source |> Source.return_origin)
                   returns)
            in
            let outputs =
              ref
                (List.fold_left
                   (fun map (marker, statement, values) ->
                     if Span_map.mem marker map then
                       fail marker "HCRUN0004"
                         "duplicate implicit output marker identity";
                     Span_map.add marker (statement, values) map)
                   Span_map.empty outputs)
            in
            let consume map span detail =
              match Span_map.find_opt span !map with
              | Some value ->
                  map := Span_map.remove span !map;
                  value
              | None -> fail span "HCRUN0003" detail
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
              | Ast.Implicit_output_statement item ->
                  let lowered, expected =
                    consume outputs item.marker.literal_location.span
                      "implicit output has no exact checked binding"
                  in
                  let fixed =
                    match item.fixed_argument with
                    | Ast.Marker_fixed_argument value
                    | Ast.Expression_fixed_argument value -> value
                  in
                  let actual =
                    List.map expression
                      (fixed
                      :: List.map
                           (fun (argument : Ast.implicit_output_argument) ->
                             argument.value)
                           item.arguments)
                  in
                  if
                    List.length actual <> List.length expected
                    || not (List.for_all2 ( == ) actual expected)
                  then
                    fail item.location.span "HCRUN0004"
                      "implicit output does not own its checked fixed and \
                       trailing roots";
                  lowered
              | Ast.Local_declaration_statement declaration ->
                  Lower.Block
                    (List.filter_map
                       (fun (declarator : Ast.local_declarator) ->
                         match declarator.local_initializer with
                         | None -> None
                         | Some ast_initial ->
                             let batch =
                               consume initializers
                                 declarator.local_declarator_location.span
                                 "initializer has no complete checked \
                                  declaration batch"
                             in
                             let initial =
                               match batch with
                               | first :: _ -> first
                               | [] ->
                                   fail
                                     declarator.local_declarator_location.span
                                     "HCRUN0004"
                                     "initializer declaration batch is empty"
                             in
                             let local =
                               initial |> Typed.initializer_source
                               |> Source.initializer_local
                             in
                             let exact_batch =
                               List.for_all
                                 (fun root ->
                                   root |> Typed.initializer_source
                                   |> Source.initializer_local
                                   |> fun owner -> owner == local)
                                 batch
                               &&
                               match
                                 Option.bind
                                   (Sema.Local_type_resolution.local_initializer
                                      local)
                                   Sema.Local_type_resolution.initializer_source
                               with
                               | None -> false
                               | Some manifest ->
                                   Sema.Initializer_source.matches_ast manifest
                                     ast_initial.local_initializer_value
                                   &&
                                   let leaves =
                                     Sema.Initializer_source.leaves manifest
                                   in
                                   List.length leaves = List.length batch
                                   && List.for_all2
                                        (fun leaf root ->
                                          Option.fold ~none:false
                                            ~some:(( == ) leaf)
                                            (root |> Typed.initializer_source
                                           |> Source.initializer_leaf))
                                        leaves batch
                             in
                             if not exact_batch then
                               fail declarator.local_declarator_location.span
                                 "HCRUN0004"
                                 "initializer roots do not cover their \
                                  original source declaration";
                             if
                               Sema.Local_type_resolution.local_storage local
                               = Sema.Local_type_resolution.Static
                             then
                               match
                                 Ir.Integer_globals.find_static globals_
                                   (Sema.Local_type_resolution.local_symbol
                                      local)
                               with
                               | Some slot
                                 when let owned =
                                        Ir.Integer_globals.static_initializers
                                          slot
                                      in
                                      List.length owned = List.length batch
                                      && List.for_all2 ( == ) owned batch ->
                                   None
                               | _ ->
                                   fail
                                     declarator.local_declarator_location.span
                                     "HCRUN0004"
                                     "static initializer has no exact \
                                      persistent owner"
                             else if
                               List.length batch = 1
                               &&
                               match ast_initial.local_initializer_value with
                               | Ast.Scalar_initializer _ -> true
                               | _ -> false
                             then Some (Lower.Initialize initial)
                             else
                               fail declarator.local_declarator_location.span
                                 "HCRUN0001"
                                 "automatic array declaration initializer \
                                  execution is unresolved")
                       declaration.local_declarators)
              | Ast.Return_statement returned ->
                  Lower.Return
                    (consume returns returned.return_location.span
                       "return has no matching checked function root")
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
            if
              not
                (Span_map.is_empty !initializers
                && Span_map.is_empty !returns && Span_map.is_empty !outputs)
            then
              fail ast.span "HCRUN0004"
                "function body did not consume every initializer, return and \
                 output root";
            statements
          in
          let checked_result show =
            Result.map_error (fun error ->
                [
                  Integer_source.diagnostic ~span:ast.span "HCRUN0004"
                    (show error);
                ])
          in
          let rec map_checked apply = function
            | [] -> Ok []
            | value :: rest ->
                let* value = apply value in
                let* rest = map_checked apply rest in
                Ok (value :: rest)
          in
          let records = Integer_source.records prepared in
          let* top_calls =
            Typed.top_level_direct_calls typed
            |> map_checked (fun call ->
                Sema.Top_level_function_call_target_classification.classify
                  ~records call
                |> checked_result
                     Sema.Top_level_function_call_target_classification
                     .error_to_string)
          in
          let all_function_calls = ref [] in
          let function_contexts = ref [] in
          let* definitions =
            ast.items
            |> List.mapi (fun index item -> (index, item))
            |> List.filter_map (function
              | index, Ast.Function_definition definition ->
                  Some (index, definition)
              | _ -> None)
            |> map_checked
                 (fun (item_index, (definition : Ast.function_definition)) ->
                   let function_ =
                     Integer_source.functions prepared
                     |> Typed.functions
                     |> List.find_opt (fun function_ ->
                         Typed.function_item_index function_ = item_index)
                     |> function
                     | Some value -> value
                     | None ->
                         fail definition.location.span "HCRUN0004"
                           "source function has no typed body"
                   in
                   let frame =
                     Frame.find_function
                       (Integer_source.frames prepared)
                       (Typed.function_symbol function_)
                     |> function
                     | Some value -> value
                     | None ->
                         fail definition.location.span "HCRUN0004"
                           "source function has no checked frame"
                   in
                   let record =
                     Records.declarations records
                     |> List.find_opt (fun record ->
                         record |> Records.classified_declaration_source
                         |> Resolution.resolved_declaration_site
                         |> Resolution.declaration_site_function
                         |> Sema.Function_type_resolution.function_symbol
                         |> fun symbol -> symbol == Frame.function_symbol frame)
                     |> function
                     | Some value -> value
                     | None ->
                         fail definition.location.span "HCRUN0004"
                           "source function has no checked declaration record"
                   in
                   let declaration =
                     Records.classified_declaration_source record
                   in
                   let typed_header =
                     declaration |> Resolution.resolved_declaration_site
                     |> Resolution.declaration_site_function
                   in
                   let return_type =
                     typed_header
                     |> Sema.Function_type_resolution.function_return_type
                     |> Sema.Type_reference.resolved_type
                   in
                   let* function_calls =
                     Typed.function_calls function_
                     |> List.filter_map (function
                       | Typed.Direct_call_result call -> Some call
                       | _ -> None)
                     |> map_checked (fun call ->
                         Sema.Function_call_target_classification.classify
                           ~records call
                         |> checked_result
                              Sema.Function_call_target_classification
                              .error_to_string)
                   in
                   all_function_calls :=
                     List.rev_append function_calls !all_function_calls;
                   let outputs =
                     let module Bound = Sema.Implicit_output_argument_binding in
                     let module Target = Sema.Implicit_output_target_resolution
                     in
                     let bound_function =
                       Bound.find_function
                         (Integer_source.function_outputs prepared)
                         (Typed.function_symbol function_)
                     in
                     match bound_function with
                     | None ->
                         fail definition.location.span "HCRUN0004"
                           "source function has no checked output batch"
                     | Some bound_function ->
                         Bound.function_outputs bound_function
                         |> List.map (fun result ->
                             let target = Bound.output_source result in
                             let typed = Target.output_source target in
                             let marker =
                               typed |> Typed.implicit_output_source
                               |> Source.implicit_output_marker_origin
                               |> source_span
                             in
                             match result with
                             | Bound.Deferred_outer_output _ ->
                                 fail marker "HCRUN0003"
                                   "implicit output target requires an exact \
                                    checked runtime header"
                             | Bound.Bound_output output ->
                                 let values =
                                   Typed.implicit_output_fixed_value typed
                                   :: List.map
                                        Typed.implicit_output_argument_value
                                        (Typed.implicit_output_arguments typed)
                                 in
                                 (marker, Lower.Function_output output, values))
                   in
                   let roots =
                     root_map
                       (List.map Typed.expression_statement_value
                          (Typed.function_expression_statements function_)
                       @ List.map Typed.condition_value
                           (Typed.function_conditions function_)
                       @ List.concat_map (fun (_, _, values) -> values) outputs
                       )
                   in
                   let statements =
                     lower_statements roots
                       (Typed.function_initializers function_)
                       (Typed.function_returns function_)
                       outputs
                       (Option.to_list definition.body)
                   in
                   let* lowered =
                     Lower.lower_complete ~frame ~globals:globals_ ~records
                       ~function_calls ~span:definition.location.span statements
                   in
                   let graph = Lower.graph lowered in
                   let members kind =
                     Frame.function_locations frame
                     |> List.filter (fun location ->
                         Frame.location_kind location = kind)
                     |> List.mapi (fun position location ->
                         Body.
                           {
                             position;
                             symbol = Frame.location_symbol location;
                             type_ = Frame.location_checked_type location;
                             span =
                               Some
                                 (source_span
                                    (Sema.Symbol.origin
                                       (Frame.location_symbol location)));
                           })
                   in
                   let* function_id =
                     Body.Function_id.of_int item_index
                     |> checked_result (fun (error : Body.error) ->
                         error.message)
                   in
                   let* body =
                     Body.create
                       {
                         function_id;
                         symbol = Frame.function_symbol frame;
                         function_scope =
                           Sema.Symbol_table.scope_id
                             (Frame.function_scope frame);
                         return_type;
                         parameters = members Frame.Named_parameter;
                         locals = members Frame.Automatic_local;
                         stored_flags =
                           Records.classified_declaration_record record
                           |> Records.stored_flag_mask;
                         compiler_options =
                           Records.classified_declaration_state record
                           |> Records.declaration_state_compiler_option_mask;
                         span = Some definition.location.span;
                         body = Ir.X87_stack.graph graph;
                       }
                     |> (fun result ->
                     Result.bind result
                       (Body.with_definition ~records
                          ~sources:(Integer_source.functions prepared)
                          ~frames:(Integer_source.frames prepared)
                          ~definition:record ~frame))
                     |> Result.map_error
                          (List.map (fun (error : Body.error) ->
                               Integer_source.diagnostic
                                 ~span:
                                   (Option.value error.span
                                      ~default:definition.location.span)
                                 error.code error.message))
                   in
                   function_contexts :=
                     (body, Lower.runtime_calls lowered) :: !function_contexts;
                   Ok Ir.Integer_interpreter.{ frame; body })
          in
          let* preparation_ =
            Integer_initializers.prepare ~max_steps:max_initializer_steps
              ?retained_function_source
              ~allow_zero_budget:(Option.is_some task_view)
              ?on_progress:initializer_progress
              ~function_calls:(List.rev !all_function_calls)
              ~span:ast.span ~globals:globals_ ~top_calls ~functions:definitions
              ()
          in
          let globals_ = Integer_initializers.globals preparation_ in
          let roots =
            Typed.top_level_statements typed
            |> List.concat_map Typed.top_level_statement_roots
            |> List.filter (fun root ->
                match
                  root |> Typed.top_level_root_source
                  |> Sema.Top_level_expression_tree.root_role
                with
                | Sema.Top_level_expression_tree.Global_initializer _ -> false
                | _ -> true)
            |> List.map Typed.top_level_root_value
            |> root_map
          in
          let outputs =
            let module Bound = Sema.Top_level_implicit_output_argument_binding
            in
            let module Target = Sema.Top_level_implicit_output_target_resolution
            in
            Integer_source.top_level_outputs prepared
            |> Bound.outputs
            |> List.map (fun result ->
                let target = Bound.output_source result in
                let marker =
                  Target.output_marker_origin target |> source_span
                in
                match result with
                | Bound.Deferred_outer_output _ ->
                    fail marker "HCRUN0003"
                      "implicit output target requires an exact checked \
                       runtime header"
                | Bound.Bound_output output ->
                    let values =
                      Target.output_fixed_value target
                      :: Target.output_arguments target
                      |> List.map Typed.top_level_root_value
                    in
                    (marker, Lower.Top_level_output output, values))
          in
          let ordinary =
            ref (lower_statements roots [] [] outputs statements)
          in
          let pending =
            Ir.Integer_globals.slots globals_
            |> List.concat_map (fun slot ->
                Ir.Integer_globals.slot_initializers slot
                |> List.filter_map (fun root ->
                    if not (Ir.Integer_globals.slot_root_materialized slot root)
                    then Some (slot, Lower.Initialize_global root)
                    else if
                      Option.is_some
                        (Ir.Integer_globals.slot_array_initializers slot)
                      && Ir.Integer_globals.slot_opcode slot
                         = Ir.Opcode.Ic_imm_i64
                    then
                      Some
                        ( slot,
                          Lower.Publish_array
                            (Ir.Global_initialization.Prepared_global root) )
                    else None))
          in
          let pending_statics =
            Ir.Integer_globals.statics globals_
            |> List.concat_map (fun slot ->
                Ir.Integer_globals.static_initializers slot
                |> List.filter_map (fun root ->
                    if
                      not
                        (Ir.Integer_globals.static_root_materialized slot root)
                    then Some (slot, Lower.Initialize_static_leaf (slot, root))
                    else if
                      Option.is_some
                        (Ir.Integer_globals.static_array_initializers slot)
                      && Ir.Integer_globals.storage_opcode
                           (Ir.Integer_globals.static_storage slot)
                         = Ir.Opcode.Ic_imm_i64
                    then
                      Some
                        ( slot,
                          Lower.Publish_array
                            (Ir.Global_initialization.Prepared_static
                               (slot, root)) )
                    else None))
          in
          let statements =
            ast.items
            |> List.mapi (fun item_index item ->
                match item with
                | Ast.Top_level_statement _ -> (
                    match !ordinary with
                    | statement :: rest ->
                        ordinary := rest;
                        [ statement ]
                    | [] ->
                        fail ast.span "HCRUN0004"
                          "module statement composition lost a source root")
                | _ ->
                    (pending
                    |> List.filter_map (fun (slot, statement) ->
                        let global =
                          Ir.Integer_globals.slot_record slot
                          |> Sema.Global_record_classification
                             .classified_record_source
                          |> Sema.Global_resolution.global_record_global
                        in
                        if
                          Sema.Global_type_resolution.global_item_index global
                          = item_index
                        then Some statement
                        else None))
                    @ (pending_statics
                      |> List.filter_map (fun (slot, statement) ->
                          if
                            Frame.function_item_index
                              (Ir.Integer_globals.static_frame slot)
                            = item_index
                          then Some statement
                          else None)))
            |> List.concat
          in
          let* lowered_entry =
            Lower.lower_complete ~globals:globals_ ~records ~top_calls
              ~function_calls:(List.rev !all_function_calls)
              ~span:ast.span statements
          in
          let entry_ = Lower.graph lowered_entry in
          let regions = Lower.initializer_regions lowered_entry in
          let static_descriptions =
            Lower.static_initializer_regions lowered_entry
          in
          let* initialization_ =
            Ir.Global_initialization.create ~static_descriptions
              ~publications:(Lower.publications lowered_entry)
              ~span:ast.span
              ?publication_evidence:(Lower.publication_evidence lowered_entry)
              ~globals:globals_ ~entry:entry_ regions
          in
          let entry_calls = Lower.runtime_calls lowered_entry in
          let* runtime_calls_ =
            Ir.Runtime_call_context.create ~records
              ~function_sources:(Integer_source.functions prepared)
              ~top_level:typed ~initialization:initialization_ ~entry:entry_
              ~entry_calls
              ~functions:(List.rev !function_contexts)
          in
          Ok
            {
              entry_;
              globals_;
              initialization_;
              preparation_;
              dimension_work_ =
                (match (declaration_command, source_command) with
                | Some command, None ->
                    Task_declarations.command_dimension_work command
                | None, Some command ->
                    Task_declarations.source_dimension_work command
                | _ -> 0);
              functions_ = definitions;
              runtime_calls_;
              entry_has_calls_ = entry_calls <> [];
            }
        with Invalid diagnostic -> Error [ diagnostic ]
      in
      match lowered with
      | Ok value -> Ok { value; diagnostics = parsed.diagnostics }
      | Error diagnostics -> Error (parsed.diagnostics @ diagnostics))

let compile_ast_internal ?task_view ?initializer_progress ?declaration_command
    ?retained_function_source ?(max_initializer_steps = 100_000) session ~config
    ast =
  if
    max_initializer_steps < 0
    || (max_initializer_steps = 0 && Option.is_none task_view)
  then
    Error
      [
        Integer_source.diagnostic ~span:ast.Ast.span "HCIRVM0001"
          "max_initializer_steps must be greater than zero";
      ]
  else
    compile_parsed_with_limit ?task_view ?initializer_progress
      ?declaration_command ?retained_function_source ~max_initializer_steps
      session ~config
      { Frontend.Parser.ast = Some ast; diagnostics = [] }

let compile_ast ?max_initializer_steps session ~config ast =
  compile_ast_internal ?max_initializer_steps session ~config ast

let compile_source_output ~source_command ~max_initializer_steps session ~config
    parsed =
  compile_parsed_with_limit ~source_command ~max_initializer_steps session
    ~config parsed

let compile_task_ast ~task ?declaration_command session ~config
    (ast : Ast.module_) =
  let module VM = Ir.Integer_interpreter in
  if
    (not (VM.task_owns_table task (Session.semantic_symbols session)))
    || Frontend.Preprocessor.Config.compilation_mode config
       <> Frontend.Preprocessor.Jit
  then
    Error
      [
        Integer_source.diagnostic ~span:ast.span "HCRUN0004"
          "task compilation requires its owning semantic table and JIT mode";
      ]
  else if
    Option.fold ~none:false
      ~some:(fun command -> not (Task_declarations.owns_runtime task command))
      declaration_command
  then
    Error
      [
        Integer_source.diagnostic ~span:ast.span "HCRUN0004"
          "parser command belongs to another runtime or a semantic-only ledger";
      ]
  else if
    Option.is_none declaration_command
    && Sema.Task_command_order.has_source_syntax (VM.task_source_order task) ast
  then
    Error
      [
        Integer_source.diagnostic ~span:ast.span "HCRUN0004"
          "source-owned task syntax requires its original declaration receipt";
      ]
  else
    let* task_view =
      VM.task_snapshot task
      |> Result.map_error (fun message ->
          [ Integer_source.diagnostic ~span:ast.span "HCRUN0004" message ])
    in
    let before = VM.task_initializer_steps task in
    let max_initializer_steps = VM.task_initializer_limit task - before in
    let* task_view =
      match declaration_command with
      | None -> Ok task_view
      | Some command ->
          let* order =
            Task_declarations.command_order ~runtime:task
              ~table:(Session.semantic_symbols session)
              ~ast command
          in
          Ir.Integer_globals.with_source_command task_view ~ast order
          |> Result.map_error (fun message ->
              [ Integer_source.diagnostic ~span:ast.span "HCRUN0004" message ])
    in
    let* compiled =
      compile_ast_internal ~task_view ~max_initializer_steps
        ?declaration_command
        ~retained_function_source:(VM.task_function_source task)
        ~initializer_progress:(fun steps ->
          VM.record_task_preparation task ~before ~steps)
        session ~config ast
    in
    let program = compiled.value in
    let* () =
      match declaration_command with
      | None -> Ok ()
      | Some _ ->
          VM.bind_task_source_program task ~runtime_calls:program.runtime_calls_
            ~globals:program.globals_ ~initialization:program.initialization_
            ~functions:program.functions_ program.entry_
          |> Result.map_error (fun message ->
              [ Integer_source.diagnostic ~span:ast.span "HCRUN0004" message ])
    in
    Ok compiled
