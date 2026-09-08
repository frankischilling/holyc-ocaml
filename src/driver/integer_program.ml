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
  functions_ : Ir.Integer_interpreter.function_definition list;
}

let entry compiled = compiled.entry_
let globals compiled = compiled.globals_
let initialization compiled = compiled.initialization_
let initializer_preparation compiled = compiled.preparation_
let functions compiled = compiled.functions_

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
               Body.human function_.body)
             functions)

let ( let* ) = Result.bind

exception Invalid of Common.Diagnostic.t

let fail span code message =
  raise (Invalid (Integer_source.diagnostic ~span code message))

let compile_with_limit ~max_initializer_steps session ~config ~source =
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
          let rec validate ~in_function = function
            | Ast.Empty_statement _
            | Ast.Expression_statement _
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
                | Ast.Global_variable _ | Ast.Global_declaration _ -> None
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
          let* prepared =
            Integer_source.prepare_unit ~include_global_initializers:true
              session ~config ~span:ast.span ast
          in
          let typed = Integer_source.top_level prepared in
          let* globals_ =
            Ir.Integer_globals.create ~initializers:typed ~span:ast.span
              (Integer_source.global_records prepared)
          in
          let* globals_ =
            Ir.Integer_globals.with_statics ~span:ast.span
              ~frames:(Integer_source.frames prepared)
              ~functions:(Integer_source.functions prepared)
              ~records:(Integer_source.records prepared)
              globals_
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
          let lower_statements roots initializers returns statements =
            let initializers =
              ref
                (source_map
                   (fun initial ->
                     initial |> Typed.initializer_source
                     |> Source.initializer_local
                     |> Sema.Local_type_resolution.local_declarator_origin)
                   initializers)
            in
            let returns =
              ref
                (source_map
                   (fun returned ->
                     returned |> Typed.return_source |> Source.return_origin)
                   returns)
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
              | Ast.Local_declaration_statement declaration ->
                  Lower.Block
                    (List.filter_map
                       (fun (declarator : Ast.local_declarator) ->
                         match declarator.local_initializer with
                         | None -> None
                         | Some _ ->
                             let initial =
                               consume initializers
                                 declarator.local_declarator_location.span
                                 "initializer has no supported checked scalar \
                                  root"
                             in
                             let local =
                               initial |> Typed.initializer_source
                               |> Source.initializer_local
                             in
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
                                 when Option.fold ~none:false
                                        ~some:(fun root -> root == initial)
                                        (Ir.Integer_globals.static_initializer
                                           slot) -> None
                               | _ ->
                                   fail
                                     declarator.local_declarator_location.span
                                     "HCRUN0004"
                                     "static initializer has no exact \
                                      persistent owner"
                             else Some (Lower.Initialize initial))
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
              not (Span_map.is_empty !initializers && Span_map.is_empty !returns)
            then
              fail ast.span "HCRUN0004"
                "function body did not consume every initializer and return \
                 root";
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
                   if
                     Resolution.resolved_declaration_identity_symbol declaration
                     != Frame.function_symbol frame
                   then
                     fail definition.location.span "HCRUN0001"
                       "joined function declaration identities are outside \
                        integer execution";
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
                   let roots =
                     root_map
                       (List.map Typed.expression_statement_value
                          (Typed.function_expression_statements function_)
                       @ List.map Typed.condition_value
                           (Typed.function_conditions function_))
                   in
                   let statements =
                     lower_statements roots
                       (Typed.function_initializers function_)
                       (Typed.function_returns function_)
                       (Option.to_list definition.body)
                   in
                   let* graph =
                     Lower.lower ~frame ~globals:globals_ ~function_calls
                       ~span:definition.location.span statements
                   in
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
                     |> Result.map_error
                          (List.map (fun (error : Body.error) ->
                               Integer_source.diagnostic
                                 ~span:
                                   (Option.value error.span
                                      ~default:definition.location.span)
                                 error.code error.message))
                   in
                   Ok Ir.Integer_interpreter.{ frame; body })
          in
          let* preparation_ =
            Integer_initializers.prepare ~max_steps:max_initializer_steps
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
          let ordinary = ref (lower_statements roots [] [] statements) in
          let pending =
            Ir.Integer_globals.slots globals_
            |> List.filter_map (fun slot ->
                if Ir.Integer_globals.slot_initializer_materialized slot then
                  None
                else
                  Option.map
                    (fun root -> (slot, root))
                    (Ir.Integer_globals.slot_initializer slot))
          in
          let pending_statics =
            Ir.Integer_globals.statics globals_
            |> List.filter (fun slot ->
                Option.is_some (Ir.Integer_globals.static_initializer slot)
                && Ir.Integer_globals.storage_preparation_steps
                     (Ir.Integer_globals.static_storage slot)
                   = 0)
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
                    |> List.filter_map (fun (slot, root) ->
                        let global =
                          Ir.Integer_globals.slot_record slot
                          |> Sema.Global_record_classification
                             .classified_record_source
                          |> Sema.Global_resolution.global_record_global
                        in
                        if
                          Sema.Global_type_resolution.global_item_index global
                          = item_index
                        then Some (Lower.Initialize_global root)
                        else None))
                    @ (pending_statics
                      |> List.filter_map (fun slot ->
                          if
                            Frame.function_item_index
                              (Ir.Integer_globals.static_frame slot)
                            = item_index
                          then Some (Lower.Initialize_static slot)
                          else None)))
            |> List.concat
          in
          let* entry_, regions, static_descriptions =
            Lower.lower_with_storage_initializers ~globals:globals_ ~top_calls
              ~function_calls:(List.rev !all_function_calls)
              ~span:ast.span statements
          in
          let* initialization_ =
            Ir.Global_initialization.create ~static_descriptions ~span:ast.span
              ~globals:globals_ ~entry:entry_ regions
          in
          Ok
            {
              entry_;
              globals_;
              initialization_;
              preparation_;
              functions_ = definitions;
            }
        with Invalid diagnostic -> Error [ diagnostic ]
      in
      match lowered with
      | Ok value -> Ok { value; diagnostics = parsed.diagnostics }
      | Error diagnostics -> Error (parsed.diagnostics @ diagnostics))

let compile ?(max_initializer_steps = 100_000) session ~config ~source =
  if max_initializer_steps <= 0 then
    Error
      [
        Integer_source.diagnostic
          ~span:(Integer_source.source_span source)
          "HCIRVM0001" "max_initializer_steps must be greater than zero";
      ]
  else compile_with_limit ~max_initializer_steps session ~config ~source

let lower session ~config ~source =
  let* compiled = compile session ~config ~source in
  match compiled.value.functions_ with
  | [] when Ir.Integer_globals.byte_size compiled.value.globals_ = 0 ->
      Ok { value = compiled.value.entry_; diagnostics = compiled.diagnostics }
  | _ ->
      Error
        (compiled.diagnostics
        @ [
            Integer_source.diagnostic
              ~span:(Integer_source.source_span source)
              "HCRUN0001"
              "named functions and global storage require the compiled-program \
               API";
          ])

let run ?(max_initializer_steps = 100_000) ?(max_global_bytes = 1_048_576)
    ?(max_literal_bytes = 1_048_576) ?(max_frame_bytes = 1_048_576)
    ?(max_call_depth = 128) session ~config ~source ~max_steps =
  let span = Integer_source.source_span source in
  if
    max_steps <= 0 || max_frame_bytes <= 0 || max_call_depth <= 0
    || max_global_bytes <= 0 || max_initializer_steps <= 0
    || max_literal_bytes <= 0
  then
    Error
      [
        Integer_source.diagnostic ~span "HCIRVM0001"
          "max_steps, max_frame_bytes, max_call_depth, max_global_bytes, \
           max_literal_bytes and max_initializer_steps must be greater than \
           zero";
      ]
  else
    let* graph = compile ~max_initializer_steps session ~config ~source in
    Ir.Integer_interpreter.execute_program ~globals:graph.value.globals_
      ~initialization:graph.value.initialization_ ~max_global_bytes
      ~max_literal_bytes ~max_steps ~max_frame_bytes ~max_call_depth
      ~functions:graph.value.functions_ graph.value.entry_
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
                  @ identity "instruction_id" error.instruction_id
                  @ identity "function_id" error.function_id
                  @ identity "initializer_symbol_id" error.initializer_symbol_id
                  @ Option.to_list
                      (Option.map
                         (fun name -> "initializer=" ^ name)
                         error.initializer_name)
                  @ Option.to_list
                      (Option.map
                         (fun phase ->
                           "initializer_phase="
                           ^ Ir.Global_initialization.phase_name phase)
                         error.initializer_phase)
                  @ Option.to_list
                      (Option.map
                         (fun name -> "function=" ^ name)
                         error.function_name))
                ()))
    |> Result.map_error (fun diagnostics -> graph.diagnostics @ diagnostics)
