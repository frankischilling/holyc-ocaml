module Int_map = Map.Make (Int)

let symbol_number symbol = Sema.Symbol.id symbol |> Sema.Symbol.Id.to_int

let global_map table globals =
  let rec loop map = function
    | [] -> Ok map
    | global :: rest ->
        let symbol = Sema.Global_type_resolution.global_symbol global in
        let key = symbol_number symbol in
        if not (Sema.Symbol_table.owns_symbol table symbol) then
          Error "top-level global type belongs to another symbol table"
        else if Int_map.mem key map then
          Error "top-level global type symbol is repeated"
        else loop (Int_map.add key global map) rest
  in
  loop Int_map.empty (Sema.Global_type_resolution.globals globals)

let function_map table functions =
  let rec loop map = function
    | [] -> Ok map
    | declaration :: rest ->
        let site =
          Sema.Function_resolution.resolved_declaration_site declaration
        in
        let source_symbol =
          site |> Sema.Function_resolution.declaration_site_function
          |> Sema.Function_type_resolution.function_symbol
        in
        let identity =
          Sema.Function_resolution.resolved_declaration_identity_symbol
            declaration
        in
        let key = symbol_number source_symbol in
        if
          not
            (Sema.Symbol_table.owns_symbol table source_symbol
            && Sema.Symbol_table.owns_symbol table identity)
        then Error "top-level function result belongs to another symbol table"
        else if Int_map.mem key map then
          Error "top-level function source symbol is repeated"
        else loop (Int_map.add key declaration map) rest
  in
  loop Int_map.empty (Sema.Function_resolution.declarations functions)

let find map publication =
  publication |> Sema.Module_expression_binding.publication_source_symbol
  |> symbol_number
  |> Fun.flip Int_map.find_opt map

let module_resolution globals functions compilation_mode publication =
  match Sema.Module_expression_binding.publication_kind publication with
  | Sema.Module_expression_binding.Aggregate ->
      Ok
        (Sema.Top_level_identifier_resolution.Module_value
           (Sema.Top_level_identifier_resolution.Aggregate_offset_base
              publication))
  | Sema.Module_expression_binding.Global_variable -> (
      match find globals publication with
      | None -> Error "top-level global publication has no checked type"
      | Some global -> (
          match
            Sema.Function_call_resolution.global_identifier_value global
          with
          | Error _ as error -> error
          | Ok value ->
              Ok
                (Sema.Top_level_identifier_resolution.Module_value
                   (Sema.Top_level_identifier_resolution.Global_value
                      { global; value }))))
  | Sema.Module_expression_binding.Function -> (
      match find functions publication with
      | None -> Error "top-level function publication has no checked identity"
      | Some declaration -> (
          match
            Sema.Function_call_resolution.direct_function_address_path
              compilation_mode declaration
          with
          | Error _ as error -> error
          | Ok address_path -> (
              match
                Sema.Function_call_resolution.direct_function_identifier_value
                  ~declaration ~address_path
              with
              | Error _ as error -> error
              | Ok value ->
                  Ok
                    (Sema.Top_level_identifier_resolution.Module_value
                       (Sema.Top_level_identifier_resolution
                        .Direct_function_value
                          { declaration; value })))))

let leaf globals functions compilation_mode node occurrence =
  let resolution =
    match
      Sema.Top_level_outer_expression_binding.occurrence_resolution occurrence
    with
    | Sema.Top_level_outer_expression_binding.Module_binding publication ->
        module_resolution globals functions compilation_mode publication
    | Sema.Top_level_outer_expression_binding.Outer_binding binding ->
        let entry = Sema.Outer_environment.binding_entry binding in
        if Option.is_some (Sema.Outer_environment.entry_global_metadata entry)
        then Ok (Sema.Top_level_identifier_resolution.Outer_value binding)
        else
          Ok (Sema.Top_level_identifier_resolution.Outer_type_required binding)
  in
  match resolution with
  | Error _ as error -> error
  | Ok resolution -> (
      match
        Sema.Top_level_identifier_resolution.make_leaf ~node ~occurrence
          ~resolution
      with
      | Error error ->
          Error (Sema.Top_level_identifier_resolution.error_to_string error)
      | Ok leaf -> Ok leaf)

let classify_nodes globals functions compilation_mode expressions =
  let rec loop rev = function
    | [] -> Ok (List.rev rev)
    | node :: rest -> (
        let source =
          Sema.Top_level_expression_tree.expression_node_source node
        in
        match Sema.Function_call_resolution.argument_expression_kind source with
        | Sema.Function_call_resolution.Top_level_bound_identifier_expression
            identifier -> (
            let occurrence =
              Sema.Function_call_resolution
              .top_level_bound_identifier_occurrence identifier
            in
            match leaf globals functions compilation_mode node occurrence with
            | Error _ as error -> error
            | Ok leaf -> loop (leaf :: rev) rest)
        | _ -> loop rev rest)
  in
  loop [] (Sema.Top_level_expression_tree.all_expression_nodes expressions)

let publication_values globals functions compilation_mode expressions =
  expressions |> Sema.Top_level_expression_tree.source
  |> Sema.Top_level_outer_expression_binding.source
  |> Sema.Top_level_expression_binding.module_expressions
  |> Sema.Module_expression_binding.publications
  |> List.fold_left
       (fun result publication ->
         match result with
         | Error _ as error -> error
         | Ok reversed
           when Sema.Module_expression_binding.publication_kind publication
                <> Sema.Module_expression_binding.Global_variable -> Ok reversed
         | Ok reversed -> (
             match
               module_resolution globals functions compilation_mode publication
             with
             | Error _ as error -> error
             | Ok (Sema.Top_level_identifier_resolution.Module_value value) ->
                 Ok ((publication, value) :: reversed)
             | Ok
                 ( Sema.Top_level_identifier_resolution.Outer_value _
                 | Sema.Top_level_identifier_resolution.Outer_type_required _ )
               -> Error "module publication resolved as an outer binding"))
       (Ok [])
  |> Result.map List.rev

let classify ~table ~globals ~functions ~expressions =
  if not (Sema.Top_level_expression_tree.owns_table expressions table) then
    Error
      "HCSEMA0056: top-level expression tree belongs to another symbol table"
  else
    let environment =
      expressions |> Sema.Top_level_expression_tree.source
      |> Sema.Top_level_outer_expression_binding.environment
    in
    let compilation_mode =
      Sema.Outer_environment.compilation_mode environment
    in
    if Sema.Function_resolution.compilation_mode functions <> compilation_mode
    then
      Error
        "HCSEMA0056: top-level function identities use another compilation mode"
    else
      match (global_map table globals, function_map table functions) with
      | Error message, _ | _, Error message -> Error ("HCSEMA0056: " ^ message)
      | Ok globals, Ok functions -> (
          match
            ( classify_nodes globals functions compilation_mode expressions,
              publication_values globals functions compilation_mode expressions
            )
          with
          | Error message, _ | _, Error message ->
              if String.starts_with ~prefix:"HCSEMA0056:" message then
                Error message
              else Error ("HCSEMA0056: " ^ message)
          | Ok leaves, Ok module_values -> (
              match
                Sema.Top_level_identifier_resolution.create ~table
                  ~source:expressions ~module_values leaves
              with
              | Ok result -> Ok result
              | Error error ->
                  Error
                    (Sema.Top_level_identifier_resolution.error_to_string error)
              ))
