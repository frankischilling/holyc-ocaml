let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let same_scope left right =
  Sema.Symbol.Scope_id.equal
    (Sema.Symbol_table.scope_id left)
    (Sema.Symbol_table.scope_id right)

let binding_kind = function
  | Sema.Function_collection.Named_parameter ->
      Sema.Function_binding_index.Named_parameter
  | Sema.Function_collection.Variadic_argc ->
      Sema.Function_binding_index.Variadic_argc
  | Sema.Function_collection.Variadic_argv ->
      Sema.Function_binding_index.Variadic_argv
  | Sema.Function_collection.Automatic_local ->
      Sema.Function_binding_index.Automatic_local
  | Sema.Function_collection.Static_local ->
      Sema.Function_binding_index.Static_local

type typed_parameter =
  | Named of Sema.Function_type_resolution.parameter_binding
  | Synthetic of Sema.Function_type_resolution.synthetic_binding

let typed_parameters function_ =
  let named =
    Sema.Function_type_resolution.function_parameter_bindings function_
    |> List.map (fun binding -> Named binding)
  in
  let synthetic =
    match
      Sema.Function_type_resolution.function_variadic_bindings function_
    with
    | None -> []
    | Some variadic ->
        [
          Synthetic (Sema.Function_type_resolution.variadic_argc variadic);
          Synthetic (Sema.Function_type_resolution.variadic_argv variadic);
        ]
  in
  named @ synthetic

let typed_parameter_symbol = function
  | Named binding ->
      Sema.Function_type_resolution.parameter_binding_symbol binding
  | Synthetic binding ->
      Sema.Function_type_resolution.synthetic_binding_symbol binding

let typed_parameter_index = function
  | Named binding ->
      Sema.Function_type_resolution.parameter_binding_index binding
  | Synthetic binding ->
      Sema.Function_type_resolution.synthetic_binding_index binding

let typed_parameter_kind = function
  | Named _ -> Sema.Function_collection.Named_parameter
  | Synthetic binding -> (
      match Sema.Function_type_resolution.synthetic_binding_kind binding with
      | Sema.Function_type_resolution.Argc ->
          Sema.Function_collection.Variadic_argc
      | Sema.Function_type_resolution.Argv ->
          Sema.Function_collection.Variadic_argv)

let validate_parameter table entry typed =
  let symbol = Sema.Function_collection.entry_symbol entry in
  let typed_symbol = typed_parameter_symbol typed in
  let entry_kind = Sema.Function_collection.entry_kind entry in
  let parameter_index = Sema.Function_collection.entry_parameter_index entry in
  if not (Sema.Symbol_table.owns_symbol table typed_symbol) then
    Error "function binding parameter type belongs to another symbol table"
  else if entry_kind <> typed_parameter_kind typed then
    Error "function binding parameter kinds do not match"
  else if not (same_symbol symbol typed_symbol) then
    Error "function binding parameter symbols do not match"
  else if parameter_index <> Some (typed_parameter_index typed) then
    Error "function binding parameter positions do not match"
  else if
    Sema.Function_collection.entry_local_declaration_index entry <> None
    || Sema.Function_collection.entry_declarator_index entry <> None
  then Error "function binding parameter has a local source position"
  else Ok ()

let local_storage_matches kind local =
  match (kind, Sema.Local_type_resolution.local_storage local) with
  | ( Sema.Function_collection.Automatic_local,
      Sema.Local_type_resolution.Automatic )
  | Sema.Function_collection.Static_local, Sema.Local_type_resolution.Static ->
      true
  | ( ( Sema.Function_collection.Named_parameter
      | Sema.Function_collection.Variadic_argc
      | Sema.Function_collection.Variadic_argv
      | Sema.Function_collection.Automatic_local
      | Sema.Function_collection.Static_local ),
      (Sema.Local_type_resolution.Automatic | Sema.Local_type_resolution.Static)
    ) -> false

let validate_local table entry local =
  let symbol = Sema.Function_collection.entry_symbol entry in
  let local_symbol = Sema.Local_type_resolution.local_symbol local in
  let kind = Sema.Function_collection.entry_kind entry in
  if not (Sema.Symbol_table.owns_symbol table local_symbol) then
    Error "function binding local type belongs to another symbol table"
  else if not (local_storage_matches kind local) then
    Error "function binding local storage kinds do not match"
  else if not (same_symbol symbol local_symbol) then
    Error "function binding local symbols do not match"
  else if
    Sema.Function_collection.entry_local_declaration_index entry
    <> Some (Sema.Local_type_resolution.local_declaration_index local)
  then Error "function binding local declaration positions do not match"
  else if
    Sema.Function_collection.entry_declarator_index entry
    <> Some (Sema.Local_type_resolution.local_declarator_index local)
  then Error "function binding local declarator positions do not match"
  else if Sema.Function_collection.entry_parameter_index entry <> None then
    Error "function binding local has a parameter source position"
  else Ok ()

let input_of_entry entry =
  {
    Sema.Function_binding_index.binding_symbol =
      Sema.Function_collection.entry_symbol entry;
    binding_kind = binding_kind (Sema.Function_collection.entry_kind entry);
    parameter_index = Sema.Function_collection.entry_parameter_index entry;
    local_declaration_index =
      Sema.Function_collection.entry_local_declaration_index entry;
    local_declarator_index =
      Sema.Function_collection.entry_declarator_index entry;
  }

let binding_inputs table collected typed local_types =
  let entries = Sema.Function_collection.function_entries collected in
  let parameters = typed_parameters typed in
  let locals = Sema.Local_type_resolution.function_locals local_types in
  let rec loop inputs_rev entries parameters locals =
    match entries with
    | [] ->
        if parameters <> [] then
          Error "function binding index is missing resolved parameters"
        else if locals <> [] then
          Error "function binding index is missing resolved locals"
        else Ok (List.rev inputs_rev)
    | entry :: rest -> (
        match Sema.Function_collection.entry_kind entry with
        | Sema.Function_collection.Named_parameter
        | Sema.Function_collection.Variadic_argc
        | Sema.Function_collection.Variadic_argv -> (
            match parameters with
            | [] ->
                Error "function binding index has an extra collected parameter"
            | parameter :: parameter_rest -> (
                match validate_parameter table entry parameter with
                | Error _ as error -> error
                | Ok () ->
                    loop
                      (input_of_entry entry :: inputs_rev)
                      rest parameter_rest locals))
        | Sema.Function_collection.Automatic_local
        | Sema.Function_collection.Static_local -> (
            if parameters <> [] then
              Error
                "function binding index places a local before its parameters"
            else
              match locals with
              | [] ->
                  Error "function binding index has an extra collected local"
              | local :: local_rest -> (
                  match validate_local table entry local with
                  | Error _ as error -> error
                  | Ok () ->
                      loop
                        (input_of_entry entry :: inputs_rev)
                        rest [] local_rest)))
  in
  loop [] entries parameters locals

let validate_function table collected typed local_types =
  let collected_symbol = Sema.Function_collection.function_symbol collected in
  let collected_scope = Sema.Function_collection.function_scope collected in
  let collected_item = Sema.Function_collection.function_item_index collected in
  let typed_symbol = Sema.Function_type_resolution.function_symbol typed in
  let typed_scope = Sema.Function_type_resolution.function_scope typed in
  let typed_item = Sema.Function_type_resolution.function_item_index typed in
  let local_symbol = Sema.Local_type_resolution.function_symbol local_types in
  let local_scope = Sema.Local_type_resolution.function_scope local_types in
  let local_item = Sema.Local_type_resolution.function_item_index local_types in
  if not (Sema.Symbol_table.owns_symbol table collected_symbol) then
    Error "function binding collection belongs to another symbol table"
  else if not (Sema.Symbol_table.owns_scope table collected_scope) then
    Error "function binding collection scope belongs to another symbol table"
  else if
    not
      (Sema.Symbol_table.owns_symbol table typed_symbol
      && Sema.Symbol_table.owns_symbol table local_symbol)
  then Error "function binding type results belong to another symbol table"
  else if
    not
      (Sema.Symbol_table.owns_scope table typed_scope
      && Sema.Symbol_table.owns_scope table local_scope)
  then Error "function binding type scopes belong to another symbol table"
  else if
    not
      (same_symbol collected_symbol typed_symbol
      && same_symbol collected_symbol local_symbol)
  then Error "function binding pass results have different function symbols"
  else if
    not
      (same_scope collected_scope typed_scope
      && same_scope collected_scope local_scope)
  then Error "function binding pass results have different function scopes"
  else if collected_item <> typed_item || collected_item <> local_item then
    Error "function binding pass results have different module positions"
  else
    match binding_inputs table collected typed local_types with
    | Error _ as error -> error
    | Ok function_bindings ->
        Ok
          {
            Sema.Function_binding_index.function_symbol = collected_symbol;
            function_scope = collected_scope;
            function_item_index = collected_item;
            function_bindings;
          }

let function_inputs table functions function_types local_types =
  let rec loop inputs_rev functions function_types local_types =
    match (functions, function_types, local_types) with
    | [], [], [] -> Ok (List.rev inputs_rev)
    | collected :: collected_rest, typed :: typed_rest, local :: local_rest -> (
        match validate_function table collected typed local with
        | Error _ as error -> error
        | Ok input ->
            loop (input :: inputs_rev) collected_rest typed_rest local_rest)
    | [], _, _ | _, [], _ | _, _, [] ->
        Error "function binding pass results contain different function counts"
  in
  loop [] functions function_types local_types

let build ~table ~declarations ~functions ~function_types ~local_types =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "function binding declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "function binding indexes require a module declaration collection"
    else
      match
        function_inputs table
          (Sema.Function_collection.functions functions)
          (Sema.Function_type_resolution.functions function_types)
          (Sema.Local_type_resolution.functions local_types)
      with
      | Error _ as error -> error
      | Ok inputs ->
          Sema.Function_binding_index.build ~table ~parent inputs
          |> Result.map_error Sema.Function_binding_index.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0014: " ^ message)
    result
