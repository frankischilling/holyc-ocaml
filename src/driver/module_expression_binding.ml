module Int_map = Map.Make (Int)

type target = {
  source_symbol : Sema.Symbol.t;
  canonical_symbol : Sema.Symbol.t;
  publication_kind : Sema.Module_expression_binding.publication_kind;
  item_index : int;
  declarator_index : int option;
}

let symbol_number symbol = Sema.Symbol.id symbol |> Sema.Symbol.Id.to_int

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let add_target targets target =
  let key = symbol_number target.source_symbol in
  if Int_map.mem key targets then
    Error "module expression publication target is repeated"
  else Ok (Int_map.add key target targets)

let fold_targets make targets values =
  let rec loop targets = function
    | [] -> Ok targets
    | value :: rest -> (
        match add_target targets (make value) with
        | Error _ as error -> error
        | Ok targets -> loop targets rest)
  in
  loop targets values

let aggregate_target declaration =
  let site = Sema.Aggregate_resolution.resolved_declaration_site declaration in
  {
    source_symbol = Sema.Aggregate_resolution.declaration_site_symbol site;
    canonical_symbol =
      Sema.Aggregate_resolution.resolved_declaration_identity_symbol declaration;
    publication_kind = Sema.Module_expression_binding.Aggregate;
    item_index = Sema.Aggregate_resolution.declaration_site_item_index site;
    declarator_index = None;
  }

let function_target declaration =
  let site = Sema.Function_resolution.resolved_declaration_site declaration in
  let function_ = Sema.Function_resolution.declaration_site_function site in
  {
    source_symbol = Sema.Function_type_resolution.function_symbol function_;
    canonical_symbol =
      Sema.Function_resolution.resolved_declaration_identity_symbol declaration;
    publication_kind = Sema.Module_expression_binding.Function;
    item_index = Sema.Function_type_resolution.function_item_index function_;
    declarator_index = None;
  }

let global_target record =
  let global = Sema.Global_resolution.global_record_global record in
  let symbol = Sema.Global_type_resolution.global_symbol global in
  {
    source_symbol = symbol;
    canonical_symbol = symbol;
    publication_kind = Sema.Module_expression_binding.Global_variable;
    item_index = Sema.Global_type_resolution.global_item_index global;
    declarator_index =
      Sema.Global_type_resolution.global_declarator_index global;
  }

let targets aggregates functions globals =
  match
    fold_targets aggregate_target Int_map.empty
      (Sema.Aggregate_resolution.declarations aggregates)
  with
  | Error _ as error -> error
  | Ok targets -> (
      match
        fold_targets function_target targets
          (Sema.Function_resolution.declarations functions)
      with
      | Error _ as error -> error
      | Ok targets ->
          fold_targets global_target targets
            (Sema.Global_resolution.records globals))

let publication_kind_of_declaration entry =
  match Sema.Declaration_collection.entry_kind entry with
  | Sema.Declaration_collection.Aggregate_forward
  | Sema.Declaration_collection.Aggregate_definition ->
      Sema.Module_expression_binding.Aggregate
  | Sema.Declaration_collection.Function_prototype
  | Sema.Declaration_collection.Function_definition ->
      Sema.Module_expression_binding.Function
  | Sema.Declaration_collection.Aggregate_attached_global
  | Sema.Declaration_collection.Global_variable ->
      Sema.Module_expression_binding.Global_variable

let target_matches_entry entry target =
  let symbol = Sema.Declaration_collection.entry_symbol entry in
  same_symbol symbol target.source_symbol
  && publication_kind_of_declaration entry = target.publication_kind
  && Sema.Declaration_collection.entry_item_index entry = target.item_index
  && Sema.Declaration_collection.entry_declarator_index entry
     = target.declarator_index

let publications declarations targets =
  let rec loop declaration_index publications_rev remaining = function
    | [] ->
        if Int_map.is_empty remaining then Ok (List.rev publications_rev)
        else
          Error
            "module expression semantic records are absent from declaration \
             collection"
    | entry :: rest -> (
        let symbol = Sema.Declaration_collection.entry_symbol entry in
        let key = symbol_number symbol in
        match Int_map.find_opt key remaining with
        | None ->
            Error
              "module expression declaration has no matching semantic record"
        | Some target -> (
            if not (target_matches_entry entry target) then
              Error
                "module expression declaration does not match its semantic \
                 record"
            else
              match
                Sema.Module_expression_binding.make_publication
                  ~source_symbol:target.source_symbol
                  ~canonical_symbol:target.canonical_symbol
                  ~publication_kind:target.publication_kind ~declaration_index
                  ~item_index:target.item_index
                  ?declarator_index:target.declarator_index ()
              with
              | Error _ as error -> error
              | Ok publication ->
                  loop (declaration_index + 1)
                    (publication :: publications_rev)
                    (Int_map.remove key remaining)
                    rest))
  in
  loop 0 [] targets (Sema.Declaration_collection.entries declarations)

let modes_match functions globals =
  match
    ( Sema.Function_resolution.compilation_mode functions,
      Sema.Global_resolution.compilation_mode globals )
  with
  | Sema.Function_resolution.Jit, Sema.Global_resolution.Jit
  | Sema.Function_resolution.Aot, Sema.Global_resolution.Aot -> true
  | Sema.Function_resolution.Jit, Sema.Global_resolution.Aot
  | Sema.Function_resolution.Aot, Sema.Global_resolution.Jit -> false

let validate_expression_functions functions expressions =
  let rec pair declarations expression_functions =
    match (declarations, expression_functions) with
    | [], [] -> Ok ()
    | declaration :: declaration_rest, expression :: expression_rest ->
        let site =
          Sema.Function_resolution.resolved_declaration_site declaration
        in
        let function_ =
          Sema.Function_resolution.declaration_site_function site
        in
        let expected_symbol =
          Sema.Function_type_resolution.function_symbol function_
        in
        let expected_item =
          Sema.Function_type_resolution.function_item_index function_
        in
        if
          same_symbol expected_symbol
            (Sema.Function_expression_binding.function_symbol expression)
          && expected_item
             = Sema.Function_expression_binding.function_item_index expression
        then pair declaration_rest expression_rest
        else
          Error
            "module expression functions do not match function identity \
             resolution"
    | [], _ :: _ | _ :: _, [] ->
        Error
          "module expression function count does not match function identity \
           resolution"
  in
  pair
    (Sema.Function_resolution.declarations functions)
    (Sema.Function_expression_binding.functions expressions)

let resolve ~table ~declarations ~aggregates ~functions ~globals ~expressions =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "module expression declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "module expression declarations require a module scope"
    else if not (modes_match functions globals) then
      Error "module expression function and global modes do not match"
    else
      match validate_expression_functions functions expressions with
      | Error _ as error -> error
      | Ok () -> (
          match targets aggregates functions globals with
          | Error _ as error -> error
          | Ok targets -> (
              match publications declarations targets with
              | Error _ as error -> error
              | Ok publications ->
                  Sema.Module_expression_binding.resolve ~table ~parent
                    ~expressions publications
                  |> Result.map_error
                       Sema.Module_expression_binding.error_to_string))
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0020: " ^ message)
    result
