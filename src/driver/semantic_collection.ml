let declaration ~(identifier : Frontend.Ast.identifier) ~declaration_kind
    ~item_index ?declarator_index () =
  let location = identifier.location in
  let origin =
    Sema.Symbol.Source_location
      {
        span = location.span;
        source_segments = location.source_segments;
        generated_from = location.generated_from;
        defined_at = location.defined_at;
      }
  in
  Sema.Declaration_collection.make_declaration ~name:identifier.spelling
    ~declaration_kind ~origin ~item_index ?declarator_index ()

let declarations ~declaration_kind ~item_index declarators =
  let rec collect declarator_index declarations_rev = function
    | [] -> Ok (List.rev declarations_rev)
    | (declarator : Frontend.Ast.global_declarator) :: rest -> (
        match
          declaration ~identifier:declarator.name ~declaration_kind ~item_index
            ~declarator_index ()
        with
        | Error _ as error -> error
        | Ok declaration ->
            collect (declarator_index + 1)
              (declaration :: declarations_rev)
              rest)
  in
  collect 0 [] declarators

let declarations_for_item item_index = function
  | Frontend.Ast.Aggregate_forward_declaration forward ->
      Result.map
        (fun declaration -> [ declaration ])
        (declaration ~identifier:forward.name
           ~declaration_kind:Sema.Declaration_collection.Aggregate_forward
           ~item_index ())
  | Frontend.Ast.Aggregate_definition definition -> (
      match
        declaration ~identifier:definition.name
          ~declaration_kind:Sema.Declaration_collection.Aggregate_definition
          ~item_index ()
      with
      | Error _ as error -> error
      | Ok aggregate ->
          Result.map
            (fun globals -> aggregate :: globals)
            (declarations
               ~declaration_kind:
                 Sema.Declaration_collection.Aggregate_attached_global
               ~item_index definition.attached_declarators))
  | Frontend.Ast.Global_variable variable ->
      Result.map
        (fun declaration -> [ declaration ])
        (declaration ~identifier:variable.name
           ~declaration_kind:Sema.Declaration_collection.Global_variable
           ~item_index ())
  | Frontend.Ast.Global_declaration globals ->
      declarations ~declaration_kind:Sema.Declaration_collection.Global_variable
        ~item_index globals.declarators
  | Frontend.Ast.Function_prototype prototype ->
      Result.map
        (fun declaration -> [ declaration ])
        (declaration ~identifier:prototype.name
           ~declaration_kind:Sema.Declaration_collection.Function_prototype
           ~item_index ())
  | Frontend.Ast.Function_definition definition ->
      Result.map
        (fun declaration -> [ declaration ])
        (declaration ~identifier:definition.name
           ~declaration_kind:Sema.Declaration_collection.Function_definition
           ~item_index ())
  | Frontend.Ast.Top_level_statement _ -> Ok []

let all_declarations (module_ : Frontend.Ast.module_) =
  let rec collect item_index declarations_rev = function
    | [] -> Ok (List.rev declarations_rev |> List.concat)
    | item :: rest -> (
        match declarations_for_item item_index item with
        | Error _ as error -> error
        | Ok declarations ->
            collect (item_index + 1) (declarations :: declarations_rev) rest)
  in
  collect 0 [] module_.items

let module_name sources (module_ : Frontend.Ast.module_) =
  match Common.Source_manager.find sources module_.source with
  | Some source -> Ok (Common.Source_file.display_path source)
  | None ->
      Error
        (Printf.sprintf
           "cannot collect semantic declarations because source %d is not \
            registered"
           (Common.Source_id.to_int module_.source))

let collect ~sources ~table module_ =
  match module_name sources module_ with
  | Error _ as error -> error
  | Ok module_name -> (
      match all_declarations module_ with
      | Error _ as error -> error
      | Ok declarations ->
          Sema.Declaration_collection.collect ~table ~module_name declarations)
