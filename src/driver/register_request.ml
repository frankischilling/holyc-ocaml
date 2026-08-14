let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let of_ast (qualifier : Frontend.Ast.register_qualifier) =
  let kind =
    match qualifier.kind with
    | Frontend.Ast.Reg -> Sema.Register_request.Allocate
    | Frontend.Ast.Noreg -> Sema.Register_request.Disable
  in
  let position =
    match qualifier.position with
    | Frontend.Ast.Before_type -> Sema.Register_request.Before_type
    | Frontend.Ast.After_type -> Sema.Register_request.After_type
  in
  let explicit_register =
    Option.map
      (fun (register : Frontend.Ast.identifier) -> register.spelling)
      qualifier.explicit_register
  in
  let explicit_register_number =
    Option.bind explicit_register Sema.Register_request.canonical_u64_register_number
  in
  let explicit_register_origin =
    Option.map
      (fun (register : Frontend.Ast.identifier) -> origin register.location)
      qualifier.explicit_register
  in
  Sema.Register_request.make ~kind ~position ~spelling:qualifier.spelling
    ~origin:(origin qualifier.location) ?explicit_register
    ?explicit_register_number ?explicit_register_origin ()

let of_list qualifiers =
  let rec collect requests_rev = function
    | [] -> Ok (List.rev requests_rev)
    | qualifier :: rest -> (
        match of_ast qualifier with
        | Error _ as error -> error
        | Ok request -> collect (request :: requests_rev) rest)
  in
  collect [] qualifiers
