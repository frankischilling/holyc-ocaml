val of_ast :
  Frontend.Ast.register_qualifier -> (Sema.Register_request.t, string) result

val of_list :
  Frontend.Ast.register_qualifier list ->
  (Sema.Register_request.t list, string) result
