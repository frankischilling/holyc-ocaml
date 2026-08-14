let semantic_mode = function
  | Frontend.Preprocessor.Jit -> Sema.Outer_environment.Jit
  | Frontend.Preprocessor.Aot -> Sema.Outer_environment.Aot

let create_environment ~table ~compilation_mode tables =
  Sema.Outer_environment.create ~table
    ~compilation_mode:(semantic_mode compilation_mode)
    tables
  |> Result.map_error Sema.Outer_environment.error_to_string

let resolve ~table ~environment ~expressions =
  Sema.Outer_expression_binding.resolve ~table ~environment ~expressions
  |> Result.map_error Sema.Outer_expression_binding.error_to_string
