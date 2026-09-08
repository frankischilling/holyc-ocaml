type t = {
  metadata : Sema.Outer_environment.function_metadata;
  identity : unit ref;
}

let create metadata = { metadata; identity = ref () }
let metadata reference = reference.metadata

let symbol reference =
  reference.metadata |> Sema.Outer_environment.function_declaration
  |> Sema.Function_resolution.resolved_declaration_identity_symbol

let same left right = left.identity == right.identity
