type t = {
  table : Symbol_table.t;
  publication_ : Declaration_collection.publication;
  receipt_ : Frontend.Parser.completed_parameter_default;
  expression_ : Frontend.Ast.expression;
  environment_ : Outer_environment.t;
  references_ : (Frontend.Ast.identifier * Reference_selection.t) list;
  queries_ : Query_selection.t list;
}

type authority = { authorized_fragment : t }

let owns_table fragment table = fragment.table == table
let publication fragment = fragment.publication_
let receipt fragment = fragment.receipt_
let expression fragment = fragment.expression_

let origin fragment =
  Frontend.Ast.expression_location fragment.expression_
  |> Initializer_source.origin_of_location

let environment fragment = fragment.environment_
let references fragment = fragment.references_
let queries fragment = fragment.queries_
let authorized_fragment authority = authority.authorized_fragment

let authorize ?activation ~namespace fragment =
  if
    (not
       (Declaration_collection.namespace_owns_publication namespace
          fragment.publication_))
    || not
         (Frontend.Parser.parameter_default_is_current fragment.receipt_
         || Option.fold ~none:false
              ~some:(fun a -> Source_activation.owns_namespace a namespace)
              activation
            && Source_activation.parameter_default activation fragment.receipt_
         )
  then
    Error
      "default execution authority requires its original namespace and current \
       callback"
  else Ok { authorized_fragment = fragment }

let create ~table ~publication ~receipt ~environment ~references ~queries =
  let ( let* ) = Result.bind in
  let* () =
    if
      (not
         (Symbol_table.owns_symbol table
            (Declaration_collection.publication_symbol publication)))
      || (not (Outer_environment.owns_table environment table))
      || Outer_environment.compilation_mode environment <> Outer_environment.Jit
    then
      Error
        "default fragment requires its original table and retained JIT \
         environment"
    else
      match Declaration_collection.publication_source_function publication with
      | Some owner when owner == receipt.Frontend.Parser.default_function ->
          Ok ()
      | _ -> Error "default fragment has another source function publication"
  in
  let* expression_ =
    match receipt.default_ast.value with
    | Frontend.Ast.Expression_default expression -> Ok expression
    | Frontend.Ast.Lastclass_default _ ->
        Error "lastclass has no declaration-time expression fragment"
  in
  let rec validate expected actual =
    match (expected, actual) with
    | [], [] -> Ok ()
    | ( (identifier : Frontend.Ast.identifier) :: rest,
        (selected, selection) :: selections )
      when identifier == selected ->
        let* () =
          Reference_selection.validate ~table
            ~name:identifier.Frontend.Ast.spelling selection
        in
        let* () =
          match Reference_selection.kind selection with
          | Reference_selection.Outer (owner, binding)
            when owner == environment
                 && Outer_environment.owns_binding environment binding -> Ok ()
          | Reference_selection.Absent | Reference_selection.Unavailable ->
              Ok ()
          | _ ->
              Error
                "default fragment reference has another retained environment"
        in
        validate rest selections
    | _ ->
        Error
          "default fragment references differ from its original ordered \
           expression"
  in
  let* () =
    validate
      (Initializer_source.expression_identifier_nodes expression_)
      references
  in
  let* () =
    Query_selection.validate_manifest ~table ~expression:expression_ queries
  in
  Ok
    {
      table;
      publication_ = publication;
      receipt_ = receipt;
      expression_;
      environment_ = environment;
      references_ = references;
      queries_ = queries;
    }

let reference_for fragment identifier =
  match
    List.find_opt (fun (source, _) -> source == identifier) fragment.references_
  with
  | Some (_, selection) -> Ok selection
  | None -> Error "identifier is absent from the original default fragment"

let query_for fragment expression =
  match
    List.find_opt
      (fun query -> Query_selection.expression query == expression)
      fragment.queries_
  with
  | Some query -> Ok query
  | None -> Error "query is absent from the original default fragment"
