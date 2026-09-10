type t = {
  table : Symbol_table.t;
  declaration_ : Compiler_record.declared_global;
  leaf_ : Initializer_source.leaf;
  environment_ : Outer_environment.t;
  references_ : (Frontend.Ast.identifier * Reference_selection.t) list;
  queries_ : Query_selection.t list;
}

let owns_table fragment table = fragment.table == table
let declaration fragment = fragment.declaration_
let leaf fragment = fragment.leaf_
let environment fragment = fragment.environment_
let references fragment = fragment.references_
let queries fragment = fragment.queries_

type authority = { authorized_fragment : t }

let authorize ~namespace fragment =
  if
    not
      (Compiler_record.declared_global_owns_namespace fragment.declaration_
         namespace)
  then
    Error "initializer execution authority belongs to another source namespace"
  else
    match Initializer_source.leaf_parser_receipt fragment.leaf_ with
    | Some receipt when Frontend.Parser.initializer_leaf_is_current receipt ->
        Ok { authorized_fragment = fragment }
    | _ ->
        Error
          "initializer execution authority requires its original current leaf"

let authorized_fragment authority = authority.authorized_fragment

let create ~table ~declaration ~leaf ~environment ~references ~queries =
  let ( let* ) = Result.bind in
  let* () =
    if
      (not (Compiler_record.declared_global_owns_table declaration table))
      || not (Outer_environment.owns_table environment table)
    then Error "initializer fragment belongs to another semantic table"
    else if
      Outer_environment.compilation_mode environment <> Outer_environment.Jit
    then Error "initializer fragment requires a retained JIT environment"
    else
      match Initializer_source.leaf_parser_receipt leaf with
      | Some receipt
        when receipt.leaf_initializer.initializer_owner
             == Compiler_record.declared_global_source declaration -> Ok ()
      | _ -> Error "initializer fragment has no original declaration leaf"
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
                "initializer fragment reference has another source environment"
        in
        validate rest selections
    | _ ->
        Error
          "initializer fragment references differ from its original ordered \
           leaf"
  in
  let* () =
    validate (Initializer_source.leaf_identifier_nodes leaf) references
  in
  let* () =
    Query_selection.validate_manifest ~table
      ~expression:(Initializer_source.leaf_expression_ast leaf)
      queries
  in
  Ok
    {
      table;
      declaration_ = declaration;
      leaf_ = leaf;
      environment_ = environment;
      references_ = references;
      queries_ = queries;
    }

let reference_for fragment identifier =
  match
    List.find_opt (fun (source, _) -> source == identifier) fragment.references_
  with
  | Some (_, selection) -> Ok selection
  | None -> Error "identifier is absent from this initializer fragment"

let query_for fragment expression =
  match
    List.find_opt
      (fun query -> Query_selection.expression query == expression)
      fragment.queries_
  with
  | Some query -> Ok query
  | None -> Error "query is absent from this initializer fragment"
