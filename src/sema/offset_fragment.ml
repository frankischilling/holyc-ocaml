type t = {
  table : Symbol_table.t;
  namespace_ : Declaration_collection.namespace;
  receipt_ : Frontend.Parser.aggregate_phase;
  progress_ : Compiler_record.aggregate_progress;
  expression_ : Frontend.Ast.expression;
  environment_ : Outer_environment.t;
  references_ : (Frontend.Ast.identifier * Reference_selection.t) list;
  queries_ : Query_selection.t list;
}

type authority = {
  authorized_fragment : t;
  preparation_ : Compiler_record.runtime_aggregate_offset;
}

let owns_table fragment table = fragment.table == table
let namespace fragment = fragment.namespace_
let receipt fragment = fragment.receipt_
let expression fragment = fragment.expression_

let origin fragment =
  Frontend.Ast.expression_location fragment.expression_
  |> Initializer_source.origin_of_location

let environment fragment = fragment.environment_
let references fragment = fragment.references_
let queries fragment = fragment.queries_
let authorized_fragment authority = authority.authorized_fragment
let preparation authority = authority.preparation_

let authorize fragment =
  Compiler_record.begin_runtime_aggregate_offset ~table:fragment.table
    ~namespace:fragment.namespace_
    ~queries:(List.map Query_selection.checked_read fragment.queries_)
    fragment.progress_ fragment.receipt_
  |> Result.map (fun preparation_ ->
      { authorized_fragment = fragment; preparation_ })

let create ~table ~namespace ~progress ~receipt ~environment ~references
    ~queries =
  let ( let* ) = Result.bind in
  let* () =
    if
      Declaration_collection.namespace_owns_table namespace table
      && Outer_environment.owns_table environment table
      && Outer_environment.compilation_mode environment = Outer_environment.Jit
      && Frontend.Parser.context_mode
           receipt.Frontend.Parser.phase_aggregate.aggregate_header
             .declaration_command
             .command_context
         = Frontend.Preprocessor.Jit
    then Ok ()
    else Error "runtime offset has another table, namespace or compilation mode"
  in
  let* expression_ =
    match receipt.phase_step with
    | Frontend.Parser.Aggregate_offset_reached expression -> Ok expression
    | _ -> Error "aggregate phase has no offset expression"
  in
  let rec validate expected actual =
    match (expected, actual) with
    | [], [] -> Ok ()
    | ( (identifier : Frontend.Ast.identifier) :: rest,
        (selected, selection) :: selections )
      when identifier == selected ->
        let* () =
          Reference_selection.validate ~table ~name:identifier.spelling
            selection
        in
        let* () =
          match Reference_selection.kind selection with
          | Reference_selection.Outer (owner, binding)
            when owner == environment
                 && Outer_environment.owns_binding environment binding -> Ok ()
          | Reference_selection.Absent | Reference_selection.Unavailable ->
              Ok ()
          | _ -> Error "offset reference has another retained environment"
        in
        validate rest selections
    | _ -> Error "offset references differ from its original ordered expression"
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
      namespace_ = namespace;
      receipt_ = receipt;
      progress_ = progress;
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
  | None -> Error "identifier is absent from the original offset fragment"

let query_for fragment expression =
  match
    List.find_opt
      (fun query -> Query_selection.expression query == expression)
      fragment.queries_
  with
  | Some query -> Ok query
  | None -> Error "query is absent from the original offset fragment"
