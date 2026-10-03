type source =
  | Named of
      Declaration_collection.publication
      * Frontend.Parser.completed_parameter_default
  | Callback of
      Declaration_collection.namespace
      * Frontend.Parser.completed_callback_default

type t = {
  table : Symbol_table.t;
  source_ : source;
  expression_ : Frontend.Ast.expression;
  environment_ : Outer_environment.t;
  references_ : (Frontend.Ast.identifier * Reference_selection.t) list;
  position_reads_ :
    (Frontend.Ast.expression * Compiler_record.compiler_position) list;
  queries_ : Query_selection.t list;
}

type authority = { authorized_fragment : t }

let owns_table fragment table = fragment.table == table
let table fragment = fragment.table
let source fragment = fragment.source_

let publication fragment =
  match fragment.source_ with
  | Named (p, _) -> p
  | Callback _ -> invalid_arg "anonymous default has no named publication"

let receipt fragment =
  match fragment.source_ with
  | Named (_, r) -> r
  | Callback _ -> invalid_arg "anonymous default has no named receipt"

let symbol_opt fragment =
  match fragment.source_ with
  | Named (p, _) -> Some (Declaration_collection.publication_symbol p)
  | Callback _ -> None

let source_ast = function
  | Named (_, r) -> r.Frontend.Parser.default_ast
  | Callback (_, r) -> r.Frontend.Parser.callback_default_ast

let ast fragment = source_ast fragment.source_

let index fragment =
  match fragment.source_ with
  | Named (_, r) -> r.default_parameter_index
  | Callback (_, r) -> r.callback_default_index

let parameter_parts fragment =
  match fragment.source_ with
  | Named (_, r) ->
      ( r.default_type_specifier,
        r.default_pointer_layers,
        r.default_function_pointer )
  | Callback (_, r) ->
      let p = r.callback_default_parameter in
      ( p.callback_parameter_type_specifier,
        p.callback_parameter_pointer_layers,
        p.callback_parameter_function_pointer )

let same_source a b =
  match (a, b) with
  | Named (p, r), Named (q, s) -> p == q && r == s
  | Callback (p, r), Callback (q, s) -> p == q && r == s
  | _ -> false

let current_source ?(allow_activation = true) ~activation = function
  | Named (_, r) ->
      Frontend.Parser.parameter_default_is_current r
      || (allow_activation && Source_activation.parameter_default activation r)
  | Callback (_, r) ->
      Frontend.Parser.callback_default_is_current r
      || (allow_activation && Source_activation.callback_default activation r)

let in_scope fragment scope =
  match fragment.source_ with
  | Named (p, _) ->
      Symbol.Scope_id.equal
        (Symbol.scope_id (Declaration_collection.publication_symbol p))
        (Symbol_table.scope_id scope)
  | Callback (n, _) -> Declaration_collection.namespace_scope n == scope

let expression fragment = fragment.expression_

let origin fragment =
  Frontend.Ast.expression_location fragment.expression_
  |> Initializer_source.origin_of_location

let environment fragment = fragment.environment_
let references fragment = fragment.references_
let queries fragment = fragment.queries_
let authorized_fragment authority = authority.authorized_fragment

let authorize ?activation ~namespace fragment =
  let owns =
    match fragment.source_ with
    | Named (p, _) ->
        Declaration_collection.namespace_owns_publication namespace p
    | Callback (n, _) -> namespace == n
  in
  if
    (not owns)
    || (not (current_source ~activation fragment.source_))
    || Option.is_some activation
       && not
            (Option.fold ~none:false
               ~some:(fun a -> Source_activation.owns_namespace a namespace)
               activation)
  then
    Error
      "default execution requires its original namespace and active source \
       boundary"
  else Ok { authorized_fragment = fragment }

let create_common ~table ~source ~environment ~references ~queries =
  let ( let* ) = Result.bind in
  let* expression_ =
    match (source_ast source).value with
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
      source_ = source;
      expression_;
      environment_ = environment;
      references_ = references;
      position_reads_ = [];
      queries_ = queries;
    }

let create ~table ~publication ~receipt ~environment ~references ~queries =
  let ( let* ) = Result.bind in
  let* () =
    if
      (not
         (Symbol_table.owns_symbol table
            (Declaration_collection.publication_symbol publication)))
      || (not (Outer_environment.owns_table environment table))
      ||
      match
        ( Outer_environment.compilation_mode environment,
          Frontend.Parser.context_mode
            receipt.Frontend.Parser.default_function.function_header
              .declaration_command
              .command_context )
      with
      | Outer_environment.Jit, Frontend.Preprocessor.Jit
      | Outer_environment.Aot, Frontend.Preprocessor.Aot -> false
      | _ -> true
    then
      Error "default fragment requires its original table and compilation mode"
    else
      match Declaration_collection.publication_source_function publication with
      | Some owner when owner == receipt.Frontend.Parser.default_function ->
          Ok ()
      | _ -> Error "default fragment has another source function publication"
  in
  create_common ~table
    ~source:(Named (publication, receipt))
    ~environment ~references ~queries

let create_callback ~table ~namespace ~receipt ~environment ~references ~queries
    =
  let command =
    receipt.Frontend.Parser.callback_default_signature.callback_command
  in
  if
    (not (Declaration_collection.namespace_owns_table namespace table))
    || (not (Outer_environment.owns_table environment table))
    || receipt.callback_default_parameter.callback_parameter_signature
       != receipt.callback_default_signature
    ||
    match
      ( Outer_environment.compilation_mode environment,
        Frontend.Parser.context_mode command.command_context )
    with
    | Outer_environment.Jit, Frontend.Preprocessor.Jit
    | Outer_environment.Aot, Frontend.Preprocessor.Aot -> false
    | _ -> true
  then
    Error
      "anonymous default requires its own original namespace, signature and \
       mode"
  else
    create_common ~table
      ~source:(Callback (namespace, receipt))
      ~environment ~references ~queries

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

type position = {
  position_fragment_ : t;
  position_source_ : Frontend.Ast.expression;
  position_value_ : Compiler_record.compiler_position;
}

let source_position_reads = function
  | Named (_, r) -> r.Frontend.Parser.default_position_reads
  | Callback (_, r) -> r.Frontend.Parser.callback_default_position_reads

let with_positions ~compiler_positions fragment =
  let ( let* ) = Result.bind in
  let reads = source_position_reads fragment.source_ in
  let expected =
    Initializer_source.expression_position_nodes fragment.expression_
  in
  if
    List.length expected <> List.length reads
    || not
         (List.for_all2
            (fun expected (actual, _) -> expected == actual)
            expected reads)
  then
    Error
      "default positions differ from its exact original ordered expression \
       nodes"
  else
    let sources =
      match fragment.source_ with
      | Named (_, r) -> r.default_function.function_header.declaration_sources
      | Callback (_, r) ->
          Frontend.Parser.context_sources
            r.callback_default_signature.callback_command.command_context
    in
    let class_reads =
      List.filter_map
        (function
          | node, Frontend.Parser.Class_default_position source ->
              Some (node, source)
          | _, Instruction_default_position -> None)
        reads
    in
    let* position_reads_ =
      Compiler_record.resolve_default_position_reads compiler_positions ~sources
        class_reads
    in
    Ok { fragment with position_reads_ }

let position_for fragment source =
  match
    List.find_opt (fun (node, _) -> node == source) fragment.position_reads_
  with
  | Some (_, position_value_) ->
      Ok
        {
          position_fragment_ = fragment;
          position_source_ = source;
          position_value_;
        }
  | None ->
      Error
        "default current position has no original lexical write and expression \
         receipt"

let position_matches position fragment source =
  position.position_fragment_ == fragment && position.position_source_ == source

let position_value position =
  Compiler_record.compiler_position_value position.position_value_

let position_dependencies position =
  Compiler_record.compiler_position_dependencies position.position_value_

let position_runtime_dependencies position =
  Compiler_record.compiler_position_runtime_dependencies
    position.position_value_

let position_is_instruction fragment source =
  source_position_reads fragment.source_
  |> List.exists (function
    | node, Frontend.Parser.Instruction_default_position -> node == source
    | _, Class_default_position _ -> false)
