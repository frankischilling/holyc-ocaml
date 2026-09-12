module Parser = Frontend.Parser

type node = {
  receipt : Parser.completed_command;
  mutable predecessor : node option option;
}

type family = {
  root : Parser.command_context;
  mutable last_resumed : node option;
}

type t = {
  mutable events : Parser.command_event list;
  mutable starts : Parser.command_start list;
  table : Symbol_table.t;
  mutable families : family list;
  mutable contexts : (Parser.command_context * family) list;
  mutable nodes : node list;
  mutable sequences : (Parser.completed_sequence * node option) list;
}

type identity =
  | Single of Parser.completed_command
  | Sequence of Parser.completed_sequence

type command = {
  owner : t;
  ast : Frontend.Ast.module_;
  identity : identity;
  nodes : node list;
  last_resumed : node option;
}

let create ~table =
  {
    table;
    events = [];
    starts = [];
    families = [];
    contexts = [];
    nodes = [];
    sequences = [];
  }

let command_receipts (command : command) =
  List.map (fun node -> node.receipt) command.nodes

let check_completion_kind ~nested ?(require_accepted = true) order ~admitted
    receipt =
  if
    (require_accepted && not (Parser.sequence_accepted receipt))
    || (match order.events with
      | Parser.Sequence_completed latest :: _ -> latest != receipt
      | _ -> true)
    || (not nested)
       && Option.is_some (Parser.context_parent receipt.Parser.sequence_context)
    || (not
          (List.exists
             (fun (original, _) -> original == receipt)
             order.sequences))
    || not
         (List.for_all
            (fun completed ->
              List.exists
                (fun command ->
                  command.owner == order
                  && List.exists
                       (fun node -> node.receipt == completed)
                       command.nodes)
                admitted)
            receipt.sequence_commands)
  then
    Error "task result requires its exact accepted and admitted root sequence"
  else Ok ()

let check_completion ?require_accepted order ~admitted receipt =
  check_completion_kind ~nested:false ?require_accepted order ~admitted receipt

let check_suspended_completion order ~admitted ~suspension receipt =
  if not (Parser.suspension_owns_sequence suspension receipt) then
    Error "nested task completion has another parser suspension"
  else check_completion_kind ~nested:true order ~admitted receipt

let rec root context =
  match Parser.context_parent context with
  | None -> context
  | Some (Parser.Before_first_command parent) -> root parent
  | Some (Parser.Reading_command start) -> root start.command_context
  | Some (Parser.Awaiting_resume command) ->
      root command.command_start.command_context

let context_family order context =
  List.find_map
    (fun (saved, family) -> if saved == context then Some family else None)
    order.contexts

let find_node (order : t) receipt =
  List.find_opt (fun node -> node.receipt == receipt) order.nodes

let observe_impl order event =
  match event with
  | Parser.Sequence_started context ->
      if Parser.context_mode context <> Frontend.Preprocessor.Jit then
        Error "task source commands require their original parser JIT mode"
      else if Option.is_some (context_family order context) then
        Error "task source context was already registered"
      else
        let root = root context in
        let family =
          match
            List.find_opt (fun family -> family.root == root) order.families
          with
          | Some family -> family
          | None ->
              let family = { root; last_resumed = None } in
              order.families <- family :: order.families;
              family
        in
        order.contexts <- (context, family) :: order.contexts;
        Ok ()
  | Parser.Command_completed receipt ->
      if
        Option.is_none
          (context_family order receipt.command_start.command_context)
        || Option.is_some (find_node order receipt)
      then Error "task source completion is foreign or repeated"
      else (
        order.nodes <- { receipt; predecessor = None } :: order.nodes;
        Ok ())
  | Parser.Command_resumed receipt -> (
      match
        ( find_node order receipt,
          context_family order receipt.command_start.command_context )
      with
      | Some node, Some family when Option.is_none node.predecessor ->
          node.predecessor <- Some family.last_resumed;
          family.last_resumed <- Some node;
          Ok ()
      | _ -> Error "task source resume is foreign, repeated or incomplete")
  | Parser.Sequence_completed receipt -> (
      match context_family order receipt.sequence_context with
      | Some _
        when List.exists
               (fun (original, _) -> original == receipt)
               order.sequences ->
          Error "task source completion has already been observed"
      | Some family ->
          order.sequences <- (receipt, family.last_resumed) :: order.sequences;
          Ok ()
      | None -> Error "task source sequence has no original context")
  | Parser.Command_started start ->
      if
        Option.is_none (context_family order start.command_context)
        || List.exists (( == ) start) order.starts
      then Error "task source start is foreign or repeated"
      else (
        order.starts <- start :: order.starts;
        Ok ())
  | Parser.Sequence_aborted _ -> Ok ()

let observe order event =
  Result.map
    (fun () -> order.events <- event :: order.events)
    (observe_impl order event)

let import_source_events order events =
  if
    order.contexts <> [] || order.starts <> [] || order.families <> []
    || order.nodes <> [] || order.sequences <> []
  then Error "source promotion requires an empty task command order"
  else
    let pending = create ~table:order.table in
    let result =
      List.fold_left
        (fun result event ->
          Result.bind result (fun () -> observe pending event))
        (Ok ()) events
    in
    Result.map
      (fun () ->
        order.events <- pending.events;
        order.starts <- pending.starts;
        order.families <- pending.families;
        order.contexts <- pending.contexts;
        order.nodes <- pending.nodes;
        order.sequences <- pending.sequences)
      result

let seal_command order receipt =
  match find_node order receipt with
  | None -> Error "task source command has no original completion"
  | Some node ->
      Ok
        {
          owner = order;
          ast = receipt.command_ast;
          identity = Single receipt;
          nodes = [ node ];
          last_resumed = None;
        }

let seal_sequence order receipt =
  match List.find_opt (fun (saved, _) -> saved == receipt) order.sequences with
  | Some (_, last_resumed) when Parser.sequence_accepted receipt ->
      let rec collect rev = function
        | [] -> Ok (List.rev rev)
        | receipt :: rest -> (
            match find_node order receipt with
            | Some node -> collect (node :: rev) rest
            | None -> Error "task source sequence lacks an original command")
      in
      Result.map
        (fun nodes ->
          {
            owner = order;
            ast = receipt.sequence_ast;
            identity = Sequence receipt;
            nodes;
            last_resumed;
          })
        (collect [] receipt.sequence_commands)
  | _ -> Error "task source sequence lacks accepted original completion"

let owns order ~ast command =
  command.owner == order
  && command.owner.table == order.table
  && command.ast == ast

let same_item left right =
  let open Frontend.Ast in
  match (left, right) with
  | Aggregate_forward_declaration left, Aggregate_forward_declaration right ->
      left == right
  | Aggregate_definition left, Aggregate_definition right -> left == right
  | Global_variable left, Global_variable right -> left == right
  | Global_declaration left, Global_declaration right -> left == right
  | Function_prototype left, Function_prototype right -> left == right
  | Function_definition left, Function_definition right -> left == right
  | Top_level_statement left, Top_level_statement right ->
      statement_location left == statement_location right
  | _ -> false

let has_source_syntax (order : t) (ast : Frontend.Ast.module_) =
  let overlaps (saved : Frontend.Ast.module_) =
    saved == ast
    || List.exists
         (fun item -> List.exists (same_item item) saved.items)
         ast.items
  in
  List.exists (fun node -> overlaps node.receipt.command_ast) order.nodes
  || List.exists
       (fun (receipt, _) -> overlaps receipt.Parser.sequence_ast)
       order.sequences

let same_identity left right =
  match (left, right) with
  | Single left, Single right -> left == right
  | Sequence left, Sequence right -> left == right
  | _ -> false

let contains node nodes = List.exists (fun saved -> saved == node) nodes

let check_preparation order ~admitted ~(start : Parser.command_start)
    ~predecessor =
  let observed_events =
    List.fold_left
      (fun count event ->
        let context =
          match event with
          | Parser.Sequence_started context | Parser.Sequence_aborted context ->
              context
          | Parser.Command_started start -> start.command_context
          | Parser.Command_completed receipt | Parser.Command_resumed receipt ->
              receipt.command_start.command_context
          | Parser.Sequence_completed receipt -> receipt.sequence_context
        in
        if context == start.command_context then count + 1 else count)
      0 order.events
  in
  if not (Parser.context_is_current start.command_context ~observed_events) then
    Error "declared storage source context is no longer current"
  else if not (List.exists (( == ) start) order.starts) then
    Error "declared storage has no original task command start"
  else
    match predecessor with
    | None -> Ok ()
    | Some receipt -> (
        match find_node order receipt with
        | Some node
          when Option.is_some node.predecessor
               && root receipt.command_start.command_context
                  == root start.command_context
               && List.exists
                    (fun command ->
                      command.owner == order && contains node command.nodes)
                    admitted -> Ok ()
        | _ -> Error "declared storage predecessor has not been admitted")

let check_declaration order ~admitted ~(publication : Parser.global_publication)
    ~predecessor =
  check_preparation order ~admitted
    ~start:publication.global_header.declaration_command ~predecessor

let check_function_publication order ~admitted
    (publication : Parser.function_publication) =
  let start = publication.function_header.declaration_command in
  check_preparation order ~admitted ~start
    ~predecessor:start.command_predecessor

let check_function_header order ~admitted
    (header : Parser.completed_function_header) =
  check_function_publication order ~admitted header.function_publication

let check_dimension ?(require_admitted = true) order ~admitted
    (receipt : Parser.array_dimension_preparation) =
  let start = receipt.dimension_owner.dimensions_command in
  if not (Parser.dimension_preparation_is_current receipt) then
    Error "dimension source callback is not current"
  else
    check_preparation order ~admitted ~start
      ~predecessor:
        (if require_admitted then start.command_predecessor else None)

let check_offset ?(require_admitted = true) order ~admitted
    (receipt : Parser.aggregate_phase) =
  let start = receipt.phase_aggregate.aggregate_header.declaration_command in
  if not (Parser.aggregate_phase_is_current receipt) then
    Error "offset source callback is not current"
  else
    check_preparation order ~admitted ~start
      ~predecessor:
        (if require_admitted then start.command_predecessor else None)

let contains_global command ~(publication : Parser.global_publication)
    ~(completed : Frontend.Ast.global_declarator) ~item_index ~declarator_index
    =
  let open Frontend.Ast in
  List.exists
    (fun node ->
      node.receipt.command_start
      == publication.global_header.declaration_command)
    command.nodes
  &&
  match List.nth_opt command.ast.items item_index with
  | Some (Global_declaration declaration) ->
      Option.fold ~none:false
        ~some:(fun index ->
          Option.fold ~none:false ~some:(( == ) completed)
            (List.nth_opt declaration.declarators index))
        declarator_index
  | Some (Global_variable variable) ->
      Option.is_none declarator_index
      && variable.name == completed.name
      && variable.pointer_layers == completed.pointer_layers
      && variable.array_dimensions == completed.array_dimensions
      && Option.is_none completed.global_initial_value
      && Option.is_none completed.function_pointer
      && completed.delimiter.kind = Semicolon
      && completed.delimiter.location.span == variable.semicolon
  | _ -> false

let check order ~admitted command =
  if command.owner != order then Error "source order belongs to another task"
  else if
    List.exists
      (fun prior -> same_identity command.identity prior.identity)
      admitted
  then Error "task source command or sequence has already been admitted"
  else
    let prior_nodes = List.concat_map (fun prior -> prior.nodes) admitted in
    if List.exists (fun node -> contains node prior_nodes) command.nodes then
      Error "task source syntax has already been admitted"
    else
      let rec check_nodes available = function
        | [] -> (
            match command.last_resumed with
            | Some node when not (contains node available) ->
                Error "task source sequence has pending nested commands"
            | _ -> Ok ())
        | node :: rest -> (
            match node.predecessor with
            | None -> Error "task source command has not reached parser resume"
            | Some (Some previous) when not (contains previous available) ->
                Error "task source predecessor has not been admitted"
            | Some _ -> check_nodes (node :: available) rest)
      in
      check_nodes prior_nodes command.nodes
