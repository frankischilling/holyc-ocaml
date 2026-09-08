module Ast = Frontend.Ast
module Parser = Frontend.Parser
module Visibility = Frontend.Symbol_visibility
module Collection = Sema.Declaration_collection

module Names = Hashtbl.Make (struct
  type t = Ast.identifier

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

module Entries = Hashtbl.Make (struct
  type t = Visibility.entry

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

type source =
  | Global of {
      publication : Parser.global_publication;
      mutable completed : Ast.global_declarator option;
    }
  | Function of {
      publication : Parser.function_publication;
      mutable header : Parser.completed_function_header option;
      mutable body : Ast.function_definition option;
    }

type assigned = {
  publication : Collection.publication;
  ordinal : int;
  source : source;
  mutable claimed : bool;
}

type command = {
  table : Sema.Symbol_table.t;
  ast : Ast.module_;
  declarations : Collection.t;
}

type command_phase =
  | Ready
  | Reading of Parser.command_start
  | Pending of Parser.completed_command
  | Closed
  | Aborted

type parsed_command = {
  receipt : Parser.completed_command;
  mutable sealed : bool;
}

type command_sequence = {
  context : Parser.command_context;
  mutable phase : command_phase;
  mutable completed_rev : parsed_command list;
}

type t = {
  table : Sema.Symbol_table.t;
  sources : Common.Source_manager.t;
  symbols : Visibility.Environment.t;
  namespace : Collection.namespace;
  names : assigned Names.t;
  entries : assigned Entries.t;
  mutable commands : command list;
  mutable next_ordinal : int;
  mutable sequences : command_sequence list;
  mutable active : command_sequence list;
  mutable views : (Ast.module_ * parsed_command list) list;
  mutable sequence_views : (Ast.module_ * Parser.completed_sequence) list;
}

exception Invalid of Common.Diagnostic.t

let fail ?(code = "HCRUN0004") span message =
  raise
    (Invalid
       (Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error
          ~primary:span ~message ()))

let protect run =
  try Ok (run ()) with Invalid diagnostic -> Error [ diagnostic ]

let checked span = function
  | Ok value -> value
  | Error message -> fail span message

let origin (name : Ast.identifier) =
  let location = name.location in
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let create session =
  let table = Session.semantic_symbols session in
  Collection.create_namespace ~table ()
  |> Result.map (fun namespace ->
      {
        table;
        namespace;
        sources = Session.sources session;
        symbols = Session.symbols session;
        names = Names.create 32;
        entries = Entries.create 32;
        commands = [];
        next_ordinal = 0;
        sequences = [];
        active = [];
        views = [];
        sequence_views = [];
      })

let symbol_for ledger entry =
  Entries.find_opt ledger.entries entry
  |> Option.map (fun assigned ->
      Collection.publication_symbol assigned.publication)

let context_span context =
  let source = Parser.context_source context in
  Common.Span.unsafe_make
    ~source:(Common.Source_file.id source)
    ~start:0
    ~stop:(Common.Source_file.length source)

let active_sequence ledger context =
  match ledger.active with
  | sequence :: _ when sequence.context == context -> sequence
  | _ -> fail (context_span context) "parser command context is not active"

let same_option equal left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> equal left right
  | _ -> false

let observe_command ledger event =
  protect (fun () ->
      match event with
      | Parser.Sequence_started context ->
          let source = Parser.context_source context in
          let span = context_span context in
          if
            Parser.context_sources context != ledger.sources
            || Parser.context_environment context != ledger.symbols
            || not
                 (Option.fold ~none:false
                    ~some:(fun registered -> registered == source)
                    (Common.Source_manager.find ledger.sources
                       (Common.Source_file.id source)))
          then
            fail span
              "parser command context has a foreign source or environment";
          if
            List.exists
              (fun sequence -> sequence.context == context)
              ledger.sequences
          then fail span "parser command context was already consumed";
          let parent_context, matches =
            match Parser.context_parent context with
            | None -> (None, fun _ -> false)
            | Some (Parser.Before_first_command parent) ->
                ( Some parent,
                  fun sequence ->
                    sequence.phase = Ready && sequence.completed_rev = [] )
            | Some (Parser.Reading_command start) ->
                ( Some start.command_context,
                  fun sequence ->
                    match sequence.phase with
                    | Reading saved -> saved == start
                    | _ -> false )
            | Some (Parser.Awaiting_resume command) ->
                ( Some command.command_start.command_context,
                  fun sequence ->
                    match sequence.phase with
                    | Pending saved -> saved == command
                    | _ -> false )
          in
          (match (parent_context, ledger.active) with
          | None, [] -> ()
          | Some parent, sequence :: _
            when sequence.context == parent && matches sequence -> ()
          | Some parent, []
            when Parser.context_environment parent != ledger.symbols -> ()
          | _ ->
              fail span
                "nested parser context does not match the suspended parent \
                 phase");
          let sequence = { context; phase = Ready; completed_rev = [] } in
          ledger.sequences <- sequence :: ledger.sequences;
          ledger.active <- sequence :: ledger.active
      | Parser.Command_started start ->
          let sequence = active_sequence ledger start.command_context in
          let previous =
            List.nth_opt sequence.completed_rev 0
            |> Option.map (fun entry -> entry.receipt)
          in
          if
            sequence.phase <> Ready
            || start.command_ordinal <> List.length sequence.completed_rev
            || not (same_option ( == ) start.command_predecessor previous)
          then
            fail
              (context_span start.command_context)
              "parser command start is repeated or out of order";
          sequence.phase <- Reading start
      | Parser.Command_completed receipt ->
          let sequence =
            active_sequence ledger receipt.command_start.command_context
          in
          (match sequence.phase with
          | Reading start when start == receipt.command_start -> ()
          | _ ->
              fail receipt.command_ast.span
                "parser command completion is foreign or out of order");
          let entry = { receipt; sealed = false } in
          sequence.completed_rev <- entry :: sequence.completed_rev;
          sequence.phase <- Pending receipt;
          ledger.views <- (receipt.command_ast, [ entry ]) :: ledger.views
      | Parser.Command_resumed receipt -> (
          let sequence =
            active_sequence ledger receipt.command_start.command_context
          in
          match sequence.phase with
          | Pending saved when saved == receipt -> sequence.phase <- Ready
          | _ ->
              fail receipt.command_ast.span
                "parser command resume is foreign, repeated or out of order")
      | Parser.Sequence_completed receipt ->
          let sequence = active_sequence ledger receipt.sequence_context in
          let entries = List.rev sequence.completed_rev in
          if
            sequence.phase <> Ready
            || List.length entries <> List.length receipt.sequence_commands
            || not
                 (List.for_all2
                    (fun entry command -> entry.receipt == command)
                    entries receipt.sequence_commands)
          then
            fail receipt.sequence_ast.span
              "parser sequence is incomplete or out of order";
          ledger.views <- (receipt.sequence_ast, entries) :: ledger.views;
          ledger.sequence_views <-
            (receipt.sequence_ast, receipt) :: ledger.sequence_views;
          sequence.phase <- Closed;
          ledger.active <- List.tl ledger.active
      | Parser.Sequence_aborted context -> (
          let tentative =
            List.exists
              (fun (_, receipt) ->
                receipt.Parser.sequence_context == context
                && not (Parser.sequence_accepted receipt))
              ledger.sequence_views
          in
          match
            List.find_opt
              (fun sequence -> sequence.context == context)
              ledger.sequences
          with
          | Some sequence when tentative && sequence.phase = Closed ->
              sequence.phase <- Aborted
          | _ ->
              let sequence = active_sequence ledger context in
              sequence.phase <- Aborted;
              ledger.active <- List.tl ledger.active))

let validate_command ledger (header : Parser.declaration_header) =
  let start = header.declaration_command in
  let sequence = active_sequence ledger start.command_context in
  match sequence.phase with
  | Reading saved when saved == start -> ()
  | _ ->
      fail
        (context_span start.command_context)
        "declaration does not belong to the active parser command"

let validate_source ledger environment (header : Parser.declaration_header)
    (name : Ast.identifier) =
  validate_command ledger header;
  if
    environment != ledger.symbols
    || header.declaration_sources != ledger.sources
  then
    fail name.Ast.location.span
      "parser declaration belongs to another source or symbol environment";
  match
    Common.Source_manager.find ledger.sources
      (Common.Source_file.id header.declaration_source)
  with
  | Some source when source == header.declaration_source -> ()
  | _ ->
      fail name.location.span
        "parser declaration input is not the exact registered source"

let assign ledger (name : Ast.identifier) kind source entry =
  if Names.mem ledger.names name || Entries.mem ledger.entries entry then
    fail name.Ast.location.span
      "parser declaration publication was already consumed";
  if ledger.next_ordinal = max_int then
    fail name.location.span "task declaration publication order is exhausted";
  let publication =
    Collection.publish ledger.namespace ~name:name.spelling ~kind
      ~origin:(origin name)
    |> checked name.location.span
  in
  let assigned =
    { publication; source; ordinal = ledger.next_ordinal; claimed = false }
  in
  ledger.next_ordinal <- ledger.next_ordinal + 1;
  Names.add ledger.names name assigned;
  Entries.add ledger.entries entry assigned

let find ledger (name : Ast.identifier) =
  match Names.find_opt ledger.names name with
  | Some assigned when not assigned.claimed -> assigned
  | Some _ ->
      fail name.Ast.location.span
        "parser publication already belongs to a sealed command"
  | None ->
      fail name.location.span
        "source declaration has no assigned parser publication"

let observe ledger event =
  protect (fun () ->
      match event with
      | Parser.Global_declared publication ->
          validate_source ledger publication.global_environment
            publication.global_header publication.global_name;
          assign ledger publication.global_name Sema.Symbol.Global_variable
            (Global { publication; completed = None })
            publication.global_entry
      | Parser.Function_declared publication ->
          validate_source ledger publication.function_environment
            publication.function_header publication.function_name;
          assign ledger publication.function_name Sema.Symbol.Function
            (Function { publication; header = None; body = None })
            publication.function_entry
      | Parser.Global_completed (publication, completed) -> (
          validate_command ledger publication.global_header;
          let assigned = find ledger publication.global_name in
          match assigned.source with
          | Global state
            when state.publication == publication
                 && Option.is_none state.completed ->
              state.completed <- Some completed
          | _ ->
              fail publication.global_name.location.span
                "global completion is foreign, repeated or out of order")
      | Parser.Function_header_completed header -> (
          let publication = header.function_publication in
          validate_command ledger publication.function_header;
          let assigned = find ledger publication.function_name in
          match assigned.source with
          | Function state
            when state.publication == publication && Option.is_none state.header
            ->
              if Entries.mem ledger.entries header.completed_entry then
                fail publication.function_name.location.span
                  "completed parser entry already has a semantic owner";
              state.header <- Some header;
              Entries.add ledger.entries header.completed_entry assigned
          | _ ->
              fail publication.function_name.location.span
                "function header completion is foreign, repeated or out of \
                 order")
      | Parser.Function_body_completed (header, body) -> (
          let publication = header.function_publication in
          validate_command ledger publication.function_header;
          let assigned = find ledger publication.function_name in
          match assigned.source with
          | Function state
            when state.publication == publication
                 && Option.is_none state.body
                 && Option.fold ~none:false
                      ~some:(fun saved -> saved == header)
                      state.header -> state.body <- Some body
          | _ ->
              fail publication.function_name.location.span
                "function body completion is foreign, repeated or out of order"))

let check_global_header (name : Ast.identifier)
    (header : Parser.declaration_header) modifiers binding type_specifier =
  if
    header.modifiers != modifiers
    || (not (same_option ( == ) header.binding binding))
    || header.type_specifier != type_specifier
  then
    fail name.Ast.location.span
      "global command substituted its original declaration header"

let seal ledger (ast : Ast.module_) =
  if
    List.exists
      (fun (view, receipt) ->
        view == ast && not (Parser.sequence_accepted receipt))
      ledger.sequence_views
  then
    protect (fun () ->
        fail ast.span "parser sequence completion was not accepted")
  else
    match List.find_opt (fun command -> command.ast == ast) ledger.commands with
    | Some command -> Ok command
    | None ->
        protect (fun () ->
            let original_commands =
              match
                List.find_opt (fun (view, _) -> view == ast) ledger.views
              with
              | Some (_, entries) -> entries
              | None ->
                  fail ast.span
                    "task AST has no exact parser command or sequence receipt"
            in
            if List.exists (fun entry -> entry.sealed) original_commands then
              fail ast.span "task AST overlaps an already sealed parser command";
            if
              Option.is_none
                (Common.Source_manager.find ledger.sources ast.source)
            then fail ast.span "task command source is not registered";
            let claimed = ref [] in
            let facts = ref [] in
            let previous_ordinal = ref (-1) in
            let add item_index ?declarator_index kind (name : Ast.identifier)
                assigned =
              let header =
                match assigned.source with
                | Global state -> state.publication.global_header
                | Function state -> state.publication.function_header
              in
              if
                not
                  (List.exists
                     (fun entry ->
                       entry.receipt.command_start == header.declaration_command)
                     original_commands)
              then
                fail name.location.span
                  "declaration belongs to a different parser command";
              if
                not
                  (Common.Source_id.equal
                     (Common.Source_file.id header.declaration_source)
                     ast.source)
              then
                fail name.location.span
                  "task command has a different parser input source";
              if List.exists (fun saved -> saved == assigned) !claimed then
                fail name.location.span
                  "task command repeats an assigned declaration";
              if assigned.ordinal <= !previous_ordinal then
                fail name.location.span
                  "task command reorders original declaration publications";
              previous_ordinal := assigned.ordinal;
              let fact =
                Collection.make_declaration ~name:name.spelling
                  ~declaration_kind:kind ~origin:(origin name) ~item_index
                  ?declarator_index ()
                |> checked name.location.span
              in
              claimed := assigned :: !claimed;
              facts := (assigned.publication, fact) :: !facts
            in
            List.iteri
              (fun item_index -> function
                | Ast.Global_variable variable -> (
                    let assigned = find ledger variable.name in
                    match assigned.source with
                    | Global { publication; completed = Some completed } ->
                        check_global_header variable.name
                          publication.global_header variable.modifiers
                          variable.binding variable.type_specifier;
                        if
                          completed.name != variable.name
                          || completed.pointer_layers != variable.pointer_layers
                          || completed.array_dimensions
                             != variable.array_dimensions
                          || Option.is_some completed.global_initial_value
                          || Option.is_some completed.function_pointer
                          || completed.delimiter.kind <> Ast.Semicolon
                          || completed.delimiter.location.span
                             != variable.semicolon
                        then
                          fail variable.location.span
                            "singleton global substituted its completed \
                             declarator";
                        add item_index Collection.Global_variable variable.name
                          assigned
                    | _ ->
                        fail variable.location.span
                          "global command lacks its completed declarator")
                | Ast.Global_declaration declaration ->
                    List.iteri
                      (fun declarator_index (declarator : Ast.global_declarator)
                         ->
                        let assigned = find ledger declarator.name in
                        match assigned.source with
                        | Global { publication; completed = Some completed }
                          when completed == declarator ->
                            check_global_header declarator.name
                              publication.global_header declaration.modifiers
                              declaration.binding declaration.type_specifier;
                            add item_index ~declarator_index
                              Collection.Global_variable declarator.name
                              assigned
                        | _ ->
                            fail declarator.location.span
                              "global command substituted or lacks its \
                               completed declarator")
                      declaration.declarators
                | Ast.Function_prototype prototype -> (
                    let assigned = find ledger prototype.name in
                    match assigned.source with
                    | Function
                        { publication; header = Some header; body = None } ->
                        if
                          (not
                             (same_option ( == )
                                publication.function_header.binding
                                (Some prototype.binding)))
                          || publication.function_header.modifiers
                             != prototype.modifiers
                          || publication.function_header.type_specifier
                             != prototype.return_type
                          || publication.function_pointer_layers
                             != prototype.return_pointer_layers
                          || publication.function_opening_parenthesis
                             != prototype.opening_parenthesis
                          || header.parameters != prototype.parameters
                          || header.empty_parameter_entries
                             != prototype.empty_parameter_entries
                          || (not
                                (same_option ( == ) header.variadic
                                   prototype.variadic))
                          || header.closing_parenthesis
                             != prototype.closing_parenthesis
                        then
                          fail prototype.location.span
                            "prototype command substituted its completed header";
                        add item_index Collection.Function_prototype
                          prototype.name assigned
                    | _ ->
                        fail prototype.location.span
                          "prototype command lacks its completed header")
                | Ast.Function_definition definition -> (
                    let assigned = find ledger definition.name in
                    match assigned.source with
                    | Function { body = Some body; _ } when body == definition
                      ->
                        add item_index Collection.Function_definition
                          definition.name assigned
                    | _ ->
                        fail definition.location.span
                          "function command substituted or lacks its completed \
                           body")
                | Ast.Top_level_statement _ -> ()
                | Ast.Aggregate_forward_declaration _
                | Ast.Aggregate_definition _ ->
                    fail ~code:"HCRUN0001" ast.span
                      "declaration is outside integer program execution")
              ast.items;
            let declarations =
              Collection.view ledger.namespace (List.rev !facts)
              |> checked ast.span
            in
            let command = { table = ledger.table; ast; declarations } in
            List.iter (fun entry -> entry.sealed <- true) original_commands;
            List.iter (fun assigned -> assigned.claimed <- true) !claimed;
            ledger.commands <- command :: ledger.commands;
            command)

let collection ~table ~ast (command : command) =
  protect (fun () ->
      if command.table != table || command.ast != ast then
        fail ast.Ast.span
          "task declaration seal belongs to another table or source AST";
      command.declarations)
