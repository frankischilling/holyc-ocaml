type output = {
  ast : Ast.module_ option;
  diagnostics : Common.Diagnostic.t list;
}

type located_token = {
  token : Token.t;
  context : Preprocessor.diagnostic_context;
}

type cursor = {
  stream : Preprocessor.t;
  symbols : Symbol_visibility.Environment.t;
  compilation_mode : Preprocessor.compilation_mode;
  mutable lookahead : located_token option;
  mutable diagnostics_rev : Common.Diagnostic.t list;
}

type parsed_declarator = { node : Ast.global_declarator; tokens : Token.t list }
type parsed_modifier = { node : Ast.declaration_modifier; item : located_token }

type parsed_binding = {
  node : Ast.declaration_binding;
  keyword : located_token;
  tokens : Token.t list;
}

type binding_parse =
  | No_binding
  | Parsed_binding of parsed_binding
  | Bad_binding

let max_pointer_depth = 4

let has_error diagnostics =
  List.exists
    (fun diagnostic ->
      diagnostic.Common.Diagnostic.severity = Common.Diagnostic.Error)
    diagnostics

let has_errors output = has_error output.diagnostics

let rec pull cursor =
  match Preprocessor.next cursor.stream with
  | Lexer.Diagnostic diagnostic ->
      cursor.diagnostics_rev <- diagnostic :: cursor.diagnostics_rev;
      pull cursor
  | Lexer.Token token ->
      { token; context = Preprocessor.diagnostic_context cursor.stream }

let peek cursor =
  match cursor.lookahead with
  | Some item -> item
  | None ->
      let item = pull cursor in
      cursor.lookahead <- Some item;
      item

let take cursor =
  let item = peek cursor in
  cursor.lookahead <- None;
  item

let token_segments token =
  match token.Token.source_segments with
  | [] -> [ token.span ]
  | segments -> segments

let token_location token =
  Ast.make_location ?generated_from:token.Token.origin.generated_from
    ?defined_at:token.origin.defined_at ~span:token.Token.span
    ~source_segments:(token_segments token) ()

let location_from_tokens = function
  | [] -> invalid_arg "a syntax location needs at least one token"
  | first_token :: _ as tokens ->
      let last_token = List.hd (List.rev tokens) in
      let segments = List.concat_map token_segments tokens in
      let all_in_primary_source =
        List.for_all
          (fun segment ->
            Common.Source_id.equal segment.Common.Span.source
              first_token.Token.span.source)
          segments
      in
      let span =
        if all_in_primary_source then
          Common.Span.unsafe_make ~source:first_token.span.source
            ~start:first_token.span.start ~stop:last_token.span.stop
        else first_token.span
      in
      Ast.make_location ~span ~source_segments:segments ()

let token_text token =
  match token.Token.value with
  | Token.Text text -> text
  | _ -> token.raw

let token_description token =
  match token.Token.kind with
  | Token_kind.Eof -> "end of input"
  | Token_kind.Identifier -> Printf.sprintf "identifier %S" (token_text token)
  | _ when String.length token.raw > 0 -> Printf.sprintf "%S" token.raw
  | kind -> Token_kind.name kind

let same_related (left : Common.Diagnostic.related)
    (right : Common.Diagnostic.related) =
  String.equal left.message right.message
  && Common.Span.compare left.span right.span = 0

let append_unique_related items additions =
  List.fold_left
    (fun result item ->
      if List.exists (same_related item) result then result
      else result @ [ item ])
    items additions

let report ?(secondary = []) cursor item ~code ~message =
  let secondary =
    append_unique_related item.context.definition_trace secondary
  in
  let diagnostic =
    Common.Diagnostic.make ~secondary ~include_stack:item.context.include_stack
      ~code ~severity:Common.Diagnostic.Error ~message ~primary:item.token.span
      ()
  in
  cursor.diagnostics_rev <- diagnostic :: cursor.diagnostics_rev

let rec recover_declaration cursor =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Eof -> ()
  | Token_kind.Punctuation ';' -> ignore (take cursor)
  | _ ->
      ignore (take cursor);
      recover_declaration cursor

let rec parse_pointer_layers cursor depth layers_rev items_rev =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Punctuation '*' ->
      if depth = max_pointer_depth then (
        report cursor item ~code:"HCPARSE0004"
          ~message:
            (Printf.sprintf
               "HolyC types may have at most %d pointer stars; this star \
                exceeds that limit"
               max_pointer_depth);
        recover_declaration cursor;
        None)
      else
        let item = take cursor in
        let depth = depth + 1 in
        let layer =
          Ast.make_pointer_layer ~depth ~spelling:item.token.raw
            ~location:(token_location item.token)
        in
        parse_pointer_layers cursor depth (layer :: layers_rev)
          (item :: items_rev)
  | _ -> Some (List.rev layers_rev, List.rev items_rev)

let pointer_definition_trace pointer_items =
  List.fold_left
    (fun trace item ->
      append_unique_related trace item.context.definition_trace)
    [] pointer_items

let type_spelling primitive_spelling pointer_layers =
  primitive_spelling ^ String.make (List.length pointer_layers) '*'

let publish_global cursor (name : Ast.identifier) =
  ignore
    (Symbol_visibility.Environment.add cursor.symbols ~name:name.spelling
       ~kind:Symbol_visibility.Global_variable
       ~origin:(Symbol_visibility.Source_span name.location.span) ())

let delimiter_kind token =
  match token.Token.kind with
  | Token_kind.Punctuation ',' -> Some Ast.Comma
  | Token_kind.Punctuation ';' -> Some Ast.Semicolon
  | _ -> None

let declaration_modifier_kind token =
  match token.Token.kind with
  | Token_kind.Keyword Keyword.Public -> Some Ast.Public
  | Token_kind.Keyword Keyword.Static -> Some Ast.Static
  | _ -> None

let rec parse_modifiers cursor (modifiers_rev : parsed_modifier list) =
  let item = peek cursor in
  match declaration_modifier_kind item.token with
  | None -> List.rev modifiers_rev
  | Some kind ->
      let item = take cursor in
      let node =
        Ast.make_declaration_modifier ~kind ~spelling:item.token.raw
          ~location:(token_location item.token)
      in
      parse_modifiers cursor ({ node; item } :: modifiers_rev)

let declaration_binding_kind token =
  match token.Token.kind with
  | Token_kind.Keyword Keyword.Extern -> Some (Ast.Extern, false)
  | Token_kind.Keyword Keyword.Import -> Some (Ast.Import, false)
  | Token_kind.Keyword Keyword.Underscore_extern -> Some (Ast.Extern, true)
  | Token_kind.Keyword Keyword.Underscore_import -> Some (Ast.Import, true)
  | _ -> None

let parse_binding cursor =
  let item = peek cursor in
  match declaration_binding_kind item.token with
  | None -> No_binding
  | Some (kind, false) ->
      let item = take cursor in
      let node =
        Ast.make_declaration_binding ~kind ~spelling:item.token.raw
          ~location:(token_location item.token)
          ~target:None
      in
      Parsed_binding { node; keyword = item; tokens = [ item.token ] }
  | Some (kind, true) ->
      let keyword = take cursor in
      let target_item = peek cursor in
      if target_item.token.kind <> Token_kind.Identifier then (
        report cursor target_item ~code:"HCPARSE0007"
          ~message:
            (Printf.sprintf
               "expected a target symbol after declaration binding %S, but \
                found %s"
               keyword.token.raw
               (token_description target_item.token));
        recover_declaration cursor;
        Bad_binding)
      else
        let target_item = take cursor in
        let target =
          Ast.make_identifier ~spelling:target_item.token.raw
            ~location:(token_location target_item.token)
        in
        let node =
          Ast.make_declaration_binding ~kind ~spelling:keyword.token.raw
            ~location:(token_location keyword.token)
            ~target:(Some target)
        in
        Parsed_binding
          { node; keyword; tokens = [ keyword.token; target_item.token ] }

let parse_declarator cursor primitive_spelling =
  match parse_pointer_layers cursor 0 [] [] with
  | None -> None
  | Some (pointer_layers, pointer_items) -> (
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let pointer_trace = pointer_definition_trace pointer_items in
      let name_item = peek cursor in
      if name_item.token.kind = Token_kind.Punctuation '(' then (
        report ~secondary:pointer_trace cursor name_item ~code:"HCPARSE0005"
          ~message:
            "parenthesized function-pointer declarators are not implemented";
        recover_declaration cursor;
        None)
      else if name_item.token.kind <> Token_kind.Identifier then (
        report ~secondary:pointer_trace cursor name_item ~code:"HCPARSE0002"
          ~message:
            (Printf.sprintf "expected an identifier after type %S, but found %s"
               (type_spelling primitive_spelling pointer_layers)
               (token_description name_item.token));
        recover_declaration cursor;
        None)
      else
        let name_item = take cursor in
        let delimiter_item = peek cursor in
        match delimiter_kind delimiter_item.token with
        | None ->
            report ~secondary:pointer_trace cursor delimiter_item
              ~code:"HCPARSE0003"
              ~message:
                (Printf.sprintf
                   "expected ';' after global variable %S, but found %s"
                   (token_text name_item.token)
                   (token_description delimiter_item.token));
            recover_declaration cursor;
            None
        | Some kind ->
            let delimiter_item = take cursor in
            let name =
              Ast.make_identifier ~spelling:name_item.token.raw
                ~location:(token_location name_item.token)
            in
            let delimiter =
              Ast.make_declaration_delimiter ~kind
                ~spelling:delimiter_item.token.raw
                ~location:(token_location delimiter_item.token)
            in
            let tokens =
              pointer_tokens @ [ name_item.token; delimiter_item.token ]
            in
            let node =
              Ast.make_global_declarator ~pointer_layers ~name ~delimiter
                ~location:(location_from_tokens tokens)
            in
            publish_global cursor name;
            Some { node; tokens })

let rec parse_declarators cursor primitive_spelling declarators_rev =
  match parse_declarator cursor primitive_spelling with
  | None -> None
  | Some declarator -> (
      let declarators_rev = declarator :: declarators_rev in
      match declarator.node.delimiter.kind with
      | Ast.Semicolon -> Some (List.rev declarators_rev)
      | Ast.Comma -> parse_declarators cursor primitive_spelling declarators_rev
      )

let parse_global cursor =
  let parsed_modifiers = parse_modifiers cursor [] in
  let modifiers =
    List.map
      (fun (modifier : parsed_modifier) -> modifier.node)
      parsed_modifiers
  in
  let modifier_tokens =
    List.map
      (fun (modifier : parsed_modifier) -> modifier.item.token)
      parsed_modifiers
  in
  match parse_binding cursor with
  | Bad_binding -> None
  | Parsed_binding binding
    when binding.node.kind = Ast.Import
         && cursor.compilation_mode = Preprocessor.Jit ->
      report cursor binding.keyword ~code:"HCPARSE0006"
        ~message:
          "import declarations require AOT mode; select AOT mode before \
           parsing this declaration";
      recover_declaration cursor;
      None
  | binding_parse -> (
      let binding, binding_tokens =
        match binding_parse with
        | No_binding -> (None, [])
        | Parsed_binding binding -> (Some binding.node, binding.tokens)
        | Bad_binding -> assert false
      in
      let type_item = take cursor in
      let spelling = token_text type_item.token in
      match
        (type_item.token.Token.kind, Sema.Primitive_type.of_spelling spelling)
      with
      | Token_kind.Identifier, Some primitive -> (
          match parse_declarators cursor spelling [] with
          | None -> None
          | Some declarators -> (
              let type_specifier =
                Ast.make_primitive_type ~primitive ~spelling:type_item.token.raw
                  ~location:(token_location type_item.token)
              in
              let declaration_tokens =
                modifier_tokens @ binding_tokens
                @ type_item.token
                  :: List.concat_map
                       (fun (item : parsed_declarator) -> item.tokens)
                       declarators
              in
              match declarators with
              | [ declarator ] ->
                  let variable =
                    Ast.make_global_variable ~modifiers ~binding ~type_specifier
                      ~pointer_layers:declarator.node.pointer_layers
                      ~name:declarator.node.name
                      ~semicolon:declarator.node.delimiter.location.span
                      ~location:(location_from_tokens declaration_tokens)
                  in
                  Some (Ast.Global_variable variable)
              | _ ->
                  let declaration =
                    Ast.make_global_declaration ~modifiers ~binding
                      ~type_specifier
                      ~declarators:
                        (List.map
                           (fun (item : parsed_declarator) -> item.node)
                           declarators)
                      ~location:(location_from_tokens declaration_tokens)
                  in
                  Some (Ast.Global_declaration declaration)))
      | _ ->
          let prefix =
            match binding with
            | Some (binding : Ast.declaration_binding) ->
                Printf.sprintf "after declaration binding %S" binding.spelling
            | None -> (
                match modifiers with
                | [] -> "at the start of a global declaration"
                | _ ->
                    Printf.sprintf "after declaration modifier%s %S"
                      (if List.length modifiers = 1 then "" else "s")
                      (modifiers
                      |> List.map (fun (modifier : Ast.declaration_modifier) ->
                          modifier.spelling)
                      |> String.concat " "))
          in
          report cursor type_item ~code:"HCPARSE0001"
            ~message:
              (Printf.sprintf "expected a primitive type %s, but found %s"
                 prefix
                 (token_description type_item.token));
          recover_declaration cursor;
          None)

let parse ~sources ~definitions ~symbols ~config source =
  let stream =
    Preprocessor.create ~sources ~definitions ~symbols ~config source
  in
  let cursor =
    {
      stream;
      symbols;
      compilation_mode = Preprocessor.Config.compilation_mode config;
      lookahead = None;
      diagnostics_rev = [];
    }
  in
  let items_rev = ref [] in
  let finished = ref false in
  while not !finished do
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof ->
        ignore (take cursor);
        finished := true
    | _ -> (
        match parse_global cursor with
        | Some item -> items_rev := item :: !items_rev
        | None -> ())
  done;
  let diagnostics = List.rev cursor.diagnostics_rev in
  let ast =
    if has_error diagnostics then None
    else
      let span =
        Common.Span.unsafe_make
          ~source:(Common.Source_file.id source)
          ~start:0
          ~stop:(Common.Source_file.length source)
      in
      Some
        (Ast.make_module
           ~source:(Common.Source_file.id source)
           ~span ~items:(List.rev !items_rev))
  in
  { ast; diagnostics }
