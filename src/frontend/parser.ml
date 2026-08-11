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
  mutable lookahead : located_token option;
  mutable diagnostics_rev : Common.Diagnostic.t list;
}

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

let declaration_location type_token pointer_tokens name_token semicolon_token =
  let segments =
    token_segments type_token
    @ List.concat_map token_segments pointer_tokens
    @ token_segments name_token
    @ token_segments semicolon_token
  in
  let all_in_primary_source =
    List.for_all
      (fun segment ->
        Common.Source_id.equal segment.Common.Span.source
          type_token.Token.span.source)
      segments
  in
  let span =
    if all_in_primary_source then
      Common.Span.unsafe_make ~source:type_token.span.source
        ~start:type_token.span.start ~stop:semicolon_token.span.stop
    else type_token.span
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

let parse_global cursor =
  let type_item = take cursor in
  let spelling = token_text type_item.token in
  match
    (type_item.token.Token.kind, Sema.Primitive_type.of_spelling spelling)
  with
  | Token_kind.Identifier, Some primitive -> (
      match parse_pointer_layers cursor 0 [] [] with
      | None -> None
      | Some (pointer_layers, pointer_items) ->
          let pointer_tokens =
            List.map (fun item -> item.token) pointer_items
          in
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
                (Printf.sprintf
                   "expected an identifier after type %S, but found %s"
                   (type_spelling spelling pointer_layers)
                   (token_description name_item.token));
            recover_declaration cursor;
            None)
          else
            let name_item = take cursor in
            let semicolon_item = peek cursor in
            if semicolon_item.token.kind <> Token_kind.Punctuation ';' then (
              report ~secondary:pointer_trace cursor semicolon_item
                ~code:"HCPARSE0003"
                ~message:
                  (Printf.sprintf
                     "expected ';' after global variable %S, but found %s"
                     (token_text name_item.token)
                     (token_description semicolon_item.token));
              recover_declaration cursor;
              None)
            else
              let semicolon_item = take cursor in
              let type_specifier =
                Ast.make_primitive_type ~primitive ~spelling:type_item.token.raw
                  ~location:(token_location type_item.token)
              in
              let name =
                Ast.make_identifier ~spelling:name_item.token.raw
                  ~location:(token_location name_item.token)
              in
              let variable =
                Ast.make_global_variable ~type_specifier ~pointer_layers ~name
                  ~semicolon:semicolon_item.token.span
                  ~location:
                    (declaration_location type_item.token pointer_tokens
                       name_item.token semicolon_item.token)
              in
              ignore
                (Symbol_visibility.Environment.add cursor.symbols
                   ~name:name.spelling ~kind:Symbol_visibility.Global_variable
                   ~origin:(Symbol_visibility.Source_span name.location.span) ());
              Some (Ast.Global_variable variable))
  | _ ->
      report cursor type_item ~code:"HCPARSE0001"
        ~message:
          (Printf.sprintf
             "expected a primitive type at the start of a global declaration, \
              but found %s"
             (token_description type_item.token));
      recover_declaration cursor;
      None

let parse ~sources ~definitions ~symbols ~config source =
  let stream =
    Preprocessor.create ~sources ~definitions ~symbols ~config source
  in
  let cursor = { stream; symbols; lookahead = None; diagnostics_rev = [] } in
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
