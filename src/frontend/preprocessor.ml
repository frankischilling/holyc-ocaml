type compilation_mode = Jit | Aot

let compilation_mode_name = function
  | Jit -> "jit"
  | Aot -> "aot"

module Config = struct
  type t = {
    resolver : Include_resolver.t;
    compilation_mode : compilation_mode;
    max_conditional_depth : int;
    max_include_depth : int;
    max_source_bytes : int;
    max_definition_depth : int;
    max_generated_bytes : int;
    max_expression_nodes : int;
  }

  let create ?working_directory ?include_roots ?templeos_root
      ?(compilation_mode = Jit) ?(max_conditional_depth = 64)
      ?(max_include_depth = 64) ?(max_source_bytes = 64 * 1024 * 1024)
      ?(max_definition_depth = 64) ?(max_generated_bytes = 16 * 1024 * 1024)
      ?(max_expression_nodes = 512) () =
    if max_conditional_depth < 0 then
      Error "conditional depth limit must be nonnegative"
    else if max_include_depth < 0 then
      Error "include depth limit must be nonnegative"
    else if max_source_bytes < 0 then
      Error "included source size limit must be nonnegative"
    else if max_definition_depth < 0 then
      Error "definition depth limit must be nonnegative"
    else if max_generated_bytes < 0 then
      Error "generated definition byte limit must be nonnegative"
    else if max_expression_nodes < 0 then
      Error "conditional expression node limit must be nonnegative"
    else
      match
        Include_resolver.create ?working_directory ?include_roots ?templeos_root
          ()
      with
      | Error _ as error -> error
      | Ok resolver ->
          Ok
            {
              resolver;
              compilation_mode;
              max_conditional_depth;
              max_include_depth;
              max_source_bytes;
              max_definition_depth;
              max_generated_bytes;
              max_expression_nodes;
            }

  let resolver config = config.resolver
  let compilation_mode config = config.compilation_mode
  let max_conditional_depth config = config.max_conditional_depth
  let max_include_depth config = config.max_include_depth
  let max_source_bytes config = config.max_source_bytes
  let max_definition_depth config = config.max_definition_depth
  let max_generated_bytes config = config.max_generated_bytes
  let max_expression_nodes config = config.max_expression_nodes
end

type conditional_branch = Then_branch | Else_branch

type conditional = {
  keyword : Keyword.t;
  opener : Common.Span.t;
  parent_active : bool;
  condition : bool;
  mutable branch : conditional_branch;
  mutable valid : bool;
  include_stack : Common.Diagnostic.related list;
  definition_trace : Common.Diagnostic.related list;
}

type t = {
  sources : Common.Source_manager.t;
  definitions : Definition.Environment.t;
  symbols : Symbol_visibility.Environment.t;
  config : Config.t;
  mutable current : Lexer_frame.t;
  mutable lookahead : Token.t option;
  mutable generated_bytes : int;
  mutable conditionals : conditional list;
  mutable conditional_poisoned : bool;
  mutable pending_diagnostics : Common.Diagnostic.t list;
}

let create ~sources ~definitions ~symbols ~config source =
  {
    sources;
    definitions;
    symbols;
    config;
    current = Lexer_frame.root ~mode:Token.Holyc source;
    lookahead = None;
    generated_bytes = 0;
    conditionals = [];
    conditional_poisoned = false;
    pending_diagnostics = [];
  }

let zero_span source =
  Common.Span.unsafe_make
    ~source:(Common.Source_file.id source)
    ~start:0 ~stop:0

let diagnostic stream ?(secondary = []) ?(notes = []) ?help ~code ~message
    primary =
  let secondary = secondary @ Lexer_frame.definition_trace stream.current in
  Common.Diagnostic.make ~secondary
    ~include_stack:(Lexer_frame.include_stack stream.current)
    ~notes ?help ~code ~severity:Common.Diagnostic.Error ~message ~primary ()

let decorate_lexer_diagnostic stream (item : Common.Diagnostic.t) =
  {
    item with
    Common.Diagnostic.include_stack = Lexer_frame.include_stack stream.current;
    secondary = item.secondary @ Lexer_frame.definition_trace stream.current;
  }

let current_active stream =
  if stream.conditional_poisoned then false
  else
    match stream.conditionals with
    | [] -> true
    | conditional :: _ -> (
        conditional.valid && conditional.parent_active
        &&
        match conditional.branch with
        | Then_branch -> conditional.condition
        | Else_branch -> not conditional.condition)

let directive_span hash token =
  if Common.Source_id.equal hash.Token.span.source token.Token.span.source then
    Common.Span.unsafe_make ~source:hash.Token.span.source
      ~start:hash.Token.span.start ~stop:token.Token.span.stop
  else token.Token.span

let push_conditional ?parent_active ?(valid = true) stream hash token keyword
    condition =
  let limit = Config.max_conditional_depth stream.config in
  if List.length stream.conditionals >= limit then (
    stream.conditionals <- [];
    stream.conditional_poisoned <- true;
    Error
      (diagnostic stream ~code:"HCPP0019"
         ~message:
           (Printf.sprintf "#%s would exceed the conditional depth limit of %d"
              (Keyword.spelling keyword) limit)
         ~help:"Raise the conditional depth limit only for trusted source."
         token.Token.span))
  else
    let parent_active =
      Option.value parent_active ~default:(current_active stream)
    in
    let conditional =
      {
        keyword;
        opener = directive_span hash token;
        parent_active;
        condition;
        branch = Then_branch;
        valid;
        include_stack = Lexer_frame.include_stack stream.current;
        definition_trace = Lexer_frame.definition_trace stream.current;
      }
    in
    stream.conditionals <- conditional :: stream.conditionals;
    Ok ()

let opener_related conditional : Common.Diagnostic.related =
  {
    span = conditional.opener;
    message =
      Printf.sprintf "the #%s conditional starts here"
        (Keyword.spelling conditional.keyword);
  }

let conditional_else stream token =
  match stream.conditionals with
  | [] ->
      Error
        (diagnostic stream ~code:"HCPP0015"
           ~message:"found #else without an active conditional"
           ~help:"Remove #else or add its opening conditional." token.Token.span)
  | conditional :: _ -> (
      match conditional.branch with
      | Then_branch ->
          conditional.branch <- Else_branch;
          Ok ()
      | Else_branch ->
          conditional.valid <- false;
          Error
            (diagnostic stream
               ~secondary:[ opener_related conditional ]
               ~code:"HCPP0016" ~message:"this conditional already has an #else"
               ~help:"Keep one #else branch for each conditional."
               token.Token.span))

let conditional_endif stream token =
  match stream.conditionals with
  | [] ->
      Error
        (diagnostic stream ~code:"HCPP0017"
           ~message:"found #endif without an active conditional"
           ~help:"Remove #endif or add its opening conditional."
           token.Token.span)
  | _ :: rest ->
      stream.conditionals <- rest;
      Ok ()

let unterminated_conditional conditional =
  Common.Diagnostic.make ~secondary:conditional.definition_trace
    ~include_stack:conditional.include_stack ~code:"HCPP0018"
    ~severity:Common.Diagnostic.Error
    ~message:
      (Printf.sprintf "#%s has no matching #endif"
         (Keyword.spelling conditional.keyword))
    ~primary:conditional.opener
    ~help:"Add #endif before the end of the preprocessing stream." ()

let finish_conditionals stream =
  match List.rev stream.conditionals with
  | [] -> None
  | first :: rest ->
      stream.conditionals <- [];
      stream.pending_diagnostics <-
        List.map unterminated_conditional rest @ stream.pending_diagnostics;
      Some (unterminated_conditional first)

let token_text token =
  match token.Token.value with
  | Token.Text text | Token.Bytes text -> Some text
  | Token.No_value | Token.Int64 _ | Token.Float64 _ -> None

let path_problem stream primary = function
  | Include_resolver.Empty_path ->
      diagnostic stream ~code:"HCPP0002"
        ~message:"the #include path cannot be empty" primary
  | Include_resolver.Path_contains_nul ->
      diagnostic stream ~code:"HCPP0002"
        ~message:"the #include path contains a NUL byte" primary
  | Include_resolver.Not_found { spelling; searched } ->
      let notes = List.map (Printf.sprintf "searched %s") searched in
      diagnostic stream ~notes ~code:"HCPP0003"
        ~message:(Printf.sprintf "could not find included source %S" spelling)
        primary
  | Include_resolver.Outside_allowed_roots { spelling; resolved; allowed_roots }
    ->
      let notes =
        Printf.sprintf "resolved path: %s" resolved
        :: List.map (Printf.sprintf "allowed root: %s") allowed_roots
      in
      diagnostic stream ~notes ~code:"HCPP0004"
        ~message:
          (Printf.sprintf "included source %S is outside the allowed roots"
             spelling)
        ~help:
          "Add the containing directory as an include root only if the source \
           is trusted."
        primary
  | Include_resolver.Is_directory path ->
      diagnostic stream ~code:"HCPP0007"
        ~message:(Printf.sprintf "included path is a directory: %s" path)
        primary
  | Include_resolver.Not_regular_file path ->
      diagnostic stream ~code:"HCPP0007"
        ~message:(Printf.sprintf "included path is not a regular file: %s" path)
        primary
  | Include_resolver.Io_error { path; message } ->
      diagnostic stream
        ~notes:[ Printf.sprintf "host error: %s" message ]
        ~code:"HCPP0007"
        ~message:(Printf.sprintf "could not inspect included source %s" path)
        primary
  | Include_resolver.Home_path_requires_mapping spelling ->
      diagnostic stream ~code:"HCPP0009"
        ~message:
          (Printf.sprintf
             "TempleOS home path %S has no hosted directory mapping" spelling)
        ~help:"Use a configured include root and a relative include path."
        primary
  | Include_resolver.Templeos_root_requires_mapping spelling ->
      diagnostic stream ~code:"HCPP0009"
        ~message:
          (Printf.sprintf "TempleOS root path %S requires --templeos-root"
             spelling)
        primary
  | Include_resolver.Drive_path_unsupported spelling ->
      diagnostic stream ~code:"HCPP0009"
        ~message:
          (Printf.sprintf "TempleOS drive path %S is not supported on this host"
             spelling)
        ~help:"Map the source tree with --templeos-root and use a root path."
        primary

let cycle_diagnostic stream ~spelling ~canonical_path ~primary active =
  let active_span =
    match Lexer_frame.include_origin active with
    | Some span -> span
    | None -> zero_span (Lexer_frame.source active)
  in
  let related : Common.Diagnostic.related =
    {
      span = active_span;
      message = "this source is already active in the include stack";
    }
  in
  diagnostic stream ~secondary:[ related ]
    ~notes:[ Printf.sprintf "canonical path: %s" canonical_path ]
    ~code:"HCPP0005"
    ~message:(Printf.sprintf "including %S would create a cycle" spelling)
    primary

let depth_diagnostic stream ~spelling ~primary =
  let limit = Config.max_include_depth stream.config in
  diagnostic stream ~code:"HCPP0006"
    ~message:
      (Printf.sprintf "including %S would exceed the include depth limit of %d"
         spelling limit)
    ~help:"Raise the include depth limit only for a trusted source tree."
    primary

let load_diagnostic stream ~spelling ~path ~primary message =
  diagnostic stream
    ~notes:[ Printf.sprintf "host error: %s" message ]
    ~code:"HCPP0007"
    ~message:
      (Printf.sprintf "could not read included source %S at %s" spelling path)
    primary

let push_include stream token spelling =
  let next_depth = Lexer_frame.source_depth stream.current + 1 in
  if next_depth >= Config.max_include_depth stream.config then
    Error (depth_diagnostic stream ~spelling ~primary:token.Token.span)
  else
    match
      Include_resolver.resolve (Config.resolver stream.config) ~spelling
    with
    | Error problem -> Error (path_problem stream token.span problem)
    | Ok resolution -> (
        match
          Lexer_frame.find_active_path stream.current resolution.canonical_path
        with
        | Some active ->
            Error
              (cycle_diagnostic stream ~spelling
                 ~canonical_path:resolution.canonical_path ~primary:token.span
                 active)
        | None -> (
            match
              Common.Source_manager.load
                ~max_bytes:(Config.max_source_bytes stream.config)
                ~display_path:spelling stream.sources
                ~path:resolution.canonical_path
            with
            | Error message ->
                Error
                  (load_diagnostic stream ~spelling
                     ~path:resolution.canonical_path ~primary:token.span message)
            | Ok source ->
                stream.current <-
                  Lexer_frame.push_include ~caller:stream.current ~source
                    ~include_origin:token.span ~include_spelling:spelling;
                Ok ()))

let expected_directive stream token =
  diagnostic stream ~code:"HCPP0001"
    ~message:"expected a preprocessor directive name after #" token.Token.span

let unsupported_directive stream token keyword =
  let spelling = Keyword.spelling keyword in
  diagnostic stream ~code:"HCPP0008"
    ~message:
      (Printf.sprintf
         "#%s is not implemented by the preprocessed token stream yet" spelling)
    token.Token.span

let definition_name token =
  match token.Token.kind with
  | Token_kind.Identifier | Token_kind.Keyword _ -> token_text token
  | _ -> None

let invalid_definition_name stream token =
  diagnostic stream ~code:"HCPP0010"
    ~message:"expected a definition name after #define" token.Token.span

let invalid_symbol_condition_name stream token keyword =
  diagnostic stream ~code:"HCPP0020"
    ~message:
      (Printf.sprintf "expected a symbol name after #%s"
         (Keyword.spelling keyword))
    token.Token.span

let rec next_unexpanded stream =
  match Lexer.next (Lexer_frame.lexer stream.current) with
  | Lexer.Diagnostic item ->
      Lexer.Diagnostic (decorate_lexer_diagnostic stream item)
  | Lexer.Token token when token.Token.kind = Token_kind.Eof -> (
      match Lexer_frame.caller stream.current with
      | None -> Lexer.Token token
      | Some caller ->
          stream.current <- caller;
          next_unexpanded stream)
  | item -> item

let define stream hash =
  match next_unexpanded stream with
  | Lexer.Diagnostic item -> Error item
  | Lexer.Token name_token -> (
      match definition_name name_token with
      | None -> Error (invalid_definition_name stream name_token)
      | Some name -> (
          let lexer = Lexer_frame.lexer stream.current in
          let capture = Lexer.capture_definition_replacement lexer in
          let same_source =
            Common.Source_id.equal hash.Token.span.source name_token.span.source
          in
          let definition_span =
            Common.Span.unsafe_make ~source:name_token.Token.span.source
              ~start:
                (if same_source then hash.Token.span.start
                 else name_token.Token.span.start)
              ~stop:(Lexer.offset lexer)
          in
          match capture.terminator with
          | Lexer.Nul ->
              Error
                (diagnostic stream ~code:"HCPP0014"
                   ~message:"a NUL byte ended the #define replacement"
                   capture.replacement_span)
          | Lexer.End_of_line | Lexer.End_of_file ->
              ignore
                (Definition.Environment.define stream.definitions ~name
                   ~replacement:capture.replacement ~name_span:name_token.span
                   ~definition_span ~replacement_span:capture.replacement_span
                   ~segments:capture.segments);
              Ok ()))

let definition_cycle stream definition invocation =
  diagnostic stream ~code:"HCPP0011"
    ~message:
      (Printf.sprintf "definition %S expands recursively"
         (Definition.name definition))
    ~help:"Change the definition so its active expansion cannot reach itself."
    invocation

let definition_depth stream definition invocation =
  diagnostic stream ~code:"HCPP0012"
    ~message:
      (Printf.sprintf
         "expanding definition %S would exceed the definition depth limit of %d"
         (Definition.name definition)
         (Config.max_definition_depth stream.config))
    ~help:"Raise the definition depth limit only for trusted source." invocation

let generated_bytes stream definition invocation =
  diagnostic stream ~code:"HCPP0013"
    ~message:
      (Printf.sprintf
         "expanding definition %S would exceed the generated byte limit of %d"
         (Definition.name definition)
         (Config.max_generated_bytes stream.config))
    ~notes:
      [
        Printf.sprintf "bytes already injected: %d" stream.generated_bytes;
        Printf.sprintf "replacement bytes: %d"
          (String.length (Definition.replacement definition));
      ]
    ~help:"Raise the generated byte limit only for trusted source." invocation

let push_definition stream token definition =
  let id = Definition.id definition in
  match Lexer_frame.find_active_definition stream.current id with
  | Some _ -> Error (definition_cycle stream definition token.Token.span)
  | None ->
      let next_depth = Lexer_frame.definition_depth stream.current + 1 in
      if next_depth > Config.max_definition_depth stream.config then
        Error (definition_depth stream definition token.span)
      else
        let replacement = Definition.replacement definition in
        let replacement_bytes = String.length replacement in
        let byte_limit = Config.max_generated_bytes stream.config in
        if replacement_bytes > byte_limit - stream.generated_bytes then
          Error (generated_bytes stream definition token.span)
        else
          let logical_path =
            Printf.sprintf "<definition:%08d:%s>" id
              (Definition.name definition)
          in
          let source =
            Common.Source_manager.add_string stream.sources ~path:logical_path
              ~contents:replacement
          in
          stream.generated_bytes <- stream.generated_bytes + replacement_bytes;
          stream.current <-
            Lexer_frame.push_definition ~caller:stream.current ~source
              ~definition ~invocation_span:token.span;
          Ok ()

let expand stream token =
  match definition_name token with
  | None -> Ok false
  | Some name -> (
      match Definition.Environment.find stream.definitions name with
      | None -> Ok false
      | Some definition ->
          Result.map (fun () -> true) (push_definition stream token definition))

let rec next_expanded_source stream =
  match Lexer.next (Lexer_frame.lexer stream.current) with
  | Lexer.Diagnostic item ->
      Lexer.Diagnostic (decorate_lexer_diagnostic stream item)
  | Lexer.Token token -> (
      match token.Token.kind with
      | Token_kind.Eof -> (
          match Lexer_frame.caller stream.current with
          | None -> Lexer.Token token
          | Some caller ->
              stream.current <- caller;
              next_expanded_source stream)
      | Token_kind.Identifier | Token_kind.Keyword _ -> (
          match expand stream token with
          | Ok false -> Lexer.Token token
          | Ok true -> next_expanded_source stream
          | Error item -> Lexer.Diagnostic item)
      | _ -> Lexer.Token token)

let next_expanded stream =
  match stream.lookahead with
  | Some token ->
      stream.lookahead <- None;
      Lexer.Token token
  | None -> next_expanded_source stream

let retain_lookahead stream token =
  match stream.lookahead with
  | None -> stream.lookahead <- Some token
  | Some _ -> invalid_arg "preprocessor already has a retained token"

let include_path stream token =
  match token.Token.kind with
  | Token_kind.String -> (
      match token_text token with
      | Some spelling -> push_include stream token spelling
      | None -> Error (expected_directive stream token))
  | _ ->
      Error
        (diagnostic stream ~code:"HCPP0002"
           ~message:"expected a quoted path after #include" token.Token.span)

let is_conditional_opener = function
  | Keyword.If | Keyword.Ifdef | Keyword.Ifndef | Keyword.Ifaot | Keyword.Ifjit
    -> true
  | _ -> false

let mode_condition stream = function
  | Keyword.Ifaot -> Config.compilation_mode stream.config = Aot
  | Keyword.Ifjit -> Config.compilation_mode stream.config = Jit
  | _ -> false

let symbol_present stream name =
  match Symbol_visibility.Environment.find_preprocessor stream.symbols name with
  | Symbol_visibility.Shadowed_by_local -> false
  | Symbol_visibility.Present _ -> true
  | Symbol_visibility.Absent ->
      Definition.Environment.find stream.definitions name |> Option.is_some

let expression_symbol_defined stream token =
  match definition_name token with
  | None -> false
  | Some name -> (
      match
        Symbol_visibility.Environment.find_preprocessor stream.symbols name
      with
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          true
      | Symbol_visibility.Absent -> false)

let expression_problem stream (problem : Conditional_expression.problem) =
  diagnostic stream ~secondary:problem.secondary ~notes:problem.notes
    ?help:problem.help ~code:problem.code ~message:problem.message
    problem.primary

let reject_conditional stream hash token keyword item =
  Result.bind
    (push_conditional ~parent_active:false ~valid:false stream hash token
       keyword false) (fun () -> Error item)

let enter_symbol_conditional stream hash token keyword =
  match next_unexpanded stream with
  | Lexer.Diagnostic item -> reject_conditional stream hash token keyword item
  | Lexer.Token name_token -> (
      match definition_name name_token with
      | Some name ->
          let present = symbol_present stream name in
          let condition =
            match keyword with
            | Keyword.Ifdef -> present
            | Keyword.Ifndef -> not present
            | _ -> invalid_arg "expected a symbol conditional"
          in
          push_conditional stream hash token keyword condition
      | None ->
          reject_conditional stream hash token keyword
            (invalid_symbol_condition_name stream name_token keyword))

let enter_expression_conditional stream hash token keyword =
  match
    Conditional_expression.parse
      ~opener:(directive_span hash token)
      ~max_nodes:(Config.max_expression_nodes stream.config)
      ~next:(fun () -> next_expanded stream)
      ~symbol_defined:(expression_symbol_defined stream)
      ()
  with
  | Error (Conditional_expression.Lexer_diagnostic item) ->
      reject_conditional stream hash token keyword item
  | Error (Conditional_expression.Problem { problem; lookahead }) ->
      Option.iter (retain_lookahead stream) lookahead;
      reject_conditional stream hash token keyword
        (expression_problem stream problem)
  | Ok parsed -> (
      retain_lookahead stream (Conditional_expression.lookahead parsed);
      match Conditional_expression.evaluate parsed with
      | Ok value ->
          push_conditional stream hash token keyword
            (Conditional_expression.truthy value)
      | Error problem ->
          reject_conditional stream hash token keyword
            (expression_problem stream problem))

let enter_conditional stream ~inactive hash token keyword =
  match keyword with
  | Keyword.Ifaot | Keyword.Ifjit ->
      push_conditional stream hash token keyword (mode_condition stream keyword)
  | Keyword.Ifdef | Keyword.Ifndef ->
      if inactive then push_conditional stream hash token keyword false
      else enter_symbol_conditional stream hash token keyword
  | Keyword.If ->
      if inactive then push_conditional stream hash token keyword false
      else enter_expression_conditional stream hash token keyword
  | _ -> invalid_arg "expected a conditional opener"

let directive stream ~inactive hash =
  match next_expanded stream with
  | Lexer.Diagnostic item -> Error item
  | Lexer.Token token -> (
      match token.Token.kind with
      | Token_kind.Keyword keyword when is_conditional_opener keyword ->
          enter_conditional stream ~inactive hash token keyword
      | Token_kind.Keyword Keyword.Else -> conditional_else stream token
      | Token_kind.Keyword Keyword.Endif -> conditional_endif stream token
      | _ when inactive -> Ok ()
      | Token_kind.Keyword Keyword.Include -> (
          match next_expanded stream with
          | Lexer.Diagnostic item -> Error item
          | Lexer.Token path -> include_path stream path)
      | Token_kind.Keyword Keyword.Define -> define stream hash
      | Token_kind.Keyword keyword ->
          Error (unsupported_directive stream token keyword)
      | Token_kind.Identifier ->
          let spelling = Option.value (token_text token) ~default:token.raw in
          Error
            (diagnostic stream ~code:"HCPP0001"
               ~message:
                 (Printf.sprintf "unknown preprocessor directive #%s" spelling)
               token.span)
      | Token_kind.Eof -> Error (expected_directive stream hash)
      | _ -> Error (expected_directive stream token))

let finish_eof stream token =
  match finish_conditionals stream with
  | None -> Lexer.Token token
  | Some item -> Lexer.Diagnostic item

let rec next stream =
  match stream.pending_diagnostics with
  | item :: rest ->
      stream.pending_diagnostics <- rest;
      Lexer.Diagnostic item
  | [] ->
      if current_active stream then next_active stream else next_inactive stream

and next_active stream =
  match next_expanded stream with
  | Lexer.Token token when token.Token.kind = Token_kind.Punctuation '#' -> (
      match directive stream ~inactive:false token with
      | Ok () -> next stream
      | Error item -> Lexer.Diagnostic item)
  | Lexer.Token token when token.Token.kind = Token_kind.Eof ->
      finish_eof stream token
  | item -> item

and next_inactive stream =
  match stream.lookahead with
  | Some token ->
      stream.lookahead <- None;
      if token.Token.kind = Token_kind.Punctuation '#' then
        if stream.conditional_poisoned then next stream
        else
          match directive stream ~inactive:true token with
          | Ok () -> next stream
          | Error item -> Lexer.Diagnostic item
      else next_inactive stream
  | None -> (
      match
        Lexer.scan_to_directive_marker (Lexer_frame.lexer stream.current)
      with
      | Error item -> Lexer.Diagnostic (decorate_lexer_diagnostic stream item)
      | Ok (Some _) when stream.conditional_poisoned -> next stream
      | Ok (Some hash) -> (
          match directive stream ~inactive:true hash with
          | Ok () -> next stream
          | Error item -> Lexer.Diagnostic item)
      | Ok None -> (
          match Lexer_frame.caller stream.current with
          | Some caller ->
              stream.current <- caller;
              next stream
          | None -> (
              match Lexer.next (Lexer_frame.lexer stream.current) with
              | Lexer.Token token -> finish_eof stream token
              | Lexer.Diagnostic item ->
                  Lexer.Diagnostic (decorate_lexer_diagnostic stream item))))

let definitions stream = Definition.Environment.all stream.definitions

let definition_dump stream =
  Definition.Environment.dump stream.sources stream.definitions

let lex_all ~sources ~definitions ~symbols ~config source =
  let stream = create ~sources ~definitions ~symbols ~config source in
  let rec collect tokens diagnostics =
    match next stream with
    | Lexer.Token token when token.Token.kind = Token_kind.Eof ->
        let tokens = List.rev (token :: tokens) in
        if diagnostics = [] then Ok tokens else Error (List.rev diagnostics)
    | Lexer.Token token -> collect (token :: tokens) diagnostics
    | Lexer.Diagnostic item -> collect tokens (item :: diagnostics)
  in
  collect [] []
