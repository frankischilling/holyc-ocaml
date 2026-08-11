module Config = struct
  type t = {
    resolver : Include_resolver.t;
    max_include_depth : int;
    max_source_bytes : int;
  }

  let create ?working_directory ?include_roots ?templeos_root
      ?(max_include_depth = 64) ?(max_source_bytes = 64 * 1024 * 1024) () =
    if max_include_depth < 0 then
      Error "include depth limit must be nonnegative"
    else if max_source_bytes < 0 then
      Error "included source size limit must be nonnegative"
    else
      match
        Include_resolver.create ?working_directory ?include_roots ?templeos_root
          ()
      with
      | Error _ as error -> error
      | Ok resolver -> Ok { resolver; max_include_depth; max_source_bytes }

  let resolver config = config.resolver
  let max_include_depth config = config.max_include_depth
  let max_source_bytes config = config.max_source_bytes
end

type t = {
  sources : Common.Source_manager.t;
  config : Config.t;
  mutable current : Lexer_frame.t;
}

let create ~sources ~config source =
  { sources; config; current = Lexer_frame.root ~mode:Token.Holyc source }

let zero_span source =
  Common.Span.unsafe_make
    ~source:(Common.Source_file.id source)
    ~start:0 ~stop:0

let diagnostic stream ?(secondary = []) ?(notes = []) ?help ~code ~message
    primary =
  Common.Diagnostic.make ~secondary
    ~include_stack:(Lexer_frame.include_stack stream.current)
    ~notes ?help ~code ~severity:Common.Diagnostic.Error ~message ~primary ()

let decorate_lexer_diagnostic stream item =
  Common.Diagnostic.with_include_stack item
    (Lexer_frame.include_stack stream.current)

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

let directive stream hash =
  match Lexer.next (Lexer_frame.lexer stream.current) with
  | Lexer.Diagnostic item -> Error (decorate_lexer_diagnostic stream item)
  | Lexer.Token token -> (
      match token.Token.kind with
      | Token_kind.Keyword Keyword.Include -> (
          match Lexer.next (Lexer_frame.lexer stream.current) with
          | Lexer.Diagnostic item ->
              Error (decorate_lexer_diagnostic stream item)
          | Lexer.Token path -> include_path stream path)
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

let rec next stream =
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
              next stream)
      | Token_kind.Punctuation '#' -> (
          match directive stream token with
          | Ok () -> next stream
          | Error item -> Lexer.Diagnostic item)
      | _ -> Lexer.Token token)

let lex_all ~sources ~config source =
  let stream = create ~sources ~config source in
  let rec collect tokens diagnostics =
    match next stream with
    | Lexer.Token token when token.Token.kind = Token_kind.Eof ->
        let tokens = List.rev (token :: tokens) in
        if diagnostics = [] then Ok tokens else Error (List.rev diagnostics)
    | Lexer.Token token -> collect (token :: tokens) diagnostics
    | Lexer.Diagnostic item -> collect tokens (item :: diagnostics)
  in
  collect [] []
