open Holyc_lib

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-preprocessor-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let make_directory path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o700

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let create_config ?include_roots ?templeos_root ?max_include_depth
    ?max_source_bytes ?max_definition_depth ?max_generated_bytes
    working_directory =
  Preprocessor.Config.create ~working_directory ?include_roots ?templeos_root
    ?max_include_depth ?max_source_bytes ?max_definition_depth
    ?max_generated_bytes ()
  |> function
  | Ok config -> config
  | Error message -> Alcotest.fail message

let preprocess ?include_roots ?templeos_root ?max_include_depth
    ?max_source_bytes ?max_definition_depth ?max_generated_bytes
    working_directory root =
  let session = Session.create () in
  let source =
    Session.load_source session ~path:root |> function
    | Ok source -> source
    | Error message -> Alcotest.fail message
  in
  let config =
    create_config ?include_roots ?templeos_root ?max_include_depth
      ?max_source_bytes ?max_definition_depth ?max_generated_bytes
      working_directory
  in
  (session, source, Holyc_lib.preprocess session ~config ~source)

let without_eof tokens =
  List.filter (fun token -> token.Token.kind <> Token_kind.Eof) tokens

let token_words tokens =
  without_eof tokens
  |> List.map (fun token ->
      match token.Token.value with
      | Token.Text value -> value
      | _ -> Token_kind.name token.kind)

let token_raw tokens =
  without_eof tokens |> List.map (fun token -> token.Token.raw)

let contains_text text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

let drain stream =
  let rec collect tokens diagnostics =
    match Preprocessor.next stream with
    | Lexer.Token token when token.Token.kind = Token_kind.Eof ->
        (List.rev (token :: tokens), List.rev diagnostics)
    | Lexer.Token token -> collect (token :: tokens) diagnostics
    | Lexer.Diagnostic item -> collect tokens (item :: diagnostics)
  in
  collect [] []

let error_with_code expected = function
  | Error diagnostics -> (
      match
        List.find_opt
          (fun item -> String.equal item.Diagnostic.code expected)
          diagnostics
      with
      | Some item -> item
      | None ->
          Alcotest.failf "expected diagnostic %s, got %s" expected
            (String.concat ", "
               (List.map (fun item -> item.Diagnostic.code) diagnostics)))
  | Ok _ -> Alcotest.failf "expected diagnostic %s" expected

let position session token =
  let source =
    Source_manager.find (Session.sources session) token.Token.span.source
    |> Option.get
  in
  Source_file.position source token.span.start |> Result.get_ok

let nested_include_order () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "root_before #include \"one\" root_after";
      write_file
        (Filename.concat root "one.HC")
        "one_before #include \"two\" one_after";
      write_file (Filename.concat root "two.HC") "two";
      let session, source, result = preprocess root root_file in
      let tokens = Result.get_ok result |> without_eof in
      Alcotest.(check (list string))
        "caller resumes after nested source"
        [ "root_before"; "one_before"; "two"; "one_after"; "root_after" ]
        (token_words tokens);
      let source_ids =
        List.map (fun token -> Source_id.to_int token.Token.span.source) tokens
      in
      Alcotest.(check (list int))
        "tokens retain frame source IDs"
        [
          Source_id.to_int (Source_file.id source);
          1;
          2;
          1;
          Source_id.to_int (Source_file.id source);
        ]
        source_ids;
      Alcotest.(check bool)
        "preprocessed token mode" true
        (List.for_all (fun token -> token.Token.mode = Token.Holyc) tokens);
      let two = List.nth tokens 2 in
      let two_position = position session two in
      Alcotest.(check int) "included line" 1 two_position.line;
      Alcotest.(check int) "included column" 1 two_position.column)

let working_directory_not_caller_directory () =
  with_temp_directory (fun root ->
      let subdirectory = Filename.concat root "sub" in
      make_directory subdirectory;
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"sub/first\"";
      write_file (Filename.concat subdirectory "first.HC") "#include \"peer\"";
      write_file (Filename.concat root "peer.HC") "working_directory_peer";
      write_file (Filename.concat subdirectory "peer.HC") "caller_peer";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "relative path uses compiler working directory"
        [ "working_directory_peer" ]
        (Result.get_ok result |> token_words))

let include_root_precedence () =
  with_temp_directory (fun root ->
      let first = Filename.concat root "first" in
      let second = Filename.concat root "second" in
      make_directory first;
      make_directory second;
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"shared\"";
      write_file (Filename.concat first "shared.HC") "from_first";
      write_file (Filename.concat second "shared.HC") "from_second";
      let _, _, result =
        preprocess ~include_roots:[ first; second ] root root_file
      in
      Alcotest.(check (list string))
        "first configured root wins" [ "from_first" ]
        (Result.get_ok result |> token_words))

let extensionless_and_empty_include () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "before #include \"empty\" after";
      write_file (Filename.concat root "empty.HC") "";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "decompressed HC fallback and empty frame" [ "before"; "after" ]
        (Result.get_ok result |> token_words))

let default_extension_precedence () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"choice\"";
      write_file (Filename.concat root "choice.HC.Z") "compressed_name";
      write_file (Filename.concat root "choice.HC") "decompressed_name";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "logical HC.Z name wins" [ "compressed_name" ]
        (Result.get_ok result |> token_words))

let include_at_end_of_file () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "head #include \"tail\"";
      write_file (Filename.concat root "tail.HC") "tail";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "included tokens precede root EOF" [ "head"; "tail" ]
        (Result.get_ok result |> token_words))

let missing_include () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"missing\"";
      let _, _, result = preprocess root root_file in
      let item = error_with_code "HCPP0003" result in
      Alcotest.(check int)
        "both source-name candidates" 2
        (List.length item.Diagnostic.notes))

let directory_include () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let directory = Filename.concat root "source.HC" in
      make_directory directory;
      write_file root_file "#include \"source.HC\"";
      let _, _, result = preprocess root root_file in
      ignore (error_with_code "HCPP0007" result))

let outside_allowed_root () =
  with_temp_directory (fun sandbox ->
      let allowed = Filename.concat sandbox "allowed" in
      make_directory allowed;
      let root_file = Filename.concat allowed "root.HC" in
      write_file root_file "#include \"../outside.HC\"";
      write_file (Filename.concat sandbox "outside.HC") "outside";
      let _, _, result = preprocess allowed root_file in
      ignore (error_with_code "HCPP0004" result))

let self_cycle () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"root.HC\"";
      let _, _, result = preprocess root root_file in
      let item = error_with_code "HCPP0005" result in
      Alcotest.(check int)
        "active source note" 1
        (List.length item.Diagnostic.secondary))

let multi_file_cycle () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"a\"";
      write_file (Filename.concat root "a.HC") "#include \"root.HC\"";
      let _, _, result = preprocess root root_file in
      let item = error_with_code "HCPP0005" result in
      Alcotest.(check int)
        "one active include origin" 1
        (List.length item.Diagnostic.include_stack))

let depth_exhaustion () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"a\"";
      write_file (Filename.concat root "a.HC") "#include \"b\"";
      write_file (Filename.concat root "b.HC") "unreached";
      let _, _, result = preprocess ~max_include_depth:1 root root_file in
      ignore (error_with_code "HCPP0006" result))

let nested_lexer_backtrace () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"a\"";
      write_file (Filename.concat root "a.HC") "#include \"b\"";
      write_file (Filename.concat root "b.HC") "\\";
      let session, _, result = preprocess root root_file in
      let item = error_with_code "HCLEX0001" result in
      Alcotest.(check int)
        "complete include chain" 2
        (List.length item.Diagnostic.include_stack);
      Alcotest.(check (list string))
        "ordered include descriptions"
        [ "#include \"a\""; "#include \"b\"" ]
        (List.map
           (fun (related : Diagnostic.related) -> related.message)
           item.include_stack);
      let rendered = Diagnostic_render.human (Session.sources session) item in
      Alcotest.(check bool)
        "nested display path" true
        (String.starts_with ~prefix:"b:1:1: error[HCLEX0001]" rendered);
      let rendered_include_lines =
        String.split_on_char '\n' rendered
        |> List.filter (String.starts_with ~prefix:"included from ")
      in
      Alcotest.(check int)
        "rendered include chain" 2
        (List.length rendered_include_lines);
      Alcotest.(check bool)
        "rendered outer include" true
        (String.ends_with ~suffix:"#include \"a\""
           (List.nth rendered_include_lines 0));
      Alcotest.(check bool)
        "rendered inner include" true
        (String.ends_with ~suffix:"#include \"b\""
           (List.nth rendered_include_lines 1));
      let json =
        Diagnostic_render.json (Session.sources session) [ item ]
        |> Yojson.Safe.from_string
      in
      let open Yojson.Safe.Util in
      Alcotest.(check int)
        "JSON include chain" 2
        (json |> index 0 |> member "include_stack" |> to_list |> List.length))

let lf_and_crlf_positions () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "first\r\n#include \"inc\"\r\nlast";
      write_file (Filename.concat root "inc.HC") "inside\nsecond";
      let session, _, result = preprocess root root_file in
      let tokens = Result.get_ok result |> without_eof in
      let positions =
        List.map
          (fun token ->
            let item = position session token in
            (item.Source_file.line, item.column))
          tokens
      in
      Alcotest.(check (list (pair int int)))
        "byte positions survive frame switches"
        [ (1, 1); (1, 1); (2, 1); (3, 1) ]
        positions)

let raw_lexer_keeps_directive () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"raw.HC" ~contents:"#include \"item\""
  in
  let tokens = Holyc_lib.lex session ~source |> Result.get_ok |> without_eof in
  Alcotest.(check (list string))
    "raw directive tokens"
    [ "punctuation('#')"; "keyword(include)"; "string" ]
    (List.map (fun token -> Token_kind.name token.Token.kind) tokens)

let raw_lexer_keeps_definition () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"raw.HC" ~contents:"#define NAME value"
  in
  let tokens = Holyc_lib.lex session ~source |> Result.get_ok |> without_eof in
  Alcotest.(check (list string))
    "raw definition tokens"
    [ "#"; "define"; "NAME"; "value" ]
    (List.map (fun token -> token.Token.raw) tokens)

let templeos_root_mapping () =
  with_temp_directory (fun root ->
      let templeos = Filename.concat root "TempleOS" in
      let kernel = Filename.concat templeos "Kernel" in
      make_directory templeos;
      make_directory kernel;
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#include \"/Kernel/Test\" #include \"::/Kernel/Second\"";
      write_file (Filename.concat kernel "Test.HC") "mapped";
      write_file (Filename.concat kernel "Second.HC") "second";
      let _, _, result = preprocess ~templeos_root:templeos root root_file in
      Alcotest.(check (list string))
        "root path mapping" [ "mapped"; "second" ]
        (Result.get_ok result |> token_words))

let directive_diagnostics () =
  with_temp_directory (fun root ->
      let check contents code =
        let root_file = Filename.concat root (code ^ ".HC") in
        write_file root_file contents;
        let _, _, result = preprocess root root_file in
        ignore (error_with_code code result)
      in
      check "#unknown" "HCPP0001";
      check "#include identifier" "HCPP0002";
      check "#include \"\"" "HCPP0002";
      check "#include \"nul\\0path\"" "HCPP0002";
      check "#include <path>" "HCPP0002";
      check "#" "HCPP0001";
      check "#define 42 value" "HCPP0010";
      check "#include \"/Kernel/Test\"" "HCPP0009";
      check "#include \"~/Test\"" "HCPP0009")

let invalid_limits () =
  with_temp_directory (fun root ->
      let invalid result = Result.is_error result in
      Alcotest.(check bool)
        "negative depth" true
        (invalid
           (Preprocessor.Config.create ~working_directory:root
              ~max_include_depth:(-1) ()));
      Alcotest.(check bool)
        "negative byte limit" true
        (invalid
           (Preprocessor.Config.create ~working_directory:root
              ~max_source_bytes:(-1) ()));
      Alcotest.(check bool)
        "negative definition depth" true
        (invalid
           (Preprocessor.Config.create ~working_directory:root
              ~max_definition_depth:(-1) ()));
      Alcotest.(check bool)
        "negative generated byte limit" true
        (invalid
           (Preprocessor.Config.create ~working_directory:root
              ~max_generated_bytes:(-1) ())))

let source_size_limit () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"large\"";
      write_file (Filename.concat root "large.HC") "too large";
      let _, _, result = preprocess ~max_source_bytes:3 root root_file in
      ignore (error_with_code "HCPP0007" result))

let frame_metadata () =
  let session = Session.create () in
  let root = Session.add_source session ~path:"root.HC" ~contents:"root" in
  let child = Session.add_source session ~path:"child.HC" ~contents:"child" in
  let root_frame = Lexer_frame.root ~mode:Token.Holyc root in
  let origin =
    Span.unsafe_make ~source:(Source_file.id root) ~start:0 ~stop:4
  in
  let child_frame =
    Lexer_frame.push_include ~caller:root_frame ~source:child
      ~include_origin:origin ~include_spelling:"child"
  in
  Alcotest.(check int)
    "root source depth" (-1)
    (Lexer_frame.source_depth root_frame);
  Alcotest.(check int)
    "first include source depth" 0
    (Lexer_frame.source_depth child_frame);
  Alcotest.(check int)
    "include stack entry" 1
    (List.length (Lexer_frame.include_stack child_frame));
  Alcotest.(check bool)
    "root frame kind" true
    (Lexer_frame.kind root_frame = Lexer_frame.Root);
  Alcotest.(check bool)
    "included frame kind" true
    (Lexer_frame.kind child_frame = Lexer_frame.Included);
  Alcotest.(check int)
    "frame source ID" 1
    (Lexer_frame.source_id child_frame |> Source_id.to_int);
  ignore (Lexer.next (Lexer_frame.lexer child_frame));
  Alcotest.(check int) "frame cursor" 5 (Lexer_frame.current_offset child_frame);
  let current_position =
    Lexer_frame.current_position child_frame |> Result.get_ok
  in
  Alcotest.(check int) "frame line" 1 current_position.line;
  Alcotest.(check int) "frame column" 6 current_position.column;
  Alcotest.(check string)
    "canonical path" "child.HC"
    (Lexer_frame.canonical_path child_frame);
  Alcotest.(check string)
    "display path" "child.HC"
    (Lexer_frame.display_path child_frame);
  Alcotest.(check bool)
    "active root path" true
    (Option.is_some (Lexer_frame.find_active_path child_frame "root.HC"));
  let environment = Definition.Environment.create () in
  let definition =
    Definition.Environment.define environment ~name:"ITEM" ~replacement:"1"
      ~name_span:origin ~definition_span:origin ~replacement_span:origin
      ~segments:[]
  in
  let generated =
    Session.add_source session ~path:"<definition:ITEM>" ~contents:"1"
  in
  let definition_frame =
    Lexer_frame.push_definition ~caller:child_frame ~source:generated
      ~definition ~invocation_span:origin
  in
  Alcotest.(check bool)
    "definition frame kind" true
    (Lexer_frame.kind definition_frame = Lexer_frame.Definition);
  Alcotest.(check int)
    "definition depth" 1
    (Lexer_frame.definition_depth definition_frame);
  Alcotest.(check int)
    "definition does not add include depth" 0
    (Lexer_frame.source_depth definition_frame);
  Alcotest.(check int)
    "include provenance crosses definition frame" 1
    (List.length (Lexer_frame.include_stack definition_frame));
  Alcotest.(check int)
    "definition provenance" 2
    (List.length (Lexer_frame.definition_trace definition_frame));
  Alcotest.(check bool)
    "active definition" true
    (Option.is_some
       (Lexer_frame.find_active_definition definition_frame
          (Definition.id definition)))

let basic_definition_expansion () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define ANSWER 40 + 2\nbefore ANSWER ANSWER_suffix after";
      let session, source, result = preprocess root root_file in
      let tokens = Result.get_ok result |> without_eof in
      Alcotest.(check (list string))
        "replacement tokens"
        [ "before"; "40"; "+"; "2"; "ANSWER_suffix"; "after" ]
        (List.map (fun token -> token.Token.raw) tokens);
      let generated = List.nth tokens 1 in
      Alcotest.(check bool)
        "generated source ID" true
        (generated.span.source <> Source_file.id source);
      let invocation = Option.get generated.origin.generated_from in
      let declared = Option.get generated.origin.defined_at in
      Alcotest.(check bool)
        "invocation source" true
        (invocation.source = Source_file.id source);
      Alcotest.(check bool)
        "definition source" true
        (declared.source = Source_file.id source);
      let origin_json = Token.to_yojson (Session.sources session) generated in
      let open Yojson.Safe.Util in
      Alcotest.(check bool)
        "JSON invocation provenance" true
        (origin_json |> member "origin" |> member "generated_from" <> `Null);
      Alcotest.(check bool)
        "JSON definition provenance" true
        (origin_json |> member "origin" |> member "defined_at" <> `Null))

let empty_definitions () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define EMPTY\n#define WHITESPACE     \nleft EMPTY WHITESPACE right";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "empty replacement disappears" [ "left"; "right" ]
        (Result.get_ok result |> token_raw))

let replacement_lexical_content () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define MIX \"a//b\" 'xy' 1/*kept as trivia*/+2 // removed\nMIX";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "replacement is lexed as source"
        [ "\"a//b\""; "'xy'"; "1"; "+"; "2" ]
        (Result.get_ok result |> token_raw))

let definition_capture_edges () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define COMMENT // retained in stored text\n\
         #define TRAILING 1  \n\
         COMMENT TRAILING\n\
         #define AT_EOF 7";
      let session, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "comment-only replacement is empty when lexed" [ "1" ]
        (Result.get_ok result |> token_raw);
      let replacements =
        Definition.Environment.all (Session.definitions session)
        |> List.map (fun definition ->
            (Definition.name definition, Definition.replacement definition))
      in
      Alcotest.(check (list (pair string string)))
        "captured replacement bytes"
        [
          ("COMMENT", "// retained in stored text");
          ("TRAILING", "1  ");
          ("AT_EOF", "7");
        ]
        replacements)

let definitions_are_not_c_macros () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#define PICK(x) x+1\nPICK(3)";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "parentheses belong to replacement text"
        [ "("; "x"; ")"; "x"; "+"; "1"; "("; "3"; ")" ]
        (Result.get_ok result |> token_raw))

let continued_definition () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#define LONG 1 + \\\r\n 2\nLONG";
      let session, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "continued replacement" [ "1"; "+"; "2" ]
        (Result.get_ok result |> token_raw);
      let definition =
        Definition.Environment.all (Session.definitions session) |> List.hd
      in
      Alcotest.(check string)
        "continuation bytes are removed" "1 +  2"
        (Definition.replacement definition);
      Alcotest.(check int)
        "segment map splits at continuation" 2
        (List.length (Definition.segments definition)))

let source_order_and_redefinition () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "A #define A 1\nA\n#define A 2\nA\n#define if 3\nif";
      let session, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "latest visible definition wins" [ "A"; "1"; "2"; "3" ]
        (Result.get_ok result |> token_raw);
      Alcotest.(check int)
        "redefinitions remain in history" 3
        (Definition.Environment.all (Session.definitions session) |> List.length))

let definitions_cross_include_boundaries () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define ROOT 7\n\
         #define DEFS_PATH \"defs\"\n\
         #include DEFS_PATH\n\
         FROM_INCLUDE ROOT";
      write_file
        (Filename.concat root "defs.HC")
        "#define FROM_INCLUDE ROOT + 1";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "included definition sees caller environment" [ "7"; "+"; "1"; "7" ]
        (Result.get_ok result |> token_raw))

let definition_injects_directive () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define MAKE_DEFINITION #define MADE 5\nMAKE_DEFINITION\nMADE";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "replacement directive changes shared state" [ "5" ]
        (Result.get_ok result |> token_raw))

let injected_directive_crosses_frame () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define START_DEFINITION #define\nSTART_DEFINITION MADE 6\nMADE";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "unexpanded name resumes in caller" [ "6" ]
        (Result.get_ok result |> token_raw))

let definitions_persist_in_session () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let config = create_config root in
      let first =
        Session.add_source session ~path:"first.HC" ~contents:"#define SHARED 9"
      in
      ignore
        (Holyc_lib.preprocess session ~config ~source:first |> Result.get_ok);
      let second =
        Session.add_source session ~path:"second.HC" ~contents:"SHARED"
      in
      let result = Holyc_lib.preprocess session ~config ~source:second in
      Alcotest.(check (list string))
        "later stream uses session definition" [ "9" ]
        (Result.get_ok result |> token_raw))

let nested_replacement_resumes_caller () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file
        "#define OUTER INNER\n#define INNER 3\nleft OUTER right";
      let _, _, result = preprocess root root_file in
      Alcotest.(check (list string))
        "nested replacement order" [ "left"; "3"; "right" ]
        (Result.get_ok result |> token_raw))

let recursive_definitions () =
  with_temp_directory (fun root ->
      let check file contents =
        let root_file = Filename.concat root file in
        write_file root_file contents;
        let _, _, result = preprocess root root_file in
        error_with_code "HCPP0011" result
      in
      let direct = check "direct.HC" "#define A A\nA" in
      Alcotest.(check int)
        "direct trace" 2
        (List.length direct.Diagnostic.secondary);
      let indirect = check "indirect.HC" "#define A B\n#define B A\nA" in
      Alcotest.(check int)
        "indirect trace" 4
        (List.length indirect.Diagnostic.secondary))

let definition_depth_exhaustion () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#define A B\n#define B C\n#define C 1\nA";
      let _, _, result = preprocess ~max_definition_depth:2 root root_file in
      ignore (error_with_code "HCPP0012" result))

let generated_byte_exhaustion () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#define A 123\nA A";
      let _, _, result = preprocess ~max_generated_bytes:5 root root_file in
      let item = error_with_code "HCPP0013" result in
      Alcotest.(check (list string))
        "budget details"
        [ "bytes already injected: 3"; "replacement bytes: 3" ]
        item.Diagnostic.notes)

let definition_nul_diagnostic () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"nul.HC"
      ~contents:"#define BAD value\x00BAD"
  in
  let config = create_config "." in
  let result = Holyc_lib.preprocess session ~config ~source in
  ignore (error_with_code "HCPP0014" result)

let replacement_diagnostic_provenance () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#define BAD \"unterminated\nBAD";
      let session, _, result = preprocess root root_file in
      let item = error_with_code "HCLEX0003" result in
      Alcotest.(check (list string))
        "definition trace"
        [
          "definition \"BAD\" was expanded here";
          "definition \"BAD\" was declared here";
        ]
        (List.map
           (fun (related : Diagnostic.related) -> related.message)
           item.Diagnostic.secondary);
      let rendered = Diagnostic_render.human (Session.sources session) item in
      Alcotest.(check bool)
        "generated location" true
        (String.starts_with ~prefix:"<definition:00000000:BAD>:1:1" rendered);
      Alcotest.(check bool)
        "invocation note" true
        (contains_text rendered "definition \"BAD\" was expanded here");
      Alcotest.(check bool)
        "declaration note" true
        (contains_text rendered "definition \"BAD\" was declared here");
      let json =
        Diagnostic_render.json (Session.sources session) [ item ]
        |> Yojson.Safe.from_string
      in
      let open Yojson.Safe.Util in
      Alcotest.(check int)
        "JSON definition trace" 2
        (json |> index 0 |> member "secondary" |> to_list |> List.length))

let unterminated_comment_provenance () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#define BAD value /*\nBAD";
      let _, _, result = preprocess root root_file in
      let item = error_with_code "HCLEX0002" result in
      Alcotest.(check int)
        "comment trace" 2
        (List.length item.Diagnostic.secondary))

let deterministic_definition_dump () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"dump.HC"
      ~contents:"#define A 1\n#define A 2\nA"
  in
  let config = create_config "." in
  let stream =
    Preprocessor.create ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~config source
  in
  let _, diagnostics = drain stream in
  Alcotest.(check int) "dump diagnostics" 0 (List.length diagnostics);
  Alcotest.(check string)
    "stable definition dump"
    "holyc-definition-dump-v1\n\
     definition 0 name=\"A\" definition_at=dump.HC:1:1..2:1 \
     name_at=dump.HC:1:9..1:10 replacement_at=dump.HC:1:11..1:12 bytes=\"1\"\n\
    \  segment generated=0..1 source=dump.HC:1:11..1:12\n\
     definition 1 name=\"A\" definition_at=dump.HC:2:1..3:1 \
     name_at=dump.HC:2:9..2:10 replacement_at=dump.HC:2:11..2:12 bytes=\"2\"\n\
    \  segment generated=0..1 source=dump.HC:2:11..2:12\n"
    (Preprocessor.definition_dump stream)

let tests =
  [
    Alcotest.test_case "nested token order" `Quick nested_include_order;
    Alcotest.test_case "working-directory resolution" `Quick
      working_directory_not_caller_directory;
    Alcotest.test_case "include-root precedence" `Quick include_root_precedence;
    Alcotest.test_case "extension and empty include" `Quick
      extensionless_and_empty_include;
    Alcotest.test_case "default extension precedence" `Quick
      default_extension_precedence;
    Alcotest.test_case "include at EOF" `Quick include_at_end_of_file;
    Alcotest.test_case "missing include" `Quick missing_include;
    Alcotest.test_case "directory include" `Quick directory_include;
    Alcotest.test_case "outside allowed root" `Quick outside_allowed_root;
    Alcotest.test_case "self cycle" `Quick self_cycle;
    Alcotest.test_case "multi-file cycle" `Quick multi_file_cycle;
    Alcotest.test_case "depth exhaustion" `Quick depth_exhaustion;
    Alcotest.test_case "nested lexer backtrace" `Quick nested_lexer_backtrace;
    Alcotest.test_case "LF and CRLF positions" `Quick lf_and_crlf_positions;
    Alcotest.test_case "raw lexer directive" `Quick raw_lexer_keeps_directive;
    Alcotest.test_case "raw lexer definition" `Quick raw_lexer_keeps_definition;
    Alcotest.test_case "TempleOS root mapping" `Quick templeos_root_mapping;
    Alcotest.test_case "directive diagnostics" `Quick directive_diagnostics;
    Alcotest.test_case "source size limit" `Quick source_size_limit;
    Alcotest.test_case "invalid limits" `Quick invalid_limits;
    Alcotest.test_case "frame metadata" `Quick frame_metadata;
    Alcotest.test_case "basic definition expansion" `Quick
      basic_definition_expansion;
    Alcotest.test_case "empty definitions" `Quick empty_definitions;
    Alcotest.test_case "replacement lexical content" `Quick
      replacement_lexical_content;
    Alcotest.test_case "definition capture edges" `Quick
      definition_capture_edges;
    Alcotest.test_case "definitions are not C macros" `Quick
      definitions_are_not_c_macros;
    Alcotest.test_case "continued definition" `Quick continued_definition;
    Alcotest.test_case "source order and redefinition" `Quick
      source_order_and_redefinition;
    Alcotest.test_case "definitions across includes" `Quick
      definitions_cross_include_boundaries;
    Alcotest.test_case "definition injects directive" `Quick
      definition_injects_directive;
    Alcotest.test_case "injected directive crosses frame" `Quick
      injected_directive_crosses_frame;
    Alcotest.test_case "session definition persistence" `Quick
      definitions_persist_in_session;
    Alcotest.test_case "nested replacement resumption" `Quick
      nested_replacement_resumes_caller;
    Alcotest.test_case "recursive definitions" `Quick recursive_definitions;
    Alcotest.test_case "definition depth exhaustion" `Quick
      definition_depth_exhaustion;
    Alcotest.test_case "generated byte exhaustion" `Quick
      generated_byte_exhaustion;
    Alcotest.test_case "definition NUL" `Quick definition_nul_diagnostic;
    Alcotest.test_case "replacement diagnostic provenance" `Quick
      replacement_diagnostic_provenance;
    Alcotest.test_case "unterminated comment provenance" `Quick
      unterminated_comment_provenance;
    Alcotest.test_case "deterministic definition dump" `Quick
      deterministic_definition_dump;
  ]
