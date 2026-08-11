open Holyc_lib

(* These cases follow TempleOS commit
   c26482bb6ad3f80106d28504ec5db3c6a360732c: Compiler/Lex.HC handles
   KW_HELP_INDEX and KW_HELP_FILE, Compiler/LexLib.HC supplies LexExtStr, and
   Kernel/KHashB.HC attaches source and index metadata. *)

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-help-directive-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let make_directory path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o700

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let config ?templeos_root root =
  match
    Preprocessor.Config.create ~working_directory:root ?templeos_root ()
  with
  | Ok config -> config
  | Error message -> Alcotest.fail message

let preprocess ?templeos_root session root ~path contents =
  let source = Session.add_source session ~path ~contents in
  Holyc_lib.preprocess_detailed session
    ~config:(config ?templeos_root root)
    ~source

let without_eof tokens =
  List.filter (fun token -> token.Token.kind <> Token_kind.Eof) tokens

let raw_tokens output =
  without_eof output.Preprocessor.tokens
  |> List.map (fun token -> token.Token.raw)

let diagnostic_codes output =
  List.map (fun item -> item.Diagnostic.code) output.Preprocessor.diagnostics

let contains_text text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

let index_values metadata =
  Help_metadata.index_events metadata
  |> List.map (fun (entry : Help_metadata.index_entry) -> entry.value)

let records_metadata_in_source_order () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let root_file = Filename.concat root "root.HC" in
      let output =
        preprocess session root ~path:root_file
          "#help_index \"Compiler/Lex\"\n\
           #help_file \"Doc/Lex\"\n\
           #help_index \"\"\n\
           #help_file \"Doc/NoIndex.DD\"\n\
           selected"
      in
      Alcotest.(check (list string))
        "no diagnostics" [] (diagnostic_codes output);
      Alcotest.(check (list string))
        "language token remains" [ "selected" ] (raw_tokens output);
      let metadata = output.help_metadata in
      Alcotest.(check (list string))
        "index history" [ "Compiler/Lex"; "" ] (index_values metadata);
      Alcotest.(check bool)
        "empty index clears current value" true
        (Option.is_none (Help_metadata.current_index metadata));
      let files = Help_metadata.help_files metadata in
      Alcotest.(check int) "two help files" 2 (List.length files);
      let first = List.nth files 0 in
      let second = List.nth files 1 in
      Alcotest.(check string) "declared path" "Doc/Lex" first.declared_path;
      Alcotest.(check string)
        "default extension" "Doc/Lex.DD.Z" first.effective_path;
      Alcotest.(check bool)
        "resolved without target file" true
        (Include_resolver.equal_path first.resolved_path
           (Filename.concat root "Doc/Lex.DD.Z"));
      Alcotest.(check bool)
        "target was not created" false
        (Sys.file_exists (Filename.concat root "Doc/Lex.DD.Z"));
      Alcotest.(check (option string))
        "attached current index" (Some "Compiler/Lex")
        (Option.map
           (fun (entry : Help_metadata.index_entry) -> entry.value)
           first.index);
      Alcotest.(check string)
        "existing extension" "Doc/NoIndex.DD" second.effective_path;
      Alcotest.(check bool)
        "cleared index is not attached" true
        (Option.is_none second.index);
      Alcotest.(check bool)
        "source link line" true
        (String.ends_with ~suffix:",2" first.source_link))

let joins_only_explicit_continuations () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let output =
        preprocess session root
          ~path:(Filename.concat root "root.HC")
          "#help_index \"Debugging/\"\\\n\
           \t\"Dump\"\n\
           #help_index \"First\" \"language\""
      in
      Alcotest.(check (list string))
        "no diagnostics" [] (diagnostic_codes output);
      Alcotest.(check (list string))
        "continued values"
        [ "Debugging/Dump"; "First" ]
        (index_values output.help_metadata);
      let events = Help_metadata.index_events output.help_metadata in
      Alcotest.(check int)
        "two value fragments" 2
        (List.length (List.hd events).provenance.value_spans);
      Alcotest.(check (list string))
        "ordinary adjacent string remains input" [ "\"language\"" ]
        (raw_tokens output))

let maps_templeos_help_paths () =
  with_temp_directory (fun root ->
      let templeos = Filename.concat root "TempleOS" in
      make_directory templeos;
      let session = Session.create () in
      let output =
        preprocess ~templeos_root:templeos session root
          ~path:(Filename.concat root "root.HC")
          "#help_file \"::/Doc/Map\""
      in
      Alcotest.(check (list string))
        "no diagnostics" [] (diagnostic_codes output);
      let file = Help_metadata.help_files output.help_metadata |> List.hd in
      Alcotest.(check string)
        "TempleOS spelling retained" "::/Doc/Map.DD.Z" file.effective_path;
      Alcotest.(check bool)
        "mapped root" true
        (Include_resolver.equal_path file.resolved_path
           (Filename.concat templeos "Doc/Map.DD.Z")))

let include_state_and_provenance () =
  with_temp_directory (fun root ->
      write_file
        (Filename.concat root "child.HC")
        "#help_index \"Child\"\n#help_file \"Doc/Child\"";
      let session = Session.create () in
      let output =
        preprocess session root
          ~path:(Filename.concat root "root.HC")
          "#help_index \"Root\"\n#include \"child\"\n#help_file \"Doc/After\""
      in
      Alcotest.(check (list string))
        "no diagnostics" [] (diagnostic_codes output);
      let files = Help_metadata.help_files output.help_metadata in
      let child = List.nth files 0 in
      let after = List.nth files 1 in
      let attached entry =
        Option.map
          (fun (index : Help_metadata.index_entry) -> index.value)
          entry.Help_metadata.index
      in
      Alcotest.(check (option string))
        "child index" (Some "Child") (attached child);
      Alcotest.(check (option string))
        "included state persists" (Some "Child") (attached after);
      Alcotest.(check int)
        "child include stack" 1
        (List.length child.provenance.include_stack);
      Alcotest.(check int)
        "caller include stack" 0
        (List.length after.provenance.include_stack))

let definition_directives_keep_provenance () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let output =
        preprocess session root
          ~path:(Filename.concat root "root.HC")
          "#define SET_INDEX #help_index \"Generated\"\n\
           #define ADD_FILE #help_file \"Doc/Generated\"\n\
           SET_INDEX\n\
           ADD_FILE"
      in
      Alcotest.(check (list string))
        "no diagnostics" [] (diagnostic_codes output);
      Alcotest.(check (list string))
        "directives emit no tokens" [] (raw_tokens output);
      let index = Help_metadata.index_events output.help_metadata |> List.hd in
      let file = Help_metadata.help_files output.help_metadata |> List.hd in
      Alcotest.(check int)
        "index expansion trace" 2
        (List.length index.provenance.definition_trace);
      Alcotest.(check int)
        "file expansion trace" 2
        (List.length file.provenance.definition_trace);
      Alcotest.(check bool)
        "generated source link retained" true
        (String.starts_with ~prefix:"FL:<definition:" file.source_link))

let inactive_directives_have_no_effect () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let output =
        preprocess session root
          ~path:(Filename.concat root "root.HC")
          "#if 0\n\
           #help_index \"Hidden\"\n\
           #help_file \"Hidden\"\n\
           #endif\n\
           #help_file \"Visible\""
      in
      Alcotest.(check (list string))
        "no index events" []
        (index_values output.help_metadata);
      let files = Help_metadata.help_files output.help_metadata in
      Alcotest.(check int) "one active file" 1 (List.length files);
      Alcotest.(check string)
        "visible file" "Visible" (List.hd files).declared_path;
      Alcotest.(check bool)
        "no hidden index attached" true
        (Option.is_none (List.hd files).index))

let malformed_input_is_retained () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let output =
        preprocess session root
          ~path:(Filename.concat root "root.HC")
          "#help_index name\n#help_file 42\n#help_index \"A\"\\ value\ntail"
      in
      Alcotest.(check (list string))
        "diagnostic order"
        [ "HCPP0028"; "HCPP0028"; "HCPP0029" ]
        (diagnostic_codes output);
      Alcotest.(check (list string))
        "input is not dropped"
        [ "name"; "42"; "value"; "tail" ]
        (raw_tokens output);
      Alcotest.(check (list string))
        "failed updates are not recorded" []
        (index_values output.help_metadata);
      let human =
        Diagnostic_render.human (Session.sources session)
          (List.hd output.diagnostics)
      in
      Alcotest.(check bool)
        "human diagnostic code" true
        (contains_text human "error[HCPP0028]");
      let json =
        Diagnostic_render.json (Session.sources session) output.diagnostics
        |> Yojson.Safe.from_string
      in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "JSON diagnostic code" "HCPP0028"
        (json |> index 0 |> member "code" |> to_string);
      Alcotest.(check int)
        "continuation marker context" 1
        (List.length (List.nth output.diagnostics 2).secondary))

let help_path_diagnostics () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let output =
        preprocess session root
          ~path:(Filename.concat root "root.HC")
          "#help_file \"nul\\0path\"\n\
           #help_file \"../outside\"\n\
           #help_file \"::/Doc/Map\"\n\
           tail"
      in
      Alcotest.(check (list string))
        "path diagnostics"
        [ "HCPP0030"; "HCPP0004"; "HCPP0009" ]
        (diagnostic_codes output);
      Alcotest.(check (list string))
        "following input remains" [ "tail" ] (raw_tokens output);
      Alcotest.(check int)
        "no invalid files" 0
        (Help_metadata.help_files output.help_metadata |> List.length))

let metadata_dump_is_versioned () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let output =
        preprocess session root ~path:"root.HC"
          "#help_index \"Index\"\n#help_file \"Doc/File\""
      in
      let human =
        Help_metadata.human (Session.sources session) output.help_metadata
      in
      Alcotest.(check bool)
        "human schema" true
        (String.starts_with ~prefix:"holyc-help-metadata-v1\n" human);
      Alcotest.(check bool)
        "human index" true
        (String.split_on_char '\n' human |> List.mem "current_index=\"Index\"");
      let json =
        Help_metadata.json (Session.sources session) output.help_metadata
        |> Yojson.Safe.from_string
      in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "JSON schema" "holyc-help-metadata-v1"
        (json |> member "schema" |> to_string);
      Alcotest.(check int)
        "JSON index events" 1
        (json |> member "index_events" |> to_list |> List.length);
      Alcotest.(check int)
        "JSON files" 1
        (json |> member "help_files" |> to_list |> List.length);
      Alcotest.(check string)
        "JSON source link" "FL:root.HC,2"
        (json |> member "help_files" |> index 0 |> member "source_link"
       |> to_string))

let metadata_is_per_stream () =
  with_temp_directory (fun root ->
      let session = Session.create () in
      let first =
        preprocess session root ~path:"first.HC" "#help_index \"First\""
      in
      let second =
        preprocess session root ~path:"second.HC" "#help_file \"Second\""
      in
      Alcotest.(check (list string))
        "first stream index" [ "First" ]
        (index_values first.help_metadata);
      Alcotest.(check (list string))
        "second stream index history" []
        (index_values second.help_metadata);
      let file = Help_metadata.help_files second.help_metadata |> List.hd in
      Alcotest.(check bool)
        "index does not leak between streams" true
        (Option.is_none file.index))

let extension_rule_matches_file_ext_dot () =
  Alcotest.(check string)
    "plain path scan terminates" "Plain.DD.Z"
    (Help_metadata.with_default_extension "Plain");
  Alcotest.(check string)
    "missing extension" "Doc/File.DD.Z"
    (Help_metadata.with_default_extension "Doc/File");
  Alcotest.(check string)
    "existing extension" "Doc/File.DD"
    (Help_metadata.with_default_extension "Doc/File.DD");
  Alcotest.(check string)
    "final dot counts as an extension" "Doc/File."
    (Help_metadata.with_default_extension "Doc/File.");
  Alcotest.(check string)
    "dot in directory counts in TempleOS" "Dir.Name/File"
    (Help_metadata.with_default_extension "Dir.Name/File");
  Alcotest.(check string)
    "empty name receives default" ".DD.Z"
    (Help_metadata.with_default_extension "");
  Alcotest.(check string)
    "FileNameAbs sanitation" "Doc/File .DD.Z"
    (Help_metadata.with_default_extension " \tDoc/\x01File "
    |> Help_metadata.sanitize_file_name);
  Alcotest.(check string)
    "internal whitespace is retained" "Doc/\tFile.DD.Z"
    (Help_metadata.with_default_extension "Doc/\tFile"
    |> Help_metadata.sanitize_file_name)

let raw_lexer_is_unchanged () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"raw.HC"
      ~contents:"#help_index \"Index\" #help_file \"File\""
  in
  let tokens = Holyc_lib.lex session ~source |> Result.get_ok |> without_eof in
  Alcotest.(check (list string))
    "raw directive tokens"
    [ "#"; "help_index"; "\"Index\""; "#"; "help_file"; "\"File\"" ]
    (List.map (fun token -> token.Token.raw) tokens)

let pinned_kernel_c_metadata_counts () =
  let reference_root =
    [ "third_party/TempleOS"; "../third_party/TempleOS" ]
    |> List.find_opt (fun root ->
        Sys.file_exists (Filename.concat root "Kernel/KernelC.HH"))
    |> Option.value ~default:"../third_party/TempleOS"
  in
  let source_path = Filename.concat reference_root "Kernel/KernelC.HH" in
  let session = Session.create () in
  let source =
    match Session.load_source session ~path:source_path with
    | Ok source -> source
    | Error message -> Alcotest.fail message
  in
  let config = config ~templeos_root:reference_root (Sys.getcwd ()) in
  let output = Holyc_lib.preprocess_detailed session ~config ~source in
  Alcotest.(check (list string))
    "pinned file has no preprocessor diagnostics" [] (diagnostic_codes output);
  Alcotest.(check int)
    "all #help_index directives" 104
    (Help_metadata.index_events output.help_metadata |> List.length);
  Alcotest.(check int)
    "all #help_file directives" 20
    (Help_metadata.help_files output.help_metadata |> List.length);
  Alcotest.(check bool)
    "KernelC clears its final index" true
    (Option.is_none (Help_metadata.current_index output.help_metadata))

let tests =
  [
    Alcotest.test_case "source order" `Quick records_metadata_in_source_order;
    Alcotest.test_case "continuation" `Quick joins_only_explicit_continuations;
    Alcotest.test_case "TempleOS path" `Quick maps_templeos_help_paths;
    Alcotest.test_case "include state" `Quick include_state_and_provenance;
    Alcotest.test_case "definition provenance" `Quick
      definition_directives_keep_provenance;
    Alcotest.test_case "inactive" `Quick inactive_directives_have_no_effect;
    Alcotest.test_case "malformed" `Quick malformed_input_is_retained;
    Alcotest.test_case "path diagnostics" `Quick help_path_diagnostics;
    Alcotest.test_case "dumps" `Quick metadata_dump_is_versioned;
    Alcotest.test_case "stream isolation" `Quick metadata_is_per_stream;
    Alcotest.test_case "extension rule" `Quick
      extension_rule_matches_file_ext_dot;
    Alcotest.test_case "raw lexer" `Quick raw_lexer_is_unchanged;
    Alcotest.test_case "pinned KernelC" `Quick pinned_kernel_c_metadata_counts;
  ]
