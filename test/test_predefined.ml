open Holyc_lib

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-predefined-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned path =
  [ "third_party/TempleOS"; "../third_party/TempleOS" ]
  |> List.map (fun root -> Filename.concat root path)
  |> List.find_opt Sys.file_exists
  |> function
  | Some source -> read_file source
  | None -> Alcotest.failf "pinned source is unavailable: %s" path

let create_config ?max_definition_depth ?max_generated_bytes ?predefined_date
    ?predefined_time ?command_line_source working_directory =
  Preprocessor.Config.create ~working_directory ?max_definition_depth
    ?max_generated_bytes ?predefined_date ?predefined_time ?command_line_source
    ()
  |> function
  | Ok config -> config
  | Error message -> Alcotest.fail message

let preprocess ?max_definition_depth ?max_generated_bytes ?predefined_date
    ?predefined_time ?command_line_source root path =
  let session = Session.create () in
  let source =
    Session.load_source session ~path |> function
    | Ok source -> source
    | Error message -> Alcotest.fail message
  in
  let config =
    create_config ?max_definition_depth ?max_generated_bytes ?predefined_date
      ?predefined_time ?command_line_source root
  in
  (session, source, Holyc_lib.preprocess_detailed session ~config ~source)

let without_eof tokens =
  List.filter (fun token -> token.Token.kind <> Token_kind.Eof) tokens

let int_value token =
  match token.Token.value with
  | Token.Int64 value -> value
  | _ -> Alcotest.failf "expected an integer token, got %s" token.raw

let bytes_value token =
  match token.Token.value with
  | Token.Bytes value -> value
  | _ -> Alcotest.failf "expected a string token, got %s" token.raw

let error_with_code code output =
  match
    List.find_opt
      (fun diagnostic -> String.equal diagnostic.Diagnostic.code code)
      output.Preprocessor.diagnostics
  with
  | Some diagnostic -> diagnostic
  | None ->
      Alcotest.failf "expected diagnostic %s, got %s" code
        (String.concat ", "
           (List.map
              (fun diagnostic -> diagnostic.Diagnostic.code)
              output.diagnostics))

let find_substring text needle =
  let rec find index =
    if index + String.length needle > String.length text then None
    else if String.sub text index (String.length needle) = needle then
      Some index
    else find (index + 1)
  in
  find 0

let definition_body source name =
  let marker = "#define " ^ name in
  let start =
    match find_substring source marker with
    | None -> Alcotest.failf "missing pinned definition %s" name
    | Some index -> index + String.length marker
  in
  let rec skip_space index =
    if index < String.length source then
      match source.[index] with
      | ' ' | '\t' -> skip_space (index + 1)
      | _ -> index
    else index
  in
  let buffer = Buffer.create 96 in
  let rec capture index =
    if index < String.length source then (
      match source.[index] with
      | '\\'
        when index + 1 < String.length source
             && Char.equal source.[index + 1] '\n' -> capture (index + 2)
      | '\\'
        when index + 2 < String.length source
             && Char.equal source.[index + 1] '\r'
             && Char.equal source.[index + 2] '\n' -> capture (index + 3)
      | '\r' | '\n' -> Buffer.contents buffer
      | byte ->
          Buffer.add_char buffer byte;
          capture (index + 1))
    else Buffer.contents buffer
  in
  capture (skip_space start)

let settings_validation () =
  let defaults = Predefined.Settings.create () |> Result.get_ok in
  Alcotest.(check string)
    "default date" "01/01/70"
    (Predefined.Settings.date defaults);
  Alcotest.(check string)
    "default time" "00:00:00"
    (Predefined.Settings.time defaults);
  Alcotest.(check bool)
    "default command-line mode" false
    (Predefined.Settings.command_line defaults);
  let invalid result = Result.is_error result in
  Alcotest.(check bool)
    "invalid leap day" true
    (invalid (Predefined.Settings.create ~date:"02/29/01" ()));
  Alcotest.(check bool)
    "invalid date shape" true
    (invalid (Predefined.Settings.create ~date:"2026-08-11" ()));
  Alcotest.(check bool)
    "invalid hour" true
    (invalid (Predefined.Settings.create ~time:"24:00:00" ()));
  Alcotest.(check bool)
    "invalid time shape" true
    (invalid (Predefined.Settings.create ~time:"9:00" ()))

let expands_all_values () =
  with_temp_directory (fun root ->
      let path = Filename.concat root "root.HC" in
      write_file path
        "__DATE__ __TIME__\n__LINE__ __CMD_LINE__ __FILE__ __DIR__";
      let session, source, output =
        preprocess ~predefined_date:"12/31/99" ~predefined_time:"23:59:58"
          ~command_line_source:true root path
      in
      Alcotest.(check bool) "no diagnostics" true (output.diagnostics = []);
      let tokens = without_eof output.tokens in
      Alcotest.(check int) "six values" 6 (List.length tokens);
      Alcotest.(check string)
        "date" "12/31/99"
        (bytes_value (List.nth tokens 0));
      Alcotest.(check string)
        "time" "23:59:58"
        (bytes_value (List.nth tokens 1));
      Alcotest.(check int64) "line" 2L (int_value (List.nth tokens 2));
      Alcotest.(check int64) "command line" 1L (int_value (List.nth tokens 3));
      Alcotest.(check string)
        "file" (Source_file.path source)
        (bytes_value (List.nth tokens 4));
      Alcotest.(check string)
        "directory"
        (Filename.dirname (Source_file.path source))
        (bytes_value (List.nth tokens 5));
      let generated = List.hd tokens in
      Alcotest.(check bool)
        "generated source" true
        (generated.span.source <> Source_file.id source);
      let invocation = Option.get generated.origin.generated_from in
      Alcotest.(check bool)
        "invocation source" true
        (Source_id.equal invocation.source (Source_file.id source));
      Alcotest.(check bool)
        "compiler definition has no source declaration" true
        (Option.is_none generated.origin.defined_at);
      let json = Token.to_yojson (Session.sources session) generated in
      let open Yojson.Safe.Util in
      Alcotest.(check bool)
        "JSON keeps invocation" true
        (json |> member "origin" |> member "generated_from" <> `Null))

let command_line_depth () =
  with_temp_directory (fun root ->
      let root_path = Filename.concat root "root.HC" in
      write_file root_path "__CMD_LINE__ #include \"one.HC\"";
      write_file
        (Filename.concat root "one.HC")
        "__CMD_LINE__ #include \"two.HC\"";
      write_file (Filename.concat root "two.HC") "__CMD_LINE__";
      let _, _, output = preprocess ~command_line_source:true root root_path in
      Alcotest.(check bool) "no diagnostics" true (output.diagnostics = []);
      Alcotest.(check (list int64))
        "root and first include count as command-line source" [ 1L; 1L; 0L ]
        (without_eof output.tokens |> List.map int_value))

let values_across_include_frames () =
  with_temp_directory (fun root ->
      let root_path = Filename.concat root "root.HC" in
      let child_path = Filename.concat root "child.HC" in
      write_file root_path
        "__DATE__ __TIME__ __LINE__ __CMD_LINE__ __FILE__ __DIR__\n\
         #include \"child.HC\"\n\
         __LINE__ __LINE__";
      write_file child_path
        "__DATE__ __TIME__\n__LINE__ __CMD_LINE__ __FILE__ __DIR__";
      let _, root_source, output =
        preprocess ~predefined_date:"08/11/26" ~predefined_time:"05:42:17"
          ~command_line_source:true root root_path
      in
      Alcotest.(check bool) "no diagnostics" true (output.diagnostics = []);
      let tokens = without_eof output.tokens in
      Alcotest.(check int)
        "root, include, and repeat values" 14 (List.length tokens);
      Alcotest.(check string)
        "root date" "08/11/26"
        (bytes_value (List.nth tokens 0));
      Alcotest.(check string)
        "root time" "05:42:17"
        (bytes_value (List.nth tokens 1));
      Alcotest.(check int64) "root line" 1L (int_value (List.nth tokens 2));
      Alcotest.(check int64)
        "root command line" 1L
        (int_value (List.nth tokens 3));
      Alcotest.(check string)
        "root file"
        (Source_file.path root_source)
        (bytes_value (List.nth tokens 4));
      Alcotest.(check string)
        "root directory"
        (Filename.dirname (Source_file.path root_source))
        (bytes_value (List.nth tokens 5));
      Alcotest.(check string)
        "include date" "08/11/26"
        (bytes_value (List.nth tokens 6));
      Alcotest.(check string)
        "include time" "05:42:17"
        (bytes_value (List.nth tokens 7));
      Alcotest.(check int64) "include line" 2L (int_value (List.nth tokens 8));
      Alcotest.(check int64)
        "include command line" 1L
        (int_value (List.nth tokens 9));
      Alcotest.(check string)
        "include file" (Unix.realpath child_path)
        (bytes_value (List.nth tokens 10));
      Alcotest.(check string)
        "include directory"
        (Filename.dirname (Unix.realpath child_path))
        (bytes_value (List.nth tokens 11));
      Alcotest.(check (list int64))
        "repeated root line" [ 3L; 3L ]
        [ int_value (List.nth tokens 12); int_value (List.nth tokens 13) ])

let ordinary_override_and_presence () =
  with_temp_directory (fun root ->
      let path = Filename.concat root "root.HC" in
      write_file path
        "#define __DATE__ \"custom\"\n\
         __DATE__\n\
         #ifdef __TIME__ present #else missing #endif #if defined(__TIME__) \
         wrong #else expanded_value #endif";
      let _, _, output = preprocess root path in
      Alcotest.(check bool) "no diagnostics" true (output.diagnostics = []);
      let tokens = without_eof output.tokens in
      Alcotest.(check (list string))
        "custom definition wins and compiler values are present"
        [ "custom"; "present"; "expanded_value" ]
        (List.map
           (fun token ->
             match token.Token.value with
             | Token.Bytes value | Token.Text value -> value
             | _ -> token.raw)
           tokens);
      let fresh_path = Filename.concat root "fresh.HC" in
      write_file fresh_path "__DATE__";
      let _, _, fresh = preprocess root fresh_path in
      Alcotest.(check string)
        "fresh session restores compiler value" "01/01/70"
        (fresh.tokens |> without_eof |> List.hd |> bytes_value))

let pinned_definitions_remain_dynamic () =
  with_temp_directory (fun root ->
      let kernel = pinned "Kernel/KernelA.HH" in
      let definitions =
        Predefined.all
        |> List.map (fun item ->
            let name = Predefined.spelling item in
            let body = definition_body kernel name in
            Alcotest.(check bool)
              ("pinned body " ^ name) true
              (Predefined.matches_standard_body item body);
            Printf.sprintf "#define %s %s" name body)
      in
      Alcotest.(check bool)
        "separate identifiers do not fuse" false
        (Predefined.matches_standard_body Predefined.Date
           {|#exe{Stream Print("\"%D\"",Now);}|});
      Alcotest.(check bool)
        "separate operators do not fuse" false
        (Predefined.matches_standard_body Predefined.Command_line
           {|#exe{StreamPrint("%d",Fs->last_cc->flags&CCF_CMD_LINE& &Fs->last_cc->lex_include_stk->depth<1);}|});
      let path = Filename.concat root "root.HC" in
      let source =
        String.concat "\n" definitions
        ^ "\n__DATE__ __TIME__ __LINE__ __CMD_LINE__ __FILE__ __DIR__"
      in
      write_file path source;
      let _, root_source, output =
        preprocess ~predefined_date:"08/11/26" ~predefined_time:"05:42:17"
          ~command_line_source:true root path
      in
      Alcotest.(check bool) "no diagnostics" true (output.diagnostics = []);
      let tokens = without_eof output.tokens in
      Alcotest.(check int) "six dynamic values" 6 (List.length tokens);
      Alcotest.(check string)
        "dynamic date" "08/11/26"
        (bytes_value (List.nth tokens 0));
      Alcotest.(check string)
        "dynamic time" "05:42:17"
        (bytes_value (List.nth tokens 1));
      Alcotest.(check int64) "dynamic line" 7L (int_value (List.nth tokens 2));
      Alcotest.(check int64)
        "dynamic command line" 1L
        (int_value (List.nth tokens 3));
      Alcotest.(check string)
        "dynamic file"
        (Source_file.path root_source)
        (bytes_value (List.nth tokens 4));
      Alcotest.(check string)
        "dynamic directory"
        (Filename.dirname (Source_file.path root_source))
        (bytes_value (List.nth tokens 5));
      Alcotest.(check bool)
        "standard definitions keep declaration provenance" true
        (List.for_all
           (fun token -> Option.is_some token.Token.origin.defined_at)
           tokens))

let generated_string_escaping () =
  let session = Session.create () in
  let path = "/tmp/holy\"$$/a\\b.HC" in
  let source = Session.add_source session ~path ~contents:"" in
  let settings = Predefined.Settings.create () |> Result.get_ok in
  let check item expected =
    let expansion =
      Predefined.expand settings item ~source ~line:1 ~source_depth:(-1)
    in
    let generated =
      Session.add_source session ~path:"<generated>" ~contents:expansion
    in
    let tokens = Lexer.lex_all generated |> Result.get_ok |> without_eof in
    Alcotest.(check int) "one string" 1 (List.length tokens);
    Alcotest.(check string)
      (Predefined.spelling item ^ " escaped bytes round trip")
      expected
      (bytes_value (List.hd tokens))
  in
  check Predefined.File path;
  check Predefined.Directory (Filename.dirname path)

let resource_limits () =
  with_temp_directory (fun root ->
      let path = Filename.concat root "root.HC" in
      write_file path "__DATE__";
      let _, _, bytes = preprocess ~max_generated_bytes:1 root path in
      ignore (error_with_code "HCPP0013" bytes);
      let _, _, depth = preprocess ~max_definition_depth:0 root path in
      ignore (error_with_code "HCPP0012" depth))

let deterministic_output () =
  with_temp_directory (fun root ->
      let path = Filename.concat root "root.HC" in
      write_file path "__DATE__ __TIME__ __LINE__ __CMD_LINE__";
      let run () =
        let session, _, output =
          preprocess ~predefined_date:"08/11/26" ~predefined_time:"06:00:00"
            ~command_line_source:true root path
        in
        Token.json (Session.sources session) output.tokens
      in
      Alcotest.(check string) "same JSON" (run ()) (run ()))

let pinned_format_functions () =
  let source = pinned "Kernel/StrPrint.HC" in
  Alcotest.(check bool)
    "date format" true
    (Option.is_some
       (find_substring source
          {|return MStrPrint("%02d/%02d/%02d",ds.mon,ds.day_of_mon,ds.year%100);|}));
  Alcotest.(check bool)
    "time format" true
    (Option.is_some
       (find_substring source
          {|return MStrPrint("%02d:%02d:%02d",ds.hour,ds.min,ds.sec);|}))

let tests =
  [
    Alcotest.test_case "settings validation" `Quick settings_validation;
    Alcotest.test_case "all predefined values" `Quick expands_all_values;
    Alcotest.test_case "command-line source depth" `Quick command_line_depth;
    Alcotest.test_case "values across include frames" `Quick
      values_across_include_frames;
    Alcotest.test_case "ordinary override and presence" `Quick
      ordinary_override_and_presence;
    Alcotest.test_case "pinned definitions stay dynamic" `Quick
      pinned_definitions_remain_dynamic;
    Alcotest.test_case "generated string escaping" `Quick
      generated_string_escaping;
    Alcotest.test_case "resource limits" `Quick resource_limits;
    Alcotest.test_case "deterministic output" `Quick deterministic_output;
    Alcotest.test_case "pinned date and time formats" `Quick
      pinned_format_functions;
  ]
