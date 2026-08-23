open Holyc_lib
module Validation = Semantic_top_level_statement_validation

let checked = Test_function_call_conversion_policy.checked
let prepare = Test_function_call_conversion_policy.prepare

type prepared = Test_function_call_conversion_policy.prepared

let expect_error = function
  | Ok () -> Alcotest.fail "expected top-level statement validation to fail"
  | Error error -> error

let validate (source : prepared) =
  Holyc_lib.validate_top_level_statements source.ast

let checked_outer = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_outer_environment.error_to_string |> Alcotest.fail

let outer_environment (source : prepared) =
  let make table_kind table_index =
    Semantic_outer_environment.make_table ~table_kind ~table_index []
    |> checked_outer
  in
  let tables =
    match source.mode with
    | Preprocessor.Jit ->
        [
          make (Semantic_outer_environment.Jit_task 0) 0;
          make Semantic_outer_environment.Assembler 1;
        ]
    | Preprocessor.Aot -> [ make Semantic_outer_environment.Assembler 0 ]
  in
  Holyc_lib.create_outer_environment source.session
    ~compilation_mode:source.mode tables
  |> checked

let build (source : prepared) =
  let bindings =
    Holyc_lib.resolve_top_level_expressions source.session
      ~declarations:source.declarations
      ~module_expressions:source.module_expressions source.ast
    |> checked
  in
  let environment = outer_environment source in
  let expressions =
    Holyc_lib.resolve_top_level_outer_expressions source.session ~environment
      ~expressions:bindings
    |> checked
  in
  Holyc_lib.build_top_level_expression_trees source.session
    ~declarations:source.declarations ~compilation_mode:source.mode ~expressions
    source.ast

let check_return_error label error =
  Alcotest.(check string)
    (label ^ " code") "HCSEMA0066"
    (Validation.error_code error);
  Alcotest.(check string)
    (label ^ " message") "explicit return is not allowed outside a function"
    (Validation.error_message error);
  match Validation.error_kind error with
  | Validation.Explicit_return -> ()

let direct_returns_are_rejected_in_both_modes () =
  List.iter
    (fun mode ->
      List.iter
        (fun contents ->
          let source = prepare ~mode ~path:"top-level-return.HC" contents in
          let before =
            Session.semantic_symbols source.session
            |> Semantic_symbol_table.all_symbols |> List.length
          in
          let first = validate source |> expect_error in
          let second = validate source |> expect_error in
          check_return_error "direct return" first;
          Alcotest.(check string)
            "validation replays identically"
            (Validation.error_to_string first)
            (Validation.error_to_string second);
          Alcotest.(check int)
            "validation leaves semantic symbols unchanged" before
            (Session.semantic_symbols source.session
            |> Semantic_symbol_table.all_symbols |> List.length);
          (match Validation.error_origin first with
          | Semantic_symbol.Source_location location ->
              Alcotest.(check int)
                "diagnostic starts at the return keyword" 0 location.span.start
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected a direct source return origin");
          match build source with
          | Ok _ -> Alcotest.fail "expected tree construction to reject return"
          | Error message ->
              Alcotest.(check string)
                "tree construction keeps the validation diagnostic"
                "HCSEMA0066: explicit return is not allowed outside a function"
                message)
        [ "return;"; "return 1;" ];
      let function_source =
        prepare ~mode ~path:"function-return.HC" "I64 F(){return 1;}"
      in
      match validate function_source with
      | Ok () -> ()
      | Error error -> error |> Validation.error_to_string |> Alcotest.fail)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_returns_are_rejected () =
  [
    ("block", "{return;}");
    ("sequence", "1,return 2;");
    ("if branch", "if(1)return;");
    ("else branch", "if(1);else return;");
    ("while body", "while(1)return;");
    ("do body", "do return;while(1);");
    ("for initializer", "for(return;1;);");
    ("for update", "for(;1;return 2);");
    ("for body", "for(;1;)return;");
    ("switch body", "switch(1){case 1:return;}");
    ("subswitch body", "switch(1){start:case 1:return;end:}");
    ("lock body", "lock return;");
    ("try body", "try return;catch;");
    ("catch body", "try;catch return;");
  ]
  |> List.iter (fun (label, contents) ->
      let source = prepare ~path:(label ^ ".HC") contents in
      validate source |> expect_error |> check_return_error label)

let with_included_source contents apply =
  let directory = Filename.temp_dir "holyc-top-level-return-" "" in
  let rec remove_tree path =
    if Sys.file_exists path then
      if Sys.is_directory path then (
        Sys.readdir path
        |> Array.iter (fun entry -> remove_tree (Filename.concat path entry));
        Unix.rmdir path)
      else Sys.remove path
  in
  let write_file path contents =
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel contents)
  in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      write_file root_path "#include \"returns\"";
      write_file (Filename.concat directory "returns.HC") contents;
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let ast =
        Holyc_lib.parse_with_config session
          ~config:
            (Test_function_call_conversion_policy.config
               ~working_directory:directory Preprocessor.Jit)
          ~source
        |> Test_function_call_conversion_policy.expect_ast
      in
      Test_function_call_conversion_policy.finish_prepare Preprocessor.Jit
        session ast
      |> apply)

let provenance_and_ordinary_expression_boundary () =
  with_included_source "return 1;" (fun source ->
      let error = validate source |> expect_error in
      match Validation.error_origin error with
      | Semantic_symbol.Source_location location ->
          let source_file =
            Source_manager.find
              (Session.sources source.session)
              location.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "return keyword keeps its include source" "returns.HC"
            (Source_file.path source_file |> Filename.basename)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected an included return origin");
  with_included_source "#define LEAVE return\nLEAVE 1;" (fun source ->
      let error = validate source |> expect_error in
      match Validation.error_origin error with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "definition-backed return keeps its definition origin" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected a definition-backed return origin");
  let ordinary = prepare ~path:"top-level-final-expression.HC" "1;" in
  (match validate ordinary with
  | Ok () -> ()
  | Error error -> error |> Validation.error_to_string |> Alcotest.fail);
  let _, _, _, typed = Test_top_level_expression_result.analyze ordinary in
  let root =
    typed |> Semantic_function_call_expression_result.top_level_statements
    |> List.hd
    |> Semantic_function_call_expression_result.top_level_statement_roots
    |> List.hd
  in
  Alcotest.(check (option string))
    "ordinary final expression still has discarded-result intent"
    (Some "ICF_RES_NOT_USED")
    (root |> Semantic_function_call_expression_result.top_level_root_result_use
    |> Option.map Semantic_function_call_expression_result.result_use_name)

let tests =
  [
    Alcotest.test_case "direct returns in both modes" `Quick
      direct_returns_are_rejected_in_both_modes;
    Alcotest.test_case "nested returns" `Quick nested_returns_are_rejected;
    Alcotest.test_case "provenance and ordinary expression" `Quick
      provenance_and_ordinary_expression_boundary;
  ]
