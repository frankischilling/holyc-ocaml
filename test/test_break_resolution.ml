open Holyc_lib
module Breaks = Semantic_break_resolution

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-breaks-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %d diagnostics"
        (List.length diagnostics)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let config ?working_directory mode =
  Preprocessor.Config.create ?working_directory ~compilation_mode:mode ()
  |> checked

let region_number region =
  let id = Breaks.region_id region in
  Breaks.Region_id.to_int id

let quick name run = Alcotest.test_case name `Quick run
let targetless_message = "without an enclosing loop or switch region"
let nested_targets = [ 0; 1; 2; 3; 4; 5; 6; 4 ]

let resolve ?working_directory ~mode ~path source =
  let session = Session.create () in
  let file = Session.add_source session ~path ~contents:source in
  let config = config ?working_directory mode in
  let parsed = parse_with_config session ~config ~source:file in
  let ast = expect_ast parsed in
  let declarations = collect_declarations session ast |> checked in
  let collected = collect_functions session ~declarations ast in
  let functions = checked collected in
  resolve_breaks session ~functions ast

let require_resolution result = result |> checked

let function_named name resolution =
  Breaks.functions resolution
  |> List.find (fun function_ ->
      function_ |> Breaks.function_symbol |> Semantic_symbol.name
      |> String.equal name)

let region_numbers function_ =
  function_ |> Breaks.function_regions |> List.map region_number

let region_kinds function_ =
  function_ |> Breaks.function_regions
  |> List.map (fun region ->
      region |> Breaks.region_kind |> Breaks.region_kind_name)

let target_numbers function_ =
  function_ |> Breaks.function_breaks
  |> List.map (fun occurrence ->
      occurrence |> Breaks.break_target |> Breaks.Region_id.to_int)

let nested_regions_choose_nearest_target () =
  let source =
    "U0 Nested(I64 active){\n\
     while(active){break;for(;active;){break;while(active)break;}}\n\
     do break;while(active);\n\
     switch(active){case 0:break;start:case 1:break;\n\
     while(active)break;end:break;}\n\
     }"
  in
  List.iter
    (fun mode ->
      let resolution =
        let result = resolve ~mode ~path:"nested-breaks.HC" source in
        require_resolution result
      in
      let function_ = function_named "Nested" resolution in
      Alcotest.(check (list int))
        "region identities are contiguous" [ 0; 1; 2; 3; 4; 5; 6 ]
        (region_numbers function_);
      Alcotest.(check (list string))
        "regions retain their structural kind"
        [
          "while";
          "for-body";
          "while";
          "do-while";
          "switch";
          "subswitch";
          "while";
        ]
        (region_kinds function_);
      Alcotest.(check (list int))
        "each break uses the nearest active region" nested_targets
        (target_numbers function_);
      Alcotest.(check (list int))
        "regions retain their break counts" [ 1; 1; 1; 1; 2; 1; 1 ]
        (function_ |> Breaks.function_regions
        |> List.map Breaks.region_break_count))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_statement_routes_keep_target () =
  let source =
    "U0 Routes(I64 active){while(active){\n\
     {break;}if(active)break;else lock break;\n\
     try break;catch break;break,break;\n\
     }}"
  in
  let resolution =
    resolve ~mode:Preprocessor.Jit ~path:"break-routes.HC" source
    |> require_resolution
  in
  let function_ = function_named "Routes" resolution in
  let expected_targets = List.init 7 (Fun.const 0) in
  Alcotest.(check (list int))
    "recursive statement routes keep the loop target" expected_targets
    (target_numbers function_);
  Alcotest.(check int)
    "the target counts every nested break" 7
    (function_ |> Breaks.function_regions |> List.hd
   |> Breaks.region_break_count)

let targetless_breaks_are_rejected () =
  let cases =
    [
      ("top-level", "break;");
      ("function body", "U0 Bad(){break;}");
      ("for initializer", "U0 Bad(I64 x){for(break;x;){}}");
      ("for update", "U0 Bad(I64 x){for(;x;break){}}");
    ]
  in
  List.iter
    (fun (name, source) ->
      let path = name ^ ".HC" in
      match resolve ~mode:Preprocessor.Jit ~path source with
      | Error message ->
          Alcotest.(check bool)
            (name ^ " has the targetless diagnostic")
            true
            (contains message targetless_message)
      | Ok _ -> Alcotest.failf "%s break unexpectedly resolved" name)
    cases

let provenance_and_replay () =
  let source =
    "#define EXIT break\nU0 Generated(I64 active){while(active)EXIT;}"
  in
  let first =
    resolve ~mode:Preprocessor.Jit ~path:"generated-break.HC" source
    |> require_resolution
  in
  let second =
    resolve ~mode:Preprocessor.Jit ~path:"generated-break.HC" source
    |> require_resolution
  in
  Alcotest.(check string)
    "break resolution replays deterministically" (Breaks.human first)
    (Breaks.human second);
  let occurrence =
    let function_ = function_named "Generated" first in
    function_ |> Breaks.function_breaks |> List.hd
  in
  (match Breaks.break_origin occurrence with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "definition-backed break keeps its definition site" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected definition-backed source provenance");
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let include_file = Filename.concat root "included.HC" in
      let root_source = "#include \"included\"" in
      write_file root_file root_source;
      write_file include_file "U0 Included(I64 x){while(x)break;}";
      let resolution =
        let mode = Preprocessor.Aot in
        resolve ~working_directory:root ~mode ~path:root_file root_source
        |> require_resolution
      in
      let occurrence =
        resolution |> function_named "Included" |> Breaks.function_breaks
        |> List.hd
      in
      match Breaks.break_origin occurrence with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "included break keeps source segments" true
            (location.source_segments <> [])
      | Semantic_symbol.Pinned_source _ ->
          Alcotest.fail "expected included source provenance"
      | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected included source provenance")

let constructor_validation () =
  let table = Semantic_symbol_table.create () in
  let root = Semantic_symbol_table.root table in
  let module_scope =
    Semantic_symbol_table.create_scope table ~parent:root
      ~kind:Semantic_symbol_table.Module ~name:"manual.HC" ()
    |> checked
  in
  let symbol =
    Semantic_symbol_table.add table ~scope:module_scope ~name:"Manual"
      ~kind:Semantic_symbol.Function
      ~origin:(Semantic_symbol.Synthesized "manual function")
    |> checked
  in
  let scope =
    Semantic_symbol_table.create_scope table ~parent:module_scope
      ~kind:Semantic_symbol_table.Function ~name:"Manual" ()
    |> checked
  in
  let origin = Semantic_symbol.Synthesized "manual break fact" in
  let region =
    Breaks.make_region ~region_index:0 ~kind:Breaks.While_region ~origin
    |> checked
  in
  let occurrence =
    Breaks.make_break ~occurrence_index:0 ~origin ~target_region_index:1
    |> checked
  in
  let facts =
    Breaks.make_function ~symbol ~scope ~item_index:0 ~regions:[ region ]
      ~breaks:[ occurrence ]
    |> checked
  in
  Alcotest.(check bool)
    "a missing target region is rejected" true
    (Breaks.resolve ~table [ facts ] |> Result.is_error);
  let skipped_region =
    Breaks.make_region ~region_index:1 ~kind:Breaks.While_region ~origin
    |> checked
  in
  Alcotest.(check bool)
    "noncontiguous region identities are rejected" true
    (Breaks.make_function ~symbol ~scope ~item_index:0
       ~regions:[ skipped_region ] ~breaks:[]
    |> Result.is_error)

let tests =
  let routes = nested_statement_routes_keep_target in
  [
    quick "nearest nested target" nested_regions_choose_nearest_target;
    quick "recursive statement routes" routes;
    quick "targetless failures" targetless_breaks_are_rejected;
    quick "provenance and replay" provenance_and_replay;
    quick "constructor validation" constructor_validation;
  ]
