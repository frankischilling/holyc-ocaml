open Holyc_lib
module Source = Holyc_lib__Driver.Integer_source
module Declarations = Task_declarations
module Indexed = Holyc_lib__Driver.Label_resolution
module Region = Holyc_lib__Sema.Exception_region
module Regions = Holyc_lib__Ir.Exception_regions
module Lower = Holyc_lib__Ir.Integer_program_lowering
module Records = Semantic_function_record_classification
module Functions = Semantic_function_resolution
module Headers = Semantic_function_type_resolution
module Frame = Semantic_function_frame_layout
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module Body = Ir_function_body
module Calls = Ir_runtime_call_context
module Globals = Ir_integer_globals
module Initialization = Ir_global_initialization
module X87 = Ir_x87_stack
module Visibility = Symbol_visibility

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let graph_errors errors =
  errors
  |> List.map (fun (error : Graph.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let body_errors errors =
  errors
  |> List.map (fun (error : Body.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let reject ?(code = "HCRUN0004") label = function
  | Ok _ -> Alcotest.failf "%s was accepted" label
  | Error diagnostics ->
      Alcotest.(check bool)
        (label ^ ": " ^ diagnostics_text diagnostics)
        true
        (List.exists
           (fun (error : Diagnostic.t) -> error.code = code)
           diagnostics)

let headers =
  "extern U0 SysTry(U8 *catch_start,U8 *untry);\nextern U0 SysUntry();\n"

type fixture = {
  session : Session.t;
  config : Preprocessor.Config.t;
  source_ast : Ast.module_;
  source_command : Declarations.source_command;
  ast : Ast.module_;
  prepared : Source.prepared;
}

let prepare ?(mode = Preprocessor.Jit) contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"exception-authority.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  let ledger =
    Declarations.create_source session ~source |> require_ok Fun.id
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = Some (Declarations.observe_command ledger);
      query = Some (Declarations.observe_query ledger);
      reference = Some (Declarations.observe_reference ledger);
      implicit_output = None;
      call = None;
      declaration =
        Some
          (fun event ->
            Result.bind (Declarations.observe ledger event) (fun () ->
                match event with
                | Parser.Function_header_completed header ->
                    Declarations.complete_source_defaults ledger header
                | _ -> Ok ()));
      dimension_count = Some (Declarations.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  if Parser.has_errors parsed then
    Alcotest.fail (diagnostics_text parsed.diagnostics);
  let ast = Option.get parsed.ast in
  let source_command =
    Declarations.seal_source ledger ast |> require_ok diagnostics_text
  in
  let prepared =
    Source.prepare_unit ~source_command session ~config ~span:ast.span ast
    |> require_ok diagnostics_text
  in
  {
    session;
    config;
    source_ast = ast;
    source_command;
    ast = Source.semantic_ast prepared;
    prepared;
  }

let records fixture = Source.records fixture.prepared
let table fixture = Session.semantic_symbols fixture.session

let bind_headers fixture records =
  let sources =
    Declarations.source_function_header_sources ~table:(table fixture)
      ~ast:fixture.source_ast fixture.source_command
    |> require_ok diagnostics_text
  in
  let namespace, selected_aggregate =
    Declarations.source_selected_type_resolver ~table:(table fixture)
      ~ast:fixture.source_ast fixture.source_command
    |> require_ok diagnostics_text
  in
  Region.bind_headers ~table:(table fixture) ~namespace ~selected_aggregate
    ~records sources

let rec statement_regions = function
  | Ast.Try_catch_statement source ->
      source
      :: (statement_regions source.try_body
         @ statement_regions source.catch_body)
  | Ast.Block_statement block ->
      List.concat_map statement_regions block.block_statements
  | Ast.Sequence_statement sequence ->
      List.concat_map
        (fun element -> statement_regions element.Ast.sequence_statement)
        sequence.sequence_elements
  | _ -> []

let sources ast =
  List.concat_map
    (function
      | Ast.Top_level_statement statement -> statement_regions statement
      | Ast.Function_definition definition ->
          Option.fold ~none:[] ~some:statement_regions definition.body
      | _ -> [])
    ast.Ast.items

let proof fixture source =
  Region.create ~table:(table fixture) ~records:(records fixture)
    ~ast:fixture.ast ~source ()
  |> require_ok diagnostics_text

let module_with fixture items =
  Ast.make_module ~source:fixture.ast.source ~span:fixture.ast.span ~items

let copy_try source =
  Ast.make_try_catch_statement ~try_keyword:source.Ast.try_keyword
    ~try_body:source.try_body ~catch_keyword:source.catch_keyword
    ~catch_body:source.catch_body ~location:source.try_catch_location

let copy_function definition body =
  Ast.make_function_definition ~modifiers:definition.Ast.modifiers
    ~return_type:definition.return_type
    ~return_pointer_layers:definition.return_pointer_layers
    ~name:definition.name ~opening_parenthesis:definition.opening_parenthesis
    ~parameters:definition.parameters
    ~empty_parameter_entries:definition.empty_parameter_entries
    ~variadic:definition.variadic
    ~closing_parenthesis:definition.closing_parenthesis ~body
    ~location:definition.location

let definition fixture name =
  fixture.ast.items
  |> List.find_map (function
    | Ast.Function_definition definition when definition.name.spelling = name ->
        Some definition
    | _ -> None)
  |> Option.get

let frame fixture name =
  Frame.functions (Source.frames fixture.prepared)
  |> List.find (fun frame ->
      Semantic_symbol.name (Frame.function_symbol frame) = name)

let lower_statement proof =
  let original = Region.source proof in
  Lower.Try
    ( proof,
      Lower.Empty (Ast.statement_location original.try_body).span,
      Lower.Empty (Ast.statement_location original.catch_body).span )

let lower ?frame fixture statements =
  let labels =
    Option.bind frame (fun frame ->
        Indexed.function_for_symbol
          (Source.labels fixture.prepared)
          (Frame.function_symbol frame))
  in
  Lower.lower_complete ?frame ?labels ~records:(records fixture)
    ~span:fixture.ast.span statements

let raw_graph lowered = X87.graph (Lower.graph lowered)

let instructions graph =
  Graph.blocks graph
  |> List.concat_map (fun block ->
      Graph.instructions block |> Sequence.instructions
      |> List.map Sequence.description)

let block_descriptions graph =
  Graph.blocks graph
  |> List.map (fun block ->
      {
        Graph.block_id = Graph.block_id block;
        instructions =
          Graph.instructions block |> Sequence.instructions
          |> List.map Sequence.description;
      })

let rebuild graph blocks =
  Graph.create ~entry:(Graph.block_id (Graph.entry graph)) blocks
  |> require_ok graph_errors

type graph_fixture = {
  source_fixture : fixture;
  lowered : Lower.t;
  context : Regions.t;
  description : Regions.description;
  blocks : Regions.block_context list;
}

let graph_fixture () =
  let source_fixture = prepare (headers ^ "try {} catch {}") in
  let source = proof source_fixture (List.hd (sources source_fixture.ast)) in
  let lowered =
    lower source_fixture [ lower_statement source ]
    |> require_ok diagnostics_text
  in
  let graph = raw_graph lowered in
  let context = Lower.exception_regions lowered in
  let region = List.hd (Regions.regions context) in
  let marker matches =
    instructions graph
    |> List.find (fun instruction ->
        Option.fold ~none:false ~some:matches (Regions.find context instruction))
  in
  let description =
    {
      Regions.source;
      enter =
        marker (function
          | Regions.Enter _ -> true
          | _ -> false);
      leave =
        marker (function
          | Regions.Leave _ -> true
          | _ -> false);
      early_leaves = [];
      handler_return =
        marker (function
          | Regions.Catch_finish _ -> true
          | _ -> false);
      catch_target = Regions.catch_target region;
      untry_target = Regions.untry_target region;
    }
  in
  let blocks =
    Graph.blocks graph
    |> List.map (fun block ->
        let block = Graph.block_id block in
        let scopes =
          if Sequence.Block_id.equal block description.catch_target then
            [ (source, Region.Catch_body) ]
          else if Sequence.Block_id.equal block description.untry_target then
            [ (source, Region.Try_body) ]
          else []
        in
        { Regions.block; scopes })
  in
  ignore
    (Regions.create ~span:source_fixture.ast.span ~graph ~blocks [ description ]
    |> require_ok diagnostics_text);
  { source_fixture; lowered; context; description; blocks }

let seal fixture graph descriptions =
  Regions.create ~span:fixture.source_fixture.ast.span ~graph
    ~blocks:fixture.blocks descriptions

let original_source_and_records () =
  let contents = headers ^ "try {} catch {}" in
  let fixture = prepare contents and foreign = prepare contents in
  let source = List.hd (sources fixture.ast) in
  let original = proof fixture source in
  Alcotest.(check bool)
    "original semantic table" true
    (Region.owns_table original (table fixture));
  Alcotest.(check bool)
    "original record snapshot" true
    (Region.matches_records original (records fixture));
  Alcotest.(check bool)
    "equal foreign records are distinct" false
    (Region.matches_records original (records foreign));
  reject "foreign semantic table"
    (Region.create ~table:(table foreign) ~records:(records fixture)
       ~ast:fixture.ast ~source ());
  reject "foreign equal-source function records"
    (Region.create ~table:(table fixture) ~records:(records foreign)
       ~ast:fixture.ast ~source ());
  let copied = copy_try source in
  Alcotest.(check bool)
    "source copy has a distinct identity" true (copied != source);
  let copied_ast =
    module_with fixture
      [ Ast.Top_level_statement (Ast.Try_catch_statement copied) ]
  in
  reject "reconstructed source has no parser receipt"
    (Region.create ~table:(table fixture) ~records:(records fixture)
       ~ast:copied_ast ~source:copied ());
  reject "original source is absent from the module"
    (Region.create ~table:(table fixture) ~records:(records fixture)
       ~ast:(module_with fixture []) ~source ());
  let duplicate = Ast.Top_level_statement (Ast.Try_catch_statement source) in
  reject "original source appears twice"
    (Region.create ~table:(table fixture) ~records:(records fixture)
       ~ast:(module_with fixture [ duplicate; duplicate ])
       ~source ())

let original_parent_arms () =
  let fixture = prepare (headers ^ "try {try {} catch {}} catch {}") in
  let outer = List.nth (sources fixture.ast) 0 in
  let inner = List.nth (sources fixture.ast) 1 in
  let outer_proof = proof fixture outer and inner_proof = proof fixture inner in
  Alcotest.(check bool)
    "original parent is retained" true
    (match Region.ancestry inner_proof with
    | [ (parent, Region.Try_body) ] -> parent == outer
    | _ -> false);
  let copied_parent = copy_try outer in
  reject "original inner source under a reconstructed parent"
    (Region.create ~table:(table fixture) ~records:(records fixture)
       ~ast:
         (module_with fixture
            [ Ast.Top_level_statement (Ast.Try_catch_statement copied_parent) ])
       ~source:inner ());
  reject "original inner source moved to entry"
    (Region.create ~table:(table fixture) ~records:(records fixture)
       ~ast:
         (module_with fixture
            [ Ast.Top_level_statement (Ast.Try_catch_statement inner) ])
       ~source:inner ());
  let empty = Lower.Empty fixture.ast.span in
  ignore
    (lower fixture
       [ Lower.Try (outer_proof, lower_statement inner_proof, empty) ]
    |> require_ok diagnostics_text);
  reject "nested try moved from protected body to catch"
    (lower fixture
       [ Lower.Try (outer_proof, empty, lower_statement inner_proof) ]);
  reject "one source region lowered twice"
    (lower fixture [ lower_statement outer_proof; lower_statement outer_proof ])

let original_function_owner () =
  let fixture = prepare (headers ^ "U0 F(){try {} catch {}} U0 G(){}") in
  let source = List.hd (sources fixture.ast) in
  let original = proof fixture source in
  let owner = definition fixture "F" in
  let selection = Parser.exception_selection source |> Option.get in
  Alcotest.(check bool)
    "parser retains the completed original definition" true
    (match Parser.exception_function selection with
    | Some (Some definition) -> definition == owner
    | _ -> false);
  ignore
    (lower ~frame:(frame fixture "F") fixture [ lower_statement original ]
    |> require_ok diagnostics_text);
  reject "function source lowered as entry"
    (lower fixture [ lower_statement original ]);
  reject "function source lowered with another original frame"
    (lower ~frame:(frame fixture "G") fixture [ lower_statement original ]);
  let moved =
    copy_function (definition fixture "G")
      (Some (Ast.Try_catch_statement source))
  in
  reject "original try attached to another definition"
    (Region.create ~table:(table fixture) ~records:(records fixture)
       ~ast:(module_with fixture [ Ast.Function_definition moved ])
       ~source ());
  let copied_owner = copy_function owner owner.body in
  reject "rebuilt containing definition cannot reuse original try"
    (Region.create ~table:(table fixture) ~records:(records fixture)
       ~ast:(module_with fixture [ Ast.Function_definition copied_owner ])
       ~source ())

let reconstructed_header_visibility () =
  let fixture = prepare headers in
  let symbols = Visibility.Environment.copy (Session.symbols fixture.session) in
  let original =
    Visibility.Environment.find_function symbols "SysTry" |> Option.get
  in
  let copied =
    Visibility.Environment.add
      ~origin:(Visibility.origin original)
      ?function_call_shape:(Visibility.function_call_shape original)
      symbols ~name:(Visibility.name original) ~kind:Visibility.Function ()
  in
  Alcotest.(check bool)
    "matching header shape is a plain registration" true
    (Visibility.function_alias_original copied = None);
  let source =
    Session.add_source fixture.session ~path:"rebuilt-header.hc"
      ~contents:"try {} catch {}"
  in
  let parsed =
    Parser.parse
      ~sources:(Session.sources fixture.session)
      ~definitions:(Session.definitions fixture.session)
      ~symbols ~config:fixture.config source
  in
  Alcotest.(check bool)
    (diagnostics_text parsed.diagnostics)
    false (Parser.has_errors parsed);
  let ast = Option.get parsed.ast in
  let source = List.hd (sources ast) in
  let selected = Parser.exception_selection source |> Option.get in
  Alcotest.(check bool)
    "parser selected the reconstructed entry" true
    (Option.fold ~none:false ~some:(( == ) copied)
       (Parser.exception_try_header selected));
  reject "same-name origin and call shape confer no source authority"
    (Region.create ~table:(table fixture) ~records:(records fixture) ~ast
       ~source ())

let original_headers_in_both_modes () =
  List.iter
    (fun mode ->
      let fixture = prepare ~mode (headers ^ "U0 F(){try {} catch {}}") in
      let source = proof fixture (List.hd (sources fixture.ast)) in
      let original = Region.try_declaration source in
      Alcotest.(check bool)
        "ordinary checked header retains its journal witness" true
        (List.exists
           (fun record ->
             Records.classified_declaration_source record == original)
           (Records.declarations (records fixture)));
      ignore
        (lower ~frame:(frame fixture "F") fixture [ lower_statement source ]
        |> require_ok diagnostics_text))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let checked_exception_header_abi () =
  let cases =
    [
      ( "SysTry noargpop changes the standard cleanup",
        "noargpop extern U0 SysTry(U8 *catch_start,U8 *untry);extern U0 \
         SysUntry();try {} catch {}" );
      ( "SysTry argpop is an extra calling flag",
        "argpop extern U0 SysTry(U8 *catch_start,U8 *untry);extern U0 \
         SysUntry();try {} catch {}" );
      ( "SysUntry cannot acquire argument cleanup",
        "extern U0 SysTry(U8 *catch_start,U8 *untry);argpop extern U0 \
         SysUntry();try {} catch {}" );
      ( "SysTry body is an ordinary source function",
        "U0 SysTry(U8 *catch_start,U8 *untry){} extern U0 SysUntry();try {} \
         catch {}" );
      ( "SysUntry body is an ordinary source function",
        "extern U0 SysTry(U8 *catch_start,U8 *untry);U0 SysUntry(){} try {} \
         catch {}" );
      ( "a later source body cannot be suppressed by an earlier selection",
        headers ^ "try {} catch {} U0 SysTry(U8 *catch_start,U8 *untry){}" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents) ->
          let fixture = prepare ~mode contents in
          reject label
            (Region.create ~table:(table fixture) ~records:(records fixture)
               ~ast:fixture.ast
               ~source:(List.hd (sources fixture.ast))
               ()))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let reclassify_header fixture target change =
  let originals = Records.declarations (records fixture) in
  let facts =
    List.map
      (fun classified ->
        let declaration = Records.classified_declaration_source classified in
        let site = Functions.resolved_declaration_site declaration in
        let header = Functions.declaration_site_function site in
        let function_ = if header == target then change header else header in
        Functions.make_declaration_with_options
          ~compiler_option_mask:
            (Functions.declaration_site_compiler_option_mask site)
          ~function_
          ~kind:(Functions.declaration_site_source_kind site)
        |> require_ok Fun.id)
      originals
  in
  let parent =
    Headers.function_scope target |> Semantic_symbol_table.parent |> Option.get
  in
  let resolution =
    Functions.resolve ~table:(table fixture) ~parent
      ~compilation_mode:(Records.compilation_mode (records fixture))
      facts
    |> require_ok Fun.id
  in
  Records.classify resolution
    (List.map Records.classified_declaration_state originals)
  |> require_ok Fun.id

let copied_checked_header fixture original ~return_type ~signature =
  let declaration =
    Headers.make_function
      ~symbol:(Headers.function_symbol original)
      ~scope:(Headers.function_scope original)
      ~item_index:(Headers.function_item_index original)
      ~return_type ~signature
      ~parameter_bindings:(Headers.function_parameter_bindings original)
      ~variadic_bindings:(Headers.function_variadic_bindings original)
    |> require_ok Fun.id
  in
  let parent =
    Headers.function_scope original
    |> Semantic_symbol_table.parent |> Option.get
  in
  Headers.resolve ~table:(table fixture) ~parent [ declaration ]
  |> require_ok Fun.id |> Headers.functions |> List.hd

let same_symbol_cannot_replace_header_syntax () =
  List.iter
    (fun mode ->
      let fixture =
        prepare ~mode
          (headers
         ^ "extern U0 SysTry(U8 *other_catch,U8 *other_untry);try {} catch {}")
      in
      let source = List.hd (sources fixture.ast) in
      ignore (proof fixture source);
      let tries =
        Records.declarations (records fixture)
        |> List.filter_map (fun classified ->
            let header =
              Records.classified_declaration_source classified
              |> Functions.resolved_declaration_site
              |> Functions.declaration_site_function
            in
            if Semantic_symbol.name (Headers.function_symbol header) = "SysTry"
            then Some header
            else None)
      in
      let previous = List.nth tries 0 and selected = List.nth tries 1 in
      let reject_change label change =
        let changed = reclassify_header fixture selected change in
        bind_headers fixture changed;
        reject label
          (Region.create ~table:(table fixture) ~records:changed
             ~ast:fixture.ast ~source ())
      in
      reject_change "same symbol cannot borrow an earlier return occurrence"
        (fun original ->
          copied_checked_header fixture original
            ~return_type:(Headers.function_return_type previous)
            ~signature:(Headers.function_signature original));
      reject_change
        "same symbol cannot reconstruct original parameter AST children"
        (fun original ->
          let signature = Headers.function_signature original in
          let parameters =
            List.map
              (fun parameter ->
                let source : Ast.function_parameter =
                  Headers.parameter_source parameter |> Option.get
                in
                let copied_source : Ast.function_parameter =
                  Ast.make_function_parameter
                    ~register_qualifiers:source.register_qualifiers
                    ~type_specifier:source.type_specifier
                    ~pointer_layers:source.pointer_layers ~name:source.name
                    ~function_pointer:source.function_pointer
                    ~default:source.default ~delimiter:source.delimiter
                    ~location:source.location
                in
                Headers.make_parameter ~source:copied_source
                  ~index:(Headers.parameter_index parameter)
                  ~origin:(Headers.parameter_origin parameter)
                  ~register_requests:
                    (Headers.parameter_register_requests parameter)
                  ?name:(Headers.parameter_name parameter)
                  ?name_origin:(Headers.parameter_name_origin parameter)
                  ~type_reference:(Headers.parameter_type_reference parameter)
                  ~declarator_kind:(Headers.parameter_declarator_kind parameter)
                  ~default:(Headers.parameter_default parameter)
                  ?delimiter_origin:
                    (Headers.parameter_delimiter_origin parameter)
                  ()
                |> require_ok Fun.id)
              (Headers.signature_parameters signature)
          in
          let copied =
            Headers.make_signature
              ~opening_origin:(Headers.signature_opening_origin signature)
              ~parameters
              ?closing_origin:(Headers.signature_closing_origin signature)
              ()
            |> require_ok Fun.id
          in
          copied_checked_header fixture original
            ~return_type:(Headers.function_return_type original)
            ~signature:copied))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let retained_task_headers () =
  let session = Session.create () in
  let task = Integer_task.create session |> require_ok Fun.id in
  let run path contents =
    let source = Session.add_source session ~path ~contents in
    Integer_task.run task ~source |> Test_integer_program.checked
  in
  ignore (run "exception-headers.hc" headers);
  let first = run "first-retained-try.hc" "I64 N=40;try {N++;}catch {N=0;}N;" in
  let second = run "second-retained-try.hc" "try {N++;}catch {N=0;}N;" in
  let bits execution =
    Ir_integer_interpreter.final_value execution |> Option.get |> fun word ->
    word.Ir_integer_interpreter.bits
  in
  Alcotest.(check int64)
    "first input uses original retained headers" 41L (bits first);
  Alcotest.(check int64)
    "later input retains the same header authority" 42L (bits second)

let marker_identity_and_reconstructed_metadata () =
  let fixture = graph_fixture () in
  let graph = raw_graph fixture.lowered in
  let description = fixture.description in
  List.iter
    (fun (name, (marker : Sequence.description)) ->
      let copied = { marker with Sequence.flags = marker.flags } in
      Alcotest.(check bool)
        (name ^ " copied record is distinct")
        true (copied != marker);
      Alcotest.(check bool)
        (name ^ " copied record has no operation")
        true
        (Regions.find fixture.context copied = None))
    [
      ("enter", description.enter);
      ("leave", description.leave);
      ("catch return", description.handler_return);
    ];
  let copied_enter =
    { description.enter with flags = description.enter.flags }
  in
  reject "rebuilt enter metadata cannot name an equal instruction"
    (seal fixture graph [ { description with enter = copied_enter } ]);
  reject "leave cannot also claim entry"
    (seal fixture graph [ { description with enter = description.leave } ]);
  reject "normal leave cannot be repeated as an early leave"
    (seal fixture graph
       [ { description with early_leaves = [ description.leave ] } ]);
  let copied_graph = rebuild graph (block_descriptions graph) in
  Alcotest.(check bool)
    "physically copied graph has no original seal" false
    (Regions.matches_graph fixture.context copied_graph);
  let foreign = graph_fixture () in
  Alcotest.(check bool)
    "equal foreign graph has no original seal" false
    (Regions.matches_graph fixture.context (raw_graph foreign.lowered));
  Alcotest.(check bool)
    "equal foreign marker has no original operation" true
    (Regions.find fixture.context foreign.description.enter = None)

let removed_and_moved_markers () =
  let fixture = graph_fixture () in
  let graph = raw_graph fixture.lowered in
  let description = fixture.description in
  let remove marker =
    block_descriptions graph
    |> List.map (fun (block : Graph.block_description) ->
        {
          block with
          instructions =
            List.filter (fun item -> item != marker) block.instructions;
        })
  in
  List.iter
    (fun (name, marker) ->
      let changed = rebuild graph (remove marker) in
      reject ("removed " ^ name) (seal fixture changed [ description ]))
    [
      ("enter", description.enter);
      ("leave", description.leave);
      ("catch return", description.handler_return);
    ];
  let move marker target replace_jump =
    remove marker
    |> List.map (fun (block : Graph.block_description) ->
        if not (Sequence.Block_id.equal block.block_id target) then block
        else
          let instructions =
            block.instructions
            |> List.concat_map (fun (item : Sequence.description) ->
                if item.opcode = Ir_opcode.Ic_jmp then
                  if replace_jump then [ marker ] else [ marker; item ]
                else [ item ])
          in
          { block with instructions })
    |> rebuild graph
  in
  let entry = Graph.block_id (Graph.entry graph) in
  reject ~code:"HCRUN0001" "enter moved after the protected region begins"
    (seal fixture
       (move description.enter description.untry_target false)
       [ description ]);
  reject "normal leave moved out of its untry target"
    (seal fixture (move description.leave entry false) [ description ]);
  reject "catch return moved into the normal cleanup block"
    (seal fixture
       (move description.handler_return description.untry_target true)
       [ description ])

let protected_control_edges () =
  let fixture = graph_fixture () in
  let graph = raw_graph fixture.lowered in
  let description = fixture.description in
  let entry = Graph.block_id (Graph.entry graph) in
  let change_jump from target =
    block_descriptions graph
    |> List.map (fun (block : Graph.block_description) ->
        if not (Sequence.Block_id.equal block.block_id from) then block
        else
          {
            block with
            instructions =
              List.map
                (fun (item : Sequence.description) ->
                  if item.opcode = Ir_opcode.Ic_jmp then
                    { item with payload = Some (Sequence.Block target) }
                  else item)
                block.instructions;
          })
    |> rebuild graph
  in
  reject ~code:"HCRUN0001" "ordinary protected edge enters catch"
    (seal fixture (change_jump entry description.catch_target) [ description ]);
  let normal_done =
    Graph.find_block graph description.untry_target
    |> Option.get |> Graph.successors |> List.hd
  in
  reject ~code:"HCRUN0001" "ordinary protected edge bypasses cleanup"
    (seal fixture (change_jump entry normal_done) [ description ]);
  reject ~code:"HCRUN0001" "untry edge reenters the protected body"
    (seal fixture
       (change_jump description.untry_target description.untry_target)
       [ description ])

let make_body fixture name graph =
  let frame = frame fixture name in
  let header = Frame.function_header frame in
  let record =
    Records.declarations (records fixture)
    |> List.find (fun classified ->
        Functions.resolved_declaration_header
          (Records.classified_declaration_source classified)
        == header)
  in
  let function_id =
    Body.Function_id.of_int (Frame.function_item_index frame)
    |> require_ok (fun (error : Body.error) -> error.message)
  in
  let body =
    Body.create
      {
        function_id;
        symbol = Frame.function_symbol frame;
        function_scope =
          Semantic_symbol_table.scope_id (Frame.function_scope frame);
        return_type =
          Headers.function_return_type header
          |> Semantic_type_reference.resolved_type;
        parameters = [];
        locals = [];
        stored_flags =
          Records.classified_declaration_record record
          |> Records.stored_flag_mask;
        compiler_options =
          Records.classified_declaration_state record
          |> Records.declaration_state_compiler_option_mask;
        span = Some (definition fixture name).location.span;
        body = graph;
      }
    |> require_ok body_errors
  in
  Body.with_definition ~records:(records fixture)
    ~sources:(Source.functions fixture.prepared)
    ~frames:(Source.frames fixture.prepared)
    ~definition:record ~frame body
  |> require_ok body_errors

let runtime_context ?entry_exceptions ?(function_exceptions = []) fixture entry
    functions =
  let globals =
    Globals.create ~span:fixture.ast.span
      (Source.global_records fixture.prepared)
    |> require_ok diagnostics_text
  in
  let initialization =
    Initialization.create ~span:fixture.ast.span ~globals ~entry []
    |> require_ok diagnostics_text
  in
  Calls.create_with_exceptions ?entry_exceptions ~function_exceptions
    ~records:(records fixture)
    ~function_sources:(Source.functions fixture.prepared)
    ~top_level:(Source.top_level fixture.prepared)
    ~initialization ~entry ~entry_calls:[]
    ~functions:(List.map (fun body -> (body, [])) functions)
    ()

let exact_runtime_owner () =
  let fixture = prepare (headers ^ "U0 F(){try {} catch {}} U0 G(){}") in
  let source = proof fixture (List.hd (sources fixture.ast)) in
  let lowered =
    lower ~frame:(frame fixture "F") fixture [ lower_statement source ]
    |> require_ok diagnostics_text
  in
  let regions = Lower.exception_regions lowered in
  let graph = raw_graph lowered in
  let original = make_body fixture "F" graph in
  let copied = make_body fixture "F" graph in
  let foreign = make_body fixture "G" graph in
  let entry = lower fixture [] |> require_ok diagnostics_text |> Lower.graph in
  Alcotest.(check bool)
    "foreign owner deliberately shares the exact graph" true
    (Body.body foreign == graph);
  Alcotest.(check bool)
    "original checked function owns its regions" true
    (Regions.matches_function regions (Some original));
  Alcotest.(check bool)
    "matching graph cannot replace the containing function" false
    (Regions.matches_function regions (Some foreign));
  Alcotest.(check bool)
    "function regions cannot belong to entry" false
    (Regions.matches_function regions None);
  let context =
    runtime_context
      ~function_exceptions:[ (original, regions) ]
      fixture entry [ original ]
    |> require_ok diagnostics_text
  in
  Alcotest.(check bool)
    "original body retrieves the same exception context" true
    (Option.fold ~none:false ~some:(( == ) regions)
       (Calls.exception_regions context ~owner:(Calls.Function original)));
  Alcotest.(check bool)
    "another body for the same definition is foreign" true
    (Calls.exception_regions context ~owner:(Calls.Function copied) = None);
  reject ~code:"HCIRVM0014"
    "foreign body cannot be attached to original exception source"
    (runtime_context
       ~function_exceptions:[ (foreign, regions) ]
       fixture entry [ foreign ]);
  reject ~code:"HCIRVM0014"
    "metadata owner must occur in the exact function bundle"
    (runtime_context
       ~function_exceptions:[ (copied, regions) ]
       fixture entry [ original ])

let runtime_graph_and_records () =
  let fixture = graph_fixture () in
  let source_fixture = fixture.source_fixture in
  let entry = Lower.graph fixture.lowered in
  let context =
    runtime_context ~entry_exceptions:fixture.context source_fixture entry []
    |> require_ok diagnostics_text
  in
  Alcotest.(check bool)
    "original entry retains exception metadata" true
    (Option.fold ~none:false ~some:(( == ) fixture.context)
       (Calls.exception_regions context ~owner:Calls.Entry));
  let graph = raw_graph fixture.lowered in
  let copied_graph = rebuild graph (block_descriptions graph) in
  let copied =
    X87.verify copied_graph
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : X87.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
  in
  reject ~code:"HCIRVM0014" "original regions cannot be rebound to copied entry"
    (runtime_context ~entry_exceptions:fixture.context source_fixture copied []);
  let foreign = prepare (headers ^ "try {} catch {}") in
  reject ~code:"HCIRVM0014"
    "original graph does not acquire another record snapshot"
    (runtime_context ~entry_exceptions:fixture.context foreign entry [])

let tests =
  [
    Alcotest.test_case "exception source and record identities" `Quick
      original_source_and_records;
    Alcotest.test_case "exception ancestry keeps original parent arms" `Quick
      original_parent_arms;
    Alcotest.test_case "exception source retains its original function" `Quick
      original_function_owner;
    Alcotest.test_case "reconstructed header visibility has no authority" `Quick
      reconstructed_header_visibility;
    Alcotest.test_case "original source header witnesses work in both modes"
      `Quick original_headers_in_both_modes;
    Alcotest.test_case
      "exception headers retain the standard calling convention" `Quick
      checked_exception_header_abi;
    Alcotest.test_case "same symbols cannot replace original header syntax"
      `Quick same_symbol_cannot_replace_header_syntax;
    Alcotest.test_case "task inputs retain original exception headers" `Quick
      retained_task_headers;
    Alcotest.test_case "exception marker identities cannot be reconstructed"
      `Quick marker_identity_and_reconstructed_metadata;
    Alcotest.test_case "removed and moved exception markers reject" `Quick
      removed_and_moved_markers;
    Alcotest.test_case "ordinary edges preserve exception scopes" `Quick
      protected_control_edges;
    Alcotest.test_case "runtime exception metadata owns an exact body" `Quick
      exact_runtime_owner;
    Alcotest.test_case "runtime exception graph and records stay sealed" `Quick
      runtime_graph_and_records;
  ]
