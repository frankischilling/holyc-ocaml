open Holyc_lib
module Output = Test_integer_output
module G = Test_integer_globals
module F = Test_integer_functions
module H = Test_ir_integer_interpreter
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Headers = Semantic_function_type_resolution
module Resolution = Semantic_function_resolution
module Records = Semantic_function_record_classification
module Typed = Semantic_function_call_expression_result
module VM = Ir_integer_interpreter
module PF = Test_function_frame_layout
module Symbol = Semantic_symbol

let checked = PF.checked

let prepare mode text =
  let p = PF.prepare ~mode ~path:"joined-definition-binding.HC" text in
  let members =
    index_aggregate_members p.session ~declarations:p.declarations
      ~headers:p.aggregate_headers ~members:p.aggregate_members
      ~layouts:p.aggregate_layouts
    |> checked
  in
  let global_types =
    resolve_global_types p.session ~declarations:p.declarations
      ~aggregates:p.aggregates p.ast
    |> checked
  in
  let functions =
    resolve_function_identities p.session ~declarations:p.declarations
      ~functions:p.function_types ~compilation_mode:mode p.ast
    |> checked
  in
  let globals =
    resolve_global_records p.session ~declarations:p.declarations
      ~globals:global_types ~compilation_mode:mode p.ast
    |> checked
  in
  let expressions =
    resolve_function_expressions p.session ~declarations:p.declarations
      ~functions:p.functions ~local_types:p.local_types ~bindings:p.bindings
      p.ast
    |> checked
  in
  let expressions =
    resolve_module_expressions p.session ~declarations:p.declarations
      ~aggregates:p.aggregates ~functions ~globals ~expressions
    |> checked
  in
  let calls =
    resolve_function_calls p.session ~declarations:p.declarations ~members
      ~function_types:p.function_types ~local_types:p.local_types ~global_types
      ~functions ~expressions p.ast
    |> checked
  in
  let policies =
    analyze_function_call_conversions p.session ~declarations:p.declarations
      ~headers:p.aggregate_headers ~calls
    |> Test_function_call_conversion_policy.checked_policy
  in
  let sources =
    type_function_call_expressions p.session ~members ~policies
    |> Test_function_call_target_classification.checked_results
  in
  let records =
    classify_function_records p.session ~resolution:functions p.ast |> checked
  in
  (p, sources, records, PF.layout p)

let definition_named records name =
  Records.declarations records
  |> List.find (fun record ->
      let site =
        record |> Records.classified_declaration_source
        |> Resolution.resolved_declaration_site
      in
      Resolution.declaration_site_source_kind site = Resolution.Definition
      && site |> Resolution.declaration_site_function |> Headers.function_symbol
         |> Semantic_symbol.name = name)

let header record =
  record |> Records.classified_declaration_source
  |> Resolution.resolved_declaration_site
  |> Resolution.declaration_site_function

let body_description frame definition =
  let members kind =
    Frame.function_locations frame
    |> List.filter (fun location -> Frame.location_kind location = kind)
    |> List.mapi (fun position location ->
        Body.
          {
            position;
            symbol = Frame.location_symbol location;
            type_ = Frame.location_checked_type location;
            span = None;
          })
  in
  let graph =
    H.verified ~entry:0
      [
        H.block 0
          [
            H.imm ~id:0 ~value:0 ~type_:H.i64 42L;
            H.return_value ~id:1 ~operand:0 ~type_:H.i64;
            H.ret 2;
          ];
      ]
  in
  Body.
    {
      function_id =
        Test_ir_function_body.function_id (Frame.function_item_index frame);
      symbol = Frame.function_symbol frame;
      function_scope =
        Semantic_symbol_table.scope_id (Frame.function_scope frame);
      return_type =
        header definition |> Headers.function_return_type
        |> Semantic_type_reference.resolved_type;
      parameters = members Frame.Named_parameter;
      locals = members Frame.Automatic_local;
      stored_flags =
        Records.classified_declaration_record definition
        |> Records.stored_flag_mask;
      compiler_options =
        Records.classified_declaration_state definition
        |> Records.declaration_state_compiler_option_mask;
      span = None;
      body = Ir_x87_stack.graph graph;
    }

let checked_body =
  H.require_ok (fun errors ->
      String.concat "; "
        (List.map Test_ir_function_body.show_function_error errors))

let reject_binding result =
  match result with
  | Ok _ ->
      Alcotest.fail "unowned definition metadata acquired callable identity"
  | Error errors ->
      Alcotest.(check string)
        "definition ownership diagnostic" "HCIR0027" (List.hd errors).Body.code

let reconstructed_declarations () =
  List.iter
    (fun mode ->
      let p, sources, records, frames =
        prepare mode
          "extern I64 Id(I64 value);I64 Id(I64 n){return n;}U64 Other(U64 \
           q){return q;}"
      in
      let definition = definition_named records "Id" in
      let frame = PF.frame_named frames "Id" in
      let raw =
        Body.create (body_description frame definition) |> checked_body
      in
      let bind ?(records = records) ?(definition = definition) () =
        Body.with_definition ~records ~sources ~frames ~definition ~frame raw
      in
      let original = bind () |> checked_body in
      Alcotest.(check bool)
        "original association changes callable identity" true
        (Body.callable_symbol original != Body.symbol original);
      let original_header = header definition in
      let options = Body.compiler_options raw in
      let replay selected_header =
        let fact =
          Resolution.make_declaration_with_options ~compiler_option_mask:options
            ~function_:selected_header ~kind:Resolution.Definition
          |> checked
        in
        let resolution =
          Resolution.resolve
            ~table:(Session.semantic_symbols p.session)
            ~parent:(Semantic_declaration_collection.scope p.declarations)
            ~compilation_mode:(Test_function_resolution.semantic_mode mode)
            [ fact ]
          |> checked
        in
        Records.classify resolution
          [
            Records.make_declaration_state ~staging_mask:0L
              ~compiler_option_mask:options ();
          ]
        |> checked
      in
      let replayed = replay original_header in
      let replayed_definition = List.hd (Records.declarations replayed) in
      Alcotest.(check bool)
        "replay retains the original physical header" true
        (header replayed_definition == original_header);
      reject_binding (bind ~records:replayed ~definition:replayed_definition ());
      let rebuild signature =
        Headers.make_function
          ~symbol:(Headers.function_symbol original_header)
          ~scope:(Headers.function_scope original_header)
          ~item_index:(Headers.function_item_index original_header)
          ~return_type:(Headers.function_return_type original_header)
          ~signature
          ~parameter_bindings:
            (Headers.function_parameter_bindings original_header)
          ~variadic_bindings:None
        |> checked
        |> fun input ->
        Headers.resolve
          ~table:(Session.semantic_symbols p.session)
          ~parent:(Semantic_declaration_collection.scope p.declarations)
          [ input ]
        |> checked |> Headers.functions |> List.hd
      in
      let signature = Headers.function_signature original_header in
      let other_parameter =
        definition_named records "Other"
        |> header |> Headers.function_signature |> Headers.signature_parameters
        |> List.hd
      in
      let original_parameter =
        Headers.signature_parameters signature |> List.hd
      in
      let changed_parameter =
        Headers.make_parameter
          ~index:(Headers.parameter_index original_parameter)
          ~origin:(Headers.parameter_origin original_parameter)
          ?name:(Headers.parameter_name original_parameter)
          ?name_origin:(Headers.parameter_name_origin original_parameter)
          ~type_reference:(Headers.parameter_type_reference other_parameter)
          ~declarator_kind:Headers.Object
          ~default:(Headers.parameter_default original_parameter)
          ()
        |> checked
      in
      let changed_signature =
        Headers.make_signature
          ~opening_origin:(Headers.signature_opening_origin signature)
          ~parameters:[ changed_parameter ]
          ~closing_origin:(Headers.signature_closing_origin signature)
          ()
        |> checked
      in
      List.iter
        (fun signature ->
          let reconstructed = rebuild signature |> replay in
          reject_binding
            (bind ~records:reconstructed
               ~definition:(List.hd (Records.declarations reconstructed))
               ()))
        [ signature; changed_signature ])
    G.modes

let gates =
  [
    ( "prototype and renamed definition parameter",
      "extern I64 Id(I64 value);I64 Id(I64 n){return n;}Id(42);",
      "" );
    ( "joined U0 procedure",
      "extern U0 Set(I64 value);I64 G=0;U0 Set(I64 n){G=n;}Set(42);G;",
      "" );
    ( "nested joined calls",
      "extern I64 Inc(I64 value);I64 Inc(I64 n){return n+1;}I64 Twice(I64 \
       n){return Inc(Inc(n));}Twice(40);",
      "" );
    ( "hosted and source declaration snapshots",
      "I64 G=0;extern U0 PutChars(U64 ch);PutChars('A');U0 PutChars(U64 \
       word){G=42;}PutChars('B');G;",
      "A" );
  ]

let ordinary_execution () =
  Output.cases
    [
      ( "extern I64 Id(I64 a);extern I64 Id(I64 b);I64 Id(I64 n){I64 \
         copy=n;return copy;}Id(42);",
        "" );
      ( "extern I64 F(I64 a);I64 F(I64 n){if(n)return F(n-1);return 42;}F(4);",
        "" );
      ( "extern U0 Set(I64 *value);U0 Set(I64 *n){*n=42;}I64 F(){I64 \
         n=0;Set(&n);return n;}F();",
        "" );
      ( "extern U0 Set(U8 *value);U0 Set(U8 *n){n[1]=42;}I64 F(){U8 \
         a[2];Set(a);return a[1];}F();",
        "" );
      ( "extern I64 Add(I64 a,I64 b);I64 Add(I64 x,I64 y){return x+y;}I64 \
         n=0;Add(n++,n=21);",
        "" );
    ];
  List.iter
    (fun mode ->
      let report =
        Output.run ~mode
          "extern U64 Id(U64 value);U64 Id(U64 n){return n;}Id(-1);"
      in
      let result = (F.checked (integer_program_report_outcome report)).value in
      let word = Option.get (VM.final_value result) in
      Alcotest.(check int64) "joined unsigned bits" (-1L) word.bits;
      Alcotest.(check bool)
        "joined unsigned return type" true (word.type_ = VM.U64);
      ignore
        (Output.run ~mode "extern U0 F();U0 F(){return;}42;F();"
        |> Output.expect ~value:None ""))
    G.modes

let persistent_and_initializer_owners () =
  Output.cases
    [
      ( "extern I64 Next();I64 Next(){static I64 n=40;return ++n;}Next();Next();",
        "" );
      ( "extern I64 Seed(I64 value);I64 Seed(I64 n){return n;}I64 G=Seed(42);G;",
        "" );
      ( "extern I64 Seed(I64 value);I64 Seed(I64 n){return n;}I64 F(){static \
         I64 n=Seed(42);return n;}F();",
        "" );
      ( Output.print_header
        ^ "extern I64 Seed(I64 value);I64 Seed(I64 n){Print(\"%d\",n);return \
           n;}I64 G=Seed(42);G;",
        "42" );
      ( Output.print_header
        ^ "extern I64 Seed(I64 value);I64 Seed(I64 n){Print(\"%d\",n);return \
           n;}I64 F(){static I64 n=Seed(42);return n;}F();",
        "42" );
      ( "extern I64 Seed(I64 value);I64 Seed(I64 n){return n;}I64 F(){static \
         I64 n=Seed(40);return ++n;}F();F();",
        "" );
    ];
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "extern I64 Next();I64 Next(){static I64 n=40;return \
           ++n;}Next();Next();"
      in
      let execute () =
        VM.execute_program_report
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~runtime_calls:(integer_program_runtime_calls compiled)
          ~functions:(integer_program_functions compiled)
          ~max_steps:1000 ~max_frame_bytes:1 ~max_call_depth:1
          (integer_program_entry compiled)
      in
      List.iter
        (fun () ->
          let report = execute () in
          let result =
            VM.report_outcome report |> H.require_ok H.show_vm_errors
          in
          Alcotest.(check int64)
            "joined statics reset for each image" 42L
            (Option.get (VM.final_value result)).bits;
          Alcotest.(check string)
            "fresh empty capture" ""
            (VM.report_output_bytes report))
        [ (); () ];
      let slot =
        integer_program_globals compiled
        |> Ir_integer_globals.statics |> List.hd
      in
      let definition = integer_program_functions compiled |> List.hd in
      Alcotest.(check bool)
        "static keeps the exact definition frame" true
        (Ir_integer_globals.static_frame slot == definition.frame))
    G.modes

let initialization_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (suffix, owner) ->
          let report =
            Output.run ~mode
              (Output.print_header
             ^ "extern I64 Seed(I64 value);I64 Seed(I64 n){Print(\"A\");return \
                42/n;}" ^ suffix)
          in
          let error = Output.fault ~output:"A" "HCIRVM0009" report in
          List.iter
            (fun note ->
              Alcotest.(check bool) note true (List.mem note error.notes))
            [
              "function=Seed";
              "initializer=" ^ owner;
              "initializer_phase=" ^ Test_integer_static_initializers.phase mode;
            ])
        [
          ("I64 G=Seed(0);G;", "G");
          ("I64 F(){static I64 n=Seed(0);return n;}F();", "n");
        ];
      List.iter
        (fun suffix ->
          ignore
            (Output.run ~mode
               ("extern I64 Seed(I64 value);I64 Seed(I64 n){return n/3;}"
              ^ suffix)
            |> Output.fault "HCRUN0006"))
        [ "I64 G=Seed(42);G;"; "I64 F(){static I64 n=Seed(42);return n;}F();" ])
    G.modes

let source_order_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let error = Output.run ~mode source |> Output.fault "HCIRVM0014" in
          Alcotest.(check bool)
            "earlier ordinary extern remains a preflight boundary" true
            (List.mem "stage=preflight" error.notes))
        [
          "extern I64 Id(I64 n);Id(42);I64 Id(I64 n){return n;}";
          "extern I64 Id(I64 n);I64 Caller(){return Id(42);}I64 Id(I64 \
           n){return n;}Caller();";
        ];
      let compiled =
        G.compile ~mode
          "I64 G=0;extern U0 PutChars(U64 ch);PutChars('A');U0 PutChars(U64 \
           word){G=42;}PutChars('B');G;"
      in
      let body = (List.hd (integer_program_functions compiled)).body in
      let context = integer_program_runtime_calls compiled in
      let sites =
        Test_integer_output.SI.code (integer_program_entry compiled)
        |> List.filter_map
             (fun (instruction : Ir_instruction_sequence.description) ->
               Ir_runtime_call_context.find_start context
                 ~owner:Ir_runtime_call_context.Entry instruction.instruction_id)
      in
      let first, second = (List.nth sites 0, List.nth sites 1) in
      Alcotest.(check bool)
        "joined calls share the exact callable identity" true
        (Ir_runtime_call_context.symbol first
         == Ir_runtime_call_context.symbol second
        && Ir_runtime_call_context.symbol second == Body.callable_symbol body);
      Alcotest.(check bool)
        "earlier selected extern remains hosted" true
        (Ir_runtime_call_context.provider first
        = Some Ir_runtime_call_context.Put_chars);
      Alcotest.(check bool)
        "later selected definition uses its source body" true
        (Ir_runtime_call_context.provider second = None
        && Ir_runtime_call_context.declaration second
           == Option.get (Body.definition_declaration body)))
    G.modes;
  ignore
    (Output.run ~mode:Preprocessor.Jit
       "I64 G=0;I64 Id(){G=41;return G;}Id();I64 Id(){return G+1;}Id();"
    |> Output.expect "");
  ignore
    (Output.run ~mode:Preprocessor.Aot
       "import I64 Id(I64 value);I64 Id(I64 n){return n;}Id(42);"
    |> Output.expect "");
  ignore
    (Output.run ~mode:Preprocessor.Aot
       "extern I64 Id(I64 value);I64 Id(I64 n){return n;}I64 Id(I64 n){return \
        n;}Id(42);"
    |> Output.fault "HCIRVM0014")

let resources () =
  List.iter
    (fun mode ->
      let plain = "I64 Id(I64 n){return n;}Id(42);" in
      let joined = "extern I64 Id(I64 value);" ^ plain in
      let baseline = Output.run ~mode plain |> Output.expect "" in
      let steps = VM.executed_steps baseline in
      let result =
        Output.run ~mode ~max_steps:steps ~max_frame_bytes:8 ~max_call_depth:1
          joined
        |> Output.expect ""
      in
      Alcotest.(check int)
        "prototype adds no runtime instructions" steps
        (VM.executed_steps result);
      Alcotest.(check int)
        "prototype adds no preparation" 0
        (VM.compiled_initializer_steps result);
      ignore
        (Output.run ~mode ~max_steps:(steps - 1) joined
        |> Output.fault "HCIRVM0007");
      ignore
        (Output.run ~mode ~max_frame_bytes:7 joined |> Output.fault "HCIRVM0011");
      let recursive =
        "extern I64 F(I64 n);I64 F(I64 n){if(n)return F(n-1);return 42;}F(1);"
      in
      ignore
        (Output.run ~mode ~max_frame_bytes:16 ~max_call_depth:2 recursive
        |> Output.expect "");
      ignore
        (Output.run ~mode ~max_frame_bytes:15 recursive
        |> Output.fault "HCIRVM0011");
      ignore
        (Output.run ~mode ~max_call_depth:1 recursive
        |> Output.fault "HCIRVM0015"))
    G.modes

let foreign_and_rebuilt_frames () =
  List.iter
    (fun mode ->
      let text =
        "extern I64 Id(I64 value);I64 Id(I64 n){I64 copy;return n;}I64 \
         Other(I64 n){return n;}"
      in
      let p, sources, records, frames = prepare mode text in
      let _, foreign_sources, foreign_records, foreign_frames =
        prepare mode text
      in
      let definition = definition_named records "Id" in
      let frame = PF.frame_named frames "Id" in
      let description = body_description frame definition in
      let raw = Body.create description |> checked_body in
      let bind ?(sources = sources) ?(records = records) ?(frames = frames)
          ?(definition = definition) ?(frame = frame) body =
        Body.with_definition ~sources ~records ~frames ~definition ~frame body
      in
      let bound = bind raw |> checked_body in
      reject_binding (bind ~sources:foreign_sources raw);
      reject_binding (bind ~records:foreign_records raw);
      reject_binding (bind ~frames:foreign_frames raw);
      reject_binding
        (bind ~definition:(definition_named foreign_records "Id") raw);
      reject_binding (bind ~frame:(PF.frame_named foreign_frames "Id") raw);
      reject_binding (bind ~definition:(definition_named records "Other") raw);
      reject_binding
        (bind ~definition:(List.hd (Records.declarations records)) raw);
      List.iter
        (fun description ->
          reject_binding (bind (Body.create description |> checked_body)))
        [
          { description with symbol = Body.callable_symbol bound };
          { description with return_type = H.u64 };
          { description with stored_flags = 0L };
          {
            description with
            compiler_options = Int64.logxor description.compiler_options 1L;
          };
          {
            description with
            parameters =
              List.map
                (fun (member : Body.member_description) ->
                  { member with type_ = H.u64 })
                description.parameters;
          };
          { description with locals = [] };
        ];
      let clone header =
        Headers.make_function
          ~symbol:(Headers.function_symbol header)
          ~scope:(Headers.function_scope header)
          ~item_index:(Headers.function_item_index header)
          ~return_type:(Headers.function_return_type header)
          ~signature:(Headers.function_signature header)
          ~parameter_bindings:(Headers.function_parameter_bindings header)
          ~variadic_bindings:(Headers.function_variadic_bindings header)
        |> checked
      in
      let function_types =
        Headers.functions p.function_types
        |> List.map clone
        |> Headers.resolve
             ~table:(Session.semantic_symbols p.session)
             ~parent:(Semantic_declaration_collection.scope p.declarations)
        |> checked
      in
      let cloned_frames = PF.layout { p with function_types } in
      let cloned_frame = PF.frame_named cloned_frames "Id" in
      Alcotest.(check bool)
        "cloned frame retains the same symbol" true
        (Frame.function_symbol cloned_frame == Frame.function_symbol frame);
      Alcotest.(check bool)
        "cloned frame has a different physical header" true
        (Frame.function_header cloned_frame != header definition);
      Alcotest.(check bool)
        "original typed declaration ownership remains valid" true
        (Typed.owns_declaration sources
           (Records.classified_declaration_source definition));
      reject_binding (bind ~frames:cloned_frames ~frame:cloned_frame raw);
      Alcotest.(check bool)
        "associated frame identity is physical" false
        (Body.definition_matches_frame bound cloned_frame);
      let error =
        VM.execute_function ~max_steps:100 ~max_frame_bytes:64
          ~frame:cloned_frame ~arguments:[ 42L ] bound
        |> Test_ir_integer_frames.expect_error "HCIRVM0011"
      in
      ignore error)
    G.modes

let raw_body_contract () =
  List.iter
    (fun mode ->
      let source = "extern I64 Id(I64 value);I64 Id(I64 n){return n;}Id(42);" in
      let compiled = G.compile ~mode source in
      let definition = List.hd (integer_program_functions compiled) in
      let named_body = definition.body in
      let member member =
        Body.
          {
            position = Body.member_position member;
            symbol = Body.member_symbol member;
            type_ = Body.member_type member;
            span = Body.member_span member;
          }
      in
      let description =
        Body.
          {
            function_id = Body.function_id named_body;
            symbol = Body.symbol named_body;
            function_scope = Body.function_scope named_body;
            return_type = Body.return_type named_body;
            parameters = List.map member (Body.parameters named_body);
            locals = List.map member (Body.locals named_body);
            stored_flags = Body.stored_flags named_body;
            compiler_options = Body.compiler_options named_body;
            span = Body.span named_body;
            body = Body.body named_body;
          }
      in
      let raw = Body.create description |> checked_body in
      let execute functions =
        VM.execute_program ~functions ~max_steps:100 ~max_frame_bytes:8
          ~max_call_depth:1
          (integer_program_entry compiled)
      in
      let success = execute [ definition ] |> H.require_ok H.show_vm_errors in
      Alcotest.(check int64)
        "checked binding works through raw VM API" 42L
        (Option.get (VM.final_value success)).bits;
      ignore
        (execute [ { definition with body = raw } ]
        |> Test_ir_integer_frames.expect_error "HCIRVM0014");
      let renamed =
        Body.create
          { description with symbol = Body.callable_symbol named_body }
        |> checked_body
      in
      ignore
        (execute [ { definition with body = renamed } ]
        |> Test_ir_integer_frames.expect_error "HCIRVM0011");
      ignore
        (execute [ definition; definition ]
        |> Test_ir_integer_frames.expect_error "HCIRVM0014"))
    G.modes

let definition_publication_index () =
  let module SI = Test_integer_static_initializers in
  let module Initial = Ir_global_initialization in
  let module Seq = Ir_instruction_sequence in
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "extern I64 Seed(I64 value);I64 F(){static I64 n=Seed(42);return \
           n;}I64 Seed(I64 n){return n;}F();"
      in
      let entry =
        F.rewrite_entry
          (fun (d : Seq.description) ->
            if
              d.opcode = Ir_opcode.Ic_call_indirect2
              || d.opcode = Ir_opcode.Ic_call_extern
            then { d with opcode = Ir_opcode.Ic_call }
            else d)
          (integer_program_entry compiled)
      in
      let descriptions = SI.descriptions compiled in
      let globals = integer_program_globals compiled in
      let initialization =
        Initial.create ~static_descriptions:descriptions
          ~span:(SI.span (List.hd descriptions))
          ~globals ~entry []
        |> F.checked
      in
      let result =
        VM.execute_program ~globals ~initialization
          ~functions:(integer_program_functions compiled)
          ~max_steps:1000 ~max_frame_bytes:8 ~max_call_depth:1 entry
      in
      if mode = Preprocessor.Jit then
        let error =
          result |> Test_ir_integer_frames.expect_error "HCIRVM0017"
        in
        Alcotest.(check bool)
          "actual definition, not earlier prototype, controls publication" true
          (String.ends_with ~suffix:"not yet published" error.message)
      else
        let value = result |> H.require_ok H.show_vm_errors in
        Alcotest.(check int64)
          "AOT load sees the checked joined definition" 42L
          (Option.get (VM.final_value value)).bits)
    G.modes

let tests =
  List.map
    (fun (name, source, output) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode ->
              ignore (Output.run ~mode source |> Output.expect output))
            G.modes))
    gates
  @ [
      Alcotest.test_case "reconstructed declaration chains and headers" `Quick
        reconstructed_declarations;
      Alcotest.test_case "ordinary execution domains" `Quick ordinary_execution;
      Alcotest.test_case "persistent storage and initializer owners" `Quick
        persistent_and_initializer_owners;
      Alcotest.test_case "initializer faults and transitive arithmetic guards"
        `Quick initialization_faults;
      Alcotest.test_case "source declaration order and snapshots" `Quick
        source_order_boundaries;
      Alcotest.test_case "prototype-free resource accounting" `Quick resources;
      Alcotest.test_case "foreign metadata and reconstructed frame headers"
        `Quick foreign_and_rebuilt_frames;
      Alcotest.test_case "raw body and callable identity contracts" `Quick
        raw_body_contract;
      Alcotest.test_case "definition-position initializer publication" `Quick
        definition_publication_index;
      Alcotest.test_case "callable identity dump" `Quick (fun () ->
          List.iter
            (fun mode ->
              let source =
                "extern I64 Id(I64 value);I64 Id(I64 n){return n;}Id(42);"
              in
              let compiled = G.compile ~mode source in
              let first = integer_program_human compiled in
              Alcotest.(check string)
                "deterministic joined dump" first
                (G.compile ~mode source |> integer_program_human);
              let definition = List.hd (integer_program_functions compiled) in
              let expected =
                Printf.sprintf
                  "function=^f%d definition=@s%d callable=@s%d item=%d"
                  (Body.function_id definition.body |> Body.Function_id.to_int)
                  (Body.symbol definition.body |> Symbol.id |> Symbol.Id.to_int)
                  (Body.callable_symbol definition.body
                  |> Symbol.id |> Symbol.Id.to_int)
                  (Frame.function_item_index definition.frame)
              in
              Alcotest.(check bool)
                "versioned callable mapping" true
                (PF.contains_substring first "holyc-ir-function-binding-v1");
              Alcotest.(check bool)
                "exact function, definition, callable and item" true
                (PF.contains_substring first expected);
              Alcotest.(check bool)
                "unjoined dump keeps its existing form" false
                ( G.compile ~mode "I64 Id(I64 n){return n;}Id(42);"
                |> integer_program_human
                |> fun text ->
                  PF.contains_substring text "holyc-ir-function-binding-v1" ))
            G.modes);
    ]
