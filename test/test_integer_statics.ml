open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter
module Globals = Ir_integer_globals
module Frame = Semantic_function_frame_layout
module Prep = Integer_initializer_preparation
module Seq = Ir_instruction_sequence
module Body = Ir_function_body
module H = Test_ir_integer_interpreter

let source = "I64 Next(){static I64 n=40;return ++n;}Next();Next();"

let cases examples =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (G.run ~mode text |> F.expect expected))
        examples)
    G.modes

let source_gate () = cases [ (source, 42L) ]

let lifetime () =
  cases
    [
      ( "I64 F(I64 p){static I64 n=37;++n;if(p)return F(p-1);return n;}F(4);",
        42L );
      ( "I64 A(){static I64 n=19;return ++n;}I64 B(){static I64 n=21;return \
         ++n;}(A()+B());",
        42L );
      ( "I64 F(){static I64 n=19;return ++n;}I64 G(){I64 a=F();return \
         a+F();}G();",
        41L );
      ("I64 Next(){static I64 n=40;return ++n;}I64 G=Next();Next();", 42L);
    ]

let zero_fill () =
  ignore
    (G.run ~mode:Preprocessor.Aot
       "I64 Next(){static I64 n;n+=1;return n;}Next();Next();"
    |> F.expect 2L)

let updates () =
  cases
    [
      ("I64 F(){static I64 n=1;return (n+=(n=2));}F();", 4L);
      ("I64 F(){static I64 n=1;n+=n++;return n;}F();", 3L);
      ("I64 F(){static I64 n=42;return n++;}F();", 42L);
      ("I64 F(){static I64 n=42;return n--;}F();", 42L);
      ("I64 F(){static I64 n=43;return --n;}F();", 42L);
      ( "I64 F(){static I64 \
         n=21;n*=4;n/=2;n%=43;n&=63;n|=1;n^=1;n<<=1;n>>=1;return n;}F();",
        42L );
      ("I64 F(){static I64 n=44;n-=2;return n;}F();", 42L);
      ("I64 F(){static I64 n=0;while(n<42)n++;return n;}F();F();", 42L);
      ("I64 F(){static I64 n=0;if(0&&n++){}if(1||n++){}return n;}F();", 0L);
      ("I64 F(){static I64 n=0;(0&&n++);return n;}F();", 1L);
      ("I64 F(){static I64 n=0;(0<n++<3);return n;}F();", 1L);
      ("I64 F(){static I64 n=0x7FFFFFFFFFFFFFFF;return ++n;}F();", Int64.min_int);
      ("I64 F(){static I64 n=-6;U64 d=2;return n/=d;}F();", -3L);
    ];
  List.iter
    (fun mode ->
      ignore
        (G.run ~mode "U64 F(){static U64 n=-1;return ++n;}F();"
        |> F.expect ~type_:VM.U64 0L);
      ignore
        (G.run ~mode "U64 F(){static U64 n=-1;I64 d=1;return n>>=d;}F();"
        |> F.expect ~type_:VM.U64 Int64.max_int))
    G.modes

let definition_time () =
  cases
    [
      ("I64 F(){if(0){static I64 n=42;}return n;}F();", 42L);
      ("I64 F(){return 42;static I64 n=42;}F();", 42L);
      ("I64 F(I64 p){static I64 n=sizeof(p)+34;return n;}F(99);", 42L);
      ("I64 Never(){static I64 n=42;return n;}42;", 42L);
      ("I64 G;I64 F(){static I64 n;return 1;}42;", 42L);
      ("I64 F(){static I64 n;return 1;}F();", 1L);
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let error = F.first_error (G.run ~mode text) in
          Alcotest.(check string)
            "unreachable constant fault" "HCIRVM0009" error.code;
          Alcotest.(check bool)
            "constant owner" true
            (List.mem "initializer=s" error.notes);
          Alcotest.(check bool)
            "constant phase" true
            (List.mem "initializer_phase=constant-preparation" error.notes))
        [
          "I64 Never(){static I64 s=1/0;return 0;}42;";
          "I64 F(){if(0){static I64 s=1/0;}return 0;}42;";
          "I64 F(){return 0;static I64 s=1/0;}42;";
          "I64 F(){static I64 s=1/0;return 0;}I64 g=1%0;42;";
        ])
    G.modes

let boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          Alcotest.(check string)
            "nonconstant static phase boundary" "HCRUN0006"
            (F.first_error (G.run ~mode text)).code)
        [
          "I64 F(I64 p){static I64 n=p;return n;}F(40);";
          "I64 F(){I64 p=40;static I64 n=p;return n;}F();";
          "I64 F(){static I64 n=1<<2;return n;}42;";
        ];
      List.iter
        (fun text -> ignore (F.first_error (G.run ~mode text)))
        [
          "I64 F(){static I64 *n;return 0;}42;";
          "I64 F(){static F64 n;return 0;}42;";
          "I64 F(){static I0 n;return 0;}42;";
          "I64 F(){static I64 n={42};return 0;}42;";
        ])
    G.modes;
  let error =
    F.first_error (G.run "I64 Next(){static I64 n;n+=1;return n;}Next();")
  in
  Alcotest.(check string) "unknown JIT static" "HCIRVM0012" error.code;
  Alcotest.(check string)
    "static update diagnostic"
    "hosted execution reached an uninitialized JIT persistent object"
    error.message;
  let read = F.first_error (G.run "I64 F(){static I64 n;return n;}F();") in
  Alcotest.(check string) "static read diagnostic" error.message read.message;
  Alcotest.(check bool)
    "active function" true
    (List.mem "function=Next" error.notes)

let limits () =
  List.iter
    (fun mode ->
      let result =
        G.run ~mode ~max_frame_bytes:1 ~max_global_bytes:8 source
        |> F.expect 42L
      in
      let steps = VM.executed_steps result in
      ignore
        (G.run ~mode ~max_frame_bytes:1 ~max_global_bytes:8 ~max_steps:steps
           source
        |> F.expect 42L);
      List.iter
        (fun (result, code) ->
          Alcotest.(check string)
            "precise bound" code (F.first_error result).code)
        [
          (G.run ~mode ~max_global_bytes:7 source, "HCIRVM0016");
          (G.run ~mode ~max_steps:(steps - 1) source, "HCIRVM0007");
          ( G.run ~mode ~max_global_bytes:8
              "I64 G;I64 Never(){static I64 n;return 0;}42;",
            "HCIRVM0016" );
          ( G.run ~mode ~max_global_bytes:8
              "I64 A(){static I64 n;return 0;}I64 B(){static I64 n;return \
               0;}42;",
            "HCIRVM0016" );
        ];
      let text = "I64 F(){static I64 s=40;return s;}I64 g=2;(F()+g);" in
      let recursion =
        "I64 F(I64 p){static I64 n=40;++n;if(p)return F(p-1);return n;}F(1);"
      in
      ignore
        (G.run ~mode ~max_call_depth:2 ~max_frame_bytes:16 recursion
        |> F.expect 42L);
      Alcotest.(check string)
        "static recursion depth bound" "HCIRVM0015"
        (F.first_error (G.run ~mode ~max_call_depth:1 recursion)).code;
      Alcotest.(check string)
        "only active parameters consume frame bytes" "HCIRVM0011"
        (F.first_error (G.run ~mode ~max_frame_bytes:15 recursion)).code;
      let fault_text = "I64 F(I64 d){static I64 n=42;n/=d;return n;}F(0);" in
      let fault = F.first_error (G.run ~mode fault_text) in
      Alcotest.(check string)
        "static update arithmetic fault" "HCIRVM0009" fault.code;
      Alcotest.(check int)
        "static update operator span"
        (String.index fault_text '/')
        fault.primary.start;
      Alcotest.(check bool)
        "static fault active owner" true
        (List.mem "function=F" fault.notes);
      let compile limit =
        let session, config, source = F.inputs ~mode text in
        compile_integer_program ~max_initializer_steps:limit session ~config
          ~source
      in
      let compiled = (F.checked (compile 7)).value in
      Alcotest.(check int)
        "shared preparation count" 7
        (Prep.executed_steps (integer_program_initializer_preparation compiled));
      let error = F.first_error (compile 4) in
      Alcotest.(check string)
        "aggregate preparation budget" "HCIRVM0007" error.code;
      Alcotest.(check bool)
        "source order before later global" true
        (List.mem "initializer=g" error.notes))
    G.modes

let require_preflight = function
  | Ok _ -> Alcotest.fail "invalid static context executed"
  | Error errors ->
      List.iter
        (fun (error : VM.error) ->
          Alcotest.(check bool) "preflight" true (error.stage = VM.Preflight);
          Alcotest.(check int) "no effects" 0 error.executed_steps)
        errors

let replay () =
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode source in
      let globals = integer_program_globals compiled in
      let entry = integer_program_entry compiled in
      let functions = integer_program_functions compiled in
      let initialization = integer_program_initialization compiled in
      let execute ?initialization ?(globals = globals) graph =
        VM.execute_program ~globals ?initialization ~functions ~max_steps:1000
          ~max_frame_bytes:1 ~max_call_depth:1 graph
      in
      require_preflight (execute entry);
      require_preflight (execute ~initialization (F.rewrite_entry Fun.id entry));
      let foreign = G.compile ~mode source |> integer_program_globals in
      require_preflight (execute ~initialization ~globals:foreign entry);
      List.iter
        (fun _ ->
          match execute ~initialization entry with
          | Error errors -> Alcotest.fail (List.hd errors).message
          | Ok result ->
              Alcotest.(check int64)
                "fresh static image" 42L
                (Option.get (VM.final_value result)).bits;
              Alcotest.(check int)
                "static preparation count" 4
                (VM.compiled_initializer_steps result))
        [ (); () ];
      Alcotest.(check int)
        "global slots stay global" 0
        (List.length (Globals.slots globals));
      Alcotest.(check int) "static storage bound" 8 (Globals.byte_size globals);
      let slot = List.hd (Globals.statics globals) in
      let storage = Globals.static_storage slot in
      Alcotest.(check (option int64))
        "immutable initial bits" (Some 40L)
        (Globals.storage_initial_bits storage);
      Alcotest.(check bool)
        "no invocation frame slot" true
        (Frame.location_frame_slot (Globals.static_location slot) = None);
      Alcotest.(check bool)
        "canonical mode address" true
        (Globals.storage_opcode storage
        =
        if mode = Preprocessor.Jit then Ir_opcode.Ic_imm_i64
        else Ir_opcode.Ic_abs_addr);
      let prep = integer_program_initializer_preparation compiled in
      Alcotest.(check int)
        "global preparation API stays global" 0
        (List.length (Prep.items prep));
      let item = List.hd (Prep.static_items prep) in
      Alcotest.(check bool)
        "published static owner" true
        (Prep.static_slot item == slot);
      Alcotest.(check string)
        "deterministic static dump"
        (integer_program_human compiled)
        (integer_program_human (G.compile ~mode source)))
    G.modes

let rebuild ?compiler_options:options apply checked_body =
  let member value =
    Body.
      {
        position = member_position value;
        symbol = member_symbol value;
        type_ = member_type value;
        span = member_span value;
      }
  in
  let graph =
    Body.body checked_body |> Ir_x87_stack.verify
    |> H.require_ok (fun _ -> "x87")
    |> F.rewrite_entry apply |> Ir_x87_stack.graph
  in
  Body.create
    Body.
      {
        function_id = function_id checked_body;
        symbol = symbol checked_body;
        function_scope = function_scope checked_body;
        return_type = return_type checked_body;
        parameters = List.map member (parameters checked_body);
        locals = List.map member (locals checked_body);
        stored_flags = stored_flags checked_body;
        compiler_options =
          Option.value options ~default:(Body.compiler_options checked_body);
        span = span checked_body;
        body = graph;
      }
  |> H.require_ok (fun _ -> "body")

let address_ownership () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 F(){static I64 s;s=42;return s;}I64 G(){static I64 t;t=7;return \
           t;}42;"
      in
      let globals = integer_program_globals compiled in
      let entry = integer_program_entry compiled in
      let functions = integer_program_functions compiled in
      let first = List.hd functions in
      let owner =
        Globals.statics globals |> List.hd |> Globals.static_storage
      in
      let execute ?(functions = functions) entry =
        VM.execute_program ~globals ~functions ~max_steps:1000
          ~max_frame_bytes:1 ~max_call_depth:1 entry
      in
      let rewrite_other (d : Seq.description) =
        match d.payload with
        | Some (Seq.Symbol symbol) when Semantic_symbol.name symbol = "t" ->
            {
              d with
              payload = Some (Seq.Symbol (Globals.storage_symbol owner));
            }
        | _ -> d
      in
      let wrong_owner =
        List.map
          (fun (fn : VM.function_definition) ->
            { fn with body = rebuild rewrite_other fn.body })
          functions
      in
      require_preflight (execute ~functions:wrong_owner entry);
      require_preflight
        (VM.execute_function ~max_steps:100 ~max_frame_bytes:1
           ~frame:first.frame ~arguments:[] first.body);
      let wrong_options =
        {
          first with
          body =
            rebuild
              ~compiler_options:
                (Int64.logxor (Body.compiler_options first.body) 1L)
              Fun.id first.body;
        }
        :: List.tl functions
      in
      require_preflight (execute ~functions:wrong_options entry);
      let pointer =
        Semantic_type.pointer_to (Globals.storage_type owner)
        |> H.require_ok Fun.id
      in
      let entry =
        F.rewrite_entry
          (fun d ->
            match d.Seq.payload with
            | Some (Seq.Integer 42L) ->
                {
                  d with
                  opcode = Globals.storage_opcode owner;
                  target_type = Some pointer;
                  payload = Some (Seq.Symbol (Globals.storage_symbol owner));
                }
            | _ -> d)
          entry
      in
      require_preflight (execute entry);
      List.iter
        (fun rewrite ->
          let bad =
            {
              first with
              body =
                rebuild
                  (fun d ->
                    match d.Seq.payload with
                    | Some (Seq.Symbol _) -> rewrite d
                    | _ -> d)
                  first.body;
            }
            :: List.tl functions
          in
          require_preflight
            (execute ~functions:bad (integer_program_entry compiled)))
        [
          (fun d -> { d with Seq.flags = 1L });
          (fun d -> { d with Seq.target_type = Some H.i64 });
          (fun d ->
            {
              d with
              Seq.opcode =
                (if mode = Preprocessor.Jit then Ir_opcode.Ic_abs_addr
                 else Ir_opcode.Ic_imm_i64);
            });
        ])
    G.modes

let internal_storage_join () =
  (* Exercise the private producer without adding an image constructor to the
     public library API. *)
  let module Source = Holyc_lib__Driver__Integer_source in
  let module Internal = Holyc_lib__Ir__Integer_globals in
  let prepare text =
    let session, config, source = F.inputs text in
    let parsed =
      Parser.parse ~sources:(Session.sources session)
        ~definitions:(Session.definitions session)
        ~symbols:(Session.symbols session) ~config source
    in
    let ast = Option.get parsed.ast in
    let prepared =
      F.checked
        (Source.prepare_unit ~include_global_initializers:true session ~config
           ~span:ast.span ast)
    in
    (ast, prepared)
  in
  let left_ast, left = prepare "I64 A,B,C,D;42;" in
  let right_ast, right = prepare "I64 F(){static I64 s=40;return s;}42;" in
  let globals =
    F.checked
      (Internal.create ~initializers:(Source.top_level left) ~span:left_ast.span
         (Source.global_records left))
  in
  let static_symbol =
    Source.frames right |> Frame.functions |> List.hd
    |> Frame.function_locations
    |> List.find (fun location ->
        Frame.location_kind location = Frame.Static_local)
    |> Frame.location_symbol
  in
  Alcotest.(check bool)
    "fixture has a cross-table storage ID collision" true
    (Globals.slots globals
    |> List.exists (fun slot ->
        Semantic_symbol.Id.equal
          (Semantic_symbol.id (Globals.slot_symbol slot))
          (Semantic_symbol.id static_symbol)));
  let result =
    Internal.with_statics ~span:right_ast.span ~frames:(Source.frames right)
      ~functions:(Source.functions right) ~records:(Source.records right)
      globals
  in
  Alcotest.(check string)
    "cross-kind numeric identity cannot alias initial images" "HCIRL0004"
    (F.first_error result).code

let option_globals mode text =
  let module Source = Holyc_lib__Driver__Integer_source in
  let module Internal = Holyc_lib__Ir__Integer_globals in
  let module R = Semantic_function_resolution in
  let module Records = Semantic_function_record_classification in
  let session, config, source = F.inputs ~mode text in
  let parsed =
    Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  let ast = Option.get parsed.ast in
  let prepared =
    F.checked
      (Source.prepare_unit ~include_global_initializers:true session ~config
         ~span:ast.span ast)
  in
  let originals = Source.records prepared |> Records.declarations in
  let options = Test_globals_on_data_heap.data_heap_mask in
  let declarations =
    List.map
      (fun record ->
        let site =
          record |> Records.classified_declaration_source
          |> R.resolved_declaration_site
        in
        R.make_declaration_with_options ~compiler_option_mask:options
          ~function_:(R.declaration_site_function site)
          ~kind:R.Definition
        |> H.require_ok Fun.id)
      originals
  in
  let resolution =
    R.resolve
      ~table:(Session.semantic_symbols session)
      ~parent:
        (Source.frames prepared |> Frame.functions |> List.hd
       |> Frame.function_scope |> Semantic_symbol_table.parent |> Option.get)
      ~compilation_mode:(if mode = Preprocessor.Jit then R.Jit else R.Aot)
      declarations
    |> H.require_ok Fun.id
  in
  let states =
    List.map
      (fun record ->
        let state = Records.classified_declaration_state record in
        Records.make_declaration_state
          ~staging_mask:(Records.declaration_state_staging_mask state)
          ~compiler_option_mask:options ())
      originals
  in
  let records = Records.classify resolution states |> H.require_ok Fun.id in
  let globals =
    F.checked
      (Internal.create
         ~initializers:(Source.top_level prepared)
         ~span:ast.span
         (Source.global_records prepared))
    |> Internal.with_statics ~span:ast.span ~frames:(Source.frames prepared)
         ~functions:(Source.functions prepared)
         ~records
    |> F.checked
  in
  (ast.span, globals)

let nondefault_options () =
  List.iter
    (fun mode ->
      let span, globals =
        option_globals mode "I64 F(){static I64 n=42;return n;}42;"
      in
      let options = Test_globals_on_data_heap.data_heap_mask in
      let preparation =
        Holyc_lib__Driver__Integer_initializers.prepare ~max_steps:4 ~span
          ~globals ~top_calls:[] ~functions:[] ()
        |> F.checked
      in
      let slot = Prep.globals preparation |> Globals.statics |> List.hd in
      Alcotest.(check int64)
        "retain nondefault owner options" options
        (Globals.static_compiler_options slot);
      let storage = Globals.static_storage slot in
      Alcotest.(check bool)
        "static allocation remains code heap" true
        (Globals.storage_opcode storage
        =
        if mode = Preprocessor.Jit then Ir_opcode.Ic_imm_i64
        else Ir_opcode.Ic_abs_addr);
      Alcotest.(check (option int64))
        "constant image with data-heap option" (Some 42L)
        (Globals.storage_initial_bits storage))
    G.modes

let tests =
  [
    Alcotest.test_case "persistent counter" `Quick source_gate;
    Alcotest.test_case "recursive and separate function lifetime" `Quick
      lifetime;
    Alcotest.test_case "AOT zero image" `Quick zero_fill;
    Alcotest.test_case "updates and signedness" `Quick updates;
    Alcotest.test_case "definition-time constant preparation" `Quick
      definition_time;
    Alcotest.test_case "uninitialized and unsupported boundaries" `Quick
      boundaries;
    Alcotest.test_case "storage frame and preparation bounds" `Quick limits;
    Alcotest.test_case "immutable image and fresh replay" `Quick replay;
    Alcotest.test_case "canonical address and exact owner" `Quick
      address_ownership;
    Alcotest.test_case "private storage join rejects cross-table IDs" `Quick
      internal_storage_join;
    Alcotest.test_case "static code heap under nondefault options" `Quick
      nondefault_options;
  ]
