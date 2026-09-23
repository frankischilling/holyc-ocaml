open Holyc_lib
module Program = X86_64_program
module VM = Ir_integer_interpreter
module Runtime_calls = Ir_runtime_call_context
module Graph = Ir_block_graph
module Sequence = Ir_instruction_sequence
module Type = Semantic_type
module Opcode = Ir_opcode
module Prepared_default = Holyc_lib__Ir.Prepared_parameter_default
module Unit = Holyc_lib__Driver.Integer_unit

let require_ok show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let program_errors errors =
  errors
  |> List.map (fun (error : Program.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let source_inputs ~mode ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let compile_source ?max_ir_instructions ?max_code_bytes ?max_stack_bytes
    ?max_blocks ?max_initializer_steps ?max_default_bytes ~mode contents =
  let session, config, source =
    source_inputs ~mode ~path:"native-scalar-functions-test.hc" contents
  in
  Native_program.compile ?max_ir_instructions ?max_code_bytes ?max_stack_bytes
    ?max_blocks ?max_initializer_steps ?max_default_bytes session ~config
    ~source

let image ?max_ir_instructions ?max_code_bytes ?max_stack_bytes ?max_blocks
    ?max_initializer_steps ?max_default_bytes ~mode contents =
  compile_source ?max_ir_instructions ?max_code_bytes ?max_stack_bytes
    ?max_blocks ?max_initializer_steps ?max_default_bytes ~mode contents
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let reject_gate label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly compiled" label
  | Error [] -> Alcotest.failf "%s returned no diagnostic" label
  | Error diagnostics ->
      Alcotest.(check bool)
        (label ^ " rejects before native execution")
        false
        (List.exists
           (fun (error : Diagnostic.t) ->
             String.starts_with ~prefix:"HCIRVM" error.code
             || String.starts_with ~prefix:"HCNATIVE" error.code)
           diagnostics)

let reject_compile ?code label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly compiled" label
  | Error [] -> Alcotest.failf "%s returned no diagnostic" label
  | Error diagnostics ->
      Option.iter
        (fun expected ->
          Alcotest.(check bool)
            (label ^ " contains " ^ expected)
            true
            (List.exists
               (fun (error : Diagnostic.t) -> error.code = expected)
               diagnostics))
        code

let reject_backend label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly compiled" label
  | Error [] -> Alcotest.failf "%s returned no backend diagnostic" label
  | Error _ -> ()

let reject_backend_code code label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly compiled" label
  | Error [] -> Alcotest.failf "%s returned no backend diagnostic" label
  | Error errors ->
      Alcotest.(check bool)
        (label ^ " contains " ^ code)
        true
        (List.exists (fun (error : Program.error) -> error.code = code) errors)

let integer_unit ~mode contents =
  let session, config, source =
    source_inputs ~mode ~path:"native-scalar-authority.hc" contents
  in
  compile_integer_program session ~config ~source |> require_ok diagnostics_text
  |> fun checked -> checked.value

let native_batch_unit ~mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-scalar-batch-authority.hc"
    ~contents ()
  |> require_ok diagnostics_text
  |> fun fixture -> fixture.unit_

let compile_callable unit =
  Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
    ~max_ir_instructions:4096 ~max_code_bytes:65536
    ~runtime_calls:(integer_program_runtime_calls unit)
    ~initialization:(integer_program_initialization unit)
    ~entry:(integer_program_entry unit)
    ~functions:(integer_program_functions unit)
    ()

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let scalar_rows =
  [
    ("I8", "255");
    ("U8", "255");
    ("I16", "65535");
    ("U16", "65535");
    ("I32", "4294967295");
    ("U32", "4294967295");
    ("I64", "-1");
    ("U64", "0xffffffffffffffff");
  ]

let default_rows =
  [
    ("I8", "255", 3);
    ("U8", "554", 3);
    ("I16", "65535", 3);
    ("U16", "65578", 3);
    ("I32", "4294967295", 3);
    ("U32", "4294967338", 3);
    (* -1 is a literal plus the checked unary-minus operation. *)
    ("I64", "-1", 4);
    ("U64", "0xffffffffffffffff", 3);
  ]

let scalar_source_gate_both_modes () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, high) ->
          let source =
            Printf.sprintf "%s Echo(%s n){%s saved=n;return saved;}\nEcho(%s);"
              type_name type_name type_name high
          in
          let compiled = image ~mode source in
          Alcotest.(check int)
            (type_name ^ " emits one named scalar function")
            1
            (Program.function_count compiled);
          let wide =
            Printf.sprintf "%s Wide(){return %s;}\nWide();" type_name high
          in
          ignore (image ~mode wide))
        scalar_rows;
      List.iter
        (fun source -> ignore (image ~mode source))
        [
          "U0 V(){return;}\nV();";
          "U0i V(){return;}\nV();";
          "U0 V(){I8 n=42;if(n)return;}\nV();";
          "U0 V(){I16 n=42;n;}\nV();";
          "U0 R(I32 n){if(n){R(n-1);return;}}\nR(3);";
        ])
    modes

let narrow_defaults_prepare_exact_raw_payloads () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_name, literal, preparation_steps) ->
          let declaration =
            Printf.sprintf "%s Value(%s n=%s){return n;}" type_name type_name
              literal
          in
          List.iter
            (fun suffix ->
              ignore
                (image ~max_initializer_steps:preparation_steps
                   ~max_default_bytes:8 ~mode
                   (declaration ^ "\n" ^ suffix)))
            [ "Value();"; "Value(42);"; "42;" ];
          compile_source ~max_initializer_steps:(preparation_steps - 1)
            ~max_default_bytes:8 ~mode
            (declaration ^ "\nValue();")
          |> reject_compile ~code:"HCIRVM0007"
               (type_name ^ " one-below default preparation");
          compile_source ~max_initializer_steps:preparation_steps
            ~max_default_bytes:7 ~mode
            (declaration ^ "\nValue();")
          |> reject_compile ~code:"HCIRVM0011"
               (type_name ^ " one-below default payload"))
        default_rows;
      List.iter
        (fun source ->
          compile_source ~mode source
          |> reject_compile "non-constant native default")
        [
          "I64 Inc(I64 n){return n+1;} U8 F(U8 n=Inc(41)){return n;} F();";
          "I64 F(I64 n=\"A\"){return n;} F();";
          "I64 F(I64 n=lastclass){return n;} F();";
        ])
    modes;
  (* The prepared default retains the expression bits. Parameter entry performs
     the U8 narrowing later, so 554 is intentionally not pre-normalized to 42. *)
  List.iter
    (fun mode ->
      let unit = native_batch_unit ~mode "U8 F(U8 n=554){return n;}\nF();" in
      let runtime_calls = Unit.runtime_calls unit in
      let call_start =
        Unit.entry unit |> Ir_x87_stack.graph |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Sequence.instructions)
        |> List.map Sequence.description
        |> List.find (fun (description : Sequence.description) ->
            description.opcode = Opcode.Ic_call_start)
      in
      let call =
        Runtime_calls.find_start runtime_calls ~owner:Runtime_calls.Entry
          call_start.instruction_id
        |> Option.get
      in
      let argument =
        Runtime_calls.arguments call
        |> List.find (fun argument ->
            Runtime_calls.argument_role argument = Runtime_calls.Fixed 0)
      in
      let prepared =
        Runtime_calls.argument_prepared_default argument |> Option.get
      in
      Alcotest.(check int64)
        "U8 declaration saves raw 554 bits" 554L
        (Prepared_default.bits prepared))
    modes

let scalar_compile_limits_are_exact () =
  let source =
    "I8 A(I8 n){I8 x=n;return x;}\n\
     U8 B(U8 n){U8 x=n;return x;}\n\
     I16 C(I16 n){I16 x=n;return x;}\n\
     U16 D(U16 n){U16 x=n;return x;}\n\
     I32 E(I32 n){I32 x=n;return x;}\n\
     U32 F(U32 n){U32 x=n;return x;}\n\
     I64 G(I64 n){I64 x=n;return x;}\n\
     U64 H(U64 n){U64 x=n;return x;}\n\
     U0 V(I8 n){I16 x=n;if(x)return;}\n\
     A(42)+B(42)+C(42)+D(42)+E(42)+F(42)+G(42)+H(42);"
  in
  List.iter
    (fun mode ->
      let baseline = image ~mode source in
      Alcotest.(check int)
        "eight word functions plus one U0 function" 9
        (Program.function_count baseline);
      let ir = Program.ir_instructions baseline in
      let code = String.length (Program.code baseline) in
      let stack = Program.frame_bytes baseline in
      let blocks = Program.block_count baseline in
      Alcotest.(check bool) "scalar fixture has IR work" true (ir > 1);
      Alcotest.(check bool) "scalar fixture has code bytes" true (code > 1);
      Alcotest.(check bool) "scalar fixture has stack storage" true (stack > 0);
      Alcotest.(check bool)
        "scalar fixture has multiple blocks" true (blocks > 1);
      ignore
        (image ~max_ir_instructions:ir ~max_code_bytes:code
           ~max_stack_bytes:stack ~max_blocks:blocks ~mode source);
      compile_source ~max_ir_instructions:(ir - 1) ~mode source
      |> reject_compile ~code:"HCBACK0001" "scalar IR one-below";
      compile_source ~max_code_bytes:(code - 1) ~mode source
      |> reject_compile ~code:"HCBACK0005" "scalar code one-below";
      compile_source ~max_stack_bytes:(stack - 1) ~mode source
      |> reject_compile ~code:"HCBACK0004" "scalar stack one-below";
      compile_source ~max_blocks:(blocks - 1) ~mode source
      |> reject_compile ~code:"HCBACK0001" "scalar block one-below")
    modes

let unsupported_neighbors_stay_outside_native_gate () =
  let cases =
    [
      "Bool F(Bool n){return n;} F(1);";
      "I0 F(I0 n){return n;} F(1);";
      "F64 F(F64 n){return n;} F(1.0);";
      "I64 F(I64 **p){return **p;} 42;";
      "I8 G=1<<2; I64 F(){return G;} F();";
      "I64 F(){static I8 n={42};return n;} F();";
      "I64 F(I64 n,...){return n;} F(42);";
      "extern I64 F(I64 n); 42;";
      "U0 V(){return 42;} V();";
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          compile_source ~mode source
          |> reject_gate "unsupported native scalar neighbor")
        cases;
      List.iter
        (fun source ->
          compile_source ~mode source
          |> reject_compile ~code:"HCBACK0002"
               "narrow word function with reachable missing return")
        [
          "I8 F(I8 n){if(n)return 1;} 42;";
          "I8 F(I8 n){if(n)return 1;} F(1);";
          "U8 F(){} 42;";
          "U8 F(){} F();";
        ])
    modes

let scalar_frame_call_and_default_authority_are_exact () =
  List.iter
    (fun mode ->
      let left =
        integer_unit ~mode "I8 Left(I8 n){I8 x=n;return x;}\nLeft(42);"
      in
      let right =
        integer_unit ~mode "U8 Right(U8 n){U8 x=n;return x;}\nRight(42);"
      in
      ignore (compile_callable left |> require_ok program_errors);
      let left_function = List.hd (integer_program_functions left) in
      let right_function = List.hd (integer_program_functions right) in
      let forged : VM.function_definition =
        { frame = right_function.frame; body = left_function.body }
      in
      Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
        ~max_ir_instructions:4096 ~max_code_bytes:65536
        ~runtime_calls:(integer_program_runtime_calls left)
        ~initialization:(integer_program_initialization left)
        ~entry:(integer_program_entry left)
        ~functions:[ forged ] ()
      |> reject_backend "narrow body joined to foreign checked frame";
      Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
        ~max_ir_instructions:4096 ~max_code_bytes:65536
        ~runtime_calls:(integer_program_runtime_calls left)
        ~initialization:(integer_program_initialization left)
        ~entry:(integer_program_entry left)
        ~functions:[] ()
      |> reject_backend "narrow direct call without its checked definition";
      Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
        ~max_ir_instructions:4096 ~max_code_bytes:65536
        ~runtime_calls:(integer_program_runtime_calls right)
        ~initialization:(integer_program_initialization left)
        ~entry:(integer_program_entry left)
        ~functions:(integer_program_functions left)
        ()
      |> reject_backend "narrow bundle rejects a foreign runtime-call authority";
      let defaulted =
        native_batch_unit ~mode
          "U8 Defaulted(U8 n=554){return n;}\nDefaulted();"
      in
      Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
        ~max_ir_instructions:4096 ~max_code_bytes:65536
        ~runtime_calls:(Unit.runtime_calls defaulted)
        ~initialization:(Unit.initialization defaulted)
        ~entry:(Unit.entry defaulted) ~functions:(Unit.functions defaulted) ()
      |> reject_backend "narrow prepared default without native authority proof")
    modes

let pointer_source_and_authority () =
  let source =
    "I64 Add(I64 *p){*p+=2;return *p;}I64 F(){I64 x=40;I64 *p=&x;return \
     Add(p);}F();"
  in
  List.iter
    (fun mode ->
      let unit = integer_unit ~mode source in
      let other = integer_unit ~mode source in
      List.iter
        (fun abi ->
          let compile ?(entry = integer_program_entry unit)
              ?(functions = integer_program_functions unit)
              ?(calls = integer_program_runtime_calls unit) () =
            Program.compile_callable ~status_abi:abi ~max_stack_bytes:4088
              ~max_blocks:4096 ~max_ir_instructions:4096 ~max_code_bytes:65536
              ~runtime_calls:calls
              ~initialization:(integer_program_initialization unit)
              ~entry ~functions ()
          in
          ignore (compile () |> require_ok program_errors);
          compile ~calls:(integer_program_runtime_calls other) ()
          |> reject_backend "foreign pointer call context";
          compile ~entry:(integer_program_entry other) ()
          |> reject_backend "foreign pointer entry";
          compile ~functions:(integer_program_functions other) ()
          |> reject_backend "foreign pointer functions";
          let functions = integer_program_functions unit in
          let first = List.hd functions in
          compile
            ~functions:
              ({ first with frame = Obj.obj (Obj.dup (Obj.repr first.frame)) }
              :: List.tl functions)
            ()
          |> reject_backend "reconstructed pointer frame")
        [ Program.Windows_x64; Program.System_v_x64 ];
      let sample = image ~mode source in
      let stack = Program.frame_bytes sample in
      ignore (image ~mode ~max_stack_bytes:stack source);
      compile_source ~mode ~max_stack_bytes:(stack - 1) source
      |> reject_compile ~code:"HCBACK0004" "reference bookkeeping stack quota";
      List.iter
        (fun source ->
          compile_source ~mode source
          |> reject_gate "unsupported reference escape or fabrication")
        [
          "I64 *F(){I64 x;return &x;}42;";
          "I64 *G;42;";
          "I64 F(){static I64 *p;return 42;}42;";
          "I64 F(I64 **p){return **p;}42;";
          "I64 F(I64 *p=0){return *p;}42;";
          "I64 F(){I64 *p=0;return *p;}42;";
          "I64 F(){I64 x=42;I64 *p=&x;return p;}F();";
          "I64 F(){I64 x=42;I64 *p=&x;return p(I64);}F();";
          "I64 F(){I64 x=42;I64 *p=&x;p++;return *p;}F();";
          "I64 F(){I64 x=42;I64 *p=&x;return *(p+1);}F();";
          "I64 F(){U64 x=42;I64 *p=&x;return *p;}F();";
          "I64 F(I64 *p){return *p;}F(42);";
          "I64 F(){I64 x=42;I64 *p=&x;I64 **q=&p;return **q;}F();";
        ];
      List.iter
        (fun (label, select, mutate) ->
          let unit = integer_unit ~mode source in
          ignore (compile_callable unit |> require_ok program_errors);
          let function_ = List.nth (integer_program_functions unit) 1 in
          let graph =
            Ir_function_body.x87 function_.body |> Ir_x87_stack.graph
          in
          let rec find = function
            | [] -> None
            | instruction :: rest as cell ->
                let d = Sequence.description instruction in
                if select d then Some (cell, d) else find rest
          in
          let cell, d =
            Graph.blocks graph
            |> List.find_map (fun b ->
                find (Graph.instructions b |> Sequence.instructions))
            |> Option.get
          in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (mutate d));
          compile_callable unit |> reject_backend label)
        [
          ( "address flags",
            (fun d -> d.Sequence.opcode = Opcode.Ic_addr),
            fun d -> { d with flags = 1L } );
          ( "raw integer address",
            (fun d -> d.Sequence.opcode = Opcode.Ic_addr),
            fun d ->
              {
                d with
                opcode = Opcode.Ic_imm_i64;
                operands = [];
                payload = Some (Sequence.Integer 1L);
              } );
          ( "address payload",
            (fun d -> d.Sequence.opcode = Opcode.Ic_addr),
            fun d -> { d with payload = Some (Sequence.Integer 0L) } );
          ( "missing address operand",
            (fun d -> d.Sequence.opcode = Opcode.Ic_addr),
            fun d -> { d with operands = [] } );
          ( "outside exact frame",
            (fun d ->
              d.Sequence.opcode = Opcode.Ic_imm_i64
              && Option.fold ~none:false
                   ~some:(fun t -> Type.pointer_depth t > 0)
                   d.target_type),
            fun d -> { d with payload = Some (Sequence.Integer (-4096L)) } );
        ])
    modes

let borrowed_static_reference_does_not_grant_symbol_authority () =
  List.iter
    (fun mode ->
      let unit =
        integer_unit ~mode
          "I64 Add(I64 *p){static I64 private;private=0;return *p+=2;}I64 \
           F(){static I64 shared;shared=40;return Add(&shared);}F();"
      in
      ignore (compile_callable unit |> require_ok program_errors);
      let first = List.hd (integer_program_functions unit) in
      let statics = Ir_integer_globals.statics (integer_program_globals unit) in
      let foreign =
        List.nth statics 1 |> Ir_integer_globals.static_storage
        |> Ir_integer_globals.storage_symbol
      in
      let graph = Ir_function_body.x87 first.body |> Ir_x87_stack.graph in
      let rec find = function
        | [] -> None
        | instruction :: rest as cell -> (
            let d = Sequence.description instruction in
            match d.payload with
            | Some (Sequence.Symbol _) -> Some (cell, d)
            | _ -> find rest)
      in
      let cell, d =
        Graph.blocks graph
        |> List.find_map (fun b ->
            find (Graph.instructions b |> Sequence.instructions))
        |> Option.get
      in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { d with payload = Some (Sequence.Symbol foreign) });
      compile_callable unit
      |> reject_backend
           "borrowed reference cannot synthesize caller static symbol")
    modes

let automatic_array_layout () =
  List.iter
    (fun mode ->
      List.iter
        (fun type_ ->
          List.iter
            (fun status_abi ->
              let session, config, source =
                source_inputs ~mode ~path:"array-layout.hc"
                  (Printf.sprintf
                     "%s F(){%s a[2][3];a[0][0]=40;a[1][2]=2;return \
                      a[0][0]+a[1][2];}F();"
                     "I64" type_)
              in
              ignore
                (Native_program.compile ~status_abi session ~config ~source
                |> require_ok diagnostics_text))
            [ Program.Windows_x64; Program.System_v_x64 ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ];
      let indexed = "I64 F(){I16 a[3][7];a[1][2]=42;return a[1][2];}F();" in
      let unit = integer_unit ~mode indexed in
      let other = integer_unit ~mode indexed in
      let original = List.hd (integer_program_functions unit) in
      let compile functions =
        Program.compile_callable ~max_ir_instructions:4096 ~max_code_bytes:65536
          ~runtime_calls:(integer_program_runtime_calls unit)
          ~initialization:(integer_program_initialization unit)
          ~entry:(integer_program_entry unit)
          ~functions ()
      in
      ignore (compile [ original ] |> require_ok program_errors);
      compile
        [
          {
            original with
            frame = (List.hd (integer_program_functions other)).frame;
          };
        ]
      |> reject_backend "array body joined to foreign frame";
      compile
        [
          { original with frame = Obj.obj (Obj.dup (Obj.repr original.frame)) };
        ]
      |> reject_backend "array body joined to reconstructed frame";
      let graph = Ir_function_body.x87 original.body |> Ir_x87_stack.graph in
      let rec find_stride = function
        | [] -> None
        | instruction :: rest as cell ->
            let d = Sequence.description instruction in
            if
              d.opcode = Opcode.Ic_imm_i64
              && d.payload = Some (Sequence.Integer 14L)
            then Some (cell, d)
            else find_stride rest
      in
      let cell, stride =
        Graph.blocks graph
        |> List.find_map (fun block ->
            find_stride (Graph.instructions block |> Sequence.instructions))
        |> Option.get
      in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { stride with payload = Some (Sequence.Integer 12L) });
      compile [ original ]
      |> reject_backend_code "HCBACK0003"
           "array stride must match its original dimension metadata";
      let contents = indexed in
      let compiled = image ~mode contents in
      let bytes = Program.frame_bytes compiled in
      ignore (image ~mode ~max_stack_bytes:bytes contents);
      compile_source ~mode ~max_stack_bytes:(bytes - 1) contents
      |> reject_compile ~code:"HCBACK0004" "array private frame one below";
      List.iter
        (fun contents ->
          ignore (compile_source ~mode contents |> require_ok diagnostics_text))
        [ "I64 F(){static I8 a[2];return 42;}F();"; "I8 G[2];42;" ];
      List.iter
        (fun contents ->
          compile_source ~mode contents
          |> reject_compile "array unsupported storage or addressing")
        [
          "I64 F(){I8 a[2]={1,2};return 42;}F();";
          "I64 F(){I8 *a[2];return 42;}F();";
          "I8 *F(){I8 a[2];return a;}42;";
          "I64 F(){I8 a[0];return 42;}F();";
          "I64 F(){I8 a[1000000000];return 42;}F();";
          "I64 F(){I8 a[9223372036854775807][2];return 42;}F();";
          "I64 Count(){return 2;}I64 F(){I8 a[Count()];return 42;}F();";
        ])
    modes

let tests =
  [
    Alcotest.test_case "original automatic array dimensions and bounded layout"
      `Quick automatic_array_layout;
    Alcotest.test_case
      "borrowed static reference does not grant symbol ownership" `Quick
      borrowed_static_reference_does_not_grant_symbol_authority;
    Alcotest.test_case "native pointer source and exact authority boundaries"
      `Quick pointer_source_and_authority;
    Alcotest.test_case
      "all scalar signatures and U0 bodies pass source preflight" `Quick
      scalar_source_gate_both_modes;
    Alcotest.test_case
      "all scalar defaults retain raw payloads and exact preparation bounds"
      `Quick narrow_defaults_prepare_exact_raw_payloads;
    Alcotest.test_case "scalar callable input limits are exact" `Quick
      scalar_compile_limits_are_exact;
    Alcotest.test_case "unsupported neighboring source forms remain rejected"
      `Quick unsupported_neighbors_stay_outside_native_gate;
    Alcotest.test_case "narrow frame call and default authority remain exact"
      `Quick scalar_frame_call_and_default_authority_are_exact;
  ]
