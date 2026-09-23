open Holyc_lib
module Program = X86_64_program
module Native = Native_program
module Layout = Holyc_lib__Backend.X86_64_global_storage
module Globals = Ir_integer_globals
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module X87 = Ir_x87_stack
module Opcode = Ir_opcode
module Type = Semantic_type
module Runtime_calls = Ir_runtime_call_context
module Declarations = Task_declarations
module Default_preparation = Holyc_lib__Driver.Native_default_preparation

let require_ok show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let ( let* ) = Result.bind

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let program_errors errors =
  errors
  |> List.map (fun (error : Program.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let layout_errors errors =
  errors
  |> List.map (fun (error : Layout.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-persistent-storage.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let integer_unit ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  compile_integer_program session ~config ~source |> require_ok diagnostics_text
  |> fun checked -> checked.value

let native_image ?max_global_bytes ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  Native.compile ?max_global_bytes session ~config ~source
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let compile_callable ?(max_global_bytes = 1_048_576) unit =
  Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
    ~max_ir_instructions:4096 ~max_code_bytes:65536 ~max_global_bytes
    ~runtime_calls:(integer_program_runtime_calls unit)
    ~initialization:(integer_program_initialization unit)
    ~entry:(integer_program_entry unit)
    ~functions:(integer_program_functions unit)
    ()

let reject_program ~code label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly compiled" label
  | Error errors ->
      Alcotest.(check bool)
        (label ^ " code") true
        (List.exists (fun (error : Program.error) -> error.code = code) errors)

let reject_layout ~code label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly sealed storage" label
  | Error errors ->
      Alcotest.(check bool)
        (label ^ " code") true
        (List.exists (fun (error : Layout.error) -> error.code = code) errors)

let internal_u64 =
  Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive_type.U64
    ~pointer_depth:0
  |> require_ok Fun.id

let byte image offset = Char.code image.[offset]

let check_zero_range label image first length =
  for offset = first to first + length - 1 do
    Alcotest.(check int)
      (Printf.sprintf "%s byte %d" label offset)
      0 (byte image offset)
  done

let check_word image ~offset ~width bits label =
  for index = 0 to width - 1 do
    let expected =
      Int64.shift_right_logical bits (index * 8)
      |> Int64.logand 255L |> Int64.to_int
    in
    Alcotest.(check int)
      (Printf.sprintf "%s byte %d" label index)
      expected
      (byte image (offset + index))
  done

let find_cell graph predicate =
  let rec find = function
    | [] -> None
    | head :: rest as cell ->
        let description = Sequence.description head in
        if predicate description then Some (cell, description) else find rest
  in
  match
    Graph.blocks graph
    |> List.find_map (fun block ->
        Graph.instructions block |> Sequence.instructions |> find)
  with
  | Some found -> found
  | None -> Alcotest.fail "expected persistent-array instruction"

let mutate graph predicate change =
  let cell, description = find_cell graph predicate in
  Obj.set_field (Obj.repr cell) 0 (Obj.repr (change description))

let symbol_payload (description : Sequence.description) =
  match description.payload with
  | Some (Sequence.Symbol _) -> true
  | _ -> false

let native_defaults mode =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-persistent-default.hc"
      ~contents:"I64 F(I64 n=42){return n;}42;"
  in
  let table = Session.semantic_symbols session in
  let ledger =
    Declarations.create_source session ~source |> require_ok Fun.id
  in
  let preparation =
    Default_preparation.create ~compilation_mode:mode ~max_initializer_steps:100
      session
    |> require_ok Fun.id
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (Declarations.observe_command ledger);
      query = Some (Declarations.observe_query ledger);
      reference = Some (Declarations.observe_reference ledger);
      implicit_output = None;
      call = None;
      declaration =
        Some
          (fun event ->
            let* () = Declarations.observe ledger event in
            match event with
            | Parser.Parameter_default_completed receipt ->
                Default_preparation.prepare preparation ~session ~ledger receipt
            | Parser.Function_header_completed header ->
                Declarations.complete_source_defaults ledger header
            | _ -> Ok ());
      dimension_count = Some (Declarations.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  if Parser.has_errors output then
    Alcotest.fail (diagnostics_text output.diagnostics);
  let ast = Option.get output.ast in
  let source_command =
    Declarations.seal_source ledger ast |> require_ok diagnostics_text
  in
  Declarations.native_source_defaults ~table ~ast source_command
  |> require_ok diagnostics_text

let persistent_authority () =
  let global_source = "I64 G[2][3];G[1][2]=42;G[1][2];" in
  let static_source =
    "I64 F(){static U16 A[2][3];A[1][2]=42;return A[1][2];}F();"
  in
  List.iter
    (fun mode ->
      let unit = integer_unit ~mode global_source in
      let other = integer_unit ~mode global_source in
      let globals = integer_program_globals unit in
      let source_slot = Globals.slots globals |> List.hd in
      let symbol = Globals.slot_symbol source_slot in
      let layout =
        Layout.create ~functions:[] ~max_global_bytes:48
          ~initialization:(integer_program_initialization unit)
          ~entry:(integer_program_entry unit)
        |> require_ok layout_errors
      in
      let slot = Layout.find_symbol layout symbol |> Option.get in
      Alcotest.(check (list int64))
        "global dimensions" [ 2L; 3L ] (Layout.dimensions slot);
      Alcotest.(check (list int64))
        "global strides" [ 24L; 8L ] (Layout.strides slot);
      Alcotest.(check int) "global elements" 6 (Layout.element_count slot);
      Alcotest.(check int) "global extent" 48 (Layout.extent_bytes slot);
      let initial = Layout.image layout in
      let exported = Layout.image layout in
      Bytes.fill
        (Bytes.unsafe_of_string exported)
        0 (String.length exported) '\255';
      Alcotest.(check string)
        "exported storage does not mutate the sealed image" initial
        (Layout.image layout);
      Alcotest.(check bool)
        "foreign equal source has no authority" true
        (Globals.slots (integer_program_globals other)
        |> List.hd |> Globals.slot_symbol |> Layout.find_symbol layout
        |> Option.is_none);
      ignore
        (compile_callable ~max_global_bytes:48 unit |> require_ok program_errors);
      List.iter
        (fun (label, predicate, change) ->
          let damaged = integer_unit ~mode global_source in
          let graph = integer_program_entry damaged |> X87.graph in
          mutate graph predicate change;
          compile_callable ~max_global_bytes:48 damaged
          |> reject_program ~code:"HCBACK0003" label)
        [
          ( "global address opcode authority",
            symbol_payload,
            fun description ->
              {
                description with
                opcode =
                  (if mode = Preprocessor.Jit then Opcode.Ic_abs_addr
                   else Opcode.Ic_imm_i64);
              } );
          ( "global address type authority",
            symbol_payload,
            fun description ->
              {
                description with
                target_type =
                  Some (Type.pointer_to internal_u64 |> require_ok Fun.id);
              } );
          ( "global dimension stride authority",
            (fun description ->
              description.opcode = Opcode.Ic_imm_i64
              && description.payload = Some (Sequence.Integer 24L)
              && Option.fold ~none:false
                   ~some:(fun type_ -> Type.pointer_depth type_ = 1)
                   description.target_type),
            fun description ->
              { description with payload = Some (Sequence.Integer 16L) } );
        ];
      let static_unit = integer_unit ~mode static_source in
      let functions = integer_program_functions static_unit in
      let definition = List.hd functions in
      let static =
        Globals.statics (integer_program_globals static_unit) |> List.hd
      in
      let static_symbol =
        Globals.static_storage static |> Globals.storage_symbol
      in
      let static_layout =
        Layout.create ~functions ~max_global_bytes:16
          ~initialization:(integer_program_initialization static_unit)
          ~entry:(integer_program_entry static_unit)
        |> require_ok layout_errors
      in
      let static_slot =
        Layout.find_symbol static_layout static_symbol |> Option.get
      in
      Alcotest.(check (list int64))
        "static dimensions" [ 2L; 3L ]
        (Layout.dimensions static_slot);
      Alcotest.(check (list int64))
        "static strides" [ 6L; 2L ]
        (Layout.strides static_slot);
      Alcotest.(check int)
        "static elements" 6
        (Layout.element_count static_slot);
      Alcotest.(check int)
        "static object extent excludes padding" 12
        (Layout.extent_bytes static_slot);
      Alcotest.(check bool)
        "static exact function owner" true
        (Layout.owns_address static_slot
           (Runtime_calls.Function definition.body));
      Alcotest.(check bool)
        "static entry is foreign" false
        (Layout.owns_address static_slot Runtime_calls.Entry);
      let reconstructed =
        [
          {
            definition with
            frame = Obj.obj (Obj.dup (Obj.repr definition.frame));
          };
        ]
      in
      Layout.create ~functions:reconstructed ~max_global_bytes:16
        ~initialization:(integer_program_initialization static_unit)
        ~entry:(integer_program_entry static_unit)
      |> reject_layout ~code:"HCBACK0003" "reconstructed static array frame")
    [ Preprocessor.Jit; Preprocessor.Aot ]

let check_array_flags ~mode ~logical ~initialized image =
  let expected = if initialized || mode = Preprocessor.Aot then 1 else 0 in
  Alcotest.(check int) "reserved object flag byte" 0 (byte image logical);
  let low_flag = logical + 1 in
  let root_flag = low_flag + 8 in
  Alcotest.(check int) "array element 1 flag" expected (byte image low_flag);
  Alcotest.(check int)
    "array element 0 root flag" expected (byte image root_flag);
  check_zero_range "element 1 flag padding" image (low_flag + 1) 7;
  check_zero_range "element 0 flag padding" image (root_flag + 1) 7

let global_images_across_widths () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, width, first, second) ->
          let logical = width * 2 in
          let empty =
            native_image ~max_global_bytes:logical ~mode
              (Printf.sprintf "%s G[2];42;" type_)
          in
          Alcotest.(check int)
            "global logical bytes" logical
            (Program.global_bytes empty);
          Alcotest.(check int)
            "array metadata bytes" 17
            (Program.arena_metadata_bytes empty);
          Alcotest.(check int)
            "array physical bytes" (logical + 17)
            (String.length (Program.global_image empty));
          let empty_image = Program.global_image empty in
          check_zero_range "uninitialized data" empty_image 0 logical;
          check_array_flags ~mode ~logical ~initialized:false empty_image;
          let prepared =
            native_image ~max_global_bytes:logical ~mode
              (Printf.sprintf "%s G[2]={%s,%s};G[0]+G[1];" type_ first second)
          in
          let prepared_image = Program.global_image prepared in
          check_word prepared_image ~offset:0 ~width 40L "prepared first";
          check_word prepared_image ~offset:width ~width 2L "prepared second";
          check_array_flags ~mode ~logical ~initialized:true prepared_image)
        [
          ("U8", 1, "296", "258");
          ("U16", 2, "65576", "65538");
          ("U32", 4, "4294967336", "4294967298");
          ("I64", 8, "40", "2");
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let static_images_and_padding () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, width) ->
          let extent = width * 2 in
          let logical = if extent <= 8 then 8 else 16 in
          let source =
            Printf.sprintf
              "I64 F(){static %s A[2]={40,2};return A[0]+A[1];}F();" type_
          in
          let image = native_image ~max_global_bytes:logical ~mode source in
          Alcotest.(check int)
            "padded static logical bytes" logical
            (Program.global_bytes image);
          Alcotest.(check int)
            "static array metadata bytes" 17
            (Program.arena_metadata_bytes image);
          let bytes = Program.global_image image in
          Alcotest.(check int)
            "static physical bytes" (logical + 17) (String.length bytes);
          check_word bytes ~offset:0 ~width 40L "static first";
          check_word bytes ~offset:width ~width 2L "static second";
          if extent < logical then
            check_zero_range "static allocation padding" bytes extent
              (logical - extent);
          check_array_flags ~mode ~logical ~initialized:true bytes)
        [ ("U8", 1); ("U16", 2); ("U32", 4); ("I64", 8) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let preparation_proof_and_metadata_quotas () =
  let source = "U8 G[2]={40,2};G[0]+G[1];" in
  List.iter
    (fun mode ->
      let unit = integer_unit ~mode source in
      compile_callable ~max_global_bytes:2 unit
      |> reject_program ~code:"HCBACK0002" "missing original preparation proof";
      Layout.create ~functions:[] ~max_global_bytes:2
        ~initialization:(integer_program_initialization unit)
        ~entry:(integer_program_entry unit)
      |> reject_layout ~code:"HCBACK0002" "raw materialized array image";
      let defaults = native_defaults mode in
      Alcotest.(check bool)
        "prepared array rejects native source defaults" true
        (Result.is_error
           (Holyc_lib__Ir.Integer_globals.with_native_source_defaults
              (integer_program_globals unit)
              defaults));
      let prepared = native_image ~max_global_bytes:2 ~mode source in
      Alcotest.(check int)
        "prepared logical quota" 2
        (Program.global_bytes prepared);
      Alcotest.(check int)
        "prepared metadata accounting" 17
        (Program.arena_metadata_bytes prepared);
      Alcotest.(check int)
        "prepared exact physical image" 19
        (String.length (Program.global_image prepared));
      let session, config, source_file = source_inputs ~mode source in
      match
        Native.compile ~max_global_bytes:1 session ~config ~source:source_file
      with
      | Error diagnostics ->
          Alcotest.(check bool)
            "logical quota one below" true
            (List.exists
               (fun (d : Diagnostic.t) -> d.code = "HCBACK0001")
               diagnostics)
      | Ok _ -> Alcotest.fail "array metadata escaped logical global quota")
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let exact_elements = (Layout.hard_max_arena_bytes - 1) / 9 in
  let exact_source = Printf.sprintf "U8 G[%d];42;" exact_elements in
  let exact_unit = integer_unit ~mode:Preprocessor.Jit exact_source in
  let exact =
    Layout.create ~functions:[] ~max_global_bytes:exact_elements
      ~initialization:(integer_program_initialization exact_unit)
      ~entry:(integer_program_entry exact_unit)
    |> require_ok layout_errors
  in
  Alcotest.(check int)
    "largest 1-byte array arena under hard bound"
    ((9 * exact_elements) + 1)
    (String.length (Layout.image exact));
  let too_many = exact_elements + 1 in
  let too_many_unit =
    integer_unit ~mode:Preprocessor.Jit (Printf.sprintf "U8 G[%d];42;" too_many)
  in
  Layout.create ~functions:[] ~max_global_bytes:too_many
    ~initialization:(integer_program_initialization too_many_unit)
    ~entry:(integer_program_entry too_many_unit)
  |> reject_layout ~code:"HCBACK0001" "array metadata hard bound"

let tests =
  [
    Alcotest.test_case "persistent array source authority is exact" `Quick
      persistent_authority;
    Alcotest.test_case "persistent global images retain widths and flags" `Quick
      global_images_across_widths;
    Alcotest.test_case "persistent static images retain padding and flags"
      `Quick static_images_and_padding;
    Alcotest.test_case "persistent preparation proof and metadata quotas" `Slow
      preparation_proof_and_metadata_quotas;
  ]
