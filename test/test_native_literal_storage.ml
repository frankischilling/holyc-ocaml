open Holyc_lib
module H = Test_native_program
module Storage = Holyc_lib__Backend.X86_64_literal_storage
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module Runtime = Ir_runtime_call_context
module Function = Ir_function_body
module Type = Semantic_type

let show errors =
  errors
  |> List.map (fun (error : Storage.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let create ?(literal_limit = 1024) ?(arena_limit = 4096) ?(prefix = 0) unit =
  Storage.create ~max_literal_bytes:literal_limit ~max_arena_bytes:arena_limit
    ~arena_prefix_bytes:prefix
    ~runtime_calls:(integer_program_runtime_calls unit)
    ~initialization:(integer_program_initialization unit)
    ~entry:(integer_program_entry unit)
    ~functions:(integer_program_functions unit)

let literals graph =
  Graph.blocks graph
  |> List.concat_map (fun block ->
      Graph.instructions block |> Sequence.instructions
      |> List.filter_map (fun instruction ->
          let description = Sequence.description instruction in
          if description.opcode = Ir_opcode.Ic_str_const then Some description
          else None))

let region storage owner graph description =
  Storage.find storage ~owner ~graph description.Sequence.instruction_id
  |> Option.get

let rejects label = function
  | Error (_ :: _) -> ()
  | Error [] -> Alcotest.fail (label ^ " returned no error")
  | Ok _ -> Alcotest.fail (label ^ " acquired native literal storage")

let distinct_original_regions () =
  List.iter
    (fun mode ->
      let source = "I64 F(){U8 *a=\"*\",*b=\"*\";return *a+*b;}42;" in
      let unit = H.integer_unit ~mode source in
      let storage = create ~prefix:7 unit |> H.require_ok show in
      let fn = List.hd (integer_program_functions unit) in
      let graph = Function.body fn.body in
      let descriptions = literals graph in
      Alcotest.(check int)
        "two original literal producers" 2 (List.length descriptions);
      let first =
        region storage (Runtime.Function fn.body) graph
          (List.nth descriptions 0)
      in
      let second =
        region storage (Runtime.Function fn.body) graph
          (List.nth descriptions 1)
      in
      Alcotest.(check int)
        "both terminators are owned" 4
        (Storage.literal_bytes storage);
      Alcotest.(check bool)
        "equal text has distinct object addresses" true
        (Storage.data_offset first <> Storage.data_offset second);
      Alcotest.(check bool)
        "equal text has distinct canonical tables" true
        (Storage.table_offset first <> Storage.table_offset second);
      let image = Storage.image storage in
      List.iter
        (fun region ->
          Alcotest.(check int) "exact byte extent" 2 (Storage.byte_count region);
          Alcotest.(check string)
            "payload plus terminator" "*\000"
            (String.sub image (Storage.data_offset region - 7) 2);
          Alcotest.(check bool)
            "table is after its data and fits the suffix" true
            (Storage.table_offset region >= Storage.data_offset region + 2
            && Storage.table_offset region - 7 + 96 <= String.length image))
        [ first; second ];
      Alcotest.(check int)
        "logical and private bytes cover suffix" (String.length image)
        (Storage.literal_bytes storage + Storage.metadata_bytes storage);
      let original = Bytes.to_string (Bytes.of_string image) in
      Bytes.fill (Bytes.unsafe_of_string image) 0 (String.length image) '\255';
      Alcotest.(check string)
        "export does not alias sealed bytes" original (Storage.image storage);
      let foreign = H.integer_unit ~mode source in
      let foreign_fn = List.hd (integer_program_functions foreign) in
      Alcotest.(check bool)
        "foreign same-text function owner" true
        (Option.is_none
           (Storage.find storage ~owner:(Runtime.Function foreign_fn.body)
              ~graph (List.hd descriptions).instruction_id));
      Alcotest.(check bool)
        "foreign same-text graph" true
        (Option.is_none
           (Storage.find storage ~owner:(Runtime.Function fn.body)
              ~graph:(Function.body foreign_fn.body)
              (List.hd descriptions).instruction_id));
      Alcotest.(check bool)
        "entry cannot borrow a function producer" true
        (Option.is_none
           (Storage.find storage ~owner:Runtime.Entry ~graph
              (List.hd descriptions).instruction_id));
      rejects "foreign callable context"
        (Storage.create ~max_literal_bytes:1024 ~max_arena_bytes:4096
           ~arena_prefix_bytes:0
           ~runtime_calls:(integer_program_runtime_calls foreign)
           ~initialization:(integer_program_initialization unit)
           ~entry:(integer_program_entry unit)
           ~functions:(integer_program_functions unit)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let quotas_before_expansion () =
  let unit = H.integer_unit "I64 F(){return \"*\"[0];}42;" in
  let exact =
    create ~literal_limit:2 ~arena_limit:98 unit |> H.require_ok show
  in
  Alcotest.(check int)
    "two bytes and three reference records" 98
    (String.length (Storage.image exact));
  rejects "one-below literal bytes" (create ~literal_limit:1 unit);
  rejects "one-below data and table bytes" (create ~arena_limit:97 unit);
  let prefixed =
    create ~literal_limit:2 ~arena_limit:105 ~prefix:7 unit |> H.require_ok show
  in
  Alcotest.(check int)
    "prefix is not duplicated in suffix" 98
    (String.length (Storage.image prefixed));
  rejects "prefix consumes arena quota" (create ~arena_limit:104 ~prefix:7 unit);
  rejects "near-cap prefix checked before image allocation"
    (create ~arena_limit:33_554_432 ~prefix:33_554_400 unit);
  List.iter
    (fun prefix -> rejects "invalid arena prefix" (create ~prefix unit))
    [ -1; 4097 ];
  List.iter
    (fun literal_limit ->
      rejects "invalid literal quota" (create ~literal_limit unit))
    [ 0; -1; Storage.hard_max_literal_bytes + 1 ];
  let unit = H.integer_unit "I64 F(){return \"A\\0B\"[2];}42;" in
  let storage = create ~literal_limit:4 unit |> H.require_ok show in
  let fn = List.hd (integer_program_functions unit) in
  let graph = Function.body fn.body in
  let literal =
    region storage (Runtime.Function fn.body) graph (List.hd (literals graph))
  in
  Alcotest.(check string)
    "embedded NUL does not shorten original extent" "A\000B\000"
    (String.sub (Storage.image storage) (Storage.data_offset literal) 4)

let malformed_producers () =
  let pointer form primitive =
    Type.make_primitive ~form ~primitive ~pointer_depth:1 |> H.require_ok Fun.id
  in
  let mutations =
    [
      ( "absent bytes",
        fun (d : Sequence.description) -> { d with payload = None } );
      ( "integer payload",
        fun d -> { d with payload = Some (Sequence.Integer 42L) } );
      ( "public literal type",
        fun d ->
          {
            d with
            target_type = Some (pointer Type.Public_spelling Primitive_type.U8);
          } );
      ( "wider literal type",
        fun d ->
          {
            d with
            target_type = Some (pointer Type.Public_spelling Primitive_type.U64);
          } );
      ("literal flags", fun d -> { d with flags = 1L });
      ("unowned literal argument flag", fun d -> { d with flags = 0x2000L });
      ("missing result", fun d -> { d with result = None });
    ]
  in
  List.iter
    (fun (label, transform) ->
      let unit = H.integer_unit "I64 F(){return \"*\"[0];}F();" in
      ignore (create unit |> H.require_ok show);
      let fn = List.hd (integer_program_functions unit) in
      let rec producer = function
        | [] -> None
        | instruction :: rest as cell ->
            let d = Sequence.description instruction in
            if d.opcode = Ir_opcode.Ic_str_const then Some (cell, d)
            else producer rest
      in
      let cell, d =
        Graph.blocks (Function.body fn.body)
        |> List.find_map (fun block ->
            producer (Graph.instructions block |> Sequence.instructions))
        |> Option.get
      in
      (* Retain the exact sealed graph while corrupting the producer. The storage
         consumer must reject the malformed description before expanding bytes. *)
      Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
      rejects label (create unit))
    mutations

let tests =
  [
    Alcotest.test_case "literal regions retain exact original owners" `Quick
      distinct_original_regions;
    Alcotest.test_case "literal and private table quotas precede expansion"
      `Quick quotas_before_expansion;
    Alcotest.test_case "malformed original producers cannot acquire bytes"
      `Quick malformed_producers;
  ]
