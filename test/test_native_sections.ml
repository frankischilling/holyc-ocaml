open Holyc_lib
module S = Native_sections

let get = function
  | Ok x -> x
  | Error (e : S.error) -> Alcotest.fail e.message

let fails label = function
  | Ok _ -> Alcotest.fail (label ^ " succeeded")
  | Error _ -> ()

let section ?(alignment_bits = 0) ?(kind = S.Code) id name body =
  S.{ id; name; kind; alignment_bits; bytes = body }

let symbol ?(scope = None) ?(binding = S.Local) id name definition =
  S.
    {
      id;
      name;
      scope;
      binding;
      source_identity = None;
      definition;
      source = no_source;
    }

let at section offset = S.{ section; offset }

let set ?(phase = S.Compile) ?(range = S.Signed) ?(addend = 0L) ?relative_to
    section offset width target =
  S.
    {
      field = at section offset;
      width;
      intent = Set { target; addend; relative_to; range };
      phase;
      source = no_source;
    }

let create ?max_bytes ?max_work ?position ?(mode = S.Aot) ?(symbols = [])
    ?(relocations = []) ?(mappings = []) sections =
  S.create ?max_bytes ?max_work ?position ~mode ~sections ~symbols ~relocations
    ~mappings ()

let hex = Test_bin_module.hex

let forward_sections () =
  let object_ =
    get
      (create
         [
           section 37 ".text" "\xe9\000\000\000\000\xc3";
           section ~alignment_bits:3 ~kind:S.Data 91 ".data"
             (String.make 8 '\000');
         ]
         ~symbols:
           [
             symbol 100003 "datum" (S.Defined (at 91 4));
             symbol ~binding:S.Export 8 "entry" (S.Defined (at 37 0));
           ]
         ~relocations:
           [
             set ~relative_to:(at 37 5) 37 1 4 (S.Symbol 100003);
             set ~phase:S.Load ~range:S.Unsigned 91 0 4 (S.Symbol 8);
           ])
  in
  Alcotest.(check (option int))
    "aligned data" (Some 8)
    (S.section_offset object_ 91);
  Alcotest.(check (list int))
    "symbol order is supplied order" [ 100003; 8 ]
    (List.map (fun (s : S.symbol) -> s.id) (S.symbols object_));
  List.iter
    (fun (base, expected) ->
      let linked = get (S.link ~base object_) in
      Alcotest.(check string)
        "literal independently calculated addresses" expected
        (hex (S.bytes linked)))
    [
      (0x1000L, "e907000000c300000010000000000000");
      (0x4000L, "e907000000c300000040000000000000");
    ]

let imports_and_phases () =
  let imported = symbol ~binding:S.Import 17 "Ext" S.Undefined in
  let object_ =
    get
      (create
         [ section 0 "image" "\xff\000\000\000\000" ]
         ~symbols:[ imported ]
         ~relocations:
           [
             set ~range:S.Modulo 0 0 1 (S.Value 255L);
             S.
               {
                 field = at 0 0;
                 width = 1;
                 intent = Add_bias;
                 phase = Load;
                 source = no_source;
               };
             set ~phase:S.Load ~range:S.Modulo 0 0 1 (S.Symbol 17);
             set ~phase:S.Load ~relative_to:(at 0 5) 0 1 4 (S.Symbol 17);
           ])
  in
  let compiled = get (S.link ~compile_only:true ~base:0L object_) in
  Alcotest.(check int)
    "load records retained" 3
    (List.length (S.pending compiled));
  let deferred = get (S.link ~base:1L ~defer_imports:true object_) in
  Alcotest.(check string)
    "bias applies while imports stay pending" "0000000000"
    (hex (S.bytes deferred));
  Alcotest.(check int)
    "only unresolved imports pending" 2
    (List.length (S.pending deferred));
  let linked = get (S.link ~base:1L ~imports:[ ("Ext", 10L) ] object_) in
  Alcotest.(check string)
    "import overwrites bias; loader uses field end" "0a04000000"
    (hex (S.bytes linked));
  fails "unresolved imports" (S.link ~base:1L object_);
  fails "duplicate import values"
    (S.link ~base:0L ~imports:[ ("unused", 1L); ("unused", 2L) ] object_);
  fails "compile-only rebase" (S.link ~compile_only:true ~base:1L object_)

let field_ranges () =
  List.iter
    (fun (width, min, max) ->
      let object_for value =
        get
          (create
             [ section 0 "code" (String.make width '\000') ]
             ~relocations:[ set 0 0 width (S.Value value) ])
      in
      ignore (get (S.link ~base:0L (object_for min)));
      ignore (get (S.link ~base:0L (object_for max)));
      if width <> 8 then (
        fails "signed low overflow"
          (S.link ~base:0L (object_for (Int64.pred min)));
        fails "signed high overflow"
          (S.link ~base:0L (object_for (Int64.succ max)))))
    [
      (1, -128L, 127L);
      (2, -32768L, 32767L);
      (4, -2147483648L, 2147483647L);
      (8, Int64.min_int, Int64.max_int);
    ];
  let object_for range value =
    get
      (create
         [ section 0 "code" "\000" ]
         ~relocations:[ set ~range 0 0 1 (S.Value value) ])
  in
  fails "unsigned narrowing" (S.link ~base:0L (object_for S.Unsigned 256L));
  fails "unsigned negative" (S.link ~base:0L (object_for S.Unsigned (-1L)));
  Alcotest.(check string)
    "explicit modulo narrowing" "23"
    (hex (S.bytes (get (S.link ~base:0L (object_for S.Modulo 0x123L)))));
  ignore (get (S.link ~base:0L (object_for S.Bit_pattern 255L)));
  let overflow =
    get
      (create
         [ section 0 "code" (String.make 8 '\000') ]
         ~relocations:
           [ set ~relative_to:(at 0 8) 0 0 8 (S.Value Int64.min_int) ])
  in
  fails "signed relative arithmetic overflow" (S.link ~base:1L overflow);
  let overflow =
    get
      (create
         [ section 0 "code" "\000" ]
         ~relocations:[ set ~addend:1L 0 0 1 (S.Value Int64.max_int) ])
  in
  fails "signed addend overflow" (S.link ~base:0L overflow)

let identity_and_bounds () =
  let body = [ section 0 "code" "\000\000" ] in
  let a = symbol 3 "same" (S.Defined (at 0 0)) in
  fails "duplicate identity"
    (create body ~symbols:[ a; { a with name = "other" } ]);
  fails "duplicate scoped name" (create body ~symbols:[ a; { a with id = 4 } ]);
  ignore
    (get
       (create body
          ~symbols:
            [ { a with scope = Some "A" }; { a with id = 4; scope = Some "B" } ]));
  fails "undefined local"
    (create body ~symbols:[ { a with definition = S.Undefined } ]);
  fails "defined import"
    (create body ~symbols:[ { a with binding = S.Import } ]);
  fails "unknown field section"
    (create body ~relocations:[ set 2 0 1 (S.Value 0L) ]);
  fails "field past section"
    (create body ~relocations:[ set 0 1 2 (S.Value 0L) ]);
  fails "unknown symbol" (create body ~relocations:[ set 0 0 1 (S.Symbol 4) ]);
  fails "out-of-section anchor"
    (create body ~relocations:[ set ~relative_to:(at 0 3) 0 0 1 (S.Value 0L) ]);
  fails "bad width" (create body ~relocations:[ set 0 0 3 (S.Value 0L) ]);
  fails "raw end overflow" (create ~position:Int64.max_int body);
  let object_ = get (create body) in
  fails "loaded end overflow" (S.link ~base:Int64.max_int object_);
  fails "bad alignment" (create [ section ~alignment_bits:63 0 "code" "" ]);
  fails "map past section"
    (create body
       ~mappings:[ S.{ address = at 0 1; size = 2; source = no_source } ])

let exact_limits_and_ownership () =
  let input = Bytes.of_string "\000\000" in
  let symbols = [ symbol 0 "x" (S.Defined (at 0 2)) ] in
  let relocations = [ set ~phase:S.Load 0 0 2 (S.Symbol 0) ] in
  let arguments () = [ section 0 "c" (Bytes.unsafe_to_string input) ] in
  let original = get (create (arguments ()) ~symbols ~relocations) in
  let work = S.work original in
  ignore
    (get
       (create ~max_work:work ~max_bytes:2 (arguments ()) ~symbols ~relocations));
  fails "one below object work"
    (create ~max_work:(work - 1) (arguments ()) ~symbols ~relocations);
  fails "one below byte count"
    (create ~max_bytes:1 (arguments ()) ~symbols ~relocations);
  let link_work = work + 1 + 2 in
  ignore (get (S.link ~max_work:link_work ~base:0L original));
  fails "one below link work"
    (S.link ~max_work:(link_work - 1) ~base:0L original);
  Bytes.fill input 0 2 '\xff';
  Alcotest.(check string)
    "input bytes owned" "0200"
    (hex (S.bytes (get (S.link ~base:0L original))));
  let view = (List.hd (S.sections original)).bytes |> Bytes.unsafe_of_string in
  Bytes.fill view 0 2 '\xff';
  Alcotest.(check string)
    "public section bytes copied" "0200"
    (hex (S.bytes (get (S.link ~base:0L original))));
  let linked = get (S.link ~base:0L original) in
  Bytes.fill (Bytes.unsafe_of_string (S.bytes linked)) 0 2 '\xff';
  Alcotest.(check string)
    "linked byte view copied" "0200"
    (hex (S.bytes linked));
  fails "late field failure" (S.link ~base:0x10000L original);
  Alcotest.(check string)
    "failure preserves original" "0200"
    (hex (S.bytes (get (S.link ~base:0L original))))

let assembly_bin_consumer () =
  let _, config, assembly =
    Test_assembly_layout.parse ~compilation_mode:Preprocessor.Aot
      "USE64\n\
       IMPORT Ext;\n\
       Entry:: JMP Done;\n\
       DU8 0;\n\
       Done: RET;\n\
       DU32 Entry;\n\
       MOV RAX,Ext;\n"
  in
  let image = Test_assembly_layout.emit config assembly in
  let object_ =
    match Assembly_encoding.native_sections image with
    | Ok x -> x
    | Error e -> Alcotest.fail e.message
  in
  List.iter
    (fun base ->
      let classic =
        match
          Assembly_encoding.link ~base ~symbols:[ ("Ext", 0x10203040L) ] image
        with
        | Ok x -> x.bytes
        | Error e -> Alcotest.fail e.message
      in
      let shared =
        get (S.link ~base ~imports:[ ("Ext", 0x10203040L) ] object_)
      in
      Alcotest.(check string)
        "assembler and object agree" classic (S.bytes shared))
    [ 0L; 0x1000L ];
  let module_ =
    match Assembly_bin.emit ~main:[ "Entry" ] image with
    | Ok x -> x
    | Error e -> Alcotest.fail e.message
  in
  let projected =
    match
      Templeos_bin.of_sections
        ~main:
          [
            (List.find
               (fun (s : S.symbol) -> s.name = "Entry")
               (S.symbols object_))
              .id;
          ]
        object_
    with
    | Ok x -> x
    | Error e -> Alcotest.fail e.message
  in
  Alcotest.(check string)
    "BIN consumer shares ordered object"
    (Templeos_bin.bytes module_)
    (Templeos_bin.bytes projected);
  Alcotest.(check bool)
    "source spans retained" true
    (List.exists
       (fun (m : S.mapping) -> m.source.span <> None)
       (S.mappings object_));
  ignore
    (match Bin_validator.read (Templeos_bin.bytes module_) with
    | Ok x -> x
    | Error e -> Alcotest.fail e.message)

let original_compiler_artifact () =
  let _, config, assembly =
    Test_assembly_layout.parse ~compilation_mode:Preprocessor.Aot
      "USE64\n\
       IMPORT NVImport;\n\
       NVEntry:: MOV RAX,NVImport; RET;\n\
       NVDatum:: DU32 NVEntry;\n"
  in
  let image = Test_assembly_layout.emit config assembly in
  let object_ =
    match Assembly_encoding.native_sections image with
    | Ok x -> x
    | Error e -> Alcotest.fail e.message
  in
  let path =
    if Sys.file_exists "oracle/bin-validator/ASM.BIN" then
      "oracle/bin-validator/ASM.BIN"
    else "test/oracle/bin-validator/ASM.BIN"
  in
  let channel = open_in_bin path in
  let literal =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  let native =
    match Templeos_bin.read literal with
    | Ok x -> x
    | Error e -> Alcotest.fail e.message
  in
  Alcotest.(check string)
    "original Cmp-produced body" (Templeos_bin.body native) image.bytes;
  List.iter
    (fun (base, expected) ->
      let linked =
        get (S.link ~base ~imports:[ ("NVImport", 0x12345678L) ] object_)
      in
      Alcotest.(check string)
        "literal original body and independently computed patches" expected
        (hex (S.bytes linked)))
    [ (0x1000L, "b878563412c300100000"); (0x4000L, "b878563412c300400000") ]

let native_source_consumer () =
  List.iter
    (fun mode ->
      let source =
        "I64 G=5; I64 F(I64 x) {if (x) return F(x-1)+G; return 1;} F(3);"
      in
      let image = Test_native_scalar_functions.image ~mode source in
      let again = Test_native_scalar_functions.image ~mode source in
      let object_ = X86_64_program.native_sections image in
      Alcotest.(check string)
        "deterministic source object"
        (Yojson.Safe.to_string (S.to_yojson object_))
        (Yojson.Safe.to_string
           (S.to_yojson (X86_64_program.native_sections again)));
      let symbols = S.symbols object_ in
      Alcotest.(check bool)
        "function/data byte ranges retain source spans" true
        (List.length
           (List.filter
              (fun (m : S.mapping) -> m.source.span <> None && m.size > 0)
              (S.mappings object_))
        >= 2);
      Alcotest.(check bool)
        "function source identity" true
        (List.exists
           (fun (s : S.symbol) ->
             s.name = "F" && s.source_identity <> None && s.source.span <> None)
           symbols);
      Alcotest.(check bool)
        "data source identity" true
        (List.exists
           (fun (s : S.symbol) -> s.name = "G" && s.source_identity <> None)
           symbols);
      Alcotest.(check bool)
        "real branch and call relocations" true
        (S.relocations object_ <> []);
      List.iter
        (fun base ->
          let linked = get (S.link ~base object_) in
          let code_size = X86_64_program.code_bytes image in
          Alcotest.(check string)
            "source code rebases independently"
            (X86_64_program.code image)
            (String.sub (S.bytes linked) 0 code_size))
        [ 0L; 0x100000L; 0x400000L ];
      fails "hosted object cannot become BIN" (Templeos_bin.of_sections object_))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let legacy_quota_is_preserved () =
  let name = String.make 99_999_999 'x' in
  let exact =
    Asm_relocation.apply ~max_work:100_000_000 ~position:0L ~base:0L
      ~symbols:[ (name, 0L) ]
      ~bytes:"" []
  in
  (match exact with
  | Ok bytes -> Alcotest.(check string) "legacy maximum exact cost" "" bytes
  | Error e -> Alcotest.fail e.message);
  fails "legacy maximum one below"
    (Asm_relocation.apply ~max_work:99_999_999 ~position:0L ~base:0L
       ~symbols:[ (name, 0L) ]
       ~bytes:"" [])

let tests =
  [
    Alcotest.test_case "legacy public maximum quota remains exact" `Quick
      legacy_quota_is_preserved;
    Alcotest.test_case "ordered aligned sections and literal forward rebases"
      `Quick forward_sections;
    Alcotest.test_case
      "compile/load phases, overlapping patches and deferred imports" `Quick
      imports_and_phases;
    Alcotest.test_case "target widths and checked relative arithmetic" `Quick
      field_ranges;
    Alcotest.test_case "sparse identities, duplicates and ranges" `Quick
      identity_and_bounds;
    Alcotest.test_case "exact limits, one below and independent owned views"
      `Quick exact_limits_and_ownership;
    Alcotest.test_case "source assembly, shared linking and BIN output" `Quick
      assembly_bin_consumer;
    Alcotest.test_case "original Cmp artifact and literal rebased patch fields"
      `Quick original_compiler_artifact;
    Alcotest.test_case
      "source functions, data, forward calls and independent rebases" `Quick
      native_source_consumer;
  ]
