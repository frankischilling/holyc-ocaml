module Relocation = Holyc_lib.Asm_relocation

let relocation offset width kind =
  match Relocation.make ~offset ~width ~kind with
  | Ok relocation -> relocation
  | Error message -> Alcotest.fail message

let hex bytes =
  let digits = "0123456789abcdef" in
  String.init
    (2 * String.length bytes)
    (fun index ->
      let byte = Char.code bytes.[index / 2] in
      digits.[if index mod 2 = 0 then byte lsr 4 else byte land 15])

let apply ?max_work ?(position = 0L) ?(base = 0L) ?(symbols = []) bytes records
    =
  Relocation.apply ?max_work ~position ~base ~symbols ~bytes records

let linked ?max_work ?position ?base ?symbols bytes records =
  match apply ?max_work ?position ?base ?symbols bytes records with
  | Ok bytes -> bytes
  | Error error -> Alcotest.fail error.message

let rejected ?max_work ?position ?base ?symbols ?index label bytes records =
  match apply ?max_work ?position ?base ?symbols bytes records with
  | Ok _ -> Alcotest.failf "%s unexpectedly linked" label
  | Error error ->
      Alcotest.(check (option int))
        (label ^ " relocation index")
        index error.relocation_index;
      Alcotest.(check bool)
        (label ^ " has a diagnostic")
        true
        (String.length error.message > 0)

let constructors () =
  List.iter
    (fun (offset, width, kind) ->
      match Relocation.make ~offset ~width ~kind with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "invalid relocation was constructed")
    [
      (-1, 1, Relocation.Add_base);
      (0, 0, Relocation.Add_base);
      (0, 3, Relocation.Add_base);
      (0, 16, Relocation.Subtract_base);
      (0, 4, Relocation.Relative_import "");
      (0, 8, Relocation.Absolute_import "");
    ]

(* CMain.HC:183-197 applies AAT arithmetic to the selected storage width.
   These fixed words cover every width and its carry or borrow. *)
let modular_base_fields () =
  let records kind =
    List.map
      (fun (offset, width) -> relocation offset width kind)
      [ (0, 1); (1, 2); (3, 4); (7, 8) ]
  in
  let initial = String.make 15 '\255' in
  let result = linked ~base:1L initial (records Relocation.Add_base) in
  Alcotest.(check string)
    "all widths carry modulo their field" (String.make 15 '\000') result;
  Alcotest.(check string)
    "adding base preserves the source image" (String.make 15 '\255') initial;
  Alcotest.(check string)
    "all widths borrow modulo their field" (String.make 15 '\255')
    (linked ~base:1L (String.make 15 '\000') (records Relocation.Subtract_base));
  Alcotest.(check string)
    "base is a bias and excludes the raw position" "1010"
    (hex
       (linked ~position:0x4000L ~base:0x1000L "\x10\x00"
          [ relocation 0 2 Relocation.Add_base ]));
  Alcotest.(check string)
    "qword addition wraps through the signed boundary" "0000000000000080"
    (hex
       (linked ~base:1L "\xff\xff\xff\xff\xff\xff\xff\x7f"
          [ relocation 0 8 Relocation.Add_base ]))

let overlapping_order () =
  let byte = relocation 0 1 Relocation.Add_base in
  let word = relocation 0 2 Relocation.Add_base in
  Alcotest.(check string)
    "byte carry is discarded before word addition" "0100"
    (hex (linked ~base:1L "\xff\x00" [ byte; word ]));
  Alcotest.(check string)
    "word carry precedes byte addition" "0101"
    (hex (linked ~base:1L "\xff\x00" [ word; byte ]));
  let records =
    [
      relocation 0 2 (Relocation.Absolute_import "Word");
      relocation 0 1 Relocation.Add_base;
      relocation 0 1 (Relocation.Absolute_import "Byte");
    ]
  in
  Alcotest.(check string)
    "base records precede imports, whose supplied order is retained" "5612cc"
    (hex
       (linked ~base:5L
          ~symbols:[ ("Word", 0x1234L); ("Byte", 0x56L) ]
          "\xff\xbb\xcc" records))

(* AsmResolve.HC:5-18 stores an import expression with the symbol set to zero.
   CMain.HC:251-285 and KLoad.HC:32-48 later overwrite its addend. *)
let import_overwrites () =
  let raw = "\xe8\x07\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00" in
  let records =
    [
      relocation 1 4 (Relocation.Relative_import "F");
      relocation 5 2 (Relocation.Absolute_import "F");
      relocation 7 8 (Relocation.Absolute_import "F");
    ]
  in
  Alcotest.(check string)
    "CALL F+7, DU16 F+3 and DU64 F after linking"
    "e8fb00000000110011000000000000"
    (hex (linked ~base:0x1000L ~symbols:[ ("F", 0x1100L) ] raw records));
  Alcotest.(check string)
    "import linking preserves the provisional source"
    "e80700000003000000000000000000" (hex raw);
  Alcotest.(check string)
    "relative imports use the loaded field end" "c705fa00000009000000"
    (hex
       (linked ~base:0x1000L
          ~symbols:[ ("F", 0x1100L) ]
          "\xc7\x05\x07\x00\x00\x00\x09\x00\x00\x00"
          [ relocation 2 4 (Relocation.Relative_import "F") ]));
  Alcotest.(check string)
    "absolute imports retain low bits at every width" (String.make 15 '\255')
    (linked
       ~symbols:[ ("Negative", -1L) ]
       (String.make 15 '\000')
       (List.map
          (fun (offset, width) ->
            relocation offset width (Relocation.Absolute_import "Negative"))
          [ (0, 1); (1, 2); (3, 4); (7, 8) ]));
  Alcotest.(check string)
    "absolute imports narrow large positive values"
    "888877887766558877665544332211"
    (hex
       (linked
          ~symbols:[ ("Wide", 0x1122334455667788L) ]
          (String.make 15 '\000')
          (List.map
             (fun (offset, width) ->
               relocation offset width (Relocation.Absolute_import "Wide"))
             [ (0, 1); (1, 2); (3, 4); (7, 8) ])))

let relative_ranges () =
  List.iter
    (fun (width, displacement, expected) ->
      let position = 32L and base = 0x100000000L in
      let field_end = Int64.add 0x100000020L (Int64.of_int (width + 1)) in
      let target = Int64.add field_end displacement in
      let raw = String.make (width + 2) '\xa5' in
      Alcotest.(check string)
        "relative field retains the surrounding bytes"
        ("a5" ^ expected ^ "a5")
        (hex
           (linked ~position ~base
              ~symbols:[ ("Target", target) ]
              raw
              [ relocation 1 width (Relocation.Relative_import "Target") ])))
    [
      (1, -128L, "80");
      (1, 127L, "7f");
      (2, -32768L, "0080");
      (2, 32767L, "ff7f");
      (4, -2147483648L, "00000080");
      (4, 2147483647L, "ffffff7f");
    ];
  List.iter
    (fun (width, displacement) ->
      let target = Int64.add (Int64.of_int width) displacement in
      rejected
        ~symbols:[ ("Target", target) ]
        ~index:0 "relative field outside its signed range"
        (String.make width '\000')
        [ relocation 0 width (Relocation.Relative_import "Target") ])
    [
      (1, -129L);
      (1, 128L);
      (2, -32769L);
      (2, 32768L);
      (4, -2147483649L);
      (4, 2147483648L);
    ];
  let qword = [ relocation 0 8 (Relocation.Relative_import "Target") ] in
  Alcotest.(check string)
    "relative qword accepts the signed minimum" "0000000000000080"
    (hex
       (linked
          ~symbols:[ ("Target", Int64.add Int64.min_int 8L) ]
          (String.make 8 '\000') qword));
  Alcotest.(check string)
    "relative qword accepts the largest target" "f7ffffffffffff7f"
    (hex
       (linked
          ~symbols:[ ("Target", Int64.max_int) ]
          (String.make 8 '\000') qword));
  rejected
    ~symbols:[ ("Target", Int64.min_int) ]
    ~index:0 "relative qword arithmetic underflow" (String.make 8 '\000') qword

let coordinate_and_field_limits () =
  rejected ~position:(-1L) "negative raw position" "" [];
  rejected ~base:(-1L) "negative load bias" "" [];
  rejected ~position:Int64.max_int "raw image end overflow" "\x00" [];
  rejected ~position:Int64.max_int ~base:1L "loaded position overflow" "" [];
  rejected ~position:(Int64.pred Int64.max_int) ~base:1L
    "loaded image end overflow" "\x00" [];
  Alcotest.(check string)
    "maximum loaded end is accepted" "\x00"
    (linked ~position:(Int64.pred Int64.max_int) "\x00" []);
  Alcotest.(check string)
    "empty image can begin at the maximum coordinate" ""
    (linked ~position:Int64.max_int "" []);
  List.iter
    (fun (raw, offset, width) ->
      rejected ~index:0 "relocation bounds" raw
        [ relocation offset width Relocation.Add_base ])
    [ ("", 0, 1); ("\x00", 1, 1); ("\x00", 0, 2); ("\x00", max_int, 8) ];
  Alcotest.(check string)
    "field can end exactly at image end" "003412"
    (hex
       (linked
          ~symbols:[ ("Target", 0x1234L) ]
          "\x00\x00\x00"
          [ relocation 1 2 (Relocation.Absolute_import "Target") ]))

let symbol_validation () =
  rejected ~symbols:[ ("", 42L) ] "empty unused symbol" "" [];
  rejected
    ~symbols:[ ("Same", 42L); ("Same", 42L) ]
    "duplicate unused symbol" "" [];
  let raw = "\x12\x34" in
  rejected ~base:1L ~index:1 "missing import after a valid base record" raw
    [
      relocation 0 1 Relocation.Add_base;
      relocation 1 1 (Relocation.Absolute_import "Missing");
    ];
  Alcotest.(check string) "failed linking preserves its source" "1234" (hex raw);
  Alcotest.(check string)
    "symbol names retain source case" "1234"
    (hex
       (linked
          ~symbols:[ ("Name", 0x12L); ("name", 0x34L) ]
          "\x00\x00"
          [
            relocation 0 1 (Relocation.Absolute_import "Name");
            relocation 1 1 (Relocation.Absolute_import "name");
          ]))

let work_limits () =
  let raw = "\x00\x00\x00\x00" in
  let records =
    [
      relocation 0 1 Relocation.Add_base;
      relocation 1 1 (Relocation.Absolute_import "Long");
    ]
  in
  let symbols = [ ("Long", 42L); ("z", 0L) ] in
  (* Four bytes, two records, two names costing five and two, and four
     characters for the import lookup consume exactly seventeen work units. *)
  Alcotest.(check string)
    "exact work allowance succeeds" "012a0000"
    (hex (linked ~max_work:17 ~base:1L ~symbols raw records));
  rejected ~max_work:16 ~base:1L ~symbols ~index:1
    "one below the complete work requirement" raw records;
  let repeated =
    [
      relocation 0 1 (Relocation.Absolute_import "F");
      relocation 1 1 (Relocation.Absolute_import "F");
    ]
  in
  Alcotest.(check string)
    "repeated import lookups contribute their name lengths" "0707"
    (hex (linked ~max_work:8 ~symbols:[ ("F", 7L) ] "\x00\x00" repeated));
  rejected ~max_work:7
    ~symbols:[ ("F", 7L) ]
    ~index:1 "one below repeated import work" "\x00\x00" repeated;
  Alcotest.(check string)
    "image bytes are charged once" raw
    (linked ~max_work:4 raw []);
  rejected ~max_work:3 "image byte allowance" raw [];
  rejected ~max_work:0 "zero work limit" "" [];
  rejected ~max_work:100_000_001 "work limit maximum" "" [];
  Alcotest.(check string)
    "empty input has zero work" "" (linked ~max_work:1 "" [])

let tests =
  [
    Alcotest.test_case "relocation construction" `Quick constructors;
    Alcotest.test_case "all AAT storage widths" `Quick modular_base_fields;
    Alcotest.test_case "phase and overlapping record order" `Quick
      overlapping_order;
    Alcotest.test_case "source import overwrite semantics" `Quick
      import_overwrites;
    Alcotest.test_case "relative widths and arithmetic" `Quick relative_ranges;
    Alcotest.test_case "coordinates and field ownership" `Quick
      coordinate_and_field_limits;
    Alcotest.test_case "symbol validation and immutable input" `Quick
      symbol_validation;
    Alcotest.test_case "exact relocation work limits" `Quick work_limits;
  ]
