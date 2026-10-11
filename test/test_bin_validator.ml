open Holyc_lib
module V = Bin_validator

let get = function
  | Ok x -> x
  | Error (e : V.error) ->
      Alcotest.fail (Printf.sprintf "%s: %s" e.artifact e.message)

let error label = function
  | Ok _ -> Alcotest.fail (label ^ " succeeded")
  | Error e -> e

let i64 label expected actual = Alcotest.(check int64) label expected actual
let int label expected actual = Alcotest.(check int) label expected actual
let str label expected actual = Alcotest.(check string) label expected actual

let put bytes at width value =
  for i = 0 to width - 1 do
    Bytes.set bytes (at + i)
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical value (8 * i)) 255L)))
  done

let word width value =
  let b = Bytes.make width '\000' in
  put b 0 width value;
  Bytes.to_string b

let record typ value name payload =
  String.make 1 (Char.chr typ) ^ word 4 value ^ name ^ "\000" ^ payload

let import ?(name = "S") typ target = record typ (Int64.of_int target) name ""
let export ?(typ = 17) name value = record typ value name ""
let main n = record 25 (Int64.of_int n) "" ""

let abs offsets =
  record 20
    (Int64.of_int (List.length offsets))
    ""
    (String.concat "" (List.map (fun n -> word 4 (Int64.of_int n)) offsets))

let heap ?(name = "H") ?(typ = 23) size offsets =
  record typ
    (Int64.of_int (List.length offsets))
    name
    (word (if typ <= 22 then 4 else 8) size
    ^ String.concat "" (List.map (fun n -> word 4 (Int64.of_int n)) offsets))

(* Literal wire builder deliberately does not call Templeos_bin.create/read. *)
let raw ?(bits = 0) ?(org = Int64.max_int) ?(body = String.make 128 '\000')
    records =
  let patch = 32 + String.length body in
  let table = String.concat "" records ^ "\000" in
  let size = (patch + String.length table + 15) land lnot 15 in
  let header = Bytes.make 32 '\000' in
  Bytes.set header 0 '\235';
  Bytes.set header 1 '\030';
  Bytes.set header 2 (Char.chr bits);
  put header 4 4 0x42534f54L;
  put header 8 8 org;
  put header 16 8 (Int64.of_int patch);
  put header 24 8 (Int64.of_int size);
  Bytes.to_string header ^ body ^ table
  ^ String.make (size - patch - String.length table) '\000'

let artifact ?name records = V.read ?name (raw records) |> get
let image w id = List.find (fun (i : V.image) -> i.id = id) (V.images w)

let field body offset width =
  let v = ref 0L in
  for i = 0 to width - 1 do
    v :=
      Int64.logor !v
        (Int64.shift_left (Int64.of_int (Char.code body.[offset + i])) (8 * i))
  done;
  !v

let replace bytes at width value =
  let b = Bytes.of_string bytes in
  put b at width value;
  Bytes.to_string b

let independent_reader () =
  let imports = List.init 10 (fun i -> import (i + 2) (i * 8)) in
  let records =
    [
      export ~typ:16 "R32" 1L;
      export "I32" 0xffffffffL;
      export ~typ:18 "R64" 128L;
      export ~typ:19 "I64" 0xffffffffL;
      abs [ 0; 4; 0 ];
      heap ~typ:21 0L [ 8; 8 ];
      heap ~typ:22 4L [ 12 ];
      heap ~typ:23 16L [ 16 ];
      heap ~typ:24 8L [ 24 ];
      main 127;
    ]
    @ imports
  in
  let a = artifact records in
  int "all twenty nonterminal types" 20 (List.length (V.records a));
  int "absolute payload offsets" 160 (List.hd (V.records a)).offset;
  str "preserved full envelope" (raw records) (V.bytes a);
  let narrow =
    V.read ~limits:{ V.default_limits with work = V.work a } (V.bytes a) |> get
  in
  int "exact reader work" (V.work a) (V.work narrow);
  ignore
    (error "one below reader work"
       (V.read
          ~limits:{ V.default_limits with work = V.work a - 1 }
          (V.bytes a)));
  ignore
    (error "one below reader bytes"
       (V.read
          ~limits:
            { V.default_limits with file_bytes = String.length (V.bytes a) - 1 }
          (V.bytes a)));
  ignore
    (get
       (V.read
          ~limits:
            { V.default_limits with file_bytes = String.length (V.bytes a) }
          (V.bytes a)));
  str "8-bit name preserved" "\255"
    (List.hd (V.records (artifact [ export "\255" 1L ]))).name

let malformed () =
  let valid = raw [] in
  for n = 0 to 31 do
    let e = error "truncated header" (V.read (String.sub valid 0 n)) in
    Alcotest.(check (option int)) "precise header offset" (Some n) e.offset
  done;
  let bad label expected bytes =
    let e = error label (V.read ~name:"bad.bin" bytes) in
    str "artifact name" "bad.bin" e.artifact;
    Alcotest.(check (option int)) label (Some expected) e.offset
  in
  bad "jump" 0 (replace valid 0 1 0L);
  bad "alignment" 2 (replace valid 2 1 63L);
  bad "reserved" 3 (replace valid 3 1 1L);
  bad "signature" 4 (replace valid 4 4 0L);
  bad "negative org" 8 (replace valid 8 8 (-1L));
  bad "patch header overlap" 16 (replace valid 16 8 31L);
  bad "patch huge" 16 (replace valid 16 8 Int64.max_int);
  bad "file size" 24 (replace valid 24 8 0L);
  List.iter
    (fun typ -> bad "reserved type" 160 (raw [ record typ 0L "" "" ]))
    [ 1; 12; 13; 14; 15; 26; 255 ];
  bad "unbound continuation" 160 (raw [ import ~name:"" 9 0 ]);
  bad "consumed ABS" 167 (raw [ import 9 0; abs [] ]);
  bad "huge count" 161 (raw [ record 20 0xffffffffL "" "" ]);
  bad "field upper boundary" 161 (raw [ import 11 121 ]);
  ignore (get (V.read (raw [ import 11 120 ])));
  ignore (get (V.read (raw [ import 3 128 ])));
  bad "zero width beyond end" 161 (raw [ import 3 129 ]);
  bad "empty export" 165 (raw [ export "" 1L ]);
  bad "relative export" 161 (raw [ export ~typ:16 "S" 129L ]);
  bad "MAIN at end" 161 (raw [ main 128 ]);
  bad "negative code heap" 167 (raw [ heap ~typ:21 (-1L) [] ]);
  bad "negative data heap" 167 (raw [ heap (-1L) [] ]);
  bad "ABS field" 166 (raw [ abs [ 125 ] ]);
  bad "heap field" 175 (raw [ heap 8L [ 121 ] ]);
  bad "nonzero trailing pad" 161 (replace valid 161 1 1L);
  let extended = valid ^ String.make 16 '\000' in
  bad "excess zero padding" 161
    (replace extended 24 8 (Int64.of_int (String.length extended)));
  let no_nul = raw [ export "S" 1L ] in
  let b = Bytes.of_string no_nul in
  for i = 165 to Bytes.length b - 1 do
    Bytes.set b i 'X'
  done;
  bad "unterminated name" 165 (Bytes.to_string b)

let import_widths () =
  List.iter
    (fun base ->
      let w = V.bind (V.empty ()) ~name:"S" ~value:0x123456789abcdef0L |> get in
      let types =
        List.init 10 (fun i ->
            import ~name:(if i = 0 then "S" else "") (i + 2) (i * 8))
      in
      let a = artifact types in
      let w = V.load ~base w a |> get in
      int "all imports including zero-width" 10 (List.length (V.relocations w));
      List.iteri
        (fun i (r : V.relocation) ->
          let width = if i < 2 then 0 else 1 lsl ((i - 2) / 2) in
          let expected =
            if width = 0 then 0L
            else if i land 1 = 0 then
              Int64.sub 0x123456789abcdef0L
                (Int64.add base (Int64.of_int ((i * 8) + width)))
            else 0x123456789abcdef0L
          in
          let expected =
            if width = 8 then expected
            else if width = 0 then 0L
            else
              Int64.logand expected
                (Int64.pred (Int64.shift_left 1L (width * 8)))
          in
          i64 "modular import stores" expected r.after;
          int "original record offset" (List.nth (V.records a) i).offset
            r.record_offset;
          int "artifact field offset" (32 + (i * 8)) r.field_offset)
        (V.relocations w);
      int "imports resolved" 0 (List.length (V.unresolved w)))
    [ 0x10000000L; 0x100000000L ]

let deferred () =
  let consumer =
    artifact ~name:"consumer"
      [ import 9 0; import ~name:"" 11 8; import ~name:"HEAP" 9 20 ]
  in
  let old = V.load ~base:0x10000000L (V.empty ()) consumer |> get in
  int "two pending named groups" 2 (List.length (V.unresolved old));
  let newest = List.hd (V.unresolved old) in
  int "precise unresolved offset" (List.nth (V.records consumer) 2).offset
    newest.record_offset;
  let provider =
    artifact [ heap ~name:"HEAP" 16L []; export "S" 0x11223344L ]
  in
  let w = V.load ~base:0x10001000L old provider |> get in
  i64 "deferred U32" 0x11223344L (field (image w 0).body 0 4);
  i64 "deferred I64" 0x11223344L (field (image w 0).body 8 8);
  int "heap publication leaves earlier group pending" 1
    (List.length (V.unresolved w));
  i64 "heap pending bytes retained" 0L (field (image w 0).body 20 4);
  let w = V.load ~base:0x10002000L w (artifact [ export "HEAP" 85L ]) |> get in
  int "ordinary export replays heap import" 0 (List.length (V.unresolved w));
  i64 "heap deferred import final" 85L (field (image w 0).body 20 4);
  i64 "older world retained" 0L (field (image old 0).body 0 4);
  i64 "artifact retained" 0L (field (V.body consumer) 0 4);
  let rebound = V.bind old ~name:"S" ~value:(-1L) |> get in
  i64 "external binding supports all I64 bits" (-1L)
    (field (image rebound 0).body 8 8)

let passes_and_heaps () =
  let records =
    [
      main 5;
      abs [ 0; 0 ];
      heap ~typ:21 ~name:"C" 16L [ 4; 4 ];
      heap ~typ:22 ~name:"ZC" 0L [];
      heap ~name:"D" 16L [ 12; 12 ];
      heap ~typ:24 ~name:"ZD" 8L [ 24 ];
      export "X" 1L;
      export "X" 2L;
      main 6;
      main 5;
    ]
  in
  let a = artifact records in
  let w = V.load ~base:0x10000000L (V.empty ()) a |> get in
  let bytes = (image w 0).body in
  i64 "duplicate ABS adds twice" 0x20000000L (field bytes 0 4);
  i64 "duplicate code heap adds twice" 0x40000000L (field bytes 4 4);
  i64 "duplicate data heap adds twice" 0x60000000L (field bytes 12 8);
  i64 "zeroed data pointer" 0x30000010L (field bytes 24 8);
  i64 "newest export wins" 2L (List.assoc "X" (V.symbols w));
  Alcotest.(check (list int64))
    "pass two MAIN order"
    [ 0x10000005L; 0x10000006L; 0x10000005L ]
    (List.map (fun (e : V.entry) -> e.address) (V.entries w));
  Alcotest.(check (list bool))
    "zeroed/unknown allocation contents"
    [ false; true; false; true ]
    (List.map (fun (h : V.heap) -> h.zeroed) (V.heaps w));
  let flags : V.flags = { no_abss = true; just_load = true; silent = true } in
  let silent = V.load ~flags (V.empty ()) (artifact [ import 9 0 ]) |> get in
  int "silent does not drop deferred imports" 1
    (List.length (V.unresolved silent));
  let w = V.load ~flags ~base:0x10000000L (V.empty ()) a |> get in
  i64 "NO_ABSS" 0L (field (image w 0).body 0 4);
  int "JUST_LOAD" 0 (List.length (V.entries w))

let budgets_and_atomicity () =
  let a = artifact [ abs [ 0 ]; heap 16L [ 8 ]; main 1 ] in
  let w = V.load ~base:0x10000000L (V.empty ()) a |> get in
  let exact =
    {
      V.default_limits with
      work = V.transaction_work w;
      image_bytes = 128;
      heap_bytes = 16L;
      modules = 1;
    }
  in
  ignore (get (V.load ~limits:exact ~base:0x10000000L (V.empty ()) a));
  List.iter
    (fun limits ->
      ignore
        (error "one below model limit"
           (V.load ~limits ~base:0x10000000L (V.empty ()) a)))
    [
      { exact with work = exact.work - 1 };
      { exact with image_bytes = 127 };
      { exact with heap_bytes = 15L };
    ];
  ignore (error "module count" (V.load ~limits:exact ~base:0x10001000L w a));
  let old = V.load (V.empty ()) (artifact [ import 9 0 ]) |> get in
  let before = V.map old in
  let bad = artifact [ export "S" 3L; heap 16L [] ] in
  ignore
    (error "allocation checked before deferred mutation"
       (V.load ~limits:{ V.default_limits with heap_bytes = 15L } old bad));
  str "failed transaction retains world map" before (V.map old);
  i64 "failed transaction retains pending bytes" 0L
    (field (image old 0).body 0 4);
  let good = V.load old bad |> get in
  let total =
    Yojson.Safe.Util.(V.report good |> member "total_work" |> to_int)
  in
  ignore
    (error "replay work fails atomically"
       (V.load ~limits:{ V.default_limits with work = total - 1 } old bad));
  str "work failure retains old world" before (V.map old);
  ignore (error "address overflow" (V.load ~base:Int64.max_int (V.empty ()) a));
  ignore (error "overlap" (V.load ~base:(image w 0).base w a));
  ignore (error "heap/module overlap" (V.load ~base:0x30000020L (V.empty ()) a));
  ignore
    (error "code/data heap overlap"
       (V.load
          (V.empty ~code_base:0x20000000L ~data_base:0x20000000L ())
          (artifact [ heap ~typ:21 1L []; heap 1L [] ])))

let origin_and_ownership () =
  let bytes = raw ~bits:12 [] in
  let a = V.read bytes |> get in
  Bytes.set (Bytes.unsafe_of_string bytes) 0 '\000';
  int "reader owns input" 235 (Char.code (V.bytes a).[0]);
  let w = V.load (V.empty ~module_base:0x10000001L ()) a |> get in
  i64 "body alignment" 0L (Int64.logand (image w 0).base 4095L);
  let fixed = V.read (raw ~org:0x14000000L [ export "S" 7L ]) |> get in
  i64 "org is header address" 0x14000020L
    (image (V.load (V.empty ()) fixed |> get) 0).base;
  i64 "explicit body base overrides org" 0x10000000L
    (image (V.load ~base:0x10000000L (V.empty ()) fixed |> get) 0).base;
  let w = V.load (V.empty ()) fixed |> get in
  let projection = image w 0 in
  Bytes.set (Bytes.unsafe_of_string projection.body) 0 'X';
  let names = V.symbols w in
  Bytes.set (Bytes.unsafe_of_string (fst (List.hd names))) 0 'X';
  let records = V.records fixed in
  Bytes.set (Bytes.unsafe_of_string (List.hd records).name) 0 'X';
  let json = V.to_yojson fixed in
  let name =
    Yojson.Safe.Util.(
      json |> member "records" |> to_list |> List.hd |> member "name"
      |> to_string)
  in
  Bytes.set (Bytes.unsafe_of_string name) 0 'X';
  str "artifact records own names" "S" (List.hd (V.records fixed)).name;
  i64 "symbol projection owns names" 7L (List.assoc "S" (V.symbols w));
  int "image projection owns bytes" 0 (Char.code (image w 0).body.[0])

let rec replay_order_and_overlap () =
  ignore
    (error "binding name quota before ownership copy"
       (V.bind
          ~limits:{ V.default_limits with work = 4 }
          (V.empty ()) ~name:"LONG" ~value:0L));
  ignore
    (error "artifact label quota"
       (V.read ~name:(String.make 65537 'X') (raw [])));
  let a = artifact [ import 9 0; import 8 0 ] in
  let w = V.load ~base:0x10000000L (V.empty ()) a |> get in
  let w = V.bind w ~name:"S" ~value:7L |> get in
  i64 "newest pending group replays first" 7L (field (image w 0).body 0 4);
  Alcotest.(check (list int))
    "pending replay record order" [ 167; 160 ]
    (List.map (fun (r : V.relocation) -> r.record_offset) (V.relocations w));
  let w =
    V.load (V.empty ())
      (artifact [ import 9 0; export "S" 1L; export "S" 2L; import 9 8 ])
    |> get
  in
  i64 "first export resolves retained group" 1L (field (image w 0).body 0 4);
  i64 "later import uses newest export" 2L (field (image w 0).body 8 4);
  let w =
    V.load ~base:0x10000000L (V.empty ()) (artifact [ abs [ 0; 1; 0 ] ]) |> get
  in
  i64 "overlapping fields see earlier additions" 0x20000000L
    (field (image w 0).body 0 4);
  i64 "overlap retains upper byte" 0x10200000L (field (image w 0).body 1 4);
  let w =
    V.load (V.empty ()) (artifact [ heap 16L []; import ~name:"H" 9 0 ]) |> get
  in
  i64 "later import sees published heap" 0x30000000L
    (field (image w 0).body 0 4);
  let a = artifact [ export "\255" 1L ] in
  let j = V.to_yojson a |> Yojson.Safe.to_string in
  require_ascii j;
  let j =
    V.load (V.empty ()) (artifact [ import ~name:"\255" 9 0 ])
    |> get |> V.report |> Yojson.Safe.to_string
  in
  require_ascii j

and require_ascii text =
  String.iter
    (fun c ->
      if Char.code c >= 128 then Alcotest.fail "8-bit name escaped in JSON")
    text

let tests =
  [
    Alcotest.test_case "independent complete reader" `Quick independent_reader;
    Alcotest.test_case "malformed artifacts and precise offsets" `Quick
      malformed;
    Alcotest.test_case "every import width at two bases" `Quick import_widths;
    Alcotest.test_case "deferred groups and heap publication" `Quick deferred;
    Alcotest.test_case "two passes duplicate patches heaps and flags" `Quick
      passes_and_heaps;
    Alcotest.test_case "exact limits and transaction atomicity" `Quick
      budgets_and_atomicity;
    Alcotest.test_case "placement and ownership" `Quick origin_and_ownership;
    Alcotest.test_case "pending order overlapping fields and binary names"
      `Quick replay_order_and_overlap;
  ]
