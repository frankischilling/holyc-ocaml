module H = Holyc_lib__Common.Native_hash_record
module T = H.Table

let check = Alcotest.(check bool)

let count name expected record =
  Alcotest.(check int64) name expected (H.use_count record)

let hash_and_instruction_oracle () =
  let vectors =
    [
      ("", 0L);
      ("A", 65L);
      ("C", 67L);
      ("abc\000ignored", 683L);
      ("Hello", 0xa9fL);
      (String.make 16 '\255', 0xff0000L);
      (String.make 80 '\255', 0xfe7fffL);
      ( String.concat ""
          (List.init 3 (fun _ -> String.init 255 (fun i -> Char.chr (i + 1)))),
        0xff00fefffffffe3fL );
    ]
  in
  List.iter
    (fun (text, expected) ->
      Alcotest.(check int64) "byte hash" expected (H.hash_string text))
    vectors;
  (* Unlike a source-level hash formula, this executes the original SHL/ADC,
     SHR/ADC and table-selection instructions over 513 byte inputs and 6,912
     independent bucket/chain/count comparisons. *)
  match Holyc_lib.Native_execution.platform () with
  | Holyc_lib.Native_execution.Unsupported -> ()
  | _ ->
      check "independent hash and table instruction comparison" true
        (T.verify_storage ())

let bucket_priority_and_types () =
  let table = T.create ~size:2 in
  let first = H.create_function ~name:"A" in
  let collision = H.create_function ~name:"C" in
  let class_ = H.create_prefix ~name:"A" ~type_bits:0x10l in
  let flagged = H.create_prefix ~name:"A" ~type_bits:0x80000040l in
  List.iter (T.add table) [ first; collision; class_; flagged ];
  check "newest same-name function" true
    (T.find table ~expected:flagged ~name:"A" ~mask:0x40l);
  check "different physical allocation rejects without counting" false
    (T.find table ~expected:first ~name:"A" ~mask:0x40l);
  check "class mask skips a newer function" true
    (T.find table ~expected:class_ ~name:"A" ~mask:0x10l);
  check "flags are searchable low U32 bits" true
    (T.find table ~expected:flagged ~name:"A" ~mask:Int32.min_int);
  check "collision compares exact bytes" true
    (T.find table ~expected:collision ~name:"C" ~mask:0x40l);
  check "instance two skips class and collision" true
    (T.find ~instance:2L table ~expected:first ~name:"A" ~mask:0x40l);
  check "read-only selection" true
    (T.selects table ~expected:flagged ~name:"A" ~mask:0x40l);
  count "only selected first" 1L first;
  count "only selected collision" 1L collision;
  count "only selected class" 1L class_;
  count "only selected flagged" 2L flagged

let chain_and_missing_instances () =
  let first = T.create ~size:1 and second = T.create ~size:4 in
  let newest = H.create_function ~name:"A" in
  let older = H.create_function ~name:"A" in
  let oldest = H.create_function ~name:"A" in
  T.add first newest;
  T.add second oldest;
  T.add second older;
  T.set_next first second;
  check "single table excludes successor" false
    (T.find ~instance:2L first ~expected:older ~name:"A" ~mask:0x40l);
  check "remaining instance spans tables" true
    (T.find ~instance:2L ~chain:true first ~expected:older ~name:"A" ~mask:0x40l);
  check "third instance spans tables" true
    (T.find ~instance:3L ~chain:true first ~expected:oldest ~name:"A"
       ~mask:0x40l);
  List.iter
    (fun instance ->
      check "zero and exhausted instances do not count" false
        (T.find ~instance ~chain:true first ~expected:newest ~name:"A"
           ~mask:0x40l))
    [ 0L; 4L; Int64.max_int ];
  check "empty mask" false
    (T.find ~chain:true first ~expected:newest ~name:"A" ~mask:0l);
  check "missing bytes" false
    (T.find ~chain:true first ~expected:newest ~name:"missing" ~mask:0x40l);
  count "skipped local match remains unused" 0L newest;
  count "selected second match" 1L older;
  count "selected third match" 1L oldest

let rejects action =
  try
    action ();
    false
  with Invalid_argument _ -> true

let ownership_and_cycles () =
  let first = T.create ~size:2 and second = T.create ~size:2 in
  let record = H.create_function ~name:"A" in
  T.add first record;
  check "duplicate insertion" true (rejects (fun () -> T.add first record));
  check "one native next pointer cannot join two tables" true
    (rejects (fun () -> T.add second record));
  T.set_next first second;
  check "self cycle" true (rejects (fun () -> T.set_next first first));
  check "indirect cycle" true (rejects (fun () -> T.set_next second first));
  check "rejected insertion retains original bucket" true
    (T.find first ~expected:record ~name:"A" ~mask:0x40l);
  check "rejected cycle retains empty successor" false
    (T.find second ~expected:record ~name:"A" ~mask:0x40l);
  List.iter
    (fun size ->
      check "invalid table size" true
        (rejects (fun () -> ignore (T.create ~size))))
    [ 0; -1; 3; 17 ];
  check "negative instance" true
    (rejects (fun () ->
         ignore
           (T.find ~instance:(-1L) first ~expected:record ~name:"A" ~mask:0x40l)))

let collection_and_retained_chain () =
  let first = T.create ~size:2 in
  let record = H.create_function ~name:"A" in
  let weak_table = Weak.create 1 and weak_record = Weak.create 1 in
  let populate () =
    let second = T.create ~size:4 in
    let collision = H.create_function ~name:"E" in
    T.add second record;
    T.add second collision;
    Weak.set weak_table 0 (Some second);
    Weak.set weak_record 0 (Some collision);
    T.set_next first second
  in
  populate ();
  Gc.full_major ();
  Gc.compact ();
  check "successor handle collected" true (Weak.get weak_table 0 = None);
  check "collision handle collected" true (Weak.get weak_record 0 = None);
  check "C-owned chain and bucket records survive" true
    (T.find ~chain:true first ~expected:record ~name:"A" ~mask:0x40l);
  let replacement = T.create ~size:2 in
  T.set_next first replacement;
  Gc.full_major ();
  (* Releasing the old table clears membership and releases only its lease.
     The independently retained source record remains alive. *)
  count "record survives collected owner" 1L record;
  T.add replacement record;
  check "record retains its actual count on a new owner" true
    (T.find ~chain:true first ~expected:record ~name:"A" ~mask:0x40l);
  count "retained record increment" 2L record

let domain_guards () =
  let table = T.create ~size:2 in
  let record = H.create_function ~name:"A" in
  T.add table record;
  let other =
    Domain.spawn (fun () ->
        List.for_all Fun.id
          [
            rejects (fun () -> T.add table record);
            rejects (fun () -> T.set_next table (T.create ~size:2));
            rejects (fun () ->
                ignore (T.find table ~expected:record ~name:"A" ~mask:0x40l));
            rejects (fun () ->
                ignore
                  (T.find (T.create ~size:2) ~expected:record ~name:"A"
                     ~mask:0x40l));
          ])
  in
  check "foreign domain rejected before native access" true (Domain.join other);
  count "rejected domains preserve count" 0L record;
  check "original domain remains usable" true
    (T.find table ~expected:record ~name:"A" ~mask:0x40l)

let finalizers_in_another_domain () =
  let retained =
    List.init 128 (fun i ->
        let record = H.create_function ~name:(Printf.sprintf "Retained%d" i) in
        let owner = T.create ~size:2 in
        T.add owner record;
        record)
  in
  let collect =
    Domain.spawn (fun () ->
        for _ = 1 to 32 do
          Gc.full_major ()
        done)
  in
  for _ = 1 to 20 do
    List.iter H.increment retained
  done;
  Domain.join collect;
  Gc.full_major ();
  Gc.compact ();
  let table = T.create ~size:256 in
  List.iteri
    (fun i record ->
      count "other-domain owner finalization preserves original record" 20L
        record;
      T.add table record;
      check "released native membership can join its new actual owner" true
        (T.find table ~expected:record
           ~name:(Printf.sprintf "Retained%d" i)
           ~mask:0x40l);
      count "retained record after cross-domain collection" 21L record)
    retained

let tests =
  [
    Alcotest.test_case "pinned byte hash and instruction oracle" `Quick
      hash_and_instruction_oracle;
    Alcotest.test_case "native buckets, physical selection and type masks"
      `Quick bucket_priority_and_types;
    Alcotest.test_case "native chain priority and selected instances" `Quick
      chain_and_missing_instances;
    Alcotest.test_case "native record ownership and acyclic chains" `Quick
      ownership_and_cycles;
    Alcotest.test_case "collected handles retain native chain allocations"
      `Quick collection_and_retained_chain;
    Alcotest.test_case "native table and record domain guards" `Quick
      domain_guards;
    Alcotest.test_case "native leases survive other-domain finalizers" `Quick
      finalizers_in_another_domain;
  ]
