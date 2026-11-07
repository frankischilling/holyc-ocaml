module Allocation = Holyc_lib__Ir.Scored_register_allocation

let candidates scores =
  List.mapi
    (fun member score ->
      {
        Allocation.member;
        score;
        offset = Int64.of_int (16 + (8 * member));
        lea_balance = 0L;
        pointer_depth = 0;
        has_dimensions = false;
        name = "m" ^ string_of_int member;
      })
    scores

let ties () =
  List.iter
    (fun (count, expected) ->
      let actual =
        Allocation.sort (candidates (List.init count (fun _ -> 2L)))
        |> List.map (fun candidate -> candidate.Allocation.member)
      in
      Alcotest.(check (list int))
        "original QSort2b equal-score permutation" expected actual)
    [
      (2, [ 1; 0 ]);
      (6, [ 3; 4; 5; 0; 1; 2 ]);
      (7, [ 4; 5; 6; 3; 0; 1; 2 ]);
      (8, [ 5; 4; 7; 6; 1; 0; 3; 2 ]);
      (9, [ 6; 5; 8; 7; 4; 1; 0; 3; 2 ]);
    ]

let allocate () =
  let check label reserved scores expected =
    Alcotest.(check (list (pair int int)))
      label expected
      (Allocation.allocate ~reserved (candidates scores))
  in
  check "six candidates do not open nonpointer bank" []
    (List.init 6 (fun _ -> 2L))
    [ (3, 6); (4, 7); (5, 10); (0, 11); (1, 14); (2, 15) ];
  check "seven candidates open R12/R13 first" []
    (List.init 7 (fun _ -> 2L))
    [ (4, 12); (5, 13); (6, 6); (3, 7); (0, 10); (1, 11); (2, 14) ];
  check "ninth tied candidate spills" []
    (List.init 9 (fun _ -> 2L))
    [ (6, 12); (5, 13); (8, 6); (7, 7); (4, 10); (1, 11); (0, 14); (3, 15) ];
  check "zero-score records participate before trimming" [] [ 0L; 0L; 2L; 2L ]
    [ (2, 6); (3, 7) ];
  check "reservations do not lower threshold" [ 6; 7; 10; 11; 14; 15 ]
    [ 2L; 2L ] [];
  check "both banks honor reservations" [ 12; 6; 10; 14 ]
    (List.init 7 (fun _ -> 2L))
    [ (4, 13); (5, 7); (6, 11); (3, 15) ];
  let forced = Int64.div Int64.max_int 2L in
  check "unused forced score occupies first slot" [] [ 2L; forced ]
    [ (1, 6); (0, 7) ];
  check "forced occurrences still break forced ties" []
    [ Int64.add forced 4L; Int64.add forced 2L ]
    [ (0, 6); (1, 7) ];
  check "all zero members disappear after sorting" [] [ 0L; 0L; 0L ] [];
  let filtered =
    candidates [ 2L; 2L; 2L; 2L ]
    |> List.mapi (fun index candidate ->
        if index = 1 then { candidate with Allocation.lea_balance = 1L }
        else if index = 2 then { candidate with Allocation.offset = 0L }
        else candidate)
  in
  Alcotest.(check (list (pair int int)))
    "escape/zero offset filter follows sorting"
    [ (3, 6); (0, 7) ]
    (Allocation.allocate ~reserved:[] filtered)

let member_shapes () =
  let mixed count pointer_start =
    candidates (List.init count (fun _ -> 2L))
    |> List.mapi (fun index candidate ->
        {
          candidate with
          Allocation.pointer_depth = (if index >= pointer_start then 1 else 0);
        })
  in
  List.iter
    (fun (count, scalar_count, expected) ->
      Alcotest.(check (list (pair int int)))
        "mixed original banks" expected
        (Allocation.allocate ~reserved:[] (mixed count scalar_count)))
    [
      (7, 3, [ (0, 12); (1, 13); (4, 6); (5, 7); (6, 10); (3, 11); (2, 14) ]);
      ( 8,
        4,
        [ (1, 12); (0, 13); (5, 6); (4, 7); (7, 10); (6, 11); (3, 14); (2, 15) ]
      );
      ( 9,
        5,
        [ (4, 12); (1, 13); (6, 6); (5, 7); (8, 10); (7, 11); (0, 14); (3, 15) ]
      );
      (9, 0, [ (6, 6); (5, 7); (8, 10); (7, 11); (4, 14); (1, 15) ]);
    ];
  let dimensions =
    candidates [ 8L; 6L; 4L; 2L ]
    |> List.mapi (fun index candidate ->
        {
          candidate with
          Allocation.has_dimensions = true;
          offset = (if index = 1 then -8L else candidate.Allocation.offset);
          name = (if index = 2 then "argv" else candidate.Allocation.name);
        })
  in
  Alcotest.(check (list (pair int int)))
    "general bank admits positive dimensioned parameters except argv"
    [ (0, 6); (3, 7) ]
    (Allocation.allocate ~reserved:[] dimensions);
  let seven = mixed 7 3 in
  let escaped =
    List.mapi
      (fun index candidate ->
        if index = 4 then { candidate with Allocation.lea_balance = 1L }
        else candidate)
      seven
  in
  Alcotest.(check (list (pair int int)))
    "LEA filtering closes nonpointer bank only after sorting"
    [ (5, 6); (6, 7); (3, 10); (0, 11); (1, 14); (2, 15) ]
    (Allocation.allocate ~reserved:[] escaped)

let local_array_members () =
  let base = candidates [ 12L; 10L; 8L; 6L; 4L; 2L; 0L ] in
  let array score lea_balance =
    List.mapi
      (fun index candidate ->
        if index = 6 then
          {
            candidate with
            Allocation.has_dimensions = true;
            offset = -40000L;
            score;
            lea_balance;
          }
        else candidate)
      base
  in
  let check label expected candidates =
    Alcotest.(check (list (pair int int)))
      label expected
      (Allocation.allocate ~reserved:[] candidates)
  in
  let general = [ (0, 6); (1, 7); (2, 10); (3, 11); (4, 14); (5, 15) ] in
  let nonpointer = [ (0, 12); (1, 13); (2, 6); (3, 7); (4, 10); (5, 11) ] in
  check "ordinary unused dimensioned member sorts then trims" general
    (array 0L 0L);
  check "unused forced array survives but never receives a home" nonpointer
    (array (Int64.div Int64.max_int 2L) 0L);
  check "zero index cancellation retains array in candidate pressure" nonpointer
    (array 2L 0L);
  check "interior index address filters array before candidate pressure" general
    (array 1L 1L);
  check "forced array LEA still filters before candidate pressure" general
    (array (Int64.add (Int64.div Int64.max_int 2L) 1L) 1L)

let explicit_requests () =
  let check label requests owners shadowed conflicts =
    let result = Allocation.resolve_explicit requests in
    Alcotest.(check (list (pair int int)))
      (label ^ " final owners") owners result.owners;
    Alcotest.(check (list (pair int int)))
      (label ^ " displaced members")
      shadowed result.shadowed;
    Alcotest.(check (list (triple int int int)))
      (label ^ " original warnings")
      conflicts
      (List.map
         (fun (conflict : Allocation.explicit_conflict) ->
           ( conflict.previous_member,
             conflict.replacement_member,
             conflict.register ))
         result.conflicts)
  in
  check "empty" [] [] [] [];
  check "one owner" [ (0, 12) ] [ (0, 12) ] [] [];
  check "three owners of one bank"
    [ (0, 12); (2, 12); (5, 12) ]
    [ (5, 12) ]
    [ (0, 12); (2, 12) ]
    [ (0, 2, 12); (2, 5, 12) ];
  check "interleaved replacement order"
    [ (0, 12); (1, 6); (2, 12); (4, 6); (7, 12) ]
    [ (4, 6); (7, 12) ]
    [ (0, 12); (1, 6); (2, 12) ]
    [ (0, 2, 12); (1, 4, 6); (2, 7, 12) ];
  check "reversed source traversal"
    [ (7, 12); (2, 12); (0, 12) ]
    [ (0, 12) ]
    [ (7, 12); (2, 12) ]
    [ (7, 2, 12); (2, 0, 12) ];
  List.iter
    (fun register ->
      check "supported variable bank"
        [ (0, register); (1, register) ]
        [ (1, register) ]
        [ (0, register) ]
        [ (0, 1, register) ])
    [ 6; 7; 10; 11; 12; 13; 14; 15 ];
  let resolved = Allocation.resolve_explicit [ (1, 6); (2, 6) ] in
  Alcotest.(check (list (pair int int)))
    "one final reservation still excludes RSI from automatic allocation"
    [ (0, 7) ]
    (Allocation.allocate
       ~reserved:(List.map snd resolved.owners)
       (candidates [ 2L ]))

let charged_sort () =
  let values = candidates (List.init 8 (fun _ -> 2L)) in
  let expected = [ 5; 4; 7; 6; 1; 0; 3; 2 ] in
  let charges = ref 0 in
  let result = Allocation.sort ~charge:(fun () -> incr charges) values in
  Alcotest.(check (list int))
    "charged comparisons preserve pinned unstable ties" expected
    (List.map (fun value -> value.Allocation.member) result);
  Alcotest.(check bool)
    "comparisons and partitions charge work" true (!charges > 8);
  let limited count =
    let remaining = ref count in
    fun () -> if !remaining = 0 then raise Exit else decr remaining
  in
  Alcotest.(check (list int))
    "exact sort allowance succeeds" expected
    (Allocation.sort ~charge:(limited !charges) values
    |> List.map (fun value -> value.Allocation.member));
  Alcotest.check_raises "one-below sort allowance aborts complete result" Exit
    (fun () -> ignore (Allocation.sort ~charge:(limited (!charges - 1)) values));
  Alcotest.(check (list int))
    "failed sort does not mutate source candidates" expected
    (Allocation.sort values |> List.map (fun value -> value.Allocation.member));
  let charges = ref 0 in
  let expected_allocation = Allocation.allocate ~reserved:[] values in
  Alcotest.(check (list (pair int int)))
    "charged bank scans preserve allocation" expected_allocation
    (Allocation.allocate ~charge:(fun () -> incr charges) ~reserved:[] values);
  Alcotest.check_raises "bank allocation exhaustion never returns partial homes"
    Exit (fun () ->
      ignore
        (Allocation.allocate
           ~charge:(limited (!charges - 1))
           ~reserved:[] values));
  let patterned =
    candidates
      (List.init 10000 (fun index -> Int64.of_int (index * 7919 mod 10007)))
  in
  let sorted = Allocation.sort patterned in
  Alcotest.(check int)
    "large partition stack retains every member" 10000 (List.length sorted);
  let rec descending = function
    | first :: (second :: _ as rest) ->
        first.Allocation.score >= second.score && descending rest
    | _ -> true
  in
  Alcotest.(check bool)
    "large patterned scores sort without host recursion" true
    (descending sorted)

let () =
  Alcotest.run "TempleOS scored register allocation"
    [
      ( "source algorithm",
        [
          Alcotest.test_case "unstable ties" `Quick ties;
          Alcotest.test_case "charged iterative partitions" `Quick charged_sort;
          Alcotest.test_case "ordered explicit owners" `Quick explicit_requests;
          Alcotest.test_case "local array candidate membership" `Quick
            local_array_members;
          Alcotest.test_case "member shapes" `Quick member_shapes;
          Alcotest.test_case "register banks and filters" `Quick allocate;
        ] );
    ]
