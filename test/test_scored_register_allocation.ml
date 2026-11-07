module Allocation = Holyc_lib__Ir.Scored_register_allocation

let candidates scores =
  List.mapi
    (fun member score ->
      {
        Allocation.member;
        score;
        offset = Int64.of_int (16 + (8 * member));
        lea_balance = 0L;
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

let () =
  Alcotest.run "TempleOS scored register allocation"
    [
      ( "source algorithm",
        [
          Alcotest.test_case "unstable ties" `Quick ties;
          Alcotest.test_case "register banks and filters" `Quick allocate;
        ] );
    ]
