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

let () =
  Alcotest.run "TempleOS scored register allocation"
    [
      ( "source algorithm",
        [
          Alcotest.test_case "unstable ties" `Quick ties;
          Alcotest.test_case "member shapes" `Quick member_shapes;
          Alcotest.test_case "register banks and filters" `Quick allocate;
        ] );
    ]
