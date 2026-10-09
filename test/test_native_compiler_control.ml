module C = Holyc_lib__Common.Native_compiler_control

let check = Alcotest.(check bool)

let count name expected control =
  Alcotest.(check int64) name expected (C.warning_count control)

let errors name expected control =
  Alcotest.(check int64) name expected (C.error_count control)

let instruction_storage () =
  match Holyc_lib.Native_execution.platform () with
  | Holyc_lib.Native_execution.Unsupported -> ()
  | _ ->
      check "literal-offset BT/BTS/BTR/INC comparison" true
        (C.verify_storage ())

let option_bits_and_old_values () =
  let control = C.create ~options:0x90000L in
  Alcotest.(check int64) "original defaults" 0x90000L (C.options control);
  for bit_index = 0 to 63 do
    let initially_enabled = bit_index = 16 || bit_index = 19 in
    check "initial field bit" initially_enabled
      (C.get_option control ~bit_index);
    check "BEqu returns old enabled state" initially_enabled
      (C.set_option control ~bit_index true);
    check "setting twice returns true" true
      (C.set_option control ~bit_index true)
  done;
  Alcotest.(check int64)
    "all 64 bits retain their pattern" (-1L) (C.options control);
  for bit_index = 0 to 63 do
    check "clearing returns old true state" true
      (C.set_option control ~bit_index false);
    check "clearing twice returns false" false
      (C.set_option control ~bit_index false)
  done;
  Alcotest.(check int64) "all bits cleared" 0L (C.options control);
  count "bit operations do not count warnings" 0L control;
  errors "bit operations do not count errors" 0L control

let child_and_shared_storage () =
  let parent = C.create ~options:0x90000L in
  let directive = parent in
  check "new input has no value return" false (C.has_return parent);
  C.set_has_return directive true;
  check "directive shares HAS_RETURN" true (C.has_return parent);
  C.increment_warning directive;
  C.increment_error directive;
  ignore (C.set_option directive ~bit_index:37 true);
  let child = C.child parent in
  check "child starts with fresh flags" false (C.has_return child);
  C.set_has_return child true;
  C.set_has_return parent false;
  Alcotest.(check int64)
    "child copies current native opts" 0x2000090000L (C.options child);
  count "child starts a fresh warning count" 0L child;
  errors "child starts a fresh error count" 0L child;
  C.increment_warning child;
  C.increment_error child;
  C.increment_error child;
  ignore (C.set_option child ~bit_index:19 false);
  ignore (C.set_option parent ~bit_index:37 false);
  Gc.full_major ();
  Gc.compact ();
  count "directive still shares the parent allocation" 1L directive;
  count "child counter is separate" 1L child;
  errors "directive shares parent error count" 1L directive;
  errors "child error counter is separate" 2L child;
  check "child return flag survives collection" true (C.has_return child);
  check "child flag does not change parent" false (C.has_return directive);
  check "child mutation does not clear parent bit" true
    (C.get_option parent ~bit_index:19);
  check "parent mutation does not clear child bit" true
    (C.get_option child ~bit_index:37);
  let next = C.child parent in
  Alcotest.(check int64)
    "next child copies live parent" 0x90000L (C.options next);
  count "successive child has a fresh counter" 0L next;
  errors "successive child has a fresh error count" 0L next

let rejects action =
  try
    action ();
    false
  with Invalid_argument _ -> true

let invalid_bits_preserve_fields () =
  let control = C.create ~options:Int64.min_int in
  C.increment_warning control;
  C.increment_error control;
  List.iter
    (fun bit_index ->
      check "out of field read rejects" true
        (rejects (fun () -> ignore (C.get_option control ~bit_index)));
      List.iter
        (fun enabled ->
          check "out of field write rejects" true
            (rejects (fun () ->
                 ignore (C.set_option control ~bit_index enabled))))
        [ false; true ])
    [ min_int; -1; 64; 65; max_int ];
  Alcotest.(check int64)
    "invalid operations preserve opts" Int64.min_int (C.options control);
  count "invalid operations preserve warning count" 1L control;
  errors "invalid operations preserve error count" 1L control

let original_domain () =
  let control = C.create ~options:0x90000L in
  let denied =
    Domain.spawn (fun () ->
        List.for_all rejects
          [
            (fun () -> ignore (C.options control));
            (fun () -> ignore (C.child control));
            (fun () -> ignore (C.get_option control ~bit_index:16));
            (fun () -> ignore (C.set_option control ~bit_index:16 false));
            (fun () -> ignore (C.warning_count control));
            (fun () -> C.increment_warning control);
            (fun () -> ignore (C.error_count control));
            (fun () -> C.increment_error control);
            (fun () -> ignore (C.has_return control));
            (fun () -> C.set_has_return control true);
          ])
    |> Domain.join
  in
  check "foreign domain rejects every field operation" true denied;
  Alcotest.(check int64)
    "foreign writes preserve options" 0x90000L (C.options control);
  count "foreign writes preserve count" 0L control;
  errors "foreign writes preserve error count" 0L control;
  (* Collect unrelated native allocations from another domain while retaining
     the original allocation through its owning OCaml handle. *)
  let garbage =
    Domain.spawn (fun () ->
        for _ = 1 to 128 do
          ignore (C.create ~options:(-1L))
        done;
        Gc.full_major ())
  in
  Domain.join garbage;
  C.increment_warning control;
  C.increment_error control;
  count "original allocation survives domain collection" 1L control;
  errors "error field survives domain collection" 1L control

let tests =
  [
    Alcotest.test_case "native field instruction oracle" `Quick
      instruction_storage;
    Alcotest.test_case "option bits and previous values" `Quick
      option_bits_and_old_values;
    Alcotest.test_case "shared and child native fields" `Quick
      child_and_shared_storage;
    Alcotest.test_case "invalid indices preserve fields" `Quick
      invalid_bits_preserve_fields;
    Alcotest.test_case "original domain and collection" `Quick original_domain;
  ]
