open Yojson.Safe.Util

let read name =
  let c = open_in_bin ("oracle/automatic-void/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in c)
    (fun () -> really_input_string c (in_channel_length c))

let fixture stem =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "pinned compiler" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (suffix, key) ->
      Alcotest.(check string)
        "retained native evidence"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read (stem ^ suffix))
        |> Digestif.SHA256.to_hex))
    [ (".hc", "payload_sha256"); (".capture", "capture_sha256") ];
  let fields = metadata |> member "fields" |> to_assoc in
  let expected =
    List.map
      (fun (name, value) -> name ^ "=" ^ string_of_int (to_int value))
      fields
    @ [ "POINTER_DONE" ]
  in
  let lines =
    read (stem ^ ".capture")
    |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  let values =
    List.filter
      (fun line -> not (String.starts_with ~prefix:"CODE|" line))
      lines
  in
  Alcotest.(check (list string))
    "two complete native observations" (expected @ expected) values;
  (metadata, lines)

let layout () =
  let metadata, lines = fixture "layout" in
  List.iter
    (fun (name, count) ->
      let prefix = "CODE|" ^ name ^ "|" in
      let code = List.filter (String.starts_with ~prefix) lines in
      let count = to_int count in
      Alcotest.(check int)
        "two bounded instruction records" (count * 2) (List.length code);
      List.iteri
        (fun index line ->
          let fields = String.split_on_char '|' line in
          Alcotest.(check string)
            "original instruction order"
            (string_of_int (index mod count))
            (List.nth fields 2);
          let bytes = List.nth fields 3 in
          Alcotest.(check bool)
            "stop at the first return"
            (index mod count = count - 1)
            (bytes = "C3"))
        code)
    (metadata |> member "instruction_counts" |> to_assoc)

let () =
  Alcotest.run "Automatic U0 native evidence"
    [
      ( "source",
        [
          Alcotest.test_case "declaration and initializer observations" `Quick
            (fun () -> ignore (fixture "native"));
          Alcotest.test_case "assignment alias is not a no-op" `Quick layout;
        ] );
    ]
