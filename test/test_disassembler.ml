module D = Holyc_lib.Asm_disassembler
module O = Holyc_lib.Asm_operand

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let decode ?(mode = O.Use64) ?(position = 0L) ?max_bytes ?max_instructions
    ?symbols hex =
  match
    D.decode ?max_bytes ?max_instructions ?symbols ~mode ~position (bytes hex)
  with
  | Ok report -> report
  | Error error -> Alcotest.failf "at byte %d: %s" error.offset error.message

let check ?mode ?position hex expected =
  let instructions = D.instructions (decode ?mode ?position hex) in
  Alcotest.(check (list string)) hex expected (List.map D.text instructions);
  Alcotest.(check int)
    "complete byte consumption"
    (String.length hex / 2)
    (List.fold_left
       (fun count (instruction : D.instruction) ->
         count + String.length instruction.bytes)
       0 instructions)

let independent_bytes () =
  List.iter
    (fun (hex, text) -> check hex [ text ])
    [
      ("55", "PUSH\tU64 RBP");
      ("4889E5", "MOV\tU64 RBP,U64 RSP");
      ("4881EC380B0000", "SUB\tU64 RSP,I32 00000B38");
      ("48B88877665544332211", "MOV\tU64 RAX,I64 1122334455667788");
      ("408AC4", "MOV\tU8 AL,U8 RSPu8");
      ("8AC4", "MOV\tU8 AL,U8 AH");
      ("488B44C8F8", "MOV\tU64 RAX,U64 F8[RAX+RCX*8]");
      ("648B00", "MOV\tU32 EAX,U32 FS:[RAX]");
      ("C21000", "RET1\tI16 0010");
      ("C8200000", "ENTER\tI16 0020");
      ("D9C1", "FLD\tST1");
      ("DEC1", "FADDP\tST1,ST0");
      ("D9E8", "FLD1");
      ("DD18", "FSTP\tU64 [RAX]");
      ("DC03", "FADD\tST0,U64 [RBX]");
      ("DC0B", "FMUL\tST0,U64 [RBX]");
      ("CD0E", "INT\tU8 0E Page Fault");
      ("CD30", "INT\tU8 30 MP Crash");
      ("CD20", "INT\tU8 20  ");
      ("CDFF", "INT\tU8 FF");
      ("F348A5", "REP_MOVSQ");
    ];
  check "F0490101" [ "LOCK"; "ADD\tU64 [R9],U64 RAX" ];
  check
    (String.concat "" (List.init 13 (fun _ -> "64")) ^ "8B00")
    [
      "MOV\tU32 EAX,U32 "
      ^ String.concat "" (List.init 8 (fun _ -> "FS:"))
      ^ "[RAX]";
    ];
  check ~mode:O.Use16 "B83412" [ "MOV\tU16 AX,I16 1234" ];
  check ~mode:O.Use32 "B878563412" [ "MOV\tU32 EAX,I32 12345678" ];
  check ~mode:O.Use32 "66B83412" [ "MOV\tU16 AX,I16 1234" ];
  check ~mode:O.Use16 "66B878563412" [ "MOV\tU32 EAX,I32 12345678" ]

let relative_targets () =
  check ~position:0x1000L "EBFE" [ "JMP\tI8 1000" ];
  let symbols =
    D.
      [
        { name = "start"; address = 0x1000L };
        { name = "next"; address = 0x1007L };
      ]
  in
  let report = decode ~position:0x1000L ~symbols "EBFE488B0500000000" in
  let instructions = D.instructions report in
  Alcotest.(check (list string))
    "exact symbols"
    [ "JMP\tI8 start"; "MOV\tU64 RAX,U64 [1009]" ]
    (List.map D.text instructions);
  Alcotest.(check (option int64))
    "relative jump" (Some 0x1000L) (List.hd instructions).jump_target;
  Alcotest.(check string)
    "stable map"
    "holyc-disassembly-map-v1\n\
     symbol 0000000000001000 start\n\
     symbol 0000000000001007 next\n\
     branch 0000000000001000 0000000000001000 start\n"
    (D.map report);
  Alcotest.(check string)
    "deterministic JSON"
    (Yojson.Safe.to_string (D.to_yojson report))
    (Yojson.Safe.to_string
       (D.to_yojson (decode ~position:0x1000L ~symbols "EBFE488B0500000000")))

let rejected ?(mode = O.Use64) ?(position = 0L) ?(max_bytes = 100)
    ?(max_instructions = 100) ?symbols hex offset =
  match
    D.decode ~max_bytes ~max_instructions ?symbols ~mode ~position (bytes hex)
  with
  | Ok report -> Alcotest.failf "unexpected report: %s" (D.listing report)
  | Error error ->
      Alcotest.(check int) "original failing byte" offset error.offset

let malformed_and_limits () =
  List.iter
    (fun hex -> rejected hex 0)
    [
      "48";
      "0F";
      "488B";
      "488B04";
      "488B4424";
      "48B800";
      "EB";
      "D9";
      "C82000";
      "C8200001";
      "F1";
      "0F0B";
      "8EF8";
    ];
  rejected "55F1" 1;
  check "55C3" [ "PUSH\tU64 RBP"; "RET" ];
  ignore (decode ~max_bytes:2 ~max_instructions:2 "55C3");
  rejected ~max_bytes:1 "55C3" 0;
  rejected ~max_instructions:1 "55C3" 1;
  rejected ~max_bytes:0 "" 0;
  rejected ~max_instructions:0 "" 0;
  rejected (String.concat "" (List.init 15 (fun _ -> "66")) ^ "C3") 0;
  ignore (decode ~position:(Int64.pred Int64.max_int) "C3");
  rejected ~position:Int64.max_int "C3" 0;
  rejected ~position:Int64.min_int "EB80" 0;
  rejected ~symbols:D.[ { name = "bad\nname"; address = 0L } ] "C3" 0;
  rejected
    ~symbols:
      D.[ { name = "same"; address = 0L }; { name = "same"; address = 1L } ]
    "C3" 0;
  Alcotest.(check int) "empty valid input" 0 (D.byte_count (decode ""))

let all_truncation_points () =
  List.iter
    (fun hex ->
      let length = String.length hex / 2 in
      for count = 1 to length - 1 do
        rejected (String.sub hex 0 (count * 2)) 0
      done)
    [
      "48B88877665544332211"; "488B44C8F8"; "C8200000"; "0F8400000000"; "F348A5";
    ]

let bounded_byte_streams () =
  let random = Random.State.make [| 0x5541736d |] in
  let check mode input =
    let length = String.length input in
    match D.decode ~mode ~position:0L input with
    | Error error ->
        Alcotest.(check bool)
          "error identifies an input byte" true
          (error.offset >= 0 && error.offset < length)
    | Ok report ->
        let consumed =
          List.fold_left
            (fun offset (instruction : D.instruction) ->
              Alcotest.(check int)
                "contiguous instruction" offset instruction.offset;
              let size = String.length instruction.bytes in
              Alcotest.(check bool)
                "bounded instruction" true
                (size > 0 && size <= 15);
              Alcotest.(check string)
                "original instruction bytes"
                (String.sub input offset size)
                instruction.bytes;
              offset + size)
            0 (D.instructions report)
        in
        Alcotest.(check int) "complete report" length consumed
  in
  List.iter
    (fun mode ->
      for value = 0 to 255 do
        check mode (String.make 1 (Char.chr value));
        List.iter
          (fun prefix ->
            check mode
              (String.init 2 (function
                | 0 -> Char.chr prefix
                | _ -> Char.chr value)))
          [ 0x0f; 0x40; 0x48; 0x64; 0x66; 0x67; 0xf0; 0xf3 ]
      done;
      for _ = 1 to 2048 do
        let length = 2 + Random.State.int random 14 in
        check mode
          (String.init length (fun _ -> Char.chr (Random.State.int random 256)))
      done)
    [ O.Use16; O.Use32; O.Use64 ]

let source_form_fixtures () =
  let path =
    if Sys.file_exists "fixtures/assembly-encoder-forms.tsv" then
      "fixtures/assembly-encoder-forms.tsv"
    else "test/fixtures/assembly-encoder-forms.tsv"
  in
  let channel = open_in path in
  let failures = ref [] and count = ref 0 in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      try
        while true do
          let line = String.trim (input_line channel) in
          if line <> "" && line.[0] <> '#' then
            match String.split_on_char '\t' line with
            | [ _; _; _; _; _; _; _; _; "!empty-opcode" ] -> ()
            | [ source_line; opcode; _; bits; _; _; _; _; hex ] -> (
                incr count;
                let mode =
                  match bits with
                  | "16" -> O.Use16
                  | "32" -> O.Use32
                  | _ -> O.Use64
                in
                let rejected =
                  List.mem
                    (int_of_string source_line)
                    [
                      659;
                      813;
                      814;
                      815;
                      965;
                      973;
                      974;
                      975;
                      976;
                      977;
                      978;
                      1197;
                      1202;
                      1203;
                      1212;
                      1219;
                      1231;
                      1258;
                      1259;
                    ]
                in
                match D.decode ~mode ~position:0L (bytes hex) with
                | Error _ when rejected -> ()
                | Ok report
                  when (not rejected)
                       && List.length (D.instructions report)
                          = if opcode = "FCLEX" then 2 else 1 -> ()
                | Ok report ->
                    failures :=
                      (source_line ^ " " ^ opcode ^ ": " ^ D.listing report)
                      :: !failures
                | Error error ->
                    failures :=
                      (source_line ^ " " ^ opcode ^ ": " ^ error.message)
                      :: !failures)
            | _ -> Alcotest.fail "invalid source form fixture"
        done
      with End_of_file -> ());
  Alcotest.(check int) "complete nonempty source form fixture set" 923 !count;
  Alcotest.(check (list string))
    "independent fixture instruction boundaries" [] (List.rev !failures)

let native_captures () =
  let open Yojson.Safe.Util in
  let directory =
    if Sys.file_exists "oracle/source-disassembly.json" then "oracle"
    else "test/oracle"
  in
  let fixture =
    Yojson.Safe.from_file (Filename.concat directory "source-disassembly.json")
  in
  Alcotest.(check string)
    "pinned reference" Holyc_lib.Version.reference_commit
    (fixture |> member "reference" |> member "commit" |> to_string);
  let manifest_path =
    if directory = "oracle" then "../reference/manifest.json"
    else "reference/manifest.json"
  in
  let audited =
    Yojson.Safe.from_file manifest_path |> member "files" |> to_list
  in
  let source_checks =
    fixture |> member "reference" |> member "source_checks" |> to_list
  in
  Alcotest.(check int)
    "ISO source identity checks" 8
    (List.length source_checks);
  Alcotest.(check (list string))
    "all captured producer and decoder sources"
    [
      "Compiler/AsmInit.HC";
      "Compiler/OpCodes.DD";
      "Compiler/UAsm.HC";
      "Kernel/BlkDev/DskDrv.HC";
      "Kernel/KDefine.HC";
      "Kernel/KHashA.HC";
      "Kernel/KMisc.HC";
      "Kernel/QSort.HC";
    ]
    (List.map (fun row -> row |> member "path" |> to_string) source_checks
    |> List.sort String.compare);
  List.iter
    (fun row ->
      let path = row |> member "path" |> to_string in
      let source =
        List.find
          (fun entry -> entry |> member "path" |> to_string = path)
          audited
      in
      Alcotest.(check string)
        ("ISO source canonical identity: " ^ path)
        (source |> member "sha256" |> to_string)
        (row |> member "git_blob_sha256" |> to_string))
    source_checks;
  let read file =
    let channel = open_in_bin file in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  let hash =
    Digestif.SHA256.(
      to_hex
        (digest_string
           (read (Filename.concat directory "source-disassembly.capture"))))
  in
  Alcotest.(check string)
    "original capture checksum"
    (fixture |> member "capture" |> member "raw_capture_sha256" |> to_string)
    hash;
  Alcotest.(check string)
    "original capture payload checksum"
    (fixture |> member "capture" |> member "payload_sha256" |> to_string)
    Digestif.SHA256.(
      to_hex
        (digest_string
           (read (Filename.concat directory "source-disassembly.hc"))));
  let raw =
    read (Filename.concat directory "source-disassembly.capture")
    |> String.split_on_char '\n'
    |> List.filter (String.starts_with ~prefix:"DOR|")
  in
  let first = List.filteri (fun i _ -> i < 89) raw
  and second = List.filteri (fun i _ -> i >= 89) raw in
  Alcotest.(check int) "all native observations" 178 (List.length raw);
  Alcotest.(check (list string)) "complete native repeats" first second;
  let checks = fixture |> member "checks" |> to_list in
  Alcotest.(check int)
    "compiler, kernel and literal captures" 89 (List.length checks);
  let original_records =
    List.map
      (fun row ->
        let target =
          match row |> member "jump_target" with
          | `Null -> -1L
          | value -> value |> to_string |> Int64.of_string
        in
        Printf.sprintf "DOR|%d|%d|%LX|%d|%LX|%s|%s"
          (row |> member "id" |> to_int)
          (row |> member "bits" |> to_int)
          (row |> member "position" |> to_string |> Int64.of_string)
          (row |> member "size" |> to_int)
          target
          (row |> member "bytes_hex" |> to_string)
          (row |> member "text" |> to_string))
      checks
  in
  Alcotest.(check (list string))
    "fixture expectations come from the original capture" first original_records;
  List.iter
    (fun row ->
      let mode =
        match row |> member "bits" |> to_int with
        | 16 -> O.Use16
        | 32 -> O.Use32
        | _ -> O.Use64
      in
      let position = row |> member "position" |> to_string |> Int64.of_string in
      let expected = row |> member "text" |> to_string in
      let report =
        decode ~mode ~position (row |> member "bytes_hex" |> to_string)
      in
      Alcotest.(check int)
        "one native instruction" 1
        (List.length (D.instructions report));
      let instruction = List.hd (D.instructions report) in
      Alcotest.(check int)
        "native byte length"
        (row |> member "size" |> to_int)
        (String.length instruction.bytes);
      let target =
        match row |> member "jump_target" with
        | `Null -> None
        | value -> Some (value |> to_string |> Int64.of_string)
      in
      Alcotest.(check (option int64))
        "native relative target" target instruction.jump_target;
      match target with
      | None ->
          Alcotest.(check string)
            "original native operands" expected (D.text instruction)
      | Some target ->
          let prefix = String.sub expected 0 (String.index expected '$') in
          Alcotest.(check string)
            "source spelling, width and relative destination"
            (prefix ^ Printf.sprintf "%LX" target)
            (D.text instruction))
    checks

let tests =
  [
    Alcotest.test_case "independent source-dialect bytes and modes" `Quick
      independent_bytes;
    Alcotest.test_case "relative addresses, symbols and deterministic reports"
      `Quick relative_targets;
    Alcotest.test_case "malformed input, budgets and address boundaries" `Quick
      malformed_and_limits;
    Alcotest.test_case "every interior truncation boundary" `Quick
      all_truncation_points;
    Alcotest.test_case "bounded deterministic malformed byte streams" `Quick
      bounded_byte_streams;
    Alcotest.test_case "all independent pinned form byte fixtures" `Quick
      source_form_fixtures;
    Alcotest.test_case "repeated pinned compiler and kernel UAsm captures"
      `Quick native_captures;
  ]
