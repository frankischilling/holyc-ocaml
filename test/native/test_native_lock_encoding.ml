open Holyc_lib
module Encoder = X86_64_encoder
module Image = X86_64_program

external contend : string array -> int -> int64 -> int64 * int64
  = "holyc_test_native_lock_contend"

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let hex bytes =
  String.to_seq bytes
  |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> List.of_seq |> String.concat ""

let widths =
  [
    (Encoder.Atomic8, 8, "U8");
    (Encoder.Atomic16, 16, "U16");
    (Encoder.Atomic32, 32, "U32");
    (Encoder.Atomic64, 64, "U64");
  ]

let operations =
  [
    (Encoder.Add, "+=", 0x00);
    (Encoder.Sub, "-=", 0x28);
    (Encoder.And, "&=", 0x20);
    (Encoder.Or, "|=", 0x08);
    (Encoder.Xor, "^=", 0x30);
  ]

let encode instructions =
  Encoder.encode_all ~max_code_bytes:256 instructions |> checked

let natural_mask bits =
  if bits = 64 then Int64.minus_one else Int64.pred (Int64.shift_left 1L bits)

let canary bits =
  Int64.logand 0xa5a5a5a5a5a5a5a5L (Int64.lognot (natural_mask bits))

let native test =
  match Native_execution.platform () with
  | Windows_x86_64 -> test Encoder.Windows_x64
  | Linux_x86_64 -> test Encoder.System_v_x64
  | Unsupported ->
      Printf.printf "Native LOCK checks require Windows or Linux x86-64.\n%!"

let exact_encoding () =
  List.iter
    (fun (width, bits, _) ->
      let prefix = if width = Encoder.Atomic16 then "f066" else "f0" in
      List.iter
        (fun (base, source, rex, modrm) ->
          let rex = rex lor if width = Encoder.Atomic64 then 8 else 0 in
          List.iter
            (fun (operation, spelling, byte_opcode) ->
              let instruction =
                Encoder.Lock_binary_indirect (operation, base, width, source)
              in
              let expected =
                Printf.sprintf "%s%02x%02x%02x00000000" prefix rex
                  (byte_opcode + if bits = 8 then 0 else 1)
                  modrm
              in
              Alcotest.(check string)
                (Printf.sprintf "%s exact %d-bit memory opcode" spelling bits)
                expected
                (hex (Encoder.encode instruction));
              Alcotest.(check int)
                "reported atomic instruction size"
                (String.length expected / 2)
                (Encoder.size instruction))
            operations;
          let instruction =
            Encoder.Lock_cmpxchg_indirect (base, width, source)
          in
          let expected =
            Printf.sprintf "%s%02x0f%02x%02x00000000" prefix rex
              (if bits = 8 then 0xb0 else 0xb1)
              modrm
          in
          Alcotest.(check string)
            "exact-width locked CMPXCHG" expected
            (hex (Encoder.encode instruction));
          Alcotest.(check int)
            "reported CMPXCHG size"
            (String.length expected / 2)
            (Encoder.size instruction))
        [
          (Encoder.Rdx, Encoder.Rcx, 0x40, 0x8a);
          (Encoder.R8, Encoder.R10, 0x45, 0x90);
        ])
    widths;
  match
    Encoder.encode_all ~max_code_bytes:64
      [
        Encoder.Lock_binary_indirect
          (Encoder.Imul, Encoder.Rdx, Encoder.Atomic64, Encoder.Rcx);
      ]
  with
  | Error _ -> ()
  | Ok _ ->
      Alcotest.fail
        "LOCK IMUL must reject before emitting an invalid instruction"

let binary_entry abi operation width operand =
  encode
    [
      Encoder.Capture_status abi;
      Encoder.Mov (Encoder.Rdx, Encoder.R11);
      Encoder.Mov_imm64 (Encoder.Rcx, operand);
      Encoder.Lock_binary_indirect (operation, Encoder.Rdx, width, Encoder.Rcx);
      Encoder.Mov_imm64 (Encoder.Rax, 0L);
      Encoder.Ret;
    ]

let binary_contention () =
  native (fun abi ->
      let threads = 3 and rounds = 100003 in
      let total = Int64.of_int (threads * rounds) in
      List.iter
        (fun (width, bits, _) ->
          List.iter
            (fun operation ->
              let initial = Int64.logor (canary bits) 13L in
              let entry = binary_entry abi operation width 1L in
              let actual, returned =
                contend (Array.make threads entry) rounds initial
              in
              let arithmetic =
                if operation = Encoder.Add then Int64.add 13L total
                else Int64.sub 13L total
              in
              let expected =
                Int64.logor (canary bits)
                  (Int64.logand arithmetic (natural_mask bits))
              in
              Alcotest.(check int64)
                (Printf.sprintf
                   "contended %d-bit update preserves every write and adjacent \
                    byte"
                   bits)
                expected actual;
              Alcotest.(check int64) "binary probe returns zero" 0L returned)
            [ Encoder.Add; Encoder.Sub ])
        widths)

let bit_entry abi operation width bits =
  let load =
    match width with
    | Encoder.Atomic64 -> Encoder.Load_indirect (Encoder.Rax, Encoder.R8, 0)
    | Atomic8 ->
        Encoder.Load_indirect_narrow
          (Encoder.Rax, Encoder.R8, Encoder.Frame8, Encoder.Zero_extend)
    | Atomic16 ->
        Encoder.Load_indirect_narrow
          (Encoder.Rax, Encoder.R8, Encoder.Frame16, Encoder.Zero_extend)
    | Atomic32 ->
        Encoder.Load_indirect_narrow
          (Encoder.Rax, Encoder.R8, Encoder.Frame32, Encoder.Zero_extend)
  in
  let retry =
    [
      Encoder.Mov (Encoder.Rdx, Encoder.Rax);
      Encoder.Bit (operation, Encoder.Rdx, Encoder.Rcx);
      Encoder.Lock_cmpxchg_indirect (Encoder.R8, width, Encoder.Rdx);
    ]
  in
  let retry_bytes =
    List.fold_left
      (fun total instruction -> total + Encoder.size instruction)
      0 retry
  in
  encode
    ([
       Encoder.Capture_status abi;
       Encoder.Mov (Encoder.R8, Encoder.R11);
       load;
       Encoder.Mov_imm64 (Encoder.Rcx, Int64.of_int (bits - 1));
     ]
    @ retry
    @ [
        Encoder.Jump_not_equal (Int64.of_int (-retry_bytes - 6));
        Encoder.Bit (Encoder.Bt, Encoder.Rax, Encoder.Rcx);
        Encoder.Mov_imm64 (Encoder.Rax, 0L);
        Encoder.Setcc (Encoder.B, Encoder.Rax);
        Encoder.Ret;
      ])

let bit_contention () =
  native (fun abi ->
      let threads = 3 and rounds = 25001 in
      let total = Int64.of_int (threads * rounds) in
      List.iter
        (fun (width, bits, _) ->
          let mask = Int64.shift_left 1L (bits - 1) in
          List.iter
            (fun operation ->
              let initial =
                if operation = Encoder.Btr then Int64.logor (canary bits) mask
                else canary bits
              in
              let expected =
                if operation = Encoder.Btr then canary bits
                else Int64.logor (canary bits) mask
              in
              let expected_prior =
                match operation with
                | Encoder.Bts -> Int64.pred total
                | Btr -> 1L
                | Btc -> Int64.div total 2L
                | Bt -> assert false
              in
              let entry = bit_entry abi operation width bits in
              let actual, prior =
                contend (Array.make threads entry) rounds initial
              in
              Alcotest.(check int64)
                "contended bit mutation stays within its exact scalar width"
                expected actual;
              Alcotest.(check int64)
                "prior-bit results belong to successful atomic exchanges"
                expected_prior prior)
            [ Encoder.Bts; Encoder.Btr; Encoder.Btc ])
        widths)

let contains bytes expected =
  let length = String.length bytes and target = String.length expected in
  let rec search offset =
    offset + target <= length
    && (String.sub bytes offset target = expected || search (offset + 1))
  in
  search 0

let compile mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-lock-encoding.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  Native_program.compile session ~config ~source
  |> Result.map_error diagnostics
  |> checked
  |> fun checked -> checked.value

let source_emits_locked_memory () =
  native (fun _ ->
      List.iter
        (fun mode ->
          List.iter
            (fun (width, _, type_name) ->
              List.iter
                (fun (operation, spelling, _) ->
                  let source =
                    Printf.sprintf
                      "I64 F(){%s value=13;lock value%s1;return value;}F();"
                      type_name spelling
                  in
                  let image = compile mode source in
                  let expected =
                    Encoder.encode
                      (Encoder.Lock_binary_indirect
                         (operation, Encoder.Rdx, width, Encoder.Rcx))
                  in
                  Alcotest.(check bool)
                    ("source uses the checked memory instruction: " ^ source)
                    true
                    (contains (Image.code image) expected))
                operations;
              let source =
                Printf.sprintf
                  "public _intern 0x7d Bool LBtc(U8 *p,I64 n);I64 F(){%s \
                   value=0;LBtc(&value,3);return value;}F();"
                  type_name
              in
              let image = compile mode source in
              let expected =
                Encoder.encode
                  (Encoder.Lock_cmpxchg_indirect (Encoder.R8, width, Encoder.Rdx))
              in
              Alcotest.(check bool)
                ("locked bit source uses exact-width CMPXCHG: " ^ source)
                true
                (contains (Image.code image) expected))
            widths;
          let image =
            compile mode
              "I64 F(){I64 (*p)(I64 n);p=34;lock p++;return p==42;}F();"
          in
          Alcotest.(check bool)
            "numeric callback update uses its checked qword atomic instruction"
            true
            (contains (Image.code image)
               (Encoder.encode
                  (Encoder.Lock_binary_indirect
                     (Encoder.Add, Encoder.Rdx, Encoder.Atomic64, Encoder.Rcx)))))
        [ Preprocessor.Jit; Preprocessor.Aot ])

let () =
  Alcotest.run "native LOCK encoding and contention"
    [
      ( "encoding",
        [
          Alcotest.test_case "exact memory forms and invalid register operation"
            `Quick exact_encoding;
          Alcotest.test_case "source emits the tested atomic forms" `Quick
            source_emits_locked_memory;
        ] );
      ( "contention",
        [
          Alcotest.test_case "native exact-width add and subtract" `Quick
            binary_contention;
          Alcotest.test_case "native exact-width bit exchange and prior value"
            `Quick bit_contention;
        ] );
    ]
