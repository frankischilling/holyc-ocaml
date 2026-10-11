module Encoding = Holyc_lib__Asm.Encoding
module Operand = Holyc_lib__Asm.Operand
module Facts = Holyc_lib__Generated.Opcode_keywords
module Native = Holyc_lib.X86_64_encoder

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject label = function
  | Error message ->
      Alcotest.(check bool) (label ^ " explains failure") true (message <> "")
  | Ok _ -> Alcotest.fail (label ^ " unexpectedly succeeded")

let form source_line =
  Facts.opcodes
  |> List.concat_map (fun (opcode : Facts.opcode) -> opcode.instructions)
  |> List.find (fun (form : Facts.instruction) ->
      form.source_line = source_line)

let numeric bytes range value = Encoding.field ~bytes ~range value |> checked

let hex bytes =
  String.to_seq bytes
  |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> List.of_seq |> String.concat ""

let check_plan label expected result =
  let plan = checked result in
  Alcotest.(check string) label expected (hex (Encoding.encode plan));
  Alcotest.(check int)
    (label ^ " length")
    (String.length expected / 2)
    (Encoding.size plan);
  plan

let numeric_boundaries () =
  let open Encoding in
  List.iter
    (fun (bytes, minimum, maximum, unsigned_maximum) ->
      List.iter
        (fun value -> ignore (numeric bytes Signed value))
        [ minimum; 0L; maximum ];
      List.iter
        (fun value -> ignore (numeric bytes Unsigned value))
        [ 0L; unsigned_maximum ];
      List.iter
        (fun value -> ignore (numeric bytes Bit_pattern value))
        [ minimum; -1L; 0L; maximum; unsigned_maximum ];
      reject "signed field below its minimum"
        (field ~bytes ~range:Signed (Int64.pred minimum));
      reject "signed field above its maximum"
        (field ~bytes ~range:Signed (Int64.succ maximum));
      reject "unsigned field below zero" (field ~bytes ~range:Unsigned (-1L));
      reject "unsigned field above its maximum"
        (field ~bytes ~range:Unsigned (Int64.succ unsigned_maximum));
      reject "bit pattern below its signed minimum"
        (field ~bytes ~range:Bit_pattern (Int64.pred minimum));
      reject "bit pattern above its unsigned maximum"
        (field ~bytes ~range:Bit_pattern (Int64.succ unsigned_maximum)))
    [
      (1, -128L, 127L, 255L);
      (2, -32768L, 32767L, 65535L);
      (4, -0x80000000L, 0x7fffffffL, 0xffffffffL);
    ];
  List.iter
    (fun range ->
      List.iter
        (fun value -> ignore (numeric 8 range value))
        [ Int64.min_int; -1L; 0L; Int64.max_int ])
    [ Signed; Unsigned; Bit_pattern ];
  List.iter
    (fun bytes ->
      reject "unsupported field width" (field ~bytes ~range:Signed 0L))
    [ -1; 0; 3; 5; 7; 9; max_int ]

let provenance_and_opcode_patches () =
  let mode = Operand.Use64 in
  let return = form 961 in
  let copied : Facts.instruction =
    Marshal.from_string (Marshal.to_string return []) 0
  in
  Alcotest.(check bool)
    "marshalling copied the generated record" false (copied == return);
  reject "copied generated form" (Encoding.plan ~mode ~form:copied ());
  reject "owned empty opcode form" (Encoding.plan ~mode ~form:(form 920) ());
  ignore (check_plan "owned RET" "c3" (Encoding.plan ~mode ~form:return ()));
  reject "patch on a fixed opcode"
    (Encoding.plan ~mode ~form:return ~opcode_patch:(Encoding.Or_low3 1) ());
  let mov = form 276 in
  let make patch =
    Encoding.plan ~mode ~form:mov ~prefixes:[ 0x49 ] ~opcode_patch:patch
      ~immediate:(numeric 8 Encoding.Bit_pattern 0x0123456789abcdefL)
      ()
  in
  ignore
    (check_plan "last-opcode register patch" "49bbefcdab8967452301"
       (make (Encoding.Or_low3 3)));
  List.iter
    (fun register ->
      reject "opcode patch outside three bits"
        (make (Encoding.Or_low3 register)))
    [ -1; 8; 255 ];
  reject "additive patch on an integer form" (make (Encoding.Add_low3 3));
  let floating =
    Facts.opcodes
    |> List.concat_map (fun (opcode : Facts.opcode) -> opcode.instructions)
    |> List.find (fun (form : Facts.instruction) ->
        form.flags land 0x004 <> 0
        && form.slash_value = 9 && form.argument2 = 0
        && form.opcode_modifier = 0)
  in
  let unpatched = Encoding.plan ~mode ~form:floating () |> checked in
  let patched =
    Encoding.plan ~mode ~form:floating ~opcode_patch:(Encoding.Add_low3 7) ()
    |> checked
  in
  let before = Encoding.encode unpatched and after = Encoding.encode patched in
  Alcotest.(check string)
    "floating opcode prefix is retained"
    (String.sub before 0 (String.length before - 1))
    (String.sub after 0 (String.length after - 1));
  Alcotest.(check int)
    "floating-stack patch changes only its low register field"
    (Char.code before.[String.length before - 1] + 7)
    (Char.code after.[String.length after - 1])

let prefixes_and_modes () =
  let return = form 961 in
  List.iter
    (fun prefixes ->
      reject "invalid prefix sequence"
        (Encoding.plan ~mode:Operand.Use64 ~form:return ~prefixes ()))
    [
      [ -1 ];
      [ 256 ];
      [ 0x00 ];
      [ 0x48; 0x66 ];
      [ 0x40; 0x41 ];
      [ 0x66; 0x66 ];
      [ 0xf0; 0x64; 0x66; 0x67; 0x48 ];
    ];
  reject "REX outside USE64"
    (Encoding.plan ~mode:Operand.Use32 ~form:return ~prefixes:[ 0x48 ] ());
  let legacy =
    Facts.opcodes
    |> List.concat_map (fun (opcode : Facts.opcode) -> opcode.instructions)
    |> List.find (fun (form : Facts.instruction) -> form.flags land 0x020 <> 0)
  in
  reject "legacy-only form in USE64"
    (Encoding.plan ~mode:Operand.Use64 ~form:legacy ());
  reject "missing operand-size prefix"
    (Encoding.plan ~mode:Operand.Use64 ~form:(form 263) ~modrm:0xc8 ());
  ignore
    (check_plan "explicit word width" "6689c8"
       (Encoding.plan ~mode:Operand.Use64 ~form:(form 263) ~prefixes:[ 0x66 ]
          ~modrm:0xc8 ()));
  reject "missing qword REX.W"
    (Encoding.plan ~mode:Operand.Use64 ~form:(form 265) ~modrm:0xc8 ());
  reject "missing implicit REX.W"
    (Encoding.plan ~mode:Operand.Use64 ~form:(form 780) ());
  ignore
    (check_plan "same-register XOR prefix suppression" "31c0"
       (Encoding.plan ~mode:Operand.Use64 ~form:(form 501) ~modrm:0xc0 ()));
  reject "extended-register XOR still requires W"
    (Encoding.plan ~mode:Operand.Use64 ~form:(form 501) ~prefixes:[ 0x45 ]
       ~modrm:0xc9 ());
  ignore
    (check_plan "extended-register XOR retains W" "4d31c9"
       (Encoding.plan ~mode:Operand.Use64 ~form:(form 501) ~prefixes:[ 0x4d ]
          ~modrm:0xc9 ()));
  ignore
    (check_plan "implicit qword CALL" "ffd0"
       (Encoding.plan ~mode:Operand.Use64 ~form:(form 573) ~modrm:0xd0 ()));
  ignore
    (check_plan "unsigned imm32 form clears W" "41b9ffffffff"
       (Encoding.plan ~mode:Operand.Use64 ~form:(form 274) ~prefixes:[ 0x41 ]
          ~opcode_patch:(Encoding.Or_low3 1)
          ~immediate:(numeric 4 Encoding.Unsigned 0xffffffffL)
          ()))

let structural_fields () =
  let mode = Operand.Use64 and mov = form 261 in
  let displacement = numeric 4 Encoding.Signed (-128L) in
  ignore
    (check_plan "fixed SIB with signed disp32" "488b842480ffffff"
       (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:0x84 ~sib:0x24
          ~displacement ()));
  reject "invalid ModR/M byte"
    (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:256 ());
  reject "invalid SIB byte"
    (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:0x84 ~sib:(-1)
       ~displacement ());
  reject "missing SIB"
    (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:0x84 ~displacement
       ());
  reject "SIB without its selector"
    (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:0x83 ~sib:0x24
       ~displacement ());
  reject "SIB on register addressing"
    (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:0xc4 ~sib:0x24 ());
  reject "wrong fixed displacement width"
    (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:0x83
       ~displacement:(numeric 1 Encoding.Signed 1L)
       ());
  reject "missing ModR/M" (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ());
  reject "extra ModR/M" (Encoding.plan ~mode ~form:(form 961) ~modrm:0xc0 ());
  reject "wrong slash extension"
    (Encoding.plan ~mode ~form:(form 680) ~prefixes:[ 0x48 ] ~modrm:0xc0 ());
  reject "relative field outside signed range"
    (Encoding.plan ~mode ~form:(form 584)
       ~immediate:(numeric 4 Encoding.Unsigned 0x80000000L)
       ());
  reject "RIP displacement outside signed range"
    (Encoding.plan ~mode ~form:mov ~prefixes:[ 0x48 ] ~modrm:0x05
       ~displacement:(numeric 4 Encoding.Unsigned 0x80000000L)
       ());
  reject "immediate width differs from the generated form"
    (Encoding.plan ~mode ~form:(form 276) ~prefixes:[ 0x48 ]
       ~immediate:(numeric 4 Encoding.Signed 1L)
       ());
  ignore
    (check_plan "original USE16 disp16 with address prefix" "6789830080"
       (Encoding.plan ~mode:Operand.Use16 ~form:(form 263) ~prefixes:[ 0x67 ]
          ~modrm:0x83
          ~displacement:(numeric 2 Encoding.Signed (-32768L))
          ()));
  reject "missing ENTER trailing zero"
    (Encoding.plan ~mode ~form:(form 799)
       ~immediate:(numeric 2 Encoding.Unsigned 256L)
       ());
  reject "extra trailing zero"
    (Encoding.plan ~mode ~form:(form 961) ~trailing_zero:true ())

let aggregate_emission () =
  let mode = Operand.Use64 in
  let mov =
    Encoding.plan ~mode ~form:(form 276) ~prefixes:[ 0x49 ]
      ~opcode_patch:(Encoding.Or_low3 3)
      ~immediate:(numeric 8 Encoding.Bit_pattern 0x0123456789abcdefL)
      ()
    |> checked
  in
  let enter =
    Encoding.plan ~mode ~form:(form 799) ~trailing_zero:true
      ~immediate:(numeric 2 Encoding.Unsigned 256L)
      ()
    |> checked
  in
  let return = Encoding.plan ~mode ~form:(form 961) () |> checked in
  let expected = "49bbefcdab8967452301c8000100c3" in
  let plans = [ mov; enter; return ] in
  let length = String.length expected / 2 in
  Alcotest.(check string)
    "shared aggregate at the exact bound" expected
    (Encoding.encode_all ~max_bytes:length plans |> checked |> hex);
  reject "aggregate one byte over limit"
    (Encoding.encode_all ~max_bytes:(length - 1) plans);
  reject "nonpositive aggregate limit" (Encoding.encode_all ~max_bytes:0 plans);
  Alcotest.(check string)
    "empty shared batch" ""
    (Encoding.encode_all ~max_bytes:1 [] |> checked)

let native_recipe_boundaries () =
  let slot = Native.stack_slot ~offset:4080 |> checked in
  let frame = Native.scalar_frame_slot ~offset:(-128) |> checked in
  let open Native in
  let cases =
    [
      (Push_rbp, "55");
      (Compiler_option_arguments System_v_x64, "4889d64c89c24c89df");
      (Load_stack (R10, slot), "4c8b9424f00f0000");
      (Store_frame_narrow (frame, Frame16, R8), "6644898580ffffff");
      (Lock_cmpxchg_indirect (R8, Atomic16, R10), "f066450fb19000000000");
      (Jump (-0x80000000L), "e900000080");
      (Compiler_option_arguments Windows_x64, "4c89d9");
      (Pop_rbp, "5d");
      (Ret, "c3");
    ]
  in
  List.iter
    (fun (instruction, expected) ->
      Alcotest.(check string)
        "native recipe retains exact bytes" expected
        (hex (encode instruction));
      Alcotest.(check int)
        "native recipe retains exact size"
        (String.length expected / 2)
        (size instruction))
    cases;
  let instructions = List.map fst cases in
  let expected = String.concat "" (List.map snd cases) in
  let length = String.length expected / 2 in
  Alcotest.(check string)
    "compound ABI plans retain aggregate order" expected
    (encode_all ~max_code_bytes:length instructions |> checked |> hex);
  reject "compound native plans exceed exact limit"
    (encode_all ~max_code_bytes:(length - 1) instructions);
  List.iter
    (fun invalid ->
      reject "native validation before aggregate emission"
        (encode_all ~max_code_bytes:256 [ Ret; invalid ]))
    [
      Jump 0x80000000L;
      Store_context (184, Rax);
      Store_context_imm (72, 0);
      Cmp_imm8 (Rax, 128);
      Lock_binary_indirect (Imul, Rax, Atomic64, Rcx);
    ]

let tests =
  [
    Alcotest.test_case "checked numeric field boundaries" `Quick
      numeric_boundaries;
    Alcotest.test_case "generated ownership and opcode patches" `Quick
      provenance_and_opcode_patches;
    Alcotest.test_case "required prefixes and source exceptions" `Quick
      prefixes_and_modes;
    Alcotest.test_case "checked ModR/M and numeric layouts" `Quick
      structural_fields;
    Alcotest.test_case "one aggregate byte emission" `Quick aggregate_emission;
    Alcotest.test_case "native recipes retain fixed layouts" `Quick
      native_recipe_boundaries;
  ]
