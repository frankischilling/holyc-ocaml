module Primitive = Holyc_lib.Primitive_type
open Primitive

let expected_spellings =
  [
    "I0";
    "I8";
    "I16";
    "I32";
    "I64";
    "U0";
    "U8";
    "U16";
    "U32";
    "U64";
    "F64";
    "Bool";
  ]

let supported_spellings () =
  Alcotest.(check (list string))
    "supported primitive order" expected_spellings
    (List.map Primitive.to_string Primitive.all);
  List.iter
    (fun primitive ->
      let spelling = Primitive.to_string primitive in
      Alcotest.(check bool)
        (Printf.sprintf "%s lookup" spelling)
        true
        (match Primitive.of_spelling spelling with
        | Some found -> Primitive.equal found primitive
        | None -> false))
    Primitive.all

let unsupported_spellings () =
  let unsupported =
    [
      "F32";
      "UF32";
      "UF64";
      "I0i";
      "U0i";
      "I8i";
      "U8i";
      "I16i";
      "U16i";
      "I32i";
      "U32i";
      "I64i";
      "U64i";
      "F64i";
    ]
  in
  List.iter
    (fun spelling ->
      Alcotest.(check bool)
        (Printf.sprintf "%s is not semantic syntax" spelling)
        true
        (Option.is_none (Primitive.of_spelling spelling)))
    unsupported

let storage_spellings () =
  let expected =
    [
      ("I0i", I0);
      ("I8i", I8);
      ("I16i", I16);
      ("I32i", I32);
      ("I64i", I64);
      ("U0i", U0);
      ("U8i", U8);
      ("U16i", U16);
      ("U32i", U32);
      ("U64i", U64);
      ("F64i", F64);
    ]
  in
  List.iter
    (fun (spelling, primitive) ->
      Alcotest.(check bool)
        (spelling ^ " storage lookup") true
        (match Primitive.of_storage_spelling spelling with
        | Some found -> Primitive.equal found primitive
        | None -> false))
    expected;
  List.iter
    (fun spelling ->
      Alcotest.(check bool)
        (spelling ^ " is not an intrinsic storage spelling") true
        (Option.is_none (Primitive.of_storage_spelling spelling)))
    expected_spellings

let raw_ids_and_sizes () =
  let expected =
    [
      (Primitive.I0, "RT_I0", 2, 0, false);
      (Primitive.I8, "RT_I8", 4, 1, false);
      (Primitive.I16, "RT_I16", 6, 2, false);
      (Primitive.I32, "RT_I32", 8, 4, false);
      (Primitive.I64, "RT_I64", 10, 8, false);
      (Primitive.U0, "RT_U0", 3, 0, true);
      (Primitive.U8, "RT_U8", 5, 1, true);
      (Primitive.U16, "RT_U16", 7, 2, true);
      (Primitive.U32, "RT_U32", 9, 4, true);
      (Primitive.U64, "RT_U64", 11, 8, true);
      (Primitive.F64, "RT_F64", 14, 8, false);
      (Primitive.Bool, "RT_I8", 4, 1, false);
    ]
  in
  List.iter
    (fun (primitive, raw_name, raw_id, byte_size, raw_is_unsigned) ->
      let info = Primitive.info primitive in
      let spelling = Primitive.to_string primitive in
      Alcotest.(check string) (spelling ^ " raw name") raw_name info.raw_name;
      Alcotest.(check int) (spelling ^ " raw ID") raw_id info.raw_id;
      Alcotest.(check int) (spelling ^ " size") byte_size info.byte_size;
      Alcotest.(check bool)
        (spelling ^ " unsigned raw slot")
        raw_is_unsigned info.raw_is_unsigned)
    expected

let signedness_and_categories () =
  let signed = [ Primitive.I0; I8; I16; I32; I64 ] in
  let unsigned = [ Primitive.U0; U8; U16; U32; U64 ] in
  List.iter
    (fun primitive ->
      Alcotest.(check bool)
        (Primitive.to_string primitive ^ " is signed")
        true
        ((Primitive.info primitive).signedness = Primitive.Signed);
      Alcotest.(check bool)
        (Primitive.to_string primitive ^ " is integral")
        true
        ((Primitive.info primitive).category = Primitive.Integer))
    signed;
  List.iter
    (fun primitive ->
      Alcotest.(check bool)
        (Primitive.to_string primitive ^ " is unsigned")
        true
        ((Primitive.info primitive).signedness = Primitive.Unsigned);
      Alcotest.(check bool)
        (Primitive.to_string primitive ^ " is integral")
        true
        ((Primitive.info primitive).category = Primitive.Integer))
    unsigned;
  Alcotest.(check bool)
    "F64 category" true
    ((Primitive.info F64).category = Primitive.Floating);
  Alcotest.(check bool)
    "F64 signedness is not applicable" true
    ((Primitive.info F64).signedness = Primitive.Not_applicable);
  Alcotest.(check bool)
    "Bool category" true
    ((Primitive.info Bool).category = Primitive.Boolean);
  Alcotest.(check bool)
    "Bool signedness is not applicable" true
    ((Primitive.info Bool).signedness = Primitive.Not_applicable)

let zero_sized_types () =
  Alcotest.(check bool) "I0 has zero size" true (Primitive.is_zero_sized I0);
  Alcotest.(check bool) "U0 has zero size" true (Primitive.is_zero_sized U0);
  List.iter
    (fun primitive ->
      Alcotest.(check bool)
        (Primitive.to_string primitive ^ " has storage")
        false
        (Primitive.is_zero_sized primitive))
    [ I8; I16; I32; I64; U8; U16; U32; U64; F64; Bool ]

let bool_identity () =
  let boolean = Primitive.info Bool in
  let signed_byte = Primitive.info I8 in
  Alcotest.(check bool) "Bool is not I8" false (Primitive.equal Bool I8);
  Alcotest.(check int) "shared raw ID" signed_byte.raw_id boolean.raw_id;
  Alcotest.(check int)
    "shared byte size" signed_byte.byte_size boolean.byte_size;
  Alcotest.(check string) "Bool storage" "I8i" boolean.storage_spelling;
  Alcotest.(check int) "Bool declaration line" 8 boolean.declaration_source_line

let declaration_forms () =
  let union_types =
    [
      (Primitive.I16, "I16i", 73);
      (I32, "I32i", 87);
      (I64, "I64i", 105);
      (U16, "U16i", 67);
      (U32, "U32i", 79);
      (U64, "U64i", 95);
    ]
  in
  List.iter
    (fun (primitive, storage, declaration_line) ->
      let info = Primitive.info primitive in
      Alcotest.(check bool)
        (Primitive.to_string primitive ^ " uses a public union")
        true
        (info.declaration_form = Primitive.Public_union);
      Alcotest.(check string)
        (Primitive.to_string primitive ^ " storage")
        storage info.storage_spelling;
      Alcotest.(check int)
        (Primitive.to_string primitive ^ " declaration line")
        declaration_line info.declaration_source_line)
    union_types;
  List.iter
    (fun primitive ->
      Alcotest.(check bool)
        (Primitive.to_string primitive ^ " is an internal type")
        true
        ((Primitive.info primitive).declaration_form = Primitive.Internal_type))
    [ I0; I8; U0; U8; F64; Bool ]

let pointer_alias () =
  let pointer = Primitive.pointer_representation in
  let i64 = Primitive.info I64 in
  Alcotest.(check string) "pointer raw name" "RT_PTR" pointer.raw_name;
  Alcotest.(check string) "pointer target" "RT_I64" pointer.target_raw_name;
  Alcotest.(check int) "pointer ID" 10 pointer.raw_id;
  Alcotest.(check int) "pointer source line" 1573 pointer.source_line;
  Alcotest.(check int)
    "pointer and I64 share the raw ID" i64.raw_id pointer.raw_id

let provenance () =
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    Primitive.reference_commit;
  Alcotest.(check string)
    "raw source" "Kernel/KernelA.HH" Primitive.raw_source_path;
  Alcotest.(check string)
    "raw source checksum"
    "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41"
    Primitive.raw_source_sha256;
  Alcotest.(check string)
    "internal source" "Compiler/CInit.HC" Primitive.internal_type_source_path;
  Alcotest.(check string)
    "internal source checksum"
    "f187d11043dcceb8791409a3e6809ea26e9c3b4f182fe2cbe5c5e644e6938b19"
    Primitive.internal_type_source_sha256

let deterministic_order () =
  let reverse = List.rev Primitive.all in
  let sorted = List.sort Primitive.compare reverse in
  Alcotest.(check (list string))
    "stable compare order" expected_spellings
    (List.map Primitive.to_string sorted)

let tests =
  [
    Alcotest.test_case "supported spellings" `Quick supported_spellings;
    Alcotest.test_case "unsupported spellings" `Quick unsupported_spellings;
    Alcotest.test_case "intrinsic storage spellings" `Quick storage_spellings;
    Alcotest.test_case "raw IDs and sizes" `Quick raw_ids_and_sizes;
    Alcotest.test_case "signedness and categories" `Quick
      signedness_and_categories;
    Alcotest.test_case "zero-sized types" `Quick zero_sized_types;
    Alcotest.test_case "Bool identity" `Quick bool_identity;
    Alcotest.test_case "declaration forms" `Quick declaration_forms;
    Alcotest.test_case "pointer raw alias" `Quick pointer_alias;
    Alcotest.test_case "source provenance" `Quick provenance;
    Alcotest.test_case "deterministic order" `Quick deterministic_order;
  ]
