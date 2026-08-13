module Source = Global_record_flag_source

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let normalize_line_endings text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec copy offset =
    if offset < length then
      if
        Char.equal text.[offset] '\r'
        && offset + 1 < length
        && Char.equal text.[offset + 1] '\n'
      then (
        Buffer.add_char buffer '\n';
        copy (offset + 2))
      else (
        Buffer.add_char buffer text.[offset];
        copy (offset + 1))
  in
  copy 0;
  Buffer.contents buffer

let pinned path =
  read_file ("../third_party/TempleOS/" ^ path) |> normalize_line_endings

type sources = {
  kernel : string;
  prs_stmt : string;
  prs_exp : string;
  khash : string;
  chash : string;
  asm_resolve : string;
  scoping : string;
}

let pinned_sources () =
  {
    kernel = pinned "Kernel/KernelA.HH";
    prs_stmt = pinned "Compiler/PrsStmt.HC";
    prs_exp = pinned "Compiler/PrsExp.HC";
    khash = pinned "Kernel/KHashB.HC";
    chash = pinned "Compiler/CHash.HC";
    asm_resolve = pinned "Compiler/AsmResolve.HC";
    scoping = pinned "Doc/ScopingLinkage.DD";
  }

let parse sources =
  Source.parse ~kernel_source:sources.kernel ~prs_stmt_source:sources.prs_stmt
    ~prs_exp_source:sources.prs_exp ~khash_source:sources.khash
    ~chash_source:sources.chash ~asm_resolve_source:sources.asm_resolve
    ~scoping_source:sources.scoping

let parse_ok sources =
  match parse sources with
  | Ok tables -> tables
  | Error problem -> Alcotest.fail (Source.error_to_string problem)

let contains ~needle text =
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > String.length text then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let expect_error ~needle sources =
  match parse sources with
  | Ok _ -> Alcotest.fail "source parser unexpectedly accepted changed input"
  | Error problem ->
      let message = Source.error_to_string problem in
      Alcotest.(check bool) message true (contains ~needle message)

let replace_once text ~needle ~replacement =
  let needle_length = String.length needle in
  let rec find offset =
    if offset + needle_length > String.length text then
      Alcotest.failf "test fixture does not contain %S" needle
    else if String.sub text offset needle_length = needle then offset
    else find (offset + 1)
  in
  let offset = find 0 in
  String.sub text 0 offset ^ replacement
  ^ String.sub text (offset + needle_length)
      (String.length text - offset - needle_length)

let complete_registry () =
  let tables = parse_ok (pinned_sources ()) in
  Alcotest.(check (pair int int64))
    "global hash type" (3, 0x8L)
    (tables.global_type.type_index, tables.global_type.type_mask);
  Alcotest.(check (list string))
    "hash flags"
    [
      "HTF_PRIVATE";
      "HTF_PUBLIC";
      "HTF_EXPORT";
      "HTF_IMPORT";
      "HTF_IMM";
      "HTF_GOTO_LABEL";
      "HTF_RESOLVE";
      "HTF_UNRESOLVED";
      "HTF_LOCAL";
    ]
    (List.map
       (fun (entry : Source.flag_entry) -> entry.mask_name)
       tables.hash_flags);
  Alcotest.(check (list int64))
    "global flags"
    [ 1L; 2L; 4L; 8L; 16L; 32L ]
    (List.map
       (fun (entry : Source.flag_entry) -> entry.mask)
       tables.global_flags)

let behavior_provenance () =
  let tables = parse_ok (pinned_sources ()) in
  Alcotest.(check int) "behavior count" 25 (List.length tables.behaviors);
  let find id =
    List.find
      (fun (entry : Source.behavior_entry) -> String.equal entry.id id)
      tables.behaviors
  in
  let alias = find "alias-transfer" in
  Alcotest.(check (pair string int))
    "alias transfer"
    ("Compiler/PrsStmt.HC", 443)
    (alias.source.path, alias.source.line);
  let map = find "map-omits-import-private" in
  Alcotest.(check (pair string int))
    "map filtering" ("Compiler/CHash.HC", 104)
    (map.source.path, map.source.line)

let rejects_missing_definition () =
  let sources = pinned_sources () in
  let kernel =
    replace_once sources.kernel ~needle:"#define GVF_ARRAY\t32\n"
      ~replacement:""
  in
  expect_error ~needle:"missing GVF_ARRAY" { sources with kernel }

let rejects_duplicate_definition () =
  let sources = pinned_sources () in
  let line = "#define GVF_FUN\t\t1\n" in
  let kernel =
    replace_once sources.kernel ~needle:line ~replacement:(line ^ line)
  in
  expect_error ~needle:"requires GVF_IMPORT here" { sources with kernel }

let rejects_reordered_definition () =
  let sources = pinned_sources () in
  let original = "#define GVF_FUN\t\t1\n#define GVF_IMPORT\t2" in
  let changed = "#define GVF_IMPORT\t2\n#define GVF_FUN\t\t1" in
  let kernel =
    replace_once sources.kernel ~needle:original ~replacement:changed
  in
  expect_error ~needle:"requires GVF_FUN here" { sources with kernel }

let rejects_changed_value () =
  let sources = pinned_sources () in
  let kernel =
    replace_once sources.kernel ~needle:"#define GVF_EXTERN\t4"
      ~replacement:"#define GVF_EXTERN\t64"
  in
  expect_error ~needle:"GVF_EXTERN evaluates" { sources with kernel }

let rejects_hash_bit_mask_drift () =
  let sources = pinned_sources () in
  let kernel =
    replace_once sources.kernel ~needle:"#define HTF_PUBLIC\t\t0x01000000"
      ~replacement:"#define HTF_PUBLIC\t\t0x00010000"
  in
  expect_error ~needle:"HTF_PUBLIC evaluates" { sources with kernel }

let rejects_consumer_drift () =
  let sources = pinned_sources () in
  let prs_stmt =
    replace_once sources.prs_stmt ~needle:"tmpg->flags|=GVF_FUN;"
      ~replacement:"tmpg->flags|=GVF_ARRAY;"
  in
  expect_error ~needle:"function-pointer-flag" { sources with prs_stmt }

let checksum_and_line_endings () =
  let source = (pinned_sources ()).kernel in
  let expected =
    "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41"
  in
  Alcotest.(check bool)
    "pinned checksum" true
    (Result.is_ok (Source.verify_sha256 ~expected source));
  let crlf = String.split_on_char '\n' source |> String.concat "\r\n" in
  Alcotest.(check bool)
    "checkout line endings" true
    (Result.is_ok (Source.verify_sha256 ~expected crlf));
  Alcotest.(check bool)
    "changed input" true
    (Result.is_error (Source.verify_sha256 ~expected (source ^ " ")))

let deterministic_parse () =
  let sources = pinned_sources () in
  Alcotest.(check bool) "same tables" true (parse_ok sources = parse_ok sources)

let tests =
  [
    Alcotest.test_case "complete registry" `Quick complete_registry;
    Alcotest.test_case "behavior provenance" `Quick behavior_provenance;
    Alcotest.test_case "missing definition" `Quick rejects_missing_definition;
    Alcotest.test_case "duplicate definition" `Quick
      rejects_duplicate_definition;
    Alcotest.test_case "definition order" `Quick rejects_reordered_definition;
    Alcotest.test_case "definition value" `Quick rejects_changed_value;
    Alcotest.test_case "bit and mask relation" `Quick
      rejects_hash_bit_mask_drift;
    Alcotest.test_case "consumer behavior" `Quick rejects_consumer_drift;
    Alcotest.test_case "checksum and line endings" `Quick
      checksum_and_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
  ]
