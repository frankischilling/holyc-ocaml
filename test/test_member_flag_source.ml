module Source = Member_flag_source

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
  [ "third_party/TempleOS"; "../third_party/TempleOS" ]
  |> List.map (fun root -> Filename.concat root path)
  |> List.find_opt Sys.file_exists
  |> function
  | None -> Alcotest.failf "pinned source is unavailable: %s" path
  | Some source -> read_file source |> normalize_line_endings

type sources = {
  kernel : string;
  lex_lib : string;
  prs_var : string;
  prs_stmt : string;
  prs_exp : string;
}

let pinned_sources () =
  {
    kernel = pinned "Kernel/KernelA.HH";
    lex_lib = pinned "Compiler/LexLib.HC";
    prs_var = pinned "Compiler/PrsVar.HC";
    prs_stmt = pinned "Compiler/PrsStmt.HC";
    prs_exp = pinned "Compiler/PrsExp.HC";
  }

let parse sources =
  Source.parse ~kernel_source:sources.kernel ~lex_lib_source:sources.lex_lib
    ~prs_var_source:sources.prs_var ~prs_stmt_source:sources.prs_stmt
    ~prs_exp_source:sources.prs_exp

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
  Alcotest.(check (list string))
    "source names"
    [
      "MLF_DFT_AVAILABLE";
      "MLF_LASTCLASS";
      "MLF_STR_DFT_AVAILABLE";
      "MLF_FUN";
      "MLF_DOT_DOT_DOT";
      "MLF_NO_UNUSED_WARN";
      "MLF_STATIC";
    ]
    (List.map
       (fun (entry : Source.flag_entry) -> entry.source_name)
       tables.flags);
  Alcotest.(check (list int64))
    "masks"
    [ 1L; 2L; 4L; 8L; 16L; 32L; 64L ]
    (List.map (fun (entry : Source.flag_entry) -> entry.mask) tables.flags);
  Alcotest.(check (list int))
    "definition lines"
    [ 778; 779; 780; 781; 782; 783; 784 ]
    (List.map
       (fun (entry : Source.flag_entry) -> entry.definition_line)
       tables.flags);
  Alcotest.(check (list int))
    "audited consumer counts" [ 3; 3; 5; 5; 3; 3; 2 ]
    (List.map
       (fun (entry : Source.flag_entry) -> List.length entry.consumers)
       tables.flags);
  Alcotest.(check int) "behavior count" 23 (List.length tables.behaviors)

let behavior_provenance () =
  let tables = parse_ok (pinned_sources ()) in
  let find id =
    List.find
      (fun (entry : Source.behavior_entry) -> String.equal entry.id id)
      tables.behaviors
  in
  let assignment = find "callback-assignment" in
  Alcotest.(check (pair string int))
    "callback assignment"
    ("Compiler/PrsVar.HC", 524)
    (assignment.source.path, assignment.source.line);
  let member = find "callback-member-expression" in
  Alcotest.(check (pair string int))
    "callback member use"
    ("Compiler/PrsExp.HC", 1009)
    (member.source.path, member.source.line)

let rejects_missing_definition () =
  let sources = pinned_sources () in
  let kernel =
    replace_once sources.kernel ~needle:"#define MLF_STATIC\t\t64\n"
      ~replacement:""
  in
  expect_error ~needle:"missing MLF_STATIC" { sources with kernel }

let rejects_duplicate_definition () =
  let sources = pinned_sources () in
  let line = "#define MLF_FUN\t\t\t8\n" in
  let kernel =
    replace_once sources.kernel ~needle:line ~replacement:(line ^ line)
  in
  expect_error ~needle:"requires MLF_DOT_DOT_DOT here, found MLF_FUN"
    { sources with kernel }

let rejects_reordered_definition () =
  let sources = pinned_sources () in
  let original = "#define MLF_DFT_AVAILABLE\t1\n#define MLF_LASTCLASS\t\t2" in
  let changed = "#define MLF_LASTCLASS\t\t2\n#define MLF_DFT_AVAILABLE\t1" in
  let kernel =
    replace_once sources.kernel ~needle:original ~replacement:changed
  in
  expect_error ~needle:"requires MLF_DFT_AVAILABLE here, found MLF_LASTCLASS"
    { sources with kernel }

let rejects_changed_value () =
  let sources = pinned_sources () in
  let kernel =
    replace_once sources.kernel ~needle:"#define MLF_FUN\t\t\t8"
      ~replacement:"#define MLF_FUN\t\t\t128"
  in
  expect_error ~needle:"MLF_FUN evaluates" { sources with kernel }

let rejects_extra_definition () =
  let sources = pinned_sources () in
  let line = "#define MLF_STATIC\t\t64\n" in
  let kernel =
    replace_once sources.kernel ~needle:line
      ~replacement:(line ^ "#define MLF_EXTRA\t\t128\n")
  in
  expect_error ~needle:"unexpected MLF_EXTRA" { sources with kernel }

let rejects_nonliteral_definition () =
  let sources = pinned_sources () in
  let kernel =
    replace_once sources.kernel ~needle:"#define MLF_STATIC\t\t64"
      ~replacement:"#define MLF_STATIC\t\t(1<<6)"
  in
  expect_error ~needle:"must retain a direct integer literal"
    { sources with kernel }

let rejects_consumer_drift () =
  let sources = pinned_sources () in
  let prs_var =
    replace_once sources.prs_var ~needle:"tmpm->flags|=MLF_FUN;"
      ~replacement:"tmpm->flags|=MLF_UNKNOWN;"
  in
  expect_error ~needle:"callback-assignment" { sources with prs_var }

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
    Alcotest.test_case "extra definition" `Quick rejects_extra_definition;
    Alcotest.test_case "literal form" `Quick rejects_nonliteral_definition;
    Alcotest.test_case "consumer behavior" `Quick rejects_consumer_drift;
    Alcotest.test_case "checksum and line endings" `Quick
      checksum_and_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
  ]
