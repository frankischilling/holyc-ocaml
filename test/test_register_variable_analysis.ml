open Holyc_lib
open Yojson.Safe.Util
module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Global = Holyc_lib__Ir.Global_immediate_update_analysis
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence
module Symbol = Semantic_symbol

let read name =
  let channel = open_in_bin ("oracle/register-variable-analysis/" ^ name) in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture stem =
  let metadata = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "pinned reference" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (metadata |> member "reference_commit" |> to_string);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "retained reference evidence"
        (metadata |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [ (stem ^ ".hc", "payload_sha256"); (stem ^ ".capture", "capture_sha256") ];
  (match metadata |> member "boot_payload_sha256" with
  | `String expected ->
      let source = read (stem ^ ".hc") in
      let size = metadata |> member "boot_payload_bytes" |> to_int in
      Alcotest.(check bool)
        "boot payload padding is nonnegative" true
        (size >= String.length source);
      let padded = source ^ String.make (size - String.length source) ' ' in
      Alcotest.(check string)
        "exact padded boot payload" expected
        (Digestif.SHA256.digest_string padded |> Digestif.SHA256.to_hex)
  | `Null -> ()
  | _ -> failwith "invalid boot payload hash");
  let fields =
    metadata |> member "fields" |> to_assoc
    |> List.map (fun (k, v) -> (k, to_string v))
  in
  let one = List.map (fun (k, v) -> k ^ "=" ^ v) fields @ [ "POINTER_DONE" ] in
  let actual =
    read (stem ^ ".capture")
    |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  Alcotest.(check (list string))
    "two complete original observations" (one @ one) actual;
  fields

let compile ?name mode contents =
  let session = Session.create () in
  let source = Session.add_source session ~path:"register-proof.hc" ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  let program =
    match compile_integer_program session ~config ~source with
    | Ok program -> program
    | Error diagnostics ->
        failwith
          (contents ^ ": "
          ^ String.concat "; "
              (List.map
                 (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                 diagnostics))
  in
  let functions = integer_program_functions program.value in
  match name with
  | None -> List.hd functions
  | Some name ->
      List.find
        (fun (fn : Ir_integer_interpreter.function_definition) ->
          Symbol.name (Body.symbol fn.body) = name)
        functions

let location frame name =
  Frame.function_locations frame
  |> List.find (fun item -> Symbol.name (Frame.location_symbol item) = name)

let check_incomplete stem =
  let incomplete = Yojson.Safe.from_string (read (stem ^ ".json")) in
  Alcotest.(check string)
    "incomplete high literal reference"
    "incomplete: invalid opcode observed in JIT before compiled controls"
    (incomplete |> member "status" |> to_string);
  List.iter
    (fun (name, key) ->
      Alcotest.(check string)
        "incomplete probe evidence hash"
        (incomplete |> member key |> to_string)
        (Digestif.SHA256.digest_string (read name) |> Digestif.SHA256.to_hex))
    [
      (stem ^ ".hc", "payload_sha256");
      (stem ^ ".capture", "capture_sha256");
      (stem ^ ".png", "screenshot_sha256");
    ];
  let incomplete_source = read (stem ^ ".hc") in
  let incomplete_size = incomplete |> member "boot_payload_bytes" |> to_int in
  Alcotest.(check bool)
    "incomplete boot payload padding" true
    (incomplete_size >= String.length incomplete_source);
  let incomplete_boot =
    incomplete_source
    ^ String.make (incomplete_size - String.length incomplete_source) ' '
  in
  Alcotest.(check string)
    "incomplete boot payload hash"
    (incomplete |> member "boot_payload_sha256" |> to_string)
    (Digestif.SHA256.digest_string incomplete_boot |> Digestif.SHA256.to_hex);
  Alcotest.(check string)
    "incomplete JIT output" "J_S=255\nJ_U=256\n"
    (read (stem ^ ".capture"))

let source_controls () =
  let fields = fixture "selection" in
  List.iter
    (fun prefix ->
      Alcotest.(check string)
        "original register parameter" "254"
        (List.assoc (prefix ^ "REG") fields);
      Alcotest.(check string)
        "original disabled register" "-2"
        (List.assoc (prefix ^ "NOREG") fields);
      Alcotest.(check string)
        "original addressed parameter" "-2"
        (List.assoc (prefix ^ "ADDR") fields);
      Alcotest.(check string)
        "original register local" "254"
        (List.assoc (prefix ^ "LOCAL") fields);
      Alcotest.(check string)
        "original disabled local" "-2"
        (List.assoc (prefix ^ "NOLOCAL") fields))
    [ "JIT_"; "AOT_" ];
  List.iter
    (fun kind ->
      List.iter
        (fun reg ->
          let fields = fixture (kind ^ "-" ^ reg) in
          List.iter
            (fun prefix ->
              List.iter
                (fun (name, expected) ->
                  Alcotest.(check string)
                    "original explicit register value" expected
                    (List.assoc (prefix ^ name) fields))
                [ ("S", "254"); ("U", "510"); ("SN", "-2"); ("UN", "254") ])
            [ "J_"; "A_" ])
        [ "r12"; "r13"; "r14"; "r15" ])
    [ "explicit"; "explicit-local" ];
  List.iter
    (fun (stem, bits) ->
      let fields = fixture stem in
      let open Int64 in
      let n = shift_left 1L bits in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original requested register width" (to_string expected)
                (List.assoc (prefix ^ name) fields))
            [
              ("S", sub n 2L);
              ("U", sub (mul n 2L) 2L);
              ("SN", -2L);
              ("UN", sub n 2L);
            ])
        [ "J_"; "A_" ])
    [
      ("allocatable-explicit", 8);
      ("allocatable-explicit-local", 8);
      ("explicit-count-r12", 8);
      ("explicit-r12-16", 16);
      ("explicit-r12-32", 32);
      ("explicit-local-r12-16", 16);
      ("explicit-local-r12-32", 32);
    ];
  let explicit_exe = fixture "explicit-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original requested register in compile-time execution" expected
        (List.assoc name explicit_exe))
    [ ("PARAM", "254"); ("LOCAL", "510") ];
  let explicit_count_exe = fixture "explicit-count-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original operand register in compile-time execution" expected
        (List.assoc name explicit_count_exe))
    [ ("PARAM", "254"); ("LOCAL", "510") ];
  List.iter
    (fun stem ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original multi-parameter selection" expected
                (List.assoc (prefix ^ name) fields))
            [ ("S", "254"); ("U", "510"); ("SN", "-2"); ("UN", "254") ])
        [ "J_"; "A_" ])
    [
      "many-parameters-3";
      "many-parameters-4";
      "many-parameters-5";
      "many-parameters";
      "many-parameters-target";
      "many-parameters-count";
    ];
  List.iter
    (fun (op, values) ->
      let fields = fixture ("many-parameters-" ^ op) in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original multi-parameter arithmetic" expected
                (List.assoc (prefix ^ name) fields))
            values)
        [ "J_"; "A_" ])
    [
      ("add", [ ("S", "128"); ("U", "256"); ("SN", "-128"); ("UN", "0") ]);
      ("sub", [ ("S", "-129"); ("U", "-1"); ("SN", "127"); ("UN", "255") ]);
      ("mul", [ ("S", "254"); ("U", "510"); ("SN", "-2"); ("UN", "254") ]);
    ];
  let many_exe = fixture "many-parameters-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original multi-parameter compile-time execution" expected
        (List.assoc name many_exe))
    [ ("PARAM", "254"); ("LOCAL", "510") ];
  List.iter
    (fun (stem, values) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original two-update selection" expected
                (List.assoc (prefix ^ name) fields))
            values)
        [ "J_"; "A_" ])
    [
      ( "two-updates",
        [ ("S", "256"); ("U", "512"); ("SN", "-256"); ("UN", "0") ] );
      ( "two-updates-mixed",
        [ ("S", "256"); ("U", "512"); ("SN", "0"); ("UN", "256") ] );
      ( "two-updates-explicit",
        [ ("S", "256"); ("U", "512"); ("SN", "-256"); ("UN", "0") ] );
      ( "two-updates-narrow-count",
        [ ("S", "256"); ("U", "512"); ("SN", "-256"); ("UN", "0") ] );
      ( "two-updates-negative-count",
        [ ("S", "-258"); ("U", "-2"); ("SN", "254"); ("UN", "510") ] );
      ( "two-updates-sub",
        [ ("S", "-258"); ("U", "-2"); ("SN", "254"); ("UN", "510") ] );
      ( "two-updates-mul",
        [ ("S", "508"); ("U", "1020"); ("SN", "-4"); ("UN", "508") ] );
      ( "two-updates-mixedops",
        [ ("S", "254"); ("U", "510"); ("SN", "-2"); ("UN", "254") ] );
      ( "two-updates-two-counts",
        [ ("S", "257"); ("U", "513"); ("SN", "-255"); ("UN", "1") ] );
      ( "two-updates-shl",
        [ ("S", "508"); ("U", "1020"); ("SN", "-4"); ("UN", "508") ] );
      ( "two-updates-shr",
        [ ("S", "-128"); ("U", "254"); ("SN", "-128"); ("UN", "254") ] );
      ( "two-updates-or",
        [ ("S", "766"); ("U", "1022"); ("SN", "254"); ("UN", "510") ] );
      ( "two-updates-xor",
        [ ("S", "766"); ("U", "1022"); ("SN", "254"); ("UN", "510") ] );
      ( "two-updates-and",
        [ ("S", "510"); ("U", "510"); ("SN", "-2"); ("UN", "510") ] );
      ( "two-updates-div",
        [ ("S", "256"); ("U", "254"); ("SN", "-256"); ("UN", "254") ] );
    ];
  List.iter
    (fun bits ->
      let fields = fixture ("two-updates-" ^ string_of_int bits) in
      let open Int64 in
      let n = shift_left 1L bits in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original two-update width" (to_string expected)
                (List.assoc (prefix ^ name) fields))
            [ ("S", n); ("U", mul n 2L); ("SN", neg n); ("UN", 0L) ])
        [ "J_"; "A_" ])
    [ 16; 32 ];
  let pair_six = fixture "two-updates-six" in
  List.iter
    (fun name ->
      Alcotest.(check string)
        "original six-parameter two-update result" "256"
        (List.assoc name pair_six))
    [ "J_S"; "A_S" ];
  let pair_single = fixture "two-updates-single-return" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original pair single return" expected
        (List.assoc name pair_single))
    [
      ("J_S", "128");
      ("J_U", "43");
      ("J_SN", "-128");
      ("J_UN", "43");
      ("ERR", "0");
      ("A_S", "128");
      ("A_U", "43");
      ("A_SN", "-128");
      ("A_UN", "43");
    ];
  let pair_second = fixture "two-updates-second-return" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original pair second return" expected
        (List.assoc name pair_second))
    [
      ("J_S", "43");
      ("J_U", "128");
      ("J_SN", "43");
      ("J_UN", "-128");
      ("ERR", "0");
      ("A_S", "43");
      ("A_U", "128");
      ("A_SN", "43");
      ("A_UN", "-128");
    ];
  let pair_exe = fixture "two-updates-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original two-update compile-time execution" expected
        (List.assoc name pair_exe))
    [ ("PARAM", "256"); ("LOCAL", "510") ];
  let pair_distinct_exe = fixture "two-updates-two-counts-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original distinct operand compile-time execution" expected
        (List.assoc name pair_distinct_exe))
    [ ("PARAM", "257"); ("LOCAL", "510") ];
  List.iter
    (fun bits ->
      let fields = fixture ("two-updates-mixed-types-" ^ string_of_int bits) in
      let open Int64 in
      let n = shift_left 1L bits in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original mixed-signedness updates" (to_string expected)
                (List.assoc (prefix ^ name) fields))
            [
              ("S", add n (div n 2L));
              ("U", add n (div n 2L));
              ("SN", neg (div n 2L));
              ("UN", neg (div n 2L));
            ])
        [ "J_"; "A_" ])
    [ 8; 16; 32 ];
  let sequence = fixture "many-updates-sequence" in
  List.iter
    (fun count ->
      List.iter
        (fun (kind, value) ->
          List.iter
            (fun prefix ->
              Alcotest.(check string)
                "original multi-update sequence" (string_of_int value)
                (List.assoc
                   (Printf.sprintf "%s_%d%s" prefix count kind)
                   sequence))
            [ "J"; "A" ])
        [
          ("SR", count * 128);
          ("SN", count * -128);
          ("UR", count * 256);
          ("UN", 0);
        ])
    [ 3; 4; 5 ];
  let many_ops_exe = fixture "many-updates-operators-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original compile-time many-update operator" expected
        (List.assoc name many_ops_exe))
    [ ("PARAM", "384"); ("FIVE", "2550"); ("ERR", "0") ];
  let full64_edges = fixture "many-updates-full64-edges" in
  List.iter
    (fun (name, wide, narrow) ->
      List.iter
        (fun prefix ->
          List.iter
            (fun (suffix, expected) ->
              Alcotest.(check string)
                "original full-width boundary updates" expected
                (List.assoc (prefix ^ "_" ^ name ^ suffix) full64_edges))
            [ ("R", wide); ("N", narrow) ])
        [ "J"; "A" ])
    [
      ("A", "-9223372036854775299", "9223372036854775805");
      ("B", "381", "125");
      ("C", "317", "61");
      ("D", "-4611686018427387394", "-4611686018427387906");
      ("E", "21474902133", "4295032693");
    ];
  let fullwidth_explicit = fixture "many-updates-fullwidth-explicit" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original full-width explicit registers" expected
        (List.assoc name fullwidth_explicit))
    [
      ("J_A", "65832");
      ("J_E", "21474902220");
      ("ERR", "0");
      ("A_A", "65832");
      ("A_E", "21474902220");
    ];
  let fullwidth_ops_exe = fixture "many-updates-fullwidth-ops-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original compile-time full-width updates" expected
        (List.assoc name fullwidth_ops_exe))
    [ ("PARAM", "384"); ("MIXED", "21474902176"); ("ERR", "0") ];
  let right_tree = fixture "many-updates-fullwidth-right-tree" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original parenthesized update result" expected
        (List.assoc name right_tree))
    [
      ("J_AR", "131368");
      ("J_AN", "65576");
      ("J_BR", "131283");
      ("J_BN", "65491");
      ("J_CR", "131283");
      ("J_CN", "65491");
      ("J_DR", "4295033041");
      ("J_DN", "65489");
      ("J_ER", "2147615014");
      ("J_EN", "2147549222");
      ("J_FR", "21474902176");
      ("J_FN", "4295032736");
      ("J_GR", "8590196896");
      ("J_GN", "4295032992");
      ("ERR", "0");
      ("A_AR", "131368");
      ("A_AN", "65576");
      ("A_BR", "131283");
      ("A_BN", "65491");
      ("A_CR", "131283");
      ("A_CN", "65491");
      ("A_DR", "4295033041");
      ("A_DN", "65489");
      ("A_ER", "2147615014");
      ("A_EN", "2147549222");
      ("A_FR", "21474902176");
      ("A_FN", "4295032736");
      ("A_GR", "8590196896");
      ("A_GN", "4295032992");
    ];
  let reverse_tree = fixture "many-updates-fullwidth-reverse-tree" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original reversed update result" expected
        (List.assoc name reverse_tree))
    [
      ("J_AR", "131368");
      ("J_AN", "65576");
      ("J_BR", "131283");
      ("J_BN", "65491");
      ("J_CR", "131283");
      ("J_CN", "65491");
      ("J_DR", "4295033041");
      ("J_DN", "65489");
      ("J_ER", "2147615014");
      ("J_EN", "2147549222");
      ("J_FR", "21474902176");
      ("J_FN", "4295032736");
      ("J_GR", "8590196896");
      ("J_GN", "4295032992");
      ("ERR", "0");
      ("A_AR", "131368");
      ("A_AN", "65576");
      ("A_BR", "131283");
      ("A_BN", "65491");
      ("A_CR", "131283");
      ("A_CN", "65491");
      ("A_DR", "4295033041");
      ("A_DN", "65489");
      ("A_ER", "2147615014");
      ("A_EN", "2147549222");
      ("A_FR", "21474902176");
      ("A_FN", "4295032736");
      ("A_GR", "8590196896");
      ("A_GN", "4295032992");
    ];
  let repeat_tree = fixture "many-updates-fullwidth-repeat-tree" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original repeated update result" expected
        (List.assoc name repeat_tree))
    [
      ("J_AR", "131158");
      ("J_AN", "65622");
      ("J_BR", "131328");
      ("J_BN", "65280");
      ("J_CR", "342");
      ("J_CN", "-170");
      ("J_DR", "65876");
      ("J_DN", "65364");
      ("J_ER", "2147483988");
      ("J_EN", "2147483732");
      ("J_FR", "4295033125");
      ("J_FN", "65317");
      ("J_GR", "8589934800");
      ("J_GN", "4294967504");
      ("ERR", "0");
      ("A_AR", "131158");
      ("A_AN", "65622");
      ("A_BR", "131328");
      ("A_BN", "65280");
      ("A_CR", "342");
      ("A_CN", "-170");
      ("A_DR", "65876");
      ("A_DN", "65364");
      ("A_ER", "2147483988");
      ("A_EN", "2147483732");
      ("A_FR", "4295033125");
      ("A_FN", "65317");
      ("A_GR", "8589934800");
      ("A_GN", "4294967504");
    ];
  let short_tree = fixture "many-updates-fullwidth-short-tree" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original shorter update result" expected
        (List.assoc name short_tree))
    [
      ("J_AR", "131114");
      ("J_AN", "65578");
      ("J_BR", "131199");
      ("J_BN", "65407");
      ("J_CR", "213");
      ("J_CN", "-43");
      ("J_DR", "65747");
      ("J_DN", "65491");
      ("J_ER", "2147483944");
      ("J_EN", "2147483688");
      ("J_FR", "4295032996");
      ("J_FN", "65444");
      ("J_GR", "8589934756");
      ("J_GN", "4294967460");
      ("ERR", "0");
      ("A_AR", "131114");
      ("A_AN", "65578");
      ("A_BR", "131199");
      ("A_BN", "65407");
      ("A_CR", "213");
      ("A_CN", "-43");
      ("A_DR", "65747");
      ("A_DN", "65491");
      ("A_ER", "2147483944");
      ("A_EN", "2147483688");
      ("A_FR", "4295032996");
      ("A_FN", "65444");
      ("A_GR", "8589934756");
      ("A_GN", "4294967460");
    ];
  let shorter_tree = fixture "many-updates-fullwidth-shorter-tree" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original two-fewer update result" expected
        (List.assoc name shorter_tree))
    [
      ("J_AR", "131114");
      ("J_AN", "65578");
      ("J_BR", "131199");
      ("J_BN", "65407");
      ("J_CR", "213");
      ("J_CN", "-43");
      ("J_DR", "213");
      ("J_DN", "-43");
      ("J_ER", "298");
      ("J_EN", "42");
      ("J_FR", "4294967463");
      ("J_FN", "-89");
      ("J_GR", "4294967463");
      ("J_GN", "167");
      ("ERR", "0");
      ("A_AR", "131114");
      ("A_AN", "65578");
      ("A_BR", "131199");
      ("A_BN", "65407");
      ("A_CR", "213");
      ("A_CN", "-43");
      ("A_DR", "213");
      ("A_DN", "-43");
      ("A_ER", "298");
      ("A_EN", "42");
      ("A_FR", "4294967463");
      ("A_FN", "-89");
      ("A_GR", "4294967463");
      ("A_GN", "167");
    ];
  let long_tree = fixture "many-updates-fullwidth-long-tree" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original longer update result" expected
        (List.assoc name long_tree))
    [
      ("J_AR", "131412");
      ("J_AN", "65620");
      ("J_BR", "131412");
      ("J_BN", "65364");
      ("J_CR", "131412");
      ("J_CN", "65364");
      ("J_DR", "4295033170");
      ("J_DN", "65362");
      ("J_ER", "2147615058");
      ("J_EN", "2147549266");
      ("J_FR", "21474902305");
      ("J_FN", "4295032609");
      ("J_GR", "8590196940");
      ("J_GN", "4295033036");
      ("ERR", "0");
      ("A_AR", "131412");
      ("A_AN", "65620");
      ("A_BR", "131412");
      ("A_BN", "65364");
      ("A_CR", "131412");
      ("A_CN", "65364");
      ("A_DR", "4295033170");
      ("A_DN", "65362");
      ("A_ER", "2147615058");
      ("A_EN", "2147549266");
      ("A_FR", "21474902305");
      ("A_FN", "4295032609");
      ("A_GR", "8590196940");
      ("A_GN", "4295033036");
    ];
  let extreme_tree = fixture "many-updates-fullwidth-extreme-tree" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original two-to-six-term update result" expected
        (List.assoc name extreme_tree))
    [
      ("J_AR", "131666");
      ("J_AN", "65618");
      ("J_BR", "131496");
      ("J_BN", "65448");
      ("J_CR", "262482");
      ("J_CN", "130898");
      ("J_DR", "4295098704");
      ("J_DN", "130896");
      ("J_ER", "4295098704");
      ("J_EN", "4295032912");
      ("J_FR", "169");
      ("J_FN", "-87");
      ("J_GR", "169");
      ("J_GN", "169");
      ("ERR", "0");
      ("A_AR", "131666");
      ("A_AN", "65618");
      ("A_BR", "131496");
      ("A_BN", "65448");
      ("A_CR", "262482");
      ("A_CN", "130898");
      ("A_DR", "4295098704");
      ("A_DN", "130896");
      ("A_ER", "4295098704");
      ("A_EN", "4295032912");
      ("A_FR", "169");
      ("A_FN", "-87");
      ("A_GR", "169");
      ("A_GN", "169");
    ];
  let single_return = fixture "many-updates-fullwidth-single-return" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original single-variable update result" expected
        (List.assoc name single_return))
    [
      ("J_AR", "254");
      ("J_AN", "-2");
      ("J_BR", "84");
      ("J_BN", "84");
      ("J_CR", "131070");
      ("J_CN", "65534");
      ("J_DR", "4294967294");
      ("J_DN", "-2");
      ("J_ER", "131070");
      ("J_EN", "65534");
      ("J_FR", "17179869180");
      ("J_FN", "4294967292");
      ("J_GR", "262140");
      ("J_GN", "65532");
      ("ERR", "0");
      ("A_AR", "254");
      ("A_AN", "-2");
      ("A_BR", "84");
      ("A_BN", "84");
      ("A_CR", "131070");
      ("A_CN", "65534");
      ("A_DR", "4294967294");
      ("A_DN", "-2");
      ("A_ER", "131070");
      ("A_EN", "65534");
      ("A_FR", "17179869180");
      ("A_FN", "4294967292");
      ("A_GR", "262140");
      ("A_GN", "65532");
    ];
  let fullwidth_ops = fixture "many-updates-fullwidth-ops" in
  List.iter
    (fun (name, wide, narrow) ->
      List.iter
        (fun prefix ->
          List.iter
            (fun (suffix, expected) ->
              Alcotest.(check string)
                "original full-width and narrow updates" expected
                (List.assoc (prefix ^ "_" ^ name ^ suffix) fullwidth_ops))
            [ ("R", wide); ("N", narrow) ])
        [ "J"; "A" ])
    [
      ("A", "131368", "65576");
      ("B", "131283", "65491");
      ("C", "131283", "65491");
      ("D", "4295033041", "65489");
      ("E", "2147615014", "2147549222");
      ("F", "21474902176", "4295032736");
      ("G", "8590196896", "4295032992");
    ];
  let mixed_types_explicit = fixture "many-updates-mixed-types-explicit" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original mixed types explicit registers" expected
        (List.assoc name mixed_types_explicit))
    [
      ("J_A", "131197");
      ("J_E", "21474902390");
      ("ERR", "0");
      ("A_A", "131197");
      ("A_E", "21474902390");
    ];
  let mixed_types_ops_exe = fixture "many-updates-mixed-types-ops-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original compile-time mixed types and operators" expected
        (List.assoc name mixed_types_ops_exe))
    [ ("PARAM", "384"); ("MIXED", "21474902389"); ("ERR", "0") ];
  let mixed_types_ops = fixture "many-updates-mixed-types-ops" in
  List.iter
    (fun (name, wide, narrow) ->
      List.iter
        (fun prefix ->
          List.iter
            (fun (suffix, expected) ->
              Alcotest.(check string)
                "original mixed types and operators" expected
                (List.assoc (prefix ^ "_" ^ name ^ suffix) mixed_types_ops))
            [ ("R", wide); ("N", narrow) ])
        [ "J"; "A" ])
    [
      ("A", "4295098493", "65405");
      ("B", "2147614973", "-2147418115");
      ("C", "8589967867", "4295000059");
      ("D", "4295033467", "4294967419");
      ("E", "21474902389", "4295032949");
      ("F", "8590197109", "4295032949");
    ];
  let mixed_types_exe = fixture "many-updates-mixed-types-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original compile-time mixed update types" expected
        (List.assoc name mixed_types_exe))
    [ ("PARAM", "384"); ("MIXED", "6442516608"); ("ERR", "0") ];
  let mixed_types = fixture "many-updates-mixed-types" in
  List.iter
    (fun (name, wide, narrow) ->
      List.iter
        (fun prefix ->
          List.iter
            (fun (suffix, expected) ->
              Alcotest.(check string)
                "original mixed narrow update types" expected
                (List.assoc (prefix ^ "_" ^ name ^ suffix) mixed_types))
            [ ("R", wide); ("N", narrow) ])
        [ "J"; "A" ])
    [
      ("A", "2147549312", "-2147483776");
      ("B", "2147549312", "-2147483776");
      ("C", "4295000320", "-32768");
      ("D", "4295000320", "-32768");
      ("E", "6442516608", "-2147483776");
      ("F", "6442516608", "-2147483776");
    ];
  let independent_explicit = fixture "many-updates-independent-explicit" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original independent explicit operands" expected
        (List.assoc name independent_explicit))
    [ ("J_U8N3R", "1021"); ("ERR", "0"); ("A_U8N3R", "1021") ];
  let independent_exe = fixture "many-updates-independent-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original compile-time independent operands" expected
        (List.assoc name independent_exe))
    [ ("PARAM", "384"); ("INDEP", "1277"); ("ERR", "0") ];
  let independent = fixture "many-updates-independent" in
  List.iter
    (fun (kind, bits, signed) ->
      let unit = Int64.shift_left 1L (if signed then bits - 1 else bits) in
      let max = Int64.pred unit in
      List.iter
        (fun count ->
          let wide =
            if count = 3 then Int64.add (Int64.mul 5L max) 2L
            else Int64.add (Int64.mul 6L max) 1L
          in
          let narrow =
            if count = 3 then
              if signed then Int64.sub (Int64.neg unit) 3L
              else Int64.sub (Int64.mul 2L unit) 3L
            else if signed then -5L
            else Int64.sub (Int64.mul 3L unit) 5L
          in
          List.iter
            (fun prefix ->
              List.iter
                (fun (suffix, value) ->
                  Alcotest.(check string)
                    "original independent update operands"
                    (Int64.to_string value)
                    (List.assoc
                       (Printf.sprintf "%s_%sN%d%s" prefix kind count suffix)
                       independent))
                [ ("R", wide); ("N", narrow) ])
            [ "J"; "A" ])
        [ 3; 4 ])
    [
      ("S8", 8, true);
      ("U8", 8, false);
      ("S16", 16, true);
      ("U16", 16, false);
      ("S32", 32, true);
      ("U32", 32, false);
    ];
  let mixed_explicit = fixture "many-updates-mixed-explicit" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original mixed explicit register updates" expected
        (List.assoc name mixed_explicit))
    [
      ("J_3", "1020");
      ("J_5", "2294");
      ("ERR", "0");
      ("A_3", "1020");
      ("A_5", "2294");
    ];
  let mixed_exe = fixture "many-updates-mixed-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original compile-time mixed updates" expected
        (List.assoc name mixed_exe))
    [ ("PARAM", "384"); ("FIVE", "2293"); ("ERR", "0") ];
  let mixed = fixture "many-updates-mixed-operators" in
  List.iter
    (fun (kind, bits, signed) ->
      let unit = Int64.shift_left 1L (if signed then bits - 1 else bits) in
      let modulus = Int64.shift_left 1L bits in
      let max = Int64.pred unit in
      let wrap value =
        let value = Int64.logand value (Int64.pred modulus) in
        if signed && value >= unit then Int64.sub value modulus else value
      in
      let value = function
        | "+" -> Int64.add max 2L
        | "-" -> Int64.sub max 2L
        | "*" -> Int64.mul max 2L
        | "/" -> Int64.div max 2L
        | "<<" -> Int64.mul max 4L
        | ">>" -> Int64.div max 4L
        | "&" -> Int64.logand max 2L
        | "|" -> Int64.logor max 2L
        | "^" -> Int64.logxor max 2L
        | _ -> assert false
      in
      List.iter
        (fun count ->
          List.iteri
            (fun index pattern ->
              let ops = List.filteri (fun i _ -> i < count) pattern in
              List.iter
                (fun prefix ->
                  List.iter
                    (fun (suffix, stack) ->
                      let expected =
                        List.fold_left
                          (fun sum op ->
                            let v = value op in
                            Int64.add sum (if stack then wrap v else v))
                          0L ops
                      in
                      Alcotest.(check string)
                        "original mixed-update operators"
                        (Int64.to_string expected)
                        (List.assoc
                           (Printf.sprintf "%s_%sN%dP%d%s" prefix kind count
                              index suffix)
                           mixed))
                    [ ("R", false); ("N", true) ])
                [ "J"; "A" ])
            [
              [ "+"; "-"; "*"; "<<"; "^" ];
              [ "/"; ">>"; "&"; "|"; "+" ];
              [ "^"; "&"; "*"; ">>"; "/" ];
            ])
        [ 3; 4; 5 ])
    [
      ("S8", 8, true);
      ("U8", 8, false);
      ("S16", 16, true);
      ("U16", 16, false);
      ("S32", 32, true);
      ("U32", 32, false);
    ];
  let many_ops = fixture "many-updates-operators" in
  List.iter
    (fun count ->
      List.iteri
        (fun index (wide, narrow) ->
          List.iter
            (fun prefix ->
              List.iter
                (fun (suffix, value) ->
                  Alcotest.(check string)
                    "original many-update operator"
                    (string_of_int (count * value))
                    (List.assoc
                       (Printf.sprintf "%s_%dX%d%s" prefix count index suffix)
                       many_ops))
                [ ("R", wide); ("N", narrow) ])
            [ "J"; "A" ])
        [
          (257, 1);
          (254, 254);
          (510, 254);
          (255, 255);
          (510, 254);
          (127, 127);
          (127, 127);
          (255, 255);
          (254, 254);
        ])
    [ 4; 5 ];
  let many_widths = fixture "many-updates-operator-widths" in
  List.iter
    (fun (kind, bits, signed) ->
      let unit = Int64.shift_left 1L (if signed then bits - 1 else bits) in
      let max = Int64.pred unit in
      let wrap_add = if signed then Int64.succ (Int64.neg unit) else 1L in
      let wrap_double = if signed then -2L else Int64.sub unit 2L in
      let values =
        [
          (Int64.add max 2L, wrap_add);
          (Int64.pred max, Int64.pred max);
          (Int64.mul max 2L, wrap_double);
          (max, max);
          (Int64.mul max 2L, wrap_double);
          (Int64.div max 2L, Int64.div max 2L);
          (127L, 127L);
          (max, max);
          (Int64.pred max, Int64.pred max);
        ]
      in
      List.iter
        (fun count ->
          List.iteri
            (fun index (wide, narrow) ->
              List.iter
                (fun prefix ->
                  List.iter
                    (fun (suffix, value) ->
                      Alcotest.(check string)
                        "original many-update operator width"
                        (Int64.to_string (Int64.mul (Int64.of_int count) value))
                        (List.assoc
                           (Printf.sprintf "%s_%sN%dX%d%s" prefix kind count
                              index suffix)
                           many_widths))
                    [ ("R", wide); ("N", narrow) ])
                [ "J"; "A" ])
            values)
        [ 4; 5 ])
    [
      ("S8", 8, true);
      ("S16", 16, true);
      ("U16", 16, false);
      ("S32", 32, true);
      ("U32", 32, false);
    ];
  let operator_widths = fixture "three-updates-operator-widths" in
  List.iter
    (fun (kind, bits, signed) ->
      let unit = Int64.shift_left 1L (if signed then bits - 1 else bits) in
      let max = Int64.pred unit in
      let wrap_add = if signed then Int64.succ (Int64.neg unit) else 1L in
      let wrap_double = if signed then -2L else Int64.sub unit 2L in
      let values =
        [
          (Int64.add max 2L, wrap_add);
          (Int64.pred max, Int64.pred max);
          (Int64.mul max 2L, wrap_double);
          (max, max);
          (Int64.mul max 2L, wrap_double);
          (Int64.div max 2L, Int64.div max 2L);
          (127L, 127L);
          (max, max);
          (Int64.pred max, Int64.pred max);
        ]
      in
      List.iteri
        (fun index (wide, narrow) ->
          List.iter
            (fun prefix ->
              List.iter
                (fun (suffix, value) ->
                  Alcotest.(check string)
                    "original three-update operator width"
                    (Int64.to_string (Int64.mul 3L value))
                    (List.assoc
                       (Printf.sprintf "%s_%sX%d%s" prefix kind index suffix)
                       operator_widths))
                [ ("R", wide); ("N", narrow) ])
            [ "J"; "A" ])
        values)
    [
      ("S8", 8, true);
      ("S16", 16, true);
      ("U16", 16, false);
      ("S32", 32, true);
      ("U32", 32, false);
    ];
  let sequence_ops = fixture "three-updates-operators" in
  List.iteri
    (fun index (register, stack) ->
      List.iter
        (fun prefix ->
          List.iter
            (fun (suffix, expected) ->
              Alcotest.(check string)
                "original three-update operator" expected
                (List.assoc
                   (Printf.sprintf "%s_%d%s" prefix index suffix)
                   sequence_ops))
            [ ("R", register); ("N", stack) ])
        [ "J"; "A" ])
    [
      ("771", "3");
      ("762", "762");
      ("1530", "762");
      ("765", "765");
      ("1530", "762");
      ("381", "381");
      ("381", "381");
      ("765", "765");
      ("762", "762");
    ];
  let sequence_exe = fixture "many-updates-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original compile-time update sequence" expected
        (List.assoc name sequence_exe))
    [ ("PARAM", "384"); ("FIVE", "1280"); ("ERR", "0") ];
  let sequence_widths = fixture "many-updates-widths" in
  List.iter
    (fun (kind, expected) ->
      List.iter
        (fun prefix ->
          Alcotest.(check string)
            "original multi-update width" expected
            (List.assoc (prefix ^ "_5" ^ kind) sequence_widths))
        [ "J"; "A" ])
    [
      ("S16R", "163840");
      ("S16N", "-163840");
      ("U16R", "327680");
      ("U16N", "0");
      ("S32R", "10737418240");
      ("S32N", "-10737418240");
      ("U32R", "21474836480");
      ("U32N", "0");
    ];
  let full_narrow = fixture "two-updates-full-narrow" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "original full-width and narrow updates" expected
            (List.assoc (prefix ^ name) full_narrow))
        [ ("S", "171"); ("U", "171"); ("SN", "-85"); ("UN", "-85") ])
    [ "J_"; "A_" ];
  let mixed_ops = fixture "two-updates-mixed-types-ops" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "original mixed-signedness operations" expected
            (List.assoc (prefix ^ name) mixed_ops))
        [ ("S", "509"); ("U", "637"); ("SN", "253"); ("UN", "381") ])
    [ "J_"; "A_" ];
  List.iter
    (fun (sb, ub) ->
      let fields =
        fixture (Printf.sprintf "two-updates-mixed-width-%d-%d" sb ub)
      in
      let open Int64 in
      let s = shift_left 1L (sb - 1) and u = shift_left 1L ub in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original mixed-width updates" (to_string expected)
                (List.assoc (prefix ^ name) fields))
            [ ("S", add s u); ("U", add s u); ("SN", neg s); ("UN", neg s) ])
        [ "J_"; "A_" ])
    [ (8, 16); (16, 32); (8, 32) ];
  List.iter
    (fun (label, first, second) ->
      let fields = fixture ("two-updates-width-" ^ label) in
      let values ty =
        let bits = int_of_string (String.sub ty 1 (String.length ty - 1)) in
        let open Int64 in
        if ty.[0] = 'I' then
          let n = shift_left 1L (bits - 1) in
          (n, neg n)
        else (shift_left 1L bits, 0L)
      in
      let fw, fn = values first and sw, sn = values second in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original unequal-width updates" (Int64.to_string expected)
                (List.assoc (prefix ^ name) fields))
            [
              ("S", Int64.add fw sw);
              ("U", Int64.add fw sw);
              ("SN", Int64.add fn sn);
              ("UN", Int64.add fn sn);
            ])
        [ "J_"; "A_" ])
    [
      ("i16-u8", "I16", "U8");
      ("i32-u16", "I32", "U16");
      ("i32-u8", "I32", "U8");
      ("i8-i16", "I8", "I16");
      ("i8-i32", "I8", "I32");
      ("i16-i32", "I16", "I32");
      ("u8-u16", "U8", "U16");
      ("u8-u32", "U8", "U32");
      ("u16-u32", "U16", "U32");
    ];
  let width_ops = fixture "two-updates-mixed-width-ops" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "original unequal-width operations" expected
            (List.assoc (prefix ^ name) width_ops))
        [ ("S", "65789"); ("U", "131197"); ("SN", "65533"); ("UN", "65661") ])
    [ "J_"; "A_" ];
  let width_exe = fixture "two-updates-mixed-width-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original unequal-width compile-time execution" expected
        (List.assoc name width_exe))
    [ ("PARAM", "65664"); ("LOCAL", "510") ];
  let mixed_exe = fixture "two-updates-mixed-types-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original mixed-signedness compile-time execution" expected
        (List.assoc name mixed_exe))
    [ ("PARAM", "384"); ("LOCAL", "510") ];
  let unsigned = fixture "u8" in
  List.iter
    (fun prefix ->
      Alcotest.(check string)
        "unsigned register" "510"
        (List.assoc (prefix ^ "REG") unsigned);
      Alcotest.(check string)
        "unsigned disabled register" "254"
        (List.assoc (prefix ^ "NOREG") unsigned))
    [ "JIT_"; "AOT_" ];
  let entry = fixture "entry" in
  List.iter
    (fun field ->
      Alcotest.(check string) "signed entry" "-1" (List.assoc field entry))
    [ "JS"; "AS" ];
  List.iter
    (fun field ->
      Alcotest.(check string) "unsigned entry" "0" (List.assoc field entry))
    [ "JU"; "AU" ];
  let retained = fixture "retained" in
  Alcotest.(check string)
    "retained signed register" "254"
    (List.assoc "SIGNED" retained);
  Alcotest.(check string)
    "retained unsigned register" "510"
    (List.assoc "UNSIGNED" retained);
  let default = fixture "default" in
  Alcotest.(check string)
    "default signed register" "254"
    (List.assoc "SIGNED" default);
  Alcotest.(check string)
    "default unsigned register" "510"
    (List.assoc "UNSIGNED" default);
  let widths = fixture "widths" in
  let noreg_widths = fixture "widths-noreg" in
  let entry_widths = fixture "widths-entry" in
  let default_widths = fixture "widths-default" in
  List.iter
    (fun (name, wide, narrow, entry) ->
      List.iter
        (fun prefix ->
          Alcotest.(check string)
            "wide register result" wide
            (List.assoc (prefix ^ name) widths);
          Alcotest.(check string)
            "noreg stored result" narrow
            (List.assoc (prefix ^ name) noreg_widths);
          Alcotest.(check string)
            "wide parameter entry" entry
            (List.assoc (prefix ^ name) entry_widths))
        [ "J_"; "A_" ];
      Alcotest.(check string)
        "wide retained default" wide
        (List.assoc name default_widths))
    [
      ("S16", "65534", "-2", "-1");
      ("U16", "131070", "65534", "0");
      ("S32", "4294967294", "-2", "-1");
      ("U32", "8589934590", "4294967294", "0");
    ];
  let local = fixture "local-entry" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local register initialization" expected
            (List.assoc (prefix ^ name) local))
        [ ("S", "255"); ("U", "256"); ("S16", "65535"); ("U16", "65536") ])
    [ "J_"; "A_" ];
  let local_noreg = fixture "local-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local noreg initialization" expected
            (List.assoc (prefix ^ name) local_noreg))
        [ ("S", "-1"); ("U", "0"); ("S16", "-1"); ("U16", "0") ])
    [ "J_"; "A_" ];
  let local_wide = fixture "local-wide" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "32-bit local initialization" expected
            (List.assoc (prefix ^ name) local_wide))
        [ ("S", "4294967295"); ("U", "4294967296"); ("SN", "-1"); ("UN", "0") ])
    [ "J_"; "A_" ];
  let local_default = fixture "local-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained default" expected
        (List.assoc name local_default))
    [ ("S", "255"); ("U", "256"); ("SH", "254") ];
  let local_right = fixture "local-right" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local right shift" expected
            (List.assoc (prefix ^ name) local_right))
        [ ("S", "127"); ("U", "128"); ("SN", "-1"); ("UN", "0") ])
    [ "J_"; "A_" ];
  List.iter
    (fun (stem, signed, unsigned) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "wider local right shift" expected
                (List.assoc (prefix ^ name) fields))
            [ ("S", signed); ("U", unsigned); ("SN", "-1"); ("UN", "0") ])
        [ "J_"; "A_" ])
    [
      ("local-mid-right", "32767", "32768");
      ("local-wide-right", "2147483647", "2147483648");
    ];
  let local_default_right = fixture "local-default-right" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained default right shift" expected
        (List.assoc name local_default_right))
    [ ("S", "127"); ("U", "128"); ("SH", "63") ];
  let local_return = fixture "local-return" in
  let local_return_small = fixture "local-return-small" in
  let local_return_noreg = fixture "local-return-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "small local return" expected
            (List.assoc (prefix ^ name) local_return_small))
        [ ("S", "255"); ("U", "256"); ("S16", "65535"); ("U16", "65536") ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "noreg local return" expected
            (List.assoc (prefix ^ name) local_return_noreg))
        [ ("S", "-1"); ("U", "0"); ("S16", "-1"); ("U16", "0") ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local return" expected
            (List.assoc (prefix ^ name) local_return))
        [ ("S", "4294967295"); ("U", "4294967296"); ("SN", "-1"); ("UN", "0") ])
    [ "J_"; "A_" ];
  let local_return_init = fixture "local-return-init" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained local return" expected
        (List.assoc name local_return_init))
    [ ("S8", "255"); ("U8", "256"); ("S16", "65535"); ("S32", "4294967338") ];
  let high = fixture "local-return-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun name ->
          Alcotest.(check string)
            "high-bit local return" "-9223372036854775766"
            (List.assoc (prefix ^ name) high))
        [ "S8"; "S16"; "U16"; "S32"; "U32" ])
    [ "J_"; "A_" ];
  let local_return_default = fixture "local-return-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained direct local return" expected
        (List.assoc name local_return_default))
    [ ("S", "255"); ("U", "256"); ("SH", "127") ];
  let narrow_return = fixture "local-narrow-return" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "narrow declared local return" expected
            (List.assoc (prefix ^ name) narrow_return))
        [ ("S", "255"); ("U", "256"); ("S16", "65535"); ("U16", "65536") ])
    [ "J_"; "A_" ];
  let hidden = fixture "local-return-hidden" in
  Alcotest.(check string) "hidden callee result" "0" (List.assoc "VALUE" hidden);
  Alcotest.(check string)
    "hidden callee stored value" "0"
    (List.assoc "STORED" hidden);
  let add_byte = fixture "add-byte" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "byte compound addition" expected
            (List.assoc (prefix ^ name) add_byte))
        [ ("S", "128"); ("U", "256"); ("SN", "-128"); ("UN", "0") ])
    [ "J_"; "A_" ];
  let add_wide = fixture "add-wide" and add_noreg = fixture "add-wide-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "wide compound addition" wide
            (List.assoc (prefix ^ name) add_wide);
          Alcotest.(check string)
            "noreg compound addition" narrow
            (List.assoc (prefix ^ name) add_noreg))
        [
          ("S16", "32768", "-32768");
          ("U16", "65536", "0");
          ("S32", "2147483648", "-2147483648");
          ("U32", "4294967296", "0");
        ])
    [ "J_"; "A_" ];
  let add_default = fixture "add-default" in
  Alcotest.(check string)
    "signed retained addition" "128"
    (List.assoc "SIGNED" add_default);
  Alcotest.(check string)
    "unsigned retained addition" "256"
    (List.assoc "UNSIGNED" add_default);
  let add_wide_default = fixture "add-wide-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "wide retained addition" expected
        (List.assoc name add_wide_default))
    [
      ("S16", "32768");
      ("U16", "65536");
      ("S32", "2147483648");
      ("U32", "4294967296");
    ];
  let local_add = fixture "local-add"
  and local_add_noreg = fixture "local-add-noreg" in
  let local_add_wide = fixture "local-add-wide" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "local addition" wide
            (List.assoc (prefix ^ name) local_add);
          Alcotest.(check string)
            "noreg local addition" narrow
            (List.assoc (prefix ^ name) local_add_noreg))
        [
          ("S", "256", "0");
          ("U", "257", "1");
          ("S16", "65536", "0");
          ("U16", "65537", "1");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local addition" expected
            (List.assoc (prefix ^ name) local_add_wide))
        [ ("S", "4294967296"); ("U", "4294967297"); ("SN", "0"); ("UN", "1") ])
    [ "J_"; "A_" ];
  let local_add_default = fixture "local-add-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained addition" expected
        (List.assoc name local_add_default))
    [ ("S", "256"); ("U", "257"); ("SH", "128") ];
  let add_overflow = fixture "local-add-overflow" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local addition crossing byte bounds" expected
            (List.assoc (prefix ^ name) add_overflow))
        [ ("S", "175"); ("U", "298"); ("SN", "-81"); ("UN", "42") ])
    [ "J_"; "A_" ];
  let add_count = fixture "local-add-count" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local addition wider literal" expected
            (List.assoc (prefix ^ name) add_count))
        [ ("S", "511"); ("U", "512"); ("S16", "65791"); ("U16", "65792") ])
    [ "J_"; "A_" ];
  check_incomplete "local-add-high-fault";
  check_incomplete "local-sub-high-fault";
  let add_param_high = fixture "add-param-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high-bit parameter addition" expected
            (List.assoc (prefix ^ name) add_param_high))
        [
          ("S", "-9223372036854775681");
          ("U", "-9223372036854775553");
          ("SN", "127");
          ("UN", "255");
        ])
    [ "J_"; "A_" ];
  let add_max = fixture "local-add-max32" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "maximum proven local add literal" expected
            (List.assoc (prefix ^ name) add_max))
        [
          ("S", "2147483902");
          ("U", "2147483903");
          ("S16", "2147549182");
          ("U16", "2147549183");
        ])
    [ "J_"; "A_" ];
  let sub_byte = fixture "sub-byte" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "byte compound subtraction" expected
            (List.assoc (prefix ^ name) sub_byte))
        [ ("S", "-129"); ("U", "-1"); ("SN", "127"); ("UN", "255") ])
    [ "J_"; "A_" ];
  let sub_wide = fixture "sub-wide" and sub_noreg = fixture "sub-wide-noreg" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "wide compound subtraction" wide
            (List.assoc (prefix ^ name) sub_wide);
          Alcotest.(check string)
            "noreg compound subtraction" narrow
            (List.assoc (prefix ^ name) sub_noreg))
        [
          ("S16", "-32769", "32767");
          ("U16", "-1", "65535");
          ("S32", "-2147483649", "2147483647");
          ("U32", "-1", "4294967295");
        ])
    [ "J_"; "A_" ];
  let sub_high = fixture "sub-param-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high-bit parameter subtraction" expected
            (List.assoc (prefix ^ name) sub_high))
        [
          ("S", "-9223372036854775681");
          ("U", "-9223372036854775553");
          ("SN", "127");
          ("UN", "255");
        ])
    [ "J_"; "A_" ];
  let local_sub = fixture "local-sub"
  and local_sub_noreg = fixture "local-sub-noreg" in
  let local_sub_wide = fixture "local-sub-wide"
  and local_sub_max = fixture "local-sub-max32" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "local subtraction" wide
            (List.assoc (prefix ^ name) local_sub);
          Alcotest.(check string)
            "noreg local subtraction" narrow
            (List.assoc (prefix ^ name) local_sub_noreg))
        [
          ("S", "254", "-2");
          ("U", "255", "255");
          ("S16", "65534", "-2");
          ("U16", "65535", "65535");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local subtraction" expected
            (List.assoc (prefix ^ name) local_sub_wide))
        [
          ("S", "4294967294");
          ("U", "4294967295");
          ("SN", "-2");
          ("UN", "4294967295");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "local subtraction immediate boundary" expected
            (List.assoc (prefix ^ name) local_sub_max))
        [
          ("S", "-2147483392");
          ("U", "-2147483391");
          ("S16", "-2147418112");
          ("U16", "-2147418111");
        ])
    [ "J_"; "A_" ];
  let sub_default = fixture "sub-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained subtraction" expected
        (List.assoc name sub_default))
    [ ("SIGNED", "-129"); ("UNSIGNED", "-1"); ("LOCAL", "254") ];
  let mul_byte = fixture "mul-byte" and mul_wide = fixture "mul-wide" in
  let mul_noreg = fixture "mul-wide-noreg"
  and mul_high = fixture "mul-param-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "byte multiplication" expected
            (List.assoc (prefix ^ name) mul_byte))
        [ ("S", "254"); ("U", "510"); ("SN", "-2"); ("UN", "254") ];
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "wide multiplication" wide
            (List.assoc (prefix ^ name) mul_wide);
          Alcotest.(check string)
            "noreg multiplication" narrow
            (List.assoc (prefix ^ name) mul_noreg))
        [
          ("S16", "65534", "-2");
          ("U16", "131070", "65534");
          ("S32", "4294967294", "-2");
          ("U32", "8589934590", "4294967294");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high parameter multiplication" expected
            (List.assoc (prefix ^ name) mul_high))
        [
          ("S", "-9223372036854775808");
          ("U", "-9223372036854775808");
          ("SN", "0");
          ("UN", "0");
        ])
    [ "J_"; "A_" ];
  let mul_default = fixture "mul-default"
  and mul_wide_default = fixture "mul-wide-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained multiplication" expected
        (List.assoc name mul_default))
    [ ("SIGNED", "254"); ("UNSIGNED", "510") ];
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "wide retained multiplication" expected
        (List.assoc name mul_wide_default))
    [
      ("S16", "65534");
      ("U16", "131070");
      ("S32", "4294967294");
      ("U32", "8589934590");
    ];
  let local_mul = fixture "local-mul"
  and local_noreg_mul = fixture "local-mul-noreg" in
  let local_mul_wide = fixture "local-mul-wide"
  and local_mul_max = fixture "local-mul-max32" in
  let local_mul_high = fixture "local-mul-high"
  and local_noreg_high = fixture "local-mul-noreg-high" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, wide, narrow) ->
          Alcotest.(check string)
            "local multiplication" wide
            (List.assoc (prefix ^ name) local_mul);
          Alcotest.(check string)
            "noreg local multiplication" narrow
            (List.assoc (prefix ^ name) local_noreg_mul))
        [
          ("S", "510", "-2");
          ("U", "512", "0");
          ("S16", "131070", "-2");
          ("U16", "131072", "0");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "wide local multiplication" expected
            (List.assoc (prefix ^ name) local_mul_wide))
        [ ("S", "8589934590"); ("U", "8589934592"); ("SN", "-2"); ("UN", "0") ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "large local multiplication" expected
            (List.assoc (prefix ^ name) local_mul_max))
        [
          ("S", "547608329985");
          ("U", "549755813632");
          ("S16", "140735340806145");
          ("U16", "140737488289792");
        ];
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "high local multiplication" expected
            (List.assoc (prefix ^ name) local_mul_high);
          Alcotest.(check string)
            "high noreg multiplication" "0"
            (List.assoc (prefix ^ name) local_noreg_high))
        [
          ("S", "-9223372036854775808");
          ("U", "0");
          ("S16", "-9223372036854775808");
          ("U16", "0");
        ])
    [ "J_"; "A_" ];
  let local_mul_default = fixture "local-mul-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "local retained multiplication" expected
        (List.assoc name local_mul_default))
    [ ("S", "510"); ("U", "512"); ("SH", "254") ];
  let high_mul_default = fixture "local-mul-high-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "high retained local multiplication" expected
        (List.assoc name high_mul_default))
    [
      ("S", "-9223372036854775808"); ("U", "0"); ("SH", "-9223372036854775808");
    ];
  List.iter
    (fun (stem, expected) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, value) ->
              Alcotest.(check string)
                "parameter division" value
                (List.assoc (prefix ^ name) fields))
            expected)
        [ "J_"; "A_" ])
    [
      ("div-byte", [ ("S", "128"); ("U", "127"); ("SN", "-128"); ("UN", "127") ]);
      ("div-round", [ ("S", "-3"); ("U", "127"); ("SN", "-3"); ("UN", "127") ]);
      ( "div-unsigned-neg",
        [ ("S", "128"); ("U", "0"); ("SN", "-128"); ("UN", "0") ] );
      ( "div-wide",
        [
          ("S16", "32768");
          ("U16", "32767");
          ("S32", "2147483648");
          ("U32", "2147483647");
        ] );
      ( "div-wide-noreg",
        [
          ("S16", "-32768");
          ("U16", "32767");
          ("S32", "-2147483648");
          ("U32", "2147483647");
        ] );
    ];
  List.iter
    (fun (stem, signed, unsigned) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "parameter-initialized local" expected
                (List.assoc (prefix ^ name) fields))
            [ ("S", signed); ("U", unsigned); ("SN", "-1"); ("UN", "0") ])
        [ "J_"; "A_" ])
    [
      ("local-param-byte", "255", "256");
      ("local-param-16", "65535", "65536");
      ("local-param-32", "4294967295", "4294967296");
    ];
  List.iter
    (fun (stem, signed, unsigned, signed_memory, unsigned_memory) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "updated parameter local" expected
                (List.assoc (prefix ^ name) fields))
            [
              ("S", signed);
              ("U", unsigned);
              ("SN", signed_memory);
              ("UN", unsigned_memory);
            ])
        [ "J_"; "A_" ])
    [
      ("local-param-add-byte", "256", "257", "0", "1");
      ("local-param-add-bound", "2147483902", "2147483903", "-2", "255");
      ("local-param-shift-byte", "510", "512", "-2", "0");
      ("local-param-sub-byte", "254", "255", "-2", "255");
      ("local-param-sub-16", "65534", "65535", "-2", "65535");
      ("local-param-sub-32", "4294967294", "4294967295", "-2", "4294967295");
      ("local-param-right-byte", "127", "128", "-1", "0");
      ("local-param-right-16", "32767", "32768", "-1", "0");
      ("local-param-right-32", "2147483647", "2147483648", "-1", "0");
      ("local-param-mul-byte", "765", "768", "-3", "0");
      ("local-param-mul-16", "196605", "196608", "-3", "0");
      ("local-param-mul-32", "12884901885", "12884901888", "-3", "0");
      ("local-param-mul-high", "-9223372036854775808", "0", "0", "0");
      ("local-param-right-high", "-1", "9223372036854775807", "-1", "127");
      ("local-param-add-16", "65536", "65537", "0", "1");
      ("local-param-shift-16", "131070", "131072", "-2", "0");
      ("local-param-add-32", "4294967296", "4294967297", "0", "1");
      ("local-param-shift-32", "8589934590", "8589934592", "-2", "0");
    ];
  let param_update_default = fixture "local-param-update-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained updated parameter local" expected
        (List.assoc name param_update_default))
    [ ("SIGNED", "256"); ("UNSIGNED", "512") ];
  let more_default = fixture "local-param-more-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained parameter local update" expected
        (List.assoc name more_default))
    [ ("SUB", "254"); ("RIGHT", "127"); ("MUL", "765") ];
  List.iter
    (fun (stem, signed, unsigned, signed_memory, unsigned_memory) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "dynamic parameter local" expected
                (List.assoc (prefix ^ name) fields))
            [
              ("S", signed);
              ("U", unsigned);
              ("SN", signed_memory);
              ("UN", unsigned_memory);
            ])
        [ "J_"; "A_" ])
    [
      ("local-param-dynamic-add-byte", "256", "257", "0", "1");
      ("local-param-dynamic-div-byte", "127", "128", "0", "0");
      ("local-param-dynamic-div-negative", "-255", "0", "1", "0");
      ("local-param-dynamic-div-unsigned-count", "-255", "0", "1", "0");
      ("local-param-dynamic-mod-byte", "1", "0", "-1", "0");
      ("local-param-dynamic-mod-negative", "0", "256", "0", "0");
      ("local-param-dynamic-mod-unsigned-count", "0", "256", "0", "0");
      ("local-param-dynamic-div-16", "32767", "32768", "0", "0");
      ("local-param-dynamic-div-32", "2147483647", "2147483648", "0", "0");
      ("local-param-dynamic-mod-16", "1", "0", "-1", "0");
      ("local-param-dynamic-mod-32", "1", "0", "-1", "0");
      ("local-param-dynamic-and-byte", "255", "255", "-1", "255");
      ("local-param-dynamic-and-16", "65535", "65535", "-1", "65535");
      ( "local-param-dynamic-and-32",
        "4294967295",
        "4294967295",
        "-1",
        "4294967295" );
      ("local-param-dynamic-or-byte", "511", "511", "-1", "255");
      ("local-param-dynamic-or-16", "131071", "131071", "-1", "65535");
      ( "local-param-dynamic-or-32",
        "8589934591",
        "8589934591",
        "-1",
        "4294967295" );
      ("local-param-dynamic-xor-byte", "511", "511", "-1", "255");
      ("local-param-dynamic-xor-16", "131071", "131071", "-1", "65535");
      ( "local-param-dynamic-xor-32",
        "8589934591",
        "8589934591",
        "-1",
        "4294967295" );
      ("local-param-dynamic-sub-byte", "254", "255", "-2", "255");
      ("local-param-dynamic-sub-16", "65534", "65535", "-2", "65535");
      ( "local-param-dynamic-sub-32",
        "4294967294",
        "4294967295",
        "-2",
        "4294967295" );
      ("local-param-dynamic-right-byte", "127", "128", "-1", "0");
      ("local-param-dynamic-right-16", "32767", "32768", "-1", "0");
      ("local-param-dynamic-right-32", "2147483647", "2147483648", "-1", "0");
      ("local-param-dynamic-mul-byte", "765", "768", "-3", "0");
      ("local-param-dynamic-mul-16", "196605", "196608", "-3", "0");
      ("local-param-dynamic-mul-32", "12884901885", "12884901888", "-3", "0");
      ("local-param-count-U8", "510", "511", "-2", "255");
      ("local-param-count-I16", "254", "255", "-2", "255");
      ("local-param-count-I32", "254", "255", "-2", "255");
      ("local-param-count-U32", "4294967550", "4294967551", "-2", "255");
      ("local-param-count-U16", "65790", "65791", "-2", "255");
      ("local-param-count-I64", "511", "512", "-1", "0");
      ("local-param-count-U64", "511", "512", "-1", "0");
      ("local-param-dynamic-negative", "254", "255", "-2", "255");
      ("local-param-dynamic-add-16", "65536", "65537", "0", "1");
      ("local-param-dynamic-add-32", "4294967296", "4294967297", "0", "1");
      ("local-param-dynamic-shift-byte", "510", "512", "-2", "0");
      ( "local-param-dynamic-shift-negative",
        "-9223372036854775808",
        "0",
        "0",
        "0" );
      ("local-param-dynamic-shift-16", "131070", "131072", "-2", "0");
      ("local-param-dynamic-shift-32", "8589934590", "8589934592", "-2", "0");
    ];
  let count_default = fixture "local-param-count-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained wide count local" expected
        (List.assoc name count_default))
    [ ("SIGNED", "511"); ("UNSIGNED", "256") ];
  let dynamic_default = fixture "local-param-dynamic-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained dynamic local" expected
        (List.assoc name dynamic_default))
    [ ("SIGNED", "256"); ("UNSIGNED", "512") ];
  let dynamic_more_default = fixture "local-param-dynamic-more-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained dynamic update" expected
        (List.assoc name dynamic_more_default))
    [ ("SUB", "254"); ("RIGHT", "127"); ("MUL", "765") ];
  let dynamic_bit_default = fixture "local-param-dynamic-bit-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained dynamic bitwise" expected
        (List.assoc name dynamic_bit_default))
    [ ("AND", "255"); ("OR", "511"); ("XOR", "511") ];
  List.iter
    (fun stem ->
      let fields = fixture stem in
      List.iter
        (fun name ->
          Alcotest.(check string)
            "original dynamic division fault" "1" (List.assoc name fields))
        [ "J_CAUGHT"; "A_CAUGHT" ])
    [
      "local-param-dynamic-div-zero";
      "local-param-dynamic-div-overflow";
      "local-param-dynamic-mod-zero";
      "local-param-dynamic-mod-overflow";
    ];
  let divmod_default = fixture "local-param-dynamic-divmod-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained dynamic division" expected
        (List.assoc name divmod_default))
    [ ("DIV", "127"); ("MOD", "1"); ("MUL", "765") ];
  List.iter
    (fun (stem, signed_value, unsigned_value, signed_memory, unsigned_memory) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "narrow parameter local" expected
                (List.assoc (prefix ^ name) fields))
            [
              ("S", signed_value);
              ("U", unsigned_value);
              ("SN", signed_memory);
              ("UN", unsigned_memory);
            ])
        [ "J_"; "A_" ])
    [
      ("local-init-i8-8", "-1", "255", "-1", "-1");
      ("local-init-u8-8", "-1", "255", "255", "255");
      ("local-init-i8-16", "-1", "65535", "-1", "-1");
      ("local-init-u8-16", "-1", "65535", "255", "255");
      ("local-init-i8-32", "-1", "4294967295", "-1", "-1");
      ("local-init-u8-32", "-1", "4294967295", "255", "255");
      ("local-init-i16-32", "-1", "4294967295", "-1", "-1");
      ("local-init-u16-32", "-1", "4294967295", "65535", "65535");
      ("local-init-i32-32", "-1", "4294967295", "-1", "-1");
      ("local-init-u32-32", "-1", "4294967295", "4294967295", "4294967295");
      ("local-init-narrow-update", "128", "256", "-128", "0");
    ];
  let narrow_default = fixture "local-init-narrow-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained narrow initializer" expected
        (List.assoc name narrow_default))
    [ ("DIRECT", "255"); ("UPDATE", "765") ];
  List.iter
    (fun (stem, signed, unsigned, signed_memory, unsigned_memory) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "literal local with parameter" expected
                (List.assoc (prefix ^ name) fields))
            [
              ("S", signed);
              ("U", unsigned);
              ("SN", signed_memory);
              ("UN", unsigned_memory);
            ])
        [ "J_"; "A_" ])
    [
      ("local-literal-param-direct", "255", "256", "-1", "0");
      ("local-literal-param-constant-add", "256", "257", "0", "1");
      ("local-literal-param-constant-shift", "510", "512", "-2", "0");
      ("local-literal-param-direct-16", "65535", "65536", "-1", "0");
      ("local-literal-param-direct-32", "4294967295", "4294967296", "-1", "0");
      ("local-literal-param-update", "510", "512", "-2", "0");
      ( "local-literal-param-update-high",
        "-9223372036854775553",
        "-9223372036854775552",
        "-1",
        "0" );
      ("local-literal-param-sub-byte", "254", "255", "-2", "255");
      ("local-literal-param-mul-byte", "765", "768", "-3", "0");
      ("local-literal-param-left-byte", "510", "512", "-2", "0");
      ("local-literal-param-right-byte", "127", "128", "-1", "0");
      ("local-literal-param-xor-byte", "511", "0", "-1", "0");
      ("local-literal-param-or-byte", "511", "256", "-1", "0");
      ("local-literal-param-and-byte", "255", "0", "-1", "0");
      ("local-literal-param-div-byte", "127", "128", "0", "0");
      ("local-literal-param-mod-byte", "1", "0", "-1", "0");
      ("local-literal-param-update-16", "131070", "131072", "-2", "0");
      ("local-literal-param-update-32", "8589934590", "8589934592", "-2", "0");
    ];
  let literal_parameter_default = fixture "local-literal-param-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained literal local with parameter" expected
        (List.assoc name literal_parameter_default))
    [ ("DIRECT", "255"); ("UPDATE", "510") ];
  List.iter
    (fun stem ->
      let fields = fixture stem in
      List.iter
        (fun name ->
          Alcotest.(check string)
            "original literal-local division fault" "1" (List.assoc name fields))
        [ "J_CAUGHT"; "A_CAUGHT" ])
    [
      "local-literal-param-div-zero";
      "local-literal-param-div-overflow";
      "local-literal-param-mod-zero";
      "local-literal-param-mod-overflow";
    ];
  let literal_more_default = fixture "local-literal-param-more-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained literal parameter update" expected
        (List.assoc name literal_more_default))
    [ ("SUB", "254"); ("MUL", "765"); ("OR", "511"); ("XOR", "511") ];
  let literal_remaining_default =
    fixture "local-literal-param-remaining-default"
  in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained literal parameter arithmetic" expected
        (List.assoc name literal_remaining_default))
    [ ("DIV", "127"); ("MOD", "1"); ("SHL", "510"); ("AND", "255") ];
  List.iter
    (fun (shape, operation, signed, unsigned, signed_memory, unsigned_memory) ->
      List.iter
        (fun prefix_form ->
          let fields =
            fixture ("unary-" ^ shape ^ "-" ^ prefix_form ^ operation ^ "-byte")
          in
          List.iter
            (fun prefix ->
              List.iter
                (fun (name, expected) ->
                  Alcotest.(check string)
                    "original unary update" expected
                    (List.assoc (prefix ^ name) fields))
                [
                  ("S", signed);
                  ("U", unsigned);
                  ("SN", signed_memory);
                  ("UN", unsigned_memory);
                ])
            [ "J_"; "A_" ])
        [ ""; "prefix-" ])
    [
      ("parameter", "increment", "128", "256", "-128", "0");
      ("parameter", "decrement", "-129", "-1", "127", "255");
      ("local", "increment", "256", "257", "0", "1");
      ("local", "decrement", "254", "255", "-2", "255");
    ];
  List.iter
    (fun ( shape,
           operation,
           width,
           signed,
           unsigned,
           signed_memory,
           unsigned_memory ) ->
      let fields = fixture ("unary-" ^ shape ^ "-" ^ operation ^ "-" ^ width) in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "wide original unary update" expected
                (List.assoc (prefix ^ name) fields))
            [
              ("S", signed);
              ("U", unsigned);
              ("SN", signed_memory);
              ("UN", unsigned_memory);
            ])
        [ "J_"; "A_" ])
    [
      ("parameter", "increment", "16", "32768", "65536", "-32768", "0");
      ( "parameter",
        "increment",
        "32",
        "2147483648",
        "4294967296",
        "-2147483648",
        "0" );
      ("parameter", "decrement", "16", "-32769", "-1", "32767", "65535");
      ( "parameter",
        "decrement",
        "32",
        "-2147483649",
        "-1",
        "2147483647",
        "4294967295" );
      ("local", "increment", "16", "65536", "65537", "0", "1");
      ("local", "increment", "32", "4294967296", "4294967297", "0", "1");
      ("local", "decrement", "16", "65534", "65535", "-2", "65535");
      ( "local",
        "decrement",
        "32",
        "4294967294",
        "4294967295",
        "-2",
        "4294967295" );
    ];
  List.iter
    (fun shape ->
      List.iter
        (fun operation ->
          List.iter
            (fun width ->
              let suffix = shape ^ "-" ^ operation ^ "-" ^ width in
              let expected = fixture ("unary-" ^ suffix) in
              let actual = fixture ("prefix-return-" ^ suffix) in
              List.iter
                (fun name ->
                  Alcotest.(check string)
                    "original returned prefix update" (List.assoc name expected)
                    (List.assoc name actual))
                [ "J_S"; "J_U"; "J_SN"; "J_UN"; "A_S"; "A_U"; "A_SN"; "A_UN" ])
            [ "byte"; "16"; "32" ])
        [ "increment"; "decrement" ])
    [ "parameter"; "local" ];
  List.iter
    (fun (stem, values) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original narrow function prefix return" expected
                (List.assoc (prefix ^ name) fields))
            values)
        [ "J_"; "A_" ])
    [
      ( "prefix-return-narrow-type",
        [ ("S", "256"); ("U", "257"); ("SN", "0"); ("UN", "1") ] );
      ( "prefix-return-narrow-edge",
        [ ("S", "128"); ("U", "256"); ("SN", "-128"); ("UN", "0") ] );
    ];
  List.iter
    (fun ( shape,
           operation,
           width,
           signed,
           unsigned,
           signed_memory,
           unsigned_memory ) ->
      let fields =
        fixture
          (if shape = "narrow" then "postfix-return-narrow-type"
           else "postfix-return-" ^ shape ^ "-" ^ operation ^ "-" ^ width)
      in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original returned postfix update" expected
                (List.assoc (prefix ^ name) fields))
            [
              ("S", signed);
              ("U", unsigned);
              ("SN", signed_memory);
              ("UN", unsigned_memory);
            ])
        [ "J_"; "A_" ])
    [
      ("local", "increment", "byte", "255", "256", "-1", "0");
      ("local", "decrement", "byte", "255", "256", "-1", "0");
      ("local", "increment", "16", "65535", "65536", "-1", "0");
      ("local", "decrement", "16", "65535", "65536", "-1", "0");
      ("local", "increment", "32", "4294967295", "4294967296", "-1", "0");
      ("local", "decrement", "32", "4294967295", "4294967296", "-1", "0");
      ("parameter", "increment", "byte", "127", "255", "127", "255");
      ("parameter", "decrement", "byte", "-128", "0", "-128", "0");
      ("parameter", "increment", "16", "32767", "65535", "32767", "65535");
      ("parameter", "decrement", "16", "-32768", "0", "-32768", "0");
      ( "parameter",
        "increment",
        "32",
        "2147483647",
        "4294967295",
        "2147483647",
        "4294967295" );
      ("parameter", "decrement", "32", "-2147483648", "0", "-2147483648", "0");
      ("narrow", "type", "", "255", "256", "-1", "0");
    ];
  List.iter
    (fun (width, bits) ->
      let n = Int64.shift_left 1L bits in
      let open Int64 in
      List.iter
        (fun (operation, values) ->
          let fields =
            fixture ("compound-return-local-" ^ operation ^ "-" ^ width)
          in
          List.iter
            (fun prefix ->
              List.iter2
                (fun name expected ->
                  Alcotest.(check string)
                    "original returned compound update" (to_string expected)
                    (List.assoc (prefix ^ name) fields))
                [ "S"; "U"; "SN"; "UN" ] values)
            [ "J_"; "A_" ])
        [
          ("add", [ n; add n 1L; 0L; 1L ]);
          ("sub", [ sub n 2L; sub n 1L; -2L; sub n 1L ]);
          ("mul", [ sub n 1L; n; -1L; 0L ]);
          ("shl", [ mul (sub n 1L) 2L; mul n 2L; -2L; 0L ]);
          ("shr", [ sub (div n 2L) 1L; div n 2L; -1L; 0L ]);
        ])
    [ ("byte", 8); ("16", 16); ("32", 32) ];
  List.iter
    (fun (operation, values) ->
      let fields =
        fixture ("compound-return-local-" ^ operation ^ "-boundary")
      in
      List.iter
        (fun prefix ->
          List.iter2
            (fun name expected ->
              Alcotest.(check string)
                "original compound immediate boundary" expected
                (List.assoc (prefix ^ name) fields))
            [ "S"; "U"; "SN"; "UN" ] values)
        [ "J_"; "A_" ])
    [
      ("add", [ "2147483902"; "2147483903"; "-2"; "255" ]);
      ("sub", [ "-2147483392"; "-2147483391"; "0"; "1" ]);
    ];
  let add_256 = fixture "compound-return-local-add-256" in
  List.iter
    (fun prefix ->
      List.iter2
        (fun name expected ->
          Alcotest.(check string)
            "original compound return byte crossing" expected
            (List.assoc (prefix ^ name) add_256))
        [ "S"; "U"; "SN"; "UN" ]
        [ "511"; "512"; "-1"; "0" ])
    [ "J_"; "A_" ];
  List.iter
    (fun (width, bits) ->
      let open Int64 in
      let n = shift_left 1L bits in
      let signed_max = sub (div n 2L) 1L and unsigned_max = sub n 1L in
      List.iter
        (fun (operation, values) ->
          let fields =
            fixture ("compound-return-parameter-" ^ operation ^ "-" ^ width)
          in
          List.iter
            (fun prefix ->
              List.iter2
                (fun name expected ->
                  Alcotest.(check string)
                    "original parameter compound return" (to_string expected)
                    (List.assoc (prefix ^ name) fields))
                [ "S"; "U"; "SN"; "UN" ] values)
            [ "J_"; "A_" ])
        [
          ("add", [ add signed_max 1L; add unsigned_max 1L; neg (div n 2L); 0L ]);
          ( "sub",
            [
              sub signed_max 1L;
              sub unsigned_max 1L;
              sub signed_max 1L;
              sub unsigned_max 1L;
            ] );
          ("mul", [ signed_max; unsigned_max; signed_max; unsigned_max ]);
          ( "shl",
            [
              mul signed_max 2L;
              mul unsigned_max 2L;
              mul signed_max 2L;
              mul unsigned_max 2L;
            ] );
          ( "shr",
            [
              div signed_max 2L;
              div unsigned_max 2L;
              div signed_max 2L;
              div unsigned_max 2L;
            ] );
        ];
      List.iter
        (fun (operation, values) ->
          let fields =
            fixture ("compound-return-parameter-" ^ operation ^ "-edge-" ^ width)
          in
          List.iter
            (fun prefix ->
              List.iter2
                (fun name expected ->
                  Alcotest.(check string)
                    "original parameter compound edge" (to_string expected)
                    (List.assoc (prefix ^ name) fields))
                [ "S"; "U"; "SN"; "UN" ] values)
            [ "J_"; "A_" ])
        [
          ("sub", [ sub (neg (div n 2L)) 1L; -1L; signed_max; unsigned_max ]);
          ( "mul",
            [
              mul signed_max 2L;
              mul unsigned_max 2L;
              mul signed_max 2L;
              mul unsigned_max 2L;
            ] );
        ])
    [ ("byte", 8); ("16", 16); ("32", 32) ];
  List.iter
    (fun (stem, values) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter2
            (fun name expected ->
              Alcotest.(check string)
                "original compound return overflow" expected
                (List.assoc (prefix ^ name) fields))
            [ "S"; "U"; "SN"; "UN" ] values)
        [ "J_"; "A_" ])
    [
      ("compound-return-local-mul-overflow", [ "254"; "510"; "254"; "510" ]);
      ("compound-return-local-shl-overflow", [ "254"; "510"; "254"; "510" ]);
      ( "compound-return-parameter-add-boundary",
        [ "2147483774"; "2147483902"; "126"; "254" ] );
      ( "compound-return-parameter-sub-boundary",
        [ "-2147483520"; "-2147483392"; "-128"; "0" ] );
    ];
  let parameter_default = fixture "compound-param-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained original parameter compound return" expected
        (List.assoc name parameter_default))
    [
      ("ADD", "128");
      ("SUB", "-129");
      ("MUL", "510");
      ("SHL", "510");
      ("SHR", "63");
    ];
  let parameter_narrow_type = fixture "compound-param-narrow-type" in
  List.iter
    (fun prefix ->
      List.iter2
        (fun name expected ->
          Alcotest.(check string)
            "original narrow parameter compound return" expected
            (List.assoc (prefix ^ name) parameter_narrow_type))
        [ "S"; "U"; "SN"; "UN" ]
        [ "128"; "256"; "-128"; "0" ])
    [ "J_"; "A_" ];
  let compound_default = fixture "compound-return-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained original compound return" expected
        (List.assoc name compound_default))
    [
      ("ADD", "256");
      ("SUB", "254");
      ("MUL", "255");
      ("SHL", "510");
      ("SHR", "127");
    ];
  List.iter
    (fun (width, bits) ->
      let open Int64 in
      let n = shift_left 1L bits in
      List.iter
        (fun (operation, signed, unsigned) ->
          let fields =
            fixture ("global-immediate-" ^ operation ^ "-" ^ width)
          in
          List.iter
            (fun prefix ->
              List.iter
                (fun (name, expected) ->
                  Alcotest.(check string)
                    "original global immediate update" (to_string expected)
                    (List.assoc (prefix ^ name) fields))
                [
                  ("S", signed);
                  ("U", unsigned);
                  ("SN", signed);
                  ("UN", unsigned);
                ])
            [ "J_"; "A_" ])
        [ ("add", neg (div n 2L), 0L); ("sub", sub (div n 2L) 1L, sub n 1L) ])
    [ ("byte", 8); ("16", 16); ("32", 32) ];
  List.iter
    (fun (stem, values) ->
      let fields = fixture stem in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original global compound context" expected
                (List.assoc (prefix ^ name) fields))
            values)
        [ "J_"; "A_" ])
    [
      ( "global-compound-edge",
        [
          ("ADD", "0");
          ("SUB", "255");
          ("A256", "0");
          ("MUL", "510");
          ("SHL", "510");
        ] );
      ("global-compound-context", [ ("NEST", "0"); ("ARG", "0") ]);
      ( "global-compound-variants",
        [ ("DYN", "256"); ("PTR", "256"); ("EXPR", "256") ] );
    ];
  List.iter
    (fun (width, bits) ->
      let open Int64 in
      let n = shift_left 1L bits in
      List.iter
        (fun (label, amount) ->
          List.iter
            (fun operation ->
              let initial_s =
                if operation = "add" then sub (div n 2L) 1L else neg (div n 2L)
              in
              let initial_u = if operation = "add" then sub n 1L else 0L in
              let apply x =
                if operation = "add" then add x amount else sub x amount
              in
              let unsigned x = logand (apply x) (sub n 1L) in
              let signed x =
                let value = unsigned x in
                if value >= div n 2L then sub value n else value
              in
              let fields =
                fixture
                  ("global-immediate-" ^ operation ^ "-" ^ width ^ "-" ^ label)
              in
              List.iter
                (fun prefix ->
                  List.iter
                    (fun (name, expected) ->
                      Alcotest.(check string)
                        "original global literal boundary" (to_string expected)
                        (List.assoc (prefix ^ name) fields))
                    [
                      ("S", signed initial_s);
                      ("U", unsigned initial_u);
                      ("SN", signed initial_s);
                      ("UN", unsigned initial_u);
                    ])
                [ "J_"; "A_" ])
            [ "add"; "sub" ])
        [ ("mid", 256L); ("max", 2147483647L) ])
    [ ("byte", 8); ("16", 16); ("32", 32) ];
  List.iter
    (fun (operation, amount, signed, unsigned) ->
      let fields =
        fixture ("global-immediate-" ^ operation ^ "-byte-imm" ^ amount)
      in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "original byte immediate encoding boundary" expected
                (List.assoc (prefix ^ name) fields))
            [ ("S", signed); ("U", unsigned); ("SN", signed); ("UN", unsigned) ])
        [ "J_"; "A_" ])
    [
      ("add", "127", "-2", "126");
      ("add", "128", "-1", "127");
      ("sub", "127", "1", "129");
      ("sub", "128", "0", "128");
    ];
  let check_nested stem direct_stem outer =
    let fields = fixture stem and direct = fixture direct_stem in
    List.iter
      (fun prefix ->
        List.iter
          (fun name ->
            let expected =
              Int64.add
                (Int64.of_string (List.assoc (prefix ^ name) direct))
                outer
              |> Int64.to_string
            in
            Alcotest.(check string)
              "original nested global update" expected
              (List.assoc (prefix ^ name) fields))
          [ "S"; "U"; "SN"; "UN" ])
      [ "J_"; "A_" ]
  in
  List.iter
    (fun op ->
      List.iter
        (fun width ->
          let nested = "global-nested-" ^ op ^ "-" ^ width in
          let direct = "global-immediate-" ^ op ^ "-" ^ width in
          check_nested nested direct 0L;
          check_nested (nested ^ "-outermax") direct 2147483647L;
          check_nested (nested ^ "-innermid") (direct ^ "-mid") 0L;
          check_nested (nested ^ "-innermax") (direct ^ "-max") 0L)
        [ "byte"; "16"; "32" ])
    [ "add"; "sub" ];
  List.iter
    (fun width ->
      List.iter
        (fun (suffix, amount) ->
          check_nested
            ("global-nested-add-" ^ width ^ "-outer" ^ suffix)
            ("global-immediate-add-" ^ width)
            amount)
        [ ("1", 1L); ("256", 256L) ])
    [ "byte"; "32" ];
  List.iter
    (fun op ->
      List.iter
        (fun width ->
          check_nested
            ("global-subouter-" ^ op ^ "-" ^ width)
            ("global-immediate-" ^ op ^ "-" ^ width)
            (-1L))
        [ "byte"; "16"; "32" ])
    [ "add"; "sub" ];
  let subouter_exe = fixture "global-subouter-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original subtraction in compile-time execution" expected
        (List.assoc name subouter_exe))
    [ ("ADD", "-1"); ("SUB", "254") ];
  let check_multiply stem direct_stem factor =
    let observed = fixture stem and direct = fixture direct_stem in
    List.iter
      (fun prefix ->
        List.iter
          (fun name ->
            let expected =
              Int64.mul
                (Int64.of_string (List.assoc (prefix ^ name) direct))
                factor
              |> Int64.to_string
            in
            Alcotest.(check string)
              "original multiplied global update" expected
              (List.assoc (prefix ^ name) observed))
          [ "S"; "U"; "SN"; "UN" ])
      [ "J_"; "A_" ]
  in
  List.iter
    (fun op ->
      List.iter
        (fun width ->
          let nested = "global-mulouter-" ^ op ^ "-" ^ width in
          let direct = "global-immediate-" ^ op ^ "-" ^ width in
          check_multiply nested direct 2L;
          check_multiply (nested ^ "-innermax") (direct ^ "-max") 2L;
          check_multiply (nested ^ "-outermax") direct 2147483647L)
        [ "byte"; "16"; "32" ])
    [ "add"; "sub" ];
  List.iter
    (fun op ->
      List.iter
        (fun width ->
          check_multiply
            ("global-muldynamic-" ^ op ^ "-" ^ width)
            ("global-immediate-" ^ op ^ "-" ^ width)
            7L)
        [ "byte"; "16"; "32" ])
    [ "add"; "sub" ];
  List.iter
    (fun op ->
      List.iter
        (fun (label, factor) ->
          check_multiply
            ("global-mulunsigned-" ^ op ^ "-" ^ label)
            ("global-immediate-" ^ op ^ "-byte")
            factor)
        [ ("small", 7L); ("max", -1L) ];
      List.iter
        (fun (label, factor) ->
          check_multiply
            ("global-muldynamic-" ^ op ^ "-" ^ label)
            ("global-immediate-" ^ op ^ "-byte")
            factor)
        [ ("negative", -7L); ("high", Int64.max_int) ])
    [ "add"; "sub" ];
  let muldynamic_exe = fixture "global-muldynamic-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original dynamic multiplication in compile-time execution" expected
        (List.assoc name muldynamic_exe))
    [ ("ADD", "0"); ("SUB", "1785") ];
  let mulouter_exe = fixture "global-mulouter-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original multiplied update in compile-time execution" expected
        (List.assoc name mulouter_exe))
    [ ("ADD", "0"); ("SUB", "510") ];
  let check_dynamic stem direct_stem amount =
    let fields = fixture stem and direct = fixture direct_stem in
    List.iter
      (fun prefix ->
        List.iter
          (fun name ->
            let expected =
              Int64.add
                (Int64.of_string (List.assoc (prefix ^ name) direct))
                amount
              |> Int64.to_string
            in
            Alcotest.(check string)
              "original dynamic global addition" expected
              (List.assoc (prefix ^ name) fields))
          [ "S"; "U"; "SN"; "UN" ])
      [ "J_"; "A_" ]
  in
  List.iter
    (fun op ->
      List.iter
        (fun width ->
          check_dynamic
            ("global-nested-dynamic-" ^ op ^ "-" ^ width)
            ("global-immediate-" ^ op ^ "-" ^ width)
            7L)
        [ "byte"; "16"; "32" ];
      List.iter
        (fun width ->
          List.iter
            (fun (label, value) ->
              check_dynamic
                ("global-nested-dynamic-" ^ op ^ "-" ^ width ^ "-" ^ label)
                ("global-immediate-" ^ op ^ "-" ^ width)
                value)
            [ ("negative", -7L); ("high", Int64.max_int) ])
        [ "byte"; "32" ];
      List.iter
        (fun (label, value) ->
          check_dynamic
            ("global-nested-unsigned-" ^ op ^ "-" ^ label)
            ("global-immediate-" ^ op ^ "-byte")
            value)
        [ ("small", 7L); ("max", -1L) ])
    [ "add"; "sub" ];
  List.iter
    (fun op ->
      List.iter
        (fun width ->
          check_dynamic
            ("global-subdynamic-" ^ op ^ "-" ^ width)
            ("global-immediate-" ^ op ^ "-" ^ width)
            (-7L))
        [ "byte"; "16"; "32" ])
    [ "add"; "sub" ];
  List.iter
    (fun op ->
      List.iter
        (fun (label, amount) ->
          check_dynamic
            ("global-subunsigned-" ^ op ^ "-" ^ label)
            ("global-immediate-" ^ op ^ "-byte")
            amount)
        [ ("small", -7L); ("max", 1L) ];
      List.iter
        (fun (label, amount) ->
          check_dynamic
            ("global-subdynamic-" ^ op ^ "-byte-" ^ label)
            ("global-immediate-" ^ op ^ "-byte")
            amount)
        [ ("negative", 7L); ("high", Int64.neg Int64.max_int) ])
    [ "add"; "sub" ];
  let subdynamic_exe = fixture "global-subdynamic-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original global subtraction in compile-time execution" expected
        (List.assoc name subdynamic_exe))
    [ ("ADD", "-7"); ("SUB", "248") ];
  let global_dynamic_exe = fixture "global-nested-dynamic-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original dynamic global compile-time execution" expected
        (List.assoc name global_dynamic_exe))
    [ ("ADD", "7"); ("SUB", "262") ];
  let global_nested_exe = fixture "global-nested-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original nested global update in compile-time execution" expected
        (List.assoc name global_nested_exe))
    [ ("ADD", "1"); ("SUB", "511") ];
  let global_exe = fixture "global-immediate-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original global update in compile-time execution" expected
        (List.assoc name global_exe))
    [ ("ADD", "0"); ("SUB", "255") ];
  let check_global_call stem direct multiplier =
    let observed = fixture stem and expected = fixture direct in
    List.iter
      (fun prefix ->
        List.iter
          (fun name ->
            let value =
              Int64.mul
                (Int64.of_string (List.assoc (prefix ^ name) expected))
                multiplier
              |> Int64.to_string
            in
            Alcotest.(check string)
              "original global update argument" value
              (List.assoc (prefix ^ name) observed))
          [ "S"; "U"; "SN"; "UN" ])
      [ "J_"; "A_" ]
  in
  List.iter
    (fun op ->
      List.iter
        (fun width ->
          List.iter
            (fun suffix ->
              let direct_suffix = if suffix = "-unsigned" then "" else suffix in
              check_global_call
                ("global-call-" ^ op ^ "-" ^ width ^ suffix)
                ("global-immediate-" ^ op ^ "-" ^ width ^ direct_suffix)
                1L)
            [ ""; "-mid"; "-max"; "-unsigned" ])
        [ "byte"; "16"; "32" ])
    [ "add"; "sub" ];
  check_global_call "global-call-double" "global-immediate-add-byte" 2L;
  let call_exe = fixture "global-call-exe" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "original global call in compile-time execution" expected
        (List.assoc name call_exe))
    [ ("ADD", "0"); ("SUB", "255") ];
  let postfix_default = fixture "postfix-return-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained original postfix return" expected
        (List.assoc name postfix_default))
    [ ("PIN", "127"); ("PDEC", "-128"); ("LIN", "255"); ("LDEC", "255") ];
  let prefix_default = fixture "prefix-return-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained original prefix return" expected
        (List.assoc name prefix_default))
    [ ("PIN", "128"); ("PDEC", "-129"); ("LIN", "256"); ("LDEC", "254") ];
  let unary_default = fixture "unary-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained original unary update" expected
        (List.assoc name unary_default))
    [ ("PIN", "128"); ("PDEC", "-129"); ("LIN", "256"); ("LDEC", "254") ];
  let unsigned_parameter = fixture "local-param-unsigned" in
  List.iter
    (fun prefix ->
      List.iter
        (fun (name, expected) ->
          Alcotest.(check string)
            "unsigned parameter local" expected
            (List.assoc (prefix ^ name) unsigned_parameter))
        [ ("S", "-1"); ("U", "-1"); ("SN", "-1"); ("UN", "255") ])
    [ "J_"; "A_" ];
  let param_default = fixture "local-param-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained parameter local" expected
        (List.assoc name param_default))
    [ ("SIGNED", "255"); ("UNSIGNED", "256") ];
  let div_zero = fixture "div-zero" in
  List.iter
    (fun name ->
      Alcotest.(check string)
        "original division fault" "1" (List.assoc name div_zero))
    [ "J_CAUGHT"; "A_CAUGHT" ];
  let div_default = fixture "div-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained parameter division" expected
        (List.assoc name div_default))
    [ ("SIGNED", "128"); ("UNSIGNED", "127") ];
  let bit_default = fixture "bit-default" in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        "retained bitwise" expected
        (List.assoc name bit_default))
    [ ("OR", "383"); ("XOR", "383"); ("AND", "255") ];
  List.iter
    (fun (operation, byte_values, narrow_values, wide_values, memory_values) ->
      let byte = fixture ("bit-" ^ operation ^ "-byte") in
      let narrow_count = fixture ("bit-" ^ operation ^ "-narrow-count") in
      let wide = fixture ("bit-" ^ operation ^ "-wide") in
      let noreg = fixture ("bit-" ^ operation ^ "-wide-noreg") in
      List.iter
        (fun prefix ->
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "byte bitwise" expected
                (List.assoc (prefix ^ name) byte))
            byte_values;
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "narrow RHS bitwise" expected
                (List.assoc (prefix ^ name) narrow_count))
            narrow_values;
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "wide bitwise" expected
                (List.assoc (prefix ^ name) wide))
            wide_values;
          List.iter
            (fun (name, expected) ->
              Alcotest.(check string)
                "noreg wide bitwise" expected
                (List.assoc (prefix ^ name) noreg))
            memory_values)
        [ "J_"; "A_" ])
    [
      ( "or",
        [ ("S", "383"); ("U", "511"); ("SN", "127"); ("UN", "255") ],
        [ ("S", "127"); ("U", "255"); ("SN", "127"); ("UN", "255") ],
        [
          ("S16", "98303");
          ("U16", "131071");
          ("S32", "6442450943");
          ("U32", "8589934591");
        ],
        [
          ("S16", "32767");
          ("U16", "65535");
          ("S32", "2147483647");
          ("U32", "4294967295");
        ] );
      ( "xor",
        [ ("S", "383"); ("U", "511"); ("SN", "127"); ("UN", "255") ],
        [ ("S", "127"); ("U", "255"); ("SN", "127"); ("UN", "255") ],
        [
          ("S16", "98303");
          ("U16", "131071");
          ("S32", "6442450943");
          ("U32", "8589934591");
        ],
        [
          ("S16", "32767");
          ("U16", "65535");
          ("S32", "2147483647");
          ("U32", "4294967295");
        ] );
      ( "and",
        [ ("S", "255"); ("U", "255"); ("SN", "-1"); ("UN", "255") ],
        [ ("S", "-1"); ("U", "255"); ("SN", "-1"); ("UN", "255") ],
        [
          ("S16", "65535");
          ("U16", "65535");
          ("S32", "4294967295");
          ("U32", "4294967295");
        ],
        [
          ("S16", "-1"); ("U16", "65535"); ("S32", "-1"); ("U32", "4294967295");
        ] );
    ];
  let option = fixture "option" in
  Alcotest.(check string)
    "original option enabled" "1"
    (List.assoc "AFTER" option);
  Alcotest.(check string)
    "original option-case result" "254"
    (List.assoc "AOT_NOOPT" option);
  let pressure = fixture "pressure" in
  List.iter
    (fun prefix ->
      Alcotest.(check string)
        "original low pressure" "254"
        (List.assoc (prefix ^ "SMALL") pressure);
      Alcotest.(check string)
        "original high pressure" "-2"
        (List.assoc (prefix ^ "LARGE") pressure))
    [ "JIT_"; "AOT_" ]

let simple_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (base, qualifier, expected) ->
          let source =
            "I64 F(" ^ base ^ " " ^ qualifier ^ " x,I64 c){x<<=c;return x;}42;"
          in
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check bool)
            "original bounded parameter selection" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some expected))
        [
          ("I8", "", Analysis.Register_candidate);
          ("I8", "noreg", Analysis.Stack);
          ("U8", "", Analysis.Register_candidate);
          ("U8", "noreg", Analysis.Stack);
          ("I16", "", Analysis.Register_candidate);
          ("I16", "noreg", Analysis.Stack);
          ("U16", "", Analysis.Register_candidate);
          ("U16", "noreg", Analysis.Stack);
          ("I32", "", Analysis.Register_candidate);
          ("I32", "noreg", Analysis.Stack);
          ("U32", "", Analysis.Register_candidate);
          ("U32", "noreg", Analysis.Stack);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let remainder_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun ty ->
          List.iter
            (fun (qualifier, expected) ->
              let fn =
                compile mode
                  ("I64 F(" ^ ty ^ " " ^ qualifier
                 ^ " x,I64 c){x%=c;return x;}42;")
              in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let symbol = Frame.location_symbol (location fn.frame "x") in
              Alcotest.(check bool)
                "single remainder selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some expected))
            [
              ("", Analysis.Register_candidate);
              ("noreg", Analysis.Stack);
              ("reg R12", Analysis.Register_candidate);
            ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "remainder proof rejects aliases and effects" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 c){I8 *p=&x;x%=c;return x;}42;";
          "I64 F(I8 reg RAX x,I64 c){x%=c;return x;}42;";
          "I64 F(I8 reg R12 x,I64 reg R12 c){x%=c;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let repeated_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun ty ->
          List.iter
            (fun (qualifier, expected) ->
              List.iter
                (fun count ->
                  List.iter
                    (fun op ->
                      let statements =
                        String.concat ""
                          (List.init count (fun _ -> "x" ^ op ^ "=c;"))
                      in
                      let fn =
                        compile mode
                          ("I64 F(" ^ ty ^ " " ^ qualifier ^ " x,I64 c){"
                         ^ statements ^ "return x;}42;")
                      in
                      let proof =
                        Option.get
                          (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                      in
                      let symbol =
                        Frame.location_symbol (location fn.frame "x")
                      in
                      Alcotest.(check bool)
                        "repeated updates retain selection" true
                        (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                           ~symbol
                        = Some expected))
                    [ "+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "|"; "^" ])
                [ 2; 3; 4; 5 ])
            [
              ("", Analysis.Register_candidate);
              ("noreg", Analysis.Stack);
              ("reg R12", Analysis.Register_candidate);
            ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "interleaved repeated updates" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,U16 y,I64 c){x+=c;y*=c;x-=c;return x+y;}42;";
          "I64 F(I8 x,I64 c,I64 d){x+=c;x*=d;x-=c;return x+x;}42;";
        ];
      let fn = compile mode "I64 F(I8 x,I64 c,I64 d){x+=c;x*=d;return x;}42;" in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = Frame.location_symbol (location fn.frame "x") in
      let updates =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
        |> List.filter (fun (item : Seq.description) ->
            item.opcode = Holyc_lib__Ir.Opcode.Ic_add_equ
            || item.opcode = Holyc_lib__Ir.Opcode.Ic_mul_equ)
      in
      let first = List.hd updates and second = List.nth updates 1 in
      let original = second.operands in
      let substituted = [ List.hd original; List.nth first.operands 1 ] in
      Obj.set_field (Obj.repr second) 2 (Obj.repr substituted);
      Alcotest.(check bool)
        "repeated update substitution invalidates witness" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "repeated update cannot acquire foreign operand" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      Obj.set_field (Obj.repr second) 2 (Obj.repr original);
      Alcotest.(check bool)
        "original repeated update witness remains valid" true
        (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
        = Some Analysis.Register_candidate);
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "whole-function repeated updates are selected" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 c){x+=c;x+=x;return x;}42;";
          "I64 F(I8 x,I64 c){x+=c;c+=x;x*=c;return x;}42;";
          "I64 F(I8 x,I64 c){x+=c;x+=c;x+=c;x+=c;x+=c;x+=c;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let many_parameter_selection () =
  let signatures =
    [
      ("I64 a,", ",I64 c");
      ("I64 a,", ",I64 d,I64 c");
      ("I64 a,I64 b,", ",I64 d,I64 c");
      ("I64 a,I64 b,", ",I64 d,I64 e,I64 c");
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (prefix, suffix) ->
          List.iter
            (fun (ty, qualifier, expected) ->
              let parameters = prefix ^ ty ^ " " ^ qualifier ^ " x" ^ suffix in
              let fn =
                compile mode ("I64 F(" ^ parameters ^ "){x<<=c;return x;}42;")
              in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let symbol = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "multi-parameter register selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some expected))
            [
              ("I8", "", Analysis.Register_candidate);
              ("U8", "", Analysis.Register_candidate);
              ("I8", "noreg", Analysis.Stack);
              ("U8", "noreg", Analysis.Stack);
            ])
        signatures;
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "multi-parameter requested register proved" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I64 a,I64 b,I8 reg R12 x,I64 d,I64 e,I64 c){x<<=c;return \
           x;}42;";
          "I64 F(I64 a,I64 b,I8 x,I64 d,I64 e,I64 reg R12 c){x<<=c;return \
           x;}42;";
        ];
      let fn =
        compile mode
          "I64 F(I64 a,I64 b,I64 d,I64 e,I64 f,I8 x,I64 c){x<<=c;return x;}42;"
      in
      Alcotest.(check bool)
        "unused parameters do not consume the variable bank" true
        (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let pair_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (qx, qy, sx, sy) ->
          let source =
            Printf.sprintf
              "I64 F(I8 %s x,I8 %s y,I64 c){x+=c;y+=c;return x+y;}42;" qx qy
          in
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          List.iter
            (fun (name, expected) ->
              let symbol = location fn.frame name |> Frame.location_symbol in
              Alcotest.(check bool)
                "two-update selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some expected))
            [ ("x", sx); ("y", sy) ])
        [
          ("", "", Analysis.Register_candidate, Analysis.Register_candidate);
          ("noreg", "noreg", Analysis.Stack, Analysis.Stack);
          ("", "noreg", Analysis.Register_candidate, Analysis.Stack);
          ( "reg R12",
            "reg R13",
            Analysis.Register_candidate,
            Analysis.Register_candidate );
        ];
      List.iter
        (fun first ->
          List.iter
            (fun second ->
              let source =
                Printf.sprintf
                  "I64 F(I8 x,I8 y,I64 c){x%s=c;y%s=c;return x+y;}42;" first
                  second
              in
              let fn = compile mode source in
              Alcotest.(check bool)
                "two-update arithmetic proved" true
                (Option.is_some
                   (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
            [ "+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "|"; "^" ])
        [ "+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "|"; "^" ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "two-update independent operands proved" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I8 y,I64 c,I64 d){x+=c;y+=d;return x+y;}42;";
          "I64 F(I8 x,I8 y,I64 reg R12 c,I64 reg R13 d){x+=c;y+=d;return \
           x+y;}42;";
        ];
      List.iter
        (fun (left, right) ->
          let fn =
            compile mode
              (Printf.sprintf "I64 F(%s x,%s y,I64 c){x+=c;y+=c;return x+y;}42;"
                 left right)
          in
          Alcotest.(check bool)
            "mixed-signedness update pair" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          ("I8", "U8");
          ("U8", "I8");
          ("I16", "U16");
          ("U16", "I16");
          ("I32", "U32");
          ("U32", "I32");
        ];
      List.iter
        (fun (left, right) ->
          let fn =
            compile mode
              (Printf.sprintf "I64 F(%s x,%s y,I64 c){x+=c;y+=c;return x+y;}42;"
                 left right)
          in
          Alcotest.(check bool)
            "mixed-width update pair" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          ("I8", "U16");
          ("U16", "I8");
          ("I16", "U32");
          ("U32", "I16");
          ("I8", "I16");
          ("I16", "I8");
          ("U16", "U32");
          ("U32", "U16");
          ("I8", "I64");
          ("I64", "I8");
          ("U16", "U64");
          ("U64", "U16");
          ("I32", "U64");
          ("U64", "I32");
        ];
      let six =
        compile mode
          "I64 F(I64 a,I8 x,I64 b,I8 y,I64 c,I64 d){x+=c;y+=c;return x+y;}42;"
      in
      Alcotest.(check bool)
        "six-parameter two-update selection" true
        (Option.is_some (Analysis.analyze ~frame:six.frame ~body:six.body));
      let fn =
        compile mode "I64 F(I8 x,I8 y,I64 c){x+=c;y+=c;return x+y;}42;"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      let sum =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
        |> List.find (fun (item : Seq.description) ->
            item.opcode = Holyc_lib__Ir.Opcode.Ic_add
            && Option.fold ~none:false
                 ~some:(fun ty -> Semantic_type.pointer_depth ty = 0)
                 item.target_type)
      in
      Obj.set_field (Obj.repr sum) 5 (Obj.repr (Some (Seq.Integer 1L)));
      Alcotest.(check bool)
        "changed two-update graph rejected" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Obj.set_field (Obj.repr sum) 5 (Obj.repr None);
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "unproved two-update shape" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [ "I64 F(I8 reg R12 x,I8 reg R12 y,I64 c){x+=c;y+=c;return x+y;}42;" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let sequence_selection () =
  let all = [ "x"; "y"; "z"; "w"; "v" ] in
  List.iter
    (fun mode ->
      List.iter
        (fun count ->
          let names = List.filteri (fun index _ -> index < count) all in
          List.iter
            (fun (ty, qualifier, expected) ->
              let parameters =
                names
                |> List.map (fun name -> ty ^ " " ^ qualifier ^ " " ^ name)
                |> String.concat ","
              in
              let updates =
                names
                |> List.map (fun name -> name ^ "+=c;")
                |> String.concat ""
              in
              let source =
                "I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
                ^ String.concat "+" names ^ ";}42;"
              in
              let fn = compile mode source in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              List.iter
                (fun name ->
                  let symbol =
                    location fn.frame name |> Frame.location_symbol
                  in
                  Alcotest.(check bool)
                    "multi-update register selection" true
                    (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                       ~symbol
                    = Some expected))
                names)
            [
              ("I8", "", Analysis.Register_candidate);
              ("U8", "", Analysis.Register_candidate);
              ("I8", "noreg", Analysis.Stack);
              ("U8", "noreg", Analysis.Stack);
            ])
        [ 3; 4; 5 ];
      List.iter
        (fun count ->
          let names = List.filteri (fun index _ -> index < count) all in
          List.iter
            (fun ty ->
              List.iter
                (fun op ->
                  let parameters =
                    names
                    |> List.map (fun name -> ty ^ " " ^ name)
                    |> String.concat ","
                  in
                  let updates =
                    names
                    |> List.map (fun name -> name ^ op ^ "=c;")
                    |> String.concat ""
                  in
                  let fn =
                    compile mode
                      ("I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
                     ^ String.concat "+" names ^ ";}42;")
                  in
                  Alcotest.(check bool)
                    "matching update operators" true
                    (Option.is_some
                       (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
                [ "+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "|"; "^" ])
            [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ])
        [ 3; 4; 5 ];
      List.iter
        (fun count ->
          let names = List.filteri (fun index _ -> index < count) all in
          List.iter
            (fun ty ->
              List.iter
                (fun pattern ->
                  let ops =
                    List.filteri (fun index _ -> index < count) pattern
                  in
                  let parameters =
                    names
                    |> List.map (fun name -> ty ^ " " ^ name)
                    |> String.concat ","
                  in
                  let updates =
                    List.map2 (fun name op -> name ^ op ^ "=c;") names ops
                    |> String.concat ""
                  in
                  let fn =
                    compile mode
                      ("I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
                     ^ String.concat "+" names ^ ";}42;")
                  in
                  Alcotest.(check bool)
                    "mixed update operators" true
                    (Option.is_some
                       (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
                [
                  [ "+"; "-"; "*"; "<<"; "^" ];
                  [ "/"; ">>"; "&"; "|"; "+" ];
                  [ "^"; "&"; "*"; ">>"; "/" ];
                ])
            [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ])
        [ 3; 4; 5 ];
      let explicit =
        compile mode
          "I64 F(U8 reg R12 x,U8 reg R13 y,U8 reg R14 z,U8 reg R15 w,U8 noreg \
           v,I64 c){x+=c;y-=c;z*=c;w<<=c;v*=c;return x+y+z+w+v;}42;"
      in
      let proof =
        Option.get (Analysis.analyze ~frame:explicit.frame ~body:explicit.body)
      in
      List.iter
        (fun (name, expected) ->
          let symbol = location explicit.frame name |> Frame.location_symbol in
          Alcotest.(check bool)
            "mixed explicit register selection" true
            (Analysis.selection proof ~frame:explicit.frame ~body:explicit.body
               ~symbol
            = Some expected))
        [
          ("x", Analysis.Register_candidate);
          ("y", Analysis.Register_candidate);
          ("z", Analysis.Register_candidate);
          ("w", Analysis.Register_candidate);
          ("v", Analysis.Stack);
        ];
      let independent_explicit =
        compile mode
          "I64 F(U8 reg R12 x,U8 reg R13 y,U8 noreg z,I64 c,I64 d,I64 \
           e){x+=c;y*=d;z<<=e;return x+y+z;}42;"
      in
      let independent_proof =
        Option.get
          (Analysis.analyze ~frame:independent_explicit.frame
             ~body:independent_explicit.body)
      in
      List.iter
        (fun (name, expected) ->
          let symbol =
            location independent_explicit.frame name |> Frame.location_symbol
          in
          Alcotest.(check bool)
            "independent explicit register selection" true
            (Analysis.selection independent_proof
               ~frame:independent_explicit.frame ~body:independent_explicit.body
               ~symbol
            = Some expected))
        [
          ("x", Analysis.Register_candidate);
          ("y", Analysis.Register_candidate);
          ("z", Analysis.Stack);
        ];
      List.iter
        (fun ty ->
          List.iter
            (fun source ->
              let fn = compile mode source in
              Alcotest.(check bool)
                "independent update operands" true
                (Option.is_some
                   (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
            [
              Printf.sprintf
                "I64 F(%s x,%s y,%s z,I64 c,I64 d,I64 \
                 e){x+=c;y*=d;z<<=e;return x+y+z;}42;"
                ty ty ty;
              Printf.sprintf
                "I64 F(%s x,%s y,%s z,%s w,I64 c,I64 \
                 d){x+=c;y-=d;z*=c;w<<=d;return x+y+z+w;}42;"
                ty ty ty ty;
            ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun types ->
          let names =
            List.filteri (fun index _ -> index < List.length types) all
          in
          let parameters =
            List.map2 (fun ty name -> ty ^ " " ^ name) types names
            |> String.concat ","
          in
          let updates =
            names |> List.map (fun name -> name ^ "+=c;") |> String.concat ""
          in
          let fn =
            compile mode
              ("I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
             ^ String.concat "+" names ^ ";}42;")
          in
          Alcotest.(check bool)
            "mixed narrow update types" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          [ "I8"; "U16"; "I32" ];
          [ "I32"; "U16"; "I8" ];
          [ "U8"; "I16"; "U32" ];
          [ "U32"; "I16"; "U8" ];
          [ "I8"; "U16"; "I32"; "U32" ];
          [ "U32"; "I32"; "U16"; "I8" ];
        ];
      let mixed_type_explicit =
        compile mode
          "I64 F(I8 reg R12 x,U16 reg R13 y,I32 reg R14 z,U32 reg R15 w,U8 \
           noreg v,I64 c){x+=c;y-=c;z*=c;w<<=c;v*=c;return x+y+z+w+v;}42;"
      in
      let mixed_type_proof =
        Option.get
          (Analysis.analyze ~frame:mixed_type_explicit.frame
             ~body:mixed_type_explicit.body)
      in
      List.iter
        (fun (name, expected) ->
          let symbol =
            location mixed_type_explicit.frame name |> Frame.location_symbol
          in
          Alcotest.(check bool)
            "mixed type explicit register selection" true
            (Analysis.selection mixed_type_proof
               ~frame:mixed_type_explicit.frame ~body:mixed_type_explicit.body
               ~symbol
            = Some expected))
        [
          ("x", Analysis.Register_candidate);
          ("y", Analysis.Register_candidate);
          ("z", Analysis.Register_candidate);
          ("w", Analysis.Register_candidate);
          ("v", Analysis.Stack);
        ];
      let fullwidth_explicit =
        compile mode
          "I64 F(I8 reg R12 x,U16 reg R13 y,I32 reg R14 z,U32 reg R15 w,I64 \
           noreg v,I64 c){x+=c;y-=c;z*=c;w<<=c;v*=c;return x+y+z+w+v;}42;"
      in
      let fullwidth_proof =
        Option.get
          (Analysis.analyze ~frame:fullwidth_explicit.frame
             ~body:fullwidth_explicit.body)
      in
      List.iter
        (fun (name, expected) ->
          let symbol =
            location fullwidth_explicit.frame name |> Frame.location_symbol
          in
          Alcotest.(check bool)
            "full-width explicit register selection" true
            (Analysis.selection fullwidth_proof ~frame:fullwidth_explicit.frame
               ~body:fullwidth_explicit.body ~symbol
            = Some expected))
        [
          ("x", Analysis.Register_candidate);
          ("y", Analysis.Register_candidate);
          ("z", Analysis.Register_candidate);
          ("w", Analysis.Register_candidate);
          ("v", Analysis.Stack);
        ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "full-width and narrow update sequence" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return z+y+x;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return x+y+x;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return x+z;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return z;}42;";
          "I64 F(I8 x,I64 y,I64 c){x+=c;y+=c;return x;}42;";
          "I64 F(I8 x,I64 y,I64 c){x+=c;y+=c;return y;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return x+y+z+x;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return x+y+z+x+y;}42;";
          "I64 F(I8 x,I8 y,I8 z,I8 w,I64 c){x+=c;y+=c;z+=c;w+=c;return x+w;}42;";
          "I64 F(I64 x,I8 y,U16 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+y+z;}42;";
          "I64 F(I8 x,I64 y,U16 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+y+z;}42;";
          "I64 F(I8 x,U16 y,I64 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+y+z;}42;";
          "I64 F(I8 x,U16 y,I32 z,I64 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return x+y+z+w;}42;";
          "I64 F(I64 x,I32 y,U16 z,I8 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return x+y+z+w;}42;";
          "I64 F(I8 x,U16 y,I32 z,U32 w,I64 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return x+y+z+w+v;}42;";
          "I64 F(I64 x,U32 y,I32 z,U16 w,I8 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return x+y+z+w+v;}42;";
        ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "mixed narrow types and update operators" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,U16 y,I32 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+y+z;}42;";
          "I64 F(I32 x,U16 y,I8 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+y+z;}42;";
          "I64 F(U8 x,I16 y,U32 z,I8 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return x+y+z+w;}42;";
          "I64 F(I8 x,U32 y,I16 z,U8 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return x+y+z+w;}42;";
          "I64 F(I8 x,U16 y,I32 z,U32 w,U8 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return x+y+z+w+v;}42;";
          "I64 F(U8 x,U32 y,I32 z,U16 w,I8 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return x+y+z+w+v;}42;";
        ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "whole-function update sequence is selected" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I64 x,I64 y,I64 z,I64 c){x+=c;y+=c;z+=c;return x+y+z;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return x+y+c;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return c;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;z+=c;return x+y+z+x+y+z+x;}42;";
          "I64 F(I8 x,I8 y,I8 z,I8 w,I64 c,I64 d){x-=c;y-=c;z-=c;w+=x;return \
           x+y+z+w;}42;";
          "I64 F(I8 x,I8 y,I8 z,I64 c){x+=c;y+=c;c+=c;return x+y+z;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let explicit_register_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun reg ->
          List.iter
            (fun source ->
              let fn = compile mode source in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let symbol = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "explicit register candidate" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some Analysis.Register_candidate))
            [
              Printf.sprintf "I64 F(I8 reg %s x,I64 c){x<<=c;return x;}42;" reg;
              Printf.sprintf "I64 F(){I8 reg %s x=127;x<<=1;return x;}42;" reg;
            ])
        [ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15" ];
      List.iter
        (fun reg ->
          let fn =
            compile mode
              (Printf.sprintf "I64 F(I8 x,I64 reg %s c){x<<=c;return x;}42;" reg)
          in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let symbol = location fn.frame "c" |> Frame.location_symbol in
          Alcotest.(check bool)
            "explicit unchanged operand proved" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
            = Some Analysis.Register_candidate))
        [ "RSI"; "RDI"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15" ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "unsafe register request remains unproved" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 reg RAX x,I64 c){x<<=c;return x;}42;";
          "I64 F(I8 reg R12 x,I64 reg R12 c){x<<=c;return x;}42;";
          "I64 F(){I8 reg RAX x=127;x<<=1;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let physical_register_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (register, number) ->
          List.iter
            (fun type_ ->
              List.iter
                (fun source ->
                  let fn = compile mode source in
                  let proof =
                    Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                  in
                  let symbol = location fn.frame "x" |> Frame.location_symbol in
                  Alcotest.(check (option int))
                    "selected original explicit member" (Some number)
                    (Analysis.physical_register proof ~frame:fn.frame
                       ~body:fn.body ~symbol))
                [
                  Printf.sprintf
                    "I64 F(%s reg %s x,I64 c){if(c)x+=1;return x;}42;" type_
                    register;
                  Printf.sprintf
                    "I64 F(I64 c){%s reg %s x=127;if(c)x+=1;return x;}42;" type_
                    register;
                ])
            [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ])
        [
          ("RSI", 6);
          ("RDI", 7);
          ("R10", 10);
          ("R11", 11);
          ("R12", 12);
          ("R13", 13);
          ("R14", 14);
          ("R15", 15);
        ];
      List.iter
        (fun qualifier ->
          let fn =
            compile mode
              (Printf.sprintf "I64 F(I8 %s x,I64 c){if(c)x+=1;return x;}42;"
                 qualifier)
          in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let symbol = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check (option int))
            "whole-function scores grant an ordinary physical register"
            (if qualifier = "noreg" then None else Some 6)
            (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
               ~symbol))
        [ ""; "noreg" ];
      let fn = compile mode "I64 F(I8 x,I64 reg R12 c){x<<=c;return x;}42;" in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "c" |> Frame.location_symbol in
      Alcotest.(check bool)
        "exact proof authenticates unchanged explicit operand" true
        (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
        = Some Analysis.Register_candidate);
      Alcotest.(check (option int))
        "authenticated used explicit operand keeps requested register" (Some 12)
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let undefined_suffix_register_selection () =
  List.iter
    (fun mode ->
      let fn =
        compile ~name:"Call" mode
          "extern I64 F();I64 (*p)()=&F;I64 Indirect(){p();return 0;}I64 \
           Call(I64 reg R11 x){x+=1;Indirect();return x;}Call(0);"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      Alcotest.(check (option int))
        "undefined-slot regression holds a physical source value in R11"
        (Some 11)
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol))
    (* Unresolved source addresses are a JIT-only operation. AOT rejects them
     before native dispatch. *)
    [ Preprocessor.Jit ]

let automatic_physical_register_selection () =
  let check ?(proved = true) mode label expected source =
    let fn = compile mode source in
    let symbol = location fn.frame "x" |> Frame.location_symbol in
    let proof = Analysis.analyze ~frame:fn.frame ~body:fn.body in
    if proved then
      Alcotest.(check bool)
        (label ^ " has a selection witness")
        true (Option.is_some proof);
    Alcotest.(check (option int))
      label expected
      (Option.bind proof (fun proof ->
           Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
             ~symbol))
  in
  let reservations registers =
    List.mapi
      (fun index register -> Printf.sprintf "I64 reg %s r%d" register index)
      registers
  in
  List.iter
    (fun mode ->
      List.iter
        (fun type_ ->
          List.iter
            (fun qualifier ->
              List.iter
                (fun source ->
                  check mode "sole automatic integer member" (Some 6) source)
                [
                  Printf.sprintf "I64 F(%s %s x){x+=1;return x;}42;" type_
                    qualifier;
                  Printf.sprintf "I64 F(){%s %s x=1;x+=1;return x;}42;" type_
                    qualifier;
                  Printf.sprintf
                    "I64 F(%s %s x,I64 noreg c){if(c)x+=1;return x;}42;" type_
                    qualifier;
                  Printf.sprintf
                    "I64 F(I64 noreg c){%s %s x=1;if(c)x+=1;return x;}42;" type_
                    qualifier;
                ])
            [ ""; "reg" ];
          List.iter
            (fun source ->
              check mode "sole forced member outranks ordinary uses" (Some 6)
                source)
            [
              Printf.sprintf "I64 F(%s reg x,I64 c){if(c)x+=1;return x;}42;"
                type_;
              Printf.sprintf "I64 F(I64 c){%s reg x=1;if(c)x+=1;return x;}42;"
                type_;
            ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ];
      List.iter
        (fun qualifier ->
          List.iter
            (fun (registers, expected) ->
              let parameters = reservations registers in
              check
                ~proved:(List.length registers < 6)
                mode "unused explicit parameters reserve the first general slot"
                expected
                (Printf.sprintf "I64 F(%s){x+=1;return x;}42;"
                   (String.concat ","
                      (("I8 " ^ qualifier ^ " x") :: parameters)));
              check
                ~proved:(List.length registers < 6)
                mode "unused explicit locals reserve the first general slot"
                expected
                (Printf.sprintf "I64 F(){%s I8 %s x=1;x+=1;return x;}42;"
                   (String.concat "" (List.map (fun p -> p ^ "=0;") parameters))
                   qualifier))
            [
              ([], Some 6);
              ([ "RSI" ], Some 7);
              ([ "RSI"; "RDI" ], Some 10);
              ([ "RSI"; "RDI"; "R10" ], Some 11);
              ([ "RSI"; "RDI"; "R10"; "R11" ], Some 14);
              ([ "RSI"; "RDI"; "R10"; "R11"; "R14" ], Some 15);
              ([ "RSI"; "RDI"; "R10"; "R11"; "R14"; "R15" ], None);
              ([ "R12"; "R13" ], Some 6);
              ([ "RDI"; "R10"; "R11"; "R14"; "R15" ], Some 6);
            ])
        [ ""; "reg" ];
      List.iter
        (fun (label, expected, source) -> check mode label expected source)
        [
          ( "ordinary scores count target and operand",
            Some 6,
            "I64 F(I8 x,I64 c){x<<=c;return x;}42;" );
          ( "sole forced member precedes unknown ordinary operand",
            Some 6,
            "I64 F(I8 reg x,I64 c){x<<=c;return x;}42;" );
          ( "unknown explicit operand still reserves RSI",
            Some 7,
            "I64 F(I8 x,I64 reg RSI c){x<<=c;return x;}42;" );
          ( "unknown disabled operand does not compete",
            Some 6,
            "I64 F(I8 x,I64 noreg c){x<<=c;return x;}42;" );
          ( "forced scores count target and operand",
            Some 6,
            "I64 F(I8 reg x,I64 reg c){x<<=c;return x;}42;" );
          ( "zero-score ordinary parameter sorts then disappears",
            Some 6,
            "I64 F(I8 x,I64 unused){x+=1;return x;}42;" );
          ( "used forced score precedes unused forced seed",
            Some 6,
            "I64 F(I8 reg x,I64 reg unused){x+=1;return x;}42;" );
          ( "unused forced seed precedes ordinary target",
            Some 7,
            "I64 F(I8 x,I64 reg unused){x+=1;return x;}42;" );
          ( "ordinary occurrence scores select highest member",
            Some 6,
            "I64 F(I8 x,I64 c){x+=1;x+=1;x+=1;return x+c;}42;" );
          ( "forced occurrence scores select highest member",
            Some 6,
            "I64 F(I8 reg x,I64 reg c){x+=1;x+=1;return x+c;}42;" );
          ( "disabled target stays on the stack",
            None,
            "I64 F(I8 noreg x){x+=1;return x;}42;" );
          ( "unused target remains unknown",
            None,
            "I64 F(I8 reg x){return 1;}42;" );
        ];
      List.iter
        (fun count ->
          let ordinary =
            List.init count (fun index -> Printf.sprintf "I64 p%d" index)
          in
          List.iter
            (fun used ->
              let rhs =
                if used then
                  "x+"
                  ^ String.concat "+"
                      (List.init count (fun index -> Printf.sprintf "p%d" index))
                else "x"
              in
              check mode
                "surviving scored candidates determine nonpointer activation"
                (if (not used) || count = 5 then Some 6 else Some 12)
                (Printf.sprintf "I64 F(%s){x+=1;return %s;}42;"
                   (String.concat "," ("I8 reg x" :: ordinary))
                   rhs))
            [ false; true ])
        [ 5; 6; 8 ];
      let pressured =
        compile mode
          "I64 F(I8 reg x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 reg RSI r0,I64 reg \
           RDI r1,I64 reg R10 r2,I64 reg R11 r3){x+=1;return x+a+b+c+d+e;}42;"
      in
      let pressure_proof =
        Option.get
          (Analysis.analyze ~frame:pressured.frame ~body:pressured.body)
      in
      let x = location pressured.frame "x" |> Frame.location_symbol in
      Alcotest.(check (option int))
        "sole forced candidate uses first free slot under bounded pressure"
        (Some 14)
        (Analysis.physical_register pressure_proof ~frame:pressured.frame
           ~body:pressured.body ~symbol:x);
      List.iter
        (fun name ->
          let symbol = location pressured.frame name |> Frame.location_symbol in
          Alcotest.(check bool)
            "exact pressure winner or spill is proved" true
            (Analysis.selection pressure_proof ~frame:pressured.frame
               ~body:pressured.body ~symbol
            = Some
                (if name = "d" then Analysis.Register_candidate
                 else Analysis.Stack));
          Alcotest.(check (option int))
            "exact pressure placement follows original tie order"
            (if name = "d" then Some 15 else None)
            (Analysis.physical_register pressure_proof ~frame:pressured.frame
               ~body:pressured.body ~symbol))
        [ "a"; "b"; "c"; "d"; "e" ];
      let fn = compile mode "I64 F(I8 reg x,I64 c){x<<=c;return x;}42;" in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "c" |> Frame.location_symbol in
      Alcotest.(check (option int))
        "used ordinary operand receives the second slot" (Some 7)
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let physical_register_unnamed_parameters () =
  let module Headers = Semantic_function_type_resolution in
  List.iter
    (fun mode ->
      List.iter
        (fun qualifier ->
          List.iter
            (fun unnamed ->
              List.iter
                (fun source ->
                  let fn =
                    try Some (compile mode source)
                    with
                    | Failure message
                    when String.ends_with
                           ~suffix:
                             "HCIR0027: function body members do not belong to \
                              its definition header and frame"
                           message
                    ->
                      None
                  in
                  Alcotest.(check bool)
                    "unnamed formal cannot be omitted from selection or \
                     placement"
                    true
                    (Option.fold ~none:true
                       ~some:(fun
                           (fn : Ir_integer_interpreter.function_definition) ->
                         Option.is_none
                           (Analysis.analyze ~frame:fn.frame ~body:fn.body))
                       fn))
                [
                  Printf.sprintf "I64 F(I8 %s x,%s){x+=1;return x;}42;"
                    qualifier unnamed;
                  Printf.sprintf "I64 F(%s,I8 %s x){x+=1;return x;}42;" unnamed
                    qualifier;
                  Printf.sprintf "I64 F(%s){I8 %s x=1;x+=1;return x;}42;"
                    unnamed qualifier;
                ])
            [ "I64"; "I64 reg"; "I64 reg RSI"; "I64 reg R12"; "I64 noreg" ])
        [ ""; "reg"; "reg RSI"; "reg R12" ];
      let pressured =
        compile mode
          "I64 F(I8 x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 reg,I64 reg,I64 \
           reg){x+=1;return x+a+b+c+d+e;}42;"
      in
      Alcotest.(check bool)
        "unnamed forced competitors cannot acquire a false capacity witness"
        true
        (Option.is_none
           (Analysis.analyze ~frame:pressured.frame ~body:pressured.body));
      let omitted = compile mode "I64 F(I8 x,I64 reg RSI){x+=1;return x;}42;" in
      let signature =
        Frame.function_header omitted.frame |> Headers.function_signature
      in
      let parameters = Headers.signature_parameters signature in
      let original_sources =
        Headers.function_source_parameters (Frame.function_header omitted.frame)
        |> Option.get
      in
      Alcotest.(check int)
        "producer retains both original formal children" 2
        (List.length original_sources);
      Obj.set_field (Obj.repr signature) 2 (Obj.repr [ List.hd parameters ]);
      Alcotest.(check int)
        "source receipt is independent of semantic omission" 2
        (List.length
           (Headers.function_source_parameters
              (Frame.function_header omitted.frame)
           |> Option.get));
      Alcotest.(check bool)
        "dropping the semantic unnamed formal cannot hide its original source"
        true
        (Option.is_none
           (Analysis.analyze ~frame:omitted.frame ~body:omitted.body));
      let reordered =
        compile mode "I64 F(I8 reg x,I64 c){x<<=c;return x;}42;"
      in
      let proof =
        Option.get
          (Analysis.analyze ~frame:reordered.frame ~body:reordered.body)
      in
      let symbol = location reordered.frame "x" |> Frame.location_symbol in
      let signature =
        Frame.function_header reordered.frame |> Headers.function_signature
      in
      let parameters = Headers.signature_parameters signature in
      Obj.set_field (Obj.repr signature) 2 (Obj.repr (List.rev parameters));
      Alcotest.(check (option int))
        "changed original formal order revokes physical authority" None
        (Analysis.physical_register proof ~frame:reordered.frame
           ~body:reordered.body ~symbol);
      Alcotest.(check bool)
        "changed formal order cannot acquire a fresh source witness" true
        (Option.is_none
           (Analysis.analyze ~frame:reordered.frame ~body:reordered.body));
      let altered = compile mode "I64 F(I8 reg x,I64 c){x<<=c;return x;}42;" in
      let proof =
        Option.get (Analysis.analyze ~frame:altered.frame ~body:altered.body)
      in
      let symbol = location altered.frame "x" |> Frame.location_symbol in
      Alcotest.(check (option int))
        "complete receipt initially authorizes placement" (Some 6)
        (Analysis.physical_register proof ~frame:altered.frame
           ~body:altered.body ~symbol);
      let sources =
        Headers.function_source_parameters (Frame.function_header altered.frame)
        |> Option.get
      in
      Obj.set_field (Obj.repr sources) 1 (Obj.repr []);
      Alcotest.(check (option int))
        "in-place original receipt change revokes saved physical authority" None
        (Analysis.physical_register proof ~frame:altered.frame
           ~body:altered.body ~symbol);
      Alcotest.(check bool)
        "changed original receipt cannot acquire a fresh witness" true
        (Option.is_none
           (Analysis.analyze ~frame:altered.frame ~body:altered.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let physical_register_parameter_source_constructors () =
  let module Headers = Semantic_function_type_resolution in
  List.iter
    (fun mode ->
      let source = "I64 F(I8 reg x,I64 c){x<<=c;return x;}42;" in
      let fn = compile mode source and foreign = compile mode source in
      let header = Frame.function_header fn.frame in
      let make constructor =
        constructor
          ~symbol:(Headers.function_symbol header)
          ~scope:(Headers.function_scope header)
          ~item_index:(Headers.function_original_item_index header)
          ~return_type:(Headers.function_return_type header)
          ~signature:(Headers.function_signature header)
          ~parameter_bindings:(Headers.function_parameter_bindings header)
          ~variadic_bindings:(Headers.function_variadic_bindings header)
      in
      let originals = Headers.function_source_parameters header |> Option.get in
      Alcotest.(check bool)
        "source-aware constructor accepts exact children" true
        (Result.is_ok
           (make (Headers.make_function_with_source_parameters originals)));
      List.iter
        (fun (label, sources) ->
          Alcotest.(check bool)
            label true
            (Result.is_error
               (make (Headers.make_function_with_source_parameters sources))))
        [
          ( "source-aware constructor rejects missing child",
            [ List.hd originals ] );
          ( "source-aware constructor rejects reordered children",
            List.rev originals );
          ( "source-aware constructor rejects foreign equal source children",
            Headers.function_source_parameters
              (Frame.function_header foreign.frame)
            |> Option.get );
          ( "source-aware constructor rejects duplicated child",
            [ List.hd originals; List.hd originals ] );
        ];
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      let metadata = make Headers.make_function |> Result.get_ok in
      (* Deliberately substitute the metadata-only constructor's record without
         resolving or admitting it as the original source header. A frame using
         this forged header must not recover source authority from shared fields. *)
      Obj.set_field (Obj.repr fn.frame) 1 (Obj.repr metadata);
      Alcotest.(check bool)
        "metadata-only constructor retains no source receipt" true
        (Option.is_none
           (Headers.function_source_parameters (Frame.function_header fn.frame)));
      Alcotest.(check (option int))
        "metadata-only header revokes saved authority" None
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol);
      Alcotest.(check bool)
        "metadata-only header cannot acquire fresh authority" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let compile_option mode source =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"register-options.hc" ~contents:source
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  let report = compile_integer_program_report session ~config ~source in
  let units =
    match integer_program_compilation_result report with
    | Ok result -> (
        match result.value with
        | Isolated program -> [ program ]
        | Stateful _ -> integer_program_compilation_units report)
    | Error diagnostics ->
        failwith
          (String.concat "; "
             (List.map
                (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                diagnostics))
  in
  units
  |> List.concat_map integer_program_functions
  |> List.find (fun (fn : Ir_integer_interpreter.function_definition) ->
      Symbol.name (Body.symbol fn.body) = "F")

let automatic_physical_register_options () =
  List.iter
    (fun mode ->
      List.iter
        (fun bit ->
          let fn =
            compile_option mode
              (Printf.sprintf
                 "#exe {Option(%d,1);}I64 F(I8 x,I8 y){if(y)x+=1;return \
                  x+y;}42;"
                 bit)
          in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          List.iter
            (fun (name, register) ->
              let symbol = location fn.frame name |> Frame.location_symbol in
              Alcotest.(check (option int))
                "allocation-neutral option retains tied source order"
                (Some register)
                (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
                   ~symbol))
            [ ("y", 6); ("x", 7) ])
        [ 0; 1; 33 ];
      List.iter
        (fun (qualifier, expected) ->
          List.iter
            (fun body ->
              let fn =
                compile_option mode ("#exe {Option(34,1);}" ^ body ^ "42;")
              in
              Alcotest.(check bool)
                "source enabled NoReg" true
                (Compiler_option.is_enabled
                   ~mask:(Body.compiler_options fn.body)
                   Compiler_option.No_reg_var);
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let symbol = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check (option int))
                "NoReg disables automatic and forced but preserves explicit"
                expected
                (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
                   ~symbol))
            [
              Printf.sprintf "I64 F(I8 %s x){x+=1;return x;}" qualifier;
              Printf.sprintf "I64 F(){I8 %s x=1;x+=1;return x;}" qualifier;
            ])
        [ ("", None); ("reg", None); ("reg RSI", Some 6) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let automatic_physical_register_ownership () =
  let module Request = Semantic_register_request in
  let module Headers = Semantic_function_type_resolution in
  let module Locals = Semantic_local_type_resolution in
  let source = "I64 F(I8 reg x,I64 c){x<<=c;return x;}42;" in
  let requests frame location =
    match Frame.location_kind location with
    | Frame.Named_parameter ->
        let index =
          Frame.location_binding location
          |> Semantic_function_binding_index.binding_parameter_index
          |> Option.get
        in
        let parameter =
          Frame.function_header frame
          |> Headers.function_signature |> Headers.signature_parameters
          |> List.find (fun parameter ->
              Headers.parameter_index parameter = index)
        in
        (Obj.repr parameter, 3, Headers.parameter_register_requests parameter)
    | Automatic_local ->
        let local = Frame.location_local_source location |> Option.get in
        (Obj.repr local, 8, Locals.local_register_requests local)
    | _ -> failwith "expected source-owned integer member"
  in
  List.iter
    (fun mode ->
      let fn = compile mode source and foreign = compile mode source in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      Alcotest.(check (option int))
        "automatic mapping has original authority" (Some 6)
        (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol);
      Alcotest.(check (option int))
        "automatic mapping rejects foreign frame" None
        (Analysis.physical_register proof ~frame:foreign.frame ~body:fn.body
           ~symbol);
      Alcotest.(check (option int))
        "automatic mapping rejects foreign body" None
        (Analysis.physical_register proof ~frame:fn.frame ~body:foreign.body
           ~symbol);
      List.iter
        (fun source ->
          List.iter
            (fun (label, mutate) ->
              let fn = compile mode source in
              let original = location fn.frame "x" in
              let symbol = Frame.location_symbol original in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              Alcotest.(check (option int))
                (label ^ " begins selected")
                (Some 6)
                (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
                   ~symbol);
              let owner, field, original_requests =
                requests fn.frame original
              in
              mutate owner field original_requests;
              Alcotest.(check (option int))
                (label ^ " invalidates authority")
                None
                (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
                   ~symbol))
            [
              ( "equal reconstructed plain reg owner",
                fun owner field original_requests ->
                  let copies : Request.t list =
                    List.map
                      (fun request -> Obj.obj (Obj.dup (Obj.repr request)))
                      original_requests
                  in
                  Obj.set_field owner field (Obj.repr copies) );
              ( "changed plain reg kind",
                fun _ _ original_requests ->
                  Obj.set_field
                    (Obj.repr (List.hd original_requests))
                    0 (Obj.repr Request.Disable) );
              ( "changed plain reg position",
                fun _ _ original_requests ->
                  Obj.set_field
                    (Obj.repr (List.hd original_requests))
                    1
                    (Obj.repr Request.Before_type) );
              ( "changed plain reg origin",
                fun _ _ original_requests ->
                  Obj.set_field
                    (Obj.repr (List.hd original_requests))
                    3
                    (Obj.repr (Symbol.Synthesized "changed reg owner")) );
              ( "changed plain reg spelling contents",
                fun _ _ original_requests ->
                  let spelling = Request.spelling (List.hd original_requests) in
                  Bytes.set (Bytes.unsafe_of_string spelling) 0 'x' );
            ])
        [ source; "I64 F(I64 c){I8 reg x=1;x+=c;return x;}42;" ];
      List.iter
        (fun (name, replacement) ->
          let fn = compile mode "I64 F(I8 x,I64 c){x<<=c;return x;}42;" in
          let symbol = location fn.frame "x" |> Frame.location_symbol in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          Obj.set_field
            (Obj.repr (location fn.frame name))
            12 (Obj.repr replacement);
          Alcotest.(check (option int))
            "forged request rejects old witness" None
            (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
               ~symbol);
          Alcotest.(check bool)
            "forged reservation, forced priority or exclusion has no fresh \
             source authority"
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          ("c", Request.Disabled);
          ("x", Request.Allocatable);
          ( "c",
            let donor = compile mode "I64 F(I8 reg RSI x){return x;}42;" in
            Frame.location_register_selection (location donor.frame "x") );
        ];
      let explicit =
        compile mode "I64 F(I8 reg R12 x,I64 c){x<<=c;return x;}42;"
      in
      let explicit_location = location explicit.frame "x" in
      let explicit_symbol = Frame.location_symbol explicit_location in
      let explicit_proof =
        Option.get (Analysis.analyze ~frame:explicit.frame ~body:explicit.body)
      in
      let _, _, explicit_requests = requests explicit.frame explicit_location in
      Obj.set_field
        (Obj.repr (List.hd explicit_requests))
        0 (Obj.repr Request.Disable);
      Alcotest.(check bool)
        "malformed selected explicit request invalidates selection without \
         raising"
        true
        (Option.is_none
           (Analysis.selection explicit_proof ~frame:explicit.frame
              ~body:explicit.body ~symbol:explicit_symbol));
      Alcotest.(check (option int))
        "malformed selected explicit request rejects placement without raising"
        None
        (Analysis.physical_register explicit_proof ~frame:explicit.frame
           ~body:explicit.body ~symbol:explicit_symbol);
      Alcotest.(check (option int))
        "malformed semantic request cannot gain authority through reanalysis"
        None
        (Option.bind
           (Analysis.analyze ~frame:explicit.frame ~body:explicit.body)
           (fun proof ->
             Analysis.physical_register proof ~frame:explicit.frame
               ~body:explicit.body ~symbol:explicit_symbol));
      List.iter
        (fun (label, mutate) ->
          let fn =
            compile mode "I64 F(I8 x,I64 reg RSI c){x<<=c;return x;}42;"
          in
          let symbol = location fn.frame "x" |> Frame.location_symbol in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          Alcotest.(check (option int))
            "unknown explicit operand reserves RSI" (Some 7)
            (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
               ~symbol);
          mutate fn;
          Alcotest.(check (option int))
            label None
            (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
               ~symbol))
        [
          ( "malformed unknown reservation rejects without raising",
            fun (fn : Ir_integer_interpreter.function_definition) ->
              let _, _, original_requests =
                requests fn.frame (location fn.frame "c")
              in
              Obj.set_field
                (Obj.repr (List.hd original_requests))
                0 (Obj.repr Request.Disable) );
          ( "changed unknown reservation invalidates placement",
            fun (fn : Ir_integer_interpreter.function_definition) ->
              match
                Frame.location_register_selection (location fn.frame "c")
              with
              | Request.Explicit register ->
                  Obj.set_field (Obj.repr register) 0 (Obj.repr "RDI");
                  Obj.set_field (Obj.repr register) 1 (Obj.repr 7)
              | _ -> assert false );
          ( "changed unknown member ownership invalidates placement",
            fun fn ->
              Obj.set_field
                (Obj.repr (location fn.frame "c"))
                2
                (Obj.repr (Frame.location_symbol (location foreign.frame "c")))
          );
          ( "changed frame displacement invalidates placement",
            fun fn ->
              let slot =
                Frame.location_frame_slot (location fn.frame "c") |> Option.get
              in
              Obj.set_field (Obj.repr slot) 0 (Obj.repr 999L) );
          ( "changed original body invalidates placement",
            fun fn ->
              let instruction =
                Body.body fn.body |> Graph.blocks |> List.hd
                |> Graph.instructions |> Seq.instructions |> List.hd
                |> Seq.description
              in
              Obj.set_field (Obj.repr instruction) 6 (Obj.repr 1L) );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let physical_register_ownership () =
  let module Request = Semantic_register_request in
  let source = "I64 F(I8 reg R12 x,I64 c){x<<=c;return x;}42;" in
  let explicit location =
    match Frame.location_register_selection location with
    | Request.Explicit register -> register
    | _ -> failwith "expected explicit register request"
  in
  List.iter
    (fun mode ->
      let fn = compile mode source and foreign = compile mode source in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      Alcotest.(check (option int))
        "foreign frame cannot reuse physical register authority" None
        (Analysis.physical_register proof ~frame:foreign.frame ~body:fn.body
           ~symbol);
      Alcotest.(check (option int))
        "foreign body cannot reuse physical register authority" None
        (Analysis.physical_register proof ~frame:fn.frame ~body:foreign.body
           ~symbol);
      let foreign_symbol =
        location foreign.frame "x" |> Frame.location_symbol
      in
      let clone =
        Symbol.create ~id:(Symbol.id symbol) ~scope_id:(Symbol.scope_id symbol)
          ~name:(Symbol.name symbol) ~kind:(Symbol.kind symbol)
          ~origin:(Symbol.origin symbol)
      in
      List.iter
        (fun symbol ->
          Alcotest.(check (option int))
            "foreign or reconstructed symbol grants no authority" None
            (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
               ~symbol))
        [ foreign_symbol; clone ];
      let mutations =
        [
          ( "number",
            fun register -> Obj.set_field (Obj.repr register) 1 (Obj.repr 13) );
          ( "spelling",
            fun register -> Obj.set_field (Obj.repr register) 0 (Obj.repr "R13")
          );
          ( "canonical number and spelling",
            fun register ->
              Obj.set_field (Obj.repr register) 0 (Obj.repr "R13");
              Obj.set_field (Obj.repr register) 1 (Obj.repr 13) );
          ( "origin",
            fun register ->
              Obj.set_field (Obj.repr register) 2
                (Obj.repr (Symbol.Synthesized "forged physical request")) );
          ( "origin source span",
            fun register ->
              match Request.explicit_register_origin register with
              | Symbol.Source_location origin ->
                  Obj.set_field (Obj.repr origin.span) 1
                    (Obj.repr (origin.span.start + 1))
              | _ -> failwith "expected source register origin" );
          ( "shared spelling contents",
            fun register ->
              let spelling = Request.explicit_register_spelling register in
              Bytes.set (Bytes.unsafe_of_string spelling) 2 '3';
              Obj.set_field (Obj.repr register) 1 (Obj.repr 13) );
        ]
      in
      List.iter
        (fun (label, mutate) ->
          let changed = compile mode source in
          let original_location = location changed.frame "x" in
          let symbol = Frame.location_symbol original_location in
          let proof =
            Option.get
              (Analysis.analyze ~frame:changed.frame ~body:changed.body)
          in
          Alcotest.(check (option int))
            "original request has physical authority" (Some 12)
            (Analysis.physical_register proof ~frame:changed.frame
               ~body:changed.body ~symbol);
          mutate (explicit original_location);
          Alcotest.(check bool)
            (label ^ " invalidates selection")
            true
            (Option.is_none
               (Analysis.selection proof ~frame:changed.frame ~body:changed.body
                  ~symbol));
          Alcotest.(check (option int))
            (label ^ " cannot change physical authority")
            None
            (Analysis.physical_register proof ~frame:changed.frame
               ~body:changed.body ~symbol);
          Alcotest.(check bool)
            (label ^ " cannot acquire a fresh request witness")
            true
            (Option.is_none
               (Analysis.analyze ~frame:changed.frame ~body:changed.body)))
        mutations;
      List.iter
        (fun source ->
          let changed = compile mode source in
          let original_location = location changed.frame "x" in
          let symbol = Frame.location_symbol original_location in
          let proof =
            Option.get
              (Analysis.analyze ~frame:changed.frame ~body:changed.body)
          in
          let register = explicit original_location in
          let copied : Request.explicit_register =
            Obj.obj (Obj.dup (Obj.repr register))
          in
          Obj.set_field
            (Obj.repr original_location)
            12
            (Obj.repr (Request.Explicit copied));
          Alcotest.(check (option int))
            "equal reconstructed request cannot reuse authority" None
            (Analysis.physical_register proof ~frame:changed.frame
               ~body:changed.body ~symbol);
          let reproved =
            Option.get
              (Analysis.analyze ~frame:changed.frame ~body:changed.body)
          in
          Alcotest.(check (option int))
            "frame-only request cannot acquire source authority" None
            (Analysis.physical_register reproved ~frame:changed.frame
               ~body:changed.body ~symbol))
        [ source; "I64 F(){I8 reg R12 x=127;x<<=1;return x;}42;" ];
      List.iter
        (fun qualifier ->
          let changed =
            compile mode
              (Printf.sprintf "I64 F(I8 %s x,I64 c){x<<=c;return x;}42;"
                 qualifier)
          in
          let original_location = location changed.frame "x" in
          let symbol = Frame.location_symbol original_location in
          let proof =
            Option.get
              (Analysis.analyze ~frame:changed.frame ~body:changed.body)
          in
          Obj.set_field
            (Obj.repr original_location)
            12
            (Obj.repr
               (Frame.location_register_selection (location fn.frame "x")));
          Alcotest.(check (option int))
            "replacing a nonphysical request cannot grant authority" None
            (Analysis.physical_register proof ~frame:changed.frame
               ~body:changed.body ~symbol))
        [ ""; "reg"; "noreg"; "reg RSI" ];
      let malformed = compile mode source in
      let register = explicit (location malformed.frame "x") in
      Obj.set_field (Obj.repr register) 0 (Obj.repr "RAX");
      Alcotest.(check bool)
        "mismatched canonical spelling cannot acquire a witness" true
        (Option.is_none
           (Analysis.analyze ~frame:malformed.frame ~body:malformed.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_parameter_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (ty, qualifier, expected) ->
          List.iter
            (fun update ->
              let source =
                Printf.sprintf "I64 F(I64 c){%s %s x=c;%sreturn x;}42;" ty
                  qualifier update
              in
              let fn = compile mode source in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let x = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "parameter local selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                   ~symbol:x
                = Some expected))
            [ ""; "x+=1;"; "x-=1;"; "x<<=1;"; "x>>=1;"; "x*=3;" ])
        (List.concat_map
           (fun ty ->
             [
               (ty, "", Analysis.Register_candidate);
               (ty, "noreg", Analysis.Stack);
             ])
           [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ]))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let dynamic_parameter_local_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (ty, qualifier, expected) ->
          List.iter
            (fun operation ->
              let source =
                Printf.sprintf "I64 F(I64 c,I8 n){%s %s x=c;x%s=n;return x;}42;"
                  ty qualifier operation
              in
              let fn = compile mode source in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let x = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "dynamic local selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                   ~symbol:x
                = Some expected))
            [ "+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "|"; "^" ])
        (List.concat_map
           (fun ty ->
             [
               (ty, "", Analysis.Register_candidate);
               (ty, "noreg", Analysis.Stack);
             ])
           [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ]))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let dynamic_count_width_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun ty ->
          List.iter
            (fun operation ->
              let source =
                Printf.sprintf "I64 F(I64 c,%s n){I8 x=c;x%s=n;return x;}42;" ty
                  operation
              in
              let fn = compile mode source in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let x = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "dynamic count width selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                   ~symbol:x
                = Some Analysis.Register_candidate))
            [ "+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "|"; "^" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let narrow_initializer_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun arg ->
          List.iter
            (fun target ->
              List.iter
                (fun (qualifier, expected) ->
                  List.iter
                    (fun dynamic ->
                      let parameters =
                        if dynamic then arg ^ " c,I8 n" else arg ^ " c"
                      in
                      let update = if dynamic then "x+=n;" else "" in
                      let source =
                        Printf.sprintf "I64 F(%s){%s %s x=c;%sreturn x;}42;"
                          parameters target qualifier update
                      in
                      let fn = compile mode source in
                      let proof =
                        Option.get
                          (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                      in
                      let x = location fn.frame "x" |> Frame.location_symbol in
                      Alcotest.(check bool)
                        "narrow initializer selection" true
                        (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                           ~symbol:x
                        = Some expected))
                    [ false; true ])
                [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
            [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let literal_parameter_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun ty ->
          List.iter
            (fun (qualifier, expected) ->
              List.iter
                (fun update ->
                  let source =
                    Printf.sprintf "I64 F(I64 c){%s %s x=255;%sreturn x;}42;" ty
                      qualifier update
                  in
                  let fn = compile mode source in
                  let proof =
                    Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                  in
                  let x = location fn.frame "x" |> Frame.location_symbol in
                  Alcotest.(check bool)
                    "literal parameter selection" true
                    (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                       ~symbol:x
                    = Some expected))
                [
                  "";
                  "x<<=1;";
                  "x+=c;";
                  "x-=c;";
                  "x*=c;";
                  "x/=c;";
                  "x%=c;";
                  "x<<=c;";
                  "x>>=c;";
                  "x&=c;";
                  "x|=c;";
                  "x^=c;";
                ])
            [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unary_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun ty ->
          List.iter
            (fun (qualifier, expected) ->
              List.iter
                (fun operation ->
                  List.iter
                    (fun local ->
                      let source =
                        if local then
                          Printf.sprintf "I64 F(){%s %s x=255;%s;return x;}42;"
                            ty qualifier operation
                        else
                          Printf.sprintf "I64 F(%s %s x){%s;return x;}42;" ty
                            qualifier operation
                      in
                      let fn = compile mode source in
                      let proof =
                        Option.get
                          (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                      in
                      let x = location fn.frame "x" |> Frame.location_symbol in
                      Alcotest.(check bool)
                        "unary selection" true
                        (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                           ~symbol:x
                        = Some expected))
                    [ false; true ])
                [ "x++"; "++x"; "x--"; "--x" ];
              List.iter
                (fun op ->
                  List.iter
                    (fun local ->
                      let source =
                        if local then
                          Printf.sprintf "I64 F(){%s %s x=255;return %sx;}42;"
                            ty qualifier op
                        else
                          Printf.sprintf "I64 F(%s %s x){return %sx;}42;" ty
                            qualifier op
                      in
                      let fn = compile mode source in
                      let proof =
                        Option.get
                          (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                      in
                      let x = location fn.frame "x" |> Frame.location_symbol in
                      Alcotest.(check bool)
                        "prefix return selection" true
                        (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                           ~symbol:x
                        = Some expected))
                    [ false; true ])
                [ "++"; "--" ];
              List.iter
                (fun op ->
                  List.iter
                    (fun local ->
                      let source =
                        if local then
                          Printf.sprintf "I64 F(){%s %s x=255;return x%s;}42;"
                            ty qualifier op
                        else
                          Printf.sprintf "I64 F(%s %s x){return x%s;}42;" ty
                            qualifier op
                      in
                      let fn = compile mode source in
                      let proof =
                        Option.get
                          (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                      in
                      let x = location fn.frame "x" |> Frame.location_symbol in
                      Alcotest.(check bool)
                        "postfix return selection" true
                        (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                           ~symbol:x
                        = Some expected))
                    [ false; true ])
                [ "++"; "--" ])
            [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let global_immediate_proof () =
  List.iter
    (fun mode ->
      List.iter
        (fun ty ->
          List.iter
            (fun op ->
              let fn =
                compile mode
                  (Printf.sprintf "%s G=0;I64 F(){return G%s=1;}42;" ty op)
              in
              let proof =
                Option.get (Global.analyze ~frame:fn.frame ~body:fn.body)
              in
              let descriptions =
                Body.body fn.body |> Graph.blocks
                |> List.concat_map (fun block ->
                    Graph.instructions block |> Seq.instructions
                    |> List.map Seq.description)
              in
              let address =
                List.find
                  (fun (item : Seq.description) ->
                    match item.payload with
                    | Some (Seq.Symbol _) -> true
                    | _ -> false)
                  descriptions
              in
              let symbol =
                match address.payload with
                | Some (Seq.Symbol symbol) -> symbol
                | _ -> assert false
              in
              let update =
                List.find
                  (fun (item : Seq.description) ->
                    item.opcode
                    =
                    if op = "+" then Holyc_lib__Ir.Opcode.Ic_add_equ
                    else Ic_sub_equ)
                  descriptions
              in
              Alcotest.(check bool)
                "global proof matches" true
                (Global.matches proof ~frame:fn.frame ~body:fn.body
                   ~instruction:update ~symbol);
              let foreign =
                compile mode
                  (Printf.sprintf "%s G=0;I64 F(){return G%s=1;}42;" ty op)
              in
              Alcotest.(check bool)
                "foreign function cannot use proof" false
                (Global.matches proof ~frame:foreign.frame ~body:foreign.body
                   ~instruction:update ~symbol);
              let cloned =
                { update with operands = List.map Fun.id update.operands }
              in
              Alcotest.(check bool)
                "equivalent lowered instruction" true
                (Global.matches proof ~frame:fn.frame ~body:fn.body
                   ~instruction:cloned ~symbol);
              let wrong_span = { cloned with span = None } in
              Alcotest.(check bool)
                "changed lowered origin rejected" false
                (Global.matches proof ~frame:fn.frame ~body:fn.body
                   ~instruction:wrong_span ~symbol);
              Obj.set_field (Obj.repr update) 6 (Obj.repr 1L);
              Alcotest.(check bool)
                "changed update invalidates global proof" false
                (Global.matches proof ~frame:fn.frame ~body:fn.body
                   ~instruction:update ~symbol);
              Obj.set_field (Obj.repr update) 6 (Obj.repr 0L);
              let literal =
                List.find
                  (fun (item : Seq.description) ->
                    item.payload = Some (Seq.Integer 1L))
                  descriptions
              in
              Obj.set_field (Obj.repr literal) 5
                (Obj.repr (Some (Seq.Integer 2L)));
              Alcotest.(check bool)
                "changed literal invalidates global proof" false
                (Global.matches proof ~frame:fn.frame ~body:fn.body
                   ~instruction:update ~symbol);
              Obj.set_field (Obj.repr literal) 5
                (Obj.repr (Some (Seq.Integer 1L)));
              Obj.set_field (Obj.repr literal) 4 (Obj.repr update.target_type);
              Alcotest.(check bool)
                "changed literal type invalidates proof" false
                (Global.matches proof ~frame:fn.frame ~body:fn.body
                   ~instruction:update ~symbol))
            [ "+"; "-" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun ty ->
          List.iter
            (fun operation ->
              List.iter
                (fun (inner, outer) ->
                  let source =
                    Printf.sprintf "%s G=0;I64 F(){return (G%s=%s)+%s;}42;" ty
                      operation inner outer
                  in
                  let fn = compile mode source in
                  Alcotest.(check bool)
                    "nested global immediate proved" true
                    (Option.is_some
                       (Global.analyze ~frame:fn.frame ~body:fn.body)))
                [
                  ("1", "0");
                  ("1", "1");
                  ("1", "256");
                  ("1", "2147483647");
                  ("256", "0");
                  ("2147483647", "0");
                ])
            [ "+"; "-" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun ty ->
          List.iter
            (fun op ->
              List.iter
                (fun (inner, outer) ->
                  let source =
                    Printf.sprintf "%s G=0;I64 F(){return (G%s=%s)-%s;}42;" ty
                      op inner outer
                  in
                  let fn = compile mode source in
                  Alcotest.(check bool)
                    "subtracted global immediate proved" true
                    (Option.is_some
                       (Global.analyze ~frame:fn.frame ~body:fn.body)))
                [
                  ("1", "0");
                  ("1", "1");
                  ("1", "2147483647");
                  ("256", "1");
                  ("2147483647", "1");
                ])
            [ "+"; "-" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun ty ->
          List.iter
            (fun op ->
              List.iter
                (fun (inner, outer) ->
                  let source =
                    Printf.sprintf "%s G=0;I64 F(){return (G%s=%s)*%s;}42;" ty
                      op inner outer
                  in
                  let fn = compile mode source in
                  Alcotest.(check bool)
                    "multiplied global immediate proved" true
                    (Option.is_some
                       (Global.analyze ~frame:fn.frame ~body:fn.body)))
                [
                  ("1", "1");
                  ("1", "2");
                  ("1", "3");
                  ("1", "256");
                  ("1", "2147483647");
                  ("256", "2");
                  ("2147483647", "2");
                ])
            [ "+"; "-" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun ty ->
          List.iter
            (fun rhs_type ->
              List.iter
                (fun op ->
                  let source =
                    Printf.sprintf "%s G=0;%s N=7;I64 F(){return (G%s=1)*N;}42;"
                      ty rhs_type op
                  in
                  let fn = compile mode source in
                  Alcotest.(check bool)
                    "dynamic global multiplication proved" true
                    (Option.is_some
                       (Global.analyze ~frame:fn.frame ~body:fn.body)))
                [ "+"; "-" ])
            [ "I64"; "U64" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun ty ->
          List.iter
            (fun rhs_type ->
              List.iter
                (fun op ->
                  let source =
                    Printf.sprintf "%s G=0;%s N=7;I64 F(){return (G%s=1)-N;}42;"
                      ty rhs_type op
                  in
                  let fn = compile mode source in
                  Alcotest.(check bool)
                    "dynamic global subtraction proved" true
                    (Option.is_some
                       (Global.analyze ~frame:fn.frame ~body:fn.body)))
                [ "+"; "-" ])
            [ "I64"; "U64" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun ty ->
          List.iter
            (fun rhs_type ->
              List.iter
                (fun operator ->
                  let source =
                    Printf.sprintf "%s G=0;%s N=7;I64 F(){return (G%s=1)+N;}42;"
                      ty rhs_type operator
                  in
                  let fn = compile mode source in
                  Alcotest.(check bool)
                    "dynamic nested global proved" true
                    (Option.is_some
                       (Global.analyze ~frame:fn.frame ~body:fn.body)))
                [ "+"; "-" ])
            [ "I64"; "U64" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun ty ->
          List.iter
            (fun parameter_type ->
              List.iter
                (fun operation ->
                  List.iter
                    (fun literal ->
                      let source =
                        Printf.sprintf
                          "%s G=0;I64 ID(%s x){return x;}I64 F(){return \
                           ID(G%s=%s);}42;"
                          ty parameter_type operation literal
                      in
                      let fn = compile ~name:"F" mode source in
                      Alcotest.(check bool)
                        "global update call proved" true
                        (Option.is_some
                           (Global.analyze ~frame:fn.frame ~body:fn.body)))
                    [ "1"; "256"; "2147483647" ])
                [ "+"; "-" ])
            [ "I64"; "U64" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ];
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "unproved global update call" true
            (Option.is_none (Global.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "U8 G=255;I64 ID(I8 x){return x;}I64 F(){return ID(G+=1);}42;";
          "U8 G=255;I64 ID(I64 x,...){return x;}I64 F(){return ID(G+=1);}42;";
          "U8 G=255;I64 ID(I64 x,I64 y){return x+y;}I64 F(){return \
           ID(G+=1,1);}42;";
          "U8 G=255;I64 ID(I64 x){return x;}I64 F(){return ID((G+=1)+0);}42;";
          "U8 G=255;I64 ID(I64 x){return x;}I64 F(){return \
           ID(G+=2147483648);}42;";
          "U8 G=255;I64 N=1;I64 ID(I64 x){return x;}I64 F(){return \
           ID(G+=N);}42;";
        ];
      let called =
        compile ~name:"F" mode
          "U8 G=255;I64 ID(I64 x){return x;}I64 F(){return ID(G+=1);}42;"
      in
      let call_proof =
        Option.get (Global.analyze ~frame:called.frame ~body:called.body)
      in
      let call_descriptions =
        Body.body called.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
      in
      let global_address =
        List.find
          (fun (item : Seq.description) ->
            match item.payload with
            | Some (Seq.Symbol symbol) -> Symbol.name symbol = "G"
            | _ -> false)
          call_descriptions
      in
      let symbol =
        match global_address.payload with
        | Some (Seq.Symbol symbol) -> symbol
        | _ -> assert false
      in
      let update =
        List.find
          (fun (item : Seq.description) ->
            item.opcode = Holyc_lib__Ir.Opcode.Ic_add_equ)
          call_descriptions
      in
      Alcotest.(check bool)
        "pushed update proof matches" true
        (Global.matches call_proof ~frame:called.frame ~body:called.body
           ~instruction:update ~symbol);
      let lowered = { update with flags = 0L } in
      Alcotest.(check bool)
        "normalized push instruction matches" true
        (Global.matches call_proof ~frame:called.frame ~body:called.body
           ~instruction:lowered ~symbol);
      let invalid = { update with flags = 0x4000L } in
      Alcotest.(check bool)
        "unrelated call flag rejected" false
        (Global.matches call_proof ~frame:called.frame ~body:called.body
           ~instruction:invalid ~symbol);
      let call =
        List.find
          (fun (item : Seq.description) ->
            item.opcode = Holyc_lib__Ir.Opcode.Ic_call)
          call_descriptions
      in
      Obj.set_field (Obj.repr call) 5 (Obj.repr (Some (Seq.Symbol symbol)));
      Alcotest.(check bool)
        "mutated callee invalidates proof" false
        (Global.matches call_proof ~frame:called.frame ~body:called.body
           ~instruction:update ~symbol);
      let dynamic =
        compile mode "U8 G=255;I64 N=7;I64 F(){return (G+=1)+N;}42;"
      in
      let dynamic_proof =
        Option.get (Global.analyze ~frame:dynamic.frame ~body:dynamic.body)
      in
      let descriptions =
        Body.body dynamic.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
      in
      let address name =
        List.find
          (fun (item : Seq.description) ->
            match item.payload with
            | Some (Seq.Symbol symbol) -> Symbol.name symbol = name
            | _ -> false)
          descriptions
      in
      let global_address = address "G" and right_address = address "N" in
      let symbol =
        match global_address.payload with
        | Some (Seq.Symbol symbol) -> symbol
        | _ -> assert false
      in
      let update =
        List.find
          (fun (item : Seq.description) ->
            item.opcode = Holyc_lib__Ir.Opcode.Ic_add_equ)
          descriptions
      in
      Alcotest.(check bool)
        "dynamic global proof matches" true
        (Global.matches dynamic_proof ~frame:dynamic.frame ~body:dynamic.body
           ~instruction:update ~symbol);
      Obj.set_field (Obj.repr right_address) 5
        (Obj.repr (Some (Seq.Symbol symbol)));
      Alcotest.(check bool)
        "foreign dynamic operand invalidates proof" false
        (Global.matches dynamic_proof ~frame:dynamic.frame ~body:dynamic.body
           ~instruction:update ~symbol);
      let nested = compile mode "U8 G=255;I64 F(){return (G+=1)+0;}42;" in
      let nested_proof =
        Option.get (Global.analyze ~frame:nested.frame ~body:nested.body)
      in
      let descriptions =
        Body.body nested.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
      in
      let address =
        List.find
          (fun (item : Seq.description) ->
            match item.payload with
            | Some (Seq.Symbol _) -> true
            | _ -> false)
          descriptions
      in
      let symbol =
        match address.payload with
        | Some (Seq.Symbol symbol) -> symbol
        | _ -> assert false
      in
      let update =
        List.find
          (fun (item : Seq.description) ->
            item.opcode = Holyc_lib__Ir.Opcode.Ic_add_equ)
          descriptions
      in
      Alcotest.(check bool)
        "nested proof matches" true
        (Global.matches nested_proof ~frame:nested.frame ~body:nested.body
           ~instruction:update ~symbol);
      let outer =
        List.find
          (fun (item : Seq.description) -> item.payload = Some (Seq.Integer 0L))
          descriptions
      in
      Obj.set_field (Obj.repr outer) 5 (Obj.repr (Some (Seq.Integer 1L)));
      Alcotest.(check bool)
        "changed outer literal invalidates nested proof" false
        (Global.matches nested_proof ~frame:nested.frame ~body:nested.body
           ~instruction:update ~symbol);
      List.iter
        (fun literal ->
          let fn =
            compile mode ("U8 G=255;I64 F(){return G+=" ^ literal ^ ";}42;")
          in
          Alcotest.(check bool)
            "global literal boundary proved" true
            (Option.is_some (Global.analyze ~frame:fn.frame ~body:fn.body)))
        [ "0"; "256"; "2147483647" ];
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            "unproved global shape" true
            (Option.is_none (Global.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "U8 G=255;I64 N=1;I64 F(){return G+=N;}42;";
          "U8 G=255;I64 F(){U8 *p=&G;return *p+=1;}42;";
          "U8 G=255;U8 N=7;I64 F(){return (G+=1)+N;}42;";
          "U8 G=255;I64 F(){return (G+=1)+2147483648;}42;";
          "U8 G=255;I64 F(){return 1+(G+=1);}42;";
          "U8 G=255;I64 N=1;I64 F(){return (G+=1)+(N+0);}42;";
          "U8 G=255;I64 F(){return G+=2147483648;}42;";
          "U8 G=255;I64 F(){return G*=1;}42;";
          "U8 G=255;I64 F(I64 x){return G+=1;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (qualifier, expected) ->
          let source =
            "I64 F(){I8 " ^ qualifier ^ " x=255;x<<=0;return x;}42;"
          in
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check bool)
            "bounded local selection" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some expected))
        [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let compound_return_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun ty ->
          List.iter
            (fun (qualifier, expected) ->
              List.iter
                (fun operator ->
                  let source =
                    Printf.sprintf "I64 F(){%s %s x=255;return x%s=1;}42;" ty
                      qualifier operator
                  in
                  let fn = compile mode source in
                  let proof =
                    Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                  in
                  let symbol = location fn.frame "x" |> Frame.location_symbol in
                  Alcotest.(check bool)
                    "compound return selection" true
                    (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                       ~symbol
                    = Some expected))
                [ "+"; "-"; "*"; "<<"; ">>" ];
              List.iter
                (fun operator ->
                  let source =
                    Printf.sprintf "I64 F(%s %s x){return x%s=1;}42;" ty
                      qualifier operator
                  in
                  let fn = compile mode source in
                  let proof =
                    Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
                  in
                  let symbol = location fn.frame "x" |> Frame.location_symbol in
                  Alcotest.(check bool)
                    "parameter compound return selection" true
                    (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                       ~symbol
                    = Some expected))
                [ "+"; "-"; "*"; "<<"; ">>" ])
            [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_return_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun (qualifier, expected) ->
          let source = "I64 F(){I8 " ^ qualifier ^ " x=255;return x;}42;" in
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let symbol = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check bool)
            "local return selection" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
            = Some expected))
        [ ("", Analysis.Register_candidate); ("noreg", Analysis.Stack) ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_ownership () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = location fn.frame "x" |> Frame.location_symbol in
          let store =
            Body.body fn.body |> Graph.blocks
            |> List.concat_map (fun block ->
                Graph.instructions block |> Seq.instructions
                |> List.map Seq.description)
            |> List.find (fun (item : Seq.description) ->
                item.opcode = Holyc_lib__Ir.Opcode.Ic_assign)
          in
          Obj.set_field (Obj.repr store) 5 (Obj.repr (Some (Seq.Integer 1L)));
          Alcotest.(check bool)
            "changed local store invalidates proof" true
            (Option.is_none
               (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x));
          Alcotest.(check bool)
            "changed local store cannot be reproved" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(){I8 x=255;x<<=0;return x;}42;";
          "I64 F(){I8 x=255;x++;return x;}42;";
          "I64 F(){I8 x=255;return ++x;}42;";
          "I64 F(){I8 x=255;return x++;}42;";
          "I64 F(){I8 x=255;return x+=1;}42;";
          "I64 F(){I8 noreg x=255;return x+=1;}42;";
          "I64 F(){I8 x=255;return x;}42;";
          "I64 F(I64 c){I8 x=c;return x;}42;";
          "I64 F(I64 c){I8 x=c;x+=1;return x;}42;";
          "I64 F(I64 c){I8 x=c;x<<=1;return x;}42;";
          "I64 F(I64 c){I8 x=c;x-=1;return x;}42;";
          "I64 F(I64 c){I8 x=c;x>>=1;return x;}42;";
          "I64 F(I64 c){I8 x=c;x*=3;return x;}42;";
          "I64 F(I64 c,I8 n){I8 x=c;x+=n;return x;}42;";
          "I64 F(I64 c,I8 n){I8 x=c;x<<=n;return x;}42;";
          "I64 F(I64 c,I64 n){I8 x=c;x&=n;return x;}42;";
          "I64 F(I64 c,I64 n){I8 x=c;x|=n;return x;}42;";
          "I64 F(I64 c,I64 n){I8 x=c;x^=n;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let control_flow_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = Frame.location_symbol (location fn.frame "x") in
          Alcotest.(check bool)
            "control-flow register value" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some Analysis.Register_candidate);
          let foreign = compile mode source in
          Alcotest.(check bool)
            "control-flow foreign frame rejected" true
            (Option.is_none
               (Analysis.selection proof ~frame:foreign.frame ~body:fn.body
                  ~symbol:x)))
        [
          "I64 F(I8 x,I64 c){if(c)x<<=c;return x;}42;";
          "I64 F(I8 x,I64 c){if(c)x%=c;return x;}42;";
          "I64 F(I8 x,I64 n,I64 c,I64 one){while(n){x+=c;n-=one;}return x;}42;";
          "I64 F(I8 x,I64 n,I64 c,I64 one){do{x*=c;n-=one;}while(n);return \
           x;}42;";
          "I64 F(I8 x,I64 n,I64 c,I64 one){for(;n;n-=one){x+=c;}return x;}42;";
          "I64 F(I8 x,I64 n,I64 c,I64 one){again:if(!n)goto \
           done;x+=c;n-=one;goto again;done:return x;}42;";
        ];
      let source =
        "I64 F(I8 noreg x,I64 n,I64 c,I64 one){while(n){x+=c;n-=one;}return \
         x;}42;"
      in
      let fn = compile mode source in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let x = Frame.location_symbol (location fn.frame "x") in
      Alcotest.(check bool)
        "control-flow noreg remains stack" true
        (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
        = Some Analysis.Stack);
      let instruction =
        Graph.blocks (Body.body fn.body)
        |> List.concat_map (fun block ->
            Seq.instructions (Graph.instructions block))
        |> List.find (fun item ->
            (Seq.description item).opcode = Ir_opcode.Ic_br_zero)
        |> Seq.description
      in
      Obj.set_field (Obj.repr instruction) 6 (Obj.repr 1L);
      Alcotest.(check bool)
        "control-flow changed branch invalidates proof" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let direct_call_selection () =
  let defaulted =
    compile ~name:"F" Preprocessor.Aot
      "I64 G(I64 n=1){return n;}I64 F(I8 x,I64 c){x+=c;G();return x;}42;"
  in
  Alcotest.(check bool)
    "default arguments remain outside register call proof" true
    (Option.is_none
       (Analysis.analyze ~frame:defaulted.frame ~body:defaulted.body));
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = Frame.location_symbol (location fn.frame "x") in
          Alcotest.(check bool)
            "selected value survives direct call" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some Analysis.Register_candidate);
          let foreign = compile ~name:"G" mode "I64 G(I64 n){return n;}42;" in
          let call =
            Graph.blocks (Body.body fn.body)
            |> List.concat_map (fun block ->
                Seq.instructions (Graph.instructions block))
            |> List.find (fun item ->
                (Seq.description item).opcode = Ir_opcode.Ic_call_start)
            |> Seq.description
          in
          Obj.set_field (Obj.repr call) 5
            (Obj.repr (Some (Seq.Symbol (Body.callable_symbol foreign.body))));
          Alcotest.(check bool)
            "foreign call symbol invalidates proof" true
            (Option.is_none
               (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x)))
        [
          "I64 G(I64 n){return n*3;}I64 F(I8 x,I64 c){x+=c;G(x);x+=c;return \
           x;}42;";
          "I64 F(I8 x,I64 n,I64 c,I64 one){x+=c;if(n){n-=one;return \
           F(x,n,c,one)+x;}return x;}42;";
          "U0 G(I64 n){n+=n;}I64 F(I8 x,I64 c){x+=c;G(x);return x;}42;";
        ];
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "unproved call remains outside proof" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 G(I8 *p){return *p;}I64 F(I8 x,I64 c){x+=c;G(&x);return x;}42;";
          "I64 G(I64 n){return n;}I64 F(I8 reg RAX x,I64 c){x+=c;G(x);return \
           x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let local_control_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = Frame.location_symbol (location fn.frame "x") in
          Alcotest.(check bool)
            "local selected across control flow" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some Analysis.Register_candidate))
        [
          "I64 F(I64 n,I64 c,I64 one){I8 x=129;while(n){x+=c;n-=one;}return \
           x;}42;";
          "I64 F(I64 a,I64 n,I64 c,I64 one){I8 \
           x=a;while(n){x+=c;n-=one;}return x;}42;";
          "I64 G(I64 v){return v;}I64 F(I64 c){I8 x=129;G(x);x+=c;return x;}42;";
          "I64 F(I64 n,I64 c,I64 one){I8 x=129;U16 \
           y=65537;while(n){x+=c;y+=c;n-=one;}return x+y;}42;";
        ];
      let changed =
        compile ~name:"F" mode
          "I64 F(I64 n,I64 c){I8 x=129;if(n)x+=c;return x;}42;"
      in
      let proof =
        Option.get (Analysis.analyze ~frame:changed.frame ~body:changed.body)
      in
      let x = Frame.location_symbol (location changed.frame "x") in
      let initial =
        Graph.blocks (Body.body changed.body)
        |> List.concat_map (fun block ->
            Seq.instructions (Graph.instructions block))
        |> List.map Seq.description
        |> List.find (fun (item : Seq.description) ->
            item.opcode = Ir_opcode.Ic_imm_i64
            && item.payload = Some (Seq.Integer 129L))
      in
      Obj.set_field (Obj.repr initial) 5 (Obj.repr (Some (Seq.Integer 130L)));
      Alcotest.(check bool)
        "changed initializer invalidates witness" true
        (Option.is_none
           (Analysis.selection proof ~frame:changed.frame ~body:changed.body
              ~symbol:x));
      Alcotest.(check bool)
        "foreign initializer cannot be reproved" true
        (Option.is_none
           (Analysis.analyze ~frame:changed.frame ~body:changed.body));
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "unsafe local shape remains unproved" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I64 n,I64 c){I8 x;if(n)x=c;return x;}42;";
          "I64 F(I64 n,I64 c){I8 x=n(I8);if(n)x+=c;return x;}42;";
          "I64 F(I64 n,I64 c){I8 x=127;I8 *p=&x;if(n)*p+=c;return x;}42;";
          "I64 F(I64 n,I64 c){if(n){I8 x=127;x+=c;return x;}return 0;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let register_bank_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let x = Frame.location_symbol (location fn.frame "x") in
          Alcotest.(check bool)
            "remaining register bank fits" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol:x
            = Some Analysis.Register_candidate))
        [
          "I64 F(I8 x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 \
           g){if(g)x+=a;return x+b+c+d+e+f+g;}42;";
          "I64 F(I8 x,I64 reg R12 a,I64 b,I64 c,I64 d,I64 e,I64 \
           f){if(f)x+=a;return x+b+c+d+e+f;}42;";
          "I64 F(I8 x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h,I64 \
           i){if(a)x+=a;return x;}42;";
          "I64 F(I8 x,I64 noreg a,I64 noreg b,I64 noreg c,I64 noreg d,I64 \
           noreg e,I64 noreg f,I64 noreg g,I64 noreg h,I64 noreg \
           i){if(a)x+=a;return x+b+c+d+e+f+g+h+i;}42;";
          "I64 F(I8 reg RSI x,I64 reg RDI a,I64 reg R10 b,I64 reg R11 c,I64 \
           reg R12 d,I64 reg R13 e,I64 reg R14 f,I64 reg R15 \
           g){if(g)x+=a;return x+b+c+d+e+f+g;}42;";
        ];
      List.iter
        (fun count ->
          let parameters =
            List.init (count - 1) (fun index ->
                Printf.sprintf "I64 noreg p%d" index)
          in
          let source =
            "I64 F(I8 x,"
            ^ String.concat "," parameters
            ^ "){if(p0)x+=p0;return x;}42;"
          in
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "frame member bound" (count = 32)
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [ 32; 33 ];
      List.iter
        (fun register ->
          let source =
            Printf.sprintf "I64 F(I8 reg %s x,I64 c){if(c)x+=c;return x;}42;"
              register
          in
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "fixed and frame registers remain outside proof" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [ "RAX"; "RCX"; "RDX"; "RBX"; "RSP"; "RBP"; "R8"; "R9" ];
      List.iteri
        (fun index source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "exact pressure is proved; duplicate requests still reject"
            (index < 3)
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
           h){if(h)x+=a;return x+b+c+d+e+f+g+h;}42;";
          "I64 F(I8 reg RSI x,I8 a,I64 b,I64 c,I64 d,I64 e,I64 \
           f){if(f)x+=a;return x+b+c+d+e+f;}42;";
          "I64 F(I8 x,I64 reg a,I64 reg b,I64 reg c,I64 reg d,I64 reg e,I64 \
           reg f,I64 reg g,I64 reg h){if(x)x+=x;return x;}42;";
          "I64 F(I8 reg RSI x,I64 reg RSI a){if(a)x+=a;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let loop_update_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun qualifier ->
          List.iter
            (fun update ->
              let fn =
                compile ~name:"F" mode
                  (Printf.sprintf
                     "I64 F(I8 %s x,I64 n){I64 i=0;for(;i<n;i++){%s;}return \
                      x;}42;"
                     qualifier update)
              in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let symbol = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "loop update retains checked storage selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some
                    (if qualifier = "noreg" then Analysis.Stack
                     else Analysis.Register_candidate)))
            [
              "x+=1";
              "x-=2147483647";
              "x*=3";
              "x<<=63";
              "x>>=0";
              "++x";
              "x++";
              "--x";
              "x--";
            ])
        [ ""; "reg"; "noreg"; "reg R12" ];
      List.iter
        (fun update ->
          let fn =
            compile ~name:"F" mode
              (Printf.sprintf "I64 F(I8 x,I64 n){if(n){%s;}return x;}42;" update)
          in
          Alcotest.(check bool)
            ("unproved immediate or consumed update remains rejected: " ^ update)
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "x+=2147483648";
          "x*=2147483648";
          "x<<=n(I8)";
          "x/=2";
          "x%=2";
          "x|=1";
          "return x++";
          "return ++x";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let loop_update_ownership () =
  let module O = Holyc_lib__Ir.Opcode in
  let module Typed = Semantic_function_call_expression_result in
  let module Calls = Semantic_function_call_resolution in
  List.iter
    (fun mode ->
      let fn =
        compile ~name:"F" mode
          "I64 F(I8 x,I64 c,I64 d){if(c){x+=c;x*=d;}return x;}42;"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      let items =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
      in
      let first =
        List.find (fun (d : Seq.description) -> d.opcode = O.Ic_add_equ) items
      in
      let second =
        List.find (fun (d : Seq.description) -> d.opcode = O.Ic_mul_equ) items
      in
      let original = second.operands in
      Obj.set_field (Obj.repr second) 2
        (Obj.repr [ List.hd original; List.nth first.operands 1 ]);
      Alcotest.(check bool)
        "loop operand substitution invalidates existing proof" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "loop operand substitution cannot be reproved" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      Obj.set_field (Obj.repr second) 2 (Obj.repr original);
      let source = Option.get (Body.definition_source fn.body) in
      let binary =
        Typed.function_all_results source
        |> List.find_map (fun value ->
            match
              Calls.argument_expression_source (Typed.result_source value)
            with
            | Some (Ast.Binary_expression binary)
              when binary.binary_operator_spec.spelling = "+=" -> Some binary
            | _ -> None)
        |> Option.get
      in
      Obj.set_field (Obj.repr binary) 2
        (Obj.repr (Option.get (Operator.find_binary "-=")));
      Alcotest.(check bool)
        "loop typed operator mutation cannot be reproved" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      let fn =
        compile ~name:"F" mode
          "I64 F(I8 x,I64 n){while(n){x++;--n;}return x;}42;"
      in
      let update =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
        |> List.find (fun (d : Seq.description) -> d.opcode = O.Ic__pp)
      in
      Obj.set_field (Obj.repr update) 5 (Obj.repr (Some (Seq.Integer 1L)));
      Alcotest.(check bool)
        "unary loop payload cannot acquire a proof" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let forced_pressure_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun qualifier ->
          let fn =
            compile ~name:"F" mode
              (Printf.sprintf
                 "I64 F(I8 %s x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
                  h){if(a)x+=1;return x+a+b+c+d+e+f+g+h;}42;"
                 qualifier)
          in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          List.iter
            (fun (name, expected) ->
              let symbol = location fn.frame name |> Frame.location_symbol in
              Alcotest.(check bool)
                "whole-function pressure proves winners and spills" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some expected))
            [
              ( "x",
                if qualifier = "noreg" then Analysis.Stack
                else Analysis.Register_candidate );
              ("a", Analysis.Register_candidate);
              ("h", Analysis.Register_candidate);
              ( "d",
                if qualifier = "noreg" then Analysis.Register_candidate
                else Analysis.Stack );
            ])
        [ "reg"; "reg RSI"; "reg R12"; "noreg" ];
      List.iter
        (fun (forced, reserved, expected) ->
          let requests =
            List.init forced (fun n -> Printf.sprintf "I8 reg x%d" n)
          in
          let explicit =
            List.mapi
              (fun n register -> Printf.sprintf "I64 reg %s r%d" register n)
              reserved
          in
          let ordinary = List.init 9 (fun n -> Printf.sprintf "I64 p%d" n) in
          let source =
            "I64 F("
            ^ String.concat "," (requests @ explicit @ ordinary)
            ^ "){if(p0)x0+=1;return x0+p0+p1+p2+p3+p4+p5+p6+p7+p8;}42;"
          in
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "forced requests respect reserved banks" expected
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          (6, [], true);
          (7, [], true);
          (8, [], true);
          (9, [], true);
          (5, [ "RSI" ], true);
          (6, [ "RSI" ], true);
          (7, [ "RSI" ], true);
          (8, [ "RSI" ], true);
          (7, [ "R12" ], true);
          (8, [ "R12" ], true);
          (6, [ "R12"; "R13" ], true);
          (7, [ "R12"; "R13" ], true);
        ];
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "whole-function narrow pressure receives exact selection" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
           h){if(a)x+=1;return x+a+b+c+d+e+f+g+h;}42;";
          "I64 F(I8 reg x,I8 y,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
           h){if(a)x+=1;return x+y+a+b+c+d+e+f+g+h;}42;";
          "I64 F(I8 reg x,I64 reg a,I64 reg b,I64 reg c,I64 reg d,I64 reg \
           e,I64 reg f,I64 reg g,I64 reg h){if(a)x+=1;return \
           x+a+b+c+d+e+f+g+h;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let computed_update_selection () =
  let module O = Holyc_lib__Ir.Opcode in
  List.iter
    (fun mode ->
      List.iter
        (fun operator ->
          let fn =
            compile ~name:"F" mode
              (Printf.sprintf
                 "I64 F(I8 x,I64 a,I64 b){if(a)x%s(a+65)*(b-1);return x;}42;"
                 operator)
          in
          Alcotest.(check bool)
            ("source-owned compound tree: " ^ operator)
            true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [ "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^="; "<<="; ">>=" ];
      let fn =
        compile ~name:"F" mode "I64 F(I8 x,I64 a){if(a)x<<=a+65;return x;}42;"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      let literal =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
        |> List.find (fun (d : Seq.description) ->
            d.opcode = O.Ic_imm_i64 && d.payload = Some (Seq.Integer 65L))
      in
      Obj.set_field (Obj.repr literal) 5 (Obj.repr (Some (Seq.Integer 1L)));
      Alcotest.(check bool)
        "changed nested shift leaf invalidates proof" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "nested shift literals are not masked" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "unsupported computed update remains unproved" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 a){if(a)x+=a(I8);return x;}42;";
          "I64 F(I8 x,I64 a){if(a)return x+=a+1;return x;}42;";
          "I64 F(I8 x,I64 a){if(a)x/=3;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let integer_expression_selection () =
  let module O = Holyc_lib__Ir.Opcode in
  List.iter
    (fun mode ->
      List.iter
        (fun expression ->
          let fn =
            compile ~name:"F" mode
              (Printf.sprintf
                 "I64 F(I64 a,I64 b){I8 x=%s;if(b)x+=1;return x;}42;" expression)
          in
          Alcotest.(check bool)
            ("integer initializer tree: " ^ expression)
            true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "-a";
          "+a";
          "~a";
          "!a";
          "a/b";
          "a%b";
          "a<<b";
          "a>>b";
          "a<<65";
          "a==b";
          "a!=b";
          "a<b";
          "a<=b";
          "a>b";
          "a>=b";
          "-(a/b)";
        ];
      let fn =
        compile ~name:"F" mode "I64 F(I8 x,I64 a){if(a)x=~a;return x;}42;"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      let expression =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
        |> List.find (fun (d : Seq.description) -> d.opcode = O.Ic_com)
      in
      Obj.set_field (Obj.repr expression) 5 (Obj.repr (Some (Seq.Integer 1L)));
      Alcotest.(check bool)
        "unary payload mutation invalidates proof" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "unary payload cannot acquire proof" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      let fn =
        compile ~name:"F" mode
          "I64 F(I64 a,I64 b){I64 y=b;I8 x=a<<y;if(b)x+=1;return x;}42;"
      in
      Alcotest.(check bool)
        "shift initializer reads an earlier initialized local" true
        (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let computed_initializer_selection () =
  let module O = Holyc_lib__Ir.Opcode in
  List.iter
    (fun mode ->
      List.iter
        (fun qualifier ->
          List.iter
            (fun expression ->
              let fn =
                compile ~name:"F" mode
                  (Printf.sprintf
                     "I64 F(I64 a,I64 b){I8 %s x=%s;if(b)x+=1;return x;}42;"
                     qualifier expression)
              in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let symbol = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "computed initializer retains destination selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some
                    (if qualifier = "noreg" then Analysis.Stack
                     else Analysis.Register_candidate)))
            [ "a+1"; "a-b"; "a*b"; "a&b"; "a|b"; "a^b"; "(a+1)*(b-2)" ])
        [ ""; "reg"; "noreg"; "reg R12" ];
      let fn =
        compile ~name:"F" mode
          "I64 F(I64 a,I64 b){I8 x=(a+1)*(b-2);if(b)x+=1;return x;}42;"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      let expression =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
        |> List.find (fun (d : Seq.description) -> d.opcode = O.Ic_mul)
      in
      Obj.set_field (Obj.repr expression) 2
        (Obj.repr (List.rev expression.operands));
      Alcotest.(check bool)
        "computed initializer substitution invalidates proof" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "computed initializer substitution cannot be reproved" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "initializer needs entry placement and authenticated expressions"
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I64 a,I64 b){if(b){I8 x=a+1;x+=1;return x;}return 0;}42;";
          "I64 F(I64 a,I64 b){I8 x=a(I8);if(b)x+=1;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let computed_assignment_selection () =
  let module O = Holyc_lib__Ir.Opcode in
  List.iter
    (fun mode ->
      List.iter
        (fun expression ->
          let fn =
            compile ~name:"F" mode
              (Printf.sprintf "I64 F(I8 x,I64 a,I64 b){if(a)x=%s;return x;}42;"
                 expression)
          in
          Alcotest.(check bool)
            "source-owned computed assignment is selected" true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [ "a+1"; "a-b"; "a*b"; "a&b"; "a|b"; "a^b"; "(a+1)*(b-2)"; "x+(a^b)" ];
      let fn =
        compile ~name:"F" mode
          "I64 F(I8 x,I64 a,I64 b){if(a)x=(a+1)*(b-2);return x;}42;"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      let items =
        Body.body fn.body |> Graph.blocks
        |> List.concat_map (fun block ->
            Graph.instructions block |> Seq.instructions
            |> List.map Seq.description)
      in
      let expression =
        List.find (fun (d : Seq.description) -> d.opcode = O.Ic_mul) items
      in
      let original = expression.operands in
      Obj.set_field (Obj.repr expression) 2 (Obj.repr (List.rev original));
      Alcotest.(check bool)
        "computed operand substitution invalidates proof" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "computed operand substitution cannot be reproved" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      Obj.set_field (Obj.repr expression) 2 (Obj.repr original);
      Obj.set_field (Obj.repr expression) 5 (Obj.repr (Some (Seq.Integer 1L)));
      Alcotest.(check bool)
        "computed payload injection cannot be reproved" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let assignment_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun qualifier ->
          List.iter
            (fun source ->
              let fn =
                compile ~name:"F" mode (Printf.sprintf source qualifier)
              in
              let proof =
                Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
              in
              let symbol = location fn.frame "x" |> Frame.location_symbol in
              Alcotest.(check bool)
                "reassignment retains original storage selection" true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some
                    (if qualifier = "noreg" then Analysis.Stack
                     else Analysis.Register_candidate)))
            [
              "I64 F(I8 %s x,I64 value){if(value)x=value;return x;}42;";
              "I64 F(I64 n,I64 c){I8 %s x=127;if(n)x=c;return x;}42;";
              "I64 F(I64 n){I8 %s x=257;x=257;if(n)x=257;return x;}42;";
              "I64 F(I8 %s x,I64 n){if(n)x=0x7FFFFFFFFFFFFFFF;return x;}42;";
            ])
        [ ""; "reg"; "noreg"; "reg R12" ];
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "assignment needs initialized storage and direct source operands"
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I64 n,I64 value){I8 x;if(n)x=value;return x;}42;";
          "I64 F(I64 n){I8 x;x=257;if(n)x=257;return x;}42;";
          "I64 F(I8 x,I64 value){if(value)x=value(I8);return x;}42;";
          "I64 F(I64 n,I64 value){if(n){I8 x=0;x=value;return x;}return 0;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let consumed_assignment_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun type_ ->
          List.iter
            (fun qualifier ->
              List.iter
                (fun local ->
                  List.iter
                    (fun body ->
                      let source =
                        "I64 G(I64 value){return value;}"
                        ^
                        if local then
                          Printf.sprintf "I64 F(I64 a){%s %s x=0;%s}42;" type_
                            qualifier body
                        else
                          Printf.sprintf "I64 F(%s %s x,I64 a){%s}42;" type_
                            qualifier body
                      in
                      let fn = compile ~name:"F" mode source in
                      let proof =
                        match
                          Analysis.analyze ~frame:fn.frame ~body:fn.body
                        with
                        | Some proof -> proof
                        | None ->
                            Alcotest.fail ("missing assignment proof: " ^ source)
                      in
                      let symbol =
                        location fn.frame "x" |> Frame.location_symbol
                      in
                      Alcotest.(check bool)
                        "consumed assignment retains proven storage" true
                        (Analysis.selection proof ~frame:fn.frame ~body:fn.body
                           ~symbol
                        = Some
                            (if qualifier = "noreg" then Analysis.Stack
                             else Analysis.Register_candidate)))
                    [
                      "if(x=a+1)return x;return -1;";
                      "while(x=a+1)return x;return -1;";
                      "do {a-=1;}while(x=a+1);return x;";
                      "for(;(x=a+1)>0;a-=1){if(a)return x;}return x;";
                      "return x=a+1;";
                      "return (x=a+1)*3;";
                      "return G(x=a+1);";
                      "G(x=a+1);return x;";
                    ])
                [ false; true ])
            [ ""; "reg"; "noreg"; "reg R12" ])
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ];
      List.iter
        (fun qualifier ->
          let fn =
            compile_option mode
              (Printf.sprintf
                 "I64 F(I8 %s x,I64 a){if(x=a+1)return x;return -1;}#exe \
                  {Option(34,1);}42;"
                 qualifier)
          in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let symbol = location fn.frame "x" |> Frame.location_symbol in
          Alcotest.(check bool)
            "completed-body NO_REG_VAR retains explicit selection only" true
            (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
            = Some
                (if qualifier = "reg R12" then Analysis.Register_candidate
                 else Analysis.Stack)))
        [ ""; "reg"; "noreg"; "reg R12" ];
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            ("consumed assignment boundary remains unproved: " ^ source)
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 noreg x,I64 a){if(x+=a)return x;return -1;}42;";
          "I64 F(I8 noreg x,I64 a){if(x*=a)return x;return -1;}42;";
          "I64 F(I8 noreg x,I64 a){if(x=a(I8))return x;return -1;}42;";
          "I64 G(I64 a){return a;}I64 F(I8 noreg x,I64 a){return x=G(a);}42;";
          "I64 F(I8 noreg x,I8 noreg y,I64 a){return x=(y=a+1);}42;";
          "I64 F(I64 a){I8 noreg x;if(x=a+1)return x;return -1;}42;";
          "I64 F(I64 a){if(a){I8 noreg x=0;return x=a+1;}return 0;}42;";
          "I64 F(I8 noreg x,I64 a){I8 *p=&x;return *p=a+1;}42;";
          "I8 x;I64 F(I64 a){return x=a+1;}42;";
          "I64 F(I8 reg R12 x,I64 reg R12 a){return x=a+1;}42;";
          "I64 F(I8 reg RAX x,I64 a){return x=a+1;}42;";
          "I64 F(Bool noreg x,I64 a){return x=a+1;}42;";
          "I64 F(I8 x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
           h){if(x=a+1)return x+b+c+d+e+f+g+h;return -1;}42;";
          "I64 F(I8 reg x,I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
           h){if(a=b+1)return x+a+c+d+e+f+g+h;return -1;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let consumed_assignment_ownership () =
  let module O = Holyc_lib__Ir.Opcode in
  let descriptions fn =
    Body.body fn.Ir_integer_interpreter.body
    |> Graph.blocks
    |> List.concat_map (fun block ->
        Graph.instructions block |> Seq.instructions |> List.map Seq.description)
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, mutate) ->
          let source =
            "I64 F(I8 noreg x,I8 noreg y,I64 a,I64 b){if(x=(a+1)*(b-2))return \
             x+y;return -1;}42;"
          in
          let fn = compile ~name:"F" mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let symbol = location fn.frame "x" |> Frame.location_symbol in
          mutate (descriptions fn);
          Alcotest.(check bool)
            (label ^ " revokes stack assignment witness")
            true
            (Option.is_none
               (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
          Alcotest.(check bool)
            (label ^ " cannot acquire a new stack assignment witness")
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          ( "changed assignment operator",
            fun items ->
              let assignment =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_assign)
                  items
              in
              Obj.set_field (Obj.repr assignment) 1 (Obj.repr O.Ic_add_equ) );
          ( "changed assignment span",
            fun items ->
              let assignment =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_assign)
                  items
              in
              Obj.set_field (Obj.repr assignment) 7 (Obj.repr None) );
          ( "assignment payload injection",
            fun items ->
              let assignment =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_assign)
                  items
              in
              Obj.set_field (Obj.repr assignment) 5
                (Obj.repr (Some (Seq.Integer 1L))) );
          ( "reordered right-hand operands",
            fun items ->
              let expression =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_mul)
                  items
              in
              Obj.set_field (Obj.repr expression) 2
                (Obj.repr (List.rev expression.operands)) );
          ( "changed right-hand literal",
            fun items ->
              let literal =
                List.find
                  (fun (d : Seq.description) ->
                    d.opcode = O.Ic_imm_i64 && d.payload = Some (Seq.Integer 2L))
                  items
              in
              Obj.set_field (Obj.repr literal) 5
                (Obj.repr (Some (Seq.Integer 3L))) );
          ( "substituted assignment destination",
            fun items ->
              let assignment =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_assign)
                  items
              in
              let loads =
                List.filter
                  (fun (d : Seq.description) -> d.opcode = O.Ic_deref)
                  items
              in
              let y = List.hd (List.rev loads) in
              Obj.set_field (Obj.repr assignment) 2
                (Obj.repr
                   [ List.hd y.operands; List.nth assignment.operands 1 ]) );
          ( "changed assignment result width",
            fun items ->
              let assignment =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_assign)
                  items
              in
              let literal =
                List.find
                  (fun (d : Seq.description) ->
                    d.opcode = O.Ic_imm_i64 && d.payload = Some (Seq.Integer 2L))
                  items
              in
              Obj.set_field (Obj.repr assignment) 4
                (Obj.repr literal.target_type) );
        ];
      let source = "I64 F(I8 noreg x,I64 a){return x=a+1;}42;" in
      let fn = compile ~name:"F" mode source
      and foreign = compile ~name:"F" mode source in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "x" |> Frame.location_symbol in
      Alcotest.(check bool)
        "foreign frame cannot claim stack assignment semantics" true
        (Option.is_none
           (Analysis.selection proof ~frame:foreign.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "foreign body cannot claim stack assignment semantics" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:foreign.body ~symbol));
      let options =
        Int64.logxor (Body.compiler_options fn.body) (Int64.shift_left 1L 34)
      in
      Obj.set_field (Obj.repr fn.body) 8 (Obj.repr options);
      Alcotest.(check bool)
        "forged stack option snapshot cannot acquire proof" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let dependent_initializer_selection () =
  let module O = Holyc_lib__Ir.Opcode in
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            "uninitialized dependency is not proved" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(){I8 x=x+1;return x;}42;";
          "I64 F(){I8 x;I8 y=x+1;return y;}42;";
        ];
      let fn =
        compile ~name:"F" mode "I64 F(){I8 x=127;I8 y=x+1;return y;}42;"
      in
      let proof = Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body) in
      let symbol = location fn.frame "y" |> Frame.location_symbol in
      let entry = Body.body fn.body |> Graph.entry in
      let items = Graph.instructions entry |> Seq.instructions in
      let stores =
        List.filter
          (fun item -> (Seq.description item).opcode = O.Ic_assign)
          items
      in
      let first = List.hd stores and last = List.hd (List.rev stores) in
      let reordered =
        List.map
          (fun item ->
            if item == first then last else if item == last then first else item)
          items
      in
      Obj.set_field (Obj.repr entry) 1 (Obj.repr reordered);
      Alcotest.(check bool)
        "initializer reordering invalidates witness" true
        (Option.is_none
           (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "read before initialization cannot acquire proof" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let straight_line_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile ~name:"F" mode source in
          Alcotest.(check bool)
            ("straight-line source proof: " ^ source)
            true
            (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 c){x%=x;return x;}42;";
          "I64 F(I8 x,I64 c){c+=x;x%=c;return x;}42;";
          "I64 F(I8 x,I8 y,I64 c){x+=c;y+=c;return y+x;}42;";
          "I64 F(I64 x,I64 y,I64 c){x+=c;y+=c;return x+y;}42;";
          "I64 F(I8 x,I8 y,I64 c){x+=c;y+=x;return x+y;}42;";
          "I64 F(I8 x,I8 y,I64 c){x+=c;y+=c;return x+y+0;}42;";
          "I64 F(){I8 x=127;I8 y=x+1;return y;}42;";
          "I64 F(I64 a,I64 b){I64 y=a;I8 x=y+1;if(b)x+=1;return x;}42;";
          "I64 F(){I8 x=127;x+=1;I8 y=x+1;I8 z=y+x;return z;}42;";
          "I64 F(){I8 x=1;x<<=1;x++;return x;}42;";
          "I64 F(){I8 x=255;x++;x++;return x;}42;";
          "I64 F(){I8 x=1,y=2;x<<=1;return x;}42;";
          "I64 F(I64 c,I8 n){I8 x=c;return x;}42;";
          "I64 F(I64 c,I8 n){I8 x=c;x+=c;return x;}42;";
          "I64 F(I64 c){I8 x=255;c++;return x;}42;";
          "I64 F(I8 x,I64 c){x<<=c;return c;}42;";
          "I64 F(I8 x,I64 c){x<<=c;return 42;}42;";
          "I64 F(I8 x,I64 c){return x;x<<=c;}42;";
          "I64 F(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e,I64 f){x<<=c;return \
           x+a+b+d+e+f;}42;";
        ];
      let fn =
        compile mode
          ("I64 F(I8 x,I64 c){"
          ^ String.concat "" (List.init 40 (fun _ -> "c;"))
          ^ "x<<=c;return x;}42;")
      in
      Alcotest.(check bool)
        "bounded straight-line reads preserve selection" true
        (Option.is_some (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unsupported_selection () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let fn = compile mode source in
          Alcotest.(check bool)
            ("unproved allocation remains unknown: " ^ source)
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "I64 F(I8 x,I64 c){if(c)x+=2147483648;return x;}42;";
          "I64 F(I8 x,I64 c){if(c)return x+=c;return x;}42;";
          "I64 F(I8 x,I64 c){I8 *p=&x;if(c)*p+=c;return x;}42;";
          "I64 F(I8 reg R12 x,I64 reg R12 c){if(c)x+=c;return x;}42;";
          "I64 F(I8 x,Bool c){if(c)x+=c;return x;}42;";
          "I64 F(I8 x,I64 c){I8 *p=&x;*p<<=c;return x;}42;";
          "I64 F(){I8 x=255;return x+=2147483648;}42;";
          "I64 F(I8 x){return x+=2147483648;}42;";
          "I64 F(){I8 x=1;I8 *p=&x;*p<<=1;return x;}42;";
          "I64 F(){I8 x;x<<=1;return x;}42;";
          "I64 F(){Bool x=1;x<<=1;return x;}42;";
          "I64 F(I64 c,Bool n){I8 x=c;x+=n;return x;}42;";
          "I64 F(I64 c,I8 n){I8 x=c;x/=2;return x;}42;";
          "I64 F(Bool c){I8 x=c;return x;}42;";
          "I64 F(I64 c){I8 x=c;x-=2147483648;return x;}42;";
          "I64 F(I64 c){I8 x=c;x+=2147483648;return x;}42;";
          "I64 F(){I8 x=255;x/=2;return x;}42;";
          "I64 F(){I8 x=1;x|=256;return x;}42;";
          "I64 F(){I8 x=1;x^=256;return x;}42;";
          "I64 F(){I8 x=1;x&=255;return x;}42;";
          "I64 F(){I8 x=255;x+=0x8000000000000000;return x;}42;";
          "I64 F(){I8 x=255;x+=2147483648;return x;}42;";
          "I64 F(){I8 x=255;x-=0x8000000000000000;return x;}42;";
          "I64 F(){I8 x=255;x-=2147483648;return x;}42;";
          "I64 F(I8 x,I64 c){"
          ^ String.concat "" (List.init 200 (fun _ -> "c;"))
          ^ "x<<=c;return x;}42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let ownership () =
  List.iter
    (fun mode ->
      let source = "I64 F(I8 x,I64 c){x<<=c;return x;}42;" in
      let original = compile mode source and foreign = compile mode source in
      let proof =
        Option.get (Analysis.analyze ~frame:original.frame ~body:original.body)
      in
      let x = Frame.location_symbol (location original.frame "x") in
      Alcotest.(check bool)
        "foreign frame" true
        (Option.is_none
           (Analysis.selection proof ~frame:foreign.frame ~body:original.body
              ~symbol:x));
      Alcotest.(check bool)
        "foreign body" true
        (Option.is_none
           (Analysis.selection proof ~frame:original.frame ~body:foreign.body
              ~symbol:x));
      let cell =
        Body.body original.body |> Graph.blocks |> List.hd |> Graph.instructions
        |> Seq.instructions
      in
      let description = Seq.description (List.hd cell) in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { description with span = None });
      Alcotest.(check bool)
        "changed original graph" true
        (Option.is_none
           (Analysis.selection proof ~frame:original.frame ~body:original.body
              ~symbol:x));
      let changed = compile mode source in
      let checked =
        Option.get (Analysis.analyze ~frame:changed.frame ~body:changed.body)
      in
      let changed_x = Frame.location_symbol (location changed.frame "x") in
      let description =
        Body.body changed.body |> Graph.blocks |> List.hd |> Graph.instructions
        |> Seq.instructions |> List.hd |> Seq.description
      in
      Obj.set_field (Obj.repr description) 6 (Obj.repr 1L);
      Alcotest.(check bool)
        "in-place flag change" true
        (Option.is_none
           (Analysis.selection checked ~frame:changed.frame ~body:changed.body
              ~symbol:changed_x));
      let changed_option = compile mode source in
      let option_proof =
        Option.get
          (Analysis.analyze ~frame:changed_option.frame
             ~body:changed_option.body)
      in
      let option_x =
        Frame.location_symbol (location changed_option.frame "x")
      in
      let altered_options =
        Int64.logor
          (Body.compiler_options changed_option.body)
          (Int64.shift_left 1L 34)
      in
      Obj.set_field (Obj.repr changed_option.body) 8 (Obj.repr altered_options);
      Alcotest.(check bool)
        "changed options invalidate proof" true
        (Option.is_none
           (Analysis.selection option_proof ~frame:changed_option.frame
              ~body:changed_option.body ~symbol:option_x));
      Alcotest.(check bool)
        "forged option snapshot cannot be reproved" true
        (Option.is_none
           (Analysis.analyze ~frame:changed_option.frame
              ~body:changed_option.body));
      List.iter
        (fun bit ->
          let changed = compile mode source in
          let forged =
            Int64.logxor
              (Body.compiler_options changed.body)
              (Int64.shift_left 1L bit)
          in
          Obj.set_field (Obj.repr changed.body) 8 (Obj.repr forged);
          Alcotest.(check bool)
            "warning snapshot requires original definition" true
            (Option.is_none
               (Analysis.analyze ~frame:changed.frame ~body:changed.body)))
        [ 16; 17; 18; 19 ];
      let changed_slot = compile mode source in
      let slot_proof =
        Option.get
          (Analysis.analyze ~frame:changed_slot.frame ~body:changed_slot.body)
      in
      let slot_location = location changed_slot.frame "x" in
      let slot_symbol = Frame.location_symbol slot_location in
      let slot = Option.get (Frame.location_frame_slot slot_location) in
      Obj.set_field (Obj.repr slot) 0 (Obj.repr 999L);
      Alcotest.(check bool)
        "changed frame displacement invalidates proof" true
        (Option.is_none
           (Analysis.selection slot_proof ~frame:changed_slot.frame
              ~body:changed_slot.body ~symbol:slot_symbol));
      let changed_source = compile mode source in
      let source_proof =
        Option.get
          (Analysis.analyze ~frame:changed_source.frame
             ~body:changed_source.body)
      in
      let source_symbol =
        Frame.location_symbol (location changed_source.frame "x")
      in
      let typed = Option.get (Body.definition_source changed_source.body) in
      let statement =
        List.hd
          (Semantic_function_call_expression_result
           .function_expression_statements typed)
      in
      let expression =
        Semantic_function_call_expression_result.expression_statement_value
          statement
        |> Semantic_function_call_expression_result.result_source
        |> Semantic_function_call_resolution.argument_expression_source
        |> Option.get
      in
      (match expression with
      | Ast.Binary_expression binary ->
          Obj.set_field (Obj.repr binary) 2
            (Obj.repr (Option.get (Operator.find_binary ">>=")))
      | _ -> failwith "expected original binary expression");
      Alcotest.(check bool)
        "changed typed source invalidates proof" true
        (Option.is_none
           (Analysis.selection source_proof ~frame:changed_source.frame
              ~body:changed_source.body ~symbol:source_symbol));
      let changed_frame = compile mode source in
      let frame_proof =
        Option.get
          (Analysis.analyze ~frame:changed_frame.frame ~body:changed_frame.body)
      in
      let original_location = location changed_frame.frame "x" in
      let original_symbol = Frame.location_symbol original_location in
      let other_symbol = Frame.location_symbol (location foreign.frame "x") in
      Obj.set_field (Obj.repr original_location) 2 (Obj.repr other_symbol);
      Alcotest.(check bool)
        "changed frame location identity" true
        (Option.is_none
           (Analysis.selection frame_proof ~frame:changed_frame.frame
              ~body:changed_frame.body ~symbol:original_symbol)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let scored_ownership () =
  let module Exact = Holyc_lib__Ir.Exact_register_scores in
  let module Typed = Semantic_function_call_expression_result in
  let module Source = Semantic_function_call_resolution in
  let module Ast = Holyc_lib__Frontend.Ast in
  let module O = Ir_opcode in
  let source =
    "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I8 h,I8 i){return \
     a+b+c+d+e+f+g+h+i;}42;"
  in
  let typed fn =
    Option.get (Body.definition_source fn.Ir_integer_interpreter.body)
  in
  let original fn = Option.get (Typed.function_source_body (typed fn)) in
  let items fn =
    Body.body fn.Ir_integer_interpreter.body
    |> Graph.entry |> Graph.instructions |> Seq.instructions
    |> List.map Seq.description
  in
  let mutations =
    [
      ( "frame member order",
        fun fn ->
          Obj.set_field
            (Obj.repr fn.Ir_integer_interpreter.frame)
            4
            (Obj.repr (List.rev (Frame.function_locations fn.frame))) );
      ( "binding ordinal",
        fun fn ->
          Obj.set_field
            (Obj.repr
               (Frame.location_binding
                  (location fn.Ir_integer_interpreter.frame "a")))
            2 (Obj.repr 8) );
      ( "body member position",
        fun fn ->
          Obj.set_field
            (Obj.repr
               (List.hd (Body.parameters fn.Ir_integer_interpreter.body)))
            0 (Obj.repr 8) );
      ( "missing read span",
        fun fn ->
          let read =
            List.find
              (fun (item : Seq.description) -> item.opcode = O.Ic_deref)
              (items fn)
          in
          Obj.set_field (Obj.repr read) 7 (Obj.repr None) );
      ( "changed member offset",
        fun fn ->
          let offset =
            List.find
              (fun (item : Seq.description) -> item.opcode = O.Ic_imm_i64)
              (items fn)
          in
          Obj.set_field (Obj.repr offset) 5 (Obj.repr (Some (Seq.Integer 999L)))
      );
      ( "shared read address",
        fun fn ->
          let reads =
            List.filter
              (fun (item : Seq.description) -> item.opcode = O.Ic_deref)
              (items fn)
          in
          Obj.set_field
            (Obj.repr (List.nth reads 1))
            2
            (Obj.repr (List.hd reads).operands) );
      ( "omitted body receipt",
        fun fn -> Obj.set_field (Obj.repr (typed fn)) 14 (Obj.repr None) );
      ( "copied body receipt owner",
        fun fn ->
          let source = Obj.repr (typed fn) in
          let owner = Obj.field (Obj.field source 14) 0 in
          Obj.set_field source 14 (Obj.repr (Some (Obj.obj (Obj.dup owner)))) );
      ( "copied original body",
        fun fn ->
          let source = Obj.repr (typed fn) in
          let owner = Obj.field (Obj.field source 14) 0 in
          let definition = Obj.field owner 12 in
          Obj.set_field owner 12 (Obj.dup definition) );
      ( "changed original parameter order",
        fun fn ->
          let definition = original fn in
          Obj.set_field (Obj.repr definition) 5
            (Obj.repr (List.rev definition.parameters)) );
      ( "added untyped try statement",
        fun fn ->
          let definition = original fn in
          match definition.body with
          | Some (Ast.Block_statement block) ->
              let statement = List.hd block.block_statements in
              let location = Ast.statement_location statement in
              let tried =
                Ast.Try_catch_statement
                  (Ast.make_try_catch_statement ~try_keyword:location
                     ~try_body:statement ~catch_keyword:location
                     ~catch_body:statement ~location)
              in
              Obj.set_field (Obj.repr block) 1
                (Obj.repr (tried :: block.block_statements))
          | _ -> assert false );
      ( "synchronized semantic children and canonical IR",
        fun fn ->
          let returned = List.hd (Typed.function_returns (typed fn)) in
          let value = Option.get (Typed.return_value returned) in
          let left, right = Option.get (Typed.result_binary_operands value) in
          let binary =
            match
              Source.argument_expression_kind (Typed.result_source value)
            with
            | Source.Binary_expression binary -> binary
            | _ -> assert false
          in
          Obj.set_field (Obj.repr value) 4 (Obj.repr (Some (right, left)));
          Obj.set_field (Obj.repr binary) 2
            (Obj.repr (Typed.result_source right));
          Obj.set_field (Obj.repr binary) 3
            (Obj.repr (Typed.result_source left));
          let entry = Body.body fn.Ir_integer_interpreter.body |> Graph.entry in
          let code = items fn in
          let first = List.hd code in
          let leave =
            match (List.hd (List.rev code)).payload with
            | Some (Seq.Block leave) -> leave
            | _ -> assert false
          in
          let module Return = Holyc_lib__Ir.Return_lowering in
          let lowered =
            Return.lower_function_return ~frame:fn.frame ~optimize_shifts:true
              ~optimize_division:true ~instruction_id:first.instruction_id
              ~value_id:(Option.get first.result).value_id ~leave returned
          in
          let sequence =
            match lowered with
            | Ok (Return.Lowered result) -> Return.sequence result
            | _ -> Alcotest.fail "mutation must still lower canonically"
          in
          Obj.set_field (Obj.repr entry) 1 (Obj.repr sequence) );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, mutate) ->
          let fn = compile mode source in
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          let symbol = location fn.frame "g" |> Frame.location_symbol in
          Alcotest.(check (option int))
            (label ^ " starts exact") (Some 12)
            (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
               ~symbol);
          mutate fn;
          Alcotest.(check bool)
            (label ^ " rejects saved proof")
            true
            (Option.is_none
               (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
          Alcotest.(check bool)
            (label ^ " rejects fresh score proof")
            true
            (Option.is_none (Exact.analyze ~frame:fn.frame ~body:fn.body));
          Alcotest.(check bool)
            (label ^ " cannot recover through fallback")
            true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        mutations;
      let fn =
        compile ~name:"F" mode
          "extern U0 SysTry(U8 *catch_start,U8 *untry);extern U0 \
           SysUntry();I64 F(I8 a){try{}catch{}return a;}42;"
      in
      Alcotest.(check bool)
        "empty original try disables allocation" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      let definition = original fn in
      (match definition.body with
      | Some (Ast.Block_statement block) ->
          Obj.set_field (Obj.repr block) 1
            (Obj.repr
               (List.filter
                  (function
                    | Ast.Try_catch_statement _ -> false
                    | _ -> true)
                  block.block_statements))
      | _ -> assert false);
      Alcotest.(check bool)
        "deleting original empty try cannot mint authority" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let scored_selection () =
  let module Exact = Holyc_lib__Ir.Exact_register_scores in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let fn = compile mode source in
          let scores = Exact.analyze ~frame:fn.frame ~body:fn.body in
          Alcotest.(check bool)
            ("complete exact source: " ^ source)
            true (Option.is_some scores);
          let proof =
            Option.get (Analysis.analyze ~frame:fn.frame ~body:fn.body)
          in
          List.iter
            (fun (name, register, selected) ->
              let symbol = location fn.frame name |> Frame.location_symbol in
              Alcotest.(check (option int))
                ("source-scored register " ^ name)
                register
                (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
                   ~symbol);
              Alcotest.(check bool)
                ("source-scored storage " ^ name)
                true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some selected))
            expected)
        [
          ( "I64 F(I8 a,I8 b){return a+b;}42;",
            [
              ("a", Some 7, Analysis.Register_candidate);
              ("b", Some 6, Analysis.Register_candidate);
            ] );
          ( "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,I8 g,I8 h,I8 i){return \
             a+b+c+d+e+f+g+h+i;}42;",
            [
              ("g", Some 12, Analysis.Register_candidate);
              ("f", Some 13, Analysis.Register_candidate);
              ("i", Some 6, Analysis.Register_candidate);
              ("h", Some 7, Analysis.Register_candidate);
              ("e", Some 10, Analysis.Register_candidate);
              ("b", Some 11, Analysis.Register_candidate);
              ("a", Some 14, Analysis.Register_candidate);
              ("d", Some 15, Analysis.Register_candidate);
              ("c", None, Analysis.Stack);
            ] );
          ( "I64 F(I64 a,I64 b,I64 c,I64 d){return c+d;}42;",
            [
              ("c", Some 6, Analysis.Register_candidate);
              ("d", Some 7, Analysis.Register_candidate);
              ("a", None, Analysis.Unknown);
              ("b", None, Analysis.Unknown);
            ] );
          ( "I64 F(I8 a,I64 reg unused){a+=1;return a;}42;",
            [
              ("a", Some 7, Analysis.Register_candidate);
              ("unused", None, Analysis.Unknown);
            ] );
          ( "I64 F(){I8 a=127,b=127;a+=1;b+=1;return a+b;}42;",
            [
              ("a", Some 7, Analysis.Register_candidate);
              ("b", Some 6, Analysis.Register_candidate);
            ] );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

(* Literal expectations independently derived from OptPass3 and QSort2b.
   The flag is an ordinary tenth candidate in weighted cases. A has three
   accesses, all others one: [a,h,g,flag,i,f,c,b,e,d], with e/d spilled.
   The nine-member weighted permutation is [a,g,f,i,h,c,b,e,d]. *)
let whole_scored_selection () =
  let names = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ] in
  let parameters =
    String.concat "," (List.map (fun name -> "I8 " ^ name) names)
  in
  let sum = String.concat "+" names in
  let updates = String.concat "" (List.map (fun name -> name ^ "+=1;") names) in
  let weighted =
    [
      ("a", 12);
      ("h", 13);
      ("g", 6);
      ("flag", 7);
      ("i", 10);
      ("f", 11);
      ("c", 14);
      ("b", 15);
    ]
  in
  let nine_weighted =
    [
      ("a", 12);
      ("g", 13);
      ("f", 6);
      ("i", 7);
      ("h", 10);
      ("c", 11);
      ("b", 14);
      ("e", 15);
    ]
  in
  let tied =
    [
      ("g", 12);
      ("f", 13);
      ("i", 6);
      ("h", 7);
      ("e", 10);
      ("b", 11);
      ("a", 14);
      ("d", 15);
    ]
  in
  let definition ?(flag = "I64 flag") body =
    "I64 F(" ^ parameters ^ "," ^ flag ^ "){" ^ body ^ "}42;"
  in
  let compile mode contents =
    let session = Session.create () in
    let source =
      Session.add_source session ~path:"whole-register-proof.hc" ~contents
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
    in
    match compile_integer_program session ~config ~source with
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (d : Diagnostic.t) -> d.message) errors))
    | Ok program ->
        let fn =
          List.find
            (fun (fn : Ir_integer_interpreter.function_definition) ->
              Symbol.name (Body.symbol fn.body) = "F")
            (integer_program_functions program.value)
        in
        (fn, integer_program_runtime_calls program.value)
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected, spills) ->
          let fn, runtime = compile mode source in
          let proof =
            match
              Analysis.analyze_with_runtime ~runtime ~frame:fn.frame
                ~body:fn.body
            with
            | Some proof -> proof
            | None -> Alcotest.fail (label ^ ": missing whole-function proof")
          in
          List.iter
            (fun name ->
              let symbol = location fn.frame name |> Frame.location_symbol in
              let expected = List.assoc_opt name expected in
              Alcotest.(check (option int))
                (label ^ " register " ^ name)
                expected
                (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body
                   ~symbol);
              Alcotest.(check bool)
                (label ^ " selection " ^ name)
                true
                (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
                = Some
                    (if Option.is_some expected then Analysis.Register_candidate
                     else Analysis.Stack)))
            (List.map fst expected @ spills))
        [
          ( "weighted branch",
            definition ("if(flag)a=a+1;return " ^ sum ^ ";"),
            weighted,
            [ "d"; "e" ] );
          ( "equal branch arms",
            definition ~flag:"I64 noreg flag"
              ("if(flag){" ^ updates ^ "}return " ^ sum ^ ";"),
            tied,
            [ "c" ] );
          ( "multiple returns",
            definition "if(flag)return a+a+a;return b+c+d+e+f+g+h+i;",
            weighted,
            [ "d"; "e" ] );
          ( "zero trip loop",
            "I64 F(" ^ parameters ^ "){while(0)a=a+1;return " ^ sum ^ ";}42;",
            nine_weighted,
            [ "d" ] );
          ( "dead-looking arm",
            "I64 F(" ^ parameters ^ "){if(0)a=a+1;return " ^ sum ^ ";}42;",
            nine_weighted,
            [ "d" ] );
          ( "after return",
            "I64 F(" ^ parameters ^ "){a+=1;return " ^ sum ^ ";a;a;}42;",
            nine_weighted,
            [ "d" ] );
          ( "while static counts",
            definition ~flag:"I64 noreg flag"
              ("while(flag>0){" ^ updates ^ "flag-=1;}return " ^ sum ^ ";"),
            tied,
            [ "c" ] );
          ( "do static counts",
            definition ~flag:"I64 noreg flag"
              ("do{" ^ updates ^ "}while(flag);return " ^ sum ^ ";"),
            tied,
            [ "c" ] );
          ( "for static counts",
            definition ~flag:"I64 noreg flag"
              ("for(flag=0;flag<1;flag++){" ^ updates ^ "}return " ^ sum ^ ";"),
            tied,
            [ "c" ] );
          ( "named call arguments",
            "I64 G(I64 x){return x;}"
            ^ definition ("if(flag)a=G(a+1);return " ^ sum ^ ";"),
            weighted,
            [ "d"; "e" ] );
          ( "recursive arguments",
            definition
              ("if(flag)return F(a,b,c,d,e,f,g,h,i,flag-1);a+=1;return " ^ sum
             ^ ";"),
            weighted,
            [ "d"; "e" ] );
          ( "unused explicit reservation",
            "I64 F(" ^ parameters
            ^ ",I64 reg R12 unused,I64 noreg flag){if(flag){" ^ updates
            ^ "}return " ^ sum ^ ";}42;",
            [
              ("g", 13);
              ("f", 6);
              ("i", 7);
              ("h", 10);
              ("e", 11);
              ("b", 14);
              ("a", 15);
            ],
            [ "c"; "d" ] );
          ( "mixed forced member",
            "I64 F(" ^ parameters
            ^ ",F64 reg real,I64 noreg flag){if(flag){real=real+1.0;" ^ updates
            ^ "}return " ^ sum ^ ";}42;",
            [
              ("real", 12);
              ("g", 13);
              ("f", 6);
              ("i", 7);
              ("h", 10);
              ("b", 11);
              ("a", 14);
              ("c", 15);
            ],
            [ "d"; "e" ] );
        ];
      let call_source =
        "I64 G(I64 x){return x;}"
        ^ definition ("if(flag)a=G(a+1);return " ^ sum ^ ";")
      in
      let fn, runtime = compile mode call_source in
      let foreign, foreign_runtime = compile mode call_source in
      Alcotest.(check bool)
        "whole call pressure requires original runtime receipts" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      Alcotest.(check bool)
        "foreign runtime cannot mint whole call scores" true
        (Option.is_none
           (Analysis.analyze_with_runtime ~runtime:foreign_runtime
              ~frame:fn.frame ~body:fn.body));
      Alcotest.(check bool)
        "original runtime cannot authorize foreign whole body" true
        (Option.is_none
           (Analysis.analyze_with_runtime ~runtime ~frame:foreign.frame
              ~body:foreign.body));
      (* Reconstructing a canonical graph must not excuse changed original
       statements, control edges, source accesses or definition options. *)
      List.iter
        (fun (label, mutate) ->
          let fn, runtime =
            compile mode (definition ("if(flag)a=a+1;return " ^ sum ^ ";"))
          in
          let proof =
            Option.get
              (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame
                 ~body:fn.body)
          in
          let symbol = location fn.frame "a" |> Frame.location_symbol in
          mutate fn;
          Alcotest.(check bool)
            (label ^ " revokes saved proof")
            true
            (Option.is_none
               (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol));
          Alcotest.(check bool)
            (label ^ " cannot mint a new pressure proof")
            true
            (Option.is_none
               (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame
                  ~body:fn.body)))
        [
          ( "changed branch edge",
            fun fn ->
              let entry =
                Graph.entry (Body.body fn.Ir_integer_interpreter.body)
              in
              let branch =
                Graph.instructions entry |> Seq.instructions
                |> List.map Seq.description
                |> List.find (fun (d : Seq.description) ->
                    d.opcode = Ir_opcode.Ic_br_zero)
              in
              Obj.set_field (Obj.repr branch) 5
                (Obj.repr (Some (Seq.Block (Graph.block_id entry)))) );
          ( "changed access payload",
            fun fn ->
              let code =
                Graph.blocks (Body.body fn.Ir_integer_interpreter.body)
                |> List.concat_map (fun b ->
                    Graph.instructions b |> Seq.instructions
                    |> List.map Seq.description)
              in
              let item =
                List.find
                  (fun (d : Seq.description) -> d.opcode = Ir_opcode.Ic_imm_i64)
                  code
              in
              Obj.set_field (Obj.repr item) 5
                (Obj.repr (Some (Seq.Integer 999L))) );
          ( "changed nested original arm",
            fun fn ->
              let module T = Semantic_function_call_expression_result in
              let original =
                Option.get
                  (T.function_source_body
                     (Option.get
                        (Body.definition_source fn.Ir_integer_interpreter.body)))
              in
              match original.Holyc_lib__Frontend.Ast.body with
              | Some (Holyc_lib__Frontend.Ast.Block_statement block) ->
                  Obj.set_field (Obj.repr block) 1
                    (Obj.repr (List.rev block.block_statements))
              | _ -> assert false );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Register variable analysis"
    [
      ( "source",
        [
          Alcotest.test_case "exact scored selection" `Quick scored_selection;
          Alcotest.test_case "whole-function scored selection" `Quick
            whole_scored_selection;
          Alcotest.test_case "exact scored ownership" `Quick scored_ownership;
          Alcotest.test_case "reference controls" `Quick source_controls;
          Alcotest.test_case "bounded simple selection" `Quick simple_selection;
          Alcotest.test_case "multi-update selection" `Quick sequence_selection;
          Alcotest.test_case "remainder selection" `Quick remainder_selection;
          Alcotest.test_case "repeated update selection" `Quick
            repeated_selection;
          Alcotest.test_case "two-update selection" `Quick pair_selection;
          Alcotest.test_case "multi-parameter selection" `Quick
            many_parameter_selection;
          Alcotest.test_case "explicit register selection" `Quick
            explicit_register_selection;
          Alcotest.test_case "bounded physical register selection" `Quick
            physical_register_selection;
          Alcotest.test_case "physical register ownership" `Quick
            physical_register_ownership;
          Alcotest.test_case "undefined suffix register selection" `Quick
            undefined_suffix_register_selection;
          Alcotest.test_case "automatic physical register selection" `Quick
            automatic_physical_register_selection;
          Alcotest.test_case "unnamed physical register parameters" `Quick
            physical_register_unnamed_parameters;
          Alcotest.test_case "physical parameter source constructors" `Quick
            physical_register_parameter_source_constructors;
          Alcotest.test_case "automatic physical register options" `Quick
            automatic_physical_register_options;
          Alcotest.test_case "automatic physical register ownership" `Quick
            automatic_physical_register_ownership;
          Alcotest.test_case "parameter-initialized local selection" `Quick
            local_parameter_selection;
          Alcotest.test_case "dynamic parameter local selection" `Quick
            dynamic_parameter_local_selection;
          Alcotest.test_case "dynamic count width selection" `Quick
            dynamic_count_width_selection;
          Alcotest.test_case "narrow initializer selection" `Quick
            narrow_initializer_selection;
          Alcotest.test_case "literal parameter selection" `Quick
            literal_parameter_selection;
          Alcotest.test_case "unary selection" `Quick unary_selection;
          Alcotest.test_case "global immediate proof" `Quick
            global_immediate_proof;
          Alcotest.test_case "bounded local selection" `Quick local_selection;
          Alcotest.test_case "compound return selection" `Quick
            compound_return_selection;
          Alcotest.test_case "local return selection" `Quick
            local_return_selection;
          Alcotest.test_case "local ownership" `Quick local_ownership;
          Alcotest.test_case "control-flow parameter selection" `Quick
            control_flow_selection;
          Alcotest.test_case "direct-call parameter selection" `Quick
            direct_call_selection;
          Alcotest.test_case "local control-flow selection" `Quick
            local_control_selection;
          Alcotest.test_case "register bank capacity" `Quick
            register_bank_selection;
          Alcotest.test_case "literal and unary loop updates" `Quick
            loop_update_selection;
          Alcotest.test_case "loop update ownership" `Quick
            loop_update_ownership;
          Alcotest.test_case "forced register pressure" `Quick
            forced_pressure_selection;
          Alcotest.test_case "computed update ownership" `Quick
            computed_update_selection;
          Alcotest.test_case "integer expression ownership" `Quick
            integer_expression_selection;
          Alcotest.test_case "computed initializer ownership" `Quick
            computed_initializer_selection;
          Alcotest.test_case "computed assignment ownership" `Quick
            computed_assignment_selection;
          Alcotest.test_case "parameter and local reassignment" `Quick
            assignment_selection;
          Alcotest.test_case "consumed register and stack assignments" `Quick
            consumed_assignment_selection;
          Alcotest.test_case "consumed stack assignment ownership" `Quick
            consumed_assignment_ownership;
          Alcotest.test_case "ordered local initializers" `Quick
            dependent_initializer_selection;
          Alcotest.test_case "whole straight-line functions" `Quick
            straight_line_selection;
          Alcotest.test_case "unsupported shapes" `Quick unsupported_selection;
          Alcotest.test_case "original ownership" `Quick ownership;
        ] );
    ]
