open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let contains text fragment =
  let rec search index =
    index + String.length fragment <= String.length text
    && (String.sub text index (String.length fragment) = fragment
       || search (index + 1))
  in
  search 0

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-encoding-cli-" suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      write path contents;
      action path)

let compiler = Sys.argv.(1)
let fixture = Sys.argv.(2)
let count = ref 0

let invoke ?(status = 0) ?(format = "json") ?(emit = true) ?(options = []) mode
    path =
  temporary ".out" "" (fun output_path ->
      temporary ".err" "" (fun error_path ->
          let output_fd =
            Unix.openfile output_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          and error_fd =
            Unix.openfile error_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let args =
            [ compiler; "asm"; "--format=" ^ format; "--mode=" ^ mode ]
            @ (if emit then [ "--emit" ] else [])
            @ options @ [ path ]
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close output_fd;
                Unix.close error_fd)
              (fun () ->
                Unix.create_process compiler (Array.of_list args) Unix.stdin
                  output_fd error_fd)
          in
          let _, reached = Unix.waitpid [] pid in
          incr count;
          let output = read output_path and error = read error_path in
          require
            (reached = Unix.WEXITED status)
            (String.concat " " args ^ "\n" ^ output ^ error);
          (output, error)))

let success ?options mode path =
  let output, error = invoke ?options mode path in
  require (error = "") ("unexpected encoding diagnostics: " ^ error);
  Yojson.Safe.from_string output

let bytes_of_hex text =
  String.init
    (String.length text / 2)
    (fun index ->
      Char.chr (int_of_string ("0x" ^ String.sub text (index * 2) 2)))

(* Independent Intel encodings, also checked with Clang's integrated assembler:
   MOV imm16, MOV imm32, MOVABS imm64, FLD ST1, FADDP ST1,ST0, LOCK ADD. *)
let expected = "b83412b87856341249b88877665544332211d9c1dec1f0490101"

let example_and_artifact mode =
  temporary ".bin" "old contents\000" (fun destination ->
      let json =
        success
          ~options:[ "--output=" ^ destination; "--code-byte-limit=26" ]
          mode fixture
      in
      require
        (json |> member "schema" |> to_string = "holyc-assembly-encoding-v1")
        "encoding schema is missing";
      require
        (json |> member "reference_commit" |> to_string
       = "c26482bb6ad3f80106d28504ec5db3c6a360732c")
        "encoding report lost the TempleOS pin";
      require
        (json |> member "bytes" |> to_string = expected)
        "public example bytes differ from independent encodings";
      require
        (json |> member "code_size" |> to_int = 26)
        "public example has the wrong exact length";
      require
        (read destination = bytes_of_hex expected)
        "raw artifact bytes differ from the checked report";
      let instructions = json |> member "instructions" |> to_list in
      require
        (List.map (fun item -> item |> member "offset" |> to_int) instructions
        = [ 0; 3; 8; 18; 20; 22; 23 ])
        "instruction byte offsets are wrong";
      require
        (List.map (fun item -> item |> member "mode" |> to_int) instructions
        = [ 16; 32; 64; 64; 64; 64; 64 ])
        "USE mode changes did not reach emission";
      require
        (json |> member "assembly" |> member "schema" |> to_string
       = "holyc-assembly-v1")
        "encoding report lost its original parsed source metadata");
  let first, error = invoke mode fixture in
  let second, repeated_error = invoke mode fixture in
  require
    (first = second && error = repeated_error)
    "encoding report is not deterministic";
  let human, error = invoke ~format:"human" mode fixture in
  require
    (error = ""
    && contains human "code-bytes 26"
    && contains human "OpCodes.DD:")
    "human encoding report is incomplete";
  let parsed, error = invoke ~emit:false mode fixture in
  require (error = "") "plain assembly inspection gained a warning";
  require
    (Yojson.Safe.from_string parsed
    |> member "schema" |> to_string = "holyc-assembly-v1")
    "plain asm no longer reports the original parser schema"

let preflight_failures mode =
  temporary ".bin" "sentinel\000contents" (fun destination ->
      let original = read destination in
      let reject ?(options = []) path expected_message =
        let output, error =
          invoke ~status:1
            ~options:(("--output=" ^ destination) :: options)
            mode path
        in
        require (output = "") "failed emission published a success report";
        require
          (contains error expected_message)
          ("missing emission diagnostic: " ^ error);
        require
          (read destination = original)
          "failed preflight replaced the existing output file"
      in
      reject ~options:[ "--code-byte-limit=25" ] fixture "HCASM0002";
      reject ~options:[ "--code-byte-limit=0" ] fixture "HCASM0002";
      reject ~options:[ "--code-byte-limit=16777217" ] fixture "HCASM0002";
      reject ~options:[ "--position=-1" ] fixture "HCASM0003";
      List.iter
        (fun (source, message) ->
          temporary ".asm" source (fun path -> reject path message))
        [
          ("NOP MOV RAX,UnknownTarget", "HCASM0001");
          ("NOP DU8 1;", "HCASM0004");
          ("NOP entry_label: RET", "HCASM0004");
          ("NOP LIST RET", "HCASM0004");
          ("NOP MOV AH,R8u8", "HCASM0001");
        ]);
  temporary ".asm" "NOP" (fun path ->
      let output, error =
        invoke ~status:1 ~options:[ "--position=9223372036854775807" ] mode path
      in
      require
        (output = "" && contains error "HCASM0003")
        "overflowing instruction end position was accepted";
      temporary ".bin" "" (fun destination ->
          Sys.remove destination;
          let output, error =
            invoke ~status:1
              ~options:[ "--code-byte-limit=0"; "--output=" ^ destination ]
              mode path
          in
          require
            (output = "" && error <> "" && not (Sys.file_exists destination))
            "failed preflight created an output artifact"));
  let output, error =
    invoke ~status:1 ~emit:false ~options:[ "--position=1" ] mode fixture
  in
  require
    (output = "" && contains error "require --emit")
    "an emission option was silently ignored without --emit"

let positions_and_relative_boundaries mode =
  List.iter
    (fun (target, expected) ->
      temporary ".asm"
        ("JMP I8 " ^ string_of_int target)
        (fun path ->
          let json = success ~options:[ "--position=256" ] mode path in
          require
            (json |> member "bytes" |> to_string = expected)
            "relative displacement was not based on the instruction end";
          require
            (json |> member "end_position" |> to_string = "0x0000000000000102")
            "relative instruction end metadata is incorrect"))
    [ (130, "eb80"); (258, "eb00"); (385, "eb7f") ];
  List.iter
    (fun target ->
      temporary ".asm"
        ("JMP I8 " ^ string_of_int target)
        (fun path ->
          let output, error =
            invoke ~status:1 ~options:[ "--position=256" ] mode path
          in
          require
            (output = "" && contains error "range")
            "out-of-range rel8 emitted a partial instruction"))
    [ 129; 386 ];
  temporary ".asm" "MOV RAX,FS:[RBP+8]" (fun path ->
      let json = success mode path in
      require
        (json |> member "bytes" |> to_string = "64488b4508")
        "segment override did not precede the REX/opcode bytes")

let macro_diagnostics mode =
  temporary ".asm" "#define BAD MOV AH,R8u8\nNOP BAD\n" (fun path ->
      let output, error = invoke ~status:1 mode path in
      require
        (output = ""
        && contains error "expanded from here"
        && contains error "defined here")
        ("emission error lost macro provenance: " ^ error))

let combined_diagnostics mode =
  let diagnostics error expected_code =
    let items = Yojson.Safe.from_string error |> to_list in
    require
      (List.length items = 2
      && contains error "HCPARSE0176"
      && contains error expected_code)
      ("warning and error were not one complete JSON diagnostic list: " ^ error)
  in
  temporary ".asm" "FLD [RAX]\nMOV AH,R8u8\n" (fun path ->
      let output, error = invoke ~status:1 mode path in
      require (output = "") "warning followed by emission error published bytes";
      diagnostics error "HCASM0001");
  temporary ".asm" "FLD [RAX]\n" (fun path ->
      let output, error = invoke mode path in
      require
        (Yojson.Safe.from_string output |> member "code_size" |> to_int > 0)
        "valid assembly with a warning did not emit";
      require
        (List.length (Yojson.Safe.from_string error |> to_list) = 1)
        "successful emission did not retain its parser warning";
      temporary ".output-directory" "" (fun directory ->
          Sys.remove directory;
          Unix.mkdir directory 0o700;
          Fun.protect
            ~finally:(fun () -> Unix.rmdir directory)
            (fun () ->
              let output, error =
                invoke ~status:1 ~options:[ "--output=" ^ directory ] mode path
              in
              require
                (output = "" && Sys.readdir directory = [||])
                "failed artifact replacement published a result";
              diagnostics error "HCASM0006")))

let () =
  List.iter
    (fun mode ->
      example_and_artifact mode;
      preflight_failures mode;
      positions_and_relative_boundaries mode;
      macro_diagnostics mode;
      combined_diagnostics mode)
    [ "jit"; "aot" ];
  Printf.printf "assembly encoder public CLI: %d checks passed\n" !count
