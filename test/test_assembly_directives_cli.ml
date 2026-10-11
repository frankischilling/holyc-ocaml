open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let contains text fragment =
  let rec at index =
    index + String.length fragment <= String.length text
    && (String.sub text index (String.length fragment) = fragment
       || at (index + 1))
  in
  at 0

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
  let path = Filename.temp_file "holyc-directives-cli-" suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      write path contents;
      action path)

let temporary_directory action =
  let directory = Filename.temp_file "holyc-directives-binfile-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat directory name))
        (Sys.readdir directory);
      Unix.rmdir directory)
    (fun () -> action directory)

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
  require (error = "") ("unexpected directive diagnostic: " ^ error);
  Yojson.Safe.from_string output

let bytes_of_hex text =
  String.init
    (String.length text / 2)
    (fun index ->
      Char.chr (int_of_string ("0x" ^ String.sub text (index * 2) 2)))

(* Fixed from the original directive loops and MOV/RET opcode forms. *)
let expected =
  "23ff410042909090b83412b8785634123412fecafeca7856341288776655443322115a5ac3"

let example_and_artifact mode =
  temporary ".bin" "old contents\000" (fun destination ->
      let json =
        success
          ~options:[ "--output=" ^ destination; "--code-byte-limit=37" ]
          mode fixture
      in
      require
        (json |> member "schema" |> to_string = "holyc-assembly-encoding-v1")
        "directive encoding schema is missing";
      require
        (json |> member "reference_commit" |> to_string
       = "c26482bb6ad3f80106d28504ec5db3c6a360732c")
        "directive report lost the pinned source revision";
      require
        (json |> member "bytes" |> to_string = expected)
        "public directive example differs from original-source byte \
         expectations";
      require
        (json |> member "code_size" |> to_int = 37)
        "public directive example has the wrong length";
      require
        (read destination = bytes_of_hex expected)
        "raw directive output differs from its JSON report";
      require
        (json |> member "origin" |> to_string = "0x0000000000001003")
        "ORG metadata is missing";
      require
        (json |> member "position" |> to_string = "0x0000000000000000"
        && json |> member "end_position" |> to_string = "0x0000000000000025")
        "ORG incorrectly changed raw output positions";
      require
        (json |> member "max_align_bits" |> to_int = 3)
        "ALIGN metadata lost the maximum requested power";
      require
        (json |> member "imports" |> to_list |> List.map to_string
       = [ "ExternalAddress" ])
        "unused import metadata is missing";
      let instructions = json |> member "instructions" |> to_list in
      require
        (List.map (fun item -> item |> member "offset" |> to_int) instructions
        = [ 8; 11; 36 ])
        "instruction offsets ignore preceding directive bytes";
      require
        (List.map (fun item -> item |> member "mode" |> to_int) instructions
        = [ 16; 32; 64 ])
        "USE directives did not change the encoded instruction modes";
      let emitted =
        json |> member "entries" |> to_list
        |> List.filter (fun entry -> entry |> member "size" |> to_int > 0)
      in
      require
        (List.map (fun entry -> entry |> member "listing" |> to_bool) emitted
        = [ false; false; true; true; true; true; true; true; true ])
        "LIST state does not match the source-ordered emitted ranges";
      require
        (json |> member "assembly" |> member "schema" |> to_string
       = "holyc-assembly-v1")
        "directive encoding report lost the original parsed assembly";
      let work = json |> member "work" |> to_int in
      let exact =
        success
          ~options:[ "--directive-work-limit=" ^ string_of_int work ]
          mode fixture
      in
      require
        (exact |> member "bytes" |> to_string = expected)
        "the exact completed work allowance changed output";
      let original = read destination in
      let output, error =
        invoke ~status:1
          ~options:
            [
              "--output=" ^ destination;
              "--directive-work-limit=" ^ string_of_int (work - 1);
            ]
          mode fixture
      in
      require
        (output = "" && error <> "" && read destination = original)
        "work exhaustion replaced the completed artifact or published bytes");
  let first, first_error = invoke mode fixture in
  let second, second_error = invoke mode fixture in
  require
    (first = second && first_error = second_error)
    "directive reports are not deterministic";
  let human, error = invoke ~format:"human" mode fixture in
  require
    (error = "" && contains human "code-bytes 37")
    "human directive report lost the emitted code length";
  let parsed, error = invoke ~emit:false mode fixture in
  require (error = "") "plain directive inspection gained a diagnostic";
  require
    (Yojson.Safe.from_string parsed
    |> member "schema" |> to_string = "holyc-assembly-v1")
    "plain asm no longer returns the parser report"

let preflight_failures mode =
  temporary ".bin" "sentinel\000contents" (fun destination ->
      let original = read destination in
      let reject ?(options = []) path fragment =
        let output, error =
          invoke ~status:1
            ~options:(("--output=" ^ destination) :: options)
            mode path
        in
        require (output = "") "failed directives published a success report";
        require
          (error <> "" && contains error fragment)
          ("missing directive failure detail: " ^ error);
        require
          (read destination = original)
          "failed directives replaced the existing output file"
      in
      reject ~options:[ "--code-byte-limit=36" ] fixture "HCASM0002";
      reject ~options:[ "--code-byte-limit=0" ] fixture "HCASM0002";
      reject ~options:[ "--directive-work-limit=0" ] fixture "";
      reject ~options:[ "--directive-work-limit=-1" ] fixture "";
      List.iter
        (fun (source, fragment) ->
          temporary ".asm" source (fun path -> reject path fragment))
        [
          ("DU8 1; ORG 0;", "ORG");
          ("ORG 0; ORG 0;", "ORG");
          ("DU64 9223372036854775807 DUP(1);", "HCASM0002");
          ("DU16 2 DUP(3 DUP(4));", "HCPARSE");
          ("DU8 2 DUP(1,2);", "HCPARSE");
          ("DU32 2 DUP();", "HCPARSE");
          ("DU8 0 DUP(MissingValue);", "");
          ("IMPORT ExternalValue; DU64 ExternalValue;", "HCASM0009");
          ("IMPORT @@x; @@x: DU8 1;", "HCASM0004");
          ("IMPORT DU8; DU8 1;", "HCPARSE");
          ("BINFILE \"no-such-directive-fixture-713\";", "BINFILE");
        ]);
  temporary ".asm" "DU8 1;" (fun path ->
      temporary ".bin" "" (fun destination ->
          Sys.remove destination;
          let output, error =
            invoke ~status:1
              ~options:
                [ "--output=" ^ destination; "--position=9223372036854775807" ]
              mode path
          in
          require
            (output = "" && contains error "HCASM0003"
            && not (Sys.file_exists destination))
            "position failure created a directive output artifact"));
  List.iter
    (fun option ->
      let output, error =
        invoke ~status:1 ~emit:false ~options:[ option ] mode fixture
      in
      require
        (output = "" && contains error "require --emit")
        "a directive emission option was ignored without --emit")
    [ "--directive-work-limit=1"; "--binary-input-byte-limit=1" ]

let binfile_and_input_limits mode =
  temporary_directory (fun directory ->
      let binary_path = Filename.concat directory "payload.BIN" in
      let source_path = Filename.concat directory "include.asm" in
      let destination = Filename.concat directory "emitted.bin" in
      write binary_path "\x11\000\x22";
      write source_path "DU8 0xAA; BINFILE \"payload\"; DU16 0x1234;";
      let json =
        success
          ~options:
            [
              "--include=" ^ directory;
              "--output=" ^ destination;
              "--code-byte-limit=6";
              "--binary-input-byte-limit=3";
            ]
          mode source_path
      in
      require
        (json |> member "bytes" |> to_string = "aa1100223412"
        && read destination = "\xAA\x11\000\x22\x34\x12")
        "BINFILE did not append the raw file at its directive position";
      require
        (json |> member "input_bytes" |> to_int = 3)
        "binary input consumption is not reported";
      let inputs = json |> member "binary_inputs" |> to_list in
      require
        (List.length inputs = 1
        && List.hd inputs |> member "raw_size" |> to_int = 3
        && List.hd inputs |> member "decoded_size" |> to_int = 3
        && List.hd inputs |> member "compression" |> to_string = "raw")
        "raw BINFILE provenance is incomplete";
      let original = read destination in
      let reject options =
        let output, error =
          invoke ~status:1
            ~options:
              ([ "--include=" ^ directory; "--output=" ^ destination ] @ options)
            mode source_path
        in
        require
          (output = "" && error <> "" && read destination = original)
          "BINFILE limit failure changed an existing artifact"
      in
      reject [ "--binary-input-byte-limit=2" ];
      reject [ "--code-byte-limit=5" ];
      write source_path "BINFILE \"payload\"; BINFILE \"payload\";";
      let repeated =
        success
          ~options:
            [
              "--include=" ^ directory;
              "--code-byte-limit=6";
              "--binary-input-byte-limit=6";
            ]
          mode source_path
      in
      require
        (repeated |> member "bytes" |> to_string = "110022110022"
        && repeated |> member "input_bytes" |> to_int = 6)
        "repeated BINFILE directives did not charge both reads";
      reject [ "--binary-input-byte-limit=5" ])

let modes_and_position_values mode =
  List.iter
    (fun (source, expected) ->
      temporary ".asm" source (fun path ->
          let json = success mode path in
          require
            (json |> member "bytes" |> to_string = expected)
            ("closed instruction or typed expression has the wrong bytes: "
           ^ source)))
    [
      ("MOV RAX,[RBX+$$];", "488b03");
      ("DU8 1; MOV RAX,[RBX+$$];", "01488b4301");
      ("MOV RAX,1.0;", "48b8000000000000f03f");
      ("DU64 (~0(U64))>>63;", "0100000000000000");
      ( "DU64 0`-1,(-4.0)`0.5,(-4.0)`1.5,(-4.0)`-1.5;",
        "0000000000000000000000000000004000000000000020c0000000000000c0bf" );
    ];
  List.iter
    (fun (source, expected, expected_relocations) ->
      temporary ".asm" source (fun path ->
          let json = success mode path in
          require
            (json |> member "bytes" |> to_string = expected)
            ("a later global import displaced a local binding: " ^ source);
          let relocations =
            json |> member "relocations" |> to_list
            |> List.map (fun relocation ->
                ( relocation |> member "offset" |> to_int,
                  relocation |> member "width" |> to_int,
                  relocation |> member "kind" |> to_string ))
          in
          require
            (relocations = expected_relocations)
            "local import precedence changed absolute data relocation metadata"))
    [
      ("@@x: IMPORT @@x; @@x: DU8 1;", "01", []);
      ( "DU64 @@x; IMPORT @@x; @@x: DU8 1;",
        "080000000000000001",
        [ (0, 8, "AAT_ADD_U64") ] );
    ];
  temporary ".asm" "IMPORT if;" (fun path ->
      let json = success mode path in
      require
        (json |> member "bytes" |> to_string = ""
        && json |> member "imports" |> to_list |> List.map to_string = [ "if" ]
        )
        "IMPORT lost a valid original identifier with a keyword spelling");
  temporary ".asm" "ORG 0x1003; DU8 $$,$$; DU16 $$-$$; DU8 2 DUP($$);"
    (fun path ->
      let json = success mode path in
      require
        (json |> member "bytes" |> to_string = "000100000404")
        "current-position values or frozen DUP values include ORG";
      require
        (json |> member "relocations" |> to_list |> List.length = 4)
        "absolute data relocation records lost current-position parity");
  temporary ".asm" "DU8 1; ALIGN 0x8000000000000000,0xCC; DU8 2;" (fun path ->
      let json = success mode path in
      require
        (json |> member "bytes" |> to_string = "0102"
        && json |> member "max_align_bits" |> to_int = 63)
        "the original high-bit ALIGN rule changed");
  temporary ".asm" "DU8 1; ALIGN 16,0xCC;" (fun path ->
      if mode = "aot" then
        let json = success mode path in
        require
          (json |> member "bytes" |> to_string
         = "01cccccccccccccccccccccccccccccc")
          "AOT alignment above eight did not emit the requested padding"
      else
        let output, error = invoke ~status:1 mode path in
        require
          (output = "" && contains error "HCPARSE0179")
          "JIT accepted positive alignment above eight")

let directive_macro_diagnostics mode =
  temporary ".asm" "#define TOO_MUCH DU64 8 DUP(0x7E);\nDU8 1;\nTOO_MUCH\n"
    (fun path ->
      let output, error =
        invoke ~status:1 ~options:[ "--code-byte-limit=8" ] mode path
      in
      require
        (output = "" && contains error "HCASM0002"
        && contains error "expanded from here"
        && contains error "defined here")
        ("directive error lost macro provenance: " ^ error))

let () =
  List.iter
    (fun mode ->
      example_and_artifact mode;
      preflight_failures mode;
      binfile_and_input_limits mode;
      modes_and_position_values mode;
      directive_macro_diagnostics mode)
    [ "jit"; "aot" ];
  Printf.printf "assembly directives public CLI: %d checks passed\n" !count
