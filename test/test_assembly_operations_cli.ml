open Yojson.Safe.Util

let require condition message = if not condition then failwith message

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
  let path = Filename.temp_file "holyc-assembly-cli-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      write path contents;
      action path)

let compiler = Sys.argv.(1)
let assembly_fixture = Sys.argv.(2)
let example = Sys.argv.(3)
let count = ref 0

let invoke ?(status = 0) ?(format = "json") ?(options = []) mode command path =
  temporary ".out" "" (fun output ->
      temporary ".err" "" (fun error ->
          let out_fd =
            Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile error [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let args =
            [ compiler; command; "--format=" ^ format; "--mode=" ^ mode ]
            @ options @ [ path ]
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close out_fd;
                Unix.close err_fd)
              (fun () ->
                Unix.create_process compiler (Array.of_list args) Unix.stdin
                  out_fd err_fd)
          in
          let _, reached = Unix.waitpid [] pid in
          incr count;
          let output = read output and error = read error in
          require
            (reached = Unix.WEXITED status)
            (String.concat " " args ^ "\n" ^ output ^ error);
          (output, error)))

let success ?options mode command path =
  let output, error = invoke ?options mode command path in
  require (error = "") ("unexpected successful-command diagnostics: " ^ error);
  Yojson.Safe.from_string output

let rec instructions = function
  | `Assoc fields as json ->
      if List.assoc_opt "kind" fields = Some (`String "instruction") then
        [ json ]
      else List.concat_map (fun (_, value) -> instructions value) fields
  | `List values -> List.concat_map instructions values
  | _ -> []

let opcode operation =
  operation |> member "opcode" |> member "spelling" |> to_string

let selection operation = member "selection" operation

let operand index operation =
  operation |> member "operands" |> to_list |> fun values ->
  List.nth values index

let operand_value index operation = operand index operation |> member "value"

let check_form_source operation =
  let selected = selection operation in
  require
    (selected |> member "form" |> member "source_path" |> to_string
   = "Compiler/OpCodes.DD")
    "selected instruction lost its original table source";
  require
    (selected |> member "form" |> member "source_line" |> to_int > 0)
    "selected instruction has no source row";
  require
    (selected |> member "candidate_count" |> to_int > 0)
    "accepted instruction has no matching original form";
  require
    (selected |> member "estimated_bytes" |> to_int > 0)
    "accepted instruction has no encoded-size estimate"

let fixture_and_example mode =
  let json = success mode "asm" assembly_fixture in
  require
    (json |> member "schema" |> to_string = "holyc-assembly-v1")
    "standalone assembly schema changed";
  require
    (json |> member "reference_commit" |> to_string
   = "c26482bb6ad3f80106d28504ec5db3c6a360732c")
    "assembly report lost the pinned TempleOS revision";
  require
    (json |> member "initial_mode" |> to_int = 64
    && json |> member "final_mode" |> to_int = 64)
    "standalone mode summary is wrong";
  let operations = instructions json in
  require
    (List.map opcode operations
    = [
        "REP_MOVSB";
        "MOV";
        "REP_MOVSB";
        "MOV";
        "MOV";
        "MOV";
        "LOCK";
        "BTS";
        "JZ";
        "FLD";
        "FMUL";
        "FADDP";
        "FXCH";
        "IMUL2";
        "BPT";
        "RET1";
      ])
    "standalone source instruction order changed";
  List.iter check_form_source operations;
  let modes =
    List.map (fun operation -> operation |> member "mode" |> to_int) operations
  in
  require
    (modes = [ 16; 16; 32; 32; 64; 64; 64; 64; 64; 64; 64; 64; 64; 64; 64; 64 ])
    "USE directives did not reach the selected operations";
  let indexed = List.nth operations 4 in
  let address = operand_value 1 indexed in
  require
    (address |> member "size_bytes" |> to_int = 8
    && address |> member "scale" |> to_string = "8"
    && address |> member "register1" |> member "spelling" |> to_string = "RCX"
    && address |> member "register2" |> member "spelling" |> to_string = "RBP"
    && address |> member "immediate" |> member "value" |> to_string = "24")
    "the sized scaled address lost its parsed values";
  require
    (operand 0 indexed |> member "tokens" |> to_list |> List.hd
   |> member "location" |> member "generated_from" <> `Null)
    "macro destination provenance is missing";
  require
    (operand 1 indexed |> member "tokens" |> to_list |> List.hd
   |> member "location" |> member "defined_at" <> `Null)
    "macro memory provenance is missing";
  let segmented = operand_value 1 (List.nth operations 5) in
  require
    (segmented |> member "segment" |> member "spelling" |> to_string = "FS")
    "segment override is missing";
  require
    (segmented |> member "unresolved" |> to_bool)
    "the symbolic displacement was fabricated as a resolved address";
  let branch = List.nth operations 8 in
  require
    (branch |> member "opcode"
     |> member "canonical_spelling"
     |> to_string = "JE"
    && branch |> member "opcode" |> member "source_is_alias" |> to_bool)
    "JZ lost its JE alias identity";
  require
    (selection branch |> member "unresolved" |> to_bool)
    "relative target was incorrectly treated as a finished relocation";
  let example_json = success mode "parse" example in
  let operations = instructions example_json in
  require
    (List.length operations = 17)
    "public example lost block or direct operations";
  require
    ( List.rev operations |> List.map opcode |> fun names ->
      List.filteri (fun index _ -> index < 3) names = [ "BPT"; "ADD"; "MOV" ] )
    "public example lost its direct assembly run";
  List.iter check_form_source operations

let command_modes_and_determinism mode =
  temporary ".ASM" "REP_MOVSB" (fun path ->
      List.iter
        (fun (bits, expected_bytes) ->
          let json =
            success ~options:[ "--bits=" ^ string_of_int bits ] mode "asm" path
          in
          let operation = instructions json |> List.hd in
          require
            (json |> member "initial_mode" |> to_int = bits
            && operation |> member "mode" |> to_int = bits)
            "--bits did not configure standalone assembly";
          require
            (selection operation |> member "form" |> member "opcode_bytes"
           |> to_list |> List.map to_int = expected_bytes)
            "--bits selected the wrong original REP_MOVSB form")
        [
          (16, [ 0xf3; 0xa4 ]); (32, [ 0xf3; 0xa4 ]); (64, [ 0xf3; 0x48; 0xa4 ]);
        ]);
  List.iter
    (fun format ->
      let first, error = invoke ~format mode "asm" assembly_fixture in
      let second, repeated_error = invoke ~format mode "asm" assembly_fixture in
      require (first = second) (format ^ " assembly output is not deterministic");
      require (error = repeated_error)
        "assembly diagnostics are not deterministic";
      require (String.length first > 100) (format ^ " assembly dump is empty"))
    [ "human"; "json" ]

let failures mode =
  List.iter
    (fun (source, code) ->
      temporary ".ASM" source (fun path ->
          let output, error = invoke ~status:1 mode "asm" path in
          require (output = "") "rejected assembly published a successful dump";
          let diagnostic =
            Yojson.Safe.from_string error
            |> to_list
            |> List.find (fun diagnostic ->
                diagnostic |> member "severity" |> to_string = "error")
          in
          require
            (diagnostic |> member "code" |> to_string = code)
            (source ^ ": unexpected diagnostic: " ^ error);
          require
            (diagnostic |> member "primary" |> member "line" |> to_int >= 1)
            "assembly error lost its source line"))
    [
      ("MOV RAX RBX", "HCPARSE0150");
      ("MOV RAX,", "HCPARSE0149");
      ("MOV RAX,U64 [RBP", "HCPARSE0151");
      ("MOV RAX,EAX", "HCPARSE0175");
      ("MOV U64 [RAX],U64 [RBX]", "HCPARSE0175");
      ("FADD ST1,ST2", "HCPARSE0175");
      ("MOV AL,256", "HCPARSE0175");
      ("@@local:: RET", "HCPARSE0178");
    ]

let include_and_macro_diagnostics mode =
  let directory = Filename.temp_dir "holyc-assembly-cli-include-" "" in
  let root = Filename.concat directory "root.ASM" in
  let included = Filename.concat directory "included.HC" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists root then Sys.remove root;
      if Sys.file_exists included then Sys.remove included;
      Unix.rmdir directory)
    (fun () ->
      write root "#include \"included\"";
      write included "#define OP MOV\nOP RAX,U64 8[RBP]";
      let json = success ~options:[ "-I"; directory ] mode "asm" root in
      let operation = instructions json |> List.hd in
      require
        (operation |> member "opcode" |> member "location"
       |> member "generated_from" <> `Null)
        "included macro opcode lost its expansion source";
      write included "#define BAD [RAX*]\nMOV RAX,BAD";
      let output, error =
        invoke ~status:1 ~options:[ "-I"; directory ] mode "asm" root
      in
      require (output = "") "included assembly failure published a dump";
      let diagnostic =
        Yojson.Safe.from_string error
        |> to_list
        |> List.find (fun diagnostic ->
            diagnostic |> member "severity" |> to_string = "error")
      in
      require
        (diagnostic |> member "include_stack" |> to_list |> List.length = 1)
        "included assembly error lost its include stack";
      require
        (diagnostic |> member "secondary" |> to_list |> List.length >= 2)
        "generated assembly error lost its expansion and definition notes")

let quota_failure ~options mode path code =
  let output, error = invoke ~status:1 ~options mode "asm" path in
  require (output = "")
    "exhausted preprocessing quota published an assembly dump";
  let diagnostic =
    Yojson.Safe.from_string error
    |> to_list
    |> List.find (fun diagnostic ->
        diagnostic |> member "severity" |> to_string = "error")
  in
  require
    (diagnostic |> member "code" |> to_string = code)
    ("unexpected preprocessing quota diagnostic: " ^ error);
  require
    (diagnostic |> member "primary" |> member "line" |> to_int >= 1)
    "preprocessing quota diagnostic lost its source";
  diagnostic

let preprocessing_limits mode =
  temporary ".ASM" "#define OP NOP\nOP OP" (fun path ->
      let json =
        success ~options:[ "--generated-byte-limit=6" ] mode "asm" path
      in
      require
        (instructions json |> List.map opcode = [ "NOP"; "NOP" ])
        "the exact generated-byte budget rejected valid assembly";
      let diagnostic =
        quota_failure
          ~options:[ "--generated-byte-limit=5" ]
          mode path "HCPP0013"
      in
      require
        (diagnostic |> member "notes" |> to_list |> List.map to_string
        = [ "bytes already injected: 3"; "replacement bytes: 3" ])
        "assembly macro expansion did not share the generated-byte budget");
  temporary ".ASM" "#define A B\n#define B C\n#define C NOP\nA" (fun path ->
      let json =
        success ~options:[ "--definition-depth-limit=3" ] mode "asm" path
      in
      require
        (instructions json |> List.map opcode = [ "NOP" ])
        "the exact definition-depth limit rejected valid assembly";
      let diagnostic =
        quota_failure
          ~options:[ "--definition-depth-limit=2" ]
          mode path "HCPP0012"
      in
      require
        (diagnostic |> member "secondary" |> to_list |> List.length >= 2)
        "definition-depth failure lost the active expansion trace");
  let directory = Filename.temp_dir "holyc-assembly-cli-depth-" "" in
  let root = Filename.concat directory "root.ASM" in
  let middle = Filename.concat directory "middle.HC" in
  let leaf = Filename.concat directory "leaf.HC" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> if Sys.file_exists path then Sys.remove path)
        [ root; middle; leaf ];
      Unix.rmdir directory)
    (fun () ->
      write root "#include \"middle\"";
      write middle "#include \"leaf\"";
      write leaf "NOP";
      let json =
        success
          ~options:[ "-I"; directory; "--include-depth-limit=2" ]
          mode "asm" root
      in
      require
        (instructions json |> List.map opcode = [ "NOP" ])
        "the root source incorrectly counted toward the include-depth limit";
      let diagnostic =
        quota_failure
          ~options:[ "-I"; directory; "--include-depth-limit=1" ]
          mode root "HCPP0006"
      in
      require
        (diagnostic |> member "include_stack" |> to_list |> List.length = 1)
        "include-depth failure lost the active include trace")

let () =
  List.iter
    (fun mode ->
      fixture_and_example mode;
      command_modes_and_determinism mode;
      failures mode;
      include_and_macro_diagnostics mode;
      preprocessing_limits mode)
    [ "jit"; "aot" ];
  Printf.printf "assembly operations CLI: %d commands passed\n" !count
