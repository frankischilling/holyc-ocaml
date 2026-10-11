open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write path bytes =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes)

let count = ref 0

let invoke dir status args =
  let out = Filename.concat dir "stdout"
  and err = Filename.concat dir "stderr" in
  let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
  let out_fd = Unix.openfile out flags 0o600
  and err_fd = Unix.openfile err flags 0o600 in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close out_fd;
        Unix.close err_fd)
      (fun () ->
        Unix.create_process Sys.argv.(1)
          (Array.of_list (Sys.argv.(1) :: args))
          Unix.stdin out_fd err_fd)
  in
  let _, result = Unix.waitpid [] pid in
  incr count;
  require
    (result = Unix.WEXITED status)
    (String.concat " " args ^ "\n" ^ read out ^ read err);
  (read out, read err)

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

let () =
  let dir = Filename.temp_file "holyc-disassembly-cli-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () ->
      let binary = Filename.concat dir "code.bin"
      and listing = Filename.concat dir "code.lst"
      and map = Filename.concat dir "code.map" in
      let output, _ =
        invoke dir 0
          [
            "asm";
            "--emit";
            "--format=json";
            "--output=" ^ binary;
            "--position=4096";
            Sys.argv.(2);
          ]
      in
      let encoded = Yojson.Safe.from_string output in
      require
        (encoded |> member "schema" |> to_string = "holyc-assembly-encoding-v1")
        "assembly schema";
      let output, err =
        invoke dir 0
          [
            "disasm";
            "--format=json";
            "--position=4096";
            "--symbol=entry=4096";
            "--listing=" ^ listing;
            "--map=" ^ map;
            binary;
          ]
      in
      require (err = "") "unexpected disassembly diagnostic";
      let report = Yojson.Safe.from_string output in
      require
        (report |> member "schema" |> to_string = "holyc-disassembly-v1")
        "disassembly schema";
      let instructions = report |> member "instructions" |> to_list in
      require (List.length instructions = 4) "all emitted instructions";
      require
        (List.map (fun i -> i |> member "opcode" |> to_string) instructions
        = [ "MOV"; "MOV"; "JMP"; "RET1" ])
        "source canonical spellings";
      require
        (List.nth instructions 2 |> member "jump_target" |> to_string = "4096")
        "branch target";
      require
        (read map
       = "holyc-disassembly-map-v1\n\
          symbol 0000000000001000 entry\n\
          branch 000000000000100F 0000000000001000 entry\n")
        "deterministic branch map";
      let human, _ =
        invoke dir 0
          [ "disasm"; "--position=4096"; "--symbol=entry=4096"; binary ]
      in
      require
        (String.concat "" (String.split_on_char '\r' human) = read listing)
        "public listing and report agree";
      let repeat, _ =
        invoke dir 0
          [
            "disasm";
            "--format=json";
            "--position=4096";
            "--symbol=entry=4096";
            binary;
          ]
      in
      require (repeat = output) "deterministic JSON";
      write binary (bytes "00B83412FF");
      let narrow, _ =
        invoke dir 0
          [
            "disasm";
            "--bits=16";
            "--offset=1";
            "--length=3";
            "--format=json";
            "--byte-limit=5";
            "--instruction-limit=1";
            binary;
          ]
      in
      let instruction =
        Yojson.Safe.from_string narrow
        |> member "instructions" |> to_list |> List.hd
      in
      require
        (instruction |> member "text" |> to_string = "MOV\tU16 AX,I16 1234")
        "selected binary range";
      List.iter
        (fun options ->
          ignore
            (invoke dir 1
               ([ "disasm"; "--format=json" ] @ options @ [ binary ])))
        [
          [ "--byte-limit=4" ];
          [ "--byte-limit=0" ];
          [ "--instruction-limit=0" ];
          [ "--instruction-limit=16777217" ];
          [ "--offset=-1" ];
          [ "--offset=6" ];
          [ "--length=-1" ];
          [ "--length=6" ];
        ];
      ignore (invoke dir 124 [ "disasm"; "--bits=17"; binary ]);
      ignore (invoke dir 1 [ "disasm"; dir ]);
      ignore (invoke dir 1 [ "disasm"; Filename.concat dir "missing.bin" ]);
      write binary (bytes "5548B800");
      write listing "preserved-listing";
      write map "preserved-map";
      let partial, diagnostic =
        invoke dir 1
          [
            "disasm";
            "--format=json";
            "--listing=" ^ listing;
            "--map=" ^ map;
            binary;
          ]
      in
      require (partial = "") "failed decoding publishes no partial report";
      let diagnostic = Yojson.Safe.from_string diagnostic in
      require
        (diagnostic |> member "schema" |> to_string = "holyc-command-error-v1")
        "versioned failure report";
      require
        (String.starts_with ~prefix:"byte 1:"
           (diagnostic |> member "message" |> to_string))
        "original failing binary byte";
      require
        (read listing = "preserved-listing" && read map = "preserved-map")
        "malformed input preserves output files";
      write binary (bytes "55C3");
      ignore (invoke dir 1 [ "disasm"; "--instruction-limit=1"; binary ]);
      ignore (invoke dir 1 [ "disasm"; "--listing=" ^ binary; binary ]);
      require (read binary = bytes "55C3") "input cannot be overwritten";
      if not Sys.win32 then (
        let alias = Filename.concat dir "input-alias.bin" in
        Unix.symlink binary alias;
        ignore (invoke dir 1 [ "disasm"; "--listing=" ^ binary; alias ]);
        require (read alias = bytes "55C3") "symlink input target is preserved";
        ignore (invoke dir 1 [ "disasm"; "--listing=" ^ alias; alias ]);
        require (read alias = bytes "55C3") "symlink input path is preserved");
      ignore
        (invoke dir 1
           [ "disasm"; "--listing=" ^ listing; "--map=" ^ listing; binary ]);
      ignore
        (invoke dir 1
           [ "disasm"; "--symbol=same=0"; "--symbol=same=1"; binary ]);
      write binary "";
      ignore (invoke dir 0 [ "disasm"; "--format=json"; binary ]);
      Printf.printf "checked %d disassembly CLI reports\n" !count)
