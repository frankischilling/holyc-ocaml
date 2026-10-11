open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let contains text fragment =
  let rec loop index =
    index + String.length fragment <= String.length text
    && (String.sub text index (String.length fragment) = fragment
       || loop (index + 1))
  in
  loop 0

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

let temporary_directory action =
  let directory = Filename.temp_file "holyc-layout-cli-" "" in
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

let invoke directory ?(status = 0) ?(emit = true) ?(format = "json") mode path
    options =
  let output_path = Filename.concat directory "stdout.txt"
  and error_path = Filename.concat directory "stderr.txt" in
  let output_fd =
    Unix.openfile output_path
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      0o600
  and error_fd =
    Unix.openfile error_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let args =
    [ compiler; "asm"; "--mode=" ^ mode; "--format=" ^ format ]
    @ (if emit then [ "--emit" ] else [])
    @ options @ [ path ]
  in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close output_fd;
        Unix.close error_fd)
      (fun () ->
        Unix.create_process compiler (Array.of_list args) Unix.stdin output_fd
          error_fd)
  in
  let _, result = Unix.waitpid [] pid in
  incr count;
  let output = read output_path and error = read error_path in
  require
    (result = Unix.WEXITED status)
    (String.concat " " args ^ "\n" ^ output ^ error);
  (output, error)

let success directory mode path options =
  let output, error = invoke directory mode path options in
  require (error = "") ("unexpected assembly diagnostic: " ^ error);
  Yojson.Safe.from_string output

let bytes_of_hex hex =
  String.init
    (String.length hex / 2)
    (fun index ->
      Char.chr (int_of_string ("0x" ^ String.sub hex (index * 2) 2)))

let example mode =
  temporary_directory (fun directory ->
      let binary = Filename.concat directory "image.bin"
      and map = Filename.concat directory "image.map"
      and listing = Filename.concat directory "image.lst" in
      let json =
        success directory mode fixture
          [
            "--output=" ^ binary;
            "--map=" ^ map;
            "--listing=" ^ listing;
            "--code-byte-limit=16";
          ]
      in
      let expected = "eb06aa9090909090488b830f000000c3" in
      require
        (json |> member "bytes" |> to_string = expected)
        "forward layout bytes differ";
      require
        (read binary = bytes_of_hex expected)
        "raw artifact differs from its report";
      require
        (json |> member "initial_bytes" |> to_string
       = "ebfeaa9090909090488b8300000000c3")
        "normalized initial bytes were replaced by final fixups";
      require
        (contains (read map) "0x0000000000000000 export Entry")
        "map lost exported symbol coordinate";
      require
        (contains (read map) "0x0000000000000008 global Tail")
        "map lost ordinary symbol coordinate";
      require
        (contains (read listing) "eb06")
        "listing omitted resolved branch bytes";
      require
        (contains (read listing) "488b830f000000")
        "listing omitted fixed displacement bytes";
      require
        (not (contains (read listing) "909090"))
        "NOLIST output appeared in listing";
      let linked =
        success directory mode fixture [ "--base=0x1003"; "--output=" ^ binary ]
      in
      require
        (linked |> member "schema" |> to_string = "holyc-linked-assembly-v1")
        "linked report schema differs";
      require
        (linked |> member "bytes" |> to_string
       = "eb06aa9090909090488b8312100000c3")
        "loaded displacement did not rebase";
      require
        (linked |> member "image" |> member "bytes" |> to_string = expected)
        "link changed the source image";
      write binary "keep this artifact";
      ignore
        (invoke directory ~status:1 mode fixture
           [ "--code-byte-limit=15"; "--output=" ^ binary ]);
      require
        (read binary = "keep this artifact")
        "size failure replaced an existing artifact";
      ignore
        (invoke directory ~status:1 mode fixture
           [ "--map=" ^ binary; "--output=" ^ binary ]);
      require
        (read binary = "keep this artifact")
        "colliding destinations replaced an artifact";
      let aliases =
        [ Filename.concat (Filename.concat directory ".") "image.bin" ]
        @ if Sys.win32 then [ Filename.concat directory "IMAGE.BIN" ] else []
      in
      List.iter
        (fun alias ->
          ignore
            (invoke directory ~status:1 mode fixture
               [ "--output=" ^ binary; "--map=" ^ alias ]);
          require
            (read binary = "keep this artifact")
            "an aliased map destination replaced the binary")
        aliases;
      ignore
        (invoke directory ~status:1 mode fixture
           [ "--base=9223372036854775807"; "--output=" ^ binary ]);
      require
        (read binary = "keep this artifact")
        "load-address overflow replaced an artifact")

let imports mode =
  temporary_directory (fun directory ->
      let source = Filename.concat directory "imports.ASM"
      and binary = Filename.concat directory "imports.bin"
      and map = Filename.concat directory "imports.map" in
      write source "LIST; IMPORT F; CALL F+7; DU16 F+3; DU64 F;";
      let report = success directory mode source [] in
      require
        (report |> member "bytes" |> to_string
       = "e80700000003000000000000000000")
        "provisional import addends differ";
      require
        (report |> member "relocations" |> to_list |> List.length = 3)
        "import fixups are missing";
      write binary "old binary";
      write map "old map";
      ignore
        (invoke directory ~status:1 mode source
           [ "--output=" ^ binary; "--map=" ^ map ]);
      require
        (read binary = "old binary" && read map = "old map")
        "unresolved output modified artifacts";
      let linked =
        success directory mode source
          [
            "--base=0x1000";
            "--symbol=F=0x1100";
            "--output=" ^ binary;
            "--map=" ^ map;
          ]
      in
      let expected = "e8fb00000000110011000000000000" in
      require
        (linked |> member "bytes" |> to_string = expected)
        "import linker retained provisional addends";
      require
        (read binary = bytes_of_hex expected)
        "linked import artifact differs";
      require
        (contains (read map) "IET_REL_I32 symbol=F")
        "map lost import relocation identity";
      write binary "retain";
      ignore
        (invoke directory ~status:1 mode source
           [ "--base=0"; "--output=" ^ binary ]);
      require (read binary = "retain") "missing import binding modified output";
      ignore
        (invoke directory ~status:1 mode source
           [ "--symbol=F=1"; "--symbol=F=2"; "--output=" ^ binary ]);
      require (read binary = "retain") "duplicate binding modified output";
      write source "IMPORT F; MOV U32 [F+7],9;";
      let linked =
        success directory mode source [ "--base=0x1000"; "--symbol=F=0x1100" ]
      in
      require
        (linked |> member "bytes" |> to_string = "c705fa00000009000000")
        "imported displacement did not use the source field-end rule";
      write source "IMPORT F; DU32 F+F;";
      ignore (invoke directory ~status:1 mode source []);
      write source "CALL Required;";
      ignore (invoke directory ~status:1 mode source [ "--base=0" ]))

let options mode =
  temporary_directory (fun directory ->
      List.iter
        (fun option ->
          ignore
            (invoke directory ~status:1 ~emit:false mode fixture [ option ]))
        [
          "--base=0";
          "--symbol=F=1";
          "--map=" ^ Filename.concat directory "new.map";
          "--listing=" ^ Filename.concat directory "new.lst";
        ];
      require
        (not (Sys.file_exists (Filename.concat directory "new.map")))
        "parse-only invocation wrote a map";
      let output, error =
        invoke directory ~format:"human" mode fixture [ "--base=0x1000" ]
      in
      require
        (error = "" && contains output "loaded-position 0x0000000000001000")
        "human linked report lacks loaded coordinates")

let large_link_budget () =
  temporary_directory (fun directory ->
      let source = Filename.concat directory "large.ASM"
      and binary = Filename.concat directory "large.bin" in
      write source "DU8 1000001 DUP(0);";
      let json =
        success directory "jit" source
          [
            "--base=0";
            "--code-byte-limit=1000001";
            "--directive-work-limit=4000000";
            "--output=" ^ binary;
          ]
      in
      require
        (json |> member "code_size" |> to_int = 1000001)
        "linking did not receive the configured work allowance";
      let bytes = read binary in
      require
        (String.length bytes = 1000001 && String.for_all (( = ) '\000') bytes)
        "large linked output differs from its data directive")

let () =
  List.iter
    (fun mode ->
      example mode;
      imports mode;
      options mode)
    [ "jit"; "aot" ];
  large_link_budget ();
  Printf.printf "assembly layout CLI: %d invocations passed\n" !count
