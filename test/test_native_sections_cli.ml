open Holyc_lib
open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel contents)

let invoke compiler directory args =
  let out = Filename.concat directory "stdout"
  and err = Filename.concat directory "stderr" in
  let flags = [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] in
  let stdout = Unix.openfile out flags 0o600
  and stderr = Unix.openfile err flags 0o600 in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close stdout;
        Unix.close stderr)
      (fun () ->
        Unix.create_process compiler
          (Array.of_list (compiler :: args))
          Unix.stdin stdout stderr)
  in
  let _, status = Unix.waitpid [] pid in
  require
    (status = Unix.WEXITED 0 && read err = "")
    ("command failed: " ^ read out ^ read err);
  Yojson.Safe.from_string (read out)

let symbol object_ name =
  object_ |> member "symbols" |> to_list
  |> List.find (fun s -> s |> member "name" |> to_string = name)

let assembly compiler directory =
  let source = Filename.concat directory "sections.asm" in
  write source
    "USE64\n\
     IMPORT Ext;\n\
     Entry:: JMP Done;\n\
     DU8 0;\n\
     Done: RET;\n\
     DU32 Entry;\n\
     MOV RAX,Ext;\n";
  List.iter
    (fun mode ->
      let report =
        invoke compiler directory
          [
            "asm";
            "--emit";
            "--format=json";
            "--mode=" ^ mode;
            "--base=0x1000";
            "--symbol=Ext=0x10203040";
            source;
          ]
      in
      let object_ = report |> member "image" |> member "native_sections" in
      require
        (object_ |> member "mode" |> to_string = mode)
        "source compilation mode";
      require
        (object_ |> member "schema" |> to_string = "holyc-native-sections-v1")
        "assembly object schema";
      require
        (symbol object_ "Entry" |> member "binding" |> to_string = "export")
        "source export identity";
      require
        (symbol object_ "Ext" |> member "binding" |> to_string = "import")
        "source import identity";
      require
        (object_ |> member "mappings" |> to_list
        |> List.for_all (fun m ->
            m |> member "source" |> member "span" <> `Null))
        "source map spans";
      require
        (report |> member "bytes" |> to_string = "eb0100c300100000b840302010")
        "literal CLI rebased bytes")
    [ "jit"; "aot" ]

let native compiler directory =
  let text =
    "I64 G=5; I64 F(I64 x) {if (x) return F(x-1)+G; return 1;} F(3);"
  in
  let source = Filename.concat directory "sections.hc" in
  write source text;
  List.iter
    (fun mode ->
      let report =
        invoke compiler directory
          [
            "run";
            "--target=host-jit";
            "--format=json";
            "--mode=" ^ mode;
            source;
          ]
      in
      require
        (report |> member "final_value" |> member "bits" |> to_string
       = "0x0000000000000010")
        "real native source execution";
      let object_ =
        report |> member "native" |> member "image" |> member "native_sections"
      in
      require
        (object_ |> member "schema" |> to_string = "holyc-native-sections-v1")
        "native object schema";
      let session = Session.create () in
      let config =
        match
          Preprocessor.Config.create
            ~compilation_mode:
              (if mode = "jit" then Preprocessor.Jit else Preprocessor.Aot)
            ()
        with
        | Ok x -> x
        | Error e -> failwith e
      in
      let input = Session.add_source session ~path:source ~contents:text in
      let image =
        match Native_program.compile session ~config ~source:input with
        | Ok x -> x.value
        | Error errors ->
            failwith
              (String.concat ";"
                 (List.map (fun (e : Diagnostic.t) -> e.message) errors))
      in
      require
        (object_
        = Native_sections.to_yojson (X86_64_program.native_sections image))
        "native API/CLI object identity";
      require
        (symbol object_ "F" |> member "source_identity" <> `Null)
        "function semantic identity";
      require
        (symbol object_ "G" |> member "source_identity" <> `Null)
        "global semantic identity")
    [ "jit"; "aot" ]

let () =
  require
    (Array.length Sys.argv = 3)
    "usage: test_native_sections_cli <holyc> --assembly|--native";
  let directory = Filename.temp_file "holyc-sections-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat directory name))
        (Sys.readdir directory);
      Unix.rmdir directory)
    (fun () ->
      match Sys.argv.(2) with
      | "--assembly" -> assembly Sys.argv.(1) directory
      | "--native" -> native Sys.argv.(1) directory
      | _ -> failwith "unknown test mode");
  print_endline "Shared native sections: 2 CLI source reports passed."
