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

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let sha value = Digestif.SHA256.(to_hex (digest_string value))

let normalize value =
  let buffer = Buffer.create (String.length value) in
  String.iteri
    (fun i c ->
      if c = '\r' && i + 1 < String.length value && value.[i + 1] = '\n' then ()
      else Buffer.add_char buffer (if c = '\031' then ' ' else c))
    value;
  Buffer.contents buffer

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

let () =
  let metadata = Yojson.Safe.from_file "oracle/bin-modules/native.json" in
  let capture = read "oracle/bin-modules/native.capture" in
  require
    (sha capture = (metadata |> member "capture_sha256" |> to_string))
    "intact native capture";
  require
    (sha (read "oracle/bin-modules/native.hc")
    = (metadata |> member "payload_sha256" |> to_string))
    "intact native payload";
  let expected = metadata |> member "expected" |> to_assoc in
  let observed = Array.make 2 [] in
  String.split_on_char '\n' capture
  |> List.iter (fun line ->
      if String.starts_with ~prefix:"BN|" line then
        match String.split_on_char '|' line with
        | [ _; pass; name; value ] ->
            let pass = int_of_string pass in
            require (pass >= 0 && pass < 2) "native pass";
            require
              (List.assoc name expected = `String value)
              ("native result " ^ name);
            observed.(pass) <- name :: observed.(pass)
        | _ -> failwith "malformed native observation");
  Array.iter
    (fun names ->
      require
        (List.sort String.compare names
        = List.sort String.compare (List.map fst expected))
        "every native observation occurs once")
    observed;
  require
    (String.ends_with ~suffix:"BINDONE\n" capture)
    "complete original-loader run";
  let sources = metadata |> member "source_checks" |> to_list in
  List.iter
    (fun source ->
      let path = source |> member "path" |> to_string in
      let prefix = "SRC|T:/" ^ path ^ ".Z|" in
      let line =
        String.split_on_char '\n' capture
        |> List.find (String.starts_with ~prefix)
      in
      let expanded =
        bytes
          (String.sub line (String.length prefix)
             (String.length line - String.length prefix))
      in
      require
        (sha expanded = (source |> member "expanded_sha256" |> to_string))
        "expanded ISO source digest";
      let normalized = normalize expanded in
      require
        (sha normalized = (source |> member "normalized_sha256" |> to_string))
        "normalized ISO source digest";
      require
        (normalized = normalize (read ("../third_party/TempleOS/" ^ path)))
        "original loader/producer source matches pinned Git source")
    sources;
  let dir = Filename.temp_file "holyc-bin-cli-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () ->
      let output = Filename.concat dir "module.BIN"
      and map = Filename.concat dir "module.MAP" in
      let modules = metadata |> member "modules" |> to_list in
      List.iter
        (fun module_ ->
          let source = module_ |> member "source" |> to_string in
          let source =
            if String.starts_with ~prefix:"examples/" source then Sys.argv.(2)
            else String.sub source 5 (String.length source - 5)
          in
          let records =
            match module_ |> member "records" with
            | `Null -> []
            | `String path ->
                [ "--records=" ^ String.sub path 5 (String.length path - 5) ]
            | _ -> failwith "recipe"
          in
          let options =
            module_ |> member "options" |> to_list |> List.map to_string
          in
          let args =
            [
              "bin";
              source;
              "--format=json";
              "--output=" ^ output;
              "--map=" ^ map;
            ]
            @ records @ options
          in
          let report, err = invoke dir 0 args in
          require (err = "") "clean native fixture compilation";
          require
            (read output = bytes (module_ |> member "hex" |> to_string))
            "literal module passed by pinned loader";
          require
            (sha (read output) = (module_ |> member "sha256" |> to_string))
            "independent artifact checksum";
          require
            (Yojson.Safe.from_string report = (module_ |> member "report"))
            "exact original record report";
          let first_map = read map in
          ignore (invoke dir 0 args);
          require (read map = first_map) "deterministic module map";
          let inspected, err =
            invoke dir 0 [ "bin"; "--inspect"; "--format=json"; output ]
          in
          require
            (err = ""
            && Yojson.Safe.from_string inspected
               = Yojson.Safe.from_string report)
            "public complete artifact inspection")
        modules;
      let source = Filename.concat dir "source.asm" in
      write source "entry:: RET;";
      let report, _ =
        invoke dir 0
          [
            "bin";
            source;
            "--format=json";
            "--output=" ^ output;
            "--main=entry";
            "--byte-limit=64";
          ]
      in
      require
        (Yojson.Safe.from_string report |> member "file_size" |> to_int = 64)
        "exact CLI byte limit";
      write output "preserved-bin";
      write map "preserved-map";
      let failures =
        [
          [ "--byte-limit=63" ];
          [ "--byte-limit=0" ];
          [ "--byte-limit=16777217" ];
          [ "--work-limit=0" ];
          [ "--work-limit=1" ];
          [ "--main=missing" ];
          [ "--records=" ^ source ];
          [ "--output=" ^ source ];
          [ "--output=" ^ map; "--map=" ^ map ];
        ]
      in
      List.iter
        (fun options ->
          let partial, _ =
            invoke dir 1
              ([
                 "bin";
                 source;
                 "--format=json";
                 "--main=entry";
                 "--output=" ^ output;
                 "--map=" ^ map;
               ]
              @ options)
          in
          require (partial = "") "failure has no partial module report";
          require
            (read output = "preserved-bin" && read map = "preserved-map")
            "failure preserves artifact files")
        (List.filteri (fun i _ -> i <> 7 && i <> 8) failures);
      ignore (invoke dir 1 [ "bin"; source; "--output=" ^ source ]);
      require (read source = "entry:: RET;") "root source is preserved";
      ignore (invoke dir 1 [ "bin"; source; "--output=" ^ map; "--map=" ^ map ]);
      let included = Filename.concat dir "empty.hc" in
      write included "";
      write source
        (Printf.sprintf "#include \"%s\"\nRET;"
           (String.map (fun c -> if c = '\\' then '/' else c) included));
      ignore
        (invoke dir 1
           [ "bin"; source; "--include-root=" ^ dir; "--output=" ^ included ]);
      require (read included = "") "empty included source is preserved";
      let input = Filename.concat dir "data.BIN" in
      write input "\195";
      write source "BINFILE \"data.BIN\";";
      ignore
        (invoke dir 1
           [ "bin"; source; "--include-root=" ^ dir; "--output=" ^ input ]);
      require (read input = "\195") "binary input is preserved";
      let record_input = Filename.concat dir "records.json" in
      write record_input "[]";
      write source "RET;";
      ignore
        (invoke dir 1
           [
             "bin";
             source;
             "--records=" ^ record_input;
             "--output=" ^ record_input;
           ]);
      require (read record_input = "[]") "record input is preserved";
      if not Sys.win32 then (
        let alias = Filename.concat dir "alias.asm" in
        Unix.symlink source alias;
        ignore (invoke dir 1 [ "bin"; alias; "--output=" ^ source ]);
        ignore (invoke dir 1 [ "bin"; source; "--output=" ^ alias ]);
        require (read alias = "RET;") "source alias and target are preserved");
      write source "IMPORT X; DU64 X;";
      ignore (invoke dir 0 [ "bin"; source; "--output=" ^ output ]);
      write source "RET; DU8 entry; entry: RET;";
      ignore (invoke dir 1 [ "bin"; source; "--output=" ^ output ]);
      write output (String.make 32 '\000');
      ignore (invoke dir 1 [ "bin"; "--inspect"; output; "--map=" ^ map ]);
      require (read map = "preserved-map") "invalid artifact preserves map";
      ignore (invoke dir 1 [ "bin"; "--inspect"; "--main=entry"; output ]);
      Printf.printf "checked %d BIN CLI reports and %d native observations\n"
        !count
        (2 * List.length expected))
