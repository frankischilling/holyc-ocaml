open Yojson.Safe.Util
module V = Holyc_lib.Bin_validator

let require b message = if not b then failwith message

let read path =
  let c = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr c)
    (fun () -> really_input_string c (in_channel_length c))

let write path text =
  let c = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr c)
    (fun () -> output_string c text)

let bytes hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

let sha text = Digestif.SHA256.(to_hex (digest_string text))

let get = function
  | Ok x -> x
  | Error (e : V.error) -> failwith (e.artifact ^ ": " ^ e.message)

let count = ref 0

let invoke dir status args =
  let out = Filename.concat dir "stdout"
  and err = Filename.concat dir "stderr" in
  let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
  let ofd = Unix.openfile out flags 0o600
  and efd = Unix.openfile err flags 0o600 in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close ofd;
        Unix.close efd)
      (fun () ->
        Unix.create_process Sys.argv.(1)
          (Array.of_list (Sys.argv.(1) :: args))
          Unix.stdin ofd efd)
  in
  let _, code = Unix.waitpid [] pid in
  incr count;
  require
    (code = Unix.WEXITED status)
    (String.concat " " args ^ "\n" ^ read out ^ read err);
  (read out, read err)

let field body at width =
  let x = ref 0L in
  for i = 0 to width - 1 do
    x :=
      Int64.logor !x
        (Int64.shift_left (Int64.of_int (Char.code body.[at + i])) (8 * i))
  done;
  !x

let check_field body name expected offset width =
  require
    (field body offset width = expected)
    ("native normalized field: " ^ name)

let image w id = List.find (fun (i : V.image) -> i.id = id) (V.images w)
let int64 text = Int64.of_string text

let () =
  let temp = Filename.temp_file "holyc-validator-" "" in
  Sys.remove temp;
  Unix.mkdir temp 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat temp name))
        (Sys.readdir temp);
      Unix.rmdir temp)
    (fun () ->
      (* Capture bytes and original compiler source inputs have their own digests. *)
      let metadata = Yojson.Safe.from_file "oracle/bin-validator/native.json" in
      require
        (sha (read "oracle/bin-validator/native.hc")
        = (metadata |> member "payload_sha256" |> to_string))
        "native producer payload digest";
      let capture = read "oracle/bin-validator/native.capture" in
      require
        (sha capture = (metadata |> member "capture_sha256" |> to_string))
        "native producer capture digest";
      require
        (String.ends_with ~suffix:"NATIVEDONE\n" capture)
        "completed original compiler capture";
      let native = metadata |> member "artifacts" |> to_list in
      List.iter
        (fun data ->
          let name = data |> member "name" |> to_string in
          let path = "oracle/bin-validator/" ^ name ^ ".BIN" in
          require
            (sha (read path) = (data |> member "sha256" |> to_string))
            "original BIN digest";
          require
            (sha (read ("oracle/bin-validator/" ^ name ^ ".HC"))
            = (data |> member "source_sha256" |> to_string))
            "original source digest";
          require (String.contains capture '|') "native capture envelopes";
          let literal = "NATIVE|" ^ name ^ "|" in
          let line =
            String.split_on_char '\n' capture
            |> List.find (String.starts_with ~prefix:literal)
          in
          require
            (bytes
               (String.sub line (String.length literal)
                  (String.length line - String.length literal))
            = read path)
            "capture contains the complete original artifact";
          let output, _ =
            invoke temp 0 [ "verify-bin"; path; "--format=json" ]
          in
          let report =
            Yojson.Safe.from_string output
            |> member "artifacts" |> to_list |> List.hd
          in
          require
            (report |> member "sha256" |> to_string = sha (read path))
            "public independent verifier digest";
          List.iter
            (fun base ->
              let output, _ =
                invoke temp 0
                  [
                    "dump-relocs";
                    path;
                    "--base=" ^ base;
                    "--bind=NVImport=0x55667788";
                    "--format=json";
                  ]
              in
              let report = Yojson.Safe.from_string output in
              require
                (report |> member "unresolved" |> to_list = [])
                "native-produced imports resolved";
              let raw = V.read ~name:path (read path) |> get in
              let w =
                V.bind (V.empty ()) ~name:"NVImport" ~value:0x55667788L |> get
              in
              let w = V.load ~base:(int64 base) w raw |> get in
              require
                (V.report w = report)
                "CLI uses the public independent model";
              if name = "ASM" then (
                let body = (image w 0).body in
                check_field body "original IMM32" 0x55667788L 1 4;
                check_field body "original ABS"
                  (Int64.logand (int64 base) 0xffffffffL)
                  6 4;
                require
                  (List.assoc "NVEntry" (V.symbols w) = int64 base)
                  "original REL export";
                require
                  (List.assoc "NVDatum" (V.symbols w)
                  = Int64.add (int64 base) 6L)
                  "original data export";
                require
                  (List.map (fun (r : V.record) -> r.wire_type) (V.records raw)
                  = [ 20; 16; 16; 9 ])
                  "literal original writer record order")
              else (
                require
                  (List.assoc "NVGlobal" (V.symbols w) = int64 base)
                  "original HolyC global export";
                require
                  (List.assoc "NVFunction" (V.symbols w)
                  = Int64.add (int64 base) 8L)
                  "original HolyC function export";
                require
                  (List.map (fun (e : V.entry) -> e.address) (V.entries w)
                  = [ Int64.add (int64 base) 32L ])
                  "original HolyC MAIN entry";
                require
                  ((image w 0).body = V.body raw)
                  "original RIP-relative body retains its bytes";
                require
                  (List.map (fun (r : V.record) -> r.wire_type) (V.records raw)
                  = [ 16; 16; 25 ])
                  "original HolyC writer record order"))
            [ "0x10000000"; "0x100000000" ])
        native;
      let recipes =
        Yojson.Safe.from_file "oracle/bin-modules/native.json"
        |> member "modules" |> to_list
      in
      let modules =
        List.map
          (fun item ->
            let name = item |> member "name" |> to_string in
            let path = Filename.concat temp (name ^ ".BIN") in
            let data = bytes (item |> member "hex" |> to_string) in
            write path data;
            let emitted = Filename.concat temp (name ^ "-emitted.BIN") in
            let source = "../" ^ (item |> member "source" |> to_string) in
            let opts =
              item |> member "options" |> to_list |> List.map to_string
            in
            let args =
              [ "bin"; source; "--output=" ^ emitted ]
              @ opts
              @
              match item |> member "records" with
              | `Null -> []
              | x -> [ "--records=../" ^ to_string x ]
            in
            ignore (invoke temp 0 args);
            require
              (read emitted = data)
              "public writer matches original-loader-tested artifact";
            ignore (invoke temp 0 [ "verify-bin"; emitted; "--format=json" ]);
            (name, path, V.read ~name:path data |> get))
          recipes
      in
      let artifact name =
        let _, _, a = List.find (fun (n, _, _) -> n = name) modules in
        a
      in
      List.iter
        (fun base ->
          let w =
            V.bind (V.empty ()) ~name:"BMODCounter" ~value:0x40000000L |> get
          in
          let w = V.load ~base w (artifact "ALL") |> get in
          let b = (image w 0).body in
          List.iter
            (fun (name, expected, at, width) ->
              check_field b name expected at width)
            [
              ("rel8", 95L, 0, 1);
              ("imm8", 0x78L, 1, 1);
              ("rel16", 92L, 2, 2);
              ("imm16", 0x5678L, 4, 2);
              ("rel32", 86L, 6, 4);
              ("imm32", 0x12345678L, 10, 4);
              ("rel64", 82L, 14, 8);
              ("imm64", 0x89abcdefL, 22, 8);
            ];
          let mod32 x = Int64.logand x 0xffffffffL in
          require
            (field b 30 4 = mod32 (Int64.mul base 2L))
            "native duplicate ABS";
          require (field b 34 4 = mod32 base) "native single ABS";
          require
            (field b 38 4 = 0x40000000L && field b 42 4 = 0x20000000L)
            "native duplicate/single code heap";
          require
            (field b 46 8 = 0x60000000L && field b 54 8 = 0x30000000L)
            "native duplicate/single data heap";
          require
            (field b 62 4 = 0x20000010L && field b 66 8 = 0x30000010L)
            "native zeroed heap patches";
          require
            (List.map (fun (e : V.entry) -> e.address) (V.entries w)
            = [ Int64.add base 127L; Int64.add base 127L ])
            "native repeated MAIN address order";
          let wait_base = Int64.add base 0x1000L in
          let w = V.load ~base:wait_base w (artifact "WAIT") |> get in
          let before = w in
          require (List.length (V.unresolved w) = 2) "native unresolved groups";
          let w =
            V.load ~base:(Int64.add base 0x2000L) w (artifact "PROV") |> get
          in
          check_field (image w 1).body "native deferred U8" 0x44L 0 1;
          check_field (image w 1).body "native deferred U32" 0x11223344L 1 4;
          require
            (List.length (V.unresolved w) = 1)
            "native heap publication leaves pending import";
          check_field (image w 1).body "native heap remains unpatched"
            (-3689348814741910324L) 8 8;
          let w =
            V.load ~base:(Int64.add base 0x3000L) w (artifact "RES") |> get
          in
          check_field (image w 1).body
            "native ordinary export resolves heap import" 85L 8 8;
          require (V.unresolved w = []) "native final resolution";
          check_field (image before 1).body "retained prior snapshot" 0xccL 0 1;
          let flags : V.flags =
            { no_abss = true; just_load = true; silent = true }
          in
          let flagged =
            V.load ~flags ~base (V.empty ()) (artifact "ALL") |> get
          in
          check_field (image flagged 0).body "native NO_ABSS" 0L 30 8;
          require (V.entries flagged = []) "native JUST_LOAD";
          let paths =
            List.map
              (fun n ->
                let _, p, _ =
                  List.find (fun (name, _, _) -> name = n) modules
                in
                p)
              [ "ALL"; "WAIT"; "PROV"; "RES" ]
          in
          let bases =
            List.init 4 (fun i ->
                "--base="
                ^ Int64.to_string (Int64.add base (Int64.of_int (i * 4096))))
          in
          let batch_map = Filename.concat temp "batch.MAP" in
          let out, _ =
            invoke temp 0
              ([
                 "dump-relocs";
                 "--format=json";
                 "--bind=BMODCounter=0x40000000";
                 "--require-resolved";
                 "--map=" ^ batch_map;
               ]
              @ bases @ paths)
          in
          require
            (Yojson.Safe.from_string out = V.report w)
            "public batch model and API agree at both bases";
          require
            (read batch_map = V.map w)
            "public map preserves API order and offsets";
          let expected =
            Printf.sprintf
              "PATCH 0 RECORD 160 FIELD 62 WIDTH 4 absolute \"\" \
               0x0000000000000000 -> 0x%016Lx"
              (mod32 base)
          in
          require
            (List.mem expected (String.split_on_char '\n' (read batch_map)))
            "literal map relocation at each base")
        [ 0x10000000L; 0x100000000L ];
      let _, path, _ = List.hd modules in
      let protected = read path in
      ignore (invoke temp 1 [ "verify-bin"; path; "--output=" ^ path ]);
      require (read path = protected) "input protection";
      let output = Filename.concat temp "report.json"
      and map = Filename.concat temp "map.txt" in
      ignore
        (invoke temp 0
           [
             "dump-relocs";
             path;
             "--format=json";
             "--output=" ^ output;
             "--map=" ^ map;
           ]);
      require (String.length (read map) > 0) "public map";
      require
        (List.mem "UNRESOLVED 0 \"BMODCounter\" RECORD 117"
           (String.split_on_char '\n' (read map)))
        "literal unresolved map location";
      write output "preserved";
      ignore
        (invoke temp 1
           [
             "dump-relocs";
             path;
             "--format=json";
             "--require-resolved";
             "--output=" ^ output;
           ]);
      require
        (read output = "preserved")
        "failed unresolved check does not replace output";
      let bad = Filename.concat temp "malformed.BIN" in
      write bad "TOSB";
      let out, err =
        invoke temp 1
          [ "verify-bin"; bad; "--format=json"; "--output=" ^ output ]
      in
      require
        (out = "" && String.contains err ':')
        "precise error and no partial report";
      require (read output = "preserved") "failed reader preserves output";
      List.iter
        (fun args -> ignore (invoke temp 1 args))
        [
          [ "verify-bin"; path; "--base=1" ];
          [ "dump-relocs"; path; "--base=-1" ];
          [ "dump-relocs"; path; "--base=1"; "--base=2" ];
          [ "dump-relocs"; path; "--bind=bad" ];
          [ "dump-relocs"; path; "--work-limit=1" ];
          [ "dump-relocs"; path; "--image-limit=1" ];
          [ "verify-bin"; path; "--output=" ^ output; "--map=" ^ output ];
        ];
      if not Sys.win32 then (
        let alias = Filename.concat temp "input-alias" in
        Unix.symlink path alias;
        ignore (invoke temp 1 [ "verify-bin"; path; "--output=" ^ alias ]);
        require (read path = protected) "input symlink alias protection");
      Printf.printf
        "BIN validator: %d public command reports; original compiler artifacts \
         and normalized loader fields passed\n"
        !count)
