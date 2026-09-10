let capture executable arguments =
  let stdout_path = Filename.temp_file "holyc-jit-stdout" ".txt" in
  let stderr_path = Filename.temp_file "holyc-jit-stderr" ".txt" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove stdout_path;
      Sys.remove stderr_path)
    (fun () ->
      let stdout =
        Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
      in
      let stderr =
        Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
      in
      let pid =
        Fun.protect
          ~finally:(fun () ->
            Unix.close stdout;
            Unix.close stderr)
          (fun () ->
            Unix.create_process executable
              (Array.of_list (executable :: arguments))
              Unix.stdin stdout stderr)
      in
      let _, status = Unix.waitpid [] pid in
      let read path =
        let channel = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in channel)
          (fun () -> really_input_string channel (in_channel_length channel))
      in
      (status, read stdout_path, read stderr_path))

let with_source contents run =
  let path = Filename.temp_file "holyc-jit-source" ".hc" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      run path)

let require condition message = if not condition then failwith message

let () =
  let executable = Sys.argv.(1) in
  List.iter
    (fun (mode, text) ->
      with_source text (fun path ->
          let status, output, errors =
            capture executable
              [ "run"; "--format=json"; "--mode=" ^ mode; path ]
          in
          require
            (status = Unix.WEXITED 0 && errors = "")
            ("stateful run failed: " ^ output ^ errors);
          let open Yojson.Basic.Util in
          let report = Yojson.Basic.from_string output in
          require
            (report |> member "final_value" |> member "value" |> to_string
           = "42")
            "stateful CLI result";
          require
            (report |> member "diagnostics" |> to_list = [])
            "stateful CLI diagnostics";
          require
            (report |> member "output_hex" |> to_string = "")
            "stream output leaked into ordinary output"))
    [
      ("jit", {|#exe {StreamPrint("42;");}|});
      ("aot", {|#exe {StreamPrint("42;");}|});
      ("jit", {|I64 N=40;#exe {StreamPrint("%d;",N+2);}|});
      ("jit", {|I64 F(I64 n=42){return n;};F();|});
      ("aot", {|I64 F(I64 n=42){return n;};F();|});
      ( "aot",
        {|extern I64 Unused(I64 n=20+22);I64 Saved(U8 n=sizeof U8+276){return n;};I64 Twice(){return Saved()+Saved();};Twice();|}
      );
      ( "jit",
        {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;Saved()+Saved();|}
      );
      ( "jit",
        {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;#exe {StreamPrint("%d;",Saved()+Saved());}|}
      );
    ];
  List.iter
    (fun source ->
      with_source source (fun path ->
          let status, output, errors =
            capture executable [ "dump-ir"; "--program"; "--mode=jit"; path ]
          in
          require
            (status = Unix.WEXITED 0 && errors = "")
            ("stateful IR inspection failed: " ^ output ^ errors);
          let units =
            String.split_on_char '\n' output
            |> List.filter (String.starts_with ~prefix:"task unit ")
          in
          require
            (List.length units >= 3)
            "stateful IR must identify separate task units"))
    [
      {|#exe {I64 N=40;StreamPrint("%d;",N+2);}|};
      {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;Saved()+Saved();|};
    ];
  print_endline
    "Stateful JIT/AOT CLI execution and separate JIT IR units passed."
