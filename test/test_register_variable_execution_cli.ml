open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix f =
  let path = Filename.temp_file "holyc register execution " suffix in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let compiler = Sys.argv.(1)
let native = Array.length Sys.argv = 3 && Sys.argv.(2) = "--native"

let run source expected target mode =
  with_file ".hc" (fun path ->
      let channel = open_out_bin path in
      output_string channel source;
      close_out channel;
      with_file ".json" (fun output ->
          with_file ".err" (fun errors ->
              let out_fd =
                Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile errors [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let args =
                [|
                  compiler;
                  "run";
                  "--format=json";
                  "--target=" ^ target;
                  "--mode=" ^ mode;
                  path;
                |]
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler args Unix.stdin out_fd err_fd)
              in
              let _, status = Unix.waitpid [] pid in
              require (read errors = "") "unexpected stderr";
              let result = Yojson.Safe.from_string (read output) in
              require
                (status = Unix.WEXITED 0
                && member "outcome" result = `String "success")
                (Yojson.Safe.to_string result);
              require
                (result |> member "final_value" |> member "value"
               = `String expected)
                (target ^ " " ^ mode ^ ": " ^ Yojson.Safe.to_string result))))

let () =
  let targets = if native then [ "host-jit"; "host-jit-task" ] else [ "ir" ] in
  let cases =
    [
      ("I64 F(I8 x,I8 c){x<<=c;return x;}F(127,1);", "254");
      ("I64 F(I8 noreg x,I8 c){x<<=c;return x;}F(127,1);", "-2");
      ("I64 F(I8 x,I8 c){I8 *p=&x;*p<<=c;return x;}F(127,1);", "-2");
      ("I64 F(U8 x,I8 c){x<<=c;return x;}F(255,1);", "510");
      ("I64 F(U8 noreg x,I8 c){x<<=c;return x;}F(255,1);", "254");
      ("I64 F(I8 x,I8 c){x<<=c;return x;}F(255,0);", "-1");
      ("I64 F(U8 x,I8 c){x<<=c;return x;}F(256,0);", "0");
      ( "I64 F(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e,I64 f,I64 g,I64 h,I64 \
         i){x<<=c;return \
         x+a+a+a+b+b+b+d+d+d+e+e+e+f+f+f+g+g+g+h+h+h+i+i+i;}F(127,1,0,0,0,0,0,0,0,0);",
        "-2" );
    ]
  in
  let local_cases =
    [
      ("I8", "255", "255", "-1");
      ("U8", "256", "256", "0");
      ("I16", "65535", "65535", "-1");
      ("U16", "65536", "65536", "0");
      ("I32", "4294967295", "4294967295", "-1");
      ("U32", "4294967296", "4294967296", "0");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(){%s %s x=%s;x<<=0;return x;}F();" ty
              qualifier initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        local_cases
    @ [
        ("I64 F(){I8 x=127;x<<=1;return x;}F();", "254");
        ("I64 F(){I8 noreg x=127;x<<=1;return x;}F();", "-2");
        ("I64 F(){I8 x=255;x>>=1;return x;}F();", "127");
        ("I64 F(){U8 x=256;x>>=1;return x;}F();", "128");
        ("I64 F(){I8 noreg x=255;x>>=1;return x;}F();", "-1");
        ("I64 F(){U8 noreg x=256;x>>=1;return x;}F();", "0");
        ("I64 F(){I16 x=65535;x>>=1;return x;}F();", "32767");
        ("I64 F(){U16 x=65536;x>>=1;return x;}F();", "32768");
        ("I64 F(){I32 x=4294967295;x>>=1;return x;}F();", "2147483647");
        ("I64 F(){U32 x=4294967296;x>>=1;return x;}F();", "2147483648");
        ("I64 F(){I16 noreg x=65535;x>>=1;return x;}F();", "-1");
        ("I64 F(){U16 noreg x=65536;x>>=1;return x;}F();", "0");
        ("I64 F(){I32 noreg x=4294967295;x>>=1;return x;}F();", "-1");
        ("I64 F(){U32 noreg x=4294967296;x>>=1;return x;}F();", "0");
      ]
  in
  let returns =
    [
      ("I8", "255", "255", "-1");
      ("U8", "256", "256", "0");
      ("I16", "65535", "65535", "-1");
      ("U16", "65536", "65536", "0");
      ("I32", "4294967295", "4294967295", "-1");
      ("U32", "4294967296", "4294967296", "0");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(){%s %s x=%s;return x;}F();" ty qualifier
              initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        returns
  in
  let cases =
    cases
    @ [
        ("I8 F(){I8 x=255;return x;}F();", "255");
        ("U8 F(){U8 x=256;return x;}F();", "256");
        ("I16 F(){I16 x=65535;return x;}F();", "65535");
        ("U16 F(){U16 x=65536;return x;}F();", "65536");
      ]
  in
  let cases =
    cases
    @ List.map
        (fun ty ->
          ( Printf.sprintf "I64 F(){%s x=0x800000000000002A;return x;}F();" ty,
            "-9223372036854775766" ))
        [ "I8"; "I16"; "U16"; "I32"; "U32" ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(%s %s x,I8 c){x+=c;return x;}F(%s,1);" ty
              qualifier initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        [
          ("I8", "127", "128", "-128");
          ("U8", "255", "256", "0");
          ("I16", "32767", "32768", "-32768");
          ("U16", "65535", "65536", "0");
          ("I32", "2147483647", "2147483648", "-2147483648");
          ("U32", "4294967295", "4294967296", "0");
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(){%s %s x=%s;x+=1;return x;}F();" ty qualifier
              initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        [
          ("I8", "255", "256", "0");
          ("U8", "256", "257", "1");
          ("I16", "65535", "65536", "0");
          ("U16", "65536", "65537", "1");
          ("I32", "4294967295", "4294967296", "0");
          ("U32", "4294967296", "4294967297", "1");
        ]
  in
  let cases =
    cases
    @ [
        ("I64 F(){I8 x=255;x+=256;return x;}F();", "511");
        ("I64 F(){U8 x=256;x+=256;return x;}F();", "512");
        ("I64 F(){I16 x=65535;x+=256;return x;}F();", "65791");
        ("I64 F(){U16 x=65536;x+=256;return x;}F();", "65792");
      ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I8 x,I64 c){x+=c;return x;}F(127,0x8000000000000000);",
          "-9223372036854775681" );
        ( "I64 F(U8 x,I64 c){x+=c;return x;}F(255,0x8000000000000000);",
          "-9223372036854775553" );
        ( "I64 F(I8 noreg x,I64 c){x+=c;return x;}F(127,0x8000000000000000);",
          "127" );
        ( "I64 F(U8 noreg x,I64 c){x+=c;return x;}F(255,0x8000000000000000);",
          "255" );
      ]
  in
  let cases =
    cases
    @ [
        ("I64 F(){I8 x=255;x+=2147483647;return x;}F();", "2147483902");
        ("I64 F(){U8 x=256;x+=2147483647;return x;}F();", "2147483903");
        ("I64 F(){I16 x=65535;x+=2147483647;return x;}F();", "2147549182");
        ("I64 F(){U16 x=65536;x+=2147483647;return x;}F();", "2147549183");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(%s %s x,I8 c){x-=c;return x;}F(%s,1);" ty
              qualifier initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        [
          ("I8", "-128", "-129", "127");
          ("U8", "0", "-1", "255");
          ("I16", "-32768", "-32769", "32767");
          ("U16", "0", "-1", "65535");
          ("I32", "-2147483648", "-2147483649", "2147483647");
          ("U32", "0", "-1", "4294967295");
        ]
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(){%s %s x=%s;x-=1;return x;}F();" ty qualifier
              initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        [
          ("I8", "255", "254", "-2");
          ("U8", "256", "255", "255");
          ("I16", "65535", "65534", "-2");
          ("U16", "65536", "65535", "65535");
          ("I32", "4294967295", "4294967294", "-2");
          ("U32", "4294967296", "4294967295", "4294967295");
        ]
    @ [
        ("I64 F(){I8 x=255;x-=2147483647;return x;}F();", "-2147483392");
        ("I64 F(){U8 x=256;x-=2147483647;return x;}F();", "-2147483391");
        ("I64 F(){I16 x=65535;x-=2147483647;return x;}F();", "-2147418112");
        ("I64 F(){U16 x=65536;x-=2147483647;return x;}F();", "-2147418111");
        ( "I64 F(I8 x,I64 c){x-=c;return x;}F(127,0x8000000000000000);",
          "-9223372036854775681" );
        ( "I64 F(U8 x,I64 c){x-=c;return x;}F(255,0x8000000000000000);",
          "-9223372036854775553" );
        ( "I64 F(I8 noreg x,I64 c){x-=c;return x;}F(127,0x8000000000000000);",
          "127" );
        ( "I64 F(U8 noreg x,I64 c){x-=c;return x;}F(255,0x8000000000000000);",
          "255" );
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(%s %s x,I8 c){x*=c;return x;}F(%s,2);" ty
              qualifier initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        [
          ("I8", "127", "254", "-2");
          ("U8", "255", "510", "254");
          ("I16", "32767", "65534", "-2");
          ("U16", "65535", "131070", "65534");
          ("I32", "2147483647", "4294967294", "-2");
          ("U32", "4294967295", "8589934590", "4294967294");
        ]
    @ List.concat_map
        (fun (ty, initial, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(){%s %s x=%s;x*=2;return x;}F();" ty qualifier
              initial
          in
          [ (source "", wide); (source "noreg", narrow) ])
        [
          ("I8", "255", "510", "-2");
          ("U8", "256", "512", "0");
          ("I16", "65535", "131070", "-2");
          ("U16", "65536", "131072", "0");
          ("I32", "4294967295", "8589934590", "-2");
          ("U32", "4294967296", "8589934592", "0");
        ]
    @ [
        ( "I64 F(){I8 x=255;x*=0x8000000000000000;return x;}F();",
          "-9223372036854775808" );
        ("I64 F(){U8 x=256;x*=0x8000000000000000;return x;}F();", "0");
        ("I64 F(){I8 noreg x=255;x*=0x8000000000000000;return x;}F();", "0");
        ("I64 F(){I8 x=255;x*=2147483647;return x;}F();", "547608329985");
        ("I64 F(){U8 x=256;x*=2147483647;return x;}F();", "549755813632");
        ("I64 F(){I16 x=65535;x*=2147483647;return x;}F();", "140735340806145");
        ("I64 F(){U16 x=65536;x*=2147483647;return x;}F();", "140737488289792");
        ( "I64 F(I8 x,I64 c){x*=c;return x;}F(127,0x8000000000000000);",
          "-9223372036854775808" );
        ( "I64 F(I8 noreg x,I64 c){x*=c;return x;}F(127,0x8000000000000000);",
          "0" );
      ]
  in
  let widths =
    [
      ("I16", "32767", "65534", "-2", "65535", "-1");
      ("U16", "65535", "131070", "65534", "65536", "0");
      ("I32", "2147483647", "4294967294", "-2", "4294967295", "-1");
      ("U32", "4294967295", "8589934590", "4294967294", "4294967296", "0");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, wide, narrow, overflow, converted) ->
          let source qualifier value count =
            Printf.sprintf "I64 F(%s %s x,I8 c){x<<=c;return x;}F(%s,%d);" ty
              qualifier value count
          in
          [
            (source "" initial 1, wide);
            (source "noreg" initial 1, narrow);
            (source "" overflow 0, converted);
          ])
        widths
  in
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          List.iter
            (fun (source, expected) -> run source expected target mode)
            cases)
        [ "jit"; "aot" ])
    targets;
  if native then
    List.iter
      (fun mode ->
        List.iter
          (fun (source, expected) -> run source expected "host-jit-task" mode)
          [
            ( "#exe {I64 F(I8 x,I8 c){x<<=c;return x;}I64 I(){return \
               F(127,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
              "254" );
            ( "#exe {I64 F(U8 x,I8 c){x<<=c;return x;}I64 I(){return \
               F(255,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
              "510" );
          ])
      [ "jit"; "aot" ];
  Printf.printf "Register execution checks passed (%d reports).\n"
    ((List.length targets * 2 * List.length cases) + if native then 4 else 0)
