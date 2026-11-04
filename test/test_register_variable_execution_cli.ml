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

let run ?(fault = false) source expected target mode =
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
                (status = Unix.WEXITED (if fault then 1 else 0)
                && member "outcome" result
                   = `String (if fault then "error" else "success"))
                (Yojson.Safe.to_string result);
              if fault then
                require
                  (result |> member "diagnostics" |> to_list
                  |> List.exists (fun diagnostic ->
                      member "code" diagnostic = `String expected))
                  (target ^ " " ^ mode ^ ": " ^ Yojson.Safe.to_string result)
              else
                require
                  (result |> member "final_value" |> member "value"
                 = `String expected)
                  (target ^ " " ^ mode ^ ": " ^ Yojson.Safe.to_string result))))

let () =
  let targets = if native then [ "host-jit"; "host-jit-task" ] else [ "ir" ] in
  let cases =
    [
      ("I64 F(I8 x,I64 y,I64 c){x+=c;y+=c;return x+y;}F(127,42,1);", "171");
      ( "I64 F(U8 reg R12 x,U8 reg R13 y,U8 reg R14 z,I64 \
         c){x+=c;y-=c;z*=c;return x+y+z;}F(255,255,255,2);",
        "1020" );
      ( "I64 F(U8 reg R12 x,U8 reg R13 y,U8 noreg z,I64 c,I64 d,I64 \
         e){x+=c;y*=d;z<<=e;return x+y+z;}F(255,255,255,2,2,1);",
        "1021" );
      ( "I64 F(I8 reg R12 x,U16 reg R13 y,I32 noreg z,I64 c,I64 d,I64 \
         e){x+=c;y*=d;z<<=e;return x+y+z;}F(127,65535,2147483647,2,2,1);",
        "131197" );
      ( "I64 F(I64 reg R12 x,I8 reg R13 y,U16 noreg z,I64 c,I64 d,I64 \
         e){x+=c;y*=d;z<<=e;return x+y+z;}F(42,127,65535,2,2,1);",
        "65832" );
      ( "I64 F(I8 reg R12 x,U16 reg R13 y,I32 reg R14 z,U32 reg R15 w,I64 \
         noreg v,I64 c){x+=c;y-=c;z*=c;w<<=c;v*=c;return \
         x+y+z+w+v;}F(127,65535,2147483647,4294967295,42,2);",
        "21474902220" );
      ( "I64 F(I8 reg R12 x,U16 reg R13 y,I32 reg R14 z,U32 reg R15 w,U8 noreg \
         v,I64 c){x+=c;y-=c;z*=c;w<<=c;v*=c;return \
         x+y+z+w+v;}F(127,65535,2147483647,4294967295,255,2);",
        "21474902390" );
      ( "I64 F(U8 reg R12 x,U8 reg R13 y,U8 reg R14 z,U8 reg R15 w,U8 noreg \
         v,I64 c){x+=c;y-=c;z*=c;w<<=c;v*=c;return \
         x+y+z+w+v;}F(255,255,255,255,255,2);",
        "2294" );
      ("I64 F(I64 x,I8 y,I64 c){x+=c;y+=c;return x+y;}F(42,127,1);", "171");
      ( "I64 F(I8 noreg x,I64 noreg y,I64 c){x+=c;y+=c;return x+y;}F(127,42,1);",
        "-85" );
      ( "I64 F(I64 noreg x,I8 noreg y,I64 c){x+=c;y+=c;return x+y;}F(42,127,1);",
        "-85" );
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
  let parameter_local_widths =
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
        (fun (ty, input, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(I64 c){%s %s x=c;return x;}F(%s);" ty
              qualifier input
          in
          [ (source "", wide); (source "noreg", narrow) ])
        parameter_local_widths
  in
  let cases =
    cases
    @ [
        ("I64 F(U64 c){I8 x=c;return x;}F(0xFFFFFFFFFFFFFFFF);", "-1");
        ("I64 F(U64 c){U8 x=c;return x;}F(0xFFFFFFFFFFFFFFFF);", "-1");
        ("I64 F(U64 c){U8 noreg x=c;return x;}F(0xFFFFFFFFFFFFFFFF);", "255");
      ]
  in
  let parameter_local_updates =
    [
      ("I8", "255", "256", "0", "510", "-2");
      ("U8", "256", "257", "1", "512", "0");
      ("I16", "65535", "65536", "0", "131070", "-2");
      ("U16", "65536", "65537", "1", "131072", "0");
      ("I32", "4294967295", "4294967296", "0", "8589934590", "-2");
      ("U32", "4294967296", "4294967297", "1", "8589934592", "0");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, add, add_memory, shift, shift_memory) ->
          let source qualifier op =
            Printf.sprintf "I64 F(I64 c){%s %s x=c;x%s=1;return x;}F(%s);" ty
              qualifier op input
          in
          [
            (source "" "+", add);
            (source "noreg" "+", add_memory);
            (source "" "<<", shift);
            (source "noreg" "<<", shift_memory);
          ])
        parameter_local_updates
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c){I8 x=c;x+=2147483647;return x;}F(255);", "2147483902");
        ("I64 F(I64 c){I8 noreg x=c;x+=2147483647;return x;}F(255);", "-2");
        ("I64 F(I64 c){U8 x=c;x+=2147483647;return x;}F(256);", "2147483903");
        ("I64 F(I64 c){U8 noreg x=c;x+=2147483647;return x;}F(256);", "255");
      ]
  in
  let parameter_local_more_updates =
    [
      ("I8", "255", "254", "-2", "127", "-1", "765", "-3");
      ("U8", "256", "255", "255", "128", "0", "768", "0");
      ("I16", "65535", "65534", "-2", "32767", "-1", "196605", "-3");
      ("U16", "65536", "65535", "65535", "32768", "0", "196608", "0");
      ( "I32",
        "4294967295",
        "4294967294",
        "-2",
        "2147483647",
        "-1",
        "12884901885",
        "-3" );
      ( "U32",
        "4294967296",
        "4294967295",
        "4294967295",
        "2147483648",
        "0",
        "12884901888",
        "0" );
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, sub, sub_memory, right, right_memory, mul, mul_memory)
           ->
          let source qualifier operation =
            Printf.sprintf "I64 F(I64 c){%s %s x=c;x%s;return x;}F(%s);" ty
              qualifier operation input
          in
          [
            (source "" "-=1", sub);
            (source "noreg" "-=1", sub_memory);
            (source "" ">>=1", right);
            (source "noreg" ">>=1", right_memory);
            (source "" "*=3", mul);
            (source "noreg" "*=3", mul_memory);
          ])
        parameter_local_more_updates
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c){I8 x=c;x>>=1;return x;}F(-1);", "-1");
        ("I64 F(I64 c){U8 x=c;x>>=1;return x;}F(-1);", "9223372036854775807");
        ("I64 F(I64 c){U8 noreg x=c;x>>=1;return x;}F(-1);", "127");
      ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I64 c){I8 x=c;x*=0x8000000000000000;return x;}F(255);",
          "-9223372036854775808" );
        ( "I64 F(I64 c){I8 noreg x=c;x*=0x8000000000000000;return x;}F(255);",
          "0" );
        ("I64 F(I64 c){U8 x=c;x*=0x8000000000000000;return x;}F(256);", "0");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, add, add_memory, shift, shift_memory) ->
          let source qualifier op =
            Printf.sprintf
              "I64 F(I64 c,I8 n){%s %s x=c;x%s=n;return x;}F(%s,1);" ty
              qualifier op input
          in
          [
            (source "" "+", add);
            (source "noreg" "+", add_memory);
            (source "" "<<", shift);
            (source "noreg" "<<", shift_memory);
          ])
        parameter_local_updates
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c,I8 n){I8 x=c;x+=n;return x;}F(255,255);", "254");
        ("I64 F(I64 c,I8 n){I8 noreg x=c;x+=n;return x;}F(255,255);", "-2");
        ("I64 F(I64 c,I8 n){U8 x=c;x+=n;return x;}F(256,255);", "255");
        ("I64 F(I64 c,I8 n){U8 noreg x=c;x+=n;return x;}F(256,255);", "255");
      ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I64 c,I8 n){I8 x=c;x<<=n;return x;}F(255,255);",
          "-9223372036854775808" );
        ("I64 F(I64 c,I8 n){I8 noreg x=c;x<<=n;return x;}F(255,255);", "0");
        ("I64 F(I64 c,I8 n){U8 x=c;x<<=n;return x;}F(256,255);", "0");
      ]
  in
  let dynamic_count_widths =
    [
      ("I8", "255", "254", "255", "-2", "255");
      ("U8", "255", "510", "511", "-2", "255");
      ("I16", "65535", "254", "255", "-2", "255");
      ("U16", "65535", "65790", "65791", "-2", "255");
      ("I32", "4294967295", "254", "255", "-2", "255");
      ("U32", "4294967295", "4294967550", "4294967551", "-2", "255");
      ("I64", "256", "511", "512", "-1", "0");
      ("U64", "256", "511", "512", "-1", "0");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (count_type, n, signed, unsigned, signed_memory, unsigned_memory)
           ->
          let source ty qualifier initial =
            Printf.sprintf
              "I64 F(I64 c,%s n){%s %s x=c;x+=n;return x;}F(%s,%s);" count_type
              ty qualifier initial n
          in
          [
            (source "I8" "" "255", signed);
            (source "U8" "" "256", unsigned);
            (source "I8" "noreg" "255", signed_memory);
            (source "U8" "noreg" "256", unsigned_memory);
          ])
        dynamic_count_widths
  in
  let cases =
    cases
    @ List.concat_map
        (fun (count_type, n, _, _, _, _) ->
          let wide_count = count_type = "I64" || count_type = "U64" in
          let source ty qualifier initial =
            Printf.sprintf
              "I64 F(I64 c,%s n){%s %s x=c;x<<=n;return x;}F(%s,%s);" count_type
              ty qualifier initial n
          in
          [
            ( source "I8" "" "255",
              if wide_count then "255" else "-9223372036854775808" );
            (source "U8" "" "256", if wide_count then "256" else "0");
            (source "I8" "noreg" "255", if wide_count then "-1" else "0");
            (source "U8" "noreg" "256", "0");
          ])
        dynamic_count_widths
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, sub, sub_memory, right, right_memory, mul, mul_memory)
           ->
          let source qualifier operation n =
            Printf.sprintf
              "I64 F(I64 c,I8 n){%s %s x=c;x%s=n;return x;}F(%s,%s);" ty
              qualifier operation input n
          in
          [
            (source "" "-" "1", sub);
            (source "noreg" "-" "1", sub_memory);
            (source "" ">>" "1", right);
            (source "noreg" ">>" "1", right_memory);
            (source "" "*" "3", mul);
            (source "noreg" "*" "3", mul_memory);
          ])
        parameter_local_more_updates
  in
  let dynamic_bitwise_locals =
    [
      ("I8", "255", "256", "511", "-1", "255", "-1");
      ("U8", "255", "256", "511", "255", "255", "255");
      ("I16", "65535", "65536", "131071", "-1", "65535", "-1");
      ("U16", "65535", "65536", "131071", "65535", "65535", "65535");
      ("I32", "4294967295", "4294967296", "8589934591", "-1", "4294967295", "-1");
      ( "U32",
        "4294967295",
        "4294967296",
        "8589934591",
        "4294967295",
        "4294967295",
        "4294967295" );
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, mask, wide, memory, and_wide, and_memory) ->
          let source qualifier operation initial operand =
            Printf.sprintf
              "I64 F(I64 c,I64 n){%s %s x=c;x%s=n;return x;}F(%s,%s);" ty
              qualifier operation initial operand
          in
          List.concat_map
            (fun op ->
              [
                (source "" op input mask, wide);
                (source "noreg" op input mask, memory);
              ])
            [ "|"; "^" ]
          @ [
              (source "" "&" "-1" input, and_wide);
              (source "noreg" "&" "-1" input, and_memory);
            ])
        dynamic_bitwise_locals
  in
  let dynamic_local_division =
    [
      ("I8", "255", "127", "0", "1", "-1");
      ("U8", "256", "128", "0", "0", "0");
      ("I16", "65535", "32767", "0", "1", "-1");
      ("U16", "65536", "32768", "0", "0", "0");
      ("I32", "4294967295", "2147483647", "0", "1", "-1");
      ("U32", "4294967296", "2147483648", "0", "0", "0");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, div, div_memory, rem, rem_memory) ->
          let source qualifier op =
            Printf.sprintf
              "I64 F(I64 c,I8 n){%s %s x=c;x%s=n;return x;}F(%s,2);" ty
              qualifier op input
          in
          [
            (source "" "/", div);
            (source "noreg" "/", div_memory);
            (source "" "%", rem);
            (source "noreg" "%", rem_memory);
          ])
        dynamic_local_division
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c,I8 n){I8 x=c;x/=n;return x;}F(255,-1);", "-255");
        ("I64 F(I64 c,I8 n){I8 noreg x=c;x/=n;return x;}F(255,-1);", "1");
        ("I64 F(I64 c,I8 n){U8 x=c;x/=n;return x;}F(256,-1);", "0");
      ]
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c,I8 n){I8 x=c;x%=n;return x;}F(255,-1);", "0");
        ("I64 F(I64 c,I8 n){U8 x=c;x%=n;return x;}F(256,-1);", "256");
        ("I64 F(I64 c,I8 n){U8 noreg x=c;x%=n;return x;}F(256,-1);", "0");
      ]
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c,U64 n){I8 x=c;x/=n;return x;}F(255,-1);", "-255");
        ("I64 F(I64 c,U64 n){I8 noreg x=c;x/=n;return x;}F(255,-1);", "1");
        ("I64 F(I64 c,U64 n){U8 x=c;x/=n;return x;}F(256,-1);", "0");
      ]
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c,U64 n){I8 x=c;x%=n;return x;}F(255,-1);", "0");
        ("I64 F(I64 c,U64 n){U8 x=c;x%=n;return x;}F(256,-1);", "256");
        ("I64 F(I64 c,U64 n){U8 noreg x=c;x%=n;return x;}F(256,-1);", "0");
      ]
  in
  let narrow_initializers =
    [
      ("I8", "255", "-1", "0");
      ("U8", "255", "255", "256");
      ("I16", "65535", "-1", "0");
      ("U16", "65535", "65535", "65536");
      ("I32", "4294967295", "-1", "0");
      ("U32", "4294967295", "4294967295", "4294967296");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (argument_type, input, wide, updated) ->
          List.concat_map
            (fun (target, memory) ->
              let source qualifier dynamic =
                if dynamic then
                  Printf.sprintf
                    "I64 F(%s c,I8 n){%s %s x=c;x+=n;return x;}F(%s,1);"
                    argument_type target qualifier input
                else
                  Printf.sprintf "I64 F(%s c){%s %s x=c;return x;}F(%s);"
                    argument_type target qualifier input
              in
              [
                (source "" false, wide);
                (source "noreg" false, memory);
                (source "" true, updated);
                (source "noreg" true, "0");
              ])
            [ ("I8", "-1"); ("U8", "255") ])
        narrow_initializers
  in
  let cases =
    cases
    @ [
        ("I64 F(U32 c){I16 x=c;return x;}F(4294967295);", "4294967295");
        ("I64 F(U32 c){I16 noreg x=c;return x;}F(4294967295);", "-1");
        ("I64 F(I32 c){U16 x=c;return x;}F(4294967295);", "-1");
        ("I64 F(I32 c){U16 noreg x=c;return x;}F(4294967295);", "65535");
        ("I64 F(U32 c){I32 x=c;return x;}F(4294967295);", "4294967295");
        ("I64 F(U32 c){I32 noreg x=c;return x;}F(4294967295);", "-1");
        ("I64 F(I32 c){U32 x=c;return x;}F(4294967295);", "-1");
        ("I64 F(I32 c){U32 noreg x=c;return x;}F(4294967295);", "4294967295");
        ( "I64 F(U32 c,I8 n){I16 x=c;x+=n;return x;}F(4294967295,1);",
          "4294967296" );
        ("I64 F(U32 c,I8 n){I16 noreg x=c;x+=n;return x;}F(4294967295,1);", "0");
        ( "I64 F(U32 c,I8 n){I32 x=c;x+=n;return x;}F(4294967295,1);",
          "4294967296" );
        ("I64 F(U32 c,I8 n){I32 noreg x=c;x+=n;return x;}F(4294967295,1);", "0");
      ]
  in
  let literal_parameter_locals =
    [
      ("I8", "255", "255", "-1", "510", "-2");
      ("U8", "256", "256", "0", "512", "0");
      ("I16", "65535", "65535", "-1", "131070", "-2");
      ("U16", "65536", "65536", "0", "131072", "0");
      ("I32", "4294967295", "4294967295", "-1", "8589934590", "-2");
      ("U32", "4294967296", "4294967296", "0", "8589934592", "0");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, wide, memory, updated, updated_memory) ->
          let source qualifier update =
            Printf.sprintf "I64 F(I64 c){%s %s x=%s;%sreturn x;}F(%s);" ty
              qualifier input update input
          in
          [
            (source "" "", wide);
            (source "noreg" "", memory);
            (source "" "x+=c;", updated);
            (source "noreg" "x+=c;", updated_memory);
          ])
        literal_parameter_locals
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, _, _, _, _) ->
          let source qualifier operation operand =
            Printf.sprintf "I64 F(I64 c){%s %s x=%s;x%s=c;return x;}F(%s);" ty
              qualifier input operation operand
          in
          let unsigned = String.starts_with ~prefix:"U" ty in
          let integer = Int64.of_string input in
          let sub = Int64.to_string (Int64.sub integer 1L) in
          let mul = Int64.to_string (Int64.mul integer 3L) in
          let or_value =
            if unsigned then input
            else Int64.to_string (Int64.logor integer (Int64.add integer 1L))
          in
          let xor_value = if unsigned then "0" else or_value in
          let mask =
            if unsigned then input else Int64.to_string (Int64.add integer 1L)
          in
          [
            (source "" "-" "1", sub);
            ( source "noreg" "-" "1",
              if unsigned then Int64.to_string (Int64.sub integer 1L) else "-2"
            );
            (source "" "*" "3", mul);
            (source "noreg" "*" "3", if unsigned then "0" else "-3");
            (source "" "|" mask, or_value);
            (source "noreg" "|" mask, if unsigned then "0" else "-1");
            (source "" "^" mask, xor_value);
            (source "noreg" "^" mask, if unsigned then "0" else "-1");
          ])
        literal_parameter_locals
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, _, _, updated, updated_memory) ->
          let source qualifier =
            Printf.sprintf "I64 F(I64 c){%s %s x=%s;x<<=c;return x;}F(1);" ty
              qualifier input
          in
          [ (source "", updated); (source "noreg", updated_memory) ])
        literal_parameter_locals
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, _, _, _, _) ->
          let source qualifier =
            Printf.sprintf "I64 F(I64 c){%s %s x=%s;x>>=c;return x;}F(1);" ty
              qualifier input
          in
          [
            ( source "",
              Int64.to_string
                (Int64.shift_right_logical (Int64.of_string input) 1) );
            ( source "noreg",
              if String.starts_with ~prefix:"U" ty then "0" else "-1" );
          ])
        literal_parameter_locals
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, _, _, _, _) ->
          let unsigned = String.starts_with ~prefix:"U" ty in
          let operand =
            if unsigned then
              Int64.to_string (Int64.sub (Int64.of_string input) 1L)
            else input
          in
          let source qualifier =
            Printf.sprintf "I64 F(I64 c){%s %s x=%s;x&=c;return x;}F(%s);" ty
              qualifier input operand
          in
          [
            (source "", if unsigned then "0" else input);
            (source "noreg", if unsigned then "0" else "-1");
          ])
        literal_parameter_locals
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, _, _, _, _) ->
          let source qualifier =
            Printf.sprintf "I64 F(I64 c){%s %s x=%s;x/=c;return x;}F(2);" ty
              qualifier input
          in
          [
            (source "", Int64.to_string (Int64.div (Int64.of_string input) 2L));
            (source "noreg", "0");
          ])
        literal_parameter_locals
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, input, _, _, _, _) ->
          let source qualifier =
            Printf.sprintf "I64 F(I64 c){%s %s x=%s;x%%=c;return x;}F(2);" ty
              qualifier input
          in
          let unsigned = String.starts_with ~prefix:"U" ty in
          [
            (source "", if unsigned then "0" else "1");
            (source "noreg", if unsigned then "0" else "-1");
          ])
        literal_parameter_locals
  in
  let cases =
    cases
    @ [
        ( "I64 F(I64 c){I8 x=255;x+=c;return x;}F(0x8000000000000000);",
          "-9223372036854775553" );
        ( "I64 F(I64 c){U8 x=256;x+=c;return x;}F(0x8000000000000000);",
          "-9223372036854775552" );
        ( "I64 F(I64 c){I8 noreg x=255;x+=c;return x;}F(0x8000000000000000);",
          "-1" );
      ]
  in
  let cases =
    cases
    @ [
        ("I64 F(I64 c){I8 x=255;x/=c;return x;}F(-1);", "-255");
        ("I64 F(I64 c){I8 noreg x=255;x/=c;return x;}F(-1);", "1");
        ("I64 F(I64 c){U8 x=256;x%=c;return x;}F(-1);", "256");
        ("I64 F(I64 c){U8 noreg x=256;x%=c;return x;}F(-1);", "0");
      ]
  in
  let unary_widths =
    [
      ( "I8",
        "127",
        "128",
        "-128",
        "-128",
        "-129",
        "127",
        "255",
        "256",
        "0",
        "254",
        "-2" );
      ( "U8",
        "255",
        "256",
        "0",
        "0",
        "-1",
        "255",
        "256",
        "257",
        "1",
        "255",
        "255" );
      ( "I16",
        "32767",
        "32768",
        "-32768",
        "-32768",
        "-32769",
        "32767",
        "65535",
        "65536",
        "0",
        "65534",
        "-2" );
      ( "U16",
        "65535",
        "65536",
        "0",
        "0",
        "-1",
        "65535",
        "65536",
        "65537",
        "1",
        "65535",
        "65535" );
      ( "I32",
        "2147483647",
        "2147483648",
        "-2147483648",
        "-2147483648",
        "-2147483649",
        "2147483647",
        "4294967295",
        "4294967296",
        "0",
        "4294967294",
        "-2" );
      ( "U32",
        "4294967295",
        "4294967296",
        "0",
        "0",
        "-1",
        "4294967295",
        "4294967296",
        "4294967297",
        "1",
        "4294967295",
        "4294967295" );
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun ( ty,
               pin,
               pinc,
               pinc_memory,
               pdecin,
               pdec,
               pdec_memory,
               initial,
               linc,
               linc_memory,
               ldec,
               ldec_memory ) ->
          let parameter qualifier operation input =
            Printf.sprintf "I64 F(%s %s x){%s;return x;}F(%s);" ty qualifier
              operation input
          in
          let local qualifier operation =
            Printf.sprintf "I64 F(){%s %s x=%s;%s;return x;}F();" ty qualifier
              initial operation
          in
          List.concat_map
            (fun (operation, input, wide, memory, lwide, lmemory) ->
              [
                (parameter "" operation input, wide);
                (parameter "noreg" operation input, memory);
                (local "" operation, lwide);
                (local "noreg" operation, lmemory);
              ])
            [
              ("x++", pin, pinc, pinc_memory, linc, linc_memory);
              ("++x", pin, pinc, pinc_memory, linc, linc_memory);
              ("x--", pdecin, pdec, pdec_memory, ldec, ldec_memory);
              ("--x", pdecin, pdec, pdec_memory, ldec, ldec_memory);
            ])
        unary_widths
  in
  let cases =
    cases
    @ List.concat_map
        (fun ( ty,
               pin,
               pinc,
               pinc_memory,
               pdecin,
               pdec,
               pdec_memory,
               initial,
               linc,
               linc_memory,
               ldec,
               ldec_memory ) ->
          List.concat_map
            (fun (operator, input, wide, memory, lwide, lmemory) ->
              let parameter qualifier =
                Printf.sprintf "I64 F(%s %s x){return %sx;}F(%s);" ty qualifier
                  operator input
              in
              let local qualifier =
                Printf.sprintf "I64 F(){%s %s x=%s;return %sx;}F();" ty
                  qualifier initial operator
              in
              [
                (parameter "", wide);
                (parameter "noreg", memory);
                (local "", lwide);
                (local "noreg", lmemory);
              ])
            [
              ("++", pin, pinc, pinc_memory, linc, linc_memory);
              ("--", pdecin, pdec, pdec_memory, ldec, ldec_memory);
            ])
        unary_widths
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, pin, _, _, pdecin, _, _, initial, _, _, _, _) ->
          let memory = if ty.[0] = 'I' then "-1" else "0" in
          List.concat_map
            (fun (operator, input) ->
              let parameter qualifier =
                Printf.sprintf "I64 F(%s %s x){return x%s;}F(%s);" ty qualifier
                  operator input
              in
              let local qualifier =
                Printf.sprintf "I64 F(){%s %s x=%s;return x%s;}F();" ty
                  qualifier initial operator
              in
              [
                (parameter "", input);
                (parameter "noreg", input);
                (local "", initial);
                (local "noreg", memory);
              ])
            [ ("++", pin); ("--", pdecin) ])
        unary_widths
  in
  let cases =
    cases
    @ List.concat_map
        (fun ( ty,
               initial,
               wide_add,
               memory_add,
               wide_sub,
               memory_sub,
               wide_shift,
               memory_shift,
               wide_right,
               memory_right,
               memory_initial ) ->
          List.concat_map
            (fun (operator, wide, memory) ->
              let source qualifier =
                Printf.sprintf "I64 F(){%s %s x=%s;return x%s=1;}F();" ty
                  qualifier initial operator
              in
              [ (source "", wide); (source "noreg", memory) ])
            [
              ("+", wide_add, memory_add);
              ("-", wide_sub, memory_sub);
              ("*", initial, memory_initial);
              ("<<", wide_shift, memory_shift);
              (">>", wide_right, memory_right);
            ])
        [
          ("I8", "255", "256", "0", "254", "-2", "510", "-2", "127", "-1", "-1");
          ("U8", "256", "257", "1", "255", "255", "512", "0", "128", "0", "0");
          ( "I16",
            "65535",
            "65536",
            "0",
            "65534",
            "-2",
            "131070",
            "-2",
            "32767",
            "-1",
            "-1" );
          ( "U16",
            "65536",
            "65537",
            "1",
            "65535",
            "65535",
            "131072",
            "0",
            "32768",
            "0",
            "0" );
          ( "I32",
            "4294967295",
            "4294967296",
            "0",
            "4294967294",
            "-2",
            "8589934590",
            "-2",
            "2147483647",
            "-1",
            "-1" );
          ( "U32",
            "4294967296",
            "4294967297",
            "1",
            "4294967295",
            "4294967295",
            "8589934592",
            "0",
            "2147483648",
            "0",
            "0" );
        ]
  in
  let cases =
    cases
    @ [
        ("I64 F(){I8 x=255;return x+=2147483647;}F();", "2147483902");
        ("I64 F(){U8 x=256;return x+=2147483647;}F();", "2147483903");
        ("I64 F(){I8 noreg x=255;return x+=2147483647;}F();", "-2");
        ("I64 F(){U8 noreg x=256;return x+=2147483647;}F();", "255");
        ("I64 F(){I8 x=255;return x-=2147483647;}F();", "-2147483392");
        ("I64 F(){U8 x=256;return x-=2147483647;}F();", "-2147483391");
        ("I64 F(){I8 noreg x=255;return x-=2147483647;}F();", "0");
        ("I64 F(){U8 noreg x=256;return x-=2147483647;}F();", "1");
      ]
  in
  let cases =
    cases
    @ [
        ("I64 F(){I8 x=255;return x+=256;}F();", "511");
        ("I64 F(){U8 x=256;return x+=256;}F();", "512");
        ("I64 F(){I8 noreg x=255;return x+=256;}F();", "-1");
        ("I64 F(){U8 noreg x=256;return x+=256;}F();", "0");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let add_narrow = if signed then minimum else 0L in
          let sub_narrow = if signed then maximum else sub n 1L in
          let twice_narrow = mul maximum 2L in
          let rows =
            [
              ("+", maximum, add maximum 1L, add_narrow);
              ("-", minimum, sub minimum 1L, sub_narrow);
              ("*", maximum, mul maximum 2L, twice_narrow);
              ("<<", maximum, mul maximum 2L, twice_narrow);
              (">>", maximum, div maximum 2L, div maximum 2L);
            ]
          in
          List.concat_map
            (fun (operator, input, wide, memory) ->
              let operand = if operator = "*" then "2" else "1" in
              let source qualifier =
                Printf.sprintf "I64 F(%s %s x){return x%s=%s;}F(%s);" ty
                  qualifier operator operand (to_string input)
              in
              [
                (source "", to_string wide); (source "noreg", to_string memory);
              ])
            rows)
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ [
        ("I64 F(){I8 noreg x=127;return x*=2;}F();", "254");
        ("I64 F(){U8 noreg x=255;return x*=2;}F();", "510");
        ("I64 F(){I8 noreg x=127;return x<<=1;}F();", "254");
        ("I64 F(){U8 noreg x=255;return x<<=1;}F();", "510");
      ]
  in
  let cases =
    cases
    @ [
        ("I64 F(I8 x){return x+=2147483647;}F(127);", "2147483774");
        ("I64 F(U8 x){return x+=2147483647;}F(255);", "2147483902");
        ("I64 F(I8 noreg x){return x+=2147483647;}F(127);", "126");
        ("I64 F(U8 noreg x){return x+=2147483647;}F(255);", "254");
        ("I64 F(I8 x){return x-=2147483647;}F(127);", "-2147483520");
        ("I64 F(U8 x){return x-=2147483647;}F(255);", "-2147483392");
        ("I64 F(I8 noreg x){return x-=2147483647;}F(127);", "-128");
        ("I64 F(U8 noreg x){return x-=2147483647;}F(255);", "0");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, maximum, minimum, add_result, sub_result) ->
          [
            ( Printf.sprintf "%s G=%s;I64 F(){return G+=1;}F();" ty maximum,
              add_result );
            ( Printf.sprintf "%s G=%s;I64 F(){return G-=1;}F();" ty minimum,
              sub_result );
          ])
        [
          ("I8", "127", "-128", "-128", "127");
          ("U8", "255", "0", "0", "255");
          ("I16", "32767", "-32768", "-32768", "32767");
          ("U16", "65535", "0", "0", "65535");
          ("I32", "2147483647", "-2147483648", "-2147483648", "2147483647");
          ("U32", "4294967295", "0", "0", "4294967295");
        ]
    @ [
        ("U8 G=255;I64 N=1;I64 F(){return G+=N;}F();", "256");
        ("U8 G=255;I64 F(){U8 *p=&G;return *p+=1;}F();", "256");
        ("U8 G=255;I64 F(){return G*=2;}F();", "510");
        ("U8 G=255;I64 F(){return G<<=1;}F();", "510");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let normalize value =
            let value = logand value (sub n 1L) in
            if signed && value >= div n 2L then sub value n else value
          in
          List.concat_map
            (fun amount ->
              [
                ( Printf.sprintf "%s G=%s;I64 F(){return G+=%s;}F();" ty
                    (to_string maximum) (to_string amount),
                  to_string (normalize (add maximum amount)) );
                ( Printf.sprintf "%s G=%s;I64 F(){return G-=%s;}F();" ty
                    (to_string minimum) (to_string amount),
                  to_string (normalize (sub minimum amount)) );
              ])
            [ 256L; 2147483647L ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ [
        ("I8 G=127;I64 F(){return G+=127;}F();", "-2");
        ("U8 G=255;I64 F(){return G+=128;}F();", "127");
        ("I8 G=-128;I64 F(){return G-=127;}F();", "1");
        ("U8 G=0;I64 F(){return G-=128;}F();", "128");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let normalize value =
            let value = logand value (sub n 1L) in
            if signed && value >= div n 2L then sub value n else value
          in
          List.concat_map
            (fun (operator, initial) ->
              List.map
                (fun (inner, outer) ->
                  let updated =
                    if operator = "+" then add initial inner
                    else sub initial inner
                  in
                  let source =
                    Printf.sprintf "%s G=%s;I64 F(){return (G%s=%s)+%s;}F();" ty
                      (to_string initial) operator (to_string inner)
                      (to_string outer)
                  in
                  (source, to_string (add (normalize updated) outer)))
                [
                  (1L, 0L);
                  (1L, 1L);
                  (1L, 256L);
                  (1L, 2147483647L);
                  (256L, 0L);
                  (2147483647L, 0L);
                ])
            [ ("+", maximum); ("-", minimum) ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let add_result = if signed then neg (div n 2L) else 0L in
          let sub_result = if signed then maximum else sub n 1L in
          List.concat_map
            (fun rhs_type ->
              List.map
                (fun (operator, initial, narrowed) ->
                  let source =
                    Printf.sprintf
                      "%s G=%s;%s N=7;I64 F(){return (G%s=1)+N;}F();" ty
                      (to_string initial) rhs_type operator
                  in
                  (source, to_string (add narrowed 7L)))
                [ ("+", maximum, add_result); ("-", minimum, sub_result) ])
            [ "I64"; "U64" ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
    @ [
        ("U8 G=255;I64 N=-7;I64 F(){return (G+=1)+N;}F();", "-7");
        ("U8 G=0;I64 N=-7;I64 F(){return (G-=1)+N;}F();", "248");
        ( "U8 G=255;I64 N=0x7FFFFFFFFFFFFFFF;I64 F(){return (G+=1)+N;}F();",
          "9223372036854775807" );
        ("U8 G=0;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G-=1)+N;}F();", "254");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let add_result = if signed then neg (div n 2L) else 0L in
          let sub_result = if signed then maximum else sub n 1L in
          List.concat_map
            (fun (rhs, rhs_source) ->
              List.map
                (fun (operator, initial, narrowed) ->
                  ( Printf.sprintf
                      "%s G=%s;I64 N=%s;I64 F(){return (G%s=1)+N;}F();" ty
                      (to_string initial) rhs_source operator,
                    to_string (add narrowed rhs) ))
                [ ("+", maximum, add_result); ("-", minimum, sub_result) ])
            [ (-7L, "-7"); (max_int, "0x7FFFFFFFFFFFFFFF") ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
    @ [
        ( "I8 G=127;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G+=1)+N;}F();",
          "-129" );
        ("U8 G=255;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G+=1)+N;}F();", "-1");
        ( "I8 G=-128;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G-=1)+N;}F();",
          "126" );
        ("U8 G=0;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G-=1)+N;}F();", "254");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let normalize value =
            let value = logand value (sub n 1L) in
            if signed && value >= div n 2L then sub value n else value
          in
          List.concat_map
            (fun parameter ->
              List.concat_map
                (fun (op, initial) ->
                  List.map
                    (fun amount ->
                      let updated =
                        if op = "+" then add initial amount
                        else sub initial amount
                      in
                      ( Printf.sprintf
                          "%s G=%s;I64 ID(%s x){return x;}I64 F(){return \
                           ID(G%s=%s);}F();"
                          ty (to_string initial) parameter op (to_string amount),
                        to_string (normalize updated) ))
                    [ 1L; 256L; 2147483647L ])
                [ ("+", maximum); ("-", minimum) ])
            [ "I64"; "U64" ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
    @ [
        ( "I8 G=127;I64 ID(I64 x){return x*2;}I64 F(){return ID(G+=1);}F();",
          "-256" );
        ("U8 G=255;I64 ID(I64 x){return x*2;}I64 F(){return ID(G+=1);}F();", "0");
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let normalize value =
            let value = logand value (sub n 1L) in
            if signed && value >= div n 2L then sub value n else value
          in
          List.concat_map
            (fun (op, initial) ->
              List.map
                (fun (inner, outer) ->
                  let updated =
                    if op = "+" then add initial inner else sub initial inner
                  in
                  ( Printf.sprintf "%s G=%s;I64 F(){return (G%s=%s)-%s;}F();" ty
                      (to_string initial) op (to_string inner) (to_string outer),
                    to_string (sub (normalize updated) outer) ))
                [ (1L, 0L); (1L, 1L); (1L, 2147483647L); (256L, 1L) ])
            [ ("+", maximum); ("-", minimum) ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let add_result = if signed then neg (div n 2L) else 0L in
          let sub_result = if signed then maximum else sub n 1L in
          List.concat_map
            (fun rhs_type ->
              List.map
                (fun (op, initial, narrowed) ->
                  ( Printf.sprintf
                      "%s G=%s;%s N=7;I64 F(){return (G%s=1)-N;}F();" ty
                      (to_string initial) rhs_type op,
                    to_string (sub narrowed 7L) ))
                [ ("+", maximum, add_result); ("-", minimum, sub_result) ])
            [ "I64"; "U64" ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ [
        ("U8 G=255;I64 N=-7;I64 F(){return (G+=1)-N;}F();", "7");
        ("U8 G=0;I64 N=-7;I64 F(){return (G-=1)-N;}F();", "262");
        ( "I8 G=127;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G+=1)-N;}F();",
          "-127" );
        ("U8 G=255;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G+=1)-N;}F();", "1");
        ( "I8 G=-128;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G-=1)-N;}F();",
          "128" );
        ("U8 G=0;U64 N=0xFFFFFFFFFFFFFFFF;I64 F(){return (G-=1)-N;}F();", "256");
        ( "I8 G=127;I64 N=0x7FFFFFFFFFFFFFFF;I64 F(){return (G+=1)-N;}F();",
          "9223372036854775681" );
        ( "I8 G=-128;I64 N=0x7FFFFFFFFFFFFFFF;I64 F(){return (G-=1)-N;}F();",
          "-9223372036854775680" );
        ( "U8 G=255;I64 N=0x7FFFFFFFFFFFFFFF;I64 F(){return (G+=1)-N;}F();",
          "-9223372036854775807" );
        ( "U8 G=0;I64 N=0x7FFFFFFFFFFFFFFF;I64 F(){return (G-=1)-N;}F();",
          "-9223372036854775552" );
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let normalize v =
            let v = logand v (sub n 1L) in
            if signed && v >= div n 2L then sub v n else v
          in
          List.concat_map
            (fun (op, initial) ->
              List.map
                (fun (inner, outer) ->
                  let updated =
                    if op = "+" then add initial inner else sub initial inner
                  in
                  ( Printf.sprintf "%s G=%s;I64 F(){return (G%s=%s)*%s;}F();" ty
                      (to_string initial) op (to_string inner) (to_string outer),
                    to_string (mul (normalize updated) outer) ))
                [
                  (1L, 2L);
                  (1L, 3L);
                  (1L, 256L);
                  (1L, 2147483647L);
                  (256L, 2L);
                  (2147483647L, 2L);
                ])
            [ ("+", maximum); ("-", minimum) ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let minimum = if signed then neg (div n 2L) else 0L in
          let add_result = if signed then neg (div n 2L) else 0L in
          let sub_result = if signed then maximum else sub n 1L in
          List.concat_map
            (fun rhs_type ->
              List.map
                (fun (op, initial, narrowed) ->
                  ( Printf.sprintf
                      "%s G=%s;%s N=7;I64 F(){return (G%s=1)*N;}F();" ty
                      (to_string initial) rhs_type op,
                    to_string (mul narrowed 7L) ))
                [ ("+", maximum, add_result); ("-", minimum, sub_result) ])
            [ "I64"; "U64" ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
    @ List.concat_map
        (fun (rhs_type, value, source) ->
          List.map
            (fun (ty, initial, op, narrowed) ->
              ( Printf.sprintf "%s G=%s;%s N=%s;I64 F(){return (G%s=1)*N;}F();"
                  ty initial rhs_type source op,
                Int64.(to_string (mul narrowed value)) ))
            [
              ("I8", "127", "+", -128L);
              ("U8", "255", "+", 0L);
              ("I8", "-128", "-", 127L);
              ("U8", "0", "-", 255L);
            ])
        [
          ("I64", -7L, "-7");
          ("I64", Int64.max_int, "0x7FFFFFFFFFFFFFFF");
          ("U64", -1L, "0xFFFFFFFFFFFFFFFF");
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun reg ->
          [
            ( Printf.sprintf
                "I64 F(I8 reg %s x,I64 c){x<<=c;return x;}F(127,1);" reg,
              "254" );
            ( Printf.sprintf
                "I64 F(U8 reg %s x,I64 c){x<<=c;return x;}F(255,1);" reg,
              "510" );
            ( Printf.sprintf "I64 F(){I8 reg %s x=127;x<<=1;return x;}F();" reg,
              "254" );
            ( Printf.sprintf "I64 F(){U8 reg %s x=255;x<<=1;return x;}F();" reg,
              "510" );
          ])
        [ "R12"; "R13"; "R14"; "R15" ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, expected) ->
          List.concat_map
            (fun qualifier ->
              [
                ( Printf.sprintf "I64 F(%s %s x,I64 c){x<<=c;return x;}F(%s,1);"
                    ty qualifier initial,
                  expected );
                ( Printf.sprintf "I64 F(){%s %s x=%s;x<<=1;return x;}F();" ty
                    qualifier initial,
                  expected );
              ])
            [ "reg R12"; "reg" ])
        [
          ("I8", "127", "254");
          ("U8", "255", "510");
          ("I16", "32767", "65534");
          ("U16", "65535", "131070");
          ("I32", "2147483647", "4294967294");
          ("U32", "4294967295", "8589934590");
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun reg ->
          [
            ( Printf.sprintf
                "I64 F(I8 x,I64 reg %s c){x<<=c;return x;}F(127,1);" reg,
              "254" );
            ( Printf.sprintf
                "I64 F(U8 x,I64 reg %s c){x<<=c;return x;}F(255,1);" reg,
              "510" );
          ])
        [ "R12"; "R13"; "R14"; "R15" ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (prefix, suffix, before, after) ->
          List.concat_map
            (fun (ty, initial, wide, narrow) ->
              List.map
                (fun (qualifier, expected) ->
                  ( Printf.sprintf
                      "I64 F(%s%s %s x%s){x<<=c;return x;}F(%s%s%s);" prefix ty
                      qualifier suffix before initial after,
                    expected ))
                [ ("", wide); ("noreg", narrow) ])
            [
              ("I8", "127", "254", "-2");
              ("U8", "255", "510", "254");
              ("I16", "32767", "65534", "-2");
              ("U16", "65535", "131070", "65534");
              ("I32", "2147483647", "4294967294", "-2");
              ("U32", "4294967295", "8589934590", "4294967294");
            ])
        [
          ("I64 a,", ",I64 c", "0,", ",1");
          ("I64 a,", ",I64 d,I64 c", "0,", ",0,1");
          ("I64 a,I64 b,", ",I64 d,I64 c", "0,0,", ",0,1");
          ("I64 a,I64 b,", ",I64 d,I64 e,I64 c", "0,0,", ",0,0,1");
        ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I64 a,I64 b,I8 reg R12 x,I64 d,I64 e,I64 c){x<<=c;return \
           x;}F(0,0,127,0,0,1);",
          "254" );
        ( "I64 F(I64 a,I64 b,I8 x,I64 d,I64 e,I64 reg R12 c){x<<=c;return \
           x;}F(0,0,127,0,0,1);",
          "254" );
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, op, count, wide, narrow) ->
          List.map
            (fun (qualifier, expected) ->
              ( Printf.sprintf
                  "I64 F(I64 a,I64 b,%s %s x,I64 d,I64 e,I64 c){x%s=c;return \
                   x;}F(0,0,%s,0,0,%s);"
                  ty qualifier op initial count,
                expected ))
            [ ("", wide); ("noreg", narrow) ])
        [
          ("I8", "127", "+", "1", "128", "-128");
          ("U8", "255", "+", "1", "256", "0");
          ("I8", "-128", "-", "1", "-129", "127");
          ("U8", "0", "-", "1", "-1", "255");
          ("I8", "127", "*", "2", "254", "-2");
          ("U8", "255", "*", "2", "510", "254");
          ("I8", "-128", "/", "-1", "128", "-128");
          ("U8", "255", "/", "2", "127", "127");
          ("I8", "127", "|", "256", "383", "127");
          ("U8", "255", "|", "256", "511", "255");
          ("I8", "127", "^", "256", "383", "127");
          ("U8", "255", "^", "256", "511", "255");
          ("I8", "-1", "&", "255", "255", "-1");
          ("U8", "255", "&", "255", "255", "255");
        ]
  in
  let cases =
    cases
    @ [ (read "../examples/register-variable-selection.hc", "512") ]
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
          let narrow = if signed then neg (div n 2L) else 0L in
          let wide = add maximum 1L in
          List.map
            (fun (qx, qy, vx, vy) ->
              ( Printf.sprintf
                  "I64 F(%s %s x,%s %s y,I64 c){x+=c;y+=c;return \
                   x+y;}F(%s,%s,1);"
                  ty qx ty qy (to_string maximum) (to_string maximum),
                to_string (add vx vy) ))
            [
              ("", "", wide, wide);
              ("noreg", "noreg", narrow, narrow);
              ("", "noreg", wide, narrow);
              ("noreg", "", narrow, wide);
              ("reg R12", "reg R13", wide, wide);
            ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ [
        ("I64 F(I8 x,I8 y,I8 c){x+=c;y+=c;return x+y;}F(-128,-128,-1);", "-258");
        ( "I64 F(I8 noreg x,I8 noreg y,I8 c){x+=c;y+=c;return \
           x+y;}F(-128,-128,-1);",
          "254" );
        ("I64 F(U8 x,U8 y,I8 c){x+=c;y+=c;return x+y;}F(0,0,-1);", "-2");
        ( "I64 F(U8 noreg x,U8 noreg y,I8 c){x+=c;y+=c;return x+y;}F(0,0,-1);",
          "510" );
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, first, second, count, wide, narrow) ->
          List.map
            (fun (qualifier, expected) ->
              ( Printf.sprintf
                  "I64 F(%s %s x,%s %s y,I64 c){x%s=c;y%s=c;return \
                   x+y;}F(%s,%s,%s);"
                  ty qualifier ty qualifier first second initial initial count,
                expected ))
            [ ("", wide); ("noreg", narrow) ])
        [
          ("I8", "-128", "-", "-", "1", "-258", "254");
          ("U8", "0", "-", "-", "1", "-2", "510");
          ("I8", "127", "*", "*", "2", "508", "-4");
          ("U8", "255", "*", "*", "2", "1020", "508");
          ("I8", "127", "+", "-", "1", "254", "-2");
          ("U8", "255", "+", "-", "1", "510", "254");
        ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I64 a,I8 x,I64 b,I8 y,I64 c,I64 d){x+=c;y+=c;return \
           x+y;}F(0,127,0,127,1,0);",
          "256" );
      ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I8 x,I8 y,I64 c,I64 d){x+=c;y+=d;return x+y;}F(127,127,1,2);",
          "257" );
        ( "I64 F(U8 x,U8 y,I64 c,I64 d){x+=c;y+=d;return x+y;}F(255,255,1,2);",
          "513" );
        ( "I64 F(I8 noreg x,I8 noreg y,I64 c,I64 d){x+=c;y+=d;return \
           x+y;}F(127,127,1,2);",
          "-255" );
        ( "I64 F(U8 noreg x,U8 noreg y,I64 c,I64 d){x+=c;y+=d;return \
           x+y;}F(255,255,1,2);",
          "1" );
        ( "I64 F(I8 x,I8 y,I64 reg R12 c,I64 reg R13 d){x+=c;y+=d;return \
           x+y;}F(127,127,1,2);",
          "257" );
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, op, count, wide, narrow) ->
          List.map
            (fun (qualifier, expected) ->
              ( Printf.sprintf
                  "I64 F(%s %s x,%s %s y,I64 c){x%s=c;y%s=c;return \
                   x+y;}F(%s,%s,%s);"
                  ty qualifier ty qualifier op op initial initial count,
                expected ))
            [ ("", wide); ("noreg", narrow) ])
        [
          ("I8", "127", "<<", "1", "508", "-4");
          ("U8", "255", "<<", "1", "1020", "508");
          ("I8", "-128", ">>", "1", "-128", "-128");
          ("U8", "255", ">>", "1", "254", "254");
          ("I8", "127", "|", "256", "766", "254");
          ("U8", "255", "|", "256", "1022", "510");
          ("I8", "127", "^", "256", "766", "254");
          ("U8", "255", "^", "256", "1022", "510");
          ("I8", "-1", "&", "255", "510", "-2");
          ("U8", "255", "&", "255", "510", "510");
          ("I8", "-128", "/", "-1", "256", "-256");
          ("U8", "255", "/", "2", "254", "254");
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (signed_ty, unsigned_ty, bits) ->
          let open Int64 in
          let n = shift_left 1L bits in
          let smax = sub (div n 2L) 1L and umax = sub n 1L in
          let swide = div n 2L and uwide = n and snarrow = neg (div n 2L) in
          List.concat_map
            (fun (left, right, linit, rinit, lwide, rwide, lnarrow, rnarrow) ->
              List.map
                (fun (ql, qr, lv, rv) ->
                  ( Printf.sprintf
                      "I64 F(%s %s x,%s %s y,I64 c){x+=c;y+=c;return \
                       x+y;}F(%s,%s,1);"
                      left ql right qr (to_string linit) (to_string rinit),
                    to_string (add lv rv) ))
                [
                  ("", "", lwide, rwide);
                  ("noreg", "noreg", lnarrow, rnarrow);
                  ("", "noreg", lwide, rnarrow);
                  ("noreg", "", lnarrow, rwide);
                ])
            [
              (signed_ty, unsigned_ty, smax, umax, swide, uwide, snarrow, 0L);
              (unsigned_ty, signed_ty, umax, smax, uwide, swide, 0L, snarrow);
            ])
        [ ("I8", "U8", 8); ("I16", "U16", 16); ("I32", "U32", 32) ]
  in
  let cases =
    cases
    @ [
        ("I64 F(I8 x,U8 y,I64 c){x<<=c;y|=c;return x+y;}F(127,255,1);", "509");
        ("I64 F(U8 x,I8 y,I64 c){x<<=c;y|=c;return x+y;}F(255,127,1);", "637");
        ( "I64 F(I8 noreg x,U8 noreg y,I64 c){x<<=c;y|=c;return \
           x+y;}F(127,255,1);",
          "253" );
        ( "I64 F(U8 noreg x,I8 noreg y,I64 c){x<<=c;y|=c;return \
           x+y;}F(255,127,1);",
          "381" );
      ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (left, lb, ls, right, rb, rs) ->
          let open Int64 in
          let values bits signed =
            let n = shift_left 1L bits in
            let maximum = if signed then sub (div n 2L) 1L else sub n 1L in
            (maximum, add maximum 1L, if signed then neg (div n 2L) else 0L)
          in
          let linit, lwide, lnarrow = values lb ls
          and rinit, rwide, rnarrow = values rb rs in
          List.concat_map
            (fun (l, r, li, ri, lw, rw, ln, rn) ->
              List.map
                (fun (ql, qr, lv, rv) ->
                  ( Printf.sprintf
                      "I64 F(%s %s x,%s %s y,I64 c){x+=c;y+=c;return \
                       x+y;}F(%s,%s,1);"
                      l ql r qr (to_string li) (to_string ri),
                    to_string (add lv rv) ))
                [
                  ("", "", lw, rw);
                  ("noreg", "noreg", ln, rn);
                  ("", "noreg", lw, rn);
                  ("noreg", "", ln, rw);
                ])
            [
              (left, right, linit, rinit, lwide, rwide, lnarrow, rnarrow);
              (right, left, rinit, linit, rwide, lwide, rnarrow, lnarrow);
            ])
        [
          ("I8", 8, true, "U16", 16, false);
          ("I16", 16, true, "U32", 32, false);
          ("I8", 8, true, "U32", 32, false);
          ("I16", 16, true, "U8", 8, false);
          ("I32", 32, true, "U16", 16, false);
          ("I32", 32, true, "U8", 8, false);
          ("I8", 8, true, "I16", 16, true);
          ("I8", 8, true, "I32", 32, true);
          ("I16", 16, true, "I32", 32, true);
          ("U8", 8, false, "U16", 16, false);
          ("U8", 8, false, "U32", 32, false);
          ("U16", 16, false, "U32", 32, false);
        ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I8 x,U16 y,I64 c){x<<=c;y|=c;return x+y;}F(127,65535,1);",
          "65789" );
        ( "I64 F(U16 x,I8 y,I64 c){x<<=c;y|=c;return x+y;}F(65535,127,1);",
          "131197" );
        ( "I64 F(I8 noreg x,U16 noreg y,I64 c){x<<=c;y|=c;return \
           x+y;}F(127,65535,1);",
          "65533" );
        ( "I64 F(U16 noreg x,I8 noreg y,I64 c){x<<=c;y|=c;return \
           x+y;}F(65535,127,1);",
          "65661" );
      ]
  in
  let division_widths =
    [
      ("I8", "-128", "-1", "128", "-128");
      ("U8", "255", "2", "127", "127");
      ("I16", "-32768", "-1", "32768", "-32768");
      ("U16", "65535", "2", "32767", "32767");
      ("I32", "-2147483648", "-1", "2147483648", "-2147483648");
      ("U32", "4294967295", "2", "2147483647", "2147483647");
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, initial, divisor, wide, narrow) ->
          let source qualifier =
            Printf.sprintf "I64 F(%s %s x,I8 c){x/=c;return x;}F(%s,%s);" ty
              qualifier initial divisor
          in
          [ (source "", wide); (source "noreg", narrow) ])
        division_widths
    @ [
        ("I64 F(I8 x,I8 c){x/=c;return x;}F(-7,2);", "-3");
        ("I64 F(I8 noreg x,I8 c){x/=c;return x;}F(-7,2);", "-3");
        ("I64 F(U8 x,I8 c){x/=c;return x;}F(255,-1);", "0");
        ("I64 F(U8 noreg x,I8 c){x/=c;return x;}F(255,-1);", "0");
      ]
  in
  let bitwise_widths =
    [
      ("I8", "127", "256", "383", "127", "-1", "255", "255", "-1");
      ("U8", "255", "256", "511", "255", "255", "255", "255", "255");
      ("I16", "32767", "65536", "98303", "32767", "-1", "65535", "65535", "-1");
      ( "U16",
        "65535",
        "65536",
        "131071",
        "65535",
        "65535",
        "65535",
        "65535",
        "65535" );
      ( "I32",
        "2147483647",
        "4294967296",
        "6442450943",
        "2147483647",
        "-1",
        "4294967295",
        "4294967295",
        "-1" );
      ( "U32",
        "4294967295",
        "4294967296",
        "8589934591",
        "4294967295",
        "4294967295",
        "4294967295",
        "4294967295",
        "4294967295" );
    ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun ( ty,
               initial,
               mask,
               wide,
               narrow,
               and_initial,
               and_mask,
               and_wide,
               and_narrow ) ->
          let source qualifier op x y =
            Printf.sprintf "I64 F(%s %s x,I64 c){x%s=c;return x;}F(%s,%s);" ty
              qualifier op x y
          in
          List.concat_map
            (fun op ->
              [
                (source "" op initial mask, wide);
                (source "noreg" op initial mask, narrow);
              ])
            [ "|"; "^" ]
          @ [
              (source "" "&" and_initial and_mask, and_wide);
              (source "noreg" "&" and_initial and_mask, and_narrow);
            ])
        bitwise_widths
    @ [
        ("I64 F(I8 x,I8 c){x|=c;return x;}F(127,256);", "127");
        ("I64 F(I8 x,I8 c){x^=c;return x;}F(127,256);", "127");
        ("I64 F(I8 x,I8 c){x&=c;return x;}F(-1,255);", "-1");
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
  let cases =
    cases
    @ List.concat_map
        (fun count ->
          let names =
            List.filteri
              (fun index _ -> index < count)
              [ "x"; "y"; "z"; "w"; "v" ]
          in
          List.concat_map
            (fun (ty, bits, signed) ->
              let unit =
                Int64.shift_left 1L (if signed then bits - 1 else bits)
              in
              let initial = Int64.to_string (Int64.pred unit) in
              List.map
                (fun stack ->
                  let qualifier = if stack then "noreg" else "" in
                  let parameters =
                    names
                    |> List.map (fun name -> ty ^ " " ^ qualifier ^ " " ^ name)
                    |> String.concat ","
                  in
                  let updates =
                    names
                    |> List.map (fun name -> name ^ "+=c;")
                    |> String.concat ""
                  in
                  let arguments =
                    names |> List.map (fun _ -> initial) |> String.concat ","
                  in
                  let source =
                    "I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
                    ^ String.concat "+" names ^ ";}F(" ^ arguments ^ ",1);"
                  in
                  let expected =
                    if stack then
                      if signed then
                        Int64.neg (Int64.mul (Int64.of_int count) unit)
                      else 0L
                    else Int64.mul (Int64.of_int count) unit
                  in
                  (source, Int64.to_string expected))
                [ false; true ])
            [
              ("I8", 8, true);
              ("U8", 8, false);
              ("I16", 16, true);
              ("U16", 16, false);
              ("I32", 32, true);
              ("U32", 32, false);
            ])
        [ 3; 4; 5 ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun count ->
          let names =
            List.filteri
              (fun index _ -> index < count)
              [ "x"; "y"; "z"; "w"; "v" ]
          in
          List.concat_map
            (fun (ty, bits, signed) ->
              let unit =
                Int64.shift_left 1L (if signed then bits - 1 else bits)
              in
              let max = Int64.pred unit in
              let wrap_add =
                if signed then Int64.succ (Int64.neg unit) else 1L
              in
              let wrap_double = if signed then -2L else Int64.sub unit 2L in
              let operations =
                [
                  ("+", 2, Int64.add max 2L, wrap_add);
                  ("-", 1, Int64.pred max, Int64.pred max);
                  ("*", 2, Int64.mul max 2L, wrap_double);
                  ("/", 1, max, max);
                  ("<<", 1, Int64.mul max 2L, wrap_double);
                  (">>", 1, Int64.div max 2L, Int64.div max 2L);
                  ("&", 127, 127L, 127L);
                  ("|", 1, max, max);
                  ("^", 1, Int64.pred max, Int64.pred max);
                ]
              in
              List.concat_map
                (fun (op, operand, wide, narrow) ->
                  List.map
                    (fun (qualifier, expected) ->
                      let parameters =
                        names
                        |> List.map (fun name ->
                            ty ^ " " ^ qualifier ^ " " ^ name)
                        |> String.concat ","
                      in
                      let updates =
                        names
                        |> List.map (fun name -> name ^ op ^ "=c;")
                        |> String.concat ""
                      in
                      let arguments =
                        names
                        |> List.map (fun _ -> Int64.to_string max)
                        |> String.concat ","
                      in
                      let source =
                        "I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
                        ^ String.concat "+" names ^ ";}F(" ^ arguments ^ ","
                        ^ string_of_int operand ^ ");"
                      in
                      ( source,
                        Int64.to_string
                          (Int64.mul (Int64.of_int count) expected) ))
                    [ ("", wide); ("noreg", narrow) ])
                operations)
            [
              ("I8", 8, true);
              ("U8", 8, false);
              ("I16", 16, true);
              ("U16", 16, false);
              ("I32", 32, true);
              ("U32", 32, false);
            ])
        [ 3; 4; 5 ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun count ->
          let names =
            List.filteri
              (fun index _ -> index < count)
              [ "x"; "y"; "z"; "w"; "v" ]
          in
          List.concat_map
            (fun (ty, bits, signed) ->
              let unit =
                Int64.shift_left 1L (if signed then bits - 1 else bits)
              in
              let modulus = Int64.shift_left 1L bits in
              let max = Int64.pred unit in
              let wrap value =
                let value = Int64.logand value (Int64.pred modulus) in
                if signed && value >= unit then Int64.sub value modulus
                else value
              in
              let value = function
                | "+" -> Int64.add max 2L
                | "-" -> Int64.sub max 2L
                | "*" -> Int64.mul max 2L
                | "/" -> Int64.div max 2L
                | "<<" -> Int64.mul max 4L
                | ">>" -> Int64.div max 4L
                | "&" -> Int64.logand max 2L
                | "|" -> Int64.logor max 2L
                | "^" -> Int64.logxor max 2L
                | _ -> assert false
              in
              List.concat_map
                (fun pattern ->
                  let ops =
                    List.filteri (fun index _ -> index < count) pattern
                  in
                  List.map
                    (fun stack ->
                      let qualifier = if stack then "noreg" else "" in
                      let parameters =
                        names
                        |> List.map (fun name ->
                            ty ^ " " ^ qualifier ^ " " ^ name)
                        |> String.concat ","
                      in
                      let updates =
                        List.map2 (fun name op -> name ^ op ^ "=c;") names ops
                        |> String.concat ""
                      in
                      let arguments =
                        names
                        |> List.map (fun _ -> Int64.to_string max)
                        |> String.concat ","
                      in
                      let source =
                        "I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
                        ^ String.concat "+" names ^ ";}F(" ^ arguments ^ ",2);"
                      in
                      let expected =
                        List.fold_left
                          (fun sum op ->
                            let v = value op in
                            Int64.add sum (if stack then wrap v else v))
                          0L ops
                      in
                      (source, Int64.to_string expected))
                    [ false; true ])
                [
                  [ "+"; "-"; "*"; "<<"; "^" ];
                  [ "/"; ">>"; "&"; "|"; "+" ];
                  [ "^"; "&"; "*"; ">>"; "/" ];
                ])
            [
              ("I8", 8, true);
              ("U8", 8, false);
              ("I16", 16, true);
              ("U16", 16, false);
              ("I32", 32, true);
              ("U32", 32, false);
            ])
        [ 3; 4; 5 ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (ty, bits, signed) ->
          let unit = Int64.shift_left 1L (if signed then bits - 1 else bits) in
          let max = Int64.pred unit in
          List.concat_map
            (fun count ->
              let names =
                List.filteri
                  (fun index _ -> index < count)
                  [ "x"; "y"; "z"; "w" ]
              in
              let suffix, values, operands =
                if count = 3 then
                  ( "I64 c,I64 d,I64 e",
                    "2,2,1",
                    [ ("+", "c"); ("*", "d"); ("<<", "e") ] )
                else
                  ( "I64 c,I64 d",
                    "2,1",
                    [ ("+", "c"); ("-", "d"); ("*", "c"); ("<<", "d") ] )
              in
              List.map
                (fun stack ->
                  let qualifier = if stack then "noreg" else "" in
                  let parameters =
                    names
                    |> List.map (fun name -> ty ^ " " ^ qualifier ^ " " ^ name)
                    |> String.concat ","
                  in
                  let updates =
                    List.map2
                      (fun name (op, operand) ->
                        name ^ op ^ "=" ^ operand ^ ";")
                      names operands
                    |> String.concat ""
                  in
                  let arguments =
                    names
                    |> List.map (fun _ -> Int64.to_string max)
                    |> String.concat ","
                  in
                  let source =
                    "I64 F(" ^ parameters ^ "," ^ suffix ^ "){" ^ updates
                    ^ "return " ^ String.concat "+" names ^ ";}F(" ^ arguments
                    ^ "," ^ values ^ ");"
                  in
                  let expected =
                    if not stack then
                      if count = 3 then Int64.add (Int64.mul 5L max) 2L
                      else Int64.add (Int64.mul 6L max) 1L
                    else if count = 3 then
                      if signed then Int64.sub (Int64.neg unit) 3L
                      else Int64.sub (Int64.mul 2L unit) 3L
                    else if signed then -5L
                    else Int64.sub (Int64.mul 3L unit) 5L
                  in
                  (source, Int64.to_string expected))
                [ false; true ])
            [ 3; 4 ])
        [
          ("I8", 8, true);
          ("U8", 8, false);
          ("I16", 16, true);
          ("U16", 16, false);
          ("I32", 32, true);
          ("U32", 32, false);
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (types, values, wide, narrow) ->
          let names =
            List.filteri
              (fun index _ -> index < List.length types)
              [ "x"; "y"; "z"; "w" ]
          in
          List.map
            (fun (qualifier, expected) ->
              let parameters =
                List.map2
                  (fun ty name -> ty ^ " " ^ qualifier ^ " " ^ name)
                  types names
                |> String.concat ","
              in
              let updates =
                names
                |> List.map (fun name -> name ^ "+=c;")
                |> String.concat ""
              in
              let source =
                "I64 F(" ^ parameters ^ ",I64 c){" ^ updates ^ "return "
                ^ String.concat "+" names ^ ";}F(" ^ String.concat "," values
                ^ ",1);"
              in
              (source, expected))
            [ ("", wide); ("noreg", narrow) ])
        [
          ( [ "I8"; "U16"; "I32" ],
            [ "127"; "65535"; "2147483647" ],
            "2147549312",
            "-2147483776" );
          ( [ "I32"; "U16"; "I8" ],
            [ "2147483647"; "65535"; "127" ],
            "2147549312",
            "-2147483776" );
          ( [ "U8"; "I16"; "U32" ],
            [ "255"; "32767"; "4294967295" ],
            "4295000320",
            "-32768" );
          ( [ "U32"; "I16"; "U8" ],
            [ "4294967295"; "32767"; "255" ],
            "4295000320",
            "-32768" );
          ( [ "I8"; "U16"; "I32"; "U32" ],
            [ "127"; "65535"; "2147483647"; "4294967295" ],
            "6442516608",
            "-2147483776" );
          ( [ "U32"; "I32"; "U16"; "I8" ],
            [ "4294967295"; "2147483647"; "65535"; "127" ],
            "6442516608",
            "-2147483776" );
        ]
  in
  let cases =
    cases
    @ List.concat_map
        (fun (types, values, updates, counts, extra, wide, narrow) ->
          let names =
            List.filteri
              (fun index _ -> index < List.length types)
              [ "x"; "y"; "z"; "w"; "v" ]
          in
          List.map
            (fun (qualifier, expected) ->
              let parameters =
                List.map2
                  (fun ty name -> ty ^ " " ^ qualifier ^ " " ^ name)
                  types names
                |> String.concat ","
              in
              let statements =
                List.map2
                  (fun name (op, operand) -> name ^ op ^ "=" ^ operand ^ ";")
                  names updates
                |> String.concat ""
              in
              let source =
                "I64 F(" ^ parameters ^ "," ^ counts ^ "){" ^ statements
                ^ "return " ^ String.concat "+" names ^ ";}F("
                ^ String.concat "," (values @ extra)
                ^ ");"
              in
              (source, expected))
            [ ("", wide); ("noreg", narrow) ])
        [
          ( [ "I8"; "U16"; "I32" ],
            [ "127"; "65535"; "2147483647" ],
            [ ("+", "c"); ("*", "d"); ("<<", "e") ],
            "I64 c,I64 d,I64 e",
            [ "2"; "2"; "1" ],
            "4295098493",
            "65405" );
          ( [ "I32"; "U16"; "I8" ],
            [ "2147483647"; "65535"; "127" ],
            [ ("+", "c"); ("*", "d"); ("<<", "e") ],
            "I64 c,I64 d,I64 e",
            [ "2"; "2"; "1" ],
            "2147614973",
            "-2147418115" );
          ( [ "U8"; "I16"; "U32"; "I8" ],
            [ "255"; "32767"; "4294967295"; "127" ],
            [ ("+", "c"); ("-", "d"); ("*", "c"); ("<<", "d") ],
            "I64 c,I64 d",
            [ "2"; "1" ],
            "8589967867",
            "4295000059" );
          ( [ "I8"; "U32"; "I16"; "U8" ],
            [ "127"; "4294967295"; "32767"; "255" ],
            [ ("+", "c"); ("-", "d"); ("*", "c"); ("<<", "d") ],
            "I64 c,I64 d",
            [ "2"; "1" ],
            "4295033467",
            "4294967419" );
          ( [ "I8"; "U16"; "I32"; "U32"; "U8" ],
            [ "127"; "65535"; "2147483647"; "4294967295"; "255" ],
            [ ("+", "c"); ("-", "c"); ("*", "c"); ("<<", "c"); ("^", "c") ],
            "I64 c",
            [ "2" ],
            "21474902389",
            "4295032949" );
          ( [ "U8"; "U32"; "I32"; "U16"; "I8" ],
            [ "255"; "4294967295"; "2147483647"; "65535"; "127" ],
            [ ("+", "c"); ("-", "c"); ("*", "c"); ("<<", "c"); ("^", "c") ],
            "I64 c",
            [ "2" ],
            "8590197109",
            "4295032949" );
          ( [ "I64"; "I8"; "U16" ],
            [ "42"; "127"; "65535" ],
            [ ("+", "c"); ("*", "d"); ("<<", "e") ],
            "I64 c,I64 d,I64 e",
            [ "2"; "2"; "1" ],
            "131368",
            "65576" );
          ( [ "I8"; "I64"; "U16" ],
            [ "127"; "42"; "65535" ],
            [ ("+", "c"); ("*", "d"); ("<<", "e") ],
            "I64 c,I64 d,I64 e",
            [ "2"; "2"; "1" ],
            "131283",
            "65491" );
          ( [ "I8"; "U16"; "I64" ],
            [ "127"; "65535"; "42" ],
            [ ("+", "c"); ("*", "d"); ("<<", "e") ],
            "I64 c,I64 d,I64 e",
            [ "2"; "2"; "1" ],
            "131283",
            "65491" );
          ( [ "I8"; "U16"; "I32"; "I64" ],
            [ "127"; "65535"; "2147483647"; "42" ],
            [ ("+", "c"); ("-", "d"); ("*", "c"); ("<<", "d") ],
            "I64 c,I64 d",
            [ "2"; "1" ],
            "4295033041",
            "65489" );
          ( [ "I64"; "I32"; "U16"; "I8" ],
            [ "42"; "2147483647"; "65535"; "127" ],
            [ ("+", "c"); ("-", "d"); ("*", "c"); ("<<", "d") ],
            "I64 c,I64 d",
            [ "2"; "1" ],
            "2147615014",
            "2147549222" );
          ( [ "I8"; "U16"; "I32"; "U32"; "I64" ],
            [ "127"; "65535"; "2147483647"; "4294967295"; "42" ],
            [ ("+", "c"); ("-", "c"); ("*", "c"); ("<<", "c"); ("^", "c") ],
            "I64 c",
            [ "2" ],
            "21474902176",
            "4295032736" );
          ( [ "I64"; "U32"; "I32"; "U16"; "I8" ],
            [ "42"; "4294967295"; "2147483647"; "65535"; "127" ],
            [ ("+", "c"); ("-", "c"); ("*", "c"); ("<<", "c"); ("^", "c") ],
            "I64 c",
            [ "2" ],
            "8590196896",
            "4295032992" );
        ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I64 x,I8 y,U16 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+(y+(z));}F(42,127,65535,2,2,1);",
          "131368" );
        ( "I64 F(I64 noreg x,I8 noreg y,U16 noreg z,I64 c,I64 d,I64 \
           e){x+=c;y*=d;z<<=e;return x+(y+(z));}F(42,127,65535,2,2,1);",
          "65576" );
        ( "I64 F(I8 x,I64 y,U16 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+(y+(z));}F(127,42,65535,2,2,1);",
          "131283" );
        ( "I64 F(I8 noreg x,I64 noreg y,U16 noreg z,I64 c,I64 d,I64 \
           e){x+=c;y*=d;z<<=e;return x+(y+(z));}F(127,42,65535,2,2,1);",
          "65491" );
        ( "I64 F(I8 x,U16 y,I64 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           x+(y+(z));}F(127,65535,42,2,2,1);",
          "131283" );
        ( "I64 F(I8 noreg x,U16 noreg y,I64 noreg z,I64 c,I64 d,I64 \
           e){x+=c;y*=d;z<<=e;return x+(y+(z));}F(127,65535,42,2,2,1);",
          "65491" );
        ( "I64 F(I8 x,U16 y,I32 z,I64 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           x+(y+(z+(w)));}F(127,65535,2147483647,42,2,1);",
          "4295033041" );
        ( "I64 F(I8 noreg x,U16 noreg y,I32 noreg z,I64 noreg w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           x+(y+(z+(w)));}F(127,65535,2147483647,42,2,1);",
          "65489" );
        ( "I64 F(I64 x,I32 y,U16 z,I8 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           x+(y+(z+(w)));}F(42,2147483647,65535,127,2,1);",
          "2147615014" );
        ( "I64 F(I64 noreg x,I32 noreg y,U16 noreg z,I8 noreg w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           x+(y+(z+(w)));}F(42,2147483647,65535,127,2,1);",
          "2147549222" );
        ( "I64 F(I8 x,U16 y,I32 z,U32 w,I64 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           x+(y+(z+(w+(v))));}F(127,65535,2147483647,4294967295,42,2);",
          "21474902176" );
        ( "I64 F(I8 noreg x,U16 noreg y,I32 noreg z,U32 noreg w,I64 noreg \
           v,I64 c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           x+(y+(z+(w+(v))));}F(127,65535,2147483647,4294967295,42,2);",
          "4295032736" );
        ( "I64 F(I64 x,U32 y,I32 z,U16 w,I8 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           x+(y+(z+(w+(v))));}F(42,4294967295,2147483647,65535,127,2);",
          "8590196896" );
        ( "I64 F(I64 noreg x,U32 noreg y,I32 noreg z,U16 noreg w,I8 noreg \
           v,I64 c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           x+(y+(z+(w+(v))));}F(42,4294967295,2147483647,65535,127,2);",
          "4295032992" );
      ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(I64 x,I8 y,U16 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           z+(y+(x));}F(42,127,65535,2,2,1);",
          "131368" );
        ( "I64 F(I64 noreg x,I8 noreg y,U16 noreg z,I64 c,I64 d,I64 \
           e){x+=c;y*=d;z<<=e;return z+(y+(x));}F(42,127,65535,2,2,1);",
          "65576" );
        ( "I64 F(I8 x,I64 y,U16 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           z+(y+(x));}F(127,42,65535,2,2,1);",
          "131283" );
        ( "I64 F(I8 noreg x,I64 noreg y,U16 noreg z,I64 c,I64 d,I64 \
           e){x+=c;y*=d;z<<=e;return z+(y+(x));}F(127,42,65535,2,2,1);",
          "65491" );
        ( "I64 F(I8 x,U16 y,I64 z,I64 c,I64 d,I64 e){x+=c;y*=d;z<<=e;return \
           z+(y+(x));}F(127,65535,42,2,2,1);",
          "131283" );
        ( "I64 F(I8 noreg x,U16 noreg y,I64 noreg z,I64 c,I64 d,I64 \
           e){x+=c;y*=d;z<<=e;return z+(y+(x));}F(127,65535,42,2,2,1);",
          "65491" );
        ( "I64 F(I8 x,U16 y,I32 z,I64 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           w+(z+(y+(x)));}F(127,65535,2147483647,42,2,1);",
          "4295033041" );
        ( "I64 F(I8 noreg x,U16 noreg y,I32 noreg z,I64 noreg w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           w+(z+(y+(x)));}F(127,65535,2147483647,42,2,1);",
          "65489" );
        ( "I64 F(I64 x,I32 y,U16 z,I8 w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           w+(z+(y+(x)));}F(42,2147483647,65535,127,2,1);",
          "2147615014" );
        ( "I64 F(I64 noreg x,I32 noreg y,U16 noreg z,I8 noreg w,I64 c,I64 \
           d){x+=c;y-=d;z*=c;w<<=d;return \
           w+(z+(y+(x)));}F(42,2147483647,65535,127,2,1);",
          "2147549222" );
        ( "I64 F(I8 x,U16 y,I32 z,U32 w,I64 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           v+(w+(z+(y+(x))));}F(127,65535,2147483647,4294967295,42,2);",
          "21474902176" );
        ( "I64 F(I8 noreg x,U16 noreg y,I32 noreg z,U32 noreg w,I64 noreg \
           v,I64 c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           v+(w+(z+(y+(x))));}F(127,65535,2147483647,4294967295,42,2);",
          "4295032736" );
        ( "I64 F(I64 x,U32 y,I32 z,U16 w,I8 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           v+(w+(z+(y+(x))));}F(42,4294967295,2147483647,65535,127,2);",
          "8590196896" );
        ( "I64 F(I64 noreg x,U32 noreg y,I32 noreg z,U16 noreg w,I8 noreg \
           v,I64 c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           v+(w+(z+(y+(x))));}F(42,4294967295,2147483647,65535,127,2);",
          "4295032992" );
      ]
  in
  let cases =
    cases
    @ [
        ( "I64 F(U64 x,U8 y,I8 z,I64 c,I64 d,I64 e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(0xFFFFFFFFFFFFFFFF,255,127,1,1,2);",
          "-9223372036854775299" );
        ( "I64 F(U64 noreg x,U8 noreg y,I8 noreg z,I64 c,I64 d,I64 \
           e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(0xFFFFFFFFFFFFFFFF,255,127,1,1,2);",
          "9223372036854775805" );
        ( "I64 F(U8 x,U64 y,I8 z,I64 c,I64 d,I64 e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(255,0xFFFFFFFFFFFFFFFF,127,1,1,2);",
          "381" );
        ( "I64 F(U8 noreg x,U64 noreg y,I8 noreg z,I64 c,I64 d,I64 \
           e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(255,0xFFFFFFFFFFFFFFFF,127,1,1,2);",
          "125" );
        ( "I64 F(I8 x,U8 y,U64 z,I64 c,I64 d,I64 e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(127,255,0xFFFFFFFFFFFFFFFF,1,1,2);",
          "317" );
        ( "I64 F(I8 noreg x,U8 noreg y,U64 noreg z,I64 c,I64 d,I64 \
           e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(127,255,0xFFFFFFFFFFFFFFFF,1,1,2);",
          "61" );
        ( "I64 F(I64 x,U8 y,I8 z,I64 c,I64 d,I64 e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(0x8000000000000000,255,127,1,1,2);",
          "-4611686018427387394" );
        ( "I64 F(I64 noreg x,U8 noreg y,I8 noreg z,I64 c,I64 d,I64 \
           e){x>>=c;y+=d;z*=e;return \
           x+y+z;}F(0x8000000000000000,255,127,1,1,2);",
          "-4611686018427387906" );
        ( "I64 F(I8 x,U16 y,I32 z,U32 w,U64 v,I64 \
           c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           x+y+z+w+v;}F(127,65535,2147483647,4294967295,0xFFFFFFFFFFFFFFFF,2);",
          "21474902133" );
        ( "I64 F(I8 noreg x,U16 noreg y,I32 noreg z,U32 noreg w,U64 noreg \
           v,I64 c){x+=c;y-=c;z*=c;w<<=c;v^=c;return \
           x+y+z+w+v;}F(127,65535,2147483647,4294967295,0xFFFFFFFFFFFFFFFF,2);",
          "4295032693" );
      ]
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
  let literal_faults =
    [
      ("I64 F(I64 c){I8 x=1;x/=c;return x;}F(0);", "HCIRVM0009");
      ("I64 F(I64 c){I8 x=1;x%=c;return x;}F(0);", "HCIRVM0009");
      ( "I64 F(I64 c){I8 x=0x8000000000000000;x/=c;return x;}F(-1);",
        "HCIRVM0010" );
      ( "I64 F(I64 c){I8 x=0x8000000000000000;x%=c;return x;}F(-1);",
        "HCIRVM0010" );
    ]
  in
  let faults =
    [
      ("I64 F(I8 x,I8 y,I64 c){x/=c;y/=c;return x+y;}F(1,1,0);", "HCIRVM0009");
      ( "I64 F(U8 x,U8 y,U8 z,I64 c){x/=c;y/=c;z/=c;return x+y+z;}F(1,1,1,0);",
        "HCIRVM0009" );
      ("I64 F(I64 c,I8 n){I8 x=c;x/=n;return x;}F(1,0);", "HCIRVM0009");
      ("I64 F(I64 c,I8 n){I8 x=c;x%=n;return x;}F(1,0);", "HCIRVM0009");
      ( "I64 F(I64 c,I8 n){I8 x=c;x/=n;return x;}F(0x8000000000000000,-1);",
        "HCIRVM0010" );
      ( "I64 F(I64 c,I8 n){I8 x=c;x%=n;return x;}F(0x8000000000000000,-1);",
        "HCIRVM0010" );
    ]
    @ literal_faults
  in
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          List.iter
            (fun (source, code) -> run ~fault:true source code target mode)
            faults)
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
  if native then
    List.iter
      (fun mode ->
        List.iter
          (fun target ->
            run ~fault:true
              "I64 F(I8 reg RAX x,I64 c){x<<=c;return x;}F(127,1);"
              (if target = "host-jit" || mode = "aot" then "HCRUN0001"
               else "HCBACK0002")
              target mode;
            run ~fault:true
              "I64 F(I8 reg R12 x,I64 reg R12 c){x<<=c;return x;}F(127,1);"
              "HCBACK0002" target mode)
          [ "host-jit"; "host-jit-task" ])
      [ "jit"; "aot" ];
  Printf.printf "Register execution checks passed (%d reports).\n"
    ((List.length targets * 2 * (List.length cases + List.length faults))
    + if native then 4 else 0)
