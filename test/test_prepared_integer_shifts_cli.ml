open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let with_file suffix contents action =
  let path = Filename.temp_file "holyc prepared shift " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let invoke ?(target = "ir") ?(steps = 100_000) ?(preparation = 100_000) ~status
    compiler mode source =
  with_file ".hc" source (fun path ->
      with_file ".stdout" "" (fun stdout ->
          with_file ".stderr" "" (fun stderr ->
              let out_fd =
                Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              and err_fd =
                Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
              in
              let arguments =
                [|
                  compiler;
                  "run";
                  "--format=json";
                  "--report-version=2";
                  "--mode=" ^ mode;
                  "--target=" ^ target;
                  "--step-limit=" ^ string_of_int steps;
                  "--initializer-step-limit=" ^ string_of_int preparation;
                  path;
                |]
              in
              let pid =
                Fun.protect
                  ~finally:(fun () ->
                    Unix.close out_fd;
                    Unix.close err_fd)
                  (fun () ->
                    Unix.create_process compiler arguments Unix.stdin out_fd
                      err_fd)
              in
              let _, result = Unix.waitpid [] pid in
              let read path =
                let channel = open_in_bin path in
                Fun.protect
                  ~finally:(fun () -> close_in channel)
                  (fun () ->
                    really_input_string channel (in_channel_length channel))
              in
              let text = read stdout in
              require
                (result = Unix.WEXITED status)
                ("unexpected exit: " ^ text ^ read stderr);
              require (read stderr = "") "JSON CLI wrote to stderr";
              Yojson.Safe.from_string text)))

let error report code =
  require (member "outcome" report = `String "error") "expected error outcome";
  require (member "final_value" report = `Null) "fault exposes a final value";
  match
    member "diagnostics" report
    |> to_list
    |> List.find_opt (fun diagnostic ->
        member "severity" diagnostic = `String "error")
  with
  | Some first -> require (member "code" first = `String code) "fault code"
  | None -> failwith "fault has no error diagnostic"

let () =
  require (Array.length Sys.argv = 4) "expected compiler, fixture and example";
  let compiler = Sys.argv.(1)
  and fixture = Yojson.Safe.from_file Sys.argv.(2) in
  require
    (member "id" fixture = `String "arithmetic/prepared-integer-shifts-001")
    "wrong native fixture";
  let projections = member "hosted_value_projections" fixture |> to_list in
  require (List.length projections = 19) "all native fields required";
  let observed projection field =
    member "checks" fixture |> to_list
    |> List.find (fun check -> member "id" check = member field projection)
    |> member "observed_fields"
    |> member (member "field" projection |> to_string)
    |> to_string
  in
  let channel = open_in_bin Sys.argv.(3) in
  let example =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  List.iter
    (fun mode ->
      List.iter
        (fun projection ->
          let bits = observed projection "case_id" in
          require
            (bits = observed projection "repeat_case_id")
            "native repeat differs";
          let report =
            invoke ~status:0 compiler mode
              (member "holy_c_source" projection |> to_string)
          in
          require
            (member "outcome" report = `String "success")
            "preparation outcome";
          require
            (member "final_value" report
            |> member "bits"
            = `String ("0x" ^ String.lowercase_ascii bits))
            "saved native bits";
          require
            (member "output_byte_length" report = `Int 0)
            "unexpected replay output")
        projections;
      let report =
        invoke ~steps:202 ~preparation:7 ~status:0 compiler mode example
      in
      require
        (member "final_value" report |> member "value" = `String "42")
        "maintained example value";
      require (member "executed_steps" report = `Int 202) "runtime work";
      require
        (member "compiled_initializer_steps" report = `Int 7)
        "preparation work";
      require
        (member "output_hex" report = `String "")
        "once-only source assertions";
      require (member "output_work" report = `Int 7) "formatting work";
      error (invoke ~steps:201 ~status:1 compiler mode example) "HCIRVM0007";
      error (invoke ~preparation:6 ~status:1 compiler mode example) "HCIRVM0007";
      error
        (invoke ~target:"host-jit" ~status:1 compiler mode example)
        "HCPP0008";
      let sources =
        [
          ( "#exe {I64 XR=7,CR=65,NR=0;I64 FR(I64 x){NR++;return x<<CR;}I64 \
             IR(){return FR(XR);}I64 SR(I64 n=IR()){return \
             n;}StreamPrint(\"%d;\",SR()+(NR!=1)*1000);}",
            "14" );
          ( "#exe {I64 XR=7,CR=65,NR=0;I64 FR(I64 x,I64 c){NR++;x<<=c;return \
             x;}I64 IR(){return FR(XR,CR);}I64 SR(I64 n=IR()){return \
             n;}StreamPrint(\"%d;\",SR()+(NR!=1)*1000);}",
            "14" );
          ( "#exe {I64 XR=-8,CR=65,NR=0;I64 FR(I64 x,I64 c){NR++;x>>=c;return \
             x;}I64 IR(){return FR(XR,CR);}I64 SR(I64 n=IR()){return \
             n;}StreamPrint(\"%d;\",SR()+(NR!=1)*1000);}",
            "-4" );
          ( "#exe {I64 NR=0;I64 FR(I8 x,I64 c){NR++;x>>=c;return x;}I64 \
             IR(){return FR(-7,65);}I64 SR(I64 n=IR()){return \
             n;}StreamPrint(\"%d;\",SR()+(NR!=1)*1000);}",
            "-4" );
          ( "#exe {I64 FR(U8 x,I64 c){x>>=c;return x;}I64 IR(){return \
             FR(255,65);}I64 SR(I64 n=IR()){return \
             n;}StreamPrint(\"%d;\",SR());}",
            "127" );
          ( "#exe {I64 F(I8 x,I8 c){x<<=c;return x;}I64 I(){return \
             F(127,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(U8 x,I8 c){x<<=c;return x;}I64 I(){return \
             F(255,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(I16 x,I8 c){x<<=c;return x;}I64 I(){return \
             F(32767,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "65534" );
          ( "#exe {I64 F(U16 x,I8 c){x<<=c;return x;}I64 I(){return \
             F(65535,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "131070" );
          ( "#exe {I64 F(I32 x,I8 c){x<<=c;return x;}I64 I(){return \
             F(2147483647,1);}I64 S(I64 n=I()){return \
             n;}StreamPrint(\"%d;\",S());}",
            "4294967294" );
          ( "#exe {I64 F(U32 x,I8 c){x<<=c;return x;}I64 I(){return \
             F(4294967295,1);}I64 S(I64 n=I()){return \
             n;}StreamPrint(\"%d;\",S());}",
            "8589934590" );
          ( "#exe {I64 F(I8 x){x++;return x;}I64 I(){return F(127);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "128" );
          ( "#exe {I64 F(I8 x){--x;return x;}I64 I(){return F(-128);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "-129" );
          ( "#exe {I64 F(){I8 x=255;++x;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(){I8 x=255;x--;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(I8 x){return ++x;}I64 I(){return F(127);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "128" );
          ( "#exe {I64 F(I8 x){return --x;}I64 I(){return F(-128);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "-129" );
          ( "#exe {I64 F(){I8 x=255;return ++x;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(){I8 x=255;return --x;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(I8 x){return x++;}I64 I(){return F(127);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(I8 x){return x--;}I64 I(){return F(-128);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "-128" );
          ( "#exe {I64 F(){I8 x=255;return x++;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(){I8 x=255;return x--;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(){I8 x=255;return x+=1;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(){I8 x=255;return x-=1;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(){I8 x=255;return x*=1;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(){I8 x=255;return x<<=1;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(){I8 x=255;return x>>=1;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(I8 x){return x+=1;}I64 I(){return F(127);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "128" );
          ( "#exe {I64 F(I8 x){return x-=1;}I64 I(){return F(-128);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "-129" );
          ( "#exe {I64 F(U8 x){return x*=2;}I64 I(){return F(255);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(U8 x){return x<<=1;}I64 I(){return F(255);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(I8 x){return x>>=1;}I64 I(){return F(127);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "63" );
          ( "#exe {U8 G=255;I64 ID(I64 x){return x;}I64 F(){return \
             ID(G+=1);}StreamPrint(\"%d;\",F());}",
            "0" );
          ( "#exe {U8 G=0;I64 ID(I64 x){return x;}I64 F(){return \
             ID(G-=1);}StreamPrint(\"%d;\",F());}",
            "255" );
          ( "#exe {U8 G=255;I64 F(){return (G+=1)-1;}StreamPrint(\"%d;\",F());}",
            "-1" );
          ( "#exe {U8 G=0;I64 F(){return (G-=1)-1;}StreamPrint(\"%d;\",F());}",
            "254" );
          ( "#exe {U8 G=255;I64 N=7;I64 F(){return \
             (G+=1)-N;}StreamPrint(\"%d;\",F());}",
            "-7" );
          ( "#exe {U8 G=0;I64 N=7;I64 F(){return \
             (G-=1)-N;}StreamPrint(\"%d;\",F());}",
            "248" );
          ( "#exe {U8 G=255;I64 F(){return (G+=1)*2;}StreamPrint(\"%d;\",F());}",
            "0" );
          ( "#exe {U8 G=0;I64 F(){return (G-=1)*2;}StreamPrint(\"%d;\",F());}",
            "510" );
          ( "#exe {U8 G=255;I64 N=7;I64 F(){return \
             (G+=1)*N;}StreamPrint(\"%d;\",F());}",
            "0" );
          ( "#exe {U8 G=0;I64 N=7;I64 F(){return \
             (G-=1)*N;}StreamPrint(\"%d;\",F());}",
            "1785" );
          ( "#exe {I64 F(I8 reg R12 x,I64 c){x<<=c;return \
             x;}StreamPrint(\"%d;\",F(127,1));}",
            "254" );
          ( "#exe {I64 F(){U8 reg R15 x=255;x<<=1;return \
             x;}StreamPrint(\"%d;\",F());}",
            "510" );
          ( "#exe {I64 F(I8 x,I64 reg R12 c){x<<=c;return \
             x;}StreamPrint(\"%d;\",F(127,1));}",
            "254" );
          ("#exe {U8 G=255;I64 F(){return G+=1;}StreamPrint(\"%d;\",F());}", "0");
          ("#exe {U8 G=0;I64 F(){return G-=1;}StreamPrint(\"%d;\",F());}", "255");
          ( "#exe {U8 G=255;I64 F(){return (G+=1)+1;}StreamPrint(\"%d;\",F());}",
            "1" );
          ( "#exe {U8 G=0;I64 F(){return (G-=1)+256;}StreamPrint(\"%d;\",F());}",
            "511" );
          ( "#exe {U8 G=255;I64 N=7;I64 F(){return \
             (G+=1)+N;}StreamPrint(\"%d;\",F());}",
            "7" );
          ( "#exe {U8 G=0;I64 N=7;I64 F(){return \
             (G-=1)+N;}StreamPrint(\"%d;\",F());}",
            "262" );
          ( "#exe {I64 F(){I8 x=255;x<<=0;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(){U8 x=256;x<<=0;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(){I8 x=127;x<<=1;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(){I8 x=255;x>>=1;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(){U8 x=256;x>>=1;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "128" );
          ( "#exe {I64 F(){I8 x=255;return x;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(){U8 x=256;return x;}I64 I(){return F();}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(I8 x,I8 c){x+=c;return x;}I64 I(){return \
             F(127,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "128" );
          ( "#exe {I64 F(U32 x,I8 c){x+=c;return x;}I64 I(){return \
             F(4294967295,1);}I64 S(I64 n=I()){return \
             n;}StreamPrint(\"%d;\",S());}",
            "4294967296" );
          ( "#exe {I64 F(){I8 x=255;x+=1;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(){U8 x=256;x+=1;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "257" );
          ( "#exe {I64 F(I8 x,I8 c){x-=c;return x;}I64 I(){return \
             F(-128,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "-129" );
          ( "#exe {I64 F(U8 x,I8 c){x-=c;return x;}I64 I(){return F(0,1);}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "-1" );
          ( "#exe {I64 F(){I8 x=255;x-=1;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(I8 x,I8 c){x*=c;return x;}I64 I(){return \
             F(127,2);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(U8 x,I8 c){x*=c;return x;}I64 I(){return \
             F(255,2);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(){I8 x=255;x*=2;return x;}I64 I(){return F();}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(){I8 x=255;x*=0x8000000000000000;return x;}I64 \
             I(){return F();}I64 S(I64 n=I()){return \
             n;}StreamPrint(\"%d;\",S());}",
            "-9223372036854775808" );
          ( "#exe {I64 F(I64 c){I8 x=255;return x;}I64 I(){return F(0);}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(I64 c){I8 x=255;x/=c;return x;}I64 I(){return \
             F(2);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(I64 c){I8 x=255;x%=c;return x;}I64 I(){return \
             F(2);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "1" );
          ( "#exe {I64 F(I64 c){I8 x=255;x<<=c;return x;}I64 I(){return \
             F(1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(I64 c){I8 x=255;x&=c;return x;}I64 I(){return \
             F(255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(I64 c){I8 x=255;x-=c;return x;}I64 I(){return \
             F(1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(I64 c){I8 x=255;x*=c;return x;}I64 I(){return \
             F(3);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "765" );
          ( "#exe {I64 F(I64 c){I8 x=255;x|=c;return x;}I64 I(){return \
             F(256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "511" );
          ( "#exe {I64 F(I64 c){I8 x=255;x^=c;return x;}I64 I(){return \
             F(256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "511" );
          ( "#exe {I64 F(I64 c){I8 x=255;x+=c;return x;}I64 I(){return \
             F(255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "510" );
          ( "#exe {I64 F(U8 c){I8 x=c;return x;}I64 I(){return F(255);}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(U8 c,I8 n){I8 x=c;x*=n;return x;}I64 I(){return \
             F(255,3);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "765" );
          ( "#exe {I64 F(I64 c,I8 n){I8 x=c;x/=n;return x;}I64 I(){return \
             F(255,2);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(I64 c,I8 n){I8 x=c;x%=n;return x;}I64 I(){return \
             F(255,2);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "1" );
          ( "#exe {I64 F(I64 c,I64 n){I8 x=c;x&=n;return x;}I64 I(){return \
             F(-1,255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(I64 c,I64 n){I8 x=c;x|=n;return x;}I64 I(){return \
             F(255,256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "511" );
          ( "#exe {I64 F(I64 c,I64 n){I8 x=c;x^=n;return x;}I64 I(){return \
             F(255,256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "511" );
          ( "#exe {I64 F(I64 c,I8 n){I8 x=c;x-=n;return x;}I64 I(){return \
             F(255,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(I64 c,I8 n){I8 x=c;x>>=n;return x;}I64 I(){return \
             F(255,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(I64 c,I8 n){I8 x=c;x*=n;return x;}I64 I(){return \
             F(255,3);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "765" );
          ( "#exe {I64 F(I64 c,I64 n){I8 x=c;x+=n;return x;}I64 I(){return \
             F(255,256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "511" );
          ( "#exe {I64 F(I64 c,I64 n){U8 x=c;x<<=n;return x;}I64 I(){return \
             F(256,256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(I64 c,I8 n){I8 x=c;x+=n;return x;}I64 I(){return \
             F(255,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(I64 c,I8 n){U8 x=c;x<<=n;return x;}I64 I(){return \
             F(256,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "512" );
          ( "#exe {I64 F(I64 c){I8 x=c;x-=1;return x;}I64 I(){return \
             F(255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "254" );
          ( "#exe {I64 F(I64 c){I8 x=c;x>>=1;return x;}I64 I(){return \
             F(255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(I64 c){I8 x=c;x*=3;return x;}I64 I(){return \
             F(255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "765" );
          ( "#exe {I64 F(I64 c){I8 x=c;x+=1;return x;}I64 I(){return \
             F(255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(I64 c){U8 x=c;x<<=1;return x;}I64 I(){return \
             F(256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "512" );
          ( "#exe {I64 F(I64 c){I8 x=c;return x;}I64 I(){return F(255);}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(I64 c){U8 x=c;return x;}I64 I(){return F(256);}I64 \
             S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "256" );
          ( "#exe {I64 F(I8 x,I8 c){x/=c;return x;}I64 I(){return \
             F(-128,-1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "128" );
          ( "#exe {I64 F(U8 x,I8 c){x/=c;return x;}I64 I(){return \
             F(255,2);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "127" );
          ( "#exe {I64 F(I8 x,I64 c){x|=c;return x;}I64 I(){return \
             F(127,256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "383" );
          ( "#exe {I64 F(I8 x,I64 c){x^=c;return x;}I64 I(){return \
             F(127,256);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "383" );
          ( "#exe {I64 F(I8 x,I64 c){x&=c;return x;}I64 I(){return \
             F(-1,255);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "255" );
          ( "#exe {I64 F(I8 x,I8 c){I8 *p=&x;*p<<=c;return x;}I64 I(){return \
             F(127,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}",
            "-2" );
          ( "#exe {I64 NR=0;I64 F(I8 x,I64 c){I8 *p=&x;NR++;I64 \
             a=(*p<<=c);return a*1000+x;}I64 I(){return F(127,65);}I64 S(I64 \
             n=I()){return n;}StreamPrint(\"%d;\",S()+(NR!=1)*1000000);}",
            "253998" );
        ]
      in
      List.iter
        (fun (variable, expected) ->
          List.iter
            (fun target ->
              let baseline = invoke ~target ~status:0 compiler mode variable in
              let steps = member "executed_steps" baseline |> to_int in
              let preparation =
                member "compiled_initializer_steps" baseline |> to_int
              in
              require
                (member "final_value" baseline
                |> member "value" = `String expected)
                "retained variable shift result and single call";
              let exact =
                if preparation = 0 then
                  invoke ~target ~steps ~status:0 compiler mode variable
                else
                  invoke ~target ~steps ~preparation ~status:0 compiler mode
                    variable
              in
              require
                (member "outcome" exact = `String "success")
                "exact retained limits";
              error
                (invoke ~target ~steps:(steps - 1) ~status:1 compiler mode
                   variable)
                "HCIRVM0007";
              if preparation > 1 then
                error
                  (invoke ~target ~preparation:(preparation - 1) ~status:1
                     compiler mode variable)
                  "HCIRVM0007")
            [ "ir"; "host-jit-task" ])
        sources;
      error
        (invoke ~target:"ir" ~status:1 compiler mode
           "#exe {I64 F(I8 x,I8 c){x<<=c;c++;return x;}I64 I(){return \
            F(127,1);}I64 S(I64 n=I()){return n;}StreamPrint(\"%d;\",S());}")
        "HCRUN0006";
      let fault = member "hosted_fault_projection" fixture in
      error
        (invoke ~status:1 compiler mode
           (member "holy_c_source" fault |> to_string))
        (member "hosted_code" fault |> to_string))
    [ "jit"; "aot" ];
  print_endline
    "38 prepared source projections and retained CLI work/fault boundaries \
     passed."
