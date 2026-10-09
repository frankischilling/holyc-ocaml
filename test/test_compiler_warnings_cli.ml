open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let temporary suffix contents action =
  let path = Filename.temp_file "holyc-compiler-warnings-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel contents;
      close_out channel;
      action path)

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native_only = Array.exists (( = ) "--native") Sys.argv
let targets = if native_only then [ "host-jit-task" ] else [ "ir" ]
let count = ref 0

let invoke ?(status = 0) ?(options = []) mode target path =
  temporary ".out" "" (fun output ->
      temporary ".err" "" (fun error ->
          let out_fd =
            Unix.openfile output [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile error [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let args =
            [
              compiler;
              "run";
              "--format=json";
              "--mode=" ^ mode;
              "--target=" ^ target;
              "--code-byte-limit=1048576";
            ]
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
          let text = read output in
          require (reached = Unix.WEXITED status) (text ^ read error);
          require (read error = "") "warning JSON wrote stderr";
          Yojson.Safe.from_string text))

let warnings report =
  report |> member "diagnostics" |> to_list
  |> List.filter (fun diagnostic ->
      diagnostic |> member "severity" |> to_string = "warning")
  |> List.map (fun diagnostic ->
      ( diagnostic |> member "code" |> to_string,
        diagnostic |> member "message" |> to_string ))

let expected =
  [
    ("HCSEMA0034", "unused variable \"unused\" in function \"Loud\"");
    ("HCSEMA0035", "unneeded no_warn for \"used\" in function \"Suppression\"");
    ("HCSEMA0034", "unused variable \"unused\" in function \"Child\"");
  ]

let header_warnings =
  [
    ( "HCSEMA0037",
      "function \"F\" return type does not match the replaced header" );
    ( "HCSEMA0038",
      "function \"F\" argument list does not match the replaced header" );
  ]

let unused_extern = ("HCSEMA0075", "Unused extern 'F'")

let duplicate name function_name =
  ( "HCSEMA0076",
    Printf.sprintf "duplicate local-variable type for %S in function %S" name
      function_name )

let duplicate_cases =
  [
    ( "default disabled",
      "#exe {Option(16,0);I64 F(){I64 a;I64 b;return 42;}}42;",
      [],
      0 );
    ( "first comma declarator",
      "#exe {Option(16,0);Option(18,1);I64 F(){I64 a,b;I64 c,d;return 42;}}42;",
      [ duplicate "c" "F" ],
      0 );
    ( "automatic mode only",
      "#exe {Option(16,0);Option(18,1);I64 F(I64 parameter){static I64 s;I64 \
       a;I64 b;I64 c;static I64 t;return parameter;}F(42);}42;",
      [ duplicate "b" "F"; duplicate "c" "F" ],
      0 );
    ( "exact primitive class",
      "#exe {Option(16,0);Option(18,1);I64 F(){I64 a;U64 b;I64i c;Bool d;I8 \
       e;I64 *f;I64i *g;Bool h;I8 i;return 42;}}42;",
      [
        duplicate "f" "F";
        duplicate "g" "F";
        duplicate "h" "F";
        duplicate "i" "F";
      ],
      0 );
    ( "callback intrinsic class",
      "#exe {Option(16,0);Option(18,1);I64 F(){I64 a;I64 (*first)(I64 n);U8 \
       (*second)(U8 n);I64i third;I64 *fourth;return 42;}}42;",
      [ duplicate "second" "F"; duplicate "third" "F"; duplicate "fourth" "F" ],
      0 );
    ( "ordinary source warning",
      "#exe {Option(16,0);Option(18,1);}I64 F(){I64 a=40;I64 b=2;return \
       a+b;}F();",
      [ duplicate "b" "F" ],
      0 );
    ( "dimension enables warning",
      "#exe {Option(16,0);Option(18,0);I64 F(){I64 a[Option(18,1)+1];I64 \
       b;return 42;}}42;",
      [ duplicate "b" "F" ],
      0 );
    ( "warning before initializer error",
      "#exe {Option(16,0);Option(18,1);I64 F(){I64 a;I64 b=;}}42;",
      [ duplicate "b" "F" ],
      1 );
    ( "warning before body error",
      "#exe {Option(16,0);Option(18,1);I64 F(){I64 a;I64 b;return missing;}}42;",
      [ duplicate "b" "F" ],
      1 );
    ( "ordinary child index and options",
      {|#exe {Option(16,0);Option(18,1);StreamExePrint("I64 Child(){I64 a;I64 b;return 42;}Child();");I64 Parent(){I64 a;I64 b;return 42;}Parent();}42;|},
      [ duplicate "b" "Child"; duplicate "b" "Parent" ],
      0 );
    ( "member collision precedes type warning",
      "#exe {Option(16,0);Option(18,1);I64 F(I64 a){I64 a;I64 b;return 42;}}42;",
      [],
      1 );
    ( "reentrant actual type index",
      "#exe {Option(16,0);Option(18,1);Option(19,0);I64 F(){I64 a;#exe {extern \
       I64 F();}I64 b;I64 c;return 42;}}42;",
      [ unused_extern; duplicate "c" "F" ],
      0 );
  ]

let header_cases =
  [
    ( "same evaluated callback owner",
      "I64 A(I64 n){return n;}extern I64 F(I64 (*cb)(I64 n)=&A);I64 F(I64 \
       (*cb)(I64 n)=&A){return cb(42);}F();",
      [ unused_extern ],
      0 );
    ( "different evaluated callback owners",
      "I64 A(I64 n){return n;}I64 B(I64 n){return n;}extern I64 F(I64 \
       (*cb)(I64 n)=&A);I64 F(I64 (*cb)(I64 n)=&B){return cb(42);}F();",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "same actual data address",
      "I64 A[2]={42,17};extern I64 F(I64 *p=&A[0]);I64 F(I64 *p=A){return \
       *p;}F();",
      [ unused_extern ],
      0 );
    ( "different actual data offsets",
      "I64 A[2]={42,17};extern I64 F(I64 *p=&A[0]);I64 F(I64 *p=&A[1]){return \
       *p+25;}F();",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "different data objects with equal contents",
      "I64 A=42;I64 B=42;extern I64 F(I64 *p=&A);I64 F(I64 *p=&B){return \
       *p;}F();",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "copied string bytes match",
      "extern I64 F(U8 *s=\"ABC\");I64 F(U8 *s=\"ABC\"){return s[0]-23;}F();",
      [ unused_extern ],
      0 );
    ( "copied string bytes differ",
      "extern I64 F(U8 *s=\"ABC\");I64 F(U8 *s=\"AXC\"){return s[0]-23;}F();",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "copied strings stop at NUL",
      "extern I64 F(U8 *s=\"AB\\0C\");I64 F(U8 *s=\"AB\\0D\"){return \
       s[0]-23;}F();",
      [ unused_extern ],
      0 );
    ( "string flag differs from ordinary data word",
      "U8 A[2]={65,0};extern I64 F(U8 *s=\"A\");I64 F(U8 *s=A){return \
       s[0]-23;}F();",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "matching classes",
      "extern I64 F(I64 n);I64 F(I64 n){return n;}42;",
      [ unused_extern ],
      0 );
    ( "return and member classes",
      "extern I64 F(I64 n);U8 F(U8 n){return n;}42;",
      unused_extern :: header_warnings,
      0 );
    ( "member name",
      "extern I64 F(I64 before);I64 F(I64 after){return after;}42;",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "saved full default words",
      "extern I64 F(U8 n=554);I64 F(U8 n=42){return n;}F();",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "once-only default effects",
      "I64 N=40;extern I64 F(I64 n=++N);I64 F(I64 n=++N-1){return \
       n;}(F()==41&&N==42)*42;",
      [ unused_extern ],
      0 );
    ( "changing evaluated defaults",
      "I64 N=40;extern I64 F(I64 n=++N);I64 F(I64 n=++N){return n;}N=0;F()+N;",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "lastclass zero",
      "extern I64 F(I64 n=lastclass);I64 F(I64 n=0){return n;}42;",
      [ unused_extern ],
      0 );
    ( "empty saved list",
      "extern I64 F();extern I64 F(...);42;",
      [ unused_extern; List.nth header_warnings 1 ],
      0 );
    ( "saved variadic members",
      "extern I64 F(I64 n,...);extern I64 F(I64 n,I64 extra);42;",
      [ unused_extern ],
      0 );
    ( "definition ends JIT reuse",
      "I64 F(I64 n){return n;}U8 F(U8 n){return n;}42;",
      [],
      0 );
    ( "warnings precede failed body",
      "extern I64 F(I64 n);U8 F(U8 n){return missing;}42;",
      unused_extern :: header_warnings,
      1 );
    ( "header mask precedes body option",
      "extern U8 Option(I64 num,U8 val);extern I64 F(I64 n);U8 F(U8 n){#exe \
       {Option(19,0);}return n;}42;",
      unused_extern :: header_warnings,
      0 );
    ( "header mask follows default option",
      "extern U8 Option(I64 num,U8 val);extern I64 F(I64 n=42);U8 F(U8 \
       m=Option(19,0)+41){return m;}F();",
      [ unused_extern ],
      0 );
  ]

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let report = invoke mode target example in
          require
            (report |> member "outcome" |> to_string = "success")
            "warning example failed";
          require
            (report |> member "final_value" |> member "value" |> to_string
           = "42")
            "warning example result changed";
          require
            (report |> member "output_hex" |> to_string
           = "34323b34323b34323b34323b")
            "warnings changed program output";
          require
            (warnings report = expected)
            "reached warning order or option mask changed";
          (if native_only then
             let fragments =
               report |> member "native" |> member "fragments" |> to_list
             in
             require
               (List.length fragments > 8
               && List.for_all
                    (fun fragment ->
                      fragment |> member "outcome" |> to_string = "success")
                    fragments
               && List.exists
                    (fun fragment ->
                      fragment |> member "function_count" |> to_int > 0)
                    fragments)
               "warning fixture lacks completed native function execution");
          let steps = report |> member "executed_steps" |> to_int in
          let exact =
            invoke
              ~options:[ "--step-limit=" ^ string_of_int steps ]
              mode target example
          in
          require (warnings exact = expected) "exact budget changed warnings";
          let below =
            invoke ~status:1
              ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
              mode target example
          in
          require
            (warnings below = expected)
            "later budget fault lost reached warnings";
          require
            (below |> member "executed_steps" |> to_int = steps - 1
            && below |> member "diagnostics" |> to_list
               |> List.exists (fun diagnostic ->
                   diagnostic |> member "code" |> to_string = "HCIRVM0007"))
            "one-below run did not exhaust the actual instruction budget";
          temporary ".hc" "#exe {I64 Broken(I64 unused){return missing;}}42;"
            (fun path ->
              require
                (warnings (invoke ~status:1 mode target path) = [])
                "failed body emitted unused warning");
          temporary ".hc"
            "#exe {I64 F(I64 used){used;return 42;}Print(\"%d;\",F(0));}"
            (fun ordinary ->
              temporary ".hc"
                "#exe {I64 F(I64 used){no_warn used;used;return \
                 42;}Print(\"%d;\",F(0));}" (fun suppressed ->
                  let ordinary = invoke mode target ordinary
                  and suppressed = invoke mode target suppressed in
                  require
                    (ordinary |> member "executed_steps"
                     = (suppressed |> member "executed_steps")
                    && ordinary
                       |> member "compiled_initializer_steps"
                       = (suppressed |> member "compiled_initializer_steps"))
                    "no_warn emitted runtime work";
                  require
                    (warnings ordinary = [])
                    "used local warned without no_warn";
                  require
                    (warnings suppressed
                    = [
                        ( "HCSEMA0035",
                          "unneeded no_warn for \"used\" in function \"F\"" );
                      ])
                    "no_warn did not update local warning state")))
        [ "jit"; "aot" ])
    targets;
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let path =
            Filename.concat (Filename.dirname example)
              "compiler-duplicate-types.hc"
          in
          let report = invoke mode target path in
          require
            (warnings report
            = [ duplicate "b" "Sum"; duplicate "second" "CallbackBase" ])
            "duplicate-type example lost exact class bases";
          require
            (report |> member "final_value" |> member "value" |> to_string
             = "42"
            && report |> member "output_hex" |> to_string = "34323b")
            "duplicate-type example changed execution";
          List.iter
            (fun (label, text, expected, status) ->
              temporary ".hc" text (fun path ->
                  let report = invoke ~status mode target path in
                  require
                    (warnings report = expected)
                    (label ^ ": expected ["
                    ^ String.concat "; " (List.map snd expected)
                    ^ "], received ["
                    ^ String.concat "; " (List.map snd (warnings report))
                    ^ "]")))
            duplicate_cases)
        [ "jit"; "aot" ])
    targets;
  List.iter
    (fun target ->
      List.iter
        (fun (label, source, expected, status) ->
          temporary ".hc" source (fun path ->
              let report = invoke ~status "jit" target path in
              require
                (warnings report = expected)
                (label ^ ": expected ["
                ^ String.concat "; " (List.map snd expected)
                ^ "], received ["
                ^ String.concat "; " (List.map snd (warnings report))
                ^ "]");
              if status = 0 then
                require
                  (report |> member "final_value" |> member "value" |> to_string
                 = "42")
                  (label ^ ": header warnings changed execution")))
        header_cases)
    targets;
  Printf.printf "%d compiler warning CLI executions passed\n%!" !count
