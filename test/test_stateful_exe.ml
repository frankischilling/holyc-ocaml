open Holyc_lib
module Output = Test_integer_output
module G = Test_integer_globals

let gates =
  [
    ("literal", {|#exe {StreamPrint("42;");}|});
    ("joined fragments", {|#exe {StreamPrint("4");StreamPrint("2;");}|});
    ( "task function",
      {|#exe {I64 Add(I64 a,I64 b){return a+b;}StreamPrint("%d;",Add(20,22));}|}
    );
    ( "persistent task cell",
      {|#exe {I64 N=40;} #exe {N+=2;StreamPrint("%d;",N);}|} );
    ( "generated function",
      {|#exe {StreamPrint("I64 Generated(){return 42;}");} Generated();|} );
    ( "generated array",
      {|#exe {StreamPrint("I64 Values[2]={40,2};");} Values[0]+Values[1];|} );
    ("task loop", {|#exe {I64 N=0;while(N<42)++N;StreamPrint("%d;",N);}|});
    ("expression fragment", {|#exe {StreamPrint("40+");} 2;|});
  ]

let expect ?(modes = G.modes) value source () =
  List.iter
    (fun mode ->
      ignore (Output.run ~mode source |> Output.expect ~value:(Some value) ""))
    modes

let tests =
  List.map
    (fun (name, source) -> Alcotest.test_case name `Quick (expect 42L source))
    gates
  @ [
      Alcotest.test_case "implicit defaults are saved in directive tasks" `Quick
        (expect 42L
           {|#exe {I64 N=40;I64 Out=0;U0 Print(U8 *s,I64 n=++N){Out=n;}N=0;"saved";U0 Saved(){"saved";}Out=0;Saved;StreamPrint("%d;",Out+N+1);}|});
      Alcotest.test_case "ordinary implicit calls use closed saved defaults"
        `Quick
        (expect 42L
           {|I64 Out=0;U0 Print(U8 *s,I64 n=40+2){Out=n;}"top";U0 Saved(){"body";}Out=0;Saved;Out;|});
      Alcotest.test_case "implicit output in directive tasks" `Quick (fun () ->
          List.iter
            (fun mode ->
              ignore
                (Output.run ~mode {|#exe {"A";'B';StreamPrint("42;");}|}
                |> Output.expect ~value:(Some 42L) "AB"))
            G.modes);
      Alcotest.test_case
        "pending implicit output resumes after directive output" `Quick
        (fun () ->
          List.iter
            (fun mode ->
              ignore
                (Output.run ~mode
                   {|extern U0 Print(U8 *fmt,...);"A";#exe {'B';StreamPrint("42;");}|}
                |> Output.expect ~value:(Some 42L) "BA"))
            G.modes);
      Alcotest.test_case
        "directive implicit calls preserve selected definitions" `Quick
        (expect 42L
           {|#exe {I64 N=0;U0 Print(U8 *s){N=42;}U0 Saved(){"old";}"old" #exe {U0 Print(U8 *s){N=7;}};I64 Before=N;"new";I64 After=N;Saved;StreamPrint("%d;",Before+N-After-35);}|});
      Alcotest.test_case "replaced directive headers save each default once"
        `Quick
        (expect 42L
           {|#exe {I64 N=38;extern I64 Joined(I64 a=++N);} #exe {extern I64 Joined(I64 b=++N);} #exe {I64 Joined(I64 value=++N){return value+1;}} #exe {StreamPrint("%d;",Joined()+Joined()-N-1);}|});
      Alcotest.test_case "separate directive commands join extern headers"
        `Quick
        (expect 42L
           {|#exe {extern I64 Joined(I64 n);} #exe {I64 Joined(I64 value){return value+2;}} #exe {StreamPrint("%d;",Joined(40));}|});
      Alcotest.test_case
        "joined directive function supplies a runtime dimension" `Quick
        (expect 42L
           {|#exe {I64 N=1;extern I64 Extent();} #exe {I64 Extent(){return ++N;}} #exe {I64 A[Extent()];A[1]=40;StreamPrint("%d;",A[1]+N);}|});
      Alcotest.test_case "cross-frame integer token" `Quick
        (expect 42L {|#exe {StreamPrint("4");}2;|});
      Alcotest.test_case "generated function body" `Quick
        (expect 42L {|I64 F(){#exe {StreamPrint("return 42;");}}F();|});
      Alcotest.test_case "generated initializer operand" `Quick
        (expect 42L {|I64 G=#exe {StreamPrint("42");};G;|});
      Alcotest.test_case "nested stream buffers" `Quick
        (expect 42L {|#exe {#exe {StreamPrint("StreamPrint(\"42;\");");}}|});
      Alcotest.test_case "outer JIT task declaration" `Quick
        (expect ~modes:[ Preprocessor.Jit ] 42L
           {|I64 N=40;#exe {StreamPrint("%d;",N+2);}|});
      Alcotest.test_case "directive precedes pending statement execution" `Quick
        (expect ~modes:[ Preprocessor.Jit ] 0L
           {|I64 N=0;N=1;#exe {StreamPrint("%d;",N);}|});
      Alcotest.test_case "outer defaults retain their original result" `Quick
        (expect ~modes:[ Preprocessor.Jit ] 42L
           {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;#exe {StreamPrint("%d;",Saved()+Saved());}|});
      Alcotest.test_case "activation during an outer initializer" `Quick
        (expect ~modes:[ Preprocessor.Jit ] 42L
           {|I64 N=40;I64 A[2]={++N,#exe {StreamPrint("%d",N+1);}};A[1];|});
      Alcotest.test_case "activation during a later outer default" `Quick
        (expect ~modes:[ Preprocessor.Jit ] 42L
           {|I64 N=40;I64 F(I64 a=++N,I64 b=#exe {StreamPrint("%d",N+1);}){return b;};F();|});
      Alcotest.test_case "generated declarations share outer task storage"
        `Quick
        (expect ~modes:[ Preprocessor.Jit ] 42L
           {|I64 N=40;#exe {StreamPrint("I64 Add(){return N+2;};");}Add();|});
      Alcotest.test_case "initializer effects precede pending outer stores"
        `Quick
        (expect ~modes:[ Preprocessor.Jit ] 41L
           {|I64 N=40;I64 A=++N;N=0;#exe {StreamPrint("%d;",A);}|});
    ]
