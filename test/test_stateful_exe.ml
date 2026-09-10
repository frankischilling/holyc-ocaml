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
