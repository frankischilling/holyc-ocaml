let headers = {|extern U0 Print(U8 *fmt,...);|}

let successes =
  [
    ( "directive JIT conditional mode",
      "#exe {\n\
       #ifjit\n\
       StreamPrint(\"42;\");\n\
       #else\n\
       StreamPrint(\"0;\");\n\
       #endif\n\
       }",
      "",
      3 );
    ( "surrounding function locals are hidden",
      {|I64 F(I64 value=1){#exe {I64 value=40;StreamPrint("return %d;",value+2);}}F;|},
      "",
      String.length "return 42;" );
    ("closed module", "I64 A=41;I64 B=A+1;B;", "", 0);
    ("zeroed module storage", "I64 N;N+42;", "", 0);
    ( "load-time function call",
      "I64 A=40;I64 F(I64 n=2){return A+n;}I64 B=F;B;",
      "",
      0 );
    ("generated expression", {|#exe {StreamPrint("42;");}|}, "", 3);
    ( "separate task and module tables",
      {|I64 Seed=1;#exe {I64 Seed=40;I64 Read(I64 n=2){return Seed+n;}StreamPrint("I64 Answer=%d;",Read);}#exe {Seed=41;Print("parse%d;",Read);}Print("load%d;",Seed);Answer;|},
      "parse43;load1;",
      String.length "I64 Answer=42;" );
    ( "parse output before module execution",
      {|#exe {Print("parse;");StreamPrint("Print(\"load;\");42;");}|},
      "parse;load;",
      String.length {|Print("load;");42;|} );
    ( "original provider capture",
      {|#exe {U0 (*p)(U8 *fmt,...)=&StreamPrint;p("42;");}|},
      "",
      3 );
    ( "provider saved across streams",
      {|#exe {I64 Emit(U0 (*p)(U8 *fmt,...)=&StreamPrint){p("42;");return 0;}}#exe {Emit;}|},
      "",
      3 );
    ( "task statics across streams",
      {|#exe {I64 F(){static I64 N=40;return ++N;}StreamPrint("I64 A=%d;",F);}#exe {StreamPrint("I64 B=%d;",F);}A+B-41;|},
      "",
      String.length "I64 A=41;I64 B=42;" );
    ( "nested generated directive",
      {|#exe {StreamPrint("#exe {StreamPrint(\"42;\");}");}|},
      "",
      29 );
    ( "nested task generation",
      {|#exe {#exe {StreamPrint("I64 N=40;");}StreamPrint("%d;",N+2);}|},
      "",
      12 );
    ( "original task layouts",
      {|#exe {I64 N=2;class B{U8 a[N];};class C:B{I64 b;};StreamPrint("%d;",sizeof(C)+32);}|},
      "",
      3 );
    ( "historical callback body",
      {|#exe {I64 F(I64 n=40){return n+2;}I64 (*p)(I64 n=40)=&F;I64 F(I64 n=1){return n;}StreamPrint("%d;",p());}|},
      "",
      3 );
    ( "reached module effects after parse effects",
      {|#exe {Print("parse;");}I64 A=40;I64 F(){Print("init;");return A+2;}I64 B=F;Print("load;");B;|},
      "parse;init;load;",
      0 );
    ( "task defaults and module defaults share allowance",
      {|I64 F(I64 n=40){return n;}#exe {I64 G(I64 n=2){return n;}StreamPrint("I64 N=%d;",G);}F+N;|},
      "",
      8 );
  ]
