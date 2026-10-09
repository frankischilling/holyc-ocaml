let headers = "extern U0 Print(U8 *fmt,...);extern U0 StreamPrint(U8 *fmt,...);"

let values =
  [
    ( "uncalled function publishes type",
      {|U0 Make(){class C{U8 a[42];};}sizeof(C);|} );
    ( "called function body",
      {|I64 F(){class C{U8 a[42];};return sizeof(C);}F();|} );
    ( "following function",
      {|U0 Make(){class C{U8 a[42];};}I64 F(){return sizeof(C);}F();|} );
    ( "saved default",
      {|U0 Make(){class C{U8 a[42];};}I64 F(I64 n=sizeof(C)){return n;}F;|} );
    ( "two declarations",
      {|I64 F(){class A{U8 a[20];};class B{U8 b[22];};return sizeof(A)+sizeof(B);}F();|}
    );
    ("union", {|U0 Make(){union C{U8 a[42];I64 b;};}sizeof(C);|});
    ( "forward completion",
      {|U0 Make(){extern class C;}class C{U8 a[42];};sizeof(C);|} );
    ( "comma forwards",
      {|U0 Make(){extern class A,extern union B;}class A{U8 a[20];};union B{U8 a[22];};sizeof(A)+sizeof(B);|}
    );
    ( "historical repeated name",
      {|I64 F(){class C{U8 a[20];};I64 n=sizeof(C);class C{U8 a[22];};return n+sizeof(C);}F();|}
    );
    ( "default keeps original class",
      {|U0 Make(){class C{U8 a[20];};}I64 F(I64 n=sizeof(C)){return n;}U0 Other(){class C{U8 a[22];};}F+sizeof(C);|}
    );
    ("empty class", {|U0 Make(){class C{};}sizeof(C)+42;|});
    ( "anonymous union",
      {|U0 Make(){class C{U8 a;union{U8 b[33];I64 c;}U8 d;};}sizeof(C)+7;|} );
    ( "following global array",
      {|U0 Make(){class C{U8 a[6];};}I64 A[sizeof(C)];sizeof(A)-6;|} );
    ( "unbraced if ends at class",
      {|I64 F(){I64 n=40;if(0)class C{U8 a[42];};n+=2;return n;}F()+sizeof(C)-42;|}
    );
    ("while body", {|U0 Make(){while(0){class C{U8 a[42];};}}sizeof(C);|});
    ("for body", {|U0 Make(){for(;0;){class C{U8 a[42];};}}sizeof(C);|});
    ("do body", {|U0 Make(){do{class C{U8 a[42];};}while(0);}sizeof(C);|});
    ( "switch body",
      {|U0 Make(){switch(0){case 1:class C{U8 a[42];};}}sizeof(C);|} );
    ("nested ordinary block", {|{class C{U8 a[42];};}sizeof(C);|});
    ( "preprocessor visibility",
      {|U0 Make(){class C{U8 a[42];};}
#ifdef C
sizeof(C);
#else
0;
#endif
|} );
    ( "class in generated stream",
      {|#exe {StreamPrint("U0 Make(){class C{U8 a[42];};}sizeof(C);");}|} );
    ( "constant aggregate offset",
      {|U0 Make(){class C{U8 a;$$=34;U8 b[8];};}sizeof(C);|} );
    ("callback member", {|U0 Make(){class C{U0 (*cb)();U8 a[34];};}sizeof(C);|});
    ("public declaration", {|U0 Make(){public class C{U8 a[42];};}sizeof(C);|});
    ( "comma class definitions",
      {|U0 Make(){class A{U8 a[20];},union B{U8 b[22];};}sizeof(A)+sizeof(B);|}
    );
  ]

let effects =
  {|I64 N=34;I64 Count(){Print("dim");return 8;}I64 Offset(){Print("off");return N;}
U0 Make(){class C{U8 a;$$=Offset();U8 b[Count()];};}sizeof(C);|}

let reached_failure =
  {|I64 Count(){Print("kept");return 42;}U0 Make(){class C{U8 a[Count()];} variable;}|}

let local_position = {|I64 F(){I64 #exe {Print("P");} n=40;return n+2;}F();|}
