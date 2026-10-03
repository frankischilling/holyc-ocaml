let cases =
  [
    ( "restored signed class reaches division",
      "I64 F(I64 x){return (x>>0x8000000000000001)/2;}F(-7);",
      -2L );
    ( "restored signed class reaches unary minus",
      "I64 F(I64 x){return -(x>>0x8000000000000001);}F(-7);",
      4L );
    ( "restored signed class reaches complement",
      "I64 F(I64 x){return ~(x>>0x8000000000000001);}F(-7);",
      3L );
    ( "restored signed class reaches logical not",
      "I64 F(I64 x){return !(x>>0x8000000000000001);}F(-7);",
      0L );
    ( "sticky unsigned less-than",
      "I64 F(I64 x){return (x>>0x8000000000000001)<0;}F(-7);",
      0L );
    ( "sticky unsigned greater-than",
      "I64 F(I64 x){return (x>>0x8000000000000001)>0;}F(-7);",
      1L );
    ("signed following comparison", "I64 F(I64 x){return (x>>65)<0;}F(-7);", 1L);
    ( "opposite directions stay sequential",
      "I64 F(I64 x){return (x<<63)>>1;}F(1);",
      -4611686018427387904L );
    ("constant children fold separately", "(-7<<63)<<1;", 0L);
    ("constant signed right children", "(-7>>63)>>1;", -1L);
    ( "constant unsigned count selects logical shift",
      "-7>>0x8000000000000001;",
      9223372036854775804L );
    ( "variable unsigned count stays logical",
      "I64 F(I64 x,U64 n){return x>>n;}F(-7,0x8000000000000001);",
      9223372036854775804L );
    ( "automatic initializer uses canonical shift",
      "I64 F(I64 x){I64 n=(x<<63)<<1;return n;}F(-7);",
      -7L );
    ( "fixed argument uses canonical shift",
      "I64 Id(I64 n){return n;}I64 F(I64 x){return Id((x<<63)<<1);}F(-7);",
      -7L );
    ( "intrinsic argument uses canonical shift",
      "_intern 0x7f I64 Bsr(I64 n);I64 F(I64 x){return Bsr((x<<63)<<1);}F(8);",
      3L );
    ("folded global value", "I64 G=1<<3;G;", 8L);
    ("folded narrow destination", "I8 G=255<<1;I64 F(){return G;}F();", -2L);
    ( "folded static value persists",
      "I64 F(){static I64 n=1<<2;return ++n;}F();F();",
      6L );
    ( "folded narrow static destination",
      "I64 F(){static I8 n=255<<1;return n;}F();",
      -2L );
    ( "folded default prepares even when supplied",
      "I64 F(I64 n=1<<3){return n;}F(42);",
      42L );
    ("folded default reused", "I64 F(I64 n=1<<3){return n;}F()+F();", 16L);
    ("nested folded default", "I64 F(I64 n=(-7<<63)<<1){return n;}F();", 0L);
    ( "effectful word evaluated once",
      "I64 N=0;I64 Next(){return ++N;}I64 F(){return (Next()<<63)<<1;}F()*10+N;",
      11L );
    ( "left value captured before right write",
      "I64 n=0;I64 F(){return ((n<<63)<<1)+(n=5);}n=-7;F()*10+n;",
      -15L );
    ( "argument order survives removed counts",
      "I64 N=0;I64 Next(){return ++N;}I64 Pair(I64 a,I64 b){return \
       a*10+b;}Pair((Next()<<63)<<1,(Next()<<63)<<1)*10+N;",
      212L );
  ]
  @ Public_shift_class_cases.cases ()

let retained =
  "#exe {I64 N=0;I64 Init(){N++;return 1<<2;}I64 Saved(I64 n=Init()){return \
   n;}if(N!=1||Saved()!=4)Print(\"bad\");N=0;if(Saved()!=4||N)Print(\"bad\");StreamPrint(\"%d;\",Saved()+38);}"

let rejected =
  [
    "I64 G=1;I64 H=(G<<63)<<1;H;";
    "I64 F(I64 x){return x<<2;}I64 G=F(1);G;";
    "I64 N=0;I64 Touch(){N=42;return N<<1;}I64 A[2]={40,Touch()};";
    "I64 F(){static I64 n=1;n<<=2;return n;}I64 G=F();G;";
  ]

let limit_source =
  "extern U0 Print(U8 *fmt,...);I64 F(I64 x){Print(\"kept\");return \
   49+((x<<63)<<1);}F(-7);"

let fault_source =
  "extern U0 Print(U8 *fmt,...);I64 F(I64 x){Print(\"kept\");return \
   ((x<<63)<<1)/0;}F(-7);"

let fault_cases =
  [
    (fault_source, "kept");
    ( "extern U0 Print(U8 *fmt,...);I64 Word(){Print(\"left\");return 1/0;}I64 \
       F(){return (Word()<<63)<<1;}F();",
      "left" );
    ( "extern U0 Print(U8 *fmt,...);I64 Count(){Print(\"right\");return \
       1/0;}I64 F(I64 x){return (x<<63)<<Count();}F(-7);",
      "right" );
  ]

let output_source =
  "extern U0 Print(U8 *fmt,...);I64 N=0;I64 Next(){return ++N;}I64 \
   F(){Print(\"%d:%d;\",(Next()<<63)<<1,(Next()<<63)<<1);return N+40;}F();"
