(* Source-derived values from TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
   RT_PTR is signed I64. PrsAddOp inserts size placeholders and callback
   difference division. OptFixSizeOf resolves each placeholder using the
   opposite operand's optimized class. OptFixupBinaryOp1 selects raw classes. *)

let word_operators =
  [
    ("or", 40L, "p|2", 42L);
    ("and", 63L, "p&42", 42L);
    ("xor", 40L, "p^2", 42L);
    ("multiply", -21L, "p*-2", 42L);
    ("divide", -84L, "p/-2", 42L);
    ("remainder", 127L, "p%85", 42L);
    ("left shift", 21L, "p<<1", 42L);
    ("right shift", 84L, "p>>1", 42L);
    ("signed right shift", -84L, "p>>1", -42L);
    ("negate", -42L, "-p", 42L);
    ("complement", -43L, "~p", 42L);
    ("unary plus", 42L, "+p", 42L);
    ("logical not", 0L, "!p", 1L);
    ("logical and", 42L, "p&&1", 1L);
    ("logical or", 0L, "p||1", 1L);
    ("logical xor", 42L, "p^^1", 0L);
    ("less", -42L, "p<0", 1L);
    ("greater", 42L, "p>0", 1L);
    ("less equal", 42L, "p<=42", 1L);
    ("greater equal", 42L, "p>=42", 1L);
  ]

let storage_cases =
  List.concat_map
    (fun (label, initial, expression, expected) ->
      List.map
        (fun (shape, text) -> (label ^ " " ^ shape, text, expected))
        [
          ("global", Printf.sprintf "I64 (*p)()=%Ld;%s;" initial expression);
          ( "automatic",
            Printf.sprintf "I64 Run(){F64 (*p)();p=%Ld;return %s;}Run();"
              initial expression );
          ( "static",
            Printf.sprintf
              "class Pair{I64 a;I64 b;};I64 Run(){static Pair \
               (*p)()=%Ld;return %s;}Run();"
              initial expression );
          ( "parameter",
            Printf.sprintf "I64 Run(U0 (*p)()){return %s;}Run(%Ld);" expression
              initial );
          ( "indexed",
            Printf.sprintf
              "I64 (*a)()[2][2]={{0,0},{0,%Ld}};I64 Run(I64 (*p)()){return \
               %s;}Run(a[1][1]);"
              initial expression );
        ])
    word_operators

let parser_cases =
  [
    ("assignment scale", "I64 (*p)();(p=34)+1;", 42L);
    ("assignment or", "I64 Run(){I64 (*p)();return (p=40)|2;}Run();", 42L);
    ("indexed assignment", "I64 (*p)()[2]={0,0};(p[1]=40)|2;", 42L);
    ( "nested assignment",
      "I64 Run(){I64 (*p)(),(*q)();return (q=(p=40))|2;}Run();",
      42L );
    ("grouped scalar class", "I64 (*p)()=32;((p|2))+1;", 35L);
    ("multiply selects scalar class", "I64 (*p)()=17;(p*2)+1;", 35L);
    ("identity multiply retains pointer class", "I64 (*p)()=34;(p*1)+1;", 42L);
    ("identity divide retains pointer class", "I64 (*p)()=34;(p/1)+1;", 42L);
    ( "unsigned identity selects callback class",
      "I64 (*p)()=-1;(p*1(U64))>1;",
      0L );
    ( "unsigned divisor rewrite selects callback class",
      "I64 (*p)()=-85;p/2(U64);",
      -43L );
    ("folded identity retains pointer class", "I64 (*p)()=34;(p*(1+0))+1;", 42L);
    ( "comparison identity retains pointer class",
      "I64 (*p)()=34;(p*(1==1))+1;",
      42L );
    ("chain identity retains pointer class", "I64 (*p)()=34;(p*(1<2<3))+1;", 42L);
    ("false chain resolves scalar class", "I64 (*p)()=34;(p*(3<2<4))+1;", 1L);
    ( "query identity retains pointer class",
      "I64 (*p)()=34;(p*sizeof(I8))+1;",
      42L );
    ( "defined identity retains pointer class",
      "I64 (*p)()=34;(p*defined(p))+1;",
      42L );
    ( "constant shift resolves size before later class",
      "I64 (*p)()=17;(p<<1)+1;",
      35L );
    ("logical result resolves scalar size", "I64 (*p)()=42;(p&&1)+1;", 2L);
    ("right logical result removes left scale", "I64 (*p)()=42;41+(p&&1);", 42L);
    ("right callback scales left", "I64 (*p)()=34;8+p;", 98L);
    ("right callback subtraction", "I64 (*p)()=34;8-p;", 30L);
    ("narrow RHS retains pointer class", "I64 (*p)()=17;I8 n=2;(p*n)+1;", 42L);
    ( "later pointer class does not reinsert scaling",
      "I64 (*p)()=17;I8 n=2;(p*2*n)+1;",
      69L );
    ("two callback addition", "I64 (*p)()=34,(*q)()=8;p+q;", 42L);
    ( "computed right callback is unscaled",
      "I64 (*p)()=34,(*q)()=8;p+(q|0);",
      42L );
    ("difference", "I64 (*p)()=378,(*q)()=42;p-q;", 42L);
    ("grouped difference", "I64 (*p)()=378,(*q)()=42;(p|0)-(q|0);", 42L);
    ("scaled left difference", "I64 (*p)()=370,(*q)()=42;(p+1)-q;", 42L);
    ( "difference consumes pointer class",
      "I64 (*p)()=370,(*q)()=42;(p-q)+1;",
      42L );
    ("negative difference", "I64 (*p)()=-378,(*q)()=-42;p-q;", -42L);
    ("difference truncates toward zero", "I64 (*p)()=-377,(*q)()=-42;p-q;", -41L);
    ( "callback read keeps its cell",
      "I64 F(){return 42;}I64 Run(){I64 (*p)(),(*q)();q=(p=&F);return \
       q();}Run();",
      42L );
    ( "stored numeric result",
      "I64 Run(){I64 (*p)(),(*q)();p=40;q=p|2;return q;}Run();",
      42L );
    ( "ordinary initializer",
      "I64 Run(){I64 (*p)();p=40;I64 n=(p|2);return n;}Run();",
      42L );
    ( "argument",
      "I64 Id(I64 n){return n;}I64 Run(){I64 (*p)();return Id((p=40)|2);}Run();",
      42L );
    ( "condition",
      "I64 Run(){I64 (*p)();if((p=40)|2)return 42;return 0;}Run();",
      42L );
    ( "skipped consumer",
      "I64 F(){return 42;}I64 Run(){I64 (*p)();p=&F;if(0){return p|2;}return \
       42;}Run();",
      42L );
    ("shared middle word", "I64 (*p)()=42;0<p<50;", 1L);
    ("chain consumes pointer class", "I64 (*p)()=42;(p<50<60)+41;", 42L);
    ("grouped comparison selects scalar class", "I64 (*p)()=42;(p<50)+1;", 2L);
  ]

let unsigned_cases =
  [
    ("unsigned division", "I64 (*p)()=-1;U64 d=2;p/d;", Int64.max_int);
    ("unsigned shift", "I64 (*p)()=-1;U64 d=1;p>>d;", Int64.max_int);
    ("unsigned comparison", "I64 (*p)()=-1;U64 d=0;p>d;", 1L);
    ( "parser class controls nested complement",
      "I64 (*p)()=0;U64 d=0;(~(p|d))>1;",
      0L );
    ( "parser class controls nested logical not",
      "I64 (*p)()=0;U64 d=0;(!(p|d))+1;",
      9L );
    ( "later pass selects unsigned scaled addition",
      "I64 (*p)()=-9;U64 n=1;(p+n)>1;",
      1L );
    ("assignment remains signed", "I64 (*p)();U64 n=-1;(p=n)>>1;", -1L);
    ("assignment division remains signed", "I64 (*p)();U64 n=-84;(p=n)/2;", -42L);
    ( "word addition wraps",
      "I64 (*p)()=0x7ffffffffffffffa;p+1;",
      0x8000000000000002L );
    ( "difference wraps before division",
      "I64 (*p)()=0x8000000000000000,(*q)()=8;p-q;",
      0x0fffffffffffffffL );
    ("constant shift masks count", "I64 (*p)()=21;p<<65;", 42L);
    ("signed divisor optimization", "I64 (*p)()=-85;p/2;", -43L);
    ("selected F64 header divisor optimization", "F64 (*p)()=-85;p/2;", -43L);
  ]

let cases = storage_cases @ parser_cases @ unsigned_cases

let jit_cases =
  [
    ( "saved default",
      "I64 (*p)()=40;I64 Run(I64 n=(p|2)){return n;}p=0;Run();",
      42L );
    ( "static initializer",
      "I64 (*p)()=40;I64 Run(){static I64 (*q)()=(p|2);return q;}Run();Run();",
      42L );
  ]

let owned_consumers =
  [
    "(p=&F)+0";
    "p+1";
    "p|0";
    "p&0";
    "p^0";
    "p*1";
    "p/1";
    "p%1";
    "p<<0";
    "p>>0";
    "-p";
    "~p";
    "!p";
    "p<1";
    "p&&1";
    "p||1";
    "p^^1";
    "p-q";
  ]
