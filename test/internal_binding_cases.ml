let parenthesized =
  [
    ("_intern (0x1d) Bool F(I64 i);F(42);", 1L);
    ("_intern (0x77) Bool F(U8 *p,I64 n);I64 q=0;F(&q,0);", 0L);
    ("_intern (0x1e) I64 Bad(U8 ch);Bad(97);", 65L);
    ("_intern (0xa9) I64 Bad(I64 i);Bad(42);", 42L);
    ("_intern (0xab) I64 Bad(I64 a,I64 b);Bad(1,2);", 1L);
    ("_intern (0x7f) I64 Bad(I64 bits);Bad(1);", 0L);
    ("_intern (0xaf) U64 F(U64 *q,U64 d);U64 q=42;F(&q,2);", 0L);
  ]

let is_parenthesized source =
  List.exists (fun (original, _) -> source = original) parenthesized

let values =
  parenthesized
  @ [
      ("_intern 0x1d U8 F(I64 n);F(-1);", 1L);
      ("_intern 0xaa I64 F(I64 n);F(-42);", -1L);
      ("_intern 0xb0 I64 F(I64 n);F(-7);", 49L);
      ("_intern 0xb1 U64 F(U64 n);F(0xffffffffffffffff);", 1L);
      ("_intern 0xac U64 F(U64 a,U64 b);F(0xffffffffffffffff,42);", 42L);
      ("_intern 0xad I64 F(I64 a,I64 b);F(-1,42);", 42L);
      ("_intern 0xae U64 F(U64 a,U64 b);F(0x8000000000000000,1);", Int64.min_int);
      ("_intern 0x77 Bool F(U8 *p,I64 n);U64 bits=0x100;F((&bits)(U8 *),8);", 1L);
      ("_intern 0x78 Bool F(U8 *p,I64 n);U8 bits=0;F(&bits,1);bits;", 2L);
      ("_intern 0x79 Bool F(U8 *p,I64 n);U8 bits=3;F(&bits,1);bits;", 1L);
      ("_intern 0x7a Bool F(U8 *p,I64 n);U8 bits=3;F(&bits,1);bits;", 1L);
      ("_intern 0xa5 U0 F(U8 *a,U8 *b);U8 a=41,b=42;F(&a,&b);a;", 42L);
      ("_intern 0xa6 U0 F(U16 *a,U16 *b);U16 a=41,b=42;F(&a,&b);a;", 42L);
      ("_intern 0xa7 U0 F(U32 *a,U32 *b);U32 a=41,b=42;F(&a,&b);a;", 42L);
      ("_intern 0xa8 U0 F(I64 *a,I64 *b);I64 a=41,b=42;F(&a,&b);a;", 42L);
      ("_intern 0x10+0xe I64 Convert(U8 c);Convert(97);", 65L);
      ("I64 Op=0x1e;_intern Op I64 Convert(U8 c);Op=0x84;Convert(97);", 65L);
      ( "I64 N=0;I64 Target(){N++;return 0x1e;}_intern Target() I64 Convert(U8 \
         c);Convert(97)+N;",
        66L );
      ( "I64 N=0;I64 Target(){N++;return 0x1e;}_intern Target() I64 Unused(U8 \
         c);N+41;",
        42L );
      ( "I64 Target(){return 0x1e;}I64 (*p)()=&Target;_intern p() I64 \
         Convert(U8 c);Convert(97);",
        65L );
      ( "_intern 0x7e I64 Lowest(I64 n);_intern Lowest(0x40000000) I64 \
         Convert(U8 c);Convert(97);",
        65L );
      ( "_intern 0x84 I64 Length(U8 *s);_intern \
         Length(\"..............................\") I64 Convert(U8 \
         c);Convert(97);",
        65L );
      ( "_intern 0x1e I64 Convert(U8 c);I64 Old(){return Convert(97);}_intern \
         0x84 I64 Convert(U8 *s);Old();",
        65L );
      ( "_intern 0x1e I64 Convert(U8 c);I64 Old(){return Convert(97);}I64 \
         Convert(U8 c){return c;}Old();",
        65L );
      ( "_intern 0x84 I64 Length(U8 *s);I64 F(U8 *p=\"AB\"){return \
         Length(p)+40;}F();",
        42L );
      ( "_intern 0x84 I64 Length(U8 *s);I64 F(U8 *p=\"AB\"){p[0]=0;return \
         Length(p)+42;}F();",
        42L );
      ("_intern -1 I64 Unused();42;", 42L);
      ("_intern 0xffffffffffffffff I64 Unused();42;", 42L);
    ]

let faults =
  [
    ("_intern 42 I64 G;42;", "HCRUN0004");
    ("I64 N;_intern N I64 Convert(U8 c);", "HCIRVM0012");
    ( "I64 A[1]={1};I64 N=0;I64 Target(){N++;return A[1];}_intern Target() I64 \
       Convert(U8 c);",
      "HCIRVM0019" );
    ("_intern 1.0 I64 Convert(U8 c);", "HCRUN0001");
    ("_intern 0x1e I64 Convert(U8 c=97);Convert();", "HCRUN0003");
  ]
