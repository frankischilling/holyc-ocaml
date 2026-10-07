let values =
  [
    ("I64 N=-9;class C{U8 a;$$=N;I64 b;};sizeof(C)+34;", 42L);
    ("I64 N=0;I64 Next(){N++;return 3;}I64 A[Next()];N=9;sizeof(A)+18;", 42L);
    ("I64 N=1;I64 A[++N][++N]={1,2,3,4,5,6};A[1][2]+N+33;", 42L);
    ("I64 N=2;I64 A[N]={41,1};I64 B=A[0]+A[1];B;", 42L);
    ( "I64 N=0;I64 Next(){N++;return 3;}I64 F(){I64 A[Next()];return \
       sizeof(A)+N+17;}F();F();",
      42L );
    ("I64 N=0;I64 F(){I64 A[++N+2];return 0;}N+41;", 42L);
    ("I64 N=2;I64 F(){I64 A[N];A[0]=40;A[1]=2;return A[0]+A[1];}N=9;F();", 42L);
    ("I64 N=0;I64 F(){static I64 A[++N+2];return sizeof(A)+N+17;}F();F();", 42L);
    ( "I64 N=2;I64 F(){static I64 A[N]={40,2};return A[0]+A[1];}N=9;F();F();",
      42L );
    ("I64 Next(){return 3;}I64 (*p)()=&Next;I64 A[p()];sizeof(A)+18;", 42L);
    ( "I64 Next(){return 3;}I64 (*p)()=&Next;I64 F(){I64 A[p()];return \
       sizeof(A)+18;}F();",
      42L );
    ("I64 N=3;I64 Next(I64 n=N){return n;}N=9;I64 A[Next()];sizeof(A)+18;", 42L);
    ("_intern 0xb0 I64 Square(I64 n);I64 A[Square(2)];sizeof(A)+10;", 42L);
    ("I64 N=0;class C{I64 A[++N+2];};sizeof(C)+N+17;", 42L);
    ("I64 N=2;class C{U8 head;I64 A[N];U8 tail;};sizeof(C)+24;", 42L);
    ("I64 N=3;union C{I64 A[N];U8 B[N+1];};sizeof(C)+18;", 42L);
    ( "I64 N=2;class C{U8 head;union{I64 A[N];U8 b;};U8 tail;};sizeof(C)+24;",
      42L );
    ("I64 N=2;class C{I64 A[N];};I64 A[sizeof(C)];sizeof(A)/8+26;", 42L);
    ( "I64 N=2;class C{I64 A[N];};I64 Old(){return sizeof(C);}class C{U8 \
       a;};Old()+sizeof(C)+25;",
      42L );
    ( "I64 N=0;I64 Next(){N++;return 16;}class C{U8 a;$$=Next();I64 \
       b;};sizeof(C)+N+17;",
      42L );
    ( "I64 Next(){return 16;}I64 (*p)()=&Next;class C{U8 a;$$=p();I64 \
       b;};sizeof(C)+18;",
      42L );
    ("I64 N=16;class C{U8 a;$$=N;I64 b;};N=99;sizeof(C)+18;", 42L);
    ("I64 N=7;class C{U8 a;$$=$$+N;I64 b;};sizeof(C)+26;", 42L);
    ("I64 N=16;union C{U8 a;$$=N;I64 b;};sizeof(C)+18;", 42L);
    ( "I64 N=7;class C{U8 a;union{U8 b;$$=$$+N;I64 c;};U8 tail;};sizeof(C)+25;",
      42L );
    ("I64 N=2;class C{I64 A[N];$$=$$+N;I64 b;};sizeof(C)+16;", 42L);
    ("I64 N=16;class C{U8 a;$$=N;I64 b;};I64 A[sizeof(C)];sizeof(A)/8+18;", 42L);
    ( "I64 N=16;class C{U8 a;$$=N;I64 b;};I64 Old(){return sizeof(C);}class \
       C{U8 a;};Old()+sizeof(C)+17;",
      42L );
    ( "I64 N=2;class C{I64 A[N];};I64 F(){I64 A[sizeof(C)];return \
       sizeof(A)/8+26;}F();",
      42L );
    ( "I64 N=2;I64 F(){I64 A[N];return sizeof(A);}I64 (*old)()=&F;N=3;I64 \
       F(){I64 A[N];return sizeof(A);}old()+F()+2;",
      42L );
    ( "I64 N=1;I64 F(){I64 A[++N][++N];A[1][2]=N+39;return A[1][2];}F();F();",
      42L );
    ("42;I64 N=2;I64 A[N];", 42L);
    ("42;I64 N=16;class C{U8 a;$$=N;I64 b;};", 42L);
  ]

let unsupported =
  [
    ( "I64 N=16;I64 F(){class C{U8 a;$$=N;I64 b;};return sizeof(C)+18;}F();",
      "HCPARSE0048" );
  ]

let output =
  "extern U0 Print(U8 *s,...);I64 Next(){Print(\"dim\");return 2;}I64 \
   Offset(){Print(\"off\");return 16;}I64 A[Next()]={40,2};class C{U8 \
   a;$$=Offset();I64 b;};A[0]+A[1];"

let reached_failures =
  [
    "extern U0 Print(U8 *s,...);I64 Next(){Print(\"kept\");return -1;}I64 \
     A[Next()];";
    "extern U0 Print(U8 *s,...);I64 Next(){Print(\"kept\");return 2;}I64 \
     A[Next();";
    "extern U0 Print(U8 *s,...);I64 Next(){Print(\"kept\");return 2;}I64 \
     A[Next()][Missing];";
    "extern U0 Print(U8 *s,...);I64 Next(){Print(\"kept\");return 2;}I64 \
     A[Next()];Missing;";
    "extern U0 Print(U8 *s,...);I64 Next(){Print(\"kept\");return 2;}class \
     C{I64 A[Next()];Missing bad;};";
    "extern U0 Print(U8 *s,...);I64 Next(){Print(\"kept\");return 16;}class \
     C{U8 a;$$=Next();Missing b;};";
  ]
