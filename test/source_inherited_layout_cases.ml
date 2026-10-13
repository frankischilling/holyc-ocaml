let headers = "extern U0 Print(U8 *fmt,...);extern U0 StreamPrint(U8 *fmt,...);"

let values =
  [
    ( "member metadata words and joined strings",
      {|class B{U8 x count 40+2 note "A" "B" bits 1.5 at $$;};sizeof(B)+41;|} );
    ( "metadata survives inherited and nested objects",
      {|class B{U8 x data 42;};class C:B{I64 y data 43;};class D{C c data 44;};sizeof(D)+33;|}
    );
    ( "metadata on callback and union members",
      {|union B{I64 (*cb)() note "callback";U8 x data 1;};class C{B b;};sizeof(C)+34;|}
    );
    ( "metadata on multiple declarators",
      {|class B{U8 x data 1,y data 2;I64 z data 3;};sizeof(B)+32;|} );
    ( "metadata before persistent initialization",
      {|class B{I64 x data 42;};B b={42};b.x;|} );
    ( "metadata on a function local class",
      {|I64 F(){class B{I64 x data 42;};B b;b.x=42;return b.x;}F();|} );
    ("base prefix", {|class B{U8 a[34];};class C:B{I64 b;};sizeof(C);|});
    ( "three original bases",
      {|class A{U8 a;};class B:A{I64 b;};class C:B{U8 c[33];};sizeof(C);|} );
    ("empty child", {|class B{U8 a[42];};class C:B{};sizeof(C);|});
    ("forward base", {|extern class B;class C:B{U8 a[42];};sizeof(C);|});
    ("self base metadata", {|class C:C{U8 a[42];};sizeof(C);|});
    ( "completed forward",
      {|extern class B;class B{U8 a[34];};class C:B{I64 b;};sizeof(C);|} );
    ( "union still overlaps at zero",
      {|class B{U8 a[30];};union C:B{U8 b[40];I64 c;};sizeof(C)+2;|} );
    ( "union retains larger base",
      {|class B{U8 a[42];};union C:B{U8 b;I64 c;};sizeof(C);|} );
    ( "union as base",
      {|union B{U8 a[34];I64 b;};class C:B{I64 child;};sizeof(C);|} );
    ( "offset replaces current size",
      {|class B{U8 a[34];};class C:B{$$=34;I64 b;};sizeof(C);|} );
    ( "position starts at copied size",
      {|class B{U8 a[34];};class C:B{$$=$$;I64 b;};sizeof(C);|} );
    ( "base padding is copied once",
      {|class B{$$=-2;U8 a[36];};class C:B{U8 b[6];};sizeof(C);|} );
    ( "child owns its own padding",
      {|class B{U8 a[34];};class C:B{$$=-2;U8 b[40];};sizeof(C)+2;|} );
    ( "original child partial sizeof",
      {|class B{U8 a[14];};class C:B{U8 b[sizeof(C)];};sizeof(C)+14;|} );
    ( "pointer and callback base fields",
      {|class B{U0 (*cb)();U8 *p;U8 a[18];};class C:B{I64 b;};sizeof(C);|} );
    ( "uncalled function publishes bases",
      {|U0 Make(){class B{U8 a[34];};class C:B{I64 b;};}sizeof(C);|} );
    ( "saved original derived default",
      {|class B{U8 a[34];};class C:B{I64 b;};I64 F(I64 n=sizeof(C)){return n;}class B{U8 a;};class C:B{U8 b;};F;|}
    );
    ( "derived global and local extents",
      {|class B{U8 a[34];};class C:B{I64 b;};I64 A[sizeof(C)];I64 F(){I64 a[sizeof(C)];return sizeof(a)/8;}F()+sizeof(A)/8-42;|}
    );
    ( "directive forward completion during lookahead",
      {|#exe {extern class B;class C:B #exe{class B{U8 a[34];};}{I64 b;};StreamPrint("%d;",sizeof(C));}|}
    );
    ( "directive partial base",
      {|#exe {I64 Saved=0;class B{U8 a[34];#exe{class C:B{I64 b;};Saved=sizeof(C);}U8 tail;};StreamPrint("%d;",Saved);}|}
    );
    ( "selected completed base survives replacement",
      {|class B{U8 a[34];};class C:B #exe{class B{U8 a;};}{I64 b;};sizeof(C);|}
    );
  ]

let jit_values =
  [
    ( "forward completion after frozen lookup",
      {|extern class B;class C:B #exe{class B{U8 a[34];};}{I64 b;};sizeof(C);|}
    );
    ( "partial base before later growth",
      {|I64 Saved=0;class B{U8 a[34];#exe{class C:B{I64 b;};Saved=sizeof(C);}U8 tail;};Saved;|}
    );
    ( "negative partial base",
      {|I64 Saved=0;class B{$$=-2;#exe{class C:B{U8 b[44];};Saved=sizeof(C);}U8 tail[3];};Saved;|}
    );
    ( "runtime base dimension",
      {|I64 Next(){return 34;}class B{U8 a[Next()];};class C:B{I64 b;};sizeof(C);|}
    );
    ( "runtime base offset",
      {|I64 Next(){return 34;}class B{$$=Next();};class C:B{I64 b;};sizeof(C);|}
    );
    ( "runtime inherited union offset",
      {|I64 N=34;class B{U8 a[30];};union C:B{$$=N;U8 b[8];};sizeof(C);|} );
    ( "runtime inherited saved default",
      {|I64 N=34;class B{U8 a[N];};class C:B{I64 b;};I64 F(I64 n=sizeof(C)){return n;}N=1;F;|}
    );
    ( "metadata retains executed offset dependencies",
      {|I64 N=7;class B{$$=++N;I64 x data $$;};sizeof(B)+N+18;|} );
    ( "metadata retains executed dimension query dependencies",
      {|I64 N=1;class B{U8 x[++N] data sizeof(B);};sizeof(B)+N+38;|} );
    ( "live metadata evaluates once per source entry",
      {|I64 N=0;I64 Next(){return ++N;}class B{I64 x data Next() data Next();I64 y data Next();};N+39;|}
    );
    ( "live metadata variable updates preserve source order",
      {|I64 N=0;class B{I64 x a N++ b ++N;};N+40;|} );
    ( "live metadata uses placed size and compiler position",
      {|I64 N=0;I64 Save(I64 value){N=value;return value;}class B{U8 x data Save($$+sizeof(B));};N+40;|}
    );
    ( "live metadata runs during uncalled function parsing",
      {|I64 N=0;I64 Next(){return ++N;}U0 Never(){class B{I64 x data Next();};}N+41;|}
    );
    ( "live metadata calls retained callbacks",
      {|I64 N=0;I64 Next(){return ++N;}I64 (*Cb)();Cb=&Next;class B{I64 x data Cb();};N+41;|}
    );
    ( "metadata lookahead precedes saved runtime evaluation",
      {|I64 N=1;I64 Next(){return ++N;}class B{I64 x data Next() #exe{N=40;};};N+1;|}
    );
    ( "metadata arguments use original reverse source order",
      {|I64 N=0;I64 Pair(I64 a,I64 b){N=a*10+b;return N;}class B{I64 x data Pair(++N,++N);};N+21;|}
    );
    ( "metadata source input can execute nested child input",
      {|#exe {I64 N=0;I64 Next(){StreamExePrint("N=41;");return ++N;}class B{I64 x data Next();};StreamPrint("%d;",N);}|}
    );
    ( "metadata child retains its own closed metadata work",
      {|#exe {I64 Next(){StreamExePrint("class C{I64 x data 40+2;};");return 42;}class B{I64 x data Next();};StreamPrint("42;");}|}
    );
  ]

let metadata_effects =
  {|I64 Next(){Print("M");return 42;}class B{I64 x a Next() b Next();};42;|}

let metadata_faults =
  [
    ( {|I64 Z=0;I64 Bad(){Print("A");return 1/Z;}class B{I64 x data Bad();};Print("B");42;|},
      "HCIRVM0009",
      "A" );
    ( {|I64 Next(){Print("A");return 42;}class B{I64 x data Next() 9;};42;|},
      "HCPARSE0114",
      "A" );
  ]

let metadata_counter =
  {|I64 N=0;I64 Side(){N++;return 42;}class B{I64 x data Side();};N+41;|}

let metadata_children =
  [
    {|#exe {I64 N=0;I64 Next(){StreamExePrint("N=41;");return ++N;}class B{I64 x data Next();};StreamPrint("%d;",N);}|};
    {|#exe {I64 Next(){StreamExePrint("class C{I64 x data 40+2;};");return 42;}class B{I64 x data Next();};StreamPrint("42;");}|};
  ]

let effects =
  {|I64 Count(){Print("dim");return 4;}I64 Offset(){Print("off");return 34;}
class B{U8 a[Count()];$$=Offset();};class C:B{I64 b;};class D:C{};
I64 F(I64 n=sizeof(D)){I64 a[sizeof(D)];return sizeof(a)/8+n-42;}
class B{U8 a;};F;F;|}

let bad_brace =
  {|class B{U8 a[34];};class C:B #exe{Print("%d",sizeof(C));} Missing;|}

let comma = {|class B{U8 a[34];};class C:B #exe{Print("%d",sizeof(C));},B {};|}
let overflow = {|class B{$$=9223372036854775807;};class C:B{U8 a;};sizeof(C);|}

let object_storage =
  {|class B{U8 a[34];};class C:B{I64 b;};I64 F(){C value;return 42;}F();|}
