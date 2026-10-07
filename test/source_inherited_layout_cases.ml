let headers = "extern U0 Print(U8 *fmt,...);extern U0 StreamPrint(U8 *fmt,...);"

let values =
  [
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
    ("union as base", {|union B{U8 a[34];I64 b;};class C:B{I64 b;};sizeof(C);|});
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
