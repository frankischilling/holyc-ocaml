let values =
  [
    ( "earlier callback-field gate now executes its original source",
      {|class Box{I64 (*call)(I64 value);};I64 Id(I64 value){return value;}I64 F(){Box o;o.call=&Id;return o.call(42);}F();|},
      42L,
      "" );
    ( "direct callback field",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);};I64 Run(){Box b;b.f=&Add;return b.f(40);}Run();|},
      42L,
      "" );
    ( "callback field array copies",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n)[2];};I64 Run(){Box b;b.f[0]=&Add;b.f[1]=b.f[0];return b.f[1](40);}Run();|},
      42L,
      "" );
    ( "multidimensional callback field array",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n)[2][2];};I64 Run(){Box b;b.f[1][0]=&Add;b.f[0][1]=b.f[1][0];return b.f[0][1](40);}Run();|},
      42L,
      "" );
    ( "inherited unaligned callback field",
      {|I64 Add(I64 n){return n+2;}class Base{U8 pad;I64 (*f)(I64 n);};class Box:Base{U16 tail;};I64 Run(){Box b;b.f=&Add;return b.f(40);}Run();|},
      42L,
      "" );
    ( "nested callback field",
      {|I64 Add(I64 n){return n+2;}class Inner{I64 (*f)(I64 n);};class Box{U8 pad;Inner item;};I64 Run(){Box b;b.item.f=&Add;return b.item.f(40);}Run();|},
      42L,
      "" );
    ( "caller object stores a callee-selected callback",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);};U0 Set(Box *b){b->f=&Add;}I64 Run(){Box b;Set(&b);return b.f(40);}Run();|},
      42L,
      "" );
    ( "saved member callee precedes argument side effects",
      {|I64 A(I64 n){return n+2;}I64 B(I64 n){return n+3;}class Box{I64 (*f)(I64 n);};I64 Side(Box *b){b->f=&B;return 40;}I64 Run(){Box b;b.f=&A;return b.f(Side(&b));}Run();|},
      42L,
      "" );
    ( "callback field fixed declaration default",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n=40);};I64 Run(){Box b;b.f=&Add;return b.f();}Run();|},
      42L,
      "" );
    ( "persistent callback field",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);};Box b;b.f=&Add;b.f(40);|},
      42L,
      "" );
    ( "static callback field",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);};I64 Run(I64 set){static Box b;if(set)b.f=&Add;return b.f(40);}Run(1);Run(0);|},
      42L,
      "" );
    ( "union callback field shares an owned word",
      {|I64 Add(I64 n){return n+2;}union Box{I64 (*f)(I64 n);I64 (*g)(I64 n);};I64 Run(){Box b;b.f=&Add;return b.g(40);}Run();|},
      42L,
      "" );
    ( "disjoint stores preserve callback ownership",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);U8 n;};I64 Run(){Box b;b.f=&Add;b.n=40;return b.f(b.n);}Run();|},
      42L,
      "" );
    ( "field to local callback transport",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);};I64 Run(){Box b;b.f=&Add;I64 (*p)(I64 n);p=b.f;return p(40);}Run();|},
      42L,
      "" );
    ( "field to fixed callback parameter",
      {|I64 Add(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 n),I64 n){return p(n);}class Box{I64 (*f)(I64 n);};I64 Run(){Box b;b.f=&Add;return Apply(b.f,40);}Run();|},
      42L,
      "" );
    ( "void callback field",
      {|I64 n=40;U0 Add(){n+=2;}class Box{U0 (*f)();};I64 Run(){Box b;b.f=&Add;b.f();return n;}Run();|},
      42L,
      "" );
    ( "variadic callback field",
      {|I64 Add(I64 n,...){return n+argc+argv[0];}class Box{I64 (*f)(I64 n,...);};I64 Run(){Box b;b.f=&Add;return b.f(40,1);}Run();|},
      42L,
      "" );
    ( "numeric callback field updates use pointer stride",
      {|class Box{I64 (*f)();};I64 Run(){Box b;b.f=2;b.f+=4;b.f++;return b.f(I64);}Run();|},
      42L,
      "" );
    ( "numeric callback field array updates",
      {|class Box{I64 (*f)()[2];};I64 Run(){Box b;b.f[1]=2;b.f[1]+=4;b.f[1]++;return b.f[1](I64);}Run();|},
      42L,
      "" );
    ( "locked numeric callback field array updates",
      {|class Box{I64 (*f)()[2];};I64 Run(){Box b;b.f[1]=2;lock b.f[1]+=4;lock b.f[1]++;return b.f[1](I64);}Run();|},
      42L,
      "" );
    ( "Bool callback field",
      {|Bool Yes(){return 1;}class Box{Bool (*f)();};I64 Run(){Box b;b.f=&Yes;return 41+b.f();}Run();|},
      42L,
      "" );
  ]

let width_values =
  List.map
    (fun name ->
      ( "callback field return width " ^ name,
        Printf.sprintf
          "%s Add(I64 n){return n+2;}class Box{%s (*f)(I64 n);};I64 Run(){Box \
           b;b.f=&Add;return b.f(40);}Run();"
          name name,
        42L,
        "" ))
    [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ]

let faults =
  [
    ( "unknown callback field",
      {|class Box{I64 (*f)();};I64 Run(){Box b;return b.f();}Run();|},
      "HCIRVM0012",
      "" );
    ( "numeric callback field grants no executable owner",
      {|class Box{I64 (*f)();};I64 Run(){Box b;b.f=17;return b.f();}Run();|},
      "HCIRVM0024",
      "" );
    ( "restored callback bits retain no owner",
      {|I64 Add(){return 42;}union Box{I64 (*f)();U64 bits;};I64 Run(){Box b;b.f=&Add;U64 old=b.bits;b.bits=old;return b.f();}Run();|},
      "HCIRVM0024",
      "" );
    ( "partial callback byte overwrite invalidates owner",
      {|I64 Add(){return 42;}class Box{I64 (*f)();};I64 Run(){Box b;b.f=&Add;U8 *p=(&b)(U8*);p[3]=p[3];return b.f();}Run();|},
      "HCIRVM0024",
      "" );
    ( "callback field requires its selected signature",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)();};I64 Run(){Box b;b.f=&Add;return b.f();}Run();|},
      "HCIRVM0014",
      "" );
    ( "owned callback field rejects numeric update before further output",
      {|extern U0 Print(U8 *fmt,...);I64 Add(){return 42;}class Box{I64 (*f)();};I64 Run(){Box b;b.f=&Add;Print("kept");b.f+=0;Print("bad");return b.f();}Run();|},
      "HCIRVM0024",
      "kept" );
    ( "locked callback field update requires numeric ownership",
      {|I64 Add(){return 42;}class Box{I64 (*f)();};I64 Run(){Box b;b.f=&Add;lock b.f++;return 42;}Run();|},
      "HCIRVM0024",
      "" );
    ( "callback field array one-past window",
      {|class Box{I64 (*f)()[2];};I64 Run(){Box b;b.f[2]=17;return 42;}Run();|},
      "HCIRVM0019",
      "" );
  ]

let retained_values =
  [
    ( "callback field retains earlier class header",
      {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);};I64 Run(){Box b;b.f=&Add;return b.f(40);}class Box{U8 changed;};Run();|},
      42L,
      "" );
  ]

let quota_source =
  {|I64 Add(I64 n){return n+2;}class Box{I64 (*f)(I64 n);};I64 Run(){Box b;b.f=&Add;return b.f(40);}Run();|}

let proof_source =
  {|class Box{U8 pad;U8 (*f)();};I64 Run(){Box b;b.f=42;return b.f(I64);}Run();|}
