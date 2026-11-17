let values =
  [
    ( "returned caller scalar retains its storage",
      {|I64 *Pass(I64 *p){return p;}I64 Run(){I64 n=42;return *Pass(&n);}Run();|},
      42L,
      "" );
    ( "returned class parameter retains caller fields",
      {|class Box{U8 pad;U16 value;};Box *Pass(Box *p){return p;}I64 Run(){Box b;b.value=42;return Pass(&b)->value;}Run();|},
      42L,
      "" );
    ( "returned class pointer supplies the original whole-value prefix",
      {|class Box{U8 bytes[8];};Box *Pass(Box *p){return p;}I64 Run(){Box b;b(U64)=42;return *Pass(&b);}Run();|},
      42L,
      "" );
    ( "returned backed class pointer keeps its scalar read width",
      {|U16 class Box{U16 value;};Box *Pass(Box *p){return p;}I64 Run(){Box b;b.value=42;return *Pass(&b);}Run();|},
      42L,
      "" );
    ( "returned inherited class retains its original offset",
      {|class Base{U8 pad;U16 value;};class Box:Base{U8 tail;};Box *Pass(Box *p){return p;}I64 Run(){Box b;b.value=42;return Pass(&b)->value;}Run();|},
      42L,
      "" );
    ( "returned root array preserves interior coordinates",
      {|I64 *Pick(I64 *p){return p+1;}I64 Run(){I64 a[2];a[1]=42;return *Pick(a);}Run();|},
      42L,
      "" );
    ( "returned class array keeps selected stride",
      {|class Box{U8 pad;U16 value;};Box *Pick(Box *p){return p+1;}I64 Run(){Box a[2];a[1].value=42;return Pick(a)->value;}Run();|},
      42L,
      "" );
    ( "returned member pointer keeps its containing root",
      {|class Box{U8 pad;I64 values[2];};I64 *Pick(Box *p){return &p->values[1];}I64 Run(){Box b;b.values[1]=42;return *Pick(&b);}Run();|},
      42L,
      "" );
    ( "returned global scalar survives callee retirement",
      {|I64 N=42;I64 *Get(){return &N;}I64 Run(){return *Get();}Run();|},
      42L,
      "" );
    ( "returned static scalar survives later calls",
      {|I64 *Get(){static I64 n=40;n++;return &n;}I64 Run(){I64 *p=Get();Get();return *p;}Run();|},
      42L,
      "" );
    ( "returned persistent class keeps original byte storage",
      {|class Box{U8 pad;U16 value;};Box b;Box *Get(){return &b;}I64 Run(){b.value=42;return Get()->value;}Run();|},
      42L,
      "" );
    ( "nested reference returns preserve the caller activation",
      {|I64 *Inner(I64 *p){return p;}I64 *Outer(I64 *p){return Inner(p);}I64 Run(){I64 n=42;return *Outer(&n);}Run();|},
      42L,
      "" );
    ( "callee-pop reference return preserves the captured descriptor",
      {|argpop I64 *Pass(I64 *p){return p;}I64 Run(){I64 n=42;return *Pass(&n);}Run();|},
      42L,
      "" );
    ( "caller-pop reference return preserves the captured descriptor",
      {|noargpop I64 *Pass(I64 *p){return p;}I64 Run(){I64 n=42;return *Pass(&n);}Run();|},
      42L,
      "" );
    ( "recursive reference returns snapshot each completion",
      {|I64 *Pass(I64 *p,I64 n){if(n)return Pass(p,n-1);return p;}I64 Run(){I64 n=42;return *Pass(&n,3);}Run();|},
      42L,
      "" );
    ( "variadic reference return retains its captured tail count",
      {|I64 *Pick(I64 *p,...){if(argc)return p+argv[0];return p;}I64 Run(){I64 a[2];a[1]=42;return *Pick(a,1);}Run();|},
      42L,
      "" );
    ( "argument evaluation snapshots earlier returned descriptors",
      {|I64 *Pass(I64 *p){return p;}I64 Add(I64 *a,I64 *b){return *a+*b;}I64 Run(){I64 a=19,b=23;return Add(Pass(&a),Pass(&b));}Run();|},
      42L,
      "" );
    ( "a returned pointer can enter an aggregate field",
      {|class Box{I64 *p;};I64 *Pass(I64 *p){return p;}I64 Run(){I64 n=42;Box b;b.p=Pass(&n);return *b.p;}Run();|},
      42L,
      "" );
    ( "a pointer field can supply an owned return",
      {|class Box{I64 *p;};I64 *Get(Box *b){return b->p;}I64 Run(){I64 n=42;Box b;b.p=&n;return *Get(&b);}Run();|},
      42L,
      "" );
    ( "returned string retains initialized literal bytes",
      {|U8 *Text(){return "AB";}I64 Run(){U8 *p=Text();return p[0]+p[1]-89;}Run();|},
      42L,
      "" );
    ( "one-past return remains movable within its original root",
      {|I64 *End(I64 *p){return p+1;}I64 Run(){I64 n=42;I64 *p=End(&n);return p[-1];}Run();|},
      42L,
      "" );
    ( "return conversion changes the view without extending storage",
      {|I64 *View(U8 *p){return p;}I64 Run(){U8 a[8];I64 i;for(i=0;i<8;i++)a[i]=0;a[0]=42;return *View(a);}Run();|},
      42L,
      "" );
    ( "class pointer return selects its declared nominal view",
      {|class Box{U8 value;};Box *View(I64 *p){return p;}I64 Run(){I64 n=42;return View(&n)->value;}Run();|},
      42L,
      "" );
    ( "callback invocation returns an owned caller pointer",
      {|I64 *Pass(I64 *p){return p;}I64 Run(){I64 n=42;I64 *(*f)(I64 *p);f=&Pass;return *f(&n);}Run();|},
      42L,
      "" );
    ( "callback field invocation returns an owned class pointer",
      {|class Value{U8 pad;U16 n;};Value *Pass(Value *p){return p;}class Box{Value *(*f)(Value *p);};I64 Run(){Value v;v.n=42;Box b;b.f=&Pass;return b.f(&v)->n;}Run();|},
      42L,
      "" );
  ]

let width_values =
  List.map
    (fun name ->
      ( "returned pointer to " ^ name,
        Printf.sprintf
          "%s *Pass(%s *p){return p;}I64 Run(){%s n=42;return *Pass(&n);}Run();"
          name name name,
        42L,
        "" ))
    [
      "Bool";
      "I8";
      "U8";
      "I16";
      "U16";
      "I32";
      "U32";
      "I64";
      "U64";
      "I8i";
      "U8i";
      "I16i";
      "U16i";
      "I32i";
      "U32i";
      "I64i";
      "U64i";
    ]

let faults =
  [
    ( "capturing an expired scalar return checks its lifetime",
      {|I64 *F(){I64 x;return &x;}F();|},
      "HCIRVM0018",
      "" );
    ( "capturing a returned callee address plus zero checks lifetime",
      {|I64 *Bad(){I64 a=42;return &a+0;}Bad();|},
      "HCIRVM0018",
      "" );
    ( "capturing a returned callee address minus zero checks lifetime",
      {|I64 *Bad(){I64 a=42;return &a-0;}Bad();|},
      "HCIRVM0018",
      "" );
    ( "capturing a returned class array element checks lifetime",
      {|class Box{U16 value;};Box *Get(){Box a[2];return &a[1];}Get();|},
      "HCIRVM0018",
      "" );
    ( "expired return faults before assignment and later output",
      {|extern U0 PutChars(U64 ch);I64 *Bad(){I64 n=42;return &n;}I64 Run(){I64 *p=Bad();PutChars('K');return *p;}Run();|},
      "HCIRVM0018",
      "" );
    ( "expired argument return preserves earlier argument effects",
      {|extern U0 PutChars(U64 ch);I64 *Bad(){I64 n=42;return &n;}I64 Side(){PutChars('K');return 0;}I64 Read(I64 *p,I64 n){return *p+n;}I64 Run(){return Read(Bad(),Side());}Run();|},
      "HCIRVM0018",
      "K" );
    ( "expired return faults before replacing a pointer field",
      {|extern U0 PutChars(U64 ch);class Box{I64 *p;};I64 *Bad(){I64 n=42;return &n;}I64 Run(){I64 n=41;Box b;b.p=&n;b.p=Bad();PutChars('K');return *b.p;}Run();|},
      "HCIRVM0018",
      "" );
    ( "returned scalar local expires before caller access",
      {|I64 *Bad(){I64 n=42;return &n;}I64 Run(){return *Bad();}Run();|},
      "HCIRVM0018",
      "" );
    ( "returned class local expires before member access",
      {|extern U0 PutChars(U64 ch);class Box{U8 value;};Box *Bad(){Box b;b.value=42;return &b;}I64 Run(){PutChars('K');Box *p=Bad();return p->value;}Run();|},
      "HCIRVM0018",
      "K" );
    ( "returned pointer preserves unknown caller bytes",
      {|I64 *Pass(I64 *p){return p;}I64 Run(){I64 n;return *Pass(&n);}Run();|},
      "HCIRVM0012",
      "" );
    ( "one-past returned pointer cannot supply bytes",
      {|I64 *End(I64 *p){return p+1;}I64 Run(){I64 n=42;return *End(&n);}Run();|},
      "HCIRVM0019",
      "" );
    ( "return conversion cannot enlarge an original byte root",
      {|I64 *View(U8 *p){return p;}I64 Run(){U8 a[1];a[0]=42;return *View(a);}Run();|},
      "HCIRVM0019",
      "" );
    ( "restored returned-pointer bytes retain no field ownership",
      {|I64 *Pass(I64 *p){return p;}union Box{I64 *p;U64 bits;};I64 Run(){I64 n=42;Box b;b.p=Pass(&n);U64 bits=b.bits;b.bits=bits;return *b.p;}Run();|},
      "HCIRVM0018",
      "" );
  ]

let retained_values =
  [
    ( "reference return retains the original class after replacement",
      {|class Box{U8 pad;U16 value;};Box *Pass(Box *p){return p;}I64 Run(){Box b;b.value=42;return Pass(&b)->value;}class Box{U64 changed;};Run();|},
      42L,
      "" );
  ]

let quota_source =
  {|I64 *Pass(I64 *p){return p;}I64 Run(){I64 n=42;return *Pass(&n);}Run();|}

let discarded_values =
  [
    ( "uninitialized global address can be returned without reading bytes",
      {|I64 N;I64 *Get(){return &N;}Get();|} );
    ( "live one-past global return can be discarded",
      {|I64 N[1];I64 *End(){return N+1;}End();|} );
    ( "uninitialized static address can be returned without reading bytes",
      {|I64 *Get(){static I64 n;return &n;}Get();|} );
    ( "returned literal address can be discarded",
      {|U8 *Get(){return "A";}Get();|} );
  ]

let unsupported =
  [
    ( "numeric return grants no pointer descriptor",
      {|I64 *Get(){return 42;}Get();|} );
    ( "null return needs the numeric pointer path",
      {|I64 *Get(){return 0;}Get();|} );
    ("multiple pointer return layers", {|I64 **Get(I64 **p){return p;}Get(0);|});
    ( "floating pointer execution remains separate",
      {|F64 *Get(F64 *p){return p;}Get(0);|} );
  ]
