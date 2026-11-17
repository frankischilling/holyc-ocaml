let values =
  [
    ( "function-global",
      {|I64 n=0;U0 Fn(){n++;}class Box{U0 empty;I64 value;};Box b={&Fn,42};I64 F(){if(n)return -1;return b.value;}F();|},
      42L,
      "" );
    ( "function-static",
      {|I64 n=0;U0 Fn(){n++;}class Box{U0 empty;I64 value;};I64 F(){static Box b={&Fn,42};if(n)return -1;return b.value;}F();|},
      42L,
      "" );
    ( "data-global",
      {|I64 a=7;class Box{U0 empty;I64 value;};Box b={&a,42};I64 F(){return b.value;}F();|},
      42L,
      "" );
    ( "data-static",
      {|I64 a=7;class Box{U0 empty;I64 value;};I64 F(){static Box b={&a,42};return b.value;}F();|},
      42L,
      "" );
    ( "array",
      {|U8 a[2]={7,8};class Box{U0 empty;I64 value;};Box b={a,42};I64 F(){return b.value;}F();|},
      42L,
      "" );
    ( "string",
      {|class Box{U0 empty;I64 value;};Box b={"AB",42};I64 F(){return b.value;}F();|},
      42L,
      "" );
    ( "returned",
      {|I64 a=7,n=0;I64 *Get(){n++;return &a;}class Box{U0 empty;I64 value;};Box b={Get(),42};I64 F(){if(n!=1)return -1;return b.value;}F();|},
      42L,
      "" );
    ( "callback-word",
      {|I64 n=0;I64 Fn(){n++;return 7;}I64 (*p)()=&Fn;class Box{U0 empty;I64 value;};Box b={p,42};I64 F(){if(n)return -1;return b.value;}F();|},
      42L,
      "" );
    ( "returned-static",
      {|I64 a=7,n=0;I64 *Get(){n++;return &a;}class Box{U0 empty;I64 value;};I64 F(){static Box b={Get(),21};return b.value;}I64 Check(){if(n!=1)return -1;return F()+F();}Check();|},
      42L,
      "" );
    ( "pointee-Bool",
      {|Bool target=1;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=1)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-I8",
      {|I8 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-U8",
      {|U8 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-I16",
      {|I16 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-U16",
      {|U16 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-I32",
      {|I32 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-U32",
      {|U32 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-I64",
      {|I64 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "pointee-U64",
      {|U64 target=7;class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "class-address",
      {|class Item{I64 value;};Item target={7};class Box{U0 empty;I64 value;};Box object={&target,42};I64 F(){if(target.value!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "embedded-nul-string",
      {|class Box{U0 empty;I64 value;};Box object={"\0B",42};I64 F(){return object.value;}F();|},
      42L,
      "" );
    ( "nested-references",
      {|I64 target=7,n=0;U0 Fn(){n++;}class Inner{U0 first,second;};class Box{Inner inner;I64 value;};Box object={{&target,&Fn},42};I64 F(){if(n||target!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "empty-root",
      {|I64 target=7;class Empty{};Empty object=&target;I64 F(){return target+35;}F();|},
      42L,
      "" );
    ( "once-only-index",
      {|I64 values[2]={7,8},n=0;I64 Index(){n++;return 1;}class Box{U0 empty;I64 value;};Box object={&values[Index()],42};I64 F(){if(n!=1||values[1]!=8)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "returned-string",
      {|I64 n=0;U8 *Get(){n++;return "X";}class Box{U0 empty;I64 value;};Box object={Get(),42};I64 F(){if(n!=1)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "numeric-callback-word",
      {|I64 (*p)()=42;class Box{U0 empty;I64 value;};Box object={p,42};I64 F(){return object.value;}F();|},
      42L,
      "" );
    ( "static-string",
      {|class Box{U0 empty;I64 value;};I64 F(){static Box object={"AB",21};return object.value;}F()+F();|},
      42L,
      "" );
    ( "static-empty-root",
      {|I64 target=7;class Empty{};I64 F(){static Empty object=&target;return sizeof(object)+21;}F()+F();|},
      42L,
      "" );
  ]

let faults =
  [
    ( "numeric-address-is-not-ownership",
      {|class Box{U0 empty;I64 value;};Box object={42(I64*),42};42;|},
      "HCRUN0003",
      "" );
    ( "static-next-leaf-stays-unknown",
      {|I64 target=7;class Box{U0 empty;I64 value;};I64 F(){static Box object={&target,object.value};return object.value;}F();|},
      "HCIRVM0012",
      "" );
    ( "uninitialized-reference",
      {|class P{I64 *p;};P source;class Box{U0 empty;I64 value;};Box b={source.p,42};42;|},
      "HCIRVM0012",
      "" );
    ( "grown-destination",
      {|extern U0 Print(U8 *fmt,...);#exe{I64 target=7;extern class Item;class Box{Item item;$$=8;};I64 *Effect(){Print("A");StreamExePrint("class Item{U8 value;};");return &target;}Box object={Effect()};StreamPrint("42;");}|},
      "HCRUN0006",
      "A" );
    ( "next-leaf-stays-unknown",
      {|I64 target=7;class Box{U0 empty;I64 value;};Box object={&target,object.value};42;|},
      "HCIRVM0012",
      "" );
    ( "discard-does-not-authorize-call",
      {|I64 (*p)()=42;class Box{U0 empty;I64 value;};Box object={p,42};I64 F(){return p();}F();|},
      "HCIRVM0024",
      "" );
    ( "callee-failure-before-reference",
      {|extern U0 Print(U8 *fmt,...);I64 values[1]={7},z=0;I64 *Get(){Print("A");I64 q=1/z;return &values[q];}class Box{U0 empty;I64 value;};Box object={Get(),42};|},
      "HCIRVM0009",
      "A" );
  ]

let quota_source =
  {|U0 Fn(){}class Empty{};Empty first=&Fn;I64 answer=42;I64 F(){static Empty second=&Fn;return answer+sizeof(first)+sizeof(second);}F();|}
