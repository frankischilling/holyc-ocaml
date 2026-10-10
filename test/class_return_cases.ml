let example =
  {|U16 class Packet{U16 low;U8 guard;};
Packet Make(){return 0x07002a;}
Packet Read(Packet o){return o;}
I64 Take(Packet o){if(o.guard!=7 || Read(o)!=42)return -1;return o;}
Take(Make());|}

let quota_source =
  {|class Box{I64 word;};Box F(){U8 scratch[24];scratch[23]=42;return scratch[23];}F();|}

let quota_frame_bytes = 24

let warning_values =
  [
    ( "empty default class returns its register word with original warning",
      {|class Box{};Box F(){return 42;}F();|},
      42L,
      "" );
    ( "empty backed class returns its register word with original warning",
      {|U16 class Box{};Box F(){return 0x1002a;}F();|},
      0x1002aL,
      "" );
    ( "empty backing chain keeps its raw return and original warning",
      {|class Base{};Base class Box{};Box F(){return 42;}F();|},
      42L,
      "" );
  ]

let views = Class_parameter_cases.views

let view_matrix =
  List.concat_map
    (fun (backing, expected, _) ->
      [
        ( "class return " ^ backing ^ " preserves a register word",
          Printf.sprintf "%s class Box{U8 bytes[8];};Box F(){return -1;}F();"
            backing,
          -1L,
          "" );
        ( "class return " ^ backing ^ " reads the object prefix",
          Printf.sprintf
            "%s class Box{U8 bytes[8];};Box F(Box o){return o;}F(-1);" backing,
          Int64.of_string expected,
          "" );
      ])
    views

let values =
  [
    ("class return example", example, 42L, "");
    ( "default return register word",
      {|class Box{I64 word;};Box F(){return 42;}F();|},
      42L,
      "" );
    ( "default return from owned local object",
      {|class Box{U64 word;};Box F(){Box o;o=42;return o;}F();|},
      42L,
      "" );
    ( "backed return from owned local object",
      {|I64 class Box{I64 word;};Box F(){Box o;o=42;return o;}F();|},
      42L,
      "" );
    ( "default return from class parameter",
      {|class Box{I64 word;};Box F(Box o){return o;}F(42);|},
      42L,
      "" );
    ( "narrow return register retains bytes above backing width",
      {|U16 class Box{U16 low;U8 guard;};Box F(){return 0x07002a;}F();|},
      0x07002aL,
      "" );
    ( "narrow return object read excludes guard bytes",
      {|U16 class Box{U16 low;U8 guard;};Box F(Box o){return o;}F(0x07002a);|},
      42L,
      "" );
    ( "narrow assignment return preserves produced register word",
      {|U16 class Box{U16 low;U8 guard;};Box F(){Box o;o.guard=9;return o=0x07002a;}I64 Take(Box o){if(o.guard!=7)return -1;return o;}Take(F());|},
      42L,
      "" );
    ( "return word initializes only the receiving prefix",
      {|U16 class Box{U16 low;U8 guard;};Box F(){return 0x07002a;}I64 G(){Box o;o.guard=9;o=F();if(o.guard!=9)return -1;return o;}G();|},
      42L,
      "" );
    ( "register result arithmetic retains bits above narrow view",
      {|U8 class Box{U8 low;};Box F(){return 258;}F()/2;|},
      129L,
      "" );
    ( "signed default return drives signed comparison",
      {|class Box{U64 word;};Box F(){return -1;}if(F()<0)42;else -1;|},
      42L,
      "" );
    ( "unsigned backing drives unsigned comparison",
      {|U64 class Box{U64 word;};Box F(){return -1;}if(F()>0)42;else -1;|},
      42L,
      "" );
    ( "returned word reaches scalar fixed argument",
      {|U16 class Box{U16 low;};Box F(){return 0x1002a;}I64 Take(I64 n){return n-0x10000;}Take(F());|},
      42L,
      "" );
    ( "returned word reaches variadic tail",
      {|U16 class Box{U16 low;};Box F(){return 0x1002a;}I64 Take(...){if(argc!=1)return -1;return argv[0]-0x10000;}Take(F());|},
      42L,
      "" );
    ( "returned word reaches implicit output",
      {|extern U0 Print(U8 *fmt,...);U16 class Box{U16 low;};Box F(){return 0x1002a;}I64 G(){"%d",F();return 42;}G();|},
      42L,
      "65578" );
    ( "nested class returns retain full result words",
      {|U16 class Box{U16 low;U8 guard;};Box F(){return 0x07002a;}Box G(){return F();}I64 Take(Box o){if(o.guard!=7)return -1;return o;}Take(G());|},
      42L,
      "" );
    ( "grouping and unary plus keep the returned word view",
      {|U16 class Box{U16 low;U8 guard;};Box F(){return 0x07002a;}I64 Take(Box o){if(o.guard!=7)return -1;return o;}Take(+(F()));|},
      42L,
      "" );
    ( "class return recursion retains fresh activations",
      {|class Box{I64 word;};Box F(I64 n){if(!n)return 40;return F(n-1)+1;}F(2);|},
      42L,
      "" );
    ( "class result follows original no-argument-pop cleanup",
      {|class Box{I64 word;};noargpop Box F(I64 n){return n;}F(40)+F(2);|},
      42L,
      "" );
    ( "class result follows original argument-pop cleanup",
      {|class Box{I64 word;};argpop Box F(I64 n){return n;}F(40)+F(2);|},
      42L,
      "" );
    ( "class calls retain right-to-left argument evaluation",
      {|class Box{I64 word;};I64 order=0;Box F(I64 n){order=order*10+n;return n;}I64 Take(I64 a,I64 b){if(order!=21 || a!=1 || b!=2)return -1;return 42;}Take(F(1),F(2));|},
      42L,
      "" );
    ( "union return retains its selected raw view",
      {|U16 union Box{U16 low;U8 byte;};Box F(){return 0x1002a;}F()-0x10000;|},
      42L,
      "" );
    ( "inherited return keeps the derived default raw type",
      {|U8 class Base{U8 byte;};class Box:Base{U8 rest[7];};Box F(Box o){return o;}F(42);|},
      42L,
      "" );
    ( "return backing chain selects the original prefix",
      {|U16 class Base{U16 low;};Base class Box{U8 byte;};Box F(Box o){return o;}F(0x1002a);|},
      42L,
      "" );
    ( "return backing chain retains its selection after shadowing",
      {|U16 class Base{U16 low;};Base class Box{U8 byte;};U8 class Base{U8 other;};Box F(Box o){return o;}F(0x1002a);|},
      42L,
      "" );
    ( "large nominal return transports a single word",
      {|class Box{I64 word;U8 guard;};Box F(){return 42;}F();|},
      42L,
      "" );
    ( "return prefix reads initialized local bytes",
      {|U16 class Box{U16 low;U8 guard;};Box F(){Box o;o.low=42;return o;}F();|},
      42L,
      "" );
    ( "class result has no caller object copy",
      {|U16 class Box{U16 low;U8 guard;};Box F(Box o){o.guard=9;return o;}I64 G(){Box o;o.low=42;o.guard=7;if(F(o)!=42 || o.guard!=7)return -1;return o;}G();|},
      42L,
      "" );
    ( "different class return and argument views retain word transport",
      {|U8 class Tiny{U8 low;};U16 class Box{U16 low;U8 guard;};Tiny F(){return 0x07002a;}I64 Take(Box o){if(o.guard!=7)return -1;return o;}Take(F());|},
      42L,
      "" );
  ]

let faults =
  [
    ( "class return reads unknown prefix before completing the call",
      {|extern U0 Print(U8 *fmt,...);class Box{I64 word;};Box F(){Box o;"X";return o;}F();|},
      "HCIRVM0012",
      "X" );
    ( "class return cannot read beyond a small local root",
      {|class Box{U8 byte;};Box F(){Box o;o.byte=42;return o;}F();|},
      "HCIRVM0019",
      "" );
    ( "class return cannot read an unknown byte of its narrow prefix",
      {|U16 class Box{U8 bytes[2];};Box F(){Box o;o.bytes[0]=42;return o;}F();|},
      "HCIRVM0012",
      "" );
  ]

let unsupported =
  [
    ( "floating backing requires the F64 return convention",
      {|F64 class Box{F64 word;};Box F(){return 42.;}F();|} );
    ( "pointer backing requires separate pointer return ownership",
      {|I64* class Box{I64 word;};Box F(){return 42;}F();|} );
    ( "class return callback requires separate signature admission",
      {|class Box{I64 word;};Box F(){return 42;}Box (*p)();p=&F;p();|} );
    ( "class result has no addressable returned object",
      {|class Box{I64 word;};Box F(){return 42;}F().word;|} );
    ( "callback ownership cannot supply class return bytes",
      {|class Box{I64 word;};I64 Target(){return 42;}Box F(){I64 (*p)();p=&Target;return p;}F();|}
    );
    ( "data pointer cannot supply class return bytes",
      {|class Box{I64 word;};Box F(){I64 n=42;return &n;}F();|} );
  ]
