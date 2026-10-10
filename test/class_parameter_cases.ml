let example =
  {|U16 class Packet{U16 low;U8 guard;};
I64 Take(Packet o,I64 n){
if(sizeof(Packet)!=3 || o.low!=33 || o.guard!=7 || n!=9)return -1;
o.guard=11;o+=0x10000;if(o.guard!=11)return -2;return o+n;}
Take(0x070021,9);|}

let quota_source =
  {|class Word{I64 word;};I64 Take(Word o){U8 scratch[16];scratch[15]=2;return o+scratch[15];}Take(40);|}

let quota_frame_bytes = 24

let proof_source =
  {|U16 class Box{U16 low;U8 guard;};
I64 F(Box o){o.guard=9;return o+o.guard;}
I64 G(Box o){o.guard=9;return o+o.guard;}F(33);|}

let views =
  [
    ("Bool", "-1", 1);
    ("I8", "-1", 1);
    ("U8", "255", 1);
    ("I16", "-1", 2);
    ("U16", "65535", 2);
    ("I32", "-1", 4);
    ("U32", "4294967295", 4);
    ("I64", "-1", 8);
    ("U64", "0xffffffffffffffff", 8);
  ]

let view_matrix =
  List.map
    (fun (backing, expected, bytes) ->
      ( "class parameter " ^ backing ^ " view retains full incoming word",
        Printf.sprintf
          "%s class Box{U8 bytes[8];};I64 Take(Box o,I64 n){if(o!=%s || \
           o.bytes[7]!=255 || n!=2)return -1;o=40;if(o!=40 || \
           o.bytes[0]!=40)return -2;%sreturn o+n;}Take(-1,2);"
          backing expected
          (if bytes < 8 then
             Printf.sprintf "if(o.bytes[%d]!=255)return -3;" bytes
           else ""),
        42L,
        "" ))
    views

let values =
  [
    ("class parameter example", example, 42L, "");
    ( "default parameter whole value",
      {|class Box{U64 word;};I64 Take(Box o){return o;}I64 F(){Box o;o=42;return Take(o);}F();|},
      42L,
      "" );
    ( "backed parameter whole value",
      {|I64 class Box{I64 word;};I64 Take(Box o){return o;}I64 F(){Box o;o=42;return Take(o);}F();|},
      42L,
      "" );
    ( "one-byte default class receives an eight-byte slot",
      {|class Box{U8 byte;};I64 Take(Box o){if(sizeof(Box)!=1 || o.byte!=42)return -1;o+=2;return o-2;}Take(42);|},
      42L,
      "" );
    ( "parameter pointer stride uses nominal size",
      {|class Box{U8 byte;};I64 Take(Box o){Box *p=&o;if(sizeof(Box)!=1 || (p+7)-p!=7 || p[7].byte!=42)return -1;return p[7].byte;}Take(0x2a00000000000000);|},
      42L,
      "" );
    ( "narrow view may cross a one-byte class inside its slot",
      {|U16 class Box{U8 byte;};I64 Take(Box o){Box *p=&o;p[6]=42;return p[6];}Take(0);|},
      42L,
      "" );
    ( "empty class still receives the argument word",
      {|class Box{};I64 Take(Box o){if(sizeof(Box)!=0)return -1;o++;return o;}Take(41);|},
      42L,
      "" );
    ( "empty explicitly backed class reads its parameter prefix",
      {|U16 class Box{};I64 Take(Box o){return o;}Take(42);|},
      42L,
      "" );
    ( "large nominal class reads only its word",
      {|class Box{I64 word;U8 guard;};I64 Take(Box o){if(sizeof(Box)!=9)return -1;return o;}Take(42);|},
      42L,
      "" );
    ( "class and scalar parameters retain their original positions",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(I64 a,Box b,I64 c,Box d){if(a!=1 || b.low!=20 || b.guard!=7 || c!=2 || d.low!=19 || d.guard!=8)return -1;return a+b+c+d;}Take(1,0x070014,2,0x080013);|},
      42L,
      "" );
    ( "parameter writes leave caller storage intact",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o){o=99;o.guard=9;return o;}I64 F(){Box o;o=42;o.guard=7;Take(o);if(o.guard!=7)return -1;return o;}F();|},
      42L,
      "" );
    ( "passing a narrow class reads its prefix before transport",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o){if(o.guard!=0)return -1;return o;}I64 F(){Box o;o=42;o.guard=7;return Take(o);}F();|},
      42L,
      "" );
    ( "unsigned word arguments retain all eight bytes",
      {|U8 class Box{U8 bytes[8];};I64 Take(Box o){if(o.bytes[7]!=42)return -1;o=42;return o;}I64 F(){U64 n=0x2a00000000000000;return Take(n);}F();|},
      42L,
      "" );
    ( "assignment expression preserves full bits at the call",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o){if(o.low!=42 || o.guard!=7)return -1;return o;}I64 F(){Box o;return Take(o=0x07002a);}F();|},
      42L,
      "" );
    ( "union parameter shares its initialized bytes",
      {|U16 union Box{U16 low;U8 bytes[8];};I64 Take(Box o){if(o.bytes[7]!=42)return -1;o.bytes[0]=40;o+=2;return o;}Take(0x2a00000000000000);|},
      42L,
      "" );
    ( "inherited parameter retains layout and default signed word",
      {|U16 class Base{U16 low;};class Box:Base{U8 tail[6];};I64 Take(Box o){if(o!=-1 || o.low!=65535 || o.tail[5]!=255)return -1;o=42;return o;}Take(-1);|},
      42L,
      "" );
    ( "forwarded backing keeps its original chain",
      {|U16 class Base{U16 low;};Base class Box{U16 low;U8 guard;};I64 Take(Box o){if(o.guard!=7)return -1;return o;}Take(0x07002a);|},
      42L,
      "" );
    ( "later same-name class does not replace a parameter view",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o){if(sizeof(Box)!=3 || o.guard!=7)return -1;return o;}U8 class Box{U8 low;};Take(0x07002a);|},
      42L,
      "" );
    ( "nested calls use separate initialized slots",
      {|U16 class Box{U16 low;U8 guard;};I64 B(Box o,I64 n){o.guard=9;return o+n;}I64 A(Box o){if(B(20,2)!=22 || o.guard!=7)return -1;return B(o,2);}A(0x070028);|},
      42L,
      "" );
    ( "class argument words preserve right-to-left effects",
      {|U16 class Box{U16 low;U8 guard;};I64 n=0;I64 Next(){n++;return 0x070000+n;}I64 Take(Box a,Box b){if(a.low!=2 || b.low!=1 || a.guard!=7 || b.guard!=7)return -1;return 39+a+b;}Take(Next(),Next());|},
      42L,
      "" );
    ( "U0 source functions receive the same class slot",
      {|I64 n=0;U16 class Box{U16 low;U8 guard;};U0 Take(Box o){n=o+o.guard;}Take(0x090021);n;|},
      42L,
      "" );
    ( "recursive class arguments have fresh storage",
      {|U16 class Box{U16 low;U8 guard;};I64 F(Box o,I64 n){if(!n)return o;if(o.guard!=7)return -1;o.guard=9;return F(0x07002a,n-1);}F(0x07002a,3);|},
      42L,
      "" );
    ( "noargpop preserves class argument cleanup",
      {|class Box{I64 word;};noargpop I64 Take(Box o,I64 n){return o+n;}Take(40,2);|},
      42L,
      "" );
    ( "class parameter precedes variadic count and tail",
      {|class Box{I64 word;};I64 Take(Box o,...){return o+argc+argv[0]-argv[1];}Take(40,1,1);|},
      42L,
      "" );
    ( "class parameter updates retain initialized neighbor bytes",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o){I64 a=o++;if(a!=40 || o!=41 || o.guard!=7)return -1;return ++o;}Take(0x070028);|},
      42L,
      "" );
    ( "parameter address aliases whole and member writes",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o){Box *p=&o;p->guard=9;*p=33;return o+o.guard;}Take(0);|},
      42L,
      "" );
    ("eight-byte class slot participates in frame limits", quota_source, 42L, "");
  ]

let faults =
  [
    ( "ninth class byte cannot borrow the next parameter",
      {|extern U0 Print(U8 *format,...);class Box{I64 word;U8 guard;};I64 Take(Box o,I64 next){if(next!=99)return -1;Print("AB");return o.guard;}Take(42,99);|},
      "HCIRVM0019",
      "AB" );
    ( "narrow prefix cannot cross the eight-byte argument root",
      {|U16 class Box{U8 byte;};I64 Take(Box o){Box *p=&o;return p[7];}Take(0);|},
      "HCIRVM0019",
      "" );
    ( "one-past parameter member access is outside its root",
      {|class Box{U8 byte;};I64 Take(Box o){Box *p=&o;return p[8].byte;}Take(0);|},
      "HCIRVM0019",
      "" );
    ( "passing an unknown class prefix faults before entry",
      {|extern U0 Print(U8 *format,...);U16 class Box{U16 low;};I64 Take(Box o){Print("B");return o;}I64 F(){Box o;Print("A");return Take(o);}F();|},
      "HCIRVM0012",
      "A" );
  ]

let unsupported =
  [
    ("class return ABI", {|class Box{I64 word;};Box F(Box o){return o;}F(42);|});
    ( "floating class parameter ABI",
      {|F64 class Box{F64 word;};I64 Take(Box o){return o.word;}Take(42);|} );
    ( "pointer-backed class parameter ABI",
      {|I64* class Box{I64 word;};I64 Take(Box o){return o.word;}Take(42);|} );
    ( "persistent class parameter copy",
      {|class Box{I64 word;};Box saved;I64 Take(Box o){saved=o;return o;}Take(42);|}
    );
    ( "class parameter default requires separate declaration preparation",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o=0x07002a){return o;}Take();|}
    );
    ( "class callback parameter requires separate signature admission",
      {|class Box{I64 word;};I64 Take(Box o){return o;}I64 (*p)(Box o);p=&Take;p(42);|}
    );
    ( "empty automatic class has no argument slot authority",
      {|class Box{};I64 F(){Box o;o=42;return o;}F();|} );
    ( "callback ownership cannot initialize class bytes",
      {|class Box{I64 word;};I64 Target(){return 42;}I64 Take(Box o){return o;}I64 F(){I64 (*p)();p=&Target;return Take(p);}F();|}
    );
  ]
