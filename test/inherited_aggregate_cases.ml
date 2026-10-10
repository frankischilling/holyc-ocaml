let example =
  {|extern U0 Print(U8 *fmt,...);
class Base{U8 text[3];};class Item:Base{U16 value;};
I64 Sum(Item *p){I64 n=p->value;p++;return n+p->value;}
I64 F(){Item a[2];Item *p=a;a[0].value=20;a[1].value=22;
p[1].text[0]=65;p[1].text[1]=66;p[1].text[2]=0;
Print("%s",(p+1)->text);return Sum(p);}F();|}

let quota_source =
  {|class Base{U8 tag;U16 value;};class Box:Base{};
I64 F(){Box a[2];Box *p=a;p++;p->value=40;p-=1;
return (++p)->value+2;}F();|}

let quota_frame_bytes = 16

let lookahead_source =
  {|class B{U8 tag;};class Box:B #exe{class B{U8 other;};}{U16 value;};I64 F(){Box o;o.tag=20;o.value=22;return o.tag+o.value;}F();|}

let proof_source =
  {|class Base{U8 tag;U16 value;};class Box:Base{};
I64 F(){Box a[2];Box *p=a;p[1].value=42;p++;return p->value;}
I64 G(){Box a[2];Box *p=a;p++;p->value=42;return p->value;}F();|}

let values =
  [
    ("inherited aggregate example", example, 42L, "AB");
    ( "base prefix and packed child field",
      {|class B{U8 tag;};class Box:B{U16 value;};I64 F(){Box o;o.tag=20;o.value=22;U8 *p=(&o)(U8*);if(p[0]!=20 || p[1]!=22 || p[2]!=0 || sizeof(Box)!=3)return -1;return o.tag+o.value;}F();|},
      42L,
      "" );
    ( "three levels retain absolute inherited offsets",
      {|class A{U8 first;};class B:A{U16 second;};class Box:B{U32 third;};I64 F(){Box o;o.first=10;o.second=12;o.third=20;Box *p=&o;if(sizeof(Box)!=7)return -1;return p->first+p->second+p->third;}F();|},
      42L,
      "" );
    ( "empty child keeps inherited array dimensions",
      {|class B{U16 words[2][3];};class Box:B{};I64 F(){Box o;o.words[1][2]=42;return o.words[1][2];}F();|},
      42L,
      "" );
    ( "union child overlaps inherited fields at zero",
      {|class B{U64 word;};union Box:B{U16 half;U8 bytes[8];};I64 F(){Box o;o.word=0;o.half=0x1234;o.bytes[0]=42;if(o.word!=0x122a || sizeof(Box)!=8)return -1;return o.bytes[0];}F();|},
      42L,
      "" );
    ( "union base and larger union child",
      {|union B{U16 word;U8 bytes[2];};union Box:B{U8 tail[5];};I64 F(){Box o;o.word=20;o.tail[4]=22;if(sizeof(Box)!=5)return -1;return o.bytes[0]+o.tail[4];}F();|},
      42L,
      "" );
    ( "nested inherited objects and aggregate arrays",
      {|class A{U8 tag;};class Item:A{U16 value;};class B{Item items[2][2];};class Box:B{U8 tail;};I64 F(){Box o;o.items[1][1].tag=20;o.items[1][1].value=22;o.tail=9;return o.items[1][1].tag+o.items[1][1].value;}F();|},
      42L,
      "" );
    ( "multidimensional roots and inherited member row decay",
      {|class B{U16 words[2][3];};class Box:B{U8 tag;};I64 F(){Box a[2][2];a[1][1].words[1][2]=42;U16 *p=a[1][1].words[1];if(sizeof(Box)!=13)return -1;return p[2];}F();|},
      42L,
      "" );
    ( "inherited generic strides and pointer comparisons",
      {|class B{U8 tag;};class Box:B{U16 value;};I64 F(){Box a[3];Box *p=a,*q=p+2;a[1].value=42;if(q-p!=2 || p-q!=-2 || !(p<q) || !(p<=q) || !(q>p) || !(q>=p) || p==q || !(p!=q))return -1;return (q-1)->value;}F();|},
      42L,
      "" );
    ( "inherited postfix snapshot and compound updates",
      {|class B{U8 tag;U16 value;};class Box:B{};I64 F(){Box a[3];a[0].value=20;a[1].value=22;a[2].value=99;Box *p=a;I64 n=(p++)->value;n+=p->value;if((--p)->value!=20 || (++p)->value!=22)return -1;if((p--)->value!=22 || p->value!=20)return -2;p+=2;p-=1;return n;}F();|},
      42L,
      "" );
    ( "inherited destination remains captured across RHS rebinding",
      {|class B{U16 value;};class Box:B{U8 tag;};I64 F(){Box a[3];a[0].value=20;a[1].value=99;Box *p=a;(p++)->value+=((p=&a[2])==&a[2])*22;if(a[1].value!=99)return -1;return a[0].value;}F();|},
      42L,
      "" );
    ( "compound pointer update reads binding after RHS",
      {|class B{U8 tag;U16 value;};class Box:B{};I64 F(){Box a[3];Box *p;a[2].value=42;p+=((p=&a[1])==&a[1]);return p->value;}F();|},
      42L,
      "" );
    ( "callee owns a copy of the derived pointer binding",
      {|class B{U8 tag;U16 value;};class Box:B{};I64 Read(Box *p,I64 n){p+=n;return p->value;}I64 F(){Box a[2];Box *p=a;a[1].value=42;I64 n=Read(p,1);if(p!=a)return -1;return n;}F();|},
      42L,
      "" );
    ( "old base survives later spelling replacement",
      {|class B{U8 tag;};class Box:B{U16 value;};class B{U64 other;};I64 F(){Box o;o.tag=20;o.value=22;if(sizeof(Box)!=3)return -1;return o.tag+o.value;}F();|},
      42L,
      "" );
    ( "same-size base replacement cannot replace selected members",
      {|class B{I8 signed_byte;};class Box:B{U16 value;};class B{U8 unsigned_byte;};I64 F(){Box o;o.signed_byte=-2;o.value=44;return o.signed_byte+o.value;}F();|},
      42L,
      "" );
    ( "old derived pointer survives both redefinitions",
      {|class B{U8 tag;};class Box:B{U16 value;};I64 Read(Box *p){return p[1].tag+p[1].value;}I64 F(){Box a[2];a[1].tag=20;a[1].value=22;return Read(a);}class B{U64 other;};class Box:B{U64 other2;};F();|},
      42L,
      "" );
    ( "completed forward base and derived identity",
      {|extern class B;extern class Box;class B{U8 tag;};class Box:B{U16 value;};I64 F(){Box o;o.tag=20;o.value=22;return o.tag+o.value;}F();|},
      42L,
      "" );
    ( "allowed duplicate selects child pad first",
      {|class B{U8 pad;};class Box:B{U8 pad;};I64 F(){Box o;U8 *p=(&o)(U8*);p[0]=20;o.pad=22;if(p[1]!=22)return -1;return p[0]+o.pad;}F();|},
      42L,
      "" );
    ( "explicit padding is owned once across inherited roots",
      {|class B{U8 tag;$$=8;U16 value;};class Box:B{U8 tail;};I64 F(){Box a[2];U8 *p=(&a[0])(U8*);p[21]=42;a[1].value=20;a[1].tail=22;if(sizeof(Box)!=11 || p[19]!=20)return -1;return a[1].value+a[1].tail;}F();|},
      42L,
      "" );
    ( "primitive backing keeps inherited member identities",
      {|class B{U8 tag;};U64 class Box:B{U16 value;};I64 F(){Box a[2];Box *p=a;p[1].tag=20;p[1].value=22;if(sizeof(Box)!=3)return -1;return p[1].tag+p[1].value;}F();|},
      42L,
      "" );
    ( "inherited dimensions keep original sizeof preparation",
      {|class Size{U8 bytes[2];};class B{U16 words[sizeof(Size)];};class Box:B{U8 tag;};class Size{U8 byte;};I64 F(){Box o;o.words[1]=42;if(sizeof(Box)!=5)return -1;return o.words[1];}F();|},
      42L,
      "" );
  ]

let widths = [ "Bool"; "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ]

let view_matrix =
  List.map
    (fun type_ ->
      ( "inherited field width and update " ^ type_,
        Printf.sprintf
          {|class B{U8 tag;%s value;};class Box:B{U8 tail;};I64 F(){Box o;o.tag=99;o.tail=88;o.value=20;I64 n=o.value++;n+=o.value;o.value+=1;if(o.tag!=99 || o.tail!=88 || o.value!=22)return -1;return n+1;}F();|}
          type_,
        42L,
        "" ))
    widths

let faults =
  [
    ( "inherited unknown bytes",
      {|class B{U16 value;};class Box:B{U8 tag;};I64 F(){Box o;o.tag=42;return o.value;}F();|},
      "HCIRVM0012",
      "" );
    ( "inherited one-past member",
      {|extern U0 Print(U8 *fmt,...);class B{U8 tag;};class Box:B{U16 value;};I64 F(){Box a[2];Box *p=a+2;Print("kept");p->tag=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "inherited array extent",
      {|extern U0 Print(U8 *fmt,...);class B{U16 words[2];};class Box:B{};I64 F(){Box o;Print("kept");o.words[2]=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "child offset can shrink below inherited field width",
      {|extern U0 Print(U8 *fmt,...);class B{U64 value;};class Box:B{$$=1;U8 tail;};I64 F(){Box o;Print("kept");o.value=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "inherited negative offset is outside owned bytes",
      {|extern U0 Print(U8 *fmt,...);class B{$$=-2;U8 value[4];};class Box:B{U8 tail;};I64 F(){Box o;Print("kept");o.value[0]=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "inherited cross-object difference",
      {|extern U0 Print(U8 *fmt,...);class B{U16 value;};class Box:B{U8 tag;};I64 F(){Box a,b;Print("kept");return &a-&b;}F();|},
      "HCIRVM0018",
      "kept" );
    ( "inherited pointer scale overflow",
      {|extern U0 Print(U8 *fmt,...);class B{U8 tag;};class Box:B{U16 value;};I64 F(){Box a[2];Box *p=a;Print("kept");p+=0x7fffffffffffffff;return 0;}F();|},
      "HCIRVM0020",
      "kept" );
    ( "inherited fresh activation has unknown bytes",
      {|class B{U16 value;};class Box:B{};I64 once=0;I64 F(){Box o;if(once)return o.value;once=1;o.value=42;return o.value;}F();F();|},
      "HCIRVM0012",
      "" );
  ]

let retained_values =
  [
    ("original base survives retained lookahead", lookahead_source, 42L, "");
    ( "retained runtime base keeps its saved dimension",
      {|I64 Count=0;I64 Next(){Count++;return 2;}class B{U16 words[Next()];};class Box:B{U8 tag;};I64 F(){Box o;o.words[1]=41;return o.words[1]+Count;}F();|},
      42L,
      "" );
  ]

let source_unsupported =
  [
    ( "runtime-prepared base outside the current object compilation",
      {|I64 Count=0;I64 Next(){Count++;return 2;}class B{U16 words[Next()];};class Box:B{U8 tag;};I64 F(){Box o;o.words[1]=41;return o.words[1]+Count;}F();|}
    );
    ( "forward base completion cannot supply later storage",
      {|extern class B;class Box:B{U8 tag;};class B{U16 value;};I64 F(){Box o;o.tag=42;return o.tag;}F();|}
    );
    ( "self base has metadata without an acyclic object layout",
      {|class Box:Box{U8 tag;};I64 F(){Box o;o.tag=42;return o.tag;}F();|} );
    ( "inherited duplicate ordinary member",
      {|class B{U8 value;};class Box:B{U16 value;};I64 F(){Box o;o.value=42;return o.value;}F();|}
    );
    ( "derived pointer cannot silently become a base pointer",
      {|class B{U8 tag;};class Box:B{U16 value;};I64 Read(B *p){return p->tag;}I64 F(){Box o;o.tag=42;return Read(&o);}F();|}
    );
    ( "whole inherited object copy",
      {|class B{U8 tag;};class Box:B{U16 value;};I64 F(){Box a,b=a;return 42;}F();|}
    );
    ( "persistent inherited object",
      {|class B{U8 tag;};class Box:B{U16 value;};Box o;42;|} );
  ]

let unsupported = List.filteri (fun index _ -> index > 0) source_unsupported
