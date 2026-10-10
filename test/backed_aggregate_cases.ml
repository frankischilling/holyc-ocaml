let example =
  {|U16 class Word{U16 low;U8 guard;};
I64 F(){Word a[2];a[0].guard=7;a[1].guard=9;a[0]=20;a[1]=22;
Word *p=a;if(sizeof(Word)!=3 || (p+1)-p!=1)return -1;
if(a[0].guard!=7 || a[1].guard!=9)return -2;return *p+p[1];}F();|}

let quota_source = example
let quota_frame_bytes = 16

let proof_source =
  {|U16 class Word{U8 tag;U16 low;};
I64 F(){Word a[2];Word *p=a;a[1].low=42;a[1]=42;p++;return *p;}
I64 G(){Word a[2];Word *p=a;a[1].low=42;a[1]=42;p++;return *p;}F();|}

let widths =
  [
    ("Bool", "-1");
    ("I8", "-1");
    ("U8", "255");
    ("I16", "-1");
    ("U16", "65535");
    ("I32", "-1");
    ("U32", "4294967295");
    ("I64", "-1");
    ("U64", "-1");
  ]

let values =
  List.map
    (fun (type_, minus_one) ->
      ( "whole backed scalar " ^ type_,
        Printf.sprintf
          {|%s class Box{%s word;};I64 F(){Box o;o=-1;if(o.word!=%s || o!=%s)return -1;o=41;return ++o;}F();|}
          type_ type_ minus_one minus_one,
        42L,
        "" ))
    widths
  @ [
      ("backed aggregate example", example, 42L, "");
      ( "whole copy writes the scalar prefix only",
        {|U16 class Box{U16 word;U8 guard;};I64 F(){Box a,b;a=42;a.guard=7;b.guard=9;b=a;if(b.guard!=9)return -1;return b;}F();|},
        42L,
        "" );
      ( "nested backed members retain absolute addresses",
        {|U16 class Word{U16 low;U8 guard;};class Box{U8 tag;Word words[2][2];};I64 F(){Box o;o.words[1][1].guard=9;o.words[1][1]=40;o.words[1][1]+=2;if(o.words[1][1].guard!=9)return -1;return o.words[1][1];}F();|},
        42L,
        "" );
      ( "inherited storage and backing are separate selections",
        {|class Base{U16 word;};U16 class Box:Base{U8 guard;};I64 F(){Box o;o.guard=9;o=42;if(o.word!=42 || o.guard!=9 || sizeof(Box)!=3)return -1;return o;}F();|},
        42L,
        "" );
      ( "whole union value overlaps its selected integer fields",
        {|U32 union Box{U32 word;U8 bytes[4];};I64 F(){Box o;o=0;o.bytes[0]=42;return o;}F();|},
        42L,
        "" );
      ( "backing chains preserve the originally selected class",
        {|U16 class Base{U16 low;};Base class Box{U16 word;U8 guard;};U8 class Base{U8 other;};I64 F(){Box o;o.guard=9;o=0x1234;if(o.word!=0x1234 || o.guard!=9)return -1;return o-0x120a;}F();|},
        42L,
        "" );
      ( "equal-size backing replacement cannot change signedness",
        {|I64 class Base{U64 word;};Base class Box{U64 word;};U64 class Base{U64 other;};I64 F(){Box o;o=-1;if(o>1)return -1;o=42;return o;}F();|},
        42L,
        "" );
      ( "signed backed arithmetic and pre/post updates",
        {|I16 class Box{I16 word;U8 guard;};I64 F(){Box o;o=-4;I64 old=o++;if(old!=-4 || o!=-3)return -1;if(--o!=-4 || o--!=-4 || ++o!=-4)return -2;o=7;o*=6;o/=2;o%=20;o+=42;o-=1;return o;}F();|},
        42L,
        "" );
      ( "unsigned backed comparisons and shifts",
        {|U64 class Box{U64 word;};I64 F(){Box o;o=-1;if(!(o>1) || o/2!=0x7fffffffffffffff)return -1;o=21;o<<=1;return o;}F();|},
        42L,
        "" );
      ( "whole destination remains captured across RHS rebinding",
        {|U16 class Box{U16 word;U8 guard;};I64 F(){Box a[2];a[0]=20;a[1]=99;Box *p=a;*p+=((p=&a[1])==&a[1])*22;if(a[1]!=99)return -1;return a[0];}F();|},
        42L,
        "" );
      ( "whole read aliases byte stores at the original root",
        {|U16 class Box{U16 word;U8 guard;};I64 F(){Box o;U8 *p=(&o)(U8*);p[0]=42;p[1]=0;return o;}F();|},
        42L,
        "" );
      ( "whole scalar may cross a nested class extent inside its root",
        {|U32 class Small{U8 first;};class Root{Small part;U8 tail[3];};I64 F(){Root o;o.part=42;if(o.part.first!=42 || o.tail[0]!=0 || o.tail[1]!=0 || o.tail[2]!=0)return -1;return o.part;}F();|},
        42L,
        "" );
      ( "backed lvalue pointer scalar updates",
        {|U16 class Box{U16 word;U8 guard;};I64 F(){Box a[2];a[1]=41;Box *p=&a[1];return ++*p;}F();|},
        42L,
        "" );
      ( "whole postfix store reads its binding after the RHS",
        {|U16 class Box{U16 word;U8 guard;};I64 F(){Box a[3];a[0]=9;a[1].guard=7;Box *p=a;I64 n=(*p++=((p=&a[1])==&a[1])*42);if(p!=&a[2] || a[0]!=9 || a[1].guard!=7 || n!=42)return -1;return a[1];}F();|},
        42L,
        "" );
      ( "grouped whole postfix store initializes before decrement",
        {|U16 class Box{U16 word;U8 guard;};I64 F(){Box a[2];Box *p;(*p--)=((p=&a[1])==&a[1])*42;if(p!=&a[0])return -1;return a[1];}F();|},
        42L,
        "" );
      ( "backed scalar enters an integer fixed argument",
        {|U16 class Box{U16 word;U8 guard;};I64 Take(I64 n){return n+2;}I64 F(){Box o;o=40;return Take(o);}F();|},
        42L,
        "" );
      ( "integer backing supplies a class pointer offset",
        {|U16 class Box{U16 word;U8 guard;};I64 class Count{I64 n;};I64 F(){Box a[2];Count step;step=1;a[1]=42;Box *p=a;return *(p+step);}F();|},
        42L,
        "" );
      ( "backed scalar retains its variadic producer class",
        {|extern U0 Print(U8 *fmt,...);U16 class Box{U16 word;U8 guard;};I64 F(){Box o;o=42;Print("%d",o);return o;}F();|},
        42L,
        "42" );
    ]

let faults =
  [
    ( "whole backing read requires every scalar byte initialized",
      {|U16 class Box{U16 word;U8 guard;};I64 F(){Box o;U8 *p=(&o)(U8*);p[0]=42;return o;}F();|},
      "HCIRVM0012",
      "" );
    ( "wide backing store exceeds a standalone root",
      {|U64 class Box{U8 byte;};I64 F(){Box o;o=42;return o.byte;}F();|},
      "HCIRVM0019",
      "" );
    ( "whole backing read rejects one-past class pointers",
      {|U16 class Box{U16 word;U8 guard;};I64 F(){Box a[2];Box *p=a+2;return *p;}F();|},
      "HCIRVM0019",
      "" );
    ( "whole backing view keeps an unknown pointer unknown",
      {|U16 class Box{U16 word;};I64 F(){Box *p;return *p;}F();|},
      "HCIRVM0012",
      "" );
  ]

let unsupported =
  [
    ( "floating whole-value arithmetic execution",
      {|U16 class Box{U16 word;U8 guard;};I64 F(){Box o;o=40;return o+2.;}F();|}
    );
    ( "floating fixed parameter storage",
      {|U16 class Box{U16 word;U8 guard;};I64 Take(F64 n){return n+2.;}I64 F(){Box o;o=40;return Take(o);}F();|}
    );
    ( "F64 class backing",
      {|F64 class Box{F64 word;};I64 F(){Box o;o=42;return o;}F();|} );
    ( "pointer class backing",
      {|U8 *class Box{U8 *word;};I64 F(){Box o;o=0;return o;}F();|} );
    ( "unbacked whole class value",
      {|class Box{I64 word;};I64 F(){Box o;o.word=42;return o;}F();|} );
    ( "backed class parameter ABI",
      {|I64 class Box{I64 word;};I64 Take(Box o){return o;}I64 F(){Box o;o=42;return Take(o);}F();|}
    );
    ( "backed class return ABI",
      {|I64 class Box{I64 word;};Box F(){Box o;o=42;return o;}F();|} );
    ( "backed persistent class object",
      {|I64 class Box{I64 word;};Box o;I64 F(){o=42;return o;}F();|} );
    ( "backed whole postfix cast",
      {|I64 class Box{I64 word;};I64 F(){Box o;o=42;return o(U8);}F();|} );
  ]
