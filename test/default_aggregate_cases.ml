let example =
  {|class Word{U64 low;U8 guard;};
I64 F(){Word a[2];a[0].guard=7;a[1].guard=9;a[0]=20;a[1]=22;
Word *p=a;if(sizeof(Word)!=9 || (p+1)-p!=1)return -1;
if(a[0].guard!=7 || a[1].guard!=9)return -2;return *p+p[1];}F();|}

let quota_source = example
let quota_frame_bytes = 32

let proof_source =
  {|class Word{U8 tag;U64 low;};
I64 F(){Word a[2];Word *p=a;a[1].low=42;a[1]=42;p++;return *p;}
I64 G(){Word a[2];Word *p=a;a[1].low=42;a[1]=42;p++;return *p;}F();|}

let values =
  [
    ("default aggregate example", example, 42L, "");
    ( "ordinary class reads its initialized word",
      {|class Box{I64 word;};I64 F(){Box o;o.word=42;return o;}F();|},
      42L,
      "" );
    ( "ordinary class uses signed RT_PTR arithmetic",
      {|class Box{U64 word;};I64 F(){Box o;o=-8;if(o>1 || o/2!=-4 || (o>>1)!=-4)return -1;o=21;o<<=1;return o;}F();|},
      42L,
      "" );
    ( "default whole assignment preserves bytes after its word",
      {|class Box{U64 word;U8 guard;};I64 F(){Box a,b;a=42;a.guard=7;b.guard=9;b=a;if(b.guard!=9)return -1;return b;}F();|},
      42L,
      "" );
    ( "default union word aliases its byte array",
      {|union Box{U64 word;U8 bytes[8];};I64 F(){Box o;o=0;o.bytes[0]=42;return o;}F();|},
      42L,
      "" );
    ( "nested default array elements retain their root",
      {|class Word{U64 low;U8 guard;};class Root{U8 tag;Word words[2][2];};I64 F(){Root o;o.words[1][1].guard=9;o.words[1][1]=40;o.words[1][1]+=2;if(o.words[1][1].guard!=9)return -1;return o.words[1][1];}F();|},
      42L,
      "" );
    ( "default inherited class keeps its selected base layout",
      {|class Base{U64 low;};class Box:Base{U8 guard;};I64 F(){Box o;o.guard=9;o=42;if(o.low!=42 || o.guard!=9 || sizeof(Box)!=9)return -1;return o;}F();|},
      42L,
      "" );
    ( "inheritance does not inherit the base backing width",
      {|U16 class Base{U16 low;};class Box:Base{U8 tail[6];};I64 F(){Box o;o=-1;if(o.low!=65535 || o.tail[5]!=255 || o>1 || sizeof(Box)!=8)return -1;o=42;return o;}F();|},
      42L,
      "" );
    ( "backing chain ends at the original default class",
      {|class Base{U64 low;};Base class Box{U64 word;U8 guard;};U64 class Base{U64 replacement;};I64 F(){Box o;o.guard=9;o=-1;if(o>1 || o.guard!=9)return -1;o=42;return o;}F();|},
      42L,
      "" );
    ( "completed empty class supplies a default backing without storage",
      {|class Empty{};Empty class Box{U64 word;};I64 F(){Box o;o=-1;if(o>1)return -1;o=42;return o;}F();|},
      42L,
      "" );
    ( "equal-name replacement cannot alter a retained default value",
      {|class Box{U64 word;U8 guard;};I64 F(){Box o;o.guard=9;o=-1;if(o>1 || o.guard!=9 || sizeof(Box)!=9)return -1;o=42;return o;}U8 class Box{U8 byte;};F();|},
      42L,
      "" );
    ( "default value may reach adjacent bytes within a nested root",
      {|class Small{U8 first;};class Root{Small part;U8 tail[7];};I64 F(){Root o;o.part=0x010203040506072a;if(o.part.first!=42 || o.tail[0]!=7 || o.tail[6]!=1)return -1;o.part=42;return o.part;}F();|},
      42L,
      "" );
    ( "default class stride remains one inside a larger array root",
      {|class Small{U8 first;};I64 F(){Small a[8];Small *p=a;*p=42;if((p+1)-p!=1 || (p+1)->first!=0 || a[0].first!=42)return -1;return *p;}F();|},
      42L,
      "" );
    ( "default scalar updates normalize a signed word",
      {|class Box{U64 word;U8 guard;};I64 F(){Box o;o.guard=9;o=-4;if(o++!=-4 || --o!=-4 || o--!=-4 || ++o!=-4)return -1;o=7;o*=6;o/=2;o%=20;o+=42;o-=1;if(o.guard!=9)return -2;return o;}F();|},
      42L,
      "" );
    ( "default pointer lvalue increments the value",
      {|class Box{U64 word;U8 guard;};I64 F(){Box a[2];a[1]=41;Box *p=&a[1];return ++*p;}F();|},
      42L,
      "" );
    ( "default destination is captured before RHS rebinding",
      {|class Box{U64 word;};I64 F(){Box a[2];a[0]=20;a[1]=99;Box *p=a;*p+=((p=&a[1])==&a[1])*22;if(a[1]!=99)return -1;return a[0];}F();|},
      42L,
      "" );
    ( "default fused store uses the binding after the RHS",
      {|class Box{U64 word;U8 guard;};I64 F(){Box a[3];a[0]=9;a[1].guard=7;Box *p=a;I64 n=(*p++=((p=&a[1])==&a[1])*42);if(p!=&a[2] || a[0]!=9 || a[1].guard!=7 || n!=42)return -1;return a[1];}F();|},
      42L,
      "" );
    ( "grouped default fused decrement accepts RHS initialization",
      {|class Box{U64 word;U8 guard;};I64 F(){Box a[2];a[1].guard=9;Box *p;(*p--)=((p=&a[1])==&a[1])*42;if(p!=a || a[1].guard!=9)return -1;return a[1];}F();|},
      42L,
      "" );
    ( "default word aliases byte pointer writes",
      {|class Box{U64 word;};I64 F(){Box o;U8 *p=(&o)(U8*);I64 i=0;while(i<8){p[i]=0;i++;}p[0]=42;return o;}F();|},
      42L,
      "" );
    ( "default scalar supplies a primitive fixed argument",
      {|class Box{U64 word;};I64 Take(I64 n){return n+2;}I64 F(){Box o;o=40;return Take(o);}F();|},
      42L,
      "" );
    ( "default scalar supplies a pointer offset",
      {|class Count{U64 n;};class Box{U8 byte;};I64 F(){Count n;n=1;Box a[2];a[1].byte=42;Box *p=a;return (p+n)->byte;}F();|},
      42L,
      "" );
    ( "default variadic value retains its signed producer",
      {|extern U0 Print(U8 *fmt,...);class Box{U64 word;};I64 F(){Box o;o=-1;Print("%d",o);o=42;return o;}F();|},
      42L,
      "-1" );
    ( "default value participates in shared comparisons and conditions",
      {|class Box{U64 word;};I64 F(){Box o;o=42;if(0<o<43 && o && !(!o))return o;return -1;}F();|},
      42L,
      "" );
  ]

let faults =
  [
    ( "default word requires all eight bytes initialized",
      {|class Box{U64 word;};I64 F(){Box o;U8 *p=(&o)(U8*);p[0]=42;return o;}F();|},
      "HCIRVM0012",
      "" );
    ( "default store exceeds a one-byte standalone root",
      {|class Box{U8 byte;};I64 F(){Box o;o=42;return o.byte;}F();|},
      "HCIRVM0019",
      "" );
    ( "default read cannot use rounded frame padding",
      {|class Box{U8 bytes[7];};I64 F(){Box o;return o;}F();|},
      "HCIRVM0019",
      "" );
    ( "default read cannot spill from the last array element",
      {|class Box{U8 byte;};I64 F(){Box a[8];a[0]=42;return a[1];}F();|},
      "HCIRVM0019",
      "" );
    ( "default word rejects a one-past pointer",
      {|class Box{U64 word;};I64 F(){Box a[2];Box *p=a+2;return *p;}F();|},
      "HCIRVM0019",
      "" );
    ( "default word keeps an unknown pointer unknown",
      {|class Box{U64 word;};I64 F(){Box *p;return *p;}F();|},
      "HCIRVM0012",
      "" );
    ( "default whole values have fresh activation bytes",
      {|class Box{U64 word;};I64 once=0;I64 F(){Box o;if(once)return o;once=1;o=42;return o;}F();F();|},
      "HCIRVM0012",
      "" );
  ]

let unsupported =
  [
    ( "default class return ABI",
      {|class Box{U64 word;};Box F(){Box o;o=42;return o;}F();|} );
    ( "default persistent whole object",
      {|class Box{U64 word;};Box o;I64 F(){o=42;return o;}F();|} );
    ( "default whole postfix cast",
      {|class Box{U64 word;};I64 F(){Box o;o=42;return o(U8);}F();|} );
  ]
