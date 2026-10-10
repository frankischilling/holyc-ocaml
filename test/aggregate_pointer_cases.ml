let example =
  {|extern U0 Print(U8 *fmt,...);
class Item{U8 text[3];U16 value;};
I64 Sum(Item *p){I64 n=p->value;p++;return n+p->value;}
I64 F(){Item a[2];Item *p=a;a[0].value=20;a[1].value=22;
p[1].text[0]=65;p[1].text[1]=66;p[1].text[2]=0;
Print("%s",(p+1)->text);return Sum(p);}F();|}

let quota_source =
  {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=a;
p++;p->value=40;p-=1;return (++p)->value+2;}F();|}

let quota_frame_bytes = 16

let proof_source =
  {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=a;
p[1].value=42;p++;return p->value;}
I64 G(){Box a[2];Box *p=a;p++;p->value=42;return p->value;}F();|}

let values =
  [
    ("class pointer example", example, 42L, "AB");
    ( "generic indexing retains the selected class stride",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=a;p[1].value=42;return p[1].value;}F();|},
      42L,
      "" );
    ( "parameter index and signed in-range reverse index",
      {|class Box{U8 tag;U16 value;};I64 Read(Box *p,I64 i){return p[i].value;}I64 F(){Box a[3];a[1].value=42;return Read(&a[2],-1);}F();|},
      42L,
      "" );
    ( "scaled addition, subtraction and all pointer comparisons",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[3];Box *p=a,*q=p+2;a[1].value=42;if(q-p!=2 || p-q!=-2 || !(p<q) || !(p<=q) || !(q>p) || !(q>=p) || p==q || !(p!=q))return -1;return (q-1)->value;}F();|},
      42L,
      "" );
    ( "postfix result retains its original descriptor",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[3];a[0].value=20;a[1].value=22;a[2].value=99;Box *p=a;I64 n=(p++)->value;n+=p->value;if((--p)->value!=20)return -1;if((++p)->value!=22)return -2;if((p--)->value!=22 || p->value!=20)return -3;p+=2;p-=1;if(p->value!=22)return -4;return n;}F();|},
      42L,
      "" );
    ( "compound update reads the pointer after its RHS",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[3];Box *p=&a[0];a[2].value=42;p+=((p=&a[1])==&a[1]);return p->value;}F();|},
      42L,
      "" );
    ( "compound RHS can initialize an unknown pointer",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[3];Box *p;a[2].value=42;p+=((p=&a[1])==&a[1]);return p->value;}F();|},
      42L,
      "" );
    ( "postfix member destination is captured before RHS rebinding",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[3];a[0].value=20;a[1].value=99;Box *p=a;(p++)->value+=((p=&a[2])==&a[2])*22;if(a[1].value!=99)return -1;return a[0].value;}F();|},
      42L,
      "" );
    ( "callee updates only its local pointer binding",
      {|class Box{U8 tag;U16 value;};I64 Read(Box *p){return (++p)->value;}I64 F(){Box a[2];a[0].value=20;a[1].value=22;Box *p=a;return Read(p)+p->value;}F();|},
      42L,
      "" );
    ( "pointer loop uses one-past comparisons",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[3];a[0].value=10;a[1].value=12;a[2].value=20;Box *p=a;I64 n=0;for(;p<a+3;p++)n+=p->value;return n;}F();|},
      42L,
      "" );
    ( "multidimensional root arithmetic uses class size",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2][3];a[0][1].value=42;Box *p=a+1;return p->value;}F();|},
      42L,
      "" );
    ( "selected row decay retains the root object",
      {|class Box{U8 tag;U16 value;};I64 Read(Box *p){return p[2].value;}I64 F(){Box a[2][3];a[1][2].value=42;return Read(a[1]);}F();|},
      42L,
      "" );
    ( "nested array stride is independent of containing object size",
      {|class Inner{U8 tag;U16 value;};class Box{U64 prefix;Inner a[3];U64 suffix;};I64 F(){Box b;Inner *p=b.a;b.a[2].value=42;return (p+2)->value;}F();|},
      42L,
      "" );
    ( "union pointee stride preserves overlap and neighboring objects",
      {|union Box{U64 word;U8 byte;};I64 F(){Box a[2];Box *p=a;a[0].word=99;p++;p->word=0x080706050403022a;p->byte+=2;if(a[0].word!=99)return -1;return p->word-0x0807060504030200;}F();|},
      44L,
      "" );
    ( "explicit padding is part of pointee size",
      {|class Box{U8 tag;$$=16;U16 value;};I64 F(){Box a[2];Box *p=a;p++;p->value=42;return a[1].value;}F();|},
      42L,
      "" );
    ( "backing class stride retains nominal member layout",
      {|U64 class Box{U8 value;};I64 F(){Box a[2];Box *p=a;p+=1;p->value=42;return a[1].value;}F();|},
      42L,
      "" );
    ( "later shadow cannot replace the function's selected pointee",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=a;p++;p->value=41;return p->value;}class Box{U8 value;};F()+sizeof(Box);|},
      42L,
      "" );
    ( "byte view shares class pointer writes",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=a;p++;p->value=42;U8 *q=p(U8*);return q[1]+q[2];}F();|},
      42L,
      "" );
    ( "class size one still retains pointee authority",
      {|class Box{U8 value;};I64 F(){Box a[2];Box *p=a,*q=p+1;q->value=42;if(q-p!=1)return -1;return (++p)->value;}F();|},
      42L,
      "" );
    ( "negative pointer arithmetic within the root",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[3];a[1].value=42;Box *p=&a[2];p+=-1;return p->value;}F();|},
      42L,
      "" );
    ( "postfix and compound pointer copies retain their results",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];a[0].value=20;a[1].value=22;Box *p=a,*q=p++;I64 n=q->value+p->value;q=(p-=1);if(q!=p || q->value!=20)return -1;return n;}F();|},
      42L,
      "" );
    ( "primitive multidimensional root arithmetic uses pointee size",
      {|I64 F(){U16 a[2][3];a[0][1]=42;U16 *p=a+1;return *p;}F();|},
      42L,
      "" );
  ]

let spellings = [ "Bool"; "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ]

let view_matrix =
  List.concat_map
    (fun spelling ->
      [
        ( "class pointer field width " ^ spelling,
          Printf.sprintf
            "class Box{U8 tag;%s value;};I64 F(){Box a[2];Box *p=a;%s \
             i=1;(++p)->value=40;p-=i;p+=i;p->value++;return \
             ++p[0].value;}F();"
            spelling spelling,
          42L,
          "" );
        ( "primitive pointer updates " ^ spelling,
          Printf.sprintf
            {|I64 F(){%s a[3];a[0]=20;a[1]=22;a[2]=99;%s *p=a;
I64 n=*p++;n+=*p;if(*--p!=20)return -1;if(*++p!=22)return -2;
if(*p--!=22 || *p!=20)return -3;p+=2;p-=1;if(*p!=22)return -4;
p=a;*p++=*p;if(a[0]!=20 || p!=&a[1])return -5;
p=a;*(p++)=*p;if(a[0]!=20 || p!=&a[1])return -9;
p=a;*p++=((p=&a[1])==&a[1])*42;if(a[0]!=20 || a[1]!=42 || p!=&a[2])return -6;
p=&a[1];U64 stored=(*p--=42);if(stored!=42 || p!=a)return -7;
%s *q;*q++=((q=a)==a)*42;if(q!=&a[1] || a[0]!=42)return -8;return n;}F();|}
            spelling spelling spelling,
          42L,
          "" );
      ])
    spellings

let faults =
  [
    ( "class pointer reads untouched bytes",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a[2];Box *p=a;a[0].value=42;p++;Print("kept");return p->value;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "class pointer one-past member access",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a[2];Box *p=a+2;Print("kept");return p->value;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "class pointer compound update exceeds root",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a[2];Box *p=a;Print("kept");p+=3;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "class pointer prefix decrement precedes root",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a[2];Box *p=a;Print("kept");--p;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "class pointer signed scale overflow",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=a;Print("kept");p+=0x7fffffffffffffff;return 0;}F();|},
      "HCIRVM0020",
      "kept" );
    ( "class pointer unsigned index is outside signed address range",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=a;U64 i=0xffffffffffffffff;Print("kept");return p[i].value;}F();|},
      "HCIRVM0020",
      "kept" );
    ( "unknown pointer update retains reached output",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box *p;Print("kept");p++;return 0;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "cross-object pointer difference",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a,b;Box *p=&a,*q=&b;Print("kept");return p-q;}F();|},
      "HCIRVM0018",
      "kept" );
    ( "cross-object pointer order",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a,b;Box *p=&a,*q=&b;Print("kept");return p<q;}F();|},
      "HCIRVM0018",
      "kept" );
  ]

let unsupported =
  [
    ( "forward pointee cannot borrow later completion",
      {|class Box;I64 Read(Box *p){return p[1].value;}class Box{U16 value;};I64 F(){Box a[2];a[1].value=42;return Read(a);}F();|}
    );
    ( "pointer arithmetic cannot borrow later completed size",
      {|class Box;I64 Read(Box *p){p++;return 42;}class Box{U16 value;};I64 F(){Box a[2];return Read(a);}F();|}
    );
    ( "class pointer update cannot manufacture integer ownership",
      {|class Box{U16 value;};I64 F(){Box *p=1(Box*);p++;return p->value;}F();|}
    );
    ( "whole pointee value remains separate",
      {|class Box{U16 value;};I64 F(){Box a[2];Box *p=a;p++;*p;return 42;}F();|}
    );
    ( "whole pointee assignment remains separate",
      {|class Box{U16 value;};I64 F(){Box a[2];Box *p=a;p[0]=p[1];return 42;}F();|}
    );
    ( "class pointer fields need per-field ownership",
      {|class Box{U16 value;};class Holder{Box *p;};I64 F(){Holder h;h.p++;return 42;}F();|}
    );
    ( "class pointer returns need caller lifetime handling",
      {|class Box{U16 value;};Box *Get(){Box a[2];return &a[1];}Get();|} );
  ]
