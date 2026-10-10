let example =
  {|extern U0 Print(U8 *fmt,...);
class Packed{U8 text[3];U16 value;};
I64 Bump(Packed *p){return ++p->value;}
I64 F(){Packed object;Packed *p=&object;
p->text[0]=65;p->text[1]=66;p->text[2]=0;Print("%s",p->text);
object.value=40;Bump(p);return Bump(&object);}F();|}

let quota_source =
  {|class Box{U8 bytes[9];};I64 F(){Box object;U8 *p=&object.bytes[8];
*p=40;object.bytes[8]+=2;return *p;}F();|}

let pointer_example =
  {|extern U0 Print(U8 *fmt,...);
class Item{U16 value;};
class Box{U8 *text;Item *items[2];};
I64 Bump(Item *p){return ++p->value;}
I64 F(){Box o;U8 text[3];Item item;
text[0]=65;text[1]=66;text[2]=0;item.value=40;
o.text=text;o.items[1]=&item;Print("%s",o.text);
Bump(o.items[1]);return Bump(o.items[1]);}F();|}

let pointer_quota_source =
  {|class Box{U16 *p;};I64 F(){Box o;U16 values[2];values[1]=42;
o.p=values;o.p++;return *o.p;}F();|}

let pointer_proof_source =
  {|class Box{U8 tag;U8 *p[2][3];};I64 F(){Box o;U8 value=42;
o.p[1][2]=&value;return *o.p[1][2];}F();|}

let pointer_local_proof_source =
  {|I64 F(){class Box{U8 tag;U8 *p[2][3];};Box o;U8 value=42;
o.p[1][2]=&value;return *o.p[1][2];}F();|}

let pointer_values =
  [
    ( "function-local pointer fields retain their original projection",
      pointer_local_proof_source,
      42L,
      "" );
    ( "function-local class pointer field uses its selected stride",
      {|I64 F(){class Item{U8 tag;U16 value;};class Box{Item *p;};
Item items[2];Box box;items[1].value=42;box.p=items;box.p++;
return box.p->value;}F();|},
      42L,
      "" );
    ( "pointer field execution combines with builtin member size queries",
      {|class Box{U16 *p;U64 bits;};I64 F(){Box box;U16 value=32;
box.p=&value;return *box.p+sizeof(box.p)+sizeof(box.bits.u32.u16.u8);}F();|},
      42L,
      "" );
    ( "address cancellation through a pointer field",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;U8 *q=&*o.p;return *q;}F();|},
      42L,
      "" );
    ("packed pointer fields and call arguments", pointer_example, 42L, "AB");
    ( "scalar pointer field store and load",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;return *o.p;}F();|},
      42L,
      "" );
    ( "nested pointer field",
      {|class I{U8 *p;};class Box{U8 tag;I item;};I64 F(){Box o;U8 value=42;o.item.p=&value;return *o.item.p;}F();|},
      42L,
      "" );
    ("multidimensional pointer member array", pointer_proof_source, 42L, "");
    ( "class pointer member array",
      {|class I{U16 value;};class Box{I *p[2];};I64 F(){Box o;I item;item.value=42;o.p[1]=&item;return o.p[1]->value;}F();|},
      42L,
      "" );
    ( "pointer fields in an aggregate root array",
      {|class Box{U8 tag;U16 *p;};I64 F(){Box o[2][3];U16 value=42;o[1][2].p=&value;return *o[1][2].p;}F();|},
      42L,
      "" );
    ( "inherited pointer field",
      {|class Base{U8 tag;U16 *p;};class Box:Base{U8 tail;};I64 F(){Box o;U16 value=42;o.p=&value;return *o.p;}F();|},
      42L,
      "" );
    ( "self class pointer field",
      {|class Node{Node *next;U16 value;};I64 F(){Node a,b;b.value=42;a.next=&b;return a.next->value;}F();|},
      42L,
      "" );
    ( "pointer field copied to a local pointer",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;U8 *q=o.p;return *q;}F();|},
      42L,
      "" );
    ( "class pointer field copied to a local pointer",
      {|class I{U16 value;};class Box{I *p;};I64 F(){Box o;I item;item.value=42;o.p=&item;I *q=o.p;return q->value;}F();|},
      42L,
      "" );
    ( "pointer fields copy owned values between objects",
      {|class Box{U8 *p;};I64 F(){Box a,b;U8 value=42;a.p=&value;b.p=a.p;return *b.p;}F();|},
      42L,
      "" );
    ( "pointer member reached through a class parameter",
      {|class Box{U8 *p;};I64 Read(Box *p){return *p->p;}I64 F(){Box o;U8 value=42;o.p=&value;return Read(&o);}F();|},
      42L,
      "" );
    ( "callee saves a caller owned address",
      {|class Box{U8 *p;};U0 Save(Box *p,U8 *value){p->p=value;}I64 F(){Box o;U8 value=42;Save(&o,&value);return *o.p;}F();|},
      42L,
      "" );
    ( "callee overwrites an expired pointer field",
      {|class Box{U8 *p;};U0 Save(Box *p){U8 value=99;p->p=&value;}I64 F(){Box o;U8 value=42;Save(&o);o.p=&value;return *o.p;}F();|},
      42L,
      "" );
    ( "dereference destination retained across field retargeting",
      {|class Box{U16 *p;};I64 F(){Box o;U16 a=20,b=22;o.p=&a;*o.p+=(o.p=&b)[0];if(b!=22)return -1;return a;}F();|},
      42L,
      "" );
    ( "field destination retained across root retargeting",
      {|class Box{U8 *p;};I64 F(){Box a,b;Box *root=&a;U8 old=99,value=42;b.p=&old;root->p=(root=&b)->p=&value;return *a.p;}F();|},
      42L,
      "" );
    ( "indexed field destination retained across index changes",
      {|class Box{U8 *p[2];};I64 F(){Box o;U8 values[2];values[1]=42;I64 i=0;o.p[i]=&values[i=1];return *o.p[0];}F();|},
      42L,
      "" );
    ( "pointer field postfix and compound updates",
      {|class Box{U16 *p;};I64 F(){Box o;U16 values[3];values[0]=40;values[2]=2;o.p=values;U16 *old=o.p++;o.p+=1;return *old+*o.p;}F();|},
      42L,
      "" );
    ( "class pointer field updates use the selected pointee size",
      {|class I{U8 tag;U16 value;};class Box{I *p;};I64 F(){Box o;I items[2];items[1].value=42;o.p=items;++o.p;return o.p->value;}F();|},
      42L,
      "" );
    ( "same type union fields share pointer ownership",
      {|union Box{U8 *p;U8 *q;};I64 F(){Box o;U8 value=42;o.p=&value;return *o.q;}F();|},
      42L,
      "" );
    ( "disjoint scalar store preserves a pointer field",
      {|class Box{U8 *p;U8 tag;};I64 F(){Box o;U8 value=42;o.p=&value;o.tag=99;return *o.p;}F();|},
      42L,
      "" );
    ( "disjoint byte view preserves a pointer field",
      {|class Box{U8 tag;U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;U8 *bytes=(&o)(U8*);bytes[0]=99;return *o.p;}F();|},
      42L,
      "" );
    ( "pointer bytes initialize the default class prefix",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;return (o!=0)*42;}F();|},
      42L,
      "" );
    ( "pointer byte aliases preserve equality for the same target",
      {|union Box{U8 *p;U64 bits;};I64 F(){Box a,b;U8 value=42;a.p=&value;b.p=&value;return (a.bits==b.bits)*42;}F();|},
      42L,
      "" );
  ]

let pointer_width_matrix =
  List.map
    (fun (spelling, width) ->
      ( "pointer field pointee width " ^ spelling,
        Printf.sprintf
          "union Box{%s *p;U64 bits;};I64 F(){Box o;%s \
           values[2];values[1]=40;o.p=values;U64 \
           old=o.bits;++o.p;*o.p+=2;if(o.bits-old!=%d)return -1;return \
           *o.p;}F();"
          spelling spelling width,
        42L,
        "" ))
    [
      ("Bool", 1);
      ("I8", 1);
      ("U8", 1);
      ("I16", 2);
      ("U16", 2);
      ("I32", 4);
      ("U32", 4);
      ("I64", 8);
      ("U64", 8);
    ]

let pointer_faults =
  [
    ( "whole class prefix store removes pointer ownership",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;U64 old=o;o=old;return *o.p;}F();|},
      "HCIRVM0018",
      "" );
    ( "a later activation cannot revive a saved local pointer",
      {|class Box{U8 *p;};U0 Save(Box *p,I64 keep){U8 value=42;if(keep)p->p=&value;}I64 F(){Box o;Save(&o,1);Save(&o,0);return *o.p;}F();|},
      "HCIRVM0018",
      "" );
    ( "pointer union cannot change nominal class identity",
      {|class A{U16 value;};class B{U16 value;};union Box{A *p;B *q;};I64 F(){Box o;A value;value.value=42;o.p=&value;return o.q->value;}F();|},
      "HCIRVM0018",
      "" );
    ( "unknown pointer field",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 *p;};I64 F(){Box o;Print("kept");return *o.p;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "pointer field cannot keep a callee local alive",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 *p;};U0 Save(Box *p){U8 value=42;p->p=&value;}I64 F(){Box o;Save(&o);Print("kept");return *o.p;}F();|},
      "HCIRVM0018",
      "kept" );
    ( "scalar union overwrite removes pointer ownership",
      {|union Box{U8 *p;U64 bits;};I64 F(){Box o;U8 value=42;o.p=&value;o.bits=0;return *o.p;}F();|},
      "HCIRVM0018",
      "" );
    ( "restoring pointer bits cannot restore ownership",
      {|union Box{U8 *p;U64 bits;};I64 F(){Box o;U8 value=42;o.p=&value;U64 old=o.bits;o.bits=old;return *o.p;}F();|},
      "HCIRVM0018",
      "" );
    ( "same byte overwrite removes pointer ownership",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;U8 *bytes=(&o)(U8*);bytes[7]=bytes[7];return *o.p;}F();|},
      "HCIRVM0018",
      "" );
    ( "pointer union cannot change pointee type",
      {|union Box{U8 *p;I8 *q;};I64 F(){Box o;U8 value=42;o.p=&value;return *o.q;}F();|},
      "HCIRVM0018",
      "" );
    ( "initialized numeric bytes do not create a pointer field",
      {|union Box{U8 *p;U64 bits;};I64 F(){Box o;o.bits=42;return *o.p;}F();|},
      "HCIRVM0018",
      "" );
    ( "partial numeric bytes leave a pointer field unknown",
      {|union Box{U8 *p;U8 byte;};I64 F(){Box o;o.byte=42;return *o.p;}F();|},
      "HCIRVM0012",
      "" );
    ( "one past pointer field target cannot be read",
      {|class Box{U16 *p;};I64 F(){Box o;U16 value=42;o.p=&value;o.p++;return *o.p;}F();|},
      "HCIRVM0019",
      "" );
    ( "pointer field array store checks its full width",
      {|class Box{U8 *p[2];};I64 F(){Box o;U8 value=42;o.p[2]=&value;return 0;}F();|},
      "HCIRVM0019",
      "" );
    ( "word class parameters do not transfer pointer ownership",
      {|class Box{U8 *p;};I64 Read(Box o){return *o.p;}I64 F(){Box o;U8 value=42;o.p=&value;return Read(o);}F();|},
      "HCIRVM0018",
      "" );
  ]

let pointer_unsupported =
  [
    ( "explicit address of a pointer field needs another pointer layer",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;*(&o.p)=&value;return **(&o.p);}F();|}
    );
    ( "numeric null pointer field",
      {|class Box{U8 *p;};I64 F(){Box o;o.p=0;return *o.p;}F();|} );
    ( "pointer field cast to a numeric word",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;return o.p(U64);}F();|}
    );
    ("F64 pointer field", {|class Box{F64 *p;};I64 F(){Box o;return *o.p;}F();|});
    ("U0 pointer field", {|class Box{U0 *p;};I64 F(){Box o;return o.p==0;}F();|});
    ( "multiple pointer layers in a field",
      {|class Box{U8 **p;};I64 F(){Box o;return **o.p;}F();|} );
  ]

let pointer_retained_values =
  [
    ( "pointer fields and builtin queries preserve their selected classes",
      {|class Item{U16 value;};class Box{Item *p;U64 bits;};
I64 F(){Box box;Item item;item.value=40;box.p=&item;
return box.p->value+sizeof(box.bits.u16.u8);}
class Item{U8 value;};class U64{U8 changed;};F();|},
      42L,
      "" );
    ( "pointer member retains the earlier selected class",
      {|class I{U16 value;};class Box{I *p;};I64 F(){Box o;I item;item.value=42;o.p=&item;return o.p->value;}class I{U8 value;};F();|},
      42L,
      "" );
    ( "pointer field survives a later containing class replacement",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;return *o.p;}class Box{U64 bits;};F();|},
      42L,
      "" );
  ]

let values =
  [
    ( "direct member store and load",
      {|class Box{U8 byte;};I64 F(){Box object;object.byte=42;return object.byte;}F();|},
      42L,
      "" );
    ( "function-local object layout",
      {|I64 F(){class Box{U8 byte;};Box o;o.byte=42;return o.byte;}F();|},
      42L,
      "" );
    ("packed string array and class pointer parameter", example, 42L, "AB");
    ( "nested aggregate fields",
      {|class I{U8 tag;U16 value;};class Box{U8 prefix;I item;U8 tail;};I64 F(){Box o;o.item.value=40;(&o)->item.value+=2;return o.item.value;}F();|},
      42L,
      "" );
    ( "two-dimensional primitive member array",
      {|class Box{U8 tag;U16 values[2][3];U8 tail;};I64 F(){Box o;o.values[1][2]=40;(&o)->values[1][2]+=2;return o.values[1][2];}F();|},
      42L,
      "" );
    ( "two-dimensional aggregate member array",
      {|class I{U8 tag;U16 value;};class Box{U8 tag;I items[2][3];U8 tail;};I64 F(){Box o;o.items[1][2].value=40;(&o)->items[1][2].value+=2;return o.items[1][2].value;}F();|},
      42L,
      "" );
    ( "address and dereference of aggregate array element",
      {|class I{U8 tag;U16 value;};class Box{I items[2];};I64 F(){Box o;(&o.items[1])->value=42;return (*(&o.items[1])).value;}F();|},
      42L,
      "" );
    ( "indexed aggregate address passed to a class pointer parameter",
      {|class I{U8 tag;U16 value;};class Box{I items[2];};I64 Bump(I *p){return ++p->value;}I64 F(){Box o;I *p=&o.items[1];p->value=40;Bump(p);return Bump(&o.items[1]);}F();|},
      42L,
      "" );
    ( "local class pointer",
      {|class Box{U8 tag;U16 value;};I64 F(){Box o;Box *p=&o;p->value=40;return ++p->value;}F()+1;|},
      42L,
      "" );
    ( "class pointer aliases captured before RHS",
      {|class Box{U16 value;};I64 F(){Box a,b;Box *p=&a;a.value=20;b.value=22;p->value+=(p=&b)->value;if(b.value!=22)return -99;return a.value;}F();|},
      42L,
      "" );
    ( "member pointer alias parameter",
      {|class Box{U8 tag;U16 value;};I64 Add(U16 *p){*p+=2;return *p;}I64 F(){Box o;o.value=40;return Add(&o.value);}F();|},
      42L,
      "" );
    ( "union fields overlap at their widths",
      {|union Box{U64 word;U8 byte;};I64 F(){Box o;o.word=0x080706050403022a;o.byte+=2;return o.word-0x0807060504030200;}F();|},
      44L,
      "" );
    ( "anonymous union fields",
      {|class Box{U8 tag;union{U64 word;U8 byte;}U8 tail;};I64 F(){Box o;o.word=0x080706050403022a;o.tail=99;return o.byte;}F();|},
      42L,
      "" );
    ( "signed field and unsigned byte view",
      {|class Box{I8 value;};I64 F(){Box o;o.value=-2;U8 *p=(&o)(U8*);return o.value+p[0];}F();|},
      252L,
      "" );
    ( "explicit member padding",
      {|class Box{U8 tag;$$=16;U16 value;};I64 F(){Box o;o.value=40;return ++o.value;}F()+1;|},
      42L,
      "" );
    ( "backing class keeps its member identity",
      {|U64 class Box{U8 value;};I64 F(){Box o;o.value=42;return o.value;}F();|},
      42L,
      "" );
    ( "shadow keeps the earlier field width and offset",
      {|class Box{U8 tag;U16 value;};I64 F(){Box o;o.value=40;return ++o.value;}class Box{U8 value;};F()+sizeof(Box);|},
      42L,
      "" );
    ( "member array decay crosses a block",
      {|class Box{U8 tag;U16 values[2][3];};I64 F(){Box o;U16 *p=o.values[1];p[2]=42;if(p[2])return o.values[1][2];return 0;}F();|},
      42L,
      "" );
  ]

let view_matrix =
  List.map
    (fun spelling ->
      ( "field width " ^ spelling,
        Printf.sprintf
          "class Box{U8 tag;%s value;};I64 F(){Box \
           o;o.value=40;o.value++;return ++o.value;}F();"
          spelling,
        42L,
        "" ))
    [ "Bool"; "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ]

let faults =
  [
    ( "unknown untouched field",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 tag;U16 value;};I64 F(){Box o;o.tag=42;Print("kept");return o.value;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "partial union field",
      {|extern U0 Print(U8 *fmt,...);union Box{U16 word;U8 byte;};I64 F(){Box o;o.byte=42;Print("kept");return o.word;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "unknown class pointer",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box *p;Print("kept");return p->value;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "member array one past object",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 bytes[3];};I64 F(){Box o;Print("kept");o.bytes[3]=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "aggregate array field one past object",
      {|extern U0 Print(U8 *fmt,...);class I{U8 tag;U16 value;};class Box{I items[2];};I64 F(){Box o;Print("kept");o.items[2].value=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "fresh activation has fresh fields",
      {|class Box{U16 value;};I64 once=0;I64 F(){Box o;if(once)return o.value;once=1;o.value=42;return o.value;}F();F();|},
      "HCIRVM0012",
      "" );
    ( "whole member word exceeds its root",
      {|class I{U8 byte;};class Box{I item;};I64 F(){Box o;o.item;return 42;}F();|},
      "HCIRVM0019",
      "" );
    ( "whole array element word exceeds its root",
      {|class I{U8 byte;};class Box{I items[2];};I64 F(){Box o;o.items[0];return 42;}F();|},
      "HCIRVM0019",
      "" );
  ]

let unsupported =
  [
    ("zero-size automatic object", {|class Box{};I64 F(){Box o;return 42;}F();|});
    ( "automatic aggregate pointer array",
      {|class Box{U8 byte;};I64 F(){Box *items[2];return 42;}F();|} );
    ("persistent aggregate", {|class Box{U8 byte;};Box object;42;|});
    ( "whole-object copy",
      {|class Box{U8 byte;};I64 F(){Box a,b=a;return 42;}F();|} );
    ( "class pointer return",
      {|class Box{U8 byte;};Box *Get(Box *p){return p;}I64 F(){Box o;return Get(&o)->byte;}F();|}
    );
    ( "callback field has no executable owner",
      {|class Box{I64 (*call)(I64 value);};I64 Id(I64 value){return value;}I64 F(){Box o;o.call=&Id;return o.call(42);}F();|}
    );
    ( "numeric bits cannot become an owned class pointer",
      {|class Box{U8 byte;};I64 F(){Box *p=42(Box*);return p->byte;}F();|} );
  ]

let extents =
  [
    ("class Box{U8 tag;U8 bytes[3];};", 3);
    ("union Box{U64 word;U8 bytes[8];};", 8);
    ("class Box{U8 tag;union{U64 word;U8 bytes[8];}};", 8);
    ("class Box{U8 tag;$$=16;U8 bytes[2];};", 2);
  ]

let extent_source definition index =
  Printf.sprintf "%s I64 F(){Box o;o.bytes[%d]=42;return o.bytes[%d];}F();"
    definition index index

let nested_values =
  [
    ( "nested comma declarations retain independent array extents",
      {|class I{U16 word;};class Box{I left[2],right[3];};I64 F(){Box o;o.right[2].word=42;return sizeof(Box)+o.right[2].word-10;}F();|},
      42L,
      "" );
    ( "nested default argument retains the outer guard word",
      {|class I{U16 low;};class Box{I item;U8 guard;};I64 Take(Box o=0x07002a){if(o.guard!=7)return -1;return o.item.low;}Take();|},
      42L,
      "" );
    ( "a completed nested selection survives a later same-name header",
      {|class I{U16 word;};class Box{I item;};class I{U8 byte;};I64 F(){Box o;o.item.word=42;return o.item.word;}F();|},
      42L,
      "" );
  ]

let retained_nested_values =
  [
    ( "nested runtime dimensions prepare once",
      {|I64 Count=0;I64 Next(){Count++;return 2;}class I{U16 value[Next()];};class Box{I item[2];};I64 F(){Box o;o.item[1].value[1]=41;return sizeof(Box)+o.item[1].value[1]+Count-8;}F();|},
      42L,
      "" );
    ( "nested runtime offsets retain their original preparation",
      {|I64 Count=0;I64 Next(){Count++;return 4;}class I{U16 word;$$=Next();U8 tag;};class Box{I item[2];};I64 F(){Box o;o.item[1].tag=41;return sizeof(Box)+o.item[1].tag+Count-10;}F();|},
      42L,
      "" );
    ( "member selection precedes dimension lookahead replacement",
      {|class I{U16 word;};class Box{I item[2 #exe{class I{U8 byte;};}];};I64 F(){Box o;o.item[1].word=42;return sizeof(Box)+o.item[1].word-4;}F();|},
      42L,
      "" );
    ( "an admitted child class completes before its containing member",
      {|class I{U16 word;};class Box{#exe{class I{U8 byte;};}I item;};I64 F(){Box o;o.item.byte=42;return sizeof(Box)+o.item.byte-1;}F();|},
      42L,
      "" );
    ( "named backing selection precedes child body replacement",
      {|U16 class I{U16 word;};I class Box{#exe{U8 class I{U8 byte;};}U8 low;};I64 F(Box o=0x12a){return o;}F();|},
      298L,
      "" );
    ( "named backing selection precedes publication lookahead replacement",
      {|U16 class I{U16 word;};I #exe{U8 class I{U8 byte;};}class Box{U8 low;};I64 F(Box o=0x12a){return o;}F();|},
      298L,
      "" );
    ( "a called class default uses its nested member layout",
      {|class I{U16 low;};class Box{I item;U8 guard;};Box Make(){return 0x07002a;}I64 Take(Box o=Make()){if(o.guard!=7)return -1;return o.item.low;}Take();|},
      42L,
      "" );
    ( "named pointer metadata uses pointer bytes",
      {|class I{U16 word;};class Box{I *link;U8 tag;};sizeof(Box)+33;|},
      42L,
      "" );
    ( "nested source sizes survive an original compile directive",
      {|#exe{class I{U16 word;};class Box{I item[2];};StreamPrint("%d;",sizeof(Box)+38);}|},
      42L,
      "" );
  ]

let retained_nested_limits =
  {|I64 Count=0;I64 Next(){Count++;return 2;}class I{U16 value[Next()];};class Box{I item[2];};I64 F(){Box o;o.item[1].value[1]=41;return o.item[1].value[1]+Count;}F();|}
