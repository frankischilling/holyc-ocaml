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

let values =
  [
    ( "direct member store and load",
      {|class Box{U8 byte;};I64 F(){Box object;object.byte=42;return object.byte;}F();|},
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
  ]

let unsupported =
  [
    ( "function-local object layout",
      {|I64 F(){class Box{U8 byte;};Box o;o.byte=42;return o.byte;}F();|} );
    ("zero-size automatic object", {|class Box{};I64 F(){Box o;return 42;}F();|});
    ( "automatic aggregate pointer array",
      {|class Box{U8 byte;};I64 F(){Box *items[2];return 42;}F();|} );
    ("persistent aggregate", {|class Box{U8 byte;};Box object;42;|});
    ( "whole-object copy",
      {|class Box{U8 byte;};I64 F(){Box a,b=a;return 42;}F();|} );
    ( "whole aggregate member value",
      {|class I{U8 byte;};class Box{I item;};I64 F(){Box o;o.item;return 42;}F();|}
    );
    ( "whole aggregate array element value",
      {|class I{U8 byte;};class Box{I items[2];};I64 F(){Box o;o.items[0];return 42;}F();|}
    );
    ( "class pointer return",
      {|class Box{U8 byte;};Box *Get(Box *p){return p;}I64 F(){Box o;return Get(&o)->byte;}F();|}
    );
    ( "pointer field has no descriptor ownership",
      {|class Box{U8 *p;};I64 F(){Box o;U8 value=42;o.p=&value;return *o.p;}F();|}
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
