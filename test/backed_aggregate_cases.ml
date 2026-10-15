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

let assignment_values =
  List.map
    (fun (primitive, _) ->
      ( "assignment lookahead reaches " ^ primitive ^ " backing",
        Printf.sprintf
          {|extern class Item;class Box{Item item;U8 bytes[16];};I64 F(){Box object;object.bytes[8]=7;object.item=#exe{%s class Item{%s value;};}42;if(object.bytes[8]!=7)return -1;return object.bytes[0];}F();|}
          primitive primitive,
        42L,
        "" ))
    widths
  @ [
      ( "compound assignment observes backing after operator lookahead",
        {|extern class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.bytes[0]=40;object.bytes[1]=0;object.item+=#exe{U16 class Item{U16 value;};}2;return object.bytes[0];}F();|},
        42L,
        "" );
      ( "assignment result retains its observed backing word",
        {|extern class Item;class Box{Item item;U8 bytes[8];};I64 Take(I64 n){return n;}I64 F(){Box object;return Take(object.item=#exe{U16 class Item{U16 value;};}42);}F();|},
        42L,
        "" );
      ( "lookahead assignment result preserves the full register word",
        {|extern class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;I64 n=(object.item=#exe{U8 class Item{U8 value;};}298);if(object.bytes[0]!=42)return -1;return n-256;}F();|},
        42L,
        "" );
      ( "pointer member assignment keeps the original containing root",
        {|extern class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;Box *p=&object;p->item=#exe{U16 class Item{U16 value;};}42;return object.bytes[0];}F();|},
        42L,
        "" );
      ( "class dereference assignment observes original forward completion",
        {|extern class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;Item *p=&object.item;*p=#exe{U16 class Item{U16 value;};}42;return object.bytes[0];}F();|},
        42L,
        "" );
      ( "zero stride class array assignment retains its original dimensions",
        {|extern class Item;class Box{Item items[2];U8 bytes[8];};I64 F(){Box object;object.items[1]=#exe{U16 class Item{U16 value;};}42;return object.bytes[0];}F();|},
        42L,
        "" );
      ( "zero member stride aliases stay within the original containing root",
        {|extern class Item;class Box{Item items[2];U8 bytes[8];};I64 F(){Box object;object.items[2]=#exe{U16 class Item{U16 value;};}42;return object.bytes[0];}F();|},
        42L,
        "" );
      ( "later member index observes the completed original class size",
        {|extern class Item;class Box{Item items[2];U8 bytes[8];};I64 F(){Box object;object.items[1]=#exe{U16 class Item{U16 value;};}42;object.items[1]=44;if(object.bytes[2]!=44)return -1;return object.bytes[0];}F();|},
        42L,
        "" );
      ( "index opening lookahead observes original class completion",
        {|extern class Item;class Box{Item items[2];U8 bytes[8];};I64 F(){Box object;object.items[#exe{U16 class Item{U16 value;};}1]=42;return object.bytes[2];}F();|},
        42L,
        "" );
      ( "completion during index value preserves its earlier zero stride",
        {|extern class Item;class Box{Item items[2];U8 bytes[8];};I64 F(){Box object;object.items[1#exe{U16 class Item{U16 value;};}]=42;return object.bytes[0];}F();|},
        42L,
        "" );
      ( "multidimensional indices keep separately reached strides",
        {|extern class Item;class Box{Item items[2][3];U8 bytes[16];};I64 F(){Box object;object.items[1#exe{U16 class Item{U16 value;};}][1]=42;return object.bytes[2];}F();|},
        42L,
        "" );
      ( "original internal endpoint survives right operand completion",
        {|extern U16 class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.bytes[1]=0;object.item=0x12a#exe{U8 class Item{U8 value;};};return object.bytes[1]+41;}F();|},
        42L,
        "" );
      ( "original internal endpoint survives later body completion",
        {|extern U16 class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.bytes[1]=0;object.item=0x12a;#exe{U8 class Item{U8 value;};}return object.bytes[1]+41;}F();|},
        42L,
        "" );
      ( "later equal name definition does not change the original store width",
        {|extern U16 class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.bytes[1]=0;object.item=0x12a#exe{U16 class Item{U16 value;};U8 class Item{U8 value;};};return object.bytes[1]+41;}F();|},
        42L,
        "" );
      ( "named backing chain reaches the original completed endpoint",
        {|extern class Word;Word class Item{U64 word;};I64 F(){Item object;object=#exe{U16 class Word{U16 value;};}42;U8 *p=(&object)(U8*);return p[0];}F();|},
        42L,
        "" );
    ]

let in_task_source source =
  let suffix = "F();" in
  assert (String.ends_with ~suffix source);
  Printf.sprintf "#exe{%sStreamPrint(\"%%d;\",F());}"
    (String.sub source 0 (String.length source - String.length suffix))

let isolated_assignment_source =
  {|extern class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.item=#exe{U16 class Item{U16 value;};}42;return object.bytes[0];}F();|}

let isolated_assignment_value_source =
  {|extern U16 class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.bytes[1]=0;object.item=0x12a#exe{U8 class Item{U8 value;};};return object.bytes[1]+42;}F();|}

let source_values =
  List.map
    (fun (name, source, value, output) ->
      (name, in_task_source source, value, output))
    assignment_values

let source_faults =
  [
    ( "assignment lookahead cannot enlarge an original empty local",
      in_task_source
        {|extern class Item;I64 F(){Item object;object=#exe{U16 class Item{U16 value;};}42;return 42;}F();|},
      "HCIRVM0019",
      "" );
    ( "observed backing width remains bounded by the original containing root",
      in_task_source
        {|extern class Item;class Box{Item item;U8 byte;};I64 F(){Box object;object.item=#exe{U64 class Item{U64 value;};}42;return 42;}F();|},
      "HCIRVM0019",
      "" );
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
    ( "completion after the right operand cannot authorize assignment",
      {|extern class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.item=42#exe{U16 class Item{U16 value;};};return 42;}F();|}
    );
    ( "completion after assignment cannot authorize its earlier lvalue",
      {|extern class Item;class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.item=42;#exe{U16 class Item{U16 value;};}return 42;}F();|}
    );
    ( "lookahead replacement cannot supply backing to an earlier class",
      {|class Item{U16 value;};class Box{Item item;U8 bytes[8];};I64 F(){Box object;object.item=#exe{U16 class Item{U16 value;};}42;return 42;}F();|}
    );
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
  ]
