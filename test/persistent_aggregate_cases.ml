let example =
  {|extern U0 Print(U8 *fmt,...);
class Item{U8 text[3];U16 value;};Item items[2];
I64 F(){static Item saved;saved.value=40;items[1].value=++saved.value;
items[1].text[0]=65;items[1].text[1]=66;items[1].text[2]=0;
Print("%s",items[1].text);return ++items[1].value;}F();|}

let quota_source =
  {|class Item{U8 tag;U16 value;};Item items[2];
I64 F(){static Item saved;saved.value=40;items[1].value=saved.value+2;return items[1].value;}F();|}

let values =
  [
    ( "attached comma globals retain separate dimensions and roots",
      {|class Box{U16 value;} a[2],b;I64 F(){a[1].value=20;b.value=22;return a[1].value+b.value;}F();|},
      42L,
      "" );
    ( "attached class globals retain the completed original layout",
      {|class Box{U8 tag;U16 value;} objects[2];I64 F(){objects[1].value=42;return objects[1].value;}F();|},
      42L,
      "" );
    ( "static names in separate functions retain separate roots",
      {|class Box{U16 value;};I64 F(){static Box object;object.value=20;return object.value;}I64 G(){static Box object;object.value=22;return object.value;}F()+G();|},
      42L,
      "" );
    ( "class returns can be stored in a persistent backing prefix",
      {|U16 class Box{U16 value;U8 guard;};Box object;Box Make(){return 42;}I64 F(){object.guard=9;object=Make();if(object.guard!=9)return -1;return object;}F();|},
      42L,
      "" );
    ( "persistent parameter copy retains a separate source object",
      {|class Box{I64 word;};Box saved;I64 Take(Box o){saved=o;o=99;return saved;}Take(42);|},
      42L,
      "" );
    ("persistent aggregate example", example, 42L, "AB");
    ( "packed globals and statics have separate original byte roots",
      quota_source,
      42L,
      "" );
    ( "persistent multidimensional class and member arrays",
      {|class Item{U8 tag;U16 value;};class Box{U8 prefix;Item values[2][2];};Box objects[2][3];I64 F(){objects[1][2].values[1][1].value=42;return objects[1][2].values[1][1].value;}F();|},
      42L,
      "" );
    ( "persistent class pointer parameters retain the source element stride",
      {|class Box{U8 tag;U16 value;};Box a[2];I64 Read(Box *p){return p[1].value;}I64 F(){a[1].value=42;return Read(a);}F();|},
      42L,
      "" );
    ( "persistent interior and backward class indexing",
      {|class Box{U8 tag;U16 value;};Box a[3];I64 F(){Box *p=&a[2];p[-1].value=42;p--;return p->value;}F();|},
      42L,
      "" );
    ( "inherited persistent objects preserve absolute field offsets",
      {|class Base{U8 tag;};class Box:Base{U16 value;};Box object;I64 F(){object.tag=7;object.value=42;if(sizeof(Box)!=3 || object.tag!=7)return -1;return object.value;}F();|},
      42L,
      "" );
    ( "persistent unions share integer and byte views",
      {|union Box{U32 value;U8 bytes[4];};Box object;I64 F(){object.value=0;object.bytes[0]=42;return object.value;}F();|},
      42L,
      "" );
    ( "persistent padding does not change the selected field window",
      {|class Box{U8 tag;$$=6;U16 value;};Box object;I64 F(){object.tag=7;object.value=42;return object.value;}F();|},
      42L,
      "" );
    ( "persistent backing copies retain the destination guard",
      {|U16 class Box{U16 value;U8 guard;};Box a,b;I64 F(){a=42;a.guard=7;b.guard=9;b=a;if(b.guard!=9 || a.guard!=7)return -1;return b;}F();|},
      42L,
      "" );
    ( "persistent ordinary class words use their complete eight-byte prefix",
      {|class Box{U64 value;U8 guard;};Box object;I64 F(){object.guard=9;object=40;object+=2;if(object.guard!=9)return -1;return object;}F();|},
      42L,
      "" );
    ( "static class bytes persist across recursive activations",
      {|class Box{U16 value;};I64 once=0;I64 F(I64 n){static Box object;if(!once){object.value=40;once=1;}if(n)return F(n-1);return ++object.value;}F(2);F(1);|},
      42L,
      "" );
    ( "function-local static class layouts are selected at their allocation",
      {|I64 F(){class Box{U8 tag;U16 value;};static Box objects[2];objects[1].value=42;return objects[1].value;}F();|},
      42L,
      "" );
    ( "persistent assignment captures its root before RHS pointer rebinding",
      {|class Box{U16 value;};Box a[2];I64 F(){a[0].value=20;a[1].value=99;Box *p=a;p->value+=((p=&a[1])==&a[1])*22;if(a[1].value!=99)return -1;return a[0].value;}F();|},
      42L,
      "" );
    ( "persistent class byte views retain the complete original extent",
      {|class Box{U16 value;U8 guard;};Box a[2];I64 F(){U8 *p=(a)(U8*);p[3]=42;p[4]=0;return a[1].value;}F();|},
      42L,
      "" );
    ( "persistent class sizes use source element counts instead of byte cells",
      {|class Box{U8 tag;U16 value;};Box a[2];I64 F(){static Box b[3];a[1].value=42;if(sizeof(a)!=6 || sizeof(b)!=9 || sizeof(a.value)!=2)return -1;return a[1].value;}F();|},
      42L,
      "" );
    ( "persistent integer backing is independently selected",
      {|I64 class Box{I64 word;};Box object;I64 F(){object=42;return object;}F();|},
      42L,
      "" );
    ( "persistent default whole class value",
      {|class Box{U64 word;};Box object;I64 F(){object=42;return object;}F();|},
      42L,
      "" );
  ]
  @ List.map
      (fun (type_, minus_one) ->
        ( "persistent field width " ^ type_,
          Printf.sprintf
            {|class Box{U8 tag;%s value;};Box object;I64 F(){object.value=-1;if(object.value!=%s)return -1;object.value=42;return object.value;}F();|}
            type_ minus_one,
          42L,
          "" ))
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

let retained_values =
  [
    ( "persistent runtime member dimensions prepare once",
      {|I64 Count=0;I64 Next(){Count++;return 2;}class Box{U16 values[Next()];};Box object;I64 F(){static Box saved;object.values[1]=40;saved.values[1]=1;return object.values[1]+saved.values[1]+Count;}F();|},
      42L,
      "" );
    ( "persistent runtime member offsets retain their original preparation",
      {|I64 Count=0;I64 Next(){Count++;return 4;}class Box{U16 value;$$=Next();U8 tag;};Box object;I64 F(){static Box saved;object.tag=40;saved.tag=1;return object.tag+saved.tag+Count;}F();|},
      42L,
      "" );
    ( "persistent global and class replacements preserve an earlier function",
      {|class Box{U8 tag;U16 value;};Box object;I64 F(){object.value=42;return object.value;}class Box{U8 value;};Box object;object.value=99;F();|},
      42L,
      "" );
    ( "retained static class fields keep their original layout",
      {|class Box{U8 tag;U16 value;};I64 F(){static Box object;object.value=42;return object.value;}class Box{U8 value;};F();|},
      42L,
      "" );
    ( "persistent globals reached through original directives share one \
       allocation",
      {|class Box{U8 tag;U16 value;};Box object;I64 F(){return ++object.value;}#exe{object.value=40;}F();F();|},
      42L,
      "" );
  ]

let faults =
  [
    ( "persistent out-of-root class field window",
      {|class Box{U8 byte;};Box objects[2];I64 F(){U16 *p=(&objects[1])(U16*);*p=42;return 0;}F();|},
      "HCIRVM0019",
      "" );
    ( "static allocation padding grants no extra bytes",
      {|class Box{U8 byte;};I64 F(){static Box object;U16 *p=(&object)(U16*);*p=42;return 0;}F();|},
      "HCIRVM0019",
      "" );
    ( "persistent class one-past read",
      {|class Box{U16 value;};Box a[2];I64 F(){return (a+2)->value;}F();|},
      "HCIRVM0019",
      "" );
    ( "persistent unrelated roots cannot supply class pointer differences",
      {|class Box{U16 value;};Box a,b;I64 F(){return &a-&b;}F();|},
      "HCIRVM0018",
      "" );
  ]

let jit_faults =
  [
    ( "JIT persistent classes begin with unknown bytes",
      {|class Box{U16 value;};Box object;I64 F(){return object.value;}F();|},
      "HCIRVM0012",
      "" );
    ( "JIT static class union partial write retains unknown bytes",
      {|union Box{U16 value;U8 bytes[2];};I64 F(){static Box object;object.bytes[0]=42;return object.value;}F();|},
      "HCIRVM0012",
      "" );
  ]

let aot_values =
  [
    ( "AOT persistent class bytes begin initialized to zero",
      {|class Box{U8 tag;U64 value;};Box object;I64 F(){static Box saved;return object.tag+object.value+saved.value+42;}F();|},
      42L,
      "" );
  ]

let pointer_values =
  [
    ( "IR global pointer fields retain the current activation's address",
      {|class Box{U8 *p;};Box object;I64 F(){U8 byte=42;object.p=&byte;return *object.p;}F();|},
      42L,
      "" );
    ( "IR static pointer fields can retain a separate persistent target",
      {|class Box{U16 *p;};U16 value;I64 F(){static Box object;value=42;object.p=&value;return *object.p;}F();|},
      42L,
      "" );
  ]

let pointer_faults =
  [
    ( "IR persistent pointer fields cannot extend a retired activation",
      {|class Box{U8 *p;};Box object;U0 Save(){U8 byte=42;object.p=&byte;}I64 F(){Save();return *object.p;}F();|},
      "HCIRVM0018",
      "" );
  ]

let unsupported =
  [
    ( "persistent aggregate scalar initializer awaits member leaves",
      {|class Box{U64 value;};Box object=42;I64 F(){return object.value;}F();|}
    );
    ( "static aggregate braced initializer awaits member leaves",
      {|class Box{U16 value;};I64 F(){static Box object={42};return object.value;}F();|}
    );
  ]
