let headers = "extern U0 Print(U8 *fmt,...);"

let source_zero_member =
  {|I64 calls=0;extern class Item;class Box{Item item;U8 tail;$$=8;};I64 Next(){calls++;Print("A");return 7;}Box object={Next(),42};I64 F(){if(calls!=1)return -1;return object.tail;}F();|}

let current_leaf =
  {|#exe {
extern class Item;class Box{Item item;$$=8;};
I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}
Box object={Complete()};StreamPrint("%d;",object.item.value);
}|}

let nested_metadata =
  {|#exe {
extern class Item;class Box{Item item;$$=8;};
I64 Complete(){Print("A");StreamExePrint("I64 Next(){return 42;}class Item{U16 value data Next();};");return 42;}
Box object={Complete()};StreamPrint("%d;",object.item.value);
}|}

let saved_tables =
  {|#exe {
extern class Item;class Box{Item item;U8 tail;$$=8;};
I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}
Box object={Complete(),7};StreamPrint("%d;",object.tail);
}|}

let backing raw =
  Printf.sprintf
    {|#exe {
extern class Item;class Box{Item item;$$=8;};
I64 Complete(){Print("A");StreamExePrint("%s class Item{I64 value;};");return -1;}
Box object={Complete()};
I64 Check(){U8 *p=(&object)(U8*);I64 i;for(i=0;i<8;i++)if(p[i]!=255)return -1;return 42;}
StreamPrint("%%d;",Check());
}|}
    raw

let ordinary_source_values =
  [
    ( "ordinary source activates before a zero-byte member initializer",
      {|extern class Item;class Box{Item item;U8 tail;$$=8;};Box object={7,42};I64 F(){return object.tail;}F();|},
      "" );
    ( "ordinary source evaluates zero-byte members exactly once",
      source_zero_member,
      "A" );
    ( "ordinary source zero-byte member arrays keep each leaf",
      {|I64 calls=0;extern class Item;class Box{Item items[2];U8 tail;$$=8;};I64 Next(I64 n){calls++;if(n==40)Print("A");return n;}Box object={{Next(40),Next(2)},42};I64 F(){if(calls!=2)return -1;return object.tail;}F();|},
      "A" );
    ( "ordinary source zero-byte leaves follow the direct member cursor",
      {|extern class Item;class Base{U8 base;};class Box:Base{Item item;U8 tail;$$=8;};I64 Next(){Print("A");return 7;}Box object={Next(),42};I64 F(){return object.base;}F();|},
      "A" );
    ( "ordinary source later completion preserves the earlier copy",
      {|extern class Item;class Box{Item item;U8 tail;$$=8;};I64 Next(){Print("A");return 7;}Box object={Next(),42};class Item{U16 value;};I64 F(){return object.tail;}F();|},
      "A" );
    ( "ordinary source shares reached globals with later functions",
      {|extern class Item;class Box{Item item;U8 tail;$$=8;};I64 saved=0;I64 Next(){saved=40;Print("A");return 7;}Box object={Next(),42};I64 F(){if(saved!=40)return -1;object.tail+=2;return object.tail;}I64 Check(){F();return F()-4;}Check();|},
      "A" );
    ( "ordinary source zero-byte union members evaluate once",
      {|extern class Item;union Box{Item item;U8 value;};I64 Next(){Print("A");return 7;}Box object={Next(),42};I64 F(){return object.value;}F();|},
      "A" );
    ( "ordinary source aggregate arrays retain each initializer effect",
      {|I64 calls=0;extern class Item;class Box{Item item;U8 tail[2];$$=8;};I64 Next(I64 n){calls++;if(n==40)Print("A");return n;}Box objects[2]={{Next(40),{40,2}},{Next(2),{1,1}}};I64 F(){if(calls!=2)return -1;return objects[0].tail[0]+objects[0].tail[1];}F();|},
      "A" );
  ]

let jit_values =
  [
    ("callee completes the cached scalar class", current_leaf, "A");
    ("callee completion includes child runtime metadata", nested_metadata, "A");
    ( "lookahead completes the cached scalar class",
      {|extern class Item;class Box{Item item;$$=8;};Box object={42#exe{class Item{U16 value;};}};I64 F(){return object.item.value;}F();|},
      "" );
    ( "completed class changes the next array element branch",
      {|#exe {
extern class Item;class Box{Item items[2];U8 tail;$$=8;};
I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}
Box object={{Complete(),{40}},7};
I64 Check(){U8 *p=(&object)(U8*);if(p[2]!=40||p[4]!=7)return -1;return p[0];}
StreamPrint("%d;",Check());
}|},
      "A" );
    ( "same-name replacement keeps the original copy width",
      {|#exe {
extern class Item;class Box{Item item;$$=8;};
I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};class Item{U8 value;};");return 0x12a;}
Box object={Complete()};StreamPrint("%d;",object.item.value-256);
}|},
      "A" );
    ( "late U16 backing keeps the entire cached register word",
      backing "U16",
      "A" );
    ( "late F64 backing keeps the entire cached register word",
      backing "F64",
      "A" );
  ]

let backed_array_lexer static =
  let object_ =
    {|Box object={{#exe{Print("A");U8 class Item{U8 value;};}0x12a,7}};|}
  in
  let check =
    {|U8 *p=(&object)(U8*);if(p[0]!=42||p[1]!=1||p[2]!=7||p[3]!=0)return -1;return 42;|}
  in
  "#exe{extern U16 class Item;class Box{Item items[2];$$=8;};"
  ^ (if static then "I64 F(){static " ^ object_ ^ check ^ "}"
     else object_ ^ "I64 F(){" ^ check ^ "}")
  ^ "StreamPrint(\"%d;\",F());}"

let backed_array_callee static =
  let object_ = "Box object={{Complete(),7}};" in
  let check =
    {|U8 *p=(&object)(U8*);if(p[0]!=42||p[1]!=1||p[2]!=7||p[3]!=0)return -1;return 42;|}
  in
  "#exe{extern U16 class Item;class Box{Item items[2];$$=8;};"
  ^ {|I64 Complete(){Print("A");StreamExePrint("U8 class Item{U8 value;};");return 0x12a;}|}
  ^ (if static then "I64 F(){static " ^ object_ ^ check ^ "}"
     else object_ ^ "I64 F(){" ^ check ^ "}")
  ^ "StreamPrint(\"%d;\",F());}"

let backed_forward_timing =
  {|#exe{extern U16 class Item #exe{Print("A");class Box{Item item;U8 tail;$$=8;};Box object={9,42};};StreamPrint("%d;",object.tail);}|}

let backed_strings =
  {|#exe{extern U8 class Item;class Box{Item items[2][2];$$=8;};I64 F(){static Box object={{#exe{Print("A");U16 class Item{U16 value;};}"A","B"}};U8 *p=(&object)(U8*);if(p[0]!=65||p[1]!=0||p[2]!=66||p[3]!=0)return -1;return 42;}StreamPrint("%d;",F());}|}

let named_backed_array =
  {|#exe{U16 class Base{U8 value;};extern Base class Item;class Box{Item items[2];$$=8;};Box object={{#exe{Print("A");U8 class Base{U8 value;};U8 class Item{U8 value;};}0x12a,7}};I64 F(){U8 *p=(&object)(U8*);if(p[1]!=1||p[2]!=7||p[3]!=0)return -1;return 42;}StreamPrint("%d;",F());}|}

let backed_values =
  [
    ( "callback member lookahead completes the selected forward",
      {|#exe{extern class Item;I64 F(Item *p){return p->#exe{Print("A");class Item{I64 (*callback)(I64 n);};}callback(40);}I64 Next(I64 n){return n+2;}Item object;object.callback=&Next;StreamPrint("%d;",F(&object));}|},
      "A" );
    ( "partial pointer member callbacks keep original storage",
      {|#exe{class Item{I64 (*callback)(I64 n);#exe{Print("A");class Box{Item item;};I64 Next(I64 n){return n+2;}I64 Read(Item *p){return p->callback(40);}I64 F(){static Box object;object.item.callback=&Next;return Read(&object.item);}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial callback member calls retain the original signature",
      {|#exe{class Item{I64 (*callback)(I64 n);#exe{Print("A");class Box{Item item;};I64 Next(I64 n){return n+2;}I64 F(){static Box object;object.item.callback=&Next;return object.item.callback(40);}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial global callback fields retain original outer metadata",
      {|#exe{class Item{I64 (*callback)(I64 n);#exe{Print("A");class Box{Item item;};Box object;I64 Next(I64 n){return n+2;}I64 F(){object.item.callback=&Next;return object.item.callback(40);}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial callback calls retain original named parameter types",
      {|#exe{class Value{I64 value;};class Item{I64 (*callback)(Value *p);#exe{Print("A");class Box{Item item;};I64 Next(Value *p){return p->value+2;}I64 F(){static Value value={40};static Box object;object.item.callback=&Next;return object.item.callback(&value);}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial callback returns retain owned data pointers",
      {|#exe{class Item{U8 *(*callback)(U8 *p);#exe{Print("A");class Box{Item item;};U8 *Next(U8 *p){return p;}I64 F(){static U8 values[1]={42};static Box object;object.item.callback=&Next;U8 *p=object.item.callback(values);return p[0];}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial callbacks retain variadic argument slots",
      {|#exe{class Item{I64 (*callback)(I64 base,...);#exe{Print("A");class Box{Item item;};I64 Next(I64 base,...){return base+argv[argc-1];}I64 F(){static Box object;object.item.callback=&Next;return object.item.callback(40,2);}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial callback callee is captured before argument mutation",
      {|#exe{class Item{I64 (*callback)(I64 n);#exe{Print("A");class Box{Item item;};Box object;I64 Old(I64 n){return 40+n;}I64 New(I64 n){return 100+n;}I64 Change(){object.item.callback=&New;return 2;}I64 F(){object.item.callback=&Old;return object.item.callback(Change());}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "top-level partial callback calls run before the body resumes",
      {|#exe{I64 answer;class Item{I64 (*callback)(I64 n);#exe{Print("A");class Box{Item item;};Box object;I64 Next(I64 n){return n+2;}object.item.callback=&Next;answer=object.item.callback(40);}};StreamPrint("%d;",answer);}|},
      "A" );
    ( "partial callback arrays retain original dimensions",
      {|#exe{class Item{I64 (*callbacks)(I64 n)[2];#exe{Print("A");class Box{Item item;};I64 Next(I64 n){return n+2;}I64 F(){static Box object;object.item.callbacks[1]=&Next;return object.item.callbacks[1](40);}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial callback calls retain saved defaults",
      {|#exe{class Item{I64 (*callback)(I64 n=40);#exe{Print("A");class Box{Item item;};I64 Next(I64 n){return n+2;}I64 F(){static Box object;object.item.callback=&Next;return object.item.callback();}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial inherited callback calls retain the original base",
      {|#exe{class Base{I64 (*callback)(I64 n);};class Item:Base{#exe{Print("A");class Box{Item item;};I64 Next(I64 n){return n+2;}I64 F(){static Box object;object.item.callback=&Next;return object.item.callback(40);}}U8 tail;};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial callback signatures survive same-name replacement",
      {|#exe{class Item{I64 (*callback)(I64 n);#exe{Print("A");class Box{Item item;};class Item{U8 replacement;};I64 Next(I64 n){return n+2;}I64 F(){static Box object;object.item.callback=&Next;return object.item.callback(40);}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "top-level partial member reads use the reached direct fields",
      {|#exe{I64 answer;class Item{U8 value;#exe{Print("A");class Box{Item item;};Box object={{42}};answer=object.item.value;}};StreamPrint("%d;",answer);}|},
      "A" );
    ( "top-level partial writes retain inherited and anonymous union fields",
      {|#exe{I64 answer;class Base{U8 value;};class Item:Base{union{U16 word;U8 bytes[2];};#exe{Print("A");class Box{Item item;};Box object;object.item.value=40;object.item.word=0x12a;answer=object.item.bytes[0]+object.item.value-40;}};StreamPrint("%d;",answer);}|},
      "A" );
    ( "top-level partial arrays retain explicit member offsets",
      {|#exe{I64 answer;class Item{U8 tag;$$=3;U16 values[2];#exe{Print("A");class Box{Item item;};Box object;object.item.values[1]=40;answer=object.item.values[1]+2;}};StreamPrint("%d;",answer);}|},
      "A" );
    ( "top-level partial reads retain the old class after same-name replacement",
      {|#exe{I64 answer;class Item{U8 value;#exe{Print("A");class Box{Item item;};Box object={{42}};class Item{U8 replacement;};answer=object.item.value;}};StreamPrint("%d;",answer);}|},
      "A" );
    ( "top-level completed members retain the object's original class",
      {|#exe{class Item{U8 value;};Item object={42};class Item{U8 replacement;};Print("A");StreamPrint("%d;",object.value);}|},
      "A" );
    ( "top-level member lookahead can complete the selected forward",
      {|#exe{extern class Item;class Box{Item item;U8 bytes[8];};Box object={0,{42,0,0,0,0,0,0,0}};StreamPrint("%d;",object.item.#exe{Print("A");class Item{U8 value;};}value);}|},
      "A" );
    ( "ordinary backing retains its original named target through body effects",
      {|#exe{U16 class Base{U8 value;};Base class Item{#exe{Print("A");U8 class Base{U8 value;};}U8 value;};class Box{Item item;$$=8;};Box actual={0x12a};I64 F(){U8 *p=(&actual)(U8*);return p[1]+41;}StreamPrint("%d;",F());}|},
      "A" );
    ( "ordinary backing waits for class-name lookahead",
      {|#exe{U16 class Item #exe{Print("A");class Box{Item item;U8 tail;$$=8;};Box object={9,42};}{U8 value;};StreamPrint("%d;",object.tail);}|},
      "A" );
    ( "ordinary backing waits for following-token lookahead",
      {|#exe{U16 class Item{U8 value;}#exe{Print("A");class Box{Item item;$$=8;};Box object={{42}};};StreamPrint("%d;",object.item.value);}|},
      "A" );
    ( "partial ordinary body exposes its reached direct members",
      {|#exe{U16 class Item{U8 value;#exe{Print("A");class Box{Item item;$$=8;};Box object={{42}};}};StreamPrint("%d;",object.item.value);}|},
      "A" );
    ( "static preparation uses reached partial direct members",
      {|#exe{U16 class Item{U8 value;#exe{Print("A");class Box{Item item;$$=8;};I64 F(){static Box object={{42}};return object.item.value;}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial member reads retain explicit offsets and array dimensions",
      {|#exe{class Item{U8 tag;$$=3;U16 values[2];#exe{Print("A");class Box{Item item;};I64 F(){static Box object={{1,{7,40}}};object.item.values[1]=40;U8 *p=(&object)(U8*);if(p[5]!=40)return -1;return object.item.values[1]+2;}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial signed member reads preserve original scalar storage",
      {|#exe{class Item{I8 value;#exe{Print("A");class Box{Item item;};I64 F(){static Box object={{-1}};return object.item.value+43;}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial member writes keep their original placement",
      {|#exe{class Item{U8 value;#exe{Print("A");class Box{Item item;};I64 F(){static Box object={{40}};object.item.value+=2;return object.item.value;}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial inherited member reads keep the selected base snapshot",
      {|#exe{class Base{U8 value;};class Item:Base{#exe{Print("A");class Box{Item item;};I64 F(){static Box object;object.item.value=40;return object.item.value+2;}}U8 tail;};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial anonymous union fields retain their shared original offset",
      {|#exe{class Item{U8 tag;union{U16 value;U8 bytes[2];};#exe{Print("A");class Box{Item item;};I64 F(){static Box object;object.item.value=0x12a;return object.item.bytes[0];}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "partial pointer fields retain owned static targets",
      {|#exe{class Item{U8 *value;#exe{Print("A");class Box{Item item;};I64 F(){static U8 target[1]={42};static Box object;object.item.value=target;return object.item.value[0];}}};StreamPrint("%d;",F());}|},
      "A" );
    ( "member lookahead observes class completion after the base identifier",
      {|#exe{extern class Item;class Box{Item item;$$=8;};I64 F(){static Box object;U8 *p=(&object)(U8*);p[0]=42;return object.item.#exe{Print("A");class Item{U8 value;};}value;}StreamPrint("%d;",F());}|},
      "A" );
    ( "pointer member lookahead observes completion after the base read",
      {|#exe{extern class Item;I64 F(Item *p){return p#exe{Print("A");class Item{I64 value;};}->value;}Item object={42};StreamPrint("%d;",F(&object));}|},
      "A" );
    ( "completion retains old forwarding until the new body finishes",
      {|#exe{extern U16 class Item;U8 class Item #exe{Print("A");class Box{Item item;$$=8;};Box object={0x12a};}{U8 value;};I64 F(){U8 *p=(&object)(U8*);return p[1]+41;}StreamPrint("%d;",F());}|},
      "A" );
    ( "unbacked completion retains prior extern forwarding",
      {|#exe{extern U16 class Item;class Item{U8 value;};class Box{Item item;$$=8;};Box object={0x12a};I64 F(){U8 *p=(&object)(U8*);return p[1]+41;}Print("A");StreamPrint("%d;",F());}|},
      "A" );
    ( "cached zero-width forwarding still evaluates its leaf",
      {|#exe{extern U0 class Item;class Box{Item items[2];U8 tail;$$=8;};I64 Side(){Print("A");StreamExePrint("U8 class Item{U8 value;};");return 7;}Box object={{Side(),9},42};StreamPrint("%d;",object.tail);}|},
      "A" );
    ( "global array caches forwarding before lexer completion",
      backed_array_lexer false,
      "A" );
    ( "static array caches forwarding before lexer completion",
      backed_array_lexer true,
      "A" );
    ( "global array retains forwarding across callee completion",
      backed_array_callee false,
      "A" );
    ( "static array retains forwarding across callee completion",
      backed_array_callee true,
      "A" );
    ( "forward backing is installed after class-name lookahead",
      backed_forward_timing,
      "A" );
    ("cached byte forwarding retains nested string copies", backed_strings, "A");
    ( "named backing preserves original targets through replacement",
      named_backed_array,
      "A" );
  ]

let both_values =
  ordinary_source_values @ backed_values
  @ [
      ( "global initialization retains the forward through same-name replacement",
        {|extern class Item;class Box{Item item;U8 tail;$$=8;};I64 Next(){Print("A");return 7;}Box object={Next(),42};class Item{U16 value;};class Item{U8 replacement;};I64 F(){return object.tail;}F();|},
        "A" );
      ( "global initialization sees completion before its original start",
        {|extern class Item;class Box{Item item;U8 tail;$$=8;};class Item{U16 value;};I64 Next(){Print("A");return 7;}Box object={{Next()},42};I64 F(){U8 *p=(&object)(U8*);return p[2];}F();|},
        "A" );
      ( "static initialization keeps the earlier scalar branch after completion",
        {|extern class Item;class Box{Item item;U8 tail;$$=8;};I64 F(){static Box object={7,42};return object.tail;}class Item{U16 value;};#exe{Print("A");}F();|},
        "A" );
      ( "static initialization survives same-name class replacement",
        {|extern class Item;class Box{Item item;U8 tail;$$=8;};I64 F(){static Box object={7,42};return object.tail;}class Item{U16 value;};class Item{U8 replacement;};#exe{Print("A");}F();|},
        "A" );
      ( "static initialization remains once-only across later calls",
        {|extern class Item;class Box{Item item;U8 tail;$$=8;};I64 F(){static Box object={7,40};return ++object.tail;}class Item{U16 value;};#exe{Print("A");}I64 Check(){F();F();return F()-1;}Check();|},
        "A" );
      ( "static member arrays retain their original zero-byte cursor",
        {|extern class Item;class Box{Item items[2];U8 tail;$$=8;};I64 F(){static Box object={{7,9},42};return object.tail;}class Item{U16 value;};#exe{Print("A");}F();|},
        "A" );
      ( "uncompleted scalar consumes zero bytes before the next leaf",
        {|#exe {extern class Item;class Box{Item item;U8 tail;$$=8;};I64 Side(){Print("A");return 7;}Box object={Side(),42};StreamPrint("%d;",object.tail);}|},
        "A" );
    ]

let source_activation_faults =
  [
    ( "ordinary source initializer output precedes later malformed input",
      {|class Box{I64 value;};I64 Next(){Print("A");return 42;}Box object={Next()};I64 Broken(garbage|},
      "HCPARSE0009" );
    ( "ordinary source initializer faults precede later malformed input",
      {|class Box{I64 value;};I64 Next(I64 divisor){Print("A");return 1/divisor;}Box object={Next(0)};I64 Broken(garbage|},
      "HCIRVM0009" );
  ]

let faults =
  source_activation_faults
  @ [
      ( "new ordinary forwarding cannot shrink a captured nominal copy",
        {|#exe{extern class Item;class Box{Item item;$$=8;};I64 Complete(){Print("A");StreamExePrint("U16 class Item{U8 value[9];};");return 42;}Box object={Complete()};}|},
        "HCRUN0006" );
      ( "cached global array backing cannot shrink around original bounds",
        {|#exe{extern U64 class Item;class Box{Item items[2];$$=1;};Box object={{#exe{Print("A");U8 class Item{U8 value;};}42,7}};}|},
        "HCRUN0006" );
      ( "cached static array backing cannot shrink around original bounds",
        {|#exe{extern U64 class Item;class Box{Item items[2];$$=1;};I64 F(){static Box object={{#exe{Print("A");U8 class Item{U8 value;};}42,7}};return 42;}}|},
        "HCRUN0006" );
      ( "completion cannot enlarge the original root",
        {|#exe {extern class Item;class Box{Item item;$$=1;};I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}Box object={Complete()};StreamPrint("42;");}|},
        "HCRUN0006" );
      ( "scalar copy cannot read beyond the computed register word",
        {|#exe {extern class Item;class Box{Item item;$$=16;};I64 Complete(){Print("A");StreamExePrint("class Item{U8 value[9];};");return 42;}Box object={Complete()};StreamPrint("42;");}|},
        "HCRUN0006" );
      ( "failed evaluation preserves its earlier output",
        {|#exe {extern class Item;class Box{Item item;$$=8;};I64 Z=0;I64 Bad(){Print("A");return 1/Z;}Box object={Bad()};StreamPrint("42;");}|},
        "HCIRVM0009" );
    ]

let static_current_leaf =
  {|#exe {
extern class Item;class Box{Item item;$$=8;};
I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}
I64 F(){static Box object={Complete()};return ++object.item.value;}
F();StreamPrint("%d;",F()-2);
}|}

let static_saved_tables =
  {|#exe {
extern class Item;class Box{Item item;U8 tail;$$=8;};
I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}
I64 F(){static Box object={Complete(),7};return object.tail;}
StreamPrint("%d;",F());
}|}

let static_nested_metadata =
  {|#exe {extern class Item;class Box{Item item;$$=8;};I64 Complete(){Print("A");StreamExePrint("I64 Next(){return 42;}class Item{U16 value data Next();};");return 42;}I64 F(){static Box object={Complete()};return object.item.value;}StreamPrint("%d;",F());}|}

let static_backing raw =
  Printf.sprintf
    {|#exe {
extern class Item;class Box{Item item;$$=8;};
I64 Complete(){Print("A");StreamExePrint("%s class Item{I64 value;};");return -1;}
I64 F(){static Box object={Complete()};U8 *p=(&object)(U8*);I64 i;for(i=0;i<8;i++)if(p[i]!=255)return -1;return 42;}
StreamPrint("%%d;",F());
}|}
    raw

let static_before_nested_brace =
  {|extern class Item;class Box{Item item;U8 tail;$$=8;};
I64 Next(){return 42;}
I64 F(){static Box object={#exe{Print("A");class Item{U16 value;};}{Next()},7};U8 *p=(&object)(U8*);if(p[2]!=7)return -1;return object.item.value;}F();|}

let static_before_nested_brace_task =
  "#exe{" ^ static_before_nested_brace ^ "StreamPrint(\"%d;\",F());}"

let grammar_faults =
  [
    ( "static nested branch is selected before opening-brace lookahead",
      {|extern class Item;class Box{Item item;$$=8;};I64 F(){static Box object={{#exe{Print("L");class Item{U16 value;};}42}};return object.item.value;}F();|},
      "" );
    ( "global nested branch is selected before opening-brace lookahead",
      {|extern class Item;class Box{Item item;$$=8;};Box object={{#exe{Print("L");class Item{U16 value;};}42}};I64 F(){return object.item.value;}F();|},
      "" );
    ( "static grammar failure retains the earlier successful leaf",
      {|extern class Item;class Box{U16 first;Item item;$$=8;};I64 First(){Print("A");return 7;}I64 F(){static Box object={First(),{#exe{Print("L");class Item{U16 value;};}42}};return object.item.value;}F();|},
      "A" );
  ]

let static_jit_values =
  [
    ( "completion before nested opening brace selects direct members",
      static_before_nested_brace,
      "A" );
    ( "active task retains grammar selected before nested braces",
      static_before_nested_brace_task,
      "A" );
    ( "static completion shares the child metadata allowance",
      static_nested_metadata,
      "A" );
    ( "static callee completion copies once before repeated calls",
      static_current_leaf,
      "A" );
    ( "static lookahead retains its original scalar branch",
      {|#exe {extern class Item;class Box{Item item;$$=8;};I64 F(){static Box object={42#exe{class Item{U16 value;};}};return object.item.value;}StreamPrint("%d;",F());}|},
      "" );
    ( "static array changes the next element branch",
      {|#exe {extern class Item;class Box{Item items[2];U8 tail;$$=8;};I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}I64 F(){static Box object={{Complete(),{40}},7};U8 *p=(&object)(U8*);if(p[2]!=40||p[4]!=7)return -1;return p[0];}StreamPrint("%d;",F());}|},
      "A" );
    ( "static completion retains its selected class after replacement",
      {|#exe {extern class Item;class Box{Item item;$$=8;};I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};class Item{U8 value;};");return 0x12a;}I64 F(){static Box object={Complete()};return object.item.value-256;}StreamPrint("%d;",F());}|},
      "A" );
    ( "static late U16 backing preserves the cached register word",
      static_backing "U16",
      "A" );
    ( "static late F64 backing preserves the cached register word",
      static_backing "F64",
      "A" );
  ]

let static_both_values =
  [
    ( "static incomplete member consumes zero bytes",
      {|#exe {extern class Item;class Box{Item item;U8 tail;$$=8;};I64 Side(){Print("A");return 7;}I64 F(){static Box object={Side(),42};return object.tail;}StreamPrint("%d;",F());}|},
      "A" );
  ]

let partial_member_faults =
  [
    ( "callback completion after the member name cannot authorize selection",
      {|#exe{extern class Item;I64 F(Item *p){return p->callback#exe{Print("A");class Item{I64 (*callback)(I64 n);};}(40);}StreamPrint("%d;",42);}|},
      "HCSEMA0039" );
    ( "partial callback ownership rejects an overlapping byte write",
      {|#exe{class Item{I64 (*callback)(I64 n);#exe{Print("A");class Box{Item item;};I64 Next(I64 n){return n+2;}I64 F(){static Box object;object.item.callback=&Next;U8 *p=(&object)(U8*);p[3]=p[3];return object.item.callback(40);}}};StreamPrint("%d;",F());}|},
      "HCIRVM0024" );
    ( "partial callback calls require their selected signature",
      {|#exe{class Item{I64 (*callback)();#exe{Print("A");class Box{Item item;};I64 Next(I64 n){return n+2;}I64 F(){static Box object;object.item.callback=&Next;return object.item.callback();}}};StreamPrint("%d;",F());}|},
      "HCIRVM0014" );
    ( "partial callback arrays retain their original root bounds",
      {|#exe{class Item{I64 (*callbacks)(I64 n)[2];#exe{Print("A");class Box{Item item;};I64 F(){static Box object;return object.item.callbacks[2](40);}}};StreamPrint("%d;",F());}|},
      "HCIRVM0019" );
    ( "partial callback lookup cannot borrow an unreached member",
      {|#exe{class Item{U8 value;#exe{Print("A");class Box{Item item;};I64 F(){static Box object;return object.item.callback(40);}}I64 (*callback)(I64 n);};StreamPrint("%d;",F());}|},
      "HCSEMA0039" );
    ( "unreached partial member cannot borrow later body completion",
      {|#exe{class Item{U8 value;#exe{Print("A");class Box{Item item;};I64 F(){static Box object={{42}};return object.item.later;}}U8 later;};StreamPrint("%d;",F());}|},
      "HCSEMA0046" );
    ( "completion after member selection cannot authorize the earlier read",
      {|#exe{extern class Item;class Box{Item item;$$=8;};Print("A");I64 F(){static Box object;return object.item.value#exe{class Item{U8 value;};};}StreamPrint("%d;",F());}|},
      "HCSEMA0046" );
    ( "partial member projection retains the original root bounds",
      {|#exe{class Item{U16 values[2];#exe{Print("A");class Box{Item item;};I64 F(){static Box object={{{40,42}}};return object.item.values[2];}}};StreamPrint("%d;",F());}|},
      "HCIRVM0019" );
    ( "top-level partial reads cannot borrow an unreached field",
      {|#exe{class Item{U8 value;#exe{Print("A");class Box{Item item;};Box object={{42}};StreamPrint("%d;",object.item.later);}U8 later;};}|},
      "HCSEMA0046" );
    ( "top-level completion after member selection cannot authorize the read",
      {|#exe{extern class Item;class Box{Item item;$$=8;};Box object;Print("A");StreamPrint("%d;",object.item.value#exe{class Item{U8 value;};});}|},
      "HCSEMA0046" );
    ( "top-level partial projections retain original root bounds",
      {|#exe{class Item{U16 values[2];#exe{Print("A");class Box{Item item;};Box object={{{40,42}}};StreamPrint("%d;",object.item.values[2]);}};}|},
      "HCIRVM0019" );
    ( "top-level replacement members cannot retarget the original object",
      {|#exe{class Item{U8 value;};Item object={42};class Item{U8 replacement;};Print("A");StreamPrint("%d;",object.replacement);}|},
      "HCSEMA0046" );
  ]

let static_faults =
  [
    ( "static completion cannot extend its original allocation",
      {|#exe {extern class Item;class Box{Item item;$$=1;};I64 Complete(){Print("A");StreamExePrint("class Item{U16 value;};");return 42;}I64 F(){static Box object={Complete()};return 42;}StreamPrint("%d;",F());}|},
      "HCRUN0006" );
    ( "static copy cannot read beyond its computed word",
      {|#exe {extern class Item;class Box{Item item;$$=16;};I64 Complete(){Print("A");StreamExePrint("class Item{U8 value[9];};");return 42;}I64 F(){static Box object={Complete()};return 42;}StreamPrint("%d;",F());}|},
      "HCRUN0006" );
    ( "static evaluation fault retains prior output",
      {|#exe {extern class Item;class Box{Item item;$$=8;};I64 Z=0;I64 Bad(){Print("A");return 1/Z;}I64 F(){static Box object={Bad()};return 42;}StreamPrint("%d;",F());}|},
      "HCIRVM0009" );
  ]
