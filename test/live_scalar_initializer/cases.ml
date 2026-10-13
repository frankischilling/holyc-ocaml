let headers = "extern U0 Print(U8 *fmt,...);"

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
  backed_values
  @ [
      ( "uncompleted scalar consumes zero bytes before the next leaf",
        {|#exe {extern class Item;class Box{Item item;U8 tail;$$=8;};I64 Side(){Print("A");return 7;}Box object={Side(),42};StreamPrint("%d;",object.tail);}|},
        "A" );
    ]

let faults =
  [
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
    ( "unreached partial member cannot borrow later body completion",
      {|#exe{class Item{U8 value;#exe{Print("A");class Box{Item item;};I64 F(){static Box object={{42}};return object.item.later;}}U8 later;};StreamPrint("%d;",F());}|}
    );
    ( "completion after member selection cannot authorize the earlier read",
      {|#exe{extern class Item;class Box{Item item;$$=8;};Print("A");I64 F(){static Box object;return object.item.value#exe{class Item{U8 value;};};}StreamPrint("%d;",F());}|}
    );
    ( "partial member projection retains the original root bounds",
      {|#exe{class Item{U16 values[2];#exe{Print("A");class Box{Item item;};I64 F(){static Box object={{{40,42}}};return object.item.values[2];}}};StreamPrint("%d;",F());}|}
    );
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
