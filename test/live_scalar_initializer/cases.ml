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

let both_values =
  [
    ( "uncompleted scalar consumes zero bytes before the next leaf",
      {|#exe {extern class Item;class Box{Item item;U8 tail;$$=8;};I64 Side(){Print("A");return 7;}Box object={Side(),42};StreamPrint("%d;",object.tail);}|},
      "A" );
  ]

let faults =
  [
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
