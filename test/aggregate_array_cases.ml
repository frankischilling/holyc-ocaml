let example =
  {|extern U0 Print(U8 *fmt,...);
class Item{U8 text[3];U16 value;};
I64 Bump(Item *p){return ++p->value;}
I64 F(){Item items[2][3];
items[1][2].text[0]=65;items[1][2].text[1]=66;items[1][2].text[2]=0;
Print("%s",items[1][2].text);items[1][2].value=40;
Bump(&items[1][2]);return Bump(&items[1][2]);}F();|}

let quota_source =
  {|class Box{U8 tag;U16 value;};I64 F(){Box items[2][3];
items[1][2].value=40;items[1][2].value+=2;return items[1][2].value;}F();|}

let proof_source =
  {|class Box{U8 tag;U16 value;};I64 F(){Box items[2];
items[1].value=42;return items[1].value;}F();|}

let values =
  [
    ( "unused automatic aggregate array",
      {|class Box{U8 byte;};I64 F(){Box objects[2];return 42;}F();|},
      42L,
      "" );
    ( "function-local class array",
      {|I64 F(){class Box{U8 byte;};Box a[2];return 42;}F();|},
      42L,
      "" );
    ( "standalone member array admission",
      {|class Box{U8 byte;};I64 F(){Box items[2];items[0].byte=42;return items[0].byte;}F();|},
      42L,
      "" );
    ("two-dimensional root and selected element parameter", example, 42L, "AB");
    ( "element stride differs from pointer and access widths",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];a[0].tag=2;a[0].value=20;a[1].tag=3;a[1].value=22;return a[0].value+a[1].value;}F();|},
      42L,
      "" );
    ( "array decay to first class pointer parameter",
      {|class Box{U16 value;};I64 Bump(Box *p){return ++p->value;}I64 F(){Box a[2];a[0].value=41;return Bump(a);}F();|},
      42L,
      "" );
    ( "row decay to selected class pointer parameter",
      {|class Box{U8 tag;U16 value;};I64 Bump(Box *p){return ++p->value;}I64 F(){Box a[2][3];a[1][0].value=41;return Bump(a[1]);}F();|},
      42L,
      "" );
    ( "element address retained across a branch",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];Box *p=&a[1];p->value=40;if(p->value)p->value++;return ++(*p).value;}F();|},
      42L,
      "" );
    ( "three-dimensional aggregate roots",
      {|class Box{U16 value;};I64 F(){Box a[2][2][3];a[1][1][2].value=40;return ++a[1][1][2].value+1;}F();|},
      42L,
      "" );
    ( "dynamic loop indices retain dimensions",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2][3];I64 sum=0,i,j;for(i=0;i<2;i++)for(j=0;j<3;j++){a[i][j].value=7;sum+=a[i][j].value;}return sum;}F();|},
      42L,
      "" );
    ( "indexed destination captured before RHS changes index",
      {|class Box{U16 value;};I64 F(){Box a[2];I64 i=0;a[0].value=20;a[1].value=99;a[i].value+=(i=1)*22;if(a[1].value!=99)return -1;return a[0].value;}F();|},
      42L,
      "" );
    ( "nested member arrays inside root arrays",
      {|class I{U8 tag;U16 values[2][3];};class Box{U8 tag;I items[2];};I64 F(){Box a[2][3];a[1][2].items[1].values[1][2]=40;return ++a[1][2].items[1].values[1][2]+1;}F();|},
      42L,
      "" );
    ( "primitive member address passed from indexed root",
      {|class Box{U8 tag;U16 value;};I64 Add(U16 *p){*p+=2;return *p;}I64 F(){Box a[2];a[1].value=40;return Add(&a[1].value);}F();|},
      42L,
      "" );
    ( "unions overlap within the selected array element",
      {|union Box{U64 word;U8 byte;};I64 F(){Box a[2];a[0].word=99;a[1].word=0x080706050403022a;a[1].byte+=2;if(a[0].word!=99)return -1;return a[1].word-0x0807060504030200;}F();|},
      44L,
      "" );
    ( "explicit padding participates in array stride",
      {|class Box{U8 tag;$$=16;U16 value;};I64 F(){Box a[2];a[0].value=20;a[1].value=22;return a[0].value+a[1].value;}F();|},
      42L,
      "" );
    ( "backing class array keeps member identity",
      {|U64 class Box{U8 value;};I64 F(){Box a[2];a[1].value=42;return a[1].value;}F();|},
      42L,
      "" );
    ( "later shadow cannot change selected element stride",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];a[1].value=41;return a[1].value;}class Box{U8 value;};F()+sizeof(Box);|},
      42L,
      "" );
    ( "byte view shares indexed root initialization",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[2];a[1].value=0x2a;U8 *p=(&a[1])(U8*);return p[1]+p[2];}F();|},
      42L,
      "" );
    ( "array dimensions use selected class sizeof",
      {|class Box{U8 tag;U16 value;};I64 F(){Box a[sizeof(Box)];a[2].value=42;return a[2].value;}F();|},
      42L,
      "" );
  ]

let view_matrix =
  List.map
    (fun spelling ->
      ( "root array field width " ^ spelling,
        Printf.sprintf
          "class Box{U8 tag;%s value;};I64 F(){Box \
           a[2];a[1].value=40;a[1].value++;return ++a[1].value;}F();"
          spelling,
        42L,
        "" ))
    [ "Bool"; "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ]

let faults =
  [
    ( "untouched neighboring element",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a[2];a[0].value=42;Print("kept");return a[1].value;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "partial union element is still unknown",
      {|extern U0 Print(U8 *fmt,...);union Box{U16 word;U8 byte;};I64 F(){Box a[2];a[1].byte=42;Print("kept");return a[1].word;}F();|},
      "HCIRVM0012",
      "kept" );
    ( "root array one past extent",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a[2];Print("kept");a[2].value=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "negative root index",
      {|extern U0 Print(U8 *fmt,...);class Box{U16 value;};I64 F(){Box a[2];Print("kept");a[-1].value=42;return 0;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "fresh array activation has unknown bytes",
      {|class Box{U16 value;};I64 once=0;I64 F(){Box a[2];if(once)return a[1].value;once=1;a[1].value=42;return a[1].value;}F();F();|},
      "HCIRVM0012",
      "" );
  ]

let unsupported =
  [
    ( "zero array extent",
      {|class Box{U8 byte;};I64 F(){Box a[0];return 42;}F();|} );
    ( "negative array extent",
      {|class Box{U8 byte;};I64 F(){Box a[-1];return 42;}F();|} );
    ( "aggregate extent multiplication overflow",
      {|class Box{U64 value;};I64 F(){Box a[0x7fffffffffffffff][2];return 42;}F();|}
    );
    ("empty class array", {|class Box{};I64 F(){Box a[2];return 42;}F();|});
    ("persistent class array", {|class Box{U8 byte;};Box a[2];42;|});
    ( "static class array",
      {|class Box{U8 byte;};I64 F(){static Box a[2];return 42;}F();|} );
    ( "aggregate array initializer",
      {|class Box{U8 byte;};I64 F(){Box a[2]={1,2};return 42;}F();|} );
    ( "whole root array element value",
      {|class Box{U8 byte;};I64 F(){Box a[2];a[0];return 42;}F();|} );
    ( "whole root array element copy",
      {|class Box{U8 byte;};I64 F(){Box a[2];a[0]=a[1];return 42;}F();|} );
    ( "class pointer array has no per-element ownership",
      {|class Box{U8 byte;};I64 F(){Box *a[2];return 42;}F();|} );
  ]

let extents =
  [
    ("class Box{U8 tag;U16 value;};", "[2][3]", 18);
    ("union Box{U64 word;U8 byte;};", "[2]", 16);
    ("class Box{U8 tag;$$=16;U16 value;};", "[2]", 36);
  ]

let extent_source (definition, dimensions, _) index =
  let first =
    String.to_seq dimensions
    |> Seq.filter (( = ) '[')
    |> Seq.map (fun _ -> "[0]")
    |> List.of_seq |> String.concat ""
  in
  Printf.sprintf
    "%s I64 F(){Box a%s;U8 *p=(&a%s)(U8*);p[%d]=42;return p[%d];}F();"
    definition dimensions first index index
