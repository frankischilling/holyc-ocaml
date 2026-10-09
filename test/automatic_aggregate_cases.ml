let example =
  {|extern U0 Print(U8 *fmt,...);
class Packed{U8 text[3];U16 value;};
I64 F(){Packed object;U8 *p=(&object)(U8*);
p[0]=65;p[1]=66;p[2]=0;Print("%s",p);
U16 *q=(p+3)(U16*);*q=40;*q+=2;return *q;}F();|}

let quota_source =
  {|class Box{U8 bytes[9];};I64 F(){Box object;U8 *p=(&object)(U8*);
p[8]=42;return p[8];}F();|}

let values =
  [
    ("packed unaligned window", example, 42L, "AB");
    ( "named union overlap",
      {|union Word{U64 value;U8 bytes[8];};I64 F(){Word object;
U64 *q=(&object)(U64*);*q=0x080706050403022a;
U8 *p=(&object)(U8*);return p[0];}F();|},
      42L,
      "" );
    ( "nested aggregate storage",
      {|class Inner{U8 tag;U16 value;};class Outer{U8 prefix;Inner item;U8 tail;};
I64 F(){Outer object;U8 *p=(&object)(U8*);
U16 *q=(p+2)(U16*);*q=42;
p[4]=99;return *q;}F();|},
      42L,
      "" );
    ( "anonymous union",
      {|class Box{U8 tag;union{U64 value;U8 bytes[8];}U8 tail;};
I64 F(){Box object;U8 *p=(&object)(U8*);
U64 *q=(p+1)(U64*);*q=0x080706050403022a;
p[9]=99;return p[1];}F();|},
      42L,
      "" );
    ( "explicit padding stays inside object",
      {|class Box{U8 tag;$$=16;U8 tail[2];};I64 F(){Box object;
U8 *p=(&object)(U8*);p[17]=42;return p[17];}F();|},
      42L,
      "" );
    ( "source-selected identity survives shadow",
      {|class C{U8 bytes[3];};I64 F(){C object;U8 *p=(&object)(U8*);
p[2]=41;return p[2];}class C{U8 byte;};F()+sizeof(C);|},
      42L,
      "" );
    ( "backing class preserves storage identity",
      {|U64 union Bits{U8 bytes[8];};I64 F(){Bits object;
U64 *q=(&object)(U64*);*q=42;return *q;}F();|},
      42L,
      "" );
    ( "primitive parameter retains aggregate storage",
      {|class Box{U8 bytes[9];};I64 Set(U16 *p){*p=42;return *p;}
I64 F(){Box object;U8 *p=(&object)(U8*);return Set((p+7)(U16*));}F();|},
      42L,
      "" );
    ( "RHS changes pointer after captured destination",
      {|class Box{U8 byte;};I64 F(){Box a,b;U8 *p=(&a)(U8*),*q=(&b)(U8*);
*p=20;*q=22;*p+=*(p=q);return *(&a)(U8*);}F();|},
      42L,
      "" );
    ( "fresh activation does not reuse known bytes",
      {|class Box{U8 byte;};I64 F(I64 n){Box object;U8 *p=(&object)(U8*);
*p=n;return *p;}F(20)+F(22);|},
      42L,
      "" );
  ]

let classes =
  [
    ("Bool", 1, false);
    ("I8", 1, false);
    ("U8", 1, true);
    ("I16", 2, false);
    ("U16", 2, true);
    ("I32", 4, false);
    ("U32", 4, true);
    ("I64", 8, false);
    ("U64", 8, true);
  ]

let view_matrix =
  List.map
    (fun (view, width, unsigned) ->
      let contents =
        Printf.sprintf
          "class Box{U8 bytes[8];};I64 F(){Box object;U64 \
           *q=(&object)(U64*);*q=-1;%s *p=(&object)(%s*);return *p;}F();"
          view view
      in
      let expected =
        if unsigned && width < 8 then
          Int64.pred (Int64.shift_left 1L (width * 8))
        else -1L
      in
      (view, contents, expected, ""))
    classes

let faults =
  [
    ( "unwritten adjacent byte",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 bytes[2];};
I64 F(){Box object;U8 *p=(&object)(U8*);p[0]=42;Print("kept");
return *(&object)(U16*);}F();|},
      "HCIRVM0012",
      "kept" );
    ( "one-past store",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 bytes[7];};
I64 F(){Box object;U8 *p=(&object)(U8*);p[0]=42;Print("kept");
p[7]=99;return p[0];}F();|},
      "HCIRVM0019",
      "kept" );
    ( "wider store crosses object extent",
      {|extern U0 Print(U8 *fmt,...);class Box{U8 bytes[7];};
I64 F(){Box object;U8 *p=(&object)(U8*);p[0]=42;Print("kept");
*(&object)(U64*)=99;return p[0];}F();|},
      "HCIRVM0019",
      "kept" );
    ( "activation has independent byte initialization",
      {|class Box{U8 byte;};I64 F(I64 n){Box object;U8 *p=(&object)(U8*);
if(n)*p=42;return *p;}F(1);F(0);|},
      "HCIRVM0012",
      "" );
  ]

let unsupported =
  [
    ( "inherited metadata is not runtime layout authority",
      {|class Base{U8 byte;};class Child:Base{U8 tail;};
I64 F(){Child object;return 42;}F();|}
    );
    ("zero-sized object", {|class Box{};I64 F(){Box object;return 42;}F();|});
    ( "aggregate array",
      {|class Box{U8 byte;};I64 F(){Box objects[2];return 42;}F();|} );
    ( "aggregate pointer return",
      {|class Box{U8 byte;};Box *Get(Box *p){return p;}I64 F(){Box object;return Get(&object)->byte;}F();|}
    );
    ( "whole-object copy",
      {|class Box{U8 byte;};I64 F(){Box a,b=a;return 42;}F();|} );
    ("persistent aggregate storage", {|class Box{U8 byte;};Box object;42;|});
    ( "numeric bits do not grant aggregate ownership",
      {|class Box{U8 byte;};I64 F(){U8 *p=42(Box*)(U8*);return *p;}F();|} );
    ( "later forward completion cannot resize an earlier frame",
      {|extern class Box;I64 F(){Box object;return 42;}class Box{U8 bytes[8];};F();|}
    );
  ]

let extents =
  [
    ("class Box{U8 tag;U16 value;};", 3);
    ("union Box{U64 value;U8 bytes[8];};", 8);
    ("class Box{U8 tag;union{U64 value;U8 bytes[8];}U8 tail;};", 10);
    ( "class Inner{U8 tag;U16 value;};class Box{U8 prefix;Inner item;U8 tail;};",
      5 );
    ("class Box{U8 tag;$$=16;U8 tail[2];};", 18);
    ("U64 union Box{U8 bytes[8];};", 8);
  ]

let extent_source definition offset =
  Printf.sprintf
    "%s I64 F(){Box object;U8 *p=(&object)(U8*);p[%d]=42;return p[%d];}F();"
    definition offset offset
