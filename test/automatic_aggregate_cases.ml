let example =
  {|extern U0 Print(U8 *fmt,...);
class Packed{U8 text[3];U16 value;};
I64 F(){Packed object;U8 *p=(&object)(U8*);
p[0]=65;p[1]=66;p[2]=0;Print("%s",p);
U16 *q=(p+3)(U16*);*q=40;*q+=2;return *q;}F();|}

let quota_source =
  {|class Box{U8 bytes[9];};I64 F(){Box object;U8 *p=(&object)(U8*);
p[8]=42;return p[8];}F();|}

let empty_quota_source =
  {|class Empty{};I64 F(){Empty first,second,objects[2][3];return sizeof(first)+sizeof(second)+sizeof(objects)+42;}F();|}

let initializer_quota_source =
  {|U16 class Box{U16 value;U8 guard;};I64 F(){Box object=40;object.guard=2;return object.value+object.guard;}F();|}

let initializer_proof_source =
  {|U16 class Box{U8 tag;U8 value;};I64 F(){Box object=10752;return object.value;}I64 G(){Box object=10752;return object.value;}F();|}

let source_values =
  [
    ( "source-generated initializers use their own allocation",
      {|#exe{U16 class Word{U16 value;};I64 F(){Word object=42;return object.value;}Print("A");StreamPrint("%d;",F());}|},
      42L,
      "A" );
    ( "partial class bodies retain the allocation reached in nested input",
      {|#exe{class Item{U8 first;#exe{I64 F(){Item object;object.first=42;return object.first;}}U8 later;};Print("A");StreamPrint("%d;",F());}|},
      42L,
      "A" );
    ( "member lookahead observes base completion without resizing the frame",
      {|#exe{extern class Base;class Child:Base{U8 tail;};I64 F(){Child object;object.tail=40;object.#exe{class Base{U8 head;};}head+=2;return object.tail;}Print("A");StreamPrint("%d;",F());}|},
      42L,
      "A" );
  ]

let values =
  [
    ( "automatic aggregate literal initializes its integer backing",
      {|U16 class Box{U16 value;U8 guard;};I64 F(){Box object=298;object.guard=7;return object.value-256;}F();|},
      42L,
      "" );
    ( "automatic integer-backed initializer writes its register prefix",
      {|U64 class Box{U64 word;U8 guard;};I64 F(){Box object=42;object.guard=7;return object.word;}F();|},
      42L,
      "" );
    ( "automatic comma initializers preserve calls and order",
      {|extern U0 Print(U8 *fmt,...);U16 class Box{U16 value;};I64 Next(I64 n){Print("%c",n+45);return n;}I64 F(){Box first=Next(20),second=Next(21);return first.value+second.value+1;}F();|},
      42L,
      "AB" );
    ( "automatic initializer reads the earlier selected object",
      {|U16 class Box{U16 value;};I64 F(){Box first=40,second=first+2;return second.value;}F();|},
      42L,
      "" );
    ( "automatic initializer preserves class identity after replacement",
      {|U16 class Box{U16 value;};I64 F(){Box object=298;return object.value-256;}U8 class Box{U8 replacement;};F();|},
      42L,
      "" );
    ( "automatic initializer follows a named backing chain",
      {|U16 class Word{U16 value;};Word class Box{U16 value;U8 guard;};I64 F(){Box object=298;return object.value-256;}F();|},
      42L,
      "" );
    ( "automatic initializer writes inherited storage without a whole copy",
      {|class Base{U64 word;};U64 class Box:Base{U8 guard;};I64 F(){Box object=42;object.guard=7;return object.word;}F();|},
      42L,
      "" );
    ( "automatic initializer runs separately in each activation",
      {|I64 Count=0;U16 class Box{U16 value;};I64 Next(){Count++;return 19+Count;}I64 F(){Box object=Next();return object.value;}F()+F()+1;|},
      42L,
      "" );
    ( "forward automatic allocation keeps its original zero extent",
      {|extern class Box;I64 F(){Box object;return sizeof(object)+42;}class Box{U8 bytes[8];};F();|},
      42L,
      "" );
    ( "forward arrays retain their original zero strides after completion",
      {|extern class Box;I64 F(){Box objects[2][3];U8 *p=(&objects[1][2])(U8*);if(p!=p)return -1;return sizeof(objects)+42;}class Box{U16 value;};F();|},
      42L,
      "" );
    ( "later allocations observe completion without growing earlier frames",
      {|extern class Box;I64 Before(){Box object;return sizeof(object);}class Box{U8 value;};I64 After(){Box object;object.value=41;return object.value;}Before()+After()+1;|},
      42L,
      "" );
    ( "forward base storage uses the original child extent",
      {|extern class Base;class Child:Base{U8 tail;};I64 F(){Child object;object.tail=42;return object.tail;}class Base{U64 later;};F();|},
      42L,
      "" );
    ( "reached base completion supplies overlapping inherited members",
      {|extern class Base;class Child:Base{U8 tail;};class Base{U8 head;};I64 F(){Child object;object.tail=40;object.head+=2;return object.tail;}F();|},
      42L,
      "" );
    ( "multilevel inherited lookup retains completed canonical bases",
      {|extern class Base;class Middle:Base{U8 tag;};class Leaf:Middle{U8 tail;};class Base{U8 head;};I64 Read(Leaf *p){return p->head+p->tag+p->tail-40;}I64 F(){Leaf objects[2];objects[1].tag=40;objects[1].tail=2;return Read(&objects[1]);}F();|},
      42L,
      "" );
    ( "base replacement cannot retarget the original inherited chain",
      {|extern class Base;class Child:Base{U8 tail;};class Base{U8 head;};class Base{U64 other;};I64 F(){Child object;object.tail=40;object.head+=2;return object.tail;}F();|},
      42L,
      "" );
    ( "original inherited statics retain their allocation and reached fields",
      {|extern class Base;class Child:Base{U8 tail;};class Base{U8 head;};I64 F(){static Child object={40};object.head++;return object.tail;}F();F();|},
      42L,
      "" );
    ( "empty automatic roots retain zero frame bytes",
      {|class Empty{};I64 F(){Empty first,second;return sizeof(first)+sizeof(second)+42;}F();|},
      42L,
      "" );
    ( "empty roots keep original addresses beside scalar storage",
      {|class Empty{};I64 F(){U8 before=40;Empty first,second;U8 after=2;U8 *p=(&first)(U8*),*q=(&second)(U8*);if(p!=q||p!=(&before)(U8*))return -1;return before+after+sizeof(first);}F();|},
      42L,
      "" );
    ( "fixed empty aggregate arrays keep zero strides",
      {|class Empty{};I64 F(){Empty objects[2][3];U8 *p=(&objects[1][2])(U8*);if(p!=p)return -1;return sizeof(objects)+42;}F();|},
      42L,
      "" );
    ( "empty unions and inherited roots remain addressable",
      {|union Base{U0 value;};class Empty:Base{};I64 F(){Base a;Empty b;U8 *p=(&a)(U8*),*q=(&b)(U8*);if(p!=p||q!=q)return -1;return sizeof(a)+sizeof(b)+42;}F();|},
      42L,
      "" );
    ( "original empty frame survives later class replacement",
      {|class Empty{};I64 F(){Empty object;return sizeof(object)+42;}class Empty{U8 bytes[8];};F();|},
      42L,
      "" );
    ( "empty pointers retain caller storage across calls",
      {|class Empty{};Empty *Keep(Empty *p){return p;}I64 Read(Empty *p){if(p!=p)return -1;return 42;}I64 F(){Empty object;return Read(Keep(&object));}F();|},
      42L,
      "" );
    ( "recursive empty roots keep activation owners",
      {|class Empty{};I64 F(I64 n){Empty object;Empty *p=&object;if(p!=p)return -1;if(n)return F(n-1);return sizeof(object)+42;}F(3);|},
      42L,
      "" );
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

let initializer_matrix =
  List.map
    (fun (type_, width, unsigned) ->
      let expected =
        if unsigned && width < 8 then
          Int64.pred (Int64.shift_left 1L (width * 8))
        else -1L
      in
      ( "automatic initializer " ^ type_,
        Printf.sprintf
          "%s class Box{%s value;};I64 F(){Box object=-1;return \
           object.value;}F();"
          type_ type_,
        expected,
        "" ))
    classes

let faults =
  [
    ( "empty backed initializer cannot write an adjacent local",
      {|extern U0 Print(U8 *fmt,...);U8 class Empty{};I64 Value(){Print("kept");return 42;}I64 F(){U8 neighbor=7;Empty object=Value();return neighbor+sizeof(object);}F();|},
      "HCIRVM0019",
      "kept" );
    ( "initializer does not initialize the rest of its class",
      {|U16 class Box{U16 value;U8 guard;};I64 F(){Box object=42;return object.guard;}F();|},
      "HCIRVM0012",
      "" );
    ( "original initializer extent cannot borrow a wider backing",
      {|extern U0 Print(U8 *fmt,...);U16 class Box{U8 byte;};I64 Value(){Print("kept");return 42;}I64 F(){Box object=Value();return sizeof(object);}F();|},
      "HCIRVM0019",
      "kept" );
    ( "completion cannot add bytes to an earlier forward frame",
      {|extern U0 Print(U8 *fmt,...);extern class Box;I64 F(){Box object;Print("kept");return *(&object)(U8*);}class Box{U8 value;};F();|},
      "HCIRVM0019",
      "kept" );
    ( "completed base member cannot exceed the original child allocation",
      {|extern U0 Print(U8 *fmt,...);extern class Base;class Child:Base{U8 tail;};class Base{U16 wide;};I64 F(){Child object;object.tail=42;Print("kept");return object.wide;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "empty array dimensions still check each index",
      {|extern U0 Print(U8 *fmt,...);class Empty{};I64 F(){Empty objects[2][3];Print("kept");U8 *p=(&objects[2][0])(U8*);return 42;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "empty array address cannot expose the adjacent local",
      {|extern U0 Print(U8 *fmt,...);class Empty{};I64 F(){U8 neighbor=42;Empty objects[2][3];Print("kept");return *(&objects[1][2])(U8*);}F();|},
      "HCIRVM0019",
      "kept" );
    ( "empty root cannot read an adjacent initialized local",
      {|extern U0 Print(U8 *fmt,...);class Empty{};I64 F(){U8 neighbor=42;Empty object;Print("kept");return *(&object)(U8*);}F();|},
      "HCIRVM0019",
      "kept" );
    ( "empty root cannot write an adjacent local",
      {|extern U0 Print(U8 *fmt,...);class Empty{};I64 F(){U8 neighbor=42;Empty object;Print("kept");*(&object)(U8*)=7;return neighbor;}F();|},
      "HCIRVM0019",
      "kept" );
    ( "expired empty roots cannot supply a byte",
      {|extern U0 Print(U8 *fmt,...);class Empty{};U8 *Get(){Empty object;Print("kept");return (&object)(U8*);}I64 F(){U8 *p=Get();return *p;}F();|},
      "HCIRVM0018",
      "kept" );
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
    ( "unbacked class is not an automatic assignment lvalue",
      {|class Box{U64 word;};I64 F(){Box object=42;return object.word;}F();|} );
    ( "ordinary class backing cannot manufacture an internal lvalue",
      {|class Word{U64 word;};Word class Box{U64 value;};I64 F(){Box object=42;return object.value;}F();|}
    );
    ( "unreached inherited fields cannot borrow later completion",
      {|extern class Base;class Child:Base{U8 tail;};I64 F(){Child object;return object.head;}class Base{U8 head;};F();|}
    );
    ( "replacement-only base members are absent from the original chain",
      {|extern class Base;class Child:Base{U8 tail;};class Base{U8 head;};class Base{U8 replacement;};I64 F(){Child object;return object.replacement;}F();|}
    );
    ( "aggregate pointer array",
      {|class Box{U8 byte;};I64 F(){Box *objects[2];return 42;}F();|} );
    ( "aggregate pointer return",
      {|class Box{U8 byte;};Box *Get(Box *p){return p;}I64 F(){Box object;return Get(&object)->byte;}F();|}
    );
    ( "whole-object copy",
      {|class Box{U8 byte;};I64 F(){Box a,b=a;return 42;}F();|} );
    ( "numeric bits do not grant aggregate ownership",
      {|class Box{U8 byte;};I64 F(){U8 *p=42(Box*)(U8*);return *p;}F();|} );
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
