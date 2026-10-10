let headers = "extern U0 Print(U8 *fmt,...);extern U0 StreamPrint(U8 *fmt,...);"

let child_backed_type =
  {|I64 F(){#exe{U16 class C{U16 value;};}C object;object=40;return object+2;}F();|}

let completed_during_lookahead =
  {|I64 F(){extern class C;C object #exe{class C{U16 value;};};object.value=42;return object.value;}F();|}

let replaced_during_dimension =
  {|I64 F(){class C{U8 tag;U16 value;};C objects[2 #exe{class C{U8 byte;};}];objects[1].value=41;return objects[1].value+sizeof(C);}F();|}

let suffix_completion_boundary =
  {|I64 F(){extern class C;C *p;return p #exe{class C{I64 value;};}->value;}F();|}

let values =
  [
    ( "uncalled function publishes type",
      {|U0 Make(){class C{U8 a[42];};}sizeof(C);|} );
    ( "called function body",
      {|I64 F(){class C{U8 a[42];};return sizeof(C);}F();|} );
    ( "following function",
      {|U0 Make(){class C{U8 a[42];};}I64 F(){return sizeof(C);}F();|} );
    ( "saved default",
      {|U0 Make(){class C{U8 a[42];};}I64 F(I64 n=sizeof(C)){return n;}F;|} );
    ( "two declarations",
      {|I64 F(){class A{U8 a[20];};class B{U8 b[22];};return sizeof(A)+sizeof(B);}F();|}
    );
    ("union", {|U0 Make(){union C{U8 a[42];I64 b;};}sizeof(C);|});
    ( "forward completion",
      {|U0 Make(){extern class C;}class C{U8 a[42];};sizeof(C);|} );
    ( "comma forwards",
      {|U0 Make(){extern class A,extern union B;}class A{U8 a[20];};union B{U8 a[22];};sizeof(A)+sizeof(B);|}
    );
    ( "historical repeated name",
      {|I64 F(){class C{U8 a[20];};I64 n=sizeof(C);class C{U8 a[22];};return n+sizeof(C);}F();|}
    );
    ( "default keeps original class",
      {|U0 Make(){class C{U8 a[20];};}I64 F(I64 n=sizeof(C)){return n;}U0 Other(){class C{U8 a[22];};}F+sizeof(C);|}
    );
    ("empty class", {|U0 Make(){class C{};}sizeof(C)+42;|});
    ( "anonymous union",
      {|U0 Make(){class C{U8 a;union{U8 b[33];I64 c;}U8 d;};}sizeof(C)+7;|} );
    ( "following global array",
      {|U0 Make(){class C{U8 a[6];};}I64 A[sizeof(C)];sizeof(A)-6;|} );
    ( "unbraced if ends at class",
      {|I64 F(){I64 n=40;if(0)class C{U8 a[42];};n+=2;return n;}F()+sizeof(C)-42;|}
    );
    ("while body", {|U0 Make(){while(0){class C{U8 a[42];};}}sizeof(C);|});
    ("for body", {|U0 Make(){for(;0;){class C{U8 a[42];};}}sizeof(C);|});
    ("do body", {|U0 Make(){do{class C{U8 a[42];};}while(0);}sizeof(C);|});
    ( "switch body",
      {|U0 Make(){switch(0){case 1:class C{U8 a[42];};}}sizeof(C);|} );
    ("nested ordinary block", {|{class C{U8 a[42];};}sizeof(C);|});
    ( "preprocessor visibility",
      {|U0 Make(){class C{U8 a[42];};}
#ifdef C
sizeof(C);
#else
0;
#endif
|} );
    ( "class in generated stream",
      {|#exe {StreamPrint("U0 Make(){class C{U8 a[42];};}sizeof(C);");}|} );
    ( "constant aggregate offset",
      {|U0 Make(){class C{U8 a;$$=34;U8 b[8];};}sizeof(C);|} );
    ("callback member", {|U0 Make(){class C{U0 (*cb)();U8 a[34];};}sizeof(C);|});
    ("public declaration", {|U0 Make(){public class C{U8 a[42];};}sizeof(C);|});
    ( "comma class definitions",
      {|U0 Make(){class A{U8 a[20];},union B{U8 b[22];};}sizeof(A)+sizeof(B);|}
    );
    ( "local class object",
      {|I64 F(){class C{I64 value;};C object;object.value=40;return object.value+2;}F();|}
    );
    ( "outer backed object survives a local replacement",
      {|U16 class C{U16 value;};I64 F(){C object;object=40;class C{U8 replacement[2];};return object+sizeof(C);}F();|}
    );
    ( "default class whole value",
      {|I64 F(){class C{I64 value;};C object;object=42;return object;}F();|} );
    ( "local union byte view",
      {|I64 F(){union U{U8 bytes[8];I64 word;};U object;object.word=42;return object.bytes[0];}F();|}
    );
    ( "multidimensional local class objects",
      {|I64 F(){class C{U16 value;};C objects[2][2];objects[1][1].value=42;return objects[1][1].value;}F();|}
    );
    ( "nested local class members",
      {|I64 F(){class Inner{U16 value;};class Outer{U8 tag;Inner nested;};Outer object;object.nested.value=42;return object.nested.value;}F();|}
    );
    ( "inherited local class members",
      {|I64 F(){class Base{U16 first;};class Child:Base{U8 second;};Child object;object.first=40;object.second=2;return object.first+object.second;}F();|}
    );
    ( "local class pointer stride",
      {|I64 F(){class C{U16 value;U8 extra;};C objects[2];C *p=objects;objects[1].value=40;p++;p->extra=2;return p->value+p->extra;}F();|}
    );
    ( "same-name local objects keep their types",
      {|I64 F(){class C{U8 first;};C old;old.first=20;class C{U16 second;};C newer;newer.second=22;return old.first+newer.second;}F();|}
    );
    ( "local class replaces an outer spelling",
      {|class C{U8 first;};I64 F(){C old;old.first=20;class C{U16 second;};C newer;newer.second=22;return old.first+newer.second;}F();|}
    );
    ( "skipped declaration still completes the local type",
      {|I64 F(){if(0)class C{I64 value;};C object;object.value=42;return object.value;}F();|}
    );
    ( "recursive local objects have separate storage",
      {|I64 F(I64 n){class C{I64 value;};C object;object.value=n;if(n)return object.value+F(n-1);return 0;}F(8)+6;|}
    );
    ( "retained local object after class replacement",
      {|I64 F(){class C{I64 value;};C object;object.value=42;return object.value;}F();class C{U8 replacement[3];};F();|}
    );
    ( "retained caller keeps local class and function",
      {|I64 F(){class C{U16 value;};C object;object.value=40;return object.value;}I64 G(){return F()+2;}G();I64 F(){return 0;}class C{U8 replacement[3];};G();|}
    );
    ( "generated function-local objects",
      {|#exe {StreamPrint("I64 F(){class C{U16 value;};C object;object.value=42;return object.value;}F();");}|}
    );
    ( "local forward pointer completed before access",
      {|I64 F(){extern class C;C *p;class C{U16 value;};C object;p=&object;p->value=42;return p->value;}F();|}
    );
    ( "local default classes retain separate call conversions",
      {|I64 Id(I64 n){return n;}I64 F(){class C{I64 value;};C object;object=20;return Id(object);}I64 G(){class C{I64 value;};C object;object=22;return Id(object);}F()+G();|}
    );
    ( "do condition follows the local declaration",
      {|I64 F(){do{class C{I64 value;};C object;object.value=42;}while(object.value!=42);return object.value;}F();|}
    );
  ]

let jit_values =
  [
    ("child declaration supplies explicit backing", child_backed_type);
    ( "local name lookahead completes selected forward",
      completed_during_lookahead );
    ("array lookahead keeps original selected class", replaced_during_dimension);
  ]

let effects =
  {|I64 N=34;I64 Count(){Print("dim");return 8;}I64 Offset(){Print("off");return N;}
U0 Make(){class C{U8 a;$$=Offset();U8 b[Count()];};}sizeof(C);|}

let reached_failure =
  {|I64 Count(){Print("kept");return 42;}U0 Make(){class C{U8 a[Count()];} variable;}|}

let local_position = {|I64 F(){I64 #exe {Print("P");} n=40;return n+2;}F();|}

let object_effects =
  {|I64 N=2;I64 Count(){Print("dim");return N;}I64 Offset(){Print("off");return 1;}
I64 F(){class C{U8 tag;$$=Offset();U16 values[Count()];};C object;object.values[1]=40;return object.values[1]+2;}
F();N=1;F();|}

let object_failures =
  [
    ( "HCSEMA0074",
      {|I64 F(){extern class C;C object;class C{I64 value;};object.value=42;return object.value;}F();|}
    );
    ( "HCSEMA0046",
      {|I64 F(){extern class C;C *p;return p->value;class C{I64 value;};}F();|}
    );
    ( "HCSEMA0046",
      {|I64 F(){extern class C;C *p;return p->value;#exe{class C{I64 value;};}}F();|}
    );
    ( "HCSEMA0046",
      {|I64 F(){extern class C;C *p;I64 n=0;for(;n<0;p->value++){class C{I64 value;};}return 42;}F();|}
    );
    ( "HCSEMA0046",
      {|I64 F(){class C{I64 original;};C object;class C{I64 replacement;};object.replacement=42;return 0;}F();|}
    );
    ( "HCIRVM0012",
      {|I64 F(){class C{I64 value;};C object;return object.value;}F();|} );
    ( "HCIRVM0019",
      {|I64 F(){class C{I64 value;};C objects[1];C *p=objects;p++;return p->value;}F();|}
    );
  ]
