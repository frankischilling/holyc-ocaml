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

let quota_source =
  {|class Box{U64 word;};I64 F(){Box o;o.word=40;o(U8)+=2;return o(U8);}F();|}

let quota_frame_bytes = 8

let proof_source =
  {|U16 class Word{U16 value;};class Root{U8 tag;Word word;};class View{U16 value;};
I64 F(){Root o;o.word.value=42;return o.word(View).value;}
I64 G(){Root o;o.word.value=42;return o.word(View).value;}F();|}

let values =
  List.concat_map
    (fun (backing, _) ->
      List.map
        (fun (target, expected) ->
          ( "ungrouped " ^ backing ^ " class reads " ^ target,
            Printf.sprintf
              {|%s class Box{U64 word;};I64 F(){Box o;o.word=-1;if(o(%s)!=%s)return -2;o.word=42;return o(%s);}F();|}
              backing target expected target,
            42L,
            "" ))
        widths)
    widths
  @ List.concat_map
      (fun (target, expected) ->
        [
          ( "default class storage cast " ^ target,
            Printf.sprintf
              {|class Box{U64 word;};I64 F(){Box o;o.word=-1;if(o(%s)!=%s)return -2;o.word=42;return o(%s);}F();|}
              target expected target,
            42L,
            "" );
          ( "grouped class cast preserves register bits " ^ target,
            Printf.sprintf
              {|%s class Box{U64 word;};class Raw{U64 word;};I64 F(){Raw o;o.word=0x1002a;return (o)(Box);}F();|}
              target,
            0x1002aL,
            "" );
          ( "literal class cast preserves register bits " ^ target,
            Printf.sprintf
              {|%s class Box{U64 word;};I64 F(){return 0x1002a(Box);}F();|}
              target,
            0x1002aL,
            "" );
          ( "primitive storage cast selects class backing " ^ target,
            Printf.sprintf
              {|%s class Box{U64 word;};I64 F(){I64 raw=-1;if(raw(Box)!=%s)return -2;raw=42;return raw(Box);}F();|}
              target expected,
            42L,
            "" );
          ( "grouped backing read keeps its original width " ^ target,
            Printf.sprintf
              {|%s class Box{U64 word;};I64 F(){Box o;o.word=-1;return (o)(I64);}F();|}
              target,
            Int64.of_string expected,
            "" );
        ])
      widths
  @ [
      ("cast byte update preserves other bytes", quota_source, 42L, "");
      ( "ungrouped default cast reads one initialized byte",
        {|class Box{U8 bytes[8];};I64 F(){Box o;o.bytes[0]=42;return o(U8);}F();|},
        42L,
        "" );
      ( "cast reads the exact one-byte root",
        {|class Box{U8 byte;};I64 F(){Box o;o.byte=42;return o(U8);}F();|},
        42L,
        "" );
      ( "cast destination writes only its selected bytes",
        {|class Box{U64 word;};I64 F(){Box o;o.word=0x10000;o(U8)=42;return o;}F();|},
        0x1002aL,
        "" );
      ( "cast prefix and postfix updates use selected storage",
        {|class Box{U64 word;};I64 F(){Box o;o.word=0x10026;if(o(U8)++!=38 || ++o(U8)!=40)return -1;o(U8)+=2;return o(U8);}F();|},
        42L,
        "" );
      ( "grouped update cast retains completed register value",
        {|class Box{U64 word;};I64 F(){Box o;o.word=0x1002a;if((o++)(U8)!=0x1002a)return -1;return o;}F();|},
        0x1002bL,
        "" );
      ( "consecutive storage casts select the final width",
        {|class Box{U64 word;};I64 F(){Box o;o.word=0x1002a;return o(U8)(U32);}F();|},
        0x1002aL,
        "" );
      ( "consecutive grouped casts preserve the original register word",
        {|class Box{U64 word;};I64 F(){Box o;o.word=0x1002a;return (o)(U8)(U16);}F();|},
        0x1002aL,
        "" );
      ( "grouping a completed storage cast fixes its read width",
        {|class Box{U64 word;};I64 F(){Box o;o.word=0x1002a;return (o(U8))(U32);}F();|},
        42L,
        "" );
      ("class storage cast exposes the selected member", proof_source, 42L, "");
      ( "union cast selects overlapping scalar bytes",
        {|union Box{U64 word;U8 bytes[8];};I64 F(){Box o;o.word=0x1002a;if(o(U8)!=42 || (o)(U8)!=0x1002a)return -1;return o.bytes[0];}F();|},
        42L,
        "" );
      ( "inherited cast storage keeps original member offsets",
        {|class Base{U16 value;};class Root:Base{U8 rest[6];};class View{U16 value;};I64 F(){Root o;o(I64)=42;return o(View).value;}F();|},
        42L,
        "" );
      ( "cast destination is captured before RHS rebinding",
        {|class Box{U64 word;};I64 F(){Box a[2];a[0].word=20;a[1].word=99;Box *p=a;p[0](U8)+=((p=&a[1])==&a[1])*22;if(a[1]!=99)return -1;return a[0];}F();|},
        42L,
        "" );
      ( "cast member address retains its original byte offset",
        {|U16 class Part{U16 value;U8 guard;};class Root{U8 tag;Part parts[2][2];};I64 F(){Root o;o.parts[1][1].guard=9;o.parts[1][1](U16)=40;U8 *p=&o.parts[1][1](U8);*p+=2;if(o.parts[1][1].guard!=9)return -1;return o.parts[1][1];}F();|},
        42L,
        "" );
      ( "cast widens within the original containing root",
        {|U8 class Part{U8 byte;};class Root{Part part;U8 rest[3];};I64 F(){Root o;o.part.byte=42;o.rest[0]=0;o.rest[1]=0;o.rest[2]=0;return o.part(U32);}F();|},
        42L,
        "" );
      ( "function-local storage and value casts",
        {|I64 F(){class View{U16 value;};class Raw{U64 word;};Raw o;o.word=0x1002a;if(o(View).value!=42 || (o)(View)!=0x1002a)return -1;return o(View).value;}F();|},
        42L,
        "" );
      ( "persistent class cast aliases original bytes",
        {|class Box{U64 word;};Box global={40};I64 F(){static Box local={2};global(U8)+=local(U8);return global(U8);}F();|},
        42L,
        "" );
      ( "class register cast enters fixed arguments and returns",
        {|U16 class Word{U16 value;};Word Give(){return 40(Word);}I64 Take(Word o){return o+2;}Take(Give());|},
        42L,
        "" );
      ( "register cast does not copy a class tail",
        {|U16 class Word{U16 value;U8 guard;};I64 F(){Word o;o.guard=9;o=0x1002a(Word);if(o.guard!=9)return -1;return o;}F();|},
        42L,
        "" );
    ]

let retained_values =
  [
    ( "cast keeps original equal-name class backing",
      {|U8 class Word{U8 value;};I64 F(){I64 n=0x1002a;return n(Word);}U32 class Word{U32 value;};F();|},
      42L,
      "" );
    ( "retained class cast keeps original member layout",
      {|class Root{U64 word;};class View{U16 value;};I64 F(){Root o;o(I64)=42;return o(View).value;}class View{U8 pad;U16 value;};F();|},
      42L,
      "" );
  ]

let faults =
  [
    ( "cast cannot enlarge a one-byte root",
      {|U8 class Box{U8 byte;};I64 F(){Box o;o.byte=42;return o(U64);}F();|},
      "HCIRVM0019",
      "" );
    ( "grouped cast first reads original default bytes",
      {|class Box{U8 bytes[8];};I64 F(){Box o;o.bytes[0]=42;return (o)(U8);}F();|},
      "HCIRVM0012",
      "" );
    ( "cast requires all bytes of the selected width",
      {|extern U0 Print(U8 *fmt,...);class Box{U64 word;};I64 F(){Box o;o(U8)=42;Print("A");return o(U16);}F();|},
      "HCIRVM0012",
      "A" );
    ( "cast preserves the one-past root boundary",
      {|class Box{U64 word;};I64 F(){Box a[2];Box *p=a+2;return p[0](U8);}F();|},
      "HCIRVM0019",
      "" );
    ( "cast keeps an unknown data reference unknown",
      {|class Box{U64 word;};I64 F(){Box *p;return p[0](U8);}F();|},
      "HCIRVM0012",
      "" );
  ]

let unsupported =
  [
    ( "ungrouped update does not consume another cast",
      {|class Box{U64 word;};I64 F(){Box o;o.word=42;return o++(U8);}F();|} );
    ( "narrow view cannot retype owned callback code",
      {|I64 Give(){return 42;}I64 F(){I64 (*call)()=&Give;return (call)(U8);}F();|}
    );
    ( "nested narrow view cannot retype owned callback code",
      {|I64 Give(){return 42;}I64 F(){I64 (*call)()=&Give;return ((call)(I64))(U8);}F();|}
    );
    ( "class register bits do not supply a member root",
      {|class Box{U64 word;};I64 F(){return 42(Box).word;}F();|} );
    ( "numeric class pointer cast has no owned data reference",
      {|class Box{U64 word;};I64 F(){Box *p=42(Box*);return p->word;}F();|} );
    ( "class cast cannot grant a callback field invocation",
      {|class Box{I64 (*call)();};I64 Give(){return 42;}I64 F(){Box o;return o(Box).call();}F();|}
    );
    ( "floating class cast has no integer backing",
      {|F64 class Box{F64 word;};I64 F(){return 42(Box);}F();|} );
    ( "pointer-backed class cast has no integer backing",
      {|U8 *class Box{U8 *word;};I64 F(){return 42(Box);}F();|} );
  ]
