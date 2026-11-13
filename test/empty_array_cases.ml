let values =
  [
    ("global-string", {|U8 text[]="AB";sizeof(text);|}, 0L, "");
    ("global-words", {|I64 words[]={};sizeof(words);|}, 0L, "");
    ( "global-class",
      {|class Item{U8 value;};Item items[]={};sizeof(items);|},
      0L,
      "" );
    ( "static-string",
      {|I64 F(){static U8 text[]="AB";return sizeof(text);}F();|},
      0L,
      "" );
    ( "static-words",
      {|I64 F(){static I64 words[]={};return sizeof(words);}F();|},
      0L,
      "" );
    ("automatic", {|I64 F(){U8 text[];return sizeof(text);}F();|}, 0L, "");
    ( "width-global-Bool",
      {|Bool values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-Bool",
      {|I64 F(){static Bool values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-Bool",
      {|I64 F(){Bool values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-I8",
      {|I8 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-I8",
      {|I64 F(){static I8 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-I8",
      {|I64 F(){I8 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-U8",
      {|U8 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-U8",
      {|I64 F(){static U8 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-U8",
      {|I64 F(){U8 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-I16",
      {|I16 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-I16",
      {|I64 F(){static I16 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-I16",
      {|I64 F(){I16 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-U16",
      {|U16 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-U16",
      {|I64 F(){static U16 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-U16",
      {|I64 F(){U16 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-I32",
      {|I32 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-I32",
      {|I64 F(){static I32 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-I32",
      {|I64 F(){I32 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-U32",
      {|U32 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-U32",
      {|I64 F(){static U32 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-U32",
      {|I64 F(){U32 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-I64",
      {|I64 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-I64",
      {|I64 F(){static I64 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-I64",
      {|I64 F(){I64 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-global-U64",
      {|U64 values[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-static-U64",
      {|I64 F(){static U64 values[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "width-local-U64",
      {|I64 F(){U64 values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "outer-zero",
      {|I64 values[0][2]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "inner-zero",
      {|I64 values[2][0]={{},{}};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "static-inner-zero",
      {|I64 F(){static I64 values[2][0]={{},{}};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "local-inner-zero",
      {|I64 F(){I64 values[2][0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "static-class",
      {|class Item{U8 value;};I64 F(){static Item values[]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "local-class",
      {|class Item{U8 value;};I64 F(){Item values[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "zero-field",
      {|class Box{U8 empty[0];U8 value;};Box object={{},42};I64 F(){return object.value;}F();|},
      42L,
      "" );
    ( "zero-field-string",
      {|class Box{U8 empty[];U8 value;};Box object={"AB",42};I64 F(){return object.value;}F();|},
      42L,
      "" );
    ( "static-zero-field-string",
      {|class Box{U8 empty[];U8 value;};I64 F(){static Box object={"AB",42};return object.value;}F();|},
      42L,
      "" );
    ( "forward-class",
      {|extern class Item;Item values[]={};class Item{U16 value;};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "global-neighbor",
      {|U8 empty[]="AB";I64 value=42;U8 tail[0]={};I64 F(){return value;}F();|},
      42L,
      "" );
    ( "local-neighbor",
      {|I64 F(){U8 empty[];I64 value=42;U8 tail[0];return value+sizeof(empty)+sizeof(tail);}F();|},
      42L,
      "" );
    ( "static-neighbor",
      {|I64 F(){static U8 empty[]="AB";static I64 value=42;static U8 tail[0]={};return value+sizeof(empty)+sizeof(tail);}F();|},
      42L,
      "" );
    ( "static-address-used",
      {|I64 F(){static U8 empty[0]={};U8 *p=empty;return (p==empty)*42;}F();|},
      42L,
      "" );
    ( "callback-global",
      {|I64 (*values)(I64 n)[0]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "callback-static",
      {|I64 F(){static I64 (*values)(I64 n)[0]={};return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "callback-local",
      {|I64 F(){I64 (*values)(I64 n)[0];return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "fractional",
      {|I64 values[0.5]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
    ( "negative-fractional",
      {|I64 values[-0.5]={};I64 F(){return sizeof(values)+42;}F();|},
      42L,
      "" );
  ]

let jit_values =
  [
    ( "runtime-zero",
      {|I64 count=0;I64 Size(){count++;return 0;}I64 values[Size()]={};I64 F(){return count+41+sizeof(values);}F();|},
      42L,
      "" );
    ( "runtime-static-zero",
      {|I64 count=0;I64 Size(){count++;return 0;}I64 F(){static I64 values[Size()]={};return count+41+sizeof(values);}F();|},
      42L,
      "" );
    ( "runtime-inner-zero",
      {|I64 count=0;I64 Size(){count++;return 0;}I64 values[0][Size()]={};I64 F(){return count+41+sizeof(values);}F();|},
      42L,
      "" );
  ]

let faults =
  [
    ( "fault-global-read",
      {|I64 values[0]={};I64 F(){return values[0];}F();|},
      "HCIRVM0019",
      "" );
    ( "fault-local-read",
      {|I64 F(){I64 values[0];return values[0];}F();|},
      "HCRUN0003",
      "" );
    ( "fault-static-read",
      {|I64 F(){static I64 values[0]={};return values[0];}F();|},
      "HCIRVM0019",
      "" );
    ( "fault-nonempty",
      {|extern U0 Print(U8 *fmt,...);I64 Effect(){Print("A");return 42;}I64 values[0]={Effect()};42;|},
      "HCRUN0006",
      "" );
    ( "fault-static-nonempty",
      {|extern U0 Print(U8 *fmt,...);I64 Effect(){Print("A");return 42;}I64 F(){static I64 values[0]={Effect()};return 42;}F();|},
      "HCRUN0006",
      "" );
    ("fault-negative", {|I64 values[-1]={};42;|}, "HCRUN0004", "");
  ]

let copy_unknown =
  {|class Box{U8 empty[0];U8 value;};Box object={"AB",object.value};I64 F(){return object.value;}F();|}

let quota_source =
  {|U8 empty[]="AB";I64 value=42;U8 tail[0]={};I64 F(){return value;}F();|}

let static_copy_unknown =
  {|class Box{U8 empty[0];U8 value;};I64 F(){static Box object={"AB",object.value};return object.value;}F();|}
