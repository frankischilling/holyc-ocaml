let values =
  [
    ( "ordinary class default",
      {|class Box{I64 word;};I64 Take(Box o=42){return o;}Take();|},
      42L,
      "" );
    ( "narrow default preserves the other bytes",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o=0x07002a){if(o.guard!=7)return -1;return o;}Take();|},
      42L,
      "" );
    ( "signed prefix does not clamp the saved default",
      {|I8 class Box{I8 low;U8 guard;};I64 Take(Box o=0x07ff){if(o.guard!=7)return 0;return o;}Take();|},
      -1L,
      "" );
    ( "provided class word replaces the saved word",
      {|U8 class Box{U8 low;U8 guard;};I64 Take(Box o=0x072a){return o.guard*256+o;}Take(0x09ab);|},
      0x09abL,
      "" );
    ( "class and scalar defaults retain their positions",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o=0x07002a,I64 tail=3){if(o.guard!=7)return -1;return o+tail;}Take();|},
      45L,
      "" );
    ( "two class defaults use separate full slots",
      {|U8 class Box{U8 low;U8 guard;};I64 Take(Box a=0x0714,Box b=0x0916){if(a.guard!=7||b.guard!=9)return -1;return a+b;}Take();|},
      42L,
      "" );
    ( "union default uses its own raw class",
      {|U16 union Box{U16 low;U8 byte;};I64 Take(Box o=0x1002a){if(o.byte!=42)return -1;return o;}Take();|},
      42L,
      "" );
    ( "named backing chain",
      {|U16 class Small{U16 low;};Small class Box{U16 low;U8 guard;};I64 Take(Box o=0x07002a){if(o.guard!=7)return -1;return o;}Take();|},
      42L,
      "" );
    ( "named chain ending in the default signed word",
      {|class Root{U8 byte;};Root class Box{I64 word;};I64 Take(Box o=42){return o;}Take();|},
      42L,
      "" );
    ( "inheritance keeps the default raw word",
      {|class Base{U8 byte;};class Box:Base{U8 next;};I64 Take(Box o=42){return o;}Take();|},
      42L,
      "" );
    ( "backed inheritance",
      {|class Base{U16 low;};U16 class Box:Base{U8 guard;};I64 Take(Box o=0x07002a){if(o.guard!=7)return -1;return o;}Take();|},
      42L,
      "" );
    ( "empty class still has a default argument word",
      {|class Box{};I64 Take(Box o=42){return o;}Take();|},
      42L,
      "" );
    ( "empty backed class",
      {|U16 class Box{};I64 Take(Box o=0x1002a){return o;}Take();|},
      42L,
      "" );
    ( "nominal size does not expand the default argument slot",
      {|class Box{I64 word;U8 ninth;};I64 Take(Box o=42){if(sizeof(Box)!=9)return -1;return o;}Take();|},
      42L,
      "" );
    ( "default remains attached to the original class",
      {|class Box{I64 word;};I64 Take(Box o=42){return o;}U8 class Box{U8 byte;};Take();|},
      42L,
      "" );
    ( "passing the prefix again loads only its raw width",
      {|U16 class Box{U16 low;U8 guard;};I64 Read(Box o){if(o.guard!=0)return -1;return o;}I64 Take(Box o=0x07002a){return Read(o);}Take();|},
      42L,
      "" );
    ( "recursive calls materialize the saved word",
      {|U8 class Box{U8 low;U8 guard;};I64 Take(I64 depth,Box o=0x072a){if(o.guard!=7)return -1;if(depth)return Take(depth-1);return o;}Take(2);|},
      42L,
      "" );
    ( "noreg class default",
      {|U16 class Box{U16 low;U8 guard;};I64 Take(noreg Box o=0x07002a){if(o.guard!=7)return -1;return o;}Take();|},
      42L,
      "" );
  ]

let view_matrix =
  [
    ("Bool", 1);
    ("I8", 1);
    ("U8", 1);
    ("I16", 2);
    ("U16", 2);
    ("I32", 4);
    ("U32", 4);
    ("I64", 8);
    ("U64", 8);
  ]
  |> List.map (fun (type_, width) ->
      let word =
        Int64.logor 42L
          (if width < 8 then Int64.shift_left 7L (8 * width) else 0L)
      in
      let members = type_ ^ " low;" ^ if width < 8 then "U8 guard;" else "" in
      let check = if width < 8 then "if(o.guard!=7)return -1;" else "" in
      ( type_ ^ " class default word",
        Printf.sprintf
          "%s class Box{%s};I64 Take(Box o=0x%Lx){%sreturn o;}Take();" type_
          members word check,
        42L,
        "" ))

let native_values = values @ view_matrix

let prototype_values =
  [
    ( "replacement header uses its own prepared default",
      {|class Box{I64 word;};extern I64 Take(Box o=41);I64 Take(Box o=42){return o;}Take();|},
      42L,
      "" );
    ( "an earlier emitted call keeps its saved default",
      {|class Box{I64 word;};extern I64 Take(Box o=41);I64 Earlier(){return Take();}I64 Take(Box o=42){return o;}Earlier();|},
      41L,
      "" );
  ]

let all_values = native_values @ prototype_values

let unsupported =
  [
    ( "floating class default",
      {|F64 class Box{F64 word;};I64 Take(Box o=42){return 42;}Take();|} );
    ( "pointer backing has no integer default word",
      {|I64* class Box{I64 word;};I64 Take(Box o=42){return 42;}Take();|} );
    ( "class-pointer default has no owned data address",
      {|class Box{I64 word;};I64 Take(Box *o=0){return 42;}Take();|} );
    ( "class callback signature needs separate admission",
      {|class Box{I64 word;};I64 Take(Box o){return o;}I64 (*p)(Box o=42);p=&Take;p();|}
    );
    ( "called class-returning default needs callable preparation",
      {|U16 class Box{U16 low;U8 guard;};Box Make(){return 0x07002a;}I64 Take(Box o=Make()){return o;}Take();|}
    );
    ( "a provided argument does not bypass an effectful class default",
      {|extern U0 Print(U8 *format,...);U16 class Box{U16 low;U8 guard;};Box Make(){Print("X");return 0x07002a;}I64 Take(Box o=Make()){return o;}Take(42);|}
    );
  ]

let jit_values =
  [
    ( "called class default preserves the returned register word",
      {|U16 class Box{U16 low;U8 guard;};Box Make(){return 0x07002a;}I64 Take(Box o=Make()){if(o.guard!=7)return -1;return o;}Take();|},
      42L,
      "" );
    ( "provided arguments still prepare an effectful class default",
      {|extern U0 Print(U8 *format,...);U16 class Box{U16 low;U8 guard;};Box Make(){Print("X");return 0x07002a;}I64 Take(Box o=Make()){return o;}Take(42);|},
      42L,
      "X" );
    ( "repeated calls do not prepare the class default again",
      {|I64 Count=0;U16 class Box{U16 low;U8 guard;};Box Make(){Count++;return 0x07002a;}I64 Take(Box o=Make()){if(o.guard!=7)return -1;return o;}Take();Take()+Count;|},
      43L,
      "" );
    ( "default preparation can call a class parameter and return",
      {|U16 class Box{U16 low;U8 guard;};Box Add(Box o){return o+1;}I64 Take(Box o=Add(41)){if(o.guard!=0)return -1;return o;}Take();|},
      42L,
      "" );
    ( "recursive class return in a default",
      {|U16 class Box{U16 low;U8 guard;};Box Make(I64 n){if(n)return Make(n-1);return 0x07002a;}I64 Take(Box o=Make(2)){if(o.guard!=7)return -1;return o;}Take();|},
      42L,
      "" );
    ( "a scalar default keeps the class result word",
      {|U16 class Box{U16 low;};Box Make(){return 0x07002a;}I64 Take(I64 n=Make()){return n;}Take();|},
      0x07002aL,
      "" );
    ( "class shadowing keeps the prepared call and parameter selections",
      {|U16 class Box{U16 low;U8 guard;};Box Make(){return 0x07002a;}I64 Take(Box o=Make()){if(o.guard!=7)return -1;return o;}U8 class Box{U8 byte;};Take();|},
      42L,
      "" );
    ( "ordinary class default keeps the full returned word",
      {|class Box{U16 low;U8 guard;};Box Make(){return 0x07002a;}I64 Take(Box o=Make()){if(o.guard!=7)return -1;return o;}Take();|},
      0x07002aL,
      "" );
    ( "a completed forward identity supplies the later parameter",
      {|extern class Box;class Box{I64 word;};I64 Take(Box o=42){return o;}Take();|},
      42L,
      "" );
    ( "imported runtime dimensions retain one original preparation",
      {|I64 Count=0;I64 Next(){Count++;return 2;}class B{U16 words[Next()];};class Box:B{U8 tag;};I64 Take(Box o=42){if(sizeof(Box)!=5)return -1;return o.words[0]+Count;}Take();|},
      43L,
      "" );
    ( "imported runtime offsets retain one original preparation",
      {|I64 Count=0;I64 Next(){Count++;return 8;}class Box{I64 word;$$=Next();U8 ninth;};I64 Take(Box o=42){if(sizeof(Box)!=9)return -1;return o.word+Count;}Take();|},
      43L,
      "" );
  ]

let jit_unsupported = List.filteri (fun index _ -> index < 4) unsupported

let native_unsupported =
  ( "explicit class register default",
    {|class Box{I64 word;};I64 Take(reg Box o=42){return o;}Take();|} )
  :: unsupported

let quota_source =
  {|U16 class Box{U16 low;U8 guard;};I64 Take(Box o=0x070000+42){if(o.guard!=7)return -1;return o;}Take();|}

let extent_source =
  {|class Box{I64 word;U8 ninth;};I64 Take(Box o=42){return o.ninth;}Take();|}
