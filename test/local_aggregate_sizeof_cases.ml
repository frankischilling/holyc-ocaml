(* Each expected word comes from the selected TempleOS CMemberLst.member_class
   size and its CArrayDim.total_cnt at the original PrsSizeOf query. In
   particular, function argument slot width does not determine sizeof. *)

type case = {
  label : string;
  source : string;
  expected : int64;
  output : string;
}

let make ?(output = "") label expected source =
  { label; source; expected; output }

let headers = "extern U0 Print(U8 *fmt,...);"

let closed =
  [
    make "packed automatic class" 3L
      {|class C{U8 tag;U16 value;};I64 F(){C object;return sizeof(object);}F();|};
    make "default class uses full layout" 9L
      {|class C{I64 word;U8 guard;};I64 F(){C item;return sizeof(item);}F();|};
    make "overlapping union uses maximum member extent" 8L
      {|union U{U8 bytes[7];I64 word;};I64 F(){U item;return sizeof(item);}F();|};
    make "completed inherited layout" 4L
      {|class Base{U8 first;};class Child:Base{U16 second;U8 last;};I64 F(){Child item;return sizeof(item);}F();|};
    make "multidimensional automatic class extent" 18L
      {|class C{U8 tag;U16 value;};I64 F(){C items[2][3];return sizeof(items);}F();|};
    make "comma declarators retain independent dimensions" 18L
      {|class C{U8 bytes[3];};I64 F(){C a[2],b[4];return sizeof(a)+sizeof(b);}F();|};
    make "class value parameter uses declared class size" 7L
      {|class C{U8 bytes[7];};I64 F(C item){return sizeof(item);}F(0);|};
    make "class parameter size differs from pointer local" 15L
      {|class C{U8 bytes[7];};I64 F(C item){C *ptr=&item;return sizeof(item)+sizeof(ptr);}F(0);|};
    make "named class pointer local remains pointer-sized" 8L
      {|class C{U8 bytes[3];};I64 F(){C *ptr;return sizeof(ptr);}F();|};
    make "callback local retains pointer-sized type" 13L
      {|class C{U8 bytes[5];};I64 F(){C item;I64 (*fn)();return sizeof(item)+sizeof(fn);}F();|};
    make "backed class size is independent of scalar width" 3L
      {|U16 class C{U16 low;U8 guard;};I64 F(){C item;return sizeof(item);}F();|};
    make "class defined inside function" 7L
      {|I64 F(){class C{U8 bytes[7];};C item;return sizeof(item);}F();|};
    make "same spelling objects keep exact selected classes" 59L
      {|I64 F(){class C{U8 five[5];};C old;class C{U8 nine[9];};C newer;return sizeof(old)*10+sizeof(newer);}F();|};
    make "outer class replacement leaves prior object identity" 5L
      {|class C{U8 old[5];};I64 F(){C item;return sizeof(item);}class C{U8 other[9];};F();|};
    make "retained function keeps earlier local class" 6L
      {|I64 F(){class C{U8 bytes[6];};C item;return sizeof(item);}I64 G(){return F();}class C{U8 other[9];};G();|};
    make "star suffix overrides automatic aggregate extent" 8L
      {|class C{U8 bytes[3];};I64 F(){C items[3];return sizeof(items *);}F();|};
  ]

let jit_only =
  [
    make "forward class completes during local allocation lookahead" 2L
      {|I64 F(){extern class C;C object #exe{class C{U16 value;};};return sizeof(object);}F();|};
    make "dimension lookahead replacement does not change selected class" 6L
      {|I64 F(){class C{U8 tag;U16 value;};C objects[2 #exe{class C{U8 other[9];};}];return sizeof(objects);}F();|};
  ]

let runtime_layout =
  make ~output:"offdim" "JIT runtime offset and bound run once" 42L
    {|I64 Calls=0;
I64 Offset(){Print("off");Calls++;return 4;}
I64 Count(){Print("dim");Calls++;return 2;}
I64 F(){class C{U8 tag;$$=Offset();U16 words[Count()];};C items[2];
  return sizeof(items)+Calls+24;}
F();|}
