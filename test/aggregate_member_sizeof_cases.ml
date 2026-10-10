(* Compiler/PrsExp.HC:303-350: PrsSizeOf reads the selected member_class->size
   times that member's dim.total_cnt. Each successful dot replaces the earlier
   extent, even when the original object or an intermediate member is an array.
   Compiler/LexLib.HC:67-85 searches the selected class before its bases.
   Expected words are derived from those rules, never from object contents. *)

type case = {
  label : string;
  source : string;
  expected : int64;
  output : string;
}

let make ?(output = "") label expected source : case =
  { label; source; expected; output }

type rejection = {
  label : string;
  source : string;
  code : string option;
  message_fragment : string option;
}

let reject ?code ?message_fragment label source : rejection =
  { label; source; code; message_fragment }

type unsupported_storage_case = {
  label : string;
  source : string;
  ir_code : string;
  native_code : string;
}

let headers = "extern U0 Print(U8 *fmt,...);\n"

let closed =
  [
    make "named packed class selects exact field extents" 16L
      {|class Packet{U8 tag;U16 words[3];};
I64 F(){return sizeof(Packet.tag)*10+sizeof(Packet.words);}F();|};
    make "named union selects each overlapping field" 15L
      {|union Choice{U8 bytes[7];U64 word;};
I64 F(){return sizeof(Choice.bytes)+sizeof(Choice.word);}F();|};
    make "uninitialized automatic object performs metadata reads" 9L
      {|class Packet{U8 tag;U32 words[2];};
I64 F(){Packet object;return sizeof(object.words)+sizeof(object.tag);}F();|};
    make "value parameter member uses declaration rather than slot" 61L
      {|class Packet{U8 tag;U16 words[3];};
I64 F(Packet value){return sizeof(value.words)*10+sizeof(value.tag);}F(0);|};
    make "function-local class member" 4L
      {|I64 F(){class Packet{U16 words[2];};Packet object;
return sizeof(object.words);}F();|};
    make "nested member arrays discard intermediate counts" 12L
      {|class Leaf{U16 value[2][3];};
class Middle{Leaf leaves[4];};class Outer{Middle nodes[5];};
I64 F(){Outer object;return sizeof(object.nodes.leaves.value);}F();|};
    make "root and middle arrays cannot inflate last field" 12L
      {|class Leaf{U16 value[2][3];};
class Middle{Leaf leaves[2];};class Outer{Middle nodes[2];};
I64 F(){Outer objects[3];return sizeof(objects.nodes.leaves.value);}F();|};
    make "array terminal keeps its own extent before next dot" 204L
      {|class Leaf{U16 value[2];};class Box{Leaf records[5];};
I64 F(){return sizeof(Box.records)*10+sizeof(Box.records.value);}F();|};
    make "outer root array does not multiply selected member" 6L
      {|class Box{U8 tag;U16 words[3];};
I64 F(){Box objects[4];return sizeof(objects.words);}F();|};
    make "inherited multidimensional field retains own extent" 121L
      {|class Base{U16 words[2][3];};class Child:Base{U8 tag;};
I64 F(){return sizeof(Child.words)*10+sizeof(Child.tag);}F();|};
    make "three inherited levels keep declaring member sizes" 524L
      {|class Base{U8 text[5];};
class Middle:Base{U16 marker;};class Child:Middle{U32 flags;};
I64 F(){return sizeof(Child.text)*100+sizeof(Child.marker)*10+
sizeof(Child.flags);}F();|};
    make "child pad member wins over allowed inherited pad" 4L
      {|class Base{U8 pad[7];};class Child:Base{U16 pad[2];};
I64 F(){return sizeof(Child.pad);}F();|};
    make "first same-class duplicate pad keeps lookup authority" 3L
      {|class C{U8 pad[3];U8 pad[8];};
I64 F(){return sizeof(C.pad);}F();|};
    make "nested union class uses final member dimension" 7L
      {|union Choice{U32 number;U8 bytes[7];};
class Box{Choice options[3];};
I64 F(){return sizeof(Box.options.bytes);}F();|};
    make "pointer and callback fields require no value load" 88L
      {|class Box{U8 tag;I64 *pointer;I64 (*callback)(I64);};
I64 F(){Box object;return sizeof(object.pointer)*10+
sizeof(object.callback);}F();|};
    make "nested pointer field ends the dot path" 8L
      {|class Leaf{U64 *pointer;};class Box{Leaf items[5];};
I64 F(){return sizeof(Box.items.pointer);}F();|};
    make "same spelling retains both original member types" 58L
      {|I64 F(){class C{U8 value[5];};C earlier;
class C{U16 value[4];};C later;
return sizeof(earlier.value)*10+sizeof(later.value);}F();|};
    make "function keeps class member after later replacement" 3L
      {|class C{U8 words[3];};
I64 F(){C object;return sizeof(object.words);}
class C{U16 words[5];};F();|};
    make "inherited original class survives same-name replacement" 4L
      {|class Base{U16 words[2];};class Child:Base{U8 tag;};
class Base{U64 words;};
I64 F(){return sizeof(Child.words);}F();|};
    make "five trailing stars override valid member array extent" 14L
      {|class C{U16 words[3];};
I64 F(){return sizeof(((C.words *****)))+sizeof(C.words);}F();|};
    make "expanded root dot and member names retain their query" 14L
      {|#define SZ sizeof
#define ROOT Packet
#define DOT .
#define FIELD words
class Packet{U16 words[3];};
I64 F(){return SZ(((ROOT DOT FIELD *****)))+SZ(ROOT DOT FIELD);}F();|};
    make "partially declared class self-member drives next field count" 2L
      {|class C{U16 first;U8 second[sizeof(C.first)];};
I64 F(){return sizeof(C.second);}F();|};
    make "forward member size reads before and after class completion" 2L
      {|I64 F(){extern class Leaf;
class Box{Leaf field;U8 tag;};
I64 before=sizeof(Box.field);
class Leaf{U16 value;};
return before*10+sizeof(Box.field);}F();|};
    make "self field keeps partial placement and reads completed class size" 55L
      {|class Partial{U16 prefix;Partial earlier;U8 tail;};
I64 F(){Partial object;return sizeof(Partial)*10+sizeof(object.earlier);}F();|};
  ]

(* These are valid sizeof member expressions, but persistent aggregate storage
   is outside the current executable source scope. Keep their refusals visible
   so a later storage implementation cannot silently change their status. *)
let unsupported_storage : unsupported_storage_case list =
  [
    {
      label = "static aggregate object member awaits persistent storage";
      source =
        {|class Packet{U16 words[2];};
I64 F(){static Packet stored;return sizeof(stored.words);}F();|};
      ir_code = "HCEVAL0003";
      native_code = "HCRUN0004";
    };
    {
      label = "module aggregate object member awaits persistent storage";
      source =
        {|class Packet{U16 words[2][3];};Packet stored;
I64 F(){return sizeof(stored.words);}F();|};
      ir_code = "HCRUN0004";
      native_code = "HCRUN0004";
    };
  ]

let jit_only =
  [
    make "root lookup precedes class replacement in lookahead" 4L
      {|I64 F(){class C{U16 value[2];};C object;
return sizeof(object #exe{class C{U8 value[9];};}.value);}F();|};
    make "dot lookup precedes replacement before member name" 4L
      {|I64 F(){class C{U16 value[2];};C object;
return sizeof(object. #exe{class C{U8 value[9];};} value);}F();|};
    make "member query precedes following JIT class completion" 2L
      {|I64 F(){extern class Leaf;
class Box{Leaf field;U8 tag;};
I64 before=sizeof(Box.field #exe{class Leaf{U16 value;};});
return before*10+sizeof(Box.field);}F();|};
  ]

let rejected =
  [
    reject ~code:"HCPARSE0033" "arrow inside raw sizeof path"
      {|class C{U16 value;};I64 F(){C object;return sizeof(object->value);}F();|};
    reject ~code:"HCPARSE0033" "index inside raw sizeof path"
      {|class C{U16 value[2];};I64 F(){C object;return sizeof(object.value[0]);}F();|};
    reject ~code:"HCPARSE0033" "arbitrary addition inside raw sizeof"
      {|class C{U16 value;};I64 F(){C object;return sizeof(object.value+1);}F();|};
    reject ~code:"HCPARSE0032" "dot must have a member name"
      {|class C{U16 value;};I64 F(){C object;return sizeof(object.);}F();|};
    reject ~code:"HCPARSE0034" "arrow after completed sizeof term"
      {|class C{U16 value;};I64 F(){C object;return sizeof(object.value)->other;}F();|};
    reject ~message_fragment:"member" "missing object field"
      {|class C{U16 value;};I64 F(){C object;return sizeof(object.missing);}F();|};
    reject ~message_fragment:"member" "missing nested field"
      {|class Leaf{U16 value;};class C{Leaf leaf;};
I64 F(){return sizeof(C.leaf.missing);}F();|};
    reject "scalar intermediate is not a class"
      {|class C{U16 value;};
I64 F(){return sizeof(C.value.second);}F();|};
    reject ~message_fragment:"member" "stars cannot rescue missing field"
      {|class C{U16 value;};
I64 F(){return sizeof(C.missing *****);}F();|};
    reject ~code:"HCSEMA0012" "inherited ordinary duplicate member is invalid"
      {|class Base{U8 value[7];};class Child:Base{U16 value[2];};
I64 F(){return sizeof(Child.value);}F();|};
  ]

(* PrsSizeOf rejects HTT_INTERNAL_TYPE at dot because only HTT_CLASS and
   HTT_GLBL_VAR may continue. A pointer companion is HTT_CLASS in
   PrsLib.HC:40-59, so its dot passes and the next Lex may execute #exe
   before MemberFind discovers that the class has no member. *)
let jit_rejected_effects : (rejection * string) list =
  [
    ( reject ~code:"HCRUN0004"
        "internal U8 rejects at dot before member-token directive"
        {|I64 F(){return sizeof(U8. #exe{Print("late");} missing);}F();|},
      "" );
    ( reject ~code:"HCRUN0004"
        "class pointer rejects after member-token directive"
        {|class C{U8 value;};
I64 F(){C *ptr;return sizeof(ptr. #exe{Print("late");} value);}F();|},
      "late" );
  ]

let provenance_source =
  {|#define SZ sizeof
#define ROOT Packet
#define DOT .
#define MIDDLE inner
#define FIELD bytes
_intern SZ(((ROOT DOT MIDDLE DOT FIELD *****))) I64 Check();|}

let runtime_layout =
  make ~output:"offdim" "JIT member layout offset and dimension execute once"
    42L
    {|I64 Calls=0;
I64 Offset(){Print("off");Calls++;return 4;}
I64 Count(){Print("dim");Calls++;return 2;}
I64 F(){class C{U8 tag;$$=Offset();U16 words[Count()];};C object;
return sizeof(object.words)+Calls+36;}
F();|}
