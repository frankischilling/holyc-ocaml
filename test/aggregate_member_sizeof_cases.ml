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
    make "static aggregate object reads original member metadata" 4L
      {|class Packet{U16 words[2];};I64 F(){static Packet stored;return sizeof(stored.words);}F();|};
    make "module aggregate object reads original member metadata" 12L
      {|class Packet{U16 words[2][3];};Packet stored;I64 F(){return sizeof(stored.words);}F();|};
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
    make "all six pinned public unions keep their own member extents" 884422L
      {|I64 F(){return sizeof(U16.u8)+sizeof(I16.i8)*10+
sizeof(U32.u16)*100+sizeof(I32.i16)*1000+
sizeof(U64.u32)*10000+sizeof(I64.i32)*100000;}F();|};
    make "nested public union arrays replace every earlier extent" 2222L
      {|I64 F(){return sizeof(U64.u32.u16.u8)*1000+
sizeof(I64.i32.i16.i8)*100+sizeof(U32.u16.u8)*10+
sizeof(I32.i16.i8);}F();|};
    make "pinned union terminal array supports trailing pointer override" 86L
      {|I64 F(){return sizeof(I64.i32.i16.u8 *****)*10+
sizeof(U16.u8)+sizeof(U32.i16);}F();|};
    make "primitive parameter and locals retain selected member classes" 222L
      {|I64 F(U64 argument){U32 local;I16 signed_word;
return sizeof(argument.u16.u8)*100+sizeof(local.u16.u8)*10+
sizeof(signed_word.i8);}F(0);|};
    make "aggregate primitive arrays do not multiply later union fields" 24364L
      {|class Box{U64 rows[3];I32 cols[4];};
I64 F(){Box objects[2];return sizeof(Box.rows)*1000+
sizeof(objects.rows.u32.u16.u8)*100+sizeof(Box.cols)*10+
sizeof(Box.cols.i16);}F();|};
    make "scalar global and global array use original primitive class" 8328L
      {|U64 selected;I32 words[3];
I64 F(){return sizeof(selected.u8)*1000+sizeof(words.i16.u8)*100+
sizeof(words)*10+sizeof(selected.u32);}F();|};
    make "primitive scalar globals keep their selected class after shadowing"
      82L
      {|U64 kept;U64 parts[2];
class U64{U8 changed[9];};
I64 F(){return sizeof(kept.u32)*10+sizeof(parts.u16.u8);}F();|};
    make "static primitive objects and arrays retain public union fields" 12L
      {|I64 F(){static U64 stored;static U32 units[2];
return sizeof(stored.u16)+sizeof(units.u8);}F();|};
    make "primitive local remains canonical after same-name class" 89L
      {|I64 F(){U64 earlier;class U64{U8 changed[9];};U64 later;
return sizeof(earlier.u16)*10+sizeof(later.changed);}F();|};
    make "function keeps original primitive query after source replacement" 8L
      {|I64 F(){return sizeof(U64.u32);}
class U64{U8 changed[9];};F();|};
  ]

let unsupported_storage : unsupported_storage_case list = []

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
    make "pinned primitive root stays selected across member-name directive" 8L
      {|I64 F(){return sizeof(U64. #exe{class U64{U8 changed[9];};} u32);}F();|};
    make "primitive object stays selected across member-name directive" 8L
      {|I64 F(){U64 original;
return sizeof(original. #exe{class U64{U8 changed[9];};} u16);}F();|};
    make "nested primitive member preserves original class before directive" 2L
      {|I64 F(){return sizeof(U64.u32. #exe{class U32{U8 changed[9];};} u16.u8);}F();|};
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
    reject "public union intermediate lacks the requested member"
      {|class C{U16 value;};
I64 F(){return sizeof(C.value.second);}F();|};
    reject ~message_fragment:"pointer class has no members"
      "local U64 pointer has no builtin union fields"
      {|I64 F(){U64 *ptr;return sizeof(ptr.u8);}F();|};
    reject ~message_fragment:"member" "stars cannot rescue missing field"
      {|class C{U16 value;};
I64 F(){return sizeof(C.missing *****);}F();|};
    reject ~code:"HCSEMA0012" "inherited ordinary duplicate member is invalid"
      {|class Base{U8 value[7];};class Child:Base{U16 value[2];};
I64 F(){return sizeof(Child.value);}F();|};
    reject ~message_fragment:"member" "pinned public union lacks named member"
      {|I64 F(){return sizeof(U64.nonexistent);}F();|};
    reject ~message_fragment:"member" "pinned nested public union lacks member"
      {|I64 F(){return sizeof(U64.u16.nonexistent);}F();|};
    reject ~message_fragment:"internal" "pinned internal U8i cannot continue"
      {|I64 F(){return sizeof(U64.u8.other);}F();|};
    reject ~message_fragment:"internal" "nested internal I8i cannot continue"
      {|I64 F(){return sizeof(I64.i32.i16.i8.other);}F();|};
    reject ~message_fragment:"member" "pointer stars cannot rescue union typo"
      {|I64 F(){return sizeof(U32.nonexistent *****);}F();|};
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
    ( reject ~code:"HCRUN0004"
        "pinned internal union field rejects before member-token directive"
        {|I64 F(){return sizeof(U64.u8. #exe{Print("late");} wrong);}F();|},
      "" );
    ( reject ~code:"HCRUN0004"
        "pinned union missing field reads directive before rejection"
        {|I64 F(){return sizeof(U64. #exe{Print("late");} wrong);}F();|},
      "late" );
    ( reject ~code:"HCRUN0004"
        "internal scalar global reads member directive before rejection"
        {|U8 scalar;
I64 F(){return sizeof(scalar. #exe{Print("late");} wrong);}F();|},
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
return sizeof(object.words)+sizeof(object.words.u8)+Calls+34;}
F();|}
