(* Expectations follow TempleOS c26482bb: BackFA/BackFB floating templates,
   BackB word equality and truth, and OptLib's exact PUBLIC F64 zero rule.
   Values below are exact binary fractions or source-derived binary64 words;
   these fixtures do not claim new TempleOS runtime captures. *)

type kind = I64 | F64

type value_case = {
  name : string;
  source : string;
  kind : kind;
  bits : int64;
  output : string;
  fisttp : bool;
}

type fault_case = {
  name : string;
  source : string;
  code : string;
  output : string;
}

let integer ?(output = "") ?(fisttp = false) name source bits =
  { name; source; kind = I64; bits; output; fisttp }

let floating ?(output = "") name source bits =
  { name; source; kind = F64; bits; output; fisttp = false }

let fault ?(output = "") name source code = { name; source; code; output }
let slot place body = String.concat place (String.split_on_char '@' body)

let shapes =
  [
    ("global", "F64 value=1.5;", "", "value", "");
    ("static", "", "static F64 value=1.5;", "value", "");
    ( "global array",
      "F64 values[2]={9.0,1.5};",
      "",
      "values[1]",
      "if(values[0]!=9.0)return -30;" );
    ( "static array",
      "",
      "static F64 values[2]={9.0,1.5};",
      "values[1]",
      "if(values[0]!=9.0)return -30;" );
    ( "packed global member",
      "class Box{U8 tag;F64 values[2];U8 tail;};Box box={65,{9.0,1.5},66};",
      "",
      "box.values[1]",
      "if(box.tag!=65 || box.tail!=66 || sizeof(Box)!=18)return \
       -31;if(box.values[0]!=9.0)return -32;" );
    ( "packed static member",
      "class Box{U8 tag;F64 values[2];U8 tail;};",
      "static Box box={65,{9.0,1.5},66};",
      "box.values[1]",
      "if(box.tag!=65 || box.tail!=66 || sizeof(Box)!=18)return \
       -31;if(box.values[0]!=9.0)return -32;" );
    ( "automatic array",
      "",
      "F64 values[2];values[0]=9.0;values[1]=1.5;",
      "values[1]",
      "if(values[0]!=9.0)return -30;" );
    ( "packed automatic member",
      "class Box{U8 tag;F64 values[2];U8 tail;};",
      "Box box;box.tag=65;box.values[0]=9.0;box.values[1]=1.5;box.tail=66;",
      "box.values[1]",
      "if(box.tag!=65 || box.tail!=66 || sizeof(Box)!=18)return \
       -31;if(box.values[0]!=9.0)return -32;" );
  ]

let shape_source prefix setup place neighbors body =
  prefix ^ "I64 Check(){" ^ setup ^ slot place body ^ neighbors
  ^ "return 42;}Check();"

let storage_values =
  List.map
    (fun (name, prefix, setup, place, neighbors) ->
      integer ~fisttp:true
        (name ^ " updates, arithmetic, conversions and loops")
        (shape_source prefix setup place neighbors
           {|F64 old=@++;if(old!=1.5)return -1;
             old=@--;if(old!=2.5)return -2;
             F64 changed=++@;if(changed!=2.5)return -3;
             changed=--@;if(changed!=1.5)return -4;
             @+=2;@-=0.5;@*=2.0;@/=4.0;
             if(@!=1.5)return -5;
             I64 truncated=@;if(truncated!=1)return -6;
             @=truncated;if(@!=1.0)return -7;
             I64 rounds=0;
             while(@<3.0){@+=0.5;rounds++;}
             if(rounds!=4)return -8;
             I64 i;for(i=0;i<2;i++)@-=0.5;
             do{@+=0.5;}while(@<3.0);
             if(!(@>=3.0))return -9;
             if(@<=2.5)return -10;
             F64 linked=(@+1.0)*2.0-@;
             if(linked!=5.0)return -11;
             @=-@;if(@>-2.0)return -12;|})
        42L)
    shapes

let conversion_values =
  List.map
    (fun (name, prefix, setup, place, neighbors) ->
      integer ~fisttp:true
        (name ^ " narrow conversion and integer return")
        (prefix ^ "I64 Check(){" ^ setup
        ^ slot place
            {|@=-3.75;I8 noreg small=@;U8 noreg wrapped=@;
              if(small!=-3 || wrapped!=253)return -1;
              F64 widened=wrapped;if(widened!=253.0)return -2;
              @=wrapped;|}
        ^ neighbors ^ "return " ^ place ^ ";}Check();")
        253L)
    shapes
  @ [
      integer ~fisttp:true "mixed persistent destination widths"
        {|I64 whole=0;I16 words[2]={7,0};class Box{U8 tag;I8 small;F64 value;U8 tail;};Box box={65,0,0.0,66};
          I64 Check(){static I32 saved=0;box.value=-3.75;whole=box.value;words[1]=box.value;saved=box.value;box.small=255.75;
            if(whole!=-3 || words[0]!=7 || words[1]!=-3 || saved!=-3 || box.small!=-1)return -1;
            box.value=saved;if(box.value!=-3.0)return -2;
            if(box.tag!=65 || box.tail!=66)return -3;return 42;}Check();|}
        42L;
      floating "persistent integer return converts signed full word"
        {|U64 word=0xffffffffffffffff;F64 Read(){return word;}Read();|}
        0xbff0000000000000L;
      floating "packed narrow integer return widens after the load"
        {|class Box{U8 tag;I8 small;U8 tail;};Box box={65,255,66};F64 Read(){return box.small;}Read();|}
        0xbff0000000000000L;
      integer ~fisttp:true
        "constant automatic index returns stored narrow compound"
        {|I64 Read(){I8 noreg values[2];values[0]=65;values[1]=127;I64 result=(values[1]+=1.0);if(values[0]!=65)return 0;return result;}Read();|}
        (-128L);
      integer ~fisttp:true
        "constant static index returns stored narrow compound"
        {|I64 Read(){static I8 values[2]={65,127};return values[1]+=1.0;}Read();|}
        (-128L);
      integer ~fisttp:true "packed member returns stored narrow compound"
        {|class Box{U8 tag;I8 small;U8 tail;};Box box={65,127,66};I64 Read(){I64 result=(box.small+=1.0);if(box.tag!=65 || box.tail!=66)return 0;return result;}Read();|}
        (-128L);
      integer ~fisttp:true
        "packed member array keeps constant address authority"
        {|class Box{U8 tag;I8 values[2];U8 tail;};Box box={65,{65,127},66};I64 Read(){return box.values[1]+=1.0;}Read();|}
        (-128L);
      floating "constant global index returns stored floating compound"
        {|F64 values[2]={9.0,1.5};F64 Read(){return values[1]+=0.5;}Read();|}
        0x4000000000000000L;
      floating "packed member returns stored floating compound"
        {|class Box{U8 tag;F64 value;U8 tail;};Box box={65,1.5,66};F64 Read(){return box.value+=0.5;}Read();|}
        0x4000000000000000L;
    ]

(* BackFB.HC:573-614 and Templates.HC:178-190 preserve the old postfix
   materialized word, but a linked floating consumer receives the new extended
   ST0 value. A separate store remains a binary64 boundary. *)
let precision_values =
  [
    floating "prefix update donates its extended new value"
      {|F64 value=9007199254740992.0;F64 Read(){return (++value)-9007199254740992.0;}Read();|}
      0x3ff0000000000000L;
    floating "postfix link also donates the extended new value"
      {|F64 value=9007199254740992.0;F64 Read(){return (value++)-9007199254740992.0;}Read();|}
      0x3ff0000000000000L;
    floating "separate postfix word store preserves the old materialized value"
      {|F64 value=9007199254740992.0;F64 Read(){F64 old=value++;return old-value;}Read();|}
      0L;
    floating "postfix old word preserves negative zero"
      {|F64 value=-0.0;F64 Read(){F64 old=value++;return old;}Read();|}
      Int64.min_int;
    floating "global left arithmetic link"
      {|F64 a=9007199254740992.0,b=1.0;F64 Read(){return (a+b)-a;}Read();|}
      0x3ff0000000000000L;
    floating "global asymmetric arithmetic children"
      {|F64 a=9007199254740992.0,b=1.0;F64 Read(){return (a+b)-(a+b);}Read();|}
      0xbff0000000000000L;
    floating "static right arithmetic link"
      {|F64 Read(){static F64 a=9007199254740992.0,b=1.0;return a-(a+b);}Read();|}
      0xbff0000000000000L;
    floating "unaligned packed arithmetic link"
      {|class Box{U8 tag;F64 a;F64 b;U8 tail;};Box box={65,9007199254740992.0,1.0,66};F64 Read(){return (box.a+box.b)-box.a;}Read();|}
      0x3ff0000000000000L;
    floating "persistent array store materializes the sum"
      {|F64 values[3]={9007199254740992.0,1.0,0.0};F64 Read(){values[2]=values[0]+values[1];return values[2]-values[0];}Read();|}
      0L;
    integer ~fisttp:true "global integer conversion feeds extended arithmetic"
      {|I64 word=9007199254740993;F64 opposite=-9007199254740992.0;I64 Read(){return word+opposite;}Read();|}
      1L;
    integer ~fisttp:true "stored operands retain implicit return conversion"
      {|F64 values[2]={9007199254740992.0,1.0};I64 Read(){return values[0]+values[1];}Read();|}
      9007199254740993L;
    floating "persistent assignment materializes integer conversion"
      {|I64 word=9007199254740993;F64 value;F64 Read(){value=word;return value-9007199254740992.0;}Read();|}
      0L;
    integer "stored extended comparison"
      {|F64 values[2]={9007199254740992.0,1.0};I64 Read(){return (values[0]+values[1])>values[0];}Read();|}
      1L;
    integer "stored equality materializes arithmetic"
      {|F64 values[2]={9007199254740992.0,1.0};I64 Read(){return (values[0]+values[1])==values[0];}Read();|}
      1L;
  ]

let signed_zero_values =
  List.map
    (fun (name, prefix, setup, place, neighbors) ->
      integer
        (name ^ " exact public F64 zero and truth rules")
        (shape_source prefix setup place neighbors
           {|@=-0.0;if(!@)return -1;
             if(@==0.0)return -2;if(@!=-0.0)return -3;
             I64 less=@<0.0;I64 nonnegative=@>=0.0;
             if(less!=1 || nonnegative!=0)return -4;
             I64 greater=@>0.0;I64 at_most=@<=0.0;
             if(greater!=0 || at_most!=1)return -5;
             if(@<0.0){}else return -6;
             if(@>=0.0)return -7;|})
        42L)
    shapes

let nan_prefix =
  {|F64 values[2];I64 Check(){(&values[0])(U64*)[0]=0x7ff8000000000042;values[1]=1.0;|}

let comparison_values =
  signed_zero_values
  @ [
      integer "F64i storage does not acquire the public zero shortcut"
        {|F64i value;I64 Check(){value=-0.0;return value<0.0;}Check();|} 0L;
      integer "stored NaN value comparison uses the unlinked value template"
        (nan_prefix ^ "return values[0]<values[1];}Check();")
        1L;
      integer "stored NaN direct condition retains the branch predicate"
        (nan_prefix ^ "if(values[0]<values[1])return 1;return 0;}Check();")
        0L;
      integer "stored NaN left arithmetic link"
        (nan_prefix ^ "return (values[0]+values[1])<values[1];}Check();")
        0L;
      integer "stored NaN right arithmetic link"
        (nan_prefix ^ "return values[1]<(values[0]+values[1]);}Check();")
        1L;
      floating "packed signaling NaN equality and byte transport"
        {|class Box{U8 tag;F64 values[2];U8 tail;};Box box;
          F64 Read(){box.tag=65;box.tail=66;(&box.values[0])(U64*)[0]=0x7ff0000000000042;box.values[1]=box.values[0];
            if(box.values[0]!=box.values[1])return 0.0;
            if(box.tag!=65 || box.tail!=66)return 0.0;return box.values[1];}Read();|}
        0x7ff0000000000042L;
    ]

let effect_values =
  [
    integer ~output:"AR"
      "update captures its address before RHS and reads after it"
      {|extern U0 PutChars(U64 ch);I64 selected=0,calls=0;F64 values[2]={1.5,9.0};
        I64 Address(){PutChars('A');calls++;return selected;}
        F64 Right(){PutChars('R');selected=1;values[0]=10.0;return 0.5;}
        I64 Check(){values[Address()]+=Right();if(calls!=1 || selected!=1)return -1;
          if(values[0]!=10.5)return -2;if(values[1]!=9.0)return -3;return 42;}Check();|}
      42L;
    integer "indexed postfix and prefix select each element once"
      {|I64 index=0;F64 values[2]={1.5,2.5};I64 Next(){return index++;}
        I64 Check(){F64 old=values[Next()]++;F64 current=++values[Next()];
          if(index!=2)return -1;if(old!=1.5)return -2;if(current!=3.5)return -3;
          if(values[0]!=2.5)return -4;if(values[1]!=3.5)return -5;return 42;}Check();|}
      42L;
    integer "borrowed packed pointer updates its original storage"
      {|class Box{U8 tag;F64 values[2];U8 tail;};Box box={65,{1.5,2.5},66};
        I64 Change(F64 *pointer){(*pointer)++;pointer[1]+=0.5;if(*pointer!=2.5)return -1;return 42;}
        I64 Check(){I64 answer=Change(&box.values[0]);if(box.values[1]!=3.0)return -2;
          if(box.tag!=65 || box.tail!=66)return -3;return answer;}Check();|}
      42L;
    integer ~output:"AB"
      "argument calls retain reverse effects and stored reads"
      {|extern U0 PutChars(U64 ch);F64 value=1.5;
        F64 Mark(U64 ch){PutChars(ch);value+=0.5;return value;}
        I64 Compare(F64 left,F64 right){return left>right;}
        Compare(Mark('B'),Mark('A'));|}
      1L;
    floating "repeated direct calls reuse one static allocation"
      {|F64 Next(){static F64 values[2]={0.5,1.0};values[0]+=0.25;return values[0]+values[1];}Next();Next();|}
      0x4000000000000000L;
    integer ~output:"A" "later calls see earlier persistent stores"
      {|extern U0 PutChars(U64 ch);F64 value=1.5;
        F64 Write(){PutChars('A');value*=2.0;return value;}
        I64 Read(){if(value>=3.0)return 42;return 0;}
        Write();Read();|}
      42L;
  ]

let jit_task_values =
  [
    floating "historical callees retain original globals and static storage"
      {|F64 value=1.5;F64 Step(){static F64 saved=0.5;value+=1.0;saved+=0.25;return value+saved;}
        F64 Old(){return Step();}Old();F64 value=100.0;F64 Step(){return -99.0;}Old();|}
      0x4012000000000000L;
    integer "saved comparison reads its original global after replacement"
      {|F64 value=1.5;I64 Old(){return value<2.0;}Old();F64 value=100.0;Old();|}
      1L;
    floating "new source commands preserve existing packed writes"
      {|class Box{U8 tag;F64 value;};Box box={65,1.5};F64 Step(){box.value+=0.5;return box.value;}
        Step();I64 unrelated[2]={20,22};Step();|}
      0x4004000000000000L;
  ]

let faults =
  [
    fault ~output:"A" "out-of-window update preserves earlier output"
      {|extern U0 PutChars(U64 ch);F64 values[1]={1.5};
        I64 Check(){PutChars('A');values[1]+=1.0;PutChars('B');return 42;}Check();|}
      "HCIRVM0019";
    fault "out-of-window comparison checks before x87 consumption"
      {|F64 values[1]={1.5};I64 Check(){return values[1]<2.0;}Check();|}
      "HCIRVM0019";
    fault "unread automatic bytes cannot feed a comparison"
      {|I64 Check(){F64 values[2];return values[1]<2.0;}Check();|} "HCIRVM0012";
    fault "retired borrowed pointer cannot feed a comparison"
      {|F64 *Bad(){F64 value=1.5;return &value;}I64 Check(){F64 *pointer=Bad();return *pointer<2.0;}Check();|}
      "HCIRVM0018";
    fault "floating update invalidates overlapped data-pointer ownership"
      {|class Box{I64 *pointer;};F64 value;
        I64 Check(){Box *box=(&value)(Box*);I64 target=42;box->pointer=&target;value+=1.0;return *box->pointer;}Check();|}
      "HCIRVM0018";
    fault "floating update invalidates overlapped callback ownership"
      {|class Box{F64 (*callback)(F64 value);};F64 Id(F64 value){return value;}
        F64 Check(){Box box;box.callback=&Id;F64 *alias=(&box)(F64*);*alias+=1.0;return box.callback(1.5);}Check();|}
      "HCIRVM0024";
  ]

let jit_faults =
  [
    fault "unread global bytes cannot feed a comparison"
      {|F64 value;I64 Check(){return value<2.0;}Check();|} "HCIRVM0012";
    fault "unread static bytes cannot feed an update"
      {|I64 Check(){static F64 value;value+=1.0;return 42;}Check();|}
      "HCIRVM0012";
    fault "partial byte write does not initialize a complete F64 read"
      {|F64 value;I64 Check(){(&value)(U8*)[0]=0;return value==0.0;}Check();|}
      "HCIRVM0012";
  ]

let unsupported =
  [
    ("deeper pointer", "F64 Read(F64 **pointer){return **pointer;}42;");
    ( "unproved suffix bit reinterpretation",
      "F64 Read(I64 word){return word(F64);}Read(3);" );
  ]

let quota_source =
  {|F64 value;I64 Check(){value=1.5;value+=0.5;if(value<3.0)return 42;return 0;}Check();|}

let maintained_source =
  {|class Sample {
  U8 tag;
  F64 values[2];
  I16 whole;
  U8 tail;
};

F64 Total=1.5;
F64 Grid[2][2]={{1.0,2.0},{3.0,4.0}};
Sample Record={65,{1.25,2.5},0,66};

F64 Read(Sample *sample) {
  return sample->values[0]+sample->values[1];
}

F64 Tick(F64 increment) {
  static F64 saved[2]={0.5,1.0};
  Total+=increment;
  Grid[1][0]-=0.5;
  Record.values[0]*=2.0;
  Record.values[1]/=2.0;
  saved[0]+=0.25;
  Record.whole=Read(&Record);
  return saved[0]+saved[1];
}

I64 Check() {
  F64 first=Tick(1.0);
  F64 second=Tick(1.0);
  if(first!=1.75)return -1;
  if(second!=2.0)return -2;
  if(Total!=3.5)return -3;
  if(Grid[1][0]!=2.0)return -4;
  if(Record.values[0]!=5.0)return -5;
  if(Record.values[1]!=0.625)return -6;
  if(Record.whole!=5)return -7;
  F64 widened=Record.whole;
  if(widened!=5.0)return -8;
  I64 rounds=0;
  while(Total<4.5) {
    Total+=0.5;
    rounds++;
  }
  I64 index;
  for(index=0;index<2;index++)
    Grid[0][index]+=1.0;
  do {
    Total-=0.5;
  } while(Total>3.5);
  if(rounds!=2)return -9;
  if(Grid[0][0]!=2.0)return -10;
  if(Grid[0][1]!=3.0)return -11;
  if(Record.tag!=65 || Record.tail!=66 || sizeof(Sample)!=20)return -12;
  return 42;
}

Check();
|}

let maintained =
  integer ~fisttp:true "maintained storage execution example" maintained_source
    42L

let values =
  storage_values @ conversion_values @ precision_values @ comparison_values
  @ effect_values @ [ maintained ]
