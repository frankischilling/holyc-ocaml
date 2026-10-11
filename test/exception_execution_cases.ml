(* Issue #692: source-level exception execution fixtures.
   The TempleOS oracle is pinned at c26482bb6ad3f80106d28504ec5db3c6a360732c.

   Kernel/KernelA.HH:41,3255-3256,3271-3325 fixes STR_LEN=144,
   TASK_NAME_LEN=32, TASK_EXCEPT_CALLERS=8, and the CTask prefix.
   Counting that exact prefix gives:
     CTask.except_rbp     808
     CTask.except_ch      816
     CTask.except_callers 824 (eight pointers, 64 bytes)
     CTask.catch_except   888
   The source below describes the same field layout through catch_except.
   The 816-byte padding replaces only unrelated preceding fields; it is not
   a made-up exception getter or setter. Fs must point to runtime-owned task
   storage with these original offsets. Keep the offset checks in the source
   so a layout change cannot silently corrupt a catch.

   Compiler/PrsStmt.HC:842-902,1089-1119 and Kernel/KExcept.HC:64-113
   determine the handler, return and propagation cases below. *)

let headers =
  {|
#define FALSE 0
#define TRUE 1
class CTask {
  U8 original_prefix[816];
  I64 except_ch;
  U8 *except_callers[8];
  Bool catch_except;
};
extern U0 SysTry(U8 *catch_start,U8 *untry_start);
extern U0 SysUntry();
extern U0 throw(I64 ch=0,Bool no_log=FALSE);
public _intern 21 CTask *Fs();
extern U0 Print(U8 *fmt,...);
extern U0 PutChars(U64 ch);
I64 ExceptionLayout(){return offset(CTask.except_ch)==816 &&
  offset(CTask.except_callers)==824 && offset(CTask.catch_except)==888;}
if(!ExceptionLayout())throw('Layout');
|}

type success = {
  name : string;
  program : string;
  value : int64;
  output : string;
}

let successful name program value = { name; program; value; output = "" }

let successful_output name program output value =
  { name; program; value; output }

(* Both public source compilation modes run these same ordinary HolyC
   programs. In particular the catch is only handled when the source
   explicitly sets Fs->catch_except. *)
let success_cases =
  [
    successful "top-level normal try skips catch"
      {|I64 N=40;try {N+=2;}catch {N=0;Fs->catch_except=TRUE;}N;|} 42L;
    successful "top-level throw enters catch"
      {|I64 N=0;try {throw('Top');N=999;}catch {if(Fs->except_ch=='Top'){N=42;Fs->catch_except=TRUE;}}N;|}
      42L;
    successful "normal path skips catch"
      {|I64 F(){I64 n=40;try {n+=2;} catch {n=0;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "throw transfers past protected tail"
      {|I64 F(){I64 n=0;try {n++;throw('Point1');n=100;}catch {if(Fs->except_ch=='Point1'){n+=40;Fs->catch_except=TRUE;}}return n+1;}F();|}
      42L;
    successful "default throw call raises zero"
      {|I64 F(){I64 n=0;try throw;catch {if(Fs->except_ch==0)n=42;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "explicit zero throw raises zero"
      {|I64 F(){I64 n=0;try throw(0);catch {if(Fs->except_ch==0)n=42;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "throw no_log preserves source value"
      {|I64 F(){I64 n=0;try {throw('Silent',TRUE);n=100;}catch {if(Fs->except_ch=='Silent')n=42;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "unmarked inner catch propagates to outer"
      {|I64 F(){I64 n=0;try {try {throw('Point2');n=99;}catch {n+=10;}n=100;}catch {if(Fs->except_ch=='Point2'){n+=30;Fs->catch_except=TRUE;}}return n+2;}F();|}
      42L;
    successful "matched inner catch prevents outer handler"
      {|I64 F(){I64 n=0;try {try throw('Inner');catch {if(Fs->except_ch=='Inner'){n=40;Fs->catch_except=TRUE;}}n+=2;}catch {n=999;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "conditional throw from catch reenters handler"
      {|I64 F(){I64 n=0;try throw('First');catch {n++;if(n==1)throw('Second');if(Fs->except_ch=='Second'){n+=40;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "nested handled throw preserves the task catch state"
      {|I64 F(){I64 n=0;try throw('Outer');catch {n++;try throw('Inner');catch {if(Fs->except_ch=='Inner'){n+=40;Fs->catch_except=TRUE;}}if(Fs->except_ch=='Inner')n++;}return n;}F();|}
      42L;
    successful "rejected nested catch resumes its active outer handler"
      {|I64 F(){I64 n=0;try throw('Outer');catch {n++;if(n==1){try throw('Inner');catch {n+=10;}}if(Fs->except_ch=='Inner'){n+=30;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "throw propagates across function calls"
      {|I64 N=0;U0 Leaf(){N+=10;throw('Leaf');N+=100;}U0 Middle(){Leaf();N+=1000;}I64 F(){try Middle();catch {if(Fs->except_ch=='Leaf'){N+=32;Fs->catch_except=TRUE;}}return N;}F();|}
      42L;
    successful "throwing child keeps writes to caller storage"
      {|U0 Child(I64 *result){I64 n=42;*result=n;throw('Exit');}I64 F(){I64 found=0;try Child(&found);catch {if(Fs->except_ch=='Exit')Fs->catch_except=TRUE;}return found;}F();|}
      42L;
    successful "argument throw skips target invocation"
      {|I64 N=0;I64 Arg(){N+=2;throw('Arg');return 7;}I64 Target(I64 x){N+=100;return x;}I64 F(){try Target(Arg());catch {if(Fs->except_ch=='Arg'){N+=40;Fs->catch_except=TRUE;}}return N;}F();|}
      42L;
    successful "unused conditional throw stays inactive"
      {|I64 F(){I64 n=40;try {if(0)throw('Skip');n+=2;}catch {n=0;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "nested return unregisters both handlers"
      {|I64 F(){try {try {return 42;}catch {Fs->catch_except=TRUE;}}catch {Fs->catch_except=TRUE;}return 0;}F();|}
      42L;
    successful "returned try frame can be invoked twice"
      {|I64 F(){try {return 21;}catch {Fs->catch_except=TRUE;}return 0;}F()+F();|}
      42L;
    successful "return operand runs after current try is unregistered"
      {|I64 Raises(){throw('Eval');return 0;}I64 Inner(){try {return Raises();}catch {Fs->catch_except=TRUE;}return 0;}I64 F(){I64 n=0;try {n=Inner();}catch {if(Fs->except_ch=='Eval'){n=42;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "subsequent calls register fresh exception frames"
      {|I64 F(I64 fail){I64 n=40;try {if(fail)throw('Retry');n+=2;}catch {if(Fs->except_ch=='Retry'){n+=2;Fs->catch_except=TRUE;}}return n;}F(1)+F(0)-42;|}
      42L;
    successful "recursive exception releases younger calls"
      {|I64 N=0;I64 Recurse(I64 depth){I64 local=depth;N++;if(depth==0)throw('Leaf');return local+Recurse(depth-1);}I64 F(){try Recurse(3);catch {if(Fs->except_ch=='Leaf'){N+=38;Fs->catch_except=TRUE;}}return N;}F();|}
      42L;
    successful "break inside protected loop stays inside try"
      {|I64 F(){I64 n=40;try {while(1){n+=2;break;}}catch {n=0;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "goto within the same try arm retains handler"
      {|I64 F(){I64 n=0;try {goto hit;n=999;hit:n=42;}catch {n=0;Fs->catch_except=TRUE;}return n;}F();|}
      42L;
    successful "later throw after prior handled invocation"
      {|I64 F(I64 x){I64 n=40;try {if(x)throw('Again');n+=2;}catch {if(Fs->except_ch=='Again'){n+=2;Fs->catch_except=TRUE;}}return n;}F(1)-F(0)+F(1);|}
      42L;
    successful "named UndefExt source exception"
      {|I64 F(){I64 n=0;try {throw('UndefExt');n=999;}catch {if(Fs->except_ch=='UndefExt'){n=42;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "reached undefined extern becomes UndefExt"
      {|extern U0 Missing();I64 F(){I64 n=0;try {Missing();n=999;}catch {if(Fs->except_ch=='UndefExt'){n=42;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "divide by zero becomes DivZero"
      {|I64 Divide(I64 x,I64 y){return x/y;}I64 F(){I64 n=0;try {n=Divide(21,0);n=999;}catch {if(Fs->except_ch=='DivZero'){n=42;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "modulo by zero becomes DivZero"
      {|I64 Rem(I64 x,I64 y){return x%y;}I64 F(){I64 n=0;try {n=Rem(21,0);}catch {if(Fs->except_ch=='DivZero'){n=42;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "signed division overflow becomes DivZero"
      {|I64 Divide(I64 x,I64 y){return x/y;}I64 F(){I64 n=0;try {n=Divide(0x8000000000000000,-1);}catch {if(Fs->except_ch=='DivZero'){n=42;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful "signed modulo overflow becomes DivZero"
      {|I64 Rem(I64 x,I64 y){return x%y;}I64 F(){I64 n=0;try {n=Rem(0x8000000000000000,-1);}catch {if(Fs->except_ch=='DivZero'){n=42;Fs->catch_except=TRUE;}}return n;}F();|}
      42L;
    successful_output "throw retains earlier output and follows catch"
      {|I64 F(){Print("before");try {Print("body");throw('Print');Print("skipped");}catch {Print("catch");Fs->catch_except=TRUE;}Print("after");return 42;}F();|}
      "beforebodycatchafter" 42L;
    successful_output "handled child effects persist"
      {|U0 G(){Print("leaf");throw('Leaf');Print("skipped");}I64 F(){Print("first");try G();catch {Print("caught");Fs->catch_except=TRUE;}return 42;}F();|}
      "firstleafcaught" 42L;
  ]

type failure = { name : string; program : string; code : string option }

let failure ?code name program = { name; program; code }

(* Reaching an invalid transfer must be rejected even if the branch is
   statically unselected. The source's plain IC_JMP has no SysUntry, and
   entering a protected arm would bypass SysTry. Return from catch is
   explicitly unsafe in Demo/Exceptions.HC:1-2. *)
let invalid_transfers =
  [
    failure ~code:"HCRUN0001" "goto enters try without registering handler"
      {|I64 F(){goto inside;try {inside:return 42;}catch {Fs->catch_except=TRUE;}return 0;}F();|};
    failure ~code:"HCRUN0001" "goto leaves try without running SysUntry"
      {|I64 F(){try {goto outside;}catch {Fs->catch_except=TRUE;}outside:return 42;}F();|};
    failure ~code:"HCRUN0001" "goto enters catch from outside"
      {|I64 F(){goto handler;try {return 0;}catch {handler:Fs->catch_except=TRUE;}return 42;}F();|};
    failure ~code:"HCRUN0001" "goto leaves catch for normal code"
      {|I64 F(){try throw('X');catch {goto outside;Fs->catch_except=TRUE;}outside:return 42;}F();|};
    failure ~code:"HCRUN0001" "goto crosses sibling try arms"
      {|I64 F(){try {goto sibling;}catch {Fs->catch_except=TRUE;}try {sibling:return 42;}catch {Fs->catch_except=TRUE;}return 0;}F();|};
    failure ~code:"HCRUN0001" "return inside catch skips exception dispatch"
      {|I64 F(){try throw('X');catch {Fs->catch_except=TRUE;return 42;}return 0;}F();|};
    failure ~code:"HCPARSE0170" "break cannot inherit an outer loop across try"
      {|I64 F(){while(1)try break;catch {Fs->catch_except=TRUE;}return 42;}F();|};
  ]

let uncaught =
  [
    failure ~code:"HCEX0001" "unhandled source throw terminates"
      {|I64 F(){throw('Lost');return 42;}F();|};
    failure ~code:"HCEX0001"
      "unmarked handler propagates without an outer catch"
      {|I64 F(){try throw('Lost');catch {I64 n=1;n++;}return 42;}F();|};
    failure "unhandled arithmetic stays an execution fault"
      {|I64 F(I64 x){return 1/x;}F(0);|};
    failure ~code:"HCIRVM0015"
      "unbounded catch reentry reaches the hosted call depth limit"
      {|I64 F(){try throw('Again');catch throw('Again');return 42;}F();|};
  ]

(* The canonical result is deliberately unaffected by the size of a previous
   or subsequently installed exception stack. Runtime quota faults are not
   synthesized into source throw codes. *)
let quota_source =
  {|I64 F(){I64 n=40;try {n++;throw('Q');n=999;}catch {if(Fs->except_ch=='Q'){n++;Fs->catch_except=TRUE;}}return n;}F();|}

let preparation_fault =
  {|I64 F(){try {I64 a[2];a[0]=42;return a[0];}catch {Fs->catch_except=TRUE;}return 0;}F();|}
