U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 LWJNext(I64 n){return n;}extern class LWJItem;class LWJBox{LWJItem item;U8 tail;$$=8;};
I64 LWJ(){static LWJBox object={LWJNext(
#exe { StreamExePrint("class LWJItem{U16 value;};"); }
7),42};U8 *p=(&object)(U8*);return p[2];}
POValue("JIT_LOOKAHEAD",LWJ());
U8 *LWSource="#include \"/Kernel/KernelA.HH\"\nI64 LWNext(I64 n){return n;}extern class LWItem;class LWBox{LWItem item;U8 tail;$$$$=8;};\npublic I64 LWRead(){static LWBox object={LWNext(\n#exe {StreamExePrint(\"class LWItem{U16 value;};\");}\n7),42};U8 *p=(&object)(U8*);return p[2];}\n";
FileWrite("B:/LWSTORE.HC",LWSource,StrLen(LWSource));
I64 LWErrors=Cmp("B:/LWSTORE.HC");POValue("COMPILE_ERRORS",LWErrors);
if(!LWErrors){Load("B:/LWSTORE.BIN");ExePrint("extern I64 LWRead();POValue(\"AOT_LOOKAHEAD\",LWRead());");}
POOut("POINTER_DONE\n");
