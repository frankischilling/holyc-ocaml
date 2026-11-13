U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 ZWJitGet(){return 7;}
class ZWJitBox{U0 empty;I64 value;};
ZWJitBox ZWJit={ZWJitGet(),0x112233445566772a};
POValue("JIT_INTEGER",ZWJit.value);
U8 *ZWSource="#include \"/Kernel/KernelA.HH\"\nI64 ZWGet(){return 7;}\nclass ZWBox{U0 empty;I64 value;};\nZWBox ZWInteger={ZWGet(),0x112233445566772a},ZWConstant={7,0x112233445566772a};\npublic I64 ZWReadInteger(){return ZWInteger.value;}\npublic I64 ZWReadConstant(){return ZWConstant.value;}\npublic I64 ZWReadStatic(){static ZWBox b={ZWGet(),0x112233445566772a};return b.value;}\n";
FileWrite("B:/ZWWIDTH.HC",ZWSource,StrLen(ZWSource));
I64 ZWErrors=Cmp("B:/ZWWIDTH.HC");
POValue("COMPILE_ERRORS",ZWErrors);
if(!ZWErrors){Load("B:/ZWWIDTH.BIN");ExePrint("extern I64 ZWReadInteger();extern I64 ZWReadConstant();extern I64 ZWReadStatic();POValue(\"AOT_INTEGER\",ZWReadInteger());POValue(\"AOT_CONSTANT\",ZWReadConstant());POValue(\"AOT_STATIC\",ZWReadStatic());");}
POOut("POINTER_DONE\n");
