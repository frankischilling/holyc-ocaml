U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 CWJitGet(){return 7;}
class CWJitEmpty{};
class CWJitEmptyBox{CWJitEmpty empty;I64 value;};
CWJitEmptyBox CWJitEmptyObject={CWJitGet(),0x112233445566772a};
class CWJitBase{U8 bytes[3];};
class CWJitLeaf:CWJitBase{};
class CWJitBox{CWJitLeaf leaf;U8 tail[5];};
CWJitBox CWJitObject={CWJitGet(),{5,4,3,2,1}};
POValue("JIT_EMPTY",CWJitEmptyObject.value);
POValue("JIT_THREE",*(&CWJitObject)(I64*));
U8 *CWSource="#include \"/Kernel/KernelA.HH\"\nI64 CWGet(){return 7;}\nclass CWEmpty{};class CWEmptyBox{CWEmpty empty;I64 value;};CWEmptyBox CWEmptyObject={CWGet(),0x112233445566772a};\nclass CWBase{U8 bytes[3];};class CWLeaf:CWBase{};class CWBox{CWLeaf leaf;U8 tail[5];};CWBox CWObject={CWGet(),{5,4,3,2,1}},CWConstant={7,{5,4,3,2,1}};\npublic I64 CWReadEmpty(){return CWEmptyObject.value;}\npublic I64 CWReadThree(){return *(&CWObject)(I64*);}\npublic I64 CWReadConstant(){return *(&CWConstant)(I64*);}\npublic I64 CWReadStatic(){static CWBox b={CWGet(),{5,4,3,2,1}};return *(&b)(I64*);}\n";
FileWrite("B:/CWWIDTH.HC",CWSource,StrLen(CWSource));
I64 CWErrors=Cmp("B:/CWWIDTH.HC");
POValue("COMPILE_ERRORS",CWErrors);
if(!CWErrors){Load("B:/CWWIDTH.BIN");ExePrint("extern I64 CWReadEmpty();extern I64 CWReadThree();extern I64 CWReadConstant();extern I64 CWReadStatic();POValue(\"AOT_EMPTY\",CWReadEmpty());POValue(\"AOT_THREE\",CWReadThree());POValue(\"AOT_CONSTANT\",CWReadConstant());POValue(\"AOT_STATIC\",CWReadStatic());");}
POOut("POINTER_DONE\n");
