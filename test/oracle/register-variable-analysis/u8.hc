U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 JUReg(U8 x,I8 c){x<<=c;return x;}I64 JUNo(U8 noreg x,I8 c){x<<=c;return x;}POValue("JIT_REG",JUReg(255,1));POValue("JIT_NOREG",JUNo(255,1));
U8 *RSrc="#include \"/Kernel/KernelA.HH\"\npublic I64 AUReg(U8 x,I8 c){x<<=c;return x;}public I64 AUNo(U8 noreg x,I8 c){x<<=c;return x;}\n";
FileWrite("B:/REGU8.HC",RSrc,StrLen(RSrc));I64 RE=Cmp("B:/REGU8.HC");POValue("COMPILE_ERRORS",RE);if(!RE){Load("B:/REGU8.BIN");ExePrint("extern I64 AUReg(U8 x,I8 c);extern I64 AUNo(U8 noreg x,I8 c);POValue(\"AOT_REG\",AUReg(255,1));POValue(\"AOT_NOREG\",AUNo(255,1));");}POOut("POINTER_DONE\n");
