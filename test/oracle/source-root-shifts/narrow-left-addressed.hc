U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 FR(I8 x,I8 c){I8 *p=&x;*p<<=c;return x;}I64 FU(U8 x,I8 c){U8 *p=&x;*p<<=c;return x;}POValue(\"SIGNED\",FR(127,1));POValue(\"UNSIGNED\",FU(255,1));}\n";
FileWrite("B:/RETNADDR.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/RETNADDR.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
