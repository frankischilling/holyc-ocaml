U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 FV(I8 x,I8 c){I8 *p=&x;return *p<<=c;}I64 FS(I8 x,I8 c){I8 *p=&x;*p<<=c;return x;}POValue(\"EXPRESSION\",FV(127,1));POValue(\"STORED\",FS(127,1));}\n";
FileWrite("B:/RETMEM.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/RETMEM.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
