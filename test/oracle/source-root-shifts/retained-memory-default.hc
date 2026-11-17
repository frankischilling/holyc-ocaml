U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 NR=0;I64 FR(I8 x,I8 c){I8 *p=&x;NR++;*p<<=c;return x;}I64 IR(){return FR(127,1);}I64 SR(I64 n=IR()){return n;}POValue(\"VALUE\",SR());POValue(\"CALLS\",NR);}\n";
FileWrite("B:/RETMEMDEF.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/RETMEMDEF.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
