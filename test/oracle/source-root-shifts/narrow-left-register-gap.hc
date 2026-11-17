U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 NR=0;I64 FR(I8 x,I8 c){NR++;x<<=c;return x;}I64 FU(U8 x,I8 c){x<<=c;return x;}I64 IR(){return FR(127,1);}I64 SR(I64 n=IR()){return n;}POValue(\"SIGNED\",SR());POValue(\"CALLS\",NR);POValue(\"UNSIGNED\",FU(255,1));}\n";
FileWrite("B:/RETNARROW.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/RETNARROW.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
