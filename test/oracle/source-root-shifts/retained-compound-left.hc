U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 XR=7,CR=65,NR=0;I64 FR(I64 x,I64 c){NR++;x<<=c;return x;}I64 IR(){return FR(XR,CR);}I64 SR(I64 n=IR()){return n;}POValue(\"VALUE\",SR());POValue(\"CALLS\",NR);}\n";
FileWrite("B:/RETCOMPOUND.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/RETCOMPOUND.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
