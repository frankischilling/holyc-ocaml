U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 NR=0;I64 F(I8 x,I64 c){I8 *p=&x;NR++;I64 a=(*p<<=c);return a*1000+x;}I64 I(){return F(127,65);}I64 S(I64 n=I()){return n;}POValue(\"VALUE\",S());POValue(\"CALLS\",NR);}\n";
FileWrite("B:/RETMEMBOTH.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/RETMEMBOTH.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
