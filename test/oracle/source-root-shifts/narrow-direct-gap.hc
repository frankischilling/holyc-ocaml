U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 JReg(I8 x,I8 c){x<<=c;return x;}I64 JMem(I8 x,I8 c){I8 *p=&x;*p<<=c;return x;}POValue("JIT_REG",JReg(127,1));POValue("JIT_MEM",JMem(127,1));
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\npublic I64 AReg(I8 x,I8 c){x<<=c;return x;}public I64 AMem(I8 x,I8 c){I8 *p=&x;*p<<=c;return x;}\n";
FileWrite("B:/NARROWDIRECT.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/NARROWDIRECT.HC");POValue("COMPILE_ERRORS",AE);if(!AE){Load("B:/NARROWDIRECT.BIN");ExePrint("extern I64 AReg(I8 x,I8 c);extern I64 AMem(I8 x,I8 c);POValue(\"AOT_REG\",AReg(127,1));POValue(\"AOT_MEM\",AMem(127,1));");}POOut("POINTER_DONE\n");
