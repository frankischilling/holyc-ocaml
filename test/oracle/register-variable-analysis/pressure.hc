U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 JSmall(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e){x<<=c;return x+a+b+d+e;}I64 JLarge(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e,I64 f,I64 g,I64 h,I64 i){x<<=c;return x+a+a+a+b+b+b+d+d+d+e+e+e+f+f+f+g+g+g+h+h+h+i+i+i;}
POValue("JIT_SMALL",JSmall(127,1,0,0,0,0));POValue("JIT_LARGE",JLarge(127,1,0,0,0,0,0,0,0,0));
U8 *RSrc="#include \"/Kernel/KernelA.HH\"\npublic I64 ASmall(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e){x<<=c;return x+a+b+d+e;}public I64 ALarge(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e,I64 f,I64 g,I64 h,I64 i){x<<=c;return x+a+a+a+b+b+b+d+d+d+e+e+e+f+f+f+g+g+g+h+h+h+i+i+i;}\n";
FileWrite("B:/REGPRESS.HC",RSrc,StrLen(RSrc));I64 RE=Cmp("B:/REGPRESS.HC");POValue("COMPILE_ERRORS",RE);if(!RE){Load("B:/REGPRESS.BIN");ExePrint("extern I64 ASmall(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e);extern I64 ALarge(I8 x,I64 c,I64 a,I64 b,I64 d,I64 e,I64 f,I64 g,I64 h,I64 i);POValue(\"AOT_SMALL\",ASmall(127,1,0,0,0,0));POValue(\"AOT_LARGE\",ALarge(127,1,0,0,0,0,0,0,0,0));");}POOut("POINTER_DONE\n");
