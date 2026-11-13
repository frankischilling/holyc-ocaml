U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 NR=0;I64 FR(I8 x,I64 c){NR++;x>>=c;return x;}POValue(\"NEG\",FR(-7,-1));POValue(\"ZERO\",FR(-7,0));POValue(\"C63\",FR(-7,63));POValue(\"C64\",FR(-7,64));POValue(\"C65\",FR(-7,65));POValue(\"C128\",FR(-7,128));POValue(\"CALLS\",NR);}\n";
FileWrite("B:/NRIGHTCNT.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/NRIGHTCNT.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
