U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 DJN=-3;I64 DJO=DJN/2;POValue("JIT_DIV",DJO);
U8 *DSSource="#include \"/Kernel/KernelA.HH\"\nI64 DSN=-3;I64 DSO=DSN/2;public I64 DSRead(){return DSO;}\n";
FileWrite("B:/DIV.HC",DSSource,StrLen(DSSource));I64 DSErrors=Cmp("B:/DIV.HC");POValue("COMPILE_ERRORS",DSErrors);if(!DSErrors){Load("B:/DIV.BIN");ExePrint("extern I64 DSRead();POValue(\"AOT_DIV\",DSRead());");}POOut("POINTER_DONE\n");
