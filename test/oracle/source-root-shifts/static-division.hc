U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 DJN=-3;I64 DJF(){static I64 O=DJN/2;return O;}POValue("JIT_DIV",DJF());
U8 *DSSource="#include \"/Kernel/KernelA.HH\"\nI64 DSN=-3;public I64 DSF(){static I64 O=DSN/2;return O;}\n";
FileWrite("B:/SDIV.HC",DSSource,StrLen(DSSource));I64 DSErrors=Cmp("B:/SDIV.HC");POValue("COMPILE_ERRORS",DSErrors);if(!DSErrors){Load("B:/SDIV.BIN");ExePrint("extern I64 DSF();POValue(\"AOT_DIV\",DSF());");}POOut("POINTER_DONE\n");
