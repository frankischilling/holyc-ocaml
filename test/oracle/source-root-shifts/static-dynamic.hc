U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 SJN=7,SJC=64;I64 SJF(){static I64 O=(++SJN)<<(++SJC);return O;}POValue("JIT_N",SJN);POValue("JIT_C",SJC);POValue("JIT_FIRST",SJF());SJC=1;POValue("JIT_SAVED",SJF());
U8 *SSSource="#include \"/Kernel/KernelA.HH\"\nI64 SSN=7,SSC=64;public I64 SSNRead(){return SSN;}public I64 SSCRead(){return SSC;}public I64 SSF(){static I64 O=(++SSN)<<(++SSC);return O;}public U0 SSSet(){SSC=1;}\n";
FileWrite("B:/SDYN.HC",SSSource,StrLen(SSSource));I64 SSErrors=Cmp("B:/SDYN.HC");POValue("COMPILE_ERRORS",SSErrors);if(!SSErrors){Load("B:/SDYN.BIN");ExePrint("extern I64 SSNRead();extern I64 SSCRead();extern I64 SSF();extern U0 SSSet();POValue(\"AOT_N\",SSNRead());POValue(\"AOT_C\",SSCRead());POValue(\"AOT_FIRST\",SSF());SSSet();POValue(\"AOT_SAVED\",SSF());");}POOut("POINTER_DONE\n");
