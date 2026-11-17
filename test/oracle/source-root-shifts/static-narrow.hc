U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I8 SJI=-8,SJC=0;I64 SJF(){static I64 O=(++SJI)>>(++SJC);return O;}POValue("JIT_N",SJI);POValue("JIT_C",SJC);POValue("JIT_FIRST",SJF());SJI=7;POValue("JIT_SAVED",SJF());
U8 *SSSource="#include \"/Kernel/KernelA.HH\"\nI8 SSI=-8,SSC=0;public I64 SSNRead(){return SSI;}public I64 SSCRead(){return SSC;}public I64 SSF(){static I64 O=(++SSI)>>(++SSC);return O;}public U0 SSSet(){SSI=7;}\n";
FileWrite("B:/SNARROW.HC",SSSource,StrLen(SSSource));I64 SSErrors=Cmp("B:/SNARROW.HC");POValue("COMPILE_ERRORS",SSErrors);if(!SSErrors){Load("B:/SNARROW.BIN");ExePrint("extern I64 SSNRead();extern I64 SSCRead();extern I64 SSF();extern U0 SSSet();POValue(\"AOT_N\",SSNRead());POValue(\"AOT_C\",SSCRead());POValue(\"AOT_FIRST\",SSF());SSSet();POValue(\"AOT_SAVED\",SSF());");}POOut("POINTER_DONE\n");
