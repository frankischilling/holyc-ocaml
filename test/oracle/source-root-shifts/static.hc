U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 SJN=7;I64 SJF(){static I64 O=(++SJN)<<1;return O;}POValue("JIT_N_BEFORE",SJN);POValue("JIT_FIRST",SJF());SJN=20;POValue("JIT_SECOND",SJF());
U8 *SSSource="#include \"/Kernel/KernelA.HH\"\nI64 SSN=7;public I64 SSReadN(){return SSN;}public I64 SSF(){static I64 O=(++SSN)<<1;return O;}public U0 SSSet(){SSN=20;}\n";
FileWrite("B:/STATIC.HC",SSSource,StrLen(SSSource));I64 SSErrors=Cmp("B:/STATIC.HC");POValue("COMPILE_ERRORS",SSErrors);if(!SSErrors){Load("B:/STATIC.BIN");ExePrint("extern I64 SSReadN();extern I64 SSF();extern U0 SSSet();POValue(\"AOT_N_BEFORE\",SSReadN());POValue(\"AOT_FIRST\",SSF());SSSet();POValue(\"AOT_SECOND\",SSF());");}POOut("POINTER_DONE\n");
