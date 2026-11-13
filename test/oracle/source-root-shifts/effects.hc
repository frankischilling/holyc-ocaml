U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 EJN=7;I64 EJO=(++EJN)<<1;POValue("JIT_N",EJN);POValue("JIT_O",EJO);EJN=20;POValue("JIT_SAVED",EJO);
U8 *ESSource="#include \"/Kernel/KernelA.HH\"\nI64 ESN=7;I64 ESO=(++ESN)<<1;public I64 ESReadN(){return ESN;}public I64 ESReadO(){return ESO;}public U0 ESSet(){ESN=20;}\n";
FileWrite("B:/EFFECT.HC",ESSource,StrLen(ESSource));I64 ESErrors=Cmp("B:/EFFECT.HC");POValue("COMPILE_ERRORS",ESErrors);if(!ESErrors){Load("B:/EFFECT.BIN");ExePrint("extern I64 ESReadN();extern I64 ESReadO();extern U0 ESSet();POValue(\"AOT_N\",ESReadN());POValue(\"AOT_O\",ESReadO());ESSet();POValue(\"AOT_SAVED\",ESReadO());");}POOut("POINTER_DONE\n");
