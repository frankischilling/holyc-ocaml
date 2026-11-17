U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 JU=1;I64 JN=7;I64 JGet(I64 n){return n;}I64 JO=JGet(JU<<JN);I8 JS=-8,JC=1;I64 JR=JGet(JS>>JC);POValue("JIT_MIXED",JO);POValue("JIT_SIGNED",JR);
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\nU8 AU=1;I64 AN=7;I64 AGet(I64 n){return n;}I64 AO=AGet(AU<<AN);I8 AS=-8,AC=1;I64 AR=AGet(AS>>AC);public I64 AReadM(){return AO;}public I64 AReadS(){return AR;}\n";
FileWrite("B:/ARGSHIFT.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/ARGSHIFT.HC");POValue("COMPILE_ERRORS",AE);if(!AE){Load("B:/ARGSHIFT.BIN");ExePrint("extern I64 AReadM();extern I64 AReadS();POValue(\"AOT_MIXED\",AReadM());POValue(\"AOT_SIGNED\",AReadS());");}POOut("POINTER_DONE\n");
