U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I8 JPos=127,JNeg=-128,JC=1;U8 JU=255;I64 JP=JPos<<JC;I64 JN=JNeg<<JC;I64 JUU=JU<<JC;I64 JPC=JPos<<1;POValue("JIT_POS",JP);POValue("JIT_NEG",JN);POValue("JIT_UNSIGNED",JUU);POValue("JIT_CONST",JPC);
U8 *LSrc="#include \"/Kernel/KernelA.HH\"\nI8 APos=127,ANeg=-128,AC=1;U8 AU=255;I64 AP=APos<<AC;I64 AN=ANeg<<AC;I64 AUU=AU<<AC;I64 APC=APos<<1;public I64 NLeftP(){return AP;}public I64 NLeftN(){return AN;}public I64 NLeftU(){return AUU;}public I64 NLeftC(){return APC;}\n";
FileWrite("B:/NLEFT.HC",LSrc,StrLen(LSrc));I64 LE=Cmp("B:/NLEFT.HC");POValue("COMPILE_ERRORS",LE);if(!LE){Load("B:/NLEFT.BIN");ExePrint("extern I64 NLeftP();extern I64 NLeftN();extern I64 NLeftU();extern I64 NLeftC();POValue(\"AOT_POS\",NLeftP());POValue(\"AOT_NEG\",NLeftN());POValue(\"AOT_UNSIGNED\",NLeftU());POValue(\"AOT_CONST\",NLeftC());");}POOut("POINTER_DONE\n");
