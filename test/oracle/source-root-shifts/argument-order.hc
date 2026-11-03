U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 JN=3,JC=1;I64 JPair(I64 a,I64 b){return a*100+b;}I64 JR=JPair((++JN)<<JC,(++JN)<<JC);POValue("JIT_N",JN);POValue("JIT_RESULT",JR);
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\nI64 AN=3,AC=1;I64 APair(I64 a,I64 b){return a*100+b;}I64 AR=APair((++AN)<<AC,(++AN)<<AC);public I64 AReadN(){return AN;}public I64 AReadR(){return AR;}\n";
FileWrite("B:/ARGORDER.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/ARGORDER.HC");POValue("COMPILE_ERRORS",AE);if(!AE){Load("B:/ARGORDER.BIN");ExePrint("extern I64 AReadN();extern I64 AReadR();POValue(\"AOT_N\",AReadN());POValue(\"AOT_RESULT\",AReadR());");}POOut("POINTER_DONE\n");
