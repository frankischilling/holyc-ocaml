U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *RSrc="#include \"/Kernel/KernelA.HH\"\n#exe {POValue(\"BEFORE\",GetOption(34));Option(34,1);POValue(\"SET\",GetOption(34));}\npublic I64 ANoOpt(I8 x,I64 c){x<<=c;return x;}\n#exe {POValue(\"AFTER\",GetOption(34));Option(34,0);}\n";
FileWrite("B:/REGNOOPT2.HC",RSrc,StrLen(RSrc));I64 RE=Cmp("B:/REGNOOPT2.HC");POValue("COMPILE_ERRORS",RE);if(!RE){Load("B:/REGNOOPT2.BIN");ExePrint("extern I64 ANoOpt(I8 x,I64 c);POValue(\"AOT_NOOPT\",ANoOpt(127,1));");}POOut("POINTER_DONE\n");
