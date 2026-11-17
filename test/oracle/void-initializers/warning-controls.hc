U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 WCCalls=0;
U0 WCNext(){WCCalls++;}
ExePrint("U0 (*WCPtr)()=&WCNext;class WCBox{U0 empty;U8 value;};WCBox WCObject={WCPtr,42};POValue(\"CALLBACK_WORD_WARNINGS\",Fs->last_cc->warning_cnt);POValue(\"CALLBACK_WORD_CALLS\",WCCalls);POValue(\"CALLBACK_WORD_VALUE\",WCObject.value);");
ExePrint("WCBox WCAddress={&WCNext,42};POValue(\"FUNCTION_ADDRESS_WARNINGS\",Fs->last_cc->warning_cnt);POValue(\"FUNCTION_ADDRESS_CALLS\",WCCalls);");
ExePrint("WCBox WCInteger={7,42};POValue(\"INTEGER_LEAF_WARNINGS\",Fs->last_cc->warning_cnt);");
POOut("POINTER_DONE\n");
