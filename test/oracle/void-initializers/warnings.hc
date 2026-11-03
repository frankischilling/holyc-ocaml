U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 VWCalls=0;
U0 VWNext(){VWCalls++;}
ExePrint("VWNext();POValue(\"STATEMENT_WARNINGS\",Fs->last_cc->warning_cnt);");
ExePrint("U0 VWGlobal=VWNext();POValue(\"GLOBAL_WARNINGS\",Fs->last_cc->warning_cnt);");
ExePrint("class VWBox{U0 empty;U8 value;};VWBox VWObject={VWNext(),42};POValue(\"AGGREGATE_WARNINGS\",Fs->last_cc->warning_cnt);POValue(\"AGGREGATE_VALUE\",VWObject.value);");
ExePrint("I64 VWStatic(){static VWBox box={VWNext(),42};return box.value;}POValue(\"STATIC_WARNINGS\",Fs->last_cc->warning_cnt);POValue(\"STATIC_SUM\",VWStatic()+VWStatic());POValue(\"WARNINGS_AFTER_CALLS\",Fs->last_cc->warning_cnt);");
POValue("CALLS",VWCalls);
POOut("POINTER_DONE\n");
