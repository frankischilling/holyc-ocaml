U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 PVCount=0,PVTarget=7;
U0 PVEffect(){PVCount++;}
I64 PVWord(){PVCount++;return 42;}
I64 *PVRef(){PVCount++;return &PVTarget;}
U0 PVEmpty;
POValue("UNINITIALIZED_SIZE",sizeof(PVEmpty));
ExePrint("U0 PVVoid=PVEffect();POValue(\"VOID_WARNINGS\",Fs->last_cc->warning_cnt);");
POValue("VOID_SIZE",sizeof(PVVoid));
ExePrint("U0 PVInteger=PVWord();POValue(\"INTEGER_WARNINGS\",Fs->last_cc->warning_cnt);");
ExePrint("U0 PVReference=PVRef();POValue(\"REFERENCE_WARNINGS\",Fs->last_cc->warning_cnt);");
POValue("GLOBAL_CALLS",PVCount);
I64 PVStatic(){static U0 empty,value=PVEffect();return sizeof(empty)+sizeof(value)+21;}
POValue("STATIC_CALLS_BEFORE",PVCount);
POValue("STATIC_SUM",PVStatic()+PVStatic());
POValue("STATIC_CALLS_AFTER",PVCount);
POValue("TARGET",PVTarget);
POOut("POINTER_DONE\n");
