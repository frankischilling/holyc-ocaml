U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 EDText[]="AB";
I64 EDWords[]={};
class EDItem{U8 value;};EDItem EDItems[]={};
POValue("GLOBAL_STRING_BYTES",sizeof(EDText));
POValue("GLOBAL_WORD_BYTES",sizeof(EDWords));
POValue("GLOBAL_CLASS_BYTES",sizeof(EDItems));
I64 EDStaticText(){static U8 text[]="AB";return sizeof(text);}
I64 EDStaticWords(){static I64 words[]={};return sizeof(words);}
I64 EDAutomatic(){U8 text[];return sizeof(text);}
POValue("STATIC_STRING_BYTES",EDStaticText());
POValue("STATIC_WORD_BYTES",EDStaticWords());
POValue("AUTOMATIC_BYTES",EDAutomatic());
U8 EDFixed[3]="AB";
POValue("FIXED_BYTES",sizeof(EDFixed));
POValue("FIXED_LAST",EDFixed[2]);
I64 EDReached=0,EDCalls=0;
I64 EDEffect(){EDCalls++;return 42;}
ExePrint("I64 EDBad[]={EDEffect()};EDReached=1;");
POValue("NONEMPTY_REACHED",EDReached);
POValue("NONEMPTY_CALLS",EDCalls);
ExePrint("I64 EDNegative[-1]={};EDReached=1;");
POValue("NEGATIVE_REACHED",EDReached);
POOut("POINTER_DONE\n");
