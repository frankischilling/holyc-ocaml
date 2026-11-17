U0 VOOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 VOValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);VOOut(buf);}
I64 VOCount=0;
U0 VOEffect(){VOCount++;}
class VOBox{U0 empty;U8 value;};
VOBox VOObject={VOEffect(),42};
VOValue("GLOBAL_COUNT",VOCount);
VOValue("GLOBAL_VALUE",VOObject.value);
I64 VOStatic(){static VOBox object={VOEffect(),42};return object.value;}
VOValue("STATIC_VALUE",VOStatic()+VOStatic());
VOValue("STATIC_COUNT",VOCount);
VOOut("VOID_DONE\n");
