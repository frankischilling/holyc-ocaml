U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 POData=40,PONotes=0;
I64 *PORef(){PONotes++;return &POData;}
class POBox{U8 tag;I64 *value;};
POBox POObject={7,PORef()};
POValue("TAG",POObject.tag);
POValue("READ",*POObject.value);
*POObject.value=41;
POValue("ALIAS",POData);
I64 POStatic(){static POBox saved={9,PORef()};return ++(*saved.value);}
POValue("STATIC_SUM",POStatic()+POStatic());
POValue("FINAL_DATA",POData);
POValue("INITIALIZATIONS",PONotes);
POValue("PACKED_BYTES",sizeof(POBox));
POOut("POINTER_DONE\n");
