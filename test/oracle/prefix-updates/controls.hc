U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 data[3]={40,41,42};class Box{I64 *values[2];};Box object={{data,data}};I64 F(){I64 index=0;I64 value=++*object.values[index++];if(value!=41 || index!=1 || object.values[0]!=&data[1] || data[0]!=40)return -1;return 42;}POValue("CONTROL_0",F());
I64 data[3]={40,41,42};class Box{I64 *values[2];};Box object={{data,data}};I64 F(){I64 index=0;I64 value=++(*object.values[index++]);if(value!=41 || index!=1 || object.values[0]!=&data[0] || data[0]!=41)return -1;return 42;}POValue("CONTROL_1",F());
I64 data[3]={40,41,42};class Box{I64 *value;};Box object={&data[2]};I64 F(){I64 value=--*object.value;if(value!=41 || object.value!=&data[1] || data[2]!=42)return -1;return 42;}POValue("CONTROL_2",F());
I64 F(){I64 value=4;I64 result=++~value;if(value!=5 || result!=-6)return -1;return 42;}POValue("CONTROL_3",F());
POOut("POINTER_DONE\n");
