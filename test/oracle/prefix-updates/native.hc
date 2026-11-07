U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 PMData[3]={40,41,42};
I64 PMLocal(){I64 *p=PMData;I64 a=++*p,b=++*p;if(a!=41 || b!=42)return -1;return PMData[0]*10+(p==&PMData[2]);}
POValue("LOCAL_POINTER",PMLocal());
I64 PMDown(){I64 *p=&PMData[2];I64 a=--*p,b=--*p;if(a!=41 || b!=40)return -1;return PMData[0]*10+(p==&PMData[0]);}
POValue("LOCAL_DECREMENT",PMDown());
I64 PMMinus(){I64 x=4,y=++-x;return x*100+y;}
POValue("MINUS",PMMinus());
I64 PMNot(){I64 x=0,y=++!x;return x*100+y;}
POValue("NOT",PMNot());
I64 PMMixed(){I64 x=4,y=++--x;return x*100+y;}
POValue("MIXED",PMMixed());
I64 PMPost(){I64 x=4,y=++x++;return x*100+y;}
POValue("POST",PMPost());
I64 PMGrouped(){I64 x=4,y=++(-x);return x*100+y;}
POValue("GROUPED_MINUS",PMGrouped());
POOut("POINTER_DONE\n");
