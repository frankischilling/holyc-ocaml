U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 ALCalls=0;
I64 ALNext(){ALCalls++;return 42;}
I64 ALPlain(){I64 noreg before=7;U0 x=ALNext();return before;}
I64 ALAfter(){U0 x=ALNext();I64 noreg after=9;return after;}
U0 ALCode(U8 *name,U8 *p){U8 text[1024],record[1400],*start;I64 i,j,target;for(i=0;i<18;i++){start=p;Ui(text,&p,64,&target,TRUE);StrPrint(record,"CODE|%s|%d|",name,i);for(j=0;j<p-start;j++)CatPrint(record,"%02X",start[j]);CatPrint(record,"|%s",text);POOut(record);if(start[0]==0xC3)break;}}
ALCode("BEFORE",&ALPlain);ALCode("AFTER",&ALAfter);
POValue("BEFORE",ALPlain());POValue("AFTER",ALAfter());POValue("CALLS",ALCalls);
POOut("POINTER_DONE\n");
