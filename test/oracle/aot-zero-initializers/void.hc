U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ATControl="#include \"/Kernel/KernelA.HH\"\npublic I64 ATGetControl(){return 42;}\n";
FileWrite("B:/ATCONTROL.HC",ATControl,StrLen(ATControl));
I64 ATControlErrors=Cmp("B:/ATCONTROL.HC");
POValue("CONTROL_ERRORS",ATControlErrors);
if(!ATControlErrors){Load("B:/ATCONTROL.BIN");ExePrint("extern I64 ATGetControl();POValue(\"CONTROL_VALUE\",ATGetControl());");}
U8 *ATSource="#include \"/Kernel/KernelA.HH\"\nU0 ATReturn(){asm {MOV RAX,7;}}\nclass ATBox{U0 empty;I64 value;};\nATBox ATObject={ATReturn(),42};\npublic I64 ATGet(){return ATObject.value;}\n";
FileWrite("B:/ATVOID.HC",ATSource,StrLen(ATSource));
I64 ATErrors=Cmp("B:/ATVOID.HC");
POValue("VOID_ERRORS",ATErrors);
if(!ATErrors){Load("B:/ATVOID.BIN");ExePrint("extern I64 ATGet();POValue(\"VOID_VALUE\",ATGet());");}
POOut("POINTER_DONE\n");
