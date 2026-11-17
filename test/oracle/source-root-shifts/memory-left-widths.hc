U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *ASrc="#include \"/Kernel/KernelA.HH\"\n#exe {I64 FI8(I8 x,I64 c){I8 *p=&x;return *p<<=c;}I64 FU8(U8 x,I64 c){U8 *p=&x;return *p<<=c;}I64 FI16(I16 x,I64 c){I16 *p=&x;return *p<<=c;}I64 FU16(U16 x,I64 c){U16 *p=&x;return *p<<=c;}I64 FI32(I32 x,I64 c){I32 *p=&x;return *p<<=c;}I64 FU32(U32 x,I64 c){U32 *p=&x;return *p<<=c;}POValue(\"I8\",FI8(127,65));POValue(\"U8\",FU8(255,65));POValue(\"I16\",FI16(32767,65));POValue(\"U16\",FU16(65535,65));POValue(\"I32\",FI32(2147483647,65));POValue(\"U32\",FU32(4294967295,65));}\n";
FileWrite("B:/MEMWIDTH.HC",ASrc,StrLen(ASrc));I64 AE=Cmp("B:/MEMWIDTH.HC");POValue("COMPILE_ERRORS",AE);POOut("POINTER_DONE\n");
