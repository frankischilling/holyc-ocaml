U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *AWSource="#include \"/Kernel/KernelA.HH\"\n#exe {POValue(\"START\",Fs->last_cc->warning_cnt);}\nI64 AWGet(I64 n,I64 m){return n+m;}U0 AWVoid(I64 n){}class AWBox{U0 empty;I64 value;};\nAWBox AWConstant={7,42};\n#exe {POValue(\"CONSTANT\",Fs->last_cc->warning_cnt);}\nAWBox AWInteger={AWGet(3,4),42};\n#exe {POValue(\"INTEGER\",Fs->last_cc->warning_cnt);}\nAWBox AWNoWord={AWVoid(7),42};\n#exe {POValue(\"VOID\",Fs->last_cc->warning_cnt);}\nI64 AWStatic(){static AWBox object={AWGet(3,4),42};return object.value;}\n#exe {POValue(\"STATIC\",Fs->last_cc->warning_cnt);}\n";
FileWrite("B:/AWWARN.HC",AWSource,StrLen(AWSource));
POValue("COMPILE_ERRORS",Cmp("B:/AWWARN.HC"));
POOut("POINTER_DONE\n");
