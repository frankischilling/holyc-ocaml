U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *AWSource="#include \"/Kernel/KernelA.HH\"\nI64 AWN=7,AWM=1;class AWBox{U0 empty;I64 value;};I64 AWGet(I64 n){return n;}\n#exe {POValue(\"START\",Fs->last_cc->warning_cnt);}\nAWBox AWNEST1O={AWGet((AWN+1)*2),42};\n#exe {POValue(\"NEST1\",Fs->last_cc->warning_cnt);}\nAWBox AWNEST2O={AWGet((3+4)*(AWN-6)),42};\n#exe {POValue(\"NEST2\",Fs->last_cc->warning_cnt);}\nAWBox AWNEST3O={AWGet(-(-AWN)),42};\n#exe {POValue(\"NEST3\",Fs->last_cc->warning_cnt);}\nAWBox AWNEST4O={AWGet((AWN|AWM)&7),42};\n#exe {POValue(\"NEST4\",Fs->last_cc->warning_cnt);}\nAWBox AWNEST5O={AWGet((AWN+1)<<1),42};\n#exe {POValue(\"NEST5\",Fs->last_cc->warning_cnt);}\n";
FileWrite("B:/AWNEST.HC",AWSource,StrLen(AWSource));
POValue("COMPILE_ERRORS",Cmp("B:/AWNEST.HC"));
POOut("POINTER_DONE\n");
