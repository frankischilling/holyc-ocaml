U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *AWSource="#include \"/Kernel/KernelA.HH\"\nI64 AWN=7;class AWBox{U0 empty;I64 value;};I64 AWGet(I64 n){return n;}\n#exe {POValue(\"START\",Fs->last_cc->warning_cnt);}\nAWBox AWADDO={AWGet(AWN+0),42};\n#exe {POValue(\"ADD\",Fs->last_cc->warning_cnt);}\nAWBox AWSUBO={AWGet(AWN-0),42};\n#exe {POValue(\"SUB\",Fs->last_cc->warning_cnt);}\nAWBox AWMULO={AWGet(AWN*1),42};\n#exe {POValue(\"MUL\",Fs->last_cc->warning_cnt);}\nAWBox AWANDO={AWGet(AWN&7),42};\n#exe {POValue(\"AND\",Fs->last_cc->warning_cnt);}\nAWBox AWORO={AWGet(AWN|0),42};\n#exe {POValue(\"OR\",Fs->last_cc->warning_cnt);}\nAWBox AWXORO={AWGet(AWN^0),42};\n#exe {POValue(\"XOR\",Fs->last_cc->warning_cnt);}\nAWBox AWSHLO={AWGet(AWN<<1),42};\n#exe {POValue(\"SHL\",Fs->last_cc->warning_cnt);}\nAWBox AWSHRO={AWGet(AWN>>1),42};\n#exe {POValue(\"SHR\",Fs->last_cc->warning_cnt);}\nAWBox AWNEGO={AWGet(-AWN),42};\n#exe {POValue(\"NEG\",Fs->last_cc->warning_cnt);}\n";
FileWrite("B:/AWARITH.HC",AWSource,StrLen(AWSource));
POValue("COMPILE_ERRORS",Cmp("B:/AWARITH.HC"));
POOut("POINTER_DONE\n");
