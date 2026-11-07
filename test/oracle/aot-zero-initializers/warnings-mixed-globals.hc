U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *AWSource="#include \"/Kernel/KernelA.HH\"\nI64 AWN=7;U8 AWU=1;I8 AWS=7;U64 AWW=1;Bool AWB=1;U16 AWT=1;class AWBox{U0 empty;I64 value;};I64 AWGet(I64 n){return n;}\n#exe {POValue(\"START\",Fs->last_cc->warning_cnt);}\nAWBox AWMIX1O={AWGet(AWN+AWU),42};\n#exe {POValue(\"MIX1\",Fs->last_cc->warning_cnt);}\nAWBox AWMIX2O={AWGet(AWU+AWN),42};\n#exe {POValue(\"MIX2\",Fs->last_cc->warning_cnt);}\nAWBox AWMIX3O={AWGet(AWS+AWW),42};\n#exe {POValue(\"MIX3\",Fs->last_cc->warning_cnt);}\nAWBox AWMIX4O={AWGet(AWB+AWT),42};\n#exe {POValue(\"MIX4\",Fs->last_cc->warning_cnt);}\nAWBox AWSHIFTO={AWGet(AWU<<AWN),42};\n#exe {POValue(\"SHIFT\",Fs->last_cc->warning_cnt);}\n";
FileWrite("B:/AWMIX.HC",AWSource,StrLen(AWSource));
POValue("COMPILE_ERRORS",Cmp("B:/AWMIX.HC"));
POOut("POINTER_DONE\n");
