U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *AWSource="#include \"/Kernel/KernelA.HH\"\nclass AWBox{U0 empty;I64 value;};\n#exe {POValue(\"START\",Fs->last_cc->warning_cnt);}\nI64 AWBOTH(I64 n=3,I64 m=4){return n+m;}AWBox AWBOTHO={AWBOTH(),42};\n#exe {POValue(\"BOTH\",Fs->last_cc->warning_cnt);}\nI64 AWLAST(I64 n,I64 m=4){return n+m;}AWBox AWLASTO={AWLAST(3),42};\n#exe {POValue(\"LAST\",Fs->last_cc->warning_cnt);}\nI64 AWFIRST(I64 n=3,I64 m=4){return n+m;}AWBox AWFIRSTO={AWFIRST(,4),42};\n#exe {POValue(\"FIRST\",Fs->last_cc->warning_cnt);}\nI64 AWTRAIL(I64 n=3,I64 m=4){return n+m;}AWBox AWTRAILO={AWTRAIL(3,),42};\n#exe {POValue(\"TRAIL\",Fs->last_cc->warning_cnt);}\nI64 AWFULL(I64 n=3,I64 m=4){return n+m;}AWBox AWFULLO={AWFULL(3,4),42};\n#exe {POValue(\"FULL\",Fs->last_cc->warning_cnt);}\n";
FileWrite("B:/AW2DEF.HC",AWSource,StrLen(AWSource));
POValue("COMPILE_ERRORS",Cmp("B:/AW2DEF.HC"));
POOut("POINTER_DONE\n");
