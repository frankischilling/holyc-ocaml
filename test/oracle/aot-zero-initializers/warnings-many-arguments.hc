U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
U8 *AWSource="#include \"/Kernel/KernelA.HH\"\nclass AWBox{U0 empty;I64 value;};\n#exe {POValue(\"START\",Fs->last_cc->warning_cnt);}\nI64 AWTHREE(I64 p0,I64 p1,I64 p2){return p0+p1+p2;}AWBox AWTHREEO={AWTHREE(1,1,5),42};\n#exe {POValue(\"THREE\",Fs->last_cc->warning_cnt);}\nI64 AWFOUR(I64 p0,I64 p1,I64 p2,I64 p3){return p0+p1+p2+p3;}AWBox AWFOURO={AWFOUR(1,1,1,4),42};\n#exe {POValue(\"FOUR\",Fs->last_cc->warning_cnt);}\nI64 AWSIX(I64 p0,I64 p1,I64 p2,I64 p3,I64 p4,I64 p5){return p0+p1+p2+p3+p4+p5;}AWBox AWSIXO={AWSIX(1,1,1,1,1,2),42};\n#exe {POValue(\"SIX\",Fs->last_cc->warning_cnt);}\nI64 AWEIGHT(I64 p0,I64 p1,I64 p2,I64 p3,I64 p4,I64 p5,I64 p6,I64 p7){return p0+p1+p2+p3+p4+p5+p6+p7;}AWBox AWEIGHTO={AWEIGHT(1,1,1,1,1,1,1,0),42};\n#exe {POValue(\"EIGHT\",Fs->last_cc->warning_cnt);}\nI64 AWDEF(I64 a=3,I64 b=2,I64 c=2){return a+b+c;}AWBox AWDEFO={AWDEF(,2,),42};\n#exe {POValue(\"DEFAULT\",Fs->last_cc->warning_cnt);}\n";
FileWrite("B:/AWMANY.HC",AWSource,StrLen(AWSource));
POValue("COMPILE_ERRORS",Cmp("B:/AWMANY.HC"));
POOut("POINTER_DONE\n");
