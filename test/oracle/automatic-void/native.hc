U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 AVCalls=0,AVReached=0,AVResult=-1;
I64 AVNext(){AVCalls++;return 42;}
U0 AVVoid(){AVCalls++;}
ExePrint("I64 AVPlain(){U0 x;return sizeof(x)+42;}AVResult=AVPlain();AVReached=1;");
POValue("PLAIN_REACHED",AVReached);POValue("PLAIN_RESULT",AVResult);
AVReached=0;AVResult=-1;
ExePrint("I64 AVInteger(){U0 x=AVNext();return AVCalls;}AVResult=AVInteger();AVReached=1;");
POValue("INTEGER_REACHED",AVReached);POValue("INTEGER_RESULT",AVResult);POValue("INTEGER_CALLS",AVCalls);
AVReached=0;AVResult=-1;AVCalls=0;
ExePrint("I64 AVEffect(){U0 x=AVVoid();return AVCalls;}AVResult=AVEffect();AVReached=1;");
POValue("VOID_REACHED",AVReached);POValue("VOID_RESULT",AVResult);POValue("VOID_CALLS",AVCalls);
POOut("POINTER_DONE\n");
