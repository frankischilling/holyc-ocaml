U0 POOut(U8 *s) {while (*s) OutU8(0xE9,*s++);}
U0 POValue(U8 *name,I64 value) {U8 buf[80];StrPrint(buf,"%s=%d\n",name,value);POOut(buf);}
I64 ZCount=0,ZValue=7;
U0 ZVoid(){ZCount+=100;}
I64 ZFunction(){ZCount+=100;return 7;}
I64 (*ZCallback)()=&ZFunction;
I64 *ZGet(){ZCount++;return &ZValue;}
U8 ZArray[2]={7,8};
class ZBox{U0 empty;I64 value;};
ZBox ZAddress={&ZVoid,42},ZData={&ZValue,42},ZDecay={ZArray,42},ZString={"AB",42},ZReturned={ZGet(),42},ZWord={ZCallback,42};
POValue("FUNCTION_ADDRESS",ZAddress.value);
POValue("DATA_ADDRESS",ZData.value);
POValue("ARRAY_DECAY",ZDecay.value);
POValue("STRING_ADDRESS",ZString.value);
POValue("RETURNED_REFERENCE",ZReturned.value);
POValue("CALLBACK_WORD",ZWord.value);
POValue("GLOBAL_CALLS",ZCount);
I64 ZStatic(){static ZBox object={ZGet(),42};return object.value;}
POValue("STATIC_CALLS_BEFORE",ZCount);
POValue("STATIC_SUM",ZStatic()+ZStatic());
POValue("STATIC_CALLS_AFTER",ZCount);
POOut("POINTER_DONE\n");
