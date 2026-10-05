extern U0 PutChars(U64 ch);
extern U0 Print(U8 *fmt,...);
U8 Format[4]={37,100,59,0};
I64 Emit(){PutChars('A');Print(Format,42);return 42;}
Emit();
