extern U0 Print(U8 *fmt,...);
I64 Counter=0;
I64 Values[2]={17,40};
I64 Read(I64 *p=&Values[++Counter]){return ++*p;}
I64 Emit(U8 *s="AB"){Print("%s",s);return Values[1]+Counter-1;}
Read();
Read();
Emit();
