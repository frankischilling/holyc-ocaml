extern U0 Print(U8 *fmt,...);

U8 Format[6]="%0*tX";
U8 Text[3]="OK";

I64 Width()
{
  return 8;
}

Print(Format,Width(),0x10000002A);
Print("|%-5s|%05d|%u\n",Text,-42,-1);
42;
