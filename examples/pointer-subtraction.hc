extern U0 Print(U8 *fmt,...);

I64 ReadPrevious(I64 *p){return *(p-1);}

I64 Entry(){
  I64 q[3];q[0]=42;q[1]=8;q[2]=20;
  I64 *p=&q[2],*saved=p-2;
  p=p-1;
  U8 *bytes="AB";
  bytes=bytes+1;
  Print("%d:%d:%d:%s;",*saved,ReadPrevious(p),*(p-1),bytes-1);
  return *saved;
}

Entry();
