extern U0 Print(U8 *fmt,...);

I64 ReadNext(I64 *p){return *(p+1);}

I64 Entry(){
  I64 q[3];q[0]=20;q[1]=42;q[2]=8;
  I64 *p=q,*saved=p+1;
  p=p+2;
  p=p+(-1);
  U8 *bytes="AB";
  bytes=bytes+1;
  Print("%d:%d:%d:%s;",*saved,ReadNext(q),*p,bytes);
  return *saved;
}

Entry();
