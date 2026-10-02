extern U0 Print(U8 *fmt,...);

I64 Same(I64 *p,I64 *q) {return p==q;}

I64 Entry() {
  I64 q[3];
  I64 *p=q,*alias=&q[0],*past=q+3;
  U8 *bytes="AB";
  I64 equal=Same(p,alias),different=p!=&q[1];
  Print("%d:%d:%d:%d:%d;",equal,p==&q[1],different,
        past==q+3,bytes+1==&bytes[1]);
  return 41+Same(p,alias);
}

Entry();
