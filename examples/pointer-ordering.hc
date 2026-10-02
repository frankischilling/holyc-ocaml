extern U0 Print(U8 *fmt,...);

I64 Before(I64 *p,I64 *q) {return p<q;}

I64 Entry() {
  I64 q[3];
  I64 *p=q,*alias=q+1,*past=q+3;
  Print("%d:%d:%d:%d:%d;",Before(p,alias),alias>=p,
        past>alias,past<=q+3,p<(p=&q[1]));
  return 41+Before(q,p);
}

Entry();
