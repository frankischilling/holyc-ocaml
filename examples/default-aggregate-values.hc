class Word { U64 low; U8 guard; };
I64 F() {
  Word a[2];
  a[0].guard=7;
  a[1].guard=9;
  a[0]=20;
  a[1]=22;
  Word *p=a;
  if (sizeof(Word)!=9 || (p+1)-p!=1) return -1;
  if (a[0].guard!=7 || a[1].guard!=9) return -2;
  return *p+p[1];
}
F();
