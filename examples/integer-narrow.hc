extern U0 Print(U8 *fmt,...);
I8 Id(I8 n){return n;}
I16 Wide(){return 65578;}
I8 Signed[2]="\xFF";
I16 Parts[2]={Wide(),0};
U16 Seed=Id(42);
U32 Bump(I16 n,U16 *p){*p+=n;return *p+4294967296;}
I64 Result(){
  static I32 Calls=0;
  ++Calls;
  I8 small=Signed[0];
  I16 part=Parts[0];
  U16 before=Seed;
  U32 total=Bump(part,&Seed);
  Print("%d",total-before);
  return total-before+small+Calls;
}
Result();
