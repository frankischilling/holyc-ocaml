extern U0 Print(U8 *fmt,...);
U8 Id(U8 n){return n;}
U8 Wide(){return 554;}
U8 Seed=Id(552);
U8 Text[3]="41";
U8 Bump(U8 n,U8 *p){(*p)++;return n+512;}
I64 Result(){
  static U8 Calls=0;
  ++Calls;
  U8 n=Id(Wide());
  U8 result=Bump(Seed+2,&Text[1]);
  Print("%s",Text);
  return result+n-42+Calls-1;
}
Result();
