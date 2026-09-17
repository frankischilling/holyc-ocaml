// TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
// Compiler/PrsVar.HC:132-134,631-656 and Compiler/PrsExp.HC:455-468,530-588.
// Native scalar functions keep eight-byte call slots while declared storage
// narrows at parameter/local entry. Numeric returns keep their full register bits.
I8 KeepI8(I8 n){I8 saved=n;return saved;}
U8 KeepU8(U8 n){U8 saved=n;return saved;}
I16 KeepI16(I16 n){I16 saved=n;return saved;}
U16 KeepU16(U16 n){U16 saved=n;return saved;}
I32 KeepI32(I32 n){I32 saved=n;return saved;}
U32 KeepU32(U32 n){U32 saved=n;return saved;}
I64 KeepI64(I64 n){I64 saved=n;return saved;}
U64 KeepU64(U64 n){U64 saved=n;return saved;}

I8 WideI8(){return 298;}
U16 WideU16(){return 65578;}

I64 Adjacent()
{
  I8 a=1;
  U8 b=40;
  I16 c=2;
  U16 d=1;
  I32 e=3;
  U32 f=1;
  a=-1;
  c=-1;
  e=-1;
  return b+d+f;
}

I64 Updates()
{
  U8 assigned=0;
  I64 assignment_result=(assigned=298);
  U8 compound=250;
  I64 compound_result=(compound+=48);
  I8 step=127;
  I64 prefix=++step;
  I64 postfix=step++;
  return (assignment_result==298)+(assigned==42)+(compound_result==298)+
         (compound==42)+(prefix==-128)+(postfix==-128)+(step==-127);
}

I64 Defaults(I64 left=40,U64 right=2)
{
  I8 saved=KeepI8(left+right);
  return saved;
}

(KeepI8(255)==-1)+
(KeepU8(255)==255)+
(KeepI16(65535)==-1)+
(KeepU16(65535)==65535)+
(KeepI32(4294967295)==-1)+
(KeepU32(4294967295)==4294967295)+
(KeepI64(-1)==-1)+
(KeepU64(0xffffffffffffffff)==0xffffffffffffffff)+
(WideI8()==298)+
(WideU16()==65578)+
(Adjacent()==42)+
(Updates()==7)+Defaults()-12;
