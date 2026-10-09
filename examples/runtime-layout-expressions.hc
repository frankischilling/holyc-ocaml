extern U0 Print(U8 *s,...);
I64 Next(){Print("dim");return 2;}
I64 Offset(){Print("off");return 16;}
I64 A[Next()]={40,2};
class C{U8 a;$$=Offset();I64 b;};
A[0]+A[1];
