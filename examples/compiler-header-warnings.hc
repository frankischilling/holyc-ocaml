#exe {
  I64 N=40;
  extern I64 Count(I64 n=++N);
  I64 Count(I64 n=++N){return n;}
  N=0;
  Print("%d;",Count()+N);
  extern I64 Width(U8 n=554);
  I64 Width(U8 n=42){return n;}
  Print("%d;",Width());
  I64 A[2]={42,17};
  extern I64 Data(I64 *p=&A[0]);
  I64 Data(I64 *p=A){return *p;}
  Print("%d;",Data());
  I64 Identity(I64 n){return n;}
  extern I64 Callback(I64 (*cb)(I64 n)=&Identity);
  I64 Callback(I64 (*cb)(I64 n)=&Identity){return cb(42);}
  Print("%d;",Callback());
  Option(19,0);
  extern I64 Quiet(I64 before);
  U8 Quiet(U8 after){return after;}
  Option(19,1);
  extern I64 Both(I64 before);
  U8 Both(U8 after){return after;}
  Print("%d;",Both(42));
}
42;
