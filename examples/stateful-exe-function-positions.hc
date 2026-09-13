#exe {
  I64 Bias=1;
  class Span {
    U8 head;
    $$=Bias+$$+ #exe {
      I64 Marker(I64 a,I64 b,) { return a+b; };
    } $$;
    I64 tail;
  };
  StreamPrint("%d;",sizeof(Span)+Marker(7,9));
}
