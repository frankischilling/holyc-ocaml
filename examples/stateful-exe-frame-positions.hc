#exe {
  class Span {
    $$=58+ #exe {
      I64 Marker() {
        U8 a;
        U16 b;
        I32 c;
        I64 d;
        U8 e;
        return 0;
      };
    } $$;
  };
  StreamPrint("%d;",sizeof(Span)+Marker());
}
