#exe {
  I64 N=1;
  class Span {
    $$=66+ #exe {
      I64 Marker() {
        U8 head;
        I64 values[++N];
        U8 tail;
        values[0]=20;
        values[1]=22;
        return values[0]+values[1];
      };
    } $$;
  };
  StreamPrint("%d;",sizeof(Span)+Marker()+N-44);
}
