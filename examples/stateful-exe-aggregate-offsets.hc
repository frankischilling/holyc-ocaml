#exe {
  I64 Seen=0;
  class Span {
    U8 first;
    $$=$$+7;
    #exe {Seen=sizeof(Span);}
    I64 last;
  };
  StreamPrint("%d;",Seen+sizeof(Span)+18);
}
