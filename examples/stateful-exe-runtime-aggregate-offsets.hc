#exe {
  I64 Position=6;
  I64 Next() { return ++Position; }
  class Span {
    U8 head;
    $$=Next() #exe { Position+=1; };
    I64 last;
  };
  I64 Saved() { return sizeof(Span); }
  StreamPrint("%d;",Saved()+sizeof(Span)+Position+2);
}
