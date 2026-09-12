#exe {
  I64 Position=6;
  I64 Next(I64 old) { return old+ ++Position; }
  class Span {
    U8 head;
    $$=Next($$) #exe { Position+=1; class Noise { $$=50; }; };
    I64 last;
  };
  StreamPrint("%d;",sizeof(Span)+Position+17);
}
