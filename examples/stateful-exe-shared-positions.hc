#exe {
  I64 N=7;
  class Span {
    U8 head;
    $$=N+$$+ #exe { class Nested { $$=N+1; }; } $$;
    I64 last;
  };
  StreamPrint("%d;",sizeof(Span)+18);
}
