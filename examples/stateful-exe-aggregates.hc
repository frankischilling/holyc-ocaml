#exe {
  class Pair { I64 first; I64 second; };
  class Sized { U8 bytes[sizeof(Pair)+0]; };
  I64 SavedSize() { return sizeof(Sized); };
}
#exe {
  class Pair { U8 small; };
  StreamPrint("%d;",SavedSize()+sizeof(Pair)+25);
}
