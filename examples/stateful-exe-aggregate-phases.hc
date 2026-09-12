#exe {
  I64 Seen=0;
  class Pair #exe {Seen=sizeof(Pair);} {
    I64 first;
    #exe {Seen+=sizeof(Pair);}
    I64 second;
  } #exe {Seen+=sizeof(Pair);} ;
  StreamPrint("%d;",Seen+18);
}
