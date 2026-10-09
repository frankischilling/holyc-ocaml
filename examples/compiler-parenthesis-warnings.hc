#exe {
  Option(16,0);
  Option(17,1);
  #define VALUE (40+2)
  I64 MacroValue(){return VALUE;}
  I64 Answer(){return (40*1)+2;}
  Print("%d;",Answer());
}
42;
