#exe {
  Option(16,0);
  I64 Selected(I8 value,I64 step) {
    if (step) value+=step;
    return value;
  }
  Option(34,1);
  I64 Stored(I8 value,I64 step) {
    if (step) value+=step;
    return value;
  }
  Option(34,0);
  StreamPrint("%d;",Selected(127,2)-Stored(127,2));
}
