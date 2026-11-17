I64 Accumulate(I8 value,I64 count) {
  I64 i=0;
  for (;i<count;i++) {
    value+=1;
  }
  return value;
}

I64 Stored(I8 noreg value,I64 count) {
  I64 i=0;
  while (i<count) {
    value++;
    ++i;
  }
  return value;
}

Accumulate(127,3)-Stored(127,3);
