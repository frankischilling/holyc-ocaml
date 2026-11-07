I64 Reassign(I8 value,I64 next,I64 count) {
  I8 local=0;
  while (count) {
    value=next;
    local=value;
    --count;
  }
  return local;
}

I64 Stored(I8 noreg value,I64 next,I64 count) {
  I8 noreg local=0;
  while (count) {
    value=next;
    local=value;
    --count;
  }
  return local;
}

Reassign(0,257,2)-Stored(0,257,2);
