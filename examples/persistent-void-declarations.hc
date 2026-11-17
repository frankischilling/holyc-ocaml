I64 calls=0;
U0 Effect() { calls++; }
U0 global=Effect();
I64 Result() {
  static U0 local=Effect();
  return calls+sizeof(global)+sizeof(local)+40;
}
Result();
