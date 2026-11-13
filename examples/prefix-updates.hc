I64 values[3]={40,41,42};
I64 Check() {
  I64 *p=values;
  I64 next=++*p;
  if (next!=41 || p!=&values[1] || values[0]!=40) return -1;
  I64 changed=++(*p);
  if (changed!=42 || p!=&values[1]) return -1;
  return values[1];
}
Check();
