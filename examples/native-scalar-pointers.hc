I64 Global=20;
U0 Add(I8 *p,I64 amount=1) { *p+=amount; }
I64 Run() {
  static I8 saved=19;
  I8 local;
  I8 *p=&local;
  *p=1;
  Global++;
  Add(&saved);
  Add(p);
  return Global+saved+local-1;
}
Run();
