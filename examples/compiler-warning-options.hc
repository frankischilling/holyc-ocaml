#exe {
  Option(16,0);
  I64 Quiet(I64 unused) {return 42;}
  Print("%d;",Quiet(0));
  Option(16,1);
  I64 Loud(I64 unused) {return 42;}
  Print("%d;",Loud(0));
  Option(16,0);
  I64 Suppression(I64 used,I64 _anon_) {
    no_warn used;
    used;
    no_warn _anon_;
    _anon_;
    return 42;
  }
  Print("%d;",Suppression(0,0));
  StreamExePrint("Option(16,1);I64 Child(I64 unused){return 42;}Child(0);");
  I64 Parent(I64 unused) {return 42;}
  Print("%d;",Parent(0));
}
42;
