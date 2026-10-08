#exe {
  Print("%d;",GetOption(33));
  Print("%d;",Option(33,1));
  Print("%d;",StreamExePrint("Print(\"%%d;\",GetOption(33));Print(\"%%d;\",Option(33,0));Print(\"%%d;\",GetOption(33));42;"));
  Print("%d;",GetOption(33));
}
42;
