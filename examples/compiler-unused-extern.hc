// Unused extern joins precede parameter input and ignore the header-warning mask.
#exe {
  Option(19,0);
  extern I64 OwnedExternUnused();
  extern I64 OwnedExternUnused();
  Print("42;");
}
42;
