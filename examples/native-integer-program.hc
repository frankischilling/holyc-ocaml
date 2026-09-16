// TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
// PrsStmt.HC:459-565 and OptLib.HC:229-484.
// Closed structured control flow retains I64 42 as its last reached expression.
if (0 && (1/0)) 1/0;
if (1 || (1/0)) {
  for (6; 1; 1/0) {
    do {
      if (84/2==42) {
        42;
        break;
      }
      1/0;
    } while (1);
    break;
  }
} else 1/0;
