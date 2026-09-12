#exe {
  I64 Out=0;
  extern U0 Print();
  // Empty-marker lookahead supplies the fixed argument before '(' is read.
  if (0) { ""#exe {extern U0 Print(I64 n);}(40); }
  // Closing-parenthesis lookahead joins the selected unresolved function.
  extern U0 PutChars();
  ''()#exe {U0 PutChars(){Out=42;}};
  I64 Joined=Out;
  // A resolved call keeps its executable when a fresh definition appears.
  ''()#exe {U0 PutChars(){Out=17;}};
  StreamPrint("%d;",Joined+Out-42);
}
