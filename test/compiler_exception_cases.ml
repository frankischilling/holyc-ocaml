let headers =
  {|extern U0 Print(U8 *fmt,...);extern I64 StreamExePrint(U8 *fmt,...);|}

let failures =
  (* Missing-function cases remain distinct from inherited-function rejection. *)
  [
    ("bare root", "return;", "");
    ("value root", "return 42;", "");
    ("before expression lookahead", {|return #exe {Print("late");}42;|}, "");
    ("block root", "{return 42;}", "");
    ("loop root", "while(1)return 42;", "");
    ("directive", {|#exe {Print("kept");return 42;Print("late");}42;|}, "kept");
  ]

let caught_children =
  [
    ( "child",
      {|#exe {StreamExePrint("Print(\"kept\");return 42;Print(\"skipped\");");Print("after");}42;|},
      "keptafter",
      1 );
    ( "saved function child",
      {|#exe {I64 F(){return StreamExePrint("Print(\"kept\");return 42;");}F();Print("after");}42;|},
      "keptafter",
      1 );
    ( "nested directive child",
      {|#exe {StreamExePrint("#exe {Print(\"kept\");return @}Print(\"skipped\");");Print("after");}42;|},
      "keptafter",
      1 );
    ( "interrupted child initializer",
      {|#exe {StreamExePrint("I64 Lost=#exe {Print(\"kept\");return @}42;Print(\"skipped\");");Print("after");}42;|},
      "keptafter",
      1 );
    ( "parent initializer",
      {|#exe {I64 V=StreamExePrint("Print(\"kept\");return 42;");Print("%d;after",V);}42;|},
      "kept0;after",
      1 );
    ( "parent dimension",
      {|#exe {I64 A[StreamExePrint("Print(\"kept\");return 42;")+1];Print("%d;after",sizeof(A));}42;|},
      "kept8;after",
      1 );
    ( "parent default",
      {|#exe {I64 G(){return StreamExePrint("Print(\"kept\");return 42;");}I64 F(I64 n=G()){return n;}Print("%d;after",F());}42;|},
      "kept0;after",
      1 );
    ( "next child",
      {|#exe {StreamExePrint("Print(\"kept\");return 42;");StreamExePrint("I64 Good=42;");Print("after");}42;|},
      "keptafter",
      1 );
    ( "successive catches",
      {|#exe {StreamExePrint("Print(\"a\");return 1;");StreamExePrint("Print(\"b\");return 2;");Print("after");}42;|},
      "abafter",
      2 );
    ( "reached child declaration",
      {|#exe {StreamExePrint("I64 Good=40;Print(\"kept\");return 1;");StreamExePrint("Print(\"%%d;\",Good+2);");Print("after");}42;|},
      "kept42;after",
      1 );
    ( "inherited function directive",
      {|I64 F(){#exe {StreamExePrint("#exe {Print(\"kept\");return @}Print(\"skipped\");");Print("after");}return 42;}F();|},
      "keptafter",
      1 );
    ( "interrupted child binding",
      {|I64 F(I64 n){return n;}#exe {StreamExePrint("I64 (*Lost)(I64)=#exe {Print(\"kept\");return @}F;");Print("after");}42;|},
      "keptafter",
      1 );
  ]

let inherited_function_failure =
  {|I64 F(){#exe {Print("kept");StreamExePrint("return 42;");Print("late");}return 42;}F();|}

let faults_after_caught_children =
  [
    ( "uninitialized child remains unavailable",
      {|#exe {StreamExePrint("I64 Lost=#exe {Print(\"kept\");return @}42;");StreamExePrint("Lost;Print(\"skipped\");");Print("skipped");}42;|}
    );
    ( "incomplete child callback remains unavailable",
      {|I64 F(I64 n){return n;}#exe {StreamExePrint("I64 (*Lost)(I64)=#exe {Print(\"kept\");return @}F;");StreamExePrint("Lost(42);Print(\"skipped\");");Print("skipped");}42;|}
    );
    ( "later runtime fault",
      {|#exe {StreamExePrint("Print(\"kept\");return 42;");1/0;Print("skipped");}42;|}
    );
  ]

let ordinary_failures =
  [
    ("grammar", "I64 X=;", "HCPARSE");
    ("binding", "Unknown;", "HCRUN");
    ("runtime", "1/0;", "HCIRVM");
  ]
