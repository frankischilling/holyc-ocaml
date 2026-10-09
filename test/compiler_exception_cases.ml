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
    ( "child",
      {|#exe {StreamExePrint("Print(\"kept\");return 42;Print(\"late\");");Print("late");}42;|},
      "kept" );
    ( "saved function child",
      {|#exe {I64 F(){return StreamExePrint("Print(\"kept\");return 42;");}F();Print("late");}42;|},
      "kept" );
  ]

let inherited_function_failure =
  {|I64 F(){#exe {Print("kept");StreamExePrint("return 42;");Print("late");}return 42;}F();|}

let ordinary_failures =
  [
    ("grammar", "I64 X=;", "HCPARSE");
    ("binding", "Unknown;", "HCRUN");
    ("runtime", "1/0;", "HCIRVM");
  ]
