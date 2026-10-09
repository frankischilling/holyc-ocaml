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

(* Literal statement phases audited against Compiler/PrsStmt.HC. The marker is
   the original current token at LexExcept, which follows Lex for invalid break. *)
let statement_failures =
  [
    ("if opening", "if 1;", "HCPARSE0052", "1", "");
    ("if closing", "if(1;", "HCPARSE0053", ";", "");
    ("while opening", "while 1;", "HCPARSE0058", "1", "");
    ("while closing", "while(1;", "HCPARSE0059", ";", "");
    ("do missing while", "do;42;", "HCPARSE0063", "42", "");
    ("do opening", "do;while 1;", "HCPARSE0064", "1", "");
    ("do closing", "do;while(1;", "HCPARSE0065", ";", "");
    ("do semicolon", "do;while(0)42;", "HCPARSE0066", "42", "");
    ("for opening", "for 1;", "HCPARSE0067", "1", "");
    ("for condition semicolon", "for(;1)42;", "HCPARSE0069", ")", "");
    ("for closing", "for(;1;42;", "HCPARSE0070", ";", "");
    ("switch opening", "switch 1;", "HCPARSE0085", "1", "");
    ("switch closing", "switch(1;", "HCPARSE0086", ";", "");
    ("nobound closing", "switch[1;", "HCPARSE0086", ";", "");
    ("switch brace", "switch(1);", "HCPARSE0087", ";", "");
    ("goto identifier", "goto;", "HCPARSE0075", ";", "");
    ("goto semicolon", "goto Done}", "HCPARSE0076", "}", "");
    ("expression semicolon", "42}", "HCPARSE0047", "}", "");
    ( "output semicolon",
      "extern U0 PutChars(U64 ch);'A'}",
      "HCPARSE0046",
      "}",
      "" );
    ("return semicolon", "I64 F(){return 42}", "HCPARSE0073", "}", "");
    ("root break", "break;", "HCPARSE0170", ";", "");
    ("break semicolon", "while(1)break }", "HCPARSE0072", "}", "");
    ("break for initializer", "while(1)for(break;1;);", "HCPARSE0170", ";", "");
    ("break for update", "while(1)for(;1;break);", "HCPARSE0170", ")", "");
    ("break lock", "while(1)lock break;", "HCPARSE0170", ";", "");
    ("try missing headers", "try;", "HCPARSE0171", ";", "");
    ("try one header", "extern U0 SysTry();try;", "HCPARSE0171", ";", "");
    ( "try wrong header kinds",
      "I64 SysTry=0;I64 SysUntry=0;try;",
      "HCPARSE0171",
      ";",
      "" );
    ( "try missing catch",
      "extern U0 SysTry(I64 a,I64 b);extern U0 SysUntry();try;42;",
      "HCPARSE0080",
      "42",
      "" );
    ( "break try",
      "extern U0 SysTry(I64 a,I64 b);extern U0 SysUntry();while(1)try \
       break;catch;",
      "HCPARSE0170",
      ";",
      "" );
    ( "break catch",
      "extern U0 SysTry(I64 a,I64 b);extern U0 SysUntry();while(1)try;catch \
       break;",
      "HCPARSE0170",
      ";",
      "" );
    ( "break after directive",
      {|break #exe {Print("kept");} ;|},
      "HCPARSE0170",
      ";",
      "kept" );
    ( "first directive producer",
      {|break #exe {return @} ;|},
      "HCPARSE0168",
      "return",
      "" );
  ]

let statement_caught_children =
  List.map
    (fun (label, text, code, marker, output) ->
      ( label,
        Printf.sprintf "#exe {StreamExePrint(%S);Print(\"after\");}42;" text,
        code,
        marker,
        output ^ "after" ))
    statement_failures
  @ [
      ( "child does not inherit caller loop target",
        {|#exe {while(1){StreamExePrint("break;");break;}Print("after");}42;|},
        "HCPARSE0170",
        ";",
        "after" );
    ]

(* PrsExp.HC:380-560 selects HTT_FUN and captures fixed argument metadata before
   traversal. These cases reach only the audited delimiter/header producers. *)
let call_failures =
  [
    ("missing Print header", "\"text\";", "HCPARSE0172", "\"text\"");
    ("missing PutChars header", "'A';", "HCPARSE0172", "'A'");
    ("wrong Print kind", "I64 Print=0;\"text\";", "HCPARSE0172", "\"text\"");
    ("empty marker before Lex", "\"\" #error late\n42;", "HCPARSE0172", "\"\"");
    ("fixed comma", "I64 F(I64 a,I64 b){return a+b;}F(1 2);", "HCPARSE0024", "2");
    ("fixed close", "I64 F(I64 a){return a;}F(1;", "HCPARSE0025", ";");
    ("zero fixed close", "I64 F(){return 0;}F(1);", "HCPARSE0025", "1");
    ("variadic comma", "I64 F(I64 a,...){return a;}F(1 2);", "HCPARSE0024", "2");
    ( "zero fixed variadic close",
      "I64 F(...){return 0;}F(1 2);",
      "HCPARSE0025",
      "2" );
    ( "Print variadic comma",
      "extern U0 Print(U8 *fmt,...);\"text\"}",
      "HCPARSE0167",
      "}" );
    ( "Print fixed comma",
      "extern U0 Print(U8 *fmt,I64 n);\"text\" 42;",
      "HCPARSE0167",
      "42" );
    ( "PutChars parenthesized close",
      "extern U0 PutChars(I64 n);''(42;",
      "HCPARSE0167",
      ";" );
    ( "zero fixed Print marker",
      "extern U0 Print();\"text\";42;",
      "HCPARSE0046",
      "\"text\"" );
    ( "zero fixed PutChars marker",
      "extern U0 PutChars();'A';42;",
      "HCPARSE0046",
      "'A'" );
    ( "zero fixed variadic PutChars marker",
      "extern U0 PutChars(...);'A';42;",
      "HCPARSE0046",
      "'A'" );
    ( "zero fixed Print first literal",
      "extern U0 Print();\"first\" \"second\";42;",
      "HCPARSE0046",
      "\"first\"" );
    ( "zero fixed Print before Lex",
      "extern U0 Print();\"text\" #error late\n42;",
      "HCPARSE0046",
      "\"text\"" );
    ( "Print initial default",
      "extern U0 Print(I64 n=0);\"text\";",
      "HCPARSE0046",
      "\"text\"" );
    ( "PutChars initial default",
      "extern U0 PutChars(I64 n=65);'A';",
      "HCPARSE0046",
      "'A'" );
    ( "Print default then fixed",
      "extern U0 Print(I64 n=0,I64 m=1);\"text\";",
      "HCPARSE0167",
      "\"text\"" );
    ( "Print default then variadic",
      "extern U0 Print(I64 n=0,...);\"text\";",
      "HCPARSE0167",
      "\"text\"" );
    ( "Print later default",
      "extern U0 Print(U8 *s,I64 n=7);\"text\",42;",
      "HCPARSE0046",
      "42" );
    ( "Print later default then variadic",
      "extern U0 Print(U8 *s,I64 n=7,...);\"text\",42;",
      "HCPARSE0167",
      "42" );
    ( "Print later default then required",
      "extern U0 Print(U8 *s,I64 n=7,I64 m);\"text\",42;",
      "HCPARSE0167",
      "42" );
    ( "Print empty marker default",
      "extern U0 Print(I64 n=0);\"\"42;",
      "HCPARSE0046",
      "42" );
    ( "Print default first literal",
      "extern U0 Print(I64 n=0);\"first\" \"second\" #error late\n",
      "HCPARSE0046",
      "\"first\"" );
    ( "PutChars all defaults before Lex",
      "extern U0 PutChars(I64 a=40,I64 b=2);'A' #error late\n",
      "HCPARSE0046",
      "'A'" );
    ("Print empty zero fixed", "extern U0 Print();\"\"42;", "HCPARSE0046", "42");
    ( "zero fixed Print body",
      "U0 Print(){}\"text\";42;",
      "HCPARSE0046",
      "\"text\"" );
  ]

let call_caught_children =
  List.map
    (fun (label, text, code, marker) ->
      let tail, output =
        if
          String.starts_with ~prefix:"zero fixed PutChars" label
          || String.starts_with ~prefix:"zero fixed variadic PutChars" label
          || String.starts_with ~prefix:"PutChars " label
          || label = "PutChars parenthesized close"
        then ("Print(\"after\");", "after")
        else ("PutChars('A');", "A")
      in
      ( label,
        true,
        Printf.sprintf "#exe {StreamExePrint(%S);%s}42;" text tail,
        code,
        marker,
        output ))
    (List.filter (fun (_, _, code, _) -> code <> "HCPARSE0172") call_failures)
  @ [
      ( "caught missing Print function",
        false,
        {|I64 Print=0;#exe {StreamExePrint("\"text\";");}42;|},
        "HCPARSE0172",
        "\"text\"",
        "" );
      ( "caught missing PutChars function",
        false,
        {|I64 PutChars=0;#exe {StreamExePrint("'A';");}42;|},
        "HCPARSE0172",
        "'A'",
        "" );
      ( "nested directive fixed comma",
        true,
        {|#exe {StreamExePrint("#exe {I64 F(I64 a,I64 b){return a+b;}F(1 2);}Print(\"skipped\");");Print("after");}42;|},
        "HCPARSE0024",
        "2",
        "after" );
      ( "nested directive implicit comma",
        true,
        {|#exe {StreamExePrint("#exe {\"text\"}Print(\"skipped\");");Print("after");}42;|},
        "HCPARSE0167",
        "}",
        "after" );
    ]

let call_successes =
  [
    ( "Print initial saved default",
      {|#exe {I64 Out=0;U0 Print(I64 n=41){Out=n+1;}"";StreamPrint("%d;",Out);}|},
      "" );
    ( "PutChars initial saved default",
      {|#exe {I64 Out=0;U0 PutChars(I64 n=41){Out=n+1;}'';StreamPrint("%d;",Out);}|},
      "" );
    ( "fixed Print comma is a statement",
      {|#exe {I64 Out=0;U0 Print(U8 *s){Out=40;}"text",2;StreamPrint("%d;",Out+2);}|},
      "" );
    ( "Print omitted tail leaves statement comma",
      {|#exe {I64 Out=0;U0 Print(U8 *s,I64 n=40){Out=n;}"text",,2;StreamPrint("%d;",Out+2);}|},
      "" );
    ( "PutChars marker reaches later required formal",
      {|#exe {I64 Out=0;U0 PutChars(I64 a=40,I64 b){Out=a+b;}'B'-64;StreamPrint("%d;",Out);}|},
      "" );
    ( "PutChars all defaults leave statement comma",
      {|#exe {I64 Out=0;U0 PutChars(I64 a=40,I64 b=0){Out=a+b;}'',2;StreamPrint("%d;",Out+2);}|},
      "" );
  ]
