let header = "extern U0 PutChars(U64 ch);"

let self_placeholder =
  header
  ^ "U0 PutChars(U64 ch){static U0 (*p)(U64 \
     arg)=&PutChars;p(ch);}PutChars('A');"

let cases =
  List.map
    (fun (name, source, output) -> (name, header ^ source, output))
    [
      ("top-level capture", "U0 (*p)(U64 ch);p=&PutChars;p('AB');42;", "AB");
      ("global initializer", "U0 (*p)(U64 ch)=&PutChars;p('AB');42;", "AB");
      ( "automatic cell",
        "I64 Run(){U0 (*p)(U64 ch);p=&PutChars;p('AB');return 42;}Run();",
        "AB" );
      ( "global array copy",
        "U0 (*p)(U64 ch)[2];p[0]=&PutChars;p[1]=p[0];p[1]('AB');42;",
        "AB" );
      ( "static cell",
        "I64 Run(){static U0 (*p)(U64 ch)=&PutChars;p('AB');return \
         42;}Run();Run();",
        "ABAB" );
      ( "static array",
        "I64 Run(){static U0 (*p)(U64 ch)[2]={0,&PutChars};p[1]('AB');return \
         42;}Run();Run();",
        "ABAB" );
      ( "fixed parameter",
        "I64 Run(U0 (*p)(U64 ch)){p('AB');return 42;}Run(&PutChars);",
        "AB" );
      ( "parameter forwarding",
        "I64 Inner(U0 (*p)(U64 ch)){p('AB');return 42;}I64 Outer(U0 (*q)(U64 \
         ch)){return Inner(q);}Outer(&PutChars);",
        "AB" );
      ( "recursive forwarding",
        "I64 Run(U0 (*p)(U64 ch),I64 n){if(n)return Run(p,n-1);p('AB');return \
         42;}Run(&PutChars,3);",
        "AB" );
      ( "repeated address identity",
        "U0 (*p)(U64 ch),(*q)(U64 ch);p=&PutChars;q=&PutChars;(p==q)*42;",
        "" );
      ( "captured before arguments",
        "U0 (*p)(U64 ch);p=&PutChars;U0 Replacement(U64 ch){}U64 \
         Mark(){p=&Replacement;PutChars('M');return 'A';}p(Mark());42;",
        "MA" );
      ( "later joined definition",
        "I64 N;U0 (*p)(U64 ch),(*q)(U64 ch);p=&PutChars;U0 PutChars(U64 \
         ch){N=ch;}q=&PutChars;p('A');q(42);N;",
        "A" );
      ( "named default capture",
        "I64 Run(U0 (*p)(U64 ch)=&PutChars){p('A');return 42;}Run();Run();",
        "AA" );
      ( "default before joined definition",
        "I64 N=0;I64 Run(U0 (*p)(U64 ch)=&PutChars){p('A');return 42;}U0 \
         PutChars(U64 ch){N=ch;}Run();N;42;",
        "A" );
      ( "default from copied cell",
        "U0 (*p)(U64 ch)=&PutChars;I64 Run(U0 (*q)(U64 ch)=p){q('A');return \
         42;}p=0;Run();",
        "A" );
      ("anonymous word default", "U0 (*p)(U64 ch='AB')=&PutChars;p();42;", "AB");
      ( "packed zero and high bytes",
        "U0 (*p)(U64 ch)=&PutChars;p(0);p(0x00420041);p(0x008000FF);42;",
        "AB\255\128" );
      ( "full packed word",
        "U0 (*p)(U64 ch)=&PutChars;p(0xffffffffffffffff);42;",
        String.make 8 '\255' );
      ( "unreached mismatched call",
        "I64 Run(){I64 (*p)(U64 ch);p=&PutChars;if(0)p('A');return 42;}Run();",
        "" );
    ]

let mismatches =
  List.map
    (fun (name, declarator, argument) ->
      ( name,
        header ^ "U64 Mark(){PutChars('M');return 'A';}" ^ declarator
        ^ "p=&PutChars;p(" ^ argument ^ ");42;" ))
    [
      ("return class", "I64 (*p)(U64 ch);", "Mark()");
      ("fixed class", "U0 (*p)(I64 ch);", "Mark()");
      ("fixed count", "U0 (*p)(U64 ch,U64 other);", "Mark(),0");
      ("variadic shape", "U0 (*p)(U64 ch,...);", "Mark()");
    ]
