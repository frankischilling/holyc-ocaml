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

let print_header = "extern U0 Print(U8 *fmt,...);"
let print_capture = "U0 (*p)(U8 *fmt,...)=&Print;"

let print_cases =
  List.map
    (fun (name, source, output) -> (name, print_header ^ source, output))
    [
      ( "format and string tails",
        print_capture ^ "p(\"A%d;%s\",42,\"B\");42;",
        "A42;B" );
      ("empty tail", print_capture ^ "p(\"\");42;", "");
      ( "dynamic widths",
        print_capture ^ "p(\"%*s;%d\",4,\"A\",42);42;",
        "   A;42" );
      ( "numeric kinds",
        print_capture ^ "p(\"%08X|%u|%b\",42,42,42);42;",
        "0000002A|42|101010" );
      ("packed byte kind", print_capture ^ "p(\"%c\",0x00420041);42;", "A");
      ("quoted bytes", print_capture ^ "p(\"%Q\",\"A\\n\");42;", "A\\n");
      ( "list selector",
        print_capture ^ "p(\"%z\",1,\"Red\\0Blue\\0\");42;",
        "Blue" );
      ("unused scalar pointer", print_capture ^ "I64 A=1;p(\"X\",&A);42;", "X");
      ( "ordinary pointer callback parameter",
        "I64 Read(I64 *a){return a[0];}I64 A[1]={42};I64 (*p)(I64 \
         *a)=&Read;p(A);",
        "" );
      ( "mutable format",
        print_capture ^ "U8 F[3]={37,100,0};p(F,42);F[1]=117;p(F,42);42;",
        "4242" );
      ( "automatic capture",
        "I64 Run(){U0 (*p)(U8 *fmt,...);p=&Print;p(\"%s\",\"A\");return \
         42;}Run();",
        "A" );
      ( "fixed callback parameter",
        "I64 Run(U0 (*p)(U8 *fmt,...)){p(\"A%d\",42);return 42;}Run(&Print);",
        "A42" );
      ( "pointer tail before provider publication",
        "I64 Run(U0 (*p)(U8 *fmt,...)){p(\"%*s\",3,\"A\");return \
         42;}Run(&Print);",
        "  A" );
      ( "recursive callback",
        "I64 Run(U0 (*p)(U8 *fmt,...),I64 n){if(n)return \
         Run(p,n-1);p(\"%d\",42);return 42;}Run(&Print,3);",
        "42" );
      ( "global indexed copy",
        "U0 (*p)(U8 *fmt,...)[2];p[0]=&Print;p[1]=p[0];p[1](\"A%d\",42);42;",
        "A42" );
      ( "static indexed capture",
        "I64 Run(){static U0 (*p)(U8 \
         *fmt,...)[2]={0,&Print};p[1](\"%s\",\"A\");return 42;}Run();Run();",
        "AA" );
      ( "named saved default",
        "I64 Run(U0 (*p)(U8 *fmt,...)=&Print){p(\"A%d\",42);return \
         42;}Run();Run();",
        "A42A42" );
      ( "saved copied default",
        print_capture
        ^ "I64 Run(U0 (*q)(U8 *fmt,...)=p){q(\"%s\",\"A\");return \
           42;}p=0;Run();",
        "A" );
      ( "saved default after replacement",
        "I64 N=0;I64 Run(U0 (*p)(U8 *fmt,...)=&Print){p(\"A%d\",42);return \
         42;}U0 Print(U8 *fmt,...){N=1;}Run();N+42;",
        "A42" );
      ( "old and new captures",
        "I64 N=0;U0 (*p)(U8 *fmt,...),(*q)(U8 *fmt,...);p=&Print;U0 Print(U8 \
         *fmt,...){N=1;}q=&Print;p(\"A%d\",42);q(\"ignored\",42);N+41;",
        "A42" );
      ( "capture before argument effects",
        "extern U0 PutChars(U64 ch);" ^ print_capture
        ^ "U0 Other(U8 *fmt,...){}I64 Mark(){p=&Other;PutChars('M');return \
           42;}p(\"%d\",Mark());42;",
        "M42" );
      ( "repeated provider identity",
        "U0 (*p)(U8 *fmt,...),(*q)(U8 *fmt,...);p=&Print;q=&Print;(p==q)*42;",
        "" );
    ]

let print_mismatches =
  List.map
    (fun (name, declarator, arguments) ->
      ( name,
        print_header
        ^ "extern U0 PutChars(U64 ch);I64 Mark(){PutChars('M');return 0;}"
        ^ declarator ^ "p=&Print;p(" ^ arguments ^ ");42;" ))
    [
      ("return class", "I64 (*p)(U8 *fmt,...);", "\"X\",Mark()");
      ( "return mismatch with pointer tail",
        "I64 (*p)(U8 *fmt,...);",
        "\"X\",Mark(),\"Y\"" );
      ("fixed class", "I8 F[2]={88,0};U0 (*p)(I8 *fmt,...);", "F,Mark()");
      ("fixed count", "U0 (*p)(U8 *fmt,I64 n,...);", "\"X\",Mark()");
      ("variadic shape", "U0 (*p)(U8 *fmt);", "\"X\"+Mark()");
    ]

let print_faults =
  List.map
    (fun (name, source, code) ->
      ( name,
        print_header ^ "extern U0 PutChars(U64 ch);" ^ print_capture
        ^ "PutChars('B');" ^ source,
        code ))
    [
      ("invalid format draft", "p(\"A%j\",42);", "HCIRVM0024");
      ("wrong string kind", "p(\"%s\",42);", "HCIRVM0025");
      ("wrong word kind", "p(\"%d\",\"A\");", "HCIRVM0025");
      ("missing word", "p(\"%d\");", "HCIRVM0025");
      ("missing star pair", "p(\"%*s\",4);", "HCIRVM0025");
      ("missing list pair", "p(\"%z\",1);", "HCIRVM0025");
      ("signed byte scan", "I8 A[2]={65,0};p(\"%s\",A);", "HCIRVM0008");
      ("wider pointee", "I64 A[1]={0};p(\"%s\",A);", "HCIRVM0018");
      ("unknown byte", "U8 A[2];A[0]=65;p(\"%s\",A);", "HCIRVM0012");
      ("byte extent", "U8 A[1]={65};p(\"%s\",A);", "HCIRVM0019");
    ]
