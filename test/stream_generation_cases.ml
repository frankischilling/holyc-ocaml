let headers =
  {|extern U0 StreamPrint(U8 *fmt,...);extern I64 StreamExePrint(U8 *fmt,...);extern U0 Print(U8 *fmt,...);|}

let compiler_option_headers =
  {|extern U8 GetOption(I64 num);extern U8 Option(I64 num,U8 val);|}

let compiler_option_bits =
  [
    (0, false);
    (1, false);
    (16, true);
    (17, false);
    (18, false);
    (19, true);
    (32, false);
    (33, false);
    (34, false);
    (35, false);
    (36, false);
    (37, false);
  ]

let compiler_option_registry =
  List.map
    (fun (index, enabled) ->
      let old = if enabled then 1 else 0 in
      let next = 1 - old in
      ( Printf.sprintf "original compiler option %d" index,
        Printf.sprintf
          "#exe \
           {Print(\"%%d;\",GetOption(%d));Print(\"%%d;\",Option(%d,%d));Print(\"%%d;\",GetOption(%d));Print(\"%%d;\",Option(%d,%d));Print(\"%%d;\",GetOption(%d));}42;"
          index index next index index old index,
        Printf.sprintf "%d;%d;%d;%d;%d;" old old next next old ))
    compiler_option_bits

let compiler_options =
  compiler_option_registry
  @ [
      ( "ordinary current control and Bool coercion",
        {|#exe {Print("%d;",GetOption(33));Print("%d;",Option(33,1));Print("%d;",Option(33,255));Print("%d;",Option(33,256));Print("%d;",GetOption(33));}42;|},
        "0;0;1;1;0;" );
      ( "saved table child copies live caller options",
        {|#exe {Print("%d;",GetOption(33));Print("%d;",Option(33,1));Print("%d;",StreamExePrint("Print(\"%%d;\",GetOption(33));Print(\"%%d;\",Option(33,0));Print(\"%%d;\",GetOption(33));42;"));Print("%d;",GetOption(33));}42;|},
        "0;0;1;1;0;42;1;" );
      ( "successive children keep independent changes",
        {|#exe {Option(33,1);StreamExePrint("Option(33,0);42;");Print("%d;",StreamExePrint("GetOption(33)+41;"));Print("%d;",GetOption(33));}42;|},
        "42;1;" );
      ( "retained direct function reads current control",
        {|#exe {I64 Read(){return GetOption(33);}Option(33,1);Print("%d;",Read());Option(33,0);Print("%d;",Read());}42;|},
        "1;0;" );
      ( "owned callbacks preserve original option signatures",
        {|#exe {U8 (*read)(I64 num)=&GetOption;U8 (*write)(I64 num,U8 val)=&Option;Print("%d;",read(33));Print("%d;",write(33,1));Print("%d;",read(33));}42;|},
        "0;0;1;" );
      ( "retained callback defaults reach current control",
        {|#exe {I64 Read(U8 (*p)(I64 num)=&GetOption){return p(33);}Option(33,1);Print("%d;",Read());Option(33,0);Print("%d;",Read());}42;|},
        "1;0;" );
      ( "default expression changes the live header control",
        {|#exe {I64 Read(I64 n=Option(33,1)){return n;}Print("%d;",GetOption(33));Print("%d;",Read());}42;|},
        "1;0;" );
      ( "retained setter callback defaults preserve Bool narrowing",
        {|#exe {I64 Write(U8 (*p)(I64 num,U8 val)=&Option){Print("%d;",p(33,255));Print("%d;",p(33,256));return GetOption(33);}Print("%d;",Write());}42;|},
        "0;1;0;" );
      ( "source body replaces the option extern slot",
        {|#exe {U8 Option(I64 num,U8 val){return 7;}Print("%d;",Option(33,1));Print("%d;",GetOption(33));}42;|},
        "7;0;" );
    ]

(* Expected source values and generation lengths are derived from the text,
   independently of either executor. The ordinary capture is separate. *)
let values =
  [
    ("original stream", {|#exe {StreamPrint("42;");}|}, "", 3);
    ( "generated global and ordinary output",
      {|#exe {StreamPrint("I64 N=40;");Print("side");}N+2;|},
      "side",
      9 );
    ( "original provider callback",
      {|#exe {U0 (*p)(U8 *fmt,...)=&StreamPrint;p("I64 N=%d;",40);}N+2;|},
      "",
      9 );
    ( "retained function in owning task",
      {|#exe {U0 Emit(I64 n){StreamPrint("I64 N=%d;",n);}Emit(40);}N+2;|},
      "",
      9 );
    ( "saved callback default",
      {|#exe {I64 F(U0 (*p)(U8 *fmt,...)=&StreamPrint){p("42;");return 0;}F;}|},
      "",
      3 );
    ( "retained saved default across streams",
      {|#exe {I64 F(U0 (*p)(U8 *fmt,...)=&StreamPrint){p("42;");return 0;}}#exe {F;}|},
      "",
      3 );
    ( "callback survives later stream",
      {|U0 (*p)(U8 *fmt,...)=&StreamPrint;#exe {p("40;");}#exe {p("42;");}|},
      "",
      6 );
    ( "copied callback keeps executable owner",
      {|U0 (*p)(U8 *fmt,...)=&StreamPrint;U0 (*q)(U8 *fmt,...)=p;p=0;#exe {q("42;");}|},
      "",
      3 );
    ( "indexed callback capture",
      {|#exe {U0 (*p)(U8 *fmt,...)[2]={0,&StreamPrint};p[1]("42;");}|},
      "",
      3 );
    ( "mutable owned format",
      {|#exe {U8 fmt[3]={37,100,0};StreamPrint(fmt,42);StreamPrint(";");}|},
      "",
      3 );
    ( "successive atomic calls",
      {|#exe {StreamPrint("4");StreamPrint("2;");}|},
      "",
      3 );
    ( "generated source consumed before next stream",
      {|#exe {StreamPrint("I64 N=%d;",40);}#exe {StreamPrint("N+2;");}|},
      "",
      13 );
    ( "dynamic width and owned string",
      {|#exe {StreamPrint("%*s;",2,"42");}|},
      "",
      3 );
    ( "original nested stream",
      {|#exe {StreamPrint("#exe {StreamPrint(\"42;\");}");}|},
      "",
      29 );
    ( "retained recursive callback",
      {|#exe {I64 Emit(U0 (*p)(U8 *fmt,...),I64 n){if(n)return Emit(p,n-1);p("42;");return 0;}Emit(&StreamPrint,3);}|},
      "",
      3 );
    ( "default captured before clearing cell",
      {|#exe {U0 (*p)(U8 *fmt,...)=&StreamPrint;I64 Emit(U0 (*q)(U8 *fmt,...)=p){q("42;");return 0;}p=0;Emit;}|},
      "",
      3 );
    ( "owned list subscript",
      {|#exe {U8 L[6]={52,48,0,52,50,0};StreamPrint("%z;",1,L);}|},
      "",
      3 );
    ( "unused scalar pointer tail",
      {|#exe {I64 A=7;StreamPrint("42;",&A);}|},
      "",
      3 );
    ( "ASCII uppercase packed output",
      {|#exe {StreamPrint("I64 ");StreamPrint("%C",'a');StreamPrint("=42;");}A;|},
      "",
      9 );
    ( "old provider and new source captures",
      {|#exe {I64 N=0;U0 (*p)(U8 *fmt,...)=&StreamPrint;U0 StreamPrint(U8 *fmt,...){N=1;}U0 (*q)(U8 *fmt,...)=&StreamPrint;p("42;");q("ignored");Print("%d",N);}|},
      "1",
      3 );
    ( "saved default survives provider replacement",
      {|#exe {I64 N=0;I64 Emit(U0 (*p)(U8 *fmt,...)=&StreamPrint){p("42;");return 0;}U0 StreamPrint(U8 *fmt,...){N=1;}Emit;Print("%d",N);}|},
      "0",
      3 );
    ( "unreached malformed format",
      {|#exe {if(0)StreamPrint("%f",42);StreamPrint("42;");}|},
      "",
      3 );
  ]

let failures =
  [
    ("inactive StreamPrint", {|StreamPrint("hello");|}, "HCIRVM0027", "", 11, 0);
    ( "format before inactive check",
      {|StreamPrint("%f",42);|},
      "HCIRVM0024",
      "",
      2,
      0 );
    ("formatted JIT source", {|StreamExePrint("42;");|}, "HCIRVM0027", "", 7, 0);
    ( "formatted callback JIT source",
      {|I64 (*p)(U8 *fmt,...)=&StreamExePrint;p("42;");|},
      "HCIRVM0027",
      "",
      7,
      0 );
    ( "StreamExePrint JIT format fault",
      {|#exe {StreamExePrint("%f",42);}|},
      "HCIRVM0024",
      "",
      2,
      0 );
    ( "earlier generation survives format fault",
      {|#exe {StreamPrint("42;");StreamPrint("%f",42);}|},
      "HCIRVM0024",
      "",
      9,
      3 );
    ( "earlier generation survives an inactive source call",
      {|#exe {StreamPrint("42;");}StreamExePrint("42;");|},
      "HCIRVM0027",
      "",
      14,
      3 );
    ( "abort retains output and generated charges",
      {|#exe {StreamPrint("42;");Print("side");1/0;}|},
      "HCIRVM0009",
      "side",
      16,
      3 );
    ( "pointer validation before stream check",
      {|StreamPrint("%s;",0);|},
      "HCIRVM0025",
      "",
      2,
      0 );
    ( "completed stream is inactive",
      {|#exe {StreamPrint("42;");}StreamPrint("x");|},
      "HCIRVM0027",
      "",
      10,
      3 );
  ]

let quota_source = {|#exe {StreamPrint("42;");Print("ok");}|}
let two_streams = {|#exe {StreamPrint("40;");}#exe {StreamPrint("42;");}|}

let example =
  {|#exe {
I64 Emit(U0 (*sink)(U8 *fmt,...)=&StreamPrint) {
  sink("I64 Answer=%d;",40);
  return 0;
}
Emit;
Print("made");
}
Answer+2;
|}
