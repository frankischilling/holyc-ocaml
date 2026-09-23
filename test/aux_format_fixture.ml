module F = Integer_format_fixture

(* Kernel/StrPrint.HC:270-315 retains the auxiliary accumulator between h
   modifiers. Its I64 arithmetic wraps; a literal minus remains sticky across
   later modifiers. Lines 390-411 apply OutStr separately to each packed copy.
   Work counts below include the existing visits and appends for every copy. *)
let all =
  [
    F.case "bare auxiliary selects zero copies" "%hc" "'a'" "" 4;
    F.case "question auxiliary starts with zero copies" "%h?c" "'a'" "" 5;
    F.case "zero copies consume the packed argument" "%h0C|%c" "'a','b'" "|b" 12;
    F.case "three packed copies" "%h3c" "'a'" "aaa" 14;
    F.case "repeated uppercase full word" "%h2C" "'abcdefgh'" "ABCDEFGHABCDEFGH"
      37;
    F.case "each packed copy receives right padding" "%4h2C" "'ab'" "  AB  AB"
      20;
    F.case "each packed copy receives left padding" "%-4h2C" "'ab'" "AB  AB  "
      21;
    F.case "each packed copy truncates its own prefix" "%1th2C" "'az'" "AA" 15;
    F.case "ordinary modifiers resume after auxiliary digits" "%1h2tC" "'az'"
      "AA" 15;
    F.case "zero-width copies still visit packed bytes" "%0th2C" "'az'" "" 13;
    F.case "zero flag preserves repeated packed visits" "%0h2C" "'ab'" "ABAB" 16;
    F.case "repeated uppercase preserves high bytes" "%h2C" "0x7a8061ff"
      "\255A\128Z\255A\128Z" 23;
    F.case "later auxiliary digits append to the accumulator" "%h1h2c" "'a'"
      "aaaaaaaaaaaa" 43;
    F.case "sticky minus negates after later digits" "%h-2h3c" "'a'"
      "aaaaaaaaaaaaaaaaa" 59;
    F.case "later minus negates the combined accumulator" "%h2h-3c" "'a'" "" 8;
    F.case "bare later auxiliary reapplies sticky minus" "%h-2hc" "'a'" "aa" 13;
    F.case "repeated literal minus preserves sticky state" "%h-2h-c" "'a'" "aa"
      14;
    F.case "minus without digits applies to later auxiliary" "%h-h2c" "'a'" "" 7;
    F.case "question before digits preserves auxiliary" "%h?h2c" "'a'" "aa" 13;
    F.case "question after digits preserves auxiliary" "%h2h?c" "'a'" "aa" 13;
    F.case "question preserves sticky sign for a later bare auxiliary"
      "%h-2h?hc" "'a'" "aa" 15;
    F.case "later auxiliary star replaces the accumulator" "%h*h*c" "2,3,'a'"
      "aaa" 16;
    F.case "literal digits append to a captured auxiliary" "%h*h2c" "1,'a'"
      "aaaaaaaaaaaa" 43;
    F.case "star replaces earlier literal auxiliary" "%h1h*c" "2,'a'" "aa" 13;
    F.case "star preserves sticky sign for a later bare auxiliary" "%h-2h*hc"
      "3,'a'" "" 9;
    F.case "negative star then sticky sign restores positive copies" "%h-2h*hc"
      "-3,'a'" "aaa" 18;
    F.case "negative copies preserve the next argument" "%h*c|%d" "-1,'a',42"
      "|42" 11;
    F.case "minimum captured auxiliary skips packed visits" "%h*c"
      "(-9223372036854775807-1),'a'" "" 5;
    F.case "auxiliary literal wraps to zero" "%h18446744073709551616c" "'a'" ""
      24;
    F.case "auxiliary literal wraps to one" "%h18446744073709551617c" "'a'" "a"
      27;
    F.case "auxiliary literal wraps to signed minimum" "%h9223372036854775808c"
      "'a'" "" 23;
    F.case "negating minimum auxiliary keeps its bits" "%h-9223372036854775808c"
      "'a'" "" 24;
    F.case "negative auxiliary literal wraps to one" "%h-18446744073709551615c"
      "'a'" "a" 28;
    F.case "width precision and auxiliary stars consume in order" "%*.*h*C"
      "4,99,2,'ab'" "  AB  AB" 22;
    F.case "zero auxiliary does not suppress quoted conversion" "%h0Q"
      (F.quote "AB") "AB" 12;
    F.case "negative auxiliary does not suppress decoding" "%h-2q"
      (F.quote "AB") "AB" 17;
    F.case "auxiliary star precedes quoted pointer" "%h*Q|%d"
      ("6," ^ F.quote "AB" ^ ",42")
      "AB|42" 18;
    F.case "auxiliary star precedes ordinary string pointer" "%h*s|%d"
      ("9," ^ F.quote "AB" ^ ",42")
      "AB|42" 16;
    F.case "auxiliary star is consumed for literal percent" "%h*%|%d" "7,42"
      "%|42" 12;
    F.case "auxiliary digits leave hexadecimal output unchanged" "%h123X" "42"
      "2A" 9;
    F.case "question auxiliary leaves hexadecimal output unchanged" "%h?X" "12"
      "C" 6;
    F.case "auxiliary binary remains ordinary binary" "%h2B" "5" "101" 8;
    F.case "auxiliary flags reset at the next decimal field" "%h2c|%d" "'a',42"
      "aa|42" 17;
    F.case "compiler listing indentation precedes hexadecimal address"
      "%h*c%08X " "3,' ',42" "   0000002A " 28;
  ]

(* These measured or full-word cases have no terminator visit after their last
   append, so the existing exact/one-below quota matrix applies unchanged. *)
let quota_cases =
  List.filter
    (fun (case : F.t) ->
      List.mem case.label
        [
          "repeated uppercase full word";
          "each packed copy receives right padding";
          "each packed copy receives left padding";
          "each packed copy truncates its own prefix";
          "zero-width copies still visit packed bytes";
          "width precision and auxiliary stars consume in order";
        ])
    all

let invalid_fields =
  [
    ( "engineering decimal remains explicit",
      "Print(\"%h0d\",1);",
      4,
      "HCIRVM0024" );
    ( "engineering unsigned remains explicit",
      "Print(\"%h0u\",1);",
      4,
      "HCIRVM0024" );
    ( "engineering question remains explicit",
      "Print(\"%h?d\",1);",
      4,
      "HCIRVM0024" );
    ( "engineering format still requires its value",
      "Print(\"%h0d\");",
      4,
      "HCIRVM0025" );
    ( "engineering format checks the consumed word",
      "Print(\"%h0u\",\"x\");",
      4,
      "HCIRVM0025" );
    ( "auxiliary star fails before conversion lookahead",
      "Print(\"%h*c\");",
      3,
      "HCIRVM0025" );
    ( "auxiliary star rejects a pointer before lookahead",
      "Print(\"%h*c\",\"x\",1);",
      3,
      "HCIRVM0025" );
    ( "auxiliary star still needs a conversion",
      "Print(\"%h*\",1);",
      4,
      "HCIRVM0024" );
    ( "bare auxiliary still needs a conversion",
      "Print(\"%h\");",
      3,
      "HCIRVM0024" );
    ( "auxiliary minus still needs a conversion",
      "Print(\"%h-\");",
      4,
      "HCIRVM0024" );
    ( "auxiliary question still needs a conversion",
      "Print(\"%h?\");",
      4,
      "HCIRVM0024" );
    ( "auxiliary digits still need a conversion",
      "Print(\"%h2\");",
      4,
      "HCIRVM0024" );
    ( "auxiliary digits have no trailing star override",
      "Print(\"%h2*c\",2,'a');",
      4,
      "HCIRVM0024" );
    ( "auxiliary minus has no trailing star override",
      "Print(\"%h-*c\",2,'a');",
      4,
      "HCIRVM0024" );
    ( "zero copies still require packed input",
      "Print(\"%hc\");",
      3,
      "HCIRVM0025" );
    ( "zero copies still validate packed input",
      "Print(\"%h0C\",\"x\");",
      4,
      "HCIRVM0025" );
    ( "precision star precedes auxiliary star",
      "Print(\"%*.*h*C\",4);",
      4,
      "HCIRVM0025" );
    ( "auxiliary star follows precision star",
      "Print(\"%*.*h*C\",4,9);",
      6,
      "HCIRVM0025" );
  ]

let memory_failures =
  [
    ( "missing auxiliary precedes out-of-bounds format byte",
      "U8 Fmt[3]={37,104,42};Print(\"|\");Print(Fmt);42;",
      "HCIRVM0025",
      3 );
    ( "pointer auxiliary precedes out-of-bounds format byte",
      "U8 Fmt[3]={37,104,42};Print(\"|\");Print(Fmt,\"x\");42;",
      "HCIRVM0025",
      3 );
    ( "valid auxiliary exposes the next format read",
      "U8 Fmt[3]={37,104,42};Print(\"|\");Print(Fmt,1);42;",
      "HCIRVM0019",
      4 );
    ( "zero auxiliary still reads the conversion byte",
      "U8 Fmt[3]={37,104,42};Print(\"|\");Print(Fmt,0);42;",
      "HCIRVM0019",
      4 );
    ( "zero auxiliary does not bypass quoted bounds",
      "U8 Text[1]={'A'};Print(\"|\");Print(\"%h0Q\",Text);42;",
      "HCIRVM0019",
      6 );
  ]

let empty_repeats =
  [
    ("empty packed input", "%h*c", "9223372036854775807,0");
    ("truncated packed input", "%0th*C", "9223372036854775807,'A'");
  ]

let argument_effects =
  [
    ( "repeat count is evaluated once",
      "extern U0 Print(U8 *fmt,...);I64 Calls=0;I64 Count(){++Calls;return \
       3;}Print(\"%h*c\",Count(),'a');if(Calls!=1)1/0;42;",
      "aaa",
      14 );
  ]
