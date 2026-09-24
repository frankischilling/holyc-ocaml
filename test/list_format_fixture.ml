module F = Integer_format_fixture

(* Kernel/StrA.HC:397-414 selects NUL-delimited entries. Each skipped entry
   charges an outer probe, its complete inner scan and the following alias
   probe. A found entry has both the final outer and existence probes, then
   StrPrint.HC:373-386 measures it through OutStr before layout/copy. *)
let names = "A\000BB\000CCC\000"

let entry label format index list bytes work =
  F.case label format (index ^ "," ^ F.quote list) bytes work

let all =
  [
    entry "list selects its first entry" "%z" "0" names "A" 9;
    entry "list skips one entry" "%z" "1" names "BB" 16;
    entry "list skips differently sized entries" "%z" "2" names "CCC" 24;
    entry "list exact exhaustion checks the final empty entry" "%z" "3" names ""
      20;
    entry "huge list index stops at the sentinel" "%z" "9223372036854775807"
      names "" 19;
    entry "negative list index retains its initial probe" "%z" "-1" names "" 4;
    entry "minimum signed list index retains its initial probe" "%z"
      "(-9223372036854775807-1)" names "" 4;
    entry "unsigned high-bit list index uses signed interpretation" "%z"
      "0xffffffffffffffff" names "" 4;
    entry "empty list index zero repeats the existence probe" "%z" "0" "" "" 5;
    entry "empty list positive index stops at the first probe" "%z" "1" "" "" 4;
    entry "leading empty entry terminates the list" "%z" "1" "\000A\000" "" 4;
    entry "list right padding uses selected entry length" "%5z" "1" names
      "   BB" 20;
    entry "list left padding uses selected entry length" "%-5z" "1" names
      "BB   " 21;
    entry "list zero flag still pads with spaces" "%05z" "1" names "   BB" 21;
    entry "list truncation measures the selected entry" "%1tz" "1" names "B" 16;
    entry "zero-width list output still measures the selected entry" "%0tz" "1"
      names "" 14;
    entry "list truncation pads shorter selected entries" "%4tz" "1" names
      "  BB" 20;
    F.case "negative dynamic list width truncates after measurement" "%*tz"
      ("-1,1," ^ F.quote names)
      "" 14;
    F.case "negative dynamic list width preserves untruncated output" "%*z"
      ("-1,1," ^ F.quote names)
      "BB" 17;
    entry "negative list index still receives field padding" "%3z" "-1" names
      "   " 8;
    entry "exhausted list still receives field padding" "%3z" "3" names "   " 24;
    entry "empty list still receives field padding" "%3z" "0" "" "   " 9;
    entry "list alias does not consume an index" "%z" "1" "A\000@Alias\000B\000"
      "B" 21;
    entry "consecutive aliases do not consume an index" "%z" "1"
      "A\000@x\000@y\000BB\000" "BB" 24;
    entry "initial at-sign remains part of the first entry" "%z" "0"
      "@A\000B\000" "@A" 12;
    entry "initial at-sign participates in skipped entry length" "%z" "1"
      "@A\000B\000" "B" 14;
    entry "empty alias terminates selection" "%z" "1" "A\000@\000B\000" "" 8;
    entry "alias without a later ordinary entry exhausts selection" "%z" "1"
      "A\000@B\000" "" 13;
    entry "selected list entry preserves high bytes" "%z" "1"
      "A\000\255\128\000" "\255\128" 16;
    entry "selected list entry preserves spaces" "%z" "0" " \000" " " 9;
    entry "zero auxiliary does not suppress list selection" "%h0z" "1" names
      "BB" 18;
    F.case "list stars consume width precision auxiliary then index and pointer"
      "%*.*h*z"
      ("4,99,-2,1," ^ F.quote names)
      "  BB" 23;
    entry "list dollar flag does not transform selected bytes" "%$z" "0"
      "$%\000" "$%" 13;
    entry "list slash flag does not transform selected bytes" "%/z" "0" "$%\000"
      "$%" 13;
    entry "list length and comma flags preserve selected bytes" "%l,z" "1" names
      "BB" 18;
    F.case "list conversion consumes two arguments before the next field"
      "%z|%d"
      ("1," ^ F.quote names ^ ",42")
      "BB|42" 22;
    F.case "list auxiliary flags reset before the next decimal" "%h2z|%d"
      ("0," ^ F.quote names ^ ",42")
      "A|42" 17;
    F.case "two list fields retain independent selected offsets" "%z|%z|%d"
      ("0," ^ F.quote names ^ ",2," ^ F.quote names ^ ",42")
      "A|CCC|42" 40;
    F.case "measured ordinary string resets the selected list offset" "%z|%3s"
      ("1," ^ F.quote names ^ "," ^ F.quote "X")
      "BB|  X" 27;
    entry "compiler disassembler register list selects RAX" "%z" "3"
      "AL\000AX\000EAX\000RAX\000" "RAX" 31;
  ]

let quota_cases =
  List.filter
    (fun (case : F.t) ->
      List.mem case.label
        [
          "list selects its first entry";
          "list skips one entry";
          "list alias does not consume an index";
          "list right padding uses selected entry length";
          "list left padding uses selected entry length";
          "list truncation measures the selected entry";
          "zero-width list output still measures the selected entry";
          "exhausted list still receives field padding";
          "huge list index stops at the sentinel";
        ])
    all

let invalid_fields =
  [
    ( "list conversion requires both arguments",
      "Print(\"%z\");",
      2,
      "HCIRVM0025" );
    ( "list conversion requires its second argument",
      "Print(\"%z\",0);",
      2,
      "HCIRVM0025" );
    ( "list pair availability precedes first kind validation",
      "Print(\"%z\",\"A\");",
      2,
      "HCIRVM0025" );
    ("list index must be a word", "Print(\"%z\",\"A\",\"B\");", 2, "HCIRVM0025");
    ("list requires an owned pointer", "Print(\"%z\",0,42);", 2, "HCIRVM0025");
    ( "internal list misses grant no raw-null argument",
      "Print(\"%z\",0,0);",
      2,
      "HCIRVM0025" );
    ( "negative index still validates its list argument",
      "Print(\"%z\",-1,42);",
      2,
      "HCIRVM0025" );
    ( "list pair follows all captured field stars",
      "Print(\"%*.*h*z\",2,3,4,1);",
      7,
      "HCIRVM0025" );
    ( "define-list uppercase conversion remains explicit",
      "Print(\"%Z\",0,\"A\");",
      2,
      "HCIRVM0024" );
  ]

let memory_failures =
  [
    ( "negative index still probes an uninitialized list",
      "I64 F(){U8 Text[1];Print(\"|\");Print(\"%z\",-1,Text);return 42;}F();",
      "HCIRVM0012",
      3 );
    ( "negative index still probes a one-past pointer",
      "U8 Text[1]={0};Print(\"|\");Print(\"%z\",-1,&Text[1]);42;",
      "HCIRVM0019",
      3 );
    ( "selected list entry must terminate before any append",
      "U8 Text[2]={'A','B'};Print(\"|\");Print(\"%z\",0,Text);42;",
      "HCIRVM0019",
      7 );
    ( "selected list truncation cannot hide a missing terminator",
      "U8 Text[2]={'A','B'};Print(\"|\");Print(\"%1tz\",0,Text);42;",
      "HCIRVM0019",
      9 );
    ( "zero-width list output cannot hide an uninitialized selected byte",
      "I64 F(){U8 \
       Text[2];Text[0]='A';Print(\"|\");Print(\"%0tz\",0,Text);return 42;}F();",
      "HCIRVM0012",
      8 );
    ( "skipped list entry must terminate",
      "U8 Text[2]={'A','B'};Print(\"|\");Print(\"%z\",1,Text);42;",
      "HCIRVM0019",
      6 );
    ( "skipped list entry retains uninitialized-byte faults",
      "I64 F(){U8 Text[2];Text[0]='A';Print(\"|\");Print(\"%z\",1,Text);return \
       42;}F();",
      "HCIRVM0012",
      5 );
    ( "skipped terminator requires the following alias probe",
      "U8 Text[2]={'A',0};Print(\"|\");Print(\"%z\",1,Text);42;",
      "HCIRVM0019",
      6 );
    ( "alias marker requires the next outer probe",
      "U8 Text[3]={'A',0,'@'};Print(\"|\");Print(\"%z\",1,Text);42;",
      "HCIRVM0019",
      7 );
    ( "list lookup preserves signed-byte pointer policy",
      "I8 Text[2]={'A',0};Print(\"|\");Print(\"%z\",0,Text);42;",
      "HCIRVM0008",
      3 );
    ( "list lookup preserves wider-element pointer policy",
      "U16 Text[2]={'A',0};Print(\"|\");Print(\"%z\",0,Text);42;",
      "HCIRVM0018",
      3 );
    ( "list pair failure precedes a later format bounds fault",
      "U8 Fmt[2]={'%','z'};Print(\"|\");Print(Fmt,0);42;",
      "HCIRVM0025",
      2 );
    ( "completed list draft still requires the final format byte",
      "U8 Fmt[2]={'%','z'};Print(\"|\");Print(Fmt,0,\"A\");42;",
      "HCIRVM0019",
      9 );
  ]

let source_effects =
  [
    ( "list selection respects an interior pointer base",
      "extern U0 Print(U8 *fmt,...);U8 \
       Text[7]=\"?A\\0BB\\0\";Print(\"%z\",1,&Text[1]);42;",
      "BB",
      16 );
    ( "list pointer sees argument-time writes",
      "extern U0 Print(U8 *fmt,...);U8 Text[2]={'A',0};I64 \
       Index(){Text[0]='X';return 0;}Print(\"%z\",Index(),Text);42;",
      "X",
      9 );
    ( "list format remains mutable before the call",
      "extern U0 Print(U8 *fmt,...);U8 Fmt[3]=\"%d\";I64 \
       Index(){Fmt[1]='z';return 0;}Print(Fmt,Index(),\"A\");42;",
      "A",
      9 );
    ( "selected list entry does not inspect its unknown tail",
      "extern U0 Print(U8 *fmt,...);I64 F(){U8 \
       Text[4];Text[0]='A';Text[1]=0;Print(\"%z\",0,Text);return 42;}F();",
      "A",
      9 );
    ( "last selected entry needs no following sentinel",
      "extern U0 Print(U8 *fmt,...);U8 Text[2]={'A',0};Print(\"%z\",0,Text);42;",
      "A",
      9 );
  ]
