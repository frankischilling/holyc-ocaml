module F = Integer_format_fixture

let string_case label format input bytes work =
  F.case label format (F.quote input) bytes work

(* Byte expectations follow MPrintQ/MPrintq in Kernel/StrPrint.HC:56-197.
   Work counts include a complete first pass, selected-prefix reconstruction,
   format reads and appends. MPrintq's separate lookahead reads are charged. *)
let all =
  [
    string_case "quoted empty string" "%Q" "" "" 4;
    string_case "decoded empty string" "%q" "" "" 4;
    string_case "quoted ordinary bytes" "%Q" "AB" "AB" 10;
    string_case "decoded ordinary bytes retain lookahead reads" "%q" "AB" "AB"
      14;
    string_case "quoted double quote" "%Q" "\"" "\\\"" 8;
    string_case "quoted named control escapes" "%Q" "\n\r\t\"\\"
      "\\n\\r\\t\\\"\\\\" 24;
    string_case "quoted low control byte" "%Q" "\001" "\\x01" 10;
    string_case "quoted last escaped control byte" "%Q" "\030" "\\x1E" 10;
    string_case "quoted shift-space passes through" "%Q" "\031" "\031" 7;
    string_case "quoted delete byte" "%Q" "\127" "\\x7F" 10;
    string_case "quoted high bytes stay unchanged" "%Q" "\128\255" "\128\255" 10;
    string_case "quoted real NUL stops input" "%Q" "A\000B" "A" 7;
    string_case "decoded real NUL stops input" "%q" "A\000B" "A" 9;
    string_case "quoted dollar duplicates" "%Q" "$%" "$$%" 11;
    string_case "quoted dollar escape modifier" "%$Q" "$%" "\\d%" 12;
    string_case "quoted percent escape modifier" "%/Q" "$%" "$$%%" 13;
    string_case "quoted dollar and percent modifiers" "%$/Q" "$%" "\\d%%" 14;
    string_case "quoted repeated modifiers" "%//$Q" "$%" "\\d%%" 15;
    string_case "quoted adjacent dollars each expand" "%Q" "$$" "$$$$" 12;
    string_case "decoded named escapes" "%q" "\\'\\`\\\"\\\\\\d\\n\\r\\t"
      "'`\"\\$\n\r\t" 44;
    string_case "decoded unknown escape preserves its bytes" "%q" "\\z" "\\z" 14;
    string_case "decoded trailing backslash" "%q" "\\" "\\" 9;
    string_case "decoded two hexadecimal digits" "%q" "\\x41" "A" 13;
    string_case "decoded mixed hexadecimal case" "%q" "\\X4a" "J" 13;
    string_case "decoded high byte" "%q" "\\xff" "\255" 13;
    string_case "decoded one hexadecimal digit" "%q" "\\x4" "\004" 13;
    string_case "decoded invalid hex is left for the next token" "%q" "\\x4G"
      "\004G" 18;
    string_case "decoded hex consumes at most two digits" "%q" "\\x414" "A4" 18;
    string_case "decoded missing hex digits produce NUL" "%q" "\\x" "" 7;
    string_case "decoded non-hex first digit produces NUL" "%q" "\\xG" "" 9;
    string_case "decoded NUL still scans later input" "%q" "A\\0B" "A" 13;
    F.case "decoded NUL padding preserves the next argument" "%5q|%d"
      (F.quote "A\\0B" ^ ",42")
      "    A|42" 24;
    string_case "decoded initial NUL has no visible payload" "%q" "\\0A" "" 8;
    string_case "decoded hexadecimal NUL still scans" "%q" "\\x00A" "" 10;
    string_case "decoded dollar pairs collapse" "%q" "$$$" "$$" 14;
    string_case "decoded percent pair normally stays doubled" "%q" "%%" "%%" 14;
    string_case "decoded percent pair collapses with slash" "%/q" "%%" "%" 10;
    string_case "decoded dollar modifier does not change dollar rule" "%$q" "$$"
      "$" 10;
    string_case "quoted right padding boundary" "%5Q" "AB" "   AB" 14;
    string_case "decoded right alignment" "%5q" "AB" "   AB" 18;
    string_case "decoded left alignment" "%-5q" "AB" "AB   " 19;
    string_case "quoted zero flag uses spaces" "%05Q" "AB" "   AB" 15;
    string_case "quoted truncation splits an escape" "%1tQ" "\n" "\\" 9;
    string_case "quoted truncation splits a hexadecimal escape" "%3tQ" "\001"
      "\\x0" 11;
    string_case "quoted prefix excludes later source from the copy pass" "%2tQ"
      "\nX" "\\n" 11;
    string_case "quoted zero truncation still measures" "%0tQ" "AB" "" 8;
    string_case "decoded zero truncation still measures" "%0tq" "AB" "" 10;
    string_case "decoded prefix retains lookahead work" "%1tq" "AB" "A" 13;
    string_case "decoded prefix ends after an escape" "%1tq" "\\nX" "\n" 13;
    F.case "quoted dynamic width and ignored precision" "%*.*Q"
      ("5,3," ^ F.quote "\n")
      "   \\n" 14;
    F.case "decoded negative truncation still measures" "%*tq"
      ("-1," ^ F.quote "AB")
      "" 10;
    F.case "quoted modifiers reset at the next field" "%$Q|%Q"
      (F.quote "$" ^ "," ^ F.quote "$")
      "\\d|$$" 18;
    F.case "uppercase packed ASCII" "%C" "'aBz1'" "ABZ1" 12;
    F.case "uppercase full packed word" "%C" "'abcdefgh'" "ABCDEFGH" 19;
    F.case "uppercase packed high bytes stay unchanged" "%C" "0x7a8061ff"
      "\255A\128Z" 12;
    F.case "uppercase packed stops at the first NUL" "%C" "0x00620061" "A" 6;
    F.case "uppercase empty packed word" "%C" "0" "" 4;
    F.case "uppercase packed right alignment" "%5C" "'ab'" "   AB" 12;
    F.case "uppercase packed left alignment" "%-5C" "'ab'" "AB   " 13;
    F.case "uppercase packed prefix truncation" "%1tC" "'az'" "A" 9;
    F.case "uppercase full packed prefix" "%1tC" "'abcdefgh'" "A" 14;
    F.case "uppercase packed flag resets before lowercase c" "%C|%c" "'a','a'"
      "A|a" 13;
  ]

let quota_cases =
  List.filter
    (fun (case : F.t) ->
      List.mem case.label
        [
          "quoted double quote";
          "quoted low control byte";
          "quoted dollar and percent modifiers";
          "decoded named escapes";
          "decoded invalid hex is left for the next token";
          "quoted zero truncation still measures";
          "decoded NUL still scans later input";
          "decoded NUL padding preserves the next argument";
          "uppercase packed right alignment";
          "uppercase full packed prefix";
        ])
    all

let invalid_fields =
  [
    ("missing escaped string", "Print(\"%Q\");", 2, "HCIRVM0025");
    ("missing decoded string", "Print(\"%q\");", 2, "HCIRVM0025");
    ("word supplied to escaped string", "Print(\"%Q\",42);", 2, "HCIRVM0025");
    ("word supplied to decoded string", "Print(\"%q\",42);", 2, "HCIRVM0025");
    ( "pointer supplied to uppercase packed field",
      "Print(\"%C\",\"a\");",
      2,
      "HCIRVM0025" );
  ]

let interleaved_faults =
  [
    ("uppercase packed visits interleave", "%C", "'abcdefgh'", 6);
    ("inert uppercase precision retains visit order", "%.*C", "1,'abcdefgh'", 8);
  ]

let memory_failures =
  [
    ( "escaped truncation requires real input NUL",
      "U8 Text[1]={'A'};Print(\"|\");Print(\"%1tQ\",Text);42;",
      "HCIRVM0019",
      6 );
    ( "decoded lookahead is checked before conversion",
      "U8 Text[1]={'A'};Print(\"|\");Print(\"%q\",Text);42;",
      "HCIRVM0019",
      4 );
    ( "decoded NUL cannot hide later object bounds",
      "U8 Text[3]={92,48,65};Print(\"|\");Print(\"%q\",Text);42;",
      "HCIRVM0019",
      6 );
    ( "zero-digit hexadecimal NUL still reads the hidden candidate",
      "U8 Text[3]={92,120,71};Print(\"|\");Print(\"%q\",Text);42;",
      "HCIRVM0019",
      7 );
    ( "decoded NUL cannot hide later uninitialized lookahead",
      "I64 F(){U8 \
       Text[4];Text[0]=92;Text[1]=48;Text[2]=65;Print(\"|\");Print(\"%q\",Text);return \
       42;}F();",
      "HCIRVM0012",
      6 );
    ( "decoded hexadecimal candidates have checked bounds",
      "U8 Text[3]={92,120,65};Print(\"|\");Print(\"%q\",Text);42;",
      "HCIRVM0019",
      6 );
    ( "escaped byte pointer retains signed-cell failure",
      "I64 F(){I8 Text[1];Text[0]=0;Print(\"|\");Print(\"%Q\",Text);return \
       42;}F();",
      "HCIRVM0008",
      3 );
    ( "decoded wider pointer fails at its first read",
      "I64 F(){I16 Text[1];Text[0]=0;Print(\"|\");Print(\"%q\",Text);return \
       42;}F();",
      "HCIRVM0018",
      3 );
  ]
