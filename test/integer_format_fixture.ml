type t = {
  label : string;
  format : string;
  arguments : string;
  bytes : string;
  work : int;
}

let case label format arguments bytes work =
  { label; format; arguments; bytes; work }

(* Expectations follow Kernel/StrPrint.HC at
   c26482bb6ad3f80106d28504ec5db3c6a360732c. In particular, integer precision
   does not truncate digits, numeric minus does not left-align, and grouped
   zero padding can begin with a comma. *)
let numbers =
  [
    case "unsigned maximum" "%u" "-1" "18446744073709551615" 23;
    case "unsigned high bit" "%u" "0x8000000000000000" "9223372036854775808" 22;
    case "lower hexadecimal maximum" "%x" "-1" "ffffffffffffffff" 19;
    case "upper hexadecimal high bit" "%X" "0x8000000000000000"
      "8000000000000000" 19;
    case "binary zero" "%b" "0" "0" 4;
    case "binary upper spelling" "%B" "42" "101010" 9;
    case "binary high bit" "%b" "0x8000000000000000"
      ("1" ^ String.make 63 '0')
      67;
    case "signed zero padding" "%05d" "-42" "-0042" 10;
    case "signed space padding" "%5d" "-42" "  -42" 9;
    case "numeric minus keeps right alignment" "%-5d" "42" "   42" 10;
    case "numeric minus with zero padding" "%-08d" "42" "00000042" 14;
    case "dynamic zero-padded hexadecimal" "%0*tX" "8,0x10000002A" "0000002A" 14;
    case "negative dynamic width" "%*d" "-5,42" "42" 6;
    case "minimum dynamic width" "%*d" "(-9223372036854775807-1),42" "42" 6;
    case "negative zero-padded width" "%0*d" "-5,-42" "-42" 8;
    case "literal integer precision is ignored" "%5.0d" "42" "   42" 11;
    case "dynamic integer precision is consumed" "%*.*d" "5,0,42" "   42" 11;
    case "star overrides literal width" "%12*d" "5,42" "   42" 11;
    case "star overrides literal precision" "%1.2*d" "1234,42" "42" 9;
    case "literal and dynamic width precede precision" "%2*.*d" "5,0,42" "   42"
      12;
    case "ignored precision preserves next argument" "%.*d|%u" "7,42,-1"
      "42|18446744073709551615" 31;
    case "negative precision is ignored" "%.*d" "-1,42" "42" 7;
    case "maximum literal precision allocates no padding"
      "%.9223372036854775807d" "42" "42" 25;
    case "signed grouped decimal" "%,d" "1234567" "1,234,567" 13;
    case "unsigned grouped maximum" "%,u" "-1" "18,446,744,073,709,551,615" 30;
    case "signed grouped minimum" "%,d" "(-9223372036854775807-1)"
      "-9,223,372,036,854,775,808" 30;
    case "four-nibble hexadecimal groups" "%,X" "-1" "FFFF,FFFF,FFFF,FFFF" 23;
    case "four-bit binary groups" "%,b" "-1"
      "1111,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111,1111"
      83;
    case "grouped zero padding starts with comma" "%08,d" "123" ",000,123" 14;
    case "grouped negative zero padding" "%08,d" "-123" "-000,123" 14;
    case "grouped zero padding crosses two groups" "%010,d" "123" "00,000,123"
      17;
    case "hex zero padding retains group phase" "%06,X" "0xABC" "0,0ABC" 12;
    case "hex zero padding starts with comma" "%05,X" "0xABC" ",0ABC" 11;
    case "truncation retains low decimal digits" "%4td" "123456" "3456" 9;
    case "truncation retains a group separator" "%4,td" "1234567" ",567" 10;
    case "zero-width signed truncation retains sign" "%0td" "-42" "-" 6;
    case "one-wide signed truncation retains sign" "%1td" "-42" "-" 6;
    case "two-wide signed truncation retains low digit" "%2td" "-42" "-2" 7;
    case "zero-width positive truncation" "%td" "42" "" 4;
    case "ignored ordinary integer modifiers" "%/l$d" "42" "42" 8;
    case "percent ignores width" "%3%" "" "%" 5;
    case "percent consumes dynamic width" "%*%:%d" "3,42" "%:42" 11;
  ]

let strings =
  [
    case "string right alignment" "%5s" "\"AB\"" "   AB" 14;
    case "string left alignment" "%-5s" "\"AB\"" "AB   " 15;
    case "string zero flag still pads spaces" "%05s" "\"AB\"" "   AB" 15;
    case "string width does not truncate by itself" "%1s" "\"AB\"" "AB" 11;
    case "string truncation retains prefix" "%1ts" "\"AB\"" "A" 10;
    case "zero-width string truncation still scans" "%0ts" "\"AB\"" "" 8;
    case "negative string truncation still scans" "%*ts" "-1,\"AB\"" "" 8;
    case "negative string width does not imply left alignment" "%*s" "-1,\"AB\""
      "AB" 9;
    case "string precision is consumed but ignored" "%.*s" "1,\"AB\"" "AB" 10;
    case "dynamic left string alignment" "%-*s" "5,\"AB\"" "AB   " 15;
    case "string truncation permits padding" "%4ts" "\"AB\"" "  AB" 14;
    case "empty string padding" "%5s" "\"\"" "     " 10;
    case "binary format and string padding" "\128%5s" "\"\\xff\"" "\128    \255"
      14;
    case "packed character right alignment" "%5c" "65" "    A" 11;
    case "packed character left alignment" "%-5c" "0x4241" "AB   " 13;
    case "packed zero flag still pads spaces" "%05c" "65" "    A" 12;
    case "full packed word truncation" "%1tc" "'ABCDEFGH'" "A" 14;
    case "zero-width packed truncation" "%0tc" "'ABCDEFGH'" "" 13;
    case "negative packed truncation" "%*tc" "-3,'ABCDEFGH'" "" 13;
    case "empty packed word padding" "%8c" "0" "        " 13;
    case "packed width does not truncate by itself" "%1c" "'ABCDEFGH'"
      "ABCDEFGH" 20;
    case "packed precision is ignored" "%5.1c" "0x4241" "   AB" 14;
    case "packed binary truncation" "%2tc" "0x0081fe" "\254\129" 10;
  ]

let all = numbers @ strings

let quota_cases =
  List.filter
    (fun case ->
      List.mem case.label
        [
          "unsigned maximum";
          "four-bit binary groups";
          "grouped zero padding starts with comma";
          "dynamic zero-padded hexadecimal";
          "string right alignment";
          "string truncation retains prefix";
          "zero-width string truncation still scans";
          "packed character right alignment";
          "zero-width packed truncation";
        ])
    all

let quote bytes =
  let out = Buffer.create (String.length bytes + 2) in
  Buffer.add_char out '"';
  String.iter
    (function
      | '"' -> Buffer.add_string out "\\\""
      | '\\' -> Buffer.add_string out "\\\\"
      | '$' -> Buffer.add_string out "$$"
      | '\n' -> Buffer.add_string out "\\n"
      | byte when Char.code byte < 32 || Char.code byte > 126 ->
          Buffer.add_string out (Printf.sprintf "\\x%02X" (Char.code byte))
      | byte -> Buffer.add_char out byte)
    bytes;
  Buffer.add_char out '"';
  Buffer.contents out

let call provider case =
  provider ^ "(" ^ quote case.format
  ^ (if case.arguments = "" then "" else "," ^ case.arguments)
  ^ ");"

let source case = "extern U0 Print(U8 *fmt,...);" ^ call "Print" case ^ "42;"

let argument_effects =
  [
    ( "width precision and value preserve right-to-left effects",
      "extern U0 Print(U8 *fmt,...);I64 N=0;I64 Next(){return \
       ++N;}Print(\"%*.*d\",Next(),Next(),Next());N+39;",
      "  1",
      9 );
    ( "argument call mutates the live format",
      "extern U0 Print(U8 *fmt,...);U8 Pattern[5]=\"%04q\";I64 \
       V(){Pattern[3]='X';return 42;}Print(Pattern,V());42;",
      "002A",
      9 );
    ( "width string capture survives pointer retargeting",
      "extern U0 Print(U8 *fmt,...);I64 F(){U8 A[3];A[0]=65;A[1]=66;A[2]=0;U8 \
       *p=A;Print(\"%*ts%s\",1,p=&A[1],p);return 42;}F();",
      "BAB",
      16 );
  ]

let invalid_fields =
  [
    ("missing bare string argument", "Print(\"%s\");", 2, "HCIRVM0025");
    ("missing bare packed argument", "Print(\"%c\");", 2, "HCIRVM0025");
    ("word supplied to bare string", "Print(\"%s\",42);", 2, "HCIRVM0025");
    ("pointer supplied to bare packed", "Print(\"%c\",\"AB\");", 2, "HCIRVM0025");
    ( "missing width precedes unknown directive",
      "Print(\"%*j\");",
      2,
      "HCIRVM0025" );
    ( "pointer width precedes unknown directive",
      "Print(\"%*j\",\"x\");",
      2,
      "HCIRVM0025" );
    ( "literal width followed by pointer override",
      "Print(\"%2*d\",\"x\",42);",
      3,
      "HCIRVM0025" );
    ("missing precision precedes directive", "Print(\"%.*d\");", 3, "HCIRVM0025");
    ( "precision consumes its word before missing value",
      "Print(\"%.*d\",0);",
      4,
      "HCIRVM0025" );
    ( "pointer precision precedes unknown directive",
      "Print(\"%.*j\",\"x\");",
      3,
      "HCIRVM0025" );
    ("width followed by end of format", "Print(\"%5\");", 3, "HCIRVM0024");
    ("precision override followed by end", "Print(\"%.*\",0);", 4, "HCIRVM0024");
    ( "literal width overflow",
      "Print(\"%9223372036854775808d\",42);",
      20,
      "HCIRVM0024" );
    ( "literal precision overflow",
      "Print(\"%.9223372036854775808d\",42);",
      21,
      "HCIRVM0024" );
    ( "overflowing literal precedes star override",
      "Print(\"%9223372036854775808*d\",5,42);",
      20,
      "HCIRVM0024" );
    ("minus cannot follow zero flag", "Print(\"%0-5d\",42);", 3, "HCIRVM0024");
  ]

(* With one byte available, the second append fails after its own visit/read.
   Measuring the whole payload first would change these fault-work counts. *)
let interleaved_faults =
  [
    ("bare packed visits interleave with appends", "%c", "'ABCDEFGH'", 6);
    ("zero-width packed visits interleave", "%0c", "'ABCDEFGH'", 7);
    ("negative-width packed visits interleave", "%*c", "-1,'ABCDEFGH'", 7);
    ("ignored packed precision keeps visit order", "%.*c", "1,'ABCDEFGH'", 8);
    ("bare string reads interleave with appends", "%s", "\"AB\"", 6);
    ("zero-width string reads interleave", "%0s", "\"AB\"", 7);
    ("negative-width string reads interleave", "%*s", "-1,\"AB\"", 7);
    ("ignored string precision keeps read order", "%.*s", "1,\"AB\"", 8);
  ]
