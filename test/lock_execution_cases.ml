type success = {
  name : string;
  program : string;
  value : int64;
  output : string;
}

type failure = {
  name : string;
  program : string;
  code : string;
  output : string;
}

let successful ?(output = "") name program value =
  ({ name; program; value; output } : success)

let failure ?(output = "") name program code =
  ({ name; program; code; output } : failure)

let print_header = {|extern U0 Print(U8 *fmt,...);|}

let bit_headers =
  {|public _intern 0x77 Bool Bt(U8 *p,I64 n);public _intern 0x78 Bool Bts(U8 *p,I64 n);public _intern 0x79 Bool Btr(U8 *p,I64 n);public _intern 0x7a Bool Btc(U8 *p,I64 n);public _intern 0x7b Bool LBts(U8 *p,I64 n);public _intern 0x7c Bool LBtr(U8 *p,I64 n);public _intern 0x7d Bool LBtc(U8 *p,I64 n);|}

let widths =
  [
    ("Bool", 8, true);
    ("I8", 8, true);
    ("U8", 8, false);
    ("I16", 16, true);
    ("U16", 16, false);
    ("I32", 32, true);
    ("U32", 32, false);
    ("I64", 64, true);
    ("U64", 64, false);
  ]

let word value = Printf.sprintf "0x%Lx" value

let bounds bits signed =
  if signed then
    let low = Int64.shift_left 1L (bits - 1) in
    let low = if bits = 64 then low else Int64.neg low in
    (low, Int64.pred (Int64.neg low))
  else (0L, if bits = 64 then -1L else Int64.pred (Int64.shift_left 1L bits))

let width_cases =
  List.concat_map
    (fun (type_, bits, signed) ->
      let low, high = bounds bits signed in
      let compounds =
        [
          ("+=", high, "1", low);
          ("-=", low, "1", high);
          ("&=", -1L, "42", 42L);
          ("|=", 40L, "2", 42L);
          ("^=", 43L, "1", 42L);
        ]
        |> List.map (fun (op, initial, rhs, stored) ->
            successful
              (type_ ^ " locked " ^ op ^ " reloads its stored width")
              (Printf.sprintf
                 "I64 F(){%s n=%s;I64 r=0;lock \
                  r=(n%s%s);if(r!=%s||n!=%s)return 0;return 42;}F();"
                 type_ (word initial) op rhs (word stored) (word stored))
              42L)
      in
      let increments =
        [
          ("prefix increment", high, "++n", low, low);
          ("postfix increment", high, "n++", high, low);
          ("prefix decrement", low, "--n", high, high);
          ("postfix decrement", low, "n--", low, high);
        ]
        |> List.map (fun (name, initial, update, result, stored) ->
            successful
              (type_ ^ " locked " ^ name)
              (Printf.sprintf
                 "I64 F(){%s n=%s;I64 r=0;lock r=%s;if(r!=%s||n!=%s)return \
                  0;return 42;}F();"
                 type_ (word initial) update (word result) (word stored))
              42L)
      in
      let owners =
        [
          successful
            (type_ ^ " copied local pointer alias")
            (Printf.sprintf
               "I64 F(){%s n=40;%s *p=&n,*q=p;lock *q+=2;return n;}F();" type_
               type_)
            42L;
          successful
            (type_ ^ " indexed update preserves neighboring cells")
            (Printf.sprintf
               "I64 F(){%s a[3];a[0]=11;a[1]=40;a[2]=13;lock \
                a[1]+=2;if(a[0]!=11||a[2]!=13)return 0;return a[1];}F();"
               type_)
            42L;
          successful
            (type_ ^ " persistent global mutation")
            (Printf.sprintf "%s G=40;I64 F(){lock G+=2;return G;}F();" type_)
            42L;
          successful
            (type_ ^ " static mutation survives later invocation")
            (Printf.sprintf "I64 F(){static %s n=40;lock n++;return n;}F();F();"
               type_)
            42L;
          successful
            (type_ ^ " borrowed parameter storage")
            (Printf.sprintf
               "U0 Bump(%s *p){lock *p+=2;}I64 F(%s n){Bump(&n);return \
                n;}F(40);"
               type_ type_)
            42L;
        ]
      in
      compounds @ increments @ owners)
    widths

let context_cases =
  [
    successful "top-level lock command" "I64 G=40;lock G+=2;G;" 42L;
    successful "one statement ends lock before following multiply"
      "I64 G=20;lock G++;G*=2;G;" 42L;
    successful "comma-linked statement stays in lock" "I64 G=40;lock G++,G++;G;"
      42L;
    successful "nested lock regions retain the outer scope"
      "I64 G=38;lock {G++;lock {++G;}G+=2;}G;" 42L;
    successful "lock does not extend into called function body"
      "I64 G=20;U0 Double(){G*=2;}lock Double();G+=2;G;" 42L;
    successful "rhs call completes before the atomic destination read"
      "I64 G=1;I64 Set(){G=20;return 22;}lock G+=Set();G;" 42L;
    successful "rhs assignment precedes the compound read"
      "I64 G=1;lock G+=(G=2);G;" 4L;
    successful "rhs postfix update completes before compound mutation"
      "I64 G=1;lock G+=G++;G;" 3L;
    successful "rhs prefix update completes before compound mutation"
      "I64 G=1;lock G+=++G;G;" 4L;
    successful "captured pointer destination survives rhs rebinding"
      "I64 F(){I64 n=20,m=22;I64 *p=&n;lock *p+=*(p=&m);return n;}F();" 42L;
    successful "indexed destination is captured once before rhs call"
      "I64 Bump(I64 *p){*p+=1;return *p;}I64 F(){I64 a[2];I64 \
       i=0;a[0]=20;a[1]=99;lock a[i++]+=Bump(&a[0]);if(i!=1||a[1]!=99)return \
       0;return a[0];}F();"
      42L;
    successful "loop control admits nested locked statements"
      "I64 F(){I64 n=0;lock while(n<42){++n;}return n;}F();" 42L;
    successful "conditional lock does not execute the skipped branch"
      "I64 F(){I64 n=40;lock if(n==40){n+=2;}else {n-=2;}return n;}F();" 42L;
    successful "locked AND condition uses the original test branch"
      "I64 F(I64 n){I64 result=40;lock if(n&1){result+=2;}return result;}F(3);"
      42L;
    successful "locked AND condition preserves the zero branch"
      "I64 F(I64 n){I64 result=42;lock if(n&1){result-=2;}return result;}F(2);"
      42L;
    successful "locked AND loop tests two runtime operands"
      "I64 F(I64 n,I64 mask){I64 result=38;lock \
       while(n&mask){n--;result++;}return result;}F(7,4);"
      42L;
    successful "break exits a locked loop"
      "I64 F(){I64 n=40;lock while(1){n++;if(n==42)break;}return n;}F();" 42L;
    successful "lock return preserves its original operand result"
      "I64 F(){I64 n=41;lock return ++n;}F();" 42L;
    successful "lock switch retains atomic updates in selected arms"
      "I64 F(I64 value){I64 n=40;lock switch(value){case \
       1:n+=2;break;default:n=0;}return n;}F(1);"
      42L;
    successful "goto can enter and leave a lexical lock body"
      "I64 F(){I64 n=40;goto inside;lock {inside:n++;goto \
       outside;}outside:n++;return n;}F();"
      42L;
    successful "borrowed pointer remains valid across recursive calls"
      "U0 Add(I64 *p,I64 n){if(n)Add(p,n-1);lock (*p)++;}I64 F(){I64 \
       n=38;Add(&n,3);return n;}F();"
      42L;
    successful "aligned subword alias changes only its original bytes"
      "I64 F(){U64 n=0x112233445566ffff;U16 *p=(&n)(U16*);lock ++*p;return \
       n==0x1122334455660000;}F();"
      1L;
    successful "locked U0 helper completes through ordinary call control"
      "I64 G=40;U0 Bump(){lock {G++;G++;}}Bump();G;" 42L;
    successful ~output:"rhs;42;" "rhs output is reached exactly once"
      (print_header
     ^ "I64 G=40;I64 Right(){Print(\"rhs;\");return 2;}I64 F(){lock \
        G+=Right();Print(\"%d;\",G);return G;}F();")
      42L;
  ]

let bit_cases =
  let rows =
    [
      ("Bts", 0L, 0L, 2L);
      ("Bts", 2L, 1L, 2L);
      ("Btr", 2L, 1L, 0L);
      ("Btr", 0L, 0L, 0L);
      ("Btc", 0L, 0L, 2L);
      ("Btc", 2L, 1L, 0L);
    ]
    |> List.concat_map (fun (operation, initial, prior, stored) ->
        [ ("lock ", operation); ("", "L" ^ operation) ]
        |> List.map (fun (prefix, name) ->
            successful
              (Printf.sprintf "%s%s returns prior bit %Ld" prefix name prior)
              (bit_headers
              ^ Printf.sprintf
                  "I64 F(){I64 n=%Ld;I64 r=0;%sr=%s(&n,1);if(r!=%Ld)return \
                   99;return n;}F();"
                  initial prefix name prior)
              stored))
  in
  let storage =
    List.concat_map
      (fun (type_, bits, signed) ->
        let high = Int64.shift_left 1L (bits - 1) in
        let stored = if signed && bits < 64 then Int64.neg high else high in
        [
          successful
            (type_ ^ " locked high bit preserves source width")
            (bit_headers
            ^ Printf.sprintf "I64 F(){%s n=0;lock Bts(&n,%d);return n;}F();"
                type_ (bits - 1))
            stored;
          successful
            (type_ ^ " explicit locked bit clears its high bit")
            (bit_headers
            ^ Printf.sprintf
                "I64 F(){%s n=%s;I64 old=LBtr(&n,%d);if(old!=1)return \
                 99;return n;}F();"
                type_ (word high) (bits - 1))
            0L;
        ])
      widths
  in
  rows @ storage
  @ [
      successful "plain Bt remains a read inside lock"
        (bit_headers
       ^ "I64 F(){I64 n=2;I64 r=0;lock r=Bt(&n,1);return r*100+n;}F();")
        102L;
      successful "locked bit selects a byte across a word boundary"
        (bit_headers
       ^ "I64 F(){U8 n[9];I64 i;for(i=0;i<9;i++)n[i]=0;lock Bts(n,64);return \
          n[8];}F();")
        1L;
      successful "locked bit rhs precedes address selection"
        (bit_headers
       ^ "I64 N=0;I64 Index(){N++;return 1;}I64 F(){I64 \
          n[3];n[0]=0;n[1]=2;n[2]=0;I64 r=0;lock r=Btc(&n[N++],Index());return \
          r*100+N*10+n[1];}F();")
        120L;
      successful "explicit locked bit operation retains a borrowed alias"
        (bit_headers
       ^ "I64 Set(I64 *p){I64 *q=p;return LBts(q,5);}I64 F(){I64 \
          n=0;Set(&n);return n;}F();")
        32L;
      successful "numeric locked bit binding permits a different source name"
        "public _intern 0x7d Bool Toggle(U8 *p,I64 n);I64 F(){I64 n=2;return \
         Toggle(&n,1)*100+n;}F();"
        100L;
      successful "ordinary LBtc spelling retains its ordinary source body"
        "Bool LBtc(I64 *p,I64 bit){*p=42;return 7;}I64 F(){I64 n=0;return \
         LBtc(&n,1)*100+n;}F();"
        742L;
    ]

(* BackA.HC's integer multiply/divide/modulus and shift branches do not
   consume ICF_LOCK. OptPass012.HC rewrites a constant power-of-two %= to
   IC_AND_EQU before that selection, so that form does consume it. *)
let ordinary_locked_cases =
  [
    successful "locked multiply keeps its ordinary integer operation"
      "I64 F(){I64 n=14,d=3;lock n*=d;return n;}F();" 42L;
    successful "locked divide keeps its ordinary integer operation"
      "I64 F(){I64 n=84,d=2;lock n/=d;return n;}F();" 42L;
    successful "locked modulus keeps its ordinary integer operation"
      "I64 F(){I64 n=85,d=43;lock n%=d;return n;}F();" 42L;
    successful "locked left shift keeps its ordinary integer operation"
      "I64 F(){I64 n=21,d=1;lock n<<=d;return n;}F();" 42L;
    successful "locked right shift keeps its ordinary integer operation"
      "I64 F(){I64 n=84,d=1;lock n>>=d;return n;}F();" 42L;
    successful "ordinary locked narrow multiply returns its full computation"
      "I64 F(){U8 n=200;I64 r=0,d=2;lock r=(n*=d);return r*1000+n;}F();" 400144L;
    successful "ordinary locked narrow shift returns its full computation"
      "I64 F(){U8 n=42;I64 r=0;lock r=(n<<=8);return r*1000+n;}F();" 10752000L;
    successful "locked power-of-two modulus uses the original AND rewrite"
      "I64 F(){I64 n=-3;lock n%=8;return n;}F();" 5L;
    successful "locked non-power-of-two modulus remains signed remainder"
      "I64 F(){I64 n=-3;lock n%=7;return n;}F();" (-3L);
    successful "folded constant rhs needs no invalid register prefix"
      "I64 F(){I64 n=40;lock n+=1+1;return n;}F();" 42L;
  ]

(* OptPass3.HC emits IC_ADD_CONST for either immediate ADD operand and
   IC_SUB_CONST for a right immediate SUB operand. BackA.HC ignores LOCK
   for these forms; the normalized addition delta must fit signed 32 bits. *)
let immediate_cases =
  [
    successful "locked scalar addition uses its safe immediate form"
      "I64 F(I64 n){lock n=n+1;return n;}F(41);" 42L;
    successful "locked commuted addition uses its safe immediate form"
      "I64 F(I64 n){lock n=1+n;return n;}F(41);" 42L;
    successful "locked scalar subtraction uses its safe immediate form"
      "I64 F(I64 n){lock n=n-1;return n;}F(43);" 42L;
    successful "locked immediate addition promotes an unsigned byte"
      "I64 F(U8 n){I64 r=0;lock r=n+1;if(n!=255)return 0;return r;}F(255);" 256L;
    successful "locked immediate addition promotes a signed Bool byte"
      "I64 F(Bool n){I64 r=0;lock r=n+1;if(n!=127)return 0;return r;}F(127);"
      128L;
    successful "locked immediate addition promotes an unsigned dword"
      "I64 F(U32 n){I64 r=0;lock r=n+1;if(n!=0xffffffff)return 0;return \
       r;}F(0xffffffff);"
      4294967296L;
    successful "locked immediate addition admits the positive delta edge"
      "I64 F(I64 n){lock n=n+2147483647;return n;}F(0);" 2147483647L;
    successful "locked immediate addition admits the negative delta edge"
      "I64 F(I64 n){lock n=n+(-2147483648);return n;}F(0);" (-2147483648L);
    successful "locked immediate subtraction admits the negative delta edge"
      "I64 F(I64 n){lock n=n-2147483648;return n;}F(0);" (-2147483648L);
    successful "locked immediate subtraction admits the positive delta edge"
      "I64 F(I64 n){lock n=n-(-2147483647);return n;}F(0);" 2147483647L;
  ]

(* PrsAddOp and ICPreIncDec/ICPostIncDec scale one-star callbacks by
   the eight-byte RT_PTR size. Other callback compounds use signed word bits. *)
let callback_cases =
  [
    successful "locked numeric callback prefix uses an eight-byte stride"
      "I64 F(){I64 (*p)(I64 n);p=34;I64 r=0;lock r=++p;if(p!=42)return \
       0;return r;}F();"
      42L;
    successful "locked numeric callback postfix keeps the prior word"
      "I64 F(){I64 (*p)(I64 n);p=34;I64 r=0;lock r=p++;if(p!=42)return \
       0;return r;}F();"
      34L;
    successful "locked indexed callback decrement preserves adjacent cells"
      "I64 (*P)(I64 n)[2][2];I64 F(){P[0][0]=77;P[1][1]=50;I64 old=0;lock \
       old=P[1][1]--;if(P[0][0]!=77)return 0;return \
       old*100+(P[1][1]==42);}F();"
      5001L;
    successful "locked callback addition scales a runtime unsigned rhs"
      "I64 F(U64 amount){I64 (*p)(I64 n);p=26;I64 r=0;lock \
       r=(p+=amount);if(p!=42)return 0;return r;}F(2);"
      42L;
    successful "locked static callback subtraction uses the pointer stride"
      "I64 F(){static I64 (*p)(I64 n)[2];p[1]=58;I64 r=0;lock \
       r=(p[1]-=2);if(p[1]!=42)return 0;return r;}F();"
      42L;
    successful "locked numeric callback bitwise compounds use unscaled words"
      "I64 F(){I64 (*p)(I64 n);p=85;lock {p&=63;p|=32;p^=31;}if(p!=42)return \
       0;return 42;}F();"
      42L;
    successful "locked fixed callback parameter retains its numeric owner"
      "I64 F(I64 (*p)(I64 n)){lock p+=2;if(p!=42)return 0;return 42;}F(26);" 42L;
  ]

let success_cases =
  width_cases @ context_cases @ bit_cases @ ordinary_locked_cases
  @ immediate_cases @ callback_cases

let invalid_cases =
  List.map
    (fun op ->
      failure
        ("lock rejects a residual scalar " ^ op)
        (print_header
        ^ Printf.sprintf
            "I64 F(){I64 n=42,d=3;Print(\"bad;\");lock n=n%sd;return n;}F();" op
        )
        "HCLOCK0001")
    [ "+"; "-"; "&"; "|"; "^" ]
  @ List.map
      (fun (name, expression) ->
        failure name
          (print_header
          ^ Printf.sprintf
              "I64 F(I64 n){Print(\"bad;\");lock n=%s;return n;}F(0);"
              expression)
          "HCLOCK0001")
      [
        ("locked scalar ADD above the immediate delta range", "n+2147483648");
        ("locked scalar ADD below the immediate delta range", "n+(-2147483649)");
        ("locked scalar SUB below the immediate delta range", "n-2147483649");
        ("locked scalar SUB above the immediate delta range", "n-(-2147483648)");
        ("locked scalar SUB cannot commute an immediate left operand", "1-n");
      ]

let invalid_bit_signatures =
  List.concat_map
    (fun (code, name) ->
      List.map
        (fun (result, pointer, index) ->
          Printf.sprintf
            "public _intern 0x%x %s %s(%s p,%s n);I64 F(){I64 \
             q=0;Print(\"bad;\");%s(&q,0);return q;}F();"
            code result name pointer index name
          |> fun source -> print_header ^ source)
        [
          ("I64", "U8 *", "I64");
          ("Bool", "I64 *", "I64");
          ("Bool", "U8 *", "U64");
        ])
    [ (0x7b, "LBts"); (0x7c, "LBtr"); (0x7d, "LBtc") ]

let fault_cases =
  let fault name source code =
    failure ~output:"before;" name (print_header ^ source) code
  in
  [
    fault "locked local read requires initialized bytes"
      "I64 F(){I64 n;Print(\"before;\");lock n++;Print(\"after;\");return \
       n;}F();"
      "HCIRVM0012";
    fault "locked pointer read requires initialized storage"
      "I64 F(){U8 n[2];n[0]=42;Print(\"before;\");lock \
       n[1]+=1;Print(\"after;\");return n[0];}F();"
      "HCIRVM0012";
    fault "locked one-past array update keeps the bounds check"
      "I64 F(){I64 n[1];n[0]=42;I64 *p=&n[1];Print(\"before;\");lock \
       (*p)++;Print(\"after;\");return n[0];}F();"
      "HCIRVM0019";
    fault "locked narrow bitmap bounds are checked"
      (bit_headers
     ^ "I64 F(){Bool n=0;Print(\"before;\");lock \
        Btc(&n,8);Print(\"after;\");return n;}F();")
      "HCIRVM0019";
    fault "explicit locked bit rejects negative index"
      (bit_headers
     ^ "I64 F(){I64 \
        n=0;Print(\"before;\");LBts(&n,-1);Print(\"after;\");return n;}F();")
      "HCIRVM0019";
    fault "explicit locked bit requires initialized selected byte"
      (bit_headers
     ^ "I64 F(){U8 \
        n[2];n[0]=0;Print(\"before;\");LBtr(n,8);Print(\"after;\");return \
        n[0];}F();")
      "HCIRVM0012";
    fault "misaligned locked natural-width view is rejected"
      "I64 F(){U64 n=0;U8 *bytes=(&n)(U8*);U16 \
       *p=(&bytes[1])(U16*);Print(\"before;\");lock \
       *p+=1;Print(\"after;\");return n;}F();"
      "HCLOCK0002";
    failure ~output:"before;rhs;" "rhs effect survives rejected destination"
      (print_header
     ^ "I64 Right(){Print(\"rhs;\");return 2;}I64 F(){U8 \
        n[1];n[0]=42;Print(\"before;\");lock \
        n[1]+=Right();Print(\"after;\");return n[0];}F();")
      "HCIRVM0019";
    fault "locked callback update rejects an owned executable target"
      "I64 Target(I64 n){return n+2;}I64 F(){I64 (*p)(I64 \
       n);p=&Target;Print(\"before;\");lock p++;Print(\"after;\");return \
       42;}F();"
      "HCIRVM0024";
    failure ~output:"before;rhs;"
      "locked indexed callback checks its code owner after rhs effects"
      (print_header
     ^ "I64 Target(I64 n){return n+2;}I64 (*P)(I64 n)[2];I64 \
        Right(){Print(\"rhs;\");return 1;}I64 \
        F(){P[1]=&Target;Print(\"before;\");lock \
        P[1]+=Right();Print(\"after;\");return 42;}F();")
      "HCIRVM0024";
  ]

let quota_source =
  "I64 G=0;I64 F(){I64 n=0;lock while(n<21){G+=2;n++;}return G;}F();"

let retained_cases =
  [
    successful "later source fragments observe locked mutations"
      "I64 G=39;lock ++G;I64 Saved=G;I64 Add(){lock G+=2;return \
       G;}Add();Saved+G-40;"
      42L;
    successful "locked static initializer executes at its original declaration"
      "I64 G=40;I64 Next(){lock ++G;return G;}I64 F(){static I64 \
       Saved=Next();return Saved;}F();F();G+F()-40;"
      42L;
    successful "locked default preparation executes once"
      "I64 G=40;I64 Next(){lock ++G;return G;}I64 F(I64 n=Next()){return \
       n;}F();F();G+F()-40;"
      42L;
    successful "nested source command updates retained parent storage"
      (print_header
     ^ "#exe {I64 G=40;StreamExePrint(\"lock \
        G+=2;G;\");StreamPrint(\"%d;\",G);}")
      42L;
    successful "source directive emits the result of a locked update"
      (print_header ^ "#exe {I64 G=40;lock G+=2;StreamPrint(\"%d;\",G);}")
      42L;
  ]
