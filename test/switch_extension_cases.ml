(* Compiler/PrsStmt.HC:578-789 is the source of the switch rules exercised here.
   Brackets select IC_NOBOUND_SWITCH; an out-of-table selector is an execution
   fault. Repeated defaults define the same dispatch label, so its last source
   placement wins. A selected case calls each active start: prefix in outer-to-
   inner order. A break inside a sub-switch lands at that region's end: suffix.
   Each expected result is calculated from the HolyC source below. *)

type value = { label : string; source : string; expected : int64 }

let value label expected source = { label; expected; source }

type rejection = { label : string; source : string; code : string }

let reject label code source = { label; code; source }

let bracket_values =
  [
    value "bracket dispatch selects a case" 42L
      {|I64 F(I64 n) {
  switch [n] {
    case 1: return 7;
    case 3: return 42;
    default: return -1;
  }
  return -2;
}
F(3);|};
    value "bracket in-range table hole selects explicit default" 42L
      {|I64 F(I64 n) {
  switch [n] {
    case 1: return 7;
    case 3: return 9;
    default: return 42;
  }
  return -1;
}
F(2);|};
    value "bracket in-range hole without default exits switch" 42L
      {|I64 F(I64 n) {
  I64 result=40;
  switch [n] {
    case 1: result=0; break;
    case 3: result=1; break;
  }
  return result+2;
}
F(2);|};
    value "bracket selector updates exactly once" 42L
      {|I64 F() {
  I64 n=0;
  switch [++n] {
    case 1: n+=41; break;
    default: return -1;
  }
  return n;
}
F();|};
    value "uncalled bracket switch does not execute its selector" 42L
      {|I64 Never(I64 n) {
  switch [n] {
    case 1: return 7;
    case 3: return 9;
    default: return -1;
  }
  return -2;
}
42;|};
    value "bounded switch keeps its out-of-range default" 42L
      {|I64 F() {
  switch (100) {
    case 1: return 7;
    case 3: return 9;
    default: return 42;
  }
  return -1;
}
F();|};
    value "case source sizeof and defined queries select original values" 42L
      {|I64 Known;
I64 F() {
  switch (7) {
    case sizeof(I64)-defined(Known): return 42;
    default: return -1;
  }
  return -2;
}
F();|};
  ]

let repeated_default_values =
  [
    value "bounded repeated default selects last definition" 42L
      {|I64 F() {
  switch (2) {
    case 1: return 1;
    default: return 5;
    case 3: return 3;
    default: return 42;
  }
  return -1;
}
F();|};
    value "bracket repeated default selects last definition" 42L
      {|I64 F(I64 n) {
  switch [n] {
    case 1: return 7;
    default: return 5;
    case 3: return 9;
    default: return 42;
  }
  return -1;
}
F(2);|};
    value "case table still targets its case with repeated defaults" 42L
      {|I64 F() {
  switch (2) {
    default: return 5;
    case 2: return 42;
    default: return -1;
  }
  return -2;
}
F();|};
  ]

(* TempleOS only builds a start: continuation when its source statements
   actually append intermediate code. A language label emits IC_LABEL even
   without an expression, while ; and empty braces emit none. *)
let empty_prefix_values =
  [
    value "semicolon-only setup emits no local continuation" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      ;
    case 1: result+=30; break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "empty-block setup emits no local continuation" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      {}
    case 1: result+=30; break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "nested empty-block setup emits no local continuation" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      { {}; { ; } }
    case 1: result+=30; break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "language label inside setup still emits local continuation" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
    kept:
      ;
    case 1: result+=30; break;
    end:
      result+=12;
  }
  return result;
}
F();|};
  ]

(* Direct case entry and fallthrough have deliberately different expected
   values. Each traverses the prefix once, then the region's end: suffix. *)
let subswitch_values =
  [
    value "direct region entry calls its prefix once" 42L
      {|I64 F(I64 n) {
  I64 result=0;
  switch (n) {
    case 0: result=1;
    start:
      result+=10;
    case 1: result+=20; break;
    end:
      result+=12;
      break;
    default: return -1;
  }
  return result;
}
F(1);|};
    value "fallthrough into region calls its prefix once" 43L
      {|I64 F(I64 n) {
  I64 result=0;
  switch (n) {
    case 0: result=1;
    start:
      result+=10;
    case 1: result+=20; break;
    end:
      result+=12;
      break;
    default: return -1;
  }
  return result;
}
F(0);|};
    value "fallthrough between cases in one region skips the prefix" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      result+=10;
    case 1:
      result+=1;
    case 2:
      result+=20;
      break;
    end:
      result+=11;
  }
  return result;
}
F();|};
    value "nested prefixes run outer then inner and both suffixes" 12345L
      {|I64 F() {
  I64 result=0;
  switch (3) {
    start:
      result=result*10+1;
    start:
      result=result*10+2;
    case 3:
      result=result*10+3;
      break;
    end:
      result=result*10+4;
    end:
      result=result*10+5;
      break;
  }
  return result;
}
F();|};
    value "inner break enters its suffix before the outer suffix" 42L
      {|I64 F() {
  I64 result=0;
  switch (3) {
    start:
      result+=8;
    start:
      result+=9;
    case 3:
      result+=10;
      break;
    end:
      result+=7;
    end:
      result+=8;
      break;
  }
  return result;
}
F();|};
    value "sub-switch default enters the prefix and suffix" 42L
      {|I64 F() {
  I64 result=0;
  switch (8) {
    start:
      result+=10;
    case 4: return -1;
    default: result+=20; break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "reversed range advances the next implicit case" 42L
      {|I64 F() {
  I64 result=0;
  switch (6) {
    start:
      result+=10;
    case 5...3: return -1;
    case:
      result+=20;
      break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "empty region before first label stays outside case dispatch" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      result+=100;
    end:
      result+=200;
    case 1:
      result+=42;
      break;
  }
  return result;
}
F();|};
  ]
  @ empty_prefix_values

let nested_control_values =
  [
    value "ordinary inner switch break stays inside the prefix" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      switch (2) {
        case 2: result+=10; break;
        default: result=100;
      }
    case 1: result+=20; break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "inner while break is valid in the prefix" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      while (1) {
        result+=30;
        break;
      }
      result+=10;
    case 1: result+=2; break;
    end:
  }
  return result;
}
F();|};
    value "goto within selected case retains region suffix" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      result+=10;
    case 1:
      goto done;
      result=100;
    done:
      result+=20;
      break;
    end:
      result+=12;
  }
  return result;
}
F();|};
  ]

let goto_values =
  [
    value "goto within a setup prefix retains its continuation" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      goto resume;
      result=100;
    resume:
      result+=10;
    case 1:
      result+=20;
      break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "goto between case bodies preserves the empty prefix stack" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      result+=10;
    case 1:
      goto second;
    end:
      result=100;
    start:
      result+=90;
    case 2:
    second:
      result+=20;
      break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "nested switch can goto a label in its enclosing prefix" 42L
      {|I64 F() {
  I64 result=0;
  switch (1) {
    start:
      switch (3) {
        case 3: goto resume;
        default: result=100;
      }
      result=100;
    resume:
      result+=10;
    case 1:
      result+=20;
      break;
    end:
      result+=12;
  }
  return result;
}
F();|};
    value "same label spelling belongs to each function separately" 42L
      {|I64 A() {
  I64 result=0;
  switch (1) {
    start:
      goto resume;
    resume:
      result+=10;
    case 1:
      result+=20;
      break;
    end:
      result+=12;
  }
  return result;
}
I64 B() {
  goto resume;
resume:
  return 42;
}
A()+B()-42;|};
  ]

let values =
  bracket_values @ repeated_default_values @ subswitch_values
  @ nested_control_values @ goto_values

let goto_rejections =
  [
    reject "goto from setup prefix into its case body" "HCRUN0001"
      {|I64 F() {
  switch (1) {
    start:
      goto body;
    case 1:
    body:
      break;
    end:
      ;
  }
  return 42;
}
F();|};
    reject "goto from case body into its setup prefix" "HCRUN0001"
      {|I64 F() {
  switch (1) {
    start:
    setup:
      ;
    case 1:
      goto setup;
    end:
      ;
  }
  return 42;
}
F();|};
    reject "goto between sibling setup prefixes" "HCRUN0001"
      {|I64 F() {
  switch (2) {
    start:
    first:
      ;
    case 1:
      break;
    end:
      ;
    start:
      goto first;
    case 2:
      break;
    end:
      ;
  }
  return 42;
}
F();|};
    reject "goto from inner setup prefix into its outer prefix" "HCRUN0001"
      {|I64 F() {
  switch (1) {
    start:
    outer:
      ;
    start:
      goto outer;
    case 1:
      break;
    end:
      ;
    end:
      ;
  }
  return 42;
}
F();|};
    reject "goto from outer prefix into nested setup prefix" "HCRUN0001"
      {|I64 F() {
  switch (1) {
    start:
      goto inner;
    start:
    inner:
      ;
    case 1:
      break;
    end:
      ;
    end:
      ;
  }
  return 42;
}
F();|};
  ]

(* These are parser-phase rejections even in an uncalled function. The later
   case expression must not be evaluated after the invalid front-porch break. *)
let parser_rejections =
  [
    reject "front-porch break precedes a faulting case value" "HCPARSE0170"
      {|I64 Unused(I64 n) {
  switch (n) {
    start:
      break;
    case 1/0: return 3;
    end:
      return 0;
  }
}
42;|};
    reject "front-porch break precedes effectful case value" "HCPARSE0170"
      {|I64 Unused(I64 n) {
  switch (n) {
    start:
      break;
    case n++: return 3;
    end:
      return 0;
  }
}
42;|};
    reject "nested front-porch break rejects at inner region" "HCPARSE0170"
      {|I64 Unused() {
  switch (1) {
    start:
    start:
      break;
    case 1: return 7;
    end:
    end:
      return 0;
  }
}
42;|};
  ]

(* IC_NOBOUND_SWITCH omits the original dispatch bounds guard. The hosted
   runtime reports HCSW0005 for a reached out-of-table selector. *)
let runtime_rejections =
  [
    reject "bracket selector below the first case" "HCSW0005"
      {|I64 F(I64 n) {
  switch [n] {
    case 1: return 7;
    case 3: return 9;
    default: return 42;
  }
  return -1;
}
F(-1);|};
    reject "bracket selector above the last case" "HCSW0005"
      {|I64 F(I64 n) {
  switch [n] {
    case 1: return 7;
    case 3: return 9;
    default: return 42;
  }
  return -1;
}
F(100);|};
  ]

(* Effectful switch endpoints run during original JIT/current-task source
   preparation, including in a function whose body is never called. Ordinary
   AOT nonconstant endpoints require a source image and still reject. *)
let jit_effectful_values =
  [
    value "uncalled function prepares effectful cases in source order" 12L
      {|I64 counter=0;
I64 CaseValue(I64 n) { counter=counter*10+n; return n; }
I64 Unused(I64 n) {
  switch (n) {
    case CaseValue(1): return 0;
    case CaseValue(2): return 0;
  }
  return 0;
}
counter;|};
    value "effectful reversed endpoints precede implicit numbering" 42L
      {|I64 counter=0;
I64 CaseValue(I64 n) { counter=counter*10+n; return n; }
I64 F(I64 n) {
  switch (n) {
    case CaseValue(5)...CaseValue(3): return 0;
    case: return counter-11;
    default: return -1;
  }
  return -2;
}
F(6);|};
  ]

(* The original second endpoint faults after the first case value has run,
   so the third case is not reached during source preparation. *)
let jit_effectful_rejections =
  [
    reject "case preparation stops at an intervening arithmetic failure"
      "HCSEMA0004"
      {|I64 counter=0;
I64 CaseValue(I64 n) { counter=counter*10+n; return n; }
I64 Unused(I64 n) {
  switch (n) {
    case CaseValue(1): return 7;
    case 1/0: return 9;
    case CaseValue(2): return 11;
  }
  return 0;
}
counter;|};
  ]

(* StreamExePrint must enter the child source while the enclosing #exe is
   active. The first child case is closed (2+3), while ++N is evaluated at the
   reached case endpoint. The increment makes N+Mark equal 13, so the parent
   switch returns 42; replacing ++N with 6 instead generates -1. *)
let nested_source_switch ?(first_case = "2+3") ?(second_case = "++N")
    ?(markers = false) () =
  let marker text = if markers then Printf.sprintf "Print(%S);" text else "" in
  let child =
    Printf.sprintf
      "%sI64 N=5;I64 Mark=0;%sswitch(6){case %s:Mark=9;break;case \
       %s:Mark=7;break;default:Mark=0;}%sN+Mark;"
      (marker "entered;") (marker "before;") first_case second_case
      (marker "after;")
  in
  Printf.sprintf
    {|#exe {
  I64 Pick(I64 value) {
    switch (value) {
      case StreamExePrint(%S): return 42;
      default: return -1;
    }
  }
  StreamPrint("%%d;", Pick(13));
}|}
    child
