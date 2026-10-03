open Yojson.Safe.Util

let load name =
  [ "oracle/" ^ name; "../oracle/" ^ name; "test/oracle/" ^ name ]
  |> List.find_opt Sys.file_exists
  |> function
  | Some path -> Yojson.Safe.from_file path
  | None -> Alcotest.fail (name ^ " native fixture is missing")

let fixture () = load "division-strength-reductions.json"
let field json name = json |> member name |> to_string

let projections fixture =
  fixture |> member "hosted_value_projections" |> to_list

let observed fixture projection case_field =
  fixture |> member "checks" |> to_list
  |> List.find (fun check -> field check "id" = field projection case_field)
  |> member "observed_fields"
  |> member (field projection "field")
  |> to_string
  |> fun bits -> Int64.of_string ("0x" ^ bits)

let cases () =
  let fixture = fixture () in
  let projections = projections fixture in
  Alcotest.(check int)
    "complete primary native fields" 38 (List.length projections);
  List.map
    (fun projection ->
      let expected = observed fixture projection "case_id" in
      Alcotest.(check int64)
        "same-boot native repeat" expected
        (observed fixture projection "repeat_case_id");
      (field projection "field", field projection "holy_c_source", expected))
    projections

let compilation_faults () =
  let fixture = load "integer-division.json" in
  [
    ("constant-division-overflow-compile", "OracleConstOverflow");
    ("constant-modulo-overflow-compile", "OracleConstModOverflow");
    ("unreachable-overflow-compile", "OracleDeadOverflow");
    ("discarded-overflow-compile", "OracleDiscardOverflow");
  ]
  |> List.map (fun (id, function_) ->
      let check =
        fixture |> member "checks" |> to_list
        |> List.find (fun check -> field check "id" = id)
      in
      Alcotest.(check string)
        "native first failing phase" "compilation"
        (field check "first_failing_phase");
      let command = field check "command" in
      let prefix = "OracleCompile(" and suffix = ");" in
      assert (
        String.starts_with ~prefix command && String.ends_with ~suffix command);
      let definition =
        String.sub command (String.length prefix)
          (String.length command - String.length prefix - String.length suffix)
        |> Yojson.Safe.from_string |> to_string
      in
      (id, definition ^ function_ ^ "();"))

let reached_faults =
  [
    ("literal zero", "I64 F(){return 1/0;}F();", "HCIRVM0009", "");
    ("discarded zero", "I64 F(){1%0;return 7;}F();", "HCIRVM0009", "");
    ("eager AND", "I64 F(I64 x){return x&&(1/0);}F(0);", "HCIRVM0009", "");
    ("eager OR", "I64 F(I64 x){return x||(1%0);}F(1);", "HCIRVM0009", "");
    ( "variable signed overflow",
      "I64 F(I64 x,I64 y){return x/y;}F(0x8000000000000000,-1);",
      "HCIRVM0010",
      "" );
    ( "variable signed remainder overflow",
      "I64 F(I64 x,I64 y){return x%y;}F(0x8000000000000000,-1);",
      "HCIRVM0010",
      "" );
    ( "output before fault",
      "extern U0 Print(U8 *fmt,...);I64 F(I64 x){Print(\"kept\");return \
       (x/2)/0;}F(-7);",
      "HCIRVM0009",
      "kept" );
    ( "compound RHS fault before destination read",
      "extern U0 Print(U8 *fmt,...);I64 RHS(){Print(\"right\");return 1/0;}I64 \
       F(){I64 a[1];I64 *p=a+1;*p/=RHS();return 7;}F();",
      "HCIRVM0009",
      "right" );
  ]

let contextual_cases =
  [
    ( "narrow public computation stays raw",
      "Bool F(){return 84;}(~F())/2;",
      -42L );
    ( "eliminated divisor as a fixed argument",
      "U64 Take(U64 x){return x;}I64 F(I64 x){return Take(x/1(U64));}F(-7);",
      -7L );
    ( "rewritten divisor as a fixed argument",
      "U64 Take(U64 x){return x;}I64 F(I64 x){return Take(x/2(U64));}F(-7);",
      -4L );
    ("skipped literal zero", "I64 F(){if(0)return 1/0;return 7;}F();", 7L);
    ("conditional AND", "I64 F(){if(0&&(1/0))return 1;return 7;}F();", 7L);
    ("conditional OR", "I64 F(){if(1||(1%0))return 7;return 1;}F();", 7L);
    ("folded global and narrow destination", "I8 A=255/2;I64 B=-7/2;A+B;", 124L);
    ( "folded static persists",
      "I64 F(){static I64 n=-7/2;return n++;}F()*10+F();",
      -32L );
    ("folded default reused", "I64 F(I64 n=-7/2){return n;}F()+F();", -6L);
    ( "folded default with supplied argument",
      "I64 F(I64 n=-7/2){return n;}F(42);",
      42L );
    ( "indexed destination evaluates once",
      "I64 N=0;I64 Index(){return N++;}I64 F(){I64 \
       a[2];a[0]=-7;a[1]=11;a[Index()]/=2;return a[0]*10+N;}F();",
      -39L );
    ("discarded update still writes", "I64 F(){I64 x=-7;x%=2;return x;}F();", 1L);
    ( "division by one keeps producer effects",
      "I64 N=0;I64 Next(){return ++N;}I64 F(){Next()/1;return N+41;}F();",
      42L );
  ]

let retained =
  "#exe {I64 N=0;I64 Init(){N++;return -7/2;}I64 Saved(I64 n=Init()){return \
   n;}if(N!=1||Saved()!=-3)Print(\"bad\");N=0;if(Saved()!=-3||N)Print(\"bad\");StreamPrint(\"%d;\",Saved()+45);}"

let output_cases =
  [
    ( "extern U0 Print(U8 *fmt,...);I64 N=0;I64 Next(){return ++N;}I64 \
       F(){Print(\"%d:%d;\",Next()/1(U64),Next()/1(U64));return N+40;}F();",
      "2:1;" );
    ( "extern U0 Print(U8 *fmt,...);I64 N=0;I64 Next(){return ++N;}I64 \
       F(){Print(\"%d:%d;\",Next()/2,Next()/2);return N+40;}F();",
      "1:0;" );
  ]

let preparation_boundary =
  [
    "I64 F(I64 x){return x/2;}I64 G=F(-7);G;";
    "I64 F(){static I64 x=-7;x/=2;return x;}I64 G=F();G;";
  ]
