open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter

let modes = G.modes

(* TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c gives callback
   declarators the signed RT_PTR class. PrsAddOp scales callback +=/-= by the
   pointee size, and ICPreIncDec/ICPostIncDec use the same size for ++/--.
   RT_PTR is eight bytes, so these four operations move a one-star callback by
   eight bytes while the remaining compound operators use signed I64 bits. *)

let run_value mode source expected =
  ignore (G.run ~mode source |> F.expect expected)

let numeric_storage_shapes () =
  let rows =
    [
      ("automatic scalar", "I64 F(){I64 (*p)(I64 n);p=34;p++;return p==42;}F();");
      ( "fixed callback parameter",
        "I64 F(I64 (*p)(I64 n)){p++;return p==42;}F(34);" );
      ( "static scalar",
        "I64 F(){static I64 (*p)(I64 n);p=34;p++;return p==42;}F();" );
      ("global scalar", "I64 (*P)(I64 n);I64 F(){P=34;P++;return P==42;}F();");
      ( "automatic callback array",
        "I64 F(){I64 (*p)(I64 n)[2][3];p[1][2]=34;p[1][2]++;return \
         p[1][2]==42;}F();" );
      ( "static callback array",
        "I64 F(){static I64 (*p)(I64 n)[2];p[1]=50;--p[1];return p[1]==42;}F();"
      );
      ( "F64 return metadata keeps RT_PTR storage",
        "I64 F(){F64 (*p)(I64 n);p=34;p++;return p==42;}F();" );
      ( "pointer return metadata keeps RT_PTR storage",
        "I64 F(){U8 *(*p)(I64 n);p=34;p++;return p==42;}F();" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          ignore label;
          run_value mode source 1L)
        rows)
    modes

let stride_results_and_signed_compounds () =
  let rows =
    [
      ( "prefix increment result",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=34;return \
         Id(++p);}F();",
        42L );
      ( "postfix increment result",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=34;return \
         Id(p++);}F();",
        34L );
      ( "scaled add assignment result",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=26;return \
         Id(p+=2);}F();",
        42L );
      ( "scaled subtract assignment result",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=58;return \
         Id(p-=2);}F();",
        42L );
      ( "scaled add with U64 RHS",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 n=2;p=26;return \
         Id(p+=n);}F();",
        42L );
      ( "scaled subtract with U64 RHS",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 n=2;p=58;return \
         Id(p-=n);}F();",
        42L );
      ( "signed multiply",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=-21;return \
         Id(p*=-2);}F();",
        42L );
      ( "signed division",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=-84;return \
         Id(p/=-2);}F();",
        42L );
      ( "signed remainder",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=-85;return \
         Id(p%=127);}F();",
        -85L );
      ( "signed right shift",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=-8;return \
         Id(p>>=1);}F();",
        -4L );
      ( "U64 RHS does not change signed RT_PTR division",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 d=2;p=-84;return \
         Id(p/=d);}F();",
        -42L );
      ( "U64 RHS does not change signed RT_PTR shift",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);U64 n=1;p=-8;return \
         Id(p>>=n);}F();",
        -4L );
      ( "bitwise and shift compounds",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 \
         n);p=85;p&=63;p|=32;p^=15;return Id(p<<=1);}F();",
        116L );
      ( "global 2D callback array postfix result",
        "I64 (*P)(I64 n)[2][2];I64 F(){P[1][1]=50;I64 old=P[1][1]--;return \
         old*100+(P[1][1]==42);}F();",
        5001L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, expected) -> run_value mode source expected)
        rows)
    modes

let canceled_star_updates_same_cell () =
  let rows =
    [
      ( "prefix canceled star",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=34;return \
         Id(++*p);}F();",
        42L );
      ( "postfix canceled star result",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=50;I64 \
         old=Id((*p)--);return old;}F();",
        50L );
      ( "postfix canceled star store",
        "I64 F(){I64 (*p)(I64 n);p=50;(*p)--;return p==42;}F();",
        1L );
      ( "compound canceled star",
        "I64 F(){I64 (*p)(I64 n);p=26;*p+=2;return p==42;}F();",
        1L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, expected) -> run_value mode source expected)
        rows)
    modes

let update_results_flow_as_words_and_callbacks () =
  let rows =
    [
      ( "numeric argument",
        "I64 Id(I64 n){return n;}I64 F(){I64 (*p)(I64 n);p=34;return \
         Id(++p);}F();",
        42L );
      ( "numeric return",
        "I64 F(){I64 (*p)(I64 n);p=26;I64 n=(p+=2);return n;}F();",
        42L );
      ( "direct update return",
        "I64 F(){I64 (*p)(I64 n);p=34;return ++p;}F();",
        42L );
      ( "ordinary assignment from update result",
        "I64 F(){I64 (*p)(I64 n);I64 n;p=26;n=(p+=2);return n;}F();",
        42L );
      ( "callback argument keeps numeric bits without authority",
        "I64 Check(I64 (*q)(I64 n)){return q==42;}I64 F(){I64 (*p)(I64 \
         n);p=34;return Check(++p);}F();",
        1L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, expected) -> run_value mode source expected)
        rows)
    modes

let rhs_effect_precedes_old_cell_read () =
  let source =
    "I64 (*P)(I64 n);I64 Side(){P=40;return 1;}I64 F(){P=34;P+=Side();return \
     P==48;}F();"
  in
  List.iter (fun mode -> run_value mode source 1L) modes

let bounds_and_uninitialized_cells () =
  List.iter
    (fun mode ->
      let unknown =
        F.first_error
          (G.run ~mode "I64 F(){I64 (*p)(I64 n);p++;return 42;}F();")
      in
      Alcotest.(check string)
        "update reads an uninitialized callback cell" "HCIRVM0012" unknown.code;
      let bounds =
        F.first_error
          (G.run ~mode "I64 F(){I64 (*p)(I64 n)[2];p[2]++;return 42;}F();")
      in
      Alcotest.(check string)
        "callback update checks its selected element" "HCIRVM0019" bounds.code;
      ignore
        (Test_integer_output.run ~mode
           "extern U0 PutChars(U64 ch);I64 Index(){PutChars('I');return 2;}I64 \
            Side(){PutChars('R');return 1;}I64 F(){I64 (*p)(I64 \
            n)[2];p[Index()]+=Side();return 42;}F();"
        |> Test_integer_output.fault ~output:"IR" "HCIRVM0019"))
    modes

let owned_code_update_faults_after_reached_effects () =
  let source =
    "extern U0 PutChars(U64 ch);I64 Target(I64 n){return n+2;}I64 F(){I64 \
     (*p)(I64 n);p=&Target;PutChars('B');p++;PutChars('X');return 42;}F();"
  in
  List.iter
    (fun mode ->
      ignore
        (Test_integer_output.run ~mode source
        |> Test_integer_output.fault ~output:"B" "HCIRVM0024");
      let indexed =
        F.first_error
          (G.run ~mode
             "I64 Target(I64 n){return n+2;}I64 (*P)(I64 n)[2][2];I64 \
              F(){P[1][1]=&Target;P[1][1]++;return 42;}F();")
      in
      Alcotest.(check string)
        "owned code in an indexed callback slot is rejected at update"
        "HCIRVM0024" indexed.code)
    modes

let exact_update_work () =
  let source = "I64 F(){I64 (*p)(I64 n);p=34;p++;return p==42;}F();" in
  List.iter
    (fun mode ->
      let baseline = G.run ~mode ~max_steps:10_000 source |> F.expect 1L in
      let steps = VM.executed_steps baseline in
      ignore (G.run ~mode ~max_steps:steps source |> F.expect 1L);
      Alcotest.(check string)
        "one below callback-update work" "HCIRVM0007"
        (F.first_error (G.run ~mode ~max_steps:(steps - 1) source)).code)
    modes

let tests =
  [
    Alcotest.test_case "numeric callback storage shapes" `Quick
      numeric_storage_shapes;
    Alcotest.test_case "stride and signed compound semantics" `Quick
      stride_results_and_signed_compounds;
    Alcotest.test_case "canceled star updates the original callback cell" `Quick
      canceled_star_updates_same_cell;
    Alcotest.test_case "update results flow as numeric and callback values"
      `Quick update_results_flow_as_words_and_callbacks;
    Alcotest.test_case "RHS effects precede the compound old-cell read" `Quick
      rhs_effect_precedes_old_cell_read;
    Alcotest.test_case "bounds and uninitialized callback cells" `Quick
      bounds_and_uninitialized_cells;
    Alcotest.test_case "owned callback addresses fault at the reached update"
      `Quick owned_code_update_faults_after_reached_effects;
    Alcotest.test_case "callback updates charge exact work" `Quick
      exact_update_work;
  ]
