open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module T = Test_integer_task
module O = Test_integer_output
module VM = Ir_integer_interpreter

let forward () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (G.run ~mode source |> F.expect 42L))
        [
          "extern I64 F(I64 n);I64 Use(){return F(42);}I64 F(I64 n){return \
           n;}Use();";
          "extern I64 Odd(I64 n);I64 Even(I64 n){if(n)return Odd(n-1);return \
           42;}I64 Odd(I64 n){if(n)return Even(n-1);return 0;}Even(6);";
          "extern I64 F(I64 n=40);I64 Use(){return F();}I64 F(I64 n=99){return \
           n+2;}Use();";
          "extern I64 F(...);I64 Use(){return F(20,20);}I64 F(...){return \
           argc+argv[0]+argv[1];}Use();";
        ])
    G.modes

let retained () =
  let session = Session.create () in
  let task = T.create session in
  let accept text =
    ignore (T.run session task text |> Test_integer_program.checked)
  in
  accept "extern I64 F(I64 n);";
  accept "I64 Use(){return F(42);}";
  accept "I64 F(I64 value){return value;}";
  T.value 42L (T.run session task "Use();");
  accept "I64 F(I64 value){return 99;}";
  T.value 42L (T.run session task "Use();")

let publication_order () =
  let early = "extern I64 F();F();I64 F(){return 42;}" in
  ignore (O.run ~mode:Preprocessor.Jit early |> O.fault "HCIRVM0030");
  ignore (O.run ~mode:Preprocessor.Aot early |> O.expect "");
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode "extern I64 F();I64 Unused(){return F();}if(0)F();42;"
        |> O.expect "");
      let error =
        O.run ~mode (O.putchars_header ^ "extern I64 F();PutChars('A');F();")
        |> O.fault ~output:"A" "HCIRVM0030"
      in
      Alcotest.(check bool)
        "undefined extern is reached" true
        (List.mem "stage=execution" error.notes))
    G.modes;
  let session = Session.create () in
  let task = T.create session in
  ignore
    (T.run session task "I64 N=40;extern I64 F();I64 Use(){return F();}"
    |> Test_integer_program.checked);
  T.fault "HCIRVM0030" (T.run session task "N+=2;Use();");
  T.value 42L (T.run session task "N;");
  ignore
    (T.run session task "I64 F(){return N;}" |> Test_integer_program.checked);
  T.value 42L (T.run session task "Use();")

let providers () =
  let source =
    "I64 N=0;extern U0 PutChars(U64 ch);U0 Saved(){PutChars('A');}Saved();U0 \
     PutChars(U64 word){N=42;}Saved();N;"
  in
  ignore (O.run ~mode:Preprocessor.Jit source |> O.expect "A");
  ignore (O.run ~mode:Preprocessor.Aot source |> O.expect "");
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "extern U0 PutChars(U64 ch);U0 Saved(){PutChars('A');}U0 \
            PutChars(I64 word){}Saved();"
        |> O.fault "HCIRVM0014"))
    G.modes

let initializers () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.expect ""))
        [
          "extern I64 F();I64 Use(){return F();}I64 F(){return 42;}I64 \
           N=Use();N;";
          "extern I64 F();I64 Use(){return F();}I64 F(){return 42;}I64 \
           Saved(){static I64 n=Use();return n;}Saved();";
        ];
      ignore
        (O.run ~mode
           "extern I64 F(I64 n);I64 Use(){return F(126);}I64 F(I64 n){return \
            n/3;}I64 N=Use();N;"
        |> O.fault "HCRUN0006"))
    G.modes;
  let session = Session.create () in
  let task = T.create session in
  List.iter
    (fun source ->
      ignore (T.run session task source |> Test_integer_program.checked))
    [
      "extern I64 F(I64 n);";
      "I64 Use(){return F(42);}";
      "I64 F(I64 n){return n;}";
    ];
  T.value 42L (T.run session task "I64 N=Use();N;");
  let session = Session.create () in
  let task = T.create session in
  List.iter
    (fun source ->
      ignore (T.run session task source |> Test_integer_program.checked))
    [
      "extern I64 F(I64 n);";
      "I64 Use(){return F(126);}";
      "I64 F(I64 n){return n/3;}";
    ];
  T.fault "HCRUN0006" (T.run session task "I64 N=Use();");
  let early_static =
    "extern I64 F();I64 Use(){return F();}I64 Saved(){static I64 \
     n=Use();return n;}I64 F(){return 42;}Saved();"
  in
  ignore (O.run ~mode:Preprocessor.Jit early_static |> O.fault "HCIRVM0030");
  ignore (O.run ~mode:Preprocessor.Aot early_static |> O.expect "")

let initializer_provider_timing () =
  List.iter
    (fun root ->
      let source =
        "extern U0 PutChars(U64 ch);I64 Use(){PutChars('A');return 42;}" ^ root
        ^ "U0 PutChars(U64 x){I64 y=x/3;}N;"
      in
      ignore (O.run ~mode:Preprocessor.Jit source |> O.expect "A");
      ignore (O.run ~mode:Preprocessor.Aot source |> O.fault "HCRUN0006"))
    [
      "I64 N=Use();"; "I64 Saved(){static I64 n=Use();return n;}I64 N=Saved();";
    ];
  let session = Session.create () in
  let task = T.create session in
  List.iter
    (fun source ->
      ignore (T.run session task source |> Test_integer_program.checked))
    [ "extern I64 F(I64 n);"; "I64 Use(){return F(126);}" ];
  let ast = T.parse session "I64 F(I64 n){return n/3;}I64 N=Use();N;" in
  match Integer_task.compile_ast task ast with
  | Error (error :: _) ->
      Alcotest.(check string)
        "current image target is inspected through retained caller" "HCRUN0006"
        error.code
  | Ok command ->
      (* A legacy AST definition cannot replace the source-selected lineage. *)
      T.fault "HCIRVM0030" (Integer_task.execute task command)
  | Error [] -> Alcotest.fail "missing initializer diagnostic"

let fragments () =
  let session = Session.create () in
  let task = T.create session in
  List.iter
    (fun source ->
      ignore (T.run session task source |> Test_integer_program.checked))
    [ "extern I64 F();"; "I64 Use(){return F();}"; "I64 F(){return 42;}" ];
  T.value 42L (T.run session task "U8 A[Use()];sizeof A;");
  T.value 42L (T.run session task "I64 Saved(I64 n=Use()){return n;}Saved();");
  let session = Session.create () in
  let task = T.create session in
  List.iter
    (fun source ->
      ignore (T.run session task source |> Test_integer_program.checked))
    [
      "extern I64 F(I64 n);";
      "I64 Use(){return F(126);}";
      "I64 F(I64 n){return n/3;}";
    ];
  T.fault "HCRUN0006" (T.run session task "U8 A[Use()];")

let signature_and_arguments () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.expect ""))
        [
          "extern I64 F(I64 a,I64 b);I64 N=0;I64 Use(){return \
           F(N=1,N=2)*10+N;}I64 F(I64 a,I64 b){return b-a;}Use()+31;";
          "extern U0 F(I64 *p);I64 Use(){I64 n=40;F(&n);return n;}U0 F(I64 \
           *v){*v+=2;}Use();";
          "extern I64 F(I8 n);I64 Use(){return F(257);}I64 F(I8 v){return \
           v+41;}Use();";
        ];
      List.iter
        (fun (prototype, definition) ->
          let source =
            "I64 N=0;" ^ prototype ^ "I64 Use(){return F(++N);}" ^ definition
            ^ "Use();"
          in
          let error = O.run ~mode source |> O.fault "HCIRVM0014" in
          Alcotest.(check bool)
            "incompatible source is rejected when reached" true
            (List.mem "stage=execution" error.notes))
        [
          ("extern I64 F(I64 n);", "U64 F(I64 n){return n;}");
          ("extern I64 F(I64 n);", "I64 F(U64 n){return n;}");
          ("extern I64 F(I64 n);", "I64 F(I64 n,I64 m){return n;}");
          ("extern I64 F(I64 n);", "I64 F(I64 n,...){return n;}");
        ])
    G.modes

let ownership () =
  List.iter
    (fun mode ->
      let source =
        "extern I64 F();I64 Use(){return F();}I64 F(){return 42;}Use();"
      in
      let compiled = G.compile ~mode source in
      let foreign = G.compile ~mode source in
      let run context functions =
        VM.execute_program
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~runtime_calls:context ~functions ~max_steps:1000 ~max_frame_bytes:64
          ~max_call_depth:2
          (integer_program_entry compiled)
      in
      List.iter
        (fun result ->
          match result with
          | Ok _ -> Alcotest.fail "foreign extern publication evidence admitted"
          | Error errors ->
              Alcotest.(check bool)
                "ownership fails before execution" true
                (List.for_all
                   (fun (error : VM.error) ->
                     error.stage = VM.Preflight && error.executed_steps = 0)
                   errors))
        [
          run
            (integer_program_runtime_calls foreign)
            (integer_program_functions compiled);
          run
            (integer_program_runtime_calls compiled)
            (integer_program_functions foreign);
        ])
    G.modes

let quotas () =
  let source =
    "extern I64 F(...);I64 Use(){return F(40,0);}I64 F(...){return \
     argc+argv[0];}Use();"
  in
  List.iter
    (fun mode ->
      let execution =
        G.run ~mode ~max_frame_bytes:24 ~max_call_depth:2 source |> F.expect 42L
      in
      let steps = VM.executed_steps execution in
      ignore (G.run ~mode ~max_steps:steps source |> F.expect 42L);
      List.iter
        (fun (code, result) ->
          Alcotest.(check string)
            "extern call quota" code (F.first_error result).code)
        [
          ("HCIRVM0007", G.run ~mode ~max_steps:(steps - 1) source);
          ("HCIRVM0011", G.run ~mode ~max_frame_bytes:23 source);
          ("HCIRVM0015", G.run ~mode ~max_call_depth:1 source);
        ])
    G.modes

let tests =
  [
    Alcotest.test_case "published forward definitions" `Quick forward;
    Alcotest.test_case "retained unresolved body and later shadowing" `Quick
      retained;
    Alcotest.test_case "publication order and reached failure recovery" `Quick
      publication_order;
    Alcotest.test_case "published source replaces provider" `Quick providers;
    Alcotest.test_case "initializer transitive extern calls" `Quick initializers;
    Alcotest.test_case "initializer provider publication timing" `Quick
      initializer_provider_timing;
    Alcotest.test_case "published targets in preparation fragments" `Quick
      fragments;
    Alcotest.test_case "extern invocation resource bounds" `Quick quotas;
    Alcotest.test_case "captured ABI and argument order" `Quick
      signature_and_arguments;
    Alcotest.test_case "exact published executable ownership" `Quick ownership;
  ]
