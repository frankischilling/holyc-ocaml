open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter

let gates =
  [
    ("local-elements", "I64 F(){I64 a[2];a[0]=40;a[1]=2;return a[0]+a[1];}F();");
    ( "caller-element",
      "I64 Set(I64 *p){*p+=2;return 0;}I64 F(){I64 \
       a[2];a[0]=40;Set(&a[0]);return a[0];}F();" );
    ("row-major", "I64 F(){I64 a[2][3];a[1][2]=42;return a[1][2];}F();");
    ( "dynamic-loop",
      "I64 F(){I64 a[3];I64 i=0;while(i<3){a[i]=14;i++;}return \
       a[0]+a[1]+a[2];}F();" );
    ( "pointer-index",
      "I64 F(){I64 a[2];a[0]=40;a[1]=2;I64 *p=a;return p[0]+p[1];}F();" );
    ("cross-row", "I64 F(){I64 a[2][3];a[0][3]=42;return a[1][0];}F();");
    ( "indexed-rhs",
      "I64 Bump(I64 *p){*p+=1;return *p;}I64 F(){I64 a[2];I64 \
       i=0;a[0]=20;a[i++]+=Bump(&a[0]);return a[0]+i-1;}F();" );
    ( "interior-pointer",
      "I64 F(){I64 a[2];a[0]=42;I64 *p=&a[1];return p[-1];}F();" );
  ]

let grouping_and_decay () =
  Test_integer_statics.cases
    (List.map
       (fun text -> (text, 42L))
       [
         "I64 Set(I64 *p){p[0]=42;return 0;}I64 F(){I64 a[2];Set(a);return \
          a[0];}F();";
         "I64 Set(I64 *p){p[5]=42;return 0;}I64 F(){I64 a[2][3];Set(a);return \
          a[1][2];}F();";
         "I64 Set(I64 *p){p[5]=42;return 0;}I64 F(){I64 \
          a[2][3];Set((a));return a[1][2];}F();";
         "I64 F(){I64 a[2][3];a[0][1]=40;(a)[1]+=2;return a[0][1];}F();";
         "I64 F(){I64 a[2][3];a[1][2]=40;I64 *p=a;p[5]+=2;return \
          (a[1])[2];}F();";
         "I64 F(){I64 a[2][3];I64 *p=a[1];p[2]=42;return a[1][2];}F();";
         "I64 F(){I64 a[2][3];I64 *p=((a));p[5]=42;return ((a))[5];}F();";
         "I64 F(){I64 a[2][3];I64 *p;p=a[1];p[2]=42;return a[1][2];}F();";
         "I64 F(){I64 a[2][3];*a=42;return (*a);}F();";
       ])

let evaluation_order () =
  Test_integer_statics.cases
    (List.map
       (fun text -> (text, 42L))
       [
         "I64 F(){I64 a[2];I64 i=0;a[i++]=i;return 40+a[0]+i;}F();";
         "I64 F(){I64 a[2][3];I64 i=0;a[i++][i++]=40;return a[0][1]+i;}F();";
         "I64 Id(I64 n){return n;}I64 F(){I64 a[3];a[Id(1)+1]=42;return \
          a[2];}F();";
         "I64 F(){I64 a[2],b[2];a[0]=42;I64 *p=a;p[(p=b)[0]=0]+=0;return \
          a[0];}F();";
         "I64 F(){I64 a[2];a[0]=41;return ++a[0];}F();";
         "I64 F(){I64 a[2];a[0]=42;return a[0]++;}F();";
         "I64 F(){I64 a[2];a[0]=40;I64 *p=&a[1];p[-1]+=2;return a[0];}F();";
       ])

let extents_and_calls () =
  Test_integer_statics.cases
    (List.map
       (fun text -> (text, 42L))
       [
         "I64 F(){I64 a[2][3];a[2][-1]=42;return a[1][2];}F();";
         "I64 F(){I64 a[2][3];a[3][-4]=42;return a[1][2];}F();";
         "I64 F(){I64 a[2];I64 *p=&a[1];*p=42;return a[1];}F();";
         "I64 Set(I64 *p){I64 *q=p;q[-1]=42;return 0;}I64 F(){I64 a[2];I64 \
          *p=&a[2];Set(p);return a[1];}F();";
         "I64 R(I64 n,I64 *p){I64 a[2];a[0]=n;if(n)R(n-1,a);p[0]+=a[0];return \
          0;}I64 F(){I64 a[2];a[0]=39;R(2,a);return a[0];}F();";
       ])

let errors () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) ->
          let error = F.first_error (G.run ~mode text) in
          Alcotest.(check string) text expected error.code;
          if String.starts_with ~prefix:"HCIRVM" expected then
            Alcotest.(check bool)
              "indexed fault retains owner" true
              (List.mem "function=F" error.notes))
        [
          ("I64 F(){I64 a[2];a[0]=42;return a[1];}F();", "HCIRVM0012");
          ("I64 F(){I64 a[2],neighbor=42;return a[2];}F();", "HCIRVM0019");
          ( "I64 F(){I64 neighbor=42,a[2];a[-1]=7;return neighbor;}F();",
            "HCIRVM0019" );
          ("I64 F(){I64 a[2];I64 *p=&a[2];return *p;}F();", "HCIRVM0019");
          ("I64 F(){I64 a[2];I64 *p=&a[3];return 42;}F();", "HCIRVM0019");
          ( "I64 F(){I64 n=42,neighbor=7;I64 *p=&n;return p[1];}F();",
            "HCIRVM0019" );
          ("I64 F(){I64 a[2];a[2]=1/0;return 42;}F();", "HCIRVM0009");
          ("I64 F(){I64 a[2];U64 i=-1;return a[i];}F();", "HCIRVM0020");
          ("I64 F(){I64 a[2];return a[0x7FFFFFFFFFFFFFFF];}F();", "HCIRVM0020");
          ( "I64 F(){I64 a[2][3];return a[384307168202282325][2];}F();",
            "HCIRVM0020" );
        ];
      List.iter
        (fun text -> ignore (F.first_error (G.run ~mode text)))
        [
          "I64 F(){I64 a[2][3];return (a)[1][2];}F();";
          "I64 F(){I64 a[2][3];return (*a)[0];}F();";
          "I64 F(){I64 a[2];return a[1.0];}F();";
          "I64 F(){I64 *a[2];return a[0][0];}F();";
          "I64 F(){I64 (*a)()[2];return (a)[0];}F();";
        ])
    G.modes

let explicit_array_address_boundary () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I64 F(){I64 a[2];a[0]=42;I64 *p=&a;return *p;}F();";
          "I64 F(){I64 a[2][3];a[1][0]=42;I64 *p=&a[1];return *p;}F();";
        ])
    G.modes

let fresh_frames_and_initializer_phases () =
  let module SI = Test_integer_static_initializers in
  let module H = Test_ir_integer_interpreter in
  List.iter
    (fun mode ->
      let source = "I64 F(){I64 a[2];a[1]=42;return a[1];}I64 G=F();G;" in
      let compiled = G.compile ~mode source in
      List.iter
        (fun () ->
          let result =
            SI.execute compiled
            |> H.require_ok (fun errors -> (List.hd errors).VM.message)
          in
          Alcotest.(check int64)
            "fresh array-bearing initializer invocation" 42L
            (Option.get (VM.final_value result)).bits)
        [ (); () ];
      let error =
        F.first_error
          (G.run ~mode
             "I64 F(I64 n){I64 a[1];if(n)a[0]=42;return a[0];}F(1);F(0);")
      in
      Alcotest.(check string)
        "next invocation does not reuse initialized elements" "HCIRVM0012"
        error.code;
      let error =
        F.first_error
          (G.run ~mode "I64 F(){I64 a[2];return a[2];}I64 G=F();42;")
      in
      Alcotest.(check string)
        "initializer callee bounds" "HCIRVM0019" error.code;
      List.iter
        (fun note ->
          Alcotest.(check bool) note true (List.mem note error.notes))
        [ "function=F"; "initializer=G"; "initializer_phase=" ^ SI.phase mode ];
      let compiled =
        G.compile ~mode "I64 F(){I64 a[2];a[1]=42;return a[1];}42;"
      in
      let fn = List.hd (integer_program_functions compiled) in
      let result =
        VM.execute_function ~max_steps:100 ~max_frame_bytes:16 ~frame:fn.frame
          ~arguments:[] fn.body
        |> H.require_ok (fun errors -> (List.hd errors).VM.message)
      in
      match VM.termination result with
      | VM.Returned (Some word) ->
          Alcotest.(check int64) "raw owned array body" 42L word.bits
      | _ -> Alcotest.fail "raw array body did not return its checked word")
    G.modes

let unsigned () =
  List.iter
    (fun mode ->
      ignore
        (G.run ~mode
           "U64 F(){U64 a[2];a[1]=-1;U64 *p=a;p[1]>>=1;return a[1];}F();"
        |> F.expect ~type_:VM.U64 Int64.max_int))
    G.modes

let resource_limits () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, bytes, depth) ->
          let source = List.assoc name gates in
          let result =
            G.run ~mode ~max_frame_bytes:bytes ~max_call_depth:depth source
            |> F.expect 42L
          in
          let steps = VM.executed_steps result in
          ignore
            (G.run ~mode ~max_frame_bytes:bytes ~max_call_depth:depth
               ~max_steps:steps source
            |> F.expect 42L);
          List.iter
            (fun (result, expected) ->
              Alcotest.(check string)
                "one below exact array budget" expected
                (F.first_error result).code)
            [
              (G.run ~mode ~max_frame_bytes:(bytes - 1) source, "HCIRVM0011");
              ( G.run ~mode ~max_call_depth:(depth - 1) source,
                if depth = 1 then "HCIRVM0001" else "HCIRVM0015" );
              (G.run ~mode ~max_steps:(steps - 1) source, "HCIRVM0007");
            ])
        [
          ("local-elements", 16, 1);
          ("caller-element", 24, 2);
          ("row-major", 48, 1);
        ];
      let count = Int64.succ (Int64.of_int Sys.max_array_length) in
      let source = Printf.sprintf "I64 F(){I64 a[%Ld];return 42;}F();" count in
      Alcotest.(check string)
        "oversized host array rejected before expanding cells" "HCIRVM0011"
        (F.first_error (G.run ~mode ~max_frame_bytes:max_int source)).code)
    G.modes

let malformed_index_ir () =
  let module S = Test_integer_statics in
  let module SI = Test_integer_static_initializers in
  let module Seq = Ir_instruction_sequence in
  let module O = Ir_opcode in
  let module T = Semantic_type in
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode (List.assoc "row-major" gates) in
      let rewrite transform =
        let functions =
          integer_program_functions compiled
          |> List.map (fun (fn : VM.function_definition) ->
              let descriptions =
                Ir_function_body.body fn.body
                |> Ir_x87_stack.verify
                |> Test_ir_integer_interpreter.require_ok (fun _ -> "x87")
                |> SI.code
              in
              let scale =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_mul)
                  descriptions
              in
              let offset = (Option.get scale.result).value_id in
              let base =
                List.find
                  (fun (d : Seq.description) -> d.opcode = O.Ic_add)
                  descriptions
              in
              let base = (Option.get base.result).value_id in
              { fn with body = S.rebuild (transform base offset) fn.body })
        in
        S.require_preflight (SI.execute ~functions compiled)
      in
      List.iter
        (fun stride ->
          rewrite (fun _ _ (d : Seq.description) ->
              if d.opcode = O.Ic_imm_i64 && d.payload = Some (Seq.Integer 24L)
              then { d with payload = Some (Seq.Integer stride) }
              else d))
        [ 0L; -8L; 8L; 16L ];
      rewrite (fun _ _ (d : Seq.description) ->
          if d.opcode = O.Ic_mul then
            {
              d with
              target_type =
                Some
                  (T.pointer_to Test_ir_integer_interpreter.public_u64
                  |> Result.get_ok);
            }
          else d);
      rewrite (fun base _ (d : Seq.description) ->
          if d.opcode = O.Ic_deref then { d with operands = [ base ] } else d);
      rewrite (fun _ offset (d : Seq.description) ->
          match (d.opcode, d.operands) with
          | O.Ic_assign, [ address; _ ] ->
              { d with operands = [ address; offset ] }
          | _ -> d);
      rewrite (fun base _ (d : Seq.description) ->
          match (d.opcode, d.operands) with
          | O.Ic_add, [ original; offset ]
            when not (Seq.Value_id.equal original base) -> (
              (* Keep canonical RBP roots intact; replace only already-indexed bases. *)
              match d.target_type with
              | Some _
                when List.exists
                       (fun (fn : VM.function_definition) ->
                         Ir_function_body.body fn.body
                         |> Ir_x87_stack.verify
                         |> Test_ir_integer_interpreter.require_ok (fun _ ->
                             "x87")
                         |> SI.code
                         |> List.exists (fun (producer : Seq.description) ->
                             producer.opcode = O.Ic_add
                             && Option.fold ~none:false
                                  ~some:(fun r ->
                                    Seq.Value_id.equal r.Seq.value_id original)
                                  producer.result))
                       (integer_program_functions compiled) ->
                  { d with operands = [ base; offset ] }
              | _ -> d)
          | _ -> d))
    G.modes

let tests =
  List.map
    (fun (name, source) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode -> ignore (G.run ~mode source |> F.expect 42L))
            G.modes))
    gates
  @ [
      Alcotest.test_case "grouping and array decay" `Quick grouping_and_decay;
      Alcotest.test_case "index and destination evaluation order" `Quick
        evaluation_order;
      Alcotest.test_case "object extents and recursive calls" `Quick
        extents_and_calls;
      Alcotest.test_case "bounds overflow and storage boundaries" `Quick errors;
      Alcotest.test_case "remaining-rank address-of boundary" `Quick
        explicit_array_address_boundary;
      Alcotest.test_case "fresh frames and initializer provenance" `Quick
        fresh_frames_and_initializer_phases;
      Alcotest.test_case "unsigned element bits" `Quick unsigned;
      Alcotest.test_case "resource limits before cell expansion" `Quick
        resource_limits;
      Alcotest.test_case "malformed index stride rank and operands" `Quick
        malformed_index_ir;
    ]
