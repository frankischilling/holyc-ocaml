open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter

let gates =
  [
    ("local-alias", "I64 F(){I64 n=40;I64 *p=&n;*p+=2;return n;}F();");
    ("global-alias", "I64 G=40;I64 F(){I64 *p=&G;*p=42;return G;}F();");
    ( "caller-alias",
      "I64 Set(I64 *p){*p+=2;return *p;}I64 F(){I64 n=40;return Set(&n);}F();"
    );
    ( "caller-writeback",
      "I64 Set(I64 *p){*p+=2;return *p;}I64 F(){I64 n=40;Set(&n);return n;}F();"
    );
    ("address-only", "I64 F(){I64 n;I64 *p=&n;*p=42;return n;}F();");
    ( "rhs-alias",
      "I64 Bump(I64 *p){*p+=1;return *p;}I64 F(){I64 n=20;I64 \
       *p=&n;*p+=Bump(p);return n;}F();" );
    ( "static-alias",
      "I64 Next(){static I64 n=40;I64 *p=&n;return ++*p;}Next();Next();" );
  ]

module S = Test_integer_statics
module SI = Test_integer_static_initializers
module Seq = Ir_instruction_sequence
module Body = Ir_function_body
module Initial = Ir_global_initialization
module H = Test_ir_integer_interpreter

let cases = S.cases

let copies_and_reassignment () =
  cases
    [
      ("I64 F(){I64 n=40,m=1;I64 *p=&n,*q=p;p=&m;*q+=2;return n;}F();", 42L);
      ("I64 F(){I64 n=40;I64 *p,*q;q=p=&n;*q+=2;return *p;}F();", 42L);
      ("I64 F(){I64 n=40;I64 *p=&n,*q=&*p;(*q)+=2;return *(&n);}F();", 42L);
      ( "I64 Set(I64 *p){I64 m=100;p=&m;*p=99;return 0;}I64 F(){I64 n=42;I64 \
         *p=&n;Set(p);return *p;}F();",
        42L );
      ("I64 F(){I64 n=0,m=42;I64 *p=&n;*p=*(p=&m);return n;}F();", 42L);
      ("I64 F(){I64 n=20,m=22;I64 *p=&n;*p+=*(p=&m);return n;}F();", 42L);
    ]

let updates () =
  cases
    [
      ("I64 F(){I64 n=42;I64 *p=&n;return (*p)++;}F();", 42L);
      ("I64 F(){I64 n=43;I64 *p=&n;return --*p;}F();", 42L);
      ( "I64 F(){I64 n=21;I64 \
         *p=&n;*p*=4;*p/=2;*p%=43;*p&=63;*p|=1;*p^=1;*p<<=1;*p>>=1;return \
         n;}F();",
        42L );
      ("I64 F(){I64 n=44;I64 *p=&n;*p-=2;return n;}F();", 42L);
    ]

let unsigned () =
  List.iter
    (fun mode ->
      ignore
        (G.run ~mode "U64 F(){U64 n=-1;U64 *p=&n;*p>>=1;return n;}F();"
        |> F.expect ~type_:VM.U64 Int64.max_int);
      ignore
        (G.run ~mode
           "U64 Set(U64 *p){return (*p)++;}U64 F(){U64 n=-1;return \
            Set(&n);}F();"
        |> F.expect ~type_:VM.U64 (-1L));
      ignore
        (G.run ~mode "U64 F(){U64 n=-1;U64 *p=&n;return ++*p;}F();"
        |> F.expect ~type_:VM.U64 0L))
    G.modes

let calls_and_activations () =
  cases
    [
      ( "I64 Acc(I64 a,I64 *p,U64 b,I64 *q){*p+=a;*q+=b;return *p+*q;}I64 \
         F(){I64 n=10,m=20;return Acc(5,&n,7,&m);}F();",
        42L );
      ( "I64 Add(I64 *p,I64 n){*p+=n;return *p;}I64 F(){I64 n=20;return \
         Add(&n,Add(&n,1));}F();",
        42L );
      ( "I64 R(I64 depth,I64 *p){I64 \
         local=depth;if(depth)R(depth-1,&local);*p+=local;return 0;}I64 \
         F(){I64 n=39;R(2,&n);return n;}F();",
        42L );
      ( "I64 Add(I64 *p){*p+=2;return 0;}I64 F(I64 n){Add(&n);return n;}F(40);",
        42L );
      ( "I64 Add(I64 *p){*p+=1;return *p;}I64 F(){static I64 n=40;return \
         Add(&n);}F();F();",
        42L );
    ]

let control_flow () =
  cases
    [
      ("I64 F(){I64 n=0;I64 *p=&n;while(*p<42)(*p)++;return n;}F();", 42L);
      ( "I64 F(){I64 n=40,m=1;I64 *p=&m;if(n)p=&n;else p=&m;*p+=2;return n;}F();",
        42L );
      ( "I64 F(){I64 n=40;I64 *p=&n;if(0&&(*p)++){}if(1||(*p)++){}*p+=2;return \
         n;}F();",
        42L );
    ]

let initializer_calls () =
  cases
    [
      ("I64 G=40;I64 Set(I64 *p){*p+=2;return *p;}I64 H=Set(&G);G;", 42L);
      ( "I64 G=40;I64 Set(I64 *p){*p+=2;return *p;}I64 F(){static I64 \
         n=Set(&G);return n;}G;",
        42L );
      ( "I64 Set(I64 *p){*p=42;return *p;}I64 F(){static I64 n=Set(&n);return \
         n;}F();",
        42L );
    ];
  List.iter
    (fun mode ->
      let text =
        "I64 Fail(I64 *p,I64 d){return *p/d;}I64 G=42;I64 F(){static I64 \
         n=Fail(&G,0);return n;}42;"
      in
      let error = F.first_error (G.run ~mode text) in
      Alcotest.(check string) "callee pointer fault" "HCIRVM0009" error.code;
      List.iter
        (fun note ->
          Alcotest.(check bool) note true (List.mem note error.notes))
        [
          "function=Fail"; "initializer=n"; "initializer_phase=" ^ SI.phase mode;
        ])
    G.modes

let unknown_values () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let error = F.first_error (G.run ~mode text) in
          Alcotest.(check string) "unknown storage" "HCIRVM0012" error.code;
          Alcotest.(check bool)
            "active owner" true
            (List.mem "function=F" error.notes))
        [
          "I64 F(){I64 *p;return *p;}F();";
          "I64 F(){I64 n;I64 *p=&n;return *p;}F();";
          "I64 F(){I64 n;I64 *p=&n;*p+=2;return n;}F();";
        ])
    G.modes;
  let text = "I64 G;I64 F(){I64 *p=&G;return *p;}F();" in
  Alcotest.(check string)
    "unknown JIT global" "HCIRVM0012" (F.first_error (G.run text)).code;
  ignore (G.run ~mode:Preprocessor.Aot text |> F.expect 0L)

let boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun text -> ignore (F.first_error (G.run ~mode text)))
        [
          "I64 F(){I64 *p=0;return 42;}F();";
          "I64 F(){I64 n=42;I64 *p=&n;p++;return n;}F();";
          "I64 F(){I64 n=42;I64 *p=&n;return p(I64);}F();";
          "I64 F(){I64 n=42;I64 *p=&n;return p;}F();";
          "I64 *F(){I64 n=42;return &n;}42;";
          "I64 n=42;I64 *p=&n;42;";
          "I64 F(){static I64 n=42;static I64 *p=&n;return n;}F();";
          "I64 F(){I64 n=42;I64 *p=&n;I64 **q=&p;return **q;}F();";
          "I64 F(){I0 n=42;I0 *p=&n;return *p;}F();";
          "I64 F(){I64 n=42;U64 *p=&n;return *p;}F();";
        ])
    G.modes

let source = List.assoc "caller-writeback" gates

let replay_and_limits () =
  List.iter
    (fun mode ->
      let result =
        G.run ~mode ~max_frame_bytes:16 ~max_call_depth:2 source |> F.expect 42L
      in
      let steps = VM.executed_steps result in
      Alcotest.(check int) "caller writeback instruction count" 43 steps;
      Alcotest.(check int)
        "no constant pointer preparation" 0
        (VM.compiled_initializer_steps result);
      ignore
        (G.run ~mode ~max_frame_bytes:16 ~max_call_depth:2 ~max_steps:steps
           source
        |> F.expect 42L);
      List.iter
        (fun (result, expected) ->
          Alcotest.(check string)
            "exact bound" expected (F.first_error result).code)
        [
          (G.run ~mode ~max_frame_bytes:15 source, "HCIRVM0011");
          (G.run ~mode ~max_call_depth:1 source, "HCIRVM0015");
          (G.run ~mode ~max_steps:(steps - 1) source, "HCIRVM0007");
        ];
      let compiled = G.compile ~mode (List.assoc "static-alias" gates) in
      List.iter
        (fun _ ->
          let result =
            SI.execute compiled
            |> H.require_ok (fun errors -> (List.hd errors).VM.message)
          in
          Alcotest.(check int64)
            "fresh persistent pointee" 42L
            (Option.get (VM.final_value result)).bits)
        [ (); () ];
      ignore
        (G.run ~mode ~max_global_bytes:8 (List.assoc "static-alias" gates)
        |> F.expect 42L);
      Alcotest.(check string)
        "persistent bytes" "HCIRVM0016"
        (F.first_error
           (G.run ~mode ~max_global_bytes:7 (List.assoc "static-alias" gates)))
          .code)
    G.modes

let raw_pointer_arguments () =
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode source in
      let fn = List.hd (integer_program_functions compiled) in
      S.require_preflight
        (VM.execute_function ~max_steps:100 ~max_frame_bytes:8 ~frame:fn.frame
           ~arguments:[ 42L ] fn.body);
      let compiled =
        G.compile ~mode "I64 F(){I64 n;I64 *p=&n;*p=42;return n;}42;"
      in
      let fn = List.hd (integer_program_functions compiled) in
      let result =
        VM.execute_function ~max_steps:100 ~max_frame_bytes:16 ~frame:fn.frame
          ~arguments:[] fn.body
        |> H.require_ok (fun errors -> (List.hd errors).VM.message)
      in
      match VM.termination result with
      | VM.Returned (Some word) ->
          Alcotest.(check int64) "raw owned local alias" 42L word.bits
      | _ -> Alcotest.fail "owned pointer local did not return its pointee")
    G.modes

let malformed_operands () =
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode source in
      List.iter
        (fun opcode ->
          let functions =
            integer_program_functions compiled
            |> List.map (fun (fn : VM.function_definition) ->
                let descriptions =
                  Body.body fn.body |> Ir_x87_stack.verify
                  |> H.require_ok (fun _ -> "x87")
                  |> SI.code
                in
                let word =
                  List.find_opt
                    (fun (d : Seq.description) ->
                      d.opcode = Ir_opcode.Ic_imm_i64
                      && d.target_type = Some H.i64)
                    descriptions
                in
                {
                  fn with
                  body =
                    S.rebuild
                      (fun (d : Seq.description) ->
                        match word with
                        | Some word when d.opcode = opcode ->
                            {
                              d with
                              operands = [ (Option.get word.result).value_id ];
                            }
                        | _ -> d)
                      fn.body;
                })
          in
          S.require_preflight (SI.execute ~functions compiled))
        [ Ir_opcode.Ic_addr ];
      let functions =
        integer_program_functions compiled
        |> List.map (fun (fn : VM.function_definition) ->
            {
              fn with
              body =
                S.rebuild
                  (fun (d : Seq.description) ->
                    if Int64.logand d.flags 0x2000L <> 0L then
                      {
                        d with
                        opcode = Ir_opcode.Ic_imm_i64;
                        operands = [];
                        target_type = Some H.i64;
                        payload = Some (Seq.Integer 42L);
                      }
                    else d)
                  fn.body;
            })
      in
      S.require_preflight (SI.execute ~functions compiled);
      let functions =
        integer_program_functions compiled
        |> List.map (fun (fn : VM.function_definition) ->
            {
              fn with
              body =
                S.rebuild
                  (fun (d : Seq.description) ->
                    if d.opcode = Ir_opcode.Ic_addr then
                      {
                        d with
                        target_type =
                          Some
                            (Semantic_type.pointer_to H.public_u64
                            |> Result.get_ok);
                      }
                    else d)
                  fn.body;
            })
      in
      S.require_preflight (SI.execute ~functions compiled))
    G.modes

let escaped_address () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 Use(I64 *p){return *p;}I64 G=40;I64 F(){static I64 n=G;return \
           n;}Use(&G);"
      in
      let baseline =
        SI.execute compiled
        |> H.require_ok (fun errors -> (List.hd errors).VM.message)
      in
      Alcotest.(check int64)
        "valid pointer consumer before mutation" 40L
        (Option.get (VM.final_value baseline)).bits;
      let entry = integer_program_entry compiled
      and globals = integer_program_globals compiled in
      let ds = SI.descriptions compiled in
      let d = List.hd ds in
      let address =
        SI.code entry
        |> List.find (fun (i : Seq.description) ->
            Seq.Instruction_id.equal i.instruction_id d.first)
        |> fun i -> (Option.get i.result).value_id
      in
      let consumer =
        SI.code entry
        |> List.find (fun (i : Seq.description) -> i.opcode = Ir_opcode.Ic_addr)
      in
      let entry =
        F.rewrite_entry
          (fun (i : Seq.description) ->
            if
              i.opcode = Ir_opcode.Ic_addr
              && Seq.Instruction_id.compare i.instruction_id d.last > 0
            then { i with operands = [ address ] }
            else i)
          entry
      in
      let initialization =
        Initial.create ~static_descriptions:ds ~span:(SI.span d) ~globals ~entry
          []
        |> F.checked
      in
      let result =
        VM.execute_program ~globals ~initialization
          ~functions:(integer_program_functions compiled)
          ~max_steps:100 ~max_frame_bytes:8 ~max_call_depth:1 entry
      in
      S.require_preflight result;
      match result with
      | Error errors ->
          Alcotest.(check bool)
            "IC_ADDR itself rejected" true
            (List.exists
               (fun (e : VM.error) ->
                 e.instruction_id
                 = Some (Seq.Instruction_id.to_int consumer.instruction_id))
               errors)
      | Ok _ -> assert false)
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
      Alcotest.test_case "copies and pointer reassignment" `Quick
        copies_and_reassignment;
      Alcotest.test_case "indirect scalar updates" `Quick updates;
      Alcotest.test_case "unsigned pointee bits" `Quick unsigned;
      Alcotest.test_case "mixed calls and recursive activations" `Quick
        calls_and_activations;
      Alcotest.test_case "pointer storage across control flow" `Quick
        control_flow;
      Alcotest.test_case "initializer calls and provenance" `Quick
        initializer_calls;
      Alcotest.test_case "unknown pointers and pointees" `Quick unknown_values;
      Alcotest.test_case "unsupported pointer domains" `Quick boundaries;
      Alcotest.test_case "fresh images and exact limits" `Quick
        replay_and_limits;
      Alcotest.test_case "raw bits cannot supply pointers" `Quick
        raw_pointer_arguments;
      Alcotest.test_case "malformed pointer operands and types" `Quick
        malformed_operands;
      Alcotest.test_case "IC_ADDR cannot borrow static region metadata" `Quick
        escaped_address;
    ]
