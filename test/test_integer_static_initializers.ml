open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter

let source =
  "I64 Seed(){return 40;}I64 Next(){static I64 n=Seed();return \
   ++n;}Next();Next();"

let cases examples =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (G.run ~mode text |> F.expect expected))
        examples)
    G.modes

let unused_effect () =
  cases [ ("I64 G=0;I64 Never(){static I64 n=(G=7);return n;}G;", 7L) ]

let early_snapshot () =
  cases [ ("I64 G=1;I64 F(){static I64 n=G;return n;}G=7;F();", 1L) ]

let late_snapshot () =
  cases [ ("I64 G=1;G=7;I64 F(){static I64 n=G;return n;}F();", 7L) ]

let earlier_callee () = cases [ (source, 42L) ]

module S = Test_integer_statics
module Initial = Ir_global_initialization
module Globals = Ir_integer_globals
module Frame = Semantic_function_frame_layout
module Prep = Integer_initializer_preparation
module Seq = Ir_instruction_sequence
module Body = Ir_function_body
module H = Test_ir_integer_interpreter

let phase mode =
  if mode = Preprocessor.Jit then "compile-initializer" else "load-initializer"

let descriptions compiled =
  Initial.static_regions (integer_program_initialization compiled)
  |> List.map Initial.describe_static

let code entry =
  Ir_x87_stack.graph entry |> Ir_block_graph.blocks
  |> List.concat_map (fun block ->
      Ir_block_graph.instructions block
      |> Seq.instructions |> List.map Seq.description)

let span (description : Initial.static_region_description) =
  match
    Semantic_function_call_expression_result.initializer_value
      description.static_root
    |> Semantic_function_call_expression_result.result_origin
  with
  | Semantic_symbol.Source_location location -> location.span
  | _ -> assert false

let execute ?functions compiled =
  VM.execute_program
    ~globals:(integer_program_globals compiled)
    ~initialization:(integer_program_initialization compiled)
    ~functions:
      (Option.value functions ~default:(integer_program_functions compiled))
    ~max_steps:10000 ~max_frame_bytes:1024 ~max_call_depth:16
    (integer_program_entry compiled)

let declaration_order () =
  cases
    [
      ( "I64 G=19;I64 A(){static I64 n=G;return ++n;}G=21;I64 B(){static I64 \
         n=G;return ++n;}(A()+B());",
        42L );
      ("I64 G=0;I64 F(){if(0){static I64 n=(G=7);}return 0;}G;", 7L);
      ("I64 G=0;I64 F(){return 0;static I64 n=(G=7);}G;", 7L);
      ("I64 G=0;I64 F(){static I64 a=(G=1),b=(G=G*10+2);return a+b;}G;", 12L);
      ("I64 G=40;I64 F(){static I64 a=G,b=++a;return a+b;}F();", 82L);
      ("I64 G=40;I64 F(){static I64 a=G,b=(a+=2);return a;}F();", 42L);
      ("I64 G=0;7;I64 F(){static I64 n=(G=42);return n;}", 7L);
      ("I64 G=0;I64 F(){static I64 n=(G=7);return n;}I64 H=G+1;H;", 8L);
      ( "I64 Seed(I64 n){return n+1;}I64 F(){static I64 \
         n=Seed(Seed(40));return n;}F();",
        42L );
      ( "I64 G=40;I64 Seed(){static I64 n=G;return ++n;}I64 F(){static I64 \
         n=Seed();return n;}Seed();",
        42L );
      ( "I64 Sum(I64 n){if(n)return n+Sum(n-1);return 0;}I64 F(){static I64 \
         n=Sum(8)+6;return n;}F();",
        42L );
    ]

let frame_isolation () =
  List.iter
    (fun mode ->
      let text =
        "I64 Seed(I64 p){I64 q=p+1;return q;}I64 F(I64 unused){static I64 \
         n=Seed(41);return n;}42;"
      in
      ignore
        (G.run ~mode ~max_frame_bytes:16 ~max_call_depth:1 text |> F.expect 42L);
      Alcotest.(check string)
        "only Seed consumes active frame" "HCIRVM0011"
        (F.first_error (G.run ~mode ~max_frame_bytes:15 text)).code;
      List.iter
        (fun text ->
          Alcotest.(check string)
            "containing frame is absent" "HCRUN0006"
            (F.first_error (G.run ~mode text)).code)
        [
          "I64 Seed(I64 p){return p;}I64 F(I64 p){static I64 n=Seed(p);return \
           n;}42;";
          "I64 F(){I64 p=42;static I64 n=p;return n;}42;";
        ])
    G.modes

let publication () =
  let text = "I64 F(){static I64 n=F();return 40;}F();" in
  ignore (G.run ~mode:Preprocessor.Aot text |> F.expect 40L);
  let error = F.first_error (G.run ~mode:Preprocessor.Jit text) in
  Alcotest.(check string) "JIT owner is not published" "HCIRVM0017" error.code;
  Alcotest.(check bool)
    "publication retains phase" true
    (List.mem "initializer_phase=compile-initializer" error.notes);
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 Base(){return 40;}I64 Relay(){return Base();}I64 F(){static I64 \
           n=Relay();return 40;}F();"
      in
      let functions = integer_program_functions compiled in
      let target =
        List.nth functions 2 |> fun (fn : VM.function_definition) ->
        Body.symbol fn.body
      in
      let functions =
        List.mapi
          (fun index (fn : VM.function_definition) ->
            if index = 1 then
              {
                fn with
                body =
                  S.rebuild
                    (fun (d : Seq.description) ->
                      if
                        d.opcode = Ir_opcode.Ic_call
                        || d.opcode = Ir_opcode.Ic_call_start
                        || d.opcode = Ir_opcode.Ic_call_end
                      then { d with payload = Some (Seq.Symbol target) }
                      else d)
                    fn.body;
              }
            else fn)
          functions
      in
      match execute ~functions compiled with
      | Error (error :: _) when mode = Preprocessor.Jit ->
          Alcotest.(check string)
            "actual transitive body checked" "HCIRVM0017" error.code;
          Alcotest.(check int)
            "publication precedes effects" 0 error.executed_steps;
          Alcotest.(check (option string))
            "publication caller identity" (Some "Relay") error.function_name;
          Alcotest.(check (option int))
            "publication caller ID" (Some 1) error.function_id
      | Ok result when mode = Preprocessor.Aot ->
          Alcotest.(check int64)
            "AOT completed owner available" 40L
            (Option.get (VM.final_value result)).bits
      | Error errors ->
          Alcotest.fail
            (String.concat "; "
               (List.map
                  (fun (e : VM.error) -> e.code ^ ": " ^ e.message)
                  errors))
      | _ -> Alcotest.fail "wrong publication outcome")
    G.modes

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun declaration ->
          let text = "I64 D=0;I64 Never(){" ^ declaration ^ "}42;" in
          let error = F.first_error (G.run ~mode text) in
          Alcotest.(check string)
            "definition-time division fault" "HCIRVM0009" error.code;
          Alcotest.(check int)
            "initializer operator span" (String.index text '/')
            error.primary.start;
          Alcotest.(check bool)
            "initializer phase retained" true
            (List.mem ("initializer_phase=" ^ phase mode) error.notes))
        [
          "static I64 s=1/D;return 0;";
          "if(0){static I64 s=1/D;}return 0;";
          "return 0;static I64 s=1/D;";
        ];
      let text =
        "I64 Fail(I64 d){return 1/d;}I64 F(){static I64 s=Fail(0);return s;}42;"
      in
      let compiled = G.compile ~mode text in
      let region = List.hd (descriptions compiled) in
      let callee = List.hd (integer_program_functions compiled) in
      let overlapping =
        Body.body callee.body |> Ir_x87_stack.verify
        |> H.require_ok (fun _ -> "x87")
        |> code
        |> List.exists (fun (d : Seq.description) ->
            Seq.Instruction_id.compare d.instruction_id region.first >= 0
            && Seq.Instruction_id.compare d.instruction_id region.last <= 0)
      in
      Alcotest.(check bool) "entry/callee IDs overlap" true overlapping;
      match execute compiled with
      | Error (error :: _) ->
          Alcotest.(check string)
            "callee division fault" "HCIRVM0009" error.code;
          Alcotest.(check (option string))
            "callee owner" (Some "Fail") error.function_name;
          Alcotest.(check (option string))
            "initializer owner through call" (Some "s") error.initializer_name;
          Alcotest.(check (option string))
            "initializer phase through call"
            (Some (phase mode))
            (Option.map Initial.phase_name error.initializer_phase)
      | _ -> Alcotest.fail "expected callee fault")
    G.modes

let replay_and_limits () =
  List.iter
    (fun mode ->
      let result =
        G.run ~mode ~max_frame_bytes:1 ~max_global_bytes:8 ~max_call_depth:1
          source
        |> F.expect 42L
      in
      Alcotest.(check int)
        "scheduled runtime steps" 32 (VM.executed_steps result);
      Alcotest.(check int)
        "no constant preparation" 0
        (VM.compiled_initializer_steps result);
      ignore (G.run ~mode ~max_steps:32 source |> F.expect 42L);
      List.iter
        (fun (result, expected) ->
          Alcotest.(check string)
            "exact bound" expected (F.first_error result).code)
        [
          (G.run ~mode ~max_steps:31 source, "HCIRVM0007");
          (G.run ~mode ~max_global_bytes:7 source, "HCIRVM0016");
        ];
      let compiled = G.compile ~mode source in
      let globals = integer_program_globals compiled in
      let initial = integer_program_initialization compiled in
      Alcotest.(check int)
        "global API excludes statics" 0
        (List.length (Initial.regions initial));
      Alcotest.(check int)
        "one pending static" 1
        (List.length (Initial.static_regions initial));
      let item =
        integer_program_initializer_preparation compiled
        |> Prep.static_items |> List.hd
      in
      Alcotest.(check bool)
        "static classification" true
        (Prep.static_classification item = Prep.Scheduled);
      Alcotest.(check bool)
        "published slot" true
        (Prep.static_slot item == List.hd (Globals.statics globals));
      let direct ?initialization ?(globals = globals) entry =
        VM.execute_program ~globals ?initialization
          ~functions:(integer_program_functions compiled)
          ~max_steps:100 ~max_frame_bytes:1 ~max_call_depth:1 entry
      in
      let entry = integer_program_entry compiled in
      S.require_preflight (direct entry);
      S.require_preflight
        (direct ~initialization:initial (F.rewrite_entry Fun.id entry));
      S.require_preflight
        (direct ~initialization:initial
           ~globals:(integer_program_globals (G.compile ~mode source))
           entry);
      List.iter
        (fun _ ->
          let result =
            direct ~initialization:initial entry
            |> H.require_ok (fun errors -> (List.hd errors).VM.message)
          in
          Alcotest.(check int64)
            "fresh image replay" 42L (Option.get (VM.final_value result)).bits)
        [ (); () ];
      Alcotest.(check string)
        "deterministic dump"
        (integer_program_human compiled)
        (integer_program_human (G.compile ~mode source)))
    G.modes

let malformed_regions () =
  List.iter
    (fun mode ->
      let text =
        "I64 G=40;7;I64 F(){static I64 a=G,b=a+1;return b;}if(1){42;}42;"
      in
      let compiled = G.compile ~mode text in
      let entry = integer_program_entry compiled
      and globals = integer_program_globals compiled in
      let ds = descriptions compiled in
      let d = List.hd ds in
      let check message entry ds =
        Alcotest.(check bool)
          message true
          (Result.is_error
             (Initial.create ~static_descriptions:ds ~span:(span d) ~globals
                ~entry []))
      in
      check "missing region" entry [];
      check "duplicate region" entry (ds @ ds);
      check "reordered regions" entry (List.rev ds);
      let foreign = G.compile ~mode text |> descriptions |> List.hd in
      check "foreign root" entry
        ({ d with static_root = foreign.static_root } :: List.tl ds);
      check "foreign slot" entry
        ({ d with static_slot = foreign.static_slot } :: List.tl ds);
      check "missing ending" entry ({ d with last = d.first } :: List.tl ds);
      let final = code entry |> List.rev |> List.hd in
      check "region crosses a block" entry
        ({ d with last = final.instruction_id } :: List.tl ds);
      let ordinary =
        code entry
        |> List.find (fun (i : Seq.description) ->
            i.payload = Some (Seq.Integer 7L))
      in
      let borrowed = (Option.get ordinary.result).value_id in
      List.iter
        (fun (message, apply) -> check message (F.rewrite_entry apply entry) ds)
        [
          ( "external operand",
            fun (i : Seq.description) ->
              if i.opcode = Ir_opcode.Ic_assign then
                { i with operands = [ List.hd i.operands; borrowed ] }
              else i );
          ( "wrong destination",
            fun i ->
              if Seq.Instruction_id.equal i.Seq.instruction_id d.first then
                {
                  i with
                  payload =
                    Some
                      (Seq.Symbol
                         (Globals.slot_symbol (List.hd (Globals.slots globals))));
                }
              else i );
          ( "wrong destination type",
            fun i ->
              if Seq.Instruction_id.equal i.Seq.instruction_id d.first then
                {
                  i with
                  target_type =
                    Some
                      (Globals.storage_type
                         (Globals.static_storage d.static_slot));
                }
              else i );
        ];
      let statements =
        List.map
          (fun slot -> Ir_integer_program_lowering.Initialize_static slot)
          (Globals.statics globals)
      in
      Alcotest.(check bool)
        "compatibility projection rejects static evidence loss" true
        (Result.is_error
           (Ir_integer_program_lowering.lower_with_initializers ~globals
              ~span:(span d) statements)))
    G.modes

let escaped_address () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode "I64 G=40;I64 F(){static I64 n=G;return n;}G;"
      in
      let entry = integer_program_entry compiled
      and globals = integer_program_globals compiled in
      let ds = descriptions compiled in
      let d = List.hd ds in
      let address =
        code entry
        |> List.find (fun (i : Seq.description) ->
            Seq.Instruction_id.equal i.instruction_id d.first)
        |> fun i -> (Option.get i.result).value_id
      in
      let entry =
        F.rewrite_entry
          (fun (i : Seq.description) ->
            if
              i.opcode = Ir_opcode.Ic_deref
              && Seq.Instruction_id.compare i.instruction_id d.last > 0
            then { i with operands = [ address ] }
            else i)
          entry
      in
      let initialization =
        Initial.create ~static_descriptions:ds ~span:(span d) ~globals ~entry []
        |> F.checked
      in
      S.require_preflight
        (VM.execute_program ~globals ~initialization
           ~functions:(integer_program_functions compiled)
           ~max_steps:100 ~max_frame_bytes:1 ~max_call_depth:1 entry))
    G.modes

let callee_authority () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 G=40;I64 Seed(){return G;}I64 F(){static I64 n=Seed();return \
           n;}42;"
      in
      let globals = integer_program_globals compiled in
      let slot = List.hd (Globals.statics globals) |> Globals.static_storage in
      let functions =
        integer_program_functions compiled
        |> List.mapi (fun index (fn : VM.function_definition) ->
            if index = 0 then
              {
                fn with
                body =
                  S.rebuild
                    (fun (i : Seq.description) ->
                      match i.payload with
                      | Some (Seq.Symbol _)
                        when i.opcode = Globals.storage_opcode slot ->
                          {
                            i with
                            payload =
                              Some (Seq.Symbol (Globals.storage_symbol slot));
                          }
                      | _ -> i)
                    fn.body;
              }
            else fn)
      in
      S.require_preflight (execute ~functions compiled))
    G.modes

let nondefault_options () =
  List.iter
    (fun mode ->
      let span, globals =
        S.option_globals mode "I64 F(){static I64 n=(n=42);return n;}42;"
      in
      let result =
        Holyc_lib__Driver__Integer_initializers.prepare ~max_steps:10 ~span
          ~globals ~top_calls:[] ~functions:[] ()
      in
      if mode = Preprocessor.Aot then
        Alcotest.(check string)
          "AOT separate option phase" "HCRUN0006" (F.first_error result).code
      else
        let prepared = F.checked result in
        Alcotest.(check bool)
          "JIT option uses scheduled phase" true
          (Prep.static_classification (List.hd (Prep.static_items prepared))
          = Prep.Scheduled))
    G.modes

let tests =
  [
    Alcotest.test_case "unused definition effect" `Quick unused_effect;
    Alcotest.test_case "early declaration snapshot" `Quick early_snapshot;
    Alcotest.test_case "late declaration snapshot" `Quick late_snapshot;
    Alcotest.test_case "earlier completed callee" `Quick earlier_callee;
    Alcotest.test_case "declaration order and effects" `Quick declaration_order;
    Alcotest.test_case "invocation frame isolation" `Quick frame_isolation;
    Alcotest.test_case "actual callee publication" `Quick publication;
    Alcotest.test_case "definition faults and call provenance" `Quick faults;
    Alcotest.test_case "replay and exact limits" `Quick replay_and_limits;
    Alcotest.test_case "malformed static regions" `Quick malformed_regions;
    Alcotest.test_case "address cannot escape region" `Quick escaped_address;
    Alcotest.test_case "callee cannot inherit authority" `Quick callee_authority;
    Alcotest.test_case "nondefault compiler options" `Quick nondefault_options;
  ]
