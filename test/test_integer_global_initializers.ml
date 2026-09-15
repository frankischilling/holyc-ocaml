module F = Test_integer_functions
module G = Test_integer_globals
module T = Test_top_level_expression_result
open Holyc_lib

let source =
  "I64 Total=0;I64 AddTo(I64 n){Total=Total+n;return \
   Total;}AddTo(20);AddTo(22);Total;"

let source_gate () =
  List.iter (fun mode -> ignore (G.run ~mode source |> F.expect 42L)) G.modes

let source_order () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (G.run ~mode text |> F.expect expected))
        [
          ("I64 G=1;G=7;I64 H=G;H;", 7L);
          ("I64 G=1;I64 Next(){G=G+1;return G;}I64 H=Next();(H*10+G);", 22L);
          ("I64 G=1,H=G+1;(G*10+H);", 12L);
          ("I64 G=(G=7)+1;G;", 8L);
          ("I64 A=1,B=(A=A+1),C=A+B;(A*100+B*10+C);", 224L);
          ("7;I64 G=1;I64 H=G;", 7L);
        ])
    G.modes

let typed_inputs mode text =
  let prepared = T.prepared ~mode ~path:"global-initializer-roots.hc" text in
  let environment = T.empty_environment prepared in
  let globals =
    resolve_global_records prepared.session ~declarations:prepared.declarations
      ~globals:prepared.global_types ~compilation_mode:mode prepared.ast
    |> T.checked
  in
  let initializers =
    resolve_global_initializers prepared.session ~environment
      ~expressions:prepared.module_expressions ~globals prepared.ast
    |> T.checked
  in
  let tree, _, _, typed = T.analyze ~environment ~initializers prepared in
  (prepared, environment, initializers, tree, typed)

let initializer_roots typed =
  let module Typed = Semantic_function_call_expression_result in
  Typed.top_level_statements typed
  |> List.concat_map Typed.top_level_statement_roots
  |> List.filter_map (fun root ->
      match
        root |> Typed.top_level_root_source
        |> Semantic_top_level_expression_tree.root_role
      with
      | Semantic_top_level_expression_tree.Global_initializer owner ->
          Some (owner, root)
      | _ -> None)

let checked_roots () =
  let module Typed = Semantic_function_call_expression_result in
  let module Initial = Semantic_global_initializer_binding in
  List.iter
    (fun mode ->
      let _, _, initializers, _, typed =
        typed_inputs mode
          "I64 A=1,B=(A=A+1),C=A+B;I64 Id(I64 n){return n;}I64 D=Id(C);D;"
      in
      let roots = initializer_roots typed in
      Alcotest.(check (list string))
        "source-ordered declaration owners" [ "A"; "B"; "C"; "D" ]
        (List.map
           (fun (owner, _) ->
             Initial.global_symbol owner |> Semantic_symbol.name)
           roots);
      List.iter
        (fun (owner, root) ->
          Alcotest.(check bool)
            "exact owner retained" true
            (match
               Initial.find_global initializers (Initial.global_symbol owner)
             with
            | Some expected -> expected == owner
            | None -> false);
          Alcotest.(check bool)
            "initializer is not an ordinary unused expression" true
            (Typed.top_level_root_result_use root = None);
          Alcotest.(check bool)
            "initializer root has a checked type" true
            (Option.is_some
               (Typed.result_type (Typed.top_level_root_value root))))
        roots;
      Alcotest.(check int)
        "initializer call uses the shared direct-call engine" 1
        (List.length (Typed.top_level_direct_calls typed)))
    G.modes

let foreign_roots () =
  let module Tree = Semantic_top_level_expression_tree in
  let _, _, left, tree, _ = typed_inputs Preprocessor.Jit "I64 G=1;" in
  let _, _, _, _, right = typed_inputs Preprocessor.Jit "I64 G=1;" in
  let foreign, root = List.hd (initializer_roots right) in
  Alcotest.(check bool)
    "initializer group rejects a foreign owner" true
    (Result.is_error
       (Semantic_top_level_expression_binding.make_global_initializer
          ~statement_index:0 ~initializers:left ~global:foreign []));
  let group = List.hd (Tree.statements tree) in
  Alcotest.(check bool)
    "tree rejects a same-spelling foreign initializer root" true
    (Result.is_error
       (Tree.make_statement
          ~source:(Tree.statement_source group)
          ~roots:
            [
              Semantic_function_call_expression_result.top_level_root_source
                root;
            ]
          ~calls:[] ~switch_cases:[]))

let storage_roots () =
  let module Globals = Ir_integer_globals in
  List.iter
    (fun mode ->
      let prepared, _, initializers, _, typed =
        typed_inputs mode "I64 G=41+1;"
      in
      let records =
        classify_global_records prepared.session
          ~resolution:
            (Semantic_global_initializer_binding.source_globals initializers)
          prepared.ast
        |> T.checked
      in
      let globals =
        F.checked
          (Globals.create ~initializers:typed ~span:prepared.ast.span records)
      in
      let slot = List.hd (Globals.slots globals) in
      let root = snd (List.hd (initializer_roots typed)) in
      Alcotest.(check bool)
        "storage retains its exact initializer root" true
        (match Globals.slot_initializer slot with
        | Some expected -> expected == root
        | None -> false);
      let lowered =
        Ir_expression_lowering.lower_global_initializer ~globals
          ~instruction_id:
            (Result.get_ok (Ir_instruction_sequence.Instruction_id.of_int 0))
          ~value_id:(Result.get_ok (Ir_instruction_sequence.Value_id.of_int 0))
          root
      in
      (match lowered with
      | Ok (Ir_expression_lowering.Lowered fragment) ->
          let ops =
            fragment |> Ir_expression_lowering.sequence
            |> Ir_instruction_sequence.instructions
            |> List.map (fun instruction ->
                (Ir_instruction_sequence.description instruction).opcode)
          in
          Alcotest.(check bool)
            "initializer emits a destination store" true
            (List.mem Ir_opcode.Ic_assign ops);
          Alcotest.(check bool)
            "initializer does not read its old destination" false
            (List.mem Ir_opcode.Ic_deref ops)
      | _ -> Alcotest.fail "checked global initializer did not lower");
      Alcotest.(check bool)
        "pending storage has not prepared its initializer" false
        (Globals.slot_initializer_materialized slot);
      let entry = integer_program_entry (G.compile ~mode "7;") in
      match
        Ir_integer_interpreter.execute_program ~globals ~max_steps:100
          ~max_frame_bytes:100 ~max_call_depth:4 ~functions:[] entry
      with
      | Error (error :: _) ->
          Alcotest.(check string)
            "missing initialization context" "HCIRVM0017" error.code;
          Alcotest.(check int)
            "missing initialization fails before execution" 0
            error.executed_steps
      | _ ->
          Alcotest.fail
            "initializer-bearing storage executed without its initialization \
             context")
    G.modes

let execute_compiled compiled =
  Ir_integer_interpreter.execute_program
    ~globals:(integer_program_globals compiled)
    ~initialization:(integer_program_initialization compiled)
    ~functions:(integer_program_functions compiled)
    ~max_steps:10000 ~max_frame_bytes:1024 ~max_call_depth:16
    (integer_program_entry compiled)

let region_execution () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 G=1;I64 Next(){G=G+1;return G;}I64 H=Next();(H*10+G);"
      in
      List.iter
        (fun _ ->
          match execute_compiled compiled with
          | Ok result ->
              Alcotest.(check (option int64))
                "each execution owns fresh global words" (Some 22L)
                (Option.map
                   (fun (word : Ir_integer_interpreter.word) -> word.bits)
                   (Ir_integer_interpreter.final_value result))
          | Error errors -> Alcotest.fail (List.hd errors).message)
        [ (); () ];
      let foreign =
        G.compile ~mode
          "I64 G=1;I64 Next(){G=G+1;return G;}I64 H=Next();(H*10+G);"
      in
      (match
         Ir_integer_interpreter.execute_program
           ~globals:(integer_program_globals compiled)
           ~initialization:(integer_program_initialization foreign)
           ~functions:(integer_program_functions compiled)
           ~max_steps:10000 ~max_frame_bytes:1024 ~max_call_depth:16
           (integer_program_entry compiled)
       with
      | Error (error :: _) ->
          Alcotest.(check string)
            "foreign same-spelling initialization context" "HCIRVM0017"
            error.code;
          Alcotest.(check int)
            "foreign context fails before effects" 0 error.executed_steps
      | _ -> Alcotest.fail "foreign initialization context accepted");
      let failed =
        G.compile ~mode "I64 Fail(I64 d){return 1/d;}I64 G=Fail(0);"
      in
      match execute_compiled failed with
      | Error (error :: _) ->
          Alcotest.(check (option string))
            "active callee retained" (Some "Fail") error.function_name;
          Alcotest.(check (option string))
            "initializer owner retained through call" (Some "G")
            error.initializer_name;
          Alcotest.(check (option string))
            "initializer phase retained through call"
            (Some
               (match mode with
               | Preprocessor.Jit -> "compile-initializer"
               | Aot -> "load-initializer"))
            (Option.map Ir_global_initialization.phase_name
               error.initializer_phase)
      | _ -> Alcotest.fail "initializer division by zero did not fault")
    G.modes

let constant_images () =
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode "I64 A=41+1;U64 B=-1;I64 C=~0;" in
      let slots =
        integer_program_globals compiled |> Ir_integer_globals.slots
      in
      Alcotest.(check (list (option int64)))
        "constant expressions materialize their bits"
        [ Some 42L; Some (-1L); Some (-1L) ]
        (List.map Ir_integer_globals.slot_initial_bits slots);
      Alcotest.(check bool)
        "all constant initializers are prepared" true
        (List.for_all Ir_integer_globals.slot_initializer_materialized slots);
      Alcotest.(check int)
        "constant images need no scheduled stores" 0
        (Ir_global_initialization.regions
           (integer_program_initialization compiled)
        |> List.length);
      (match
         Ir_integer_interpreter.execute_program
           ~globals:(integer_program_globals compiled)
           ~functions:[] ~max_steps:100 ~max_frame_bytes:8 ~max_call_depth:1
           (integer_program_entry compiled)
       with
      | Error (error :: _) ->
          Alcotest.(check string)
            "constant images still require their initialization context"
            "HCIRVM0017" error.code;
          Alcotest.(check int)
            "missing constant context fails before effects" 0
            error.executed_steps
      | _ ->
          Alcotest.fail
            "constant image executed without its initialization context");
      match execute_compiled compiled with
      | Ok result ->
          Alcotest.(check bool)
            "declarations supply no final expression" true
            (Ir_integer_interpreter.final_value result = None)
      | Error errors -> Alcotest.fail (List.hd errors).message)
    G.modes

let preparation_limits_and_domain () =
  List.iter
    (fun mode ->
      let text = "I64 A=41+1;U64 B=-1;(A+B);" in
      let compiled = G.compile ~mode text in
      let count =
        Integer_initializer_preparation.executed_steps
          (integer_program_initializer_preparation compiled)
      in
      Alcotest.(check int)
        "both constant harnesses consume exact preparation steps" 9 count;
      let run limit text =
        let session, config, source = F.inputs ~mode text in
        run_integer_program ~max_initializer_steps:limit ~max_steps:10000
          session ~config ~source
      in
      let result =
        run count text |> F.expect ~type_:Ir_integer_interpreter.U64 41L
      in
      Alcotest.(check int)
        "preparation count stays separate from execution" count
        (Ir_integer_interpreter.compiled_initializer_steps result);
      Alcotest.(check string)
        "total preparation budget is exact" "HCIRVM0007"
        (F.first_error (run (count - 1) text)).code;
      List.iter
        (fun limit ->
          Alcotest.(check string)
            "invalid preparation budget precedes parsing" "HCIRVM0001"
            (F.first_error (run limit "@invalid")).code)
        [ 0; -1 ];
      List.iter
        (fun (text, expected) -> ignore (G.run ~mode text |> F.expect expected))
        [
          ("I64 G=(-3)/2;G;", -1L);
          ("I64 G=(2+3)*8+2;G;", 42L);
          ("I64 G=(1<2<3);G;", 1L);
          ("I64 G=1;I64 H=(G=7)+1;(G*10+H);", 78L);
        ];
      List.iter
        (fun text ->
          Alcotest.(check string)
            "optimizer boundary remains explicit" "HCRUN0006"
            (F.first_error (G.run ~mode text)).code)
        [
          "I64 G=-3;I64 H=G/2;H;";
          "I64 Half(I64 n){return n/2;}I64 G=Half(-3);G;";
          "I64 Half(I64 n){return n/2;}I64 Outer(I64 n){return Half(n);}I64 \
           G=Outer(-3);G;";
          "I64 G=1<<3;G;";
          "I64 G=1;I64 H=(G<<63)<<1;H;";
        ];
      let error = F.first_error (G.run ~mode "I64 G=1/0;G;") in
      Alcotest.(check string)
        "faulting constants fail during bounded preparation" "HCIRVM0009"
        error.code;
      Alcotest.(check bool)
        "constant fault phase" true
        (List.mem "initializer_phase=constant-preparation" error.notes))
    G.modes

let scheduling_boundaries () =
  ignore (G.run ~mode:Preprocessor.Aot "I64 G=G+1;G;" |> F.expect 1L);
  let error = F.first_error (G.run "I64 G=G+1;G;") in
  Alcotest.(check string)
    "JIT self-read remains unknown" "HCIRVM0012" error.code;
  Alcotest.(check bool)
    "JIT self-read has its initialization phase" true
    (List.mem "initializer_phase=compile-initializer" error.notes);
  List.iter
    (fun mode ->
      ignore (F.first_error (G.run ~mode "I64 A=B,B=1;A;"));
      let text =
        "I64 Count=0;I64 F(I64 n){if(n){Count=Count+1;return F(n-1);}return \
         Count;}I64 G=F(5);G;"
      in
      let result = G.run ~mode text |> F.expect 5L in
      let steps = Ir_integer_interpreter.executed_steps result in
      ignore (G.run ~mode ~max_steps:steps text |> F.expect 5L);
      Alcotest.(check string)
        "initializer calls share execution budget" "HCIRVM0007"
        (F.first_error (G.run ~mode ~max_steps:(steps - 1) text)).code;
      Alcotest.(check string)
        "initializer calls share depth budget" "HCIRVM0015"
        (F.first_error (G.run ~mode ~max_call_depth:5 text)).code;
      Alcotest.(check string)
        "initializer calls share frame budget" "HCIRVM0011"
        (F.first_error (G.run ~mode ~max_frame_bytes:47 text)).code;
      Alcotest.(check string)
        "initializer objects share global budget" "HCIRVM0016"
        (F.first_error (G.run ~mode ~max_global_bytes:15 text)).code)
    G.modes

let malformed_regions () =
  let module Initial = Ir_global_initialization in
  let module Seq = Ir_instruction_sequence in
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode "7;I64 U;I64 G=U+1;G;" in
      let entry = integer_program_entry compiled in
      let globals = integer_program_globals compiled in
      let descriptions =
        Initial.regions (integer_program_initialization compiled)
        |> List.map Initial.describe
      in
      let description = List.hd descriptions in
      let span =
        match
          Semantic_function_call_expression_result.result_origin
            (Semantic_function_call_expression_result.top_level_root_value
               description.root)
        with
        | Semantic_symbol.Source_location location -> location.span
        | _ -> assert false
      in
      let check message entry descriptions =
        Alcotest.(check bool)
          message true
          (Result.is_error (Initial.create ~span ~globals ~entry descriptions))
      in
      check "missing pending initializer region" entry [];
      check "duplicated initializer region" entry (descriptions @ descriptions);
      let earlier =
        entry |> Ir_x87_stack.graph |> Ir_block_graph.blocks
        |> List.concat_map (fun block ->
            Ir_block_graph.instructions block
            |> Seq.instructions |> List.map Seq.description)
        |> List.find (fun (item : Seq.description) ->
            item.payload = Some (Seq.Integer 7L))
      in
      let earlier = (Option.get earlier.result).value_id in
      let changed =
        F.rewrite_entry
          (fun (item : Seq.description) ->
            if item.opcode = Ir_opcode.Ic_assign then
              { item with operands = [ List.hd item.operands; earlier ] }
            else item)
          entry
      in
      check "initializer cannot borrow an ordinary expression value" changed
        descriptions;
      let reordered =
        F.rewrite_entry
          (fun (item : Seq.description) ->
            {
              item with
              instruction_id =
                Result.get_ok
                  (Seq.Instruction_id.of_int
                     (1000 - Seq.Instruction_id.to_int item.instruction_id));
            })
          entry
      in
      check "valid renumbered graph must obey initializer physical ordering"
        reordered descriptions;
      let changed =
        F.rewrite_entry
          (fun (item : Seq.description) ->
            if item.opcode = Ir_opcode.Ic_assign then { item with flags = 1L }
            else item)
          entry
      in
      check "initializer destination store has canonical flags" changed
        descriptions)
    G.modes

let external_call_scope rhs leaks_call_result () =
  let module Seq = Ir_instruction_sequence in
  let module Initial = Ir_global_initialization in
  List.iter
    (fun mode ->
      List.iter
        (fun (rhs, leaks_call_result) ->
          let compiled =
            G.compile ~mode
              ("I64 Id(I64 n){return n;}I64 U;Id(9);I64 G=" ^ rhs ^ ";G;")
          in
          let globals = integer_program_globals compiled in
          let original = integer_program_entry compiled |> Ir_x87_stack.graph in
          let region =
            List.hd (Initial.regions (integer_program_initialization compiled))
            |> Initial.describe
          in
          let block = Ir_block_graph.entry original in
          let code =
            Ir_block_graph.instructions block
            |> Seq.instructions |> List.map Seq.description
          in
          let before, region_and_after =
            let rec split reversed = function
              | (item : Seq.description) :: _ as rest
                when Seq.Instruction_id.equal item.instruction_id region.first
                -> (List.rev reversed, rest)
              | item :: rest -> split (item :: reversed) rest
              | [] -> assert false
            in
            split [] code
          in
          let start = List.hd before in
          let argument = List.nth before 1 in
          let closing = List.tl (List.tl before) in
          let moved =
            let rec place = function
              | [] -> assert false
              | (item : Seq.description) :: rest
                when Seq.Instruction_id.equal item.instruction_id region.last ->
                  (item :: closing) @ rest
              | item :: rest -> item :: place rest
            in
            start :: { argument with flags = 0L } :: place region_and_after
          in
          let inside (item : Seq.description) =
            Seq.Instruction_id.compare item.instruction_id region.first >= 0
            && Seq.Instruction_id.compare item.instruction_id region.last <= 0
          in
          let candidate =
            moved
            |> List.filter (fun (item : Seq.description) ->
                inside item
                &&
                if leaks_call_result then item.opcode = Ir_opcode.Ic_call_end
                else item.payload = Some (Seq.Integer 1L))
            |> List.rev |> List.hd
          in
          let new_first = ref None and new_last = ref None in
          let rewritten =
            moved
            |> List.mapi (fun index (item : Seq.description) ->
                let instruction_id =
                  Result.get_ok (Seq.Instruction_id.of_int index)
                in
                if Seq.Instruction_id.equal item.instruction_id region.first
                then new_first := Some instruction_id;
                if Seq.Instruction_id.equal item.instruction_id region.last then
                  new_last := Some instruction_id;
                let flags =
                  if item == candidate then Int64.logor item.flags 0x2000L
                  else item.flags
                in
                { item with instruction_id; flags })
          in
          let entry =
            Ir_block_graph.create
              ~entry:(Ir_block_graph.block_id block)
              [
                {
                  Ir_block_graph.block_id = Ir_block_graph.block_id block;
                  instructions = rewritten;
                };
              ]
            |> Result.get_ok |> Ir_x87_stack.verify |> Result.get_ok
          in
          let region =
            {
              region with
              first = Option.get !new_first;
              last = Option.get !new_last;
            }
          in
          let span = Option.get candidate.span in
          match Initial.create ~span ~globals ~entry [ region ] with
          | Error _ -> ()
          | Ok initialization ->
              (* The enclosing call is otherwise a valid checked VM program; the
             initialization boundary, rather than generic call validation,
             must reject these escaping pushes. *)
              let result =
                Ir_integer_interpreter.execute_program ~globals ~initialization
                  ~functions:(integer_program_functions compiled)
                  ~max_steps:1000 ~max_frame_bytes:1024 ~max_call_depth:16 entry
              in
              if mode = Preprocessor.Aot then
                Alcotest.(check bool)
                  "crafted surrounding call is valid" true (Result.is_ok result);
              Alcotest.fail
                (if leaks_call_result then
                   "initializer CALL_END pushed into an outside call"
                 else "initializer literal pushed into an outside call"))
        [ (rhs, leaks_call_result) ])
    G.modes

let source_query_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, before) ->
          let session, config, source =
            Test_integer_program.inputs ~mode text
          in
          Alcotest.(check bool)
            "source query requires original compiler metadata" true
            (compile_integer_program session ~config ~source |> Result.is_error);
          Alcotest.(check bool)
            "member-token directive has native reachability" before
            (Definition.Environment.find (Session.definitions session) "BEFORE"
            |> Option.is_some);
          Alcotest.(check bool)
            "error precedes the next directive" false
            (Definition.Environment.find (Session.definitions session) "AFTER"
            |> Option.is_some))
        [
          ("sizeof Missing\n#define AFTER 1\n;", false);
          ("sizeof U8.\n#define AFTER 1\nmember;", false);
          ("sizeof I64.\n#define BEFORE 1\nmember\n#define AFTER 1\n;", true);
          ("I64 F(){I64i Local;sizeof Local.\n#define AFTER 1\nmember;}", false);
          ( "I64 F(){I64 Local;sizeof Local.\n\
             #define BEFORE 1\n\
             member\n\
             #define AFTER 1\n\
             ;}",
            true );
        ])
    G.modes

let source_function_pointer_queries () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let session, config, source =
            Test_integer_program.inputs ~mode text
          in
          ignore
            (compile_integer_program session ~config ~source
            |> Test_integer_program.checked))
        [
          "I64 F(){I64 (*P)();return sizeof P+34;}F();";
          "I64 F(I64 (*P)()){return sizeof P+34;}";
          "I64 F(){I64 (*P)();return sizeof P*+34;}F();";
        ];
      match G.run ~mode "I64 F(){I64 (*P)();return sizeof P+34;}F();" with
      | Error (diagnostic :: _) ->
          Alcotest.(check string)
            "function-pointer storage retains its VM preflight boundary"
            "HCIRVM0011" diagnostic.Diagnostic.code
      | _ -> Alcotest.fail "expected unsupported function-pointer frame")
    G.modes

let tests =
  [
    Alcotest.test_case "source function-pointer sizeof preserves compilation"
      `Quick source_function_pointer_queries;
  ]
  @ List.map
      (fun (name, source) ->
        Alcotest.test_case name `Quick (fun () ->
            List.iter
              (fun mode -> ignore (G.run ~mode source |> F.expect 42L))
              G.modes))
      [
        ("source query retains internal pointer size", "sizeof I64i*+34;");
        ("source query reads its published global", "I64 N=sizeof N;N+34;");
        ( "source dimension consumes original query",
          "I64 A[sizeof U8*];A[0]=42;A[0];" );
        ("source query retains keyword presence", "defined return+41;");
        ( "source query preserves absence before later declaration",
          "I64 N=defined Future;I64 Future;N+42;" );
        ("source query reads public primitive class", "sizeof I64+34;");
        ( "source query reads scalar local",
          "I64 F(){I64 N=sizeof N;return N+34;}F();" );
        ("source query reads parameter", "I64 F(U8 N){return sizeof N+41;}F(0);");
        ( "source query reads global inside function",
          "I64 N;I64 F(){return sizeof N+34;}F();" );
      ]
  @ [
      Alcotest.test_case
        "source query errors preserve native directive reachability" `Quick
        source_query_boundaries;
      Alcotest.test_case "initialized source accumulator" `Quick source_gate;
      Alcotest.test_case "initializer source order and calls" `Quick
        source_order;
      Alcotest.test_case "checked declaration roots and calls" `Quick
        checked_roots;
      Alcotest.test_case "foreign declaration roots" `Quick foreign_roots;
      Alcotest.test_case "checked storage roots and initializer stores" `Quick
        storage_roots;
      Alcotest.test_case "region identity, repeated execution and callee faults"
        `Quick region_execution;
      Alcotest.test_case "constant initial images" `Quick constant_images;
      Alcotest.test_case "preparation budgets and optimizer domain" `Quick
        preparation_limits_and_domain;
      Alcotest.test_case "scheduled initialization bounds and self-reads" `Quick
        scheduling_boundaries;
      Alcotest.test_case "malformed initializer regions" `Quick
        malformed_regions;
      Alcotest.test_case "initializer literal cannot push to an external call"
        `Quick
        (external_call_scope "U+1" false);
      Alcotest.test_case
        "initializer call result cannot push to an external call" `Quick
        (external_call_scope "Id(U+1)" true);
    ]
