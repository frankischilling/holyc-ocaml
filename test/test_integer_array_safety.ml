open Holyc_lib
module G = Test_integer_globals
module F = Test_integer_functions
module H = Test_ir_integer_interpreter
module Output = Test_integer_output
module Bytes = Test_integer_persistent_bytes
module SI = Test_integer_static_initializers
module Initial = Ir_global_initialization
module Seq = Ir_instruction_sequence
module VM = Ir_integer_interpreter

let region_descriptions compiled =
  Initial.regions (integer_program_initialization compiled)
  |> List.map Initial.describe

let publications compiled =
  Initial.publications (integer_program_initialization compiled)
  |> List.map Initial.describe_publication

let context compiled entry pubs regions =
  let code = SI.code entry in
  Initial.create
    ~span:(Option.get (List.hd code).Seq.span)
    ~globals:(integer_program_globals compiled)
    ~entry ~publications:pubs
    ?publication_evidence:
      (Initial.publication_evidence (integer_program_initialization compiled))
    ~static_descriptions:(SI.descriptions compiled) regions

let publication_evidence () =
  let shifted = G.compile "I64 A[1]={1};A[0]=42;A[0];" in
  let shifted_entry = integer_program_entry shifted in
  let last_instruction = SI.code shifted_entry |> List.rev |> List.hd in
  let moved =
    List.map
      (fun (description : Initial.publication_description) ->
        { description with before = last_instruction.instruction_id })
      (publications shifted)
  in
  Alcotest.(check bool)
    "publication moved across ordinary statements" true
    (Result.is_error
       (context shifted shifted_entry moved (region_descriptions shifted)));
  let source = "I64 G=0;I64 A[3]={40,(G=1),2};A[0]+A[2];" in
  let compiled = G.compile source in
  let entry = integer_program_entry compiled in
  let pubs = publications compiled in
  let regions = region_descriptions compiled in
  Alcotest.(check int) "two distinct prepared leaves" 2 (List.length pubs);
  ignore (context compiled entry pubs regions |> F.checked);
  let reject label pubs =
    Alcotest.(check bool)
      label true
      (Result.is_error (context compiled entry pubs regions))
  in
  reject "missing publication" [];
  reject "duplicate publication" (pubs @ pubs);
  reject "reordered publications" (List.rev pubs);
  reject "foreign equal-source publication" (publications (G.compile source));
  let first = List.hd pubs and last = List.hd (List.rev pubs) in
  reject "earlier constant moved after scheduled leaf"
    [ { first with before = last.before }; last ];
  let absent =
    Seq.Instruction_id.of_int 100000
    |> H.require_ok (fun (error : Seq.error) -> error.message)
  in
  reject "absent entry instruction" [ { first with before = absent }; last ];
  Test_integer_statics.require_preflight
    (VM.execute_program
       ~globals:(integer_program_globals compiled)
       ~functions:(integer_program_functions compiled)
       ~max_steps:1000 ~max_frame_bytes:1024 ~max_call_depth:16 entry);
  let aot = G.compile ~mode:Preprocessor.Aot source in
  Alcotest.(check int)
    "AOT constants need no entry publication" 0
    (List.length (publications aot));
  ignore
    (context aot (integer_program_entry aot) [] (region_descriptions aot)
    |> F.checked)

let receipt_controls () =
  let compiled = G.compile "I64 A[2]={40,2};A[0]+A[1];" in
  let entry = integer_program_entry compiled in
  let globals = integer_program_globals compiled in
  let pubs = publications compiled in
  let regions = region_descriptions compiled in
  let span = Option.get (List.hd (SI.code entry)).Seq.span in
  let receipt =
    Initial.publication_evidence (integer_program_initialization compiled)
  in
  let reject label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  Alcotest.(check bool)
    "constant array retains its receipt" true (Option.is_some receipt);
  ignore (context compiled entry pubs regions |> F.checked);
  reject "exact publications require their receipt"
    (Initial.create ~span ~globals ~entry ~publications:pubs regions);
  let clone = F.rewrite_entry Fun.id entry in
  Alcotest.(check bool)
    "cloned entry preserves every instruction" true
    (SI.code entry = SI.code clone);
  Alcotest.(check bool)
    "cloned entry has a distinct identity" true (entry != clone);
  reject "receipt rejects an equal-code cloned entry"
    (context compiled clone pubs regions);
  let foreign_globals =
    G.compile "I64 A[2]={40,2};A[0]+A[1];" |> integer_program_globals
  in
  reject "receipt rejects foreign equal-source storage"
    (Initial.create ~span ~globals:foreign_globals ~entry ~publications:pubs
       ?publication_evidence:receipt regions);
  let statics =
    G.compile
      "I64 F(){static I64 A[1]={40};static I64 B[1]={2};return A[0]+B[0];}F();"
  in
  let static_entry = integer_program_entry statics in
  let static_pubs = publications statics in
  ignore
    (context statics static_entry static_pubs (region_descriptions statics)
    |> F.checked);
  (match static_pubs with
  | [
   ({ prepared_root = Initial.Prepared_static (left_slot, left_root); _ } as
    left);
   ({ prepared_root = Initial.Prepared_static (right_slot, right_root); _ } as
    right);
  ] ->
      let changed root = [ { left with prepared_root = root }; right ] in
      reject "receipt rejects a substituted static root"
        (context statics static_entry
           (changed (Initial.Prepared_static (left_slot, right_root)))
           (region_descriptions statics));
      reject "receipt rejects a substituted static slot"
        (context statics static_entry
           (changed (Initial.Prepared_static (right_slot, left_root)))
           (region_descriptions statics))
  | _ -> Alcotest.fail "expected two distinct static array publications");
  let module Lower = Ir_integer_program_lowering in
  let statements =
    List.map
      (fun (description : Initial.publication_description) ->
        Lower.Publish_array description.prepared_root)
      pubs
  in
  let complete = Lower.lower_complete ~globals ~span statements |> F.checked in
  Alcotest.(check bool)
    "complete lowering retains publication evidence" true
    (Option.is_some (Lower.publication_evidence complete));
  ignore
    (Initial.create ~span ~globals ~entry:(Lower.graph complete)
       ~publications:(Lower.publications complete)
       ?publication_evidence:(Lower.publication_evidence complete)
       (Lower.initializer_regions complete)
    |> F.checked);
  reject "storage-region wrapper cannot discard publications"
    (Lower.lower_with_storage_initializers ~globals ~span statements);
  reject "global-region wrapper cannot discard publications"
    (Lower.lower_with_initializers ~globals ~span statements);
  reject "graph-only wrapper cannot discard publications"
    (Lower.lower ~globals ~span statements)

let destination_evidence () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode "I64 G=40;I64 A[2][2]={{G,G},{G,2}};A[1][0]+A[1][1];"
      in
      let entry = integer_program_entry compiled in
      let regions = region_descriptions compiled
      and pubs = publications compiled in
      let first = List.hd regions in
      ignore (context compiled entry pubs regions |> F.checked);
      let reject label entry regions =
        Alcotest.(check bool)
          label true
          (Result.is_error (context compiled entry pubs regions))
      in
      reject "missing array leaf region" entry (List.tl regions);
      reject "reordered array leaf regions" entry (List.rev regions);
      reject "duplicate array leaf region" entry (first :: regions);
      let base = Seq.Instruction_id.to_int first.first in
      List.iter
        (fun (label, offset, payload) ->
          let changed =
            F.rewrite_entry
              (fun (instruction : Seq.description) ->
                if
                  Seq.Instruction_id.to_int instruction.instruction_id
                  = base + offset
                then { instruction with payload = Some (Seq.Integer payload) }
                else instruction)
              entry
          in
          reject label changed regions)
        [
          ("wrong outer coordinate", 2, 1L);
          ("wrong inner coordinate", 6, 1L);
          ("wrong declared stride", 1, 8L);
        ])
    G.modes

let exact_budgets () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, persistent) ->
          let result = Output.run ~mode source |> Bytes.expect "42" in
          let runtime = VM.executed_steps result
          and preparation = VM.compiled_initializer_steps result in
          ignore
            (Output.run ~mode ~max_steps:runtime
               ~max_initializer_steps:preparation ~max_global_bytes:persistent
               source
            |> Bytes.expect "42");
          ignore
            (Output.run ~mode ~max_global_bytes:(persistent - 1) source
            |> Output.fault "HCIRVM0016");
          ignore
            (Output.run ~mode ~max_steps:(runtime - 1) source
            |> Output.fault ~output:"42" "HCIRVM0007");
          ignore
            (Output.run ~mode ~max_initializer_steps:(preparation - 1) source
            |> Output.fault "HCIRVM0007"))
        [
          ( "extern U0 Print(U8 *fmt,...);U8 A[3]=\"42\";I64 \
             N[2]={40,2};Print(\"%s\",A);N[0]+N[1];",
            19 );
          ( "extern U0 Print(U8 *fmt,...);I64 F(){static U8 A[3]=\"42\";static \
             I64 N[2]={40,2};Print(\"%s\",A);return N[0]+N[1];}F();",
            24 );
        ])
    G.modes

let prior_effects () =
  List.iter
    (fun mode ->
      let report =
        Output.run ~mode
          "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('A');return 40;}I64 \
           Zero(){return 0;}I64 A[2]={Seed(),1/Zero()};42;"
      in
      ignore (Output.fault ~output:"A" "HCIRVM0009" report);
      Alcotest.(check string)
        "earlier initializer output survives fault" "A"
        (integer_program_report_output_bytes report))
    G.modes

let tests =
  [
    Alcotest.test_case "complete ordered publications" `Quick
      publication_evidence;
    Alcotest.test_case "publication receipt and legacy wrapper controls" `Quick
      receipt_controls;
    Alcotest.test_case "canonical array leaf destinations" `Quick
      destination_evidence;
    Alcotest.test_case "initialized array exact budgets" `Quick exact_budgets;
    Alcotest.test_case "earlier initializer effects on faults" `Quick
      prior_effects;
  ]
