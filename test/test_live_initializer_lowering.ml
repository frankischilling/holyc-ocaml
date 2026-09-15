open Holyc_lib
module Destination = Ir_initializer_fragment_destination
module Layout = Ir_integer_initializer_layout
module Address = Ir_global_address_lowering
module Sequence = Ir_instruction_sequence
module Lower = Ir_integer_program_lowering
module Initialization = Ir_global_initialization
module Program = Ir_initializer_fragment_program

let checked = function
  | Ok value -> value
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map
              (fun (error : Sequence.error) ->
                error.code ^ ": " ^ error.message)
              errors))

let expect = Test_integer_program.checked

let ordinary_fragment_rejected () =
  let module VM = Ir_integer_interpreter in
  let module D = Task_declarations in
  let module Typing = Holyc_lib__Driver__Initializer_fragment_typing in
  let module Fragment_lowering =
    Holyc_lib__Driver__Initializer_fragment_lowering
  in
  let checked = Test_declaration_collection.checked in
  let session, source, ledger = Test_source_promotion.inputs {|I64 N=42;|} in
  let runtime =
    VM.create_task_state ~table:(Session.semantic_symbols session) () |> checked
  in
  let cursor = ref None in
  let declaration event =
    Result.bind (D.observe ledger event) (fun () ->
        match event with
        | Parser.Global_declared publication ->
            D.admit_global ledger ~runtime publication
        | Parser.Global_initializer_started start ->
            let declaration =
              D.initializer_declaration ledger start |> expect
            in
            cursor := Some (Layout.begin_live declaration |> checked);
            Ok ()
        | Parser.Global_initializer_leaf_completed receipt ->
            let view = VM.task_snapshot runtime |> checked in
            let authority =
              D.initializer_fragment_authority ledger ~runtime ~task_view:view
                receipt
              |> expect
            in
            let fragment =
              Semantic_initializer_fragment.authorized_fragment authority
            in
            let context =
              Typing.create_context
                ~table:(Session.semantic_symbols session)
                ~parent:(D.initializer_scope ledger)
              |> checked
            in
            let typed = Typing.prepare context fragment |> checked in
            let _, layout =
              Layout.prepare_live (Option.get !cursor)
                (Semantic_initializer_fragment.leaf fragment)
              |> checked
            in
            let symbol =
              Semantic_compiler_record.declared_global_symbol
                (Semantic_initializer_fragment.declaration fragment)
            in
            let reference, slot =
              match VM.admitted_publication_for_symbol runtime symbol with
              | Some (VM.Admitted_declared_global (reference, slot)) ->
                  (reference, slot)
              | _ -> Alcotest.fail "missing original storage"
            in
            let destination =
              Destination.create ~task_view:view ~reference ~slot ~layout typed
              |> checked
            in
            let program =
              Fragment_lowering.lower ~context ~authority destination |> expect
            in
            let before = VM.task_progress runtime in
            let execute () =
              VM.execute_task_program runtime
                ~runtime_calls:(Program.runtime_calls program)
                ~globals:(Destination.globals destination)
                ~initialization:(Program.initialization program)
                ~functions:[] (Program.entry program)
            in
            List.iter
              (fun () ->
                Alcotest.(check bool)
                  "ordinary task entry rejects fragment execution" true
                  (Result.is_error (execute ()));
                Alcotest.(check bool)
                  "rejection leaves all runtime accounting unchanged" true
                  (before = VM.task_progress runtime))
              [ (); () ];
            Ok ()
        | _ -> Ok ())
  in
  let checkpoint event =
    Result.bind (D.observe_command ledger event) (fun () ->
        match event with
        | Parser.Sequence_started _ ->
            D.promote_source ledger ~runtime session ~source |> checked;
            Ok ()
        | _ -> Ok ())
  in
  Test_source_promotion.parse ~declaration ~checkpoint session source ledger
  |> Test_parser.expect_ast |> ignore

let destinations text expected =
  let reached = ref [] in
  let on_leaf task receipt entry =
    let program =
      Integer_task.lower_initializer_fragment task ~destination:entry receipt
      |> expect
    in
    let destination = Program.destination program in
    let globals = Destination.globals destination in
    Alcotest.(check int)
      "fragment allocates no storage" 0
      (Ir_integer_globals.byte_size globals);
    Alcotest.(check bool)
      "retains original destination leaf" true
      (Layout.leaf (Destination.layout destination) == Layout.leaf entry);
    let address = Address.prepare_fragment_initializer destination |> checked in
    let lowered =
      Address.lower_prepared
        ~instruction_id:(Sequence.Instruction_id.of_int 0 |> Result.get_ok)
        ~value_id:(Sequence.Value_id.of_int 0 |> Result.get_ok)
        address
      |> checked
    in
    let code =
      Address.sequence lowered |> Sequence.instructions
      |> List.map Sequence.description
    in
    let first = List.hd code in
    Alcotest.(check bool)
      "declared storage uses retained JIT address" true
      (first.opcode = Ir_opcode.Ic_imm_i64
      &&
      match first.payload with
      | Some (Sequence.Retained_global reference) ->
          reference == Destination.reference destination
      | _ -> false);
    let initialization = Program.initialization program in
    let region =
      match Initialization.storage_regions initialization with
      | [ region ] -> region
      | _ -> Alcotest.fail "expected one checked storage region"
    in
    Alcotest.(check bool)
      "region retains original typed root" true
      (Option.get (Initialization.storage_root region)
      == Destination.root destination);
    Alcotest.(check bool)
      "fragment has compile-initializer phase" true
      (Initialization.storage_phase region = Initialization.Compile_initializer);
    Alcotest.(check int)
      "lowering allocates no completed global" 0
      (List.length (Ir_integer_globals.slots globals));
    reached := (Layout.byte_offset entry, List.length code) :: !reached;
    Ok ()
  in
  let parsed, _, _ = Test_live_initializer_layout.parse ~on_leaf text in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (list (pair int int)))
    "exact cell address prefixes" expected (List.rev !reached)

let tests =
  [
    Alcotest.test_case "ordinary task rejects fragment storage" `Quick
      ordinary_fragment_rejected;
    Alcotest.test_case "scalar retained destination" `Quick (fun () ->
        destinations {|I64 N=42;|} [ (0, 1) ]);
    Alcotest.test_case "ranked retained destinations" `Quick (fun () ->
        destinations {|I64 A[2][2]={{1,2},{3,4}};|}
          [ (0, 9); (8, 9); (16, 9); (24, 9) ]);
    Alcotest.test_case "retained call initializer region" `Quick (fun () ->
        destinations {|I64 Add(I64 a,I64 b){return a+b;};I64 N=Add(20,22);|}
          [ (0, 1) ]);
  ]
