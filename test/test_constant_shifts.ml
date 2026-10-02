open Holyc_lib
module F = Test_native_expression
module VM = Ir_integer_interpreter
module Native = X86_64_expression
module Program = X86_64_program
module Encoder = X86_64_encoder
module Sequence = Ir_instruction_sequence
module Opcode = Ir_opcode
module Type = Semantic_type
open Yojson.Safe.Util

let field json name = json |> member name |> to_string

let fixture () =
  [
    "oracle/constant-shifts.json";
    "../oracle/constant-shifts.json";
    "test/oracle/constant-shifts.json";
    "../test/oracle/constant-shifts.json";
  ]
  |> List.find_opt Sys.file_exists
  |> function
  | Some path -> Yojson.Safe.from_file path
  | None -> Alcotest.fail "constant-shift native fixture is missing"

let projections fixture =
  fixture |> member "hosted_value_projections" |> to_list

let semantic_type = function
  | "I64" -> F.i64
  | "U64" -> F.u64
  | s -> Alcotest.fail s

let word_type type_ = if Type.equal type_ F.u64 then VM.U64 else VM.I64

let observed fixture projection case_field =
  let case_id = field projection case_field in
  let check =
    fixture |> member "checks" |> to_list
    |> List.find (fun check -> field check "id" = case_id)
  in
  let prefix = field projection "field" ^ "=" in
  check |> member "observed_output" |> to_list |> List.map to_string
  |> List.concat_map (String.split_on_char ' ')
  |> List.filter (String.starts_with ~prefix)
  |> function
  | [ value ] ->
      Int64.of_string
        ("0x"
        ^ String.sub value (String.length prefix)
            (String.length value - String.length prefix))
  | _ -> Alcotest.fail (case_id ^ " requires exactly one observed field")

let constant id opcode operand type_ count =
  F.description ~operands:[ operand ] ~result:id ~target_type:type_
    ~payload:(Sequence.Integer count) id opcode

let projected_graph projection =
  let type_ = ref (semantic_type (field projection "input_type")) in
  let instructions =
    ref
      [
        F.imm ~type_:!type_ 0 (Int64.of_string (field projection "input_bits"));
      ]
  in
  let value = ref 0 and next = ref 1 in
  let emit description =
    instructions := description :: !instructions;
    incr next
  in
  projection |> member "operations" |> to_list
  |> List.iter (fun operation ->
      match field operation "kind" with
      | "constant-shift" ->
          let opcode =
            if field operation "direction" = "left" then Opcode.Ic_shl_const
            else Opcode.Ic_shr_const
          in
          type_ := semantic_type (field operation "type");
          let result = !next in
          emit
            (constant result opcode !value !type_
               (Int64.of_string (field operation "count")));
          value := result
      | "raw-shift" ->
          let opcode =
            if field operation "direction" = "left" then Opcode.Ic_shl
            else Opcode.Ic_shr
          in
          let count = !next in
          emit
            (F.imm
               ~type_:(semantic_type (field operation "count_type"))
               count
               (Int64.of_string (field operation "count")));
          type_ := semantic_type (field operation "type");
          let result = !next in
          emit (F.binary ~type_:!type_ result opcode !value count);
          value := result
      | "word-view" ->
          type_ := semantic_type (field operation "type");
          let result = !next in
          emit (F.word_view ~type_:!type_ result !value);
          value := result
      | "less-zero" ->
          let zero = !next in
          emit (F.imm zero 0L);
          let result = !next in
          emit (F.binary result Opcode.Ic_less !value zero);
          type_ := F.i64;
          value := result
      | kind -> Alcotest.fail ("unknown constant-shift projection " ^ kind));
  emit (F.return_value ~type_:!type_ !next !value);
  emit (F.ret !next);
  (F.single (List.rev !instructions), !type_, !next)

let execution ~max_steps graph =
  match VM.execute ~max_steps graph with
  | Ok result -> result
  | Error errors ->
      Alcotest.fail
        (String.concat "; "
           (List.map (fun (e : VM.error) -> e.code ^ ": " ^ e.message) errors))

let returned result =
  match VM.termination result with
  | VM.Returned (Some word) -> word
  | _ -> Alcotest.fail "constant-shift execution did not return a word"

let program_graph graph =
  F.descriptions graph
  |> List.map (fun (d : Sequence.description) ->
      match d.opcode with
      | Opcode.Ic_return_val ->
          {
            d with
            opcode = Opcode.Ic_end_exp;
            flags = 0x200L;
            target_type = None;
          }
      | Opcode.Ic_ret -> { d with opcode = Opcode.Ic_end }
      | _ -> d)
  |> F.single

let program_image ?status_abi graph =
  Program.compile ?status_abi ~max_ir_instructions:1000 ~max_code_bytes:65536
    graph
  |> F.require_ok F.native_errors

let native_oracle () =
  let fixture = fixture () in
  Alcotest.(check string)
    "reference pin" Version.reference_commit
    (fixture |> member "reference" |> member "commit" |> to_string);
  Alcotest.(check int)
    "49 independent fields" 49
    (List.length (projections fixture));
  let differences =
    projections fixture
    |> List.filter (fun p ->
        not (p |> member "source_pipeline_matches_baseline" |> to_bool))
  in
  Alcotest.(check (list string))
    "explicit source optimizer gap"
    [ "RH1"; "LN64"; "RN64"; "UN64"; "LN128"; "RN128" ]
    (List.map (fun p -> field p "field") differences);
  List.iter
    (fun projection ->
      let label = field projection "field" in
      let expected = observed fixture projection "case_id" in
      Alcotest.(check int64)
        (label ^ " native repeated run")
        expected
        (observed fixture projection "repeat_case_id");
      let graph, type_, count = projected_graph projection in
      let result = execution ~max_steps:count graph in
      let word = returned result in
      Alcotest.(check int64) (label ^ " projected bits") expected word.bits;
      Alcotest.(check bool)
        (label ^ " source-derived class")
        true
        (word.type_ = word_type type_);
      Alcotest.(check int)
        (label ^ " exact instruction work")
        count (VM.executed_steps result);
      List.iter
        (fun status_abi ->
          let image = F.image ~status_abi graph in
          Alcotest.(check int)
            (label ^ " native IC count")
            count
            (Native.ir_instructions image);
          Alcotest.(check bool)
            (label ^ " native class") true
            (Native.value_type image
            = if word.type_ = VM.U64 then Native.U64 else Native.I64);
          ignore (program_image ~status_abi (program_graph graph)))
        [ Native.Windows_x64; Native.System_v_x64 ])
    (projections fixture)

let matrix_and_limits () =
  List.iter
    (fun (opcode, expected) ->
      let graph =
        F.single
          [
            F.imm 0 (-7L);
            constant 1 opcode 0 F.i64 65L;
            F.return_value 2 1;
            F.ret 3;
          ]
      in
      Alcotest.(check int64)
        "count 65" expected (returned (execution ~max_steps:4 graph)).bits;
      (match VM.execute ~max_steps:3 graph with
      | Error [ error ] ->
          Alcotest.(check string) "one below budget" "HCIRVM0007" error.code;
          Alcotest.(check int) "charged work" 3 error.executed_steps;
          Alcotest.(check (option int))
            "stopped before return" (Some 3) error.instruction_id
      | _ -> Alcotest.fail "one below budget published a result");
      let image = F.image graph in
      let size = String.length (Native.code image) in
      ignore
        (F.compile ~max_ir_instructions:4 ~max_code_bytes:size
           ~max_stack_bytes:0 graph
        |> F.require_ok F.native_errors);
      ignore
        (F.compile ~max_ir_instructions:3 graph
        |> F.reject ~code:"HCBACK0001" "IR budget");
      ignore
        (F.compile ~max_code_bytes:(size - 1) graph
        |> F.reject ~code:"HCBACK0005" "code budget"))
    [ (Opcode.Ic_shl_const, -14L); (Opcode.Ic_shr_const, -4L) ];
  let graph =
    F.single
      [
        F.imm ~type_:F.u64 0 Int64.min_int;
        F.unary 1 Opcode.Ic_com 0;
        constant 2 Opcode.Ic_shr_const 1 F.u64 1L;
        F.return_value ~type_:F.u64 3 2;
        F.ret 4;
      ]
  in
  Alcotest.(check int64)
    "forward computation class through complement" 0x3fffffffffffffffL
    (returned (execution ~max_steps:5 graph)).bits;
  ignore (F.image graph)

let malformed () =
  List.iter
    (fun opcode ->
      let valid =
        F.single
          [
            F.imm 0 (-7L);
            constant 1 opcode 0 F.i64 1L;
            F.return_value 2 1;
            F.ret 3;
          ]
      in
      let reject label code replacement =
        let graph = F.replace_instruction 1 replacement valid in
        (match VM.execute ~max_steps:1 graph with
        | Error errors when errors <> [] ->
            Alcotest.(check bool)
              label true
              (List.exists (fun (e : VM.error) -> e.code = code) errors);
            List.iter
              (fun (error : VM.error) ->
                Alcotest.(check int)
                  (label ^ " no execution") 0 error.executed_steps;
                Alcotest.(check bool)
                  (label ^ " preflight") true
                  (error.stage = VM.Preflight))
              errors
        | _ -> Alcotest.fail (label ^ " admitted malformed IR"));
        ignore (F.compile graph |> F.reject label)
      in
      reject "no payload" "HCIRVM0004" (fun d -> { d with payload = None });
      reject "byte payload" "HCIRVM0004" (fun d ->
          { d with payload = Some (Sequence.Bytes "x") });
      List.iter
        (fun operands ->
          let descriptions =
            F.descriptions valid
            |> List.map (fun (d : Sequence.description) ->
                if Sequence.Instruction_id.to_int d.instruction_id = 1 then
                  { d with operands }
                else d)
          in
          match Sequence.create descriptions with
          | Error errors ->
              Alcotest.(check bool)
                "constructor rejects incorrect arity" true
                (List.exists
                   (fun (e : Sequence.error) -> e.code = "HCIR0004")
                   errors)
          | Ok _ ->
              Alcotest.fail
                "constructor admitted incorrect constant-shift arity")
        [ []; [ F.value_id 0; F.value_id 0 ] ];
      reject "wrong surviving class" "HCIRVM0006" (fun d ->
          { d with target_type = Some F.u64 });
      List.iter
        (fun flags -> reject "flags" "HCIRVM0003" (fun d -> { d with flags }))
        [ 1L; 0x80L; 0x2000L ];
      List.iter
        (fun (label, type_) ->
          reject label "HCIRVM0005" (fun d ->
              { d with target_type = Some type_ }))
        [
          ( "public target",
            F.primitive ~form:Type.Public_spelling Primitive_type.I64 );
          ("narrow target", F.primitive Primitive_type.U8);
          ("F64 target", F.primitive Primitive_type.F64);
          ("pointer target", F.primitive ~pointer_depth:1 Primitive_type.I64);
        ])
    [ Opcode.Ic_shl_const; Opcode.Ic_shr_const ]

let encoder_bytes () =
  let open Encoder in
  List.iter
    (fun (instruction, expected) ->
      let bytes = encode instruction in
      Alcotest.(check string)
        "independent immediate bytes" expected (F.hex bytes);
      Alcotest.(check int)
        "measured immediate size"
        (String.length expected / 2)
        (size instruction))
    [
      (Shift_immediate (Shl, Rax, 63L), "48c1e03f");
      (Shift_immediate (Shr, Rax, 63L), "48c1e83f");
      (Shift_immediate (Sar, Rax, 63L), "48c1f83f");
      (Shift_immediate (Sar, R8, 1L), "49d1f8");
      (Shift_immediate (Sar, Rax, 1L), "48d1f8");
      (Shift_immediate (Sar, Rax, 65L), "48c1f841");
      (Shift_immediate (Sar, Rax, -1L), "48c1f8ff");
      (Shift_immediate (Sar, Rax, 0L), "48c1f800");
      (Shift_immediate (Shl, Rax, 64L), "48c1e040");
      (Shift_immediate (Shl, Rax, 128L), "48c1e080");
      (Shift_immediate (Sar, Rax, 64L), "48c1f840");
      (Shift_immediate (Sar, Rax, 128L), "48c1f880");
      (Shift_immediate (Sar, Rax, -9223372036854775807L), "48c1f801");
      (Shift_immediate (Sar, R8, -9223372036854775807L), "49c1f801");
      (Shift_immediate (Shl, Rcx, 1L), "48d1e1");
      (Shift_immediate (Shr, Rdx, 1L), "48d1ea");
      (Shift_immediate (Shr, R9, 0L), "49c1e900");
      (Shift_immediate (Shl, R10, -64L), "49c1e2c0");
      (Shift_immediate (Sar, R11, 257L), "49c1fb01");
    ]

let tests =
  [
    Alcotest.test_case "native fields and canonical IR" `Quick native_oracle;
    Alcotest.test_case "computation class and exact budgets" `Quick
      matrix_and_limits;
    Alcotest.test_case "malformed forms reject before execution" `Quick
      malformed;
    Alcotest.test_case "captured immediate instruction bytes" `Quick
      encoder_bytes;
  ]
