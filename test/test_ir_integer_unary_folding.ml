module Fold = Holyc_lib.Ir_integer_unary_folding
module X87 = Holyc_lib.Ir_x87_stack
module Graph = Holyc_lib.Ir_block_graph
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Span = Holyc_lib.Span
module Source_id = Holyc_lib.Source_id
module Int_map = Map.Make (Int)

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_graph_error (error : Graph.error) = error.code ^ ": " ^ error.message
let show_x87_error (error : X87.error) = error.code ^ ": " ^ error.message
let show_fold_error (error : Fold.error) = error.code ^ ": " ^ error.message

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let primitive_type primitive =
  Type.make_primitive ~form:Type.Internal_storage ~primitive ~pointer_depth:0
  |> require_ok Fun.id

let i64 = primitive_type Primitive.I64
let u64 = primitive_type Primitive.U64
let u8 = primitive_type Primitive.U8
let f64 = primitive_type Primitive.F64

let public_i64 =
  Type.make_primitive ~form:Type.Public_spelling ~primitive:Primitive.I64
    ~pointer_depth:0
  |> require_ok Fun.id

let source = Source_id.of_int 0 |> require_ok Fun.id
let span start stop = Span.unsafe_make ~source ~start ~stop
let result value : Sequence.value_definition = { value_id = value_id value }

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L)
    ?span id opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands;
    result;
    target_type;
    payload;
    flags;
    span;
  }

let imm_i64 ?(flags = 0L) ?span ~id ~value ~type_ bits =
  description ~result:(result value) ~target_type:type_
    ~payload:(Sequence.Integer bits) ~flags ?span id Opcode.Ic_imm_i64

let imm_f64 ~id ~value bits =
  description ~result:(result value) ~target_type:f64
    ~payload:(Sequence.Float_bits bits) id Opcode.Ic_imm_f64

let unary ?(flags = 0L) ?span ?payload ~id ~operand ~value ~type_ opcode =
  description
    ~operands:[ value_id operand ]
    ~result:(result value) ~target_type:type_ ~flags ?span ?payload id opcode

let terminal id = description id Opcode.Ic_ret

let return_value ~id ~operand ~type_ =
  description
    ~operands:[ value_id operand ]
    ~target_type:type_ id Opcode.Ic_return_val

let block id instructions : Graph.block_description =
  { block_id = block_id id; instructions }

let verified ~entry blocks =
  Graph.create ~entry:(block_id entry) blocks
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_graph_error errors))
  |> X87.verify
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_x87_error errors))

let fold checked =
  Fold.fold checked
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_fold_error errors))

let folded_graph folded = Fold.x87 folded |> X87.graph

let read_file path =
  let path =
    if Sys.file_exists path then path else Filename.concat "test" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let block_descriptions graph id =
  match Graph.find_block graph (block_id id) with
  | None -> Alcotest.failf "block ^b%d was not found" id
  | Some block ->
      Graph.instructions block |> Sequence.instructions
      |> List.map Sequence.description

let instruction_number description =
  Sequence.Instruction_id.to_int description.Sequence.instruction_id

let result_number description =
  match description.Sequence.result with
  | None -> Alcotest.fail "instruction has no result"
  | Some value -> Sequence.Value_id.to_int value.value_id

let integer_payload description =
  match description.Sequence.payload with
  | Some (Sequence.Integer bits) -> bits
  | _ -> Alcotest.fail "instruction has no integer payload"

type test_word_type = Test_i64 | Test_u64

(* This evaluator belongs only to the randomized regression below.  It gives
   the supported word operations an observable return value without claiming
   that the compiler has a production IR interpreter. *)
let evaluate_test_graph checked =
  let graph = X87.graph checked in
  let word_type = function
    | Some type_ when type_ = i64 -> Some Test_i64
    | Some type_ when type_ = u64 -> Some Test_u64
    | Some _ | None -> None
  in
  let is_unary opcode =
    Opcode.equal opcode Opcode.Ic_com
    || Opcode.equal opcode Opcode.Ic_not
    || Opcode.equal opcode Opcode.Ic_unary_minus
  in
  let unary_operation opcode operand_type result_type =
    if Opcode.equal opcode Opcode.Ic_com && result_type = Test_i64 then
      Some (fun bits -> Int64.logxor bits (-1L))
    else if Opcode.equal opcode Opcode.Ic_not && result_type = operand_type then
      Some (fun bits -> if Int64.equal bits 0L then 1L else 0L)
    else if Opcode.equal opcode Opcode.Ic_unary_minus && result_type = Test_i64
    then Some (fun bits -> Int64.sub 0L bits)
    else None
  in
  let malformed description message =
    Error
      (Printf.sprintf "!i%d %s: %s"
         (instruction_number description)
         (Opcode.to_source_name description.Sequence.opcode)
         message)
  in
  let rec execute values returned = function
    | [] -> Error "graph ended before IC_RET"
    | description :: rest ->
        let opcode = description.Sequence.opcode in
        if Opcode.equal opcode Opcode.Ic_imm_i64 then
          match
            ( description.operands,
              description.result,
              word_type description.target_type,
              description.payload,
              description.flags )
          with
          | [], Some result, Some word_type, Some (Sequence.Integer bits), 0L ->
              execute
                (Int_map.add
                   (Sequence.Value_id.to_int result.value_id)
                   (word_type, bits) values)
                returned rest
          | _ -> malformed description "malformed integer immediate"
        else if is_unary opcode then
          match
            ( description.operands,
              description.result,
              word_type description.target_type,
              description.payload,
              description.flags )
          with
          | [ operand ], Some result, Some result_type, None, 0L -> (
              match
                Int_map.find_opt (Sequence.Value_id.to_int operand) values
              with
              | Some (operand_type, bits) -> (
                  match unary_operation opcode operand_type result_type with
                  | Some operation ->
                      execute
                        (Int_map.add
                           (Sequence.Value_id.to_int result.value_id)
                           (result_type, operation bits)
                           values)
                        returned rest
                  | None ->
                      malformed description "invalid unary word type matrix")
              | None -> malformed description "operand has no value")
          | _ -> malformed description "malformed unary operation"
        else if Opcode.equal opcode Opcode.Ic_return_val then
          match returned with
          | Some _ -> malformed description "more than one IC_RETURN_VAL"
          | None -> (
              match
                ( description.operands,
                  description.result,
                  word_type description.target_type,
                  description.payload,
                  description.flags )
              with
              | [ operand ], None, Some return_type, None, 0L -> (
                  match
                    Int_map.find_opt (Sequence.Value_id.to_int operand) values
                  with
                  | Some ((operand_type, _) as value)
                    when operand_type = return_type ->
                      execute values (Some value) rest
                  | Some _ ->
                      malformed description "return word type does not match"
                  | None -> malformed description "return operand has no value")
              | _ -> malformed description "malformed return value")
        else if Opcode.equal opcode Opcode.Ic_ret then
          match
            ( description.operands,
              description.result,
              description.target_type,
              description.payload,
              description.flags,
              rest,
              returned )
          with
          | [], None, None, None, 0L, [], Some (_, bits) -> Ok bits
          | [], None, None, None, 0L, [], None ->
              malformed description "IC_RET has no observed return value"
          | _ -> malformed description "malformed or non-final IC_RET"
        else malformed description "unsupported test evaluator opcode"
  in
  match Graph.blocks graph with
  | [ block ]
    when Graph.Block_id.equal (Graph.block_id block)
           (Graph.entry graph |> Graph.block_id)
         && Graph.successors block = [] ->
      Graph.instructions block |> Sequence.instructions
      |> List.map Sequence.description
      |> execute Int_map.empty None
  | _ -> Error "test evaluator requires one entry block with no successors"

let only_folded_constant folded block_number =
  match block_descriptions (folded_graph folded) block_number with
  | [ constant; terminator ] ->
      Alcotest.(check bool)
        "folded opcode" true
        (Opcode.equal constant.opcode Opcode.Ic_imm_i64);
      Alcotest.(check bool)
        "terminator" true
        (Opcode.equal terminator.opcode Opcode.Ic_ret);
      constant
  | descriptions ->
      Alcotest.failf "expected one constant and one terminator, got %d items"
        (List.length descriptions)

let complement_rewrites_the_operator () =
  let producer_span = span 20 21 in
  let operator_span = span 12 13 in
  let checked =
    verified ~entry:4
      [
        block 4
          [
            imm_i64 ~span:producer_span ~id:10 ~value:100 ~type_:u64 1L;
            unary ~span:operator_span ~id:11 ~operand:100 ~value:101 ~type_:i64
              Opcode.Ic_com;
            terminal 12;
          ];
      ]
  in
  let folded = fold checked in
  let constant = only_folded_constant folded 4 in
  Alcotest.(check int)
    "retained instruction ID" 11
    (instruction_number constant);
  Alcotest.(check int) "retained result ID" 101 (result_number constant);
  Alcotest.(check int64) "complement bits" (-2L) (integer_payload constant);
  Alcotest.(check bool) "no operands" true (constant.operands = []);
  Alcotest.(check bool) "I64 result" true (constant.target_type = Some i64);
  Alcotest.(check bool) "operator span" true (constant.span = Some operator_span);
  Alcotest.(check int64) "zero flags" 0L constant.flags;
  match Fold.rewrites folded with
  | [ rewrite ] ->
      Alcotest.(check int)
        "trace block" 4
        (Sequence.Block_id.to_int rewrite.block_id);
      Alcotest.(check int)
        "removed instruction" 10
        (Sequence.Instruction_id.to_int rewrite.removed_instruction_id);
      Alcotest.(check int)
        "removed value" 100
        (Sequence.Value_id.to_int rewrite.removed_value_id);
      Alcotest.(check int)
        "retained instruction" 11
        (Sequence.Instruction_id.to_int rewrite.retained_instruction_id);
      Alcotest.(check int)
        "retained value" 101
        (Sequence.Value_id.to_int rewrite.retained_value_id);
      Alcotest.(check bool)
        "trace opcode" true
        (Opcode.equal rewrite.opcode Opcode.Ic_com);
      Alcotest.(check int64) "trace input" 1L rewrite.input_bits;
      Alcotest.(check int64) "trace output" (-2L) rewrite.folded_bits
  | rewrites ->
      Alcotest.failf "expected one rewrite, got %d" (List.length rewrites)

let logical_not_uses_zero_truth_semantics () =
  List.iteri
    (fun index (input, expected) ->
      let checked =
        verified ~entry:index
          [
            block index
              [
                imm_i64 ~id:0 ~value:0 ~type_:i64 input;
                unary ~id:1 ~operand:0 ~value:1 ~type_:i64 Opcode.Ic_not;
                terminal 2;
              ];
          ]
      in
      let constant = only_folded_constant (fold checked) index in
      Alcotest.(check int64)
        (Printf.sprintf "logical-not 0x%Lx" input)
        expected (integer_payload constant))
    [ (0L, 1L); (1L, 0L); (-1L, 0L); (Int64.min_int, 0L) ]

let negation_wraps_at_i64_width () =
  List.iteri
    (fun index (input, expected) ->
      let checked =
        verified ~entry:index
          [
            block index
              [
                imm_i64 ~id:0 ~value:0 ~type_:i64 input;
                unary ~id:1 ~operand:0 ~value:1 ~type_:i64 Opcode.Ic_unary_minus;
                terminal 2;
              ];
          ]
      in
      let constant = only_folded_constant (fold checked) index in
      Alcotest.(check int64)
        (Printf.sprintf "negate 0x%Lx" input)
        expected (integer_payload constant))
    [ (0L, 0L); (1L, -1L); (-1L, 1L); (Int64.min_int, Int64.min_int) ]

let unsigned_type_rules_are_exact () =
  let check opcode expected_type =
    let checked =
      verified ~entry:0
        [
          block 0
            [
              imm_i64 ~id:0 ~value:0 ~type_:u64 1L;
              unary ~id:1 ~operand:0 ~value:1 ~type_:expected_type opcode;
              terminal 2;
            ];
        ]
    in
    let constant = only_folded_constant (fold checked) 0 in
    Alcotest.(check bool)
      (Opcode.to_source_name opcode ^ " result type")
      true
      (constant.target_type = Some expected_type)
  in
  check Opcode.Ic_com i64;
  check Opcode.Ic_not u64;
  check Opcode.Ic_unary_minus i64

let nested_chains_reach_a_fixed_point () =
  let checked =
    verified ~entry:7
      [
        block 7
          [
            imm_i64 ~id:0 ~value:10 ~type_:u64 1L;
            unary ~id:1 ~operand:10 ~value:11 ~type_:i64 Opcode.Ic_com;
            unary ~id:2 ~operand:11 ~value:12 ~type_:i64 Opcode.Ic_unary_minus;
            unary ~id:3 ~operand:12 ~value:13 ~type_:i64 Opcode.Ic_not;
            terminal 4;
          ];
      ]
  in
  let first = fold checked in
  let golden_actual =
    "--- before ---\n"
    ^ Graph.human (X87.graph checked)
    ^ "--- after ---\n"
    ^ Graph.human (folded_graph first)
    ^ "--- rewrite trace ---\n" ^ Fold.human first
  in
  Alcotest.(check string)
    "reviewed before/after IR"
    (read_file "golden/integer-unary-folding.ir")
    golden_actual;
  let constant = only_folded_constant first 7 in
  Alcotest.(check int) "final instruction" 3 (instruction_number constant);
  Alcotest.(check int) "final result" 13 (result_number constant);
  Alcotest.(check int64) "nested bits" 0L (integer_payload constant);
  Alcotest.(check (list string))
    "source-ordered rewrites"
    [ "IC_COM"; "IC_UNARY_MINUS"; "IC_NOT" ]
    (List.map
       (fun (rewrite : Fold.rewrite) -> Opcode.to_source_name rewrite.opcode)
       (Fold.rewrites first));
  let second = fold (Fold.x87 first) in
  Alcotest.(check int)
    "second pass rewrites" 0
    (List.length (Fold.rewrites second));
  Alcotest.(check string)
    "idempotent graph"
    (Graph.human (folded_graph first))
    (Graph.human (folded_graph second));
  (match X87.verify (folded_graph first) with
  | Ok _ -> ()
  | Error errors ->
      Alcotest.fail (String.concat "; " (List.map show_x87_error errors)));
  Alcotest.(check string)
    "empty trace"
    (Printf.sprintf
       "holyc-ir-integer-unary-folding-v1 reference=%s\nrewrites=0\n"
       Sequence.reference_commit)
    (Fold.human second)

let shared_immediates_are_not_removed () =
  let checked =
    verified ~entry:0
      [
        block 0
          [
            imm_i64 ~id:0 ~value:0 ~type_:i64 1L;
            unary ~id:1 ~operand:0 ~value:1 ~type_:i64 Opcode.Ic_com;
            unary ~id:2 ~operand:0 ~value:2 ~type_:i64 Opcode.Ic_not;
            terminal 3;
          ];
      ]
  in
  let folded = fold checked in
  Alcotest.(check int) "no rewrites" 0 (List.length (Fold.rewrites folded));
  Alcotest.(check string)
    "graph unchanged"
    (Graph.human (X87.graph checked))
    (Graph.human (folded_graph folded))

let flag_bearing_candidates_are_unchanged () =
  let result_not_used = 0x000000200L in
  let checked =
    verified ~entry:0
      [
        block 0
          [
            imm_i64 ~flags:result_not_used ~id:0 ~value:0 ~type_:i64 1L;
            unary ~id:1 ~operand:0 ~value:1 ~type_:i64 Opcode.Ic_com;
            imm_i64 ~id:2 ~value:2 ~type_:i64 0L;
            unary ~flags:result_not_used ~id:3 ~operand:2 ~value:3 ~type_:i64
              Opcode.Ic_not;
            terminal 4;
          ];
      ]
  in
  let folded = fold checked in
  Alcotest.(check int) "no rewrites" 0 (List.length (Fold.rewrites folded));
  Alcotest.(check string)
    "flagged graph unchanged"
    (Graph.human (X87.graph checked))
    (Graph.human (folded_graph folded))

let excluded_numeric_domains_are_unchanged () =
  let checked =
    verified ~entry:0
      [
        block 0
          [
            imm_f64 ~id:0 ~value:0 0x3ff0000000000000L;
            unary ~id:1 ~operand:0 ~value:1 ~type_:f64 Opcode.Ic_unary_minus;
            imm_i64 ~id:2 ~value:2 ~type_:u8 1L;
            unary ~id:3 ~operand:2 ~value:3 ~type_:u8 Opcode.Ic_not;
            terminal 4;
          ];
      ]
  in
  let folded = fold checked in
  Alcotest.(check int) "no rewrites" 0 (List.length (Fold.rewrites folded));
  Alcotest.(check string)
    "excluded graph unchanged"
    (Graph.human (X87.graph checked))
    (Graph.human (folded_graph folded))

let malformed_and_public_candidates_are_unchanged () =
  let checked =
    verified ~entry:0
      [
        block 0
          [
            imm_i64 ~id:0 ~value:0 ~type_:public_i64 1L;
            unary ~id:1 ~operand:0 ~value:1 ~type_:public_i64 Opcode.Ic_not;
            description ~result:(result 2) ~target_type:i64 2 Opcode.Ic_imm_i64;
            unary ~id:3 ~operand:2 ~value:3 ~type_:i64 Opcode.Ic_com;
            description ~result:(result 4) ~target_type:i64
              ~payload:(Sequence.Bytes "not-an-integer") 4 Opcode.Ic_imm_i64;
            unary ~id:5 ~operand:4 ~value:5 ~type_:i64
              ~payload:(Sequence.Integer 0L) Opcode.Ic_not;
            imm_i64 ~id:6 ~value:6 ~type_:i64 42L;
            unary ~id:7 ~operand:6 ~value:7 ~type_:i64
              ~payload:(Sequence.Integer 99L) Opcode.Ic_not;
            terminal 8;
          ];
      ]
  in
  let folded = fold checked in
  Alcotest.(check int) "no rewrites" 0 (List.length (Fold.rewrites folded));
  Alcotest.(check string)
    "malformed graph unchanged"
    (Graph.human (X87.graph checked))
    (Graph.human (folded_graph folded))

let cross_block_values_are_rejected_before_folding () =
  match
    Graph.create ~entry:(block_id 0)
      [
        block 0
          [
            imm_i64 ~id:0 ~value:0 ~type_:i64 1L;
            description ~payload:(Sequence.Block (block_id 1)) 1 Opcode.Ic_jmp;
          ];
        block 1
          [
            unary ~id:2 ~operand:0 ~value:1 ~type_:i64 Opcode.Ic_not; terminal 3;
          ];
      ]
  with
  | Ok _ -> Alcotest.fail "cross-block value unexpectedly reached the pass seam"
  | Error errors ->
      Alcotest.(check bool)
        "HCIR0009" true
        (List.exists
           (fun (error : Graph.error) -> error.code = "HCIR0009")
           errors)

let block_order_edges_and_terminators_are_preserved () =
  let checked =
    verified ~entry:10
      [
        block 10
          [
            imm_i64 ~id:0 ~value:0 ~type_:i64 0L;
            unary ~id:1 ~operand:0 ~value:1 ~type_:i64 Opcode.Ic_not;
            description ~payload:(Sequence.Block (block_id 20)) 2 Opcode.Ic_jmp;
          ];
        block 20 [ description 3 Opcode.Ic_end ];
      ]
  in
  let before = X87.graph checked in
  let after = fold checked |> folded_graph in
  let block_numbers graph =
    Graph.blocks graph
    |> List.map (fun block -> Graph.block_id block |> Graph.Block_id.to_int)
  in
  let successors graph id =
    match Graph.find_block graph (block_id id) with
    | None -> []
    | Some block -> List.map Graph.Block_id.to_int (Graph.successors block)
  in
  Alcotest.(check (list int))
    "block order" (block_numbers before) (block_numbers after);
  Alcotest.(check (list int))
    "entry successors" (successors before 10) (successors after 10);
  Alcotest.(check (list int))
    "exit successors" (successors before 20) (successors after 20);
  match List.rev (block_descriptions after 10) with
  | terminator :: _ ->
      Alcotest.(check bool)
        "jump retained" true
        (Opcode.equal terminator.opcode Opcode.Ic_jmp)
  | [] -> Alcotest.fail "entry block became empty"

let rewrite_dump_is_deterministic () =
  let checked =
    verified ~entry:4
      [
        block 4
          [
            imm_i64 ~id:10 ~value:100 ~type_:u64 1L;
            unary ~id:11 ~operand:100 ~value:101 ~type_:i64 Opcode.Ic_com;
            terminal 12;
          ];
      ]
  in
  let first = fold checked in
  let repeated = fold checked in
  let expected =
    Printf.sprintf
      "holyc-ir-integer-unary-folding-v1 reference=%s\n\
       rewrites=1\n\
       ^b4 remove=!i10:%%v100 retain=!i11:%%v101 opcode=IC_COM \
       input=0x0000000000000001 output=0xfffffffffffffffe\n"
      Sequence.reference_commit
  in
  Alcotest.(check string) "versioned trace" expected (Fold.human first);
  Alcotest.(check string)
    "repeat trace" (Fold.human first) (Fold.human repeated);
  Alcotest.(check string)
    "repeat graph"
    (Graph.human (folded_graph first))
    (Graph.human (folded_graph repeated))

let word_semantics_property =
  QCheck.Test.make ~count:500
    ~name:"original and folded unary graphs return the same direct 64-bit word"
    QCheck.(pair int64 (int_bound 2))
    (fun (bits, selector) ->
      let opcode, expected =
        match selector with
        | 0 -> (Opcode.Ic_com, Int64.lognot bits)
        | 1 -> (Opcode.Ic_not, if bits = 0L then 1L else 0L)
        | _ -> (Opcode.Ic_unary_minus, Int64.neg bits)
      in
      let checked =
        verified ~entry:0
          [
            block 0
              [
                imm_i64 ~id:0 ~value:0 ~type_:i64 bits;
                unary ~id:1 ~operand:0 ~value:1 ~type_:i64 opcode;
                return_value ~id:2 ~operand:1 ~type_:i64;
                terminal 3;
              ];
          ]
      in
      let folded = fold checked in
      let optimized_x87 = Fold.x87 folded in
      let optimized = X87.graph optimized_x87 in
      match
        ( evaluate_test_graph checked,
          evaluate_test_graph optimized_x87,
          block_descriptions optimized 0 )
      with
      | Ok original_return, Ok folded_return, constant :: _ ->
          Int64.equal original_return expected
          && Int64.equal folded_return expected
          && Int64.equal original_return folded_return
          && Opcode.equal constant.opcode Opcode.Ic_imm_i64
          && Int64.equal (integer_payload constant) expected
      | Error _, _, _ | _, Error _, _ | _, _, [] -> false)

let tests =
  [
    Alcotest.test_case "complement rewrite" `Quick
      complement_rewrites_the_operator;
    Alcotest.test_case "logical-not semantics" `Quick
      logical_not_uses_zero_truth_semantics;
    Alcotest.test_case "negation wrapping" `Quick negation_wraps_at_i64_width;
    Alcotest.test_case "unsigned type rules" `Quick
      unsigned_type_rules_are_exact;
    Alcotest.test_case "nested fixed point and idempotence" `Quick
      nested_chains_reach_a_fixed_point;
    Alcotest.test_case "shared immediate" `Quick
      shared_immediates_are_not_removed;
    Alcotest.test_case "flagged candidates" `Quick
      flag_bearing_candidates_are_unchanged;
    Alcotest.test_case "excluded numeric domains" `Quick
      excluded_numeric_domains_are_unchanged;
    Alcotest.test_case "malformed and public candidates" `Quick
      malformed_and_public_candidates_are_unchanged;
    Alcotest.test_case "cross-block constructor boundary" `Quick
      cross_block_values_are_rejected_before_folding;
    Alcotest.test_case "control-flow preservation" `Quick
      block_order_edges_and_terminators_are_preserved;
    Alcotest.test_case "deterministic rewrite dump" `Quick
      rewrite_dump_is_deterministic;
    QCheck_alcotest.to_alcotest word_semantics_property;
  ]
