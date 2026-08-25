module Function = Holyc_lib.Ir_function_body
module Graph = Holyc_lib.Ir_block_graph
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Symbol = Holyc_lib.Semantic_symbol
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Flag = Holyc_lib.Function_flag
module Option_ = Holyc_lib.Compiler_option
module Source_id = Holyc_lib.Source_id
module Span = Holyc_lib.Span

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_graph_error (error : Graph.error) = error.code ^ ": " ^ error.message

let show_function_error (error : Function.error) =
  error.code ^ ": " ^ error.message

let function_id value =
  Function.Function_id.of_int value |> require_ok show_function_error

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id_of_int value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let source_id value = Source_id.of_int value |> require_ok Fun.id

let type_ primitive =
  Type.make_primitive ~form:Type.Internal_storage ~primitive ~pointer_depth:0
  |> require_ok Fun.id

let i64 = type_ Primitive.I64
let f64 = type_ Primitive.F64

let symbol ?(scope = 2) kind id name =
  Symbol.create ~id:(Symbol.Id.of_int id)
    ~scope_id:(Symbol.Scope_id.of_int scope)
    ~name ~kind ~origin:(Symbol.Synthesized "IR function-body test")

let instruction ?(operands = []) ?result ?target_type ?payload ?(flags = 0L) id
    opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands;
    result =
      Option.map (fun value -> Sequence.{ value_id = value_id value }) result;
    target_type;
    payload;
    flags;
    span = None;
  }

let graph ~entry blocks =
  Graph.create ~entry:(block_id_of_int entry) blocks
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_graph_error errors))

let valid_graph () =
  graph ~entry:4
    [
      Graph.
        {
          block_id = block_id_of_int 4;
          instructions =
            [ instruction 0 Opcode.Ic_enter; instruction 1 Opcode.Ic_end ];
        };
    ]

let member ?span position symbol type_ : Function.member_description =
  { position; symbol; type_; span }

let description ?(function_symbol = symbol ~scope:1 Symbol.Function 100 "Mix")
    ?(scope = 2) ?(parameters = []) ?(locals = []) ?(stored_flags = 0L)
    ?(compiler_options = 0L) ?span ?body () : Function.description =
  {
    function_id = function_id 7;
    symbol = function_symbol;
    function_scope = Symbol.Scope_id.of_int scope;
    return_type = i64;
    parameters;
    locals;
    stored_flags;
    compiler_options;
    span;
    body = Option.value body ~default:(valid_graph ());
  }

let create description =
  Function.create description
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_function_error errors))

let errors description =
  match Function.create description with
  | Ok _ -> Alcotest.fail "expected function-body validation to fail"
  | Error errors -> errors

let has_code code errors =
  Alcotest.(check bool)
    ("contains " ^ code) true
    (List.exists (fun (error : Function.error) -> error.code = code) errors)

let function_ids_are_checked () =
  match Function.Function_id.of_int (-1) with
  | Ok _ -> Alcotest.fail "negative function ID was accepted"
  | Error error -> Alcotest.(check string) "stable code" "HCIR0026" error.code

let named_function_retains_ordered_metadata () =
  let function_span =
    Some Span.{ source = source_id 0; start = 0; stop = 20 }
  in
  let right = symbol Symbol.Parameter 30 "right" in
  let left = symbol Symbol.Parameter 10 "left" in
  let temporary = symbol Symbol.Local_variable 80 "temporary" in
  let stored_flags = Flag.Stored.to_mask Flag.Stored.Argument_pop in
  let compiler_options = Option_.mask Option_.Trace in
  let checked =
    description
      ~parameters:[ member 2 right i64; member 0 left f64 ]
      ~locals:[ member 0 temporary i64 ]
      ~stored_flags ~compiler_options ?span:function_span ()
    |> create
  in
  Alcotest.(check int)
    "function ID" 7
    (Function.function_id checked |> Function.Function_id.to_int);
  Alcotest.(check int)
    "body scope" 2
    (Function.function_scope checked |> Symbol.Scope_id.to_int);
  Alcotest.(check (list int))
    "parameter source order" [ 30; 10 ]
    (Function.parameters checked
    |> List.map (fun item ->
        Function.member_symbol item |> Symbol.id |> Symbol.Id.to_int));
  Alcotest.(check (list int))
    "parameter positions remain independent" [ 2; 0 ]
    (Function.parameters checked |> List.map Function.member_position);
  Alcotest.(check int) "one local" 1 (List.length (Function.locals checked));
  Alcotest.(check bool)
    "checked x87 graph retained" true
    (Function.body checked
    == Holyc_lib.Ir_x87_stack.graph (Function.x87 checked));
  let expected =
    "holyc-ir-function-v1 reference=c26482bb6ad3f80106d28504ec5db3c6a360732c\n\
     function ^f7 symbol=@s100 name=\"Mix\" declaration-scope=^s1 body-scope=^s2\n\
     return=internal:I64 flags=0x400 [Ff_ARGPOP] options=0x2 [OPTf_TRACE] \
     @source=0:0..20\n\
     parameters=2\n\
     parameter position=2 symbol=@s30 name=\"right\" type=internal:I64 scope=^s2\n\
     parameter position=0 symbol=@s10 name=\"left\" type=internal:F64 scope=^s2\n\
     locals=1\n\
     local position=0 symbol=@s80 name=\"temporary\" type=internal:I64 scope=^s2\n\
     body\n\
     entry=^b4\n\
     block ^b4\n\
     !i0 IC_ENTER flags=0x000000000\n\
     !i1 IC_END flags=0x000000000\n\
     successors=[]\n\
     x87=verified\n"
  in
  Alcotest.(check string) "versioned dump" expected (Function.human checked);
  let repeated =
    description
      ~parameters:[ member 2 right i64; member 0 left f64 ]
      ~locals:[ member 0 temporary i64 ]
      ~stored_flags ~compiler_options ?span:function_span ()
    |> create
  in
  Alcotest.(check string)
    "repeat construction" (Function.human checked) (Function.human repeated)

let symbol_kinds_and_scopes_are_checked () =
  let function_symbol = symbol ~scope:1 Symbol.Global_variable 100 "not_fun" in
  let parameter = symbol ~scope:9 Symbol.Local_variable 20 "wrong" in
  let local = symbol Symbol.Parameter 21 "also_wrong" in
  let result =
    description ~function_symbol
      ~parameters:[ member 0 parameter i64 ]
      ~locals:[ member 0 local i64 ]
      ()
    |> errors
  in
  result |> has_code "HCIR0027";
  result |> has_code "HCIR0032";
  let same_scope =
    description ~function_symbol:(symbol Symbol.Function 101 "nested") ()
    |> errors
  in
  same_scope |> has_code "HCIR0027"

let repeated_members_are_rejected () =
  let first = symbol Symbol.Parameter 20 "first" in
  let second = symbol Symbol.Parameter 21 "second" in
  let duplicate =
    description
      ~parameters:[ member 0 first i64; member 0 second i64 ]
      ~locals:[ member 4 first i64 ]
      ()
    |> errors
  in
  let duplicate_errors =
    List.filter
      (fun (error : Function.error) -> error.code = "HCIR0033")
      duplicate
  in
  Alcotest.(check int)
    "position and symbol duplicates" 2
    (List.length duplicate_errors)

let flag_and_option_masks_are_checked () =
  let combine flags = List.fold_left Int64.logor 0L flags in
  let stored_flags =
    combine
      [
        Flag.Stored.to_mask Flag.Stored.Argument_pop;
        Flag.Stored.to_mask Flag.Stored.No_argument_pop;
        Flag.Stored.to_mask Flag.Stored.Has_error_code;
        Int64.shift_left 1L 63;
      ]
  in
  let invalid =
    description ~stored_flags ~compiler_options:(Int64.shift_left 1L 62) ()
    |> errors
  in
  invalid |> has_code "HCIR0029";
  Alcotest.(check int)
    "both incompatible flag rules" 2
    (List.length
       (List.filter
          (fun (error : Function.error) -> error.code = "HCIR0030")
          invalid));
  let interrupt_flags =
    combine
      [
        Flag.Stored.to_mask Flag.Stored.Interrupt;
        Flag.Stored.to_mask Flag.Stored.Has_error_code;
        Flag.Stored.to_mask Flag.Stored.No_argument_pop;
      ]
  in
  ignore (description ~stored_flags:interrupt_flags () |> create)

let invalid_spans_are_rejected () =
  let invalid_span = Span.{ source = source_id 0; start = 9; stop = 3 } in
  let parameter = symbol Symbol.Parameter 20 "value" in
  let invalid =
    description ~span:invalid_span
      ~parameters:[ member ~span:invalid_span 0 parameter i64 ]
      ()
    |> errors
  in
  Alcotest.(check int)
    "function and parameter spans" 2
    (List.length
       (List.filter
          (fun (error : Function.error) -> error.code = "HCIR0028")
          invalid))

let x87_failures_are_function_failures () =
  let dont_push = Int64.shift_left 1L 21 in
  let body =
    graph ~entry:0
      [
        Graph.
          {
            block_id = block_id_of_int 0;
            instructions =
              [
                instruction ~result:0 ~target_type:f64 0 Opcode.Ic_imm_f64;
                instruction
                  ~operands:[ value_id 0 ]
                  ~result:1 ~target_type:f64 ~flags:dont_push 1 Opcode.Ic_sqr;
                instruction 2 Opcode.Ic_ret;
              ];
          };
      ]
  in
  let failure = description ~body () |> errors in
  failure |> has_code "HCIR0022";
  let error =
    List.find (fun (error : Function.error) -> error.code = "HCIR0022") failure
  in
  Alcotest.(check (option int)) "function context" (Some 7) error.function_id;
  Alcotest.(check (option int)) "block context" (Some 0) error.block_id;
  Alcotest.(check (option int))
    "instruction context" (Some 1) error.instruction_id

let failures_are_deterministic () =
  let parameter = symbol ~scope:9 Symbol.Local_variable 20 "wrong" in
  let invalid () =
    description ~parameters:[ member (-1) parameter i64 ] ()
    |> errors
    |> List.map (fun (error : Function.error) ->
        ( error.code,
          error.message,
          error.function_id,
          error.symbol_id,
          error.position ))
  in
  Alcotest.(check bool) "same ordered failures" true (invalid () = invalid ())

let tests =
  [
    Alcotest.test_case "function IDs" `Quick function_ids_are_checked;
    Alcotest.test_case "ordered named metadata" `Quick
      named_function_retains_ordered_metadata;
    Alcotest.test_case "symbol kinds and scopes" `Quick
      symbol_kinds_and_scopes_are_checked;
    Alcotest.test_case "repeated members" `Quick repeated_members_are_rejected;
    Alcotest.test_case "flag and option masks" `Quick
      flag_and_option_masks_are_checked;
    Alcotest.test_case "invalid spans" `Quick invalid_spans_are_rejected;
    Alcotest.test_case "x87 failure propagation" `Quick
      x87_failures_are_function_failures;
    Alcotest.test_case "deterministic failures" `Quick
      failures_are_deterministic;
  ]
