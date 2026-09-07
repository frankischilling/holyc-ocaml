module Sequence = Instruction_sequence
module Typed = Sema.Function_call_expression_result
module Source = Sema.Function_call_resolution

type statement =
  | Empty of Common.Span.t
  | Expression of Typed.expression_result
  | Block of statement list
  | If of Typed.expression_result * statement * statement option
  | While of Typed.expression_result * statement
  | Do_while of statement * Typed.expression_result
  | For of statement * Typed.expression_result * statement option * statement
  | Break of Common.Span.t

exception Invalid of Common.Diagnostic.t list

let fail span code message =
  raise
    (Invalid
       [
         Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
           ~primary:span ();
       ])

let span_of_result fallback result =
  match Typed.result_origin result with
  | Sema.Symbol.Source_location location -> location.span
  | _ -> fallback

let lower ~span statements =
  try
    let instruction_count = ref 0
    and value_count = ref 0
    and block_count = ref 0 in
    let checked_id = function
      | Ok value -> value
      | Error (e : Sequence.error) -> fail span e.code e.message
    in
    let allocate make count =
      if !count = Int.max_int then
        fail span "HCIRL0005" "integer program identity space is exhausted";
      let result = make !count |> checked_id in
      incr count;
      result
    in
    let block () = allocate Sequence.Block_id.of_int block_count in
    let entry = block () in
    let current = ref (Some (entry, [])) and blocks = ref [] in
    let start block_id =
      match !current with
      | None -> current := Some (block_id, [])
      | Some _ ->
          fail span "HCRUN0004"
            "cannot start a block before terminating its predecessor"
    in
    let ensure_open () = if !current = None then start (block ()) in
    let append description =
      ensure_open ();
      match !current with
      | Some (id, rev) -> current := Some (id, description :: rev)
      | None -> assert false
    in
    let finish () =
      match !current with
      | Some (block_id, rev) ->
          blocks :=
            { Block_graph.block_id; instructions = List.rev rev } :: !blocks;
          current := None
      | None -> fail span "HCRUN0004" "cannot terminate an absent block"
    in
    let instruction ?(operands = []) ?payload ?(flags = 0L) ~at opcode =
      let instruction_id =
        allocate Sequence.Instruction_id.of_int instruction_count
      in
      append
        {
          Sequence.instruction_id;
          opcode;
          operands;
          result = None;
          target_type = None;
          payload;
          flags;
          span = Some at;
        }
    in
    let jump ~at target =
      instruction ~at ~payload:(Sequence.Block target) Opcode.Ic_jmp;
      finish ()
    in
    let rec validate_expression expression =
      let source = Typed.result_source expression in
      (match Source.argument_expression_kind source with
      | Source.Binary_expression binary ->
          let comparison = function
            | Opcode.Ic_less
            | Opcode.Ic_greater
            | Opcode.Ic_less_equ
            | Opcode.Ic_greater_equ
            | Opcode.Ic_equ_equ
            | Opcode.Ic_not_equ -> true
            | _ -> false
          in
          let adjacent source =
            match Source.argument_expression_kind source with
            | Source.Binary_expression child ->
                comparison (Source.binary_operator child)
            | _ -> false
          in
          if
            comparison (Source.binary_operator binary)
            && adjacent (Source.binary_left binary)
          then
            fail
              (span_of_result span expression)
              "HCRUN0003"
              "chained comparisons require shared-operand lowering, which is \
               not implemented"
      | _ -> ());
      Option.iter validate_expression (Typed.result_operand expression);
      Option.iter
        (fun (left, right) ->
          validate_expression left;
          validate_expression right)
        (Typed.result_binary_operands expression)
    in
    let expression value =
      validate_expression value;
      let instruction_id =
        Sequence.Instruction_id.of_int !instruction_count |> checked_id
      in
      let value_id = Sequence.Value_id.of_int !value_count |> checked_id in
      match
        Expression_lowering.lower_typed_result ~instruction_id ~value_id value
      with
      | Error errors ->
          raise
            (Invalid
               (List.map
                  (fun (e : Sequence.error) ->
                    Common.Diagnostic.make ~code:e.code
                      ~severity:Common.Diagnostic.Error ~message:e.message
                      ~primary:(Option.value e.span ~default:span)
                      ())
                  errors))
      | Ok Expression_lowering.Unsupported_expression ->
          fail
            (span_of_result span value)
            "HCRUN0003" "expression is outside integer program lowering"
      | Ok (Expression_lowering.Lowered result) ->
          result |> Expression_lowering.sequence |> Sequence.instructions
          |> List.iter (fun instruction ->
              append (Sequence.description instruction));
          instruction_count :=
            Sequence.Instruction_id.to_int
              (Expression_lowering.next_instruction_id result);
          value_count :=
            Sequence.Value_id.to_int (Expression_lowering.next_value_id result);
          Expression_lowering.result_value result
    in
    let rec condition value ~yes ~no =
      let at = span_of_result span value in
      let ordinary () =
        let operand = expression value in
        instruction ~at ~operands:[ operand ] ~payload:(Sequence.Block no)
          Opcode.Ic_br_zero;
        finish ();
        start (block ());
        jump ~at yes
      in
      if
        Typed.result_intrinsic_conversion value <> Typed.No_intrinsic_conversion
      then ordinary ()
      else
        match Source.argument_expression_kind (Typed.result_source value) with
        | Source.Parenthesized_expression _ -> (
            match Typed.result_operand value with
            | Some operand -> condition operand ~yes ~no
            | None -> ordinary ())
        | Source.Prefix_expression prefix -> (
            match
              (Source.prefix_operator prefix, Typed.result_operand value)
            with
            | Source.Logical_not, Some operand ->
                condition operand ~yes:no ~no:yes
            | Source.Unary_plus, Some operand -> condition operand ~yes ~no
            | _ -> ordinary ())
        | Source.Binary_expression binary -> (
            match
              (Source.binary_operator binary, Typed.result_binary_operands value)
            with
            | Opcode.Ic_and_and, Some (left, right) ->
                let rhs = block () in
                condition left ~yes:rhs ~no;
                start rhs;
                condition right ~yes ~no
            | Opcode.Ic_or_or, Some (left, right) ->
                let rhs = block () in
                condition left ~yes ~no:rhs;
                start rhs;
                condition right ~yes ~no
            | _ -> ordinary ())
        | _ -> ordinary ()
    in
    let rec statement break_target = function
      | Empty _ -> ()
      | Expression value ->
          let operand = expression value in
          instruction
            ~at:(span_of_result span value)
            ~operands:[ operand ] ~flags:0x200L Opcode.Ic_end_exp
      | Block body -> List.iter (statement break_target) body
      | Break at -> (
          match break_target with
          | Some target -> jump ~at target
          | None -> fail at "HCRUN0002" "break has no enclosing loop target")
      | If (value, then_branch, else_branch) ->
          let at = span_of_result span value in
          let yes = block () in
          let no = block () in
          let done_ = block () in
          condition value ~yes ~no;
          start yes;
          statement break_target then_branch;
          jump ~at done_;
          start no;
          Option.iter (statement break_target) else_branch;
          jump ~at done_;
          start done_
      | While (value, body) ->
          let at = span_of_result span value in
          let test = block () in
          let yes = block () in
          let done_ = block () in
          jump ~at test;
          start test;
          condition value ~yes ~no:done_;
          start yes;
          statement (Some done_) body;
          jump ~at test;
          start done_
      | Do_while (body, value) ->
          let at = span_of_result span value in
          let body_id = block () in
          let done_ = block () in
          jump ~at body_id;
          start body_id;
          statement (Some done_) body;
          condition value ~yes:body_id ~no:done_;
          start done_
      | For (initial, value, update, body) ->
          statement None initial;
          let at = span_of_result span value in
          let test = block () in
          let yes = block () in
          let done_ = block () in
          jump ~at test;
          start test;
          condition value ~yes ~no:done_;
          start yes;
          statement (Some done_) body;
          Option.iter (statement None) update;
          jump ~at test;
          start done_
    in
    List.iter (statement None) statements;
    instruction ~at:span Opcode.Ic_end;
    finish ();
    let graph =
      match Block_graph.create ~entry (List.rev !blocks) with
      | Ok graph -> graph
      | Error errors ->
          raise
            (Invalid
               (List.map
                  (fun (e : Block_graph.error) ->
                    Common.Diagnostic.make ~code:e.code
                      ~severity:Common.Diagnostic.Error ~message:e.message
                      ~primary:(Option.value e.span ~default:span)
                      ())
                  errors))
    in
    X87_stack.verify graph
    |> Result.map_error
         (List.map (fun (e : X87_stack.error) ->
              Common.Diagnostic.make ~code:e.code
                ~severity:Common.Diagnostic.Error ~message:e.message
                ~primary:(Option.value e.span ~default:span)
                ()))
  with Invalid diagnostics -> Error diagnostics
