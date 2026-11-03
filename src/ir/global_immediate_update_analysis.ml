module Frame = Sema.Function_frame_layout
module Seq = Instruction_sequence
module O = Opcode
module Typed = Sema.Function_call_expression_result
module Calls = Sema.Function_call_resolution
module Bind = Sema.Module_expression_binding

type stamp = Seq.description * Seq.description
type block_stamp = Seq.Block_id.t * stamp list * Seq.Block_id.t list

type outer =
  | Literal of int64 * Sema.Type.t
  | Global of Sema.Symbol.t * bool * Sema.Type.t * Sema.Type.t
  | Call of Sema.Symbol.t * Sema.Type.t

type t = {
  frame : Frame.function_layout;
  body : Function_body.t;
  definition : Sema.Function_resolution.resolved_declaration option;
  source : Typed.resolved_function option;
  blocks : block_stamp list;
  instruction : Seq.description;
  symbol : Sema.Symbol.t;
}

let snapshot body =
  Block_graph.blocks (Function_body.body body)
  |> List.map (fun block ->
      ( Block_graph.block_id block,
        Block_graph.instructions block
        |> Seq.instructions
        |> List.map (fun instruction ->
            let item = Seq.description instruction in
            ( item,
              {
                item with
                operands = List.map Fun.id item.operands;
                result =
                  Option.map
                    (fun (result : Seq.value_definition) ->
                      { Seq.value_id = result.value_id })
                    item.result;
              } )),
        List.map Fun.id (Block_graph.successors block) ))

let same_payload a b =
  match (a, b) with
  | None, None -> true
  | Some (Seq.Integer x), Some (Seq.Integer y) -> x = y
  | Some (Seq.Symbol x), Some (Seq.Symbol y) -> x == y
  | Some (Seq.Retained_global x), Some (Seq.Retained_global y) ->
      Retained_global.same x y
  | Some (Seq.Block x), Some (Seq.Block y) -> Seq.Block_id.equal x y
  | _ -> false

let same_description a b =
  Seq.Instruction_id.equal a.Seq.instruction_id b.Seq.instruction_id
  && a.opcode = b.opcode && a.operands = b.operands && a.result = b.result
  && Option.equal Sema.Type.equal a.target_type b.target_type
  && same_payload a.payload b.payload
  && a.flags = b.flags && a.span = b.span

let same_list eq a b = List.length a = List.length b && List.for_all2 eq a b

let same_blocks a b =
  same_list
    (fun (id, instructions, successors)
         (other_id, other_instructions, other_successors) ->
      Seq.Block_id.equal id other_id
      && same_list
           (fun (original, stamp) (other, other_stamp) ->
             original == other && same_description stamp other_stamp)
           instructions other_instructions
      && same_list Seq.Block_id.equal successors other_successors)
    a b

let analyze ~frame ~body =
  let id (item : Seq.description) =
    Option.map
      (fun (result : Seq.value_definition) -> result.value_id)
      item.result
  in
  let instructions block =
    Block_graph.instructions block
    |> Seq.instructions |> List.map Seq.description
  in
  let blocks = Block_graph.blocks (Function_body.body body) in
  let all_items = List.concat_map instructions blocks in
  let entry =
    Block_graph.block_id (Block_graph.entry (Function_body.body body))
  in
  let is_entry block = Seq.Block_id.equal (Block_graph.block_id block) entry in
  let valid_other block =
    match (instructions block, Block_graph.successors block) with
    | [ item ], successors -> (
        item.Seq.operands = [] && item.result = None && item.flags = 0L
        &&
        match (item.opcode, item.payload, successors) with
        | O.Ic_ret, None, [] -> true
        | O.Ic_jmp, Some (Seq.Block target), [ successor ] ->
            Seq.Block_id.equal target successor
        | _ -> false)
    | _ -> false
  in
  let source_matches_identifier ~retained symbol value =
    match Calls.argument_expression_kind (Typed.result_source value) with
    | Calls.Bound_identifier_expression bound -> (
        match
          Bind.occurrence_resolution (Calls.bound_identifier_occurrence bound)
        with
        | Bind.Module_binding publication
          when Bind.publication_kind publication = Bind.Global_variable ->
            Bind.publication_canonical_symbol publication == symbol
        | _ -> false)
    | Calls.Unresolved_expression Calls.Identifier_expression when retained -> (
        (* Retained globals are resolved through the exact task catalog; the
           unresolved outer occurrence must name that retained symbol. *)
        match
          Calls.argument_expression_source_identifier
            (Typed.result_source value)
        with
        | Some occurrence ->
            Bind.occurrence_resolution occurrence = Bind.Outer_candidate
            && Bind.occurrence_name occurrence = Sema.Symbol.name symbol
        | None -> false)
    | _ -> false
  in
  let call_argument source value callee call_type =
    match (Typed.result_call_resolution value, Typed.function_calls source) with
    | Some (Calls.Direct_call direct), [ Typed.Direct_call_result typed ] -> (
        let typed_direct =
          typed |> Typed.direct_source
          |> Sema.Function_call_conversion_policy.direct_source
        in
        let call = Calls.direct_source direct in
        let typed_call = Calls.direct_source typed_direct in
        match (Typed.direct_fixed_results typed, Calls.call_arguments call) with
        | [ fixed ], [ argument ] -> (
            match
              (Typed.fixed_path fixed, Calls.argument_expression argument)
            with
            | Typed.Provided_result result, Some raw ->
                let parameter =
                  fixed |> Typed.fixed_source
                  |> Sema.Function_call_conversion_policy.fixed_source
                  |> Calls.fixed_parameter
                in
                let parameter_type =
                  parameter
                  |> Sema.Function_type_resolution.parameter_type_reference
                  |> Sema.Type_reference.resolved_type
                in
                if
                  call == typed_call
                  && Calls.direct_target_symbol direct == callee
                  && Calls.direct_target_symbol typed_direct == callee
                  && Calls.call_callee_form call = Calls.Identifier_callee
                  && Calls.argument_kind argument = Calls.Provided
                  && Typed.result_source result == raw
                  && Typed.direct_variadic_results typed = []
                  && Option.is_none
                       (Sema.Function_type_resolution.function_variadic_bindings
                          (Calls.direct_active_header direct))
                  && Sema.Type.pointer_depth parameter_type = 0
                  && (match Sema.Type.base parameter_type with
                    | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64))
                      -> true
                    | _ -> false)
                  && Option.fold ~none:false
                       ~some:(Sema.Type.equal call_type)
                       (Typed.result_type value)
                then Some result
                else None
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  let valid_source ~retained ~outer symbol target operation count =
    Option.fold ~none:false
      ~some:(fun source ->
        match
          ( Typed.function_initializers source,
            Typed.function_expression_statements source,
            Typed.function_returns source )
        with
        | [], [], [ returned ] -> (
            let selected =
              Option.bind (Typed.return_value returned) (fun value ->
                  match outer with
                  | None -> Some value
                  | Some (Call (callee, call_type)) ->
                      call_argument source value callee call_type
                  | Some outer -> (
                      let sum_type =
                        match outer with
                        | Literal (_, sum_type) | Global (_, _, _, sum_type) ->
                            sum_type
                        | Call _ -> assert false
                      in
                      match
                        ( Calls.argument_expression_source
                            (Typed.result_source value),
                          Typed.result_binary_operands value )
                      with
                      | ( Some (Frontend.Ast.Binary_expression binary),
                          Some (left, right) )
                        when Option.fold ~none:false
                               ~some:(Sema.Type.equal sum_type)
                               (Typed.result_type value)
                             && binary.binary_operator_spec.spelling = "+"
                             && (match outer with
                               | Literal (expected, _) -> (
                                   match
                                     Calls.argument_expression_kind
                                       (Typed.result_source right)
                                   with
                                   | Calls.Integer_literal bits ->
                                       bits = expected
                                   | _ -> false)
                               | Global
                                   (right_symbol, right_retained, right_type, _)
                                 ->
                                   source_matches_identifier
                                     ~retained:right_retained right_symbol right
                                   && Option.fold ~none:false
                                        ~some:(Sema.Type.equal right_type)
                                        (Typed.result_type right)
                               | Call _ -> false)
                             &&
                             match
                               Calls.argument_expression_kind
                                 (Typed.result_source left)
                             with
                             | Calls.Parenthesized_expression _ -> true
                             | _ -> false -> Typed.result_operand left
                      | _ -> None))
            in
            match selected with
            | Some value -> (
                match
                  ( Calls.argument_expression_source (Typed.result_source value),
                    Typed.result_binary_operands value )
                with
                | ( Some (Frontend.Ast.Binary_expression binary),
                    Some (left, right) ) -> (
                    let spelling = binary.binary_operator_spec.spelling in
                    let operator_matches =
                      (spelling = "+=" && operation = O.Ic_add_equ)
                      || (spelling = "-=" && operation = O.Ic_sub_equ)
                    in
                    operator_matches
                    && source_matches_identifier ~retained symbol left
                    && Option.fold ~none:false ~some:(Sema.Type.equal target)
                         (Typed.result_type left)
                    &&
                    match
                      Calls.argument_expression_kind (Typed.result_source right)
                    with
                    | Calls.Integer_literal bits -> bits = count
                    | _ -> false)
                | _ -> false)
            | None -> false)
        | _ -> false)
      (Function_body.definition_source body)
  in
  if
    (not (Function_body.definition_matches_frame body frame))
    || Option.is_none (Function_body.definition_declaration body)
    || Function_body.compiler_options body <> Sema.Compiler_option.initial_mask
    || Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       <> 0L
    || Frame.function_locations frame <> []
    || not
         (List.for_all
            (fun block -> is_entry block || valid_other block)
            blocks)
  then None
  else
    match List.filter is_entry blocks with
    | [ block ] -> (
        let items = instructions block in
        let valid_terminator jump =
          jump.Seq.opcode = O.Ic_jmp && jump.operands = [] && jump.result = None
          &&
          match (jump.payload, Block_graph.successors block) with
          | Some (Seq.Block target), [ successor ] ->
              Seq.Block_id.equal target successor
          | _ -> false
        in
        let word_literal item expected =
          item.Seq.opcode = O.Ic_imm_i64
          && item.payload = Some (Seq.Integer expected)
          && item.operands = []
          && Option.fold ~none:false
               ~some:(fun type_ ->
                 Sema.Type.pointer_depth type_ = 0
                 &&
                 match Sema.Type.base type_ with
                 | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) ->
                     true
                 | _ -> false)
               item.target_type
        in
        let global_symbol (item : Seq.description) =
          match item.payload with
          | Some (Seq.Symbol symbol) -> Some (symbol, false)
          | Some (Seq.Retained_global reference) ->
              Some (Retained_global.symbol reference, true)
          | _ -> None
        in
        let word_type type_ =
          Sema.Type.pointer_depth type_ = 0
          &&
          match Sema.Type.base type_ with
          | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) -> true
          | _ -> false
        in
        let shape =
          match items with
          | [ address; literal; update; returned; jump ]
            when valid_terminator jump ->
              Option.map
                (fun result ->
                  (address, literal, update, returned, result, None))
                (id update)
          | [ address; literal; update; outer; sum; returned; jump ]
            when valid_terminator jump -> (
              match
                ( id update,
                  id outer,
                  outer.Seq.payload,
                  id sum,
                  sum.Seq.target_type )
              with
              | ( Some update_id,
                  Some outer_id,
                  Some (Seq.Integer outer_bits),
                  Some sum_id,
                  Some sum_type )
                when outer_bits >= 0L
                     && outer_bits <= Int64.of_int32 Int32.max_int
                     && word_literal outer outer_bits
                     && sum.opcode = O.Ic_add && sum.payload = None
                     && sum.operands = [ update_id; outer_id ]
                     && Option.fold ~none:false
                          ~some:(fun type_ ->
                            Sema.Type.pointer_depth type_ = 0
                            &&
                            match Sema.Type.base type_ with
                            | Sema.Type.Primitive
                                (_, (Sema.Primitive_type.I64 | U64)) -> true
                            | _ -> false)
                          sum.target_type ->
                  Some
                    ( address,
                      literal,
                      update,
                      returned,
                      sum_id,
                      Some (Literal (outer_bits, sum_type)) )
              | _ -> None)
          | [
           address; literal; update; right_address; load; sum; returned; jump;
          ]
            when valid_terminator jump -> (
              match
                ( id update,
                  id right_address,
                  global_symbol right_address,
                  id load,
                  load.Seq.target_type,
                  id sum,
                  sum.Seq.target_type )
              with
              | ( Some update_id,
                  Some address_id,
                  Some (right_symbol, right_retained),
                  Some load_id,
                  Some right_type,
                  Some sum_id,
                  Some sum_type )
                when Sema.Symbol.kind right_symbol = Sema.Symbol.Global_variable
                     && (right_address.opcode = O.Ic_imm_i64
                        || right_address.opcode = O.Ic_abs_addr)
                     && right_address.operands = []
                     && Option.fold ~none:false
                          ~some:(fun pointer ->
                            match Sema.Type.dereference pointer with
                            | Ok actual -> Sema.Type.equal actual right_type
                            | Error _ -> false)
                          right_address.target_type
                     && word_type right_type && load.opcode = O.Ic_deref
                     && load.payload = None
                     && load.operands = [ address_id ]
                     && sum.opcode = O.Ic_add && sum.payload = None
                     && sum.operands = [ update_id; load_id ]
                     && word_type sum_type ->
                  Some
                    ( address,
                      literal,
                      update,
                      returned,
                      sum_id,
                      Some
                        (Global
                           (right_symbol, right_retained, right_type, sum_type))
                    )
              | _ -> None)
          | [
           call_start;
           address;
           literal;
           update;
           call;
           cleanup;
           call_end;
           returned;
           jump;
          ]
            when valid_terminator jump -> (
              match
                ( call_start.Seq.payload,
                  call.payload,
                  call_end.payload,
                  id call_end,
                  call_end.target_type )
              with
              | ( Some (Seq.Symbol callee),
                  Some (Seq.Symbol called),
                  Some (Seq.Symbol finished),
                  Some result,
                  Some call_type )
                when callee == called && callee == finished
                     && Sema.Symbol.kind callee = Sema.Symbol.Function
                     && call_start.opcode = O.Ic_call_start
                     && call_start.operands = [] && call_start.result = None
                     && call_start.target_type = None
                     && call.opcode = O.Ic_call && call.operands = []
                     && call.result = None
                     && cleanup.opcode = O.Ic_add_rsp1
                     && cleanup.operands = [] && cleanup.result = None
                     && cleanup.payload = Some (Seq.Integer 8L)
                     && call_end.opcode = O.Ic_call_end
                     && call_end.operands = []
                     && Option.equal Sema.Type.equal call.target_type
                          (Some call_type)
                     && Option.equal Sema.Type.equal cleanup.target_type
                          (Some call_type)
                     && word_type call_type ->
                  Some
                    ( address,
                      literal,
                      update,
                      returned,
                      result,
                      Some (Call (callee, call_type)) )
              | _ -> None)
          | _ -> None
        in
        match shape with
        | Some (address, literal, update, returned, return_id, outer) -> (
            let address_symbol = global_symbol address in
            match
              ( address_symbol,
                id address,
                literal.payload,
                id literal,
                update.target_type,
                id update )
            with
            | ( Some (symbol, retained),
                Some address_id,
                Some (Seq.Integer count),
                Some literal_id,
                Some target,
                Some update_id )
              when count >= 0L
                   && count <= Int64.of_int32 Int32.max_int
                   && Sema.Symbol.kind symbol = Sema.Symbol.Global_variable
                   && (address.opcode = O.Ic_imm_i64
                      || address.opcode = O.Ic_abs_addr)
                   && address.operands = [] && word_literal literal count
                   && List.length
                        (List.sort_uniq Int.compare
                           (List.map
                              (fun (item : Seq.description) ->
                                Seq.Instruction_id.to_int item.instruction_id)
                              all_items))
                      = List.length all_items
                   && (let ids =
                         List.filter_map id all_items
                         |> List.map Seq.Value_id.to_int
                       in
                       List.length (List.sort_uniq Int.compare ids)
                       = List.length ids)
                   && (update.opcode = O.Ic_add_equ
                      || update.opcode = O.Ic_sub_equ)
                   && update.payload = None
                   && update.operands = [ address_id; literal_id ]
                   && returned.opcode = O.Ic_return_val
                   && returned.payload = None && returned.result = None
                   && returned.operands = [ return_id ]
                   && List.for_all
                        (fun (item : Seq.description) ->
                          if
                            item == update
                            &&
                            match outer with
                            | Some (Call _) -> true
                            | _ -> false
                          then item.flags = 0x2000L
                          else item.flags = 0L)
                        items
                   && Sema.Type.pointer_depth target = 0
                   && (match Sema.Type.base target with
                     | Sema.Type.Primitive
                         ( _,
                           (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32)
                         ) -> true
                     | _ -> false)
                   && Option.fold ~none:false
                        ~some:(fun pointer ->
                          match Sema.Type.dereference pointer with
                          | Ok actual -> Sema.Type.equal actual target
                          | Error _ -> false)
                        address.target_type
                   && valid_source ~retained ~outer symbol target update.opcode
                        count ->
                ignore update_id;
                Some
                  {
                    frame;
                    body;
                    definition = Function_body.definition_declaration body;
                    source = Function_body.definition_source body;
                    blocks = snapshot body;
                    instruction = update;
                    symbol;
                  }
            | _ -> None)
        | _ -> None)
    | _ -> None

let matches proof ~frame ~body ~(instruction : Seq.description) ~symbol =
  let original = proof.instruction in
  proof.frame == frame && proof.body == body && proof.symbol == symbol
  && Option.equal ( == ) proof.definition
       (Function_body.definition_declaration body)
  && Option.equal ( == ) proof.source (Function_body.definition_source body)
  && same_blocks proof.blocks (snapshot body)
  && Seq.Instruction_id.equal original.instruction_id instruction.instruction_id
  && original.opcode = instruction.opcode
  && original.operands = instruction.operands
  && original.result = instruction.result
  && Option.equal Sema.Type.equal original.target_type instruction.target_type
  && original.payload = None && instruction.payload = None
  && (original.flags = instruction.flags
     || (original.flags = 0x2000L && instruction.flags = 0L))
  && original.span = instruction.span
  &&
  match analyze ~frame ~body with
  | Some current -> current.instruction == original && current.symbol == symbol
  | None -> false
