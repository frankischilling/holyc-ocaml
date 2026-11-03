module Frame = Sema.Function_frame_layout
module Seq = Instruction_sequence
module O = Opcode
module Typed = Sema.Function_call_expression_result
module Calls = Sema.Function_call_resolution
module Bind = Sema.Module_expression_binding

type stamp = Seq.description * Seq.description
type block_stamp = Seq.Block_id.t * stamp list * Seq.Block_id.t list

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
  let valid_source ~retained symbol target operation count =
    Option.fold ~none:false
      ~some:(fun source ->
        match
          ( Typed.function_initializers source,
            Typed.function_expression_statements source,
            Typed.function_returns source )
        with
        | [], [], [ returned ] -> (
            match Typed.return_value returned with
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
                    let source_matches =
                      match
                        Calls.argument_expression_kind
                          (Typed.result_source left)
                      with
                      | Calls.Bound_identifier_expression bound -> (
                          match
                            Bind.occurrence_resolution
                              (Calls.bound_identifier_occurrence bound)
                          with
                          | Bind.Module_binding publication
                            when Bind.publication_kind publication
                                 = Bind.Global_variable ->
                              Bind.publication_canonical_symbol publication
                              == symbol
                          | _ -> false)
                      | Calls.Unresolved_expression Calls.Identifier_expression
                        when retained -> (
                          (* Retained globals are resolved through the exact task
                             catalog; the outer occurrence must name that symbol. *)
                          match
                            Calls.argument_expression_source_identifier
                              (Typed.result_source left)
                          with
                          | Some occurrence ->
                              Bind.occurrence_resolution occurrence
                              = Bind.Outer_candidate
                              && Bind.occurrence_name occurrence
                                 = Sema.Symbol.name symbol
                          | None -> false)
                      | _ -> false
                    in
                    operator_matches && source_matches
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
        match items with
        | [ address; literal; update; returned; jump ]
          when valid_terminator jump -> (
            let address_symbol =
              match address.payload with
              | Some (Seq.Symbol symbol) -> Some (symbol, false)
              | Some (Seq.Retained_global reference) ->
                  Some (Retained_global.symbol reference, true)
              | _ -> None
            in
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
                   && address.operands = []
                   && literal.opcode = O.Ic_imm_i64
                   && literal.operands = []
                   && Option.fold ~none:false
                        ~some:(fun type_ ->
                          Sema.Type.pointer_depth type_ = 0
                          &&
                          match Sema.Type.base type_ with
                          | Sema.Type.Primitive
                              (_, (Sema.Primitive_type.I64 | U64)) -> true
                          | _ -> false)
                        literal.target_type
                   && List.length
                        (List.sort_uniq Int.compare
                           (List.map
                              (fun (item : Seq.description) ->
                                Seq.Instruction_id.to_int item.instruction_id)
                              all_items))
                      = List.length all_items
                   && List.length
                        (List.sort_uniq Int.compare
                           (List.map Seq.Value_id.to_int
                              [ address_id; literal_id; update_id ]))
                      = 3
                   && (update.opcode = O.Ic_add_equ
                      || update.opcode = O.Ic_sub_equ)
                   && update.payload = None
                   && update.operands = [ address_id; literal_id ]
                   && returned.opcode = O.Ic_return_val
                   && returned.payload = None && returned.result = None
                   && returned.operands = [ update_id ]
                   && List.for_all
                        (fun (item : Seq.description) -> item.flags = 0L)
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
                   && valid_source ~retained symbol target update.opcode count
              ->
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
  && original.flags = instruction.flags
  && original.span = instruction.span
  &&
  match analyze ~frame ~body with
  | Some current -> current.instruction == original && current.symbol == symbol
  | None -> false
