module Sequence = Instruction_sequence
module Literal = Literal_lowering
module Semantic_result = Sema.Function_call_expression_result
module Semantic_source = Sema.Function_call_resolution
module Type = Sema.Type
module Int_map = Map.Make (Int)

type t = {
  sequence_ : Sequence.t;
  result_value_ : Sequence.Value_id.t;
  result_type_ : Type.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_expression
type checked_type = Checked_type of Type.t | Unsupported_type

type cancellation =
  | No_cancellation
  | Canceled_dereference of Semantic_result.expression_result

type plan_node =
  | Literal of Semantic_result.expression_result
  | Alias of {
      result : Semantic_result.expression_result;
      operand : Semantic_result.expression_result;
    }
  | Unary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      operand : Semantic_result.expression_result;
    }
  | Binary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      left : Semantic_result.expression_result;
      right : Semantic_result.expression_result;
    }

type task =
  | Visit of Semantic_result.expression_result
  | Finish_alias of {
      result : Semantic_result.expression_result;
      operand : Semantic_result.expression_result;
    }
  | Finish_unary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      operand : Semantic_result.expression_result;
    }
  | Finish_binary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      left : Semantic_result.expression_result;
      right : Semantic_result.expression_result;
    }

type planned = Planned of plan_node list | Unsupported_plan

type lowered_node = {
  lowered_value : Sequence.Value_id.t;
  lowered_type : Type.t;
}

type allocator = { mutable instruction : int; mutable value : int }

let reference_commit = Opcode.reference_commit

let result_span result =
  match Semantic_result.result_origin result with
  | Sema.Symbol.Source_location location -> Some location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ -> None

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error ?span message = lowering_error ?span "HCIRL0004" message

let type_equal left right =
  Type.pointer_depth left = Type.pointer_depth right
  &&
  match (Type.base left, Type.base right) with
  | ( Type.Primitive (left_form, left_primitive),
      Type.Primitive (right_form, right_primitive) ) ->
      left_form = right_form
      && Sema.Primitive_type.equal left_primitive right_primitive
  | Type.Aggregate left_symbol, Type.Aggregate right_symbol ->
      Sema.Symbol.Id.equal
        (Sema.Symbol.id left_symbol)
        (Sema.Symbol.id right_symbol)
  | Type.Primitive _, Type.Aggregate _ | Type.Aggregate _, Type.Primitive _ ->
      false

let checked_integer_type result =
  match Semantic_result.result_type result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic expression does not have a checked result type")
  | Some type_ -> (
      match
        ( Semantic_result.result_class result,
          Type.pointer_depth type_,
          Type.base type_ )
      with
      | Semantic_result.Integer_result, 0, Type.Primitive (_, primitive)
        when (Sema.Primitive_type.info primitive).category
             <> Sema.Primitive_type.Floating
             && not (Sema.Primitive_type.is_zero_sized primitive) ->
          Ok (Checked_type type_)
      | Semantic_result.Integer_result, _, (Type.Primitive _ | Type.Aggregate _)
      | ( (Semantic_result.F64_result | Semantic_result.Unresolved_actual_class),
          _,
          (Type.Primitive _ | Type.Aggregate _) ) -> Ok Unsupported_type)

let checked_integer_or_pointer_type result =
  match Semantic_result.result_type result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic expression does not have a checked result type")
  | Some type_ -> (
      match (Semantic_result.result_class result, Type.base type_) with
      | Semantic_result.Integer_result, Type.Primitive (_, primitive)
        when (Sema.Primitive_type.info primitive).category
             <> Sema.Primitive_type.Floating
             && not (Sema.Primitive_type.is_zero_sized primitive) ->
          Ok (Checked_type type_)
      | Semantic_result.Integer_result, (Type.Primitive _ | Type.Aggregate _)
      | ( (Semantic_result.F64_result | Semantic_result.Unresolved_actual_class),
          (Type.Primitive _ | Type.Aggregate _) ) -> Ok Unsupported_type)

let accepted_binary_opcode = function
  | Opcode.Ic_shl
  | Opcode.Ic_shr
  | Opcode.Ic_mul
  | Opcode.Ic_div
  | Opcode.Ic_mod
  | Opcode.Ic_and
  | Opcode.Ic_or
  | Opcode.Ic_xor
  | Opcode.Ic_add
  | Opcode.Ic_sub
  | Opcode.Ic_equ_equ
  | Opcode.Ic_not_equ
  | Opcode.Ic_less
  | Opcode.Ic_greater_equ
  | Opcode.Ic_greater
  | Opcode.Ic_less_equ
  | Opcode.Ic_and_and
  | Opcode.Ic_or_or
  | Opcode.Ic_xor_xor -> true
  | _ -> false

let accepted_prefix = function
  | Semantic_source.Unary_minus ->
      Some (Opcode.Ic_unary_minus, "unary-minus expression")
  | Semantic_source.Logical_not -> Some (Opcode.Ic_not, "logical-not expression")
  | Semantic_source.Bitwise_not ->
      Some (Opcode.Ic_com, "bitwise-complement expression")
  | Semantic_source.Unary_plus
  | Semantic_source.Dereference
  | Semantic_source.Address_of
  | Semantic_source.Pre_increment
  | Semantic_source.Pre_decrement -> None

let checked_operand result expected_source description =
  match Semantic_result.result_operand result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf
              "typed semantic %s does not retain its checked operand"
              description))
  | Some operand when Semantic_result.result_source operand != expected_source
    ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf
              "typed semantic %s operand does not match its source expression"
              description))
  | Some operand -> Ok operand

let checked_binary_operands result binary =
  match Semantic_result.result_binary_operands result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic binary expression does not retain its checked \
            children")
  | Some (left, right)
    when Semantic_result.result_source left
         != Semantic_source.binary_left binary
         || Semantic_result.result_source right
            != Semantic_source.binary_right binary ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic binary children do not match their source \
            expressions")
  | Some operands -> Ok operands

let operator_span result description = function
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf "typed semantic %s does not have a source location"
              description))

let binary_span result binary =
  Semantic_source.binary_operator_origin binary
  |> operator_span result "binary operator"

let unary_span result description prefix =
  Semantic_source.prefix_operator_origin prefix
  |> operator_span result description

let internal_i64 type_ =
  Type.pointer_depth type_ = 0
  &&
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, Sema.Primitive_type.I64) -> true
  | Type.Primitive _ | Type.Aggregate _ -> false

let checked_numeric_unary_types result opcode operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | None, _ | _, None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic unary expression does not have complete checked \
            types")
  | Some result_type, Some operand_type ->
      let valid =
        match opcode with
        | Opcode.Ic_unary_minus | Opcode.Ic_not ->
            type_equal result_type operand_type
        | Opcode.Ic_com -> internal_i64 result_type
        | _ -> false
      in
      if valid then Ok ()
      else
        Error
          (metadata_error ?span:(result_span result)
             "typed semantic unary result type does not match the audited \
              operator rule")

let checked_pointer_unary_types result opcode operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | None, _ | _, None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic pointer expression does not have complete checked \
            types")
  | Some result_type, Some operand_type -> (
      let expected =
        match opcode with
        | Opcode.Ic_deref -> (
            match Type.dereference operand_type with
            | Ok type_ -> Ok type_
            | Error _ -> Ok operand_type)
        | Opcode.Ic_addr -> Type.pointer_to operand_type
        | _ -> Error "not a pointer prefix opcode"
      in
      match expected with
      | Error message ->
          Error (metadata_error ?span:(result_span result) message)
      | Ok expected_type when type_equal result_type expected_type -> Ok ()
      | Ok _ ->
          Error
            (metadata_error ?span:(result_span result)
               "typed semantic pointer result type does not match the audited \
                operator rule"))

let checked_alias_types result operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | Some result_type, Some operand_type when type_equal result_type operand_type
    -> Ok ()
  | Some _, Some _ ->
      Error
        (metadata_error ?span:(result_span result)
           "transparent expression changes its checked operand type")
  | None, _ | _, None ->
      Error
        (metadata_error ?span:(result_span result)
           "transparent expression does not have complete checked types")

let validate_numeric_unary result opcode operand =
  match checked_integer_type operand with
  | Error item -> Error item
  | Ok Unsupported_type -> Ok false
  | Ok (Checked_type _) -> (
      match checked_integer_type result with
      | Error item -> Error item
      | Ok Unsupported_type -> Ok false
      | Ok (Checked_type _) ->
          Result.map
            (fun () -> true)
            (checked_numeric_unary_types result opcode operand))

let validate_pointer_unary result opcode operand =
  match checked_integer_or_pointer_type operand with
  | Error item -> Error item
  | Ok Unsupported_type -> Ok false
  | Ok (Checked_type _) -> (
      match checked_integer_or_pointer_type result with
      | Error item -> Error item
      | Ok Unsupported_type -> Ok false
      | Ok (Checked_type _) ->
          Result.map
            (fun () -> true)
            (checked_pointer_unary_types result opcode operand))

let cancellable_dereference operand =
  let current = ref operand in
  let result = ref No_cancellation in
  let error = ref None in
  let searching = ref true in
  while !searching && Option.is_none !error do
    match
      Semantic_result.result_source !current
      |> Semantic_source.argument_expression_kind
    with
    | Semantic_source.Parenthesized_expression source -> (
        match checked_operand !current source "parenthesized expression" with
        | Error item -> error := Some item
        | Ok next -> (
            match checked_alias_types !current next with
            | Error item -> error := Some item
            | Ok () -> current := next))
    | Semantic_source.Prefix_expression prefix
      when Semantic_source.prefix_operator prefix = Semantic_source.Unary_plus
      -> (
        match
          checked_operand !current
            (Semantic_source.prefix_operand prefix)
            "unary-plus expression"
        with
        | Error item -> error := Some item
        | Ok next -> (
            match checked_alias_types !current next with
            | Error item -> error := Some item
            | Ok () -> current := next))
    | Semantic_source.Prefix_expression prefix
      when Semantic_source.prefix_operator prefix = Semantic_source.Dereference
      -> (
        match
          ( checked_operand !current
              (Semantic_source.prefix_operand prefix)
              "dereference expression",
            unary_span !current "dereference expression" prefix )
        with
        | Error item, _ | _, Error item -> error := Some item
        | Ok next, Ok _ -> (
            match checked_pointer_unary_types !current Opcode.Ic_deref next with
            | Error item -> error := Some item
            | Ok () ->
                result := Canceled_dereference next;
                searching := false))
    | Semantic_source.Integer_literal _
    | Semantic_source.Float_literal _
    | Semantic_source.Character_literal _
    | Semantic_source.String_literal _
    | Semantic_source.Prefix_expression _
    | Semantic_source.Postfix_expression _
    | Semantic_source.Postfix_cast_expression _
    | Semantic_source.Binary_expression _
    | Semantic_source.Index_expression _
    | Semantic_source.Member_access_expression _
    | Semantic_source.Bound_identifier_expression _
    | Semantic_source.Top_level_bound_identifier_expression _
    | Semantic_source.Unresolved_expression _ -> searching := false
  done;
  match !error with
  | Some item -> Error item
  | None -> Ok !result

let validate_binary result left right =
  match (checked_integer_type left, checked_integer_type right) with
  | Error item, _ | _, Error item -> Error item
  | Ok Unsupported_type, _ | _, Ok Unsupported_type -> Ok false
  | Ok (Checked_type _), Ok (Checked_type _) -> (
      match checked_integer_type result with
      | Error item -> Error item
      | Ok Unsupported_type -> Ok false
      | Ok (Checked_type _) -> Ok true)

let plan root =
  let pending = ref [ Visit root ] in
  let reversed = ref [] in
  let unsupported = ref false in
  let error = ref None in
  while !pending <> [] && (not !unsupported) && Option.is_none !error do
    match !pending with
    | [] -> ()
    | task :: remaining -> (
        pending := remaining;
        match task with
        | Visit result -> (
            match
              Semantic_result.result_source result
              |> Semantic_source.argument_expression_kind
            with
            | Semantic_source.Integer_literal _
            | Semantic_source.Character_literal _ -> (
                match checked_integer_type result with
                | Error item -> error := Some item
                | Ok Unsupported_type -> unsupported := true
                | Ok (Checked_type _) -> reversed := Literal result :: !reversed
                )
            | Semantic_source.Parenthesized_expression source -> (
                match
                  checked_operand result source "parenthesized expression"
                with
                | Error item -> error := Some item
                | Ok operand ->
                    pending :=
                      Visit operand
                      :: Finish_alias { result; operand }
                      :: !pending)
            | Semantic_source.Prefix_expression prefix -> (
                let source_operand = Semantic_source.prefix_operand prefix in
                match Semantic_source.prefix_operator prefix with
                | Semantic_source.Unary_plus -> (
                    match
                      checked_operand result source_operand
                        "unary-plus expression"
                    with
                    | Error item -> error := Some item
                    | Ok operand ->
                        pending :=
                          Visit operand
                          :: Finish_alias { result; operand }
                          :: !pending)
                | (Semantic_source.Dereference | Semantic_source.Address_of) as
                  operator -> (
                    let opcode, description =
                      match operator with
                      | Semantic_source.Dereference ->
                          (Opcode.Ic_deref, "dereference expression")
                      | Semantic_source.Address_of ->
                          (Opcode.Ic_addr, "address-of expression")
                      | Semantic_source.Unary_plus
                      | Semantic_source.Unary_minus
                      | Semantic_source.Logical_not
                      | Semantic_source.Bitwise_not
                      | Semantic_source.Pre_increment
                      | Semantic_source.Pre_decrement -> assert false
                    in
                    match
                      ( checked_operand result source_operand description,
                        unary_span result description prefix )
                    with
                    | Error item, _ | _, Error item -> error := Some item
                    | Ok operand, Ok span -> (
                        match validate_pointer_unary result opcode operand with
                        | Error item -> error := Some item
                        | Ok false -> unsupported := true
                        | Ok true ->
                            if Opcode.equal opcode Opcode.Ic_addr then
                              match cancellable_dereference operand with
                              | Error item -> error := Some item
                              | Ok No_cancellation ->
                                  pending :=
                                    Visit operand
                                    :: Finish_unary
                                         { result; opcode; span; operand }
                                    :: !pending
                              | Ok (Canceled_dereference source_operand) ->
                                  pending :=
                                    Visit source_operand
                                    :: Finish_unary
                                         {
                                           result;
                                           opcode;
                                           span;
                                           operand = source_operand;
                                         }
                                    :: !pending
                            else
                              pending :=
                                Visit operand
                                :: Finish_unary
                                     { result; opcode; span; operand }
                                :: !pending))
                | operator -> (
                    match accepted_prefix operator with
                    | None -> unsupported := true
                    | Some (opcode, description) -> (
                        match
                          ( checked_operand result source_operand description,
                            unary_span result description prefix )
                        with
                        | Error item, _ | _, Error item -> error := Some item
                        | Ok operand, Ok span -> (
                            match
                              validate_numeric_unary result opcode operand
                            with
                            | Error item -> error := Some item
                            | Ok false -> unsupported := true
                            | Ok true ->
                                pending :=
                                  Visit operand
                                  :: Finish_unary
                                       { result; opcode; span; operand }
                                  :: !pending))))
            | Semantic_source.Binary_expression binary -> (
                let opcode = Semantic_source.binary_operator binary in
                if not (accepted_binary_opcode opcode) then unsupported := true
                else
                  match
                    ( checked_binary_operands result binary,
                      binary_span result binary )
                  with
                  | Error item, _ | _, Error item -> error := Some item
                  | Ok (left, right), Ok span -> (
                      match validate_binary result left right with
                      | Error item -> error := Some item
                      | Ok false -> unsupported := true
                      | Ok true ->
                          pending :=
                            Visit left :: Visit right
                            :: Finish_binary
                                 { result; opcode; span; left; right }
                            :: !pending))
            | Semantic_source.Float_literal _
            | Semantic_source.String_literal _
            | Semantic_source.Postfix_expression _
            | Semantic_source.Postfix_cast_expression _
            | Semantic_source.Index_expression _
            | Semantic_source.Member_access_expression _
            | Semantic_source.Bound_identifier_expression _
            | Semantic_source.Top_level_bound_identifier_expression _
            | Semantic_source.Unresolved_expression _ -> unsupported := true)
        | Finish_alias { result; operand } ->
            reversed := Alias { result; operand } :: !reversed
        | Finish_unary { result; opcode; span; operand } ->
            reversed := Unary { result; opcode; span; operand } :: !reversed
        | Finish_binary { result; opcode; span; left; right } ->
            reversed :=
              Binary { result; opcode; span; left; right } :: !reversed)
  done;
  match (!error, !unsupported) with
  | Some item, _ -> Error [ item ]
  | None, true -> Ok Unsupported_plan
  | None, false -> Ok (Planned (List.rev !reversed))

let result_key result =
  result |> Semantic_result.result_id |> Semantic_result.Id.to_int

let find_lowered lowered result description =
  match Int_map.find_opt (result_key result) lowered with
  | Some node -> Ok node
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf "%s was not lowered before its parent" description))

let take_identity allocator span =
  if allocator.instruction = Int.max_int || allocator.value = Int.max_int then
    Error
      (lowering_error ?span "HCIRL0005"
         "cannot allocate another expression identity because the host integer \
          range is exhausted")
  else
    match
      ( Sequence.Instruction_id.of_int allocator.instruction,
        Sequence.Value_id.of_int allocator.value )
    with
    | Ok instruction_id, Ok value_id ->
        allocator.instruction <- allocator.instruction + 1;
        allocator.value <- allocator.value + 1;
        Ok (instruction_id, value_id)
    | Error item, _ | _, Error item -> Error item

let one_literal_description lowered =
  match
    lowered |> Literal.sequence |> Sequence.instructions
    |> List.map Sequence.description
  with
  | [ description ] -> Ok description
  | _ ->
      Error
        (metadata_error
           "integer literal lowering did not produce exactly one instruction")

let lower_literal allocator result =
  let span = result_span result in
  match take_identity allocator span with
  | Error item -> Error item
  | Ok (instruction_id, value_id) -> (
      match Literal.lower_typed_result ~instruction_id ~value_id result with
      | Error (item :: _) -> Error item
      | Error [] ->
          Error
            (metadata_error ?span
               "integer literal lowering failed without a diagnostic")
      | Ok Literal.Not_literal ->
          Error
            (metadata_error ?span
               "checked integer literal was not accepted by literal lowering")
      | Ok (Literal.Lowered lowered) -> (
          match one_literal_description lowered with
          | Error item -> Error item
          | Ok description ->
              Ok
                ( description,
                  {
                    lowered_value = value_id;
                    lowered_type = Literal.result_type lowered;
                  } )))

let emit_plan ~instruction_id ~value_id nodes =
  let allocator =
    {
      instruction = Sequence.Instruction_id.to_int instruction_id;
      value = Sequence.Value_id.to_int value_id;
    }
  in
  let lowered = ref Int_map.empty in
  let descriptions_rev = ref [] in
  let error = ref None in
  List.iter
    (fun node ->
      if Option.is_none !error then
        match node with
        | Literal result -> (
            match lower_literal allocator result with
            | Error item -> error := Some item
            | Ok (description, lowered_node) ->
                descriptions_rev := description :: !descriptions_rev;
                lowered := Int_map.add (result_key result) lowered_node !lowered
            )
        | Alias { result; operand } -> (
            match find_lowered !lowered operand "transparent operand" with
            | Error item -> error := Some item
            | Ok lowered_operand -> (
                match Semantic_result.result_type result with
                | None ->
                    error :=
                      Some
                        (metadata_error ?span:(result_span result)
                           "transparent expression does not have a checked \
                            result type")
                | Some result_type ->
                    if not (type_equal result_type lowered_operand.lowered_type)
                    then
                      error :=
                        Some
                          (metadata_error ?span:(result_span result)
                             "transparent expression changes its checked \
                              operand type")
                    else
                      lowered :=
                        Int_map.add (result_key result) lowered_operand !lowered
                ))
        | Unary { result; opcode; span; operand } -> (
            match
              ( find_lowered !lowered operand "unary operand",
                Semantic_result.result_type result )
            with
            | Error item, _ -> error := Some item
            | _, None ->
                error :=
                  Some
                    (metadata_error ~span
                       "unary expression does not have a checked result type")
            | Ok operand_node, Some result_type -> (
                match take_identity allocator (Some span) with
                | Error item -> error := Some item
                | Ok (instruction_id, value_id) ->
                    let description : Sequence.description =
                      {
                        instruction_id;
                        opcode;
                        operands = [ operand_node.lowered_value ];
                        result = Some { value_id };
                        target_type = Some result_type;
                        payload = None;
                        flags = 0L;
                        span = Some span;
                      }
                    in
                    descriptions_rev := description :: !descriptions_rev;
                    lowered :=
                      Int_map.add (result_key result)
                        { lowered_value = value_id; lowered_type = result_type }
                        !lowered))
        | Binary { result; opcode; span; left; right } -> (
            match
              ( find_lowered !lowered left "left binary operand",
                find_lowered !lowered right "right binary operand",
                Semantic_result.result_type result )
            with
            | Error item, _, _ | _, Error item, _ -> error := Some item
            | _, _, None ->
                error :=
                  Some
                    (metadata_error ~span
                       "binary expression does not have a checked result type")
            | Ok left_node, Ok right_node, Some result_type -> (
                match take_identity allocator (Some span) with
                | Error item -> error := Some item
                | Ok (instruction_id, value_id) ->
                    let description : Sequence.description =
                      {
                        instruction_id;
                        opcode;
                        operands =
                          [ left_node.lowered_value; right_node.lowered_value ];
                        result = Some { value_id };
                        target_type = Some result_type;
                        payload = None;
                        flags = 0L;
                        span = Some span;
                      }
                    in
                    descriptions_rev := description :: !descriptions_rev;
                    lowered :=
                      Int_map.add (result_key result)
                        { lowered_value = value_id; lowered_type = result_type }
                        !lowered)))
    nodes;
  match !error with
  | Some item -> Error [ item ]
  | None -> (
      match List.rev !descriptions_rev with
      | [] ->
          Error
            [ metadata_error "expression lowering produced no instructions" ]
      | descriptions -> (
          match Sequence.create descriptions with
          | Error items -> Error items
          | Ok sequence -> (
              let root =
                match List.rev nodes with
                | Literal result :: _
                | Alias { result; _ } :: _
                | Unary { result; _ } :: _
                | Binary { result; _ } :: _ -> result
                | [] -> assert false
              in
              match find_lowered !lowered root "expression result" with
              | Error item -> Error [ item ]
              | Ok lowered_root -> (
                  match
                    ( Sequence.Instruction_id.of_int allocator.instruction,
                      Sequence.Value_id.of_int allocator.value )
                  with
                  | Ok next_instruction_id_, Ok next_value_id_ ->
                      Ok
                        {
                          sequence_ = sequence;
                          result_value_ = lowered_root.lowered_value;
                          result_type_ = lowered_root.lowered_type;
                          next_instruction_id_;
                          next_value_id_;
                        }
                  | Error item, _ | _, Error item -> Error [ item ]))))

let lower_typed_result ~instruction_id ~value_id result =
  match plan result with
  | Error items -> Error items
  | Ok Unsupported_plan -> Ok Unsupported_expression
  | Ok (Planned nodes) ->
      emit_plan ~instruction_id ~value_id nodes
      |> Result.map (fun t -> Lowered t)

let sequence lowered = lowered.sequence_
let result_value lowered = lowered.result_value_
let result_type lowered = lowered.result_type_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  Printf.sprintf
    "holyc-ir-expression-v1 reference=%s\n\
     result=%%v%d result-type=%s next-instruction=%d next-value=%d\n\
     %s"
    reference_commit
    (Sequence.Value_id.to_int lowered.result_value_)
    (Sequence.type_name lowered.result_type_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Sequence.human_body lowered.sequence_)
