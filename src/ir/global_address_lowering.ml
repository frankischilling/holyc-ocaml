module Sequence = Instruction_sequence
module Result = Sema.Function_call_expression_result
module Source = Sema.Function_call_resolution
module Binding = Sema.Module_expression_binding
module Top = Sema.Top_level_outer_expression_binding
module Global = Sema.Global_type_resolution
module Type = Sema.Type

type prepared_address = {
  slot : Integer_globals.storage_slot;
  address_type : Type.t;
  span : Common.Span.t;
}

type t = {
  sequence_ : Sequence.t;
  result_value_ : Sequence.Value_id.t;
  result_type_ : Type.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

let error ?span code message =
  Error [ { Sequence.code; message; instruction_id = None; span } ]

let prepare_global ~globals result =
  let source = Result.result_source result in
  let origin = Source.argument_expression_origin source in
  let bound =
    match Source.argument_expression_kind source with
    | Source.Bound_identifier_expression identifier -> (
        let occurrence = Source.bound_identifier_occurrence identifier in
        match Binding.occurrence_resolution occurrence with
        | Binding.Module_binding publication ->
            Some
              ( publication,
                Binding.occurrence_name occurrence,
                Binding.occurrence_origin occurrence,
                Some
                  ( Source.bound_identifier_type identifier,
                    Source.bound_identifier_array_rank identifier,
                    Source.bound_identifier_shape identifier ) )
        | _ -> None)
    | Source.Top_level_bound_identifier_expression identifier -> (
        let occurrence =
          Source.top_level_bound_identifier_occurrence identifier
        in
        match Top.occurrence_resolution occurrence with
        | Top.Module_binding publication ->
            Some
              ( publication,
                Top.occurrence_name occurrence,
                Top.occurrence_origin occurrence,
                None )
        | _ -> None)
    | _ -> None
  in
  match bound with
  | None -> Ok None
  | Some (publication, name, occurrence_origin, source_type) -> (
      if Binding.publication_kind publication <> Binding.Global_variable then
        Ok None
      else
        let span =
          match origin with
          | Sema.Symbol.Source_location location -> Some location.span
          | _ -> None
        in
        let invalid message = error ?span "HCIRL0004" message in
        let symbol = Binding.publication_source_symbol publication in
        match Integer_globals.find globals symbol with
        | None ->
            invalid "bound global is absent from the supplied program storage"
        | Some slot -> (
            let type_ = Integer_globals.slot_type slot in
            let global =
              Integer_globals.slot_record slot
              |> Sema.Global_record_classification.classified_record_source
              |> Sema.Global_resolution.global_record_global
            in
            if
              Binding.publication_canonical_symbol publication != symbol
              || Binding.publication_item_index publication
                 <> Global.global_item_index global
              || Binding.publication_declarator_index publication
                 <> Global.global_declarator_index global
              || not (String.equal name (Sema.Symbol.name symbol))
            then
              invalid "global occurrence and declaration publication disagree"
            else if
              Result.result_origin result <> origin
              || occurrence_origin <> origin
            then
              invalid
                "global identifier origins disagree across semantic results"
            else if
              (not
                 (Option.fold ~none:false ~some:(Type.equal type_)
                    (Result.result_type result)))
              || Result.result_array_rank result <> 0
              || Option.is_some (Result.result_function_declaration result)
              || Option.is_some (Result.result_function_address_path result)
            then
              invalid
                "global identifier type or value shape disagrees with its \
                 object"
            else if
              not
                (match Result.result_category result with
                | Result.Object_value | Result.Lvalue -> true
                | _ -> false)
            then
              invalid
                "global scalar identifier has an inconsistent result category"
            else if
              not
                (match source_type with
                | None -> true
                | Some (source_type, 0, Source.Object_value) ->
                    Type.equal source_type type_
                | _ -> false)
            then
              invalid
                "global source identifier has inconsistent scalar metadata"
            else
              match (span, Type.pointer_to type_) with
              | Some span, Ok address_type ->
                  Ok
                    (Some
                       {
                         slot = Integer_globals.global_storage slot;
                         address_type;
                         span;
                       })
              | _ ->
                  invalid
                    "global identifier has no checked pointer type or physical \
                     span"))

let prepare ?frame ~globals result =
  let ( let* ) = Stdlib.Result.bind in
  match Source.argument_expression_kind (Result.result_source result) with
  | Source.Bound_identifier_expression identifier -> (
      let occurrence = Source.bound_identifier_occurrence identifier in
      match Binding.occurrence_resolution occurrence with
      | Binding.Local_binding binding
        when Sema.Function_binding_index.binding_kind binding
             = Sema.Function_binding_index.Static_local -> (
          let span =
            match Result.result_origin result with
            | Sema.Symbol.Source_location location -> Some location.span
            | _ -> None
          in
          let invalid message = error ?span "HCIRL0004" message in
          match frame with
          | None ->
              invalid
                "static identifier requires its exact declaring function frame"
          | Some frame -> (
              let* _ = Frame_address_lowering.prepare ~frame result in
              let symbol = Sema.Function_binding_index.binding_symbol binding in
              match Integer_globals.find_static globals symbol with
              | Some slot when Integer_globals.static_frame slot == frame -> (
                  match
                    ( span,
                      Type.pointer_to
                        (Sema.Function_frame_layout.location_checked_type
                           (Integer_globals.static_location slot)) )
                  with
                  | Some span, Ok address_type ->
                      Ok
                        (Some
                           {
                             slot = Integer_globals.static_storage slot;
                             address_type;
                             span;
                           })
                  | _ ->
                      invalid
                        "static identifier has no checked pointer type or \
                         physical span")
              | _ ->
                  invalid
                    "static identifier is absent from its exact persistent \
                     storage context"))
      | _ -> prepare_global ~globals result)
  | _ -> prepare_global ~globals result

let prepare_initializer ~globals root =
  match
    root |> Result.top_level_root_source
    |> Sema.Top_level_expression_tree.root_role
  with
  | Sema.Top_level_expression_tree.Global_initializer owner -> (
      let symbol = Sema.Global_initializer_binding.global_symbol owner in
      match Integer_globals.find globals symbol with
      | Some slot
        when match Integer_globals.slot_initializer slot with
             | Some expected -> expected == root
             | None -> false -> (
          match
            ( Sema.Global_initializer_binding.global_initializer_origin owner,
              Type.pointer_to (Integer_globals.slot_type slot) )
          with
          | Some (Sema.Symbol.Source_location location), Ok address_type ->
              Ok
                {
                  slot = Integer_globals.global_storage slot;
                  address_type;
                  span = location.span;
                }
          | _ ->
              error "HCIRL0004"
                "global initializer has no checked address type or source span")
      | _ ->
          error "HCIRL0004"
            "global initializer destination does not match its storage context")
  | _ ->
      error "HCIRL0004"
        "global initializer destination requires a declaration-owned root"

let prepare_static_initializer ~globals slot root =
  let ( let* ) = Stdlib.Result.bind in
  let location = Integer_globals.static_location slot in
  let symbol = Sema.Function_frame_layout.location_symbol location in
  let invalid message = error "HCIRL0004" message in
  match Integer_globals.find_static globals symbol with
  | Some expected
    when expected == slot
         && Option.fold ~none:false
              ~some:(fun expected -> expected == root)
              (Integer_globals.static_initializer slot) -> (
      let* _ =
        Frame_address_lowering.prepare_initializer
          ~frame:(Integer_globals.static_frame slot)
          root
      in
      match
        ( Result.initializer_source root |> Source.initializer_origin,
          Type.pointer_to
            (Sema.Function_frame_layout.location_checked_type location) )
      with
      | Sema.Symbol.Source_location location, Ok address_type ->
          Ok
            {
              slot = Integer_globals.static_storage slot;
              address_type;
              span = location.span;
            }
      | _ ->
          invalid
            "static initializer has no checked pointer type or physical span")
  | _ ->
      invalid
        "static initializer destination does not match its exact storage \
         context"

let lower_prepared ~instruction_id ~value_id address =
  let instruction = Sequence.Instruction_id.to_int instruction_id
  and value = Sequence.Value_id.to_int value_id in
  if instruction = Int.max_int || value = Int.max_int then
    error ~span:address.span "HCIRL0005"
      "global address identity space is exhausted"
  else
    let ( let* ) = Stdlib.Result.bind in
    let singleton result =
      Stdlib.Result.map_error (fun item -> [ item ]) result
    in
    let* next_instruction_id_ =
      Sequence.Instruction_id.of_int (instruction + 1) |> singleton
    in
    let* next_value_id_ = Sequence.Value_id.of_int (value + 1) |> singleton in
    let* sequence_ =
      Sequence.create
        [
          {
            Sequence.instruction_id;
            opcode = Integer_globals.storage_opcode address.slot;
            operands = [];
            result = Some { value_id };
            target_type = Some address.address_type;
            payload =
              Some
                (Sequence.Symbol (Integer_globals.storage_symbol address.slot));
            flags = 0L;
            span = Some address.span;
          };
        ]
    in
    Ok
      {
        sequence_;
        result_value_ = value_id;
        result_type_ = address.address_type;
        next_instruction_id_;
        next_value_id_;
      }

let sequence lowered = lowered.sequence_
let result_value lowered = lowered.result_value_
let result_type lowered = lowered.result_type_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_
