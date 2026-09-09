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
  initializer_indices : (int64 * int64) list;
  retained : Retained_global.t option;
}

let strides address = Integer_globals.storage_strides address.slot

type t = {
  sequence_ : Sequence.t;
  result_value_ : Sequence.Value_id.t;
  result_type_ : Type.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

let error ?span code message =
  Error [ { Sequence.code; message; instruction_id = None; span } ]

let prepare_retained ~globals result =
  let module Outer = Sema.Outer_expression_binding in
  let source = Result.result_source result in
  let origin = Source.argument_expression_origin source in
  let span =
    match origin with
    | Sema.Symbol.Source_location location -> Some location.span
    | _ -> None
  in
  let invalid message = error ?span "HCIRL0004" message in
  match Result.result_outer_binding result with
  | None -> Ok None
  | Some binding -> (
      let occurrence_matches =
        match
          ( Source.argument_expression_kind source,
            Result.result_outer_occurrence result,
            Result.result_top_level_outer_occurrence result )
        with
        | ( Source.Unresolved_expression Source.Identifier_expression,
            Some occurrence,
            None ) -> (
            Binding.occurrence_resolution (Outer.occurrence_source occurrence)
            = Binding.Outer_candidate
            && Binding.occurrence_origin (Outer.occurrence_source occurrence)
               = origin
            && Outer.occurrence_origin occurrence = origin
            && String.equal
                 (Outer.occurrence_name occurrence)
                 (Sema.Symbol.name
                    (Sema.Outer_environment.entry_symbol
                       (Sema.Outer_environment.binding_entry binding)))
            &&
            match Outer.occurrence_resolution occurrence with
            | Outer.Outer_binding expected -> expected == binding
            | _ -> false)
        | ( Source.Top_level_bound_identifier_expression identifier,
            None,
            Some occurrence ) -> (
            occurrence
            == Source.top_level_bound_identifier_occurrence identifier
            && Top.occurrence_origin occurrence = origin
            &&
            match Top.occurrence_resolution occurrence with
            | Top.Outer_binding expected -> expected == binding
            | _ -> false)
        | _ -> false
      in
      if (not occurrence_matches) || Result.result_origin result <> origin then
        invalid "retained global requires its exact typed outer occurrence"
      else
        match Integer_globals.retained_binding globals binding with
        | None ->
            invalid "outer global is absent from the compiled task storage view"
        | Some (reference, slot) -> (
            let type_ = Integer_globals.storage_type slot in
            let rank = Integer_globals.storage_dimensions slot |> List.length in
            if
              (not
                 (Option.fold ~none:false ~some:(Type.equal type_)
                    (Result.result_type result)))
              || Result.result_array_rank result <> rank
              || Result.result_is_array_address result <> (rank > 0)
              || Option.is_some (Result.result_function_declaration result)
              || Option.is_some (Result.result_function_address_path result)
              || not
                   (match Result.result_category result with
                   | Result.Object_value | Result.Lvalue -> rank = 0
                   | Result.Array_value -> rank > 0
                   | _ -> false)
            then
              invalid
                "retained global type or value shape disagrees with its exact \
                 object"
            else
              match (span, Type.pointer_to type_) with
              | Some span, Ok address_type ->
                  Ok
                    (Some
                       {
                         slot;
                         address_type;
                         span;
                         initializer_indices = [];
                         retained = Some reference;
                       })
              | _ ->
                  invalid
                    "retained global has no checked pointer type or physical \
                     span"))

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
  | None -> prepare_retained ~globals result
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
            let rank =
              Integer_globals.slot_shape slot
              |> Integer_storage_shape.dimensions |> List.length
            in
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
              || Result.result_array_rank result <> rank
              || Result.result_is_array_address result <> (rank > 0)
              || Option.is_some (Result.result_function_declaration result)
              || Option.is_some (Result.result_function_address_path result)
            then
              invalid
                "global identifier type or value shape disagrees with its \
                 object"
            else if
              not
                (match Result.result_category result with
                | Result.Object_value | Result.Lvalue -> rank = 0
                | Result.Array_value -> rank > 0
                | _ -> false)
            then
              invalid
                "global scalar identifier has an inconsistent result category"
            else if
              not
                (match source_type with
                | None -> true
                | Some (source_type, source_rank, shape) ->
                    Type.equal source_type type_
                    && source_rank = rank
                    &&
                    if rank = 0 then shape = Source.Object_value
                    else shape = Source.Array_value)
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
                         initializer_indices = [];
                         retained = None;
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
                             initializer_indices = [];
                             retained = None;
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

let initializer_indices ~slot ~roots ~arrays root =
  let invalid message = error "HCIRL0004" message in
  if not (List.exists (fun expected -> expected == root) roots) then
    invalid "initializer root is absent from its exact storage context"
  else
    match arrays with
    | None ->
        if Integer_globals.storage_dimensions slot = [] then Ok []
        else invalid "array initializer has no checked destination layout"
    | Some arrays -> (
        match Integer_array_initializers.find arrays root with
        | None -> invalid "array initializer has no exact destination leaf"
        | Some entry -> (
            let destination = Integer_array_initializers.destination entry in
            match Integer_initializer_layout.operation destination with
            | Integer_initializer_layout.Copy_bytes _ ->
                invalid
                  "array string copies cannot be lowered as scheduled scalar \
                   stores"
            | Integer_initializer_layout.Scalar_store -> (
                let cell = Integer_initializer_layout.cell_offset destination in
                let bytes =
                  Integer_initializer_layout.byte_offset destination
                in
                match
                  Integer_scalar_storage.public_byte_size
                    (Integer_globals.storage_type slot)
                with
                | Some width
                  when cell >= 0
                       && cell < Integer_globals.storage_element_count slot
                       && bytes >= 0
                       && bytes mod width = 0
                       && bytes / width = cell ->
                    let rec coordinates offset reversed dimensions strides =
                      match (dimensions, strides) with
                      | [], [] when offset = 0L -> Ok (List.rev reversed)
                      | count :: dimensions, stride :: strides
                        when count > 0L && stride > 0L ->
                          let index = Int64.div offset stride in
                          if index >= count then
                            invalid
                              "array initializer destination exceeds its \
                               checked extent"
                          else
                            coordinates (Int64.rem offset stride)
                              ((stride, index) :: reversed)
                              dimensions strides
                      | _ ->
                          invalid
                            "array initializer destination has inconsistent \
                             dimensions or strides"
                    in
                    if Integer_globals.storage_dimensions slot = [] then
                      invalid
                        "array initializer destination has no declared rank"
                    else
                      coordinates (Int64.of_int bytes) []
                        (Integer_globals.storage_dimensions slot)
                        (Integer_globals.storage_strides slot)
                | _ ->
                    invalid
                      "array initializer cell and byte destinations disagree")))

let prepare_initializer ~globals root =
  let ( let* ) = Stdlib.Result.bind in
  match
    root |> Result.top_level_root_source
    |> Sema.Top_level_expression_tree.root_role
  with
  | Sema.Top_level_expression_tree.Global_initializer owner -> (
      let symbol = Sema.Global_initializer_binding.global_symbol owner in
      match Integer_globals.find globals symbol with
      | Some slot -> (
          let storage = Integer_globals.global_storage slot in
          let* initializer_indices =
            initializer_indices ~slot:storage
              ~roots:(Integer_globals.slot_initializers slot)
              ~arrays:(Integer_globals.slot_array_initializers slot)
              root
          in
          match
            ( Sema.Global_initializer_binding.global_initializer_origin owner,
              Type.pointer_to (Integer_globals.slot_type slot) )
          with
          | Some (Sema.Symbol.Source_location location), Ok address_type ->
              Ok
                {
                  slot = storage;
                  address_type;
                  span = location.span;
                  initializer_indices;
                  retained = None;
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
  | Some expected when expected == slot -> (
      let storage = Integer_globals.static_storage slot in
      let* initializer_indices =
        initializer_indices ~slot:storage
          ~roots:(Integer_globals.static_initializers slot)
          ~arrays:(Integer_globals.static_array_initializers slot)
          root
      in
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
              slot = storage;
              address_type;
              span = location.span;
              initializer_indices;
              retained = None;
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
  let rank = List.length address.initializer_indices in
  let exhausted () =
    error ~span:address.span "HCIRL0005"
      "global address identity space is exhausted"
  in
  if rank > (Int.max_int - 1) / 4 then exhausted ()
  else
    let length = 1 + (4 * rank) in
    if instruction > Int.max_int - length || value > Int.max_int - length then
      exhausted ()
    else
      let ( let* ) = Stdlib.Result.bind in
      let singleton result =
        Stdlib.Result.map_error (fun item -> [ item ]) result
      in
      let instruction_cursor = ref instruction and value_cursor = ref value in
      let descriptions = ref [] in
      let emit ~opcode ~operands ~target_type ~payload =
        let* instruction_id =
          Sequence.Instruction_id.of_int !instruction_cursor |> singleton
        in
        let* value_id = Sequence.Value_id.of_int !value_cursor |> singleton in
        incr instruction_cursor;
        incr value_cursor;
        descriptions :=
          {
            Sequence.instruction_id;
            opcode;
            operands;
            result = Some { value_id };
            target_type = Some target_type;
            payload;
            flags = 0L;
            span = Some address.span;
          }
          :: !descriptions;
        Ok value_id
      in
      let* base =
        emit
          ~opcode:(Integer_globals.storage_opcode address.slot)
          ~operands:[] ~target_type:address.address_type
          ~payload:
            (Some
               (match address.retained with
               | Some reference -> Sequence.Retained_global reference
               | None ->
                   Sequence.Symbol (Integer_globals.storage_symbol address.slot)))
      in
      let rec indexed base = function
        | [] -> Ok base
        | (stride, coordinate) :: rest ->
            let* index_type =
              Type.make_primitive ~form:Type.Internal_storage
                ~primitive:Sema.Primitive_type.I64 ~pointer_depth:0
              |> Stdlib.Result.map_error (fun message ->
                  [
                    {
                      Sequence.code = "HCIRL0004";
                      message;
                      instruction_id = None;
                      span = Some address.span;
                    };
                  ])
            in
            let* stride_value =
              emit ~opcode:Opcode.Ic_imm_i64 ~operands:[]
                ~target_type:address.address_type
                ~payload:(Some (Sequence.Integer stride))
            in
            let* index_value =
              emit ~opcode:Opcode.Ic_imm_i64 ~operands:[]
                ~target_type:index_type
                ~payload:(Some (Sequence.Integer coordinate))
            in
            let* scaled =
              emit ~opcode:Opcode.Ic_mul
                ~operands:[ stride_value; index_value ]
                ~target_type:address.address_type ~payload:None
            in
            let* result =
              emit ~opcode:Opcode.Ic_add ~operands:[ base; scaled ]
                ~target_type:address.address_type ~payload:None
            in
            indexed result rest
      in
      let* result_value_ = indexed base address.initializer_indices in
      let* next_instruction_id_ =
        Sequence.Instruction_id.of_int !instruction_cursor |> singleton
      in
      let* next_value_id_ =
        Sequence.Value_id.of_int !value_cursor |> singleton
      in
      let* sequence_ = Sequence.create (List.rev !descriptions) in
      Ok
        {
          sequence_;
          result_value_;
          result_type_ = address.address_type;
          next_instruction_id_;
          next_value_id_;
        }

let sequence lowered = lowered.sequence_
let result_value lowered = lowered.result_value_
let result_type lowered = lowered.result_type_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_
