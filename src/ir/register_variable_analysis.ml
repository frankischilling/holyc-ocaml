module Frame = Sema.Function_frame_layout
module Request = Sema.Register_request
module Seq = Instruction_sequence
module Values = Map.Make (Seq.Value_id)
module O = Opcode

type selection = Register_candidate | Stack | Unknown
type snapshot = Seq.Block_id.t * Seq.description list * Seq.Block_id.t list
type payload_stamp = No_payload | Integer of int64 | Block of int

type instruction_stamp = {
  id : int;
  opcode : O.t;
  operands : int list;
  result : int option;
  target : (Sema.Type.primitive_form * Sema.Primitive_type.t * int) option;
  payload : payload_stamp;
  flags : int64;
  span : (int * int * int) option;
}

let equal_stamp a b =
  a.id = b.id && a.opcode = b.opcode && a.operands = b.operands
  && a.result = b.result && a.target = b.target && a.payload = b.payload
  && a.flags = b.flags && a.span = b.span

let instruction_stamp (item : Seq.description) =
  let target =
    match item.target_type with
    | None -> Some None
    | Some type_ -> (
        match Sema.Type.base type_ with
        | Sema.Type.Primitive (form, primitive) ->
            Some (Some (form, primitive, Sema.Type.pointer_depth type_))
        | Sema.Type.Aggregate _ -> None)
  in
  let payload =
    match item.payload with
    | None -> Some No_payload
    | Some (Seq.Integer value) -> Some (Integer value)
    | Some (Seq.Block block) -> Some (Block (Seq.Block_id.to_int block))
    | _ -> None
  in
  Option.bind target (fun target ->
      Option.map
        (fun payload ->
          {
            id = Seq.Instruction_id.to_int item.instruction_id;
            opcode = item.opcode;
            operands = List.map Seq.Value_id.to_int item.operands;
            result =
              Option.map
                (fun (result : Seq.value_definition) ->
                  Seq.Value_id.to_int result.value_id)
                item.result;
            target;
            payload;
            flags = item.flags;
            span =
              Option.map
                (fun (span : Common.Span.t) ->
                  (Common.Source_id.to_int span.source, span.start, span.stop))
                item.span;
          })
        payload)

type location_stamp = {
  location : Frame.location;
  symbol : Sema.Symbol.t;
  binding : Sema.Function_binding_index.binding;
  kind : Frame.location_kind;
  request : Request.selection;
  value_shape : Frame.value_shape;
  checked_type : Sema.Type.primitive_form * Sema.Primitive_type.t * int;
  slot : (Frame.frame_slot * int64 * int64) option;
  allocated_size : int64;
}

let location_stamp location =
  match Sema.Type.base (Frame.location_checked_type location) with
  | Sema.Type.Aggregate _ -> None
  | Sema.Type.Primitive (form, primitive) ->
      Some
        {
          location;
          symbol = Frame.location_symbol location;
          binding = Frame.location_binding location;
          kind = Frame.location_kind location;
          request = Frame.location_register_selection location;
          value_shape = Frame.location_value_shape location;
          checked_type =
            ( form,
              primitive,
              Sema.Type.pointer_depth (Frame.location_checked_type location) );
          slot =
            Option.map
              (fun slot ->
                ( slot,
                  Frame.frame_slot_displacement slot,
                  Frame.frame_slot_size slot ))
              (Frame.location_frame_slot location);
          allocated_size = Frame.location_allocated_size location;
        }

let equal_location a b =
  a.location == b.location && a.symbol == b.symbol && a.binding == b.binding
  && a.kind = b.kind && a.request = b.request
  && a.value_shape = b.value_shape
  && a.checked_type = b.checked_type
  && a.allocated_size = b.allocated_size
  && Option.equal
       (fun (slot, offset, size) (other_slot, other_offset, other_size) ->
         slot == other_slot && offset = other_offset && size = other_size)
       a.slot b.slot

type t = {
  frame_ : Frame.function_layout;
  body_ : Function_body.t;
  definition_ : Sema.Function_resolution.resolved_declaration option;
  source_ : Sema.Function_call_expression_result.resolved_function option;
  options_ : int64;
  flags_ : int64;
  locations_ : location_stamp list;
  blocks_ : snapshot list;
  stamps_ : (Seq.description * instruction_stamp) list;
  selections_ : (Sema.Symbol.t * selection) list;
}

let descriptions block =
  Block_graph.instructions block |> Seq.instructions |> List.map Seq.description

let snapshot body =
  Function_body.body body |> Block_graph.blocks
  |> List.map (fun block ->
      ( Block_graph.block_id block,
        descriptions block,
        List.map Fun.id (Block_graph.successors block) ))

let same_list equal a b =
  List.length a = List.length b && List.for_all2 equal a b

let matches_blocks expected actual =
  same_list
    (fun (id, instructions, successors)
         (other_id, other_instructions, other_successors) ->
      Seq.Block_id.equal id other_id
      && same_list ( == ) instructions other_instructions
      && same_list Seq.Block_id.equal successors other_successors)
    expected actual

(* Explicit register variables are observable through their values only in the
   bounded, call-free graphs below. Keep conflicting or scratch-register requests
   outside the proof. *)
let safe_register_requests locations =
  let rec check used = function
    | [] -> true
    | location :: rest -> (
        match Frame.location_register_selection location with
        | Request.Explicit register ->
            let number = Request.explicit_register_number register in
            number >= 12 && number <= 15
            && (not (List.mem number used))
            && check (number :: used) rest
        | Unspecified | Allocatable | Disabled -> check used rest)
  in
  check [] locations

let analyze_parameter ~frame ~body =
  let locations = Frame.function_locations frame in
  let ordinary =
    Option.is_some (Function_body.definition_declaration body)
    && Function_body.definition_matches_frame body frame
    && Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       = 0L
    && Function_body.compiler_options body = Sema.Compiler_option.initial_mask
    && List.length locations >= 2
    && List.length locations <= 6
    && safe_register_requests locations
    && List.for_all
         (fun location ->
           Frame.location_kind location = Frame.Named_parameter
           && Frame.location_value_shape location = Frame.Scalar
           && Option.is_some
                (Integer_scalar_storage.of_type
                   (Frame.location_checked_type location))
           && Option.is_some (Frame.location_frame_slot location))
         locations
  in
  if not ordinary then None
  else
    let blocks = snapshot body in
    let code =
      List.concat_map (fun (_, instructions, _) -> instructions) blocks
    in
    let allowed (item : Seq.description) =
      let flags =
        if item.opcode = O.Ic_end_exp then item.flags = 0x200L
        else item.flags = 0L
      in
      flags
      && Option.is_some (instruction_stamp item)
      &&
      match item.opcode with
      | O.Ic_rbp
      | Ic_imm_i64
      | Ic_add
      | Ic_deref
      | Ic_shl_equ
      | Ic_shr_equ
      | Ic_add_equ
      | Ic_sub_equ
      | Ic_mul_equ
      | Ic_div_equ
      | Ic_and_equ
      | Ic_or_equ
      | Ic_xor_equ
      | Ic_end_exp
      | Ic_return_val
      | Ic_jmp
      | Ic_ret -> true
      | _ -> false
    in
    if List.length code > 96 || not (List.for_all allowed code) then None
    else
      let produced =
        List.fold_left
          (fun map (item : Seq.description) ->
            match item.result with
            | None -> map
            | Some result -> Values.add result.value_id item map)
          Values.empty code
      in
      let users id =
        List.filter
          (fun (item : Seq.description) ->
            List.exists (Seq.Value_id.equal id) item.operands)
          code
      in
      let location_at offset =
        List.find_opt
          (fun location ->
            Option.fold ~none:false
              ~some:(fun slot -> Frame.frame_slot_displacement slot = offset)
              (Frame.location_frame_slot location))
          locations
      in
      let frame_address (item : Seq.description) =
        match (item.opcode, item.operands, item.result) with
        | O.Ic_add, [ left; right ], Some result ->
            let is_rbp id =
              Option.fold ~none:false
                ~some:(fun (d : Seq.description) -> d.opcode = O.Ic_rbp)
                (Values.find_opt id produced)
            in
            let offset id =
              Option.bind (Values.find_opt id produced)
                (fun (d : Seq.description) ->
                  match (d.opcode, d.payload) with
                  | O.Ic_imm_i64, Some (Seq.Integer n) -> Some n
                  | _ -> None)
            in
            let value =
              if is_rbp left then offset right
              else if is_rbp right then offset left
              else None
            in
            Option.bind value (fun n ->
                Option.bind (location_at n) (fun location ->
                    Option.bind item.target_type (fun pointer ->
                        match Sema.Type.dereference pointer with
                        | Ok actual
                          when Sema.Type.equal actual
                                 (Frame.location_checked_type location) ->
                            Some (result.value_id, location)
                        | _ -> None)))
        | _ -> None
      in
      let additions =
        List.filter (fun (d : Seq.description) -> d.opcode = O.Ic_add) code
      in
      let addresses = List.filter_map frame_address additions in
      let arithmetic_additions =
        List.filter (fun item -> Option.is_none (frame_address item)) additions
      in
      let safe_use id location (item : Seq.description) =
        let same_type =
          Option.fold ~none:false
            ~some:(Sema.Type.equal (Frame.location_checked_type location))
            item.target_type
        in
        same_type
        &&
        match (item.opcode, item.operands) with
        | O.Ic_deref, [ target ] -> Seq.Value_id.equal target id
        | ( ( Ic_shl_equ
            | Ic_shr_equ
            | Ic_add_equ
            | Ic_sub_equ
            | Ic_mul_equ
            | Ic_div_equ
            | Ic_and_equ
            | Ic_or_equ
            | Ic_xor_equ ),
            [ target; _ ] ) -> Seq.Value_id.equal target id
        | _ -> false
      in
      let addresses_safe =
        List.for_all
          (fun (id, location) ->
            let consumers = users id in
            consumers <> [] && List.for_all (safe_use id location) consumers)
          addresses
      in
      let rbp_safe =
        List.for_all
          (fun (item : Seq.description) ->
            if item.opcode <> O.Ic_rbp then true
            else
              match item.result with
              | None -> false
              | Some result ->
                  let consumers = users result.value_id in
                  consumers <> []
                  && List.for_all
                       (fun (d : Seq.description) ->
                         d.opcode = O.Ic_add && Option.is_some (frame_address d))
                       consumers)
          code
      in
      let dereferences_safe =
        List.for_all
          (fun (item : Seq.description) ->
            match (item.opcode, item.operands) with
            | O.Ic_deref, [ target ]
            | ( ( Ic_shl_equ
                | Ic_shr_equ
                | Ic_add_equ
                | Ic_sub_equ
                | Ic_mul_equ
                | Ic_div_equ
                | Ic_and_equ
                | Ic_or_equ
                | Ic_xor_equ ),
                [ target; _ ] ) ->
                List.exists
                  (fun (id, _) -> Seq.Value_id.equal id target)
                  addresses
            | _ -> true)
          code
      in
      (* The checked pattern lives wholly in the entry block. A later or
         unreachable update must not be mistaken for the source optimizer's
         live register-variable use. *)
      let entry_id =
        Block_graph.block_id (Block_graph.entry (Function_body.body body))
      in
      let simple_flow =
        List.for_all
          (fun (id, instructions, _) ->
            Seq.Block_id.equal id entry_id
            || List.for_all
                 (fun (item : Seq.description) ->
                   item.opcode = O.Ic_jmp || item.opcode = O.Ic_ret)
                 instructions)
          blocks
      in
      let matching_target =
        let address_location id =
          List.find_map
            (fun (original, location) ->
              if Seq.Value_id.equal original id then Some location else None)
            addresses
        in
        let loaded_location id =
          Option.bind (Values.find_opt id produced)
            (fun (item : Seq.description) ->
              match (item.opcode, item.operands) with
              | O.Ic_deref, [ address ] -> address_location address
              | _ -> None)
        in
        let updates =
          List.filter
            (fun (item : Seq.description) ->
              item.opcode = O.Ic_shl_equ || item.opcode = O.Ic_shr_equ
              || item.opcode = O.Ic_add_equ || item.opcode = O.Ic_sub_equ
              || item.opcode = O.Ic_mul_equ || item.opcode = O.Ic_div_equ
              || item.opcode = O.Ic_and_equ || item.opcode = O.Ic_or_equ
              || item.opcode = O.Ic_xor_equ)
            code
        in
        let returns =
          List.filter
            (fun (item : Seq.description) -> item.opcode = O.Ic_return_val)
            code
        in
        match (updates, returns) with
        | [ update ], [ returned ] -> (
            match (update.operands, returned.operands) with
            | [ target; count ], [ result ] -> (
                match
                  ( address_location target,
                    loaded_location count,
                    loaded_location result )
                with
                | ( Some target_location,
                    Some count_location,
                    Some return_location )
                  when target_location == return_location
                       && target_location != count_location
                       && Seq.Instruction_id.compare update.instruction_id
                            returned.instruction_id
                          < 0 -> (
                    match
                      Sema.Type.base
                        (Frame.location_checked_type target_location)
                    with
                    | Sema.Type.Primitive
                        ( _,
                          (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32)
                        ) -> Some (target_location, count_location)
                    | _ -> None)
                | _ -> None)
            | _ -> None)
        | _ -> None
      in
      (* For two updates, authenticate the only non-address addition as the
         return sum and tie each operand to its own frame location. *)
      let matching_pair =
        let integer_shape type_ =
          if Sema.Type.pointer_depth type_ <> 0 then None
          else
            match Sema.Type.base type_ with
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I8 | U8)) ->
                Some (form, 1)
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I16 | U16)) ->
                Some (form, 2)
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I32 | U32)) ->
                Some (form, 4)
            | Sema.Type.Primitive (form, (Sema.Primitive_type.I64 | U64)) ->
                Some (form, 8)
            | _ -> None
        in
        let compatible_sum first second result =
          match
            (integer_shape first, integer_shape second, integer_shape result)
          with
          | ( Some (first_form, first_width),
              Some (second_form, second_width),
              Some (result_form, result_width) ) ->
              first_form = second_form && first_form = result_form
              && min first_width second_width < 8
              && result_width = max first_width second_width
          | _ -> false
        in
        let supported_update = function
          | O.Ic_add_equ
          | O.Ic_sub_equ
          | O.Ic_mul_equ
          | O.Ic_div_equ
          | O.Ic_shl_equ
          | O.Ic_shr_equ
          | O.Ic_and_equ
          | O.Ic_or_equ
          | O.Ic_xor_equ -> true
          | _ -> false
        in
        let address_location id =
          List.find_map
            (fun (original, location) ->
              if Seq.Value_id.equal original id then Some location else None)
            addresses
        in
        let loaded_location id =
          Option.bind (Values.find_opt id produced)
            (fun (item : Seq.description) ->
              match (item.opcode, item.operands) with
              | O.Ic_deref, [ address ] -> address_location address
              | _ -> None)
        in
        let updates =
          List.filter
            (fun (item : Seq.description) ->
              item.opcode = O.Ic_add_equ || item.opcode = O.Ic_sub_equ
              || item.opcode = O.Ic_mul_equ || item.opcode = O.Ic_div_equ
              || item.opcode = O.Ic_shl_equ || item.opcode = O.Ic_shr_equ
              || item.opcode = O.Ic_and_equ || item.opcode = O.Ic_or_equ
              || item.opcode = O.Ic_xor_equ)
            code
        in
        let returns =
          List.filter
            (fun (item : Seq.description) -> item.opcode = O.Ic_return_val)
            code
        in
        match (updates, returns, arithmetic_additions) with
        | [ first; second ], [ returned ], [ sum ]
          when supported_update first.opcode
               && supported_update second.opcode
               && first.payload = None && second.payload = None
               && sum.payload = None && returned.payload = None
               && Seq.Instruction_id.compare first.instruction_id
                    second.instruction_id
                  < 0
               && Seq.Instruction_id.compare second.instruction_id
                    sum.instruction_id
                  < 0
               && Seq.Instruction_id.compare sum.instruction_id
                    returned.instruction_id
                  < 0 -> (
            match
              ( first.operands,
                second.operands,
                sum.operands,
                sum.result,
                returned.operands )
            with
            | ( [ first_address; first_count ],
                [ second_address; second_count ],
                [ left; right ],
                Some sum_result,
                [ returned_value ] )
              when Seq.Value_id.equal sum_result.value_id returned_value
                   && List.for_all
                        (fun value ->
                          Option.fold ~none:false
                            ~some:(fun (item : Seq.description) ->
                              Seq.Instruction_id.compare second.instruction_id
                                item.instruction_id
                              < 0)
                            (Values.find_opt value produced))
                        [ left; right ] -> (
                match
                  ( address_location first_address,
                    address_location second_address,
                    loaded_location first_count,
                    loaded_location second_count,
                    loaded_location left,
                    loaded_location right,
                    sum.target_type )
                with
                | ( Some first_target,
                    Some second_target,
                    Some count,
                    Some other_count,
                    Some left_target,
                    Some right_target,
                    Some sum_type )
                  when first_target != second_target
                       && first_target != count && second_target != count
                       && first_target != other_count
                       && second_target != other_count
                       && left_target == first_target
                       && right_target == second_target
                       && compatible_sum
                            (Frame.location_checked_type first_target)
                            (Frame.location_checked_type second_target)
                            sum_type ->
                    Some
                      ( first_target,
                        second_target,
                        count,
                        other_count,
                        sum_type,
                        first.opcode,
                        second.opcode )
                | _ -> None)
            | _ -> None)
        | _ -> None
      in
      let source_matches (target, count_location) =
        let module Typed = Sema.Function_call_expression_result in
        let module Calls = Sema.Function_call_resolution in
        let module Bind = Sema.Module_expression_binding in
        let binding_of value =
          match Calls.argument_expression_kind (Typed.result_source value) with
          | Calls.Bound_identifier_expression bound -> (
              match
                Bind.occurrence_resolution
                  (Calls.bound_identifier_occurrence bound)
              with
              | Bind.Local_binding binding -> Some binding
              | _ -> None)
          | _ -> None
        in
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ statement ], [ returned ] -> (
                let value = Typed.expression_statement_value statement in
                match
                  ( Calls.argument_expression_source (Typed.result_source value),
                    Typed.result_binary_operands value,
                    Typed.return_value returned,
                    List.filter
                      (fun (item : Seq.description) ->
                        item.opcode = O.Ic_shl_equ || item.opcode = O.Ic_shr_equ
                        || item.opcode = O.Ic_add_equ
                        || item.opcode = O.Ic_sub_equ
                        || item.opcode = O.Ic_mul_equ
                        || item.opcode = O.Ic_div_equ
                        || item.opcode = O.Ic_and_equ
                        || item.opcode = O.Ic_or_equ
                        || item.opcode = O.Ic_xor_equ)
                      code )
                with
                | ( Some (Frontend.Ast.Binary_expression binary),
                    Some (left, right),
                    Some return_value,
                    [ update ] ) ->
                    let opcode_matches =
                      match
                        (binary.binary_operator_spec.spelling, update.opcode)
                      with
                      | "<<=", O.Ic_shl_equ
                      | ">>=", Ic_shr_equ
                      | "+=", Ic_add_equ
                      | "-=", Ic_sub_equ
                      | "*=", Ic_mul_equ
                      | "/=", Ic_div_equ
                      | "&=", Ic_and_equ
                      | "|=", Ic_or_equ
                      | "^=", Ic_xor_equ -> true
                      | _ -> false
                    in
                    let target_binding = Frame.location_binding target in
                    opcode_matches
                    && Option.fold ~none:false ~some:(( == ) target_binding)
                         (binding_of left)
                    && Option.fold ~none:false ~some:(( == ) target_binding)
                         (binding_of return_value)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding count_location))
                         (binding_of right)
                | _ -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let pair_source_matches
          ( first_target,
            second_target,
            first_count,
            second_count,
            sum_type,
            first_opcode,
            second_opcode ) =
        let module Typed = Sema.Function_call_expression_result in
        let module Calls = Sema.Function_call_resolution in
        let module Bind = Sema.Module_expression_binding in
        let binding_of value =
          match Calls.argument_expression_kind (Typed.result_source value) with
          | Calls.Bound_identifier_expression bound -> (
              match
                Bind.occurrence_resolution
                  (Calls.bound_identifier_occurrence bound)
              with
              | Bind.Local_binding binding -> Some binding
              | _ -> None)
          | _ -> None
        in
        let matches_binding location value =
          Option.fold ~none:false
            ~some:(( == ) (Frame.location_binding location))
            (binding_of value)
        in
        let update_matches target count opcode statement =
          let value = Typed.expression_statement_value statement in
          match
            ( Calls.argument_expression_source (Typed.result_source value),
              Typed.result_binary_operands value )
          with
          | Some (Frontend.Ast.Binary_expression binary), Some (left, right) ->
              (match (binary.binary_operator_spec.spelling, opcode) with
                | "+=", O.Ic_add_equ
                | "-=", O.Ic_sub_equ
                | "*=", O.Ic_mul_equ
                | "/=", O.Ic_div_equ
                | "<<=", O.Ic_shl_equ
                | ">>=", O.Ic_shr_equ
                | "&=", O.Ic_and_equ
                | "|=", O.Ic_or_equ
                | "^=", O.Ic_xor_equ -> true
                | _ -> false)
              && matches_binding target left
              && matches_binding count right
          | _ -> false
        in
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ first; second ], [ returned ] -> (
                match Typed.return_value returned with
                | Some value -> (
                    match
                      ( Calls.argument_expression_source
                          (Typed.result_source value),
                        Typed.result_binary_operands value )
                    with
                    | ( Some (Frontend.Ast.Binary_expression binary),
                        Some (left, right) ) ->
                        binary.binary_operator_spec.spelling = "+"
                        && Option.fold ~none:false
                             ~some:(Sema.Type.equal sum_type)
                             (Typed.result_type value)
                        && update_matches first_target first_count first_opcode
                             first
                        && update_matches second_target second_count
                             second_opcode second
                        && matches_binding first_target left
                        && matches_binding second_target right
                    | _ -> false)
                | None -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      (* Match straight-line updates only when every distinct target appears in
         order in the returned addition tree. *)
      let matching_sequence =
        let address_location id =
          List.find_map
            (fun (original, location) ->
              if Seq.Value_id.equal original id then Some location else None)
            addresses
        in
        let loaded_location id =
          Option.bind (Values.find_opt id produced)
            (fun (item : Seq.description) ->
              match (item.opcode, item.operands) with
              | O.Ic_deref, [ address ] -> address_location address
              | _ -> None)
        in
        let updates =
          List.filter
            (fun (item : Seq.description) ->
              match item.opcode with
              | O.Ic_shl_equ
              | Ic_shr_equ
              | Ic_add_equ
              | Ic_sub_equ
              | Ic_mul_equ
              | Ic_div_equ
              | Ic_and_equ
              | Ic_or_equ
              | Ic_xor_equ -> true
              | _ -> false)
            code
        in
        let returns =
          List.filter
            (fun (item : Seq.description) -> item.opcode = O.Ic_return_val)
            code
        in
        let count = List.length updates in
        let rec ordered = function
          | first :: (second :: _ as rest) ->
              Seq.Instruction_id.compare first.Seq.instruction_id
                second.Seq.instruction_id
              < 0
              && ordered rest
          | _ -> true
        in
        match (updates, returns) with
        | (_ :: _ as updates), [ returned ]
          when count >= 3 && count <= 5
               && List.length arithmetic_additions = count - 1
               && List.for_all
                    (fun (item : Seq.description) -> item.payload = None)
                    updates
               && returned.payload = None && ordered updates -> (
            let last = List.hd (List.rev updates) in
            let mapped =
              List.map
                (fun (item : Seq.description) ->
                  match item.operands with
                  | [ address; operand ] ->
                      Option.bind (address_location address) (fun target ->
                          Option.map
                            (fun operand -> (target, operand))
                            (loaded_location operand))
                  | _ -> None)
                updates
            in
            if List.exists Option.is_none mapped then None
            else
              let pairs = List.map Option.get mapped in
              let targets = List.map fst pairs in
              let operands = List.map snd pairs in
              let integer_shape type_ =
                if Sema.Type.pointer_depth type_ <> 0 then None
                else
                  match Sema.Type.base type_ with
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I8 | U8)) ->
                      Some (form, 1)
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I16 | U16))
                    -> Some (form, 2)
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I32 | U32))
                    -> Some (form, 4)
                  | Sema.Type.Primitive (form, (Sema.Primitive_type.I64 | U64))
                    -> Some (form, 8)
                  | _ -> None
              in
              let compatible_sum left right result =
                match
                  (integer_shape left, integer_shape right, integer_shape result)
                with
                | Some (lf, lw), Some (rf, rw), Some (sf, sw) ->
                    lf = rf && lf = sf && sw = max lw rw
                | _ -> false
              in
              let supported_targets =
                List.for_all
                  (fun target ->
                    Option.is_some
                      (integer_shape (Frame.location_checked_type target)))
                  targets
                && List.exists
                     (fun target ->
                       Option.fold ~none:false
                         ~some:(fun (_, width) -> width < 8)
                         (integer_shape (Frame.location_checked_type target)))
                     targets
              in
              let unique_targets =
                List.length
                  (List.sort_uniq Int.compare
                     (List.map
                        (fun location ->
                          Sema.Symbol.id (Frame.location_symbol location)
                          |> Sema.Symbol.Id.to_int)
                        targets))
                = count
              in
              if
                (not supported_targets) || (not unique_targets)
                || List.exists
                     (fun target -> List.exists (( == ) target) operands)
                     targets
                || Seq.Instruction_id.compare last.instruction_id
                     returned.instruction_id
                   >= 0
              then None
              else
                let rec collect seen value =
                  if List.exists (Seq.Value_id.equal value) seen then None
                  else
                    Option.bind (Values.find_opt value produced)
                      (fun (item : Seq.description) ->
                        if
                          Seq.Instruction_id.compare last.instruction_id
                            item.instruction_id
                          >= 0
                          || Seq.Instruction_id.compare item.instruction_id
                               returned.instruction_id
                             >= 0
                        then None
                        else
                          match (item.opcode, item.operands) with
                          | O.Ic_deref, [ address ] ->
                              Option.map
                                (fun location ->
                                  ( [ location ],
                                    [],
                                    Frame.location_checked_type location ))
                                (address_location address)
                          | O.Ic_add, [ left; right ] when item.payload = None
                            ->
                              Option.bind
                                (collect (value :: seen) left)
                                (fun (left_locations, left_sums, left_type) ->
                                  Option.bind
                                    (collect (value :: seen) right)
                                    (fun ( right_locations,
                                           right_sums,
                                           right_type )
                                       ->
                                      Option.bind item.target_type
                                        (fun result_type ->
                                          if
                                            compatible_sum left_type right_type
                                              result_type
                                          then
                                            Some
                                              ( left_locations @ right_locations,
                                                item :: (left_sums @ right_sums),
                                                result_type )
                                          else None)))
                          | _ -> None)
                in
                match returned.operands with
                | [ root ] -> (
                    match collect [] root with
                    | Some (leaves, sums, _)
                      when same_list ( == ) leaves targets
                           && List.length sums
                              = List.length arithmetic_additions
                           && List.length
                                (List.sort_uniq Int.compare
                                   (List.map
                                      (fun (item : Seq.description) ->
                                        Seq.Instruction_id.to_int
                                          item.instruction_id)
                                      sums))
                              = List.length sums
                           && List.for_all
                                (fun sum ->
                                  List.exists (( == ) sum) arithmetic_additions)
                                sums ->
                        Some
                          ( targets,
                            operands,
                            root,
                            List.map
                              (fun (item : Seq.description) -> item.opcode)
                              updates )
                    | _ -> None)
                | _ -> None)
        | _ -> None
      in
      let sequence_source_matches (targets, operands, root, opcodes) =
        let module Typed = Sema.Function_call_expression_result in
        let module Calls = Sema.Function_call_resolution in
        let module Bind = Sema.Module_expression_binding in
        let binding_of value =
          match Calls.argument_expression_kind (Typed.result_source value) with
          | Calls.Bound_identifier_expression bound -> (
              match
                Bind.occurrence_resolution
                  (Calls.bound_identifier_occurrence bound)
              with
              | Bind.Local_binding binding -> Some binding
              | _ -> None)
          | _ -> None
        in
        let matches_binding location value =
          Option.fold ~none:false
            ~some:(( == ) (Frame.location_binding location))
            (binding_of value)
        in
        let update_matches (target, (operand, opcode)) statement =
          let value = Typed.expression_statement_value statement in
          match
            ( Calls.argument_expression_source (Typed.result_source value),
              Typed.result_binary_operands value )
          with
          | Some (Frontend.Ast.Binary_expression binary), Some (left, right) ->
              (match (binary.binary_operator_spec.spelling, opcode) with
                | "+=", O.Ic_add_equ
                | "-=", O.Ic_sub_equ
                | "*=", O.Ic_mul_equ
                | "/=", O.Ic_div_equ
                | "<<=", O.Ic_shl_equ
                | ">>=", O.Ic_shr_equ
                | "&=", O.Ic_and_equ
                | "|=", O.Ic_or_equ
                | "^=", O.Ic_xor_equ -> true
                | _ -> false)
              && matches_binding target left
              && matches_binding operand right
          | _ -> false
        in
        let rec result_matches typed value =
          match Calls.argument_expression_kind (Typed.result_source typed) with
          | Calls.Parenthesized_expression source -> (
              match Typed.result_operand typed with
              | Some operand
                when Typed.result_source operand == source
                     && Typed.result_intrinsic_conversion typed
                        = Typed.No_intrinsic_conversion
                     && Option.equal Sema.Type.equal (Typed.result_type typed)
                          (Typed.result_type operand) ->
                  result_matches operand value
              | _ -> false)
          | _ -> (
              match Values.find_opt value produced with
              | None -> false
              | Some (item : Seq.description) -> (
                  match (item.opcode, item.operands) with
                  | O.Ic_deref, [ address ] ->
                      Option.fold ~none:false
                        ~some:(fun location -> matches_binding location typed)
                        (List.find_map
                           (fun (id, location) ->
                             if Seq.Value_id.equal id address then Some location
                             else None)
                           addresses)
                  | O.Ic_add, [ left; right ] -> (
                      match
                        ( Calls.argument_expression_source
                            (Typed.result_source typed),
                          Typed.result_binary_operands typed )
                      with
                      | ( Some (Frontend.Ast.Binary_expression binary),
                          Some (typed_left, typed_right) ) ->
                          binary.binary_operator_spec.spelling = "+"
                          && Option.equal Sema.Type.equal
                               (Typed.result_type typed) item.target_type
                          && result_matches typed_left left
                          && result_matches typed_right right
                      | _ -> false)
                  | _ -> false))
        in
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | statements, [ returned ]
              when List.length statements = List.length targets ->
                List.for_all2 update_matches
                  (List.combine targets (List.combine operands opcodes))
                  statements
                && Option.fold ~none:false
                     ~some:(fun value -> result_matches value root)
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let selected_targets =
        if arithmetic_additions = [] then
          Option.bind matching_target (fun (target, count) ->
              if source_matches (target, count) then Some [ target ] else None)
        else
          match matching_pair with
          | Some
              ( first,
                second,
                first_count,
                second_count,
                sum_type,
                first_opcode,
                second_opcode )
            when pair_source_matches
                   ( first,
                     second,
                     first_count,
                     second_count,
                     sum_type,
                     first_opcode,
                     second_opcode ) -> Some [ first; second ]
          | _ ->
              Option.bind matching_sequence
                (fun (targets, operands, root, opcodes) ->
                  if sequence_source_matches (targets, operands, root, opcodes)
                  then Some targets
                  else None)
      in
      if
        (not (addresses_safe && rbp_safe && dereferences_safe && simple_flow))
        || Option.is_none selected_targets
      then None
      else
        let targets = Option.get selected_targets in
        let selections =
          List.map
            (fun location ->
              let selection =
                if not (List.exists (( == ) location) targets) then Unknown
                else if
                  Frame.location_register_selection location = Request.Disabled
                then Stack
                else Register_candidate
              in
              (Frame.location_symbol location, selection))
            locations
        in
        Some
          {
            frame_ = frame;
            body_ = body;
            definition_ = Function_body.definition_declaration body;
            source_ = Function_body.definition_source body;
            options_ = Function_body.compiler_options body;
            flags_ = Function_body.stored_flags body;
            locations_ =
              List.map
                (fun location -> Option.get (location_stamp location))
                locations;
            blocks_ = blocks;
            stamps_ =
              List.map
                (fun item -> (item, Option.get (instruction_stamp item)))
                code;
            selections_ = selections;
          }

let analyze_local ~frame ~body =
  let module Typed = Sema.Function_call_expression_result in
  let module Calls = Sema.Function_call_resolution in
  let module Bind = Sema.Module_expression_binding in
  let locations = Frame.function_locations frame in
  let supported location =
    Frame.location_kind location = Frame.Automatic_local
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32)) -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let unary_parameter location =
    Frame.location_kind location = Frame.Named_parameter
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32)) -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let ordinary =
    Option.is_some (Function_body.definition_declaration body)
    && Function_body.definition_matches_frame body frame
    && Function_body.compiler_options body = Sema.Compiler_option.initial_mask
    && safe_register_requests locations
    && Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       = 0L
  in
  let parameter location =
    Frame.location_kind location = Frame.Named_parameter
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64))
        -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let count_parameter location =
    Frame.location_kind location = Frame.Named_parameter
    && Frame.location_value_shape location = Frame.Scalar
    && Sema.Type.pointer_depth (Frame.location_checked_type location) = 0
    && (match Sema.Type.base (Frame.location_checked_type location) with
      | Sema.Type.Primitive
          (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64))
        -> true
      | _ -> false)
    && Option.is_some (Frame.location_frame_slot location)
  in
  let shape =
    match locations with
    | [ location ] when supported location || unary_parameter location ->
        Some (None, None, location)
    | [ argument; location ] when parameter argument && supported location ->
        Some (Some argument, None, location)
    | [ argument; count; location ]
      when parameter argument && count_parameter count && supported location ->
        Some (Some argument, Some count, location)
    | _ -> None
  in
  match shape with
  | Some (argument, count_argument, location) when ordinary ->
      let blocks = snapshot body in
      let code =
        List.concat_map (fun (_, instructions, _) -> instructions) blocks
      in
      let entry =
        Block_graph.block_id (Block_graph.entry (Function_body.body body))
      in
      let valid_terminator (item : Seq.description) successors =
        item.operands = [] && item.result = None
        &&
        match (item.opcode, item.payload, successors) with
        | O.Ic_jmp, Some (Seq.Block target), [ successor ] ->
            Seq.Block_id.equal target successor
        | O.Ic_ret, None, [] -> true
        | _ -> false
      in
      let valid_block (id, instructions, successors) =
        if Seq.Block_id.equal id entry then true
        else
          match instructions with
          | [ item ] -> valid_terminator item successors
          | _ -> false
      in
      let core =
        List.filter
          (fun (item : Seq.description) ->
            item.opcode <> O.Ic_jmp && item.opcode <> O.Ic_ret)
          code
      in
      let entry_sequence =
        List.find_map
          (fun (id, instructions, successors) ->
            if Seq.Block_id.equal id entry then Some (instructions, successors)
            else None)
          blocks
      in
      let exact_entry =
        Option.fold ~none:false
          ~some:(fun (instructions, successors) ->
            match List.rev instructions with
            | jump :: reversed when valid_terminator jump successors ->
                same_list ( == ) core (List.rev reversed)
            | _ -> false)
          entry_sequence
      in
      let unique values =
        List.length values = List.length (List.sort_uniq Int.compare values)
      in
      let unique_ids =
        unique
          (List.map
             (fun (item : Seq.description) ->
               Seq.Instruction_id.to_int item.instruction_id)
             code)
        && unique
             (List.filter_map
                (fun (item : Seq.description) ->
                  Option.map
                    (fun (result : Seq.value_definition) ->
                      Seq.Value_id.to_int result.value_id)
                    item.result)
                code)
      in
      let id (item : Seq.description) =
        Option.map (fun (v : Seq.value_definition) -> v.value_id) item.result
      in
      let is_one item operand = item.Seq.operands = [ operand ] in
      let integer (item : Seq.description) =
        match (item.opcode, item.payload, item.operands) with
        | O.Ic_imm_i64, Some (Seq.Integer value), [] -> Some value
        | _ -> None
      in
      let address location rbp offset add =
        rbp.Seq.opcode = O.Ic_rbp && rbp.operands = [] && rbp.payload = None
        && offset.Seq.opcode = O.Ic_imm_i64
        && Option.fold ~none:false
             ~some:(fun slot ->
               integer offset = Some (Frame.frame_slot_displacement slot))
             (Frame.location_frame_slot location)
        && add.Seq.opcode = O.Ic_add && add.payload = None
        && (match (id rbp, id offset) with
          | Some r, Some o -> add.operands = [ r; o ] || add.operands = [ o; r ]
          | _ -> false)
        && Option.fold ~none:false
             ~some:(fun pointer ->
               Option.fold ~none:false ~some:(Sema.Type.equal pointer)
                 rbp.target_type
               && Option.fold ~none:false ~some:(Sema.Type.equal pointer)
                    offset.target_type
               &&
               match Sema.Type.dereference pointer with
               | Ok actual ->
                   Sema.Type.equal actual (Frame.location_checked_type location)
               | Error _ -> false)
             add.target_type
      in
      let scalar_literal (item : Seq.description) =
        Option.fold ~none:false
          ~some:(fun type_ ->
            Sema.Type.pointer_depth type_ = 0
            &&
            match Sema.Type.base type_ with
            | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) -> true
            | _ -> false)
          item.target_type
      in
      let same_type (item : Seq.description) =
        Option.fold ~none:false
          ~some:(Sema.Type.equal (Frame.location_checked_type location))
          item.target_type
      in
      let binding_of value =
        match Calls.argument_expression_kind (Typed.result_source value) with
        | Calls.Bound_identifier_expression bound -> (
            match
              Bind.occurrence_resolution
                (Calls.bound_identifier_occurrence bound)
            with
            | Bind.Local_binding binding -> Some binding
            | _ -> None)
        | _ -> None
      in
      let literal value =
        match Calls.argument_expression_kind (Typed.result_source value) with
        | Calls.Integer_literal bits -> Some bits
        | _ -> None
      in
      let source_matches initial count update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [ statement ], [ returned ] -> (
                let initial_source = Typed.initializer_source initialization in
                let local = Calls.initializer_local initial_source in
                let value = Typed.expression_statement_value statement in
                match
                  ( Calls.argument_expression_source (Typed.result_source value),
                    Typed.result_binary_operands value,
                    Typed.return_value returned )
                with
                | ( Some (Frontend.Ast.Binary_expression binary),
                    Some (left, right),
                    Some returned ) ->
                    let operator =
                      match
                        (binary.binary_operator_spec.spelling, update.Seq.opcode)
                      with
                      | "<<=", O.Ic_shl_equ
                      | ">>=", Ic_shr_equ
                      | "+=", Ic_add_equ
                      | "-=", Ic_sub_equ
                      | "*=", Ic_mul_equ
                      | "/=", Ic_div_equ
                      | "%=", Ic_mod_equ
                      | "&=", Ic_and_equ
                      | "|=", Ic_or_equ
                      | "^=", Ic_xor_equ -> true
                      | _ -> false
                    in
                    operator
                    && Sema.Local_type_resolution.local_symbol local
                       == Frame.location_symbol location
                    && (match (argument, initial) with
                      | _, Some bits ->
                          literal (Typed.initializer_value initialization)
                          = Some bits
                      | Some argument, None ->
                          Option.fold ~none:false
                            ~some:(( == ) (Frame.location_binding argument))
                            (binding_of
                               (Typed.initializer_value initialization))
                      | _ -> false)
                    && (match (count_argument, count) with
                      | None, Some bits -> literal right = Some bits
                      | Some argument, None ->
                          Option.fold ~none:false
                            ~some:(( == ) (Frame.location_binding argument))
                            (binding_of right)
                      | _ -> false)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of left)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of returned)
                | _ -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_direct initial =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [], [ returned ] ->
                let local =
                  initialization |> Typed.initializer_source
                  |> Calls.initializer_local
                in
                Sema.Local_type_resolution.local_symbol local
                == Frame.location_symbol location
                && literal (Typed.initializer_value initialization)
                   = Some initial
                && Option.fold ~none:false
                     ~some:(fun returned ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of returned))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_parameter argument =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [], [ returned ] ->
                let local =
                  initialization |> Typed.initializer_source
                  |> Calls.initializer_local
                in
                Sema.Local_type_resolution.local_symbol local
                == Frame.location_symbol location
                && Option.fold ~none:false
                     ~some:(( == ) (Frame.location_binding argument))
                     (binding_of (Typed.initializer_value initialization))
                && Option.fold ~none:false
                     ~some:(fun value ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of value))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_literal_parameter initial argument update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | [ initialization ], [ statement ], [ returned ] -> (
                let local =
                  initialization |> Typed.initializer_source
                  |> Calls.initializer_local
                in
                let value = Typed.expression_statement_value statement in
                match
                  ( Calls.argument_expression_source (Typed.result_source value),
                    Typed.result_binary_operands value,
                    Typed.return_value returned )
                with
                | ( Some (Frontend.Ast.Binary_expression binary),
                    Some (left, right),
                    Some returned ) ->
                    (match
                       (binary.binary_operator_spec.spelling, update.Seq.opcode)
                     with
                      | "+=", O.Ic_add_equ
                      | "-=", Ic_sub_equ
                      | "*=", Ic_mul_equ
                      | "/=", Ic_div_equ
                      | "%=", Ic_mod_equ
                      | "<<=", Ic_shl_equ
                      | ">>=", Ic_shr_equ
                      | "&=", Ic_and_equ
                      | "|=", Ic_or_equ
                      | "^=", Ic_xor_equ -> true
                      | _ -> false)
                    && Sema.Local_type_resolution.local_symbol local
                       == Frame.location_symbol location
                    && literal (Typed.initializer_value initialization)
                       = Some initial
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of left)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding argument))
                         (binding_of right)
                    && Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of returned)
                | _ -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_unary initial update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | initializers, [ statement ], [ returned ] ->
                let initial_matches =
                  match (initial, initializers) with
                  | None, [] -> unary_parameter location
                  | Some bits, [ initialization ] ->
                      supported location
                      && Sema.Local_type_resolution.local_symbol
                           (initialization |> Typed.initializer_source
                          |> Calls.initializer_local)
                         == Frame.location_symbol location
                      && literal (Typed.initializer_value initialization)
                         = Some bits
                  | _ -> false
                in
                let value = Typed.expression_statement_value statement in
                let operator_matches =
                  match
                    Calls.argument_expression_source (Typed.result_source value)
                  with
                  | Some (Frontend.Ast.Prefix_expression prefix) -> (
                      match
                        (prefix.prefix_operator_kind, update.Seq.opcode)
                      with
                      | Frontend.Ast.Pre_increment, O.Ic_pp_
                      | Frontend.Ast.Pre_decrement, Ic_mm_ -> true
                      | _ -> false)
                  | Some (Frontend.Ast.Postfix_expression postfix) -> (
                      match
                        (postfix.postfix_operator_kind, update.Seq.opcode)
                      with
                      | Frontend.Ast.Post_increment, O.Ic__pp
                      | Frontend.Ast.Post_decrement, Ic__mm -> true
                      | _ -> false)
                  | _ -> false
                in
                initial_matches && operator_matches
                && Option.fold ~none:false
                     ~some:(fun operand ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of operand))
                     (Typed.result_operand value)
                && Option.fold ~none:false
                     ~some:(fun value ->
                       Option.fold ~none:false
                         ~some:(( == ) (Frame.location_binding location))
                         (binding_of value))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_unary_return initial update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | initializers, [], [ returned ] ->
                let initial_matches =
                  match (initial, initializers) with
                  | None, [] -> unary_parameter location
                  | Some bits, [ initialization ] ->
                      supported location
                      && Sema.Local_type_resolution.local_symbol
                           (initialization |> Typed.initializer_source
                          |> Calls.initializer_local)
                         == Frame.location_symbol location
                      && literal (Typed.initializer_value initialization)
                         = Some bits
                  | _ -> false
                in
                initial_matches
                && Option.fold ~none:false
                     ~some:(fun value ->
                       let operator_matches =
                         match
                           Calls.argument_expression_source
                             (Typed.result_source value)
                         with
                         | Some (Frontend.Ast.Prefix_expression prefix) -> (
                             match
                               (prefix.prefix_operator_kind, update.Seq.opcode)
                             with
                             | Frontend.Ast.Pre_increment, O.Ic_pp_
                             | Frontend.Ast.Pre_decrement, Ic_mm_ -> true
                             | _ -> false)
                         | Some (Frontend.Ast.Postfix_expression postfix) -> (
                             match
                               (postfix.postfix_operator_kind, update.Seq.opcode)
                             with
                             | Frontend.Ast.Post_increment, O.Ic__pp
                             | Frontend.Ast.Post_decrement, Ic__mm -> true
                             | _ -> false)
                         | _ -> false
                       in
                       operator_matches
                       && Option.fold ~none:false
                            ~some:(fun operand ->
                              Option.fold ~none:false
                                ~some:(( == ) (Frame.location_binding location))
                                (binding_of operand))
                            (Typed.result_operand value))
                     (Typed.return_value returned)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let source_matches_compound_return initial count update =
        Option.fold ~none:false
          ~some:(fun source ->
            match
              ( Typed.function_initializers source,
                Typed.function_expression_statements source,
                Typed.function_returns source )
            with
            | initializers, [], [ returned ] -> (
                let initial_matches =
                  match (initial, initializers) with
                  | None, [] -> unary_parameter location
                  | Some bits, [ initialization ] ->
                      supported location
                      && Sema.Local_type_resolution.local_symbol
                           (initialization |> Typed.initializer_source
                          |> Calls.initializer_local)
                         == Frame.location_symbol location
                      && literal (Typed.initializer_value initialization)
                         = Some bits
                  | _ -> false
                in
                match Typed.return_value returned with
                | Some value -> (
                    match
                      ( Calls.argument_expression_source
                          (Typed.result_source value),
                        Typed.result_binary_operands value )
                    with
                    | ( Some (Frontend.Ast.Binary_expression binary),
                        Some (left, right) ) ->
                        (match
                           ( binary.binary_operator_spec.spelling,
                             update.Seq.opcode )
                         with
                          | "+=", O.Ic_add_equ
                          | "-=", Ic_sub_equ
                          | "*=", Ic_mul_equ
                          | "<<=", Ic_shl_equ
                          | ">>=", Ic_shr_equ -> true
                          | _ -> false)
                        && initial_matches
                        && literal right = Some count
                        && Option.fold ~none:false
                             ~some:(( == ) (Frame.location_binding location))
                             (binding_of left)
                    | _ -> false)
                | None -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      let matched =
        match core with
        | [ rbp; offset; address_; count; update; returned ]
          when update.Seq.opcode = O.Ic_add_equ
               || update.opcode = O.Ic_sub_equ
               || update.opcode = O.Ic_mul_equ
               || update.opcode = O.Ic_shl_equ
               || update.opcode = O.Ic_shr_equ -> (
            match (id address_, id count, integer count, id update) with
            | Some addressid, Some countid, Some countbits, Some updateid ->
                Option.is_none argument
                && Option.is_none count_argument
                && unary_parameter location
                && address location rbp offset address_
                && scalar_literal count
                && (update.opcode <> O.Ic_add_equ
                    && update.opcode <> O.Ic_sub_equ
                   || countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.payload = None && same_type update
                && update.operands = [ addressid; countid ]
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && item.flags = 0L)
                     code
                && source_matches_compound_return None countbits update
            | _ -> false)
        | [
         r1; o1; a1; initial; store; end1; r2; o2; a2; count; update; returned;
        ]
          when update.Seq.opcode = O.Ic_add_equ
               || update.opcode = O.Ic_sub_equ
               || update.opcode = O.Ic_mul_equ
               || update.opcode = O.Ic_shl_equ
               || update.opcode = O.Ic_shr_equ -> (
            match
              ( id a1,
                id initial,
                integer initial,
                id store,
                id a2,
                id count,
                integer count,
                id update )
            with
            | ( Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some countid,
                Some countbits,
                Some updateid ) ->
                Option.is_none argument
                && Option.is_none count_argument
                && supported location && address location r1 o1 a1
                && scalar_literal initial && address location r2 o2 a2
                && scalar_literal count
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.opcode <> O.Ic_add_equ
                    && update.opcode <> O.Ic_sub_equ
                   || countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_compound_return (Some initialbits) countbits
                     update
            | _ -> false)
        | [ r1; o1; a1; update; returned ] -> (
            match (id a1, id update) with
            | Some a1id, Some updateid ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a1id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && item.flags = 0L)
                     code
                && source_matches_unary_return None update
            | _ -> false)
        | [ r1; o1; a1; initial; store; end1; r2; o2; a2; update; returned ]
          when update.Seq.opcode = O.Ic_pp_
               || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
               || update.opcode = O.Ic__mm -> (
            match
              (id a1, id initial, integer initial, id store, id a2, id update)
            with
            | ( Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some updateid ) ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1 && scalar_literal initial
                && address location r2 o2 a2
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned updateid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_unary_return (Some initialbits) update
            | _ -> false)
        | [ r1; o1; a1; update; end1; r2; o2; a2; load; returned ] -> (
            match (id a1, id update, id a2, id load) with
            | Some a1id, Some updateid, Some a2id, Some loadid ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1 && address location r2 o2 a2
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a1id
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_unary None update
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         initial;
         store;
         end1;
         r2;
         o2;
         a2;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( id a1,
                id initial,
                integer initial,
                id store,
                id a2,
                id update,
                id a3,
                id load )
            with
            | ( Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                Option.is_none argument
                && Option.is_none count_argument
                && address location r1 o1 a1 && scalar_literal initial
                && address location r2 o2 a2 && address location r3 o3 a3
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_pp_
                   || update.opcode = O.Ic_mm_ || update.opcode = O.Ic__pp
                   || update.opcode = O.Ic__mm)
                && update.payload = None && same_type update
                && is_one update a2id
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_unary (Some initialbits) update
            | _ -> false)
        | [
         r1; o1; a1; rp; op; ap; input; store; end1; r2; o2; a2; load; returned;
        ] -> (
            match
              (argument, id a1, id ap, id input, id store, id a2, id load)
            with
            | ( Some argument,
                Some a1id,
                Some apid,
                Some inputid,
                Some storeid,
                Some a2id,
                Some loadid ) ->
                address location r1 o1 a1 && address argument rp op ap
                && address location r2 o2 a2
                && input.Seq.opcode = O.Ic_deref
                && input.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     input.target_type
                && is_one input apid
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; inputid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && Option.is_none count_argument
                && source_matches_parameter argument
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         rp;
         op;
         ap;
         input;
         store;
         end1;
         r2;
         o2;
         a2;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ]
          when rp.Seq.opcode = O.Ic_rbp -> (
            match
              ( argument,
                id a1,
                id ap,
                id input,
                id store,
                id a2,
                id count,
                integer count,
                id update,
                id a3,
                id load )
            with
            | ( Some argument,
                Some a1id,
                Some apid,
                Some inputid,
                Some storeid,
                Some a2id,
                Some countid,
                Some countbits,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                address location r1 o1 a1 && address argument rp op ap
                && address location r2 o2 a2 && address location r3 o3 a3
                && input.Seq.opcode = O.Ic_deref
                && input.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     input.target_type
                && is_one input apid && scalar_literal count
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; inputid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_mul_equ
                   || (update.opcode = O.Ic_add_equ
                      || update.opcode = O.Ic_sub_equ)
                      && countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches None (Some countbits) update
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         rp;
         op;
         ap;
         input;
         store;
         end1;
         r2;
         o2;
         a2;
         rc;
         oc;
         ac;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( argument,
                count_argument,
                id a1,
                id ap,
                id input,
                id store,
                id a2,
                id ac,
                id count,
                id update,
                id a3,
                id load )
            with
            | ( Some argument,
                Some count_argument,
                Some a1id,
                Some apid,
                Some inputid,
                Some storeid,
                Some a2id,
                Some acid,
                Some countid,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                address location r1 o1 a1 && address argument rp op ap
                && address location r2 o2 a2
                && address count_argument rc oc ac
                && address location r3 o3 a3
                && input.Seq.opcode = O.Ic_deref
                && input.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     input.target_type
                && is_one input apid
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; inputid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && count.Seq.opcode = O.Ic_deref
                && count.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal
                          (Frame.location_checked_type count_argument))
                     count.target_type
                && is_one count acid
                && (update.Seq.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_add_equ
                   || update.opcode = O.Ic_sub_equ
                   || update.opcode = O.Ic_mul_equ
                   || update.opcode = O.Ic_div_equ
                   || update.opcode = O.Ic_mod_equ
                   || update.opcode = O.Ic_and_equ
                   || update.opcode = O.Ic_or_equ
                   || update.opcode = O.Ic_xor_equ)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches None None update
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         initial;
         store;
         end1;
         r2;
         o2;
         a2;
         rp;
         op;
         ap;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( argument,
                count_argument,
                id a1,
                id initial,
                integer initial,
                id store,
                id a2,
                id ap,
                id count,
                id update,
                id a3,
                id load )
            with
            | ( Some argument,
                None,
                Some a1id,
                Some initialid,
                Some initialbits,
                Some storeid,
                Some a2id,
                Some apid,
                Some countid,
                Some updateid,
                Some a3id,
                Some loadid ) ->
                (match
                   Sema.Type.base (Frame.location_checked_type argument)
                 with
                  | Sema.Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) ->
                      true
                  | _ -> false)
                && address location r1 o1 a1 && scalar_literal initial
                && address location r2 o2 a2 && address argument rp op ap
                && address location r3 o3 a3
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && count.Seq.opcode = O.Ic_deref
                && count.payload = None
                && Option.fold ~none:false
                     ~some:
                       (Sema.Type.equal (Frame.location_checked_type argument))
                     count.target_type
                && is_one count apid
                && (update.Seq.opcode = O.Ic_add_equ
                   || update.opcode = O.Ic_sub_equ
                   || update.opcode = O.Ic_mul_equ
                   || update.opcode = O.Ic_div_equ
                   || update.opcode = O.Ic_mod_equ
                   || update.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_and_equ
                   || update.opcode = O.Ic_or_equ
                   || update.opcode = O.Ic_xor_equ)
                && update.payload = None && same_type update
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 updateid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && source_matches_literal_parameter initialbits argument update
            | _ -> false)
        | [ r1; o1; a1; initial; store; end1; r2; o2; a2; load; returned ] -> (
            match
              (id a1, id initial, id store, id a2, id load, integer initial)
            with
            | ( Some a1id,
                Some initialid,
                Some storeid,
                Some a2id,
                Some loadid,
                Some bits ) ->
                address location r1 o1 a1 && address location r2 o2 a2
                && scalar_literal initial
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a2id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && Option.is_none count_argument
                && source_matches_direct bits
            | _ -> false)
        | [
         r1;
         o1;
         a1;
         initial;
         store;
         end1;
         r2;
         o2;
         a2;
         count;
         update;
         end2;
         r3;
         o3;
         a3;
         load;
         returned;
        ] -> (
            match
              ( id a1,
                id initial,
                id store,
                id a2,
                id count,
                id update,
                id a3,
                id load,
                integer initial,
                integer count )
            with
            | ( Some a1id,
                Some initialid,
                Some storeid,
                Some a2id,
                Some countid,
                Some shiftid,
                Some a3id,
                Some loadid,
                Some initialbits,
                Some countbits ) ->
                address location r1 o1 a1 && address location r2 o2 a2
                && address location r3 o3 a3 && scalar_literal initial
                && scalar_literal count
                && store.Seq.opcode = O.Ic_assign
                && store.payload = None && same_type store
                && store.operands = [ a1id; initialid ]
                && end1.Seq.opcode = O.Ic_end_exp
                && end1.payload = None && end1.result = None
                && end1.flags = 0x200L && is_one end1 storeid
                && (update.Seq.opcode = O.Ic_shl_equ
                   || update.opcode = O.Ic_shr_equ
                   || update.opcode = O.Ic_add_equ
                   || update.opcode = O.Ic_sub_equ
                   || update.opcode = O.Ic_mul_equ)
                && update.payload = None && same_type update
                && (update.opcode <> O.Ic_add_equ
                    && update.opcode <> O.Ic_sub_equ
                   || countbits >= 0L
                      && countbits <= Int64.of_int32 Int32.max_int)
                && update.operands = [ a2id; countid ]
                && end2.Seq.opcode = O.Ic_end_exp
                && end2.payload = None && end2.result = None
                && end2.flags = 0x200L && is_one end2 shiftid
                && load.Seq.opcode = O.Ic_deref
                && load.payload = None && same_type load && is_one load a3id
                && returned.Seq.opcode = O.Ic_return_val
                && returned.payload = None && returned.result = None
                && is_one returned loadid
                && List.for_all
                     (fun (item : Seq.description) ->
                       Option.is_some (instruction_stamp item)
                       && (item.opcode = O.Ic_end_exp || item.flags = 0L))
                     code
                && Option.is_none count_argument
                && source_matches (Some initialbits) (Some countbits) update
            | _ -> false)
        | _ -> false
      in
      if
        List.length code > 64
        || (not (List.for_all valid_block blocks))
        || (not exact_entry) || (not unique_ids) || not matched
      then None
      else
        Some
          {
            frame_ = frame;
            body_ = body;
            definition_ = Function_body.definition_declaration body;
            source_ = Function_body.definition_source body;
            options_ = Function_body.compiler_options body;
            flags_ = Function_body.stored_flags body;
            locations_ =
              List.map (fun item -> Option.get (location_stamp item)) locations;
            blocks_ = blocks;
            stamps_ =
              List.map
                (fun item -> (item, Option.get (instruction_stamp item)))
                code;
            selections_ =
              [
                ( Frame.location_symbol location,
                  if
                    Frame.location_register_selection location
                    = Request.Disabled
                  then Stack
                  else Register_candidate );
              ];
          }
  | _ -> None

let analyze ~frame ~body =
  match analyze_parameter ~frame ~body with
  | Some proof -> Some proof
  | None -> analyze_local ~frame ~body

let selection t ~frame ~body ~symbol =
  if
    t.frame_ != frame || t.body_ != body
    || (not (Function_body.definition_matches_frame body frame))
    || (not
          (Option.equal ( == ) t.definition_
             (Function_body.definition_declaration body)))
    || (not
          (Option.equal ( == ) t.source_ (Function_body.definition_source body)))
    || t.options_ <> Function_body.compiler_options body
    || t.flags_ <> Function_body.stored_flags body
    || (not
          (same_list
             (fun original location ->
               Option.fold ~none:false ~some:(equal_location original)
                 (location_stamp location))
             t.locations_
             (Frame.function_locations frame)))
    || (not (matches_blocks t.blocks_ (snapshot body)))
    || not
         (List.for_all
            (fun (item, stamp) ->
              Option.fold ~none:false ~some:(equal_stamp stamp)
                (instruction_stamp item))
            t.stamps_)
  then None
  else
    let find selections =
      List.find_map
        (fun (original, selection) ->
          if original == symbol then Some selection else None)
        selections
    in
    match (find t.selections_, analyze ~frame ~body) with
    | Some expected, Some current when find current.selections_ = Some expected
      -> Some expected
    | _ -> None
