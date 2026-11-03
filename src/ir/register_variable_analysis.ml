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
    && List.length locations = 2
    && List.for_all
         (fun location ->
           Frame.location_kind location = Frame.Named_parameter
           && Frame.location_value_shape location = Frame.Scalar
           && Option.is_some
                (Integer_scalar_storage.of_type
                   (Frame.location_checked_type location))
           && Option.is_some (Frame.location_frame_slot location)
           &&
           match Frame.location_register_selection location with
           | Request.Explicit _ -> false
           | Unspecified | Allocatable | Disabled -> true)
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
    if List.length code > 64 || not (List.for_all allowed code) then None
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
        List.length additions = List.length addresses
        && List.for_all
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
                        ) -> Some target_location
                    | _ -> None)
                | _ -> None)
            | _ -> None)
        | _ -> None
      in
      let source_matches target =
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
                    let count_location =
                      List.find_opt
                        (fun location -> location != target)
                        locations
                    in
                    opcode_matches
                    && Option.fold ~none:false ~some:(( == ) target_binding)
                         (binding_of left)
                    && Option.fold ~none:false ~some:(( == ) target_binding)
                         (binding_of return_value)
                    && Option.fold ~none:false
                         ~some:(fun count ->
                           Option.fold ~none:false
                             ~some:(( == ) (Frame.location_binding count))
                             (binding_of right))
                         count_location
                | _ -> false)
            | _ -> false)
          (Function_body.definition_source body)
      in
      if
        (not (addresses_safe && rbp_safe && dereferences_safe && simple_flow))
        || Option.is_none matching_target
        || not (Option.fold ~none:false ~some:source_matches matching_target)
      then None
      else
        let target = Option.get matching_target in
        let selections =
          List.map
            (fun location ->
              let selection =
                if location != target then Unknown
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
    &&
    match Frame.location_register_selection location with
    | Request.Unspecified | Allocatable | Disabled -> true
    | Explicit _ -> false
  in
  let ordinary =
    Option.is_some (Function_body.definition_declaration body)
    && Function_body.definition_matches_frame body frame
    && Function_body.compiler_options body = Sema.Compiler_option.initial_mask
    && Int64.logand
         (Function_body.stored_flags body)
         (Int64.lognot Function_body.ordinary_calling_flag_mask)
       = 0L
  in
  match locations with
  | [ location ] when ordinary && supported location ->
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
      let address rbp offset add =
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
                      | "*=", Ic_mul_equ -> true
                      | _ -> false
                    in
                    operator
                    && Sema.Local_type_resolution.local_symbol local
                       == Frame.location_symbol location
                    && literal (Typed.initializer_value initialization)
                       = Some initial
                    && literal right = Some count
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
      let matched =
        match core with
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
                address r1 o1 a1 && address r2 o2 a2 && scalar_literal initial
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
                address r1 o1 a1 && address r2 o2 a2 && address r3 o3 a3
                && scalar_literal initial && scalar_literal count
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
                && source_matches initialbits countbits update
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
            locations_ = [ Option.get (location_stamp location) ];
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
