module Seq = Ir.Instruction_sequence
module O = Ir.Opcode
module T = Sema.Type
module Frame = Sema.Function_frame_layout
module Globals = Ir.Integer_globals
module Scalar = Ir.Integer_scalar_storage
module Values = Map.Make (Seq.Value_id)
module Locations = Map.Make (Sema.Symbol.Id)

type terminal = { value : Seq.Value_id.t; destination_type : T.t }
type failure = { instruction : Seq.description; reason : string }

type memory =
  | Memory
  | Register_candidate of Frame.location
  | Explicit_register

type address =
  | Unknown_address
  | Base of T.t
  | Offset of T.t * int64
  | Scaled of T.t * int64
  | Reference of T.t * int64 list * memory

type number = { constant : int64 option; range : (int64 * int64) option }

let unknown = { constant = None; range = None }
let bounded range = { constant = None; range = Some range }
let known bits = { constant = Some bits; range = Some (bits, bits) }
let option_exists predicate = Option.fold ~none:false ~some:predicate

let narrow_type type_ =
  option_exists
    (fun scalar -> Scalar.byte_size scalar < 8)
    (Scalar.of_type type_)

let fits scalar value =
  match (Scalar.bounds scalar, value.range) with
  | None, _ -> true
  | Some (low, high), Some (minimum, maximum) ->
      low <= minimum && maximum <= high
  | Some _, None -> false

let fits_type type_ value =
  option_exists (fun scalar -> fits scalar value) (Scalar.of_type type_)

let storage_number type_ =
  { constant = None; range = Option.bind (Scalar.of_type type_) Scalar.bounds }

let excludes bits value =
  option_exists (fun (low, high) -> bits < low || bits > high) value.range

let narrow_scalars =
  Sema.Primitive_type.all
  |> List.filter_map (fun primitive ->
      Option.bind
        (T.make_primitive ~form:T.Internal_storage ~primitive ~pointer_depth:0
        |> Result.to_option)
        Scalar.of_type)
  |> List.filter (fun scalar -> Scalar.byte_size scalar < 8)
  |> List.sort (fun left right ->
      Int.compare (Scalar.byte_size left) (Scalar.byte_size right))

let bitwise_range left right =
  List.find_opt
    (fun scalar -> fits scalar left && fits scalar right)
    narrow_scalars
  |> Option.fold ~none:unknown ~some:(fun scalar ->
      { constant = None; range = Scalar.bounds scalar })

let pointer type_ =
  match T.dereference type_ with
  | Ok pointee -> Option.is_some (Scalar.of_type pointee)
  | Error _ -> false

let update = function
  | O.Ic_add_equ
  | Ic_sub_equ
  | Ic_mul_equ
  | Ic_div_equ
  | Ic_mod_equ
  | Ic_and_equ
  | Ic_or_equ
  | Ic_xor_equ
  | Ic_shl_equ
  | Ic_shr_equ
  | Ic_pp_
  | Ic_mm_
  | Ic__pp
  | Ic__mm -> true
  | _ -> false

let increment = function
  | O.Ic_pp_ | Ic_mm_ | Ic__pp | Ic__mm -> true
  | _ -> false

let safe_compound scalar opcode right =
  match opcode with
  | O.Ic_and_equ -> Scalar.is_unsigned scalar || fits scalar right
  | Ic_div_equ -> Scalar.is_unsigned scalar || excludes (-1L) right
  | Ic_mod_equ -> true
  | Ic_or_equ | Ic_xor_equ -> fits scalar right
  | Ic_add_equ | Ic_sub_equ -> right.constant = Some 0L
  | Ic_mul_equ -> right.constant = Some 0L || right.constant = Some 1L
  | _ -> false

let discarded_bit_hazard scalar opcode right =
  match opcode with
  | O.Ic_and_equ | Ic_or_equ | Ic_xor_equ ->
      let rec check position =
        if position = 64 then false
        else
          let bit = Int64.shift_left 1L position in
          let candidate =
            if opcode = O.Ic_and_equ then Int64.lognot bit else bit
          in
          (not (opcode = O.Ic_or_equ && position = 63))
          && not (excludes candidate right)
          || check (position + 1)
      in
      check (8 * Scalar.byte_size scalar)
  | _ -> false

let scalar_number target_type opcode operands =
  match (opcode, operands) with
  | O.Ic_holyc_typecast, [ value ] -> value
  | Ic_assign, [ _; value ] ->
      (* Do not turn an unbounded assignment into optimizer constant evidence. *)
      if option_exists (fun type_ -> fits_type type_ value) target_type then
        value
      else unknown
  | Ic_com, [ { constant = Some bits; _ } ] -> known (Int64.lognot bits)
  | Ic_unary_minus, [ { constant = Some bits; _ } ] -> known (Int64.neg bits)
  | Ic_not, [ { constant = Some bits; _ } ] ->
      known (if bits = 0L then 1L else 0L)
  | ( (Ic_add | Ic_sub | Ic_mul | Ic_and | Ic_or | Ic_xor),
      [ { constant = Some left; _ }; { constant = Some right; _ } ] ) ->
      let operation =
        match opcode with
        | Ic_add -> Int64.add
        | Ic_sub -> Int64.sub
        | Ic_mul -> Int64.mul
        | Ic_and -> Int64.logand
        | Ic_or -> Int64.logor
        | Ic_xor -> Int64.logxor
        | _ -> assert false
      in
      known (operation left right)
  | Ic_and, [ left; right ] -> (
      let nonnegative value =
        match value.range with
        | Some (low, high) when low >= 0L -> Some high
        | _ -> None
      in
      match (nonnegative left, nonnegative right) with
      | Some left, Some right -> bounded (0L, min left right)
      | Some high, None | None, Some high -> bounded (0L, high)
      | None, None -> bitwise_range left right)
  | (Ic_or | Ic_xor), [ left; right ] -> bitwise_range left right
  | ( ( Ic_not
      | Ic_equ_equ
      | Ic_not_equ
      | Ic_less
      | Ic_greater_equ
      | Ic_greater
      | Ic_less_equ
      | Ic_and_and
      | Ic_or_or
      | Ic_xor_xor ),
      _ ) -> bounded (0L, 1L)
  | _ -> unknown

let check_graph ~globals ~frame ~compiler_options ~terminal graph =
  let code =
    Ir.Block_graph.blocks graph
    |> List.concat_map (fun block ->
        Ir.Block_graph.instructions block
        |> Seq.instructions |> List.map Seq.description)
  in
  let uses =
    List.fold_left
      (fun uses (item : Seq.description) ->
        List.fold_left
          (fun uses id ->
            Values.update id
              (fun previous -> Some (item :: Option.value previous ~default:[]))
              uses)
          uses item.operands)
      Values.empty code
  in
  (* A source use may disappear under identity folding. Only these sinks prove
     that a native result remains observed; casts may forward their flags. *)
  let observed =
    List.fold_left
      (fun observed (item : Seq.description) ->
        match item.result with
        | None -> observed
        | Some result ->
            let consumers =
              Option.value (Values.find_opt result.value_id uses) ~default:[]
            in
            let retained =
              Int64.logand item.flags 0x2000L <> 0L
              ||
              match consumers with
              | [ consumer ] -> (
                  match consumer.opcode with
                  | O.Ic_return_val -> true
                  | Ic_end_exp ->
                      option_exists
                        (fun sink ->
                          Seq.Value_id.equal sink.value result.value_id)
                        terminal
                  | Ic_holyc_typecast ->
                      option_exists
                        (fun type_ ->
                          T.pointer_depth type_ = 0
                          &&
                          match T.base type_ with
                          | T.Primitive (_, (Sema.Primitive_type.I64 | U64)) ->
                              true
                          | _ -> false)
                        consumer.target_type
                      && option_exists
                           (fun (result : Seq.value_definition) ->
                             Option.value
                               (Values.find_opt result.value_id observed)
                               ~default:false)
                           consumer.result
                  | _ -> false)
              | _ -> false
            in
            Values.add result.value_id retained observed)
      Values.empty (List.rev code)
  in
  let frame_memory location =
    if not (narrow_type (Frame.location_checked_type location)) then Memory
    else
      match Frame.location_register_selection location with
      | Sema.Register_request.Explicit _ -> Explicit_register
      | Disabled -> Memory
      | Unspecified | Allocatable ->
          if
            Frame.location_value_shape location = Frame.Array
            || Sema.Compiler_option.is_enabled ~mask:compiler_options
                 Sema.Compiler_option.No_reg_var
          then Memory
          else Register_candidate location
  in
  let frame_reference type_ offset =
    Option.bind frame (fun frame ->
        List.find_opt
          (fun location ->
            match
              ( Frame.location_frame_slot location,
                T.pointer_to (Frame.location_checked_type location) )
            with
            | Some slot, Ok expected ->
                Frame.frame_slot_displacement slot = offset
                && T.equal type_ expected
            | _ -> false)
          (Frame.function_locations frame))
    |> Option.map (fun location ->
        let dimensions =
          Frame.location_dimensions location |> List.map Frame.dimension_value
        in
        let rec strides = function
          | [] -> []
          | _ :: rest ->
              List.fold_left Int64.mul
                (Frame.location_element_size location)
                rest
              :: strides rest
        in
        Reference (type_, strides dimensions, frame_memory location))
    |> Option.value ~default:Unknown_address
  in
  let rec check ~verify ~proven writes addresses numbers = function
    | [] -> Ok writes
    | (item : Seq.description) :: rest -> (
        let address id =
          Option.value (Values.find_opt id addresses) ~default:Unknown_address
        in
        let number id =
          Option.value (Values.find_opt id numbers) ~default:unknown
        in
        let exact_memory id type_ =
          match address id with
          | Reference (pointer, [], Memory) -> (
              match T.dereference pointer with
              | Ok actual -> T.equal actual type_
              | Error _ -> false)
          | _ -> false
        in
        let invariant_narrow_read id type_ =
          match address id with
          | Reference (pointer, [], memory) -> (
              let bounded =
                match memory with
                | Memory -> true
                | Explicit_register -> false
                | Register_candidate location ->
                    Locations.mem
                      (Frame.location_symbol location |> Sema.Symbol.id)
                      proven
              in
              bounded
              &&
              match T.dereference pointer with
              | Ok actual -> T.equal actual type_
              | Error _ -> false)
          | _ -> false
        in
        let produced_address =
          match (item.target_type, item.payload) with
          | Some type_, Some (Seq.Symbol symbol) -> (
              match Globals.find_storage globals symbol with
              | Some slot
                when item.opcode = Globals.storage_opcode slot
                     &&
                     match Globals.storage_frame slot with
                     | None -> true
                     | Some owner ->
                         option_exists (fun frame -> frame == owner) frame -> (
                  match T.pointer_to (Globals.storage_type slot) with
                  | Ok expected when T.equal expected type_ ->
                      Reference (type_, Globals.storage_strides slot, Memory)
                  | _ -> Unknown_address)
              | _ -> Unknown_address)
          | Some type_, _ -> (
              match (item.opcode, item.operands, item.payload) with
              | O.Ic_rbp, [], None when T.pointer_depth type_ > 0 -> Base type_
              | Ic_imm_i64, [], Some (Seq.Integer bits)
                when T.pointer_depth type_ > 0 -> Offset (type_, bits)
              | Ic_mul, [ stride; _ ], None when pointer type_ -> (
                  match address stride with
                  | Offset (expected, stride)
                    when T.equal expected type_ && stride > 0L ->
                      Scaled (type_, stride)
                  | _ -> Unknown_address)
              | Ic_add, [ base; offset ], None -> (
                  match (address base, address offset) with
                  | Base expected, Offset (offset_type, bits)
                    when T.equal expected type_ && T.equal offset_type type_ ->
                      frame_reference type_ bits
                  | ( Reference (expected, dimensions, memory),
                      Scaled (offset_type, stride) )
                    when T.equal expected type_ && T.equal offset_type type_
                    -> (
                      match dimensions with
                      | head :: tail when head = stride ->
                          Reference (type_, tail, memory)
                      | [] -> Reference (type_, [], memory)
                      | _ -> Unknown_address)
                  | _ -> Unknown_address)
              | Ic_addr, [ source ], None -> (
                  match address source with
                  | Reference (expected, _, memory) when T.equal expected type_
                    -> Reference (type_, [], memory)
                  | _ -> Unknown_address)
              | Ic_str_const, [], Some (Seq.Bytes _) when pointer type_ ->
                  Reference (type_, [], Memory)
              | Ic_deref, [ _ ], None when pointer type_ ->
                  (* An actual pointer-slot load retains an indirect memory access.
                     Explicit narrow-object escapes are rejected at IC_ADDR below. *)
                  Reference (type_, [], Memory)
              | Ic_assign, [ _; source ], None when pointer type_ ->
                  address source
              | _ -> Unknown_address)
          | None, _ -> Unknown_address
        in
        let source_discard, narrow_sink =
          match item.result with
          | None -> (false, false)
          | Some result ->
              let consumers =
                Option.value (Values.find_opt result.value_id uses) ~default:[]
              in
              let only_end =
                match consumers with
                | [ consumer ] ->
                    consumer.opcode = O.Ic_end_exp && consumer.flags = 0x200L
                | _ -> false
              in
              let pushed = Int64.logand item.flags 0x2000L <> 0L in
              ( terminal = None && only_end && not pushed,
                only_end && (not pushed)
                && option_exists
                     (fun sink ->
                       Seq.Value_id.equal sink.value result.value_id
                       &&
                       match
                         ( Scalar.of_type sink.destination_type,
                           Option.bind item.target_type Scalar.of_type )
                       with
                       | Some sink, Some updated ->
                           Scalar.byte_size sink <= Scalar.byte_size updated
                       | _ -> false)
                     terminal )
        in
        let is_narrow_update =
          update item.opcode && option_exists narrow_type item.target_type
        in
        let right =
          match item.operands with
          | [ _; rhs ] -> number rhs
          | _ -> unknown
        in
        let writes =
          match item.operands with
          | target :: _ when item.opcode = O.Ic_assign || update item.opcode
            -> (
              match address target with
              | Reference (_, [], Register_candidate location) ->
                  let bounded =
                    item.opcode = O.Ic_assign
                    && fits_type (Frame.location_checked_type location) right
                  in
                  Locations.update
                    (Frame.location_symbol location |> Sema.Symbol.id)
                    (fun previous ->
                      Some (bounded && Option.value previous ~default:true))
                    writes
              | _ -> writes)
          | _ -> writes
        in
        let failure =
          if
            item.opcode = O.Ic_addr
            &&
            match produced_address with
            | Reference (_, _, Explicit_register) -> true
            | _ -> false
          then
            Some
              "initializer narrow update proof cannot use an escaping \
               explicit-register narrow object"
          else if
            match (item.opcode, item.operands, item.target_type) with
            | O.Ic_deref, [ target ], Some type_ when narrow_type type_ ->
                not (invariant_narrow_read target type_)
            | _ -> false
          then
            Some
              "initializer narrow update proof requires invariant narrow reads \
               throughout its callees"
          else if not is_narrow_update then None
          else
            match (item.operands, item.target_type) with
            | target :: _, Some type_ when exact_memory target type_ ->
                let scalar = Option.get (Scalar.of_type type_) in
                if
                  discarded_bit_hazard scalar item.opcode right
                  && not
                       (option_exists
                          (fun (result : Seq.value_definition) ->
                            Option.value
                              (Values.find_opt result.value_id observed)
                              ~default:false)
                          item.result)
                then
                  Some
                    "initializer narrow update may select a native bit \
                     operation outside its narrow object"
                else if
                  increment item.opcode
                  || safe_compound scalar item.opcode right
                  || source_discard || narrow_sink
                then None
                else
                  Some
                    "initializer narrow update exposes an unproved native \
                     full-versus-narrow result"
            | _ ->
                Some
                  "initializer narrow update requires proven native memory \
                   rather than a possible wide register"
        in
        match if verify then failure else None with
        | Some reason -> Error { instruction = item; reason }
        | None ->
            let produced_number =
              match
                (item.opcode, item.operands, item.target_type, item.payload)
              with
              | O.Ic_imm_i64, [], Some type_, Some (Seq.Integer bits)
                when T.pointer_depth type_ = 0 -> known bits
              | Ic_deref, [ target ], Some type_, None
                when narrow_type type_ && invariant_narrow_read target type_ ->
                  storage_number type_
              | _, _, Some type_, _
                when is_narrow_update
                     && (match (item.operands, item.target_type) with
                       | target :: _, Some type_ -> exact_memory target type_
                       | _ -> false)
                     && (increment item.opcode
                        || safe_compound
                             (Option.get (Scalar.of_type type_))
                             item.opcode right) -> storage_number type_
              | _ ->
                  scalar_number item.target_type item.opcode
                    (List.map number item.operands)
            in
            let addresses, numbers =
              match item.result with
              | None -> (addresses, numbers)
              | Some result ->
                  ( Values.add result.value_id produced_address addresses,
                    Values.add result.value_id produced_number numbers )
            in
            check ~verify ~proven writes addresses numbers rest)
  in
  (* Every write to an ordinary candidate must fit its declared range, and any direct
     update disqualifies it. This is a whole-graph range invariant, not a guess
     from its first initializer. Explicit hardware registers never qualify.
     Seed from constants/memory and native declared-width parameter entry, then
     admit dependent locals to a fixed point;
     unproved cycles remain outside the native initializer domain. *)
  let entry_bounds =
    Option.fold ~none:Locations.empty
      ~some:(fun frame ->
        Frame.function_locations frame
        |> List.fold_left
             (fun entries location ->
               match (Frame.location_kind location, frame_memory location) with
               | Frame.Named_parameter, Register_candidate _ ->
                   Locations.add
                     (Frame.location_symbol location |> Sema.Symbol.id)
                     true entries
               | _ -> entries)
             Locations.empty)
      frame
  in
  let rec prove proven =
    match
      check ~verify:false ~proven entry_bounds Values.empty Values.empty code
    with
    | Error _ -> assert false
    | Ok writes ->
        let next = Locations.filter (fun _ bounded -> bounded) writes in
        if Locations.equal Bool.equal proven next then proven else prove next
  in
  let proven = prove Locations.empty in
  check ~verify:true ~proven Locations.empty Values.empty Values.empty code
  |> Result.map (fun _ -> ())
