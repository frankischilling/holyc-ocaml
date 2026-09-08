module Seq = Ir.Instruction_sequence
module O = Ir.Opcode
module T = Sema.Type
module Frame = Sema.Function_frame_layout
module Globals = Ir.Integer_globals
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

type number = { constant : int64 option; byte : bool }

let unknown = { constant = None; byte = false }
let byte = { constant = None; byte = true }
let byte_bits bits = bits >= 0L && bits <= 255L
let known bits = { constant = Some bits; byte = byte_bits bits }
let option_exists predicate = Option.fold ~none:false ~some:predicate

let byte_type type_ =
  T.pointer_depth type_ = 0
  &&
  match T.base type_ with
  | T.Primitive (_, Sema.Primitive_type.U8) -> true
  | _ -> false

let pointer type_ =
  T.pointer_depth type_ = 1
  &&
  match T.base type_ with
  | T.Primitive (_, (Sema.Primitive_type.U8 | I64 | U64)) -> true
  | _ -> false

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

let safe_compound opcode right =
  match opcode with
  | O.Ic_and_equ | Ic_div_equ | Ic_mod_equ -> true
  | Ic_or_equ | Ic_xor_equ -> right.byte
  | Ic_add_equ | Ic_sub_equ -> right.constant = Some 0L
  | Ic_mul_equ -> right.constant = Some 0L || right.constant = Some 1L
  | _ -> false

let one_bit bits = bits <> 0L && Int64.logand bits (Int64.sub bits 1L) = 0L

let discarded_bit_hazard opcode right =
  match (opcode, right.constant) with
  | O.Ic_and_equ, Some bits ->
      let bit = Int64.lognot bits in
      one_bit bit && not (byte_bits bit)
  | Ic_xor_equ, Some bit -> one_bit bit && not (byte_bits bit)
  | Ic_or_equ, Some bit -> one_bit bit && bit > 255L
  | (Ic_and_equ | Ic_or_equ | Ic_xor_equ), None -> not right.byte
  | _ -> false

let scalar_number opcode operands =
  match (opcode, operands) with
  | O.Ic_holyc_typecast, [ value ] -> value
  | Ic_assign, [ _; value ] ->
      (* Do not turn an unbounded assignment into optimizer constant evidence. *)
      if value.byte then value else unknown
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
  | Ic_and, [ left; right ] when left.byte || right.byte -> byte
  | (Ic_or | Ic_xor), [ left; right ] when left.byte && right.byte -> byte
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
      _ ) -> byte
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
    if not (byte_type (Frame.location_checked_type location)) then Memory
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
        let invariant_byte_read id type_ =
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
                     Explicit byte-object escapes are rejected at IC_ADDR below. *)
                  Reference (type_, [], Memory)
              | Ic_assign, [ _; source ], None when pointer type_ ->
                  address source
              | _ -> Unknown_address)
          | None, _ -> Unknown_address
        in
        let source_discard, byte_sink =
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
                       && byte_type sink.destination_type)
                     terminal )
        in
        let is_byte_update =
          update item.opcode && option_exists byte_type item.target_type
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
                  let bounded = item.opcode = O.Ic_assign && right.byte in
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
              "initializer byte update proof cannot use an escaping \
               explicit-register byte object"
          else if
            match (item.opcode, item.operands, item.target_type) with
            | O.Ic_deref, [ target ], Some type_ when byte_type type_ ->
                not (invariant_byte_read target type_)
            | _ -> false
          then
            Some
              "initializer byte update proof requires invariant byte reads \
               throughout its callees"
          else if not is_byte_update then None
          else
            match (item.operands, item.target_type) with
            | target :: _, Some type_ when exact_memory target type_ ->
                if
                  discarded_bit_hazard item.opcode right
                  && not
                       (option_exists
                          (fun (result : Seq.value_definition) ->
                            Option.value
                              (Values.find_opt result.value_id observed)
                              ~default:false)
                          item.result)
                then
                  Some
                    "initializer byte update may select a native bit operation \
                     outside its byte object"
                else if
                  increment item.opcode
                  || safe_compound item.opcode right
                  || source_discard || byte_sink
                then None
                else
                  Some
                    "initializer byte update exposes an unproved native \
                     full-versus-narrow result"
            | _ ->
                Some
                  "initializer byte update requires proven native memory \
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
                when byte_type type_ && invariant_byte_read target type_ -> byte
              | _, _, _, _
                when is_byte_update
                     && (match (item.operands, item.target_type) with
                       | target :: _, Some type_ -> exact_memory target type_
                       | _ -> false)
                     && (increment item.opcode
                        || safe_compound item.opcode right) -> byte
              | _ -> scalar_number item.opcode (List.map number item.operands)
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
  (* Every write to an ordinary candidate must stay byte-bounded, and any direct
     update disqualifies it. This is a whole-graph range invariant, not a guess
     from its first initializer. Explicit hardware registers never qualify.
     Seed from constants/memory, then admit dependent locals to a fixed point;
     unproved cycles remain outside the native initializer domain. *)
  let rec prove proven =
    match
      check ~verify:false ~proven Locations.empty Values.empty Values.empty code
    with
    | Error _ -> assert false
    | Ok writes ->
        let next = Locations.filter (fun _ bounded -> bounded) writes in
        if Locations.equal Bool.equal proven next then proven else prove next
  in
  let proven = prove Locations.empty in
  check ~verify:true ~proven Locations.empty Values.empty Values.empty code
  |> Result.map (fun _ -> ())
