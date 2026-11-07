(* TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c:
   OptLib.HC:535-554 and OptPass3.HC:165-228 select links from actual producers;
   BackFA.HC:339-518 selects operand orientation and materializes unlinked
   operations; BackFB.HC:488-594 handles bit-preserving identities. Source-authenticated pure regions prove normalized links; raw graphs
   retain isolated admission. Numeric conversions require canonical scalar source
   and ordered conversion events; explicit intrinsics require sealed declarations. *)
module S = Ir.Instruction_sequence
module G = Ir.Block_graph
module O = Ir.Opcode
module V = Map.Make (S.Value_id)
module I = Map.Make (S.Instruction_id)

type load = Bits | Zero | One | Signed_integer
type conversion = To_float | To_integer

type conversion_entry = {
  conversion_instruction : S.Instruction_id.t;
  conversion_result : S.Value_id.t;
  conversion : conversion;
  deferred : bool;
}

type intrinsic_entry = {
  intrinsic_instruction : S.Instruction_id.t;
  intrinsic_input : S.Value_id.t;
  intrinsic_result : S.Value_id.t;
  intrinsic_conversion : Ir.Numeric_intrinsic.conversion;
}

type operation =
  | Convert_to_float
  | Negate
  | Add
  | Subtract
  | Multiply
  | Divide
  | Copy_left
  | Copy_right
  | Alias_left
  | Alias_right

type orientation = Left_first | Right_first | Bit_copy

type entry = {
  instruction_id : S.Instruction_id.t;
  result_id : S.Value_id.t;
  operands : S.Value_id.t list;
  loads : load list;
  operation : operation;
  live_before : S.Value_id.t list;
  live_after : S.Value_id.t list;
  peak_depth : int;
  orientation : orientation;
  live_steps : S.Value_id.t list list;
  result_conversion : conversion option;
}

type error = { message : string; span : Common.Span.t option }

type t = {
  graph : G.t;
  entries : (S.description * entry) I.t;
  peak : int;
  conversions : conversion_entry I.t;
  intrinsics : intrinsic_entry I.t;
  valid : unit -> bool;
}

let is_f64 type_ =
  Sema.Type.pointer_depth type_ = 0
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> true
  | _ -> false

let integer_type type_ =
  Sema.Type.pointer_depth type_ = 0
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive
      (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64)) ->
      true
  | _ -> false

let conversion_type = function
  | To_float ->
      Sema.Type.make_primitive ~form:Internal_storage ~primitive:F64
        ~pointer_depth:0
      |> Result.get_ok
  | To_integer ->
      Sema.Type.make_primitive ~form:Internal_storage ~primitive:I64
        ~pointer_depth:0
      |> Result.get_ok

let arithmetic = function
  | O.Ic_unary_minus -> Some Negate
  | Ic_add -> Some Add
  | Ic_sub -> Some Subtract
  | Ic_mul -> Some Multiply
  | Ic_div -> Some Divide
  | _ -> None

(* Source proof is deliberately separate from opcode recognition. Each maximal
   pure source subtree is re-lowered once at zero and once at its original IDs.
   Disjoint roots and a graph-derived node budget bound this work linearly. *)
module Typed = Sema.Function_call_expression_result
module Source = Sema.Function_call_resolution

module Physical = Hashtbl.Make (struct
  type t = Typed.expression_result

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type source = {
  runtime : Ir.Runtime_call_context.t;
  body : Ir.Function_body.t;
  frame : Sema.Function_frame_layout.function_layout;
}

let source_regions source graph =
  let covered = ref I.empty in
  (match source with
  | Some { runtime; body; frame }
    when Ir.Runtime_call_context.matches_graph runtime
           ~owner:(Ir.Runtime_call_context.Function body) graph
         && Ir.Function_body.definition_matches_frame body frame ->
      Option.iter
        (fun original ->
          let blocks =
            List.map
              (fun block ->
                Array.of_list
                  (List.map S.description
                     (S.instructions (G.instructions block))))
              (G.blocks graph)
          in
          let count = List.fold_left (fun n a -> n + Array.length a) 0 blocks in
          let budget = ref ((4 * count) + 64) in
          let memo = Physical.create count in
          let children = Physical.create count in
          let rec pure depth value =
            match Physical.find_opt memo value with
            | Some result -> result
            | None ->
                decr budget;
                let result =
                  if
                    !budget < 0 || depth > 256
                    || Typed.result_array_rank value <> 0
                    || not
                         (Option.fold ~none:false
                            ~some:(fun t -> is_f64 t || integer_type t)
                            (Typed.result_type value))
                  then false
                  else
                    let child v = pure (depth + 1) v in
                    match
                      Source.argument_expression_kind
                        (Typed.result_source value)
                    with
                    | Source.Float_literal _ | Source.Integer_literal _ -> true
                    | Bound_identifier_expression _ -> (
                        match
                          Ir.Frame_address_lowering.prepare ~frame value
                        with
                        | Ok (Some _) -> true
                        | _ -> false)
                    | Parenthesized_expression _ ->
                        Option.fold ~none:false ~some:child
                          (Typed.result_operand value)
                    | Prefix_expression p
                      when List.mem (Source.prefix_operator p)
                             [ Source.Unary_plus; Source.Unary_minus ] ->
                        Option.fold ~none:false ~some:child
                          (Typed.result_operand value)
                    | Binary_expression b
                      when List.mem
                             (Source.binary_operator_name
                                (Source.binary_operator b))
                             [ "IC_ADD"; "IC_SUB"; "IC_MUL"; "IC_DIV" ] -> (
                        match Typed.result_binary_operands value with
                        | Some (l, r) -> child l && child r
                        | None -> false)
                    | _ -> false
                in
                Physical.replace memo value result;
                if result then (
                  Option.iter
                    (fun v -> Physical.replace children v ())
                    (Typed.result_operand value);
                  Option.iter
                    (fun (l, r) ->
                      Physical.replace children l ();
                      Physical.replace children r ())
                    (Typed.result_binary_operands value));
                result
          in
          let all = Typed.function_all_results original in
          List.iter (fun value -> ignore (pure 0 value)) all;
          let starts = Hashtbl.create count in
          List.iter
            (fun block ->
              Array.iteri
                (fun index d ->
                  let key = (d.S.opcode, d.span) in
                  let old =
                    Option.value (Hashtbl.find_opt starts key) ~default:[]
                  in
                  Hashtbl.replace starts key ((block, index) :: old))
                block)
            blocks;
          let lower instruction_id value_id value =
            match
              Ir.Expression_lowering.lower_typed_result ~frame ~instruction_id
                ~value_id value
            with
            | Ok (Ir.Expression_lowering.Lowered result) ->
                Some
                  (List.map S.description
                     (S.instructions (Ir.Expression_lowering.sequence result)))
            | _ -> None
          in
          let zero_i = S.Instruction_id.of_int 0 |> Result.get_ok in
          let zero_v = S.Value_id.of_int 0 |> Result.get_ok in
          let region = ref 0 in
          List.iter
            (fun value ->
              if
                !budget >= 0 && pure 0 value
                && not (Physical.mem children value)
              then
                let expression = Typed.result_source value in
                match Source.argument_expression_source expression with
                | Some ast
                  when Source.validate_source_expression ~source:ast ~expression
                         ~calls:[] ()
                       = Ok () -> (
                    match lower zero_i zero_v value with
                    | Some (first :: _ as canonical) -> (
                        let candidates =
                          Option.value
                            (Hashtbl.find_opt starts (first.opcode, first.span))
                            ~default:[]
                        in
                        (* Ambiguous source sites cannot confer ownership. *)
                        match candidates with
                        | [ (block, index) ]
                          when List.length canonical
                               <= Array.length block - index -> (
                            let start = block.(index) in
                            match start.result with
                            | Some result -> (
                                match
                                  lower start.instruction_id result.value_id
                                    value
                                with
                                | Some descriptions ->
                                    let length = List.length descriptions in
                                    let valid =
                                      List.mapi
                                        (fun offset expected ->
                                          let actual = block.(index + offset) in
                                          expected = actual
                                          || offset = length - 1
                                             && Int64.logand actual.flags
                                                  (Int64.lognot 0x2003L)
                                                = Int64.logand expected.flags
                                                    (Int64.lognot 0x2003L)
                                             && {
                                                  actual with
                                                  flags = expected.flags;
                                                }
                                                = expected)
                                        descriptions
                                      |> List.for_all Fun.id
                                    in
                                    if valid then (
                                      incr region;
                                      List.iter
                                        (fun d ->
                                          covered :=
                                            I.add d.S.instruction_id !region
                                              !covered)
                                        descriptions)
                                | None -> ())
                            | None -> ())
                        | _ -> ())
                    | _ -> ())
                | _ -> ())
            all)
        (Ir.Function_body.definition_source body)
  | _ -> ());
  !covered

let prepare ?source ?runtime ?owner graph =
  let runtime_context =
    match (runtime, owner, source) with
    | Some runtime, Some owner, _ -> Some (runtime, owner)
    | _, _, Some s -> Some (s.runtime, Ir.Runtime_call_context.Function s.body)
    | _ -> None
  in
  let runtime_valid () =
    Option.fold ~none:false
      ~some:(fun (runtime, owner) ->
        Ir.Runtime_call_context.matches_graph runtime ~owner graph)
      runtime_context
  in
  let authentic_source () =
    match source with
    | Some s ->
        Ir.Runtime_call_context.matches_graph s.runtime
          ~owner:(Ir.Runtime_call_context.Function s.body) graph
        && Ir.Register_variable_analysis.authenticates_mixed_scalar_source
             ~runtime:s.runtime ~frame:s.frame ~body:s.body
    | None -> false
  in
  let authenticated = authentic_source () in
  let regions = source_regions source graph in
  let definitions = ref V.empty in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          let d = S.description instruction in
          Option.iter
            (fun (r : S.value_definition) ->
              definitions := V.add r.value_id d !definitions)
            d.result)
        (S.instructions (G.instructions block)))
    (G.blocks graph);
  let errors = ref [] in
  let entries = ref I.empty in
  let peak = ref 0 in
  let fail (d : S.description) message =
    errors := { message; span = d.span } :: !errors
  in
  let producer id = V.find_opt id !definitions in
  let users id =
    List.concat_map
      (fun block ->
        List.filter_map
          (fun instruction ->
            let d = S.description instruction in
            if List.mem id d.operands then Some d else None)
          (S.instructions (G.instructions block)))
      (G.blocks graph)
  in
  let rec integer_constant depth id =
    depth > 0
    &&
    match producer id with
    | Some
        {
          opcode = O.Ic_imm_i64;
          payload = Some (S.Integer _);
          target_type = Some t;
          _;
        } -> integer_type t
    | Some d
      when Option.fold ~none:false ~some:integer_type d.target_type
           && List.mem d.opcode
                [
                  O.Ic_unary_minus;
                  Ic_com;
                  Ic_not;
                  Ic_add;
                  Ic_sub;
                  Ic_mul;
                  Ic_div;
                  Ic_mod;
                  Ic_and;
                  Ic_or;
                  Ic_xor;
                  Ic_shl;
                  Ic_shr;
                  Ic_shl_const;
                  Ic_shr_const;
                ] ->
        d.operands <> []
        && List.for_all (integer_constant (depth - 1)) d.operands
    | _ -> false
  in
  let intrinsics = ref I.empty in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          let d = S.description instruction in
          match Ir.Numeric_intrinsic.conversion d.opcode with
          | None -> ()
          | Some direction -> (
              let receipt =
                Option.bind runtime_context (fun (runtime, owner) ->
                    if runtime_valid () then
                      Ir.Runtime_call_context.find_intrinsic_instruction runtime
                        ~owner d.instruction_id
                    else None)
              in
              match receipt with
              | Some receipt -> (
                  let module R = Ir.Runtime_call_context in
                  match R.intrinsic_arguments receipt with
                  | [ argument ] ->
                      let input = R.argument_value argument in
                      let result = R.intrinsic_result_value receipt in
                      let source_type = R.argument_source_type argument in
                      let family =
                        match direction with
                        | Ir.Numeric_intrinsic.To_float ->
                            integer_type source_type
                        | To_integer -> is_f64 source_type
                      in
                      let simple_input =
                        match (producer input, direction) with
                        | Some p, Ir.Numeric_intrinsic.To_integer ->
                            List.mem p.opcode
                              [
                                O.Ic_deref;
                                Ic_imm_f64;
                                Ic_call_end;
                                Ic_unary_minus;
                              ]
                            && Int64.logand p.flags 3L = 0L
                        | Some p, To_float -> Int64.logand p.flags 3L = 0L
                        | _ -> false
                      in
                      let boundary =
                        List.for_all
                          (fun (consumer : S.description) ->
                            List.mem consumer.opcode
                              [ O.Ic_assign; Ic_return_val; Ic_end_exp ])
                          (users result)
                      in
                      let result_producer = producer result in
                      if
                        (not family) || (not simple_input) || (not boundary)
                        || not
                             (Option.fold ~none:false
                                ~some:(fun (p : S.description) ->
                                  Int64.logand p.flags 3L = 0L)
                                result_producer)
                      then
                        fail d
                          "numeric intrinsic requires an original materialized \
                           scalar boundary"
                      else
                        intrinsics :=
                          I.add d.instruction_id
                            {
                              intrinsic_instruction = d.instruction_id;
                              intrinsic_input = input;
                              intrinsic_result = result;
                              intrinsic_conversion = direction;
                            }
                            !intrinsics
                  | _ ->
                      fail d "numeric intrinsic lost its exact unary argument")
              | None ->
                  fail d
                    "numeric intrinsic lacks its sealed original declaration"))
        (S.instructions (G.instructions block)))
    (G.blocks graph);
  let conversions = ref I.empty in
  V.iter
    (fun id (d : S.description) ->
      let bits = Int64.logand d.flags 3L in
      if bits <> 0L then
        let direction = if bits = 1L then To_float else To_integer in
        if
          (not authenticated) || bits = 3L
          || not
               (Option.fold ~none:false
                  ~some:(if direction = To_float then integer_type else is_f64)
                  d.target_type)
        then
          fail d "numeric conversion has no exact original scalar source proof"
        else
          let deferred =
            match users id with
            | [ consumer ]
              when d.opcode = O.Ic_deref && direction = To_float
                   && Int64.logand d.flags 0x2000L = 0L
                   && Option.is_some (arithmetic consumer.opcode)
                   && Option.fold ~none:false ~some:is_f64 consumer.target_type
              -> true
            | _ -> false
          in
          conversions :=
            I.add d.instruction_id
              {
                conversion_instruction = d.instruction_id;
                conversion_result = id;
                conversion = direction;
                deferred;
              }
              !conversions)
    !definitions;
  let conversion d = I.find_opt d.S.instruction_id !conversions in
  let deferred id =
    Option.fold ~none:false
      ~some:(fun d ->
        Option.fold ~none:false ~some:(fun c -> c.deferred) (conversion d))
      (producer id)
  in
  let float_value id =
    Option.fold ~none:false
      ~some:(fun (d : S.description) ->
        match conversion d with
        | Some c -> c.conversion = To_float
        | None -> Option.fold ~none:false ~some:is_f64 d.target_type)
      (producer id)
  in
  let rec integer_constant_bits depth id =
    if depth <= 0 then None
    else
      match producer id with
      | Some { opcode = O.Ic_imm_i64; payload = Some (S.Integer bits); _ } ->
          Some bits
      | Some d -> (
          let child = integer_constant_bits (depth - 1) in
          match (d.opcode, d.operands) with
          | O.Ic_unary_minus, [ input ] -> Option.map Int64.neg (child input)
          | ((O.Ic_add | Ic_sub | Ic_mul) as opcode), [ left; right ] ->
              Option.bind (child left) (fun left ->
                  Option.map
                    (fun right ->
                      (match opcode with
                      | O.Ic_add -> Int64.add
                      | Ic_sub -> Int64.sub
                      | _ -> Int64.mul)
                        left right)
                    (child right))
          | _ -> None)
      | None -> None
  in
  let literal id =
    match producer id with
    | Some { opcode = O.Ic_imm_f64; payload = Some (S.Float_bits bits); _ } ->
        Some bits
    | Some d
      when Option.fold ~none:false
             ~some:(fun c -> c.conversion = To_float)
             (conversion d) ->
        Option.map
          (fun bits -> Int64.bits_of_float (Int64.to_float bits))
          (integer_constant_bits 256 id)
    | _ -> None
  in
  let rec unproved_integer_normalization depth id =
    if depth <= 0 then true
    else
      match producer id with
      | Some d when Option.fold ~none:false ~some:integer_type d.target_type ->
          let immediate = integer_constant_bits 256 in
          let literal_child id =
            match producer id with
            | Some child when Option.is_some (arithmetic child.opcode) ->
                List.exists
                  (fun id -> Option.is_some (immediate id))
                  child.operands
            | _ -> false
          in
          let here =
            match (d.opcode, d.operands) with
            | O.Ic_add, [ l; r ] ->
                immediate l = Some 0L
                || immediate r = Some 0L
                || (Option.is_some (immediate l) && literal_child r)
                || (Option.is_some (immediate r) && literal_child l)
            | Ic_sub, [ l; r ] ->
                immediate r = Some 0L
                || (Option.is_some (immediate r) && literal_child l)
            | Ic_mul, [ l; r ] ->
                List.exists
                  (fun x -> List.mem (immediate x) [ Some 0L; Some 1L ])
                  [ l; r ]
                || (Option.is_some (immediate r) && literal_child l)
            | Ic_div, [ _; r ] -> immediate r = Some 1L
            | _ -> false
          in
          here
          || List.exists (unproved_integer_normalization (depth - 1)) d.operands
      | _ -> false
  in
  I.iter
    (fun _ receipt ->
      if
        receipt.conversion = To_float
        && (not (integer_constant 256 receipt.conversion_result))
        && unproved_integer_normalization 256 receipt.conversion_result
      then
        Option.iter
          (fun d ->
            fail d
              "integer conversion producer requires source normalization \
               evidence")
          (producer receipt.conversion_result);
      if
        receipt.conversion = To_float
        && integer_constant 256 receipt.conversion_result
        && Option.is_none (integer_constant_bits 256 receipt.conversion_result)
      then
        Option.iter
          (fun d ->
            fail d
              "integer constant conversion requires source normalization \
               evidence")
          (producer receipt.conversion_result))
    !conversions;
  let literal_load d id =
    if deferred id then Signed_integer
    else
      match literal id with
      | Some bits when Int64.logand bits 0x7fffffffffffffffL = 0L -> Zero
      | Some 0x3ff0000000000000L -> One
      | Some
          ( 0x400921fb54442d18L
          | 0x400a934f0979a371L
          | 0x3ff71547652b82feL
          | 0x3fd34413509f79ffL
          | 0x3fe62e42fefa39efL ) ->
          fail d
            "native F64 arithmetic builtin constants require original \
             compiler-option evidence";
          Bits
      | _ -> Bits
  in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          let d = S.description instruction in
          let target_float =
            Option.fold ~none:false ~some:is_f64 d.target_type
          in
          (match (d.opcode, d.target_type) with
          | (O.Ic_addr | Ic_holyc_typecast), Some type_
            when Sema.Type.pointer_depth type_ > 0
                 &&
                 match Sema.Type.base type_ with
                 | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> true
                 | _ -> false ->
              fail d "native F64 pointer values are unsupported"
          | _ -> ());
          let touches_float =
            target_float || List.exists float_value d.operands
          in
          if touches_float then (
            (* Conversion, comparison and suppression flags are not a source link
         producer. A forged or merely depth-balanced suppression cannot admit
         an extended value. PUSH and END_EXP are transport flags only. *)
            let allowed_flags =
              if d.opcode = O.Ic_end_exp then 0x200L
              else if Option.is_some (conversion d) then 0x2003L
              else 0x2000L
            in
            if Int64.logand d.flags (Int64.lognot allowed_flags) <> 0L then
              fail d
                "native F64 conversions and unproved x87 links are unsupported";
            match if target_float then arithmetic d.opcode else None with
            | Some operation -> (
                if
                  (not target_float)
                  || not (List.for_all float_value d.operands)
                then
                  fail d
                    "native F64 arithmetic requires exclusively F64 operands";
                let loads = List.map (literal_load d) d.operands in
                (* BackFB.HC:488-594 selects bit copies for these identities before
             the general x87 template, preserving even signaling NaN bits. *)
                let operation =
                  match (operation, d.operands) with
                  | Add, [ l; _ ] when literal l = Some 0L -> Copy_right
                  | (Add | Subtract), [ _; r ] when literal r = Some 0L ->
                      Copy_left
                  | Multiply, [ l; _ ] when literal l = Some 0x3ff0000000000000L
                    -> Copy_right
                  | (Multiply | Divide), [ _; r ]
                    when literal r = Some 0x3ff0000000000000L -> Copy_left
                  | _ -> operation
                in
                let operation =
                  if
                    (Option.is_some (conversion d)
                    || List.mem Signed_integer loads)
                    && List.mem operation [ Copy_left; Copy_right ]
                  then
                    fail d
                      "numeric conversion identity requires source \
                       normalization evidence";
                  operation
                in
                let operation =
                  if I.mem d.instruction_id regions then
                    match (operation, d.opcode) with
                    | Copy_left, (O.Ic_add | Ic_sub | Ic_mul) -> Alias_left
                    | Copy_right, O.Ic_add -> Alias_right
                    | _ -> operation
                  else operation
                in
                let depth =
                  match (operation, loads) with
                  | (Copy_left | Copy_right | Alias_left | Alias_right), _ -> 0
                  | Negate, _ -> 1
                  | _, [ Bits; _ ] | _, [ _; Bits ] -> 1
                  | _ -> 2
                in
                match d.result with
                | None ->
                    fail d
                      "native F64 arithmetic has no original result identity"
                | Some result ->
                    let orientation, live_steps =
                      match (operation, d.operands, loads) with
                      | ( (Copy_left | Copy_right | Alias_left | Alias_right),
                          _,
                          _ ) -> (Bit_copy, [ [] ])
                      | Negate, [ input ], _ ->
                          ( Left_first,
                            [ []; [ input ]; [ result.value_id ]; [] ] )
                      | _, [ _; right ], [ Bits; (Zero | One | Signed_integer) ]
                        ->
                          ( Right_first,
                            [ []; [ right ]; [ result.value_id ]; [] ] )
                      | _, [ left; _ ], [ _; Bits ] ->
                          (Left_first, [ []; [ left ]; [ result.value_id ]; [] ])
                      | _, [ left; right ], _ ->
                          ( Left_first,
                            [
                              [];
                              [ left ];
                              [ right; left ];
                              [ result.value_id ];
                              [];
                            ] )
                      | _ -> (Left_first, [])
                    in
                    let entry =
                      {
                        instruction_id = d.instruction_id;
                        result_id = result.value_id;
                        operands = d.operands;
                        loads;
                        operation;
                        live_before = [];
                        live_after = [];
                        peak_depth = depth;
                        orientation;
                        live_steps;
                        result_conversion =
                          Option.map (fun c -> c.conversion) (conversion d);
                      }
                    in
                    entries := I.add d.instruction_id (d, entry) !entries;
                    peak := max !peak depth)
            | None -> (
                match d.opcode with
                | (O.Ic_to_i64 | Ic_to_f64)
                  when I.mem d.instruction_id !intrinsics -> ()
                | O.Ic_imm_i64
                | O.Ic_imm_f64
                | Ic_deref
                | Ic_assign
                | Ic_call
                | Ic_call_indirect2
                | Ic_add_rsp
                | Ic_add_rsp1
                | Ic_call_end
                | Ic_return_val
                | Ic_end_exp
                | Ic_set_rax -> ()
                | _ ->
                    fail d
                      (O.to_source_name d.opcode
                     ^ " is outside the native scalar F64 transport subset"))))
        (S.instructions (G.instructions block)))
    (G.blocks graph);
  (* A non-MOV integer producer keeps RES_TO_F64 at CN_RES. Its conversion
     is itself an extended-value producer, unlike an immediate folded in
     OptPass012/OptLib before the floating event pass. *)
  I.iter
    (fun id receipt ->
      if receipt.conversion = To_float && not receipt.deferred then
        match producer receipt.conversion_result with
        | Some d when not (integer_constant 256 receipt.conversion_result) ->
            if not (I.mem id regions) then (
              if
                List.exists
                  (fun (consumer : S.description) ->
                    Option.is_some (arithmetic consumer.opcode)
                    && Option.fold ~none:false ~some:is_f64 consumer.target_type)
                  (users receipt.conversion_result)
              then
                fail d
                  "numeric conversion producer lacks a canonical arithmetic \
                   region")
            else
              entries :=
                I.add id
                  ( d,
                    {
                      instruction_id = id;
                      result_id = receipt.conversion_result;
                      operands = [ receipt.conversion_result ];
                      loads = [ Signed_integer ];
                      operation = Convert_to_float;
                      live_before = [];
                      live_after = [];
                      peak_depth = 1;
                      orientation = Left_first;
                      live_steps = [ []; [ receipt.conversion_result ]; [] ];
                      result_conversion = None;
                    } )
                  !entries
        | _ -> ())
    !conversions;
  (* Only an authenticated source region can normalize producers or select
     asymmetric OptLib links. Raw graphs retain the isolated-only contract. *)
  let uses = ref V.empty in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          List.iter
            (fun id ->
              uses :=
                V.add id
                  (1 + Option.value (V.find_opt id !uses) ~default:0)
                  !uses)
            (S.description instruction).operands)
        (S.instructions (G.instructions block)))
    (G.blocks graph);
  let entry_of id =
    Option.bind (producer id) (fun d -> I.find_opt d.S.instruction_id !entries)
  in
  let alias_operand entry =
    match (entry.operation, entry.operands) with
    | Alias_left, left :: _ -> Some left
    | Alias_right, [ _; right ] -> Some right
    | _ -> None
  in
  let normalized = ref V.empty in
  I.iter
    (fun _ (_, entry) ->
      let id =
        match alias_operand entry with
        | Some id -> Option.value (V.find_opt id !normalized) ~default:id
        | None -> entry.result_id
      in
      normalized := V.add entry.result_id id !normalized)
    !entries;
  let normal id = Option.value (V.find_opt id !normalized) ~default:id in
  let normal_literal id = literal (normal id) in
  let fpop id =
    match entry_of (normal id) with
    | Some
        ( _,
          {
            operation = Add | Subtract | Multiply | Divide | Convert_to_float;
            result_conversion = None;
            _;
          } ) -> true
    | _ -> false
  in
  let mov_or_imm id =
    match producer (normal id) with
    | Some d when I.mem d.instruction_id regions ->
        (d.opcode = O.Ic_imm_f64 || d.opcode = O.Ic_deref)
        && Option.is_none (conversion d)
    | _ -> false
  in
  let retained = ref V.empty in
  let inputs = ref I.empty in
  let rec retain (consumer : S.description) id =
    if Option.value (V.find_opt id !uses) ~default:0 <> 1 then
      fail consumer
        "native F64 extended producer must have exactly one original use"
    else
      match entry_of id with
      | Some (d, entry) -> (
          retained := V.add id () !retained;
          match alias_operand entry with
          | Some source ->
              inputs := I.add d.instruction_id source !inputs;
              retain d source
          | None -> ())
      | None ->
          fail consumer "native F64 link has no original arithmetic producer"
  in
  let linked_regions = Hashtbl.create 16 in
  I.iter
    (fun _ ((d : S.description), entry) ->
      let adjacent =
        entry.operation <> Convert_to_float
        && List.exists (fun id -> Option.is_some (entry_of id)) entry.operands
      in
      (if adjacent then
         let region = I.find_opt d.instruction_id regions in
         if
           Option.is_none region
           || List.exists
                (fun id ->
                  match entry_of id with
                  | Some (p, _) -> I.find_opt p.instruction_id regions <> region
                  | None -> false)
                entry.operands
         then
           fail d
             "native F64 linked arithmetic requires an original extended-value \
              link proof"
         else Hashtbl.replace linked_regions (Option.get region) ());
      match (entry.operation, entry.operands) with
      | (Add | Subtract | Multiply | Divide), [ left; right ]
        when I.mem d.instruction_id regions ->
          let selected =
            if deferred left || deferred right then None
            else if mov_or_imm right && fpop left then Some left
            else if
              fpop right
              && not
                   (Option.fold ~none:false
                      ~some:(fun p ->
                        Option.fold ~none:false
                          ~some:(fun c -> c.conversion = To_float)
                          (conversion p))
                      (producer left))
            then Some right
            else None
          in
          Option.iter
            (fun id ->
              inputs := I.add d.instruction_id id !inputs;
              retain d id)
            selected
      | _ -> ())
    !entries;
  (* Folding/reassociation must precede link selection. These shapes are not
     implemented yet; reject the entire affected region rather than round it. *)
  I.iter
    (fun _ ((d : S.description), entry) ->
      let linked =
        Option.fold ~none:false
          ~some:(Hashtbl.mem linked_regions)
          (I.find_opt d.instruction_id regions)
      in
      if linked then
        let literal_child id =
          match entry_of (normal id) with
          | Some (_, child) ->
              List.exists
                (fun id -> Option.is_some (normal_literal id))
                child.operands
          | _ -> false
        in
        let bad =
          match (entry.operation, entry.operands) with
          | Negate, [ id ] -> Option.is_some (normal_literal id)
          | _, [ l; r ]
            when Option.is_some (normal_literal l)
                 && Option.is_some (normal_literal r) -> true
          | (Add | Subtract | Multiply | Divide), [ l; r ] -> (
              Option.is_some (normal_literal l)
              && Option.is_some (normal_literal r)
              ||
              match entry.operation with
              | Add ->
                  (Option.is_some (normal_literal l) && literal_child r)
                  || (Option.is_some (normal_literal r) && literal_child l)
              | Subtract | Multiply ->
                  Option.is_some (normal_literal r) && literal_child l
              | _ -> false)
          | _ -> false
        in
        if bad then
          fail d
            "native F64 linked region requires unsupported source constant \
             normalization")
    !entries;
  let live = ref [] in
  entries :=
    I.mapi
      (fun id ((d : S.description), entry) ->
        let before = Option.to_list (I.find_opt id !inputs) in
        let after =
          if V.mem entry.result_id !retained then [ entry.result_id ] else []
        in
        let before, after =
          match (entry.operation, before, after, !live) with
          | (Alias_left | Alias_right), [], [], ([ owner ] as live)
            when Option.bind (producer owner) (fun p ->
                     I.find_opt p.S.instruction_id regions)
                 = I.find_opt d.instruction_id regions -> (live, live)
          | _ -> (before, after)
        in
        if !live <> before then
          fail d "native F64 source link crosses an unproved floating event";
        live := after;
        let peak_depth, live_steps =
          match (before, entry.operation, entry.operands, entry.loads) with
          | [ _ ], (Alias_left | Alias_right), _, _ -> (1, [ before; after ])
          | [ id ], _, [ left; right ], [ ll; rl ] ->
              let other, loading =
                if id = left then (right, rl) else (left, ll)
              in
              let transient =
                if loading = Bits then [] else [ [ other; id ] ]
              in
              ( (if loading = Bits then 1 else 2),
                (before :: transient) @ [ [ entry.result_id ]; after ] )
          | [], _, _, _ when after <> [] ->
              (entry.peak_depth, List.rev (List.tl (List.rev entry.live_steps)))
          | _ -> (entry.peak_depth, entry.live_steps)
        in
        peak := max !peak peak_depth;
        ( d,
          {
            entry with
            live_before = before;
            live_after = after;
            peak_depth;
            live_steps;
          } ))
      !entries;
  if !live <> [] then
    errors :=
      { message = "native F64 link escapes its source region"; span = None }
      :: !errors;
  match List.rev !errors with
  | [] ->
      (* Freeze only primitive graph fields. Opaque payloads retain identity
         stamps and their existing domain validators; saved native defaults may
         contain closures and must never be recursively serialized here. *)
      let snapshot () =
        let opaque = ref [] in
        let copy text = Bytes.to_string (Bytes.of_string text) in
        let type_stamp type_ =
          let kind, name =
            match Sema.Type.base type_ with
            | Sema.Type.Primitive (form, primitive) ->
                ( 0,
                  Sema.Type.primitive_form_name form
                  ^ ":"
                  ^ Sema.Primitive_type.to_string primitive )
            | Aggregate symbol ->
                ( Sema.Symbol.execution_identity symbol,
                  copy (Sema.Symbol.name symbol) )
          in
          (Sema.Type.pointer_depth type_, kind, name)
        in
        let description (d : S.description) =
          let payload =
            match d.payload with
            | None -> (0, None, None, [])
            | Some (S.Integer bits) -> (1, Some bits, None, [])
            | Some (S.Float_bits bits) -> (2, Some bits, None, [])
            | Some (S.Bytes bytes) -> (3, None, Some (copy bytes), [])
            | Some (S.Symbol symbol) ->
                ( 4,
                  None,
                  Some (copy (Sema.Symbol.name symbol)),
                  [ Sema.Symbol.execution_identity symbol ] )
            | Some (S.Block block) ->
                (5, None, None, [ S.Block_id.to_int block ])
            | Some (S.Block_targets blocks) ->
                (6, None, None, List.map S.Block_id.to_int blocks)
            | Some payload ->
                opaque := (d.instruction_id, payload) :: !opaque;
                let kind =
                  match payload with
                  | S.Callback _ -> 7
                  | Member_projection _ -> 8
                  | Backing_projection _ -> 9
                  | Pointee_stride _ -> 10
                  | Empty_frame_root _ -> 11
                  | Class_pointer_view _ -> 12
                  | Retained_global _ -> 13
                  | Saved_parameter_default _ -> 14
                  | Saved_callback_default _ -> 15
                  | _ -> assert false
                in
                (kind, None, None, [])
          in
          ( S.Instruction_id.to_int d.instruction_id,
            O.to_source_name d.opcode,
            List.map S.Value_id.to_int d.operands,
            Option.map
              (fun (r : S.value_definition) -> S.Value_id.to_int r.value_id)
              d.result,
            Option.map type_stamp d.target_type,
            payload,
            d.flags,
            Option.map
              (fun (span : Common.Span.t) ->
                (Common.Source_id.to_int span.source, span.start, span.stop))
              d.span )
        in
        let blocks =
          List.map
            (fun block ->
              ( S.Block_id.to_int (G.block_id block),
                List.map
                  (fun instruction -> description (S.description instruction))
                  (S.instructions (G.instructions block)),
                List.map S.Block_id.to_int (G.successors block) ))
            (G.blocks graph)
        in
        ((S.Block_id.to_int (G.block_id (G.entry graph)), blocks), !opaque)
      in
      let original =
        if
          I.is_empty !entries && I.is_empty !conversions
          && I.is_empty !intrinsics
        then None
        else Some (snapshot ())
      in
      let source_owner =
        Option.bind source (fun s -> Ir.Function_body.definition_source s.body)
      in
      let valid () =
        match original with
        | None -> true
        | Some (original, owners) ->
            let current, current_owners = snapshot () in
            current = original
            && List.length owners = List.length current_owners
            && List.for_all2
                 (fun (id, payload) (other_id, other_payload) ->
                   S.Instruction_id.equal id other_id
                   && payload == other_payload)
                 owners current_owners
            && (match (source, source_owner) with
              | Some s, Some owner ->
                  Option.fold ~none:false
                    ~some:(fun current -> current == owner)
                    (Ir.Function_body.definition_source s.body)
              | None, None -> true
              | Some _, None -> true
              | _ -> false)
            && source_regions source graph = regions
            && (I.is_empty !conversions || authentic_source ())
            && (I.is_empty !intrinsics || runtime_valid ())
      in
      Ok
        {
          graph;
          entries = !entries;
          conversions = !conversions;
          intrinsics = !intrinsics;
          peak = !peak;
          valid;
        }
  | errors -> Error errors

let find plan ~graph description =
  if graph != plan.graph || not (plan.valid ()) then None
  else
    match I.find_opt description.S.instruction_id plan.entries with
    | Some (original, entry) when original = description -> Some entry
    | _ -> None

let peak_depth plan = plan.peak

let validated_entries plan ~graph =
  if graph != plan.graph || not (plan.valid ()) then None
  else Some (I.bindings plan.entries |> List.map (fun (_, (_, entry)) -> entry))

let validated_conversions plan ~graph =
  if graph != plan.graph || not (plan.valid ()) then None
  else Some (I.bindings plan.conversions |> List.map snd)

let validated_intrinsics plan ~graph =
  if graph != plan.graph || not (plan.valid ()) then None
  else Some (I.bindings plan.intrinsics |> List.map snd)
