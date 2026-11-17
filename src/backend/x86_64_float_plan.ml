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

type load =
  | Bits
  | Zero
  | One
  | Builtin of X86_64_encoder.x87_constant
  | Signed_integer

type conversion = To_float | To_integer

type conversion_entry = {
  conversion_instruction : S.Instruction_id.t;
  conversion_result : S.Value_id.t;
  conversion : conversion;
  deferred : bool;
  elided : bool;
}

type intrinsic_entry = {
  intrinsic_instruction : S.Instruction_id.t;
  intrinsic_input : S.Value_id.t;
  intrinsic_result : S.Value_id.t;
  intrinsic_conversion : Ir.Numeric_intrinsic.conversion;
}

type operation =
  | Normalized of Ir.Numeric_normalization.t
  | Convert_to_float
  | Literal_bits of int64
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

type comparison_kind = Bitwise | Signed_word | Ordered
type comparison_link = Unlinked | Linked_left | Linked_right

type comparison_context =
  | Comparison_value
  | Comparison_branch of {
      role : Sema.Function_call_resolution.condition_role;
      negated : bool;
      consumer : S.Instruction_id.t;
    }

type comparison_entry = {
  comparison_instruction : S.Instruction_id.t;
  comparison_result : S.Value_id.t;
  comparison_operands : S.Value_id.t list;
  comparison_loads : load list;
  comparison_kind : comparison_kind;
  comparison_constant : int64 option;
  comparison_context : comparison_context;
  comparison_opcode : O.t;
  comparison_predicate : X86_64_encoder.condition;
  comparison_negate_result : bool;
  comparison_orientation : orientation;
  comparison_link : comparison_link;
  comparison_conversion_input : S.Value_id.t option;
  comparison_live_before : S.Value_id.t list;
  comparison_live_steps : S.Value_id.t list list;
  comparison_peak_depth : int;
}

type emission = {
  arithmetic_entries : entry list;
  conversion_entries : conversion_entry list;
  intrinsic_entries : intrinsic_entry list;
  comparison_entries : comparison_entry list;
}

type error = { message : string; span : Common.Span.t option }

type t = {
  graph : G.t;
  entries : (S.description * entry) I.t;
  peak : int;
  comparisons : (S.description * comparison_entry) I.t;
  conversions : conversion_entry I.t;
  intrinsics : intrinsic_entry I.t;
  valid : unit -> bool;
}

let is_f64 = Sema.Type.is_f64

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

let comparison_opcode = function
  | O.Ic_equ_equ
  | Ic_not_equ
  | Ic_less
  | Ic_greater_equ
  | Ic_greater
  | Ic_less_equ -> true
  | _ -> false

let ordered_comparison = function
  | O.Ic_less | Ic_greater_equ | Ic_greater | Ic_less_equ -> true
  | _ -> false

let invert_comparison = function
  | O.Ic_equ_equ -> O.Ic_not_equ
  | Ic_not_equ -> Ic_equ_equ
  | Ic_less -> Ic_greater_equ
  | Ic_greater_equ -> Ic_less
  | Ic_greater -> Ic_less_equ
  | Ic_less_equ -> Ic_greater
  | _ -> invalid_arg "not a comparison"

(* Source proof is deliberately separate from opcode recognition. Each maximal
   arithmetic subtree is re-lowered once at zero and once at its original IDs.
   Ordinary calls retain their checked target and saved-default snapshot during
   replay. Disjoint roots and a graph-derived node budget bound this work. *)
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

type entry_source = {
  lowered : Ir.Integer_program_lowering.t;
  globals : Ir.Integer_globals.t;
  value : Typed.expression_result;
}

let entry_source_matches source graph =
  Ir.X87_stack.graph (Ir.Integer_program_lowering.graph source.lowered) == graph
  && Ir.Integer_program_lowering.owns_expression source.lowered
       ~globals:source.globals ~value:source.value

let entry_expression_results graph value =
  let remaining =
    ref
      (64
      + 4
        * List.fold_left
            (fun count block ->
              count + List.length (S.instructions (G.instructions block)))
            0 (G.blocks graph))
  in
  let seen = Physical.create 16 in
  let rec collect depth result =
    if depth > 256 || !remaining <= 0 || Physical.mem seen result then []
    else (
      decr remaining;
      Physical.add seen result ();
      result
      :: (Option.fold ~none:[]
            ~some:(collect (depth + 1))
            (Typed.result_operand result)
         @ Option.fold ~none:[]
             ~some:(fun (left, right) ->
               collect (depth + 1) left @ collect (depth + 1) right)
             (Typed.result_binary_operands result)))
  in
  collect 0 value

let source_regions source entry_source runtime_context graph =
  let covered = ref I.empty in
  let comparisons = ref I.empty in
  let calls = Physical.create 16 in
  let call_source value =
    match Physical.find_opt calls value with
    | Some original -> original
    | None ->
        let original =
          Option.bind runtime_context (fun (runtime, owner) ->
              Ir.Runtime_call_context.original_expression_call_source runtime
                ~owner value)
        in
        Physical.add calls value original;
        original
  in
  let provided path =
    match path with
    | Typed.Provided_result value -> Some value
    | Declared_default_result _ -> None
  in
  let call_children = function
    | Ir.Runtime_call_context.Function_call target ->
        let call = Sema.Function_call_target_classification.source target in
        ( Typed.direct_source call
          |> Sema.Function_call_conversion_policy.direct_source
          |> Source.direct_source,
          List.filter_map
            (fun fixed -> provided (Typed.fixed_path fixed))
            (Typed.direct_fixed_results call)
          @ Typed.direct_variadic_results call )
    | Ir.Runtime_call_context.Top_level_call target ->
        let call =
          Sema.Top_level_function_call_target_classification.source target
        in
        ( Typed.top_level_direct_source call
          |> Sema.Top_level_expression_tree.call_source,
          List.filter_map
            (fun fixed -> provided (Typed.top_level_fixed_path fixed))
            (Typed.top_level_direct_fixed_results call)
          @ Typed.top_level_direct_variadic_results call )
    | Ir.Runtime_call_context.Callback_call call ->
        ( Ir.Callback_source.source call,
          Option.to_list (Ir.Callback_source.callee call)
          @ List.filter_map snd (Ir.Callback_source.fixed_arguments call)
          @ Ir.Callback_source.variadic_arguments call )
    | _ -> assert false
  in
  let selected =
    match source with
    | Some { runtime; body; frame }
      when Ir.Runtime_call_context.matches_graph runtime
             ~owner:(Ir.Runtime_call_context.Function body) graph
           && Ir.Function_body.definition_matches_frame body frame ->
        Option.map
          (fun original ->
            (Some frame, None, Typed.function_all_results original, false))
          (Ir.Function_body.definition_source body)
    | None -> (
        match entry_source with
        | Some source when entry_source_matches source graph ->
            Some
              ( None,
                Some source.globals,
                entry_expression_results graph source.value,
                false )
        | Some _ -> None
        | None ->
            Option.bind runtime_context (fun (runtime, owner) ->
                if
                  not
                    (Ir.Runtime_call_context.matches_graph runtime ~owner graph)
                then None
                else
                  Option.map
                    (fun values -> (None, None, values, true))
                    (Ir.Runtime_call_context.original_expression_results runtime
                       ~owner)))
    | Some _ -> None
  in
  Option.iter
    (fun (frame, globals, all, calls_only) ->
      let globals =
        match globals with
        | Some _ -> globals
        | None ->
            List.find_map
              (fun value ->
                match
                  Source.argument_expression_kind (Typed.result_source value)
                with
                | Source.Unresolved_expression Source.Call_expression ->
                    Option.map snd (call_source value)
                | _ -> None)
              all
      in
      let blocks =
        List.map
          (fun block ->
            Array.of_list
              (List.map S.description (S.instructions (G.instructions block))))
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
                  Source.argument_expression_kind (Typed.result_source value)
                with
                | Source.Float_literal _ | Source.Integer_literal _ -> true
                | Source.Unresolved_expression Source.Call_expression ->
                    Option.is_some (call_source value)
                | Bound_identifier_expression _ -> (
                    match
                      Option.map
                        (fun frame ->
                          Ir.Frame_address_lowering.prepare ~frame value)
                        frame
                    with
                    | Some (Ok (Some _)) -> true
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
                         (Source.binary_operator_name (Source.binary_operator b))
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
                (Typed.result_binary_operands value);
              Option.iter
                (fun (source, _) ->
                  let _, arguments = call_children source in
                  List.iter
                    (fun value -> Physical.replace children value ())
                    arguments)
                (match
                   Source.argument_expression_kind (Typed.result_source value)
                 with
                | Source.Unresolved_expression Source.Call_expression ->
                    call_source value
                | _ -> None));
            result
      in
      let terminal value =
        match Source.argument_expression_kind (Typed.result_source value) with
        | Source.Binary_expression binary
          when comparison_opcode (Source.binary_operator binary) -> (
            match Typed.result_binary_operands value with
            | Some (left, right) when pure 0 left && pure 0 right ->
                Physical.replace children left ();
                Physical.replace children right ();
                true
            | _ -> false)
        | _ -> pure 0 value
      in
      List.iter (fun value -> ignore (terminal value)) all;
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
      let rec lower_call ~instruction_id ~value_id value =
        let lowered =
          match call_source value with
          | Some (Ir.Runtime_call_context.Function_call target, _) ->
              Ir.Direct_call_lowering.lower ?frame ?globals ~lower_call
                ~optimize_shifts:true ~optimize_division:true ~instruction_id
                ~value_id ~target value
          | Some (Ir.Runtime_call_context.Top_level_call target, _) ->
              Ir.Direct_call_lowering.lower_top_level ?frame ?globals
                ~lower_call ~optimize_shifts:true ~optimize_division:true
                ~instruction_id ~value_id ~target value
          | Some (Ir.Runtime_call_context.Callback_call call, _) ->
              Ir.Direct_call_lowering.lower_callback ?frame ?globals ~lower_call
                ~optimize_shifts:true ~optimize_division:true ~instruction_id
                ~value_id ~call value
          | _ -> Ok Ir.Direct_call_lowering.Unsupported_call
        in
        Result.map
          (function
            | Ir.Direct_call_lowering.Lowered call ->
                Some (Ir.Direct_call_lowering.sequence call)
            | Unsupported_call -> None)
          lowered
      in
      let lower instruction_id value_id value =
        match
          Ir.Expression_lowering.lower_typed_result ?frame ?globals ~lower_call
            ~instruction_id ~value_id value
        with
        | Ok (Ir.Expression_lowering.Lowered result) ->
            Some
              (List.map S.description
                 (S.instructions (Ir.Expression_lowering.sequence result)))
        | _ -> None
      in
      let zero_i = S.Instruction_id.of_int 0 |> Result.get_ok in
      let zero_v = S.Value_id.of_int 0 |> Result.get_ok in
      let source_calls value =
        let seen = Physical.create 16 in
        let callees = ref [] in
        let rec collect depth value =
          if depth > 256 || Physical.mem seen value then None
          else (
            Physical.add seen value ();
            let selected, children =
              match call_source value with
              | Some (source, _) ->
                  let call, arguments = call_children source in
                  (match source with
                  | Ir.Runtime_call_context.Callback_call source ->
                      Option.iter
                        (fun callee -> callees := (call, callee) :: !callees)
                        (Ir.Callback_source.callee_source source)
                  | _ -> ());
                  (Some (call, Typed.result_source value), arguments)
              | None ->
                  ( None,
                    Option.to_list (Typed.result_operand value)
                    @ Option.fold ~none:[]
                        ~some:(fun (a, b) -> [ a; b ])
                        (Typed.result_binary_operands value) )
            in
            List.fold_left
              (fun collected child ->
                Option.bind collected (fun collected ->
                    Option.map
                      (fun nested -> collected @ nested)
                      (collect (depth + 1) child)))
              (Some (Option.to_list selected))
              children)
        in
        Option.map
          (fun pairs ->
            ( List.sort
                (fun (a, _) (b, _) ->
                  Int.compare (Source.call_index a) (Source.call_index b))
                pairs,
              !callees ))
          (collect 0 value)
      in
      let same_payload left right =
        match (left, right) with
        | None, None -> true
        | Some (S.Integer a), Some (S.Integer b)
        | Some (S.Float_bits a), Some (S.Float_bits b) -> a = b
        | Some (S.Bytes a), Some (S.Bytes b) -> a = b
        | Some (S.Block a), Some (S.Block b) -> a = b
        | Some (S.Block_targets a), Some (S.Block_targets b) -> a = b
        | Some (S.Symbol a), Some (S.Symbol b) -> a == b
        | Some (S.Callback a), Some (S.Callback b) -> a == b
        | Some (S.Member_projection a), Some (S.Member_projection b)
        | Some (S.Backing_projection a), Some (S.Backing_projection b) -> a == b
        | Some (S.Pointee_stride a), Some (S.Pointee_stride b) -> a == b
        | Some (S.Empty_frame_root a), Some (S.Empty_frame_root b) -> a == b
        | Some (S.Class_pointer_view (a, x)), Some (S.Class_pointer_view (b, y))
          -> a == b && x = y
        | Some (S.Retained_global a), Some (S.Retained_global b) -> a == b
        | Some (S.Saved_parameter_default a), Some (S.Saved_parameter_default b)
          -> a == b
        | Some (S.Saved_callback_default a), Some (S.Saved_callback_default b)
          -> a == b
        | _ -> false
      in
      let same_description (left : S.description) (right : S.description) =
        left.instruction_id = right.instruction_id
        && left.opcode = right.opcode
        && left.operands = right.operands
        && left.result = right.result
        && Option.equal Sema.Type.equal left.target_type right.target_type
        && same_payload left.payload right.payload
        && left.flags = right.flags && left.span = right.span
      in
      let region = ref 0 in
      List.iter
        (fun value ->
          if !budget >= 0 && terminal value && not (Physical.mem children value)
          then
            let expression = Typed.result_source value in
            match
              (Source.argument_expression_source expression, source_calls value)
            with
            | Some ast, Some (pairs, callees)
              when ((not calls_only) || pairs <> [])
                   && Source.validate_source_expression ~source:ast ~expression
                        ~calls:(List.map fst pairs) ~call_expressions:pairs
                        ~callee_expressions:callees ()
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
                      when List.length canonical <= Array.length block - index
                      -> (
                        let start = block.(index) in
                        let first_value =
                          let rec find offset = function
                            | [] -> None
                            | (expected : S.description) :: rest ->
                                if Option.is_some expected.result then
                                  block.(index + offset).result
                                else find (offset + 1) rest
                          in
                          find 0 canonical
                        in
                        match first_value with
                        | Some result -> (
                            match
                              lower start.instruction_id result.value_id value
                            with
                            | Some descriptions ->
                                let length = List.length descriptions in
                                let valid =
                                  List.mapi
                                    (fun offset expected ->
                                      let actual = block.(index + offset) in
                                      same_description expected actual
                                      || offset = length - 1
                                         && Int64.logand actual.flags
                                              (Int64.lognot 0x2003L)
                                            = Int64.logand expected.flags
                                                (Int64.lognot 0x2003L)
                                         && same_description
                                              {
                                                actual with
                                                flags = expected.flags;
                                              }
                                              expected)
                                    descriptions
                                  |> List.for_all Fun.id
                                in
                                if valid then (
                                  incr region;
                                  let last = List.hd (List.rev descriptions) in
                                  if comparison_opcode last.opcode then
                                    comparisons :=
                                      I.add last.instruction_id value
                                        !comparisons;
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
    selected;
  (!covered, !comparisons)

let initializer_conversion_source ~runtime ~owner graph description =
  let module R = Ir.Runtime_call_context in
  let module Initial = Ir.Global_initialization in
  let ( let* ) = Option.bind in
  let* initialization, region =
    R.original_initializer_region runtime ~owner description
  in
  let* original = Initial.load_source initialization region in
  let value, frame =
    match original with
    | Initial.Global_load root -> (Typed.top_level_root_value root, None)
    | Static_load (slot, root) ->
        ( Typed.initializer_value root,
          Some (Ir.Integer_globals.static_frame slot) )
  in
  let globals = Initial.globals initialization in
  let calls = Physical.create 16 in
  let call_source value =
    match Physical.find_opt calls value with
    | Some original -> original
    | None ->
        let original = R.original_expression_call_source runtime ~owner value in
        Physical.add calls value original;
        original
  in
  let provided = function
    | Typed.Provided_result value -> Some value
    | Declared_default_result _ -> None
  in
  let call_children = function
    | R.Function_call target ->
        let call = Sema.Function_call_target_classification.source target in
        ( Typed.direct_source call
          |> Sema.Function_call_conversion_policy.direct_source
          |> Source.direct_source,
          List.filter_map
            (fun fixed -> provided (Typed.fixed_path fixed))
            (Typed.direct_fixed_results call)
          @ Typed.direct_variadic_results call )
    | Top_level_call target ->
        let call =
          Sema.Top_level_function_call_target_classification.source target
        in
        ( Typed.top_level_direct_source call
          |> Sema.Top_level_expression_tree.call_source,
          List.filter_map
            (fun fixed -> provided (Typed.top_level_fixed_path fixed))
            (Typed.top_level_direct_fixed_results call)
          @ Typed.top_level_direct_variadic_results call )
    | Callback_call call ->
        ( Ir.Callback_source.source call,
          Option.to_list (Ir.Callback_source.callee call)
          @ List.filter_map snd (Ir.Callback_source.fixed_arguments call)
          @ Ir.Callback_source.variadic_arguments call )
    | Function_output _ | Top_level_output _ -> assert false
  in
  let seen = Physical.create 16 in
  let callees = ref [] in
  let rec collect depth value =
    if depth > 256 || Physical.mem seen value then None
    else (
      Physical.add seen value ();
      let selected, children =
        match call_source value with
        | Some (source, _) ->
            let call, arguments = call_children source in
            (match source with
            | R.Callback_call source ->
                Option.iter
                  (fun callee -> callees := (call, callee) :: !callees)
                  (Ir.Callback_source.callee_source source)
            | _ -> ());
            (Some (call, Typed.result_source value), arguments)
        | None ->
            ( None,
              Option.to_list (Typed.result_operand value)
              @ Option.fold ~none:[]
                  ~some:(fun (left, right) -> [ left; right ])
                  (Typed.result_binary_operands value) )
      in
      List.fold_left
        (fun collected child ->
          let* collected = collected in
          let* nested = collect (depth + 1) child in
          Some (collected @ nested))
        (Some (Option.to_list selected))
        children)
  in
  let* pairs = collect 0 value in
  let pairs =
    List.sort
      (fun (left, _) (right, _) ->
        Int.compare (Source.call_index left) (Source.call_index right))
      pairs
  in
  let expression = Typed.result_source value in
  let* ast = Source.argument_expression_source expression in
  let* () =
    if
      Source.validate_source_expression ~source:ast ~expression
        ~calls:(List.map fst pairs) ~call_expressions:pairs
        ~callee_expressions:!callees ()
      = Ok ()
    then Some ()
    else None
  in
  let rec lower_call ~instruction_id ~value_id value =
    let lowered =
      match call_source value with
      | Some (R.Function_call target, _) ->
          Ir.Direct_call_lowering.lower ?frame ~globals ~lower_call
            ~optimize_shifts:true ~optimize_division:true ~instruction_id
            ~value_id ~target value
      | Some (Top_level_call target, _) ->
          Ir.Direct_call_lowering.lower_top_level ?frame ~globals ~lower_call
            ~optimize_shifts:true ~optimize_division:true ~instruction_id
            ~value_id ~target value
      | Some (Callback_call call, _) ->
          Ir.Direct_call_lowering.lower_callback ?frame ~globals ~lower_call
            ~optimize_shifts:true ~optimize_division:true ~instruction_id
            ~value_id ~call value
      | _ -> Ok Ir.Direct_call_lowering.Unsupported_call
    in
    Result.map
      (function
        | Ir.Direct_call_lowering.Lowered call ->
            Some (Ir.Direct_call_lowering.sequence call)
        | Unsupported_call -> None)
      lowered
  in
  let first = Initial.storage_first region in
  let last = Initial.storage_last region in
  let* actual =
    match
      G.blocks graph
      |> List.filter_map (fun block ->
          let items =
            S.instructions (G.instructions block)
            |> List.map S.description
            |> List.filter (fun item ->
                S.Instruction_id.compare item.S.instruction_id first >= 0
                && S.Instruction_id.compare item.instruction_id last <= 0)
          in
          if items = [] then None else Some items)
    with
    | [ items ] -> Some items
    | _ -> None
  in
  let* first_value =
    match actual with
    | first :: _ -> Option.map (fun r -> r.S.value_id) first.result
    | [] -> None
  in
  let lowered =
    match original with
    | Initial.Global_load root ->
        Ir.Expression_lowering.lower_global_initializer ~globals ~lower_call
          ~optimize_shifts:true ~optimize_division:true ~instruction_id:first
          ~value_id:first_value root
    | Static_load (slot, root) ->
        Ir.Expression_lowering.lower_static_initializer ~globals ~root
          ~lower_call ~optimize_shifts:true ~optimize_division:true
          ~instruction_id:first ~value_id:first_value slot
  in
  let* expected =
    match lowered with
    | Ok (Ir.Expression_lowering.Lowered lowered) ->
        Some
          (S.instructions (Ir.Expression_lowering.sequence lowered)
          |> List.map S.description)
    | _ -> None
  in
  let same_payload left right =
    match (left, right) with
    | None, None -> true
    | Some (S.Integer left), Some (S.Integer right)
    | Some (S.Float_bits left), Some (S.Float_bits right) -> left = right
    | Some (S.Bytes left), Some (S.Bytes right) -> left = right
    | Some (S.Symbol left), Some (S.Symbol right) -> left == right
    | Some (S.Callback left), Some (S.Callback right) -> left == right
    | Some (S.Member_projection left), Some (S.Member_projection right)
    | Some (S.Backing_projection left), Some (S.Backing_projection right) ->
        left == right
    | Some (S.Pointee_stride left), Some (S.Pointee_stride right) ->
        left == right
    | Some (S.Empty_frame_root left), Some (S.Empty_frame_root right) ->
        left == right
    | ( Some (S.Class_pointer_view (left, x)),
        Some (S.Class_pointer_view (right, y)) ) -> left == right && x = y
    | Some (S.Retained_global left), Some (S.Retained_global right) ->
        left == right
    | ( Some (S.Saved_parameter_default left),
        Some (S.Saved_parameter_default right) ) -> left == right
    | ( Some (S.Saved_callback_default left),
        Some (S.Saved_callback_default right) ) -> left == right
    | _ -> false
  in
  let same (left : S.description) (right : S.description) =
    left.instruction_id = right.instruction_id
    && left.opcode = right.opcode
    && left.operands = right.operands
    && left.result = right.result
    && Option.equal Sema.Type.equal left.target_type right.target_type
    && same_payload left.payload right.payload
    && left.flags = right.flags && left.span = right.span
  in
  match List.rev actual with
  | ending :: reversed
    when ending.instruction_id = last
         && ending.opcode = O.Ic_end_exp
         && List.length reversed = List.length expected
         && List.for_all2 same expected (List.rev reversed)
         && List.exists (( == ) description) reversed -> Some (region, value)
  | _ -> None

let prepare ?source ?entry_source ?runtime ?owner graph =
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
        Option.fold ~none:false
          ~some:(Int64.equal (Ir.Function_body.compiler_options s.body))
          (Ir.Function_body.definition_compiler_options s.body)
        && Option.fold ~none:false
             ~some:(fun original ->
               List.for_all
                 (fun condition ->
                   Typed.result_source (Typed.condition_value condition)
                   == Source.condition_expression
                        (Typed.condition_source condition))
                 (Typed.function_conditions original)
               && Option.fold ~none:false
                    ~some:
                      (Int64.equal (Ir.Function_body.compiler_options s.body))
                    (Typed.function_compilation_options original))
             (Ir.Function_body.definition_source s.body)
        && Ir.Runtime_call_context.matches_graph s.runtime
             ~owner:(Ir.Runtime_call_context.Function s.body) graph
        && Ir.Register_variable_analysis.authenticates_mixed_scalar_source
             ~runtime:s.runtime ~frame:s.frame ~body:s.body
    | None ->
        runtime_valid ()
        && Option.fold ~none:false
             ~some:(fun source -> entry_source_matches source graph)
             entry_source
  in
  let authenticated = authentic_source () in
  let regions, comparison_sources =
    source_regions source entry_source runtime_context graph
  in
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
  let normalizations =
    match source with
    | Some s ->
        Ir.Numeric_normalization.prepare ~runtime:s.runtime ~body:s.body
          ~frame:s.frame graph
    | None -> []
  in
  let normalization (d : S.description) =
    List.find_opt
      (fun n -> Ir.Numeric_normalization.instruction n = d.instruction_id)
      normalizations
  in
  let errors = ref [] in
  let entries = ref I.empty in
  let comparisons = ref I.empty in
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
  let canonical_consumer = ref false in
  let option_consumer = ref false in
  let arithmetic_arguments = ref [] in
  let intrinsic_instruction id =
    Option.bind runtime_context (fun (runtime, owner) ->
        if runtime_valid () then
          Ir.Runtime_call_context.find_intrinsic_instruction runtime ~owner id
        else None)
  in
  let materialized_intrinsic_result receipt (producer : S.description) =
    let module R = Ir.Runtime_call_context in
    producer.opcode = O.Ic_call_end
    && producer.flags = 0L && producer.operands = []
    && producer.instruction_id = R.intrinsic_last receipt
    && Option.equal Sema.Type.equal producer.target_type
         (Some (R.intrinsic_return_type receipt))
    && Option.fold ~none:false
         ~some:(fun (result : S.value_definition) ->
           result.value_id = R.intrinsic_result_value receipt)
         producer.result
    &&
    match producer.payload with
    | Some (S.Symbol symbol) -> symbol == R.intrinsic_symbol receipt
    | _ -> false
  in
  List.iter
    (fun block ->
      List.iter
        (fun instruction ->
          let d = S.description instruction in
          match Ir.Numeric_intrinsic.conversion d.opcode with
          | None -> ()
          | Some direction -> (
              let receipt = intrinsic_instruction d.instruction_id in
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
                      let arithmetic_input =
                        match producer input with
                        | Some p ->
                            direction = Ir.Numeric_intrinsic.To_integer
                            && Option.is_some (arithmetic p.opcode)
                            && I.mem p.instruction_id regions
                            && Int64.logand p.flags 3L = 0L
                        | None -> false
                      in
                      let nested_intrinsic =
                        Option.bind (producer input) (fun (p : S.description) ->
                            Option.bind runtime_context (fun (runtime, owner) ->
                                R.find_intrinsic_end runtime ~owner
                                  p.instruction_id))
                      in
                      let nested_input =
                        match (nested_intrinsic, producer input) with
                        | Some nested, Some p ->
                            Ir.Numeric_intrinsic.supports
                              (R.intrinsic_opcode nested)
                            && materialized_intrinsic_result nested p
                            && R.argument_producer argument = p.instruction_id
                            && R.intrinsic_producer_matches receipt p
                            && Sema.Type.equal
                                 (R.intrinsic_return_type nested)
                                 source_type
                        | _ -> false
                      in
                      let simple_input =
                        (Option.is_none nested_intrinsic || nested_input)
                        &&
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
                      let result_producer = producer result in
                      let numeric_consumer (consumer : S.description) =
                        match
                          ( intrinsic_instruction consumer.instruction_id,
                            result_producer )
                        with
                        | Some next, Some p
                          when Ir.Numeric_intrinsic.supports
                                 (R.intrinsic_opcode next)
                               && consumer.opcode = R.intrinsic_opcode next
                               && consumer.operands = [ result ]
                               && R.intrinsic_producer_matches next p -> (
                            match R.intrinsic_arguments next with
                            | [ argument ] ->
                                R.argument_value argument = result
                                && R.argument_producer argument
                                   = p.instruction_id
                                && Sema.Type.equal
                                     (R.argument_source_type argument)
                                     (R.intrinsic_return_type receipt)
                            | _ -> false)
                        | _ -> false
                      in
                      (* OptPass3:444-505 does not select a floating link for
                         explicit TO_I64/TO_F64. BackB:277-286 invokes ICFCvt at
                         CN_INST; the unflagged nested CALL_END therefore keeps
                         its materialized word. Each side needs its own original
                         call receipt, including the exact argument producer. *)
                      let boundary =
                        List.for_all
                          (fun (consumer : S.description) ->
                            List.mem consumer.opcode
                              [ O.Ic_assign; Ic_return_val; Ic_end_exp ]
                            || numeric_consumer consumer
                            || authenticated
                               && Option.is_some (arithmetic consumer.opcode)
                               && Option.fold ~none:false
                                    ~some:
                                      (if
                                         direction
                                         = Ir.Numeric_intrinsic.To_float
                                       then is_f64
                                       else integer_type)
                                    consumer.target_type
                               && (direction = Ir.Numeric_intrinsic.To_float
                                  || Int64.logand consumer.flags 3L = 0L)
                               && List.for_all
                                    (fun id ->
                                      id = result
                                      || Option.fold ~none:false
                                           ~some:(fun (p : S.description) ->
                                             List.mem p.opcode
                                               (if
                                                  direction
                                                  = Ir.Numeric_intrinsic
                                                    .To_float
                                                then [ O.Ic_deref; Ic_imm_f64 ]
                                                else [ O.Ic_deref; Ic_imm_i64 ])
                                             && Int64.logand p.flags 3L = 0L
                                             && Option.fold ~none:false
                                                  ~some:
                                                    (if
                                                       direction
                                                       = Ir.Numeric_intrinsic
                                                         .To_float
                                                     then is_f64
                                                     else integer_type)
                                                  p.target_type)
                                           (producer id))
                                    consumer.operands)
                          (users result)
                      in
                      if
                        (not family)
                        || ((not simple_input) && not arithmetic_input)
                        || (not boundary)
                        || not
                             (Option.fold ~none:false
                                ~some:(materialized_intrinsic_result receipt)
                                result_producer)
                      then
                        fail d
                          "numeric intrinsic requires an original materialized \
                           scalar boundary"
                      else (
                        if
                          List.exists
                            (fun (consumer : S.description) ->
                              not
                                (List.mem consumer.opcode
                                   [ O.Ic_assign; Ic_return_val; Ic_end_exp ]
                                || numeric_consumer consumer))
                            (users result)
                        then canonical_consumer := true;
                        if arithmetic_input then
                          arithmetic_arguments :=
                            (d, input) :: !arithmetic_arguments;
                        intrinsics :=
                          I.add d.instruction_id
                            {
                              intrinsic_instruction = d.instruction_id;
                              intrinsic_input = input;
                              intrinsic_result = result;
                              intrinsic_conversion = direction;
                            }
                            !intrinsics)
                  | _ ->
                      fail d "numeric intrinsic lost its exact unary argument")
              | None ->
                  fail d
                    "numeric intrinsic lacks its sealed original declaration"))
        (S.instructions (G.instructions block)))
    (G.blocks graph);
  let conversions = ref I.empty in
  let call_conversions = ref I.empty in
  let initializer_conversions = ref I.empty in
  let initializer_comparisons = ref I.empty in
  let original_initializer_conversion description =
    Option.bind runtime_context (fun (runtime, owner) ->
        if runtime_valid () then
          initializer_conversion_source ~runtime ~owner graph description
        else None)
  in
  let original_call_conversion (d : S.description) =
    Option.bind runtime_context (fun (runtime, owner) ->
        if not (runtime_valid ()) then None
        else
          Option.bind
            (Ir.Runtime_call_context.original_call_result_conversion runtime
               ~owner d) (fun original ->
              let flags =
                match
                  Ir.Runtime_call_context.call_result_conversion_kind original
                with
                | Typed.No_intrinsic_conversion -> 0L
                | Result_to_f64 -> 1L
                | Result_to_int -> 2L
              in
              if flags <> 0L && Int64.logand d.flags 3L = flags then
                Some original
              else None))
  in
  V.iter
    (fun id (d : S.description) ->
      let bits = Int64.logand d.flags 3L in
      if bits <> 0L then
        let direction = if bits = 1L then To_float else To_integer in
        let call_source = original_call_conversion d in
        let initializer_source = original_initializer_conversion d in
        if
          (not authenticated) && Option.is_none call_source
          && Option.is_none initializer_source
          || bits = 3L
          || not
               (Option.fold ~none:false
                  ~some:(if direction = To_float then integer_type else is_f64)
                  d.target_type)
        then
          fail d "numeric conversion has no exact original scalar source proof"
        else (
          Option.iter
            (fun original ->
              call_conversions :=
                I.add d.instruction_id (d, original) !call_conversions)
            call_source;
          Option.iter
            (fun original ->
              initializer_conversions :=
                I.add d.instruction_id (d, original) !initializer_conversions)
            initializer_source;
          let deferred =
            match users id with
            | [ consumer ]
              when d.opcode = O.Ic_deref && direction = To_float
                   && Int64.logand d.flags 0x2000L = 0L
                   && (Option.is_some (arithmetic consumer.opcode)
                       && Option.fold ~none:false ~some:is_f64
                            consumer.target_type
                      || ordered_comparison consumer.opcode
                         && I.mem consumer.instruction_id comparison_sources) ->
                true
            | _ -> false
          in
          conversions :=
            I.add d.instruction_id
              {
                conversion_instruction = d.instruction_id;
                conversion_result = id;
                conversion = direction;
                deferred;
                elided = false;
              }
              !conversions))
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
  let negative_zero_literal id =
    match producer id with
    | Some
        {
          opcode = O.Ic_unary_minus;
          operands = [ input ];
          flags = 0L;
          target_type = Some type_;
          instruction_id;
          _;
        }
      when is_f64 type_
           && Option.fold ~none:false
                ~some:(fun region ->
                  I.exists
                    (fun comparison _ ->
                      I.find_opt comparison regions = Some region)
                    comparison_sources)
                (I.find_opt instruction_id regions) -> (
        match producer input with
        | Some
            {
              opcode = O.Ic_imm_f64;
              payload = Some (S.Float_bits 0L);
              flags = 0L;
              _;
            } -> true
        | _ -> false)
    | _ -> false
  in
  let literal id =
    if negative_zero_literal id then Some Int64.min_int
    else
      match producer id with
      | Some { opcode = O.Ic_imm_f64; payload = Some (S.Float_bits bits); _ } ->
          Some bits
      | Some d
        when Option.fold ~none:false
               ~some:(fun c -> c.conversion = To_float)
               (conversion d) ->
          Option.map Common.Binary64_conversion.of_signed_word
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
      | Some 0L -> Zero
      | Some 0x3ff0000000000000L -> One
      | Some
          (( 0x400921fb54442d18L
           | 0x400a934f0979a371L
           | 0x3ff71547652b82feL
           | 0x3fd34413509f79ffL
           | 0x3fe62e42fefa39efL ) as bits) ->
          if not authenticated then (
            fail d
              "native F64 arithmetic builtin constants require original \
               compiler-option evidence";
            Bits)
          else (
            option_consumer := true;
            (* OptPass012 folds literal operands from their binary64 data
               before backend builtin selection. Recomputing an isolated fold
               must load those words, not introduce an extended constant.
               Linked folding/reassociation still needs its separate proof. *)
            if
              List.for_all
                (fun operand -> Option.is_some (literal operand))
                d.operands
              || Option.fold ~none:false
                   ~some:(fun s ->
                     Sema.Compiler_option.is_enabled
                       ~mask:(Ir.Function_body.compiler_options s.body)
                       Sema.Compiler_option.No_builtin_const)
                   source
            then Bits
            else
              Builtin
                (match bits with
                | 0x400921fb54442d18L -> X86_64_encoder.Fpi
                | 0x400a934f0979a371L -> Flog2_10
                | 0x3ff71547652b82feL -> Flog2_e
                | 0x3fd34413509f79ffL -> Flog10_2
                | _ -> Floge_2))
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
          let touches_float =
            target_float || List.exists float_value d.operands
          in
          if touches_float then (
            (* Conversion, comparison and suppression flags are not a source link
         producer. A forged or merely depth-balanced suppression cannot admit
         an extended value. PUSH and END_EXP are transport flags only. *)
            let allowed_flags =
              if d.opcode = O.Ic_end_exp then 0x200L
              else if comparison_opcode d.opcode then
                if
                  I.mem d.instruction_id !initializer_conversions
                  && Int64.logand d.flags 3L = 1L
                then 0x2041L
                else 0x2040L
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
                  if
                    Option.fold ~none:false
                      ~some:(fun (result : S.value_definition) ->
                        negative_zero_literal result.value_id)
                      d.result
                  then Literal_bits Int64.min_int
                  else
                    match (operation, d.operands) with
                    | Add, [ l; _ ] when literal l = Some 0L -> Copy_right
                    | (Add | Subtract), [ _; r ] when literal r = Some 0L ->
                        Copy_left
                    | Multiply, [ l; _ ]
                      when literal l = Some 0x3ff0000000000000L -> Copy_right
                    | (Multiply | Divide), [ _; r ]
                      when literal r = Some 0x3ff0000000000000L -> Copy_left
                    | _ -> operation
                in
                let operation =
                  if
                    (Option.is_some (conversion d)
                    || List.mem Signed_integer loads)
                    && List.mem operation [ Copy_left; Copy_right ]
                    && Option.is_none (normalization d)
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
                let operation, operands, loads =
                  match normalization d with
                  | Some receipt ->
                      ( Normalized receipt,
                        [ Ir.Numeric_normalization.input receipt ],
                        [
                          (match
                             (List.hd (Ir.Numeric_normalization.states receipt))
                               .input_class
                           with
                          | Integer64 -> Signed_integer
                          | Float64 -> Bits);
                        ] )
                  | None -> (operation, d.operands, loads)
                in
                let depth =
                  match (operation, loads) with
                  | ( ( Copy_left
                      | Copy_right
                      | Alias_left
                      | Alias_right
                      | Literal_bits _ ),
                      _ ) -> 0
                  | (Negate | Normalized _), _ -> 1
                  | _, [ Bits; _ ] | _, [ _; Bits ] -> 1
                  | _ -> 2
                in
                match d.result with
                | None ->
                    fail d
                      "native F64 arithmetic has no original result identity"
                | Some result ->
                    let orientation, live_steps =
                      match (operation, operands, loads) with
                      | Normalized receipt, [ input ], _ ->
                          let owner = ref input in
                          let steps = ref [ [] ] in
                          List.iter
                            (function
                              | Ir.Numeric_normalization.Load_signed_integer
                              | Load_binary64 ->
                                  steps := !steps @ [ [ !owner ] ]
                              | Store_binary64 | Store_integer_truncate ->
                                  steps := !steps @ [ [] ];
                                  owner := result.value_id
                              | Copy_binary64 -> owner := result.value_id)
                            (Ir.Numeric_normalization.events receipt);
                          (Left_first, !steps)
                      | ( ( Copy_left
                          | Copy_right
                          | Alias_left
                          | Alias_right
                          | Literal_bits _ ),
                          _,
                          _ ) -> (Bit_copy, [ [] ])
                      | Negate, [ input ], _ ->
                          ( Left_first,
                            [ []; [ input ]; [ result.value_id ]; [] ] )
                      | ( _,
                          [ _; right ],
                          [ Bits; (Zero | One | Builtin _ | Signed_integer) ] )
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
                        operands;
                        loads;
                        operation;
                        live_before = [];
                        live_after = [];
                        peak_depth = depth;
                        orientation;
                        live_steps;
                        result_conversion =
                          (match operation with
                          | Normalized _ -> None
                          | _ ->
                              Option.map (fun c -> c.conversion) (conversion d));
                      }
                    in
                    entries := I.add d.instruction_id (d, entry) !entries;
                    peak := max !peak depth)
            | None -> (
                match d.opcode with
                | opcode when comparison_opcode opcode -> ()
                | (O.Ic_to_i64 | Ic_to_f64)
                  when I.mem d.instruction_id !intrinsics -> ()
                | O.Ic_holyc_typecast
                  when target_float && Option.is_none (conversion d) -> (
                    (* OptPass012.HC:87-110 retains the word while replacing
                       its class. Only an already materialized operand may use
                       the word-copy emitter; this grants no extended link
                       through a cast around an arithmetic producer. *)
                    match (d.operands, d.result, d.payload) with
                    | [ input ], Some _, Some (S.Integer (0L | 1L))
                      when float_value input
                           && Option.fold ~none:false
                                ~some:(fun input ->
                                  List.mem input.S.opcode
                                    [ O.Ic_imm_f64; Ic_deref; Ic_call_end ]
                                  && Option.is_none (conversion input))
                                (producer input) -> ()
                    | _ ->
                        fail d
                          "native F64 typecast requires an already \
                           materialized scalar F64 word")
                | O.Ic_imm_i64
                | O.Ic_imm_f64
                | Ic_deref
                | Ic_assign
                | Ic_call
                (* The callable backend checks the original callback scope;
                   saving and calling its raw words grants no x87 link. *)
                | Ic_push_regs
                | Ic_call_indirect
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
                    && Option.fold ~none:false ~some:is_f64 consumer.target_type
                    || ordered_comparison consumer.opcode)
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
  let constant_comparison opcode left right =
    (* OptLib:124-139,161-166 converts immediate integer operands to binary64
       before OptPass012:725-822 replaces the comparison by IC_IMM_I64.
       Equality compares the stored words. Ordered predicates use the pinned
       value-comparison templates, including their unordered result. *)
    let magnitude bits = Int64.logand bits Int64.max_int in
    let nan bits =
      Int64.unsigned_compare (magnitude bits) 0x7ff0000000000000L > 0
    in
    let unordered = nan left || nan right in
    let order =
      if magnitude left = 0L && magnitude right = 0L then 0
      else if left < 0L <> (right < 0L) then if left < 0L then -1 else 1
      else
        let magnitude_order =
          Int64.unsigned_compare (magnitude left) (magnitude right)
        in
        if left < 0L then -magnitude_order else magnitude_order
    in
    let result =
      match opcode with
      | O.Ic_equ_equ -> left = right
      | Ic_not_equ -> left <> right
      | Ic_less -> unordered || order < 0
      | Ic_greater_equ -> (not unordered) && order >= 0
      | Ic_greater -> unordered || order > 0
      | Ic_less_equ -> (not unordered) && order <= 0
      | _ -> assert false
    in
    if result then 1L else 0L
  in
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
        (d.opcode = O.Ic_imm_f64 || d.opcode = O.Ic_deref
        || negative_zero_literal (normal id))
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
  (* A comparison is a terminal consumer, never a floating result producer.
     Its source root, rather than adjacency to BR_ZERO, selects the branch
     template. The graph seal authenticates the branch edge and its target. *)
  let original_conditions =
    Option.fold ~none:[]
      ~some:(fun s ->
        Option.fold ~none:[] ~some:Typed.function_conditions
          (Ir.Function_body.definition_source s.body))
      source
  in
  let rec condition_root depth negated value =
    if depth > 256 then None
    else
      match Source.argument_expression_kind (Typed.result_source value) with
      | Source.Parenthesized_expression _ ->
          Option.bind
            (Typed.result_operand value)
            (condition_root (depth + 1) negated)
      | Source.Prefix_expression p -> (
          match Source.prefix_operator p with
          | Source.Logical_not ->
              Option.bind
                (Typed.result_operand value)
                (condition_root (depth + 1) (not negated))
          | Source.Unary_plus ->
              Option.bind
                (Typed.result_operand value)
                (condition_root (depth + 1) negated)
          | _ -> None)
      | Source.Binary_expression b
        when comparison_opcode (Source.binary_operator b) ->
          Some (value, negated)
      | _ -> None
  in
  let rec condition_contains depth original value =
    depth > 256 || value == original
    || Option.fold ~none:false
         ~some:(condition_contains (depth + 1) original)
         (Typed.result_operand value)
    || Option.fold ~none:false
         ~some:(fun (left, right) ->
           condition_contains (depth + 1) original left
           || condition_contains (depth + 1) original right)
         (Typed.result_binary_operands value)
  in
  let context result original =
    let conditions =
      List.filter_map
        (fun c ->
          match condition_root 0 false (Typed.condition_value c) with
          | Some (root, negated) when root == original -> Some (c, negated)
          | _ -> None)
        original_conditions
    in
    match (conditions, users result) with
    | [ (condition, negated) ], [ consumer ]
      when consumer.opcode = O.Ic_br_zero
           && consumer.operands = [ result ]
           && consumer.flags = 0L && consumer.result = None
           && consumer.target_type = None
           && (consumer.span
              =
              match Typed.result_origin original with
              | Sema.Symbol.Source_location location -> Some location.span
              | _ -> None)
           &&
           match consumer.payload with
           | Some (S.Block _) -> true
           | _ -> false ->
        Some
          (Comparison_branch
             {
               role = Source.condition_role (Typed.condition_source condition);
               negated;
               consumer = consumer.instruction_id;
             })
    | [], (_ :: _ as consumers)
      when (not
              (List.exists
                 (fun c ->
                   condition_contains 0 original (Typed.condition_value c))
                 original_conditions))
           && List.for_all
                (fun (consumer : S.description) ->
                  List.mem consumer.opcode
                    [ O.Ic_assign; Ic_return_val; Ic_end_exp ]
                  || List.mem consumer.opcode
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
                       ]
                     && Int64.logand consumer.flags 3L = 0L
                     && Option.fold ~none:false ~some:integer_type
                          consumer.target_type)
                consumers -> Some Comparison_value
    | _ -> None
  in
  let initializer_comparison (d : S.description) original =
    let rec selected depth value =
      if depth > 256 then false
      else if value == original then true
      else
        match Source.argument_expression_kind (Typed.result_source value) with
        | Source.Parenthesized_expression _ ->
            Option.fold ~none:false
              ~some:(selected (depth + 1))
              (Typed.result_operand value)
        | Prefix_expression prefix
          when Source.prefix_operator prefix = Source.Unary_plus ->
            Option.fold ~none:false
              ~some:(selected (depth + 1))
              (Typed.result_operand value)
        | _ -> false
    in
    match
      (I.find_opt d.instruction_id !initializer_conversions, conversion d)
    with
    | Some (_, (_, value)), Some receipt
      when receipt.conversion = To_float && Int64.logand d.flags 3L = 1L ->
        selected 0 value
    | _ -> false
  in
  V.iter
    (fun _ (d : S.description) ->
      if comparison_opcode d.opcode && List.exists float_value d.operands then
        match
          (d.result, d.operands, I.find_opt d.instruction_id comparison_sources)
        with
        | Some result, [ left; right ], Some original
          when ((authenticated && Int64.logand d.flags 3L = 0L)
               || initializer_comparison d original)
               && Int64.logand d.flags 0x40L = 0x40L
               && d.payload = None
               && Option.fold ~none:false
                    ~some:(Sema.Type.equal (conversion_type To_integer))
                    d.target_type
               && List.for_all float_value d.operands -> (
            match context result.value_id original with
            | None ->
                fail d
                  "F64 comparison lacks its original terminal consumer context"
            | Some comparison_context ->
                if initializer_comparison d original then
                  initializer_comparisons :=
                    I.add d.instruction_id original !initializer_comparisons;
                let direct_public_float id =
                  match producer id with
                  | Some p ->
                      p.opcode = O.Ic_deref
                      && Option.is_none (conversion p)
                      && Option.fold ~none:false
                           ~some:(fun t ->
                             Sema.Type.pointer_depth t = 0
                             && Sema.Type.base t
                                = Sema.Type.Primitive
                                    ( Sema.Type.Public_spelling,
                                      Sema.Primitive_type.F64 ))
                           p.target_type
                  | None -> false
                in
                let immediate_zero id =
                  match producer id with
                  | Some
                      {
                        opcode = O.Ic_imm_f64;
                        payload = Some (S.Float_bits 0L);
                        _;
                      } -> true
                  | Some
                      ({
                         opcode = O.Ic_imm_i64;
                         payload = Some (S.Integer 0L);
                         _;
                       } as p) ->
                      Option.fold ~none:false
                        ~some:(fun c -> c.conversion = To_float)
                        (conversion p)
                  | _ -> false
                in
                let zero_candidate =
                  List.mem d.opcode [ O.Ic_less; Ic_greater_equ ]
                  && (normal_literal left = Some 0L
                      && direct_public_float (normal right)
                     || normal_literal right = Some 0L
                        && direct_public_float (normal left))
                in
                if
                  List.mem d.opcode [ O.Ic_less; Ic_greater_equ ]
                  && ((normal_literal left = Some 0L && deferred right)
                     || (normal_literal right = Some 0L && deferred left))
                then
                  fail d
                    "mixed integer zero comparison requires source \
                     conversion-phase normalization";
                let signed_zero =
                  zero_candidate
                  && ((immediate_zero left && direct_public_float right)
                     || (immediate_zero right && direct_public_float left))
                in
                if zero_candidate && not signed_zero then
                  fail d
                    "F64 comparison requires unsupported source zero \
                     normalization";
                let kind =
                  if signed_zero then Signed_word
                  else if ordered_comparison d.opcode then Ordered
                  else Bitwise
                in
                (* OptFixupBinaryOp1 changes all three classes to I64 before
                   OptPass3. The source's == typo leaves the immediate opcode
                   unchanged, but pass4 transports its zero word unchanged.
                   Elide only the canonical integer-zero conversion, whose bits
                   already equal the final word. *)
                if signed_zero then
                  List.iter
                    (fun id ->
                      if immediate_zero id then
                        Option.iter
                          (fun p ->
                            Option.iter
                              (fun c ->
                                conversions :=
                                  I.add p.S.instruction_id
                                    { c with elided = true } !conversions)
                              (conversion p))
                          (producer id))
                    d.operands;
                let constant =
                  match (literal left, literal right) with
                  | Some left, Some right ->
                      Some (constant_comparison d.opcode left right)
                  | _ ->
                      if
                        Option.is_some (normal_literal left)
                        && Option.is_some (normal_literal right)
                      then
                        fail d
                          "F64 comparison requires unsupported source constant \
                           normalization";
                      None
                in
                let region = I.find d.instruction_id regions in
                if
                  List.exists
                    (fun id -> Option.is_some (entry_of id))
                    d.operands
                then Hashtbl.replace linked_regions region ();
                let linked =
                  if
                    Option.is_some constant || kind <> Ordered || deferred left
                    || deferred right
                  then None
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
                  linked;
                let conversion_input =
                  if Option.is_some constant || kind <> Ordered then None
                  else if deferred left then Some left
                  else if deferred right then Some right
                  else None
                in
                if deferred left && deferred right then
                  fail d
                    "F64 comparison requires an unproved pair of argument \
                     conversions";
                let semantic_link =
                  match conversion_input with
                  | Some _ -> conversion_input
                  | None -> linked
                in
                let link =
                  match semantic_link with
                  | Some id when id = left -> Linked_left
                  | Some _ -> Linked_right
                  | None -> Unlinked
                in
                let branch, negate_result =
                  match comparison_context with
                  | Comparison_value -> (false, false)
                  | Comparison_branch { role; negated; _ } ->
                      (true, role <> Source.Do_while_condition <> negated)
                in
                let opcode =
                  if negate_result then invert_comparison d.opcode else d.opcode
                in
                let predicate, orientation =
                  let open X86_64_encoder in
                  if kind = Bitwise then
                    ((if opcode = O.Ic_equ_equ then E else NE), Bit_copy)
                  else if kind = Signed_word then
                    ((if opcode = O.Ic_less then L else GE), Bit_copy)
                  else if link = Linked_right then
                    ( (match opcode with
                      | O.Ic_less -> B
                      | Ic_greater_equ -> AE
                      | Ic_greater -> A
                      | Ic_less_equ -> BE
                      | _ -> assert false),
                      Right_first )
                  else if branch || link = Linked_left then
                    ( (match opcode with
                      | O.Ic_less -> A
                      | Ic_greater_equ -> BE
                      | Ic_greater -> B
                      | Ic_less_equ -> AE
                      | _ -> assert false),
                      Left_first )
                  else
                    match opcode with
                    | O.Ic_less -> (B, Right_first)
                    | Ic_greater_equ -> (AE, Right_first)
                    | Ic_greater -> (B, Left_first)
                    | Ic_less_equ -> (AE, Left_first)
                    | _ -> assert false
                in
                let loads =
                  List.map
                    (fun id ->
                      if Option.is_some constant || kind <> Ordered then Bits
                      else if deferred id then Signed_integer
                      else if branch then literal_load d id
                      else Bits)
                    d.operands
                in
                let before = Option.to_list linked in
                let steps =
                  if Option.is_some constant || kind <> Ordered then [ [] ]
                  else
                    match semantic_link with
                    | Some id ->
                        let other = if id = left then right else left in
                        (if Option.is_some conversion_input then [ []; [ id ] ]
                         else [ before ])
                        @ [ [ other; id ]; [ id ]; [] ]
                    | None ->
                        let first, second =
                          if orientation = Left_first then (left, right)
                          else (right, left)
                        in
                        [ []; [ first ]; [ second; first ]; [ first ]; [] ]
                in
                comparisons :=
                  I.add d.instruction_id
                    ( d,
                      {
                        comparison_instruction = d.instruction_id;
                        comparison_result = result.value_id;
                        comparison_operands = d.operands;
                        comparison_loads = loads;
                        comparison_kind = kind;
                        comparison_constant = constant;
                        comparison_context;
                        comparison_opcode = opcode;
                        comparison_predicate = predicate;
                        comparison_negate_result = negate_result;
                        comparison_orientation = orientation;
                        comparison_link = link;
                        comparison_conversion_input = conversion_input;
                        comparison_live_before = before;
                        comparison_live_steps = steps;
                        comparison_peak_depth =
                          (if Option.is_some constant || kind <> Ordered then 0
                           else 2);
                      } )
                    !comparisons)
        | _ ->
            fail d
              "F64 comparison lacks an exact original scalar source receipt")
    !definitions;
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
  let event_ids =
    I.fold
      (fun id _ ids -> I.add id () ids)
      !comparisons
      (I.map (fun _ -> ()) !entries)
  in
  I.iter
    (fun id () ->
      match I.find_opt id !comparisons with
      | Some (d, comparison) ->
          if !live <> comparison.comparison_live_before then
            fail d "F64 comparison crosses an unproved floating owner";
          live := [];
          peak := max !peak comparison.comparison_peak_depth
      | None ->
          let (d : S.description), entry = I.find id !entries in
          let before = Option.to_list (I.find_opt id !inputs) in
          let after =
            if V.mem entry.result_id !retained then [ entry.result_id ] else []
          in
          let before, after =
            match (entry.operation, before, after, !live) with
            | ( (Alias_left | Alias_right | Literal_bits _),
                [],
                [],
                ([ owner ] as live) )
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
            | [ _ ], (Alias_left | Alias_right | Literal_bits _), _, _ ->
                (1, [ before; after ])
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
                ( entry.peak_depth,
                  List.rev (List.tl (List.rev entry.live_steps)) )
            | _ -> (entry.peak_depth, entry.live_steps)
          in
          peak := max !peak peak_depth;
          entries :=
            I.add id
              ( d,
                {
                  entry with
                  live_before = before;
                  live_after = after;
                  peak_depth;
                  live_steps;
                } )
              !entries)
    event_ids;
  (* Explicit TO_I64 is not an OptPass3 CmpF1PushPop consumer. BackB
     invokes ICFCvt at CN_INST, so its argument must have reached memory.
     Internal links in the argument region remain valid, but its terminal
     producer cannot transfer ST0 ownership across the intrinsic boundary. *)
  List.iter
    (fun (d, input) ->
      match entry_of input with
      | Some (_, entry)
        when entry.live_after = [] && entry.result_conversion = None -> ()
      | _ ->
          fail d
            "numeric intrinsic arithmetic argument lacks a materialized \
             terminal receipt")
    !arithmetic_arguments;
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
          && I.is_empty !intrinsics && I.is_empty !comparisons
        then None
        else Some (snapshot ())
      in
      let source_owner =
        Option.bind source (fun s -> Ir.Function_body.definition_source s.body)
      in
      let options =
        Option.map (fun s -> Ir.Function_body.compiler_options s.body) source
      in
      let valid () =
        match original with
        | None -> true
        | Some (original, owners) ->
            let current, current_owners = snapshot () in
            current = original
            && Option.map
                 (fun s -> Ir.Function_body.compiler_options s.body)
                 source
               = options
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
            && List.for_all
                 (fun n -> Ir.Numeric_normalization.valid n ~graph)
                 normalizations
            && (let current_regions, current_comparisons =
                  source_regions source entry_source runtime_context graph
                in
                current_regions = regions
                && I.equal ( == ) current_comparisons comparison_sources)
            && I.for_all
                 (fun _ (description, original) ->
                   Option.fold ~none:false ~some:(( == ) original)
                     (original_call_conversion description))
                 !call_conversions
            && I.for_all
                 (fun _ (description, (region, value)) ->
                   Option.fold ~none:false
                     ~some:(fun (current_region, current_value) ->
                       current_region == region && current_value == value)
                     (original_initializer_conversion description))
                 !initializer_conversions
            && I.for_all
                 (fun id original ->
                   match I.find_opt id !comparisons with
                   | Some (description, _) ->
                       Option.fold ~none:false ~some:(( == ) original)
                         (I.find_opt id comparison_sources)
                       && initializer_comparison description original
                   | None -> false)
                 !initializer_comparisons
            && (I.for_all
                  (fun id _ ->
                    I.mem id !call_conversions
                    || I.mem id !initializer_conversions)
                  !conversions
                && I.for_all
                     (fun id _ -> I.mem id !initializer_comparisons)
                     !comparisons
                && (not !canonical_consumer) && not !option_consumer
               || authentic_source ())
            && (I.is_empty !intrinsics || runtime_valid ())
      in
      Ok
        {
          graph;
          entries = !entries;
          comparisons = !comparisons;
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

let validated_comparisons plan ~graph =
  if graph != plan.graph || not (plan.valid ()) then None
  else
    Some (I.bindings plan.comparisons |> List.map (fun (_, (_, entry)) -> entry))

let validated_emission plan ~graph =
  if graph != plan.graph || not (plan.valid ()) then None
  else
    Some
      {
        arithmetic_entries =
          I.bindings plan.entries |> List.map (fun (_, (_, entry)) -> entry);
        conversion_entries = I.bindings plan.conversions |> List.map snd;
        intrinsic_entries = I.bindings plan.intrinsics |> List.map snd;
        comparison_entries =
          I.bindings plan.comparisons |> List.map (fun (_, (_, entry)) -> entry);
      }
