module Seq = Ir.Instruction_sequence
module Typed = Sema.Function_call_expression_result
module Globals = Ir.Integer_globals
module VM = Ir.Integer_interpreter
module Symbol = Sema.Symbol
module Values = Map.Make (Seq.Value_id)
module Arrays = Ir.Integer_array_initializers
module Layout = Ir.Integer_initializer_layout
module Updates = Integer_update_initializers
module Destination = Ir.Initializer_fragment_destination
module Default = Ir.Default_fragment_destination
module Dimension = Ir.Dimension_fragment_destination
module Offset = Ir.Offset_fragment_destination
module Runtime = Ir.Runtime_call_context

type classification = Prepared_constant of int64 | Scheduled

type 'a prepared_item = {
  root_ : 'a;
  value_graph_ : Ir.X87_stack.t;
  classification_ : classification;
  steps : int;
}

type item = Typed.top_level_root_result prepared_item

type static_item =
  (Globals.static_slot * Typed.initializer_result) prepared_item

type owner =
  | Global of Globals.slot * Typed.top_level_root_result
  | Static of Globals.static_slot * Typed.initializer_result
  | Fragment of Destination.t
  | Default of Default.t
  | Dimension of Dimension.t
  | Offset of Offset.t

type t = {
  globals_ : Globals.t;
  items_ : item list;
  static_items_ : static_item list;
  copies_ : (owner * string * int) list;
  fragment_items_ : Destination.t prepared_item list;
  default_items_ : Default.t prepared_item list;
  dimension_items_ : Dimension.t prepared_item list;
  offset_items_ : Offset.t prepared_item list;
  steps : int;
}

let globals prepared = prepared.globals_
let items prepared = prepared.items_
let static_items prepared = prepared.static_items_
let static_root (item : static_item) = snd item.root_
let static_slot (item : static_item) = fst item.root_
let static_value_graph (item : static_item) = item.value_graph_
let static_item_steps (item : static_item) = item.steps
let static_classification (item : static_item) = item.classification_
let executed_steps (prepared : t) = prepared.steps
let root item = item.root_
let value_graph item = item.value_graph_
let classification item = item.classification_
let item_steps (item : item) = item.steps
let ( let* ) = Result.bind

let instructions graph =
  Ir.Block_graph.blocks graph
  |> List.concat_map (fun block ->
      Ir.Block_graph.instructions block
      |> Seq.instructions |> List.map Seq.description)

let value_instructions graph =
  instructions (Ir.X87_stack.graph graph)
  |> List.filter (fun (item : Seq.description) ->
      match item.opcode with
      | Ir.Opcode.Ic_end_exp | Ic_end -> false
      | _ -> true)

let prepare_internal ?fragment ?default ?dimension ?offset
    ?(function_calls = []) ?(allow_zero_budget = false)
    ?(retained_function_source = fun _ -> None) ?(on_progress = fun _ -> ())
    ~max_steps ~span ~globals ~top_calls ~functions () =
  let invalid ?(notes = []) ?(at = span) code message =
    Error
      [
        Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
          ~notes ~primary:at ();
      ]
  in
  if max_steps < 0 || (max_steps = 0 && not allow_zero_budget) then
    invalid "HCIRVM0001" "max_initializer_steps must be greater than zero"
  else
    let work =
      match offset with
      | Some destination
        when Option.is_none dimension && Option.is_none fragment
             && Option.is_none default -> [ Offset destination ]
      | Some _ -> invalid_arg "conflicting offset preparation owners"
      | None -> (
          match (dimension, fragment, default) with
          | Some destination, None, None -> [ Dimension destination ]
          | Some _, _, _ ->
              invalid_arg "conflicting dimension preparation owners"
          | None, Some destination, None -> [ Fragment destination ]
          | None, None, Some destination -> [ Default destination ]
          | None, Some _, Some _ ->
              invalid_arg "conflicting fragment preparation owners"
          | None, None, None ->
              (Globals.slots globals
              |> List.concat_map (fun slot ->
                  List.map
                    (fun root -> Global (slot, root))
                    (Globals.slot_initializers slot
                    |> List.filter (fun root ->
                        not (Globals.slot_root_executed slot root)))))
              @ (Globals.statics globals
                |> List.concat_map (fun slot ->
                    List.map
                      (fun root -> Static (slot, root))
                      (Globals.static_initializers slot)))
              |> List.stable_sort (fun left right ->
                  let index = function
                    | Fragment _ | Default _ | Dimension _ | Offset _ -> 0
                    | Global (slot, _) ->
                        Globals.slot_record slot
                        |> Sema.Global_record_classification
                           .classified_record_source
                        |> Sema.Global_resolution.global_record_global
                        |> Sema.Global_type_resolution.global_item_index
                    | Static (slot, _) ->
                        Globals.static_frame slot
                        |> Sema.Function_frame_layout.function_item_index
                  in
                  Int.compare (index left) (index right)))
    in
    let rec collect total updates reversed work =
      on_progress total;
      match work with
      | [] ->
          let updates = List.rev updates in
          let scalar_values =
            List.filter_map
              (fun (owner, payload, steps) ->
                match (owner, payload) with
                | Global (slot, _), Arrays.Word bits
                  when Globals.slot_array_initializers slot = None ->
                    Some (Globals.slot_symbol slot, bits, steps)
                | Static (slot, _), Arrays.Word bits
                  when Globals.static_array_initializers slot = None ->
                    Some
                      ( Globals.storage_symbol (Globals.static_storage slot),
                        bits,
                        steps )
                | _ -> None)
              updates
          in
          let* globals_ =
            if
              Option.is_some fragment || Option.is_some default
              || Option.is_some dimension || Option.is_some offset
            then Ok globals
            else Globals.with_initial_values ~span globals scalar_values
          in
          let global_values =
            List.filter_map
              (fun (owner, payload, steps) ->
                match owner with
                | Global (slot, root)
                  when Option.is_some (Globals.slot_array_initializers slot) ->
                    Some (root, payload, steps)
                | _ -> None)
              updates
          in
          let static_values =
            List.filter_map
              (fun (owner, payload, steps) ->
                match owner with
                | Static (slot, root)
                  when Option.is_some (Globals.static_array_initializers slot)
                  -> Some (root, payload, steps)
                | _ -> None)
              updates
          in
          let* globals_ =
            if
              Option.is_some fragment || Option.is_some default
              || Option.is_some dimension || Option.is_some offset
            then Ok globals_
            else
              Globals.with_array_initial_values ~span globals_ ~global_values
                ~static_values
          in
          let prepared = List.rev reversed in
          let items_ =
            List.filter_map
              (fun item ->
                match item.root_ with
                | Global (_, root_) -> Some { item with root_ }
                | Static _ | Fragment _ | Default _ | Dimension _ | Offset _ ->
                    None)
              prepared
          in
          let static_items_ =
            List.filter_map
              (fun item ->
                match item.root_ with
                | Static (slot, root) ->
                    let symbol =
                      Globals.static_location slot
                      |> Sema.Function_frame_layout.location_symbol
                    in
                    let slot =
                      Option.get (Globals.find_static globals_ symbol)
                    in
                    Some { item with root_ = (slot, root) }
                | Global _ | Fragment _ | Default _ | Dimension _ | Offset _ ->
                    None)
              prepared
          in
          let copies_ =
            List.filter_map
              (fun (owner, payload, steps) ->
                match payload with
                | Arrays.Bytes bytes -> Some (owner, bytes, steps)
                | _ -> None)
              updates
          in
          let fragment_items_ =
            List.filter_map
              (fun item ->
                match item.root_ with
                | Fragment root_ -> Some { item with root_ }
                | _ -> None)
              prepared
          in
          Ok
            {
              globals_;
              items_;
              static_items_;
              copies_;
              fragment_items_;
              offset_items_ =
                List.filter_map
                  (fun item ->
                    match item.root_ with
                    | Offset root_ -> Some { item with root_ }
                    | _ -> None)
                  prepared;
              dimension_items_ =
                List.filter_map
                  (fun item ->
                    match item.root_ with
                    | Dimension root_ -> Some { item with root_ }
                    | _ -> None)
                  prepared;
              default_items_ =
                List.filter_map
                  (fun item ->
                    match item.root_ with
                    | Default root_ -> Some { item with root_ }
                    | _ -> None)
                  prepared;
              steps = total;
            }
      | root_ :: rest -> (
          let symbol, value, frame =
            match root_ with
            | Offset destination ->
                ( None,
                  Typed.top_level_root_value (Offset.root destination),
                  None )
            | Dimension destination ->
                ( None,
                  Typed.top_level_root_value (Dimension.root destination),
                  None )
            | Default destination ->
                ( Some (Default.symbol destination),
                  Typed.top_level_root_value (Default.root destination),
                  None )
            | Fragment destination ->
                ( Some (Globals.storage_symbol (Destination.storage destination)),
                  Typed.top_level_root_value (Destination.root destination),
                  None )
            | Global (slot, root) ->
                ( Some (Globals.slot_symbol slot),
                  Typed.top_level_root_value root,
                  None )
            | Static (slot, root) ->
                ( Some
                    (Globals.static_location slot
                    |> Sema.Function_frame_layout.location_symbol),
                  Typed.initializer_value root,
                  Some (Globals.static_frame slot) )
          in
          let at =
            match Typed.result_origin value with
            | Symbol.Source_location location -> location.span
            | _ -> span
          in
          let notes =
            match symbol with
            | None -> [ "dimension=runtime-expression" ]
            | Some symbol ->
                [
                  "initializer=" ^ Symbol.name symbol;
                  Printf.sprintf "initializer_symbol_id=%d"
                    (Symbol.id symbol |> Symbol.Id.to_int);
                ]
          in
          let operation =
            match root_ with
            | Default _ | Dimension _ | Offset _ -> None
            | Fragment destination ->
                Some (Layout.operation (Destination.layout destination))
            | Global (slot, root) ->
                Option.bind (Globals.slot_array_initializers slot)
                  (fun arrays -> Arrays.find arrays root)
                |> Option.map (fun entry ->
                    Layout.operation (Arrays.destination entry))
            | Static (slot, root) ->
                Option.bind (Globals.static_array_initializers slot)
                  (fun arrays -> Arrays.find arrays root)
                |> Option.map (fun entry ->
                    Layout.operation (Arrays.destination entry))
          in
          match operation with
          | Some (Layout.Copy_bytes bytes) ->
              let steps = String.length bytes in
              if steps > max_steps - total then
                invalid ~at
                  ~notes:
                    (notes
                    @ [
                        "initializer_phase=constant-preparation";
                        Printf.sprintf "compiled_initializer_steps=%d" total;
                      ])
                  "HCIRVM0007"
                  "the bounded initializer copy preparation work limit was \
                   exhausted"
              else
                collect (total + steps)
                  ((root_, Arrays.Bytes bytes, steps) :: updates)
                  reversed rest
          | Some Layout.Scalar_store | None -> (
              let* value_lowered =
                Ir.Integer_program_lowering.lower_complete ?frame ~globals
                  ~top_calls ~function_calls ~span:at
                  [ Ir.Integer_program_lowering.Expression value ]
                |> Result.map_error (fun errors ->
                    match root_ with
                    | Global _ | Fragment _ | Default _ | Dimension _ | Offset _
                      -> errors
                    | Static _ ->
                        List.map
                          (fun (error : Common.Diagnostic.t) ->
                            if error.code = "HCRUN0003" then
                              Common.Diagnostic.make ~code:"HCRUN0006"
                                ~severity:Common.Diagnostic.Error
                                ~message:
                                  "static initializer expression is outside \
                                   checked scalar storage and direct-call \
                                   lowering"
                                ~primary:at ~notes ()
                            else error)
                          errors)
              in
              let value_graph_ =
                Ir.Integer_program_lowering.graph value_lowered
              in
              let value_code = value_instructions value_graph_ in
              let constant =
                List.for_all
                  (fun (item : Seq.description) ->
                    not (Ir.Opcode.info item.opcode).prevents_constant_folding)
                  value_code
              in
              let guard ~constant code =
                let rec check pure = function
                  | [] -> Ok ()
                  | (item : Seq.description) :: rest ->
                      let* () =
                        if Option.is_some offset then
                          match item.opcode with
                          | Ir.Opcode.Ic_rip ->
                              invalid
                                ~at:(Option.value item.span ~default:at)
                                ~notes "HCRUN0006"
                                "runtime offset preparation requires checked \
                                 current-position lowering"
                          | _ -> Ok ()
                        else Ok ()
                      in
                      let known id =
                        Option.value (Values.find_opt id pure) ~default:false
                      in
                      let rejected =
                        match (item.opcode, item.operands) with
                        | ( (Ir.Opcode.Ic_shl | Ic_shr | Ic_shl_equ | Ic_shr_equ),
                            _ ) -> true
                        | ( (Ir.Opcode.Ic_div | Ic_mod | Ic_div_equ | Ic_mod_equ),
                            [ _; right ] )
                          when not constant -> known right
                        | _ -> false
                      in
                      if rejected then
                        invalid
                          ~at:(Option.value item.span ~default:at)
                          ~notes "HCRUN0006"
                          "initializer arithmetic requires unresolved \
                           constant-divisor or shift optimizer behavior"
                      else
                        let is_pure =
                          match (item.opcode, item.payload) with
                          | Ir.Opcode.Ic_imm_i64, Some (Seq.Integer _) -> true
                          | _, _ ->
                              (not
                                 (Ir.Opcode.info item.opcode)
                                   .prevents_constant_folding)
                              && item.operands <> []
                              && List.for_all known item.operands
                        in
                        let pure =
                          match item.result with
                          | None -> pure
                          | Some result ->
                              Values.add result.value_id is_pure pure
                        in
                        check pure rest
                in
                check Values.empty code
              in
              let* () = guard ~constant value_code in
              let guard_updates ~globals ~frame ~compiler_options ~terminal
                  graph =
                Updates.check_graph ~globals ~frame ~compiler_options ~terminal
                  graph
                |> Result.map_error (fun (failure : Updates.failure) ->
                    [
                      Common.Diagnostic.make ~code:"HCRUN0006"
                        ~severity:Common.Diagnostic.Error
                        ~message:failure.reason
                        ~primary:
                          (Option.value failure.instruction.span ~default:at)
                        ~notes ();
                    ])
              in
              let destination_type, compiler_options =
                match root_ with
                | Offset destination -> (Offset.type_ destination, 0L)
                | Dimension destination -> (Dimension.type_ destination, 0L)
                | Default destination -> (Default.type_ destination, 0L)
                | Fragment destination ->
                    (Globals.storage_type (Destination.storage destination), 0L)
                | Global (slot, _) -> (Globals.slot_type slot, 0L)
                | Static (slot, _) ->
                    ( Globals.static_storage slot |> Globals.storage_type,
                      Globals.static_compiler_options slot )
              in
              let* terminal =
                instructions (Ir.X87_stack.graph value_graph_)
                |> List.filter_map (fun (item : Seq.description) ->
                    match (item.opcode, item.operands) with
                    | Ir.Opcode.Ic_end_exp, [ value ] -> Some value
                    | _ -> None)
                |> function
                | [ value ] -> Ok (Some Updates.{ value; destination_type })
                | _ ->
                    invalid ~at ~notes "HCRUN0006"
                      "initializer has no unique checked declaration value sink"
              in
              let* () =
                guard_updates ~globals ~frame ~compiler_options ~terminal
                  (Ir.X87_stack.graph value_graph_)
              in
              let called globals functions runtime code =
                List.filter_map
                  (fun (item : Seq.description) ->
                    match runtime with
                    | Some (context, owner) ->
                        Runtime.find_start context ~owner item.instruction_id
                        |> Option.map (fun call ->
                            ( globals,
                              functions,
                              runtime,
                              Runtime.symbol call,
                              Runtime.call_opcode call <> Ir.Opcode.Ic_call,
                              Runtime.retained_function call ))
                    | None -> (
                        match (item.opcode, item.payload) with
                        | ( ( Ir.Opcode.Ic_call
                            | Ic_call_indirect2
                            | Ic_call_extern ),
                            Some (Seq.Symbol symbol) ) ->
                            Some
                              ( globals,
                                functions,
                                runtime,
                                symbol,
                                item.opcode <> Ir.Opcode.Ic_call,
                                None )
                        | _ -> None))
                  code
              in
              let rec guard_callees visited = function
                | [] -> Ok ()
                | ( owner_globals,
                    owner_functions,
                    runtime,
                    symbol,
                    external_,
                    reference )
                  :: rest -> (
                    let before =
                      match root_ with
                      | Global (slot, _) ->
                          Some
                            (Globals.slot_record slot
                           |> Sema.Global_record_classification
                              .classified_record_source
                           |> Sema.Global_resolution.global_record_global
                           |> Sema.Global_type_resolution.global_item_index)
                      | Static (slot, _) ->
                          Some
                            (Globals.static_frame slot
                           |> Sema.Function_frame_layout.function_item_index)
                      (* The source callers pass no local definitions. Their source
                         inspection callback exposes only admitted task bodies. *)
                      | Fragment _ | Default _ | Dimension _ | Offset _ -> None
                    in
                    let find source_globals source_functions =
                      List.find_opt
                        (fun (function_ : VM.function_definition) ->
                          Ir.Function_body.callable_symbol function_.body
                          == symbol
                          && ((not external_) || source_globals != globals
                             ||
                             let aot =
                               Option.fold ~none:false
                                 ~some:(fun declaration ->
                                   Sema.Function_resolution
                                   .resolved_declaration_compilation_mode
                                     declaration
                                   = Sema.Function_resolution.Aot)
                                 (Ir.Function_body.definition_declaration
                                    function_.body)
                             in
                             aot
                             || Option.fold ~none:true
                                  ~some:(fun before ->
                                    Sema.Function_frame_layout
                                    .function_item_index function_.frame
                                    < before)
                                  before))
                        source_functions
                      |> Option.map (fun function_ ->
                          ( source_globals,
                            source_functions,
                            (if source_globals == owner_globals then runtime
                             else None),
                            function_ ))
                    in
                    let source =
                      match
                        if external_ then find globals functions else None
                      with
                      | Some _ as source -> source
                      | None -> find owner_globals owner_functions
                    in
                    let retained reference =
                      Option.map
                        (fun (source : VM.task_function_source) ->
                          ( source.source_globals,
                            source.source_functions,
                            Some
                              ( source.source_runtime_calls,
                                Runtime.Function source.source_definition.body
                              ),
                            source.source_definition ))
                        (retained_function_source reference)
                    in
                    let source =
                      match reference with
                      | Some reference -> retained reference
                      | None -> (
                          match source with
                          | Some _ -> source
                          | None ->
                              Option.bind
                                (Globals.retained_function_symbol owner_globals
                                   symbol)
                                retained)
                    in
                    match source with
                    | None when external_ -> guard_callees visited rest
                    | None ->
                        invalid ~at ~notes "HCRUN0006"
                          "initializer call has no checked source definition"
                    | Some (_, _, _, function_)
                      when List.exists
                             (fun body -> body == function_.body)
                             visited -> guard_callees visited rest
                    | Some (owner_globals, owner_functions, runtime, function_)
                      ->
                        let code =
                          instructions (Ir.Function_body.body function_.body)
                        in
                        let* () = guard ~constant:false code in
                        let* () =
                          guard_updates ~globals:owner_globals
                            ~frame:(Some function_.frame)
                            ~compiler_options:
                              (Ir.Function_body.compiler_options function_.body)
                            ~terminal:None
                            (Ir.Function_body.body function_.body)
                        in
                        let runtime =
                          Option.map
                            (fun (context, _) ->
                              (context, Runtime.Function function_.body))
                            runtime
                        in
                        guard_callees
                          (function_.body :: visited)
                          (called owner_globals owner_functions runtime code
                          @ rest))
              in
              let* () =
                let value_calls =
                  Ir.Integer_program_lowering.runtime_calls value_lowered
                  |> List.filter_map (fun (description : Runtime.description) ->
                      let selected =
                        match description.source with
                        | Runtime.Function_call target ->
                            let source =
                              Sema.Function_call_target_classification.source
                                target
                            in
                            Some
                              ( source |> Typed.direct_source
                                |> Sema.Function_call_conversion_policy
                                   .direct_source
                                |> Sema.Function_call_resolution
                                   .direct_target_symbol,
                                Sema.Function_call_target_classification
                                .call_access target,
                                Typed.direct_outer_binding source )
                        | Runtime.Top_level_call target ->
                            let source =
                              Sema.Top_level_function_call_target_classification
                              .source target
                            in
                            Some
                              ( Typed.top_level_direct_target_symbol source,
                                Sema
                                .Top_level_function_call_target_classification
                                .call_access target,
                                Typed.top_level_direct_outer_binding source )
                        (* An expression cannot contain an implicit output statement. *)
                        | Runtime.Function_output _ | Runtime.Top_level_output _
                          -> None
                      in
                      Option.map
                        (fun (symbol, access, binding) ->
                          ( globals,
                            functions,
                            None,
                            symbol,
                            access
                            <> Sema.Function_record_classification
                               .Direct_executable_call,
                            Option.bind binding
                              (Globals.retained_function_binding globals) ))
                        selected)
                in
                guard_callees [] value_calls
              in
              if
                Option.is_some frame
                && List.exists
                     (fun (item : Seq.description) ->
                       item.opcode = Ir.Opcode.Ic_rbp)
                     value_code
              then
                invalid ~at ~notes "HCRUN0006"
                  "static initialization has no invocation frame for parameter \
                   or automatic-local reads"
              else if
                (not constant)
                &&
                match root_ with
                | Static (slot, _) ->
                    Globals.storage_opcode (Globals.static_storage slot)
                    = Ir.Opcode.Ic_abs_addr
                    && Sema.Compiler_option.is_enabled
                         ~mask:(Globals.static_compiler_options slot)
                         Sema.Compiler_option.Globals_on_data_heap
                | Global _ | Fragment _ | Default _ | Dimension _ | Offset _ ->
                    false
              then
                invalid ~at ~notes "HCRUN0006"
                  "nonconstant AOT static initialization with \
                   globals-on-data-heap requires a separate compile-time phase"
              else if not constant then
                collect total updates
                  ({
                     root_;
                     value_graph_;
                     classification_ = Scheduled;
                     steps = 0;
                   }
                  :: reversed)
                  rest
              else if total >= max_steps then
                invalid ~at
                  ~notes:
                    (notes
                    @ [
                        "initializer_phase=constant-preparation";
                        Printf.sprintf "compiled_initializer_steps=%d" total;
                      ])
                  "HCIRVM0007"
                  "the bounded constant initializer preparation step limit was \
                   exhausted"
              else
                let* result =
                  VM.execute_program ~max_steps:(max_steps - total)
                    ~max_frame_bytes:1 ~max_call_depth:1 ~functions:[]
                    value_graph_
                  |> Result.map_error
                       (List.map (fun (error : VM.error) ->
                            on_progress (total + error.executed_steps);
                            Common.Diagnostic.make ~code:error.code
                              ~severity:Common.Diagnostic.Error
                              ~message:error.message
                              ~primary:(Option.value error.span ~default:at)
                              ~notes:
                                (notes
                                @ [
                                    "initializer_phase=constant-preparation";
                                    Printf.sprintf
                                      "compiled_initializer_steps=%d"
                                      (total + error.executed_steps);
                                    "constant preparation precedes hosted \
                                     whole-program execution";
                                  ])
                              ()))
                in
                match VM.final_value result with
                | None ->
                    invalid ~at ~notes "HCRUN0004"
                      "constant initializer preparation produced no word"
                | Some word ->
                    let steps = VM.executed_steps result in
                    collect (total + steps)
                      ((root_, Arrays.Word word.bits, steps) :: updates)
                      ({
                         root_;
                         value_graph_;
                         classification_ = Prepared_constant word.bits;
                         steps;
                       }
                      :: reversed)
                      rest))
    in
    collect 0 [] [] work

let prepare ?function_calls ?allow_zero_budget ?retained_function_source
    ?on_progress ~max_steps ~span ~globals ~top_calls ~functions () =
  prepare_internal ?function_calls ?allow_zero_budget ?retained_function_source
    ?on_progress ~max_steps ~span ~globals ~top_calls ~functions ()

type fragment_preparation = {
  fragment_destination_ : Destination.t;
  fragment_payload_ : Arrays.payload option;
  fragment_steps_ : int;
}

let fragment_destination prepared = prepared.fragment_destination_
let fragment_payload prepared = prepared.fragment_payload_
let fragment_steps prepared = prepared.fragment_steps_

let prepare_fragment ?retained_function_source ?on_progress ~max_steps
    ~top_calls ~functions destination =
  let* prepared =
    prepare_internal ~fragment:destination ~allow_zero_budget:true
      ?retained_function_source ?on_progress ~max_steps
      ~span:(Destination.span destination)
      ~globals:(Destination.globals destination)
      ~top_calls ~functions ()
  in
  let payload =
    match (prepared.fragment_items_, prepared.copies_) with
    | [ item ], [] -> (
        match item.classification_ with
        | Prepared_constant bits -> Some (Arrays.Word bits)
        | Scheduled -> None)
    | [], [ (Fragment original, bytes, _) ] when original == destination ->
        Some (Arrays.Bytes bytes)
    | _ -> invalid_arg "fragment preparation lost its unique original work item"
  in
  Ok
    {
      fragment_destination_ = destination;
      fragment_payload_ = payload;
      fragment_steps_ = prepared.steps;
    }

let prepare_default ?retained_function_source ?on_progress ~max_steps ~top_calls
    destination =
  let* prepared =
    prepare_internal ~default:destination ~allow_zero_budget:true
      ?retained_function_source ?on_progress ~max_steps
      ~span:(Default.span destination)
      ~globals:(Default.globals destination)
      ~top_calls ~functions:[] ()
  in
  match prepared.default_items_ with
  | [ item ] when item.root_ == destination ->
      Ok (item.classification_, prepared.steps)
  | _ -> invalid_arg "default preparation lost its unique original work item"

let prepare_dimension ?retained_function_source ?on_progress ~max_steps
    ~top_calls destination =
  let* prepared =
    prepare_internal ~dimension:destination ~allow_zero_budget:true
      ?retained_function_source ?on_progress ~max_steps
      ~span:(Dimension.span destination)
      ~globals:(Dimension.globals destination)
      ~top_calls ~functions:[] ()
  in
  match prepared.dimension_items_ with
  | [ item ] when item.root_ == destination ->
      Ok (item.classification_, prepared.steps)
  | _ -> invalid_arg "dimension preparation lost its original work item"

let prepare_offset ?retained_function_source ?on_progress ~max_steps ~top_calls
    destination =
  let* prepared =
    prepare_internal ~offset:destination ~allow_zero_budget:true
      ?retained_function_source ?on_progress ~max_steps
      ~span:(Offset.span destination)
      ~globals:(Offset.globals destination)
      ~top_calls ~functions:[] ()
  in
  match prepared.offset_items_ with
  | [ item ] when item.root_ == destination ->
      Ok (item.classification_, prepared.steps)
  | _ -> invalid_arg "offset preparation lost its original work item"

let global_human prepared =
  match prepared.items_ with
  | [] -> ""
  | items ->
      Printf.sprintf "holyc-initializer-preparation-v1 steps=%d\n"
        prepared.steps
      ^ String.concat ""
          (List.map
             (fun item ->
               let owner =
                 match
                   item.root_ |> Typed.top_level_root_source
                   |> Sema.Top_level_expression_tree.root_role
                 with
                 | Sema.Top_level_expression_tree.Global_initializer owner ->
                     Sema.Global_initializer_binding.global_symbol owner
                 | _ -> assert false
               in
               Printf.sprintf
                 "initializer symbol=%d:%s class=%s preparation-steps=%d\n"
                 (Symbol.id owner |> Symbol.Id.to_int)
                 (Symbol.name owner)
                 (match item.classification_ with
                 | Prepared_constant bits ->
                     Printf.sprintf "constant:0x%016Lx" bits
                 | Scheduled -> "nonconstant")
                 item.steps)
             items)

let numeric_human prepared =
  global_human prepared
  ^
  match prepared.static_items_ with
  | [] -> ""
  | items ->
      Printf.sprintf "holyc-static-initializer-preparation-v1 total-steps=%d\n"
        prepared.steps
      ^ String.concat ""
          (List.map
             (fun item ->
               let slot = fst item.root_ in
               let symbol =
                 Globals.static_location slot
                 |> Sema.Function_frame_layout.location_symbol
               in
               Printf.sprintf
                 "static-initializer function=%s symbol=%d:%s \
                  preparation-steps=%d\n"
                 (Globals.static_frame slot
                |> Sema.Function_frame_layout.function_symbol |> Symbol.name)
                 (Symbol.id symbol |> Symbol.Id.to_int)
                 (Symbol.name symbol) item.steps)
             items)

let human prepared =
  numeric_human prepared
  ^
  match prepared.copies_ with
  | [] -> ""
  | copies ->
      Printf.sprintf "holyc-initializer-copies-v1 total-steps=%d\n"
        prepared.steps
      ^ String.concat ""
          (List.map
             (fun (owner, bytes, steps) ->
               let symbol =
                 match owner with
                 | Dimension _ | Offset _ ->
                     invalid_arg "dimension cannot own copied bytes"
                 | Default destination -> Default.symbol destination
                 | Fragment destination ->
                     Globals.storage_symbol (Destination.storage destination)
                 | Global (slot, _) -> Globals.slot_symbol slot
                 | Static (slot, _) ->
                     Globals.storage_symbol (Globals.static_storage slot)
               in
               Printf.sprintf
                 "initializer-copy symbol=%d:%s bytes=%d preparation-work=%d\n"
                 (Symbol.id symbol |> Symbol.Id.to_int)
                 (Symbol.name symbol) (String.length bytes) steps)
             copies)
