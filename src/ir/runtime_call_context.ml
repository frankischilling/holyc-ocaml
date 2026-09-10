module Seq = Instruction_sequence
module Typed = Sema.Function_call_expression_result
module Resolution = Sema.Function_call_resolution
module Headers = Sema.Function_type_resolution
module Records = Sema.Function_record_classification
module Functions = Sema.Function_resolution
module Type = Sema.Type
module Instructions = Map.Make (Seq.Instruction_id)

type source =
  | Function_call of Sema.Function_call_target_classification.t
  | Top_level_call of Sema.Top_level_function_call_target_classification.t
  | Function_output of Sema.Implicit_output_argument_binding.bound_output
  | Top_level_output of
      Sema.Top_level_implicit_output_argument_binding.bound_output

type description = {
  source : source;
  first : Seq.Instruction_id.t;
  last : Seq.Instruction_id.t;
  discard : Seq.Instruction_id.t option;
}

type provider = Print | Put_chars | Stream_print
type owner = Entry | Function of Function_body.t
type argument_role = Fixed of int | Variadic_count | Variadic of int

type argument = {
  prepared_default : bool;
  role : argument_role;
  producer : Seq.Instruction_id.t;
  value : Seq.Value_id.t;
  source_type : Type.t;
  target_type : Type.t;
}

type call = {
  description : description;
  provider_ : provider option;
  symbol_ : Sema.Symbol.t;
  return_type_ : Type.t;
  call_opcode_ : Opcode.t;
  cleanup_opcode_ : Opcode.t;
  cleanup_bytes_ : int64;
  call_instruction_ : Seq.Instruction_id.t;
  cleanup_instruction_ : Seq.Instruction_id.t;
  result_value_ : Seq.Value_id.t;
  arguments_ : argument list;
  variadic_count_ : int64 option;
  declaration_ : Functions.resolved_declaration;
  header_ : Headers.resolved_function;
  retained_function_ : Retained_function.t option;
}

type graph_context = {
  owner : owner;
  calls : call Instructions.t;
  discards : call Instructions.t;
}

type t = {
  typed_top_level : Typed.top_level_t;
  dimension_dependencies_ :
    Sema.Compiler_record.runtime_dimension_proposal list;
  entry : X87_stack.t;
  initialization : Global_initialization.t;
  functions : Function_body.t list;
  graphs : graph_context list;
}

let provider call = call.provider_
let symbol call = call.symbol_
let return_type call = call.return_type_
let call_opcode call = call.call_opcode_
let cleanup_opcode call = call.cleanup_opcode_
let cleanup_bytes call = call.cleanup_bytes_
let first call = call.description.first
let call_instruction call = call.call_instruction_
let cleanup_instruction call = call.cleanup_instruction_
let last call = call.description.last
let result_value call = call.result_value_
let arguments call = call.arguments_
let argument_role argument = argument.role
let argument_producer argument = argument.producer
let argument_value argument = argument.value
let argument_source_type argument = argument.source_type
let argument_target_type argument = argument.target_type
let variadic_count call = call.variadic_count_
let declaration call = call.declaration_
let header call = call.header_
let retained_function call = call.retained_function_

let same_owner left right =
  match (left, right) with
  | Entry, Entry -> true
  | Function left, Function right -> left == right
  | _ -> false

let matches context ~entry ~initialization ~functions =
  context.entry == entry
  && Option.fold ~none:false
       ~some:(fun supplied -> supplied == context.initialization)
       initialization
  && List.length context.functions = List.length functions
  && List.for_all2 ( == ) context.functions functions

let find_graph context owner =
  List.find_opt (fun graph -> same_owner graph.owner owner) context.graphs

let find_start context ~owner id =
  Option.bind (find_graph context owner) (fun graph ->
      Instructions.find_opt id graph.calls)

let is_prepared_default context ~owner id =
  Option.fold ~none:false
    ~some:(fun graph ->
      Instructions.exists
        (fun _ call ->
          List.exists
            (fun argument ->
              argument.prepared_default
              && Seq.Instruction_id.equal argument.producer id)
            call.arguments_)
        graph.calls)
    (find_graph context owner)

let is_implicit_discard context ~owner id =
  Option.fold ~none:false
    ~some:(fun graph -> Instructions.mem id graph.discards)
    (find_graph context owner)

exception Invalid of Common.Diagnostic.t

let fallback_span =
  match Common.Source_id.of_int 0 with
  | Ok source -> Common.Span.unsafe_make ~source ~start:0 ~stop:0
  | Error _ -> assert false

let origin_span = function
  | Sema.Symbol.Source_location location -> Some location.span
  | _ -> None

let fail ?span message =
  raise
    (Invalid
       (Common.Diagnostic.make ~code:"HCIRVM0014"
          ~severity:Common.Diagnostic.Error ~message
          ~primary:(Option.value span ~default:fallback_span)
          ()))

let require ?span condition message = if not condition then fail ?span message

type fixed_value =
  | Provided of Typed.expression_result
  | Prepared_default of Prepared_parameter_default.t

type shape = {
  source_description : description;
  selected_declaration : Functions.resolved_declaration;
  selected_header : Headers.resolved_function;
  selected_record : Records.record;
  selected_symbol : Sema.Symbol.t;
  result_type : Type.t;
  fixed : (Headers.parameter * fixed_value) list;
  variadic : Typed.expression_result list;
  count_type : Type.t option;
  origin : Sema.Symbol.origin;
  retained_function : Retained_function.t option;
}

let parameter_type parameter =
  parameter |> Headers.parameter_type_reference
  |> Sema.Type_reference.resolved_type

let prepared_default ~globals ~header ~parameter ?span () =
  match
    Integer_globals.prepared_parameter_default globals ~header ~parameter
  with
  | Some prepared -> Prepared_default prepared
  | None ->
      fail ?span "runtime call has no original prepared default in its snapshot"

let provided ~globals ~header ~parameter ?span = function
  | Typed.Provided_result result -> Provided result
  | Typed.Declared_default_result _ ->
      prepared_default ~globals ~header ~parameter ?span ()

let shape ~globals records description =
  let declaration, header, symbol, fixed, variadic, count, origin, implicit =
    match description.source with
    | Function_call target ->
        let module Target = Sema.Function_call_target_classification in
        let typed = Target.source target in
        let direct =
          typed |> Typed.direct_source
          |> Sema.Function_call_conversion_policy.direct_source
        in
        let origin =
          direct |> Resolution.direct_source |> Resolution.call_origin
        in
        let span = origin_span origin in
        let fixed =
          List.map
            (fun result ->
              let parameter =
                result |> Typed.fixed_source
                |> Sema.Function_call_conversion_policy.fixed_source
                |> Resolution.fixed_parameter
              in
              ( parameter,
                provided ~globals
                  ~header:(Resolution.direct_active_header direct)
                  ~parameter ?span (Typed.fixed_path result) ))
            (Typed.direct_fixed_results typed)
        in
        ( Target.declaration target,
          Resolution.direct_active_header direct,
          Resolution.direct_target_symbol direct,
          fixed,
          Typed.direct_variadic_results typed,
          Some (Resolution.direct_variadic_count direct),
          origin,
          false )
    | Top_level_call target ->
        let module Target = Sema.Top_level_function_call_target_classification
        in
        let typed = Target.source target in
        let origin =
          typed |> Typed.top_level_direct_source
          |> Sema.Top_level_expression_tree.call_source
          |> Resolution.call_origin
        in
        let span = origin_span origin in
        let fixed =
          List.map
            (fun result ->
              let parameter =
                result |> Typed.top_level_fixed_source
                |> Resolution.fixed_parameter
              in
              ( parameter,
                provided ~globals
                  ~header:(Typed.top_level_direct_header typed)
                  ~parameter ?span
                  (Typed.top_level_fixed_path result) ))
            (Typed.top_level_direct_fixed_results typed)
        in
        ( Target.declaration target,
          Typed.top_level_direct_header typed,
          Typed.top_level_direct_target_symbol typed,
          fixed,
          Typed.top_level_direct_variadic_results typed,
          Some (Typed.top_level_direct_variadic_count typed),
          origin,
          false )
    | Function_output output ->
        let module Bound = Sema.Implicit_output_argument_binding in
        let module Target = Sema.Implicit_output_target_resolution in
        let source = Bound.bound_source output in
        let typed = Target.output_source source in
        let origin =
          typed |> Typed.implicit_output_source
          |> Resolution.implicit_output_origin
        in
        let span = origin_span origin in
        require ?span
          (Typed.implicit_output_result_use typed = Typed.Result_not_used)
          "implicit output lost its checked discarded-result intent";
        let declaration, symbol =
          match Target.output_binding source with
          | Target.Module_function target ->
              ( Target.module_declaration target,
                Target.module_target_symbol target )
          | Target.Outer_function binding -> (
              let entry = Sema.Outer_environment.binding_entry binding in
              match Sema.Outer_environment.entry_function_metadata entry with
              | Some metadata ->
                  ( Sema.Outer_environment.function_declaration metadata,
                    Sema.Outer_environment.entry_symbol entry )
              | None ->
                  fail ?span "outer output has no selected runtime declaration")
        in
        let fixed =
          List.map
            (fun slot ->
              let value =
                match Bound.fixed_path slot with
                | Bound.Provided_path value ->
                    require ?span
                      (Bound.provided_conversion value = Bound.No_conversion)
                      "implicit output requires an unsupported argument \
                       conversion";
                    Provided (Bound.provided_result value)
                | Bound.Defaulted_path default ->
                    require ?span
                      (Bound.default_materialization default
                      = Bound.Immediate_default)
                      "implicit output requires unsupported default \
                       materialization";
                    prepared_default ~globals
                      ~header:(Bound.bound_header output)
                      ~parameter:(Bound.fixed_parameter slot)
                      ?span ()
              in
              (Bound.fixed_parameter slot, value))
            (Bound.bound_fixed_slots output)
        in
        ( declaration,
          Bound.bound_header output,
          symbol,
          fixed,
          Bound.bound_variadic_values output,
          None,
          origin,
          true )
    | Top_level_output output ->
        let module Bound = Sema.Top_level_implicit_output_argument_binding in
        let module Target = Sema.Top_level_implicit_output_target_resolution in
        let source = Bound.bound_source output in
        let origin = Target.output_marker_origin source in
        let span = origin_span origin in
        require ?span
          (match Target.output_supplied_fixed_value source with
          | Some root -> (
              match
                root |> Typed.top_level_root_source
                |> Sema.Top_level_expression_tree.root_role
              with
              | Sema.Top_level_expression_tree.Implicit_output_fixed
                  { output_index; _ } ->
                  output_index = Target.output_index source
              | _ -> false)
          | None ->
              Option.fold ~none:false
                ~some:(fun ast ->
                  ast.Frontend.Ast.fixed_argument
                  = Frontend.Ast.Absent_fixed_argument
                  && Option.fold ~none:false
                       ~some:
                         (List.exists (fun (index, original) ->
                              index = Target.output_index source
                              && original == ast))
                       (Target.output_statement source
                       |> Typed.top_level_statement_source
                       |> Sema.Top_level_expression_tree
                          .statement_implicit_outputs))
                (Target.output_source_statement source))
          "top-level output does not retain its checked implicit root role";
        let declaration, symbol =
          match Target.output_binding source with
          | Target.Module_function target ->
              ( Target.module_declaration target,
                Target.module_target_symbol target )
          | Target.Outer_function binding -> (
              let entry = Sema.Outer_environment.binding_entry binding in
              match Sema.Outer_environment.entry_function_metadata entry with
              | Some metadata ->
                  ( Sema.Outer_environment.function_declaration metadata,
                    Sema.Outer_environment.entry_symbol entry )
              | None ->
                  fail ?span "outer output has no selected runtime declaration")
        in
        let fixed =
          List.map
            (fun slot ->
              let value =
                match Bound.fixed_path slot with
                | Bound.Provided_path value ->
                    require ?span
                      (Bound.provided_conversion value = Bound.No_conversion)
                      "implicit output requires an unsupported argument \
                       conversion";
                    Provided (Bound.provided_result value)
                | Bound.Defaulted_path default ->
                    require ?span
                      (Bound.default_materialization default
                      = Bound.Immediate_default)
                      "implicit output requires unsupported default \
                       materialization";
                    prepared_default ~globals
                      ~header:(Bound.bound_header output)
                      ~parameter:(Bound.fixed_parameter slot)
                      ?span ()
              in
              (Bound.fixed_parameter slot, value))
            (Bound.bound_fixed_slots output)
        in
        ( declaration,
          Bound.bound_header output,
          symbol,
          fixed,
          List.map Typed.top_level_root_value
            (Bound.bound_variadic_roots output),
          None,
          origin,
          true )
  in
  let span = origin_span origin in
  require ?span
    (Option.is_some description.discard = implicit)
    "call context discard identity does not match its checked statement source";
  let outer_binding =
    match description.source with
    | Function_call target ->
        Sema.Function_call_target_classification.source target
        |> Typed.direct_outer_binding
    | Top_level_call target ->
        Sema.Top_level_function_call_target_classification.source target
        |> Typed.top_level_direct_outer_binding
    | Function_output output -> (
        match
          Sema.Implicit_output_target_resolution.output_binding
            (Sema.Implicit_output_argument_binding.bound_source output)
        with
        | Sema.Implicit_output_target_resolution.Outer_function binding ->
            Some binding
        | _ -> None)
    | Top_level_output output -> (
        match
          Sema.Top_level_implicit_output_target_resolution.output_binding
            (Sema.Top_level_implicit_output_argument_binding.bound_source output)
        with
        | Sema.Top_level_implicit_output_target_resolution.Outer_function
            binding -> Some binding
        | _ -> None)
  in
  let retained_function =
    Option.map
      (fun binding ->
        match Integer_globals.retained_function_binding globals binding with
        | Some reference ->
            let metadata = Retained_function.metadata reference in
            require ?span
              (Sema.Outer_environment.function_declaration metadata
               == declaration
              &&
              match
                Sema.Outer_environment.binding_entry binding
                |> Sema.Outer_environment.entry_function_metadata
              with
              | Some expected -> expected == metadata
              | None -> false)
              "retained call does not own its selected task declaration";
            reference
        | None ->
            fail ?span "retained call has no exact selected task function link")
      outer_binding
  in
  let classified =
    match retained_function with
    | Some reference ->
        Some
          (Retained_function.metadata reference
          |> Sema.Outer_environment.function_classified_declaration)
    | None ->
        Records.declarations records
        |> List.find_opt (fun candidate ->
            Records.classified_declaration_source candidate == declaration)
  in
  let selected_record =
    match classified with
    | Some classified -> Records.classified_declaration_record classified
    | None ->
        fail ?span
          "call declaration does not belong to the supplied record snapshots"
  in
  require ?span
    ( declaration |> Functions.resolved_declaration_site
      |> Functions.declaration_site_function
    |> fun expected -> expected == header )
    "call header is not its selected declaration header";
  require ?span
    (Functions.resolved_declaration_identity_symbol declaration == symbol)
    "call symbol does not own its selected declaration";
  (match description.source with
  | Function_call target ->
      require ?span
        (Sema.Function_call_target_classification.record target
        == selected_record)
        "function call record belongs to another classification"
  | Top_level_call target ->
      require ?span
        (Sema.Top_level_function_call_target_classification.record target
        == selected_record)
        "top-level call record belongs to another classification"
  | Function_output _ | Top_level_output _ -> ());
  let parameters =
    header |> Headers.function_signature |> Headers.signature_parameters
  in
  require ?span
    (List.length parameters = List.length fixed
    && List.for_all2
         (fun parameter (actual, _) -> parameter == actual)
         parameters fixed)
    "call fixed values do not match the selected parameter identities";
  let count_type =
    header |> Headers.function_variadic_bindings
    |> Option.map (fun bindings ->
        bindings |> Headers.variadic_argc |> Headers.synthetic_binding_type)
  in
  require ?span
    (Option.is_some count_type || variadic = [])
    "nonvariadic call has a variadic argument tail";
  Option.iter
    (fun count ->
      require ?span
        (Int64.equal count (Int64.of_int (List.length variadic)))
        "call hidden count does not match its checked supplied tail")
    count;
  Option.iter
    (fun type_ ->
      require ?span
        (Type.pointer_depth type_ = 0
        && Type.base type_
           = Type.Primitive (Type.Internal_storage, Sema.Primitive_type.I64))
        "call hidden count is not the synthesized internal I64 class")
    count_type;
  {
    source_description = description;
    selected_declaration = declaration;
    selected_header = header;
    selected_record;
    selected_symbol = symbol;
    result_type =
      header |> Headers.function_return_type
      |> Sema.Type_reference.resolved_type;
    fixed;
    variadic;
    count_type;
    origin;
    retained_function;
  }

let selected_opcode ?span record =
  match Records.call_access record with
  | Records.Direct_executable_call -> Opcode.Ic_call
  | Records.Jit_extern_address_slot_call -> Opcode.Ic_call_indirect2
  | Records.Aot_extern_call -> Opcode.Ic_call_extern
  | Records.Aot_import_call -> Opcode.Ic_call_import
  | Records.Internal_operation ->
      fail ?span "internal operation has no ordinary runtime call scope"

let selected_cleanup record =
  if
    Sema.Function_flag.caller_expects_callee_pop
      ~stored_mask:(Records.stored_flag_mask record)
  then Opcode.Ic_add_rsp1
  else Opcode.Ic_add_rsp

let approved_provider shape =
  let primitive type_ depth value =
    Type.pointer_depth type_ = depth
    &&
    match Type.base type_ with
    | Type.Primitive (_, actual) -> Sema.Primitive_type.equal actual value
    | _ -> false
  in
  let module Flags = Sema.Function_flag.Stored in
  let flags = Records.stored_flag_mask shape.selected_record in
  let parameter =
    match shape.fixed with
    | [ (parameter, _) ] -> Some parameter
    | _ -> None
  in
  let ordinary =
    Records.is_extern shape.selected_record
    && (not (Records.is_internal shape.selected_record))
    && Records.import_name shape.selected_record = None
    && shape.selected_declaration |> Functions.resolved_declaration_site
       |> Functions.declaration_site_kind = Functions.Extern
    && primitive shape.result_type 0 Sema.Primitive_type.U0
  in
  match (ordinary, Sema.Symbol.name shape.selected_symbol, parameter) with
  | true, (("Print" | "StreamPrint") as name), Some parameter
    when Headers.parameter_default parameter = None
         && Headers.parameter_register_requests parameter = []
         && primitive (parameter_type parameter) 1 Sema.Primitive_type.U8
         && Option.is_some shape.count_type
         && Int64.equal flags (Flags.to_mask Flags.Variadic) ->
      Some (if name = "Print" then Print else Stream_print)
  | true, "PutChars", Some parameter
    when Headers.parameter_default parameter = None
         && Headers.parameter_register_requests parameter = []
         && primitive (parameter_type parameter) 0 Sema.Primitive_type.U64
         && Option.is_none shape.count_type
         && Int64.equal flags (Flags.to_mask Flags.Ret1) -> Some Put_chars
  | _ -> None

type expected_argument = {
  expected_default : bool;
  expected_role : argument_role;
  expected_source : Type.t;
  expected_target : Type.t;
  expected_origin : Common.Span.t option;
  expected_count : int64 option;
}

let rec producer_origin result =
  (* Expression_lowering emits operator origins for operations. Transparent
     grouping and unary plus reuse their operand's producer; array
     materialization instead emits its own result-origin IC_ADDR. *)
  let own () = origin_span (Typed.result_origin result) in
  let operand () =
    match Typed.result_operand result with
    | Some operand -> operand
    | None ->
        fail ?span:(own ())
          "transparent call argument has no checked operand origin"
  in
  if Typed.result_is_array_address result then own ()
  else
    match Typed.result_source result |> Resolution.argument_expression_kind with
    | Resolution.Parenthesized_expression _ ->
        let operand = operand () in
        if Typed.result_is_array_address operand then own ()
        else producer_origin operand
    | Resolution.Prefix_expression prefix -> (
        match Resolution.prefix_operator prefix with
        | Resolution.Unary_plus -> producer_origin (operand ())
        | _ -> origin_span (Resolution.prefix_operator_origin prefix))
    | Resolution.Postfix_expression postfix ->
        origin_span (Resolution.postfix_operator_origin postfix)
    | Resolution.Binary_expression binary ->
        origin_span (Resolution.binary_operator_origin binary)
    | _ -> own ()

let rec producer_type ~globals result =
  let span = origin_span (Typed.result_origin result) in
  let type_ =
    match Typed.result_type result with
    | Some type_ -> type_
    | None -> fail ?span "call argument has no checked result type"
  in
  if not (Typed.result_is_array_address result) then type_
  else
    (* checked_frame_value materializes an ordinary array's retained element
       class as a one-level pointer. Its checked dimensions remain separate
       from Type.t; a scalar byte expression does not receive this conversion. *)
    let rank = Typed.result_array_rank result in
    require ?span
      (rank > 0
      && Typed.result_category result = Typed.Array_value
      && Typed.result_class result = Typed.Integer_result
      && Type.pointer_depth type_ = 0
      &&
      match Type.base type_ with
      | Type.Primitive (_, (Sema.Primitive_type.U8 | I64 | U64)) -> true
      | _ -> false)
      "materialized call argument has no supported checked array element class";
    (match
       Typed.result_source result |> Resolution.argument_expression_kind
     with
    | _ when Option.is_some (Typed.result_outer_binding result) -> (
        match Global_address_lowering.prepare ~globals result with
        | Ok (Some address) ->
            require ?span
              (List.length (Global_address_lowering.strides address) = rank)
              "retained array argument disagrees with its exact task object \
               rank"
        | _ ->
            fail ?span
              "retained array argument has no checked task storage reference")
    | Resolution.Bound_identifier_expression identifier ->
        require ?span
          (Resolution.bound_identifier_is_ordinary_array identifier
          && Resolution.bound_identifier_shape identifier
             = Resolution.Array_value
          && Resolution.bound_identifier_array_rank identifier = rank
          && Type.equal (Resolution.bound_identifier_type identifier) type_)
          "materialized call argument disagrees with its checked array \
           declaration"
    | Resolution.Top_level_bound_identifier_expression identifier ->
        let module Top = Sema.Top_level_outer_expression_binding in
        let module Binding = Sema.Module_expression_binding in
        let occurrence =
          Resolution.top_level_bound_identifier_occurrence identifier
        in
        require ?span
          (Top.occurrence_origin occurrence = Typed.result_origin result
          &&
          match Top.occurrence_resolution occurrence with
          | Top.Module_binding publication ->
              Binding.publication_kind publication = Binding.Global_variable
              && Binding.publication_source_symbol publication
                 == Binding.publication_canonical_symbol publication
          | _ -> false)
          "materialized call argument has no checked global array publication"
    | Resolution.Index_expression source -> (
        match Typed.result_index_operands result with
        | Some (base, index) ->
            require ?span
              (Typed.result_source base == Resolution.index_base source
              && Typed.result_source index == Resolution.index_value source
              && Typed.result_is_array_address base
              && Typed.result_array_rank base = rank + 1
              && Option.fold ~none:false ~some:(Type.equal type_)
                   (Typed.result_type base))
              "materialized call argument lost its checked remaining array \
               dimensions";
            ignore (producer_type ~globals base)
        | None -> fail ?span "materialized array index has no checked operands")
    | _ ->
        fail ?span
          "materialized call argument has no checked ordinary-array source");
    match Type.pointer_to type_ with
    | Ok pointer -> pointer
    | Error _ ->
        fail ?span
          "materialized call argument cannot form its checked element pointer"

let expected_arguments ~globals shape =
  let span = origin_span shape.origin in
  let actual role target value =
    let source = producer_type ~globals value in
    {
      expected_role = role;
      expected_source = source;
      expected_target = Option.value target ~default:source;
      expected_origin = producer_origin value;
      expected_count = None;
      expected_default = false;
    }
  in
  let fixed =
    List.mapi
      (fun i (parameter, value) ->
        match value with
        | Provided value ->
            actual (Fixed i) (Some (parameter_type parameter)) value
        | Prepared_default prepared ->
            {
              expected_role = Fixed i;
              expected_source = Prepared_parameter_default.type_ prepared;
              expected_target = parameter_type parameter;
              expected_origin = span;
              expected_count = Some (Prepared_parameter_default.bits prepared);
              expected_default = true;
            })
      shape.fixed
  in
  let variadic =
    List.mapi (fun i value -> actual (Variadic i) None value) shape.variadic
  in
  let count =
    match shape.count_type with
    | None -> []
    | Some type_ ->
        [
          {
            expected_role = Variadic_count;
            expected_source = type_;
            expected_target = type_;
            expected_origin = span;
            expected_count = Some (Int64.of_int (List.length shape.variadic));
            expected_default = false;
          };
        ]
  in
  List.rev variadic @ count @ List.rev fixed

type phase =
  | Collecting
  | Called of Seq.Instruction_id.t
  | Cleaned of Seq.Instruction_id.t * Seq.Instruction_id.t

type pending = {
  shape : shape;
  mutable phase : phase;
  mutable pushes : argument list;
  mutable expected : expected_argument list;
}

let graph_context ~globals ~records ~validate_source owner graph descriptions =
  let pending_shapes =
    List.fold_left
      (fun map description ->
        let shape = shape ~globals records description in
        let span = origin_span shape.origin in
        validate_source owner description span;
        require ?span
          (not (Instructions.mem description.first map))
          "duplicate runtime call start identity";
        (match (owner, description.source) with
        | Function _, (Top_level_call _ | Top_level_output _) ->
            fail ?span "top-level call source cannot authorize a function body"
        | _ -> ());
        Instructions.add description.first shape map)
      Instructions.empty descriptions
  in
  let remaining = ref pending_shapes in
  let calls = ref Instructions.empty and discards = ref Instructions.empty in
  let push_flag = 0x2000L in
  let blocks = Block_graph.blocks graph in
  List.iter
    (fun block ->
      let stack = ref [] in
      let items =
        block |> Block_graph.instructions |> Seq.instructions
        |> List.map Seq.description
      in
      List.iter
        (fun (item : Seq.description) ->
          let span = item.span in
          let push = Int64.logand item.flags push_flag <> 0L in
          let ordinary_flags =
            Int64.logand item.flags (Int64.lognot push_flag)
          in
          let require_shape shape =
            require ?span
              (item.operands = [] && ordinary_flags = 0L)
              "runtime call instruction has unexpected operands or flags";
            require ?span
              (Option.fold ~none:false
                 ~some:(Type.equal shape.result_type)
                 item.target_type)
              "runtime call instruction has a different declared return type"
          in
          (match (item.opcode, !stack) with
          | Opcode.Ic_call_start, _ ->
              require ?span
                (item.operands = [] && item.result = None
               && item.target_type = None && item.flags = 0L)
                "runtime call start has an invalid shape";
              (match !stack with
              | { phase = Called _ | Cleaned _; _ } :: _ ->
                  fail ?span "nested call interrupts an unfinished cleanup"
              | _ -> ());
              let shape =
                match Instructions.find_opt item.instruction_id !remaining with
                | Some shape -> shape
                | None ->
                    fail ?span
                      "call start has no unique checked source description"
              in
              require ?span
                (match item.payload with
                | Some (Seq.Symbol symbol) -> symbol == shape.selected_symbol
                | _ -> false)
                "call start does not retain its exact selected symbol";
              require ?span
                (item.span = origin_span shape.origin)
                "call start does not retain its checked source origin";
              remaining := Instructions.remove item.instruction_id !remaining;
              stack :=
                {
                  shape;
                  phase = Collecting;
                  pushes = [];
                  expected = expected_arguments ~globals shape;
                }
                :: !stack
          | ( ( Opcode.Ic_call
              | Ic_call_indirect2
              | Ic_call_extern
              | Ic_call_import ),
              pending :: _ ) ->
              require_shape pending.shape;
              require ?span
                (pending.phase = Collecting && item.result = None && not push)
                "runtime call is outside its argument-collection phase";
              require ?span
                (item.opcode
                = selected_opcode ?span pending.shape.selected_record)
                "runtime call opcode differs from its declaration snapshot";
              require ?span
                (match item.payload with
                | Some (Seq.Symbol symbol) ->
                    symbol == pending.shape.selected_symbol
                | _ -> false)
                "runtime call changed its selected symbol";
              require ?span (pending.expected = [])
                "runtime call does not contain all checked argument producers";
              pending.phase <- Called item.instruction_id
          | (Opcode.Ic_add_rsp | Ic_add_rsp1), pending :: _ ->
              require_shape pending.shape;
              let call =
                match pending.phase with
                | Called call -> call
                | _ -> fail ?span "runtime cleanup has no completed call"
              in
              require ?span
                (item.result = None && (not push)
                && item.opcode = selected_cleanup pending.shape.selected_record
                )
                "runtime cleanup differs from the selected flag policy";
              let bytes =
                Int64.mul 8L (Int64.of_int (List.length pending.pushes))
              in
              require ?span
                (item.payload = Some (Seq.Integer bytes))
                "runtime cleanup byte count differs from its checked ABI slots";
              pending.phase <- Cleaned (call, item.instruction_id)
          | Opcode.Ic_call_end, pending :: rest ->
              require_shape pending.shape;
              let call_instruction_, cleanup_instruction_ =
                match pending.phase with
                | Cleaned (call, cleanup) -> (call, cleanup)
                | _ -> fail ?span "runtime call end has no matching cleanup"
              in
              require ?span
                (Seq.Instruction_id.equal item.instruction_id
                   pending.shape.source_description.last)
                "runtime call end differs from its checked source description";
              require ?span
                (match item.payload with
                | Some (Seq.Symbol symbol) ->
                    symbol == pending.shape.selected_symbol
                | _ -> false)
                "runtime call end changed its selected symbol";
              let result_value_ =
                match item.result with
                | Some value -> value.value_id
                | None -> fail ?span "runtime call end has no result identity"
              in
              let call =
                {
                  description = pending.shape.source_description;
                  provider_ = approved_provider pending.shape;
                  symbol_ = pending.shape.selected_symbol;
                  return_type_ = pending.shape.result_type;
                  call_opcode_ =
                    selected_opcode ?span pending.shape.selected_record;
                  cleanup_opcode_ =
                    selected_cleanup pending.shape.selected_record;
                  cleanup_bytes_ =
                    Int64.mul 8L (Int64.of_int (List.length pending.pushes));
                  call_instruction_;
                  cleanup_instruction_;
                  result_value_;
                  arguments_ = List.rev pending.pushes;
                  variadic_count_ =
                    Option.map
                      (fun _ ->
                        Int64.of_int (List.length pending.shape.variadic))
                      pending.shape.count_type;
                  declaration_ = pending.shape.selected_declaration;
                  header_ = pending.shape.selected_header;
                  retained_function_ = pending.shape.retained_function;
                }
              in
              calls := Instructions.add call.description.first call !calls;
              Option.iter
                (fun id ->
                  require ?span
                    (not (Instructions.mem id !discards))
                    "duplicate implicit discard identity";
                  require ?span
                    (Seq.Instruction_id.to_int id
                    = Seq.Instruction_id.to_int item.instruction_id + 1)
                    "implicit output discard does not immediately follow its \
                     call end";
                  discards := Instructions.add id call !discards)
                call.description.discard;
              stack := rest
          | ( ( Opcode.Ic_call
              | Ic_call_indirect2
              | Ic_call_extern
              | Ic_call_import
              | Ic_add_rsp
              | Ic_add_rsp1
              | Ic_call_end ),
              [] ) ->
              fail ?span
                "runtime call instruction has no checked enclosing scope"
          | _, { phase = Called _ | Cleaned _; _ } :: _ ->
              fail ?span "runtime call and cleanup are not canonically adjacent"
          | Opcode.Ic_end_exp, _ :: _ ->
              fail ?span
                "expression discard cannot occur inside a call argument scope"
          | _ -> ());
          if push then
            match !stack with
            | pending :: _ when pending.phase = Collecting ->
                let expected =
                  match pending.expected with
                  | value :: rest ->
                      pending.expected <- rest;
                      value
                  | [] ->
                      fail ?span "call has an extra pushed argument producer"
                in
                let value =
                  match item.result with
                  | Some value -> value.value_id
                  | None -> fail ?span "pushed argument has no result identity"
                in
                require ?span
                  (Option.fold ~none:false
                     ~some:(Type.equal expected.expected_source)
                     item.target_type)
                  "pushed argument class differs from its checked source value";
                require ?span
                  (item.span = expected.expected_origin)
                  "pushed argument does not retain its checked source origin";
                Option.iter
                  (fun count ->
                    require ?span
                      (item.opcode = Opcode.Ic_imm_i64
                      && item.operands = []
                      && item.payload = Some (Seq.Integer count)
                      && item.flags = push_flag)
                      "hidden variadic count is not its canonical checked \
                       immediate")
                  expected.expected_count;
                pending.pushes <-
                  {
                    role = expected.expected_role;
                    prepared_default = expected.expected_default;
                    producer = item.instruction_id;
                    value;
                    source_type = expected.expected_source;
                    target_type = expected.expected_target;
                  }
                  :: pending.pushes
            | _ ->
                fail ?span "pushed argument is outside a collecting call scope")
        items;
      require (!stack = []) "runtime call scope crosses a block boundary")
    blocks;
  require
    (Instructions.is_empty !remaining)
    "runtime call context has unused source descriptions";
  let all_items =
    List.concat_map
      (fun block ->
        block |> Block_graph.instructions |> Seq.instructions
        |> List.map Seq.description)
      blocks
  in
  Instructions.iter
    (fun id call ->
      let item =
        List.find_opt
          (fun (item : Seq.description) ->
            Seq.Instruction_id.equal item.instruction_id id)
          all_items
      in
      match item with
      | Some item ->
          require ?span:item.span
            (item.opcode = Opcode.Ic_end_exp
            && item.operands = [ call.result_value_ ]
            && item.result = None && item.target_type = None
            && item.payload = None && item.flags = 0x200L)
            "implicit discard does not consume its exact checked call result"
      | None -> fail "implicit output discard is absent from its exact graph")
    !discards;
  { owner; calls = !calls; discards = !discards }

let create ~records ~function_sources ~top_level ~initialization ~entry
    ~entry_calls ~functions =
  try
    let globals = Global_initialization.globals initialization in
    let provided = function
      | Typed.Provided_result value -> Some value
      | Typed.Declared_default_result _ -> None
    in
    let direct_resolution call =
      call |> Typed.direct_source
      |> Sema.Function_call_conversion_policy.direct_source
    in
    let expression_children value =
      match Typed.result_operand value with
      | Some operand -> [ operand ]
      | None -> (
          match Typed.result_binary_operands value with
          | Some (left, right) -> [ left; right ]
          | None -> (
              match Typed.result_index_operands value with
              | Some (base, index) -> [ base; index ]
              | None -> []))
    in
    let subtree_contains ~resolve ~arguments ~selected root =
      let rec visit = function
        | [] -> false
        | value :: rest -> (
            let children = expression_children value in
            match resolve value with
            | Some call when call == selected -> true
            | Some call ->
                visit (List.rev_append (arguments call) (children @ rest))
            | None -> visit (children @ rest))
      in
      visit [ root ]
    in
    let function_subtree_contains source selected root =
      let calls =
        List.filter_map
          (function
            | Typed.Direct_call_result call -> Some call
            | _ -> None)
          (Typed.function_calls source)
      in
      let resolve value =
        match Typed.result_call_resolution value with
        | Some (Resolution.Direct_call resolution) ->
            List.find_opt
              (fun call -> direct_resolution call == resolution)
              calls
        | _ -> None
      in
      let arguments call =
        List.filter_map
          (fun fixed -> provided (Typed.fixed_path fixed))
          (Typed.direct_fixed_results call)
        @ Typed.direct_variadic_results call
      in
      subtree_contains ~resolve ~arguments ~selected root
    in
    let top_calls = Typed.top_level_direct_calls top_level in
    let top_roots =
      List.concat_map Typed.top_level_statement_roots
        (Typed.top_level_statements top_level)
    in
    let top_level_subtree_contains selected root =
      (* The caller first proves physical root membership in this batch. Child
         accessors and call arguments retain that ownership; a numeric result
         ID alone cannot join a different checked expression to this call. *)
      let resolve value =
        List.find_opt
          (fun call ->
            Typed.Id.equal
              (Typed.top_level_direct_result_id call)
              (Typed.result_id value)
            && call |> Typed.top_level_direct_source
               |> Sema.Top_level_expression_tree.call_result_expression
               == Typed.result_source value)
          top_calls
      in
      let arguments call =
        List.filter_map
          (fun fixed -> provided (Typed.top_level_fixed_path fixed))
          (Typed.top_level_direct_fixed_results call)
        @ Typed.top_level_direct_variadic_results call
      in
      subtree_contains ~resolve ~arguments ~selected root
    in
    let entry_region ?span description =
      let region =
        Global_initialization.storage_regions initialization
        |> List.find_opt (fun region ->
            Seq.Instruction_id.compare description.first
              (Global_initialization.storage_last region)
            <= 0
            && Seq.Instruction_id.compare description.last
                 (Global_initialization.storage_first region)
               >= 0)
      in
      Option.iter
        (fun region ->
          require ?span
            (Seq.Instruction_id.compare description.first
               (Global_initialization.storage_first region)
             >= 0
            && Seq.Instruction_id.compare description.last
                 (Global_initialization.storage_last region)
               <= 0)
            "entry call crosses its checked initializer region")
        region;
      region
    in
    let source_function ?span symbol =
      match
        Typed.functions function_sources
        |> List.find_opt (fun source -> Typed.function_symbol source == symbol)
      with
      | Some source -> source
      | None ->
          fail ?span "runtime function owner has no exact typed source function"
    in
    let function_member source = function
      | Function_call target ->
          List.exists
            (function
              | Typed.Direct_call_result actual ->
                  actual
                  == Sema.Function_call_target_classification.source target
              | _ -> false)
            (Typed.function_calls source)
      | Function_output output ->
          let actual =
            output |> Sema.Implicit_output_argument_binding.bound_source
            |> Sema.Implicit_output_target_resolution.output_source
          in
          List.exists
            (fun expected -> actual == expected)
            (Typed.function_implicit_outputs source)
      | Top_level_call _ | Top_level_output _ -> false
    in
    let validate_source owner description span =
      match (owner, description.source) with
      | Function body, source ->
          require ?span
            (function_member
               (source_function ?span (Function_body.symbol body))
               source)
            "runtime call source is not owned by this exact typed function body"
      | Entry, Function_output _ ->
          fail ?span "function output statement cannot authorize a module entry"
      | Entry, (Function_call target as source) ->
          let region, frame =
            match entry_region ?span description with
            | Some region -> (
                match Global_initialization.storage_frame region with
                | Some frame -> (region, frame)
                | None ->
                    fail ?span
                      "function-scope entry call cannot belong to a global \
                       initializer")
            | None ->
                fail ?span
                  "function-scope entry call has no checked static-initializer \
                   owner"
          in
          let function_source =
            source_function ?span
              (Sema.Function_frame_layout.function_symbol frame)
          in
          require ?span
            (function_member function_source source)
            "entry call belongs to another static-initializer function";
          let static =
            Global_initialization.static_regions initialization
            |> List.find_opt (fun static ->
                let bounds = Global_initialization.describe_static static in
                Seq.Instruction_id.equal bounds.first
                  (Global_initialization.storage_first region)
                && Seq.Instruction_id.equal bounds.last
                     (Global_initialization.storage_last region))
          in
          let root =
            match static with
            | Some static -> Global_initialization.static_root static
            | None ->
                fail ?span "static-initializer region has no exact source root"
          in
          require ?span
            (List.exists
               (fun actual -> actual == root)
               (Typed.function_initializers function_source))
            "static-initializer root is foreign to its typed function";
          require ?span
            (function_subtree_contains function_source
               (Sema.Function_call_target_classification.source target)
               (Typed.initializer_value root))
            "entry call is absent from its exact static-initializer expression"
      | Entry, Top_level_call target ->
          let selected =
            Sema.Top_level_function_call_target_classification.source target
          in
          require ?span
            (List.exists (fun source -> source == selected) top_calls)
            "entry call source does not belong to the exact top-level batch";
          Option.iter
            (fun region ->
              require ?span
                (Option.is_none (Global_initialization.storage_frame region))
                "top-level entry call cannot belong to a static initializer";
              let root =
                match Global_initialization.storage_root region with
                | Some root -> root
                | None ->
                    fail ?span
                      "global-initializer region has no exact source root"
              in
              require ?span
                (List.exists (fun actual -> actual == root) top_roots)
                "global-initializer root is foreign to its top-level batch";
              require ?span
                (top_level_subtree_contains selected
                   (Typed.top_level_root_value root))
                "entry call is absent from its exact global-initializer \
                 expression")
            (entry_region ?span description)
      | Entry, Top_level_output output ->
          require ?span
            (Option.is_none (entry_region ?span description))
            "implicit output statement cannot belong to an initializer region";
          let module Target = Sema.Top_level_implicit_output_target_resolution
          in
          let target =
            Sema.Top_level_implicit_output_argument_binding.bound_source output
          in
          let statement = Target.output_statement target in
          require ?span
            (List.exists
               (fun source -> source == statement)
               (Typed.top_level_statements top_level))
            "entry output source does not belong to the exact top-level batch";
          let roots = Typed.top_level_statement_roots statement in
          List.iter
            (fun root ->
              require ?span
                (List.exists (fun source -> source == root) roots)
                "entry output root is foreign to its containing statement")
            (Option.to_list (Target.output_supplied_fixed_value target)
            @ Target.output_arguments target)
    in
    let rec checked_functions seen = function
      | [] -> []
      | (body, descriptions) :: rest ->
          require ?span:(Function_body.span body)
            (not (List.exists (fun other -> other == body) seen))
            "runtime call context repeats a function owner";
          ignore
            (source_function ?span:(Function_body.span body)
               (Function_body.symbol body));
          graph_context ~globals ~records ~validate_source (Function body)
            (Function_body.body body) descriptions
          :: checked_functions (body :: seen) rest
    in
    let graphs =
      graph_context ~globals ~records ~validate_source Entry
        (X87_stack.graph entry) entry_calls
      :: checked_functions [] functions
    in
    Ok
      {
        typed_top_level = top_level;
        dimension_dependencies_ =
          Dimension_requirements.top_level top_level
          @ Dimension_requirements.functions function_sources;
        entry;
        initialization;
        functions = List.map fst functions;
        graphs;
      }
  with Invalid error -> Error [ error ]

let dimension_dependencies context = context.dimension_dependencies_
let owns_top_level context typed = context.typed_top_level == typed
