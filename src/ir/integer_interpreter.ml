module Sequence = Instruction_sequence
module Graph = Block_graph
module X87 = X87_stack
module Block_id = Sequence.Block_id
module Instruction_id = Sequence.Instruction_id
module Value_id = Sequence.Value_id
module Frame = Sema.Function_frame_layout
module Function = Function_body
module Type = Sema.Type
module Runtime = Runtime_call_context
module Output = Integer_output
module Scalar = Integer_scalar_storage
module Computation = Sema.Integer_computation_class
module Offset_map = Map.Make (Int64)

module Value_map = Map.Make (struct
  type t = Value_id.t

  let compare = Value_id.compare
end)

module Block_map = Map.Make (struct
  type t = Block_id.t

  let compare = Block_id.compare
end)

module Instruction_map = Map.Make (struct
  type t = Instruction_id.t

  let compare = Instruction_id.compare
end)

type word_type = I64 | U64
type word = { type_ : word_type; bits : int64 }
type return_kind = Word_return of word_type | Void_return
type function_definition = { frame : Frame.function_layout; body : Function.t }

type task_function_source = {
  source_globals : Integer_globals.t;
  source_runtime_calls : Runtime_call_context.t;
  source_functions : function_definition list;
  source_definition : function_definition;
}

type stored_type =
  | Stored_word of word_type
  | Stored_narrow of Scalar.t
  | Stored_pointer of Type.t

type runtime_value =
  | Runtime_word of word
  | Runtime_pointer of runtime_address
  | Runtime_offset of int64
  | Runtime_void

and runtime_address = {
  pointer_storage : runtime_storage;
  pointer_base : int;
  pointer_count : int;
  pointer_element_bytes : int;
  pointer_extent_bytes : int64;
  pointer_offset : int64;
  pointer_pointee : Type.t;
}

and runtime_storage = {
  cells : runtime_value option array;
  mutable live : bool;
  unknown_message : string;
}

type frame_slot = {
  slot_type : Type.t;
  stored_type : stored_type;
  initial : runtime_value option;
  object_count : int;
  strides : int64 list;
}

type frame_context = {
  layout : Frame.function_layout;
  slots : frame_slot array;
  offsets : int Offset_map.t;
  return_type : Type.t;
  allocated_bytes : int;
  variadic_location : (int64 * Type.t) option;
  initial_variadic : runtime_value option array;
}

type termination = Stream_end | Returned of word option
type error_stage = Configuration | Preflight | Execution

type error = {
  stage : error_stage;
  code : string;
  message : string;
  executed_steps : int;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
  function_id : int option;
  function_name : string option;
  initializer_phase : Global_initialization.phase option;
  initializer_symbol_id : int option;
  initializer_name : string option;
}

type t = {
  termination_ : termination;
  executed_steps_ : int;
  final_value_ : word option;
  compiled_initializer_steps_ : int;
}

type report = {
  outcome_ : (t, error list) result;
  output_bytes_ : string;
  output_work_ : int;
}

type task_progress = {
  executed_steps : int;
  initializer_steps : int;
  global_bytes : int;
  literal_bytes : int;
  output_bytes : string;
  output_work : int;
  generated_bytes : int;
  final_value : word option;
}

let report_outcome report = report.outcome_
let report_output_bytes report = report.output_bytes_
let report_output_work report = report.output_work_

type literal_region = { literal_base : int; literal_count : int }

type literal_context = {
  literal_graph : Graph.t;
  literal_regions : literal_region Instruction_map.t;
}

type literal_image = {
  mutable literal_byte_count : int;
  mutable literal_chunks_rev : (int * string) list;
}

type prepared_operand = {
  value_id : Value_id.t;
  expected_type : word_type;
  computation_type : word_type;
}

type prepared_pointer = { pointer_value : Value_id.t; pointer_type : Type.t }

type prepared_value =
  | Word_operand of prepared_operand
  | Pointer_operand of prepared_pointer

type unary_operation = Complement | Logical_not | Negate

type comparison_operation =
  | Equal
  | Not_equal
  | Less
  | Greater_equal
  | Greater
  | Less_equal

type logical_operation = Logical_and | Logical_or | Logical_xor

type binary_operation =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_left
  | Shift_right
  | Compare of comparison_operation
  | Logical of logical_operation

type branch_condition = Zero | Not_zero

type storage_location =
  | Frame_slot of int * int
  | Variadic_slot
  | Global_slot of Integer_globals.storage_slot
  | Literal_slot of int * int
  | Indirect_slot of prepared_pointer
  | Indexed_slot of prepared_pointer

type prepared_operation =
  | Call_start of int option
  | Call of int
  | Retained_call of Retained_function.t
  | Extern_call of Runtime.call * stored_type array
  | Call_cleanup
  | Call_end of Value_id.t * word_type
  | Call_end_void of Value_id.t
  | Frame_address_tick
  | Scale_index of prepared_operand * int64 * Value_id.t
  | Index_address of storage_location * Value_id.t * Value_id.t * Type.t
  | Materialize_address of storage_location * Value_id.t * Type.t
  | Load_slot of storage_location * Value_id.t
  | Store_slot of storage_location * prepared_value * Value_id.t * stored_type
  | Update_slot of
      storage_location
      * binary_operation
      * prepared_operand option
      * bool
      * Value_id.t
      * stored_type
  | Immediate of Value_id.t * word
  | Unary of unary_operation * prepared_operand * Value_id.t * word_type
  | Word_view of prepared_operand * Value_id.t * word_type
  | Binary of
      binary_operation
      * prepared_operand
      * prepared_operand
      * Value_id.t
      * word_type
  | Discard of prepared_value
  | Discard_void of Value_id.t
  | Return_value of prepared_operand * word_type
  | Jump of int
  | Branch of branch_condition * prepared_operand * int
  | Return
  | End

type prepared_instruction = {
  instruction_id : Instruction_id.t;
  span : Common.Span.t option;
  operation : prepared_operation;
  push_result : prepared_value option;
  capture_discard : bool;
}

type prepared_block = {
  block_id : Block_id.t;
  instructions : prepared_instruction array;
  fallthrough : int option;
}

type prepared = {
  blocks : prepared_block array;
  entry_index : int;
  initial_slots : runtime_value option array;
  initial_frame_bytes : int;
  variadic_base : int option;
  is_function : bool;
  required_return : return_kind option;
  owner : (int * string) option;
}

type callee = {
  callee_index : int;
  callee_symbol : Sema.Symbol.t;
  callee_definition : Sema.Function_resolution.resolved_declaration option;
  callee_return_type : Type.t;
  parameter_types : stored_type array;
  cleanup_opcode : Opcode.t;
  frame_bytes : int;
  variadic : bool;
}

(* A prepared index and a literal offset are meaningful only in the command
   that admitted them. Keep that owner when a body outlives its entry. *)
type executable_owner = {
  owner_callees : (callee * prepared) array;
  owner_literals : runtime_storage;
}

type retained_executable = {
  function_link : Retained_function.t;
  function_callee : callee;
  function_program : prepared;
  function_owner : executable_owner;
  function_source : task_function_source;
}

type task_stream = { stream_output : Output.t }

type admitted_publication =
  | Admitted_declared_global of
      Retained_global.t * Integer_globals.declared_slot
  | Admitted_global of Retained_global.t * Integer_globals.slot
  | Admitted_function of Retained_function.t

type task_admission = {
  admission_catalog : Integer_globals.task_catalog;
  admission_globals : Integer_globals.t;
  admission_entry : X87.t;
  admission_publications : admitted_publication list;
}

type task_source_program = {
  source_entry : X87.t;
  source_storage : Integer_globals.t;
  source_initialization : Global_initialization.t;
  source_calls : Runtime.t;
  source_bodies : function_definition list;
}

type isolated_preparation = {
  preparation_catalog : Integer_globals.task_catalog;
  mutable preparation_steps : int;
  mutable preparation_closed : bool;
}

type initializer_attempt_state =
  | Preparing_initializer
  | Executing_initializer
  | Successful_initializer
  | Failed_initializer

type task_initializer = {
  initializer_catalog : Integer_globals.task_catalog;
  initializer_slot : Integer_globals.declared_slot;
  initializer_start : Frontend.Parser.global_initializer_start;
  mutable initializer_cursor : Integer_initializer_layout.live;
  mutable initializer_seen : Frontend.Parser.completed_initializer_leaf list;
  mutable initializer_attempt : initializer_attempt option;
  mutable initializer_complete : bool;
}

and initializer_attempt = {
  attempt_initializer : task_initializer;
  attempt_leaf : Sema.Initializer_source.leaf;
  attempt_receipt : Frontend.Parser.completed_initializer_leaf;
  attempt_destination : Integer_initializer_layout.entry;
  attempt_next : Integer_initializer_layout.live;
  attempt_preparation_before : int;
  mutable attempt_state : initializer_attempt_state;
}

type default_attempt = {
  default_catalog : Integer_globals.task_catalog;
  default_publication : Sema.Declaration_collection.publication;
  default_receipt : Frontend.Parser.completed_parameter_default;
  default_preparation_before : int;
  mutable default_state : initializer_attempt_state;
  mutable default_bits : int64 option;
}

type dimension_attempt = {
  dimension_catalog : Integer_globals.task_catalog;
  dimension_authority : Sema.Dimension_fragment.authority;
  dimension_receipt : Frontend.Parser.array_dimension_preparation;
  dimension_preparation_before : int;
  mutable dimension_state : initializer_attempt_state;
  mutable dimension_bits : int64 option;
  mutable dimension_work : int option;
}

type task_input = {
  input_context : Frontend.Parser.command_context;
  input_streams : task_stream list;
  input_failure : unit ref;
  input_seen_dimensions : Frontend.Parser.array_dimension_preparation list;
  input_dimensions : dimension_attempt list;
  input_defaults : default_attempt list;
  input_initializers : task_initializer list;
  input_ready : bool;
  mutable input_value : word option;
  mutable input_result :
    (Frontend.Parser.completed_sequence * (t, string) result) option;
}

type task_call_start = {
  call_capture : Sema.Function_record_phase.call_start_snapshot;
  call_start : Frontend.Parser.call_start;
  call_namespace : Sema.Declaration_collection.namespace;
  call_selected : Retained_function.t;
  call_arguments : Sema.Function_type_resolution.resolved_function;
  mutable call_completed : bool;
}

type task_implicit_call_start = {
  implicit_capture : Sema.Function_record_phase.implicit_arguments_snapshot;
  implicit_start : Frontend.Parser.implicit_output_selection;
  implicit_namespace : Sema.Declaration_collection.namespace;
  implicit_selected : Retained_function.t;
  implicit_arguments : Sema.Function_type_resolution.resolved_function;
  mutable implicit_completed : bool;
}

type task_state = {
  mutable implicit_selections :
    (Frontend.Parser.implicit_output_selection * Retained_function.t) list;
  mutable implicit_starts : task_implicit_call_start list;
  mutable call_selections :
    (Frontend.Parser.reference_selection * Retained_function.t) list;
  mutable call_starts : task_call_start list;
  mutable call_phases : Sema.Function_call_phase.t list;
  mutable inputs : task_input list;
  mutable failure_generation : unit ref;
  mutable seen_dimensions : Frontend.Parser.array_dimension_preparation list;
  mutable closed_dimensions : Sema.Compiler_record.dimension_preparation list;
  mutable completed_dimensions : Frontend.Parser.completed_array_dimension list;
  mutable dimensions : dimension_attempt list;
  mutable defaults : default_attempt list;
  mutable initializers : task_initializer list;
  mutable declared_admissions : admitted_publication list;
  mutable source_promotion_open : bool;
  mutable source_activation : Sema.Source_activation.t option;
  mutable deferred_dimensions : Sema.Compiler_record.dimension_preparation list;
  mutable deferred_offsets : Sema.Compiler_record.aggregate_offset list;
  mutable charged_offsets : Sema.Compiler_record.aggregate_offset list;
  mutable attempted_offsets : Frontend.Parser.aggregate_phase list;
  mutable source_execution_failed : bool;
  mutable source_result : (Frontend.Parser.completed_sequence * t) option;
  catalog : Integer_globals.task_catalog;
  mutable arenas : (Integer_globals.t * runtime_storage) list;
  mutable literal_arenas : runtime_storage list;
  mutable started : X87.t list;
  mutable functions : retained_executable list;
  mutable global_bytes : int;
  mutable literal_bytes : int;
  mutable steps : int;
  mutable initializer_steps : int;
  mutable outer_value : word option;
  max_steps : int;
  max_initializer_steps : int;
  max_global_bytes : int;
  max_literal_bytes : int;
  max_frame_bytes : int;
  max_call_depth : int;
  output : Output.t;
  generated : Output.t;
  max_stream_depth : int;
  mutable streams : task_stream list;
  mutable admissions : task_admission list;
  mutable source_programs : task_source_program list;
  mutable isolated_programs : task_source_program list;
}

let create_task_state ?(max_steps = 100_000) ?(max_initializer_steps = 100_000)
    ?(max_global_bytes = 1_048_576) ?(max_literal_bytes = 1_048_576)
    ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128)
    ?(max_output_bytes = 1_048_576) ?(max_output_work = 1_048_576)
    ?(max_generated_bytes = 16 * 1024 * 1024) ?(max_stream_depth = 64) ~table ()
    =
  if
    List.exists
      (fun limit -> limit <= 0)
      [
        max_steps;
        max_initializer_steps;
        max_global_bytes;
        max_literal_bytes;
        max_frame_bytes;
        max_call_depth;
        max_output_bytes;
        max_output_work;
        max_stream_depth;
      ]
    || max_output_bytes > Sys.max_string_length
    || max_generated_bytes < 0
    || max_generated_bytes > Sys.max_string_length
  then
    Error
      "task limits must be positive, generated capacity must be nonnegative, \
       and output capacities must fit host strings"
  else
    let output = Output.create ~max_output_bytes ~max_output_work in
    Ok
      {
        call_starts = [];
        call_selections = [];
        implicit_selections = [];
        implicit_starts = [];
        call_phases = [];
        defaults = [];
        dimensions = [];
        initializers = [];
        declared_admissions = [];
        source_promotion_open = true;
        source_activation = None;
        deferred_dimensions = [];
        deferred_offsets = [];
        charged_offsets = [];
        attempted_offsets = [];
        seen_dimensions = [];
        closed_dimensions = [];
        completed_dimensions = [];
        source_execution_failed = false;
        inputs = [];
        failure_generation = ref ();
        source_result = None;
        catalog = Integer_globals.create_task_catalog ~table;
        arenas = [];
        literal_arenas = [];
        started = [];
        functions = [];
        global_bytes = 0;
        literal_bytes = 0;
        steps = 0;
        initializer_steps = 0;
        outer_value = None;
        max_steps;
        max_initializer_steps;
        max_global_bytes;
        max_literal_bytes;
        max_frame_bytes;
        max_call_depth;
        output;
        generated =
          Output.share_work output ~max_output_bytes:max_generated_bytes;
        max_stream_depth;
        streams = [];
        admissions = [];
        source_programs = [];
        isolated_programs = [];
      }

let begin_task_stream task =
  task.source_promotion_open <- false;
  if List.length task.streams >= task.max_stream_depth then
    Error "HCIRVM0029: the task generation nesting limit was exhausted"
  else
    let stream = { stream_output = Output.fork task.generated } in
    task.streams <- stream :: task.streams;
    Ok stream

let task_stream_is_active task stream =
  match task.streams with
  | active :: _ -> active == stream
  | [] -> false

let finish_task_stream task stream =
  match task.streams with
  | active :: rest when active == stream ->
      let contents = Output.contents stream.stream_output in
      task.streams <- rest;
      Ok contents
  | _ -> Error "HCIRVM0027: generation buffer is not active in this task"

let abort_task_stream task stream =
  match task.streams with
  | active :: rest when active == stream ->
      task.streams <- rest;
      Ok ()
  | _ -> Error "HCIRVM0027: generation buffer is not active in this task"

let task_snapshot task = Integer_globals.snapshot_task task.catalog
let task_source_order task = Integer_globals.task_source_order task.catalog

let rec input_prefix_complete before current complete =
  current == before
  ||
  match current with
  | [] -> false
  | item :: rest -> complete item && input_prefix_complete before rest complete

let input_has_active_work task =
  let active = function
    | Preparing_initializer | Executing_initializer -> true
    | _ -> false
  in
  List.exists (fun attempt -> active attempt.default_state) task.defaults
  || List.exists (fun attempt -> active attempt.dimension_state) task.dimensions
  || List.exists
       (fun state ->
         Option.fold ~none:false
           ~some:(fun attempt -> active attempt.attempt_state)
           state.initializer_attempt)
       task.initializers

let completed_input task input =
  input.input_ready
  && input.input_failure == task.failure_generation
  && input.input_streams == task.streams
  && Sema.Source_activation.finished task.source_activation
  && task.deferred_dimensions = []
  && task.deferred_offsets = []
  && input_prefix_complete input.input_seen_dimensions task.seen_dimensions
       (fun preparation ->
         List.exists
           (fun receipt ->
             receipt.Frontend.Parser.dimension_preparation == preparation)
           task.completed_dimensions)
  && input_prefix_complete input.input_dimensions task.dimensions
       (fun attempt -> attempt.dimension_state = Successful_initializer)
  && input_prefix_complete input.input_defaults task.defaults (fun attempt ->
      attempt.default_state = Successful_initializer)
  && input_prefix_complete input.input_initializers task.initializers
       (fun state -> state.initializer_complete)

let observe_task_source_event task event =
  Result.map
    (fun () ->
      match event with
      | Frontend.Parser.Sequence_started context
        when Option.is_none (Frontend.Parser.context_parent context) ->
          task.inputs <-
            {
              input_context = context;
              input_streams = task.streams;
              input_failure = task.failure_generation;
              input_seen_dimensions = task.seen_dimensions;
              input_dimensions = task.dimensions;
              input_defaults = task.defaults;
              input_initializers = task.initializers;
              input_ready = not (input_has_active_work task);
              input_value = None;
              input_result = None;
            }
            :: task.inputs
      | Frontend.Parser.Sequence_aborted _ -> task.failure_generation <- ref ()
      | Frontend.Parser.Sequence_completed sequence
        when Option.is_none
               (Frontend.Parser.context_parent sequence.sequence_context)
             && (not (Frontend.Parser.sequence_accepted sequence))
             && Result.is_ok
                  (Integer_globals.check_source_completion
                     ~require_accepted:false task.catalog sequence) ->
          let result =
            {
              termination_ = Stream_end;
              executed_steps_ = task.steps;
              compiled_initializer_steps_ = task.initializer_steps;
              final_value_ = task.outer_value;
            }
          in
          task.source_result <- Some (sequence, result);
          List.iter
            (fun input ->
              if input.input_context == sequence.sequence_context then
                input.input_result <-
                  Some
                    ( sequence,
                      if completed_input task input then
                        Ok { result with final_value_ = input.input_value }
                      else
                        Error
                          "task input requires successful completion of its \
                           original execution" ))
            task.inputs
      | _ -> ())
    (match event with
    | Frontend.Parser.Sequence_started context
      when not (Frontend.Parser.context_is_current context ~observed_events:1)
      -> Error "task input start is outside its original parser callback"
    | _ -> Sema.Task_command_order.observe (task_source_order task) event)

let start_task_compilation task = task.source_promotion_open <- false

let bind_task_namespace task namespace =
  Integer_globals.bind_task_namespace task.catalog namespace

let promote_task_source ?(offsets = []) ?(dimensions = [])
    ?(completed_dimensions = []) task ~namespace ~events ~dimension_steps =
  let module Record = Sema.Compiler_record in
  let originals = List.map Record.dimension_preparation_source dimensions in
  let completions = List.map Record.dimension_receipt completed_dimensions in
  let offset_work =
    List.fold_left
      (fun total offset -> total + Record.aggregate_offset_work offset)
      0 offsets
  in
  let offsets_valid =
    let rec loop seen = function
      | [] -> true
      | offset :: rest ->
          let phase = Record.aggregate_offset_phase offset in
          Record.aggregate_offset_namespace offset == namespace
          && (not (List.exists (( == ) phase) seen))
          && List.exists
               (function
                 | Frontend.Parser.Command_started start ->
                     start
                     == phase.phase_aggregate.aggregate_header
                          .declaration_command
                 | _ -> false)
               events
          && loop (phase :: seen) rest
    in
    loop [] offsets
  in
  let manifest_valid =
    let rec preparations total seen = function
      | [] -> total = dimension_steps
      | prepared :: rest ->
          let source = Record.dimension_preparation_source prepared in
          let work = Record.dimension_preparation_work prepared in
          Record.dimension_preparation_namespace prepared == namespace
          && Record.dimension_preparation_runtime_dependencies prepared = []
          && (not (List.exists (( == ) source) seen))
          && List.exists
               (function
                 | Frontend.Parser.Command_started start ->
                     start == source.dimension_owner.dimensions_command
                 | _ -> false)
               events
          && (match source.dimension_predecessor with
            | None -> source.dimension_index = 0
            | Some prior ->
                List.exists (( == ) prior) completions
                && List.exists (( == ) prior.dimension_preparation) seen)
          && work >= 0
          && work <= dimension_steps - total
          && preparations (total + work) (source :: seen) rest
    in
    preparations 0 [] dimensions
    && List.for_all
         (fun checked ->
           List.exists
             (( == ) (Record.declared_dimension_preparation checked))
             dimensions)
         completed_dimensions
    && List.length completions
       = List.length
           (List.sort_uniq compare
              (List.map
                 (fun receipt ->
                   let rec position index = function
                     | [] -> -1
                     | source :: rest ->
                         if
                           source
                           == receipt.Frontend.Parser.dimension_preparation
                         then index
                         else position (index + 1) rest
                   in
                   position 0 originals)
                 completions))
  in
  if not task.source_promotion_open then
    Error "source promotion requires a fresh task runtime"
  else if
    dimension_steps < 0
    || dimension_steps > task.max_initializer_steps
    || offset_work > task.max_initializer_steps - dimension_steps
  then Error "source dimension work exceeds the task preparation allowance"
  else if (not manifest_valid) || not offsets_valid then
    Error "source promotion requires its original closed dimension manifest"
  else
    Result.bind (Integer_globals.check_task_namespace task.catalog namespace)
      (fun () ->
        Sema.Task_command_order.import_source_events (task_source_order task)
          events)
    |> fun result ->
    Result.bind result (fun () -> bind_task_namespace task namespace)
    |> Result.map (fun () ->
        task.initializer_steps <- dimension_steps + offset_work;
        task.charged_offsets <- offsets;
        task.seen_dimensions <- originals;
        task.closed_dimensions <- dimensions;
        task.completed_dimensions <- completions;
        task.source_promotion_open <- false)

let bind_source_activation task ~namespace activation =
  if
    Option.is_some task.source_activation
    || (not (Sema.Source_activation.owns_namespace activation namespace))
    || not (Integer_globals.task_catalog_owns_namespace task.catalog namespace)
  then Error "source activation belongs to another or already activated task"
  else (
    task.source_activation <- Some activation;
    Ok ())

let promote_task_source_activation ?(offsets = []) ?pending_runtime_dimension
    task ~namespace ~activation ~dimensions =
  let originals = Sema.Source_activation.dimension_preparations activation in
  let pending_valid, closed_originals =
    match pending_runtime_dimension with
    | None -> (true, originals)
    | Some pending -> (
        let valid =
          Frontend.Parser.dimension_preparation_is_current pending
          && Option.fold ~none:false ~some:(( == ) pending)
               (Sema.Source_activation.trailing_dimension_preparation activation)
          && Option.fold ~none:false
               ~some:(fun expression ->
                 Sema.Initializer_source.expression_identifier_nodes expression
                 <> [])
               pending.dimension_expression
        in
        match List.rev originals with
        | last :: rest when valid && last == pending -> (true, List.rev rest)
        | _ -> (false, originals))
  in
  if
    Option.is_some task.source_activation
    || (not (Sema.Source_activation.available activation))
    || (not (Sema.Source_activation.owns_namespace activation namespace))
    || (not pending_valid)
    || (let phases =
          Sema.Source_activation.aggregate_offset_phases activation
        in
        List.length phases <> List.length offsets
        || not
             (List.for_all2
                (fun phase offset ->
                  Sema.Compiler_record.aggregate_offset_phase offset == phase
                  && Sema.Compiler_record.aggregate_offset_namespace offset
                     == namespace)
                phases offsets))
    || List.length closed_originals <> List.length dimensions
    || not
         (List.for_all2
            (fun original checked ->
              Sema.Compiler_record.dimension_preparation_source checked
              == original
              && Sema.Compiler_record.dimension_preparation_namespace checked
                 == namespace)
            closed_originals dimensions)
  then
    Error "source activation requires its original checked dimension manifest"
  else
    promote_task_source task ~namespace
      ~events:(Sema.Source_activation.command_events activation)
      ~dimension_steps:0
    |> Result.map (fun () ->
        task.source_activation <- Some activation;
        task.deferred_offsets <- offsets;
        task.deferred_dimensions <- dimensions)

let charge_source_aggregate_offset task phase =
  match task.deferred_offsets with
  | offset :: rest
    when (not task.source_execution_failed)
         && Sema.Compiler_record.aggregate_offset_phase offset == phase
         && Sema.Source_activation.aggregate_offset_preparing
              task.source_activation phase ->
      let work = Sema.Compiler_record.aggregate_offset_work offset in
      let remaining = task.max_initializer_steps - task.initializer_steps in
      task.initializer_steps <- task.initializer_steps + min work remaining;
      task.deferred_offsets <- rest;
      task.charged_offsets <- offset :: task.charged_offsets;
      if work > remaining then (
        task.source_execution_failed <- true;
        task.failure_generation <- ref ();
        Error
          "HCIRVM0007: the bounded aggregate offset preparation work limit was \
           exhausted")
      else Ok ()
  | _ ->
      Error
        "aggregate offset charge is foreign, repeated or outside its \
         activation event"

let source_dimensions_ready task =
  (match (task.deferred_offsets, task.source_activation) with
    | [], _ -> true
    | next :: _, Some activation ->
        Sema.Source_activation.before_aggregate_offset activation
          (Sema.Compiler_record.aggregate_offset_phase next)
    | _ -> false)
  &&
  match (task.deferred_dimensions, task.source_activation) with
  | [], _ -> true
  | next :: _, Some activation ->
      Sema.Source_activation.before_dimension activation
        (Sema.Compiler_record.dimension_preparation_source next)
  | _ -> false

let charge_isolated_aggregate_offsets task ~table offsets =
  let module Record = Sema.Compiler_record in
  let rec valid seen = function
    | [] -> true
    | offset :: rest ->
        Record.aggregate_offset_table offset == table
        && (not
              (List.exists
                 (fun original ->
                   Record.aggregate_offset_phase original
                   == Record.aggregate_offset_phase offset)
                 seen))
        && valid (offset :: seen) rest
  in
  if not (valid task.charged_offsets offsets) then
    Error "isolated offsets have a foreign table or repeated preparation"
  else (
    task.source_promotion_open <- false;
    task.charged_offsets <- offsets @ task.charged_offsets;
    let rec charge = function
      | [] -> Ok ()
      | offset :: rest ->
          let work = Record.aggregate_offset_work offset in
          let remaining = task.max_initializer_steps - task.initializer_steps in
          task.initializer_steps <- task.initializer_steps + min work remaining;
          if work > remaining then
            Error
              "HCIRVM0007: the bounded aggregate offset preparation work limit \
               was exhausted"
          else charge rest
    in
    charge offsets)

let dimension_predecessor_ready task
    (preparation : Frontend.Parser.array_dimension_preparation) =
  match preparation.dimension_predecessor with
  | None -> preparation.dimension_index = 0
  | Some prior ->
      prior.dimension_preparation.dimension_owner == preparation.dimension_owner
      && prior.dimension_preparation.dimension_index
         = preparation.dimension_index - 1
      && List.exists (( == ) prior) task.completed_dimensions

let charge_source_dimension task preparation =
  match task.deferred_dimensions with
  | next :: rest
    when (not task.source_execution_failed)
         && dimension_predecessor_ready task preparation
         && (not (List.exists (( == ) preparation) task.seen_dimensions))
         && Sema.Compiler_record.dimension_preparation_source next
            == preparation
         && Sema.Source_activation.dimension_preparing task.source_activation
              preparation ->
      let work = Sema.Compiler_record.dimension_preparation_work next in
      task.seen_dimensions <- preparation :: task.seen_dimensions;
      let remaining = task.max_initializer_steps - task.initializer_steps in
      task.initializer_steps <- task.initializer_steps + min work remaining;
      if work > remaining then (
        task.source_execution_failed <- true;
        task.failure_generation <- ref ();
        Error
          "HCIRVM0007: the bounded array dimension preparation work limit was \
           exhausted")
      else (
        task.deferred_dimensions <- rest;
        task.closed_dimensions <- next :: task.closed_dimensions;
        Ok ())
  | _ ->
      Error
        "source dimension charge is repeated, foreign or outside its \
         activation event"

let matches_source_program program ~runtime_calls ~globals ~initialization
    ~functions entry =
  program.source_entry == entry
  && program.source_storage == globals
  && program.source_initialization == initialization
  && program.source_calls == runtime_calls
  && List.length program.source_bodies = List.length functions
  && List.for_all2
       (fun (left : function_definition) (right : function_definition) ->
         left.frame == right.frame && left.body == right.body)
       program.source_bodies functions

let bind_task_source_program task ~runtime_calls ~globals ~initialization
    ~functions entry =
  task.source_promotion_open <- false;
  if
    (not (Integer_globals.owns_task_storage task.catalog globals))
    || not (Integer_globals.has_source_command globals)
  then Error "source program requires its owning task and source storage proof"
  else
    match
      List.find_opt
        (fun program -> program.source_storage == globals)
        task.source_programs
    with
    | Some program ->
        if
          matches_source_program program ~runtime_calls ~globals ~initialization
            ~functions entry
        then Ok ()
        else
          Error
            "task source storage is already bound to another compiled program"
    | None ->
        task.source_programs <-
          {
            source_entry = entry;
            source_storage = globals;
            source_initialization = initialization;
            source_calls = runtime_calls;
            source_bodies = functions;
          }
          :: task.source_programs;
        Ok ()

let task_owns_snapshot task view =
  Integer_globals.task_catalog_owns_view task.catalog view

let task_owns_table task table =
  Integer_globals.task_catalog_owns_table task.catalog table

let task_admission task ~globals ~entry =
  List.find_opt
    (fun receipt ->
      receipt.admission_globals == globals && receipt.admission_entry == entry)
    task.admissions

let owns_task_admission task receipt =
  receipt.admission_catalog == task.catalog
  && List.exists (fun saved -> saved == receipt) task.admissions

let admission_publications receipt = receipt.admission_publications
let latest_task_admission task = List.nth_opt task.admissions 0

let admitted_source_symbol = function
  | Admitted_declared_global (reference, _) -> Retained_global.symbol reference
  | Admitted_global (reference, _) -> Retained_global.symbol reference
  | Admitted_function reference ->
      Retained_function.metadata reference
      |> Sema.Outer_environment.function_declaration
      |> Sema.Function_resolution.resolved_declaration_header
      |> Sema.Function_type_resolution.function_symbol

let admitted_publication_for_symbol task symbol =
  let completed () =
    List.find_map
      (fun receipt ->
        List.find_opt
          (fun publication -> admitted_source_symbol publication == symbol)
          receipt.admission_publications)
      task.admissions
  in
  match
    List.find_opt
      (fun publication -> admitted_source_symbol publication == symbol)
      task.declared_admissions
  with
  | Some (Admitted_function _ as pending) -> (
      match completed () with
      | Some _ as current -> current
      | None -> Some pending)
  | Some publication -> Some publication
  | None -> completed ()

let check_function_header_source task ~namespace source =
  let header = Sema.Compiler_record.declared_function_source source in
  if
    (not (source_dimensions_ready task))
    || (not
          (Sema.Source_activation.default_completion task.source_activation
             header))
    || not
         (Frontend.Parser.function_header_is_current header
         || Sema.Source_activation.function_header task.source_activation header
         )
  then
    Error
      "pending header admission is outside its original live or active event"
  else
    Integer_globals.check_function_header_source task.catalog ~namespace source

let function_record_head task snapshot =
  Integer_globals.function_record_head task.catalog snapshot

let observe_task_function_selection task ~namespace ~selection ~selected =
  let module A = Sema.Source_activation in
  let module P = Frontend.Parser in
  let module N = Sema.Function_record_phase in
  let module F = Sema.Function_resolution in
  let snapshot =
    Retained_function.metadata selected
    |> Sema.Outer_environment.function_declaration
    |> F.resolved_declaration_site |> F.declaration_site_native_snapshot
  in
  let matches =
    Option.fold ~none:true
      ~some:(fun snapshot ->
        let rec original entry =
          match Frontend.Symbol_visibility.function_alias_original entry with
          | Some source -> original source
          | None -> entry
        in
        let source_matches =
          match P.selected_lookup selection with
          | Frontend.Symbol_visibility.Present entry ->
              let entry = original entry in
              entry == (N.source snapshot).function_entry
              || Option.fold ~none:false
                   ~some:(fun header -> entry == header.P.completed_entry)
                   (Sema.Provisional_function.completed_header
                      (N.source_snapshot snapshot))
          | _ -> false
        in
        source_matches
        && Option.fold ~none:false
             ~some:(Retained_function.same selected)
             (function_record_head task snapshot))
      snapshot
  in
  if
    not
      (Integer_globals.task_catalog_owns_namespace task.catalog namespace
      && matches
      && Integer_globals.task_catalog_contains_function task.catalog selected
      && A.reference_admission task.source_activation selection
      && (P.reference_selection_is_current selection
         || A.reference task.source_activation selection)
      && not
           (List.exists
              (fun (original, _) -> original == selection)
              task.call_selections))
  then Error "function selection requires its original admitted task reference"
  else (
    task.call_selections <- (selection, selected) :: task.call_selections;
    Ok ())

let capture_task_call_start task ~namespace ~capture ~selected ~arguments =
  let module A = Sema.Source_activation in
  let module N = Sema.Function_record_phase in
  let module F = Sema.Function_resolution in
  let module P = Frontend.Parser in
  let start = N.call_start_receipt capture in
  let declaration =
    Retained_function.metadata selected
    |> Sema.Outer_environment.function_declaration
  in
  let selected_snapshot =
    F.resolved_declaration_site declaration
    |> F.declaration_site_native_snapshot
  in
  let snapshot =
    Option.map N.shape_snapshot
      (Sema.Function_type_resolution.function_provisional_call arguments)
  in
  let rec original_entry entry =
    match Frontend.Symbol_visibility.function_alias_original entry with
    | Some source -> original_entry source
    | None -> entry
  in
  let selected_source_matches snapshot =
    match P.selected_lookup start.P.call_reference with
    | Frontend.Symbol_visibility.Present entry ->
        let entry = original_entry entry in
        entry == (N.source snapshot).function_entry
        || Option.fold ~none:false
             ~some:(fun header -> entry == header.P.completed_entry)
             (Sema.Provisional_function.completed_header
                (N.source_snapshot snapshot))
    | _ -> false
  in
  if
    not
      (Integer_globals.task_catalog_owns_namespace task.catalog namespace
      && List.exists
           (fun (original, retained) ->
             original == start.P.call_reference
             && Retained_function.same retained selected)
           task.call_selections
      && A.call_start_admission task.source_activation start
      && (P.call_start_is_current start
         || A.call_start task.source_activation start)
      && not
           (List.exists
              (fun original -> original.call_start == start)
              task.call_starts))
  then Error "call arguments are outside their original task event"
  else
    match (selected_snapshot, snapshot) with
    | Some selected_snapshot, Some snapshot
      when N.owns_namespace snapshot namespace
           && snapshot == N.call_argument_snapshot capture
           && selected_source_matches selected_snapshot
           && N.same_identity selected_snapshot snapshot -> (
        match function_record_head task snapshot with
        | None -> Error "call arguments have no admitted native function"
        | Some current
          when Option.fold ~none:false ~some:(N.same_cursor snapshot)
                 (Retained_function.metadata current
                 |> Sema.Outer_environment.function_declaration
                 |> F.resolved_declaration_site
                 |> F.declaration_site_native_snapshot) ->
            let pending =
              {
                call_capture = capture;
                call_start = start;
                call_namespace = namespace;
                call_selected = selected;
                call_arguments = arguments;
                call_completed = false;
              }
            in
            task.call_starts <- pending :: task.call_starts;
            Ok pending
        | Some _ ->
            Error "call arguments do not match the current native cursor")
    | _ -> Error "call arguments differ from their selected native allocation"

let capture_task_call_emission task ~table ~capture pending =
  let module A = Sema.Source_activation in
  let module P = Frontend.Parser in
  let receipt = Sema.Function_record_phase.call_emission_receipt capture in
  let snapshot = Sema.Function_record_phase.call_emission_snapshot capture in
  if
    not
      (Integer_globals.task_catalog_owns_table task.catalog table
      && Sema.Function_record_phase.call_emission_arguments capture
         == pending.call_capture
      && List.exists (( == ) pending) task.call_starts
      && (not pending.call_completed)
      && receipt.P.call_start == pending.call_start
      && A.call_emission_admission task.source_activation receipt
      && (P.call_emission_is_current receipt
         || A.call_emission task.source_activation receipt))
  then Error "call emission is outside its original task event"
  else
    match function_record_head task snapshot with
    | None -> Error "call emission has no admitted native function"
    | Some current ->
        let emission =
          Retained_function.metadata current
          |> Sema.Outer_environment.function_classified_declaration
        in
        if
          not
            (Option.fold ~none:false
               ~some:(Sema.Function_record_phase.same_cursor snapshot)
               (Sema.Function_record_classification
                .classified_declaration_source emission
               |> Sema.Function_resolution.resolved_declaration_site
               |> Sema.Function_resolution.declaration_site_native_snapshot))
        then Error "call emission does not match the current native cursor"
        else
          Result.map
            (fun phase ->
              pending.call_completed <- true;
              task.call_phases <- phase :: task.call_phases;
              phase)
            (Sema.Function_call_phase.create ~table
               ~namespace:pending.call_namespace ~receipt
               ~selected:
                 (Retained_function.metadata pending.call_selected
                 |> Sema.Outer_environment.function_declaration)
               ~arguments:pending.call_arguments ~emission_snapshot:snapshot
               ~emission)

let observe_task_implicit_selection task ~namespace ~selection ~selected =
  let module A = Sema.Source_activation in
  let module P = Frontend.Parser in
  let module N = Sema.Function_record_phase in
  let module F = Sema.Function_resolution in
  let snapshot =
    Retained_function.metadata selected
    |> Sema.Outer_environment.function_declaration
    |> F.resolved_declaration_site |> F.declaration_site_native_snapshot
  in
  let matches =
    Option.fold ~none:true
      ~some:(fun snapshot ->
        let rec original entry =
          match Frontend.Symbol_visibility.function_alias_original entry with
          | Some source -> original source
          | None -> entry
        in
        let source_matches =
          match
            match P.implicit_lookup selection with
            | Some entry -> Frontend.Symbol_visibility.Present entry
            | None -> Frontend.Symbol_visibility.Absent
          with
          | Frontend.Symbol_visibility.Present entry ->
              let entry = original entry in
              entry == (N.source snapshot).function_entry
              || Option.fold ~none:false
                   ~some:(fun header -> entry == header.P.completed_entry)
                   (Sema.Provisional_function.completed_header
                      (N.source_snapshot snapshot))
          | _ -> false
        in
        source_matches
        && Option.fold ~none:false
             ~some:(Retained_function.same selected)
             (function_record_head task snapshot))
      snapshot
  in
  if
    not
      (Integer_globals.task_catalog_owns_namespace task.catalog namespace
      && matches
      && Integer_globals.task_catalog_contains_function task.catalog selected
      && A.implicit_selection_admission task.source_activation selection
      && (P.implicit_selection_is_current selection
         || A.implicit_output task.source_activation selection)
      && not
           (List.exists
              (fun (original, _) -> original == selection)
              task.implicit_selections))
  then Error "function selection requires its original admitted task reference"
  else (
    task.implicit_selections <-
      (selection, selected) :: task.implicit_selections;
    Ok ())

let capture_task_implicit_arguments task ~namespace ~capture ~selected
    ~arguments =
  let module A = Sema.Source_activation in
  let module N = Sema.Function_record_phase in
  let module F = Sema.Function_resolution in
  let module P = Frontend.Parser in
  let start = N.implicit_arguments_receipt capture in
  let declaration =
    Retained_function.metadata selected
    |> Sema.Outer_environment.function_declaration
  in
  let selected_snapshot =
    F.resolved_declaration_site declaration
    |> F.declaration_site_native_snapshot
  in
  let snapshot =
    Option.map N.shape_snapshot
      (Sema.Function_type_resolution.function_provisional_call arguments)
  in
  let rec original_entry entry =
    match Frontend.Symbol_visibility.function_alias_original entry with
    | Some source -> original_entry source
    | None -> entry
  in
  let selected_source_matches snapshot =
    match
      match P.implicit_lookup start with
      | Some entry -> Frontend.Symbol_visibility.Present entry
      | None -> Frontend.Symbol_visibility.Absent
    with
    | Frontend.Symbol_visibility.Present entry ->
        let entry = original_entry entry in
        entry == (N.source snapshot).function_entry
        || Option.fold ~none:false
             ~some:(fun header -> entry == header.P.completed_entry)
             (Sema.Provisional_function.completed_header
                (N.source_snapshot snapshot))
    | _ -> false
  in
  if
    not
      (Integer_globals.task_catalog_owns_namespace task.catalog namespace
      && List.exists
           (fun (original, retained) ->
             original == start && Retained_function.same retained selected)
           task.implicit_selections
      && A.implicit_arguments_admission task.source_activation start
      && (P.implicit_arguments_are_current start
         || A.implicit_arguments task.source_activation start)
      && not
           (List.exists
              (fun original -> original.implicit_start == start)
              task.implicit_starts))
  then Error "call arguments are outside their original task event"
  else
    match (selected_snapshot, snapshot) with
    | Some selected_snapshot, Some snapshot
      when N.owns_namespace snapshot namespace
           && snapshot == N.implicit_argument_snapshot capture
           && selected_source_matches selected_snapshot
           && N.same_identity selected_snapshot snapshot -> (
        match function_record_head task snapshot with
        | None -> Error "call arguments have no admitted native function"
        | Some current
          when Option.fold ~none:false ~some:(N.same_cursor snapshot)
                 (Retained_function.metadata current
                 |> Sema.Outer_environment.function_declaration
                 |> F.resolved_declaration_site
                 |> F.declaration_site_native_snapshot) ->
            let pending =
              {
                implicit_capture = capture;
                implicit_start = start;
                implicit_namespace = namespace;
                implicit_selected = selected;
                implicit_arguments = arguments;
                implicit_completed = false;
              }
            in
            task.implicit_starts <- pending :: task.implicit_starts;
            Ok pending
        | Some _ ->
            Error "call arguments do not match the current native cursor")
    | _ -> Error "call arguments differ from their selected native allocation"

let capture_task_implicit_emission task ~table ~capture pending =
  let module A = Sema.Source_activation in
  let module P = Frontend.Parser in
  let receipt =
    Sema.Function_record_phase.implicit_arguments_receipt
      (Sema.Function_record_phase.implicit_emission_arguments capture)
  in
  let snapshot = Sema.Function_record_phase.implicit_emitted_snapshot capture in
  if
    not
      (Integer_globals.task_catalog_owns_table task.catalog table
      && Sema.Function_record_phase.implicit_emission_arguments capture
         == pending.implicit_capture
      && List.exists (( == ) pending) task.implicit_starts
      && (not pending.implicit_completed)
      && receipt == pending.implicit_start
      && A.implicit_emission_admission task.source_activation receipt
      && (P.implicit_emission_is_current receipt
         || A.implicit_emission task.source_activation receipt))
  then Error "call emission is outside its original task event"
  else
    match function_record_head task snapshot with
    | None -> Error "call emission has no admitted native function"
    | Some current ->
        let emission =
          Retained_function.metadata current
          |> Sema.Outer_environment.function_classified_declaration
        in
        if
          not
            (Option.fold ~none:false
               ~some:(Sema.Function_record_phase.same_cursor snapshot)
               (Sema.Function_record_classification
                .classified_declaration_source emission
               |> Sema.Function_resolution.resolved_declaration_site
               |> Sema.Function_resolution.declaration_site_native_snapshot))
        then Error "call emission does not match the current native cursor"
        else
          Result.map
            (fun phase ->
              pending.implicit_completed <- true;
              task.call_phases <- phase :: task.call_phases;
              phase)
            (Sema.Function_call_phase.create_implicit ~table
               ~namespace:pending.implicit_namespace ~receipt
               ~selected:
                 (Retained_function.metadata pending.implicit_selected
                 |> Sema.Outer_environment.function_declaration)
               ~arguments:pending.implicit_arguments ~emission_snapshot:snapshot
               ~emission)

let owns_call_phase task phase =
  let module Phase = Sema.Function_call_phase in
  let committed =
    Integer_globals.call_command_is_admitted task.catalog
      (Phase.command_start phase)
  in
  List.exists (( == ) phase) task.call_phases
  &&
  match (Phase.receipt phase, Phase.implicit_receipt phase) with
  | Some receipt, None ->
      Sema.Source_activation.call_binding_available task.source_activation
        receipt ~committed
  | None, Some receipt ->
      Sema.Source_activation.implicit_binding_available task.source_activation
        receipt ~committed
  | _ -> false

let check_function_phase_source task ~namespace ~event snapshot =
  let module Parser = Frontend.Parser in
  let module Native = Sema.Function_record_phase in
  let live =
    match event with
    | Parser.Function_declared p -> Parser.function_publication_is_current p
    | Parser.Function_parameter_declared p ->
        Parser.function_parameter_is_current p
    | Parser.Function_parameter_completed p ->
        Parser.function_parameter_completion_is_current p
    | Parser.Parameter_default_completed p ->
        Parser.parameter_default_is_current p
    | Parser.Function_variadic_started p ->
        Parser.function_variadic_start_is_current p
    | Parser.Function_variadic_completed p ->
        Parser.function_variadic_completion_is_current p
    | Parser.Function_header_completed p -> Parser.function_header_is_current p
    | _ -> false
  in
  if
    not
      (source_dimensions_ready task
      && Native.owns_namespace snapshot namespace
      && Integer_globals.task_catalog_owns_namespace task.catalog namespace
      && Native.matches_event snapshot event
      && Sema.Source_activation.function_phase_admission task.source_activation
           event
      && (live
         || Sema.Source_activation.declaration task.source_activation event))
  then
    Error
      "native function admission is outside its original live or active phase"
  else
    Integer_globals.check_function_phase_source task.catalog ~namespace ~event
      snapshot

let admit_function_phase task ~namespace ~event ~snapshot ~records =
  Result.bind (check_function_phase_source task ~namespace ~event snapshot)
    (fun () ->
      Result.map
        (fun reference ->
          task.declared_admissions <-
            Admitted_function reference :: task.declared_admissions)
        (Integer_globals.publish_function_phase task.catalog ~namespace ~event
           ~snapshot ~records))

let admit_function_header task ~namespace ~source ~records =
  Result.bind (check_function_header_source task ~namespace source) (fun () ->
      Result.map
        (fun reference ->
          task.declared_admissions <-
            Admitted_function reference :: task.declared_admissions)
        (Integer_globals.publish_function_header task.catalog ~namespace ~source
           ~records))

let validate_dimension_dependencies task dependencies =
  let module Record = Sema.Compiler_record in
  if
    List.for_all
      (fun dependency ->
        Option.fold ~none:false
          ~some:(fun task ->
            Integer_globals.task_catalog_owns_namespace task.catalog
              (Record.runtime_dimension_namespace dependency)
            && List.exists
                 (fun attempt ->
                   attempt.dimension_catalog == task.catalog
                   && attempt.dimension_receipt
                      == Record.runtime_dimension_source dependency
                   && attempt.dimension_state = Successful_initializer
                   && attempt.dimension_bits
                      = Some (Record.runtime_dimension_count dependency)
                   && attempt.dimension_work
                      = Some (Record.runtime_dimension_work dependency))
                 task.dimensions)
          task)
      dependencies
  then Ok ()
  else
    Error
      "runtime array extent requires its owning task's successful original \
       evaluation"

let admit_declared_global task declaration =
  let ( let* ) = Result.bind in
  let* () =
    validate_dimension_dependencies (Some task)
      (Sema.Compiler_record.declared_global_runtime_dependencies declaration)
  in
  let* () =
    if
      source_dimensions_ready task
      && Sema.Source_activation.global_admission task.source_activation
           (Sema.Compiler_record.declared_global_source declaration)
    then Ok ()
    else
      Error
        "deferred storage admission is outside its original activation event"
  in
  let* globals, slot =
    Integer_globals.prepare_declared task.catalog declaration
  in
  let bytes = Integer_globals.byte_size globals in
  if bytes > task.max_global_bytes - task.global_bytes then
    Error "HCIRVM0016: task global storage exceeds the cumulative byte limit"
  else
    let storage =
      {
        cells = Array.make (Integer_globals.cell_count globals) None;
        live = true;
        unknown_message =
          "hosted execution reached an uninitialized JIT persistent object";
      }
    in
    let publication =
      match Integer_globals.publish_declared task.catalog slot with
      | Integer_globals.Declared_publication (reference, slot) ->
          Admitted_declared_global (reference, slot)
      | _ -> assert false
    in
    task.arenas <- (globals, storage) :: task.arenas;
    task.declared_admissions <- publication :: task.declared_admissions;
    task.global_bytes <- task.global_bytes + bytes;
    task.source_promotion_open <- false;
    Ok ()

let require_initializer_namespace task namespace =
  if Integer_globals.task_catalog_owns_namespace task.catalog namespace then
    Ok ()
  else Error "initializer operation belongs to another task source namespace"

let begin_task_default task ~namespace ~publication receipt =
  let ( let* ) = Result.bind in
  let* () = require_initializer_namespace task namespace in
  let rec predecessor = function
    | None -> true
    | Some (prior : Frontend.Parser.completed_parameter_default) -> (
        match prior.default_ast.value with
        | Frontend.Ast.Lastclass_default _ ->
            predecessor prior.default_predecessor
        | Frontend.Ast.Expression_default _ ->
            List.exists
              (fun attempt ->
                attempt.default_receipt == prior
                && attempt.default_state = Successful_initializer)
              task.defaults)
  in
  if
    (not (source_dimensions_ready task))
    || (not
          (Frontend.Parser.parameter_default_is_current receipt
          || Sema.Source_activation.parameter_default task.source_activation
               receipt))
    || (not (predecessor receipt.default_predecessor))
    || (not
          (Sema.Declaration_collection.namespace_owns_publication namespace
             publication))
    || (not
          (Option.fold ~none:false
             ~some:(( == ) receipt.Frontend.Parser.default_function)
             (Sema.Declaration_collection.publication_source_function
                publication)))
    || List.exists
         (fun attempt -> attempt.default_receipt == receipt)
         task.defaults
  then
    Error
      "default preparation has another source, namespace or consumed boundary"
  else
    let attempt =
      {
        default_catalog = task.catalog;
        default_publication = publication;
        default_receipt = receipt;
        default_preparation_before = task.initializer_steps;
        default_state = Preparing_initializer;
        default_bits = None;
      }
    in
    task.defaults <- attempt :: task.defaults;
    task.source_promotion_open <- false;
    Ok attempt

let fail_task_default task attempt =
  if
    attempt.default_catalog != task.catalog
    || (not (List.exists (( == ) attempt) task.defaults))
    || attempt.default_state <> Preparing_initializer
       && attempt.default_state <> Executing_initializer
  then Error "default failure has another task or inactive attempt"
  else (
    attempt.default_state <- Failed_initializer;
    Ok ())

let task_default_bits task receipt =
  List.find_map
    (fun attempt ->
      if
        attempt.default_receipt == receipt
        && attempt.default_state = Successful_initializer
      then attempt.default_bits
      else None)
    task.defaults

let prepare_task_closed_dimension task ~table ~namespace ~preparation ~queries =
  let module Record = Sema.Compiler_record in
  let invalid message = (Error message, 0) in
  match require_initializer_namespace task namespace with
  | Error message -> invalid message
  | Ok () -> (
      match
        Integer_globals.check_dimension_source ~require_admitted:false
          task.catalog preparation
      with
      | Error message -> invalid message
      | Ok () ->
          if
            (not (task_owns_table task table))
            || (not
                  (Sema.Source_activation.dimension_admission
                     task.source_activation preparation))
            || (not (source_dimensions_ready task))
            || (not (dimension_predecessor_ready task preparation))
            || List.exists (( == ) preparation) task.seen_dimensions
          then
            invalid
              "closed dimension preparation has a skipped predecessor or \
               consumed boundary"
          else (
            task.seen_dimensions <- preparation :: task.seen_dimensions;
            let result, work =
              Record.prepare_dimension ~table ~namespace ~preparation ~queries
                ~max_work:(task.max_initializer_steps - task.initializer_steps)
            in
            task.initializer_steps <- task.initializer_steps + work;
            (match result with
            | Ok checked ->
                task.closed_dimensions <- checked :: task.closed_dimensions
            | Error _ -> ());
            (result, work)))

let prepare_aggregate_offset_in_task task ~table ~namespace ~queries progress
    phase =
  if
    (not
       (Sema.Compiler_record.aggregate_offset_is_current ~table ~namespace
          progress phase))
    || (not (source_dimensions_ready task))
    || List.exists (( == ) phase) task.attempted_offsets
    || List.exists
         (fun offset ->
           Sema.Compiler_record.aggregate_offset_phase offset == phase)
         task.charged_offsets
  then (Error "aggregate offset lacks its live task source boundary", 0)
  else (
    task.source_promotion_open <- false;
    task.attempted_offsets <- phase :: task.attempted_offsets;
    let result, work =
      Sema.Compiler_record.prepare_aggregate_offset ~table ~namespace ~queries
        ~max_work:(task.max_initializer_steps - task.initializer_steps)
        progress phase
    in
    task.initializer_steps <- task.initializer_steps + work;
    (match result with
    | Ok offset -> task.charged_offsets <- offset :: task.charged_offsets
    | Error _ -> ());
    (result, work))

let prepare_task_aggregate_offset task ~table ~namespace ~queries progress phase
    =
  match require_initializer_namespace task namespace with
  | Error message -> (Error message, 0)
  | Ok () when not (task_owns_table task table) ->
      (Error "aggregate offset belongs to another task table", 0)
  | Ok () ->
      prepare_aggregate_offset_in_task task ~table ~namespace ~queries progress
        phase

let prepare_isolated_aggregate_offset task ~table ~namespace ~queries progress
    phase =
  if
    Frontend.Parser.context_mode
      phase.Frontend.Parser.phase_aggregate.aggregate_header.declaration_command
        .command_context
    <> Frontend.Preprocessor.Aot
  then (Error "isolated aggregate offsets require their original AOT source", 0)
  else
    prepare_aggregate_offset_in_task task ~table ~namespace ~queries progress
      phase

let settle_isolated_aggregate_offsets task ~table offsets =
  if
    List.exists
      (fun offset ->
        Sema.Compiler_record.aggregate_offset_table offset != table)
      offsets
  then Error "isolated offset settlement has a foreign source table"
  else
    charge_isolated_aggregate_offsets task ~table
      (List.filter
         (fun offset -> not (List.exists (( == ) offset) task.charged_offsets))
         offsets)

let complete_task_dimension task ~namespace checked =
  let module Record = Sema.Compiler_record in
  let ( let* ) = Result.bind in
  let receipt = Record.dimension_receipt checked in
  let preparation = receipt.Frontend.Parser.dimension_preparation in
  let* () = require_initializer_namespace task namespace in
  let* () =
    validate_dimension_dependencies (Some task)
      (Record.dimension_runtime_dependencies checked)
  in
  let prepared = Record.declared_dimension_preparation checked in
  let success =
    List.exists (( == ) prepared) task.closed_dimensions
    || List.exists
         (fun attempt ->
           attempt.dimension_receipt == preparation
           && attempt.dimension_state = Successful_initializer
           && attempt.dimension_bits = Some (Record.dimension_count checked)
           && attempt.dimension_work = Some (Record.dimension_work checked))
         task.dimensions
  in
  if
    (not
       (Frontend.Parser.dimension_completion_is_current receipt
       || Sema.Source_activation.dimension_completed task.source_activation
            receipt))
    || (not (dimension_predecessor_ready task preparation))
    || Record.dimension_preparation_namespace prepared != namespace
    || (not success)
    || List.exists (( == ) receipt) task.completed_dimensions
  then
    Error
      "dimension completion requires its original successful ordered \
       preparation"
  else (
    task.completed_dimensions <- receipt :: task.completed_dimensions;
    Ok ())

let task_dimension_is_completed task receipt =
  List.exists (( == ) receipt) task.completed_dimensions

let begin_task_dimension task authority =
  let ( let* ) = Result.bind in
  let fragment = Sema.Dimension_fragment.authorized_fragment authority in
  let receipt = Sema.Dimension_fragment.receipt fragment in
  let* () =
    require_initializer_namespace task
      (Sema.Dimension_fragment.namespace fragment)
  in
  let* () = Integer_globals.check_dimension_source task.catalog receipt in
  if
    (not
       (Sema.Source_activation.dimension_admission task.source_activation
          receipt))
    || (not (source_dimensions_ready task))
    || (not (dimension_predecessor_ready task receipt))
    || List.exists (( == ) receipt) task.seen_dimensions
  then Error "dimension preparation has another source or consumed boundary"
  else
    let attempt =
      {
        dimension_catalog = task.catalog;
        dimension_authority = authority;
        dimension_receipt = receipt;
        dimension_preparation_before = task.initializer_steps;
        dimension_state = Preparing_initializer;
        dimension_bits = None;
        dimension_work = None;
      }
    in
    task.dimensions <- attempt :: task.dimensions;
    task.seen_dimensions <- receipt :: task.seen_dimensions;
    task.source_promotion_open <- false;
    Ok attempt

let fail_task_dimension task attempt =
  if
    attempt.dimension_catalog != task.catalog
    || (not (List.exists (( == ) attempt) task.dimensions))
    || attempt.dimension_state <> Preparing_initializer
       && attempt.dimension_state <> Executing_initializer
  then Error "dimension failure has another task or inactive attempt"
  else (
    attempt.dimension_state <- Failed_initializer;
    Ok ())

let task_dimension_bits task receipt =
  List.find_map
    (fun attempt ->
      if
        attempt.dimension_receipt == receipt
        && attempt.dimension_state = Successful_initializer
      then attempt.dimension_bits
      else None)
    task.dimensions

let complete_task_defaults task ~namespace header =
  let ( let* ) = Result.bind in
  let* () = require_initializer_namespace task namespace in
  let* () =
    if Sema.Source_activation.default_completion task.source_activation header
    then Ok ()
    else
      Error
        "deferred defaults completion is outside its original activation event"
  in
  let expected =
    List.mapi
      (fun index (parameter : Frontend.Ast.function_parameter) ->
        Option.bind parameter.default (fun default ->
            match default.value with
            | Frontend.Ast.Expression_default _ -> Some (index, default)
            | Lastclass_default _ -> None))
      header.Frontend.Parser.parameters
    |> List.filter_map Fun.id
  in
  let rec collect rev = function
    | [] ->
        Integer_globals.publish_parameter_defaults task.catalog ~namespace
          (List.rev rev)
    | (index, default) :: rest ->
        let* attempt =
          match
            List.find_opt
              (fun attempt ->
                attempt.default_receipt.default_function
                == header.function_publication
                && attempt.default_receipt.default_parameter_index = index
                && attempt.default_receipt.default_ast == default)
              task.defaults
          with
          | Some attempt
            when attempt.default_state = Successful_initializer
                 && Option.is_some attempt.default_bits -> Ok attempt
          | _ ->
              Error
                "function header requires each successful original default \
                 preparation"
        in
        let* value =
          Prepared_parameter_default.create
            ~publication:attempt.default_publication ~header
            ~receipt:attempt.default_receipt
            ~bits:(Option.get attempt.default_bits)
        in
        collect (value :: rev) rest
  in
  collect [] expected

let begin_task_initializer task ~namespace declaration start =
  let ( let* ) = Result.bind in
  let* () = require_initializer_namespace task namespace in
  let* slot =
    match
      admitted_publication_for_symbol task
        (Sema.Compiler_record.declared_global_symbol declaration)
    with
    | Some (Admitted_declared_global (_, slot))
      when Integer_globals.declared_record slot == declaration -> Ok slot
    | _ -> Error "initializer start has no original admitted task object"
  in
  if
    (not
       (Frontend.Parser.initializer_start_is_current start
       || Sema.Source_activation.initializer_start task.source_activation start
       ))
    || (not (source_dimensions_ready task))
    || start.initializer_owner
       != Sema.Compiler_record.declared_global_source declaration
    || List.exists
         (fun state -> state.initializer_slot == slot)
         task.initializers
  then Error "initializer start is foreign, delayed or repeated"
  else
    let* initializer_cursor =
      Integer_initializer_layout.begin_live declaration
    in
    let* () = Integer_globals.begin_declared_initializer slot in
    let state =
      {
        initializer_catalog = task.catalog;
        initializer_slot = slot;
        initializer_start = start;
        initializer_cursor;
        initializer_seen = [];
        initializer_attempt = None;
        initializer_complete = false;
      }
    in
    task.initializers <- state :: task.initializers;
    Ok ()

let find_task_initializer task start =
  match
    List.find_opt
      (fun state -> state.initializer_start == start)
      task.initializers
  with
  | Some state
    when (not state.initializer_complete)
         && not
              (Integer_globals.declared_initializer_failed
                 state.initializer_slot) -> Ok state
  | _ -> Error "initializer has no active original task destination"

let initializer_is_idle state =
  match state.initializer_attempt with
  | None -> true
  | Some attempt -> attempt.attempt_state = Successful_initializer

let observe_task_initializer_delimiter task ~namespace receipt =
  let ( let* ) = Result.bind in
  let* () = require_initializer_namespace task namespace in
  let* state =
    find_task_initializer task receipt.Frontend.Parser.delimiter_initializer
  in
  if
    (not
       (Frontend.Parser.initializer_delimiter_is_current receipt
       || Sema.Source_activation.initializer_delimiter task.source_activation
            receipt))
    || not (initializer_is_idle state)
  then
    Error "initializer delimiter is delayed or precedes completion of its leaf"
  else
    let* next =
      Integer_initializer_layout.observe_live_delimiter state.initializer_cursor
        receipt
    in
    state.initializer_cursor <- next;
    Ok ()

let begin_task_initializer_leaf task ~namespace leaf =
  let ( let* ) = Result.bind in
  let* () = require_initializer_namespace task namespace in
  let* receipt =
    match Sema.Initializer_source.leaf_parser_receipt leaf with
    | Some receipt
      when Frontend.Parser.initializer_leaf_is_current receipt
           || Sema.Source_activation.initializer_leaf task.source_activation
                receipt -> Ok receipt
    | _ -> Error "initializer attempt requires its original current leaf"
  in
  let* state = find_task_initializer task receipt.leaf_initializer in
  if
    (not (initializer_is_idle state))
    || List.exists (( == ) receipt) state.initializer_seen
  then
    Error
      "initializer leaf has already been attempted or precedes its prior leaf"
  else (
    state.initializer_seen <- receipt :: state.initializer_seen;
    match
      Integer_initializer_layout.prepare_live state.initializer_cursor leaf
    with
    | Error message ->
        Integer_globals.fail_declared_initializer state.initializer_slot;
        Error message
    | Ok (attempt_next, attempt_destination) ->
        let attempt =
          {
            attempt_initializer = state;
            attempt_leaf = leaf;
            attempt_receipt = receipt;
            attempt_next;
            attempt_destination;
            attempt_preparation_before = task.initializer_steps;
            attempt_state = Preparing_initializer;
          }
        in
        state.initializer_attempt <- Some attempt;
        Ok attempt)

let initializer_attempt_destination attempt = attempt.attempt_destination

let fail_task_initializer_attempt task attempt =
  if
    attempt.attempt_initializer.initializer_catalog != task.catalog
    || attempt.attempt_state = Successful_initializer
  then Error "initializer failure does not belong to an unfinished task attempt"
  else (
    attempt.attempt_state <- Failed_initializer;
    Integer_globals.fail_declared_initializer
      attempt.attempt_initializer.initializer_slot;
    Ok ())

let complete_task_initializer task ~namespace start source =
  let ( let* ) = Result.bind in
  let* () = require_initializer_namespace task namespace in
  let* () =
    if
      Sema.Source_activation.initializer_completion task.source_activation start
    then Ok ()
    else
      Error
        "deferred initializer completion is outside its original activation \
         event"
  in
  let* state = find_task_initializer task start in
  if not (initializer_is_idle state) then
    Error "initializer completion has an unfinished leaf"
  else
    let* _ =
      Integer_initializer_layout.complete_live state.initializer_cursor source
    in
    let* () =
      Integer_globals.complete_declared_initializer state.initializer_slot
    in
    state.initializer_complete <- true;
    Ok ()

let task_result task ~sequence =
  if
    task.streams <> [] || task.source_execution_failed
    || task.deferred_dimensions <> []
    || task.deferred_offsets <> []
    || List.exists
         (fun preparation ->
           not
             (List.exists
                (fun receipt ->
                  receipt.Frontend.Parser.dimension_preparation == preparation)
                task.completed_dimensions))
         task.seen_dimensions
    || List.exists
         (fun attempt -> attempt.dimension_state <> Successful_initializer)
         task.dimensions
    || (not (Sema.Source_activation.finished task.source_activation))
    || (not
          (Sema.Source_activation.owns_context task.source_activation
             sequence.Frontend.Parser.sequence_context))
    || List.exists
         (fun state -> not state.initializer_complete)
         task.initializers
    || List.exists
         (fun attempt -> attempt.default_state <> Successful_initializer)
         task.defaults
  then Error "task result requires completed source execution"
  else
    match task.source_result with
    | Some (original, result) when original == sequence ->
        Result.map
          (fun () -> result)
          (Integer_globals.check_source_completion task.catalog sequence)
    | _ -> Error "task result has no original execution completion"

let task_input_result task ~sequence =
  let ( let* ) = Result.bind in
  let* () = Integer_globals.check_source_completion task.catalog sequence in
  match
    List.find_opt
      (fun input ->
        input.input_context == sequence.Frontend.Parser.sequence_context)
      task.inputs
  with
  | Some { input_result = Some (original, result); _ } when original == sequence
    -> result
  | _ -> Error "task input has no original execution completion"

let task_function_source task link =
  let module Records = Sema.Function_record_classification in
  let dynamic =
    link |> Retained_function.metadata
    |> Sema.Outer_environment.function_classified_declaration
    |> Records.classified_declaration_record |> Records.call_access
    |> fun access -> access = Records.Jit_extern_address_slot_call
  in
  List.find_opt
    (fun executable ->
      Retained_function.same executable.function_link link
      || dynamic
         && Retained_function.symbol link
            == executable.function_callee.callee_symbol
         && Option.fold ~none:false
              ~some:(fun later ->
                Sema.Function_resolution.is_joined_successor
                  ~earlier:
                    (link |> Retained_function.metadata
                   |> Sema.Outer_environment.function_declaration)
                  ~later)
              executable.function_callee.callee_definition)
    task.functions
  |> Option.map (fun executable -> executable.function_source)

let task_output_bytes task = Output.contents task.output
let task_output_work task = Output.work task.output
let task_generated_bytes task = Output.committed_bytes task.generated
let task_executed_steps task = task.steps
let task_initializer_steps task = task.initializer_steps
let task_initializer_limit task = task.max_initializer_steps

let task_progress (task : task_state) =
  {
    executed_steps = task.steps;
    initializer_steps = task.initializer_steps;
    global_bytes = task.global_bytes;
    literal_bytes = task.literal_bytes;
    output_bytes = Output.contents task.output;
    output_work = Output.work task.output;
    generated_bytes = Output.committed_bytes task.generated;
    final_value = task.outer_value;
  }

let record_task_preparation task ~before ~steps =
  if
    before < 0 || steps < 0
    || before > task.initializer_steps
    || steps > task.max_initializer_steps - before
    || before + steps < task.initializer_steps
  then
    invalid_arg
      "task preparation progress is inconsistent with its cumulative budget";
  task.source_promotion_open <- false;
  task.initializer_steps <- before + steps

let begin_isolated_preparation task =
  task.source_promotion_open <- false;
  {
    preparation_catalog = task.catalog;
    preparation_steps = 0;
    preparation_closed = false;
  }

let record_isolated_preparation task preparation ~steps =
  if
    preparation.preparation_catalog != task.catalog
    || preparation.preparation_closed
    || steps < preparation.preparation_steps
    || steps - preparation.preparation_steps
       > task.max_initializer_steps - task.initializer_steps
  then invalid_arg "isolated preparation does not match its owning allowance";
  task.initializer_steps <-
    task.initializer_steps + steps - preparation.preparation_steps;
  preparation.preparation_steps <- steps

let abort_isolated_preparation task preparation =
  if preparation.preparation_catalog != task.catalog then
    invalid_arg "isolated preparation belongs to another invocation";
  preparation.preparation_closed <- true

let finish_isolated_preparation task preparation ~runtime_calls ~globals
    ~initialization ~functions checked =
  if
    preparation.preparation_catalog != task.catalog
    || preparation.preparation_closed
  then Error "isolated preparation is foreign or already closed"
  else if
    preparation.preparation_steps
    <> Global_initialization.prepared_steps initialization
  then Error "isolated initializer work lacks its exact charged preparation"
  else if
    Integer_globals.is_task_command globals
    || (not
          (Global_initialization.matches initialization ~globals ~entry:checked))
    || not
         (Runtime.matches runtime_calls ~entry:checked
            ~initialization:(Some initialization)
            ~functions:
              (List.map
                 (fun (definition : function_definition) -> definition.body)
                 functions))
  then Error "isolated preparation requires its exact ordinary compiled bundle"
  else (
    preparation.preparation_closed <- true;
    task.isolated_programs <-
      {
        source_entry = checked;
        source_storage = globals;
        source_initialization = initialization;
        source_calls = runtime_calls;
        source_bodies = functions;
      }
      :: task.isolated_programs;
    Ok ())

type call_phase = Collecting of int | Needs_cleanup | Needs_end

type checked_call = {
  callee : callee;
  site : Runtime.call option;
  remaining_arguments : Runtime.argument list option;
  phase : call_phase;
}

type opcode_kind =
  | Literal_address_kind
  | Scale_index_kind
  | Index_address_kind
  | Pointer_address_kind
  | Global_address_kind
  | Frame_address_kind
  | Load_slot_kind
  | Store_slot_kind
  | Update_slot_kind of binary_operation
  | Increment_slot_kind of binary_operation * bool
  | Immediate_kind
  | Unary_kind of unary_operation
  | Word_view_kind
  | Binary_kind of binary_operation
  | Discard_kind
  | Return_value_kind
  | Jump_kind
  | Branch_kind of branch_condition
  | Return_kind
  | End_kind

type declared_type =
  | Pointer_value of Type.t
  | Supported of word_type * Type.t * Type.t
  | Void_value
  | Frame_base of Type.t
  | Frame_offset of Type.t * int64
  | Frame_address of int
  | Variadic_address of Type.t
  | Global_address of Integer_globals.storage_slot
  | Index_offset of Type.t * int64 * prepared_operand
  | Indexed_address of Type.t * int64 list
  | Unsupported

let reference_commit = Sequence.reference_commit

let make_error ?block_id ?instruction_id ?span ~stage ~executed_steps code
    message =
  {
    stage;
    code;
    message;
    executed_steps;
    block_id = Option.map Block_id.to_int block_id;
    instruction_id = Option.map Instruction_id.to_int instruction_id;
    span;
    function_id = None;
    function_name = None;
    initializer_phase = None;
    initializer_symbol_id = None;
    initializer_name = None;
  }

let identify_initializer region error =
  match region with
  | None -> error
  | Some region ->
      let symbol = Global_initialization.storage_symbol region in
      {
        error with
        initializer_phase = Some (Global_initialization.storage_phase region);
        initializer_symbol_id =
          Some (Sema.Symbol.id symbol |> Sema.Symbol.Id.to_int);
        initializer_name = Some (Sema.Symbol.name symbol);
      }

let preflight_error block_id (description : Sequence.description) code message =
  make_error ~stage:Preflight ~executed_steps:0 ~block_id
    ~instruction_id:description.instruction_id ?span:description.span code
    message

let opcode_kind = function
  | Opcode.Ic_imm_i64 -> Some Immediate_kind
  | Opcode.Ic_com -> Some (Unary_kind Complement)
  | Opcode.Ic_not -> Some (Unary_kind Logical_not)
  | Opcode.Ic_unary_minus -> Some (Unary_kind Negate)
  | Opcode.Ic_holyc_typecast -> Some Word_view_kind
  | Opcode.Ic_add -> Some (Binary_kind Add)
  | Opcode.Ic_sub -> Some (Binary_kind Subtract)
  | Opcode.Ic_mul -> Some (Binary_kind Multiply)
  | Opcode.Ic_div -> Some (Binary_kind Divide)
  | Opcode.Ic_mod -> Some (Binary_kind Remainder)
  | Opcode.Ic_and -> Some (Binary_kind Bitwise_and)
  | Opcode.Ic_or -> Some (Binary_kind Bitwise_or)
  | Opcode.Ic_xor -> Some (Binary_kind Bitwise_xor)
  | Opcode.Ic_shl -> Some (Binary_kind Shift_left)
  | Opcode.Ic_shr -> Some (Binary_kind Shift_right)
  | Opcode.Ic_equ_equ -> Some (Binary_kind (Compare Equal))
  | Opcode.Ic_not_equ -> Some (Binary_kind (Compare Not_equal))
  | Opcode.Ic_less -> Some (Binary_kind (Compare Less))
  | Opcode.Ic_greater_equ -> Some (Binary_kind (Compare Greater_equal))
  | Opcode.Ic_greater -> Some (Binary_kind (Compare Greater))
  | Opcode.Ic_less_equ -> Some (Binary_kind (Compare Less_equal))
  | Opcode.Ic_and_and -> Some (Binary_kind (Logical Logical_and))
  | Opcode.Ic_or_or -> Some (Binary_kind (Logical Logical_or))
  | Opcode.Ic_xor_xor -> Some (Binary_kind (Logical Logical_xor))
  | Opcode.Ic_end_exp -> Some Discard_kind
  | Opcode.Ic_return_val -> Some Return_value_kind
  | Opcode.Ic_jmp -> Some Jump_kind
  | Opcode.Ic_br_zero -> Some (Branch_kind Zero)
  | Opcode.Ic_br_not_zero -> Some (Branch_kind Not_zero)
  | Opcode.Ic_ret -> Some Return_kind
  | Opcode.Ic_end -> Some End_kind
  | _ -> None

let update_kind = function
  | Opcode.Ic_add_equ -> Some (Update_slot_kind Add)
  | Opcode.Ic_sub_equ -> Some (Update_slot_kind Subtract)
  | Opcode.Ic_mul_equ -> Some (Update_slot_kind Multiply)
  | Opcode.Ic_div_equ -> Some (Update_slot_kind Divide)
  | Opcode.Ic_mod_equ -> Some (Update_slot_kind Remainder)
  | Opcode.Ic_and_equ -> Some (Update_slot_kind Bitwise_and)
  | Opcode.Ic_or_equ -> Some (Update_slot_kind Bitwise_or)
  | Opcode.Ic_xor_equ -> Some (Update_slot_kind Bitwise_xor)
  | Opcode.Ic_shl_equ -> Some (Update_slot_kind Shift_left)
  | Opcode.Ic_shr_equ -> Some (Update_slot_kind Shift_right)
  | Opcode.Ic_pp_ -> Some (Increment_slot_kind (Add, false))
  | Opcode.Ic_mm_ -> Some (Increment_slot_kind (Subtract, false))
  | Opcode.Ic__pp -> Some (Increment_slot_kind (Add, true))
  | Opcode.Ic__mm -> Some (Increment_slot_kind (Subtract, true))
  | _ -> None

let scalar_word_type ~allow_public type_ =
  if Sema.Type.pointer_depth type_ <> 0 then None
  else
    match Sema.Type.base type_ with
    | Sema.Type.Primitive (form, primitive)
      when (allow_public || form = Sema.Type.Internal_storage)
           && Sema.Primitive_type.equal primitive Sema.Primitive_type.I64 ->
        Some I64
    | Sema.Type.Primitive (form, primitive)
      when (allow_public || form = Sema.Type.Internal_storage)
           && Sema.Primitive_type.equal primitive Sema.Primitive_type.U64 ->
        Some U64
    | Sema.Type.Primitive _ | Sema.Type.Aggregate _ -> None

let producer_word_type type_ = scalar_word_type ~allow_public:false type_
let return_word_type type_ = scalar_word_type ~allow_public:true type_
let scalar_runtime_type scalar = if Scalar.is_unsigned scalar then U64 else I64

(* A narrow expression retains its checked raw class and full register bits.
   Only storage narrows it; the public execution result remains I64/U64. *)
let scalar_value_type ~allow_byte ~allow_public type_ =
  match scalar_word_type ~allow_public type_ with
  | Some _ as word -> word
  | None when allow_byte && Type.pointer_depth type_ = 0 -> (
      match (Type.base type_, Scalar.of_type type_) with
      | Type.Primitive (form, _), Some scalar
        when allow_public || form = Type.Internal_storage ->
          Some (scalar_runtime_type scalar)
      | _ -> None)
  | None -> None

let function_return_word_type type_ =
  scalar_value_type ~allow_byte:true ~allow_public:true type_

let checked_return_kind type_ =
  match function_return_word_type type_ with
  | Some word -> Some (Word_return word)
  | None when Type.pointer_depth type_ = 0 -> (
      match Type.base type_ with
      | Type.Primitive (_, Sema.Primitive_type.U0) -> Some Void_return
      | _ -> None)
  | None -> None

let scalar_element_bytes type_ =
  Option.map Scalar.byte_size (Scalar.of_type type_)

let scalar_pointer_type type_ =
  match Type.dereference type_ with
  | Ok pointee -> Option.is_some (Scalar.of_type pointee)
  | Error _ -> false

let literal_pointer_type type_ =
  Type.pointer_depth type_ = 1
  &&
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, Sema.Primitive_type.U8) -> true
  | _ -> false

let stored_type type_ =
  match return_word_type type_ with
  | Some word -> Some (Stored_word word)
  | None when scalar_pointer_type type_ -> Some (Stored_pointer type_)
  | None ->
      Option.map (fun scalar -> Stored_narrow scalar) (Scalar.of_type type_)

let stored_bytes = function
  | Stored_narrow scalar -> Scalar.byte_size scalar
  | Stored_word _ | Stored_pointer _ -> 8

let frame_context ?globals ?(pointer_arguments = false) ~max_frame_bytes ~frame
    ~arguments function_ =
  let invalid message =
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0
          ?span:(Function.span function_) "HCIRVM0011" message;
      ]
  in
  let locations = Frame.function_locations frame in
  let of_kind kind =
    List.filter (fun item -> Frame.location_kind item = kind) locations
  in
  let parameters = of_kind Frame.Named_parameter in
  let locals = of_kind Frame.Automatic_local in
  let statics = of_kind Frame.Static_local in
  let argc = of_kind Frame.Variadic_argc in
  let argv = of_kind Frame.Variadic_argv in
  let variadic =
    Option.is_some
      (Sema.Function_type_resolution.function_variadic_bindings
         (Frame.function_header frame))
  in
  let synthetic_match =
    match (variadic, argc, argv) with
    | false, [], [] -> true
    | true, [ count ], [ vector ] ->
        let bindings =
          Frame.function_header frame
          |> Sema.Function_type_resolution.function_variadic_bindings
          |> Option.get
        in
        let matches location binding =
          Frame.location_symbol location
          == Sema.Function_type_resolution.synthetic_binding_symbol binding
          && Type.equal
               (Frame.location_checked_type location)
               (Sema.Function_type_resolution.synthetic_binding_type binding)
        in
        matches count (Sema.Function_type_resolution.variadic_argc bindings)
        && matches vector (Sema.Function_type_resolution.variadic_argv bindings)
    | _ -> false
  in
  let statics_match =
    List.for_all
      (fun location ->
        match
          Option.bind globals (fun globals ->
              Integer_globals.find_static globals
                (Frame.location_symbol location))
        with
        | Some slot ->
            Integer_globals.static_frame slot == frame
            && Integer_globals.static_location slot == location
            && Integer_globals.static_compiler_options slot
               = Function.compiler_options function_
        | None -> false)
      statics
  in
  let members_match members locations =
    List.length members = List.length locations
    && List.for_all2
         (fun (position, member) location ->
           Function.member_position member = position
           && Function.member_symbol member == Frame.location_symbol location
           && Type.equal
                (Function.member_type member)
                (Frame.location_checked_type location))
         (List.mapi (fun index member -> (index, member)) members)
         locations
  in
  let parameter_count = List.length parameters in
  let argument_count = List.length arguments in
  let stack_count =
    if variadic && argument_count < Int.max_int then argument_count + 1
    else argument_count
  in
  let frame_size = Frame.function_frame_size frame in
  let allowed_flags =
    Int64.logor
      (Sema.Function_flag.Stored.to_mask Ret1)
      (Int64.logor
         (Sema.Function_flag.Stored.to_mask Argument_pop)
         (Sema.Function_flag.Stored.to_mask No_argument_pop))
  in
  let allowed_flags =
    if variadic && synthetic_match then
      Int64.logor allowed_flags (Sema.Function_flag.Stored.to_mask Variadic)
    else
      match Function.definition_declaration function_ with
      | Some declaration ->
          let header =
            declaration |> Sema.Function_resolution.resolved_declaration_site
            |> Sema.Function_resolution.declaration_site_function
          in
          if
            header == Frame.function_header frame
            && Function.definition_matches_frame function_ frame
            && Option.is_none
                 (Sema.Function_type_resolution.function_variadic_bindings
                    header)
            && header |> Sema.Function_type_resolution.function_signature
               |> Sema.Function_type_resolution.signature_variadic_origin
               |> Option.is_none
          then
            (* PrsFunJoin retains this bit when a fixed header replaces a variadic
             extern. The exact new header and frame still have only fixed slots. *)
            Int64.logor allowed_flags
              (Sema.Function_flag.Stored.to_mask Variadic)
          else allowed_flags
      | _ -> allowed_flags
  in
  if
    Function.symbol function_ != Frame.function_symbol frame
    || (not (Function.definition_matches_frame function_ frame))
    || not
         (Sema.Symbol.Scope_id.equal
            (Function.function_scope function_)
            (Sema.Symbol_table.scope_id (Frame.function_scope frame)))
  then
    invalid
      "the named function and frame have different symbol or scope identities"
  else if
    not
      (members_match (Function.parameters function_) parameters
      && members_match (Function.locals function_) locals)
  then
    invalid "the named function members disagree with the exact checked frame"
  else if
    List.length locations
    <> parameter_count + List.length locals + List.length statics
       + List.length argc + List.length argv
    || (not synthetic_match) || (not statics_match)
    || Int64.logand
         (Function.stored_flags function_)
         (Int64.lognot allowed_flags)
       <> 0L
  then
    invalid
      "execution requires ordinary parameters, automatic scalar locals and \
       exact function-owned persistent statics"
  else if Option.is_none (checked_return_kind (Function.return_type function_))
  then
    invalid "the function return type is outside nonzero integer/U0 execution"
  else if
    if variadic then argument_count < parameter_count
    else argument_count <> parameter_count
  then invalid "the argument word count does not match the checked parameters"
  else if
    stack_count > max_frame_bytes / 8
    || (variadic && argument_count = Int.max_int)
    || frame_size < 0L
    || frame_size > Int64.of_int (max_frame_bytes - (stack_count * 8))
    || List.length locations > Sys.max_array_length
  then invalid "the checked function frame exceeds max_frame_bytes"
  else
    let locations =
      List.filter
        (fun location ->
          Frame.location_kind location <> Frame.Static_local
          && Frame.location_kind location <> Frame.Variadic_argv)
        locations
    in
    let arguments = ref arguments in
    let prepared_rev = ref []
    and total_cells = ref 0L
    and total_bytes = ref 0L
    and error = ref None in
    let allocated_bytes = Int64.to_int frame_size + (stack_count * 8) in
    let tail_count = if variadic then argument_count - parameter_count else 0 in
    let max_cells =
      Int64.of_int (min Sys.max_array_length max_frame_bytes - tail_count)
    in
    List.iter
      (fun location ->
        let dimensions = Frame.location_dimensions location in
        let storage_kind = stored_type (Frame.location_checked_type location) in
        let allocation_bytes object_bytes =
          if
            Frame.location_kind location = Frame.Named_parameter
            || Frame.location_kind location = Frame.Variadic_argc
          then 8L
          else object_bytes
        in
        let rec array_strides = function
          | [] ->
              Option.map
                (fun kind -> (Int64.of_int (stored_bytes kind), []))
                storage_kind
          | dimension :: rest -> (
              match array_strides rest with
              | Some (bytes, strides) ->
                  let count = Frame.dimension_value dimension in
                  if count <= 0L || count > Int64.div Int64.max_int bytes then
                    None
                  else Some (Int64.mul count bytes, bytes :: strides)
              | None -> None)
        in
        match
          ( storage_kind,
            Frame.location_frame_slot location,
            array_strides dimensions )
        with
        | Some stored_type, Some slot, Some (bytes, strides)
          when Frame.location_declarator_shape location = Frame.Object
               && Frame.location_element_size location
                  = Int64.of_int (stored_bytes stored_type)
               && Frame.location_allocated_size location
                  = allocation_bytes bytes
               && Frame.frame_slot_size slot = allocation_bytes bytes
               && (dimensions = []
                   && Frame.location_value_shape location = Frame.Scalar
                  || dimensions <> []
                     && Frame.location_value_shape location = Frame.Array
                     && Frame.location_kind location = Frame.Automatic_local
                     &&
                     match stored_type with
                     | Stored_word _ | Stored_narrow _ -> true
                     | _ -> false) ->
            let offset = Frame.frame_slot_displacement slot in
            let count =
              Int64.div bytes (Int64.of_int (stored_bytes stored_type))
            in
            if
              count > Int64.sub max_cells !total_cells
              || allocation_bytes bytes
                 > Int64.sub (Int64.of_int allocated_bytes) !total_bytes
            then
              error := Some "the flattened frame exceeds the cell or byte limit"
            else
              let initial =
                match (Frame.location_kind location, !arguments) with
                | Frame.Named_parameter, bits :: rest -> (
                    arguments := rest;
                    match stored_type with
                    | Stored_word type_ -> Some (Runtime_word { type_; bits })
                    | Stored_narrow scalar ->
                        Some
                          (Runtime_word
                             {
                               type_ = scalar_runtime_type scalar;
                               bits = Scalar.normalize scalar bits;
                             })
                    | Stored_pointer _ ->
                        if not pointer_arguments then
                          error :=
                            Some
                              "integer argument bits cannot supply a pointer \
                               parameter";
                        None)
                | Frame.Variadic_argc, _ ->
                    Some
                      (Runtime_word
                         {
                           type_ = I64;
                           bits = Int64.of_int (argument_count - parameter_count);
                         })
                | _ -> None
              in
              let entry =
                {
                  slot_type = Frame.location_checked_type location;
                  stored_type;
                  initial;
                  object_count = Int64.to_int count;
                  strides;
                }
              in
              prepared_rev :=
                (Int64.to_int !total_cells, offset, entry) :: !prepared_rev;
              total_cells := Int64.add !total_cells count;
              total_bytes := Int64.add !total_bytes (allocation_bytes bytes)
        | _ ->
            error :=
              Some
                "the checked frame contains an unsupported storage type or \
                 shape")
      locations;
    match !error with
    | Some message -> invalid message
    | None -> (
        let slots =
          Array.make
            (Int64.to_int !total_cells)
            {
              slot_type = Function.return_type function_;
              stored_type = Stored_word I64;
              initial = None;
              object_count = 1;
              strides = [];
            }
        in
        let offsets = ref Offset_map.empty in
        List.iter
          (fun (base, offset, entry) ->
            if Offset_map.mem offset !offsets then
              error := Some "the checked frame contains overlapping roots";
            offsets := Offset_map.add offset base !offsets;
            Array.fill slots base entry.object_count entry)
          (List.rev !prepared_rev);
        match !error with
        | Some message -> invalid message
        | None ->
            Ok
              {
                layout = frame;
                slots;
                offsets = !offsets;
                return_type = Function.return_type function_;
                allocated_bytes;
                variadic_location =
                  (match argv with
                  | [ location ] ->
                      Some
                        ( Frame.frame_slot_displacement
                            (Option.get (Frame.location_frame_slot location)),
                          Frame.location_checked_type location )
                  | _ -> None);
                initial_variadic =
                  (if variadic then
                     Array.of_list
                       (List.map
                          (fun bits ->
                            Some (Runtime_word { type_ = I64; bits }))
                          !arguments)
                   else [||]);
              })

let frame_pointer type_ =
  scalar_pointer_type type_
  ||
  match Type.dereference type_ with
  | Ok pointee -> scalar_pointer_type pointee
  | Error _ -> false

let address_slot context types (description : Sequence.description) =
  match (description.opcode, description.operands, description.target_type) with
  | Opcode.Ic_add, [ base; displacement ], Some target_type -> (
      match
        (Value_map.find_opt base types, Value_map.find_opt displacement types)
      with
      | Some (Frame_base base_type), Some (Frame_offset (offset_type, offset))
        when Type.equal base_type target_type
             && Type.equal offset_type target_type -> (
          match context.variadic_location with
          | Some (expected, pointee) when expected = offset -> (
              match Type.pointer_to pointee with
              | Ok pointer when Type.equal pointer target_type ->
                  Variadic_address pointee
              | _ -> Unsupported)
          | _ -> (
              match Offset_map.find_opt offset context.offsets with
              | Some index -> (
                  match Type.pointer_to context.slots.(index).slot_type with
                  | Ok pointer when Type.equal pointer target_type ->
                      Frame_address index
                  | _ -> Unsupported)
              | None -> Unsupported))
      | _ -> Unsupported)
  | _ -> Unsupported

let storage_allowed frame initialization instruction slot =
  match Integer_globals.storage_frame slot with
  | None -> true
  | Some owner ->
      Option.fold ~none:false ~some:(fun frame -> frame.layout == owner) frame
      || Option.fold ~none:false
           ~some:(fun context ->
             match Global_initialization.find_storage context instruction with
             | Some region ->
                 Option.fold ~none:false
                   ~some:(fun frame -> frame == owner)
                   (Global_initialization.storage_frame region)
             | None -> false)
           initialization

let global_address frame globals initialization
    (description : Sequence.description) =
  match (globals, description.target_type) with
  | Some globals, Some type_ -> (
      let selected =
        match description.payload with
        | Some (Sequence.Symbol symbol) ->
            Integer_globals.find_storage globals symbol
        | Some (Sequence.Retained_global reference) ->
            Integer_globals.retained_slot globals reference
        | _ -> None
      in
      match selected with
      | Some slot
        when description.opcode = Integer_globals.storage_opcode slot
             && storage_allowed frame initialization description.instruction_id
                  slot -> (
          match Type.pointer_to (Integer_globals.storage_type slot) with
          | Ok expected when Type.equal type_ expected -> Some slot
          | _ -> None)
      | _ -> None)
  | _ -> None

let index_offset types (description : Sequence.description) =
  match (description.operands, description.target_type) with
  | [ stride_id; value_id ], Some pointer when scalar_pointer_type pointer -> (
      match
        (Value_map.find_opt stride_id types, Value_map.find_opt value_id types)
      with
      | ( Some (Frame_offset (stride_type, stride)),
          Some (Supported (expected_type, index_type, computation_type)) )
        when Type.equal pointer stride_type
             && stride > 0L
             && Option.is_some (return_word_type index_type) ->
          Index_offset
            ( pointer,
              stride,
              {
                value_id;
                expected_type;
                computation_type =
                  Option.get (function_return_word_type computation_type);
              } )
      | _ -> Unsupported)
  | _ -> Unsupported

let indexed_address frame types (description : Sequence.description) =
  match (description.operands, description.target_type) with
  | [ base; offset ], Some pointer when scalar_pointer_type pointer -> (
      let strides =
        match Value_map.find_opt base types with
        | Some (Frame_address index) ->
            Option.bind frame (fun context ->
                let slot = context.slots.(index) in
                match Type.pointer_to slot.slot_type with
                | Ok expected when Type.equal expected pointer ->
                    Some slot.strides
                | _ -> None)
        | Some (Variadic_address pointee) -> (
            match Type.pointer_to pointee with
            | Ok expected when Type.equal expected pointer -> Some [ 8L ]
            | _ -> None)
        | Some (Global_address slot) -> (
            match Type.pointer_to (Integer_globals.storage_type slot) with
            | Ok expected when Type.equal expected pointer ->
                Some (Integer_globals.storage_strides slot)
            | _ -> None)
        | Some (Indexed_address (expected, strides))
          when Type.equal expected pointer -> Some strides
        | Some (Pointer_value expected) when Type.equal expected pointer ->
            Option.bind
              (Result.to_option (Type.dereference pointer))
              (fun pointee ->
                Option.map
                  (fun width -> [ Int64.of_int width ])
                  (scalar_element_bytes pointee))
        | _ -> None
      in
      match (strides, Value_map.find_opt offset types) with
      | Some (stride :: remaining), Some (Index_offset (expected, actual, _))
        when stride = actual && Type.equal expected pointer ->
          Indexed_address (pointer, remaining)
      | _ -> Unsupported)
  | _ -> Unsupported

let declared_types ?frame ?globals ?literals ?initialization
    ?(allow_calls = false) ?(is_default = fun _ -> false) block =
  let memory_enabled =
    Option.is_some frame || Option.is_some globals || Option.is_some literals
  in
  let allow_byte = Option.is_some frame || Option.is_some literals in
  Graph.instructions block |> Sequence.instructions
  |> List.fold_left
       (fun types instruction ->
         let description = Sequence.description instruction in
         let supported word_type type_ =
           let computation_type =
             match (description.opcode, description.operands) with
             | (Opcode.Ic_call_end | Opcode.Ic_holyc_typecast), _ ->
                 Computation.declared type_
             | Opcode.Ic_com, [ operand ] -> (
                 match Value_map.find_opt operand types with
                 | Some (Supported (_, _, operand_type)) ->
                     Computation.forward operand_type
                 | _ -> Computation.forward type_)
             | _ -> Computation.forward type_
           in
           Supported (word_type, type_, computation_type)
         in
         match description.result with
         | None -> types
         | Some result ->
             let declared =
               match
                 global_address frame globals initialization description
               with
               | Some slot -> Global_address slot
               | None -> (
                   match description.target_type with
                   | Some type_ -> (
                       if
                         Option.is_some literals
                         && description.opcode = Opcode.Ic_str_const
                         && literal_pointer_type type_
                       then Pointer_value type_
                       else if
                         memory_enabled && scalar_pointer_type type_
                         && (description.opcode = Opcode.Ic_addr
                            || description.opcode = Opcode.Ic_deref
                            || description.opcode = Opcode.Ic_assign)
                       then Pointer_value type_
                       else if
                         allow_calls && description.opcode = Opcode.Ic_call_end
                       then
                         match checked_return_kind type_ with
                         | Some (Word_return word_type) ->
                             supported word_type type_
                         | Some Void_return -> Void_value
                         | None -> Unsupported
                       else
                         match (frame, description.opcode) with
                         | _, Opcode.Ic_imm_i64
                           when is_default description.instruction_id -> (
                             match
                               scalar_value_type ~allow_byte:true
                                 ~allow_public:true type_
                             with
                             | Some word_type -> supported word_type type_
                             | None -> Unsupported)
                         | _, opcode
                           when memory_enabled
                                && (opcode = Opcode.Ic_deref
                                  || opcode = Opcode.Ic_assign
                                   || Option.is_some (update_kind opcode)) -> (
                             match
                               scalar_value_type ~allow_byte ~allow_public:true
                                 type_
                             with
                             | Some word_type -> supported word_type type_
                             | None -> Unsupported)
                         | Some _, Opcode.Ic_rbp when frame_pointer type_ ->
                             Frame_base type_
                         | _, Opcode.Ic_imm_i64
                           when frame_pointer type_ && memory_enabled -> (
                             match description.payload with
                             | Some (Sequence.Integer offset) ->
                                 Frame_offset (type_, offset)
                             | _ -> Unsupported)
                         | _, Opcode.Ic_mul
                           when scalar_pointer_type type_ && memory_enabled ->
                             index_offset types description
                         | _, Opcode.Ic_add when frame_pointer type_ -> (
                             match indexed_address frame types description with
                             | Indexed_address _ as indexed -> indexed
                             | _ -> (
                                 match frame with
                                 | Some context ->
                                     address_slot context types description
                                 | None -> Unsupported))
                         | _, opcode
                           when (memory_enabled || allow_calls)
                                &&
                                match opcode_kind opcode with
                                | Some (Unary_kind _ | Binary_kind _) -> true
                                | _ -> false -> (
                             match
                               scalar_value_type ~allow_byte ~allow_public:true
                                 type_
                             with
                             | Some word_type -> supported word_type type_
                             | None -> Unsupported)
                         | _ -> (
                             match producer_word_type type_ with
                             | Some word_type -> supported word_type type_
                             | None -> Unsupported))
                   | None -> Unsupported)
             in
             Value_map.add result.value_id declared types)
       Value_map.empty

let operand_of_value types value_id =
  match Value_map.find_opt value_id types with
  | Some (Supported (expected_type, _, computation_type)) ->
      Option.map
        (fun computation_type -> { value_id; expected_type; computation_type })
        (function_return_word_type computation_type)
  | Some
      ( Pointer_value _
      | Void_value
      | Unsupported
      | Frame_base _
      | Frame_offset _
      | Frame_address _
      | Variadic_address _
      | Index_offset _
      | Indexed_address _
      | Global_address _ )
  | None -> None

let pointer_operand_of_value types pointer_value =
  match Value_map.find_opt pointer_value types with
  | Some (Pointer_value pointer_type) -> Some { pointer_value; pointer_type }
  | _ -> None

let memory_operand_of_value types id =
  match operand_of_value types id with
  | Some word -> Some (Word_operand word)
  | None ->
      Option.map
        (fun p -> Pointer_operand p)
        (pointer_operand_of_value types id)

let value_matches stored operand =
  match (stored, operand) with
  | (Stored_word _ | Stored_narrow _), Word_operand _ -> true
  | Stored_pointer expected, Pointer_operand actual ->
      Scalar.compatible_pointer expected actual.pointer_type
  | _ -> false

let storage_operand ?(allow_array = false) frame initialization types
    instruction address =
  match (frame, Value_map.find_opt address types) with
  | Some _, Some (Variadic_address pointee) when allow_array ->
      Some (Variadic_slot, pointee, Stored_word I64)
  | Some context, Some (Frame_address index) ->
      let slot = context.slots.(index) in
      if slot.strides <> [] && not allow_array then None
      else
        Some
          ( Frame_slot (index, slot.object_count),
            slot.slot_type,
            slot.stored_type )
  | _, Some (Global_address slot)
    when storage_allowed frame initialization instruction slot
         && (allow_array || Integer_globals.storage_dimensions slot = []) ->
      let type_ = Integer_globals.storage_type slot in
      Option.map
        (fun kind -> (Global_slot slot, type_, kind))
        (stored_type type_)
  | _, Some (Indexed_address (pointer_type, remaining))
    when allow_array || remaining = [] -> (
      match Type.dereference pointer_type with
      | Ok pointee ->
          Option.map
            (fun stored ->
              ( Indexed_slot { pointer_value = address; pointer_type },
                pointee,
                stored ))
            (stored_type pointee)
      | Error _ -> None)
  | _, Some (Pointer_value pointer_type) -> (
      match Type.dereference pointer_type with
      | Ok pointee ->
          Option.map
            (fun stored ->
              ( Indirect_slot { pointer_value = address; pointer_type },
                pointee,
                stored ))
            (stored_type pointee)
      | Error _ -> None)
  | _ -> None

let valid_unary_type types operation operand_id result_type =
  match Value_map.find_opt operand_id types with
  | Some (Supported (_, _, operand_type)) -> (
      let internal_i64 type_ =
        Type.pointer_depth type_ = 0
        &&
        match Type.base type_ with
        | Type.Primitive (Type.Internal_storage, Sema.Primitive_type.I64) ->
            true
        | _ -> false
      in
      match operation with
      | Complement -> internal_i64 result_type
      | Negate -> Type.equal (Computation.negate operand_type) result_type
      | Logical_not -> Type.equal (Computation.forward operand_type) result_type
      )
  | _ -> false

let promoted_word_type left right =
  match (left, right) with
  | I64, I64 -> I64
  | I64, U64 | U64, I64 | U64, U64 -> U64

let valid_binary_result_type types operation left right result_type =
  match operation with
  | Compare _ | Logical _ -> return_word_type result_type = Some I64
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_left
  | Shift_right -> (
      let raw_id type_ =
        match Type.base type_ with
        | Type.Primitive (_, primitive) when Type.pointer_depth type_ = 0 ->
            Some (Sema.Primitive_type.info primitive).raw_id
        | _ -> None
      in
      match (Value_map.find_opt left types, Value_map.find_opt right types) with
      | Some (Supported (_, _, left)), Some (Supported (_, _, right)) -> (
          match (raw_id left, raw_id right, raw_id result_type) with
          | Some left, Some right, Some actual -> actual = max left right
          | _ -> false)
      | _ -> false)

let shift_count bits = Int64.to_int (Int64.logand bits 63L)

let comparison_order left right =
  match promoted_word_type left.type_ right.type_ with
  | I64 -> Int64.compare left.bits right.bits
  | U64 -> Int64.unsigned_compare left.bits right.bits

let comparison_bits operation left right =
  let predicate =
    match operation with
    | Equal -> Int64.equal left.bits right.bits
    | Not_equal -> not (Int64.equal left.bits right.bits)
    | Less -> comparison_order left right < 0
    | Greater_equal -> comparison_order left right >= 0
    | Greater -> comparison_order left right > 0
    | Less_equal -> comparison_order left right <= 0
  in
  if predicate then 1L else 0L

let logical_bits operation left right =
  let left = not (Int64.equal left.bits 0L) in
  let right = not (Int64.equal right.bits 0L) in
  let predicate =
    match operation with
    | Logical_and -> left && right
    | Logical_or -> left || right
    | Logical_xor -> left <> right
  in
  if predicate then 1L else 0L

let divide_bits ~opcode ~remainder result_type left right =
  if Int64.equal right.bits 0L then
    Error ("HCIRVM0009", opcode ^ " divisor is zero")
  else if
    result_type = I64
    && Int64.equal left.bits Int64.min_int
    && Int64.equal right.bits (-1L)
  then
    (* BackA.HC:ICDiv and ICMod both execute IDIV. Its quotient overflows
       even when only the remainder would be consumed. *)
    Error ("HCIRVM0010", opcode ^ " signed quotient overflows I64")
  else
    let operation =
      match (result_type, remainder) with
      | I64, false -> Int64.div
      | I64, true -> Int64.rem
      | U64, false -> Int64.unsigned_div
      | U64, true -> Int64.unsigned_rem
    in
    Ok (operation left.bits right.bits)

let binary_bits ?(compound = false) operation left right result_type =
  match operation with
  | Divide ->
      divide_bits
        ~opcode:(if compound then "IC_DIV_EQU" else "IC_DIV")
        ~remainder:false result_type left right
  | Remainder ->
      divide_bits
        ~opcode:(if compound then "IC_MOD_EQU" else "IC_MOD")
        ~remainder:true result_type left right
  | Add -> Ok (Int64.add left.bits right.bits)
  | Subtract -> Ok (Int64.sub left.bits right.bits)
  | Multiply -> Ok (Int64.mul left.bits right.bits)
  | Bitwise_and -> Ok (Int64.logand left.bits right.bits)
  | Bitwise_or -> Ok (Int64.logor left.bits right.bits)
  | Bitwise_xor -> Ok (Int64.logxor left.bits right.bits)
  | Shift_left -> Ok (Int64.shift_left left.bits (shift_count right.bits))
  | Shift_right ->
      let shift =
        match result_type with
        | I64 -> Int64.shift_right
        | U64 -> Int64.shift_right_logical
      in
      Ok (shift left.bits (shift_count right.bits))
  | Compare comparison -> Ok (comparison_bits comparison left right)
  | Logical logical -> Ok (logical_bits logical left right)

let malformed block_id description =
  preflight_error block_id description "HCIRVM0004"
    (Printf.sprintf "%s has malformed operands, result, target type, or payload"
       (Opcode.to_source_name description.Sequence.opcode))

let unsupported_type block_id description =
  preflight_error block_id description "HCIRVM0005"
    (Printf.sprintf "%s uses an unsupported word type"
       (Opcode.to_source_name description.Sequence.opcode))

let invalid_type_matrix block_id description =
  preflight_error block_id description "HCIRVM0006"
    (Printf.sprintf "%s has an invalid operand/result word-type relationship"
       (Opcode.to_source_name description.Sequence.opcode))

let fresh_literal_image () = { literal_byte_count = 0; literal_chunks_rev = [] }

let collect_literals ~max_literal_bytes image graph =
  let ( let* ) = Result.bind in
  (* Each owner gets a fresh map, even if two definitions share a graph object.
     Only immutable payloads are retained here; byte cells are allocated after
     every owner and instruction has passed preflight. *)
  let* literal_regions =
    Graph.blocks graph
    |> List.fold_left
         (fun result block ->
           let* regions = result in
           let block_id = Graph.block_id block in
           Graph.instructions block |> Sequence.instructions
           |> List.fold_left
                (fun result instruction ->
                  let* regions = result in
                  let description = Sequence.description instruction in
                  if description.opcode <> Opcode.Ic_str_const then Ok regions
                  else
                    match
                      ( description.operands,
                        description.result,
                        description.target_type,
                        description.payload )
                    with
                    | [], Some _, Some type_, Some (Sequence.Bytes bytes) ->
                        if not (literal_pointer_type type_) then
                          Error
                            [
                              preflight_error block_id description "HCIRVM0005"
                                "IC_STR_CONST requires internal-storage U8*";
                            ]
                        else
                          let remaining =
                            min max_literal_bytes Sys.max_array_length
                            - image.literal_byte_count
                          in
                          let length = String.length bytes in
                          if length >= remaining then
                            Error
                              [
                                preflight_error block_id description
                                  "HCIRVM0021"
                                  "owned string literals exceed the literal \
                                   byte limit or host array capacity";
                              ]
                          else
                            let literal_count = length + 1 in
                            let literal_base = image.literal_byte_count in
                            image.literal_byte_count <-
                              literal_base + literal_count;
                            image.literal_chunks_rev <-
                              (literal_base, bytes) :: image.literal_chunks_rev;
                            Ok
                              (Instruction_map.add description.instruction_id
                                 { literal_base; literal_count }
                                 regions)
                    | _ -> Error [ malformed block_id description ])
                (Ok regions))
         (Ok Instruction_map.empty)
  in
  Ok { literal_graph = graph; literal_regions }

let prepare_instruction ?frame ?globals ?literals ?initialization
    ?(allow_public = false) ?(is_default = false) block_index types block_id
    (description : Sequence.description) =
  let memory_enabled =
    Option.is_some frame || Option.is_some globals || Option.is_some literals
  in
  let allow_byte = Option.is_some frame || Option.is_some literals in
  let produced =
    Option.bind description.result (fun result ->
        Value_map.find_opt result.value_id types)
  in
  let kind =
    match (frame, description.opcode) with
    | _, Opcode.Ic_str_const when Option.is_some literals ->
        Some Literal_address_kind
    | _, Opcode.Ic_mul
      when match produced with
           | Some (Index_offset _) -> true
           | _ -> false -> Some Scale_index_kind
    | _, Opcode.Ic_add
      when match produced with
           | Some (Indexed_address _) -> true
           | _ -> false -> Some Index_address_kind
    | _, Opcode.Ic_addr when memory_enabled -> Some Pointer_address_kind
    | _, (Opcode.Ic_imm_i64 | Opcode.Ic_abs_addr)
      when Option.is_some globals
           &&
           match description.payload with
           | Some (Sequence.Symbol _ | Sequence.Retained_global _) -> true
           | _ -> false -> Some Global_address_kind
    | Some _, Opcode.Ic_rbp -> Some Frame_address_kind
    | _, (Opcode.Ic_imm_i64 | Opcode.Ic_add)
      when memory_enabled
           && Option.fold ~none:false
                ~some:(fun type_ -> Type.pointer_depth type_ > 0)
                description.target_type -> Some Frame_address_kind
    | _, Opcode.Ic_deref when memory_enabled -> Some Load_slot_kind
    | _, Opcode.Ic_assign when memory_enabled -> Some Store_slot_kind
    | _, opcode when memory_enabled && Option.is_some (update_kind opcode) ->
        update_kind opcode
    | _ -> opcode_kind description.opcode
  in
  match kind with
  | None ->
      Error
        (preflight_error block_id description "HCIRVM0002"
           (Printf.sprintf "%s is outside the bounded integer interpreter"
              (Opcode.to_source_name description.opcode)))
  | Some kind ->
      let required_flags =
        match kind with
        | Discard_kind -> 0x000000200L
        | _ -> 0L
      in
      if description.flags <> required_flags then
        Error
          (preflight_error block_id description "HCIRVM0003"
             (Printf.sprintf "%s requires flags=0x%09Lx"
                (Opcode.to_source_name description.opcode)
                required_flags))
      else
        let operation =
          match kind with
          | Literal_address_kind -> (
              match
                ( literals,
                  description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | ( Some context,
                  [],
                  Some result,
                  Some pointer,
                  Some (Sequence.Bytes _) )
                when literal_pointer_type pointer -> (
                  match
                    ( Instruction_map.find_opt description.instruction_id
                        context.literal_regions,
                      Type.dereference pointer )
                  with
                  | Some region, Ok pointee ->
                      Ok
                        (Materialize_address
                           ( Literal_slot
                               (region.literal_base, region.literal_count),
                             result.value_id,
                             pointee ))
                  | _ -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Scale_index_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.payload,
                  produced )
              with
              | ( [ _; _ ],
                  Some result,
                  None,
                  Some (Index_offset (_, stride, operand)) ) ->
                  Ok (Scale_index (operand, stride, result.value_id))
              | _ -> Error (malformed block_id description))
          | Index_address_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.payload,
                  produced )
              with
              | ( [ base; offset ],
                  Some result,
                  None,
                  Some (Indexed_address (pointer, _)) ) -> (
                  match
                    ( storage_operand ~allow_array:true frame initialization
                        types description.instruction_id base,
                      Type.dereference pointer )
                  with
                  | ( Some (location, actual, (Stored_word _ | Stored_narrow _)),
                      Ok pointee )
                    when Type.equal actual pointee ->
                      Ok
                        (Index_address
                           (location, offset, result.value_id, pointee))
                  | _ -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Pointer_address_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ address ], Some result, Some target_type, None
                when scalar_pointer_type target_type -> (
                  match
                    storage_operand ~allow_array:true frame initialization types
                      description.instruction_id address
                  with
                  | Some (location, pointee, (Stored_word _ | Stored_narrow _))
                    -> (
                      match Type.pointer_to pointee with
                      | Ok expected when Type.equal expected target_type ->
                          Ok
                            (Materialize_address
                               (location, result.value_id, pointee))
                      | _ -> Error (invalid_type_matrix block_id description))
                  | _ -> Error (invalid_type_matrix block_id description))
              | _ -> Error (malformed block_id description))
          | Global_address_kind -> (
              match (description.operands, description.result) with
              | [], Some result -> (
                  match Value_map.find_opt result.value_id types with
                  | Some (Global_address _) -> Ok Frame_address_tick
                  | _ -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Frame_address_kind -> (
              match description.result with
              | Some result -> (
                  match
                    ( description.opcode,
                      description.operands,
                      description.payload,
                      Value_map.find_opt result.value_id types )
                  with
                  | Opcode.Ic_rbp, [], None, Some (Frame_base _)
                  | ( Opcode.Ic_imm_i64,
                      [],
                      Some (Sequence.Integer _),
                      Some (Frame_offset _) )
                  | Opcode.Ic_add, [ _; _ ], None, Some (Frame_address _)
                  | Opcode.Ic_add, [ _; _ ], None, Some (Variadic_address _) ->
                      Ok Frame_address_tick
                  | _ -> Error (malformed block_id description))
              | None -> Error (malformed block_id description))
          | Load_slot_kind
          | Store_slot_kind
          | Update_slot_kind _
          | Increment_slot_kind _ -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | address :: operands, Some result, Some target_type, None -> (
                  let slot =
                    storage_operand frame initialization types
                      description.instruction_id address
                  in
                  match slot with
                  | Some (location, slot_type, stored_type)
                    when Type.equal slot_type target_type -> (
                      match (kind, operands) with
                      | Load_slot_kind, [] ->
                          Ok (Load_slot (location, result.value_id))
                      | Store_slot_kind, [ operand ] -> (
                          match memory_operand_of_value types operand with
                          | Some operand when value_matches stored_type operand
                            ->
                              Ok
                                (Store_slot
                                   ( location,
                                     operand,
                                     result.value_id,
                                     stored_type ))
                          | _ ->
                              Error (invalid_type_matrix block_id description))
                      | Update_slot_kind operation, [ operand ]
                        when match stored_type with
                             | Stored_word _ | Stored_narrow _ -> true
                             | _ -> false -> (
                          match operand_of_value types operand with
                          | Some operand ->
                              Ok
                                (Update_slot
                                   ( location,
                                     operation,
                                     Some operand,
                                     false,
                                     result.value_id,
                                     stored_type ))
                          | None ->
                              Error (invalid_type_matrix block_id description))
                      | Increment_slot_kind (operation, old_result), []
                        when match stored_type with
                             | Stored_word _ | Stored_narrow _ -> true
                             | _ -> false ->
                          Ok
                            (Update_slot
                               ( location,
                                 operation,
                                 None,
                                 old_result,
                                 result.value_id,
                                 stored_type ))
                      | _ -> Error (malformed block_id description))
                  | _ -> Error (invalid_type_matrix block_id description))
              | _ -> Error (malformed block_id description))
          | Immediate_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], Some result, Some type_, Some (Sequence.Integer bits) -> (
                  match
                    if is_default then
                      scalar_value_type ~allow_byte:true ~allow_public:true
                        type_
                    else producer_word_type type_
                  with
                  | Some type_ ->
                      Ok (Immediate (result.value_id, { type_; bits }))
                  | None -> Error (unsupported_type block_id description))
              | _ -> Error (malformed block_id description))
          | Unary_kind unary -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], Some result, Some result_type, None -> (
                  match
                    scalar_value_type ~allow_byte
                      ~allow_public:(memory_enabled || allow_public)
                      result_type
                  with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match operand_of_value types operand_id with
                      | None -> Error (invalid_type_matrix block_id description)
                      | Some operand ->
                          let valid =
                            Option.fold ~none:false
                              ~some:(valid_unary_type types unary operand_id)
                              description.target_type
                          in
                          if valid then
                            Ok
                              (Unary
                                 (unary, operand, result.value_id, result_type))
                          else Error (invalid_type_matrix block_id description))
                  )
              | _ -> Error (malformed block_id description))
          | Word_view_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | ( [ operand_id ],
                  Some result,
                  Some result_type,
                  Some (Sequence.Integer (0L | 1L)) ) -> (
                  match producer_word_type result_type with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match operand_of_value types operand_id with
                      | None -> Error (invalid_type_matrix block_id description)
                      | Some operand
                        when match Value_map.find_opt operand_id types with
                             | Some (Supported (_, type_, _)) ->
                                 Option.is_some (return_word_type type_)
                             | _ -> false ->
                          Ok (Word_view (operand, result.value_id, result_type))
                      | Some _ ->
                          Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Binary_kind binary -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ left_id; right_id ], Some result, Some result_type, None -> (
                  match
                    scalar_value_type ~allow_byte
                      ~allow_public:(memory_enabled || allow_public)
                      result_type
                  with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match
                        ( operand_of_value types left_id,
                          operand_of_value types right_id )
                      with
                      | Some left, Some right
                        when Option.fold ~none:false
                               ~some:
                                 (valid_binary_result_type types binary left_id
                                    right_id)
                               description.target_type ->
                          Ok
                            (Binary
                               ( binary,
                                 left,
                                 right,
                                 result.value_id,
                                 result_type ))
                      | Some _, Some _ | None, _ | _, None ->
                          Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Discard_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, None, None -> (
                  match Value_map.find_opt operand_id types with
                  | Some Void_value -> Ok (Discard_void operand_id)
                  | _ -> (
                      match memory_operand_of_value types operand_id with
                      | Some (Word_operand _ as operand) -> Ok (Discard operand)
                      | Some (Pointer_operand _ as operand)
                        when Option.is_some frame || Option.is_some literals ->
                          Ok (Discard operand)
                      | _ -> Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Return_value_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, Some target_type, None
                when Option.fold ~none:true
                       ~some:(fun context ->
                         Type.equal context.return_type target_type)
                       frame -> (
                  match function_return_word_type target_type with
                  | None -> Error (unsupported_type block_id description)
                  | Some target_type -> (
                      match operand_of_value types operand_id with
                      | Some operand
                        when Option.is_some frame
                             || operand.expected_type = target_type ->
                          Ok (Return_value (operand, target_type))
                      | Some _ | None ->
                          Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Jump_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, Some (Sequence.Block target) -> (
                  match Block_map.find_opt target block_index with
                  | Some target -> Ok (Jump target)
                  | None -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Branch_kind condition -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, None, Some (Sequence.Block target) -> (
                  match
                    ( operand_of_value types operand_id,
                      Block_map.find_opt target block_index )
                  with
                  | Some operand, Some target ->
                      Ok (Branch (condition, operand, target))
                  | None, _ -> Error (invalid_type_matrix block_id description)
                  | Some _, None -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Return_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, None -> Ok Return
              | _ -> Error (malformed block_id description))
          | End_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, None when Option.is_none frame -> Ok End
              | _ -> Error (malformed block_id description))
        in
        Result.map
          (fun operation ->
            {
              instruction_id = description.instruction_id;
              span = description.span;
              operation;
              push_result = None;
              capture_discard = true;
            })
          operation

let prepare ?frame ?globals ?literals ?initialization ?callees ?runtime_calls
    ?(retained_functions = []) ?(runtime_owner = Runtime.Entry) graph =
  let ( let* ) = Result.bind in
  let is_default id =
    Option.fold ~none:false
      ~some:(fun context ->
        Runtime.is_prepared_default context ~owner:runtime_owner id)
      runtime_calls
  in
  let* () =
    match literals with
    | Some context when context.literal_graph != graph ->
        Error
          [
            make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0004"
              "literal storage requires its exact checked instruction graph";
          ]
    | _ -> Ok ()
  in
  let source_blocks = Graph.blocks graph in
  let block_count = List.length source_blocks in
  let block_index =
    source_blocks
    |> List.mapi (fun index block -> (Graph.block_id block, index))
    |> List.fold_left
         (fun map (block_id, index) -> Block_map.add block_id index map)
         Block_map.empty
  in
  let errors_rev = ref [] in
  let blocks =
    source_blocks
    |> List.mapi (fun index block ->
        let block_id = Graph.block_id block in
        let types =
          declared_types ?frame ?globals ?literals ?initialization
            ~allow_calls:(Option.is_some callees) ~is_default block
        in
        let instructions_rev = ref [] in
        let calls = ref [] in
        let call_error description message =
          preflight_error block_id description "HCIRVM0014" message
        in
        let call_instruction description operation =
          Ok
            {
              instruction_id = description.Sequence.instruction_id;
              span = description.span;
              operation;
              push_result = None;
              capture_discard = true;
            }
        in
        let prepare_call (description : Sequence.description) =
          let no_operands =
            description.operands = [] && description.flags = 0L
          in
          let target_matches callee =
            Option.fold ~none:false
              ~some:(Type.equal callee.callee_return_type)
              description.target_type
          in
          let site_id site select =
            Option.fold ~none:true
              ~some:(fun site ->
                Instruction_id.equal (select site) description.instruction_id)
              site
          in
          let runtime_callee site =
            let argument_type argument =
              let type_ = Runtime.argument_target_type argument in
              match
                scalar_value_type ~allow_byte:true ~allow_public:true type_
              with
              | Some word -> Some (Stored_word word)
              | None when scalar_pointer_type type_ ->
                  Some (Stored_pointer type_)
              | None -> None
            in
            let arguments = List.rev (Runtime.arguments site) in
            let types = List.filter_map argument_type arguments in
            if
              List.length types <> List.length arguments
              || List.length types > Sys.max_array_length
            then None
            else
              Some
                {
                  callee_index = -1;
                  callee_symbol = Runtime.symbol site;
                  callee_definition = None;
                  callee_return_type = Runtime.return_type site;
                  parameter_types = Array.of_list types;
                  cleanup_opcode = Runtime.cleanup_opcode site;
                  frame_bytes = 0;
                  variadic = false;
                }
          in
          match (description.opcode, !calls) with
          | Opcode.Ic_call_start, stack
            when no_operands && description.result = None
                 && description.target_type = None -> (
              match description.payload with
              | Some (Sequence.Symbol symbol) -> (
                  let site =
                    Option.bind runtime_calls (fun context ->
                        Runtime.find_start context ~owner:runtime_owner
                          description.instruction_id)
                  in
                  let selected =
                    match site with
                    | Some site
                      when Option.is_some (Runtime.provider site)
                           || Runtime.call_opcode site
                              = Opcode.Ic_call_indirect2
                           || Runtime.call_opcode site = Opcode.Ic_call_extern
                      -> runtime_callee site
                    | Some site when Runtime.call_opcode site <> Opcode.Ic_call
                      -> None
                    | Some site
                      when Option.is_some (Runtime.retained_function site) ->
                        let link =
                          Option.get (Runtime.retained_function site)
                        in
                        List.find_opt
                          (fun executable ->
                            Retained_function.same executable.function_link link)
                          retained_functions
                        |> Option.map (fun executable ->
                            executable.function_callee)
                    | _ ->
                        Option.value callees ~default:[]
                        |> List.find_opt (fun callee ->
                            callee.callee_symbol == symbol
                            &&
                            match (site, callee.callee_definition) with
                            | Some site, Some declaration ->
                                Runtime.declaration site == declaration
                            | _ -> true)
                  in
                  let selected =
                    Option.bind selected (fun callee ->
                        if not callee.variadic then Some callee
                        else
                          Option.bind site (fun site ->
                              Option.bind (runtime_callee site) (fun actual ->
                                  let fixed =
                                    Array.length callee.parameter_types
                                  in
                                  let types = actual.parameter_types in
                                  let tail = Array.length types - fixed - 1 in
                                  let same_type a b =
                                    match (a, b) with
                                    | Stored_pointer a, Stored_pointer b ->
                                        Type.equal a b
                                    | Stored_narrow scalar, Stored_word word ->
                                        scalar_runtime_type scalar = word
                                    | _ -> a = b
                                  in
                                  if
                                    tail < 0
                                    || Runtime.variadic_count site
                                       <> Some (Int64.of_int tail)
                                    || types.(fixed) <> Stored_word I64
                                    || (not
                                          (Array.for_all Fun.id
                                             (Array.mapi
                                                (fun i expected ->
                                                  same_type expected types.(i))
                                                callee.parameter_types)))
                                    || not
                                         (Array.for_all
                                            (function
                                              | Stored_word _ -> true
                                              | _ -> false)
                                            (Array.sub types (fixed + 1) tail))
                                  then None
                                  else
                                    Some
                                      {
                                        callee with
                                        parameter_types =
                                          Array.mapi
                                            (fun i type_ ->
                                              if i < fixed then
                                                callee.parameter_types.(i)
                                              else type_)
                                            types;
                                      })))
                  in
                  match selected with
                  | Some callee
                    when callee.callee_symbol == symbol
                         &&
                         match stack with
                         | [] | { phase = Collecting _; _ } :: _ -> true
                         | _ -> false ->
                      calls :=
                        {
                          callee;
                          site;
                          remaining_arguments =
                            Option.map Runtime.arguments site;
                          phase = Collecting 0;
                        }
                        :: stack;
                      call_instruction description
                        (Call_start
                           (Option.bind runtime_calls (fun context ->
                                Option.bind site
                                  (Runtime.entry_item_index context))))
                  | _ ->
                      Error
                        (call_error description
                           "direct call has no matching executable definition \
                            or valid enclosing call"))
              | _ -> Error (malformed block_id description))
          | ( (Opcode.Ic_call | Opcode.Ic_call_indirect2 | Opcode.Ic_call_extern),
              ({ callee; site; phase = Collecting count; _ } as call) :: rest )
            when no_operands && description.result = None
                 && target_matches callee
                 && description.opcode
                    = Option.fold ~none:Opcode.Ic_call ~some:Runtime.call_opcode
                        site
                 && site_id site Runtime.call_instruction -> (
              match description.payload with
              | Some (Sequence.Symbol symbol)
                when symbol == callee.callee_symbol
                     && count = Array.length callee.parameter_types ->
                  calls := { call with phase = Needs_cleanup } :: rest;
                  let operation =
                    match site with
                    | Some site
                      when Option.is_some (Runtime.provider site)
                           || Runtime.call_opcode site
                              = Opcode.Ic_call_indirect2
                           || Runtime.call_opcode site = Opcode.Ic_call_extern
                      -> Extern_call (site, callee.parameter_types)
                    | Some site -> (
                        match Runtime.retained_function site with
                        | Some link -> Retained_call link
                        | None -> Call callee.callee_index)
                    | None -> Call callee.callee_index
                  in
                  call_instruction description operation
              | _ ->
                  Error
                    (call_error description
                       "call target or pushed argument count disagrees with \
                        its definition"))
          | ( (Opcode.Ic_add_rsp | Opcode.Ic_add_rsp1),
              ({ callee; site; phase = Needs_cleanup; _ } as call) :: rest )
            when no_operands && description.result = None
                 && target_matches callee
                 && description.opcode = callee.cleanup_opcode
                 && site_id site Runtime.cleanup_instruction -> (
              match description.payload with
              | Some (Sequence.Integer bytes)
                when bytes
                     = Option.fold
                         ~none:
                           (Int64.mul 8L
                              (Int64.of_int
                                 (Array.length callee.parameter_types)))
                         ~some:Runtime.cleanup_bytes site ->
                  calls := { call with phase = Needs_end } :: rest;
                  call_instruction description Call_cleanup
              | _ ->
                  Error
                    (call_error description
                       "call cleanup does not match its fixed argument slots"))
          | Opcode.Ic_call_end, { callee; site; phase = Needs_end; _ } :: rest
            when no_operands && target_matches callee
                 && site_id site Runtime.last
                 && Option.fold ~none:true
                      ~some:(fun site ->
                        Option.fold ~none:false
                          ~some:(fun result ->
                            Value_id.equal result.Sequence.value_id
                              (Runtime.result_value site))
                          description.result)
                      site -> (
              match
                ( description.payload,
                  description.result,
                  checked_return_kind callee.callee_return_type )
              with
              | ( Some (Sequence.Symbol symbol),
                  Some result,
                  Some (Word_return type_) )
                when symbol == callee.callee_symbol ->
                  calls := rest;
                  call_instruction description
                    (Call_end (result.value_id, type_))
              | Some (Sequence.Symbol symbol), Some result, Some Void_return
                when symbol == callee.callee_symbol ->
                  calls := rest;
                  call_instruction description (Call_end_void result.value_id)
              | _ ->
                  Error
                    (call_error description
                       "call end does not match its checked target and result"))
          | ( ( Opcode.Ic_call_start
              | Opcode.Ic_call
              | Opcode.Ic_call_indirect2
              | Opcode.Ic_call_extern
              | Opcode.Ic_call_import
              | Opcode.Ic_call_end
              | Opcode.Ic_add_rsp
              | Opcode.Ic_add_rsp1 ),
              _ ) ->
              Error
                (call_error description
                   "direct call instructions have an invalid order, type or \
                    shape")
          | _, { phase = Needs_cleanup | Needs_end; _ } :: _ ->
              Error
                (call_error description
                   "direct call cleanup and call end must follow the call")
          | _ ->
              prepare_instruction ?frame ?globals ?literals ?initialization
                ~allow_public:true
                ~is_default:(is_default description.instruction_id)
                block_index types block_id description
        in
        Graph.instructions block |> Sequence.instructions
        |> List.iter (fun instruction ->
            let description = Sequence.description instruction in
            let pushes =
              Option.is_some callees
              && Int64.logand description.flags 0x2000L <> 0L
            in
            let checked_description =
              if pushes then
                {
                  description with
                  flags = Int64.logand description.flags (Int64.lognot 0x2000L);
                }
              else description
            in
            match
              if Option.is_some callees then prepare_call checked_description
              else
                prepare_instruction ?frame ?globals ?literals ?initialization
                  block_index types block_id description
            with
            | Ok prepared ->
                let control_transfer =
                  match prepared.operation with
                  | Jump _ | Branch _ | Return | End -> true
                  | _ -> false
                in
                if control_transfer && !calls <> [] then
                  errors_rev :=
                    call_error description
                      "call protocol cannot cross a basic-block boundary"
                    :: !errors_rev;
                let push_result =
                  if not pushes then None
                  else
                    match (description.result, !calls) with
                    | ( Some result,
                        ({
                           callee;
                           remaining_arguments;
                           phase = Collecting count;
                           _;
                         } as call)
                        :: rest )
                      when count < Array.length callee.parameter_types -> (
                        match memory_operand_of_value types result.value_id with
                        | Some operand
                          when value_matches
                                 callee.parameter_types.(Array.length
                                                           callee
                                                             .parameter_types
                                                         - 1 - count)
                                 operand
                               && Option.fold ~none:true
                                    ~some:(function
                                      | [] -> false
                                      | argument :: _ ->
                                          Instruction_id.equal
                                            (Runtime.argument_producer argument)
                                            description.instruction_id
                                          && Value_id.equal
                                               (Runtime.argument_value argument)
                                               result.value_id
                                          && Option.fold ~none:false
                                               ~some:
                                                 (Type.equal
                                                    (Runtime
                                                     .argument_source_type
                                                       argument))
                                               description.target_type)
                                    remaining_arguments ->
                            calls :=
                              {
                                call with
                                phase = Collecting (count + 1);
                                remaining_arguments =
                                  Option.map
                                    (function
                                      | [] -> []
                                      | _ :: rest -> rest)
                                    remaining_arguments;
                              }
                              :: rest;
                            Some operand
                        | _ ->
                            errors_rev :=
                              call_error description
                                "pushed argument does not match its checked \
                                 word or pointer parameter"
                              :: !errors_rev;
                            None)
                    | _ ->
                        errors_rev :=
                          call_error description
                            "argument push has no matching fixed parameter"
                          :: !errors_rev;
                        None
                in
                let implicit_discard =
                  Option.fold ~none:false
                    ~some:(fun context ->
                      Runtime.is_implicit_discard context ~owner:runtime_owner
                        description.instruction_id)
                    runtime_calls
                in
                if
                  implicit_discard
                  &&
                  match prepared.operation with
                  | Discard _ | Discard_void _ -> false
                  | _ -> true
                then
                  errors_rev :=
                    call_error description
                      "implicit output metadata does not identify a checked \
                       discard"
                    :: !errors_rev;
                instructions_rev :=
                  {
                    prepared with
                    push_result;
                    capture_discard = not implicit_discard;
                  }
                  :: !instructions_rev
            | Error error -> errors_rev := error :: !errors_rev);
        if !calls <> [] then
          errors_rev :=
            make_error ~stage:Preflight ~executed_steps:0 ~block_id "HCIRVM0014"
              "basic block ends inside an incomplete direct call"
            :: !errors_rev;
        {
          block_id;
          instructions = Array.of_list (List.rev !instructions_rev);
          fallthrough =
            (if index + 1 < block_count then Some (index + 1) else None);
        })
    |> Array.of_list
  in
  match List.rev !errors_rev with
  | _ :: _ as errors -> Error errors
  | [] -> (
      let entry_id = Graph.entry graph |> Graph.block_id in
      match Block_map.find_opt entry_id block_index with
      | Some entry_index ->
          let initial_slots =
            Option.fold ~none:[||]
              ~some:(fun context ->
                Array.append
                  (Array.map (fun slot -> slot.initial) context.slots)
                  context.initial_variadic)
              frame
          in
          Ok
            {
              blocks;
              entry_index;
              initial_slots;
              initial_frame_bytes =
                Option.fold ~none:0
                  ~some:(fun context -> context.allocated_bytes)
                  frame;
              variadic_base =
                Option.bind frame (fun context ->
                    Option.map
                      (fun _ -> Array.length context.slots)
                      context.variadic_location);
              is_function = Option.is_some frame;
              required_return =
                Option.bind frame (fun context ->
                    checked_return_kind context.return_type);
              owner = None;
            }
      | None ->
          Error
            [
              make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0004"
                "the verified graph entry is unavailable";
            ])

let runtime_error ?instruction block executed_steps code message =
  match instruction with
  | None ->
      make_error ~stage:Execution ~executed_steps ~block_id:block.block_id code
        message
  | Some instruction ->
      make_error ~stage:Execution ~executed_steps ~block_id:block.block_id
        ~instruction_id:instruction.instruction_id ?span:instruction.span code
        message

type call_completion = Pending | Completed_void | Completed_word of word

type call_scope = {
  arguments_rev : runtime_value list;
  completion : call_completion;
  publication_item : int option;
}

type caller = {
  saved_program : prepared;
  saved_owner : executable_owner;
  saved_block : int;
  saved_instruction : int;
  saved_values : runtime_value Value_map.t;
  saved_slots : runtime_storage;
  saved_return : word option;
  saved_calls : call_scope list;
  saved_publication_item : int option;
}

let storage_word slot bits =
  let type_ =
    match
      scalar_value_type ~allow_byte:true ~allow_public:true
        (Integer_globals.storage_type slot)
    with
    | Some type_ -> type_
    | None -> assert false
  in
  { type_; bits = Scalar.narrow_bits (Integer_globals.storage_type slot) bits }

let publish_array_payload ~slot ~cell_offset payload write =
  let base = Integer_globals.storage_index slot + cell_offset in
  match payload with
  | Integer_array_initializers.Word bits -> write base (storage_word slot bits)
  | Integer_array_initializers.Bytes bytes ->
      String.iteri
        (fun offset byte ->
          write (base + offset)
            (storage_word slot (Int64.of_int (Char.code byte))))
        bytes

let execute_prepared ?(callees = [||]) ?(aot_linked = false)
    ?(max_frame_bytes = Int.max_int) ?(max_call_depth = Int.max_int)
    ?(capture_last = false) ?on_capture ?initialization ?(global_words = [||])
    ?literal_image ?output ?stream_output ?generation_output ?admit
    ?(retained_regions = []) ?(retained_functions = []) ~max_steps program =
  let entry_program = program in
  let current_block = ref program.entry_index in
  let current_instruction = ref 0 in
  let values = ref Value_map.empty in
  let make_storage cells unknown_message =
    { cells = Array.copy cells; live = true; unknown_message }
  in
  let frame_storage cells =
    make_storage cells "the reached frame slot has not been initialized"
  in
  let global_storage =
    make_storage
      (Array.map (Option.map (fun word -> Runtime_word word)) global_words)
      "hosted execution reached an uninitialized JIT persistent object"
  in
  let global_region slot =
    let region =
      List.find_map
        (fun (owner, storage) ->
          match
            Integer_globals.find_allocated_storage owner
              (Integer_globals.storage_symbol slot)
          with
          | Some expected when Integer_globals.same_storage expected slot ->
              Some (storage, Integer_globals.storage_index expected)
          | _ -> None)
        retained_regions
      |> Option.value
           ~default:(global_storage, Integer_globals.storage_index slot)
    in
    region
  in
  let publications =
    Option.fold ~none:[] ~some:Global_initialization.publications initialization
    |> Array.of_list
  in
  let applied_publications = Array.make (Array.length publications) false in
  let publications_before =
    let by_instruction = ref Instruction_map.empty in
    Array.iteri
      (fun index publication ->
        let before = Global_initialization.publication_before publication in
        let previous =
          Instruction_map.find_opt before !by_instruction
          |> Option.value ~default:[]
        in
        by_instruction :=
          Instruction_map.add before
            ((index, publication) :: previous)
            !by_instruction)
      publications;
    Instruction_map.map List.rev !by_instruction
  in
  let publish_before instruction =
    Instruction_map.find_opt instruction publications_before
    |> Option.value ~default:[]
    |> List.iter (fun (index, publication) ->
        if not applied_publications.(index) then (
          publish_array_payload
            ~slot:(Global_initialization.publication_storage publication)
            ~cell_offset:
              (Global_initialization.publication_cell_offset publication)
            (Global_initialization.publication_payload publication)
            (fun cell word ->
              let storage, base =
                global_region
                  (Global_initialization.publication_storage publication)
              in
              let original =
                Integer_globals.storage_index
                  (Global_initialization.publication_storage publication)
              in
              storage.cells.(base + cell - original) <- Some (Runtime_word word));
          applied_publications.(index) <- true))
  in
  let literal_storage =
    let cells =
      match literal_image with
      | None -> [||]
      | Some image ->
          let cells =
            Array.make image.literal_byte_count
              (Some (Runtime_word { type_ = U64; bits = 0L }))
          in
          List.iter
            (fun (base, bytes) ->
              String.iteri
                (fun index byte ->
                  cells.(base + index) <-
                    Some
                      (Runtime_word
                         { type_ = U64; bits = Int64.of_int (Char.code byte) }))
                bytes)
            image.literal_chunks_rev;
          cells
    in
    {
      cells;
      live = true;
      unknown_message =
        "owned string literal byte is unexpectedly uninitialized";
    }
  in
  let owner =
    ref { owner_callees = callees; owner_literals = literal_storage }
  in
  let entry_owner = !owner in
  let publication_item = ref None in
  Option.iter (fun admit -> admit global_storage !owner) admit;
  let slots = ref (frame_storage program.initial_slots) in
  let program = ref program in
  let callers = ref [] in
  let calls = ref [] in
  let depth = ref 0 in
  let live_frame_bytes = ref !program.initial_frame_bytes in
  let final_value = ref None in
  let capture value =
    final_value := value;
    Option.iter (fun observe -> observe value) on_capture
  in
  let pending_return = ref None in
  let steps = ref 0 in
  let completed = ref None in
  let failed = ref None in
  let active_initializer = ref None in
  let transfer target =
    current_block := target;
    current_instruction := 0;
    values := Value_map.empty
  in
  let require_operand ?(computation = false) block instruction operand =
    match Value_map.find_opt operand.value_id !values with
    | Some (Runtime_word word) when word.type_ = operand.expected_type ->
        Some
          (if computation then { word with type_ = operand.computation_type }
           else word)
    | Some _ | None ->
        failed :=
          Some
            (runtime_error ~instruction block !steps "HCIRVM0008"
               "a prepared operand is unavailable or has the wrong word type");
        None
  in
  let address_bounds ~one_past block instruction address =
    let offset = address.pointer_offset in
    let width = Int64.of_int address.pointer_element_bytes in
    if
      offset < 0L
      || Int64.rem offset width <> 0L
      ||
      if one_past then offset > address.pointer_extent_bytes
      else offset > Int64.sub address.pointer_extent_bytes width
    then (
      failed :=
        Some
          (runtime_error ~instruction block !steps "HCIRVM0019"
             "indexed address is outside its declared object extent");
      false)
    else true
  in
  let require_pointer ?(bounded = true) block instruction operand =
    match Value_map.find_opt operand.pointer_value !values with
    | Some (Runtime_pointer address) -> (
        match Type.dereference operand.pointer_type with
        | Ok expected
          when Type.equal expected address.pointer_pointee
               && scalar_element_bytes expected
                  = Some address.pointer_element_bytes
               && address.pointer_element_bytes > 0
               && Int64.of_int address.pointer_count
                  <= Int64.div Int64.max_int
                       (Int64.of_int address.pointer_element_bytes)
               && address.pointer_extent_bytes
                  = Int64.mul
                      (Int64.of_int address.pointer_count)
                      (Int64.of_int address.pointer_element_bytes)
               && address.pointer_storage.live && address.pointer_base >= 0
               && address.pointer_count >= 0
               && address.pointer_count
                  <= Array.length address.pointer_storage.cells
               && address.pointer_base
                  <= Array.length address.pointer_storage.cells
                     - address.pointer_count ->
            if
              (not bounded)
              || address_bounds ~one_past:true block instruction address
            then Some address
            else None
        | _ ->
            failed :=
              Some
                (runtime_error ~instruction block !steps "HCIRVM0018"
                   "pointer does not identify a live object of its checked \
                    pointee type");
            None)
    | _ ->
        failed :=
          Some
            (runtime_error ~instruction block !steps "HCIRVM0018"
               "prepared pointer value is unavailable or invalid");
        None
  in
  let require_value block instruction = function
    | Word_operand operand ->
        Option.map
          (fun word -> Runtime_word word)
          (require_operand block instruction operand)
    | Pointer_operand operand ->
        Option.map
          (fun address -> Runtime_pointer address)
          (require_pointer block instruction operand)
  in
  let coerce_value expected = function
    | Runtime_offset _ | Runtime_void -> None
    | Runtime_word word -> (
        match expected with
        | Stored_word type_ -> Some (Runtime_word { type_; bits = word.bits })
        | Stored_narrow scalar ->
            Some
              (Runtime_word
                 {
                   type_ = scalar_runtime_type scalar;
                   bits = Scalar.normalize scalar word.bits;
                 })
        | _ -> None)
    | Runtime_pointer address -> (
        match expected with
        | Stored_pointer type_ -> (
            match
              (Type.pointer_to address.pointer_pointee, Type.dereference type_)
            with
            | Ok actual, Ok pointer_pointee
              when Scalar.compatible_pointer type_ actual
                   && address.pointer_storage.live ->
                Some (Runtime_pointer { address with pointer_pointee })
            | _ -> None)
        | _ -> None)
  in
  let prepare_arguments callee arguments =
    let rec fixed position rev arguments =
      if position < Array.length callee.parameter_types then
        match arguments with
        | value :: rest ->
            Option.bind
              (coerce_value callee.parameter_types.(position) value)
              (fun value -> fixed (position + 1) (Some value :: rev) rest)
        | [] -> None
      else if not callee.variadic then
        if arguments = [] then Some (Array.of_list (List.rev rev), [||])
        else None
      else
        match arguments with
        | Runtime_word count :: tail
          when count.type_ = I64 && count.bits = Int64.of_int (List.length tail)
          ->
            let rec words rev = function
              | [] -> Some (Array.of_list (List.rev rev))
              | Runtime_word word :: rest ->
                  words
                    (Some (Runtime_word { word with type_ = I64 }) :: rev)
                    rest
              | _ -> None
            in
            Option.map
              (fun tail ->
                ( Array.of_list (List.rev (Some (Runtime_word count) :: rev)),
                  tail ))
              (words [] tail)
        | _ -> None
    in
    fixed 0 [] arguments
  in
  let extern_target site visible_item =
    let successor callee =
      callee.callee_symbol == Runtime.symbol site
      && Option.fold ~none:false
           ~some:(fun later ->
             Sema.Function_resolution.is_joined_successor
               ~earlier:(Runtime.declaration site) ~later)
           callee.callee_definition
    in
    let local =
      Array.to_list entry_owner.owner_callees
      |> List.find_opt (fun (callee, _) ->
          successor callee
          && (aot_linked
             || Option.fold ~none:false
                  ~some:(fun item ->
                    Option.fold ~none:false
                      ~some:(fun declaration ->
                        declaration
                        |> Sema.Function_resolution.resolved_declaration_site
                        |> Sema.Function_resolution.declaration_site_function
                        |> Sema.Function_type_resolution.function_item_index
                        |> fun declared -> declared < item)
                      callee.callee_definition)
                  visible_item))
    in
    match local with
    | Some (callee, body) -> Some (callee, body, entry_owner)
    | None ->
        List.find_opt
          (fun executable -> successor executable.function_callee)
          retained_functions
        |> Option.map (fun executable ->
            ( executable.function_callee,
              executable.function_program,
              executable.function_owner ))
  in
  let extern_signature_matches site callee =
    let module Headers = Sema.Function_type_resolution in
    let module Functions = Sema.Function_resolution in
    match callee.callee_definition with
    | None -> false
    | Some declaration ->
        let header =
          declaration |> Functions.resolved_declaration_site
          |> Functions.declaration_site_function
        in
        let parameters header =
          header |> Headers.function_signature |> Headers.signature_parameters
        in
        let expected = parameters (Runtime.header site)
        and actual = parameters header in
        Type.equal callee.callee_return_type (Runtime.return_type site)
        && callee.cleanup_opcode = Runtime.cleanup_opcode site
        && callee.variadic = Option.is_some (Runtime.variadic_count site)
        && List.length expected = List.length actual
        && List.for_all2
             (fun expected actual ->
               let resolved parameter =
                 parameter |> Headers.parameter_type_reference
                 |> Sema.Type_reference.resolved_type
               in
               Type.equal (resolved expected) (resolved actual))
             expected actual
  in
  let resolve_address block instruction location pointer_pointee =
    let root pointer_storage pointer_base pointer_count =
      Option.map
        (fun pointer_element_bytes ->
          {
            pointer_storage;
            pointer_base;
            pointer_count;
            pointer_element_bytes;
            pointer_extent_bytes =
              Int64.mul
                (Int64.of_int pointer_count)
                (Int64.of_int pointer_element_bytes);
            pointer_offset = 0L;
            pointer_pointee;
          })
        (scalar_element_bytes pointer_pointee)
    in
    match location with
    | Variadic_slot ->
        Option.bind !program.variadic_base (fun base ->
            root !slots base (Array.length !slots.cells - base))
    | Frame_slot (base, count) -> root !slots base count
    | Global_slot slot ->
        let storage, base = global_region slot in
        root storage base (Integer_globals.storage_element_count slot)
    | Literal_slot (base, count) -> root !owner.owner_literals base count
    | Indirect_slot operand -> require_pointer block instruction operand
    | Indexed_slot operand ->
        require_pointer ~bounded:false block instruction operand
  in
  let resolve_location block instruction = function
    | Variadic_slot -> None
    | Frame_slot (index, _) -> Some (!slots, index)
    | Global_slot slot -> Some (global_region slot)
    | Literal_slot (index, _) -> Some (!owner.owner_literals, index)
    | Indirect_slot operand | Indexed_slot operand ->
        Option.bind (require_pointer ~bounded:false block instruction operand)
          (fun address ->
            if address_bounds ~one_past:false block instruction address then
              Some
                ( address.pointer_storage,
                  address.pointer_base
                  + Int64.to_int
                      (Int64.div address.pointer_offset
                         (Int64.of_int address.pointer_element_bytes)) )
            else None)
  in
  let read_output_byte block instruction address relative =
    let error code message =
      Error (runtime_error ~instruction block !steps code message)
    in
    let storage = address.pointer_storage in
    if
      (not storage.live)
      || address.pointer_element_bytes <> 1
      || scalar_element_bytes address.pointer_pointee <> Some 1
      || address.pointer_base < 0 || address.pointer_count <= 0
      || address.pointer_count > Array.length storage.cells
      || address.pointer_base
         > Array.length storage.cells - address.pointer_count
      || address.pointer_extent_bytes <> Int64.of_int address.pointer_count
    then
      error "HCIRVM0018"
        "output pointer does not identify a live owned U8 object"
    else if address.pointer_offset < 0L || relative < 0L then
      error "HCIRVM0019" "output scan is outside its declared object extent"
    else if relative > Int64.sub Int64.max_int address.pointer_offset then
      error "HCIRVM0020" "output scan offset exceeds the integer address range"
    else
      let offset = Int64.add address.pointer_offset relative in
      if offset >= address.pointer_extent_bytes then
        error "HCIRVM0019" "output scan is outside its declared object extent"
      else
        match storage.cells.(address.pointer_base + Int64.to_int offset) with
        | Some (Runtime_word { type_ = U64; bits })
          when bits >= 0L && bits <= 255L -> Ok (Char.chr (Int64.to_int bits))
        | None -> error "HCIRVM0012" storage.unknown_message
        | Some _ ->
            error "HCIRVM0008" "output scan reached an invalid byte cell"
  in
  let invoke_output block instruction site parameter_types scope =
    let provider_name =
      match Runtime.provider site with
      | Some Runtime.Print -> "Print"
      | Some Runtime.Put_chars -> "PutChars"
      | Some Runtime.Stream_print -> "StreamPrint"
      | None -> "runtime output"
    in
    let provider_message message =
      let message =
        if
          provider_name = "StreamPrint"
          && String.starts_with ~prefix:"Print " message
        then "StreamPrint" ^ String.sub message 5 (String.length message - 5)
        else message
      in
      if
        String.starts_with ~prefix:(provider_name ^ " ") message
        || String.starts_with ~prefix:(provider_name ^ ":") message
      then message
      else provider_name ^ ": " ^ message
    in
    let make_provider_error code message =
      runtime_error ~instruction block !steps code (provider_message message)
    in
    let error code message = Error (make_provider_error code message) in
    if !depth >= max_call_depth then
      error "HCIRVM0015" "the runtime call depth limit was exhausted"
    else if
      Array.length parameter_types > (max_frame_bytes - !live_frame_bytes) / 8
    then
      error "HCIRVM0011"
        "runtime argument slots exceed the active frame byte limit"
    else if List.length scope.arguments_rev <> Array.length parameter_types then
      error "HCIRVM0008" "prepared runtime argument count is inconsistent"
    else
      let rec arguments index rev = function
        | [] -> Ok (List.rev rev)
        | value :: rest -> (
            match coerce_value parameter_types.(index) value with
            | Some value -> arguments (index + 1) (value :: rev) rest
            | None ->
                error "HCIRVM0008"
                  "runtime argument disagrees with its checked slot")
      in
      let ( let* ) = Result.bind in
      let* arguments = arguments 0 [] scope.arguments_rev in
      let selected_output =
        match Runtime.provider site with
        | Some Runtime.Stream_print -> (
            match stream_output with
            | Some _ -> stream_output
            | None -> (
                match generation_output with
                | Some _ -> generation_output
                | None ->
                    Option.map
                      (fun output ->
                        Output.share_work output
                          ~max_output_bytes:(16 * 1024 * 1024))
                      output))
        | _ -> output
      in
      match selected_output with
      | None -> error "HCIRVM0008" "prepared runtime call has no output state"
      | Some output -> (
          let provider_error = function
            | Output.Memory error ->
                { error with message = provider_message error.message }
            | Output.Output_limit ->
                if Runtime.provider site = Some Runtime.Stream_print then
                  make_provider_error "HCIRVM0028"
                    "generated output exceeds the task generated byte limit"
                else
                  make_provider_error "HCIRVM0022"
                    "runtime output exceeds the output byte limit"
            | Output.Work_limit ->
                make_provider_error "HCIRVM0023"
                  "runtime output work limit was exhausted"
            | Output.Offset_overflow ->
                make_provider_error "HCIRVM0020"
                  "output scan offset exceeds the integer address range"
            | Output.Invalid_format message ->
                make_provider_error "HCIRVM0024" message
            | Output.Invalid_argument message ->
                make_provider_error "HCIRVM0025" message
          in
          match (Runtime.provider site, arguments) with
          | Some Runtime.Put_chars, [ Runtime_word word ] ->
              Output.put_chars output word.bits
              |> Result.map_error provider_error
          | ( Some (Runtime.Print | Runtime.Stream_print),
              Runtime_pointer format :: Runtime_word count :: tail )
            when count.type_ = I64
                 && count.bits = Int64.of_int (List.length tail) ->
              let rec variadic rev = function
                | [] -> Ok (Array.of_list (List.rev rev))
                | Runtime_word word :: rest ->
                    variadic (Output.Word word.bits :: rev) rest
                | Runtime_pointer address :: rest ->
                    variadic (Output.Pointer address :: rev) rest
                | (Runtime_offset _ | Runtime_void) :: _ ->
                    error "HCIRVM0008"
                      "prepared variadic output argument is invalid"
              in
              let* arguments = variadic [] tail in
              let inactive =
                Runtime.provider site = Some Runtime.Stream_print
                && Option.is_none stream_output
              in
              let format_call =
                if inactive then Output.discard_print else Output.print
              in
              let* () =
                format_call output
                  ~read_byte:(read_output_byte block instruction)
                  ~format arguments
                |> Result.map_error provider_error
              in
              if inactive then
                error "HCIRVM0027" "requires an active task generation buffer"
              else Ok ()
          | _ ->
              error "HCIRVM0008"
                "prepared runtime provider arguments are inconsistent")
  in
  while Option.is_none !completed && Option.is_none !failed do
    if !current_block < 0 || !current_block >= Array.length !program.blocks then
      failed :=
        Some
          (make_error ~stage:Execution ~executed_steps:!steps "HCIRVM0008"
             "the prepared block cursor is out of bounds")
    else
      let block = !program.blocks.(!current_block) in
      if !current_instruction >= Array.length block.instructions then
        match block.fallthrough with
        | Some target -> transfer target
        | None ->
            failed :=
              Some
                (runtime_error block !steps "HCIRVM0008"
                   "execution reached an impossible final-block fallthrough")
      else
        let instruction = block.instructions.(!current_instruction) in
        let () =
          if not !program.is_function then (
            active_initializer :=
              Option.bind initialization (fun context ->
                  Global_initialization.find_storage context
                    instruction.instruction_id);
            if !program == entry_program then
              publish_before instruction.instruction_id)
        in
        if !steps >= max_steps then
          failed :=
            Some
              (runtime_error ~instruction block !steps "HCIRVM0007"
                 "the bounded integer execution step limit was exhausted")
        else (
          steps := !steps + 1;
          current_instruction := !current_instruction + 1;
          (match instruction.operation with
          | Call_start item ->
              calls :=
                {
                  arguments_rev = [];
                  completion = Pending;
                  publication_item =
                    (match item with
                    | Some _ -> item
                    | None -> !publication_item);
                }
                :: !calls
          | Call_cleanup -> ()
          | (Call _ | Retained_call _ | Extern_call _) as operation -> (
              let target =
                match operation with
                | Call index
                  when index >= 0 && index < Array.length !owner.owner_callees
                  ->
                    let callee, body = !owner.owner_callees.(index) in
                    Some (callee, body, !owner)
                | Retained_call link ->
                    List.find_opt
                      (fun executable ->
                        Retained_function.same executable.function_link link)
                      retained_functions
                    |> Option.map (fun executable ->
                        ( executable.function_callee,
                          executable.function_program,
                          executable.function_owner ))
                | Extern_call (site, _) ->
                    let visible_item =
                      match !calls with
                      | scope :: _ -> scope.publication_item
                      | [] -> None
                    in
                    extern_target site visible_item
                | _ -> None
              in
              match (!calls, target) with
              | ({ completion = Pending; _ } as scope) :: rest, None -> (
                  match operation with
                  | Extern_call (site, parameter_types) ->
                      if Option.is_some (Runtime.provider site) then
                        match
                          invoke_output block instruction site parameter_types
                            scope
                        with
                        | Ok () ->
                            calls :=
                              { scope with completion = Completed_void } :: rest
                        | Error error -> failed := Some error
                      else
                        failed :=
                          Some
                            (runtime_error ~instruction block !steps
                               "HCIRVM0030"
                               "the reached extern function has no published \
                                executable definition")
                  | _ ->
                      failed :=
                        Some
                          (runtime_error ~instruction block !steps "HCIRVM0008"
                             "prepared direct call has no available caller \
                              scope"))
              | ( ({ completion = Pending; _ } as scope) :: _,
                  Some (callee, body, callee_owner) ) -> (
                  let tail_count =
                    if callee.variadic then
                      List.length scope.arguments_rev
                      - Array.length callee.parameter_types
                      - 1
                    else 0
                  in
                  if
                    match operation with
                    | Extern_call (site, _) ->
                        not (extern_signature_matches site callee)
                    | _ -> false
                  then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0014"
                           "published extern definition disagrees with the \
                            captured call signature")
                  else if !depth >= max_call_depth then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0015"
                           "the integer call depth limit was exhausted")
                  else if
                    callee.frame_bytes > max_frame_bytes - !live_frame_bytes
                    || tail_count
                       > (max_frame_bytes - !live_frame_bytes
                        - callee.frame_bytes)
                         / 8
                    || tail_count
                       > Sys.max_array_length - Array.length body.initial_slots
                  then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0011"
                           "the active function frames exceed the frame byte \
                            limit")
                  else
                    match prepare_arguments callee scope.arguments_rev with
                    | None ->
                        failed :=
                          Some
                            (runtime_error ~instruction block !steps
                               "HCIRVM0008"
                               "prepared arguments disagree with the checked \
                                function frame")
                    | Some (arguments, tail) ->
                        let frame_bytes =
                          callee.frame_bytes + (8 * Array.length tail)
                        in
                        callers :=
                          {
                            saved_program = !program;
                            saved_owner = !owner;
                            saved_block = !current_block;
                            saved_instruction = !current_instruction;
                            saved_values = !values;
                            saved_slots = !slots;
                            saved_return = !pending_return;
                            saved_calls = !calls;
                            saved_publication_item = !publication_item;
                          }
                          :: !callers;
                        incr depth;
                        live_frame_bytes := !live_frame_bytes + frame_bytes;
                        let initialized =
                          frame_storage (Array.append body.initial_slots tail)
                        in
                        Array.blit arguments 0 initialized.cells 0
                          (Array.length arguments);
                        program :=
                          { body with initial_frame_bytes = frame_bytes };
                        publication_item := scope.publication_item;
                        owner := callee_owner;
                        slots := initialized;
                        values := Value_map.empty;
                        pending_return := None;
                        calls := [];
                        current_block := body.entry_index;
                        current_instruction := 0)
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared direct call has no available caller scope"))
          | Call_end (result, type_) -> (
              match !calls with
              | { completion = Completed_word word; _ } :: rest
                when word.type_ = type_ ->
                  calls := rest;
                  values := Value_map.add result (Runtime_word word) !values
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "direct call did not supply its declared return word"))
          | Call_end_void result -> (
              match !calls with
              | { completion = Completed_void; _ } :: rest ->
                  calls := rest;
                  values := Value_map.add result Runtime_void !values
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared U0 call did not complete without a value"))
          | Frame_address_tick -> ()
          | Scale_index (operand, stride, result) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some index ->
                  if
                    (index.type_ = U64 && index.bits < 0L)
                    || index.bits > Int64.div Int64.max_int stride
                    || index.bits < Int64.div Int64.min_int stride
                  then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0020"
                           "index byte scaling exceeds the hosted signed \
                            address range")
                  else
                    values :=
                      Value_map.add result
                        (Runtime_offset (Int64.mul index.bits stride))
                        !values)
          | Index_address (location, offset, result, pointee) -> (
              match
                ( resolve_address block instruction location pointee,
                  Value_map.find_opt offset !values )
              with
              | Some address, Some (Runtime_offset delta) ->
                  if
                    delta > 0L
                    && address.pointer_offset > Int64.sub Int64.max_int delta
                    || delta < 0L
                       && address.pointer_offset < Int64.sub Int64.min_int delta
                  then
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0020"
                           "index address addition exceeds the hosted signed \
                            address range")
                  else
                    values :=
                      Value_map.add result
                        (Runtime_pointer
                           {
                             address with
                             pointer_offset =
                               Int64.add address.pointer_offset delta;
                           })
                        !values
              | None, _ -> ()
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared index offset is unavailable"))
          | Materialize_address (location, result, pointer_pointee) -> (
              match
                resolve_address block instruction location pointer_pointee
              with
              | Some address
                when address_bounds ~one_past:true block instruction address ->
                  values :=
                    Value_map.add result (Runtime_pointer address) !values
              | _ -> ())
          | Load_slot (location, result) -> (
              match resolve_location block instruction location with
              | None -> ()
              | Some (storage, index) -> (
                  match storage.cells.(index) with
                  | Some value -> values := Value_map.add result value !values
                  | None ->
                      failed :=
                        Some
                          (runtime_error ~instruction block !steps "HCIRVM0012"
                             storage.unknown_message)))
          | Store_slot (location, operand, result, type_) -> (
              match require_value block instruction operand with
              | None -> ()
              | Some operand -> (
                  match
                    ( coerce_value type_ operand,
                      resolve_location block instruction location )
                  with
                  | Some value, Some (storage, index) ->
                      storage.cells.(index) <- Some value;
                      let expression_value =
                        match (type_, operand) with
                        | Stored_narrow scalar, Runtime_word word ->
                            Runtime_word
                              {
                                type_ = scalar_runtime_type scalar;
                                bits = word.bits;
                              }
                        | _ -> value
                      in
                      values := Value_map.add result expression_value !values
                  | _, None -> ()
                  | None, _ ->
                      failed :=
                        Some
                          (runtime_error ~instruction block !steps "HCIRVM0008"
                             "prepared store disagrees with its checked \
                              storage type")))
          | Update_slot
              (location, operation, operand, old_result, result, stored) -> (
              let type_ =
                match stored with
                | Stored_word type_ -> type_
                | Stored_narrow scalar -> scalar_runtime_type scalar
                | Stored_pointer _ -> assert false
              in
              let right =
                match operand with
                | None -> Some { type_; bits = 1L }
                | Some operand -> require_operand block instruction operand
              in
              match right with
              | None -> ()
              | Some right -> (
                  match resolve_location block instruction location with
                  | None -> ()
                  | Some (storage, index) -> (
                      match storage.cells.(index) with
                      | None ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps
                                 "HCIRVM0012" storage.unknown_message)
                      | Some
                          (Runtime_pointer _ | Runtime_offset _ | Runtime_void)
                        ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps
                                 "HCIRVM0008"
                                 "scalar update reached a pointer-valued slot")
                      | Some (Runtime_word old) -> (
                          (* Read the original object after RHS effects, including calls through aliases. *)
                          match
                            binary_bits ~compound:true operation old right type_
                          with
                          | Error (code, message) ->
                              failed :=
                                Some
                                  (runtime_error ~instruction block !steps code
                                     message)
                          | Ok bits ->
                              let computed = { type_; bits } in
                              let word =
                                match stored with
                                | Stored_narrow scalar ->
                                    {
                                      type_;
                                      bits = Scalar.normalize scalar bits;
                                    }
                                | Stored_word _ -> computed
                                | Stored_pointer _ -> assert false
                              in
                              storage.cells.(index) <- Some (Runtime_word word);
                              values :=
                                Value_map.add result
                                  (Runtime_word
                                     (if old_result then old
                                      else if Option.is_some operand then
                                        computed
                                      else word))
                                  !values))))
          | Immediate (result, word) ->
              values := Value_map.add result (Runtime_word word) !values
          | Unary (operation, operand, result, result_type) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some operand ->
                  let bits =
                    match operation with
                    | Complement -> Int64.lognot operand.bits
                    | Logical_not ->
                        if Int64.equal operand.bits 0L then 1L else 0L
                    | Negate -> Int64.neg operand.bits
                  in
                  values :=
                    Value_map.add result
                      (Runtime_word { type_ = result_type; bits })
                      !values)
          | Word_view (operand, result, result_type) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some operand ->
                  values :=
                    Value_map.add result
                      (Runtime_word { type_ = result_type; bits = operand.bits })
                      !values)
          | Binary (operation, left, right, result, result_type) -> (
              match
                require_operand ~computation:true block instruction left
              with
              | None -> ()
              | Some left -> (
                  match
                    require_operand ~computation:true block instruction right
                  with
                  | None -> ()
                  | Some right -> (
                      match binary_bits operation left right result_type with
                      | Ok bits ->
                          values :=
                            Value_map.add result
                              (Runtime_word { type_ = result_type; bits })
                              !values
                      | Error (code, message) ->
                          failed :=
                            Some
                              (runtime_error ~instruction block !steps code
                                 message))))
          | Discard operand ->
              let value =
                Option.bind (require_value block instruction operand) (function
                  | Runtime_word word -> Some word
                  | Runtime_pointer _ | Runtime_offset _ | Runtime_void -> None)
              in
              if
                capture_last && instruction.capture_discard
                && (not !program.is_function)
                && Option.is_none !active_initializer
                && Option.is_none !failed
              then capture value
          | Discard_void value_id -> (
              match Value_map.find_opt value_id !values with
              | Some Runtime_void ->
                  if
                    capture_last && instruction.capture_discard
                    && (not !program.is_function)
                    && Option.is_none !active_initializer
                  then capture None
              | _ ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "prepared no-value result is unavailable or invalid"))
          | Return_value (operand, type_) -> (
              match require_operand block instruction operand with
              | Some word -> pending_return := Some { type_; bits = word.bits }
              | None -> ())
          | Jump target -> transfer target
          | Branch (condition, operand, target) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some word -> (
                  let is_zero = Int64.equal word.bits 0L in
                  let take_target =
                    match condition with
                    | Zero -> is_zero
                    | Not_zero -> not is_zero
                  in
                  if take_target then transfer target
                  else
                    match block.fallthrough with
                    | Some fallthrough -> transfer fallthrough
                    | None ->
                        failed :=
                          Some
                            (runtime_error ~instruction block !steps
                               "HCIRVM0008"
                               "a conditional branch has no physical \
                                fallthrough")))
          | Return
            when match !program.required_return with
                 | Some (Word_return _) -> Option.is_none !pending_return
                 | Some Void_return | None -> false ->
              failed :=
                Some
                  (runtime_error ~instruction block !steps "HCIRVM0013"
                     "the integer function returned without a value")
          | Return -> (
              let completion =
                match (!program.required_return, !pending_return) with
                | (Some Void_return | None), None -> Some Completed_void
                | Some (Word_return type_), Some word when word.type_ = type_ ->
                    Some (Completed_word word)
                | None, Some word -> Some (Completed_word word)
                | _ -> None
              in
              match completion with
              | None ->
                  failed :=
                    Some
                      (runtime_error ~instruction block !steps "HCIRVM0008"
                         "function completion disagrees with its checked \
                          return kind")
              | Some completion -> (
                  !slots.live <- false;
                  match !callers with
                  | [] -> completed := Some (Returned !pending_return)
                  | caller :: rest -> (
                      callers := rest;
                      decr depth;
                      live_frame_bytes :=
                        !live_frame_bytes - !program.initial_frame_bytes;
                      program := caller.saved_program;
                      owner := caller.saved_owner;
                      publication_item := caller.saved_publication_item;
                      current_block := caller.saved_block;
                      current_instruction := caller.saved_instruction;
                      values := caller.saved_values;
                      slots := caller.saved_slots;
                      pending_return := caller.saved_return;
                      calls :=
                        match caller.saved_calls with
                        | scope :: rest -> { scope with completion } :: rest
                        | [] -> [])))
          | End -> completed := Some Stream_end);
          if Option.is_none !failed then
            Option.iter
              (fun operand ->
                match (require_value block instruction operand, !calls) with
                | Some word, scope :: rest ->
                    calls :=
                      { scope with arguments_rev = word :: scope.arguments_rev }
                      :: rest
                | _ ->
                    failed :=
                      Some
                        (runtime_error ~instruction block !steps "HCIRVM0008"
                           "prepared argument push has no active call"))
              instruction.push_result)
  done;
  !slots.live <- false;
  List.iter (fun caller -> caller.saved_slots.live <- false) !callers;
  if Option.is_none admit then (
    global_storage.live <- false;
    literal_storage.live <- false);
  match (!failed, !completed) with
  | Some error, _ ->
      let error =
        match !program.owner with
        | None -> error
        | Some (function_id, function_name) ->
            {
              error with
              function_id = Some function_id;
              function_name = Some function_name;
            }
      in
      Error [ identify_initializer !active_initializer error ]
  | None, Some termination ->
      Ok
        {
          termination_ = termination;
          executed_steps_ = !steps;
          final_value_ = !final_value;
          compiled_initializer_steps_ =
            Option.fold ~none:0 ~some:Global_initialization.prepared_steps
              initialization;
        }
  | None, None ->
      Error
        [
          make_error ~stage:Execution ~executed_steps:!steps "HCIRVM0008"
            "bounded integer execution stopped without a result";
        ]

let execute ~max_steps checked =
  if max_steps <= 0 then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps must be greater than zero";
      ]
  else
    match prepare (X87.graph checked) with
    | Error errors -> Error errors
    | Ok program -> execute_prepared ~max_steps program

let execute_function ?(max_literal_bytes = 1_048_576) ~max_steps
    ~max_frame_bytes ~frame ~arguments function_ =
  let ( let* ) = Result.bind in
  let* () =
    validate_dimension_dependencies None
      (Dimension_requirements.frame frame
      @ Function.dimension_dependencies function_)
    |> Result.map_error (fun message ->
        [ make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0026" message ])
  in
  if max_steps <= 0 || max_frame_bytes <= 0 || max_literal_bytes <= 0 then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps, max_frame_bytes and max_literal_bytes must be greater \
           than zero";
      ]
  else
    match frame_context ~max_frame_bytes ~frame ~arguments function_ with
    | Error errors -> Error errors
    | Ok frame -> (
        let function_id =
          Function.Function_id.to_int (Function.function_id function_)
        and function_name = Sema.Symbol.name (Function.symbol function_) in
        let identify error =
          {
            error with
            function_id = Some function_id;
            function_name = Some function_name;
          }
        in
        let literal_image = fresh_literal_image () in
        let* literals =
          collect_literals ~max_literal_bytes literal_image
            (Function.body function_)
          |> Result.map_error (List.map identify)
        in
        match
          prepare ~frame ~literals (Function.body function_)
          |> Result.map_error (List.map identify)
        with
        | Error errors -> Error errors
        | Ok program ->
            execute_prepared ~literal_image ~max_steps
              { program with owner = Some (function_id, function_name) })

let execute_program_with_output ?task ?isolated_budget
    ?(initializer_mode = false) ?(capture_fragment_value = false) ?runtime_calls
    ~output ?globals ?initialization ?(max_global_bytes = 1_048_576)
    ?(max_literal_bytes = 1_048_576) ~max_steps ~max_frame_bytes ~max_call_depth
    ~functions checked =
  let ( let* ) = Result.bind in
  let* () =
    let dependencies =
      Option.fold ~none:[] ~some:Integer_globals.dimension_dependencies globals
      @ Option.fold ~none:[] ~some:Runtime.dimension_dependencies runtime_calls
      @ List.concat_map
          (fun (definition : function_definition) ->
            Dimension_requirements.frame definition.frame
            @ Function.dimension_dependencies definition.body)
          functions
    in
    validate_dimension_dependencies task dependencies
    |> Result.map_error (fun message ->
        [ make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0026" message ])
  in
  let accounting =
    match (task, isolated_budget) with
    | Some task, None | None, Some task -> Some task
    | None, None -> None
    | Some _, Some _ -> invalid_arg "execution has two accounting owners"
  in
  let* () =
    let phases =
      Option.fold ~none:[] ~some:Runtime.original_phases runtime_calls
    in
    if
      List.for_all
        (fun phase ->
          Option.fold ~none:false
            ~some:(fun task -> owns_call_phase task phase)
            accounting)
        phases
    then Ok ()
    else
      Error
        [
          make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0014"
            "original call phases lack their owning runtime admission";
        ]
  in
  if
    max_steps <= 0 || max_frame_bytes <= 0 || max_call_depth <= 0
    || max_global_bytes <= 0 || max_literal_bytes <= 0
  then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps, max_frame_bytes, max_call_depth, max_global_bytes and \
           max_literal_bytes must be greater than zero";
      ]
  else if
    Option.fold ~none:false
      ~some:(fun context ->
        not
          (Runtime.matches context ~entry:checked ~initialization
             ~functions:
               (List.map
                  (fun (definition : function_definition) -> definition.body)
                  functions)))
      runtime_calls
  then
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0014"
          "runtime call metadata requires its exact entry and function bodies";
      ]
  else if
    Option.fold ~none:false
      ~some:(fun globals ->
        Integer_globals.byte_size globals
        > max_global_bytes
          - Option.fold ~none:0
              ~some:(fun task -> task.global_bytes)
              isolated_budget)
      globals
  then
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0016"
          "program global storage exceeds the global byte limit";
      ]
  else if
    match (globals, initialization) with
    | Some globals, Some context ->
        not (Global_initialization.matches context ~globals ~entry:checked)
    | None, Some _ -> true
    | Some globals, None -> Integer_globals.has_initializers globals
    | None, None -> false
  then
    Error
      [
        make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0017"
          "global initializer execution requires its matching checked \
           initialization context";
      ]
  else
    let* () =
      let invalid code message =
        Error [ make_error ~stage:Preflight ~executed_steps:0 code message ]
      in
      match (task, globals) with
      | None, Some globals when Integer_globals.is_task_command globals ->
          invalid "HCIRVM0026"
            "compiled task storage requires its owning task runtime"
      | None, _ -> Ok ()
      | Some _, None ->
          invalid "HCIRVM0026" "task command has no checked storage context"
      | Some task, Some globals -> (
          if
            Integer_globals.has_source_command globals
            && not
                 (match (runtime_calls, initialization) with
                 | Some runtime_calls, Some initialization ->
                     List.exists
                       (fun program ->
                         matches_source_program program ~runtime_calls ~globals
                           ~initialization ~functions checked)
                       task.source_programs
                 | _ -> false)
          then
            invalid "HCIRVM0026"
              "task source order requires its exact compiled program"
          else if List.exists (fun entry -> entry == checked) task.started then
            invalid "HCIRVM0026" "task command has already started"
          else if
            Integer_globals.byte_size globals
            > max_global_bytes - task.global_bytes
          then
            invalid "HCIRVM0016"
              "task global storage exceeds the cumulative byte limit"
          else if initializer_mode then Ok ()
          else
            match Integer_globals.check_task_command task.catalog globals with
            | Error message -> invalid "HCIRVM0026" message
            | Ok () -> Ok ())
    in
    let owner body =
      ( Function.Function_id.to_int (Function.function_id body),
        Sema.Symbol.name (Function.symbol body) )
    in
    let identify body error =
      let function_id, function_name = owner body in
      {
        error with
        function_id = Some function_id;
        function_name = Some function_name;
      }
    in
    let rec summaries index symbols ids rev = function
      | [] -> Ok (List.rev rev)
      | ({ frame; body } : function_definition) :: rest ->
          let symbol = Function.callable_symbol body in
          let function_id =
            Function.Function_id.to_int (Function.function_id body)
          in
          let parameter_count = List.length (Function.parameters body) in
          if
            List.exists
              (fun other ->
                Sema.Symbol.Id.equal (Sema.Symbol.id other)
                  (Sema.Symbol.id symbol))
              symbols
            || List.mem function_id ids
          then
            Error
              [
                identify body
                  (make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0014"
                     "integer program definitions have duplicate function or \
                      symbol identities");
              ]
          else if parameter_count > max_frame_bytes / 8 then
            Error
              [
                identify body
                  (make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0011"
                     "function parameters exceed the frame byte limit");
              ]
          else
            let* context =
              frame_context ?globals ~pointer_arguments:true ~max_frame_bytes
                ~frame
                ~arguments:(List.init parameter_count (fun _ -> 0L))
                body
              |> Result.map_error (List.map (identify body))
            in
            let callee =
              {
                callee_index = index;
                callee_symbol = symbol;
                callee_definition = Function.definition_declaration body;
                callee_return_type = Function.return_type body;
                parameter_types =
                  Array.init parameter_count (fun position ->
                      context.slots.(position).stored_type);
                cleanup_opcode =
                  (if
                     Sema.Function_flag.caller_expects_callee_pop
                       ~stored_mask:(Function.stored_flags body)
                   then Opcode.Ic_add_rsp1
                   else Opcode.Ic_add_rsp);
                frame_bytes = context.allocated_bytes;
                variadic = Option.is_some context.variadic_location;
              }
            in
            summaries (index + 1) (symbol :: symbols) (function_id :: ids)
              ((callee, context, body) :: rev)
              rest
    in
    let* summaries = summaries 0 [] [] [] functions in
    let retained_functions =
      Option.fold ~none:[] ~some:(fun task -> task.functions) task
    in
    let calls ?caller graph =
      let runtime_owner =
        Option.fold ~none:Runtime.Entry
          ~some:(fun body -> Runtime.Function body)
          caller
      in
      let retained_calls =
        Graph.blocks graph
        |> List.concat_map (fun block ->
            Graph.instructions block |> Sequence.instructions
            |> List.filter_map (fun instruction ->
                let description = Sequence.description instruction in
                if description.opcode <> Opcode.Ic_call_start then None
                else
                  Option.bind runtime_calls (fun context ->
                      Option.bind
                        (Runtime.find_start context ~owner:runtime_owner
                           description.instruction_id) (fun site ->
                          Option.map
                            (fun link -> (Runtime.call_instruction site, link))
                            (Runtime.retained_function site)))))
      in
      Graph.blocks graph
      |> List.concat_map (fun block ->
          Graph.instructions block |> Sequence.instructions
          |> List.filter_map (fun instruction ->
              let description = Sequence.description instruction in
              match (description.opcode, description.payload) with
              | ( (Opcode.Ic_call | Ic_call_indirect2 | Ic_call_extern),
                  Some (Sequence.Symbol symbol) )
                when description.opcode = Opcode.Ic_call
                     || List.exists
                          (fun (callee, _, _) -> callee.callee_symbol == symbol)
                          summaries ->
                  let retained =
                    List.find_map
                      (fun (instruction_id, link) ->
                        if
                          Instruction_id.equal instruction_id
                            description.instruction_id
                        then Some link
                        else None)
                      retained_calls
                  in
                  Some
                    (Graph.block_id block, description, symbol, caller, retained)
              | _ -> None))
    in
    let rec available region declaring_index visited = function
      | [] -> Ok ()
      | (_, _, _, _, Some link) :: rest
        when List.exists
               (fun executable ->
                 Retained_function.same executable.function_link link)
               retained_functions ->
          available region declaring_index visited rest
      | (_, _, symbol, _, _) :: rest
        when List.exists (fun prior -> prior == symbol) visited ->
          available region declaring_index visited rest
      | (block_id, description, symbol, caller, _) :: rest -> (
          match
            List.find_opt
              (fun (callee, _, _) -> callee.callee_symbol == symbol)
              summaries
          with
          | Some (_, context, body)
            when Frame.function_item_index context.layout < declaring_index ->
              available region declaring_index (symbol :: visited)
                (calls ~caller:body (Function.body body) @ rest)
          | _ when description.opcode <> Opcode.Ic_call ->
              available region declaring_index visited rest
          | _ ->
              let error =
                preflight_error block_id description "HCIRVM0017"
                  "JIT static initializer calls a function whose definition is \
                   not yet published"
              in
              let error =
                Option.fold ~none:error
                  ~some:(fun body -> identify body error)
                  caller
              in
              Error [ identify_initializer (Some region) error ])
    in
    let* () =
      Option.fold ~none:[] ~some:Global_initialization.storage_regions
        initialization
      |> List.fold_left
           (fun result region ->
             let* () = result in
             match
               ( Global_initialization.storage_phase region,
                 Global_initialization.storage_frame region )
             with
             | Global_initialization.Compile_initializer, Some frame ->
                 let called =
                   calls (X87.graph checked)
                   |> List.filter (fun (_, description, _, _, _) ->
                       Instruction_id.compare
                         description.Sequence.instruction_id
                         (Global_initialization.storage_first region)
                       >= 0
                       && Instruction_id.compare description.instruction_id
                            (Global_initialization.storage_last region)
                          <= 0)
                 in
                 available region (Frame.function_item_index frame) [] called
             | _ -> Ok ())
           (Ok ())
    in
    let callees = List.map (fun (callee, _, _) -> callee) summaries in
    let literal_image = fresh_literal_image () in
    let max_literal_bytes =
      max_literal_bytes
      - Option.fold ~none:0 ~some:(fun task -> task.literal_bytes) accounting
    in
    let rec bodies rev = function
      | [] -> Ok (Array.of_list (List.rev rev))
      | (callee, frame, body) :: rest ->
          let* literals =
            collect_literals ~max_literal_bytes literal_image
              (Function.body body)
            |> Result.map_error (List.map (identify body))
          in
          let* program =
            prepare ~frame ?globals ~literals ~callees ?runtime_calls
              ~retained_functions ~runtime_owner:(Runtime.Function body)
              (Function.body body)
            |> Result.map_error (List.map (identify body))
          in
          bodies
            ((callee, { program with owner = Some (owner body) }) :: rev)
            rest
    in
    let* programs = bodies [] summaries in
    let identify_entry (error : error) =
      let region =
        Option.bind initialization (fun context ->
            Option.bind error.instruction_id (fun id ->
                match Instruction_id.of_int id with
                | Ok id -> Global_initialization.find_storage context id
                | Error _ -> None))
      in
      identify_initializer region error
    in
    let* literals =
      collect_literals ~max_literal_bytes literal_image (X87.graph checked)
      |> Result.map_error (List.map identify_entry)
    in
    let* entry =
      prepare ?globals ~literals ?initialization ~callees ?runtime_calls
        ~retained_functions (X87.graph checked)
      |> Result.map_error (List.map identify_entry)
    in
    let global_words =
      let cells =
        Array.make
          (Option.fold ~none:0 ~some:Integer_globals.cell_count globals)
          None
      in
      Option.fold ~none:[] ~some:Integer_globals.allocated_storage_slots globals
      |> List.iter (fun slot ->
          let initial =
            Option.map (storage_word slot)
              (Integer_globals.storage_initial_bits slot)
          in
          Array.fill cells
            (Integer_globals.storage_index slot)
            (Integer_globals.storage_element_count slot)
            initial;
          if Integer_globals.storage_opcode slot = Opcode.Ic_abs_addr then
            Integer_globals.storage_array_image slot
            |> List.iter (fun (cell_offset, payload) ->
                publish_array_payload ~slot ~cell_offset payload
                  (fun cell word -> cells.(cell) <- Some word)));
      cells
    in
    let retained_regions =
      Option.fold ~none:[] ~some:(fun task -> task.arenas) task
    in
    let admit =
      Option.map
        (fun task storage owner ->
          let globals = Option.get globals in
          task.started <- checked :: task.started;
          task.literal_arenas <- owner.owner_literals :: task.literal_arenas;
          if not initializer_mode then (
            task.arenas <- (globals, storage) :: task.arenas;
            let executable_publications =
              Integer_globals.function_publications globals
              |> List.filter_map (fun function_link ->
                  let declaration =
                    Retained_function.metadata function_link
                    |> Sema.Outer_environment.function_declaration
                  in
                  let site =
                    Sema.Function_resolution.resolved_declaration_site
                      declaration
                  in
                  if
                    Sema.Function_resolution.declaration_site_kind site
                    <> Sema.Function_resolution.Definition
                  then None
                  else
                    Array.to_list owner.owner_callees
                    |> List.find_opt (fun (callee, _) ->
                        match callee.callee_definition with
                        | Some definition -> definition == declaration
                        | None -> false)
                    |> Option.map (fun (function_callee, function_program) ->
                        {
                          function_link;
                          function_callee;
                          function_program;
                          function_owner = owner;
                          function_source =
                            {
                              source_globals = globals;
                              source_runtime_calls = Option.get runtime_calls;
                              source_functions = functions;
                              source_definition =
                                List.nth functions function_callee.callee_index;
                            };
                        }))
            in
            task.functions <- executable_publications @ task.functions;
            let publications =
              Integer_globals.publish_task task.catalog globals
            in
            let admission_publications =
              List.map
                (function
                  | Integer_globals.Global_publication (reference, slot) ->
                      Admitted_global (reference, slot)
                  | Integer_globals.Declared_publication (reference, slot) ->
                      Admitted_declared_global (reference, slot)
                  | Integer_globals.Function_publication reference ->
                      Admitted_function reference)
                publications
            in
            task.admissions <-
              {
                admission_catalog = task.catalog;
                admission_globals = globals;
                admission_entry = checked;
                admission_publications;
              }
              :: task.admissions))
        task
    in
    let admit =
      Option.map
        (fun account storage owner ->
          Option.iter (fun retain -> retain storage owner) admit;
          if Option.is_some isolated_budget then
            account.started <- checked :: account.started;
          account.global_bytes <-
            account.global_bytes
            + Option.fold ~none:0 ~some:Integer_globals.byte_size globals;
          account.literal_bytes <-
            account.literal_bytes + literal_image.literal_byte_count)
        accounting
    in
    let outcome =
      let stream_output =
        Option.bind task (fun task ->
            match task.streams with
            | active :: _ -> Some active.stream_output
            | [] -> None)
      in
      let generation_output =
        Option.map (fun task -> task.generated) accounting
      in
      let on_capture =
        Option.bind accounting (fun task ->
            if not initializer_mode then
              Some
                (fun value ->
                  if task.streams = [] then task.outer_value <- value;
                  Option.iter
                    (fun globals ->
                      let receipts =
                        Integer_globals.source_command_receipts globals
                      in
                      List.iter
                        (fun input ->
                          if
                            input.input_result = None
                            && List.exists
                                 (fun receipt ->
                                   receipt.Frontend.Parser.command_start
                                     .command_context == input.input_context)
                                 receipts
                          then input.input_value <- value)
                        task.inputs)
                    globals)
            else None)
      in
      execute_prepared ~callees:programs
        ~aot_linked:
          (Option.fold ~none:false
             ~some:(fun context ->
               Runtime.compilation_mode context = Sema.Function_resolution.Aot)
             runtime_calls)
        ~max_frame_bytes ~max_call_depth ?initialization ~global_words
        ~literal_image ~output ?stream_output ?generation_output
        ~capture_last:((not initializer_mode) || capture_fragment_value)
        ?on_capture ?admit ~retained_regions ~retained_functions ~max_steps
        entry
    in
    Option.iter
      (fun task ->
        let steps =
          match outcome with
          | Ok result -> result.executed_steps_
          | Error errors ->
              List.fold_left
                (fun count (error : error) -> max count error.executed_steps)
                0 errors
        in
        task.steps <- task.steps + steps)
      accounting;
    outcome

let execute_task_initializer task attempt execution =
  let module Program = Initializer_fragment_program in
  let module Destination = Initializer_fragment_destination in
  let ( let* ) = Result.bind in
  let destination = Program.execution_destination execution in
  let state = attempt.attempt_initializer in
  let slot = Destination.storage destination in
  let span = Destination.span destination in
  let invalid code message =
    Error [ make_error ~stage:Preflight ~span ~executed_steps:0 code message ]
  in
  let* () =
    if
      state.initializer_catalog != task.catalog
      || attempt.attempt_state <> Preparing_initializer
      || (not
            (Option.fold ~none:false ~some:(( == ) attempt)
               state.initializer_attempt))
      || (not
            (Frontend.Parser.initializer_leaf_is_current attempt.attempt_receipt
            || Sema.Source_activation.initializer_leaf task.source_activation
                 attempt.attempt_receipt))
      || Destination.layout destination != attempt.attempt_destination
      || Sema.Initializer_fragment.leaf (Destination.fragment destination)
         != attempt.attempt_leaf
      || Sema.Initializer_fragment.authorized_fragment
           (Program.execution_authority execution)
         != Destination.fragment destination
      || (not
            (Integer_globals.same_storage slot
               (Integer_globals.declared_storage state.initializer_slot)))
      || (not
            (Integer_globals.owns_task_storage task.catalog
               (Destination.globals destination)))
      || (not
            (Integer_globals.is_initializer_fragment
               (Destination.globals destination)))
      || Program.execution_steps execution
         <> task.initializer_steps - attempt.attempt_preparation_before
      || Integer_globals.byte_size (Destination.globals destination) <> 0
    then
      invalid "HCIRVM0026"
        "initializer execution has another attempt, source, destination or \
         preparation"
    else Ok ()
  in
  attempt.attempt_state <- Executing_initializer;
  let outcome =
    let* () =
      validate_dimension_dependencies (Some task)
        (Dimension_requirements.top_level (Destination.typed destination))
      |> Result.map_error (fun message ->
          [
            make_error ~stage:Preflight ~span ~executed_steps:0 "HCIRVM0026"
              message;
          ])
    in
    match Program.execution_code execution with
    | Program.Prepared payload ->
        let* storage =
          match
            List.find_map
              (fun (owner, storage) ->
                match
                  Integer_globals.find_allocated_storage owner
                    (Integer_globals.storage_symbol slot)
                with
                | Some expected when Integer_globals.same_storage expected slot
                  -> Some storage
                | _ -> None)
              task.arenas
          with
          | Some storage when storage.live -> Ok storage
          | _ ->
              invalid "HCIRVM0026"
                "initializer destination has no live retained storage"
        in
        publish_array_payload ~slot
          ~cell_offset:
            (Integer_initializer_layout.cell_offset attempt.attempt_destination)
          payload (fun cell word ->
            storage.cells.(cell) <- Some (Runtime_word word));
        Ok ()
    | Program.Scheduled program ->
        if task.steps >= task.max_steps then
          invalid "HCIRVM0007"
            "the task cumulative execution step limit was exhausted"
        else
          execute_program_with_output ~task ~initializer_mode:true
            ~runtime_calls:(Program.runtime_calls program)
            ~output:task.output
            ~globals:(Destination.globals destination)
            ~initialization:(Program.initialization program)
            ~max_global_bytes:task.max_global_bytes
            ~max_literal_bytes:task.max_literal_bytes
            ~max_steps:(task.max_steps - task.steps)
            ~max_frame_bytes:task.max_frame_bytes
            ~max_call_depth:task.max_call_depth ~functions:[]
            (Program.entry program)
          |> Result.map ignore
  in
  match outcome with
  | Error errors ->
      ignore (fail_task_initializer_attempt task attempt);
      Error errors
  | Ok () -> (
      match
        Integer_globals.record_declared_initializer state.initializer_slot
          attempt.attempt_destination
      with
      | Error message ->
          ignore (fail_task_initializer_attempt task attempt);
          invalid "HCIRVM0026" message
      | Ok () ->
          state.initializer_cursor <- attempt.attempt_next;
          attempt.attempt_state <- Successful_initializer;
          Ok ())

let execute_task_default task attempt execution =
  let module Program = Default_fragment_program in
  let module Destination = Default_fragment_destination in
  let ( let* ) = Result.bind in
  let destination = Program.execution_destination execution in
  let fragment = Destination.fragment destination in
  let span = Destination.span destination in
  let invalid message =
    Error
      [
        make_error ~stage:Preflight ~span ~executed_steps:0 "HCIRVM0026" message;
      ]
  in
  let* () =
    if
      attempt.default_catalog != task.catalog
      || (not (List.exists (( == ) attempt) task.defaults))
      || attempt.default_state <> Preparing_initializer
      || (not
            (Frontend.Parser.parameter_default_is_current
               attempt.default_receipt
            || Sema.Source_activation.parameter_default task.source_activation
                 attempt.default_receipt))
      || Sema.Default_fragment.receipt fragment != attempt.default_receipt
      || Sema.Default_fragment.publication fragment
         != attempt.default_publication
      || Sema.Default_fragment.authorized_fragment (Program.authority execution)
         != fragment
      || (not
            (Integer_globals.owns_task_storage task.catalog
               (Destination.globals destination)))
      || (not
            (Integer_globals.is_default_fragment
               (Destination.globals destination)))
      || Integer_globals.byte_size (Destination.globals destination) <> 0
      || Program.steps execution
         <> task.initializer_steps - attempt.default_preparation_before
    then
      invalid
        "default execution has another task, source attempt or preparation"
    else Ok ()
  in
  attempt.default_state <- Executing_initializer;
  let outcome =
    let* () =
      validate_dimension_dependencies (Some task)
        (Dimension_requirements.top_level (Destination.typed destination))
      |> Result.map_error (fun message ->
          [
            make_error ~stage:Preflight ~span ~executed_steps:0 "HCIRVM0026"
              message;
          ])
    in
    match Program.code execution with
    | Program.Prepared bits -> Ok bits
    | Program.Scheduled program -> (
        if task.steps >= task.max_steps then
          Error
            [
              make_error ~stage:Preflight ~span ~executed_steps:0 "HCIRVM0007"
                "the task cumulative execution step limit was exhausted";
            ]
        else
          let* result =
            execute_program_with_output ~task ~initializer_mode:true
              ~capture_fragment_value:true
              ~runtime_calls:(Program.runtime_calls program)
              ~output:task.output
              ~globals:(Destination.globals destination)
              ~initialization:(Program.initialization program)
              ~max_global_bytes:task.max_global_bytes
              ~max_literal_bytes:task.max_literal_bytes
              ~max_steps:(task.max_steps - task.steps)
              ~max_frame_bytes:task.max_frame_bytes
              ~max_call_depth:task.max_call_depth ~functions:[]
              (Program.entry program)
          in
          match result.final_value_ with
          | Some word -> Ok word.bits
          | None ->
              invalid "default evaluation produced no checked parameter value")
  in
  match outcome with
  | Error errors ->
      ignore (fail_task_default task attempt);
      Error errors
  | Ok bits ->
      attempt.default_bits <- Some bits;
      attempt.default_state <- Successful_initializer;
      Ok ()

let execute_task_dimension task attempt execution =
  let module Program = Dimension_fragment_program in
  let module Destination = Dimension_fragment_destination in
  let ( let* ) = Result.bind in
  let destination = Program.execution_destination execution in
  let fragment = Destination.fragment destination in
  let span = Destination.span destination in
  let invalid message =
    Error
      [
        make_error ~stage:Preflight ~span ~executed_steps:0 "HCIRVM0026" message;
      ]
  in
  let* () =
    if
      attempt.dimension_catalog != task.catalog
      || (not (List.exists (( == ) attempt) task.dimensions))
      || attempt.dimension_state <> Preparing_initializer
      || (not
            (Frontend.Parser.dimension_preparation_is_current
               attempt.dimension_receipt))
      || Program.authority execution != attempt.dimension_authority
      || Sema.Dimension_fragment.receipt fragment != attempt.dimension_receipt
      || Sema.Dimension_fragment.authorized_fragment
           (Program.authority execution)
         != fragment
      || (not
            (Integer_globals.owns_task_storage task.catalog
               (Destination.globals destination)))
      || (not
            (Integer_globals.is_dimension_fragment
               (Destination.globals destination)))
      || Integer_globals.byte_size (Destination.globals destination) <> 0
      || Program.steps execution
         <> task.initializer_steps - attempt.dimension_preparation_before
    then
      invalid
        "dimension execution has another task, source attempt or preparation"
    else Ok ()
  in
  attempt.dimension_state <- Executing_initializer;
  let outcome =
    let* () =
      validate_dimension_dependencies (Some task)
        (Dimension_requirements.top_level (Destination.typed destination))
      |> Result.map_error (fun message ->
          [
            make_error ~stage:Preflight ~span ~executed_steps:0 "HCIRVM0026"
              message;
          ])
    in
    match Program.code execution with
    | Program.Scheduled program -> (
        if task.steps >= task.max_steps then
          Error
            [
              make_error ~stage:Preflight ~span ~executed_steps:0 "HCIRVM0007"
                "the task cumulative execution step limit was exhausted";
            ]
        else
          let* result =
            execute_program_with_output ~task ~initializer_mode:true
              ~capture_fragment_value:true
              ~runtime_calls:(Program.runtime_calls program)
              ~output:task.output
              ~globals:(Destination.globals destination)
              ~initialization:(Program.initialization program)
              ~max_global_bytes:task.max_global_bytes
              ~max_literal_bytes:task.max_literal_bytes
              ~max_steps:(task.max_steps - task.steps)
              ~max_frame_bytes:task.max_frame_bytes
              ~max_call_depth:task.max_call_depth ~functions:[]
              (Program.entry program)
          in
          match result.final_value_ with
          | Some word -> Ok word.bits
          | None ->
              invalid "dimension evaluation produced no checked parameter value"
        )
  in
  match outcome with
  | Error errors ->
      ignore (fail_task_dimension task attempt);
      Error errors
  | Ok bits ->
      attempt.dimension_bits <- Some bits;
      attempt.dimension_work <-
        Some (task.initializer_steps - attempt.dimension_preparation_before);
      attempt.dimension_state <- Successful_initializer;
      Ok ()

let execute_task_program task ~runtime_calls ~globals ~initialization ~functions
    checked =
  task.source_promotion_open <- false;
  let result =
    if
      (not (source_dimensions_ready task))
      || not
           (List.for_all
              (Sema.Source_activation.command_admission task.source_activation)
              (Integer_globals.source_command_receipts globals))
    then
      Error
        [
          make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0026"
            "deferred source command is outside its original activation event";
        ]
    else if
      List.exists
        (fun state ->
          (not (initializer_is_idle state))
          && not
               (Integer_globals.declared_initializer_failed
                  state.initializer_slot))
        task.initializers
      || List.exists
           (fun attempt ->
             attempt.default_state = Preparing_initializer
             || attempt.default_state = Executing_initializer)
           task.defaults
      || List.exists
           (fun attempt ->
             attempt.dimension_state = Preparing_initializer
             || attempt.dimension_state = Executing_initializer)
           task.dimensions
    then
      Error
        [
          make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0026"
            "ordinary command cannot interleave an active initializer attempt";
        ]
    else if task.steps >= task.max_steps then
      Error
        [
          make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0007"
            "the task cumulative execution step limit was exhausted";
        ]
    else
      execute_program_with_output ~task ~runtime_calls ~output:task.output
        ~globals ~initialization ~max_global_bytes:task.max_global_bytes
        ~max_literal_bytes:task.max_literal_bytes
        ~max_steps:(task.max_steps - task.steps)
        ~max_frame_bytes:task.max_frame_bytes
        ~max_call_depth:task.max_call_depth ~functions checked
  in
  if Result.is_error result then (
    task.source_execution_failed <- true;
    task.failure_generation <- ref ());
  result

let execute_isolated_program_in_task task ~runtime_calls ~globals
    ~initialization ~functions checked =
  task.source_promotion_open <- false;
  let invalid code message =
    Error
      [ make_error ~stage:Preflight ~executed_steps:task.steps code message ]
  in
  let result =
    if
      not
        (List.exists
           (fun program ->
             matches_source_program program ~runtime_calls ~globals
               ~initialization ~functions checked)
           task.isolated_programs)
    then
      invalid "HCIRVM0026"
        "isolated output lacks its owning preparation and compiled bundle"
    else if task.streams <> [] then
      invalid "HCIRVM0027"
        "isolated output cannot execute inside an active stream"
    else if List.exists (fun entry -> entry == checked) task.started then
      invalid "HCIRVM0026"
        "isolated output has already started in this invocation"
    else if task.steps >= task.max_steps then
      invalid "HCIRVM0007" "the invocation execution step limit was exhausted"
    else
      let before = task.steps in
      execute_program_with_output ~isolated_budget:task ~runtime_calls
        ~output:task.output ~globals ~initialization
        ~max_global_bytes:task.max_global_bytes
        ~max_literal_bytes:task.max_literal_bytes
        ~max_steps:(task.max_steps - before)
        ~max_frame_bytes:task.max_frame_bytes
        ~max_call_depth:task.max_call_depth ~functions checked
      |> Result.map (fun result ->
          {
            result with
            executed_steps_ = task.steps;
            compiled_initializer_steps_ = task.initializer_steps;
          })
      |> Result.map_error
           (List.map (fun (error : error) ->
                { error with executed_steps = before + error.executed_steps }))
  in
  if Result.is_error result then (
    task.source_execution_failed <- true;
    task.failure_generation <- ref ());
  result

let execute_program_report ?runtime_calls ?globals ?initialization
    ?max_global_bytes ?max_literal_bytes ?(max_output_bytes = 1_048_576)
    ?(max_output_work = 1_048_576) ~max_steps ~max_frame_bytes ~max_call_depth
    ~functions checked =
  if
    max_output_bytes <= 0 || max_output_work <= 0
    || max_output_bytes > Sys.max_string_length
  then
    {
      outcome_ =
        Error
          [
            make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
              "output limits must be positive and max_output_bytes must fit a \
               host string";
          ];
      output_bytes_ = "";
      output_work_ = 0;
    }
  else
    let output = Output.create ~max_output_bytes ~max_output_work in
    let outcome_ =
      execute_program_with_output ?runtime_calls ~output ?globals
        ?initialization ?max_global_bytes ?max_literal_bytes ~max_steps
        ~max_frame_bytes ~max_call_depth ~functions checked
    in
    {
      outcome_;
      output_bytes_ = Output.contents output;
      output_work_ = Output.work output;
    }

let execute_program ?runtime_calls ?globals ?initialization ?max_global_bytes
    ?max_literal_bytes ?max_output_bytes ?max_output_work ~max_steps
    ~max_frame_bytes ~max_call_depth ~functions checked =
  execute_program_report ?runtime_calls ?globals ?initialization
    ?max_global_bytes ?max_literal_bytes ?max_output_bytes ?max_output_work
    ~max_steps ~max_frame_bytes ~max_call_depth ~functions checked
  |> report_outcome

let termination execution = execution.termination_
let executed_steps execution = execution.executed_steps_
let final_value execution = execution.final_value_
let compiled_initializer_steps execution = execution.compiled_initializer_steps_

let word_type_name = function
  | I64 -> "i64"
  | U64 -> "u64"

let termination_name = function
  | Stream_end -> "stream-end"
  | Returned None -> "returned:none"
  | Returned (Some word) ->
      Printf.sprintf "returned:%s:0x%016Lx"
        (word_type_name word.type_)
        word.bits

let human execution =
  Printf.sprintf
    "holyc-ir-integer-execution-v1 reference=%s\nsteps=%d\ntermination=%s\n"
    reference_commit execution.executed_steps_
    (termination_name execution.termination_)

let check_task_suspended_completion task ~suspension receipt =
  Integer_globals.check_suspended_completion task.catalog ~suspension receipt
