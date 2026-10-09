type runtime_dimension_proposal

val propose_runtime_dimension :
  namespace:Declaration_collection.namespace ->
  preparation:Frontend.Parser.array_dimension_preparation ->
  count:int64 ->
  work:int ->
  (runtime_dimension_proposal, string) result
(** An unverified proposed shape. Only the owning VM can establish that its
    original expression executed with this result. *)

val runtime_dimension_namespace :
  runtime_dimension_proposal -> Declaration_collection.namespace

val runtime_dimension_source :
  runtime_dimension_proposal -> Frontend.Parser.array_dimension_preparation

val runtime_dimension_count : runtime_dimension_proposal -> int64
val runtime_dimension_work : runtime_dimension_proposal -> int

type t
type declared_dimension
type declared_global
type declared_function

val declare_function :
  ?activation:Source_activation.t ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  Frontend.Parser.completed_function_header ->
  (declared_function, string) result
(** Preserve the exact completed header at its original callback or active
    source-journal event. This is source authority for header typing; it does
    not admit a runtime binding, executable, frame or completed command. *)

val declared_function_source :
  declared_function -> Frontend.Parser.completed_function_header

val declared_function_symbol : declared_function -> Symbol.t
val declared_function_owns_table : declared_function -> Symbol_table.t -> bool

val declared_function_owns_namespace :
  declared_function -> Declaration_collection.namespace -> bool

val declare_global :
  ?callback:
    Frontend.Parser.completed_callback_signature
    * Function_type_resolution.function_pointer ->
  ?selected_aggregate:Function_type_resolution.selected_aggregate_resolver ->
  dimensions:declared_dimension list ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  predecessor:Frontend.Parser.completed_command option ->
  previous_global:Symbol.t option ->
  Declaration_collection.publication ->
  (declared_global, string) result

val declared_global_symbol : declared_global -> Symbol.t

val declared_global_source :
  declared_global -> Frontend.Parser.global_publication

val declared_global_type : declared_global -> Type_reference.t

val declared_global_callback_pointer :
  declared_global -> Function_type_resolution.function_pointer option
(** Retain the checked original anonymous header while its global initializer is
    still being parsed. The header does not supply executable identity. *)

val declared_global_storage_type : declared_global -> (Type.t, string) result
(** Preserve physical [RT_PTR] storage separately from callback return metadata.
*)

val declared_global_dimensions : declared_global -> int64 list

val declared_global_runtime_dependencies :
  declared_global -> runtime_dimension_proposal list

val declared_global_owns_table : declared_global -> Symbol_table.t -> bool

val declared_global_owns_namespace :
  declared_global -> Declaration_collection.namespace -> bool

val declared_global_predecessor :
  declared_global -> Frontend.Parser.completed_command option

val declared_global_previous_global : declared_global -> Symbol.t option

val complete_declared_global :
  declared_global -> Frontend.Parser.declaration_event -> (unit, string) result

val declared_global_completion :
  declared_global -> Frontend.Ast.global_declarator option

val validate_declared_global_type :
  declared_global -> Global_type_resolution.global -> (unit, string) result

type sizeof_read
type dimension_preparation

val declared_dimension_preparation : declared_dimension -> dimension_preparation

val dimension_preparation_runtime_dependencies :
  dimension_preparation -> runtime_dimension_proposal list

val seed_primitive :
  table:Symbol_table.t ->
  entry:Frontend.Symbol_visibility.entry ->
  symbol:Symbol.t ->
  primitive:Primitive_type.t ->
  (t, string) result
(** Establish the frontend/semantic association at primitive seeding. *)

val seed_public_union :
  table:Symbol_table.t ->
  entry:Frontend.Symbol_visibility.entry ->
  symbol:Symbol.t ->
  source:Generated.Primitive_raw_types.public_union ->
  (t, string) result
(** Establish a public class record from its exact generated union declaration
    and checked primitive backing, preserving the pinned declaration origin. *)

val rebind_primitive :
  table:Symbol_table.t -> symbol:Symbol.t -> t -> (t, string) result
(** Bind the known seeded record to a fork's fresh symbol without another
    spelling lookup. *)

val published_scalar :
  ?dimensions:declared_dimension list ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  (t, string) result
(** Read original published type children, independently of storage admission.
    Arrays consume their original ordered checked dimensions. Aggregate layouts
    and function-pointer signatures still need separate checked metadata. *)

type aggregate_progress
type inherited_base
type inherited_metadata
type aggregate_offset
type runtime_aggregate_offset
type compiler_position
type compiler_positions
type static_allocation

val create_compiler_positions :
  sources:Common.Source_manager.t -> compiler_positions
(** Original shared compiler-cell writes for one source manager. Outer AOT and
    nested JIT ledgers share this registry while retaining separate namespaces.
    Entries require live original aggregate or named JIT header/local phases;
    numeric values cannot be supplied by callers. Runtime-sized primitive frames
    retain their original dimension dependencies. Aggregate frames, unnamed
    callback and ordinary AOT record writes remain unavailable. *)

val compiler_positions_own_sources :
  compiler_positions -> Common.Source_manager.t -> bool

val compiler_position_value : compiler_position -> int64
val compiler_position_dependencies : compiler_position -> aggregate_offset list

val compiler_position_runtime_dependencies :
  compiler_position -> runtime_dimension_proposal list

val record_local_allocation :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  dimensions:declared_dimension list ->
  compiler_positions ->
  Function_record_phase.t ->
  Frontend.Parser.function_local_allocation ->
  (unit, string) result
(** Consume an original live local allocation after checking source manager,
    table, namespace, function and predecessor. Checked dimensions retain their
    original owner, order and runtime dependencies. Unsupported layouts are
    recorded as unavailable; neither a byte size nor executable authority can be
    supplied by a caller. *)

val static_allocation :
  compiler_positions ->
  Frontend.Parser.function_local_allocation ->
  static_allocation option
(** Read the original static allocation retained by a successful live
    [record_local_allocation]. Automatic locals, copied receipts and failed
    observations have no witness. A remembered witness describes source
    ownership; it grants no storage or initializer execution authority. *)

val static_allocation_owns_table : static_allocation -> Symbol_table.t -> bool
val static_allocation_table : static_allocation -> Symbol_table.t

val static_allocation_namespace :
  static_allocation -> Declaration_collection.namespace

val static_allocation_publication :
  static_allocation -> Declaration_collection.publication

val static_allocation_receipt :
  static_allocation -> Frontend.Parser.function_local_allocation

val static_allocation_dimensions : static_allocation -> declared_dimension list

val record_function_position :
  compiler_positions ->
  Function_record_phase.t ->
  Frontend.Parser.function_position_write ->
  (unit, string) result
(** Capture a named JIT function's original native header or local position. The
    record supplies the value internally; this does not grant executable
    authority. *)

val aggregate_offset_namespace :
  aggregate_offset -> Declaration_collection.namespace

val aggregate_offset_table : aggregate_offset -> Symbol_table.t
val aggregate_offset_phase : aggregate_offset -> Frontend.Parser.aggregate_phase
val aggregate_offset_expression : aggregate_offset -> Frontend.Ast.expression
val aggregate_offset_value : aggregate_offset -> int64
val aggregate_offset_work : aggregate_offset -> int

val begin_aggregate :
  ?compiler_positions:compiler_positions ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  (aggregate_progress, string) result

val aggregate_metadata : aggregate_progress -> (t, string) result
(** An immutable snapshot of reached layout, not storage or command authority.
    Advancing invalidates this snapshot for new reads, not consumed queries. *)

val select_aggregate_base :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  selected_publication:Declaration_collection.publication ->
  Frontend.Parser.aggregate_phase ->
  t ->
  (inherited_base, string) result
(** Read the current original layout of the exact class entry selected before
    lookahead, during its live base attachment. Only completion of that entry's
    original forward identity may supply a newer publication. The opaque proof
    preserves runtime dependencies and grants no storage or executable
    authority. *)

val advance_aggregate :
  ?callbacks:
    (Frontend.Ast.function_pointer_declarator ->
    Frontend.Parser.completed_callback_signature option) ->
  ?bases:(Frontend.Parser.aggregate_phase -> (inherited_base, string) result) ->
  dimensions:(Frontend.Ast.array_dimension -> declared_dimension option) ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  (unit, string) result

(** Apply each original live aggregate phase once, in predecessor order. *)

val complete_aggregate :
  ?callbacks:
    (Frontend.Ast.function_pointer_declarator ->
    Frontend.Parser.completed_callback_signature option) ->
  ?progress:aggregate_progress ->
  ?dimensions:(Frontend.Ast.array_dimension -> declared_dimension option) ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  Frontend.Parser.completed_aggregate ->
  (t, string) result
(** Compute original completed aggregate metadata during its live callback. An
    arbitrary layout or a matching symbol cannot supply the size. *)

val retain_inherited_metadata :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Frontend.Ast.aggregate_definition ->
  t ->
  (inherited_metadata, string) result
(** Preserve a completed original inherited layout for metadata queries. This
    does not admit an aggregate object or a member index. *)

val inherited_metadata_owns_definition :
  table:Symbol_table.t ->
  scope:Symbol_table.scope ->
  Frontend.Ast.aggregate_definition ->
  inherited_metadata ->
  bool

val bind_retained_scalar :
  table:Symbol_table.t ->
  entry:Frontend.Symbol_visibility.entry ->
  Global_type_resolution.global ->
  (t, string) result
(** Associate a newly published retained frontend entry with its original
    checked global. Call at publication, never recover an association by name.
*)

val return_class_size :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  type_:Type.t ->
  aggregate:t option ->
  (int64, string) result
(** Read the checked return class size. Aggregate identity, table, namespace,
    current canonical publication and layout stamp must match. An unavailable
    layout is not size zero. Pointers use the audited pointer size. *)

val read_sizeof :
  table:Symbol_table.t ->
  root:Frontend.Parser.query_root ->
  t ->
  (sizeof_read, string) result

val complete_sizeof :
  table:Symbol_table.t ->
  receipt:Frontend.Parser.completed_query ->
  sizeof_read ->
  (unit, string) result

val read_local_sizeof :
  dimensions:declared_dimension list ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  function_publication:Declaration_collection.publication ->
  root:Frontend.Parser.query_root ->
  (sizeof_read, string) result

val sizeof_value : sizeof_read -> pointer:bool -> int64
val sizeof_primitive : sizeof_read -> Primitive_type.t option
val sizeof_is_internal : sizeof_read -> bool

type query_role = Query_source.role =
  | Sizeof_root
  | Offset_root
  | Defined_operand

type query_read

val aggregate_offset_is_current :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  bool

val prepare_aggregate_offset :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  max_work:int ->
  queries:query_read list ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  (aggregate_offset, string) result * int
(** Evaluate once at the original live offset phase. Failed attempts also
    consume the boundary. Work counts evaluated numeric nodes, including failure
    work. *)

val complete_query :
  ?sizeof_read:sizeof_read ->
  table:Symbol_table.t ->
  receipt:Frontend.Parser.completed_query ->
  unit ->
  (query_read, string) result
(** Retain a parser-completed query for semantic event validation. The source
    driver must first establish its original command seal and table ownership.
    This is query-read evidence, not storage, executable or layout authority. *)

val validate_query :
  table:Symbol_table.t ->
  role:query_role ->
  name:string ->
  origin:Symbol.origin ->
  query_read ->
  (unit, string) result

val query_expression : query_read -> Frontend.Ast.expression
val query_owns_table : query_read -> Symbol_table.t -> bool
val query_is_local : query_read -> bool
val query_presence : query_read -> bool option
val query_sizeof : query_read -> (Primitive_type.t option * int64 * bool) option
val query_constant : query_read -> int64 option

val validate_query_manifest :
  table:Symbol_table.t ->
  expression:Frontend.Ast.expression ->
  query_read list ->
  (unit, string) result

val prepare_dimension :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  max_work:int ->
  preparation:Frontend.Parser.array_dimension_preparation ->
  queries:query_read list ->
  (dimension_preparation, string) result * int
(** Evaluate the original closed expression once with an enforced allowance. The
    second result retains numeric visits even on failure. The owning driver must
    consume the attempt before calling and charge the reached work. *)

val complete_dimension :
  receipt:Frontend.Parser.completed_array_dimension ->
  dimension_preparation ->
  (declared_dimension, string) result

val prepared_dimension_count : dimension_preparation -> int64

val dimension_preparation_source :
  dimension_preparation -> Frontend.Parser.array_dimension_preparation

val dimension_preparation_work : dimension_preparation -> int

val dimension_preparation_namespace :
  dimension_preparation -> Declaration_collection.namespace

val dimension_count : declared_dimension -> int64
val dimension_work : declared_dimension -> int

val dimension_receipt :
  declared_dimension -> Frontend.Parser.completed_array_dimension

val validate_dimension :
  table:Symbol_table.t ->
  dimension:Frontend.Ast.array_dimension ->
  declared_dimension ->
  (unit, string) result

val validate_dimension_queries :
  declared_dimension -> query_read list -> (unit, string) result

type global_dimension_extent
type global_extent

val reuse_global_dimension :
  table:Symbol_table.t ->
  record:Global_resolution.global_record ->
  dimension:Global_type_resolution.array_dimension ->
  queries:query_read list ->
  declared_dimension ->
  (global_dimension_extent, string) result
(** Reuse the original parser preparation only for the original publication's
    complete ordered source dimensions and exact query reads. The result binds
    this dimension value to the supplied physical semantic record. *)

val evaluate_global_dimension :
  table:Symbol_table.t ->
  record:Global_resolution.global_record ->
  dimension:Global_type_resolution.array_dimension ->
  queries:query_read list option ->
  (global_dimension_extent, string) result
(** Evaluate the supplied record's original typed dimension internally. This
    legacy layout path remains unmetered and does not create parser evidence. *)

val make_global_extent :
  table:Symbol_table.t ->
  record:Global_resolution.global_record ->
  global_dimension_extent list ->
  (global_extent, string) result
(** Require every dimension, in original order, with positive counts and a
    nonoverflowing product. Layout evidence does not grant runtime admission. *)

val global_extent_record : global_extent -> Global_resolution.global_record
val global_dimension_extent_count : global_dimension_extent -> int64
val global_extent_dimensions : global_extent -> int64 list
val global_extent_element_count : global_extent -> int64

val validate_global_extent :
  table:Symbol_table.t ->
  record:Global_resolution.global_record ->
  global_extent ->
  (unit, string) result

val bind_retained_global :
  table:Symbol_table.t ->
  entry:Frontend.Symbol_visibility.entry ->
  record:Global_resolution.global_record ->
  extent:global_extent option ->
  (t, string) result
(** Associate a newly admitted frontend entry with its original record and
    checked extent. Declared byte size is independent of padded VM storage. *)

val complete_runtime_dimension :
  table:Symbol_table.t ->
  receipt:Frontend.Parser.completed_array_dimension ->
  queries:query_read list ->
  runtime_dimension_proposal ->
  (declared_dimension, string) result
(** Check original parser source associations while retaining the proposal as an
    unverified runtime dependency. This does not establish VM execution or
    authorize storage; consumers must retain and validate the dependencies. *)

val dimension_runtime_dependencies :
  declared_dimension -> runtime_dimension_proposal list

val query_runtime_dependencies : query_read -> runtime_dimension_proposal list

val global_extent_runtime_dependencies :
  global_extent -> runtime_dimension_proposal list

val aggregate_offset_is_runtime : aggregate_offset -> bool
(** Includes closed offsets derived from runtime dimensions or runtime offsets;
    these cannot be charged as independent closed-source preparation. *)

val begin_runtime_aggregate_offset :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  queries:query_read list ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  (runtime_aggregate_offset, string) result

val runtime_aggregate_offset_is_current : runtime_aggregate_offset -> bool

val runtime_aggregate_offset_dimension_dependencies :
  runtime_aggregate_offset -> runtime_dimension_proposal list

val runtime_aggregate_offset_dependencies :
  runtime_aggregate_offset -> aggregate_offset list

val aggregate_offset_positions :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  ((Frontend.Ast.expression * compiler_position) list, string) result
(** Resolve original token reads against immutable shared compiler-state writes.
    These facts retain layout dependencies but grant no execution authority. *)

val aggregate_offset_dimension_dependencies :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  queries:query_read list ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  (runtime_dimension_proposal list, string) result
(** Runtime dimension requirements of the preceding layout and original position
    and query inputs. The owning task must validate these before evaluation. *)

val aggregate_offset_dependencies :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  queries:query_read list ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  (aggregate_offset list, string) result
(** Original offset executions required by the preceding layout and the
    expression's query and position inputs, including inherited requirements
    when the expression itself does not read the preceding layout. *)

val finish_runtime_aggregate_offset :
  runtime_aggregate_offset ->
  value:int64 ->
  work:int ->
  (aggregate_offset, string) result
(** This produces semantic layout metadata, not task execution authority.
    Runtime admission must independently match the original successful typed
    execution and its cumulative preparation work. Closed-source charging must
    reject runtime metadata. *)

val query_runtime_offsets : query_read -> aggregate_offset list
val dimension_offset_dependencies : declared_dimension -> aggregate_offset list

val dimension_preparation_offset_dependencies :
  dimension_preparation -> aggregate_offset list

val declared_global_offset_dependencies :
  declared_global -> aggregate_offset list

val global_extent_offset_dependencies : global_extent -> aggregate_offset list

val record_callback_position :
  compiler_positions ->
  parameters:Frontend.Parser.completed_callback_parameter list ->
  Frontend.Parser.callback_position_write ->
  (unit, string) result

val resolve_default_position_reads :
  compiler_positions ->
  sources:Common.Source_manager.t ->
  (Frontend.Ast.expression * Frontend.Parser.compiler_position_source option)
  list ->
  ((Frontend.Ast.expression * compiler_position) list, string) result
