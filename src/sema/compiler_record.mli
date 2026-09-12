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

val begin_aggregate :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  (aggregate_progress, string) result

val aggregate_metadata : aggregate_progress -> (t, string) result
(** An immutable snapshot of reached layout, not storage or command authority.
    Advancing invalidates this snapshot for new reads, not consumed queries. *)

val advance_aggregate :
  dimensions:(Frontend.Ast.array_dimension -> declared_dimension option) ->
  aggregate_progress ->
  Frontend.Parser.aggregate_phase ->
  (unit, string) result

(** Apply each original live aggregate phase once, in predecessor order. *)

val complete_aggregate :
  ?progress:aggregate_progress ->
  ?dimensions:(Frontend.Ast.array_dimension -> declared_dimension option) ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  Frontend.Parser.completed_aggregate ->
  (t, string) result
(** Compute original completed aggregate metadata during its live callback. An
    arbitrary layout or a matching symbol cannot supply the size. *)

val bind_retained_scalar :
  table:Symbol_table.t ->
  entry:Frontend.Symbol_visibility.entry ->
  Global_type_resolution.global ->
  (t, string) result
(** Associate a newly published retained frontend entry with its original
    checked global. Call at publication, never recover an association by name.
*)

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
