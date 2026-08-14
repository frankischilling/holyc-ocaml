type resolution = Global_binding_environment.resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type event
type dimension_input
type global_input
type occurrence
type resolved_dimension
type resolved_global
type t

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      global_symbol : Symbol.t;
      dimension_index : int;
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error

val make_identifier :
  name:string ->
  origin:Symbol.origin ->
  occurrence_index:int ->
  dimension_index:int ->
  (event, string) result

val make_dimension :
  dimension:Global_type_resolution.array_dimension ->
  event list ->
  (dimension_input, string) result

val make_global :
  record:Global_resolution.global_record ->
  dimension_input list ->
  (global_input, string) result

val resolve :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  expressions:Module_expression_binding.t ->
  globals:Global_resolution.t ->
  global_input list ->
  (t, error) result
(** Bind ordinary identifiers in global array extents before publishing the
    owning global. Earlier records are visible; the current and later records
    fall through to the complete outer table chain. *)

val globals : t -> resolved_global list
val environment : t -> Outer_environment.t
val expressions : t -> Module_expression_binding.t
val source_globals : t -> Global_resolution.t
val owns_table : t -> Symbol_table.t -> bool
val find_global : t -> Symbol.t -> resolved_global option
val global_record : resolved_global -> Global_resolution.global_record

val global_publication :
  resolved_global -> Module_expression_binding.publication

val global_symbol : resolved_global -> Symbol.t
val global_item_index : resolved_global -> int
val global_declarator_index : resolved_global -> int option
val global_dimensions : resolved_global -> resolved_dimension list

val dimension_source :
  resolved_dimension -> Global_type_resolution.array_dimension

val dimension_index : resolved_dimension -> int
val dimension_origin : resolved_dimension -> Symbol.origin
val dimension_opening_origin : resolved_dimension -> Symbol.origin
val dimension_expression_origin : resolved_dimension -> Symbol.origin option
val dimension_closing_origin : resolved_dimension -> Symbol.origin
val dimension_occurrences : resolved_dimension -> occurrence list
val occurrence_index : occurrence -> int
val occurrence_dimension_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
