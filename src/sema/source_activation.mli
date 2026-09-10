type event =
  | Command of Frontend.Parser.command_event
  | Declaration of Frontend.Parser.declaration_event
  | Reference of Frontend.Parser.reference_selection

type t

val create :
  namespace:Declaration_collection.namespace ->
  context:Frontend.Parser.command_context ->
  observed_events:int ->
  event list ->
  (t, string) result

val owns_namespace : t -> Declaration_collection.namespace -> bool
val available : t -> bool
val command_events : t -> Frontend.Parser.command_event list

val dimension_preparations :
  t -> Frontend.Parser.array_dimension_preparation list

val before_dimension : t -> Frontend.Parser.array_dimension_preparation -> bool

val dimension_preparing :
  t option -> Frontend.Parser.array_dimension_preparation -> bool

val dimension_completed :
  t option -> Frontend.Parser.completed_array_dimension -> bool

val run : t -> invalid:'e -> (event -> (unit, 'e) result) -> (unit, 'e) result
(** Consumes the complete original journal once. Only the current event has
    authority, and failure or exception permanently revokes the journal. *)

val initializer_start :
  t option -> Frontend.Parser.global_initializer_start -> bool

val initializer_leaf :
  t option -> Frontend.Parser.completed_initializer_leaf -> bool

val initializer_delimiter :
  t option -> Frontend.Parser.completed_initializer_delimiter -> bool

val parameter_default :
  t option -> Frontend.Parser.completed_parameter_default -> bool

val declaration : t option -> Frontend.Parser.declaration_event -> bool
val reference : t option -> Frontend.Parser.reference_selection -> bool
val finished : t option -> bool
val owns_context : t option -> Frontend.Parser.command_context -> bool
val global_admission : t option -> Frontend.Parser.global_publication -> bool
val command_admission : t option -> Frontend.Parser.completed_command -> bool

val default_completion :
  t option -> Frontend.Parser.completed_function_header -> bool

val initializer_completion :
  t option -> Frontend.Parser.global_initializer_start -> bool
(** Journaled runtime boundaries require their exact active event even after
    revocation. Boundaries first observed after activation keep their live path.
*)
