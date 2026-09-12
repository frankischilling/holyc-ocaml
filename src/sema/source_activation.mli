type event = Frontend.Parser.source_observation =
  | Command of Frontend.Parser.command_event
  | Declaration of Frontend.Parser.declaration_event
  | Reference of Frontend.Parser.reference_selection
  | Call_start of Frontend.Parser.call_start
  | Call_emission of Frontend.Parser.completed_call
  | Implicit_output of Frontend.Parser.implicit_output_selection
  | Implicit_arguments of Frontend.Parser.implicit_output_selection
  | Implicit_emission of Frontend.Parser.implicit_output_selection

type t
type call_journal

val create_call_journal :
  namespace:Declaration_collection.namespace -> unit -> call_journal

val capture_call_start :
  call_journal ->
  events_rev:event list ->
  Frontend.Parser.call_start ->
  (event, string) result

val capture_call_emission :
  call_journal ->
  events_rev:event list ->
  Frontend.Parser.completed_call ->
  (event, string) result

val call_start : t option -> Frontend.Parser.call_start -> bool
val call_emission : t option -> Frontend.Parser.completed_call -> bool
val call_start_admission : t option -> Frontend.Parser.call_start -> bool

val reference_admission :
  t option -> Frontend.Parser.reference_selection -> bool

val call_binding_available :
  t option -> Frontend.Parser.completed_call -> committed:bool -> bool

val call_emission_admission : t option -> Frontend.Parser.completed_call -> bool

val function_declaration :
  t option -> Frontend.Parser.function_publication -> bool

val function_header :
  t option -> Frontend.Parser.completed_function_header -> bool
(** Only the exact currently active header event may create header evidence. An
    absent event, an inactive journal and a consumed journal grant no authority.
*)

val implicit_output :
  t option -> Frontend.Parser.implicit_output_selection -> bool

val create :
  ?calls:call_journal ->
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

val trailing_dimension_preparation :
  t -> Frontend.Parser.array_dimension_preparation option
(** The exact final observation, only if it is a dimension preparation. This
    structural query grants no evaluation or activation authority. *)

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

val dimension_admission :
  t option -> Frontend.Parser.array_dimension_preparation -> bool
(** Journaled preparations require their exact active event, including after
    replay fails. Later unjournaled preparations retain the normal live path. *)

val function_phase_admission :
  t option -> Frontend.Parser.declaration_event -> bool
(** Journaled declaration/member/default/variadic/header phases require the
    exact active original receipt, even after failure or exception revokes
    replay. Unjournaled phases remain eligible for the caller's live checks.
    Other event kinds, including body completion, grant no authority through
    this API. *)

val command_admission : t option -> Frontend.Parser.completed_command -> bool

val default_completion :
  t option -> Frontend.Parser.completed_function_header -> bool

val initializer_completion :
  t option -> Frontend.Parser.global_initializer_start -> bool
(** Journaled runtime boundaries require their exact active event even after
    revocation. Boundaries first observed after activation keep their live path.
*)

val capture_implicit :
  call_journal ->
  events_rev:event list ->
  Frontend.Parser.implicit_output_selection ->
  emission:bool ->
  (event, string) result

val implicit_arguments :
  t option -> Frontend.Parser.implicit_output_selection -> bool

val implicit_emission :
  t option -> Frontend.Parser.implicit_output_selection -> bool

val implicit_selection_admission :
  t option -> Frontend.Parser.implicit_output_selection -> bool

val implicit_arguments_admission :
  t option -> Frontend.Parser.implicit_output_selection -> bool

val implicit_emission_admission :
  t option -> Frontend.Parser.implicit_output_selection -> bool

val implicit_binding_available :
  t option ->
  Frontend.Parser.implicit_output_selection ->
  committed:bool ->
  bool
