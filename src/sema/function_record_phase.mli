type registry
type t
type snapshot
type native_identity
type checked_call_shape
type transition
type call_start_snapshot
type call_emission_snapshot
type implicit_arguments_snapshot
type implicit_emission_snapshot

val capture_implicit_arguments :
  t ->
  Frontend.Parser.implicit_output_selection ->
  (implicit_arguments_snapshot, string) result

val capture_implicit_emission :
  implicit_arguments_snapshot ->
  Frontend.Parser.implicit_output_selection ->
  (implicit_emission_snapshot, string) result

val implicit_arguments_receipt :
  implicit_arguments_snapshot -> Frontend.Parser.implicit_output_selection

val implicit_argument_snapshot : implicit_arguments_snapshot -> snapshot

val implicit_emission_arguments :
  implicit_emission_snapshot -> implicit_arguments_snapshot

val implicit_emitted_snapshot : implicit_emission_snapshot -> snapshot

val capture_call_start :
  t -> Frontend.Parser.call_start -> (call_start_snapshot, string) result

val capture_call_emission :
  call_start_snapshot ->
  Frontend.Parser.completed_call ->
  (call_emission_snapshot, string) result

val call_start_receipt : call_start_snapshot -> Frontend.Parser.call_start
val call_argument_snapshot : call_start_snapshot -> snapshot

val call_emission_receipt :
  call_emission_snapshot -> Frontend.Parser.completed_call

val call_emission_snapshot : call_emission_snapshot -> snapshot

val call_emission_arguments : call_emission_snapshot -> call_start_snapshot
(** Immutable native snapshots captured only at the original live call events.
    Replaying these receipts cannot resample a later mutable record. These are
    source evidence; the owning VM independently admits runtime authority. *)

val transition :
  earlier:snapshot -> later:snapshot -> (transition, string) result

val transition_earlier : transition -> snapshot

val transition_later : transition -> snapshot
(** Strict ancestry of successful native events on the same allocation. Reads,
    rejected events, equal-phase source views and backwards changes grant none.
*)

val create_registry :
  mode:Frontend.Preprocessor.compilation_mode ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  (registry, string) result
(** JIT same-table records only. AOT cannot use this registry. *)

val begin_header :
  ?activation:Source_activation.t ->
  registry ->
  Declaration_collection.publication ->
  Frontend.Parser.function_publication ->
  (t, string) result
(** Exact live publication and previous parser-entry lineage are checked before
    resetting a shared extern record. An untracked predecessor retains explicit
    unavailable native evidence; it never grants assumed fresh counts. Explicit
    frontend function aliases retain exact ancestry to their registered source
    entries; an alias of a known nonlatest source still rejects. *)

val event_belongs : t -> Frontend.Parser.declaration_event -> bool

val observe :
  ?activation:Source_activation.t ->
  t ->
  Frontend.Parser.declaration_event ->
  (unit, string) result

val snapshot : t -> snapshot

val matches_event : snapshot -> Frontend.Parser.declaration_event -> bool
(** Retains the exact successful declaration, member, default, variadic or
    header event and its native revision. A later read after shared-record
    mutation cannot authenticate an earlier source event. Original snapshots
    remain immutable; this proves neither live admission nor body/execution
    authority. *)

val source_snapshot : snapshot -> Provisional_function.snapshot
val source : snapshot -> Frontend.Parser.function_publication
val publication : snapshot -> Declaration_collection.publication
val owns_table : snapshot -> Symbol_table.t -> bool
val owns_namespace : snapshot -> Declaration_collection.namespace -> bool
val native_identity : snapshot -> native_identity
val same_identity : snapshot -> snapshot -> bool
val same_revision : snapshot -> snapshot -> bool

val same_cursor : snapshot -> snapshot -> bool
(** Exact native member slots, owner, active count and variadic flag; completed
    body bookkeeping may change without changing the argument cursor. *)

val native_source : snapshot -> Frontend.Parser.function_publication
(** Latest header installed on the shared record; distinct from this snapshot's
    immutable per-publication source transcript after nested reuse. *)

val native_members : snapshot -> Provisional_function.member list
(** Original concrete member sources in the current native cursor, including
    members beyond the active argument count. These can belong to nested headers
    rather than this snapshot's per-publication source transcript. *)

val argument_count : snapshot -> int option
val member_count : snapshot -> int option
val saved_previous_argument_count : snapshot -> int option
val ellipsis_flag : snapshot -> bool
val is_extern : snapshot -> bool option
val unavailable_reason : snapshot -> string option
val call_shape : snapshot -> (checked_call_shape, string) result
val shape_snapshot : checked_call_shape -> snapshot
val fixed_members : checked_call_shape -> Provisional_function.member list

val variadic_tail :
  checked_call_shape -> Frontend.Parser.function_variadic_publication option
(** Traverses the actual native member cursor for the retained native arg_cnt,
    then tests the remaining member for the ellipsis tail. A function flag does
    not itself establish variadic calls. Unknown count or invalid member-pointer
    evidence stays unavailable. The full immutable snapshot remains attached.

    This component tracks header member/count state, extern identity and the
    sticky ellipsis flag. It does not prove evaluated defaults, other native
    flags, executable address, return type, ABI layout or body-local state
    during body parsing. Those require the caller's independently checked
    lifecycle and typed metadata, including native member type validity.
    Duplicate member insertion checks use the actual retained native slots,
    including argc/argv and the native pad/reserved/_anon_ exemptions. A
    rejected insertion grants neither member nor argument count authority.
    Completed bodies with unmodeled member mutations make the member count
    unavailable before a suspended header can use it. Bound/import headers
    require separate executable installation evidence and become unavailable at
    header completion; they cannot establish extern reuse. *)
