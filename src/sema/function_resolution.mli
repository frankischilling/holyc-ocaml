type compilation_mode = Jit | Aot
type declaration_kind = Extern | Bound_extern | Import | Intern | Definition
type state = Unresolved_extern | Imported | Resolved
type phase = Legacy | Provisional | Completed_header | Completed_body
type declaration
type declaration_site
type identity
type resolved_declaration
type t

val make_provisional_declaration :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  compiler_option_mask:int64 ->
  function_:Function_type_resolution.resolved_function ->
  (declaration, string) result

val make_provisional_advance :
  ?pending:resolved_declaration ->
  compiler_option_mask:int64 ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  current:resolved_declaration ->
  transition:Function_record_phase.transition ->
  function_:Function_type_resolution.resolved_function ->
  unit ->
  (declaration, string) result
(** Advance a current native record. [pending] identifies a repeated source
    phase; omitting it requires a new source publication and retains its own
    options and scope. Successful resolution consumes the current head once. *)

val make_header_advance :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  pending:resolved_declaration ->
  current:resolved_declaration ->
  transition:Function_record_phase.transition ->
  source:Compiler_record.declared_function ->
  function_:Function_type_resolution.resolved_function ->
  callable_function:Function_type_resolution.resolved_function ->
  (declaration, string) result
(** Finish an exact provisional source with its ordinary typed header. Calls
    retain the independently checked current native projection, including nested
    replacement members. This does not complete the body. *)

val declaration_site_phase : declaration_site -> phase

val declaration_site_native_snapshot :
  declaration_site -> Function_record_phase.snapshot option

val resolved_declaration_phase_source :
  resolved_declaration -> resolved_declaration option

val resolved_declaration_phase_current :
  resolved_declaration -> resolved_declaration option

val make_declaration :
  function_:Function_type_resolution.resolved_function ->
  kind:declaration_kind ->
  (declaration, string) result
(** Describe one checked function header with the initial compiler options. *)

val make_declaration_with_options :
  compiler_option_mask:int64 ->
  function_:Function_type_resolution.resolved_function ->
  kind:declaration_kind ->
  (declaration, string) result
(** Describe one checked function header without changing the symbol table. The
    option snapshot converts extern forms to their effective import kind while
    retaining their source kind. *)

val make_pending_declaration :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  compiler_option_mask:int64 ->
  source:Compiler_record.declared_function ->
  function_:Function_type_resolution.resolved_function ->
  (declaration, string) result
(** Preserve the original completed source header and its checked parameter
    children. Binding kind is derived from source; pending state is independent
    of that kind and does not publish an executable. *)

val make_completion_declaration :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  pending:resolved_declaration ->
  function_:Function_type_resolution.resolved_function ->
  (declaration, string) result
(** Complete the exact retained typed header against its unchanged pending
    record. Successful resolution consumes completion once. Executable
    publication still requires independent body evidence. *)

val make_completion_declaration_against :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  pending:resolved_declaration ->
  current:resolved_declaration ->
  function_:Function_type_resolution.resolved_function ->
  (declaration, string) result
(** Complete [pending]'s original source against [current], which must be that
    exact pending declaration or an exact joined successor in the same record.
    The original body source stays distinct from the current header. [resolve]
    requires [current] in its visible predecessors or completion record heads.
*)

val complete_pending :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  pending:resolved_declaration ->
  function_:Function_type_resolution.resolved_function ->
  (t, string) result
(** Resolve one explicit completion against its original pending predecessor. *)

val resolve :
  ?previous:resolved_declaration list ->
  ?record_heads:resolved_declaration list ->
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  compilation_mode:compilation_mode ->
  declaration list ->
  (t, string) result
(** Reconcile function identities in source order. JIT joins only the newest
    unresolved extern; AOT joins the newest identity unless it is imported.
    [previous] supplies at most one newest JIT declaration per name from the
    same module namespace. New declarations require distinct source headers.
    [record_heads] supplies additional exact current records for completion,
    including hidden records with the same name as a visible shadow. It does not
    participate in ordinary name joins. Each identity has one head; a head
    repeated across the two pools must be the same object. Runtime publication
    independently checks these supplied heads against its actual catalog. *)

val compilation_mode : t -> compilation_mode
val identities : t -> identity list
val declarations : t -> resolved_declaration list
val identity_symbol : identity -> Symbol.t
val identity_sites : identity -> declaration_site list
val identity_state : identity -> state
val identity_first_item_index : identity -> int

val declaration_site_function :
  declaration_site -> Function_type_resolution.resolved_function

val declaration_site_source_kind : declaration_site -> declaration_kind
(** Return the binding written in the source. *)

val declaration_site_kind : declaration_site -> declaration_kind
(** Return the binding used for identity reconciliation. *)

val declaration_site_compiler_option_mask : declaration_site -> int64
val declaration_site_state : declaration_site -> state
val declaration_site_is_pending : declaration_site -> bool

val declaration_site_pending_source :
  declaration_site -> Compiler_record.declared_function option
(** The original source proof, only while the site represents a pending header.
*)

val declaration_site_header_source :
  declaration_site -> Compiler_record.declared_function option
(** The original source proof for pending headers and their exact completions.
*)

val resolved_declaration_site : resolved_declaration -> declaration_site
(** Original source declaration, also the body owner on completion. *)

val resolved_declaration_header :
  resolved_declaration -> Function_type_resolution.resolved_function
(** Header selected by new calls. Completion retains the current record's
    header, which may differ from its original body/source declaration. *)

val resolved_declaration_completion_source :
  resolved_declaration -> resolved_declaration option
(** Exact original pending declaration consumed by this completion. This is
    distinct from its current retained/joined predecessor. *)

val find_pending_source :
  current:resolved_declaration ->
  function_:Function_type_resolution.resolved_function ->
  resolved_declaration option
(** Find an exact typed pending source in [current]'s joined ancestry, including
    [current]. The result grants no fresh completion authority: the constructor
    independently rejects an already consumed source. *)

val resolved_declaration_compilation_mode :
  resolved_declaration -> compilation_mode

val resolved_declaration_identity_symbol : resolved_declaration -> Symbol.t

val resolved_declaration_replaced_header :
  resolved_declaration -> declaration_site option

val resolved_declaration_retained_predecessor :
  resolved_declaration -> resolved_declaration option

val is_joined_successor :
  earlier:resolved_declaration -> later:resolved_declaration -> bool
(** Whether [later] strictly succeeds the exact [earlier] declaration through
    local or retained joins. Equal source headers or identity symbols from a
    separately reconstructed resolution do not establish ancestry. *)

val compilation_mode_name : compilation_mode -> string
val declaration_kind_name : declaration_kind -> string
val state_name : state -> string
