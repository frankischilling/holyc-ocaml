type compilation_mode = Jit | Aot
type declaration_kind = Extern | Bound_extern | Import | Intern | Definition
type state = Unresolved_extern | Imported | Resolved
type declaration
type declaration_site
type identity
type resolved_declaration
type t

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

val resolve :
  ?previous:resolved_declaration list ->
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  compilation_mode:compilation_mode ->
  declaration list ->
  (t, string) result
(** Reconcile function identities in source order. JIT joins only the newest
    unresolved extern; AOT joins the newest identity unless it is imported.
    [previous] supplies at most one newest JIT declaration per name from the
    same module namespace. New declarations require distinct source headers.
    Runtime publication separately checks that each predecessor is still
    current. *)

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
val resolved_declaration_site : resolved_declaration -> declaration_site

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
