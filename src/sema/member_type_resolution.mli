type type_reference = Type_reference.t
(** The shared semantic source-type reference. *)

type function_pointer
(** The source head of a callback member. Its recursive signature is resolved by
    a later declarator pass. *)

type declarator_kind = Object | Function_pointer of function_pointer
type member
type aggregate
type t

val make_type_reference :
  spelling:string ->
  spelling_origin:Symbol.origin ->
  pointer_origins:Symbol.origin list ->
  resolved_type:Type.t ->
  (type_reference, string) result

val make_function_pointer :
  origin:Symbol.origin ->
  indirection_origins:Symbol.origin list ->
  (function_pointer, string) result

val make_member :
  symbol:Symbol.t ->
  member_path:int list ->
  declarator_index:int ->
  declarator_origin:Symbol.origin ->
  type_reference:type_reference ->
  declarator_kind:declarator_kind ->
  array_dimension_origins:Symbol.origin list ->
  (member, string) result

val make_aggregate :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  member list ->
  (aggregate, string) result

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  aggregate list ->
  (t, string) result
(** Validate member type references without changing the symbol table. *)

val aggregates : t -> aggregate list
val aggregate_symbol : aggregate -> Symbol.t
val aggregate_scope : aggregate -> Symbol_table.scope
val aggregate_item_index : aggregate -> int
val aggregate_members : aggregate -> member list
val member_symbol : member -> Symbol.t
val member_path : member -> int list
val member_declarator_index : member -> int
val member_declarator_origin : member -> Symbol.origin
val member_type_reference : member -> type_reference
val member_declarator_kind : member -> declarator_kind
val member_array_dimension_origins : member -> Symbol.origin list
val type_reference_spelling : type_reference -> string
val type_reference_spelling_origin : type_reference -> Symbol.origin
val type_reference_pointer_origins : type_reference -> Symbol.origin list
val type_reference_type : type_reference -> Type.t
val function_pointer_origin : function_pointer -> Symbol.origin

val function_pointer_indirection_origins :
  function_pointer -> Symbol.origin list
