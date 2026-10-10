val pointer_depth : Frontend.Ast.pointer_layer list -> (int, string) result
(** Validate consecutive original pointer children and the native depth limit.
*)

val builtin :
  Frontend.Ast.type_specifier ->
  Frontend.Ast.pointer_layer list ->
  (Type_reference.t, string) result
(** Check the original primitive/internal type and pointer children without
    looking up a name, constructing replacement syntax or admitting storage.
    Named aggregate types require separate selected type evidence. *)

type selected_aggregate

val selected_base_symbol : selected_aggregate -> Symbol.t
(** Immutable canonical class identity retained by the original selected-type
    proof. Reading it grants no layout, storage or execution authority. *)

type selected_source =
  | Function_return of Frontend.Parser.function_publication
  | Function_parameter of Frontend.Parser.function_parameter_publication
  | Callback_return of Frontend.Parser.callback_signature_publication
  | Callback_parameter of Frontend.Parser.callback_parameter_publication
  | Function_local of Frontend.Parser.function_local_allocation
  | Global_type of Frontend.Parser.global_publication
  | Aggregate_member of Frontend.Parser.aggregate_phase
  | Aggregate_backing of Frontend.Parser.aggregate_publication

val source_type :
  selected_source ->
  ( Frontend.Ast.type_specifier
    * Frontend.Parser.named_aggregate_selection option,
    string )
  result
(** Read the original type occurrence and frozen class selection as immutable
    metadata. This grants no type proof, layout or execution authority. Minting
    a selected aggregate proof still requires the original synchronous callback.
*)

val select_aggregate :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  source:selected_source ->
  Declaration_collection.publication ->
  (selected_aggregate, string) result
(** Mint one proof only during the exact parser owner's original declaration,
    callback-header, parameter or local-allocation callback. The receipt
    supplies its own frozen named selection, type occurrence and pointer
    children; callers cannot provide replacement children. The selected frontend
    entry must map to the exact aggregate semantic publication in this
    table/namespace. The proof stores that publication's canonical aggregate
    identity, so an exact forward/definition completion keeps one physical
    semantic symbol while a later shadow does not. The proof remains immutable
    after the callback and grants no layout or runtime authority. *)

val selected :
  selected_aggregate ->
  Frontend.Ast.type_specifier ->
  Frontend.Ast.pointer_layer list ->
  (Type_reference.t, string) result
(** Resolve only the exact retained named type occurrence and its original
    pointer-layer children. Direct aggregate pointers are admitted; aggregate
    values still require separate layout/ABI authority and are rejected here. *)

val merge_selections :
  selected_aggregate ->
  selected_aggregate ->
  (selected_aggregate, string) result
(** Join original pointer children from comma declarations or nested callback
    publication. Both proofs must retain the same physical type occurrence,
    class entry, parser environment and canonical semantic identity. *)

val selected_callback_return :
  selected_aggregate ->
  Frontend.Ast.type_specifier ->
  Frontend.Ast.pointer_layer list ->
  (Type_reference.t, string) result
(** Resolve an original callback's return metadata, including a direct aggregate
    class. The original pointer children must have been retained by a callback
    receipt. This grants no aggregate storage, layout, invocation or ABI
    authority; the callback cell has separate physical RT_PTR evidence. *)

val selected_header_class :
  selected_aggregate ->
  Frontend.Ast.type_specifier ->
  Frontend.Ast.pointer_layer list ->
  (Type_reference.t, string) result
(** Read the class of the exact original header type occurrence, including an
    aggregate value. This grants no storage, layout, call or ABI authority. *)

val validate_selected_aggregate :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  selected_aggregate ->
  (unit, string) result
(** Check that an already minted proof's canonical aggregate identity belongs to
    the consuming function's exact semantic table and module namespace. This is
    required again at consumers because the same physical parser AST can be
    analyzed under a distinct semantic namespace. *)

val callback_storage :
  header:Frontend.Parser.completed_callback_signature ->
  Frontend.Ast.function_pointer_declarator ->
  (Type_reference.t, string) result
(** Derive physical RT_PTR word storage from the exact completed anonymous
    header and its original indirection. The return class remains separate. This
    pure type evidence grants no default, layout or executable authority. *)

val callback_return :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  selected_aggregate:(Frontend.Ast.type_specifier -> selected_aggregate option) ->
  header:Frontend.Parser.completed_callback_signature ->
  Frontend.Ast.type_specifier ->
  Frontend.Ast.pointer_layer list ->
  (Type_reference.t, string) result
(** Original completed callback return metadata, with separate class identity
    and original pointer children. This has no layout or execution authority. *)
