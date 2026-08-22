type member_input = {
  member_type : Type.t;
  member_type_reference : Type_reference.t;
  member_function_pointer : Function_type_resolution.function_pointer option;
  member_layout : Aggregate_layout.member_layout;
}

type aggregate_input = {
  aggregate_scope : Symbol_table.scope;
  aggregate_layout : Aggregate_layout.aggregate_layout;
  aggregate_members : member_input list;
}

type member = private {
  symbol : Symbol.t;
  declaring_aggregate : Symbol.t;
  member_type : Type.t;
  member_type_reference : Type_reference.t;
  is_function_pointer : bool;
  function_pointer : Function_type_resolution.function_pointer option;
  layout : Aggregate_layout.member_layout;
}

type aggregate = private {
  symbol : Symbol.t;
  item_index : int;
  base_symbol : Symbol.t option;
  layout : Aggregate_layout.aggregate_layout;
  direct_members : member list;
}

type lookup = private {
  queried_aggregate : Symbol.t;
  declaring_aggregate : Symbol.t;
  inheritance_depth : int;
  member : member;
}

type t

type error_kind =
  | Invalid_input of string
  | Unresolved_base of { aggregate : Symbol.t; base : Symbol.t }
  | Duplicate_member of {
      name : string;
      original : Symbol.t;
      duplicate : Symbol.t;
    }
  | Aggregate_not_indexed of Symbol.t

type error

val owns_table : t -> Symbol_table.t -> bool
val owns_parent : t -> Symbol_table.scope -> bool
val parent_scope : t -> Symbol_table.scope

val build :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  aggregate_input list ->
  (t, error) result
(** Build direct-member indexes in source order. Each base must already have an
    index. Duplicate names are rejected except for [pad], [reserved], and
    [_anon_], whose first source occurrence remains the lookup result. *)

val aggregates : t -> aggregate list
val find_aggregate : t -> Symbol.t -> aggregate option
val aggregate_symbol : aggregate -> Symbol.t
val aggregate_item_index : aggregate -> int
val aggregate_size : aggregate -> int64

val lookup :
  t -> aggregate:Symbol.t -> name:string -> (lookup option, error) result
(** Search the queried aggregate before following its single base chain. This
    operation is pure; use-count accounting belongs to the consuming pass. *)

val lookup_queried_aggregate : lookup -> Symbol.t
val lookup_declaring_aggregate : lookup -> Symbol.t
val lookup_inheritance_depth : lookup -> int
val lookup_member : lookup -> member
val member_symbol : member -> Symbol.t
val member_type : member -> Type.t
val member_type_reference : member -> Type_reference.t
val member_is_function_pointer : member -> bool

val member_function_pointer :
  member -> Function_type_resolution.function_pointer option

val member_layout : member -> Aggregate_layout.member_layout
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
