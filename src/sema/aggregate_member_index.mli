type member_input = {
  member_type : Type.t;
  member_is_function_pointer : bool;
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
  is_function_pointer : bool;
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

val lookup :
  t -> aggregate:Symbol.t -> name:string -> (lookup option, error) result
(** Search the queried aggregate before following its single base chain. This
    operation is pure; use-count accounting belongs to the consuming pass. *)

val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
