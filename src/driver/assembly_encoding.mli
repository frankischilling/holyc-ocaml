type instruction = private {
  item_index : int;
  operation : Frontend.Ast.inline_assembly_operation;
  position : int64;
  offset : int;
  size : int;
}

type t = private {
  assembly : Frontend.Ast.assembly_unit;
  bytes : string;
  position : int64;
  end_position : int64;
  instructions : instruction list;
}

val encode :
  ?position:int64 ->
  max_code_bytes:int ->
  Frontend.Ast.assembly_unit ->
  (t, Common.Diagnostic.t) result
(** Preflight a parsed sequence of instructions, USE directives and separators.
    Every expression must already have a known value. Labels and other
    directives require the separate layout and directive phases. The positive
    output limit, instruction fields and nonnegative signed 64-bit output
    positions are checked before allocating the output buffer. This operation
    returns bytes only; it does not load or execute them. *)

val schema : string

val to_yojson : Common.Source_manager.t -> t -> Yojson.Safe.t
(** Report the encoded bytes with the exact parsed assembly retained by
    [encode]. *)

val human : t -> string
