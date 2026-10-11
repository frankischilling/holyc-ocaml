type mode = Use16 | Use32 | Use64

(** Tokens retain the source lookup category. [Symbol] denotes an export or
    unresolved label; [Import_symbol] preserves an assembler import even when
    its spelling is also a query keyword, and contributes no absolute-address
    count. [Value_symbol] denotes a visible value; [Type_name] denotes a visible
    class or internal type. [Function_call.parameters] records whether each
    fixed parameter has a default. [Function_symbol] has no available parameter
    shape and therefore requires parentheses.
    [Variable_symbol.parameters = None] proves a variable is not callable;
    [Some parameters] supplies its function-pointer call shape.
    [Address_symbol.absolute] records a known absolute-address category, or
    [None] when export metadata is still needed. It does not invoke a function.
    [Type_definition] marks a span already validated by the declaration parser;
    its count includes the original class/union keyword through the body close.
    Every original token remains present in the supplied list. Expression
    bracket markers distinguish HolyC indexing from raw assembly address
    brackets. [Floating] retains the literal's IEEE 754 bits. *)
type token =
  | Register of Register.t
  | Size of int
  | Integer of int64
  | Floating of int64
  | String_literal of string
  | Type_name of string
  | Type_definition of { name : string; token_count : int }
  | Expression_open_bracket
  | Expression_close_bracket
  | Symbol of string
  | Import_symbol of string
  | Value_symbol of string
  | Address_symbol of { name : string; absolute : bool option }
  | Variable_symbol of {
      name : string;
      parameters : bool list option;
      variadic : bool;
    }
  | Function_symbol of string
  | Function_call of { name : string; parameters : bool list; variadic : bool }
  | Operator of string
  | Punctuation of char
  | Opaque of string

type error = { token_index : int; message : string }
(** [token_index] is zero-based within the supplied token list. *)

type immediate = private {
  expression : token list;
  value : int64 option;
  symbols : string list;
  absolute : bool;
  absolute_known : bool;
}
(** [absolute] follows the source's address-count parity. When
    [absolute_known = false], unresolved address categories contribute zero
    provisionally; later export resolution must confirm the selected width. *)

type t

val mode_bits : mode -> int
val mode : t -> mode
val tokens : t -> token list
val size : t -> int
val segment : t -> Register.t option
val register1 : t -> Register.t option
val register2 : t -> Register.t option
val scale : t -> int64
val indirect : t -> bool
val just_segment : t -> bool
val immediate : t -> immediate option

val resolve_immediate : value:int64 -> absolute:bool -> t -> t
(** Retain the parsed source expression and addressing path while supplying its
    resolved word and address category. Direct expressions apply the original
    absolute-address width rule from their explicit prefix and relative role. *)

val unresolved : t -> bool
val immediate_present : t -> bool
val parse : mode:mode -> relative:bool -> token list -> (t, error) result

val argument_mask : t -> int64
(** [argument_mask] follows [AsmMakeArgMask] at the pinned TempleOS source.
    Expressions that need compilation retain [value = None]. Matching uses the
    original unresolved numeric slot's initial zero, without resolving that
    expression or performing later position-dependent fixups. *)
