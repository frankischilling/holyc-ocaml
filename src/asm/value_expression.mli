type token =
  | Integer of int64
  | Floating of float
  | Name of string
  | Type of string
  | Operator of string
  | Punctuation of char

type value = private { word : int64; absolute : bool }
type error = { token_index : int; message : string }

val evaluate :
  position:int64 ->
  resolve:(string -> (int64 * bool) option) ->
  token list ->
  (value, error) result
(** Evaluate a closed assembler scalar expression. Integer payloads and postfix
    primitive casts retain the original raw type and full word. Floating values
    return their bit representation. Resolved names have the original signed
    address class; the resolver's Boolean marks absolute-address occurrences.

    [absolute] is the parity of those occurrences and [$$] terms in the source,
    independently of arithmetic signs or Boolean results. The caller supplies
    [Type] only when the parser resolved an original primitive type; [Name]
    never authorizes a cast by spelling. The caller also bounds the token count.
    Parsing permits at most 256 nested expression levels; evaluation uses an
    explicit work stack.

    This function neither reads memory nor executes source calls. Unknown names,
    assignments, non-scalar casts and unsupported floating operations return the
    responsible token index. *)
