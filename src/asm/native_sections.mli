type mode = Aot | Jit | Hosted
type section_kind = Code | Data | Private_data

type section = {
  id : int;
  name : string;
  kind : section_kind;
  alignment_bits : int;
  bytes : string;
}

type binding = Local | Import | Export
type address = { section : int; offset : int }
type definition = Defined of address | Absolute of int64 | Undefined
type source = { item : int option; span : Common.Span.t option }

type symbol = {
  id : int;
  name : string;
  scope : string option;
  source_identity : int option;
  binding : binding;
  definition : definition;
  source : source;
}

type range = Signed | Unsigned | Bit_pattern | Modulo
type target = Symbol of int | Value of int64

type intent =
  | Set of {
      target : target;
      addend : int64;
      relative_to : address option;
      range : range;
    }
  | Add_bias
  | Subtract_bias

type phase = Compile | Load

type relocation = {
  field : address;
  width : int;
  intent : intent;
  phase : phase;
  source : source;
}

type mapping = { address : address; size : int; source : source }

type error = {
  relocation_index : int option;
  source : source option;
  message : string;
}

type t

val no_source : source

val create :
  ?max_bytes:int ->
  ?max_work:int ->
  ?position:int64 ->
  ?image_alignment_bits:int ->
  mode:mode ->
  sections:section list ->
  symbols:symbol list ->
  relocations:relocation list ->
  ?mappings:mapping list ->
  unit ->
  (t, error) result
(** Own and validate an address-independent object. Sections retain input order;
    padding to their alignment is zero. IDs are checked sparse identities, never
    allocation sizes. Names are unique within a scope; imports are root names.
    Definitions may denote the end of a section; fields and map ranges must
    belong to their section. Complete bounds, quotas and identities are checked
    before byte copies. Image alignment metadata defaults to the strongest
    section alignment and may retain the original unsigned exponent 63 without
    attempting that physical layout. BIN serialization rejects that exponent.
    Default byte/work limits are 16 MiB/100000000. *)

val mode : t -> mode
val position : t -> int64
val image_alignment_bits : t -> int
val size : t -> int
val work : t -> int
val sections : t -> section list
val symbols : t -> symbol list
val relocations : t -> relocation list
val mappings : t -> mapping list
val section_offset : t -> int -> int option
val address_offset : t -> address -> (int, error) result

type linked

val link :
  ?max_work:int ->
  ?imports:(string * int64) list ->
  ?defer_imports:bool ->
  ?compile_only:bool ->
  base:int64 ->
  t ->
  (linked, error) result
(** Resolve into a fresh image. [base] is a nonnegative bias added to the raw
    [position]. Compile-only resolution requires zero bias and retains all load
    records. Otherwise records run in source order within compile then load
    phases. Compile fields use raw coordinates; load fields use biased
    coordinates. Missing imports may be retained explicitly; all other failures
    return no output. Relative anchors are explicit, so compiler instruction
    ends and loader field ends remain distinct. Signed, unsigned and bit-pattern
    fields reject narrowing; only an explicit Modulo field or bias record wraps.
    Bias records precede imported replacements within the load phase, as in
    CMain.HC. Both arithmetic and target width are checked before allocation. *)

val base : linked -> int64
val bytes : linked -> string
val pending : linked -> relocation list
val resolved_symbols : linked -> (symbol * int64 option) list
val object_ : linked -> t
val to_yojson : t -> Yojson.Safe.t
val linked_to_yojson : linked -> Yojson.Safe.t
