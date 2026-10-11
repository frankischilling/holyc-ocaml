module Entry = Generated.Bin_records.Entry

type heap = { name : string; size : int64; offsets : int list }

type record =
  | Import of { kind : Entry.t; offset : int; name : string }
  | Export of { kind : Entry.t; value : int64; name : string }
  | Absolute_addresses of int list
  | Code_heap of { zeroed : bool; heap : heap }
  | Data_heap of { zeroed : bool; heap : heap }
  | Main of int

type error = {
  offset : int option;
  record_index : int option;
  message : string;
}

type t

val max_file_bytes : int

val create :
  ?max_bytes:int ->
  ?max_work:int ->
  alignment_bits:int ->
  origin:int64 option ->
  body:string ->
  record list ->
  (t, error) result
(** Construct the original uncompressed CBinFile envelope and terminated patch
    table. Records retain supplied order, including repeated exports, offsets
    and MAIN entries. Import groups must start with a name and cannot precede an
    empty-name nonimport record. Fields are checked against the complete body
    before serialization. AAT compiler adjustments are not IET records. Nothing
    in this value authorizes executable memory or host invocation. *)

val read : ?max_bytes:int -> ?max_work:int -> string -> (t, error) result
(** Inspect an immutable complete uncompressed module. Header sizes, reserved
    bytes, alignment, terminated names, record shapes, body fields and final
    zero padding are validated. No partial module is returned. *)

val bytes : t -> string
val body : t -> string
val records : t -> record list
val origin : t -> int64 option
val alignment_bits : t -> int
val patch_table_offset : t -> int
val file_size : t -> int
val table_end : t -> int

val work : t -> int
(** File bytes plus one unit per record, name byte and patch field. The writer
    and reader use the same cost. Their default allowance is 1,000,000. The hard
    bounds are 16 MiB per file and 100,000,000 work units. *)

val record_entry : record -> Entry.t
val record_name : record -> string
val record_value : record -> int64
val record_offsets : t -> int list
val map : t -> string
val to_yojson : t -> Yojson.Safe.t

val records_of_yojson :
  ?max_work:int -> Yojson.Safe.t -> (record list, error) result
(** Read the explicit record-array input used by the public BIN command. Each
    object uses the original IET type name, with its category-specific fields.
    Unknown keys and types fail. Body bounds are checked by [create]. *)

val of_sections :
  ?max_bytes:int ->
  ?max_work:int ->
  ?origin:int64 option ->
  ?main:int list ->
  ?records:record list ->
  Native_sections.t ->
  (t, error) result
(** Serialize a checked AOT object. Symbol IDs select MAIN entries. Exports
    follow symbol order and import groups follow first use. Compiler AAT add
    records become the original ABS_ADDR U32 table in reverse order regardless
    of the original compiler field width; subtract records are compiler-only and
    do not become loader records. BIN body bounds are checked at that U32 width.
    Relative imports must use field-end anchors and have zero addends.
    Hosted/JIT objects and private runtime data are rejected before output. *)
