type error = { artifact : string; offset : int option; message : string }
(** Independent, bounded interpretation of Kernel/KLoad.HC. Addresses are
    virtual integers. No executable allocation, host symbol lookup or call is
    performed, including for MAIN records. *)

type limits = {
  file_bytes : int;
  work : int;
  image_bytes : int;
  heap_bytes : int64;
  modules : int;
}

val default_limits : limits
(** File limits apply per artifact. Work limits apply to reading an artifact,
    and cumulatively to transactions in a world. Image and requested heap byte
    limits are cumulative. Limits cannot exceed the documented hard bounds. *)

type record_kind =
  | Import of { width : int; relative : bool; target : int }
  | Export of { relative : bool; value : int64 }
  | Absolute of int list
  | Heap of { code : bool; zeroed : bool; size : int64; targets : int list }
  | Main of int

type record = {
  offset : int;
  wire_type : int;
  name : string;
  kind : record_kind;
}

type artifact

val read : ?limits:limits -> ?name:string -> string -> (artifact, error) result
val records : artifact -> record list
val body : artifact -> string
val bytes : artifact -> string
val to_yojson : artifact -> Yojson.Safe.t
val work : artifact -> int

type flags = { no_abss : bool; just_load : bool; silent : bool }

val default_flags : flags

type world

val empty :
  ?module_base:int64 -> ?code_base:int64 -> ?data_base:int64 -> unit -> world
(** These three bases configure a deterministic virtual allocator, rather than
    predicting TempleOS heap addresses. Automatic body bases honor module
    alignment and header org. An explicit [base] overrides header org, as the
    original Load's supplied header pointer does. *)

val load :
  ?limits:limits ->
  ?flags:flags ->
  ?base:int64 ->
  world ->
  artifact ->
  (world, error) result
(** Validate and plan the entire transaction before applying any byte patches.
    Success returns a new world. The artifact and every older world retain their
    original bytes even when deferred imports are resolved. *)

val bind :
  ?limits:limits -> world -> name:string -> value:int64 -> (world, error) result
(** Publish an external symbol and replay pending import groups, newest first,
    like SysSymImportsResolve. Functions and globals can be represented by their
    virtual exe_addr/data_addr. No host authority is accepted. *)

type image = { id : int; name : string; base : int64; body : string }

type relocation = {
  module_id : int;
  artifact : string;
  record_offset : int;
  field_offset : int;
  width : int;
  kind : string;
  symbol : string;
  operand : int64;
  before : int64;
  after : int64;
}

type pending = {
  module_id : int;
  artifact : string;
  record_offset : int;
  symbol : string;
  fields : (int * int * int) list;
}

type heap = {
  module_id : int;
  record_offset : int;
  name : string;
  code : bool;
  zeroed : bool;
  base : int64;
  size : int64;
}

type entry = { module_id : int; record_offset : int; address : int64 }

val images : world -> image list
val relocations : world -> relocation list
val unresolved : world -> pending list
val heaps : world -> heap list
val entries : world -> entry list
val symbols : world -> (string * int64) list
val transaction_work : world -> int
val report : world -> Yojson.Safe.t
val map : world -> string
