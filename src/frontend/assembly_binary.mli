type compression = Raw | Ct_none | Ct_7_bit | Ct_8_bit

type provenance = {
  spelling : string;
  resolved_path : string;
  display_path : string;
  raw_size : int;
  decoded_size : int;
  raw_sha256 : string;
  decoded_sha256 : string;
  compression : compression;
}

type t = private { bytes : string; provenance : provenance }

val decode :
  max_input_bytes:int ->
  max_output_bytes:int ->
  compressed:bool ->
  string ->
  (string * compression, string) result
(** Decode bytes already in memory. [compressed] selects the TempleOS
    [CArcCompress] format; malformed archives never fall back to raw bytes. *)

val load :
  resolver:Include_resolver.t ->
  max_input_bytes:int ->
  max_output_bytes:int ->
  spelling:string ->
  (t, string) result
(** Apply TempleOS's default [.BIN] extension and named-file/[.Z] fallback
    within the resolver's configured roots. Only regular files are read. Input
    sizes and the archive header are checked before reading the payload;
    compressed code chains are checked before allocating expanded output. *)
