val emit :
  ?max_bytes:int ->
  ?max_work:int ->
  ?main:string list ->
  ?records:Asm.Bin_module.record list ->
  Assembly_encoding.t ->
  (Asm.Bin_module.t, Common.Diagnostic.t) result
(** Serialize a checked standalone AOT assembly image at raw position zero. Its
    exported labels become relative exports and its declared imports retain
    original widths, grouped by name. [main] names are resolved in order and can
    repeat. Additional typed records precede generated import groups. The pinned
    compiler converts every even AAT kind into a U32 ABS_ADDR offset; this
    adapter preserves that behavior, including its narrow-field quirk, while
    rejecting fields which extend beyond the body. Odd AAT kinds remain compiler
    adjustments and do not become IET records. No host bridge is used. *)
