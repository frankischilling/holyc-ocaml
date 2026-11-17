val is_storage_free : Sema.Function_frame_layout.location -> bool
(** Recognize an original uninitialized scalar U0 automatic declaration with a
    checked zero-byte slot. This grants no address, runtime cell or
    initialization flag. Initialized declarations, arrays, parameters and
    register requests are separate consumers and are not admitted here. *)
