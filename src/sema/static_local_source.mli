type t

val bind :
  allocation:Compiler_record.static_allocation ->
  frame:Function_frame_layout.function_layout ->
  location:Function_frame_layout.location ->
  (t, string) result
(** Join a live original static allocation to its later declaring frame and
    retained local type evidence. An available source header must belong to the
    original publication. A matching name or numeric symbol identity cannot
    replace the original source objects. This witness grants no arena
    allocation, initializer entry or executable authority. *)

val allocation : t -> Compiler_record.static_allocation
val frame : t -> Function_frame_layout.function_layout
val location : t -> Function_frame_layout.location
