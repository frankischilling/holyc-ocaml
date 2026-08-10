type t
type position = { offset : int; line : int; column : int }

val create :
  id:Source_id.t -> path:string -> display_path:string -> contents:string -> t

val load :
  ?max_bytes:int -> id:Source_id.t -> path:string -> unit -> (t, string) result

val id : t -> Source_id.t
val path : t -> string
val display_path : t -> string
val contents : t -> string
val length : t -> int
val line_count : t -> int
val position : t -> int -> (position, string) result
val line_bounds : t -> line:int -> (int * int, string) result
val line_text : t -> line:int -> (string, string) result
