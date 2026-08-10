type t

val create : unit -> t
val add_string : t -> path:string -> contents:string -> Source_file.t
val load : ?max_bytes:int -> t -> path:string -> (Source_file.t, string) result
val find : t -> Source_id.t -> Source_file.t option
