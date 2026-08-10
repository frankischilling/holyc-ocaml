type t

val create : unit -> t
val sources : t -> Common.Source_manager.t
val add_source : t -> path:string -> contents:string -> Common.Source_file.t

val load_source :
  ?max_bytes:int -> t -> path:string -> (Common.Source_file.t, string) result
