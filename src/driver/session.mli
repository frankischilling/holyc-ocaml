type t

val create : unit -> t
val sources : t -> Common.Source_manager.t
val definitions : t -> Frontend.Definition.Environment.t
val symbols : t -> Frontend.Symbol_visibility.Environment.t
val add_source : t -> path:string -> contents:string -> Common.Source_file.t

val load_source :
  ?max_bytes:int ->
  ?display_path:string ->
  t ->
  path:string ->
  (Common.Source_file.t, string) result
