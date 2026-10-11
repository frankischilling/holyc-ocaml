val schema : string
val human : Common.Source_manager.t -> Ast.module_ -> string
val to_yojson : Common.Source_manager.t -> Ast.module_ -> Yojson.Safe.t
val json : Common.Source_manager.t -> Ast.module_ -> string
val assembly_schema : string
val assembly_human : Common.Source_manager.t -> Ast.assembly_unit -> string

val assembly_to_yojson :
  Common.Source_manager.t -> Ast.assembly_unit -> Yojson.Safe.t

val assembly_json : Common.Source_manager.t -> Ast.assembly_unit -> string
