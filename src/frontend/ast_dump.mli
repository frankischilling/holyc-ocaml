val schema : string
val human : Common.Source_manager.t -> Ast.module_ -> string
val to_yojson : Common.Source_manager.t -> Ast.module_ -> Yojson.Safe.t
val json : Common.Source_manager.t -> Ast.module_ -> string
