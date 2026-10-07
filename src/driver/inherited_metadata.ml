let contains ~table ~scope metadata definition =
  List.exists
    (Sema.Compiler_record.inherited_metadata_owns_definition ~table ~scope
       definition)
    metadata
