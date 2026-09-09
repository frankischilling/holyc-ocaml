type role = Query_source.role = Sizeof_root | Offset_root | Defined_operand
type t = Compiler_record.query_read

let make = Compiler_record.complete_query
let validate = Compiler_record.validate_query
let expression = Compiler_record.query_expression
let owns_table = Compiler_record.query_owns_table
let is_local = Compiler_record.query_is_local
let presence = Compiler_record.query_presence
let sizeof = Compiler_record.query_sizeof
let constant = Compiler_record.query_constant
let validate_manifest = Compiler_record.validate_query_manifest
let source_queries = Query_source.source_queries
let name_query_facts = Query_source.name_query_facts
let checked_read selection = selection
