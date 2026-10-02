type t = {
  table : Symbol_table.t;
  namespace_ : Declaration_collection.namespace;
  receipt_ : Frontend.Parser.internal_binding_preparation;
  bits_ : int64;
  work_ : int;
}

let receipt value = value.receipt_
let bits value = value.bits_
let work value = value.work_
let owns_table value table = value.table == table
let namespace value = value.namespace_

let create ~table ~namespace ~receipt ~bits ~work =
  if
    work < 0
    || (not (Declaration_collection.namespace_owns_table namespace table))
    || not (Frontend.Parser.internal_binding_is_current receipt)
  then Error "internal target requires its original live source and work count"
  else
    Ok
      {
        table;
        namespace_ = namespace;
        receipt_ = receipt;
        bits_ = bits;
        work_ = work;
      }

let matches_header value (header : Frontend.Parser.declaration_header) =
  Option.fold ~none:false ~some:(( == ) value.receipt_)
    header.binding_preparation
  && Option.fold ~none:false
       ~some:(( == ) value.receipt_.binding_ast)
       header.binding
  && header.declaration_command == value.receipt_.binding_command
