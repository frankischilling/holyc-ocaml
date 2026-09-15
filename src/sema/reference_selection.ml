type source_stage =
  | Global_declared
  | Global_completed
  | Function_declared
  | Function_header_completed
  | Function_body_completed

type kind =
  | Absent
  | Unavailable
  | Local
  | Source of Symbol.t * source_stage
  | Outer of Outer_environment.t * Outer_environment.binding

type t = { table : Symbol_table.t; name : string; kind : kind }

let kind selection = selection.kind

let validate ~table ~name selection =
  if selection.table != table then
    Error "identifier selection belongs to another semantic table"
  else if not (String.equal name selection.name) then
    Error "identifier selection belongs to another spelling"
  else Ok ()

let make table name kind =
  if String.equal name "" then
    Error "identifier selection has an empty spelling"
  else Ok { table; name; kind }

let absent ~table ~name = make table name Absent
let unavailable ~table ~name = make table name Unavailable
let local ~table ~name = make table name Local

let source ~table ~name ~symbol ~stage =
  let expected =
    match stage with
    | Global_declared | Global_completed -> Symbol.Global_variable
    | Function_declared | Function_header_completed | Function_body_completed ->
        Symbol.Function
  in
  if not (Symbol_table.owns_symbol table symbol) then
    Error "selected source symbol belongs to another semantic table"
  else if not (String.equal name (Symbol.name symbol)) then
    Error "selected source symbol has another spelling"
  else if not (Symbol.equal_kind (Symbol.kind symbol) expected) then
    Error "selected source stage has another declaration kind"
  else make table name (Source (symbol, stage))

let outer ~table ~name ~environment ~binding =
  if not (Outer_environment.owns_table environment table) then
    Error "selected outer environment belongs to another semantic table"
  else if not (Outer_environment.owns_binding environment binding) then
    Error "selected outer binding belongs to another environment"
  else if
    not
      (String.equal name
         (Outer_environment.binding_entry binding
         |> Outer_environment.entry_symbol |> Symbol.name))
  then Error "selected outer binding has another spelling"
  else make table name (Outer (environment, binding))
