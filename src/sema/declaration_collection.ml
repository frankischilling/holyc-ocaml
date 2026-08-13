type declaration_kind =
  | Aggregate_forward
  | Aggregate_definition
  | Aggregate_attached_global
  | Global_variable
  | Function_prototype
  | Function_definition

type declaration = {
  name : string;
  declaration_kind : declaration_kind;
  origin : Symbol.origin;
  item_index : int;
  declarator_index : int option;
}

type entry = {
  symbol : Symbol.t;
  declaration_kind : declaration_kind;
  item_index : int;
  declarator_index : int option;
}

type t = { scope : Symbol_table.scope; entries : entry list }

let scope collection = collection.scope
let entries collection = collection.entries
let entry_symbol entry = entry.symbol
let entry_kind entry = entry.declaration_kind
let entry_item_index entry = entry.item_index
let entry_declarator_index entry = entry.declarator_index

let declaration_kind_name = function
  | Aggregate_forward -> "aggregate-forward"
  | Aggregate_definition -> "aggregate-definition"
  | Aggregate_attached_global -> "aggregate-attached-global"
  | Global_variable -> "global-variable"
  | Function_prototype -> "function-prototype"
  | Function_definition -> "function-definition"

let make_declaration ~name ~declaration_kind ~origin ~item_index
    ?declarator_index () =
  if String.equal name "" then Error "semantic declaration name cannot be empty"
  else if item_index < 0 then
    Error "semantic declaration item index cannot be negative"
  else
    match declarator_index with
    | Some index when index < 0 ->
        Error "semantic declarator index cannot be negative"
    | None | Some _ ->
        Ok { name; declaration_kind; origin; item_index; declarator_index }

let symbol_kind = function
  | Aggregate_forward | Aggregate_definition -> Symbol.Aggregate_type
  | Aggregate_attached_global | Global_variable -> Symbol.Global_variable
  | Function_prototype | Function_definition -> Symbol.Function

let create_module_scope table module_name =
  let parent = Symbol_table.root table in
  match module_name with
  | Some name when not (String.equal name "") ->
      Symbol_table.create_scope table ~parent ~kind:Symbol_table.Module ~name ()
  | None | Some _ ->
      Symbol_table.create_scope table ~parent ~kind:Symbol_table.Module ()

let add_entry table scope declaration =
  match
    Symbol_table.add table ~scope ~name:declaration.name
      ~kind:(symbol_kind declaration.declaration_kind)
      ~origin:declaration.origin
  with
  | Error _ as error -> error
  | Ok symbol ->
      Ok
        {
          symbol;
          declaration_kind = declaration.declaration_kind;
          item_index = declaration.item_index;
          declarator_index = declaration.declarator_index;
        }

let collect ~table ?module_name declarations =
  match create_module_scope table module_name with
  | Error _ as error -> error
  | Ok scope ->
      let rec add entries_rev = function
        | [] -> Ok { scope; entries = List.rev entries_rev }
        | declaration :: rest -> (
            match add_entry table scope declaration with
            | Error _ as error -> error
            | Ok entry -> add (entry :: entries_rev) rest)
      in
      add [] declarations
