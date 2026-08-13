type member = {
  name : string;
  origin : Symbol.origin;
  member_path : int list;
  declarator_index : int;
}

type aggregate = { symbol : Symbol.t; item_index : int; members : member list }

type entry = {
  symbol : Symbol.t;
  member_path : int list;
  declarator_index : int;
}

type collected_aggregate = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  entries : entry list;
}

type t = { aggregates : collected_aggregate list }

let aggregates collection = collection.aggregates
let aggregate_symbol (aggregate : collected_aggregate) = aggregate.symbol
let aggregate_scope (aggregate : collected_aggregate) = aggregate.scope

let aggregate_item_index (aggregate : collected_aggregate) =
  aggregate.item_index

let aggregate_entries (aggregate : collected_aggregate) = aggregate.entries
let entry_symbol (entry : entry) = entry.symbol
let entry_member_path (entry : entry) = entry.member_path
let entry_declarator_index (entry : entry) = entry.declarator_index

let make_member ~name ~origin ~member_path ~declarator_index =
  if String.equal name "" then Error "semantic member name cannot be empty"
  else if member_path = [] then Error "semantic member path cannot be empty"
  else if List.exists (fun index -> index < 0) member_path then
    Error "semantic member path indexes cannot be negative"
  else if declarator_index < 0 then
    Error "semantic member declarator index cannot be negative"
  else Ok { name; origin; member_path; declarator_index }

let make_aggregate ~symbol ~item_index members =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type) then
    Error "semantic member owner must be an aggregate-type symbol"
  else if item_index < 0 then
    Error "semantic aggregate item index cannot be negative"
  else Ok { symbol; item_index; members }

let rec compare_path left right =
  match (left, right) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | left_index :: left_rest, right_index :: right_rest ->
      let comparison = Int.compare left_index right_index in
      if comparison = 0 then compare_path left_rest right_rest else comparison

let compare_member_position (left : member) (right : member) =
  let comparison = compare_path left.member_path right.member_path in
  if comparison = 0 then
    Int.compare left.declarator_index right.declarator_index
  else comparison

let members_are_ordered (members : member list) =
  let rec check previous = function
    | [] -> true
    | member :: rest ->
        compare_member_position previous member < 0 && check member rest
  in
  match members with
  | [] | [ _ ] -> true
  | first :: rest -> check first rest

let validate_aggregate parent previous_item_index (aggregate : aggregate) =
  if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id aggregate.symbol)
         (Symbol_table.scope_id parent))
  then Error "semantic aggregate symbol does not belong to the module scope"
  else if aggregate.item_index <= previous_item_index then
    Error "semantic aggregate definitions must be in increasing item order"
  else if not (members_are_ordered aggregate.members) then
    Error "semantic members must be in increasing source order"
  else Ok aggregate.item_index

let validate table parent aggregates =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic aggregate parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic aggregate parent must be a module scope"
  else
    let rec check previous_item_index = function
      | [] -> Ok ()
      | aggregate :: rest -> (
          match validate_aggregate parent previous_item_index aggregate with
          | Error _ as error -> error
          | Ok item_index -> check item_index rest)
    in
    check (-1) aggregates

let add_members table scope members =
  let rec add entries_rev = function
    | [] -> Ok (List.rev entries_rev)
    | member :: rest -> (
        match
          Symbol_table.add table ~scope ~name:member.name ~kind:Symbol.Member
            ~origin:member.origin
        with
        | Error _ as error -> error
        | Ok symbol ->
            let entry =
              {
                symbol;
                member_path = member.member_path;
                declarator_index = member.declarator_index;
              }
            in
            add (entry :: entries_rev) rest)
  in
  add [] members

let collect_aggregate table parent (aggregate : aggregate) =
  match
    Symbol_table.create_scope table ~parent ~kind:Symbol_table.Aggregate
      ~name:(Symbol.name aggregate.symbol)
      ()
  with
  | Error _ as error -> error
  | Ok scope -> (
      match add_members table scope aggregate.members with
      | Error _ as error -> error
      | Ok entries ->
          Ok
            {
              symbol = aggregate.symbol;
              scope;
              item_index = aggregate.item_index;
              entries;
            })

let collect ~table ~parent aggregate_facts =
  match validate table parent aggregate_facts with
  | Error _ as error -> error
  | Ok () ->
      let rec collect_all aggregates_rev = function
        | [] -> Ok { aggregates = List.rev aggregates_rev }
        | aggregate :: rest -> (
            match collect_aggregate table parent aggregate with
            | Error _ as error -> error
            | Ok collected -> collect_all (collected :: aggregates_rev) rest)
      in
      collect_all [] aggregate_facts
