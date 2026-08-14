type type_reference = Type_reference.t
type function_pointer = Function_type_resolution.function_pointer
type declarator_kind = Object | Function_pointer of function_pointer

type member = {
  symbol : Symbol.t;
  member_path : int list;
  declarator_index : int;
  declarator_origin : Symbol.origin;
  type_reference : type_reference;
  declarator_kind : declarator_kind;
  flag_mask : int64;
  array_dimension_origins : Symbol.origin list;
}

type aggregate = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  members : member list;
}

type t = { aggregates : aggregate list }

let aggregates resolution = resolution.aggregates
let aggregate_symbol (aggregate : aggregate) = aggregate.symbol
let aggregate_scope (aggregate : aggregate) = aggregate.scope
let aggregate_item_index (aggregate : aggregate) = aggregate.item_index
let aggregate_members (aggregate : aggregate) = aggregate.members
let member_symbol (member : member) = member.symbol
let member_path (member : member) = member.member_path
let member_declarator_index (member : member) = member.declarator_index
let member_declarator_origin (member : member) = member.declarator_origin
let member_type_reference (member : member) = member.type_reference
let member_declarator_kind (member : member) = member.declarator_kind
let member_flag_mask (member : member) = member.flag_mask

let member_has_flag (member : member) flag =
  Member_flag.is_set ~mask:member.flag_mask flag

let member_array_dimension_origins (member : member) =
  member.array_dimension_origins

let type_reference_spelling = Type_reference.spelling
let type_reference_spelling_origin = Type_reference.spelling_origin
let type_reference_pointer_origins = Type_reference.pointer_origins
let type_reference_type = Type_reference.resolved_type
let function_pointer_origin = Function_type_resolution.function_pointer_origin

let function_pointer_opening_origin =
  Function_type_resolution.function_pointer_opening_origin

let function_pointer_indirection_origins =
  Function_type_resolution.function_pointer_indirection_origins

let function_pointer_closing_origin =
  Function_type_resolution.function_pointer_closing_origin

let function_pointer_signature =
  Function_type_resolution.function_pointer_signature

let make_type_reference = Type_reference.make
let make_function_pointer = Function_type_resolution.make_function_pointer

let make_member ~symbol ~member_path ~declarator_index ~declarator_origin
    ~type_reference ~declarator_kind ~array_dimension_origins =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Member) then
    Error "semantic member type requires a member symbol"
  else if member_path = [] then
    Error "semantic member type path cannot be empty"
  else if List.exists (fun index -> index < 0) member_path then
    Error "semantic member type path indexes cannot be negative"
  else if declarator_index < 0 then
    Error "semantic member type declarator index cannot be negative"
  else
    let flag_mask =
      match declarator_kind with
      | Object -> 0L
      | Function_pointer _ ->
          Member_flag.set ~mask:0L Member_flag.Function_pointer
    in
    Ok
      {
        symbol;
        member_path;
        declarator_index;
        declarator_origin;
        type_reference;
        declarator_kind;
        flag_mask;
        array_dimension_origins;
      }

let make_aggregate ~symbol ~scope ~item_index members =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type) then
    Error "semantic member type owner must be an aggregate-type symbol"
  else if Symbol_table.scope_kind scope <> Symbol_table.Aggregate then
    Error "semantic member types require an aggregate scope"
  else if item_index < 0 then
    Error "semantic member type item index cannot be negative"
  else Ok { symbol; scope; item_index; members }

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

let members_are_ordered members =
  let rec check previous = function
    | [] -> true
    | member :: rest ->
        compare_member_position previous member < 0 && check member rest
  in
  match members with
  | [] | [ _ ] -> true
  | first :: rest -> check first rest

module Int_set = Set.Make (Int)

let scope_equal left right =
  Symbol.Scope_id.equal
    (Symbol_table.scope_id left)
    (Symbol_table.scope_id right)

let validate_type_reference ~table ~parent reference =
  match Type.base (Type_reference.resolved_type reference) with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol ->
      if not (Symbol_table.owns_symbol table symbol) then
        Error "semantic member type target belongs to a different symbol table"
      else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
      then Error "semantic member type target is not an aggregate-type symbol"
      else if
        not
          (Symbol.Scope_id.equal (Symbol.scope_id symbol)
             (Symbol_table.scope_id parent))
      then Error "semantic member type target does not belong to the module"
      else Ok ()

let rec validate_signature_types ~table ~parent signature =
  let rec validate_parameters = function
    | [] -> Ok ()
    | parameter :: rest -> (
        match
          validate_type_reference ~table ~parent
            (Function_type_resolution.parameter_type_reference parameter)
        with
        | Error _ as error -> error
        | Ok () -> (
            match
              Function_type_resolution.parameter_declarator_kind parameter
            with
            | Function_type_resolution.Object -> validate_parameters rest
            | Function_type_resolution.Function_pointer pointer -> (
                match
                  validate_signature_types ~table ~parent
                    (Function_type_resolution.function_pointer_signature pointer)
                with
                | Error _ as error -> error
                | Ok () -> validate_parameters rest)))
  in
  validate_parameters (Function_type_resolution.signature_parameters signature)

let validate_member ~table ~parent ~scope seen (member : member) =
  let symbol_id = Symbol.Id.to_int (Symbol.id member.symbol) in
  if not (Symbol_table.owns_symbol table member.symbol) then
    Error "semantic member type symbol belongs to a different symbol table"
  else if not (Symbol.equal_kind (Symbol.kind member.symbol) Symbol.Member) then
    Error "semantic member type symbol is not a member"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id member.symbol)
         (Symbol_table.scope_id scope))
  then Error "semantic member type symbol does not belong to its aggregate"
  else if Int_set.mem symbol_id seen then
    Error "semantic member type symbols cannot repeat"
  else
    match validate_type_reference ~table ~parent member.type_reference with
    | Error _ as error -> error
    | Ok () -> (
        match member.declarator_kind with
        | Object -> Ok (Int_set.add symbol_id seen)
        | Function_pointer pointer ->
            Result.map
              (fun () -> Int_set.add symbol_id seen)
              (validate_signature_types ~table ~parent
                 (Function_type_resolution.function_pointer_signature pointer)))

let validate_members ~table ~parent ~scope members =
  if not (members_are_ordered members) then
    Error "semantic member types must be in increasing source order"
  else
    let rec validate seen = function
      | [] -> Ok ()
      | member :: rest -> (
          match validate_member ~table ~parent ~scope seen member with
          | Error _ as error -> error
          | Ok seen -> validate seen rest)
    in
    validate Int_set.empty members

let validate_aggregate ~table ~parent ~previous_item_index ~seen_symbols
    ~seen_scopes (aggregate : aggregate) =
  let symbol_id = Symbol.Id.to_int (Symbol.id aggregate.symbol) in
  let scope_id =
    Symbol.Scope_id.to_int (Symbol_table.scope_id aggregate.scope)
  in
  if not (Symbol_table.owns_symbol table aggregate.symbol) then
    Error "semantic member type owner belongs to a different symbol table"
  else if
    not (Symbol.equal_kind (Symbol.kind aggregate.symbol) Symbol.Aggregate_type)
  then Error "semantic member type owner is not an aggregate-type symbol"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id aggregate.symbol)
         (Symbol_table.scope_id parent))
  then Error "semantic member type owner does not belong to the module"
  else if not (Symbol_table.owns_scope table aggregate.scope) then
    Error "semantic member type scope belongs to a different symbol table"
  else if Symbol_table.scope_kind aggregate.scope <> Symbol_table.Aggregate then
    Error "semantic member types require an aggregate scope"
  else if
    match Symbol_table.parent aggregate.scope with
    | Some scope -> not (scope_equal scope parent)
    | None -> true
  then Error "semantic member type scope does not belong to the module"
  else if
    match Symbol_table.scope_name aggregate.scope with
    | Some name -> not (String.equal name (Symbol.name aggregate.symbol))
    | None -> true
  then Error "semantic member type scope does not name its aggregate"
  else if aggregate.item_index <= previous_item_index then
    Error "semantic member type aggregates must be in increasing item order"
  else if Int_set.mem symbol_id seen_symbols then
    Error "semantic member type aggregate symbols cannot repeat"
  else if Int_set.mem scope_id seen_scopes then
    Error "semantic member type aggregate scopes cannot repeat"
  else
    Result.map
      (fun () ->
        ( aggregate.item_index,
          Int_set.add symbol_id seen_symbols,
          Int_set.add scope_id seen_scopes ))
      (validate_members ~table ~parent ~scope:aggregate.scope aggregate.members)

let resolve ~table ~parent aggregate_facts =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic member type parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic member type parent must be a module scope"
  else
    let rec validate previous_item_index seen_symbols seen_scopes = function
      | [] -> Ok { aggregates = aggregate_facts }
      | aggregate :: rest -> (
          match
            validate_aggregate ~table ~parent ~previous_item_index ~seen_symbols
              ~seen_scopes aggregate
          with
          | Error _ as error -> error
          | Ok (item_index, seen_symbols, seen_scopes) ->
              validate item_index seen_symbols seen_scopes rest)
    in
    validate (-1) Int_set.empty Int_set.empty aggregate_facts
