type member_input = {
  member_type : Type.t;
  member_type_reference : Type_reference.t;
  member_function_pointer : Function_type_resolution.function_pointer option;
  member_layout : Aggregate_layout.member_layout;
}

type aggregate_input = {
  aggregate_scope : Symbol_table.scope;
  aggregate_layout : Aggregate_layout.aggregate_layout;
  aggregate_members : member_input list;
}

type member = {
  symbol : Symbol.t;
  declaring_aggregate : Symbol.t;
  member_type : Type.t;
  member_type_reference : Type_reference.t;
  is_function_pointer : bool;
  function_pointer : Function_type_resolution.function_pointer option;
  layout : Aggregate_layout.member_layout;
}

type aggregate = {
  symbol : Symbol.t;
  item_index : int;
  base_symbol : Symbol.t option;
  layout : Aggregate_layout.aggregate_layout;
  direct_members : member list;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

type indexed_aggregate = {
  aggregate : aggregate;
  direct_by_name : member String_map.t;
}

type lookup = {
  queried_aggregate : Symbol.t;
  declaring_aggregate : Symbol.t;
  inheritance_depth : int;
  member : member;
}

type t = {
  table : Symbol_table.t;
  parent : Symbol_table.scope;
  aggregates : aggregate list;
  by_symbol : indexed_aggregate Int_map.t;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_base of { aggregate : Symbol.t; base : Symbol.t }
  | Duplicate_member of {
      name : string;
      original : Symbol.t;
      duplicate : Symbol.t;
    }
  | Aggregate_not_indexed of Symbol.t

type error = {
  code : string;
  kind : error_kind;
  origin : Symbol.origin option;
  message : string;
}

let symbol_key symbol = Symbol.id symbol |> Symbol.Id.to_int
let same_symbol left right = Symbol.Id.equal (Symbol.id left) (Symbol.id right)

let same_type left right =
  Type.pointer_depth left = Type.pointer_depth right
  &&
  match (Type.base left, Type.base right) with
  | Type.Primitive (left_form, left), Type.Primitive (right_form, right) ->
      left_form = right_form && Primitive_type.equal left right
  | Type.Aggregate left, Type.Aggregate right -> same_symbol left right
  | Type.Primitive _, Type.Aggregate _ | Type.Aggregate _, Type.Primitive _ ->
      false

let make_error ?origin code kind message = { code; kind; origin; message }

let invalid_input ?origin message =
  make_error ?origin "HCSEMA0010" (Invalid_input message) message

let unresolved_base aggregate base =
  make_error ~origin:(Symbol.origin aggregate) "HCSEMA0011"
    (Unresolved_base { aggregate; base })
    (Printf.sprintf
       "aggregate member index for `%s` needs the earlier base `%s`"
       (Symbol.name aggregate) (Symbol.name base))

let duplicate_member original duplicate =
  let name = Symbol.name duplicate in
  make_error ~origin:(Symbol.origin duplicate) "HCSEMA0012"
    (Duplicate_member { name; original; duplicate })
    (Printf.sprintf
       "member `%s` duplicates an earlier direct or inherited member" name)

let aggregate_not_indexed aggregate =
  make_error ~origin:(Symbol.origin aggregate) "HCSEMA0013"
    (Aggregate_not_indexed aggregate)
    (Printf.sprintf "aggregate `%s` does not have a member index"
       (Symbol.name aggregate))

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin
let error_message error = error.message
let error_to_string error = Printf.sprintf "%s: %s" error.code error.message

let duplicate_name_is_permitted = function
  | "pad" | "reserved" | "_anon_" -> true
  | _ -> false

let base_symbol layout =
  Option.map
    (fun (base : Aggregate_layout.base_layout) -> base.symbol)
    layout.Aggregate_layout.base

let rec lookup_indexed by_symbol aggregate name inheritance_depth =
  match String_map.find_opt name aggregate.direct_by_name with
  | Some member -> Some (inheritance_depth, member)
  | None -> (
      match aggregate.aggregate.base_symbol with
      | None -> None
      | Some base -> (
          match Int_map.find_opt (symbol_key base) by_symbol with
          | None -> None
          | Some base ->
              lookup_indexed by_symbol base name (inheritance_depth + 1)))

let compare_path left right =
  let rec compare left right =
    match (left, right) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | left :: left_rest, right :: right_rest ->
        let order = Int.compare left right in
        if order = 0 then compare left_rest right_rest else order
  in
  compare left right

let compare_member_position left right =
  let left = left.member_layout in
  let right = right.member_layout in
  let order = compare_path left.path right.path in
  if order = 0 then Int.compare left.declarator_index right.declarator_index
  else order

let members_are_ordered = function
  | [] | [ _ ] -> true
  | first :: rest ->
      let rec loop previous = function
        | [] -> true
        | member :: remaining ->
            compare_member_position previous member < 0 && loop member remaining
      in
      loop first rest

let same_member_layout (left : Aggregate_layout.member_layout)
    (right : Aggregate_layout.member_layout) =
  same_symbol left.symbol right.symbol
  && left.path = right.path
  && left.declarator_index = right.declarator_index
  && left.origin = right.origin
  && Int64.equal left.offset right.offset
  && Int64.equal left.size right.size
  && Int64.equal left.element_size right.element_size
  && left.dimensions = right.dimensions
  && left.signedness = right.signedness
  && left.alignment = right.alignment

let members_match_layout inputs layouts =
  let rec loop inputs layouts =
    match (inputs, layouts) with
    | [], [] -> true
    | input :: input_rest, layout :: layout_rest ->
        same_member_layout input.member_layout layout
        && loop input_rest layout_rest
    | [], _ :: _ | _ :: _, [] -> false
  in
  loop inputs layouts

let validate_type table origin type_ =
  match Type.base type_ with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol ->
      if Symbol_table.owns_symbol table symbol then Ok ()
      else
        Error
          (invalid_input ~origin
             "aggregate member type belongs to a different symbol table")

let rec validate_function_pointer table origin pointer =
  let rec parameters = function
    | [] -> Ok ()
    | parameter :: rest -> (
        let reference =
          Function_type_resolution.parameter_type_reference parameter
        in
        match
          validate_type table origin (Type_reference.resolved_type reference)
        with
        | Error _ as error -> error
        | Ok () -> (
            match
              Function_type_resolution.parameter_declarator_kind parameter
            with
            | Function_type_resolution.Object -> parameters rest
            | Function_type_resolution.Function_pointer nested -> (
                match validate_function_pointer table origin nested with
                | Error _ as error -> error
                | Ok () -> parameters rest)))
  in
  pointer |> Function_type_resolution.function_pointer_signature
  |> Function_type_resolution.signature_parameters |> parameters

let validate_member table scope seen_symbols aggregate_symbol input =
  let layout = input.member_layout in
  let symbol = layout.symbol in
  let key = symbol_key symbol in
  if not (Symbol_table.owns_symbol table symbol) then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member belongs to a different symbol table")
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Member) then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member index received a nonmember symbol")
  else if
    not
      (Symbol.Scope_id.equal (Symbol.scope_id symbol)
         (Symbol_table.scope_id scope))
  then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member belongs to the wrong aggregate scope")
  else if Int_set.mem key seen_symbols then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member index received the same member symbol twice")
  else if layout.path = [] then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member path cannot be empty")
  else if List.exists (fun index -> index < 0) layout.path then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member path cannot contain a negative index")
  else if layout.declarator_index < 0 then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member declarator index cannot be negative")
  else if
    not
      (same_type input.member_type
         (Type_reference.resolved_type input.member_type_reference))
  then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member type reference disagrees with its resolved type")
  else
    Result.bind (validate_type table layout.origin input.member_type) (fun () ->
        let pointer_validation =
          match input.member_function_pointer with
          | None -> Ok ()
          | Some pointer ->
              validate_function_pointer table layout.origin pointer
        in
        Result.map
          (fun () ->
            ( Int_set.add key seen_symbols,
              {
                symbol;
                declaring_aggregate = aggregate_symbol;
                member_type = input.member_type;
                member_type_reference = input.member_type_reference;
                is_function_pointer =
                  Option.is_some input.member_function_pointer;
                function_pointer = input.member_function_pointer;
                layout;
              } ))
          pointer_validation)

let validate_scope table parent layout scope =
  if not (Symbol_table.owns_scope table scope) then
    Error
      (invalid_input ~origin:layout.Aggregate_layout.origin
         "aggregate member scope belongs to a different symbol table")
  else if Symbol_table.scope_kind scope <> Symbol_table.Aggregate then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member index needs an aggregate scope")
  else if Symbol_table.scope_name scope <> Some (Symbol.name layout.symbol) then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member scope does not match the aggregate name")
  else
    match Symbol_table.parent scope with
    | Some actual
      when Symbol.Scope_id.equal
             (Symbol_table.scope_id actual)
             (Symbol_table.scope_id parent) -> Ok ()
    | None | Some _ ->
        Error
          (invalid_input ~origin:layout.origin
             "aggregate member scope does not belong to the module")

let validate_aggregate table parent previous_item_index seen_symbols input =
  let layout = input.aggregate_layout in
  let symbol = layout.symbol in
  let key = symbol_key symbol in
  if not (Symbol_table.owns_symbol table symbol) then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate belongs to a different symbol table")
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
  then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member index received a nonaggregate symbol")
  else if
    not
      (Symbol.Scope_id.equal (Symbol.scope_id symbol)
         (Symbol_table.scope_id parent))
  then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate does not belong to the module scope")
  else if layout.item_index <= previous_item_index then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member indexes must follow source order")
  else if Int_set.mem key seen_symbols then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member index received the same aggregate twice")
  else if
    match layout.base with
    | None -> false
    | Some base ->
        (not (Symbol_table.owns_symbol table base.symbol))
        || not
             (Symbol.equal_kind (Symbol.kind base.symbol) Symbol.Aggregate_type)
  then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate base belongs to another table or is not an aggregate")
  else if
    not
      (members_match_layout input.aggregate_members
         layout.Aggregate_layout.members)
  then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate member inputs do not match the completed layout")
  else if not (members_are_ordered input.aggregate_members) then
    Error
      (invalid_input ~origin:layout.origin
         "aggregate members must follow source order")
  else
    Result.map
      (fun () -> (layout.item_index, Int_set.add key seen_symbols))
      (validate_scope table parent layout input.aggregate_scope)

let validate_inputs table parent inputs =
  if not (Symbol_table.owns_scope table parent) then
    Error
      (invalid_input
         "aggregate member index module belongs to another symbol table")
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error (invalid_input "aggregate member index parent must be a module scope")
  else
    let rec loop previous_item_index seen_symbols = function
      | [] -> Ok ()
      | input :: rest ->
          Result.bind
            (validate_aggregate table parent previous_item_index seen_symbols
               input) (fun (item_index, seen_symbols) ->
              loop item_index seen_symbols rest)
    in
    loop (-1) Int_set.empty inputs

let add_members table by_symbol input =
  let layout = input.aggregate_layout in
  let rec loop seen_symbols direct_by_name members_rev = function
    | [] -> Ok (direct_by_name, List.rev members_rev)
    | member_input :: rest ->
        Result.bind
          (validate_member table input.aggregate_scope seen_symbols
             layout.symbol member_input) (fun (seen_symbols, member) ->
            let name = Symbol.name member.symbol in
            let existing =
              match String_map.find_opt name direct_by_name with
              | Some member -> Some member
              | None -> (
                  match base_symbol layout with
                  | None -> None
                  | Some base -> (
                      match Int_map.find_opt (symbol_key base) by_symbol with
                      | None -> None
                      | Some base ->
                          lookup_indexed by_symbol base name 0 |> Option.map snd
                      ))
            in
            match existing with
            | Some original when not (duplicate_name_is_permitted name) ->
                Error (duplicate_member original.symbol member.symbol)
            | None | Some _ ->
                let direct_by_name =
                  if String_map.mem name direct_by_name then direct_by_name
                  else String_map.add name member direct_by_name
                in
                loop seen_symbols direct_by_name (member :: members_rev) rest)
  in
  loop Int_set.empty String_map.empty [] input.aggregate_members

let build_aggregate table by_symbol input =
  let layout = input.aggregate_layout in
  let base = base_symbol layout in
  match base with
  | Some base when not (Int_map.mem (symbol_key base) by_symbol) ->
      Error (unresolved_base layout.symbol base)
  | None | Some _ ->
      let base_matches =
        match (layout.base, base) with
        | None, None -> true
        | Some declared, Some base -> (
            match Int_map.find_opt (symbol_key base) by_symbol with
            | None -> false
            | Some indexed ->
                Int64.equal declared.offset 0L
                && Int64.equal declared.size indexed.aggregate.layout.size)
        | None, Some _ | Some _, None -> false
      in
      if not base_matches then
        Error
          (invalid_input ~origin:layout.origin
             "aggregate base layout does not match the indexed base")
      else
        Result.map
          (fun (direct_by_name, direct_members) ->
            let aggregate =
              {
                symbol = layout.symbol;
                item_index = layout.item_index;
                base_symbol = base;
                layout;
                direct_members;
              }
            in
            { aggregate; direct_by_name })
          (add_members table by_symbol input)

let build ~table ~parent inputs =
  Result.bind (validate_inputs table parent inputs) (fun () ->
      let rec loop by_symbol aggregates_rev = function
        | [] ->
            Ok { table; parent; aggregates = List.rev aggregates_rev; by_symbol }
        | input :: rest ->
            Result.bind (build_aggregate table by_symbol input) (fun indexed ->
                loop
                  (Int_map.add
                     (symbol_key indexed.aggregate.symbol)
                     indexed by_symbol)
                  (indexed.aggregate :: aggregates_rev)
                  rest)
      in
      loop Int_map.empty [] inputs)

let aggregates result = result.aggregates
let owns_table result table = result.table == table
let owns_parent result parent = result.parent == parent
let parent_scope result = result.parent
let aggregate_symbol (aggregate : aggregate) = aggregate.symbol
let aggregate_item_index (aggregate : aggregate) = aggregate.item_index
let aggregate_size (aggregate : aggregate) = aggregate.layout.size
let lookup_queried_aggregate (lookup : lookup) = lookup.queried_aggregate
let lookup_declaring_aggregate (lookup : lookup) = lookup.declaring_aggregate
let lookup_inheritance_depth (lookup : lookup) = lookup.inheritance_depth
let lookup_member (lookup : lookup) = lookup.member
let member_symbol (member : member) = member.symbol
let member_type (member : member) = member.member_type
let member_type_reference (member : member) = member.member_type_reference
let member_is_function_pointer (member : member) = member.is_function_pointer
let member_function_pointer (member : member) = member.function_pointer
let member_layout (member : member) = member.layout

let find_indexed result symbol =
  if not (Symbol_table.owns_symbol result.table symbol) then None
  else Int_map.find_opt (symbol_key symbol) result.by_symbol

let find_aggregate result symbol =
  find_indexed result symbol |> Option.map (fun indexed -> indexed.aggregate)

let lookup result ~aggregate ~name =
  if String.equal name "" then
    Error (invalid_input "aggregate member lookup name cannot be empty")
  else
    match find_indexed result aggregate with
    | None -> Error (aggregate_not_indexed aggregate)
    | Some indexed ->
        Ok
          (lookup_indexed result.by_symbol indexed name 0
          |> Option.map (fun (inheritance_depth, (member : member)) ->
              {
                queried_aggregate = aggregate;
                declaring_aggregate = member.declaring_aggregate;
                inheritance_depth;
                member;
              }))
