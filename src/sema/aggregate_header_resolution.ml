type backing_site = {
  spelling : string;
  origin : Symbol.origin;
  spelling_origin : Symbol.origin;
  pointer_origins : Symbol.origin list;
  resolved_type : Type.t;
}

type base_site = {
  spelling : string;
  origin : Symbol.origin;
  colon_origin : Symbol.origin;
  name_origin : Symbol.origin;
  symbol : Symbol.t;
}

type header = {
  symbol : Symbol.t;
  aggregate_kind : Aggregate_resolution.aggregate_kind;
  item_index : int;
  origin : Symbol.origin;
  keyword_origin : Symbol.origin;
  backing : backing_site option;
  base : base_site option;
}

type t = { headers : header list }

let headers resolution = resolution.headers
let header_symbol header = header.symbol
let header_aggregate_kind header = header.aggregate_kind
let header_item_index header = header.item_index
let header_origin header = header.origin
let header_keyword_origin header = header.keyword_origin
let header_backing header = header.backing
let header_base header = header.base
let backing_spelling (backing : backing_site) = backing.spelling
let backing_origin (backing : backing_site) = backing.origin
let backing_spelling_origin (backing : backing_site) = backing.spelling_origin
let backing_pointer_origins (backing : backing_site) = backing.pointer_origins
let backing_type (backing : backing_site) = backing.resolved_type
let base_spelling (base : base_site) = base.spelling
let base_origin (base : base_site) = base.origin
let base_colon_origin (base : base_site) = base.colon_origin
let base_name_origin (base : base_site) = base.name_origin
let base_symbol (base : base_site) = base.symbol

let expected_backing_spelling resolved_type =
  match Type.base resolved_type with
  | Type.Primitive (Type.Public_spelling, primitive) ->
      Primitive_type.to_string primitive
  | Type.Primitive (Type.Internal_storage, primitive) ->
      (Primitive_type.info primitive).storage_spelling
  | Type.Aggregate symbol -> Symbol.name symbol

let make_backing_site ~spelling ~origin ~spelling_origin ~pointer_origins
    ~resolved_type =
  if String.equal spelling "" then
    Error "semantic aggregate backing spelling cannot be empty"
  else if List.length pointer_origins <> Type.pointer_depth resolved_type then
    Error
      "semantic aggregate backing pointer provenance does not match its type"
  else
    let expected = expected_backing_spelling resolved_type in
    if not (String.equal spelling expected) then
      Error
        (Printf.sprintf
           "semantic aggregate backing spelling %S does not match %S" spelling
           expected)
    else
      Ok { spelling; origin; spelling_origin; pointer_origins; resolved_type }

let make_base_site ~spelling ~origin ~colon_origin ~name_origin ~symbol =
  if String.equal spelling "" then
    Error "semantic aggregate base spelling cannot be empty"
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
  then Error "semantic aggregate base requires an aggregate-type symbol"
  else if not (String.equal spelling (Symbol.name symbol)) then
    Error
      (Printf.sprintf "semantic aggregate base spelling %S does not match %S"
         spelling (Symbol.name symbol))
  else Ok { spelling; origin; colon_origin; name_origin; symbol }

let make_header ~symbol ~aggregate_kind ~item_index ~origin ~keyword_origin
    ~backing ~base =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type) then
    Error "semantic aggregate header requires an aggregate-type symbol"
  else if item_index < 0 then
    Error "semantic aggregate header item index cannot be negative"
  else
    Ok
      {
        symbol;
        aggregate_kind;
        item_index;
        origin;
        keyword_origin;
        backing;
        base;
      }

module Int_set = Set.Make (Int)

let validate_aggregate_symbol ~table ~parent ~role symbol =
  if not (Symbol_table.owns_symbol table symbol) then
    Error
      (Printf.sprintf
         "semantic aggregate %s belongs to a different symbol table" role)
  else if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
  then
    Error
      (Printf.sprintf "semantic aggregate %s is not an aggregate-type symbol"
         role)
  else if
    not
      (Symbol.Scope_id.equal (Symbol.scope_id symbol)
         (Symbol_table.scope_id parent))
  then
    Error
      (Printf.sprintf
         "semantic aggregate %s does not belong to the module scope" role)
  else Ok ()

let validate_backing ~table ~parent backing =
  match Type.base backing.resolved_type with
  | Type.Primitive _ -> Ok ()
  | Type.Aggregate symbol ->
      validate_aggregate_symbol ~table ~parent ~role:"backing target" symbol

let validate_header ~table ~parent ~previous_item_index ~seen header =
  let symbol_id = Symbol.Id.to_int (Symbol.id header.symbol) in
  match
    validate_aggregate_symbol ~table ~parent ~role:"header" header.symbol
  with
  | Error _ as error -> error
  | Ok () when header.item_index <= previous_item_index ->
      Error "semantic aggregate headers must be in increasing item order"
  | Ok () when Int_set.mem symbol_id seen ->
      Error "semantic aggregate header symbols cannot repeat"
  | Ok () -> (
      match header.backing with
      | Some backing -> (
          match validate_backing ~table ~parent backing with
          | Error _ as error -> error
          | Ok () -> (
              match header.base with
              | None -> Ok (header.item_index, Int_set.add symbol_id seen)
              | Some base ->
                  Result.map
                    (fun () -> (header.item_index, Int_set.add symbol_id seen))
                    (validate_aggregate_symbol ~table ~parent
                       ~role:"base target" base.symbol)))
      | None -> (
          match header.base with
          | None -> Ok (header.item_index, Int_set.add symbol_id seen)
          | Some base ->
              Result.map
                (fun () -> (header.item_index, Int_set.add symbol_id seen))
                (validate_aggregate_symbol ~table ~parent ~role:"base target"
                   base.symbol)))

let resolve ~table ~parent headers =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic aggregate header parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic aggregate header parent must be a module scope"
  else
    let rec validate previous_item_index seen = function
      | [] -> Ok { headers }
      | header :: rest -> (
          match
            validate_header ~table ~parent ~previous_item_index ~seen header
          with
          | Error _ as error -> error
          | Ok (item_index, seen) -> validate item_index seen rest)
    in
    validate (-1) Int_set.empty headers
