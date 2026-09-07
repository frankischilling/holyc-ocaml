type primitive_form = Public_spelling | Internal_storage

type base =
  | Primitive of primitive_form * Primitive_type.t
  | Aggregate of Symbol.t

type t = { base : base; pointer_depth : int }

let max_pointer_depth = 4

let validate_pointer_depth pointer_depth =
  if pointer_depth < 0 then Error "semantic pointer depth cannot be negative"
  else if pointer_depth > max_pointer_depth then
    Error
      (Printf.sprintf "semantic pointer depth %d exceeds HolyC's limit of %d"
         pointer_depth max_pointer_depth)
  else Ok ()

let primitive_has_internal_storage_spelling primitive =
  let storage_spelling = (Primitive_type.info primitive).storage_spelling in
  match Primitive_type.of_storage_spelling storage_spelling with
  | Some storage_primitive -> Primitive_type.equal primitive storage_primitive
  | None -> false

let make_primitive ~form ~primitive ~pointer_depth =
  match validate_pointer_depth pointer_depth with
  | Error _ as error -> error
  | Ok () -> (
      match form with
      | Internal_storage
        when not (primitive_has_internal_storage_spelling primitive) ->
          Error
            (Printf.sprintf "%s has no distinct intrinsic storage spelling"
               (Primitive_type.to_string primitive))
      | Public_spelling | Internal_storage ->
          Ok { base = Primitive (form, primitive); pointer_depth })

let make_aggregate ~symbol ~pointer_depth =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type) then
    Error "semantic aggregate type requires an aggregate-type symbol"
  else
    Result.map
      (fun () -> { base = Aggregate symbol; pointer_depth })
      (validate_pointer_depth pointer_depth)

let base type_ = type_.base
let pointer_depth type_ = type_.pointer_depth

let equal left right =
  left.pointer_depth = right.pointer_depth
  &&
  match (left.base, right.base) with
  | ( Primitive (left_form, left_primitive),
      Primitive (right_form, right_primitive) ) ->
      left_form = right_form
      && Primitive_type.equal left_primitive right_primitive
  | Aggregate left_symbol, Aggregate right_symbol -> left_symbol == right_symbol
  | Primitive _, Aggregate _ | Aggregate _, Primitive _ -> false

let pointer_to type_ =
  let pointer_depth = type_.pointer_depth + 1 in
  Result.map
    (fun () -> { type_ with pointer_depth })
    (validate_pointer_depth pointer_depth)

let dereference type_ =
  if type_.pointer_depth = 0 then
    Error "cannot dereference a type with no pointer layer"
  else Ok { type_ with pointer_depth = type_.pointer_depth - 1 }

let primitive_form_name = function
  | Public_spelling -> "public-spelling"
  | Internal_storage -> "internal-storage"
