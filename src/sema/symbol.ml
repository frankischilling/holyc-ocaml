module Id = struct
  type t = int

  let of_int value =
    if value < 0 then invalid_arg "semantic symbol ID cannot be negative";
    value

  let to_int value = value
  let compare = Int.compare
  let equal left right = compare left right = 0
end

module Scope_id = struct
  type t = int

  let of_int value =
    if value < 0 then invalid_arg "semantic scope ID cannot be negative";
    value

  let to_int value = value
  let compare = Int.compare
  let equal left right = compare left right = 0
end

type kind =
  | Internal_type
  | Aggregate_type
  | Function
  | Global_variable
  | Parameter
  | Local_variable
  | Member
  | Label
  | Assembler_symbol
  | Module
  | Generated

type source_origin = {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type origin =
  | Pinned_source of { path : string; line : int }
  | Source_location of source_origin
  | Synthesized of string

type t = {
  id : Id.t;
  scope_id : Scope_id.t;
  name : string;
  kind : kind;
  origin : origin;
}

let create ~id ~scope_id ~name ~kind ~origin =
  if String.equal name "" then
    invalid_arg "semantic symbol name cannot be empty";
  (match origin with
  | Pinned_source { path; line } ->
      if String.equal path "" then
        invalid_arg "pinned semantic symbol path cannot be empty";
      if line < 1 then
        invalid_arg "pinned semantic symbol line must be positive"
  | Source_location _ -> ()
  | Synthesized description ->
      if String.equal description "" then
        invalid_arg "synthesized semantic symbol origin cannot be empty");
  { id; scope_id; name; kind; origin }

let id symbol = symbol.id
let scope_id symbol = symbol.scope_id
let name symbol = symbol.name
let kind symbol = symbol.kind
let origin symbol = symbol.origin

let kind_rank = function
  | Internal_type -> 0
  | Aggregate_type -> 1
  | Function -> 2
  | Global_variable -> 3
  | Parameter -> 4
  | Local_variable -> 5
  | Member -> 6
  | Label -> 7
  | Assembler_symbol -> 8
  | Module -> 9
  | Generated -> 10

let compare_kind left right = Int.compare (kind_rank left) (kind_rank right)
let equal_kind left right = compare_kind left right = 0

let kind_name = function
  | Internal_type -> "internal-type"
  | Aggregate_type -> "aggregate-type"
  | Function -> "function"
  | Global_variable -> "global-variable"
  | Parameter -> "parameter"
  | Local_variable -> "local-variable"
  | Member -> "member"
  | Label -> "label"
  | Assembler_symbol -> "assembler-symbol"
  | Module -> "module"
  | Generated -> "generated"

let reference_commit = Generated.Opcode_keywords.reference_commit
