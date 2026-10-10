module Members = Aggregate_member_index

type t = {
  aggregate : Members.aggregate;
  pointer_type : Type.t;
  byte_size : int64;
  before_item_index : int;
}

let create ~members ~before_item_index ~pointer_type =
  match Type.base pointer_type with
  | Type.Aggregate symbol when Type.pointer_depth pointer_type = 1 ->
      Option.bind (Members.find_aggregate members symbol) (fun aggregate ->
          let byte_size = Members.aggregate_size aggregate in
          if
            Members.aggregate_symbol aggregate == symbol
            && Members.aggregate_item_index aggregate < before_item_index
            && Option.is_none aggregate.base_symbol
            && byte_size > 0L
            && byte_size <= Int64.of_int Int.max_int
          then Some { aggregate; pointer_type; byte_size; before_item_index }
          else None)
  | _ -> None

let byte_size layout = layout.byte_size
let aggregate_symbol layout = Members.aggregate_symbol layout.aggregate

let matches layout ~before_item_index ~pointer_type ~stride =
  layout.before_item_index = before_item_index
  && Members.aggregate_item_index layout.aggregate < before_item_index
  && Type.equal layout.pointer_type pointer_type
  && layout.byte_size = stride
  &&
  match Type.base pointer_type with
  | Type.Aggregate symbol -> symbol == Members.aggregate_symbol layout.aggregate
  | _ -> false
