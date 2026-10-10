module Members = Aggregate_member_index

type t = {
  aggregate : Members.aggregate;
  pointer_type : Type.t;
  byte_size : int64;
  before_item_index : int;
  layout_before_item_index : int;
  function_symbol : Symbol.t option;
}

let create_at ~members ~before_item_index ~layout_before_item_index
    ~pointer_type =
  match Type.base pointer_type with
  | Type.Aggregate symbol when Type.pointer_depth pointer_type = 1 ->
      Option.bind (Members.find_aggregate members symbol) (fun aggregate ->
          let byte_size = Members.aggregate_size aggregate in
          if
            Members.aggregate_symbol aggregate == symbol
            && Members.aggregate_item_index aggregate < layout_before_item_index
            && byte_size > 0L
            && byte_size <= Int64.of_int Int.max_int
          then
            Some
              {
                aggregate;
                pointer_type;
                byte_size;
                before_item_index;
                layout_before_item_index;
                function_symbol = None;
              }
          else None)
  | _ -> None

let create ~members ~before_item_index ~pointer_type =
  create_at ~members ~before_item_index
    ~layout_before_item_index:before_item_index ~pointer_type

let create_visible ~visibility ~source ~members ~pointer_type =
  let table = Function_aggregate_visibility.function_table visibility in
  if
    (not (Members.owns_table members table))
    || not
         (Function_aggregate_visibility.function_owns visibility ~table
            ~parent:(Members.parent_scope members))
  then None
  else
    Option.bind
      (Function_aggregate_visibility.before_expression visibility source)
      (fun layout_before_item_index ->
        Option.bind
          (create_at ~members
             ~before_item_index:
               (Function_aggregate_visibility.function_item_index visibility)
             ~layout_before_item_index ~pointer_type)
          (fun layout ->
            if
              Function_aggregate_visibility.permits_aggregate visibility ~source
                ~item_index:(Members.aggregate_item_index layout.aggregate)
                ~symbol:(Members.aggregate_symbol layout.aggregate)
            then
              Some
                {
                  layout with
                  function_symbol =
                    Some
                      (Function_aggregate_visibility.function_symbol visibility);
                }
            else None))

let byte_size layout = layout.byte_size
let aggregate_symbol layout = Members.aggregate_symbol layout.aggregate

let matches ?function_symbol layout ~before_item_index ~pointer_type ~stride =
  layout.before_item_index = before_item_index
  && Option.fold ~none:true
       ~some:(fun owner ->
         Option.fold ~none:false ~some:(( == ) owner) function_symbol)
       layout.function_symbol
  && Members.aggregate_item_index layout.aggregate
     < layout.layout_before_item_index
  && Type.equal layout.pointer_type pointer_type
  && layout.byte_size = stride
  &&
  match Type.base pointer_type with
  | Type.Aggregate symbol -> symbol == Members.aggregate_symbol layout.aggregate
  | _ -> false
