module Members = Aggregate_member_index

type t = {
  aggregate : Members.aggregate option;
  pointer_type : Type.t;
  byte_size : int64;
  before_item_index : int;
  layout_before_item_index : int;
  function_symbol : Symbol.t option;
  index :
    (Function_aggregate_visibility.function_ * Frontend.Ast.expression * int64)
    option;
  top_level_index :
    (Function_aggregate_visibility.t * Frontend.Ast.expression * int64) option;
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
                aggregate = Some aggregate;
                pointer_type;
                byte_size;
                before_item_index;
                layout_before_item_index;
                function_symbol = None;
                index = None;
                top_level_index = None;
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
                ~item_index:
                  (Members.aggregate_item_index (Option.get layout.aggregate))
                ~symbol:(Members.aggregate_symbol (Option.get layout.aggregate))
            then
              Some
                {
                  layout with
                  function_symbol =
                    Some
                      (Function_aggregate_visibility.function_symbol visibility);
                }
            else None))

let create_index ~visibility ~source ~pointer_type ~stride =
  if Type.pointer_depth pointer_type <> 1 || stride < 0L then None
  else
    Option.bind
      (Type.dereference pointer_type |> Result.to_option)
      (fun element_type ->
        Option.bind
          (Function_aggregate_visibility.index_element_size visibility ~source
             ~element_type) (fun element_size ->
            if
              (element_size = 0L && stride <> 0L)
              || (element_size > 0L && Int64.rem stride element_size <> 0L)
            then None
            else
              match Type.base pointer_type with
              | Type.Aggregate _ ->
                  Some
                    {
                      aggregate = None;
                      pointer_type;
                      byte_size = stride;
                      before_item_index =
                        Function_aggregate_visibility.function_item_index
                          visibility;
                      layout_before_item_index =
                        Function_aggregate_visibility.function_item_index
                          visibility;
                      function_symbol =
                        Some
                          (Function_aggregate_visibility.function_symbol
                             visibility);
                      index = Some (visibility, source, element_size);
                      top_level_index = None;
                    }
              | _ -> None))

let create_top_level_index ~visibility ~source ~pointer_type ~stride =
  if Type.pointer_depth pointer_type <> 1 || stride < 0L then None
  else
    Option.bind
      (Type.dereference pointer_type |> Result.to_option)
      (fun element_type ->
        Option.bind
          (Function_aggregate_visibility.top_level_index_element_size visibility
             ~source ~element_type) (fun element_size ->
            if
              (element_size = 0L && stride <> 0L)
              || (element_size > 0L && Int64.rem stride element_size <> 0L)
            then None
            else
              Some
                {
                  aggregate = None;
                  pointer_type;
                  byte_size = stride;
                  before_item_index = 0;
                  layout_before_item_index = 0;
                  function_symbol = None;
                  index = None;
                  top_level_index = Some (visibility, source, element_size);
                }))

let matches_top_level layout ~pointer_type ~stride =
  Type.equal layout.pointer_type pointer_type
  && layout.byte_size = stride
  &&
  match layout.top_level_index with
  | None -> false
  | Some (visibility, source, element_size) ->
      Option.fold ~none:false
        ~some:(fun element_type ->
          Function_aggregate_visibility.top_level_index_element_size visibility
            ~source ~element_type
          = Some element_size)
        (Type.dereference pointer_type |> Result.to_option)

let byte_size layout = layout.byte_size

let aggregate_symbol layout =
  match Type.base layout.pointer_type with
  | Type.Aggregate symbol -> symbol
  | _ -> assert false

let matches ?function_symbol layout ~before_item_index ~pointer_type ~stride =
  layout.before_item_index = before_item_index
  && Option.fold ~none:true
       ~some:(fun owner ->
         Option.fold ~none:false ~some:(( == ) owner) function_symbol)
       layout.function_symbol
  && (match layout.index with
    | None ->
        Option.fold ~none:false
          ~some:(fun aggregate ->
            Members.aggregate_item_index aggregate
            < layout.layout_before_item_index)
          layout.aggregate
    | Some (visibility, source, element_size) ->
        Option.fold ~none:false
          ~some:(fun element_type ->
            Function_aggregate_visibility.index_element_size visibility ~source
              ~element_type
            = Some element_size)
          (Type.dereference pointer_type |> Result.to_option))
  && Type.equal layout.pointer_type pointer_type
  && layout.byte_size = stride
  &&
  match Type.base pointer_type with
  | Type.Aggregate symbol -> symbol == aggregate_symbol layout
  | _ -> false
