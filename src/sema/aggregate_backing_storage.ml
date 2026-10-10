type t = {
  aggregate : Aggregate_member_index.aggregate;
  source_type : Type.t;
  value_type : Type.t;
  default_class : Aggregate_member_index.aggregate option;
  before_item_index : int;
  function_ : Function_call_conversion_policy.resolved_function;
}

let scalar_type ~members ~before_item_index forwarded =
  match Type.base forwarded with
  | Type.Aggregate symbol when Type.pointer_depth forwarded = 0 ->
      Option.bind (Aggregate_member_index.find_aggregate members symbol)
        (fun aggregate ->
          if
            Aggregate_member_index.aggregate_symbol aggregate == symbol
            && Aggregate_member_index.aggregate_item_index aggregate
               < before_item_index
          then
            (* PrsClassNew defaults to RT_PTR, which aliases signed RT_I64.
               Inheritance does not change that raw type. *)
            Type.make_primitive ~form:Type.Internal_storage
              ~primitive:Primitive_type.I64 ~pointer_depth:0
            |> Result.to_option
            |> Option.map (fun value_type -> (value_type, Some aggregate))
          else None)
  | _ -> Some (forwarded, None)

let value_class ~table ~members ~policies ~before_item_index ~source_type =
  if
    (not (Aggregate_member_index.owns_table members table))
    || (not (Function_call_conversion_policy.owns_table policies table))
    || not
         (Function_call_conversion_policy.owns_parent policies
            (Aggregate_member_index.parent_scope members))
  then None
  else
    match Type.base source_type with
    | Type.Aggregate symbol when Type.pointer_depth source_type = 0 ->
        Option.bind (Aggregate_member_index.find_aggregate members symbol)
          (fun aggregate ->
            if
              Aggregate_member_index.aggregate_symbol aggregate != symbol
              || Aggregate_member_index.aggregate_item_index aggregate
                 >= before_item_index
            then None
            else
              let selected =
                Function_call_conversion_policy.forwarded_type policies
                  ~before_item_index source_type
                |> scalar_type ~members ~before_item_index
              in
              Option.bind selected (fun (value_type, default_class) ->
                  if Type.pointer_depth value_type <> 0 then None
                  else
                    match Type.base value_type with
                    | Type.Primitive (_, primitive)
                      when Option.is_some
                             (Primitive_type.integer_storage_info primitive) ->
                        Some (aggregate, value_type, default_class)
                    | _ -> None))
    | _ -> None

let integer_value_type ~table ~members ~policies ~before_item_index ~source_type
    =
  value_class ~table ~members ~policies ~before_item_index ~source_type
  |> Option.map (fun (_, value_type, _) -> value_type)

let create ~table ~members ~policies ~before_item_index ~source_type =
  Option.bind
    (value_class ~table ~members ~policies ~before_item_index ~source_type)
    (fun (aggregate, value_type, default_class) ->
      Function_call_conversion_policy.functions policies
      |> List.find_opt (fun function_ ->
          Function_call_conversion_policy.function_item_index function_
          = before_item_index)
      |> Option.map (fun function_ ->
          {
            aggregate;
            source_type;
            value_type;
            default_class;
            before_item_index;
            function_;
          }))

let source_type storage = storage.source_type
let value_type storage = storage.value_type

let aggregate_symbol storage =
  Aggregate_member_index.aggregate_symbol storage.aggregate

let matches storage ~function_symbol ~function_scope ~before_item_index
    ~source_type ~value_type =
  storage.before_item_index = before_item_index
  && Function_call_conversion_policy.function_symbol storage.function_
     == function_symbol
  && Symbol.Scope_id.equal
       (Function_call_conversion_policy.function_scope storage.function_
       |> Symbol_table.scope_id)
       function_scope
  && Aggregate_member_index.aggregate_item_index storage.aggregate
     < before_item_index
  && Option.fold ~none:true
       ~some:(fun aggregate ->
         Aggregate_member_index.aggregate_item_index aggregate
         < before_item_index)
       storage.default_class
  && Type.equal storage.source_type source_type
  && Type.equal storage.value_type value_type
  &&
  match Type.base source_type with
  | Type.Aggregate symbol -> symbol == aggregate_symbol storage
  | _ -> false
