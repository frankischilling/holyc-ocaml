type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type owner = unit ref

type point = {
  owner : owner;
  publication : Module_expression_binding.publication;
}

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)
module String_map = Map.Make (String)

type t = {
  owner : owner;
  table : Symbol_table.t;
  environment : Outer_environment.t;
  expressions : Module_expression_binding.t;
  points : point list;
  by_source_symbol : point Int_map.t;
}

type cursor = {
  owner : owner;
  environment : Outer_environment.t;
  visible : Module_expression_binding.publication String_map.t;
  remaining : point list;
}

let symbol_number symbol = Symbol.id symbol |> Symbol.Id.to_int

let collect_points owner table expressions =
  let rec collect expected_index previous_item seen points_rev by_symbol =
    function
    | [] -> Ok (List.rev points_rev, by_symbol)
    | publication :: rest ->
        let source =
          Module_expression_binding.publication_source_symbol publication
        in
        let canonical =
          Module_expression_binding.publication_canonical_symbol publication
        in
        let source_number = symbol_number source in
        let declaration_index =
          Module_expression_binding.publication_declaration_index publication
        in
        let item_index =
          Module_expression_binding.publication_item_index publication
        in
        if declaration_index <> expected_index then
          Error "module publication declaration indexes are not contiguous"
        else if item_index < previous_item then
          Error "module publications do not follow source item order"
        else if Int_set.mem source_number seen then
          Error "module publication source symbol is repeated"
        else if
          not
            (Symbol_table.owns_symbol table source
            && Symbol_table.owns_symbol table canonical)
        then Error "module publication belongs to another symbol table"
        else
          let point = { owner; publication } in
          collect (expected_index + 1) item_index
            (Int_set.add source_number seen)
            (point :: points_rev)
            (Int_map.add source_number point by_symbol)
            rest
  in
  collect 0 (-1) Int_set.empty [] Int_map.empty
    (Module_expression_binding.publications expressions)

let create ~table ~environment ~expressions =
  if not (Module_expression_binding.owns_table expressions table) then
    Error "module expression bindings belong to another symbol table"
  else if not (Outer_environment.owns_table environment table) then
    Error "outer environment belongs to another symbol table"
  else if
    Module_expression_binding.compilation_mode expressions
    <> Outer_environment.compilation_mode environment
  then
    Error
      "module expression bindings and outer environment use different \
       compilation modes"
  else
    let owner = ref () in
    match collect_points owner table expressions with
    | Error _ as error -> error
    | Ok (points, by_source_symbol) ->
        Ok { owner; table; environment; expressions; points; by_source_symbol }

let table (state : t) = state.table
let environment (state : t) = state.environment
let expressions (state : t) = state.expressions
let owns_table (state : t) table = state.table == table
let point_publication (point : point) = point.publication

let find_point state symbol =
  if not (Symbol_table.owns_symbol state.table symbol) then None
  else
    match Int_map.find_opt (symbol_number symbol) state.by_source_symbol with
    | Some point
      when Symbol.Id.equal
             (Symbol.id
                (Module_expression_binding.publication_source_symbol
                   point.publication))
             (Symbol.id symbol) -> Some point
    | Some _ | None -> None

let initial_cursor (state : t) =
  {
    owner = state.owner;
    environment = state.environment;
    visible = String_map.empty;
    remaining = state.points;
  }

let add_point visible point =
  let publication = point.publication in
  String_map.add
    (Module_expression_binding.publication_source_symbol publication
    |> Symbol.name)
    publication visible

let rec publish_while predicate visible = function
  | point :: rest when predicate point.publication ->
      publish_while predicate (add_point visible point) rest
  | remaining -> (visible, remaining)

let advance comparison (cursor : cursor) (point : point) =
  if cursor.owner != point.owner then
    Error "module publication cursor and point belong to different environments"
  else
    let boundary =
      Module_expression_binding.publication_declaration_index point.publication
    in
    let next_boundary =
      match cursor.remaining with
      | next :: _ ->
          Module_expression_binding.publication_declaration_index
            next.publication
      | [] -> max_int
    in
    if boundary < next_boundary then
      Error "module publication cursor has already passed the selected point"
    else
      let visible, remaining =
        publish_while
          (fun publication ->
            comparison
              (Module_expression_binding.publication_declaration_index
                 publication)
              boundary)
          cursor.visible cursor.remaining
      in
      Ok { cursor with visible; remaining }

let publish_before cursor point = advance ( < ) cursor point
let publish_through cursor point = advance ( <= ) cursor point

let resolve cursor name =
  match String_map.find_opt name cursor.visible with
  | Some publication -> Some (Module_binding publication)
  | None ->
      Outer_environment.find cursor.environment name
      |> Option.map (fun binding -> Outer_binding binding)
