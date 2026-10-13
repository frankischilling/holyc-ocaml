module Layout = Integer_initializer_layout

type payload = Word of int64 | Bytes of string

type 'root entry = {
  root : 'root;
  destination : Layout.entry;
  prepared : (payload * int) option;
}

type 'root t = {
  entries : 'root entry list;
  scalar : Integer_scalar_storage.t;
  steps : int;
}

let entries initializers = initializers.entries
let root entry = entry.root
let destination entry = entry.destination
let prepared entry = entry.prepared

let find initializers root =
  List.find_opt (fun entry -> entry.root == root) initializers.entries

let steps initializers = initializers.steps

let has_unprepared initializers =
  List.exists (fun entry -> entry.prepared = None) initializers.entries

let ( let* ) = Result.bind
let invalid detail = Error ("HCIRL0004: array initializer " ^ detail)

let create_impl ~source_layout ~static_allocation ~retained_layout ~shape
    ~source ~roots ~source_leaf =
  let* layout =
    match (source_layout, retained_layout) with
    | Some layout, None when Layout.source layout == source -> Ok layout
    | Some _, _ -> invalid "source layout has another original initializer"
    | None, None -> (
        match static_allocation with
        | None -> Layout.create ~shape source
        | Some allocation ->
            Integer_static_allocation.retained_layout allocation ~shape source)
    | None, Some (declaration, layout)
      when Layout.source layout == source
           && Layout.belongs_to_declared_shape layout declaration shape ->
        Ok layout
    | None, Some _ ->
        invalid "retained layout has another declaration, source or extent"
  in
  let rec join reversed roots destinations =
    match (roots, destinations) with
    | [], [] -> Ok (List.rev reversed)
    | root :: rest, destination :: tail ->
        if
          not
            (Option.fold ~none:false
               ~some:(fun leaf -> leaf == Layout.leaf destination)
               (source_leaf root))
        then invalid "root does not match its exact ordered source leaf"
        else join ({ root; destination; prepared = None } :: reversed) rest tail
    | _ -> invalid "roots do not cover its complete source initializer"
  in
  let* entries = join [] roots (Layout.entries layout) in
  Ok { entries; scalar = Integer_storage_shape.scalar shape; steps = 0 }

let create ~shape ~source ~roots ~source_leaf =
  create_impl ~source_layout:None ~static_allocation:None ~retained_layout:None
    ~shape ~source ~roots ~source_leaf

let create_static_snapshot ~snapshot ~frame ~location ~shape ~source ~roots
    ~source_leaf =
  let* source_layout =
    Layout.static_snapshot_layout snapshot ~frame ~location ~shape ~source
  in
  create_impl ~source_layout:(Some source_layout) ~static_allocation:None
    ~retained_layout:None ~shape ~source ~roots ~source_leaf

let create_retained ~declaration ~layout ~shape ~source ~roots ~source_leaf =
  create_impl ~source_layout:None ~static_allocation:None
    ~retained_layout:(Some (declaration, layout))
    ~shape ~source ~roots ~source_leaf

let create_retained_static ~allocation ~shape ~source ~roots ~source_leaf =
  create_impl ~source_layout:None ~static_allocation:(Some allocation)
    ~retained_layout:None ~shape ~source ~roots ~source_leaf

let storage_payload entry payload =
  match (payload, Layout.scalar entry.destination) with
  | Word bits, Some scalar ->
      Bytes (Layout.word_bytes scalar bits |> Result.get_ok)
  | _ -> payload

let publish initializers updates =
  let rec validate total seen = function
    | [] -> Ok total
    | (root, payload, steps) :: rest ->
        begin match find initializers root with
        | Some entry
          when entry.prepared = None && steps > 0
               && steps <= Int.max_int - total
               && not (List.exists (( == ) root) seen) ->
            let* () =
              match (Layout.operation entry.destination, payload) with
              | Layout.Scalar_store, Word bits -> (
                  match Layout.scalar entry.destination with
                  | None -> Ok ()
                  | Some scalar ->
                      Layout.normalized_word scalar bits
                      |> Result.map (fun _ -> ()))
              | Layout.Copy_bytes expected, Bytes actual
                when String.equal expected actual -> Ok ()
              | _ ->
                  invalid "prepared payload disagrees with its source operation"
            in
            validate (total + steps) (root :: seen) rest
        | _ -> invalid "publication has a foreign, duplicate or prepared root"
        end
  in
  let* steps = validate initializers.steps [] updates in
  let entries =
    List.map
      (fun entry ->
        match
          List.find_opt (fun (root, _, _) -> root == entry.root) updates
        with
        | None -> entry
        | Some (_, payload, steps) ->
            let payload =
              match payload with
              | Word bits ->
                  Word
                    (match Layout.scalar entry.destination with
                    | None ->
                        Integer_scalar_storage.normalize initializers.scalar
                          bits
                    | Some scalar ->
                        Layout.normalized_word scalar bits |> Result.get_ok)
              | _ -> payload
            in
            { entry with prepared = Some (payload, steps) })
      initializers.entries
  in
  Ok { initializers with entries; steps }
