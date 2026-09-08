module Layout = Integer_initializer_layout

type payload = Word of int64 | Bytes of string

type 'root entry = {
  root : 'root;
  destination : Layout.entry;
  prepared : (payload * int) option;
}

type 'root t = { entries : 'root entry list; width : int; steps : int }

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

let create ~shape ~source ~roots ~source_leaf =
  let* layout = Layout.create ~shape source in
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
  Ok
    {
      entries;
      width =
        Integer_storage_shape.byte_size shape
        / Integer_storage_shape.element_count shape;
      steps = 0;
    }

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
              | Layout.Scalar_store, Word _ -> Ok ()
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
              | Word bits when initializers.width = 1 ->
                  Word (Int64.logand bits 255L)
              | _ -> payload
            in
            { entry with prepared = Some (payload, steps) })
      initializers.entries
  in
  Ok { initializers with entries; steps }
