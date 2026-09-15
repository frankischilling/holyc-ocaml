type segment = {
  generated_start : int;
  generated_stop : int;
  source_span : Common.Span.t;
}

type t = {
  owner : unit ref option;
  id : int;
  name : string;
  replacement : string;
  name_span : Common.Span.t;
  definition_span : Common.Span.t;
  replacement_span : Common.Span.t;
  segments : segment list;
}

let id definition = definition.id
let name definition = definition.name
let replacement definition = definition.replacement
let name_span definition = definition.name_span
let definition_span definition = definition.definition_span
let replacement_span definition = definition.replacement_span
let segments definition = definition.segments

let span_position sources span =
  match Common.Source_manager.find sources span.Common.Span.source with
  | None ->
      Printf.sprintf "source-%d:%d..%d"
        (Common.Source_id.to_int span.source)
        span.start span.stop
  | Some source -> (
      match
        ( Common.Source_file.position source span.start,
          Common.Source_file.position source span.stop )
      with
      | Ok start, Ok stop ->
          Printf.sprintf "%s:%d:%d..%d:%d"
            (Common.Source_file.display_path source)
            start.line start.column stop.line stop.column
      | _ ->
          Printf.sprintf "%s:%d..%d"
            (Common.Source_file.display_path source)
            span.start span.stop)

module Environment = struct
  module Names = Map.Make (String)

  type definition = t

  type store = {
    mutable next_id : int;
    mutable by_name : definition list Names.t;
    mutable history_rev : definition list;
  }

  type t = { store : store; owner : unit ref option }

  let create () =
    {
      store = { next_id = 0; by_name = Names.empty; history_rev = [] };
      owner = None;
    }

  let visible environment (definition : definition) =
    match (environment.owner, definition.owner) with
    | None, _ | _, None -> true
    | Some left, Some right -> left == right

  let task_view environment =
    { store = environment.store; owner = Some (ref ()) }

  let copy environment =
    {
      store =
        {
          next_id = environment.store.next_id;
          by_name =
            Names.map
              (List.filter (visible environment))
              environment.store.by_name;
          history_rev =
            List.filter (visible environment) environment.store.history_rev;
        };
      owner = environment.owner;
    }

  let define environment ~name ~replacement ~name_span ~definition_span
      ~replacement_span ~segments =
    if environment.store.next_id = max_int then
      invalid_arg "definition identity space is exhausted";
    let definition =
      {
        owner = environment.owner;
        id = environment.store.next_id;
        name;
        replacement;
        name_span;
        definition_span;
        replacement_span;
        segments;
      }
    in
    environment.store.next_id <- environment.store.next_id + 1;
    let prior =
      Option.value (Names.find_opt name environment.store.by_name) ~default:[]
    in
    environment.store.by_name <-
      Names.add name (definition :: prior) environment.store.by_name;
    environment.store.history_rev <- definition :: environment.store.history_rev;
    definition

  let find environment name =
    Option.bind
      (Names.find_opt name environment.store.by_name)
      (List.find_opt (visible environment))

  let all environment =
    List.rev environment.store.history_rev |> List.filter (visible environment)

  let dump sources environment =
    let buffer = Buffer.create 256 in
    Buffer.add_string buffer "holyc-definition-dump-v1\n";
    List.iter
      (fun definition ->
        Printf.bprintf buffer
          "definition %d name=%S definition_at=%s name_at=%s replacement_at=%s \
           bytes=%S\n"
          definition.id definition.name
          (span_position sources definition.definition_span)
          (span_position sources definition.name_span)
          (span_position sources definition.replacement_span)
          definition.replacement;
        List.iter
          (fun segment ->
            Printf.bprintf buffer "  segment generated=%d..%d source=%s\n"
              segment.generated_start segment.generated_stop
              (span_position sources segment.source_span))
          definition.segments)
      (all environment);
    Buffer.contents buffer
end
