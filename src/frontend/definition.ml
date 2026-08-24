type segment = {
  generated_start : int;
  generated_stop : int;
  source_span : Common.Span.t;
}

type t = {
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

  type t = {
    mutable next_id : int;
    mutable current : definition Names.t;
    mutable history_rev : definition list;
  }

  let create () = { next_id = 0; current = Names.empty; history_rev = [] }

  let copy environment =
    {
      next_id = environment.next_id;
      current = environment.current;
      history_rev = environment.history_rev;
    }

  let define environment ~name ~replacement ~name_span ~definition_span
      ~replacement_span ~segments =
    if environment.next_id = max_int then
      invalid_arg "definition identity space is exhausted";
    let definition =
      {
        id = environment.next_id;
        name;
        replacement;
        name_span;
        definition_span;
        replacement_span;
        segments;
      }
    in
    environment.next_id <- environment.next_id + 1;
    environment.current <- Names.add name definition environment.current;
    environment.history_rev <- definition :: environment.history_rev;
    definition

  let find environment name = Names.find_opt name environment.current
  let all environment = List.rev environment.history_rev

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
