type kind =
  | Export_system_symbol
  | Import_system_symbol
  | Definition
  | Global_variable
  | Class
  | Internal_type
  | Function
  | Word
  | Dictionary_word
  | Keyword
  | Assembly_keyword
  | Opcode
  | Register
  | File
  | Module
  | Help_file
  | Frame_pointer

type source_origin = {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type origin =
  | Pinned_source of { path : string; line : int }
  | Source_span of Common.Span.t
  | Source_location of source_origin
  | Session_registration

type parameter_call_shape = {
  parameter_name : string option;
  has_default : bool;
}

type function_call_shape = {
  parameters : parameter_call_shape list;
  variadic : bool;
}

type entry = {
  id : int;
  name : string;
  kind : kind;
  origin : origin;
  function_call_shape : function_call_shape option;
}

let id entry = entry.id
let name entry = entry.name
let kind entry = entry.kind
let origin entry = entry.origin
let function_call_shape entry = entry.function_call_shape

let kind_name = function
  | Export_system_symbol -> "export-system-symbol"
  | Import_system_symbol -> "import-system-symbol"
  | Definition -> "definition"
  | Global_variable -> "global-variable"
  | Class -> "class"
  | Internal_type -> "internal-type"
  | Function -> "function"
  | Word -> "word"
  | Dictionary_word -> "dictionary-word"
  | Keyword -> "keyword"
  | Assembly_keyword -> "assembly-keyword"
  | Opcode -> "opcode"
  | Register -> "register"
  | File -> "file"
  | Module -> "module"
  | Help_file -> "help-file"
  | Frame_pointer -> "frame-pointer"

let kind_bit = function
  | Export_system_symbol -> 0x00001
  | Import_system_symbol -> 0x00002
  | Definition -> 0x00004
  | Global_variable -> 0x00008
  | Class -> 0x00010
  | Internal_type -> 0x00020
  | Function -> 0x00040
  | Word -> 0x00080
  | Dictionary_word -> 0x00100
  | Keyword -> 0x00200
  | Assembly_keyword -> 0x00400
  | Opcode -> 0x00800
  | Register -> 0x01000
  | File -> 0x02000
  | Module -> 0x04000
  | Help_file -> 0x08000
  | Frame_pointer -> 0x10000

type lookup = Absent | Present of entry | Shadowed_by_local

module String_set = Set.Make (String)

module Environment = struct
  type local_context = int

  type t = {
    entries_by_name : (string, entry list) Hashtbl.t;
    mutable entries_rev : entry list;
    mutable next_entry_id : int;
    mutable local_contexts : (local_context * String_set.t) list;
    mutable next_local_context_id : int;
  }

  let create () =
    {
      entries_by_name = Hashtbl.create 128;
      entries_rev = [];
      next_entry_id = 0;
      local_contexts = [];
      next_local_context_id = 0;
    }

  let add ?(origin = Session_registration) ?function_call_shape environment
      ~name ~kind () =
    if String.length name = 0 then invalid_arg "symbol name cannot be empty";
    if Option.is_some function_call_shape && kind <> Function then
      invalid_arg "only function symbols may carry a function call shape";
    if environment.next_entry_id = max_int then
      invalid_arg "symbol visibility identity space is exhausted";
    let entry =
      {
        id = environment.next_entry_id;
        name;
        kind;
        origin;
        function_call_shape;
      }
    in
    environment.next_entry_id <- environment.next_entry_id + 1;
    let existing =
      Option.value
        (Hashtbl.find_opt environment.entries_by_name name)
        ~default:[]
    in
    Hashtbl.replace environment.entries_by_name name (entry :: existing);
    environment.entries_rev <- entry :: environment.entries_rev;
    entry

  let local_shadow environment name =
    List.exists
      (fun (_, names) -> String_set.mem name names)
      environment.local_contexts

  let preprocessor_mask = 0x1ffff land lnot (kind_bit Import_system_symbol)

  let find_preprocessor environment name =
    if local_shadow environment name then Shadowed_by_local
    else
      match Hashtbl.find_opt environment.entries_by_name name with
      | None -> Absent
      | Some entries -> (
          match
            List.find_opt
              (fun entry -> kind_bit entry.kind land preprocessor_mask <> 0)
              entries
          with
          | Some entry -> Present entry
          | None -> Absent)

  let all environment = List.rev environment.entries_rev

  let begin_local_context environment =
    if environment.next_local_context_id = max_int then
      invalid_arg "local symbol context identity space is exhausted";
    let context = environment.next_local_context_id in
    environment.next_local_context_id <- context + 1;
    environment.local_contexts <-
      (context, String_set.empty) :: environment.local_contexts;
    context

  let add_local environment context ~name =
    match environment.local_contexts with
    | (current, names) :: rest when current = context ->
        if String.length name = 0 then Error "local symbol name cannot be empty"
        else (
          environment.local_contexts <-
            (current, String_set.add name names) :: rest;
          Ok ())
    | _ -> Error "local symbol context is not active"

  let end_local_context environment context =
    match environment.local_contexts with
    | (current, _) :: rest when current = context ->
        environment.local_contexts <- rest;
        Ok ()
    | _ -> Error "local symbol contexts must end in stack order"

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

  let origin_text sources = function
    | Pinned_source { path; line } -> Printf.sprintf "%s:%d" path line
    | Source_span span -> span_position sources span
    | Source_location source -> span_position sources source.span
    | Session_registration -> "<session>"

  let print_source_origin sources buffer source =
    List.iteri
      (fun index span ->
        Printf.bprintf buffer "  source-segment %d location=%s\n" index
          (span_position sources span))
      source.source_segments;
    Option.iter
      (fun span ->
        Printf.bprintf buffer "  generated-from=%s\n"
          (span_position sources span))
      source.generated_from;
    Option.iter
      (fun span ->
        Printf.bprintf buffer "  defined-at=%s\n" (span_position sources span))
      source.defined_at

  let print_call_shape buffer shape =
    Printf.bprintf buffer "  call-shape fixed=%d variadic=%b\n"
      (List.length shape.parameters)
      shape.variadic;
    List.iteri
      (fun index parameter ->
        let name =
          match parameter.parameter_name with
          | None -> "none"
          | Some name -> Printf.sprintf "%S" name
        in
        Printf.bprintf buffer "    parameter %d name=%s default=%b\n" index name
          parameter.has_default)
      shape.parameters

  let selected_entries source_only environment =
    all environment
    |> List.filter (fun entry ->
        (not source_only)
        ||
        match entry.origin with
        | Source_span _ | Source_location _ -> true
        | Pinned_source _ | Session_registration -> false)

  let human ?(source_only = false) sources environment =
    let buffer = Buffer.create 512 in
    Buffer.add_string buffer "holyc-symbol-visibility-v2\n";
    Printf.bprintf buffer "reference_commit=%s\n"
      Generated.Opcode_keywords.reference_commit;
    List.iter
      (fun entry ->
        Printf.bprintf buffer "symbol %d name=%S kind=%s origin=%s\n" entry.id
          entry.name (kind_name entry.kind)
          (origin_text sources entry.origin);
        (match entry.origin with
        | Source_location source -> print_source_origin sources buffer source
        | Pinned_source _ | Source_span _ | Session_registration -> ());
        Option.iter (print_call_shape buffer) entry.function_call_shape)
      (selected_entries source_only environment);
    List.rev environment.local_contexts
    |> List.iter (fun (context, names) ->
        Printf.bprintf buffer "local-context %d\n" context;
        String_set.elements names
        |> List.iter (fun name ->
            Printf.bprintf buffer "  local name=%S\n" name));
    buffer |> Buffer.contents

  let span_to_yojson sources span =
    let fields =
      [
        ("source_id", `Int (Common.Source_id.to_int span.Common.Span.source));
        ("start", `Int span.start);
        ("stop", `Int span.stop);
      ]
    in
    match Common.Source_manager.find sources span.source with
    | None -> `Assoc fields
    | Some source ->
        let fields =
          ("path", `String (Common.Source_file.display_path source)) :: fields
        in
        let position_fields prefix offset =
          match Common.Source_file.position source offset with
          | Error _ -> []
          | Ok position ->
              [
                (prefix ^ "line", `Int position.line);
                (prefix ^ "column", `Int position.column);
              ]
        in
        `Assoc
          (fields
          @ position_fields "" span.start
          @ position_fields "end_" span.stop)

  let option_span_to_yojson sources = function
    | None -> `Null
    | Some span -> span_to_yojson sources span

  let origin_to_yojson sources = function
    | Pinned_source { path; line } ->
        `Assoc
          [
            ("kind", `String "pinned-source");
            ("path", `String path);
            ("line", `Int line);
          ]
    | Source_span span ->
        `Assoc
          [
            ("kind", `String "source-span");
            ("span", span_to_yojson sources span);
          ]
    | Source_location source ->
        `Assoc
          [
            ("kind", `String "source-location");
            ("span", span_to_yojson sources source.span);
            ( "source_segments",
              `List (List.map (span_to_yojson sources) source.source_segments)
            );
            ( "generated_from",
              option_span_to_yojson sources source.generated_from );
            ("defined_at", option_span_to_yojson sources source.defined_at);
          ]
    | Session_registration ->
        `Assoc [ ("kind", `String "session-registration") ]

  let call_shape_to_yojson shape =
    `Assoc
      [
        ( "parameters",
          `List
            (List.mapi
               (fun position parameter ->
                 `Assoc
                   [
                     ("position", `Int position);
                     ( "name",
                       match parameter.parameter_name with
                       | None -> `Null
                       | Some name -> `String name );
                     ("has_default", `Bool parameter.has_default);
                   ])
               shape.parameters) );
        ("variadic", `Bool shape.variadic);
      ]

  let entry_to_yojson sources entry =
    `Assoc
      [
        ("id", `Int entry.id);
        ("name", `String entry.name);
        ("kind", `String (kind_name entry.kind));
        ("origin", origin_to_yojson sources entry.origin);
        ( "call_shape",
          match entry.function_call_shape with
          | None -> `Null
          | Some shape -> call_shape_to_yojson shape );
      ]

  let to_yojson ?(source_only = false) sources environment =
    `Assoc
      [
        ("schema", `String "holyc-symbol-visibility-v2");
        ("reference_commit", `String Generated.Opcode_keywords.reference_commit);
        ( "symbols",
          `List
            (List.map (entry_to_yojson sources)
               (selected_entries source_only environment)) );
        ( "local_contexts",
          `List
            (List.rev environment.local_contexts
            |> List.map (fun (context, names) ->
                `Assoc
                  [
                    ("id", `Int context);
                    ( "names",
                      `List
                        (String_set.elements names
                        |> List.map (fun name -> `String name)) );
                  ])) );
      ]

  let json ?(source_only = false) sources environment =
    to_yojson ~source_only sources environment |> Yojson.Safe.pretty_to_string

  let dump sources environment = human sources environment
end
