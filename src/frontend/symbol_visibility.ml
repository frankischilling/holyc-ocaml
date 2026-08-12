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

type origin =
  | Pinned_source of { path : string; line : int }
  | Source_span of Common.Span.t
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
    | Session_registration -> "<session>"

  let dump sources environment =
    let buffer = Buffer.create 512 in
    Buffer.add_string buffer "holyc-symbol-visibility-v1\n";
    List.iter
      (fun entry ->
        Printf.bprintf buffer "symbol %d name=%S kind=%s origin=%s\n" entry.id
          entry.name (kind_name entry.kind)
          (origin_text sources entry.origin))
      (all environment);
    List.rev environment.local_contexts
    |> List.iter (fun (context, names) ->
        Printf.bprintf buffer "local-context %d\n" context;
        String_set.elements names
        |> List.iter (fun name ->
            Printf.bprintf buffer "  local name=%S\n" name));
    buffer |> Buffer.contents
end
