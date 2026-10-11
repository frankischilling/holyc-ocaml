open Cmdliner
module V = Holyc_lib.Bin_validator
module IO = Bin_module_command

let integer label text =
  try Int64.of_string text
  with Failure _ -> IO.fail (label ^ " must be an I64 integer")

let binding text =
  match String.index_opt text '=' with
  | None -> IO.fail "binding must have the form NAME=VALUE"
  | Some i ->
      ( String.sub text 0 i,
        integer "binding value"
          (String.sub text (i + 1) (String.length text - i - 1)) )

let unwrap = function
  | Ok x -> x
  | Error (e : V.error) ->
      IO.fail
        (Printf.sprintf "%s%s: %s" e.artifact
           (match e.offset with
           | None -> ""
           | Some n -> Printf.sprintf ":byte %d" n)
           e.message)

let run model bases bindings no_abss just_load silent require_resolved output
    map json file_bytes work image_bytes heap_bytes paths =
  try
    let limits : V.limits =
      { file_bytes; work; image_bytes; heap_bytes; modules = 4096 }
    in
    if paths = [] then IO.fail "at least one BIN artifact is required";
    if
      (not model)
      && (bases <> [] || bindings <> [] || no_abss || just_load || silent
        || require_resolved || map <> None)
    then IO.fail "loader options belong to dump-relocs";
    if bases <> [] && List.length bases <> List.length paths then
      IO.fail
        "supply one --base per artifact, or omit all bases for automatic \
         placement";
    if file_bytes < 1 || file_bytes > 16777216 then
      IO.fail "byte-limit must be between 1 and 16777216";
    (* Read and validate every input before interpreting any module. *)
    if List.length paths > limits.modules then
      IO.fail "module count limit exceeded";
    let input_bytes = ref 0 in
    let artifacts =
      List.map
        (fun path ->
          let remaining = V.default_limits.image_bytes - !input_bytes in
          let raw = IO.read ~limit:(min file_bytes remaining) path in
          input_bytes := !input_bytes + String.length raw;
          V.read ~limits ~name:path raw |> unwrap)
        paths
    in
    let report, map_text =
      if model then (
        let world =
          List.fold_left
            (fun w text ->
              let name, value = binding text in
              V.bind ~limits w ~name ~value |> unwrap)
            (V.empty ()) bindings
        in
        let flags : V.flags = { no_abss; just_load; silent } in
        let rec loads world artifacts bases =
          match (artifacts, bases) with
          | [], _ -> world
          | artifact :: rest, [] ->
              loads (V.load ~limits ~flags world artifact |> unwrap) rest []
          | artifact :: rest, base :: tail ->
              loads
                (V.load ~limits ~flags ~base:(integer "base" base) world
                   artifact
                |> unwrap)
                rest tail
        in
        let world = loads world artifacts bases in
        (if require_resolved && V.unresolved world <> [] then
           let p = List.hd (V.unresolved world) in
           IO.fail
             (Printf.sprintf "%s:byte %d: unresolved import %S" p.artifact
                p.record_offset p.symbol));
        (V.report world, V.map world))
      else
        ( `Assoc
            [
              ("schema", `String "holyc-bin-verification-v1");
              ("artifacts", `List (List.map V.to_yojson artifacts));
            ],
          "" )
    in
    let rendered =
      if json then Yojson.Safe.pretty_to_string report ^ "\n"
      else if model then map_text
      else
        String.concat ""
          (List.map2
             (fun path artifact ->
               Printf.sprintf
                 "%S: valid BIN, %d body bytes, %d records, work %d\n" path
                 (String.length (V.body artifact))
                 (List.length (V.records artifact))
                 (V.work artifact))
             paths artifacts)
    in
    let files =
      (match output with
        | None -> []
        | Some path -> [ (path, rendered) ])
      @
      match map with
      | None -> []
      | Some path -> [ (path, map_text) ]
    in
    IO.save ~inputs:paths files;
    output_string stdout rendered;
    0
  with
  | IO.Failed message | Sys_error message | Unix.Unix_error (_, _, message) ->
    if json then
      `Assoc
        [
          ("schema", `String "holyc-command-error-v1");
          ("command", `String (if model then "dump-relocs" else "verify-bin"));
          ("message", `String message);
        ]
      |> Yojson.Safe.to_string |> prerr_endline
    else
      Printf.eprintf "holyc %s: %s\n"
        (if model then "dump-relocs" else "verify-bin")
        message;
    1

let command model =
  let bases =
    Arg.(
      value & opt_all string []
      & info [ "base" ] ~docv:"BODY_ADDRESS"
          ~doc:"Virtual body base, once per input; overrides header org.")
  in
  let bindings =
    Arg.(
      value & opt_all string []
      & info [ "bind" ] ~docv:"NAME=VALUE"
          ~doc:"Initial virtual external symbol, repeatable.")
  in
  let flag name doc = Arg.(value & flag & info [ name ] ~doc) in
  let no_abss = flag "no-abss" "Model LDF_NO_ABSS." in
  let just_load =
    flag "just-load" "Model LDF_JUST_LOAD; omit MAIN addresses."
  in
  let silent =
    flag "silent"
      "Model LDF_SILENT; retain unresolved imports without messages."
  in
  let resolved =
    flag "require-resolved" "Fail if any import groups remain unresolved."
  in
  let output =
    Arg.(value & opt (some string) None & info [ "output" ] ~docv:"REPORT")
  in
  let map_arg =
    Arg.(value & opt (some string) None & info [ "map" ] ~docv:"MAP")
  in
  let json =
    Arg.(
      value
      & opt (enum [ ("json", true); ("human", false) ]) false
      & info [ "format" ])
  in
  let limit name default = Arg.(value & opt int default & info [ name ]) in
  let file_bytes = limit "byte-limit" V.default_limits.file_bytes in
  let work = limit "work-limit" V.default_limits.work in
  let image_bytes = limit "image-limit" V.default_limits.image_bytes in
  let heap_bytes =
    Arg.(value & opt int64 V.default_limits.heap_bytes & info [ "heap-limit" ])
  in
  let paths = Arg.(non_empty & pos_all string [] & info [] ~docv:"BIN") in
  let name = if model then "dump-relocs" else "verify-bin" in
  let doc =
    if model then
      "Predict BIN patches, heap allocations, deferred imports and MAIN order \
       using virtual addresses. No code is executed."
    else
      "Validate complete uncompressed TempleOS BIN artifacts using an \
       independent reader."
  in
  Cmd.v (Cmd.info name ~doc)
    Term.(
      const (run model)
      $ bases $ bindings $ no_abss $ just_load $ silent $ resolved $ output
      $ map_arg $ json $ file_bytes $ work $ image_bytes $ heap_bytes $ paths)
