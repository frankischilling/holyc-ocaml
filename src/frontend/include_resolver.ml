type error =
  | Empty_path
  | Path_contains_nul
  | Home_path_requires_mapping of string
  | Templeos_root_requires_mapping of string
  | Drive_path_unsupported of string
  | Not_found of { spelling : string; searched : string list }
  | Outside_allowed_roots of {
      spelling : string;
      resolved : string;
      allowed_roots : string list;
    }
  | Is_directory of string
  | Not_regular_file of string
  | Io_error of { path : string; message : string }

type resolution = { canonical_path : string; source_path : string }
type metadata_resolution = { resolved_path : string }

type t = {
  working_directory : string;
  include_roots : string list;
  templeos_root : string option;
  allowed_roots : string list;
}

let starts_with ~prefix text =
  let prefix_length = String.length prefix in
  String.length text >= prefix_length
  && String.sub text 0 prefix_length = prefix

let normalize_for_comparison path =
  let path =
    if Sys.win32 then
      String.map (fun byte -> if Char.equal byte '\\' then '/' else byte) path
    else path
  in
  if Sys.win32 then String.lowercase_ascii path else path

let equal_path left right =
  String.equal (normalize_for_comparison left) (normalize_for_comparison right)

let canonical_directory ~label path =
  try
    let canonical = Unix.realpath path in
    let status = Unix.stat canonical in
    if status.st_kind = Unix.S_DIR then Ok canonical
    else Error (Printf.sprintf "%s is not a directory: %s" label path)
  with
  | Sys_error message ->
      Error (Printf.sprintf "could not resolve %s %s: %s" label path message)
  | Unix.Unix_error (error, operation, argument) ->
      Error
        (Printf.sprintf "could not resolve %s %s: %s %s: %s" label path
           operation argument (Unix.error_message error))

let add_distinct paths path =
  if List.exists (equal_path path) paths then paths else paths @ [ path ]

let create ?working_directory ?(include_roots = []) ?templeos_root () =
  let ( let* ) result continuation = Result.bind result continuation in
  let working_directory =
    Option.value working_directory ~default:(Sys.getcwd ())
  in
  let* working_directory =
    canonical_directory ~label:"working directory" working_directory
  in
  let rec canonicalize_roots found = function
    | [] -> Ok found
    | root :: rest ->
        let* root = canonical_directory ~label:"include root" root in
        canonicalize_roots (add_distinct found root) rest
  in
  let* include_roots = canonicalize_roots [] include_roots in
  let* templeos_root =
    match templeos_root with
    | None -> Ok None
    | Some root ->
        let* root = canonical_directory ~label:"TempleOS root" root in
        Ok (Some root)
  in
  let allowed_roots =
    List.fold_left add_distinct [ working_directory ] include_roots
    |> fun roots ->
    match templeos_root with
    | None -> roots
    | Some root -> add_distinct roots root
  in
  Ok { working_directory; include_roots; templeos_root; allowed_roots }

let working_directory resolver = resolver.working_directory
let include_roots resolver = resolver.include_roots
let templeos_root resolver = resolver.templeos_root
let allowed_roots resolver = resolver.allowed_roots

let trim_trailing_separators path =
  let rec stop index =
    if index <= 0 then index
    else
      match path.[index - 1] with
      | '/' | '\\' -> stop (index - 1)
      | _ -> index
  in
  let length = stop (String.length path) in
  if length = 0 then path else String.sub path 0 length

let path_within ~root path =
  let root = trim_trailing_separators root |> normalize_for_comparison in
  let path = normalize_for_comparison path in
  if String.equal root path then true
  else
    let prefix =
      if String.ends_with ~suffix:"/" root then root else root ^ "/"
    in
    starts_with ~prefix path

let has_drive_prefix path =
  String.length path >= 3
  &&
  match (path.[0], path.[1], path.[2]) with
  | ('A' .. 'Z' | 'a' .. 'z'), ':', ('/' | '\\') -> true
  | _ -> false

let has_extension path =
  let base = Filename.basename path in
  not (String.equal (Filename.extension base) "")

let source_paths path =
  if has_extension path then [ path ] else [ path ^ ".HC.Z"; path ^ ".HC" ]

type search = Candidates of string list | Search_error of error

let candidates resolver spelling =
  if starts_with ~prefix:"~/" spelling then
    Search_error (Home_path_requires_mapping spelling)
  else if starts_with ~prefix:"::/" spelling then
    match resolver.templeos_root with
    | None -> Search_error (Templeos_root_requires_mapping spelling)
    | Some root ->
        let relative = String.sub spelling 3 (String.length spelling - 3) in
        Candidates (source_paths relative |> List.map (Filename.concat root))
  else if starts_with ~prefix:"/" spelling then
    match resolver.templeos_root with
    | None -> Search_error (Templeos_root_requires_mapping spelling)
    | Some root ->
        let relative = String.sub spelling 1 (String.length spelling - 1) in
        Candidates (source_paths relative |> List.map (Filename.concat root))
  else if has_drive_prefix spelling then
    if Sys.win32 then Candidates (source_paths spelling)
    else Search_error (Drive_path_unsupported spelling)
  else if Filename.is_relative spelling then
    let roots = resolver.working_directory :: resolver.include_roots in
    Candidates
      (List.concat_map
         (fun root -> source_paths spelling |> List.map (Filename.concat root))
         roots)
  else Candidates (source_paths spelling)

let io_error path error operation argument =
  Io_error
    {
      path;
      message =
        Printf.sprintf "%s %s: %s" operation argument (Unix.error_message error);
    }

let inspect resolver ~spelling path =
  try
    let status = Unix.stat path in
    if status.st_kind = Unix.S_DIR then Error (Is_directory path)
    else if status.st_kind <> Unix.S_REG then Error (Not_regular_file path)
    else
      let canonical = Unix.realpath path in
      if
        List.exists
          (fun root -> path_within ~root canonical)
          resolver.allowed_roots
      then Ok (Some { canonical_path = canonical; source_path = path })
      else
        Error
          (Outside_allowed_roots
             {
               spelling;
               resolved = canonical;
               allowed_roots = resolver.allowed_roots;
             })
  with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok None
  | Unix.Unix_error (error, operation, argument) ->
      Error (io_error path error operation argument)
  | Sys_error message -> Error (Io_error { path; message })

let resolve resolver ~spelling =
  if String.equal spelling "" then Error Empty_path
  else if String.contains spelling '\x00' then Error Path_contains_nul
  else
    match candidates resolver spelling with
    | Search_error problem -> Error problem
    | Candidates candidates ->
        let rec find searched = function
          | [] -> Error (Not_found { spelling; searched = List.rev searched })
          | path :: rest -> (
              match inspect resolver ~spelling path with
              | Error _ as error -> error
              | Ok (Some resolution) -> Ok resolution
              | Ok None -> find (path :: searched) rest)
        in
        find [] candidates

let normalize_lexical path =
  let path =
    String.map (fun byte -> if Char.equal byte '\\' then '/' else byte) path
  in
  let length = String.length path in
  let prefix, body =
    if length >= 3 && Char.equal path.[1] ':' && Char.equal path.[2] '/' then
      (String.sub path 0 3, String.sub path 3 (length - 3))
    else if starts_with ~prefix:"//" path then
      ("//", String.sub path 2 (length - 2))
    else if starts_with ~prefix:"/" path then
      ("/", String.sub path 1 (length - 1))
    else ("", path)
  in
  let segments = String.split_on_char '/' body in
  let collapsed_rev =
    List.fold_left
      (fun found segment ->
        match segment with
        | "" | "." -> found
        | ".." -> (
            match found with
            | previous :: rest when not (String.equal previous "..") -> rest
            | _ -> segment :: found)
        | _ -> segment :: found)
      [] segments
  in
  let collapsed = List.rev collapsed_rev |> String.concat "/" in
  if String.equal collapsed "" then prefix else prefix ^ collapsed

let metadata_candidate resolver spelling =
  if starts_with ~prefix:"~/" spelling then
    Error (Home_path_requires_mapping spelling)
  else if starts_with ~prefix:"::/" spelling then
    match resolver.templeos_root with
    | None -> Error (Templeos_root_requires_mapping spelling)
    | Some root ->
        let relative = String.sub spelling 3 (String.length spelling - 3) in
        Ok (Filename.concat root relative)
  else if starts_with ~prefix:"/" spelling then
    match resolver.templeos_root with
    | None -> Error (Templeos_root_requires_mapping spelling)
    | Some root ->
        let relative = String.sub spelling 1 (String.length spelling - 1) in
        Ok (Filename.concat root relative)
  else if has_drive_prefix spelling then
    if Sys.win32 then Ok spelling else Error (Drive_path_unsupported spelling)
  else if Filename.is_relative spelling then
    Ok (Filename.concat resolver.working_directory spelling)
  else Ok spelling

let resolve_metadata resolver ~spelling =
  if String.equal spelling "" then Error Empty_path
  else if String.contains spelling '\x00' then Error Path_contains_nul
  else
    match metadata_candidate resolver spelling with
    | Error _ as error -> error
    | Ok source_path ->
        let resolved_path = normalize_lexical source_path in
        if
          List.exists
            (fun root -> path_within ~root resolved_path)
            resolver.allowed_roots
        then Ok { resolved_path }
        else
          Error
            (Outside_allowed_roots
               {
                 spelling;
                 resolved = resolved_path;
                 allowed_roots = resolver.allowed_roots;
               })
