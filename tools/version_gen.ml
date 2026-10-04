let trim = String.trim

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false

let valid_commit value = String.length value = 40 && String.for_all is_hex value

let git_environment () =
  let locations =
    [
      "GIT_DIR";
      "GIT_WORK_TREE";
      "GIT_COMMON_DIR";
      "GIT_INDEX_FILE";
      "GIT_OBJECT_DIRECTORY";
      "GIT_ALTERNATE_OBJECT_DIRECTORIES";
    ]
  in
  Unix.environment () |> Array.to_list
  |> List.filter (fun binding ->
      let name =
        match String.index_opt binding '=' with
        | None -> binding
        | Some separator -> String.sub binding 0 separator
      in
      not (List.mem (String.uppercase_ascii name) locations))
  |> Array.of_list

let git_output root arguments =
  let command = Array.of_list ([ "git"; "-C"; root ] @ arguments) in
  try
    let channels =
      Unix.open_process_args_full "git" command (git_environment ())
    in
    let input, _, _ = channels in
    let value =
      try In_channel.input_all input |> trim
      with error ->
        ignore (Unix.close_process_full channels);
        raise error
    in
    match Unix.close_process_full channels with
    | Unix.WEXITED 0 -> Some value
    | _ -> None
  with Unix.Unix_error _ | Sys_error _ -> None

let source_commit () =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some workspace when not (Filename.is_relative workspace) -> (
      match Array.to_list Sys.argv with
      | [ _; project ] when Filename.is_relative project ->
          let root = Filename.concat workspace project in
          let owned =
            if Sys.file_exists (Filename.concat root ".git") then
              git_output root [ "rev-parse"; "--show-prefix" ] = Some ""
            else
              Option.is_some
                (git_output root
                   [
                     "ls-files";
                     "--error-unmatch";
                     "--";
                     "dune-project";
                     "src/dune";
                     "tools/version_gen.ml";
                   ])
          in
          if owned then
            match git_output root [ "rev-parse"; "--verify"; "HEAD" ] with
            | Some value when valid_commit value -> value
            | _ -> "unknown"
          else "unknown"
      | _ -> "unknown")
  | _ -> "unknown"

let implementation_commit () =
  match Sys.getenv_opt "HOLYC_IMPLEMENTATION_COMMIT" with
  | Some value when valid_commit value -> value
  | _ -> source_commit ()

let () =
  Printf.printf "let implementation_commit = %S\n" (implementation_commit ())
