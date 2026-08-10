let trim = String.trim

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false

let valid_commit value = String.length value = 40 && String.for_all is_hex value

let read_git_commit () =
  let command = [| "git"; "rev-parse"; "--verify"; "HEAD" |] in
  let input = Unix.open_process_args_in "git" command in
  let value = try input_line input |> trim with End_of_file -> "unknown" in
  match Unix.close_process_in input with
  | Unix.WEXITED 0 when valid_commit value -> value
  | _ -> "unknown"

let implementation_commit () =
  match Sys.getenv_opt "HOLYC_IMPLEMENTATION_COMMIT" with
  | Some value when valid_commit value -> value
  | _ -> read_git_commit ()

let () =
  Printf.printf "let implementation_commit = %S\n" (implementation_commit ())
