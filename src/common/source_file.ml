type position = { offset : int; line : int; column : int }

type t = {
  id : Source_id.t;
  path : string;
  display_path : string;
  contents : string;
  line_starts : int array;
}

let compute_line_starts contents =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index byte ->
      if Char.equal byte '\n' then starts := (index + 1) :: !starts)
    contents;
  Array.of_list (List.rev !starts)

let create ~id ~path ~display_path ~contents =
  {
    id;
    path;
    display_path;
    contents;
    line_starts = compute_line_starts contents;
  }

let default_max_bytes = 64 * 1024 * 1024

let load ?(max_bytes = default_max_bytes) ~id ~path () =
  if max_bytes < 0 then Error "source size limit must be nonnegative"
  else
    try
      let canonical = Unix.realpath path in
      let channel = open_in_bin canonical in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          let size = in_channel_length channel in
          if size > max_bytes then
            Error
              (Printf.sprintf "source file is %d bytes; the limit is %d bytes"
                 size max_bytes)
          else
            let contents = really_input_string channel size in
            Ok (create ~id ~path:canonical ~display_path:path ~contents))
    with
    | Sys_error message -> Error message
    | Unix.Unix_error (error, operation, argument) ->
        Error
          (Printf.sprintf "%s: %s: %s" operation argument
             (Unix.error_message error))

let id source = source.id
let path source = source.path
let display_path source = source.display_path
let contents source = source.contents
let length source = String.length source.contents
let line_count source = Array.length source.line_starts

let find_line_index starts offset =
  let low = ref 0 in
  let high = ref (Array.length starts - 1) in
  while !low < !high do
    let middle = (!low + !high + 1) / 2 in
    if starts.(middle) <= offset then low := middle else high := middle - 1
  done;
  !low

let position source offset =
  if offset < 0 || offset > length source then
    Error
      (Printf.sprintf "source offset %d is outside 0..%d" offset (length source))
  else
    let line_index = find_line_index source.line_starts offset in
    let line_start = source.line_starts.(line_index) in
    Ok { offset; line = line_index + 1; column = offset - line_start + 1 }

let line_bounds source ~line =
  if line < 1 || line > line_count source then
    Error
      (Printf.sprintf "source line %d is outside 1..%d" line (line_count source))
  else
    let index = line - 1 in
    let start = source.line_starts.(index) in
    let next_start =
      if index + 1 < Array.length source.line_starts then
        source.line_starts.(index + 1)
      else length source
    in
    let stop =
      if next_start > start && Char.equal source.contents.[next_start - 1] '\n'
      then
        let before_lf = next_start - 1 in
        if before_lf > start && Char.equal source.contents.[before_lf - 1] '\r'
        then before_lf - 1
        else before_lf
      else next_start
    in
    Ok (start, stop)

let line_text source ~line =
  match line_bounds source ~line with
  | Error _ as error -> error
  | Ok (start, stop) -> Ok (String.sub source.contents start (stop - start))
