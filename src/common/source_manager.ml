type t = { mutable next_id : int; files : (int, Source_file.t) Hashtbl.t }

let create () = { next_id = 0; files = Hashtbl.create 17 }

let fresh_id manager =
  let raw = manager.next_id in
  manager.next_id <- raw + 1;
  match Source_id.of_int raw with
  | Ok id -> id
  | Error message -> invalid_arg message

let register manager source =
  Hashtbl.replace manager.files
    (Source_id.to_int (Source_file.id source))
    source;
  source

let add_string manager ~path ~contents =
  let id = fresh_id manager in
  Source_file.create ~id ~path ~display_path:path ~contents |> register manager

let load ?max_bytes ?display_path manager ~path =
  let id = fresh_id manager in
  match Source_file.load ?max_bytes ?display_path ~id ~path () with
  | Error _ as error -> error
  | Ok source -> Ok (register manager source)

let find manager id = Hashtbl.find_opt manager.files (Source_id.to_int id)
