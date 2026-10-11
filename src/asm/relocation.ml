module Symbols = Map.Make (String)

type kind =
  | Add_base
  | Subtract_base
  | Relative_import of string
  | Absolute_import of string

type t = { offset : int; width : int; kind : kind }
type error = { relocation_index : int option; message : string }

let make ~offset ~width ~kind =
  if offset < 0 then Error "relocation offset must be nonnegative"
  else if width <> 1 && width <> 2 && width <> 4 && width <> 8 then
    Error "relocation width must be one, two, four or eight bytes"
  else
    match kind with
    | Relative_import "" | Absolute_import "" ->
        Error "relocation import name must be nonempty"
    | _ -> Ok { offset; width; kind }

exception Rejected of error

let reject ?relocation_index message =
  raise (Rejected { relocation_index; message })

let checked_sum message left right =
  if left > Int64.sub Int64.max_int right then reject message;
  Int64.add left right

let read_word bytes offset width =
  let word = ref 0L in
  for index = 0 to width - 1 do
    let byte = Int64.of_int (Char.code (Bytes.get bytes (offset + index))) in
    word := Int64.logor !word (Int64.shift_left byte (8 * index))
  done;
  !word

let write_word bytes offset width word =
  for index = 0 to width - 1 do
    let byte =
      Int64.to_int
        (Int64.logand (Int64.shift_right_logical word (8 * index)) 255L)
    in
    Bytes.set bytes (offset + index) (Char.chr byte)
  done

let apply ?(max_work = 1_000_000) ~position ~base ~symbols ~bytes relocations =
  try
    if max_work <= 0 || max_work > 100_000_000 then
      reject "relocation work limit must be between 1 and 100000000";
    let work = ref 0 in
    let charge ?relocation_index amount =
      if amount > max_work - !work then
        reject ?relocation_index "relocation work exceeds its configured limit";
      work := !work + amount
    in
    let length = String.length bytes in
    charge length;
    if position < 0L then reject "relocation raw position must be nonnegative";
    if base < 0L then reject "relocation load bias must be nonnegative";
    let size = Int64.of_int length in
    ignore
      (checked_sum "relocation raw image end exceeds the signed 64-bit range"
         position size);
    let loaded_position =
      checked_sum "relocation loaded position exceeds the signed 64-bit range"
        position base
    in
    ignore
      (checked_sum "relocation loaded image end exceeds the signed 64-bit range"
         loaded_position size);
    let definitions = ref Symbols.empty in
    List.iter
      (fun (name, value) ->
        charge 1;
        charge (String.length name);
        if name = "" then reject "relocation symbol name must be nonempty";
        if Symbols.mem name !definitions then
          reject "relocation symbol names must be unique";
        definitions := Symbols.add name value !definitions)
      symbols;
    let base_records = ref [] and imports = ref [] in
    List.iteri
      (fun relocation_index relocation ->
        charge ~relocation_index 1;
        if
          relocation.width > length
          || relocation.offset > length - relocation.width
        then
          reject ~relocation_index
            "relocation field extends beyond its input image";
        match relocation.kind with
        | Add_base | Subtract_base ->
            base_records := relocation :: !base_records
        | Relative_import name | Absolute_import name ->
            charge ~relocation_index (String.length name);
            let target =
              match Symbols.find_opt name !definitions with
              | Some value -> value
              | None ->
                  reject ~relocation_index
                    "relocation import has no supplied symbol value"
            in
            let value =
              match relocation.kind with
              | Relative_import _ ->
                  let field_end =
                    Int64.add loaded_position
                      (Int64.of_int (relocation.offset + relocation.width))
                  in
                  if target < Int64.add Int64.min_int field_end then
                    reject ~relocation_index
                      "relative import calculation exceeds the signed 64-bit \
                       range";
                  let value = Int64.sub target field_end in
                  (match
                     Encoding.field ~bytes:relocation.width
                       ~range:Encoding.Signed value
                   with
                  | Ok _ -> ()
                  | Error message -> reject ~relocation_index message);
                  value
              | Absolute_import _ -> target
              | Add_base | Subtract_base -> assert false
            in
            imports := (relocation, value) :: !imports)
      relocations;
    let output = Bytes.of_string bytes in
    List.iter
      (fun relocation ->
        let word = read_word output relocation.offset relocation.width in
        let value =
          match relocation.kind with
          | Add_base -> Int64.add word base
          | Subtract_base -> Int64.sub word base
          | Relative_import _ | Absolute_import _ -> assert false
        in
        write_word output relocation.offset relocation.width value)
      (List.rev !base_records);
    List.iter
      (fun (relocation, value) ->
        write_word output relocation.offset relocation.width value)
      (List.rev !imports);
    Ok (Bytes.unsafe_to_string output)
  with Rejected error -> Error error
