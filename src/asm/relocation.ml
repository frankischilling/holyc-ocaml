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
    ignore
      (checked_sum "relocation raw image end exceeds the signed 64-bit range"
         position (Int64.of_int length));
    let loaded =
      checked_sum "relocation loaded position exceeds the signed 64-bit range"
        position base
    in
    ignore
      (checked_sum "relocation loaded image end exceeds the signed 64-bit range"
         loaded (Int64.of_int length));
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
    (* The legacy allowance is checked above with its original cost. The shared
       engine additionally counts owned metadata and written field bytes. Ten
       units per admitted legacy unit plus fixed section metadata cover that
       checked bridge without narrowing the existing public allowance. *)
    let model_work = (10 * max_work) + 16 in
    let module Sections = Native_sections in
    let imports = Hashtbl.create 16 and reversed_symbols = ref [] in
    let import name =
      match Hashtbl.find_opt imports name with
      | Some id -> id
      | None ->
          let id = Hashtbl.length imports in
          Hashtbl.add imports name id;
          reversed_symbols :=
            Sections.
              {
                id;
                name;
                scope = None;
                source_identity = None;
                binding = Import;
                definition = Undefined;
                source = no_source;
              }
            :: !reversed_symbols;
          id
    in
    let records =
      List.mapi
        (fun relocation_index relocation ->
          charge ~relocation_index 1;
          if
            relocation.width > length
            || relocation.offset > length - relocation.width
          then
            reject ~relocation_index
              "relocation field extends beyond its input image";
          let intent =
            match relocation.kind with
            | Add_base -> Sections.Add_bias
            | Subtract_base -> Sections.Subtract_bias
            | Relative_import name | Absolute_import name ->
                charge ~relocation_index (String.length name);
                if not (Symbols.mem name !definitions) then
                  reject ~relocation_index
                    "relocation import has no supplied symbol value";
                let relative =
                  match relocation.kind with
                  | Relative_import _ -> true
                  | _ -> false
                in
                Sections.Set
                  {
                    target = Symbol (import name);
                    addend = 0L;
                    relative_to =
                      (if relative then
                         Some
                           {
                             section = 0;
                             offset = relocation.offset + relocation.width;
                           }
                       else None);
                    range = (if relative then Signed else Modulo);
                  }
          in
          Sections.
            {
              field = { section = 0; offset = relocation.offset };
              width = relocation.width;
              intent;
              phase = Load;
              source = no_source;
            })
        relocations
    in
    let object_ =
      match
        Sections.create ~max_work:model_work ~max_bytes:(max 1 length)
          ~mode:Sections.Jit ~position
          ~sections:
            [
              {
                id = 0;
                name = ".image";
                kind = Code;
                alignment_bits = 0;
                bytes;
              };
            ]
          ~symbols:(List.rev !reversed_symbols)
          ~relocations:records ()
      with
      | Ok object_ -> object_
      | Error e -> reject ?relocation_index:e.relocation_index e.message
    in
    match Sections.link ~max_work:model_work ~imports:symbols ~base object_ with
    | Ok linked -> Ok (Sections.bytes linked)
    | Error e ->
        Error { relocation_index = e.relocation_index; message = e.message }
  with Rejected error -> Error error
