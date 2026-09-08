module Facts = Generated.Primitive_raw_types

type t = I0 | I8 | I16 | I32 | I64 | U0 | U8 | U16 | U32 | U64 | F64 | Bool
type category = Integer | Floating | Boolean
type signedness = Signed | Unsigned | Not_applicable
type declaration_form = Internal_type | Public_union

type info = {
  primitive : t;
  spelling : string;
  storage_spelling : string;
  raw_name : string;
  raw_id : int;
  byte_size : int;
  category : category;
  signedness : signedness;
  raw_is_unsigned : bool;
  declaration_form : declaration_form;
  raw_source_line : int;
  storage_source_line : int;
  declaration_source_line : int;
}

type pointer_representation = {
  raw_name : string;
  target_raw_name : string;
  raw_id : int;
  source_line : int;
}

type specification = {
  primitive : t;
  spelling : string;
  storage_spelling : string;
  raw_name : string;
  category : category;
  signedness : signedness;
  declaration_form : declaration_form;
}

let reference_commit = Facts.reference_commit
let raw_source_path = Facts.kernel_source_path
let raw_source_sha256 = Facts.kernel_source_sha256
let internal_type_source_path = Facts.cinit_source_path
let internal_type_source_sha256 = Facts.cinit_source_sha256

let specifications =
  [
    {
      primitive = I0;
      spelling = "I0";
      storage_spelling = "I0i";
      raw_name = "RT_I0";
      category = Integer;
      signedness = Signed;
      declaration_form = Internal_type;
    };
    {
      primitive = I8;
      spelling = "I8";
      storage_spelling = "I8i";
      raw_name = "RT_I8";
      category = Integer;
      signedness = Signed;
      declaration_form = Internal_type;
    };
    {
      primitive = I16;
      spelling = "I16";
      storage_spelling = "I16i";
      raw_name = "RT_I16";
      category = Integer;
      signedness = Signed;
      declaration_form = Public_union;
    };
    {
      primitive = I32;
      spelling = "I32";
      storage_spelling = "I32i";
      raw_name = "RT_I32";
      category = Integer;
      signedness = Signed;
      declaration_form = Public_union;
    };
    {
      primitive = I64;
      spelling = "I64";
      storage_spelling = "I64i";
      raw_name = "RT_I64";
      category = Integer;
      signedness = Signed;
      declaration_form = Public_union;
    };
    {
      primitive = U0;
      spelling = "U0";
      storage_spelling = "U0i";
      raw_name = "RT_U0";
      category = Integer;
      signedness = Unsigned;
      declaration_form = Internal_type;
    };
    {
      primitive = U8;
      spelling = "U8";
      storage_spelling = "U8i";
      raw_name = "RT_U8";
      category = Integer;
      signedness = Unsigned;
      declaration_form = Internal_type;
    };
    {
      primitive = U16;
      spelling = "U16";
      storage_spelling = "U16i";
      raw_name = "RT_U16";
      category = Integer;
      signedness = Unsigned;
      declaration_form = Public_union;
    };
    {
      primitive = U32;
      spelling = "U32";
      storage_spelling = "U32i";
      raw_name = "RT_U32";
      category = Integer;
      signedness = Unsigned;
      declaration_form = Public_union;
    };
    {
      primitive = U64;
      spelling = "U64";
      storage_spelling = "U64i";
      raw_name = "RT_U64";
      category = Integer;
      signedness = Unsigned;
      declaration_form = Public_union;
    };
    {
      primitive = F64;
      spelling = "F64";
      storage_spelling = "F64i";
      raw_name = "RT_F64";
      category = Floating;
      signedness = Not_applicable;
      declaration_form = Internal_type;
    };
    {
      primitive = Bool;
      spelling = "Bool";
      storage_spelling = "I8i";
      raw_name = "RT_I8";
      category = Boolean;
      signedness = Not_applicable;
      declaration_form = Internal_type;
    };
  ]

let all = List.map (fun specification -> specification.primitive) specifications

let rank = function
  | I0 -> 0
  | I8 -> 1
  | I16 -> 2
  | I32 -> 3
  | I64 -> 4
  | U0 -> 5
  | U8 -> 6
  | U16 -> 7
  | U32 -> 8
  | U64 -> 9
  | F64 -> 10
  | Bool -> 11

let compare left right = Int.compare (rank left) (rank right)
let equal left right = compare left right = 0

let specification primitive =
  List.find
    (fun specification -> equal specification.primitive primitive)
    specifications

let to_string primitive = (specification primitive).spelling

let of_spelling spelling =
  List.find_opt
    (fun specification -> String.equal specification.spelling spelling)
    specifications
  |> Option.map (fun specification -> specification.primitive)

let of_storage_spelling spelling =
  specifications
  |> List.find_opt (fun specification ->
      specification.primitive <> Bool
      && String.equal specification.storage_spelling spelling)
  |> Option.map (fun specification -> specification.primitive)

let raw_type raw_name =
  match
    List.find_opt
      (fun (entry : Facts.raw_type) -> String.equal entry.name raw_name)
      Facts.raw_types
  with
  | Some entry -> entry
  | None ->
      invalid_arg (Printf.sprintf "missing generated raw type %s" raw_name)

let internal_type spelling =
  match
    List.find_opt
      (fun (entry : Facts.internal_type) ->
        String.equal entry.spelling spelling)
      Facts.internal_types
  with
  | Some entry -> entry
  | None ->
      invalid_arg (Printf.sprintf "missing generated internal type %s" spelling)

let public_union spelling =
  match
    List.find_opt
      (fun (entry : Facts.public_union) ->
        String.equal entry.public_spelling spelling)
      Facts.public_unions
  with
  | Some entry -> entry
  | None ->
      invalid_arg (Printf.sprintf "missing generated public union %s" spelling)

let declaration_source_line specification =
  match specification.declaration_form with
  | Internal_type -> (internal_type specification.spelling).source_line
  | Public_union -> (public_union specification.spelling).source_line

let make_info specification =
  let raw = raw_type specification.raw_name in
  let storage = internal_type specification.storage_spelling in
  if raw.not_implemented || raw.fictitious then
    invalid_arg
      (Printf.sprintf "unsupported raw type %s entered the semantic table"
         raw.name);
  if not (String.equal raw.name storage.raw_name) then
    invalid_arg
      (Printf.sprintf "storage type %s uses %s instead of %s" storage.spelling
         storage.raw_name raw.name);
  let raw_is_unsigned = raw.templeos_id land Facts.unsigned_flag <> 0 in
  let expected_unsigned =
    match specification.signedness with
    | Unsigned -> true
    | Signed | Not_applicable -> false
  in
  if raw_is_unsigned <> expected_unsigned then
    invalid_arg
      (Printf.sprintf "raw signedness for %s conflicts with %s"
         specification.spelling raw.name);
  (match specification.declaration_form with
  | Internal_type -> ()
  | Public_union ->
      let union = public_union specification.spelling in
      if not (String.equal union.storage_spelling storage.spelling) then
        invalid_arg
          (Printf.sprintf "public union %s uses storage %s instead of %s"
             union.public_spelling union.storage_spelling storage.spelling));
  {
    primitive = specification.primitive;
    spelling = specification.spelling;
    storage_spelling = storage.spelling;
    raw_name = raw.name;
    raw_id = raw.templeos_id;
    byte_size = storage.byte_size;
    category = specification.category;
    signedness = specification.signedness;
    raw_is_unsigned;
    declaration_form = specification.declaration_form;
    raw_source_line = raw.source_line;
    storage_source_line = storage.source_line;
    declaration_source_line = declaration_source_line specification;
  }

let information =
  List.map (fun specification -> make_info specification) specifications

let info primitive =
  List.find (fun (entry : info) -> equal entry.primitive primitive) information

let is_zero_sized primitive = (info primitive).byte_size = 0

let pointer_representation =
  let pointer = Facts.pointer_alias in
  let target = raw_type pointer.target_name in
  if pointer.templeos_id <> target.templeos_id then
    invalid_arg "RT_PTR no longer aliases RT_I64";
  {
    raw_name = pointer.name;
    target_raw_name = target.name;
    raw_id = pointer.templeos_id;
    source_line = pointer.source_line;
  }

let pointer_byte_size =
  let pointer = pointer_representation in
  let target = info I64 in
  if pointer.raw_id <> target.raw_id then
    invalid_arg "RT_PTR no longer has I64 storage";
  target.byte_size
