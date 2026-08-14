type kind = Allocate | Disable
type position = Before_type | After_type

type explicit_register = {
  spelling : string;
  number : int;
  origin : Symbol.origin;
}

type t = {
  kind : kind;
  position : position;
  spelling : string;
  origin : Symbol.origin;
  explicit_register : explicit_register option;
}

type selection =
  | Unspecified
  | Allocatable
  | Disabled
  | Explicit of explicit_register

let canonical_u64_registers =
  List.init 16 (fun register_number ->
      match
        List.find_opt
          (fun (register : Generated.Opcode_keywords.register) ->
            register.register_kind = Generated.Opcode_keywords.R64
            && register.register_number = register_number)
          Generated.Opcode_keywords.registers
      with
      | Some register -> (register.spelling, register_number)
      | None ->
          invalid_arg
            (Printf.sprintf
               "checked opcode table lacks canonical U64 register %d"
               register_number))

let canonical_u64_register_number spelling =
  canonical_u64_registers
  |> List.find_map (fun (candidate, number) ->
      if String.equal candidate spelling then Some number else None)

let is_canonical_u64_register spelling =
  Option.is_some (canonical_u64_register_number spelling)

let kind_name = function
  | Allocate -> "reg"
  | Disable -> "noreg"

let position_name = function
  | Before_type -> "before-type"
  | After_type -> "after-type"

let make ~kind ~position ~spelling ~origin ?explicit_register
    ?explicit_register_number ?explicit_register_origin () =
  let expected_spelling = kind_name kind in
  if not (String.equal spelling expected_spelling) then
    Error
      (Printf.sprintf "semantic register spelling %S does not match %S" spelling
         expected_spelling)
  else
    match
      (explicit_register, explicit_register_number, explicit_register_origin)
    with
    | None, None, None ->
        Ok { kind; position; spelling; origin; explicit_register = None }
    | Some _, Some _, Some _ when kind = Disable ->
        Error "semantic noreg request cannot name an explicit register"
    | Some register, Some number, Some register_origin -> (
        match canonical_u64_register_number register with
        | None ->
            Error
              (Printf.sprintf
                 "semantic explicit register %S is not in ST_U64_REGS" register)
        | Some expected when number <> expected ->
            Error
              (Printf.sprintf
                 "semantic explicit register %S has number %d, expected %d"
                 register number expected)
        | Some _ ->
            Ok
              {
                kind;
                position;
                spelling;
                origin;
                explicit_register =
                  Some { spelling = register; number; origin = register_origin };
              })
    | None, None, Some _
    | None, Some _, None
    | None, Some _, Some _
    | Some _, None, None
    | Some _, None, Some _
    | Some _, Some _, None ->
        Error
          "semantic explicit register spelling, number, and source location \
           must be supplied together"

let kind request = request.kind
let position request = request.position
let spelling request = request.spelling
let origin request = request.origin
let explicit_register request = request.explicit_register

let explicit_register_spelling (register : explicit_register) =
  register.spelling

let explicit_register_number (register : explicit_register) = register.number
let explicit_register_origin (register : explicit_register) = register.origin

let effective requests =
  match List.rev requests with
  | [] -> Unspecified
  | request :: _ -> (
      match (request.kind, request.explicit_register) with
      | Allocate, None -> Allocatable
      | Disable, None -> Disabled
      | Allocate, Some register -> Explicit register
      | Disable, Some _ -> assert false)

let source_code = function
  | Unspecified -> -128
  | Disabled -> 32
  | Allocatable -> 33
  | Explicit register -> register.number

let equal left right =
  left.kind = right.kind
  && left.position = right.position
  && String.equal left.spelling right.spelling
  && left.origin = right.origin
  && left.explicit_register = right.explicit_register

let selection_name = function
  | Unspecified -> "unspecified"
  | Allocatable -> "reg"
  | Disabled -> "noreg"
  | Explicit register -> register.spelling
