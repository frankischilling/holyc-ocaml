type t = {
  dimensions : int64 list;
  strides : int64 list;
  element_count : int;
  byte_size : int;
}

type error = Unsupported_type | Invalid_extent | Overflow

let dimensions shape = shape.dimensions
let strides shape = shape.strides
let element_count shape = shape.element_count
let byte_size shape = shape.byte_size

let padded_byte_size shape =
  if shape.byte_size > Int.max_int - 7 then None
  else Some ((shape.byte_size + 7) land lnot 7)

let create ~type_ ~dimensions =
  match Integer_scalar_storage.public_byte_size type_ with
  | None -> Error Unsupported_type
  | Some width ->
      let ( let* ) = Result.bind in
      let limit = Int64.of_int Int.max_int in
      let rec collect = function
        | [] -> Ok (1L, Int64.of_int width, [])
        | count :: rest ->
            if count <= 0L then Error Invalid_extent
            else
              let* elements, bytes, strides = collect rest in
              if count > Int64.div limit bytes then Error Overflow
              else
                Ok
                  ( Int64.mul count elements,
                    Int64.mul count bytes,
                    bytes :: strides )
      in
      let* elements, bytes, strides = collect dimensions in
      if elements > Int64.of_int Sys.max_array_length then Error Overflow
      else
        Ok
          {
            dimensions;
            strides;
            element_count = Int64.to_int elements;
            byte_size = Int64.to_int bytes;
          }
