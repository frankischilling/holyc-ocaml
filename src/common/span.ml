type t = { source : Source_id.t; start : int; stop : int }

let make ~source ~length ~start ~stop =
  if length < 0 then Error "source length must be nonnegative"
  else if start < 0 then Error "span start must be nonnegative"
  else if stop < start then Error "span stop precedes its start"
  else if stop > length then
    Error (Printf.sprintf "span stop %d exceeds source length %d" stop length)
  else Ok { source; start; stop }

let unsafe_make ~source ~start ~stop =
  if start < 0 || stop < start then invalid_arg "invalid source span";
  { source; start; stop }

let length span = span.stop - span.start
let contains span offset = span.start <= offset && offset < span.stop

let compare left right =
  match Source_id.compare left.source right.source with
  | 0 -> (
      match Int.compare left.start right.start with
      | 0 -> Int.compare left.stop right.stop
      | result -> result)
  | result -> result

let pp formatter span =
  Format.fprintf formatter "%a:%d..%d" Source_id.pp span.source span.start
    span.stop
