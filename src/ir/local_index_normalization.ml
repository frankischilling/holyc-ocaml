type 'a tree =
  | Immediate of int64
  | Scalar of 'a
  | Frame
  | Negate of 'a tree
  | Scalar_add of 'a tree * 'a tree
  | Scalar_subtract of 'a tree * 'a tree
  | Add of 'a tree * 'a tree
  | Subtract of 'a tree * 'a tree
  | Multiply of 'a tree * 'a tree
  | Shift_left of 'a tree * int

exception Unproved

let add a b =
  if
    (b > 0L && a > Int64.sub Int64.max_int b)
    || (b < 0L && a < Int64.sub Int64.min_int b)
  then raise Unproved;
  Int64.add a b

let subtract a b =
  if
    (b > 0L && a < Int64.add Int64.min_int b)
    || (b < 0L && a > Int64.add Int64.max_int b)
  then raise Unproved;
  Int64.sub a b

let multiply a b =
  if a = 0L || b = 0L then 0L
  else (
    if (a = Int64.min_int && b = -1L) || (b = Int64.min_int && a = -1L) then
      raise Unproved;
    let result = Int64.mul a b in
    if Int64.div result b <> a then raise Unproved;
    result)

let power_of_two n =
  if n = Int64.min_int then Some 63
  else if n <= 0L || Int64.logand n (Int64.pred n) <> 0L then None
  else
    let rec count n bits =
      if n = 1L then bits else count (Int64.shift_right_logical n 1) (bits + 1)
    in
    Some (count n 0)

(* PrsLib.HC:257-263 invokes precisely these two passes. The nested immediate
   rewrites below mutate the child and return it; they do not re-run that child
   in the same pass. Flattening additions or iterating to a fixed point is not
   equivalent, especially when a dynamic dimension precedes a constant one. *)
let rec pass second = function
  | (Immediate _ | Scalar _ | Frame) as tree -> tree
  | Shift_left (tree, bits) -> (
      if bits < 0 || bits >= 64 then raise Unproved;
      match pass second tree with
      | Immediate n when bits = 63 ->
          (* Only these mathematical products fit a signed word. The source
             SHL preserves the same bits for signed and unsigned operands. *)
          if n = 0L then Immediate 0L
          else if n = -1L then Immediate Int64.min_int
          else raise Unproved
      | Immediate n -> Immediate (multiply n (Int64.shift_left 1L bits))
      | Shift_left (tree, inner) ->
          if inner + bits >= 64 then raise Unproved;
          Shift_left (tree, inner + bits)
      | tree -> Shift_left (tree, bits))
  | Negate tree -> (
      match pass second tree with
      | Immediate n -> Immediate (subtract 0L n)
      | tree -> Negate tree)
  (* PrsExp.HC:173-180 inserts left MUL(..., SIZEOF(1)) for source
     scalar +/-; OptFixSizeOf runs only after the parent's first rewrite
     attempt (OptPass012.HC:1259-1265). The scalar opposite class deletes that
     wrapper then, so immediate +/- folds must wait for the next pass. *)
  | Scalar_add (left, right) -> Add (pass second left, pass second right)
  | Scalar_subtract (left, right) ->
      Subtract (pass second left, pass second right)
  | Add (left, right) -> (
      let left = pass second left and right = pass second right in
      match (left, right) with
      | Immediate a, Immediate b -> Immediate (add a b)
      | (Immediate n, tree | tree, Immediate n) when second -> (
          if n = 0L then tree
          else
            match tree with
            | Add (Immediate a, tail) -> Add (Immediate (add a n), tail)
            | Subtract (Immediate a, tail) ->
                Subtract (Immediate (add a n), tail)
            | Add (head, Immediate b) -> Add (head, Immediate (add b n))
            | Subtract (head, Immediate b) ->
                Subtract (head, Immediate (subtract b n))
            | _ -> Add (left, right))
      | _ -> Add (left, right))
  | Subtract (left, right) -> (
      let left = pass second left and right = pass second right in
      match (left, right) with
      | Immediate a, Immediate b -> Immediate (subtract a b)
      | tree, Immediate n when second -> (
          if n = 0L then tree
          else
            match tree with
            | Add (Immediate a, tail) -> Add (Immediate (subtract a n), tail)
            | Subtract (Immediate a, tail) ->
                Subtract (Immediate (subtract a n), tail)
            | Add (head, Immediate b) -> Add (head, Immediate (subtract b n))
            | Subtract (head, Immediate b) ->
                Subtract (head, Immediate (add b n))
            | _ -> Subtract (left, right))
      | _ -> Subtract (left, right))
  | Multiply (left, right) -> (
      let left = pass second left and right = pass second right in
      match (left, right) with
      | Immediate a, Immediate b -> Immediate (multiply a b)
      (* OptPass012.HC:323-375: left immediate rewrites wait for pass 2;
         right immediate rewrites already run in pass 1. Zero is never an
         annihilator, because that would delete the scalar access subtree. *)
      | Immediate n, tree when second -> product left right n tree false
      | tree, Immediate n -> product left right n tree second
      | _ -> Multiply (left, right))

and product left right n tree combine =
  (* Bsf/Bsr tests the raw bit pattern, including the single sign bit. Its
     shift-63 rewrite retains the same scalar producer and happens at the same
     phase as every other power-of-two multiplier. *)
  if n = 0L then Multiply (left, right)
  else if n = 1L then tree
  else
    match power_of_two n with
    | Some bits -> Shift_left (tree, bits)
    | None when combine -> (
        match tree with
        | Multiply (Immediate a, tail) ->
            Multiply (Immediate (multiply a n), tail)
        | Multiply (head, Immediate b) ->
            Multiply (head, Immediate (multiply b n))
        | _ -> Multiply (left, right))
    | None -> Multiply (left, right)

let normalize ?(preliminary_passes = 0) tree =
  try
    if preliminary_passes < 0 || preliminary_passes > 64 then raise Unproved;
    (* PrsExp.HC:474-475 runs pass 0/1 on each provided fixed argument.
       In this integer fragment both use the same non-pass-2 rules. *)
    let rec prepare remaining tree =
      if remaining = 0 then tree else prepare (remaining - 1) (pass false tree)
    in
    Some (pass true (pass false (prepare preliminary_passes tree)))
  with Unproved -> None

let rec scalar_reads = function
  | Scalar value -> [ value ]
  | Immediate _ | Frame -> []
  | Negate tree | Shift_left (tree, _) -> scalar_reads tree
  | Scalar_add (left, right)
  | Scalar_subtract (left, right)
  | Add (left, right)
  | Subtract (left, right)
  | Multiply (left, right) -> scalar_reads left @ scalar_reads right

let frame_displacements tree =
  let rec visit = function
    | Add (Frame, Immediate n) -> [ n ]
    | Frame -> raise Unproved
    | Immediate _ | Scalar _ -> []
    | Negate tree | Shift_left (tree, _) -> visit tree
    | Scalar_add (left, right)
    | Scalar_subtract (left, right)
    | Add (left, right)
    | Subtract (left, right)
    | Multiply (left, right) -> visit left @ visit right
  in
  try Some (visit tree) with Unproved -> None
