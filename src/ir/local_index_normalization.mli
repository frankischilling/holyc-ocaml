(** Bounded integer/address fragment of the pinned OptPass012 passes. This
    module does not authenticate source ownership or authorize lowering. *)
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

val normalize : ?preliminary_passes:int -> 'a tree -> 'a tree option
(** Run pass 1 and pass 2 in source postorder, without revisiting a rewritten
    parent. Only representable signed constant arithmetic is admitted. Dynamic
    multiplication by zero retains its operand. Each enclosing authenticated
    provided fixed argument on the direct PrsFunCall path contributes two
    preliminary non-pass-2 passes. This includes source-declared numeric
    intrinsics using that path. Defaults, variadic arguments, indirect calls and
    unowned AST call shapes do not authorize this schedule. *)

val scalar_reads : 'a tree -> 'a list

val frame_displacements : 'a tree -> int64 list option
(** Displacements of the actual surviving RBP ADDs. A naked RBP rejects. *)
