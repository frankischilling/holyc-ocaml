type kind =
  | Fallthrough
  | Label
  | Unconditional_branch
  | Conditional_branch
  | Switch
  | Return
  | End

type target_shape = No_target | Single_target | Switch_targets

val classify : Opcode.t -> kind
val target_shape : Opcode.t -> target_shape
val ends_block : Opcode.t -> bool
val may_fall_through : Opcode.t -> bool
val starts_label : Opcode.t -> bool
val kind_name : kind -> string
val target_shape_name : target_shape -> string
