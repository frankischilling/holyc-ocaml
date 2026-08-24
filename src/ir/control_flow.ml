type kind =
  | Fallthrough
  | Label
  | Unconditional_branch
  | Conditional_branch
  | Switch
  | Return
  | End

type target_shape = No_target | Single_target | Switch_targets

let first_conditional_code = Opcode.to_code Opcode.Ic_br_zero
let last_conditional_code = Opcode.to_code Opcode.Ic_br_not_btc

let is_conditional opcode =
  let code = Opcode.to_code opcode in
  code >= first_conditional_code && code <= last_conditional_code

let classify opcode =
  match opcode with
  | Opcode.Ic_end -> End
  | Opcode.Ic_label -> Label
  | Opcode.Ic_jmp -> Unconditional_branch
  | Opcode.Ic_switch | Opcode.Ic_nobound_switch -> Switch
  | Opcode.Ic_ret -> Return
  | _ when is_conditional opcode -> Conditional_branch
  | _ -> Fallthrough

let target_shape opcode =
  match classify opcode with
  | Unconditional_branch | Conditional_branch -> Single_target
  | Switch -> Switch_targets
  | Fallthrough | Label | Return | End -> No_target

let ends_block opcode =
  match classify opcode with
  | Unconditional_branch | Conditional_branch | Switch | Return | End -> true
  | Fallthrough | Label -> false

let may_fall_through opcode =
  match classify opcode with
  | Fallthrough | Label | Conditional_branch -> true
  | Unconditional_branch | Switch | Return | End -> false

let starts_label opcode = classify opcode = Label

let kind_name = function
  | Fallthrough -> "fallthrough"
  | Label -> "label"
  | Unconditional_branch -> "unconditional-branch"
  | Conditional_branch -> "conditional-branch"
  | Switch -> "switch"
  | Return -> "return"
  | End -> "end"

let target_shape_name = function
  | No_target -> "none"
  | Single_target -> "single"
  | Switch_targets -> "switch-targets"
