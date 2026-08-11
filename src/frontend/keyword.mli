type t =
  | Include
  | Define
  | Union
  | Catch
  | Class
  | Try
  | If
  | Else
  | For
  | While
  | Extern
  | Underscore_extern
  | Return
  | Sizeof
  | Underscore_intern
  | Do
  | Asm
  | Goto
  | Exe
  | Break
  | Switch
  | Start
  | End
  | Case
  | Default
  | Public
  | Offset
  | Import
  | Underscore_import
  | Ifdef
  | Ifndef
  | Ifaot
  | Ifjit
  | Endif
  | Assert
  | Reg
  | Noreg
  | Lastclass
  | No_warn
  | Help_index
  | Help_file
  | Static
  | Lock
  | Defined
  | Interrupt
  | Haserrcode
  | Argpop
  | Noargpop

val all : (string * t * int) list
val find : string -> t option
val spelling : t -> string
val templeos_id : t -> int
val source_line : t -> int
val compare : t -> t -> int
