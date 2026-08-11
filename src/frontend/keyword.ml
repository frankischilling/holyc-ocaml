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

let constructors_by_id =
  [
    Include;
    Define;
    Union;
    Catch;
    Class;
    Try;
    If;
    Else;
    For;
    While;
    Extern;
    Underscore_extern;
    Return;
    Sizeof;
    Underscore_intern;
    Do;
    Asm;
    Goto;
    Exe;
    Break;
    Switch;
    Start;
    End;
    Case;
    Default;
    Public;
    Offset;
    Import;
    Underscore_import;
    Ifdef;
    Ifndef;
    Ifaot;
    Ifjit;
    Endif;
    Assert;
    Reg;
    Noreg;
    Lastclass;
    No_warn;
    Help_index;
    Help_file;
    Static;
    Lock;
    Defined;
    Interrupt;
    Haserrcode;
    Argpop;
    Noargpop;
  ]

let all =
  if
    List.length constructors_by_id
    <> List.length Generated.Opcode_keywords.language
  then invalid_arg "generated language keyword table has an unexpected length";
  List.map2
    (fun entry keyword ->
      ( entry.Generated.Opcode_keywords.spelling,
        keyword,
        entry.templeos_id ))
    Generated.Opcode_keywords.language constructors_by_id

let find text =
  List.find_map
    (fun (spelling, keyword, _) ->
      if String.equal spelling text then Some keyword else None)
    all

let lookup keyword =
  List.find (fun (_, candidate, _) -> candidate = keyword) all

let spelling keyword =
  let spelling, _, _ = lookup keyword in
  spelling

let templeos_id keyword =
  let _, _, id = lookup keyword in
  id

let source_line keyword =
  let id = templeos_id keyword in
  (List.nth Generated.Opcode_keywords.language id).source_line

let compare left right = Int.compare (templeos_id left) (templeos_id right)
