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

let all =
  [
    ("include", Include, 0);
    ("define", Define, 1);
    ("union", Union, 2);
    ("catch", Catch, 3);
    ("class", Class, 4);
    ("try", Try, 5);
    ("if", If, 6);
    ("else", Else, 7);
    ("for", For, 8);
    ("while", While, 9);
    ("extern", Extern, 10);
    ("_extern", Underscore_extern, 11);
    ("return", Return, 12);
    ("sizeof", Sizeof, 13);
    ("_intern", Underscore_intern, 14);
    ("do", Do, 15);
    ("asm", Asm, 16);
    ("goto", Goto, 17);
    ("exe", Exe, 18);
    ("break", Break, 19);
    ("switch", Switch, 20);
    ("start", Start, 21);
    ("end", End, 22);
    ("case", Case, 23);
    ("default", Default, 24);
    ("public", Public, 25);
    ("offset", Offset, 26);
    ("import", Import, 27);
    ("_import", Underscore_import, 28);
    ("ifdef", Ifdef, 29);
    ("ifndef", Ifndef, 30);
    ("ifaot", Ifaot, 31);
    ("ifjit", Ifjit, 32);
    ("endif", Endif, 33);
    ("assert", Assert, 34);
    ("reg", Reg, 35);
    ("noreg", Noreg, 36);
    ("lastclass", Lastclass, 37);
    ("no_warn", No_warn, 38);
    ("help_index", Help_index, 39);
    ("help_file", Help_file, 40);
    ("static", Static, 41);
    ("lock", Lock, 42);
    ("defined", Defined, 43);
    ("interrupt", Interrupt, 44);
    ("haserrcode", Haserrcode, 45);
    ("argpop", Argpop, 46);
    ("noargpop", Noargpop, 47);
  ]

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

let compare left right = Int.compare (templeos_id left) (templeos_id right)
