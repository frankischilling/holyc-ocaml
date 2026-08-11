type t = Date | Time | Line | Command_line | File | Directory

let all = [ Date; Time; Line; Command_line; File; Directory ]

let spelling = function
  | Date -> "__DATE__"
  | Time -> "__TIME__"
  | Line -> "__LINE__"
  | Command_line -> "__CMD_LINE__"
  | File -> "__FILE__"
  | Directory -> "__DIR__"

let find name =
  List.find_opt (fun item -> String.equal (spelling item) name) all

module Settings = struct
  type t = { date : string; time : string; command_line : bool }

  let digit text index = Char.code text.[index] - Char.code '0'

  let has_digit text index =
    match text.[index] with
    | '0' .. '9' -> true
    | _ -> false

  let two_digits text index = (digit text index * 10) + digit text (index + 1)

  let valid_date text =
    String.length text = 8
    && Char.equal text.[2] '/'
    && Char.equal text.[5] '/'
    && List.for_all (has_digit text) [ 0; 1; 3; 4; 6; 7 ]
    &&
    let month = two_digits text 0 in
    let day = two_digits text 3 in
    let year = two_digits text 6 in
    let leap = year mod 4 = 0 in
    let days =
      match month with
      | 2 -> if leap then 29 else 28
      | 4 | 6 | 9 | 11 -> 30
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
      | _ -> 0
    in
    day >= 1 && day <= days

  let valid_time text =
    String.length text = 8
    && Char.equal text.[2] ':'
    && Char.equal text.[5] ':'
    && List.for_all (has_digit text) [ 0; 1; 3; 4; 6; 7 ]
    && two_digits text 0 < 24
    && two_digits text 3 < 60
    && two_digits text 6 < 60

  let create ?(date = "01/01/70") ?(time = "00:00:00") ?(command_line = false)
      () =
    if not (valid_date date) then
      Error "predefined date must be a valid MM/DD/YY value"
    else if not (valid_time time) then
      Error "predefined time must be a valid HH:MM:SS value"
    else Ok { date; time; command_line }

  let date settings = settings.date
  let time settings = settings.time
  let command_line settings = settings.command_line
end

let standard_body = function
  | Date -> {|#exe{StreamPrint("\"%D\"",Now);}|}
  | Time -> {|#exe{StreamPrint("\"%T\"",Now);}|}
  | Line -> {|#exe{StreamPrint("%d",Fs->last_cc->lex_include_stk->line_num);}|}
  | Command_line ->
      {|#exe{StreamPrint("%d",Fs->last_cc->flags&CCF_CMD_LINE&&Fs->last_cc->lex_include_stk->depth<1);}|}
  | File ->
      {|#exe{StreamPrint("\"%s\"",Fs->last_cc->lex_include_stk->full_name);}|}
  | Directory -> {|#exe{StreamDir;}|}

let token_spellings text =
  match Common.Source_id.of_int 0 with
  | Error _ -> None
  | Ok id -> (
      let source =
        Common.Source_file.create ~id ~path:"<predefined-check>"
          ~display_path:"<predefined-check>" ~contents:text
      in
      match Lexer.lex_all source with
      | Error _ -> None
      | Ok tokens -> Some (List.map (fun token -> token.Token.raw) tokens))

let matches_standard_body item replacement =
  match (token_spellings replacement, token_spellings (standard_body item)) with
  | Some actual, Some expected -> actual = expected
  | _ -> false

let quote text =
  let buffer = Buffer.create (String.length text + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (fun byte ->
      match byte with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '$' -> Buffer.add_string buffer "$$"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | byte ->
          let code = Char.code byte in
          if code >= 0x20 && code <= 0x7e then Buffer.add_char buffer byte
          else Printf.bprintf buffer "\\x%02X" code)
    text;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let expand settings item ~source ~line ~source_depth =
  match item with
  | Date -> quote (Settings.date settings)
  | Time -> quote (Settings.time settings)
  | Line -> string_of_int line
  | Command_line ->
      if Settings.command_line settings && source_depth < 1 then "1" else "0"
  | File -> quote (Common.Source_file.path source)
  | Directory -> quote (Filename.dirname (Common.Source_file.path source))
