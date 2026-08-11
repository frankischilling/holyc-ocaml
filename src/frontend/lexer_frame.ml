type kind = Root | Included

type t = {
  kind : kind;
  source : Common.Source_file.t;
  lexer : Lexer.t;
  caller : t option;
  include_origin : Common.Span.t option;
  include_spelling : string option;
  source_depth : int;
}

let root ~mode source =
  {
    kind = Root;
    source;
    lexer = Lexer.create ~mode source;
    caller = None;
    include_origin = None;
    include_spelling = None;
    source_depth = -1;
  }

let push_include ~caller ~source ~include_origin ~include_spelling =
  {
    kind = Included;
    source;
    lexer = Lexer.create ~mode:Token.Holyc source;
    caller = Some caller;
    include_origin = Some include_origin;
    include_spelling = Some include_spelling;
    source_depth = caller.source_depth + 1;
  }

let kind frame = frame.kind
let source frame = frame.source
let source_id frame = Common.Source_file.id frame.source
let canonical_path frame = Common.Source_file.path frame.source
let display_path frame = Common.Source_file.display_path frame.source
let lexer frame = frame.lexer
let caller frame = frame.caller
let include_origin frame = frame.include_origin
let include_spelling frame = frame.include_spelling
let source_depth frame = frame.source_depth
let current_offset frame = Lexer.offset frame.lexer

let current_position frame =
  Common.Source_file.position frame.source (current_offset frame)

let include_stack frame =
  let rec collect found current =
    match
      (current.caller, current.include_origin, current.include_spelling)
    with
    | Some caller, Some span, Some spelling ->
        let item : Common.Diagnostic.related =
          { span; message = Printf.sprintf "#include %S" spelling }
        in
        collect (item :: found) caller
    | _ -> found
  in
  collect [] frame

let find_active_path frame path =
  let rec find current =
    if Include_resolver.equal_path path (Common.Source_file.path current.source)
    then Some current
    else
      match current.caller with
      | None -> None
      | Some caller -> find caller
  in
  find frame
