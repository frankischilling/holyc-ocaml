type kind = Root | Included | Definition | Predefined

type t = {
  kind : kind;
  source : Common.Source_file.t;
  lexer : Lexer.t;
  caller : t option;
  include_origin : Common.Span.t option;
  include_spelling : string option;
  source_depth : int;
  definition_depth : int;
  definition : Definition.t option;
  definition_invocation : Common.Span.t option;
  predefined : Predefined.t option;
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
    definition_depth = 0;
    definition = None;
    definition_invocation = None;
    predefined = None;
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
    definition_depth = caller.definition_depth;
    definition = None;
    definition_invocation = None;
    predefined = None;
  }

let push_definition ~caller ~source ~definition ~invocation_span =
  {
    kind = Definition;
    source;
    lexer =
      Lexer.create ~mode:Token.Holyc ~generated_from:invocation_span
        ~defined_at:(Definition.definition_span definition)
        source;
    caller = Some caller;
    include_origin = None;
    include_spelling = None;
    source_depth = caller.source_depth;
    definition_depth = caller.definition_depth + 1;
    definition = Some definition;
    definition_invocation = Some invocation_span;
    predefined = None;
  }

let push_predefined ~caller ~source ~predefined ~invocation_span =
  {
    kind = Predefined;
    source;
    lexer =
      Lexer.create ~mode:Token.Holyc ~generated_from:invocation_span source;
    caller = Some caller;
    include_origin = None;
    include_spelling = None;
    source_depth = caller.source_depth;
    definition_depth = caller.definition_depth + 1;
    definition = None;
    definition_invocation = Some invocation_span;
    predefined = Some predefined;
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
let definition_depth frame = frame.definition_depth
let definition frame = frame.definition
let predefined frame = frame.predefined
let current_offset frame = Lexer.offset frame.lexer

let current_position frame =
  Common.Source_file.position frame.source (current_offset frame)

let include_stack frame =
  let rec collect found current =
    let found =
      match (current.include_origin, current.include_spelling) with
      | Some span, Some spelling ->
          let item : Common.Diagnostic.related =
            { span; message = Printf.sprintf "#include %S" spelling }
          in
          item :: found
      | _ -> found
    in
    match current.caller with
    | None -> found
    | Some caller -> collect found caller
  in
  collect [] frame

let definition_trace frame =
  let rec collect current =
    let here =
      match
        (current.definition, current.predefined, current.definition_invocation)
      with
      | Some definition, _, Some invocation ->
          let name = Definition.name definition in
          [
            {
              Common.Diagnostic.span = invocation;
              message = Printf.sprintf "definition %S was expanded here" name;
            };
            {
              Common.Diagnostic.span = Definition.name_span definition;
              message = Printf.sprintf "definition %S was declared here" name;
            };
          ]
      | None, Some predefined, Some invocation ->
          [
            {
              Common.Diagnostic.span = invocation;
              message =
                Printf.sprintf "predefined value %S was expanded here"
                  (Predefined.spelling predefined);
            };
          ]
      | _ -> []
    in
    let rest =
      match current.caller with
      | None -> []
      | Some caller -> collect caller
    in
    here @ rest
  in
  collect frame

let find_active_path frame path =
  let rec find current =
    let matches =
      match current.kind with
      | Root | Included ->
          Include_resolver.equal_path path
            (Common.Source_file.path current.source)
      | Definition | Predefined -> false
    in
    if matches then Some current
    else
      match current.caller with
      | None -> None
      | Some caller -> find caller
  in
  find frame

let find_active_definition frame id =
  let rec find current =
    match current.definition with
    | Some definition when Definition.id definition = id -> Some current
    | _ -> (
        match current.caller with
        | None -> None
        | Some caller -> find caller)
  in
  find frame
