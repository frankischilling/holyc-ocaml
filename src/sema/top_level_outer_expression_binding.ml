type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type occurrence = {
  source : Top_level_expression_binding.occurrence;
  resolution : resolution;
}

type statement = {
  source : Top_level_expression_binding.statement;
  occurrences : occurrence list;
}

type t = {
  table : Symbol_table.t;
  environment_ : Outer_environment.t;
  source_ : Top_level_expression_binding.t;
  statements_ : statement list;
  all_occurrences_ : occurrence list;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error = { code : string; kind : error_kind; origin : Symbol.origin option }

let invalid_input message =
  { code = "HCSEMA0053"; kind = Invalid_input message; origin = None }

let unresolved_identifier occurrence compilation_mode =
  {
    code = "HCSEMA0054";
    kind =
      Unresolved_identifier
        {
          name = Top_level_expression_binding.occurrence_name occurrence;
          compilation_mode;
        };
    origin = Some (Top_level_expression_binding.occurrence_origin occurrence);
  }

let owns_table result table = result.table == table
let environment result = result.environment_
let source result = result.source_
let statements result = result.statements_
let all_occurrences result = result.all_occurrences_
let statement_source (statement : statement) = statement.source

let statement_index (statement : statement) =
  Top_level_expression_binding.statement_index statement.source

let statement_item_index (statement : statement) =
  Top_level_expression_binding.statement_item_index statement.source

let statement_origin (statement : statement) =
  Top_level_expression_binding.statement_origin statement.source

let statement_occurrences (statement : statement) = statement.occurrences
let occurrence_source (occurrence : occurrence) = occurrence.source

let occurrence_index (occurrence : occurrence) =
  Top_level_expression_binding.occurrence_index occurrence.source

let occurrence_name (occurrence : occurrence) =
  Top_level_expression_binding.occurrence_name occurrence.source

let occurrence_origin (occurrence : occurrence) =
  Top_level_expression_binding.occurrence_origin occurrence.source

let occurrence_resolution (occurrence : occurrence) = occurrence.resolution
let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message
  | Unresolved_identifier { name; compilation_mode } ->
      Printf.sprintf
        "top-level identifier %S is absent from the complete %s outer table \
         chain"
        name
        (Outer_environment.compilation_mode_name compilation_mode)

let error_to_string error = error.code ^ ": " ^ error_message error

let resolve_occurrence environment source =
  match Top_level_expression_binding.occurrence_resolution source with
  | Top_level_expression_binding.Module_binding publication ->
      Ok { source; resolution = Module_binding publication }
  | Top_level_expression_binding.Outer_candidate -> (
      match
        Outer_environment.find environment
          (Top_level_expression_binding.occurrence_name source)
      with
      | Some binding -> Ok { source; resolution = Outer_binding binding }
      | None ->
          Error
            (unresolved_identifier source
               (Outer_environment.compilation_mode environment)))

let resolve_occurrences environment occurrences =
  let rec loop resolved_rev = function
    | [] -> Ok (List.rev resolved_rev)
    | occurrence :: rest -> (
        match resolve_occurrence environment occurrence with
        | Error _ as error -> error
        | Ok resolved -> loop (resolved :: resolved_rev) rest)
  in
  loop [] occurrences

let resolve_statements environment statements =
  let rec loop statements_rev occurrences_rev = function
    | [] -> Ok (List.rev statements_rev, List.rev occurrences_rev)
    | source :: rest -> (
        match
          resolve_occurrences environment
            (Top_level_expression_binding.statement_occurrences source)
        with
        | Error _ as error -> error
        | Ok occurrences ->
            loop
              ({ source; occurrences } :: statements_rev)
              (List.rev_append occurrences occurrences_rev)
              rest)
  in
  loop [] [] statements

let resolve ~table ~environment ~expressions =
  if not (Top_level_expression_binding.owns_table expressions table) then
    Error
      (invalid_input
         "top-level expression bindings belong to another symbol table")
  else if not (Outer_environment.owns_table environment table) then
    Error (invalid_input "outer environment belongs to another symbol table")
  else
    let module_expressions =
      Top_level_expression_binding.module_expressions expressions
    in
    if
      Module_expression_binding.compilation_mode module_expressions
      <> Outer_environment.compilation_mode environment
    then
      Error
        (invalid_input
           "top-level expression bindings and outer environment use different \
            compilation modes")
    else
      match
        resolve_statements environment
          (Top_level_expression_binding.statements expressions)
      with
      | Error _ as error -> error
      | Ok (statements_, all_occurrences_) ->
          Ok
            {
              table;
              environment_ = environment;
              source_ = expressions;
              statements_;
              all_occurrences_;
            }
