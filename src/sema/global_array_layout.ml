type dimension_input = {
  dimension : Global_dimension_binding.resolved_dimension;
  expression_origin : Symbol.origin option;
  expression : Frontend.Ast.expression option;
}

type global_input = {
  global : Global_dimension_binding.resolved_global;
  dimensions : dimension_input list;
}

type layout = {
  source_ : global_input;
  dimensions_ : int64 list;
  element_count_ : int64;
}

type t = {
  table : Symbol_table.t;
  bindings_ : Global_dimension_binding.t;
  layouts_ : layout list;
}

let owns_table result table = result.table == table
let owns_bindings result bindings = result.bindings_ == bindings
let bindings result = result.bindings_
let layouts result = result.layouts_
let source layout = layout.source_.global
let record layout = Global_dimension_binding.global_record (source layout)
let dimension_inputs layout = layout.source_.dimensions
let dimensions layout = layout.dimensions_
let element_count layout = layout.element_count_

let find result selected =
  List.find_opt (fun layout -> record layout == selected) result.layouts_

let ( let* ) = Result.bind
let invalid detail = Error ("HCSEMA0027: " ^ detail)

let invalid_extent global index detail =
  invalid
    (Printf.sprintf "global array dimension %d for %S %s" index
       (Global_dimension_binding.global_symbol global |> Symbol.name)
       detail)

let rec validate_closed_expression global index = function
  | Aggregate_layout.Integer_expression _
  | Aggregate_layout.Unsigned_integer_expression _
  | Aggregate_layout.Floating_expression _ -> Ok ()
  | Aggregate_layout.Current_position_expression _ ->
      invalid_extent global index
        "requires unresolved current-position layout evidence"
  | Aggregate_layout.Dependency_expression { detail; _ } ->
      invalid_extent global index
        ("requires unresolved closed layout evidence: " ^ detail)
  | Aggregate_layout.Unsupported_expression { description; _ } ->
      invalid_extent global index
        ("has an unsupported closed layout expression: " ^ description)
  | Aggregate_layout.Unary_expression { operand; _ } ->
      validate_closed_expression global index operand
  | Aggregate_layout.Binary_expression { left; right; _ } ->
      let* () = validate_closed_expression global index left in
      validate_closed_expression global index right

let evaluate_dimension global index input =
  let dimension = input.dimension in
  if Global_dimension_binding.dimension_index dimension <> index then
    invalid "global array layout dimensions are outside source order"
  else if
    input.expression_origin
    <> Global_dimension_binding.dimension_expression_origin dimension
  then invalid "global array layout expression has the wrong source origin"
  else
    match (input.expression_origin, input.expression) with
    | None, None ->
        invalid_extent global index
          "has an empty extent; inferred persistent arrays are unresolved"
    | None, Some _ | Some _, None ->
        invalid "global array layout expression does not match its dimension"
    | Some _, Some expression ->
        let* () =
          match
            dimension |> Global_dimension_binding.dimension_source
            |> Global_type_resolution.array_dimension_source_expression
          with
          | Some original when expression == original -> Ok ()
          | Some _ | None ->
              invalid
                "global array layout requires its original source expression"
        in
        let* () =
          match Global_dimension_binding.dimension_occurrences dimension with
          | [] -> Ok ()
          | occurrence :: _ ->
              invalid_extent global index
                (Printf.sprintf
                   "requires an evaluated value for bound identifier %S"
                   (Global_dimension_binding.occurrence_name occurrence))
        in
        let expression = Closed_layout_expression.of_ast expression in
        let* () = validate_closed_expression global index expression in
        let* value =
          Aggregate_layout.evaluate_expression
            ~context:Aggregate_layout.Array_dimension ~current_position:0L
            expression
          |> Result.map_error (fun error ->
              Printf.sprintf "HCSEMA0027: global array dimension %d for %S: %s"
                index
                (Global_dimension_binding.global_symbol global |> Symbol.name)
                (Aggregate_layout.error_to_string error))
        in
        if Int64.compare value 0L <= 0 then
          invalid_extent global index
            (Printf.sprintf
               "has nonpositive extent %Ld; persistent arrays require a \
                positive fixed extent"
               value)
        else Ok value

let evaluate_dimensions global inputs =
  let semantic =
    global |> Global_dimension_binding.global_record
    |> Global_resolution.global_record_global
    |> Global_type_resolution.global_array_dimensions
  in
  let rec loop index count reversed semantic dimensions inputs =
    match (semantic, dimensions, inputs) with
    | [], [], [] -> Ok (count, List.rev reversed)
    | expected :: semantic_rest, dimension :: rest, input :: tail ->
        if
          dimension != input.dimension
          || Global_dimension_binding.dimension_source dimension != expected
        then invalid "global array layout has foreign dimension evidence"
        else
          let* value = evaluate_dimension global index input in
          if Int64.compare count (Int64.div Int64.max_int value) > 0 then
            invalid_extent global index "overflows the declared element count"
          else if index = max_int then
            invalid "global array layout dimension identity space is exhausted"
          else
            loop (index + 1) (Int64.mul count value) (value :: reversed)
              semantic_rest rest tail
    | _ -> invalid "global array layout dimensions do not match their owner"
  in
  loop 0 1L [] semantic
    (Global_dimension_binding.global_dimensions global)
    inputs

let layout ~table ~bindings inputs =
  if not (Global_dimension_binding.owns_table bindings table) then
    invalid "global array layout bindings belong to another symbol table"
  else
    let records =
      Global_dimension_binding.source_globals bindings
      |> Global_resolution.records
    in
    let rec loop reversed records globals inputs =
      match (records, globals, inputs) with
      | [], [], [] ->
          Ok { table; bindings_ = bindings; layouts_ = List.rev reversed }
      | record :: records, global :: globals, input :: inputs ->
          let symbol = Global_resolution.global_record_symbol record in
          if
            input.global != global
            || Global_dimension_binding.global_record global != record
          then invalid "global array layout has foreign declaration evidence"
          else if not (Symbol_table.owns_symbol table symbol) then
            invalid "global array layout owner belongs to another symbol table"
          else if
            Global_dimension_binding.global_symbol global != symbol
            || Global_resolution.global_record_global record
               |> Global_type_resolution.global_symbol != symbol
          then invalid "global array layout has inconsistent symbol identities"
          else
            let* element_count_, dimensions_ =
              evaluate_dimensions global input.dimensions
            in
            loop
              ({ source_ = input; dimensions_; element_count_ } :: reversed)
              records globals inputs
      | _ ->
          invalid "global array layout inputs do not match their binding batch"
    in
    loop [] records (Global_dimension_binding.globals bindings) inputs
