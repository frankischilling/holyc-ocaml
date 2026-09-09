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
  extent_ : Compiler_record.global_extent;
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
let extent layout = layout.extent_
let dimensions layout = Compiler_record.global_extent_dimensions layout.extent_

let element_count layout =
  Compiler_record.global_extent_element_count layout.extent_

let find result selected =
  List.find_opt (fun layout -> record layout == selected) result.layouts_

let ( let* ) = Result.bind
let invalid detail = Error ("HCSEMA0027: " ^ detail)

let invalid_extent global index detail =
  invalid
    (Printf.sprintf "global array dimension %d for %S %s" index
       (Global_dimension_binding.global_symbol global |> Symbol.name)
       detail)

let evaluate_dimension ~table global index input =
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
        let record = Global_dimension_binding.global_record global in
        let source = Global_dimension_binding.dimension_source dimension in
        let queries =
          Global_dimension_binding.dimension_queries dimension
          |> Option.map (List.map Query_selection.checked_read)
        in
        (match Global_dimension_binding.dimension_prepared dimension with
          | Some prepared -> (
              match queries with
              | Some queries ->
                  Compiler_record.reuse_global_dimension ~table ~record
                    ~dimension:source ~queries prepared
              | None -> Error "checked global extent lacks its query manifest")
          | None ->
              Compiler_record.evaluate_global_dimension ~table ~record
                ~dimension:source ~queries)
        |> Result.map_error (fun detail ->
            Printf.sprintf "HCSEMA0027: global array dimension %d for %S%s%s"
              index
              (Global_dimension_binding.global_symbol global |> Symbol.name)
              (if String.starts_with ~prefix:": " detail then "" else " ")
              detail)

let evaluate_dimensions ~table global inputs =
  let semantic =
    global |> Global_dimension_binding.global_record
    |> Global_resolution.global_record_global
    |> Global_type_resolution.global_array_dimensions
  in
  let rec loop index count reversed semantic dimensions inputs =
    match (semantic, dimensions, inputs) with
    | [], [], [] ->
        Compiler_record.make_global_extent ~table
          ~record:(Global_dimension_binding.global_record global)
          (List.rev reversed)
        |> Result.map_error (fun detail -> "HCSEMA0027: " ^ detail)
    | expected :: semantic_rest, dimension :: rest, input :: tail ->
        if
          dimension != input.dimension
          || Global_dimension_binding.dimension_source dimension != expected
        then invalid "global array layout has foreign dimension evidence"
        else
          let* value = evaluate_dimension ~table global index input in
          let extent_count =
            Compiler_record.global_dimension_extent_count value
          in
          if Int64.compare count (Int64.div Int64.max_int extent_count) > 0 then
            invalid_extent global index "overflows the declared element count"
          else if index = max_int then
            invalid "global array layout dimension identity space is exhausted"
          else
            loop (index + 1)
              (Int64.mul count extent_count)
              (value :: reversed) semantic_rest rest tail
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
            let* extent_ = evaluate_dimensions ~table global input.dimensions in
            loop
              ({ source_ = input; extent_ } :: reversed)
              records globals inputs
      | _ ->
          invalid "global array layout inputs do not match their binding batch"
    in
    loop [] records (Global_dimension_binding.globals bindings) inputs
