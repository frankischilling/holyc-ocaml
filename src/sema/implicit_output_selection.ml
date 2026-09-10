type target =
  | Module of Module_expression_binding.publication
  | Outer of Outer_environment.binding
  | Unavailable

let resolve ~table ~environment ~publications ~name selection =
  let ( let* ) = Result.bind in
  let* () = Reference_selection.validate ~table ~name selection in
  match Reference_selection.kind selection with
  | Reference_selection.Absent
  | Reference_selection.Unavailable
  | Reference_selection.Local -> Ok Unavailable
  | Reference_selection.Source (symbol, stage) -> (
      match stage with
      | Reference_selection.Function_header_completed
      | Reference_selection.Function_body_completed -> (
          match
            List.find_opt
              (fun publication ->
                Module_expression_binding.publication_source_symbol publication
                == symbol
                && Module_expression_binding.publication_kind publication
                   = Module_expression_binding.Function)
              publications
          with
          | Some publication -> Ok (Module publication)
          | None -> Error "implicit selection has no original module function")
      | _ -> Ok Unavailable)
  | Reference_selection.Outer (owner, binding) ->
      if
        owner != environment
        || not (Outer_environment.owns_binding environment binding)
      then Error "implicit selection has another exact outer environment"
      else if
        Outer_environment.entry_record_kind
          (Outer_environment.binding_entry binding)
        <> Outer_environment.Function
      then Error "implicit selection has a nonfunction record"
      else Ok (Outer binding)
