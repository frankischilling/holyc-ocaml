let contains ~table ~scope metadata definition =
  List.exists
    (Sema.Compiler_record.inherited_metadata_owns_definition ~table ~scope
       definition)
    metadata

type storage_selection = {
  symbol : Sema.Symbol.t;
  size : int64;
  base_symbol : Sema.Symbol.t;
  base_size : int64;
}

type storage = {
  metadata_only : Sema.Compiler_record.inherited_metadata list;
  selections : storage_selection list;
}

let prepare_storage ~table ~scope ~aggregates ~ast metadata =
  let identities =
    Sema.Aggregate_resolution.declarations aggregates
    |> List.map (fun declaration ->
        ( Sema.Aggregate_resolution.declaration_site_item_index
            (Sema.Aggregate_resolution.resolved_declaration_site declaration),
          Sema.Aggregate_resolution.resolved_declaration_identity_symbol
            declaration ))
  in
  let definitions =
    Frontend.Ast.declaration_items ast
    |> List.filter_map (function
      | index, Frontend.Ast.Aggregate_definition definition ->
          Option.map
            (fun symbol -> (definition, symbol))
            (List.assoc_opt index identities)
      | _ -> None)
  in
  let selection definition =
    List.find_map
      (fun proof ->
        match
          Sema.Compiler_record.inherited_metadata_storage_selection ~table
            ~scope proof
        with
        | Some (original, symbol, size, base, base_symbol, base_size)
          when original == definition ->
            Some (proof, symbol, size, base, base_symbol, base_size)
        | _ -> None)
      metadata
  in
  let rec loop ready admitted selections = function
    | [] ->
        {
          metadata_only =
            List.filter (fun proof -> not (List.memq proof admitted)) metadata;
          selections = List.rev selections;
        }
    | (definition, identity) :: rest -> (
        match selection definition with
        | Some (proof, symbol, size, base, base_symbol, base_size)
          when symbol == identity
               && List.exists
                    (fun (original, symbol) ->
                      original == base && symbol == base_symbol)
                    ready ->
            loop
              ((definition, identity) :: ready)
              (proof :: admitted)
              ({ symbol; size; base_symbol; base_size } :: selections)
              rest
        | _ ->
            let ready =
              if contains ~table ~scope metadata definition then ready
              else (definition, identity) :: ready
            in
            loop ready admitted selections rest)
  in
  loop [] [] [] definitions

let metadata_only storage = storage.metadata_only

let validate_storage storage ~layouts =
  let rec loop = function
    | [] -> Ok ()
    | selection :: rest -> (
        match Sema.Aggregate_layout.find layouts selection.symbol with
        | Some layout
          when layout.symbol == selection.symbol && layout.size = selection.size
          -> (
            match layout.base with
            | Some base
              when base.symbol == selection.base_symbol
                   && base.size = selection.base_size
                   && base.offset = 0L -> loop rest
            | _ ->
                Error
                  "inherited object layout substituted its original completed \
                   base")
        | _ ->
            Error
              "inherited object layout differs from its original completed \
               definition")
  in
  loop storage.selections
