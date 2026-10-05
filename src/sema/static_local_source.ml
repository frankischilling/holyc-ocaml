module Frame = Function_frame_layout
module Local = Local_type_resolution
module Record = Compiler_record
module Parser = Frontend.Parser

type t = {
  allocation_ : Record.static_allocation;
  frame_ : Frame.function_layout;
  location_ : Frame.location;
}

let allocation value = value.allocation_
let frame value = value.frame_
let location value = value.location_

let bind ~allocation ~frame ~location =
  let ( let* ) = Result.bind in
  let receipt = Record.static_allocation_receipt allocation in
  let symbol = Frame.location_symbol location in
  let* () =
    if
      receipt.allocation_storage <> Frontend.Ast.Static_local
      || Frame.location_kind location <> Frame.Static_local
      || Option.is_some (Frame.location_frame_slot location)
      || (not
            (Symbol_table.owns_symbol
               (Record.static_allocation_table allocation)
               symbol))
      || (not
            (Symbol_table.owns_symbol
               (Record.static_allocation_table allocation)
               (Frame.function_symbol frame)))
      || not
           (Option.fold ~none:false ~some:(( == ) location)
              (Frame.find_location frame symbol))
    then Error "static source requires its exact declaring frame and location"
    else Ok ()
  in
  let* () =
    let publication = Record.static_allocation_publication allocation in
    if
      Frame.function_symbol frame
      != Declaration_collection.publication_symbol publication
    then Error "static source has another original function symbol"
    else
      match
        Frame.function_header frame
        |> Function_type_resolution.function_completed_header
      with
      | Some header
        when header.Parser.function_publication == receipt.allocation_function
        -> Ok ()
      | None -> (
          match Symbol.origin (Frame.function_symbol frame) with
          | Symbol.Source_location actual
            when actual.span
                 == receipt.allocation_function.function_name.location.span ->
              Ok ()
          | _ -> Error "static source has another original function declaration"
          )
      | _ -> Error "static source has another original function header"
  in
  let* local =
    match Frame.location_local_source location with
    | Some local
      when Local.local_storage local = Local.Static
           && Local.local_symbol local == symbol -> Ok local
    | _ -> Error "static source lacks its original checked local evidence"
  in
  let* source_dimensions =
    match receipt.allocation_local.local_source with
    | Parser.Local_variable source -> (
        let* () =
          match
            ( source.local_function_pointer,
              Frame.location_callback_pointer location )
          with
          | None, None -> Ok ()
          | Some original, Some pointer
            when Option.fold ~none:false ~some:(( == ) original)
                   (Function_type_resolution.function_pointer_source pointer) ->
              Ok ()
          | _ -> Error "static source has another original callback declarator"
        in
        let* () =
          match
            Source_type_reference.builtin source.local_type_specifier
              source.local_pointer_layers
          with
          | Ok reference
            when Type.equal
                   (Type_reference.resolved_type reference)
                   (Frame.location_checked_type location) -> Ok ()
          | Error _ when Option.is_some source.local_function_pointer ->
              (* A named callback return class retains its checked type in the
                 frame. The original callback above proves the physical pointer
                 declarator independently of that return class. *)
              Ok ()
          | _ -> Error "static source has another original local type"
        in
        match Symbol.origin symbol with
        | Symbol.Source_location actual
          when actual.span == source.local_name.location.span ->
            Ok source.local_array_dimensions
        | _ -> Error "static source has another original local declaration")
    | _ -> Error "static source requires an original local variable"
  in
  let local_dimensions = Local.local_array_dimensions local in
  let checked = Record.static_allocation_dimensions allocation in
  let* () =
    if
      List.length source_dimensions <> List.length local_dimensions
      || List.length source_dimensions <> List.length checked
      || (not
            (List.for_all2
               (fun original dimension ->
                 Option.fold ~none:false ~some:(( == ) original)
                   (Local.array_dimension_source dimension))
               source_dimensions local_dimensions))
      || (not
            (List.for_all2
               (fun original dimension ->
                 (Record.dimension_receipt dimension).Parser.dimension_ast
                 == original)
               source_dimensions checked))
      || List.map Frame.dimension_value (Frame.location_dimensions location)
         <> List.map Record.dimension_count checked
    then Error "static source has substituted or unchecked array dimensions"
    else Ok ()
  in
  Ok { allocation_ = allocation; frame_ = frame; location_ = location }
