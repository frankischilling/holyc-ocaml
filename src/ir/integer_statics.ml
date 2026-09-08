module Frame = Sema.Function_frame_layout
module Typed = Sema.Function_call_expression_result
module Source = Sema.Function_call_resolution
module Local = Sema.Local_type_resolution
module Records = Sema.Function_record_classification
module Resolution = Sema.Function_resolution
module Symbol = Sema.Symbol
module Type = Sema.Type
module Shape = Integer_storage_shape
module Arrays = Integer_array_initializers

type slot = {
  index : int;
  frame : Frame.function_layout;
  location : Frame.location;
  shape : Shape.t;
  compiler_options : int64;
  opcode : Opcode.t;
  initial : Typed.initializer_result option;
  array_initializers : Typed.initializer_result Arrays.t option;
  initial_bits : int64 option;
  preparation_steps : int;
}

let index slot = slot.index
let frame slot = slot.frame
let location slot = slot.location
let shape slot = slot.shape
let symbol slot = Frame.location_symbol slot.location
let type_ slot = Frame.location_checked_type slot.location
let compiler_options slot = slot.compiler_options
let opcode slot = slot.opcode
let initial slot = slot.initial
let array_initializers slot = slot.array_initializers

let initializers slot =
  match slot.array_initializers with
  | None -> Option.to_list slot.initial
  | Some arrays -> List.map Arrays.root (Arrays.entries arrays)

let initial_bits slot = slot.initial_bits

let preparation_steps slot =
  slot.preparation_steps
  + Option.fold ~none:0 ~some:Arrays.steps slot.array_initializers

let materialized slot =
  (Option.is_none slot.initial || slot.preparation_steps > 0)
  && not
       (Option.fold ~none:false ~some:Arrays.has_unprepared
          slot.array_initializers)

let ( let* ) = Result.bind

let create ~span ~mode ~start ~frames ~functions ~records =
  let invalid ?(code = "HCIRL0004") ?(at = span) message =
    Error
      [
        Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
          ~primary:at ();
      ]
  in
  if
    mode <> Records.compilation_mode records
    || mode <> Typed.compilation_mode functions
  then invalid "static storage has inconsistent compilation modes"
  else
    let rec collect index reversed = function
      | [] -> Ok (List.rev reversed)
      | fn :: rest ->
          let* record =
            match
              List.find_opt
                (fun record ->
                  record |> Records.classified_declaration_source
                  |> Resolution.resolved_declaration_site
                  |> Resolution.declaration_site_function
                  |> Sema.Function_type_resolution.function_symbol
                  |> fun symbol -> symbol == Typed.function_symbol fn)
                (Records.declarations records)
            with
            | Some record -> Ok record
            | None ->
                invalid
                  "static storage function has no checked declaration options"
          in
          let site =
            record |> Records.classified_declaration_source
            |> Resolution.resolved_declaration_site
          in
          let header = Resolution.declaration_site_function site in
          let* () =
            if
              Sema.Function_type_resolution.function_scope header
              == Typed.function_scope fn
              && Sema.Function_type_resolution.function_item_index header
                 = Typed.function_item_index fn
            then Ok ()
            else
              invalid "static storage function has no exact checked declaration"
          in
          if
            Resolution.declaration_site_source_kind site
            <> Resolution.Definition
          then
            if
              Option.is_some
                (Frame.find_function frames (Typed.function_symbol fn))
              || Typed.function_initializers fn <> []
            then
              invalid
                "function prototype cannot own a static frame or initializer"
            else collect index reversed rest
          else
            let* frame =
              match Frame.find_function frames (Typed.function_symbol fn) with
              | Some frame
                when Frame.function_scope frame == Typed.function_scope fn
                     && Frame.function_item_index frame
                        = Typed.function_item_index fn -> Ok frame
              | _ ->
                  invalid "static storage function has no exact checked frame"
            in
            let compiler_options =
              record |> Records.classified_declaration_state
              |> Records.declaration_state_compiler_option_mask
            in
            let roots =
              Typed.function_initializers fn
              |> List.filter (fun root ->
                  root |> Typed.initializer_source |> Source.initializer_local
                  |> Local.local_storage = Local.Static)
            in
            let rec locals index reversed remaining = function
              | [] ->
                  if remaining <> [] then
                    invalid "static initializer has no checked storage location"
                  else collect index reversed rest
              | location :: locations ->
                  let symbol = Frame.location_symbol location in
                  let at =
                    match Symbol.origin symbol with
                    | Symbol.Source_location source -> source.span
                    | _ -> span
                  in
                  let scalar_bytes =
                    Integer_scalar_storage.public_byte_size
                      (Frame.location_checked_type location)
                  in
                  let dimensions =
                    Frame.location_dimensions location
                    |> List.map Frame.dimension_value
                  in
                  let checked_shape =
                    Shape.create
                      ~type_:(Frame.location_checked_type location)
                      ~dimensions
                  in
                  let* shape =
                    match checked_shape with
                    | Ok shape -> Ok shape
                    | Error Shape.Overflow ->
                        invalid ~at ~code:"HCIRL0005"
                          "persistent storage size exceeds the host integer \
                           range"
                    | Error _ ->
                        invalid ~at ~code:"HCRUN0001"
                          "static execution requires positive fixed public \
                           I64/U64/U8 storage"
                  in
                  if
                    Option.is_none scalar_bytes
                    || dimensions <> []
                       && not
                            (Frame.location_source_dimensions_checked location)
                    || Frame.location_declarator_shape location <> Frame.Object
                    || (Frame.location_value_shape location
                       <> if dimensions = [] then Frame.Scalar else Frame.Array
                       )
                    || Frame.location_allocated_size location
                       <> Int64.of_int (Shape.byte_size shape)
                    || Frame.location_element_size location
                       <> Int64.of_int (Option.get scalar_bytes)
                    || Frame.location_alignment location <> 8
                    || Option.is_some (Frame.location_frame_slot location)
                  then
                    invalid ~at ~code:"HCRUN0001"
                      "static execution requires scalar public I64/U64/U8 \
                       objects without frame slots"
                  else if
                    Shape.element_count shape > Sys.max_array_length - index
                    || Option.is_none (Shape.padded_byte_size shape)
                  then
                    invalid ~at ~code:"HCIRL0005"
                      "persistent storage size exceeds the host integer range"
                  else if
                    List.exists
                      (fun slot ->
                        Symbol.Id.equal
                          (Symbol.id (Frame.location_symbol slot.location))
                          (Symbol.id symbol))
                      reversed
                  then
                    invalid ~at "static storage has duplicate symbol identities"
                  else
                    let owned, remaining =
                      List.partition
                        (fun root ->
                          root |> Typed.initializer_source
                          |> Source.initializer_local |> Local.local_symbol
                          |> fun owner -> owner == symbol)
                        remaining
                    in
                    let* () =
                      List.fold_left
                        (fun checked root ->
                          let* () = checked in
                          Frame_address_lowering.prepare_initializer ~frame root
                          |> Result.map (fun _ -> ())
                          |> Result.map_error
                               (List.map
                                  (fun (error : Instruction_sequence.error) ->
                                    Common.Diagnostic.make ~code:error.code
                                      ~severity:Common.Diagnostic.Error
                                      ~message:error.message
                                      ~primary:
                                        (Option.value error.span ~default:at)
                                      ())))
                        (Ok ()) owned
                    in
                    let* array_initializers =
                      if dimensions = [] then Ok None
                      else
                        match owned with
                        | [] -> Ok None
                        | root :: _ ->
                            let local =
                              root |> Typed.initializer_source
                              |> Source.initializer_local
                            in
                            begin match
                              Option.bind
                                (Local.local_initializer local)
                                Local.initializer_source
                            with
                            | None ->
                                invalid ~at
                                  "array initializer has no original source \
                                   manifest"
                            | Some source ->
                                Arrays.create ~shape ~source ~roots:owned
                                  ~source_leaf:(fun root ->
                                    root |> Typed.initializer_source
                                    |> Source.initializer_leaf)
                                |> Result.map Option.some
                                |> Result.map_error (fun message ->
                                    [
                                      Common.Diagnostic.make ~code:"HCRUN0006"
                                        ~severity:Common.Diagnostic.Error
                                        ~message ~primary:at ();
                                    ])
                            end
                    in
                    let* initial =
                      if dimensions <> [] then Ok None
                      else
                        match owned with
                        | [] -> Ok None
                        | [ root ] ->
                            let* _ =
                              Frame_address_lowering.prepare_initializer ~frame
                                root
                              |> Result.map_error
                                   (List.map
                                      (fun
                                        (error : Instruction_sequence.error) ->
                                        Common.Diagnostic.make ~code:error.code
                                          ~severity:Common.Diagnostic.Error
                                          ~message:error.message
                                          ~primary:
                                            (Option.value error.span ~default:at)
                                          ()))
                            in
                            Ok (Some root)
                        | _ ->
                            invalid ~at
                              "static initializer has duplicate declaration \
                               owners"
                    in
                    let opcode, initial_bits =
                      match mode with
                      | Resolution.Jit -> (Opcode.Ic_imm_i64, None)
                      | Resolution.Aot -> (Opcode.Ic_abs_addr, Some 0L)
                    in
                    locals
                      (index + Shape.element_count shape)
                      ({
                         index;
                         frame;
                         location;
                         shape;
                         compiler_options;
                         opcode;
                         initial;
                         array_initializers;
                         initial_bits;
                         preparation_steps = 0;
                       }
                      :: reversed)
                      remaining locations
            in
            locals index reversed roots
              (Frame.function_locations frame
              |> List.filter (fun location ->
                  Frame.location_kind location = Frame.Static_local))
    in
    collect start [] (Typed.functions functions)

let with_initial_value slot ~bits ~steps =
  {
    slot with
    initial_bits = Some (Integer_scalar_storage.narrow_bits (type_ slot) bits);
    preparation_steps = steps;
  }

let with_array_initial_values slot updates =
  match slot.array_initializers with
  | None ->
      if updates = [] then Ok slot
      else Error "HCIRL0004: static has no array initializer"
  | Some arrays ->
      let* arrays = Arrays.publish arrays updates in
      Ok { slot with array_initializers = Some arrays }
