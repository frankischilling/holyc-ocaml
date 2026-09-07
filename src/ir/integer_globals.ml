module Records = Sema.Global_record_classification
module Resolution = Sema.Global_resolution
module Global = Sema.Global_type_resolution
module Symbol = Sema.Symbol
module Type = Sema.Type
module Typed = Sema.Function_call_expression_result
module Initial = Sema.Global_initializer_binding
module Symbols = Map.Make (Symbol.Id)

type slot = {
  index : int;
  symbol : Symbol.t;
  type_ : Type.t;
  record : Records.classified_record;
  opcode : Opcode.t;
  initial_bits : int64 option;
  initializer_root : Typed.top_level_root_result option;
  initializer_materialized : bool;
  initializer_preparation_steps : int;
}

type t = { slots_ : slot list; symbols : slot Symbols.t; byte_size_ : int }

let slots globals = globals.slots_
let byte_size globals = globals.byte_size_
let slot_index slot = slot.index
let slot_symbol slot = slot.symbol
let slot_type slot = slot.type_
let slot_record slot = slot.record
let slot_opcode slot = slot.opcode
let slot_initial_bits slot = slot.initial_bits
let slot_initializer slot = slot.initializer_root
let slot_initializer_materialized slot = slot.initializer_materialized
let slot_initializer_preparation_steps slot = slot.initializer_preparation_steps

let requires_initializer_execution globals =
  List.exists
    (fun slot ->
      Option.is_some slot.initializer_root && not slot.initializer_materialized)
    globals.slots_

let find globals symbol =
  match Symbols.find_opt (Symbol.id symbol) globals.symbols with
  | Some slot when slot.symbol == symbol -> Some slot
  | _ -> None

let create ?initializers ~span:unit_span records =
  let ( let* ) = Result.bind in
  let invalid message =
    Error
      [
        Common.Diagnostic.make ~code:"HCIRL0004"
          ~severity:Common.Diagnostic.Error ~message ~primary:unit_span ();
      ]
  in
  let roots =
    match initializers with
    | None -> []
    | Some typed ->
        Typed.top_level_statements typed
        |> List.concat_map Typed.top_level_statement_roots
        |> List.filter_map (fun root ->
            match
              root |> Typed.top_level_root_source
              |> Sema.Top_level_expression_tree.root_role
            with
            | Sema.Top_level_expression_tree.Global_initializer owner ->
                Some (owner, root)
            | _ -> None)
  in
  let* roots =
    List.fold_left
      (fun result (owner, root) ->
        let* roots = result in
        let id = Initial.global_symbol owner |> Symbol.id in
        if Symbols.mem id roots then
          invalid "global initializer roots have duplicate owners"
        else Ok (Symbols.add id (owner, root) roots))
      (Ok Symbols.empty) roots
  in
  let rec collect index symbols reversed roots = function
    | [] ->
        if Symbols.is_empty roots then
          Ok { slots_ = List.rev reversed; symbols; byte_size_ = index * 8 }
        else invalid "global initializer roots include an absent declaration"
    | record :: rest -> (
        let source = Records.classified_record_source record in
        let global = Resolution.global_record_global source in
        let symbol = Resolution.global_record_symbol source in
        let origin = Global.global_declarator_origin global in
        let span =
          match origin with
          | Symbol.Source_location location -> Some location.span
          | _ -> None
        in
        let fail code message =
          Error
            [
              Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error
                ~message
                ~primary:(Option.value span ~default:unit_span)
                ();
            ]
        in
        let type_ =
          Global.global_type_reference global
          |> Sema.Type_reference.resolved_type
        in
        let scalar =
          Type.pointer_depth type_ = 0
          &&
          match Type.base type_ with
          | Type.Primitive
              (Type.Public_spelling, (Sema.Primitive_type.I64 | U64)) -> true
          | _ -> false
        in
        if symbol != Global.global_symbol global || Option.is_none span then
          fail "HCIRL0004"
            "global storage has inconsistent symbol or source evidence"
        else if Symbols.mem (Symbol.id symbol) symbols then
          fail "HCIRL0004" "global storage has duplicate symbol identities"
        else if
          Resolution.global_record_kind source <> Resolution.Definition
          || Resolution.global_record_state source <> Resolution.Defined
          || Resolution.global_record_storage source <> Resolution.Code_heap
          || Option.is_some (Resolution.global_record_alias_target source)
          || Records.cleanup record <> Records.Free_data_address
        then
          fail "HCRUN0001"
            "global execution requires ordinary non-aliased code-heap \
             definitions"
        else if
          Option.is_some (Global.global_initializer global)
          && Option.is_none initializers
        then
          fail "HCRUN0001"
            "global declaration initializer execution is not implemented"
        else if
          (not scalar)
          || Global.global_array_dimensions global <> []
          || Global.global_declarator_kind global <> Global.Object
        then
          fail "HCRUN0001"
            "global execution requires scalar public I64/U64 objects"
        else if index >= Int.max_int / 8 then
          fail "HCIRL0005" "global storage size exceeds the host integer range"
        else
          let* initializer_root =
            match
              ( Global.global_initializer global,
                Symbols.find_opt (Symbol.id symbol) roots )
            with
            | None, None -> Ok None
            | Some _, Some (owner, root) ->
                let value = Typed.top_level_root_value root in
                if
                  Initial.global_record owner != source
                  || Initial.global_symbol owner != symbol
                  || Typed.top_level_root_result_use root <> None
                then
                  fail "HCIRL0004"
                    "global initializer root has inconsistent declaration \
                     evidence"
                else if
                  Typed.result_array_rank value <> 0
                  || (not
                        (match Typed.result_category value with
                        | Typed.Object_value | Typed.Lvalue -> true
                        | _ -> false))
                  || not
                       (match Typed.result_type value with
                       | Some type_ when Type.pointer_depth type_ = 0 -> (
                           match Type.base type_ with
                           | Type.Primitive (_, (Sema.Primitive_type.I64 | U64))
                             -> true
                           | _ -> false)
                       | _ -> false)
                then
                  fail "HCRUN0001"
                    "global initializer requires a scalar I64/U64 value"
                else Ok (Some root)
            | _ ->
                fail "HCIRL0004"
                  "global declaration and initializer roots disagree"
          in
          let path =
            match
              (Records.compilation_mode records, Records.value_access record)
            with
            | Resolution.Jit, Records.Jit_direct_address ->
                Some (Opcode.Ic_imm_i64, None)
            | Resolution.Aot, Records.Aot_code_heap_reference ->
                Some (Opcode.Ic_abs_addr, Some 0L)
            | _ -> None
          in
          match path with
          | None ->
              fail "HCIRL0004"
                "global storage address path disagrees with its compilation \
                 mode"
          | Some (opcode, initial_bits) ->
              let slot =
                {
                  index;
                  symbol;
                  type_;
                  record;
                  opcode;
                  initial_bits;
                  initializer_root;
                  initializer_materialized = false;
                  initializer_preparation_steps = 0;
                }
              in
              collect (index + 1)
                (Symbols.add (Symbol.id symbol) slot symbols)
                (slot :: reversed)
                (Symbols.remove (Symbol.id symbol) roots)
                rest)
  in
  collect 0 Symbols.empty [] roots (Records.records records)

let with_initial_values ~span globals values =
  let invalid message =
    Error
      [
        Common.Diagnostic.make ~code:"HCIRL0004"
          ~severity:Common.Diagnostic.Error ~message ~primary:span ();
      ]
  in
  let ( let* ) = Result.bind in
  let* updates =
    List.fold_left
      (fun result (symbol, bits, steps) ->
        let* updates = result in
        match find globals symbol with
        | Some slot
          when steps > 0
               && Option.is_some slot.initializer_root
               && (not slot.initializer_materialized)
               && not (Symbols.mem (Symbol.id symbol) updates) ->
            Ok (Symbols.add (Symbol.id symbol) (bits, steps) updates)
        | _ ->
            invalid
              "initial global image has a foreign, duplicate or absent \
               initializer owner")
      (Ok Symbols.empty) values
  in
  let slots_ =
    List.map
      (fun slot ->
        match Symbols.find_opt (Symbol.id slot.symbol) updates with
        | None -> slot
        | Some (bits, steps) ->
            {
              slot with
              initial_bits = Some bits;
              initializer_materialized = true;
              initializer_preparation_steps = steps;
            })
      globals.slots_
  in
  let symbols =
    List.fold_left
      (fun map slot -> Symbols.add (Symbol.id slot.symbol) slot map)
      Symbols.empty slots_
  in
  Ok { globals with slots_; symbols }

let human globals =
  match globals.slots_ with
  | [] -> ""
  | slots ->
      Printf.sprintf "holyc-integer-globals-v1 bytes=%d\n" globals.byte_size_
      ^ String.concat ""
          (List.map
             (fun slot ->
               Printf.sprintf
                 "global %d symbol=%d:%s type=%s address=%s initial=%s\n"
                 slot.index
                 (Symbol.id slot.symbol |> Symbol.Id.to_int)
                 (Symbol.name slot.symbol)
                 (Instruction_sequence.type_name slot.type_)
                 (Opcode.to_source_name slot.opcode)
                 (match slot.initial_bits with
                 | None -> "hosted-uninitialized"
                 | Some bits -> Printf.sprintf "0x%016Lx" bits))
             slots)
