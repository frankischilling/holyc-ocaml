module Records = Sema.Global_record_classification
module Resolution = Sema.Global_resolution
module Global = Sema.Global_type_resolution
module Symbol = Sema.Symbol
module Type = Sema.Type
module Symbols = Map.Make (Symbol.Id)

type slot = {
  index : int;
  symbol : Symbol.t;
  type_ : Type.t;
  record : Records.classified_record;
  opcode : Opcode.t;
  initial_bits : int64 option;
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

let find globals symbol =
  match Symbols.find_opt (Symbol.id symbol) globals.symbols with
  | Some slot when slot.symbol == symbol -> Some slot
  | _ -> None

let create ~span:unit_span records =
  let rec collect index symbols reversed = function
    | [] -> Ok { slots_ = List.rev reversed; symbols; byte_size_ = index * 8 }
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
        else if Option.is_some (Global.global_initializer global) then
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
                { index; symbol; type_; record; opcode; initial_bits }
              in
              collect (index + 1)
                (Symbols.add (Symbol.id symbol) slot symbols)
                (slot :: reversed) rest)
  in
  collect 0 Symbols.empty [] (Records.records records)

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
