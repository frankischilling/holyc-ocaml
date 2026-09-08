module Seq = Ir.Instruction_sequence
module Typed = Sema.Function_call_expression_result
module Globals = Ir.Integer_globals
module VM = Ir.Integer_interpreter
module Symbol = Sema.Symbol
module Values = Map.Make (Seq.Value_id)

type classification = Prepared_constant of int64 | Scheduled

type 'a prepared_item = {
  root_ : 'a;
  value_graph_ : Ir.X87_stack.t;
  classification_ : classification;
  steps : int;
}

type item = Typed.top_level_root_result prepared_item

type static_item =
  (Globals.static_slot * Typed.initializer_result) prepared_item

type owner =
  | Global of Globals.slot * Typed.top_level_root_result
  | Static of Globals.static_slot * Typed.initializer_result

type t = {
  globals_ : Globals.t;
  items_ : item list;
  static_items_ : static_item list;
  steps : int;
}

let globals prepared = prepared.globals_
let items prepared = prepared.items_
let static_items prepared = prepared.static_items_
let static_root (item : static_item) = snd item.root_
let static_slot (item : static_item) = fst item.root_
let static_value_graph (item : static_item) = item.value_graph_
let static_item_steps (item : static_item) = item.steps
let static_classification (item : static_item) = item.classification_
let executed_steps (prepared : t) = prepared.steps
let root item = item.root_
let value_graph item = item.value_graph_
let classification item = item.classification_
let item_steps (item : item) = item.steps
let ( let* ) = Result.bind

let instructions graph =
  Ir.Block_graph.blocks graph
  |> List.concat_map (fun block ->
      Ir.Block_graph.instructions block
      |> Seq.instructions |> List.map Seq.description)

let value_instructions graph =
  instructions (Ir.X87_stack.graph graph)
  |> List.filter (fun (item : Seq.description) ->
      match item.opcode with
      | Ir.Opcode.Ic_end_exp | Ic_end -> false
      | _ -> true)

let prepare ?(function_calls = []) ~max_steps ~span ~globals ~top_calls
    ~functions () =
  let invalid ?(notes = []) ?(at = span) code message =
    Error
      [
        Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
          ~notes ~primary:at ();
      ]
  in
  if max_steps <= 0 then
    invalid "HCIRVM0001" "max_initializer_steps must be greater than zero"
  else
    let work =
      (Globals.slots globals
      |> List.filter_map (fun slot ->
          Option.map
            (fun root -> Global (slot, root))
            (Globals.slot_initializer slot)))
      @ (Globals.statics globals
        |> List.filter_map (fun slot ->
            Option.map
              (fun root -> Static (slot, root))
              (Globals.static_initializer slot)))
      |> List.stable_sort (fun left right ->
          let index = function
            | Global (slot, _) ->
                Globals.slot_record slot
                |> Sema.Global_record_classification.classified_record_source
                |> Sema.Global_resolution.global_record_global
                |> Sema.Global_type_resolution.global_item_index
            | Static (slot, _) ->
                Globals.static_frame slot
                |> Sema.Function_frame_layout.function_item_index
          in
          Int.compare (index left) (index right))
    in
    let rec collect total updates reversed = function
      | [] ->
          let* globals_ =
            Globals.with_initial_values ~span globals (List.rev updates)
          in
          let prepared = List.rev reversed in
          let items_ =
            List.filter_map
              (fun item ->
                match item.root_ with
                | Global (_, root_) -> Some { item with root_ }
                | Static _ -> None)
              prepared
          in
          let static_items_ =
            List.filter_map
              (fun item ->
                match item.root_ with
                | Static (slot, root) ->
                    let symbol =
                      Globals.static_location slot
                      |> Sema.Function_frame_layout.location_symbol
                    in
                    let slot =
                      Option.get (Globals.find_static globals_ symbol)
                    in
                    Some { item with root_ = (slot, root) }
                | Global _ -> None)
              prepared
          in
          Ok { globals_; items_; static_items_; steps = total }
      | root_ :: rest -> (
          let symbol, value, frame =
            match root_ with
            | Global (slot, root) ->
                (Globals.slot_symbol slot, Typed.top_level_root_value root, None)
            | Static (slot, root) ->
                ( Globals.static_location slot
                  |> Sema.Function_frame_layout.location_symbol,
                  Typed.initializer_value root,
                  Some (Globals.static_frame slot) )
          in
          let at =
            match Typed.result_origin value with
            | Symbol.Source_location location -> location.span
            | _ -> span
          in
          let notes =
            [
              "initializer=" ^ Symbol.name symbol;
              Printf.sprintf "initializer_symbol_id=%d"
                (Symbol.id symbol |> Symbol.Id.to_int);
            ]
          in
          let* value_graph_ =
            Ir.Integer_program_lowering.lower ?frame ~globals ~top_calls
              ~function_calls ~span:at
              [ Ir.Integer_program_lowering.Expression value ]
            |> Result.map_error (fun errors ->
                match root_ with
                | Global _ -> errors
                | Static _ ->
                    List.map
                      (fun (error : Common.Diagnostic.t) ->
                        if error.code = "HCRUN0003" then
                          Common.Diagnostic.make ~code:"HCRUN0006"
                            ~severity:Common.Diagnostic.Error
                            ~message:
                              "static initializer expression is outside \
                               checked scalar storage and direct-call lowering"
                            ~primary:at ~notes ()
                        else error)
                      errors)
          in
          let value_code = value_instructions value_graph_ in
          let constant =
            List.for_all
              (fun (item : Seq.description) ->
                not (Ir.Opcode.info item.opcode).prevents_constant_folding)
              value_code
          in
          let guard ~constant code =
            let rec check pure = function
              | [] -> Ok ()
              | (item : Seq.description) :: rest ->
                  let known id =
                    Option.value (Values.find_opt id pure) ~default:false
                  in
                  let rejected =
                    match (item.opcode, item.operands) with
                    | (Ir.Opcode.Ic_shl | Ic_shr | Ic_shl_equ | Ic_shr_equ), _
                      -> true
                    | ( (Ir.Opcode.Ic_div | Ic_mod | Ic_div_equ | Ic_mod_equ),
                        [ _; right ] )
                      when not constant -> known right
                    | _ -> false
                  in
                  if rejected then
                    invalid
                      ~at:(Option.value item.span ~default:at)
                      ~notes "HCRUN0006"
                      "initializer arithmetic requires unresolved \
                       constant-divisor or shift optimizer behavior"
                  else
                    let is_pure =
                      match (item.opcode, item.payload) with
                      | Ir.Opcode.Ic_imm_i64, Some (Seq.Integer _) -> true
                      | _, _ ->
                          (not
                             (Ir.Opcode.info item.opcode)
                               .prevents_constant_folding)
                          && item.operands <> []
                          && List.for_all known item.operands
                    in
                    let pure =
                      match item.result with
                      | None -> pure
                      | Some result -> Values.add result.value_id is_pure pure
                    in
                    check pure rest
            in
            check Values.empty code
          in
          let* () = guard ~constant value_code in
          let called code =
            List.filter_map
              (fun (item : Seq.description) ->
                match (item.opcode, item.payload) with
                | Ir.Opcode.Ic_call, Some (Seq.Symbol symbol) -> Some symbol
                | _ -> None)
              code
          in
          let rec guard_callees visited = function
            | [] -> Ok ()
            | symbol :: rest
              when List.exists (fun other -> other == symbol) visited ->
                guard_callees visited rest
            | symbol :: rest -> (
                match
                  List.find_opt
                    (fun (function_ : VM.function_definition) ->
                      Ir.Function_body.callable_symbol function_.body == symbol)
                    functions
                with
                | None ->
                    invalid ~at ~notes "HCRUN0006"
                      "initializer call has no checked source definition"
                | Some function_ ->
                    let code =
                      instructions (Ir.Function_body.body function_.body)
                    in
                    let* () = guard ~constant:false code in
                    guard_callees (symbol :: visited) (called code @ rest))
          in
          let* () = guard_callees [] (called value_code) in
          if
            Option.is_some frame
            && List.exists
                 (fun (item : Seq.description) ->
                   item.opcode = Ir.Opcode.Ic_rbp)
                 value_code
          then
            invalid ~at ~notes "HCRUN0006"
              "static initialization has no invocation frame for parameter or \
               automatic-local reads"
          else if
            (not constant)
            &&
            match root_ with
            | Static (slot, _) ->
                Globals.storage_opcode (Globals.static_storage slot)
                = Ir.Opcode.Ic_abs_addr
                && Sema.Compiler_option.is_enabled
                     ~mask:(Globals.static_compiler_options slot)
                     Sema.Compiler_option.Globals_on_data_heap
            | Global _ -> false
          then
            invalid ~at ~notes "HCRUN0006"
              "nonconstant AOT static initialization with globals-on-data-heap \
               requires a separate compile-time phase"
          else if not constant then
            collect total updates
              ({ root_; value_graph_; classification_ = Scheduled; steps = 0 }
              :: reversed)
              rest
          else if total >= max_steps then
            invalid ~at
              ~notes:
                (notes
                @ [
                    "initializer_phase=constant-preparation";
                    Printf.sprintf "compiled_initializer_steps=%d" total;
                  ])
              "HCIRVM0007"
              "the bounded constant initializer preparation step limit was \
               exhausted"
          else
            let* result =
              VM.execute_program ~max_steps:(max_steps - total)
                ~max_frame_bytes:1 ~max_call_depth:1 ~functions:[] value_graph_
              |> Result.map_error
                   (List.map (fun (error : VM.error) ->
                        Common.Diagnostic.make ~code:error.code
                          ~severity:Common.Diagnostic.Error
                          ~message:error.message
                          ~primary:(Option.value error.span ~default:at)
                          ~notes:
                            (notes
                            @ [
                                "initializer_phase=constant-preparation";
                                Printf.sprintf "compiled_initializer_steps=%d"
                                  (total + error.executed_steps);
                                "constant preparation precedes hosted \
                                 whole-program execution";
                              ])
                          ()))
            in
            match VM.final_value result with
            | None ->
                invalid ~at ~notes "HCRUN0004"
                  "constant initializer preparation produced no word"
            | Some word ->
                let steps = VM.executed_steps result in
                collect (total + steps)
                  ((symbol, word.bits, steps) :: updates)
                  ({
                     root_;
                     value_graph_;
                     classification_ = Prepared_constant word.bits;
                     steps;
                   }
                  :: reversed)
                  rest)
    in
    collect 0 [] [] work

let global_human prepared =
  match prepared.items_ with
  | [] -> ""
  | items ->
      Printf.sprintf "holyc-initializer-preparation-v1 steps=%d\n"
        prepared.steps
      ^ String.concat ""
          (List.map
             (fun item ->
               let owner =
                 match
                   item.root_ |> Typed.top_level_root_source
                   |> Sema.Top_level_expression_tree.root_role
                 with
                 | Sema.Top_level_expression_tree.Global_initializer owner ->
                     Sema.Global_initializer_binding.global_symbol owner
                 | _ -> assert false
               in
               Printf.sprintf
                 "initializer symbol=%d:%s class=%s preparation-steps=%d\n"
                 (Symbol.id owner |> Symbol.Id.to_int)
                 (Symbol.name owner)
                 (match item.classification_ with
                 | Prepared_constant bits ->
                     Printf.sprintf "constant:0x%016Lx" bits
                 | Scheduled -> "nonconstant")
                 item.steps)
             items)

let human prepared =
  global_human prepared
  ^
  match prepared.static_items_ with
  | [] -> ""
  | items ->
      Printf.sprintf "holyc-static-initializer-preparation-v1 total-steps=%d\n"
        prepared.steps
      ^ String.concat ""
          (List.map
             (fun item ->
               let slot = fst item.root_ in
               let symbol =
                 Globals.static_location slot
                 |> Sema.Function_frame_layout.location_symbol
               in
               Printf.sprintf
                 "static-initializer function=%s symbol=%d:%s \
                  preparation-steps=%d\n"
                 (Globals.static_frame slot
                |> Sema.Function_frame_layout.function_symbol |> Symbol.name)
                 (Symbol.id symbol |> Symbol.Id.to_int)
                 (Symbol.name symbol) item.steps)
             items)
