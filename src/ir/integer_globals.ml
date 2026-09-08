module Records = Sema.Global_record_classification
module Resolution = Sema.Global_resolution
module Global = Sema.Global_type_resolution
module Symbol = Sema.Symbol
module Type = Sema.Type
module Typed = Sema.Function_call_expression_result
module Initial = Sema.Global_initializer_binding
module Symbols = Map.Make (Symbol.Id)
module Scalar = Integer_scalar_storage
module Shape = Integer_storage_shape
module Arrays = Integer_array_initializers

type slot = {
  index : int;
  symbol : Symbol.t;
  type_ : Type.t;
  record : Records.classified_record;
  shape : Shape.t;
  opcode : Opcode.t;
  initial_bits : int64 option;
  initializer_root : Typed.top_level_root_result option;
  array_initializers : Typed.top_level_root_result Arrays.t option;
  initializer_materialized : bool;
  initializer_preparation_steps : int;
}

type static_slot = Integer_statics.slot
type storage_slot = Global of slot | Static of static_slot

type task_publication =
  | Global_publication of Retained_global.t * slot
  | Function_publication of Retained_function.t

type task_catalog = {
  table : Sema.Symbol_table.t;
  mutable published : task_publication list;
}

type task_view = {
  catalog : task_catalog;
  environment : Sema.Outer_environment.t;
  task_table : Sema.Outer_environment.table;
  entries : (Sema.Outer_environment.entry * Retained_global.t * slot) list;
  function_entries : (Sema.Outer_environment.entry * Retained_function.t) list;
}

type t = {
  slots_ : slot list;
  symbols : slot Symbols.t;
  statics_ : static_slot list;
  mode : Resolution.compilation_mode;
  global_byte_size_ : int;
  global_cell_count_ : int;
  byte_size_ : int;
  task_view : task_view option;
  function_publications_ : Retained_function.t list;
}

let slots globals = globals.slots_
let byte_size globals = globals.byte_size_
let slot_index slot = slot.index
let slot_symbol slot = slot.symbol
let slot_type slot = slot.type_
let slot_record slot = slot.record
let slot_shape slot = slot.shape
let slot_opcode slot = slot.opcode
let slot_initial_bits slot = slot.initial_bits
let slot_initializer slot = slot.initializer_root
let slot_array_initializers slot = slot.array_initializers

let slot_initializers slot =
  match slot.array_initializers with
  | None -> Option.to_list slot.initializer_root
  | Some arrays -> List.map Arrays.root (Arrays.entries arrays)

let slot_initializer_materialized slot = slot.initializer_materialized

let slot_root_materialized slot root =
  match slot.array_initializers with
  | Some arrays ->
      Option.fold ~none:false
        ~some:(fun entry -> Option.is_some (Arrays.prepared entry))
        (Arrays.find arrays root)
  | None ->
      Option.fold ~none:false
        ~some:(fun expected ->
          expected == root && slot.initializer_materialized)
        slot.initializer_root

let slot_initializer_preparation_steps slot =
  slot.initializer_preparation_steps
  + Option.fold ~none:0 ~some:Arrays.steps slot.array_initializers

let statics globals = globals.statics_
let static_frame = Integer_statics.frame
let static_location = Integer_statics.location
let static_initializer = Integer_statics.initial
let static_initializers = Integer_statics.initializers
let static_array_initializers = Integer_statics.array_initializers

let static_root_materialized slot root =
  match static_array_initializers slot with
  | Some arrays ->
      Option.fold ~none:false
        ~some:(fun entry -> Option.is_some (Arrays.prepared entry))
        (Arrays.find arrays root)
  | None ->
      Option.fold ~none:false
        ~some:(fun expected ->
          expected == root && Integer_statics.materialized slot)
        (static_initializer slot)

let static_compiler_options = Integer_statics.compiler_options
let static_storage slot = Static slot
let global_storage slot = Global slot

let same_storage left right =
  match (left, right) with
  | Global left, Global right -> left == right
  | Static left, Static right -> left == right
  | _ -> false

let storage_slots globals =
  List.map global_storage globals.slots_
  @ List.map static_storage globals.statics_

let storage_shape = function
  | Global slot -> slot.shape
  | Static slot -> Integer_statics.shape slot

let storage_element_count slot = Shape.element_count (storage_shape slot)
let storage_strides slot = Shape.strides (storage_shape slot)
let storage_dimensions slot = Shape.dimensions (storage_shape slot)

let cell_count globals =
  globals.global_cell_count_
  + List.fold_left
      (fun total slot ->
        total + Shape.element_count (Integer_statics.shape slot))
      0 globals.statics_

let storage_index = function
  | Global slot -> slot.index
  | Static slot -> Integer_statics.index slot

let storage_symbol = function
  | Global slot -> slot.symbol
  | Static slot -> Integer_statics.symbol slot

let storage_type = function
  | Global slot -> slot.type_
  | Static slot -> Integer_statics.type_ slot

let storage_opcode = function
  | Global slot -> slot.opcode
  | Static slot -> Integer_statics.opcode slot

let storage_initial_bits = function
  | Global slot -> slot.initial_bits
  | Static slot -> Integer_statics.initial_bits slot

let storage_preparation_steps = function
  | Global slot -> slot_initializer_preparation_steps slot
  | Static slot -> Integer_statics.preparation_steps slot

let storage_frame = function
  | Global _ -> None
  | Static slot -> Some (static_frame slot)

let find_static globals symbol =
  List.find_opt
    (fun slot -> Integer_statics.symbol slot == symbol)
    globals.statics_

let with_statics ~span ~frames ~functions ~records globals =
  let ( let* ) = Result.bind in
  let mode =
    match globals.mode with
    | Resolution.Jit -> Sema.Function_resolution.Jit
    | Resolution.Aot -> Sema.Function_resolution.Aot
  in
  let* statics_ =
    Integer_statics.create ~span ~mode ~start:globals.global_cell_count_ ~frames
      ~functions ~records
  in
  if
    List.exists
      (fun slot ->
        Symbols.mem (Symbol.id (Integer_statics.symbol slot)) globals.symbols)
      statics_
  then
    Error
      [
        Common.Diagnostic.make ~code:"HCIRL0004"
          ~severity:Common.Diagnostic.Error
          ~message:"global and static storage have colliding symbol identities"
          ~primary:span ();
      ]
  else
    let* byte_size_ =
      List.fold_left
        (fun total slot ->
          let* total = total in
          match Shape.padded_byte_size (Integer_statics.shape slot) with
          | Some bytes when bytes <= Int.max_int - total -> Ok (total + bytes)
          | _ ->
              Error
                [
                  Common.Diagnostic.make ~code:"HCIRL0005"
                    ~severity:Common.Diagnostic.Error
                    ~message:
                      "persistent storage size exceeds the host integer range"
                    ~primary:span ();
                ])
        (Ok globals.global_byte_size_) statics_
    in
    Ok { globals with statics_; byte_size_ }

let has_initializers globals =
  List.exists (fun slot -> slot_initializers slot <> []) globals.slots_
  || List.exists (fun slot -> static_initializers slot <> []) globals.statics_

let has_unprepared_statics globals =
  List.exists
    (fun slot -> not (Integer_statics.materialized slot))
    globals.statics_

let requires_initializer_execution globals =
  List.exists
    (fun slot ->
      (Option.is_some slot.initializer_root && not slot.initializer_materialized)
      || Option.fold ~none:false ~some:Arrays.has_unprepared
           slot.array_initializers)
    globals.slots_

let find globals symbol =
  match Symbols.find_opt (Symbol.id symbol) globals.symbols with
  | Some slot when slot.symbol == symbol -> Some slot
  | _ -> None

let find_storage globals symbol =
  match find globals symbol with
  | Some slot -> Some (Global slot)
  | None -> Option.map static_storage (find_static globals symbol)

let create_impl ?layout ?initializers ~span:unit_span records =
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
        match Symbols.find_opt id roots with
        | None -> Ok (Symbols.add id (owner, [ root ]) roots)
        | Some (expected, previous) when expected == owner ->
            Ok (Symbols.add id (owner, root :: previous) roots)
        | Some _ ->
            invalid "global initializer roots have foreign declaration owners")
      (Ok Symbols.empty) roots
  in
  let rec collect index byte_size symbols reversed roots = function
    | [] ->
        if Symbols.is_empty roots then
          Ok
            {
              slots_ = List.rev reversed;
              symbols;
              statics_ = [];
              mode = Records.compilation_mode records;
              global_byte_size_ = byte_size;
              global_cell_count_ = index;
              byte_size_ = byte_size;
              task_view = None;
              function_publications_ = [];
            }
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
          Option.is_none (Scalar.public_byte_size type_)
          || Global.global_declarator_kind global <> Global.Object
        then
          fail "HCRUN0001"
            "global execution requires public nonzero integer objects"
        else
          let* dimensions =
            match Global.global_array_dimensions global with
            | [] -> Ok []
            | _ -> (
                match
                  Option.bind layout (fun layout ->
                      Sema.Global_array_layout.find layout source)
                with
                | Some checked ->
                    Ok (Sema.Global_array_layout.dimensions checked)
                | None ->
                    fail "HCRUN0001"
                      "global array execution requires its exact checked \
                       extents")
          in
          let* shape =
            match Shape.create ~type_ ~dimensions with
            | Ok shape -> Ok shape
            | Error Shape.Overflow ->
                fail "HCIRL0005"
                  "global storage size exceeds the host integer range"
            | Error _ ->
                fail "HCRUN0001"
                  "global execution requires positive fixed integer storage"
          in
          let* () =
            if
              Shape.element_count shape > Sys.max_array_length - index
              || Shape.byte_size shape > Int.max_int - byte_size
            then
              fail "HCIRL0005"
                "global storage size exceeds the host integer range"
            else Ok ()
          in
          let owned =
            Option.map
              (fun (owner, roots) -> (owner, List.rev roots))
              (Symbols.find_opt (Symbol.id symbol) roots)
          in
          let* array_initializers =
            if dimensions = [] then Ok None
            else
              match (Global.global_initializer global, owned) with
              | None, None -> Ok None
              | Some initial, Some (owner, owned_roots) ->
                  if
                    Initial.global_record owner != source
                    || Initial.global_symbol owner != symbol
                    || List.exists
                         (fun root ->
                           Typed.top_level_root_result_use root <> None)
                         owned_roots
                  then
                    fail "HCIRL0004"
                      "array initializer has inconsistent declaration evidence"
                  else
                    begin match Global.initializer_source initial with
                    | None ->
                        fail "HCIRL0004"
                          "array initializer has no original source manifest"
                    | Some source ->
                        Arrays.create ~shape ~source ~roots:owned_roots
                          ~source_leaf:(fun root ->
                            root |> Typed.top_level_root_source
                            |> Sema.Top_level_expression_tree
                               .root_initializer_leaf)
                        |> Result.map Option.some
                        |> Result.map_error (fun message ->
                            [
                              Common.Diagnostic.make ~code:"HCRUN0006"
                                ~severity:Common.Diagnostic.Error ~message
                                ~primary:(Option.value span ~default:unit_span)
                                ();
                            ])
                    end
              | _ ->
                  fail "HCIRL0004"
                    "array declaration and initializer roots disagree"
          in
          let* initializer_root =
            if dimensions <> [] then Ok None
            else
              match (Global.global_initializer global, owned) with
              | None, None -> Ok None
              | Some initial, Some (owner, [ root ]) ->
                  let value = Typed.top_level_root_value root in
                  if
                    Global.initializer_kind initial <> Global.Scalar_initializer
                  then
                    fail "HCRUN0001"
                      "scalar storage requires a scalar initializer expression"
                  else if
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
                         | Some type_ ->
                             Option.is_some
                               (Integer_scalar_storage.of_type type_)
                         | _ -> false)
                  then
                    fail "HCRUN0001"
                      "global initializer requires a nonzero scalar integer \
                       value"
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
                  shape;
                  opcode;
                  initial_bits;
                  initializer_root;
                  array_initializers;
                  initializer_materialized = false;
                  initializer_preparation_steps = 0;
                }
              in
              collect
                (index + Shape.element_count shape)
                (byte_size + Shape.byte_size shape)
                (Symbols.add (Symbol.id symbol) slot symbols)
                (slot :: reversed)
                (Symbols.remove (Symbol.id symbol) roots)
                rest)
  in
  collect 0 0 Symbols.empty [] roots (Records.records records)

let create ?initializers ~span records = create_impl ?initializers ~span records

let create_with_layout ~layout ?initializers ~span records =
  create_impl ~layout ?initializers ~span records

let create_task_catalog ~table = { table; published = [] }
let task_catalog_owns_table catalog table = catalog.table == table

let publication_symbol = function
  | Global_publication (_, slot) -> slot.symbol
  | Function_publication reference -> Retained_function.symbol reference

let newest_publications publications =
  List.fold_right
    (fun publication selected ->
      if
        List.exists
          (fun prior ->
            publication_symbol prior == publication_symbol publication)
          selected
      then selected
      else publication :: selected)
    publications []

let function_publications globals = globals.function_publications_

let with_function_publications ~records globals =
  let module Outer = Sema.Outer_environment in
  let ( let* ) = Result.bind in
  let* publications =
    List.fold_left
      (fun result classified ->
        let* publications = result in
        let declaration =
          Sema.Function_record_classification.classified_declaration_source
            classified
        in
        let* metadata =
          Outer.make_function_metadata ~records ~declaration
          |> Result.map_error Outer.error_to_string
        in
        Ok
          (Function_publication (Retained_function.create metadata)
          :: publications))
      (Ok [])
      (Sema.Function_record_classification.declarations records)
  in
  let function_publications_ =
    newest_publications (List.rev publications)
    |> List.filter_map (function
      | Function_publication reference -> Some reference
      | Global_publication _ -> None)
  in
  Ok { globals with function_publications_ }

let snapshot_task catalog =
  let module Outer = Sema.Outer_environment in
  let ( let* ) = Result.bind in
  let checked result = Result.map_error Outer.error_to_string result in
  let rec collect index rev globals functions = function
    | [] -> Ok (List.rev rev, List.rev globals, List.rev functions)
    | Function_publication reference :: rest ->
        let* entry =
          Outer.make_function_entry ~entry_index:index
            ~function_metadata:(Retained_function.metadata reference)
          |> checked
        in
        collect (index + 1) (entry :: rev) globals
          ((entry, reference) :: functions)
          rest
    | Global_publication (reference, slot) :: rest ->
        let source =
          Records.classified_record_source slot.record
          |> Resolution.global_record_global
        in
        let declarator_kind =
          match Global.global_declarator_kind source with
          | Global.Object -> Outer.Object_global
          | Global.Function_pointer pointer ->
              Outer.Function_pointer_global pointer
        in
        let* global_metadata =
          Outer.make_global_metadata
            ~type_reference:(Global.global_type_reference source)
            ~declarator_kind
            ~array_rank:(List.length (Shape.dimensions slot.shape))
          |> checked
        in
        let* entry =
          Outer.make_global_entry ~symbol:slot.symbol ~entry_index:index
            ~global_metadata
          |> checked
        in
        collect (index + 1) (entry :: rev)
          ((entry, reference, slot) :: globals)
          functions rest
  in
  let* all_entries, entries, function_entries =
    collect 0 [] [] [] (newest_publications catalog.published)
  in
  let* task_table =
    Outer.make_table ~table_kind:(Outer.Jit_task 0) ~table_index:0 all_entries
    |> checked
  in
  let* assembler =
    Outer.make_table ~table_kind:Outer.Assembler ~table_index:1 [] |> checked
  in
  let* environment =
    Outer.create ~table:catalog.table ~compilation_mode:Outer.Jit
      [ task_table; assembler ]
    |> checked
  in
  Ok { catalog; environment; task_table; entries; function_entries }

let task_environment view = view.environment
let task_catalog_owns_view catalog view = view.catalog == catalog

let task_global_binding view reference =
  List.find_map
    (fun (entry, candidate, _) ->
      if Retained_global.same reference candidate then
        Sema.Outer_environment.binding_for_entry view.environment entry
      else None)
    view.entries

let task_function_binding view reference =
  List.find_map
    (fun (entry, candidate) ->
      if Retained_function.same reference candidate then
        Sema.Outer_environment.binding_for_entry view.environment entry
      else None)
    view.function_entries

let with_task_view view globals = { globals with task_view = Some view }

let retained_binding globals binding =
  let module Outer = Sema.Outer_environment in
  Option.bind globals.task_view (fun view ->
      if Outer.binding_table binding != view.task_table then None
      else
        List.find_map
          (fun (entry, reference, slot) ->
            if Outer.binding_entry binding == entry then Some (reference, slot)
            else None)
          view.entries)

let retained_slot globals reference =
  Option.bind globals.task_view (fun view ->
      List.find_map
        (fun (_, candidate, slot) ->
          if Retained_global.same candidate reference then Some (Global slot)
          else None)
        view.entries)

let retained_function_binding globals binding =
  let module Outer = Sema.Outer_environment in
  Option.bind globals.task_view (fun view ->
      if Outer.binding_table binding != view.task_table then None
      else
        List.find_map
          (fun (entry, reference) ->
            if Outer.binding_entry binding == entry then Some reference
            else None)
          view.function_entries)

let retained_function_symbol globals symbol =
  Option.bind globals.task_view (fun view ->
      List.find_map
        (fun (_, reference) ->
          if Retained_function.symbol reference == symbol then Some reference
          else None)
        view.function_entries)

let is_task_command globals = Option.is_some globals.task_view

let check_task_command catalog globals =
  match globals.task_view with
  | None -> Error "task execution requires a compiled task storage view"
  | Some view when view.catalog != catalog ->
      Error "compiled storage view belongs to another task"
  | Some view ->
      if globals.mode <> Resolution.Jit then
        Error "task commands require JIT storage"
      else if
        not
          (List.for_all
             (fun slot ->
               Sema.Symbol_table.owns_symbol catalog.table (storage_symbol slot))
             (storage_slots globals))
      then Error "new task storage has foreign symbols"
      else if
        List.exists
          (fun slot ->
            List.exists
              (fun prior -> publication_symbol prior == slot.symbol)
              catalog.published)
          globals.slots_
      then Error "task storage declaration has already been admitted"
      else if
        not
          (List.for_all
             (fun (_, reference, slot) ->
               List.exists
                 (function
                   | Global_publication (prior, expected) ->
                       Retained_global.same prior reference && expected == slot
                   | Function_publication _ -> false)
                 catalog.published)
             view.entries)
      then Error "retained global reference is absent from this task"
      else if
        not
          (List.for_all
             (fun (_, reference) ->
               List.exists
                 (function
                   | Function_publication prior ->
                       Retained_function.same prior reference
                   | Global_publication _ -> false)
                 catalog.published)
             view.function_entries)
      then Error "retained function reference is absent from this task"
      else if
        List.exists
          (fun reference ->
            (not
               (Sema.Symbol_table.owns_symbol catalog.table
                  (Retained_function.symbol reference)))
            || List.exists
                 (fun prior ->
                   publication_symbol prior
                   == Retained_function.symbol reference)
                 catalog.published)
          globals.function_publications_
      then Error "task function declaration is foreign or already admitted"
      else Ok ()

let publish_task catalog globals =
  let order = function
    | Global_publication (_, slot) ->
        let source =
          Records.classified_record_source slot.record
          |> Resolution.global_record_global
        in
        ( Global.global_item_index source,
          Option.value (Global.global_declarator_index source) ~default:0 )
    | Function_publication reference ->
        let header =
          Retained_function.metadata reference
          |> Sema.Outer_environment.function_declaration
          |> Sema.Function_resolution.resolved_declaration_site
          |> Sema.Function_resolution.declaration_site_function
        in
        (Sema.Function_type_resolution.function_item_index header, 0)
  in
  let publications =
    List.map
      (fun slot ->
        Global_publication (Retained_global.create slot.symbol, slot))
      globals.slots_
    @ List.map
        (fun reference -> Function_publication reference)
        globals.function_publications_
    |> List.stable_sort (fun left right -> compare (order left) (order right))
  in
  catalog.published <- catalog.published @ publications;
  publications

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
        match find_storage globals symbol with
        | Some (Global slot)
          when steps > 0
               && Option.is_some slot.initializer_root
               && (not slot.initializer_materialized)
               && not (Symbols.mem (Symbol.id symbol) updates) ->
            Ok (Symbols.add (Symbol.id symbol) (bits, steps) updates)
        | Some (Static slot)
          when steps > 0
               && Option.is_some (static_initializer slot)
               && (not (Integer_statics.materialized slot))
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
              initial_bits = Some (Scalar.narrow_bits slot.type_ bits);
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
  let statics_ =
    List.map
      (fun slot ->
        match
          Symbols.find_opt (Symbol.id (Integer_statics.symbol slot)) updates
        with
        | None -> slot
        | Some (bits, steps) ->
            Integer_statics.with_initial_value slot ~bits ~steps)
      globals.statics_
  in
  Ok { globals with slots_; symbols; statics_ }

let with_array_initial_values ~span globals ~global_values ~static_values =
  let invalid message =
    Error
      [
        Common.Diagnostic.make ~code:"HCIRL0004"
          ~severity:Common.Diagnostic.Error ~message ~primary:span ();
      ]
  in
  let ( let* ) = Result.bind in
  let partition arrays values =
    List.partition
      (fun (root, _, _) ->
        Option.fold ~none:false
          ~some:(fun arrays -> Option.is_some (Arrays.find arrays root))
          arrays)
      values
  in
  let rec update_globals reversed values = function
    | [] ->
        if values = [] then Ok (List.rev reversed)
        else invalid "array image contains a foreign global initializer root"
    | slot :: rest ->
        let owned, values = partition slot.array_initializers values in
        let* slot =
          match slot.array_initializers with
          | None -> Ok slot
          | Some arrays ->
              begin match Arrays.publish arrays owned with
              | Error message -> invalid message
              | Ok arrays -> Ok { slot with array_initializers = Some arrays }
              end
        in
        update_globals (slot :: reversed) values rest
  in
  let rec update_statics reversed values = function
    | [] ->
        if values = [] then Ok (List.rev reversed)
        else invalid "array image contains a foreign static initializer root"
    | slot :: rest ->
        let owned, values = partition (static_array_initializers slot) values in
        let* slot =
          match Integer_statics.with_array_initial_values slot owned with
          | Error message -> invalid message
          | Ok slot -> Ok slot
        in
        update_statics (slot :: reversed) values rest
  in
  let* slots_ = update_globals [] global_values globals.slots_ in
  let* statics_ = update_statics [] static_values globals.statics_ in
  let symbols =
    List.fold_left
      (fun map slot -> Symbols.add (Symbol.id slot.symbol) slot map)
      Symbols.empty slots_
  in
  Ok { globals with slots_; statics_; symbols }

let storage_array_image slot =
  let image arrays =
    match arrays with
    | None -> []
    | Some arrays ->
        Arrays.entries arrays
        |> List.filter_map (fun entry ->
            Option.map
              (fun (payload, _) ->
                ( Integer_initializer_layout.cell_offset
                    (Arrays.destination entry),
                  payload ))
              (Arrays.prepared entry))
  in
  match slot with
  | Global slot -> image slot.array_initializers
  | Static slot -> image (Integer_statics.array_initializers slot)

let global_human globals =
  match globals.slots_ with
  | [] -> ""
  | slots ->
      Printf.sprintf "holyc-integer-globals-v1 bytes=%d\n"
        globals.global_byte_size_
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

let scalar_human globals =
  global_human globals
  ^
  match globals.statics_ with
  | [] -> ""
  | slots ->
      Printf.sprintf "holyc-integer-statics-v1 bytes=%d\n"
        (globals.byte_size_ - globals.global_byte_size_)
      ^ String.concat ""
          (List.map
             (fun slot ->
               Printf.sprintf
                 "static %d function=%s symbol=%d:%s type=%s address=%s \
                  initial=%s preparation-steps=%d\n"
                 (Integer_statics.index slot)
                 (static_frame slot
                |> Sema.Function_frame_layout.function_symbol |> Symbol.name)
                 (Integer_statics.symbol slot |> Symbol.id |> Symbol.Id.to_int)
                 (Integer_statics.symbol slot |> Symbol.name)
                 (Integer_statics.type_ slot |> Instruction_sequence.type_name)
                 (Integer_statics.opcode slot |> Opcode.to_source_name)
                 (match Integer_statics.initial_bits slot with
                 | None -> "hosted-uninitialized"
                 | Some bits -> Printf.sprintf "0x%016Lx" bits)
                 (Integer_statics.preparation_steps slot))
             slots)

let array_human globals =
  let hex bytes =
    String.to_seq bytes
    |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
    |> List.of_seq |> String.concat ""
  in
  let initializers arrays =
    match arrays with
    | None -> ""
    | Some arrays ->
        Arrays.entries arrays
        |> List.map (fun entry ->
            let destination = Arrays.destination entry in
            let state, steps =
              match Arrays.prepared entry with
              | None -> ("scheduled", 0)
              | Some (Arrays.Word bits, steps) ->
                  (Printf.sprintf "prepared-word:0x%016Lx" bits, steps)
              | Some (Arrays.Bytes bytes, steps) ->
                  ("prepared-bytes:" ^ hex bytes, steps)
            in
            Printf.sprintf
              "array-initializer leaf=%d cell=%d byte=%d state=%s \
               preparation-steps=%d\n"
              (Integer_initializer_layout.leaf destination
              |> Sema.Initializer_source.leaf_index)
              (Integer_initializer_layout.cell_offset destination)
              (Integer_initializer_layout.byte_offset destination)
              state steps)
        |> String.concat ""
  in
  let arrays =
    storage_slots globals
    |> List.filter (fun slot -> storage_dimensions slot <> [])
  in
  match arrays with
  | [] -> ""
  | _ ->
      "holyc-persistent-arrays-v1\n"
      ^ (arrays
        |> List.map (fun slot ->
            let shape = storage_shape slot in
            let numbers values =
              String.concat "," (List.map Int64.to_string values)
            in
            let symbol = storage_symbol slot in
            let owner, values =
              match slot with
              | Global slot -> ("global", initializers slot.array_initializers)
              | Static slot ->
                  ( "static:"
                    ^ (static_frame slot
                     |> Sema.Function_frame_layout.function_symbol
                     |> Symbol.name),
                    initializers (static_array_initializers slot) )
            in
            Printf.sprintf
              "array storage=%s symbol=%d:%s base-cell=%d dimensions=[%s] \
               strides=[%s] cells=%d bytes=%d preparation-steps=%d\n"
              owner
              (Symbol.id symbol |> Symbol.Id.to_int)
              (Symbol.name symbol) (storage_index slot)
              (numbers (Shape.dimensions shape))
              (numbers (Shape.strides shape))
              (Shape.element_count shape)
              (Shape.byte_size shape)
              (storage_preparation_steps slot)
            ^ values)
        |> String.concat "")

let human globals = scalar_human globals ^ array_human globals
