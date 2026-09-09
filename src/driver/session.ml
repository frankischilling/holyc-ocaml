type primitive_binding = {
  frontend_entry : Frontend.Symbol_visibility.entry;
  semantic_symbol : Sema.Symbol.t;
  primitive : Common.Primitive_type.t;
  record : Sema.Compiler_record.t;
}

type t = {
  sources : Common.Source_manager.t;
  definitions : Frontend.Definition.Environment.t;
  symbols : Frontend.Symbol_visibility.Environment.t;
  semantic_symbols : Sema.Symbol_table.t;
  primitives : primitive_binding list;
}

module Symbol_visibility = Frontend.Symbol_visibility
module Definition = Frontend.Definition

let add_pinned symbols ~name ~kind ~path ~line =
  ignore
    (Symbol_visibility.Environment.add symbols ~name ~kind
       ~origin:(Symbol_visibility.Pinned_source { path; line })
       ())

let seed_compiler_symbols symbols =
  List.iter
    (fun (entry : Generated.Opcode_keywords.entry) ->
      add_pinned symbols ~name:entry.spelling ~kind:Symbol_visibility.Keyword
        ~path:Generated.Opcode_keywords.source_path ~line:entry.source_line)
    Generated.Opcode_keywords.language;
  List.iter
    (fun (entry : Generated.Opcode_keywords.entry) ->
      add_pinned symbols ~name:entry.spelling
        ~kind:Symbol_visibility.Assembly_keyword
        ~path:Generated.Opcode_keywords.source_path ~line:entry.source_line)
    Generated.Opcode_keywords.assembly;
  let primitives =
    List.map
      (fun (entry : Generated.Primitive_raw_types.internal_type) ->
        let frontend_entry =
          Symbol_visibility.Environment.add symbols ~name:entry.spelling
            ~kind:Symbol_visibility.Internal_type
            ~origin:
              (Symbol_visibility.Pinned_source
                 {
                   path = Generated.Primitive_raw_types.cinit_source_path;
                   line = entry.source_line;
                 })
            ()
        in
        (frontend_entry, entry))
      Generated.Primitive_raw_types.internal_types
  in
  List.iter
    (fun (register : Generated.Opcode_keywords.register) ->
      add_pinned symbols ~name:register.spelling
        ~kind:Symbol_visibility.Register
        ~path:Generated.Opcode_keywords.source_path ~line:register.source_line)
    Generated.Opcode_keywords.registers;
  List.iter
    (fun (opcode : Generated.Opcode_keywords.opcode) ->
      add_pinned symbols ~name:opcode.spelling ~kind:Symbol_visibility.Opcode
        ~path:Generated.Opcode_keywords.source_path ~line:opcode.source_line;
      List.iter
        (fun (alias : Generated.Opcode_keywords.opcode_alias) ->
          add_pinned symbols ~name:alias.spelling ~kind:Symbol_visibility.Opcode
            ~path:Generated.Opcode_keywords.source_path ~line:alias.source_line)
        opcode.aliases)
    Generated.Opcode_keywords.opcodes;
  primitives

let seed_semantic_symbols table primitives =
  let root = Sema.Symbol_table.root table in
  List.map
    (fun (frontend_entry, (entry : Generated.Primitive_raw_types.internal_type))
       ->
      let semantic_symbol =
        match
          Sema.Symbol_table.add table ~scope:root ~name:entry.spelling
            ~kind:Sema.Symbol.Internal_type
            ~origin:
              (Sema.Symbol.Pinned_source
                 {
                   path = Generated.Primitive_raw_types.cinit_source_path;
                   line = entry.source_line;
                 })
        with
        | Ok symbol -> symbol
        | Error message -> invalid_arg message
      in
      let primitive =
        match Common.Primitive_type.of_spelling entry.spelling with
        | Some primitive -> primitive
        | None -> (
            match Common.Primitive_type.of_storage_spelling entry.spelling with
            | Some primitive -> primitive
            | None ->
                invalid_arg "seeded internal type lacks primitive metadata")
      in
      if (Common.Primitive_type.info primitive).byte_size <> entry.byte_size
      then
        invalid_arg
          "seeded primitive size differs from its pinned internal type";
      let record =
        Sema.Compiler_record.seed_primitive ~table ~entry:frontend_entry
          ~symbol:semantic_symbol ~primitive
        |> function
        | Ok record -> record
        | Error message -> invalid_arg message
      in
      { frontend_entry; semantic_symbol; primitive; record })
    primitives

let create () =
  let symbols = Symbol_visibility.Environment.create () in
  let frontend_primitives = seed_compiler_symbols symbols in
  let semantic_symbols =
    Sema.Symbol_table.create ~root_name:"session-task" ()
  in
  let primitives = seed_semantic_symbols semantic_symbols frontend_primitives in
  {
    sources = Common.Source_manager.create ();
    definitions = Frontend.Definition.Environment.create ();
    symbols;
    semantic_symbols;
    primitives;
  }

let fork_frontend session =
  let semantic_symbols =
    Sema.Symbol_table.create ~root_name:"session-task" ()
  in
  let primitives =
    List.map
      (fun binding ->
        let semantic_symbol =
          match
            Sema.Symbol_table.add semantic_symbols
              ~scope:(Sema.Symbol_table.root semantic_symbols)
              ~name:(Sema.Symbol.name binding.semantic_symbol)
              ~kind:Sema.Symbol.Internal_type
              ~origin:(Sema.Symbol.origin binding.semantic_symbol)
          with
          | Ok symbol -> symbol
          | Error message -> invalid_arg message
        in
        let record =
          Sema.Compiler_record.rebind_primitive ~table:semantic_symbols
            ~symbol:semantic_symbol binding.record
          |> function
          | Ok record -> record
          | Error message -> invalid_arg message
        in
        { binding with semantic_symbol; record })
      session.primitives
  in
  {
    sources = session.sources;
    definitions = Definition.Environment.copy session.definitions;
    symbols = Symbol_visibility.Environment.copy session.symbols;
    semantic_symbols;
    primitives;
  }

let task_frontend session =
  {
    session with
    definitions = Definition.Environment.task_view session.definitions;
    symbols = Symbol_visibility.Environment.task_view session.symbols;
  }

let sources session = session.sources
let definitions session = session.definitions
let symbols session = session.symbols
let semantic_symbols session = session.semantic_symbols

let primitive_for session entry =
  List.find_opt
    (fun binding -> binding.frontend_entry == entry)
    session.primitives

let primitive_symbol binding = binding.semantic_symbol
let primitive_type binding = binding.primitive
let primitive_record binding = binding.record

let add_source session ~path ~contents =
  Common.Source_manager.add_string session.sources ~path ~contents

let load_source ?max_bytes ?display_path session ~path =
  Common.Source_manager.load ?max_bytes ?display_path session.sources ~path
