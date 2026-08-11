type t = {
  sources : Common.Source_manager.t;
  definitions : Frontend.Definition.Environment.t;
  symbols : Frontend.Symbol_visibility.Environment.t;
}

module Symbol_visibility = Frontend.Symbol_visibility

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
  List.iter
    (fun (entry : Generated.Primitive_raw_types.internal_type) ->
      add_pinned symbols ~name:entry.spelling
        ~kind:Symbol_visibility.Internal_type
        ~path:Generated.Primitive_raw_types.cinit_source_path
        ~line:entry.source_line)
    Generated.Primitive_raw_types.internal_types;
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
    Generated.Opcode_keywords.opcodes

let create () =
  let symbols = Symbol_visibility.Environment.create () in
  seed_compiler_symbols symbols;
  {
    sources = Common.Source_manager.create ();
    definitions = Frontend.Definition.Environment.create ();
    symbols;
  }

let sources session = session.sources
let definitions session = session.definitions
let symbols session = session.symbols

let add_source session ~path ~contents =
  Common.Source_manager.add_string session.sources ~path ~contents

let load_source ?max_bytes ?display_path session ~path =
  Common.Source_manager.load ?max_bytes ?display_path session.sources ~path
