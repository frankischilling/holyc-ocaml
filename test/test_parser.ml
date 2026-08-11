open Holyc_lib

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-parser-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.equal (String.sub text index needle_length) needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let pinned path =
  [ "third_party/TempleOS"; "../third_party/TempleOS" ]
  |> List.map (fun root -> Filename.concat root path)
  |> List.find_opt Sys.file_exists
  |> function
  | Some source -> read_file source
  | None -> Alcotest.failf "pinned source is unavailable: %s" path

let config ?include_roots ?compilation_mode working_directory =
  Preprocessor.Config.create ~working_directory ?include_roots ?compilation_mode
    ()
  |> function
  | Ok config -> config
  | Error message -> Alcotest.fail message

let parse_string ?(path = "input.HC") ?compilation_mode contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let config = config ?compilation_mode (Sys.getcwd ()) in
  let output = Holyc_lib.parse_detailed session ~config ~source in
  (session, source, output)

let expect_ast output =
  match output.Parser.ast with
  | Some ast -> ast
  | None ->
      Alcotest.failf "expected an AST, got diagnostics: %s"
        (String.concat ", "
           (List.map
              (fun diagnostic -> diagnostic.Diagnostic.code)
              output.diagnostics))

let expect_one_global ast =
  match ast.Ast.items with
  | [ Ast.Global_variable variable ] -> variable
  | [ Ast.Global_declaration _ ] ->
      Alcotest.fail "expected a singleton global, got a declaration group"
  | items ->
      Alcotest.failf "expected one global, got %d items" (List.length items)

let expect_one_declaration ast =
  match ast.Ast.items with
  | [ Ast.Global_declaration declaration ] -> declaration
  | [ Ast.Global_variable _ ] ->
      Alcotest.fail "expected a declaration group, got a singleton global"
  | items ->
      Alcotest.failf "expected one declaration group, got %d items"
        (List.length items)

let globals ast =
  List.map
    (function
      | Ast.Global_variable variable -> variable
      | Ast.Global_declaration _ ->
          Alcotest.fail "expected singleton globals, got a declaration group")
    ast.Ast.items

let first_diagnostic output =
  match output.Parser.diagnostics with
  | diagnostic :: _ -> diagnostic
  | [] -> Alcotest.fail "expected a diagnostic"

let supported_primitives () =
  let declarations =
    Primitive_type.all
    |> List.mapi (fun index primitive ->
        Printf.sprintf "%s value_%d;" (Primitive_type.to_string primitive) index)
    |> String.concat "\n"
  in
  let _, _, output = parse_string declarations in
  let ast = expect_ast output in
  Alcotest.(check int)
    "one declaration per primitive" 12 (List.length ast.items);
  List.iter2
    (fun primitive item ->
      match item with
      | Ast.Global_variable variable ->
          Alcotest.(check bool)
            "primitive identity" true
            (Primitive_type.equal primitive variable.type_specifier.primitive);
          Alcotest.(check string)
            "primitive spelling"
            (Primitive_type.to_string primitive)
            variable.type_specifier.spelling
      | Ast.Global_declaration _ ->
          Alcotest.fail "primitive fixture unexpectedly formed a group")
    Primitive_type.all ast.items

let pointer_depth_source_limit () =
  let kernel = pinned "Kernel/KernelA.HH" in
  let pointer_limit =
    kernel |> String.split_on_char '\n'
    |> List.find (fun line ->
        String.starts_with ~prefix:"#define PTR_STARS_NUM" line)
    |> fun line -> Scanf.sscanf line "#define PTR_STARS_NUM %d" Fun.id
  in
  Alcotest.(check int) "pinned pointer-star limit" 4 pointer_limit;
  Alcotest.(check int)
    "parser pointer-star limit" pointer_limit Parser.max_pointer_depth;
  let parser_source = pinned "Compiler/PrsVar.HC" in
  Alcotest.(check bool)
    "PrsType checks the shared limit" true
    (parser_source |> String.split_on_char '\n'
    |> List.exists (fun line ->
        String.equal (String.trim line) "if (++ptr_stars_cnt>PTR_STARS_NUM)"))

let pointer_layers () =
  let source =
    "I8 *signed_byte;\nU0 **zero_ptr;\nF64 ***float_ptr;\nBool ****boolean;"
  in
  let _, _, output = parse_string source in
  let variables = expect_ast output |> globals in
  let expected =
    [
      (Primitive_type.I8, "signed_byte", 1);
      (Primitive_type.U0, "zero_ptr", 2);
      (Primitive_type.F64, "float_ptr", 3);
      (Primitive_type.Bool, "boolean", 4);
    ]
  in
  List.iter2
    (fun (primitive, name, depth) (variable : Ast.global_variable) ->
      Alcotest.(check bool)
        (name ^ " base primitive") true
        (Primitive_type.equal primitive variable.type_specifier.primitive);
      Alcotest.(check string) (name ^ " identifier") name variable.name.spelling;
      Alcotest.(check (list int))
        (name ^ " ordered depths")
        (List.init depth (fun index -> index + 1))
        (List.map
           (fun (pointer : Ast.pointer_layer) -> pointer.depth)
           variable.pointer_layers);
      Alcotest.(check (list string))
        (name ^ " star spellings")
        (List.init depth (fun _ -> "*"))
        (List.map
           (fun (pointer : Ast.pointer_layer) -> pointer.spelling)
           variable.pointer_layers);
      List.iter
        (fun (pointer : Ast.pointer_layer) ->
          Alcotest.(check int)
            (name ^ " star width") 1
            (pointer.location.span.stop - pointer.location.span.start))
        variable.pointer_layers)
    expected variables;
  let signed_byte = List.hd variables in
  let first_pointer = List.hd signed_byte.pointer_layers in
  Alcotest.(check int) "first pointer start" 3 first_pointer.location.span.start;
  Alcotest.(check int) "first pointer stop" 4 first_pointer.location.span.stop

let definition_backed_pointer_layers () =
  let session, root, output = parse_string "#define PTR **\nU0 PTR value;" in
  let variable = expect_ast output |> expect_one_global in
  Alcotest.(check (list int))
    "two generated pointer depths" [ 1; 2 ]
    (List.map
       (fun (pointer : Ast.pointer_layer) -> pointer.depth)
       variable.pointer_layers);
  List.iter
    (fun (pointer : Ast.pointer_layer) ->
      Alcotest.(check bool)
        "generated frame differs from the root" false
        (Source_id.equal pointer.location.span.source (Source_file.id root));
      let generated_from =
        pointer.location.generated_from
        |> Option.value ~default:pointer.location.span
      in
      let defined_at =
        pointer.location.defined_at
        |> Option.value ~default:pointer.location.span
      in
      Alcotest.(check bool)
        "invocation points into the root" true
        (Source_id.equal generated_from.source (Source_file.id root));
      Alcotest.(check bool)
        "definition points into the root" true
        (Source_id.equal defined_at.source (Source_file.id root)))
    variable.pointer_layers;
  Alcotest.(check int)
    "declaration retains base, stars, name, and semicolon segments" 5
    (List.length variable.location.source_segments);
  let json = Ast_dump.to_yojson (Session.sources session) (expect_ast output) in
  let open Yojson.Safe.Util in
  let first_pointer =
    json |> member "module" |> member "items" |> to_list |> List.hd
    |> member "pointer_layers" |> to_list |> List.hd
  in
  Alcotest.(check bool)
    "JSON retains the invocation" true
    (first_pointer |> member "location" |> member "generated_from" <> `Null);
  Alcotest.(check bool)
    "JSON retains the definition" true
    (first_pointer |> member "location" |> member "defined_at" <> `Null)

let pointer_failures () =
  let cases =
    [
      ("excessive depth", "I64 *****value;", "HCPARSE0004");
      ("function pointer", "I64 (*function)();", "HCPARSE0005");
      ("missing name", "I64 *;", "HCPARSE0002");
      ("missing semicolon", "I64 *value", "HCPARSE0003");
    ]
  in
  List.iter
    (fun (name, source, code) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      Alcotest.(check string)
        (name ^ " diagnostic") code (first_diagnostic output).code)
    cases;
  let _, _, output = parse_string "I64 *****value;" in
  Alcotest.(check int)
    "the fifth star is primary" 8 (first_diagnostic output).primary.start

let pointer_declarations_update_symbol_conditionals () =
  let source =
    "I64 *declared;\n\
     #ifdef declared\n\
     U8 *selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  Alcotest.(check (list string))
    "the following conditional sees the pointer global"
    [ "declared"; "selected" ]
    (List.map
       (fun (variable : Ast.global_variable) -> variable.name.spelling)
       (globals ast))

let comma_source_behavior () =
  let parser_source = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains parser_source fragment))
    [
      ("global-list entry point", "U0 PrsGlblVarLst(CCmpCtrl *cc");
      ( "saved base type is reused",
        "tmpc=PrsType(cc,&saved_tmpc,&saved_mode,NULL,&st," );
      ("comma continues the list", "if (cc->token==',')");
    ];
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ("Demo/MultiCore/Primes.HC", "I64 prime_range,my_mp_cnt,pending;");
      ("Demo/MultiCore/MPRadix.HC", "I32 *arg1,*arg2;");
      ("Kernel/KDefine.HC", "U8 *ptr,**idx;");
    ]

let comma_declaration_group () =
  let _, _, output = parse_string "I64 first,*second,***third;" in
  let declaration = expect_ast output |> expect_one_declaration in
  Alcotest.(check bool)
    "shared primitive" true
    (Primitive_type.equal Primitive_type.I64
       declaration.type_specifier.primitive);
  Alcotest.(check (list string))
    "names"
    [ "first"; "second"; "third" ]
    (List.map
       (fun (item : Ast.global_declarator) -> item.name.spelling)
       declaration.declarators);
  Alcotest.(check (list int))
    "independent pointer depths" [ 0; 1; 3 ]
    (List.map
       (fun (item : Ast.global_declarator) -> List.length item.pointer_layers)
       declaration.declarators);
  let delimiter_name = function
    | Ast.Comma -> "comma"
    | Ast.Semicolon -> "semicolon"
  in
  Alcotest.(check (list string))
    "delimiter kinds"
    [ "comma"; "comma"; "semicolon" ]
    (List.map
       (fun (item : Ast.global_declarator) ->
         delimiter_name item.delimiter.kind)
       declaration.declarators);
  Alcotest.(check (list string))
    "delimiter spellings" [ ","; ","; ";" ]
    (List.map
       (fun (item : Ast.global_declarator) -> item.delimiter.spelling)
       declaration.declarators);
  Alcotest.(check (list int))
    "delimiter offsets" [ 9; 17; 26 ]
    (List.map
       (fun (item : Ast.global_declarator) ->
         item.delimiter.location.span.start)
       declaration.declarators);
  Alcotest.(check int) "group start" 0 declaration.location.span.start;
  Alcotest.(check int) "group stop" 27 declaration.location.span.stop

let definition_backed_comma_group () =
  let session, root, output =
    parse_string "#define NEXT ,**\nU8 *first NEXT second;"
  in
  let ast = expect_ast output in
  let declaration = expect_one_declaration ast in
  let first, second =
    match declaration.declarators with
    | [ first; second ] -> (first, second)
    | items ->
        Alcotest.failf "expected two declarators, got %d" (List.length items)
  in
  let generated_location description (location : Ast.location) =
    Alcotest.(check bool)
      (description ^ " uses a generated frame")
      false
      (Source_id.equal location.span.source (Source_file.id root));
    let generated_from =
      location.generated_from |> Option.value ~default:location.span
    in
    let defined_at =
      location.defined_at |> Option.value ~default:location.span
    in
    Alcotest.(check bool)
      (description ^ " retains the invocation")
      true
      (Source_id.equal generated_from.source (Source_file.id root));
    Alcotest.(check bool)
      (description ^ " retains the definition")
      true
      (Source_id.equal defined_at.source (Source_file.id root))
  in
  generated_location "comma" first.delimiter.location;
  Alcotest.(check int)
    "the next declarator receives two stars" 2
    (List.length second.pointer_layers);
  List.iter
    (fun (pointer : Ast.pointer_layer) ->
      generated_location "pointer" pointer.location)
    second.pointer_layers;
  let open Yojson.Safe.Util in
  let item =
    Ast_dump.to_yojson (Session.sources session) ast
    |> member "module" |> member "items" |> to_list |> List.hd
  in
  let declarators = item |> member "declarators" |> to_list in
  let first_delimiter = List.hd declarators |> member "delimiter" in
  let second_pointers =
    List.nth declarators 1 |> member "pointer_layers" |> to_list
  in
  Alcotest.(check bool)
    "JSON retains comma invocation" true
    (first_delimiter |> member "location" |> member "generated_from" <> `Null);
  Alcotest.(check bool)
    "JSON retains pointer definitions" true
    (List.for_all
       (fun pointer ->
         pointer |> member "location" |> member "defined_at" <> `Null)
       second_pointers)

let comma_streaming_visibility () =
  let source =
    "I64 first,\n#ifdef first\n*second;\n#else\nWidget wrong;\n#endif"
  in
  let _, _, output = parse_string source in
  let declaration = expect_ast output |> expect_one_declaration in
  Alcotest.(check (list string))
    "the next token sees the preceding declaration" [ "first"; "second" ]
    (List.map
       (fun (item : Ast.global_declarator) -> item.name.spelling)
       declaration.declarators)

let comma_failures () =
  List.iter
    (fun (name, source, code) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      Alcotest.(check string)
        (name ^ " diagnostic") code (first_diagnostic output).code)
    [
      ( "second declarator exceeds pointer depth",
        "I64 first,*****second;",
        "HCPARSE0004" );
      ("list lacks its final semicolon", "I64 first,*second", "HCPARSE0003");
    ]

let modifier_source_behavior () =
  let parser_source = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains parser_source fragment))
    [
      ("static keyword branch", "case KW_STATIC:");
      ("static replaces staged flags", "fsp_flags=FSF_STATIC|fsp_flags&FSF_ASM;");
      ("public keyword branch", "case KW_PUBLIC:");
      ( "public retains the function flag group",
        "fsp_flags=FSF_PUBLIC|fsp_flags&(FSG_FUN_FLAGS2|FSF_ASM);" );
      ("public reaches global variables", "if (fsp_flags&FSF_PUBLIC)");
    ];
  let header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains header fragment))
    [
      ("public keyword identity", "#define KW_PUBLIC\t25");
      ("static keyword identity", "#define KW_STATIC\t41");
      ("public staged flag", "#define FSF_PUBLIC\t\t0x01");
      ("static staged flag", "#define FSF_STATIC\t\t0x04");
    ]

let direct_modifiers () =
  let source =
    "public I64 exported;\n\
     static U8 hidden;\n\
     public static Bool public_then_static;\n\
     static public I16 static_then_public;\n\
     public public I8 repeated;"
  in
  let _, _, output = parse_string source in
  let variables = expect_ast output |> globals in
  let modifier_name = function
    | Ast.Public -> "public"
    | Ast.Static -> "static"
  in
  Alcotest.(check (list (list string)))
    "ordered modifier spellings"
    [
      [ "public" ];
      [ "static" ];
      [ "public"; "static" ];
      [ "static"; "public" ];
      [ "public"; "public" ];
    ]
    (List.map
       (fun (variable : Ast.global_variable) ->
         List.map
           (fun (modifier : Ast.declaration_modifier) -> modifier.spelling)
           variable.modifiers)
       variables);
  Alcotest.(check (list (list string)))
    "ordered modifier kinds"
    [
      [ "public" ];
      [ "static" ];
      [ "public"; "static" ];
      [ "static"; "public" ];
      [ "public"; "public" ];
    ]
    (List.map
       (fun (variable : Ast.global_variable) ->
         List.map
           (fun (modifier : Ast.declaration_modifier) ->
             modifier_name modifier.kind)
           variable.modifiers)
       variables);
  let exported = List.hd variables in
  let public = List.hd exported.modifiers in
  Alcotest.(check int)
    "public starts the declaration" 0 public.location.span.start;
  Alcotest.(check int)
    "public width" 6
    (public.location.span.stop - public.location.span.start);
  Alcotest.(check int)
    "declaration includes the prefix" 0 exported.location.span.start

let modifier_declaration_group () =
  let _, _, output = parse_string "public I64 first,*second;" in
  let declaration = expect_ast output |> expect_one_declaration in
  Alcotest.(check (list string))
    "one group modifier" [ "public" ]
    (List.map
       (fun (modifier : Ast.declaration_modifier) -> modifier.spelling)
       declaration.modifiers);
  Alcotest.(check (list string))
    "group names" [ "first"; "second" ]
    (List.map
       (fun (item : Ast.global_declarator) -> item.name.spelling)
       declaration.declarators);
  Alcotest.(check (list int))
    "group pointer depths" [ 0; 1 ]
    (List.map
       (fun (item : Ast.global_declarator) -> List.length item.pointer_layers)
       declaration.declarators);
  Alcotest.(check int)
    "group includes the prefix" 0 declaration.location.span.start

let definition_backed_modifier () =
  let session, root, output =
    parse_string "#define VIS public\nVIS U64 visible;"
  in
  let ast = expect_ast output in
  let variable = expect_one_global ast in
  let modifier =
    match variable.modifiers with
    | [ modifier ] -> modifier
    | items ->
        Alcotest.failf "expected one modifier, got %d" (List.length items)
  in
  Alcotest.(check string) "replacement spelling" "public" modifier.spelling;
  Alcotest.(check bool)
    "modifier uses a generated frame" false
    (Source_id.equal modifier.location.span.source (Source_file.id root));
  let generated_from =
    modifier.location.generated_from
    |> Option.value ~default:modifier.location.span
  in
  let defined_at =
    modifier.location.defined_at |> Option.value ~default:modifier.location.span
  in
  Alcotest.(check bool)
    "modifier retains the invocation" true
    (Source_id.equal generated_from.source (Source_file.id root));
  Alcotest.(check bool)
    "modifier retains the definition" true
    (Source_id.equal defined_at.source (Source_file.id root));
  let open Yojson.Safe.Util in
  let modifier_json =
    Ast_dump.to_yojson (Session.sources session) ast
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "modifiers" |> to_list |> List.hd
  in
  Alcotest.(check bool)
    "JSON retains the invocation" true
    (modifier_json |> member "location" |> member "generated_from" <> `Null);
  Alcotest.(check bool)
    "JSON retains the definition" true
    (modifier_json |> member "location" |> member "defined_at" <> `Null)

let modifier_streaming_visibility () =
  let source =
    "static I64 declared;\n\
     #ifdef declared\n\
     public U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  let variables = expect_ast output |> globals in
  Alcotest.(check (list string))
    "the conditional sees the modified declaration" [ "declared"; "selected" ]
    (List.map
       (fun (variable : Ast.global_variable) -> variable.name.spelling)
       variables)

let modifier_failures () =
  List.iter
    (fun (name, source, message_fragment) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string)
        (name ^ " diagnostic") "HCPARSE0001" diagnostic.code;
      Alcotest.(check bool)
        (name ^ " message") true
        (contains diagnostic.message message_fragment))
    [
      ( "public lacks a type",
        "public ;",
        "after declaration modifier \"public\"" );
      ( "static precedes an unresolved type",
        "static Widget invalid;",
        "after declaration modifier \"static\"" );
    ]

let binding_source_behavior () =
  let parser_source = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains parser_source fragment))
    [
      ("extern keyword branch", "case KW_EXTERN:");
      ( "extern declaration mode",
        "PrsGlblVarLst(cc,PRS0_EXTERN|PRS1_NULL,tmpex,0,fsp_flags);" );
      ("import keyword branch", "case KW_IMPORT:");
      ( "import declaration mode",
        "PrsGlblVarLst(cc,PRS0_IMPORT|PRS1_NULL,tmpex,0,fsp_flags);" );
      ("import requires AOT", "if (!(cc->flags&CCF_AOT_COMPILE))");
    ];
  let header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains header fragment))
    [
      ("extern keyword identity", "#define KW_EXTERN\t10");
      ("import keyword identity", "#define KW_IMPORT\t27");
      ("extern parser mode", "#define PRS0_EXTERN\t\t0x000004");
      ("import parser mode", "#define PRS0_IMPORT\t\t0x000005");
    ];
  let kernel = pinned "Kernel/KernelC.HH" in
  Alcotest.(check bool)
    "pinned grouped extern" true
    (contains kernel "public extern U8 *rev_bits_table,*set_bits_table;");
  let declarations = pinned "Compiler/CompilerB.HH" in
  Alcotest.(check bool)
    "pinned direct extern" true
    (contains declarations "public extern CCmpGlbls cmp;")

let direct_bindings () =
  let source =
    "extern I64 forwarded;\npublic extern U8 *rev_bits_table,*set_bits_table;"
  in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  let binding_name = function
    | Ast.Extern -> "extern"
    | Ast.Import -> "import"
  in
  (match ast.items with
  | [ Ast.Global_variable forwarded; Ast.Global_declaration bits ] ->
      let binding = Option.get forwarded.binding in
      Alcotest.(check string)
        "extern binding kind" "extern"
        (binding_name binding.kind);
      Alcotest.(check string) "extern spelling" "extern" binding.spelling;
      Alcotest.(check bool)
        "ordinary extern has no alternate target" true
        (Option.is_none binding.target);
      Alcotest.(check int)
        "extern starts at byte zero" 0 binding.location.span.start;
      Alcotest.(check (list string))
        "public prefix stays separate" [ "public" ]
        (List.map
           (fun (modifier : Ast.declaration_modifier) -> modifier.spelling)
           bits.modifiers);
      Alcotest.(check string)
        "group binding" "extern"
        (bits.binding |> Option.get |> fun binding -> binding_name binding.kind);
      Alcotest.(check (list string))
        "group names"
        [ "rev_bits_table"; "set_bits_table" ]
        (List.map
           (fun (item : Ast.global_declarator) -> item.name.spelling)
           bits.declarators)
  | items ->
      Alcotest.failf "expected two bound globals, got %d" (List.length items));
  let _, _, import_output =
    parse_string ~compilation_mode:Preprocessor.Aot "import F64 imported;"
  in
  let imported = expect_ast import_output |> expect_one_global in
  let binding = Option.get imported.binding in
  Alcotest.(check string)
    "AOT import binding" "import"
    (binding_name binding.kind)

let definition_backed_binding () =
  let session, root, output =
    parse_string "#define LINK extern\nLINK U64 visible;"
  in
  let ast = expect_ast output in
  let variable = expect_one_global ast in
  let binding = Option.get variable.binding in
  Alcotest.(check string) "replacement spelling" "extern" binding.spelling;
  Alcotest.(check bool)
    "binding uses a generated frame" false
    (Source_id.equal binding.location.span.source (Source_file.id root));
  let generated_from =
    binding.location.generated_from
    |> Option.value ~default:binding.location.span
  in
  let defined_at =
    binding.location.defined_at |> Option.value ~default:binding.location.span
  in
  Alcotest.(check bool)
    "binding retains the invocation" true
    (Source_id.equal generated_from.source (Source_file.id root));
  Alcotest.(check bool)
    "binding retains the definition" true
    (Source_id.equal defined_at.source (Source_file.id root));
  let open Yojson.Safe.Util in
  let binding_json =
    Ast_dump.to_yojson (Session.sources session) ast
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding"
  in
  Alcotest.(check bool)
    "JSON retains the invocation" true
    (binding_json |> member "location" |> member "generated_from" <> `Null);
  Alcotest.(check bool)
    "JSON retains the definition" true
    (binding_json |> member "location" |> member "defined_at" <> `Null)

let binding_streaming_visibility () =
  let source =
    "extern I64 declared;\n\
     #ifdef declared\n\
     public U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  let variables = expect_ast output |> globals in
  Alcotest.(check (list string))
    "the conditional sees the extern declaration" [ "declared"; "selected" ]
    (List.map
       (fun (variable : Ast.global_variable) -> variable.name.spelling)
       variables)

let import_mode_boundary () =
  let session, _, output = parse_string "import I64 unavailable;" in
  Alcotest.(check bool) "JIT import has no AST" true (Option.is_none output.ast);
  let diagnostic = first_diagnostic output in
  Alcotest.(check string) "JIT import diagnostic" "HCPARSE0006" diagnostic.code;
  Alcotest.(check bool)
    "JIT import explains the mode" true
    (contains diagnostic.message "require AOT mode");
  (match
     Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
       "unavailable"
   with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "a rejected JIT import became visible");
  let _, _, aot_output =
    parse_string ~compilation_mode:Preprocessor.Aot "import I64 available;"
  in
  ignore (expect_ast aot_output)

let binding_failures () =
  List.iter
    (fun (name, source, message_fragment) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string)
        (name ^ " diagnostic") "HCPARSE0001" diagnostic.code;
      Alcotest.(check bool)
        (name ^ " message") true
        (contains diagnostic.message message_fragment))
    [
      ("extern lacks a type", "extern ;", "after declaration binding \"extern\"");
      ( "binding cannot repeat",
        "extern import I64 duplicate;",
        "after declaration binding \"extern\"" );
      ( "modifier cannot follow binding",
        "extern public I64 wrong_order;",
        "after declaration binding \"extern\"" );
      ( "underscored intern is not in this slice",
        "_intern I64 unavailable;",
        "at the start of a global declaration" );
    ]

let alternate_binding_source_behavior () =
  let parser_source = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains parser_source fragment))
    [
      ("alternate extern branch", "case KW__EXTERN:");
      ( "alternate extern mode",
        "PrsGlblVarLst(cc,PRS0__EXTERN|PRS1_NULL,tmpex,i,fsp_flags);" );
      ("alternate import branch", "case KW__IMPORT:");
      ("alternate import mode", "PrsGlblVarLst(cc,PRS0__IMPORT|PRS1_NULL,tmpex,");
      ("extern-to-import option", "Bt(&cc->opts,OPTf_EXTERNS_TO_IMPORTS)");
    ];
  let header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains header fragment))
    [
      ("alternate extern keyword identity", "#define KW__EXTERN\t11");
      ("alternate import keyword identity", "#define KW__IMPORT\t28");
      ("alternate extern parser mode", "#define PRS0__EXTERN\t\t0x000001");
      ("alternate import parser mode", "#define PRS0__IMPORT\t\t0x000003");
    ];
  let kernel = pinned "Kernel/KernelB.HH" in
  Alcotest.(check bool)
    "pinned alternate-name global" true
    (contains kernel "_extern MEM_BOOT_BASE U32 mem_boot_base;")

let direct_alternate_bindings () =
  let source =
    "_extern C32_EAX U32 c32_eax;\n\
     public _extern SYS_BOOT_BASE U32 boot_base,*boot_pointer;"
  in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  (match ast.items with
  | [ Ast.Global_variable c32_eax; Ast.Global_declaration boot ] ->
      let c32_binding = Option.get c32_eax.binding in
      Alcotest.(check string)
        "alternate extern spelling" "_extern" c32_binding.spelling;
      Alcotest.(check string)
        "alternate extern target" "C32_EAX"
        (c32_binding.target |> Option.get |> fun target -> target.spelling);
      Alcotest.(check int)
        "target starts after the binding" 8
        ( c32_binding.target |> Option.get |> fun target ->
          target.location.span.start );
      Alcotest.(check (list string))
        "group keeps its modifier" [ "public" ]
        (List.map
           (fun (modifier : Ast.declaration_modifier) -> modifier.spelling)
           boot.modifiers);
      Alcotest.(check string)
        "group target" "SYS_BOOT_BASE"
        ( boot.binding |> Option.get |> fun binding ->
          binding.target |> Option.get |> fun target -> target.spelling );
      Alcotest.(check (list string))
        "group local names"
        [ "boot_base"; "boot_pointer" ]
        (List.map
           (fun (item : Ast.global_declarator) -> item.name.spelling)
           boot.declarators)
  | items ->
      Alcotest.failf "expected two alternate-name globals, got %d"
        (List.length items));
  let _, _, import_output =
    parse_string ~compilation_mode:Preprocessor.Aot
      "_import REMOTE_CLOCK I64 clock_ticks;"
  in
  let binding =
    expect_ast import_output |> expect_one_global |> fun variable ->
    Option.get variable.binding
  in
  Alcotest.(check string)
    "alternate import target" "REMOTE_CLOCK"
    (binding.target |> Option.get |> fun target -> target.spelling)

let definition_backed_alternate_binding () =
  let session, root, output =
    parse_string
      "#define LINK _extern\n\
       #define TARGET SYS_BOOT_BASE\n\
       LINK TARGET U32 boot;"
  in
  let ast = expect_ast output in
  let binding =
    expect_one_global ast |> fun variable -> Option.get variable.binding
  in
  let target = Option.get binding.target in
  Alcotest.(check bool)
    "binding uses a generated frame" false
    (Source_id.equal binding.location.span.source (Source_file.id root));
  Alcotest.(check bool)
    "target uses a generated frame" false
    (Source_id.equal target.location.span.source (Source_file.id root));
  Alcotest.(check bool)
    "target retains its invocation" true
    (target.location.generated_from |> Option.is_some);
  Alcotest.(check bool)
    "target retains its definition" true
    (target.location.defined_at |> Option.is_some);
  let open Yojson.Safe.Util in
  let target_json =
    Ast_dump.to_yojson (Session.sources session) ast
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target"
  in
  Alcotest.(check string)
    "JSON target spelling" "SYS_BOOT_BASE"
    (target_json |> member "spelling" |> to_string);
  Alcotest.(check bool)
    "JSON target invocation" true
    (target_json |> member "location" |> member "generated_from" <> `Null);
  Alcotest.(check bool)
    "JSON target definition" true
    (target_json |> member "location" |> member "defined_at" <> `Null)

let alternate_binding_streaming_visibility () =
  let source =
    "_extern SYS_BOOT_BASE I64 declared;\n\
     #ifdef declared\n\
     public U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  let variables = expect_ast output |> globals in
  Alcotest.(check (list string))
    "the conditional sees the local alias" [ "declared"; "selected" ]
    (List.map
       (fun (variable : Ast.global_variable) -> variable.name.spelling)
       variables)

let alternate_import_mode_boundary () =
  let session, _, output =
    parse_string "_import REMOTE_CLOCK I64 unavailable;"
  in
  Alcotest.(check bool)
    "JIT alternate import has no AST" true
    (Option.is_none output.ast);
  let diagnostic = first_diagnostic output in
  Alcotest.(check string)
    "JIT alternate import diagnostic" "HCPARSE0006" diagnostic.code;
  Alcotest.(check int) "diagnostic points at _import" 0 diagnostic.primary.start;
  (match
     Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
       "unavailable"
   with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "a rejected alternate import became visible");
  let _, _, aot_output =
    parse_string ~compilation_mode:Preprocessor.Aot
      "_import REMOTE_CLOCK I64 available;"
  in
  ignore (expect_ast aot_output)

let alternate_binding_failures () =
  List.iter
    (fun (name, source, code, message_fragment) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (name ^ " diagnostic") code diagnostic.code;
      Alcotest.(check bool)
        (name ^ " message") true
        (contains diagnostic.message message_fragment))
    [
      ( "alternate extern lacks a target",
        "_extern ;",
        "HCPARSE0007",
        "expected a target symbol" );
      ( "alternate import target must be an identifier",
        "_import 42 I64 value;",
        "HCPARSE0007",
        "found \"42\"" );
      ( "binding cannot repeat after an alternate target",
        "_extern TARGET extern I64 duplicate;",
        "HCPARSE0001",
        "after declaration binding \"_extern\"" );
      ( "modifier cannot follow an alternate target",
        "_extern TARGET public I64 wrong_order;",
        "HCPARSE0001",
        "after declaration binding \"_extern\"" );
      ( "underscored intern still needs an expression AST",
        "_intern IC_BSF I64 unavailable;",
        "HCPARSE0001",
        "at the start of a global declaration" );
    ]

let empty_and_comment_only () =
  List.iter
    (fun contents ->
      let _, source, output = parse_string contents in
      let ast = expect_ast output in
      Alcotest.(check int) "empty item list" 0 (List.length ast.items);
      Alcotest.(check int) "module starts at zero" 0 ast.span.start;
      Alcotest.(check int)
        "module covers the root source"
        (Source_file.length source)
        ast.span.stop)
    [ ""; "// nothing to declare\n"; "/* still empty */" ]

let order_and_spans () =
  let session, _, output = parse_string "I64 first;\nU8 second;" in
  let ast = expect_ast output in
  let variables =
    List.map
      (function
        | Ast.Global_variable variable -> variable
        | Ast.Global_declaration _ ->
            Alcotest.fail "independent declarations unexpectedly formed a group")
      ast.items
  in
  Alcotest.(check (list string))
    "source order" [ "first"; "second" ]
    (List.map (fun variable -> variable.Ast.name.spelling) variables);
  let first = List.hd variables in
  Alcotest.(check int) "declaration start" 0 first.location.span.start;
  Alcotest.(check int) "declaration stop" 10 first.location.span.stop;
  Alcotest.(check int) "type stop" 3 first.type_specifier.location.span.stop;
  Alcotest.(check int) "name start" 4 first.name.location.span.start;
  Alcotest.(check int) "semicolon start" 9 first.semicolon.start;
  let position =
    Source_manager.find (Session.sources session)
      first.name.location.span.source
    |> Option.get
    |> fun source ->
    Source_file.position source first.name.location.span.start |> Result.get_ok
  in
  Alcotest.(check int) "name line" 1 position.line;
  Alcotest.(check int) "name column" 5 position.column

let definition_backed_type () =
  let _, root, output = parse_string "#define T I64\nT count;" in
  let variable = expect_ast output |> expect_one_global in
  Alcotest.(check bool)
    "definition resolves to I64" true
    (Primitive_type.equal Primitive_type.I64 variable.type_specifier.primitive);
  Alcotest.(check string)
    "replacement spelling" "I64" variable.type_specifier.spelling;
  Alcotest.(check bool)
    "type is generated from a separate frame" false
    (Source_id.equal variable.type_specifier.location.span.source
       (Source_file.id root))

let included_declaration () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let declaration_file = Filename.concat root "declaration.HC" in
      write_file root_file "#include \"declaration\"";
      write_file declaration_file "U16 included;";
      let session = Session.create () in
      let source =
        Session.load_source session ~path:root_file |> function
        | Ok source -> source
        | Error message -> Alcotest.fail message
      in
      let output =
        Holyc_lib.parse_detailed session ~config:(config root) ~source
      in
      let variable = expect_ast output |> expect_one_global in
      let declaration_source =
        Source_manager.find (Session.sources session)
          variable.location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "declaration keeps its canonical included path"
        (Unix.realpath declaration_file)
        (Source_file.path declaration_source);
      Alcotest.(check string)
        "declaration keeps its include spelling" "declaration"
        (Source_file.display_path declaration_source))

let definition_diagnostic_context () =
  let _, _, output = parse_string "#define BAD *\nI64 BAD;" in
  Alcotest.(check bool) "parse failed" true (Option.is_none output.ast);
  let diagnostic = first_diagnostic output in
  Alcotest.(check string) "diagnostic code" "HCPARSE0002" diagnostic.code;
  Alcotest.(check int)
    "expansion and declaration notes" 2
    (List.length diagnostic.secondary);
  Alcotest.(check bool)
    "expansion note" true
    (List.exists
       (fun (item : Diagnostic.related) ->
         String.equal item.Diagnostic.message
           "definition \"BAD\" was expanded here")
       diagnostic.secondary)

let include_diagnostic_context () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      write_file root_file "#include \"bad\"";
      write_file (Filename.concat root "bad.HC") "I64 ;";
      let session = Session.create () in
      let source =
        Session.load_source session ~path:root_file |> Result.get_ok
      in
      let output =
        Holyc_lib.parse_detailed session ~config:(config root) ~source
      in
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) "diagnostic code" "HCPARSE0002" diagnostic.code;
      Alcotest.(check int)
        "one include origin" 1
        (List.length diagnostic.include_stack);
      Alcotest.(check string)
        "include spelling" "#include \"bad\""
        (List.hd diagnostic.include_stack).message)

let warnings_do_not_discard_ast () =
  let _, _, output = parse_string "#assert 0\nI64 value;" in
  ignore (expect_ast output);
  Alcotest.(check (list string))
    "warning retained" [ "HCPP0024" ]
    (List.map (fun diagnostic -> diagnostic.Diagnostic.code) output.diagnostics);
  Alcotest.(check bool)
    "warning is not a parse error" false (Parser.has_errors output)

let declarations_update_symbol_conditionals () =
  let source =
    "I64 declared;\n#ifdef declared\nU8 selected;\n#else\nWidget wrong;\n#endif"
  in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  Alcotest.(check (list string))
    "the following conditional sees the declaration" [ "declared"; "selected" ]
    (List.map
       (function
         | Ast.Global_variable variable -> variable.Ast.name.spelling
         | Ast.Global_declaration _ ->
             Alcotest.fail "conditional fixture unexpectedly formed a group")
       ast.items)

let unsupported_forms () =
  let cases =
    [
      ("unknown type", "Widget value;", "HCPARSE0001");
      ("missing name", "I64 ;", "HCPARSE0002");
      ("array", "I64 value[3];", "HCPARSE0003");
      ("missing semicolon", "I64 value", "HCPARSE0003");
      ("initializer", "I64 value=1;", "HCPARSE0003");
      ("function", "I64 Function();", "HCPARSE0003");
      ("statement", "return;", "HCPARSE0001");
    ]
  in
  List.iter
    (fun (name, source, code) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      Alcotest.(check string)
        (name ^ " diagnostic") code (first_diagnostic output).code)
    cases

let deterministic_dumps () =
  let session, _, output = parse_string "I64 first;\nBool ready;" in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  let parsed = Yojson.Safe.from_string json in
  Alcotest.(check string)
    "schema" Ast_dump.schema
    Yojson.Safe.Util.(parsed |> member "schema" |> to_string)

let default_library_entry_point () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"memory.HC" ~contents:"I8 byte;"
  in
  match Holyc_lib.parse session ~source with
  | Error _ -> Alcotest.fail "default parse entry point failed"
  | Ok ast ->
      let variable = expect_one_global ast in
      Alcotest.(check string) "parsed name" "byte" variable.name.spelling

let tests =
  [
    Alcotest.test_case "all public primitive spellings" `Quick
      supported_primitives;
    Alcotest.test_case "pinned pointer depth" `Quick pointer_depth_source_limit;
    Alcotest.test_case "pointer layers" `Quick pointer_layers;
    Alcotest.test_case "definition-backed pointer layers" `Quick
      definition_backed_pointer_layers;
    Alcotest.test_case "pointer failures" `Quick pointer_failures;
    Alcotest.test_case "pointer declarations update symbol conditionals" `Quick
      pointer_declarations_update_symbol_conditionals;
    Alcotest.test_case "pinned comma declaration behavior" `Quick
      comma_source_behavior;
    Alcotest.test_case "comma declaration group" `Quick comma_declaration_group;
    Alcotest.test_case "definition-backed comma declaration" `Quick
      definition_backed_comma_group;
    Alcotest.test_case "comma declarations update symbol conditionals" `Quick
      comma_streaming_visibility;
    Alcotest.test_case "comma declaration failures" `Quick comma_failures;
    Alcotest.test_case "pinned declaration modifier behavior" `Quick
      modifier_source_behavior;
    Alcotest.test_case "direct declaration modifiers" `Quick direct_modifiers;
    Alcotest.test_case "modifier on a declaration group" `Quick
      modifier_declaration_group;
    Alcotest.test_case "definition-backed declaration modifier" `Quick
      definition_backed_modifier;
    Alcotest.test_case "modified declarations update symbol conditionals" `Quick
      modifier_streaming_visibility;
    Alcotest.test_case "declaration modifier failures" `Quick modifier_failures;
    Alcotest.test_case "pinned extern and import behavior" `Quick
      binding_source_behavior;
    Alcotest.test_case "direct declaration bindings" `Quick direct_bindings;
    Alcotest.test_case "definition-backed declaration binding" `Quick
      definition_backed_binding;
    Alcotest.test_case "bound declarations update symbol conditionals" `Quick
      binding_streaming_visibility;
    Alcotest.test_case "import compilation mode boundary" `Quick
      import_mode_boundary;
    Alcotest.test_case "declaration binding failures" `Quick binding_failures;
    Alcotest.test_case "pinned alternate-name binding behavior" `Quick
      alternate_binding_source_behavior;
    Alcotest.test_case "direct alternate-name bindings" `Quick
      direct_alternate_bindings;
    Alcotest.test_case "definition-backed alternate-name binding" `Quick
      definition_backed_alternate_binding;
    Alcotest.test_case "alternate-name bindings update symbol conditionals"
      `Quick alternate_binding_streaming_visibility;
    Alcotest.test_case "alternate import compilation mode boundary" `Quick
      alternate_import_mode_boundary;
    Alcotest.test_case "alternate-name binding failures" `Quick
      alternate_binding_failures;
    Alcotest.test_case "empty and comment-only modules" `Quick
      empty_and_comment_only;
    Alcotest.test_case "source order and spans" `Quick order_and_spans;
    Alcotest.test_case "definition-backed primitive" `Quick
      definition_backed_type;
    Alcotest.test_case "included declaration" `Quick included_declaration;
    Alcotest.test_case "definition diagnostic context" `Quick
      definition_diagnostic_context;
    Alcotest.test_case "include diagnostic context" `Quick
      include_diagnostic_context;
    Alcotest.test_case "warnings retain the AST" `Quick
      warnings_do_not_discard_ast;
    Alcotest.test_case "declarations update symbol conditionals" `Quick
      declarations_update_symbol_conditionals;
    Alcotest.test_case "unsupported forms fail" `Quick unsupported_forms;
    Alcotest.test_case "deterministic dumps" `Quick deterministic_dumps;
    Alcotest.test_case "default library entry point" `Quick
      default_library_entry_point;
  ]
