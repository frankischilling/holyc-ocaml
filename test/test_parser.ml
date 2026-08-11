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
  | [ Ast.Function_prototype _ ] ->
      Alcotest.fail "expected a singleton global, got a function prototype"
  | items ->
      Alcotest.failf "expected one global, got %d items" (List.length items)

let expect_one_declaration ast =
  match ast.Ast.items with
  | [ Ast.Global_declaration declaration ] -> declaration
  | [ Ast.Global_variable _ ] ->
      Alcotest.fail "expected a declaration group, got a singleton global"
  | [ Ast.Function_prototype _ ] ->
      Alcotest.fail "expected a declaration group, got a function prototype"
  | items ->
      Alcotest.failf "expected one declaration group, got %d items"
        (List.length items)

let expect_one_prototype ast =
  match ast.Ast.items with
  | [ Ast.Function_prototype prototype ] -> prototype
  | [ Ast.Global_variable _ ] ->
      Alcotest.fail "expected a function prototype, got a singleton global"
  | [ Ast.Global_declaration _ ] ->
      Alcotest.fail "expected a function prototype, got a declaration group"
  | items ->
      Alcotest.failf "expected one function prototype, got %d items"
        (List.length items)

let expect_function_pointer (parameter : Ast.function_parameter) =
  match parameter.function_pointer with
  | Some function_pointer -> function_pointer
  | None -> Alcotest.fail "expected a function-pointer parameter"

let expect_parameter_default (parameter : Ast.function_parameter) =
  match parameter.default with
  | Some default -> default
  | None -> Alcotest.fail "expected a parameter default"

let expect_symbol_binding_target (binding : Ast.declaration_binding) =
  match binding.target with
  | Ast.Symbol_binding_target target -> target
  | Ast.No_binding_target | Ast.Expression_binding_target _ ->
      Alcotest.fail "expected a symbol binding target"

let expect_expression_binding_target (binding : Ast.declaration_binding) =
  match binding.target with
  | Ast.Expression_binding_target target -> target
  | Ast.No_binding_target | Ast.Symbol_binding_target _ ->
      Alcotest.fail "expected an expression binding target"

let expect_dimension_expression (dimension : Ast.array_dimension) =
  match dimension.dimension_expression with
  | Some expression -> expression
  | None -> Alcotest.fail "expected a sized array dimension"

let expect_integer_expression = function
  | Ast.Integer_literal literal -> (
      match literal.literal_value with
      | Ast.Integer_value value -> value
      | Ast.Float_value _ | Ast.Bytes_value _ ->
          Alcotest.fail "integer literal has the wrong value kind")
  | _ -> Alcotest.fail "expected an integer expression"

let expect_binary_expression = function
  | Ast.Binary_expression binary -> binary
  | _ -> Alcotest.fail "expected a binary expression"

let expect_prefix_expression = function
  | Ast.Prefix_expression prefix -> prefix
  | _ -> Alcotest.fail "expected a prefix expression"

let globals ast =
  List.map
    (function
      | Ast.Global_variable variable -> variable
      | Ast.Global_declaration _ ->
          Alcotest.fail "expected singleton globals, got a declaration group"
      | Ast.Function_prototype _ ->
          Alcotest.fail "expected singleton globals, got a function prototype")
    ast.Ast.items

let prototypes ast =
  List.map
    (function
      | Ast.Function_prototype prototype -> prototype
      | Ast.Global_variable _ ->
          Alcotest.fail "expected function prototypes, got a singleton global"
      | Ast.Global_declaration _ ->
          Alcotest.fail "expected function prototypes, got a declaration group")
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
          Alcotest.fail "primitive fixture unexpectedly formed a group"
      | Ast.Function_prototype _ ->
          Alcotest.fail "primitive fixture unexpectedly formed a prototype")
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

let modifier_name = function
  | Ast.Public -> "public"
  | Ast.Static -> "static"
  | Ast.Interrupt -> "interrupt"
  | Ast.Has_error_code -> "haserrcode"
  | Ast.Argument_pop -> "argpop"
  | Ast.No_argument_pop -> "noargpop"

let function_flag_modifier = function
  | Ast.Public -> Function_flag.Modifier.Public
  | Ast.Static -> Function_flag.Modifier.Static
  | Ast.Interrupt -> Function_flag.Modifier.Interrupt
  | Ast.Has_error_code -> Function_flag.Modifier.Has_error_code
  | Ast.Argument_pop -> Function_flag.Modifier.Argument_pop
  | Ast.No_argument_pop -> Function_flag.Modifier.No_argument_pop

let register_qualifier_signature (qualifier : Ast.register_qualifier) =
  let kind =
    match qualifier.kind with
    | Ast.Reg -> "reg"
    | Ast.Noreg -> "noreg"
  in
  let position =
    match qualifier.position with
    | Ast.Before_type -> "before_type"
    | Ast.After_type -> "after_type"
  in
  let explicit_register =
    match qualifier.explicit_register with
    | None -> "none"
    | Some register -> register.spelling
  in
  Printf.sprintf "%s:%s:%s" kind position explicit_register

let staged_function_mask modifiers =
  List.fold_left
    (fun mask (modifier : Ast.declaration_modifier) ->
      Function_flag.apply_modifier ~mask (function_flag_modifier modifier.kind))
    0L modifiers

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

let function_calling_modifier_source_behavior () =
  let parser_source = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains parser_source fragment))
    [
      ("interrupt keyword branch", "case KW_INTERRUPT:");
      ( "interrupt also prevents argument popping",
        "fsp_flags=FSF_INTERRUPT|FSF_NOARGPOP|" );
      ("error-code keyword branch", "case KW_HASERRCODE:");
      ("argument-pop keyword branch", "case KW_ARGPOP:");
      ("no-argument-pop keyword branch", "case KW_NOARGPOP:");
      ( "function join transfers the calling group",
        "tmpf->flags|=fsp_flags&FSG_FUN_FLAGS1;" );
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains compiler_header fragment))
    [
      ("interrupt keyword identity", "#define KW_INTERRUPT\t44");
      ("error-code keyword identity", "#define KW_HASERRCODE\t45");
      ("argument-pop keyword identity", "#define KW_ARGPOP\t46");
      ("no-argument-pop keyword identity", "#define KW_NOARGPOP\t47");
      ( "stored calling group identity",
        "#define FSG_FUN_FLAGS1 \
         (FSF_INTERRUPT|FSF_HASERRCODE|FSF_ARGPOP|FSF_NOARGPOP)" );
    ];
  let kernel_header = pinned "Kernel/KernelA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains kernel_header fragment))
    [
      ("stored interrupt bit", "#define Ff_INTERRUPT\t\t8");
      ("stored error-code bit", "#define Ff_HASERRCODE\t\t9");
      ("stored argument-pop bit", "#define Ff_ARGPOP\t\t10");
      ("stored no-argument-pop bit", "#define Ff_NOARGPOP\t\t11");
    ];
  let kernel_api = pinned "Kernel/KernelC.HH" in
  Alcotest.(check bool)
    "bound argument-pop prototype" true
    (contains kernel_api "public argpop extern I64 CallStkGrow(");
  let language_doc = pinned "Doc/HolyC.DD" in
  Alcotest.(check bool)
    "language documentation names function flags" true
    (contains language_doc
       "$FG,2$interrupt$FG$, $FG,2$haserrcode$FG$, $FG,2$public$FG$, \
        $FG,2$argpop$FG$ or $FG,2$noargpop$FG$ are function flags")

let direct_function_calling_modifiers () =
  let source =
    "interrupt extern U0 InterruptOnly();\n\
     haserrcode extern U0 ErrorCode();\n\
     argpop extern I64 Pop(I64 value);\n\
     noargpop _extern _NO_POP U0 NoPop();\n\
     public argpop import I64 Imported(I64 value);"
  in
  let _, _, output = parse_string ~compilation_mode:Preprocessor.Aot source in
  let parsed = expect_ast output |> prototypes in
  Alcotest.(check (list (list string)))
    "calling modifier order"
    [
      [ "interrupt" ];
      [ "haserrcode" ];
      [ "argpop" ];
      [ "noargpop" ];
      [ "public"; "argpop" ];
    ]
    (List.map
       (fun (prototype : Ast.function_prototype) ->
         List.map
           (fun (modifier : Ast.declaration_modifier) ->
             modifier_name modifier.kind)
           prototype.modifiers)
       parsed);
  Alcotest.(check string)
    "AOT import remains a function" "Imported" (List.nth parsed 4).name.spelling;
  let _, _, global_output = parse_string "argpop I64 StagedGlobal;" in
  let global = expect_ast global_output |> expect_one_global in
  Alcotest.(check (list string))
    "calling prefix remains visible on a global syntax node" [ "argpop" ]
    (List.map
       (fun (modifier : Ast.declaration_modifier) ->
         modifier_name modifier.kind)
       global.modifiers)

let function_calling_modifier_transitions () =
  let source =
    "interrupt extern U0 InterruptOnly();\n\
     public interrupt haserrcode argpop noargpop extern U0 AllFlags();\n\
     interrupt static public argpop extern U0 ResetFlags();"
  in
  let _, _, output = parse_string source in
  match expect_ast output |> prototypes with
  | [ interrupt_only; all_flags; reset_flags ] ->
      Alcotest.(check int64)
        "interrupt adds noargpop" 0x900L
        (staged_function_mask interrupt_only.modifiers);
      Alcotest.(check int64)
        "all stored calling flags and public survive" 0xF01L
        (staged_function_mask all_flags.modifiers);
      Alcotest.(check int64)
        "static clears earlier calling flags" 0x401L
        (staged_function_mask reset_flags.modifiers)
  | items ->
      Alcotest.failf "expected three flagged prototypes, got %d"
        (List.length items)

let definition_backed_function_calling_modifier () =
  let source = "#define CALLING interrupt\nCALLING extern U0 Generated();" in
  let session, root, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  let modifier = List.hd prototype.modifiers in
  Alcotest.(check string)
    "generated calling modifier" "interrupt"
    (modifier_name modifier.kind);
  Alcotest.(check bool)
    "calling modifier uses generated source" false
    (Source_id.equal modifier.location.span.source (Source_file.id root));
  Alcotest.(check bool)
    "calling modifier retains its invocation" true
    (Option.is_some modifier.location.generated_from);
  Alcotest.(check bool)
    "calling modifier retains its definition" true
    (Option.is_some modifier.location.defined_at);
  let open Yojson.Safe.Util in
  let modifier_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "modifiers" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "JSON calling modifier kind" "interrupt"
    (modifier_json |> member "kind" |> to_string)

let function_calling_modifier_visibility () =
  let source =
    "argpop extern U0 Visible();\n\
     #ifdef Visible\n\
     U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).items with
  | [ Ast.Function_prototype prototype; Ast.Global_variable variable ] ->
      Alcotest.(check string)
        "published flagged function" "Visible" prototype.name.spelling;
      Alcotest.(check string)
        "selected conditional branch" "selected" variable.name.spelling
  | items ->
      Alcotest.failf "expected a flagged function and global, got %d items"
        (List.length items)

let function_calling_modifier_failures () =
  List.iter
    (fun (description, source, name, code) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      Alcotest.(check string)
        (description ^ " diagnostic")
        code (first_diagnostic output).code;
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected flagged function %s became visible" name)
    [
      ( "unbound interrupt function",
        "interrupt U0 Unbound();",
        "Unbound",
        "HCPARSE0008" );
      ( "flagged JIT import",
        "noargpop import U0 Blocked();",
        "Blocked",
        "HCPARSE0006" );
    ]

let function_parameter_register_source_behavior () =
  let variable_parser = pinned "Compiler/PrsVar.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains variable_parser fragment))
    [
      ("register keyword branch", "case KW_REG:");
      ("automatic register request", "_reg=REG_ALLOC;");
      ("checked U64 register lookup", "DefineMatch(cc->cur_str,\"ST_U64_REGS\")");
      ("no-register keyword branch", "case KW_NOREG:");
      ("stack register request", "_reg=REG_NONE;");
      ("prefix request reaches a member", "tmpm=MemberLstNew(_reg);");
      ("variadic request forwarding", "PrsDotDotDot(cc,tmpc,_reg);");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains compiler_header fragment))
    [
      ("reg keyword identity", "#define KW_REG\t\t35");
      ("noreg keyword identity", "#define KW_NOREG\t36");
      ("function argument parser mode", "#define PRS1_FUN_ARG\t\t0x000200");
    ];
  let compiler_init = pinned "Compiler/CInit.HC" in
  Alcotest.(check bool)
    "U64 register list" true
    (contains compiler_init
       "DefineLstLoad(\"ST_U64_REGS\",\"RAX\\0RCX\\0RDX\\0RBX\\0RSP\\0RBP\\0RSI\\0RDI\\0\"");
  Alcotest.(check bool)
    "high U64 register list" true
    (contains compiler_init "\"R8\\0R9\\0R10\\0R11\\0R12\\0R13\\0R14\\0R15\\0\"");
  let kernel_header = pinned "Kernel/KernelA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains kernel_header fragment))
    [
      ("noreg stored value", "#define REG_NONE\t32");
      ("reg stored value", "#define REG_ALLOC\t33");
      ("unspecified stored value", "#define REG_UNDEF\tI8_MIN");
    ];
  let language_doc = pinned "Doc/HolyC.DD" in
  Alcotest.(check bool)
    "language register qualifier description" true
    (contains language_doc
       "$FG,2$noreg$FG$ or $FG,2$reg$FG$ can be placed before a function local \
        var name");
  let option_doc = pinned "Doc/Options.DD" in
  Alcotest.(check bool)
    "register allocation option boundary" true
    (contains option_doc
       "OPTf_NO_REG_VAR\",A=\"MN:OPTf_NO_REG_VAR\"$ forces all function local \
        vars to the stk not regs")

let direct_function_parameter_register_qualifiers () =
  let source =
    "extern U0 Qualified(reg I64 before,noreg U8 *stack,\n\
     I64 reg R15 exact,U8 noreg *pointer,reg RAX U16,\n\
     I64 reg EAX,reg R14 noreg I32 reg R13 last);\n\
     extern U0 Varargs(reg R12 noreg ...);"
  in
  let _, _, output = parse_string source in
  match expect_ast output |> prototypes with
  | [ qualified; varargs ] ->
      Alcotest.(check (list (list string)))
        "ordered register qualifiers"
        [
          [ "reg:before_type:none" ];
          [ "noreg:before_type:none" ];
          [ "reg:after_type:R15" ];
          [ "noreg:after_type:none" ];
          [ "reg:before_type:RAX" ];
          [ "reg:after_type:none" ];
          [
            "reg:before_type:R14";
            "noreg:before_type:none";
            "reg:after_type:R13";
          ];
        ]
        (List.map
           (fun (parameter : Ast.function_parameter) ->
             List.map register_qualifier_signature parameter.register_qualifiers)
           qualified.parameters);
      Alcotest.(check (list (option string)))
        "parameter names preserve register ambiguity"
        [
          Some "before";
          Some "stack";
          Some "exact";
          Some "pointer";
          None;
          Some "EAX";
          Some "last";
        ]
        (List.map
           (fun (parameter : Ast.function_parameter) ->
             Option.map
               (fun (name : Ast.identifier) -> name.spelling)
               parameter.name)
           qualified.parameters);
      Alcotest.(check int)
        "qualified pointer depth" 1
        (List.length (List.nth qualified.parameters 3).pointer_layers);
      let variadic = Option.get varargs.variadic in
      Alcotest.(check (list string))
        "qualified variadic marker"
        [ "reg:before_type:R12"; "noreg:before_type:none" ]
        (List.map register_qualifier_signature variadic.register_qualifiers)
  | items ->
      Alcotest.failf "expected two register-qualified prototypes, got %d"
        (List.length items)

let every_explicit_u64_parameter_register () =
  let expected =
    [
      "RAX";
      "RCX";
      "RDX";
      "RBX";
      "RSP";
      "RBP";
      "RSI";
      "RDI";
      "R8";
      "R9";
      "R10";
      "R11";
      "R12";
      "R13";
      "R14";
      "R15";
    ]
  in
  let parameters =
    List.mapi
      (fun index register -> Printf.sprintf "I64 reg %s value%d" register index)
      expected
    |> String.concat ","
  in
  let _, _, output = parse_string ("extern U0 Every(" ^ parameters ^ ");") in
  let prototype = expect_ast output |> expect_one_prototype in
  let actual =
    List.map
      (fun (parameter : Ast.function_parameter) ->
        match parameter.register_qualifiers with
        | [ qualifier ] -> (
            match qualifier.explicit_register with
            | Some register -> register.spelling
            | None -> Alcotest.fail "expected an explicit U64 register")
        | qualifiers ->
            Alcotest.failf "expected one register qualifier, got %d"
              (List.length qualifiers))
      prototype.parameters
  in
  Alcotest.(check (list string)) "canonical U64 register names" expected actual

let definition_backed_parameter_register_qualifier () =
  let source =
    "#define QUAL reg\n\
     #define FIXED R15\n\
     extern U0 Generated(I64 QUAL FIXED value);"
  in
  let session, root, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  let parameter = List.hd prototype.parameters in
  let qualifier = List.hd parameter.register_qualifiers in
  let register = Option.get qualifier.explicit_register in
  List.iter
    (fun ((description, location) : string * Ast.location) ->
      Alcotest.(check bool)
        (description ^ " uses generated source")
        false
        (Source_id.equal location.span.source (Source_file.id root));
      Alcotest.(check bool)
        (description ^ " retains invocation")
        true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        (description ^ " retains definition")
        true
        (Option.is_some location.defined_at))
    [
      ("register qualifier", qualifier.location);
      ("explicit register", register.location);
    ];
  let open Yojson.Safe.Util in
  let qualifier_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "parameters" |> to_list |> List.hd
    |> member "register_qualifiers"
    |> to_list |> List.hd
  in
  Alcotest.(check string)
    "JSON explicit register" "R15"
    (qualifier_json |> member "explicit_register" |> member "spelling"
   |> to_string)

let function_parameter_register_visibility () =
  let source =
    "extern U0 Visible(I64 reg R15 value);\n\
     #ifdef Visible\n\
     U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).items with
  | [ Ast.Function_prototype prototype; Ast.Global_variable variable ] ->
      Alcotest.(check string)
        "published qualified function" "Visible" prototype.name.spelling;
      Alcotest.(check string)
        "selected conditional branch" "selected" variable.name.spelling
  | items ->
      Alcotest.failf "expected a qualified function and global, got %d items"
        (List.length items)

let function_parameter_register_failures () =
  List.iter
    (fun (description, source, name, code, message_fragment) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (description ^ " diagnostic") code diagnostic.code;
      Alcotest.(check bool)
        (description ^ " message") true
        (contains diagnostic.message message_fragment);
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected qualified function %s became visible" name)
    [
      ( "qualifier after pointer stars",
        "extern U0 Misplaced(I64 *reg value);",
        "Misplaced",
        "HCPARSE0013",
        "must appear before" );
      ( "qualifier without a type",
        "extern U0 Missing(reg R15);",
        "Missing",
        "HCPARSE0009",
        "after register qualifier" );
      ( "noreg does not consume a register",
        "extern U0 NoRegValue(noreg R15 I64 value);",
        "NoRegValue",
        "HCPARSE0009",
        "parameter type" );
      ( "non-U64 register remains the name",
        "extern U0 Narrow(I64 reg EAX value);",
        "Narrow",
        "HCPARSE0010",
        "expected ',' or ')'" );
    ]

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
    | Ast.Intern -> "intern"
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
        (binding.target = Ast.No_binding_target);
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
        (expect_symbol_binding_target c32_binding).spelling;
      Alcotest.(check int)
        "target starts after the binding" 8
        (expect_symbol_binding_target c32_binding).location.span.start;
      Alcotest.(check (list string))
        "group keeps its modifier" [ "public" ]
        (List.map
           (fun (modifier : Ast.declaration_modifier) -> modifier.spelling)
           boot.modifiers);
      Alcotest.(check string)
        "group target" "SYS_BOOT_BASE"
        ( boot.binding |> Option.get |> fun binding ->
          (expect_symbol_binding_target binding).spelling );
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
    (expect_symbol_binding_target binding).spelling

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
  let target = expect_symbol_binding_target binding in
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
    ]

let intern_binding_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("_intern keyword branch", "case KW__INTERN:");
      ("target uses integer expression evaluation", "i=LexExpressionI64(cc);");
      ( "internal declaration mode",
        "PrsGlblVarLst(cc,PRS0__INTERN|PRS1_NULL,tmpex,i,fsp_flags);" );
      ("internal function branch", "case PRS0__INTERN:");
      ("target becomes the function address", "tmpf->exe_addr=val;");
      ("internal function flag is set", "Bts(&tmpf->flags,Ff_INTERNAL);");
      ("extern function flag is cleared", "LBtr(&tmpf->flags,Cf_EXTERN);");
    ];
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  Alcotest.(check bool)
    "LexExpressionI64 evaluates and converts F64" true
    (contains expression_parser "res=ToI64(res(F64));");
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains compiler_header fragment))
    [
      ("_intern keyword identity", "#define KW__INTERN\t14");
      ("internal parser mode", "#define PRS0__INTERN\t\t0x000002");
    ];
  let kernel_header = pinned "Kernel/KernelA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains kernel_header fragment))
    [
      ("extern flag identity", "#define Cf_EXTERN\t\t0");
      ("internal flag identity", "#define Ff_INTERNAL\t\t12");
    ];
  let kernel_api = pinned "Kernel/KernelB.HH" in
  let intern_declarations =
    kernel_api |> String.split_on_char '\n'
    |> List.filter (fun line -> contains line "_intern")
  in
  Alcotest.(check int)
    "pinned _intern declaration count" 60
    (List.length intern_declarations);
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains kernel_api fragment))
    [
      ("bit scan prototype", "public _intern IC_BSF I64 Bsf(");
      ("bit test prototype", "public _intern IC_BT Bool Bt(");
      ("floating prototype", "public _intern IC_ATAN F64 ATan(F64 d);");
      ("parameterless prototype", "public _intern IC_RDTSC I64 GetTSC();");
    ]

let pinned_intern_bindings () =
  let source =
    "public _intern IC_BSF I64 Bsf(I64 bit_field_val);\n\
     public _intern IC_BT Bool Bt(U8 *bit_field,I64 bit);\n\
     public _intern IC_ATAN F64 ATan(F64 d);\n\
     public _intern IC_RDTSC I64 GetTSC();"
  in
  let _, _, output = parse_string source in
  let functions = expect_ast output |> prototypes in
  Alcotest.(check (list string))
    "pinned internal function names"
    [ "Bsf"; "Bt"; "ATan"; "GetTSC" ]
    (List.map
       (fun (prototype : Ast.function_prototype) -> prototype.name.spelling)
       functions);
  Alcotest.(check (list string))
    "pinned internal target expressions"
    [ "IC_BSF"; "IC_BT"; "IC_ATAN"; "IC_RDTSC" ]
    (List.map
       (fun (prototype : Ast.function_prototype) ->
         match expect_expression_binding_target prototype.binding with
         | Ast.Identifier_expression identifier -> identifier.spelling
         | _ -> Alcotest.fail "expected a pinned identifier target")
       functions);
  let bit_test = List.nth functions 1 in
  Alcotest.(check int)
    "bit test parameter count" 2
    (List.length bit_test.parameters);
  Alcotest.(check int)
    "bit field parameter pointer depth" 1
    (List.length (List.hd bit_test.parameters).pointer_layers);
  let get_tsc = List.nth functions 3 in
  Alcotest.(check int)
    "parameterless internal function" 0
    (List.length get_tsc.parameters);
  let _, _, global_output = parse_string "_intern 4096 U64 address;" in
  let global = expect_ast global_output |> expect_one_global in
  Alcotest.(check int64)
    "shared global path retains an integer target" 4096L
    (global.binding |> Option.get |> expect_expression_binding_target
   |> expect_integer_expression);
  let _, _, aot_output =
    parse_string ~compilation_mode:Preprocessor.Aot
      "_intern IC_RDTSC I64 AotClock();"
  in
  ignore (expect_ast aot_output)

let intern_target_expression_registry () =
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let source =
        Printf.sprintf "_intern 1%s2 I64 Internal%d();" operator.spelling index
      in
      let _, _, output = parse_string source in
      let binary =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " internal target operator")
        operator.ic_name binary.binary_operator_spec.ic_name)
    Operator.binary_operators;
  let _, _, output = parse_string "_intern -(IC_BSF+1) I64 Adjusted();" in
  let prototype = expect_ast output |> expect_one_prototype in
  let prefix =
    prototype.binding |> expect_expression_binding_target
    |> expect_prefix_expression
  in
  Alcotest.(check string)
    "prefix target operator" "-" prefix.prefix_operator.operator_spelling;
  (match prefix.prefix_operand with
  | Ast.Parenthesized_expression grouped ->
      ignore (expect_binary_expression grouped.grouped_expression)
  | _ -> Alcotest.fail "expected a grouped internal target operand");
  Alcotest.(check bool)
    "the following type remains the function return type" true
    (Primitive_type.equal Primitive_type.I64 prototype.return_type.primitive)

let intern_binding_provenance () =
  let session, root, output =
    parse_string
      "#define LINK _intern\n\
       #define TARGET (IC_BSF+1)\n\
       LINK TARGET I64 Generated();"
  in
  let prototype = expect_ast output |> expect_one_prototype in
  let binding = prototype.binding in
  let target = expect_expression_binding_target binding in
  Alcotest.(check bool)
    "generated binding leaves the root source" false
    (Source_id.equal binding.location.span.source (Source_file.id root));
  List.iter
    (fun ((description, location) : string * Ast.location) ->
      Alcotest.(check bool)
        (description ^ " keeps invocation provenance")
        true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        (description ^ " keeps definition provenance")
        true
        (Option.is_some location.defined_at))
    [
      ("binding", binding.location); ("target", Ast.expression_location target);
    ];
  let open Yojson.Safe.Util in
  let target_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check string)
    "JSON target expression kind" "parenthesized"
    (target_json |> member "kind" |> to_string);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "internal.HC" in
      write_file root_file "#include \"internal\"";
      write_file declaration_file "_intern IC_BSF I64 Included();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let expression =
        expect_ast include_output |> expect_one_prototype |> fun included ->
        expect_expression_binding_target included.binding
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          (Ast.expression_location expression).span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included target keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let intern_binding_visibility () =
  let source =
    "_intern IC_BSF I64 Declared();\n\
     #ifdef Declared\n\
     public U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).items with
  | [ Ast.Function_prototype declared; Ast.Global_variable selected ] ->
      Alcotest.(check string)
        "published internal function" "Declared" declared.name.spelling;
      Alcotest.(check string)
        "conditional selected global" "selected" selected.name.spelling
  | items ->
      Alcotest.failf "expected an internal function and selected global, got %d"
        (List.length items)

let intern_binding_failures () =
  List.iter
    (fun (description, source, rejected_name, code, message_fragment) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (description ^ " code") code diagnostic.code;
      Alcotest.(check bool)
        (description ^ " message") true
        (contains diagnostic.message message_fragment);
      Option.iter
        (fun name ->
          match
            Symbol_visibility.Environment.find_preprocessor
              (Session.symbols session) name
          with
          | Symbol_visibility.Absent -> ()
          | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
              Alcotest.failf "rejected internal name %s became visible" name)
        rejected_name)
    [
      ( "missing target",
        "_intern ;",
        None,
        "HCPARSE0018",
        "_intern target expression operand" );
      ( "missing type",
        "_intern IC_BSF;",
        None,
        "HCPARSE0001",
        "after declaration binding \"_intern\"" );
      ( "unsupported call target",
        "_intern Resolve() I64 Called();",
        Some "Called",
        "HCPARSE0020",
        "target expression continuation" );
      ( "unsupported indexed target",
        "_intern Table[0] I64 Indexed();",
        Some "Indexed",
        "HCPARSE0020",
        "target expression continuation" );
      ( "binding cannot repeat",
        "_intern IC_BSF extern I64 Duplicate();",
        Some "Duplicate",
        "HCPARSE0001",
        "after declaration binding \"_intern\"" );
      ( "missing declarator",
        "_intern IC_BSF I64 ;",
        None,
        "HCPARSE0002",
        "expected an identifier" );
    ];
  let nesting = Parser.max_expression_depth in
  let nested_source =
    Printf.sprintf "_intern %s1%s I64 TooNested();" (String.make nesting '(')
      (String.make nesting ')')
  in
  let nested_session, _, nested_output = parse_string nested_source in
  Alcotest.(check string)
    "internal target nesting diagnostic" "HCPARSE0021"
    (first_diagnostic nested_output).code;
  Alcotest.(check bool)
    "internal target nesting message names its context" true
    (contains (first_diagnostic nested_output).message
       "_intern target expression nesting");
  match
    Symbol_visibility.Environment.find_preprocessor
      (Session.symbols nested_session)
      "TooNested"
  with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "the excessive internal target became visible"

let deterministic_intern_dumps () =
  let session, _, output =
    parse_string "public _intern (IC_BSF+1) I64 Adjusted(I64 value=0);"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human internal dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON internal dump repeats byte for byte" json
    (Ast_dump.json sources ast)

let function_prototype_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("function declarator dispatch", "if (cc->token=='(') {");
      ("ordinary extern joins a header", "PrsFunJoin(cc,tmpc,st,fsp_flags);");
      ( "function arguments use their dedicated parser mode",
        "PrsVarLst(cc,tmpf,PRS0_NULL|PRS1_FUN_ARG);" );
    ];
  let variable_parser = pinned "Compiler/PrsVar.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains variable_parser fragment))
    [
      ( "variadic marker dispatch",
        "cc->token==TK_ELLIPSIS && mode.u8[1]==PRS1B_FUN_ARG" );
      ("function arrays are rejected", "No arrays in fun args at ");
      ( "function commas advance to another type",
        "mode.u8[1]==PRS1B_FUN_ARG && !(mode&PRSF_UNION)" );
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "function-argument mode identity" true
    (contains compiler_header "#define PRS1_FUN_ARG\t\t0x000200");
  let kernel_header = pinned "Kernel/KernelA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains kernel_header fragment))
    [
      ("variadic function flag identity", "#define Ff_DOT_DOT_DOT\t\t14");
      ("ellipsis token identity", "#define TK_ELLIPSIS\t0x124");
    ];
  let kernel_api = pinned "Kernel/KernelB.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains kernel_api fragment))
    [
      ( "bound pointer parameter prototype",
        "public _extern _CALL I64 Call(U8 *machine_code);" );
      ("bound parameterless prototype", "public _extern _SYS_HLT U0 SysHlt();");
    ];
  let compiler_api = pinned "Compiler/CExts.HC" in
  Alcotest.(check bool)
    "ordinary parameterless prototype" true
    (contains compiler_api "extern U8 *CmdLinePmt();")

let function_pointer_parameter_source_behavior () =
  let variable_parser = pinned "Compiler/PrsVar.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains variable_parser fragment))
    [
      ("function-pointer declarator branch", "if (cc->token=='(') {");
      ("function-pointer requires a star", "if (Lex(cc)!='*')");
      ("function-pointer starts at one star", "ptr_stars_cnt=1; //fun_ptr");
      ("function-pointer accepts more stars", "while (Lex(cc)=='*')");
      ( "function-pointer joins a nested signature",
        "fun_ptr=PrsFunJoin(cc,tmpc1,NULL,fsp_flags)+ptr_stars_cnt;" );
      ("function-pointer metadata reaches members", "tmpm->flags|=MLF_FUN;");
    ];
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  Alcotest.(check bool)
    "nested signature reuses the function-argument parser" true
    (contains statement_parser "PrsVarLst(cc,tmpf,PRS0_NULL|PRS1_FUN_ARG);");
  let kernel_header = pinned "Kernel/KernelA.HH" in
  Alcotest.(check bool)
    "function-pointer depth uses the shared star limit" true
    (contains kernel_header "#define PTR_STARS_NUM\t4");
  let kernel_api = pinned "Kernel/KernelB.HH" in
  Alcotest.(check bool)
    "pinned API contains a variadic double function pointer" true
    (contains kernel_api "I64 (**ext)(...);");
  let kernel_public_api = pinned "Kernel/KernelC.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains kernel_public_api fragment))
    [
      ("FarCall32 callback", "Bool FarCall32(U0 (*fp_addr)());");
      ("pointer-returning callback", "U8  *(*fp_getstr2)(I64 flags=0);");
    ];
  let quicksort = pinned "Kernel/QSort.HC" in
  Alcotest.(check bool)
    "quicksort comparator callback" true
    (contains quicksort "I64 (*fp_compare)(I64 e1,I64 e2)")

let pinned_function_pointer_prototypes () =
  let source =
    "public _extern _FAR_CALL32 Bool FarCall32(U0 (*fp_addr)());\n\
     public argpop extern I64 CallStkGrow(I64 stk_size_threshold,I64 stk_size,\n\
     I64 (*fp_addr)(...),...);\n\
     extern U0 QSortI64(I64 *base,I64 num,\n\
     I64 (*fp_compare)(I64 e1,I64 e2));"
  in
  let _, _, output = parse_string source in
  match expect_ast output |> prototypes with
  | [ far_call; stack_grow; quicksort ] ->
      Alcotest.(check string)
        "FarCall32 alternate target" "_FAR_CALL32"
        (expect_symbol_binding_target far_call.binding).spelling;
      let far_address =
        far_call.parameters |> List.hd |> expect_function_pointer
      in
      Alcotest.(check int)
        "FarCall32 callback has an empty signature" 0
        (List.length far_address.signature_parameters);
      Alcotest.(check bool)
        "CallStkGrow is variadic" true
        (Option.is_some stack_grow.variadic);
      Alcotest.(check int)
        "CallStkGrow fixed parameter count" 3
        (List.length stack_grow.parameters);
      let grow_address =
        List.nth stack_grow.parameters 2 |> expect_function_pointer
      in
      Alcotest.(check bool)
        "CallStkGrow callback is variadic" true
        (Option.is_some grow_address.signature_variadic);
      let compare =
        List.nth quicksort.parameters 2 |> expect_function_pointer
      in
      Alcotest.(check int)
        "QSort comparator arity" 2
        (List.length compare.signature_parameters)
  | items ->
      Alcotest.failf "expected three pinned callback prototypes, got %d"
        (List.length items)

let direct_function_pointer_parameters () =
  let source =
    "extern U0 Callbacks(I64 (*one)(),I64 (**two)(),I64 (***three)(),\n\
     I64 (****four)(),U8 *(*get)(I64 flags),I64 (*)(I64 value),\n\
     I64 (**dispatch)(...),\n\
     I64 (*outer)(I64 (*inner)(reg R12 noreg U8 **value)));"
  in
  let session, _, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  Alcotest.(check int)
    "function-pointer parameter count" 8
    (List.length prototype.parameters);
  let pointers = List.map expect_function_pointer prototype.parameters in
  Alcotest.(check (list int))
    "declarator pointer depths" [ 1; 2; 3; 4; 1; 1; 2; 1 ]
    (List.map
       (fun (pointer : Ast.function_pointer_declarator) ->
         List.length pointer.indirection_layers)
       pointers);
  Alcotest.(check (list int))
    "return pointer depths stay separate" [ 0; 0; 0; 0; 1; 0; 0; 0 ]
    (List.map
       (fun (parameter : Ast.function_parameter) ->
         List.length parameter.pointer_layers)
       prototype.parameters);
  Alcotest.(check (list (option string)))
    "function-pointer names"
    [
      Some "one";
      Some "two";
      Some "three";
      Some "four";
      Some "get";
      None;
      Some "dispatch";
      Some "outer";
    ]
    (List.map
       (fun (parameter : Ast.function_parameter) ->
         Option.map
           (fun (name : Ast.identifier) -> name.spelling)
           parameter.name)
       prototype.parameters);
  let get = List.nth pointers 4 in
  Alcotest.(check string)
    "pointer-returning signature parameter" "flags"
    ( get.signature_parameters |> List.hd |> fun parameter ->
      parameter.name |> Option.get |> fun name -> name.spelling );
  let unnamed = List.nth pointers 5 in
  Alcotest.(check int)
    "unnamed callback signature parameter count" 1
    (List.length unnamed.signature_parameters);
  let dispatch = List.nth pointers 6 in
  Alcotest.(check bool)
    "variadic callback marker" true
    (Option.is_some dispatch.signature_variadic);
  let outer = List.nth pointers 7 in
  let inner_parameter = List.hd outer.signature_parameters in
  Alcotest.(check string)
    "nested callback name" "inner"
    (inner_parameter.name |> Option.get |> fun name -> name.spelling);
  let inner = expect_function_pointer inner_parameter in
  let value = List.hd inner.signature_parameters in
  Alcotest.(check int)
    "nested callback parameter pointer depth" 2
    (List.length value.pointer_layers);
  Alcotest.(check (list string))
    "nested callback register qualifiers"
    [ "reg:before_type:R12"; "noreg:before_type:none" ]
    (List.map register_qualifier_signature value.register_qualifiers);
  List.iter
    (fun (pointer : Ast.function_pointer_declarator) ->
      Alcotest.(check int)
        "declarator opening width" 1
        (pointer.declarator_opening_parenthesis.span.stop
       - pointer.declarator_opening_parenthesis.span.start);
      Alcotest.(check int)
        "declarator closing width" 1
        (pointer.declarator_closing_parenthesis.span.stop
       - pointer.declarator_closing_parenthesis.span.start);
      Alcotest.(check int)
        "signature opening width" 1
        (pointer.signature_opening_parenthesis.span.stop
       - pointer.signature_opening_parenthesis.span.start);
      Alcotest.(check int)
        "signature closing width" 1
        (pointer.signature_closing_parenthesis.span.stop
       - pointer.signature_closing_parenthesis.span.start);
      Alcotest.(check int)
        "function-pointer span starts at declarator"
        pointer.declarator_opening_parenthesis.span.start
        pointer.function_pointer_location.span.start;
      Alcotest.(check int)
        "function-pointer span ends at signature"
        pointer.signature_closing_parenthesis.span.stop
        pointer.function_pointer_location.span.stop)
    pointers;
  let open Yojson.Safe.Util in
  let json_pointer =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "parameters" |> to_list |> List.hd |> member "function_pointer"
  in
  Alcotest.(check string)
    "JSON function-pointer kind" "function_pointer"
    (json_pointer |> member "kind" |> to_string);
  Alcotest.(check int)
    "JSON function-pointer depth" 1
    (json_pointer |> member "pointer_layers" |> to_list |> List.length)

let definition_backed_function_pointer_parameter () =
  let source =
    "#define LP (\n\
     #define STAR *\n\
     #define CALLBACK callback\n\
     #define RP )\n\
     extern U0 Generated(I64 LP STAR CALLBACK RP LP I64 value RP);"
  in
  let session, root, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  let parameter = List.hd prototype.parameters in
  let pointer = expect_function_pointer parameter in
  let name = Option.get parameter.name in
  let star = List.hd pointer.indirection_layers in
  List.iter
    (fun ((description, location) : string * Ast.location) ->
      Alcotest.(check bool)
        (description ^ " uses generated source")
        false
        (Source_id.equal location.span.source (Source_file.id root));
      Alcotest.(check bool)
        (description ^ " retains its invocation")
        true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        (description ^ " retains its definition")
        true
        (Option.is_some location.defined_at))
    [
      ("declarator opening", pointer.declarator_opening_parenthesis);
      ("declarator star", star.location);
      ("function-pointer name", name.location);
      ("declarator closing", pointer.declarator_closing_parenthesis);
      ("signature opening", pointer.signature_opening_parenthesis);
      ("signature closing", pointer.signature_closing_parenthesis);
    ];
  let open Yojson.Safe.Util in
  let json_pointer =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "parameters" |> to_list |> List.hd |> member "function_pointer"
  in
  Alcotest.(check bool)
    "JSON keeps generated declarator provenance" true
    (json_pointer
    |> member "opening_parenthesis"
    |> member "generated_from" <> `Null)

let function_pointer_streaming_visibility () =
  let source =
    "extern U0 Callback(I64 (*callback)());\n\
     #ifdef Callback\n\
     U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).items with
  | [ Ast.Function_prototype prototype; Ast.Global_variable variable ] ->
      Alcotest.(check string)
        "published callback prototype" "Callback" prototype.name.spelling;
      Alcotest.(check string)
        "selected conditional branch" "selected" variable.name.spelling
  | items ->
      Alcotest.failf "expected a callback prototype and global, got %d items"
        (List.length items)

let function_pointer_parameter_failures () =
  List.iter
    (fun (description, source, name, code, message_fragment) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (description ^ " code") code diagnostic.code;
      Alcotest.(check bool)
        (description ^ " message") true
        (contains diagnostic.message message_fragment);
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected callback prototype %s became visible" name)
    [
      ( "missing declarator star",
        "extern U0 MissingStar(I64 (callback)());",
        "MissingStar",
        "HCPARSE0014",
        "expected '*' after '('" );
      ( "missing declarator closing parenthesis",
        "extern U0 MissingDeclaratorClose(I64 (*callback());",
        "MissingDeclaratorClose",
        "HCPARSE0014",
        "expected ')' after function-pointer name" );
      ( "missing signature opening parenthesis",
        "extern U0 MissingSignatureOpen(I64 (*callback));",
        "MissingSignatureOpen",
        "HCPARSE0014",
        "expected '(' for function-pointer signature" );
      ( "missing nested parameter type",
        "extern U0 MissingNestedType(I64 (*callback)(,));",
        "MissingNestedType",
        "HCPARSE0009",
        "parameter type" );
      ( "missing nested parameter delimiter",
        "extern U0 MissingNestedDelimiter(I64 (*callback)(I64 first U8 \
         second));",
        "MissingNestedDelimiter",
        "HCPARSE0010",
        "expected ',' or ')'" );
      ( "missing signature closing parenthesis",
        "extern U0 MissingSignatureClose(I64 (*callback)(I64 value;",
        "MissingSignatureClose",
        "HCPARSE0010",
        "expected ',' or ')'" );
      ( "function-pointer depth",
        "extern U0 TooDeep(I64 (*****callback)());",
        "TooDeep",
        "HCPARSE0004",
        "at most 4 pointer stars" );
    ];
  let rec nested depth =
    if depth = 0 then "I64 value"
    else Printf.sprintf "I64 (*level%d)(%s)" depth (nested (depth - 1))
  in
  let name = "TooNested" in
  let source = Printf.sprintf "extern U0 %s(%s);" name (nested 33) in
  let session, _, output = parse_string source in
  Alcotest.(check bool)
    "excessive nesting has no AST" true
    (Option.is_none output.ast);
  let diagnostic = first_diagnostic output in
  Alcotest.(check string) "excessive nesting code" "HCPARSE0017" diagnostic.code;
  Alcotest.(check bool)
    "excessive nesting message" true
    (contains diagnostic.message "hosted limit of 32");
  match
    Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
      name
  with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "excessively nested callback prototype became visible"

let direct_function_prototypes () =
  let source =
    "extern U8 *CmdLinePmt();\n\
     public _extern _SYS_HLT U0 SysHlt();\n\
     public _extern _CALL I64 Call(U8 *machine_code);\n\
     extern U8 **Format(I64,U8 **message,...);\n\
     extern U0 Deep(U8 ****value);"
  in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  match ast.items with
  | [
   Ast.Function_prototype cmd_line;
   Ast.Function_prototype sys_hlt;
   Ast.Function_prototype call;
   Ast.Function_prototype format;
   Ast.Function_prototype deep;
  ] ->
      Alcotest.(check string)
        "ordinary name" "CmdLinePmt" cmd_line.name.spelling;
      Alcotest.(check int)
        "ordinary parameter count" 0
        (List.length cmd_line.parameters);
      Alcotest.(check bool)
        "ordinary function is not variadic" false
        (Option.is_some cmd_line.variadic);
      Alcotest.(check int)
        "ordinary return pointer" 1
        (List.length cmd_line.return_pointer_layers);
      Alcotest.(check string)
        "alternate parameterless name" "SysHlt" sys_hlt.name.spelling;
      Alcotest.(check int)
        "alternate parameterless count" 0
        (List.length sys_hlt.parameters);
      Alcotest.(check string)
        "alternate target" "_CALL"
        (expect_symbol_binding_target call.binding).spelling;
      Alcotest.(check (list string))
        "public modifier" [ "public" ]
        (List.map
           (fun (modifier : Ast.declaration_modifier) -> modifier.spelling)
           call.modifiers);
      let call_parameter = List.hd call.parameters in
      Alcotest.(check string)
        "named pointer parameter" "machine_code"
        (call_parameter.name |> Option.get |> fun name -> name.spelling);
      Alcotest.(check int)
        "pointer parameter depth" 1
        (List.length call_parameter.pointer_layers);
      Alcotest.(check int)
        "return pointer depth" 2
        (List.length format.return_pointer_layers);
      Alcotest.(check int)
        "format parameter count" 2
        (List.length format.parameters);
      let unnamed = List.hd format.parameters in
      Alcotest.(check bool)
        "first format parameter is unnamed" true
        (Option.is_none unnamed.name);
      Alcotest.(check string)
        "unnamed parameter delimiter" ","
        (unnamed.delimiter |> Option.get |> fun item -> item.spelling);
      let message = List.nth format.parameters 1 in
      Alcotest.(check int)
        "message pointer depth" 2
        (List.length message.pointer_layers);
      Alcotest.(check string)
        "message delimiter" ","
        (message.delimiter |> Option.get |> fun item -> item.spelling);
      Alcotest.(check string)
        "variadic spelling" "..."
        (format.variadic |> Option.get |> fun item -> item.spelling);
      Alcotest.(check int)
        "prototype covers its semicolon" format.semicolon.span.stop
        format.location.span.stop;
      Alcotest.(check int)
        "four pointer layers are accepted" 4
        ( deep.parameters |> List.hd |> fun parameter ->
          List.length parameter.pointer_layers )
  | items ->
      Alcotest.failf "expected five function prototypes, got %d items"
        (List.length items)

let function_import_mode_boundary () =
  List.iter
    (fun (source, name) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " JIT import has no AST")
        true
        (Option.is_none output.ast);
      Alcotest.(check string)
        (name ^ " JIT diagnostic") "HCPARSE0006" (first_diagnostic output).code;
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected JIT prototype %s became visible" name)
    [
      ("import I64 OrdinaryImport();", "OrdinaryImport");
      ("_import REMOTE_ENTRY I64 AlternateImport();", "AlternateImport");
    ];
  let session, _, output =
    parse_string ~compilation_mode:Preprocessor.Aot
      "import I64 OrdinaryImport();\n\
       _import REMOTE_ENTRY I64 AlternateImport(I64 value);"
  in
  let ast = expect_ast output in
  Alcotest.(check int) "two AOT imports" 2 (List.length ast.items);
  List.iter
    (fun name ->
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Present entry ->
          Alcotest.(check string)
            (name ^ " symbol kind") "function"
            (Symbol_visibility.kind_name (Symbol_visibility.kind entry))
      | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "accepted AOT prototype %s is not visible" name)
    [ "OrdinaryImport"; "AlternateImport" ]

let definition_backed_function_prototype () =
  let source =
    "#define OPEN (\n\
     #define COMMA ,\n\
     #define CLOSE )\n\
     extern U0 Generated OPEN I64 value COMMA U8 * CLOSE;"
  in
  let session, root, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  Alcotest.(check int)
    "two generated parameters" 2
    (List.length prototype.parameters);
  let opening = prototype.opening_parenthesis in
  Alcotest.(check bool)
    "opening parenthesis uses a generated frame" false
    (Source_id.equal opening.span.source (Source_file.id root));
  Alcotest.(check bool)
    "opening parenthesis retains its invocation" true
    (Option.is_some opening.generated_from);
  Alcotest.(check bool)
    "opening parenthesis retains its definition" true
    (Option.is_some opening.defined_at);
  let comma =
    prototype.parameters |> List.hd |> fun parameter ->
    Option.get parameter.delimiter
  in
  Alcotest.(check bool)
    "comma retains its invocation" true
    (Option.is_some comma.location.generated_from);
  Alcotest.(check bool)
    "closing parenthesis retains its definition" true
    (Option.is_some prototype.closing_parenthesis.defined_at);
  let open Yojson.Safe.Util in
  let item =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "JSON prototype kind" "function_prototype"
    (item |> member "kind" |> to_string);
  Alcotest.(check bool)
    "JSON retains generated opening origin" true
    (item |> member "opening_parenthesis" |> member "generated_from" <> `Null)

let function_streaming_visibility () =
  let source =
    "extern I64 Visible();\n\
     #ifdef Visible\n\
     U8 selected;\n\
     #else\n\
     Widget wrong;\n\
     #endif"
  in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  match ast.items with
  | [ Ast.Function_prototype prototype; Ast.Global_variable variable ] ->
      Alcotest.(check string)
        "published function" "Visible" prototype.name.spelling;
      Alcotest.(check string)
        "selected branch" "selected" variable.name.spelling
  | items ->
      Alcotest.failf "expected a prototype and selected global, got %d items"
        (List.length items)

let default_parameter_expression_source_behavior () =
  let variable_parser = pinned "Compiler/PrsVar.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains variable_parser fragment))
    [
      ("default begins at assignment", "if (cc->token=='=') {");
      ( "default uses the expression compiler",
        "machine_code=LexExpression2Bin(cc,&type);" );
      ("default executes during parsing", "tmpm->dft_val=Call(machine_code);");
      ("default availability is recorded", "tmpm->flags|=MLF_DFT_AVAILABLE;");
    ];
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  Alcotest.(check bool)
    "power participates in the unary-minus exception" true
    (contains expression_parser "cur_op.u16[0]==IC_POWER");
  Alcotest.(check bool)
    "unary minus participates in the power exception" true
    (contains expression_parser "stk_op.u16[0]==IC_UNARY_MINUS");
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " precedence is pinned")
        true
        (contains compiler_header ("#define " ^ name)))
    [ "PREC_EXP"; "PREC_MUL"; "PREC_AND"; "PREC_ADD"; "PREC_ASSIGN" ];
  let cinit_source = pinned "Compiler/CInit.HC" in
  Alcotest.(check bool)
    "shift and exponentiation share a precedence" true
    (contains cinit_source "d[TK_SHL]=(PREC_EXP+ASSOCF_LEFT)<<16+IC_SHL")

let default_expression_atoms () =
  let source =
    "extern U0 Defaults(I64 integer=0,F64 floating=1.5,\n\
     I64 characters='AB',U8 *text=\"ok\\n\",I64 symbol=NULL,\n\
     I64 position=$$,I64 plain);"
  in
  let session, _, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  Alcotest.(check (list bool))
    "defaults may precede a plain parameter"
    [ true; true; true; true; true; true; false ]
    (List.map
       (fun (parameter : Ast.function_parameter) ->
         Option.is_some parameter.default)
       prototype.parameters);
  let defaults =
    prototype.parameters
    |> List.filter_map (fun (parameter : Ast.function_parameter) ->
        parameter.default)
  in
  (match (List.nth defaults 0).value with
  | Ast.Integer_literal literal ->
      Alcotest.(check string) "integer spelling" "0" literal.literal_spelling;
      Alcotest.(check int64)
        "explicit zero value" 0L
        (match literal.literal_value with
        | Ast.Integer_value value -> value
        | _ -> Alcotest.fail "integer literal has the wrong value kind")
  | _ -> Alcotest.fail "expected an integer default");
  (match (List.nth defaults 1).value with
  | Ast.Float_literal literal ->
      Alcotest.(check (float 0.))
        "floating value" 1.5
        (match literal.literal_value with
        | Ast.Float_value value -> value
        | _ -> Alcotest.fail "floating literal has the wrong value kind")
  | _ -> Alcotest.fail "expected a floating default");
  (match (List.nth defaults 2).value with
  | Ast.Character_literal literal ->
      Alcotest.(check int64)
        "multi-character value" 0x4241L
        (match literal.literal_value with
        | Ast.Integer_value value -> value
        | _ -> Alcotest.fail "character literal has the wrong value kind")
  | _ -> Alcotest.fail "expected a character default");
  (match (List.nth defaults 3).value with
  | Ast.String_literal literal ->
      Alcotest.(check string)
        "decoded string bytes" "ok\n"
        (match literal.literal_value with
        | Ast.Bytes_value value -> value
        | _ -> Alcotest.fail "string literal has the wrong value kind")
  | _ -> Alcotest.fail "expected a string default");
  (match (List.nth defaults 4).value with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string) "identifier spelling" "NULL" identifier.spelling
  | _ -> Alcotest.fail "expected an identifier default");
  (match (List.nth defaults 5).value with
  | Ast.Current_position_expression operator ->
      Alcotest.(check string)
        "current-position spelling" "$$" operator.operator_spelling
  | _ -> Alcotest.fail "expected a current-position default");
  List.iter
    (fun (default : Ast.parameter_default) ->
      Alcotest.(check int)
        "assignment token width" 1
        (default.equals.span.stop - default.equals.span.start))
    defaults;
  let open Yojson.Safe.Util in
  let first_default =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "parameters" |> to_list |> List.hd |> member "default"
  in
  Alcotest.(check string)
    "JSON atom kind" "integer_literal"
    (first_default |> member "value" |> member "kind" |> to_string)

let default_expression_prefixes () =
  let cases =
    [
      ("+", "value", Ast.Unary_plus);
      ("-", "value", Ast.Unary_minus);
      ("!", "value", Ast.Logical_not);
      ("~", "value", Ast.Bitwise_not);
      ("*", "pointer", Ast.Dereference);
      ("&", "value", Ast.Address_of);
      ("++", "value", Ast.Pre_increment);
      ("--", "value", Ast.Pre_decrement);
    ]
  in
  List.iteri
    (fun index (spelling, operand, expected_kind) ->
      let source =
        Printf.sprintf "extern U0 Prefix%d(I64 result=%s%s);" index spelling
          operand
      in
      let _, _, output = parse_string source in
      let default =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        List.hd prototype.parameters |> expect_parameter_default
      in
      let prefix = expect_prefix_expression default.value in
      Alcotest.(check bool)
        (spelling ^ " prefix kind")
        true
        (prefix.prefix_operator_kind = expected_kind);
      Alcotest.(check string)
        (spelling ^ " spelling") spelling
        prefix.prefix_operator.operator_spelling)
    cases

let default_expression_binary_registry () =
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let source =
        Printf.sprintf "extern U0 Binary%d(I64 value=1%s2);" index
          operator.spelling
      in
      let _, _, output = parse_string source in
      let binary =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        List.hd prototype.parameters |> expect_parameter_default
        |> fun default -> expect_binary_expression default.value
      in
      Alcotest.(check string)
        (operator.spelling ^ " spelling")
        operator.spelling binary.binary_operator.operator_spelling;
      Alcotest.(check string)
        (operator.spelling ^ " IC")
        operator.ic_name binary.binary_operator_spec.ic_name;
      Alcotest.(check int)
        (operator.spelling ^ " source line")
        operator.source_line binary.binary_operator_spec.source_line)
    Operator.binary_operators

let default_expression_precedence () =
  let representatives =
    Operator.binary_operators
    |> List.sort_uniq (fun left right ->
        Int.compare left.Operator.precedence_value right.precedence_value)
    |> List.sort (fun left right ->
        Int.compare left.Operator.precedence_value right.precedence_value)
  in
  let rec adjacent = function
    | stronger :: (weaker :: _ as rest) ->
        let check_grouping source expected_left expected_right =
          let _, _, output = parse_string source in
          let root =
            expect_ast output |> expect_one_prototype |> fun prototype ->
            List.hd prototype.parameters |> expect_parameter_default
            |> fun default -> expect_binary_expression default.value
          in
          Alcotest.(check string)
            "weaker operator is the root" weaker.Operator.spelling
            root.binary_operator.operator_spelling;
          expected_left root.binary_left;
          expected_right root.binary_right
        in
        check_grouping
          (Printf.sprintf "extern U0 P(I64 v=1%s2%s3);" weaker.spelling
             stronger.spelling)
          (fun _ -> ())
          (fun expression ->
            let nested = expect_binary_expression expression in
            Alcotest.(check string)
              "stronger operator binds on the right" stronger.spelling
              nested.binary_operator.operator_spelling);
        check_grouping
          (Printf.sprintf "extern U0 P(I64 v=1%s2%s3);" stronger.spelling
             weaker.spelling)
          (fun expression ->
            let nested = expect_binary_expression expression in
            Alcotest.(check string)
              "stronger operator binds on the left" stronger.spelling
              nested.binary_operator.operator_spelling)
          (fun _ -> ());
        adjacent rest
    | _ -> ()
  in
  adjacent representatives

let default_expression_associativity () =
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let source =
        Printf.sprintf "extern U0 Assoc%d(I64 value=1%s2%s3);" index
          operator.spelling operator.spelling
      in
      let _, _, output = parse_string source in
      let root =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        List.hd prototype.parameters |> expect_parameter_default
        |> fun default -> expect_binary_expression default.value
      in
      match operator.association with
      | Operator.Right -> ignore (expect_binary_expression root.binary_right)
      | Operator.Left | Operator.Unspecified ->
          ignore (expect_binary_expression root.binary_left))
    Operator.binary_operators

let default_expression_grouping_and_power () =
  let source = "extern U0 Grouped(I64 grouped=(1+2)*3,I64 powered=-2`3`4);" in
  let _, _, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  let grouped =
    List.nth prototype.parameters 0 |> expect_parameter_default
    |> fun default -> expect_binary_expression default.value
  in
  Alcotest.(check string)
    "multiplication remains outside explicit grouping" "*"
    grouped.binary_operator.operator_spelling;
  (match grouped.binary_left with
  | Ast.Parenthesized_expression parenthesized ->
      ignore (expect_binary_expression parenthesized.grouped_expression)
  | _ -> Alcotest.fail "expected explicit grouping on the left");
  let power =
    List.nth prototype.parameters 1 |> expect_parameter_default
    |> fun default -> expect_prefix_expression default.value
  in
  Alcotest.(check bool)
    "unary minus wraps exponentiation" true
    (power.prefix_operator_kind = Ast.Unary_minus);
  let outer_power = expect_binary_expression power.prefix_operand in
  Alcotest.(check string)
    "outer power spelling" "`" outer_power.binary_operator.operator_spelling;
  ignore (expect_binary_expression outer_power.binary_right)

let default_expression_nested_and_provenance () =
  let source =
    "#define VALUE 1+2\n\
     extern U0 Outer(I64 first=VALUE,I64 middle,\n\
     I64 (*callback)(I64 inner=VALUE));"
  in
  let session, root, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  Alcotest.(check bool)
    "a default need not trail" false
    (Option.is_some (List.nth prototype.parameters 1).default);
  let outer_default =
    List.hd prototype.parameters |> expect_parameter_default
  in
  let outer_binary = expect_binary_expression outer_default.value in
  List.iter
    (fun ((description, location) : string * Ast.location) ->
      Alcotest.(check bool)
        (description ^ " uses generated source")
        false
        (Source_id.equal location.span.source (Source_file.id root));
      Alcotest.(check bool)
        (description ^ " keeps invocation provenance")
        true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        (description ^ " keeps definition provenance")
        true
        (Option.is_some location.defined_at))
    [
      ( "generated left operand",
        Ast.expression_location outer_binary.binary_left );
      ("generated operator", outer_binary.binary_operator.operator_location);
      ( "generated right operand",
        Ast.expression_location outer_binary.binary_right );
    ];
  let callback = List.nth prototype.parameters 2 |> expect_function_pointer in
  let inner = List.hd callback.signature_parameters in
  ignore (expect_parameter_default inner);
  let open Yojson.Safe.Util in
  let expression_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "parameters" |> to_list |> List.hd |> member "default"
    |> member "value"
  in
  Alcotest.(check string)
    "JSON generated expression kind" "binary"
    (expression_json |> member "kind" |> to_string)

let included_default_expression () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let declaration_file = Filename.concat root "defaults.HC" in
      write_file root_file "#include \"defaults\"";
      write_file declaration_file "extern U0 Included(I64 value=1+2);";
      let session = Session.create () in
      let source =
        Session.load_source session ~path:root_file |> Result.get_ok
      in
      let output =
        Holyc_lib.parse_detailed session ~config:(config root) ~source
      in
      let default =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        List.hd prototype.parameters |> expect_parameter_default
      in
      let expression_source =
        Source_manager.find (Session.sources session)
          (Ast.expression_location default.value).span.source
        |> Option.get
      in
      Alcotest.(check string)
        "default keeps its included source path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let array_dimension_source_behavior () =
  let parser_source = pinned "Compiler/PrsVar.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains parser_source fragment))
    [
      ("array dimension entry point", "U0 PrsArrayDims(CCmpCtrl *cc");
      ("array suffix starts at a bracket", "if (cc->token=='[')");
      ("function arguments reject arrays", "mode.u8[1]==PRS1B_FUN_ARG");
      ("only the first dimension may be empty", "Lex(cc)==']' && !dim->next");
      ("dimension uses the integer expression path", "j=LexExpressionI64(cc)");
      ("closing bracket is required", "if (cc->token!=']')");
    ];
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ("Kernel/KernelC.HH", "public extern U16 mon_start_days1[12];");
      ("Compiler/Lex.HC", "cmp_type_flags_src_code[(DOCT_TYPES_NUM+63)/64]");
      ("Compiler/Asm.HC", "U8 asm_seg_prefixes[6]");
      ("Adam/Gr/GrGlbls.HC", "circle_lo[GR_PEN_BRUSHES_NUM]");
    ];
  let _, _, output =
    parse_string
      "public extern U16 mon_start_days1[12];\n\
       I64 cmp_type_flags_src_code[(DOCT_TYPES_NUM+63)/64];"
  in
  let variables = expect_ast output |> globals in
  Alcotest.(check (list string))
    "pinned primitive array names"
    [ "mon_start_days1"; "cmp_type_flags_src_code" ]
    (List.map
       (fun (variable : Ast.global_variable) -> variable.name.spelling)
       variables)

let direct_array_declarators () =
  let source = "I64 values[16];\nU8 *buffers[2][3];\nI32 inferred[];" in
  let _, _, output = parse_string source in
  let values, buffers, inferred =
    match expect_ast output |> globals with
    | [ values; buffers; inferred ] -> (values, buffers, inferred)
    | variables ->
        Alcotest.failf "expected three array globals, got %d"
          (List.length variables)
  in
  Alcotest.(check int)
    "one direct dimension" 1
    (List.length values.array_dimensions);
  let values_dimension = List.hd values.array_dimensions in
  Alcotest.(check int64)
    "direct dimension value" 16L
    (values_dimension |> expect_dimension_expression
   |> expect_integer_expression);
  Alcotest.(check int)
    "opening bracket offset" 10 values_dimension.opening_bracket.span.start;
  Alcotest.(check int)
    "closing bracket offset" 13 values_dimension.closing_bracket.span.start;
  Alcotest.(check int)
    "dimension covers both brackets" 4
    (values_dimension.location.span.stop - values_dimension.location.span.start);
  Alcotest.(check int)
    "pointer depth remains separate" 1
    (List.length buffers.pointer_layers);
  Alcotest.(check (list int64))
    "multidimensional source order" [ 2L; 3L ]
    (List.map
       (fun dimension ->
         dimension |> expect_dimension_expression |> expect_integer_expression)
       buffers.array_dimensions);
  Alcotest.(check int)
    "unsized first dimension" 1
    (List.length inferred.array_dimensions);
  Alcotest.(check bool)
    "unsized remains distinct" true
    (Option.is_none (List.hd inferred.array_dimensions).dimension_expression)

let grouped_array_declarators () =
  let _, _, output = parse_string "U32 direct[16],*pointers[2][3],plain;" in
  let declaration = expect_ast output |> expect_one_declaration in
  Alcotest.(check (list string))
    "array group names"
    [ "direct"; "pointers"; "plain" ]
    (List.map
       (fun (declarator : Ast.global_declarator) -> declarator.name.spelling)
       declaration.declarators);
  Alcotest.(check (list int))
    "array dimensions are per declarator" [ 1; 2; 0 ]
    (List.map
       (fun (declarator : Ast.global_declarator) ->
         List.length declarator.array_dimensions)
       declaration.declarators);
  Alcotest.(check (list int))
    "pointer layers remain per declarator" [ 0; 1; 0 ]
    (List.map
       (fun (declarator : Ast.global_declarator) ->
         List.length declarator.pointer_layers)
       declaration.declarators);
  let _, _, distinction_output =
    parse_string "I64 unsized[],explicit_zero[0],matrix[][2];"
  in
  let declarators =
    (expect_ast distinction_output |> expect_one_declaration).declarators
  in
  let unsized, explicit_zero, matrix =
    match declarators with
    | [ unsized; explicit_zero; matrix ] -> (unsized, explicit_zero, matrix)
    | items ->
        Alcotest.failf "expected three distinction declarators, got %d"
          (List.length items)
  in
  Alcotest.(check bool)
    "empty first dimension is absent" true
    (Option.is_none (List.hd unsized.array_dimensions).dimension_expression);
  Alcotest.(check int64)
    "explicit zero remains an expression" 0L
    (List.hd explicit_zero.array_dimensions
    |> expect_dimension_expression |> expect_integer_expression);
  Alcotest.(check (list bool))
    "only the first matrix dimension is unsized" [ false; true ]
    (List.map
       (fun (dimension : Ast.array_dimension) ->
         Option.is_some dimension.dimension_expression)
       matrix.array_dimensions)

let array_dimension_expression_registry () =
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let source =
        Printf.sprintf "I64 dimension_%d[1%s2];" index operator.spelling
      in
      let _, _, output = parse_string source in
      let dimension =
        expect_ast output |> expect_one_global |> fun variable ->
        List.hd variable.array_dimensions
      in
      let binary =
        dimension |> expect_dimension_expression |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " dimension operator")
        operator.ic_name binary.binary_operator_spec.ic_name)
    Operator.binary_operators;
  let _, _, output = parse_string "I64 grouped[(1+2)*3];" in
  let expression =
    expect_ast output |> expect_one_global |> fun variable ->
    List.hd variable.array_dimensions |> expect_dimension_expression
  in
  let outer = expect_binary_expression expression in
  Alcotest.(check string)
    "grouped dimension root" "*" outer.binary_operator.operator_spelling;
  match outer.binary_left with
  | Ast.Parenthesized_expression grouped ->
      ignore (expect_binary_expression grouped.grouped_expression)
  | _ -> Alcotest.fail "expected grouping inside the array dimension"

let array_dimension_provenance () =
  let source = "#define DIMS [1+2][4]\nI64 generated DIMS;" in
  let session, root, output = parse_string source in
  let variable = expect_ast output |> expect_one_global in
  Alcotest.(check int)
    "two generated dimensions" 2
    (List.length variable.array_dimensions);
  let first = List.hd variable.array_dimensions in
  List.iter
    (fun ((description, location) : string * Ast.location) ->
      Alcotest.(check bool)
        (description ^ " uses generated source")
        false
        (Source_id.equal location.Ast.span.source (Source_file.id root));
      Alcotest.(check bool)
        (description ^ " keeps invocation provenance")
        true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        (description ^ " keeps definition provenance")
        true
        (Option.is_some location.defined_at))
    [
      ("opening bracket", first.opening_bracket);
      ( "dimension expression",
        Ast.expression_location (expect_dimension_expression first) );
      ("closing bracket", first.closing_bracket);
    ];
  let open Yojson.Safe.Util in
  let dimensions =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "array_dimensions" |> to_list
  in
  Alcotest.(check int) "JSON dimension count" 2 (List.length dimensions);
  Alcotest.(check string)
    "JSON expression kind" "binary"
    (List.hd dimensions |> member "expression" |> member "kind" |> to_string);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "arrays.HC" in
      write_file root_file "#include \"arrays\"";
      write_file declaration_file "U16 included[12];";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let dimension =
        expect_ast include_output |> expect_one_global |> fun included ->
        List.hd included.array_dimensions
      in
      let dimension_source =
        Source_manager.find
          (Session.sources include_session)
          dimension.location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included dimension keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path dimension_source))

let array_dimension_failures () =
  List.iter
    (fun (description, source, name, code, message_fragment) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (description ^ " code") code diagnostic.code;
      Alcotest.(check bool)
        (description ^ " message") true
        (contains diagnostic.message message_fragment);
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected array global %s became visible" name)
    [
      ( "empty later dimension",
        "I64 EmptyLater[2][];",
        "EmptyLater",
        "HCPARSE0022",
        "only the first array dimension may be empty" );
      ( "missing expression operand",
        "I64 MissingOperand[1+];",
        "MissingOperand",
        "HCPARSE0018",
        "array dimension expression operand" );
      ( "missing closing bracket",
        "I64 MissingBracket[1;",
        "MissingBracket",
        "HCPARSE0023",
        "expected ']'" );
      ( "unsupported indexing",
        "I64 Indexed[count[0]];",
        "Indexed",
        "HCPARSE0020",
        "continuation" );
      ( "unsupported call",
        "I64 Called[Count()];",
        "Called",
        "HCPARSE0020",
        "continuation" );
      ( "unsupported atom",
        "I64 LastClass[lastclass];",
        "LastClass",
        "HCPARSE0020",
        "not implemented" );
    ];
  let nesting = Parser.max_expression_depth in
  let nested_source =
    Printf.sprintf "I64 TooNested[%s1%s];" (String.make nesting '(')
      (String.make nesting ')')
  in
  let nested_session, _, nested_output = parse_string nested_source in
  Alcotest.(check string)
    "array expression nesting diagnostic" "HCPARSE0021"
    (first_diagnostic nested_output).code;
  Alcotest.(check bool)
    "array nesting message names its context" true
    (contains (first_diagnostic nested_output).message
       "array dimension expression nesting");
  (match
     Symbol_visibility.Environment.find_preprocessor
       (Session.symbols nested_session)
       "TooNested"
   with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "the excessive array expression became visible");
  let session, _, output = parse_string "I64 accepted[1],rejected[2][];" in
  Alcotest.(check bool)
    "malformed group has no AST" true
    (Option.is_none output.ast);
  (match
     Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
       "rejected"
   with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "the malformed group published its rejected declarator");
  let _, _, parameter_output =
    parse_string "extern U0 ArrayArgument(I64 values[2]);"
  in
  Alcotest.(check string)
    "function argument arrays retain their source rejection" "HCPARSE0011"
    (first_diagnostic parameter_output).code

let deterministic_array_dumps () =
  let session, _, output = parse_string "I64 matrix[][2+3];" in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human array dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON array dump repeats byte for byte" json
    (Ast_dump.json sources ast)

let default_expression_failures () =
  List.iter
    (fun (description, source, name, code, message_fragment) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (description ^ " code") code diagnostic.code;
      Alcotest.(check bool)
        (description ^ " message") true
        (contains diagnostic.message message_fragment);
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected default prototype %s became visible" name)
    [
      ( "missing operand",
        "extern U0 Missing(I64 value=);",
        "Missing",
        "HCPARSE0018",
        "expected a default expression operand" );
      ( "missing right operand",
        "extern U0 MissingRight(I64 value=1+);",
        "MissingRight",
        "HCPARSE0018",
        "expected a default expression operand" );
      ( "unmatched grouping",
        "extern U0 Unmatched(I64 value=(1+2;",
        "Unmatched",
        "HCPARSE0019",
        "close default expression" );
      ( "unsupported call",
        "extern U0 Called(I64 value=Factory());",
        "Called",
        "HCPARSE0020",
        "continuation" );
      ( "unsupported postfix increment",
        "extern U0 Postfix(I64 value=counter++);",
        "Postfix",
        "HCPARSE0020",
        "continuation" );
      ( "unsupported lastclass",
        "extern U0 LastClass(I64 value=lastclass);",
        "LastClass",
        "HCPARSE0020",
        "not implemented" );
    ];
  let nesting = Parser.max_expression_depth in
  let source =
    Printf.sprintf "extern U0 TooNested(I64 value=%s1%s);"
      (String.make nesting '(') (String.make nesting ')')
  in
  let session, _, output = parse_string source in
  Alcotest.(check bool)
    "excessive expression nesting has no AST" true
    (Option.is_none output.ast);
  Alcotest.(check string)
    "excessive expression nesting code" "HCPARSE0021"
    (first_diagnostic output).code;
  match
    Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
      "TooNested"
  with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "excessively nested default prototype became visible"

let function_prototype_failures () =
  List.iter
    (fun (description, source, name, code, message_fragment) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (description ^ " code") code diagnostic.code;
      Alcotest.(check bool)
        (description ^ " message") true
        (contains diagnostic.message message_fragment);
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected prototype %s became visible" name)
    [
      ( "unbound function",
        "U0 Unbound();",
        "Unbound",
        "HCPARSE0008",
        "has no declaration binding" );
      ( "missing opening parenthesis",
        "extern U0 NoOpen);",
        "NoOpen",
        "HCPARSE0003",
        "after global variable" );
      ( "missing parameter type",
        "extern U0 NoType(,I64 value);",
        "NoType",
        "HCPARSE0009",
        "parameter type" );
      ( "unknown parameter type",
        "extern U0 Unknown(Widget value);",
        "Unknown",
        "HCPARSE0009",
        "parameter type" );
      ( "missing parameter comma",
        "extern U0 NoComma(I64 first U8 second);",
        "NoComma",
        "HCPARSE0010",
        "expected ',' or ')'" );
      ( "missing closing parenthesis",
        "extern U0 NoClose(I64 value;",
        "NoClose",
        "HCPARSE0010",
        "expected ',' or ')'" );
      ( "missing prototype semicolon",
        "extern U0 NoSemicolon()",
        "NoSemicolon",
        "HCPARSE0016",
        "expected ';'" );
      ( "trailing parameter comma",
        "extern U0 Trailing(I64 value,);",
        "Trailing",
        "HCPARSE0009",
        "after ','" );
      ( "nonterminal variadic marker",
        "extern U0 Nonterminal(...,I64 value);",
        "Nonterminal",
        "HCPARSE0015",
        "after variadic marker" );
      ( "array parameter",
        "extern U0 Arrayed(I64 values[2]);",
        "Arrayed",
        "HCPARSE0011",
        "array parameters" );
      ( "register-qualified parameter",
        "extern U0 Registered(I64 *reg value);",
        "Registered",
        "HCPARSE0013",
        "must appear before" );
      ( "parameter pointer depth",
        "extern U0 TooDeep(U8 *****value);",
        "TooDeep",
        "HCPARSE0004",
        "at most 4 pointer stars" );
    ]

let deterministic_function_dumps () =
  let session, _, output =
    parse_string "public extern U8 *Print(U8 *format,...);"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human prototype dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON prototype dump repeats byte for byte" json
    (Ast_dump.json sources ast)

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
            Alcotest.fail "independent declarations unexpectedly formed a group"
        | Ast.Function_prototype _ ->
            Alcotest.fail
              "independent declarations unexpectedly formed a prototype")
      ast.items
  in
  Alcotest.(check (list string))
    "source order" [ "first"; "second" ]
    (List.map
       (fun (variable : Ast.global_variable) -> variable.name.spelling)
       variables);
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
             Alcotest.fail "conditional fixture unexpectedly formed a group"
         | Ast.Function_prototype _ ->
             Alcotest.fail "conditional fixture unexpectedly formed a prototype")
       ast.items)

let unsupported_forms () =
  let cases =
    [
      ("unknown type", "Widget value;", "HCPARSE0001");
      ("missing name", "I64 ;", "HCPARSE0002");
      ("missing semicolon", "I64 value", "HCPARSE0003");
      ("initializer", "I64 value=1;", "HCPARSE0003");
      ("function", "I64 Function();", "HCPARSE0008");
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
    Alcotest.test_case "pinned function calling modifier behavior" `Quick
      function_calling_modifier_source_behavior;
    Alcotest.test_case "function calling modifiers" `Quick
      direct_function_calling_modifiers;
    Alcotest.test_case "function calling modifier transitions" `Quick
      function_calling_modifier_transitions;
    Alcotest.test_case "definition-backed function calling modifier" `Quick
      definition_backed_function_calling_modifier;
    Alcotest.test_case "function calling modifier visibility" `Quick
      function_calling_modifier_visibility;
    Alcotest.test_case "function calling modifier failures" `Quick
      function_calling_modifier_failures;
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
    Alcotest.test_case "pinned _intern binding behavior" `Quick
      intern_binding_source_behavior;
    Alcotest.test_case "pinned _intern prototypes" `Quick pinned_intern_bindings;
    Alcotest.test_case "_intern target expression registry" `Quick
      intern_target_expression_registry;
    Alcotest.test_case "_intern binding provenance" `Quick
      intern_binding_provenance;
    Alcotest.test_case "_intern binding updates symbol conditionals" `Quick
      intern_binding_visibility;
    Alcotest.test_case "_intern binding failures" `Quick intern_binding_failures;
    Alcotest.test_case "deterministic _intern dumps" `Quick
      deterministic_intern_dumps;
    Alcotest.test_case "pinned function prototype behavior" `Quick
      function_prototype_source_behavior;
    Alcotest.test_case "bound primitive function prototypes" `Quick
      direct_function_prototypes;
    Alcotest.test_case "function import compilation mode boundary" `Quick
      function_import_mode_boundary;
    Alcotest.test_case "definition-backed function prototype" `Quick
      definition_backed_function_prototype;
    Alcotest.test_case "function prototypes update symbol conditionals" `Quick
      function_streaming_visibility;
    Alcotest.test_case "pinned default-expression behavior" `Quick
      default_parameter_expression_source_behavior;
    Alcotest.test_case "default-expression atoms" `Quick
      default_expression_atoms;
    Alcotest.test_case "default-expression prefixes" `Quick
      default_expression_prefixes;
    Alcotest.test_case "default-expression binary registry" `Quick
      default_expression_binary_registry;
    Alcotest.test_case "default-expression precedence" `Quick
      default_expression_precedence;
    Alcotest.test_case "default-expression associativity" `Quick
      default_expression_associativity;
    Alcotest.test_case "default-expression grouping and power" `Quick
      default_expression_grouping_and_power;
    Alcotest.test_case "nested and generated defaults" `Quick
      default_expression_nested_and_provenance;
    Alcotest.test_case "included default expression" `Quick
      included_default_expression;
    Alcotest.test_case "pinned array dimension behavior" `Quick
      array_dimension_source_behavior;
    Alcotest.test_case "direct global array declarators" `Quick
      direct_array_declarators;
    Alcotest.test_case "grouped global array declarators" `Quick
      grouped_array_declarators;
    Alcotest.test_case "array dimension expression registry" `Quick
      array_dimension_expression_registry;
    Alcotest.test_case "array dimension provenance" `Quick
      array_dimension_provenance;
    Alcotest.test_case "array dimension failures" `Quick
      array_dimension_failures;
    Alcotest.test_case "deterministic array dumps" `Quick
      deterministic_array_dumps;
    Alcotest.test_case "default-expression failures" `Quick
      default_expression_failures;
    Alcotest.test_case "function prototype failures" `Quick
      function_prototype_failures;
    Alcotest.test_case "pinned function-pointer parameter behavior" `Quick
      function_pointer_parameter_source_behavior;
    Alcotest.test_case "pinned function-pointer prototypes" `Quick
      pinned_function_pointer_prototypes;
    Alcotest.test_case "function-pointer parameters" `Quick
      direct_function_pointer_parameters;
    Alcotest.test_case "definition-backed function-pointer parameter" `Quick
      definition_backed_function_pointer_parameter;
    Alcotest.test_case "function-pointer prototype visibility" `Quick
      function_pointer_streaming_visibility;
    Alcotest.test_case "function-pointer parameter failures" `Quick
      function_pointer_parameter_failures;
    Alcotest.test_case "pinned parameter register behavior" `Quick
      function_parameter_register_source_behavior;
    Alcotest.test_case "function parameter register qualifiers" `Quick
      direct_function_parameter_register_qualifiers;
    Alcotest.test_case "all explicit U64 parameter registers" `Quick
      every_explicit_u64_parameter_register;
    Alcotest.test_case "definition-backed parameter register qualifier" `Quick
      definition_backed_parameter_register_qualifier;
    Alcotest.test_case "qualified function updates symbol conditionals" `Quick
      function_parameter_register_visibility;
    Alcotest.test_case "parameter register qualifier failures" `Quick
      function_parameter_register_failures;
    Alcotest.test_case "deterministic function dumps" `Quick
      deterministic_function_dumps;
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
