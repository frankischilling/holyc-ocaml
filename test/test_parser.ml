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

let expect_default_expression (default : Ast.parameter_default) =
  match default.value with
  | Ast.Expression_default expression -> expression
  | Ast.Lastclass_default _ ->
      Alcotest.fail "expected an ordinary default expression"

let expect_lastclass_default (default : Ast.parameter_default) =
  match default.value with
  | Ast.Lastclass_default lastclass -> lastclass
  | Ast.Expression_default _ -> Alcotest.fail "expected a lastclass default"

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

let expect_postfix_expression = function
  | Ast.Postfix_expression postfix -> postfix
  | _ -> Alcotest.fail "expected a postfix expression"

let expect_postfix_cast_expression = function
  | Ast.Postfix_cast_expression cast -> cast
  | _ -> Alcotest.fail "expected a postfix cast expression"

let expect_sizeof_expression = function
  | Ast.Sizeof_expression sizeof_expression -> sizeof_expression
  | _ -> Alcotest.fail "expected a sizeof expression"

let expect_offset_expression = function
  | Ast.Offset_expression offset_expression -> offset_expression
  | _ -> Alcotest.fail "expected an offset expression"

let expect_defined_expression = function
  | Ast.Defined_expression defined_expression -> defined_expression
  | _ -> Alcotest.fail "expected a defined expression"

let expect_call_expression = function
  | Ast.Call_expression call -> call
  | _ -> Alcotest.fail "expected a call expression"

let expect_index_expression = function
  | Ast.Index_expression index -> index
  | _ -> Alcotest.fail "expected an index expression"

let expect_member_expression = function
  | Ast.Member_expression member -> member
  | _ -> Alcotest.fail "expected a member expression"

let member_access_kind_name = function
  | Ast.Direct_member -> "direct"
  | Ast.Pointer_member -> "pointer"

let postfix_operator_kind_name = function
  | Ast.Post_increment -> "post_increment"
  | Ast.Post_decrement -> "post_decrement"

let expect_provided_call_argument (argument : Ast.call_argument) =
  match argument.call_argument_value with
  | Ast.Provided_call_argument expression -> expression
  | Ast.Omitted_call_argument ->
      Alcotest.fail "expected a provided call argument"

let expect_omitted_call_argument (argument : Ast.call_argument) =
  match argument.call_argument_value with
  | Ast.Omitted_call_argument -> ()
  | Ast.Provided_call_argument _ ->
      Alcotest.fail "expected an omitted call argument"

let expect_one_implicit_output ast =
  match ast.Ast.items with
  | [ Ast.Top_level_statement (Ast.Implicit_output_statement statement) ] ->
      statement
  | items ->
      Alcotest.failf "expected one implicit output statement, got %d items"
        (List.length items)

let expect_expression_statement = function
  | Ast.Expression_statement statement -> statement
  | _ -> Alcotest.fail "expected an expression statement"

let expect_empty_statement = function
  | Ast.Empty_statement statement -> statement
  | _ -> Alcotest.fail "expected an empty statement"

let expect_statement_sequence = function
  | Ast.Sequence_statement sequence -> sequence
  | _ -> Alcotest.fail "expected a comma-linked statement sequence"

let expect_marker_fixed_argument (statement : Ast.implicit_output_statement) =
  match statement.fixed_argument with
  | Ast.Marker_fixed_argument expression -> expression
  | Ast.Expression_fixed_argument _ ->
      Alcotest.fail "expected the marker to supply the fixed argument"

let expect_following_fixed_argument (statement : Ast.implicit_output_statement)
    =
  match statement.fixed_argument with
  | Ast.Expression_fixed_argument expression -> expression
  | Ast.Marker_fixed_argument _ ->
      Alcotest.fail "expected an expression after the empty marker"

let expect_identifier_expression = function
  | Ast.Identifier_expression identifier -> identifier
  | _ -> Alcotest.fail "expected an identifier expression"

let expect_string_literal = function
  | Ast.String_literal literal -> literal
  | _ -> Alcotest.fail "expected a string literal"

let expect_character_literal = function
  | Ast.Character_literal literal -> literal
  | _ -> Alcotest.fail "expected a character literal"

let expect_bytes_value (literal : Ast.expression_literal) =
  match literal.literal_value with
  | Ast.Bytes_value value -> value
  | Ast.Integer_value _ | Ast.Float_value _ ->
      Alcotest.fail "expected a byte-string literal value"

let expect_character_value (literal : Ast.expression_literal) =
  match literal.literal_value with
  | Ast.Integer_value value -> value
  | Ast.Bytes_value _ | Ast.Float_value _ ->
      Alcotest.fail "expected an integer character value"

let globals ast =
  List.map
    (function
      | Ast.Global_variable variable -> variable
      | Ast.Global_declaration _ ->
          Alcotest.fail "expected singleton globals, got a declaration group"
      | Ast.Function_prototype _ ->
          Alcotest.fail "expected singleton globals, got a function prototype"
      | Ast.Top_level_statement _ ->
          Alcotest.fail "expected singleton globals, got a top-level statement")
    ast.Ast.items

let prototypes ast =
  List.map
    (function
      | Ast.Function_prototype prototype -> prototype
      | Ast.Global_variable _ ->
          Alcotest.fail "expected function prototypes, got a singleton global"
      | Ast.Global_declaration _ ->
          Alcotest.fail "expected function prototypes, got a declaration group"
      | Ast.Top_level_statement _ ->
          Alcotest.fail
            "expected function prototypes, got a top-level statement")
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
          Alcotest.fail "primitive fixture unexpectedly formed a prototype"
      | Ast.Top_level_statement _ ->
          Alcotest.fail "primitive fixture unexpectedly formed a statement")
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
      ( "call cannot follow a postfix increment",
        "_intern Resolve()++() I64 Called();",
        Some "Called",
        "HCPARSE0028",
        "must end the postfix chain" );
      ( "index cannot follow a postfix increment",
        "_intern Table++[0] I64 Indexed();",
        Some "Indexed",
        "HCPARSE0028",
        "must end the postfix chain" );
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

let call_expression_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("call parser entry point", "I64 PrsFunCall(CCmpCtrl *cc");
      ("direct call dispatch", "PrsFunCall(cc,ps,FALSE,tmpex)");
      ("indirect call dispatch", "PrsFunCall(cc,ps,TRUE,PrsPop2(ps))");
      ("parenthesized call detection", "if (cc->token=='(') {");
      ("fixed argument default flag", "tmpm->flags & MLF_DFT_AVAILABLE");
      ( "omitted argument token test",
        "(cc->token==')' || cc->token==',' || !needs_right_paren)" );
      ("call closing parenthesis check", "LexExcept(cc,\"Missing ')' at \"");
    ];
  let language_guide = pinned "Doc/HolyC.DD" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains language_guide fragment))
    [
      ( "parentheses may be omitted for default-only calls",
        "Function with no args, or just default args can be called without "
        ^ "parentheses." );
      ("defaults need not trail", "Default args don't have to be on the end.");
      ("documented middle hole", "Test(,3);");
    ];
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ( "Compiler/AsmInit.HC",
        "CmpCtrlNew(FileRead(\"OpCodes.DD\"),,\"OpCodes.DD.Z\")" );
      ("Compiler/CMain.HC", "PrsStmt(cc,,,cmp_flags)");
      ("Compiler/CMisc.HC", "GetStr(,,GSF_SHIFT_ESC_EXIT)");
    ]

let call_argument_slots () =
  let call_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding |> expect_call_expression
  in
  let middle_hole = call_from "_intern Test(,3) I64 Linked();" in
  Alcotest.(check int)
    "middle-hole argument count" 2
    (List.length middle_hole.call_arguments);
  let first = List.nth middle_hole.call_arguments 0 in
  let second = List.nth middle_hole.call_arguments 1 in
  expect_omitted_call_argument first;
  Alcotest.(check int64)
    "provided value after a hole" 3L
    (expect_provided_call_argument second |> expect_integer_expression);
  Alcotest.(check int)
    "omitted slot has an insertion point" 0
    (first.call_argument_location.span.stop
   - first.call_argument_location.span.start);
  Alcotest.(check bool)
    "the hole owns its following comma" true
    (Option.is_some first.following_comma);
  let explicit_zero = call_from "_intern Test(0,3) I64 Explicit();" in
  Alcotest.(check int64)
    "explicit zero remains a provided value" 0L
    (List.hd explicit_zero.call_arguments
    |> expect_provided_call_argument |> expect_integer_expression);
  let empty = call_from "_intern Clock() I64 Empty();" in
  Alcotest.(check int)
    "empty call has no argument slots" 0
    (List.length empty.call_arguments);
  let consecutive = call_from "_intern F(,,,0,) I64 Sparse();" in
  Alcotest.(check int)
    "consecutive and trailing holes" 5
    (List.length consecutive.call_arguments);
  List.iter expect_omitted_call_argument
    [
      List.nth consecutive.call_arguments 0;
      List.nth consecutive.call_arguments 1;
      List.nth consecutive.call_arguments 2;
      List.nth consecutive.call_arguments 4;
    ];
  Alcotest.(check int64)
    "sparse call retains explicit zero" 0L
    (List.nth consecutive.call_arguments 3
    |> expect_provided_call_argument |> expect_integer_expression);
  Alcotest.(check (list bool))
    "each comma belongs to the preceding slot"
    [ true; true; true; true; false ]
    (List.map
       (fun (argument : Ast.call_argument) ->
         Option.is_some argument.following_comma)
       consecutive.call_arguments)

let call_expression_precedence_and_nesting () =
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let source =
        Printf.sprintf "_intern Target(1) %s 2 I64 Call%d();" operator.spelling
          index
      in
      let _, _, output = parse_string source in
      let root =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " remains the binary root")
        operator.spelling root.binary_operator.operator_spelling;
      ignore (expect_call_expression root.binary_left))
    Operator.binary_operators;
  let _, _, nested_output =
    parse_string "_intern Outer(Inner(),,3)+4 I64 Nested();"
  in
  let outer =
    expect_ast nested_output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_binary_expression
    |> fun binary -> expect_call_expression binary.binary_left
  in
  Alcotest.(check int)
    "nested sparse call argument count" 3
    (List.length outer.call_arguments);
  ignore
    (List.hd outer.call_arguments
    |> expect_provided_call_argument |> expect_call_expression);
  expect_omitted_call_argument (List.nth outer.call_arguments 1);
  let _, _, indirect_output =
    parse_string "_intern (Factory())(1) I64 Indirect();"
  in
  let indirect =
    expect_ast indirect_output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding |> expect_call_expression
  in
  match indirect.call_callee with
  | Ast.Parenthesized_expression parenthesized ->
      ignore (expect_call_expression parenthesized.grouped_expression)
  | _ -> Alcotest.fail "expected a parenthesized call callee"

let call_expression_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=Compute());"
  in
  let default_call =
    expect_ast default_output |> expect_one_prototype |> fun prototype ->
    List.hd prototype.parameters |> expect_parameter_default |> fun default ->
    expect_call_expression (expect_default_expression default)
  in
  Alcotest.(check int)
    "default-expression call has no arguments" 0
    (List.length default_call.call_arguments);
  let _, _, array_output = parse_string "I64 values[Size()];" in
  let dimension_call =
    expect_ast array_output |> expect_one_global |> fun variable ->
    List.hd variable.array_dimensions
    |> expect_dimension_expression |> expect_call_expression
  in
  Alcotest.(check int)
    "array-dimension call has no arguments" 0
    (List.length dimension_call.call_arguments);
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode "_intern Address(,1) I64 Linked();"
      in
      let call =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_call_expression
      in
      Alcotest.(check int)
        "call syntax is shared by JIT and AOT" 2
        (List.length call.call_arguments))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let call_expression_provenance () =
  let session, root, output =
    parse_string "#define CALL Target(,3)\n_intern CALL I64 Generated();"
  in
  let call =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding |> expect_call_expression
  in
  let omitted = List.hd call.call_arguments in
  let provided = List.nth call.call_arguments 1 in
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
      ("callee", Ast.expression_location call.call_callee);
      ("opening parenthesis", call.call_opening_parenthesis);
      ("omitted slot", omitted.call_argument_location);
      ("omitted slot comma", Option.get omitted.following_comma);
      ( "provided argument",
        expect_provided_call_argument provided |> Ast.expression_location );
      ("closing parenthesis", call.call_closing_parenthesis);
    ];
  let open Yojson.Safe.Util in
  let target_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated call punctuation provenance" true
    (target_json
    |> member "opening_parenthesis"
    |> member "generated_from" <> `Null);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "call.HC" in
      write_file root_file "#include \"call\"";
      write_file declaration_file "_intern Included(,3) I64 FromInclude();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_call =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_call_expression
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          included_call.call_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included call keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let call_expression_failures () =
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
              Alcotest.failf "rejected call declaration %s became visible" name)
        rejected_name)
    [
      ( "missing argument separator",
        "_intern F(1 2) I64 MissingSeparator();",
        Some "MissingSeparator",
        "HCPARSE0024",
        "expected ',' or ')' after a call argument" );
      ( "missing closing parenthesis",
        "_intern F(",
        None,
        "HCPARSE0025",
        "expected ')' to close a call" );
      ( "missing prefix operand",
        "_intern F(+,2) I64 MissingOperand();",
        Some "MissingOperand",
        "HCPARSE0018",
        "call argument expression operand" );
      ( "call cannot continue after a postfix increment",
        "_intern F()++() I64 Indexed();",
        Some "Indexed",
        "HCPARSE0028",
        "must end the postfix chain" );
    ];
  let nesting = Parser.max_expression_depth in
  let nested_source =
    Printf.sprintf "_intern %s1%s I64 TooNested();"
      (String.concat "" (List.init nesting (fun _ -> "F(")))
      (String.make nesting ')')
  in
  let session, _, output = parse_string nested_source in
  Alcotest.(check string)
    "nested call diagnostic" "HCPARSE0021" (first_diagnostic output).code;
  Alcotest.(check bool)
    "nested call diagnostic names the argument context" true
    (contains (first_diagnostic output).message
       "call argument expression nesting");
  match
    Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
      "TooNested"
  with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "the excessively nested call became visible"

let deterministic_call_dumps () =
  let session, _, output =
    parse_string "_intern Outer(Inner(),,0) I64 Linked();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human call dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON call dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  let open Yojson.Safe.Util in
  let call_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check string)
    "JSON call kind" "call"
    (call_json |> member "kind" |> to_string);
  let arguments = call_json |> member "arguments" |> to_list in
  Alcotest.(check (list string))
    "JSON call argument kinds"
    [ "provided"; "omitted"; "provided" ]
    (List.map
       (fun argument -> argument |> member "kind" |> to_string)
       arguments);
  Alcotest.(check string)
    "nested JSON call kind" "call"
    (List.hd arguments |> member "expression" |> member "kind" |> to_string);
  Alcotest.(check bool)
    "omitted JSON slot keeps its comma" true
    (List.nth arguments 1 |> member "comma" <> `Null);
  Alcotest.(check bool)
    "last JSON argument has no comma" true
    (List.nth arguments 2 |> member "comma" = `Null)

let index_expression_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("index modifier dispatch", "case '[':");
      ("index expression parser", "if (!PrsExpression(cc,NULL,FALSE,ps))");
      ("index closing bracket check", "if (cc->token!=']')");
      ("index term precedence", "*unary_post_prec=PREC_TERM;");
      ("repeated postfix dispatch", "return PE_UNARY_MODIFIERS;");
    ];
  Alcotest.(check bool)
    "language guide indexes argv" true
    (contains (pinned "Doc/HolyC.DD") "res+=argv[i];");
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ( "Compiler/AsmLib.HC",
        "tmpbin->body[aotc->rip++ & (AOT_BIN_BLK_SIZE-1)]=b;" );
      ("Compiler/UAsm.HC", "tmpins1->opcode[j]-tmpins2->opcode[j]");
      ("Kernel/Compress.HC", "c->compress[code].ch");
      ("Kernel/Display.HC", "ptr[0]=ch;");
    ]

let index_expression_shapes_and_precedence () =
  let index_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_index_expression
  in
  let simple = index_from "_intern Table[i] I64 Simple();" in
  (match simple.index_base with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string) "simple index base" "Table" identifier.spelling
  | _ -> Alcotest.fail "expected an identifier index base");
  (match simple.index_value with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string) "simple index value" "i" identifier.spelling
  | _ -> Alcotest.fail "expected an identifier index value");
  let explicit_zero = index_from "_intern Table[0] I64 Zero();" in
  Alcotest.(check int64)
    "zero remains an explicit index expression" 0L
    (expect_integer_expression explicit_zero.index_value);
  let binary = index_from "_intern Table[1+2] I64 Binary();" in
  Alcotest.(check string)
    "binary index retains its root" "+"
    (expect_binary_expression binary.index_value).binary_operator
      .operator_spelling;
  let repeated = index_from "_intern Table[Outer(,3)][j] I64 Repeated();" in
  let first = expect_index_expression repeated.index_base in
  let inner_call = expect_call_expression first.index_value in
  Alcotest.(check int)
    "call in first index keeps its slots" 2
    (List.length inner_call.call_arguments);
  expect_omitted_call_argument (List.hd inner_call.call_arguments);
  (match repeated.index_value with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string) "second index value" "j" identifier.spelling
  | _ -> Alcotest.fail "expected the second identifier index");
  let call_then_index = index_from "_intern Factory()[0] I64 Called();" in
  ignore (expect_call_expression call_then_index.index_base);
  let _, _, index_then_call_output =
    parse_string "_intern Callbacks[0](1) I64 Invoked();"
  in
  let index_then_call =
    expect_ast index_then_call_output |> expect_one_prototype
    |> fun prototype ->
    expect_expression_binding_target prototype.binding |> expect_call_expression
  in
  ignore (expect_index_expression index_then_call.call_callee);
  let parenthesized = index_from "_intern (Table)[i] I64 Grouped();" in
  (match parenthesized.index_base with
  | Ast.Parenthesized_expression _ -> ()
  | _ -> Alcotest.fail "expected a parenthesized index base");
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let source =
        Printf.sprintf "_intern Table[1] %s 2 I64 Indexed%d();"
          operator.spelling index
      in
      let _, _, output = parse_string source in
      let root =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " remains the binary root")
        operator.spelling root.binary_operator.operator_spelling;
      ignore (expect_index_expression root.binary_left))
    Operator.binary_operators

let index_expression_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=Table[0]);"
  in
  ignore
    ( expect_ast default_output |> expect_one_prototype |> fun prototype ->
      List.hd prototype.parameters |> expect_parameter_default |> fun default ->
      expect_index_expression (expect_default_expression default) );
  let _, _, dimension_output = parse_string "I64 values[sizes[0]];" in
  ignore
    ( expect_ast dimension_output |> expect_one_global |> fun variable ->
      List.hd variable.array_dimensions
      |> expect_dimension_expression |> expect_index_expression );
  let _, _, argument_output =
    parse_string "_intern Consume(values[0]) I64 Called();"
  in
  let argument_index =
    expect_ast argument_output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding |> expect_call_expression
    |> fun call ->
    List.hd call.call_arguments
    |> expect_provided_call_argument |> expect_index_expression
  in
  Alcotest.(check int64)
    "call argument index value" 0L
    (expect_integer_expression argument_index.index_value);
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode "_intern Table[0] I64 Indexed();"
      in
      ignore
        ( expect_ast output |> expect_one_prototype |> fun prototype ->
          expect_expression_binding_target prototype.binding
          |> expect_index_expression ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let index_expression_provenance () =
  let session, root, output =
    parse_string "#define INDEX Table[Slot()]\n_intern INDEX I64 Generated();"
  in
  let index =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_index_expression
  in
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
      ("base", Ast.expression_location index.index_base);
      ("opening bracket", index.index_opening_bracket);
      ("index value", Ast.expression_location index.index_value);
      ("closing bracket", index.index_closing_bracket);
      ("full index", index.index_location);
    ];
  let open Yojson.Safe.Util in
  let target_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated index punctuation provenance" true
    (target_json |> member "opening_bracket" |> member "generated_from" <> `Null);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "index.HC" in
      write_file root_file "#include \"index\"";
      write_file declaration_file "_intern Table[0] I64 Included();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_index =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_index_expression
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          included_index.index_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included index keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let index_expression_failures () =
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
              Alcotest.failf "rejected index declaration %s became visible" name)
        rejected_name)
    [
      ( "empty index",
        "_intern Table[] I64 Empty();",
        Some "Empty",
        "HCPARSE0018",
        "index expression operand" );
      ( "missing closing bracket",
        "_intern Table[1 I64 Missing();",
        Some "Missing",
        "HCPARSE0026",
        "expected ']' to close an index expression" );
      ( "missing binary operand",
        "_intern Table[1+] I64 MissingOperand();",
        Some "MissingOperand",
        "HCPARSE0018",
        "index expression operand" );
      ( "index cannot continue after a postfix increment",
        "_intern Table[0]++[1] I64 Member();",
        Some "Member",
        "HCPARSE0028",
        "must end the postfix chain" );
    ];
  let nesting = Parser.max_expression_depth in
  let nested_source =
    Printf.sprintf "_intern %s0%s I64 TooNested();"
      (String.concat "" (List.init nesting (fun _ -> "Table[")))
      (String.make nesting ']')
  in
  let session, _, output = parse_string nested_source in
  Alcotest.(check string)
    "nested index diagnostic" "HCPARSE0021" (first_diagnostic output).code;
  Alcotest.(check bool)
    "nested index diagnostic names its context" true
    (contains (first_diagnostic output).message "index expression nesting");
  match
    Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
      "TooNested"
  with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "the excessively nested index became visible"

let deterministic_index_dumps () =
  let session, _, output =
    parse_string "_intern Table[Outer(,3)][j] I64 Indexed();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human index dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON index dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  let open Yojson.Safe.Util in
  let index_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check string)
    "JSON outer index kind" "index"
    (index_json |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON nested index kind" "index"
    (index_json |> member "base" |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON call inside index kind" "call"
    (index_json |> member "base" |> member "index" |> member "kind" |> to_string)

let member_expression_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("direct member branch", "case '.':");
      ("pointer member branch", "case TK_DEREFERENCE:");
      ("member name requirement", "if (Lex(cc)!=TK_IDENT ||");
      ("member lookup", "!(tmpm=MemberFind(cc->cur_str,tmpc)))");
      ("repeated member dispatch", "return PE_UNARY_MODIFIERS;");
      ("mixed member chain", "tmpi=cc->coc.coc_head.last;");
      ("direct member use", "cmp.internal_types[RT_I64]");
    ];
  Alcotest.(check bool)
    "arrow token mapping" true
    (contains (pinned "Compiler/CInit.HC") "d['-']=TK_DEREFERENCE<<16+'>';");
  Alcotest.(check bool)
    "arrow token identity" true
    (contains (pinned "Kernel/KernelA.HH") "#define TK_DEREFERENCE\t0x107");
  let language = pinned "Doc/HolyC.DD" in
  Alcotest.(check bool)
    "language guide pointer member" true
    (contains language "Fs->except_ch");
  Alcotest.(check bool)
    "language guide direct member" true
    (contains language "offset(classname.membername)")

let member_expression_shapes_and_precedence () =
  let member_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_member_expression
  in
  let direct = member_from "_intern object.value I64 Direct();" in
  Alcotest.(check string)
    "direct access kind" "direct"
    (member_access_kind_name direct.member_access_kind);
  Alcotest.(check string)
    "direct operator spelling" "." direct.member_operator.operator_spelling;
  Alcotest.(check string)
    "direct member name" "value" direct.member_name.spelling;
  (match direct.member_base with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string) "direct base" "object" identifier.spelling
  | _ -> Alcotest.fail "expected an identifier direct-member base");
  let pointer = member_from "_intern object->value I64 Pointer();" in
  Alcotest.(check string)
    "pointer access kind" "pointer"
    (member_access_kind_name pointer.member_access_kind);
  Alcotest.(check string)
    "pointer operator spelling" "->" pointer.member_operator.operator_spelling;
  let repeated =
    member_from "_intern root->child.value->leaf I64 Repeated();"
  in
  Alcotest.(check string)
    "outer repeated kind" "pointer"
    (member_access_kind_name repeated.member_access_kind);
  Alcotest.(check string)
    "outer repeated member" "leaf" repeated.member_name.spelling;
  let middle = expect_member_expression repeated.member_base in
  Alcotest.(check string)
    "middle repeated kind" "direct"
    (member_access_kind_name middle.member_access_kind);
  Alcotest.(check string)
    "middle repeated member" "value" middle.member_name.spelling;
  let inner = expect_member_expression middle.member_base in
  Alcotest.(check string)
    "inner repeated kind" "pointer"
    (member_access_kind_name inner.member_access_kind);
  Alcotest.(check string)
    "inner repeated member" "child" inner.member_name.spelling;
  let mixed =
    member_from "_intern Factory().nodes[0]->callback(1).result I64 Mixed();"
  in
  Alcotest.(check string)
    "mixed outer member" "result" mixed.member_name.spelling;
  let callback_call = expect_call_expression mixed.member_base in
  let callback = expect_member_expression callback_call.call_callee in
  Alcotest.(check string)
    "mixed pointer kind" "pointer"
    (member_access_kind_name callback.member_access_kind);
  Alcotest.(check string)
    "mixed callback member" "callback" callback.member_name.spelling;
  let indexed = expect_index_expression callback.member_base in
  let nodes = expect_member_expression indexed.index_base in
  Alcotest.(check string)
    "mixed direct kind" "direct"
    (member_access_kind_name nodes.member_access_kind);
  ignore (expect_call_expression nodes.member_base);
  let parenthesized = member_from "_intern (object).value I64 Grouped();" in
  (match parenthesized.member_base with
  | Ast.Parenthesized_expression _ -> ()
  | _ -> Alcotest.fail "expected a parenthesized member base");
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let source =
        Printf.sprintf "_intern object.value %s 2 I64 Member%d();"
          operator.spelling index
      in
      let _, _, output = parse_string source in
      let root =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " remains the binary root")
        operator.spelling root.binary_operator.operator_spelling;
      ignore (expect_member_expression root.binary_left))
    Operator.binary_operators

let member_expression_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=context.member);"
  in
  ignore
    ( expect_ast default_output |> expect_one_prototype |> fun prototype ->
      List.hd prototype.parameters |> expect_parameter_default |> fun default ->
      expect_member_expression (expect_default_expression default) );
  let _, _, dimension_output = parse_string "I64 values[context->count];" in
  ignore
    ( expect_ast dimension_output |> expect_one_global |> fun variable ->
      List.hd variable.array_dimensions
      |> expect_dimension_expression |> expect_member_expression );
  let _, _, argument_output =
    parse_string "_intern Consume(context.member) I64 Called();"
  in
  ignore
    ( expect_ast argument_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_call_expression
      |> fun call ->
      List.hd call.call_arguments
      |> expect_provided_call_argument |> expect_member_expression );
  let _, _, index_output =
    parse_string "_intern table[context->slot] I64 Indexed();"
  in
  ignore
    ( expect_ast index_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_index_expression
      |> fun index -> expect_member_expression index.index_value );
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode
          "_intern context->member I64 Selected();"
      in
      ignore
        ( expect_ast output |> expect_one_prototype |> fun prototype ->
          expect_expression_binding_target prototype.binding
          |> expect_member_expression ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let member_expression_provenance () =
  let session, root, output =
    parse_string
      "#define MEMBER object->child.value\n_intern MEMBER I64 Generated();"
  in
  let outer =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_member_expression
  in
  let inner = expect_member_expression outer.member_base in
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
      ("base", Ast.expression_location inner.member_base);
      ("pointer operator", inner.member_operator.operator_location);
      ("inner member", inner.member_name.location);
      ("inner expression", inner.member_location);
      ("direct operator", outer.member_operator.operator_location);
      ("outer member", outer.member_name.location);
      ("outer expression", outer.member_location);
    ];
  let open Yojson.Safe.Util in
  let target_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated member operator provenance" true
    (target_json |> member "operator" |> member "location"
   |> member "generated_from" <> `Null);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "member.HC" in
      write_file root_file "#include \"member\"";
      write_file declaration_file "_intern object.member I64 Included();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_member =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_member_expression
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          included_member.member_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included member keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let member_expression_failures () =
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
              Alcotest.failf "rejected member declaration %s became visible"
                name)
        rejected_name)
    [
      ( "missing direct member",
        "_intern object.;",
        None,
        "HCPARSE0027",
        "expected a member name after \".\"" );
      ( "missing pointer member",
        "_intern object->;",
        None,
        "HCPARSE0027",
        "expected a member name after \"->\"" );
      ( "numeric member",
        "_intern object. 1 I64 Numeric();",
        Some "Numeric",
        "HCPARSE0027",
        "\"1\"" );
      ( "repeated operator without a name",
        "_intern object.->member I64 RepeatedOperator();",
        Some "RepeatedOperator",
        "HCPARSE0027",
        "\"->\"" );
      ( "missing member inside an index",
        "_intern table[object.] I64 Nested();",
        Some "Nested",
        "HCPARSE0027",
        "in index expression" );
      ( "member cannot continue after a postfix increment",
        "_intern object.value++->next I64 Incremented();",
        Some "Incremented",
        "HCPARSE0028",
        "must end the postfix chain" );
    ]

let deterministic_member_dumps () =
  let session, _, output =
    parse_string "_intern Factory().nodes[0]->callback(1).result I64 Mixed();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human member dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON member dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  let open Yojson.Safe.Util in
  let member_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check string)
    "JSON outer member kind" "member"
    (member_json |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON outer member access" "direct"
    (member_json |> member "access_kind" |> to_string);
  let callback_json = member_json |> member "base" |> member "callee" in
  Alcotest.(check string)
    "JSON callback member access" "pointer"
    (callback_json |> member "access_kind" |> to_string);
  Alcotest.(check string)
    "JSON callback operator spelling" "->"
    (callback_json |> member "operator" |> member "spelling" |> to_string);
  Alcotest.(check string)
    "JSON direct member before index" "direct"
    (callback_json |> member "base" |> member "base" |> member "access_kind"
   |> to_string)

let postfix_update_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("postincrement modifier branch", "case TK_PLUS_PLUS:");
      ("postincrement flag", "cc->flags|=CCF_POSTINC;");
      ("postdecrement flag", "cc->flags|=CCF_POSTDEC;");
      ("postfix precedence assignment", "*unary_post_prec=PREC_UNARY_POST;");
      ("postincrement IC selection", "i=IC__PP+PREC_UNARY_POST<<16;");
      ("postdecrement IC selection", "i=IC__MM+PREC_UNARY_POST<<16;");
      ("postfix modifier ends the chain", "return PE_DEREFERENCE;");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains compiler_header fragment))
    [
      ("postincrement IC identity", "#define IC__PP");
      ("postdecrement IC identity", "#define IC__MM");
      ("postfix precedence identity", "#define PREC_UNARY_POST");
    ];
  let kernel_header = pinned "Kernel/KernelA.HH" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains kernel_header fragment))
    [
      ("increment token identity", "#define TK_PLUS_PLUS");
      ("decrement token identity", "#define TK_MINUS_MINUS");
      ("postincrement flag identity", "#define CCF_POSTINC");
      ("postdecrement flag identity", "#define CCF_POSTDEC");
    ];
  let lexer_tables = pinned "Compiler/CInit.HC" in
  Alcotest.(check bool)
    "increment token mapping" true
    (contains lexer_tables "d['+']=TK_PLUS_PLUS<<16+'+';");
  Alcotest.(check bool)
    "decrement token mapping" true
    (contains lexer_tables "d['-']=TK_MINUS_MINUS<<16+'-';");
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ("Compiler/AsmInit.HC", "tmpins->opcode[tmpins->opcode_cnt++]");
      ("Kernel/Compress.HC", "if (*src++&0x80)");
      ("Adam/ADbg.HC", "doc_e->data=ptr(I8 *)++;");
      ("Demo/DbgDemo.HC", "if (!(i++%2000000))");
    ]

let postfix_update_shapes_and_precedence () =
  let postfix_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_postfix_expression
  in
  let increment = postfix_from "_intern counter++ I64 Incremented();" in
  Alcotest.(check string)
    "postincrement kind" "post_increment"
    (postfix_operator_kind_name increment.postfix_operator_kind);
  Alcotest.(check string)
    "postincrement spelling" "++" increment.postfix_operator.operator_spelling;
  (match increment.postfix_operand with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string)
        "postincrement operand" "counter" identifier.spelling
  | _ -> Alcotest.fail "expected an identifier postincrement operand");
  let decrement = postfix_from "_intern counter-- I64 Decremented();" in
  Alcotest.(check string)
    "postdecrement kind" "post_decrement"
    (postfix_operator_kind_name decrement.postfix_operator_kind);
  Alcotest.(check string)
    "postdecrement spelling" "--" decrement.postfix_operator.operator_spelling;
  let member =
    postfix_from "_intern Factory().nodes[index].count++ I64 MemberUpdated();"
  in
  ignore (expect_member_expression member.postfix_operand);
  let _, _, dereference_output = parse_string "_intern *ptr++ I64 Value();" in
  let dereference =
    expect_ast dereference_output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_prefix_expression
  in
  Alcotest.(check bool)
    "dereference remains the prefix root" true
    (dereference.prefix_operator_kind = Ast.Dereference);
  ignore (expect_postfix_expression dereference.prefix_operand);
  let _, _, left_output =
    parse_string "_intern counter++ + 1 I64 LeftUpdated();"
  in
  let left_binary =
    expect_ast left_output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_binary_expression
  in
  Alcotest.(check string)
    "binary operator follows postincrement" "+"
    left_binary.binary_operator.operator_spelling;
  ignore (expect_postfix_expression left_binary.binary_left);
  let _, _, right_output =
    parse_string "_intern 1 + counter-- I64 RightUpdated();"
  in
  let right_binary =
    expect_ast right_output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_binary_expression
  in
  ignore (expect_postfix_expression right_binary.binary_right)

let postfix_update_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=counter++);"
  in
  ignore
    ( expect_ast default_output |> expect_one_prototype |> fun prototype ->
      List.hd prototype.parameters |> expect_parameter_default |> fun default ->
      expect_postfix_expression (expect_default_expression default) );
  let _, _, dimension_output = parse_string "I64 values[count--];" in
  ignore
    ( expect_ast dimension_output |> expect_one_global |> fun variable ->
      List.hd variable.array_dimensions
      |> expect_dimension_expression |> expect_postfix_expression );
  let _, _, argument_output =
    parse_string "_intern Consume(counter++) I64 Called();"
  in
  ignore
    ( expect_ast argument_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_call_expression
      |> fun call ->
      List.hd call.call_arguments
      |> expect_provided_call_argument |> expect_postfix_expression );
  let _, _, index_output =
    parse_string "_intern table[index--] I64 Indexed();"
  in
  ignore
    ( expect_ast index_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_index_expression
      |> fun index -> expect_postfix_expression index.index_value );
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode "_intern counter++ I64 Selected();"
      in
      ignore
        ( expect_ast output |> expect_one_prototype |> fun prototype ->
          expect_expression_binding_target prototype.binding
          |> expect_postfix_expression ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let postfix_update_provenance () =
  let session, root, output =
    parse_string "#define UPDATE counter++\n_intern UPDATE I64 Generated();"
  in
  let postfix =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_postfix_expression
  in
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
      ("operand", Ast.expression_location postfix.postfix_operand);
      ("operator", postfix.postfix_operator.operator_location);
      ("postfix expression", postfix.postfix_location);
    ];
  let open Yojson.Safe.Util in
  let target_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated postfix operator provenance" true
    (target_json |> member "operator" |> member "location"
   |> member "generated_from" <> `Null)

let postfix_update_failures () =
  List.iter
    (fun (description, source, rejected_name, suffix) ->
      let session, _, output = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string)
        (description ^ " code") "HCPARSE0028" diagnostic.code;
      Alcotest.(check bool)
        (description ^ " message") true
        (contains diagnostic.message "must end the postfix chain");
      Alcotest.(check bool)
        (description ^ " identifies the following suffix")
        true
        (contains diagnostic.message suffix);
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) rejected_name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected postfix declaration %s became visible"
            rejected_name)
    [
      ( "call after postincrement",
        "_intern counter++() I64 Called();",
        "Called",
        "\"(\"" );
      ( "index after postdecrement",
        "_intern counter--[0] I64 Indexed();",
        "Indexed",
        "\"[\"" );
      ( "pointer member after postincrement",
        "_intern counter++->field I64 PointerMember();",
        "PointerMember",
        "\"->\"" );
      ( "direct member after postdecrement",
        "_intern counter--.field I64 DirectMember();",
        "DirectMember",
        "\".\"" );
      ( "second postincrement",
        "_intern counter++++ I64 Twice();",
        "Twice",
        "\"++\"" );
      ( "postdecrement after postincrement",
        "_intern counter++-- I64 Mixed();",
        "Mixed",
        "\"--\"" );
    ]

let deterministic_postfix_update_dumps () =
  let session, _, output =
    parse_string
      "_intern Factory().items[index].count++ + cursor-- I64 Updated();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human postfix dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON postfix dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump names postincrement" true
    (contains human "expression kind=postfix operator_kind=post_increment");
  let open Yojson.Safe.Util in
  let binary_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check string)
    "JSON root remains binary" "binary"
    (binary_json |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON left postfix kind" "post_increment"
    (binary_json |> member "left" |> member "operator_kind" |> to_string);
  Alcotest.(check string)
    "JSON right postfix kind" "post_decrement"
    (binary_json |> member "right" |> member "operator_kind" |> to_string)

let postfix_cast_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("postfix cast or function pointer dispatch", "//Typecast or fun_ptr");
      ( "prefix cast compatibility diagnostic",
        "Use TempleOS postfix typecasting at " );
      ("postfix cast parser mode", "mode=PRS0_TYPECAST|PRS1_NULL;");
      ( "postfix cast type parser",
        "tmpc=PrsType(cc,&tmpc,&mode,NULL,NULL,&fun_ptr,NULL,&tmpad2,0);" );
      ("postfix cast IC emission", "ICAdd(cc,IC_HOLYC_TYPECAST,was_paren,tmpc);");
      ("postfix cast returns to modifier parsing", "return PE_UNARY_MODIFIERS;");
    ];
  let type_parser = pinned "Compiler/PrsVar.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains type_parser fragment))
    [
      ("type parser rejects a non-type", "Invalid class at ");
      ("type parser counts pointer stars", "if (++ptr_stars_cnt>PTR_STARS_NUM)");
      ("type parser rejects excessive stars", "Too many *'s at ");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "postfix cast IC identity" true
    (contains compiler_header "#define IC_HOLYC_TYPECAST");
  Alcotest.(check bool)
    "postfix cast parser-mode identity" true
    (contains compiler_header "#define PRS0_TYPECAST");
  let initialization = pinned "Compiler/CInit.HC" in
  Alcotest.(check bool)
    "postfix cast IC metadata" true
    (contains initialization "\"HOLYC_TYPECAST\"");
  Alcotest.(check bool)
    "language guide requires postfix casts" true
    (contains (pinned "Doc/HolyC.DD") "Type casting is postfix.");
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ("Compiler/UAsm.HC", "disp=*rip(U8 *)++;");
      ("Adam/ADbg.HC", "doc_e->data=ptr(I8 *)++;");
      ("Kernel/BlkDev/DskCDDVD.HC", "buf[0](U32)=iso->id[0](U32);");
      ( "Demo/Graphics/Balloon.HC",
        "*(text.vga_alias(I64)+0x1000+(i+k)*640/8+j)(U8 *)=a[i*3+j];" );
    ]

let postfix_cast_types_and_shapes () =
  let cast_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_postfix_cast_expression
  in
  List.iteri
    (fun index primitive ->
      let spelling = Primitive_type.to_string primitive in
      let cast =
        cast_from
          (Printf.sprintf "_intern value(%s) I64 Cast%d();" spelling index)
      in
      Alcotest.(check bool)
        (spelling ^ " cast primitive")
        true
        (Primitive_type.equal primitive cast.cast_type.primitive);
      Alcotest.(check string)
        (spelling ^ " cast spelling")
        spelling cast.cast_type.spelling;
      Alcotest.(check int)
        (spelling ^ " has no pointer stars")
        0
        (List.length cast.cast_pointer_layers))
    Primitive_type.all;
  List.iter
    (fun depth ->
      let stars = String.make depth '*' in
      let cast =
        cast_from
          (Printf.sprintf "_intern value(U8%s) I64 Pointer%d();" stars depth)
      in
      Alcotest.(check (list int))
        (Printf.sprintf "pointer depth %d" depth)
        (List.init depth (fun index -> index + 1))
        (List.map
           (fun (layer : Ast.pointer_layer) -> layer.depth)
           cast.cast_pointer_layers))
    [ 0; 1; 2; 3; 4 ];
  let simple = cast_from "_intern value(I64) I64 Casted();" in
  (match simple.cast_operand with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string) "cast operand" "value" identifier.spelling
  | _ -> Alcotest.fail "expected an identifier cast operand");
  Alcotest.(check int)
    "opening parenthesis follows the operand" 13
    simple.cast_opening_parenthesis.span.start;
  Alcotest.(check int)
    "closing parenthesis is retained" 17
    simple.cast_closing_parenthesis.span.start;
  let _, _, call_output = parse_string "_intern Convert(value) I64 Called();" in
  ignore
    ( expect_ast call_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_call_expression )

let postfix_cast_chains_and_precedence () =
  let expression_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
  in
  let cast_after_call =
    expression_from "_intern Factory()(U8 *) I64 FromCall();"
    |> expect_postfix_cast_expression
  in
  ignore (expect_call_expression cast_after_call.cast_operand);
  let cast_after_index =
    expression_from "_intern values[0](I64) I64 FromIndex();"
    |> expect_postfix_cast_expression
  in
  ignore (expect_index_expression cast_after_index.cast_operand);
  let cast_after_member =
    expression_from "_intern object->field(U32) I64 FromMember();"
    |> expect_postfix_cast_expression
  in
  ignore (expect_member_expression cast_after_member.cast_operand);
  let _, _, dereference_output =
    parse_string "_intern *ptr(U8 *) I64 Dereferenced();"
  in
  let dereference =
    expect_ast dereference_output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_prefix_expression
  in
  Alcotest.(check bool)
    "dereference remains outside its cast operand" true
    (dereference.prefix_operator_kind = Ast.Dereference);
  ignore (expect_postfix_cast_expression dereference.prefix_operand);
  let repeated =
    expression_from "_intern value(U8 *)(I64 *) I64 Recast();"
    |> expect_postfix_cast_expression
  in
  ignore (expect_postfix_cast_expression repeated.cast_operand);
  let index_after_cast =
    expression_from "_intern value(U8 *)[0] I64 Indexed();"
    |> expect_index_expression
  in
  ignore (expect_postfix_cast_expression index_after_cast.index_base);
  let member_after_cast =
    expression_from "_intern value(U8 *).field I64 Member();"
    |> expect_member_expression
  in
  ignore (expect_postfix_cast_expression member_after_cast.member_base);
  let call_after_cast =
    expression_from "_intern callback(U0 *)() I64 Invoked();"
    |> expect_call_expression
  in
  ignore (expect_postfix_cast_expression call_after_cast.call_callee);
  let update_after_cast =
    expression_from "_intern ptr(U8 *)++ I64 Advanced();"
    |> expect_postfix_expression
  in
  ignore (expect_postfix_cast_expression update_after_cast.postfix_operand);
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let root =
        expression_from
          (Printf.sprintf "_intern value(I64) %s 2 I64 Cast%d();"
             operator.spelling index)
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " remains the binary root")
        operator.spelling root.binary_operator.operator_spelling;
      ignore (expect_postfix_cast_expression root.binary_left))
    Operator.binary_operators

let postfix_cast_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=raw(I64));"
  in
  ignore
    ( expect_ast default_output |> expect_one_prototype |> fun prototype ->
      List.hd prototype.parameters |> expect_parameter_default |> fun default ->
      expect_postfix_cast_expression (expect_default_expression default) );
  let _, _, dimension_output = parse_string "I64 values[count(I64)];" in
  ignore
    ( expect_ast dimension_output |> expect_one_global |> fun variable ->
      List.hd variable.array_dimensions
      |> expect_dimension_expression |> expect_postfix_cast_expression );
  let _, _, argument_output =
    parse_string "_intern Consume(raw(I64)) I64 Called();"
  in
  ignore
    ( expect_ast argument_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_call_expression
      |> fun call ->
      List.hd call.call_arguments
      |> expect_provided_call_argument |> expect_postfix_cast_expression );
  let _, _, index_output =
    parse_string "_intern table[raw(I64)] I64 Indexed();"
  in
  ignore
    ( expect_ast index_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_index_expression
      |> fun index -> expect_postfix_cast_expression index.index_value );
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode "_intern value(F64) I64 Selected();"
      in
      ignore
        ( expect_ast output |> expect_one_prototype |> fun prototype ->
          expect_expression_binding_target prototype.binding
          |> expect_postfix_cast_expression ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let postfix_cast_provenance () =
  let session, root, output =
    parse_string "#define CAST (U8 *)\n_intern ptr CAST I64 Generated();"
  in
  let cast =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_postfix_cast_expression
  in
  let pointer = List.hd cast.cast_pointer_layers in
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
      ("opening parenthesis", cast.cast_opening_parenthesis);
      ("target type", cast.cast_type.location);
      ("pointer star", pointer.location);
      ("closing parenthesis", cast.cast_closing_parenthesis);
    ];
  Alcotest.(check bool)
    "combined cast location retains generated source segments" true
    (List.length cast.cast_location.source_segments > 1);
  let open Yojson.Safe.Util in
  let cast_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated cast-type provenance" true
    (cast_json |> member "target_type" |> member "location"
   |> member "generated_from" <> `Null);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "cast.HC" in
      write_file root_file "#include \"cast\"";
      write_file declaration_file "_intern value(I64) I64 Included();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_cast =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_postfix_cast_expression
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          included_cast.cast_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included cast keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let postfix_cast_failures () =
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
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) rejected_name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected postfix-cast declaration %s became visible"
            rejected_name)
    [
      ( "C-style prefix cast",
        "_intern (I64)value I64 Prefix();",
        "Prefix",
        "HCPARSE0029",
        "write the cast after its operand" );
      ( "unclosed postfix cast",
        "_intern value(I64 I64 Unclosed();",
        "Unclosed",
        "HCPARSE0030",
        "expected ')' to close postfix cast" );
      ( "token after primitive target",
        "_intern value(I64 extra) I64 Malformed();",
        "Malformed",
        "HCPARSE0030",
        "identifier \"extra\"" );
      ( "fifth pointer star",
        "_intern value(U8 *****) I64 TooDeep();",
        "TooDeep",
        "HCPARSE0004",
        "at most 4 pointer stars" );
      ( "cast after terminal update",
        "_intern value++(I64) I64 TooLate();",
        "TooLate",
        "HCPARSE0028",
        "must end the postfix chain" );
    ]

let deterministic_postfix_cast_dumps () =
  let session, _, output =
    parse_string
      "_intern Factory().items[0](U8 *)(I64 *)++ + cursor(F64) I64 Casted();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human postfix-cast dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON postfix-cast dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump names the cast target" true
    (contains human "expression kind=postfix_cast");
  let open Yojson.Safe.Util in
  let binary_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  let outer_left_cast = binary_json |> member "left" |> member "operand" in
  Alcotest.(check string)
    "JSON left update wraps a cast" "postfix_cast"
    (outer_left_cast |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON outer cast target" "I64"
    (outer_left_cast |> member "target_type" |> member "primitive" |> to_string);
  let inner_cast = outer_left_cast |> member "operand" in
  Alcotest.(check string)
    "JSON repeated cast target" "U8"
    (inner_cast |> member "target_type" |> member "primitive" |> to_string);
  Alcotest.(check int)
    "JSON repeated cast keeps one pointer layer" 1
    (inner_cast |> member "pointer_layers" |> to_list |> List.length);
  Alcotest.(check string)
    "JSON right cast target" "F64"
    (binary_json |> member "right" |> member "target_type" |> member "primitive"
   |> to_string)

let sizeof_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("sizeof keyword dispatch", "case KW_SIZEOF:");
      ("sizeof has term precedence", "if (PREC_TERM>*max_prec)");
      ("sizeof accepts wrapper parentheses", "while (Lex(cc)=='(')");
      ("sizeof delegates its named target", "PrsSizeOf(cc);");
      ("sizeof requires matching wrappers", "LexExcept(cc,\"Missing ')' at ");
      ("sizeof target must be an identifier", "if (cc->token!=TK_IDENT)");
      ( "sizeof accepts compiler object kinds",
        "HTT_CLASS|HTT_INTERNAL_TYPE|HTT_GLBL_VAR|" );
      ("sizeof follows repeated members", "while (Lex(cc)=='.') {");
      ("sizeof recognizes a pointer target", "if (cc->token=='*') {");
      ("sizeof consumes every target star", "while (Lex(cc)=='*');");
      ("sizeof uses pointer size after a star", "i=sizeof(U8 *);");
      ( "sizeof emits an immediate result",
        "ICAdd(cc,IC_IMM_I64,i,cmp.internal_types[RT_I64]);" );
      ("maybe-modifiers state is explicit", "case PE_MAYBE_MODIFIERS:");
      ( "maybe-modifiers dispatches only on a parenthesis",
        "if (cc->token=='(') { //Typecast or fun_ptr" );
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "sizeof keyword identity" true
    (contains compiler_header "#define KW_SIZEOF\t13");
  Alcotest.(check bool)
    "sizeof IC identity" true
    (contains compiler_header "#define IC_SIZEOF");
  Alcotest.(check bool)
    "sizeof IC metadata" true
    (contains (pinned "Compiler/CInit.HC") "\"SIZEOF\"");
  let language_guide = pinned "Doc/HolyC.DD" in
  Alcotest.(check bool)
    "language guide documents the one-member claim" true
    (contains language_guide "only accept one level of member vars");
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ("Compiler/PrsStmt.HC", "j=sizeof(U8 *);");
      ("Kernel/MultiProc.HC", "sizeof(CCPU.start_stk)");
      ("Adam/ADefine.HC", "sizeof(CBinFile)");
      ("Demo/GlblVars.HC", "D(g1,sizeof(g1));");
    ]

let sizeof_shapes () =
  let sizeof_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_sizeof_expression
  in
  List.iteri
    (fun index primitive ->
      let spelling = Primitive_type.to_string primitive in
      let sizeof_expression =
        sizeof_from
          (Printf.sprintf "_intern sizeof(%s) I64 Size%d();" spelling index)
      in
      Alcotest.(check string)
        (spelling ^ " target spelling")
        spelling sizeof_expression.sizeof_target.spelling;
      Alcotest.(check int)
        (spelling ^ " wrapper count")
        1
        (List.length sizeof_expression.sizeof_opening_parentheses))
    Primitive_type.all;
  let bare = sizeof_from "_intern sizeof I64 I64 Bare();" in
  Alcotest.(check string)
    "bare keyword spelling" "sizeof" bare.sizeof_keyword_spelling;
  Alcotest.(check string) "bare target" "I64" bare.sizeof_target.spelling;
  Alcotest.(check int)
    "bare form has no wrapper" 0
    (List.length bare.sizeof_opening_parentheses);
  let wrapped = sizeof_from "_intern sizeof(((I64))) I64 Wrapped();" in
  Alcotest.(check int)
    "three opening wrappers" 3
    (List.length wrapped.sizeof_opening_parentheses);
  Alcotest.(check int)
    "three closing wrappers" 3
    (List.length wrapped.sizeof_closing_parentheses);
  let members =
    sizeof_from "_intern sizeof(Root.first.second) I64 Members();"
  in
  Alcotest.(check string) "member root" "Root" members.sizeof_target.spelling;
  Alcotest.(check (list string))
    "source loop retains repeated members" [ "first"; "second" ]
    (List.map
       (fun (member : Ast.sizeof_member) -> member.sizeof_member_name.spelling)
       members.sizeof_members);
  List.iter
    (fun depth ->
      let stars = String.make depth '*' in
      let sizeof_expression =
        sizeof_from
          (Printf.sprintf "_intern sizeof(U8%s) I64 Pointer%d();" stars depth)
      in
      Alcotest.(check (list int))
        (Printf.sprintf "sizeof pointer depth %d" depth)
        (List.init depth (fun index -> index + 1))
        (List.map
           (fun (layer : Ast.pointer_layer) -> layer.depth)
           sizeof_expression.sizeof_pointer_layers))
    [ 0; 1; 4; 5 ];
  let bare_pointer = sizeof_from "_intern sizeof U8 * I64 BarePointer();" in
  Alcotest.(check int)
    "unparenthesized star belongs to sizeof" 1
    (List.length bare_pointer.sizeof_pointer_layers)

let sizeof_postfix_and_precedence () =
  let expression_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
  in
  let cast =
    expression_from "_intern sizeof(I64)(U64) I64 Casted();"
    |> expect_postfix_cast_expression
  in
  ignore (expect_sizeof_expression cast.cast_operand);
  let called =
    expression_from "_intern sizeof(I64)(U0 *)() I64 Called();"
    |> expect_call_expression
  in
  ignore
    ( called.call_callee |> expect_postfix_cast_expression |> fun cast ->
      expect_sizeof_expression cast.cast_operand );
  let indexed =
    expression_from "_intern sizeof(I64)(U8 *)[0] I64 Indexed();"
    |> expect_index_expression
  in
  ignore
    ( indexed.index_base |> expect_postfix_cast_expression |> fun cast ->
      expect_sizeof_expression cast.cast_operand );
  let member =
    expression_from "_intern sizeof(I64)(U8 *).value I64 Member();"
    |> expect_member_expression
  in
  ignore
    ( member.member_base |> expect_postfix_cast_expression |> fun cast ->
      expect_sizeof_expression cast.cast_operand );
  let update =
    expression_from "_intern sizeof(I64)(I64)++ I64 Updated();"
    |> expect_postfix_expression
  in
  ignore
    ( update.postfix_operand |> expect_postfix_cast_expression |> fun cast ->
      expect_sizeof_expression cast.cast_operand );
  let prefix =
    expression_from "_intern -sizeof(I64) I64 Negated();"
    |> expect_prefix_expression
  in
  ignore (expect_sizeof_expression prefix.prefix_operand);
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let root =
        expression_from
          (Printf.sprintf "_intern sizeof(I64) %s 2 I64 Binary%d();"
             operator.spelling index)
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " remains the binary root")
        operator.spelling root.binary_operator.operator_spelling;
      ignore (expect_sizeof_expression root.binary_left))
    Operator.binary_operators;
  let multiplication =
    expression_from "_intern sizeof(U8) * 2 I64 Multiplied();"
    |> expect_binary_expression
  in
  Alcotest.(check string)
    "a star after a closing wrapper is multiplication" "*"
    multiplication.binary_operator.operator_spelling;
  ignore (expect_sizeof_expression multiplication.binary_left)

let sizeof_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=sizeof(I64));"
  in
  ignore
    ( expect_ast default_output |> expect_one_prototype |> fun prototype ->
      List.hd prototype.parameters |> expect_parameter_default |> fun default ->
      expect_sizeof_expression (expect_default_expression default) );
  let _, _, dimension_output = parse_string "I64 values[sizeof(I64)];" in
  ignore
    ( expect_ast dimension_output |> expect_one_global |> fun variable ->
      List.hd variable.array_dimensions
      |> expect_dimension_expression |> expect_sizeof_expression );
  let _, _, argument_output =
    parse_string "_intern Consume(sizeof(I64)) I64 Called();"
  in
  ignore
    ( expect_ast argument_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_call_expression
      |> fun call ->
      List.hd call.call_arguments
      |> expect_provided_call_argument |> expect_sizeof_expression );
  let _, _, index_output =
    parse_string "_intern table[sizeof(I64)] I64 Indexed();"
  in
  ignore
    ( expect_ast index_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_index_expression
      |> fun index -> expect_sizeof_expression index.index_value );
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode
          "_intern sizeof(Packet.header) I64 Selected();"
      in
      ignore
        ( expect_ast output |> expect_one_prototype |> fun prototype ->
          expect_expression_binding_target prototype.binding
          |> expect_sizeof_expression ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let sizeof_provenance () =
  let source =
    "#define SIZE sizeof\n\
     #define OPEN (\n\
     #define ROOT Packet\n\
     #define DOT .\n\
     #define MEMBER header\n\
     #define STAR *\n\
     #define CLOSE )\n\
     _intern SIZE OPEN ROOT DOT MEMBER STAR CLOSE I64 Generated();"
  in
  let session, root, output = parse_string source in
  let sizeof_expression =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_sizeof_expression
  in
  let member = List.hd sizeof_expression.sizeof_members in
  let pointer = List.hd sizeof_expression.sizeof_pointer_layers in
  let opening = List.hd sizeof_expression.sizeof_opening_parentheses in
  let closing = List.hd sizeof_expression.sizeof_closing_parentheses in
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
      ("keyword", sizeof_expression.sizeof_keyword_location);
      ("opening parenthesis", opening);
      ("target", sizeof_expression.sizeof_target.location);
      ("member dot", member.sizeof_member_dot);
      ("member name", member.sizeof_member_name.location);
      ("pointer star", pointer.location);
      ("closing parenthesis", closing);
    ];
  Alcotest.(check bool)
    "combined sizeof location retains generated segments" true
    (List.length sizeof_expression.sizeof_location.source_segments > 1);
  let open Yojson.Safe.Util in
  let sizeof_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated keyword provenance" true
    (sizeof_json |> member "keyword" |> member "location"
   |> member "generated_from" |> Yojson.Safe.to_string |> String.equal "null"
   |> not);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "sizeof.HC" in
      write_file root_file "#include \"sizeof\"";
      write_file declaration_file
        "_intern sizeof(Packet.header) I64 Included();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_sizeof =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_sizeof_expression
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          included_sizeof.sizeof_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included sizeof keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let sizeof_failures () =
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
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) rejected_name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected sizeof declaration %s became visible"
            rejected_name)
    [
      ( "empty target",
        "_intern sizeof() I64 Empty();",
        "Empty",
        "HCPARSE0031",
        "expected a named sizeof target" );
      ( "literal target",
        "_intern sizeof(1) I64 Literal();",
        "Literal",
        "HCPARSE0031",
        "found \"1\"" );
      ( "missing member name",
        "_intern sizeof(Packet.) I64 MissingMember();",
        "MissingMember",
        "HCPARSE0032",
        "expected a member name after '.'" );
      ( "unclosed target",
        "_intern sizeof(Packet I64 Unclosed();",
        "Unclosed",
        "HCPARSE0033",
        "one wrapper parenthesis remains" );
      ( "unclosed outer wrapper",
        "_intern sizeof(((I64)) I64 Outer();",
        "Outer",
        "HCPARSE0033",
        "one wrapper parenthesis remains" );
      ( "arbitrary expression target",
        "_intern sizeof(value+1) I64 Expression();",
        "Expression",
        "HCPARSE0033",
        "found \"+\"" );
      ( "missing bare target",
        "_intern sizeof + 1 I64 Missing();",
        "Missing",
        "HCPARSE0031",
        "found \"+\"" );
      ( "direct call suffix",
        "_intern sizeof(I64)() I64 Called();",
        "Called",
        "HCPARSE0034",
        "expected a postfix cast target after sizeof" );
      ( "direct index suffix",
        "_intern sizeof(I64)[0] I64 Indexed();",
        "Indexed",
        "HCPARSE0034",
        "cannot be followed directly by \"[\"" );
      ( "direct member suffix",
        "_intern sizeof(I64).value I64 Member();",
        "Member",
        "HCPARSE0034",
        "cannot be followed directly by \".\"" );
      ( "direct pointer-member suffix",
        "_intern sizeof(I64)->value I64 PointerMember();",
        "PointerMember",
        "HCPARSE0034",
        "cannot be followed directly by \"->\"" );
      ( "direct increment suffix",
        "_intern sizeof(I64)++ I64 Incremented();",
        "Incremented",
        "HCPARSE0034",
        "cannot be followed directly by \"++\"" );
      ( "direct decrement suffix",
        "_intern sizeof(I64)-- I64 Decremented();",
        "Decremented",
        "HCPARSE0034",
        "cannot be followed directly by \"--\"" );
      ( "nonprimitive cast target",
        "_intern sizeof(I64)(Widget) I64 Casted();",
        "Casted",
        "HCPARSE0020",
        "nonprimitive postfix cast target" );
    ]

let deterministic_sizeof_dumps () =
  let session, _, output =
    parse_string
      "_intern sizeof(((Packet.header *****)))(U64) + sizeof table(U8 *)[0] \
       I64 Measured();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human sizeof dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON sizeof dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump names wrapper and pointer counts" true
    (contains human
       "expression kind=sizeof wrappers=3 members=1 pointer_layers=5");
  let open Yojson.Safe.Util in
  let binary_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  let left_sizeof = binary_json |> member "left" |> member "operand" in
  Alcotest.(check string)
    "JSON left cast wraps sizeof" "sizeof"
    (left_sizeof |> member "kind" |> to_string);
  Alcotest.(check int)
    "JSON keeps three opening wrappers" 3
    (left_sizeof |> member "opening_parentheses" |> to_list |> List.length);
  Alcotest.(check int)
    "JSON keeps five pointer layers" 5
    (left_sizeof |> member "pointer_layers" |> to_list |> List.length);
  Alcotest.(check string)
    "JSON right index base wraps sizeof in a cast" "sizeof"
    (binary_json |> member "right" |> member "base" |> member "operand"
   |> member "kind" |> to_string)

let offset_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("offset parser is a distinct routine", "U0 PrsOffsetOf(CCmpCtrl *cc)");
      ("offset target must be an identifier", "if (cc->token!=TK_IDENT)");
      ("offset accepts a local variable", "if (tmpm=cc->local_var_entry)");
      ( "offset accepts classes and globals",
        "tmpc->type & (HTT_CLASS|HTT_GLBL_VAR)" );
      ("offset requires a member dot", "if (Lex(cc)!='.')");
      ("offset starts at zero", "i=0;");
      ("offset adds each member offset", "i+=tmpm->offset;");
      ("offset follows the member class", "tmpc=tmpm->member_class;");
      ("offset follows repeated members", "} while (Lex(cc)=='.');");
      ( "offset emits an immediate result",
        "ICAdd(cc,IC_IMM_I64,i,cmp.internal_types[RT_I64]);" );
      ("offset keyword dispatch", "case KW_OFFSET:");
      ("offset accepts wrapper parentheses", "while (Lex(cc)=='(')");
      ("offset delegates its named target", "PrsOffsetOf(cc);");
      ( "offset returns the restricted modifier state",
        "return PE_MAYBE_MODIFIERS;" );
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "offset keyword identity" true
    (contains compiler_header "#define KW_OFFSET\t26");
  Alcotest.(check bool)
    "language guide documents the one-member claim" true
    (contains (pinned "Doc/HolyC.DD") "only accept one level of member vars");
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool) path true (contains (pinned path) fragment))
    [
      ("Compiler/PrsVar.HC", "offset(CVI2.base)");
      ("Kernel/MultiProc.HC", "offset(CGDT.tr)");
      ("Adam/AMem.HC", "offset(CMemUsed.next)");
    ]

let offset_shapes () =
  let offset_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_offset_expression
  in
  let bare = offset_from "_intern offset Packet.header I64 Bare();" in
  Alcotest.(check string)
    "bare keyword spelling" "offset" bare.offset_keyword_spelling;
  Alcotest.(check string) "bare target" "Packet" bare.offset_target.spelling;
  Alcotest.(check int)
    "bare form has no wrapper" 0
    (List.length bare.offset_opening_parentheses);
  Alcotest.(check (list string))
    "bare form retains its required member" [ "header" ]
    (List.map
       (fun (member : Ast.offset_member) -> member.offset_member_name.spelling)
       bare.offset_members);
  let wrapped =
    offset_from "_intern offset(((Packet.header))) I64 Wrapped();"
  in
  Alcotest.(check int)
    "three opening wrappers" 3
    (List.length wrapped.offset_opening_parentheses);
  Alcotest.(check int)
    "three closing wrappers" 3
    (List.length wrapped.offset_closing_parentheses);
  let members =
    offset_from "_intern offset(Root.first.second.third) I64 Members();"
  in
  Alcotest.(check (list string))
    "executable source loop retains repeated members"
    [ "first"; "second"; "third" ]
    (List.map
       (fun (member : Ast.offset_member) -> member.offset_member_name.spelling)
       members.offset_members)

let offset_postfix_and_precedence () =
  let expression_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
  in
  let cast =
    expression_from "_intern offset(Packet.field)(U64) I64 Casted();"
    |> expect_postfix_cast_expression
  in
  ignore (expect_offset_expression cast.cast_operand);
  let called =
    expression_from "_intern offset(Packet.field)(U0 *)() I64 Called();"
    |> expect_call_expression
  in
  ignore
    ( called.call_callee |> expect_postfix_cast_expression |> fun cast ->
      expect_offset_expression cast.cast_operand );
  let indexed =
    expression_from "_intern offset(Packet.field)(U8 *)[0] I64 Indexed();"
    |> expect_index_expression
  in
  ignore
    ( indexed.index_base |> expect_postfix_cast_expression |> fun cast ->
      expect_offset_expression cast.cast_operand );
  let member =
    expression_from "_intern offset(Packet.field)(U8 *).value I64 Member();"
    |> expect_member_expression
  in
  ignore
    ( member.member_base |> expect_postfix_cast_expression |> fun cast ->
      expect_offset_expression cast.cast_operand );
  let update =
    expression_from "_intern offset(Packet.field)(I64)++ I64 Updated();"
    |> expect_postfix_expression
  in
  ignore
    ( update.postfix_operand |> expect_postfix_cast_expression |> fun cast ->
      expect_offset_expression cast.cast_operand );
  let prefix =
    expression_from "_intern -offset(Packet.field) I64 Negated();"
    |> expect_prefix_expression
  in
  ignore (expect_offset_expression prefix.prefix_operand);
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let root =
        expression_from
          (Printf.sprintf "_intern offset(Packet.field) %s 2 I64 Binary%d();"
             operator.spelling index)
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " remains the binary root")
        operator.spelling root.binary_operator.operator_spelling;
      ignore (expect_offset_expression root.binary_left))
    Operator.binary_operators;
  let multiplication =
    expression_from "_intern offset(Packet.field) * 2 I64 Multiplied();"
    |> expect_binary_expression
  in
  Alcotest.(check string)
    "a star after offset is multiplication" "*"
    multiplication.binary_operator.operator_spelling;
  ignore (expect_offset_expression multiplication.binary_left)

let offset_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=offset(Packet.field));"
  in
  ignore
    ( expect_ast default_output |> expect_one_prototype |> fun prototype ->
      List.hd prototype.parameters |> expect_parameter_default |> fun default ->
      expect_offset_expression (expect_default_expression default) );
  let _, _, dimension_output =
    parse_string "I64 values[offset(Packet.field)];"
  in
  ignore
    ( expect_ast dimension_output |> expect_one_global |> fun variable ->
      List.hd variable.array_dimensions
      |> expect_dimension_expression |> expect_offset_expression );
  let _, _, argument_output =
    parse_string "_intern Consume(offset(Packet.field)) I64 Called();"
  in
  ignore
    ( expect_ast argument_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_call_expression
      |> fun call ->
      List.hd call.call_arguments
      |> expect_provided_call_argument |> expect_offset_expression );
  let _, _, index_output =
    parse_string "_intern table[offset(Packet.field)] I64 Indexed();"
  in
  ignore
    ( expect_ast index_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_index_expression
      |> fun index -> expect_offset_expression index.index_value );
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode
          "_intern offset(Packet.header) I64 Selected();"
      in
      ignore
        ( expect_ast output |> expect_one_prototype |> fun prototype ->
          expect_expression_binding_target prototype.binding
          |> expect_offset_expression ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let offset_provenance () =
  let source =
    "#define OFFSET offset\n\
     #define OPEN (\n\
     #define ROOT Packet\n\
     #define DOT .\n\
     #define FIRST header\n\
     #define SECOND payload\n\
     #define CLOSE )\n\
     _intern OFFSET OPEN ROOT DOT FIRST DOT SECOND CLOSE I64 Generated();"
  in
  let session, root, output = parse_string source in
  let offset_expression =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_offset_expression
  in
  let first_member = List.hd offset_expression.offset_members in
  let second_member = List.nth offset_expression.offset_members 1 in
  let opening = List.hd offset_expression.offset_opening_parentheses in
  let closing = List.hd offset_expression.offset_closing_parentheses in
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
      ("keyword", offset_expression.offset_keyword_location);
      ("opening parenthesis", opening);
      ("target", offset_expression.offset_target.location);
      ("first member dot", first_member.offset_member_dot);
      ("first member name", first_member.offset_member_name.location);
      ("second member name", second_member.offset_member_name.location);
      ("closing parenthesis", closing);
    ];
  Alcotest.(check bool)
    "combined offset location retains generated segments" true
    (List.length offset_expression.offset_location.source_segments > 1);
  let open Yojson.Safe.Util in
  let offset_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated keyword provenance" true
    (offset_json |> member "keyword" |> member "location"
   |> member "generated_from" <> `Null);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "offset.HC" in
      write_file root_file "#include \"offset\"";
      write_file declaration_file
        "_intern offset(Packet.header) I64 Included();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_offset =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_offset_expression
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          included_offset.offset_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included offset keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let offset_failures () =
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
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) rejected_name
      with
      | Symbol_visibility.Absent -> ()
      | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "rejected offset declaration %s became visible"
            rejected_name)
    [
      ( "empty target",
        "_intern offset() I64 Empty();",
        "Empty",
        "HCPARSE0035",
        "expected a named offset target" );
      ( "literal target",
        "_intern offset(1) I64 Literal();",
        "Literal",
        "HCPARSE0035",
        "found \"1\"" );
      ( "missing required member",
        "_intern offset(Packet) I64 MissingMember();",
        "MissingMember",
        "HCPARSE0036",
        "expected '.' after named offset target" );
      ( "missing member name",
        "_intern offset(Packet.) I64 MissingName();",
        "MissingName",
        "HCPARSE0037",
        "expected a member name after '.'" );
      ( "unclosed target",
        "_intern offset(Packet.field I64 Unclosed();",
        "Unclosed",
        "HCPARSE0038",
        "one wrapper parenthesis remains" );
      ( "pointer target",
        "_intern offset(Packet.field*) I64 Pointer();",
        "Pointer",
        "HCPARSE0038",
        "found \"*\"" );
      ( "direct call suffix",
        "_intern offset(Packet.field)() I64 Called();",
        "Called",
        "HCPARSE0039",
        "expected a postfix cast target after offset" );
      ( "direct index suffix",
        "_intern offset(Packet.field)[0] I64 Indexed();",
        "Indexed",
        "HCPARSE0039",
        "cannot be followed directly by \"[\"" );
      ( "direct member suffix",
        "_intern offset(Packet.field).value I64 Member();",
        "Member",
        "HCPARSE0039",
        "cannot be followed directly by \".\"" );
      ( "direct pointer-member suffix",
        "_intern offset(Packet.field)->value I64 PointerMember();",
        "PointerMember",
        "HCPARSE0039",
        "cannot be followed directly by \"->\"" );
      ( "direct increment suffix",
        "_intern offset(Packet.field)++ I64 Incremented();",
        "Incremented",
        "HCPARSE0039",
        "cannot be followed directly by \"++\"" );
      ( "direct decrement suffix",
        "_intern offset(Packet.field)-- I64 Decremented();",
        "Decremented",
        "HCPARSE0039",
        "cannot be followed directly by \"--\"" );
      ( "nonprimitive cast target",
        "_intern offset(Packet.field)(Widget) I64 Casted();",
        "Casted",
        "HCPARSE0020",
        "nonprimitive postfix cast target" );
    ]

let deterministic_offset_dumps () =
  let session, _, output =
    parse_string
      "_intern offset(((Packet.header.entry)))(U64) + offset table.item(U8 \
       *)[0] I64 Measured();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human offset dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON offset dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump names wrapper and member counts" true
    (contains human "expression kind=offset wrappers=3 members=2");
  let open Yojson.Safe.Util in
  let binary_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  let left_offset = binary_json |> member "left" |> member "operand" in
  Alcotest.(check string)
    "JSON left cast wraps offset" "offset"
    (left_offset |> member "kind" |> to_string);
  Alcotest.(check int)
    "JSON keeps three opening wrappers" 3
    (left_offset |> member "opening_parentheses" |> to_list |> List.length);
  Alcotest.(check int)
    "JSON keeps two member segments" 2
    (left_offset |> member "members" |> to_list |> List.length);
  Alcotest.(check string)
    "JSON right index base wraps offset in a cast" "offset"
    (binary_json |> member "right" |> member "base" |> member "operand"
   |> member "kind" |> to_string)

let defined_source_behavior () =
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains expression_parser fragment))
    [
      ("defined keyword dispatch", "case KW_DEFINED:");
      ("defined accepts wrapper parentheses", "while (Lex(cc)=='(')");
      ("defined checks an identifier token", "if (cc->token==TK_IDENT &&");
      ( "defined checks global and local lookup",
        "(cc->hash_entry || cc->local_var_entry))" );
      ( "defined emits true as I64",
        "ICAdd(cc,IC_IMM_I64,TRUE,cmp.internal_types[RT_I64]);" );
      ( "defined emits false as I64",
        "ICAdd(cc,IC_IMM_I64,FALSE,cmp.internal_types[RT_I64]);" );
      ( "defined returns the restricted modifier state",
        "return PE_MAYBE_MODIFIERS;" );
    ];
  Alcotest.(check bool)
    "defined keyword identity" true
    (contains (pinned "Compiler/CompilerA.HH") "#define KW_DEFINED\t43");
  Alcotest.(check bool)
    "keywords remain identifier tokens" true
    (contains
       (pinned "Compiler/PrsLib.HC")
       "cc->token==TK_IDENT &&(tmph=cc->hash_entry) && tmph->type&HTT_KEYWORD");
  Alcotest.(check bool)
    "the lexer emits identifier tokens" true
    (contains (pinned "Compiler/Lex.HC") "cc->token=TK_IDENT;");
  Alcotest.(check bool)
    "preprocessor documentation names its separate defined function" true
    (contains
       (pinned "Doc/PreProcessor.DD")
       "$FG,2$defined()$FG$\tIs a function that can be used in expressions.")

let defined_shapes () =
  let defined_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_defined_expression
  in
  let bare = defined_from "_intern defined Missing I64 Bare();" in
  Alcotest.(check string)
    "bare keyword spelling" "defined" bare.defined_keyword_spelling;
  Alcotest.(check int)
    "bare form has no wrapper" 0
    (List.length bare.defined_opening_parentheses);
  Alcotest.(check string)
    "bare operand" "Missing" bare.defined_operand.defined_operand_spelling;
  Alcotest.(check bool)
    "an unresolved-looking identifier keeps the name shape" true
    (bare.defined_operand.defined_operand_kind = Ast.Defined_name);
  let wrapped = defined_from "_intern defined(((Known))) I64 Wrapped();" in
  Alcotest.(check int)
    "three opening wrappers" 3
    (List.length wrapped.defined_opening_parentheses);
  Alcotest.(check int)
    "three closing wrappers" 3
    (List.length wrapped.defined_closing_parentheses);
  let keyword = defined_from "_intern defined(I64) I64 Keyword();" in
  Alcotest.(check bool)
    "a language keyword has the pinned identifier shape" true
    (keyword.defined_operand.defined_operand_kind = Ast.Defined_name);
  Alcotest.(check string)
    "keyword spelling is retained" "I64"
    keyword.defined_operand.defined_operand_spelling;
  let literal = defined_from "_intern defined(42) I64 Literal();" in
  Alcotest.(check bool)
    "an integer is retained as a non-name token" true
    (literal.defined_operand.defined_operand_kind = Ast.Defined_non_name);
  Alcotest.(check string)
    "integer spelling is retained" "42"
    literal.defined_operand.defined_operand_spelling;
  let closing = defined_from "_intern defined()) I64 ClosingToken();" in
  Alcotest.(check bool)
    "the source-shaped operand may be a closing parenthesis" true
    (closing.defined_operand.defined_operand_kind = Ast.Defined_non_name);
  Alcotest.(check string)
    "the consumed closing token remains distinct from the wrapper close" ")"
    closing.defined_operand.defined_operand_spelling;
  Alcotest.(check int)
    "one later parenthesis closes the wrapper" 1
    (List.length closing.defined_closing_parentheses)

let defined_postfix_and_precedence () =
  let expression_from source =
    let _, _, output = parse_string source in
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
  in
  let cast =
    expression_from "_intern defined(Name)(U64) I64 Casted();"
    |> expect_postfix_cast_expression
  in
  ignore (expect_defined_expression cast.cast_operand);
  let called =
    expression_from "_intern defined(Name)(U0 *)() I64 Called();"
    |> expect_call_expression
  in
  ignore
    ( called.call_callee |> expect_postfix_cast_expression |> fun cast ->
      expect_defined_expression cast.cast_operand );
  let indexed =
    expression_from "_intern defined(Name)(U8 *)[0] I64 Indexed();"
    |> expect_index_expression
  in
  ignore
    ( indexed.index_base |> expect_postfix_cast_expression |> fun cast ->
      expect_defined_expression cast.cast_operand );
  let member =
    expression_from "_intern defined(Name)(U8 *).value I64 Member();"
    |> expect_member_expression
  in
  ignore
    ( member.member_base |> expect_postfix_cast_expression |> fun cast ->
      expect_defined_expression cast.cast_operand );
  let update =
    expression_from "_intern defined(Name)(I64)++ I64 Updated();"
    |> expect_postfix_expression
  in
  ignore
    ( update.postfix_operand |> expect_postfix_cast_expression |> fun cast ->
      expect_defined_expression cast.cast_operand );
  let prefix =
    expression_from "_intern -defined(Name) I64 Negated();"
    |> expect_prefix_expression
  in
  ignore (expect_defined_expression prefix.prefix_operand);
  List.iteri
    (fun index (operator : Operator.binary_operator) ->
      let root =
        expression_from
          (Printf.sprintf "_intern defined(Name) %s 2 I64 Binary%d();"
             operator.spelling index)
        |> expect_binary_expression
      in
      Alcotest.(check string)
        (operator.spelling ^ " remains the binary root")
        operator.spelling root.binary_operator.operator_spelling;
      ignore (expect_defined_expression root.binary_left))
    Operator.binary_operators;
  let multiplication =
    expression_from "_intern defined(Name) * 2 I64 Multiplied();"
    |> expect_binary_expression
  in
  Alcotest.(check string)
    "a star after defined is multiplication" "*"
    multiplication.binary_operator.operator_spelling;
  ignore (expect_defined_expression multiplication.binary_left)

let defined_contexts_and_modes () =
  let _, _, default_output =
    parse_string "extern U0 Defaults(I64 value=defined(Name));"
  in
  ignore
    ( expect_ast default_output |> expect_one_prototype |> fun prototype ->
      List.hd prototype.parameters |> expect_parameter_default |> fun default ->
      expect_defined_expression (expect_default_expression default) );
  let _, _, dimension_output = parse_string "I64 values[defined(Name)];" in
  ignore
    ( expect_ast dimension_output |> expect_one_global |> fun variable ->
      List.hd variable.array_dimensions
      |> expect_dimension_expression |> expect_defined_expression );
  let _, _, argument_output =
    parse_string "_intern Consume(defined(Name)) I64 Called();"
  in
  ignore
    ( expect_ast argument_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_call_expression
      |> fun call ->
      List.hd call.call_arguments
      |> expect_provided_call_argument |> expect_defined_expression );
  let _, _, index_output =
    parse_string "_intern table[defined(Name)] I64 Indexed();"
  in
  ignore
    ( expect_ast index_output |> expect_one_prototype |> fun prototype ->
      expect_expression_binding_target prototype.binding
      |> expect_index_expression
      |> fun index -> expect_defined_expression index.index_value );
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode
          "_intern defined(Name) I64 Selected();"
      in
      ignore
        ( expect_ast output |> expect_one_prototype |> fun prototype ->
          expect_expression_binding_target prototype.binding
          |> expect_defined_expression ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let defined_provenance () =
  let source =
    "#define DEFINED defined\n\
     #define OPEN (\n\
     #define TARGET Missing\n\
     #define CLOSE )\n\
     _intern DEFINED OPEN TARGET CLOSE I64 Generated();"
  in
  let session, root, output = parse_string source in
  let defined_expression =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    expect_expression_binding_target prototype.binding
    |> expect_defined_expression
  in
  let opening = List.hd defined_expression.defined_opening_parentheses in
  let closing = List.hd defined_expression.defined_closing_parentheses in
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
      ("keyword", defined_expression.defined_keyword_location);
      ("opening parenthesis", opening);
      ("operand", defined_expression.defined_operand.defined_operand_location);
      ("closing parenthesis", closing);
    ];
  Alcotest.(check bool)
    "combined defined location retains generated segments" true
    (List.length defined_expression.defined_location.source_segments > 1);
  let open Yojson.Safe.Util in
  let defined_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  Alcotest.(check bool)
    "JSON keeps generated operand provenance" true
    (defined_json |> member "operand" |> member "location"
   |> member "generated_from" <> `Null);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "defined.HC" in
      write_file root_file "#include \"defined\"";
      write_file declaration_file "_intern defined(Name) I64 Included();";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_defined =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        expect_expression_binding_target prototype.binding
        |> expect_defined_expression
      in
      let expression_source =
        Source_manager.find
          (Session.sources include_session)
          included_defined.defined_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included defined term keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let defined_failures () =
  let check_failure description source code message_fragment =
    let _, _, output = parse_string source in
    Alcotest.(check bool)
      (description ^ " has no AST")
      true
      (Option.is_none output.ast);
    let diagnostic = first_diagnostic output in
    Alcotest.(check string) (description ^ " code") code diagnostic.code;
    Alcotest.(check bool)
      (description ^ " message") true
      (contains diagnostic.message message_fragment)
  in
  List.iter
    (fun (description, source, code, message_fragment) ->
      check_failure description source code message_fragment)
    [
      ( "end of input before operand",
        "_intern defined",
        "HCPARSE0040",
        "expected one token after defined" );
      ( "empty wrapper",
        "_intern defined() I64 Empty();",
        "HCPARSE0041",
        "one wrapper parenthesis remains" );
      ( "unclosed wrapper",
        "_intern defined(Name I64 Unclosed();",
        "HCPARSE0041",
        "expected ')' to close defined target" );
      ( "unclosed nested wrapper",
        "_intern defined((Name) I64 Nested();",
        "HCPARSE0041",
        "one wrapper parenthesis remains" );
      ( "expression-shaped operand",
        "_intern defined(Name + Other) I64 Expression();",
        "HCPARSE0041",
        "found \"+\"" );
      ( "direct call suffix",
        "_intern defined(Name)() I64 Called();",
        "HCPARSE0042",
        "expected a postfix cast target after defined" );
      ( "direct index suffix",
        "_intern defined(Name)[0] I64 Indexed();",
        "HCPARSE0042",
        "cannot be followed directly by \"[\"" );
      ( "direct member suffix",
        "_intern defined(Name).value I64 Member();",
        "HCPARSE0042",
        "cannot be followed directly by \".\"" );
      ( "direct pointer-member suffix",
        "_intern defined(Name)->value I64 PointerMember();",
        "HCPARSE0042",
        "cannot be followed directly by \"->\"" );
      ( "direct increment suffix",
        "_intern defined(Name)++ I64 Incremented();",
        "HCPARSE0042",
        "cannot be followed directly by \"++\"" );
      ( "direct decrement suffix",
        "_intern defined(Name)-- I64 Decremented();",
        "HCPARSE0042",
        "cannot be followed directly by \"--\"" );
      ( "nonprimitive cast target",
        "_intern defined(Name)(Widget) I64 Casted();",
        "HCPARSE0020",
        "nonprimitive postfix cast target" );
    ]

let deterministic_defined_dumps () =
  let session, _, output =
    parse_string
      "_intern defined(((I64)))(U64) + defined(42)(U8 *)[0] I64 Checked();"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human defined dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON defined dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump names the identifier-shaped operand" true
    (contains human "expression kind=defined wrappers=3");
  Alcotest.(check bool)
    "human dump keeps the name operand" true
    (contains human "operand kind=name spelling=\"I64\"");
  Alcotest.(check bool)
    "human dump names the non-name operand" true
    (contains human "operand kind=non_name spelling=\"42\"");
  let open Yojson.Safe.Util in
  let binary_json =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "binding" |> member "target_expression"
  in
  let left_defined = binary_json |> member "left" |> member "operand" in
  Alcotest.(check string)
    "JSON left cast wraps defined" "defined"
    (left_defined |> member "kind" |> to_string);
  Alcotest.(check int)
    "JSON keeps three opening wrappers" 3
    (left_defined |> member "opening_parentheses" |> to_list |> List.length);
  Alcotest.(check string)
    "JSON keeps the keyword operand as name-shaped" "name"
    (left_defined |> member "operand" |> member "kind" |> to_string);
  let right_defined =
    binary_json |> member "right" |> member "base" |> member "operand"
  in
  Alcotest.(check string)
    "JSON right index base wraps defined" "defined"
    (right_defined |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps the literal operand as non-name" "non_name"
    (right_defined |> member "operand" |> member "kind" |> to_string)

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
  (match expect_default_expression (List.nth defaults 0) with
  | Ast.Integer_literal literal ->
      Alcotest.(check string) "integer spelling" "0" literal.literal_spelling;
      Alcotest.(check int64)
        "explicit zero value" 0L
        (match literal.literal_value with
        | Ast.Integer_value value -> value
        | _ -> Alcotest.fail "integer literal has the wrong value kind")
  | _ -> Alcotest.fail "expected an integer default");
  (match expect_default_expression (List.nth defaults 1) with
  | Ast.Float_literal literal ->
      Alcotest.(check (float 0.))
        "floating value" 1.5
        (match literal.literal_value with
        | Ast.Float_value value -> value
        | _ -> Alcotest.fail "floating literal has the wrong value kind")
  | _ -> Alcotest.fail "expected a floating default");
  (match expect_default_expression (List.nth defaults 2) with
  | Ast.Character_literal literal ->
      Alcotest.(check int64)
        "multi-character value" 0x4241L
        (match literal.literal_value with
        | Ast.Integer_value value -> value
        | _ -> Alcotest.fail "character literal has the wrong value kind")
  | _ -> Alcotest.fail "expected a character default");
  (match expect_default_expression (List.nth defaults 3) with
  | Ast.String_literal literal ->
      Alcotest.(check string)
        "decoded string bytes" "ok\n"
        (match literal.literal_value with
        | Ast.Bytes_value value -> value
        | _ -> Alcotest.fail "string literal has the wrong value kind")
  | _ -> Alcotest.fail "expected a string default");
  (match expect_default_expression (List.nth defaults 4) with
  | Ast.Identifier_expression identifier ->
      Alcotest.(check string) "identifier spelling" "NULL" identifier.spelling
  | _ -> Alcotest.fail "expected an identifier default");
  (match expect_default_expression (List.nth defaults 5) with
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
      let prefix =
        expect_prefix_expression (expect_default_expression default)
      in
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
        |> fun default ->
        expect_binary_expression (expect_default_expression default)
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
            |> fun default ->
            expect_binary_expression (expect_default_expression default)
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
        |> fun default ->
        expect_binary_expression (expect_default_expression default)
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
    |> fun default ->
    expect_binary_expression (expect_default_expression default)
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
    |> fun default ->
    expect_prefix_expression (expect_default_expression default)
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
  let outer_binary =
    expect_binary_expression (expect_default_expression outer_default)
  in
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
          (Ast.expression_location (expect_default_expression default)).span
            .source
        |> Option.get
      in
      Alcotest.(check string)
        "default keeps its included source path"
        (Unix.realpath declaration_file)
        (Source_file.path expression_source))

let lastclass_default_source_behavior () =
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "lastclass keyword number is pinned" true
    (contains compiler_header "#define KW_LASTCLASS\t37");
  let variable_parser =
    pinned "Compiler/PrsVar.HC" |> String.split_on_char '\r' |> String.concat ""
  in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool) description true (contains variable_parser fragment))
    [
      ( "lastclass is checked before expression compilation",
        "if (PrsKeyWord(cc)==KW_LASTCLASS)" );
      ("lastclass marks the member", "tmpm->flags|=MLF_LASTCLASS;");
      ( "lastclass consumes one token",
        "tmpm->flags|=MLF_LASTCLASS;\n\t      Lex(cc);" );
      ("the default remains available", "tmpm->flags|=MLF_DFT_AVAILABLE;");
    ];
  let kernel_header = pinned "Kernel/KernelA.HH" in
  Alcotest.(check bool)
    "lastclass has a separate member flag" true
    (contains kernel_header "#define MLF_LASTCLASS\t\t2");
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  Alcotest.(check bool)
    "call parsing substitutes the prior explicit class" true
    (contains expression_parser
       "dft_val=(last_class-last_class->ptr_stars_cnt)->str;");
  let language_notes = pinned "Doc/HolyC.DD" in
  Alcotest.(check bool)
    "language notes call lastclass a default argument" true
    (contains language_notes "lastclass$FG$ you use as a dft arg");
  let example = pinned "Demo/LastClass.HC" in
  Alcotest.(check bool)
    "pinned example uses the default" true
    (contains example "U8 *class_name=lastclass")

let lastclass_default_shapes () =
  let source =
    "extern U0 Mixed(I64 first=1,U8 *object,U8 *class_name=lastclass,\n\
     I64 after=2,I64 plain);"
  in
  let session, _, output = parse_string source in
  let prototype = expect_ast output |> expect_one_prototype in
  Alcotest.(check (list bool))
    "lastclass does not force trailing defaults"
    [ true; false; true; true; false ]
    (List.map
       (fun (parameter : Ast.function_parameter) ->
         Option.is_some parameter.default)
       prototype.parameters);
  let first_default =
    List.nth prototype.parameters 0 |> expect_parameter_default
  in
  Alcotest.(check int64)
    "ordinary defaults retain their expression value" 1L
    (expect_default_expression first_default |> expect_integer_expression);
  let default = List.nth prototype.parameters 2 |> expect_parameter_default in
  let lastclass = expect_lastclass_default default in
  Alcotest.(check string)
    "keyword spelling" "lastclass" lastclass.lastclass_spelling;
  Alcotest.(check int)
    "default begins at equals" default.equals.span.start
    default.location.span.start;
  Alcotest.(check int)
    "default ends after keyword" lastclass.lastclass_location.span.stop
    default.location.span.stop;
  let open Yojson.Safe.Util in
  let value_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "parameters" |> to_list
    |> fun parameters ->
    List.nth parameters 2 |> member "default" |> member "value"
  in
  Alcotest.(check string)
    "JSON uses a distinct value kind" "lastclass_default"
    (value_json |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON retains keyword spelling" "lastclass"
    (value_json |> member "spelling" |> to_string)

let lastclass_function_pointer_and_modes () =
  let nested_source =
    "extern U0 Register(I64 (*callback)(U8 *object,\n\
     U8 *class_name=lastclass));"
  in
  let _, _, nested_output = parse_string nested_source in
  let callback =
    expect_ast nested_output |> expect_one_prototype |> fun prototype ->
    List.hd prototype.parameters |> expect_function_pointer
  in
  let nested_default =
    List.nth callback.signature_parameters 1 |> expect_parameter_default
  in
  Alcotest.(check string)
    "nested function pointer keeps lastclass" "lastclass"
    (expect_lastclass_default nested_default).lastclass_spelling;
  List.iter
    (fun mode ->
      let _, _, output =
        parse_string ~compilation_mode:mode
          "extern U0 Mode(U8 *value,U8 *class_name=lastclass);"
      in
      let lastclass =
        expect_ast output |> expect_one_prototype |> fun prototype ->
        List.nth prototype.parameters 1
        |> expect_parameter_default |> expect_lastclass_default
      in
      Alcotest.(check string)
        "mode preserves keyword spelling" "lastclass"
        lastclass.lastclass_spelling)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let lastclass_default_provenance () =
  let source =
    "#define LAST_CLASS lastclass\n\
     extern U0 Generated(U8 *value,U8 *class_name=LAST_CLASS);"
  in
  let session, root, output = parse_string source in
  let default =
    expect_ast output |> expect_one_prototype |> fun prototype ->
    List.nth prototype.parameters 1 |> expect_parameter_default
  in
  let lastclass = expect_lastclass_default default in
  let location = lastclass.lastclass_location in
  Alcotest.(check bool)
    "definition-backed keyword uses generated source" false
    (Source_id.equal location.span.source (Source_file.id root));
  Alcotest.(check bool)
    "definition-backed keyword keeps invocation provenance" true
    (Option.is_some location.generated_from);
  Alcotest.(check bool)
    "definition-backed keyword keeps definition provenance" true
    (Option.is_some location.defined_at);
  Alcotest.(check bool)
    "combined default retains both source segments" true
    (List.length default.location.source_segments > 1);
  let open Yojson.Safe.Util in
  let value_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "parameters" |> to_list
    |> fun parameters ->
    List.nth parameters 1 |> member "default" |> member "value"
  in
  Alcotest.(check bool)
    "JSON keeps keyword generation provenance" true
    (value_json |> member "location" |> member "generated_from" <> `Null);
  with_temp_directory (fun include_root ->
      let root_file = Filename.concat include_root "root.HC" in
      let declaration_file = Filename.concat include_root "lastclass.HC" in
      write_file root_file "#include \"lastclass\"";
      write_file declaration_file
        "extern U0 Included(U8 *value,U8 *class_name=lastclass);";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config include_root)
          ~source:include_source
      in
      let included_lastclass =
        expect_ast include_output |> expect_one_prototype |> fun prototype ->
        List.nth prototype.parameters 1
        |> expect_parameter_default |> expect_lastclass_default
      in
      let keyword_source =
        Source_manager.find
          (Session.sources include_session)
          included_lastclass.lastclass_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included keyword keeps its canonical path"
        (Unix.realpath declaration_file)
        (Source_file.path keyword_source))

let lastclass_default_failures () =
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
          Alcotest.failf "rejected lastclass prototype %s became visible" name)
    [
      ( "binary continuation",
        "extern U0 Added(U8 *value,U8 *name=lastclass+1);",
        "Added",
        "HCPARSE0010",
        "expected ',' or ')'" );
      ( "call continuation",
        "extern U0 Called(U8 *value,U8 *name=lastclass());",
        "Called",
        "HCPARSE0010",
        "expected ',' or ')'" );
      ( "keyword after an ordinary expression",
        "extern U0 Later(U8 *name=1 lastclass);",
        "Later",
        "HCPARSE0010",
        "expected ',' or ')'" );
      ( "ordinary binding expression",
        "_intern lastclass I64 Bound();",
        "Bound",
        "HCPARSE0020",
        "not implemented" );
    ]

let deterministic_lastclass_dumps () =
  let session, _, output =
    parse_string "extern U0 Stable(U8 *value,U8 *name=lastclass);"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human lastclass dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON lastclass dump repeats byte for byte" json
    (Ast_dump.json sources ast)

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
      ( "call after a postfix increment",
        "I64 Indexed[count++()];",
        "Indexed",
        "HCPARSE0028",
        "postfix chain" );
      ( "call after a postfix incremented call",
        "I64 Called[Count()++()];",
        "Called",
        "HCPARSE0028",
        "postfix chain" );
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
      ( "call after a postfix incremented call",
        "extern U0 Called(I64 value=Factory()++());",
        "Called",
        "HCPARSE0028",
        "postfix chain" );
      ( "index after a postfix increment",
        "extern U0 Postfix(I64 value=counter++[0]);",
        "Postfix",
        "HCPARSE0028",
        "postfix chain" );
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
              "independent declarations unexpectedly formed a prototype"
        | Ast.Top_level_statement _ ->
            Alcotest.fail
              "independent declarations unexpectedly formed a statement")
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
             Alcotest.fail "conditional fixture unexpectedly formed a prototype"
         | Ast.Top_level_statement _ ->
             Alcotest.fail "conditional fixture unexpectedly formed a statement")
       ast.items)

let implicit_output_source_behavior () =
  let statement_parser =
    pinned "Compiler/PrsStmt.HC"
    |> String.split_on_char '\r' |> String.concat ""
  in
  Alcotest.(check bool)
    "statement parser selects both literal token kinds" true
    (contains statement_parser "cc->token==TK_STR||cc->token==TK_CHAR_CONST");
  Alcotest.(check bool)
    "statement parser delegates to the implicit call path" true
    (contains statement_parser "PrsFunCall(cc,NULL,FALSE,NULL);");
  let expression_parser = pinned "Compiler/PrsExp.HC" in
  Alcotest.(check bool)
    "character literal selects PutChars" true
    (contains expression_parser "HashFind(\"PutChars\"");
  Alcotest.(check bool)
    "string literal selects Print" true
    (contains expression_parser "HashFind(\"Print\"");
  Alcotest.(check bool)
    "empty character marker advances to a variable" true
    (contains expression_parser "empty char signals PutChars with variable");
  Alcotest.(check bool)
    "empty string marker advances to a variable format" true
    (contains expression_parser
       "empty string signals Print with variable fmt_str");
  let language = pinned "Doc/HolyC.DD" in
  Alcotest.(check bool)
    "documentation states the implicit output rule" true
    (contains language "A char const all alone is sent to $LK,\"PutChars\"");
  List.iter
    (fun example ->
      Alcotest.(check bool)
        ("documentation includes " ^ example)
        true
        (contains language example))
    [
      "\"Hello World!\\n\";";
      "\"%s age %d\\n\",name,age;";
      "\"\" fmt,name,age;";
      "'' drv;";
      "'*';";
    ];
  let headers = pinned "Kernel/KExts.HC" in
  Alcotest.(check bool)
    "Print prototype is pinned" true
    (contains headers "extern U0 Print(U8 *fmt,...);");
  Alcotest.(check bool)
    "PutChars prototype is pinned" true
    (contains headers "extern U0 PutChars(U64 ch);")

let implicit_output_shapes () =
  let source =
    "\"Hello\\n\";\n\
     \"%s %d\\n\",name,age;\n\
     \"\" fmt,name,age;\n\
     '*';\n\
     'ABC';\n\
     '' drv;\n\
     \"\\0tail\" nul_fmt;"
  in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  let statements =
    List.map
      (function
        | Ast.Top_level_statement (Ast.Implicit_output_statement statement) ->
            statement
        | Ast.Global_variable _
        | Ast.Global_declaration _
        | Ast.Function_prototype _
        | Ast.Top_level_statement _ ->
            Alcotest.fail "output fixture unexpectedly parsed a declaration")
      ast.items
  in
  Alcotest.(check int) "seven statements" 7 (List.length statements);
  let hello = List.nth statements 0 in
  Alcotest.(check bool)
    "ordinary string targets Print" true
    (hello.target = Ast.Print_target);
  Alcotest.(check string)
    "decoded marker bytes" "Hello\n"
    (expect_bytes_value hello.marker);
  Alcotest.(check string)
    "marker expression supplies the fixed argument" "Hello\n"
    (expect_marker_fixed_argument hello
    |> expect_string_literal |> expect_bytes_value);
  Alcotest.(check int)
    "ordinary string has no varargs" 0
    (List.length hello.arguments);
  let formatted = List.nth statements 1 in
  Alcotest.(check int)
    "formatted string has two varargs" 2
    (List.length formatted.arguments);
  Alcotest.(check (list string))
    "formatted arguments retain order" [ "name"; "age" ]
    (List.map
       (fun (argument : Ast.implicit_output_argument) ->
         (expect_identifier_expression argument.value).spelling)
       formatted.arguments);
  let variable_format = List.nth statements 2 in
  Alcotest.(check string)
    "empty marker stays empty" ""
    (expect_bytes_value variable_format.marker);
  let variable_format_identifier =
    expect_following_fixed_argument variable_format
    |> expect_identifier_expression
  in
  Alcotest.(check string)
    "following expression supplies the format" "fmt"
    variable_format_identifier.spelling;
  Alcotest.(check (list string))
    "variable format arguments retain order" [ "name"; "age" ]
    (List.map
       (fun (argument : Ast.implicit_output_argument) ->
         (expect_identifier_expression argument.value).spelling)
       variable_format.arguments);
  let star = List.nth statements 3 in
  Alcotest.(check bool)
    "character targets PutChars" true
    (star.target = Ast.Put_chars_target);
  Alcotest.(check int64) "star value" 42L (expect_character_value star.marker);
  let multi = List.nth statements 4 in
  Alcotest.(check int64)
    "multi-character marker value" 0x434241L
    (expect_character_value multi.marker);
  let variable_character = List.nth statements 5 in
  Alcotest.(check int64)
    "empty character marker is zero" 0L
    (expect_character_value variable_character.marker);
  let variable_character_identifier =
    expect_following_fixed_argument variable_character
    |> expect_identifier_expression
  in
  Alcotest.(check string)
    "following expression supplies the character" "drv"
    variable_character_identifier.spelling;
  let nul_format = List.nth statements 6 in
  Alcotest.(check string)
    "a leading decoded NUL selects the variable format" "\000tail"
    (expect_bytes_value nul_format.marker);
  let nul_format_identifier =
    expect_following_fixed_argument nul_format |> expect_identifier_expression
  in
  Alcotest.(check string)
    "NUL-prefixed marker uses the following expression" "nul_fmt"
    nul_format_identifier.spelling

let implicit_output_fixed_expressions () =
  let source = "\"prefix\"+suffix;\n'A'+1;\n\"\" BuildFmt();\n'' value(U8);" in
  let _, _, output = parse_string source in
  let ast = expect_ast output in
  let statements =
    List.map
      (function
        | Ast.Top_level_statement (Ast.Implicit_output_statement statement) ->
            statement
        | _ -> Alcotest.fail "expected an implicit output statement")
      ast.items
  in
  List.iteri
    (fun index statement ->
      let expression =
        if index < 2 then expect_marker_fixed_argument statement
        else expect_following_fixed_argument statement
      in
      match (index, expression) with
      | (0 | 1), Ast.Binary_expression _ -> ()
      | 2, Ast.Call_expression _ -> ()
      | 3, Ast.Postfix_cast_expression _ -> ()
      | _ ->
          Alcotest.failf "unexpected fixed expression shape at index %d" index)
    statements

let implicit_output_order_and_modes () =
  let source = "I64 before;\n\"ready\";\nextern U0 Hook();\n'' before;" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable before;
       Ast.Top_level_statement (Ast.Implicit_output_statement print);
       Ast.Function_prototype hook;
       Ast.Top_level_statement (Ast.Implicit_output_statement put_chars);
      ] ->
          Alcotest.(check string) "first item" "before" before.name.spelling;
          Alcotest.(check bool)
            "second item is Print" true
            (print.target = Ast.Print_target);
          Alcotest.(check string) "third item" "Hook" hook.name.spelling;
          Alcotest.(check bool)
            "fourth item is PutChars" true
            (put_chars.target = Ast.Put_chars_target)
      | items ->
          Alcotest.failf "expected four ordered module items, got %d"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let implicit_output_provenance () =
  let source =
    "#define OUTPUT \"%s\"\nOUTPUT,value;\n#define EMPTY \"\"\nEMPTY fmt;"
  in
  let session, root, output = parse_string source in
  let statements =
    (expect_ast output).Ast.items
    |> List.map (function
      | Ast.Top_level_statement (Ast.Implicit_output_statement statement) ->
          statement
      | _ -> Alcotest.fail "expected definition-backed output statements")
  in
  List.iter
    (fun (statement : Ast.implicit_output_statement) ->
      let location = statement.marker.literal_location in
      Alcotest.(check bool)
        "definition marker uses generated source" false
        (Source_id.equal location.span.source (Source_file.id root));
      Alcotest.(check bool)
        "invocation provenance" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "definition provenance" true
        (Option.is_some location.defined_at))
    statements;
  ignore session;
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let statement_file = Filename.concat directory "statement.HC" in
      write_file root_file "#include \"statement\"";
      write_file statement_file "\"included\";";
      let include_session = Session.create () in
      let include_source =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:include_source
      in
      let statement = expect_ast include_output |> expect_one_implicit_output in
      let marker_source =
        Source_manager.find
          (Session.sources include_session)
          statement.marker.literal_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included marker keeps its canonical path"
        (Unix.realpath statement_file)
        (Source_file.path marker_source))

let implicit_output_failures () =
  let cases =
    [
      ("empty string without format", "\"\";", "HCPARSE0043");
      ("empty character without value", "'';", "HCPARSE0043");
      ("empty Print vararg", "\"x\",;", "HCPARSE0044");
      ("repeated Print comma", "\"x\",,value;", "HCPARSE0044");
      ("missing Print semicolon", "\"x\"", "HCPARSE0046");
      ("missing PutChars semicolon", "'' value", "HCPARSE0046");
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

let deterministic_implicit_output_dumps () =
  let session, _, output = parse_string "\"value=%d\",value;" in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human output is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON output is deterministic" json
    (Ast_dump.json sources ast);
  let open Yojson.Safe.Util in
  let item =
    Yojson.Safe.from_string json
    |> member "module" |> member "items" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "top-level item kind" "top_level_statement"
    (item |> member "kind" |> to_string);
  let statement = item |> member "statement" in
  Alcotest.(check string)
    "statement kind" "implicit_output_statement"
    (statement |> member "kind" |> to_string);
  Alcotest.(check string)
    "statement target" "print"
    (statement |> member "target" |> to_string);
  Alcotest.(check int)
    "one Print vararg" 1
    (statement |> member "arguments" |> to_list |> List.length)

let statement_sequence_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsStmt skips leading commas", "while (cc->token==',')");
      ("PrsStmt accepts an empty semicolon", "} else if (cc->token==';') {");
      ( "PrsStmt delegates ordinary expressions",
        "if (!PrsExpression(cc,NULL,TRUE))" );
      ( "PrsStmt accepts comma in place of a semicolon",
        "else if (cc->token!=',')" );
      ("PrsStmt continues after a comma", "if (cc->token!=',') goto sm_done;");
    ];
  let driver = pinned "Compiler/CMain.HC" in
  Alcotest.(check bool)
    "AOT compilation reads statements in source order" true
    (contains driver "while (cc->token!=TK_EOF)");
  Alcotest.(check bool)
    "JIT compilation delegates to PrsStmt" true
    (contains driver "PrsStmt(cc,,,cmp_flags);")

let statement_expression_and_empty_shapes () =
  let source = "I64 value;\nvalue=1;\n;\nvalue++;" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable value;
       Ast.Top_level_statement (Ast.Expression_statement assignment);
       Ast.Top_level_statement (Ast.Empty_statement empty);
       Ast.Top_level_statement (Ast.Expression_statement update);
      ] ->
          Alcotest.(check string) "declared name" "value" value.name.spelling;
          Alcotest.(check bool)
            "assignment keeps its semicolon" true
            (Option.is_some assignment.expression_statement_semicolon);
          let binary =
            expect_binary_expression assignment.expression_statement_expression
          in
          Alcotest.(check string)
            "assignment operator" "=" binary.binary_operator.operator_spelling;
          Alcotest.(check int)
            "empty semicolon is one byte" 1
            (Span.length empty.empty_statement_semicolon.span);
          let postfix =
            expect_postfix_expression update.expression_statement_expression
          in
          Alcotest.(check bool)
            "postfix update kind" true
            (postfix.postfix_operator_kind = Ast.Post_increment);
          Alcotest.(check bool)
            "top-level statements keep source order" true
            (assignment.expression_statement_location.span.start
             < empty.empty_statement_location.span.start
            && empty.empty_statement_location.span.start
               < update.expression_statement_location.span.start)
      | items ->
          Alcotest.failf "expected one declaration and three statements, got %d"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let statement_comma_sequence_shapes () =
  let source = "I64 first;I64 second;,,first=1,second=first+1;," in
  let _, _, output = parse_string source in
  let sequence =
    match (expect_ast output).Ast.items with
    | [
     Ast.Global_variable _;
     Ast.Global_variable _;
     Ast.Top_level_statement (Ast.Sequence_statement sequence);
    ] -> sequence
    | items ->
        Alcotest.failf "expected two declarations and one sequence, got %d"
          (List.length items)
  in
  Alcotest.(check int)
    "two leading commas" 2
    (List.length sequence.sequence_leading_commas);
  Alcotest.(check int)
    "two expression elements" 2
    (List.length sequence.sequence_elements);
  Alcotest.(check (list int))
    "each expression has a following comma" [ 1; 1 ]
    (List.map
       (fun (element : Ast.statement_sequence_element) ->
         List.length element.sequence_following_commas)
       sequence.sequence_elements);
  let first =
    List.nth sequence.sequence_elements 0 |> fun element ->
    expect_expression_statement element.sequence_statement
  in
  let second =
    List.nth sequence.sequence_elements 1 |> fun element ->
    expect_expression_statement element.sequence_statement
  in
  Alcotest.(check bool)
    "comma terminates the first expression" true
    (Option.is_none first.expression_statement_semicolon);
  Alcotest.(check bool)
    "the second expression retains its semicolon" true
    (Option.is_some second.expression_statement_semicolon);
  let _, _, semicolon_before_comma =
    parse_string "I64 first;I64 second;first=1;,second=2;"
  in
  let semicolon_sequence =
    match (expect_ast semicolon_before_comma).Ast.items with
    | [ _; _; Ast.Top_level_statement statement ] ->
        expect_statement_sequence statement
    | _ -> Alcotest.fail "expected a semicolon-before-comma sequence"
  in
  let first_with_semicolon =
    List.hd semicolon_sequence.sequence_elements |> fun element ->
    expect_expression_statement element.sequence_statement
  in
  Alcotest.(check bool)
    "semicolon before comma remains visible" true
    (Option.is_some first_with_semicolon.expression_statement_semicolon);
  let _, _, comma_only = parse_string "," in
  let comma_only_sequence =
    match (expect_ast comma_only).Ast.items with
    | [ Ast.Top_level_statement statement ] ->
        expect_statement_sequence statement
    | _ -> Alcotest.fail "expected a comma-only statement sequence"
  in
  Alcotest.(check int)
    "comma-only sequence keeps its leading comma" 1
    (List.length comma_only_sequence.sequence_leading_commas);
  Alcotest.(check int)
    "comma-only sequence has no semantic element" 0
    (List.length comma_only_sequence.sequence_elements)

let statement_output_comma_boundaries () =
  let source = "I64 value;\n'A',value++;\n\"x\";,value--;\n\"%d\",value;" in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement (Ast.Sequence_statement put_sequence);
   Ast.Top_level_statement (Ast.Sequence_statement print_sequence);
   Ast.Top_level_statement (Ast.Implicit_output_statement formatted);
  ] ->
      let put_element = List.hd put_sequence.sequence_elements in
      let put =
        match put_element.sequence_statement with
        | Ast.Implicit_output_statement statement -> statement
        | _ -> Alcotest.fail "expected PutChars before its statement comma"
      in
      Alcotest.(check bool)
        "PutChars targets character output" true
        (put.target = Ast.Put_chars_target);
      Alcotest.(check bool)
        "PutChars may end at a comma" true
        (Option.is_none put.semicolon);
      let print_element = List.hd print_sequence.sequence_elements in
      let print =
        match print_element.sequence_statement with
        | Ast.Implicit_output_statement statement -> statement
        | _ -> Alcotest.fail "expected Print before its statement comma"
      in
      Alcotest.(check bool)
        "Print targets formatted output" true
        (print.target = Ast.Print_target);
      Alcotest.(check bool)
        "Print retains its semicolon before a statement comma" true
        (Option.is_some print.semicolon);
      Alcotest.(check int)
        "the Print statement comma is not a vararg" 0
        (List.length print.arguments);
      Alcotest.(check int)
        "a comma before Print's semicolon remains a vararg" 1
        (List.length formatted.arguments)
  | items ->
      Alcotest.failf "expected one declaration and three output groups, got %d"
        (List.length items)

let statement_sequence_provenance () =
  let source = "I64 value;\n#define SET value=1\n,,SET,SET;" in
  let _, _, output = parse_string source in
  let sequence =
    match (expect_ast output).Ast.items with
    | [ _; Ast.Top_level_statement statement ] ->
        expect_statement_sequence statement
    | _ -> Alcotest.fail "expected a definition-backed statement sequence"
  in
  List.iter
    (fun (element : Ast.statement_sequence_element) ->
      let expression = expect_expression_statement element.sequence_statement in
      Alcotest.(check bool)
        "expanded statement retains its invocation" true
        (Option.is_some expression.expression_statement_location.generated_from);
      Alcotest.(check bool)
        "expanded statement retains its definition" true
        (Option.is_some expression.expression_statement_location.defined_at))
    sequence.sequence_elements;
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let statement_file = Filename.concat directory "statement.HC" in
      write_file root_file "I64 value;\n#include \"statement\"";
      write_file statement_file "value=1;";
      let session = Session.create () in
      let root = Session.load_source session ~path:root_file |> Result.get_ok in
      let output =
        Holyc_lib.parse_detailed session ~config:(config directory) ~source:root
      in
      let expression =
        match (expect_ast output).Ast.items with
        | [ _; Ast.Top_level_statement statement ] ->
            expect_expression_statement statement
        | _ -> Alcotest.fail "expected one included expression statement"
      in
      let included_source =
        Source_manager.find (Session.sources session)
          expression.expression_statement_location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included statement keeps its canonical path"
        (Unix.realpath statement_file)
        (Source_file.path included_source))

let statement_sequence_failures () =
  List.iter
    (fun (name, source, code) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      Alcotest.(check string)
        (name ^ " diagnostic") code (first_diagnostic output).code)
    [
      ("missing expression terminator", "I64 value;value=1", "HCPARSE0047");
      ("unsupported return", "return;", "HCPARSE0048");
      ("declaration after comma", "I64 value;value=1,I64 other;", "HCPARSE0048");
      ("unresolved comma target", "'A',missing;", "HCPARSE0048");
      ("missing assignment operand", "I64 value;value=;", "HCPARSE0018");
    ]

let deterministic_statement_sequence_dumps () =
  let session, _, output = parse_string "I64 value;,,value=1,value++;" in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human statement dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON statement dump is deterministic" json
    (Ast_dump.json sources ast);
  let open Yojson.Safe.Util in
  let sequence =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth_opt items 1 |> Option.get |> member "statement"
  in
  Alcotest.(check string)
    "sequence JSON kind" "statement_sequence"
    (sequence |> member "kind" |> to_string);
  Alcotest.(check int)
    "two JSON leading commas" 2
    (sequence |> member "leading_commas" |> to_list |> List.length);
  let elements = sequence |> member "elements" |> to_list in
  Alcotest.(check int) "two JSON elements" 2 (List.length elements);
  let first_statement = List.hd elements |> member "statement" in
  Alcotest.(check bool)
    "comma-terminated expression has JSON null semicolon" true
    (match first_statement |> member "semicolon" with
    | `Null -> true
    | _ -> false)

let unsupported_forms () =
  let cases =
    [
      ("unknown type", "Widget value;", "HCPARSE0001");
      ("missing name", "I64 ;", "HCPARSE0002");
      ("missing semicolon", "I64 value", "HCPARSE0003");
      ("initializer", "I64 value=1;", "HCPARSE0003");
      ("function", "I64 Function();", "HCPARSE0008");
      ("statement", "return;", "HCPARSE0048");
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
    Alcotest.test_case "pinned call expression behavior" `Quick
      call_expression_source_behavior;
    Alcotest.test_case "call argument slots" `Quick call_argument_slots;
    Alcotest.test_case "call precedence and nesting" `Quick
      call_expression_precedence_and_nesting;
    Alcotest.test_case "call expression contexts and modes" `Quick
      call_expression_contexts_and_modes;
    Alcotest.test_case "call expression provenance" `Quick
      call_expression_provenance;
    Alcotest.test_case "call expression failures" `Quick
      call_expression_failures;
    Alcotest.test_case "deterministic call dumps" `Quick
      deterministic_call_dumps;
    Alcotest.test_case "pinned index expression behavior" `Quick
      index_expression_source_behavior;
    Alcotest.test_case "index shapes and precedence" `Quick
      index_expression_shapes_and_precedence;
    Alcotest.test_case "index expression contexts and modes" `Quick
      index_expression_contexts_and_modes;
    Alcotest.test_case "index expression provenance" `Quick
      index_expression_provenance;
    Alcotest.test_case "index expression failures" `Quick
      index_expression_failures;
    Alcotest.test_case "deterministic index dumps" `Quick
      deterministic_index_dumps;
    Alcotest.test_case "pinned member expression behavior" `Quick
      member_expression_source_behavior;
    Alcotest.test_case "member shapes and precedence" `Quick
      member_expression_shapes_and_precedence;
    Alcotest.test_case "member expression contexts and modes" `Quick
      member_expression_contexts_and_modes;
    Alcotest.test_case "member expression provenance" `Quick
      member_expression_provenance;
    Alcotest.test_case "member expression failures" `Quick
      member_expression_failures;
    Alcotest.test_case "deterministic member dumps" `Quick
      deterministic_member_dumps;
    Alcotest.test_case "pinned postfix update behavior" `Quick
      postfix_update_source_behavior;
    Alcotest.test_case "postfix update shapes and precedence" `Quick
      postfix_update_shapes_and_precedence;
    Alcotest.test_case "postfix update contexts and modes" `Quick
      postfix_update_contexts_and_modes;
    Alcotest.test_case "postfix update provenance" `Quick
      postfix_update_provenance;
    Alcotest.test_case "postfix update failures" `Quick postfix_update_failures;
    Alcotest.test_case "deterministic postfix update dumps" `Quick
      deterministic_postfix_update_dumps;
    Alcotest.test_case "pinned postfix cast behavior" `Quick
      postfix_cast_source_behavior;
    Alcotest.test_case "postfix cast types and shapes" `Quick
      postfix_cast_types_and_shapes;
    Alcotest.test_case "postfix cast chains and precedence" `Quick
      postfix_cast_chains_and_precedence;
    Alcotest.test_case "postfix cast contexts and modes" `Quick
      postfix_cast_contexts_and_modes;
    Alcotest.test_case "postfix cast provenance" `Quick postfix_cast_provenance;
    Alcotest.test_case "postfix cast failures" `Quick postfix_cast_failures;
    Alcotest.test_case "deterministic postfix cast dumps" `Quick
      deterministic_postfix_cast_dumps;
    Alcotest.test_case "pinned sizeof behavior" `Quick sizeof_source_behavior;
    Alcotest.test_case "sizeof shapes" `Quick sizeof_shapes;
    Alcotest.test_case "sizeof postfix and precedence" `Quick
      sizeof_postfix_and_precedence;
    Alcotest.test_case "sizeof contexts and modes" `Quick
      sizeof_contexts_and_modes;
    Alcotest.test_case "sizeof provenance" `Quick sizeof_provenance;
    Alcotest.test_case "sizeof failures" `Quick sizeof_failures;
    Alcotest.test_case "deterministic sizeof dumps" `Quick
      deterministic_sizeof_dumps;
    Alcotest.test_case "pinned offset behavior" `Quick offset_source_behavior;
    Alcotest.test_case "offset shapes" `Quick offset_shapes;
    Alcotest.test_case "offset postfix and precedence" `Quick
      offset_postfix_and_precedence;
    Alcotest.test_case "offset contexts and modes" `Quick
      offset_contexts_and_modes;
    Alcotest.test_case "offset provenance" `Quick offset_provenance;
    Alcotest.test_case "offset failures" `Quick offset_failures;
    Alcotest.test_case "deterministic offset dumps" `Quick
      deterministic_offset_dumps;
    Alcotest.test_case "pinned defined behavior" `Quick defined_source_behavior;
    Alcotest.test_case "defined token shapes" `Quick defined_shapes;
    Alcotest.test_case "defined postfix and precedence" `Quick
      defined_postfix_and_precedence;
    Alcotest.test_case "defined contexts and modes" `Quick
      defined_contexts_and_modes;
    Alcotest.test_case "defined provenance" `Quick defined_provenance;
    Alcotest.test_case "defined failures" `Quick defined_failures;
    Alcotest.test_case "deterministic defined dumps" `Quick
      deterministic_defined_dumps;
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
    Alcotest.test_case "pinned lastclass default behavior" `Quick
      lastclass_default_source_behavior;
    Alcotest.test_case "lastclass default shapes" `Quick
      lastclass_default_shapes;
    Alcotest.test_case "lastclass function pointers and modes" `Quick
      lastclass_function_pointer_and_modes;
    Alcotest.test_case "lastclass default provenance" `Quick
      lastclass_default_provenance;
    Alcotest.test_case "lastclass default failures" `Quick
      lastclass_default_failures;
    Alcotest.test_case "deterministic lastclass dumps" `Quick
      deterministic_lastclass_dumps;
    Alcotest.test_case "pinned implicit output behavior" `Quick
      implicit_output_source_behavior;
    Alcotest.test_case "implicit output shapes" `Quick implicit_output_shapes;
    Alcotest.test_case "implicit output fixed expressions" `Quick
      implicit_output_fixed_expressions;
    Alcotest.test_case "implicit output order and modes" `Quick
      implicit_output_order_and_modes;
    Alcotest.test_case "implicit output provenance" `Quick
      implicit_output_provenance;
    Alcotest.test_case "implicit output failures" `Quick
      implicit_output_failures;
    Alcotest.test_case "deterministic implicit output dumps" `Quick
      deterministic_implicit_output_dumps;
    Alcotest.test_case "pinned statement sequence behavior" `Quick
      statement_sequence_source_behavior;
    Alcotest.test_case "expression and empty statement shapes" `Quick
      statement_expression_and_empty_shapes;
    Alcotest.test_case "comma-linked statement shapes" `Quick
      statement_comma_sequence_shapes;
    Alcotest.test_case "output statement comma boundaries" `Quick
      statement_output_comma_boundaries;
    Alcotest.test_case "statement sequence provenance" `Quick
      statement_sequence_provenance;
    Alcotest.test_case "statement sequence failures" `Quick
      statement_sequence_failures;
    Alcotest.test_case "deterministic statement sequence dumps" `Quick
      deterministic_statement_sequence_dumps;
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
