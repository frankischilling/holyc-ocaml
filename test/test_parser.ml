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

let expect_one_definition ast =
  match ast.Ast.items with
  | [ Ast.Function_definition definition ] -> definition
  | [ Ast.Global_variable _ ] ->
      Alcotest.fail "expected a function definition, got a singleton global"
  | [ Ast.Global_declaration _ ] ->
      Alcotest.fail "expected a function definition, got a declaration group"
  | [ Ast.Function_prototype _ ] ->
      Alcotest.fail "expected a function definition, got a prototype"
  | [ Ast.Top_level_statement _ ] ->
      Alcotest.fail "expected a function definition, got a top-level statement"
  | items ->
      Alcotest.failf "expected one function definition, got %d items"
        (List.length items)

let expect_function_body (definition : Ast.function_definition) =
  match definition.body with
  | Some body -> body
  | None -> Alcotest.fail "expected a present function body"

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

let expect_block_statement = function
  | Ast.Block_statement statement -> statement
  | _ -> Alcotest.fail "expected a compound block statement"

let expect_if_statement = function
  | Ast.If_statement statement -> statement
  | _ -> Alcotest.fail "expected an if statement"

let expect_while_statement = function
  | Ast.While_statement statement -> statement
  | _ -> Alcotest.fail "expected a while statement"

let expect_do_while_statement = function
  | Ast.Do_while_statement statement -> statement
  | _ -> Alcotest.fail "expected a do-while statement"

let expect_for_statement = function
  | Ast.For_statement statement -> statement
  | _ -> Alcotest.fail "expected a for statement"

let expect_goto_statement = function
  | Ast.Goto_statement statement -> statement
  | _ -> Alcotest.fail "expected a goto statement"

let expect_label_statement = function
  | Ast.Label_statement statement -> statement
  | _ -> Alcotest.fail "expected a function label"

let expect_lock_statement = function
  | Ast.Lock_statement statement -> statement
  | _ -> Alcotest.fail "expected a lock statement"

let expect_switch_statement = function
  | Ast.Switch_statement statement -> statement
  | _ -> Alcotest.fail "expected a switch statement"

let expect_try_catch_statement = function
  | Ast.Try_catch_statement statement -> statement
  | _ -> Alcotest.fail "expected a try/catch statement"

let expect_break_statement = function
  | Ast.Break_statement statement -> statement
  | _ -> Alcotest.fail "expected a break statement"

let expect_return_statement = function
  | Ast.Return_statement statement -> statement
  | _ -> Alcotest.fail "expected a return statement"

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
      | Ast.Function_definition _ ->
          Alcotest.fail "expected singleton globals, got a function definition"
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
      | Ast.Function_definition _ ->
          Alcotest.fail
            "expected function prototypes, got a function definition"
      | Ast.Top_level_statement _ ->
          Alcotest.fail
            "expected function prototypes, got a top-level statement")
    ast.Ast.items

let function_definitions ast =
  List.map
    (function
      | Ast.Function_definition definition -> definition
      | Ast.Global_variable _ ->
          Alcotest.fail "expected function definitions, got a singleton global"
      | Ast.Global_declaration _ ->
          Alcotest.fail "expected function definitions, got a declaration group"
      | Ast.Function_prototype _ ->
          Alcotest.fail "expected function definitions, got a prototype"
      | Ast.Top_level_statement _ ->
          Alcotest.fail
            "expected function definitions, got a top-level statement")
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
      | Ast.Function_definition _ ->
          Alcotest.fail "primitive fixture unexpectedly formed a function"
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

let function_definition_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("function declarator dispatch", "if (cc->token=='(') {");
      ("default declarators enter PrsFun", "PrsFun(cc,tmpc,st,fsp_flags);");
      ( "function signatures share PrsFunJoin",
        "cc->htc.local_var_lst=cc->htc.fun=PrsFunJoin" );
      ("PrsFun parses one statement", "PrsStmt(cc,,,0);");
      ("a lone semicolon is a statement", "} else if (cc->token==';') {");
      ("EOF ends a statement parse", "goto sm_done; //TK_EOF");
    ];
  List.iter
    (fun (path, fragment) ->
      Alcotest.(check bool)
        (path ^ " supplies a representative function")
        true
        (contains (pinned path) fragment))
    [
      ("Compiler/CMisc.HC", "Bool Option(I64 num,Bool val)");
      ("Kernel/FunSeg.HC", "I64 HasLower(U8 *src)");
      ( "Adam/AMem.HC",
        "public I64 TaskMemAlloced(CTask *task=NULL,Bool \
         override_validate=FALSE)" );
      ("Demo/Exceptions.HC", "U0 D1()");
    ];
  Alcotest.(check bool)
    "the language guide rejects a mandatory main" true
    (contains (pinned "Doc/HolyC.DD") "There is no $FG,2$main()$FG$ function.")

let function_definition_shapes () =
  let source =
    "public interrupt U8 *Recursive(U8 *value=0,...){return Recursive();}\n\
     U0 Empty();\n\
     U0 Bare() return;\n\
     U0 Sequence() 1,2;\n\
     U0 End()"
  in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match expect_ast output |> function_definitions with
      | [ recursive; empty; bare; sequence; absent ] ->
          Alcotest.(check (list string))
            "function modifiers retain source order" [ "public"; "interrupt" ]
            (List.map
               (fun (modifier : Ast.declaration_modifier) -> modifier.spelling)
               recursive.modifiers);
          Alcotest.(check int)
            "return pointer depth" 1
            (List.length recursive.return_pointer_layers);
          Alcotest.(check int)
            "one fixed parameter" 1
            (List.length recursive.parameters);
          Alcotest.(check bool)
            "the fixed parameter retains its default" true
            (Option.is_some (List.hd recursive.parameters).default);
          Alcotest.(check bool)
            "the variadic marker is retained" true
            (Option.is_some recursive.variadic);
          let recursive_block =
            expect_function_body recursive |> expect_block_statement
          in
          let recursive_return =
            match recursive_block.block_statements with
            | [ statement ] -> expect_return_statement statement
            | statements ->
                Alcotest.failf "expected one recursive statement, got %d"
                  (List.length statements)
          in
          let recursive_call =
            recursive_return.return_value |> Option.get
            |> expect_call_expression
          in
          let recursive_callee =
            expect_identifier_expression recursive_call.call_callee
          in
          Alcotest.(check string)
            "recursive call keeps the function name" "Recursive"
            recursive_callee.spelling;
          ignore (expect_function_body empty |> expect_empty_statement);
          ignore (expect_function_body bare |> expect_return_statement);
          let sequence =
            expect_function_body sequence |> expect_statement_sequence
          in
          Alcotest.(check int)
            "comma-linked body has two statements" 2
            (List.length sequence.sequence_elements);
          Alcotest.(check bool)
            "EOF leaves the final body absent" true
            (Option.is_none absent.body);
          Alcotest.(check int)
            "an absent definition stops at its closing parenthesis"
            absent.closing_parenthesis.span.stop absent.location.span.stop
      | definitions ->
          Alcotest.failf "expected five function definitions, got %d"
            (List.length definitions))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let function_definition_streaming_visibility () =
  let source =
    "U0 Visible()\n\
     #ifdef Visible\n\
     {return;}\n\
     #else\n\
     Widget wrong;\n\
     #endif\n\
     U8 selected;"
  in
  let session, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [ Ast.Function_definition definition; Ast.Global_variable selected ] -> (
      Alcotest.(check string)
        "the conditional sees the function" "Visible" definition.name.spelling;
      Alcotest.(check string)
        "the selected branch remains in source order" "selected"
        selected.name.spelling;
      ignore (expect_function_body definition |> expect_block_statement);
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) "Visible"
      with
      | Symbol_visibility.Present entry ->
          Alcotest.(check bool)
            "the visible symbol is a function" true
            (Symbol_visibility.kind entry = Symbol_visibility.Function)
      | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local ->
          Alcotest.fail "the function was not published before its body")
  | items ->
      Alcotest.failf "expected a function and following global, got %d items"
        (List.length items)

let function_definition_provenance () =
  let source = "#define HEAD public U0 Generated()\nHEAD {return;}" in
  let session, root, output = parse_string source in
  let definition = expect_ast output |> expect_one_definition in
  let generated_locations =
    [
      (List.hd definition.modifiers).location;
      definition.return_type.location;
      definition.name.location;
      definition.opening_parenthesis;
      definition.closing_parenthesis;
    ]
  in
  List.iter
    (fun (location : Ast.location) ->
      Alcotest.(check bool)
        "header token comes from a generated frame" false
        (Source_id.equal location.span.source (Source_file.id root));
      Alcotest.(check bool)
        "header token retains its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "header token retains its definition" true
        (Option.is_some location.defined_at))
    generated_locations;
  let body = expect_function_body definition |> expect_block_statement in
  Alcotest.(check bool)
    "the direct body remains in the root source" true
    (Source_id.equal body.block_location.span.source (Source_file.id root));
  let open Yojson.Safe.Util in
  let item =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
  in
  Alcotest.(check bool)
    "JSON retains generated function provenance" true
    (item |> member "name" |> member "location" |> member "generated_from"
   <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let body_file = Filename.concat directory "body.HC" in
      write_file root_file "U0 Included()\n#include \"body\"";
      write_file body_file "{return;}";
      let include_session = Session.create () in
      let include_root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:include_root
      in
      let included = expect_ast include_output |> expect_one_definition in
      let included_body =
        expect_function_body included |> expect_block_statement
      in
      let body_source =
        Source_manager.find
          (Session.sources include_session)
          included_body.block_opening_brace.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "an included body keeps its canonical source" (Unix.realpath body_file)
        (Source_file.path body_source))

let function_definition_failures () =
  let session, _, malformed =
    parse_string "U0 Broken(){else;} U0 Recovered();"
  in
  Alcotest.(check bool)
    "a malformed definition has no AST" true
    (Option.is_none malformed.ast);
  Alcotest.(check string)
    "the body keeps its ordinary statement diagnostic" "HCPARSE0055"
    (first_diagnostic malformed).code;
  List.iter
    (fun name ->
      match
        Symbol_visibility.Environment.find_preprocessor
          (Session.symbols session) name
      with
      | Symbol_visibility.Present entry ->
          Alcotest.(check bool)
            (name ^ " remains a function symbol")
            true
            (Symbol_visibility.kind entry = Symbol_visibility.Function)
      | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local ->
          Alcotest.failf "expected recovered function symbol %s" name)
    [ "Broken"; "Recovered" ];
  let rejected_session, _, rejected = parse_string "U0 Bad(I64 value,){}" in
  Alcotest.(check string)
    "a malformed definition parameter uses the shared diagnostic" "HCPARSE0009"
    (first_diagnostic rejected).code;
  (match
     Symbol_visibility.Environment.find_preprocessor
       (Session.symbols rejected_session)
       "Bad"
   with
  | Symbol_visibility.Absent -> ()
  | Symbol_visibility.Present _ | Symbol_visibility.Shadowed_by_local ->
      Alcotest.fail "a definition with a rejected header became visible");
  let _, _, bound_body = parse_string "extern U0 Prototype(){}" in
  Alcotest.(check string)
    "a bound form remains a prototype" "HCPARSE0016"
    (first_diagnostic bound_body).code;
  let depth = Parser.max_block_depth + 1 in
  let nested_source =
    "U0 TooDeep()" ^ String.make depth '{' ^ String.make depth '}'
  in
  let _, _, nested = parse_string nested_source in
  Alcotest.(check bool)
    "excessive definition nesting has no AST" true
    (Option.is_none nested.ast);
  Alcotest.(check string)
    "definition nesting uses the block diagnostic" "HCPARSE0051"
    (first_diagnostic nested).code

let deterministic_function_definition_dumps () =
  let session, _, output =
    parse_string "public U0 Present(){return;} U0 Absent()"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human definition dump repeats byte for byte" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON definition dump repeats byte for byte" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump distinguishes a present body" true
    (contains human "body=present");
  Alcotest.(check bool)
    "human dump distinguishes an absent body" true
    (contains human "body=absent");
  let open Yojson.Safe.Util in
  let items =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
  in
  Alcotest.(check (list string))
    "JSON uses the definition kind"
    [ "function_definition"; "function_definition" ]
    (List.map (fun item -> item |> member "kind" |> to_string) items);
  Alcotest.(check string)
    "JSON keeps a present return body" "block_statement"
    (List.hd items |> member "body" |> member "kind" |> to_string);
  Alcotest.(check bool)
    "JSON uses null for an absent body" true
    (List.nth items 1 |> member "body" = `Null)

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
        | Ast.Function_definition _ ->
            Alcotest.fail
              "independent declarations unexpectedly formed a function"
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
         | Ast.Function_definition _ ->
             Alcotest.fail "conditional fixture unexpectedly formed a function"
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
        | Ast.Function_definition _
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

let compound_block_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsStmt recognizes an opening brace", "if (cc->token=='{') {");
      ("PrsStmt advances past the opening brace", "Lex(cc);");
      ( "PrsStmt stops a block at its closing brace",
        "while (cc->token!='}' && cc->token!=TK_EOF)" );
      ( "PrsStmt parses block children recursively",
        "PrsStmt(cc,try_cnt,lb_break);" );
      ( "a comma after a block continues its statement group",
        "if (Lex(cc)!=',') goto sm_done;" );
    ];
  let driver = pinned "Compiler/CMain.HC" in
  Alcotest.(check bool)
    "AOT compilation can encounter blocks at top level" true
    (contains driver "while (cc->token!=TK_EOF)")

let compound_block_shapes () =
  let source = "I64 value;{value=1;;'A';}" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      let block =
        match (expect_ast output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_block_statement statement
        | items ->
            Alcotest.failf "expected one declaration and one block, got %d"
              (List.length items)
      in
      Alcotest.(check int)
        "opening brace is one byte" 1
        (Span.length block.block_opening_brace.span);
      Alcotest.(check int)
        "closing brace is one byte" 1
        (Span.length block.block_closing_brace.span);
      Alcotest.(check int)
        "block keeps three child statements" 3
        (List.length block.block_statements);
      let expression =
        List.nth block.block_statements 0 |> expect_expression_statement
      in
      let empty = List.nth block.block_statements 1 |> expect_empty_statement in
      let output_statement =
        match List.nth block.block_statements 2 with
        | Ast.Implicit_output_statement statement -> statement
        | _ -> Alcotest.fail "expected PutChars as the final block child"
      in
      Alcotest.(check bool)
        "assignment precedes the empty statement" true
        (expression.expression_statement_location.span.stop
       <= empty.empty_statement_location.span.start);
      Alcotest.(check bool)
        "empty statement precedes PutChars" true
        (empty.empty_statement_location.span.stop
       <= output_statement.location.span.start);
      Alcotest.(check bool)
        "block span includes both braces" true
        (block.block_location.span.start = block.block_opening_brace.span.start
        && block.block_location.span.stop = block.block_closing_brace.span.stop
        ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_block_and_sequence_shapes () =
  let source = "I64 value;{{}value=1,value++;},value--;" in
  let _, _, output = parse_string source in
  let top_sequence =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_statement_sequence statement
    | items ->
        Alcotest.failf "expected one declaration and one sequence, got %d"
          (List.length items)
  in
  Alcotest.(check int)
    "top-level sequence has two elements" 2
    (List.length top_sequence.sequence_elements);
  let first_element = List.nth top_sequence.sequence_elements 0 in
  Alcotest.(check int)
    "comma after the outer block remains explicit" 1
    (List.length first_element.sequence_following_commas);
  let outer = expect_block_statement first_element.sequence_statement in
  Alcotest.(check int)
    "outer block has a nested block and sequence" 2
    (List.length outer.block_statements);
  let inner = List.nth outer.block_statements 0 |> expect_block_statement in
  Alcotest.(check int)
    "nested empty block has no children" 0
    (List.length inner.block_statements);
  let inner_sequence =
    List.nth outer.block_statements 1 |> expect_statement_sequence
  in
  Alcotest.(check int)
    "inner comma sequence has two expressions" 2
    (List.length inner_sequence.sequence_elements);
  let trailing =
    List.nth top_sequence.sequence_elements 1 |> fun element ->
    expect_expression_statement element.sequence_statement
  in
  let postfix =
    expect_postfix_expression trailing.expression_statement_expression
  in
  Alcotest.(check bool)
    "statement after the block is a decrement" true
    (postfix.postfix_operator_kind = Ast.Post_decrement)

let compound_block_modes_and_order () =
  let source = "I64 before;{}I64 after;{;}" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable before;
       Ast.Top_level_statement (Ast.Block_statement first);
       Ast.Global_variable after;
       Ast.Top_level_statement (Ast.Block_statement second);
      ] ->
          Alcotest.(check string) "first global" "before" before.name.spelling;
          Alcotest.(check int)
            "first block is empty" 0
            (List.length first.block_statements);
          Alcotest.(check string) "second global" "after" after.name.spelling;
          Alcotest.(check int)
            "second block contains one empty statement" 1
            (List.length second.block_statements);
          Alcotest.(check bool)
            "mixed top-level items retain source order" true
            (before.location.span.start < first.block_location.span.start
            && first.block_location.span.start < after.location.span.start
            && after.location.span.start < second.block_location.span.start)
      | items ->
          Alcotest.failf "expected four mixed top-level items, got %d"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let compound_block_provenance () =
  let source =
    "#define OPEN {\n#define CLOSE }\nI64 value;\nOPEN value=1; CLOSE"
  in
  let _, _, output = parse_string source in
  let block =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_block_statement statement
    | _ -> Alcotest.fail "expected a definition-backed compound block"
  in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("opening brace", block.block_opening_brace);
      ("closing brace", block.block_closing_brace);
    ];
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let block_file = Filename.concat directory "block.HC" in
      write_file root_file "I64 value;\n#include \"block\"";
      write_file block_file "{value=1;}";
      let session = Session.create () in
      let root = Session.load_source session ~path:root_file |> Result.get_ok in
      let output =
        Holyc_lib.parse_detailed session ~config:(config directory) ~source:root
      in
      let block =
        match (expect_ast output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_block_statement statement
        | _ -> Alcotest.fail "expected one included compound block"
      in
      let included_source =
        Source_manager.find (Session.sources session)
          block.block_opening_brace.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included brace keeps its canonical path" (Unix.realpath block_file)
        (Source_file.path included_source))

let compound_block_failures () =
  List.iter
    (fun (name, source, code) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      Alcotest.(check string)
        (name ^ " diagnostic") code (first_diagnostic output).code)
    [
      ("missing closing brace", "{;", "HCPARSE0049");
      ("unmatched closing brace", "}", "HCPARSE0050");
      ("comma before closing brace", "I64 value;{value=1,}", "HCPARSE0048");
      ("invalid block expression", "I64 value;{value=;}", "HCPARSE0018");
      ("leading comma before close", "{,}", "HCPARSE0048");
    ];
  let _, _, recovered = parse_string "{\"x\"}" in
  Alcotest.(check bool)
    "failed implicit output has no AST" true
    (Option.is_none recovered.ast);
  Alcotest.(check (list string))
    "recovery stops at the block close" [ "HCPARSE0046" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       recovered.diagnostics);
  let excessive_nesting =
    String.make (Parser.max_block_depth + 1) '{'
    ^ String.make (Parser.max_block_depth + 1) '}'
  in
  let _, _, nested = parse_string excessive_nesting in
  Alcotest.(check bool)
    "excessive block nesting has no AST" true
    (Option.is_none nested.ast);
  let diagnostic = first_diagnostic nested in
  Alcotest.(check string)
    "excessive block nesting diagnostic" "HCPARSE0051" diagnostic.code;
  Alcotest.(check (list string))
    "excessive block recovery consumes the rejected block" [ "HCPARSE0051" ]
    (List.map (fun item -> item.Diagnostic.code) nested.diagnostics);
  Alcotest.(check bool)
    "excessive block nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_block_depth))

let deterministic_compound_block_dumps () =
  let session, _, output = parse_string "I64 value;{{}value=1;}" in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human block dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON block dump is deterministic" json
    (Ast_dump.json sources ast);
  let open Yojson.Safe.Util in
  let block =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth_opt items 1 |> Option.get |> member "statement"
  in
  Alcotest.(check string)
    "block JSON kind" "block_statement"
    (block |> member "kind" |> to_string);
  Alcotest.(check int)
    "block JSON has two children" 2
    (block |> member "statements" |> to_list |> List.length);
  Alcotest.(check bool)
    "block JSON has an opening brace" true
    (block |> member "opening_brace" <> `Null);
  Alcotest.(check bool)
    "block JSON has a closing brace" true
    (block |> member "closing_brace" <> `Null);
  let nested = block |> member "statements" |> to_list |> List.hd in
  Alcotest.(check string)
    "nested JSON kind" "block_statement"
    (nested |> member "kind" |> to_string)

let if_statement_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsIf requires an opening parenthesis", "if (cc->token!='(')");
      ("PrsIf parses the condition", "if (!PrsExpression(cc,NULL,FALSE))");
      ("PrsIf requires a closing parenthesis", "if (cc->token!=')')");
      ("PrsIf branches on a zero condition", "ICAdd(cc,IC_BR_ZERO,lb,0);");
      ("PrsIf parses the selected statement", "PrsStmt(cc,try_cnt,lb_break);");
      ("PrsIf recognizes else", "if (k==KW_ELSE) {");
      ("PrsIf jumps over the else branch", "ICAdd(cc,IC_JMP,lb1,0);");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "if keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_IF\t\t6");
  Alcotest.(check bool)
    "else keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_ELSE\t\t7");
  Alcotest.(check bool)
    "the language guide documents chained conditions" true
    (contains (pinned "Doc/HolyC.DD") "if (13<=age<20)")

let if_statement_shapes () =
  let source = "I64 value;if(value)value=1;else;" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      let statement =
        match (expect_ast output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_if_statement statement
        | items ->
            Alcotest.failf "expected one declaration and one if, got %d"
              (List.length items)
      in
      Alcotest.(check int)
        "if keyword is two bytes" 2
        (Span.length statement.if_keyword.span);
      Alcotest.(check int)
        "opening parenthesis is one byte" 1
        (Span.length statement.if_opening_parenthesis.span);
      Alcotest.(check int)
        "closing parenthesis is one byte" 1
        (Span.length statement.if_closing_parenthesis.span);
      Alcotest.(check string)
        "condition identifier" "value"
        (expect_identifier_expression statement.if_condition).spelling;
      ignore (expect_expression_statement statement.if_then_branch);
      let else_clause = Option.get statement.if_else_clause in
      ignore (expect_empty_statement else_clause.else_branch);
      Alcotest.(check bool)
        "if span covers its else branch" true
        (statement.if_location.span.start = statement.if_keyword.span.start
        && statement.if_location.span.stop = else_clause.else_location.span.stop
        ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_if_else_binding () =
  let source = "I64 a;I64 b;if(a)if(b)'T';else'F';if(a){'Y';}else{'N';}" in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Global_variable _;
   Ast.Top_level_statement first;
   Ast.Top_level_statement second;
  ] ->
      let outer = expect_if_statement first in
      Alcotest.(check bool)
        "outer if has no else" true
        (Option.is_none outer.if_else_clause);
      let inner = expect_if_statement outer.if_then_branch in
      Alcotest.(check bool)
        "else binds to the nearest if" true
        (Option.is_some inner.if_else_clause);
      let block_if = expect_if_statement second in
      let then_block = expect_block_statement block_if.if_then_branch in
      let else_block =
        Option.get block_if.if_else_clause |> fun clause ->
        expect_block_statement clause.else_branch
      in
      Alcotest.(check int)
        "then block keeps one statement" 1
        (List.length then_block.block_statements);
      Alcotest.(check int)
        "else block keeps one statement" 1
        (List.length else_block.block_statements)
  | items ->
      Alcotest.failf "expected two declarations and two if statements, got %d"
        (List.length items)

let if_branch_sequences_and_order () =
  let source = "I64 value;if(value),value=1,value++;else{value=2;}value--;" in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement conditional;
   Ast.Top_level_statement trailing;
  ] ->
      let conditional = expect_if_statement conditional in
      let then_sequence =
        expect_statement_sequence conditional.if_then_branch
      in
      Alcotest.(check int)
        "then branch keeps its leading comma" 1
        (List.length then_sequence.sequence_leading_commas);
      Alcotest.(check int)
        "then branch keeps both expressions" 2
        (List.length then_sequence.sequence_elements);
      let else_block =
        Option.get conditional.if_else_clause |> fun clause ->
        expect_block_statement clause.else_branch
      in
      Alcotest.(check int)
        "else branch is a block with one child" 1
        (List.length else_block.block_statements);
      let trailing = expect_expression_statement trailing in
      Alcotest.(check bool)
        "statement after if stays outside the conditional" true
        (conditional.if_location.span.stop
       <= trailing.expression_statement_location.span.start)
  | items ->
      Alcotest.failf
        "expected a declaration, an if, and a trailing statement, got %d"
        (List.length items)

let if_statement_provenance () =
  let source =
    "#define IF if\n\
     #define OPEN (\n\
     #define CLOSE )\n\
     #define OTHERWISE else\n\
     I64 value;\n\
     IF OPEN value CLOSE value=1; OTHERWISE value=2;"
  in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_if_statement statement
    | _ -> Alcotest.fail "expected one definition-backed if statement"
  in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("if keyword", statement.if_keyword);
      ("opening parenthesis", statement.if_opening_parenthesis);
      ("closing parenthesis", statement.if_closing_parenthesis);
      ("else keyword", (Option.get statement.if_else_clause).else_keyword);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated if punctuation provenance" true
    (statement_json
    |> member "opening_parenthesis"
    |> member "generated_from" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let conditional_file = Filename.concat directory "conditional.HC" in
      write_file root_file "I64 value;\n#include \"conditional\"";
      write_file conditional_file "if(value)value=1;";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_if =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_if_statement statement
        | _ -> Alcotest.fail "expected one included if statement"
      in
      let included_source =
        Source_manager.find
          (Session.sources include_session)
          included_if.if_keyword.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included if keeps its canonical path"
        (Unix.realpath conditional_file)
        (Source_file.path included_source))

let if_statement_failures () =
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
      ( "missing opening parenthesis",
        "I64 value;if value)value=1;",
        "HCPARSE0052",
        "expected '(' after 'if'" );
      ( "missing condition",
        "I64 value;if()value=1;",
        "HCPARSE0018",
        "if condition expression operand" );
      ( "missing closing parenthesis",
        "I64 value;if(value value=1;",
        "HCPARSE0053",
        "expected ')' after the if condition" );
      ( "missing then branch",
        "I64 value;if(value)",
        "HCPARSE0054",
        "statement after the if condition" );
      ("unmatched else", "else;", "HCPARSE0055", "without a matching 'if'");
      ( "missing else branch",
        "I64 value;if(value)value=1;else",
        "HCPARSE0056",
        "statement after 'else'" );
    ];
  let nested_source =
    "I64 value;"
    ^ String.concat ""
        (List.init (Parser.max_conditional_depth + 1) (fun _ -> "if(value)"))
    ^ ";"
  in
  let _, _, nested = parse_string nested_source in
  let diagnostic = first_diagnostic nested in
  Alcotest.(check string)
    "excessive conditional nesting diagnostic" "HCPARSE0057" diagnostic.code;
  Alcotest.(check (list string))
    "conditional recovery reports one depth error" [ "HCPARSE0057" ]
    (List.map (fun item -> item.Diagnostic.code) nested.diagnostics);
  Alcotest.(check bool)
    "conditional nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_conditional_depth))

let deterministic_if_dumps () =
  let session, _, output =
    parse_string "I64 value;if(value){'Y';}else if(value),value=1,value++;"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human if dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON if dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies the else clause" true
    (contains human "else_clause");
  let open Yojson.Safe.Util in
  let conditional =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check string)
    "JSON conditional kind" "if_statement"
    (conditional |> member "kind" |> to_string);
  let nested = conditional |> member "else_clause" |> member "branch" in
  Alcotest.(check string)
    "JSON else branch keeps nested if" "if_statement"
    (nested |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON nested then branch keeps sequence" "statement_sequence"
    (nested |> member "then_branch" |> member "kind" |> to_string)

let while_statement_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsWhile requires an opening parenthesis", "if (cc->token!='(')");
      ("PrsWhile parses the condition", "if (!PrsExpression(cc,NULL,FALSE))");
      ("PrsWhile requires a closing parenthesis", "if (cc->token!=')')");
      ("PrsWhile parses a body statement", "PrsStmt(cc,try_cnt,lb_done);");
      ("PrsWhile emits its back edge", "ICAdd(cc,IC_JMP,lb,0);");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "while keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_WHILE\t9");
  Alcotest.(check bool)
    "the kernel corpus uses compound while bodies" true
    (contains (pinned "Kernel/Job.HC") "while (tmpc!=head) {")

let while_statement_shapes () =
  let source = "I64 value;while(value)value--;" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      let statement =
        match (expect_ast output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_while_statement statement
        | items ->
            Alcotest.failf "expected one declaration and one while, got %d"
              (List.length items)
      in
      Alcotest.(check int)
        "while keyword is five bytes" 5
        (Span.length statement.while_keyword.span);
      Alcotest.(check int)
        "opening parenthesis is one byte" 1
        (Span.length statement.while_opening_parenthesis.span);
      Alcotest.(check int)
        "closing parenthesis is one byte" 1
        (Span.length statement.while_closing_parenthesis.span);
      Alcotest.(check string)
        "condition identifier" "value"
        (expect_identifier_expression statement.while_condition).spelling;
      let body = expect_expression_statement statement.while_body in
      Alcotest.(check bool)
        "while span covers its body" true
        (statement.while_location.span.start
         = statement.while_keyword.span.start
        && statement.while_location.span.stop
           = body.expression_statement_location.span.stop))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let while_body_shapes_and_else_binding () =
  let source =
    "I64 a;I64 \
     b;while(a){while(b),b--,b++;}if(a)while(b);else;while(a)if(b);else;"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Global_variable _;
   Ast.Top_level_statement block_loop;
   Ast.Top_level_statement outer_if;
   Ast.Top_level_statement outer_loop;
  ] ->
      let block =
        expect_while_statement block_loop |> fun loop ->
        loop.while_body |> expect_block_statement
      in
      let nested_loop =
        match block.block_statements with
        | [ statement ] -> expect_while_statement statement
        | statements ->
            Alcotest.failf "expected one nested loop, got %d statements"
              (List.length statements)
      in
      let sequence = expect_statement_sequence nested_loop.while_body in
      Alcotest.(check int)
        "loop body keeps its leading comma" 1
        (List.length sequence.sequence_leading_commas);
      Alcotest.(check int)
        "loop body keeps two expressions" 2
        (List.length sequence.sequence_elements);
      let outer_if = expect_if_statement outer_if in
      ignore (expect_while_statement outer_if.if_then_branch);
      Alcotest.(check bool)
        "else after a while body belongs to the outer if" true
        (Option.is_some outer_if.if_else_clause);
      let outer_loop = expect_while_statement outer_loop in
      let inner_if = expect_if_statement outer_loop.while_body in
      Alcotest.(check bool)
        "else inside a while body belongs to the inner if" true
        (Option.is_some inner_if.if_else_clause)
  | items ->
      Alcotest.failf
        "expected two declarations and three control-flow statements, got %d"
        (List.length items)

let while_statement_provenance () =
  let source =
    "#define LOOP while\n\
     #define OPEN (\n\
     #define CLOSE )\n\
     I64 value;\n\
     LOOP OPEN value CLOSE value--;"
  in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_while_statement statement
    | _ -> Alcotest.fail "expected one definition-backed while statement"
  in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("while keyword", statement.while_keyword);
      ("opening parenthesis", statement.while_opening_parenthesis);
      ("closing parenthesis", statement.while_closing_parenthesis);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated while punctuation provenance" true
    (statement_json
    |> member "opening_parenthesis"
    |> member "generated_from" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let loop_file = Filename.concat directory "loop.HC" in
      write_file root_file "I64 value;\n#include \"loop\"";
      write_file loop_file "while(value)value--;";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_loop =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_while_statement statement
        | _ -> Alcotest.fail "expected one included while statement"
      in
      let included_source =
        Source_manager.find
          (Session.sources include_session)
          included_loop.while_keyword.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included while keeps its canonical path" (Unix.realpath loop_file)
        (Source_file.path included_source))

let while_statement_failures () =
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
      ( "missing opening parenthesis",
        "I64 value;while value)value--;",
        "HCPARSE0058",
        "expected '(' after 'while'" );
      ( "missing condition",
        "I64 value;while()value--;",
        "HCPARSE0018",
        "while condition expression operand" );
      ( "missing closing parenthesis",
        "I64 value;while(value value--;",
        "HCPARSE0059",
        "expected ')' after the while condition" );
      ( "missing body",
        "I64 value;while(value)",
        "HCPARSE0060",
        "statement after the while condition" );
      ( "comma-only body",
        "I64 value;while(value),,,",
        "HCPARSE0060",
        "found only statement commas" );
    ];
  let nested_source =
    "I64 value;"
    ^ String.concat ""
        (List.init (Parser.max_loop_depth + 1) (fun _ -> "while(value)"))
    ^ ";"
  in
  let _, _, nested = parse_string nested_source in
  let diagnostic = first_diagnostic nested in
  Alcotest.(check string)
    "excessive loop nesting diagnostic" "HCPARSE0061" diagnostic.code;
  Alcotest.(check (list string))
    "loop recovery reports one depth error" [ "HCPARSE0061" ]
    (List.map (fun item -> item.Diagnostic.code) nested.diagnostics);
  Alcotest.(check bool)
    "loop nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_loop_depth))

let deterministic_while_dumps () =
  let session, _, output =
    parse_string "I64 value;while(value){if(value)value--;else;}"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human while dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON while dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies the while body" true
    (contains human "while_statement");
  let open Yojson.Safe.Util in
  let loop =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check string)
    "JSON loop kind" "while_statement"
    (loop |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON loop body keeps its block" "block_statement"
    (loop |> member "body" |> member "kind" |> to_string);
  let nested =
    loop |> member "body" |> member "statements" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "JSON loop block keeps its conditional" "if_statement"
    (nested |> member "kind" |> to_string)

let do_while_statement_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsDoWhile parses its body first", "PrsStmt(cc,try_cnt,lb_done);");
      ("PrsDoWhile requires while", "if (PrsKeyWord(cc)!=KW_WHILE)");
      ("PrsDoWhile requires an opening parenthesis", "if (Lex(cc)!='(')");
      ("PrsDoWhile parses the condition", "if (!PrsExpression(cc,NULL,FALSE))");
      ("PrsDoWhile requires a closing parenthesis", "if (cc->token!=')')");
      ( "PrsDoWhile branches on a nonzero condition",
        "ICAdd(cc,IC_BR_NOT_ZERO,lb,0);" );
      ("PrsDoWhile requires a semicolon", "if (Lex(cc)!=';')");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "do keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_DO\t\t15");
  Alcotest.(check bool)
    "while keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_WHILE\t9");
  let kernel_job = pinned "Kernel/Job.HC" in
  Alcotest.(check bool)
    "the kernel corpus has a compound do body" true
    (contains kernel_job "do {");
  Alcotest.(check bool)
    "the kernel corpus closes the post-test loop" true
    (contains kernel_job "} while (task=task->popup_task);")

let do_while_statement_shapes () =
  let source = "I64 value;do value--;while(value);" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      let statement =
        match (expect_ast output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_do_while_statement statement
        | items ->
            Alcotest.failf "expected one declaration and one do-while, got %d"
              (List.length items)
      in
      Alcotest.(check int)
        "do keyword is two bytes" 2
        (Span.length statement.do_keyword.span);
      Alcotest.(check int)
        "while keyword is five bytes" 5
        (Span.length statement.do_while_keyword.span);
      Alcotest.(check int)
        "opening parenthesis is one byte" 1
        (Span.length statement.do_while_opening_parenthesis.span);
      Alcotest.(check int)
        "closing parenthesis is one byte" 1
        (Span.length statement.do_while_closing_parenthesis.span);
      Alcotest.(check int)
        "trailing semicolon is one byte" 1
        (Span.length statement.do_while_semicolon.span);
      ignore (expect_expression_statement statement.do_body);
      Alcotest.(check string)
        "condition identifier" "value"
        (expect_identifier_expression statement.do_while_condition).spelling;
      Alcotest.(check bool)
        "do-while span covers its semicolon" true
        (statement.do_while_location.span.start
         = statement.do_keyword.span.start
        && statement.do_while_location.span.stop
           = statement.do_while_semicolon.span.stop))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let do_while_body_shapes_and_else_binding () =
  let source =
    "I64 a;I64 b;do{do,b--,b++;while(b);}while(a);if(a)do;while(b);else;do \
     if(a);else;while(b);do while(a);while(b);"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Global_variable _;
   Ast.Top_level_statement block_loop;
   Ast.Top_level_statement outer_if;
   Ast.Top_level_statement outer_do;
   Ast.Top_level_statement pre_test_body;
  ] ->
      let block =
        expect_do_while_statement block_loop |> fun loop ->
        loop.do_body |> expect_block_statement
      in
      let nested_loop =
        match block.block_statements with
        | [ statement ] -> expect_do_while_statement statement
        | statements ->
            Alcotest.failf "expected one nested do-while, got %d statements"
              (List.length statements)
      in
      let sequence = expect_statement_sequence nested_loop.do_body in
      Alcotest.(check int)
        "post-test body keeps its leading comma" 1
        (List.length sequence.sequence_leading_commas);
      Alcotest.(check int)
        "post-test body keeps two expressions" 2
        (List.length sequence.sequence_elements);
      let outer_if = expect_if_statement outer_if in
      ignore (expect_do_while_statement outer_if.if_then_branch);
      Alcotest.(check bool)
        "else after a do-while body belongs to the outer if" true
        (Option.is_some outer_if.if_else_clause);
      let outer_do = expect_do_while_statement outer_do in
      let inner_if = expect_if_statement outer_do.do_body in
      Alcotest.(check bool)
        "else inside a do-while body belongs to the inner if" true
        (Option.is_some inner_if.if_else_clause);
      let pre_test_body = expect_do_while_statement pre_test_body in
      ignore (expect_while_statement pre_test_body.do_body)
  | items ->
      Alcotest.failf
        "expected two declarations and four control-flow statements, got %d"
        (List.length items)

let do_while_statement_provenance () =
  let source =
    "#define BEGIN do\n\
     #define LOOP while\n\
     #define OPEN (\n\
     #define CLOSE )\n\
     #define END ;\n\
     I64 value;\n\
     BEGIN value-- END LOOP OPEN value CLOSE END"
  in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_do_while_statement statement
    | _ -> Alcotest.fail "expected one definition-backed do-while statement"
  in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("do keyword", statement.do_keyword);
      ("while keyword", statement.do_while_keyword);
      ("opening parenthesis", statement.do_while_opening_parenthesis);
      ("closing parenthesis", statement.do_while_closing_parenthesis);
      ("trailing semicolon", statement.do_while_semicolon);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated trailing-clause provenance" true
    (statement_json |> member "while_keyword" |> member "generated_from"
   <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let loop_file = Filename.concat directory "loop.HC" in
      write_file root_file "I64 value;\n#include \"loop\"";
      write_file loop_file "do{value--;}while(value);";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_loop =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_do_while_statement statement
        | _ -> Alcotest.fail "expected one included do-while statement"
      in
      let included_source =
        Source_manager.find
          (Session.sources include_session)
          included_loop.do_keyword.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included do-while keeps its canonical path" (Unix.realpath loop_file)
        (Source_file.path included_source))

let do_while_statement_failures () =
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
      ("missing body", "I64 value;do", "HCPARSE0062", "statement after 'do'");
      ( "comma-only body",
        "I64 value;do,,,,",
        "HCPARSE0062",
        "found only statement commas" );
      ( "missing while keyword",
        "I64 value;do;",
        "HCPARSE0063",
        "expected 'while' after the do-while body" );
      ( "missing opening parenthesis",
        "I64 value;do;while value);",
        "HCPARSE0064",
        "expected '(' after the do-while keyword" );
      ( "missing condition",
        "I64 value;do;while();",
        "HCPARSE0018",
        "do-while condition expression operand" );
      ( "missing closing parenthesis",
        "I64 value;do;while(value;",
        "HCPARSE0065",
        "expected ')' after the do-while condition" );
      ( "missing trailing semicolon",
        "I64 value;do;while(value)",
        "HCPARSE0066",
        "expected ';' after the do-while condition" );
    ];
  let mixed_source =
    "I64 value;"
    ^ String.concat ""
        (List.init Parser.max_loop_depth (fun _ -> "while(value)"))
    ^ "do;while(value);"
  in
  let _, _, mixed = parse_string mixed_source in
  let diagnostic = first_diagnostic mixed in
  Alcotest.(check string)
    "mixed loop nesting diagnostic" "HCPARSE0061" diagnostic.code;
  Alcotest.(check (list string))
    "mixed loop recovery reports one depth error" [ "HCPARSE0061" ]
    (List.map (fun item -> item.Diagnostic.code) mixed.diagnostics);
  Alcotest.(check bool)
    "mixed loop nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_loop_depth))

let deterministic_do_while_dumps () =
  let session, _, output =
    parse_string "I64 value;do{if(value)value--;else;}while(value);"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human do-while dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON do-while dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies the trailing clause" true
    (contains human "while_keyword");
  let open Yojson.Safe.Util in
  let loop =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check string)
    "JSON post-test loop kind" "do_while_statement"
    (loop |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON post-test body keeps its block" "block_statement"
    (loop |> member "body" |> member "kind" |> to_string);
  let nested =
    loop |> member "body" |> member "statements" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "JSON post-test block keeps its conditional" "if_statement"
    (nested |> member "kind" |> to_string)

let for_statement_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsFor requires an opening parenthesis", "if (cc->token!='(')");
      ("PrsFor parses its initializer as a statement", "PrsStmt(cc,try_cnt);");
      ("PrsFor requires a condition", "if (!PrsExpression(cc,NULL,FALSE))");
      ("PrsFor requires a condition semicolon", "if (cc->token!=';')");
      ("PrsFor makes its update optional", "if (cc->token!=')')");
      ("PrsFor disables update semicolon parsing", "PrsStmt(cc,try_cnt,NULL,0);");
      ( "PrsFor parses its body with a break label",
        "PrsStmt(cc,try_cnt,lb_done);" );
      ("PrsFor appends the deferred update", "COCAppend(cc,tmpcbh);");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "for keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_FOR\t\t8");
  Alcotest.(check bool)
    "the kernel corpus uses an empty initializer" true
    (contains (pinned "Kernel/StrPrint.HC") "for (;i<len-k;i++) {");
  let string_utils = pinned "Adam/Opt/Utils/StrUtils.HC" in
  Alcotest.(check bool)
    "the Adam corpus uses an empty update" true
    (contains string_utils "for (i=0;i<cnt;) {");
  Alcotest.(check bool)
    "the Adam corpus uses comma-linked updates" true
    (contains string_utils "j++,i++) {");
  Alcotest.(check bool)
    "the language guide shows a conventional for loop" true
    (contains (pinned "Doc/HolyC.DD") "for (i=0;i<argc;i++)")

let for_statement_shapes () =
  let source = "I64 i;for(i=0;i<3;i++);" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      let statement =
        match (expect_ast output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_for_statement statement
        | items ->
            Alcotest.failf "expected one declaration and one for, got %d"
              (List.length items)
      in
      Alcotest.(check int)
        "for keyword is three bytes" 3
        (Span.length statement.for_keyword.span);
      ignore (expect_expression_statement statement.for_initializer);
      let condition = expect_binary_expression statement.for_condition in
      Alcotest.(check string)
        "condition uses the pinned comparison operator" "IC_LESS"
        condition.binary_operator_spec.ic_name;
      let update =
        match statement.for_update with
        | Some update -> expect_expression_statement update
        | None -> Alcotest.fail "expected a for update"
      in
      Alcotest.(check bool)
        "for update has no semicolon" true
        (Option.is_none update.expression_statement_semicolon);
      ignore (expect_empty_statement statement.for_body);
      Alcotest.(check bool)
        "for span covers its body" true
        (statement.for_location.span.start = statement.for_keyword.span.start
        && statement.for_location.span.stop
           = (Ast.statement_location statement.for_body).span.stop))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let for_header_variants_and_else_binding () =
  let source =
    "I64 i;I64 j;I64 a;I64 \
     b;for(;i<3;);for(i=0;i<3;j++,i++);if(a)for(;b;);else;for(;a;)if(b);else;"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Global_variable _;
   Ast.Global_variable _;
   Ast.Global_variable _;
   Ast.Top_level_statement empty_header;
   Ast.Top_level_statement comma_update;
   Ast.Top_level_statement outer_if;
   Ast.Top_level_statement outer_for;
  ] ->
      let empty_header = expect_for_statement empty_header in
      ignore (expect_empty_statement empty_header.for_initializer);
      Alcotest.(check bool)
        "empty update is represented as absent" true
        (Option.is_none empty_header.for_update);
      let comma_update = expect_for_statement comma_update in
      let update =
        match comma_update.for_update with
        | Some update -> expect_statement_sequence update
        | None -> Alcotest.fail "expected a comma-linked for update"
      in
      Alcotest.(check int)
        "comma-linked update retains both expressions" 2
        (List.length update.sequence_elements);
      let outer_if = expect_if_statement outer_if in
      ignore (expect_for_statement outer_if.if_then_branch);
      Alcotest.(check bool)
        "else after a for body belongs to the outer if" true
        (Option.is_some outer_if.if_else_clause);
      let outer_for = expect_for_statement outer_for in
      let inner_if = expect_if_statement outer_for.for_body in
      Alcotest.(check bool)
        "else inside a for body belongs to the inner if" true
        (Option.is_some inner_if.if_else_clause)
  | items ->
      Alcotest.failf
        "expected four declarations and four control-flow statements, got %d"
        (List.length items)

let for_statement_provenance () =
  let source =
    "#define LOOP for\n\
     #define OPEN (\n\
     #define SEP ;\n\
     #define CLOSE )\n\
     I64 value;\n\
     LOOP OPEN SEP value SEP value-- CLOSE SEP"
  in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_for_statement statement
    | _ -> Alcotest.fail "expected one definition-backed for statement"
  in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("for keyword", statement.for_keyword);
      ("opening parenthesis", statement.for_opening_parenthesis);
      ("condition semicolon", statement.for_condition_semicolon);
      ("closing parenthesis", statement.for_closing_parenthesis);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated condition-delimiter provenance" true
    (statement_json
    |> member "condition_semicolon"
    |> member "generated_from" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let loop_file = Filename.concat directory "loop.HC" in
      write_file root_file "I64 value;\n#include \"loop\"";
      write_file loop_file "for(;value;value--);";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_loop =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_for_statement statement
        | _ -> Alcotest.fail "expected one included for statement"
      in
      let included_source =
        Source_manager.find
          (Session.sources include_session)
          included_loop.for_keyword.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included for keeps its canonical path" (Unix.realpath loop_file)
        (Source_file.path included_source))

let for_statement_failures () =
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
      ( "missing opening parenthesis",
        "I64 i;for;i;",
        "HCPARSE0067",
        "expected '(' after 'for'" );
      ( "missing initializer statement",
        "I64 i;for()i++;",
        "HCPARSE0068",
        "initializer statement in the for header" );
      ( "missing condition",
        "I64 i;for(;;);",
        "HCPARSE0018",
        "for condition expression operand" );
      ( "missing condition semicolon",
        "I64 i;for(;i)i++;",
        "HCPARSE0069",
        "expected ';' after the for condition" );
      ( "update has a semicolon",
        "I64 i;for(;i;i++;);",
        "HCPARSE0070",
        "expected ')' after the for update" );
      ( "missing closing parenthesis",
        "I64 i;for(;i;i++;",
        "HCPARSE0070",
        "expected ')' after the for update" );
      ( "missing body",
        "I64 i;for(;i;i++)",
        "HCPARSE0071",
        "statement after the for header" );
      ( "comma-only body",
        "I64 i;for(;i;i++),,,",
        "HCPARSE0071",
        "found only statement commas" );
    ];
  let _, _, nested_recovery = parse_string "I64 i;for(;i if(i););" in
  Alcotest.(check (list string))
    "header recovery skips a nested parenthesis pair" [ "HCPARSE0069" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       nested_recovery.diagnostics);
  let _, _, block_recovery = parse_string "{for(;);}" in
  Alcotest.(check (list string))
    "header recovery preserves the enclosing block close" [ "HCPARSE0018" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       block_recovery.diagnostics);
  let mixed_source =
    "I64 value;"
    ^ String.concat ""
        (List.init Parser.max_loop_depth (fun _ -> "while(value)"))
    ^ "for(;value;);"
  in
  let _, _, mixed = parse_string mixed_source in
  let diagnostic = first_diagnostic mixed in
  Alcotest.(check string)
    "mixed loop nesting diagnostic" "HCPARSE0061" diagnostic.code;
  Alcotest.(check bool)
    "mixed loop nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_loop_depth))

let deterministic_for_dumps () =
  let session, _, output =
    parse_string "I64 i;for(i=0;i<3;i++){if(i)i--;else;}"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human for dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON for dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies the for update" true
    (contains human "for_statement");
  let open Yojson.Safe.Util in
  let loop =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check string)
    "JSON for kind" "for_statement"
    (loop |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON for initializer keeps a statement" "expression_statement"
    (loop |> member "initializer" |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON for body keeps its block" "block_statement"
    (loop |> member "body" |> member "kind" |> to_string)

let goto_label_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsStmt dispatches goto", "case KW_GOTO:");
      ("goto requires an identifier", "if (Lex(cc)!=TK_IDENT)");
      ("goto looks up an existing label", "COCGoToLabelFind(cc,cc->cur_str)");
      ("goto creates a missing label", "COCMiscNew(cc,CMT_GOTO_LABEL)");
      ("goto counts label uses", "g_lb->use_cnt++;");
      ("goto emits a jump", "ICAdd(cc,IC_JMP,g_lb,0);");
      ("goto uses the common terminator", "goto sm_semicolon;");
      ("labels start on unresolved identifiers", "Ident, not in hash table");
      ("local variables stay expressions", "if (cc->local_var_entry)");
      ("labels are marked as defined", "g_lb->flags|=CMF_DEFINED;");
      ("labels emit their own IC", "ICAdd(cc,IC_LABEL,g_lb,0);");
      ("labels require a colon", "if (Lex(cc)==':')");
      ("duplicate labels are rejected", "Duplicate goto label at");
      ("native compilation rejects global labels", "No global labels at");
      ("the common terminator accepts a comma", "else if (cc->token!=',')");
    ];
  let parser_library = pinned "Compiler/PrsLib.HC" in
  Alcotest.(check bool)
    "language and assembly labels share lookup" true
    (contains parser_library "cm->type==CMT_GOTO_LABEL||cm->type==CMT_ASM_LABEL");
  Alcotest.(check bool)
    "cleanup checks unresolved labels" true
    (contains parser_library "if (!(cm->flags&CMF_DEFINED))");
  Alcotest.(check bool)
    "cleanup warns about unused labels" true
    (contains parser_library "else if (!cm->use_cnt)");
  Alcotest.(check bool)
    "goto keeps its pinned keyword ID" true
    (contains (pinned "Compiler/CompilerA.HH") "#define KW_GOTO\t\t17");
  let kernel_header = pinned "Kernel/KernelA.HH" in
  Alcotest.(check bool)
    "goto labels keep their pinned misc kind" true
    (contains kernel_header "#define CMT_GOTO_LABEL\t\t2");
  Alcotest.(check bool)
    "label definitions keep their pinned flag" true
    (contains kernel_header "#define CMF_DEFINED\t\t0x02");
  Alcotest.(check bool)
    "the language guide recommends goto instead of continue" true
    (contains (pinned "Doc/HolyC.DD")
       "There is no $FG,2$continue$FG$ stmt.  Use $FG,2$goto$FG$.");
  Alcotest.(check bool)
    "the linkage guide records global-name collisions" true
    (contains
       (pinned "Doc/ScopingLinkage.DD")
       "Goto labels must not have the same name as global scope objects");
  Alcotest.(check bool)
    "the compiler corpus uses a forward goto" true
    (contains (pinned "Compiler/UAsm.HC") "goto ief_compare_done;");
  Alcotest.(check bool)
    "the compiler corpus defines the forward target" true
    (contains (pinned "Compiler/UAsm.HC") "ief_compare_done:");
  Alcotest.(check bool)
    "the kernel corpus uses a shared completion label" true
    (contains (pinned "Kernel/Job.HC") "goto jh_done;")

let goto_label_statement_shapes () =
  let source = "goto forward;forward:goto forward;{backward:goto backward;}" in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Top_level_statement forward_goto;
       Ast.Top_level_statement forward_label;
       Ast.Top_level_statement backward_goto;
       Ast.Top_level_statement block;
      ] -> (
          let forward_goto = expect_goto_statement forward_goto in
          Alcotest.(check int)
            "goto keyword is four bytes" 4
            (Span.length forward_goto.goto_keyword.span);
          Alcotest.(check string)
            "forward goto keeps its target" "forward"
            forward_goto.goto_target.spelling;
          Alcotest.(check bool)
            "ordinary goto retains its semicolon" true
            (Option.is_some forward_goto.goto_semicolon);
          Alcotest.(check bool)
            "goto span covers its terminator" true
            (forward_goto.goto_location.span.stop
           = (Option.get forward_goto.goto_semicolon).span.stop);
          let forward_label = expect_label_statement forward_label in
          Alcotest.(check string)
            "forward label keeps its name" "forward"
            forward_label.label_name.spelling;
          Alcotest.(check bool)
            "label span ends at the colon" true
            (forward_label.label_location.span.stop
           = forward_label.label_colon.span.stop);
          Alcotest.(check string)
            "backward goto keeps the same target" "forward"
            (expect_goto_statement backward_goto).goto_target.spelling;
          let block = expect_block_statement block in
          match block.block_statements with
          | [ backward_label; backward_goto ] ->
              Alcotest.(check string)
                "block label keeps its name" "backward"
                (expect_label_statement backward_label).label_name.spelling;
              Alcotest.(check string)
                "block goto keeps its target" "backward"
                (expect_goto_statement backward_goto).goto_target.spelling
          | statements ->
              Alcotest.failf
                "expected a label and goto in the block, got %d statements"
                (List.length statements))
      | items ->
          Alcotest.failf "expected three statements and a block, got %d items"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let goto_label_boundaries_and_routing () =
  let source =
    "I64 active;goto first,first:,goto first;for(;active;goto done)done:"
  in
  let _, _, output = parse_string source in
  (match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement sequence;
   Ast.Top_level_statement for_loop;
  ] ->
      let sequence = expect_statement_sequence sequence in
      Alcotest.(check int)
        "comma sequence keeps goto, label, and goto" 3
        (List.length sequence.sequence_elements);
      let first = List.nth sequence.sequence_elements 0 in
      let first_goto = expect_goto_statement first.sequence_statement in
      Alcotest.(check bool)
        "comma-terminated goto has no semicolon" true
        (Option.is_none first_goto.goto_semicolon);
      Alcotest.(check int)
        "goto element retains its following comma" 1
        (List.length first.sequence_following_commas);
      let second = List.nth sequence.sequence_elements 1 in
      ignore (expect_label_statement second.sequence_statement);
      Alcotest.(check int)
        "label element retains its following comma" 1
        (List.length second.sequence_following_commas);
      ignore
        ( List.nth sequence.sequence_elements 2 |> fun element ->
          expect_goto_statement element.sequence_statement );
      let for_loop = expect_for_statement for_loop in
      let update =
        match for_loop.for_update with
        | Some update -> expect_goto_statement update
        | None -> Alcotest.fail "expected goto in the for update"
      in
      Alcotest.(check bool)
        "for-update goto has no fabricated semicolon" true
        (Option.is_none update.goto_semicolon);
      Alcotest.(check string)
        "for-update goto keeps its target" "done" update.goto_target.spelling;
      Alcotest.(check string)
        "for body accepts the matching label" "done"
        (expect_label_statement for_loop.for_body).label_name.spelling
  | items ->
      Alcotest.failf "expected a declaration, sequence, and for loop, got %d"
        (List.length items));
  List.iter
    (fun (description, source) ->
      let _, _, collision = parse_string source in
      Alcotest.(check bool)
        (description ^ " has no AST")
        true
        (Option.is_none collision.ast);
      Alcotest.(check string)
        (description ^ " keeps expression routing")
        "HCPARSE0047" (first_diagnostic collision).code)
    [
      ("global-variable collision", "I64 occupied;occupied:");
      ("function collision", "extern I64 occupied();occupied:");
    ];
  let _, _, unresolved_declaration = parse_string "Widget value;" in
  Alcotest.(check string)
    "an unresolved name without a colon remains a declaration" "HCPARSE0001"
    (first_diagnostic unresolved_declaration).code

let goto_label_provenance () =
  let source =
    "#define JUMP goto\n\
     #define TARGET finish\n\
     #define END ;\n\
     #define LABEL finish\n\
     #define COLON :\n\
     JUMP TARGET END LABEL COLON"
  in
  let session, _, output = parse_string source in
  let goto_statement, label_statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Top_level_statement goto_statement; Ast.Top_level_statement label ]
      -> (expect_goto_statement goto_statement, expect_label_statement label)
    | _ -> Alcotest.fail "expected definition-backed goto and label statements"
  in
  let semicolon = Option.get goto_statement.goto_semicolon in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("goto keyword", goto_statement.goto_keyword);
      ("goto target", goto_statement.goto_target.location);
      ("goto semicolon", semicolon);
      ("label name", label_statement.label_name.location);
      ("label colon", label_statement.label_colon);
    ];
  let open Yojson.Safe.Util in
  let items =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list
  in
  Alcotest.(check bool)
    "JSON keeps generated goto-target provenance" true
    (List.hd items |> member "statement" |> member "target" |> member "location"
   |> member "generated_from" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let labels_file = Filename.concat directory "labels.HC" in
      write_file root_file "#include \"labels\"";
      write_file labels_file "goto finish;finish:";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_goto, included_label =
        match (expect_ast include_output).Ast.items with
        | [
         Ast.Top_level_statement goto_statement; Ast.Top_level_statement label;
        ] -> (expect_goto_statement goto_statement, expect_label_statement label)
        | _ -> Alcotest.fail "expected one included goto and label"
      in
      List.iter
        (fun (name, (location : Ast.location)) ->
          let included_source =
            Source_manager.find
              (Session.sources include_session)
              location.Ast.span.source
            |> Option.get
          in
          Alcotest.(check string)
            (name ^ " keeps its canonical include path")
            (Unix.realpath labels_file)
            (Source_file.path included_source))
        [
          ("included goto", included_goto.goto_keyword);
          ("included label", included_label.label_name.location);
        ])

let goto_label_failures () =
  List.iter
    (fun (name, source, code, found) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (name ^ " diagnostic") code diagnostic.code;
      Alcotest.(check bool)
        (name ^ " describes the failure")
        true
        (contains diagnostic.message found))
    [
      ("missing target at end of input", "goto", "HCPARSE0075", "end of input");
      ("missing target before semicolon", "goto;", "HCPARSE0075", "found \";\"");
      ("numeric target", "goto 1;", "HCPARSE0075", "found \"1\"");
      ("invalid terminator", "goto done)", "HCPARSE0076", "found \")\"");
      ("following expression", "goto done 1;", "HCPARSE0076", "found \"1\"");
    ];
  let _, _, block_recovery = parse_string "{goto done}" in
  Alcotest.(check (list string))
    "goto recovery preserves the enclosing block close" [ "HCPARSE0076" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       block_recovery.diagnostics);
  let _, _, update_semicolon =
    parse_string "I64 active;for(;active;goto done;)done:"
  in
  Alcotest.(check string)
    "for-update semicolon stays an outer header error" "HCPARSE0070"
    (first_diagnostic update_semicolon).code

let deterministic_goto_label_dumps () =
  let session, _, output = parse_string "goto done;done:" in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human goto-label dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON goto-label dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies goto statements" true
    (contains human "goto_statement");
  Alcotest.(check bool)
    "human dump identifies function labels" true
    (contains human "label_statement");
  let open Yojson.Safe.Util in
  let items =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
  in
  let goto_statement = List.hd items |> member "statement" in
  Alcotest.(check string)
    "JSON keeps the goto kind" "goto_statement"
    (goto_statement |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps the goto target" "done"
    (goto_statement |> member "target" |> member "spelling" |> to_string);
  Alcotest.(check bool)
    "ordinary JSON goto retains its semicolon" true
    (goto_statement |> member "semicolon" <> `Null);
  let label = List.nth items 1 |> member "statement" in
  Alcotest.(check string)
    "JSON keeps the label kind" "label_statement"
    (label |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps the label name" "done"
    (label |> member "name" |> member "spelling" |> to_string)

let lock_statement_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsStmt dispatches lock", "case KW_LOCK:");
      ("lock enters a nested region", "cc->lock_cnt++;");
      ("lock parses an ordinary statement", "PrsStmt(cc,try_cnt);");
      ("lock leaves its nested region", "cc->lock_cnt--;");
    ];
  let parser_library = pinned "Compiler/PrsLib.HC" in
  Alcotest.(check bool)
    "ICAdd observes the active lock region" true
    (contains parser_library "if (cc->lock_cnt)");
  Alcotest.(check bool)
    "ICAdd carries the lock flag" true
    (contains parser_library "flags|=ICF_LOCK;");
  Alcotest.(check bool)
    "lock keeps its pinned keyword ID" true
    (contains (pinned "Compiler/CompilerA.HH") "#define KW_LOCK\t\t42");
  Alcotest.(check bool)
    "the generated keyword source keeps the same ID" true
    (contains (pinned "Compiler/OpCodes.DD") "KEYWORD lock\t\t42;");
  Alcotest.(check bool)
    "the language guide describes lock regions" true
    (contains (pinned "Doc/HolyC.DD")
       "$FG,2$lock{}$FG$ can be used to apply asm");
  let demo = pinned "Demo/MultiCore/Lock.HC" in
  Alcotest.(check bool)
    "the reference demo permits an unbraced lock" true
    (contains demo "lock  //Can be used without {}");
  Alcotest.(check bool)
    "the corpus also uses braced locks" true
    (contains (pinned "Demo/MultiCore/LoadTest.HC") "lock {app_done_ack--;}")

let lock_statement_shapes () =
  let source =
    "I64 value;lock {value++;}lock value++;lock lock value++;lock;"
  in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable _;
       Ast.Top_level_statement braced;
       Ast.Top_level_statement unbraced;
       Ast.Top_level_statement nested;
       Ast.Top_level_statement empty;
      ] ->
          let braced = expect_lock_statement braced in
          Alcotest.(check int)
            "lock keyword is four bytes" 4
            (Span.length braced.lock_keyword.span);
          let block = expect_block_statement braced.lock_body in
          Alcotest.(check int)
            "braced lock retains one statement" 1
            (List.length block.block_statements);
          ignore (List.hd block.block_statements |> expect_expression_statement);
          Alcotest.(check bool)
            "braced lock span ends with its body" true
            (braced.lock_location.span.stop = block.block_location.span.stop);
          let unbraced = expect_lock_statement unbraced in
          let expression = expect_expression_statement unbraced.lock_body in
          Alcotest.(check bool)
            "unbraced lock retains the body semicolon" true
            (Option.is_some expression.expression_statement_semicolon);
          let outer = expect_lock_statement nested in
          let inner = expect_lock_statement outer.lock_body in
          ignore (expect_expression_statement inner.lock_body);
          let empty = expect_lock_statement empty in
          ignore (expect_empty_statement empty.lock_body)
      | items ->
          Alcotest.failf
            "expected a declaration and four lock statements, got %d items"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let lock_statement_boundaries_and_nesting () =
  let source =
    "I64 value;lock value++,value--;if(value)lock value++;else \
     lock;while(value)lock {value--;}for(;value;lock value--;)lock value++;"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement sequence_lock;
   Ast.Top_level_statement conditional;
   Ast.Top_level_statement while_loop;
   Ast.Top_level_statement for_loop;
  ] ->
      let sequence_lock = expect_lock_statement sequence_lock in
      let sequence = expect_statement_sequence sequence_lock.lock_body in
      Alcotest.(check int)
        "one lock covers its comma-linked statement sequence" 2
        (List.length sequence.sequence_elements);
      let first = List.hd sequence.sequence_elements in
      Alcotest.(check int)
        "the first locked expression retains its comma" 1
        (List.length first.sequence_following_commas);
      let conditional = expect_if_statement conditional in
      ignore
        ( conditional.if_then_branch |> expect_lock_statement |> fun statement ->
          expect_expression_statement statement.lock_body );
      let else_lock =
        match conditional.if_else_clause with
        | Some clause -> expect_lock_statement clause.else_branch
        | None -> Alcotest.fail "expected an else lock"
      in
      ignore (expect_empty_statement else_lock.lock_body);
      let while_lock =
        expect_while_statement while_loop |> fun statement ->
        expect_lock_statement statement.while_body
      in
      ignore (expect_block_statement while_lock.lock_body);
      let for_loop = expect_for_statement for_loop in
      let update_lock =
        match for_loop.for_update with
        | Some update -> expect_lock_statement update
        | None -> Alcotest.fail "expected a lock in the for update"
      in
      let update_expression =
        expect_expression_statement update_lock.lock_body
      in
      Alcotest.(check bool)
        "a for-update lock keeps its nested statement semicolon" true
        (Option.is_some update_expression.expression_statement_semicolon);
      ignore
        ( for_loop.for_body |> expect_lock_statement |> fun statement ->
          expect_expression_statement statement.lock_body )
  | items ->
      Alcotest.failf
        "expected a declaration and four structured statements, got %d items"
        (List.length items)

let lock_statement_provenance () =
  let source = "#define ATOMIC lock\n#define END ;\nATOMIC END" in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Top_level_statement statement ] -> expect_lock_statement statement
    | _ -> Alcotest.fail "expected one definition-backed lock statement"
  in
  let body = expect_empty_statement statement.lock_body in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("lock keyword", statement.lock_keyword);
      ("locked semicolon", body.empty_statement_semicolon);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated lock provenance" true
    (statement_json |> member "keyword" |> member "generated_from" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let lock_file = Filename.concat directory "locked.HC" in
      write_file root_file "I64 value;\n#include \"locked\"";
      write_file lock_file "lock value++;";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_lock =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_lock_statement statement
        | _ -> Alcotest.fail "expected one included lock statement"
      in
      let included_source =
        Source_manager.find
          (Session.sources include_session)
          included_lock.lock_keyword.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included lock keeps its canonical path" (Unix.realpath lock_file)
        (Source_file.path included_source))

let lock_statement_failures () =
  List.iter
    (fun (name, source, found) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string)
        (name ^ " diagnostic") "HCPARSE0077" diagnostic.code;
      Alcotest.(check bool)
        (name ^ " describes the missing body")
        true
        (contains diagnostic.message found))
    [
      ("end of input", "lock", "end of input");
      ("closing block", "{lock}", "found \"}\"");
      ("stray else", "lock else", "found \"else\"");
      ("comma-only body", "lock,,,", "only statement commas");
    ];
  let _, _, malformed = parse_string "lock )" in
  Alcotest.(check string)
    "malformed child keeps the ordinary statement diagnostic" "HCPARSE0048"
    (first_diagnostic malformed).code;
  let excessive =
    String.concat "" (List.init (Parser.max_lock_depth + 1) (fun _ -> "lock "))
    ^ ";"
  in
  let _, _, nested = parse_string excessive in
  let diagnostic = first_diagnostic nested in
  Alcotest.(check string)
    "lock nesting diagnostic" "HCPARSE0078" diagnostic.code;
  Alcotest.(check bool)
    "lock nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_lock_depth))

let deterministic_lock_dumps () =
  let session, _, output = parse_string "I64 value;lock {lock value++;}" in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human lock dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON lock dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies lock statements" true
    (contains human "lock_statement");
  let open Yojson.Safe.Util in
  let outer =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check string)
    "JSON keeps the outer lock kind" "lock_statement"
    (outer |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps the braced body" "block_statement"
    (outer |> member "body" |> member "kind" |> to_string);
  let inner =
    outer |> member "body" |> member "statements" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "JSON keeps the nested lock kind" "lock_statement"
    (inner |> member "kind" |> to_string)

let switch_statement_source_behavior () =
  let normalized path =
    pinned path |> String.split_on_char '\r' |> String.concat ""
  in
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsSwitch accepts a bounded header", "if (cc->token=='(')");
      ("PrsSwitch accepts a no-bound header", "else if (cc->token=='[')");
      ("PrsSwitch parses its selector", "PrsExpression(cc,NULL,FALSE)");
      ("PrsSwitch dispatches case labels", "case KW_CASE:");
      ("PrsSwitch recognizes implicit cases", "if (cc->token==':')");
      ("PrsSwitch recognizes case ranges", "cc->token==TK_ELLIPSIS");
      ("PrsSwitch dispatches default labels", "case KW_DFT:");
      ("PrsSwitch opens sub-switch regions", "case KW_START:");
      ("PrsSwitch closes sub-switch regions", "case KW_END:");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  List.iter
    (fun (spelling, id) ->
      Alcotest.(check bool)
        (spelling ^ " keeps its pinned keyword ID")
        true
        (contains compiler_header (Printf.sprintf "#define KW_%s" id)))
    [
      ("switch", "SWITCH\t20");
      ("start", "START\t21");
      ("end", "END\t\t22");
      ("case", "CASE\t\t23");
      ("default", "DFT\t\t24");
    ];
  let opcode_source = pinned "Compiler/OpCodes.DD" in
  List.iter
    (fun fragment ->
      Alcotest.(check bool)
        ("opcode source contains " ^ fragment)
        true
        (contains opcode_source fragment))
    [
      "KEYWORD switch\t\t20;";
      "KEYWORD start \t\t21;";
      "KEYWORD end  \t\t22;";
      "KEYWORD case\t\t23;";
      "KEYWORD default\t\t24;";
    ];
  let guide = pinned "Doc/HolyC.DD" in
  Alcotest.(check bool)
    "the guide documents no-bound switches" true
    (contains guide "switch []");
  Alcotest.(check bool)
    "the guide documents ranged cases" true
    (contains guide "case 4...7:");
  Alcotest.(check bool)
    "the guide documents implicit cases" true
    (contains guide "case: \"Zero");
  Alcotest.(check bool)
    "the guide documents sub-switch regions" true
    (contains guide "start$FG$/$FG,2$end");
  let check_fragments path fragments =
    let source = normalized path in
    List.iter
      (fun (description, fragment) ->
        Alcotest.(check bool) description true (contains source fragment))
      fragments
  in
  check_fragments "Compiler/UAsm.HC"
    [
      ("UAsm uses a no-bound switch", "switch [tmpins->uasm_slash_val] {");
      ("UAsm uses a case range", "case 0...7:");
      ("UAsm opens a sub-switch", "start:");
      ("UAsm closes a sub-switch", "end:");
    ];
  check_fragments "Kernel/Compress.HC"
    [
      ( "the kernel compressor uses a no-bound switch",
        "switch [arc->compression_type] {" );
    ];
  check_fragments "Kernel/StrPrint.HC"
    [
      ("StrPrint contains a nested ordinary switch", "switch (ch1) {");
      ("StrPrint opens a sub-switch", "start:");
      ("StrPrint closes a sub-switch", "end:");
    ];
  check_fragments "Demo/NullCase.HC"
    [
      ("NullCase begins with an implicit case", "case: \"Zero\\n\";");
      ( "NullCase resumes implicit numbering after an explicit case",
        "case: \"Eleven\\n\";" );
    ];
  check_fragments "Demo/SubSwitch.HC"
    [
      ("SubSwitch has an ordinary outer case", "case 4: \"Four \";");
      ("SubSwitch opens its grouped cases", "start:");
      ("SubSwitch closes its grouped cases", "end:");
    ]

let switch_statement_shapes () =
  let source =
    "I64 value;switch(value){case:;case 1:value++;case \
     4...7:value--;default:break;}switch[value]{case 0:;}"
  in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable _;
       Ast.Top_level_statement bounded;
       Ast.Top_level_statement no_bound;
      ] ->
          let bounded = expect_switch_statement bounded in
          Alcotest.(check bool)
            "parentheses select bounded mode" true
            (bounded.switch_mode = Ast.Bounded_switch);
          Alcotest.(check int)
            "bounded switch retains each label and statement" 8
            (List.length bounded.switch_elements);
          let selector =
            expect_identifier_expression bounded.switch_expression
          in
          Alcotest.(check string)
            "switch selector is retained" "value" selector.spelling;
          (match bounded.switch_elements with
          | [
           Ast.Switch_case_element implicit;
           Ast.Switch_statement_element (Ast.Empty_statement _);
           Ast.Switch_case_element single;
           Ast.Switch_statement_element (Ast.Expression_statement _);
           Ast.Switch_case_element ranged;
           Ast.Switch_statement_element (Ast.Expression_statement _);
           Ast.Switch_default_element _;
           Ast.Switch_statement_element (Ast.Break_statement _);
          ] -> (
              Alcotest.(check bool)
                "first case is implicit" true
                (implicit.switch_case_pattern = Ast.Implicit_case);
              (match single.switch_case_pattern with
              | Ast.Single_case expression ->
                  Alcotest.(check int64)
                    "single case keeps its value" 1L
                    (expect_integer_expression expression)
              | _ -> Alcotest.fail "expected a single-valued case");
              match ranged.switch_case_pattern with
              | Ast.Ranged_case range ->
                  Alcotest.(check int64)
                    "range keeps its lower endpoint" 4L
                    (expect_integer_expression range.case_range_start);
                  Alcotest.(check int64)
                    "range keeps its upper endpoint" 7L
                    (expect_integer_expression range.case_range_end);
                  Alcotest.(check int)
                    "range keeps the ellipsis" 3
                    (Span.length range.case_range_ellipsis.span)
              | _ -> Alcotest.fail "expected a ranged case")
          | elements ->
              Alcotest.failf "unexpected bounded switch shape with %d elements"
                (List.length elements));
          let no_bound = expect_switch_statement no_bound in
          Alcotest.(check bool)
            "brackets select no-bound mode" true
            (no_bound.switch_mode = Ast.No_bound_switch);
          Alcotest.(check int)
            "no-bound switch retains its case and body" 2
            (List.length no_bound.switch_elements)
      | items ->
          Alcotest.failf
            "expected a declaration and two switch statements, got %d items"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let switch_subswitch_shapes_and_boundaries () =
  let source =
    "I64 value;switch(value){start:\"[\";start:case 1:value++;end:case \
     2:value--;end:\"]\";break;}switch(value){case 3:switch[value]{case \
     0:;}}if(value)switch[value]{case 0:;}else switch(value){default:;}"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement grouped;
   Ast.Top_level_statement nested;
   Ast.Top_level_statement conditional;
  ] -> (
      let grouped = expect_switch_statement grouped in
      (match grouped.switch_elements with
      | [
       Ast.Switch_subswitch_element outer;
       Ast.Switch_statement_element (Ast.Implicit_output_statement _);
       Ast.Switch_statement_element (Ast.Break_statement _);
      ] -> (
          match outer.subswitch_elements with
          | [
           Ast.Switch_statement_element (Ast.Implicit_output_statement _);
           Ast.Switch_subswitch_element inner;
           Ast.Switch_case_element _;
           Ast.Switch_statement_element (Ast.Expression_statement _);
          ] ->
              Alcotest.(check int)
                "nested sub-switch retains its case and statement" 2
                (List.length inner.subswitch_elements)
          | elements ->
              Alcotest.failf
                "unexpected outer sub-switch shape with %d elements"
                (List.length elements))
      | elements ->
          Alcotest.failf "unexpected grouped switch shape with %d elements"
            (List.length elements));
      let nested = expect_switch_statement nested in
      (match nested.switch_elements with
      | [
       Ast.Switch_case_element _;
       Ast.Switch_statement_element (Ast.Switch_statement child);
      ] ->
          Alcotest.(check bool)
            "ordinary nested switch keeps no-bound mode" true
            (child.switch_mode = Ast.No_bound_switch)
      | elements ->
          Alcotest.failf "unexpected nested switch shape with %d elements"
            (List.length elements));
      let conditional = expect_if_statement conditional in
      ignore (expect_switch_statement conditional.if_then_branch);
      match conditional.if_else_clause with
      | Some clause -> ignore (expect_switch_statement clause.else_branch)
      | None -> Alcotest.fail "expected else after the switch body")
  | items ->
      Alcotest.failf
        "expected a declaration and three structured statements, got %d items"
        (List.length items)

let switch_statement_contexts () =
  let source =
    "I64 value;{switch(value){}}switch(value){value++;case 0:case \
     1:;start:start:end:end:}while(value)switch[value]{}do \
     switch(value){}while(value);for(;value;switch(value){})switch[value]{}lock \
     switch(value){}try switch(value){}catch switch[value]{}"
  in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable _;
       Ast.Top_level_statement block;
       Ast.Top_level_statement ordered;
       Ast.Top_level_statement while_loop;
       Ast.Top_level_statement do_while_loop;
       Ast.Top_level_statement for_loop;
       Ast.Top_level_statement locked;
       Ast.Top_level_statement guarded;
      ] ->
          let block = expect_block_statement block in
          Alcotest.(check int)
            "a block retains its empty switch" 1
            (List.length block.block_statements);
          let empty_switch =
            List.hd block.block_statements |> expect_switch_statement
          in
          Alcotest.(check int)
            "an empty switch has no elements" 0
            (List.length empty_switch.switch_elements);
          let ordered = expect_switch_statement ordered in
          (match ordered.switch_elements with
          | [
           Ast.Switch_statement_element (Ast.Expression_statement _);
           Ast.Switch_case_element _;
           Ast.Switch_case_element _;
           Ast.Switch_statement_element (Ast.Empty_statement _);
           Ast.Switch_subswitch_element outer;
          ] -> (
              match outer.subswitch_elements with
              | [ Ast.Switch_subswitch_element inner ] ->
                  Alcotest.(check int)
                    "an empty nested front porch has no elements" 0
                    (List.length inner.subswitch_elements)
              | elements ->
                  Alcotest.failf
                    "expected one nested sub-switch, got %d outer elements"
                    (List.length elements))
          | elements ->
              Alcotest.failf "unexpected ordered switch shape with %d elements"
                (List.length elements));
          let while_switch =
            expect_while_statement while_loop |> fun statement ->
            expect_switch_statement statement.while_body
          in
          Alcotest.(check bool)
            "a while body retains bracket mode" true
            (while_switch.switch_mode = Ast.No_bound_switch);
          let do_switch =
            expect_do_while_statement do_while_loop |> fun statement ->
            expect_switch_statement statement.do_body
          in
          Alcotest.(check bool)
            "a do body retains parenthesized mode" true
            (do_switch.switch_mode = Ast.Bounded_switch);
          let for_loop = expect_for_statement for_loop in
          (match for_loop.for_update with
          | Some update -> ignore (expect_switch_statement update)
          | None -> Alcotest.fail "expected a switch in the for update");
          ignore (expect_switch_statement for_loop.for_body);
          let lock = expect_lock_statement locked in
          ignore (expect_switch_statement lock.lock_body);
          let guarded = expect_try_catch_statement guarded in
          ignore (expect_switch_statement guarded.try_body);
          ignore (expect_switch_statement guarded.catch_body)
      | items ->
          Alcotest.failf
            "expected a declaration and seven structured statements, got %d"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let switch_statement_provenance () =
  let source =
    "#define SELECT switch\n\
     #define OPEN [\n\
     #define CLOSE ]\n\
     #define BODY_OPEN {\n\
     #define CHOICE case\n\
     #define RANGE ...\n\
     #define LABEL :\n\
     #define SUB_START start\n\
     #define FALLBACK default\n\
     #define SUB_END end\n\
     #define BODY_CLOSE }\n\
     I64 value;\n\
     SELECT OPEN value CLOSE BODY_OPEN CHOICE 1 RANGE 2 LABEL SUB_START LABEL \
     FALLBACK LABEL ; SUB_END LABEL BODY_CLOSE"
  in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_switch_statement statement
    | _ -> Alcotest.fail "expected one definition-backed switch statement"
  in
  let case_label, range, subswitch, default_label =
    match statement.switch_elements with
    | [ Ast.Switch_case_element case_label; Ast.Switch_subswitch_element sub ]
      ->
        let range =
          match case_label.switch_case_pattern with
          | Ast.Ranged_case range -> range
          | _ -> Alcotest.fail "expected a definition-backed case range"
        in
        let default_label =
          match sub.subswitch_elements with
          | [
           Ast.Switch_default_element default_label;
           Ast.Switch_statement_element (Ast.Empty_statement _);
          ] -> default_label
          | _ -> Alcotest.fail "expected a definition-backed default label"
        in
        (case_label, range, sub, default_label)
    | _ -> Alcotest.fail "expected one case and one sub-switch"
  in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("switch keyword", statement.switch_keyword);
      ("opening delimiter", statement.switch_opening_delimiter);
      ("closing delimiter", statement.switch_closing_delimiter);
      ("opening brace", statement.switch_opening_brace);
      ("closing brace", statement.switch_closing_brace);
      ("case keyword", case_label.switch_case_keyword);
      ("case range ellipsis", range.case_range_ellipsis);
      ("case colon", case_label.switch_case_colon);
      ("start keyword", subswitch.subswitch_start_keyword);
      ("start colon", subswitch.subswitch_start_colon);
      ("default keyword", default_label.switch_default_keyword);
      ("default colon", default_label.switch_default_colon);
      ("end keyword", subswitch.subswitch_end_keyword);
      ("end colon", subswitch.subswitch_end_colon);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  let elements = statement_json |> member "elements" |> to_list in
  let case_json = List.nth elements 0 in
  let sub_json = List.nth elements 1 in
  let default_json = sub_json |> member "elements" |> to_list |> List.hd in
  List.iter
    (fun (description, json) ->
      Alcotest.(check bool)
        description true
        (json |> member "generated_from" <> `Null))
    [
      ( "JSON keeps generated switch delimiters",
        member "opening_delimiter" statement_json );
      ( "JSON keeps generated range punctuation",
        case_json |> member "pattern" |> member "ellipsis" );
      ("JSON keeps generated start labels", member "start_keyword" sub_json);
      ("JSON keeps generated default labels", member "keyword" default_json);
      ("JSON keeps generated end labels", member "end_keyword" sub_json);
    ];
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let switch_file = Filename.concat directory "switch.HC" in
      write_file root_file "I64 value;\n#include \"switch\"";
      write_file switch_file "switch[value]{case 1...2:start:default:;end:}";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_switch_statement statement
        | _ -> Alcotest.fail "expected one included switch statement"
      in
      let included_case, included_subswitch, included_default =
        match included.switch_elements with
        | [
         Ast.Switch_case_element case_label; Ast.Switch_subswitch_element sub;
        ] ->
            let default_label =
              match sub.subswitch_elements with
              | [
               Ast.Switch_default_element default_label;
               Ast.Switch_statement_element (Ast.Empty_statement _);
              ] -> default_label
              | _ -> Alcotest.fail "expected an included default label"
            in
            (case_label, sub, default_label)
        | _ -> Alcotest.fail "expected included switch labels"
      in
      let range =
        match included_case.switch_case_pattern with
        | Ast.Ranged_case range -> range
        | _ -> Alcotest.fail "expected an included case range"
      in
      List.iter
        (fun (location : Ast.location) ->
          let source =
            Source_manager.find
              (Session.sources include_session)
              location.Ast.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "included switch token keeps its canonical path"
            (Unix.realpath switch_file)
            (Source_file.path source))
        [
          included.switch_keyword;
          included.switch_opening_delimiter;
          included.switch_closing_delimiter;
          included.switch_opening_brace;
          included.switch_closing_brace;
          included_case.switch_case_keyword;
          range.case_range_ellipsis;
          included_case.switch_case_colon;
          included_subswitch.subswitch_start_keyword;
          included_default.switch_default_keyword;
          included_subswitch.subswitch_end_keyword;
        ])

let switch_statement_failures () =
  List.iter
    (fun (name, source, code, found) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (name ^ " diagnostic") code diagnostic.code;
      Alcotest.(check bool)
        (name ^ " describes the failure")
        true
        (contains diagnostic.message found))
    [
      ("missing opening delimiter", "switch", "HCPARSE0085", "'(' or '['");
      ("missing selector", "switch(){}", "HCPARSE0018", "switch expression");
      ("wrong bounded close", "switch(value]{}", "HCPARSE0086", "\")\"");
      ("wrong no-bound close", "switch[value){}", "HCPARSE0086", "\"]\"");
      ("missing body", "switch(value);", "HCPARSE0087", "'{'");
      ("unterminated body", "switch(value){", "HCPARSE0088", "'}'");
      ("case without colon", "switch(value){case 1}", "HCPARSE0091", "':'");
      ( "range without endpoint",
        "switch(value){case 1...:}",
        "HCPARSE0090",
        "after the case range" );
      ( "default without colon",
        "switch(value){default ;}",
        "HCPARSE0092",
        "after 'default'" );
      ( "start without colon",
        "switch(value){start case 0:;end:}",
        "HCPARSE0093",
        "after 'start'" );
      ( "unmatched end",
        "switch(value){end:}",
        "HCPARSE0094",
        "without a matching 'start:'" );
      ( "missing end",
        "switch(value){start:case 0:;}",
        "HCPARSE0095",
        "before the enclosing switch" );
      ( "end without colon",
        "switch(value){start:case 0:;end }",
        "HCPARSE0096",
        "after 'end'" );
    ];
  let nested_switches =
    String.concat ""
      (List.init (Parser.max_switch_depth + 1) (fun _ ->
           "switch(value){case 0:"))
    ^ String.make (Parser.max_switch_depth + 1) '}'
  in
  let _, _, nested = parse_string ("I64 value;" ^ nested_switches) in
  let diagnostic = first_diagnostic nested in
  Alcotest.(check string)
    "switch nesting diagnostic" "HCPARSE0084" diagnostic.code;
  Alcotest.(check bool)
    "switch nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_switch_depth));
  let nested_subswitches =
    "I64 value;switch(value){"
    ^ String.concat ""
        (List.init (Parser.max_switch_depth + 1) (fun _ -> "start:"))
    ^ String.concat ""
        (List.init (Parser.max_switch_depth + 1) (fun _ -> "end:"))
    ^ "}"
  in
  let _, _, nested = parse_string nested_subswitches in
  let diagnostic = first_diagnostic nested in
  Alcotest.(check string)
    "sub-switch nesting diagnostic" "HCPARSE0097" diagnostic.code;
  Alcotest.(check bool)
    "sub-switch nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_switch_depth));
  let _, _, malformed_child = parse_string "switch(value){)case 1:;}" in
  Alcotest.(check bool)
    "a malformed switch child exposes no AST" true
    (Option.is_none malformed_child.ast);
  Alcotest.(check (list string))
    "child recovery stops at the next case label" [ "HCPARSE0048" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       malformed_child.diagnostics);
  let _, _, label_recovery = parse_string "switch(value){case 1 case 2:;}" in
  Alcotest.(check bool)
    "a recovered label exposes no AST" true
    (Option.is_none label_recovery.ast);
  Alcotest.(check (list string))
    "label recovery leaves the next case available" [ "HCPARSE0091" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       label_recovery.diagnostics)

let deterministic_switch_dumps () =
  let session, _, output =
    parse_string
      "I64 value;switch[value]{case:;case 4...7:value++;start:default:;end:}"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human switch dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON switch dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies switch structures" true
    (contains human "switch_statement"
    && contains human "sub_switch"
    && contains human "pattern=range");
  let open Yojson.Safe.Util in
  let switch =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check string)
    "JSON keeps the switch kind" "switch_statement"
    (switch |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps no-bound mode" "no_bound"
    (switch |> member "mode" |> to_string);
  let elements = switch |> member "elements" |> to_list in
  Alcotest.(check string)
    "JSON keeps implicit case patterns" "implicit"
    (List.nth elements 0 |> member "pattern" |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps ranged case patterns" "range"
    (List.nth elements 2 |> member "pattern" |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps sub-switch regions" "sub_switch"
    (List.nth elements 4 |> member "kind" |> to_string)

let try_catch_statement_source_behavior () =
  let normalized path =
    pinned path |> String.split_on_char '\r' |> String.concat ""
  in
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsStmt dispatches try", "case KW_TRY:");
      ("try uses its dedicated parser", "PrsTryBlk(cc,try_cnt);");
      ("try disables register optimization", "cc->flags|=CCF_NO_REG_OPT;");
      ("try parses an ordinary body", "PrsStmt(cc,try_cnt+1);");
      ("try requires catch", "if (PrsKeyWord(cc)!=KW_CATCH)");
      ("try reports a missing handler", "LexExcept(cc,\"Missing 'catch' at\")");
    ];
  let compiler_header = pinned "Compiler/CompilerA.HH" in
  Alcotest.(check bool)
    "catch keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_CATCH\t3");
  Alcotest.(check bool)
    "try keeps its pinned keyword ID" true
    (contains compiler_header "#define KW_TRY\t\t5");
  let opcode_source = pinned "Compiler/OpCodes.DD" in
  Alcotest.(check bool)
    "the generated keyword source keeps catch ID 3" true
    (contains opcode_source "KEYWORD catch\t\t3;");
  Alcotest.(check bool)
    "the generated keyword source keeps try ID 5" true
    (contains opcode_source "KEYWORD try\t\t5;");
  Alcotest.(check bool)
    "the language guide distinguishes exception syntax from throw" true
    (contains (pinned "Doc/HolyC.DD")
       "$FG,2$try{} catch{}$FG$ and $FG,2$throw$FG$ are different from C++");
  Alcotest.(check bool)
    "the expression parser uses an unbraced try body" true
    (contains
       (normalized "Compiler/PrsExp.HC")
       "try\n//try catch causes noreg vars in function");
  Alcotest.(check bool)
    "the kernel uses unbraced try and catch bodies" true
    (contains
       (normalized "Kernel/Job.HC")
       "try\n\t      tmpc->res=(*tmpc->addr)(tmpc->fun_arg);\n      catch");
  let demo = normalized "Demo/Exceptions.HC" in
  Alcotest.(check bool)
    "the exception demo uses braced try and catch bodies" true
    (contains demo "try {\n    D1;");
  Alcotest.(check bool)
    "throw remains a function call in the demo" true
    (contains demo "throw('Point1')")

let try_catch_statement_shapes () =
  let source =
    "I64 value;try {value++;}catch {value--;}try value++;catch \
     value--;try;catch;try try value++;catch value--;catch value++;"
  in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable _;
       Ast.Top_level_statement braced;
       Ast.Top_level_statement unbraced;
       Ast.Top_level_statement empty;
       Ast.Top_level_statement nested;
      ] ->
          let braced = expect_try_catch_statement braced in
          Alcotest.(check int)
            "try keyword is three bytes" 3
            (Span.length braced.try_keyword.span);
          Alcotest.(check int)
            "catch keyword is five bytes" 5
            (Span.length braced.catch_keyword.span);
          ignore (expect_block_statement braced.try_body);
          let catch_block = expect_block_statement braced.catch_body in
          Alcotest.(check bool)
            "the complete span ends with the handler" true
            (braced.try_catch_location.span.stop
           = catch_block.block_location.span.stop);
          let unbraced = expect_try_catch_statement unbraced in
          ignore (expect_expression_statement unbraced.try_body);
          ignore (expect_expression_statement unbraced.catch_body);
          let empty = expect_try_catch_statement empty in
          ignore (expect_empty_statement empty.try_body);
          ignore (expect_empty_statement empty.catch_body);
          let outer = expect_try_catch_statement nested in
          ignore (expect_try_catch_statement outer.try_body);
          ignore (expect_expression_statement outer.catch_body)
      | items ->
          Alcotest.failf
            "expected a declaration and four try/catch statements, got %d items"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let try_catch_boundaries_and_nesting () =
  let source =
    "I64 value;try value++,value--;catch value++,value--;if(value)try \
     value++;catch value--;else try;catch;while(value)try {value--;}catch lock \
     value++;for(;value;try value--;catch value++;)try value++;catch \
     value--;lock try value++;catch value--;"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement sequences;
   Ast.Top_level_statement conditional;
   Ast.Top_level_statement while_loop;
   Ast.Top_level_statement for_loop;
   Ast.Top_level_statement locked;
  ] ->
      let sequences = expect_try_catch_statement sequences in
      let try_sequence = expect_statement_sequence sequences.try_body in
      let catch_sequence = expect_statement_sequence sequences.catch_body in
      Alcotest.(check int)
        "try retains both comma-linked statements" 2
        (List.length try_sequence.sequence_elements);
      Alcotest.(check int)
        "catch retains both comma-linked statements" 2
        (List.length catch_sequence.sequence_elements);
      let conditional = expect_if_statement conditional in
      ignore (expect_try_catch_statement conditional.if_then_branch);
      (match conditional.if_else_clause with
      | Some clause -> ignore (expect_try_catch_statement clause.else_branch)
      | None -> Alcotest.fail "expected else to bind outside the handler");
      let while_try =
        expect_while_statement while_loop |> fun statement ->
        expect_try_catch_statement statement.while_body
      in
      ignore (expect_block_statement while_try.try_body);
      ignore (expect_lock_statement while_try.catch_body);
      let for_loop = expect_for_statement for_loop in
      (match for_loop.for_update with
      | Some update -> ignore (expect_try_catch_statement update)
      | None -> Alcotest.fail "expected try/catch in the for update");
      ignore (expect_try_catch_statement for_loop.for_body);
      let lock = expect_lock_statement locked in
      ignore (expect_try_catch_statement lock.lock_body)
  | items ->
      Alcotest.failf
        "expected a declaration and five structured statements, got %d items"
        (List.length items)

let try_catch_statement_provenance () =
  let source =
    "#define ATTEMPT try\n\
     #define HANDLE catch\n\
     #define END ;\n\
     ATTEMPT END HANDLE END"
  in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Top_level_statement statement ] ->
        expect_try_catch_statement statement
    | _ -> Alcotest.fail "expected one definition-backed try/catch statement"
  in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("try keyword", statement.try_keyword);
      ("catch keyword", statement.catch_keyword);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated try provenance" true
    (statement_json |> member "try_keyword" |> member "generated_from" <> `Null);
  Alcotest.(check bool)
    "JSON keeps generated catch provenance" true
    (statement_json |> member "catch_keyword" |> member "defined_at" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let handler_file = Filename.concat directory "handler.HC" in
      write_file root_file "I64 value;\n#include \"handler\"";
      write_file handler_file "try value++;catch value--;";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
            expect_try_catch_statement statement
        | _ -> Alcotest.fail "expected one included try/catch statement"
      in
      List.iter
        (fun (location : Ast.location) ->
          let source =
            Source_manager.find
              (Session.sources include_session)
              location.Ast.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "included keyword keeps its canonical path"
            (Unix.realpath handler_file)
            (Source_file.path source))
        [ included.try_keyword; included.catch_keyword ])

let try_catch_statement_failures () =
  List.iter
    (fun (name, source, code, found) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (name ^ " diagnostic") code diagnostic.code;
      Alcotest.(check bool)
        (name ^ " describes the failure")
        true
        (contains diagnostic.message found))
    [
      ("try at end of input", "try", "HCPARSE0079", "end of input");
      ("try before a block close", "{try}", "HCPARSE0079", "found \"}\"");
      ("try before catch", "try catch;", "HCPARSE0079", "catch");
      ("comma-only try body", "try,,,", "HCPARSE0079", "only statement commas");
      ("missing catch", "try;", "HCPARSE0080", "end of input");
      ("wrong catch token", "try;else", "HCPARSE0080", "else");
      ("catch at end of input", "try;catch", "HCPARSE0081", "end of input");
      ("catch before a block close", "{try;catch}", "HCPARSE0081", "found \"}\"");
      ("repeated catch", "try;catch catch;", "HCPARSE0081", "catch");
      ("standalone catch", "catch;", "HCPARSE0082", "without a matching 'try'");
    ];
  let _, _, malformed_try = parse_string "try )" in
  Alcotest.(check string)
    "malformed try child keeps its ordinary diagnostic" "HCPARSE0048"
    (first_diagnostic malformed_try).code;
  let _, _, malformed_catch = parse_string "try;catch )" in
  Alcotest.(check string)
    "malformed catch child keeps its ordinary diagnostic" "HCPARSE0048"
    (first_diagnostic malformed_catch).code;
  let excessive =
    String.concat "" (List.init (Parser.max_try_depth + 1) (fun _ -> "try "))
    ^ ";"
  in
  let _, _, nested = parse_string excessive in
  let diagnostic = first_diagnostic nested in
  Alcotest.(check string) "try nesting diagnostic" "HCPARSE0083" diagnostic.code;
  Alcotest.(check bool)
    "try nesting message names the hosted limit" true
    (contains diagnostic.message (string_of_int Parser.max_try_depth))

let deterministic_try_catch_dumps () =
  let session, _, output =
    parse_string "I64 value;try {try value++;catch value--;}catch {value++;}"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human try/catch dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON try/catch dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies try/catch statements" true
    (contains human "try_catch_statement");
  let open Yojson.Safe.Util in
  let outer =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check string)
    "JSON keeps the outer try/catch kind" "try_catch_statement"
    (outer |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON keeps the braced handler" "block_statement"
    (outer |> member "catch_body" |> member "kind" |> to_string);
  let inner =
    outer |> member "try_body" |> member "statements" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "JSON keeps the nested try/catch kind" "try_catch_statement"
    (inner |> member "kind" |> to_string)

let break_statement_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsStmt dispatches break", "case KW_BREAK:");
      ("break requires an active target", "if (!lb_break)");
      ( "break reports a missing target",
        "LexExcept(cc,\"'break' not allowed\\n\")" );
      ("break emits a jump", "ICAdd(cc,IC_JMP,lb_break,0);");
      ("break uses the common terminator", "goto sm_semicolon;");
      ("the common terminator accepts a comma", "else if (cc->token!=',')");
      ("while supplies its break target", "PrsStmt(cc,try_cnt,lb_done);");
      ( "switch supplies its current break target",
        "PrsStmt(cc,try_cnt,head.last->lb_break);" );
    ];
  Alcotest.(check bool)
    "break keeps its pinned keyword ID" true
    (contains (pinned "Compiler/CompilerA.HH") "#define KW_BREAK\t19");
  Alcotest.(check bool)
    "the language guide uses break in switch cases" true
    (contains (pinned "Doc/HolyC.DD") "case 0: \"Zero \";\tbreak;")

let break_statement_shapes () =
  let source =
    "I64 active;break;while(active)break;do \
     break;while(active);for(;active;)break;"
  in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable _;
       Ast.Top_level_statement top_level_break;
       Ast.Top_level_statement while_loop;
       Ast.Top_level_statement do_loop;
       Ast.Top_level_statement for_loop;
      ] ->
          let top_level_break = expect_break_statement top_level_break in
          Alcotest.(check int)
            "break keyword is five bytes" 5
            (Span.length top_level_break.break_keyword.span);
          Alcotest.(check bool)
            "ordinary break retains its semicolon" true
            (Option.is_some top_level_break.break_semicolon);
          Alcotest.(check bool)
            "ordinary break span covers its terminator" true
            (top_level_break.break_location.span.stop
           = (Option.get top_level_break.break_semicolon).span.stop);
          let while_loop = expect_while_statement while_loop in
          ignore (expect_break_statement while_loop.while_body);
          let do_loop = expect_do_while_statement do_loop in
          ignore (expect_break_statement do_loop.do_body);
          let for_loop = expect_for_statement for_loop in
          ignore (expect_break_statement for_loop.for_body)
      | items ->
          Alcotest.failf
            "expected one declaration and four break-bearing statements, got %d"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let break_statement_boundaries_and_nesting () =
  let source =
    "I64 active;break,active--;for(;active;break)break;{if(active)break;else \
     break;}"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement sequence;
   Ast.Top_level_statement for_loop;
   Ast.Top_level_statement block;
  ] ->
      let sequence = expect_statement_sequence sequence in
      Alcotest.(check int)
        "comma sequence keeps two statements" 2
        (List.length sequence.sequence_elements);
      let first = List.hd sequence.sequence_elements in
      let comma_break = expect_break_statement first.sequence_statement in
      Alcotest.(check bool)
        "comma-terminated break has no semicolon" true
        (Option.is_none comma_break.break_semicolon);
      Alcotest.(check int)
        "break element retains its following comma" 1
        (List.length first.sequence_following_commas);
      let for_loop = expect_for_statement for_loop in
      let update =
        match for_loop.for_update with
        | Some update -> expect_break_statement update
        | None -> Alcotest.fail "expected a break in the for update"
      in
      Alcotest.(check bool)
        "for-update break has no fabricated semicolon" true
        (Option.is_none update.break_semicolon);
      let body = expect_break_statement for_loop.for_body in
      Alcotest.(check bool)
        "for-body break keeps its semicolon" true
        (Option.is_some body.break_semicolon);
      let block = expect_block_statement block in
      let conditional =
        match block.block_statements with
        | [ statement ] -> expect_if_statement statement
        | statements ->
            Alcotest.failf "expected one conditional in the block, got %d"
              (List.length statements)
      in
      ignore (expect_break_statement conditional.if_then_branch);
      let else_break =
        match conditional.if_else_clause with
        | Some clause -> expect_break_statement clause.else_branch
        | None -> Alcotest.fail "expected an else break"
      in
      Alcotest.(check bool)
        "else branch retains its break terminator" true
        (Option.is_some else_break.break_semicolon)
  | items ->
      Alcotest.failf
        "expected a declaration, sequence, for loop, and block, got %d items"
        (List.length items)

let break_statement_provenance () =
  let source = "#define EXIT break\n#define END ;\nEXIT END" in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Top_level_statement statement ] -> expect_break_statement statement
    | _ -> Alcotest.fail "expected one definition-backed break statement"
  in
  let semicolon = Option.get statement.break_semicolon in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [ ("break keyword", statement.break_keyword); ("semicolon", semicolon) ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list |> List.hd
    |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated break provenance" true
    (statement_json |> member "keyword" |> member "generated_from" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let break_file = Filename.concat directory "break.HC" in
      write_file root_file "#include \"break\"";
      write_file break_file "break;";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_break =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Top_level_statement statement ] ->
            expect_break_statement statement
        | _ -> Alcotest.fail "expected one included break statement"
      in
      let included_source =
        Source_manager.find
          (Session.sources include_session)
          included_break.break_keyword.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included break keeps its canonical path" (Unix.realpath break_file)
        (Source_file.path included_source))

let break_statement_failures () =
  List.iter
    (fun (name, source, found) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string)
        (name ^ " diagnostic") "HCPARSE0072" diagnostic.code;
      Alcotest.(check bool)
        (name ^ " describes the invalid boundary")
        true
        (contains diagnostic.message found))
    [
      ("end of input", "break", "end of input");
      ("closing parenthesis", "break)", "found \")\"");
      ("following expression", "break 1;", "found \"1\"");
    ];
  let _, _, block_recovery = parse_string "{break}" in
  Alcotest.(check (list string))
    "break recovery preserves the enclosing block close" [ "HCPARSE0072" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       block_recovery.diagnostics);
  let _, _, update_semicolon = parse_string "I64 active;for(;active;break;);" in
  Alcotest.(check string)
    "for-update semicolon stays an outer header error" "HCPARSE0070"
    (first_diagnostic update_semicolon).code

let deterministic_break_dumps () =
  let session, _, output =
    parse_string "I64 active;for(;active;break){if(active)break;}"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human break dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON break dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies break statements" true
    (contains human "break_statement");
  let open Yojson.Safe.Util in
  let for_loop =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  let update = for_loop |> member "update" in
  Alcotest.(check string)
    "JSON for update keeps the break kind" "break_statement"
    (update |> member "kind" |> to_string);
  Alcotest.(check bool)
    "JSON for-update break has a null semicolon" true
    (update |> member "semicolon" = `Null);
  let nested_break =
    for_loop |> member "body" |> member "statements" |> to_list |> List.hd
    |> member "then_branch"
  in
  Alcotest.(check string)
    "JSON nested break keeps its kind" "break_statement"
    (nested_break |> member "kind" |> to_string)

let return_statement_source_behavior () =
  let statement_parser = pinned "Compiler/PrsStmt.HC" in
  List.iter
    (fun (description, fragment) ->
      Alcotest.(check bool)
        description true
        (contains statement_parser fragment))
    [
      ("PrsStmt dispatches return", "case KW_RETURN:");
      ("return requires a function context", "if (!cc->htc.fun)");
      ("return checks for an immediate semicolon", "if (Lex(cc)!=';') {");
      ( "value-bearing return parses an expression",
        "if (!PrsExpression(cc,NULL,FALSE))" );
      ( "value-bearing return emits its dedicated IC",
        "ICAdd(cc,IC_RETURN_VAL,0,cc->htc.fun->return_class);" );
      ("return jumps to the leave label", "ICAdd(cc,IC_JMP,cc->lb_leave,0);");
      ("return uses the common terminator", "goto sm_semicolon;");
      ("the common terminator accepts a comma", "else if (cc->token!=',')");
    ];
  Alcotest.(check bool)
    "return keeps its pinned keyword ID" true
    (contains (pinned "Compiler/CompilerA.HH") "#define KW_RETURN\t12");
  Alcotest.(check bool)
    "the language guide returns a value" true
    (contains (pinned "Doc/HolyC.DD") "return res;");
  Alcotest.(check bool)
    "the compiler corpus contains valueless returns" true
    (contains (pinned "Compiler/BackC.HC") "      return;");
  Alcotest.(check bool)
    "the kernel corpus contains early returned values" true
    (contains (pinned "Kernel/FunSeg.HC") "    return NULL;")

let return_statement_shapes () =
  let source =
    "I64 active;return;return 1+2*3;while(active)return active--;do \
     return;while(active);for(;active;)return active;"
  in
  List.iter
    (fun mode ->
      let _, _, output = parse_string ~compilation_mode:mode source in
      match (expect_ast output).Ast.items with
      | [
       Ast.Global_variable _;
       Ast.Top_level_statement valueless;
       Ast.Top_level_statement valued;
       Ast.Top_level_statement while_loop;
       Ast.Top_level_statement do_loop;
       Ast.Top_level_statement for_loop;
      ] ->
          let valueless = expect_return_statement valueless in
          Alcotest.(check int)
            "return keyword is six bytes" 6
            (Span.length valueless.return_keyword.span);
          Alcotest.(check bool)
            "valueless return has no expression" true
            (Option.is_none valueless.return_value);
          Alcotest.(check bool)
            "valueless return retains its semicolon" true
            (Option.is_some valueless.return_semicolon);
          Alcotest.(check bool)
            "valueless return span covers its terminator" true
            (valueless.return_location.span.stop
           = (Option.get valueless.return_semicolon).span.stop);
          let valued = expect_return_statement valued in
          let root =
            Option.get valued.return_value |> expect_binary_expression
          in
          Alcotest.(check string)
            "returned expression keeps its addition root" "+"
            root.binary_operator.operator_spelling;
          Alcotest.(check string)
            "multiplication binds inside the returned expression" "*"
            (expect_binary_expression root.binary_right).binary_operator
              .operator_spelling;
          Alcotest.(check bool)
            "value-bearing return retains its semicolon" true
            (Option.is_some valued.return_semicolon);
          let while_loop = expect_while_statement while_loop in
          let while_return = expect_return_statement while_loop.while_body in
          ignore
            (Option.get while_return.return_value |> expect_postfix_expression);
          let do_loop = expect_do_while_statement do_loop in
          let do_return = expect_return_statement do_loop.do_body in
          Alcotest.(check bool)
            "do body keeps a valueless return" true
            (Option.is_none do_return.return_value);
          let for_loop = expect_for_statement for_loop in
          let for_return = expect_return_statement for_loop.for_body in
          ignore
            (Option.get for_return.return_value |> expect_identifier_expression)
      | items ->
          Alcotest.failf
            "expected one declaration and five return-bearing statements, got \
             %d"
            (List.length items))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let return_statement_boundaries_and_nesting () =
  let source =
    "I64 active;return active,active--;for(;active;return \
     active--){if(active)return;else return active;}"
  in
  let _, _, output = parse_string source in
  match (expect_ast output).Ast.items with
  | [
   Ast.Global_variable _;
   Ast.Top_level_statement sequence;
   Ast.Top_level_statement for_loop;
  ] ->
      let sequence = expect_statement_sequence sequence in
      Alcotest.(check int)
        "comma sequence keeps two statements" 2
        (List.length sequence.sequence_elements);
      let first = List.hd sequence.sequence_elements in
      let comma_return = expect_return_statement first.sequence_statement in
      Alcotest.(check bool)
        "comma-terminated return keeps its value" true
        (Option.is_some comma_return.return_value);
      Alcotest.(check bool)
        "comma-terminated return has no semicolon" true
        (Option.is_none comma_return.return_semicolon);
      Alcotest.(check int)
        "return element retains its following comma" 1
        (List.length first.sequence_following_commas);
      let for_loop = expect_for_statement for_loop in
      let update =
        match for_loop.for_update with
        | Some update -> expect_return_statement update
        | None -> Alcotest.fail "expected a return in the for update"
      in
      Alcotest.(check bool)
        "for-update return keeps its value" true
        (Option.is_some update.return_value);
      Alcotest.(check bool)
        "for-update return has no fabricated semicolon" true
        (Option.is_none update.return_semicolon);
      let block = expect_block_statement for_loop.for_body in
      let conditional =
        match block.block_statements with
        | [ statement ] -> expect_if_statement statement
        | statements ->
            Alcotest.failf "expected one conditional in the block, got %d"
              (List.length statements)
      in
      let then_return = expect_return_statement conditional.if_then_branch in
      Alcotest.(check bool)
        "then branch keeps a valueless return" true
        (Option.is_none then_return.return_value);
      let else_return =
        match conditional.if_else_clause with
        | Some clause -> expect_return_statement clause.else_branch
        | None -> Alcotest.fail "expected an else return"
      in
      Alcotest.(check bool)
        "else branch keeps its return value" true
        (Option.is_some else_return.return_value)
  | items ->
      Alcotest.failf
        "expected a declaration, sequence, and for loop, got %d items"
        (List.length items)

let return_statement_provenance () =
  let source =
    "#define EXIT return\n\
     #define VALUE active\n\
     #define END ;\n\
     I64 active;EXIT VALUE END"
  in
  let session, _, output = parse_string source in
  let statement =
    match (expect_ast output).Ast.items with
    | [ Ast.Global_variable _; Ast.Top_level_statement statement ] ->
        expect_return_statement statement
    | _ -> Alcotest.fail "expected one definition-backed return statement"
  in
  let value =
    Option.get statement.return_value |> expect_identifier_expression
  in
  let semicolon = Option.get statement.return_semicolon in
  List.iter
    (fun (name, location) ->
      Alcotest.(check bool)
        (name ^ " retains its invocation")
        true
        (Option.is_some location.Ast.generated_from);
      Alcotest.(check bool)
        (name ^ " retains its definition")
        true
        (Option.is_some location.Ast.defined_at))
    [
      ("return keyword", statement.return_keyword);
      ("return value", value.location);
      ("semicolon", semicolon);
    ];
  let open Yojson.Safe.Util in
  let statement_json =
    Ast_dump.to_yojson (Session.sources session) (expect_ast output)
    |> member "module" |> member "items" |> to_list
    |> fun items -> List.nth items 1 |> member "statement"
  in
  Alcotest.(check bool)
    "JSON keeps generated return provenance" true
    (statement_json |> member "keyword" |> member "generated_from" <> `Null);
  with_temp_directory (fun directory ->
      let root_file = Filename.concat directory "root.HC" in
      let return_file = Filename.concat directory "return.HC" in
      write_file root_file "#include \"return\"";
      write_file return_file "return 1;";
      let include_session = Session.create () in
      let root =
        Session.load_source include_session ~path:root_file |> Result.get_ok
      in
      let include_output =
        Holyc_lib.parse_detailed include_session ~config:(config directory)
          ~source:root
      in
      let included_return =
        match (expect_ast include_output).Ast.items with
        | [ Ast.Top_level_statement statement ] ->
            expect_return_statement statement
        | _ -> Alcotest.fail "expected one included return statement"
      in
      let included_source =
        Source_manager.find
          (Session.sources include_session)
          included_return.return_keyword.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included return keeps its canonical path"
        (Unix.realpath return_file)
        (Source_file.path included_source))

let return_statement_failures () =
  List.iter
    (fun (name, source, code, found) ->
      let _, _, output = parse_string source in
      Alcotest.(check bool)
        (name ^ " has no AST") true
        (Option.is_none output.ast);
      let diagnostic = first_diagnostic output in
      Alcotest.(check string) (name ^ " diagnostic") code diagnostic.code;
      Alcotest.(check bool)
        (name ^ " describes the failure")
        true
        (contains diagnostic.message found))
    [
      ("end of input", "return", "HCPARSE0074", "end of input");
      ("comma without a value", "return,active;", "HCPARSE0074", "found \",\"");
      ("closing parenthesis", "return)", "HCPARSE0074", "found \")\"");
      ("invalid terminator", "return 1)", "HCPARSE0073", "found \")\"");
      ("following expression", "return 1 2;", "HCPARSE0073", "found \"2\"");
    ];
  let _, _, block_recovery = parse_string "{return 1}" in
  Alcotest.(check (list string))
    "return recovery preserves the enclosing block close" [ "HCPARSE0073" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       block_recovery.diagnostics);
  let _, _, empty_block_recovery = parse_string "{return}" in
  Alcotest.(check (list string))
    "missing return value preserves the enclosing block close" [ "HCPARSE0074" ]
    (List.map
       (fun diagnostic -> diagnostic.Diagnostic.code)
       empty_block_recovery.diagnostics);
  let _, _, update_semicolon =
    parse_string "I64 active;for(;active;return;)active--;"
  in
  Alcotest.(check string)
    "a valueless for-update return remains invalid" "HCPARSE0070"
    (first_diagnostic update_semicolon).code

let deterministic_return_dumps () =
  let session, _, output =
    parse_string
      "I64 active;return;for(;active;return active--){if(active)return active;}"
  in
  let ast = expect_ast output in
  let sources = Session.sources session in
  let human = Ast_dump.human sources ast in
  let json = Ast_dump.json sources ast in
  Alcotest.(check string)
    "human return dump is deterministic" human
    (Ast_dump.human sources ast);
  Alcotest.(check string)
    "JSON return dump is deterministic" json
    (Ast_dump.json sources ast);
  Alcotest.(check bool)
    "human dump identifies return statements" true
    (contains human "return_statement");
  let open Yojson.Safe.Util in
  let items =
    Yojson.Safe.from_string json |> member "module" |> member "items" |> to_list
  in
  let valueless = List.nth items 1 |> member "statement" in
  Alcotest.(check bool)
    "JSON valueless return has a null value" true
    (valueless |> member "value" = `Null);
  Alcotest.(check bool)
    "JSON valueless return retains its semicolon" true
    (valueless |> member "semicolon" <> `Null);
  let for_loop = List.nth items 2 |> member "statement" in
  let update = for_loop |> member "update" in
  Alcotest.(check string)
    "JSON for update keeps the return kind" "return_statement"
    (update |> member "kind" |> to_string);
  Alcotest.(check string)
    "JSON for-update return keeps its value" "postfix"
    (update |> member "value" |> member "kind" |> to_string);
  Alcotest.(check bool)
    "JSON for-update return has a null semicolon" true
    (update |> member "semicolon" = `Null)

let unsupported_forms () =
  let cases =
    [
      ("unknown type", "Widget value;", "HCPARSE0001");
      ("missing name", "I64 ;", "HCPARSE0002");
      ("missing semicolon", "I64 value", "HCPARSE0003");
      ("initializer", "I64 value=1;", "HCPARSE0003");
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
    Alcotest.test_case "pinned compound block behavior" `Quick
      compound_block_source_behavior;
    Alcotest.test_case "compound block shapes" `Quick compound_block_shapes;
    Alcotest.test_case "nested block and sequence shapes" `Quick
      nested_block_and_sequence_shapes;
    Alcotest.test_case "compound block modes and order" `Quick
      compound_block_modes_and_order;
    Alcotest.test_case "compound block provenance" `Quick
      compound_block_provenance;
    Alcotest.test_case "compound block failures" `Quick compound_block_failures;
    Alcotest.test_case "deterministic compound block dumps" `Quick
      deterministic_compound_block_dumps;
    Alcotest.test_case "pinned if and else behavior" `Quick
      if_statement_source_behavior;
    Alcotest.test_case "if statement shapes" `Quick if_statement_shapes;
    Alcotest.test_case "nearest if owns else" `Quick nested_if_else_binding;
    Alcotest.test_case "if branch sequences and order" `Quick
      if_branch_sequences_and_order;
    Alcotest.test_case "if statement provenance" `Quick if_statement_provenance;
    Alcotest.test_case "if statement failures" `Quick if_statement_failures;
    Alcotest.test_case "deterministic if statement dumps" `Quick
      deterministic_if_dumps;
    Alcotest.test_case "pinned while behavior" `Quick
      while_statement_source_behavior;
    Alcotest.test_case "while statement shapes" `Quick while_statement_shapes;
    Alcotest.test_case "while bodies and else binding" `Quick
      while_body_shapes_and_else_binding;
    Alcotest.test_case "while statement provenance" `Quick
      while_statement_provenance;
    Alcotest.test_case "while statement failures" `Quick
      while_statement_failures;
    Alcotest.test_case "deterministic while statement dumps" `Quick
      deterministic_while_dumps;
    Alcotest.test_case "pinned do-while behavior" `Quick
      do_while_statement_source_behavior;
    Alcotest.test_case "do-while statement shapes" `Quick
      do_while_statement_shapes;
    Alcotest.test_case "do-while bodies and else binding" `Quick
      do_while_body_shapes_and_else_binding;
    Alcotest.test_case "do-while statement provenance" `Quick
      do_while_statement_provenance;
    Alcotest.test_case "do-while statement failures" `Quick
      do_while_statement_failures;
    Alcotest.test_case "deterministic do-while statement dumps" `Quick
      deterministic_do_while_dumps;
    Alcotest.test_case "pinned for behavior" `Quick
      for_statement_source_behavior;
    Alcotest.test_case "for statement shapes" `Quick for_statement_shapes;
    Alcotest.test_case "for header variants and else binding" `Quick
      for_header_variants_and_else_binding;
    Alcotest.test_case "for statement provenance" `Quick
      for_statement_provenance;
    Alcotest.test_case "for statement failures" `Quick for_statement_failures;
    Alcotest.test_case "deterministic for statement dumps" `Quick
      deterministic_for_dumps;
    Alcotest.test_case "pinned goto and label behavior" `Quick
      goto_label_source_behavior;
    Alcotest.test_case "goto and label statement shapes" `Quick
      goto_label_statement_shapes;
    Alcotest.test_case "goto and label boundaries and routing" `Quick
      goto_label_boundaries_and_routing;
    Alcotest.test_case "goto and label provenance" `Quick goto_label_provenance;
    Alcotest.test_case "goto and label failures" `Quick goto_label_failures;
    Alcotest.test_case "deterministic goto and label dumps" `Quick
      deterministic_goto_label_dumps;
    Alcotest.test_case "pinned lock behavior" `Quick
      lock_statement_source_behavior;
    Alcotest.test_case "lock statement shapes" `Quick lock_statement_shapes;
    Alcotest.test_case "lock statement boundaries and nesting" `Quick
      lock_statement_boundaries_and_nesting;
    Alcotest.test_case "lock statement provenance" `Quick
      lock_statement_provenance;
    Alcotest.test_case "lock statement failures" `Quick lock_statement_failures;
    Alcotest.test_case "deterministic lock statement dumps" `Quick
      deterministic_lock_dumps;
    Alcotest.test_case "pinned switch behavior" `Quick
      switch_statement_source_behavior;
    Alcotest.test_case "switch statement shapes" `Quick switch_statement_shapes;
    Alcotest.test_case "sub-switch shapes and boundaries" `Quick
      switch_subswitch_shapes_and_boundaries;
    Alcotest.test_case "switch statement contexts" `Quick
      switch_statement_contexts;
    Alcotest.test_case "switch statement provenance" `Quick
      switch_statement_provenance;
    Alcotest.test_case "switch statement failures" `Quick
      switch_statement_failures;
    Alcotest.test_case "deterministic switch dumps" `Quick
      deterministic_switch_dumps;
    Alcotest.test_case "pinned try/catch behavior" `Quick
      try_catch_statement_source_behavior;
    Alcotest.test_case "try/catch statement shapes" `Quick
      try_catch_statement_shapes;
    Alcotest.test_case "try/catch boundaries and nesting" `Quick
      try_catch_boundaries_and_nesting;
    Alcotest.test_case "try/catch statement provenance" `Quick
      try_catch_statement_provenance;
    Alcotest.test_case "try/catch statement failures" `Quick
      try_catch_statement_failures;
    Alcotest.test_case "deterministic try/catch dumps" `Quick
      deterministic_try_catch_dumps;
    Alcotest.test_case "pinned break behavior" `Quick
      break_statement_source_behavior;
    Alcotest.test_case "break statement shapes" `Quick break_statement_shapes;
    Alcotest.test_case "break statement boundaries and nesting" `Quick
      break_statement_boundaries_and_nesting;
    Alcotest.test_case "break statement provenance" `Quick
      break_statement_provenance;
    Alcotest.test_case "break statement failures" `Quick
      break_statement_failures;
    Alcotest.test_case "deterministic break statement dumps" `Quick
      deterministic_break_dumps;
    Alcotest.test_case "pinned return behavior" `Quick
      return_statement_source_behavior;
    Alcotest.test_case "return statement shapes" `Quick return_statement_shapes;
    Alcotest.test_case "return statement boundaries and nesting" `Quick
      return_statement_boundaries_and_nesting;
    Alcotest.test_case "return statement provenance" `Quick
      return_statement_provenance;
    Alcotest.test_case "return statement failures" `Quick
      return_statement_failures;
    Alcotest.test_case "deterministic return statement dumps" `Quick
      deterministic_return_dumps;
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
    Alcotest.test_case "pinned function definition behavior" `Quick
      function_definition_source_behavior;
    Alcotest.test_case "function definition shapes" `Quick
      function_definition_shapes;
    Alcotest.test_case "function definition streaming visibility" `Quick
      function_definition_streaming_visibility;
    Alcotest.test_case "function definition provenance" `Quick
      function_definition_provenance;
    Alcotest.test_case "function definition failures" `Quick
      function_definition_failures;
    Alcotest.test_case "deterministic function definition dumps" `Quick
      deterministic_function_definition_dumps;
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
