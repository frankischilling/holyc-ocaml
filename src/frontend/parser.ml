type output = {
  ast : Ast.module_ option;
  diagnostics : Common.Diagnostic.t list;
}

type located_token = {
  token : Token.t;
  context : Preprocessor.diagnostic_context;
}

type cursor = {
  stream : Preprocessor.t;
  symbols : Symbol_visibility.Environment.t;
  compilation_mode : Preprocessor.compilation_mode;
  mutable lookahead : located_token list;
  mutable diagnostics_rev : Common.Diagnostic.t list;
  mutable local_context : Symbol_visibility.Environment.local_context option;
}

type parsed_declarator = { node : Ast.global_declarator; tokens : Token.t list }

type parsed_declarator_prefix = {
  pointer_layers : Ast.pointer_layer list;
  name : Ast.identifier;
  tokens : Token.t list;
  definition_trace : Common.Diagnostic.related list;
}

type parsed_parameter = { node : Ast.function_parameter; tokens : Token.t list }
type parsed_expression = { node : Ast.expression; tokens : Token.t list }
type parsed_statement = { node : Ast.statement; tokens : Token.t list }

type parsed_local_declarator = {
  node : Ast.local_declarator;
  tokens : Token.t list;
}

type parsed_switch_element = {
  node : Ast.switch_element;
  tokens : Token.t list;
}

type switch_region_end =
  | Switch_region_brace of located_token
  | Switch_region_end_label of located_token * located_token

type parsed_switch_region = {
  region_elements : Ast.switch_element list;
  region_tokens : Token.t list;
  region_end : switch_region_end;
  region_had_error : bool;
}

type statement_boundary =
  | Top_level_boundary
  | Block_boundary
  | Switch_boundary
  | For_update_boundary of statement_boundary

type parsed_array_dimension = {
  node : Ast.array_dimension;
  tokens : Token.t list;
}

type expression_context =
  | Default_expression
  | Array_dimension_expression
  | Intern_binding_expression
  | Call_argument_expression
  | Index_expression
  | Implicit_output_argument_expression
  | Return_expression
  | Do_while_condition_expression
  | For_condition_expression
  | If_condition_expression
  | Switch_expression
  | Switch_case_expression
  | While_condition_expression
  | Local_initializer_expression
  | Statement_expression

type parsed_parameter_default = {
  node : Ast.parameter_default;
  tokens : Token.t list;
}

type parsed_function_pointer = {
  node : Ast.function_pointer_declarator;
  name : Ast.identifier option;
  tokens : Token.t list;
}

type parsed_register_qualifiers = {
  nodes : Ast.register_qualifier list;
  tokens : Token.t list;
}

type parsed_parameter_list = {
  parameters : Ast.function_parameter list;
  variadic : Ast.variadic_marker option;
  tokens : Token.t list;
  closing_parenthesis : Ast.location;
}

type parsed_modifier = { node : Ast.declaration_modifier; item : located_token }

type parsed_binding = {
  node : Ast.declaration_binding;
  keyword : located_token;
  tokens : Token.t list;
}

type binding_parse =
  | No_binding
  | Parsed_binding of parsed_binding
  | Bad_binding

let max_pointer_depth = 4
let max_function_pointer_depth = 32
let max_expression_depth = 256
let max_block_depth = 256
let max_conditional_depth = 256
let max_loop_depth = 256
let max_lock_depth = 256
let max_try_depth = 256
let max_switch_depth = 256

let canonical_u64_registers =
  List.init 16 (fun register_number ->
      match
        List.find_opt
          (fun (register : Generated.Opcode_keywords.register) ->
            register.register_kind = Generated.Opcode_keywords.R64
            && register.register_number = register_number)
          Generated.Opcode_keywords.registers
      with
      | Some register -> register
      | None ->
          invalid_arg
            (Printf.sprintf
               "checked opcode table lacks canonical U64 register %d"
               register_number))

let is_canonical_u64_register spelling =
  List.exists
    (fun (register : Generated.Opcode_keywords.register) ->
      String.equal register.spelling spelling)
    canonical_u64_registers

let has_error diagnostics =
  List.exists
    (fun diagnostic ->
      diagnostic.Common.Diagnostic.severity = Common.Diagnostic.Error)
    diagnostics

let has_errors output = has_error output.diagnostics

let rec pull cursor =
  match Preprocessor.next cursor.stream with
  | Lexer.Diagnostic diagnostic ->
      cursor.diagnostics_rev <- diagnostic :: cursor.diagnostics_rev;
      pull cursor
  | Lexer.Token token ->
      { token; context = Preprocessor.diagnostic_context cursor.stream }

let rec ensure_lookahead cursor count =
  if List.length cursor.lookahead >= count then ()
  else (
    cursor.lookahead <- cursor.lookahead @ [ pull cursor ];
    ensure_lookahead cursor count)

let peek_n cursor offset =
  if offset < 0 then invalid_arg "parser lookahead offset cannot be negative";
  if offset > 1 then invalid_arg "parser lookahead is limited to two tokens";
  ensure_lookahead cursor (offset + 1);
  List.nth cursor.lookahead offset

let peek cursor = peek_n cursor 0

let take cursor =
  let item = peek cursor in
  cursor.lookahead <- List.tl cursor.lookahead;
  item

let token_segments token =
  match token.Token.source_segments with
  | [] -> [ token.span ]
  | segments -> segments

let token_location token =
  Ast.make_location ?generated_from:token.Token.origin.generated_from
    ?defined_at:token.origin.defined_at ~span:token.Token.span
    ~source_segments:(token_segments token) ()

let location_before_token token =
  let location = token_location token in
  let empty_span (span : Common.Span.t) =
    Common.Span.unsafe_make ~source:span.source ~start:span.start
      ~stop:span.start
  in
  Ast.make_location ?generated_from:location.generated_from
    ?defined_at:location.defined_at ~span:(empty_span location.span)
    ~source_segments:(List.map empty_span location.source_segments)
    ()

let location_from_tokens = function
  | [] -> invalid_arg "a syntax location needs at least one token"
  | first_token :: _ as tokens ->
      let last_token = List.hd (List.rev tokens) in
      let segments = List.concat_map token_segments tokens in
      let all_in_primary_source =
        List.for_all
          (fun segment ->
            Common.Source_id.equal segment.Common.Span.source
              first_token.Token.span.source)
          segments
      in
      let span =
        if all_in_primary_source then
          Common.Span.unsafe_make ~source:first_token.span.source
            ~start:first_token.span.start ~stop:last_token.span.stop
        else first_token.span
      in
      Ast.make_location ~span ~source_segments:segments ()

let location_from_expression_tokens = function
  | [] -> invalid_arg "an expression location needs at least one token"
  | first_token :: _ as tokens ->
      let base = location_from_tokens tokens in
      Ast.make_location ?generated_from:first_token.Token.origin.generated_from
        ?defined_at:first_token.origin.defined_at ~span:base.span
        ~source_segments:base.source_segments ()

let location_from_locations (locations : Ast.location list) =
  match locations with
  | [] -> invalid_arg "a syntax location needs at least one child location"
  | (first : Ast.location) :: _ ->
      let last : Ast.location = List.hd (List.rev locations) in
      let all_in_primary_source =
        List.for_all
          (fun (location : Ast.location) ->
            Common.Source_id.equal location.Ast.span.source
              first.Ast.span.source)
          locations
      in
      let span =
        if all_in_primary_source then
          Common.Span.unsafe_make ~source:first.span.source
            ~start:first.span.start ~stop:last.span.stop
        else first.span
      in
      let source_segments =
        List.concat_map
          (fun (location : Ast.location) -> location.Ast.source_segments)
          locations
      in
      Ast.make_location ?generated_from:first.generated_from
        ?defined_at:first.defined_at ~span ~source_segments ()

let token_text token =
  match token.Token.value with
  | Token.Text text -> text
  | _ -> token.raw

let token_description token =
  match token.Token.kind with
  | Token_kind.Eof -> "end of input"
  | Token_kind.Identifier -> Printf.sprintf "identifier %S" (token_text token)
  | _ when String.length token.raw > 0 -> Printf.sprintf "%S" token.raw
  | kind -> Token_kind.name kind

let same_related (left : Common.Diagnostic.related)
    (right : Common.Diagnostic.related) =
  String.equal left.message right.message
  && Common.Span.compare left.span right.span = 0

let append_unique_related items additions =
  List.fold_left
    (fun result item ->
      if List.exists (same_related item) result then result
      else result @ [ item ])
    items additions

let report ?(secondary = []) cursor item ~code ~message =
  let secondary =
    append_unique_related item.context.definition_trace secondary
  in
  let diagnostic =
    Common.Diagnostic.make ~secondary ~include_stack:item.context.include_stack
      ~code ~severity:Common.Diagnostic.Error ~message ~primary:item.token.span
      ()
  in
  cursor.diagnostics_rev <- diagnostic :: cursor.diagnostics_rev

let rec recover_declaration cursor =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Eof -> ()
  | Token_kind.Punctuation ';' -> ignore (take cursor)
  | _ ->
      ignore (take cursor);
      recover_declaration cursor

let rec statement_boundary_stops_at_closing_brace = function
  | Block_boundary | Switch_boundary -> true
  | For_update_boundary boundary ->
      statement_boundary_stops_at_closing_brace boundary
  | Top_level_boundary -> false

let rec statement_boundary_is_switch = function
  | Switch_boundary -> true
  | For_update_boundary boundary -> statement_boundary_is_switch boundary
  | Top_level_boundary | Block_boundary -> false

let token_is_switch_boundary token =
  match token.Token.kind with
  | Token_kind.Keyword
      (Keyword.Case | Keyword.Default | Keyword.Start | Keyword.End) -> true
  | _ -> false

let rec recover_statement cursor ~boundary =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Eof -> ()
  | Token_kind.Punctuation '}'
    when statement_boundary_stops_at_closing_brace boundary -> ()
  | _
    when statement_boundary_is_switch boundary
         && token_is_switch_boundary item.token -> ()
  | Token_kind.Punctuation ')' -> (
      match boundary with
      | For_update_boundary _ -> ()
      | Top_level_boundary | Block_boundary | Switch_boundary ->
          ignore (take cursor);
          recover_statement cursor ~boundary)
  | Token_kind.Punctuation ';' -> ignore (take cursor)
  | _ ->
      ignore (take cursor);
      recover_statement cursor ~boundary

let recover_switch_tail cursor ~boundary =
  let rec skip_body depth =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip_body (depth + 1)
    | Token_kind.Punctuation '}' when depth > 1 ->
        ignore (take cursor);
        skip_body (depth - 1)
    | Token_kind.Punctuation '}' -> ignore (take cursor)
    | _ ->
        ignore (take cursor);
        skip_body depth
  in
  let rec seek_body () =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip_body 1
    | Token_kind.Punctuation ';' -> ignore (take cursor)
    | Token_kind.Punctuation '}'
      when statement_boundary_stops_at_closing_brace boundary -> ()
    | _
      when statement_boundary_is_switch boundary
           && token_is_switch_boundary item.token -> ()
    | _ ->
        ignore (take cursor);
        seek_body ()
  in
  seek_body ()

let recover_for_header cursor ~boundary =
  let rec skip nested_parentheses nested_braces =
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '(' ->
        ignore (take cursor);
        skip (nested_parentheses + 1) nested_braces
    | Token_kind.Punctuation ')' ->
        ignore (take cursor);
        if nested_parentheses > 0 then
          skip (nested_parentheses - 1) nested_braces
        else if nested_braces > 0 then skip nested_parentheses nested_braces
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip nested_parentheses (nested_braces + 1)
    | Token_kind.Punctuation '}' when nested_braces > 0 ->
        ignore (take cursor);
        skip nested_parentheses (nested_braces - 1)
    | Token_kind.Punctuation '}'
      when statement_boundary_stops_at_closing_brace boundary -> ()
    | _ ->
        ignore (take cursor);
        skip nested_parentheses nested_braces
  in
  skip 0 0

let rec statement_body_boundary = function
  | For_update_boundary boundary -> statement_body_boundary boundary
  | boundary -> boundary

let is_for_update_boundary = function
  | For_update_boundary _ -> true
  | Top_level_boundary | Block_boundary | Switch_boundary -> false

let recover_compound_statement cursor =
  let rec skip depth =
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof -> ()
    | Token_kind.Punctuation '{' ->
        ignore (take cursor);
        skip (depth + 1)
    | Token_kind.Punctuation '}' ->
        ignore (take cursor);
        if depth > 1 then skip (depth - 1)
    | _ ->
        ignore (take cursor);
        skip depth
  in
  skip 0

let rec parse_pointer_layers cursor depth layers_rev items_rev =
  let item = peek cursor in
  match item.token.Token.kind with
  | Token_kind.Punctuation '*' ->
      if depth = max_pointer_depth then (
        report cursor item ~code:"HCPARSE0004"
          ~message:
            (Printf.sprintf
               "HolyC types may have at most %d pointer stars; this star \
                exceeds that limit"
               max_pointer_depth);
        recover_declaration cursor;
        None)
      else
        let item = take cursor in
        let depth = depth + 1 in
        let layer =
          Ast.make_pointer_layer ~depth ~spelling:item.token.raw
            ~location:(token_location item.token)
        in
        parse_pointer_layers cursor depth (layer :: layers_rev)
          (item :: items_rev)
  | _ -> Some (List.rev layers_rev, List.rev items_rev)

let pointer_definition_trace pointer_items =
  List.fold_left
    (fun trace item ->
      append_unique_related trace item.context.definition_trace)
    [] pointer_items

let type_spelling primitive_spelling pointer_layers =
  primitive_spelling ^ String.make (List.length pointer_layers) '*'

let primitive_type_of_token token =
  match token.Token.kind with
  | Token_kind.Identifier -> Sema.Primitive_type.of_spelling (token_text token)
  | _ -> None

let publish_global cursor (name : Ast.identifier) =
  ignore
    (Symbol_visibility.Environment.add cursor.symbols ~name:name.spelling
       ~kind:Symbol_visibility.Global_variable
       ~origin:(Symbol_visibility.Source_span name.location.span) ())

let publish_function cursor (name : Ast.identifier) =
  ignore
    (Symbol_visibility.Environment.add cursor.symbols ~name:name.spelling
       ~kind:Symbol_visibility.Function
       ~origin:(Symbol_visibility.Source_span name.location.span) ())

let publish_local cursor (name : Ast.identifier) =
  match cursor.local_context with
  | None -> invalid_arg "local declaration parsed outside a function context"
  | Some context -> (
      match
        Symbol_visibility.Environment.add_local cursor.symbols context
          ~name:name.spelling
      with
      | Ok () -> ()
      | Error message -> invalid_arg message)

let with_function_local_context cursor parameters variadic run =
  if Option.is_some cursor.local_context then
    invalid_arg "function local contexts cannot be nested";
  let context =
    Symbol_visibility.Environment.begin_local_context cursor.symbols
  in
  cursor.local_context <- Some context;
  List.iter
    (fun (parameter : Ast.function_parameter) ->
      Option.iter (publish_local cursor) parameter.name)
    parameters;
  Option.iter
    (fun _ ->
      let add_generated_name spelling =
        match
          Symbol_visibility.Environment.add_local cursor.symbols context
            ~name:spelling
        with
        | Ok () -> ()
        | Error message -> invalid_arg message
      in
      add_generated_name "argc";
      add_generated_name "argv")
    variadic;
  Fun.protect run ~finally:(fun () ->
      cursor.local_context <- None;
      match
        Symbol_visibility.Environment.end_local_context cursor.symbols context
      with
      | Ok () -> ()
      | Error message -> invalid_arg message)

let delimiter_kind token =
  match token.Token.kind with
  | Token_kind.Punctuation ',' -> Some Ast.Comma
  | Token_kind.Punctuation ';' -> Some Ast.Semicolon
  | _ -> None

let declaration_modifier_kind token =
  match token.Token.kind with
  | Token_kind.Keyword Keyword.Public -> Some Ast.Public
  | Token_kind.Keyword Keyword.Static -> Some Ast.Static
  | Token_kind.Keyword Keyword.Interrupt -> Some Ast.Interrupt
  | Token_kind.Keyword Keyword.Haserrcode -> Some Ast.Has_error_code
  | Token_kind.Keyword Keyword.Argpop -> Some Ast.Argument_pop
  | Token_kind.Keyword Keyword.Noargpop -> Some Ast.No_argument_pop
  | _ -> None

let rec parse_modifiers cursor (modifiers_rev : parsed_modifier list) =
  let item = peek cursor in
  match declaration_modifier_kind item.token with
  | None -> List.rev modifiers_rev
  | Some kind ->
      let item = take cursor in
      let node =
        Ast.make_declaration_modifier ~kind ~spelling:item.token.raw
          ~location:(token_location item.token)
      in
      parse_modifiers cursor ({ node; item } :: modifiers_rev)

let parse_declarator_prefix cursor primitive_spelling =
  match parse_pointer_layers cursor 0 [] [] with
  | None -> None
  | Some (pointer_layers, pointer_items) ->
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let pointer_trace = pointer_definition_trace pointer_items in
      let name_item = peek cursor in
      if name_item.token.kind = Token_kind.Punctuation '(' then (
        report ~secondary:pointer_trace cursor name_item ~code:"HCPARSE0005"
          ~message:
            "parenthesized function-pointer declarators are not implemented";
        recover_declaration cursor;
        None)
      else if name_item.token.kind <> Token_kind.Identifier then (
        report ~secondary:pointer_trace cursor name_item ~code:"HCPARSE0002"
          ~message:
            (Printf.sprintf "expected an identifier after type %S, but found %s"
               (type_spelling primitive_spelling pointer_layers)
               (token_description name_item.token));
        recover_declaration cursor;
        None)
      else
        let name_item = take cursor in
        let name =
          Ast.make_identifier ~spelling:name_item.token.raw
            ~location:(token_location name_item.token)
        in
        Some
          {
            pointer_layers;
            name;
            tokens = pointer_tokens @ [ name_item.token ];
            definition_trace = pointer_trace;
          }

let declaration_failure ?(secondary = []) cursor item ~code ~message =
  report ~secondary cursor item ~code ~message;
  recover_declaration cursor;
  None

let unsupported_parameter_form cursor item ~code description =
  declaration_failure cursor item ~code
    ~message:
      (Printf.sprintf "%s are not implemented in function prototypes"
         description)

let rec parse_register_qualifiers cursor ~position nodes_rev tokens_rev =
  let item = peek cursor in
  let kind =
    match item.token.kind with
    | Token_kind.Keyword Keyword.Reg -> Some Ast.Reg
    | Token_kind.Keyword Keyword.Noreg -> Some Ast.Noreg
    | _ -> None
  in
  match kind with
  | None -> { nodes = List.rev nodes_rev; tokens = List.rev tokens_rev }
  | Some kind ->
      let keyword_item = take cursor in
      let tokens_rev = keyword_item.token :: tokens_rev in
      let explicit_item =
        match kind with
        | Ast.Noreg -> None
        | Ast.Reg ->
            let candidate = peek cursor in
            if
              candidate.token.kind = Token_kind.Identifier
              && is_canonical_u64_register (token_text candidate.token)
            then Some (take cursor)
            else None
      in
      let tokens_rev =
        match explicit_item with
        | None -> tokens_rev
        | Some explicit_item -> explicit_item.token :: tokens_rev
      in
      let explicit_register =
        Option.map
          (fun explicit_item ->
            Ast.make_identifier ~spelling:explicit_item.token.raw
              ~location:(token_location explicit_item.token))
          explicit_item
      in
      let node =
        Ast.make_register_qualifier ~kind ~position
          ~spelling:keyword_item.token.raw ~explicit_register
          ~location:(token_location keyword_item.token)
      in
      parse_register_qualifiers cursor ~position (node :: nodes_rev) tokens_rev

let unary_operator_kind token =
  match token.Token.kind with
  | Token_kind.Punctuation '+' -> Some Ast.Unary_plus
  | Token_kind.Punctuation '-' -> Some Ast.Unary_minus
  | Token_kind.Punctuation '!' -> Some Ast.Logical_not
  | Token_kind.Punctuation '~' -> Some Ast.Bitwise_not
  | Token_kind.Punctuation '*' -> Some Ast.Dereference
  | Token_kind.Punctuation '&' -> Some Ast.Address_of
  | Token_kind.Operator Operator.Increment -> Some Ast.Pre_increment
  | Token_kind.Operator Operator.Decrement -> Some Ast.Pre_decrement
  | _ -> None

let postfix_operator_kind token =
  match token.Token.kind with
  | Token_kind.Operator Operator.Increment -> Some Ast.Post_increment
  | Token_kind.Operator Operator.Decrement -> Some Ast.Post_decrement
  | _ -> None

let is_postfix_continuation token =
  match token.Token.kind with
  | Token_kind.Punctuation ('(' | '[' | '.')
  | Token_kind.Operator
      (Operator.Arrow | Operator.Increment | Operator.Decrement) -> true
  | _ -> false

let restricted_modifier_term = function
  | Ast.Sizeof_expression _ -> Some ("sizeof", "HCPARSE0034")
  | Ast.Offset_expression _ -> Some ("offset", "HCPARSE0039")
  | Ast.Defined_expression _ -> Some ("defined", "HCPARSE0042")
  | _ -> None

let binary_operator token =
  match token.Token.kind with
  | Token_kind.Punctuation _ | Token_kind.Operator _ ->
      Operator.find_binary token.raw
  | _ -> None

let binary_binding_power (operator : Operator.binary_operator) =
  0x100 - operator.precedence_value

let make_expression_operator token =
  Ast.make_expression_operator ~spelling:token.Token.raw
    ~location:(token_location token)

let make_literal token value constructor =
  constructor
    (Ast.make_expression_literal ~spelling:token.Token.raw ~value
       ~location:(token_location token))

let rebuild_prefix prefix operand =
  let location =
    location_from_locations
      [
        prefix.Ast.prefix_operator.operator_location;
        Ast.expression_location operand;
      ]
  in
  Ast.Prefix_expression
    (Ast.make_prefix_expression ~operator_kind:prefix.prefix_operator_kind
       ~operator:prefix.prefix_operator ~operand ~location)

let rec split_power_sensitive_minus expression =
  match expression with
  | Ast.Prefix_expression prefix
    when prefix.prefix_operator_kind = Ast.Unary_minus ->
      Some (prefix.prefix_operand, fun operand -> rebuild_prefix prefix operand)
  | Ast.Prefix_expression prefix -> (
      match split_power_sensitive_minus prefix.prefix_operand with
      | None -> None
      | Some (base, wrap) ->
          Some (base, fun operand -> rebuild_prefix prefix (wrap operand)))
  | _ -> None

let make_binary_expression left operator_item operator_spec right =
  let operator = make_expression_operator operator_item.token in
  let location =
    location_from_locations
      [
        Ast.expression_location left;
        operator.operator_location;
        Ast.expression_location right;
      ]
  in
  Ast.Binary_expression
    (Ast.make_binary_expression ~left ~operator ~operator_spec ~right ~location)

let combine_binary_expression left operator_item operator_spec right =
  if String.equal operator_spec.Operator.ic_name "IC_POWER" then
    match split_power_sensitive_minus left with
    | None -> make_binary_expression left operator_item operator_spec right
    | Some (base, wrap) ->
        wrap (make_binary_expression base operator_item operator_spec right)
  else make_binary_expression left operator_item operator_spec right

let expression_failure ?(secondary = []) cursor item ~code ~message =
  declaration_failure ~secondary cursor item ~code ~message

let expression_context_name = function
  | Default_expression -> "default expression"
  | Array_dimension_expression -> "array dimension expression"
  | Intern_binding_expression -> "_intern target expression"
  | Call_argument_expression -> "call argument expression"
  | Index_expression -> "index expression"
  | Implicit_output_argument_expression -> "implicit output argument"
  | Return_expression -> "return expression"
  | Do_while_condition_expression -> "do-while condition expression"
  | For_condition_expression -> "for condition expression"
  | If_condition_expression -> "if condition expression"
  | Switch_expression -> "switch expression"
  | Switch_case_expression -> "switch case expression"
  | While_condition_expression -> "while condition expression"
  | Local_initializer_expression -> "local initializer expression"
  | Statement_expression -> "statement expression"

let expression_operand_name = function
  | Default_expression -> "a default expression operand"
  | Array_dimension_expression -> "an array dimension expression operand"
  | Intern_binding_expression -> "an _intern target expression operand"
  | Call_argument_expression -> "a call argument expression operand"
  | Index_expression -> "an index expression operand"
  | Implicit_output_argument_expression -> "an implicit output argument"
  | Return_expression -> "a return expression operand"
  | Do_while_condition_expression -> "a do-while condition expression operand"
  | For_condition_expression -> "a for condition expression operand"
  | If_condition_expression -> "an if condition expression operand"
  | Switch_expression -> "a switch expression operand"
  | Switch_case_expression -> "a switch case expression operand"
  | While_condition_expression -> "a while condition expression operand"
  | Local_initializer_expression -> "a local initializer expression operand"
  | Statement_expression -> "a statement expression operand"

let rec parse_expression cursor ~context ~depth ~minimum_binding_power :
    parsed_expression option =
  let item = peek cursor in
  if depth >= max_expression_depth then
    expression_failure cursor item ~code:"HCPARSE0021"
      ~message:
        (Printf.sprintf "%s nesting exceeds the hosted limit of %d"
           (expression_context_name context)
           max_expression_depth)
  else
    match parse_expression_prefix cursor ~context ~depth with
    | None -> None
    | Some left ->
        parse_expression_tail cursor ~context ~depth ~minimum_binding_power left

and parse_expression_prefix cursor ~context ~depth : parsed_expression option =
  let item = peek cursor in
  match unary_operator_kind item.token with
  | Some operator_kind -> (
      let operator_item = take cursor in
      let operator = make_expression_operator operator_item.token in
      match
        parse_expression cursor ~context ~depth:(depth + 1)
          ~minimum_binding_power:max_int
      with
      | None -> None
      | Some (operand : parsed_expression) ->
          let tokens = operator_item.token :: operand.tokens in
          let location = location_from_expression_tokens tokens in
          let node =
            Ast.Prefix_expression
              (Ast.make_prefix_expression ~operator_kind ~operator
                 ~operand:operand.node ~location)
          in
          Some { node; tokens })
  | None -> parse_expression_atom cursor ~context ~depth

and parse_expression_atom cursor ~context ~depth : parsed_expression option =
  let item = peek cursor in
  let take_literal value
      (constructor : Ast.expression_literal -> Ast.expression) :
      parsed_expression option =
    let item = take cursor in
    Some
      ({
         node = make_literal item.token value constructor;
         tokens = [ item.token ];
       }
        : parsed_expression)
  in
  match (item.token.Token.kind, item.token.value) with
  | Token_kind.Integer, Token.Int64 value ->
      take_literal (Ast.Integer_value value) (fun literal ->
          Ast.Integer_literal literal)
  | Token_kind.Float, Token.Float64 value ->
      take_literal (Ast.Float_value value) (fun literal ->
          Ast.Float_literal literal)
  | Token_kind.Character, Token.Int64 value ->
      take_literal (Ast.Integer_value value) (fun literal ->
          Ast.Character_literal literal)
  | Token_kind.String, Token.Bytes value ->
      take_literal (Ast.Bytes_value value) (fun literal ->
          Ast.String_literal literal)
  | Token_kind.Identifier, _ ->
      let item = take cursor in
      let node =
        Ast.Identifier_expression
          (Ast.make_identifier ~spelling:item.token.raw
             ~location:(token_location item.token))
      in
      Some { node; tokens = [ item.token ] }
  | Token_kind.Operator Operator.Current_position, _ ->
      let item = take cursor in
      let node =
        Ast.Current_position_expression (make_expression_operator item.token)
      in
      Some { node; tokens = [ item.token ] }
  | Token_kind.Keyword Keyword.Sizeof, _ ->
      parse_sizeof_expression cursor ~context
  | Token_kind.Keyword Keyword.Offset, _ ->
      parse_offset_expression cursor ~context
  | Token_kind.Keyword Keyword.Defined, _ ->
      parse_defined_expression cursor ~context
  | Token_kind.Punctuation '(', _ -> (
      let opening = take cursor in
      let first = peek cursor in
      match primitive_type_of_token first.token with
      | Some _ ->
          expression_failure cursor first ~code:"HCPARSE0029"
            ~message:
              (Printf.sprintf
                 "C-style cast syntax is not valid HolyC in %s; write the cast \
                  after its operand, for example value(%s)"
                 (expression_context_name context)
                 (token_text first.token))
      | None -> (
          match
            parse_expression cursor ~context ~depth:(depth + 1)
              ~minimum_binding_power:0
          with
          | None -> None
          | Some expression ->
              let closing = peek cursor in
              if closing.token.kind <> Token_kind.Punctuation ')' then
                expression_failure cursor closing ~code:"HCPARSE0019"
                  ~message:
                    (Printf.sprintf "expected ')' to close %s, but found %s"
                       (expression_context_name context)
                       (token_description closing.token))
              else
                let closing = take cursor in
                let tokens =
                  (opening.token :: expression.tokens) @ [ closing.token ]
                in
                let location = location_from_expression_tokens tokens in
                let node =
                  Ast.Parenthesized_expression
                    (Ast.make_parenthesized_expression
                       ~opening_parenthesis:(token_location opening.token)
                       ~expression:expression.node
                       ~closing_parenthesis:(token_location closing.token)
                       ~location)
                in
                Some { node; tokens }))
  | (Token_kind.Punctuation (',' | ')' | ']' | ';') | Token_kind.Eof), _ ->
      expression_failure cursor item ~code:"HCPARSE0018"
        ~message:
          (Printf.sprintf "expected %s, but found %s"
             (expression_operand_name context)
             (token_description item.token))
  | _ ->
      expression_failure cursor item ~code:"HCPARSE0020"
        ~message:
          (Printf.sprintf "%s form starting with %s is not implemented"
             (expression_context_name context)
             (token_description item.token))

and parse_sizeof_expression cursor ~context : parsed_expression option =
  let keyword_item = take cursor in
  let rec take_opening_parentheses items_rev =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '(' ->
        take_opening_parentheses (take cursor :: items_rev)
    | _ -> List.rev items_rev
  in
  let opening_items = take_opening_parentheses [] in
  let target_item = peek cursor in
  if target_item.token.kind <> Token_kind.Identifier then
    expression_failure cursor target_item ~code:"HCPARSE0031"
      ~message:
        (Printf.sprintf "expected a named sizeof target in %s, but found %s"
           (expression_context_name context)
           (token_description target_item.token))
  else
    let target_item = take cursor in
    let target =
      Ast.make_identifier ~spelling:target_item.token.raw
        ~location:(token_location target_item.token)
    in
    let rec take_members members_rev items_rev =
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Punctuation '.' ->
          let dot_item = take cursor in
          let name_item = peek cursor in
          if name_item.token.kind <> Token_kind.Identifier then
            expression_failure ~secondary:dot_item.context.definition_trace
              cursor name_item ~code:"HCPARSE0032"
              ~message:
                (Printf.sprintf
                   "expected a member name after '.' in sizeof target, but \
                    found %s"
                   (token_description name_item.token))
          else
            let name_item = take cursor in
            let name =
              Ast.make_identifier ~spelling:name_item.token.raw
                ~location:(token_location name_item.token)
            in
            let member =
              Ast.make_sizeof_member
                ~dot:(token_location dot_item.token)
                ~name
                ~location:
                  (location_from_expression_tokens
                     [ dot_item.token; name_item.token ])
            in
            take_members (member :: members_rev)
              (name_item :: dot_item :: items_rev)
      | _ -> Some (List.rev members_rev, List.rev items_rev)
    in
    match take_members [] [] with
    | None -> None
    | Some (members, member_items) -> (
        let rec take_pointer_layers depth layers_rev items_rev =
          let item = peek cursor in
          match item.token.kind with
          | Token_kind.Punctuation '*' ->
              let item = take cursor in
              let depth = depth + 1 in
              let layer =
                Ast.make_pointer_layer ~depth ~spelling:item.token.raw
                  ~location:(token_location item.token)
              in
              take_pointer_layers depth (layer :: layers_rev) (item :: items_rev)
          | _ -> (List.rev layers_rev, List.rev items_rev)
        in
        let pointer_layers, pointer_items = take_pointer_layers 0 [] [] in
        let items_before_closing =
          (keyword_item :: (opening_items @ (target_item :: member_items)))
          @ pointer_items
        in
        let definition_trace =
          List.fold_left
            (fun trace item ->
              append_unique_related trace item.context.definition_trace)
            [] items_before_closing
        in
        let rec take_closing_parentheses remaining items_rev =
          if remaining = 0 then Some (List.rev items_rev)
          else
            let item = peek cursor in
            match item.token.kind with
            | Token_kind.Punctuation ')' ->
                take_closing_parentheses (remaining - 1)
                  (take cursor :: items_rev)
            | _ ->
                let remaining_text =
                  if remaining = 1 then "one wrapper parenthesis remains"
                  else Printf.sprintf "%d wrapper parentheses remain" remaining
                in
                expression_failure ~secondary:definition_trace cursor item
                  ~code:"HCPARSE0033"
                  ~message:
                    (Printf.sprintf
                       "expected ')' to close sizeof target in %s; %s, but \
                        found %s"
                       (expression_context_name context)
                       remaining_text
                       (token_description item.token))
        in
        match take_closing_parentheses (List.length opening_items) [] with
        | None -> None
        | Some closing_items ->
            let tokens =
              List.map
                (fun item -> item.token)
                (items_before_closing @ closing_items)
            in
            let node =
              Ast.Sizeof_expression
                (Ast.make_sizeof_expression
                   ~keyword_spelling:keyword_item.token.raw
                   ~keyword_location:(token_location keyword_item.token)
                   ~opening_parentheses:
                     (List.map
                        (fun item -> token_location item.token)
                        opening_items)
                   ~target ~members ~pointer_layers
                   ~closing_parentheses:
                     (List.map
                        (fun item -> token_location item.token)
                        closing_items)
                   ~location:(location_from_expression_tokens tokens))
            in
            Some { node; tokens })

and parse_offset_expression cursor ~context : parsed_expression option =
  let keyword_item = take cursor in
  let rec take_opening_parentheses items_rev =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '(' ->
        take_opening_parentheses (take cursor :: items_rev)
    | _ -> List.rev items_rev
  in
  let opening_items = take_opening_parentheses [] in
  let target_item = peek cursor in
  if target_item.token.kind <> Token_kind.Identifier then
    expression_failure cursor target_item ~code:"HCPARSE0035"
      ~message:
        (Printf.sprintf "expected a named offset target in %s, but found %s"
           (expression_context_name context)
           (token_description target_item.token))
  else
    let target_item = take cursor in
    let target =
      Ast.make_identifier ~spelling:target_item.token.raw
        ~location:(token_location target_item.token)
    in
    let first_dot = peek cursor in
    if first_dot.token.kind <> Token_kind.Punctuation '.' then
      expression_failure cursor first_dot ~code:"HCPARSE0036"
        ~message:
          (Printf.sprintf
             "expected '.' after named offset target %S in %s, but found %s"
             target.spelling
             (expression_context_name context)
             (token_description first_dot.token))
    else
      let rec take_members members_rev items_rev =
        let dot_item = take cursor in
        let name_item = peek cursor in
        if name_item.token.kind <> Token_kind.Identifier then
          expression_failure ~secondary:dot_item.context.definition_trace cursor
            name_item ~code:"HCPARSE0037"
            ~message:
              (Printf.sprintf
                 "expected a member name after '.' in offset target, but found \
                  %s"
                 (token_description name_item.token))
        else
          let name_item = take cursor in
          let name =
            Ast.make_identifier ~spelling:name_item.token.raw
              ~location:(token_location name_item.token)
          in
          let member =
            Ast.make_offset_member
              ~dot:(token_location dot_item.token)
              ~name
              ~location:
                (location_from_expression_tokens
                   [ dot_item.token; name_item.token ])
          in
          let members_rev = member :: members_rev in
          let items_rev = name_item :: dot_item :: items_rev in
          let following = peek cursor in
          match following.token.kind with
          | Token_kind.Punctuation '.' -> take_members members_rev items_rev
          | _ -> Some (List.rev members_rev, List.rev items_rev)
      in
      match take_members [] [] with
      | None -> None
      | Some (members, member_items) -> (
          let items_before_closing =
            keyword_item :: (opening_items @ (target_item :: member_items))
          in
          let definition_trace =
            List.fold_left
              (fun trace item ->
                append_unique_related trace item.context.definition_trace)
              [] items_before_closing
          in
          let rec take_closing_parentheses remaining items_rev =
            if remaining = 0 then Some (List.rev items_rev)
            else
              let item = peek cursor in
              match item.token.kind with
              | Token_kind.Punctuation ')' ->
                  take_closing_parentheses (remaining - 1)
                    (take cursor :: items_rev)
              | _ ->
                  let remaining_text =
                    if remaining = 1 then "one wrapper parenthesis remains"
                    else
                      Printf.sprintf "%d wrapper parentheses remain" remaining
                  in
                  expression_failure ~secondary:definition_trace cursor item
                    ~code:"HCPARSE0038"
                    ~message:
                      (Printf.sprintf
                         "expected ')' to close offset target in %s; %s, but \
                          found %s"
                         (expression_context_name context)
                         remaining_text
                         (token_description item.token))
          in
          match take_closing_parentheses (List.length opening_items) [] with
          | None -> None
          | Some closing_items ->
              let tokens =
                List.map
                  (fun item -> item.token)
                  (items_before_closing @ closing_items)
              in
              let node =
                Ast.Offset_expression
                  (Ast.make_offset_expression
                     ~keyword_spelling:keyword_item.token.raw
                     ~keyword_location:(token_location keyword_item.token)
                     ~opening_parentheses:
                       (List.map
                          (fun item -> token_location item.token)
                          opening_items)
                     ~target ~members
                     ~closing_parentheses:
                       (List.map
                          (fun item -> token_location item.token)
                          closing_items)
                     ~location:(location_from_expression_tokens tokens))
              in
              Some { node; tokens })

and parse_defined_expression cursor ~context : parsed_expression option =
  let keyword_item = take cursor in
  let rec take_opening_parentheses items_rev =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '(' ->
        take_opening_parentheses (take cursor :: items_rev)
    | _ -> List.rev items_rev
  in
  let opening_items = take_opening_parentheses [] in
  let operand_item = peek cursor in
  if operand_item.token.kind = Token_kind.Eof then
    let definition_trace =
      List.fold_left
        (fun trace item ->
          append_unique_related trace item.context.definition_trace)
        keyword_item.context.definition_trace opening_items
    in
    expression_failure ~secondary:definition_trace cursor operand_item
      ~code:"HCPARSE0040"
      ~message:
        (Printf.sprintf
           "expected one token after defined in %s, but reached end of input"
           (expression_context_name context))
  else
    let operand_item = take cursor in
    let operand_kind =
      match operand_item.token.kind with
      | Token_kind.Identifier | Token_kind.Keyword _ -> Ast.Defined_name
      | _ -> Ast.Defined_non_name
    in
    let operand =
      Ast.make_defined_operand ~kind:operand_kind
        ~spelling:operand_item.token.raw
        ~location:(token_location operand_item.token)
    in
    let items_before_closing =
      keyword_item :: (opening_items @ [ operand_item ])
    in
    let definition_trace =
      List.fold_left
        (fun trace item ->
          append_unique_related trace item.context.definition_trace)
        [] items_before_closing
    in
    let rec take_closing_parentheses remaining items_rev =
      if remaining = 0 then Some (List.rev items_rev)
      else
        let item = peek cursor in
        match item.token.kind with
        | Token_kind.Punctuation ')' ->
            take_closing_parentheses (remaining - 1) (take cursor :: items_rev)
        | _ ->
            let remaining_text =
              if remaining = 1 then "one wrapper parenthesis remains"
              else Printf.sprintf "%d wrapper parentheses remain" remaining
            in
            expression_failure ~secondary:definition_trace cursor item
              ~code:"HCPARSE0041"
              ~message:
                (Printf.sprintf
                   "expected ')' to close defined target in %s; %s, but found \
                    %s"
                   (expression_context_name context)
                   remaining_text
                   (token_description item.token))
    in
    match take_closing_parentheses (List.length opening_items) [] with
    | None -> None
    | Some closing_items ->
        let tokens =
          List.map
            (fun item -> item.token)
            (items_before_closing @ closing_items)
        in
        let node =
          Ast.Defined_expression
            (Ast.make_defined_expression
               ~keyword_spelling:keyword_item.token.raw
               ~keyword_location:(token_location keyword_item.token)
               ~opening_parentheses:
                 (List.map
                    (fun item -> token_location item.token)
                    opening_items)
               ~operand
               ~closing_parentheses:
                 (List.map
                    (fun item -> token_location item.token)
                    closing_items)
               ~location:(location_from_expression_tokens tokens))
        in
        Some { node; tokens }

and parse_call_suffix cursor ~context ~depth (callee : parsed_expression)
    opening : parsed_expression option =
  let build arguments_rev interior_tokens_rev closing =
    let suffix_tokens = List.rev (closing.token :: interior_tokens_rev) in
    let tokens = callee.tokens @ (opening.token :: suffix_tokens) in
    let node =
      Ast.Call_expression
        (Ast.make_call_expression ~callee:callee.node
           ~opening_parenthesis:(token_location opening.token)
           ~arguments:(List.rev arguments_rev)
           ~closing_parenthesis:(token_location closing.token)
           ~location:(location_from_expression_tokens tokens))
    in
    Some ({ node; tokens } : parsed_expression)
  in
  let omitted_argument delimiter =
    Ast.make_call_argument ~value:Ast.Omitted_call_argument
      ~following_comma:None
      ~location:(location_before_token delimiter.token)
  in
  let rec parse_arguments arguments_rev interior_tokens_rev after_comma =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation ')' ->
        let closing = take cursor in
        let arguments_rev =
          if after_comma then omitted_argument closing :: arguments_rev
          else arguments_rev
        in
        build arguments_rev interior_tokens_rev closing
    | Token_kind.Punctuation ',' ->
        let comma = take cursor in
        let argument =
          Ast.make_call_argument ~value:Ast.Omitted_call_argument
            ~following_comma:(Some (token_location comma.token))
            ~location:(location_before_token comma.token)
        in
        parse_arguments
          (argument :: arguments_rev)
          (comma.token :: interior_tokens_rev)
          true
    | Token_kind.Eof ->
        expression_failure cursor item ~code:"HCPARSE0025"
          ~message:
            (Printf.sprintf "expected ')' to close a call in %s"
               (expression_context_name context))
    | _ -> (
        match
          parse_expression cursor ~context:Call_argument_expression
            ~depth:(depth + 1) ~minimum_binding_power:0
        with
        | None -> None
        | Some (expression : parsed_expression) -> (
            let following = peek cursor in
            let expression_tokens_rev =
              List.rev_append expression.tokens interior_tokens_rev
            in
            match following.token.kind with
            | Token_kind.Punctuation ',' ->
                let comma = take cursor in
                let argument =
                  Ast.make_call_argument
                    ~value:(Ast.Provided_call_argument expression.node)
                    ~following_comma:(Some (token_location comma.token))
                    ~location:(Ast.expression_location expression.node)
                in
                parse_arguments
                  (argument :: arguments_rev)
                  (comma.token :: expression_tokens_rev)
                  true
            | Token_kind.Punctuation ')' ->
                let closing = take cursor in
                let argument =
                  Ast.make_call_argument
                    ~value:(Ast.Provided_call_argument expression.node)
                    ~following_comma:None
                    ~location:(Ast.expression_location expression.node)
                in
                build (argument :: arguments_rev) expression_tokens_rev closing
            | Token_kind.Eof ->
                expression_failure cursor following ~code:"HCPARSE0025"
                  ~message:
                    (Printf.sprintf "expected ')' to close a call in %s"
                       (expression_context_name context))
            | _ ->
                expression_failure cursor following ~code:"HCPARSE0024"
                  ~message:
                    (Printf.sprintf
                       "expected ',' or ')' after a call argument, but found %s"
                       (token_description following.token))))
  in
  parse_arguments [] [] false

and parse_postfix_cast_suffix cursor ~context (operand : parsed_expression)
    opening primitive : parsed_expression option =
  let type_item = take cursor in
  let type_specifier =
    Ast.make_primitive_type ~primitive ~spelling:type_item.token.raw
      ~location:(token_location type_item.token)
  in
  match parse_pointer_layers cursor 0 [] [] with
  | None -> None
  | Some (pointer_layers, pointer_items) ->
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let definition_trace =
        append_unique_related opening.context.definition_trace
          type_item.context.definition_trace
        |> fun trace ->
        append_unique_related trace (pointer_definition_trace pointer_items)
      in
      let closing = peek cursor in
      if closing.token.kind <> Token_kind.Punctuation ')' then
        expression_failure ~secondary:definition_trace cursor closing
          ~code:"HCPARSE0030"
          ~message:
            (Printf.sprintf
               "expected ')' to close postfix cast to %S in %s, but found %s"
               (type_spelling (token_text type_item.token) pointer_layers)
               (expression_context_name context)
               (token_description closing.token))
      else
        let closing = take cursor in
        let tokens =
          operand.tokens
          @ (opening.token :: type_item.token :: pointer_tokens)
          @ [ closing.token ]
        in
        let node =
          Ast.Postfix_cast_expression
            (Ast.make_postfix_cast_expression ~operand:operand.node
               ~opening_parenthesis:(token_location opening.token)
               ~type_specifier ~pointer_layers
               ~closing_parenthesis:(token_location closing.token)
               ~location:(location_from_expression_tokens tokens))
        in
        Some { node; tokens }

and parse_index_suffix cursor ~context ~depth (base : parsed_expression) :
    parsed_expression option =
  let opening = take cursor in
  match
    parse_expression cursor ~context:Index_expression ~depth:(depth + 1)
      ~minimum_binding_power:0
  with
  | None -> None
  | Some (index : parsed_expression) ->
      let closing = peek cursor in
      if closing.token.kind <> Token_kind.Punctuation ']' then
        expression_failure cursor closing ~code:"HCPARSE0026"
          ~message:
            (Printf.sprintf
               "expected ']' to close an index expression in %s, but found %s"
               (expression_context_name context)
               (token_description closing.token))
      else
        let closing = take cursor in
        let tokens =
          base.tokens @ (opening.token :: index.tokens) @ [ closing.token ]
        in
        let node =
          Ast.Index_expression
            (Ast.make_index_expression ~base:base.node
               ~opening_bracket:(token_location opening.token)
               ~index:index.node
               ~closing_bracket:(token_location closing.token)
               ~location:(location_from_expression_tokens tokens))
        in
        Some { node; tokens }

and parse_member_suffix cursor ~context (base : parsed_expression) access_kind :
    parsed_expression option =
  let operator_item = take cursor in
  let member_item = peek cursor in
  if member_item.token.kind <> Token_kind.Identifier then
    expression_failure cursor member_item ~code:"HCPARSE0027"
      ~message:
        (Printf.sprintf "expected a member name after %S in %s, but found %s"
           operator_item.token.raw
           (expression_context_name context)
           (token_description member_item.token))
  else
    let member_item = take cursor in
    let operator = make_expression_operator operator_item.token in
    let member =
      Ast.make_identifier ~spelling:member_item.token.raw
        ~location:(token_location member_item.token)
    in
    let tokens = base.tokens @ [ operator_item.token; member_item.token ] in
    let node =
      Ast.Member_expression
        (Ast.make_member_expression ~base:base.node ~access_kind ~operator
           ~member
           ~location:(location_from_expression_tokens tokens))
    in
    Some { node; tokens }

and parse_postfix_update_suffix cursor ~context (operand : parsed_expression)
    operator_kind : parsed_expression option =
  let operator_item = take cursor in
  let operator = make_expression_operator operator_item.token in
  let tokens = operand.tokens @ [ operator_item.token ] in
  let node =
    Ast.Postfix_expression
      (Ast.make_postfix_expression ~operand:operand.node ~operator_kind
         ~operator
         ~location:(location_from_expression_tokens tokens))
  in
  let following = peek cursor in
  if is_postfix_continuation following.token then
    expression_failure cursor following ~code:"HCPARSE0028"
      ~message:
        (Printf.sprintf
           "postfix update %S must end the postfix chain in %s, but found %s"
           operator_item.token.raw
           (expression_context_name context)
           (token_description following.token))
  else Some { node; tokens }

and parse_expression_tail cursor ~context ~depth ~minimum_binding_power
    (left : parsed_expression) : parsed_expression option =
  let item = peek cursor in
  let restricted_term = restricted_modifier_term left.node in
  let invalid_direct_restricted_suffix =
    match (restricted_term, item.token.kind) with
    | ( Some _,
        ( Token_kind.Punctuation ('[' | '.')
        | Token_kind.Operator
            (Operator.Arrow | Operator.Increment | Operator.Decrement) ) ) ->
        true
    | _ -> false
  in
  if invalid_direct_restricted_suffix then
    let term_name, code = Option.get restricted_term in
    expression_failure cursor item ~code
      ~message:
        (Printf.sprintf
           "%s result cannot be followed directly by %s in %s; apply a postfix \
            cast before this suffix"
           term_name
           (token_description item.token)
           (expression_context_name context))
  else
    match item.token.kind with
    | Token_kind.Punctuation '(' -> (
        let opening = take cursor in
        let first = peek cursor in
        let suffix =
          match primitive_type_of_token first.token with
          | Some primitive ->
              parse_postfix_cast_suffix cursor ~context left opening primitive
          | None -> (
              match restricted_term with
              | Some (term_name, code) ->
                  if first.token.kind = Token_kind.Identifier then
                    expression_failure
                      ~secondary:opening.context.definition_trace cursor first
                      ~code:"HCPARSE0020"
                      ~message:
                        (Printf.sprintf
                           "resolved nonprimitive postfix cast target %s after \
                            %s is not implemented"
                           (token_description first.token)
                           term_name)
                  else
                    expression_failure
                      ~secondary:opening.context.definition_trace cursor first
                      ~code
                      ~message:
                        (Printf.sprintf
                           "expected a postfix cast target after %s in %s, but \
                            found %s"
                           term_name
                           (expression_context_name context)
                           (token_description first.token))
              | None -> parse_call_suffix cursor ~context ~depth left opening)
        in
        match suffix with
        | None -> None
        | Some expression ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              expression)
    | Token_kind.Punctuation '[' -> (
        match parse_index_suffix cursor ~context ~depth left with
        | None -> None
        | Some index ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              index)
    | Token_kind.Punctuation '.' -> (
        match parse_member_suffix cursor ~context left Ast.Direct_member with
        | None -> None
        | Some member ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              member)
    | Token_kind.Operator Operator.Arrow -> (
        match parse_member_suffix cursor ~context left Ast.Pointer_member with
        | None -> None
        | Some member ->
            parse_expression_tail cursor ~context ~depth ~minimum_binding_power
              member)
    | Token_kind.Operator (Operator.Increment | Operator.Decrement) -> (
        match postfix_operator_kind item.token with
        | None -> assert false
        | Some operator_kind -> (
            match
              parse_postfix_update_suffix cursor ~context left operator_kind
            with
            | None -> None
            | Some postfix ->
                parse_expression_binary_tail cursor ~context ~depth
                  ~minimum_binding_power postfix))
    | _ ->
        parse_expression_binary_tail cursor ~context ~depth
          ~minimum_binding_power left

and parse_expression_binary_tail cursor ~context ~depth ~minimum_binding_power
    (left : parsed_expression) : parsed_expression option =
  let item = peek cursor in
  match binary_operator item.token with
  | Some operator_spec
    when binary_binding_power operator_spec >= minimum_binding_power -> (
      let operator_item = take cursor in
      let binding_power = binary_binding_power operator_spec in
      let right_minimum =
        match operator_spec.association with
        | Operator.Right -> binding_power
        | Operator.Left | Operator.Unspecified -> binding_power + 1
      in
      match
        parse_expression cursor ~context ~depth:(depth + 1)
          ~minimum_binding_power:right_minimum
      with
      | None -> None
      | Some (right : parsed_expression) ->
          let node =
            combine_binary_expression left.node operator_item operator_spec
              right.node
          in
          let left : parsed_expression =
            {
              node;
              tokens = left.tokens @ (operator_item.token :: right.tokens);
            }
          in
          parse_expression_binary_tail cursor ~context ~depth
            ~minimum_binding_power left)
  | _ -> Some left

let parse_parameter_default cursor =
  let equals = take cursor in
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Keyword Keyword.Lastclass ->
      let keyword = take cursor in
      let tokens = [ equals.token; keyword.token ] in
      let lastclass =
        Ast.make_lastclass_default ~spelling:keyword.token.raw
          ~location:(token_location keyword.token)
      in
      let node =
        Ast.make_parameter_default
          ~equals:(token_location equals.token)
          ~value:(Ast.Lastclass_default lastclass)
          ~location:(location_from_expression_tokens tokens)
      in
      Some ({ node; tokens } : parsed_parameter_default)
  | _ -> (
      match
        parse_expression cursor ~context:Default_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None -> None
      | Some (expression : parsed_expression) ->
          let tokens = equals.token :: expression.tokens in
          let node =
            Ast.make_parameter_default
              ~equals:(token_location equals.token)
              ~value:(Ast.Expression_default expression.node)
              ~location:(location_from_expression_tokens tokens)
          in
          Some ({ node; tokens } : parsed_parameter_default))

let declaration_binding_kind token =
  match token.Token.kind with
  | Token_kind.Keyword Keyword.Extern -> Some (Ast.Extern, false)
  | Token_kind.Keyword Keyword.Import -> Some (Ast.Import, false)
  | Token_kind.Keyword Keyword.Underscore_extern -> Some (Ast.Extern, true)
  | Token_kind.Keyword Keyword.Underscore_import -> Some (Ast.Import, true)
  | _ -> None

let parse_binding cursor =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Keyword Keyword.Underscore_intern -> (
      let keyword = take cursor in
      let parsed_expression =
        parse_expression cursor ~context:Intern_binding_expression ~depth:0
          ~minimum_binding_power:0
      in
      match parsed_expression with
      | None -> Bad_binding
      | Some expression ->
          let node =
            Ast.make_declaration_binding ~kind:Ast.Intern
              ~spelling:keyword.token.raw
              ~location:(token_location keyword.token)
              ~target:(Ast.Expression_binding_target expression.node)
          in
          Parsed_binding
            { node; keyword; tokens = keyword.token :: expression.tokens })
  | _ -> (
      match declaration_binding_kind item.token with
      | None -> No_binding
      | Some (kind, false) ->
          let item = take cursor in
          let node =
            Ast.make_declaration_binding ~kind ~spelling:item.token.raw
              ~location:(token_location item.token)
              ~target:Ast.No_binding_target
          in
          Parsed_binding { node; keyword = item; tokens = [ item.token ] }
      | Some (kind, true) ->
          let keyword = take cursor in
          let target_item = peek cursor in
          if target_item.token.kind <> Token_kind.Identifier then (
            report cursor target_item ~code:"HCPARSE0007"
              ~message:
                (Printf.sprintf
                   "expected a target symbol after declaration binding %S, but \
                    found %s"
                   keyword.token.raw
                   (token_description target_item.token));
            recover_declaration cursor;
            Bad_binding)
          else
            let target_item = take cursor in
            let target =
              Ast.make_identifier ~spelling:target_item.token.raw
                ~location:(token_location target_item.token)
            in
            let node =
              Ast.make_declaration_binding ~kind ~spelling:keyword.token.raw
                ~location:(token_location keyword.token)
                ~target:(Ast.Symbol_binding_target target)
            in
            Parsed_binding
              { node; keyword; tokens = [ keyword.token; target_item.token ] })

let parse_array_dimension cursor ~index =
  let opening = take cursor in
  let next_item = peek cursor in
  if next_item.token.kind = Token_kind.Punctuation ']' then
    if index = 0 then
      let closing = take cursor in
      let tokens = [ opening.token; closing.token ] in
      let node =
        Ast.make_array_dimension
          ~opening_bracket:(token_location opening.token)
          ~dimension_expression:None
          ~closing_bracket:(token_location closing.token)
          ~location:(location_from_expression_tokens tokens)
      in
      Some ({ node; tokens } : parsed_array_dimension)
    else
      expression_failure cursor next_item ~code:"HCPARSE0022"
        ~message:"only the first array dimension may be empty"
  else
    match
      parse_expression cursor ~context:Array_dimension_expression ~depth:0
        ~minimum_binding_power:0
    with
    | None -> None
    | Some (expression : parsed_expression) ->
        let closing = peek cursor in
        if closing.token.kind <> Token_kind.Punctuation ']' then
          expression_failure cursor closing ~code:"HCPARSE0023"
            ~message:
              (Printf.sprintf
                 "expected ']' to close array dimension, but found %s"
                 (token_description closing.token))
        else
          let closing = take cursor in
          let tokens =
            (opening.token :: expression.tokens) @ [ closing.token ]
          in
          let node =
            Ast.make_array_dimension
              ~opening_bracket:(token_location opening.token)
              ~dimension_expression:(Some expression.node)
              ~closing_bracket:(token_location closing.token)
              ~location:(location_from_expression_tokens tokens)
          in
          Some ({ node; tokens } : parsed_array_dimension)

let rec parse_array_dimensions cursor index dimensions_rev token_groups_rev =
  let item = peek cursor in
  if item.token.kind <> Token_kind.Punctuation '[' then
    Some (List.rev dimensions_rev, token_groups_rev |> List.rev |> List.concat)
  else
    match parse_array_dimension cursor ~index with
    | None -> None
    | Some dimension ->
        parse_array_dimensions cursor (index + 1)
          (dimension.node :: dimensions_rev)
          (dimension.tokens :: token_groups_rev)

let parse_variable_declarator_suffix cursor prefix =
  match parse_array_dimensions cursor 0 [] [] with
  | None -> None
  | Some (array_dimensions, array_tokens) -> (
      let delimiter_item = peek cursor in
      match delimiter_kind delimiter_item.token with
      | None ->
          report ~secondary:prefix.definition_trace cursor delimiter_item
            ~code:"HCPARSE0003"
            ~message:
              (Printf.sprintf
                 "expected ';' after global variable %S, but found %s"
                 prefix.name.spelling
                 (token_description delimiter_item.token));
          recover_declaration cursor;
          None
      | Some kind ->
          let delimiter_item = take cursor in
          let delimiter =
            Ast.make_declaration_delimiter ~kind
              ~spelling:delimiter_item.token.raw
              ~location:(token_location delimiter_item.token)
          in
          let tokens =
            prefix.tokens @ array_tokens @ [ delimiter_item.token ]
          in
          let node =
            Ast.make_global_declarator ~pointer_layers:prefix.pointer_layers
              ~name:prefix.name ~array_dimensions ~delimiter
              ~location:(location_from_tokens tokens)
          in
          publish_global cursor prefix.name;
          Some ({ node; tokens } : parsed_declarator))

let parse_declarator cursor primitive_spelling =
  match parse_declarator_prefix cursor primitive_spelling with
  | None -> None
  | Some prefix -> parse_variable_declarator_suffix cursor prefix

let rec parse_declarators cursor primitive_spelling declarators_rev =
  match parse_declarator cursor primitive_spelling with
  | None -> None
  | Some declarator -> (
      let declarators_rev = declarator :: declarators_rev in
      match declarator.node.delimiter.kind with
      | Ast.Semicolon -> Some (List.rev declarators_rev)
      | Ast.Comma -> parse_declarators cursor primitive_spelling declarators_rev
      )

let finish_function_parameter cursor ~register_qualifiers ~type_specifier
    ~pointer_layers ~name ~function_pointer ~tokens =
  let parsed_default =
    let item = peek cursor in
    if item.token.kind = Token_kind.Punctuation '=' then
      Option.map (fun parsed -> Some parsed) (parse_parameter_default cursor)
    else Some None
  in
  match parsed_default with
  | None -> None
  | Some parsed_default -> (
      let following_item = peek cursor in
      let fail_special_form () =
        match following_item.token.kind with
        | Token_kind.Punctuation '[' when Option.is_none parsed_default ->
            unsupported_parameter_form cursor following_item ~code:"HCPARSE0011"
              "array parameters"
        | Token_kind.Keyword Keyword.Reg | Token_kind.Keyword Keyword.Noreg ->
            declaration_failure cursor following_item ~code:"HCPARSE0013"
              ~message:
                "register qualifier must appear before function parameter \
                 pointer stars or its name"
        | _ ->
            declaration_failure cursor following_item ~code:"HCPARSE0010"
              ~message:
                (Printf.sprintf
                   "expected ',' or ')' after function parameter, but found %s"
                   (token_description following_item.token))
      in
      match following_item.token.kind with
      | Token_kind.Punctuation ',' | Token_kind.Punctuation ')' ->
          let delimiter_item =
            match following_item.token.kind with
            | Token_kind.Punctuation ',' -> Some (take cursor)
            | _ -> None
          in
          let delimiter =
            Option.map
              (fun item ->
                Ast.make_declaration_delimiter ~kind:Ast.Comma
                  ~spelling:item.token.raw
                  ~location:(token_location item.token))
              delimiter_item
          in
          let default_tokens =
            match parsed_default with
            | None -> []
            | Some (parsed : parsed_parameter_default) -> parsed.tokens
          in
          let tokens =
            tokens @ default_tokens
            @ Option.to_list
                (Option.map (fun item -> item.token) delimiter_item)
          in
          let node =
            Ast.make_function_parameter ~register_qualifiers ~type_specifier
              ~pointer_layers ~name ~function_pointer
              ~default:
                (Option.map
                   (fun (parsed : parsed_parameter_default) -> parsed.node)
                   parsed_default)
              ~delimiter
              ~location:(location_from_tokens tokens)
          in
          Some ({ node; tokens } : parsed_parameter)
      | _ -> fail_special_form ())

let rec parse_function_parameter cursor ~prefix_qualifiers ~prefix_tokens
    ~function_pointer_depth =
  let type_item = peek cursor in
  let spelling = token_text type_item.token in
  match
    (type_item.token.Token.kind, Sema.Primitive_type.of_spelling spelling)
  with
  | Token_kind.Identifier, Some primitive -> (
      let type_item = take cursor in
      let type_specifier =
        Ast.make_primitive_type ~primitive ~spelling:type_item.token.raw
          ~location:(token_location type_item.token)
      in
      let suffix =
        parse_register_qualifiers cursor ~position:Ast.After_type [] []
      in
      let register_qualifiers = prefix_qualifiers @ suffix.nodes in
      match parse_pointer_layers cursor 0 [] [] with
      | None -> None
      | Some (pointer_layers, pointer_items) -> (
          let pointer_tokens =
            List.map (fun item -> item.token) pointer_items
          in
          let next_item = peek cursor in
          let leading_tokens =
            prefix_tokens @ [ type_item.token ] @ suffix.tokens @ pointer_tokens
          in
          match next_item.token.kind with
          | Token_kind.Punctuation '(' -> (
              match
                parse_function_pointer_declarator cursor ~function_pointer_depth
              with
              | None -> None
              | Some parsed ->
                  finish_function_parameter cursor ~register_qualifiers
                    ~type_specifier ~pointer_layers ~name:parsed.name
                    ~function_pointer:(Some parsed.node)
                    ~tokens:(leading_tokens @ parsed.tokens))
          | Token_kind.Identifier ->
              let name_item = take cursor in
              let name =
                Ast.make_identifier ~spelling:name_item.token.raw
                  ~location:(token_location name_item.token)
              in
              finish_function_parameter cursor ~register_qualifiers
                ~type_specifier ~pointer_layers ~name:(Some name)
                ~function_pointer:None
                ~tokens:(leading_tokens @ [ name_item.token ])
          | _ ->
              finish_function_parameter cursor ~register_qualifiers
                ~type_specifier ~pointer_layers ~name:None
                ~function_pointer:None ~tokens:leading_tokens))
  | _ ->
      declaration_failure cursor type_item ~code:"HCPARSE0009"
        ~message:
          (Printf.sprintf
             "expected a primitive function parameter type, but found %s"
             (token_description type_item.token))

and parse_function_pointer_declarator cursor ~function_pointer_depth =
  let opening_item = peek cursor in
  if function_pointer_depth >= max_function_pointer_depth then
    declaration_failure cursor opening_item ~code:"HCPARSE0017"
      ~message:
        (Printf.sprintf
           "function-pointer type nesting exceeds the hosted limit of %d"
           max_function_pointer_depth)
  else
    let opening_item = take cursor in
    let first_star = peek cursor in
    if first_star.token.kind <> Token_kind.Punctuation '*' then
      declaration_failure cursor first_star ~code:"HCPARSE0014"
        ~message:
          (Printf.sprintf
             "expected '*' after '(' in function-pointer parameter, but found \
              %s"
             (token_description first_star.token))
    else
      match parse_pointer_layers cursor 0 [] [] with
      | None -> None
      | Some (pointer_layers, pointer_items) -> (
          let pointer_tokens =
            List.map (fun item -> item.token) pointer_items
          in
          let name_item = peek cursor in
          let parsed_name =
            match name_item.token.kind with
            | Token_kind.Identifier ->
                let name_item = take cursor in
                Some
                  ( Ast.make_identifier ~spelling:name_item.token.raw
                      ~location:(token_location name_item.token),
                    name_item.token )
            | Token_kind.Punctuation ')' -> None
            | _ ->
                report cursor name_item ~code:"HCPARSE0014"
                  ~message:
                    (Printf.sprintf
                       "expected a function-pointer name or ')' after pointer \
                        stars, but found %s"
                       (token_description name_item.token));
                recover_declaration cursor;
                None
          in
          if
            name_item.token.kind <> Token_kind.Identifier
            && name_item.token.kind <> Token_kind.Punctuation ')'
          then None
          else
            let name = Option.map fst parsed_name in
            let name_tokens = Option.to_list (Option.map snd parsed_name) in
            let closing_item = peek cursor in
            if closing_item.token.kind <> Token_kind.Punctuation ')' then
              declaration_failure cursor closing_item ~code:"HCPARSE0014"
                ~message:
                  (Printf.sprintf
                     "expected ')' after function-pointer name, but found %s"
                     (token_description closing_item.token))
            else
              let closing_item = take cursor in
              let signature_opening = peek cursor in
              if signature_opening.token.kind <> Token_kind.Punctuation '(' then
                declaration_failure cursor signature_opening ~code:"HCPARSE0014"
                  ~message:
                    (Printf.sprintf
                       "expected '(' for function-pointer signature, but found \
                        %s"
                       (token_description signature_opening.token))
              else
                let signature_opening = take cursor in
                match
                  parse_function_parameters cursor [] [] ~after_comma:false
                    ~function_pointer_depth:(function_pointer_depth + 1)
                with
                | None -> None
                | Some parsed_parameters ->
                    let tokens =
                      [ opening_item.token ] @ pointer_tokens @ name_tokens
                      @ [ closing_item.token; signature_opening.token ]
                      @ parsed_parameters.tokens
                    in
                    let node =
                      Ast.make_function_pointer_declarator
                        ~declarator_opening_parenthesis:
                          (token_location opening_item.token)
                        ~indirection_layers:pointer_layers
                        ~declarator_closing_parenthesis:
                          (token_location closing_item.token)
                        ~signature_opening_parenthesis:
                          (token_location signature_opening.token)
                        ~signature_parameters:parsed_parameters.parameters
                        ~signature_variadic:parsed_parameters.variadic
                        ~signature_closing_parenthesis:
                          parsed_parameters.closing_parenthesis
                        ~function_pointer_location:(location_from_tokens tokens)
                    in
                    Some { node; name; tokens })

and parse_function_parameters cursor parameters_rev tokens_rev ~after_comma
    ~function_pointer_depth : parsed_parameter_list option =
  let prefix =
    parse_register_qualifiers cursor ~position:Ast.Before_type [] []
  in
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Punctuation ')' when (not after_comma) && prefix.nodes = [] ->
      let closing = take cursor in
      Some
        {
          parameters = List.rev parameters_rev;
          variadic = None;
          tokens = List.rev (closing.token :: tokens_rev);
          closing_parenthesis = token_location closing.token;
        }
  | Token_kind.Operator Operator.Ellipsis ->
      let ellipsis = take cursor in
      let closing = peek cursor in
      if closing.token.kind <> Token_kind.Punctuation ')' then
        declaration_failure cursor closing ~code:"HCPARSE0015"
          ~message:
            (Printf.sprintf "expected ')' after variadic marker, but found %s"
               (token_description closing.token))
      else
        let closing = take cursor in
        let variadic =
          Ast.make_variadic_marker ~register_qualifiers:prefix.nodes
            ~spelling:ellipsis.token.raw
            ~location:
              (location_from_tokens (prefix.tokens @ [ ellipsis.token ]))
        in
        let tokens_rev = List.rev_append prefix.tokens tokens_rev in
        Some
          {
            parameters = List.rev parameters_rev;
            variadic = Some variadic;
            tokens = List.rev (closing.token :: ellipsis.token :: tokens_rev);
            closing_parenthesis = token_location closing.token;
          }
  | Token_kind.Punctuation ')' ->
      declaration_failure cursor item ~code:"HCPARSE0009"
        ~message:
          (if prefix.nodes = [] then
             "expected a primitive function parameter type after ',', but \
              found ')'"
           else
             "expected a primitive function parameter type after register \
              qualifier, but found ')'")
  | _ -> (
      match
        parse_function_parameter cursor ~prefix_qualifiers:prefix.nodes
          ~prefix_tokens:prefix.tokens ~function_pointer_depth
      with
      | None -> None
      | Some parameter -> (
          let tokens_rev = List.rev_append parameter.tokens tokens_rev in
          let parameters_rev = parameter.node :: parameters_rev in
          match parameter.node.delimiter with
          | Some _ ->
              parse_function_parameters cursor parameters_rev tokens_rev
                ~after_comma:true ~function_pointer_depth
          | None ->
              parse_function_parameters cursor parameters_rev tokens_rev
                ~after_comma:false ~function_pointer_depth))

let parse_function_prototype cursor ~modifier_tokens ~modifiers ~binding_tokens
    ~binding ~type_item ~return_type (prefix : parsed_declarator_prefix) =
  let opening = take cursor in
  match
    parse_function_parameters cursor [] [] ~after_comma:false
      ~function_pointer_depth:0
  with
  | None -> None
  | Some parsed_parameters ->
      let semicolon_item = peek cursor in
      if semicolon_item.token.kind <> Token_kind.Punctuation ';' then (
        report cursor semicolon_item ~code:"HCPARSE0016"
          ~message:
            (Printf.sprintf
               "expected ';' after function prototype %S, but found %s"
               prefix.name.spelling
               (token_description semicolon_item.token));
        recover_declaration cursor;
        None)
      else
        let semicolon_item = take cursor in
        let declaration_tokens =
          modifier_tokens @ binding_tokens
          @ (type_item.token :: prefix.tokens)
          @ (opening.token :: parsed_parameters.tokens)
          @ [ semicolon_item.token ]
        in
        let prototype =
          Ast.make_function_prototype ~modifiers ~binding ~return_type
            ~return_pointer_layers:prefix.pointer_layers ~name:prefix.name
            ~opening_parenthesis:(token_location opening.token)
            ~parameters:parsed_parameters.parameters
            ~variadic:parsed_parameters.variadic
            ~closing_parenthesis:parsed_parameters.closing_parenthesis
            ~semicolon:(token_location semicolon_item.token)
            ~location:(location_from_tokens declaration_tokens)
        in
        publish_function cursor prefix.name;
        Some (Ast.Function_prototype prototype)

let parse_global cursor ~parse_function_definition =
  let parsed_modifiers = parse_modifiers cursor [] in
  let modifiers =
    List.map
      (fun (modifier : parsed_modifier) -> modifier.node)
      parsed_modifiers
  in
  let modifier_tokens =
    List.map
      (fun (modifier : parsed_modifier) -> modifier.item.token)
      parsed_modifiers
  in
  match parse_binding cursor with
  | Bad_binding -> None
  | Parsed_binding binding
    when binding.node.kind = Ast.Import
         && cursor.compilation_mode = Preprocessor.Jit ->
      report cursor binding.keyword ~code:"HCPARSE0006"
        ~message:
          "import declarations require AOT mode; select AOT mode before \
           parsing this declaration";
      recover_declaration cursor;
      None
  | binding_parse -> (
      let binding, binding_tokens =
        match binding_parse with
        | No_binding -> (None, [])
        | Parsed_binding binding -> (Some binding.node, binding.tokens)
        | Bad_binding -> assert false
      in
      let type_item = take cursor in
      let spelling = token_text type_item.token in
      match
        (type_item.token.Token.kind, Sema.Primitive_type.of_spelling spelling)
      with
      | Token_kind.Identifier, Some primitive -> (
          let type_specifier =
            Ast.make_primitive_type ~primitive ~spelling:type_item.token.raw
              ~location:(token_location type_item.token)
          in
          match parse_declarator_prefix cursor spelling with
          | None -> None
          | Some first_prefix -> (
              let next_item = peek cursor in
              match (next_item.token.kind, binding) with
              | Token_kind.Punctuation '(', Some binding ->
                  parse_function_prototype cursor ~modifier_tokens ~modifiers
                    ~binding_tokens ~binding ~type_item
                    ~return_type:type_specifier first_prefix
              | Token_kind.Punctuation '(', None ->
                  parse_function_definition cursor ~modifier_tokens ~modifiers
                    ~type_item ~return_type:type_specifier first_prefix
              | _ -> (
                  match
                    parse_variable_declarator_suffix cursor first_prefix
                  with
                  | None -> None
                  | Some first_declarator -> (
                      let parsed_declarators =
                        match first_declarator.node.delimiter.kind with
                        | Ast.Semicolon -> Some [ first_declarator ]
                        | Ast.Comma ->
                            parse_declarators cursor spelling
                              [ first_declarator ]
                      in
                      match parsed_declarators with
                      | None -> None
                      | Some declarators -> (
                          let declaration_tokens =
                            modifier_tokens @ binding_tokens
                            @ type_item.token
                              :: List.concat_map
                                   (fun (item : parsed_declarator) ->
                                     item.tokens)
                                   declarators
                          in
                          match declarators with
                          | [ declarator ] ->
                              let variable =
                                Ast.make_global_variable ~modifiers ~binding
                                  ~type_specifier
                                  ~pointer_layers:declarator.node.pointer_layers
                                  ~name:declarator.node.name
                                  ~array_dimensions:
                                    declarator.node.array_dimensions
                                  ~semicolon:
                                    declarator.node.delimiter.location.span
                                  ~location:
                                    (location_from_tokens declaration_tokens)
                              in
                              Some (Ast.Global_variable variable)
                          | _ ->
                              let declaration =
                                Ast.make_global_declaration ~modifiers ~binding
                                  ~type_specifier
                                  ~declarators:
                                    (List.map
                                       (fun (item : parsed_declarator) ->
                                         item.node)
                                       declarators)
                                  ~location:
                                    (location_from_tokens declaration_tokens)
                              in
                              Some (Ast.Global_declaration declaration))))))
      | _ ->
          let prefix =
            match binding with
            | Some (binding : Ast.declaration_binding) ->
                Printf.sprintf "after declaration binding %S" binding.spelling
            | None -> (
                match modifiers with
                | [] -> "at the start of a global declaration"
                | _ ->
                    Printf.sprintf "after declaration modifier%s %S"
                      (if List.length modifiers = 1 then "" else "s")
                      (modifiers
                      |> List.map (fun (modifier : Ast.declaration_modifier) ->
                          modifier.spelling)
                      |> String.concat " "))
          in
          report cursor type_item ~code:"HCPARSE0001"
            ~message:
              (Printf.sprintf "expected a primitive type %s, but found %s"
                 prefix
                 (token_description type_item.token));
          recover_declaration cursor;
          None)

let parse_implicit_output_statement cursor ~boundary : parsed_statement option =
  let marker_item = peek cursor in
  let target, marker_value, empty_marker =
    match (marker_item.token.Token.kind, marker_item.token.value) with
    | Token_kind.String, Token.Bytes value ->
        ( Ast.Print_target,
          Ast.Bytes_value value,
          String.length value = 0 || Char.equal value.[0] '\000' )
    | Token_kind.Character, Token.Int64 value ->
        (Ast.Put_chars_target, Ast.Integer_value value, Int64.equal value 0L)
    | _ -> invalid_arg "an implicit output statement needs a literal marker"
  in
  let marker =
    Ast.make_expression_literal ~spelling:marker_item.token.raw
      ~value:marker_value
      ~location:(token_location marker_item.token)
  in
  let fixed_argument =
    if empty_marker then (
      ignore (take cursor);
      let next_item = peek cursor in
      match next_item.token.kind with
      | Token_kind.Punctuation (';' | ',') | Token_kind.Eof ->
          let target_name =
            match target with
            | Ast.Print_target -> "format"
            | Ast.Put_chars_target -> "character"
          in
          report cursor next_item ~code:"HCPARSE0043"
            ~message:
              (Printf.sprintf
                 "empty %s output marker must be followed by a %s expression"
                 (if target = Ast.Print_target then "string" else "character")
                 target_name);
          None
      | _ ->
          parse_expression cursor ~context:Implicit_output_argument_expression
            ~depth:0 ~minimum_binding_power:0
          |> Option.map (fun (expression : parsed_expression) ->
              ( Ast.Expression_fixed_argument expression.node,
                marker_item.token :: expression.tokens )))
    else
      parse_expression cursor ~context:Implicit_output_argument_expression
        ~depth:0 ~minimum_binding_power:0
      |> Option.map (fun (expression : parsed_expression) ->
          (Ast.Marker_fixed_argument expression.node, expression.tokens))
  in
  match fixed_argument with
  | None ->
      recover_statement cursor ~boundary;
      None
  | Some (fixed_argument, fixed_tokens) -> (
      let rec parse_print_arguments arguments_rev tokens_rev =
        let item = peek cursor in
        match item.token.kind with
        | Token_kind.Punctuation ',' -> (
            let comma_item = take cursor in
            let argument_item = peek cursor in
            match argument_item.token.kind with
            | Token_kind.Punctuation (';' | ',') | Token_kind.Eof ->
                report cursor argument_item ~code:"HCPARSE0044"
                  ~message:
                    "expected a Print argument expression after ',', but found \
                     an empty argument";
                None
            | _ -> (
                match
                  parse_expression cursor
                    ~context:Implicit_output_argument_expression ~depth:0
                    ~minimum_binding_power:0
                with
                | None -> None
                | Some (expression : parsed_expression) ->
                    let argument_tokens =
                      comma_item.token :: expression.tokens
                    in
                    let argument =
                      Ast.make_implicit_output_argument
                        ~leading_comma:(token_location comma_item.token)
                        ~value:expression.node
                        ~location:
                          (location_from_expression_tokens argument_tokens)
                    in
                    parse_print_arguments
                      (argument :: arguments_rev)
                      (List.rev_append argument_tokens tokens_rev)))
        | _ -> Some (List.rev arguments_rev, List.rev tokens_rev)
      in
      let parsed_arguments =
        match target with
        | Ast.Print_target -> parse_print_arguments [] []
        | Ast.Put_chars_target -> Some ([], [])
      in
      match parsed_arguments with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (arguments, argument_tokens) -> (
          let terminator_item = peek cursor in
          let terminator =
            match (boundary, terminator_item.token.kind) with
            | For_update_boundary _, _ -> Some (None, [])
            | _, Token_kind.Punctuation ';' ->
                let semicolon_item = take cursor in
                Some
                  ( Some (token_location semicolon_item.token),
                    [ semicolon_item.token ] )
            | _, Token_kind.Punctuation ',' when target = Ast.Put_chars_target
              -> Some (None, [])
            | _ ->
                report cursor terminator_item ~code:"HCPARSE0046"
                  ~message:
                    (Printf.sprintf
                       "expected ';' after implicit %s statement, but found %s"
                       (match target with
                       | Ast.Print_target -> "Print"
                       | Ast.Put_chars_target -> "PutChars")
                       (token_description terminator_item.token));
                None
          in
          match terminator with
          | None ->
              recover_statement cursor ~boundary;
              None
          | Some (semicolon, terminator_tokens) ->
              let tokens = fixed_tokens @ argument_tokens @ terminator_tokens in
              let statement =
                Ast.make_implicit_output_statement ~target ~marker
                  ~fixed_argument ~arguments ~semicolon
                  ~location:(location_from_expression_tokens tokens)
              in
              Some { node = Ast.Implicit_output_statement statement; tokens }))

let rec take_statement_commas cursor items_rev =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Punctuation ',' ->
      take_statement_commas cursor (take cursor :: items_rev)
  | _ -> List.rev items_rev

let statement_symbol_is_expression cursor name =
  match Symbol_visibility.Environment.find_preprocessor cursor.symbols name with
  | Symbol_visibility.Absent -> false
  | Symbol_visibility.Shadowed_by_local -> true
  | Symbol_visibility.Present entry -> (
      match Symbol_visibility.kind entry with
      | Symbol_visibility.Export_system_symbol
      | Symbol_visibility.Global_variable
      | Symbol_visibility.Function
      | Symbol_visibility.Word
      | Symbol_visibility.Dictionary_word
      | Symbol_visibility.Frame_pointer -> true
      | Symbol_visibility.Import_system_symbol
      | Symbol_visibility.Definition
      | Symbol_visibility.Class
      | Symbol_visibility.Internal_type
      | Symbol_visibility.Keyword
      | Symbol_visibility.Assembly_keyword
      | Symbol_visibility.Opcode
      | Symbol_visibility.Register
      | Symbol_visibility.File
      | Symbol_visibility.Module
      | Symbol_visibility.Help_file -> false)

let token_starts_function_label cursor token =
  match token.Token.kind with
  | Token_kind.Identifier -> (
      match
        Symbol_visibility.Environment.find_preprocessor cursor.symbols
          (token_text token)
      with
      | Symbol_visibility.Absent ->
          (peek_n cursor 1).token.kind = Token_kind.Punctuation ':'
      | Symbol_visibility.Shadowed_by_local | Symbol_visibility.Present _ ->
          false)
  | _ -> false

let token_starts_statement_expression cursor token =
  match token.Token.kind with
  | Token_kind.Integer | Token_kind.Float -> true
  | Token_kind.Identifier ->
      statement_symbol_is_expression cursor (token_text token)
  | Token_kind.Punctuation ('(' | '+' | '-' | '!' | '~' | '*' | '&') -> true
  | Token_kind.Operator
      (Operator.Increment | Operator.Decrement | Operator.Current_position) ->
      true
  | Token_kind.Keyword (Keyword.Sizeof | Keyword.Offset | Keyword.Defined) ->
      true
  | _ -> false

let token_starts_global_declaration cursor token =
  Option.is_some (primitive_type_of_token token)
  || Option.is_some (declaration_modifier_kind token)
  || Option.is_some (declaration_binding_kind token)
  || token.kind = Token_kind.Keyword Keyword.Underscore_intern
  ||
  match token.kind with
  | Token_kind.Identifier ->
      not (statement_symbol_is_expression cursor (token_text token))
  | _ -> false

let parse_empty_statement cursor : parsed_statement =
  let semicolon_item = take cursor in
  let location = token_location semicolon_item.token in
  {
    node =
      Ast.Empty_statement
        (Ast.make_empty_statement ~semicolon:location ~location);
    tokens = [ semicolon_item.token ];
  }

let parse_break_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let terminator_item = peek cursor in
  let terminator =
    match (boundary, terminator_item.token.kind) with
    | For_update_boundary _, _ -> Some (None, [])
    | _, Token_kind.Punctuation ';' ->
        let semicolon_item = take cursor in
        Some
          (Some (token_location semicolon_item.token), [ semicolon_item.token ])
    | _, Token_kind.Punctuation ',' -> Some (None, [])
    | _ ->
        report cursor terminator_item ~code:"HCPARSE0072"
          ~message:
            (Printf.sprintf "expected ';' or ',' after 'break', but found %s"
               (token_description terminator_item.token));
        None
  in
  match terminator with
  | None ->
      recover_statement cursor ~boundary;
      None
  | Some (semicolon, terminator_tokens) ->
      let tokens = keyword_item.token :: terminator_tokens in
      let statement =
        Ast.make_break_statement
          ~keyword:(token_location keyword_item.token)
          ~semicolon
          ~location:(location_from_expression_tokens tokens)
      in
      Some { node = Ast.Break_statement statement; tokens }

let parse_goto_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let target_item = peek cursor in
  if target_item.token.kind <> Token_kind.Identifier then (
    report cursor target_item ~code:"HCPARSE0075"
      ~message:
        (Printf.sprintf "expected a label name after 'goto', but found %s"
           (token_description target_item.token));
    recover_statement cursor ~boundary;
    None)
  else
    let target_item = take cursor in
    let terminator_item = peek cursor in
    let terminator =
      match (boundary, terminator_item.token.kind) with
      | For_update_boundary _, _ -> Some (None, [])
      | _, Token_kind.Punctuation ';' ->
          let semicolon_item = take cursor in
          Some
            ( Some (token_location semicolon_item.token),
              [ semicolon_item.token ] )
      | _, Token_kind.Punctuation ',' -> Some (None, [])
      | _ ->
          report cursor terminator_item ~code:"HCPARSE0076"
            ~message:
              (Printf.sprintf
                 "expected ';' or ',' after goto target %S, but found %s"
                 (token_text target_item.token)
                 (token_description terminator_item.token));
          None
    in
    match terminator with
    | None ->
        recover_statement cursor ~boundary;
        None
    | Some (semicolon, terminator_tokens) ->
        let tokens =
          keyword_item.token :: target_item.token :: terminator_tokens
        in
        let target =
          Ast.make_identifier
            ~spelling:(token_text target_item.token)
            ~location:(token_location target_item.token)
        in
        let statement =
          Ast.make_goto_statement
            ~keyword:(token_location keyword_item.token)
            ~target ~semicolon
            ~location:(location_from_expression_tokens tokens)
        in
        Some { node = Ast.Goto_statement statement; tokens }

let parse_label_statement cursor : parsed_statement option =
  let name_item = take cursor in
  let colon_item = take cursor in
  let tokens = [ name_item.token; colon_item.token ] in
  let name =
    Ast.make_identifier
      ~spelling:(token_text name_item.token)
      ~location:(token_location name_item.token)
  in
  let statement =
    Ast.make_label_statement ~name
      ~colon:(token_location colon_item.token)
      ~location:(location_from_expression_tokens tokens)
  in
  Some { node = Ast.Label_statement statement; tokens }

let parse_return_statement cursor ~boundary : parsed_statement option =
  let keyword_item = take cursor in
  let build value value_tokens semicolon terminator_tokens :
      parsed_statement option =
    let tokens = keyword_item.token :: (value_tokens @ terminator_tokens) in
    let statement =
      Ast.make_return_statement
        ~keyword:(token_location keyword_item.token)
        ~value ~semicolon
        ~location:(location_from_expression_tokens tokens)
    in
    Some { node = Ast.Return_statement statement; tokens }
  in
  let first_item = peek cursor in
  match (boundary, first_item.token.kind) with
  | For_update_boundary _, Token_kind.Punctuation ';' ->
      report cursor first_item ~code:"HCPARSE0070"
        ~message:"expected ')' after the for update, but found ';'";
      recover_statement cursor ~boundary;
      None
  | _, Token_kind.Punctuation ';' ->
      let semicolon_item = take cursor in
      build None []
        (Some (token_location semicolon_item.token))
        [ semicolon_item.token ]
  | _, (Token_kind.Punctuation (',' | ')' | ']' | '}') | Token_kind.Eof) ->
      report cursor first_item ~code:"HCPARSE0074"
        ~message:
          (Printf.sprintf "expected a return expression or ';', but found %s"
             (token_description first_item.token));
      recover_statement cursor ~boundary;
      None
  | _ -> (
      match
        parse_expression cursor ~context:Return_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (value : parsed_expression) -> (
          let terminator_item = peek cursor in
          let terminator =
            match (boundary, terminator_item.token.kind) with
            | For_update_boundary _, _ -> Some (None, [])
            | _, Token_kind.Punctuation ';' ->
                let semicolon_item = take cursor in
                Some
                  ( Some (token_location semicolon_item.token),
                    [ semicolon_item.token ] )
            | _, Token_kind.Punctuation ',' -> Some (None, [])
            | _ ->
                report cursor terminator_item ~code:"HCPARSE0073"
                  ~message:
                    (Printf.sprintf
                       "expected ';' or ',' after a return expression, but \
                        found %s"
                       (token_description terminator_item.token));
                None
          in
          match terminator with
          | None ->
              recover_statement cursor ~boundary;
              None
          | Some (semicolon, terminator_tokens) ->
              build (Some value.node) value.tokens semicolon terminator_tokens))

let parse_expression_statement cursor ~boundary : parsed_statement option =
  match
    parse_expression cursor ~context:Statement_expression ~depth:0
      ~minimum_binding_power:0
  with
  | None -> None
  | Some (expression : parsed_expression) -> (
      let terminator_item = peek cursor in
      let terminator =
        match (boundary, terminator_item.token.kind) with
        | For_update_boundary _, _ -> Some (None, [])
        | _, Token_kind.Punctuation ';' ->
            let semicolon_item = take cursor in
            Some
              ( Some (token_location semicolon_item.token),
                [ semicolon_item.token ] )
        | _, Token_kind.Punctuation ',' -> Some (None, [])
        | _ ->
            report cursor terminator_item ~code:"HCPARSE0047"
              ~message:
                (Printf.sprintf
                   "expected ';' or ',' after statement expression, but found \
                    %s"
                   (token_description terminator_item.token));
            None
      in
      match terminator with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (semicolon, terminator_tokens) ->
          let tokens = expression.tokens @ terminator_tokens in
          let statement =
            Ast.make_expression_statement ~expression:expression.node ~semicolon
              ~location:(location_from_expression_tokens tokens)
          in
          Some { node = Ast.Expression_statement statement; tokens })

let local_declaration_failure cursor ~boundary item ~code ~message =
  report cursor item ~code ~message;
  recover_statement cursor ~boundary;
  None

let rec take_static_local_modifiers cursor nodes_rev tokens_rev =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Keyword Keyword.Static ->
      let item = take cursor in
      let node =
        Ast.make_declaration_modifier ~kind:Ast.Static ~spelling:item.token.raw
          ~location:(token_location item.token)
      in
      take_static_local_modifiers cursor (node :: nodes_rev)
        (item.token :: tokens_rev)
  | _ -> (List.rev nodes_rev, List.rev tokens_rev)

let parse_local_declarator cursor ~boundary ~primitive_spelling
    ~register_qualifiers ~qualifier_tokens : parsed_local_declarator option =
  match parse_pointer_layers cursor 0 [] [] with
  | None -> None
  | Some (pointer_layers, pointer_items) -> (
      let pointer_tokens = List.map (fun item -> item.token) pointer_items in
      let name_item = peek cursor in
      if name_item.token.kind = Token_kind.Punctuation '(' then
        local_declaration_failure cursor ~boundary name_item ~code:"HCPARSE0103"
          ~message:
            "local function-pointer declarators are not implemented in this \
             parser slice"
      else if name_item.token.kind <> Token_kind.Identifier then
        local_declaration_failure cursor ~boundary name_item ~code:"HCPARSE0100"
          ~message:
            (Printf.sprintf
               "expected a local variable name after type %S, but found %s"
               (type_spelling primitive_spelling pointer_layers)
               (token_description name_item.token))
      else
        let name_item = take cursor in
        let name =
          Ast.make_identifier ~spelling:name_item.token.raw
            ~location:(token_location name_item.token)
        in
        match parse_array_dimensions cursor 0 [] [] with
        | None -> None
        | Some (array_dimensions, array_tokens) ->
            publish_local cursor name;
            let equals_item = peek cursor in
            let parsed_initializer =
              if equals_item.token.kind <> Token_kind.Punctuation '=' then
                Some (None, [])
              else
                let equals_item = take cursor in
                let value_item = peek cursor in
                match value_item.token.kind with
                | Token_kind.Punctuation (';' | ',')
                | Token_kind.Punctuation '}'
                | Token_kind.Eof ->
                    local_declaration_failure cursor ~boundary value_item
                      ~code:"HCPARSE0101"
                      ~message:
                        (Printf.sprintf
                           "expected a scalar initializer for local variable \
                            %S, but found %s"
                           name.spelling
                           (token_description value_item.token))
                | Token_kind.Punctuation '{' ->
                    local_declaration_failure cursor ~boundary value_item
                      ~code:"HCPARSE0104"
                      ~message:
                        "aggregate local initializers are not implemented in \
                         this parser slice"
                | _ -> (
                    match
                      parse_expression cursor
                        ~context:Local_initializer_expression ~depth:0
                        ~minimum_binding_power:0
                    with
                    | None -> None
                    | Some (value : parsed_expression) ->
                        let tokens = equals_item.token :: value.tokens in
                        let initial_value =
                          Ast.make_local_initializer
                            ~equals:(token_location equals_item.token)
                            ~value:value.node
                            ~location:(location_from_expression_tokens tokens)
                        in
                        Some (Some initial_value, tokens))
            in
            Option.bind parsed_initializer
              (fun (initial_value, initializer_tokens) ->
                let delimiter_item = peek cursor in
                match delimiter_kind delimiter_item.token with
                | None ->
                    local_declaration_failure cursor ~boundary delimiter_item
                      ~code:"HCPARSE0102"
                      ~message:
                        (Printf.sprintf
                           "expected ',' or ';' after local variable %S, but \
                            found %s"
                           name.spelling
                           (token_description delimiter_item.token))
                | Some kind ->
                    let delimiter_item = take cursor in
                    let delimiter =
                      Ast.make_declaration_delimiter ~kind
                        ~spelling:delimiter_item.token.raw
                        ~location:(token_location delimiter_item.token)
                    in
                    let tokens =
                      qualifier_tokens @ pointer_tokens @ [ name_item.token ]
                      @ array_tokens @ initializer_tokens
                      @ [ delimiter_item.token ]
                    in
                    let node =
                      Ast.make_local_declarator ~register_qualifiers
                        ~pointer_layers ~name ~array_dimensions ~initial_value
                        ~delimiter
                        ~location:(location_from_expression_tokens tokens)
                    in
                    Some ({ node; tokens } : parsed_local_declarator)))

let parse_local_declaration cursor ~boundary : parsed_statement option =
  let first_item = peek cursor in
  let storage, modifiers, modifier_tokens =
    match first_item.token.kind with
    | Token_kind.Keyword Keyword.Static ->
        let modifiers, tokens = take_static_local_modifiers cursor [] [] in
        (Ast.Static_local, modifiers, tokens)
    | _ -> (Ast.Automatic_local, [], [])
  in
  let type_item = peek cursor in
  let spelling = token_text type_item.token in
  match
    (type_item.token.Token.kind, Sema.Primitive_type.of_spelling spelling)
  with
  | Token_kind.Identifier, Some primitive ->
      let type_item = take cursor in
      let type_specifier =
        Ast.make_primitive_type ~primitive ~spelling:type_item.token.raw
          ~location:(token_location type_item.token)
      in
      let rec parse_declarators declarators_rev =
        let qualifiers =
          match storage with
          | Ast.Automatic_local ->
              parse_register_qualifiers cursor ~position:Ast.After_type [] []
          | Ast.Static_local -> { nodes = []; tokens = [] }
        in
        let qualifier_item = peek cursor in
        if
          storage = Ast.Static_local
          &&
          match qualifier_item.token.kind with
          | Token_kind.Keyword (Keyword.Reg | Keyword.Noreg) -> true
          | _ -> false
        then
          local_declaration_failure cursor ~boundary qualifier_item
            ~code:"HCPARSE0099"
            ~message:
              "register qualifiers are not accepted on static local \
               declarations by the pinned parser"
        else
          match
            parse_local_declarator cursor ~boundary ~primitive_spelling:spelling
              ~register_qualifiers:qualifiers.nodes
              ~qualifier_tokens:qualifiers.tokens
          with
          | None -> None
          | Some declarator -> (
              let declarators_rev = declarator :: declarators_rev in
              match declarator.node.local_delimiter.kind with
              | Ast.Semicolon -> Some (List.rev declarators_rev)
              | Ast.Comma -> parse_declarators declarators_rev)
      in
      Option.map
        (fun declarators ->
          let tokens =
            modifier_tokens @ [ type_item.token ]
            @ List.concat_map
                (fun (declarator : parsed_local_declarator) ->
                  declarator.tokens)
                declarators
          in
          let declaration =
            Ast.make_local_declaration ~storage ~modifiers ~type_specifier
              ~declarators:
                (List.map
                   (fun (declarator : parsed_local_declarator) ->
                     declarator.node)
                   declarators)
              ~location:(location_from_expression_tokens tokens)
          in
          ({ node = Ast.Local_declaration_statement declaration; tokens }
            : parsed_statement))
        (parse_declarators [])
  | _ ->
      let code, message =
        match (storage, type_item.token.kind) with
        | Ast.Static_local, Token_kind.Keyword (Keyword.Reg | Keyword.Noreg) ->
            ( "HCPARSE0099",
              "register qualifiers are not accepted before a static local's \
               type by the pinned parser" )
        | _ ->
            ( "HCPARSE0098",
              Printf.sprintf
                "expected a primitive type after local declaration modifier, \
                 but found %s"
                (token_description type_item.token) )
      in
      local_declaration_failure cursor ~boundary type_item ~code ~message

let rec parse_statement_atom cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Punctuation '{' ->
      parse_block_statement cursor ~block_depth ~conditional_depth ~loop_depth
        ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Break -> parse_break_statement cursor ~boundary
  | Token_kind.Keyword Keyword.Do ->
      parse_do_while_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.For ->
      parse_for_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Goto -> parse_goto_statement cursor ~boundary
  | Token_kind.Keyword Keyword.If ->
      parse_if_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Lock ->
      parse_lock_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Static when Option.is_some cursor.local_context
    -> parse_local_declaration cursor ~boundary
  | Token_kind.Keyword (Keyword.Reg | Keyword.Noreg)
    when Option.is_some cursor.local_context ->
      local_declaration_failure cursor ~boundary item ~code:"HCPARSE0099"
        ~message:
          "a local register qualifier must follow its primitive type in the \
           pinned parser"
  | Token_kind.Keyword Keyword.Return -> parse_return_statement cursor ~boundary
  | Token_kind.Keyword Keyword.Switch ->
      parse_switch_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Try ->
      parse_try_catch_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.While ->
      parse_while_statement cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
  | Token_kind.Keyword Keyword.Catch ->
      report cursor item ~code:"HCPARSE0082"
        ~message:"found 'catch' without a matching 'try'";
      recover_statement cursor ~boundary;
      None
  | Token_kind.Keyword Keyword.Else ->
      report cursor item ~code:"HCPARSE0055"
        ~message:"found 'else' without a matching 'if'";
      recover_statement cursor ~boundary;
      None
  | Token_kind.String | Token_kind.Character ->
      parse_implicit_output_statement cursor ~boundary
  | Token_kind.Punctuation ';' when is_for_update_boundary boundary ->
      report cursor item ~code:"HCPARSE0070"
        ~message:"expected ')' after the for update, but found ';'";
      recover_statement cursor ~boundary;
      None
  | Token_kind.Punctuation ';' -> Some (parse_empty_statement cursor)
  | Token_kind.Identifier when token_starts_function_label cursor item.token ->
      parse_label_statement cursor
  | Token_kind.Identifier
    when Option.is_some cursor.local_context
         && Option.is_some (primitive_type_of_token item.token) ->
      parse_local_declaration cursor ~boundary
  | _ when token_starts_statement_expression cursor item.token ->
      parse_expression_statement cursor ~boundary
  | _ ->
      let code, message =
        match item.token.kind with
        | Token_kind.Punctuation '}' when boundary = Top_level_boundary ->
            ("HCPARSE0050", "found '}' without a matching '{'")
        | Token_kind.Punctuation '}' ->
            ( "HCPARSE0048",
              "expected another statement after ',', but found '}'" )
        | Token_kind.Identifier
          when not
                 (statement_symbol_is_expression cursor (token_text item.token))
          ->
            ( "HCPARSE0048",
              Printf.sprintf
                "label or declaration syntax for unresolved identifier %S \
                 after a statement comma is not implemented"
                (token_text item.token) )
        | _ when token_starts_global_declaration cursor item.token ->
            ( "HCPARSE0048",
              "a declaration after a statement comma is not implemented" )
        | _ ->
            ( "HCPARSE0048",
              Printf.sprintf
                "statement form beginning with %s is not implemented"
                (token_description item.token) )
      in
      report cursor item ~code ~message;
      recover_statement cursor ~boundary;
      None

and parse_do_while_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let do_item = peek cursor in
  if loop_depth >= max_loop_depth then (
    report cursor do_item ~code:"HCPARSE0061"
      ~message:
        (Printf.sprintf "loop-statement nesting exceeds the hosted limit of %d"
           max_loop_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let do_item = take cursor in
    match
      parse_required_statement cursor
        ~boundary:(statement_body_boundary boundary)
        ~block_depth ~conditional_depth ~loop_depth:(loop_depth + 1) ~lock_depth
        ~try_depth ~switch_depth ~code:"HCPARSE0062"
        ~description:"a statement after 'do'"
    with
    | None -> None
    | Some body -> (
        let while_item = peek cursor in
        if while_item.token.kind <> Token_kind.Keyword Keyword.While then (
          report cursor while_item ~code:"HCPARSE0063"
            ~message:
              (Printf.sprintf
                 "expected 'while' after the do-while body, but found %s"
                 (token_description while_item.token));
          recover_statement cursor ~boundary;
          None)
        else
          let while_item = take cursor in
          let opening_item = peek cursor in
          if opening_item.token.kind <> Token_kind.Punctuation '(' then (
            report cursor opening_item ~code:"HCPARSE0064"
              ~message:
                (Printf.sprintf
                   "expected '(' after the do-while keyword, but found %s"
                   (token_description opening_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let opening_item = take cursor in
            match
              parse_expression cursor ~context:Do_while_condition_expression
                ~depth:0 ~minimum_binding_power:0
            with
            | None ->
                recover_statement cursor ~boundary;
                None
            | Some (condition : parsed_expression) ->
                let closing_item = peek cursor in
                if closing_item.token.kind <> Token_kind.Punctuation ')' then (
                  report cursor closing_item ~code:"HCPARSE0065"
                    ~message:
                      (Printf.sprintf
                         "expected ')' after the do-while condition, but found \
                          %s"
                         (token_description closing_item.token));
                  recover_statement cursor ~boundary;
                  None)
                else
                  let closing_item = take cursor in
                  let semicolon_item = peek cursor in
                  if semicolon_item.token.kind <> Token_kind.Punctuation ';'
                  then (
                    report cursor semicolon_item ~code:"HCPARSE0066"
                      ~message:
                        (Printf.sprintf
                           "expected ';' after the do-while condition, but \
                            found %s"
                           (token_description semicolon_item.token));
                    recover_statement cursor ~boundary;
                    None)
                  else
                    let semicolon_item = take cursor in
                    let tokens =
                      (do_item.token :: body.tokens)
                      @ while_item.token :: opening_item.token
                        :: condition.tokens
                      @ [ closing_item.token; semicolon_item.token ]
                    in
                    let statement =
                      Ast.make_do_while_statement
                        ~do_keyword:(token_location do_item.token)
                        ~body:body.node
                        ~while_keyword:(token_location while_item.token)
                        ~opening_parenthesis:(token_location opening_item.token)
                        ~condition:condition.node
                        ~closing_parenthesis:(token_location closing_item.token)
                        ~semicolon:(token_location semicolon_item.token)
                        ~location:(location_from_expression_tokens tokens)
                    in
                    Some { node = Ast.Do_while_statement statement; tokens })

and parse_for_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if loop_depth >= max_loop_depth then (
    report cursor keyword_item ~code:"HCPARSE0061"
      ~message:
        (Printf.sprintf "loop-statement nesting exceeds the hosted limit of %d"
           max_loop_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let statement_boundary = statement_body_boundary boundary in
    let opening_item = peek cursor in
    if opening_item.token.kind <> Token_kind.Punctuation '(' then (
      report cursor opening_item ~code:"HCPARSE0067"
        ~message:
          (Printf.sprintf "expected '(' after 'for', but found %s"
             (token_description opening_item.token));
      recover_statement cursor ~boundary;
      None)
    else
      let opening_item = take cursor in
      let initialization_item = peek cursor in
      if initialization_item.token.kind = Token_kind.Punctuation ')' then (
        report cursor initialization_item ~code:"HCPARSE0068"
          ~message:
            "expected an initializer statement in the for header, but found ')'";
        recover_for_header cursor ~boundary:statement_boundary;
        None)
      else
        match
          parse_required_statement cursor ~boundary:statement_boundary
            ~block_depth ~conditional_depth ~loop_depth:(loop_depth + 1)
            ~lock_depth ~try_depth ~switch_depth ~code:"HCPARSE0068"
            ~description:"an initializer statement in the for header"
        with
        | None ->
            recover_for_header cursor ~boundary:statement_boundary;
            None
        | Some initialization -> (
            match
              parse_expression cursor ~context:For_condition_expression ~depth:0
                ~minimum_binding_power:0
            with
            | None ->
                recover_for_header cursor ~boundary:statement_boundary;
                None
            | Some (condition : parsed_expression) -> (
                let condition_semicolon_item = peek cursor in
                if
                  condition_semicolon_item.token.kind
                  <> Token_kind.Punctuation ';'
                then (
                  report cursor condition_semicolon_item ~code:"HCPARSE0069"
                    ~message:
                      (Printf.sprintf
                         "expected ';' after the for condition, but found %s"
                         (token_description condition_semicolon_item.token));
                  recover_for_header cursor ~boundary:statement_boundary;
                  None)
                else
                  let condition_semicolon_item = take cursor in
                  let update_item = peek cursor in
                  let parsed_update =
                    if update_item.token.kind = Token_kind.Punctuation ')' then
                      Some None
                    else
                      Option.map
                        (fun update -> Some update)
                        (parse_required_statement cursor
                           ~boundary:(For_update_boundary statement_boundary)
                           ~block_depth ~conditional_depth
                           ~loop_depth:(loop_depth + 1) ~lock_depth ~try_depth
                           ~switch_depth ~code:"HCPARSE0070"
                           ~description:"a for update statement")
                  in
                  match parsed_update with
                  | None ->
                      recover_for_header cursor ~boundary:statement_boundary;
                      None
                  | Some update -> (
                      let closing_item = peek cursor in
                      if closing_item.token.kind <> Token_kind.Punctuation ')'
                      then (
                        report cursor closing_item ~code:"HCPARSE0070"
                          ~message:
                            (Printf.sprintf
                               "expected ')' after the for update, but found %s"
                               (token_description closing_item.token));
                        recover_for_header cursor ~boundary:statement_boundary;
                        None)
                      else
                        let closing_item = take cursor in
                        match
                          parse_required_statement cursor
                            ~boundary:statement_boundary ~block_depth
                            ~conditional_depth ~loop_depth:(loop_depth + 1)
                            ~lock_depth ~try_depth ~switch_depth
                            ~code:"HCPARSE0071"
                            ~description:"a statement after the for header"
                        with
                        | None -> None
                        | Some body ->
                            let update_tokens =
                              match update with
                              | None -> []
                              | Some (update : parsed_statement) ->
                                  update.tokens
                            in
                            let tokens =
                              keyword_item.token :: opening_item.token
                              :: initialization.tokens
                              @ condition.tokens
                              @ (condition_semicolon_item.token :: update_tokens)
                              @ (closing_item.token :: body.tokens)
                            in
                            let statement =
                              Ast.make_for_statement
                                ~keyword:(token_location keyword_item.token)
                                ~opening_parenthesis:
                                  (token_location opening_item.token)
                                ~initialization:initialization.node
                                ~condition:condition.node
                                ~condition_semicolon:
                                  (token_location condition_semicolon_item.token)
                                ~update:
                                  (Option.map
                                     (fun (update : parsed_statement) ->
                                       update.node)
                                     update)
                                ~closing_parenthesis:
                                  (token_location closing_item.token)
                                ~body:body.node
                                ~location:
                                  (location_from_expression_tokens tokens)
                            in
                            Some { node = Ast.For_statement statement; tokens })
                ))

and parse_if_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if conditional_depth >= max_conditional_depth then (
    report cursor keyword_item ~code:"HCPARSE0057"
      ~message:
        (Printf.sprintf "if-statement nesting exceeds the hosted limit of %d"
           max_conditional_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let opening_item = peek cursor in
    if opening_item.token.kind <> Token_kind.Punctuation '(' then (
      report cursor opening_item ~code:"HCPARSE0052"
        ~message:
          (Printf.sprintf "expected '(' after 'if', but found %s"
             (token_description opening_item.token));
      recover_statement cursor ~boundary;
      None)
    else
      let opening_item = take cursor in
      match
        parse_expression cursor ~context:If_condition_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (condition : parsed_expression) -> (
          let closing_item = peek cursor in
          if closing_item.token.kind <> Token_kind.Punctuation ')' then (
            report cursor closing_item ~code:"HCPARSE0053"
              ~message:
                (Printf.sprintf
                   "expected ')' after the if condition, but found %s"
                   (token_description closing_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let closing_item = take cursor in
            let branch_depth = conditional_depth + 1 in
            match
              parse_required_statement cursor
                ~boundary:(statement_body_boundary boundary)
                ~block_depth ~conditional_depth:branch_depth ~loop_depth
                ~lock_depth ~try_depth ~switch_depth ~code:"HCPARSE0054"
                ~description:"a statement after the if condition"
            with
            | None -> None
            | Some then_branch -> (
                let else_clause =
                  let else_item = peek cursor in
                  match else_item.token.kind with
                  | Token_kind.Keyword Keyword.Else ->
                      let else_item = take cursor in
                      Option.map
                        (fun (else_branch : parsed_statement) ->
                          let tokens = else_item.token :: else_branch.tokens in
                          let node =
                            Ast.make_else_clause
                              ~keyword:(token_location else_item.token)
                              ~branch:else_branch.node
                              ~location:(location_from_expression_tokens tokens)
                          in
                          (Some node, tokens))
                        (parse_required_statement cursor
                           ~boundary:(statement_body_boundary boundary)
                           ~block_depth ~conditional_depth:branch_depth
                           ~loop_depth ~lock_depth ~try_depth ~switch_depth
                           ~code:"HCPARSE0056"
                           ~description:"a statement after 'else'")
                  | _ -> Some (None, [])
                in
                match else_clause with
                | None -> None
                | Some (else_clause, else_tokens) ->
                    let tokens =
                      keyword_item.token :: opening_item.token
                      :: condition.tokens
                      @ (closing_item.token :: then_branch.tokens)
                      @ else_tokens
                    in
                    let statement =
                      Ast.make_if_statement
                        ~keyword:(token_location keyword_item.token)
                        ~opening_parenthesis:(token_location opening_item.token)
                        ~condition:condition.node
                        ~closing_parenthesis:(token_location closing_item.token)
                        ~then_branch:then_branch.node ~else_clause
                        ~location:(location_from_expression_tokens tokens)
                    in
                    Some { node = Ast.If_statement statement; tokens }))

and parse_lock_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if lock_depth >= max_lock_depth then (
    report cursor keyword_item ~code:"HCPARSE0078"
      ~message:
        (Printf.sprintf "lock-statement nesting exceeds the hosted limit of %d"
           max_lock_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    match
      parse_required_statement cursor
        ~boundary:(statement_body_boundary boundary)
        ~block_depth ~conditional_depth ~loop_depth ~lock_depth:(lock_depth + 1)
        ~try_depth ~switch_depth ~code:"HCPARSE0077"
        ~description:"a statement after 'lock'"
    with
    | None -> None
    | Some body ->
        let tokens = keyword_item.token :: body.tokens in
        let statement =
          Ast.make_lock_statement
            ~keyword:(token_location keyword_item.token)
            ~body:body.node
            ~location:(location_from_expression_tokens tokens)
        in
        Some { node = Ast.Lock_statement statement; tokens }

and parse_switch_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if switch_depth >= max_switch_depth then (
    report cursor keyword_item ~code:"HCPARSE0084"
      ~message:
        (Printf.sprintf
           "switch-statement nesting exceeds the hosted limit of %d"
           max_switch_depth);
    ignore (take cursor);
    recover_switch_tail cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let opening_item = peek cursor in
    let delimiter =
      match opening_item.token.kind with
      | Token_kind.Punctuation '(' ->
          Some (Ast.Bounded_switch, Token_kind.Punctuation ')')
      | Token_kind.Punctuation '[' ->
          Some (Ast.No_bound_switch, Token_kind.Punctuation ']')
      | _ -> None
    in
    match delimiter with
    | None ->
        report cursor opening_item ~code:"HCPARSE0085"
          ~message:
            (Printf.sprintf "expected '(' or '[' after 'switch', but found %s"
               (token_description opening_item.token));
        recover_switch_tail cursor ~boundary;
        None
    | Some (mode, closing_kind) -> (
        let opening_item = take cursor in
        match
          parse_expression cursor ~context:Switch_expression ~depth:0
            ~minimum_binding_power:0
        with
        | None ->
            recover_switch_tail cursor ~boundary;
            None
        | Some (expression : parsed_expression) -> (
            let closing_item = peek cursor in
            if closing_item.token.kind <> closing_kind then (
              report cursor closing_item ~code:"HCPARSE0086"
                ~message:
                  (Printf.sprintf
                     "expected %S after the switch expression, but found %s"
                     (match mode with
                     | Ast.Bounded_switch -> ")"
                     | Ast.No_bound_switch -> "]")
                     (token_description closing_item.token));
              recover_switch_tail cursor ~boundary;
              None)
            else
              let closing_item = take cursor in
              let opening_brace_item = peek cursor in
              if opening_brace_item.token.kind <> Token_kind.Punctuation '{'
              then (
                report cursor opening_brace_item ~code:"HCPARSE0087"
                  ~message:
                    (Printf.sprintf
                       "expected '{' after the switch header, but found %s"
                       (token_description opening_brace_item.token));
                recover_switch_tail cursor ~boundary;
                None)
              else
                let opening_brace_item = take cursor in
                match
                  parse_switch_region cursor ~expect_end:false
                    ~subswitch_depth:0 ~block_depth ~conditional_depth
                    ~loop_depth ~lock_depth ~try_depth
                    ~switch_depth:(switch_depth + 1)
                with
                | None -> None
                | Some region -> (
                    match region.region_end with
                    | Switch_region_end_label _ ->
                        invalid_arg
                          "switch body ended with a sub-switch terminator"
                    | Switch_region_brace closing_brace_item ->
                        if region.region_had_error then None
                        else
                          let tokens =
                            keyword_item.token :: opening_item.token
                            :: expression.tokens
                            @ closing_item.token :: opening_brace_item.token
                              :: region.region_tokens
                          in
                          let statement =
                            Ast.make_switch_statement
                              ~keyword:(token_location keyword_item.token)
                              ~mode
                              ~opening_delimiter:
                                (token_location opening_item.token)
                              ~expression:expression.node
                              ~closing_delimiter:
                                (token_location closing_item.token)
                              ~opening_brace:
                                (token_location opening_brace_item.token)
                              ~elements:region.region_elements
                              ~closing_brace:
                                (token_location closing_brace_item.token)
                              ~location:(location_from_expression_tokens tokens)
                          in
                          Some { node = Ast.Switch_statement statement; tokens }
                    )))

and parse_switch_region cursor ~expect_end ~subswitch_depth ~block_depth
    ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth :
    parsed_switch_region option =
  let finish elements_rev tokens_rev end_ ending_tokens had_error =
    Some
      {
        region_elements = List.rev elements_rev;
        region_tokens = List.rev tokens_rev @ ending_tokens;
        region_end = end_;
        region_had_error = had_error;
      }
  in
  let rec collect elements_rev tokens_rev had_error =
    let item = peek cursor in
    match item.token.kind with
    | Token_kind.Punctuation '}' when expect_end ->
        report cursor item ~code:"HCPARSE0095"
          ~message:"expected 'end:' before the enclosing switch closes";
        None
    | Token_kind.Punctuation '}' ->
        let closing_item = take cursor in
        finish elements_rev tokens_rev (Switch_region_brace closing_item)
          [ closing_item.token ] had_error
    | Token_kind.Eof ->
        report cursor item
          ~code:(if expect_end then "HCPARSE0095" else "HCPARSE0088")
          ~message:
            (if expect_end then "expected 'end:' before the end of input"
             else "expected '}' to close the switch statement");
        None
    | Token_kind.Keyword Keyword.End when expect_end ->
        let end_item = take cursor in
        let colon_item = peek cursor in
        if colon_item.token.kind <> Token_kind.Punctuation ':' then (
          report cursor colon_item ~code:"HCPARSE0096"
            ~message:
              (Printf.sprintf "expected ':' after 'end', but found %s"
                 (token_description colon_item.token));
          recover_statement cursor ~boundary:Switch_boundary;
          None)
        else
          let colon_item = take cursor in
          finish elements_rev tokens_rev
            (Switch_region_end_label (end_item, colon_item))
            [ end_item.token; colon_item.token ]
            had_error
    | Token_kind.Keyword Keyword.End ->
        let end_item = take cursor in
        report cursor end_item ~code:"HCPARSE0094"
          ~message:"found 'end:' without a matching 'start:'";
        let consumed =
          let colon_item = peek cursor in
          if colon_item.token.kind = Token_kind.Punctuation ':' then
            [ end_item.token; (take cursor).token ]
          else [ end_item.token ]
        in
        collect elements_rev (List.rev_append consumed tokens_rev) true
    | Token_kind.Keyword Keyword.Case -> (
        match parse_switch_case_element cursor with
        | Some element ->
            collect
              (element.node :: elements_rev)
              (List.rev_append element.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
    | Token_kind.Keyword Keyword.Default -> (
        match parse_switch_default_element cursor with
        | Some element ->
            collect
              (element.node :: elements_rev)
              (List.rev_append element.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
    | Token_kind.Keyword Keyword.Start -> (
        match
          parse_switch_subswitch_element cursor ~subswitch_depth ~block_depth
            ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth
        with
        | Some element ->
            collect
              (element.node :: elements_rev)
              (List.rev_append element.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
    | _ -> (
        match
          parse_statement_sequence cursor ~boundary:Switch_boundary ~block_depth
            ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth
        with
        | Some statement ->
            let element = Ast.Switch_statement_element statement.node in
            collect (element :: elements_rev)
              (List.rev_append statement.tokens tokens_rev)
              had_error
        | None -> collect elements_rev tokens_rev true)
  in
  collect [] [] false

and parse_switch_case_element cursor : parsed_switch_element option =
  let keyword_item = take cursor in
  let first_item = peek cursor in
  let parsed_pattern =
    if first_item.token.kind = Token_kind.Punctuation ':' then
      Some (Ast.Implicit_case, [])
    else
      match
        parse_expression cursor ~context:Switch_case_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None -> None
      | Some (start_expression : parsed_expression) -> (
          let ellipsis_item = peek cursor in
          if ellipsis_item.token.kind <> Token_kind.Operator Operator.Ellipsis
          then
            Some (Ast.Single_case start_expression.node, start_expression.tokens)
          else
            let ellipsis_item = take cursor in
            let end_item = peek cursor in
            if
              end_item.token.kind = Token_kind.Punctuation ':'
              || end_item.token.kind = Token_kind.Punctuation '}'
              || end_item.token.kind = Token_kind.Eof
              || token_is_switch_boundary end_item.token
            then (
              report cursor end_item ~code:"HCPARSE0090"
                ~message:
                  (Printf.sprintf
                     "expected an expression after the case range ellipsis, \
                      but found %s"
                     (token_description end_item.token));
              None)
            else
              match
                parse_expression cursor ~context:Switch_case_expression ~depth:0
                  ~minimum_binding_power:0
              with
              | None -> None
              | Some (end_expression : parsed_expression) ->
                  let range_tokens =
                    start_expression.tokens
                    @ (ellipsis_item.token :: end_expression.tokens)
                  in
                  let range =
                    Ast.make_switch_case_range ~start:start_expression.node
                      ~ellipsis:(token_location ellipsis_item.token)
                      ~end_:end_expression.node
                      ~location:(location_from_expression_tokens range_tokens)
                  in
                  Some (Ast.Ranged_case range, range_tokens))
  in
  match parsed_pattern with
  | None ->
      recover_statement cursor ~boundary:Switch_boundary;
      None
  | Some (pattern, pattern_tokens) ->
      let colon_item = peek cursor in
      if colon_item.token.kind <> Token_kind.Punctuation ':' then (
        report cursor colon_item ~code:"HCPARSE0091"
          ~message:
            (Printf.sprintf "expected ':' after the case label, but found %s"
               (token_description colon_item.token));
        recover_statement cursor ~boundary:Switch_boundary;
        None)
      else
        let colon_item = take cursor in
        let tokens =
          (keyword_item.token :: pattern_tokens) @ [ colon_item.token ]
        in
        let label =
          Ast.make_switch_case_label
            ~keyword:(token_location keyword_item.token)
            ~pattern
            ~colon:(token_location colon_item.token)
            ~location:(location_from_expression_tokens tokens)
        in
        Some { node = Ast.Switch_case_element label; tokens }

and parse_switch_default_element cursor : parsed_switch_element option =
  let keyword_item = take cursor in
  let colon_item = peek cursor in
  if colon_item.token.kind <> Token_kind.Punctuation ':' then (
    report cursor colon_item ~code:"HCPARSE0092"
      ~message:
        (Printf.sprintf "expected ':' after 'default', but found %s"
           (token_description colon_item.token));
    recover_statement cursor ~boundary:Switch_boundary;
    None)
  else
    let colon_item = take cursor in
    let tokens = [ keyword_item.token; colon_item.token ] in
    let label =
      Ast.make_switch_default_label
        ~keyword:(token_location keyword_item.token)
        ~colon:(token_location colon_item.token)
        ~location:(location_from_expression_tokens tokens)
    in
    Some { node = Ast.Switch_default_element label; tokens }

and parse_switch_subswitch_element cursor ~subswitch_depth ~block_depth
    ~conditional_depth ~loop_depth ~lock_depth ~try_depth ~switch_depth :
    parsed_switch_element option =
  let start_item = peek cursor in
  if subswitch_depth >= max_switch_depth then (
    report cursor start_item ~code:"HCPARSE0097"
      ~message:
        (Printf.sprintf "sub-switch nesting exceeds the hosted limit of %d"
           max_switch_depth);
    ignore (take cursor);
    let rec skip nested =
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Eof | Token_kind.Punctuation '}' -> ()
      | Token_kind.Keyword Keyword.Start ->
          ignore (take cursor);
          skip (nested + 1)
      | Token_kind.Keyword Keyword.End ->
          ignore (take cursor);
          if (peek cursor).token.kind = Token_kind.Punctuation ':' then
            ignore (take cursor);
          if nested > 0 then skip (nested - 1)
      | _ ->
          ignore (take cursor);
          skip nested
    in
    skip 0;
    None)
  else
    let start_item = take cursor in
    let start_colon_item = peek cursor in
    if start_colon_item.token.kind <> Token_kind.Punctuation ':' then (
      report cursor start_colon_item ~code:"HCPARSE0093"
        ~message:
          (Printf.sprintf "expected ':' after 'start', but found %s"
             (token_description start_colon_item.token));
      recover_statement cursor ~boundary:Switch_boundary;
      None)
    else
      let start_colon_item = take cursor in
      match
        parse_switch_region cursor ~expect_end:true
          ~subswitch_depth:(subswitch_depth + 1) ~block_depth ~conditional_depth
          ~loop_depth ~lock_depth ~try_depth ~switch_depth
      with
      | None -> None
      | Some region -> (
          match region.region_end with
          | Switch_region_brace _ ->
              invalid_arg "sub-switch ended with a switch-body brace"
          | Switch_region_end_label (end_item, end_colon_item) ->
              if region.region_had_error then None
              else
                let tokens =
                  start_item.token :: start_colon_item.token
                  :: region.region_tokens
                in
                let subswitch =
                  Ast.make_switch_subswitch
                    ~start_keyword:(token_location start_item.token)
                    ~start_colon:(token_location start_colon_item.token)
                    ~elements:region.region_elements
                    ~end_keyword:(token_location end_item.token)
                    ~end_colon:(token_location end_colon_item.token)
                    ~location:(location_from_expression_tokens tokens)
                in
                Some { node = Ast.Switch_subswitch_element subswitch; tokens })

and parse_try_catch_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let try_item = peek cursor in
  if try_depth >= max_try_depth then (
    report cursor try_item ~code:"HCPARSE0083"
      ~message:
        (Printf.sprintf "try-statement nesting exceeds the hosted limit of %d"
           max_try_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let try_item = take cursor in
    let body_boundary = statement_body_boundary boundary in
    let body_item = peek cursor in
    if body_item.token.kind = Token_kind.Keyword Keyword.Catch then (
      report cursor body_item ~code:"HCPARSE0079"
        ~message:"expected a statement after 'try', but found \"catch\"";
      recover_statement cursor ~boundary;
      None)
    else
      match
        parse_required_statement cursor ~boundary:body_boundary ~block_depth
          ~conditional_depth ~loop_depth ~lock_depth ~try_depth:(try_depth + 1)
          ~switch_depth ~code:"HCPARSE0079"
          ~description:"a statement after 'try'"
      with
      | None -> None
      | Some try_body -> (
          let catch_item = peek cursor in
          if catch_item.token.kind <> Token_kind.Keyword Keyword.Catch then (
            report cursor catch_item ~code:"HCPARSE0080"
              ~message:
                (Printf.sprintf
                   "expected 'catch' after the try body, but found %s"
                   (token_description catch_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let catch_item = take cursor in
            let handler_item = peek cursor in
            if handler_item.token.kind = Token_kind.Keyword Keyword.Catch then (
              report cursor handler_item ~code:"HCPARSE0081"
                ~message:
                  "expected a statement after 'catch', but found \"catch\"";
              recover_statement cursor ~boundary;
              None)
            else
              match
                parse_required_statement cursor ~boundary:body_boundary
                  ~block_depth ~conditional_depth ~loop_depth ~lock_depth
                  ~try_depth:(try_depth + 1) ~switch_depth ~code:"HCPARSE0081"
                  ~description:"a statement after 'catch'"
              with
              | None -> None
              | Some catch_body ->
                  let tokens =
                    (try_item.token :: try_body.tokens)
                    @ (catch_item.token :: catch_body.tokens)
                  in
                  let statement =
                    Ast.make_try_catch_statement
                      ~try_keyword:(token_location try_item.token)
                      ~try_body:try_body.node
                      ~catch_keyword:(token_location catch_item.token)
                      ~catch_body:catch_body.node
                      ~location:(location_from_expression_tokens tokens)
                  in
                  Some { node = Ast.Try_catch_statement statement; tokens })

and parse_while_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let keyword_item = peek cursor in
  if loop_depth >= max_loop_depth then (
    report cursor keyword_item ~code:"HCPARSE0061"
      ~message:
        (Printf.sprintf "loop-statement nesting exceeds the hosted limit of %d"
           max_loop_depth);
    recover_statement cursor ~boundary;
    None)
  else
    let keyword_item = take cursor in
    let opening_item = peek cursor in
    if opening_item.token.kind <> Token_kind.Punctuation '(' then (
      report cursor opening_item ~code:"HCPARSE0058"
        ~message:
          (Printf.sprintf "expected '(' after 'while', but found %s"
             (token_description opening_item.token));
      recover_statement cursor ~boundary;
      None)
    else
      let opening_item = take cursor in
      match
        parse_expression cursor ~context:While_condition_expression ~depth:0
          ~minimum_binding_power:0
      with
      | None ->
          recover_statement cursor ~boundary;
          None
      | Some (condition : parsed_expression) -> (
          let closing_item = peek cursor in
          if closing_item.token.kind <> Token_kind.Punctuation ')' then (
            report cursor closing_item ~code:"HCPARSE0059"
              ~message:
                (Printf.sprintf
                   "expected ')' after the while condition, but found %s"
                   (token_description closing_item.token));
            recover_statement cursor ~boundary;
            None)
          else
            let closing_item = take cursor in
            match
              parse_required_statement cursor
                ~boundary:(statement_body_boundary boundary)
                ~block_depth ~conditional_depth ~loop_depth:(loop_depth + 1)
                ~lock_depth ~try_depth ~switch_depth ~code:"HCPARSE0060"
                ~description:"a statement after the while condition"
            with
            | None -> None
            | Some body ->
                let tokens =
                  (keyword_item.token :: opening_item.token :: condition.tokens)
                  @ (closing_item.token :: body.tokens)
                in
                let statement =
                  Ast.make_while_statement
                    ~keyword:(token_location keyword_item.token)
                    ~opening_parenthesis:(token_location opening_item.token)
                    ~condition:condition.node
                    ~closing_parenthesis:(token_location closing_item.token)
                    ~body:body.node
                    ~location:(location_from_expression_tokens tokens)
                in
                Some { node = Ast.While_statement statement; tokens })

and parse_required_statement cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth ~code ~description :
    parsed_statement option =
  let first_item = peek cursor in
  let missing =
    match first_item.token.kind with
    | Token_kind.Eof
    | Token_kind.Punctuation '}'
    | Token_kind.Keyword Keyword.Else -> true
    | _
      when statement_boundary_is_switch boundary
           && token_is_switch_boundary first_item.token -> true
    | _ -> false
  in
  if missing then (
    report cursor first_item ~code
      ~message:
        (Printf.sprintf "expected %s, but found %s" description
           (token_description first_item.token));
    recover_statement cursor ~boundary;
    None)
  else
    match
      parse_statement_sequence cursor ~boundary ~block_depth ~conditional_depth
        ~loop_depth ~lock_depth ~try_depth ~switch_depth
    with
    | Some ({ node = Ast.Sequence_statement sequence; _ } : parsed_statement)
      when sequence.sequence_elements = [] ->
        report cursor first_item ~code
          ~message:
            (Printf.sprintf "expected %s, but found only statement commas"
               description);
        recover_statement cursor ~boundary;
        None
    | statement -> statement

and parse_block_statement cursor ~block_depth ~conditional_depth ~loop_depth
    ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let opening_item = peek cursor in
  if block_depth >= max_block_depth then (
    report cursor opening_item ~code:"HCPARSE0051"
      ~message:
        (Printf.sprintf
           "compound-statement nesting exceeds the hosted limit of %d"
           max_block_depth);
    recover_compound_statement cursor;
    None)
  else
    let opening_item = take cursor in
    let rec collect statements_rev tokens_rev had_error :
        parsed_statement option =
      let item = peek cursor in
      match item.token.kind with
      | Token_kind.Punctuation '}' ->
          let closing_item = take cursor in
          if had_error then None
          else
            let statements = List.rev statements_rev in
            let tokens =
              (opening_item.token :: List.rev tokens_rev)
              @ [ closing_item.token ]
            in
            let statement =
              Ast.make_block_statement
                ~opening_brace:(token_location opening_item.token)
                ~statements
                ~closing_brace:(token_location closing_item.token)
                ~location:(location_from_expression_tokens tokens)
            in
            Some { node = Ast.Block_statement statement; tokens }
      | Token_kind.Eof ->
          let secondary =
            [
              ({
                 Common.Diagnostic.span = opening_item.token.span;
                 message = "block starts here";
               }
                : Common.Diagnostic.related);
            ]
          in
          report ~secondary cursor item ~code:"HCPARSE0049"
            ~message:"expected '}' to close the compound statement";
          None
      | _ -> (
          match
            parse_statement_sequence cursor ~boundary:Block_boundary
              ~block_depth:(block_depth + 1) ~conditional_depth ~loop_depth
              ~lock_depth ~try_depth ~switch_depth
          with
          | Some statement ->
              collect
                (statement.node :: statements_rev)
                (List.rev_append statement.tokens tokens_rev)
                had_error
          | None -> collect statements_rev tokens_rev true)
    in
    collect [] [] false

and parse_statement_sequence cursor ~boundary ~block_depth ~conditional_depth
    ~loop_depth ~lock_depth ~try_depth ~switch_depth : parsed_statement option =
  let leading_items = take_statement_commas cursor [] in
  let leading_commas =
    List.map (fun item -> token_location item.token) leading_items
  in
  let leading_tokens = List.map (fun item -> item.token) leading_items in
  let rec collect elements_rev tokens_rev =
    let item = peek cursor in
    if item.token.kind = Token_kind.Eof then
      Some (List.rev elements_rev, List.rev tokens_rev)
    else
      match
        parse_statement_atom cursor ~boundary ~block_depth ~conditional_depth
          ~loop_depth ~lock_depth ~try_depth ~switch_depth
      with
      | None -> None
      | Some (statement : parsed_statement) ->
          let comma_items = take_statement_commas cursor [] in
          let following_commas =
            List.map (fun item -> token_location item.token) comma_items
          in
          let comma_tokens = List.map (fun item -> item.token) comma_items in
          let element_tokens = statement.tokens @ comma_tokens in
          let element =
            Ast.make_statement_sequence_element ~statement:statement.node
              ~following_commas
              ~location:(location_from_expression_tokens element_tokens)
          in
          let elements_rev = element :: elements_rev in
          let tokens_rev = List.rev_append element_tokens tokens_rev in
          let local_continues =
            match statement.node with
            | Ast.Local_declaration_statement _ -> true
            | _ -> false
          in
          let reaches_block_close =
            match (peek cursor).token.kind with
            | Token_kind.Punctuation '}' -> true
            | _ -> false
          in
          if comma_items = [] && ((not local_continues) || reaches_block_close)
          then Some (List.rev elements_rev, List.rev tokens_rev)
          else collect elements_rev tokens_rev
  in
  let next_item = peek cursor in
  if leading_items <> [] && next_item.token.kind = Token_kind.Eof then
    let location = location_from_expression_tokens leading_tokens in
    Some
      {
        node =
          Ast.Sequence_statement
            (Ast.make_statement_sequence ~leading_commas ~elements:[] ~location);
        tokens = leading_tokens;
      }
  else
    match collect [] (List.rev leading_tokens) with
    | None -> None
    | Some (elements, tokens) -> (
        let has_following_commas =
          List.exists
            (fun (element : Ast.statement_sequence_element) ->
              element.sequence_following_commas <> [])
            elements
        in
        match (leading_items, has_following_commas, elements) with
        | [], false, [ element ] ->
            Some { node = element.sequence_statement; tokens }
        | _ ->
            let location = location_from_expression_tokens tokens in
            Some
              {
                node =
                  Ast.Sequence_statement
                    (Ast.make_statement_sequence ~leading_commas ~elements
                       ~location);
                tokens;
              })

let parse_function_definition cursor ~modifier_tokens ~modifiers ~type_item
    ~return_type (prefix : parsed_declarator_prefix) =
  let opening = take cursor in
  match
    parse_function_parameters cursor [] [] ~after_comma:false
      ~function_pointer_depth:0
  with
  | None -> None
  | Some parsed_parameters ->
      let header_tokens =
        modifier_tokens
        @ (type_item.token :: prefix.tokens)
        @ (opening.token :: parsed_parameters.tokens)
      in
      publish_function cursor prefix.name;
      let parsed_body =
        with_function_local_context cursor parsed_parameters.parameters
          parsed_parameters.variadic (fun () ->
            let body_item = peek cursor in
            match body_item.token.kind with
            | Token_kind.Eof -> Some (None, [])
            | _ ->
                parse_statement_sequence cursor ~boundary:Top_level_boundary
                  ~block_depth:0 ~conditional_depth:0 ~loop_depth:0
                  ~lock_depth:0 ~try_depth:0 ~switch_depth:0
                |> Option.map (fun (body : parsed_statement) ->
                    (Some body.node, body.tokens)))
      in
      Option.map
        (fun (body, body_tokens) ->
          let definition_tokens = header_tokens @ body_tokens in
          let definition =
            Ast.make_function_definition ~modifiers ~return_type
              ~return_pointer_layers:prefix.pointer_layers ~name:prefix.name
              ~opening_parenthesis:(token_location opening.token)
              ~parameters:parsed_parameters.parameters
              ~variadic:parsed_parameters.variadic
              ~closing_parenthesis:parsed_parameters.closing_parenthesis ~body
              ~location:(location_from_expression_tokens definition_tokens)
          in
          Ast.Function_definition definition)
        parsed_body

let parse ~sources ~definitions ~symbols ~config source =
  let stream =
    Preprocessor.create ~sources ~definitions ~symbols ~config source
  in
  let cursor =
    {
      stream;
      symbols;
      compilation_mode = Preprocessor.Config.compilation_mode config;
      lookahead = [];
      diagnostics_rev = [];
      local_context = None;
    }
  in
  let items_rev = ref [] in
  let finished = ref false in
  while not !finished do
    let item = peek cursor in
    match item.token.Token.kind with
    | Token_kind.Eof ->
        ignore (take cursor);
        finished := true
    | Token_kind.Identifier when token_starts_function_label cursor item.token
      -> (
        match
          parse_statement_sequence cursor ~boundary:Top_level_boundary
            ~block_depth:0 ~conditional_depth:0 ~loop_depth:0 ~lock_depth:0
            ~try_depth:0 ~switch_depth:0
        with
        | Some statement ->
            items_rev := Ast.Top_level_statement statement.node :: !items_rev
        | None -> ())
    | _ when token_starts_global_declaration cursor item.token -> (
        match parse_global cursor ~parse_function_definition with
        | Some item -> items_rev := item :: !items_rev
        | None -> ())
    | _ -> (
        match
          parse_statement_sequence cursor ~boundary:Top_level_boundary
            ~block_depth:0 ~conditional_depth:0 ~loop_depth:0 ~lock_depth:0
            ~try_depth:0 ~switch_depth:0
        with
        | Some statement ->
            items_rev := Ast.Top_level_statement statement.node :: !items_rev
        | None -> ())
  done;
  let diagnostics = List.rev cursor.diagnostics_rev in
  let ast =
    if has_error diagnostics then None
    else
      let span =
        Common.Span.unsafe_make
          ~source:(Common.Source_file.id source)
          ~start:0
          ~stop:(Common.Source_file.length source)
      in
      Some
        (Ast.make_module
           ~source:(Common.Source_file.id source)
           ~span ~items:(List.rev !items_rev))
  in
  { ast; diagnostics }
