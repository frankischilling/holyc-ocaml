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
  mutable lookahead : located_token option;
  mutable diagnostics_rev : Common.Diagnostic.t list;
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

let peek cursor =
  match cursor.lookahead with
  | Some item -> item
  | None ->
      let item = pull cursor in
      cursor.lookahead <- Some item;
      item

let take cursor =
  let item = peek cursor in
  cursor.lookahead <- None;
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

let declaration_failure cursor item ~code ~message =
  report cursor item ~code ~message;
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

let expression_failure cursor item ~code ~message =
  declaration_failure cursor item ~code ~message

let expression_context_name = function
  | Default_expression -> "default expression"
  | Array_dimension_expression -> "array dimension expression"
  | Intern_binding_expression -> "_intern target expression"
  | Call_argument_expression -> "call argument expression"
  | Index_expression -> "index expression"

let expression_operand_name = function
  | Default_expression -> "a default expression operand"
  | Array_dimension_expression -> "an array dimension expression operand"
  | Intern_binding_expression -> "an _intern target expression operand"
  | Call_argument_expression -> "a call argument expression operand"
  | Index_expression -> "an index expression operand"

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
  | Token_kind.Punctuation '(', _ -> (
      let opening = take cursor in
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
            Some { node; tokens })
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

and parse_call_suffix cursor ~context ~depth (callee : parsed_expression) :
    parsed_expression option =
  let opening = take cursor in
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

and parse_expression_tail cursor ~context ~depth ~minimum_binding_power
    (left : parsed_expression) : parsed_expression option =
  let item = peek cursor in
  match item.token.kind with
  | Token_kind.Punctuation '(' -> (
      match parse_call_suffix cursor ~context ~depth left with
      | None -> None
      | Some call ->
          parse_expression_tail cursor ~context ~depth ~minimum_binding_power
            call)
  | Token_kind.Punctuation '[' -> (
      match parse_index_suffix cursor ~context ~depth left with
      | None -> None
      | Some index ->
          parse_expression_tail cursor ~context ~depth ~minimum_binding_power
            index)
  | _ -> (
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
              parse_expression_tail cursor ~context ~depth
                ~minimum_binding_power left)
      | _ -> Some left)

let parse_parameter_default cursor =
  let equals = take cursor in
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
          ~value:expression.node
          ~location:(location_from_expression_tokens tokens)
      in
      Some ({ node; tokens } : parsed_parameter_default)

let is_unimplemented_expression_continuation token =
  match token.Token.kind with
  | Token_kind.Punctuation '.'
  | Token_kind.Operator
      (Operator.Arrow | Operator.Increment | Operator.Decrement) -> true
  | _ -> false

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
          let following = peek cursor in
          if is_unimplemented_expression_continuation following.token then (
            ignore
              (expression_failure cursor following ~code:"HCPARSE0020"
                 ~message:
                   (Printf.sprintf
                      "_intern target expression continuation %s is not \
                       implemented"
                      (token_description following.token)));
            Bad_binding)
          else
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
        if is_unimplemented_expression_continuation closing.token then
          expression_failure cursor closing ~code:"HCPARSE0020"
            ~message:
              (Printf.sprintf
                 "array dimension expression continuation %s is not implemented"
                 (token_description closing.token))
        else if closing.token.kind <> Token_kind.Punctuation ']' then
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
        | Token_kind.Punctuation ('[' | '.')
        | Token_kind.Operator
            (Operator.Arrow | Operator.Increment | Operator.Decrement)
          when Option.is_some parsed_default ->
            declaration_failure cursor following_item ~code:"HCPARSE0020"
              ~message:
                (Printf.sprintf
                   "default expression continuation %s is not implemented"
                   (token_description following_item.token))
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

let parse_global cursor =
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
                  report ~secondary:first_prefix.definition_trace cursor
                    next_item ~code:"HCPARSE0008"
                    ~message:
                      (Printf.sprintf
                         "function %S has no declaration binding; function \
                          bodies are not implemented"
                         first_prefix.name.spelling);
                  recover_declaration cursor;
                  None
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

let parse ~sources ~definitions ~symbols ~config source =
  let stream =
    Preprocessor.create ~sources ~definitions ~symbols ~config source
  in
  let cursor =
    {
      stream;
      symbols;
      compilation_mode = Preprocessor.Config.compilation_mode config;
      lookahead = None;
      diagnostics_rev = [];
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
    | _ -> (
        match parse_global cursor with
        | Some item -> items_rev := item :: !items_rev
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
