module Frame = Sema.Function_frame_layout
module Binding = Sema.Function_binding_index
module Headers = Sema.Function_type_resolution
module Local = Sema.Local_type_resolution
module Typed = Sema.Function_call_expression_result
module Source = Sema.Function_call_resolution
module Seq = Instruction_sequence
module Values = Map.Make (Seq.Value_id)
module O = Opcode
module Ast = Frontend.Ast
module Request = Sema.Register_request

(* PrsLib.HC:257-263 runs OptPass012 twice before member scoring. In this
   admitted integer grammar, OptLib.HC:172-177,196-225 folds only immediate
   children. OptPass012.HC:308-432,485-658 preserves the variable subtree in
   identity/power-of-two arithmetic rewrites (multiply by zero is retained),
   :435-484,725-816 folds bitwise/comparison nodes only when both are immediate,
   and :828-895 preserves each direct assignment/update target. Pointer-update,
   chained-comparison, control-flow and call rewrites are deliberately excluded.
   Source occurrences therefore survive to OptPass3 one for one; hosted loads
   alone are never used to infer the TempleOS score. *)

exception Unproved

let require condition = if not condition then raise Unproved

let get = function
  | Some value -> value
  | None -> raise Unproved

let source_span = function
  | Sema.Symbol.Source_location location -> location.span
  | _ -> raise Unproved

let integer_type type_ =
  Sema.Type.pointer_depth type_ = 0
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive
      (_, (Sema.Primitive_type.I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64)) ->
      true
  | _ -> false

let descriptions block =
  Block_graph.instructions block |> Seq.instructions |> List.map Seq.description

type access_kind = Read | Write

type access = {
  location : Frame.location;
  span : Common.Span.t;
  kind : access_kind;
}

type root =
  | Initializer of Typed.initializer_result
  | Statement of Typed.expression_statement_result
  | Return of Typed.return_result

let root_value = function
  | Initializer root -> Typed.initializer_value root
  | Statement root -> Typed.expression_statement_value root
  | Return root -> get (Typed.return_value root)

let root_span root = root_value root |> Typed.result_origin |> source_span

let same_list f left right =
  List.length left = List.length right && List.for_all2 f left right

type source_stamp = {
  definition : Ast.function_definition;
  statements : (Ast.statement * (int * int * int)) list;
}

let source_statement_list definition =
  match definition.Ast.body with
  | Some (Ast.Block_statement block as outer) -> outer :: block.block_statements
  | Some statement -> [ statement ]
  | None -> raise Unproved

let stamp_source body =
  try
    let definition =
      get
        (Option.bind
           (Function_body.definition_source body)
           Typed.function_source_body)
    in
    let statements =
      List.map
        (fun statement ->
          let span = (Ast.statement_location statement).span in
          ( statement,
            (Common.Source_id.to_int span.source, span.start, span.stop) ))
        (source_statement_list definition)
    in
    Some { definition; statements }
  with Unproved -> None

let equal_source_stamp left right =
  left.definition == right.definition
  && same_list
       (fun (statement, span) (other, other_span) ->
         statement == other && span = other_span)
       left.statements right.statements

let permits_automatic_analysis body =
  let rec statement = function
    | Ast.Try_catch_statement _ -> false
    | Block_statement block -> List.for_all statement block.block_statements
    | Sequence_statement sequence ->
        List.for_all
          (fun element -> statement element.Ast.sequence_statement)
          sequence.sequence_elements
    | If_statement conditional ->
        statement conditional.if_then_branch
        && Option.fold ~none:true
             ~some:(fun branch -> statement branch.Ast.else_branch)
             conditional.if_else_clause
    | While_statement loop -> statement loop.while_body
    | Do_while_statement loop -> statement loop.do_body
    | For_statement loop ->
        statement loop.for_initializer
        && Option.fold ~none:true ~some:statement loop.for_update
        && statement loop.for_body
    | Lock_statement locked -> statement locked.lock_body
    | Switch_statement switch -> List.for_all element switch.switch_elements
    | _ -> true
  and element = function
    | Ast.Switch_statement_element source -> statement source
    | Switch_subswitch_element source ->
        List.for_all element source.subswitch_elements
    | Switch_case_element _ | Switch_default_element _ -> true
  in
  match Function_body.definition_source body with
  | None -> false
  | Some source ->
      Typed.function_source_body_valid source
      && Option.fold ~none:false
           ~some:(fun definition ->
             Option.fold ~none:false ~some:statement definition.Ast.body)
           (Typed.function_source_body source)

let requests_match qualifiers requests =
  same_list
    (fun (qualifier : Ast.register_qualifier) request ->
      (match (qualifier.kind, Request.kind request) with
        | Ast.Reg, Request.Allocate | Noreg, Disable -> true
        | _ -> false)
      && (match (qualifier.position, Request.position request) with
        | Ast.Before_type, Request.Before_type | After_type, After_type -> true
        | _ -> false)
      && qualifier.spelling = Request.spelling request
      && Sema.Initializer_source.origin_of_location qualifier.location
         = Request.origin request
      &&
      match
        (qualifier.explicit_register, Request.explicit_register request)
      with
      | None, None -> true
      | Some identifier, Some register ->
          identifier.spelling = Request.explicit_register_spelling register
          && Sema.Initializer_source.origin_of_location identifier.location
             = Request.explicit_register_origin register
          && Request.canonical_u64_register_number identifier.spelling
             = Some (Request.explicit_register_number register)
      | _ -> false)
    qualifiers requests

let source_requests_match ~frame ~body =
  try
    let source = get (Function_body.definition_source body) in
    let definition = get (Typed.function_source_body source) in
    let parameters =
      Frame.function_header frame
      |> Headers.function_signature |> Headers.signature_parameters
    in
    require
      (same_list
         (fun original parameter ->
           Option.equal ( == ) (Some original)
             (Headers.parameter_source parameter)
           && requests_match original.Ast.register_qualifiers
                (Headers.parameter_register_requests parameter))
         definition.parameters parameters);
    let declarations = ref [] in
    let rec statement = function
      | Ast.Local_declaration_statement local ->
          declarations := !declarations @ [ local ]
      | Block_statement block -> List.iter statement block.block_statements
      | Sequence_statement sequence ->
          List.iter
            (fun element -> statement element.Ast.sequence_statement)
            sequence.sequence_elements
      | If_statement conditional ->
          statement conditional.if_then_branch;
          Option.iter
            (fun branch -> statement branch.Ast.else_branch)
            conditional.if_else_clause
      | While_statement loop -> statement loop.while_body
      | Do_while_statement loop -> statement loop.do_body
      | For_statement loop ->
          statement loop.for_initializer;
          Option.iter statement loop.for_update;
          statement loop.for_body
      | Lock_statement locked -> statement locked.lock_body
      | Try_catch_statement tried ->
          statement tried.try_body;
          statement tried.catch_body
      | Switch_statement switch -> List.iter element switch.switch_elements
      | _ -> ()
    and element = function
      | Ast.Switch_statement_element source -> statement source
      | Switch_subswitch_element source ->
          List.iter element source.subswitch_elements
      | Switch_case_element _ | Switch_default_element _ -> ()
    in
    Option.iter statement definition.body;
    List.iter
      (fun location ->
        let requests =
          match Frame.location_kind location with
          | Frame.Named_parameter ->
              let index =
                get
                  (Binding.binding_parameter_index
                     (Frame.location_binding location))
              in
              let parameter =
                get
                  (List.find_opt
                     (fun parameter ->
                       Headers.parameter_index parameter = index)
                     parameters)
              in
              Headers.parameter_register_requests parameter
          | Automatic_local ->
              let local = get (Frame.location_local_source location) in
              let candidates =
                List.concat_map
                  (fun declaration -> declaration.Ast.local_declarators)
                  !declarations
              in
              let matching =
                List.filter
                  (fun declarator ->
                    Sema.Initializer_source.origin_of_location
                      declarator.Ast.local_name.location
                    = Sema.Symbol.origin (Local.local_symbol local))
                  candidates
              in
              let declarator =
                match matching with
                | [ declarator ] -> declarator
                | _ -> raise Unproved
              in
              require
                (requests_match declarator.local_register_qualifiers
                   (Local.local_register_requests local));
              Local.local_register_requests local
          | Static_local | Variadic_argc | Variadic_argv -> raise Unproved
        in
        require
          (Frame.location_register_selection location
          = Request.effective requests))
      (Frame.function_locations frame);
    true
  with Unproved -> false

let authenticate_body ~frame ~body ~locations source roots =
  let definition = get (Typed.function_source_body source) in
  require
    (definition.variadic = None
    && definition.empty_parameter_entries = []
    && definition.name.spelling = Sema.Symbol.name (Function_body.symbol body));
  let parameters =
    Frame.function_header frame
    |> Headers.function_signature |> Headers.signature_parameters
  in
  require
    (same_list
       (fun original parameter ->
         Option.equal ( == ) (Some original)
           (Headers.parameter_source parameter)
         && requests_match original.Ast.register_qualifiers
              (Headers.parameter_register_requests parameter))
       definition.parameters parameters);
  let statements =
    match definition.body with
    | Some (Ast.Block_statement block) -> block.block_statements
    | Some statement -> [ statement ]
    | None -> raise Unproved
  in
  let remaining = ref roots and ordered = ref [] and declarations = ref 0 in
  let take matches =
    let root =
      match List.filter matches !remaining with
      | [ root ] -> root
      | _ -> raise Unproved
    in
    remaining := List.filter (fun other -> other != root) !remaining;
    ordered := root :: !ordered
  in
  let expression_matches value ast =
    Option.equal ( == )
      (Source.argument_expression_source (Typed.result_source value))
      (Some ast)
  in
  let rec statement depth node =
    require (depth > 0);
    match node with
    | Ast.Sequence_statement sequence ->
        require (sequence.sequence_leading_commas = []);
        List.iter
          (fun element ->
            require (element.Ast.sequence_following_commas = []);
            statement (depth - 1) element.sequence_statement)
          sequence.sequence_elements
    | Ast.Expression_statement statement ->
        take (function
          | Statement root ->
              expression_matches
                (Typed.expression_statement_value root)
                statement.expression_statement_expression
          | _ -> false)
    | Ast.Return_statement statement ->
        let expression = get statement.return_value in
        take (function
          | Return root ->
              expression_matches (get (Typed.return_value root)) expression
              && Source.return_origin (Typed.return_source root)
                 = Sema.Initializer_source.origin_of_location
                     statement.return_location
          | _ -> false)
    | Ast.Local_declaration_statement declaration ->
        require (declaration.local_storage = Ast.Automatic_local);
        List.iteri
          (fun declarator_index (declarator : Ast.local_declarator) ->
            require
              (declarator.local_pointer_layers = []
              && declarator.local_array_dimensions = []
              && declarator.local_function_pointer = None);
            let initialization = get declarator.local_initializer in
            let expression =
              match initialization.local_initializer_value with
              | Ast.Scalar_initializer value -> value
              | _ -> raise Unproved
            in
            take (function
              | Initializer root ->
                  let local =
                    Typed.initializer_source root |> Source.initializer_local
                  in
                  Local.local_declaration_index local = !declarations
                  && Local.local_declarator_index local = declarator_index
                  && Sema.Symbol.origin (Local.local_symbol local)
                     = Sema.Initializer_source.origin_of_location
                         declarator.local_name.location
                  && Local.local_declaration_origin local
                     = Sema.Initializer_source.origin_of_location
                         declaration.local_declaration_location
                  && requests_match declarator.local_register_qualifiers
                       (Local.local_register_requests local)
                  && List.exists
                       (fun location ->
                         Option.equal ( == )
                           (Frame.location_local_source location)
                           (Some local))
                       locations
                  && expression_matches
                       (Typed.initializer_value root)
                       expression
              | _ -> false))
          declaration.local_declarators;
        incr declarations
    (* PrsStmt.HC:150 clears CCF_NO_REG_OPT at entry and :852 sets it for
       try/catch while parsing, even if no expression survives. Inspect the
       original statement tree, never infer its absence from optimized IR. *)
    | _ -> raise Unproved
  in
  List.iter (statement 32) statements;
  require (!remaining = []);
  List.rev !ordered

let source_order ~frame ~body =
  let locations = Frame.function_locations frame in
  require (locations <> [] && List.length locations <= 32);
  require
    (List.for_all
       (fun location ->
         Frame.location_value_shape location = Frame.Scalar
         && Frame.location_declarator_shape location = Frame.Object
         && Frame.location_dimensions location = []
         && integer_type (Frame.location_checked_type location)
         && Option.is_some (Frame.location_frame_slot location))
       locations);
  let parameters =
    Frame.function_header frame
    |> Headers.function_signature |> Headers.signature_parameters
  in
  let parameter_locations =
    List.mapi
      (fun index parameter ->
        require (Headers.parameter_index parameter = index);
        let matches =
          List.filter
            (fun location ->
              Frame.location_kind location = Frame.Named_parameter
              && Binding.binding_parameter_index
                   (Frame.location_binding location)
                 = Some index)
            locations
        in
        let location =
          match matches with
          | [ location ] -> location
          | _ -> raise Unproved
        in
        let original = get (Headers.parameter_source parameter) in
        let name = get original.name in
        require
          (Sema.Symbol.name (Frame.location_symbol location) = name.spelling);
        location)
      parameters
  in
  let locals =
    List.filter
      (fun location -> Frame.location_kind location = Frame.Automatic_local)
      locations
  in
  let local_key location =
    let original = get (Frame.location_local_source location) in
    let binding = Frame.location_binding location in
    let declaration = Local.local_declaration_index original
    and declarator = Local.local_declarator_index original in
    require
      (Binding.binding_local_declaration_index binding = Some declaration
      && Binding.binding_local_declarator_index binding = Some declarator
      && Local.local_symbol original == Frame.location_symbol location
      && Local.local_storage original = Local.Automatic
      && Option.is_some (Local.local_initializer original));
    (declaration, declarator)
  in
  let locals =
    List.sort
      (fun left right -> compare (local_key left) (local_key right))
      locals
  in
  let previous = ref None in
  List.iter
    (fun location ->
      let declaration, declarator = local_key location in
      let span =
        Sema.Symbol.origin (Frame.location_symbol location) |> source_span
      in
      (match !previous with
      | None -> require (declaration = 0 && declarator = 0)
      | Some (old_declaration, old_declarator, old_span) ->
          require
            (((declaration = old_declaration && declarator = old_declarator + 1)
             || (declaration = old_declaration + 1 && declarator = 0))
            && span.source = old_span.Common.Span.source
            && span.start > old_span.start));
      previous := Some (declaration, declarator, span))
    locals;
  let members_match members ordered =
    same_list
      (fun (position, member) location ->
        Function_body.member_position member = position
        && Function_body.member_symbol member == Frame.location_symbol location
        && Sema.Type.equal
             (Function_body.member_type member)
             (Frame.location_checked_type location))
      (List.mapi (fun position member -> (position, member)) members)
      ordered
  in
  require
    (members_match (Function_body.parameters body) parameter_locations
    && members_match (Function_body.locals body) locals);
  let ordered = parameter_locations @ locals in
  (* Neither the mutable frame list nor a prior witness supplies member order.
     Parameter indexes, original local declaration/declarator indexes, binding
     ordinals, source positions and body-member positions must all agree. *)
  require (same_list ( == ) ordered locations);
  List.iteri
    (fun ordinal location ->
      require
        (Binding.binding_ordinal (Frame.location_binding location) = ordinal
        && Binding.binding_symbol (Frame.location_binding location)
           == Frame.location_symbol location))
    ordered;
  ordered

let comparison = function
  | O.Ic_equ_equ
  | Ic_not_equ
  | Ic_less
  | Ic_less_equ
  | Ic_greater
  | Ic_greater_equ -> true
  | _ -> false

let binary_opcode = function
  | "+" -> O.Ic_add
  | "-" -> Ic_sub
  | "*" -> Ic_mul
  | "/" -> Ic_div
  | "%" -> Ic_mod
  | "<<" -> Ic_shl
  | ">>" -> Ic_shr
  | "&" -> Ic_and
  | "|" -> Ic_or
  | "^" -> Ic_xor
  | "==" -> Ic_equ_equ
  | "!=" -> Ic_not_equ
  | "<" -> Ic_less
  | "<=" -> Ic_less_equ
  | ">" -> Ic_greater
  | ">=" -> Ic_greater_equ
  | "=" -> Ic_assign
  | "+=" -> Ic_add_equ
  | "-=" -> Ic_sub_equ
  | "*=" -> Ic_mul_equ
  | "/=" -> Ic_div_equ
  | "%=" -> Ic_mod_equ
  | "<<=" -> Ic_shl_equ
  | ">>=" -> Ic_shr_equ
  | "&=" -> Ic_and_equ
  | "|=" -> Ic_or_equ
  | "^=" -> Ic_xor_equ
  | _ -> raise Unproved

let write_opcode = function
  | O.Ic_assign
  | Ic_add_equ
  | Ic_sub_equ
  | Ic_mul_equ
  | Ic_div_equ
  | Ic_mod_equ
  | Ic_shl_equ
  | Ic_shr_equ
  | Ic_and_equ
  | Ic_or_equ
  | Ic_xor_equ
  | Ic_pp_
  | Ic_mm_
  | Ic__pp
  | Ic__mm -> true
  | _ -> false

let equal_instruction (left : Seq.description) (right : Seq.description) =
  Seq.Instruction_id.equal left.instruction_id right.instruction_id
  && left.opcode = right.opcode
  && left.operands = right.operands
  && left.result = right.result && left.flags = right.flags
  && left.span = right.span
  && Option.equal Sema.Type.equal left.target_type right.target_type
  &&
  match (left.payload, right.payload) with
  | None, None -> true
  | Some (Seq.Integer left), Some (Seq.Integer right) -> left = right
  | Some (Seq.Block left), Some (Seq.Block right) ->
      Seq.Block_id.equal left right
  | _ -> false

let analyze ~frame ~body =
  try
    require (Function_body.definition_matches_frame body frame);
    let source = get (Function_body.definition_source body) in
    let locations = source_order ~frame ~body in
    require
      (Typed.function_calls source = []
      && Typed.function_conditions source = []
      && Typed.function_selectors source = []
      && Typed.function_switch_cases source = []
      && Typed.function_implicit_outputs source = []);
    let returned =
      match Typed.function_returns source with
      | [ returned ] -> returned
      | _ -> raise Unproved
    in
    require
      (Typed.return_presence returned = Typed.Matching_value
      && integer_type (Typed.return_execution_type returned));
    let roots =
      List.map
        (fun root -> Initializer root)
        (Typed.function_initializers source)
      @ List.map
          (fun root -> Statement root)
          (Typed.function_expression_statements source)
      @ [ Return returned ]
    in
    let roots = authenticate_body ~frame ~body ~locations source roots in
    let previous = ref None in
    List.iter
      (fun root ->
        let span = root_span root in
        Option.iter
          (fun old ->
            require
              (old.Common.Span.source = span.source && old.stop <= span.start))
          !previous;
        previous := Some span)
      roots;
    require
      (match List.rev roots with
      | Return actual :: _ -> actual == returned
      | _ -> false);
    let seen = ref [] and accesses = ref [] and initialized = ref [] in
    let claim value =
      require (not (List.exists (( == ) value) !seen));
      require (List.exists (( == ) value) (Typed.function_all_results source));
      require
        (Typed.result_class value = Typed.Integer_result
        && Typed.result_intrinsic_conversion value
           = Typed.No_intrinsic_conversion
        && Typed.result_array_rank value = 0
        && integer_type (get (Typed.result_type value)));
      seen := value :: !seen
    in
    let child expected value =
      require
        (Option.equal ( == )
           (Source.argument_expression_source (Typed.result_source value))
           (Some expected));
      value
    in
    let member value kind =
      let original = Typed.result_source value in
      let bound =
        match Source.argument_expression_kind original with
        | Source.Bound_identifier_expression bound -> bound
        | _ -> raise Unproved
      in
      let binding =
        match
          Sema.Module_expression_binding.occurrence_resolution
            (Source.bound_identifier_occurrence bound)
        with
        | Sema.Module_expression_binding.Local_binding binding -> binding
        | _ -> raise Unproved
      in
      let location =
        get
          (List.find_opt
             (fun location -> Frame.location_binding location == binding)
             locations)
      in
      let identifier =
        match Source.argument_expression_source original with
        | Some (Ast.Identifier_expression identifier) -> identifier
        | _ -> raise Unproved
      in
      require
        (identifier.spelling = Sema.Symbol.name (Frame.location_symbol location)
        && identifier.spelling
           = Sema.Module_expression_binding.occurrence_name
               (Source.bound_identifier_occurrence bound)
        && Sema.Initializer_source.origin_of_location identifier.location
           = Typed.result_origin value);
      require
        (Frame.location_kind location = Frame.Named_parameter
        || List.exists (( == ) location) !initialized);
      let span = Typed.result_origin value |> source_span in
      require
        (Source.argument_expression_origin original = Typed.result_origin value);
      accesses := { location; span; kind } :: !accesses
    in
    let rec expression depth ~write ~inside_comparison value =
      require (depth > 0);
      claim value;
      let original = Typed.result_source value in
      let ast = get (Source.argument_expression_source original) in
      require
        (Sema.Initializer_source.origin_of_location
           (Ast.expression_location ast)
        = Typed.result_origin value);
      match Source.argument_expression_kind original with
      | Source.Integer_literal bits -> (
          match ast with
          | Ast.Integer_literal literal ->
              require (literal.literal_value = Ast.Integer_value bits)
          | _ -> raise Unproved)
      | Bound_identifier_expression _ -> member value Read
      | Parenthesized_expression _ ->
          let grouped =
            match ast with
            | Ast.Parenthesized_expression grouped -> grouped
            | _ -> raise Unproved
          in
          expression (depth - 1) ~write:false ~inside_comparison
            (child grouped.grouped_expression
               (get (Typed.result_operand value)))
      | Prefix_expression prefix -> (
          let ast =
            match ast with
            | Ast.Prefix_expression prefix -> prefix
            | _ -> raise Unproved
          in
          require
            (match
               (Source.prefix_operator prefix, ast.prefix_operator_kind)
             with
            | Source.Unary_plus, Ast.Unary_plus
            | Unary_minus, Unary_minus
            | Bitwise_not, Bitwise_not
            | Logical_not, Logical_not
            | Pre_increment, Pre_increment
            | Pre_decrement, Pre_decrement -> true
            | _ -> false);
          let operand =
            child ast.prefix_operand (get (Typed.result_operand value))
          in
          match Source.prefix_operator prefix with
          | Source.Unary_plus | Unary_minus | Bitwise_not | Logical_not ->
              expression (depth - 1) ~write:false ~inside_comparison operand
          | (Pre_increment | Pre_decrement) when write ->
              claim operand;
              member operand Write
          | _ -> raise Unproved)
      | Postfix_expression postfix when write ->
          let ast =
            match ast with
            | Ast.Postfix_expression postfix -> postfix
            | _ -> raise Unproved
          in
          require
            (match
               (Source.postfix_operator postfix, ast.postfix_operator_kind)
             with
            | Source.Post_increment, Ast.Post_increment
            | Post_decrement, Post_decrement -> true
            | _ -> false);
          let operand =
            child ast.postfix_operand (get (Typed.result_operand value))
          in
          claim operand;
          member operand Write
      | Binary_expression binary ->
          let ast =
            match Source.argument_expression_source original with
            | Some (Frontend.Ast.Binary_expression ast) -> ast
            | _ -> raise Unproved
          in
          let opcode = binary_opcode ast.binary_operator_spec.spelling in
          require (Source.binary_operator binary = opcode);
          let left, right = get (Typed.result_binary_operands value) in
          let left = child ast.binary_left left
          and right = child ast.binary_right right in
          if write_opcode opcode then (
            require write;
            claim left;
            member left Write;
            expression (depth - 1) ~write:false ~inside_comparison:false right)
          else (
            require (not (comparison opcode && inside_comparison));
            let inside_comparison = inside_comparison || comparison opcode in
            expression (depth - 1) ~write:false ~inside_comparison left;
            expression (depth - 1) ~write:false ~inside_comparison right)
      | _ -> raise Unproved
    in
    List.iter
      (fun root ->
        let value = root_value root in
        require
          (Result.is_ok
             (Source.validate_source_expression
                ~source:
                  (get
                     (Source.argument_expression_source
                        (Typed.result_source value)))
                ~expression:(Typed.result_source value)
                ~calls:[] ()));
        expression 32
          ~write:
            (match root with
            | Statement _ -> true
            | _ -> false)
          ~inside_comparison:false value;
        match root with
        | Initializer initial ->
            let original =
              Typed.initializer_source initial |> Source.initializer_local
            in
            let location =
              get
                (List.find_opt
                   (fun location ->
                     Option.equal ( == )
                       (Frame.location_local_source location)
                       (Some original))
                   locations)
            in
            require (not (List.exists (( == ) location) !initialized));
            initialized := location :: !initialized;
            accesses :=
              {
                location;
                span =
                  Sema.Symbol.origin (Frame.location_symbol location)
                  |> source_span;
                kind = Write;
              }
              :: !accesses
        | Statement _ | Return _ -> ())
      roots;
    require (List.length !seen = List.length (Typed.function_all_results source));
    require
      (List.for_all
         (fun location ->
           Frame.location_kind location = Frame.Named_parameter
           || List.exists (( == ) location) !initialized)
         locations);
    let graph = Function_body.body body in
    let entry = Block_graph.entry graph in
    let blocks = Block_graph.blocks graph in
    let code = descriptions entry in
    require (List.length code <= 512);
    let others = List.filter (fun block -> block != entry) blocks in
    let epilogue =
      match
        List.filter
          (fun block ->
            match descriptions block with
            | [ ret ] -> ret.opcode = O.Ic_ret
            | _ -> false)
          others
      with
      | [ epilogue ] -> epilogue
      | _ -> raise Unproved
    in
    let leave = Block_graph.block_id epilogue in
    require
      (Block_graph.successors entry = [ leave ]
      && Block_graph.successors epilogue = []);
    (match descriptions epilogue with
    | [ ret ] ->
        require
          (ret.opcode = O.Ic_ret && ret.operands = [] && ret.result = None
         && ret.target_type = None && ret.payload = None && ret.flags = 0L
          && ret.span = Function_body.span body)
    | _ -> raise Unproved);
    (* Integer_program_lowering appends one unreachable final fallthrough jump
       after a source return, before the shared RET block. It must be precisely
       this scaffolding, never another expression or control-flow region. *)
    let scaffolding = List.filter (fun block -> block != epilogue) others in
    require (List.length scaffolding <= 1);
    List.iter
      (fun block ->
        require (Block_graph.successors block = [ leave ]);
        match descriptions block with
        | [ jump ] ->
            require
              (jump.opcode = O.Ic_jmp && jump.operands = []
             && jump.result = None && jump.target_type = None
              && jump.payload = Some (Seq.Block leave)
              && jump.flags = 0L
              && jump.span = Function_body.span body)
        | _ -> raise Unproved)
      scaffolding;
    let produced =
      List.fold_left
        (fun map (item : Seq.description) ->
          match item.result with
          | None -> map
          | Some result ->
              require (not (Values.mem result.value_id map));
              Values.add result.value_id item map)
        Values.empty code
    in
    let users id =
      List.filter
        (fun (item : Seq.description) ->
          List.exists (Seq.Value_id.equal id) item.operands)
        code
    in
    let pending = ref code in
    let consume expected =
      List.iter
        (fun expected ->
          match !pending with
          | actual :: rest ->
              require (equal_instruction expected actual);
              pending := rest
          | [] -> raise Unproved)
        expected
    in
    List.iter
      (fun root ->
        let first =
          match !pending with
          | first :: _ -> first
          | [] -> raise Unproved
        in
        let instruction_id = first.Seq.instruction_id in
        let value_id = (get first.result).Seq.value_id in
        require
          (Seq.Instruction_id.to_int instruction_id < max_int - 512
          && Seq.Value_id.to_int value_id < max_int - 512);
        match root with
        | Return returned -> (
            match
              Return_lowering.lower_function_return ~frame ~optimize_shifts:true
                ~optimize_division:true ~instruction_id ~value_id ~leave
                returned
            with
            | Ok (Return_lowering.Lowered result) ->
                consume
                  (Return_lowering.sequence result
                  |> Seq.instructions |> List.map Seq.description)
            | _ -> raise Unproved)
        | Initializer _ | Statement _ ->
            let lowered =
              match root with
              | Initializer initial ->
                  Expression_lowering.lower_initializer ~frame
                    ~optimize_shifts:true ~optimize_division:true
                    ~instruction_id ~value_id initial
              | Statement statement ->
                  Expression_lowering.lower_typed_result ~frame
                    ~optimize_shifts:true ~optimize_division:true
                    ~instruction_id ~value_id
                    (Typed.expression_statement_value statement)
              | Return _ -> assert false
            in
            let result =
              match lowered with
              | Ok (Expression_lowering.Lowered result) -> result
              | _ -> raise Unproved
            in
            consume
              (Expression_lowering.sequence result
              |> Seq.instructions |> List.map Seq.description);
            let terminal : Seq.description =
              {
                instruction_id = Expression_lowering.next_instruction_id result;
                opcode = O.Ic_end_exp;
                operands = [ Expression_lowering.result_value result ];
                result = None;
                target_type = None;
                payload = None;
                flags = 0x200L;
                span = Some (root_span root);
              }
            in
            consume [ terminal ])
      roots;
    require (!pending = []);
    (* Independently pair original source occurrences with unique canonical
       RBP+offset ADDs and their sole DEREF/write consumer. This guards against
       shared, omitted or extra accesses even if lowering changes in future. *)
    let remaining = ref !accesses in
    List.iter
      (fun (item : Seq.description) ->
        if item.opcode = O.Ic_rbp then (
          let rbp = (get item.result).Seq.value_id in
          let address =
            match users rbp with
            | [ address ] -> address
            | _ -> raise Unproved
          in
          require (address.opcode = O.Ic_add && address.payload = None);
          let offset =
            match address.operands with
            | [ base; offset ] when Seq.Value_id.equal base rbp ->
                get (Values.find_opt offset produced)
            | _ -> raise Unproved
          in
          require
            (offset.opcode = O.Ic_imm_i64
            && users (get offset.result).value_id = [ address ]);
          let displacement =
            match offset.payload with
            | Some (Seq.Integer n) -> n
            | _ -> raise Unproved
          in
          let consumer =
            match users (get address.result).value_id with
            | [ consumer ] -> consumer
            | _ -> raise Unproved
          in
          let kind =
            if consumer.opcode = O.Ic_deref then Read
            else (
              require (write_opcode consumer.opcode);
              Write)
          in
          let span = get address.span in
          let candidates =
            List.filter
              (fun access ->
                access.span = span && access.kind = kind
                && Frame.frame_slot_displacement
                     (get (Frame.location_frame_slot access.location))
                   = displacement)
              !remaining
          in
          let access =
            match candidates with
            | [ access ] -> access
            | _ -> raise Unproved
          in
          require (item.span = Some span && offset.span = Some span);
          remaining :=
            List.filter (fun candidate -> candidate != access) !remaining))
      code;
    require (!remaining = []);
    Some
      (List.map
         (fun location ->
           ( location,
             List.length
               (List.filter
                  (fun access -> access.location == location)
                  !accesses) ))
         locations)
  with Unproved -> None
