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
   For homogeneous F64 +, -, *, / and unary +/-, OptLib.HC:161-166,212-215
   likewise folds only immediates. OptPass012.HC:376-398,559-614,661-687
   retains every variable subtree while removing or reassociating constants.
   Direct F64 assignment is unchanged by :866-887; a same-register copy can
   disappear only after scoring (BackLib.HC:509-510). Source occurrences
   therefore survive to OptPass3 one for one; hosted loads alone are never
   used to infer the TempleOS score or floating execution semantics. *)

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

let f64_type type_ =
  Sema.Type.pointer_depth type_ = 0
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive (_, Sema.Primitive_type.F64) -> true
  | _ -> false

let scalar_type type_ = integer_type type_ || f64_type type_

let owned_type type_ =
  scalar_type type_
  || Sema.Type.pointer_depth type_ <= 2
     &&
     match Sema.Type.base type_ with
     | Sema.Type.Primitive (_, primitive) ->
         Option.is_some (Sema.Primitive_type.integer_storage_info primitive)
     | _ -> false

let authenticate_type location reference specifier layers =
  let form, primitive, spelling, origin =
    match specifier with
    | Ast.Primitive_type_specifier original ->
        ( Sema.Type.Public_spelling,
          original.primitive,
          original.spelling,
          Sema.Initializer_source.origin_of_location original.location )
    | Ast.Internal_type_specifier original ->
        ( Sema.Type.Internal_storage,
          original.primitive,
          original.spelling,
          Sema.Initializer_source.origin_of_location original.location )
    | Ast.Named_type_specifier _ -> raise Unproved
  in
  let type_ = Frame.location_checked_type location in
  require
    (Sema.Type.base type_ = Sema.Type.Primitive (form, primitive)
    && Sema.Type.pointer_depth type_ = List.length layers
    && Sema.Type.equal type_ (Sema.Type_reference.resolved_type reference)
    && Sema.Type_reference.spelling reference = spelling
    && Sema.Type_reference.spelling_origin reference = origin
    && Option.equal ( == ) (Some reference)
         (Frame.location_type_reference location)
    && List.for_all
         (fun (index, (layer : Ast.pointer_layer)) ->
           layer.depth = index + 1 && layer.spelling = "*")
         (List.mapi (fun index layer -> (index, layer)) layers));
  require
    (List.map
       (fun (layer : Ast.pointer_layer) ->
         Sema.Initializer_source.origin_of_location layer.location)
       layers
    = Sema.Type_reference.pointer_origins reference)

let descriptions block =
  Block_graph.instructions block |> Seq.instructions |> List.map Seq.description

type access_kind = Read | Write | Address | Array_element of bool
(* Array_element records whether the pre-scoring normalized address is exactly
   the member start. It is not an ordinary scalar-member read: interior and
   dynamic indexing retain the array LEA. *)

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

let source_local_declarations body =
  let rec statement = function
    | Ast.Local_declaration_statement local -> [ local ]
    | Block_statement block -> List.concat_map statement block.block_statements
    | Sequence_statement sequence ->
        List.concat_map
          (fun element -> statement element.Ast.sequence_statement)
          sequence.sequence_elements
    | If_statement conditional ->
        statement conditional.if_then_branch
        @ Option.fold ~none:[]
            ~some:(fun branch -> statement branch.Ast.else_branch)
            conditional.if_else_clause
    | While_statement loop -> statement loop.while_body
    | Do_while_statement loop -> statement loop.do_body
    | For_statement loop ->
        statement loop.for_initializer
        @ Option.fold ~none:[] ~some:statement loop.for_update
        @ statement loop.for_body
    | _ -> []
  in
  let source = get (Function_body.definition_source body) in
  let definition = get (Typed.function_source_body source) in
  Option.fold ~none:[] ~some:statement definition.body

let source_order ?(owned = false) ~frame ~body () =
  let locations = Frame.function_locations frame in
  require (locations <> [] && List.length locations <= 32);
  require
    (List.for_all
       (fun location ->
         (match
            ( Frame.location_value_shape location,
              Frame.location_dimensions location )
          with
           | Frame.Scalar, [] -> true
           | Frame.Array, _ :: _ when owned -> (
               Frame.location_kind location = Frame.Automatic_local
               && owned_type (Frame.location_checked_type location)
               && Sema.Type.pointer_depth (Frame.location_checked_type location)
                  = 0
               && Frame.location_source_dimensions_checked location
               &&
               match Frame.location_register_selection location with
               | Request.Explicit _ -> false
               | _ -> true)
           | _ -> false)
         && Frame.location_declarator_shape location = Frame.Object
         && Option.is_none (Frame.location_callback_pointer location)
         && Option.is_none (Frame.location_aggregate_allocation location)
         && (if owned then
               owned_type (Frame.location_checked_type location)
               && Sema.Type.pointer_depth (Frame.location_checked_type location)
                  <= 1
             else scalar_type (Frame.location_checked_type location))
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
        require
          (Sema.Symbol.name (Frame.location_symbol location)
           = Headers.parameter_lookup_name parameter
          && Sema.Symbol.origin (Frame.location_symbol location)
             = Headers.parameter_lookup_origin parameter);
        if owned then (
          require (original.function_pointer = None);
          authenticate_type location
            (Headers.parameter_type_reference parameter)
            original.type_specifier original.pointer_layers);
        location)
      parameters
  in
  let locals =
    List.filter
      (fun location -> Frame.location_kind location = Frame.Automatic_local)
      locations
  in
  let declarations = if owned then source_local_declarations body else [] in
  if owned then
    require
      (List.fold_left
         (fun count declaration ->
           count + List.length declaration.Ast.local_declarators)
         0 declarations
      = List.length locals);
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
      && (owned || Option.is_some (Local.local_initializer original)));
    if owned then (
      let source = get (List.nth_opt declarations declaration) in
      let item = get (List.nth_opt source.Ast.local_declarators declarator) in
      require
        (source.local_storage = Ast.Automatic_local
        && item.local_function_pointer = None
        && item.local_name.spelling
           = Sema.Symbol.name (Frame.location_symbol location)
        && Sema.Initializer_source.origin_of_location item.local_name.location
           = Sema.Symbol.origin (Frame.location_symbol location)
        && Option.is_some item.local_initializer
           = Option.is_some (Local.local_initializer original));
      authenticate_type location
        (Local.local_type_reference original)
        source.local_type_specifier item.local_pointer_layers;
      let dimensions = Local.local_array_dimensions original in
      require
        (same_list
           (fun ast dimension ->
             Option.equal ( == ) (Some ast)
               (Local.array_dimension_source dimension)
             && Option.equal ( == ) ast.Ast.dimension_expression
                  (Local.array_dimension_source_expression dimension))
           item.local_array_dimensions dimensions);
      require
        (same_list
           (fun (index, original) dimension ->
             Local.array_dimension_index original = index
             && Frame.dimension_kind dimension = Frame.Source_extent
             && Frame.dimension_runtime_dependencies dimension = []
             && Frame.dimension_offset_dependencies dimension = []
             && Frame.dimension_value dimension > 0L
             &&
             match Local.array_dimension_source_expression original with
             | None -> false
             | Some expression ->
                 Sema.Aggregate_layout.evaluate_expression
                   ~context:Sema.Aggregate_layout.Array_dimension
                   ~current_position:0L
                   (Sema.Closed_layout_expression.of_ast expression)
                 = Ok (Frame.dimension_value dimension))
           (List.mapi (fun index dimension -> (index, dimension)) dimensions)
           (Frame.location_dimensions location)));
    (declaration, declarator)
  in
  let locals =
    List.sort
      (fun left right -> compare (local_key left) (local_key right))
      locals
  in
  (if owned then
     let original =
       List.mapi
         (fun declaration_index declaration ->
           List.mapi
             (fun declarator_index declarator ->
               (declaration_index, declarator_index, declaration, declarator))
             declaration.Ast.local_declarators)
         declarations
       |> List.concat
     in
     require
       (same_list
          (fun (declaration_index, declarator_index, declaration, declarator)
               location ->
            let local = get (Frame.location_local_source location) in
            local_key location = (declaration_index, declarator_index)
            && Local.local_declaration_origin local
               = Sema.Initializer_source.origin_of_location
                   declaration.Ast.local_declaration_location
            && Local.local_declarator_origin local
               = Sema.Initializer_source.origin_of_location
                   declarator.Ast.local_declarator_location
            && requests_match declarator.local_register_qualifiers
                 (Local.local_register_requests local))
          original locals));
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
  if owned then (
    let cursor = ref 0L in
    List.iteri
      (fun index location ->
        let type_ = Frame.location_checked_type location in
        let element =
          if Sema.Type.pointer_depth type_ > 0 then 8
          else
            match Sema.Type.base type_ with
            | Sema.Type.Primitive (_, primitive) ->
                (Sema.Primitive_type.info primitive).byte_size
            | _ -> raise Unproved
        in
        require (List.mem element [ 1; 2; 4; 8 ]);
        let offset, bytes, alignment =
          if index < List.length parameter_locations then
            (Int64.of_int (16 + (8 * index)), 8L, 8)
          else
            let bytes =
              List.fold_left
                (fun bytes dimension ->
                  let count = Frame.dimension_value dimension in
                  require (count > 0L && count <= Int64.div Int64.max_int bytes);
                  Int64.mul bytes count)
                (Int64.of_int element)
                (Frame.location_dimensions location)
            in
            (* PrsVar.HC:590-606 aligns by the complete allocation extent,
               not by the primitive element size. Positive extents also make
               candidate ranges disjoint, so first-offset matching is unique. *)
            let alignment =
              if bytes >= 8L then 8
              else if bytes >= 4L then 4
              else if bytes >= 2L then 2
              else 1
            in
            require (Int64.sub Int64.min_int !cursor <= Int64.neg bytes);
            let next = Int64.sub !cursor bytes in
            require (next >= Int64.add Int64.min_int (Int64.of_int alignment));
            cursor := Int64.logand next (Int64.neg (Int64.of_int alignment));
            (!cursor, bytes, alignment)
        in
        let slot = get (Frame.location_frame_slot location) in
        require
          (Frame.location_element_size location = Int64.of_int element
          && Frame.location_allocated_size location = bytes
          && Frame.location_alignment location = alignment
          && Frame.frame_slot_size slot = bytes
          && Frame.frame_slot_displacement slot = offset))
      ordered;
    require
      (Frame.function_frame_size frame = Int64.neg (Int64.logand !cursor (-8L))));
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
  | Some (Seq.Float_bits left), Some (Seq.Float_bits right) -> left = right
  | Some (Seq.Block left), Some (Seq.Block right) ->
      Seq.Block_id.equal left right
  | _ -> false

let analyze ~frame ~body =
  try
    require (Function_body.definition_matches_frame body frame);
    let source = get (Function_body.definition_source body) in
    let locations = source_order ~frame ~body () in
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
      && scalar_type (Typed.return_execution_type returned)
      && Typed.return_conversion returned = Typed.No_intrinsic_conversion
      && f64_type (Typed.return_execution_type returned)
         = f64_type
             (get (Typed.result_type (get (Typed.return_value returned)))));
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
    let floating value = f64_type (get (Typed.result_type value)) in
    let claim value =
      require (not (List.exists (( == ) value) !seen));
      require (List.exists (( == ) value) (Typed.function_all_results source));
      require
        ((match Typed.result_class value with
           | Typed.Integer_result ->
               integer_type (get (Typed.result_type value))
           | F64_result -> floating value
           | Unresolved_actual_class -> false)
        && Typed.result_intrinsic_conversion value
           = Typed.No_intrinsic_conversion
        && Typed.result_array_rank value = 0
        && scalar_type (get (Typed.result_type value)));
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
           = Typed.result_origin value
        && Sema.Type.equal
             (get (Typed.result_type value))
             (Frame.location_checked_type location));
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
          require (not (floating value));
          match ast with
          | Ast.Integer_literal literal ->
              require (literal.literal_value = Ast.Integer_value bits)
          | _ -> raise Unproved)
      | Float_literal bits -> (
          require (floating value);
          match ast with
          | Ast.Float_literal { literal_value = Ast.Float_value actual; _ } ->
              require (Int64.bits_of_float actual = bits)
          | _ -> raise Unproved)
      | Bound_identifier_expression _ -> member value Read
      | Parenthesized_expression _ ->
          let grouped =
            match ast with
            | Ast.Parenthesized_expression grouped -> grouped
            | _ -> raise Unproved
          in
          let operand =
            child grouped.grouped_expression (get (Typed.result_operand value))
          in
          require (floating value = floating operand);
          expression (depth - 1) ~write:false ~inside_comparison operand
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
          require
            (floating value = floating operand
            && ((not (floating value))
               || List.mem
                    (Source.prefix_operator prefix)
                    [ Source.Unary_plus; Unary_minus ]));
          match Source.prefix_operator prefix with
          | Source.Unary_plus | Unary_minus | Bitwise_not | Logical_not ->
              expression (depth - 1) ~write:false ~inside_comparison operand
          | (Pre_increment | Pre_decrement) when write ->
              claim operand;
              member operand Write
          | _ -> raise Unproved)
      | Postfix_expression postfix when write ->
          require (not (floating value));
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
          require (not (floating operand));
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
          require
            (floating value = floating left
            && floating value = floating right
            && ((not (floating value))
               || List.mem opcode
                    [ O.Ic_add; Ic_sub; Ic_mul; Ic_div; Ic_assign ]));
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
            require
              (Typed.initializer_conversion initial
               = Typed.No_intrinsic_conversion
              && scalar_type (Typed.initializer_target_type initial)
              && f64_type (Typed.initializer_target_type initial)
                 = floating value);
            let original =
              Typed.initializer_source initial |> Source.initializer_local
            in
            let location =
              get
                (List.find_opt
                   (fun location ->
                     Frame.location_symbol location
                     == Typed.initializer_target_symbol initial)
                   locations)
            in
            (match
               Frame_address_lowering.prepare_initializer ~frame initial
             with
            | Ok (Some _) -> ()
            | _ -> raise Unproved);
            initialized := location :: !initialized;
            accesses :=
              {
                location;
                span =
                  Sema.Symbol.origin (Local.local_symbol original)
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

(* This ledger is a separate obligation from canonical source reconstruction.
   The statement tree is the complete original tree authenticated by
   Register_variable_analysis.Mixed_scalar_source, before Lower.lower runs.
   PrsStmt.HC:461-563 emits each condition, arm, body and saved for-update once.
   PrsLib.HC:257-270 runs OptPass012 twice, then scoring, then OptPass4.

   The expression whitelist below admits only access-conserving rewrites:
   - OptPass012.HC:308-432,485-658 folds immediates and arithmetic identities,
     retaining every nonconstant subtree (including x*0).
   - :435-484,725-816 folds bitwise/comparison nodes only with immediate inputs.
     Comparison chains and short-circuit forms are deliberately excluded.
   - :828-895 retains direct assignment and statement-update addresses.
   - :939-1068 removes conversion wrappers/constants, not member subtrees.
     Implicit conversion flags likewise do not remove a member access.
   - OptLib.HC:230-484 fuses NOT/comparison/AND branch operators but retains
     their member operands. Decrement-in-condition and nested writes are not
     admitted here; they need their own branch/update phase receipt.

   Each surviving direct frame address contributes score +1, LEA +1 at
   OptPass3.HC:179-199. Its unique direct read (:119-135), unary update
   (:149-163), or assignment/compound update (:223-245) contributes score +1,
   LEA -1. A direct access therefore proves score=2 and lea_balance=0.
   The owned-member extension also retains explicit address formations as
   score=1 and lea_balance=1. IC_ADDR survives the pre-scoring passes, so even
   *(&x) cannot use the direct IC_LEA cancellation. These are source-phase
   events, not estimates from hosted loads or temporary liveness. *)
let analyze_accesses ?(owned = false) ~frame ~body ~statements () =
  let module Lower = Integer_program_lowering in
  let module Policy = Sema.Function_call_conversion_policy in
  try
    require
      (Function_body.definition_matches_frame body frame
      && permits_automatic_analysis body
      && source_requests_match ~frame ~body);
    let source = get (Function_body.definition_source body) in
    let locations = source_order ~owned ~frame ~body () in
    let accesses = ref [] and seen = ref [] and seen_calls = ref [] in
    let initialized = ref [] in
    let preliminary_passes = ref 0 in
    let scalar_or_void_call value =
      let type_ = get (Typed.result_type value) in
      (if owned then owned_type type_ else scalar_type type_)
      || Sema.Type.pointer_depth type_ = 0
         && (match Sema.Type.base type_ with
           | Sema.Type.Primitive (_, (Sema.Primitive_type.I0 | U0)) -> true
           | _ -> false)
         &&
         match Source.argument_expression_kind (Typed.result_source value) with
         | Source.Unresolved_expression Source.Call_expression -> true
         | _ -> false
    in
    let claim ?(array = false) value =
      require
        ((not (List.exists (( == ) value) !seen))
        && List.exists (( == ) value) (Typed.function_all_results source)
        && (Typed.result_array_rank value = 0 || array)
        && scalar_or_void_call value);
      seen := value :: !seen
    in
    let floating value = f64_type (get (Typed.result_type value)) in
    let member_location value =
      let bound =
        match Source.argument_expression_kind (Typed.result_source value) with
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
      require
        (owned
        || Frame.location_kind location = Frame.Named_parameter
        || List.exists (( == ) location) !initialized);
      (match Frame_address_lowering.prepare ~frame value with
      | Ok (Some _) -> ()
      | _ -> raise Unproved);
      location
    in
    let member value kind =
      let location = member_location value in
      accesses :=
        { location; span = source_span (Typed.result_origin value); kind }
        :: !accesses;
      location
    in
    let rec expression depth ~write ~inside_comparison value =
      require (depth > 0);
      claim value;
      let original = Typed.result_source value in
      let ast = get (Source.argument_expression_source original) in
      let child expected value =
        require
          (Option.equal ( == ) (Some expected)
             (Source.argument_expression_source (Typed.result_source value)));
        value
      in
      let read = expression (depth - 1) ~write:false ~inside_comparison in
      match (Source.argument_expression_kind original, ast) with
      | Source.Integer_literal bits, Ast.Integer_literal literal ->
          require (literal.literal_value = Ast.Integer_value bits)
      | Float_literal bits, Ast.Float_literal literal -> (
          match literal.literal_value with
          | Ast.Float_value actual -> require (Int64.bits_of_float actual = bits)
          | _ -> raise Unproved)
      | Bound_identifier_expression _, Ast.Identifier_expression _ ->
          ignore (member value Read)
      | Parenthesized_expression _, Ast.Parenthesized_expression group ->
          read
            (child group.grouped_expression (get (Typed.result_operand value)))
      | Prefix_expression prefix, Ast.Prefix_expression ast -> (
          let operand =
            child ast.prefix_operand (get (Typed.result_operand value))
          in
          match Source.prefix_operator prefix with
          | Source.Unary_plus | Unary_minus -> read operand
          | Bitwise_not | Logical_not ->
              require ((not (floating value)) && not (floating operand));
              read operand
          | Source.Address_of when owned -> lvalue (depth - 1) Address operand
          | Source.Dereference when owned -> read operand
          | (Pre_increment | Pre_decrement) when write ->
              require
                ((not (floating value))
                && (not (floating operand))
                && Sema.Type.pointer_depth (get (Typed.result_type operand)) = 0
                );
              lvalue (depth - 1) Write operand
          | _ -> raise Unproved)
      | Postfix_expression _, Ast.Postfix_expression ast when write ->
          let operand =
            child ast.postfix_operand (get (Typed.result_operand value))
          in
          require ((not (floating value)) && not (floating operand));
          require (Sema.Type.pointer_depth (get (Typed.result_type operand)) = 0);
          lvalue (depth - 1) Write operand
      | Binary_expression binary, Ast.Binary_expression ast ->
          let opcode = binary_opcode ast.binary_operator_spec.spelling in
          require (Source.binary_operator binary = opcode);
          let left, right = get (Typed.result_binary_operands value) in
          let left = child ast.binary_left left
          and right = child ast.binary_right right in
          if write_opcode opcode then (
            require
              (write
              && (opcode = O.Ic_assign
                 || not (floating value || floating left || floating right)));
            if owned && opcode <> O.Ic_assign then (
              require
                (Sema.Type.pointer_depth (get (Typed.result_type left)) = 0);
              match
                Source.argument_expression_kind (Typed.result_source right)
              with
              | Source.Integer_literal n ->
                  require
                    (match opcode with
                    | O.Ic_add_equ | Ic_sub_equ | Ic_mul_equ ->
                        n >= 0L && n <= Int64.of_int32 Int32.max_int
                    | Ic_shl_equ | Ic_shr_equ -> n >= 0L && n < 64L
                    | _ -> false)
              | _ -> ());
            lvalue (depth - 1) Write left;
            expression (depth - 1) ~write:false ~inside_comparison:false right)
          else (
            require
              (((not owned)
               || List.for_all
                    (fun child ->
                      Sema.Type.pointer_depth (get (Typed.result_type child))
                      = 0)
                    [ value; left; right ])
              && (not (comparison opcode && inside_comparison))
              && ((not (floating value || floating left || floating right))
                 || List.mem opcode [ O.Ic_add; Ic_sub; Ic_mul; Ic_div ]));
            let inside_comparison = inside_comparison || comparison opcode in
            expression (depth - 1) ~write:false ~inside_comparison left;
            expression (depth - 1) ~write:false ~inside_comparison right)
      | Source.Index_expression _, Ast.Index_expression ast when owned ->
          let base, index = get (Typed.result_index_operands value) in
          let base = child ast.index_base base
          and index = child ast.index_value index in
          if Typed.result_array_rank base > 0 then
            array_index (depth - 1) Read value
          else (
            require (Sema.Type.pointer_depth (get (Typed.result_type base)) = 1);
            read base;
            read index)
      | Unresolved_expression Source.Call_expression, Ast.Call_expression ast ->
          let direct =
            match Typed.result_call_resolution value with
            | Some (Source.Direct_call direct) -> direct
            | _ -> raise Unproved
          in
          let call =
            match
              List.filter_map
                (function
                  | Typed.Direct_call_result call
                    when Typed.direct_source call |> Policy.direct_source
                         == direct -> Some call
                  | _ -> None)
                (Typed.function_calls source)
            with
            | [ call ] -> call
            | _ -> raise Unproved
          in
          require
            ((not (List.exists (( == ) call) !seen_calls))
            && Typed.direct_variadic_results call = []);
          seen_calls := call :: !seen_calls;
          let arguments =
            List.map
              (fun fixed ->
                match Typed.fixed_path fixed with
                | Typed.Provided_result value -> value
                | Declared_default_result _ -> raise Unproved)
              (Typed.direct_fixed_results call)
          in
          require (List.length arguments = List.length ast.call_arguments);
          List.iter2
            (fun original argument ->
              match original.Ast.call_argument_value with
              | Ast.Provided_call_argument ast ->
                  (* PrsExp.HC:474-475 preprocesses this exact provided fixed
                     argument before restoring the enclosing stream. Nested
                     arguments inherit those enclosing passes, in addition to
                     their own. Source-declared numeric intrinsics authenticated
                     on this direct PrsFunCall path receive the same schedule;
                     an unowned AST call shape does not establish it. *)
                  let outer = !preliminary_passes in
                  preliminary_passes := outer + 2;
                  Fun.protect
                    ~finally:(fun () -> preliminary_passes := outer)
                    (fun () ->
                      expression (depth - 1) ~write:false
                        ~inside_comparison:false (child ast argument))
              | Omitted_call_argument -> raise Unproved)
            ast.call_arguments arguments
      | _ -> raise Unproved
    and lvalue depth kind value =
      require (depth > 0);
      claim value;
      match Source.argument_expression_kind (Typed.result_source value) with
      | Source.Bound_identifier_expression _ -> ignore (member value kind)
      | Parenthesized_expression _ when owned ->
          lvalue (depth - 1) kind (get (Typed.result_operand value))
      | Prefix_expression prefix
        when owned && Source.prefix_operator prefix = Source.Dereference ->
          expression (depth - 1) ~write:false ~inside_comparison:false
            (get (Typed.result_operand value))
      | Index_expression _ when owned ->
          let base, index = get (Typed.result_index_operands value) in
          if Typed.result_array_rank base > 0 then
            array_index (depth - 1) kind value
          else (
            require (Sema.Type.pointer_depth (get (Typed.result_type base)) = 1);
            expression (depth - 1) ~write:false ~inside_comparison:false base;
            expression (depth - 1) ~write:false ~inside_comparison:false index)
      | _ -> raise Unproved
    and array_index depth kind value =
      let module N = Local_index_normalization in
      (* PrsExp.HC:728-747 preserves a grouped INDEX expression but clears
         CCF_ARRAY on a grouped BASE. Only original complete, ungrouped base
         chains have the dimension/stride receipt below. The final consumer
         has already claimed [value]; unfinished index nodes are claimed here. *)
      let child expected typed =
        require
          (Option.equal ( == ) (Some expected)
             (Source.argument_expression_source (Typed.result_source typed)));
        typed
      in
      let rec chain depth current =
        require (depth > 0);
        let source = Typed.result_source current in
        match
          ( Source.argument_expression_kind source,
            get (Source.argument_expression_source source) )
        with
        | Source.Index_expression original, Ast.Index_expression ast ->
            let base, index = get (Typed.result_index_operands current) in
            ignore (child ast.index_base base);
            ignore (child ast.index_value index);
            require
              (Typed.result_source base == Source.index_base original
              && Typed.result_source index == Source.index_value original
              && Typed.result_array_rank base
                 = Typed.result_array_rank current + 1
              && Typed.result_is_array_address base
              && Typed.result_intrinsic_conversion index = Typed.Result_to_int);
            claim ~array:true base;
            let root, indices = chain (depth - 1) base in
            (root, indices @ [ (current, index) ])
        | Bound_identifier_expression _, Ast.Identifier_expression _ ->
            (current, [])
        | _ -> raise Unproved
      in
      let base, indices = chain depth value in
      let location = member_location base in
      require
        (Typed.result_array_rank value = 0
        && Frame.location_kind location = Frame.Automatic_local
        && Frame.location_value_shape location = Frame.Array
        && List.length indices
           = List.length (Frame.location_dimensions location)
        && Typed.result_array_rank base = List.length indices);
      List.iter
        (fun (current, _) ->
          require
            (Sema.Type.equal
               (get (Typed.result_type current))
               (Frame.location_checked_type location)
            (* Compiler_record.aggregate_index_element_size supplies an
               observation only for aggregate classes. Primitive array strides
               come exclusively from the original frame dimensions. *)
            && Typed.result_index_element_size current = None))
        indices;
      (* Every node is authenticated against its typed class, original AST and
         ordered operands before it enters the phase tree. No writes, calls,
         casts, floating conversions, pointer arithmetic or nested array reads
         can masquerade as a pure integer index. *)
      let rec index_tree depth current =
        require (depth > 0);
        claim current;
        require
          (Option.is_some
             (Integer_scalar_storage.of_type (get (Typed.result_type current)))
          && Typed.result_class current = Typed.Integer_result
          && (match Typed.result_execution_class current with
            | None | Some Typed.Integer_result -> true
            | _ -> false)
          && (match Typed.result_category current with
            | Typed.Object_value | Lvalue -> true
            | _ -> false)
          && Source.argument_expression_origin (Typed.result_source current)
             = Typed.result_origin current
          &&
          match Typed.result_intrinsic_conversion current with
          | Typed.No_intrinsic_conversion | Result_to_int -> true
          | _ -> false);
        let original = Typed.result_source current in
        match
          ( Source.argument_expression_kind original,
            get (Source.argument_expression_source original) )
        with
        | Source.Integer_literal bits, Ast.Integer_literal literal ->
            require (literal.literal_value = Ast.Integer_value bits);
            N.Immediate bits
        | Bound_identifier_expression _, Ast.Identifier_expression _ ->
            let scalar = member current Read in
            require (Frame.location_value_shape scalar = Frame.Scalar);
            N.Scalar current
        | Parenthesized_expression _, Ast.Parenthesized_expression group ->
            index_tree (depth - 1)
              (child group.grouped_expression
                 (get (Typed.result_operand current)))
        | Prefix_expression prefix, Ast.Prefix_expression ast -> (
            let operand =
              index_tree (depth - 1)
                (child ast.prefix_operand (get (Typed.result_operand current)))
            in
            match
              ( Source.prefix_operator prefix,
                ast.prefix_operator_kind,
                ast.prefix_operator.operator_spelling )
            with
            | Source.Unary_plus, Ast.Unary_plus, "+" -> operand
            | Unary_minus, Unary_minus, "-" -> N.Negate operand
            | _ -> raise Unproved)
        | Binary_expression binary, Ast.Binary_expression ast -> (
            let opcode = binary_opcode ast.binary_operator_spec.spelling in
            require (Source.binary_operator binary = opcode);
            let left, right = get (Typed.result_binary_operands current) in
            let left = index_tree (depth - 1) (child ast.binary_left left)
            and right = index_tree (depth - 1) (child ast.binary_right right) in
            match opcode with
            | O.Ic_add -> N.Scalar_add (left, right)
            | Ic_sub -> N.Scalar_subtract (left, right)
            | Ic_mul -> N.Multiply (left, right)
            | _ -> raise Unproved)
        | _ -> raise Unproved
      in
      let rec strides = function
        | [] -> (Frame.location_element_size location, [])
        | dimension :: rest ->
            let bytes, tail = strides rest in
            let count = Frame.dimension_value dimension in
            require
              (bytes > 0L && count > 0L
              && count <= Int64.div Int64.max_int bytes);
            (Int64.mul count bytes, (count, bytes) :: tail)
      in
      let bytes, dimensions = strides (Frame.location_dimensions location) in
      require (bytes = Frame.location_allocated_size location);
      let displacement =
        Frame.frame_slot_displacement (get (Frame.location_frame_slot location))
      in
      let tree =
        List.fold_left2
          (fun address (_, index) (count, stride) ->
            let operand = index_tree (depth - 1) index in
            (* Reject constant out-of-range dimensions independently, including
               dead/unreachable uses. They could charge another candidate's
               numeric range, even when a flattened total appears in bounds. *)
            (match
               get (N.normalize ~preliminary_passes:!preliminary_passes operand)
             with
            | N.Immediate n -> require (n >= 0L && n < count)
            | _ -> ());
            N.Add (address, N.Multiply (N.Immediate stride, operand)))
          (N.Add (N.Frame, N.Immediate displacement))
          indices dimensions
      in
      let normalized =
        get (N.normalize ~preliminary_passes:!preliminary_passes tree)
      in
      require
        (same_list ( == ) (N.scalar_reads tree) (N.scalar_reads normalized));
      (* OptPass3.HC:179-199 charges the actual surviving RBP ADD, not an
         algebraic displacement. A dynamic prefix prevents reassociation with
         a later immediate suffix. The source address must stay in this exact
         member's nonoverlapping range or the entire ledger rejects. *)
      let address =
        match get (N.frame_displacements normalized) with
        | [ address ] -> address
        | _ -> raise Unproved
      in
      require (address >= displacement && address < Int64.add displacement bytes);
      let canceled =
        kind <> Address
        &&
        match normalized with
        | N.Add (N.Frame, N.Immediate n) -> n = displacement
        | _ -> false
      in
      accesses :=
        {
          location;
          span = source_span (Typed.result_origin base);
          kind = Array_element canceled;
        }
        :: !accesses
    in
    let value ~write root =
      expression 32 ~write ~inside_comparison:false root
    in
    let budget = ref 1024 in
    let rec statement depth = function
      | _ when depth = 0 || !budget = 0 -> raise Unproved
      | node -> (
          decr budget;
          let statement = statement (depth - 1) in
          match node with
          | Lower.Empty _ | Break _ -> ()
          | Block children -> List.iter statement children
          | Expression root -> value ~write:true root
          | Return root ->
              Option.iter (value ~write:false) (Typed.return_value root)
          | Initialize root ->
              value ~write:false (Typed.initializer_value root);
              let local =
                Typed.initializer_source root |> Source.initializer_local
              in
              let location =
                get
                  (List.find_opt
                     (fun location ->
                       Frame.location_symbol location
                       == Typed.initializer_target_symbol root)
                     locations)
              in
              (match Frame_address_lowering.prepare_initializer ~frame root with
              | Ok (Some _) -> ()
              | _ -> raise Unproved);
              initialized := location :: !initialized;
              accesses :=
                {
                  location;
                  span =
                    source_span (Sema.Symbol.origin (Local.local_symbol local));
                  kind = Write;
                }
                :: !accesses
          | If (condition, yes, no) ->
              value ~write:false condition;
              statement yes;
              Option.iter statement no
          | While (condition, body) ->
              value ~write:false condition;
              statement body
          | Do_while (body, condition) ->
              statement body;
              value ~write:false condition
          | For (initial, condition, update, body) ->
              statement initial;
              value ~write:false condition;
              statement body;
              Option.iter statement update
          | _ -> raise Unproved)
    in
    List.iter (statement 32) statements;
    require
      (List.length !seen = List.length (Typed.function_all_results source)
      && List.length !seen_calls = List.length (Typed.function_calls source)
      && List.for_all
           (fun location ->
             owned
             || Frame.location_kind location = Frame.Named_parameter
             || List.exists (( == ) location) !initialized)
           locations);
    (* The canonical lowerer retains every original block, including statements
       after returns: ensure_open opens another block, finish records it, and
       graph construction consumes the entire reversed block list. Conditions
       do not prune constant-dead arms. The x87 verifier retains that graph.
       Verify this conservation invariant here, never use reachable blocks.
       A future DCE lowerer must supply an explicit phase-deletion receipt. *)
    let blocks = Block_graph.blocks (Function_body.body body) in
    let code = List.concat_map descriptions blocks in
    require (List.length blocks <= 64 && List.length code <= 512);
    let produced =
      List.fold_left
        (fun map (item : Seq.description) ->
          match item.result with
          | None -> map
          | Some result -> Values.add result.value_id item map)
        Values.empty code
    in
    let users id =
      List.filter
        (fun (item : Seq.description) ->
          List.exists (Seq.Value_id.equal id) item.operands)
        code
    in
    let sole = function
      | [ item ] -> item
      | _ -> raise Unproved
    in
    let remaining = ref !accesses in
    List.iter
      (fun (item : Seq.description) ->
        if item.opcode = O.Ic_rbp then (
          let rbp = (get item.result).Seq.value_id in
          let address = sole (users rbp) in
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
          let consumer = sole (users (get address.result).value_id) in
          let span = get address.span in
          let access =
            sole
              (List.filter
                 (fun access ->
                   access.span = span
                   && Frame.frame_slot_displacement
                        (get (Frame.location_frame_slot access.location))
                      = displacement)
                 !remaining)
          in
          (match access.kind with
          | Array_element _ ->
              (* Hosted checked indexing keeps the base and scaled addition
                 separate. The complete canonical graph proves that chain;
                 it must not be mistaken for a scalar base read. *)
              require (owned && consumer.opcode = O.Ic_add)
          | kind ->
              let actual =
                if consumer.opcode = O.Ic_deref then Read
                else if owned && consumer.opcode = O.Ic_addr then Address
                else (
                  require (write_opcode consumer.opcode);
                  Write)
              in
              require (kind = actual));
          require (item.span = Some span && offset.span = Some span);
          remaining :=
            List.filter (fun candidate -> candidate != access) !remaining))
      code;
    require (!remaining = []);
    Some
      (List.map
         (fun location ->
           ( location,
             List.filter (fun access -> access.location == location) !accesses
           ))
         locations)
  with Unproved -> None

let analyze_whole ~frame ~body ~statements =
  Option.map
    (List.map (fun (location, accesses) -> (location, List.length accesses)))
    (analyze_accesses ~frame ~body ~statements ())

type member_score = {
  location : Frame.location;
  score : int64;
  lea_balance : int64;
}

let analyze_members ~frame ~body ~statements =
  Option.map
    (List.map (fun (location, accesses) ->
         let address_count =
           List.fold_left
             (fun count access ->
               if access.kind = Address || access.kind = Array_element false
               then count + 1
               else count)
             0 accesses
         in
         {
           location;
           score = Int64.of_int ((2 * List.length accesses) - address_count);
           lea_balance = Int64.of_int address_count;
         }))
    (analyze_accesses ~owned:true ~frame ~body ~statements ())
