module Make (Query : sig
  type t

  val expression : t -> Frontend.Ast.expression
  val constant : t -> int64 option
  val owns_table : t -> Symbol_table.t -> bool
end) =
struct
  type unary_operator = Closed_numeric_expression.unary_operator =
    | Identity
    | Negate
    | Logical_not
    | Bitwise_not

  type binary_operator = Closed_numeric_expression.binary_operator =
    | Power
    | Shift_left
    | Shift_right
    | Multiply
    | Divide
    | Modulo
    | Bit_and
    | Bit_xor
    | Bit_or
    | Add
    | Subtract
    | Less
    | Greater
    | Less_equal
    | Greater_equal
    | Equal
    | Not_equal
    | Logical_and
    | Logical_xor
    | Logical_or

  type dependency_kind = Closed_numeric_expression.dependency_kind =
    | Identifier_dependency
    | Sizeof_dependency
    | Offset_dependency
    | Defined_dependency
    | Call_dependency
    | Aggregate_dependency

  type 'query generic_expression = 'query Closed_numeric_expression.expression =
    | Selected_query_expression of 'query
    | Integer_expression of { value : int64; origin : Symbol.origin }
    | Unsigned_integer_expression of { value : int64; origin : Symbol.origin }
    | Floating_expression of { value : float; origin : Symbol.origin }
    | Current_position_expression of Symbol.origin
    | Unary_expression of {
        operator : unary_operator;
        operand : 'query generic_expression;
        origin : Symbol.origin;
      }
    | Binary_expression of {
        operator : binary_operator;
        left : 'query generic_expression;
        right : 'query generic_expression;
        origin : Symbol.origin;
      }
    | Dependency_expression of {
        dependency_kind : dependency_kind;
        detail : string;
        origin : Symbol.origin;
      }
    | Unsupported_expression of { description : string; origin : Symbol.origin }

  type expression = Query.t generic_expression

  type expression_context = Closed_numeric_expression.expression_context =
    | Array_dimension
    | Aggregate_offset

  type dimension = {
    dimension_expression : expression option;
    dimension_origin : Symbol.origin;
  }

  type member_input = {
    member_symbol : Symbol.t;
    member_path : int list;
    member_declarator_index : int;
    member_origin : Symbol.origin;
    member_type : Type.t;
    member_is_function_pointer : bool;
    member_dimensions : dimension list;
  }

  type item =
    | Field of member_input
    | Offset_directive of expression
    | Anonymous_union of {
        union_origin : Symbol.origin;
        union_items : item list;
      }
    | Empty_member of Symbol.origin

  type aggregate_kind = Class | Union
  type base_input = { base_symbol : Symbol.t; base_origin : Symbol.origin }

  type aggregate_input = {
    aggregate_symbol : Symbol.t;
    aggregate_scope : Symbol_table.scope;
    aggregate_kind : aggregate_kind;
    aggregate_item_index : int;
    aggregate_origin : Symbol.origin;
    aggregate_base : base_input option;
    aggregate_items : item list;
  }

  type signedness = Signed | Unsigned | Not_applicable

  type member_layout = {
    symbol : Symbol.t;
    path : int list;
    declarator_index : int;
    origin : Symbol.origin;
    offset : int64;
    size : int64;
    element_size : int64;
    dimensions : int64 list;
    signedness : signedness;
    alignment : int;
  }

  type base_layout = {
    symbol : Symbol.t;
    origin : Symbol.origin;
    offset : int64;
    size : int64;
  }

  type aggregate_layout = {
    symbol : Symbol.t;
    kind : aggregate_kind;
    item_index : int;
    origin : Symbol.origin;
    size : int64;
    alignment : int;
    negative_offset : int64;
    base : base_layout option;
    members : member_layout list;
  }

  module Int_map = Map.Make (Int)
  module Int_set = Set.Make (Int)

  type t = {
    table : Symbol_table.t;
    layouts : aggregate_layout list;
    by_symbol : aggregate_layout Int_map.t;
  }

  type error_kind = Closed_numeric_expression.error_kind =
    | Invalid_input of string
    | Unresolved_dependency of {
        dependency_kind : dependency_kind;
        detail : string;
      }
    | Invalid_array_dimension of int64
    | Division_by_zero
    | Signed_division_overflow
    | Non_finite_layout_value
    | Numeric_conversion_overflow
    | Metadata_overflow of string
    | Invalid_layout_expression of string

  type error = Closed_numeric_expression.error

  let dependency_kind_name = Closed_numeric_expression.dependency_kind_name
  let invalid_input = Closed_numeric_expression.invalid_input
  let unresolved = Closed_numeric_expression.unresolved
  let invalid_dimension = Closed_numeric_expression.invalid_dimension
  let metadata_overflow = Closed_numeric_expression.metadata_overflow
  let error_code = Closed_numeric_expression.error_code
  let error_kind = Closed_numeric_expression.error_kind
  let error_origin = Closed_numeric_expression.error_origin
  let error_message = Closed_numeric_expression.error_message
  let error_to_string = Closed_numeric_expression.error_to_string

  let signedness_name = function
    | Signed -> "signed"
    | Unsigned -> "unsigned"
    | Not_applicable -> "not-applicable"

  let query_origin query =
    Closed_numeric_expression.origin
      (Frontend.Ast.expression_location (Query.expression query))

  let expression_origin =
    Closed_numeric_expression.expression_origin ~query_origin

  let evaluate_expression ~context ~current_position expression =
    Closed_numeric_expression.evaluate_expression ~query_origin
      ~query_value:Query.constant ~context ~current_position expression

  let symbol_key symbol = Symbol.id symbol |> Symbol.Id.to_int

  let same_scope left right =
    Symbol.Scope_id.equal
      (Symbol_table.scope_id left)
      (Symbol_table.scope_id right)

  let checked_add origin detail left right =
    if
      Int64.compare right 0L > 0
      && Int64.compare left (Int64.sub Int64.max_int right) > 0
      || Int64.compare right 0L < 0
         && Int64.compare left (Int64.sub Int64.min_int right) < 0
    then Error (metadata_overflow origin detail)
    else Ok (Int64.add left right)

  let checked_multiply_nonnegative origin detail left right =
    if Int64.compare left 0L < 0 || Int64.compare right 0L < 0 then
      Error (invalid_input ~origin (detail ^ " cannot be negative"))
    else if
      (not (Int64.equal right 0L))
      && Int64.compare left (Int64.div Int64.max_int right) > 0
    then Error (metadata_overflow origin detail)
    else Ok (Int64.mul left right)

  let checked_negative_magnitude origin value =
    if Int64.equal value Int64.min_int then
      Error (metadata_overflow origin "the negative member displacement")
    else Ok (Int64.neg value)

  let primitive_signedness primitive =
    match (Primitive_type.info primitive).signedness with
    | Primitive_type.Signed -> Signed
    | Primitive_type.Unsigned -> Unsigned
    | Primitive_type.Not_applicable -> Not_applicable

  let element_shape (previous : aggregate_layout Int_map.t)
      (member : member_input) =
    if
      member.member_is_function_pointer
      || Type.pointer_depth member.member_type > 0
    then Ok (8L, Unsigned)
    else
      match Type.base member.member_type with
      | Type.Primitive (_, primitive) ->
          Ok
            ( Int64.of_int (Primitive_type.info primitive).byte_size,
              primitive_signedness primitive )
      | Type.Aggregate symbol -> (
          match Int_map.find_opt (symbol_key symbol) previous with
          | Some layout -> Ok (layout.size, Not_applicable)
          | None ->
              Error
                (unresolved member.member_origin Aggregate_dependency
                   (Symbol.name symbol)))

  let evaluate_dimensions current_position dimensions =
    let rec loop index total values_rev = function
      | [] -> Ok (total, List.rev values_rev)
      | { dimension_expression; dimension_origin } :: rest ->
          let value =
            match dimension_expression with
            | None when index = 0 -> Ok 0L
            | None ->
                Error
                  (invalid_input ~origin:dimension_origin
                     "only the first array dimension can be empty")
            | Some expression ->
                evaluate_expression ~context:Array_dimension ~current_position
                  expression
          in
          Result.bind value (fun value ->
              if Int64.compare value 0L < 0 then
                Error (invalid_dimension dimension_origin value)
              else
                Result.bind
                  (checked_multiply_nonnegative dimension_origin
                     "the array element count" total value) (fun total ->
                    loop (index + 1) total (value :: values_rev) rest))
    in
    loop 0 1L [] dimensions

  type layout_state = {
    size : int64;
    negative_offset : int64;
    members_rev : member_layout list;
  }

  let update_negative_offset origin state position =
    if Int64.compare position 0L >= 0 then Ok state
    else
      Result.map
        (fun magnitude ->
          {
            state with
            negative_offset = Int64.max state.negative_offset magnitude;
          })
        (checked_negative_magnitude origin position)

  let place_member ~origin ~kind ~union_base ~current_size ~member_size =
    if Int64.compare member_size 0L < 0 then
      Error (invalid_input ~origin "the member storage size cannot be negative")
    else
      match kind with
      | Class -> checked_add origin "the class size" current_size member_size
      | Union ->
          Result.map (Int64.max current_size)
            (checked_add origin "the union member end" union_base member_size)

  let member_extent ~origin ~element_size ~counts =
    let count =
      List.fold_left
        (fun result count ->
          Result.bind result (fun size ->
              checked_multiply_nonnegative origin "the array element count" size
                count))
        (Ok 1L) counts
    in
    Result.bind count (fun count ->
        checked_multiply_nonnegative origin "the member storage size"
          element_size count)

  let lay_out_member (previous : aggregate_layout Int_map.t) mode union_base
      state member =
    let current_position =
      match mode with
      | Class -> state.size
      | Union -> union_base
    in
    Result.bind (element_shape previous member)
      (fun (element_size, signedness) ->
        Result.bind
          (evaluate_dimensions current_position member.member_dimensions)
          (fun (total_count, dimensions) ->
            Result.bind
              (checked_multiply_nonnegative member.member_origin
                 "the member storage size" element_size total_count)
              (fun size ->
                let offset = current_position in
                let placed_size =
                  place_member ~origin:member.member_origin ~kind:mode
                    ~union_base ~current_size:state.size ~member_size:size
                in
                Result.map
                  (fun aggregate_size ->
                    let layout =
                      {
                        symbol = member.member_symbol;
                        path = member.member_path;
                        declarator_index = member.member_declarator_index;
                        origin = member.member_origin;
                        offset;
                        size;
                        element_size;
                        dimensions;
                        signedness;
                        alignment = 1;
                      }
                    in
                    {
                      state with
                      size = aggregate_size;
                      members_rev = layout :: state.members_rev;
                    })
                  placed_size)))

  let rec lay_out_items (previous : aggregate_layout Int_map.t) mode union_base
      state items =
    match items with
    | [] -> Ok state
    | item :: rest -> (
        match item with
        | Empty_member _ -> lay_out_items previous mode union_base state rest
        | Field member ->
            Result.bind (lay_out_member previous mode union_base state member)
              (fun state -> lay_out_items previous mode union_base state rest)
        | Offset_directive expression ->
            let current_position =
              match mode with
              | Class -> state.size
              | Union -> union_base
            in
            Result.bind
              (evaluate_expression ~context:Aggregate_offset ~current_position
                 expression) (fun position ->
                Result.bind
                  (update_negative_offset
                     (expression_origin expression)
                     state position)
                  (fun state ->
                    match mode with
                    | Class ->
                        lay_out_items previous mode union_base
                          { state with size = position }
                          rest
                    | Union -> lay_out_items previous mode position state rest))
        | Anonymous_union { union_items; _ } ->
            Result.bind
              (lay_out_items previous Union state.size state union_items)
              (fun state -> lay_out_items previous mode union_base state rest))

  let validate_member table aggregate_scope seen (member : member_input) =
    let key = symbol_key member.member_symbol in
    if not (Symbol_table.owns_symbol table member.member_symbol) then
      Error
        (invalid_input ~origin:member.member_origin
           "aggregate member belongs to a different symbol table")
    else if
      not (Symbol.equal_kind (Symbol.kind member.member_symbol) Symbol.Member)
    then
      Error
        (invalid_input ~origin:member.member_origin
           "aggregate layout received a symbol that is not a member")
    else if
      not
        (Symbol.Scope_id.equal
           (Symbol.scope_id member.member_symbol)
           (Symbol_table.scope_id aggregate_scope))
    then
      Error
        (invalid_input ~origin:member.member_origin
           "aggregate member belongs to the wrong aggregate scope")
    else if member.member_path = [] then
      Error
        (invalid_input ~origin:member.member_origin
           "aggregate member path cannot be empty")
    else if List.exists (fun index -> index < 0) member.member_path then
      Error
        (invalid_input ~origin:member.member_origin
           "aggregate member path cannot contain a negative index")
    else if member.member_declarator_index < 0 then
      Error
        (invalid_input ~origin:member.member_origin
           "aggregate member declarator index cannot be negative")
    else if Int_set.mem key seen then
      Error
        (invalid_input ~origin:member.member_origin
           "aggregate layout received the same member symbol twice")
    else
      match Type.base member.member_type with
      | Type.Aggregate symbol when not (Symbol_table.owns_symbol table symbol)
        ->
          Error
            (invalid_input ~origin:member.member_origin
               "aggregate member type belongs to a different symbol table")
      | Type.Aggregate _ | Type.Primitive _ -> Ok (Int_set.add key seen)

  let rec validate_expression_table table expression =
    match expression with
    | Selected_query_expression query ->
        if Query.owns_table query table then Ok ()
        else
          Error
            (invalid_input
               ~origin:(expression_origin expression)
               "aggregate layout query belongs to another semantic table")
    | Unary_expression { operand; _ } -> validate_expression_table table operand
    | Binary_expression { left; right; _ } ->
        Result.bind (validate_expression_table table left) (fun () ->
            validate_expression_table table right)
    | Integer_expression _
    | Unsigned_integer_expression _
    | Floating_expression _
    | Current_position_expression _
    | Dependency_expression _
    | Unsupported_expression _ -> Ok ()

  let rec validate_dimension_tables table = function
    | [] -> Ok ()
    | dimension :: rest ->
        let checked =
          match dimension.dimension_expression with
          | None -> Ok ()
          | Some expression -> validate_expression_table table expression
        in
        Result.bind checked (fun () -> validate_dimension_tables table rest)

  let rec validate_items table scope seen = function
    | [] -> Ok seen
    | Empty_member _ :: rest -> validate_items table scope seen rest
    | Offset_directive expression :: rest ->
        Result.bind (validate_expression_table table expression) (fun () ->
            validate_items table scope seen rest)
    | Field member :: rest ->
        Result.bind (validate_member table scope seen member) (fun seen ->
            Result.bind
              (validate_dimension_tables table member.member_dimensions)
              (fun () -> validate_items table scope seen rest))
    | Anonymous_union { union_items; _ } :: rest ->
        Result.bind (validate_items table scope seen union_items) (fun seen ->
            validate_items table scope seen rest)

  let validate_aggregate table parent previous_item_index seen_symbols input =
    let key = symbol_key input.aggregate_symbol in
    if not (Symbol_table.owns_symbol table input.aggregate_symbol) then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate belongs to a different symbol table")
    else if
      not
        (Symbol.equal_kind
           (Symbol.kind input.aggregate_symbol)
           Symbol.Aggregate_type)
    then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate layout received a symbol that is not an aggregate type")
    else if
      not
        (Symbol.Scope_id.equal
           (Symbol.scope_id input.aggregate_symbol)
           (Symbol_table.scope_id parent))
    then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate does not belong to the module scope")
    else if not (Symbol_table.owns_scope table input.aggregate_scope) then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate scope belongs to a different symbol table")
    else if
      Symbol_table.scope_kind input.aggregate_scope <> Symbol_table.Aggregate
    then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate layout needs an aggregate scope")
    else if
      match Symbol_table.parent input.aggregate_scope with
      | Some scope -> not (same_scope scope parent)
      | None -> true
    then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate scope does not belong to the module")
    else if input.aggregate_item_index <= previous_item_index then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate layouts must follow source order")
    else if Int_set.mem key seen_symbols then
      Error
        (invalid_input ~origin:input.aggregate_origin
           "aggregate layout received the same aggregate symbol twice")
    else
      match input.aggregate_base with
      | Some base
        when (not (Symbol_table.owns_symbol table base.base_symbol))
             || not
                  (Symbol.equal_kind
                     (Symbol.kind base.base_symbol)
                     Symbol.Aggregate_type) ->
          Error
            (invalid_input ~origin:base.base_origin
               "aggregate base is not an aggregate type from this symbol table")
      | None | Some _ ->
          Result.map
            (fun _ ->
              (input.aggregate_item_index, Int_set.add key seen_symbols))
            (validate_items table input.aggregate_scope Int_set.empty
               input.aggregate_items)

  let validate_inputs table parent inputs =
    if not (Symbol_table.owns_scope table parent) then
      Error
        (invalid_input "aggregate layout module belongs to another symbol table")
    else if Symbol_table.scope_kind parent <> Symbol_table.Module then
      Error (invalid_input "aggregate layout parent must be a module scope")
    else
      let rec loop previous_item_index seen_symbols = function
        | [] -> Ok ()
        | input :: rest ->
            Result.bind
              (validate_aggregate table parent previous_item_index seen_symbols
                 input) (fun (item_index, seen_symbols) ->
                loop item_index seen_symbols rest)
      in
      loop (-1) Int_set.empty inputs

  let base_layout (previous : aggregate_layout Int_map.t)
      (input : aggregate_input) =
    match input.aggregate_base with
    | None -> Ok None
    | Some base -> (
        match Int_map.find_opt (symbol_key base.base_symbol) previous with
        | None ->
            Error
              (unresolved base.base_origin Aggregate_dependency
                 (Symbol.name base.base_symbol))
        | Some layout ->
            Ok
              (Some
                 {
                   symbol = base.base_symbol;
                   origin = base.base_origin;
                   offset = 0L;
                   size = layout.size;
                 }))

  let lay_out_aggregate (previous : aggregate_layout Int_map.t)
      (input : aggregate_input) =
    Result.bind (base_layout previous input) (fun base ->
        let initial_size =
          match base with
          | None -> 0L
          | Some base -> base.size
        in
        let state =
          { size = initial_size; negative_offset = 0L; members_rev = [] }
        in
        let union_base = 0L in
        Result.bind
          (lay_out_items previous input.aggregate_kind union_base state
             input.aggregate_items) (fun state ->
            Result.map
              (fun size ->
                {
                  symbol = input.aggregate_symbol;
                  kind = input.aggregate_kind;
                  item_index = input.aggregate_item_index;
                  origin = input.aggregate_origin;
                  size;
                  alignment = 1;
                  negative_offset = state.negative_offset;
                  base;
                  members = List.rev state.members_rev;
                })
              (checked_add input.aggregate_origin "the final aggregate size"
                 state.size state.negative_offset)))

  let layout ~table ~parent inputs =
    Result.bind (validate_inputs table parent inputs) (fun () ->
        let rec loop by_symbol layouts_rev = function
          | [] -> Ok { table; layouts = List.rev layouts_rev; by_symbol }
          | input :: rest ->
              Result.bind (lay_out_aggregate by_symbol input) (fun layout ->
                  loop
                    (Int_map.add (symbol_key layout.symbol) layout by_symbol)
                    (layout :: layouts_rev) rest)
        in
        loop Int_map.empty [] inputs)

  let layouts result = result.layouts
  let owns_table result table = result.table == table

  let find result symbol =
    if not (Symbol_table.owns_symbol result.table symbol) then None
    else
      match Int_map.find_opt (symbol_key symbol) result.by_symbol with
      | Some layout when layout.symbol == symbol -> Some layout
      | Some _ | None -> None
end
