type role = Sizeof_root | Offset_root | Defined_operand

type t = {
  table : Symbol_table.t;
  receipt : Frontend.Parser.completed_query;
  role : role;
  name : string;
  origin : Symbol.origin;
  sizeof_read : Compiler_record.sizeof_read option;
}

let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let make ?sizeof_read ~table ~(receipt : Frontend.Parser.completed_query) () =
  let open Frontend in
  let root = receipt.query_root in
  if
    root.query_environment
    != Parser.context_environment root.query_command.command_context
  then Error "query selection has a foreign parser environment"
  else
    let facts =
      match (root.query_node, receipt.query_expression) with
      | Parser.Defined_target operand, Ast.Defined_expression expression
        when operand == expression.defined_operand ->
          Some
            ( Defined_operand,
              operand.defined_operand_spelling,
              origin operand.defined_operand_location )
      | Parser.Sizeof_target target, Ast.Sizeof_expression expression
        when target == expression.sizeof_target ->
          Some (Sizeof_root, target.spelling, origin target.location)
      | Parser.Offset_target target, Ast.Offset_expression expression
        when target == expression.offset_target ->
          Some (Offset_root, target.spelling, origin target.location)
      | _ -> None
    in
    match facts with
    | None -> Error "query selection does not retain its original AST root"
    | Some (role, name, origin) ->
        let checked =
          match sizeof_read with
          | None -> Ok ()
          | Some read -> Compiler_record.complete_sizeof ~table ~receipt read
        in
        Result.map
          (fun () -> { table; receipt; role; name; origin; sizeof_read })
          checked

let validate ~table ~role ~name ~origin selection =
  if selection.table != table then
    Error "query selection belongs to another symbol table"
  else if
    selection.role <> role || selection.name <> name
    || selection.origin <> origin
  then Error "query selection belongs to another source occurrence or role"
  else Ok ()

let expression selection = selection.receipt.query_expression
let owns_table selection table = selection.table == table

let is_local selection =
  match selection.receipt.query_root.query_lookup with
  | Frontend.Symbol_visibility.Shadowed_by_local -> true
  | Frontend.Symbol_visibility.Absent | Frontend.Symbol_visibility.Present _ ->
      false

let presence selection =
  match selection.role with
  | Defined_operand -> Some selection.receipt.query_root.query_present
  | Sizeof_root | Offset_root -> None

let sizeof selection =
  match (selection.sizeof_read, selection.receipt.query_expression) with
  | Some read, Frontend.Ast.Sizeof_expression expression ->
      let pointer = expression.sizeof_pointer_layers <> [] in
      Some
        ( Compiler_record.sizeof_primitive read,
          Compiler_record.sizeof_value read ~pointer,
          pointer )
  | _ -> None

let constant selection =
  match presence selection with
  | Some present -> Some (if present then 1L else 0L)
  | None -> sizeof selection |> Option.map (fun (_, value, _) -> value)

let rec source_queries expression =
  let open Frontend.Ast in
  match expression with
  | Sizeof_expression _ | Offset_expression _ | Defined_expression _ ->
      [ expression ]
  | Parenthesized_expression grouped ->
      source_queries grouped.grouped_expression
  | Prefix_expression prefix -> source_queries prefix.prefix_operand
  | Postfix_expression postfix -> source_queries postfix.postfix_operand
  | Postfix_cast_expression cast -> source_queries cast.cast_operand
  | Binary_expression binary ->
      source_queries binary.binary_left @ source_queries binary.binary_right
  | Index_expression index ->
      source_queries index.index_base @ source_queries index.index_value
  | Member_expression member -> source_queries member.member_base
  | Call_expression call ->
      source_queries call.call_callee
      @ List.concat_map
          (fun argument ->
            match argument.call_argument_value with
            | Omitted_call_argument -> []
            | Provided_call_argument value -> source_queries value)
          call.call_arguments
  | Integer_literal _
  | Float_literal _
  | Character_literal _
  | String_literal _
  | Current_position_expression _
  | Identifier_expression _ -> []

let validate_manifest ~table ~expression queries =
  let rec loop expressions queries =
    match (expressions, queries) with
    | [], [] -> Ok ()
    | expression :: expressions, query :: queries
      when query.table == table && query.receipt.query_expression == expression
      -> loop expressions queries
    | _ -> Error "query manifest lacks its exact ordered source reads or table"
  in
  loop (source_queries expression) queries

let name_query_facts = function
  | Frontend.Ast.Sizeof_expression expression ->
      Some
        ( Sizeof_root,
          expression.sizeof_target.spelling,
          origin expression.sizeof_target.location )
  | Frontend.Ast.Offset_expression expression ->
      Some
        ( Offset_root,
          expression.offset_target.spelling,
          origin expression.offset_target.location )
  | Frontend.Ast.Defined_expression expression
    when expression.defined_operand.defined_operand_kind
         = Frontend.Ast.Defined_name ->
      let operand = expression.defined_operand in
      Some
        ( Defined_operand,
          operand.defined_operand_spelling,
          origin operand.defined_operand_location )
  | _ -> None
