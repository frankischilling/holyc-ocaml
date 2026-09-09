let equal = Closed_numeric_expression.equal ~equal_query:( == )
let unary = Closed_numeric_expression.unary
let binary = Closed_numeric_expression.binary

let of_ast ?allow_floating ?(queries = []) ast =
  Closed_numeric_expression.of_ast ?allow_floating
    ~query_expression:Query_selection.expression ~queries ast
