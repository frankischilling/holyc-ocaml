type t = { symbol : Sema.Symbol.t; identity : unit ref }

let create symbol = { symbol; identity = ref () }
let symbol reference = reference.symbol
let same left right = left.identity == right.identity
