module Symbol = Sema.Symbol
module Stored = Sema.Function_flag.Stored
module Option_ = Sema.Compiler_option
module Int_set = Set.Make (Int)

type error = {
  code : string;
  message : string;
  function_id : int option;
  symbol_id : int option;
  position : int option;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

let make_error ?function_id ?symbol_id ?position ?block_id ?instruction_id ?span
    code message =
  {
    code;
    message;
    function_id;
    symbol_id;
    position;
    block_id;
    instruction_id;
    span;
  }

module Function_id = struct
  type t = int

  let of_int value =
    if value < 0 then
      Error
        (make_error "HCIR0026"
           (Printf.sprintf "function ID %d must be nonnegative" value))
    else Ok value

  let to_int value = value
  let compare = Int.compare
  let equal = Int.equal
end

type member_description = {
  position : int;
  symbol : Symbol.t;
  type_ : Sema.Type.t;
  span : Common.Span.t option;
}

type description = {
  function_id : Function_id.t;
  symbol : Symbol.t;
  function_scope : Symbol.Scope_id.t;
  return_type : Sema.Type.t;
  parameters : member_description list;
  locals : member_description list;
  stored_flags : int64;
  compiler_options : int64;
  span : Common.Span.t option;
  body : Block_graph.t;
}

type member = member_description

type t = {
  function_id_ : Function_id.t;
  symbol_ : Symbol.t;
  function_scope_ : Symbol.Scope_id.t;
  return_type_ : Sema.Type.t;
  parameters_ : member list;
  locals_ : member list;
  stored_flags_ : int64;
  compiler_options_ : int64;
  span_ : Common.Span.t option;
  body_ : Block_graph.t;
  x87_ : X87_stack.t;
}

let reference_commit = Instruction_sequence.reference_commit

let known_stored_flag_mask =
  List.fold_left
    (fun mask flag -> Int64.logor mask (Stored.to_mask flag))
    0L Stored.all

let known_compiler_option_mask =
  List.fold_left
    (fun mask option -> Int64.logor mask (Option_.mask option))
    0L Option_.all

let function_number description = Function_id.to_int description.function_id
let symbol_number symbol = Symbol.Id.to_int (Symbol.id symbol)
let scope_number symbol = Symbol.Scope_id.to_int (Symbol.scope_id symbol)

let invalid_span = function
  | Some (span : Common.Span.t) -> span.start < 0 || span.stop < span.start
  | None -> false

let function_errors description =
  let errors = ref [] in
  let add ?symbol_id ?span code message =
    errors :=
      make_error
        ~function_id:(function_number description)
        ?symbol_id ?span code message
      :: !errors
  in
  let symbol = description.symbol in
  let symbol_id = symbol_number symbol in
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    add ~symbol_id "HCIR0027"
      (Printf.sprintf "function ^f%d uses %s symbol @s%d instead of a function"
         (function_number description)
         (Symbol.kind_name (Symbol.kind symbol))
         symbol_id)
  else ();
  if Symbol.Scope_id.equal (Symbol.scope_id symbol) description.function_scope
  then
    add ~symbol_id "HCIR0027"
      (Printf.sprintf
         "function ^f%d reuses declaration scope ^s%d as its body scope"
         (function_number description)
         (Symbol.Scope_id.to_int description.function_scope))
  else ();
  if invalid_span description.span then
    let span = Option.get description.span in
    add ~symbol_id ~span "HCIR0028"
      (Printf.sprintf "function ^f%d has invalid source span %d..%d"
         (function_number description)
         span.start span.stop)
  else ();
  let unknown_flags =
    Int64.logand description.stored_flags (Int64.lognot known_stored_flag_mask)
  in
  if unknown_flags <> 0L then
    add ~symbol_id "HCIR0029"
      (Printf.sprintf "function ^f%d uses unsupported stored flag bits 0x%Lx"
         (function_number description)
         unknown_flags)
  else ();
  let unknown_options =
    Int64.logand description.compiler_options
      (Int64.lognot known_compiler_option_mask)
  in
  if unknown_options <> 0L then
    add ~symbol_id "HCIR0029"
      (Printf.sprintf
         "function ^f%d uses unsupported compiler option bits 0x%Lx"
         (function_number description)
         unknown_options)
  else ();
  let has flag = Stored.is_set ~mask:description.stored_flags flag in
  if has Stored.Argument_pop && has Stored.No_argument_pop then
    add ~symbol_id "HCIR0030"
      (Printf.sprintf
         "function ^f%d requests both argument-pop and no-argument-pop cleanup"
         (function_number description))
  else ();
  if has Stored.Has_error_code && not (has Stored.Interrupt) then
    add ~symbol_id "HCIR0030"
      (Printf.sprintf
         "function ^f%d requests an interrupt error code without interrupt \
          entry"
         (function_number description))
  else ();
  List.rev !errors

let member_errors description =
  let errors = ref [] in
  let seen_symbols = ref Int_set.empty in
  let add (member : member_description) ?position code message =
    errors :=
      make_error
        ~function_id:(function_number description)
        ~symbol_id:(symbol_number member.symbol)
        ?position ?span:member.span code message
      :: !errors
  in
  let validate_group label expected_kind (members : member_description list) =
    let positions = ref Int_set.empty in
    List.iter
      (fun (member : member_description) ->
        let symbol_id = symbol_number member.symbol in
        if member.position < 0 then
          add member ~position:member.position "HCIR0031"
            (Printf.sprintf "%s @s%d has negative position %d" label symbol_id
               member.position)
        else ();
        if Int_set.mem member.position !positions then
          add member ~position:member.position "HCIR0033"
            (Printf.sprintf "%s position %d is defined more than once" label
               member.position)
        else positions := Int_set.add member.position !positions;
        if Int_set.mem symbol_id !seen_symbols then
          add member ~position:member.position "HCIR0033"
            (Printf.sprintf
               "symbol @s%d appears more than once in function ^f%d" symbol_id
               (function_number description))
        else seen_symbols := Int_set.add symbol_id !seen_symbols;
        if not (Symbol.equal_kind (Symbol.kind member.symbol) expected_kind)
        then
          add member ~position:member.position "HCIR0032"
            (Printf.sprintf "%s position %d uses %s symbol @s%d" label
               member.position
               (Symbol.kind_name (Symbol.kind member.symbol))
               symbol_id)
        else ();
        if
          not
            (Symbol.Scope_id.equal
               (Symbol.scope_id member.symbol)
               description.function_scope)
        then
          add member ~position:member.position "HCIR0032"
            (Printf.sprintf
               "%s @s%d belongs to scope ^s%d, not function scope ^s%d" label
               symbol_id
               (scope_number member.symbol)
               (Symbol.Scope_id.to_int description.function_scope))
        else ();
        if invalid_span member.span then
          let span = Option.get member.span in
          add member ~position:member.position "HCIR0028"
            (Printf.sprintf "%s @s%d has invalid source span %d..%d" label
               symbol_id span.start span.stop)
        else ())
      members
  in
  validate_group "parameter" Symbol.Parameter description.parameters;
  validate_group "local" Symbol.Local_variable description.locals;
  List.rev !errors

let x87_error description (error : X87_stack.error) =
  make_error
    ~function_id:(function_number description)
    ?block_id:error.block_id ?instruction_id:error.instruction_id
    ?span:error.span error.code error.message

let create description =
  let metadata_errors =
    function_errors description @ member_errors description
  in
  match X87_stack.verify description.body with
  | Error body_errors ->
      Error (metadata_errors @ List.map (x87_error description) body_errors)
  | Ok x87 -> (
      match metadata_errors with
      | _ :: _ -> Error metadata_errors
      | [] ->
          Ok
            {
              function_id_ = description.function_id;
              symbol_ = description.symbol;
              function_scope_ = description.function_scope;
              return_type_ = description.return_type;
              parameters_ = description.parameters;
              locals_ = description.locals;
              stored_flags_ = description.stored_flags;
              compiler_options_ = description.compiler_options;
              span_ = description.span;
              body_ = description.body;
              x87_ = x87;
            })

let function_id function_ = function_.function_id_
let symbol function_ = function_.symbol_
let function_scope function_ = function_.function_scope_
let return_type function_ = function_.return_type_
let parameters function_ = function_.parameters_
let locals function_ = function_.locals_
let stored_flags function_ = function_.stored_flags_
let compiler_options function_ = function_.compiler_options_
let span function_ = function_.span_
let body function_ = function_.body_
let x87 function_ = function_.x87_
let member_position (member : member) = member.position
let member_symbol (member : member) = member.symbol
let member_type (member : member) = member.type_
let member_span (member : member) = member.span

let add_quoted buffer text =
  Buffer.add_char buffer '"';
  String.iter
    (fun character ->
      match character with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | character -> Buffer.add_char buffer character)
    text;
  Buffer.add_char buffer '"'

let add_names buffer names =
  Buffer.add_char buffer '[';
  List.iteri
    (fun index name ->
      if index > 0 then Buffer.add_char buffer ',';
      Buffer.add_string buffer name)
    names;
  Buffer.add_char buffer ']'

let stored_flag_names mask =
  Stored.all
  |> List.filter (Stored.is_set ~mask)
  |> List.map Stored.to_source_name

let compiler_option_names mask =
  Option_.all
  |> List.filter (Option_.is_enabled ~mask)
  |> List.map Option_.to_string

let add_span buffer = function
  | None -> ()
  | Some (span : Common.Span.t) ->
      Printf.bprintf buffer " @source=%d:%d..%d"
        (Common.Source_id.to_int span.source)
        span.start span.stop

let add_member buffer label member =
  Printf.bprintf buffer "%s position=%d symbol=@s%d name=" label member.position
    (symbol_number member.symbol);
  add_quoted buffer (Symbol.name member.symbol);
  Printf.bprintf buffer " type=%s scope=^s%d"
    (Instruction_sequence.type_name member.type_)
    (scope_number member.symbol);
  add_span buffer member.span;
  Buffer.add_char buffer '\n'

let human function_ =
  let buffer = Buffer.create 1024 in
  Printf.bprintf buffer "holyc-ir-function-v1 reference=%s\n" reference_commit;
  Printf.bprintf buffer "function ^f%d symbol=@s%d name="
    (Function_id.to_int function_.function_id_)
    (symbol_number function_.symbol_);
  add_quoted buffer (Symbol.name function_.symbol_);
  Printf.bprintf buffer " declaration-scope=^s%d body-scope=^s%d\n"
    (scope_number function_.symbol_)
    (Symbol.Scope_id.to_int function_.function_scope_);
  Printf.bprintf buffer "return=%s flags=0x%Lx "
    (Instruction_sequence.type_name function_.return_type_)
    function_.stored_flags_;
  add_names buffer (stored_flag_names function_.stored_flags_);
  Printf.bprintf buffer " options=0x%Lx " function_.compiler_options_;
  add_names buffer (compiler_option_names function_.compiler_options_);
  add_span buffer function_.span_;
  Buffer.add_char buffer '\n';
  Printf.bprintf buffer "parameters=%d\n" (List.length function_.parameters_);
  List.iter (add_member buffer "parameter") function_.parameters_;
  Printf.bprintf buffer "locals=%d\n" (List.length function_.locals_);
  List.iter (add_member buffer "local") function_.locals_;
  Buffer.add_string buffer "body\n";
  Buffer.add_string buffer (Block_graph.human_body function_.body_);
  Buffer.add_string buffer "x87=verified\n";
  Buffer.contents buffer
