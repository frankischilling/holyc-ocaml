type binding_kind =
  | Named_parameter
  | Variadic_argc
  | Variadic_argv
  | Automatic_local
  | Static_local

type variadic_parameter = Argc | Argv
type local_storage = Automatic | Static

type binding_position =
  | Parameter_position of int
  | Local_position of { declaration_index : int; declarator_index : int }

type binding = {
  name : string;
  kind : binding_kind;
  origin : Symbol.origin;
  position : binding_position;
}

type function_declaration = {
  symbol : Symbol.t;
  item_index : int;
  bindings : binding list;
  completed_header : Frontend.Parser.completed_function_header option;
}

type entry = {
  symbol : Symbol.t;
  kind : binding_kind;
  position : binding_position;
}

type collected_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  entries : entry list;
  completed_header : Frontend.Parser.completed_function_header option;
  mutable header_reused : bool;
}

type t = { functions : collected_function list }

let functions collection = collection.functions
let function_symbol (function_ : collected_function) = function_.symbol
let function_scope (function_ : collected_function) = function_.scope
let function_item_index (function_ : collected_function) = function_.item_index
let function_entries (function_ : collected_function) = function_.entries

let function_completed_header (function_ : collected_function) =
  function_.completed_header

let entry_symbol (entry : entry) = entry.symbol
let entry_kind (entry : entry) = entry.kind

let entry_parameter_index (entry : entry) =
  match entry.position with
  | Parameter_position index -> Some index
  | Local_position _ -> None

let entry_local_declaration_index (entry : entry) =
  match entry.position with
  | Parameter_position _ -> None
  | Local_position { declaration_index; _ } -> Some declaration_index

let entry_declarator_index (entry : entry) =
  match entry.position with
  | Parameter_position _ -> None
  | Local_position { declarator_index; _ } -> Some declarator_index

let binding_kind_name = function
  | Named_parameter -> "named-parameter"
  | Variadic_argc -> "variadic-argc"
  | Variadic_argv -> "variadic-argv"
  | Automatic_local -> "automatic-local"
  | Static_local -> "static-local"

let check_name name =
  if String.equal name "" then
    Error "semantic function binding name cannot be empty"
  else Ok ()

let check_origin = function
  | Symbol.Pinned_source { path; line } ->
      if String.equal path "" then
        Error "pinned semantic symbol path cannot be empty"
      else if line < 1 then Error "pinned semantic symbol line must be positive"
      else Ok ()
  | Symbol.Source_location _ -> Ok ()
  | Symbol.Synthesized description ->
      if String.equal description "" then
        Error "synthesized semantic symbol origin cannot be empty"
      else Ok ()

let make_named_parameter ~name ~origin ~parameter_index =
  match check_name name with
  | Error _ as error -> error
  | Ok () -> (
      match check_origin origin with
      | Error _ as error -> error
      | Ok () ->
          if parameter_index < 0 then
            Error "semantic parameter index cannot be negative"
          else
            Ok
              {
                name;
                kind = Named_parameter;
                origin;
                position = Parameter_position parameter_index;
              })

let make_variadic_parameter parameter ~origin ~parameter_index =
  match check_origin origin with
  | Error _ as error -> error
  | Ok () ->
      if parameter_index < 0 then
        Error "semantic parameter index cannot be negative"
      else
        let name, kind =
          match parameter with
          | Argc -> ("argc", Variadic_argc)
          | Argv -> ("argv", Variadic_argv)
        in
        Ok { name; kind; origin; position = Parameter_position parameter_index }

let make_local ~name ~origin ~storage ~declaration_index ~declarator_index =
  match check_name name with
  | Error _ as error -> error
  | Ok () -> (
      match check_origin origin with
      | Error _ as error -> error
      | Ok () ->
          if declaration_index < 0 then
            Error "semantic local declaration index cannot be negative"
          else if declarator_index < 0 then
            Error "semantic local declarator index cannot be negative"
          else
            let kind =
              match storage with
              | Automatic -> Automatic_local
              | Static -> Static_local
            in
            Ok
              {
                name;
                kind;
                origin;
                position =
                  Local_position { declaration_index; declarator_index };
              })

let make_function ?completed_header ~symbol ~item_index bindings =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic function scope owner must be a function symbol"
  else if item_index < 0 then
    Error "semantic function item index cannot be negative"
  else Ok { symbol; item_index; bindings; completed_header }

type binding_order =
  | Parameters of { previous_index : int; expect_argv : bool; closed : bool }
  | Locals of { previous_declaration : int; previous_declarator : int }

let validate_parameter_order state (binding : binding) index =
  match state with
  | Locals _ -> Error "semantic parameters must precede local declarations"
  | Parameters { previous_index; expect_argv; closed } -> (
      if closed then Error "semantic fixed parameters cannot follow varargs"
      else if index <= previous_index then
        Error "semantic parameters must be in increasing source order"
      else
        match binding.kind with
        | Named_parameter ->
            if expect_argv then
              Error "semantic variadic argc must be followed by argv"
            else
              Ok
                (Parameters
                   {
                     previous_index = index;
                     expect_argv = false;
                     closed = false;
                   })
        | Variadic_argc ->
            if expect_argv then Error "semantic variadic argc cannot repeat"
            else
              Ok
                (Parameters
                   {
                     previous_index = index;
                     expect_argv = true;
                     closed = false;
                   })
        | Variadic_argv ->
            if (not expect_argv) || index <> previous_index + 1 then
              Error "semantic variadic argv must immediately follow argc"
            else
              Ok
                (Parameters
                   {
                     previous_index = index;
                     expect_argv = false;
                     closed = true;
                   })
        | Automatic_local | Static_local ->
            Error "semantic local has a parameter position")

let validate_local_order state (binding : binding) declaration_index
    declarator_index =
  match state with
  | Parameters { expect_argv = true; _ } ->
      Error "semantic variadic argc must be followed by argv"
  | Parameters _ ->
      if binding.kind <> Automatic_local && binding.kind <> Static_local then
        Error "semantic parameter has a local position"
      else
        Ok
          (Locals
             {
               previous_declaration = declaration_index;
               previous_declarator = declarator_index;
             })
  | Locals { previous_declaration; previous_declarator } ->
      if binding.kind <> Automatic_local && binding.kind <> Static_local then
        Error "semantic parameter has a local position"
      else if
        declaration_index < previous_declaration
        || declaration_index = previous_declaration
           && declarator_index <= previous_declarator
      then Error "semantic locals must be in increasing source order"
      else
        Ok
          (Locals
             {
               previous_declaration = declaration_index;
               previous_declarator = declarator_index;
             })

let validate_bindings (bindings : binding list) =
  let rec validate state (remaining : binding list) =
    match remaining with
    | [] -> (
        match state with
        | Parameters { expect_argv = true; _ } ->
            Error "semantic variadic argc must be followed by argv"
        | Parameters _ | Locals _ -> Ok ())
    | binding :: rest -> (
        let next =
          match binding.position with
          | Parameter_position index ->
              validate_parameter_order state binding index
          | Local_position { declaration_index; declarator_index } ->
              validate_local_order state binding declaration_index
                declarator_index
        in
        match next with
        | Error _ as error -> error
        | Ok next -> validate next rest)
  in
  validate
    (Parameters { previous_index = -1; expect_argv = false; closed = false })
    bindings

let validate_function table parent previous_item_index
    (function_ : function_declaration) =
  if not (Symbol_table.owns_symbol table function_.symbol) then
    Error "semantic function symbol belongs to a different symbol table"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id function_.symbol)
         (Symbol_table.scope_id parent))
  then Error "semantic function symbol does not belong to the module scope"
  else if function_.item_index <= previous_item_index then
    Error "semantic function declarations must be in increasing item order"
  else
    match validate_bindings function_.bindings with
    | Error _ as error -> error
    | Ok () -> Ok function_.item_index

let validate table parent functions =
  if not (Symbol_table.owns_scope table parent) then
    Error "semantic function parent belongs to a different symbol table"
  else if Symbol_table.scope_kind parent <> Symbol_table.Module then
    Error "semantic function parent must be a module scope"
  else
    let rec validate_all previous_item_index = function
      | [] -> Ok ()
      | function_ :: rest -> (
          match
            validate_function table parent previous_item_index function_
          with
          | Error _ as error -> error
          | Ok item_index -> validate_all item_index rest)
    in
    validate_all (-1) functions

let symbol_kind = function
  | Named_parameter | Variadic_argc | Variadic_argv -> Symbol.Parameter
  | Automatic_local | Static_local -> Symbol.Local_variable

let add_bindings table scope bindings =
  let rec add entries_rev = function
    | [] -> Ok (List.rev entries_rev)
    | binding :: rest -> (
        match
          Symbol_table.add table ~scope ~name:binding.name
            ~kind:(symbol_kind binding.kind) ~origin:binding.origin
        with
        | Error _ as error -> error
        | Ok symbol ->
            add
              ({ symbol; kind = binding.kind; position = binding.position }
              :: entries_rev)
              rest)
  in
  add [] bindings

let collect_function table parent (function_ : function_declaration) =
  match
    Symbol_table.create_scope table ~parent ~kind:Symbol_table.Function
      ~name:(Symbol.name function_.symbol)
      ()
  with
  | Error _ as error -> error
  | Ok scope -> (
      match add_bindings table scope function_.bindings with
      | Error _ as error -> error
      | Ok entries ->
          Ok
            {
              symbol = function_.symbol;
              scope;
              item_index = function_.item_index;
              entries;
              completed_header = function_.completed_header;
              header_reused = false;
            })

let same_symbol left right = left == right
let same_scope left right = left == right

let same_completed_header left right =
  match (left, right) with
  | Some left, Some right -> left == right
  | None, None -> true
  | Some _, None | None, Some _ -> false

let same_position left right =
  match (left, right) with
  | Parameter_position left, Parameter_position right -> left = right
  | ( Local_position
        { declaration_index = left_declaration; declarator_index = left_index },
      Local_position
        {
          declaration_index = right_declaration;
          declarator_index = right_index;
        } ) -> left_declaration = right_declaration && left_index = right_index
  | Parameter_position _, Local_position _
  | Local_position _, Parameter_position _ -> false

let entry_matches_binding (entry : entry) (binding : binding) =
  entry.kind = binding.kind
  && same_position entry.position binding.position
  && String.equal (Symbol.name entry.symbol) binding.name
  && Symbol.origin entry.symbol = binding.origin

let parameter_bindings (bindings : binding list) =
  let rec split (parameters_rev : binding list) (remaining : binding list) =
    match remaining with
    | (binding : binding) :: rest -> (
        match binding.position with
        | Parameter_position _ -> split (binding :: parameters_rev) rest
        | Local_position _ -> (List.rev parameters_rev, remaining))
    | [] -> (List.rev parameters_rev, [])
  in
  split [] bindings

let validate_retained_header ~table ~parent (retained : collected_function)
    (function_ : function_declaration) =
  let parameters, _ = parameter_bindings function_.bindings in
  if retained.header_reused then
    Error "semantic retained function collection was already completed"
  else if Option.is_none retained.completed_header then
    Error "semantic retained function collection is not a completed header"
  else if
    not
      (same_completed_header function_.completed_header
         retained.completed_header)
  then
    Error "semantic retained function collection has different source evidence"
  else if not (same_symbol retained.symbol function_.symbol) then
    Error "semantic retained function collection has the wrong function symbol"
  else if retained.item_index <> function_.item_index then
    Error "semantic retained function collection has the wrong item order"
  else if not (Symbol_table.owns_symbol table retained.symbol) then
    Error "semantic retained function collection belongs to a different table"
  else if not (Symbol_table.owns_scope table retained.scope) then
    Error "semantic retained function scope belongs to a different table"
  else if Symbol_table.scope_kind retained.scope <> Symbol_table.Function then
    Error "semantic retained function collection does not use a function scope"
  else if
    match Symbol_table.parent retained.scope with
    | Some scope -> not (same_scope scope parent)
    | None -> true
  then Error "semantic retained function scope has the wrong parent"
  else if
    List.exists
      (fun entry -> Option.is_none (entry_parameter_index entry))
      retained.entries
  then Error "semantic retained function header already contains locals"
  else if
    not
      (List.length retained.entries = List.length parameters
      && List.for_all2 entry_matches_binding retained.entries parameters)
  then
    Error "semantic retained function parameters do not match the declaration"
  else Ok ()

let find_retained retained_headers (function_ : function_declaration) =
  List.filter
    (fun retained -> same_symbol retained.symbol function_.symbol)
    retained_headers
  |> function
  | [] -> Ok None
  | [ retained ] -> Ok (Some retained)
  | _ -> Error "semantic retained function collection repeats a function symbol"

let validate_retained ~table ~parent retained_headers function_facts =
  let rec validate_functions used_rev (remaining : function_declaration list) =
    match remaining with
    | [] ->
        if
          List.length (List.filter_map Fun.id used_rev)
          = List.length retained_headers
        then Ok (List.rev used_rev)
        else Error "semantic retained function collection was not consumed"
    | (function_ : function_declaration) :: rest -> (
        match find_retained retained_headers function_ with
        | Error _ as error -> error
        | Ok None -> validate_functions (None :: used_rev) rest
        | Ok (Some retained) -> (
            match
              validate_retained_header ~table ~parent retained function_
            with
            | Error _ as error -> error
            | Ok () -> validate_functions (Some retained :: used_rev) rest))
  in
  validate_functions [] function_facts

let collect_reused_function table (function_ : function_declaration) retained =
  let _, locals = parameter_bindings function_.bindings in
  match add_bindings table retained.scope locals with
  | Error _ as error -> error
  | Ok local_entries ->
      retained.header_reused <- true;
      Ok
        {
          symbol = retained.symbol;
          scope = retained.scope;
          item_index = function_.item_index;
          entries = retained.entries @ local_entries;
          completed_header = retained.completed_header;
          header_reused = true;
        }

let collect ?(retained_headers = []) ~table ~parent function_facts =
  match validate table parent function_facts with
  | Error _ as error -> error
  | Ok () -> (
      match
        validate_retained ~table ~parent retained_headers function_facts
      with
      | Error _ as error -> error
      | Ok retained ->
          let rec collect_all functions_rev = function
            | [], [] -> Ok { functions = List.rev functions_rev }
            | function_ :: rest, retained :: retained_rest -> (
                let collected =
                  match retained with
                  | None -> collect_function table parent function_
                  | Some retained ->
                      collect_reused_function table function_ retained
                in
                match collected with
                | Error _ as error -> error
                | Ok collected ->
                    collect_all
                      (collected :: functions_rev)
                      (rest, retained_rest))
            | [], _ :: _ | _ :: _, [] -> assert false
          in
          collect_all [] (function_facts, retained))
