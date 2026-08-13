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
}

type t = { functions : collected_function list }

let functions collection = collection.functions
let function_symbol (function_ : collected_function) = function_.symbol
let function_scope (function_ : collected_function) = function_.scope
let function_item_index (function_ : collected_function) = function_.item_index
let function_entries (function_ : collected_function) = function_.entries
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

let make_function ~symbol ~item_index bindings =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic function scope owner must be a function symbol"
  else if item_index < 0 then
    Error "semantic function item index cannot be negative"
  else Ok { symbol; item_index; bindings }

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
            })

let collect ~table ~parent function_facts =
  match validate table parent function_facts with
  | Error _ as error -> error
  | Ok () ->
      let rec collect_all functions_rev = function
        | [] -> Ok { functions = List.rev functions_rev }
        | function_ :: rest -> (
            match collect_function table parent function_ with
            | Error _ as error -> error
            | Ok collected -> collect_all (collected :: functions_rev) rest)
      in
      collect_all [] function_facts
