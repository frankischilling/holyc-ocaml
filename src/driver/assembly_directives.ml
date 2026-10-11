module Ast = Frontend.Ast
module Encoder = Asm.Encoder
module Value = Asm.Value_expression
module Token = Frontend.Token
module Token_kind = Frontend.Token_kind
module Config = Frontend.Preprocessor.Config
module Names = Map.Make (String)

type instruction = {
  item_index : int;
  operation : Ast.inline_assembly_operation;
  selection : Asm.Instruction.selection;
  position : int64;
  offset : int;
  size : int;
}

type entry = {
  item_index : int;
  kind : string;
  position : int64;
  offset : int;
  size : int;
  listing : bool;
  location : Ast.location;
}

type symbol = {
  id : int;
  name : string;
  scope : string option;
  exported : bool;
  position : int64;
  item_index : int;
  definition_item_indices : int list;
}

type relocation = {
  offset : int;
  width : int;
  kind : string;
  symbol : string option;
  type_id : int;
  position : int64;
  relative_base : int64 option;
  value : int64;
  location : Ast.location;
}

type fixup = {
  item_index : int;
  offset : int;
  position : int64;
  width : int;
  relative_base : int64 option;
  value : int64;
  symbols : string list;
  symbol_ids : int list;
  imported : string option;
  location : Ast.location;
}

type binary_input = {
  item_index : int;
  provenance : Frontend.Assembly_binary.provenance;
}

type t = {
  assembly : Ast.assembly_unit;
  bytes : string;
  initial_bytes : string;
  position : int64;
  end_position : int64;
  instructions : instruction list;
  entries : entry list;
  origin : int64 option;
  max_align_bits : int;
  imports : string list;
  symbols : symbol list;
  relocations : relocation list;
  fixups : fixup list;
  external_symbols : (string * int64) list;
  binary_inputs : binary_input list;
  work : int;
  input_bytes : int;
}

let diagnostic ~code (location : Ast.location) message =
  let related message span = Common.Diagnostic.{ span; message } in
  let secondary =
    Option.to_list
      (Option.map (related "expanded from here") location.generated_from)
    @ Option.to_list (Option.map (related "defined here") location.defined_at)
  in
  Common.Diagnostic.make ~code ~severity:Common.Diagnostic.Error ~message
    ~primary:location.span ~secondary ()

exception Rejected of Common.Diagnostic.t

let reject ?(code = "HCASM0001") location message =
  raise (Rejected (diagnostic ~code location message))

let operation_matches_selection (operation : Ast.inline_assembly_operation) =
  let selection = operation.inline_assembly_selection in
  let opcode = operation.inline_assembly_opcode in
  operation.inline_assembly_mode = selection.mode
  && opcode.assembly_source_token.raw
     = Asm.Opcode.resolved_source_spelling selection.opcode
  && (match opcode.assembly_token_kind with
    | Ast.Assembly_opcode_token { canonical_spelling; source_is_alias } ->
        canonical_spelling
        = Asm.Opcode.spelling (Asm.Opcode.resolved_opcode selection.opcode)
        && source_is_alias = Asm.Opcode.resolved_is_alias selection.opcode
    | _ -> false)
  && List.length operation.inline_assembly_operands
     = List.length selection.operands
  && List.for_all2
       (fun (operand : Ast.inline_assembly_operand) selected ->
         operand.inline_assembly_operand == selected)
       operation.inline_assembly_operands selection.operands

let use_directive_matches (directive : Ast.inline_assembly_directive) bits =
  let token = directive.inline_assembly_directive_token in
  let spelling = Printf.sprintf "USE%d" bits in
  token.assembly_source_token.raw = spelling
  && directive.inline_assembly_directive_arguments = []
  && directive.inline_assembly_directive_expressions = []
  && directive.inline_assembly_directive_type_definitions = []
  &&
  match (token.assembly_token_kind, Asm.Directive.find spelling) with
  | Ast.Assembly_directive_token { templeos_id }, Some original ->
      templeos_id = Asm.Directive.templeos_id original
  | _ -> false

let directive_spelling = function
  | Ast.Inline_assembly_import_directive -> "IMPORT"
  | Ast.Inline_assembly_org_directive -> "ORG"
  | Ast.Inline_assembly_align_directive -> "ALIGN"
  | Ast.Inline_assembly_data_directive { element_width_bytes } ->
      Printf.sprintf "DU%d" (8 * element_width_bytes)
  | Ast.Inline_assembly_binfile_directive -> "BINFILE"
  | Ast.Inline_assembly_list_directive -> "LIST"
  | Ast.Inline_assembly_nolist_directive -> "NOLIST"
  | Ast.Inline_assembly_use_directive { segment_width_bits } ->
      Printf.sprintf "USE%d" segment_width_bits

let directive_kind = function
  | Ast.Inline_assembly_import_directive -> "import"
  | Ast.Inline_assembly_org_directive -> "org"
  | Ast.Inline_assembly_align_directive -> "align"
  | Ast.Inline_assembly_data_directive _ -> "data"
  | Ast.Inline_assembly_binfile_directive -> "binfile"
  | Ast.Inline_assembly_list_directive -> "list"
  | Ast.Inline_assembly_nolist_directive -> "nolist"
  | Ast.Inline_assembly_use_directive _ -> "use"

let validate_directive (directive : Ast.inline_assembly_directive) =
  let location = directive.inline_assembly_directive_location in
  let spelling = directive_spelling directive.inline_assembly_directive_kind in
  let token = directive.inline_assembly_directive_token in
  if
    token.assembly_source_token.raw <> spelling
    ||
    match (token.assembly_token_kind, Asm.Directive.find spelling) with
    | Ast.Assembly_directive_token { templeos_id }, Some original ->
        templeos_id <> Asm.Directive.templeos_id original
    | _ -> true
  then
    reject ~code:"HCASM0005" location
      "assembly directive does not own its original keyword";
  if directive.inline_assembly_directive_type_definitions <> [] then
    reject ~code:"HCASM0004" location
      "assembly type-definition expressions require source compilation"

type cell = {
  cell_id : int;
  cell_name : string;
  cell_scope : string option;
  imported : bool;
  external_value : int64 option;
  mutable definition_indices_rev : int list;
  mutable definition : symbol option;
}

type expression = {
  expression_tokens : Ast.assembly_token list;
  model_tokens : Value.token list;
  bindings : cell Names.t;
  expression_position : int64;
  expression_location : Ast.location;
}

type resolved_value = {
  word : int64;
  absolute : bool;
  constant_address : bool;
  import : cell option;
}

type value_plan = Fixed of resolved_value | Deferred of expression

type piece =
  | Bytes of int * string
  | Fill of int * int * char
  | Data of {
      item_index : int;
      offset : int;
      width : int;
      count : int;
      value : value_plan;
      location : Ast.location;
    }
  | Instruction of {
      receipt : instruction;
      values : (Asm.Operand.t * value_plan option) list;
    }

let value_token (token : Ast.assembly_token) =
  let source = token.assembly_source_token in
  match (source.kind, source.value) with
  | ( ( Token_kind.Integer
      | Token_kind.Character
      | Token_kind.Inserted_binary_size ),
      Token.Int64 value ) -> Value.Integer value
  | Token_kind.Float, Token.Float64 value -> Value.Floating value
  | (Token_kind.Identifier | Token_kind.Keyword _), _ -> (
      match token.assembly_token_primitive with
      | Some primitive ->
          let info = Common.Primitive_type.info primitive in
          if source.raw <> info.spelling && source.raw <> info.storage_spelling
          then
            reject ~code:"HCASM0005" token.assembly_token_location
              "assembly primitive does not own its source spelling";
          Value.Type source.raw
      | None -> Value.Name source.raw)
  | Token_kind.Operator _, _ -> Value.Operator source.raw
  | Token_kind.Punctuation punctuation, _ -> Value.Punctuation punctuation
  | Token_kind.Inserted_binary, _ ->
      reject ~code:"HCASM0004" token.assembly_token_location
        "an inserted binary pointer requires source-owned miscellaneous data; \
         its payload is not a DU string"
  | _ ->
      reject ~code:"HCASM0004" token.assembly_token_location
        "this assembly expression requires source compilation"

let token_matches_source model (token : Ast.assembly_token) =
  let source = token.assembly_source_token in
  let named name =
    source.raw = name
    &&
    match source.kind with
    | Token_kind.Identifier | Token_kind.Keyword _ -> true
    | _ -> false
  in
  match model with
  | Asm.Operand.Register register -> named (Asm.Register.spelling register)
  | Size width ->
      List.exists named
        [ Printf.sprintf "I%d" (width * 8); Printf.sprintf "U%d" (width * 8) ]
  | Integer value -> (
      match (source.kind, source.value) with
      | ( ( Token_kind.Integer
          | Token_kind.Character
          | Token_kind.Inserted_binary_size ),
          Token.Int64 actual ) -> value = actual
      | _ -> false)
  | Floating value -> (
      match (source.kind, source.value) with
      | Token_kind.Float, Token.Float64 actual ->
          value = Int64.bits_of_float actual
      | _ -> false)
  | String_literal value -> (
      match (source.kind, source.value) with
      | Token_kind.String, Token.Bytes actual -> value = actual
      | _ -> false)
  | Type_name name
  | Symbol name
  | Import_symbol name
  | Value_symbol name
  | Function_symbol name -> named name
  | Address_symbol { name; _ }
  | Variable_symbol { name; _ }
  | Function_call { name; _ } -> named name
  | Type_definition _ -> named "class" || named "union"
  | Expression_open_bracket -> source.kind = Token_kind.Punctuation '['
  | Expression_close_bracket -> source.kind = Token_kind.Punctuation ']'
  | Punctuation byte -> source.kind = Token_kind.Punctuation byte
  | Operator spelling -> (
      source.raw = spelling
      &&
      match source.kind with
      | Token_kind.Operator _ -> true
      | _ -> false)
  | Opaque spelling -> source.raw = spelling

let validate_operand_source (original : Ast.inline_assembly_operand) =
  let location = original.inline_assembly_operand_location in
  let models = Asm.Operand.tokens original.inline_assembly_operand in
  let sources = original.inline_assembly_operand_tokens in
  if
    List.length models <> List.length sources
    || not (List.for_all2 token_matches_source models sources)
  then
    reject ~code:"HCASM0005" location
      "assembly operand does not own its original source tokens"

let immediate_source_tokens (original : Ast.inline_assembly_operand)
    (immediate : Asm.Operand.immediate) =
  let location = original.inline_assembly_operand_location in
  let models = Asm.Operand.tokens original.inline_assembly_operand in
  let sources = original.inline_assembly_operand_tokens in
  let rec consume expression owned result =
    match (expression, owned) with
    | [], _ -> List.rev result
    | wanted :: expression, (model, source) :: owned when wanted == model ->
        consume expression owned (source :: result)
    | _ ->
        reject ~code:"HCASM0005" location
          "assembly immediate does not own its original source expression"
  in
  let rec find expression = function
    | [] ->
        reject ~code:"HCASM0005" location
          "assembly immediate has no original source expression"
    | (model, _) :: rest as owned -> (
        match expression with
        | first :: _ when first == model -> consume expression owned []
        | _ -> find expression rest)
  in
  find immediate.expression (List.combine models sources)

let expression_cells expression =
  List.filter_map
    (function
      | Value.Name name -> Names.find_opt name expression.bindings
      | _ -> None)
    expression.model_tokens

let expression_absolute expression =
  List.fold_left
    (fun parity -> function
      | Value.Operator "$$" -> not parity
      | Value.Name name -> (
          match Names.find_opt name expression.bindings with
          | Some cell ->
              parity
              <> ((not cell.imported) && Option.is_none cell.external_value)
          | None -> parity)
      | _ -> parity)
    false expression.model_tokens

let evaluate ?(allow_imports = false) expression =
  let resolve name =
    match Names.find_opt name expression.bindings with
    | Some { external_value = Some value; _ } -> Some (value, false)
    | Some { imported = true; _ } when allow_imports -> Some (0L, false)
    | Some { imported = false; definition = Some definition; _ } ->
        Some (definition.position, true)
    | _ -> None
  in
  match
    Value.evaluate ~position:expression.expression_position ~resolve
      expression.model_tokens
  with
  | Error error -> Error error
  | Ok value ->
      let cells = expression_cells expression in
      let imports = List.filter (fun cell -> cell.imported) cells in
      if allow_imports && List.length imports > 1 then
        reject ~code:"HCASM0009" expression.expression_location
          "two imports in the same assembly expression are not allowed";
      Ok
        {
          word = value.word;
          absolute = value.absolute;
          constant_address =
            List.fold_left
              (fun parity cell -> parity <> Option.is_some cell.external_value)
              false cells;
          import = (if allow_imports then List.nth_opt imports 0 else None);
        }

let evaluate_required ?allow_imports expression =
  match evaluate ?allow_imports expression with
  | Ok value -> value
  | Error error ->
      let location =
        match List.nth_opt expression.expression_tokens error.token_index with
        | Some token -> token.Ast.assembly_token_location
        | None -> expression.expression_location
      in
      let imported =
        Names.exists (fun _ cell -> cell.imported) expression.bindings
      in
      reject ~code:"HCASM0004" location
        (if imported then
           "an imported assembly value requires symbol linkage and fixup output"
         else error.message)

let encode ?(position = 0L) ?config ?(symbols = []) ?(max_work = 1_000_000)
    ?(max_binary_input_bytes = 16 * 1024 * 1024) ~max_code_bytes
    (assembly : Ast.assembly_unit) =
  let root = assembly.assembly_unit_location in
  try
    if max_code_bytes <= 0 || max_code_bytes > Encoder.max_code_bytes then
      reject ~code:"HCASM0002" root
        (Printf.sprintf "assembly code byte limit must be between 1 and %d"
           Encoder.max_code_bytes);
    if max_work <= 0 || max_work > 100_000_000 then
      reject ~code:"HCASM0007" root
        "assembly directive work limit must be between 1 and 100000000";
    if max_binary_input_bytes <= 0 || max_binary_input_bytes > 16 * 1024 * 1024
    then
      reject ~code:"HCASM0008" root
        "assembly binary input byte limit must be between 1 and 16777216";
    if position < 0L then
      reject ~code:"HCASM0003" root
        "assembly output position must be a nonnegative signed 64-bit value";
    let work = ref 0 and offset = ref 0 and input_bytes = ref 0 in
    let mode = ref assembly.assembly_unit_initial_mode in
    let listing = ref false and origin = ref None and max_align_bits = ref 0 in
    let pieces = ref [] and instructions = ref [] and entries = ref [] in
    let imports = ref [] and binary_inputs = ref [] and cells = ref [] in
    let next_cell_id = ref 0 and local_expressions = ref [] in
    let globals = ref Names.empty and locals = ref Names.empty in
    let local_scope = ref None in
    let current () = Int64.add position (Int64.of_int !offset) in
    let charge location amount =
      if amount < 0 || amount > max_work - !work then
        reject ~code:"HCASM0007" location
          "assembly directive work exceeds its configured limit";
      work := !work + amount
    in
    let reserve location size =
      if size < 0 || size > max_code_bytes - !offset then
        reject ~code:"HCASM0002" location
          "assembly output exceeds its code byte limit";
      if current () > Int64.sub Int64.max_int (Int64.of_int size) then
        reject ~code:"HCASM0003" location
          "assembly output position exceeds the signed 64-bit range";
      charge location size;
      let start = !offset in
      offset := !offset + size;
      start
    in
    let fresh_cell ?external_value name imported =
      let cell =
        {
          cell_id = !next_cell_id;
          cell_name = name;
          cell_scope =
            (if (not imported) && String.starts_with ~prefix:"@@" name then
               !local_scope
             else None);
          imported;
          external_value;
          definition_indices_rev = [];
          definition = None;
        }
      in
      incr next_cell_id;
      cells := cell :: !cells;
      cell
    in
    List.iter
      (fun (name, value) ->
        charge root 1;
        charge root (String.length name);
        if name = "" || Names.mem name !globals then
          reject ~code:"HCASM0009" root
            "external assembly symbol names must be nonempty and unique";
        let cell = fresh_cell ~external_value:value name false in
        globals := Names.add name cell !globals)
      symbols;
    let cell_for name =
      let table =
        if String.starts_with ~prefix:"@@" name then locals else globals
      in
      match Names.find_opt name !table with
      | Some cell -> cell
      | None -> (
          match Names.find_opt name !globals with
          | Some cell -> cell
          | None ->
              let cell = fresh_cell name false in
              table := Names.add name cell !table;
              cell)
    in
    let capture location source_tokens model_tokens =
      charge location (List.length model_tokens);
      let bindings =
        List.fold_left
          (fun found -> function
            | Value.Name name when not (Names.mem name found) ->
                Names.add name (cell_for name) found
            | _ -> found)
          Names.empty model_tokens
      in
      let expression =
        {
          expression_tokens = source_tokens;
          model_tokens;
          bindings;
          expression_position = current ();
          expression_location = location;
        }
      in
      if
        Names.exists
          (fun _ cell ->
            (not cell.imported)
            && Option.is_none cell.external_value
            && String.starts_with ~prefix:"@@" cell.cell_name
            && Option.is_none cell.definition)
          bindings
      then local_expressions := expression :: !local_expressions;
      expression
    in
    let require_active_keyword (token : Ast.assembly_token) =
      match Names.find_opt token.assembly_source_token.raw !globals with
      | Some cell when cell.imported || Option.is_some cell.external_value ->
          reject ~code:"HCASM0004" token.assembly_token_location
            (Printf.sprintf
               "assembly symbol %S shadows a parsed keyword, register or type"
               token.assembly_source_token.raw)
      | _ -> ()
    in
    let require_active_expression_role (token : Ast.assembly_token) =
      if Option.is_some token.assembly_token_primitive then
        require_active_keyword token;
      match token.assembly_source_token.kind with
      | Token_kind.Keyword _
        when List.mem token.assembly_source_token.raw
               [ "sizeof"; "offset"; "defined" ] -> (
          match Names.find_opt token.assembly_source_token.raw !globals with
          | Some { imported = true; _ } -> ()
          | _ -> require_active_keyword token)
      | _ -> ()
    in
    let capture_tokens location tokens =
      List.iter require_active_expression_role tokens;
      capture location tokens (List.map value_token tokens)
    in
    let require_active_operand_roles (operand : Ast.inline_assembly_operand) =
      List.iter2
        (fun model token ->
          require_active_expression_role token;
          match model with
          | Asm.Operand.Register _ | Size _ | Type_name _ ->
              require_active_keyword token
          | _ -> ())
        (Asm.Operand.tokens operand.inline_assembly_operand)
        operand.inline_assembly_operand_tokens
    in
    let plan_value expression =
      match evaluate expression with
      | Ok value -> Fixed value
      | Error _ ->
          if
            Names.exists
              (fun _ cell -> Option.is_none cell.definition)
              expression.bindings
          then Deferred expression
          else Fixed (evaluate_required expression)
    in
    let directive_values (directive : Ast.inline_assembly_directive) =
      let pending = ref directive.inline_assembly_directive_expressions in
      let take_expression tokens =
        match !pending with
        | [] ->
            reject ~code:"HCASM0005"
              directive.inline_assembly_directive_location
              "assembly directive expression has no original parse receipt"
        | (owned, operand) :: rest ->
            if Asm.Operand.mode operand <> !mode then
              reject ~code:"HCASM0005"
                directive.inline_assembly_directive_location
                "assembly directive expression does not own its mode";
            let rec consume original arguments =
              match (original, arguments) with
              | [], remaining -> remaining
              | token :: original, argument :: arguments
                when token.Ast.assembly_source_token
                     == argument.Ast.assembly_source_token ->
                  consume original arguments
              | _ ->
                  reject ~code:"HCASM0005"
                    directive.inline_assembly_directive_location
                    "assembly directive expression does not own its argument \
                     tokens"
            in
            let remaining = consume owned tokens in
            if owned = [] then
              reject ~code:"HCASM0005"
                directive.inline_assembly_directive_location
                "assembly directive expression receipt is empty";
            pending := rest;
            let location = (List.hd owned).Ast.assembly_token_location in
            (capture_tokens location owned, remaining)
      in
      let finish () =
        if !pending <> [] then
          reject ~code:"HCASM0005" directive.inline_assembly_directive_location
            "assembly directive has unused expression receipts"
      in
      (take_expression, finish)
    in
    let is_raw text token = token.Ast.assembly_source_token.raw = text in
    let require_empty location = function
      | [] -> ()
      | _ ->
          reject ~code:"HCASM0005" location
            "assembly directive has unexpected argument tokens"
    in
    let resolve_locals () =
      List.iter
        (fun expression ->
          charge expression.expression_location
            (List.length expression.model_tokens);
          ignore (evaluate_required expression))
        (List.rev !local_expressions);
      local_expressions := []
    in
    let process_data item_index (directive : Ast.inline_assembly_directive)
        width =
      let location = directive.inline_assembly_directive_location in
      if not (List.mem width [ 1; 2; 4; 8 ]) then
        reject ~code:"HCASM0005" location
          "assembly data width is not an original DU width";
      if Option.is_none directive.inline_assembly_directive_semicolon then
        reject ~code:"HCASM0005" location
          "assembly data requires its terminating semicolon";
      let take_expression, finish = directive_values directive in
      let rec data = function
        | [] -> finish ()
        | token :: _ when is_raw "," token ->
            reject ~code:"HCASM0005" token.assembly_token_location
              "assembly data has an unexpected comma"
        | token :: rest
          when token.assembly_source_token.kind = Token_kind.String ->
            let bytes =
              match token.assembly_source_token.value with
              | Token.Bytes bytes -> bytes
              | _ ->
                  reject ~code:"HCASM0005" token.assembly_token_location
                    "assembly data string has no byte payload"
            in
            let start =
              reserve token.assembly_token_location (String.length bytes)
            in
            pieces := Bytes (start, bytes) :: !pieces;
            following rest
        | tokens ->
            let expression, rest = take_expression tokens in
            let count, value, rest =
              match rest with
              | dup :: opening :: rest
                when is_raw "DUP" dup && is_raw "(" opening -> (
                  require_active_keyword dup;
                  let count = (evaluate_required expression).word in
                  let repeated, rest = take_expression rest in
                  let value = Fixed (evaluate_required repeated) in
                  match rest with
                  | closing :: rest when is_raw ")" closing ->
                      (count, value, rest)
                  | _ ->
                      reject ~code:"HCASM0005" location
                        "assembly DUP requires one scalar and a closing \
                         parenthesis")
              | _ -> (1L, plan_value expression, rest)
            in
            let count =
              if count <= 0L then 0
              else if count > Int64.of_int ((max_code_bytes - !offset) / width)
              then
                reject ~code:"HCASM0002" location
                  "assembly data repetition exceeds its code byte limit"
              else Int64.to_int count
            in
            let start = reserve location (count * width) in
            pieces :=
              Data { item_index; offset = start; width; count; value; location }
              :: !pieces;
            following rest
      and following = function
        | comma :: rest when is_raw "," comma -> data rest
        | rest -> data rest
      in
      data directive.inline_assembly_directive_arguments
    in
    List.iteri
      (fun item_index item ->
        let start = !offset and at = current () and listed = !listing in
        let record kind location =
          entries :=
            {
              item_index;
              kind;
              position = at;
              offset = start;
              size = !offset - start;
              listing = listed;
              location;
            }
            :: !entries
        in
        match item with
        | Ast.Inline_assembly_empty location -> charge location 1
        | Ast.Inline_assembly_label label ->
            let location = label.assembly_label_location in
            charge location 1;
            let name = label.assembly_label_name.spelling in
            let local = String.starts_with ~prefix:"@@" name in
            let exported =
              label.assembly_label_kind = Ast.Assembly_exported_global_label
            in
            if
              local <> (label.assembly_label_kind = Ast.Assembly_local_label)
              || (local && exported)
            then
              reject ~code:"HCASM0005" location
                "assembly label does not own its scope";
            let cell = cell_for name in
            if cell.imported then
              reject ~code:"HCASM0004" location
                "an imported assembly symbol cannot be defined";
            (match cell.external_value with
            | Some value when value <> at ->
                reject ~code:"HCASM0004" location
                  "assembly label redefines a supplied external symbol"
            | Some _ | None -> ());
            cell.definition_indices_rev <-
              item_index :: cell.definition_indices_rev;
            (match cell.definition with
            | Some existing when existing.position <> at ->
                reject ~code:"HCASM0004" location
                  (Printf.sprintf
                     "assembly label %S is redefined at a different output \
                      position"
                     name)
            | Some _ -> ()
            | None when Option.is_some cell.external_value -> ()
            | None ->
                cell.definition <-
                  Some
                    {
                      id = cell.cell_id;
                      name;
                      scope = cell.cell_scope;
                      exported;
                      position = at;
                      item_index;
                      definition_item_indices = [ item_index ];
                    };
                if exported then (
                  resolve_locals ();
                  locals := Names.empty;
                  local_scope := Some name));
            record "label" location
        | Ast.Inline_assembly_instruction operation ->
            let location = operation.inline_assembly_operation_location in
            charge location 1;
            require_active_keyword operation.inline_assembly_opcode;
            if
              operation.inline_assembly_mode <> !mode
              || not (operation_matches_selection operation)
            then
              reject ~code:"HCASM0005" location
                "assembly operation does not own its selected opcode and \
                 operands";
            let original_selection = operation.inline_assembly_selection in
            let values =
              List.map
                (fun (original : Ast.inline_assembly_operand) ->
                  validate_operand_source original;
                  require_active_operand_roles original;
                  let operand = original.inline_assembly_operand in
                  let planned =
                    match Asm.Operand.immediate operand with
                    | None -> None
                    | Some immediate
                      when Option.is_some immediate.value
                           && immediate.absolute_known -> None
                    | Some immediate ->
                        let source_tokens =
                          immediate_source_tokens original immediate
                        in
                        Some
                          (plan_value
                             (capture_tokens
                                original.inline_assembly_operand_location
                                source_tokens))
                  in
                  (operand, planned))
                operation.inline_assembly_operands
            in
            let operands =
              List.map
                (fun (operand, planned) ->
                  match planned with
                  | Some (Fixed value) ->
                      Asm.Operand.resolve_immediate ~value:value.word
                        ~absolute:value.absolute operand
                  | Some (Deferred expression) ->
                      Asm.Operand.resolve_address
                        ~absolute:(expression_absolute expression)
                        operand
                  | None -> operand)
                values
            in
            let selection =
              if
                List.exists
                  (function
                    | _, Some _ -> true
                    | _ -> false)
                  values
              then
                match
                  Asm.Instruction.select ~mode:original_selection.mode
                    ~opcode:original_selection.opcode ~operands
                with
                | Ok selection -> selection
                | Error error -> reject location error.message
              else original_selection
            in
            let size = selection.encoded_bytes in
            ignore (reserve location size);
            let receipt =
              {
                item_index;
                operation;
                selection;
                position = at;
                offset = start;
                size;
              }
            in
            pieces := Instruction { receipt; values } :: !pieces;
            record "instruction" location
        | Ast.Inline_assembly_directive directive ->
            let location = directive.inline_assembly_directive_location in
            charge location 1;
            require_active_keyword directive.inline_assembly_directive_token;
            (match directive.inline_assembly_directive_kind with
            | Ast.Inline_assembly_use_directive { segment_width_bits } ->
                let next =
                  match segment_width_bits with
                  | 16 -> Asm.Operand.Use16
                  | 32 -> Asm.Operand.Use32
                  | 64 -> Asm.Operand.Use64
                  | _ ->
                      reject ~code:"HCASM0005" location
                        "assembly USE directive has an invalid mode"
                in
                if not (use_directive_matches directive segment_width_bits) then
                  reject ~code:"HCASM0005" location
                    "assembly USE directive does not own its mode token";
                mode := next
            | _ -> (
                validate_directive directive;
                match directive.inline_assembly_directive_kind with
                | Ast.Inline_assembly_use_directive _ -> assert false
                | Ast.Inline_assembly_list_directive
                | Ast.Inline_assembly_nolist_directive ->
                    require_empty location
                      directive.inline_assembly_directive_arguments;
                    if directive.inline_assembly_directive_expressions <> []
                    then
                      reject ~code:"HCASM0005" location
                        "assembly listing directive has expression receipts";
                    listing :=
                      directive.inline_assembly_directive_kind
                      = Ast.Inline_assembly_list_directive
                | Ast.Inline_assembly_import_directive ->
                    if
                      Option.is_none
                        directive.inline_assembly_directive_semicolon
                    then
                      reject ~code:"HCASM0005" location
                        "assembly IMPORT requires a semicolon";
                    let rec names = function
                      | [] -> ()
                      | token :: rest
                        when match token.Ast.assembly_source_token.kind with
                             | Token_kind.Identifier | Token_kind.Keyword _ ->
                                 true
                             | _ -> false -> (
                          charge token.assembly_token_location 1;
                          let name = token.assembly_source_token.raw in
                          let cell = fresh_cell name true in
                          globals := Names.add name cell !globals;
                          imports := name :: !imports;
                          match rest with
                          | comma :: rest when is_raw "," comma -> names rest
                          | rest -> names rest)
                      | token :: _ ->
                          reject ~code:"HCASM0005" token.assembly_token_location
                            "assembly IMPORT requires identifier names"
                    in
                    names directive.inline_assembly_directive_arguments
                | Ast.Inline_assembly_org_directive ->
                    if Option.is_some !origin then
                      reject ~code:"HCASM0004" location
                        "only one assembly ORG is allowed";
                    if current () <> 0L then
                      reject ~code:"HCASM0004" location
                        "assembly ORG must precede all output";
                    let take, finish = directive_values directive in
                    let expression, rest =
                      take directive.inline_assembly_directive_arguments
                    in
                    require_empty location rest;
                    finish ();
                    let word = (evaluate_required expression).word in
                    origin := if word = Int64.max_int then None else Some word
                | Ast.Inline_assembly_align_directive ->
                    let take, finish = directive_values directive in
                    let alignment, rest =
                      take directive.inline_assembly_directive_arguments
                    in
                    let rest =
                      match rest with
                      | comma :: rest when is_raw "," comma -> rest
                      | _ ->
                          reject ~code:"HCASM0005" location
                            "assembly ALIGN requires a fill expression"
                    in
                    let fill, rest = take rest in
                    require_empty location rest;
                    finish ();
                    let alignment = (evaluate_required alignment).word in
                    if
                      alignment = 0L
                      || Int64.logand alignment (Int64.pred alignment) <> 0L
                    then
                      reject ~code:"HCASM0004" location
                        "assembly ALIGN must be a power of two";
                    let compilation_mode =
                      Option.fold ~none:Frontend.Preprocessor.Jit
                        ~some:Config.compilation_mode config
                    in
                    if
                      compilation_mode = Frontend.Preprocessor.Jit
                      && alignment > 8L
                    then
                      reject ~code:"HCASM0004" location
                        "in JIT mode, maximum ALIGN is 8";
                    let rec bits value count =
                      if value = 1L then count
                      else bits (Int64.shift_right_logical value 1) (count + 1)
                    in
                    max_align_bits := max !max_align_bits (bits alignment 0);
                    let padding =
                      if alignment = Int64.min_int then 0
                      else
                        let remainder =
                          Int64.logand (current ()) (Int64.pred alignment)
                        in
                        let delta =
                          if remainder = 0L then 0L
                          else Int64.sub alignment remainder
                        in
                        if delta > Int64.of_int (max_code_bytes - !offset) then
                          reject ~code:"HCASM0002" location
                            "assembly alignment exceeds its code byte limit";
                        Int64.to_int delta
                    in
                    let fill = (evaluate_required fill).word in
                    let start = reserve location padding in
                    pieces :=
                      Fill
                        ( start,
                          padding,
                          Char.chr (Int64.to_int (Int64.logand fill 255L)) )
                      :: !pieces
                | Ast.Inline_assembly_data_directive { element_width_bytes } ->
                    process_data item_index directive element_width_bytes
                | Ast.Inline_assembly_binfile_directive -> (
                    let spelling =
                      match directive.inline_assembly_directive_arguments with
                      | [ token ]
                        when token.Ast.assembly_source_token.kind
                             = Token_kind.String -> (
                          match token.assembly_source_token.value with
                          | Token.Bytes value -> value
                          | _ ->
                              reject ~code:"HCASM0005" location
                                "assembly BINFILE requires a string value")
                      | _ ->
                          reject ~code:"HCASM0005" location
                            "assembly BINFILE requires one file-name string"
                    in
                    let config =
                      match config with
                      | Some config -> config
                      | None ->
                          reject ~code:"HCASM0008" location
                            "assembly BINFILE requires an explicit input-root \
                             configuration"
                    in
                    let remaining = max_binary_input_bytes - !input_bytes in
                    let max_output_bytes =
                      min (max_code_bytes - !offset) (max_work - !work)
                    in
                    match
                      Frontend.Assembly_binary.load
                        ~resolver:(Config.resolver config)
                        ~max_input_bytes:remaining ~max_output_bytes ~spelling
                    with
                    | Error message -> reject ~code:"HCASM0008" location message
                    | Ok binary ->
                        input_bytes := !input_bytes + binary.provenance.raw_size;
                        let start =
                          reserve location (String.length binary.bytes)
                        in
                        binary_inputs :=
                          { item_index; provenance = binary.provenance }
                          :: !binary_inputs;
                        pieces := Bytes (start, binary.bytes) :: !pieces)));
            record
              (directive_kind directive.inline_assembly_directive_kind)
              location)
      assembly.assembly_unit_items;
    resolve_locals ();
    if !mode <> assembly.assembly_unit_final_mode then
      reject ~code:"HCASM0005" root
        "assembly final mode does not match its ordered operations";
    let relocations = ref [] and fixups = ref [] in
    let resolve_value = function
      | Fixed value -> value
      | Deferred expression ->
          charge expression.expression_location
            (List.length expression.model_tokens);
          evaluate_required ~allow_imports:true expression
    in
    let type_width width =
      match width with
      | 1 -> 0
      | 2 -> 2
      | 4 -> 4
      | 8 -> 6
      | _ -> assert false
    in
    let record_field ~item_index ~offset ~width ~relative_base ~location planned
        (value : resolved_value) =
      charge location 1;
      let position = Int64.add position (Int64.of_int offset) in
      let imported = Option.map (fun cell -> cell.cell_name) value.import in
      let relocate kind type_id symbol =
        relocations :=
          {
            offset;
            width;
            kind;
            symbol;
            type_id;
            position;
            relative_base;
            value = value.word;
            location;
          }
          :: !relocations
      in
      (match relative_base with
      | None when value.absolute ->
          relocate
            (Printf.sprintf "AAT_ADD_U%d" (width * 8))
            (type_width width) None
      | Some _ when value.constant_address ->
          relocate
            (Printf.sprintf "AAT_SUB_U%d" (width * 8))
            (type_width width + 1)
            None
      | _ -> ());
      Option.iter
        (fun name ->
          let relative = Option.is_some relative_base in
          let kind =
            if relative then Printf.sprintf "IET_REL_I%d" (width * 8)
            else if width = 8 then "IET_IMM_I64"
            else Printf.sprintf "IET_IMM_U%d" (width * 8)
          in
          relocate kind
            (type_width width + if relative then 4 else 5)
            (Some name))
        imported;
      match planned with
      | Some (Deferred expression) ->
          let cells = expression_cells expression in
          fixups :=
            {
              item_index;
              offset;
              position;
              width;
              relative_base;
              value = value.word;
              symbols = List.map (fun cell -> cell.cell_name) cells;
              symbol_ids = List.map (fun cell -> cell.cell_id) cells;
              imported;
              location = expression.expression_location;
            }
            :: !fixups
      | None | Some (Fixed _) -> ()
    in
    let data_plans = ref [] and instruction_plans = ref [] in
    let imported_fields = ref [] in
    List.iter
      (function
        | Data { item_index; offset; width; count; value = planned; location }
          ->
            let value = resolve_value planned in
            data_plans := (offset, width, count, value.word) :: !data_plans;
            for index = 0 to count - 1 do
              record_field ~item_index
                ~offset:(offset + (index * width))
                ~width ~relative_base:None ~location (Some planned) value
            done
        | Instruction { receipt; values } ->
            let location =
              receipt.operation.inline_assembly_operation_location
            in
            let original = receipt.selection in
            let resolved =
              List.map
                (fun (operand, planned) ->
                  match planned with
                  | None ->
                      Option.map
                        (fun immediate ->
                          {
                            word = Option.get immediate.Asm.Operand.value;
                            absolute = immediate.absolute;
                            constant_address = false;
                            import = None;
                          })
                        (Asm.Operand.immediate operand)
                  | Some value -> Some (resolve_value value))
                values
            in
            let trailing =
              if original.form.flags land 0x400 <> 0 then 1 else 0
            in
            let relative_end =
              Int64.add receipt.position
                (Int64.of_int (receipt.size - trailing))
            in
            let displacement, immediate =
              Asm.Instruction.numeric_fields original
            in
            let relative_operand index =
              List.exists
                (function
                  | Some (field : Asm.Instruction.numeric_field) ->
                      field.relative && field.operand_index = Some index
                  | None -> false)
                [ displacement; immediate ]
            in
            let supplied =
              List.mapi
                (fun index -> function
                  | None -> None
                  | Some value ->
                      let word =
                        if Option.is_none value.import then value.word
                        else if relative_operand index then relative_end
                        else 0L
                      in
                      Some (word, value.absolute))
                resolved
            in
            let selection =
              match Asm.Instruction.resolve_values original supplied with
              | Error error -> reject ~code:"HCASM0004" location error.message
              | Ok selection -> selection
            in
            (match Encoder.plan ~position:receipt.position selection with
            | Error error ->
                let location =
                  match error.operand_index with
                  | Some index ->
                      Option.fold ~none:location
                        ~some:(fun operand ->
                          operand.Ast.inline_assembly_operand_location)
                        (List.nth_opt receipt.operation.inline_assembly_operands
                           index)
                  | None -> location
                in
                reject location error.message
            | Ok plan ->
                instructions := { receipt with selection } :: !instructions;
                instruction_plans :=
                  (receipt.offset, plan) :: !instruction_plans);
            let size_field =
              Option.fold ~none:0 ~some:(fun field ->
                  field.Asm.Instruction.bytes)
            in
            let field_offset =
              ref
                (receipt.offset + receipt.size - trailing
               - size_field displacement - size_field immediate)
            in
            List.iter
              (fun field ->
                Option.iter
                  (fun (field : Asm.Instruction.numeric_field) ->
                    (match field.operand_index with
                    | None -> ()
                    | Some index ->
                        let value = List.nth resolved index in
                        Option.iter
                          (fun value ->
                            let planned = snd (List.nth values index) in
                            let operand_location =
                              (List.nth
                                 receipt.operation.inline_assembly_operands
                                 index)
                                .inline_assembly_operand_location
                            in
                            record_field ~item_index:receipt.item_index
                              ~offset:!field_offset ~width:field.bytes
                              ~relative_base:
                                (if field.relative then Some relative_end
                                 else None)
                              ~location:operand_location planned value;
                            if Option.is_some value.import then
                              imported_fields :=
                                (!field_offset, field.bytes, value.word)
                                :: !imported_fields)
                          value);
                    field_offset := !field_offset + field.bytes)
                  field)
              [ displacement; immediate ]
        | Bytes _ | Fill _ -> ())
      (List.rev !pieces);
    charge root !offset;
    let bytes = Bytes.create !offset in
    let store_word bytes offset width word =
      for byte = 0 to width - 1 do
        Bytes.set bytes (offset + byte)
          (Char.chr
             (Int64.to_int
                (Int64.logand (Int64.shift_right_logical word (8 * byte)) 255L)))
      done
    in
    List.iter
      (function
        | Bytes (offset, source) ->
            Bytes.blit_string source 0 bytes offset (String.length source)
        | Fill (offset, size, byte) -> Bytes.fill bytes offset size byte
        | Data _ | Instruction _ -> ())
      !pieces;
    List.iter
      (fun (offset, width, count, word) ->
        for index = 0 to count - 1 do
          for byte = 0 to width - 1 do
            Bytes.set bytes
              (offset + (index * width) + byte)
              (Char.chr
                 (Int64.to_int
                    (Int64.logand
                       (Int64.shift_right_logical word (8 * byte))
                       255L)))
          done
        done)
      !data_plans;
    List.iter
      (fun (offset, plan) ->
        let source = Encoder.emit plan in
        Bytes.blit_string source 0 bytes offset (String.length source))
      !instruction_plans;
    List.iter
      (fun (offset, width, value) -> store_word bytes offset width value)
      !imported_fields;
    let initial_bytes = Bytes.copy bytes in
    List.iter
      (fun (fixup : fixup) ->
        let value = Option.fold ~none:0L ~some:Int64.neg fixup.relative_base in
        store_word initial_bytes fixup.offset fixup.width value)
      !fixups;
    let external_symbols = symbols in
    let symbols =
      List.filter_map
        (fun cell ->
          Option.map
            (fun symbol ->
              {
                symbol with
                definition_item_indices = List.rev cell.definition_indices_rev;
              })
            cell.definition)
        (List.rev !cells)
      |> List.sort (fun (left : symbol) right ->
          compare left.item_index right.item_index)
    in
    Ok
      {
        assembly;
        bytes = Bytes.unsafe_to_string bytes;
        initial_bytes = Bytes.unsafe_to_string initial_bytes;
        position;
        end_position = current ();
        instructions = List.rev !instructions;
        entries = List.rev !entries;
        origin = !origin;
        max_align_bits = !max_align_bits;
        imports = List.rev !imports;
        symbols;
        relocations = List.rev !relocations;
        fixups = List.rev !fixups;
        external_symbols;
        binary_inputs = List.rev !binary_inputs;
        work = !work;
        input_bytes = !input_bytes;
      }
  with Rejected diagnostic -> Error diagnostic
