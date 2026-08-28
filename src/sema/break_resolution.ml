type region_kind =
  | While_region
  | Do_while_region
  | For_body_region
  | Switch_region
  | Subswitch_region

module Region_id = struct
  type t = int

  let to_int value = value
  let compare = Int.compare
  let equal = Int.equal
end

type region_input = {
  region_index : int;
  region_kind : region_kind;
  region_origin : Symbol.origin;
}

type break_input = {
  break_index : int;
  break_origin : Symbol.origin;
  target_region_index : int;
}

type function_input = {
  input_symbol : Symbol.t;
  input_scope : Symbol_table.scope;
  input_item_index : int;
  input_regions : region_input list;
  input_breaks : break_input list;
}

type region = {
  id : Region_id.t;
  kind : region_kind;
  origin : Symbol.origin;
  break_count : int;
}

type resolved_break = {
  occurrence_index : int;
  origin : Symbol.origin;
  target : Region_id.t;
}

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  regions : region list;
  breaks : resolved_break list;
}

type t = { functions_ : resolved_function list }
type outcome = (t, string) result

let reference_commit = Symbol.reference_commit
let functions result = result.functions_
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index function_ = function_.item_index
let function_regions (function_ : resolved_function) = function_.regions
let function_breaks (function_ : resolved_function) = function_.breaks
let region_id (region : region) = region.id
let region_kind (region : region) = region.kind
let region_origin (region : region) = region.origin
let region_break_count (region : region) = region.break_count

let break_occurrence_index (occurrence : resolved_break) =
  occurrence.occurrence_index

let break_origin (occurrence : resolved_break) = occurrence.origin
let break_target (occurrence : resolved_break) = occurrence.target

let region_kind_name = function
  | While_region -> "while"
  | Do_while_region -> "do-while"
  | For_body_region -> "for-body"
  | Switch_region -> "switch"
  | Subswitch_region -> "subswitch"

let valid_origin = function
  | Symbol.Pinned_source { path; line } ->
      (not (String.equal path "")) && line > 0
  | Symbol.Source_location _ -> true
  | Symbol.Synthesized description -> not (String.equal description "")

let make_region ~region_index ~kind ~origin =
  if region_index < 0 then
    Error "semantic break region index cannot be negative"
  else if not (valid_origin origin) then
    Error "semantic break region has an invalid source origin"
  else Ok { region_index; region_kind = kind; region_origin = origin }

let make_break ~occurrence_index ~origin ~target_region_index =
  if occurrence_index < 0 then
    Error "semantic break occurrence index cannot be negative"
  else if target_region_index < 0 then
    Error "semantic break target region index cannot be negative"
  else if not (valid_origin origin) then
    Error "semantic break occurrence has an invalid source origin"
  else
    Ok
      {
        break_index = occurrence_index;
        break_origin = origin;
        target_region_index;
      }

let function_scope_matches_symbol symbol scope =
  match (Symbol_table.scope_name scope, Symbol_table.parent scope) with
  | Some name, Some parent ->
      String.equal name (Symbol.name symbol)
      && Symbol.Scope_id.equal
           (Symbol_table.scope_id parent)
           (Symbol.scope_id symbol)
  | None, _ | _, None -> false

let contiguous index get_index description values =
  let rec check expected = function
    | [] -> Ok ()
    | value :: rest ->
        if get_index value <> expected then
          Error
            (Printf.sprintf
               "semantic break %s index %d appears where %d was expected"
               description (get_index value) expected)
        else check (expected + 1) rest
  in
  check index values

let validate_inputs regions breaks =
  let input_region_index region = region.region_index in
  match contiguous 0 input_region_index "region" regions with
  | Error _ as error -> error
  | Ok () ->
      contiguous 0
        (fun occurrence -> occurrence.break_index)
        "occurrence" breaks

let make_function ~symbol ~scope ~item_index ~regions ~breaks =
  if not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Function) then
    Error "semantic break owner must be a function symbol"
  else if Symbol_table.scope_kind scope <> Symbol_table.Function then
    Error "semantic breaks require a function scope"
  else if not (function_scope_matches_symbol symbol scope) then
    Error "semantic break scope does not match its function symbol"
  else if item_index < 0 then
    Error "semantic break function item index cannot be negative"
  else
    match validate_inputs regions breaks with
    | Error _ as error -> error
    | Ok () ->
        Ok
          {
            input_symbol = symbol;
            input_scope = scope;
            input_item_index = item_index;
            input_regions = regions;
            input_breaks = breaks;
          }

module Int_set = Set.Make (Int)

let validate_function table previous seen_symbols seen_scopes input =
  let input_symbol = input.input_symbol in
  let symbol_number = Symbol.Id.to_int (Symbol.id input_symbol) in
  let scope_number =
    Symbol.Scope_id.to_int (Symbol_table.scope_id input.input_scope)
  in
  if not (Symbol_table.owns_symbol table input_symbol) then
    Error "semantic break owner belongs to a different symbol table"
  else if not (Symbol_table.owns_scope table input.input_scope) then
    Error "semantic break scope belongs to a different symbol table"
  else if input.input_item_index <= previous then
    Error "semantic break functions must be in increasing item order"
  else if Int_set.mem symbol_number seen_symbols then
    Error "semantic break function symbols cannot repeat"
  else if Int_set.mem scope_number seen_scopes then
    Error "semantic break function scopes cannot repeat"
  else
    Ok
      ( input.input_item_index,
        Int_set.add symbol_number seen_symbols,
        Int_set.add scope_number seen_scopes )

let validate_functions table functions =
  let rec check previous_item seen_symbols seen_scopes = function
    | [] -> Ok ()
    | function_ :: rest -> (
        match
          validate_function table previous_item seen_symbols seen_scopes
            function_
        with
        | Error _ as error -> error
        | Ok (item_index, seen_symbols, seen_scopes) ->
            check item_index seen_symbols seen_scopes rest)
  in
  check (-1) Int_set.empty Int_set.empty functions

let missing_target_message target =
  Printf.sprintf "break target region %d does not exist" target

let count_breaks region_count breaks =
  let counts = Array.make region_count 0 in
  let rec count = function
    | [] -> Ok counts
    | occurrence :: rest ->
        let target = occurrence.target_region_index in
        let target_missing = target >= region_count in
        if target_missing then Error (missing_target_message target)
        else if counts.(target) = Int.max_int then
          Error "semantic break count is exhausted"
        else (
          counts.(target) <- counts.(target) + 1;
          count rest)
  in
  count breaks

let resolve_function function_ =
  let region_count = List.length function_.input_regions in
  match count_breaks region_count function_.input_breaks with
  | Error _ as error -> error
  | Ok counts ->
      let regions =
        List.map
          (fun input ->
            {
              id = input.region_index;
              kind = input.region_kind;
              origin = input.region_origin;
              break_count = counts.(input.region_index);
            })
          function_.input_regions
      in
      let breaks =
        List.map
          (fun input ->
            {
              occurrence_index = input.break_index;
              origin = input.break_origin;
              target = input.target_region_index;
            })
          function_.input_breaks
      in
      Ok
        {
          symbol = function_.input_symbol;
          scope = function_.input_scope;
          item_index = function_.input_item_index;
          regions;
          breaks;
        }

let resolve ~table functions =
  match validate_functions table functions with
  | Error _ as error -> error
  | Ok () ->
      let rec collect reversed = function
        | [] -> Ok { functions_ = List.rev reversed }
        | function_ :: rest -> (
            match resolve_function function_ with
            | Error _ as error -> error
            | Ok resolved -> collect (resolved :: reversed) rest)
      in
      collect [] functions

let pinned_origin_name path line = Printf.sprintf "%s:%d" path line

let origin_name = function
  | Symbol.Pinned_source { path; line } -> pinned_origin_name path line
  | Symbol.Synthesized description -> "synthesized:" ^ description
  | Symbol.Source_location location ->
      Format.asprintf "%a" Common.Span.pp location.span

let region_name region =
  Printf.sprintf "region=%d kind=%s breaks=%d origin=%s"
    (Region_id.to_int region.id)
    (region_kind_name region.kind)
    region.break_count
    (origin_name region.origin)

let break_name occurrence =
  let index = occurrence.occurrence_index in
  Printf.sprintf "break=%d target=%d origin=%s" index
    (Region_id.to_int occurrence.target)
    (origin_name occurrence.origin)

let function_name function_ =
  let header =
    Printf.sprintf "function=@s%d:%S item=%d"
      (Symbol.Id.to_int (Symbol.id function_.symbol))
      (Symbol.name function_.symbol)
      function_.item_index
  in
  let regions = List.map region_name function_.regions in
  let breaks = List.map break_name function_.breaks in
  String.concat "\n" ((header :: regions) @ breaks)

let human result =
  let functions = List.map function_name result.functions_ in
  let body = String.concat "\n" functions in
  let schema = "holyc-sema-break-v1" in
  Printf.sprintf "%s reference=%s\n%s" schema reference_commit body
