module N = Function_record_phase
module F = Function_resolution
module H = Function_type_resolution
module P = Frontend.Parser

type t = {
  table : Symbol_table.t;
  namespace : Declaration_collection.namespace;
  receipt : P.completed_call;
  source : Frontend.Ast.call_expression;
  selected : F.resolved_declaration;
  arguments : H.resolved_function;
  emission : Function_record_classification.classified_declaration;
  emission_fixed_count : int;
}

let source t = t.source
let receipt t = t.receipt
let selected t = t.selected
let arguments t = t.arguments
let emission t = t.emission
let emission_fixed_count t = t.emission_fixed_count
let owns_table t table = t.table == table
let owns_namespace t namespace = t.namespace == namespace

let emission_header t =
  t.emission |> Function_record_classification.classified_declaration_source
  |> F.resolved_declaration_header

let forward earlier later =
  N.same_revision earlier later || Result.is_ok (N.transition ~earlier ~later)

let create ~table ~namespace ~receipt ~selected ~arguments ~emission_snapshot
    ~emission =
  let ( let* ) = Result.bind in
  let* source =
    match receipt.P.call_expression with
    | Frontend.Ast.Call_expression call
      when call.call_callee == receipt.call_start.call_callee -> Ok call
    | _ -> Error "call phase has another original call expression"
  in
  let* argument_snapshot =
    match H.function_provisional_call arguments with
    | Some shape -> Ok (N.shape_snapshot shape)
    | None -> Error "call phase lacks a checked native argument cursor"
  in
  let selected_snapshot =
    F.resolved_declaration_site selected |> F.declaration_site_native_snapshot
  in
  let emitted =
    Function_record_classification.classified_declaration_source emission
  in
  let emission_native =
    F.resolved_declaration_site emitted |> F.declaration_site_native_snapshot
  in
  let* () =
    match (selected_snapshot, emission_native) with
    | Some selected_snapshot, Some emission_native
      when List.for_all
             (fun snapshot ->
               N.owns_table snapshot table
               && N.owns_namespace snapshot namespace)
             [
               selected_snapshot;
               argument_snapshot;
               emission_snapshot;
               emission_native;
             ]
           && forward selected_snapshot argument_snapshot
           && forward argument_snapshot emission_snapshot
           && N.same_identity emission_native emission_snapshot
           && F.resolved_declaration_identity_symbol selected
              == F.resolved_declaration_identity_symbol emitted -> Ok ()
    | _ -> Error "call phases have another native identity or source ancestry"
  in
  let* emission_fixed_count =
    match N.argument_count emission_snapshot with
    | Some count when count >= 0 -> Ok count
    | _ -> Error "call emission has no native fixed argument count"
  in
  Ok
    {
      table;
      namespace;
      receipt;
      source;
      selected;
      arguments;
      emission;
      emission_fixed_count;
    }
