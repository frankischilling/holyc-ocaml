module Parser = Frontend.Parser

type peer_state = Available | Reserved | Consumed

type t = {
  context : Parser.command_context;
  namespace : Declaration_collection.namespace;
  fork : Symbol_table.empty_fork;
  owner : unit ref;
  mutable peer_state : peer_state;
  mutable consumer_claimed : bool;
}

let create ~context ~events_rev ~namespace ~fork ~owner =
  if
    not
      (Declaration_collection.namespace_owns_table namespace
         (Symbol_table.fork_parent fork))
  then Error "AOT invocation fork belongs to another namespace"
  else
    match Parser.claim_aot_invocation context ~events_rev with
    | Error _ as error -> error
    | Ok () ->
        Result.map
          (fun () ->
            {
              context;
              namespace;
              fork;
              owner;
              peer_state = Available;
              consumer_claimed = false;
            })
          (Symbol_table.claim_empty_fork fork)

let claim_peer value ~parent ~child =
  if
    value.peer_state <> Available
    || (not (Symbol_table.fork_matches value.fork ~parent ~child))
    || (not
          (Declaration_collection.namespace_owns_table value.namespace parent))
    || not (Parser.context_has_focus value.context)
  then Error "AOT peer requires its original live invocation and fork"
  else (
    value.peer_state <- Reserved;
    Ok ())

let fork value = value.fork
let context value = value.context
let owns_namespace value namespace = value.namespace == namespace
let namespace value = value.namespace

let consume_peer value ~parent ~child ~owner =
  if
    value.peer_state <> Reserved
    || value.owner != owner
    || (not (Symbol_table.fork_matches value.fork ~parent ~child))
    || (not
          (Declaration_collection.namespace_owns_table value.namespace parent))
    || not (Parser.context_has_focus value.context)
  then Error "AOT peer has no reserved original invocation"
  else (
    value.peer_state <- Consumed;
    Ok ())

let claim_consumer value ~owner ~child =
  if
    value.peer_state <> Consumed
    || value.consumer_claimed || value.owner != owner
    || Symbol_table.fork_child value.fork != child
    || not (Parser.context_has_focus value.context)
  then Error "AOT consumer requires the original unclaimed live invocation"
  else (
    value.consumer_claimed <- true;
    Ok ())
