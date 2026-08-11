type t = {
  sources : Common.Source_manager.t;
  definitions : Frontend.Definition.Environment.t;
}

let create () =
  {
    sources = Common.Source_manager.create ();
    definitions = Frontend.Definition.Environment.create ();
  }

let sources session = session.sources
let definitions session = session.definitions

let add_source session ~path ~contents =
  Common.Source_manager.add_string session.sources ~path ~contents

let load_source ?max_bytes ?display_path session ~path =
  Common.Source_manager.load ?max_bytes ?display_path session.sources ~path
