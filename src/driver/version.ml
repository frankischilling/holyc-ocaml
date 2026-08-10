let package_version () =
  match Build_info.V1.version () with
  | Some version -> Build_info.V1.Version.to_string version
  | None -> "0.1.0-dev"

let implementation_commit = Build_metadata.implementation_commit
let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

let render () =
  Printf.sprintf "holyc %s\nimplementation %s\ntempleos-reference %s"
    (package_version ()) implementation_commit reference_commit
