module Thing = struct
  type t = string
  let compare = String.compare
  let equal = String.equal
  let hash = Hashtbl.hash
  let output chan t = Printf.fprintf chan "%s" t
  let print = Format.pp_print_string
end

include Thing
include Identifiable.Make(Thing)

let create ~make_symbol name = make_symbol name