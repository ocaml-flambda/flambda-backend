type t = string

include Identifiable.Make (struct
  type nonrec t = t

  let equal = String.equal

  let compare = String.compare

  let hash = Hashtbl.hash

  let print = Format.pp_print_string

  let output ppf s = Printf.fprintf ppf "%s" s
end)

let of_string s = s

let of_head_of_global_name { Global.Name.head; _ } = head

let to_string t = t

let dummy = "*none*"

let predef_exn = Global.Name.predef_exn |> of_head_of_global_name
