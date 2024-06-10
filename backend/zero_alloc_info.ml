module String = Misc.Stdlib.String

type t = { mutable zero_alloc : int String.Map.t }

let create () = { zero_alloc = String.Map.empty }

let reset t = t.zero_alloc <- String.Map.empty

let merge src ~into:dst =
  let join key b1 b2 =
    Misc.fatal_errorf "Unexpected merge %s %d %d" key b1 b2
  in
  dst.zero_alloc <- String.Map.union join dst.zero_alloc src.zero_alloc

type value = int

let get_value (t : t) s = String.Map.find_opt s t.zero_alloc

let set_value (t : t) s (v : value) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    Some new_
  in
  t.zero_alloc <- String.Map.update s (f v) t.zero_alloc

module Raw = struct
  type entries = (string * int) list

  type t = entries option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e
end

let to_raw (t : t) : Raw.t =
  if String.Map.is_empty t.zero_alloc
  then None
  else Some (String.Map.bindings t.zero_alloc)

let of_raw (t : Raw.t) : t =
  match t with
  | None -> create ()
  | Some t -> { zero_alloc = Raw.entries_to_map t }

let print t =
  let print name v = Printf.printf "\t\t%s = %#x\n" name v in
  (* CR gyorsh: move encode/decode here somehow for noalloc *)
  Printf.printf "Function summaries for static checks:\n";
  String.Map.iter print t.zero_alloc
