module String = Misc.Stdlib.String

(* CR gyorsh: Add [t] per analysis when at least one more analysis is
   implemented *)
type t =
  { mutable zero_alloc : int String.Map.t;
    mutable enabled : bool
  }

let create () = { zero_alloc = String.Map.empty; enabled = false }

let reset t =
  t.zero_alloc <- String.Map.empty;
  t.enabled <- false

let merge src ~into:dst =
  let join key b1 b2 =
    Misc.fatal_errorf "Unexpected merge %s %d %d" key b1 b2
  in
  dst.zero_alloc <- String.Map.union join dst.zero_alloc src.zero_alloc;
  dst.enabled <- dst.enabled || src.enabled

type value = int option

let get_value (t : t) s : value = String.Map.find_opt s t.zero_alloc

let get_value (t : t) s : value option =
  match t.enabled with false -> None | true -> Some (get_value t s)

let set_value (t : t) s (v : value) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    new_
  in
  t.zero_alloc <- String.Map.update s (f v) t.zero_alloc;
  t.enabled <- true

module Raw = struct
  type entries = (string * int) list

  type r = { zero_alloc : entries }

  type t = r option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let print t =
    let zero_div v = v land lnot 0x30 in
    let print (name, v) =
      let v = zero_div v in
      if v = 0 then () else Printf.printf "\t\t%s = %#x\n" name v
    in
    (* CR gyorsh: move encode/decode here somehow for noalloc *)
    Printf.printf "Function summaries for static checks:\n";
    List.iter print t.zero_alloc

  let print = function None -> () | Some t -> print t
end

let to_raw (t : t) : Raw.r = { zero_alloc = String.Map.bindings t.zero_alloc }

let to_raw (t : t) : Raw.t =
  match t.enabled with false -> None | true -> Some (to_raw t)

let of_raw (t : Raw.t) : t =
  match t with
  | None -> create ()
  | Some t -> { zero_alloc = Raw.entries_to_map t.zero_alloc; enabled = true }
