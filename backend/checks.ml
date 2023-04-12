module String = Misc.Stdlib.String

(* CR gyorsh: refactor repetititve code. *)
(* CR gyorsh: Add [t] per analysis when at least one more analysis is
   implemented *)
type t =
  { mutable nor : bool String.Map.t;
    mutable exn : bool String.Map.t;
    mutable div : bool String.Map.t;
    mutable enabled : bool
  }

let create () =
  { nor = String.Map.empty;
    exn = String.Map.empty;
    div = String.Map.empty;
    enabled = false
  }

let reset t =
  t.nor <- String.Map.empty;
  t.exn <- String.Map.empty;
  t.div <- String.Map.empty;
  t.enabled <- false

let merge src ~into:dst =
  if !Clflags.zero_alloc_check
  then (
    let join _key b1 b2 = Some (b1 || b2) in
    dst.nor <- String.Map.union join dst.nor src.nor;
    dst.exn <- String.Map.union join dst.exn src.exn;
    dst.div <- String.Map.union join dst.div src.div;
    dst.enabled <- dst.enabled || src.enabled)

type value =
  { nor : bool option;
    exn : bool option;
    div : bool option
  }

let get_value (t : t) s : value =
  { nor = String.Map.find_opt s t.nor;
    exn = String.Map.find_opt s t.exn;
    div = String.Map.find_opt s t.div
  }

let get_value (t : t) s : value option =
  match t.enabled with false -> None | true -> Some (get_value t s)

let set_value (t : t) s (v : value) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    new_
  in
  t.nor <- String.Map.update s (f v.nor) t.nor;
  t.exn <- String.Map.update s (f v.exn) t.exn;
  t.div <- String.Map.update s (f v.div) t.div;
  t.enabled <- true

module Raw = struct
  type entries = (string * bool) list

  type r =
    { nor : entries;
      exn : entries;
      div : entries
    }

  type t = r option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let print t =
    let print (name, b) = Printf.printf "\t\t%s = %b\n" name b in
    let print_component c title =
      match c with
      | [] -> ()
      | _ ->
        Printf.printf "\t%s:\n" title;
        List.iter print c
    in
    (* CR gyorsh: move encode/decode here somehow for noalloc *)
    Printf.printf "Function summaries for static checks:\n";
    print_component t.nor "Normal return";
    print_component t.exn "Exceptional return";
    print_component t.div "Diverging"

  let print = function None -> () | Some t -> print t
end

let to_raw (t : t) : Raw.r =
  { nor = String.Map.bindings t.nor;
    exn = String.Map.bindings t.exn;
    div = String.Map.bindings t.div
  }

let to_raw (t : t) : Raw.t =
  match t.enabled with false -> None | true -> Some (to_raw t)

let of_raw (t : Raw.t) : t =
  match t with
  | None -> create ()
  | Some t ->
    { nor = Raw.entries_to_map t.nor;
      exn = Raw.entries_to_map t.exn;
      div = Raw.entries_to_map t.div;
      enabled = true
    }
