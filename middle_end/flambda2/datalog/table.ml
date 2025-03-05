(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_imports

module Int = struct
  include Numbers.Int
  module Tree = Patricia_tree.Make (Numbers.Int)
  module Map = Tree.Map
end

(* This is the [Type] module from OCaml 5's Stdlib *)
module Type = struct
  type (_, _) eq = Equal : ('a, 'a) eq

  module Id = struct
    type _ id = ..

    module type ID = sig
      type t

      type _ id += Id : t id
    end

    type !'a t = (module ID with type t = 'a)

    let make (type a) () : a t =
      (module struct
        type t = a

        type _ id += Id : t id
      end)

    let[@inline] uid (type a) ((module A) : a t) : int =
      Obj.Extension_constructor.id [%extension_constructor A.Id]

    let provably_equal (type a b) ((module A) : a t) ((module B) : b t) :
        (a, b) eq option =
      match A.Id with B.Id -> Some Equal | _ -> None
  end
end

let concat is_trie ~earlier:t1 ~later:t2 =
  Trie.union is_trie (fun _ v -> Some v) t1 t2

module Id = struct
  type ('t, 'k, 'v) t =
    { id : ('t * 'k) Type.Id.t;
      name : string;
      is_trie : ('t, 'k, 'v) Trie.is_trie;
      print_keys : Format.formatter -> 'k Constant.hlist -> unit;
      default_value : 'v
    }

  let is_trie { is_trie; _ } = is_trie

  let name { name; _ } = name

  type ('k, 'v) poly = Id : ('t, 'k, 'v) t -> ('k, 'v) poly

  let print ppf t = Format.fprintf ppf "%s" t.name

  let hash { id; _ } = Hashtbl.hash (Type.Id.uid id)

  let equal { id = id1; _ } { id = id2; _ } = Type.Id.uid id1 = Type.Id.uid id2

  let[@inline] provably_equal_keys_exn (type a k v a' k' v') (r1 : (a, k, v) t)
      (r2 : (a', k', v') t) : (k, k') Type.eq =
    match Type.Id.provably_equal r1.id r2.id with
    | Some Equal -> Equal
    | None -> Misc.fatal_error "Inconsistent type for uid."

  let[@inline] provably_equal_exn (type a k v a' k' v') (r1 : (a, k, v) t)
      (r2 : (a', k', v') t) : (a, a') Type.eq =
    match Type.Id.provably_equal r1.id r2.id with
    | Some Equal -> Equal
    | None -> Misc.fatal_error "Inconsistent type for uid."

  let compare { id = id1; _ } { id = id2; _ } =
    compare (Type.Id.uid id1) (Type.Id.uid id2)

  let create ~name ~is_trie ~print_keys ~default_value =
    { id = Type.Id.make (); name; is_trie; print_keys; default_value }

  let[@inline] uid { id; _ } = Type.Id.uid id

  let[@inline] cast_exn (type a b k v k' v') (r1 : (a, k, v) t)
      (r2 : (b, k', v') t) (t : a) : b =
    match Type.Id.provably_equal r1.id r2.id with
    | Some Equal -> t
    | None -> Misc.fatal_error "Inconsistent type for uid."

  let create_iterator { is_trie; default_value; name; _ } =
    let handler = ref (Trie.empty is_trie) in
    let out = ref default_value in
    let iterator = Trie.Iterator.create is_trie handler out in
    let rec get_names : type a. a Trie.Iterator.hlist -> int -> string list =
      fun (type a) (iterators : a Trie.Iterator.hlist) i : string list ->
       match iterators with
       | [] -> []
       | _ :: iterators ->
         (name ^ "." ^ string_of_int i) :: get_names iterators (i + 1)
    in
    handler, { values = iterator; names = get_names iterator 0 }, out
end

module VM = Virtual_machine.Make (Trie.Iterator)

let iter id f table =
  let input_ref, it, out_ref = Id.create_iterator id in
  input_ref.contents <- table;
  VM.iter (fun keys -> f keys out_ref.contents) (VM.iterator it)

let print id ?(pp_sep = Format.pp_print_cut) pp_row ppf table =
  let first = ref true in
  iter id
    (fun keys value ->
      if !first then first := false else pp_sep ppf ();
      pp_row keys value)
    table

let print_table (id : (_, _, _) Id.t) ppf table =
  Format.fprintf ppf "@[<v>%a@]"
    (print id (fun keys _ ->
         Format.fprintf ppf "@[%a(%a).@]" Id.print id id.print_keys keys))
    table

let print id ppf table =
  let header = Format.asprintf "%a" Id.print id in
  Format.fprintf ppf "@[<v>%s@ %s@ %a@]" header
    (String.make (String.length header) '=')
    (print_table id) table

module Map = struct
  type binding = Binding : ('t, 'k, 'v) Id.t * 't -> binding

  type t = binding Int.Map.t

  let print ppf tables =
    let first = ref true in
    Format.fprintf ppf "@[<v>";
    Int.Map.iter
      (fun _ (Binding (id, table)) ->
        if !first then first := false else Format.fprintf ppf "@ @ ";
        print id ppf table)
      tables;
    Format.fprintf ppf "@]"

  let get (type t k v) (id : (t, k, v) Id.t) tables : t =
    match Int.Map.find_opt (Id.uid id) tables with
    | Some (Binding (existing_id, table)) -> Id.cast_exn existing_id id table
    | None -> Trie.empty (Id.is_trie id)

  let set (type t k v) (id : (t, k, v) Id.t) (table : t) tables =
    Int.Map.add (Id.uid id) (Binding (id, table)) tables

  let empty = Int.Map.empty

  let is_empty = Int.Map.is_empty

  let concat ~earlier:tables1 ~later:tables2 =
    Int.Map.union
      (fun _ (Binding (id1, table1)) (Binding (id2, table2)) ->
        let table =
          concat (Id.is_trie id1) ~earlier:table1
            ~later:(Id.cast_exn id2 id1 table2)
        in
        Some (Binding (id1, table)))
      tables1 tables2
end
