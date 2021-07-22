(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Sets over ordered types *)

external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"
external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"
external not : bool -> bool = "%boolnot"
external ( & ) : bool -> bool -> bool = "%sequand"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( or ) : bool -> bool -> bool = "%sequor"
external ( || ) : bool -> bool -> bool = "%sequor"
external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( mod ) : int -> int -> int = "%modint"
external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"
external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"
external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
external acos : float -> float = "caml_acos_float" "acos"
external asin : float -> float = "caml_asin_float" "asin"
external atan : float -> float = "caml_atan_float" "atan"
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
external ceil : float -> float = "caml_ceil_float" "ceil"
external floor : float -> float = "caml_floor_float" "floor"
external abs_float : float -> float = "%absfloat"
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
external frexp : float -> float * int = "caml_frexp_float"
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external string_length : string -> int = "%string_length"
external bytes_length : bytes -> int = "%bytes_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"
external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
external ignore : 'a -> unit = "%ignore"
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
type 'a ref = { mutable contents : 'a; }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"
external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"
external int_of_string : string -> int = "caml_int_of_string"
external string_get : string -> int -> char = "%string_safe_get"
external float_of_string : string -> float = "caml_float_of_string"
external sys_exit : int -> 'a = "caml_sys_exit"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

let rec ( @ ) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val disjoint: t -> t -> bool
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val map: (elt -> elt) -> t -> t
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val min_elt_opt: t -> elt option
    val max_elt: t -> elt
    val max_elt_opt: t -> elt option
    val choose: t -> elt
    val choose_opt: t -> elt option
    val split: elt -> t -> t * bool * t
    val find: elt -> t -> elt
    val find_opt: elt -> t -> elt option
    val find_first: (elt -> bool) -> t -> elt
    val find_first_opt: (elt -> bool) -> t -> elt option
    val find_last: (elt -> bool) -> t -> elt
    val find_last_opt: (elt -> bool) -> t -> elt option
    val of_list: elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end

module Make(Ord: OrderedType) =
  struct
    type elt = Ord.t
    type t = Empty | Node of {l:t; v:elt; r:t; h:int}

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2 *)

    let height = function
        Empty -> 0
      | Node {h} -> h

    (* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

    let create l v r =
      let hl = match l with Empty -> 0 | Node {h} -> h in
      let hr = match r with Empty -> 0 | Node {h} -> h in
      Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

    let bal l v r =
      let hl = match l with Empty -> 0 | Node {h} -> h in
      let hr = match r with Empty -> 0 | Node {h} -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Set.bal"
        | Node{l=ll; v=lv; r=lr} ->
            if height ll >= height lr then
              create ll lv (create lr v r)
            else begin
              match lr with
                Empty -> invalid_arg "Set.bal"
              | Node{l=lrl; v=lrv; r=lrr}->
                  create (create ll lv lrl) lrv (create lrr v r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Set.bal"
        | Node{l=rl; v=rv; r=rr} ->
            if height rr >= height rl then
              create (create l v rl) rv rr
            else begin
              match rl with
                Empty -> invalid_arg "Set.bal"
              | Node{l=rll; v=rlv; r=rlr} ->
                  create (create l v rll) rlv (create rlr rv rr)
            end
      end else
        Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    (* Insertion of one element *)

    let rec add x = function
        Empty -> Node{l=Empty; v=x; r=Empty; h=1}
      | Node{l; v; r} as t ->
          let c = Ord.compare x v in
          if c = 0 then t else
          if c < 0 then
            let ll = add x l in
            if l == ll then t else bal ll v r
          else
            let rr = add x r in
            if r == rr then t else bal l v rr

    let singleton x = Node{l=Empty; v=x; r=Empty; h=1}

    (* Beware: those two functions assume that the added v is *strictly*
       smaller (or bigger) than all the present elements in the tree; it
       does not test for equality with the current min (or max) element.
       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

    let rec add_min_element x = function
      | Empty -> singleton x
      | Node {l; v; r} ->
        bal (add_min_element x l) v r

    let rec add_max_element x = function
      | Empty -> singleton x
      | Node {l; v; r} ->
        bal l v (add_max_element x r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v r =
      match (l, r) with
        (Empty, _) -> add_min_element v r
      | (_, Empty) -> add_max_element v l
      | (Node{l=ll; v=lv; r=lr; h=lh}, Node{l=rl; v=rv; r=rr; h=rh}) ->
          if lh > rh + 2 then bal ll lv (join lr v r) else
          if rh > lh + 2 then bal (join l v rl) rv rr else
          create l v r

    (* Smallest and greatest element of a set *)

    let rec min_elt = function
        Empty -> raise Not_found
      | Node{l=Empty; v} -> v
      | Node{l} -> min_elt l

    let rec min_elt_opt = function
        Empty -> None
      | Node{l=Empty; v} -> Some v
      | Node{l} -> min_elt_opt l

    let rec max_elt = function
        Empty -> raise Not_found
      | Node{v; r=Empty} -> v
      | Node{r} -> max_elt r

    let rec max_elt_opt = function
        Empty -> None
      | Node{v; r=Empty} -> Some v
      | Node{r} -> max_elt_opt r

    (* Remove the smallest element of the given set *)

    let rec remove_min_elt = function
        Empty -> invalid_arg "Set.remove_min_elt"
      | Node{l=Empty; r} -> r
      | Node{l; v; r} -> bal (remove_min_elt l) v r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. *)

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

    (* Splitting.  split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x. *)

    let rec split x = function
        Empty ->
          (Empty, false, Empty)
      | Node{l; v; r} ->
          let c = Ord.compare x v in
          if c = 0 then (l, true, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
          else
            let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

    (* Implementation of the set operations *)

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec mem x = function
        Empty -> false
      | Node{l; v; r} ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec remove x = function
        Empty -> Empty
      | (Node{l; v; r} as t) ->
          let c = Ord.compare x v in
          if c = 0 then merge l r
          else
            if c < 0 then
              let ll = remove x l in
              if l == ll then t
              else bal ll v r
            else
              let rr = remove x r in
              if r == rr then t
              else bal l v rr

    let rec union s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2}) ->
          if h1 >= h2 then
            if h2 = 1 then add v2 s1 else begin
              let (l2, _, r2) = split v1 s2 in
              join (union l1 l2) v1 (union r1 r2)
            end
          else
            if h1 = 1 then add v1 s2 else begin
              let (l1, _, r1) = split v2 s1 in
              join (union l1 l2) v2 (union r1 r2)
            end

    let rec inter s1 s2 =
      match (s1, s2) with
        (Empty, _) -> Empty
      | (_, Empty) -> Empty
      | (Node{l=l1; v=v1; r=r1}, t2) ->
          match split v1 t2 with
            (l2, false, r2) ->
              concat (inter l1 l2) (inter r1 r2)
          | (l2, true, r2) ->
              join (inter l1 l2) v1 (inter r1 r2)

    (* Same as split, but compute the left and right subtrees
       only if the pivot element is not in the set.  The right subtree
       is computed on demand. *)

    type split_bis =
      | Found
      | NotFound of t * (unit -> t)

    let rec split_bis x = function
        Empty ->
          NotFound (Empty, (fun () -> Empty))
      | Node{l; v; r; _} ->
          let c = Ord.compare x v in
          if c = 0 then Found
          else if c < 0 then
            match split_bis x l with
            | Found -> Found
            | NotFound (ll, rl) -> NotFound (ll, (fun () -> join (rl ()) v r))
          else
            match split_bis x r with
            | Found -> Found
            | NotFound (lr, rr) -> NotFound (join l v lr, rr)

    let rec disjoint s1 s2 =
      match (s1, s2) with
        (Empty, _) | (_, Empty) -> true
      | (Node{l=l1; v=v1; r=r1}, t2) ->
          if s1 == s2 then false
          else match split_bis v1 t2 with
              NotFound(l2, r2) -> disjoint l1 l2 && disjoint r1 (r2 ())
            | Found -> false

    let rec diff s1 s2 =
      match (s1, s2) with
        (Empty, _) -> Empty
      | (t1, Empty) -> t1
      | (Node{l=l1; v=v1; r=r1}, t2) ->
          match split v1 t2 with
            (l2, false, r2) ->
              join (diff l1 l2) v1 (diff r1 r2)
          | (l2, true, r2) ->
              concat (diff l1 l2) (diff r1 r2)

    type enumeration = End | More of elt * t * enumeration

    let rec cons_enum s e =
      match s with
        Empty -> e
      | Node{l; v; r} -> cons_enum l (More(v, r, e))

    let rec compare_aux e1 e2 =
        match (e1, e2) with
        (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, r1, e1), More(v2, r2, e2)) ->
          let c = Ord.compare v1 v2 in
          if c <> 0
          then c
          else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

    let compare s1 s2 =
      compare_aux (cons_enum s1 End) (cons_enum s2 End)

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec subset s1 s2 =
      match (s1, s2) with
        Empty, _ ->
          true
      | _, Empty ->
          false
      | Node {l=l1; v=v1; r=r1}, (Node {l=l2; v=v2; r=r2} as t2) ->
          let c = Ord.compare v1 v2 in
          if c = 0 then
            subset l1 l2 && subset r1 r2
          else if c < 0 then
            subset (Node {l=l1; v=v1; r=Empty; h=0}) l2 && subset r1 t2
          else
            subset (Node {l=Empty; v=v1; r=r1; h=0}) r2 && subset l1 t2

    let rec iter f = function
        Empty -> ()
      | Node{l; v; r} -> iter f l; f v; iter f r

    let rec fold f s accu =
      match s with
        Empty -> accu
      | Node{l; v; r} -> fold f r (f v (fold f l accu))

    let rec for_all p = function
        Empty -> true
      | Node{l; v; r} -> p v && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node{l; v; r} -> p v || exists p l || exists p r

    let rec filter p = function
        Empty -> Empty
      | (Node{l; v; r}) as t ->
          (* call [p] in the expected left-to-right order *)
          let l' = filter p l in
          let pv = p v in
          let r' = filter p r in
          if pv then
            if l==l' && r==r' then t else join l' v r'
          else concat l' r'

    let rec partition p = function
        Empty -> (Empty, Empty)
      | Node{l; v; r} ->
          (* call [p] in the expected left-to-right order *)
          let (lt, lf) = partition p l in
          let pv = p v in
          let (rt, rf) = partition p r in
          if pv
          then (join lt v rt, concat lf rf)
          else (concat lt rt, join lf v rf)

    let rec cardinal = function
        Empty -> 0
      | Node{l; r} -> cardinal l + 1 + cardinal r

    let rec elements_aux accu = function
        Empty -> accu
      | Node{l; v; r} -> elements_aux (v :: elements_aux accu r) l

    let elements s =
      elements_aux [] s

    let choose = min_elt

    let choose_opt = min_elt_opt

    let rec find x = function
        Empty -> raise Not_found
      | Node{l; v; r} ->
          let c = Ord.compare x v in
          if c = 0 then v
          else find x (if c < 0 then l else r)

    let rec find_first_aux v0 f = function
        Empty ->
          v0
      | Node{l; v; r} ->
          if f v then
            find_first_aux v f l
          else
            find_first_aux v0 f r

    let rec find_first f = function
        Empty ->
          raise Not_found
      | Node{l; v; r} ->
          if f v then
            find_first_aux v f l
          else
            find_first f r

    let rec find_first_opt_aux v0 f = function
        Empty ->
          Some v0
      | Node{l; v; r} ->
          if f v then
            find_first_opt_aux v f l
          else
            find_first_opt_aux v0 f r

    let rec find_first_opt f = function
        Empty ->
          None
      | Node{l; v; r} ->
          if f v then
            find_first_opt_aux v f l
          else
            find_first_opt f r

    let rec find_last_aux v0 f = function
        Empty ->
          v0
      | Node{l; v; r} ->
          if f v then
            find_last_aux v f r
          else
            find_last_aux v0 f l

    let rec find_last f = function
        Empty ->
          raise Not_found
      | Node{l; v; r} ->
          if f v then
            find_last_aux v f r
          else
            find_last f l

    let rec find_last_opt_aux v0 f = function
        Empty ->
          Some v0
      | Node{l; v; r} ->
          if f v then
            find_last_opt_aux v f r
          else
            find_last_opt_aux v0 f l

    let rec find_last_opt f = function
        Empty ->
          None
      | Node{l; v; r} ->
          if f v then
            find_last_opt_aux v f r
          else
            find_last_opt f l

    let rec find_opt x = function
        Empty -> None
      | Node{l; v; r} ->
          let c = Ord.compare x v in
          if c = 0 then Some v
          else find_opt x (if c < 0 then l else r)

    let try_join l v r =
      (* [join l v r] can only be called when (elements of l < v <
         elements of r); use [try_join l v r] when this property may
         not hold, but you hope it does hold in the common case *)
      if (l = Empty || Ord.compare (max_elt l) v < 0)
      && (r = Empty || Ord.compare v (min_elt r) < 0)
      then join l v r
      else union l (add v r)

    let rec map f = function
      | Empty -> Empty
      | Node{l; v; r} as t ->
         (* enforce left-to-right evaluation order *)
         let l' = map f l in
         let v' = f v in
         let r' = map f r in
         if l == l' && v == v' && r == r' then t
         else try_join l' v' r'

    let of_sorted_list l =
      let rec sub n l =
        match n, l with
        | 0, l -> Empty, l
        | 1, x0 :: l -> Node {l=Empty; v=x0; r=Empty; h=1}, l
        | 2, x0 :: x1 :: l ->
            Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1; r=Empty; h=2}, l
        | 3, x0 :: x1 :: x2 :: l ->
            Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1;
                 r=Node{l=Empty; v=x2; r=Empty; h=1}; h=2}, l
        | n, l ->
          let nl = n / 2 in
          let left, l = sub nl l in
          match l with
          | [] -> assert false
          | mid :: l ->
            let right, l = sub (n - nl - 1) l in
            create left mid right, l
      in
      fst (sub (List.length l) l)

    let of_list l =
      match l with
      | [] -> empty
      | [x0] -> singleton x0
      | [x0; x1] -> add x1 (singleton x0)
      | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
      | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
      | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
      | _ -> of_sorted_list (List.sort_uniq Ord.compare l)

    let add_seq i m =
      Seq.fold_left (fun s x -> add x s) m i

    let of_seq i = add_seq i empty

    let rec seq_of_enum_ c () = match c with
      | End -> Seq.Nil
      | More (x, t, rest) -> Seq.Cons (x, seq_of_enum_ (cons_enum t rest))

    let to_seq c = seq_of_enum_ (cons_enum c End)

    let to_seq_from low s =
      let rec aux low s c = match s with
        | Empty -> c
        | Node {l; r; v; _} ->
            begin match Ord.compare v low with
              | 0 -> More (v, r, c)
              | n when n<0 -> aux low r c
              | _ -> aux low l (More (v, r, c))
            end
      in
      seq_of_enum_ (aux low s End)
  end
