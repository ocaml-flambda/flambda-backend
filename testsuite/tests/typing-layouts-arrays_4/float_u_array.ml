(* This file is not in stdlib because closure middle end can't compile
   it. Should be moved to somewhere in otherlibs in the future. *)

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

open! Stdlib

[@@@ocaml.flambda_o3]

[@@@ocaml.nolabels]

(* An alias for the type of arrays. *)
type ('a : float64) t = ('a : float64) array

(* Array operations *)

external length : ('a : float64) array -> int = "%array_length"
external get: ('a : float64) array -> int -> ('a : float64) = "%array_safe_get"
external set: ('a : float64) array -> int -> ('a : float64) -> unit = "%array_safe_set"
external unsafe_get: ('a : float64) array -> int -> ('a : float64) = "%array_unsafe_get"
external unsafe_set: ('a : float64) array -> int -> ('a : float64) -> unit = "%array_unsafe_set"
external floatarray_create : int -> ('a : float64) array = "caml_floatarray_create"
external unsafe_blit :
  ('a : float64) array -> int -> ('a : float64) array -> int -> int -> unit = "caml_floatarray_blit"

let unsafe_fill : ('a : float64) array -> int -> int -> ('a : float64) -> unit =
  fun a ofs len v ->
    for i = ofs to ofs + len - 1 do unsafe_set a i v done

let make : int -> ('a : float64) -> ('a : float64) array = fun n v ->
  let result = floatarray_create n in
  unsafe_fill result 0 n v;
  result

let create: int -> ('a : float64) -> ('a : float64) array = make

let unsafe_sub: ('a : float64) array -> int -> int -> ('a : float64) array = fun a ofs len ->
  let result = floatarray_create len in
  unsafe_blit a ofs result 0 len;
  result

let append_prim: ('a : float64) array -> ('a : float64) array -> ('a : float64) array = fun a1 a2 ->
  let l1 = length a1 in
  let l2 = length a2 in
  let result = floatarray_create (l1 + l2) in
  unsafe_blit a1 0 result 0 l1;
  unsafe_blit a2 0 result l1 l2;
  result
let ensure_ge (x:int) y =
  if x >= y then x else invalid_arg "Float_u_array.concat"

let rec sum_lengths acc = function
  | [] -> acc
  | hd :: tl -> sum_lengths (ensure_ge (length hd + acc) acc) tl

let concat: ('a : float64) array list -> ('a : float64) array = fun l ->
  let len = sum_lengths 0 l in
  let result = floatarray_create len in
  let rec loop l i =
    match l with
    | [] -> assert (i = len)
    | hd :: tl ->
      let hlen = length hd in
      unsafe_blit hd 0 result i hlen;
      loop tl (i + hlen)
  in
  loop l 0;
  result

let empty () = floatarray_create 0

let init l f =
  if l = 0 then (empty ()) else
  if l < 0 then invalid_arg "Float_u_array.init"
  (* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... *)
  else
   let res = create l (f 0) in
   for i = 1 to pred l do
     unsafe_set res i (f i)
   done;
   res

let make_matrix sx sy init =
  let res = Array.make sx (empty ()) in
  for x = 0 to pred sx do
    Array.unsafe_set res x (create sy init)
  done;
  res

let create_matrix = make_matrix

let copy a =
  let l = length a in if l = 0 then (empty ()) else unsafe_sub a 0 l

let append a1 a2 =
  let l1 = length a1 in
  if l1 = 0 then copy a2
  else if length a2 = 0 then unsafe_sub a1 0 l1
  else append_prim a1 a2

let sub a ofs len =
  if ofs < 0 || len < 0 || ofs > length a - len
  then invalid_arg "Float_u_array.sub"
  else unsafe_sub a ofs len

let fill a ofs len v =
  if ofs < 0 || len < 0 || ofs > length a - len
  then invalid_arg "Float_u_array.fill"
  else unsafe_fill a ofs len v

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
             || ofs2 < 0 || ofs2 > length a2 - len
  then invalid_arg "Float_u_array.blit"
  else unsafe_blit a1 ofs1 a2 ofs2 len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let iter2 f a b =
  if length a <> length b then
    invalid_arg "Float_u_array.iter2: arrays must have the same length"
  else
    for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

let map f a =
  let l = length a in
  if l = 0 then (empty ()) else begin
    let r = create l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f(unsafe_get a i))
    done;
    r
  end

(* duplicated from array.ml *)
let map_inplace f a =
  for i = 0 to length a - 1 do
    unsafe_set a i (f (unsafe_get a i))
  done

let map2 f a b =
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Float_u_array.map2: arrays must have the same length"
  else begin
    if la = 0 then (empty ()) else begin
      let r = create la (f (unsafe_get a 0) (unsafe_get b 0)) in
      for i = 1 to la - 1 do
        unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      r
    end
  end

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let mapi f a =
  let l = length a in
  if l = 0 then (empty ()) else begin
    let r = create l (f 0 (unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f i (unsafe_get a i))
    done;
    r
  end

(* duplicated from array.ml *)
let mapi_inplace f a =
  for i = 0 to length a - 1 do
    unsafe_set a i (f i (unsafe_get a i))
  done

(* CR layouts: Uncommment functions below when we can have unboxed
   things in lists and other types. *)
(* let to_list a =
 *   let rec tolist i res =
 *     if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
 *   tolist (length a - 1) []
 *
 * (* Cannot use List.length here because the List module depends on Float_u_array. *)
 * let rec list_length accu = function
 *   | [] -> accu
 *   | _::t -> list_length (succ accu) t
 *
 * let of_list = function
 *     [] -> (empty ())
 *   | hd::tl as l ->
 *       let a = create (list_length 0 l) hd in
 *       let rec fill i = function
 *           [] -> a
 *         | hd::tl -> unsafe_set a i hd; fill (i+1) tl in
 *       fill 1 tl *)

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

(* CR layouts: Uncommment functions below when we can have unboxed
   things in lists and other types. *)
(* let fold_left_map f acc input_array =
 *   let len = length input_array in
 *   if len = 0 then (acc, (empty ())) else begin
 *     let acc, elt = f acc (unsafe_get input_array 0) in
 *     let output_array = create len elt in
 *     let acc = ref acc in
 *     for i = 1 to len - 1 do
 *       let acc', elt = f !acc (unsafe_get input_array i) in
 *       acc := acc';
 *       unsafe_set output_array i elt;
 *     done;
 *     !acc, output_array
 *   end *)

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

let exists p a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if p (unsafe_get a i) then true
    else loop (succ i) in
  loop 0

let for_all p a =
  (* take [n], [p], and [a] as parameters to avoid a closure allocation *)
  let rec loop n p a i =
    if i = n then true
    else if p (unsafe_get a i) then loop n p a (succ i)
    else false in
  loop (length a) p a 0

let for_all2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Float_u_array.for_all2"
  else let rec loop i =
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  loop 0

let exists2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Float_u_array.exists2"
  else let rec loop i =
    if i = n1 then false
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
    else loop (succ i) in
  loop 0


external to_float : ('a : float64) -> (float[@local_opt]) = "%box_float"
let[@inline always] compare x y = Float.compare (to_float x) (to_float y)
let mem x a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if compare (unsafe_get a i) x = 0 then true
    else loop (succ i) in
  loop 0

(* CR layouts: Uncommment functions below when we can have unboxed
   things in lists and other types. *)
(* let memq x a =
 *   let n = length a in
 *   let rec loop i =
 *     if i = n then false
 *     else if x == (unsafe_get a i) then true
 *     else loop (succ i) in
 *   loop 0 *)

(* let find_opt p a =
 *   let n = length a in
 *   let rec loop i =
 *     if i = n then None
 *     else
 *       let x = unsafe_get a i in
 *       if p x then Some x
 *       else loop (succ i)
 *   in
 *   loop 0
 *
 * let split x =
 *   if x = (empty ()) then (empty ()), (empty ())
 *   else begin
 *     let a0, b0 = unsafe_get x 0 in
 *     let n = length x in
 *     let a = create n a0 in
 *     let b = create n b0 in
 *     for i = 1 to n - 1 do
 *       let ai, bi = unsafe_get x i in
 *       unsafe_set a i ai;
 *       unsafe_set b i bi
 *     done;
 *     a, b
 *   end
 *
 * let combine a b =
 *   let na = length a in
 *   let nb = length b in
 *   if na <> nb then invalid_arg "Float_u_array.combine";
 *   if na = 0 then (empty ())
 *   else begin
 *     let x = create na (unsafe_get a 0, unsafe_get b 0) in
 *     for i = 1 to na - 1 do
 *       unsafe_set x i (unsafe_get a i, unsafe_get b i)
 *     done;
 *     x
 *   end *)

(* duplicated from array.ml *)
let find_index p a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else if p (unsafe_get a i) then Some i
    else loop (i + 1) in
  loop 0

(* duplicated from array.ml *)
let find_map f a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      match f (unsafe_get a i) with
      | None -> loop (i + 1)
      | Some _ as r -> r
  in
  loop 0

(* duplicated from array.ml *)
let find_mapi f a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      match f i (unsafe_get a i) with
      | None -> loop (i + 1)
      | Some _ as r -> r
  in
  loop 0

exception Bottom of int
let sort cmp a =
  let maxson l i =
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp (get a i31) (get a (i31+1)) < 0 then x := i31+1;
      if cmp (get a !x) (get a (i31+2)) < 0 then x := i31+2;
      !x
    end else
      if i31+1 < l && cmp (get a i31) (get a (i31+1)) < 0
      then i31+1
      else if i31 < l then i31 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp (get a j) e > 0 then begin
      set a i (get a j);
      trickledown l j e;
    end else begin
      set a i e;
    end;
  in
  let trickle l i e = try trickledown l i e with Bottom i -> set a i e in
  let rec bubbledown l i =
    let j = maxson l i in
    set a i (get a j);
    bubbledown l j
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    assert (i <> father);
    if cmp (get a father) e < 0 then begin
      set a i (get a father);
      if father > 0 then trickleup father e else set a 0 e;
    end else begin
      set a i e;
    end;
  in
  let l = length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i (get a i); done;
  for i = l - 1 downto 2 do
    let e = (get a i) in
    set a i (get a 0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = (get a 1) in set a 1 (get a 0); set a 0 e)


let cutoff = 5
let stable_sort cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        set dst d s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (get a i1) i2 s2 (d + 1)
        else
          blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        set dst d s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (get src2 i2) (d + 1)
        else
          blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs (get a src1ofs) src2ofs (get src2 src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = (get a (srcofs + i)) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp (get dst !j) e > 0) do
        set dst (!j + 1) (get dst !j);
        decr j;
      done;
      set dst (!j + 1) e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = make l2 (get a 0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end


let fast_sort = stable_sort

(** {1 Iterators} *)

(* CR layouts: Uncommment functions below when we can have unboxed
   things in lists and other types. *)
(* let to_seq a =
 *   let rec aux i () =
 *     if i < length a
 *     then
 *       let x = unsafe_get a i in
 *       Seq.Cons (x, aux (i+1))
 *     else Seq.Nil
 *   in
 *   aux 0
 *
 * let to_seqi a =
 *   let rec aux i () =
 *     if i < length a
 *     then
 *       let x = unsafe_get a i in
 *       Seq.Cons ((i,x), aux (i+1))
 *     else Seq.Nil
 *   in
 *   aux 0
 *
 * let of_rev_list = function
 *     [] -> (empty ())
 *   | hd::tl as l ->
 *       let len = list_length 0 l in
 *       let a = create len hd in
 *       let rec fill i = function
 *           [] -> a
 *         | hd::tl -> unsafe_set a i hd; fill (i-1) tl
 *       in
 *       fill (len-2) tl
 *
 * let of_seq i =
 *   let l = Seq.fold_left (fun acc x -> x::acc) [] i in
 *   of_rev_list l *)
