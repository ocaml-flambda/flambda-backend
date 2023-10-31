(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Jacques Garrigue, Kyoto University RIMS                 *)
(*              Antal Spector-Zabusky, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

(* Module [IarrayLabels]: labelled Iarray module *)

[@@@ocaml.nolabels]

(* NOTE: Please work in iarray.ml and then copy the results here; for more
   information, see the top of that file.  This is a temporary state of affairs,
   but for now, please copy things! *)

(* CR mshinwell: Change to "include Iarray"; we can't do this at present
   as it requires referencing Stdlib__Iarray which will work under the make
   build but not under dune. *)

(* An alias for the type of immutable arrays. *)
type +'a t = 'a iarray

(* Array operations *)

external length : local_ 'a iarray -> int = "%array_length"
external get : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) =
  "%array_safe_get"
external ( .:() ) : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) =
  "%array_safe_get"
external unsafe_get : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) =
  "%array_unsafe_get"
external concat : 'a iarray list -> 'a iarray = "caml_array_concat"
external concat_local : local_ 'a iarray list -> local_ 'a iarray =
  "caml_array_concat_local"

external append_prim : 'a iarray -> 'a iarray -> 'a iarray = "caml_array_append"
external append_prim_local :
  local_ 'a iarray -> local_ 'a iarray -> local_ 'a iarray =
  "caml_array_append_local"
external unsafe_sub : 'a iarray -> int -> int -> 'a iarray = "caml_array_sub"
external unsafe_sub_local : local_ 'a iarray -> int -> int -> local_ 'a iarray =
  "caml_array_sub_local"
external unsafe_of_array : 'a array -> 'a iarray = "%array_to_iarray"
external unsafe_to_array : 'a iarray -> 'a array = "%array_of_iarray"

(* Used only to reimplement [init] *)
external unsafe_set_mutable : 'a array -> int -> 'a -> unit =
  "%array_unsafe_set"

(* VERY UNSAFE: Any of these functions can be used to violate the "no forward
   pointers" restriction for the local stack if not used carefully.  Each of
   these can either make a local mutable array or mutate its contents, and if
   not careful, this can lead to an array's contents pointing forwards. *)
external make_mutable_local : int -> local_ 'a -> local_ 'a array =
  "caml_make_local_vect"
external unsafe_of_local_array : local_ 'a array -> local_ 'a iarray =
  "%array_to_iarray"
external unsafe_set_local : local_ 'a array -> int -> local_ 'a -> unit =
  "%array_unsafe_set"

(* We can't use immutable array literals in this file, since we don't want to
   require the stdlib to be compiled with extensions, so instead of [[::]] we
   use [unsafe_of_array [||]] below.  Thankfully, we never need it in the
   [local] case so we don't have to think about the details. *)

(* Really trusting the inliner here; to get maximum performance, it has to
   inline both [unsafe_init_local] *and* [f]. *)
(** Precondition: [l >= 0]. *)
let[@inline always] unsafe_init_local l (local_ f : int -> local_ 'a) = local_
  if l = 0 then
    unsafe_of_local_array [||]
  else
    (* The design of this function is exceedingly delicate, and is the only way
       we can correctly allocate a local array on the stack via mutation.  We
       are subject to the "no forward pointers" constraint on the local stack;
       we're not allowed to make pointers to later-allocated objects even within
       the same stack frame.  Thus, in order to get this right, we consume O(n)
       call-stack space: we allocate the values to put in the array, and only
       *then* recurse, creating the array as the very last thing of all and
       *returning* it.  This is why the [f i] call is the first thing in the
       function, and why it's not tail-recursive; if it were tail-recursive,
       then we wouldn't have anywhere to put the array elements during the whole
       process. *)
    let rec go i = local_ begin
      let x = f i in
      if i = l - 1 then
        make_mutable_local l x
      else begin
        let res = go (i+1) in
        unsafe_set_local res i x;
        res
      end
    end in
    unsafe_of_local_array (go 0)

(* The implementation is copied from [Array] so that [f] can be [local_] *)
let init l (local_ f) =
  if l = 0 then unsafe_of_array [||] else
  if l < 0 then invalid_arg "Iarray.init"
  (* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... *)
  else
   let res = Array.make l (f 0) in
   for i = 1 to pred l do
     unsafe_set_mutable res i (f i)
   done;
   unsafe_of_array res

let init_local l f = local_
  if l < 0 then invalid_arg "Iarray.init_local"
  (* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... *)
  else unsafe_init_local l f

let append a1 a2 =
  if length a1 = 0 then a2 (* Safe because they're immutable *)
  else if length a2 = 0 then a1
  else append_prim a1 a2

let append_local a1 a2 = local_
  if length a1 = 0 then a2 (* Safe because they're immutable *)
  else if length a2 = 0 then a1
  else append_prim_local a1 a2

let sub a ofs len =
  if ofs < 0 || len < 0 || ofs > length a - len
  then invalid_arg "Iarray.sub"
  else unsafe_sub a ofs len

let sub_local a ofs len = local_
  if ofs < 0 || len < 0 || ofs > length a - len
  then invalid_arg "Iarray.sub"
  else unsafe_sub_local a ofs len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let iter_local f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let iter2 f a b =
  if length a <> length b then
    invalid_arg "Iarray.iter2: arrays must have the same length"
  else
    for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

let iter2_local f a b =
  if length a <> length b then
    invalid_arg "Iarray.iter2_local: arrays must have the same length"
  else
    for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

let iter2_local_first f a b =
  if length a <> length b then
    invalid_arg "Iarray.iter2_local_first: arrays must have the same length"
  else
    for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

let iter2_local_second f a b =
  if length a <> length b then
    invalid_arg "Iarray.iter2_local_second: arrays must have the same length"
  else
    for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

let map f a =
  let l = length a in
  if l = 0 then unsafe_of_array [||] else begin
    let r = Array.make l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f(unsafe_get a i))
    done;
    unsafe_of_array r
  end

let map_local f a = local_
  unsafe_init_local (length a) (fun i -> local_ f (unsafe_get a i))

let map_local_input f a =
  let l = length a in
  if l = 0 then unsafe_of_array [||] else begin
    let r = Array.make l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f(unsafe_get a i))
    done;
    unsafe_of_array r
  end

let map_local_output f a = local_
  unsafe_init_local (length a) (fun i -> local_ f (unsafe_get a i))

let map2 f a b =
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2: arrays must have the same length"
  else begin
    if la = 0 then unsafe_of_array [||] else begin
      let r = Array.make la (f (unsafe_get a 0) (unsafe_get b 0)) in
      for i = 1 to la - 1 do
        Array.unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      unsafe_of_array r
    end
  end

let map2_local f a b = local_
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2_local: arrays must have the same length"
  else
    unsafe_init_local la (fun i -> local_ f (unsafe_get a i) (unsafe_get b i))

let map2_local_inputs f a b =
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2: arrays must have the same length"
  else begin
    if la = 0 then unsafe_of_array [||] else begin
      let r = Array.make la (f (unsafe_get a 0) (unsafe_get b 0)) in
      for i = 1 to la - 1 do
        Array.unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      unsafe_of_array r
    end
  end

let map2_local_output f a b = local_
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2_local: arrays must have the same length"
  else
    unsafe_init_local la (fun i -> local_ f (unsafe_get a i) (unsafe_get b i))

let map2_local_first_input f a b =
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2: arrays must have the same length"
  else begin
    if la = 0 then unsafe_of_array [||] else begin
      let r = Array.make la (f (unsafe_get a 0) (unsafe_get b 0)) in
      for i = 1 to la - 1 do
        Array.unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      unsafe_of_array r
    end
  end

let map2_local_second_input f a b =
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2: arrays must have the same length"
  else begin
    if la = 0 then unsafe_of_array [||] else begin
      let r = Array.make la (f (unsafe_get a 0) (unsafe_get b 0)) in
      for i = 1 to la - 1 do
        Array.unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      unsafe_of_array r
    end
  end

let map2_local_first_input_and_output f a b = local_
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2_local: arrays must have the same length"
  else
    unsafe_init_local la (fun i -> local_ f (unsafe_get a i) (unsafe_get b i))

let map2_local_second_input_and_output f a b = local_
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Iarray.map2_local: arrays must have the same length"
  else
    unsafe_init_local la (fun i -> local_ f (unsafe_get a i) (unsafe_get b i))

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let iteri_local f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let mapi f a =
  let l = length a in
  if l = 0 then unsafe_of_array [||] else begin
    let r = Array.make l (f 0 (unsafe_get a 0)) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f i (unsafe_get a i))
    done;
    unsafe_of_array r
  end

let mapi_local f a = local_
  unsafe_init_local (length a) (fun i -> local_ f i (unsafe_get a i))

let mapi_local_input f a =
  let l = length a in
  if l = 0 then unsafe_of_array [||] else begin
    let r = Array.make l (f 0 (unsafe_get a 0)) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f i (unsafe_get a i))
    done;
    unsafe_of_array r
  end

let mapi_local_output f a = local_
  unsafe_init_local (length a) (fun i -> local_ f i (unsafe_get a i))

let to_list a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

let to_list_local a = local_
  let rec tolist i res = local_
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

let of_list l = unsafe_of_array (Array.of_list l)

(* Cannot use List.length here because the List module depends on Array. *)
let rec list_length accu = function
  | [] -> accu
  | _::t -> list_length (succ accu) t

(* This shouldn't violate the forward-pointers restriction because the list
   elements already exist *)
let of_list_local = function
  | [] -> local_ unsafe_of_array [||]
  | hd::tl as l -> local_
      let a = make_mutable_local (list_length 0 l) hd in
      let rec fill i = function
        | [] -> local_ a
        | hd::tl -> local_ unsafe_set_local a i hd; fill (i+1) tl in
      unsafe_of_local_array (fill 1 tl)

let to_array ia = Array.copy (unsafe_to_array ia)

let of_array ma = unsafe_of_array (Array.copy ma)

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let fold_left_local f x a = local_
  let len = length a in
  let rec go r i = local_
    if i = len
    then r
    else go (f r (unsafe_get a i)) (i+1)
  in
  go x 0

let fold_left_local_input f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let fold_left_local_output f x a = local_
  let len = length a in
  let rec go r i = local_
    if i = len
    then r
    else go (f r (unsafe_get a i)) (i+1)
  in
  go x 0

let fold_left_map f acc input_array =
  let len = length input_array in
  if len = 0 then (acc, unsafe_of_array [||]) else begin
    let acc, elt = f acc (unsafe_get input_array 0) in
    let output_array = Array.make len elt in
    let acc = ref acc in
    for i = 1 to len - 1 do
      let acc', elt = f !acc (unsafe_get input_array i) in
      acc := acc';
      Array.unsafe_set output_array i elt;
    done;
    !acc, unsafe_of_array output_array
  end

let fold_left_map_local f acc input_array = local_
  let len = length input_array in
  if len = 0 then (acc, unsafe_of_local_array [||]) else begin
    let rec go acc i = local_
      let acc', elt = f acc (unsafe_get input_array i) in
      if i = len - 1 then
        acc', make_mutable_local len elt
      else begin
        let (_, output_array) as res = go acc (i+1) in
        unsafe_set_local output_array i elt;
        res
      end
    in
    let acc, output_array = go acc 0 in
    acc, unsafe_of_local_array output_array
  end

let fold_left_map_local_input f acc input_array =
  let len = length input_array in
  if len = 0 then (acc, unsafe_of_array [||]) else begin
    let acc, elt = f acc (unsafe_get input_array 0) in
    let output_array = Array.make len elt in
    let acc = ref acc in
    for i = 1 to len - 1 do
      let acc', elt = f !acc (unsafe_get input_array i) in
      acc := acc';
      Array.unsafe_set output_array i elt;
    done;
    !acc, unsafe_of_array output_array
  end

let fold_left_map_local_output f acc input_array = local_
  let len = length input_array in
  if len = 0 then (acc, unsafe_of_local_array [||]) else begin
    let rec go acc i = local_
      let acc', elt = f acc (unsafe_get input_array i) in
      if i = len - 1 then
        acc', make_mutable_local len elt
      else begin
        let (_, output_array) as res = go acc (i+1) in
        unsafe_set_local output_array i elt;
        res
      end
    in
    let acc, output_array = go acc 0 in
    acc, unsafe_of_local_array output_array
  end

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

let fold_right_local f a x = local_
  let rec go r i = local_
    if i = -1
    then r
    else go (f (unsafe_get a i) r) (i-1)
  in
  go x (length a - 1)

let fold_right_local_input f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

let fold_right_local_output f a x = local_
  let rec go r i = local_
    if i = -1
    then r
    else go (f (unsafe_get a i) r) (i-1)
  in
  go x (length a - 1)

(* CR aspectorzabusky: Why do I need this?  Shouldn't mode-crossing handle doing
   this? *)
let[@inline always] globalize_bool : local_ bool -> bool = fun b -> b

let exists p a =
  let n = length a in
  let rec loop i = local_
    if i = n then false
    else if p (unsafe_get a i) then true
    else loop (succ i) in
  globalize_bool (loop 0)

let exists_local p a =
  let n = length a in
  let rec loop i = local_
    if i = n then false
    else if p (unsafe_get a i) then true
    else loop (succ i) in
  globalize_bool (loop 0)

let for_all p a =
  let n = length a in
  let rec loop i = local_
    if i = n then true
    else if p (unsafe_get a i) then loop (succ i)
    else false in
  globalize_bool (loop 0)

let for_all_local p a =
  let n = length a in
  let rec loop i = local_
    if i = n then true
    else if p (unsafe_get a i) then loop (succ i)
    else false in
  globalize_bool (loop 0)

let for_all2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.for_all2"
  else let rec loop i = local_
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  globalize_bool (loop 0)

let for_all2_local p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.for_all2_local"
  else let rec loop i = local_
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  globalize_bool (loop 0)

let for_all2_local_first p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.for_all2_local_first"
  else let rec loop i = local_
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  globalize_bool (loop 0)

let for_all2_local_second p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.for_all2_local_second"
  else let rec loop i = local_
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  globalize_bool (loop 0)

let exists2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.exists2"
  else let rec loop i = local_
    if i = n1 then false
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
    else loop (succ i) in
  globalize_bool (loop 0)

let exists2_local p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.exists2_local"
  else let rec loop i = local_
    if i = n1 then false
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
    else loop (succ i) in
  globalize_bool (loop 0)

let exists2_local_first p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.exists2_local_first"
  else let rec loop i = local_
    if i = n1 then false
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
    else loop (succ i) in
  globalize_bool (loop 0)

let exists2_local_second p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Iarray.exists2_local_second"
  else let rec loop i = local_
    if i = n1 then false
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
    else loop (succ i) in
  globalize_bool (loop 0)

let mem x a =
  let n = length a in
  let rec loop i = local_
    if i = n then false
    else if compare (unsafe_get a i) x = 0 then true
    else loop (succ i) in
  globalize_bool (loop 0)

let memq x a =
  let n = length a in
  let rec loop i = local_
    if i = n then false
    else if x == (unsafe_get a i) then true
    else loop (succ i) in
  globalize_bool (loop 0)

let find_opt p a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      let x = unsafe_get a i in
      if p x then Some x
      else loop (succ i)
  in
  loop 0 [@nontail]

let find_opt_local p a = local_
  let n = length a in
  let rec loop i = local_
    if i = n then None
    else
      let x = unsafe_get a i in
      if p x then Some x
      else loop (succ i)
  in
  loop 0

let find_map f a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      match f (unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0 [@nontail]

let find_map_local f a = local_
  let n = length a in
  let rec loop i = local_
    if i = n then None
    else
      match f (unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0

let find_map_local_input f a =
  let n = length a in
  let rec loop i =
    if i = n then None
    else
      match f (unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0 [@nontail]

let find_map_local_output f a = local_
  let n = length a in
  let rec loop i = local_
    if i = n then None
    else
      match f (unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0

let split x =
  if x = unsafe_of_array [||]
  then unsafe_of_array [||], unsafe_of_array [||]
  else begin
    let a0, b0 = unsafe_get x 0 in
    let n = length x in
    let a = Array.make n a0 in
    let b = Array.make n b0 in
    for i = 1 to n - 1 do
      let ai, bi = unsafe_get x i in
      Array.unsafe_set a i ai;
      Array.unsafe_set b i bi
    done;
    unsafe_of_array a, unsafe_of_array b
  end

(* This shouldn't violate the forward-pointers restriction because the array
   elements already exist.  (This doesn't work for [combine], where we need to
   create the tuples.) *)
let split_local x = local_
  if x = unsafe_of_array [||]
  then unsafe_of_array [||], unsafe_of_array [||]
  else begin
    let a0, b0 = unsafe_get x 0 in
    let n = length x in
    let a = make_mutable_local n a0 in
    let b = make_mutable_local n b0 in
    for i = 1 to n - 1 do
      let ai, bi = unsafe_get x i in
      unsafe_set_local a i ai;
      unsafe_set_local b i bi
    done;
    unsafe_of_local_array a, unsafe_of_local_array b
  end

let combine a b =
  let na = length a in
  let nb = length b in
  if na <> nb then invalid_arg "Iarray.combine";
  let r = if na = 0 then [||]
  else begin
    let x = Array.make na (unsafe_get a 0, unsafe_get b 0) in
    for i = 1 to na - 1 do
      Array.unsafe_set x i (unsafe_get a i, unsafe_get b i)
    done;
    x
  end in
  unsafe_of_array r

let combine_local a b = local_
  let na = length a in
  let nb = length b in
  if na <> nb then invalid_arg "Iarray.combine_local";
  unsafe_init_local na (fun i -> local_ unsafe_get a i, unsafe_get b i)

(* Must be fully applied due to the value restriction *)
let lift_sort sorter cmp iarr =
  let arr = to_array iarr in
  sorter cmp arr;
  unsafe_of_array arr

let sort cmp iarr = lift_sort Array.sort cmp iarr
let stable_sort cmp iarr = lift_sort Array.stable_sort cmp iarr
let fast_sort cmp iarr = lift_sort Array.fast_sort cmp iarr

let to_seq a =
  let rec aux i () =
    if i < length a
    then
      let x = unsafe_get a i in
      Seq.Cons (x, aux (i+1))
    else Seq.Nil
  in
  aux 0

let to_seqi a =
  let rec aux i () =
    if i < length a
    then
      let x = unsafe_get a i in
      Seq.Cons ((i,x), aux (i+1))
    else Seq.Nil
  in
  aux 0

let of_seq i = unsafe_of_array (Array.of_seq i)
