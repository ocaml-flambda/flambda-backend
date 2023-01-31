(* CR aspectorzabusky: This needs a copyright header; should I copy [array.ml]? *)

open! Stdlib

[@@@ocaml.flambda_o3]

(* An alias for the type of immutable arrays. *)
type +'a t = 'a iarray

(* Array operations *)

external length       : 'a iarray -> int       = "%array_length"
external get          : 'a iarray -> int -> 'a = "%array_safe_get"
external ( .:() )     : 'a iarray -> int -> 'a = "%array_safe_get"
external unsafe_get   : 'a iarray -> int -> 'a = "%array_unsafe_get"

(* Immutable and mutable arrays have the same runtime representation; we
   construct immutable arrays by constructing mutable arrays and then blindly
   casting them to become immutable.  This is safe here because:
   1. None of these functions mutate their array inputs;
   2. None of these functions hold on to their array inputs; and
   3. All of these functions return fresh arrays if they return an array. *)
let init =
  Obj.magic (Array.init : int -> (int -> 'a) -> 'a array)
let append =
  Obj.magic (Array.append : 'a array -> 'a array -> 'a array)
let concat =
  Obj.magic (Array.concat : 'a array list -> 'a array)
let sub =
  Obj.magic (Array.sub : 'a array -> int -> int -> 'a array)
let to_list =
  Obj.magic (Array.to_list : 'a array -> 'a list)
let of_list =
  Obj.magic (Array.of_list : 'a list -> 'a array)
let iter =
  Obj.magic (Array.iter : ('a -> unit) -> 'a array -> unit)
let iteri =
  Obj.magic (Array.iteri : (int -> 'a -> unit) -> 'a array -> unit)
let map =
  Obj.magic (Array.map : ('a -> 'b) -> 'a array -> 'b array)
let mapi =
  Obj.magic (Array.mapi : (int -> 'a -> 'b) -> 'a array -> 'b array)
let fold_left =
  Obj.magic (Array.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a)
let fold_left_map =
  Obj.magic (Array.fold_left_map :
               ('a -> 'b -> 'a * 'c) -> 'a -> 'b array -> 'a * 'c array)
let fold_right =
  Obj.magic (Array.fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a)
let iter2 =
  Obj.magic (Array.iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit)
let map2 =
  Obj.magic (Array.map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array)
let for_all =
  Obj.magic (Array.for_all : ('a -> bool) -> 'a array -> bool)
let exists =
  Obj.magic (Array.exists : ('a -> bool) -> 'a array -> bool)
let for_all2 =
  Obj.magic (Array.for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool)
let exists2 =
  Obj.magic (Array.exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool)
let mem =
  Obj.magic (Array.mem : 'a -> 'a array -> bool)
let memq =
  Obj.magic (Array.memq : 'a -> 'a array -> bool)
let find_opt =
  Obj.magic (Array.find_opt : ('a -> bool) -> 'a array -> 'a option)
let find_map =
  Obj.magic (Array.find_map : ('a -> 'b option) -> 'a array -> 'b option)
let split =
  Obj.magic (Array.split : ('a * 'b) array -> 'a array * 'b array)
let combine =
  Obj.magic (Array.combine : 'a array -> 'b array -> ('a * 'b) array)
let to_seq =
  Obj.magic (Array.to_seq : 'a array -> 'a Seq.t)
let to_seqi =
  Obj.magic (Array.to_seqi : 'a array -> (int * 'a) Seq.t)
let of_seq =
  Obj.magic (Array.of_seq : 'a Seq.t -> 'a array)

(* Only safe if the array isn't used after this call *)
let unsafe_of_array : 'a array -> 'a iarray = Obj.magic

(* Must be fully applied due to the value restriction *)
let lift_sort sorter cmp iarr =
  let arr = Array.of_iarray iarr in
  sorter cmp arr;
  unsafe_of_array arr

let sort cmp iarr = lift_sort Array.sort cmp iarr
let stable_sort cmp iarr = lift_sort Array.stable_sort cmp iarr
let fast_sort cmp iarr = lift_sort Array.fast_sort cmp iarr

let to_array = Array.of_iarray
let of_array = Array.to_iarray
