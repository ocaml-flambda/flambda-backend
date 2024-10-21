(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*               Antal Spector-Zabusky, Jane Street, New York             *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(* NOTE:
   If this file is iarrayLabels.mli, run tools/sync_stdlib_docs after editing it
   to generate iarray.mli.

   If this file is iarray.mli, do not edit it directly -- edit
   iarrayLabels.mli instead.
 *)

(** Operations on immutable arrays.  This module mirrors the API of [Array], but
    omits functions that assume mutability; in addition to obviously mutating
    functions, it omits [copy] along with the functions [make], [create_float],
    and [make_matrix] that produce all-constant arrays.  The exception is the
    sorting functions, which are given a copying API to replace the in-place
    one. *)

type +'a t = 'a iarray
(** An alias for the type of immutable arrays. *)

external length : local_ 'a iarray -> int = "%array_length"
(** Return the length (number of elements) of the given immutable array. *)

external get : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) =
  "%array_safe_get"
(** [get a n] returns the element number [n] of immutable array [a].
   The first element has number 0.
   The last element has number [length a - 1].
   You can also write [a.:(n)] instead of [get a n].

   @raise Invalid_argument
   if [n] is outside the range 0 to [(length a - 1)]. *)

external ( .:() ) : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) =
  "%array_safe_get"
(** A synonym for [get]. *)

val init : int -> local_ (int -> 'a) -> 'a iarray
(** [init n f] returns a fresh immutable array of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [init n f] tabulates the results of [f]
   applied to the integers [0] to [n-1].

   @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
   If the return type of [f] is [float], then the maximum
   size is only [Sys.max_array_length / 2]. *)

val init_local : int -> local_ (int -> local_ 'a) -> local_ 'a iarray
(** The locally-allocating version of [init]. *)

val append : 'a iarray -> 'a iarray -> 'a iarray
(** [append v1 v2] returns a fresh immutable array containing the
   concatenation of the immutable arrays [v1] and [v2].
   @raise Invalid_argument if
   [length v1 + length v2 > Sys.max_array_length]. *)

val append_local : local_ 'a iarray -> local_ 'a iarray -> local_ 'a iarray
(** The locally-allocating version of [append]. *)

val concat : 'a iarray list -> 'a iarray
(** Same as {!append}, but concatenates a list of immutable arrays. *)

val concat_local : local_ 'a iarray list -> local_ 'a iarray
(** The locally-allocating version of [concat]. *)

val sub : 'a iarray -> int -> int -> 'a iarray
(** [sub a pos len] returns a fresh immutable array of length [len],
   containing the elements number [pos] to [pos + len - 1]
   of immutable array [a].  This creates a copy of the selected
   portion of the immutable array.

   @raise Invalid_argument if [pos] and [len] do not
   designate a valid subarray of [a]; that is, if
   [pos < 0], or [len < 0], or [pos + len > length a]. *)

val sub_local : local_ 'a iarray -> int -> int -> local_ 'a iarray
(** The locally-allocating version of [sub]. *)

val to_list : 'a iarray -> 'a list
(** [to_list a] returns the list of all the elements of [a]. *)

val to_list_local : local_ 'a iarray -> local_ 'a list
(** The locally-allocating version of []. *)

val of_list : 'a list -> 'a iarray
(** [of_list l] returns a fresh immutable array containing the elements
   of [l].

   @raise Invalid_argument if the length of [l] is greater than
   [Sys.max_array_length]. *)

val of_list_local : local_ 'a list -> local_ 'a iarray
(** The locally-allocating version of [of_list]. *)

(** {1 Converting to and from mutable arrays} *)

val to_array : 'a iarray -> 'a array
(** [to_array a] returns a mutable copy of the immutable array [a]; that is, a
   fresh (mutable) array containing the same elements as [a] *)

val of_array : 'a array -> 'a iarray
(** [of_array ma] returns an immutable copy of the mutable array [ma]; that is,
   a fresh immutable array containing the same elements as [ma] *)

(** {1 Iterators} *)

val iter : local_ ('a -> unit) -> 'a iarray -> unit
(** [iter f a] applies function [f] in turn to all
   the elements of [a].  It is equivalent to
   [f a.:(0); f a.:(1); ...; f a.:(length a - 1); ()]. *)

val iter_local : local_ (local_ 'a -> unit) -> local_ 'a iarray -> unit
(** The locally-scoped version of [iter]. *)

val iteri : local_ (int -> 'a -> unit) -> 'a iarray -> unit
(** Same as {!iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val iteri_local : local_ (int -> local_ 'a -> unit) -> local_ 'a iarray -> unit
(** The locally-scoped version of [iteri]. *)

val map : local_ ('a -> 'b) -> 'a iarray -> 'b iarray
(** [map f a] applies function [f] to all the elements of [a],
   and builds an immutable array with the results returned by [f]:
   [[| f a.:(0); f a.:(1); ...; f a.:(length a - 1) |]]. *)

val map_local :
  local_ (local_ 'a -> local_ 'b) -> local_ 'a iarray -> local_ 'b iarray
(** The locally-scoped and locally-allocating version of [map]. *)

val map_local_input : local_ (local_ 'a -> 'b) -> local_ 'a iarray -> 'b iarray
(** The locally-constrained but globally-allocating version of [map]. *)

val map_local_output : local_ ('a -> local_ 'b) -> 'a iarray -> local_ 'b iarray
(** The locally-allocating but global-input version of [map]. *)

val mapi : local_ (int -> 'a -> 'b) -> 'a iarray -> 'b iarray
(** Same as {!map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val mapi_local :
  local_ (int -> local_ 'a -> local_ 'b) ->
  local_ 'a iarray ->
  local_ 'b iarray
(** The locally-scoped and locally-allocating version of [mapi]. *)

val mapi_local_input :
  local_ (int -> local_ 'a -> 'b) -> local_ 'a iarray -> 'b iarray
(** The locally-constrained but globally-allocating version of [mapi]. *)

val mapi_local_output :
  local_ (int -> 'a -> local_ 'b) -> 'a iarray -> local_ 'b iarray
(** The locally-allocating but global-input version of [mapi]. *)

val fold_left : local_ ('a -> 'b -> 'a) -> 'a -> 'b iarray -> 'a
(** [fold_left f init a] computes
   [f (... (f (f init a.:(0)) a.:(1)) ...) a.:(n-1)],
   where [n] is the length of the immutable array [a]. *)

val fold_left_local :
  local_ (local_ 'a -> local_ 'b -> local_ 'a) ->
  local_ 'a ->
  local_ 'b iarray ->
  local_ 'a
(** The locally-constrained and locally-allocating version of [fold_left].

   WARNING: This function consumes O(n) extra stack space, as every intermediate
   accumulator will be left on the local stack! *)

val fold_left_local_input :
  local_ ('a -> local_ 'b -> 'a) -> 'a -> local_ 'b iarray -> 'a
(** The locally-constrained but globally-allocating version of [fold_left]. *)

val fold_left_local_output :
  local_ (local_ 'a -> 'b -> local_ 'a) -> local_ 'a -> 'b iarray -> local_ 'a
(** The locally-allocating but global-input version of [fold_left].

   WARNING: This function consumes O(n) extra stack space, as every intermediate
   accumulator will be left on the local stack! *)

val fold_left_map :
  local_ ('a -> 'b -> 'a * 'c) -> 'a -> 'b iarray -> 'a * 'c iarray
(** [fold_left_map] is a combination of {!fold_left} and {!map} that threads an
    accumulator through calls to [f]. *)

val fold_left_map_local :
  local_ (local_ 'a -> local_ 'b -> local_ 'a * 'c) ->
  local_ 'a ->
  local_ 'b iarray ->
  local_ 'a * 'c iarray
(** The locally-constrained and locally-allocating version of [fold_left].

   WARNING: This function consumes O(n) extra stack space, as every intermediate
   accumulator will be left on the local stack! *)

val fold_left_map_local_input :
  local_ ('a -> local_ 'b -> 'a * 'c) ->
  'a ->
  local_ 'b iarray ->
  'a * 'c iarray
(** The locally-constrained but globally-allocating version of [fold_left]. *)

val fold_left_map_local_output :
  local_ (local_ 'a -> 'b -> local_ 'a * 'c) ->
  local_ 'a ->
  'b iarray ->
  local_ 'a * 'c iarray
(** The locally-allocating but global-input version of [fold_left].

   WARNING: This function consumes O(n) extra stack space, as every intermediate
   accumulator will be left on the local stack! *)

val fold_right : local_ ('b -> 'a -> 'a) -> 'b iarray -> 'a -> 'a
(** [fold_right f a init] computes
   [f a.:(0) (f a.:(1) ( ... (f a.:(n-1) init) ...))],
   where [n] is the length of the immutable array [a]. *)

val fold_right_local :
  local_ (local_ 'b -> local_ 'a -> local_ 'a) ->
  local_ 'b iarray ->
  local_ 'a ->
  local_ 'a
(** The locally-constrained and locally-allocating version of [fold_right].

   WARNING: This function consumes O(n) extra stack space, as every intermediate
   accumulator will be left on the local stack! *)

val fold_right_local_input :
  local_ (local_ 'b -> 'a -> 'a) -> local_ 'b iarray -> 'a -> 'a
(** The locally-constrained but globally-allocating version of [fold_right]. *)

val fold_right_local_output :
  local_ ('b -> local_ 'a -> local_ 'a) -> 'b iarray -> local_ 'a -> local_ 'a
(** The locally-allocating but global-input version of [fold_right].

   WARNING: This function consumes O(n) extra stack space, as every intermediate
   accumulator will be left on the local stack! *)


(** {1 Iterators on two arrays} *)


val iter2 : local_ ('a -> 'b -> unit) -> 'a iarray -> 'b iarray -> unit
(** [iter2 f a b] applies function [f] to all the elements of [a]
   and [b].
   @raise Invalid_argument if the immutable arrays are not the same size.
   *)

val iter2_local :
  local_ (local_ 'a -> local_ 'b -> unit) ->
  local_ 'a iarray ->
  local_ 'b iarray ->
  unit
(** The locally-scoped version of [iter2]. *)

val iter2_local_first :
  local_ (local_ 'a -> 'b -> unit) -> local_ 'a iarray -> 'b iarray -> unit
(** The first-biased partly-locally-scoped version of [iter2]. *)

val iter2_local_second :
  local_ ('a -> local_ 'b -> unit) -> 'a iarray -> local_ 'b iarray -> unit
(** The second-biased partly-locally-scoped version of [iter2]. *)

val map2 : local_ ('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray
(** [map2 f a b] applies function [f] to all the elements of [a]
   and [b], and builds an immutable array with the results returned by [f]:
   [[| f a.:(0) b.:(0); ...; f a.:(length a - 1) b.:(length b - 1)|]].
   @raise Invalid_argument if the immutable arrays are not the same size. *)

val map2_local :
  local_ (local_ 'a -> local_ 'b -> local_ 'c) ->
  local_ 'a iarray ->
  local_ 'b iarray ->
  local_ 'c iarray
(** The locally-scoped and locally-allocating version of [map2]. *)

val map2_local_inputs :
  local_ (local_ 'a -> local_ 'b -> 'c) ->
  local_ 'a iarray ->
  local_ 'b iarray ->
  'c iarray
(** The locally-scoped but globally-allocating version of [map2]. *)

val map2_local_output :
  local_ ('a -> 'b -> local_ 'c) -> 'a iarray -> 'b iarray -> local_ 'c iarray
(** The locally-allocating but global-inputs version of [map2]. *)

val map2_local_first_input :
  local_ (local_ 'a -> 'b -> 'c) -> local_ 'a iarray -> 'b iarray -> 'c iarray
(** The first-biased partly-locally-scoped but globally-allocating version of
    [map2]. *)

val map2_local_second_input :
  local_ ('a -> local_ 'b -> 'c) -> 'a iarray -> local_ 'b iarray -> 'c iarray
(** The second-biased partly-locally-scoped but globally-allocating version of
    [map2]. *)

val map2_local_first_input_and_output :
  local_ (local_ 'a -> 'b -> local_ 'c) ->
  local_ 'a iarray ->
  'b iarray ->
  local_ 'c iarray
(** The locally-allocating and first-biased partly-locally-scoped version of
    [map2]. *)

val map2_local_second_input_and_output :
  local_ ('a -> local_ 'b -> local_ 'c) ->
  'a iarray ->
  local_ 'b iarray ->
  local_ 'c iarray
(** The locally-allocating and second-biased partly-locally-scoped version of
    [map2]. *)


(** {1 Array scanning} *)

val for_all : local_ ('a -> bool) -> 'a iarray -> bool
(** [for_all f [|a1; ...; an|]] checks if all elements
   of the immutable array satisfy the predicate [f]. That is, it returns
   [(f a1) && (f a2) && ... && (f an)]. *)

val for_all_local : local_ (local_ 'a -> bool) -> local_ 'a iarray -> bool
(** The locally-scoped version of [for_all]. *)

val exists : local_ ('a -> bool) -> 'a iarray -> bool
(** [exists f [|a1; ...; an|]] checks if at least one element of
    the immutable array satisfies the predicate [f]. That is, it returns
    [(f a1) || (f a2) || ... || (f an)]. *)

val exists_local : local_ (local_ 'a -> bool) -> local_ 'a iarray -> bool
(** The locally-scoped version of [exists]. *)

val for_all2 : local_ ('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool
(** Same as {!for_all}, but for a two-argument predicate.
   @raise Invalid_argument if the two immutable arrays have different
   lengths. *)

val for_all2_local :
  local_ (local_ 'a -> local_ 'b -> bool) ->
  local_ 'a iarray ->
  local_ 'b iarray ->
  bool
(** The locally-scoped version of [for_all2]. *)

val for_all2_local_first :
  local_ (local_ 'a -> 'b -> bool) -> local_ 'a iarray -> 'b iarray -> bool
(** The first-biased partly-locally-scoped version of [for_all2]. *)

val for_all2_local_second :
  local_ ('a -> local_ 'b -> bool) -> 'a iarray -> local_ 'b iarray -> bool
(** The second-biased partly-locally-scoped version of [for_all2]. *)

val exists2 : local_ ('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool
(** Same as {!exists}, but for a two-argument predicate.
   @raise Invalid_argument if the two immutable arrays have different
   lengths. *)

val exists2_local :
  local_ (local_ 'a -> local_ 'b -> bool) ->
  local_ 'a iarray ->
  local_ 'b iarray ->
  bool
(** The locally-scoped version of [exists2]. *)

val exists2_local_first :
  local_ (local_ 'a -> 'b -> bool) -> local_ 'a iarray -> 'b iarray -> bool
(** The first-biased partly-locally-scoped version of [exists2]. *)

val exists2_local_second :
  local_ ('a -> local_ 'b -> bool) -> 'a iarray -> local_ 'b iarray -> bool
(** The second-biased partly-locally-scoped version of [exists2]. *)

val mem : local_ 'a -> local_ 'a iarray -> bool
(** [mem a set] is true if and only if [a] is structurally equal
    to an element of [l] (i.e. there is an [x] in [l] such that
    [compare a x = 0]). *)

val memq : local_ 'a -> local_ 'a iarray -> bool
(** Same as {!mem}, but uses physical equality
   instead of structural equality to compare list elements. *)

val find_opt : local_ ('a -> bool) -> 'a iarray -> 'a option
(** [find_opt ~f a] returns the first element of the immutable array [a] that
    satisfies the predicate [f], or [None] if there is no value that satisfies
    [f] in the array [a]. *)

val find_opt_local :
  local_ (local_ 'a -> bool) -> local_ 'a iarray -> local_ 'a option
(** The locally-constrained and locally-allocating version of []. *)

val find_map : local_ ('a -> 'b option) -> 'a iarray -> 'b option
(** [find_map ~f a] applies [f] to the elements of [a] in order, and returns the
    first result of the form [Some v], or [None] if none exist. *)

val find_map_local :
  local_ (local_ 'a -> local_ 'b option) -> local_ 'a iarray -> local_ 'b option
(** The locally-constrained and locally-allocating version of [find_map]. *)

val find_map_local_input :
  local_ (local_ 'a -> 'b option) -> local_ 'a iarray -> 'b option
(** The locally-constrained but globally-allocating version of [find_map]. *)

val find_map_local_output :
  local_ ('a -> local_ 'b option) -> 'a iarray -> local_ 'b option
(** The locally-allocating but global-input version of [find_map]. *)

(** {1 Arrays of pairs} *)

val split : ('a * 'b) iarray -> 'a iarray * 'b iarray
(** [split [:(a1,b1); ...; (an,bn):]] is
    [([:a1; ...; an:], [:b1; ...; bn:])]. *)

val split_local : local_ ('a * 'b) iarray -> local_ 'a iarray * 'b iarray
(** The locally-allocating version of [split]. *)

val combine : 'a iarray -> 'b iarray -> ('a * 'b) iarray
(** [combine [:a1; ...; an:] [:b1; ...; bn:]] is [[:(a1,b1); ...; (an,bn):]].
    Raise [Invalid_argument] if the two immutable iarrays have different
    lengths. *)

val combine_local :
  local_ 'a iarray -> local_ 'b iarray -> local_ ('a * 'b) iarray
(** The locally-allocating version of [combine]. *)

(** {1 Sorting} *)

(* CR-someday aspectorzabusky: The comparison functions could be [local_] if we
   changed [Array] *)

val sort : ('a -> 'a -> int) -> 'a iarray -> 'a iarray
(** Sort an immutable array in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see below for a
   complete specification).  For example, {!Stdlib.compare} is
   a suitable comparison function. The result of calling [sort] is a fresh
   immutable array containing the same elements as the original sorted in
   increasing order. Other than this fresh array, [sort] is guaranteed to run in
   constant heap space and (at most) logarithmic stack space.

   The current implementation uses Heap Sort.  It runs in constant
   stack space.

   Specification of the comparison function:
   Let [a] be the immutable array and [cmp] the comparison function.  The
   following must be true for all [x], [y], [z] in [a] :
-   [cmp x y] > 0 if and only if [cmp y x] < 0
-   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

   The result of [sort], which we'll call [a'], contains the same elements as
   [a], reordered in such a way that for all i and j valid indices of [a] (or
   equivalently, of [a']):
-   [cmp a'.:(i) a'.:(j)] >= 0 if and only if i >= j
*)

(* MISSING: Requires rewriting the sorting algorithms
val sort_local :
  (local_ 'a -> local_ 'a -> int) -> local_ 'a iarray -> local_ 'a iarray
(** The locally-constrained and locally-allocating version of [sort]. *)
*)

val stable_sort : ('a -> 'a -> int) -> 'a iarray -> 'a iarray
(** Same as {!sort}, but the sorting algorithm is stable (i.e.
   elements that compare equal are kept in their original order) and
   not guaranteed to run in constant heap space.

   The current implementation uses Merge Sort. It uses a temporary array of
   length [n/2], where [n] is the length of the immutable array.  It is usually
   faster than the current implementation of {!sort}.
*)

(* MISSING: Requires rewriting the sorting algorithms
val stable_sort_local :
  (local_ 'a -> local_ 'a -> int) -> local_ 'a iarray -> local_ 'a iarray
(** The locally-constrained and locally-allocating version of [stable_sort]. *)
*)

val fast_sort : ('a -> 'a -> int) -> 'a iarray -> 'a iarray
(** Same as {!sort} or {!stable_sort}, whichever is
    faster on typical input. *)

(* MISSING: Requires rewriting the sorting algorithms
val fast_sort_local :
  (local_ 'a -> local_ 'a -> int) -> local_ 'a iarray -> local_ 'a iarray
(** The locally-constrained and locally-allocating version of [fast_sort]. *)
*)

(** {1 Iterators} *)

val to_seq : 'a iarray -> 'a Seq.t
(** Iterate on the immutable array, in increasing order. *)

(* MISSING: No meaningful local [Seq.t]s
val to_seq_local : local_ 'a iarray -> local_ 'a Seq.t
(** The locally-allocating version of [to_seq]. *)
*)

val to_seqi : 'a iarray -> (int * 'a) Seq.t
(** Iterate on the immutable array, in increasing order, yielding indices along
    elements. *)

(* MISSING: No meaningful local [Seq.t]s
val to_seqi_local : local_ 'a iarray -> local_ (int * 'a) Seq.t
(** The locally-allocating version of [to_seqi]. *)
*)

val of_seq : 'a Seq.t -> 'a iarray
(** Create an immutable array from the generator *)

(* MISSING: No meaningful local [Seq.t]s
val of_seq_local : local_ 'a Seq.t -> local_ 'a iarray
(** The locally-allocating version of [of_seq]. *)
*)

(**/**)

(** {1 Undocumented functions} *)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) =
  "%array_unsafe_get"
