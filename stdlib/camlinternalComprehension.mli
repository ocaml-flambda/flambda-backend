(** Supporting functions for list comprehensions *)

(** Backwards snoc lists that can be spine-local but element-global.  This
    allows list comprehensions to build their intermediate data structure on the
    stack instead of the heap; since list comprehensions build their
    intermediate lists backwards, we store these as snoc lists instead of cons
    lists. *)
type 'a rev_list =
  | Nil
  | Snoc of { init : 'a rev_list; global_ last : 'a }

(** List comprehensions are conceptually desugared in terms of difference lists,
    functions from ['a list -> 'a list], which have been built in reverse; we
    use [rev_list] instead of [list] to keep track of the reversedness and get
    stack allocation.  All [rev_dlist] values should be [local_], as they're all
    intermediate data structures and can also be stack allocated.  *)
type 'a rev_dlist = local_ 'a rev_list -> local_ 'a rev_list

(** Reverse a [local_] snoc list to get a global regular list.  To turn a local
    reversed difference list into a (global, normal, forwards) list, first
    provide it the empty snoc list ([Nil]) and then use this function to get the
    final result. *)
val rev_list_to_list : local_ 'a rev_list -> 'a list

(** [rev_dlist_concat_map] is [concat_map] with its arguments swapped for
    reversed difference lists. *)
val rev_dlist_concat_map
  : 'a list -> local_ ('a -> local_ 'b rev_dlist) -> local_ 'b rev_dlist

(** [rev_dlist_concat_iterate_up low high f] is the same as
    [rev_dlist_concat_map range f] where [range] is the increasing range from
    [low] to [high], inclusive. *)
val rev_dlist_concat_iterate_up
  : int -> int -> local_ (int -> local_ 'a rev_dlist) -> local_ 'a rev_dlist

(** [rev_dlist_concat_iterate_up high low f] is the same as
    [rev_dlist_concat_map range f] where [range] is the decreasing range from
    [high] to [low], inclusive. *)
val rev_dlist_concat_iterate_down
  : int -> int -> local_ (int -> local_ 'a rev_dlist) -> local_ 'a rev_dlist

(** List comprehensions always produce global values right now, but it would be
    great if they could produce local values, too.  (Then they'd be able to/have
    to iterate over local lists.)  That requires either some form of mode
    polymorphism or code duplication, so it isn't present yet. *)
