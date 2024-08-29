open! Stdlib

type 'a rev_list =
  | Nil
  | Snoc of { init : 'a rev_list; global_ last : 'a }

type 'a rev_dlist = local_ 'a rev_list -> local_ 'a rev_list

let rec rev_list_to_list' acc = function
  | Nil                 -> acc
  | Snoc { init; last } -> rev_list_to_list' (last :: acc) init
;;

let rev_list_to_list rl = rev_list_to_list' [] rl

(* Can be thought of as the combination of [map] and composition *)
let rec rev_dlist_concat_map l f acc = exclave_
  (* [List.fold_left (fun acc el -> f el acc) acc l], but more [local_] *)
  match l with
  | []      -> acc
  | el :: l -> rev_dlist_concat_map l f (f el acc)
;;

let rec rev_dlist_concat_iterate_up from to_ f acc = exclave_
  if to_ < from
  then acc
  else rev_dlist_concat_iterate_up (from + 1) to_ f (f from acc)
;;

let rec rev_dlist_concat_iterate_down from to_ f acc = exclave_
  if to_ > from
  then acc
  else rev_dlist_concat_iterate_down (from - 1) to_ f (f from acc)
;;
